use std::sync::Arc;

use rdk_core::{
    attribute, cancel, edge_group, element_selection, error, geometry, group, packed, support,
    topology, topology_index, topology_remap,
};
use rdk_core::error::Code;
use rdk_geom::contour_triangulation;
use rdk_geom::point_grid::{float_max, float_min};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Keep {
    Above,
    Below,
    All,
}

pub struct Params {
    pub cancel: Option<Arc<cancel::T>>,
    pub grain: i64,
    pub keep: Keep,
    pub snapping_tolerance: f64,
    pub fill: bool,
    pub split_connectivity: bool,
    pub clip_attribute: String,
    pub distance: f64,
    pub selection: Option<element_selection::T>,
    pub replace_existing_groups: bool,
    pub clipped_edge_group: Option<String>,
    pub cap_group: Option<String>,
    pub clipped_group: Option<String>,
    pub above_group: Option<String>,
    pub below_group: Option<String>,
    pub origin: rays_math::vec3::T,
    pub normal: rays_math::vec3::T,
}

impl Params {
    /// Native clip option defaults.
    #[cfg(test)]
    pub(crate) fn new(origin: rays_math::vec3::T, normal: rays_math::vec3::T) -> Params {
        Params {
            cancel: None,
            grain: 16_384,
            keep: Keep::Above,
            snapping_tolerance: 1e-9,
            fill: false,
            split_connectivity: false,
            clip_attribute: "P".to_owned(),
            distance: 0.0,
            selection: None,
            replace_existing_groups: true,
            clipped_edge_group: None,
            cap_group: None,
            clipped_group: None,
            above_group: None,
            below_group: None,
            origin,
            normal,
        }
    }
}

fn make(operation: &str, code: Code, message: &str) -> error::T {
    error::new(operation, code, message.to_owned())
}

fn fail(message: &str) -> error::T {
    make("clip", Code::InvalidGeometry, message)
}


fn clamp01(value: f64) -> f64 {
    if value < 0.0 {
        0.0
    } else if value > 1.0 {
        1.0
    } else {
        value
    }
}

struct Builder {
    tokens: Vec<usize>,
    corner_a: Vec<i64>,
    corner_b: Vec<i64>,
    corner_t: Vec<f64>,
    offsets: Vec<i64>,
    kinds: Vec<u8>,
    primitive_source: Vec<i64>,
    primitive_side: Vec<i64>,
    primitive_clipped: Vec<bool>,
    primitive_cap: Vec<bool>,
}

impl Builder {
    fn new() -> Builder {
        Builder {
            tokens: Vec::new(),
            corner_a: Vec::new(),
            corner_b: Vec::new(),
            corner_t: Vec::new(),
            offsets: vec![0],
            kinds: Vec::new(),
            primitive_source: Vec::new(),
            primitive_side: Vec::new(),
            primitive_clipped: Vec::new(),
            primitive_cap: Vec::new(),
        }
    }

    fn corner_count(&self) -> usize {
        self.tokens.len()
    }

    fn truncate_corners(&mut self, count: usize) {
        self.tokens.truncate(count);
        self.corner_a.truncate(count);
        self.corner_b.truncate(count);
        self.corner_t.truncate(count);
    }

    fn add_corner(&mut self, token: usize, a: i64, b: i64, t: f64) {
        self.tokens.push(token);
        self.corner_a.push(a);
        self.corner_b.push(b);
        self.corner_t.push(t);
    }

    fn add_corner_unique(&mut self, start: usize, token: usize, a: i64, b: i64, t: f64) {
        if self.corner_count() == start || self.tokens[self.corner_count() - 1] != token {
            self.add_corner(token, a, b, t);
        }
    }

    fn drop_closing_duplicate(&mut self, start: usize) {
        let count = self.corner_count();
        if count - start > 1 && self.tokens[start] == self.tokens[count - 1] {
            self.truncate_corners(count - 1);
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn finish_primitive(
        &mut self,
        start: usize,
        kind: u8,
        source: i64,
        side: i64,
        clipped: bool,
        cap: bool,
        minimum: usize,
    ) -> bool {
        let count = self.corner_count() - start;
        if count < minimum {
            self.truncate_corners(start);
            false
        } else {
            self.offsets.push(self.corner_count() as i64);
            self.kinds.push(kind);
            self.primitive_source.push(source);
            self.primitive_side.push(side);
            self.primitive_clipped.push(clipped);
            self.primitive_cap.push(cap);
            true
        }
    }
}

struct Segments {
    a: Vec<usize>,
    b: Vec<usize>,
    side: Vec<i64>,
}

impl Segments {
    fn add(&mut self, a: usize, b: usize, side: i64) {
        if a != b {
            self.a.push(a);
            self.b.push(b);
            self.side.push(side);
        }
    }
}

struct Ctx {
    cancel: Option<Arc<cancel::T>>,
    fill: bool,
    keep: Keep,
    nx: f64,
    ny: f64,
    nz: f64,
    point_count: usize,
    px: Arc<Vec<f64>>,
    py: Arc<Vec<f64>>,
    pz: Arc<Vec<f64>>,
    raw_distance: Vec<f64>,
    classified_distance: Vec<f64>,
    vertex_points: Arc<Vec<i64>>,
    primitive_offsets: Arc<Vec<i64>>,
    primitive_kinds: Arc<[u8]>,
    edge_a: Arc<[i64]>,
    edge_b: Arc<[i64]>,
    edge_of_vertex: Arc<[i64]>,
    affected_points: Option<Vec<u8>>,
    shared_token_of_point: Vec<i64>,
    shared_point_of_token: Vec<usize>,
    split: bool,
    base_token_count: usize,
    passthrough_offset: usize,
    has_isolation_tokens: bool,
    primitive_selection: Option<group::T>,
}

type Position = (f64, f64, f64);

impl Ctx {
    fn cancel_ref(&self) -> Option<&cancel::T> {
        self.cancel.as_deref()
    }

    fn poll(&self, index: usize, mask: i64) -> Result<(), error::T> {
        cancel::poll(self.cancel_ref(), index as i64, mask)
    }

    fn token(&self, side: usize, base: usize) -> usize {
        if self.split { side * self.base_token_count + base } else { base }
    }

    fn is_passthrough_token(&self, value: usize) -> bool {
        self.has_isolation_tokens && value >= self.passthrough_offset
    }

    fn passthrough_point(&self, value: usize) -> usize {
        self.shared_point_of_token[value - self.passthrough_offset]
    }

    fn base_of_token(&self, value: usize) -> usize {
        value % self.base_token_count
    }

    fn point_affected(&self, point: usize) -> bool {
        match &self.affected_points {
            None => true,
            Some(values) => values[point] != 0,
        }
    }

    fn token_on_plane(&self, value: usize) -> bool {
        if self.is_passthrough_token(value) {
            false
        } else {
            let base = self.base_of_token(value);
            base >= self.point_count
                || (self.point_affected(base) && self.classified_distance[base] == 0.0)
        }
    }

    fn primitive_selected(&self, primitive: usize) -> bool {
        match &self.primitive_selection {
            None => true,
            Some(selection) => group::mem(primitive as i64, selection),
        }
    }

    fn edge_token(&self, side: usize, vertex: usize) -> Result<usize, error::T> {
        let edge = rdk_core::topology_index::widen(self.edge_of_vertex[vertex]);
        if edge < 0 {
            Err(fail("Rdk.Plane_clip.clip: missing topology edge"))
        } else {
            Ok(self.token(side, self.point_count + edge as usize))
        }
    }

    fn passthrough_token(&self, point: usize) -> usize {
        if self.shared_token_of_point.is_empty() {
            self.token(0, point)
        } else {
            let shared = self.shared_token_of_point[point];
            if shared < 0 {
                self.token(0, point)
            } else {
                self.passthrough_offset + shared as usize
            }
        }
    }

    fn projection(&self, base: usize) -> f64 {
        if self.point_affected(base) && self.classified_distance[base] == 0.0 {
            self.raw_distance[base]
        } else {
            0.0
        }
    }

    fn position_of_selected_token(&self, value: usize) -> Result<Position, error::T> {
        let base = self.base_of_token(value);
        if base < self.point_count {
            let projection = self.projection(base);
            Ok((
                self.px[base] - projection * self.nx,
                self.py[base] - projection * self.ny,
                self.pz[base] - projection * self.nz,
            ))
        } else {
            let edge = base - self.point_count;
            let a = self.edge_a[edge] as usize;
            let b = self.edge_b[edge] as usize;
            let denominator = self.raw_distance[b] - self.raw_distance[a];
            if denominator == 0.0 || !denominator.is_finite() {
                return Err(fail("Rdk.Plane_clip.clip: unstable shared-edge intersection"));
            }
            let t = clamp01(-self.raw_distance[a] / denominator);
            Ok((
                self.px[a] + (self.px[b] - self.px[a]) * t,
                self.py[a] + (self.py[b] - self.py[a]) * t,
                self.pz[a] + (self.pz[b] - self.pz[a]) * t,
            ))
        }
    }

    fn position_of_geometry_token(&self, value: usize) -> Result<Position, error::T> {
        if self.is_passthrough_token(value) {
            let point = self.passthrough_point(value);
            Ok((self.px[point], self.py[point], self.pz[point]))
        } else {
            self.position_of_selected_token(value)
        }
    }

    fn inside(&self, side: usize, point: i64) -> bool {
        side_inside(side, self.classified_distance[point as usize])
    }

    fn add_source_corner(&self, b: &mut Builder, side: usize, start: usize, vertex: usize) {
        let point = self.vertex_points[vertex] as usize;
        b.add_corner_unique(start, self.token(side, point), vertex as i64, vertex as i64, 0.0);
    }

    fn add_intersection_corner(
        &self,
        b: &mut Builder,
        side: usize,
        start: usize,
        previous_vertex: usize,
        current_vertex: usize,
    ) -> Result<(), error::T> {
        let previous_point = self.vertex_points[previous_vertex] as usize;
        let current_point = self.vertex_points[current_vertex] as usize;
        let previous_distance = self.classified_distance[previous_point];
        let current_distance = self.classified_distance[current_point];
        if previous_distance == 0.0 {
            b.add_corner_unique(
                start,
                self.token(side, previous_point),
                previous_vertex as i64,
                previous_vertex as i64,
                0.0,
            );
        } else if current_distance == 0.0 {
            b.add_corner_unique(
                start,
                self.token(side, current_point),
                current_vertex as i64,
                current_vertex as i64,
                0.0,
            );
        } else {
            let denominator = self.raw_distance[current_point] - self.raw_distance[previous_point];
            if denominator == 0.0 || !denominator.is_finite() {
                return Err(fail("Rdk.Plane_clip.clip: unstable plane intersection"));
            }
            let t = clamp01(-self.raw_distance[previous_point] / denominator);
            b.add_corner_unique(
                start,
                self.edge_token(side, previous_vertex)?,
                previous_vertex as i64,
                current_vertex as i64,
                t,
            );
        }
        Ok(())
    }

    fn collect_plane_segment(&self, b: &Builder, segs: &mut Segments, start: usize, side: usize, crossed: bool) {
        if crossed && self.fill {
            let finish = b.corner_count();
            for corner in start..finish {
                let next = if corner + 1 == finish { start } else { corner + 1 };
                let ta = b.tokens[corner];
                let tb = b.tokens[next];
                if ta != tb && self.token_on_plane(ta) && self.token_on_plane(tb) {
                    segs.add(ta, tb, side as i64);
                }
            }
        }
    }

    fn finish_fragment(
        &self,
        b: &mut Builder,
        segs: &mut Segments,
        start: usize,
        side: usize,
        crossed: bool,
        primitive: usize,
    ) {
        b.drop_closing_duplicate(start);
        if b.corner_count() - start >= 3 {
            self.collect_plane_segment(b, segs, start, side, crossed);
            b.finish_primitive(start, 0, primitive as i64, side as i64, crossed, false, 3);
        } else {
            b.truncate_corners(start);
        }
    }

    fn process_passthrough(&self, b: &mut Builder, primitive: usize) {
        let first = self.primitive_offsets[primitive] as usize;
        let last = self.primitive_offsets[primitive + 1] as usize;
        let start = b.corner_count();
        for vertex in first..last {
            let point = self.vertex_points[vertex] as usize;
            b.add_corner(self.passthrough_token(point), vertex as i64, vertex as i64, 0.0);
        }
        let kind = self.primitive_kinds[primitive];
        let minimum = if kind == 1 { 2 } else { 3 };
        b.finish_primitive(start, kind, primitive as i64, -1, false, false, minimum);
    }

    fn process_polygon(
        &self,
        b: &mut Builder,
        segs: &mut Segments,
        primitive: usize,
        side: usize,
    ) -> Result<(), error::T> {
        let first = self.primitive_offsets[primitive] as usize;
        let last = self.primitive_offsets[primitive + 1] as usize;
        let mut has_positive = false;
        let mut has_negative = false;
        let mut inside_runs = 0;
        let mut previous = last - 1;
        for vertex in first..last {
            let value = self.classified_distance[self.vertex_points[vertex] as usize];
            if value > 0.0 {
                has_positive = true;
            }
            if value < 0.0 {
                has_negative = true;
            }
            let previous_point = self.vertex_points[previous];
            if side_inside(side, value) && !self.inside(side, previous_point) {
                inside_runs += 1;
            }
            previous = vertex;
        }
        let crossed = has_positive && has_negative;
        if inside_runs <= 1 {
            let start = b.corner_count();
            let mut previous = last - 1;
            for current in first..last {
                let previous_inside = self.inside(side, self.vertex_points[previous]);
                let current_inside = self.inside(side, self.vertex_points[current]);
                if current_inside {
                    if !previous_inside {
                        self.add_intersection_corner(b, side, start, previous, current)?;
                    }
                    self.add_source_corner(b, side, start, current);
                } else if previous_inside {
                    self.add_intersection_corner(b, side, start, previous, current)?;
                }
                previous = current;
            }
            self.finish_fragment(b, segs, start, side, crossed, primitive);
            return Ok(());
        }
        // A half-plane can disconnect a concave simple polygon, but multiple accepted boundary
        // runs do not necessarily mean multiple output components. Pair the ordered line
        // intersections through the polygon interior, then stitch accepted source-boundary chains
        // into one or more cycles.
        let count = last - first;
        let mut outside_local = 0;
        while outside_local < count && self.inside(side, self.vertex_points[first + outside_local]) {
            outside_local += 1;
        }
        if outside_local == count {
            return Err(fail("Rdk.Plane_clip.clip: disconnected-fragment plan disagreement"));
        }
        let scratch_start = b.corner_count();
        let mut chain_starts = vec![0usize; count];
        let mut chain_finishes = vec![0usize; count];
        let mut chain_count = 0;
        let mut path_start = b.corner_count();
        for edge in 0..count {
            let a = first + (outside_local + edge) % count;
            let c = first + (outside_local + edge + 1) % count;
            let a_inside = self.inside(side, self.vertex_points[a]);
            let b_inside = self.inside(side, self.vertex_points[c]);
            let mut finish_chain = false;
            if a_inside && b_inside {
                if b.corner_count() == path_start {
                    self.add_source_corner(b, side, path_start, a);
                }
                self.add_source_corner(b, side, path_start, c);
            } else if a_inside {
                if b.corner_count() == path_start {
                    self.add_source_corner(b, side, path_start, a);
                }
                self.add_intersection_corner(b, side, path_start, a, c)?;
                finish_chain = true;
            } else if b_inside {
                if b.corner_count() > path_start {
                    finish_chain_at(b, &mut path_start, scratch_start, &mut chain_starts, &mut chain_finishes, &mut chain_count);
                }
                self.add_intersection_corner(b, side, path_start, a, c)?;
                self.add_source_corner(b, side, path_start, c);
            } else if b.corner_count() > path_start {
                finish_chain = true;
            }
            if finish_chain {
                finish_chain_at(b, &mut path_start, scratch_start, &mut chain_starts, &mut chain_finishes, &mut chain_count);
            }
        }
        if b.corner_count() > path_start {
            finish_chain_at(b, &mut path_start, scratch_start, &mut chain_starts, &mut chain_finishes, &mut chain_count);
        }
        if chain_count == 0 {
            return Ok(());
        }
        let scratch_count = b.corner_count() - scratch_start;
        let scratch_tokens: Vec<usize> = b.tokens[scratch_start..scratch_start + scratch_count].to_vec();
        let scratch_a: Vec<i64> = b.corner_a[scratch_start..scratch_start + scratch_count].to_vec();
        let scratch_b: Vec<i64> = b.corner_b[scratch_start..scratch_start + scratch_count].to_vec();
        let scratch_t: Vec<f64> = b.corner_t[scratch_start..scratch_start + scratch_count].to_vec();
        b.truncate_corners(scratch_start);
        let endpoint_count = chain_count * 2;
        let mut endpoint_x = vec![0.0; endpoint_count];
        let mut endpoint_y = vec![0.0; endpoint_count];
        let mut endpoint_z = vec![0.0; endpoint_count];
        let mut endpoint_chain = vec![0usize; endpoint_count];
        let mut endpoint_start = vec![false; endpoint_count];
        let mut endpoint_token = vec![0usize; endpoint_count];
        for chain in 0..chain_count {
            let start_index = chain_starts[chain];
            let end_index = chain_finishes[chain] - 1;
            for (endpoint, index, is_start) in [(chain * 2, start_index, true), (chain * 2 + 1, end_index, false)] {
                let token = scratch_tokens[index];
                let (x, y, z) = self.position_of_selected_token(token)?;
                endpoint_x[endpoint] = x;
                endpoint_y[endpoint] = y;
                endpoint_z[endpoint] = z;
                endpoint_chain[endpoint] = chain;
                endpoint_start[endpoint] = is_start;
                endpoint_token[endpoint] = token;
            }
        }
        let span = |values: &[f64]| {
            let mut minimum = values[0];
            let mut maximum = values[0];
            for &value in &values[1..endpoint_count] {
                minimum = float_min(minimum, value);
                maximum = float_max(maximum, value);
            }
            maximum - minimum
        };
        let x_span = span(&endpoint_x);
        let y_span = span(&endpoint_y);
        let z_span = span(&endpoint_z);
        let coordinate: &Vec<f64> = if x_span >= y_span && x_span >= z_span {
            &endpoint_x
        } else if y_span >= z_span {
            &endpoint_y
        } else {
            &endpoint_z
        };
        if float_max(x_span, float_max(y_span, z_span)) == 0.0 {
            return Err(fail("Rdk.Plane_clip.clip: coincident concave clipping intersections"));
        }
        let mut order: Vec<usize> = (0..endpoint_count).collect();
        order.sort_by(|&left, &right| {
            coordinate[left]
                .total_cmp(&coordinate[right])
                .then(endpoint_token[left].cmp(&endpoint_token[right]))
                .then(endpoint_start[left].cmp(&endpoint_start[right]))
        });
        let mut successor = vec![-1i64; chain_count];
        for pair in 0..chain_count {
            let left = order[pair * 2];
            let right = order[pair * 2 + 1];
            if endpoint_start[left] == endpoint_start[right] {
                return Err(fail("Rdk.Plane_clip.clip: ambiguous concave clipping intersection order"));
            }
            let (end_chain, start_chain) = if endpoint_start[left] {
                (endpoint_chain[right], endpoint_chain[left])
            } else {
                (endpoint_chain[left], endpoint_chain[right])
            };
            if successor[end_chain] >= 0 {
                return Err(fail("Rdk.Plane_clip.clip: duplicate concave clipping successor"));
            }
            successor[end_chain] = start_chain as i64;
        }
        let mut visited = vec![false; chain_count];
        for first_chain in 0..chain_count {
            if visited[first_chain] {
                continue;
            }
            let component_start = b.corner_count();
            let mut current = first_chain as i64;
            while !visited[current as usize] {
                let chain = current as usize;
                visited[chain] = true;
                for corner in chain_starts[chain]..chain_finishes[chain] {
                    b.add_corner_unique(
                        component_start,
                        scratch_tokens[corner],
                        scratch_a[corner],
                        scratch_b[corner],
                        scratch_t[corner],
                    );
                }
                current = successor[chain];
                if current < 0 {
                    return Err(fail("Rdk.Plane_clip.clip: incomplete concave clipping cycle"));
                }
            }
            if current != first_chain as i64 {
                return Err(fail("Rdk.Plane_clip.clip: intersecting concave clipping cycles"));
            }
            self.finish_fragment(b, segs, component_start, side, crossed, primitive);
        }
        Ok(())
    }

    /// Shared cyclic/open scan used by open curves and rotated closed curves.
    fn scan_edges(
        &self,
        b: &mut Builder,
        side: usize,
        primitive: usize,
        edges: &[(usize, usize)],
    ) -> Result<(), error::T> {
        let mut path_start = b.corner_count();
        for &(a, c) in edges {
            let a_inside = self.inside(side, self.vertex_points[a]);
            let b_inside = self.inside(side, self.vertex_points[c]);
            if a_inside && b_inside {
                if b.corner_count() == path_start {
                    self.add_source_corner(b, side, path_start, a);
                }
                self.add_source_corner(b, side, path_start, c);
            } else if a_inside {
                if b.corner_count() == path_start {
                    self.add_source_corner(b, side, path_start, a);
                }
                self.add_intersection_corner(b, side, path_start, a, c)?;
                b.finish_primitive(path_start, 1, primitive as i64, side as i64, true, false, 2);
                path_start = b.corner_count();
            } else if b_inside {
                if b.corner_count() > path_start {
                    b.finish_primitive(path_start, 1, primitive as i64, side as i64, true, false, 2);
                    path_start = b.corner_count();
                }
                self.add_intersection_corner(b, side, path_start, a, c)?;
                self.add_source_corner(b, side, path_start, c);
            } else if b.corner_count() > path_start {
                b.finish_primitive(path_start, 1, primitive as i64, side as i64, true, false, 2);
                path_start = b.corner_count();
            }
        }
        if b.corner_count() > path_start {
            b.finish_primitive(path_start, 1, primitive as i64, side as i64, true, false, 2);
        }
        Ok(())
    }

    fn process_curve(&self, b: &mut Builder, primitive: usize, side: usize) -> Result<(), error::T> {
        let first = self.primitive_offsets[primitive] as usize;
        let last = self.primitive_offsets[primitive + 1] as usize;
        let kind = self.primitive_kinds[primitive];
        let mut has_outside = false;
        for vertex in first..last {
            if !self.inside(side, self.vertex_points[vertex]) {
                has_outside = true;
            }
        }
        if kind == 1 && !has_outside {
            let start = b.corner_count();
            for vertex in first..last {
                self.add_source_corner(b, side, start, vertex);
            }
            b.finish_primitive(start, 1, primitive as i64, side as i64, false, false, 2);
        } else if kind == 1 {
            let edges: Vec<(usize, usize)> = (first..last - 1).map(|vertex| (vertex, vertex + 1)).collect();
            self.scan_edges(b, side, primitive, &edges)?;
        } else if !has_outside {
            let start = b.corner_count();
            for vertex in first..last {
                self.add_source_corner(b, side, start, vertex);
            }
            b.finish_primitive(start, 2, primitive as i64, side as i64, false, false, 3);
        } else {
            // Rotate a clipped closed curve to an outside vertex, then process its cyclic edges
            // as one open scan so wraparound fragments join.
            let count = last - first;
            let mut outside_local = 0;
            while outside_local < count && self.inside(side, self.vertex_points[first + outside_local]) {
                outside_local += 1;
            }
            let edges: Vec<(usize, usize)> = (0..count)
                .map(|edge| {
                    (
                        first + (outside_local + edge) % count,
                        first + (outside_local + edge + 1) % count,
                    )
                })
                .collect();
            self.scan_edges(b, side, primitive, &edges)?;
        }
        Ok(())
    }
}

fn side_inside(side: usize, value: f64) -> bool {
    if side == 0 { value >= 0.0 } else { value <= 0.0 }
}

fn finish_chain_at(
    b: &mut Builder,
    path_start: &mut usize,
    scratch_start: usize,
    chain_starts: &mut [usize],
    chain_finishes: &mut [usize],
    chain_count: &mut usize,
) {
    b.drop_closing_duplicate(*path_start);
    if b.corner_count() - *path_start >= 3 {
        chain_starts[*chain_count] = *path_start - scratch_start;
        chain_finishes[*chain_count] = b.corner_count() - scratch_start;
        *chain_count += 1;
    } else {
        b.truncate_corners(*path_start);
    }
    *path_start = b.corner_count();
}

/// Polygon-only, unfilled clipping has independent primitive plans. Returns the slot count, the
/// per-candidate output index plus one (or zero), and the output corner offsets; `None` when a
/// primitive needs the general builder path.
fn fast_plan(
    ctx: &Ctx,
    grain: i64,
    primitive_count: usize,
) -> Result<Option<(usize, Vec<i64>, Vec<i64>)>, error::T> {
    if ctx.fill || !ctx.primitive_kinds.iter().all(|&kind| kind == 0) {
        return Ok(None);
    }
    let slots = if ctx.keep == Keep::All { 2 } else { 1 };
    let candidate_count = primitive_count * slots;
    let mut candidate_corners = vec![0i64; candidate_count];
    support::par_fill(ctx.cancel_ref(), (grain / 8).max(1), &mut candidate_corners, &|candidate| {
        let candidate = candidate as usize;
        let primitive = candidate / slots;
        let side = match ctx.keep {
            Keep::Above => 0,
            Keep::Below => 1,
            Keep::All => candidate % 2,
        };
        let first = ctx.primitive_offsets[primitive] as usize;
        let last = ctx.primitive_offsets[primitive + 1] as usize;
        if !ctx.primitive_selected(primitive) {
            Ok(if slots == 1 || candidate % slots == 0 { (last - first) as i64 } else { 0 })
        } else {
            let arity = last - first;
            let mut repeated = arity > 64;
            if !repeated {
                for left in first..last.saturating_sub(1) {
                    let point = ctx.vertex_points[left];
                    for right in left + 1..last {
                        if ctx.vertex_points[right] == point {
                            repeated = true;
                        }
                    }
                }
            }
            let enabled = if ctx.keep != Keep::All || side == 0 {
                true
            } else {
                (first..last).any(|vertex| ctx.classified_distance[ctx.vertex_points[vertex] as usize] < 0.0)
            };
            if repeated {
                Ok(-1)
            } else if enabled {
                let mut count: i64 = 0;
                let mut inside_runs = 0;
                let mut previous = last - 1;
                for current in first..last {
                    let previous_inside = ctx.inside(side, ctx.vertex_points[previous]);
                    let current_inside = ctx.inside(side, ctx.vertex_points[current]);
                    if arity > 3 && current_inside && !previous_inside {
                        inside_runs += 1;
                    }
                    if current_inside {
                        count += if previous_inside { 1 } else { 2 };
                    } else if previous_inside {
                        count += 1;
                    }
                    previous = current;
                }
                Ok(if inside_runs > 1 {
                    -1
                } else if count >= 3 {
                    count
                } else {
                    0
                })
            } else {
                Ok(0)
            }
        }
    })?;
    if candidate_corners.iter().any(|&count| count < 0) {
        return Ok(None);
    }
    let output_count = candidate_corners.iter().filter(|&&count| count > 0).count();
    let mut offsets = vec![0i64; output_count + 1];
    let mut output = 0usize;
    let mut corner = 0i64;
    for candidate in 0..candidate_count {
        let count = candidate_corners[candidate];
        if count > 0 {
            offsets[output] = corner;
            candidate_corners[candidate] = output as i64 + 1;
            corner += count;
            output += 1;
        }
    }
    offsets[output_count] = corner;
    Ok(Some((slots, candidate_corners, offsets)))
}

fn fill_fast_plan(
    ctx: &Ctx,
    grain: i64,
    b: &mut Builder,
    slots: usize,
    candidate_output: &[i64],
) -> Result<(), error::T> {
    let total = b.tokens.len();
    let output_count = b.kinds.len();
    let mut tokens = vec![0usize; total];
    let mut corner_a = vec![-1i64; total];
    let mut corner_b = vec![-1i64; total];
    let mut corner_t = vec![0.0f64; total];
    let mut kinds = vec![0u8; output_count];
    let mut primitive_source = vec![-1i64; output_count];
    let mut primitive_side = vec![0i64; output_count];
    let mut primitive_clipped = vec![false; output_count];
    let mut primitive_cap = vec![false; output_count];
    let offsets = &b.offsets;
    // Candidates map to outputs in order, so each candidate range owns a contiguous block of outputs and corners.
    let candidate_grain = (grain / 8).max(1);
    let mut first_output = vec![0usize];
    for chunk in candidate_output.chunks(candidate_grain as usize) {
        first_output.push(first_output[first_output.len() - 1] + chunk.iter().filter(|&&encoded| encoded > 0).count());
    }
    let ranges = first_output.len() as i64 - 1;
    let corner_bound = |range: i64| offsets[first_output[range as usize]] as usize;
    let output_bound = |range: i64| first_output[range as usize];
    {
        let (wt, wa, wb, wf) = (
            support::Windows::new(&mut tokens, ranges, corner_bound),
            support::Windows::new(&mut corner_a, ranges, corner_bound),
            support::Windows::new(&mut corner_b, ranges, corner_bound),
            support::Windows::new(&mut corner_t, ranges, corner_bound),
        );
        let (wk, wsrc, wside, wclipped, wcap) = (
            support::Windows::new(&mut kinds, ranges, output_bound),
            support::Windows::new(&mut primitive_source, ranges, output_bound),
            support::Windows::new(&mut primitive_side, ranges, output_bound),
            support::Windows::new(&mut primitive_clipped, ranges, output_bound),
            support::Windows::new(&mut primitive_cap, ranges, output_bound),
        );
        support::run_indexed_ranges(candidate_grain, candidate_output.len() as i64, &|range, first_candidate, last_candidate| {
            let (tokens, corner_a, corner_b, corner_t) = (wt.take(range), wa.take(range), wb.take(range), wf.take(range));
            let (kinds, primitive_source, primitive_side) = (wk.take(range), wsrc.take(range), wside.take(range));
            let (primitive_clipped, primitive_cap) = (wclipped.take(range), wcap.take(range));
            let (corner_base, output_base) = (corner_bound(range), output_bound(range));
            for candidate in first_candidate..last_candidate {
                support::poll(ctx.cancel_ref(), candidate)?;
                let candidate = candidate as usize;
                let encoded = candidate_output[candidate];
                if encoded <= 0 {
                    continue;
                }
                let output = (encoded - 1) as usize;
                let primitive = candidate / slots;
                let first = ctx.primitive_offsets[primitive] as usize;
                let last = ctx.primitive_offsets[primitive + 1] as usize;
                let mut at = offsets[output] as usize;
                if !ctx.primitive_selected(primitive) {
                    for vertex in first..last {
                        let point = ctx.vertex_points[vertex] as usize;
                        tokens[at - corner_base] = ctx.passthrough_token(point);
                        corner_a[at - corner_base] = vertex as i64;
                        corner_b[at - corner_base] = vertex as i64;
                        corner_t[at - corner_base] = 0.0;
                        at += 1;
                    }
                    if at as i64 != offsets[output + 1] {
                        return Err(fail("Rdk.Plane_clip.clip: passthrough plan/fill disagreement"));
                    }
                    kinds[output - output_base] = 0;
                    primitive_source[output - output_base] = primitive as i64;
                    primitive_side[output - output_base] = -1;
                    primitive_clipped[output - output_base] = false;
                    primitive_cap[output - output_base] = false;
                } else {
                    let side = match ctx.keep {
                        Keep::Above => 0,
                        Keep::Below => 1,
                        Keep::All => candidate % 2,
                    };
                    let mut has_positive = false;
                    let mut has_negative = false;
                    let mut previous = last - 1;
                    for current in first..last {
                        let previous_point = ctx.vertex_points[previous];
                        let current_point = ctx.vertex_points[current] as usize;
                        let value = ctx.classified_distance[current_point];
                        if value > 0.0 {
                            has_positive = true;
                        }
                        if value < 0.0 {
                            has_negative = true;
                        }
                        let previous_inside = ctx.inside(side, previous_point);
                        let current_inside = side_inside(side, value);
                        if current_inside || previous_inside {
                            if current_inside != previous_inside {
                                // write_intersection previous -> current
                                let previous_point = previous_point as usize;
                                let previous_distance = ctx.classified_distance[previous_point];
                                let current_distance = value;
                                if previous_distance == 0.0 {
                                    tokens[at - corner_base] = ctx.token(side, previous_point);
                                    corner_a[at - corner_base] = previous as i64;
                                    corner_b[at - corner_base] = previous as i64;
                                } else if current_distance == 0.0 {
                                    tokens[at - corner_base] = ctx.token(side, current_point);
                                    corner_a[at - corner_base] = current as i64;
                                    corner_b[at - corner_base] = current as i64;
                                } else {
                                    let denominator = ctx.raw_distance[current_point] - ctx.raw_distance[previous_point];
                                    if denominator == 0.0 || !denominator.is_finite() {
                                        return Err(fail("Rdk.Plane_clip.clip: unstable plane intersection"));
                                    }
                                    tokens[at - corner_base] = ctx.edge_token(side, previous)?;
                                    corner_a[at - corner_base] = previous as i64;
                                    corner_b[at - corner_base] = current as i64;
                                    corner_t[at - corner_base] = clamp01(-ctx.raw_distance[previous_point] / denominator);
                                }
                                at += 1;
                            }
                            if current_inside {
                                tokens[at - corner_base] = ctx.token(side, current_point);
                                corner_a[at - corner_base] = current as i64;
                                corner_b[at - corner_base] = current as i64;
                                corner_t[at - corner_base] = 0.0;
                                at += 1;
                            }
                        }
                        previous = current;
                    }
                    if at as i64 != offsets[output + 1] {
                        return Err(fail("Rdk.Plane_clip.clip: polygon plan/fill disagreement"));
                    }
                    kinds[output - output_base] = 0;
                    primitive_source[output - output_base] = primitive as i64;
                    primitive_side[output - output_base] = side as i64;
                    primitive_clipped[output - output_base] = has_positive && has_negative;
                    primitive_cap[output - output_base] = false;
                }
            }
            Ok(())
        })?;
    }
    (b.tokens, b.corner_a, b.corner_b, b.corner_t) = (tokens, corner_a, corner_b, corner_t);
    (b.kinds, b.primitive_source, b.primitive_side) = (kinds, primitive_source, primitive_side);
    (b.primitive_clipped, b.primitive_cap) = (primitive_clipped, primitive_cap);
    Ok(())
}

fn scale_of(ctx: &Ctx, tokens: &[usize], anchor: Position) -> Result<f64, error::T> {
    let mut scale = 0.0;
    for &token in tokens {
        let (x, y, z) = ctx.position_of_geometry_token(token)?;
        scale = float_max(scale, (x - anchor.0).abs());
        scale = float_max(scale, (y - anchor.1).abs());
        scale = float_max(scale, (z - anchor.2).abs());
    }
    Ok(scale)
}

struct Proj {
    incoming: Vec<i64>,
    u: Vec<f64>,
    v: Vec<f64>,
}

impl Proj {
    fn x(&self, token: usize) -> f64 {
        self.u[self.incoming[token] as usize]
    }
    fn y(&self, token: usize) -> f64 {
        self.v[self.incoming[token] as usize]
    }
    fn cross(&self, a: usize, b: usize, c: usize) -> f64 {
        let (ax, ay) = (self.x(a), self.y(a));
        let (bx, by) = (self.x(b), self.y(b));
        let (cx, cy) = (self.x(c), self.y(c));
        (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)
    }
    fn on_segment(&self, point: usize, a: usize, b: usize) -> bool {
        let (px, py) = (self.x(point), self.y(point));
        let (ax, ay) = (self.x(a), self.y(a));
        let (bx, by) = (self.x(b), self.y(b));
        self.cross(a, b, point).abs() <= 1e-12
            && px >= float_min(ax, bx) - 1e-12
            && px <= float_max(ax, bx) + 1e-12
            && py >= float_min(ay, by) - 1e-12
            && py <= float_max(ay, by) + 1e-12
    }
    fn segments_intersect(&self, a: usize, b: usize, c: usize, d: usize) -> bool {
        let ab_c = self.cross(a, b, c);
        let ab_d = self.cross(a, b, d);
        let cd_a = self.cross(c, d, a);
        let cd_b = self.cross(c, d, b);
        ((ab_c > 1e-12 && ab_d < -1e-12) || (ab_c < -1e-12 && ab_d > 1e-12))
            && ((cd_a > 1e-12 && cd_b < -1e-12) || (cd_a < -1e-12 && cd_b > 1e-12))
            || (ab_c.abs() <= 1e-12 && self.on_segment(c, a, b))
            || (ab_d.abs() <= 1e-12 && self.on_segment(d, a, b))
            || (cd_a.abs() <= 1e-12 && self.on_segment(a, c, d))
            || (cd_b.abs() <= 1e-12 && self.on_segment(b, c, d))
    }
    fn point_inside(&self, loop_tokens: &[usize], x: f64, y: f64) -> bool {
        let mut inside = false;
        let mut previous = loop_tokens.len() - 1;
        for current in 0..loop_tokens.len() {
            let (ax, ay) = (self.x(loop_tokens[previous]), self.y(loop_tokens[previous]));
            let (bx, by) = (self.x(loop_tokens[current]), self.y(loop_tokens[current]));
            if (ay > y) != (by > y) && x < (bx - ax) * (y - ay) / (by - ay) + ax {
                inside = !inside;
            }
            previous = current;
        }
        inside
    }
}

fn trace_caps(
    ctx: &Ctx,
    b: &mut Builder,
    segs: &Segments,
    geometry_token_count: usize,
    side: i64,
) -> Result<(), error::T> {
    let (nx, ny, nz) = (ctx.nx, ctx.ny, ctx.nz);
    let segment_count = segs.a.len();
    let mut side_segment_count = 0;
    for segment in 0..segment_count {
        ctx.poll(segment, 4095)?;
        if segs.side[segment] == side {
            side_segment_count += 1;
        }
    }
    let mut outgoing = vec![-1i64; geometry_token_count];
    let mut incoming = vec![-1i64; geometry_token_count];
    for segment in 0..segment_count {
        ctx.poll(segment, 4095)?;
        if segs.side[segment] == side {
            let a = segs.a[segment];
            let c = segs.b[segment];
            if outgoing[a] >= 0 || incoming[c] >= 0 {
                return Err(fail(
                    "Rdk.Plane_clip.clip: cap boundary is non-manifold or inconsistently wound",
                ));
            }
            outgoing[a] = c as i64;
            incoming[c] = a as i64;
        }
    }
    let mut visited = vec![false; geometry_token_count];
    let mut loops: Vec<Vec<usize>> = Vec::new();
    let mut scratch = vec![0usize; std::cmp::max(1, side_segment_count)];
    for start in 0..geometry_token_count {
        ctx.poll(start, 4095)?;
        if (outgoing[start] >= 0) != (incoming[start] >= 0) {
            return Err(fail("Rdk.Plane_clip.clip: cap boundary is open or inconsistently wound"));
        } else if outgoing[start] >= 0 && !visited[start] {
            let mut count = 0;
            let mut current = start as i64;
            while current != start as i64 || count == 0 {
                if current < 0 || (count > 0 && visited[current as usize]) {
                    return Err(fail("Rdk.Plane_clip.clip: cap boundary does not form simple loops"));
                }
                if count >= scratch.len() {
                    return Err(fail("Rdk.Plane_clip.clip: cap boundary traversal overflow"));
                }
                scratch[count] = current as usize;
                count += 1;
                visited[current as usize] = true;
                current = outgoing[current as usize];
            }
            if count < 3 {
                return Err(fail("Rdk.Plane_clip.clip: cap boundary contains fewer than three points"));
            }
            loops.push(scratch[..count].to_vec());
        }
    }
    if loops.is_empty() {
        return Err(fail("Rdk.Plane_clip.clip: cap boundary has no loops"));
    }
    let dominant = {
        let (ax, ay, az) = (nx.abs(), ny.abs(), nz.abs());
        if ax >= ay && ax >= az { 0 } else if ay >= az { 1 } else { 2 }
    };
    let anchor = ctx.position_of_geometry_token(loops[0][0])?;
    let mut cap_coordinate_scale = 0.0;
    for loop_tokens in &loops {
        cap_coordinate_scale = float_max(cap_coordinate_scale, scale_of(ctx, loop_tokens, anchor)?);
    }
    if cap_coordinate_scale == 0.0 {
        return Err(fail("Rdk.Plane_clip.clip: cap contours have zero coordinate extent"));
    }
    let boundary_token_count: usize = loops.iter().map(Vec::len).sum();
    let mut proj = Proj {
        incoming,
        u: vec![0.0; boundary_token_count],
        v: vec![0.0; boundary_token_count],
    };
    let mut projected_at = 0usize;
    for loop_tokens in &loops {
        for &token in loop_tokens {
            let (x, y, z) = ctx.position_of_geometry_token(token)?;
            let (u, v) = if dominant == 0 {
                ((y - anchor.1) / cap_coordinate_scale, (z - anchor.2) / cap_coordinate_scale)
            } else if dominant == 1 {
                ((x - anchor.0) / cap_coordinate_scale, (z - anchor.2) / cap_coordinate_scale)
            } else {
                ((x - anchor.0) / cap_coordinate_scale, (y - anchor.1) / cap_coordinate_scale)
            };
            proj.incoming[token] = projected_at as i64;
            proj.u[projected_at] = u;
            proj.v[projected_at] = v;
            projected_at += 1;
        }
    }
    let bounds: Vec<(f64, f64, f64, f64)> = loops
        .iter()
        .map(|loop_tokens| {
            let x0 = proj.x(loop_tokens[0]);
            let y0 = proj.y(loop_tokens[0]);
            let (mut min_x, mut max_x, mut min_y, mut max_y) = (x0, x0, y0, y0);
            for &token in &loop_tokens[1..] {
                let x = proj.x(token);
                let y = proj.y(token);
                min_x = float_min(min_x, x);
                max_x = float_max(max_x, x);
                min_y = float_min(min_y, y);
                max_y = float_max(max_y, y);
            }
            (min_x, min_y, max_x, max_y)
        })
        .collect();
    let loop_count = loops.len();
    let edge_count = boundary_token_count;
    let mut edge_a = vec![0usize; edge_count];
    let mut edge_b = vec![0usize; edge_count];
    let mut edge_loop = vec![0usize; edge_count];
    let mut edge_local = vec![0usize; edge_count];
    let mut edge_min_x = vec![0.0; edge_count];
    let mut edge_max_x = vec![0.0; edge_count];
    let mut edge_min_y = vec![0.0; edge_count];
    let mut edge_max_y = vec![0.0; edge_count];
    let mut edge_at = 0;
    for (loop_index, loop_tokens) in loops.iter().enumerate() {
        for local in 0..loop_tokens.len() {
            let a = loop_tokens[local];
            let c = loop_tokens[(local + 1) % loop_tokens.len()];
            edge_a[edge_at] = a;
            edge_b[edge_at] = c;
            edge_loop[edge_at] = loop_index;
            edge_local[edge_at] = local;
            edge_min_x[edge_at] = float_min(proj.x(a), proj.x(c));
            edge_max_x[edge_at] = float_max(proj.x(a), proj.x(c));
            edge_min_y[edge_at] = float_min(proj.y(a), proj.y(c));
            edge_max_y[edge_at] = float_max(proj.y(a), proj.y(c));
            edge_at += 1;
        }
    }
    let mut edge_order: Vec<usize> = (0..edge_count).collect();
    edge_order.sort_by(|&left, &right| {
        edge_min_x[left]
            .total_cmp(&edge_min_x[right])
            .then(
                edge_min_y[left]
                    .total_cmp(&edge_min_y[right]),
            )
            .then(left.cmp(&right))
    });
    let mut active = vec![0usize; edge_count];
    let mut active_count = 0;
    for ordered in 0..edge_count {
        ctx.poll(ordered, 4095)?;
        let edge = edge_order[ordered];
        let mut retained = 0;
        for slot in 0..active_count {
            let candidate = active[slot];
            if edge_max_x[candidate] >= edge_min_x[edge] - 1e-12 {
                active[retained] = candidate;
                retained += 1;
            }
        }
        active_count = retained;
        for slot in 0..active_count {
            let candidate = active[slot];
            if edge_min_y[candidate] <= edge_max_y[edge] + 1e-12
                && edge_min_y[edge] <= edge_max_y[candidate] + 1e-12
            {
                let same_loop = edge_loop[candidate] == edge_loop[edge];
                let adjacent = if !same_loop {
                    false
                } else {
                    let left = edge_local[candidate] as i64;
                    let right = edge_local[edge] as i64;
                    let count = loops[edge_loop[edge]].len() as i64;
                    (left - right).abs() == 1 || (left.min(right) == 0 && left.max(right) == count - 1)
                };
                if !adjacent
                    && proj.segments_intersect(edge_a[candidate], edge_b[candidate], edge_a[edge], edge_b[edge])
                {
                    return Err(fail(if same_loop {
                        "Rdk.Plane_clip.clip: cap contour is geometrically self-intersecting"
                    } else {
                        "Rdk.Plane_clip.clip: cap contours intersect or touch"
                    }));
                }
            }
        }
        active[active_count] = edge;
        active_count += 1;
    }
    let mut loop_orientation = vec![0.0f64; loop_count];
    for (loop_index, loop_tokens) in loops.iter().enumerate() {
        ctx.poll(loop_index, 255)?;
        let loop_anchor = ctx.position_of_geometry_token(loop_tokens[0])?;
        let coordinate_scale = scale_of(ctx, loop_tokens, loop_anchor)?;
        if coordinate_scale == 0.0 {
            return Err(fail("Rdk.Plane_clip.clip: cap contour has zero coordinate extent"));
        }
        let mut orientation = 0.0;
        for index in 0..loop_tokens.len() {
            let next = (index + 1) % loop_tokens.len();
            let (x0, y0, z0) = ctx.position_of_geometry_token(loop_tokens[index])?;
            let (x1, y1, z1) = ctx.position_of_geometry_token(loop_tokens[next])?;
            let x0 = (x0 - loop_anchor.0) / coordinate_scale;
            let y0 = (y0 - loop_anchor.1) / coordinate_scale;
            let z0 = (z0 - loop_anchor.2) / coordinate_scale;
            let x1 = (x1 - loop_anchor.0) / coordinate_scale;
            let y1 = (y1 - loop_anchor.1) / coordinate_scale;
            let z1 = (z1 - loop_anchor.2) / coordinate_scale;
            orientation = orientation
                + ((y0 - y1) * (z0 + z1) * nx)
                + ((z0 - z1) * (x0 + x1) * ny)
                + ((x0 - x1) * (y0 + y1) * nz);
        }
        if !orientation.is_finite() || orientation.abs() <= 1e-14 {
            return Err(fail("Rdk.Plane_clip.clip: cap contour has degenerate projected area"));
        }
        loop_orientation[loop_index] = orientation;
    }
    let mut parent = vec![-1i64; loop_count];
    for child in 0..loop_count {
        ctx.poll(child, 255)?;
        let x = proj.x(loops[child][0]);
        let y = proj.y(loops[child][0]);
        for candidate in 0..loop_count {
            if candidate != child && (loop_orientation[candidate] > 0.0) != (loop_orientation[child] > 0.0) {
                let (x0, y0, x1, y1) = bounds[candidate];
                if x >= x0 && x <= x1 && y >= y0 && y <= y1 && proj.point_inside(&loops[candidate], x, y) {
                    let current = parent[child];
                    if current < 0 {
                        parent[child] = candidate as i64;
                    } else {
                        let candidate_x = proj.x(loops[candidate][0]);
                        let candidate_y = proj.y(loops[candidate][0]);
                        if proj.point_inside(&loops[current as usize], candidate_x, candidate_y) {
                            parent[child] = candidate as i64;
                        }
                    }
                }
            }
        }
    }
    let mut depth = vec![0usize; loop_count];
    for loop_index in 0..loop_count {
        ctx.poll(loop_index, 255)?;
        let mut current = parent[loop_index];
        let mut value = 0;
        while current >= 0 {
            value += 1;
            if value > loop_count {
                return Err(fail("Rdk.Plane_clip.clip: cyclic cap contour containment"));
            }
            current = parent[current as usize];
        }
        depth[loop_index] = value;
    }
    let mut child_holes: Vec<Vec<Vec<i64>>> = vec![Vec::new(); loop_count];
    for child in 0..loop_count {
        let container = parent[child];
        if container >= 0 {
            child_holes[container as usize].push(loops[child].iter().map(|&t| t as i64).collect());
        }
    }
    let mut triangulated: Vec<Option<Result<Vec<i64>, String>>> = vec![None; loop_count];
    for outer in 0..loop_count {
        if depth[outer] & 1 == 0 && !child_holes[outer].is_empty() {
            let point = |token: i64| (proj.x(token as usize), proj.y(token as usize));
            let outer_tokens: Vec<i64> = loops[outer].iter().map(|&t| t as i64).collect();
            triangulated[outer] = Some(contour_triangulation::triangulate(
                ctx.cancel_ref(),
                &point,
                &outer_tokens,
                &child_holes[outer],
            )?);
        }
    }
    let want_positive = side == 1;
    for outer in 0..loop_count {
        ctx.poll(outer, 255)?;
        if depth[outer] & 1 != 0 {
            continue;
        }
        if child_holes[outer].is_empty() {
            let loop_tokens = &loops[outer];
            let reverse = (loop_orientation[outer] > 0.0) != want_positive;
            let start = b.corner_count();
            for local in 0..loop_tokens.len() {
                let source_index = if reverse { loop_tokens.len() - local - 1 } else { local };
                b.add_corner(loop_tokens[source_index], -1, -1, 0.0);
            }
            b.finish_primitive(start, 0, -1, side, true, true, 3);
        } else {
            let triangles = match &triangulated[outer] {
                Some(Ok(triangles)) => triangles,
                Some(Err(message)) => {
                    return Err(fail(&format!("Rdk.Plane_clip.clip: cap contours: {message}")));
                }
                None => return Err(fail("Rdk.Plane_clip.clip: missing cap contour triangulation plan")),
            };
            for triangle in 0..triangles.len() / 3 {
                let at = triangle * 3;
                let (ta, tb, tc) = (triangles[at] as usize, triangles[at + 1] as usize, triangles[at + 2] as usize);
                let (ax, ay, az) = ctx.position_of_geometry_token(ta)?;
                let (bx, by, bz) = ctx.position_of_geometry_token(tb)?;
                let (cx, cy, cz) = ctx.position_of_geometry_token(tc)?;
                let abx = (bx - ax) / cap_coordinate_scale;
                let aby = (by - ay) / cap_coordinate_scale;
                let abz = (bz - az) / cap_coordinate_scale;
                let acx = (cx - ax) / cap_coordinate_scale;
                let acy = (cy - ay) / cap_coordinate_scale;
                let acz = (cz - az) / cap_coordinate_scale;
                let orientation = ((aby * acz - abz * acy) * nx)
                    + ((abz * acx - abx * acz) * ny)
                    + ((abx * acy - aby * acx) * nz);
                if !orientation.is_finite() || orientation == 0.0 {
                    return Err(fail(
                        "Rdk.Plane_clip.clip: cap contour triangulation emitted a degenerate triangle",
                    ));
                }
                let (tb, tc) = if (orientation > 0.0) == want_positive { (tb, tc) } else { (tc, tb) };
                let start = b.corner_count();
                b.add_corner(ta, -1, -1, 0.0);
                b.add_corner(tb, -1, -1, 0.0);
                b.add_corner(tc, -1, -1, 0.0);
                b.finish_primitive(start, 0, -1, side, true, true, 3);
            }
        }
    }
    Ok(())
}

fn float_plane(
    cancel_ref: Option<&cancel::T>,
    grain: i64,
    source: &[f64],
    a: &[i64],
    b: &[i64],
    t: &[f64],
) -> Result<Vec<f64>, error::T> {
    let mut output = vec![0.0; a.len()];
    support::par_fill(cancel_ref, grain, &mut output, &|index| {
        let at = index as usize;
        Ok(if a[at] >= 0 {
            let sa = source[a[at] as usize];
            let sb = source[b[at] as usize];
            sa + (sb - sa) * t[at]
        } else {
            0.0
        })
    })?;
    Ok(output)
}

pub fn clip(params: &Params, geometry_value: &geometry::T) -> Result<geometry::T, error::T> {
    run(params, geometry_value).map_err(|value| match error::code(&value) {
        Code::Cancelled => error::cancelled("clip"),
        Code::InvalidArgument => value,
        _ => make("clip", Code::InvalidGeometry, error::message(&value)),
    })
}

fn run(params: &Params, geometry_value: &geometry::T) -> Result<geometry::T, error::T> {
    let cancel_ref = params.cancel.as_deref();
    let grain = params.grain;
    support::check_positive(grain).map_err(|_| make("clip", Code::InvalidArgument, "Rdk.Plane_clip.clip: grain must be positive"))?;
    cancel::check_opt(cancel_ref)?;
    let (ox, oy, oz) = (params.origin.x, params.origin.y, params.origin.z);
    let (supplied_nx, supplied_ny, supplied_nz) = (params.normal.x, params.normal.y, params.normal.z);
    if !(ox.is_finite()
        && oy.is_finite()
        && oz.is_finite()
        && supplied_nx.is_finite()
        && supplied_ny.is_finite()
        && supplied_nz.is_finite())
    {
        return Err(fail("Rdk.Plane_clip.clip: plane origin and normal must be finite"));
    }
    let snapping_tolerance = params.snapping_tolerance;
    if !snapping_tolerance.is_finite() || snapping_tolerance < 0.0 {
        return Err(fail("Rdk.Plane_clip.clip: snapping tolerance must be finite and non-negative"));
    }
    if (&params.clip_attribute).trim().is_empty() {
        return Err(fail("Rdk.Plane_clip.clip: clip attribute must not be empty"));
    }
    let distance = params.distance;
    if !distance.is_finite() {
        return Err(fail("Rdk.Plane_clip.clip: plane distance must be finite"));
    }
    let keep = params.keep;
    if params.split_connectivity && keep != Keep::All {
        return Err(fail("Rdk.Plane_clip.clip: split connectivity requires keep=All"));
    }
    let mut seen_names: Vec<&str> = Vec::new();
    for name in [&params.cap_group, &params.clipped_group, &params.above_group, &params.below_group] {
        if let Some(name) = name {
            if (name).trim().is_empty() {
                return Err(fail("Rdk.Plane_clip.clip: output group names must not be empty"));
            }
            if seen_names.contains(&name.as_str()) {
                return Err(fail("Rdk.Plane_clip.clip: output group names must be distinct"));
            }
            seen_names.push(name);
        }
    }
    if let Some(name) = &params.clipped_edge_group {
        if (name).trim().is_empty() {
            return Err(fail("Rdk.Plane_clip.clip: clipped edge group name must not be empty"));
        }
    }
    let normal_scale = float_max(supplied_nx.abs(), float_max(supplied_ny.abs(), supplied_nz.abs()));
    if normal_scale == 0.0 {
        return Err(fail("Rdk.Plane_clip.clip: plane normal must be non-zero"));
    }
    let scaled_nx = supplied_nx / normal_scale;
    let scaled_ny = supplied_ny / normal_scale;
    let scaled_nz = supplied_nz / normal_scale;
    let normal_length = (scaled_nx * scaled_nx + scaled_ny * scaled_ny + scaled_nz * scaled_nz).sqrt();
    let nx = scaled_nx / normal_length;
    let ny = scaled_ny / normal_length;
    let nz = scaled_nz / normal_length;
    let point_count = geometry::point_count(geometry_value) as usize;
    let positions = packed::float3::view::view(geometry::positions(geometry_value))?;
    let clip_attribute = params.clip_attribute.as_str();
    let (clip_x, clip_y, clip_z): (Vec<f64>, Option<Vec<f64>>, Option<Vec<f64>>) = if clip_attribute == "P" {
        (positions.x.to_vec(), Some(positions.y.to_vec()), Some(positions.z.to_vec()))
    } else {
        match geometry::find_attribute(attribute::Owner::Point, clip_attribute, geometry_value) {
            None => {
                return Err(fail(&format!(
                    "Rdk.Plane_clip.clip: point clip attribute \"{clip_attribute}\" does not exist"
                )));
            }
            Some(found) => match attribute::view::storage(&found)? {
                attribute::Storage::Float(values) => (values.to_vec(), None, None),
                attribute::Storage::Int(values) => (values.iter().map(|&v| v as f64).collect(), None, None),
                attribute::Storage::Float2(values) => {
                    let values = packed::float2::view::view(values);
                    (values.x.to_vec(), Some(values.y.to_vec()), None)
                }
                attribute::Storage::Float3(values) => {
                    let values = packed::float3::view::view(values)?;
                    (values.x.to_vec(), Some(values.y.to_vec()), Some(values.z.to_vec()))
                }
                attribute::Storage::Float4(values) => {
                    let values = packed::float4::view::view(values);
                    (values.x.to_vec(), Some(values.y.to_vec()), Some(values.z.to_vec()))
                }
                attribute::Storage::IntArray(_)
                | attribute::Storage::FloatArray(_)
                | attribute::Storage::Text(_) => {
                    return Err(fail(&format!(
                        "Rdk.Plane_clip.clip: point clip attribute \"{clip_attribute}\" must be a scalar or fixed-width numeric tuple"
                    )));
                }
            },
        }
    };
    let (mut raw_distance, mut classified_distance) = (vec![0.0f64; point_count], vec![0.0f64; point_count]);
    {
        let (wr, wc) = (support::Windows::even(&mut raw_distance, grain), support::Windows::even(&mut classified_distance, grain));
        support::run_indexed_ranges(grain, point_count as i64, &|range, first, last| {
            let (raw_distance, classified_distance) = (wr.take(range), wc.take(range));
            for element in first..last {
                support::poll(cancel_ref, element)?;
                let local = (element - first) as usize;
                let point = element as usize;
                let (x, y, z) = (positions.x[point], positions.y[point], positions.z[point]);
                if !(x.is_finite() && y.is_finite() && z.is_finite()) {
                    return Err(fail("Rdk.Plane_clip.clip: point positions must be finite"));
                }
                let cx = clip_x[point];
                let cy = clip_y.as_ref().map_or(0.0, |values| values[point]);
                let cz = clip_z.as_ref().map_or(0.0, |values| values[point]);
                if !(cx.is_finite() && cy.is_finite() && cz.is_finite()) {
                    return Err(fail(&format!(
                        "Rdk.Plane_clip.clip: point clip attribute \"{clip_attribute}\" must contain finite values"
                    )));
                }
                let value = (cx - ox) * nx + (cy - oy) * ny + (cz - oz) * nz - distance;
                if !value.is_finite() {
                    return Err(fail("Rdk.Plane_clip.clip: signed plane distance overflowed"));
                }
                raw_distance[local] = value;
                classified_distance[local] = if value.abs() <= snapping_tolerance { 0.0 } else { value };
            }
            Ok(())
        })?;
    }
    let source_topology = geometry::topology(geometry_value);
    let topology_view = topology::view::view(source_topology)?;
    let source_index = topology_index::create(cancel_ref, source_topology)?;
    let index_view = topology_index::view::view(&source_index);
    let primitive_selection = match &params.selection {
        None => None,
        Some(selection) => Some(element_selection::promote(
            cancel_ref,
            grain,
            Some("__clip_selection"),
            group::Owner::Primitive,
            selection,
            source_topology,
        )?),
    };
    let primitive_selected = |primitive: usize| match &primitive_selection {
        None => true,
        Some(selection) => group::mem(primitive as i64, selection),
    };
    let (affected_points, shared_points): (Option<Vec<u8>>, Option<Vec<u8>>) = match &primitive_selection {
        None => (None, None),
        Some(_) => {
            let (mut affected, mut shared) = (vec![0u8; point_count], vec![0u8; point_count]);
            {
                let (wa, ws) = (support::Windows::even(&mut affected, grain), support::Windows::even(&mut shared, grain));
                support::run_indexed_ranges(grain, point_count as i64, &|range, first, last| {
                    let (affected, shared) = (wa.take(range), ws.take(range));
                    for element in first..last {
                        support::poll(cancel_ref, element)?;
                        let local = (element - first) as usize;
                        let point = element as usize;
                        let mut has_selected = false;
                        let mut has_unselected = false;
                        let mut at = index_view.point_offsets[point] as usize;
                        let last = index_view.point_offsets[point + 1] as usize;
                        while at < last && !(has_selected && has_unselected) {
                            let vertex = index_view.point_vertices[at] as usize;
                            if primitive_selected(index_view.primitive_of_vertex[vertex] as usize) {
                                has_selected = true;
                            } else {
                                has_unselected = true;
                            }
                            at += 1;
                        }
                        if has_selected {
                            affected[local] = 1;
                        }
                        if has_selected && has_unselected {
                            shared[local] = 1;
                        }
                    }
                    Ok(())
                })?;
            }
            if let Some(element_selection::T::SelectedPoints(selected)) = &params.selection {
                for point in 0..point_count {
                    if group::mem(point as i64, selected) {
                        affected[point] = 1;
                    }
                }
            }
            (Some(affected), Some(shared))
        }
    };
    let (shared_token_of_point, shared_point_of_token): (Vec<i64>, Vec<usize>) = match &shared_points {
        None => (Vec::new(), Vec::new()),
        Some(shared) => {
            let count = shared.iter().filter(|&&flag| flag != 0).count();
            if count == 0 {
                (Vec::new(), Vec::new())
            } else {
                let mut mapping = vec![-1i64; point_count];
                let mut points = Vec::with_capacity(count);
                for point in 0..point_count {
                    if shared[point] != 0 {
                        mapping[point] = points.len() as i64;
                        points.push(point);
                    }
                }
                (mapping, points)
            }
        }
    };
    let edge_count = topology_index::edge_count(&source_index) as usize;
    let base_token_count = point_count + edge_count;
    let split = keep == Keep::All && params.split_connectivity;
    let selected_token_count = if split { base_token_count * 2 } else { base_token_count };
    let passthrough_offset = selected_token_count;
    let shared_token_count = shared_point_of_token.len();
    let has_isolation_tokens = shared_token_count > 0;
    let geometry_token_count = selected_token_count + shared_token_count;
    let ctx = Ctx {
        cancel: params.cancel.clone(),
        fill: params.fill,
        keep,
        nx,
        ny,
        nz,
        point_count,
        px: positions.x.clone(),
        py: positions.y.clone(),
        pz: positions.z.clone(),
        raw_distance,
        classified_distance,
        vertex_points: topology_view.vertex_points.clone(),
        primitive_offsets: topology_view.primitive_offsets.clone(),
        primitive_kinds: topology_view.primitive_kinds.clone(),
        edge_a: index_view.edge_a.clone(),
        edge_b: index_view.edge_b.clone(),
        edge_of_vertex: index_view.edge_of_vertex.clone(),
        affected_points,
        shared_token_of_point,
        shared_point_of_token,
        split,
        base_token_count,
        passthrough_offset,
        has_isolation_tokens,
        primitive_selection,
    };
    let mut referenced = vec![false; point_count];
    for &point in topology_view.vertex_points.iter() {
        referenced[point as usize] = true;
    }
    let mut standalone_used = vec![false; geometry_token_count];
    for point in 0..point_count {
        if !referenced[point] && !ctx.point_affected(point) {
            standalone_used[ctx.token(0, point)] = true;
        } else if !referenced[point] {
            let d = ctx.classified_distance[point];
            match keep {
                Keep::Above => {
                    if d >= 0.0 {
                        standalone_used[ctx.token(0, point)] = true;
                    }
                }
                Keep::Below => {
                    if d <= 0.0 {
                        standalone_used[ctx.token(1, point)] = true;
                    }
                }
                Keep::All => {
                    let side = if d < 0.0 { 1 } else { 0 };
                    standalone_used[ctx.token(side, point)] = true;
                }
            }
        }
    }
    let primitive_count = geometry::primitive_count(geometry_value) as usize;
    let plan = fast_plan(&ctx, grain, primitive_count)?;
    let mut builder = Builder::new();
    let mut segs = Segments { a: Vec::new(), b: Vec::new(), side: Vec::new() };
    match plan {
        Some((slots, candidate_output, offsets)) => {
            let output_count = offsets.len() - 1;
            let total = offsets[output_count] as usize;
            builder.tokens = vec![0; total];
            builder.corner_a = vec![-1; total];
            builder.corner_b = vec![-1; total];
            builder.corner_t = vec![0.0; total];
            builder.offsets = offsets;
            builder.kinds = vec![0; output_count];
            builder.primitive_source = vec![-1; output_count];
            builder.primitive_side = vec![0; output_count];
            builder.primitive_clipped = vec![false; output_count];
            builder.primitive_cap = vec![false; output_count];
            fill_fast_plan(&ctx, grain, &mut builder, slots, &candidate_output)?;
        }
        None => {
            for primitive in 0..primitive_count {
                cancel::poll(cancel_ref, primitive as i64, 1023)?;
                if !ctx.primitive_selected(primitive) {
                    ctx.process_passthrough(&mut builder, primitive);
                } else {
                    let kind = ctx.primitive_kinds[primitive];
                    let sides: Vec<usize> = match keep {
                        Keep::Above => vec![0],
                        Keep::Below => vec![1],
                        Keep::All => {
                            let first = ctx.primitive_offsets[primitive] as usize;
                            let last = ctx.primitive_offsets[primitive + 1] as usize;
                            let has_negative = (first..last)
                                .any(|vertex| ctx.classified_distance[ctx.vertex_points[vertex] as usize] < 0.0);
                            if has_negative { vec![0, 1] } else { vec![0] }
                        }
                    };
                    for side in sides {
                        if kind == 0 {
                            ctx.process_polygon(&mut builder, &mut segs, primitive, side)?;
                        } else {
                            ctx.process_curve(&mut builder, primitive, side)?;
                        }
                    }
                }
            }
        }
    }
    if params.fill && !segs.a.is_empty() {
        match keep {
            Keep::Above => trace_caps(&ctx, &mut builder, &segs, geometry_token_count, 0)?,
            Keep::Below => trace_caps(&ctx, &mut builder, &segs, geometry_token_count, 1)?,
            Keep::All => {
                trace_caps(&ctx, &mut builder, &segs, geometry_token_count, 0)?;
                trace_caps(&ctx, &mut builder, &segs, geometry_token_count, 1)?;
            }
        }
    }
    let total_tokens = geometry_token_count;
    let mut used = standalone_used;
    for &token in &builder.tokens {
        used[token] = true;
    }
    let mut token_to_point = vec![-1i64; total_tokens];
    let mut point_token: Vec<usize> = Vec::new();
    for value in 0..total_tokens {
        if used[value] {
            token_to_point[value] = point_token.len() as i64;
            point_token.push(value);
        }
    }
    let output_point_count = point_token.len();
    let n = output_point_count as i64;
    let len = output_point_count;
    let (mut point_source_a, mut point_source_b, mut point_source_t) = (vec![0i64; len], vec![0i64; len], vec![0.0f64; len]);
    let (mut px, mut py, mut pz) = (vec![0.0f64; len], vec![0.0f64; len], vec![0.0f64; len]);
    {
        let (wa, wb, wt) = (
            support::Windows::even(&mut point_source_a, grain),
            support::Windows::even(&mut point_source_b, grain),
            support::Windows::even(&mut point_source_t, grain),
        );
        let (wx, wy, wz) = (
            support::Windows::even(&mut px, grain),
            support::Windows::even(&mut py, grain),
            support::Windows::even(&mut pz, grain),
        );
        support::run_indexed_ranges(grain, n, &|range, first, last| {
            let (point_source_a, point_source_b, point_source_t) = (wa.take(range), wb.take(range), wt.take(range));
            let (px, py, pz) = (wx.take(range), wy.take(range), wz.take(range));
            for element in first..last {
                support::poll(cancel_ref, element)?;
                let local = (element - first) as usize;
                let output = element as usize;
                let value = point_token[output];
                if ctx.is_passthrough_token(value) {
                    let point = ctx.passthrough_point(value);
                    point_source_a[local] = point as i64;
                    point_source_b[local] = point as i64;
                    px[local] = positions.x[point];
                    py[local] = positions.y[point];
                    pz[local] = positions.z[point];
                } else {
                    let base = ctx.base_of_token(value);
                    if base < point_count {
                        point_source_a[local] = base as i64;
                        point_source_b[local] = base as i64;
                        let projection = ctx.projection(base);
                        px[local] = positions.x[base] - projection * nx;
                        py[local] = positions.y[base] - projection * ny;
                        pz[local] = positions.z[base] - projection * nz;
                    } else {
                        let edge = base - point_count;
                        let a = index_view.edge_a[edge] as usize;
                        let c = index_view.edge_b[edge] as usize;
                        let t = clamp01(-ctx.raw_distance[a] / (ctx.raw_distance[c] - ctx.raw_distance[a]));
                        point_source_a[local] = a as i64;
                        point_source_b[local] = c as i64;
                        point_source_t[local] = t;
                        px[local] = positions.x[a] + (positions.x[c] - positions.x[a]) * t;
                        py[local] = positions.y[a] + (positions.y[c] - positions.y[a]) * t;
                        pz[local] = positions.z[a] + (positions.z[c] - positions.z[a]) * t;
                    }
                }
            }
            Ok(())
        })?;
    }
    let corner_count = builder.corner_count();
    let primitive_total = builder.kinds.len();
    let output_vertex_points: Vec<i64> = builder.tokens.iter().map(|&token| token_to_point[token]).collect();
    let mut corner_cap_side = vec![-1i64; corner_count];
    for primitive in 0..primitive_total {
        if builder.primitive_cap[primitive] {
            for corner in builder.offsets[primitive] as usize..builder.offsets[primitive + 1] as usize {
                corner_cap_side[corner] = builder.primitive_side[primitive];
            }
        }
    }
    let output_topology = topology::view::create_validated_owned(
        output_point_count as i64,
        output_vertex_points.clone(),
        builder.offsets.clone(),
        builder.kinds.clone(),
    );
    let primitive_source = &builder.primitive_source;
    let remap_attribute = |source: &attribute::T| -> Result<attribute::T, error::T> {
        let owner = attribute::owner(source);
        let storage = attribute::view::storage(source)?;
        let name = attribute::name(source);
        match owner {
            attribute::Owner::Detail => Ok(source.clone()),
            attribute::Owner::Primitive => {
                let storage = topology_remap::select_storage(cancel_ref, grain, true, primitive_source, storage)?;
                attribute::create_owned(name, owner, storage)
            }
            attribute::Owner::Point | attribute::Owner::Vertex => {
                let (mapping_a, mapping_b, mapping_t): (&[i64], &[i64], &[f64]) =
                    if owner == attribute::Owner::Point {
                        (&point_source_a, &point_source_b, &point_source_t)
                    } else {
                        (&builder.corner_a, &builder.corner_b, &builder.corner_t)
                    };
                let count = mapping_a.len();
                let nearest_index = |index: usize| -> i64 {
                    let a = mapping_a[index];
                    let c = mapping_b[index];
                    if a < 0 {
                        -1
                    } else if mapping_t[index] < 0.5 {
                        a
                    } else if mapping_t[index] > 0.5 {
                        c
                    } else {
                        a.min(c)
                    }
                };
                let nearest_mapping: Vec<i64> = (0..count).map(nearest_index).collect();
                let is_normal = name == "N";
                let plane = |values: &[f64]| float_plane(cancel_ref, grain, values, mapping_a, mapping_b, mapping_t);
                let storage = match storage {
                    attribute::Storage::Float3(values) if is_normal => {
                        let values = packed::float3::view::view(values)?;
                        let mut x = plane(&values.x)?;
                        let mut y = plane(&values.y)?;
                        let mut z = plane(&values.z)?;
                        for index in 0..count {
                            let cap_side = if owner == attribute::Owner::Point { -1 } else { corner_cap_side[index] };
                            if cap_side >= 0 {
                                let direction = if cap_side == 0 { -1.0 } else { 1.0 };
                                x[index] = direction * nx;
                                y[index] = direction * ny;
                                z[index] = direction * nz;
                            } else {
                                let length = (x[index] * x[index] + y[index] * y[index] + z[index] * z[index]).sqrt();
                                if length > 1e-20 {
                                    x[index] /= length;
                                    y[index] /= length;
                                    z[index] /= length;
                                }
                            }
                        }
                        attribute::Storage::Float3(packed::float3::view::of_owned_exn(x, y, z)?)
                    }
                    storage => match attribute::map_float_planes(storage, plane)? {
                        Some(blended) => blended,
                        None => topology_remap::select_storage(cancel_ref, grain, true, &nearest_mapping, storage)?,
                    },
                };
                attribute::create_owned(name, owner, storage)
            }
        }
    };
    let mut attributes: Vec<attribute::T> = Vec::new();
    for source in geometry::attributes(geometry_value) {
        attributes.push(remap_attribute(&source)?);
    }
    let has_polygon = builder.kinds.iter().any(|&kind| kind == 0);
    let has_cap = builder.primitive_cap.iter().any(|&cap| cap);
    let source_vertex_normal = geometry::find_attribute(attribute::Owner::Vertex, "N", geometry_value)
        .is_some_and(|found| matches!(attribute::view::storage(&found), Ok(attribute::Storage::Float3(_))));
    let output_point_normal: Option<packed::float3::T> = attributes
        .iter()
        .find(|found| attribute::owner(found) == attribute::Owner::Point && attribute::name(found) == "N")
        .and_then(|found| match attribute::view::storage(found) {
            Ok(attribute::Storage::Float3(values)) => Some(values.clone()),
            _ => None,
        });
    if !source_vertex_normal && has_polygon {
        match output_point_normal {
            Some(point_normals) if has_cap => {
                let point_normals = packed::float3::view::view(&point_normals)?;
                let mut vx = vec![0.0f64; corner_count];
                let mut vy = vec![0.0f64; corner_count];
                let mut vz = vec![0.0f64; corner_count];
                for corner in 0..corner_count {
                    let cap_side = corner_cap_side[corner];
                    if cap_side >= 0 {
                        let direction = if cap_side == 0 { -1.0 } else { 1.0 };
                        vx[corner] = direction * nx;
                        vy[corner] = direction * ny;
                        vz[corner] = direction * nz;
                    } else {
                        let point = output_vertex_points[corner] as usize;
                        vx[corner] = point_normals.x[point];
                        vy[corner] = point_normals.y[point];
                        vz[corner] = point_normals.z[point];
                    }
                }
                attributes.push(attribute::create_key_owned(
                    &attribute::normal(attribute::Owner::Vertex),
                    attribute::Storage::Float3(packed::float3::view::of_owned_exn(vx, vy, vz)?),
                )?);
            }
            Some(_) => {}
            None => {
                let (mut vx, mut vy, mut vz) = (vec![0.0; corner_count], vec![0.0; corner_count], vec![0.0; corner_count]);
                let normal_grain = (grain / 8).max(1);
                let ranges = support::range_count(primitive_total as i64, normal_grain);
                let bound = |range: i64| builder.offsets[(range * normal_grain).min(primitive_total as i64) as usize] as usize;
                {
                    let (wx, wy, wz) = (
                        support::Windows::new(&mut vx, ranges, bound),
                        support::Windows::new(&mut vy, ranges, bound),
                        support::Windows::new(&mut vz, ranges, bound),
                    );
                    support::run_indexed_ranges(normal_grain, primitive_total as i64, &|range, first_primitive, last_primitive| {
                        let (vx, vy, vz) = (wx.take(range), wy.take(range), wz.take(range));
                        let base = bound(range);
                        for primitive in first_primitive..last_primitive {
                            support::poll(cancel_ref, primitive)?;
                            let primitive = primitive as usize;
                            if builder.kinds[primitive] == 0 {
                                let first = builder.offsets[primitive] as usize;
                                let last = builder.offsets[primitive + 1] as usize;
                                let (mut sx, mut sy, mut sz) = (0.0, 0.0, 0.0);
                                for corner in first..last {
                                    let next = if corner + 1 == last { first } else { corner + 1 };
                                    let a = output_vertex_points[corner] as usize;
                                    let c = output_vertex_points[next] as usize;
                                    sx += (py[a] - py[c]) * (pz[a] + pz[c]);
                                    sy += (pz[a] - pz[c]) * (px[a] + px[c]);
                                    sz += (px[a] - px[c]) * (py[a] + py[c]);
                                }
                                let length: f64 = (sx * sx + sy * sy + sz * sz).sqrt();
                                if length > 1e-20 {
                                    for corner in first..last {
                                        vx[corner - base] = sx / length;
                                        vy[corner - base] = sy / length;
                                        vz[corner - base] = sz / length;
                                    }
                                }
                            }
                        }
                        Ok(())
                    })?;
                }
                attributes.push(attribute::create_key_owned(
                    &attribute::normal(attribute::Owner::Vertex),
                    attribute::Storage::Float3(packed::float3::view::of_owned_exn(vx, vy, vz)?),
                )?);
            }
        }
    }
    let corner_a = &builder.corner_a;
    let corner_b = &builder.corner_b;
    let mut groups: Vec<group::T> = Vec::new();
    for source in geometry::groups(geometry_value) {
        let name = group::name(&source).to_owned();
        let (target, source_of_target): (group::T, Vec<i64>) = match group::owner(&source) {
            group::Owner::Point => (
                group::init(Some(grain), group::Owner::Point, &name, output_point_count as i64, &|output| Ok({
                    let a = point_source_a[output as usize];
                    let c = point_source_b[output as usize];
                    group::mem(a, &source) || group::mem(c, &source)
                }))?,
                if group::is_ordered(&source) {
                    (0..output_point_count)
                        .map(|output| {
                            let a = point_source_a[output];
                            let c = point_source_b[output];
                            if a == c { a } else { -1 }
                        })
                        .collect()
                } else {
                    Vec::new()
                },
            ),
            group::Owner::Vertex => (
                group::init(Some(grain), group::Owner::Vertex, &name, corner_count as i64, &|output| Ok({
                    let a = corner_a[output as usize];
                    let c = corner_b[output as usize];
                    a >= 0 && (group::mem(a, &source) || group::mem(c, &source))
                }))?,
                if group::is_ordered(&source) {
                    (0..corner_count)
                        .map(|output| {
                            let a = corner_a[output];
                            let c = corner_b[output];
                            if a >= 0 && a == c { a } else { -1 }
                        })
                        .collect()
                } else {
                    Vec::new()
                },
            ),
            group::Owner::Primitive => (
                group::init(Some(grain), group::Owner::Primitive, &name, primitive_total as i64, &|output| Ok({
                    let origin = primitive_source[output as usize];
                    origin >= 0 && group::mem(origin, &source)
                }))?,
                if group::is_ordered(&source) { primitive_source.clone() } else { Vec::new() },
            ),
        };
        if group::is_ordered(&source) {
            groups.push(group::view::remap_order(&source, &source_of_target, &target)?);
        } else {
            groups.push(target);
        }
    }
    let source_edge_groups = geometry::edge_groups(geometry_value);
    let target_index = if source_edge_groups.is_empty() && params.clipped_edge_group.is_none() {
        None
    } else {
        Some(topology_index::create(cancel_ref, &output_topology)?)
    };
    let mut edge_groups: Vec<edge_group::T> = Vec::new();
    if !source_edge_groups.is_empty() {
        let Some(target_index) = &target_index else {
            return Err(fail("Rdk.Plane_clip.clip: missing target edge index"));
        };
        let target_view = topology_index::view::view(target_index);
        let source_edge_of_output_edge = |edge: usize| -> Option<i64> {
            let a = target_view.edge_a[edge] as usize;
            let c = target_view.edge_b[edge] as usize;
            let aa = point_source_a[a];
            let ab = point_source_b[a];
            let ba = point_source_a[c];
            let bb = point_source_b[c];
            let candidate = if aa != ab {
                topology_index::find_edge(&source_index, aa, ab)
            } else if ba != bb {
                topology_index::find_edge(&source_index, ba, bb)
            } else {
                topology_index::find_edge(&source_index, aa, ba)
            };
            let source_edge = candidate?;
            let (ea, eb) = topology_index::edge_points(&source_index, source_edge).ok()?;
            let on_edge = |x: i64, y: i64| (x == ea || x == eb) && (y == ea || y == eb);
            if on_edge(aa, ab) && on_edge(ba, bb) { Some(source_edge) } else { None }
        };
        for source_group in &source_edge_groups {
            edge_groups.push(edge_group::init(
                Some(grain),
                &output_topology,
                target_index,
                edge_group::name(source_group),
                &|edge| Ok(match source_edge_of_output_edge(edge as usize) {
                    None => false,
                    Some(source_edge) => edge_group::mem(source_edge, source_group),
                }),
            )?);
        }
    }
    let output = geometry::create(
        packed::float3::view::of_owned_exn(px, py, pz)?,
        output_topology.clone(),
        Some(attributes),
        Some(groups),
        Some(edge_groups),
    )?;
    let output = match (&params.clipped_edge_group, &target_index) {
        (None, _) => output,
        (Some(name), Some(target_index)) => {
            let target_view = topology_index::view::view(target_index);
            let generated = edge_group::init(Some(grain), &output_topology, target_index, name, &|edge| Ok({
                let a = target_view.edge_a[edge as usize] as usize;
                let c = target_view.edge_b[edge as usize] as usize;
                ctx.token_on_plane(point_token[a]) && ctx.token_on_plane(point_token[c])
            }))?;
            let merged = if params.replace_existing_groups {
                generated
            } else {
                match geometry::find_edge_group(name, &output) {
                    None => generated,
                    Some(existing) => edge_group::union(&existing, &generated)?,
                }
            };
            geometry::with_edge_group(merged, &output)?
        }
        (Some(_), None) => return Err(fail("Rdk.Plane_clip.clip: missing target edge index")),
    };
    let primitive_cap = &builder.primitive_cap;
    let primitive_side = &builder.primitive_side;
    let primitive_clipped = &builder.primitive_clipped;
    let install = |name: &Option<String>,
                   predicate: &(dyn Fn(i64) -> bool + Send + Sync),
                   output: geometry::T|
     -> Result<geometry::T, error::T> {
        match name {
            None => Ok(output),
            Some(name) => {
                let generated =
                    group::init(Some(grain), group::Owner::Primitive, name, primitive_total as i64, &|index| Ok(predicate(index)))?;
                let merged = if params.replace_existing_groups {
                    generated
                } else {
                    match geometry::find_group(group::Owner::Primitive, name, &output) {
                        None => generated,
                        Some(existing) => group::union(&existing, &generated)?,
                    }
                };
                geometry::with_group(merged, &output)
            }
        }
    };
    let output = install(&params.cap_group, &|p| primitive_cap[p as usize], output)?;
    let output = install(
        &params.clipped_group,
        &|p| primitive_clipped[p as usize] && !primitive_cap[p as usize],
        output,
    )?;
    let output = install(
        &params.above_group,
        &|p| primitive_side[p as usize] == 0 && !primitive_cap[p as usize],
        output,
    )?;
    install(
        &params.below_group,
        &|p| primitive_side[p as usize] == 1 && !primitive_cap[p as usize],
        output,
    )
}

#[cfg(test)]
pub(crate) mod test_util {
    use rdk_core::error::Code;
    use rdk_core::{attribute, error, geometry, packed, topology};

    pub(crate) fn geom(x: Vec<f64>, y: Vec<f64>, z: Vec<f64>, polys: &[(topology::PrimitiveKind, Vec<i64>)]) -> geometry::T {
        let mut vertex_points = Vec::new();
        let mut offsets = vec![0i64];
        let mut kinds = Vec::new();
        for (kind, points) in polys {
            vertex_points.extend_from_slice(points);
            offsets.push(vertex_points.len() as i64);
            kinds.push(*kind);
        }
        let topo = topology::create_owned(x.len() as i64, vertex_points, offsets, kinds).unwrap();
        geometry::create(packed::float3::view::of_owned_exn(x, y, z).unwrap(), topo, None, None, None).unwrap()
    }

    pub(crate) fn polygons(x: Vec<f64>, y: Vec<f64>, z: Vec<f64>, polys: &[Vec<i64>]) -> geometry::T {
        let list: Vec<(topology::PrimitiveKind, Vec<i64>)> =
            polys.iter().map(|p| (topology::PrimitiveKind::Polygon, p.clone())).collect();
        geom(x, y, z, &list)
    }

    pub(crate) fn triangle() -> geometry::T {
        polygons(vec![-10.0, 0.0, 10.0], vec![0.0, 4.0, 0.0], vec![0.0; 3], &[vec![0, 1, 2]])
    }

    pub(crate) fn with_attribute(name: &str, storage: attribute::Storage, source: &geometry::T) -> geometry::T {
        with_owned_attribute(name, attribute::Owner::Point, storage, source)
    }

    pub(crate) fn with_owned_attribute(
        name: &str,
        owner: attribute::Owner,
        storage: attribute::Storage,
        source: &geometry::T,
    ) -> geometry::T {
        geometry::with_attribute(attribute::create_owned(name, owner, storage).unwrap(), source).unwrap()
    }

    pub(crate) fn int_attribute(owner: attribute::Owner, name: &str, source: &geometry::T) -> Vec<i64> {
        let found = geometry::find_attribute(owner, name, source).expect("missing attribute");
        match attribute::view::storage(&found).unwrap() {
            attribute::Storage::Int(values) => values.to_vec(),
            _ => panic!("{name} has unexpected storage"),
        }
    }

    pub(crate) fn xyz(source: &geometry::T) -> (Vec<f64>, Vec<f64>, Vec<f64>) {
        let view = packed::float3::view::view(geometry::positions(source)).unwrap();
        (view.x.to_vec(), view.y.to_vec(), view.z.to_vec())
    }

    pub(crate) fn topo(source: &geometry::T) -> topology::view::View {
        topology::view::view(geometry::topology(source)).unwrap()
    }

    /// Eight-point quad box with outward winding.
    pub(crate) fn cuboid(center: (f64, f64, f64), size: (f64, f64, f64)) -> geometry::T {
        let mut x = Vec::new();
        let mut y = Vec::new();
        let mut z = Vec::new();
        for i in 0..8 {
            x.push(center.0 + if i & 1 == 0 { -size.0 } else { size.0 } / 2.0);
            y.push(center.1 + if i & 2 == 0 { -size.1 } else { size.1 } / 2.0);
            z.push(center.2 + if i & 4 == 0 { -size.2 } else { size.2 } / 2.0);
        }
        polygons(
            x,
            y,
            z,
            &[
                vec![4, 5, 7, 6],
                vec![0, 2, 3, 1],
                vec![1, 3, 7, 5],
                vec![0, 4, 6, 2],
                vec![2, 6, 7, 3],
                vec![0, 1, 5, 4],
            ],
        )
    }

    /// A counterclockwise C-shaped polygon in the XY plane.
    pub(crate) fn c_outline() -> (Vec<f64>, Vec<f64>) {
        (
            vec![-2.0, 2.0, 2.0, -1.0, -1.0, 2.0, 2.0, -2.0],
            vec![-2.0, -2.0, -1.0, -1.0, 1.0, 1.0, 2.0, 2.0],
        )
    }

    pub(crate) fn c_polygon() -> geometry::T {
        let (x, y) = c_outline();
        polygons(x, y, vec![0.0; 8], &[(0..8).collect()])
    }

    /// Closed prism of the C outline from z = 0 to `height`, outward winding.
    pub(crate) fn c_prism(height: f64) -> geometry::T {
        let (ox, oy) = c_outline();
        let n = ox.len();
        let mut x = ox.clone();
        let mut y = oy.clone();
        let mut z = vec![0.0; n];
        x.extend(ox);
        y.extend(oy);
        z.extend(vec![height; n]);
        let mut polys: Vec<Vec<i64>> = vec![(0..n as i64).rev().collect(), (n as i64..2 * n as i64).collect()];
        for i in 0..n as i64 {
            let j = (i + 1) % n as i64;
            polys.push(vec![i, j, j + n as i64, i + n as i64]);
        }
        polygons(x, y, z, &polys)
    }

    pub(crate) fn reverse(source: &geometry::T) -> geometry::T {
        let view = topo(source);
        let (x, y, z) = xyz(source);
        let mut polys = Vec::new();
        for p in 0..view.primitive_kinds.len() {
            let mut points: Vec<i64> =
                view.vertex_points[view.primitive_offsets[p] as usize..view.primitive_offsets[p + 1] as usize].to_vec();
            points.reverse();
            polys.push(points);
        }
        polygons(x, y, z, &polys)
    }

    /// Concatenates polygon-only geometries (no attributes).
    pub(crate) fn merge(parts: &[geometry::T]) -> geometry::T {
        let (mut x, mut y, mut z) = (Vec::new(), Vec::new(), Vec::new());
        let mut polys: Vec<Vec<i64>> = Vec::new();
        for part in parts {
            let base = x.len() as i64;
            let (px, py, pz) = xyz(part);
            x.extend(px);
            y.extend(py);
            z.extend(pz);
            let view = topo(part);
            for p in 0..view.primitive_kinds.len() {
                polys.push(
                    view.vertex_points[view.primitive_offsets[p] as usize..view.primitive_offsets[p + 1] as usize]
                        .iter()
                        .map(|&v| v + base)
                        .collect(),
                );
            }
        }
        polygons(x, y, z, &polys)
    }

    pub(crate) fn map_positions(source: &geometry::T, f: &dyn Fn(f64, f64, f64) -> (f64, f64, f64)) -> geometry::T {
        let (x, y, z) = xyz(source);
        let (mut nx, mut ny, mut nz) = (Vec::new(), Vec::new(), Vec::new());
        for i in 0..x.len() {
            let (a, b, c) = f(x[i], y[i], z[i]);
            nx.push(a);
            ny.push(b);
            nz.push(c);
        }
        geometry::with_positions(packed::float3::view::of_owned_exn(nx, ny, nz).unwrap(), source).unwrap()
    }

    pub(crate) fn near(a: f64, b: f64) -> bool {
        (a - b).abs() <= 1e-10
    }

    pub(crate) fn same_geometry(a: &geometry::T, b: &geometry::T) -> bool {
        let (ta, tb) = (topo(a), topo(b));
        xyz(a) == xyz(b)
            && ta.vertex_points == tb.vertex_points
            && ta.primitive_offsets == tb.primitive_offsets
            && ta.primitive_kinds == tb.primitive_kinds
            && geometry::groups(a).len() == geometry::groups(b).len()
    }

    pub(crate) fn expect_code(result: Result<geometry::T, error::T>, code: Code) {
        match result {
            Err(e) => assert_eq!(error::code(&e), code, "{}", error::to_string(&e)),
            Ok(_) => panic!("expected error {code:?}"),
        }
    }

    pub(crate) use parallel::with_domains;

    pub(crate) fn boundary_and_non_manifold(source: &geometry::T) -> (usize, usize) {
        let index = rdk_core::topology_index::create(None, geometry::topology(source)).unwrap();
        let (mut boundary, mut non_manifold) = (0, 0);
        for edge in 0..rdk_core::topology_index::edge_count(&index) {
            let n = rdk_core::topology_index::edge_incidence_count(&index, edge).unwrap();
            if n == 1 {
                boundary += 1;
            }
            if n > 2 {
                non_manifold += 1;
            }
        }
        (boundary, non_manifold)
    }
}

#[cfg(test)]
mod tests {
    use super::test_util::*;
    use super::*;
    use rdk_core::topology::PrimitiveKind;

    const ZERO: rays_math::vec3::T = rays_math::vec3::T { x: 0.0, y: 0.0, z: 0.0 };

    fn unit_x() -> rays_math::vec3::T {
        rays_math::vec3::create(1.0, 0.0, 0.0)
    }

    fn params() -> Params {
        Params::new(ZERO, unit_x())
    }

    fn bounds(source: &geometry::T) -> ([f64; 3], [f64; 3]) {
        let (x, y, z) = xyz(source);
        let mut lo = [f64::INFINITY; 3];
        let mut hi = [f64::NEG_INFINITY; 3];
        for i in 0..x.len() {
            for (k, v) in [x[i], y[i], z[i]].into_iter().enumerate() {
                lo[k] = lo[k].min(v);
                hi[k] = hi[k].max(v);
            }
        }
        (lo, hi)
    }

    fn group_of(source: &geometry::T, name: &str) -> group::T {
        geometry::find_group(group::Owner::Primitive, name, source).unwrap_or_else(|| panic!("missing group {name}"))
    }

    #[test]
    fn custom_clip_attribute() {
        let source = with_attribute("field", attribute::Storage::Float(Arc::new(vec![-1.0, 1.0, 1.0])), &triangle());
        let mut p = params();
        p.clip_attribute = "field".to_owned();
        let clipped = clip(&p, &source).unwrap();
        let (lo, hi) = bounds(&clipped);
        assert!(near(lo[0], -5.0) && near(hi[0], 10.0), "scalar Clip Attribute did not control the intersection parameter");
        let run = |storage: attribute::Storage| {
            let mut p = params();
            p.clip_attribute = "field".to_owned();
            clip(&p, &with_attribute("field", storage, &triangle())).unwrap()
        };
        let f2 = |x: Vec<f64>, y: Vec<f64>| packed::float2::of_owned(x, y).unwrap();
        let f3 = |x: Vec<f64>, y: Vec<f64>, z: Vec<f64>| packed::float3::view::of_owned_exn(x, y, z).unwrap();
        let f4 = |x: Vec<f64>, y: Vec<f64>, z: Vec<f64>, w: Vec<f64>| packed::float4::of_owned(x, y, z, w).unwrap();
        let outputs = [
            run(attribute::Storage::Float2(f2(vec![-1.0, 1.0, 1.0], vec![0.0; 3]))),
            run(attribute::Storage::Float3(f3(vec![-1.0, 1.0, 1.0], vec![0.0; 3], vec![0.0; 3]))),
            run(attribute::Storage::Float4(f4(vec![-1.0, 1.0, 1.0], vec![0.0; 3], vec![0.0; 3], vec![-100.0, 100.0, 500.0]))),
            run(attribute::Storage::Float4(f4(vec![-1.0, 1.0, 1.0], vec![0.0; 3], vec![0.0; 3], vec![1.0, 2.0, 3.0]))),
            run(attribute::Storage::Int(Arc::new(vec![-1, 1, 1]))),
        ];
        for output in &outputs {
            assert_eq!(xyz(&clipped), xyz(output), "numeric tuple zero-fill/first-three behavior differs");
        }
    }

    #[test]
    fn distance_is_translation() {
        let source = cuboid((0.0, 0.0, 0.0), (4.0, 3.0, 2.0));
        let mut offset = Params::new(ZERO, rays_math::vec3::create(8.0, 0.0, 0.0));
        offset.keep = Keep::All;
        offset.distance = 0.75;
        let mut translated = Params::new(rays_math::vec3::create(0.75, 0.0, 0.0), unit_x());
        translated.keep = Keep::All;
        assert!(same_geometry(&clip(&offset, &source).unwrap(), &clip(&translated, &source).unwrap()));
    }

    fn shared_selection_fixture() -> geometry::T {
        let source = polygons(
            vec![-1.0, 0.0005, 1.0, -1.0],
            vec![0.0, 0.0, 1.0, 1.0],
            vec![0.0; 4],
            &[vec![0, 1, 2], vec![1, 3, 2]],
        );
        with_owned_attribute("primitive_id", attribute::Owner::Primitive, attribute::Storage::Int(Arc::new(vec![10, 20])), &source)
    }

    fn primitive_selection(source_count: i64, pick: &(dyn Fn(i64) -> bool + Send + Sync)) -> group::T {
        group::init(None, group::Owner::Primitive, "selected", source_count, &|index| Ok(pick(index))).unwrap()
    }

    #[test]
    fn selection_isolation() {
        let source = shared_selection_fixture();
        let selected = primitive_selection(2, &|p| p == 0);
        let mut p = params();
        p.grain = 1;
        p.snapping_tolerance = 0.001;
        p.selection = Some(element_selection::T::SelectedPrimitives(selected.clone()));
        p.clipped_group = Some("clipped".to_owned());
        p.above_group = Some("above".to_owned());
        p.clipped_edge_group = Some("clip_edges".to_owned());
        let output = clip(&p, &source).unwrap();
        let ids = int_attribute(attribute::Owner::Primitive, "primitive_id", &output);
        let unselected = ids.iter().position(|&id| id == 20).expect("selected Clip deleted the unselected primitive");
        let view = topo(&output);
        let (x, y, _) = xyz(&output);
        let (first, last) = (view.primitive_offsets[unselected] as usize, view.primitive_offsets[unselected + 1] as usize);
        assert_eq!(last - first, 3);
        let expected = [(0.0005, 0.0), (-1.0, 1.0), (1.0, 1.0)];
        for local in 0..3 {
            let point = view.vertex_points[first + local] as usize;
            assert!(x[point] == expected[local].0 && y[point] == expected[local].1, "moved or reordered an unselected corner");
        }
        assert!(x.iter().any(|&v| v == 0.0) && x.iter().any(|&v| v == 0.0005), "did not isolate a snapped shared point");
        assert_eq!(group::cardinality(&group_of(&output, "clipped")), 1);
        assert_eq!(group::cardinality(&group_of(&output, "above")), 1);
        let existing = primitive_selection(2, &|p| p == 1);
        let existing = group::with_name("clipped", &existing).unwrap();
        let source_with_existing = geometry::with_group(existing, &source).unwrap();
        let mut p = params();
        p.grain = 1;
        p.snapping_tolerance = 0.001;
        p.selection = Some(element_selection::T::SelectedPrimitives(selected.clone()));
        p.replace_existing_groups = false;
        p.clipped_group = Some("clipped".to_owned());
        let unioned = clip(&p, &source_with_existing).unwrap();
        assert_eq!(group::cardinality(&group_of(&unioned, "clipped")), 2);
        let mut p = params();
        p.grain = 1;
        p.keep = Keep::All;
        p.split_connectivity = true;
        p.snapping_tolerance = 0.001;
        p.selection = Some(element_selection::T::SelectedPrimitives(selected));
        let split = clip(&p, &source).unwrap();
        let split_ids = int_attribute(attribute::Owner::Primitive, "primitive_id", &split);
        let split_topology = topo(&split);
        let count = geometry::point_count(&split) as usize;
        let mut selected_points = vec![false; count];
        let mut unselected_points = vec![false; count];
        for (primitive, &id) in split_ids.iter().enumerate() {
            let target = if id == 10 { &mut selected_points } else { &mut unselected_points };
            for vertex in split_topology.primitive_offsets[primitive]..split_topology.primitive_offsets[primitive + 1] {
                target[split_topology.vertex_points[vertex as usize] as usize] = true;
            }
        }
        assert!(!(0..count).any(|i| selected_points[i] && unselected_points[i]), "shared a point with unselected topology");
    }

    fn clipped_count(selection: element_selection::T, source: &geometry::T) -> i64 {
        let mut p = params();
        p.grain = 1;
        p.selection = Some(selection);
        p.clipped_group = Some("clipped".to_owned());
        group::cardinality(&group_of(&clip(&p, source).unwrap(), "clipped"))
    }

    #[test]
    fn typed_selection_promotion() {
        let source = shared_selection_fixture();
        let unrestricted = clip(&params(), &source).unwrap();
        let points = group::init(None, group::Owner::Point, "shared_point", 4, &|p| Ok(p == 1)).unwrap();
        let mut p = params();
        p.selection = Some(element_selection::T::SelectedPoints(points));
        assert!(same_geometry(&unrestricted, &clip(&p, &source).unwrap()), "point selection did not promote");
        let vertices = group::init(None, group::Owner::Vertex, "first_corner", 6, &|v| Ok(v == 0)).unwrap();
        assert_eq!(clipped_count(element_selection::T::SelectedVertices(vertices), &source), 1);
        let index = topology_index::create(None, geometry::topology(&source)).unwrap();
        let shared_edge = topology_index::find_edge_index(&index, 1, 2);
        let boundary_edge = topology_index::find_edge_index(&index, 0, 1);
        let shared = edge_group::init(None, geometry::topology(&source), &index, "shared", &|e| Ok(e == shared_edge)).unwrap();
        let boundary =
            edge_group::init(None, geometry::topology(&source), &index, "boundary", &|e| Ok(e == boundary_edge)).unwrap();
        assert_eq!(clipped_count(element_selection::T::SelectedEdges(shared), &source), 2);
        assert_eq!(clipped_count(element_selection::T::SelectedEdges(boundary), &source), 1);
        let foreign = cuboid((0.0, 0.0, 0.0), (2.0, 2.0, 2.0));
        let foreign_index = topology_index::create(None, geometry::topology(&foreign)).unwrap();
        let foreign_edges =
            edge_group::init(None, geometry::topology(&foreign), &foreign_index, "foreign", &|e| Ok(e == 0)).unwrap();
        let mut p = params();
        p.selection = Some(element_selection::T::SelectedEdges(foreign_edges));
        expect_code(clip(&p, &source), Code::InvalidGeometry);
    }

    #[test]
    fn selected_free_points() {
        let source = geom(vec![-2.0, -1.0, 1.0], vec![0.0; 3], vec![0.0; 3], &[]);
        let source = with_attribute("id", attribute::Storage::Int(Arc::new(vec![10, 20, 30])), &source);
        let selected = group::init(None, group::Owner::Point, "selected_free", 3, &|p| Ok(p == 0)).unwrap();
        let mut p = params();
        p.selection = Some(element_selection::T::SelectedPoints(selected));
        let output = clip(&p, &source).unwrap();
        assert!(
            geometry::point_count(&output) == 2
                && int_attribute(attribute::Owner::Point, "id", &output) == vec![20, 30],
            "point-selected Clip did not restrict free-point filtering"
        );
    }

    #[test]
    fn selected_caps_and_edge_output() {
        let first = cuboid((0.0, 0.0, 0.0), (2.0, 2.0, 2.0));
        let second = cuboid((5.0, 0.0, 0.0), (2.0, 2.0, 2.0));
        let first_primitives = geometry::primitive_count(&first);
        let source = merge(&[first, second]);
        let selected =
            group::init(None, group::Owner::Primitive, "first_box", geometry::primitive_count(&source), &|p| Ok({
                p < first_primitives
            }))
            .unwrap();
        let build = || {
            let mut p = Params::new(ZERO, rays_math::vec3::create(0.0, 1.0, 0.0));
            p.grain = 1;
            p.fill = true;
            p.selection = Some(element_selection::T::SelectedPrimitives(selected.clone()));
            p.cap_group = Some("caps".to_owned());
            clip(&p, &source).unwrap()
        };
        let one = with_domains(1, &build);
        let four = with_domains(4, &build);
        assert!(same_geometry(&one, &four), "selected filled Clip differs across domain counts");
        let (lo, hi) = bounds(&one);
        assert!(near(lo[1], -1.0) && near(hi[1], 1.0));
        assert_eq!(group::cardinality(&group_of(&one, "caps")), 1);
        let source = polygons(
            vec![-1.0, 1.0, 1.0, 0.0, 0.0, 0.0],
            vec![0.0, 0.0, 1.0, 2.0, 3.0, 2.0],
            vec![0.0, 0.0, 0.0, 0.0, 0.0, 1.0],
            &[vec![0, 1, 2], vec![3, 4, 5]],
        );
        let selected = group::init(None, group::Owner::Primitive, "first", 2, &|p| Ok(p == 0)).unwrap();
        let mut p = params();
        p.selection = Some(element_selection::T::SelectedPrimitives(selected));
        p.clipped_edge_group = Some("plane_edges".to_owned());
        let output = clip(&p, &source).unwrap();
        let found = geometry::find_edge_group("plane_edges", &output).expect("edge output missing");
        assert_eq!(edge_group::cardinality(&found), 1);
    }

    #[test]
    fn degenerate_fallback() {
        let topo1 = topology::create_owned(4, vec![0, 1, 1, 2, 3], vec![0, 5], vec![PrimitiveKind::Polygon]).unwrap();
        let source = geometry::create(
            packed::float3::view::of_owned_exn(vec![0.0, 1.0, 1.0, 0.0], vec![0.0, 0.0, 1.0, 1.0], vec![0.0; 4]).unwrap(),
            topo1,
            None,
            None,
            None,
        )
        .unwrap();
        let p = Params::new(rays_math::vec3::create(-10.0, 0.0, 0.0), unit_x());
        let output = clip(&p, &source).unwrap();
        assert!(geometry::primitive_count(&output) == 1 && geometry::vertex_count(&output) == 4);
        let topo2 = topology::create_owned(3, vec![0, 1, 0, 2], vec![0, 4], vec![PrimitiveKind::Polygon]).unwrap();
        let source = geometry::create(
            packed::float3::view::of_owned_exn(vec![1.0, -1.0, 1.0], vec![0.0, 0.0, 1.0], vec![0.0; 3]).unwrap(),
            topo2,
            None,
            None,
            None,
        )
        .unwrap();
        let output = clip(&params(), &source).unwrap();
        assert!(geometry::primitive_count(&output) == 1 && geometry::vertex_count(&output) == 4);
    }

    #[test]
    fn validation() {
        let source = triangle();
        let mut p = params();
        p.clip_attribute = String::new();
        expect_code(clip(&p, &source), Code::InvalidGeometry);
        let mut p = params();
        p.clip_attribute = "missing".to_owned();
        expect_code(clip(&p, &source), Code::InvalidGeometry);
        let mut p = params();
        p.distance = f64::NAN;
        expect_code(clip(&p, &source), Code::InvalidGeometry);
        let mut p = params();
        p.clipped_edge_group = Some(" ".to_owned());
        expect_code(clip(&p, &source), Code::InvalidGeometry);
        let text = with_attribute(
            "field",
            attribute::Storage::Text(std::sync::Arc::new(vec![b"a".to_vec(), b"b".to_vec(), b"c".to_vec()].into())),
            &source,
        );
        let mut p = params();
        p.clip_attribute = "field".to_owned();
        expect_code(clip(&p, &text), Code::InvalidGeometry);
        let non_finite = with_attribute("field", attribute::Storage::Float(Arc::new(vec![-1.0, f64::INFINITY, 1.0])), &source);
        expect_code(clip(&p, &non_finite), Code::InvalidGeometry);
        let mut p = params();
        p.grain = 0;
        expect_code(clip(&p, &source), Code::InvalidArgument);
    }

    #[test]
    fn disconnected_concave_fragments() {
        let source = with_owned_attribute(
            "source_primitive",
            attribute::Owner::Primitive,
            attribute::Storage::Int(Arc::new(vec![17])),
            &c_polygon(),
        );
        let build = || {
            let mut p = params();
            p.grain = 1;
            p.clipped_group = Some("clipped".to_owned());
            p.clipped_edge_group = Some("cut_edges".to_owned());
            clip(&p, &source).unwrap()
        };
        let one = with_domains(1, &build);
        let four = with_domains(4, &build);
        assert!(same_geometry(&one, &four));
        assert!(geometry::primitive_count(&one) == 2 && geometry::vertex_count(&one) == 8);
        assert_eq!(int_attribute(attribute::Owner::Primitive, "source_primitive", &one), vec![17, 17]);
        let view = topo(&one);
        let (x, _, _) = xyz(&one);
        for primitive in 0..geometry::primitive_count(&one) as usize {
            assert_eq!(view.primitive_offsets[primitive + 1] - view.primitive_offsets[primitive], 4);
            for vertex in view.primitive_offsets[primitive]..view.primitive_offsets[primitive + 1] {
                assert!(x[view.vertex_points[vertex as usize] as usize] >= 0.0, "fragment contains a rejected point");
            }
        }
        assert_eq!(group::cardinality(&group_of(&one, "clipped")), 2);
        assert_eq!(edge_group::cardinality(&geometry::find_edge_group("cut_edges", &one).unwrap()), 2);
        let mut p = params();
        p.grain = 1;
        p.keep = Keep::All;
        let both = clip(&p, &source).unwrap();
        assert_eq!(geometry::primitive_count(&both), 3);
        let quad = polygons(vec![0.0, 2.0, 1.0, 2.0], vec![1.0, 0.0, 1.0, 2.0], vec![0.0; 4], &[vec![0, 1, 2, 3]]);
        let mut p = Params::new(rays_math::vec3::create(1.5, 0.0, 0.0), unit_x());
        p.grain = 1;
        let quad = clip(&p, &quad).unwrap();
        assert!(geometry::primitive_count(&quad) == 2 && geometry::vertex_count(&quad) == 6);
        let mut p = params();
        p.grain = 1;
        p.fill = true;
        p.cap_group = Some("caps".to_owned());
        let filled = clip(&p, &c_prism(1.0)).unwrap();
        assert_eq!(group::cardinality(&group_of(&filled, "caps")), 2);
    }

    fn hollow_square_prism() -> geometry::T {
        let mut x = vec![0.0; 16];
        let mut y = vec![0.0; 16];
        let mut z = vec![0.0; 16];
        let outer = [(-2.0, -2.0), (2.0, -2.0), (2.0, 2.0), (-2.0, 2.0)];
        let inner = [(-1.0, -1.0), (1.0, -1.0), (1.0, 1.0), (-1.0, 1.0)];
        for side in 0..2 {
            let px = if side == 0 { -1.0 } else { 1.0 };
            for corner in 0..4 {
                let op = side * 4 + corner;
                let ip = 8 + side * 4 + corner;
                x[op] = px;
                x[ip] = px;
                y[op] = outer[corner].0;
                z[op] = outer[corner].1;
                y[ip] = inner[corner].0;
                z[ip] = inner[corner].1;
            }
        }
        let mut polys: Vec<Vec<i64>> = Vec::new();
        for corner in 0..4i64 {
            let next = (corner + 1) % 4;
            polys.push(vec![corner, next, 4 + next, 4 + corner]);
            polys.push(vec![8 + corner, 12 + corner, 12 + next, 8 + next]);
            polys.push(vec![corner, 8 + corner, 8 + next, next]);
            polys.push(vec![4 + corner, 4 + next, 12 + next, 12 + corner]);
        }
        polygons(x, y, z, &polys)
    }

    #[test]
    fn nested_cap_contours() {
        let source = hollow_square_prism();
        let build = || {
            let mut p = params();
            p.grain = 1;
            p.fill = true;
            p.cap_group = Some("annulus_caps".to_owned());
            p.clipped_edge_group = Some("annulus_edges".to_owned());
            clip(&p, &source).unwrap()
        };
        let one = with_domains(1, &build);
        let four = with_domains(4, &build);
        assert!(same_geometry(&one, &four));
        let caps = group_of(&one, "annulus_caps");
        assert_eq!(group::cardinality(&caps), 8);
        let cap_edges = geometry::find_edge_group("annulus_edges", &one).unwrap();
        assert_eq!(edge_group::cardinality(&cap_edges), 16);
        let view = topo(&one);
        let (_, py, pz) = xyz(&one);
        let normals = match attribute::view::storage(
            &geometry::find_attribute(attribute::Owner::Vertex, "N", &one).expect("no hard vertex N"),
        )
        .unwrap()
        {
            attribute::Storage::Float3(values) => packed::float3::view::view(values).unwrap(),
            _ => panic!("unexpected N storage"),
        };
        let area = std::sync::Mutex::new(0.0);
        group::iter(
            &|primitive| {
                let first = view.primitive_offsets[primitive as usize] as usize;
                let last = view.primitive_offsets[primitive as usize + 1] as usize;
                assert_eq!(last - first, 3);
                let point = |local: usize| view.vertex_points[first + local] as usize;
                let (a, b, c) = (point(0), point(1), point(2));
                for vertex in first..last {
                    assert!(near(normals.x[vertex], -1.0) && near(normals.y[vertex], 0.0) && near(normals.z[vertex], 0.0));
                }
                let (aby, abz) = (py[b] - py[a], pz[b] - pz[a]);
                let (acy, acz) = (py[c] - py[a], pz[c] - pz[a]);
                *area.lock().unwrap() += (aby * acz - abz * acy).abs() * 0.5;
                let cy = (py[a] + py[b] + py[c]) / 3.0;
                let cz = (pz[a] + pz[b] + pz[c]) / 3.0;
                assert!(cy.abs() >= 1.0 || cz.abs() >= 1.0, "triangulation sealed the interior hole");
            },
            &caps,
        );
        assert!(near(*area.lock().unwrap(), 12.0));
        assert_eq!(boundary_and_non_manifold(&one), (0, 0));
        let center = (1e150, -1e150, 5e149);
        let scale = 1e140;
        let transformed = map_positions(&source, &|x, y, z| (center.0 + x * scale, center.1 + y * scale, center.2 + z * scale));
        let mut p = Params::new(rays_math::vec3::create(center.0, center.1, center.2), unit_x());
        p.grain = 1;
        p.fill = true;
        p.cap_group = Some("caps".to_owned());
        let transformed = clip(&p, &transformed).unwrap();
        assert_eq!(group::cardinality(&group_of(&transformed, "caps")), 8);
        let parts: Vec<geometry::T> = (0..8)
            .map(|component| {
                map_positions(&hollow_square_prism(), &|x, y, z| (x, y + (component as f64 - 3.5) * 6.0, z))
            })
            .collect();
        let components = merge(&parts);
        let build = || {
            let mut p = params();
            p.grain = 1;
            p.fill = true;
            p.cap_group = Some("caps".to_owned());
            clip(&p, &components).unwrap()
        };
        let component_one = with_domains(1, &build);
        let component_four = with_domains(4, &build);
        assert!(same_geometry(&component_one, &component_four));
        assert_eq!(group::cardinality(&group_of(&component_one, "caps")), 64);
    }

    fn cap_area_yz(source: &geometry::T, caps: &group::T) -> f64 {
        let view = topo(source);
        let (_, py, pz) = xyz(source);
        let total = std::sync::Mutex::new(0.0);
        group::iter(
            &|primitive| {
                let first = view.primitive_offsets[primitive as usize] as usize;
                let last = view.primitive_offsets[primitive as usize + 1] as usize;
                let mut twice = 0.0;
                for vertex in first..last {
                    let next = if vertex + 1 == last { first } else { vertex + 1 };
                    let (a, b) = (view.vertex_points[vertex] as usize, view.vertex_points[next] as usize);
                    twice += py[a] * pz[b] - py[b] * pz[a];
                }
                *total.lock().unwrap() += twice.abs() * 0.5;
            },
            caps,
        );
        total.into_inner().unwrap()
    }

    #[test]
    fn multiple_nested_cap_contours() {
        let bx = |center: (f64, f64, f64), y: f64, z: f64| cuboid(center, (2.0, y, z));
        let outer = bx((0.0, 0.0, 0.0), 6.0, 4.0);
        let first_hole = reverse(&bx((0.0, -1.5, 0.0), 1.0, 1.0));
        let second_hole = reverse(&bx((0.0, 1.5, 0.0), 1.0, 1.0));
        let source = merge(&[outer.clone(), first_hole, second_hole]);
        let run = |domains: i64, keep: Keep| {
            with_domains(domains, &|| {
                let mut p = params();
                p.grain = 1;
                p.keep = keep;
                p.fill = true;
                p.cap_group = Some("caps".to_owned());
                clip(&p, &source).unwrap()
            })
        };
        let one = run(1, Keep::Above);
        let four = run(4, Keep::Above);
        assert!(same_geometry(&one, &four));
        let caps = group_of(&one, "caps");
        assert_eq!(group::cardinality(&caps), 14);
        assert!(near(cap_area_yz(&one, &caps), 22.0));
        let both = run(4, Keep::All);
        let both_caps = group_of(&both, "caps");
        assert!(group::cardinality(&both_caps) == 28 && near(cap_area_yz(&both, &both_caps), 44.0));
        let clip_fill = |g: &geometry::T| {
            let mut p = params();
            p.grain = 1;
            p.fill = true;
            p.cap_group = Some("caps".to_owned());
            clip(&p, g).unwrap()
        };
        let middle = reverse(&bx((0.0, 0.0, 0.0), 4.0, 4.0));
        let island = bx((0.0, 0.0, 0.0), 2.0, 2.0);
        let nested = clip_fill(&merge(&[bx((0.0, 0.0, 0.0), 6.0, 6.0), middle, island]));
        let nested_caps = group_of(&nested, "caps");
        assert!(group::cardinality(&nested_caps) == 9 && near(cap_area_yz(&nested, &nested_caps), 24.0));
        let solids = clip_fill(&merge(&[bx((0.0, 0.0, 0.0), 6.0, 6.0), bx((0.0, 0.0, 0.0), 2.0, 2.0)]));
        let solid_caps = group_of(&solids, "caps");
        assert!(group::cardinality(&solid_caps) == 2 && near(cap_area_yz(&solids, &solid_caps), 40.0));
        let concave_hole = reverse(&cuboid((-1.5, 0.0, 0.5), (0.5, 0.5, 1.0)));
        let mut p = Params::new(rays_math::vec3::create(0.0, 0.0, 0.5), rays_math::vec3::create(0.0, 0.0, 1.0));
        p.grain = 1;
        p.fill = true;
        p.cap_group = Some("caps".to_owned());
        let concave_nested = clip(&p, &merge(&[c_prism(1.0), concave_hole])).unwrap();
        assert_eq!(group::cardinality(&group_of(&concave_nested, "caps")), 12);
        assert_eq!(boundary_and_non_manifold(&concave_nested), (0, 0));
        let overlap_a = reverse(&bx((0.0, -0.3, 0.0), 2.0, 2.0));
        let overlap_b = reverse(&bx((0.0, 0.3, 0.0), 2.0, 2.0));
        let mut p = params();
        p.grain = 1;
        p.fill = true;
        expect_code(clip(&p, &merge(&[outer.clone(), overlap_a, overlap_b])), Code::InvalidGeometry);
        let same = merge(&[outer, bx((0.0, -0.3, 0.0), 2.0, 2.0), bx((0.0, 0.3, 0.0), 2.0, 2.0)]);
        expect_code(clip(&p, &same), Code::InvalidGeometry);
    }

    #[test]
    fn parallel_exact_at_scale() {
        use crate::compact_points::fixtures as fx;
        use rdk_core::attribute::{Owner, Storage};
        let g = fx::grid(150, 150, 2.0, true);
        let n = geometry::point_count(&g);
        let g = fx::with_attr(&g, Owner::Point, "id", Storage::Int(Arc::new((0..n).collect())));
        let g = fx::with_attr(&g, Owner::Point, "w", Storage::Float(Arc::new((0..n).map(|p| (p % 97) as f64 * 0.125).collect())));
        let g = fx::with_attr(&g, Owner::Point, "N", fx::float3_attr(vec![0.3; n as usize], vec![0.4; n as usize], vec![0.5; n as usize]));
        let primitives = geometry::primitive_count(&g);
        let selected = group::init(None, group::Owner::Primitive, "half", primitives, &|p| Ok(p % 3 != 0)).unwrap();
        for keep in [Keep::Above, Keep::Below, Keep::All] {
            for use_selection in [false, true] {
                let build = || {
                    let mut p = Params::new(rays_math::vec3::create(0.013, 0.0, 0.0), unit_x());
                    p.grain = 256;
                    p.keep = keep;
                    p.selection = use_selection.then(|| element_selection::T::SelectedPrimitives(selected.clone()));
                    clip(&p, &g).unwrap()
                };
                let one = fx::exact_at_one_and_four(&build);
                assert!(geometry::primitive_count(&one) > 0, "{keep:?}");
            }
        }
    }

    /// `cargo test -p rdk_ops bench_parallel_sites -- --ignored --nocapture`: one-domain versus all-domain wall time.
    #[test]
    #[ignore]
    fn bench_parallel_sites() {
        use crate::compact_points::fixtures as fx;
        use rdk_core::attribute::{Owner, Storage};
        use std::time::Instant;
        let g = fx::grid(700, 700, 2.0, true);
        let n = geometry::point_count(&g);
        let g = fx::with_attr(&g, Owner::Point, "w", Storage::Float(Arc::new((0..n).map(|p| (p % 97) as f64).collect())));
        let domains = parallel::recommended_domains();
        let time = |label: &str, work: &(dyn Fn() + Sync)| {
            let mut line = format!("{label:<26}");
            for d in [1, domains] {
                let slot = std::sync::Mutex::new(0.0f64);
                parallel::run_unit(Some(d), &|| {
                    work(); // warm
                    let start = Instant::now();
                    work();
                    *slot.lock().unwrap() = start.elapsed().as_secs_f64() * 1000.0;
                    Ok(())
                })
                .unwrap();
                line += &format!("  {d:>2} domains {:>9.1} ms", slot.into_inner().unwrap());
            }
            println!("{line}");
        };
        time("triangulate (quads)", &|| {
            let _ = crate::triangulate::run(&crate::triangulate::Params { cancel: None, grain: None, primitives: None }, &g).unwrap();
        });
        time("plane_clip keep=All", &|| {
            let mut p = Params::new(rays_math::vec3::create(0.013, 0.0, 0.0), unit_x());
            p.keep = Keep::All;
            let _ = clip(&p, &g).unwrap();
        });
        time("poly_extrude (default)", &|| {
            let _ = crate::poly_extrude::run(&crate::poly_extrude::Params::new(0.5), &g).unwrap();
        });
        let dup = {
            let (x, y, z) = fx::positions(&g);
            let (vp, offsets, _) = fx::topo(&g);
            let mut nvp = vp.clone();
            let mut noff = offsets.clone();
            for prim in 0..offsets.len() - 1 {
                if prim % 5 == 0 {
                    nvp.extend_from_slice(&vp[offsets[prim] as usize..offsets[prim + 1] as usize]);
                    noff.push(nvp.len() as i64);
                }
            }
            fx::geom(&x, &y, &z, &nvp, &noff, None)
        };
        time("clean (overlaps)", &|| {
            let mut p = crate::clean::Params {
                cancel: None, grain: 16_384, epsilon: 1e-12, remove_degenerate: true, consolidate_distance: None,
                overlaps: Some(crate::clean::OverlapPolicy::KeepFirstOverlap), reverse_winding: false,
                remove_nan_points: true, remove_unused_points: false, delete_unused_groups: false,
                point_attributes: None, vertex_attributes: None, primitive_attributes: None, detail_attributes: None,
                point_groups: None, vertex_groups: None, primitive_groups: None, edge_groups: None,
            };
            p.grain = 16_384;
            let _ = crate::clean::run(&p, &dup).unwrap();
        });
    }
}
