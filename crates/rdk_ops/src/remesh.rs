//! Port of lib/rdk/ops/mesh/remesh.ml: incremental isotropic remeshing (split, collapse, flip,
//! smooth, project) over one local mutable triangle structure per iteration.
use std::sync::Arc;

use rdk_core::attribute::{self, Owner, Storage};
use rdk_core::{
    cancel, edge_group, error, geometry, group, packed, support, topology, topology_index,
};
use rdk_core::support::Windows;

use crate::subdivide_refine::{
    collect_edge_members, fmin, gf, gi,
};


const OPERATION: &str = "Rdk_mesh.Remesh.remesh";

/// OCaml `Sys.max_array_length` on a 64-bit build.

/// `fail message = Error (operation ^ ": " ^ message)`.
pub fn fail(message: &str) -> error::T {
    error::make(
        "remesh".to_owned(),
        "invalid_remesh".to_owned(),
        format!("{OPERATION}: {message}"),
        None,
    )
    .unwrap_or_else(|built| built)
}

/// OCaml `Invalid_argument`; the public entry turns it into a prefixed failure.
pub fn invalid_arg(message: String) -> error::T {
    error::make("remesh".to_owned(), "invalid_argument".to_owned(), message, None)
        .unwrap_or_else(|built| built)
}

/// OCaml `%S`.
fn quoted(value: &str) -> String {
    let mut out = String::from("\"");
    for byte in value.bytes() {
        match byte {
            b'"' => out.push_str("\\\""),
            b'\\' => out.push_str("\\\\"),
            b'\n' => out.push_str("\\n"),
            b'\t' => out.push_str("\\t"),
            b'\r' => out.push_str("\\r"),
            8 => out.push_str("\\b"),
            32..=126 => out.push(byte as char),
            other => out.push_str(&format!("\\{other:03}")),
        }
    }
    out.push('"');
    out
}

pub struct ProjectionScratch {
    pub count: i64,
    pub primitives: Vec<i64>,
    pub triangles: Vec<i64>,
    pub barycentric_a: Vec<f64>,
    pub barycentric_b: Vec<f64>,
    pub barycentric_c: Vec<f64>,
    pub distances_squared: Vec<f64>,
}

pub fn validate_name(label: &str, name: Option<&str>) -> Result<(), error::T> {
    match name {
        Some(name) if rdk_core::support::ocaml_trim_is_empty(name) => Err(fail(&format!("{label} name must not be empty"))),
        _ => Ok(()),
    }
}

pub fn validate_point_group(geometry_value: &geometry::T, selection: Option<&group::T>) -> Result<(), error::T> {
    match selection {
        None => Ok(()),
        Some(selection) if group::owner(selection) != group::Owner::Point => {
            Err(fail("hard point selection must own points"))
        }
        Some(selection) if group::length(selection) != geometry::point_count(geometry_value) => {
            Err(fail("hard point selection length does not match point count"))
        }
        Some(_) => Ok(()),
    }
}

pub fn validate_edge_group(geometry_value: &geometry::T, selection: Option<&edge_group::T>) -> Result<(), error::T> {
    let Some(selection) = selection else {
        return Ok(());
    };
    let topology_value = geometry::topology(geometry_value);
    let index = topology_index::create(None, topology_value)?;
    if edge_group::topology_data_id(selection) != topology::data_id(topology_value) {
        Err(fail("hard edge selection belongs to a different topology"))
    } else if edge_group::length(selection) != topology_index::edge_count(&index) {
        Err(fail("hard edge selection length does not match topology edge count"))
    } else {
        Ok(())
    }
}

pub fn validate_positions(cancel_token: Option<&cancel::T>, geometry_value: &geometry::T) -> Result<(), error::T> {
    let positions = packed::Float3::View::view(geometry::positions(geometry_value))?;
    let mut invalid = -1_i64;
    let mut point = 0_usize;
    while point < positions.x.len() && invalid < 0 {
        cancel::poll(cancel_token, point as i64, 4095)?;
        if !(positions.x[point].is_finite() && positions.y[point].is_finite() && positions.z[point].is_finite())
        {
            invalid = point as i64;
        }
        point += 1;
    }
    if invalid < 0 {
        Ok(())
    } else {
        Err(fail(&format!("point {invalid} has a non-finite position")))
    }
}

pub fn target_values(
    cancel_token: Option<&cancel::T>,
    name: Option<&str>,
    geometry_value: &geometry::T,
) -> Result<Option<Vec<f64>>, error::T> {
    let Some(name) = name else {
        return Ok(None);
    };
    if rdk_core::support::ocaml_trim_is_empty(name) {
        return Err(fail("target-size attribute name must not be empty"));
    }
    match geometry::find_attribute(Owner::Point, name, geometry_value) {
        None => Err(fail(&format!("point target-size attribute {} does not exist", quoted(name)))),
        Some(found) => match attribute::View::storage(&found)? {
            Storage::Float(values) => {
                let mut invalid = -1_i64;
                let mut point = 0_usize;
                while point < values.len() && invalid < 0 {
                    cancel::poll(cancel_token, point as i64, 4095)?;
                    if !values[point].is_finite() || values[point] <= 0.0 {
                        invalid = point as i64;
                    }
                    point += 1;
                }
                if invalid < 0 {
                    Ok(Some(values.clone()))
                } else {
                    Err(fail(&format!(
                        "target-size attribute {} must be finite and positive at point {}",
                        quoted(name),
                        invalid
                    )))
                }
            }
            _ => Err(fail(&format!(
                "point target-size attribute {} must be scalar float",
                quoted(name)
            ))),
        },
    }
}

pub fn fresh_group_name(
    geometry_value: &geometry::T,
    owner: group::Owner,
    base: &str,
    reserved: Option<&str>,
) -> String {
    let mut suffix = 0_i64;
    loop {
        let name = if suffix == 0 { base.to_owned() } else { format!("{base}{suffix}") };
        if Some(name.as_str()) == reserved || geometry::find_group(owner, &name, geometry_value).is_some() {
            suffix += 1;
        } else {
            return name;
        }
    }
}

pub fn fresh_edge_group_name(
    geometry_value: &geometry::T,
    base: &str,
    reserved: Option<&str>,
) -> String {
    let mut suffix = 0_i64;
    loop {
        let name = if suffix == 0 { base.to_owned() } else { format!("{base}{suffix}") };
        if Some(name.as_str()) == reserved || geometry::find_edge_group(&name, geometry_value).is_some() {
            suffix += 1;
        } else {
            return name;
        }
    }
}

pub type UvEqual = Box<dyn Fn(i64, i64) -> bool + Send + Sync>;

pub fn uv_reader(name: &str, geometry_value: &geometry::T) -> Result<Option<UvEqual>, error::T> {
    let Some(found) = geometry::find_attribute(Owner::Vertex, name, geometry_value) else {
        return Ok(None);
    };
    let equal: Option<UvEqual> = match attribute::View::storage(&found)? {
        Storage::Float(values) => {
            let values = values.clone();
            Some(Box::new(move |a, b| gf(&values, a) == gf(&values, b)))
        }
        Storage::Float2(values) => {
            let values = packed::Float2::View::view(values);
            Some(Box::new(move |a, b| {
                gf(&values.x, a) == gf(&values.x, b) && gf(&values.y, a) == gf(&values.y, b)
            }))
        }
        Storage::Float3(values) => {
            let values = packed::Float3::View::view(values)?;
            Some(Box::new(move |a, b| {
                gf(&values.x, a) == gf(&values.x, b)
                    && gf(&values.y, a) == gf(&values.y, b)
                    && gf(&values.z, a) == gf(&values.z, b)
            }))
        }
        Storage::Float4(values) => {
            let values = packed::Float4::View::view(values);
            Some(Box::new(move |a, b| {
                gf(&values.x, a) == gf(&values.x, b)
                    && gf(&values.y, a) == gf(&values.y, b)
                    && gf(&values.z, a) == gf(&values.z, b)
                    && gf(&values.w, a) == gf(&values.w, b)
            }))
        }
        _ => None,
    };
    match equal {
        Some(equal) => Ok(Some(equal)),
        None => Err(fail(&format!(
            "vertex UV attribute {} must be float, float2, float3, or float4",
            quoted(name)
        ))),
    }
}

pub fn endpoint_vertices(
    topology_view: &topology::View::View,
    index: &topology_index::View::View,
    edge: i64,
    incidence: i64,
) -> Option<(i64, i64)> {
    let vertex = gi(&index.edge_vertices, incidence);
    let next = gi(&index.next_vertex, vertex);
    if next < 0 {
        return None;
    }
    let a = gi(&index.edge_a, edge);
    let b = gi(&index.edge_b, edge);
    let point = gi(&topology_view.vertex_points, vertex);
    let next_point = gi(&topology_view.vertex_points, next);
    if point == a && next_point == b {
        Some((vertex, next))
    } else if point == b && next_point == a {
        Some((next, vertex))
    } else {
        None
    }
}

#[allow(clippy::too_many_arguments)]
pub fn install_feature_groups(
    cancel_token: Option<&cancel::T>,
    grain: i64,
    hard_points: Option<&group::T>,
    hard_edges: Option<&edge_group::T>,
    preserve_uv_seams: bool,
    uv_attribute: &str,
    point_name: &str,
    edge_name: &str,
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    let topology_value = geometry::topology(geometry_value);
    let topology_view = topology::View::view(topology_value)?;
    let index_value = topology_index::create(cancel_token, topology_value)?;
    let index = topology_index::View::view(&index_value);
    let uv_equal = if preserve_uv_seams { uv_reader(uv_attribute, geometry_value)? } else { None };
    cancel::check_opt(cancel_token)?;
    let feature_edges = edge_group::init(
        Some(grain),
        topology_value,
        &index_value,
        edge_name,
        &|edge| {
            let explicit = match hard_edges {
                None => false,
                Some(selection) => edge_group::mem(edge, selection),
            };
            if explicit {
                return true;
            }
            let first = gi(&index.edge_offsets, edge);
            let last = gi(&index.edge_offsets, edge + 1);
            if last - first > 2 {
                return true;
            }
            match &uv_equal {
                None => false,
                Some(equal) if last - first == 2 => {
                    match (
                        endpoint_vertices(&topology_view, &index, edge, first),
                        endpoint_vertices(&topology_view, &index, edge, first + 1),
                    ) {
                        (Some((a0, b0)), Some((a1, b1))) => !(equal(a0, a1) && equal(b0, b1)),
                        _ => true,
                    }
                }
                Some(_) => false,
            }
        },
    )?;
    let points = group::init(
        Some(grain),
        group::Owner::Point,
        point_name,
        geometry::point_count(geometry_value),
        &|point| match hard_points {
            None => false,
            Some(selection) => group::mem(point, selection),
        },
    )?;
    let with_edges = geometry::with_edge_group(feature_edges, geometry_value)?;
    geometry::with_group(points, &with_edges)
}

pub fn feature_group(name: &str, geometry_value: &geometry::T) -> Result<edge_group::T, error::T> {
    match geometry::find_edge_group(name, geometry_value) {
        Some(found) => Ok(found),
        None => Err(fail("internal feature-edge ancestry was lost")),
    }
}

pub fn primitive_is_triangle(topology_view: &topology::View::View, primitive: i64) -> bool {
    topology_view.primitive_kinds[primitive as usize] == 0
        && gi(&topology_view.primitive_offsets, primitive + 1)
            - gi(&topology_view.primitive_offsets, primitive)
            == 3
}

pub fn checked_add(label: &str, left: i64, right: i64) -> Result<i64, error::T> {
    if left < 0 || right < 0 {
        return Err(invalid_arg(format!("{OPERATION}: {label} exceeds array limits")));
    }
    left.checked_add(right)
        .ok_or_else(|| invalid_arg(format!("{OPERATION}: {label} exceeds array limits")))
}

// ---------------------------------------------------------------------------
// Local edit structure.
//
// One remesh iteration edits a mutable triangle mesh in place: split, collapse and flip update
// point, corner and edge incidence directly, and the result is materialized into one geometry per
// iteration. Points, corners and triangles carry ancestry into the iteration-start geometry, so
// attributes and groups are interpolated once. Edges are renumbered in Topology_index order
// (first sight while scanning corners in output order). The structure is sequential and rebuilt
// from the materialized geometry (and its shared reverse index) every iteration.

pub struct Local {
    pub cancel: Option<Arc<cancel::T>>,
    pub grain: i64,
    pub start: geometry::T,
    // points
    pub points: i64,
    pub x: Vec<f64>,
    pub y: Vec<f64>,
    pub z: Vec<f64>,
    pub p_left: Vec<i64>,
    pub p_right: Vec<i64>,
    pub p_weight: Vec<f64>,
    pub p_alive: Vec<u8>,
    pub p_used: Vec<u8>,
    pub p_first: Vec<i64>,
    pub p_target: Vec<f64>,
    pub point_groups: Vec<group::T>,
    pub p_groups: Vec<Vec<u8>>,
    pub p_orders: Vec<Option<Vec<i64>>>,
    pub vertex_groups: Vec<group::T>,
    pub v_orders: Vec<Option<Vec<i64>>>,
    // corners: three per triangle slot
    pub tris: i64,
    pub c_point: Vec<i64>,
    pub c_left: Vec<i64>,
    pub c_right: Vec<i64>,
    pub c_weight: Vec<f64>,
    pub c_next_at_point: Vec<i64>,
    pub c_edge: Vec<i64>,
    pub c_next_in_edge: Vec<i64>,
    pub t_alive: Vec<u8>,
    pub t_source: Vec<i64>,
    pub t_children: Vec<i64>,
    pub t_child_count: Vec<i64>,
    pub source_tris: i64,
    // edges
    pub edges: i64,
    pub live_edges: i64,
    pub e_a: Vec<i64>,
    pub e_b: Vec<i64>,
    pub e_first: Vec<i64>,
    pub e_incidence: Vec<i64>,
    pub e_alive: Vec<u8>,
    pub edge_group_names: Vec<String>,
    pub e_groups: Vec<Vec<u8>>,
    pub scratch: Vec<i64>,
    pub scratch_count: i64,
}

pub fn capacity_for(label: &str, current: i64, needed: i64) -> Result<i64, error::T> {
    if needed < 0 {
        return Err(invalid_arg(format!("{OPERATION}: {label} exceeds array limits")));
    }
    Ok(needed.max(16.max(current.saturating_mul(2))))
}

pub fn corner_next(corner: i64) -> i64 {
    corner - (corner % 3) + ((corner + 1) % 3)
}

pub fn corner_previous(corner: i64) -> i64 {
    corner - (corner % 3) + ((corner + 2) % 3)
}

impl Local {
    pub fn cancel_token(&self) -> Option<&cancel::T> {
        self.cancel.as_deref()
    }

    pub fn scratch_push(&mut self, value: i64) {
        if self.scratch_count as usize == self.scratch.len() {
            let capacity = 64.max(2 * self.scratch_count) as usize;
            self.scratch.resize(capacity, 0);
        }
        self.scratch[self.scratch_count as usize] = value;
        self.scratch_count += 1;
    }

    pub fn ensure_points(&mut self, needed: i64) -> Result<(), error::T> {
        if needed as usize > self.x.len() {
            let capacity = capacity_for("point cardinality", self.x.len() as i64, needed)? as usize;
            self.x.resize(capacity, 0.0);
            self.y.resize(capacity, 0.0);
            self.z.resize(capacity, 0.0);
            self.p_left.resize(capacity, 0);
            self.p_right.resize(capacity, 0);
            self.p_weight.resize(capacity, 0.0);
            self.p_alive.resize(capacity, 0);
            self.p_used.resize(capacity, 0);
            self.p_first.resize(capacity, -1);
            if !self.p_target.is_empty() {
                self.p_target.resize(capacity, 0.0);
            }
            for bytes in self.p_groups.iter_mut() {
                bytes.resize(capacity, 0);
            }
        }
        Ok(())
    }

    pub fn ensure_tris(&mut self, needed: i64) -> Result<(), error::T> {
        if needed as usize > self.t_source.len() {
            let capacity =
                capacity_for("primitive cardinality", self.t_source.len() as i64, needed)?;
            let corners = capacity
                .checked_mul(3)
                .ok_or_else(|| invalid_arg(format!("{OPERATION}: vertex cardinality exceeds array limits")))? as usize;
            let capacity = capacity as usize;
            self.c_point.resize(corners, 0);
            self.c_left.resize(corners, 0);
            self.c_right.resize(corners, 0);
            self.c_weight.resize(corners, 0.0);
            self.c_next_at_point.resize(corners, -1);
            self.c_edge.resize(corners, -1);
            self.c_next_in_edge.resize(corners, -1);
            self.t_alive.resize(capacity, 0);
            self.t_source.resize(capacity, 0);
            self.t_children.resize(capacity, -1);
            self.t_child_count.resize(capacity, 0);
        }
        Ok(())
    }

    pub fn ensure_edges(&mut self, needed: i64) -> Result<(), error::T> {
        if needed as usize > self.e_a.len() {
            let capacity = capacity_for("edge cardinality", self.e_a.len() as i64, needed)? as usize;
            self.e_a.resize(capacity, 0);
            self.e_b.resize(capacity, 0);
            self.e_first.resize(capacity, -1);
            self.e_incidence.resize(capacity, 0);
            self.e_alive.resize(capacity, 0);
            for bytes in self.e_groups.iter_mut() {
                bytes.resize(capacity, 0);
            }
        }
        Ok(())
    }

    pub fn edge_add(&mut self, a: i64, b: i64) -> Result<i64, error::T> {
        self.ensure_edges(self.edges + 1)?;
        let edge = self.edges;
        self.edges = edge + 1;
        self.live_edges += 1;
        self.e_a[edge as usize] = a;
        self.e_b[edge as usize] = b;
        self.e_first[edge as usize] = -1;
        self.e_incidence[edge as usize] = 0;
        self.e_alive[edge as usize] = 1;
        for bytes in self.e_groups.iter_mut() {
            bytes[edge as usize] = 0;
        }
        Ok(edge)
    }

    pub fn edge_kill(&mut self, edge: i64) {
        if self.e_alive[edge as usize] != 0 {
            self.e_alive[edge as usize] = 0;
            self.e_first[edge as usize] = -1;
            self.live_edges -= 1;
        }
    }

    pub fn edge_revive(&mut self, edge: i64) {
        if self.e_alive[edge as usize] == 0 {
            self.e_alive[edge as usize] = 1;
            self.live_edges += 1;
        }
    }

    pub fn edge_live(&self, edge: i64) -> bool {
        self.e_alive[edge as usize] != 0
    }

    pub fn opposite_point(&self, corner: i64) -> i64 {
        self.c_point[corner_next(corner_next(corner)) as usize]
    }

    pub fn attach_edge(&mut self, corner: i64, edge: i64) {
        self.c_edge[corner as usize] = edge;
        self.c_next_in_edge[corner as usize] = self.e_first[edge as usize];
        self.e_first[edge as usize] = corner;
        self.e_incidence[edge as usize] += 1;
    }

    /// The edge joining `a` and `b`, found along `a`'s corners, or -1.
    pub fn adjacent_edge(&self, a: i64, b: i64) -> i64 {
        let mut found = -1;
        let mut corner = self.p_first[a as usize];
        while found < 0 && corner >= 0 {
            let previous = corner_previous(corner);
            if self.c_point[corner_next(corner) as usize] == b {
                found = self.c_edge[corner as usize];
            } else if self.c_point[previous as usize] == b {
                found = self.c_edge[previous as usize];
            }
            corner = self.c_next_at_point[corner as usize];
        }
        found
    }

    /// Unlink a corner from its edge; the edge slot survives even at zero incidence so a rewired
    /// triangle can rejoin it with its groups intact.
    pub fn detach_edge_soft(&mut self, corner: i64) -> i64 {
        let edge = self.c_edge[corner as usize];
        if self.e_first[edge as usize] == corner {
            self.e_first[edge as usize] = self.c_next_in_edge[corner as usize];
        } else {
            let mut cursor = self.e_first[edge as usize];
            while self.c_next_in_edge[cursor as usize] != corner {
                cursor = self.c_next_in_edge[cursor as usize];
            }
            self.c_next_in_edge[cursor as usize] = self.c_next_in_edge[corner as usize];
        }
        self.e_incidence[edge as usize] -= 1;
        self.c_edge[corner as usize] = -1;
        edge
    }

    pub fn detach_edge(&mut self, corner: i64) {
        let edge = self.detach_edge_soft(corner);
        if self.e_incidence[edge as usize] == 0 {
            self.edge_kill(edge);
        }
    }

    pub fn attach_point(&mut self, corner: i64, point: i64) {
        self.c_point[corner as usize] = point;
        self.c_next_at_point[corner as usize] = self.p_first[point as usize];
        self.p_first[point as usize] = corner;
    }

    pub fn detach_point(&mut self, corner: i64) {
        let point = self.c_point[corner as usize];
        if self.p_first[point as usize] == corner {
            self.p_first[point as usize] = self.c_next_at_point[corner as usize];
        } else {
            let mut cursor = self.p_first[point as usize];
            while self.c_next_at_point[cursor as usize] != corner {
                cursor = self.c_next_at_point[cursor as usize];
            }
            self.c_next_at_point[cursor as usize] = self.c_next_at_point[corner as usize];
        }
    }

    pub fn detach_triangle(&mut self, tri: i64) {
        let first = tri * 3;
        self.detach_edge(first);
        self.detach_edge(first + 1);
        self.detach_edge(first + 2);
        self.detach_point(first);
        self.detach_point(first + 1);
        self.detach_point(first + 2);
        self.t_alive[tri as usize] = 0;
    }

    pub fn point_group_index(&self, name: &str) -> i64 {
        let mut found = -1;
        for (index, item) in self.point_groups.iter().enumerate() {
            if group::name(item) == name {
                found = index as i64;
            }
        }
        found
    }

    pub fn edge_group_index(&self, name: &str) -> i64 {
        let mut found = -1;
        for (index, candidate) in self.edge_group_names.iter().enumerate() {
            if candidate == name {
                found = index as i64;
            }
        }
        found
    }

    pub fn local_length(&self, a: i64, b: i64) -> f64 {
        let (a, b) = (a as usize, b as usize);
        (self.x[a] - self.x[b]).hypot((self.y[a] - self.y[b]).hypot(self.z[a] - self.z[b]))
    }
}

pub fn local_of_geometry(
    cancel_arc: &Option<Arc<cancel::T>>,
    grain: i64,
    target_size_attribute: Option<&str>,
    geometry_value: &geometry::T,
) -> Result<Local, error::T> {
    let cancel_token = cancel_arc.as_deref();
    let topology_value = geometry::topology(geometry_value);
    let topology_view = topology::View::view(topology_value)?;
    let tris = topology_view.primitive_kinds.len() as i64;
    for primitive in 0..tris {
        cancel::poll(cancel_token, primitive, 4095)?;
        if !primitive_is_triangle(&topology_view, primitive) {
            return Err(invalid_arg(format!("{OPERATION}: primitive {primitive} is not a triangle")));
        }
    }
    let points = topology_view.point_count;
    let corners = tris * 3;
    let index = topology_index::View::view(&topology_index::create(cancel_token, topology_value)?);
    let positions = packed::Float3::View::view(geometry::positions(geometry_value))?;
    let point_groups: Vec<group::T> = geometry::groups(geometry_value)
        .into_iter()
        .filter(|item| group::owner(item) == group::Owner::Point)
        .collect();
    let vertex_groups: Vec<group::T> = geometry::groups(geometry_value)
        .into_iter()
        .filter(|item| group::owner(item) == group::Owner::Vertex)
        .collect();
    let edge_groups = geometry::edge_groups(geometry_value);
    let p_target = match target_size_attribute {
        None => Vec::new(),
        Some(name) => match geometry::find_attribute(Owner::Point, name, geometry_value) {
            Some(found) => match attribute::View::storage(&found)? {
                Storage::Float(values) => values.clone(),
                _ => {
                    return Err(invalid_arg(format!(
                        "{OPERATION}: point target-size attribute {} must be scalar float",
                        quoted(name)
                    )));
                }
            },
            None => {
                return Err(invalid_arg(format!(
                    "{OPERATION}: point target-size attribute {} does not exist",
                    quoted(name)
                )));
            }
        },
    };
    let edge_total = index.edge_a.len();
    let mut l = Local {
        cancel: cancel_arc.clone(),
        grain,
        start: geometry_value.clone(),
        points,
        x: positions.x.to_vec(),
        y: positions.y.to_vec(),
        z: positions.z.to_vec(),
        p_left: (0..points).collect(),
        p_right: (0..points).collect(),
        p_weight: vec![0.0; points as usize],
        p_alive: vec![1; points as usize],
        p_used: vec![0; points as usize],
        p_first: vec![-1; points as usize],
        p_target,
        p_groups: point_groups
            .iter()
            .map(|item| (0..points).map(|point| group::mem(point, item) as u8).collect())
            .collect(),
        p_orders: point_groups.iter().map(group::ordered_elements).collect(),
        point_groups,
        v_orders: vertex_groups.iter().map(group::ordered_elements).collect(),
        vertex_groups,
        tris,
        c_point: topology_view.vertex_points[..corners as usize].to_vec(),
        c_left: (0..corners).collect(),
        c_right: (0..corners).collect(),
        c_weight: vec![0.0; corners as usize],
        c_next_at_point: vec![-1; corners as usize],
        c_edge: index.edge_of_vertex.to_vec(),
        c_next_in_edge: vec![-1; corners as usize],
        t_alive: vec![1; tris as usize],
        t_source: (0..tris).collect(),
        t_children: vec![-1; tris as usize],
        t_child_count: vec![0; tris as usize],
        source_tris: tris,
        edges: edge_total as i64,
        live_edges: edge_total as i64,
        e_a: index.edge_a.to_vec(),
        e_b: index.edge_b.to_vec(),
        e_first: vec![-1; edge_total],
        e_incidence: vec![0; edge_total],
        e_alive: vec![1; edge_total],
        edge_group_names: edge_groups.iter().map(|item| edge_group::name(item).to_owned()).collect(),
        e_groups: edge_groups.iter().map(|_| vec![0u8; edge_total]).collect(),
        scratch: vec![0; 64],
        scratch_count: 0,
    };
    for corner in 0..corners {
        cancel::poll(cancel_token, corner, 16383)?;
        let point = l.c_point[corner as usize];
        l.attach_point(corner, point);
        l.p_used[point as usize] = 1;
        l.attach_edge(corner, gi(&index.edge_of_vertex, corner));
    }
    for (slot, item) in edge_groups.iter().enumerate() {
        if edge_group::length(item) != l.edges {
            return Err(invalid_arg(format!(
                "{OPERATION}: internal edge group does not match its topology"
            )));
        }
        for edge in collect_edge_members(item) {
            l.e_groups[slot][edge as usize] = 1;
        }
    }
    Ok(l)
}

/// Output order: surviving source slots and the children of split slots, in source order; edges
/// by first sight along that corner sequence.
pub struct Ranking {
    pub tri_count: i64,
    pub order: Vec<i64>,
    pub t_rank: Vec<i64>,
    pub edge_count: i64,
    pub e_order: Vec<i64>,
    pub point_offsets: Vec<i64>,
    pub point_corners: Vec<i64>,
}

pub fn rank(l: &Local, incidence: bool) -> Result<Ranking, error::T> {
    let mut order = vec![0_i64; l.tris as usize];
    let mut count = 0_i64;
    for tri in 0..l.source_tris {
        cancel::poll(l.cancel_token(), tri, 4095)?;
        if l.t_alive[tri as usize] != 0 {
            order[count as usize] = tri;
            count += 1;
        } else {
            let first = l.t_children[tri as usize];
            if first >= 0 {
                for child in first..first + l.t_child_count[tri as usize] {
                    if l.t_alive[child as usize] != 0 {
                        order[count as usize] = child;
                        count += 1;
                    }
                }
            }
        }
    }
    let mut t_rank = vec![-1_i64; l.tris as usize];
    for position in 0..count {
        t_rank[order[position as usize] as usize] = position;
    }
    let mut e_rank = vec![-1_i64; l.edges as usize];
    let mut e_order = vec![0_i64; l.live_edges as usize];
    let mut edge_count = 0_i64;
    for position in 0..count {
        let first = order[position as usize] * 3;
        for corner in first..first + 3 {
            let edge = l.c_edge[corner as usize];
            if e_rank[edge as usize] < 0 {
                e_rank[edge as usize] = edge_count;
                e_order[edge_count as usize] = edge;
                edge_count += 1;
            }
        }
    }
    let (point_offsets, point_corners) = if !incidence {
        (Vec::new(), Vec::new())
    } else {
        let mut point_offsets = vec![0_i64; l.points as usize + 1];
        for position in 0..count {
            let first = order[position as usize] * 3;
            for corner in first..first + 3 {
                let point = l.c_point[corner as usize];
                point_offsets[point as usize + 1] += 1;
            }
        }
        for point in 0..l.points as usize {
            point_offsets[point + 1] += point_offsets[point];
        }
        let mut point_corners = vec![0_i64; (count * 3) as usize];
        let mut cursor = point_offsets[..l.points as usize].to_vec();
        for position in 0..count {
            let first = order[position as usize] * 3;
            for corner in first..first + 3 {
                let point = l.c_point[corner as usize] as usize;
                point_corners[cursor[point] as usize] = corner;
                cursor[point] += 1;
            }
        }
        (point_offsets, point_corners)
    };
    Ok(Ranking { tri_count: count, order, t_rank, edge_count, e_order, point_offsets, point_corners })
}

/// The two incident corners of a two-sided edge, ordered as `Topology_index` would list them: by
/// output corner position.
pub fn sided_corners(l: &Local, ranking: &Ranking, edge: i64) -> (i64, i64) {
    let first = l.e_first[edge as usize];
    let second = l.c_next_in_edge[first as usize];
    let position = |corner: i64| ranking.t_rank[(corner / 3) as usize] * 3 + (corner % 3);
    if position(first) <= position(second) { (first, second) } else { (second, first) }
}

pub fn local_target(l: &Local, target_length: f64, a: i64, b: i64) -> f64 {
    if l.p_target.is_empty() {
        target_length
    } else {
        0.5 * (l.p_target[a as usize] + l.p_target[b as usize])
    }
}

/// Selection over the freshly built structure: edge slots are in index order.
pub fn select_long(l: &Local, target_length: f64) -> Result<(Vec<u8>, i64), error::T> {
    let mut selected = vec![0u8; l.edges as usize];
    let mut count = 0_i64;
    for edge in 0..l.edges {
        cancel::poll(l.cancel_token(), edge, 4095)?;
        let a = l.e_a[edge as usize];
        let b = l.e_b[edge as usize];
        if l.local_length(a, b) > 4.0 / 3.0 * local_target(l, target_length, a, b) {
            selected[edge as usize] = 1;
            count += 1;
        }
    }
    Ok((selected, count))
}

pub fn interpolate_target(l: &Local, a: i64, b: i64) -> Result<f64, error::T> {
    let t = 0.5;
    let value = ((1.0 - t) * l.p_target[a as usize]) + (t * l.p_target[b as usize]);
    if !value.is_finite() || value <= 0.0 {
        return Err(invalid_arg(format!(
            "{OPERATION}: interpolated target size is not finite and positive between points {a} and {b}"
        )));
    }
    Ok(value)
}

/// Rebuild every point and edge incidence list from the live triangles in slot order, then retire
/// edges that no triangle references.
pub fn relink(l: &mut Local) -> Result<(), error::T> {
    let points = l.points as usize;
    let edges = l.edges as usize;
    l.p_first[..points].fill(-1);
    l.e_first[..edges].fill(-1);
    l.e_incidence[..edges].fill(0);
    for tri in 0..l.tris {
        cancel::poll(l.cancel_token(), tri, 4095)?;
        if l.t_alive[tri as usize] != 0 {
            for corner in tri * 3..tri * 3 + 3 {
                let point = l.c_point[corner as usize] as usize;
                let edge = l.c_edge[corner as usize] as usize;
                l.c_next_at_point[corner as usize] = l.p_first[point];
                l.p_first[point] = corner;
                l.c_next_in_edge[corner as usize] = l.e_first[edge];
                l.e_first[edge] = corner;
                l.e_incidence[edge] += 1;
            }
        }
    }
    for edge in 0..l.edges {
        if l.e_incidence[edge as usize] == 0 {
            l.edge_kill(edge);
        }
    }
    Ok(())
}

struct SplitState {
    saved_point: [i64; 3],
    saved_edge: [i64; 3],
    interior: [i64; 6],
}

const SPLIT_PATTERN: [&[[i64; 3]]; 8] = [
    &[],
    &[[0, 3, 2], [3, 1, 2]],
    &[[1, 4, 0], [4, 2, 0]],
    &[[0, 3, 2], [3, 4, 2], [3, 1, 4]],
    &[[2, 5, 1], [5, 0, 1]],
    &[[2, 5, 1], [5, 3, 1], [5, 0, 3]],
    &[[1, 4, 0], [4, 5, 0], [4, 2, 5]],
    &[[0, 3, 5], [3, 1, 4], [5, 4, 2], [3, 4, 5]],
];

struct SplitMaps {
    midpoint_of_edge: Vec<i64>,
    half_low: Vec<i64>,
    half_high: Vec<i64>,
}

fn split_point_of(st: &SplitState, maps: &SplitMaps, descriptor: i64) -> i64 {
    if descriptor < 3 {
        st.saved_point[descriptor as usize]
    } else {
        maps.midpoint_of_edge[st.saved_edge[(descriptor - 3) as usize] as usize]
    }
}

fn split_interior_edge(
    l: &mut Local,
    st: &mut SplitState,
    maps: &SplitMaps,
    low: i64,
    high: i64,
) -> Result<i64, error::T> {
    let slot = match (low, high) {
        (2, 3) => 0,
        (0, 4) => 1,
        (1, 5) => 2,
        (3, 4) => 3,
        (4, 5) => 4,
        (3, 5) => 5,
        _ => {
            return Err(invalid_arg(format!("{OPERATION}: split pattern references an unknown edge")));
        }
    };
    if st.interior[slot] < 0 {
        let a = split_point_of(st, maps, low);
        let b = split_point_of(st, maps, high);
        st.interior[slot] = l.edge_add(a.min(b), a.max(b))?;
    }
    Ok(st.interior[slot])
}

fn split_edge_of_pair(
    l: &mut Local,
    st: &mut SplitState,
    maps: &SplitMaps,
    d0: i64,
    d1: i64,
) -> Result<i64, error::T> {
    let low = d0.min(d1);
    let high = d0.max(d1);
    if high < 3 {
        Ok(st.saved_edge[(if (low + 1) % 3 == high { low } else { high }) as usize])
    } else if low >= 3 {
        split_interior_edge(l, st, maps, low, high)
    } else {
        let local = high - 3;
        if low == local || low == (local + 1) % 3 {
            let edge = st.saved_edge[local as usize];
            if st.saved_point[low as usize] == l.e_a[edge as usize] {
                Ok(maps.half_low[edge as usize])
            } else {
                Ok(maps.half_high[edge as usize])
            }
        } else {
            split_interior_edge(l, st, maps, low, high)
        }
    }
}

/// Split every selected edge at its midpoint and every incident triangle into two to four, in the
/// source split's emission pattern. Child edges are derived from the parent's: an unselected
/// parent edge keeps its slot, a selected one is replaced by its two halves (which inherit its
/// groups), and edges interior to the parent are fresh. No edge is looked up.
pub fn split(l: &mut Local, selected: &[u8], count: i64) -> Result<(), error::T> {
    let start_edges = l.edges;
    let start_points = l.points;
    l.ensure_points(checked_add("split point cardinality", start_points, count)?)?;
    l.ensure_edges(checked_add("split edge cardinality", start_edges, 2 * count)?)?;
    let mut maps = SplitMaps {
        midpoint_of_edge: vec![-1; start_edges as usize],
        half_low: vec![-1; start_edges as usize],
        half_high: vec![-1; start_edges as usize],
    };
    for edge in 0..start_edges {
        cancel::poll(l.cancel_token(), edge, 4095)?;
        if selected[edge as usize] != 0 {
            let point = l.points;
            l.points = point + 1;
            maps.midpoint_of_edge[edge as usize] = point;
            let a = l.e_a[edge as usize];
            let b = l.e_b[edge as usize];
            let (pu, au, bu) = (point as usize, a as usize, b as usize);
            l.x[pu] = (0.5 * l.x[au]) + (0.5 * l.x[bu]);
            l.y[pu] = (0.5 * l.y[au]) + (0.5 * l.y[bu]);
            l.z[pu] = (0.5 * l.z[au]) + (0.5 * l.z[bu]);
            if !(l.x[pu].is_finite() && l.y[pu].is_finite() && l.z[pu].is_finite()) {
                return Err(invalid_arg(format!("{OPERATION}: split point {point} is non-finite")));
            }
            l.p_left[pu] = a;
            l.p_right[pu] = b;
            l.p_weight[pu] = 0.5;
            l.p_alive[pu] = 1;
            l.p_used[pu] = 1;
            l.p_first[pu] = -1;
            if !l.p_target.is_empty() {
                l.p_target[pu] = interpolate_target(l, a, b)?;
            }
            for bytes in l.p_groups.iter_mut() {
                bytes[pu] = (bytes[au] != 0 && bytes[bu] != 0) as u8;
            }
            let low = l.edge_add(a, point)?;
            let high = l.edge_add(b, point)?;
            maps.half_low[edge as usize] = low;
            maps.half_high[edge as usize] = high;
            for bytes in l.e_groups.iter_mut() {
                let bit = bytes[edge as usize];
                bytes[low as usize] = bit;
                bytes[high as usize] = bit;
            }
        }
    }
    // Ordered point groups: unchanged members keep their order, new midpoint members follow in
    // point order.
    for index in 0..l.p_orders.len() {
        if let Some(order) = l.p_orders[index].clone() {
            let bytes = &l.p_groups[index];
            let mut extended = order;
            let before = extended.len();
            for point in start_points..l.points {
                if bytes[point as usize] != 0 {
                    extended.push(point);
                }
            }
            if extended.len() > before {
                l.p_orders[index] = Some(extended);
            }
        }
    }
    let first_child = l.tris;
    let mut additions = 0_i64;
    for tri in 0..l.source_tris {
        cancel::poll(l.cancel_token(), tri, 4095)?;
        let first = tri * 3;
        let mut mask = 0_i64;
        for local in 0..3 {
            if maps.midpoint_of_edge[l.c_edge[(first + local) as usize] as usize] >= 0 {
                mask |= 1 << local;
            }
        }
        if mask != 0 {
            additions = checked_add(
                "split primitive cardinality",
                additions,
                1 + (mask & 1) + ((mask >> 1) & 1) + ((mask >> 2) & 1),
            )?;
        }
    }
    l.ensure_tris(checked_add("split primitive cardinality", l.tris, additions)?)?;
    let mut st = SplitState { saved_point: [0; 3], saved_edge: [0; 3], interior: [-1; 6] };
    for tri in 0..l.source_tris {
        cancel::poll(l.cancel_token(), tri, 4095)?;
        let first = tri * 3;
        let mut mask = 0_usize;
        for local in 0..3 {
            let edge = l.c_edge[(first + local) as usize];
            st.saved_point[local as usize] = l.c_point[(first + local) as usize];
            st.saved_edge[local as usize] = edge;
            if maps.midpoint_of_edge[edge as usize] >= 0 {
                mask |= 1 << local;
            }
        }
        if mask != 0 {
            l.t_alive[tri as usize] = 0;
            st.interior = [-1; 6];
            let children = SPLIT_PATTERN[mask];
            l.t_children[tri as usize] = l.tris;
            l.t_child_count[tri as usize] = children.len() as i64;
            for descriptors in children.iter() {
                let child = l.tris;
                l.tris = child + 1;
                l.t_source[child as usize] = tri;
                l.t_children[child as usize] = -1;
                l.t_child_count[child as usize] = 0;
                l.t_alive[child as usize] = 1;
                let target = child * 3;
                for slot in 0..3_usize {
                    let descriptor = descriptors[slot];
                    let corner = target + slot as i64;
                    l.c_point[corner as usize] = split_point_of(&st, &maps, descriptor);
                    if descriptor < 3 {
                        l.c_left[corner as usize] = first + descriptor;
                        l.c_right[corner as usize] = first + descriptor;
                        l.c_weight[corner as usize] = 0.0;
                    } else {
                        let local = descriptor - 3;
                        l.c_left[corner as usize] = first + local;
                        l.c_right[corner as usize] = first + ((local + 1) % 3);
                        l.c_weight[corner as usize] = 0.5;
                    }
                    let edge =
                        split_edge_of_pair(l, &mut st, &maps, descriptor, descriptors[(slot + 1) % 3])?;
                    l.c_edge[corner as usize] = edge;
                }
            }
        }
    }
    relink(l)?;
    // Ordered vertex groups: a split corner is replaced by its unweighted descendants in slot
    // order; new midpoint corners that are members follow.
    if l.v_orders.iter().any(Option::is_some) {
        let corners = l.tris * 3;
        let start_corners = first_child * 3;
        let mut bucket_first = vec![-1_i64; start_corners as usize];
        let mut bucket_next = vec![-1_i64; corners as usize];
        for corner in (start_corners..corners).rev() {
            if l.c_weight[corner as usize] == 0.0 {
                let source = l.c_left[corner as usize];
                bucket_next[corner as usize] = bucket_first[source as usize];
                bucket_first[source as usize] = corner;
            }
        }
        for index in 0..l.v_orders.len() {
            let Some(order) = l.v_orders[index].clone() else {
                continue;
            };
            let group_value = &l.vertex_groups[index];
            let mut output: Vec<i64> = Vec::new();
            for &corner in order.iter() {
                if l.t_alive[(corner / 3) as usize] != 0 {
                    output.push(corner);
                } else {
                    let mut cursor = bucket_first[corner as usize];
                    while cursor >= 0 {
                        output.push(cursor);
                        cursor = bucket_next[cursor as usize];
                    }
                }
            }
            for corner in start_corners..corners {
                if l.c_weight[corner as usize] != 0.0
                    && group::mem(l.c_left[corner as usize], group_value)
                    && group::mem(l.c_right[corner as usize], group_value)
                {
                    output.push(corner);
                }
            }
            l.v_orders[index] = Some(output);
        }
    }
    Ok(())
}

/// Points held by hard selection, a feature edge, or a non-two-sided edge.
pub fn constrained_points(l: &Local, hard: i64, feature: i64) -> Vec<u8> {
    let mut out = vec![0u8; l.points as usize];
    if hard >= 0 {
        out.copy_from_slice(&l.p_groups[hard as usize][..l.points as usize]);
    }
    let features = if feature >= 0 { Some(&l.e_groups[feature as usize]) } else { None };
    for edge in 0..l.edges {
        if l.edge_live(edge)
            && (l.e_incidence[edge as usize] != 2
                || match features {
                    Some(bytes) => bytes[edge as usize] != 0,
                    None => false,
                })
        {
            out[l.e_a[edge as usize] as usize] = 1;
            out[l.e_b[edge as usize] as usize] = 1;
        }
    }
    out
}

/// Stable LSD radix sort of candidate ranks by the bit pattern of their non-negative lengths
/// (monotone in the value), four 16-bit passes; ties keep rank order.
pub fn sort_candidates(bits: &mut [u64], keys: &mut [i64], count: usize) {
    let mut scratch_bits = vec![0_u64; count];
    let mut scratch_keys = vec![0_i64; count];
    let mut counts = vec![0_usize; 65_537];
    fn pass(
        counts: &mut [usize],
        count: usize,
        source_bits: &[u64],
        source_keys: &[i64],
        target_bits: &mut [u64],
        target_keys: &mut [i64],
        shift: u32,
    ) {
        counts.fill(0);
        for index in 0..count {
            let digit = ((source_bits[index] >> shift) & 0xffff) as usize;
            counts[digit + 1] += 1;
        }
        for digit in 0..65_536 {
            counts[digit + 1] += counts[digit];
        }
        for index in 0..count {
            let value = source_bits[index];
            let digit = ((value >> shift) & 0xffff) as usize;
            let slot = counts[digit];
            counts[digit] = slot + 1;
            target_bits[slot] = value;
            target_keys[slot] = source_keys[index];
        }
    }
    // Four passes leave the sorted data back in `bits` and `keys`.
    pass(&mut counts, count, bits, keys, &mut scratch_bits, &mut scratch_keys, 0);
    pass(&mut counts, count, &scratch_bits, &scratch_keys, bits, keys, 16);
    pass(&mut counts, count, bits, keys, &mut scratch_bits, &mut scratch_keys, 32);
    pass(&mut counts, count, &scratch_bits, &scratch_keys, bits, keys, 48);
}

fn float_max(a: f64, b: f64) -> f64 {
    rdk_geom::point_grid::float_max(a, b)
}

/// The triangle keeps a non-degenerate normal on the old side after `a` and `b` move to the
/// midpoint.
#[allow(clippy::too_many_arguments)]
pub fn collapse_preserves_triangle(
    l: &Local,
    tri: i64,
    a: i64,
    b: i64,
    mx: f64,
    my: f64,
    mz: f64,
) -> bool {
    let first = (tri * 3) as usize;
    let p0 = l.c_point[first];
    let p1 = l.c_point[first + 1];
    let p2 = l.c_point[first + 2];
    let (u0, u1, u2) = (p0 as usize, p1 as usize, p2 as usize);
    let abx = l.x[u1] - l.x[u0];
    let aby = l.y[u1] - l.y[u0];
    let abz = l.z[u1] - l.z[u0];
    let acx = l.x[u2] - l.x[u0];
    let acy = l.y[u2] - l.y[u0];
    let acz = l.z[u2] - l.z[u0];
    let scale = float_max(
        float_max(abx.abs(), aby.abs()),
        float_max(abz.abs(), float_max(acx.abs(), float_max(acy.abs(), acz.abs()))),
    );
    let flat = scale == 0.0;
    let abx = if flat { 0.0 } else { abx / scale };
    let aby = if flat { 0.0 } else { aby / scale };
    let abz = if flat { 0.0 } else { abz / scale };
    let acx = if flat { 0.0 } else { acx / scale };
    let acy = if flat { 0.0 } else { acy / scale };
    let acz = if flat { 0.0 } else { acz / scale };
    let ox = (aby * acz) - (abz * acy);
    let oy = (abz * acx) - (abx * acz);
    let oz = (abx * acy) - (aby * acx);
    let moved = |p: i64| p == a || p == b;
    let x0 = if moved(p0) { mx } else { l.x[u0] };
    let y0 = if moved(p0) { my } else { l.y[u0] };
    let z0 = if moved(p0) { mz } else { l.z[u0] };
    let x1 = if moved(p1) { mx } else { l.x[u1] };
    let y1 = if moved(p1) { my } else { l.y[u1] };
    let z1 = if moved(p1) { mz } else { l.z[u1] };
    let x2 = if moved(p2) { mx } else { l.x[u2] };
    let y2 = if moved(p2) { my } else { l.y[u2] };
    let z2 = if moved(p2) { mz } else { l.z[u2] };
    let abx = x1 - x0;
    let aby = y1 - y0;
    let abz = z1 - z0;
    let acx = x2 - x0;
    let acy = y2 - y0;
    let acz = z2 - z0;
    let scale = float_max(
        float_max(abx.abs(), aby.abs()),
        float_max(abz.abs(), float_max(acx.abs(), float_max(acy.abs(), acz.abs()))),
    );
    if scale == 0.0 {
        return false;
    }
    let abx = abx / scale;
    let aby = aby / scale;
    let abz = abz / scale;
    let acx = acx / scale;
    let acy = acy / scale;
    let acz = acz / scale;
    let nx = (aby * acz) - (abz * acy);
    let ny = (abz * acx) - (abx * acz);
    let nz = (abx * acy) - (aby * acx);
    let new_norm2 = (nx * nx) + (ny * ny) + (nz * nz);
    let old_norm2 = (ox * ox) + (oy * oy) + (oz * oz);
    new_norm2 > 1e-28
        && old_norm2 > 1e-28
        && (ox * nx) + (oy * ny) + (oz * nz) > 1e-14 * (old_norm2 * new_norm2).sqrt()
}

/// Does collapsing `(a, b)` keep every other triangle at `point` valid and unreserved?
#[allow(clippy::too_many_arguments)]
pub fn inspect_point(
    l: &Local,
    ranking: &Ranking,
    used_tris: &[u8],
    tri_a: i64,
    tri_b: i64,
    a: i64,
    b: i64,
    mx: f64,
    my: f64,
    mz: f64,
    point: i64,
) -> bool {
    let first = gi(&ranking.point_offsets, point);
    let last = gi(&ranking.point_offsets, point + 1);
    let mut valid = true;
    let mut slot = first;
    while slot < last && valid {
        let tri = gi(&ranking.point_corners, slot) / 3;
        if tri != tri_a
            && tri != tri_b
            && (used_tris[tri as usize] != 0
                || !collapse_preserves_triangle(l, tri, a, b, mx, my, mz))
        {
            valid = false;
        }
        slot += 1;
    }
    valid
}

pub fn reserve_point(ranking: &Ranking, used_tris: &mut [u8], point: i64) {
    for slot in gi(&ranking.point_offsets, point)..gi(&ranking.point_offsets, point + 1) {
        used_tris[(gi(&ranking.point_corners, slot) / 3) as usize] = 1;
    }
}

/// The link condition: `a` and `b` share exactly the opposite points of the edge's two triangles
/// as neighbours.
#[allow(clippy::too_many_arguments)]
pub fn link_condition(
    l: &Local,
    ranking: &Ranking,
    stamps: &mut [i64],
    edge_stamps: &mut [i64],
    stamp: i64,
    v0: i64,
    v1: i64,
    a: i64,
    b: i64,
) -> bool {
    let c = l.opposite_point(v0);
    let d = l.opposite_point(v1);
    for slot in gi(&ranking.point_offsets, a)..gi(&ranking.point_offsets, a + 1) {
        let corner = gi(&ranking.point_corners, slot);
        let out = l.c_point[corner_next(corner) as usize];
        let incoming = l.c_point[corner_previous(corner) as usize];
        if out != a {
            stamps[out as usize] = stamp;
        }
        if incoming != a {
            stamps[incoming as usize] = stamp;
        }
    }
    let mut common = 0_i64;
    let mut valid = true;
    for slot in gi(&ranking.point_offsets, b)..gi(&ranking.point_offsets, b + 1) {
        let corner = gi(&ranking.point_corners, slot);
        let previous = corner_previous(corner);
        let out_edge = l.c_edge[corner as usize];
        let in_edge = l.c_edge[previous as usize];
        if edge_stamps[out_edge as usize] != stamp {
            edge_stamps[out_edge as usize] = stamp;
            let neighbor = l.c_point[corner_next(corner) as usize];
            if neighbor != b && stamps[neighbor as usize] == stamp {
                common += 1;
                if neighbor != c && neighbor != d {
                    valid = false;
                }
            }
        }
        if edge_stamps[in_edge as usize] != stamp {
            edge_stamps[in_edge as usize] = stamp;
            let neighbor = l.c_point[previous as usize];
            if neighbor != b && stamps[neighbor as usize] == stamp {
                common += 1;
                if neighbor != c && neighbor != d {
                    valid = false;
                }
            }
        }
    }
    valid && common == if c == d { 1 } else { 2 }
}

/// Independent short edges whose collapse keeps every neighbouring triangle oriented; greedy from
/// the shortest, ties by output edge index.
pub fn select_short(
    l: &Local,
    ranking: &Ranking,
    target_length: f64,
    hard: i64,
    feature: i64,
) -> Result<(Vec<u8>, i64), error::T> {
    let constrained = constrained_points(l, hard, feature);
    let features = if feature >= 0 { Some(&l.e_groups[feature as usize]) } else { None };
    let mut bits = vec![0_u64; ranking.edge_count as usize];
    let mut keys = vec![0_i64; ranking.edge_count as usize];
    let mut count = 0_usize;
    for position in 0..ranking.edge_count {
        cancel::poll(l.cancel_token(), position, 4095)?;
        let edge = ranking.e_order[position as usize];
        let a = l.e_a[edge as usize];
        let b = l.e_b[edge as usize];
        if l.e_incidence[edge as usize] == 2
            && (match features {
                Some(bytes) => bytes[edge as usize] == 0,
                None => true,
            })
            && constrained[a as usize] == 0
            && constrained[b as usize] == 0
        {
            let length = l.local_length(a, b);
            if length < 4.0 / 5.0 * local_target(l, target_length, a, b) {
                // OCaml's Int64.to_int keeps the low 63 bits; lengths are non-negative.
                bits[count] = length.to_bits() & ((1_u64 << 63) - 1);
                keys[count] = position;
                count += 1;
            }
        }
    }
    sort_candidates(&mut bits, &mut keys, count);
    let mut chosen = vec![0u8; l.edges as usize];
    let mut used_points = vec![0u8; l.points as usize];
    let mut used_tris = vec![0u8; l.tris as usize];
    let mut stamps = vec![-1_i64; l.points as usize];
    let mut edge_stamps = vec![-1_i64; l.edges as usize];
    let mut chosen_count = 0_i64;
    for stamp in 0..count as i64 {
        cancel::poll(l.cancel_token(), stamp, 4095)?;
        let edge = ranking.e_order[keys[stamp as usize] as usize];
        let a = l.e_a[edge as usize];
        let b = l.e_b[edge as usize];
        let (v0, v1) = sided_corners(l, ranking, edge);
        let tri_a = v0 / 3;
        let tri_b = v1 / 3;
        if used_points[a as usize] == 0
            && used_points[b as usize] == 0
            && used_tris[tri_a as usize] == 0
            && used_tris[tri_b as usize] == 0
            && link_condition(l, ranking, &mut stamps, &mut edge_stamps, stamp, v0, v1, a, b)
        {
            let mx = 0.5 * (l.x[a as usize] + l.x[b as usize]);
            let my = 0.5 * (l.y[a as usize] + l.y[b as usize]);
            let mz = 0.5 * (l.z[a as usize] + l.z[b as usize]);
            if inspect_point(l, ranking, &used_tris, tri_a, tri_b, a, b, mx, my, mz, a)
                && inspect_point(l, ranking, &used_tris, tri_a, tri_b, a, b, mx, my, mz, b)
            {
                chosen[edge as usize] = 1;
                chosen_count += 1;
                used_points[a as usize] = 1;
                used_points[b as usize] = 1;
                reserve_point(ranking, &mut used_tris, a);
                reserve_point(ranking, &mut used_tris, b);
            }
        }
    }
    Ok((chosen, chosen_count))
}

/// Average of two coordinates as the fuse kernel computes it.
pub fn average_pair(a: f64, b: f64) -> f64 {
    let sum = a + b;
    if sum.is_finite() {
        sum / 2.0
    } else {
        let scale = float_max(a.abs(), b.abs());
        let normalized = if scale != 0.0 { (a / scale) + (b / scale) } else { 0.0 };
        normalized / 2.0 * scale
    }
}

/// Collapse each chosen edge into its lower point: the two side triangles die, the higher point's
/// corners move over, its edges to the two opposite points merge into the lower point's, and its
/// other edges are renamed. The link condition guarantees those are the only shared neighbours,
/// so no edge is looked up. Merged targets union the groups of their sources, as the fuse
/// kernel's edge-group remap does.
pub fn collapse(l: &mut Local, chosen: &[u8]) -> Result<(), error::T> {
    for edge in 0..l.edges {
        cancel::poll(l.cancel_token(), edge, 4095)?;
        if chosen[edge as usize] == 0 {
            continue;
        }
        let a = l.e_a[edge as usize];
        let b = l.e_b[edge as usize];
        let (au, bu) = (a as usize, b as usize);
        l.x[au] = average_pair(l.x[au], l.x[bu]);
        l.y[au] = average_pair(l.y[au], l.y[bu]);
        l.z[au] = average_pair(l.z[au], l.z[bu]);
        if !(l.x[au].is_finite() && l.y[au].is_finite() && l.z[au].is_finite()) {
            return Err(invalid_arg(format!(
                "{OPERATION}: collapse position is non-finite at point {a}"
            )));
        }
        for bytes in l.p_groups.iter_mut() {
            if bytes[bu] != 0 {
                bytes[au] = 1;
            }
        }
        // Side triangles and their edges to the opposite points.
        let v0 = l.e_first[edge as usize];
        let v1 = l.c_next_in_edge[v0 as usize];
        // `corner` leaves the edge's first endpoint, `next` leaves the second towards the
        // opposite point, `previous` returns from it.
        let side = |l: &Local, corner: i64| -> (i64, i64, i64) {
            let next = corner_next(corner);
            let previous = corner_previous(corner);
            let (to_a, to_b) = if l.c_point[corner as usize] == a {
                (l.c_edge[previous as usize], l.c_edge[next as usize])
            } else {
                (l.c_edge[next as usize], l.c_edge[previous as usize])
            };
            (l.c_point[corner_next(next) as usize], to_a, to_b)
        };
        let (c, ac, bc) = side(l, v0);
        let (d, ad, bd) = side(l, v1);
        l.detach_triangle(v0 / 3);
        l.detach_triangle(v1 / 3);
        // Remaining triangles at b.
        l.scratch_count = 0;
        let mut corner = l.p_first[bu];
        while corner >= 0 {
            l.scratch_push(corner);
            corner = l.c_next_at_point[corner as usize];
        }
        for index in 0..l.scratch_count {
            let corner = l.scratch[index as usize];
            let previous = corner_previous(corner);
            let out = l.detach_edge_soft(corner);
            let incoming = l.detach_edge_soft(previous);
            l.detach_point(corner);
            l.attach_point(corner, a);
            let out_other = l.c_point[corner_next(corner) as usize];
            let out_target = collapse_retarget(l, a, c, d, ac, ad, out, out_other);
            l.attach_edge(corner, out_target);
            let in_other = l.c_point[previous as usize];
            let in_target = collapse_retarget(l, a, c, d, ac, ad, incoming, in_other);
            l.attach_edge(previous, in_target);
        }
        if l.e_incidence[bc as usize] == 0 {
            l.edge_kill(bc);
        }
        if l.e_incidence[bd as usize] == 0 {
            l.edge_kill(bd);
        }
        l.p_alive[bu] = 0;
        l.p_first[bu] = -1;
        l.p_left[bu] = a;
    }
    // Ordered point groups follow the fuse rule: visit the source order mapped to survivors, then
    // the remaining members in point order.
    for index in 0..l.p_orders.len() {
        let Some(order) = l.p_orders[index].clone() else {
            continue;
        };
        let bytes = &l.p_groups[index];
        let mut seen = vec![0u8; l.points as usize];
        let mut output: Vec<i64> = Vec::new();
        for &point in order.iter() {
            let point = if l.p_alive[point as usize] != 0 { point } else { l.p_left[point as usize] };
            if seen[point as usize] == 0 {
                seen[point as usize] = 1;
                if bytes[point as usize] != 0 {
                    output.push(point);
                }
            }
        }
        for point in 0..l.points as usize {
            if l.p_alive[point] != 0 && seen[point] == 0 && bytes[point] != 0 {
                output.push(point as i64);
            }
        }
        l.p_orders[index] = Some(output);
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn collapse_retarget(
    l: &mut Local,
    a: i64,
    c: i64,
    d: i64,
    ac: i64,
    ad: i64,
    source: i64,
    other: i64,
) -> i64 {
    if other == c {
        collapse_merge(l, source, ac)
    } else if other == d {
        collapse_merge(l, source, ad)
    } else {
        l.e_a[source as usize] = a.min(other);
        l.e_b[source as usize] = a.max(other);
        source
    }
}

fn collapse_merge(l: &mut Local, source: i64, target: i64) -> i64 {
    l.edge_revive(target);
    for bytes in l.e_groups.iter_mut() {
        if bytes[source as usize] != 0 {
            bytes[target as usize] = 1;
        }
    }
    target
}

#[allow(clippy::too_many_arguments)]
pub fn same_orientation_local(l: &Local, a0: i64, b0: i64, c0: i64, a1: i64, b1: i64, c1: i64) -> bool {
    let (a0, b0, c0, a1, b1, c1) =
        (a0 as usize, b0 as usize, c0 as usize, a1 as usize, b1 as usize, c1 as usize);
    let abx = l.x[b0] - l.x[a0];
    let aby = l.y[b0] - l.y[a0];
    let abz = l.z[b0] - l.z[a0];
    let acx = l.x[c0] - l.x[a0];
    let acy = l.y[c0] - l.y[a0];
    let acz = l.z[c0] - l.z[a0];
    let ox = (aby * acz) - (abz * acy);
    let oy = (abz * acx) - (abx * acz);
    let oz = (abx * acy) - (aby * acx);
    let abx = l.x[b1] - l.x[a1];
    let aby = l.y[b1] - l.y[a1];
    let abz = l.z[b1] - l.z[a1];
    let acx = l.x[c1] - l.x[a1];
    let acy = l.y[c1] - l.y[a1];
    let acz = l.z[c1] - l.z[a1];
    let nx = (aby * acz) - (abz * acy);
    let ny = (abz * acx) - (abx * acz);
    let nz = (abx * acy) - (aby * acx);
    let old2 = (ox * ox) + (oy * oy) + (oz * oz);
    let new2 = (nx * nx) + (ny * ny) + (nz * nz);
    old2 > 0.0 && new2 > 0.0 && (ox * nx) + (oy * ny) + (oz * nz) > 1e-14 * (old2 * new2).sqrt()
}

/// Are `a` and `b` joined by an edge? Checked along `a`'s live corners.
pub fn adjacent(ranking: &Ranking, l: &Local, a: i64, b: i64) -> bool {
    let mut found = false;
    for slot in gi(&ranking.point_offsets, a)..gi(&ranking.point_offsets, a + 1) {
        let corner = gi(&ranking.point_corners, slot);
        if l.c_point[corner_next(corner) as usize] == b
            || l.c_point[corner_previous(corner) as usize] == b
        {
            found = true;
        }
    }
    found
}

/// Independent two-sided edges whose flip lowers the valence penalty and keeps both triangles
/// oriented; greedy in output edge order.
pub fn select_flip(l: &Local, ranking: &Ranking, hard: i64, feature: i64) -> Result<(Vec<u8>, i64), error::T> {
    let constrained = constrained_points(l, hard, feature);
    let features = if feature >= 0 { Some(&l.e_groups[feature as usize]) } else { None };
    let mut valence = vec![0_i64; l.points as usize];
    let mut boundary = vec![0u8; l.points as usize];
    for edge in 0..l.edges {
        if l.edge_live(edge) {
            let a = l.e_a[edge as usize];
            let b = l.e_b[edge as usize];
            valence[a as usize] += 1;
            if b != a {
                valence[b as usize] += 1;
            }
            if l.e_incidence[edge as usize] != 2 {
                boundary[a as usize] = 1;
                boundary[b as usize] = 1;
            }
        }
    }
    let ideal = |point: i64| if boundary[point as usize] != 0 { 4_i64 } else { 6_i64 };
    let penalty = |point: i64, degree: i64| (degree - ideal(point)).abs();
    let mut eligible = vec![0u8; l.edges as usize];
    for position in 0..ranking.edge_count {
        cancel::poll(l.cancel_token(), position, 4095)?;
        let edge = ranking.e_order[position as usize];
        if l.e_incidence[edge as usize] == 2
            && !match features {
                Some(bytes) => bytes[edge as usize] != 0,
                None => false,
            }
        {
            let (va, vb) = sided_corners(l, ranking, edge);
            if va / 3 != vb / 3 {
                let a = l.e_a[edge as usize];
                let b = l.e_b[edge as usize];
                let c = l.opposite_point(va);
                let d = l.opposite_point(vb);
                if a != b
                    && a != c
                    && a != d
                    && b != c
                    && b != d
                    && c != d
                    && constrained[a as usize] == 0
                    && constrained[b as usize] == 0
                    && !adjacent(ranking, l, c, d)
                {
                    let (va_, vb_, vc_, vd_) = (
                        valence[a as usize],
                        valence[b as usize],
                        valence[c as usize],
                        valence[d as usize],
                    );
                    let before = penalty(a, va_) + penalty(b, vb_) + penalty(c, vc_) + penalty(d, vd_);
                    let after = penalty(a, va_ - 1)
                        + penalty(b, vb_ - 1)
                        + penalty(c, vc_ + 1)
                        + penalty(d, vd_ + 1);
                    if after < before
                        && same_orientation_local(l, a, b, c, d, c, b)
                        && same_orientation_local(l, b, a, d, c, d, a)
                    {
                        eligible[edge as usize] = 1;
                    }
                }
            }
        }
    }
    let mut chosen = vec![0u8; l.edges as usize];
    let mut used_tris = vec![0u8; l.tris as usize];
    let mut count = 0_i64;
    for position in 0..ranking.edge_count {
        let edge = ranking.e_order[position as usize];
        if eligible[edge as usize] != 0 {
            let (va, vb) = sided_corners(l, ranking, edge);
            let tri_a = va / 3;
            let tri_b = vb / 3;
            if used_tris[tri_a as usize] == 0 && used_tris[tri_b as usize] == 0 {
                chosen[edge as usize] = 1;
                count += 1;
                used_tris[tri_a as usize] = 1;
                used_tris[tri_b as usize] = 1;
            }
        }
    }
    Ok((chosen, count))
}

fn rotate_slots(l: &mut Local, tri: i64, slot0: i64, point0: i64, point1: i64, point2: i64) {
    let first = tri * 3;
    let slot = |local: i64| first + ((slot0 - first + local) % 3);
    let left: Vec<i64> = (0..3).map(|local| l.c_left[slot(local) as usize]).collect();
    let right: Vec<i64> = (0..3).map(|local| l.c_right[slot(local) as usize]).collect();
    let weight: Vec<f64> = (0..3).map(|local| l.c_weight[slot(local) as usize]).collect();
    for local in 0..3_i64 {
        let corner = slot(local) as usize;
        let source = ((local + 1) % 3) as usize;
        l.c_left[corner] = left[source];
        l.c_right[corner] = right[source];
        l.c_weight[corner] = weight[source];
    }
    l.attach_point(slot(0), point0);
    l.attach_point(slot(1), point1);
    l.attach_point(slot(2), point2);
}

/// Rotate each chosen edge: triangle A (a0, b0, c) becomes (d, c, a0) and triangle B (b0, a0, d)
/// becomes (c, d, b0) in their own corner slots, corner payload rotating one slot with them; the
/// new diagonal inherits the edge's groups.
pub fn flip(l: &mut Local, ranking: &Ranking, chosen: &[u8]) -> Result<(), error::T> {
    let mut saved = vec![0u8; l.e_groups.len()];
    // Corner payload moves one slot back; ordered vertex groups follow it.
    let tracking = l.v_orders.iter().any(Option::is_some);
    let mut slot_map: Vec<i64> = if tracking { (0..l.tris * 3).collect() } else { Vec::new() };
    for position in 0..ranking.edge_count {
        let edge = ranking.e_order[position as usize];
        if chosen[edge as usize] == 0 {
            continue;
        }
        let (va, vb) = sided_corners(l, ranking, edge);
        let tri_a = va / 3;
        let tri_b = vb / 3;
        let a0 = l.c_point[va as usize];
        let c = l.opposite_point(va);
        let d = l.opposite_point(vb);
        for (index, bytes) in l.e_groups.iter().enumerate() {
            saved[index] = bytes[edge as usize];
        }
        let ac = l.c_edge[corner_next(corner_next(va)) as usize];
        let bc = l.c_edge[corner_next(va) as usize];
        let bd = l.c_edge[corner_next(corner_next(vb)) as usize];
        let ad = l.c_edge[corner_next(vb) as usize];
        let first_a = tri_a * 3;
        let first_b = tri_b * 3;
        for corner in first_a..first_a + 3 {
            l.detach_edge_soft(corner);
        }
        for corner in first_b..first_b + 3 {
            l.detach_edge_soft(corner);
        }
        for corner in first_a..first_a + 3 {
            l.detach_point(corner);
        }
        for corner in first_b..first_b + 3 {
            l.detach_point(corner);
        }
        l.edge_kill(edge);
        // An earlier flip in this pass may already have made (c, d): the shared edge then carries
        // this flip's groups, as the last remap wins.
        let existing = l.adjacent_edge(c, d);
        let diagonal = if existing == -1 { l.edge_add(c.min(d), c.max(d))? } else { existing };
        let b0 = l.c_point[corner_next(va) as usize];
        rotate_slots(l, tri_a, va, d, c, a0);
        rotate_slots(l, tri_b, vb, c, d, b0);
        // A is now (d, c, a0): edges (d,c) (c,a0) (a0,d); B is (c, d, b0): edges (c,d) (d,b0)
        // (b0,c).
        let slot_a = |local: i64| first_a + ((va - first_a + local) % 3);
        let slot_b = |local: i64| first_b + ((vb - first_b + local) % 3);
        l.attach_edge(slot_a(0), diagonal);
        l.attach_edge(slot_a(1), ac);
        l.attach_edge(slot_a(2), ad);
        l.attach_edge(slot_b(0), diagonal);
        l.attach_edge(slot_b(1), bd);
        l.attach_edge(slot_b(2), bc);
        for (index, bytes) in l.e_groups.iter_mut().enumerate() {
            bytes[diagonal as usize] = saved[index];
        }
        if tracking {
            for local in 0..3_i64 {
                slot_map[(first_a + ((va - first_a + local + 1) % 3)) as usize] =
                    first_a + ((va - first_a + local) % 3);
                slot_map[(first_b + ((vb - first_b + local + 1) % 3)) as usize] =
                    first_b + ((vb - first_b + local) % 3);
            }
        }
    }
    for order in l.v_orders.iter_mut().flatten() {
        for corner in order.iter_mut() {
            *corner = slot_map[*corner as usize];
        }
    }
    Ok(())
}

/// One Geometry per iteration: compact live points (dropping points that lost every triangle),
/// emit triangles in output order, interpolate payload once through ancestry, and rebuild groups
/// and edge groups.
pub fn materialize(l: &Local) -> Result<(geometry::T, Vec<i64>), error::T> {
    let cancel_token = l.cancel_token();
    let ranking = rank(l, false)?;
    let mut used_after = vec![0u8; l.points as usize];
    for position in 0..ranking.tri_count {
        let first = ranking.order[position as usize] * 3;
        used_after[l.c_point[first as usize] as usize] = 1;
        used_after[l.c_point[first as usize + 1] as usize] = 1;
        used_after[l.c_point[first as usize + 2] as usize] = 1;
    }
    let mut p_map = vec![-1_i64; l.points as usize];
    let mut point_total = 0_i64;
    for point in 0..l.points {
        cancel::poll(cancel_token, point, 16383)?;
        if l.p_alive[point as usize] != 0
            && (used_after[point as usize] != 0 || l.p_used[point as usize] == 0)
        {
            p_map[point as usize] = point_total;
            point_total += 1;
        }
    }
    let point_count = point_total;
    let mut x = vec![0.0_f64; point_count as usize];
    let mut y = vec![0.0_f64; point_count as usize];
    let mut z = vec![0.0_f64; point_count as usize];
    let mut point_left = vec![0_i64; point_count as usize];
    let mut point_right = vec![0_i64; point_count as usize];
    let mut point_weight = vec![0.0_f64; point_count as usize];
    for point in 0..l.points as usize {
        let target = p_map[point];
        if target >= 0 {
            let t = target as usize;
            x[t] = l.x[point];
            y[t] = l.y[point];
            z[t] = l.z[point];
            point_left[t] = l.p_left[point];
            point_right[t] = l.p_right[point];
            point_weight[t] = l.p_weight[point];
        }
    }
    let corner_count = ranking.tri_count * 3;
    let mut vertex_points = vec![0_i64; corner_count as usize];
    let mut vertex_left = vec![0_i64; corner_count as usize];
    let mut vertex_right = vec![0_i64; corner_count as usize];
    let mut vertex_weight = vec![0.0_f64; corner_count as usize];
    let mut primitive_source = vec![0_i64; ranking.tri_count as usize];
    for position in 0..ranking.tri_count {
        cancel::poll(cancel_token, position, 4095)?;
        let tri = ranking.order[position as usize];
        primitive_source[position as usize] = l.t_source[tri as usize];
        for local in 0..3 {
            let corner = (tri * 3 + local) as usize;
            let target = (position * 3 + local) as usize;
            let point = p_map[l.c_point[corner] as usize];
            if point < 0 {
                return Err(invalid_arg(format!("{OPERATION}: output references a dropped point")));
            }
            vertex_points[target] = point;
            vertex_left[target] = l.c_left[corner];
            vertex_right[target] = l.c_right[corner];
            vertex_weight[target] = l.c_weight[corner];
        }
    }
    let topology_value = topology::View::create_validated_owned(
        point_count,
        vertex_points,
        (0..=ranking.tri_count).map(|position| position * 3).collect(),
        vec![0u8; ranking.tri_count as usize],
    );
    let grain = Some(l.grain);
    let mut attributes = Vec::new();
    for item in geometry::attributes(&l.start) {
        if attribute::name(&item) == "N"
            && (attribute::owner(&item) == Owner::Point || attribute::owner(&item) == Owner::Vertex)
        {
            continue;
        }
        attributes.push(crate::subdivide_refine::interpolate_owned_attribute(
            cancel_token,
            grain,
            &point_left,
            &point_right,
            &point_weight,
            &vertex_left,
            &vertex_right,
            &vertex_weight,
            &primitive_source,
            &item,
        )?);
    }
    // Groups keep the source list order; point and vertex groups take their tracked membership
    // and order, primitive groups follow ancestry.
    let mut next_point = 0_usize;
    let mut next_vertex = 0_usize;
    let mut groups = Vec::new();
    for source_group in geometry::groups(&l.start) {
        match group::owner(&source_group) {
            group::Owner::Point => {
                let index = next_point;
                next_point += 1;
                let bytes = &l.p_groups[index];
                let mut builder =
                    group::Builder::create(group::Owner::Point, group::name(&source_group), point_count)?;
                for point in 0..l.points {
                    let target = p_map[point as usize];
                    if target >= 0 && bytes[point as usize] != 0 {
                        group::Builder::set(&mut builder, target, true)?;
                    }
                }
                let target = group::Builder::freeze(&mut builder)?;
                match &l.p_orders[index] {
                    None => groups.push(target),
                    Some(order) => {
                        let mapped: Vec<i64> = order
                            .iter()
                            .filter_map(|&point| {
                                let mapped = p_map[point as usize];
                                if mapped >= 0 { Some(mapped) } else { None }
                            })
                            .collect();
                        groups.push(group::View::with_owned_order(mapped, &target)?);
                    }
                }
            }
            group::Owner::Vertex => {
                let index = next_vertex;
                next_vertex += 1;
                cancel::check_opt(cancel_token)?;
                let target = group::init(
                    grain,
                    group::Owner::Vertex,
                    group::name(&source_group),
                    corner_count,
                    &|corner| {
                        group::mem(gi(&vertex_left, corner), &source_group)
                            && group::mem(gi(&vertex_right, corner), &source_group)
                    },
                )?;
                match &l.v_orders[index] {
                    None => groups.push(target),
                    Some(order) => {
                        let mut corner_rank = vec![-1_i64; (l.tris * 3) as usize];
                        for position in 0..ranking.tri_count {
                            let first = ranking.order[position as usize] * 3;
                            for local in 0..3 {
                                corner_rank[(first + local) as usize] = position * 3 + local;
                            }
                        }
                        let mapped: Vec<i64> = order
                            .iter()
                            .filter_map(|&corner| {
                                let mapped = corner_rank[corner as usize];
                                if mapped >= 0 { Some(mapped) } else { None }
                            })
                            .collect();
                        groups.push(group::View::with_owned_order(mapped, &target)?);
                    }
                }
            }
            group::Owner::Primitive => {
                cancel::check_opt(cancel_token)?;
                let target = group::init(
                    grain,
                    group::Owner::Primitive,
                    group::name(&source_group),
                    ranking.tri_count,
                    &|position| group::mem(gi(&primitive_source, position), &source_group),
                )?;
                if !group::is_ordered(&source_group) {
                    groups.push(target);
                } else {
                    groups.push(group::View::remap_order(&source_group, &primitive_source, &target)?);
                }
            }
        }
    }
    let mut edge_groups = Vec::new();
    for (index, name) in l.edge_group_names.iter().enumerate() {
        let source = &l.e_groups[index];
        let mut bits = vec![0u8; ((ranking.edge_count + 7) / 8) as usize];
        for position in 0..ranking.edge_count {
            if source[ranking.e_order[position as usize] as usize] != 0 {
                bits[(position >> 3) as usize] |= 1 << (position & 7);
            }
        }
        edge_groups.push(edge_group::View::of_owned_bits(
            &topology_value,
            ranking.edge_count,
            name,
            bits,
        )?);
    }
    let created = geometry::create(
        packed::Float3::View::of_owned_exn(x, y, z)?,
        topology_value,
        Some(attributes),
        Some(groups),
        Some(edge_groups),
    )?;
    Ok((created, p_map))
}

/// Smoothing constraints over the materialized points.
pub fn constraint_group(
    l: &Local,
    point_map: &[i64],
    hard: i64,
    feature: i64,
    name: &str,
    count: i64,
) -> Result<group::T, error::T> {
    let bytes = constrained_points(l, hard, feature);
    let mut out = vec![0u8; count as usize];
    for point in 0..l.points as usize {
        let target = point_map[point];
        if target >= 0 && bytes[point] != 0 {
            out[target as usize] = 1;
        }
    }
    cancel::check_opt(l.cancel_token())?;
    group::init(Some(l.grain), group::Owner::Point, name, count, &|point| out[point as usize] != 0)
}

pub fn projection_scratch(scratch: &mut Option<ProjectionScratch>, count: i64) {
    let reusable = matches!(scratch, Some(value) if value.count == count);
    if !reusable {
        *scratch = Some(ProjectionScratch {
            count,
            primitives: vec![-1; count as usize],
            triangles: vec![-1; count as usize],
            barycentric_a: vec![0.0; count as usize],
            barycentric_b: vec![0.0; count as usize],
            barycentric_c: vec![0.0; count as usize],
            distances_squared: vec![f64::INFINITY; count as usize],
        });
    }
}

pub fn project_to_reference(
    cancel_token: Option<&cancel::T>,
    grain: i64,
    surface: &rdk_geom::surface_index::T,
    scratch: &mut Option<ProjectionScratch>,
    reference: &geometry::T,
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    let count = geometry::point_count(geometry_value);
    let topology_value = geometry::topology(geometry_value);
    let index = topology_index::create(cancel_token, topology_value)?;
    cancel::check_opt(cancel_token)?;
    let used = group::init(
        Some(grain),
        group::Owner::Point,
        "__rdk_remesh_used",
        count,
        &|point| topology_index::point_incidence_count(&index, point).unwrap_or(0) > 0,
    )?;
    projection_scratch(scratch, count);
    let Some(scratch) = scratch.as_mut() else {
        return Err(fail("internal projection scratch is missing"));
    };
    rdk_geom::surface_index::closest_many_into(
        cancel_token,
        Some(&used),
        None,
        grain,
        surface,
        geometry::positions(geometry_value),
        f64::INFINITY,
        &mut scratch.primitives,
        &mut scratch.triangles,
        &mut scratch.barycentric_a,
        &mut scratch.barycentric_b,
        &mut scratch.barycentric_c,
        &mut scratch.distances_squared,
    )?;
    let triangles = &scratch.triangles;
    let a = &scratch.barycentric_a;
    let b = &scratch.barycentric_b;
    let c = &scratch.barycentric_c;
    let mut missed = -1_i64;
    for point in collect_group_points(&used) {
        if triangles[point as usize] < 0 && missed < 0 {
            missed = point;
        }
    }
    if missed >= 0 {
        return Err(fail(&format!("could not project point {missed} to the input surface")));
    }
    let source_positions = packed::Float3::View::view(geometry::positions(reference))?;
    let source_topology = topology::View::view(geometry::topology(reference))?;
    let current = packed::Float3::View::view(geometry::positions(geometry_value))?;
    let (mut x, mut y, mut z) = (current.x.to_vec(), current.y.to_vec(), current.z.to_vec());
    {
        let (wx, wy, wz) = (Windows::even(&mut x, grain), Windows::even(&mut y, grain), Windows::even(&mut z, grain));
        support::run_indexed_ranges(grain, count, &|range, first, last| {
            let (x, y, z) = (wx.take(range), wy.take(range), wz.take(range));
            for point in first..last {
                support::poll(cancel_token, point)?;
                if group::mem(point, &used) {
                    let at = (point - first) as usize;
                    let triangle = triangles[point as usize];
                    let vertex0 = rdk_geom::surface_index::View::triangle_vertex(surface, triangle, 0);
                    let vertex1 = rdk_geom::surface_index::View::triangle_vertex(surface, triangle, 1);
                    let vertex2 = rdk_geom::surface_index::View::triangle_vertex(surface, triangle, 2);
                    let p0 = gi(&source_topology.vertex_points, vertex0);
                    let p1 = gi(&source_topology.vertex_points, vertex1);
                    let p2 = gi(&source_topology.vertex_points, vertex2);
                    let (pa, pb, pc) = (a[point as usize], b[point as usize], c[point as usize]);
                    x[at] = (pa * gf(&source_positions.x, p0))
                        + (pb * gf(&source_positions.x, p1))
                        + (pc * gf(&source_positions.x, p2));
                    y[at] = (pa * gf(&source_positions.y, p0))
                        + (pb * gf(&source_positions.y, p1))
                        + (pc * gf(&source_positions.y, p2));
                    z[at] = (pa * gf(&source_positions.z, p0))
                        + (pb * gf(&source_positions.z, p1))
                        + (pc * gf(&source_positions.z, p2));
                }
            }
            Ok(())
        })?;
    }
    geometry::with_positions(
        packed::Float3::View::of_owned_exn(x, y, z)?,
        geometry_value,
    )
}

fn collect_group_points(selection: &group::T) -> Vec<i64> {
    let members = std::cell::RefCell::new(Vec::new());
    group::iter(&|element| members.borrow_mut().push(element), selection);
    members.into_inner()
}

#[allow(clippy::too_many_arguments)]
pub fn install_outputs(
    cancel_token: Option<&cancel::T>,
    grain: i64,
    target_length: f64,
    target_size_attribute: Option<&str>,
    edge_name: &str,
    output_hard_edges: Option<&str>,
    output_mesh_size: Option<&str>,
    output_quality: Option<&str>,
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    let topology_value = geometry::topology(geometry_value);
    let topology_view = topology::View::view(topology_value)?;
    let features = feature_group(edge_name, geometry_value)?;
    let with_hard_edges = match output_hard_edges {
        None => geometry_value.clone(),
        Some(name) => {
            let index_value = topology_index::create(cancel_token, topology_value)?;
            let index = topology_index::View::view(&index_value);
            cancel::check_opt(cancel_token)?;
            let created = edge_group::init(
                Some(grain),
                topology_value,
                &index_value,
                name,
                &|edge| {
                    edge_group::mem(edge, &features)
                        || gi(&index.edge_offsets, edge + 1) - gi(&index.edge_offsets, edge) != 2
                },
            )?;
            geometry::with_edge_group(created, geometry_value)?
        }
    };
    let with_size = match output_mesh_size {
        None => with_hard_edges,
        Some(name) => match target_size_attribute {
            Some(source) => match geometry::find_attribute(Owner::Point, source, &with_hard_edges) {
                Some(found) => {
                    let renamed = attribute::with_name(name, &found)?;
                    geometry::with_attribute(renamed, &with_hard_edges)?
                }
                None => return Err(fail("internal target-size attribute ancestry was lost")),
            },
            None => {
                let values = vec![target_length; geometry::point_count(&with_hard_edges) as usize];
                let created = attribute::create_owned(name, Owner::Point, Storage::Float(values))?;
                geometry::with_attribute(created, &with_hard_edges)?
            }
        },
    };
    match output_quality {
        None => Ok(with_size),
        Some(name) => {
            let positions = packed::Float3::View::view(geometry::positions(&with_size))?;
            let primitive_count = geometry::primitive_count(&with_size);
            let mut quality = vec![0.0; primitive_count as usize];
            support::par_fill(cancel_token, grain, &mut quality, &|primitive| {
                if !primitive_is_triangle(&topology_view, primitive) {
                    return Ok(0.0);
                }
                let first = gi(&topology_view.primitive_offsets, primitive);
                let p0 = gi(&topology_view.vertex_points, first);
                let p1 = gi(&topology_view.vertex_points, first + 1);
                let p2 = gi(&topology_view.vertex_points, first + 2);
                let dx01 = gf(&positions.x, p1) - gf(&positions.x, p0);
                let dy01 = gf(&positions.y, p1) - gf(&positions.y, p0);
                let dz01 = gf(&positions.z, p1) - gf(&positions.z, p0);
                let dx12 = gf(&positions.x, p2) - gf(&positions.x, p1);
                let dy12 = gf(&positions.y, p2) - gf(&positions.y, p1);
                let dz12 = gf(&positions.z, p2) - gf(&positions.z, p1);
                let dx20 = gf(&positions.x, p0) - gf(&positions.x, p2);
                let dy20 = gf(&positions.y, p0) - gf(&positions.y, p2);
                let dz20 = gf(&positions.z, p0) - gf(&positions.z, p2);
                let scale = float_max(
                    float_max(dx01.abs(), dy01.abs()),
                    float_max(
                        dz01.abs(),
                        float_max(
                            dx12.abs(),
                            float_max(
                                dy12.abs(),
                                float_max(
                                    dz12.abs(),
                                    float_max(dx20.abs(), float_max(dy20.abs(), dz20.abs())),
                                ),
                            ),
                        ),
                    ),
                );
                if scale == 0.0 {
                    return Ok(0.0);
                }
                let ax = dx01 / scale;
                let ay = dy01 / scale;
                let az = dz01 / scale;
                let bx = -dx20 / scale;
                let by = -dy20 / scale;
                let bz = -dz20 / scale;
                let cx = (ay * bz) - (az * by);
                let cy = (az * bx) - (ax * bz);
                let cz = (ax * by) - (ay * bx);
                let area2 = ((cx * cx) + (cy * cy) + (cz * cz)).sqrt();
                let lengths2 = (dx01 / scale).powf(2.0)
                    + (dy01 / scale).powf(2.0)
                    + (dz01 / scale).powf(2.0)
                    + (dx12 / scale).powf(2.0)
                    + (dy12 / scale).powf(2.0)
                    + (dz12 / scale).powf(2.0)
                    + (dx20 / scale).powf(2.0)
                    + (dy20 / scale).powf(2.0)
                    + (dz20 / scale).powf(2.0);
                Ok(if lengths2 == 0.0 {
                    0.0
                } else {
                    fmin(1.0, 2.0 * 3.0_f64.sqrt() * area2 / lengths2)
                })
            })?;
            let created = attribute::create_owned(
                name,
                Owner::Primitive,
                Storage::Float(quality),
            )?;
            geometry::with_attribute(created, &with_size)
        }
    }
}

#[derive(Clone)]
pub struct Params {
    pub cancel: Option<Arc<cancel::T>>,
    pub grain: i64,
    pub iterations: i64,
    pub smoothing: f64,
    pub project: bool,
    pub use_input_points_only: bool,
    pub hard_points: Option<group::T>,
    pub hard_edges: Option<edge_group::T>,
    pub target_size_attribute: Option<String>,
    pub preserve_uv_seams: bool,
    pub uv_attribute: String,
    pub output_hard_edges: Option<String>,
    pub output_mesh_size: Option<String>,
    pub output_quality: Option<String>,
    pub recompute_point_normals: bool,
    pub target_length: f64,
}

/// `Error.guard ~operation:"remesh" ~code:"invalid_remesh"`.
fn guard(result: Result<geometry::T, error::T>) -> Result<geometry::T, error::T> {
    match result {
        Ok(value) => Ok(value),
        Err(failure) => {
            let (code, message) = if error::code(&failure) == "cancelled" {
                ("cancelled", "geometry operation was cancelled".to_owned())
            } else {
                ("invalid_remesh", error::message(&failure).to_owned())
            };
            Err(error::make("remesh".to_owned(), code.to_owned(), message, None)
                .unwrap_or_else(|built| built))
        }
    }
}

/// A failure that stops the iteration loop but not the process: the message becomes the result.
fn recoverable(failure: error::T) -> Result<String, error::T> {
    let code = error::code(&failure);
    if code == "cancelled" || code == "invalid_argument" {
        Err(failure)
    } else {
        Ok(error::message(&failure).to_owned())
    }
}

fn remesh_body(params: &Params, geometry_value: &geometry::T) -> Result<geometry::T, error::T> {
    let cancel_arc = &params.cancel;
    let cancel_token = cancel_arc.as_deref();
    let grain = params.grain;
    let iterations = params.iterations;
    let target_length = params.target_length;
    let smoothing = params.smoothing;
    if grain <= 0 {
        return Err(fail("grain must be positive"));
    }
    if iterations < 0 {
        return Err(fail("iterations must be non-negative"));
    }
    if !target_length.is_finite() || target_length <= 0.0 {
        return Err(fail("target length must be finite and positive"));
    }
    if !smoothing.is_finite() || !(0.0..=1.0).contains(&smoothing) {
        return Err(fail("smoothing must be finite and in [0, 1]"));
    }
    validate_name("output hard-edge group", params.output_hard_edges.as_deref())?;
    validate_name("output mesh-size attribute", params.output_mesh_size.as_deref())?;
    validate_name("output quality attribute", params.output_quality.as_deref())?;
    validate_point_group(geometry_value, params.hard_points.as_ref())?;
    validate_edge_group(geometry_value, params.hard_edges.as_ref())?;
    validate_positions(cancel_token, geometry_value)?;
    target_values(cancel_token, params.target_size_attribute.as_deref(), geometry_value)?;
    let point_name = fresh_group_name(geometry_value, group::Owner::Point, "__rdk_remesh_hard_points", None);
    let edge_name = fresh_edge_group_name(
        geometry_value,
        "__rdk_remesh_features",
        params.output_hard_edges.as_deref(),
    );
    let prepared = install_feature_groups(
        cancel_token,
        grain,
        params.hard_points.as_ref(),
        params.hard_edges.as_ref(),
        params.preserve_uv_seams,
        &params.uv_attribute,
        &point_name,
        &edge_name,
        geometry_value,
    )?;
    let initial = crate::triangulate::run(
        &crate::triangulate::Params {
            cancel: params.cancel.clone(),
            grain: Some(grain),
            primitives: None,
        },
        &prepared,
    )?;
    let mut current = initial;
    let mut failure: Option<String> = None;
    let mut iteration = 0_i64;
    let mut projection_scratch_slot: Option<ProjectionScratch> = None;
    let projection_surface = if params.project && iterations > 0 {
        match rdk_geom::surface_index::create(cancel_token, Some(grain), None, None, None, geometry_value)
        {
            Ok(surface) => Some(surface),
            Err(failure_value) if error::code(&failure_value) == "cancelled" => {
                return Err(failure_value);
            }
            Err(failure_value) => {
                failure = Some(error::to_string(&failure_value));
                None
            }
        }
    } else {
        None
    };
    // A structure whose iteration changed nothing topologically is kept for the next one:
    // smoothing and projection only move points.
    let mut cached: Option<(Local, Ranking)> = None;
    while iteration < iterations && failure.is_none() {
        cancel::check_opt(cancel_token)?;
        let (mut l, cached_ranking) = match cached.take() {
            Some((mut l, ranking)) => {
                let positions = packed::Float3::View::view(geometry::positions(&current))?;
                let points = l.points as usize;
                l.x[..points].copy_from_slice(&positions.x[..points]);
                l.y[..points].copy_from_slice(&positions.y[..points]);
                l.z[..points].copy_from_slice(&positions.z[..points]);
                l.start = current.clone();
                (l, Some(ranking))
            }
            None => (
                local_of_geometry(
                    cancel_arc,
                    grain,
                    params.target_size_attribute.as_deref(),
                    &current,
                )?,
                None,
            ),
        };
        let hard = l.point_group_index(&point_name);
        let feature = l.edge_group_index(&edge_name);
        if hard < 0 {
            failure = Some(format!("{OPERATION}: internal hard-point ancestry was lost"));
        } else if feature < 0 {
            failure = Some(format!("{OPERATION}: internal feature-edge ancestry was lost"));
        } else {
            let mut changed = false;
            if !params.use_input_points_only {
                let (selected, count) = select_long(&l, target_length)?;
                if count > 0 {
                    split(&mut l, &selected, count)?;
                    changed = true;
                }
                let ranking = rank(&l, true)?;
                let (chosen, count) = select_short(&l, &ranking, target_length, hard, feature)?;
                if count > 0 {
                    collapse(&mut l, &chosen)?;
                    changed = true;
                }
            }
            let ranking = match cached_ranking {
                Some(ranking) if !changed => ranking,
                _ => rank(&l, true)?,
            };
            let (chosen, count) = select_flip(&l, &ranking, hard, feature)?;
            let mut stash = false;
            if count > 0 {
                flip(&mut l, &ranking, &chosen)?;
                changed = true;
            } else if !changed {
                stash = true;
            }
            let point_map: Option<Vec<i64>> = if !changed {
                Some((0..l.points).collect())
            } else {
                match materialize(&l) {
                    Err(failed) => {
                        failure = Some(recoverable(failed)?);
                        None
                    }
                    Ok((materialized, point_map)) => {
                        current = materialized;
                        Some(point_map)
                    }
                }
            };
            if let Some(point_map) = &point_map {
                if smoothing > 0.0 {
                    let constrained = constraint_group(
                        &l,
                        point_map,
                        hard,
                        feature,
                        "__rdk_remesh_constraints",
                        geometry::point_count(&current),
                    )?;
                    match crate::smooth::run(
                        &crate::smooth::Params {
                            cancel: params.cancel.clone(),
                            grain: Some(grain),
                            constrained_points: Some(constrained),
                            boundary: crate::smooth::Boundary::SmoothUnshared,
                            iterations: 1,
                            mode: crate::attribute_blur::Mode::Laplacian(smoothing),
                            recompute_normals: false,
                            attributes: "P".to_owned(),
                        },
                        &current,
                    ) {
                        Err(smooth_failure) if error::code(&smooth_failure) == "cancelled" => {
                            return Err(smooth_failure);
                        }
                        Err(smooth_failure) => failure = Some(error::to_string(&smooth_failure)),
                        Ok(smoothed) => current = smoothed,
                    }
                }
            }
            if failure.is_none() && params.project {
                let Some(surface) = projection_surface.as_ref() else {
                    return Err(invalid_arg("option is None".to_owned()));
                };
                match project_to_reference(
                    cancel_token,
                    grain,
                    surface,
                    &mut projection_scratch_slot,
                    geometry_value,
                    &current,
                ) {
                    Err(failed) => failure = Some(recoverable(failed)?),
                    Ok(projected) => current = projected,
                }
            }
            if stash {
                cached = Some((l, ranking));
            }
        }
        iteration += 1;
    }
    if let Some(message) = failure {
        return Err(error::make("remesh".to_owned(), "invalid_remesh".to_owned(), message, None)
            .unwrap_or_else(|built| built));
    }
    let output = geometry::without_attribute(Owner::Point, "N", &current);
    let output = geometry::without_attribute(Owner::Vertex, "N", &output);
    let output = if !params.recompute_point_normals {
        output
    } else {
        crate::deform::normals(
            crate::deform::Params { cancel: params.cancel.clone(), grain, primitives: None },
            &output,
        )?
    };
    let installed = install_outputs(
        cancel_token,
        grain,
        target_length,
        params.target_size_attribute.as_deref(),
        &edge_name,
        params.output_hard_edges.as_deref(),
        params.output_mesh_size.as_deref(),
        params.output_quality.as_deref(),
        &output,
    )?;
    let installed = geometry::without_group(group::Owner::Point, &point_name, &installed);
    Ok(geometry::without_edge_group(&edge_name, &installed))
}

pub fn run(params: &Params, geometry_value: &geometry::T) -> Result<geometry::T, error::T> {
    // `with Invalid_argument message -> fail message`: the message is prefixed once more.
    let result = remesh_body(params, geometry_value).map_err(|failure| {
        if error::code(&failure) == "invalid_argument" {
            fail(error::message(&failure))
        } else {
            failure
        }
    });
    guard(result)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Mutex;

    fn defaults(target_length: f64) -> Params {
        Params {
            cancel: None,
            grain: 16_384,
            iterations: 3,
            smoothing: 0.5,
            project: true,
            use_input_points_only: false,
            hard_points: None,
            hard_edges: None,
            target_size_attribute: None,
            preserve_uv_seams: true,
            uv_attribute: "uv".to_owned(),
            output_hard_edges: None,
            output_mesh_size: None,
            output_quality: None,
            recompute_point_normals: true,
            target_length,
        }
    }

    fn remesh(
        source: &geometry::T,
        target_length: f64,
        configure: &dyn Fn(&mut Params),
    ) -> Result<geometry::T, error::T> {
        let mut params = defaults(target_length);
        configure(&mut params);
        run(&params, source)
    }

    fn ok(source: &geometry::T, target_length: f64, configure: &dyn Fn(&mut Params)) -> geometry::T {
        remesh(source, target_length, configure)
            .unwrap_or_else(|failure| panic!("{}", error::to_string(&failure)))
    }

    fn add_attribute(value: &geometry::T, owner: Owner, name: &str, storage: Storage) -> geometry::T {
        geometry::with_attribute(attribute::create_owned(name, owner, storage).unwrap(), value).unwrap()
    }

    /// `Plane_generators.grid`: point counts, xz plane, alternating triangles.
    fn grid(columns: i64, rows: i64, size: f64, uv: bool) -> geometry::T {
        let mut x = Vec::new();
        let mut z = Vec::new();
        let mut u = Vec::new();
        let mut v = Vec::new();
        for row in 0..rows {
            let point_v = row as f64 / (rows - 1) as f64;
            for column in 0..columns {
                let point_u = column as f64 / (columns - 1) as f64;
                x.push((point_u - 0.5) * size);
                z.push((point_v - 0.5) * size);
                if uv {
                    u.push(point_u);
                    v.push(point_v);
                }
            }
        }
        let mut vertex_points = Vec::new();
        let mut offsets = vec![0_i64];
        for row in 0..rows - 1 {
            for column in 0..columns - 1 {
                let p00 = row * columns + column;
                let (p01, p10) = (p00 + 1, p00 + columns);
                let p11 = p10 + 1;
                if (row + column) % 2 == 0 {
                    vertex_points.extend([p00, p10, p11, p00, p11, p01]);
                } else {
                    vertex_points.extend([p00, p10, p01, p01, p10, p11]);
                }
                offsets.push(vertex_points.len() as i64 - 3);
                offsets.push(vertex_points.len() as i64);
            }
        }
        let count = x.len();
        let base = geometry::create(
            packed::Float3::View::of_owned_exn(x, vec![0.0; count], z).unwrap(),
            topology::polygons_owned(count as i64, vertex_points, offsets).unwrap(),
            None,
            None,
            None,
        )
        .unwrap();
        let base = add_attribute(
            &base,
            Owner::Point,
            "N",
            Storage::Float3(
                packed::Float3::View::of_owned_exn(vec![0.0; count], vec![1.0; count], vec![0.0; count])
                    .unwrap(),
            ),
        );
        if uv {
            add_attribute(&base, Owner::Point, "uv", Storage::Float2(packed::Float2::of_owned(u, v).unwrap()))
        } else {
            base
        }
    }

    #[test]
    fn grid_fixture_matches_ocaml_generator() {
        let source = grid(9, 7, 6.0, true);
        let topology = topology::View::view(geometry::topology(&source)).unwrap();
        assert_eq!(&topology.vertex_points[..12], &[0, 9, 10, 0, 10, 1, 1, 10, 2, 2, 10, 11]);
        let positions = packed::Float3::View::view(geometry::positions(&source)).unwrap();
        assert_eq!(positions.z[36].to_bits(), 0x3fef_ffff_ffff_fffe);
        let uv = geometry::find_attribute(Owner::Point, "uv", &source).unwrap();
        let Storage::Float2(uv) = attribute::View::storage(&uv).unwrap() else {
            panic!("grid fixture UV storage drifted");
        };
        let uv = packed::Float2::View::view(uv);
        assert_eq!(uv.y.len(), 63);
        assert_eq!(uv.y[9].to_bits(), (1.0_f64 / 6.0).to_bits());
    }

    fn scalar(owner: Owner, name: &str, value: &geometry::T) -> Vec<f64> {
        match attribute::View::storage(&geometry::find_attribute(owner, name, value).unwrap()).unwrap() {
            Storage::Float(values) => values.clone(),
            _ => panic!("{name} has wrong storage"),
        }
    }

    fn check_triangles(value: &geometry::T) {
        let view = topology::View::view(geometry::topology(value)).unwrap();
        for primitive in 0..geometry::primitive_count(value) as usize {
            assert!(
                view.primitive_kinds[primitive] == 0
                    && view.primitive_offsets[primitive + 1] - view.primitive_offsets[primitive] == 3,
                "Remesh emitted a non-triangle primitive"
            );
        }
    }

    fn edge_pairs(value: &geometry::T, found: &edge_group::T) -> Vec<(i64, i64)> {
        let index = topology_index::create(None, geometry::topology(value)).unwrap();
        let mut pairs: Vec<(i64, i64)> = collect_edge_members(found)
            .into_iter()
            .map(|edge| topology_index::edge_points(&index, edge).unwrap())
            .collect();
        pairs.sort();
        pairs
    }

    fn bits(values: &[f64]) -> Vec<u64> {
        values.iter().map(|value| value.to_bits()).collect()
    }

    fn geometry_equal(left: &geometry::T, right: &geometry::T) -> bool {
        let (lp, rp) = (
            packed::Float3::View::view(geometry::positions(left)).unwrap(),
            packed::Float3::View::view(geometry::positions(right)).unwrap(),
        );
        let (lt, rt) = (
            topology::View::view(geometry::topology(left)).unwrap(),
            topology::View::view(geometry::topology(right)).unwrap(),
        );
        if bits(&lp.x) != bits(&rp.x)
            || bits(&lp.y) != bits(&rp.y)
            || bits(&lp.z) != bits(&rp.z)
            || lt.point_count != rt.point_count
            || lt.vertex_points != rt.vertex_points
            || lt.primitive_offsets != rt.primitive_offsets
            || lt.primitive_kinds != rt.primitive_kinds
        {
            return false;
        }
        let (la, ra) = (geometry::attributes(left), geometry::attributes(right));
        if la.len() != ra.len() {
            return false;
        }
        for (a, b) in la.iter().zip(ra.iter()) {
            if attribute::name(a) != attribute::name(b) || attribute::owner(a) != attribute::owner(b) {
                return false;
            }
            let same = match (attribute::View::storage(a).unwrap(), attribute::View::storage(b).unwrap()) {
                (Storage::Float(p), Storage::Float(q)) => bits(p) == bits(q),
                (Storage::Int(p), Storage::Int(q)) => p == q,
                (Storage::Text(p), Storage::Text(q)) => p == q,
                (Storage::Float2(p), Storage::Float2(q)) => {
                    let (p, q) = (packed::Float2::View::view(p), packed::Float2::View::view(q));
                    bits(&p.x) == bits(&q.x) && bits(&p.y) == bits(&q.y)
                }
                (Storage::Float3(p), Storage::Float3(q)) => {
                    let (p, q) = (
                        packed::Float3::View::view(p).unwrap(),
                        packed::Float3::View::view(q).unwrap(),
                    );
                    bits(&p.x) == bits(&q.x) && bits(&p.y) == bits(&q.y) && bits(&p.z) == bits(&q.z)
                }
                _ => true,
            };
            if !same {
                return false;
            }
        }
        let (lg, rg) = (geometry::groups(left), geometry::groups(right));
        if lg.len() != rg.len() {
            return false;
        }
        for (a, b) in lg.iter().zip(rg.iter()) {
            if group::owner(a) != group::owner(b)
                || group::name(a) != group::name(b)
                || group::length(a) != group::length(b)
                || group::ordered_elements(a) != group::ordered_elements(b)
                || group::View::bits_view(a) != group::View::bits_view(b)
            {
                return false;
            }
        }
        let (le, re) = (geometry::edge_groups(left), geometry::edge_groups(right));
        le.len() == re.len()
            && le.iter().zip(re.iter()).all(|(a, b)| {
                edge_group::name(a) == edge_group::name(b) && edge_pairs(left, a) == edge_pairs(right, b)
            })
    }

    fn with_domains(domains: i64, work: &(dyn Fn() -> geometry::T + Sync)) -> geometry::T {
        let slot: Mutex<Option<geometry::T>> = Mutex::new(None);
        parallel::run_unit(Some(domains), &|| {
            *slot.lock().unwrap() = Some(work());
            Ok(())
        })
        .unwrap();
        slot.into_inner().unwrap().unwrap()
    }

    fn seam_fixture() -> (geometry::T, group::T) {
        let base = geometry::create(
            packed::Float3::View::of_owned_exn(
                vec![0.0, 1.0, 1.0, 0.0],
                vec![0.0, 0.0, 1.0, 1.0],
                vec![0.0; 4],
            )
            .unwrap(),
            topology::polygons_owned(4, vec![0, 1, 2, 0, 2, 3], vec![0, 3, 6]).unwrap(),
            None,
            None,
            None,
        )
        .unwrap();
        let uv = packed::Float2::of_owned(
            vec![0.0, 1.0, 1.0, 2.0, 3.0, 2.0],
            vec![0.0, 0.0, 1.0, 0.0, 1.0, 1.0],
        )
        .unwrap();
        let value = add_attribute(&base, Owner::Vertex, "uv", Storage::Float2(uv));
        let value = add_attribute(&value, Owner::Point, "target", Storage::Float(vec![0.4; 4]));
        let value = add_attribute(&value, Owner::Detail, "tag", Storage::Text(vec![b"fixture".to_vec()]));
        let hard_points = group::init(None, group::Owner::Point, "pin", 4, &|point| point == 0).unwrap();
        (value, hard_points)
    }

    #[test]
    fn adaptive_hard_and_uv_seams() {
        let (source, hard_points) = seam_fixture();
        let detail = geometry::find_attribute(Owner::Detail, "tag", &source).unwrap();
        let output = ok(&source, 1.0, &|p| {
            p.grain = 3;
            p.iterations = 2;
            p.target_size_attribute = Some("target".to_owned());
            p.hard_points = Some(hard_points.clone());
            p.output_hard_edges = Some("features".to_owned());
            p.output_mesh_size = Some("effective_size".to_owned());
            p.output_quality = Some("quality".to_owned());
        });
        check_triangles(&output);
        for value in scalar(Owner::Point, "effective_size", &output) {
            assert!(value == 0.4, "Remesh adaptive target-size ancestry drift");
        }
        let output_detail = geometry::find_attribute(Owner::Detail, "tag", &output).unwrap();
        assert!(
            attribute::storage_id(&detail) == attribute::storage_id(&output_detail),
            "Remesh copied immutable detail payload"
        );
        let features = geometry::find_edge_group("features", &output).unwrap();
        let index = topology_index::View::view(
            &topology_index::create(None, geometry::topology(&output)).unwrap(),
        );
        let boundary_edges = (0..index.edge_a.len())
            .filter(|&edge| index.edge_offsets[edge + 1] - index.edge_offsets[edge] == 1)
            .count() as i64;
        assert!(
            edge_group::cardinality(&features) > boundary_edges,
            "Remesh did not preserve the UV discontinuity as a hard interior feature"
        );
        let positions = packed::Float3::View::view(geometry::positions(&output)).unwrap();
        let pinned = (0..positions.x.len())
            .any(|point| positions.x[point] == 0.0 && positions.y[point] == 0.0 && positions.z[point] == 0.0);
        assert!(pinned, "Remesh removed or moved an explicit hard point");
    }

    #[test]
    fn input_points_only() {
        let source = geometry::create(
            packed::Float3::View::of_owned_exn(
                vec![0.0, 2.0, 2.0, 0.0],
                vec![0.0, 0.0, 1.0, 1.0],
                vec![0.0; 4],
            )
            .unwrap(),
            topology::polygons_owned(4, vec![0, 1, 2, 3], vec![0, 4]).unwrap(),
            None,
            None,
            None,
        )
        .unwrap();
        let output = ok(&source, 0.1, &|p| {
            p.iterations = 2;
            p.smoothing = 0.0;
            p.use_input_points_only = true;
            p.recompute_point_normals = false;
        });
        assert!(
            geometry::point_count(&output) == 4 && geometry::primitive_count(&output) == 2,
            "Remesh input-points-only mode changed point cardinality"
        );
        check_triangles(&output);
    }

    // OCaml 5.3 runtime/hash.c and runtime/ints.c: only the Int64/string cases used
    // by test_remesh.ml's byte-level chained-kernel oracle.
    fn fixture_hash(value: &geometry::T) -> i64 {
        fn word(hash: u32, value: u32) -> u32 {
            let value = value.wrapping_mul(0xcc9e2d51).rotate_left(15).wrapping_mul(0x1b873593);
            (hash ^ value).rotate_left(13).wrapping_mul(5).wrapping_add(0xe6546b64)
        }
        fn finish(mut hash: u32) -> i64 {
            hash ^= hash >> 16;
            hash = hash.wrapping_mul(0x85ebca6b);
            hash ^= hash >> 13;
            hash = hash.wrapping_mul(0xc2b2ae35);
            i64::from((hash ^ (hash >> 16)) & 0x3fffffff)
        }
        fn mix(hash: i64, value: i64) -> i64 {
            rays_math::hash::mix_with(1_000_003, hash, value) & ((1_i64 << 62) - 1)
        }
        fn ints(values: &[i64]) -> i64 {
            values.iter().fold(17, |hash, &value| mix(hash, value))
        }
        fn floats(values: &[f64]) -> i64 {
            values.iter().fold(17, |hash, value| {
                let bits = value.to_bits();
                mix(hash, finish(word(0, (bits as u32) ^ ((bits >> 32) as u32))))
            })
        }
        fn text(value: &[u8]) -> i64 {
            let mut hash = 0;
            for chunk in value.chunks(4) {
                let mut bytes = [0; 4];
                bytes[..chunk.len()].copy_from_slice(chunk);
                hash = word(hash, u32::from_le_bytes(bytes));
            }
            finish(hash ^ value.len() as u32)
        }
        let positions = packed::Float3::View::view(geometry::positions(value)).unwrap();
        let topology = topology::View::view(geometry::topology(value)).unwrap();
        let mut hash = 17;
        hash = mix(hash, mix(mix(floats(&positions.x), floats(&positions.y)), floats(&positions.z)));
        hash = mix(hash, ints(&topology.vertex_points));
        hash = mix(hash, ints(&topology.primitive_offsets));
        for item in geometry::attributes(value) {
            let component = match attribute::View::storage(&item).unwrap() {
                Storage::Float(values) => floats(values),
                Storage::Int(values) => ints(values),
                Storage::Text(values) => values.iter().fold(17, |hash, value| mix(hash, text(value))),
                Storage::Float2(values) => {
                    let planes = packed::Float2::View::view(values);
                    mix(floats(&planes.x), floats(&planes.y))
                }
                Storage::Float3(values) => {
                    let planes = packed::Float3::View::view(values).unwrap();
                    mix(mix(floats(&planes.x), floats(&planes.y)), floats(&planes.z))
                }
                _ => 1,
            };
            hash = mix(hash, component);
        }
        for item in geometry::groups(value) {
            let mut members = 17;
            for element in 0..group::length(&item) {
                if group::mem(element, &item) {
                    members = mix(members, element);
                }
            }
            let order = group::ordered_elements(&item).map_or(0, |values| ints(&values));
            hash = mix(hash, mix(members, order));
        }
        for item in geometry::edge_groups(value) {
            let component = edge_pairs(value, &item).iter().fold(17, |hash, &(a, b)| mix(mix(hash, a), b));
            hash = mix(hash, component);
        }
        hash
    }

    fn payload_fixture() -> (geometry::T, group::T) {
        let source = grid(9, 7, 6.0, true);
        let point_count = geometry::point_count(&source);
        let vertex_count = geometry::vertex_count(&source);
        let primitive_count = geometry::primitive_count(&source);
        let source = add_attribute(
            &source,
            Owner::Point,
            "point_id",
            Storage::Int((0..point_count).map(|point| point * 3).collect()),
        );
        let source = add_attribute(
            &source,
            Owner::Primitive,
            "material",
            Storage::Text(
                (0..primitive_count)
                    .map(|primitive| if primitive % 3 == 0 { b"red".to_vec() } else { b"blue".to_vec() })
                    .collect(),
            ),
        );
        let source = add_attribute(&source, Owner::Detail, "tag", Storage::Text(vec![b"payload".to_vec()]));
        let ordered_members: Vec<i64> =
            (0..point_count / 2).map(|index| point_count - 1 - (index * 2)).collect();
        let ordered =
            group::ordered(group::Owner::Point, "ordered_points", point_count, &ordered_members).unwrap();
        let corners =
            group::init(None, group::Owner::Vertex, "corners", vertex_count, &|v| v % 5 != 2).unwrap();
        let faces =
            group::init(None, group::Owner::Primitive, "faces", primitive_count, &|p| p % 4 < 2).unwrap();
        let pinned =
            group::init(None, group::Owner::Point, "pinned", point_count, &|p| p == 12 || p == 40).unwrap();
        let index = topology_index::create(None, geometry::topology(&source)).unwrap();
        let authored = edge_group::init(
            None,
            geometry::topology(&source),
            &index,
            "authored_edges",
            &|edge| edge % 3 != 1,
        )
        .unwrap();
        let mut value = source;
        for item in [ordered, corners, faces, pinned.clone()] {
            value = geometry::with_group(item, &value).unwrap();
        }
        (geometry::with_edge_group(authored, &value).unwrap(), pinned)
    }

    #[test]
    fn local_edits_keep_payload_and_agree_across_domains() {
        let (source, pinned) = payload_fixture();
        let run_domains = |domains: i64| {
            with_domains(domains, &|| {
                ok(&source, 0.8, &|p| {
                    p.grain = 13;
                    p.iterations = 2;
                    p.smoothing = 0.3;
                    p.project = true;
                    p.hard_points = Some(pinned.clone());
                    p.output_hard_edges = Some("hard".to_owned());
                    p.output_mesh_size = Some("size".to_owned());
                    p.output_quality = Some("quality".to_owned());
                })
            })
        };
        let output = run_domains(1);
        check_triangles(&output);
        assert!(
            group::is_ordered(&geometry::find_group(group::Owner::Point, "ordered_points", &output).unwrap()),
            "Remesh dropped the explicit order of a point group"
        );
        assert!(
            geometry::find_edge_group("authored_edges", &output).is_some(),
            "Remesh dropped an authored edge group"
        );
        assert_eq!(
            fixture_hash(&output),
            if cfg!(windows) { 3_181_404_250_202_190_309 } else { 212_434_250_856_407_980 },
            "Remesh payload fixture drifted from the chained kernels"
        );
        let flips_only = ok(&source, 0.8, &|p| {
            p.grain = 7;
            p.iterations = 2;
            p.smoothing = 0.0;
            p.project = false;
            p.use_input_points_only = true;
            p.recompute_point_normals = false;
        });
        assert_eq!(
            geometry::point_count(&flips_only),
            geometry::point_count(&source),
            "Remesh input-points-only fixture changed point cardinality"
        );
        assert_eq!(
            fixture_hash(&flips_only),
            1_893_477_268_345_860_603,
            "Remesh flip-only fixture drifted from the chained kernels"
        );
        assert!(
            geometry_equal(&output, &run_domains(4)),
            "Remesh payload fixture differs between one and four domains"
        );
    }

    #[test]
    fn errors_and_cancellation() {
        let source = grid(4, 4, 6.0, false);
        let expect = |result: Result<geometry::T, error::T>, code: &str| match result {
            Err(failure) => assert_eq!(error::code(&failure), code, "unexpected error code"),
            Ok(_) => panic!("Remesh accepted invalid input for {code}"),
        };
        expect(remesh(&source, 0.0, &|_| {}), "invalid_remesh");
        expect(remesh(&source, 1.0, &|p| p.iterations = -1), "invalid_remesh");
        expect(remesh(&source, 1.0, &|p| p.smoothing = 1.1), "invalid_remesh");
        let wrong = group::init(
            None,
            group::Owner::Vertex,
            "wrong",
            geometry::vertex_count(&source),
            &|_| true,
        )
        .unwrap();
        expect(remesh(&source, 1.0, &|p| p.hard_points = Some(wrong.clone())), "invalid_remesh");
        let sizes: Vec<f64> = (0..geometry::point_count(&source))
            .map(|point| if point == 2 { f64::NAN } else { 1.0 })
            .collect();
        let bad_size = add_attribute(&source, Owner::Point, "size", Storage::Float(sizes));
        expect(
            remesh(&bad_size, 1.0, &|p| p.target_size_attribute = Some("size".to_owned())),
            "invalid_remesh",
        );
        let mut builder = topology::Builder::create(3, Some(3), Some(1)).unwrap();
        topology::Builder::add_open_polyline(&mut builder, &[0, 1, 2]).unwrap();
        let curve = geometry::create(
            packed::Float3::View::of_owned_exn(vec![0.0, 1.0, 2.0], vec![0.0; 3], vec![0.0; 3]).unwrap(),
            topology::Builder::freeze(&mut builder).unwrap(),
            None,
            None,
            None,
        )
        .unwrap();
        expect(remesh(&curve, 1.0, &|_| {}), "invalid_remesh");
        let token = Arc::new(cancel::create());
        cancel::cancel(&token);
        expect(remesh(&source, 1.0, &|p| p.cancel = Some(token.clone())), "cancelled");
    }

    #[test]
    fn parallel_exact() {
        let source = grid(14, 12, 6.0, false);
        let run_domains = |domains: i64| {
            with_domains(domains, &|| {
                ok(&source, 0.45, &|p| {
                    p.grain = 31;
                    p.iterations = 1;
                    p.output_hard_edges = Some("hard".to_owned());
                    p.output_mesh_size = Some("size".to_owned());
                    p.output_quality = Some("quality".to_owned());
                })
            })
        };
        assert!(
            geometry_equal(&run_domains(1), &run_domains(4)),
            "Remesh one-domain/multi-domain geometry drift"
        );
    }
}
