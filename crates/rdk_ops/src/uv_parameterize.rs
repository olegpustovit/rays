//! Mean-value UV parameterization of triangle meshes (flatten to circles, or relax given UVs).
//!
//! OCaml returns `(Geometry.t, string) result` and lets `Cancel.Cancelled` escape; here a failure is
//! an `error::T` with operation `Uv_parameterize.solve`, code `invalid_parameter` and the OCaml
//! message, and a cancellation keeps code `cancelled`.
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};

use crate::transform_ops::ocaml_quote;
use rdk_core::{
    attribute, cancel, edge_group, error, geometry, group, packed, support, topology, topology_index,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BoundaryMode {
    Circle,
    Preserve,
}

#[derive(Clone)]
pub struct Params {
    pub cancel: Option<Arc<cancel::T>>,
    pub grain: Option<i64>,
    pub name: String,
    pub seams: Option<group::T>,
    pub edge_seams: Option<edge_group::T>,
    pub uv_tolerance: f64,
    pub iterations: i64,
    pub tolerance: f64,
}

fn fail(message: &str) -> error::T {
    error::make(
        "Uv_parameterize.solve".to_owned(),
        "invalid_parameter".to_owned(),
        message.to_owned(),
        None,
    )
    .unwrap_or_else(|bad| bad)
}

fn finite(value: f64) -> bool {
    value.is_finite()
}


fn find_root(parent: &mut [i64], value: i64) -> i64 {
    let mut root = value;
    while parent[root as usize] != root {
        root = parent[root as usize];
    }
    let mut at = value;
    while parent[at as usize] != root {
        let next = parent[at as usize];
        parent[at as usize] = root;
        at = next;
    }
    root
}

fn union_roots(parent: &mut [i64], rank: &mut [u8], left: i64, right: i64) {
    let left = find_root(parent, left);
    let right = find_root(parent, right);
    if left != right {
        let (left_rank, right_rank) = (rank[left as usize], rank[right as usize]);
        if left_rank < right_rank {
            parent[left as usize] = right;
        } else if right_rank < left_rank {
            parent[right as usize] = left;
        } else {
            let (root, child) = if left < right { (left, right) } else { (right, left) };
            parent[child as usize] = root;
            rank[root as usize] = left_rank + 1;
        }
    }
}

fn selected_seam(value: &group::T, index_view: &topology_index::View::View, edge: i64) -> bool {
    let first = index_view.edge_offsets[edge as usize];
    let last = index_view.edge_offsets[edge as usize + 1];
    let mut selected = false;
    for local in first..last {
        if group::mem(index_view.edge_vertices[local as usize], value) {
            selected = true;
        }
    }
    selected
}

fn edge_endpoint_corners(
    topology_view: &topology::View::View,
    index_view: &topology_index::View::View,
    edge: i64,
    local: i64,
) -> Result<(i64, i64), error::T> {
    let corner = index_view.edge_vertices[(index_view.edge_offsets[edge as usize] + local) as usize];
    let next = index_view.next_vertex[corner as usize];
    if next < 0 {
        return Err(fail("UV parameterization requires polygon edges"));
    }
    let a = topology_view.vertex_points[corner as usize];
    let b = topology_view.vertex_points[next as usize];
    let edge_a = index_view.edge_a[edge as usize];
    if a == edge_a {
        Ok((corner, next))
    } else if b == edge_a {
        Ok((next, corner))
    } else {
        Err(fail("UV parameterization encountered inconsistent reverse topology"))
    }
}

fn edge_corner_variables(
    topology_view: &topology::View::View,
    index_view: &topology_index::View::View,
    corner_to_variable: &[i64],
    edge: i64,
    local: i64,
) -> Result<(i64, i64), error::T> {
    let (a, b) = edge_endpoint_corners(topology_view, index_view, edge, local)?;
    Ok((corner_to_variable[a as usize], corner_to_variable[b as usize]))
}

/// One float per index in `0..count`, computed in ranges of `grain` and joined in range order.
fn pmap(
    grain: i64,
    count: i64,
    value: &(dyn Fn(i64) -> Result<f64, error::T> + Sync),
) -> Result<Vec<f64>, error::T> {
    if count <= 0 {
        return Ok(Vec::new());
    }
    let ranges = ((count - 1) / grain + 1) as usize;
    let slots: Vec<Mutex<Vec<f64>>> = (0..ranges).map(|_| Mutex::new(Vec::new())).collect();
    support::run_indexed_ranges(grain, count, &|range, first, last| {
        let mut local = Vec::with_capacity((last - first) as usize);
        for index in first..last {
            local.push(value(index)?);
        }
        *slots[range as usize].lock().unwrap_or_else(std::sync::PoisonError::into_inner) = local;
        Ok(())
    })?;
    let mut output = Vec::with_capacity(count as usize);
    for slot in slots {
        output.extend(slot.into_inner().unwrap_or_else(std::sync::PoisonError::into_inner));
    }
    Ok(output)
}

/// Range partial sums added in range order, so the result depends on `grain` but not on threads.
fn parallel_dot(grain: i64, count: i64, left: &[f64], right: &[f64]) -> Result<f64, error::T> {
    if count <= 0 {
        return Ok(0.0);
    }
    let ranges = ((count - 1) / grain + 1) as usize;
    let sums: Vec<Mutex<f64>> = (0..ranges).map(|_| Mutex::new(0.0)).collect();
    support::run_indexed_ranges(grain, count, &|range, first, last| {
        let mut sum = 0.0;
        for variable in first..last {
            sum += left[variable as usize] * right[variable as usize];
        }
        *sums[range as usize].lock().unwrap_or_else(std::sync::PoisonError::into_inner) = sum;
        Ok(())
    })?;
    let mut sum = 0.0;
    for part in sums {
        sum += part.into_inner().unwrap_or_else(std::sync::PoisonError::into_inner);
    }
    Ok(sum)
}

/// The assembled mean-value system over chart variables.
struct System {
    grain: i64,
    variable_count: i64,
    boundary_degree: Vec<u8>,
    adjacency_offsets: Vec<i64>,
    adjacency: Vec<i64>,
    weights: Vec<f64>,
    diagonal: Vec<f64>,
}

impl System {
    fn apply(&self, source: &[f64]) -> Result<Vec<f64>, error::T> {
        pmap(self.grain, self.variable_count, &|variable| {
            let v = variable as usize;
            if self.boundary_degree[v] != 0 {
                Ok(source[v])
            } else {
                let mut value = self.diagonal[v] * source[v];
                for slot in self.adjacency_offsets[v]..self.adjacency_offsets[v + 1] {
                    let neighbor = self.adjacency[slot as usize] as usize;
                    if self.boundary_degree[neighbor] == 0 {
                        value -= self.weights[slot as usize] * source[neighbor];
                    }
                }
                Ok(value)
            }
        })
    }

    fn dot(&self, left: &[f64], right: &[f64]) -> Result<f64, error::T> {
        parallel_dot(self.grain, self.variable_count, left, right)
    }

    fn solve_coordinate(
        &self,
        cancel_value: Option<&cancel::T>,
        iterations: i64,
        tolerance: f64,
        fixed: &[f64],
    ) -> Result<Vec<f64>, error::T> {
        let count = self.variable_count;
        let mut solution: Vec<f64> = fixed.to_vec();
        for variable in 0..count as usize {
            if self.boundary_degree[variable] != 0 {
                solution[variable] = 0.0;
            }
        }
        let rhs = pmap(self.grain, count, &|variable| {
            let v = variable as usize;
            if self.boundary_degree[v] != 0 {
                Ok(0.0)
            } else {
                let mut value = 0.0;
                for slot in self.adjacency_offsets[v]..self.adjacency_offsets[v + 1] {
                    let neighbor = self.adjacency[slot as usize] as usize;
                    if self.boundary_degree[neighbor] != 0 {
                        value += self.weights[slot as usize] * fixed[neighbor];
                    }
                }
                Ok(value)
            }
        })?;
        let applied = self.apply(&solution)?;
        let mut residual = pmap(self.grain, count, &|variable| {
            Ok(rhs[variable as usize] - applied[variable as usize])
        })?;
        let mut preconditioned =
            pmap(self.grain, count, &|variable| Ok(residual[variable as usize] / self.diagonal[variable as usize]))?;
        let mut direction = preconditioned.clone();
        let rhs_norm = self.dot(&rhs, &rhs)?.sqrt();
        let threshold = tolerance * if 1.0 > rhs_norm { 1.0 } else { rhs_norm };
        let mut residual_norm = self.dot(&residual, &residual)?.sqrt();
        let mut rz = self.dot(&residual, &preconditioned)?;
        let mut iteration = 0;
        while iteration < iterations && residual_norm > threshold {
            cancel::check_opt(cancel_value)?;
            let applied = self.apply(&direction)?;
            let denominator = self.dot(&direction, &applied)?;
            if !finite(denominator) || denominator <= 0.0 {
                return Err(fail("UV parameterization solver lost positive definiteness"));
            }
            let alpha = rz / denominator;
            solution = pmap(self.grain, count, &|variable| {
                Ok(solution[variable as usize] + alpha * direction[variable as usize])
            })?;
            residual = pmap(self.grain, count, &|variable| {
                Ok(residual[variable as usize] - alpha * applied[variable as usize])
            })?;
            preconditioned =
                pmap(self.grain, count, &|variable| Ok(residual[variable as usize] / self.diagonal[variable as usize]))?;
            residual_norm = self.dot(&residual, &residual)?.sqrt();
            if residual_norm > threshold {
                let next_rz = self.dot(&residual, &preconditioned)?;
                let beta = next_rz / rz;
                direction = pmap(self.grain, count, &|variable| {
                    Ok(preconditioned[variable as usize] + beta * direction[variable as usize])
                })?;
                rz = next_rz;
            }
            iteration += 1;
        }
        if residual_norm > threshold {
            return Err(fail(&format!("UV parameterization did not converge in {iterations} iterations")));
        }
        for variable in 0..count as usize {
            if self.boundary_degree[variable] != 0 {
                solution[variable] = fixed[variable];
            }
        }
        Ok(solution)
    }
}

fn solve_checked(
    params: &Params,
    boundary_mode: BoundaryMode,
    value: &geometry::T,
) -> Result<geometry::T, error::T> {
    let cancel_value = params.cancel.as_deref();
    let grain = params.grain.unwrap_or(16_384);
    let name = params.name.as_str();
    let (iterations, tolerance, uv_tolerance) = (params.iterations, params.tolerance, params.uv_tolerance);
    if grain <= 0 {
        return Err(fail("grain must be positive"));
    }
    if name.trim_matches(|c| matches!(c, ' ' | '\t' | '\n' | '\r' | '\x0c')).is_empty() {
        return Err(fail("UV attribute name must not be empty"));
    }
    if iterations <= 0 {
        return Err(fail("iterations must be positive"));
    }
    if !finite(tolerance) || tolerance < 0.0 {
        return Err(fail("solver tolerance must be finite and non-negative"));
    }
    if !finite(uv_tolerance) || uv_tolerance < 0.0 {
        return Err(fail("UV seam tolerance must be finite and non-negative"));
    }
    let topology_value = geometry::topology(value);
    if !topology::all_triangles(topology_value)? {
        return Err(fail("UV parameterization currently requires triangle polygons"));
    }
    let topology_view = topology::View::view(topology_value)?;
    let point_count = topology::point_count(topology_value);
    let vertex_count = topology::vertex_count(topology_value);
    let primitive_count = topology::primitive_count(topology_value);
    if primitive_count == 0 {
        return Err(fail("UV parameterization requires at least one triangle"));
    }
    if let Some(seams) = params.seams.as_ref() {
        if group::owner(seams) != group::Owner::Vertex || group::length(seams) != vertex_count {
            return Err(fail("UV seam group must own one outgoing-edge entry per vertex"));
        }
    }
    let index = topology_index::create(cancel_value, topology_value)?;
    let index_view = topology_index::View::view(&index);
    if let Some(seams) = params.edge_seams.as_ref() {
        if edge_group::topology_data_id(seams) != topology::data_id(topology_value)
            || edge_group::length(seams) != topology_index::edge_count(&index)
        {
            return Err(fail("native UV seam group belongs to a different topology"));
        }
    }
    cancel::check_opt(cancel_value)?;
    let positions = packed::Float3::View::view(geometry::positions(value))?;
    let invalid_position = AtomicBool::new(false);
    support::run_ranges(Some(grain), point_count, &|first, last| {
        for point in first..last {
            let p = point as usize;
            if !(finite(positions.x[p]) && finite(positions.y[p]) && finite(positions.z[p])) {
                invalid_position.store(true, Ordering::Relaxed);
            }
        }
        Ok(())
    })?;
    if invalid_position.load(Ordering::Relaxed) {
        return Err(fail("UV parameterization requires finite point positions"));
    }

    let edge_count = topology_index::edge_count(&index);
    let mut edge_is_seam = vec![false; edge_count as usize];
    for edge in 0..edge_count {
        cancel::poll(cancel_value, edge, 16_383)?;
        let incidence = topology_index::edge_incidence_count(&index, edge)?;
        if incidence > 2 {
            return Err(fail("UV parameterization rejects non-manifold edges"));
        }
        let selected = params.edge_seams.as_ref().is_some_and(|seams| edge_group::mem(edge, seams))
            || params.seams.as_ref().is_some_and(|seams| selected_seam(seams, &index_view, edge));
        if selected {
            edge_is_seam[edge as usize] = true;
        }
    }

    // Corners sharing one point remain one UV variable exactly while their incident faces are
    // connected across non-seam manifold edges.
    let vc = vertex_count as usize;
    let mut corner_parent: Vec<i64> = (0..vertex_count).collect();
    let mut corner_rank = vec![0_u8; vc];
    for edge in 0..edge_count {
        cancel::poll(cancel_value, edge, 16_383)?;
        if topology_index::edge_incidence_count(&index, edge)? == 2 && !edge_is_seam[edge as usize] {
            let (left_a, left_b) = edge_endpoint_corners(&topology_view, &index_view, edge, 0)?;
            let (right_a, right_b) = edge_endpoint_corners(&topology_view, &index_view, edge, 1)?;
            union_roots(&mut corner_parent, &mut corner_rank, left_a, right_a);
            union_roots(&mut corner_parent, &mut corner_rank, left_b, right_b);
        }
    }
    let mut root_to_variable = vec![-1_i64; vc];
    let mut corner_to_variable = vec![-1_i64; vc];
    let mut variable_total = 0_i64;
    for corner in 0..vc {
        let root = find_root(&mut corner_parent, corner as i64) as usize;
        if root_to_variable[root] < 0 {
            root_to_variable[root] = variable_total;
            variable_total += 1;
        }
        corner_to_variable[corner] = root_to_variable[root];
    }
    let variable_count = variable_total;
    let nv = variable_count as usize;
    let mut variable_point = vec![-1_i64; nv];
    for corner in 0..vc {
        let variable = corner_to_variable[corner] as usize;
        let point = topology_view.vertex_points[corner];
        if variable_point[variable] < 0 {
            variable_point[variable] = point;
        } else if variable_point[variable] != point {
            return Err(fail("UV chart merged corners from distinct points"));
        }
    }

    let mut component_parent: Vec<i64> = (0..variable_count).collect();
    let mut component_rank = vec![0_u8; nv];
    for primitive in 0..primitive_count as usize {
        let first = topology_view.primitive_offsets[primitive] as usize;
        let a = corner_to_variable[first];
        let b = corner_to_variable[first + 1];
        let c = corner_to_variable[first + 2];
        if a == b || b == c || c == a {
            return Err(fail("UV parameterization rejects triangles with repeated chart vertices"));
        }
        union_roots(&mut component_parent, &mut component_rank, a, b);
        union_roots(&mut component_parent, &mut component_rank, b, c);
    }
    let mut root_to_component = vec![-1_i64; nv];
    let mut variable_component = vec![-1_i64; nv];
    let mut component_total = 0_i64;
    for variable in 0..nv {
        let root = find_root(&mut component_parent, variable as i64) as usize;
        if root_to_component[root] < 0 {
            root_to_component[root] = component_total;
            component_total += 1;
        }
        variable_component[variable] = root_to_component[root];
    }
    let component_count = component_total;
    let ncomp = component_count as usize;

    let mut boundary_degree = vec![0_u8; nv];
    let mut boundary_first = vec![-1_i64; nv];
    let mut boundary_second = vec![-1_i64; nv];
    let mut add_boundary = |a: i64, b: i64| -> Result<(), error::T> {
        if a == b {
            return Err(fail("UV parameterization encountered a collapsed boundary edge"));
        }
        let mut add = |a: i64, b: i64| -> Result<(), error::T> {
            let au = a as usize;
            let degree = boundary_degree[au];
            if degree == 0 {
                boundary_first[au] = b;
            } else if degree == 1 && boundary_first[au] != b {
                boundary_second[au] = b;
            } else if degree >= 2 || boundary_first[au] == b {
                return Err(fail("UV island boundary is not a simple cycle"));
            }
            boundary_degree[au] = degree + 1;
            Ok(())
        };
        add(a, b)?;
        add(b, a)
    };
    for edge in 0..edge_count {
        let incidence = topology_index::edge_incidence_count(&index, edge)?;
        if incidence == 1 {
            let (a, b) = edge_corner_variables(&topology_view, &index_view, &corner_to_variable, edge, 0)?;
            add_boundary(a, b)?;
        } else if incidence == 2 && edge_is_seam[edge as usize] {
            for local in 0..2 {
                let (a, b) = edge_corner_variables(&topology_view, &index_view, &corner_to_variable, edge, local)?;
                add_boundary(a, b)?;
            }
        }
    }
    for variable in 0..nv {
        let degree = boundary_degree[variable];
        if degree != 0 && degree != 2 {
            return Err(fail("UV island boundary is open or branched"));
        }
    }

    let mut boundary_visited = vec![false; nv];
    let mut component_loop = vec![-1_i64; ncomp];
    let mut loop_start = vec![-1_i64; ncomp];
    let mut loop_count = 0_i64;
    for start in 0..variable_count {
        if boundary_degree[start as usize] != 0 && !boundary_visited[start as usize] {
            let component = variable_component[start as usize] as usize;
            if component_loop[component] >= 0 {
                return Err(fail("UV parameterization currently rejects islands with holes"));
            }
            component_loop[component] = loop_count;
            loop_start[component] = start;
            let (mut previous, mut current, mut count) = (-1_i64, start, 0_i64);
            while current != start || count == 0 {
                if count > variable_count {
                    return Err(fail("UV island boundary traversal did not close"));
                }
                if boundary_visited[current as usize] {
                    return Err(fail("UV island boundary self-intersects topologically"));
                }
                boundary_visited[current as usize] = true;
                count += 1;
                let first = boundary_first[current as usize];
                let second = boundary_second[current as usize];
                let next = if first != previous { first } else { second };
                if next < 0 {
                    return Err(fail("UV island boundary is incomplete"));
                }
                previous = current;
                current = next;
            }
            loop_count += 1;
        }
    }
    for component in 0..ncomp {
        if component_loop[component] < 0 {
            return Err(fail("closed UV island requires at least one seam"));
        }
    }

    let source_uv = match boundary_mode {
        BoundaryMode::Circle => None,
        BoundaryMode::Preserve => match geometry::find_attribute(attribute::Owner::Vertex, name, value) {
            None => {
                return Err(fail(&format!(
                    "UV Relax requires vertex float2 attribute {}",
                    ocaml_quote(name)
                )))
            }
            Some(item) => match attribute::View::storage(&item)? {
                attribute::Storage::Float2(values) => Some(packed::Float2::View::view(values)),
                _ => {
                    return Err(fail(&format!(
                        "UV Relax vertex attribute {} must be float2",
                        ocaml_quote(name)
                    )))
                }
            },
        },
    };
    let mut u = vec![0.0_f64; nv];
    let mut v = vec![0.0_f64; nv];
    if let Some(source) = source_uv.as_ref() {
        let mut assigned = vec![false; nv];
        for corner in 0..vc {
            let (source_u, source_v) = (source.x[corner], source.y[corner]);
            if !(finite(source_u) && finite(source_v)) {
                return Err(fail("UV Relax requires finite input coordinates"));
            }
            let variable = corner_to_variable[corner] as usize;
            if !assigned[variable] {
                assigned[variable] = true;
                u[variable] = source_u;
                v[variable] = source_v;
            } else if (u[variable] - source_u).abs() > uv_tolerance || (v[variable] - source_v).abs() > uv_tolerance
            {
                return Err(fail("UV discontinuity must be declared as a seam before relaxing"));
            }
        }
    }

    let distance = |left: i64, right: i64| -> f64 {
        let (l, r) = (left as usize, right as usize);
        let dx = positions.x[l] - positions.x[r];
        let dy = positions.y[l] - positions.y[r];
        let dz = positions.z[l] - positions.z[r];
        (dx * dx + dy * dy + dz * dz).sqrt()
    };
    if boundary_mode == BoundaryMode::Circle {
        let columns = (component_count as f64).sqrt().ceil() as i64;
        let rows = (component_count + columns - 1) / columns;
        let cell_width = 1.0 / columns as f64;
        let cell_height = 1.0 / rows as f64;
        let radius = 0.45 * if cell_width < cell_height { cell_width } else { cell_height };
        let mut component_center_u = vec![0.0_f64; ncomp];
        let mut component_center_v = vec![0.0_f64; ncomp];
        for component in 0..component_count {
            let start = loop_start[component as usize];
            let mut perimeter = 0.0;
            let (mut previous, mut current, mut count) = (-1_i64, start, 0_i64);
            while current != start || count == 0 {
                let next = if boundary_first[current as usize] != previous {
                    boundary_first[current as usize]
                } else {
                    boundary_second[current as usize]
                };
                perimeter += distance(variable_point[current as usize], variable_point[next as usize]);
                previous = current;
                current = next;
                count += 1;
            }
            if !finite(perimeter) || perimeter <= 0.0 {
                return Err(fail("UV island boundary has zero or non-finite length"));
            }
            let column = component % columns;
            let row = component / columns;
            let center_u = (column as f64 + 0.5) * cell_width;
            let center_v = (row as f64 + 0.5) * cell_height;
            component_center_u[component as usize] = center_u;
            component_center_v[component as usize] = center_v;
            let mut traversed = 0.0;
            let (mut previous, mut current, mut count) = (-1_i64, start, 0_i64);
            while current != start || count == 0 {
                let angle = 2.0 * std::f64::consts::PI * traversed / perimeter;
                u[current as usize] = center_u + radius * angle.cos();
                v[current as usize] = center_v + radius * angle.sin();
                let next = if boundary_first[current as usize] != previous {
                    boundary_first[current as usize]
                } else {
                    boundary_second[current as usize]
                };
                traversed += distance(variable_point[current as usize], variable_point[next as usize]);
                previous = current;
                current = next;
                count += 1;
            }
        }
        for variable in 0..nv {
            if boundary_degree[variable] == 0 {
                let component = variable_component[variable] as usize;
                u[variable] = component_center_u[component];
                v[variable] = component_center_v[component];
            }
        }
    }

    // Each triangle contributes two positive mean-value weights at every corner. Duplicate neighbor
    // slots are intentional and avoid a hash/sort aggregation pass.
    let mut adjacency_offsets = vec![0_i64; nv + 1];
    for corner in 0..vc {
        adjacency_offsets[corner_to_variable[corner] as usize + 1] += 2;
    }
    for variable in 0..nv {
        adjacency_offsets[variable + 1] += adjacency_offsets[variable];
    }
    let adjacency_count = adjacency_offsets[nv] as usize;
    let mut adjacency = vec![0_i64; adjacency_count];
    let mut weights = vec![0.0_f64; adjacency_count];
    let mut cursor = adjacency_offsets.clone();
    let invalid_triangle = AtomicBool::new(false);
    let corner_tangent = pmap(grain, vertex_count, &|corner| {
        // Triangles only: the corner's primitive starts at the nearest multiple of three.
        let first_corner = corner - corner % 3;
        let local = corner - first_corner;
        let corner_index = corner as usize;
        let previous = (first_corner + (local + 2) % 3) as usize;
        let next = (first_corner + (local + 1) % 3) as usize;
        let point = topology_view.vertex_points[corner_index] as usize;
        let previous_point = topology_view.vertex_points[previous] as usize;
        let next_point = topology_view.vertex_points[next] as usize;
        let ax = positions.x[previous_point] - positions.x[point];
        let ay = positions.y[previous_point] - positions.y[point];
        let az = positions.z[previous_point] - positions.z[point];
        let bx = positions.x[next_point] - positions.x[point];
        let by = positions.y[next_point] - positions.y[point];
        let bz = positions.z[next_point] - positions.z[point];
        let aa = ax * ax + ay * ay + az * az;
        let bb = bx * bx + by * by + bz * bz;
        let dot = ax * bx + ay * by + az * bz;
        let cross_x = ay * bz - az * by;
        let cross_y = az * bx - ax * bz;
        let cross_z = ax * by - ay * bx;
        let cross = (cross_x * cross_x + cross_y * cross_y + cross_z * cross_z).sqrt();
        let denominator = (aa * bb).sqrt() + dot;
        if aa <= 0.0 || bb <= 0.0 || cross <= 1e-15 * (aa * bb).sqrt() || denominator <= 0.0 {
            invalid_triangle.store(true, Ordering::Relaxed);
            Ok(0.0)
        } else {
            Ok(cross / denominator)
        }
    })?;
    if invalid_triangle.load(Ordering::Relaxed) {
        return Err(fail("UV parameterization rejects zero-area or numerically degenerate triangles"));
    }
    for primitive in 0..primitive_count {
        cancel::poll(cancel_value, primitive, 4095)?;
        let first = topology_view.primitive_offsets[primitive as usize];
        for local in 0..3 {
            let corner = first + local;
            let next = first + (local + 1) % 3;
            let previous = first + (local + 2) % 3;
            let variable = corner_to_variable[corner as usize] as usize;
            let point = topology_view.vertex_points[corner as usize];
            for neighbor_corner in [previous, next] {
                let slot = cursor[variable] as usize;
                cursor[variable] += 1;
                let neighbor = corner_to_variable[neighbor_corner as usize];
                let edge_length = distance(point, topology_view.vertex_points[neighbor_corner as usize]);
                let weight = corner_tangent[corner as usize] / edge_length;
                if !finite(weight) || weight <= 0.0 {
                    return Err(fail("UV parameterization produced an invalid mean-value weight"));
                }
                adjacency[slot] = neighbor;
                weights[slot] = weight;
            }
        }
    }

    let mut diagonal = vec![1.0_f64; nv];
    for variable in 0..nv {
        if boundary_degree[variable] == 0 {
            let mut sum = 0.0;
            for slot in adjacency_offsets[variable]..adjacency_offsets[variable + 1] {
                sum += weights[slot as usize];
            }
            if sum <= 0.0 || !finite(sum) {
                return Err(fail("UV parameterization encountered an isolated chart vertex"));
            }
            diagonal[variable] = sum;
        }
    }
    let system = System {
        grain,
        variable_count,
        boundary_degree,
        adjacency_offsets,
        adjacency,
        weights,
        diagonal,
    };
    let solved_u = system.solve_coordinate(cancel_value, iterations, tolerance, &u)?;
    let solved_v = system.solve_coordinate(cancel_value, iterations, tolerance, &v)?;
    let mut component_winding = vec![0.0_f64; ncomp];
    for primitive in 0..primitive_count as usize {
        let first = topology_view.primitive_offsets[primitive] as usize;
        let a = corner_to_variable[first] as usize;
        let b = corner_to_variable[first + 1] as usize;
        let c = corner_to_variable[first + 2] as usize;
        let area = (solved_u[b] - solved_u[a]) * (solved_v[c] - solved_v[a])
            - (solved_v[b] - solved_v[a]) * (solved_u[c] - solved_u[a]);
        if !finite(area) || area.abs() <= 1e-15 {
            return Err(fail("UV parameterization collapsed a triangle"));
        }
        let component = variable_component[a] as usize;
        let sign = if area < 0.0 { -1.0 } else { 1.0 };
        if component_winding[component] == 0.0 {
            component_winding[component] = sign;
        } else if component_winding[component] != sign {
            return Err(fail("UV parameterization produced a flipped triangle"));
        }
    }
    let result_u = pmap(grain, vertex_count, &|corner| Ok(solved_u[corner_to_variable[corner as usize] as usize]))?;
    let result_v = pmap(grain, vertex_count, &|corner| Ok(solved_v[corner_to_variable[corner as usize] as usize]))?;
    let values = packed::Float2::of_owned(result_u, result_v)?;
    let key = attribute::key(name, attribute::Owner::Vertex, attribute::float2());
    let item = attribute::create_key_owned(&key, attribute::Storage::Float2(values))?;
    geometry::with_attribute(item, value)
}

/// Solves the chart parameterization; `Circle` maps island boundaries to circles, `Preserve` keeps
/// the existing vertex float2 attribute `name` on boundaries.
pub fn solve(
    params: &Params,
    boundary_mode: BoundaryMode,
    value: &geometry::T,
) -> Result<geometry::T, error::T> {
    solve_checked(params, boundary_mode, value)
}
