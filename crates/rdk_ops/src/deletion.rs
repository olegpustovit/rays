use std::cell::RefCell;

use rdk_core::{
    attribute, cancel, edge_group, error, geometry, group, packed, topology, topology_index,
    topology_remap,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TopologyPolicy {
    DestroyTouchedPrimitives,
    HealPrimitives,
}

#[derive(Clone)]
pub struct Plan {
    pub point_map: Vec<i64>,
    pub vertex_map: Vec<i64>,
    pub primitive_map: Vec<i64>,
    pub topology: topology::T,
    pub healed: bool,
    pub point_identity: bool,
    pub unchanged: bool,
}

fn make(operation: &str, code: &str, message: String) -> error::T {
    error::make(operation.to_owned(), code.to_owned(), message, None).unwrap_or_else(|e| e)
}

// An Error.Fail of the original: guard turns it into the operation's error.
fn fail(message: &str) -> error::T {
    make("Rdk.Deletion", "fail", format!("Rdk.Deletion.delete: {message}"))
}

fn guard(operation: &str, code: &str, result: Result<geometry::T, error::T>) -> Result<geometry::T, error::T> {
    result.map_err(|failure| {
        if error::code(&failure) == "cancelled" {
            make(operation, "cancelled", "geometry operation was cancelled".to_owned())
        } else {
            make(operation, code, error::message(&failure).to_owned())
        }
    })
}

fn minimum_vertices(kind: u8) -> i64 {
    match kind {
        1 => 2,
        _ => 3,
    }
}

fn is_selected(selection: &group::T, delete_selected: bool, index: i64) -> bool {
    group::mem(index, selection) == delete_selected
}

fn validate_selection(selection: &group::T, geometry_value: &geometry::T) -> Result<(), error::T> {
    let (expected, name) = match group::owner(selection) {
        group::Owner::Point => (geometry::point_count(geometry_value), "point"),
        group::Owner::Vertex => (geometry::vertex_count(geometry_value), "vertex"),
        group::Owner::Primitive => (geometry::primitive_count(geometry_value), "primitive"),
    };
    if group::length(selection) != expected {
        return Err(fail(&format!(
            "selection length {} does not match its {} owner count {}",
            group::length(selection),
            name,
            expected
        )));
    }
    Ok(())
}

fn corner_deleted(
    owner: group::Owner,
    selection: &group::T,
    delete_selected: bool,
    view: &topology::View::View,
    vertex: i64,
) -> bool {
    match owner {
        group::Owner::Point => {
            is_selected(selection, delete_selected, view.vertex_points[vertex as usize])
        }
        group::Owner::Vertex => is_selected(selection, delete_selected, vertex),
        group::Owner::Primitive => false,
    }
}

fn primitive_retained_count(
    owner: group::Owner,
    selection: &group::T,
    delete_selected: bool,
    policy: TopologyPolicy,
    view: &topology::View::View,
    primitive: usize,
) -> i64 {
    let first = view.primitive_offsets[primitive];
    let last = view.primitive_offsets[primitive + 1];
    if owner == group::Owner::Primitive {
        if is_selected(selection, delete_selected, primitive as i64) { 0 } else { last - first }
    } else {
        match policy {
            TopologyPolicy::DestroyTouchedPrimitives => {
                let touched = (first..last)
                    .any(|vertex| corner_deleted(owner, selection, delete_selected, view, vertex));
                if touched { 0 } else { last - first }
            }
            TopologyPolicy::HealPrimitives => {
                let count = (first..last)
                    .filter(|&vertex| !corner_deleted(owner, selection, delete_selected, view, vertex))
                    .count() as i64;
                if count < minimum_vertices(view.primitive_kinds[primitive]) { 0 } else { count }
            }
        }
    }
}

pub fn build_plan(
    cancel_value: Option<&cancel::T>,
    delete_selected: bool,
    compact_points: bool,
    policy: TopologyPolicy,
    selection: &group::T,
    source: &geometry::T,
) -> Result<Plan, error::T> {
    let view = topology::View::view(geometry::topology(source))?;
    let owner = group::owner(selection);
    let primitive_count = view.primitive_kinds.len();
    let mut retained_counts = vec![0i64; primitive_count];
    let mut retained_primitives = 0usize;
    let mut retained_vertices = 0i64;
    let mut healed = false;
    for primitive in 0..primitive_count {
        cancel::poll(cancel_value, primitive as i64, 1023)?;
        let count =
            primitive_retained_count(owner, selection, delete_selected, policy, &view, primitive);
        let source_count = view.primitive_offsets[primitive + 1] - view.primitive_offsets[primitive];
        retained_counts[primitive] = count;
        if count > 0 {
            retained_primitives += 1;
            if retained_vertices > i64::MAX - count {
                return Err(fail("vertex count exceeds integer limits"));
            }
            retained_vertices += count;
            if count != source_count {
                healed = true;
            }
        }
    }
    let mut vertex_map = vec![0i64; retained_vertices as usize];
    let mut primitive_map = vec![0i64; retained_primitives];
    let mut primitive_offsets = vec![0i64; retained_primitives + 1];
    let mut primitive_kinds = vec![0u8; retained_primitives];
    let mut vertex_at = 0usize;
    let mut primitive_at = 0usize;
    for primitive in 0..primitive_count {
        cancel::poll(cancel_value, primitive as i64, 1023)?;
        if retained_counts[primitive] > 0 {
            let first = view.primitive_offsets[primitive];
            let last = view.primitive_offsets[primitive + 1];
            primitive_map[primitive_at] = primitive as i64;
            primitive_kinds[primitive_at] = view.primitive_kinds[primitive];
            for vertex in first..last {
                if owner == group::Owner::Primitive
                    || policy == TopologyPolicy::DestroyTouchedPrimitives
                    || !corner_deleted(owner, selection, delete_selected, &view, vertex)
                {
                    vertex_map[vertex_at] = vertex;
                    vertex_at += 1;
                }
            }
            primitive_at += 1;
            primitive_offsets[primitive_at] = vertex_at as i64;
        }
    }
    if vertex_at as i64 != retained_vertices {
        return Err(fail("internal vertex cardinality mismatch"));
    }
    let source_points = view.point_count as usize;
    let mut used: Option<Vec<bool>> = if compact_points { Some(vec![false; source_points]) } else { None };
    if let Some(used) = used.as_mut() {
        for (index, &vertex) in vertex_map.iter().enumerate() {
            cancel::poll(cancel_value, index as i64, 16_383)?;
            used[view.vertex_points[vertex as usize] as usize] = true;
        }
    }
    let mut retained_points = 0i64;
    let mut old_to_new_point = vec![-1i64; source_points];
    for point in 0..source_points {
        cancel::poll(cancel_value, point as i64, 16_383)?;
        let survives = owner != group::Owner::Point
            || !is_selected(selection, delete_selected, point as i64);
        let retain = survives && used.as_ref().map_or(true, |used| used[point]);
        if retain {
            old_to_new_point[point] = retained_points;
            retained_points += 1;
        }
    }
    let mut point_map = Vec::with_capacity(retained_points as usize);
    for point in 0..source_points {
        if old_to_new_point[point] >= 0 {
            point_map.push(point as i64);
        }
    }
    let mut vertex_points = vec![0i64; retained_vertices as usize];
    for vertex in 0..retained_vertices as usize {
        cancel::poll(cancel_value, vertex as i64, 16_383)?;
        let old_point = view.vertex_points[vertex_map[vertex] as usize];
        let point = old_to_new_point[old_point as usize];
        if point < 0 {
            return Err(fail("retained topology references a deleted point"));
        }
        vertex_points[vertex] = point;
    }
    let output_topology = topology::View::create_validated_owned(
        retained_points,
        vertex_points,
        primitive_offsets,
        primitive_kinds,
    );
    let point_identity = point_map.len() == source_points;
    let unchanged = point_identity
        && vertex_map.len() == view.vertex_points.len()
        && primitive_map.len() == primitive_count;
    Ok(Plan {
        point_map,
        vertex_map,
        primitive_map,
        topology: output_topology,
        healed,
        point_identity,
        unchanged,
    })
}

pub fn remap_attribute(
    cancel_value: Option<&cancel::T>,
    grain: Option<i64>,
    plan: &Plan,
    item: &attribute::T,
) -> Result<Option<attribute::T>, error::T> {
    let grain = grain.unwrap_or(16_384);
    let owner = attribute::owner(item);
    if plan.healed
        && attribute::name(item) == "N"
        && (owner == attribute::Owner::Point || owner == attribute::Owner::Vertex)
    {
        return Ok(None);
    }
    if owner == attribute::Owner::Point && plan.point_identity {
        return Ok(Some(item.clone()));
    }
    let mapping = match owner {
        attribute::Owner::Point => &plan.point_map,
        attribute::Owner::Vertex => &plan.vertex_map,
        attribute::Owner::Primitive => &plan.primitive_map,
        attribute::Owner::Detail => return Ok(Some(item.clone())),
    };
    topology_remap::attribute(cancel_value, grain, None, mapping, item).map(Some)
}

pub fn remap_group(
    cancel_value: Option<&cancel::T>,
    grain: Option<i64>,
    plan: &Plan,
    item: &group::T,
) -> Result<group::T, error::T> {
    let grain = grain.unwrap_or(16_384);
    if group::owner(item) == group::Owner::Point && plan.point_identity {
        return Ok(item.clone());
    }
    let mapping = match group::owner(item) {
        group::Owner::Point => &plan.point_map,
        group::Owner::Vertex => &plan.vertex_map,
        group::Owner::Primitive => &plan.primitive_map,
    };
    topology_remap::group(cancel_value, grain, mapping, item)
}

pub fn materialize_plan(
    cancel_value: Option<&cancel::T>,
    grain: Option<i64>,
    remap_edges: bool,
    plan: &Plan,
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    if plan.unchanged {
        return Ok(geometry_value.clone());
    }
    let positions = if plan.point_identity {
        geometry::positions(geometry_value).clone()
    } else {
        let source = packed::Float3::View::view(geometry::positions(geometry_value))?;
        let grain = grain.unwrap_or(16_384);
        packed::Float3::View::of_owned_exn(
            topology_remap::select_float(cancel_value, grain, None, &plan.point_map, &source.x)?,
            topology_remap::select_float(cancel_value, grain, None, &plan.point_map, &source.y)?,
            topology_remap::select_float(cancel_value, grain, None, &plan.point_map, &source.z)?,
        )?
    };
    let mut attributes = Vec::new();
    for item in geometry::attributes(geometry_value) {
        if let Some(remapped) = remap_attribute(cancel_value, grain, plan, &item)? {
            attributes.push(remapped);
        }
    }
    let mut groups = Vec::new();
    for item in geometry::groups(geometry_value) {
        groups.push(remap_group(cancel_value, grain, plan, &item)?);
    }
    let mut edge_groups = Vec::new();
    if remap_edges {
        let source_groups = geometry::edge_groups(geometry_value);
        if !source_groups.is_empty() {
            let mut point_map = vec![-1i64; geometry::point_count(geometry_value) as usize];
            for (output, &source) in plan.point_map.iter().enumerate() {
                point_map[source as usize] = output as i64;
            }
            edge_groups = topology_remap::edge_groups(
                cancel_value,
                geometry::topology(geometry_value),
                &plan.topology,
                &point_map,
                &source_groups,
            )?;
        }
    }
    geometry::create(positions, plan.topology.clone(), Some(attributes), Some(groups), Some(edge_groups))
}

fn lower_bound(values: &[i64], needle: i64) -> usize {
    let mut first = 0usize;
    let mut last = values.len();
    while first < last {
        let middle = first + ((last - first) >> 1);
        if values[middle] < needle {
            first = middle + 1;
        } else {
            last = middle;
        }
    }
    first
}

fn members(value: &edge_group::T) -> Vec<i64> {
    let output = RefCell::new(Vec::new());
    edge_group::iter(&|edge| output.borrow_mut().push(edge), value);
    output.into_inner()
}

fn invalid_argument(message: &str) -> error::T {
    make("Rdk.Deletion", "invalid_argument", message.to_owned())
}

pub fn primitive_partitions(
    piece_count: i64,
    primitive_pieces: &[i64],
    geometry_value: &geometry::T,
) -> Result<Vec<geometry::T>, error::T> {
    let cancel_value: Option<&cancel::T> = None;
    let grain: Option<i64> = None;
    if piece_count < 0 {
        return Err(invalid_argument("Rdk.Deletion.primitive_partitions: negative piece count"));
    }
    let view = topology::View::view(geometry::topology(geometry_value))?;
    let source_points = view.point_count as usize;
    let source_primitives = view.primitive_kinds.len();
    if primitive_pieces.len() != source_primitives {
        return Err(invalid_argument(
            "Rdk.Deletion.primitive_partitions: primitive assignment length mismatch",
        ));
    }
    let pieces = piece_count as usize;
    let valid_piece = |piece: i64| piece >= 0 && piece < piece_count;
    let mut primitive_counts = vec![0usize; pieces];
    let mut vertex_counts = vec![0usize; pieces];
    for (primitive, &piece) in primitive_pieces.iter().enumerate() {
        cancel::poll(cancel_value, primitive as i64, 4095)?;
        if piece < -1 || piece >= piece_count {
            return Err(fail(&format!(
                "primitive {primitive} has invalid piece index {piece} for {piece_count} pieces"
            )));
        }
        if valid_piece(piece) {
            let piece = piece as usize;
            primitive_counts[piece] += 1;
            vertex_counts[piece] +=
                (view.primitive_offsets[primitive + 1] - view.primitive_offsets[primitive]) as usize;
        }
    }
    let mut primitive_maps: Vec<Vec<i64>> = primitive_counts.iter().map(|&n| Vec::with_capacity(n)).collect();
    let mut vertex_maps: Vec<Vec<i64>> = vertex_counts.iter().map(|&n| Vec::with_capacity(n)).collect();
    for (primitive, &piece) in primitive_pieces.iter().enumerate() {
        if valid_piece(piece) {
            let piece = piece as usize;
            primitive_maps[piece].push(primitive as i64);
            for vertex in view.primitive_offsets[primitive]..view.primitive_offsets[primitive + 1] {
                vertex_maps[piece].push(vertex);
            }
        }
    }
    let point_maps: Vec<Vec<i64>> = vertex_maps
        .iter()
        .map(|vertices| {
            let mut candidates: Vec<i64> =
                vertices.iter().map(|&vertex| view.vertex_points[vertex as usize]).collect();
            candidates.sort_unstable();
            candidates.dedup();
            candidates
        })
        .collect();
    let mut geometries = Vec::with_capacity(pieces);
    for piece in 0..pieces {
        cancel::check_opt(cancel_value)?;
        let point_map = &point_maps[piece];
        let vertex_map = &vertex_maps[piece];
        let primitive_map = &primitive_maps[piece];
        let mut primitive_offsets = vec![0i64; primitive_map.len() + 1];
        let mut primitive_kinds = vec![0u8; primitive_map.len()];
        for (output, &source) in primitive_map.iter().enumerate() {
            let source = source as usize;
            primitive_offsets[output + 1] = primitive_offsets[output]
                + view.primitive_offsets[source + 1]
                - view.primitive_offsets[source];
            primitive_kinds[output] = view.primitive_kinds[source];
        }
        let mut vertex_points = vec![0i64; vertex_map.len()];
        for (output, &source_vertex) in vertex_map.iter().enumerate() {
            let source_point = view.vertex_points[source_vertex as usize];
            let point = lower_bound(point_map, source_point);
            if point >= point_map.len() || point_map[point] != source_point {
                return Err(fail("piece primitive references a point outside its point partition"));
            }
            vertex_points[output] = point as i64;
        }
        let output_topology = topology::View::create_validated_owned(
            point_map.len() as i64,
            vertex_points,
            primitive_offsets,
            primitive_kinds,
        );
        let point_identity = point_map.len() == source_points;
        let unchanged = point_identity
            && vertex_map.len() == view.vertex_points.len()
            && primitive_map.len() == source_primitives;
        let plan = Plan {
            point_map: point_map.clone(),
            vertex_map: vertex_map.clone(),
            primitive_map: primitive_map.clone(),
            topology: output_topology,
            healed: false,
            point_identity,
            unchanged,
        };
        geometries.push(materialize_plan(cancel_value, grain, false, &plan, geometry_value)?);
    }
    let source_groups = geometry::edge_groups(geometry_value);
    if source_groups.is_empty() {
        return Ok(geometries);
    }
    let mut point_piece_counts = vec![0usize; source_points];
    for points in &point_maps {
        for &point in points {
            point_piece_counts[point as usize] += 1;
        }
    }
    let mut point_piece_offsets = vec![0usize; source_points + 1];
    for point in 0..source_points {
        point_piece_offsets[point + 1] = point_piece_offsets[point] + point_piece_counts[point];
    }
    let association_count = point_piece_offsets[source_points];
    let mut point_piece_ids = vec![0usize; association_count];
    let mut point_local_ids = vec![0i64; association_count];
    let mut next = point_piece_offsets.clone();
    for (piece, points) in point_maps.iter().enumerate() {
        for (local, &point) in points.iter().enumerate() {
            let output = next[point as usize];
            point_piece_ids[output] = piece;
            point_local_ids[output] = local as i64;
            next[point as usize] = output + 1;
        }
    }
    let source_index = topology_index::create(cancel_value, geometry::topology(geometry_value))?;
    let mut target_indexes = Vec::with_capacity(pieces);
    for item in &geometries {
        target_indexes.push(topology_index::create(cancel_value, geometry::topology(item))?);
    }
    let mut outputs = geometries.clone();
    for source_group in &source_groups {
        let mut builders = Vec::with_capacity(pieces);
        for piece in 0..pieces {
            builders.push(edge_group::Builder::create(
                geometry::topology(&geometries[piece]),
                &target_indexes[piece],
                edge_group::name(source_group),
            )?);
        }
        for edge in members(source_group) {
            let (a, b) = topology_index::edge_points(&source_index, edge)?;
            let (a, b) = (a as usize, b as usize);
            let mut a_at = point_piece_offsets[a];
            let mut b_at = point_piece_offsets[b];
            let a_last = point_piece_offsets[a + 1];
            let b_last = point_piece_offsets[b + 1];
            while a_at < a_last && b_at < b_last {
                let a_piece = point_piece_ids[a_at];
                let b_piece = point_piece_ids[b_at];
                if a_piece < b_piece {
                    a_at += 1;
                } else if b_piece < a_piece {
                    b_at += 1;
                } else {
                    if let Some(target) = topology_index::find_edge(
                        &target_indexes[a_piece],
                        point_local_ids[a_at],
                        point_local_ids[b_at],
                    ) {
                        edge_group::Builder::set(&mut builders[a_piece], target, true)?;
                    }
                    a_at += 1;
                    b_at += 1;
                }
            }
        }
        for (piece, builder) in builders.iter_mut().enumerate() {
            let frozen = edge_group::Builder::freeze(builder)?;
            outputs[piece] = geometry::with_edge_group(frozen, &outputs[piece])?;
        }
    }
    Ok(outputs)
}

pub struct DeleteParams {
    pub cancel: Option<std::sync::Arc<cancel::T>>,
    pub grain: Option<i64>,
    pub selected: bool,
    pub compact_points: bool,
}

pub fn delete(
    params: &DeleteParams,
    selection: &group::T,
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    let cancel_value = params.cancel.as_deref();
    let policy = TopologyPolicy::DestroyTouchedPrimitives;
    guard("delete", "invalid_selection", (|| {
        cancel::check_opt(cancel_value)?;
        validate_selection(selection, geometry_value)?;
        let deletes_none = if params.selected {
            group::cardinality(selection) == 0
        } else {
            group::cardinality(selection) == group::length(selection)
        };
        if deletes_none && !params.compact_points {
            return Ok(geometry_value.clone());
        }
        let plan = build_plan(
            cancel_value, params.selected, params.compact_points, policy, selection, geometry_value,
        )?;
        if plan.unchanged {
            Ok(geometry_value.clone())
        } else {
            materialize_plan(cancel_value, params.grain, true, &plan, geometry_value)
        }
    })())
}

pub struct DeletePrimitivesParams {
    pub cancel: Option<std::sync::Arc<cancel::T>>,
    pub grain: Option<i64>,
    pub compact_points: bool,
}

pub fn delete_primitives(
    params: DeletePrimitivesParams,
    selection: &group::T,
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    if group::owner(selection) != group::Owner::Primitive {
        return Err(make(
            "delete_primitives",
            "invalid_selection",
            "selection must own primitives".to_owned(),
        ));
    }
    delete(
        &DeleteParams {
            cancel: params.cancel,
            grain: params.grain,
            selected: true,
            compact_points: params.compact_points,
        },
        selection,
        geometry_value,
    )
    .map_err(|failure| {
        make("delete_primitives", error::code(&failure), error::message(&failure).to_owned())
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;
    use crate::compact_points::fixtures::*;
    use rdk_core::attribute::{Owner, Storage};

    fn delete_params(selected: bool, compact_points: bool) -> DeleteParams {
        DeleteParams { cancel: None, grain: Some(1), selected, compact_points }
    }

    fn payload() -> geometry::T {
        let g = grid(2, 1, 2.0, true);
        let g = with_attr(&g, Owner::Point, "pid", Storage::Int(Arc::new((0..6).collect())));
        let g = with_attr(&g, Owner::Vertex, "vid", Storage::Int(Arc::new((10..18).collect())));
        let g = with_attr(&g, Owner::Primitive, "fid", Storage::Int(Arc::new(vec![20, 21])));
        with_attr(&g, Owner::Primitive, "label", Storage::Text(vec![b"a".to_vec(), b"b".to_vec()]))
    }

    #[test]
    fn deletes_primitives_and_keeps_ancestry() {
        let g = payload();
        let first = group::init(None, group::Owner::Primitive, "first", 2, &|p| p == 0).unwrap();
        let out = delete(&delete_params(true, false), &first, &g).unwrap();
        assert_eq!(geometry::primitive_count(&out), 1);
        assert_eq!(geometry::point_count(&out), 6);
        assert_eq!(ints(Owner::Primitive, "fid", &out), vec![21]);
        assert_eq!(ints(Owner::Vertex, "vid", &out), vec![14, 15, 16, 17]);
        let compacted = delete(&delete_params(true, true), &first, &g).unwrap();
        assert_eq!(geometry::point_count(&compacted), 4);
        assert_eq!(ints(Owner::Point, "pid", &compacted), vec![1, 2, 4, 5]);
        // Deleting the complement keeps only the selected primitive.
        let kept = delete(&delete_params(false, false), &first, &g).unwrap();
        assert_eq!(ints(Owner::Primitive, "fid", &kept), vec![20]);
        // An empty selection is a no-op that returns the same geometry.
        let none = group::init(None, group::Owner::Primitive, "none", 2, &|_| false).unwrap();
        let same = delete(&delete_params(true, false), &none, &g).unwrap();
        assert_eq!(geometry::data_id(&same), geometry::data_id(&g));
    }

    #[test]
    fn point_selection_destroys_touched_primitives() {
        let g = payload();
        let corner = group::init(None, group::Owner::Point, "corner", 6, &|p| p == 0).unwrap();
        let out = delete(&delete_params(true, false), &corner, &g).unwrap();
        assert_eq!(geometry::primitive_count(&out), 1);
        assert_eq!(geometry::point_count(&out), 5);
        assert_eq!(ints(Owner::Primitive, "fid", &out), vec![21]);
        assert_eq!(topo(&out).0, vec![0, 1, 4, 3]);
    }

    #[test]
    fn invalid_selections_are_diagnosed() {
        let g = payload();
        let wrong_length = group::init(None, group::Owner::Primitive, "bad", 3, &|_| true).unwrap();
        let failure = err(delete(&delete_params(true, false), &wrong_length, &g));
        assert_eq!(error::code(&failure), "invalid_selection");
        assert_eq!(
            error::message(&failure),
            "Rdk.Deletion.delete: selection length 3 does not match its primitive owner count 2"
        );
        let points = group::init(None, group::Owner::Point, "p", 6, &|_| true).unwrap();
        let failure = err(delete_primitives(
            DeletePrimitivesParams { cancel: None, grain: None, compact_points: false },
            &points,
            &g,
        ));
        assert_eq!(error::code(&failure), "invalid_selection");
        assert_eq!(error::operation(&failure), "delete_primitives");
        let token = std::sync::Arc::new(cancel::create());
        cancel::cancel(&token);
        let first = group::init(None, group::Owner::Primitive, "first", 2, &|p| p == 0).unwrap();
        let failure = err(delete(
            &DeleteParams { cancel: Some(token), grain: Some(1), selected: true, compact_points: false },
            &first,
            &g,
        ));
        assert_eq!(error::code(&failure), "cancelled");
    }

    #[test]
    fn partitions_split_by_piece() {
        let g = payload();
        let pieces = primitive_partitions(2, &[1, 0], &g).unwrap();
        assert_eq!(pieces.len(), 2);
        assert_eq!(ints(Owner::Primitive, "fid", &pieces[0]), vec![21]);
        assert_eq!(ints(Owner::Primitive, "fid", &pieces[1]), vec![20]);
        assert_eq!(geometry::point_count(&pieces[0]), 4);
        assert_eq!(ints(Owner::Point, "pid", &pieces[1]), vec![0, 1, 3, 4]);
        // A primitive of piece -1 is dropped; invalid indices are reported.
        let dropped = primitive_partitions(1, &[-1, 0], &g).unwrap();
        assert_eq!(ints(Owner::Primitive, "fid", &dropped[0]), vec![21]);
        assert!(primitive_partitions(1, &[0, 5], &g).is_err());
        assert!(primitive_partitions(1, &[0], &g).is_err());
    }

    #[test]
    fn partitions_carry_edge_groups() {
        let g = payload();
        let shared = edge_group_pairs(geometry::topology(&g), "shared", &[(1, 4), (0, 1)]);
        let g = geometry::with_edge_group(shared, &g).unwrap();
        let pieces = primitive_partitions(2, &[0, 1], &g).unwrap();
        let left = geometry::find_edge_group("shared", &pieces[0]).unwrap();
        let right = geometry::find_edge_group("shared", &pieces[1]).unwrap();
        assert_eq!(edge_group::cardinality(&left), 2);
        assert_eq!(edge_group::cardinality(&right), 1);
    }
}
