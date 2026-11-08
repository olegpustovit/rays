use rdk_core::support::Windows;
use rdk_core::{
    cancel, error, geometry, group, packed, polygon_triangulation, support, topology, topology_remap,
};
use rdk_core::error::Code;
use rdk_geom::point_grid::float_max;
use std::sync::atomic::{AtomicBool, AtomicI64, Ordering};

const PREFIX: &str = "Rdk_mesh.Triangulate.triangulate: ";

pub struct Params {
    pub cancel: Option<std::sync::Arc<cancel::T>>,
    pub grain: Option<i64>,
    pub primitives: Option<group::T>,
}

fn make(operation: &str, code: Code, message: String) -> error::T {
    error::new(operation, code, message)
}

fn fail(message: String) -> error::T {
    make("triangulate", Code::Fail, message)
}

// Which corner of a quad with exactly one collapsed edge the single triangle drops, or -1.
fn collapsed_quad_removed(
    positions: &packed::Float3::View::View,
    view: &topology::View::View,
    primitive: usize,
) -> i64 {
    let first = view.primitive_offsets[primitive];
    let last = view.primitive_offsets[primitive + 1];
    if last - first != 4 {
        return -1;
    }
    let corner = |local: i64| view.vertex_points[(first + local) as usize] as usize;
    let mut removed = -1i64;
    let mut collapsed = 0;
    for local in 0..4i64 {
        let next = (local + 1) & 3;
        let a = corner(local);
        let b = corner(next);
        if positions.x[a] == positions.x[b] && positions.y[a] == positions.y[b] && positions.z[a] == positions.z[b] {
            collapsed += 1;
            removed = next;
        }
    }
    if collapsed != 1 {
        return -1;
    }
    let remaining = |ordinal: i64| if ordinal < removed { ordinal } else { ordinal + 1 };
    let a = corner(remaining(0));
    let b = corner(remaining(1));
    let c = corner(remaining(2));
    let ux = positions.x[b] - positions.x[a];
    let uy = positions.y[b] - positions.y[a];
    let uz = positions.z[b] - positions.z[a];
    let vx = positions.x[c] - positions.x[a];
    let vy = positions.y[c] - positions.y[a];
    let vz = positions.z[c] - positions.z[a];
    let mut scale = float_max(ux.abs(), uy.abs());
    scale = float_max(scale, uz.abs());
    scale = float_max(scale, vx.abs());
    scale = float_max(scale, vy.abs());
    scale = float_max(scale, vz.abs());
    if scale == 0.0 || !scale.is_finite() {
        return -1;
    }
    let (ux, uy, uz, vx, vy, vz) = (ux / scale, uy / scale, uz / scale, vx / scale, vy / scale, vz / scale);
    let nx = (uy * vz) - (uz * vy);
    let ny = (uz * vx) - (ux * vz);
    let nz = (ux * vy) - (uy * vx);
    if nx == 0.0 && ny == 0.0 && nz == 0.0 { -1 } else { removed }
}

fn triangulate(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    primitives: Option<&group::T>,
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    if grain <= 0 {
        return Err(fail("Rdk_mesh.Triangulate.triangulate: grain must be positive".to_owned()));
    }
    cancel::check_opt(cancel_value)?;
    let topology_value = geometry::topology(geometry_value);
    let primitive_count = topology::primitive_count(topology_value) as usize;
    if let Some(selection) = primitives {
        if group::owner(selection) != group::Owner::Primitive {
            return Err(fail("Rdk_mesh.Triangulate.triangulate: selection must own primitives".to_owned()));
        }
        if group::length(selection) != primitive_count as i64 {
            return Err(fail(
                "Rdk_mesh.Triangulate.triangulate: selection length does not match primitive count".to_owned(),
            ));
        }
    }
    let positions = packed::Float3::View::view(geometry::positions(geometry_value))?;
    let view = topology::View::view(topology_value)?;
    let selected = |primitive: usize| match primitives {
        None => true,
        Some(selection) => group::mem(primitive as i64, selection),
    };
    let full_selection = match primitives {
        None => true,
        Some(selection) => group::cardinality(selection) == primitive_count as i64,
    };
    let mut collapsed_removals = vec![0u8; primitive_count];
    let mut primitive_bases = vec![1i64; primitive_count + 1];
    primitive_bases[0] = 0;
    let mut vertex_bases = vec![0i64; if full_selection { 0 } else { primitive_count + 1 }];
    let first_selected_curve = AtomicI64::new(i64::MAX);
    let changes = AtomicBool::new(false);
    let average_size = if primitive_count == 0 {
        1
    } else {
        (topology::vertex_count(topology_value) / primitive_count as i64).max(1)
    };
    let primitive_grain = (grain / average_size).max(1);
    {
        // Entry `primitive + 1` of the base arrays belongs to `primitive`; vertex bases only exist unless full.
        let ranges = support::range_count(primitive_count as i64, primitive_grain);
        let removals = Windows::even(&mut collapsed_removals, primitive_grain);
        let primitive_windows = Windows::even(&mut primitive_bases[1..], primitive_grain);
        let vertex_windows = Windows::new(vertex_bases.get_mut(1..).unwrap_or_default(), ranges, |range| {
            if full_selection { 0 } else { ((range * primitive_grain) as usize).min(primitive_count) }
        });
        support::run_indexed_ranges(primitive_grain, primitive_count as i64, &|range, first, last| {
            let (removals, primitive_bases, vertex_bases) =
                (removals.take(range), primitive_windows.take(range), vertex_windows.take(range));
            for primitive in first..last {
                support::poll(cancel_value, primitive)?;
                let (index, at) = (primitive as usize, (primitive - first) as usize);
                let size = view.primitive_offsets[index + 1] - view.primitive_offsets[index];
                if !selected(index) {
                    vertex_bases[at] = size;
                } else if view.primitive_kinds[index] != 0 {
                    if !full_selection {
                        vertex_bases[at] = size;
                    }
                    first_selected_curve.fetch_min(primitive, Ordering::Relaxed);
                } else if size == 3 {
                    if !full_selection {
                        vertex_bases[at] = 3;
                    }
                } else {
                    changes.store(true, Ordering::Relaxed);
                    let removed = collapsed_quad_removed(&positions, &view, index);
                    removals[at] = (removed + 1) as u8;
                    let triangles = if removed >= 0 { 1 } else { size - 2 };
                    primitive_bases[at] = triangles;
                    if !full_selection {
                        vertex_bases[at] = triangles * 3;
                    }
                }
            }
            Ok(())
        })?;
    }
    let first_selected_curve = first_selected_curve.load(Ordering::Relaxed);
    let changes = changes.load(Ordering::Relaxed);
    if first_selected_curve != i64::MAX {
        return Err(fail(format!(
            "{PREFIX}primitive {first_selected_curve} is a curve, not a polygon"
        )));
    }
    if !changes {
        return Ok(geometry_value.clone());
    }
    let too_big = || fail(format!("{PREFIX}output cardinality exceeds array limits"));
    for primitive in 0..primitive_count {
        cancel::poll(cancel_value, primitive as i64, 4095)?;
        let next_primitives = primitive_bases[primitive + 1];
        let next_vertices = if full_selection { next_primitives * 3 } else { vertex_bases[primitive + 1] };
        if primitive_bases[primitive].checked_add(next_primitives).is_none()
            || (!full_selection && vertex_bases[primitive].checked_add(next_vertices).is_none())
            || (full_selection
                && next_primitives
                    .checked_mul(3)
                    .and_then(|extra| primitive_bases[primitive].checked_add(extra))
                    .is_none())
        {
            return Err(too_big());
        }
        primitive_bases[primitive + 1] = primitive_bases[primitive] + next_primitives;
        if !full_selection {
            vertex_bases[primitive + 1] = vertex_bases[primitive] + next_vertices;
        }
    }
    let output_primitives = primitive_bases[primitive_count];
    let output_vertex_count =
        if full_selection { primitive_bases[primitive_count] * 3 } else { vertex_bases[primitive_count] };
    let mut output_vertices = vec![0i64; output_vertex_count as usize];
    let mut vertex_map = vec![0i64; output_vertex_count as usize];
    let mut output_offsets = vec![0i64; output_primitives as usize + 1];
    let mut output_kinds = vec![0u8; output_primitives as usize];
    let mut primitive_map = vec![0i64; output_primitives as usize];
    let first_failure = AtomicI64::new(i64::MAX);
    // Range `r` writes the output primitives/vertices of its source primitives: windows follow the prefix sums.
    let primitive_start = |range: i64| ((range * primitive_grain) as usize).min(primitive_count);
    let first_output_primitive = |primitive: usize| primitive_bases[primitive];
    let first_output_vertex =
        |primitive: usize| if full_selection { primitive_bases[primitive] * 3 } else { vertex_bases[primitive] };
    {
        let ranges = support::range_count(primitive_count as i64, primitive_grain);
        let primitive_bound = |range: i64| first_output_primitive(primitive_start(range)) as usize;
        let vertex_bound = |range: i64| first_output_vertex(primitive_start(range)) as usize;
        let vertex_windows = Windows::new(&mut output_vertices, ranges, vertex_bound);
        let map_windows = Windows::new(&mut vertex_map, ranges, vertex_bound);
        let offset_windows = Windows::new(&mut output_offsets, ranges, primitive_bound);
        let kind_windows = Windows::new(&mut output_kinds, ranges, primitive_bound);
        let origin_windows = Windows::new(&mut primitive_map, ranges, primitive_bound);
        support::run_indexed_ranges(primitive_grain, primitive_count as i64, &|range, first, last| {
            let (output_vertices, vertex_map) = (vertex_windows.take(range), map_windows.take(range));
            let (output_offsets, output_kinds, primitive_map) =
                (offset_windows.take(range), kind_windows.take(range), origin_windows.take(range));
            let (base_primitive, base_vertex) = (primitive_bound(range) as i64, vertex_bound(range) as i64);
            let mut scratch = polygon_triangulation::create_scratch();
            for primitive in first as usize..last as usize {
                cancel::poll(cancel_value, primitive as i64, 1023)?;
                let source_first = view.primitive_offsets[primitive];
                let source_last = view.primitive_offsets[primitive + 1];
                let output_primitive = first_output_primitive(primitive) - base_primitive;
                let output_vertex = first_output_vertex(primitive) - base_vertex;
                let (offset_value, source_size) = (first_output_vertex(primitive), source_last - source_first);
                if !selected(primitive) || source_size == 3 {
                    output_offsets[output_primitive as usize] = offset_value;
                    output_kinds[output_primitive as usize] = view.primitive_kinds[primitive];
                    primitive_map[output_primitive as usize] = primitive as i64;
                    for local in 0..source_size {
                        output_vertices[(output_vertex + local) as usize] =
                            view.vertex_points[(source_first + local) as usize];
                        vertex_map[(output_vertex + local) as usize] = source_first + local;
                    }
                } else if collapsed_removals[primitive] != 0 {
                    let removed = i64::from(collapsed_removals[primitive]) - 1;
                    let remaining = |ordinal: i64| if ordinal < removed { ordinal } else { ordinal + 1 };
                    let olds = [
                        source_first + remaining(0),
                        source_first + remaining(1),
                        source_first + remaining(2),
                    ];
                    for (corner, &old) in olds.iter().enumerate() {
                        output_vertices[output_vertex as usize + corner] = view.vertex_points[old as usize];
                        vertex_map[output_vertex as usize + corner] = old;
                    }
                    output_offsets[output_primitive as usize] = offset_value;
                    primitive_map[output_primitive as usize] = primitive as i64;
                } else if (primitive as i64) < first_failure.load(Ordering::Relaxed) {
                    let mut emit = |triangle: i64, a: i64, b: i64, c: i64| {
                        let vertex = output_vertex + triangle * 3;
                        let target = (output_primitive + triangle) as usize;
                        for (corner, &old) in [a, b, c].iter().enumerate() {
                            output_vertices[vertex as usize + corner] = view.vertex_points[old as usize];
                            vertex_map[vertex as usize + corner] = old;
                        }
                        output_offsets[target] = vertex + base_vertex;
                        primitive_map[target] = primitive as i64;
                    };
                    if polygon_triangulation::primitive(
                        cancel_value, &positions, &view, Some(&mut scratch), primitive as i64, &mut emit,
                    )
                    .is_err()
                    {
                        first_failure.fetch_min(primitive as i64, Ordering::Relaxed);
                    }
                }
            }
            Ok(())
        })?;
    }
    output_offsets[output_primitives as usize] = output_vertex_count;
    let failure = Some(first_failure.load(Ordering::Relaxed)).filter(|&p| p != i64::MAX).map(|p| p as usize);
    if let Some(primitive) = failure {
        let mut scratch = polygon_triangulation::create_scratch();
        return match polygon_triangulation::primitive(
            cancel_value, &positions, &view, Some(&mut scratch), primitive as i64, &mut |_, _, _, _| {},
        ) {
            Err(message) => Err(fail(format!("{PREFIX}{}", error::message(&message)))),
            Ok(()) => Err(fail(format!("{PREFIX}primitive {primitive} failed triangulation"))),
        };
    }
    let output_topology = topology::View::create_validated_owned(
        geometry::point_count(geometry_value),
        output_vertices,
        output_offsets,
        output_kinds,
    );
    topology_remap::preserving_points(
        cancel_value, grain, &output_topology, &vertex_map, &primitive_map, geometry_value,
    )
}

pub fn run(params: &Params, geometry_value: &geometry::T) -> Result<geometry::T, error::T> {
    let grain = params.grain.unwrap_or(16_384);
    triangulate(params.cancel.as_deref(), grain, params.primitives.as_ref(), geometry_value).map_err(
        |failure| {
            if error::is_cancelled(&failure) {
                error::cancelled("triangulate")
            } else {
                make("triangulate", Code::InvalidTopology, error::message(&failure).to_owned())
            }
        },
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;
    use crate::compact_points::fixtures::*;
    use rdk_core::attribute::{Owner, Storage};
    use rdk_core::topology::PrimitiveKind;

    fn params() -> Params {
        Params { cancel: None, grain: Some(1), primitives: None }
    }
    #[test]
    fn deferred_topology_triangulates_like_owned_once_in_each_domain_context() {
        use std::sync::{Arc, atomic::{AtomicUsize, Ordering}};
        for domains in [1, 4] {
            let source = grid(2, 2, 2.0, true);
            let reads = Arc::new(AtomicUsize::new(0));
            let counter = Arc::clone(&reads);
            let owned = geometry::topology(&source).clone();
            let deferred = topology::deferred(
                None, None, None, topology::point_count(&owned), topology::vertex_count(&owned),
                topology::primitive_count(&owned), Arc::new(move || {
                    counter.fetch_add(1, Ordering::SeqCst);
                    Ok(owned.clone())
                }),
            ).unwrap();
            let deferred = geometry::create(
                geometry::positions(&source).clone(), deferred, Some(geometry::attributes(&source)), None, None,
            ).unwrap();
            let actual = at_domains(domains, &|| run(&params(), &deferred).unwrap());
            let expected = at_domains(domains, &|| run(&params(), &source).unwrap());
            assert_eq!(geometry_signature(&actual), geometry_signature(&expected), "deferred triangulation");
            assert_eq!(reads.load(Ordering::SeqCst), 1, "CPU consumer did not materialise topology once");
        }
    }


    #[test]
    fn collapsed_quad_and_deterministic_failure() {
        let collapsed = geom(&[0., 1., 1., 0.], &[0., 0., 0., 1.], &[0.; 4], &[0, 1, 2, 3], &[0, 4], None);
        let collapsed = with_attr(&collapsed, Owner::Vertex, "corner", Storage::Int(Arc::new(vec![10, 11, 12, 13])));
        let out = run(&params(), &collapsed).unwrap();
        assert_eq!(topo(&out).0, vec![0, 1, 3]);
        assert_eq!(ints(Owner::Vertex, "corner", &out), vec![10, 11, 13]);

        let degenerate = geom(
            &[0., 1., 2., 3., 4., 5., 6., 7.],
            &[0.; 8],
            &[0.; 8],
            &[0, 1, 2, 3, 4, 5, 6, 7],
            &[0, 4, 8],
            None,
        );
        let failure = err(run(&params(), &degenerate));
        assert_eq!(error::kind(&failure), Code::InvalidTopology);
        assert_eq!(
            error::message(&failure),
            "Rdk_mesh.Triangulate.triangulate: primitive 0 has degenerate projected area"
        );
        let messages = |domains| {
            at_domains_strings(domains, &|| {
                vec![error::message(&err(run(&params(), &degenerate))).to_owned()]
            })
        };
        assert_eq!(messages(1), messages(4), "lowest failing primitive differs across domains");
    }

    #[test]
    fn quads_become_triangles_with_ancestry() {
        let quads = grid(2, 1, 2.0, true);
        let quads = with_attr(&quads, Owner::Primitive, "face", Storage::Int(Arc::new(vec![7, 8])));
        let out = run(&params(), &quads).unwrap();
        assert_eq!(geometry::primitive_count(&out), 4);
        assert_eq!(ints(Owner::Primitive, "face", &out), vec![7, 7, 8, 8]);
        let (vp, offsets, kinds) = topo(&out);
        assert_eq!(offsets, vec![0, 3, 6, 9, 12]);
        assert_eq!(vp.len(), 12);
        assert!(kinds.iter().all(|&k| k == 0));
        // Triangles come back untouched.
        let triangles = grid(2, 1, 2.0, false);
        assert_eq!(geometry::data_id(&run(&params(), &triangles).unwrap()), geometry::data_id(&triangles));
    }

    #[test]
    fn selection_keeps_other_primitives() {
        let quads = grid(2, 1, 2.0, true);
        let selection = group::init(None, group::Owner::Primitive, "pick", 2, &|p| p == 1).unwrap();
        let out = run(&Params { primitives: Some(selection), ..params() }, &quads).unwrap();
        let (_, offsets, _) = topo(&out);
        assert_eq!(offsets, vec![0, 4, 7, 10]);
        let wrong = group::init(None, group::Owner::Point, "pick", 2, &|_| true).unwrap();
        assert_eq!(error::code(&err(run(&Params { primitives: Some(wrong), ..params() }, &quads))), "invalid_topology");
    }

    #[test]
    fn curves_are_refused_and_cancel_is_reported() {
        let curve = polyline(&[(0., 0., 0.), (1., 0., 0.), (2., 0., 0.)]);
        let failure = err(run(&params(), &curve));
        assert_eq!(
            error::message(&failure),
            "Rdk_mesh.Triangulate.triangulate: primitive 0 is a curve, not a polygon"
        );
        let token = std::sync::Arc::new(cancel::create());
        cancel::cancel(&token);
        let quads = grid(1, 1, 1.0, true);
        let failure = err(run(&Params { cancel: Some(token), ..params() }, &quads));
        assert_eq!(error::kind(&failure), Code::Cancelled);
        let mixed = geom(
            &[0., 1., 1., 0., 2.],
            &[0., 0., 1., 1., 2.],
            &[0.; 5],
            &[0, 1, 2, 3, 3, 4],
            &[0, 4, 6],
            Some(vec![PrimitiveKind::Polygon, PrimitiveKind::OpenPolyline]),
        );
        let selection = group::init(None, group::Owner::Primitive, "polys", 2, &|p| p == 0).unwrap();
        let out = run(&Params { primitives: Some(selection), ..params() }, &mixed).unwrap();
        assert_eq!(topo(&out).2, vec![0, 0, 1]);
    }

    #[test]
    fn parallel_exact() {
        let g = grid(200, 200, 2.0, true);
        let count = topology::primitive_count(geometry::topology(&g));
        let full = exact_at_one_and_four(&|| run(&Params { cancel: None, grain: None, primitives: None }, &g).unwrap());
        assert_eq!(topology::primitive_count(geometry::topology(&full)), count * 2);
        let odd: Vec<i64> = (0..count).filter(|p| p % 3 == 0).collect();
        let selection = group::ordered(group::Owner::Primitive, "odd", count, &odd).unwrap();
        let some = exact_at_one_and_four(&|| {
            run(&Params { cancel: None, grain: None, primitives: Some(selection.clone()) }, &g).unwrap()
        });
        assert_eq!(topology::primitive_count(geometry::topology(&some)), count + odd.len() as i64);
    }
}
