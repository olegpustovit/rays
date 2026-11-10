//! Per-cook allocation budgets (thread-local `mem_sys` counters). Limits may only go down.
use rdk_ops::subdivide::{self, BoundaryInterpolation, CrackPolicy, CreasingMethod, FaceVaryingInterpolation, Params, Scheme, TrianglePolicy};
use rdk_core::attribute::{self, Owner, Storage};
use rdk_core::{geometry, packed, topology};

use crate::attribute_kernel;

fn grid(cells: i64) -> geometry::T {
    let stride = cells + 1;
    let (mut x, mut z) = (Vec::new(), Vec::new());
    for row in 0..=cells {
        for column in 0..=cells {
            x.push(column as f64);
            z.push(row as f64);
        }
    }
    let (mut vertex_points, mut offsets) = (Vec::new(), vec![0]);
    for row in 0..cells {
        for column in 0..cells {
            let p = row * stride + column;
            vertex_points.extend([p, p + 1, p + stride + 1, p + stride]);
            offsets.push(vertex_points.len() as i64);
        }
    }
    let points = x.len();
    let topology = topology::polygons_owned(points as i64, topology::saturate(&vertex_points), topology::saturate(&offsets)).unwrap();
    geometry::create(packed::float3::view::of_owned_exn(x, vec![0.0; points], z).unwrap(), topology, Some(Vec::new()), None, None).unwrap()
}

/// (d) peak live bytes while subdividing a 128x128 quad grid once, on one thread.
#[test]
fn subdivide_peak_bytes() {
    let source = grid(128);
    let params = Params {
        cancel: None,
        grain: None,
        scheme: Scheme::CatmullClark,
        iterations: 1,
        primitives: None,
        cracks: CrackPolicy::SubdivideDoNotClose,
        consistent_topology: false,
        creases: None,
        crease_primitives: None,
        crease_weight: None,
        generate_resulting_creases: true,
        resulting_crease_group: None,
        hole_primitives: None,
        remove_holes: true,
        boundary_interpolation: BoundaryInterpolation::SubdivideBoundaryEdgeOnly,
        face_varying_interpolation: FaceVaryingInterpolation::SubdivideFvarAll,
        triangle_policy: TrianglePolicy::SubdivideTrianglesCatmullClark,
        creasing_method: CreasingMethod::SubdivideCreasingUniform,
        treat_curves_as_independent: false,
        recompute_point_normals: false,
    };
    let (peak, allocs) = (std::cell::Cell::new(0), std::cell::Cell::new(0));
    parallel::run_unit(Some(1), &|| {
        mem_sys::local_reset();
        let out = subdivide::subdivide(&params, &source).unwrap();
        (peak.set(mem_sys::local_peak_bytes()), allocs.set(mem_sys::local_alloc_count()));
        assert_eq!(geometry::primitive_count(&out), 128 * 128 * 4);
        Ok(())
    })
    .unwrap();
    eprintln!("subdivide peak bytes: {} allocs: {}", peak.get(), allocs.get());
    assert!(peak.get() <= LIMIT, "subdivide peak {} > {LIMIT}", peak.get());
}

// 24_638_058 B after Z4 u32 topology (29_101_694 after C4; 29_629_590 after M18; 34_086_038 before C3/M18): 128x128 quads, one Catmull-Clark pass.
const LIMIT: i64 = 24_700_000;

/// (c) Flow read then write of P, Cd, a float, an int and a text attribute on a 100k-point grid, on
/// one thread: peak live bytes and allocation count above the baseline.
#[test]
fn attribute_round_trip_bytes() {
    let mut source = grid(316);
    let n = geometry::point_count(&source) as usize;
    for (name, storage) in [
        ("Cd", Storage::Float4(packed::float4::of_owned(vec![0.25; n], vec![0.5; n], vec![0.75; n], vec![1.0; n]).unwrap())),
        ("mass", Storage::Float(std::sync::Arc::new((0..n).map(|i| i as f64).collect()))),
        ("id", Storage::Int(std::sync::Arc::new((0..n as i64).collect()))),
        ("tag", Storage::Text(std::sync::Arc::new((0..n).map(|i| format!("t{i}").into_bytes()).collect()))),
    ] {
        source = geometry::with_attribute(attribute::create_owned(name, Owner::Point, storage).unwrap(), &source).unwrap();
    }
    let (peak, allocs) = (std::cell::Cell::new(0), std::cell::Cell::new(0));
    parallel::run_unit(Some(1), &|| {
        mem_sys::local_reset();
        let mut out = source.clone();
        for (kind, name) in [("vec3", "P"), ("vec4", "Cd"), ("float", "mass"), ("int", "id"), ("text", "tag")] {
            let value = attribute_kernel::read(None, Owner::Point, kind, name, &source).unwrap();
            out = attribute_kernel::write(16_384, Owner::Point, kind, name, &value, &out).unwrap();
        }
        (peak.set(mem_sys::local_peak_bytes()), allocs.set(mem_sys::local_alloc_count()));
        assert_eq!(geometry::point_count(&out), n as i64);
        Ok(())
    })
    .unwrap();
    eprintln!("attribute round trip peak bytes: {} allocs: {}", peak.get(), allocs.get());
    assert!(peak.get() <= ROUND_TRIP_PEAK, "round trip peak {} > {ROUND_TRIP_PEAK}", peak.get());
    assert!(allocs.get() <= ROUND_TRIP_ALLOCS, "round trip allocs {} > {ROUND_TRIP_ALLOCS}", allocs.get());
}

// Measured after M19: 11_258_675 B / 195 allocs (17_264_704 B / 201_176 after M13: the text rows were one allocation each).
const ROUND_TRIP_PEAK: i64 = 11_300_000;
const ROUND_TRIP_ALLOCS: u64 = 200;

/// (e) A Flow read then write of a 1M-row text attribute: the rows are one offsets vector and one byte
/// buffer shared by the geometry and the Flow value, so the round trip allocates a handful of times, not per row.
#[test]
fn text_attribute_million_rows() {
    let n = 1_000_000usize;
    let topology = topology::polygons_owned(n as i64, Vec::new(), vec![0]).unwrap();
    let zeros = || vec![0.0; n];
    let source = geometry::create(packed::float3::view::of_owned_exn(zeros(), zeros(), zeros()).unwrap(), topology, Some(Vec::new()), None, None).unwrap();
    let tag = Storage::Text(std::sync::Arc::new((0..n).map(|i| format!("t{i}")).collect()));
    let source = geometry::with_attribute(attribute::create_owned("tag", Owner::Point, tag).unwrap(), &source).unwrap();
    let (peak, allocs) = (std::cell::Cell::new(0), std::cell::Cell::new(0));
    parallel::run_unit(Some(1), &|| {
        mem_sys::local_reset();
        let value = attribute_kernel::read(None, Owner::Point, "text", "tag", &source).unwrap();
        let out = attribute_kernel::write(16_384, Owner::Point, "text", "tag", &value, &source).unwrap();
        (peak.set(mem_sys::local_peak_bytes()), allocs.set(mem_sys::local_alloc_count()));
        assert_eq!(geometry::point_count(&out), n as i64);
        Ok(())
    })
    .unwrap();
    eprintln!("text 1M peak bytes: {} allocs: {}", peak.get(), allocs.get());
    assert!(peak.get() <= TEXT_PEAK, "text round trip peak {} > {TEXT_PEAK}", peak.get());
    assert!(allocs.get() <= TEXT_ALLOCS, "text round trip allocs {} > {TEXT_ALLOCS}", allocs.get());
}

// Measured: 267 B / 8 allocs for 1M rows (Arc clones; the rows are never copied).
const TEXT_PEAK: i64 = 400;
const TEXT_ALLOCS: u64 = 8;

// Measured: 49_180 allocs / 82_472 peak bytes after warming the evaluator caches.
/// (a) The reference walker: field access, list destructuring and a captured closure in a loop.
#[test]
fn walker_loop_allocations() {
    let forms = flow_syntax::syntax::parse("(workspace budget
        (graph g :context host
            (let* [offset {:step (+ t 1.)}
                   f (fn [[x y]] (+ (+ x y) offset.step))]
                (sum [i (range 1024)]
                    (let* [row {:n i :next (+ i 1)}]
                        (f (list row.n row.next)))))))").unwrap();
    let catalog = flow_check::catalog::Catalog { version: 1, kinds: vec![] };
    let (workspace, diagnostics) = flow_check::workspace::check(&[], false, false, None, &catalog, &forms);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    let evaluated = flow_eval::static_eval(&workspace.unwrap()).unwrap();
    let residual = &evaluated.results[0].1;
    assert!(matches!(residual, flow_syntax::value::Value::Residual(_)));
    let live = input::at_time(0.5);
    // Warm route and pattern caches; only execution belongs to the steady-state budget.
    flow_eval::host::force_reference(residual, &live, None, None, None).unwrap();
    mem_sys::local_reset();
    let result = flow_eval::host::force_reference(residual, &live, None, None, None).unwrap();
    let (allocs, peak) = (mem_sys::local_alloc_count(), mem_sys::local_peak_bytes());
    assert_eq!(result, flow_syntax::value::Value::Float(1_050_112.0));
    eprintln!("walker loop: {allocs} allocs / {peak} peak bytes");
    assert!(allocs <= 49_200, "walker loop: {allocs} allocations");
    assert!(peak <= 83_000, "walker loop: {peak} peak bytes");
}

// Measured: 12 allocs / 164_634 peak bytes; the 4096 stores allocate no per-store copies.
/// (b) An unpublished 4096-element builder retained in two locals: each store must reuse its buffer.
#[test]
fn aliased_store_allocations() {
    use flow_ir::{core_exec, core_ir as c};
    use flow_syntax::{ty::Ty, value::Value};
    let source = c::Source { site: vec!["store-budget".into()], owner: vec![], instance: 0, prefix: vec![], iter: vec![], span: None };
    let statement = |id, node| c::Statement { id, source: source.clone(), node };
    let list = Ty::List(Box::new(Ty::Int));
    let program = c::Program {
        constants: vec![c::Literal::Int(4096), c::Literal::Int(0)],
        locals: vec![Ty::Int, list.clone(), list.clone(), Ty::Int, Ty::Int, Ty::Int, Ty::Int, Ty::Record(vec![("original".into(), list.clone()), ("alias".into(), list)])],
        body: vec![
            statement(0, c::StatementNode::Assign(0, c::Expr::Constant(0))),
            statement(1, c::StatementNode::Assign(1, c::Expr::Prim(c::Primitive::Sequence_build, vec![("count".into(), 0)]))),
            statement(2, c::StatementNode::Assign(2, c::Expr::Local(1))),
            statement(3, c::StatementNode::Assign(3, c::Expr::Constant(1))),
            statement(4, c::StatementNode::Counted {
                first: 3, last: 0, count: 4, index: 5, item: 6, limit: 4096, stop: None, init: vec![],
                body: vec![statement(5, c::StatementNode::Store { sequence: 2, index: 5, value: 6 })],
            }),
            statement(6, c::StatementNode::Assign(7, c::Expr::Fields(vec![("original".into(), 1), ("alias".into(), 2)]))),
        ],
        results: vec![7], state_slots: vec![], regions: vec![], call_slots: 0,
    };
    let prepared = core_exec::prepare(&program, false, false, &Default::default(), false);
    let options = core_exec::Options { strict: true, ..Default::default() };
    mem_sys::local_reset();
    let output = core_exec::execute(&prepared, &input::at_time(0.0), &options).unwrap();
    let (allocs, peak) = (mem_sys::local_alloc_count(), mem_sys::local_peak_bytes());
    let Value::Record(fields) = output else { panic!("expected both aliases") };
    let (Value::List(original), Value::List(alias)) = (&fields[0].1, &fields[1].1) else { panic!("expected lists") };
    assert!(std::sync::Arc::ptr_eq(original, alias));
    assert_eq!(original.len(), 4096);
    assert!(original.iter().enumerate().all(|(i, value)| *value == Value::Int(i as i64)));
    eprintln!("aliased store: {allocs} allocs / {peak} peak bytes");
    assert!(allocs <= 16, "aliased store: {allocs} allocations");
    assert!(peak <= 165_000, "aliased store: {peak} peak bytes");
}

/// C3: the shared fixed-width attribute mappers should own one output buffer per plane.
#[test]
fn attribute_map_output_buffers() {
    use rdk_ops::attribute_ops::{map_ints, map_planes};
    const COUNT: usize = 131_072;
    parallel::run_unit(Some(1), &|| {
        let planes = [vec![1.0; COUNT], vec![2.0; COUNT], vec![3.0; COUNT]];
        let refs = planes.each_ref().map(Vec::as_slice);
        mem_sys::local_reset();
        let start = std::time::Instant::now();
        let output = map_planes(None, 4096, &refs, &|i, slot| {
            slot[0] += i as f64;
            slot[1] *= 2.0;
            slot[2] -= 0.25;
            Ok(())
        }).unwrap();
        let (float_peak, float_allocs, elapsed) = (mem_sys::local_peak_bytes(), mem_sys::local_alloc_count(), start.elapsed());
        assert!(output[0].iter().enumerate().all(|(i, value)| *value == i as f64 + 1.0));
        assert!(output[1].iter().all(|value| *value == 4.0) && output[2].iter().all(|value| *value == 2.75));
        assert!(planes[0].iter().all(|value| *value == 1.0));
        eprintln!("attribute float map: {float_peak} peak bytes / {float_allocs} allocs / {elapsed:?}");
        assert!(float_peak <= 3_160_000 && float_allocs <= 110);
        drop(output);
        let initial = vec![7_i64; COUNT];
        mem_sys::local_reset();
        let start = std::time::Instant::now();
        let output = map_ints(None, 4096, &initial, &|i, slot| { *slot += i; Ok(()) }).unwrap();
        let (int_peak, int_allocs, elapsed) = (mem_sys::local_peak_bytes(), mem_sys::local_alloc_count(), start.elapsed());
        assert!(output.iter().enumerate().all(|(i, value)| *value == i as i64 + 7));
        assert!(initial.iter().all(|value| *value == 7));
        eprintln!("attribute int map: {int_peak} peak bytes / {int_allocs} allocs / {elapsed:?}");
        assert!(int_peak <= 1_055_000 && int_allocs <= 40);
        Ok(())
    }).unwrap();
}

// Shared residual/term/Record data, Symbol names and geometry layout: maxima across all three fixture sizes,
// rounded to cover key-size variation. Never raise.
// Each pair is (allocations, peak B); changed-input peak excludes only newly owned output payload.
const SUBDIVIDE_WARM: [(u64, i64); 2] = [(18, 3_752), (68_565, 2_660_000)];
const BOOLEAN_WARM: [(u64, i64); 2] = [(25, 4_426), (16_500, 855_000)];
const SCATTER_WARM: [(u64, i64); 2] = [(18, 5_360), (262_500, 6_400_000)];

// These are session cooks: lowering and input construction stay outside the counters.
// Measure a cache hit, then a genuinely new input after the same-sized kernel has warmed.
fn warm_cook_allocations(name: &str, root: &sop::node::T, inputs: &[Vec<geometry::T>; 3], limits: [(u64, i64); 2]) {
    use crate::lisp_sop::{self, support};
    use sop::{node, session};
    assert_eq!(node::inputs(root).len(), inputs[0].len(), "{name}: fixture inputs must reach the kernel");
    let variants = inputs.each_ref().map(|inputs| node::build::rebuild_with_inputs(root,
        &inputs.iter().map(lisp_sop::snapshot).collect::<Vec<_>>()));
    let input_ids: Vec<_> = inputs[2].iter().flat_map(geometry::payload_components).map(|(id, _)| id).collect();
    let context = support::context(1, 97);
    let mut session = support::new_session(128, 64 * 1024 * 1024);
    support::cook(&mut session, &context, &variants[0]);
    let warm = support::cook(&mut session, &context, &variants[1]);
    let misses = session::stats(&session).misses;
    mem_sys::local_reset();
    let unchanged = support::cook(&mut session, &context, &variants[1]);
    let hit = (mem_sys::local_alloc_count(), mem_sys::local_peak_bytes());
    assert_eq!(session::stats(&session).misses, misses, "{name}: unchanged cook missed");
    assert_eq!(geometry::data_id(&unchanged), geometry::data_id(&warm), "{name}: cache hit rebuilt output");
    mem_sys::local_reset();
    let changed = support::cook(&mut session, &context, &variants[2]);
    let changed_allocs = mem_sys::local_alloc_count();
    let changed_peak = mem_sys::local_peak_bytes();
    let mut output_components = geometry::payload_components(&changed);
    output_components.sort_unstable_by_key(|(id, _)| *id);
    output_components.dedup_by_key(|(id, _)| *id);
    let output_bytes: i64 = output_components.iter()
        .filter(|(id, _)| !input_ids.contains(id)).map(|(_, bytes)| bytes).sum();
    let scratch = (changed_allocs, (changed_peak - output_bytes).max(0));
    assert!(session::stats(&session).misses > misses, "{name}: changed input hit stale output");
    assert!(geometry::point_count(&changed) > 0, "{name}: empty kernel result");
    let bytes = support::geometry_bytes(&changed);
    assert_ne!(bytes, support::geometry_bytes(&warm), "{name}: changed input did no work");
    assert_eq!(bytes, support::geometry_bytes(&support::cook_once(1, &variants[2])), "{name}: warm result differs from fresh cook");
    session::close(&mut session);
    eprintln!("{name}: hit {hit:?}; changed {scratch:?} (allocations / scratch peak B, output {output_bytes} B)");
    for (actual, limit) in [hit, scratch].into_iter().zip(limits) {
        assert!(actual.0 <= limit.0 && actual.1 <= limit.1, "{name}: {actual:?} exceeds {limit:?}");
    }
}

fn shifted(source: &geometry::T, dx: f64) -> geometry::T {
    let view = packed::float3::view::view(geometry::positions(source)).unwrap();
    let positions = packed::float3::view::of_owned_exn(view.x.iter().map(|x| x + dx).collect(), view.y.iter().map(|y| y + dx*0.3).collect(), view.z.iter().map(|z| z + dx*0.2).collect()).unwrap();
    geometry::with_positions(positions, source).unwrap()
}

#[test]
fn warm_subdivide_allocations() {
    use crate::lisp_sop;
    for cells in [8, 16, 32] {
        let source = grid(cells);
        let inputs = [vec![source.clone()], vec![shifted(&source, 0.25)], vec![shifted(&source, 0.5)]];
        let root = lisp_sop::node(&[("src".into(), lisp_sop::snapshot(&source))], "(sop/subdivide (sop/ext_src) :iterations 1 :hole_group \"\")");
        warm_cook_allocations("subdivide", &root, &inputs, SUBDIVIDE_WARM);
    }
}

#[test]
fn warm_boolean_allocations() {
    use crate::lisp_sop::{self, support};
    for count in [1, 4, 16] {
        let boxes = lisp_sop::node(&[], &format!(r#"(sop/merge (for [i (range {count})]
            (sop/transform (sop/box :size [2 2 2] :consolidate_points true) :translate [(* i 4.) 0. 0.])))"#));
        let source = support::cook_once(1, &boxes);
        let inputs = [0.25, 0.5, 0.75].map(|dx| vec![source.clone(), shifted(&source, dx)]);
        let root = lisp_sop::node(&[("left".into(), lisp_sop::snapshot(&inputs[0][0])), ("right".into(), lisp_sop::snapshot(&inputs[0][1]))],
            "(sop/boolean (sop/ext_left) (sop/ext_right) :operation \"Intersection\")");
        warm_cook_allocations("boolean", &root, &inputs, BOOLEAN_WARM);
    }
}

#[test]
fn warm_scatter_allocations() {
    use crate::lisp_sop;
    for cells in [8, 16, 32] {
        let source = grid(cells);
        let density = Storage::Float(std::sync::Arc::new((0..geometry::point_count(&source)).map(|i| 0.05 + (i % 19) as f64 / 20.).collect()));
        let source = geometry::with_attribute(attribute::create_owned("scatter_density", Owner::Point, density).unwrap(), &source).unwrap();
        let selection = rdk_core::group::init(None, rdk_core::group::Owner::Primitive, "scatter_surface", geometry::primitive_count(&source), &|i| Ok(i % 4 != 0)).unwrap();
        let source = geometry::with_group(selection, &source).unwrap();
        let inputs = [vec![source.clone()], vec![shifted(&source, 0.25)], vec![shifted(&source, 0.5)]];
        let root = lisp_sop::node(&[("src".into(), lisp_sop::snapshot(&source))], &format!(
            "(sop/scatter (sop/ext_src) :count {} :seed 1339 :group \"scatter_surface\" :use_density true :density_owner \"Point\" :density_attribute \"scatter_density\" :point_pattern \"scatter_density\" :source_primitive_attribute \"source_primitive\")", cells*cells));
        warm_cook_allocations("scatter", &root, &inputs, SCATTER_WARM);
    }
}
