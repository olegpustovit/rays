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
    let topology = topology::polygons_owned(points as i64, vertex_points, offsets).unwrap();
    geometry::create(packed::Float3::View::of_owned_exn(x, vec![0.0; points], z).unwrap(), topology, Some(Vec::new()), None, None).unwrap()
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

// 29_629_590 B measured after M18 (34_086_038 B before C3/M18): quad grid 128x128, one Catmull-Clark pass.
const LIMIT: i64 = 29_700_000;

/// (c) Flow read then write of P, Cd, a float, an int and a text attribute on a 100k-point grid, on
/// one thread: peak live bytes and allocation count above the baseline.
#[test]
fn attribute_round_trip_bytes() {
    let mut source = grid(316);
    let n = geometry::point_count(&source) as usize;
    for (name, storage) in [
        ("Cd", Storage::Float4(packed::Float4::of_owned(vec![0.25; n], vec![0.5; n], vec![0.75; n], vec![1.0; n]).unwrap())),
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
    let source = geometry::create(packed::Float3::View::of_owned_exn(zeros(), zeros(), zeros()).unwrap(), topology, Some(Vec::new()), None, None).unwrap();
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
