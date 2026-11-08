//! Per-cook allocation budgets (thread-local `mem_sys` counters). Limits may only go down.
use rdk_ops::subdivide::{self, BoundaryInterpolation, CrackPolicy, CreasingMethod, FaceVaryingInterpolation, Params, Scheme, TrianglePolicy};
use rdk_core::{geometry, packed, topology};

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
