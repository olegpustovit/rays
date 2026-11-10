//! resident triangle indices match the host's exact ear
//! order (winding, projection, concavity, partial groups, two-pass reduction) and fall back
//! conservatively (`E_GPU_DISPLAY_TRIANGLES`); the host wrapper reuses its display storage and
//! counts only validation bytes; display normals of the result point at +z.
use crate::common;
use flow_gpu::{host, pipelines, run};
use ogpu_core::backend as b;
use ogpu_core::types::{BufferDescriptor, BufferUsage};
use rdk_core::packed::float3;
use rdk_core::{error, geometry, topology};
use rdk_ops::triangulate;
use std::cell::Cell;
use std::rc::Rc;
use std::sync::Arc;

type Shape = Vec<(f64, f64, f64)>;

struct Case {
    kind: i32,
    bad_vertex: bool,
    mixed: bool,
    copies: usize,
    accept: bool,
}

fn case(copies: usize, accept: bool) -> Case {
    Case { kind: 0, bad_vertex: false, mixed: false, copies, accept }
}

fn words(values: &[i64]) -> Vec<u8> {
    values.iter().flat_map(|v| (*v as i32).to_le_bytes()).collect()
}

fn run_case(lease: &renderer::renderer::Gpu, pipes: &pipelines::T, host_: Option<&host::T>, c: Case, shape: &Shape) {
    let copies = c.copies;
    let corners = shape.len();
    let mut offsets = vec![0usize; copies + 1];
    for i in 0..copies {
        offsets[i + 1] = offsets[i] + if c.mixed && i % 2 == 0 { 3 } else { corners };
    }
    let point_count = offsets[copies];
    let (mut xs, mut ys, mut zs) = (vec![0.0; point_count], vec![0.0; point_count], vec![0.0; point_count]);
    for i in 0..copies {
        for at in offsets[i]..offsets[i + 1] {
            let (x, y, z) = shape[at - offsets[i]];
            xs[at] = x;
            ys[at] = y;
            zs[at] = z;
        }
    }
    let mut vertices: Vec<i64> = (0..point_count as i64).collect();
    if c.bad_vertex {
        vertices[0] = point_count as i64;
    }
    let device = renderer::renderer::gpu_device(lease);
    let buffers: Rc<std::cell::RefCell<Vec<b::Buffer>>> = Rc::default();
    let buffer = |bytes: usize| -> b::Buffer {
        let x = b::create_buffer(
            &device,
            BufferDescriptor { label: Some("resident triangle test".into()), size: bytes.max(4) as i64, usage: vec![BufferUsage::Storage, BufferUsage::Index, BufferUsage::CopySrc, BufferUsage::CopyDst] },
            None,
        )
        .expect("buffer");
        buffers.borrow_mut().push(x.clone());
        x
    };
    let upload = |bytes: Vec<u8>| -> b::Buffer {
        let x = buffer(bytes.len());
        b::write_buffer(&x, 0, &bytes).expect("write");
        x
    };
    let offsets_i: Vec<i64> = offsets.iter().map(|v| *v as i64).collect();
    let p_bytes: Vec<u8> = (0..point_count).flat_map(|i| [xs[i], ys[i], zs[i]].into_iter().flat_map(|v| (v as f32).to_le_bytes())).collect();
    let (p, v, o, k) = (upload(p_bytes), upload(words(&vertices)), upload(words(&offsets_i)), upload(words(&vec![c.kind as i64; copies])));
    let out = buffer((3 * (point_count as i64 - 2 * copies as i64)).max(0) as usize * 4);
    let blocks = buffer((copies + 255) / 256 * 4);
    let status = buffer(4);
    let reads = Rc::new(Cell::new(0i64));
    let r2 = reads.clone();
    let result = run::triangle_indices(&move |n| r2.set(r2.get() + n), &renderer::renderer::gpu_queue(lease), pipes, (&p, point_count as i64), (&v, point_count as i64), (&o, copies as i64 + 1), (&k, copies as i64), &out, &blocks, &status);
    if let Some(h) = host_ {
        let before = host::stats(h);
        let invoke = || host::triangle_indices(h, (&p, point_count as i64), (&v, point_count as i64), (&o, copies as i64 + 1), (&k, copies as i64));
        match (invoke(), &result) {
            (Ok((buf, count, release)), Ok(expected)) => {
                assert_eq!(count, *expected);
                assert_eq!(b::read_buffer(&buf, 0, count * 4).unwrap(), b::read_buffer(&out, 0, count * 4).unwrap());
                release();
                let warm = host::stats(h);
                let (reused, _, release) = invoke().expect("again");
                assert_eq!(b::buffer_id(&reused), b::buffer_id(&buf));
                release();
                assert_eq!(host::stats(h).buffer_creations, warm.buffer_creations);
                assert_eq!(host::stats(h).pipeline_compilations, warm.pipeline_compilations);
            }
            (Err(d), Err(expected)) => assert_eq!(d.code, expected.code),
            _ => panic!("host triangle ownership wrapper changed the result"),
        }
        let after = host::stats(h);
        assert_eq!(after.payload_readback_bytes, before.payload_readback_bytes);
        assert_eq!(after.validation_readback_bytes - before.validation_readback_bytes, if c.accept { 8 } else { reads.get() });
    }
    if c.accept {
        let count = result.expect("accepted");
        assert_eq!(reads.get(), 4);
        let positions = float3::of_owned(xs, ys, zs).expect("positions");
        let top = topology::polygons_owned(point_count as i64, topology::saturate(&vertices), topology::saturate(&offsets_i)).expect("topology");
        let geo = geometry::create(positions, top, None, None, None).expect("geometry");
        let expected = match triangulate::run(&triangulate::Params { cancel: None, grain: None, primitives: None }, &geo) {
            Ok(g) => topology::view::view(geometry::topology(&g)).expect("view").vertex_points.to_vec(),
            Err(e) => panic!("{}", error::to_string(&e)),
        };
        let actual = b::read_buffer(&out, 0, count * 4).unwrap();
        assert_eq!(count as usize, expected.len());
        for (i, word) in expected.iter().enumerate() {
            let got = i32::from_le_bytes(actual[i * 4..i * 4 + 4].try_into().unwrap()) as i64;
            assert!(got == i64::from(*word), "quad index {i}: device {got} vs host {word}");
        }
        if let Some(h) = host_ {
            let (normals, release) = host::normals(h, &p, point_count as i64, &Arc::new(expected)).expect("normals");
            let bytes = b::read_buffer(&normals, 0, point_count as i64 * 12).unwrap();
            let f = |at: usize| f32::from_le_bytes(bytes[at..at + 4].try_into().unwrap());
            for i in 0..point_count {
                assert!(f(i * 12) == 0.0 && f(i * 12 + 4) == 0.0 && f(i * 12 + 8) == 1.0);
            }
            release();
        }
    } else {
        assert!(matches!(&result, Err(d) if d.code == "E_GPU_DISPLAY_TRIANGLES"));
        assert_eq!(reads.get(), if copies == 0 || corners > 4 { 0 } else { 4 });
    }
    for x in buffers.borrow().iter() {
        b::destroy_buffer(x).expect("destroy");
    }
}

fn square() -> Shape {
    vec![(0., 0., 0.), (2., 0., 0.), (2., 2., 0.), (0., 2., 0.)]
}
fn concave() -> Shape {
    vec![(0., 0., 0.), (2., 0., 0.), (0.5, 0.5, 0.), (0., 2., 0.)]
}

/// the host wrapper's cases
fn hosted() {
    if !common::gpu() {
        return;
    }
    let lease = common::lease();
    let pipes = pipelines::create(&|| 0.0, &renderer::renderer::gpu_device(&lease));
    let h = host::create(&|| 0.0, &lease);
    run_case(&lease, &pipes, Some(&h), case(1, true), &square());
    run_case(&lease, &pipes, Some(&h), case(1, true), &concave());
    run_case(&lease, &pipes, Some(&h), case(2, true), &square());
    run_case(&lease, &pipes, Some(&h), case(1, true), &square());
    run_case(&lease, &pipes, Some(&h), Case { kind: 1, ..case(1, false) }, &square());
    run_case(&lease, &pipes, Some(&h), Case { bad_vertex: true, ..case(1, false) }, &square());
    run_case(&lease, &pipes, Some(&h), case(0, false), &square());
    run_case(&lease, &pipes, Some(&h), case(1, false), &vec![(0., 0., 0.), (0., 0., 0.), (2., 2., 0.), (0., 2., 0.)]);
    host::close(&h);
    pipelines::close(&pipes);
}

/// winding, rotation and projection of three shapes
fn orientations() {
    if !common::gpu() {
        return;
    }
    let lease = common::lease();
    let pipes = pipelines::create(&|| 0.0, &renderer::renderer::gpu_device(&lease));
    for shape in [square(), concave(), vec![(0., 0., 0.), (2., 0., 0.), (2., 2., 0.25), (0., 2., 0.)]] {
        for rotation in 0..4 {
            let rotated: Shape = (0..4).map(|i| shape[(i + rotation) % 4]).collect();
            let reversed: Shape = (0..4).map(|i| rotated[3 - i]).collect();
            for s in [rotated, reversed] {
                let projections: [fn((f64, f64, f64)) -> (f64, f64, f64); 3] = [|p| p, |(x, y, z)| (y, z, x), |(x, y, z)| (z, x, y)];
                for project in projections {
                    run_case(&lease, &pipes, None, case(1, true), &s.iter().map(|p| project(*p)).collect());
                }
            }
        }
    }
    pipelines::close(&pipes);
}

/// partial groups and the two-pass reduction
fn groups() {
    if !common::gpu() {
        return;
    }
    let lease = common::lease();
    let pipes = pipelines::create(&|| 0.0, &renderer::renderer::gpu_device(&lease));
    for copies in [255, 256, 257, 1025, 65537] {
        run_case(&lease, &pipes, None, case(copies, true), &square());
    }
    run_case(&lease, &pipes, None, case(257, true), &vec![(0., 0., 0.), (1., 0., 0.), (0., 1., 0.)]);
    run_case(&lease, &pipes, None, Case { mixed: true, ..case(1025, true) }, &square());
    pipelines::close(&pipes);
}

/// conservative fallback
fn refusals() {
    if !common::gpu() {
        return;
    }
    let lease = common::lease();
    let pipes = pipelines::create(&|| 0.0, &renderer::renderer::gpu_device(&lease));
    run_case(&lease, &pipes, None, case(0, false), &square());
    run_case(&lease, &pipes, None, case(1, false), &vec![(0., 0., 0.), (0., 0., 0.), (2., 2., 0.), (0., 2., 0.)]);
    run_case(&lease, &pipes, None, case(1, false), &square().iter().map(|(x, y, _)| (*x, *y, *y)).collect());
    run_case(&lease, &pipes, None, case(1, false), &square().iter().map(|(x, y, z)| (x + 10000., *y, *z)).collect());
    for x in [f64::NAN, f64::INFINITY] {
        run_case(&lease, &pipes, None, case(1, false), &vec![(x, 0., 0.), (2., 0., 0.), (2., 2., 0.), (0., 2., 0.)]);
    }
    pipelines::close(&pipes);
}

pub const CASES: &[(&str, fn())] = &[("hosted", hosted), ("orientations", orientations), ("groups", groups), ("refusals", refusals)];
