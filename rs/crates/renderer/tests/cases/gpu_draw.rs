// GPU marks: exact native parity under transform, clip, six blends, Retina and stale
// output/update/lease guards. One test fn per density.
use crate::common;
use ::renderer::renderer::{self, marks_source, Native};
use ::renderer::scene_execution::{self, new_bytes};
use ogpu::backend as b;
use ogpu::error::ErrorKind;
use ogpu::types::{BufferDescriptor, BufferUsage};
use scene_command::marks;
use scene_command::render_ir::{self, Blend, Command, Rect, RenderIr, Transform};
use scene_command::ui_batch::{self, Table};
use std::cell::Cell;
use std::rc::Rc;

fn run(density: i64) {
    if !common::gpu() {
        return;
    }
    let live = || ogpu::impl_::live_handles().unwrap();
    let baseline = live();
    let cfg = renderer::configuration { logical_width: 32, logical_height: 24, drawable_width: 32 * density, drawable_height: 24 * density, title: "GPU drawing parity".into(), vsync: false, high_density: true };
    let execution = common::get(renderer::create_offscreen(&cfg));
    let gpu = common::get(renderer::acquire_gpu());
    let sink = common::get(renderer::create_gpu_marks(&gpu));
    let positions = common::get(b::create_buffer(
        &renderer::gpu_device(&gpu),
        BufferDescriptor { label: Some("GPU parity points".into()), size: 24, usage: vec![BufferUsage::Vertex, BufferUsage::Storage, BufferUsage::CopyDst] },
        None,
    ));
    let points: [f64; 6] = [8.0, 8.0, 0.0, 20.0, 14.0, 0.0];
    let point32: Vec<f32> = points.iter().map(|v| *v as f32).collect();
    common::get(b::write_buffer(&positions, 0, &common::floats(&point32)));
    let generation = Rc::new(Cell::new(1));
    // A device source is current only while its producer's output is.
    let source = |stamp: i32| -> marks_source {
        let (g, p) = (generation.clone(), positions.clone());
        marks_source::Device(Rc::new(move || if g.get() == stamp { Some(p.clone()) } else { None }), 3)
    };
    let attrs = |stamp: i32, fill: u32| -> Vec<marks_source> {
        vec![
            source(stamp),
            marks_source::Host(new_bytes(common::floats(&[2.5])), 0),
            marks_source::Host(new_bytes(common::words(&[fill as i32])), 0),
            marks_source::Host(new_bytes(common::words(&[0xff00ff80u32 as i32])), 0),
            marks_source::Host(new_bytes(common::floats(&[3.0])), 0),
        ]
    };
    let marks_ = |stamp: i32, fill: u32| renderer::gpu_marks_token(None, &sink, 4, 2, &attrs(stamp, fill));
    let expected = |fill: u32| -> Table {
        let mut bld = ui_batch::Builder::create();
        for index in 0..2 {
            bld.circle(points[index * 3], points[index * 3 + 1], 2.5, Some(fill as i32), Some(0xff00ff80u32 as i32), 3.0).unwrap();
        }
        bld.publish().unwrap()
    };
    let expected_shapes = expected(0x12345678);
    let first = common::get(marks_(generation.get(), 0x12345678));
    // Nothing changed: the same token, not a new registration.
    assert!(common::get(marks_(generation.get(), 0x12345678)) == first);
    let ir = |blend: Blend, shapes: &Table| -> Rc<RenderIr> {
        Rc::new(
            render_ir::create(vec![
                Command::SetBlend(blend),
                Command::PushClip(Rect { x: 2.0, y: 3.0, width: 25.0, height: 17.0 }),
                Command::PushTransform(Transform { xx: 0.875, xy: -0.2, yx: 0.15, yy: 1.125, tx: 0.375, ty: 0.125 }),
                Command::Shapes(shapes.copy()),
                Command::PopTransform,
                Command::PopClip,
            ])
            .unwrap(),
        )
    };
    let clear = (0.125, 0.25, 0.375, 1.0);
    let capture = |identity: Option<&str>, version: Option<i64>, ir: &Rc<RenderIr>| -> Vec<u8> {
        let submission = common::get(Native::begin_submission(&execution));
        let result = (|| {
            let batch = Native::lower_scene2(&submission, density, &|_| None, ir)?;
            Native::step(Some(clear), identity, version, &submission, &[batch])?;
            renderer::capture(&execution)
        })();
        Native::cancel(&submission);
        let pixels = common::get(result);
        assert_eq!(pixels.len() as i64, 32 * 24 * density * density * 4);
        pixels
    };
    let shapes = ui_batch::of_marks(first);
    for blend in [Blend::Replace, Blend::Alpha, Blend::Add, Blend::Multiply, Blend::Screen, Blend::Subtract] {
        let reference = capture(None, None, &ir(blend, &expected_shapes));
        if blend == Blend::Replace {
            let at = (((10 * density * 32 * density) + (6 * density)) * 4) as usize;
            assert!(reference[0..4] != reference[at..at + 4]);
        }
        for _ in 0..3 {
            assert!(capture(None, None, &ir(blend, &shapes)) == reference);
        }
    }
    let retained = |token: marks::GpuToken| capture(Some("gpu-marks"), Some(marks::gpu_stamp(token)), &ir(Blend::Replace, &ui_batch::of_marks(token)));
    retained(first);
    assert!(common::get(Native::replay(Some(clear), "gpu-marks", marks::gpu_stamp(first), &execution)) == Some(()));
    let refuse_replay = |token: marks::GpuToken| match Native::replay(Some(clear), "gpu-marks", marks::gpu_stamp(token), &execution) {
        Err(e) if e.kind == ErrorKind::StaleHandle && e.operation == "Scene_execution.replay" && e.message == "GPU vertices are stale or closed" => {}
        _ => panic!("retained GPU mark replay accepted a stale source"),
    };
    let refuse_lower = |token: marks::GpuToken| {
        let submission = common::get(Native::begin_submission(&execution));
        let result = Native::lower_scene2(&submission, density, &|_| None, &ir(Blend::Replace, &ui_batch::of_marks(token)));
        Native::cancel(&submission);
        match result {
            Err(e) if e.kind == ErrorKind::InvalidState && e.operation == "Renderer.lower_scene2" && e.message == "GPU shapes are stale or closed" => {}
            _ => panic!("GPU mark lowering accepted a stale token"),
        }
    };
    // The producer's output is overwritten: its source returns None, so the token is stale and a new
    // registration refuses until the source is current again.
    generation.set(generation.get() + 1);
    refuse_replay(first);
    refuse_lower(first);
    match renderer::gpu_marks_token(None, &sink, 4, 2, &attrs(1, 0x12345678)) {
        Err(e) if e.kind == ErrorKind::StaleHandle => {}
        _ => panic!("a stale device source was registered"),
    }
    let second = common::get(marks_(generation.get(), 0x12345678));
    assert!(marks::gpu_stamp(first) < marks::gpu_stamp(second));
    let reference = capture(None, None, &ir(Blend::Replace, &expected_shapes));
    assert!(retained(second) == reference);
    // A host attribute that changes supersedes the previous token.
    let third = common::get(marks_(generation.get(), 0x87654321));
    assert!(marks::gpu_stamp(second) < marks::gpu_stamp(third));
    refuse_replay(second);
    refuse_lower(second);
    assert!(retained(third) == capture(None, None, &ir(Blend::Replace, &expected(0x87654321))));
    assert!(retained(third) != reference);
    // A mark set that is not a valid description is refused before it registers.
    let empty = vec![
        marks_source::Host(new_bytes(common::floats(&[0.0])), 0),
        marks_source::Host(new_bytes(common::floats(&[0.0])), 0),
        marks_source::Host(new_bytes(common::words(&[0])), 0),
        marks_source::Host(new_bytes(common::words(&[0])), 0),
        marks_source::Host(new_bytes(common::floats(&[0.0])), 0),
    ];
    match renderer::gpu_marks_token(None, &sink, 4, 0, &empty) {
        Err(e) if e.kind == ErrorKind::InvalidArgument => {}
        _ => panic!("an empty mark set was registered"),
    }
    renderer::release_gpu(&gpu);
    refuse_replay(third);
    refuse_lower(third);
    assert_eq!(scene_execution::Private::gpu_vertex_count_for_test(&Native::gpu_vertices_owner(&execution)), 0);
    renderer::close_gpu_marks(&sink);
    let _ = b::destroy_buffer(&positions);
    renderer::release_gpu(&gpu);
    common::get(renderer::destroy(&execution));
    assert_eq!(live(), baseline);
    println!("GPU marks: exact native parity at {density}x under transform, clip, six blends and stale guards");
}

fn gpu_draw_1x() {
    run(1)
}

fn gpu_draw_2x() {
    run(2)
}

pub const CASES: &[(&str, fn())] = &[("gpu_draw_1x", gpu_draw_1x), ("gpu_draw_2x", gpu_draw_2x)];
