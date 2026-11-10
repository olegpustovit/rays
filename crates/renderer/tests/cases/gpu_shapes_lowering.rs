//! : published batch pixels and exact allocation ceilings.
use crate::common;
use ::renderer::renderer::{self, native};
use scene_command::render_ir::{self as ir, Blend, Command};
use scene_command::ui_batch::Builder;
use std::rc::Rc;

const WIDTH: i64 = 64;
const HEIGHT: i64 = 40;
const COUNT: i64 = 100_000;

fn rgba(r: i64, g: i64, b: i64, a: i64) -> i32 {
    (((r & 255) << 24) | ((g & 255) << 16) | ((b & 255) << 8) | (a & 255)) as u32 as i32
}

fn gpu_shapes_lowering() {
    if !common::gpu() {
        return;
    }
    let execution = renderer::create_offscreen(&renderer::Configuration {
        logical_width: WIDTH,
        logical_height: HEIGHT,
        drawable_width: WIDTH,
        drawable_height: HEIGHT,
        title: "shapes-lowering".to_owned(),
        vsync: false,
        high_density: true,
    }).unwrap();
    let positions: Vec<f64> = (0..2 * COUNT)
        .map(|k| if k % 2 == 0 { ((k / 2) % WIDTH) as f64 + 0.5 } else { ((k / 2 / WIDTH) % HEIGHT) as f64 + 0.5 })
        .collect();
    let mut builder = Builder::with_capacity(COUNT);
    builder.circles(COUNT, &positions, 2, &[0.6], 0, rgba(118, 186, 202, 255), 0, &[1.0], 0).expect("circles");
    let ir_ = Rc::new(ir::create_owned(vec![Command::SetBlend(Blend::Alpha), Command::Shapes(builder.publish().expect("publish"))]).expect("the batch is not a scene"));
    let frame = || -> Vec<u8> {
        let submission = renderer::begin_submission(&execution).unwrap();
        let batch = native::lower_scene2(&submission, 1, &|_| None, &ir_).unwrap();
        let stepped = native::step(Some((0.0, 0.0, 0.0, 1.0)), None, None, &submission, &[batch]);
        native::cancel(&submission);
        stepped.unwrap();
        renderer::capture(&execution).unwrap()
    };
    let before=mem_sys::alloc_bytes_total();
    let pixels = frame();
    let first=mem_sys::alloc_bytes_total()-before;
    let before=mem_sys::alloc_bytes_total();
    let again = frame();
    let second=mem_sys::alloc_bytes_total()-before;
    let painted = pixels.iter().enumerate().filter(|(i, v)| i % 4 == 1 && **v != 0).count() as i64;
    assert!(painted == WIDTH * HEIGHT && pixels == again, "painted {} of {}", painted, WIDTH * HEIGHT);
    assert!(first <= 32*COUNT as u64,"first frame allocated {first} bytes for {COUNT} instances (ceiling 32 B/instance)");
    assert!(second <= COUNT as u64,"unchanged frame allocated {second} bytes for {COUNT} instances (ceiling 1 B/instance)");
    renderer::destroy(&execution).unwrap();
    println!("Shapes lowering: a {}-instance batch painted {} pixels, identical on the second frame", COUNT, painted);
}

pub const CASES: &[(&str, fn())] = &[("gpu_shapes_lowering", gpu_shapes_lowering)];
