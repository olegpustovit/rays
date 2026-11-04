//! Port of lib/renderer/test_scene2_coordinates.ml: 1x and 2x density as separate test fns.
use crate::common;
use crate::common::get;
use renderer::renderer;
use ::renderer::renderer::Native;
use scene_command::render_ir::{self, Command, Geometry, Rect};
use std::rc::Rc;

fn px(bytes: &[u8], width: i64, x: i64, y: i64) -> (u8, u8, u8, u8) {
    let o = ((y * width + x) * 4) as usize;
    (bytes[o], bytes[o + 1], bytes[o + 2], bytes[o + 3])
}

fn filled(p: (u8, u8, u8, u8)) -> bool {
    p.0 > 200 && p.1 > 200 && p.2 > 200
}

fn bbox(bytes: &[u8], width: i64, height: i64) -> (i64, i64, i64, i64) {
    let (mut min_x, mut min_y, mut max_x, mut max_y) = (width, height, -1, -1);
    for y in 0..height {
        for x in 0..width {
            if filled(px(bytes, width, x, y)) {
                min_x = min_x.min(x);
                min_y = min_y.min(y);
                max_x = max_x.max(x);
                max_y = max_y.max(y);
            }
        }
    }
    assert!(max_x >= 0, "expected filled pixels");
    (min_x, min_y, max_x - min_x + 1, max_y - min_y + 1)
}

const WHITE: i32 = -1;

fn square_geometry() -> Geometry {
    Geometry { vertices: vec![8., 8., 24., 8., 24., 24., 8., 24.], indices: vec![0, 1, 2, 0, 2, 3], color: WHITE }
}

fn circle_geometry() -> Geometry {
    let (cx, cy, radius, steps) = (32., 16., 8., 32usize);
    let mut vertices = vec![0.; (steps + 1) * 2];
    vertices[0] = cx;
    vertices[1] = cy;
    for i in 0..steps {
        let angle = i as f64 * (2. * std::f64::consts::PI) / steps as f64;
        vertices[(i + 1) * 2] = cx + radius * angle.cos();
        vertices[(i + 1) * 2 + 1] = cy + radius * angle.sin();
    }
    let indices = (0..steps * 3)
        .map(|i| match i % 3 {
            0 => 0,
            1 => 1 + (i / 3) as i64,
            _ => {
                let next = 2 + (i / 3) as i64;
                if next > steps as i64 { 1 } else { next }
            }
        })
        .collect();
    Geometry { vertices, indices, color: WHITE }
}

fn expect_square(bytes: &[u8], w: i64, h: i64, x: i64, y: i64, size: i64, message: &str) {
    let got = bbox(bytes, w, h);
    assert!(got == (x, y, size, size), "{message}: expected square {size}x{size} at ({x},{y}), got {}x{} at ({},{})", got.2, got.3, got.0, got.1);
}

fn run(lw: i64, lh: i64, dw: i64, dh: i64, scale: i64) {
    if !common::gpu() {
        return;
    }
    let cfg = renderer::configuration { logical_width: lw, logical_height: lh, drawable_width: dw, drawable_height: dh, title: "scene2-coordinates".into(), vsync: false, high_density: true };
    let e = get(renderer::create_offscreen(&cfg));
    let facts = get(renderer::presentation_facts_of(&e));
    assert!(facts.logical_width == lw && facts.logical_height == lh && facts.drawable_width == dw && facts.drawable_height == dh, "offscreen logical/drawable facts");
    let render = |commands: Vec<Command>| {
        let ir = Rc::new(render_ir::create(commands).expect("scene2 coordinate IR"));
        let s = get(Native::begin_submission(&e));
        let batch = get(Native::lower_scene2(&s, scale, &|_| None, &ir));
        get(Native::step(Some((0., 0., 0., 1.)), None, None, &s, &[batch]));
        let pixels = get(renderer::capture(&e));
        assert_eq!(pixels.len() as i64, dw * dh * 4, "coordinate readback length");
        pixels
    };
    let rect = |x, y, width, height| Rect { x, y, width, height };
    let square = render(vec![Command::Geometry(square_geometry())]);
    expect_square(&square, dw, dh, 8 * scale, 8 * scale, 16 * scale, "logical square");
    let clipped = render(vec![Command::PushClip(rect(12., 12., 8., 8.)), Command::Geometry(square_geometry()), Command::PopClip]);
    expect_square(&clipped, dw, dh, 12 * scale, 12 * scale, 8 * scale, "logical clip");
    // The square at (8,8) lies outside this tall clip: a clip used as a viewport would remap it.
    let tall = rect(40., 0., 24., 32.);
    let outside = render(vec![Command::PushClip(tall), Command::Geometry(square_geometry()), Command::PopClip]);
    let mut leaked = 0;
    for y in 0..dh {
        for x in 0..dw {
            if px(&outside, dw, x, y) != (0, 0, 0, 255) {
                leaked += 1;
            }
        }
    }
    assert_eq!(leaked, 0, "{scale}x clip used as viewport leaked out-of-clip fill");
    let inside = render(vec![
        Command::PushClip(tall),
        Command::Geometry(Geometry { vertices: vec![44., 8., 60., 8., 60., 24., 44., 24.], indices: vec![0, 1, 2, 0, 2, 3], color: WHITE }),
        Command::PopClip,
    ]);
    expect_square(&inside, dw, dh, 44 * scale, 8 * scale, 16 * scale, &format!("{scale}x clipped square"));
    let circle = render(vec![Command::Geometry(circle_geometry())]);
    expect_square(&circle, dw, dh, 24 * scale, 8 * scale, 16 * scale, "logical circle bounds");
    assert!(filled(px(&circle, dw, 32 * scale, 16 * scale)), "circle center unfilled");
    assert!(!filled(px(&circle, dw, 24 * scale, 8 * scale)), "circle bounding-box corner filled");
    get(renderer::destroy(&e));
}

fn gpu_scene2_coordinates_1x() {
    run(64, 32, 64, 32, 1);
}

fn gpu_scene2_coordinates_2x() {
    run(64, 32, 128, 64, 2);
}

pub const CASES: &[(&str, fn())] = &[("gpu_scene2_coordinates_1x", gpu_scene2_coordinates_1x), ("gpu_scene2_coordinates_2x", gpu_scene2_coordinates_2x)];
