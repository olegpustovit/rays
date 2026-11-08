// The PXUI instance pipeline must light exactly the pixels the Scene2 triangle path lights for the
// design kit's square fills, 1-point strokes, and axis-aligned marker lines, at 1x and 2x density.
use crate::common;
use ::renderer::renderer::{self, native};
use scene_command::render_ir::{self, Command};
use scene_command::{shape2, ui_batch};
use std::rc::Rc;

fn rgba(r: u32, g: u32, b: u32, a: u32) -> i32 {
    ((r << 24) | (g << 16) | (b << 8) | a) as i32
}

const SHAPES: [(i64, i64, i64, i64); 3] = [(3, 4, 20, 9), (30, 2, 17, 15), (5, 20, 40, 11)];

fn new_batch(fill: i32, stroke: i32, accent: i32, marker: i32) -> ui_batch::Table {
    let mut b = ui_batch::Builder::create();
    for (x, y, w, h) in SHAPES {
        let (x, y, w, h) = (x as f64, y as f64, w as f64, h as f64);
        b.rect(x, y, w, h, fill, 0, 0.0, 0.0, false).unwrap();
        b.rect(x, y, w, h, 0, stroke, 1.0, 0.0, false).unwrap();
    }
    b.rect(5.0, 24.0, 17.0, 3.0, accent, 0, 0.0, 0.0, false).unwrap();
    // A width-2 line is a 2-point rect centred on its axis.
    b.rect(21.0, 22.0, 2.0, 7.0, marker, 0, 0.0, 0.0, false).unwrap();
    b.rect(51.5, 5.0, 1.0, 25.0, stroke, 0, 0.0, 0.0, false).unwrap();
    b.rect(49.0, 17.5, 11.0, 1.0, stroke, 0, 0.0, 0.0, false).unwrap();
    b.publish().unwrap()
}

fn old_scene(fill: i32, stroke: i32, accent: i32, marker: i32) -> Rc<render_ir::RenderIr> {
    let mut geometries = Vec::new();
    let rect = |(x, y, w, h): (i64, i64, i64, i64), f: Option<i32>, s: Option<i32>| {
        let (x, y, w, h) = (x as f64, y as f64, w as f64, h as f64);
        shape2::polygon(&[(x, y), (x + w, y), (x + w, y + h), (x, y + h)], f, s).unwrap()
    };
    for bounds in SHAPES {
        geometries.extend(rect(bounds, Some(fill), Some(stroke)));
    }
    geometries.extend(rect((5, 24, 17, 3), Some(accent), None));
    geometries.push(shape2::line((22.0, 22.0), (22.0, 29.0), 2.0, marker).unwrap());
    geometries.push(shape2::line((52.0, 5.0), (52.0, 30.0), 1.0, stroke).unwrap());
    geometries.push(shape2::line((49.0, 18.0), (60.0, 18.0), 1.0, stroke).unwrap());
    Rc::new(render_ir::create(geometries.into_iter().map(Command::Geometry).collect()).unwrap())
}

// Retain the original comparison's allowance for the rectangle corner squares.
fn band_corner(density: i64, (x, y): (i64, i64)) -> bool {
    let px = (x as f64 + 0.5) / density as f64;
    let py = (y as f64 + 0.5) / density as f64;
    SHAPES.iter().any(|&(sx, sy, w, h)| {
        let near = |edge: i64, value: f64| (value - edge as f64).abs() <= 0.5;
        (near(sx, px) || near(sx + w, px)) && (near(sy, py) || near(sy + h, py))
    })
}

fn first_difference(density: i64, width: i64, left: &[u8], right: &[u8]) -> Option<(i64, i64)> {
    for (index, byte) in left.iter().enumerate() {
        let pixel = index as i64 / 4;
        let point = (pixel % width, pixel / width);
        if *byte != right[index] && !band_corner(density, point) {
            return Some(point);
        }
    }
    None
}

fn run(density: i64) {
    if !common::gpu() {
        return;
    }
    let (fill, stroke) = (rgba(220, 227, 222, 255), rgba(34, 43, 43, 180));
    let (accent, marker) = (rgba(40, 95, 119, 175), rgba(34, 43, 43, 255));
    let scene = old_scene(fill, stroke, accent, marker);
    let ui = new_batch(fill, stroke, accent, marker);
    let cfg = renderer::Configuration { logical_width: 64, logical_height: 40, drawable_width: 64 * density, drawable_height: 40 * density, title: "ui-pipeline".into(), vsync: false, high_density: true };
    let execution = renderer::create_offscreen(&cfg).unwrap();
    let capture = |lower: &dyn Fn(&renderer::Submission) -> Result<renderer::Batch, ogpu::error::Error>| {
        let submission = native::begin_submission(&execution).unwrap();
        let result = (|| {
            let batch = lower(&submission)?;
            native::step(Some((1.0, 1.0, 1.0, 1.0)), None, None, &submission, &[batch])?;
            renderer::capture(&execution)
        })();
        native::cancel(&submission);
        result.unwrap()
    };
    let old = capture(&|s| native::lower_scene2(s, density, &|_| None, &scene));
    let current = capture(&|s| native::lower_ui(s, density, &|_| None, &ui));
    let width = cfg.drawable_width;
    let length = (width * cfg.drawable_height * 4) as usize;
    assert!(old.len() == length && current.len() == length);
    let painted = current.iter().enumerate().filter(|(i, c)| i % 4 != 3 && **c != 255).count();
    assert!(painted > 100);
    if let Some((x, y)) = first_difference(density, width, &old, &current) {
        panic!("UI pipeline differs from Scene2 at {density}x pixel ({x},{y})");
    }
    println!("UI pipeline matches Scene2 at {density}x");
    renderer::destroy(&execution).unwrap();
}

fn gpu_ui_pipeline_1x() {
    run(1)
}

fn gpu_ui_pipeline_2x() {
    run(2)
}

pub const CASES: &[(&str, fn())] = &[("gpu_ui_pipeline_1x", gpu_ui_pipeline_1x), ("gpu_ui_pipeline_2x", gpu_ui_pipeline_2x)];
