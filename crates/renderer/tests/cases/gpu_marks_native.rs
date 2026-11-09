//! The Marks pipeline family must light exactly the pixels the Ui_batch path (Ui family, 64-byte
//! instances) lights for the same marks: rect, circle and line, with and without a stroke, at 63, 64,
//! 65 and 100,000 instances, at 1x and 2x density. Inputs are multiples of 1/8 so that binary32 sums
//! such as x + width are exact; see specification/marks.md. Every channel is compared.
//! The OCaml loop over (density, kind) is split into one test fn per pair so each stays under 5 s.
use crate::common;
use ogpu_core::backend::{self, Buffer, Device};
use ogpu_core::pipeline::Blend;
use ogpu_core::render_pass::{Comparison, Cull, Load, Primitive};
use ogpu_core::types::{BufferDescriptor, BufferUsage};
use ::renderer::renderer::{self, native};
use ::renderer::scene_execution::{self, GeometryDraw, Mesh, MeshKey, new_bytes, PipelineFamily, State};
use scene_command::render_ir::{self, Command};
use scene_command::ui_batch::{Builder, Table};
use std::rc::Rc;
use std::sync::atomic::{AtomicI64, Ordering};

const WIDTH: i64 = 64;
const HEIGHT: i64 = 40;

fn eighth(n: i64) -> f64 {
    n as f64 / 8.0
}

fn rgba(r: i64, g: i64, b: i64, a: i64) -> i32 {
    (((r & 255) << 24) | ((g & 255) << 16) | ((b & 255) << 8) | (a & 255)) as u32 as i32
}

fn fill_word(i: i64) -> i32 {
    rgba(i * 13, i * 29 + 60, i * 7 + 120, 40 + (i * 11 % 200))
}
fn stroke_word(i: i64) -> i32 {
    rgba(i * 3 + 200, i * 5 + 17, i * 19, 60 + (i * 7 % 190))
}
fn x(i: i64) -> f64 {
    eighth(i * 37 % 512) - 2.0
}
fn y(i: i64) -> f64 {
    eighth(i * 53 % 320) - 2.0
}
fn rect_size(i: i64) -> (f64, f64) {
    (eighth(2 + i * 5 % 48), eighth(2 + i * 3 % 40))
}
fn radius(i: i64) -> f64 {
    eighth(2 + i % 38)
}
fn circle_width(i: i64) -> f64 {
    1.0 + (i % 3) as f64
}
fn line_end(i: i64) -> (f64, f64) {
    (x(i) + eighth(i * 11 % 64 - 32), y(i) + eighth(i * 17 % 48 - 24))
}
fn line_width(i: i64) -> f64 {
    (1 + i % 4) as f64 / 2.0
}

#[derive(Clone, Copy, PartialEq)]
enum Kind {
    Rect,
    Circle,
    Line,
}

fn name(kind: Kind) -> &'static str {
    match kind {
        Kind::Rect => "rect",
        Kind::Circle => "circle",
        Kind::Line => "line",
    }
}

struct Attributes {
    position: Vec<f64>,
    extent: Vec<f64>,
    fill: Vec<u8>,
    stroke: Vec<i32>,
    width: Vec<f64>,
    params: Vec<i32>,
    rgb: bool,
}

fn floats(a: &[f64]) -> Vec<u8> {
    a.iter().flat_map(|v| (*v as f32).to_le_bytes()).collect()
}

fn words(a: &[i32]) -> Vec<u8> {
    a.iter().flat_map(|v| v.to_le_bytes()).collect()
}

/// `styled`: a stroke (rect, circle) or per-instance colour and width (line). `rgb`: float fill colours.
fn attributes(kind: Kind, styled: bool, rgb: bool, n: i64) -> Attributes {
    let position: Vec<f64> = (0..3 * n)
        .map(|k| match k % 3 {
            0 => x(k / 3),
            1 => y(k / 3),
            _ => 0.0,
        })
        .collect();
    let extent: Vec<f64> = match kind {
        Kind::Rect => (0..2 * n)
            .map(|k| {
                let (w, h) = rect_size(k / 2);
                if k % 2 == 0 { w } else { h }
            })
            .collect(),
        Kind::Circle => (0..n).map(radius).collect(),
        Kind::Line => (0..3 * n)
            .map(|k| {
                let (ex, ey) = line_end(k / 3);
                match k % 3 {
                    0 => ex,
                    1 => ey,
                    _ => 0.0,
                }
            })
            .collect(),
    };
    let channel = |word: i32, shift: i64| ((((word as u32) >> shift) & 255) as f64) / 255.0;
    let fill: Vec<u8> = if rgb {
        floats(&(0..3 * n).map(|k| channel(fill_word(k / 3), 24 - 8 * (k % 3))).collect::<Vec<f64>>())
    } else {
        words(&(0..n).map(fill_word).collect::<Vec<i32>>())
    };
    let stroke: Vec<i32> = match (kind, styled) {
        (Kind::Rect, false) | (Kind::Circle, false) => vec![0],
        (_, true) => (0..n).map(stroke_word).collect(),
        (Kind::Line, false) => vec![rgba(90, 200, 40, 220)],
    };
    let width: Vec<f64> = match (kind, styled) {
        (Kind::Rect, _) => vec![1.0],
        (Kind::Circle, false) => vec![1.0],
        (Kind::Circle, true) => (0..n).map(circle_width).collect(),
        (Kind::Line, false) => vec![1.5],
        (Kind::Line, true) => (0..n).map(line_width).collect(),
    };
    let kind_word = match kind {
        Kind::Rect => 0,
        Kind::Circle => 4,
        Kind::Line => 5,
    };
    let stride_of = |count: usize| if count == 1 { 0 } else { 1 };
    let params = vec![
        kind_word,
        3,
        match kind {
            Kind::Rect => 2,
            Kind::Circle => 1,
            Kind::Line => 3,
        },
        if rgb { 3 } else { 1 },
        stride_of(stroke.len()),
        stride_of(width.len()),
        if rgb { 1 } else { 0 },
        0,
    ];
    Attributes { position, extent, fill, stroke, width, params, rgb }
}

fn batch(kind: Kind, a: &Attributes, n: i64) -> Table {
    let mut b = Builder::with_capacity(n);
    let fill = |i: i64| if a.rgb { fill_word(i) | 255 } else { fill_word(i) };
    let stroke_at = |i: i64| if a.stroke.len() == 1 { a.stroke[0] } else { a.stroke[i as usize] };
    for i in 0..n {
        match kind {
            Kind::Rect => {
                let (w, h) = rect_size(i);
                b.shape_rect(x(i), y(i), w, h, fill(i), stroke_at(i)).unwrap();
            }
            Kind::Circle => {
                let sw = if a.width.len() == 1 { a.width[0] } else { circle_width(i) };
                b.circle(x(i), y(i), radius(i), Some(fill(i)), Some(stroke_at(i)), sw).unwrap();
            }
            Kind::Line => {
                let (ex, ey) = line_end(i);
                let w = if a.width.len() == 1 { a.width[0] } else { line_width(i) };
                b.line(x(i), y(i), ex, ey, stroke_at(i), w).unwrap();
            }
        }
    }
    b.publish().unwrap()
}

fn quad_indices(n: i64) -> Vec<u8> {
    let mut out = Vec::with_capacity(24 * n as usize);
    for quad in 0..n {
        for corner in [0i64, 1, 2, 0, 2, 3] {
            out.extend_from_slice(&((4 * quad + corner) as i32).to_le_bytes());
        }
    }
    out
}

static COUNTER: AtomicI64 = AtomicI64::new(0);

fn upload(device: &Device, label: &str, bytes: &[u8]) -> Buffer {
    let buffer = backend::create_buffer(
        device,
        BufferDescriptor {
            label: Some(label.to_owned()),
            size: bytes.len() as i64,
            usage: vec![BufferUsage::Vertex, BufferUsage::Storage, BufferUsage::CopyDst],
        },
        None,
    ).unwrap();
    backend::write_buffer(&buffer, 0, bytes).unwrap();
    buffer
}

/// A Marks draw over `a`'s buffers and a closure releasing them. `drawn`: a count record as a fast
/// kernel writes it, {n, 6n, 1, 0, 0, 0}, the draw's from byte 4.
fn marks_draw(
    owner: &scene_execution::GpuVerticesOwner,
    device: &Device,
    a: &Attributes,
    n: i64,
    drawn: Option<i32>,
) -> (scene_execution::Draw, Box<dyn FnOnce()>) {
    let mut buffers: Vec<Buffer> = Vec::new();
    let contents = [
        ("position", floats(&a.position)),
        ("extent", floats(&a.extent)),
        ("fill", a.fill.clone()),
        ("stroke", words(&a.stroke)),
        ("width", floats(&a.width)),
        ("params", words(&a.params)),
    ];
    for (label, bytes) in contents.iter() {
        buffers.push(upload(device, label, bytes));
    }
    let key = format!("marks-native-{}", COUNTER.fetch_add(1, Ordering::SeqCst) + 1);
    let record = drawn.map(|d| upload(device, "count", &words(&[d, 6 * d, 1, 0, 0, 0])));
    if let Some(r) = &record {
        buffers.push(r.clone());
    }
    let source = scene_execution::private::register_gpu_vertices(
        owner,
        Some(buffers[1..6].to_vec()),
        None,
        record.map(|b| (b, 4)),
        None,
        None,
        &key,
        device,
        &buffers[0],
        4 * n,
        Box::new(|| true),
    ).unwrap();
    let affine = floats(&[2.0 / WIDTH as f64, 0.0, -1.0, 0.0, -2.0 / HEIGHT as f64, 1.0]);
    let st = State {
        viewport: (0, 0, WIDTH, HEIGHT),
        scissor: (0, 0, WIDTH, HEIGHT),
        cull: Cull::CullNone,
        depth_compare: Comparison::Always,
        depth_write: false,
        depth_load: Load::Load,
        depth_clear: 1.0,
        transform_uniforms: Some(new_bytes(affine)),
    };
    let m = Mesh {
        key: MeshKey::GpuVertices { name: Rc::new(key), indices: None },
        vertices: scene_execution::empty_bytes(),
        shared_vertices: None,
        vertex_count: 4 * n,
        indices: new_bytes(quad_indices(n)),
        index_count: 6 * n,
        primitive: Primitive::TriangleList,
    };
    let draw = renderer::prepared_draw(PipelineFamily::Marks, Some(Blend::Alpha), None, None, None, None, GeometryDraw { mesh: m, state: st });
    (
        draw,
        Box::new(move || {
            scene_execution::private::unregister_gpu_vertices(&source);
            for b in buffers.iter() {
                let _ = backend::destroy_buffer(b);
            }
        }),
    )
}

const CLEAR: (f64, f64, f64, f64) = (0.125, 0.25, 0.375, 1.0);

fn capture_ui(execution: &renderer::Renderer, density: i64, table: Table) -> Vec<u8> {
    let ir = Rc::new(render_ir::create(vec![Command::SetBlend(render_ir::Blend::Alpha), Command::Shapes(table)]).unwrap());
    let submission = renderer::begin_submission(execution).unwrap();
    let lowered = native::lower_scene2(&submission, density, &|_| None, &ir);
    let result = lowered.and_then(|b| native::step(Some(CLEAR), None, None, &submission, &[b]));
    native::cancel(&submission);
    result.unwrap();
    renderer::capture(execution).unwrap()
}

fn capture_marks(execution: &renderer::Renderer, draw: scene_execution::Draw) -> Vec<u8> {
    let submission = renderer::begin_submission(execution).unwrap();
    let result = native::adopt_draws(&submission, vec![draw]).and_then(|b| native::step(Some(CLEAR), None, None, &submission, &[b]));
    native::cancel(&submission);
    result.unwrap();
    renderer::capture(execution).unwrap()
}

fn painted(pixels: &[u8]) -> usize {
    pixels.iter().enumerate().filter(|(i, v)| i % 4 != 3 && **v != 0).count()
}

fn with_renderer(density: i64, body: &dyn Fn(&renderer::Renderer, &renderer::Gpu)) {
    let baseline = ogpu_core::impl_::live_handles().unwrap();
    let execution = renderer::create_offscreen(&renderer::Configuration {
        logical_width: WIDTH,
        logical_height: HEIGHT,
        drawable_width: WIDTH * density,
        drawable_height: HEIGHT * density,
        title: "marks-native".into(),
        vsync: false,
        high_density: true,
    }).unwrap();
    let gpu = renderer::acquire_gpu().unwrap();
    body(&execution, &gpu);
    renderer::release_gpu(&gpu);
    renderer::destroy(&execution).unwrap();
    assert_eq!(ogpu_core::impl_::live_handles().unwrap(), baseline, "marks native handle delta");
}

fn parity(density: i64, kind: Kind) {
    with_renderer(density, &|execution, gpu| {
        for styled in [false, true] {
            for rgb in [false, true] {
                for n in [63i64, 64, 65, 100_000] {
                    let a = attributes(kind, styled, rgb, n);
                    let reference = capture_ui(execution, density, batch(kind, &a, n));
                    let (draw, release) = marks_draw(&native::gpu_vertices_owner(execution), &renderer::gpu_device(gpu), &a, n, None);
                    let marks = capture_marks(execution, draw);
                    release();
                    if painted(&reference) < 50 && n >= 63 {
                        panic!("marks parity scene is empty");
                    }
                    if reference != marks {
                        let first = reference.iter().zip(marks.iter()).position(|(a, b)| a != b).unwrap();
                        let row = (WIDTH * density) as usize;
                        panic!(
                            "Marks differs from Ui_batch: {} styled={} rgb={} n={} density={} at pixel ({},{}) channel {}",
                            name(kind),
                            styled,
                            rgb,
                            n,
                            density,
                            first / 4 % row,
                            first / 4 / row,
                            first % 4
                        );
                    }
                }
            }
        }
        assert_eq!(scene_execution::private::gpu_vertex_count_for_test(&native::gpu_vertices_owner(execution)), 0);
    });
}

/// An indirect set registered for 100 rects whose device record says 63 draws the first 63, as the
/// host batch of 63 does.
fn indirect(density: i64) {
    with_renderer(density, &|execution, gpu| {
        let a = attributes(Kind::Rect, true, false, 100);
        let reference = capture_ui(execution, density, batch(Kind::Rect, &a, 63));
        let (draw, release) = marks_draw(&native::gpu_vertices_owner(execution), &renderer::gpu_device(gpu), &a, 100, Some(63));
        let marks = capture_marks(execution, draw);
        release();
        assert!(reference == marks, "indirect marks differ from the 63-rect batch at density {density}");
        assert_eq!(scene_execution::private::gpu_vertex_count_for_test(&native::gpu_vertices_owner(execution)), 0);
    });
}

fn gpu_marks_rect_1x() { if common::gpu() { parity(1, Kind::Rect) } }
fn gpu_marks_circle_1x() { if common::gpu() { parity(1, Kind::Circle) } }
fn gpu_marks_line_1x() { if common::gpu() { parity(1, Kind::Line) } }
fn gpu_marks_rect_2x() { if common::gpu() { parity(2, Kind::Rect) } }
fn gpu_marks_circle_2x() { if common::gpu() { parity(2, Kind::Circle) } }
fn gpu_marks_line_2x() { if common::gpu() { parity(2, Kind::Line) } }
fn gpu_marks_indirect_1x() { if common::gpu() { indirect(1) } }
fn gpu_marks_indirect_2x() { if common::gpu() { indirect(2) } }

pub const CASES: &[(&str, fn())] = &[
        ("gpu_marks_rect_1x", gpu_marks_rect_1x),
        ("gpu_marks_circle_1x", gpu_marks_circle_1x),
        ("gpu_marks_line_1x", gpu_marks_line_1x),
        ("gpu_marks_rect_2x", gpu_marks_rect_2x),
        ("gpu_marks_circle_2x", gpu_marks_circle_2x),
        ("gpu_marks_line_2x", gpu_marks_line_2x),
        ("gpu_marks_indirect_1x", gpu_marks_indirect_1x),
        ("gpu_marks_indirect_2x", gpu_marks_indirect_2x),
    ];
