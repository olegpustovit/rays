//! Neighbouring mark sets with one family, blend, clip and transform become one draw, painter order
//! kept: a run of more than 64 CPU sets is one draw (shorter runs keep their own, by design: a
//! changing member must not re-upload its neighbours), and a set drawn by the Marks family never
//! merges with a Ui_batch set but keeps its place in the order. Pixels equal one big batch.
use crate::common;
use ::renderer::renderer::{self, marks_source, Native};
use ::renderer::scene_execution::{self, new_bytes};
use scene_command::render_ir::{self, Command};
use scene_command::ui_batch::{self, Builder, Table};
use std::rc::Rc;

const WIDTH: i64 = 64;
const HEIGHT: i64 = 40;
const PER_SET: i64 = 3;
const RADIUS: f64 = 5.0;

fn rgba(r: i64, g: i64, b: i64, a: i64) -> i32 {
    (((r & 255) << 24) | ((g & 255) << 16) | ((b & 255) << 8) | (a & 255)) as u32 as i32
}

fn word(s: i64) -> i32 {
    rgba(40 + s * 37, 200 - s * 23, s * 71, 255)
}

fn centre(s: i64, j: i64) -> (f64, f64) {
    ((8 + (s * 13 + j * 9) % 48) as f64, (8 + (s * 7 + j * 5) % 24) as f64)
}

fn cpu_set(s: i64) -> Table {
    let mut b = Builder::create();
    for j in 0..PER_SET {
        let (x, y) = centre(s, j);
        b.circle(x, y, RADIUS, Some(word(s)), Some(0), 1.0).unwrap();
    }
    b.publish().unwrap()
}

fn all_sets(sets: &[i64]) -> Table {
    let mut b = Builder::create();
    for s in sets {
        for j in 0..PER_SET {
            let (x, y) = centre(*s, j);
            b.circle(x, y, RADIUS, Some(word(*s)), Some(0), 1.0).unwrap();
        }
    }
    b.publish().unwrap()
}

fn floats(a: &[f64]) -> scene_execution::bytes {
    new_bytes(a.iter().flat_map(|v| (*v as f32).to_le_bytes()).collect())
}

fn words(a: &[i32]) -> scene_execution::bytes {
    new_bytes(a.iter().flat_map(|v| v.to_le_bytes()).collect())
}

fn marks_token(gpu: &renderer::gpu, s: i64) -> (renderer::gpu_marks, scene_command::marks::GpuToken) {
    let sink = renderer::create_gpu_marks(gpu).unwrap();
    let position: Vec<f64> = (0..3 * PER_SET)
        .map(|k| {
            let (x, y) = centre(s, k / 3);
            match k % 3 {
                0 => x,
                1 => y,
                _ => 0.0,
            }
        })
        .collect();
    let token = renderer::gpu_marks_token(
        None,
        &sink,
        4,
        PER_SET,
        &[
            marks_source::Host(floats(&position), 3),
            marks_source::Host(floats(&[RADIUS]), 0),
            marks_source::Host(words(&[word(s)]), 0),
            marks_source::Host(words(&[0]), 0),
            marks_source::Host(floats(&[1.0]), 0),
        ],
    ).unwrap();
    (sink, token)
}

const CLEAR: (f64, f64, f64, f64) = (0.125, 0.25, 0.375, 1.0);

/// The pixels of `commands` after a SetBlend Alpha, and the draws the renderer encodes for them.
fn capture(execution: &renderer::t, commands: Vec<Command>) -> (Vec<u8>, i64) {
    let mut all = vec![Command::SetBlend(render_ir::Blend::Alpha)];
    all.extend(commands);
    let ir = Rc::new(render_ir::create(all).unwrap());
    let submission = renderer::begin_submission(execution).unwrap();
    let lowered = Native::lower_scene2(&submission, 1, &|_| None, &ir);
    let mut encoded = 0;
    let result = lowered.and_then(|b| {
        encoded = Native::encoded_draws_for_test(&b);
        Native::step(Some(CLEAR), None, None, &submission, &[b])
    });
    Native::cancel(&submission);
    result.unwrap();
    (renderer::capture(execution).unwrap(), encoded)
}

fn check(execution: &renderer::t, label: &str, sets: &[i64], shapes: Vec<Command>, expected_draws: i64) {
    let (reference, one) = capture(execution, vec![Command::Shapes(all_sets(sets))]);
    let (pixels, count) = capture(execution, shapes);
    let painted = reference.iter().enumerate().filter(|(i, v)| i % 4 != 3 && **v != 0).count();
    assert!(painted > 200);
    assert!(pixels == reference, "{label}: pixels differ from one batch");
    assert!(one == 1 && count == expected_draws, "{label}: {count} draws for the sets, {one} for the one batch; expected {expected_draws} and 1");
}

fn gpu_batch_merge() {
    if !common::gpu() {
        return;
    }
    let baseline = ogpu::impl_::live_handles().unwrap();
    let execution = renderer::create_offscreen(&renderer::configuration {
        logical_width: WIDTH,
        logical_height: HEIGHT,
        drawable_width: WIDTH,
        drawable_height: HEIGHT,
        title: "batch-merge".into(),
        vsync: false,
        high_density: true,
    }).unwrap();
    let gpu = renderer::acquire_gpu().unwrap();
    let owner = Native::gpu_vertices_owner(&execution);
    let range = |n: i64| (0..n).collect::<Vec<i64>>();
    check(&execution, "3 CPU sets", &range(3), range(3).iter().map(|s| Command::Shapes(cpu_set(*s))).collect(), 3);
    check(&execution, "100 CPU sets", &range(100), range(100).iter().map(|s| Command::Shapes(cpu_set(*s))).collect(), 1);
    // Alternate Ui_batch and Marks sets: three families runs never merge, order is kept.
    let sources: Vec<Option<(renderer::gpu_marks, scene_command::marks::GpuToken)>> =
        range(6).iter().map(|s| if s % 2 == 0 { None } else { Some(marks_token(&gpu, *s)) }).collect();
    let shapes: Vec<Command> = sources
        .iter()
        .enumerate()
        .map(|(s, source)| match source {
            None => Command::Shapes(cpu_set(s as i64)),
            Some((_, token)) => Command::Shapes(ui_batch::of_marks(*token)),
        })
        .collect();
    check(&execution, "interleaved Ui_batch and Marks sets", &range(6), shapes, 6);
    for source in sources.iter().flatten() {
        renderer::close_gpu_marks(&source.0);
    }
    // A clip between two sets keeps them apart; the same clip around both keeps them together.
    let clip = || Command::PushClip(render_ir::Rect { x: 4.0, y: 4.0, width: 50.0, height: 30.0 });
    let apart = || vec![Command::Shapes(cpu_set(0)), clip(), Command::Shapes(cpu_set(1)), Command::PopClip, Command::Shapes(cpu_set(2))];
    let (reference, _) = capture(&execution, apart());
    let (pixels, count) = capture(&execution, apart());
    assert!(pixels == reference && count == 3);
    renderer::release_gpu(&gpu);
    renderer::destroy(&execution).unwrap();
    assert_eq!(scene_execution::Private::gpu_vertex_count_for_test(&owner), 0);
    assert_eq!(ogpu::impl_::live_handles().unwrap(), baseline, "batch merge handle delta");
}

pub const CASES: &[(&str, fn())] = &[("gpu_batch_merge", gpu_batch_merge)];
