//! Port of lib/flow_native/test_frame_alloc.ml: recursive_signals at full depth (its fold into a
//! draw, on the editor's own path: Drawing over the workspace GPU owner, fast precision) drawn many
//! times. After warm-up a frame creates no GPU buffer and allocates at most 1.1 MB. The
//! sketch animates, so every frame does its work and its counts move; the pointer rests.
//!
//! The fold's arrays stay on the device through the draw: the picture must match the one drawn
//! after reading them back (the host declines to draw over them, so they come back and the marks
//! upload).
//!
//! Uses the counting allocator's cumulative allocated bytes for the OCaml per-frame budget.
//! `RAYS_ALLOC_PROFILE` has no Rust counterpart (no allocation sampler).
mod common;
use flow_eval as e;
use flow_native::{drawing, workspace_gpu as wg};
use flow_sop::lower;
use rays::canvas;
use std::cell::Cell;
use std::rc::Rc;

/// The source budget for the dispatch encoding and per-region bookkeeping.
const BUDGET: u64 = 1_100_000;

fn frame() {
    if !common::gpu() {
        return;
    }
    mem::configure(&|k| std::env::var(k).ok()).expect("mem configure");
    let path = concat!(env!("CARGO_MANIFEST_DIR"), "/../../sketches/recursive_signals.rays");
    let (_, doc) = web_export::web_export::load(path);
    let doc = doc.unwrap_or_else(|ds| panic!("{}", ds.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("\n")));
    let lowered = lower::of_checked(None, lower::Options { inputs: Some((*doc.inputs).clone()), ..Default::default() }, &doc.checked).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    let picture = lowered.evaluated.results.iter().find(|(n, _)| n == "picture").map(|(_, v)| v.clone()).expect("picture");
    let ws = wg::create(None);
    wg::qualification(&ws);
    let prepared = drawing::prepare(
        &drawing::PrepareOptions { gpu: Some(wg::config(&ws, true)), states: lowered.states.clone(), ..Default::default() },
        &lowered.evaluated.plan,
        &picture,
    )
    .unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    let state = e::create_state();
    let size = (840i64, 1000i64);
    let draw = |read_back: bool, k: i64| {
        // the pointer at the bottom: density 1, 32 levels
        let live = input::Frame { dt: 1. / 60., frame: k, tick: k, size, pointer: (420., 1000.), ..input::at_time(k as f64 / 60.) };
        let mut scene = None;
        let shown = Rc::new(Cell::new(0));
        wg::with_backend(&ws, true, &mut |gpu| {
            let (resident, show) = wg::resident(gpu, &ws).expect("resident GPU scope");
            let shown = shown.clone();
            let show: drawing::show = if read_back { Rc::new(|_, _| Some(None)) } else { Rc::new(move |packed, live| {
                let result = show(packed, live);
                if matches!(&result, Some(Some(_))) { shown.set(shown.get() + 1); }
                result
            }) };
            let options = drawing::RenderOptions { state: Some(state.clone()), gpu: Some(wg::marks(&ws)), resident: Some((resident, show)), ..Default::default() };
            scene = Some(drawing::render_prepared(&prepared, &live, size, &options).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d))));
        });
        wg::frame(&ws, None);
        assert!(read_back || shown.get() > 0, "recursive_signals frame {k}: no mark attributes drawn over resident fold arrays");
        scene.expect("scene")
    };
    let pixels = |scene: &rays::scene::t| {
        let c = canvas::create(size.0, size.1).expect("canvas");
        let result = canvas::render(None, &c, scene).and_then(|_| canvas::pixels(&c));
        let _ = canvas::destroy(&c);
        result.unwrap_or_else(|m| panic!("{m}"))
    };
    let buffers = || {
        let (host, _, _, sink_buffers, _) = wg::image_stats(&ws);
        host.buffer_creations + sink_buffers
    };
    // warm-up: kernels compile, placement settles, and every slot grows to the counts of these
    // times (a count past its capacity is a change of shape: it reruns and grows)
    for k in 1..=60 {
        draw(false, k);
    }
    // one after the other: the second draw replaces the outputs the first scene binds
    let kept = pixels(&draw(false, 50));
    let read = pixels(&draw(true, 50));
    assert_eq!(kept.len(), read.len(), "resident and readback pixel counts");
    let differ = kept.iter().zip(read.iter()).filter(|(a, b)| a != b).count();
    assert_eq!(differ, 0, "recursive_signals: the kept fold draws {differ} pixels differently");
    // the measured frames replay times the warm-up drew: the same shapes, all the work again
    let frames = 20i64;
    let (before, made) = (mem_sys::alloc_bytes_total(), buffers());
    for k in 41..=40 + frames {
        draw(false, k);
    }
    let per_frame = (mem_sys::alloc_bytes_total() - before) / frames as u64;
    let made = buffers() - made;
    println!("recursive_signals: {per_frame} allocated bytes and {made} GPU buffers a frame after warm-up (budget {BUDGET})");
    wg::close(&ws);
    assert!(per_frame <= BUDGET && made == 0, "recursive_signals: {per_frame} allocated bytes (budget {BUDGET}) and {made} GPU buffers a frame");
}

fn main() {
    common::window::run(&[("frame_alloc_recursive_signals", frame)]);
}
