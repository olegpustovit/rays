//! Per-frame allocation guard for the editor (Wave M): the `isogrid_orbit` harness on `basic.rays`
//! and on an empty workspace. Limits may only go down; on failure the walker profile names the sites.
use rays_editor::{Editor, Workspace};
use std::sync::atomic::Ordering::Relaxed;

const FRAMES: i64 = 30;
// (allocations per frame, bytes per frame)
// C6 mutable loop: basic 2,002 / 184,943; empty 4,335 / 184,728. Never raise.
const BASIC: (u64, u64) = (2_010, 186_000);
const EMPTY: (u64, u64) = (4_350, 186_000);

fn per_frame(source: &str, max: (u64, u64)) -> (u64, u64, String) {
    assert!(crate::common::gpu(), "native GPU required");
    let doc = Workspace::load(None, Vec::new(), None, source).unwrap_or_else(|d| panic!("{d:?}"));
    let (w, h) = (1280, 900);
    let canvas = rays::canvas::create(w, h).unwrap();
    let gpu = flow_native::workspace_gpu::create(None);
    let mut editor = Editor::create(Editor::Create { gpu_owner: Some(gpu.clone()), await_: Some(true), ..Default::default() }, &doc, None, Editor::prepare_fn(), Editor::draw_fn()).unwrap();
    std::rc::Rc::make_mut(&mut editor.core).preferences = "x".repeat(100_000);
    let identity = editor.core.preferences.as_ptr();
    let mut tick = 0;
    let mut step = |editor: Editor::t| {
        tick += 1;
        let f = rays::frame::t {
            input: input::Frame { dt: 1. / 60., frame: tick, tick, size: (w, h), pointer: (300. + (tick % 2) as f64, 400.), ..input::at_time(tick as f64 / 60.) },
            pixel_scale: (1., 1.), fps: 60., mouse_delta: (0., 0.) };
        // The native loop mutates its stored model; the owned API can reuse the same state.
        let e = Editor::update(editor, &f);
        assert_eq!(e.core.preferences.as_ptr(), identity, "update copied the editor model");
        rays::canvas::render(None, &canvas, &Editor::scene(&e, &f)).unwrap();
        let e = Editor::after_present(e, &f);
        assert_eq!(e.core.preferences.as_ptr(), identity, "after_present copied the editor model");
        e
    };
    for _ in 0..FRAMES { editor = step(editor); }
    let (n0, b0) = (mem_sys::alloc_count(), mem_sys::alloc_bytes_total());
    for _ in 0..FRAMES { editor = step(editor); }
    let (n, b) = ((mem_sys::alloc_count() - n0) / FRAMES as u64, (mem_sys::alloc_bytes_total() - b0) / FRAMES as u64);
    let report = if n > max.0 || b > max.1 {
        // mem_sys::TRACE went with M20; the walker profile names the Lisp-evaluation sites
        flow_eval::PROFILE_ON.store(true, Relaxed);
        flow_eval::profile_reset();
        for _ in 0..FRAMES { editor = step(editor); }
        flow_eval::PROFILE_ON.store(false, Relaxed);
        flow_eval::profile_report()
    } else { String::new() };
    Editor::close(&editor);
    flow_native::workspace_gpu::close(&gpu);
    rays::canvas::destroy(&canvas).unwrap();
    (n, b, report)
}

fn check(name: &str, source: &str, (max_allocs, max_bytes): (u64, u64)) {
    let (n, b, report) = per_frame(source, (max_allocs, max_bytes));
    eprintln!("{name}: {n} allocs/frame, {b} bytes/frame");
    assert!(n <= max_allocs && b <= max_bytes, "{name}: {n} allocs/frame (max {max_allocs}), {b} bytes/frame (max {max_bytes})\nwalker profile over {FRAMES} frames:\n{report}");
}

fn basic() { check("basic", &std::fs::read_to_string(concat!(env!("CARGO_MANIFEST_DIR"), "/../../sketches/basic.rays")).unwrap(), BASIC) }
fn empty() { check("empty", "(workspace empty (graph g :context value 0))", EMPTY) }

pub fn cases() -> Vec<(&'static str, fn())> { vec![("alloc_budget_basic", basic), ("alloc_budget_empty", empty)] }
