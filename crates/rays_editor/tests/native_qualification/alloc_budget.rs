//! Per-frame allocation guard for the editor (Wave M): the `isogrid_orbit` harness on `basic.rays`,
//! on the workspace `rays` opens with no file (the plan's "empty editor") and on one without an
//! editor graph. Limits may only go down; on failure the walker profile names the sites.
use rays_editor::{editor, workspace};
use std::sync::atomic::Ordering::Relaxed;

const FRAMES: i64 = 30;
// (allocations per frame, bytes per frame)
// C11 shared cache: basic 1,980 / 182,989; bare 4,335 / 184,728; empty 4,079 / 174,502.
// M: borrowed panel keys and shell tree: basic 1,645 / 177,146; empty 2,571 / 138,828; bare 2,371 / 143,259.
// M: one path buffer per layout walk, shared document shell: basic 1,521 / 166,466; empty 2,001 / 117,568;
// bare 2,106 / 129,499.
// M: inspector folders by borrow, scope sync mutates the pane: empty 1,698 / 105,039; bare 1,918 / 121,357.
// Never raise.
const BASIC: (u64, u64) = (1_525, 167_000);
const EMPTY: (u64, u64) = (1_705, 106_000);
const BARE: (u64, u64) = (1_925, 122_000);

fn per_frame(source: &str, max: (u64, u64)) -> (u64, u64, String) {
    assert!(crate::common::gpu(), "native GPU required");
    let doc = workspace::load(None, Vec::new(), None, source).unwrap_or_else(|d| panic!("{d:?}"));
    let (w, h) = (1280, 900);
    let canvas = rays::canvas::create(w, h).unwrap();
    let gpu = flow_native::workspace_gpu::create(None);
    let mut editor = editor::create(editor::Create { gpu_owner: Some(gpu.clone()), await_: Some(true), ..Default::default() }, &doc, None, editor::prepare_fn(), editor::draw_fn()).unwrap();
    std::rc::Rc::make_mut(&mut editor.core).preferences = "x".repeat(100_000);
    let identity = editor.core.preferences.as_ptr();
    let mut tick = 0;
    let mut step = |editor: editor::Editor| {
        tick += 1;
        let f = rays::frame::Frame {
            input: input::Frame { dt: 1. / 60., frame: tick, tick, size: (w, h), pointer: (300. + (tick % 2) as f64, 400.), ..input::at_time(tick as f64 / 60.) },
            pixel_scale: (1., 1.), fps: 60., mouse_delta: (0., 0.) };
        // The native loop mutates its stored model; the owned API can reuse the same state.
        let e = editor::update(editor, &f);
        assert_eq!(e.core.preferences.as_ptr(), identity, "update copied the editor model");
        rays::canvas::render(None, &canvas, &editor::scene(&e, &f)).unwrap();
        let e = editor::after_present(e, &f);
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
    editor::close(&editor);
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
// the workspace `rays` opens with no file: the plan's "empty editor"
fn empty() { check("empty", include_str!("../../../rays_bin/src/empty.rays"), EMPTY) }
// no editor graph: the host's default shell
fn bare() { check("bare", "(workspace empty (graph g :context value 0))", BARE) }

pub fn cases() -> Vec<(&'static str, fn())> { vec![("alloc_budget_basic", basic), ("alloc_budget_empty", empty), ("alloc_budget_bare", bare)] }
