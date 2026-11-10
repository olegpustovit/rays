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
// M: consecutive 2D batches share a render pass: basic 1,446 / 161,821; empty 1,533 / 94,820;
// bare 1,783 / 112,996.
// M: residency declared once per pass, reducer edits the pane in place: basic 1,426 / 161,357;
// empty 1,424 / 92,082; bare 1,736 / 111,812.
// M: inspector rows own their views, header geometry once: basic 1,419 / 160,010; empty 1,371 / 88,603;
// bare 1,643 / 106,394.
// M: one shell layout for compose and focus, UI scopes keep depths: basic 1,408 / 157,975;
// empty 1,263 / 83,202; bare 1,489 / 98,366.
// M: chrome box labels format the leaf path in place: empty 1,230 / 82,806; bare 1,456 / 97,970.
// M: the scene graph is read in place: basic 1,384 / 157,167; empty 1,206 / 81,998; bare 1,417 / 96,657.
// R3/R7 and G1: shared immutable data and GPU metadata: basic 1,382 / 157,055;
// empty 1,206 / 81,958; bare 1,417 / 96,617.
// R4/R5: shared prelude/operator/state storage: basic 1,356 / 149,687.
// R8: mutate pane view state in place: empty 1,198 / 81,820.
// R9: retain the shared document and skip idle gesture rollback snapshots: basic 1,305 / 148,265;
// empty 1,154 / 80,422; bare 1,378 / 95,326.
// R10: typed renderer keys and numeric hashes: basic 1,285 / 147,518.
// Z1/G1/Z5: reuse UI callbacks, labels and renderer buffers with shared hash/ABI changes:
// basic 1,232 / 141,996; empty 916 / 53,361; bare 1,156 / 69,746.
// Never raise.
// Z2 shared immutable Text, sorted Record fields and Symbol paths; measured idle guard pairs.
// Z8 retains immutable image cache storage instead of copying it during every frame.
// Z1: one memoised shell geometry per frame, box labels formatted into a reused buffer, stack-chunked
// Metal residency lists, no layout intent work without intents: basic 858 / 91,365; empty 609 / 36,159;
// bare 723 / 46,059.
// Z1: borrowed graph path and title crumbs: basic 855 / 91,347; empty 538 / 34,724; bare 693 / 45,448.
// Z1: exact quad cache keys, single-field settings reads: basic 805 / 89,381; empty 538 / 34,724; bare 693 / 45,448.
// Z1: shared shell tree, kept leaf list, borrowed level objects: basic 747 / 85,781; empty 462 / 31,882; bare 617 / 42,561.
// Z1: wide-hash quad keys, no generic cache constructor: basic 747 / 85,098; empty 462 / 31,882; bare 617 / 42,561.
const BASIC: (u64, u64) = (747, 85_098);
const EMPTY: (u64, u64) = (462, 31_882);
const BARE: (u64, u64) = (617, 42_561);

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
        assert!(e.context_error.is_none(), "{:?}", e.context_error);
        assert!(e.core.cook.error.is_none(), "{:?}", e.core.cook.error);
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

// A repeated callsite reuses only its allocation: captured resources are released after each
// paint, and changing the closure type at the same box/position remains safe.
fn paint_storage() {
    use std::{cell::Cell, rc::Rc};
    use pxui::ui::{BoxOpts, Paint, Rect, Size, Ui};
    let mut ui = Ui::create(input::Platform::Apple, None, None);
    let token = Rc::new(Cell::new(0));
    let mut label_owner = None::<std::rc::Weak<str>>;
    let painter = |token: Rc<Cell<i32>>| move |_: &mut Paint, _: Rect| token.set(token.get() + 1);
    for round in 0..6 {
        let f = rays::frame::Frame { input: input::Frame { size: (100, 100), ..input::at_time(round as f64) }, pixel_scale: (1., 1.), fps: 60., mouse_delta: (0., 0.) };
        ui.begin_frame(&f);
        let b = ui.box_("reuse", BoxOpts::default().w(Size::Px(20.)).h(Size::Px(20.)));
        let before = mem_sys::alloc_count();
        let previous = label_owner.as_ref().map(|previous| previous.upgrade().unwrap());
        let label = ui.cached_label(&b, 0, if round < 4 { "Repeated label" } else { "Changed label" });
        if let Some(previous) = previous {
            if round == 4 {
                assert!(!Rc::ptr_eq(&previous, &label), "changed text reused a stale label");
                assert_eq!(previous.as_ref(), "Repeated label", "changing a label mutated its retained snapshot");
            } else {
                assert!(Rc::ptr_eq(&previous, &label), "unchanged label storage was replaced");
            }
        }
        label_owner = Some(Rc::downgrade(&label));
        if round == 3 {
            let token = token.clone();
            ui.draw(&b, move |_: &mut Paint, _: Rect| token.set(token.get() + 1));
        } else {
            ui.draw(&b, painter(token.clone()));
        }
        ui.draw_over(&b, painter(token.clone()));
        if [1, 2, 5].contains(&round) {
            assert_eq!(mem_sys::alloc_count(), before, "warm callback storage allocated");
        }
        ui.end_frame(&f);
        assert_eq!(Rc::strong_count(&token), 1, "published frame retained a callback payload");
        assert_eq!(token.get(), (round + 1) * 2);
    }
    let f = rays::frame::Frame { input: input::Frame { size: (100, 100), ..input::at_time(7.) }, pixel_scale: (1., 1.), fps: 60., mouse_delta: (0., 0.) };
    ui.frame(&f, &mut |_| {});
    assert!(label_owner.unwrap().upgrade().is_none(), "a retired box retained its cached label");
    ui.destroy();
}

pub fn cases() -> Vec<(&'static str, fn())> { vec![("alloc_budget_paint_storage", paint_storage), ("alloc_budget_basic", basic), ("alloc_budget_empty", empty), ("alloc_budget_bare", bare)] }
