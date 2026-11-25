//! What a click costs (UI fleet plan, docs/fleet-ui.md): allocations on the frame of each editor action
//! and on the three frames after it, on three workspaces. Ceilings are exact counts (stable under load).
//! A lane that makes an action cheaper lowers its row. Wall times are printed, and
//! checked against the plan's targets only with `RAYS_CLICK_TIMES=1` (they move 2x on a loaded Mac).
//! Allocation sites: `MallocStackLogging=1` on the test binary, then `malloc_history <pid> -callTree`.
use input::{Event, Key, MouseButton};
use rays_editor::{editor, workspace};
use std::time::Instant;

const W: i64 = 1280;
const H: i64 = 900;

/// (workspace, action, ceiling on the key frame, ceiling on the third frame after it: the settled cost)
/// Fresh-process allocations plus 5%, rounded up. Existing ceilings can only fall unless the
/// historical baseline confirms a measurement change, as with IsoGrid’s toggle graph key frame.
const CEILINGS: &[(&str, &str, u64, u64)] = &[
    ("basic", "idle", 637, 577),
    ("basic", "hover", 590, 590),
    ("basic", "split right", 10800, 598),
    ("basic", "retype graph", 2870, 153),
    ("basic", "retype inspector", 2785, 145),
    ("basic", "retype lisp", 2766, 790),
    ("basic", "retype list", 2786, 152),
    ("basic", "retype spreadsheet", 2783, 150),
    ("basic", "retype outline", 2780, 151),
    ("basic", "close panel", 2469, 119),
    ("basic", "split below", 2902, 780),
    ("basic", "toggle inspector", 235, 160),
    ("basic", "toggle graph", 326, 143),
    ("basic", "toggle graph back", 297, 156),
    ("basic", "toggle timeline", 308, 231),
    ("basic", "undo", 396, 160),
    ("basic", "undo 2", 1433, 147),
    ("basic", "redo", 312, 160),
    ("basic", "new layout", 3387, 209),
    ("basic", "layout 0", 3257, 210),
    ("basic", "layout 1", 3233, 210),
    ("basic", "palette", 971, 217),
    ("basic", "escape", 276, 210),
    ("basic", "idle end", 256, 210),
    ("empty", "idle", 229, 172),
    ("empty", "hover", 182, 182),
    ("empty", "split right", 2790, 192),
    ("empty", "retype graph", 2272, 184),
    ("empty", "retype inspector", 2324, 175),
    ("empty", "retype lisp", 2203, 627),
    ("empty", "retype list", 2222, 188),
    ("empty", "retype spreadsheet", 2221, 182),
    ("empty", "retype outline", 2214, 182),
    ("empty", "close panel", 2024, 173),
    ("empty", "split below", 2383, 622),
    ("empty", "toggle inspector", 360, 187),
    ("empty", "toggle graph", 340, 174),
    ("empty", "toggle graph back", 334, 187),
    ("empty", "toggle timeline", 342, 264),
    ("empty", "undo", 432, 195),
    ("empty", "undo 2", 565, 181),
    ("empty", "redo", 349, 195),
    ("empty", "new layout", 3388, 298),
    ("empty", "layout 0", 3152, 299),
    ("empty", "layout 1", 3134, 299),
    ("empty", "palette", 1060, 304),
    ("empty", "escape", 363, 299),
    ("empty", "idle end", 344, 299),
    ("isogrid", "idle", 536, 458),
    ("isogrid", "hover", 469, 469),
    ("isogrid", "split right", 7061, 462),
    ("isogrid", "retype graph", 509, 462),
    ("isogrid", "retype inspector", 6438, 462),
    ("isogrid", "retype lisp", 510, 475),
    ("isogrid", "retype list", 520, 475),
    ("isogrid", "retype spreadsheet", 6464, 475),
    ("isogrid", "retype outline", 6464, 475),
    ("isogrid", "close panel", 3465, 475),
    ("isogrid", "split below", 7071, 475),
    ("isogrid", "toggle inspector", 660, 462),
    ("isogrid", "toggle graph", 1919, 465),
    ("isogrid", "toggle graph back", 931, 462),
    ("isogrid", "toggle timeline", 1026, 537),
    ("isogrid", "undo", 1069, 467),
    ("isogrid", "undo 2", 11883, 467),
    ("isogrid", "redo", 974, 467),
    ("isogrid", "new layout", 8641, 556),
    ("isogrid", "layout 0", 8449, 557),
    ("isogrid", "layout 1", 8399, 557),
    ("isogrid", "palette", 1571, 565),
    ("isogrid", "escape", 814, 557),
    ("isogrid", "idle end", 608, 557),
    ("long", "idle fresh", 637, 577),
    ("long", "split", 10802, 599),
    ("long", "close", 2532, 117),
    ("long", "undo", 2112, 159),
    ("long", "idle after session", 173, 121),
];

/// (workspace, action, ceiling on the busiest of its four frames): the first Lisp pane on IsoGrid
/// prints the document (136,747 allocations before the printer wrote flat text into one buffer).
const PEAKS: &[(&str, &str, u64)] = &[("isogrid", "retype lisp", 12_800)];

pub(crate) struct P {
    pub(crate) e: Option<editor::Editor>,
    tick: i64,
    buttons: Vec<MouseButton>,
    canvas: rays::canvas::Canvas,
    rows: Vec<(String, f64, u64, u64)>,
    /// the most allocations of any of an action's four frames
    peaks: Vec<(String, u64)>,
}

impl P {
    pub(crate) fn step(&mut self, keys: &[Key], events: Vec<Event>, pointer: (f64, f64)) -> (f64, u64) {
        self.tick += 1;
        let t = self.tick;
        for event in &events {
            match event {
                Event::PointerPressed(button, _) if !self.buttons.contains(button) => self.buttons.push(*button),
                Event::PointerReleased(button, _) => self.buttons.retain(|held| held != button),
                _ => {}
            }
        }
        let f = rays::frame::Frame {
            input: input::Frame { dt: 1. / 60., frame: t, tick: t, size: (W, H), pointer, keys: keys.to_vec(), buttons: self.buttons.clone(), events, ..input::at_time(t as f64 / 60.) },
            pixel_scale: (1., 1.), fps: 60., mouse_delta: (0., 0.) };
        let n0 = mem_sys::alloc_count();
        let s = Instant::now();
        let e = editor::update(self.e.take().unwrap(), &f);
        assert!(e.context_error.is_none(), "{:?}", e.context_error);
        let sc = editor::scene(&e, &f);
        rays::canvas::render(None, &self.canvas, &sc).unwrap();
        self.e = Some(editor::after_present(e, &f));
        (s.elapsed().as_secs_f64() * 1e3, mem_sys::alloc_count() - n0)
    }
    pub(crate) fn record(&mut self, name: &str, frames: Vec<(f64, u64)>) {
        let cells: Vec<String> = frames.iter().map(|(ms, n)| format!("{ms:7.2}ms {n:7}a")).collect();
        eprintln!("CLICK {name:26} | {}", cells.join(" | "));
        self.rows.push((name.to_string(), frames[0].0, frames[0].1, frames[3].1));
        self.peaks.push((name.to_string(), frames.iter().map(|f| f.1).max().unwrap_or(0)));
    }
    /// every press but the last on its own frame; the last is the measured click
    fn act(&mut self, name: &str, keys: &[Key], presses: &[Key]) {
        for k in &presses[..presses.len() - 1] { self.step(keys, vec![Event::KeyPressed(*k)], (600., 400.)); }
        let mut frames = vec![self.step(keys, vec![Event::KeyPressed(*presses.last().unwrap())], (600., 400.))];
        for _ in 0..3 { frames.push(self.step(&[], vec![], (600., 400.))); }
        self.record(name, frames);
    }
    fn hover(&mut self, name: &str) {
        let frames = (0..4).map(|i| { let p = (300. + 40. * i as f64, 300. + 20. * i as f64); self.step(&[], vec![Event::PointerMoved(p)], p) }).collect();
        self.record(name, frames);
    }
}

pub(crate) fn session(label: &str, source: &str, actions: &dyn Fn(&mut P)) -> Vec<(String, f64, u64, u64)> {
    assert!(crate::common::gpu(), "native GPU required");
    let doc = workspace::load(None, Vec::new(), None, source).unwrap_or_else(|d| panic!("{d:?}"));
    let canvas = rays::canvas::create(W, H).unwrap();
    let gpu = flow_native::workspace_gpu::create(None);
    let mut e = editor::create(editor::Create { gpu_owner: Some(gpu.clone()), await_: Some(true), platform: Some(input::Platform::Apple), ..Default::default() }, &doc, None, editor::prepare_fn(), editor::draw_fn()).unwrap();
    // Wall-clock cook and edit timings change the status text variants in PXUI's text cache.
    // The frame clock above still drives input, and Instant measures the real time of each action.
    let core = std::rc::Rc::make_mut(&mut e.core);
    core.clock = std::sync::Arc::new(|| 0.);
    sop::async_cook::set_clock(&core.cook.worker, core.clock.clone());
    // measured as the app runs: no verifying rebuilds
    e.core.ui.borrow_mut().verify_views(false);
    let mut p = P { e: Some(e), tick: 0, buttons: Vec::new(), canvas, rows: Vec::new(), peaks: Vec::new() };
    for _ in 0..30 { p.step(&[], vec![], (600., 400.)); }
    eprintln!("CLICK ===== {label}: key frame, +1, +2, +3 (update+render ms, allocations)");
    actions(&mut p);
    for (name, peak) in &p.peaks {
        if let Some((_, _, max)) = PEAKS.iter().find(|c| c.0 == label && c.1 == name) {
            assert!(peak <= max, "{label} {name}: a frame allocated {peak}, over {max}");
        }
    }
    editor::close(p.e.as_ref().unwrap());
    flow_native::workspace_gpu::close(&gpu);
    rays::canvas::destroy(&p.canvas).unwrap();
    p.rows
}

fn tour(p: &mut P) {
    let ch = Key::KeyChar;
    let l = |c: char| [ch('/'), ch('l'), ch(c)];
    p.act("idle", &[], &[Key::Shift]);
    p.hover("hover");
    p.act("split right", &[], &[ch('/'), ch('o'), ch('v')]);
    p.act("retype graph", &[], &l('g'));
    p.act("retype inspector", &[], &l('i'));
    p.act("retype lisp", &[], &l('t'));
    p.act("retype list", &[], &l('l'));
    p.act("retype spreadsheet", &[], &l('s'));
    p.act("retype outline", &[], &l('u'));
    p.act("close panel", &[], &[ch('/'), ch('o'), ch('x')]);
    p.act("split below", &[], &[ch('/'), ch('o'), ch('h')]);
    p.act("toggle inspector", &[], &[ch('/'), ch('i')]);
    p.act("toggle graph", &[], &[ch('/'), ch('g')]);
    p.act("toggle graph back", &[], &[ch('/'), ch('g')]);
    p.act("toggle timeline", &[], &[ch('/'), ch('t')]);
    p.act("undo", &[Key::Meta], &[ch('z')]);
    p.act("undo 2", &[Key::Meta], &[ch('z')]);
    p.act("redo", &[Key::Meta, Key::Shift], &[ch('z')]);
    p.act("new layout", &[], &[ch('/'), ch('['), ch('n')]);
    p.act("layout 0", &[], &[ch('/'), ch('['), ch('0')]);
    p.act("layout 1", &[], &[ch('/'), ch('['), ch('1')]);
    p.act("palette", &[], &[ch('/'), ch('/')]);
    p.act("escape", &[], &[Key::Escape]);
    p.act("idle end", &[], &[Key::Shift]);
}

/// A long session of split/close pairs, then undos, then settled idle.
fn long(p: &mut P, rounds: usize) {
    let ch = Key::KeyChar;
    p.act("idle fresh", &[], &[Key::Shift]);
    for _ in 0..rounds {
        p.act("split", &[], &[ch('/'), ch('o'), ch('v')]);
        p.act("close", &[], &[ch('/'), ch('o'), ch('x')]);
    }
    for _ in 0..rounds { p.act("undo", &[Key::Meta], &[ch('z')]); }
    p.act("idle after session", &[], &[Key::Shift]);
}

pub(crate) fn sketch(name: &str) -> String { std::fs::read_to_string(format!("{}/../../sketches/{name}", env!("CARGO_MANIFEST_DIR"))).unwrap() }

fn check(label: &str, rows: &[(String, f64, u64, u64)]) {
    let mut over = Vec::new();
    let mut table = String::new();
    for (name, ms, key, settled) in rows {
        // a repeated name (long session) prints and checks its worst run
        if table.contains(&format!("(\"{label}\", \"{name}\"")) { continue; }
        let (key, settled) = rows.iter().filter(|r| &r.0 == name).fold((*key, *settled), |(k, s), r| (k.max(r.2), s.max(r.3)));
        table.push_str(&format!("    (\"{label}\", \"{name}\", {key}, {settled}),\n"));
        if let Some((_, _, max_key, max_settled)) = CEILINGS.iter().find(|c| c.0 == label && c.1 == name) {
            if key > *max_key || settled > *max_settled { over.push(format!("{name}: {key}/{settled} over {max_key}/{max_settled}")); }
        } else if !CEILINGS.is_empty() {
            over.push(format!("{name}: no ceiling"));
        }
        if std::env::var_os("RAYS_CLICK_TIMES").is_some() && name != "idle" && *ms > 3.0 { over.push(format!("{name}: {ms:.1} ms over the 3 ms target")); }
    }
    eprintln!("CLICK ceilings now:\n{table}");
    assert!(over.is_empty(), "{label}: {}", over.join("; "));
}

fn basic() { check("basic", &session("basic", &sketch("basic.rays"), &tour)) }
fn empty() { check("empty", &session("empty", include_str!("../../../rays_bin/src/empty.rays"), &tour)) }
fn isogrid() { check("isogrid", &session("isogrid", &sketch("isogrid.rays"), &tour)) }
fn long_session() {
    let rounds = if std::env::var("RAYS_SLOW").as_deref() == Ok("1") { 20 } else { 12 };
    let rows = session("long", &sketch("basic.rays"), &|p| long(p, rounds));
    let fresh = rows.iter().find(|r| r.0 == "idle fresh").unwrap().3;
    let after = rows.iter().find(|r| r.0 == "idle after session").unwrap().3;
    eprintln!("CLICK long session: settled idle {fresh} fresh, {after} after {} edits and {rounds} undos", rounds * 2);
    check("long", &rows);
}

pub fn cases() -> Vec<(&'static str, fn())> {
    vec![("click_budget_basic", basic), ("click_budget_empty", empty), ("click_budget_isogrid", isogrid), ("click_budget_long", long_session)]
}
