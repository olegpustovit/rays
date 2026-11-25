//! What a click costs (UI fleet plan, docs/fleet-ui.md): allocations on the frame of each editor action
//! and on the three frames after it, on three workspaces. Ceilings are exact counts (stable under load),
//! never raised; a lane that makes an action cheaper lowers its row. Wall times are printed, and
//! checked against the plan's targets only with `RAYS_CLICK_TIMES=1` (they move 2x on a loaded Mac).
//! Allocation sites: `MallocStackLogging=1` on the test binary, then `malloc_history <pid> -callTree`.
use input::{Event, Key};
use rays_editor::{editor, workspace};
use std::time::Instant;

const W: i64 = 1280;
const H: i64 = 900;

/// (workspace, action, ceiling on the key frame, ceiling on the third frame after it: the settled cost)
/// Set 2026-11-24 at measured + 5% (dec599ffa); lower a row when a change makes it cheaper.
const CEILINGS: &[(&str, &str, u64, u64)] = &[
    ("basic", "idle", 713, 638),
    ("basic", "hover", 650, 651),
    ("basic", "split right", 13424, 659),
    ("basic", "retype graph", 2910, 200),
    ("basic", "retype inspector", 2871, 191),
    ("basic", "retype lisp", 2850, 3778),
    ("basic", "retype list", 2871, 198),
    ("basic", "retype spreadsheet", 2868, 191),
    ("basic", "retype outline", 2860, 197),
    ("basic", "close panel", 2602, 163),
    ("basic", "split below", 3074, 3824),
    ("basic", "toggle inspector", 334, 208),
    ("basic", "toggle graph", 426, 189),
    ("basic", "toggle graph back", 394, 204),
    ("basic", "toggle timeline", 408, 335),
    ("basic", "undo", 501, 208),
    ("basic", "undo 2", 1487, 194),
    ("basic", "redo", 364, 208),
    ("basic", "new layout", 3562, 258),
    ("basic", "layout 0", 3417, 259),
    ("basic", "layout 1", 3392, 259),
    ("basic", "palette", 1011, 533),
    ("basic", "escape", 398, 259),
    ("basic", "idle end", 314, 259),
    ("empty", "idle", 252, 180),
    ("empty", "hover", 194, 195),
    ("empty", "split right", 2793, 201),
    ("empty", "retype graph", 2281, 198),
    ("empty", "retype inspector", 2375, 184),
    ("empty", "retype lisp", 2246, 1928),
    ("empty", "retype list", 2267, 199),
    ("empty", "retype spreadsheet", 2266, 187),
    ("empty", "retype outline", 2253, 193),
    ("empty", "close panel", 2104, 182),
    ("empty", "split below", 2515, 1980),
    ("empty", "toggle inspector", 425, 201),
    ("empty", "toggle graph", 405, 185),
    ("empty", "toggle graph back", 397, 201),
    ("empty", "toggle timeline", 407, 333),
    ("empty", "undo", 499, 208),
    ("empty", "undo 2", 585, 193),
    ("empty", "redo", 365, 208),
    ("empty", "new layout", 3591, 311),
    ("empty", "layout 0", 3333, 312),
    ("empty", "layout 1", 3312, 312),
    ("empty", "palette", 1060, 585),
    ("empty", "escape", 451, 312),
    ("empty", "idle end", 368, 312),
    ("isogrid", "idle", 580, 488),
    ("isogrid", "hover", 502, 503),
    ("isogrid", "split right", 7381, 492),
    ("isogrid", "retype graph", 832, 492),
    ("isogrid", "retype inspector", 6761, 492),
    ("isogrid", "retype lisp", 833, 506),
    ("isogrid", "retype list", 845, 506),
    ("isogrid", "retype spreadsheet", 6789, 506),
    ("isogrid", "retype outline", 6788, 506),
    ("isogrid", "close panel", 3784, 506),
    ("isogrid", "split below", 7396, 506),
    ("isogrid", "toggle inspector", 984, 493),
    ("isogrid", "toggle graph", 1820, 493),
    ("isogrid", "toggle graph back", 1027, 493),
    ("isogrid", "toggle timeline", 1121, 860),
    ("isogrid", "undo", 1168, 497),
    ("isogrid", "undo 2", 12011, 495),
    ("isogrid", "redo", 1073, 497),
    ("isogrid", "new layout", 8970, 586),
    ("isogrid", "layout 0", 8781, 587),
    ("isogrid", "layout 1", 8730, 587),
    ("isogrid", "palette", 1685, 942),
    ("isogrid", "escape", 987, 587),
    ("isogrid", "idle end", 648, 587),
    ("long", "idle fresh", 711, 638),
    ("long", "split", 13387, 659),
    ("long", "close", 3549, 171),
    ("long", "undo", 5612, 218),
    ("long", "idle after session", 238, 177),
];

pub(crate) struct P {
    pub(crate) e: Option<editor::Editor>,
    tick: i64,
    canvas: rays::canvas::Canvas,
    rows: Vec<(String, f64, u64, u64)>,
}

impl P {
    pub(crate) fn step(&mut self, keys: &[Key], events: Vec<Event>, pointer: (f64, f64)) -> (f64, u64) {
        self.tick += 1;
        let t = self.tick;
        let f = rays::frame::Frame {
            input: input::Frame { dt: 1. / 60., frame: t, tick: t, size: (W, H), pointer, keys: keys.to_vec(), events, ..input::at_time(t as f64 / 60.) },
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
    let e = editor::create(editor::Create { gpu_owner: Some(gpu.clone()), await_: Some(true), platform: Some(input::Platform::Apple), ..Default::default() }, &doc, None, editor::prepare_fn(), editor::draw_fn()).unwrap();
    // measured as the app runs: no verifying rebuilds
    e.core.ui.borrow_mut().verify_views(false);
    let mut p = P { e: Some(e), tick: 0, canvas, rows: Vec::new() };
    for _ in 0..30 { p.step(&[], vec![], (600., 400.)); }
    eprintln!("CLICK ===== {label}: key frame, +1, +2, +3 (update+render ms, allocations)");
    actions(&mut p);
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
