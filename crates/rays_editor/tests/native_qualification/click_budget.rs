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
    ("basic", "idle", 739, 663),
    ("basic", "hover", 676, 677),
    ("basic", "split right", 13488, 764),
    ("basic", "retype graph", 3048, 323),
    ("basic", "retype inspector", 3025, 298),
    ("basic", "retype lisp", 2988, 3942),
    ("basic", "retype list", 3070, 321),
    ("basic", "retype spreadsheet", 3025, 296),
    ("basic", "retype outline", 2998, 479),
    ("basic", "close panel", 2918, 247),
    ("basic", "split below", 3188, 3946),
    ("basic", "toggle inspector", 479, 331),
    ("basic", "toggle graph", 573, 274),
    ("basic", "toggle graph back", 502, 327),
    ("basic", "toggle timeline", 555, 458),
    ("basic", "undo", 622, 331),
    ("basic", "undo 2", 1613, 278),
    ("basic", "redo", 446, 331),
    ("basic", "new layout", 3721, 381),
    ("basic", "layout 0", 3576, 382),
    ("basic", "layout 1", 3550, 382),
    ("basic", "palette", 1131, 656),
    ("basic", "escape", 521, 382),
    ("basic", "idle end", 437, 382),
    ("empty", "idle", 268, 194),
    ("empty", "hover", 207, 208),
    ("empty", "split right", 2850, 283),
    ("empty", "retype graph", 2401, 314),
    ("empty", "retype inspector", 16681, 267),
    ("empty", "retype lisp", 2367, 2080),
    ("empty", "retype list", 2459, 320),
    ("empty", "retype spreadsheet", 2427, 269),
    ("empty", "retype outline", 2376, 452),
    ("empty", "close panel", 2404, 264),
    ("empty", "split below", 2637, 2102),
    ("empty", "toggle inspector", 713, 320),
    ("empty", "toggle graph", 703, 265),
    ("empty", "toggle graph back", 657, 320),
    ("empty", "toggle timeline", 705, 451),
    ("empty", "undo", 761, 327),
    ("empty", "undo 2", 851, 272),
    ("empty", "redo", 586, 327),
    ("empty", "new layout", 3765, 430),
    ("empty", "layout 0", 3502, 431),
    ("empty", "layout 1", 3480, 431),
    ("empty", "palette", 1183, 704),
    ("empty", "escape", 570, 431),
    ("empty", "idle end", 487, 431),
    ("isogrid", "idle", 602, 508),
    ("isogrid", "hover", 522, 523),
    ("isogrid", "split right", 7584, 512),
    ("isogrid", "retype graph", 917, 512),
    ("isogrid", "retype inspector", 6962, 512),
    ("isogrid", "retype lisp", 917, 601),
    ("isogrid", "retype list", 1066, 611),
    ("isogrid", "retype spreadsheet", 7088, 611),
    ("isogrid", "retype outline", 7088, 611),
    ("isogrid", "close panel", 4065, 611),
    ("isogrid", "split below", 7695, 611),
    ("isogrid", "toggle inspector", 2667, 596),
    ("isogrid", "toggle graph", 3664, 557),
    ("isogrid", "toggle graph back", 2694, 596),
    ("isogrid", "toggle timeline", 2845, 963),
    ("isogrid", "undo", 2861, 600),
    ("isogrid", "undo 2", 13711, 559),
    ("isogrid", "redo", 2714, 600),
    ("isogrid", "new layout", 9317, 689),
    ("isogrid", "layout 0", 9123, 690),
    ("isogrid", "layout 1", 9072, 690),
    ("isogrid", "palette", 2187, 1045),
    ("isogrid", "escape", 1275, 690),
    ("isogrid", "idle end", 935, 690),
    ("long", "idle fresh", 739, 663),
    ("long", "split", 13518, 764),
    ("long", "close", 4636, 262),
    ("long", "undo", 6171, 349),
    ("long", "idle after session", 289, 228),
];

struct P {
    e: Option<editor::Editor>,
    tick: i64,
    canvas: rays::canvas::Canvas,
    rows: Vec<(String, f64, u64, u64)>,
}

impl P {
    fn step(&mut self, keys: &[Key], events: Vec<Event>, pointer: (f64, f64)) -> (f64, u64) {
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
    fn record(&mut self, name: &str, frames: Vec<(f64, u64)>) {
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

fn session(label: &str, source: &str, actions: &dyn Fn(&mut P)) -> Vec<(String, f64, u64, u64)> {
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

/// a long session: 20 split/close pairs, then 20 undos, then idle; the settled idle must cost what a
/// fresh editor's does (history depth and closed panes cost nothing)
fn long(p: &mut P) {
    let ch = Key::KeyChar;
    p.act("idle fresh", &[], &[Key::Shift]);
    for _ in 0..20 {
        p.act("split", &[], &[ch('/'), ch('o'), ch('v')]);
        p.act("close", &[], &[ch('/'), ch('o'), ch('x')]);
    }
    for _ in 0..20 { p.act("undo", &[Key::Meta], &[ch('z')]); }
    p.act("idle after session", &[], &[Key::Shift]);
}

fn sketch(name: &str) -> String { std::fs::read_to_string(format!("{}/../../sketches/{name}", env!("CARGO_MANIFEST_DIR"))).unwrap() }

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
    let rows = session("long", &sketch("basic.rays"), &long);
    let fresh = rows.iter().find(|r| r.0 == "idle fresh").unwrap().3;
    let after = rows.iter().find(|r| r.0 == "idle after session").unwrap().3;
    eprintln!("CLICK long session: settled idle {fresh} fresh, {after} after 40 edits and 20 undos");
    check("long", &rows);
}

pub fn cases() -> Vec<(&'static str, fn())> {
    vec![("click_budget_basic", basic), ("click_budget_empty", empty), ("click_budget_isogrid", isogrid), ("click_budget_long", long_session)]
}
