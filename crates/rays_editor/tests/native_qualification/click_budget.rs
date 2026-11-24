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
    ("basic", "idle", 785, 710),
    ("basic", "hover", 722, 723),
    ("basic", "split right", 27436, 13432),
    ("basic", "retype graph", 20981, 12999),
    ("basic", "retype inspector", 20984, 12949),
    ("basic", "retype lisp", 20921, 29101),
    ("basic", "retype list", 33512, 13014),
    ("basic", "retype spreadsheet", 20993, 12993),
    ("basic", "retype outline", 20965, 13250),
    ("basic", "close panel", 20951, 12877),
    ("basic", "split below", 21090, 16660),
    ("basic", "toggle inspector", 13132, 13045),
    ("basic", "toggle graph", 13264, 12942),
    ("basic", "toggle graph back", 13166, 13013),
    ("basic", "toggle timeline", 13235, 13142),
    ("basic", "undo", 13324, 13020),
    ("basic", "undo 2", 14315, 12953),
    ("basic", "redo", 13133, 13020),
    ("basic", "new layout", 21714, 13178),
    ("basic", "layout 0", 21647, 13182),
    ("basic", "layout 1", 21625, 13182),
    ("basic", "palette", 13878, 13457),
    ("basic", "escape", 13320, 13182),
    ("basic", "idle end", 13175, 13120),
    ("empty", "idle", 406, 332),
    ("empty", "hover", 345, 345),
    ("empty", "split right", 6817, 13017),
    ("empty", "retype graph", 18802, 13076),
    ("empty", "retype inspector", 33114, 13005),
    ("empty", "retype lisp", 18767, 27309),
    ("empty", "retype list", 31368, 13098),
    ("empty", "retype spreadsheet", 18860, 13048),
    ("empty", "retype outline", 18813, 13293),
    ("empty", "close panel", 18886, 12988),
    ("empty", "split below", 19010, 14875),
    ("empty", "toggle inspector", 13480, 13076),
    ("empty", "toggle graph", 13454, 13011),
    ("empty", "toggle graph back", 13397, 13087),
    ("empty", "toggle timeline", 13469, 13221),
    ("empty", "undo", 13541, 13097),
    ("empty", "undo 2", 13632, 13026),
    ("empty", "redo", 13349, 13097),
    ("empty", "new layout", 20231, 13363),
    ("empty", "layout 0", 20116, 13367),
    ("empty", "layout 1", 20093, 13367),
    ("empty", "palette", 14068, 13641),
    ("empty", "escape", 13505, 13367),
    ("empty", "idle end", 13349, 13295),
    ("isogrid", "idle", 822, 728),
    ("isogrid", "hover", 742, 742),
    ("isogrid", "split right", 177452, 755),
    ("isogrid", "retype graph", 917, 755),
    ("isogrid", "retype inspector", 176628, 755),
    ("isogrid", "retype lisp", 917, 13324),
    ("isogrid", "retype list", 13650, 894),
    ("isogrid", "retype spreadsheet", 176934, 894),
    ("isogrid", "retype outline", 176936, 894),
    ("isogrid", "close panel", 9807, 894),
    ("isogrid", "split below", 177421, 894),
    ("isogrid", "toggle inspector", 2667, 893),
    ("isogrid", "toggle graph", 3803, 839),
    ("isogrid", "toggle graph back", 2832, 897),
    ("isogrid", "toggle timeline", 2986, 1264),
    ("isogrid", "undo", 3017, 905),
    ("isogrid", "undo 2", 13868, 849),
    ("isogrid", "redo", 2869, 905),
    ("isogrid", "new layout", 211548, 1146),
    ("isogrid", "layout 0", 214926, 1148),
    ("isogrid", "layout 1", 211557, 1148),
    ("isogrid", "palette", 2187, 1508),
    ("isogrid", "escape", 1380, 1149),
    ("isogrid", "idle end", 1161, 1101),
    ("long", "idle fresh", 785, 710),
    ("long", "split", 27437, 13432),
    ("long", "close", 21879, 13243),
    ("long", "undo", 20885, 13439),
    ("long", "idle after session", 560, 500),
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
