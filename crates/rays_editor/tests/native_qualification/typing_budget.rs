//! What a keystroke in a Lisp pane costs (UI fleet L6, docs/fleet-ui.md): opens a Lisp pane beside
//! isogrid.rays, focuses it and types 20 characters. Allocations per keystroke have an exact ceiling
//! (measured + 5%, never raised); the times are printed. Also: a draft asks for the frame its live
//! check is due in (B46).
use input::{Event, Key, MouseButton};
use rays_editor::{editor, workspace};
use std::time::Instant;

const W: i64 = 1280;
const H: i64 = 900;
/// allocations on the worst of 20 keystrokes, and on the settled frame after them
const CEILING: (u64, u64) = (2974, 873);

struct P {
    e: Option<editor::Editor>,
    tick: i64,
    canvas: rays::canvas::Canvas,
}

impl P {
    /// (ms, allocations, the UI's wake, the frame's time)
    fn step(&mut self, keys: &[Key], events: Vec<Event>, pointer: (f64, f64)) -> (f64, u64, Option<f64>, f64) {
        self.tick += 1;
        let t = self.tick;
        let f = rays::frame::Frame {
            input: input::Frame { dt: 1. / 60., frame: t, tick: t, size: (W, H), pointer, keys: keys.to_vec(), events, ..input::at_time(t as f64 / 60.) },
            pixel_scale: (1., 1.), fps: 60., mouse_delta: (0., 0.) };
        let n0 = mem_sys::alloc_count();
        let s = Instant::now();
        let m = editor::update(self.e.take().unwrap(), &f);
        assert!(m.context_error.is_none(), "{:?}", m.context_error);
        rays::canvas::render(None, &self.canvas, &editor::scene(&m, &f)).unwrap();
        let wake = editor::private::ui(&m).borrow().repaint_at();
        self.e = Some(editor::after_present(m, &f));
        (s.elapsed().as_secs_f64() * 1e3, mem_sys::alloc_count() - n0, wake, t as f64 / 60.)
    }
}

/// isogrid.rays with a Lisp pane split to the right and its text focused (a click inside it)
fn open_lisp() -> (P, flow_native::workspace_gpu::WorkspaceGpu, (f64, f64)) {
    assert!(crate::common::gpu(), "native GPU required");
    let source = std::fs::read_to_string(format!("{}/../../sketches/isogrid.rays", env!("CARGO_MANIFEST_DIR"))).unwrap();
    let doc = workspace::load(None, Vec::new(), None, &source).unwrap_or_else(|d| panic!("{d:?}"));
    let canvas = rays::canvas::create(W, H).unwrap();
    let gpu = flow_native::workspace_gpu::create(None);
    let e = editor::create(editor::Create { gpu_owner: Some(gpu.clone()), await_: Some(true), platform: Some(input::Platform::Apple), ..Default::default() }, &doc, None, editor::prepare_fn(), editor::draw_fn()).unwrap();
    e.core.ui.borrow_mut().verify_views(false);
    let mut p = P { e: Some(e), tick: 0, canvas };
    let ch = Key::KeyChar;
    let click = (W as f64 * 0.75, H as f64 * 0.4);
    for _ in 0..30 { p.step(&[], vec![], click); }
    for k in [ch('/'), ch('o'), ch('v'), ch('/'), ch('l'), ch('t')] { p.step(&[], vec![Event::KeyPressed(k)], click); }
    for _ in 0..4 { p.step(&[], vec![], click); }
    p.step(&[], vec![Event::PointerMoved(click)], click);
    p.step(&[], vec![Event::PointerPressed(MouseButton::LeftButton, click), Event::PointerReleased(MouseButton::LeftButton, click)], click);
    // past the 2 Hz autosave of the layout edits, which is not a keystroke
    for _ in 0..40 { p.step(&[], vec![], click); }
    (p, gpu, click)
}

fn close(p: P, gpu: flow_native::workspace_gpu::WorkspaceGpu) {
    editor::close(p.e.as_ref().unwrap());
    flow_native::workspace_gpu::close(&gpu);
    rays::canvas::destroy(&p.canvas).unwrap();
}

/// B44: the right-click menu carries the editing, search and navigation commands; Find runs
fn menu() {
    let (mut p, gpu, click) = open_lisp();
    let summary = |p: &P| editor::private::state_text(p.e.as_ref().unwrap()).lines().find(|l| l.starts_with("text: ")).unwrap().to_owned();
    let right = MouseButton::RightButton;
    p.step(&[], vec![Event::PointerPressed(right, click), Event::PointerReleased(right, click)], click);
    p.step(&[], vec![], click);
    assert!(summary(&p).contains("menu open"), "right-click opens the menu: {}", summary(&p));
    // rows are 24 points, the separators shorter: Find is the sixth entry, below Cut, Copy, Paste, Select All
    // (measured at 114 points under the click; a click above it would use the real clipboard)
    let at = (click.0 + 24., click.1 + 114.);
    p.step(&[], vec![Event::PointerMoved(at)], at);
    p.step(&[], vec![Event::PointerPressed(MouseButton::LeftButton, at), Event::PointerReleased(MouseButton::LeftButton, at)], at);
    p.step(&[], vec![], at);
    p.step(&[], vec![], at);
    assert!(summary(&p).contains(", find "), "the menu's Find opened the find bar: {}", summary(&p));
    close(p, gpu);
}

fn typing() {
    let (mut p, gpu, click) = open_lisp();
    let step = |p: &mut P, keys: &[Key], events: Vec<Event>, pointer: (f64, f64)| p.step(keys, events, pointer);
    let mut worst = (0.0f64, 0u64);
    let mut total_ms = 0.0;
    for i in 0..20 {
        let (ms, n, wake, now) = step(&mut p, &[], vec![Event::TextInput(["x", "y"][i % 2].to_string())], click);
        if i == 1 {
            let at = wake.expect("B46: a draft asks for the frame its live check is due in");
            assert!(at.is_finite() && at > now - 1. / 60. && at <= now + 0.31, "the wake is a time ahead, got {at}");
        }
        eprintln!("TYPING key {i:2} {ms:7.2}ms {n:7}a");
        total_ms += ms;
        worst = (worst.0.max(ms), worst.1.max(n));
    }
    let (ms, settled, _, _) = step(&mut p, &[], vec![], click);
    eprintln!("TYPING worst keystroke {:.2}ms {}a; mean {:.2}ms; settled after {:.2}ms {}a", worst.0, worst.1, total_ms / 20.0, ms, settled);
    eprintln!("TYPING ceiling now: ({}, {})", worst.1, settled);
    assert!(worst.1 <= CEILING.0 && settled <= CEILING.1, "typing over its ceiling: {}/{} over {}/{}", worst.1, settled, CEILING.0, CEILING.1);
    close(p, gpu);
}

pub fn cases() -> Vec<(&'static str, fn())> {
    vec![("typing_budget", typing), ("text_menu", menu)]
}
