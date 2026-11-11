//! The graph pane's pixels through the Rays renderer, as the editor builds it (`update_cached` in a
//! pane root), at 1x and 2x: goldens `crates/rays_graph/fixtures/graph_<case>_{1,2}x.png`
//! (`RAYS_UPDATE_FIXTURES=1` rewrites them). They hold the pixels of the canvas-painted pane the
//! retained pane replaced, so they are refreshed only for a documented fix.
use crate::common;
use crate::support::*;
use input::{Event, Platform};
use pxui::ui::*;
use rays::{canvas, frame as rframe, image, scene};
use rays_graph::scope_pane as scope;
use rays_graph::scope_state::{Frame3, Pane, ScopeOpts};
use rays_math::color;
use std::cell::RefCell;
use std::rc::Rc;

const W: i64 = 560;
const H: i64 = 360;

fn frame_at(count: i64, density: i64, pointer: (f64, f64), events: Vec<Event>) -> rframe::Frame {
    let mut input = input::at_time(count as f64 / 60.0);
    input.dt = 1.0 / 60.0;
    (input.frame, input.tick) = (count, count);
    input.size = (W, H);
    input.pointer = pointer;
    input.events = events;
    rframe::Frame { input, pixel_scale: (density as f64, density as f64), fps: 60.0, mouse_delta: (0.0, 0.0) }
}

/// One editor-like frame: the pane in a root box, cached as the editor caches it.
pub fn pane_frame(ui: &mut Ui, t: &mut Pane, fr: &rframe::Frame) {
    ui.frame(fr, &mut |ui: &mut Ui| {
        let root = ui.box_("graph-root", BoxOpts::default().w(Size::Px(W as f32)).h(Size::Px(H as f32)).at((0.0, 0.0)));
        ui.within(&root, &mut |ui| scope::update_cached("graph-view", &[], t, ui, fr, &mut Vec::new()));
    });
}

pub fn golden_ui() -> Ui {
    let path = format!("{}/../../assets/fonts/DepartureMono-Regular.otf", env!("CARGO_MANIFEST_DIR"));
    let mut ui = Ui::create(Platform::Apple, Some(11), rays_shell::host::text(Some(&path)));
    ui.verify_views(true);
    ui
}

struct Case {
    name: &'static str,
    text: &'static str,
    opts: fn() -> ScopeOpts,
    /// after the pane settled: a zoom, a selection, refused nodes; and a point to hover
    setup: fn(&mut Pane) -> Option<(f64, f64)>,
}

fn none(_: &mut Pane) -> Option<(f64, f64)> {
    None
}

fn zoomed(t: &mut Pane, z: f64) {
    // about the pane's centre, as a pinch there would
    let (cx, cy) = (t.width as f64 / 2.0, t.height as f64 / 2.0);
    let k = z / t.zoom;
    (t.zoom, t.pan_x, t.pan_y) = (z, cx - (cx - t.pan_x) * k, cy - (cy - t.pan_y) * k);
    (t.pan_x, t.pan_y) = (t.pan_x.round(), t.pan_y.round());
}

const SMALL: &str = "(workspace z (graph g :context sop [(n : int 3)] (let* [a (sop/box) b (sop/scatter a :count n) c (sop/copy_to_points a b) d (sop/transform c :translate [0 1 0])] (sop/merge c d))))";

fn cases() -> Vec<Case> {
    vec![
        Case { name: "small", text: SMALL, opts: ScopeOpts::default, setup: none },
        Case {
            name: "zones",
            text: "(workspace w (graph g :context value (let* [k 6.0 ys (for [i (range 4)] (let* [a (* k (float i)) b (+ a 1.0)] b)) n (reduce (fn [acc x] (+ acc x)) 0.0 ys) x (cond false (+ 1 2) true (* 3 4) :else 5)] (+ n x))))",
            opts: || {
                let mut s = TestSaved::default();
                s.frames.push((path(&["g"]), vec![("loop".to_owned(), (-20.0, -40.0), (420.0, 120.0)) as Frame3]));
                saved_opts(s)
            },
            setup: none,
        },
        Case {
            name: "states",
            text: "(workspace r (graph g :context sop (let* [a (sop/box) b (sop/transform a) c (sop/transform b :uniform_scale 2) d (sop/transform a) e (sop/merge c d a)] e)))",
            opts: ScopeOpts::default,
            setup: |t| {
                scope::select(vec![path(&["g", "b"])], t);
                scope::with_failed(vec![(path(&["g", "c"]), "E0001".to_owned())], t);
                let (x, y, w, _) = scope::inspect::box_of(t, &path(&["g", "d"])).expect("d");
                Some((x + w / 2.0, y + 6.0))
            },
        },
        Case { name: "zoom_half", text: SMALL, opts: ScopeOpts::default, setup: |t| { zoomed(t, 0.5); None } },
        Case { name: "zoom_double", text: SMALL, opts: ScopeOpts::default, setup: |t| { zoomed(t, 2.0); None } },
    ]
}

/// The pane of `c` settled and rendered at `density` (pointer at the hover point, if any).
fn render(c: &Case, density: i64) -> canvas::Canvas {
    let ws = checked(c.text);
    let sc = Rc::new(flow_graph::projection::of_graph(&scope_catalog(), &ws, "g").unwrap_or_else(|e| panic!("{e}")));
    let mut t = scope::create(Some(W), Some(H));
    scope::with_scope((c.opts)(), "g", sc, &mut t);
    scope::with_records(recorded(&ws), &mut t);
    let ui = Rc::new(RefCell::new(golden_ui()));
    let away = (-100.0, -100.0);
    let mut count = 0;
    let mut step = |t: &mut Pane, pointer: (f64, f64)| {
        pane_frame(&mut ui.borrow_mut(), t, &frame_at(count, density, pointer, vec![Event::PointerMoved(pointer)]));
        count += 1;
    };
    for _ in 0..3 {
        step(&mut t, away);
    }
    let hover = (c.setup)(&mut t);
    for _ in 0..3 {
        step(&mut t, hover.unwrap_or(away));
    }
    let canvas_ = canvas::create(W * density, H * density).expect("canvas");
    let mut nodes = vec![scene::clear(color::rgb(228, 139, 161))];
    nodes.extend(rays_shell::host::scene(&ui, None));
    canvas::render(Some(density), &canvas_, &nodes).expect("Canvas.render");
    ui.borrow_mut().destroy();
    canvas_
}

/// The pane at rest, idle and with a selection, a hovered card and a refused node, with zones and
/// a titled frame, and zoomed to a half and to two, matches its goldens exactly.
fn gpu_graph_pixels() {
    if !common::gpu() {
        return;
    }
    let update = std::env::var("RAYS_UPDATE_FIXTURES").as_deref() == Ok("1");
    let mut failures = Vec::new();
    for c in cases() {
        for density in [1, 2] {
            let canvas_ = render(&c, density);
            let golden = format!("{}/../rays_graph/fixtures/graph_{}_{density}x.png", env!("CARGO_MANIFEST_DIR"), c.name);
            if update {
                canvas::save_png(&canvas_, &golden).expect("refresh golden");
            }
            let actual = image::pixels(&canvas::to_image(&canvas_).expect("to_image")).expect("pixels");
            let expected = image::pixels(&image::load(&golden).expect("golden")).expect("pixels");
            let worst = actual.iter().zip(&expected).map(|(a, b)| a.abs_diff(*b)).max().unwrap_or(0);
            let different = actual.chunks(4).zip(expected.chunks(4)).filter(|(a, b)| a != b).count();
            if actual.len() != expected.len() || different != 0 {
                failures.push(format!("{} at {density}x: {different} pixels differ (max channel {worst})", c.name));
                let _ = canvas::save_png(&canvas_, &format!("{}/graph_{}_{density}x.actual.png", std::env::temp_dir().display(), c.name));
            }
            let _ = canvas::destroy(&canvas_);
        }
    }
    assert!(failures.is_empty(), "graph pane pixels: {}", failures.join("; "));
}

pub const CASES: &[(&str, fn())] = &[("gpu_graph_pixels", gpu_graph_pixels)];
