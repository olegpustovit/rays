//! The graph pane's pixels through the Rays renderer, as the editor builds it (`update_cached` in a
//! pane root), at 1x and 2x: goldens `crates/rays_graph/fixtures/graph_<case>_{1,2}x.png`
//! (`RAYS_UPDATE_FIXTURES=1` rewrites them). They hold the pixels of the canvas-painted pane the
//! retained pane replaced, so they are refreshed only for a documented fix.
use crate::common;
use crate::support::*;
use input::{Event, MouseButton, Platform};
use rays_graph::scope_state::Change;
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
pub fn pane_frame(ui: &mut Ui, t: &mut Pane, fr: &rframe::Frame) -> Vec<Change> {
    let mut changes = Vec::new();
    ui.frame(fr, &mut |ui: &mut Ui| {
        let root = ui.box_("graph-root", BoxOpts::default().w(Size::Px(W as f32)).h(Size::Px(H as f32)).at((0.0, 0.0)));
        ui.within(&root, &mut |ui| scope::update_cached("graph-view", &[], t, ui, fr, &mut changes));
    });
    changes
}

pub fn golden_ui() -> Ui {
    let path = format!("{}/../../pxui/assets/fonts/DepartureMono-Regular.otf", env!("CARGO_MANIFEST_DIR"));
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
    // Metal rasterises the goldens: exact. Vulkan blends and filters a channel off by 1-2 (the
    // renderer's own sampler tests allow the same on vulkan), so there a pixel differs past 2.
    let tolerance = if ogpu_core::impl_::backend().expect("backend") == "vulkan" { 2 } else { 0 };
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
            let different = actual.chunks(4).zip(expected.chunks(4)).filter(|(a, b)| a.iter().zip(*b).any(|(x, y)| x.abs_diff(*y) > tolerance)).count();
            if actual.len() != expected.len() || different != 0 {
                failures.push(format!("{} at {density}x: {different} pixels differ (max channel {worst})", c.name));
                let _ = canvas::save_png(&canvas_, &format!("{}/graph_{}_{density}x.actual.png", std::env::temp_dir().display(), c.name));
            }
            let _ = canvas::destroy(&canvas_);
        }
    }
    assert!(failures.is_empty(), "graph pane pixels: {}", failures.join("; "));
}

/// A frame of the pane: the views it built, the instance bytes it wrote, what it allocated and the
/// changes it emitted.
struct Step {
    views: usize,
    bytes: usize,
    allocs: u64,
    changes: Vec<Change>,
}

fn measured(ui: &mut Ui, t: &mut Pane, fr: &rframe::Frame) -> Step {
    mem_sys::local_reset();
    let before = mem_sys::local_alloc_count();
    let mut changes = Vec::new();
    ui.frame(fr, &mut |ui: &mut Ui| {
        let root = ui.box_("graph-root", BoxOpts::default().w(Size::Px(W as f32)).h(Size::Px(H as f32)).at((0.0, 0.0)));
        ui.within(&root, &mut |ui| scope::update_cached("graph-view", &[], t, ui, fr, &mut changes));
    });
    let allocs = mem_sys::local_alloc_count() - before;
    let out = ui.output();
    let bytes = out.draw.iter().flat_map(|l| l.dirty.iter()).map(|(a, b)| (b - a) as usize).sum::<usize>() * std::mem::size_of::<pxui::draw::Instance>();
    for i in 0..out.draw.len() {
        ui.drawn(i);
    }
    Step { views: ui.rebuilt_views(), bytes, allocs, changes }
}

/// The pane's content is one cached view that a pan offsets and a zoom scales: a right drag on the
/// bare canvas or a wheel step builds no view, writes no instance and allocates nothing until the
/// pane would show past the overscan (one build) or the zoom has rested 0.1 s (one build at the
/// zoom), and a click finds a card through the moved and scaled content at zoom 0.5, 1, 1.5 and 2.
/// Run twice: checking every kept build (`verify_views`), then counting.
fn window_graph_view_pan_zoom() {
    for verify in [true, false] {
        let ws = checked(SMALL);
        let sc = Rc::new(flow_graph::projection::of_graph(&scope_catalog(), &ws, "g").unwrap_or_else(|e| panic!("{e}")));
        let mut t = scope::create(Some(W), Some(H));
        scope::with_scope(ScopeOpts::default(), "g", sc, &mut t);
        scope::with_records(recorded(&ws), &mut t);
        let mut ui = golden_ui();
        ui.verify_views(verify);
        let count = std::cell::Cell::new(0);
        let step = |ui: &mut Ui, t: &mut Pane, pointer: (f64, f64), buttons: &[MouseButton], events: Vec<Event>| {
            let mut fr = frame_at(count.get(), 1, pointer, events);
            fr.input.buttons = buttons.to_vec();
            count.set(count.get() + 1);
            measured(ui, t, &fr)
        };
        let quiet = |s: &Step, what: &str| {
            // (a verifying run builds every kept view again)
            check(verify || s.views == 0, &format!("{what}: {} views built", s.views));
            check(s.bytes == 0, &format!("{what}: {} instance bytes written", s.bytes));
            check(verify || s.allocs == 0, &format!("{what}: {} allocations", s.allocs));
        };
        let away = (-100.0, -100.0);
        for _ in 0..4 {
            step(&mut ui, &mut t, away, &[], vec![Event::PointerMoved(away)]);
        }
        // a right drag on the bare canvas, 2 points a frame
        let mut p = (8.0, H as f64 - 8.0);
        step(&mut ui, &mut t, p, &[MouseButton::RightButton], vec![Event::PointerMoved(p), Event::PointerPressed(MouseButton::RightButton, p)]);
        let pan0 = (t.pan_x, t.pan_y);
        for i in 0..30 {
            p.0 += 2.0;
            let s = step(&mut ui, &mut t, p, &[MouseButton::RightButton], vec![Event::PointerMoved(p)]);
            quiet(&s, &format!("pan frame {i}"));
        }
        check(t.pan_x == pan0.0 + 60.0 && t.pan_y == pan0.1, &format!("the drag panned {:?} -> {:?}", pan0, (t.pan_x, t.pan_y)));
        // past the overscan (half the pane each side): one build at the new pan of the anchored views
        // (the wires, the content and each card's view in it)
        let anchored = |t: &Pane| 2 + scope::stats(t).drawn_items as usize;
        p.0 += W as f64 / 2.0;
        let s = step(&mut ui, &mut t, p, &[MouseButton::RightButton], vec![Event::PointerMoved(p)]);
        check(verify || s.views == anchored(&t), &format!("crossing the overscan built {} views", s.views));
        p.0 += 2.0;
        quiet(&step(&mut ui, &mut t, p, &[MouseButton::RightButton], vec![Event::PointerMoved(p)]), "a pan frame after the build");
        step(&mut ui, &mut t, p, &[], vec![Event::PointerReleased(MouseButton::RightButton, p)]);
        for _ in 0..2 {
            step(&mut ui, &mut t, p, &[], vec![]);
        }
        // wheel steps at the pointer: scaled, then one build once the zoom rests
        let z0 = t.zoom;
        for _ in 0..3 {
            let s = step(&mut ui, &mut t, p, &[], vec![Event::Scrolled((0.0, 1.0))]);
            quiet(&s, "a wheel step");
        }
        check(t.zoom > z0, "the wheel did not zoom");
        let mut built = 0;
        for _ in 0..8 {
            built += step(&mut ui, &mut t, p, &[], vec![]).views;
        }
        check(verify || built == anchored(&t), &format!("the rested zoom built {built} views"));
        // a click on a card through the content scaled to each zoom (not rested: the view's transform)
        for z in [0.5, 1.0, 1.5, 2.0] {
            scope::frame_all(&mut t);
            for _ in 0..8 {
                step(&mut ui, &mut t, away, &[], vec![Event::PointerMoved(away)]);
            }
            let rested = (t.pan_x, t.pan_y, t.zoom);
            zoomed(&mut t, z);
            t.zoom_t = count.get() as f64 / 60.0;
            let card = path(&["g", "c"]);
            let (x, y, w, h) = scope::inspect::box_of(&t, &card).expect("c");
            let at = (x + w / 2.0, y + h.min(24.0 * z) / 2.0);
            let s = step(&mut ui, &mut t, at, &[], vec![Event::PointerMoved(at)]);
            check(verify || t.zoom == rested.2 || s.views == 0, &format!("a zoom built {} views", s.views));
            step(&mut ui, &mut t, at, &[MouseButton::LeftButton], vec![Event::PointerPressed(MouseButton::LeftButton, at)]);
            let s = step(&mut ui, &mut t, at, &[], vec![Event::PointerReleased(MouseButton::LeftButton, at)]);
            let picked = s.changes.iter().any(|c| matches!(c, Change::Selected)) && scope::selected(&t).contains(&card);
            check(x > 0.0 && x + w < W as f64, &format!("zoom {z}: the card is not in the pane"));
            check(picked || scope::selected(&t).contains(&card), &format!("a click at zoom {z} (shown at {:.2}) did not find the card", t.zoom));
            scope::clear_selection(&mut t);
            for _ in 0..8 {
                step(&mut ui, &mut t, away, &[], vec![Event::PointerMoved(away)]);
            }
        }
        ui.destroy();
    }
}

/// The pointer over the pane builds no content: a card's hover is its ground's look and its row's
/// highlight, patched (that card's instances only, nothing allocated warm); a port's lit wires are a
/// small view over the content; selecting builds the content once.
fn window_graph_hover() {
    for verify in [true, false] {
        let ws = checked(SMALL);
        let sc = Rc::new(flow_graph::projection::of_graph(&scope_catalog(), &ws, "g").unwrap_or_else(|e| panic!("{e}")));
        let mut t = scope::create(Some(W), Some(H));
        scope::with_scope(ScopeOpts::default(), "g", sc, &mut t);
        scope::with_records(recorded(&ws), &mut t);
        let mut ui = golden_ui();
        ui.verify_views(verify);
        let count = std::cell::Cell::new(0);
        let step = |ui: &mut Ui, t: &mut Pane, at: (f64, f64), events: Vec<Event>| {
            let fr = frame_at(count.get(), 1, at, events);
            count.set(count.get() + 1);
            measured(ui, t, &fr)
        };
        let to = |ui: &mut Ui, t: &mut Pane, at: (f64, f64)| step(ui, t, at, vec![Event::PointerMoved(at)]);
        let away = (-100.0, -100.0);
        for _ in 0..4 {
            to(&mut ui, &mut t, away);
        }
        let (c, d) = (path(&["g", "c"]), path(&["g", "d"]));
        let head = |t: &Pane, p: &[flow_syntax::symbol::Symbol]| {
            let (x, y, w, _) = scope::inspect::box_of(t, p).expect("card");
            (x + w / 2.0, y + 6.0)
        };
        let (hc, hd) = (head(&t, &c), head(&t, &d));
        let row = scope::inspect::row_center(&t, &c, 1).expect("a row of c");
        let canvas = (8.0, H as f64 - 8.0);
        // warm: every place once
        for at in [hc, row, hd, canvas, hc, canvas] {
            to(&mut ui, &mut t, at);
        }
        let quiet = |s: &Step, what: &str, bytes: usize| {
            check(verify || s.views == 0, &format!("{what}: {} views built", s.views));
            check(verify || s.bytes <= bytes, &format!("{what}: {} instance bytes written (at most {bytes})", s.bytes));
            check(verify || s.allocs == 0, &format!("{what}: {} allocations", s.allocs));
        };
        // a card's ground (its fill and edge) and its row band; a row, the band moved and filled
        let card = 5 * std::mem::size_of::<pxui::draw::Instance>();
        quiet(&to(&mut ui, &mut t, hc), "onto a card", card);
        quiet(&to(&mut ui, &mut t, (hc.0 + 3.0, hc.1)), "within a card", 0);
        quiet(&to(&mut ui, &mut t, row), "onto its row", 3 * std::mem::size_of::<pxui::draw::Instance>());
        check(scope::hovered_row(&t).is_some_and(|(p, _)| p == c), "the hovered row is not c's");
        quiet(&to(&mut ui, &mut t, hd), "to another card", 2 * card);
        quiet(&to(&mut ui, &mut t, canvas), "off the cards", card);
        check(scope::hovered_row(&t).is_none(), "a row stayed hovered off the cards");
        // a port lights its wires in the view over the content
        let port = scope::inspect::output_socket(&t, &d).expect("d's out-port");
        let s = to(&mut ui, &mut t, port);
        check(!t.highlighted.is_empty(), "the out-port lit no wire");
        check(verify || s.views == 1, &format!("hovering a port built {} views (the hover's alone)", s.views));
        to(&mut ui, &mut t, canvas);
        check(t.highlighted.is_empty(), "wires stayed lit off the port");
        // selecting a card: one content build
        step(&mut ui, &mut t, hc, vec![Event::PointerMoved(hc), Event::PointerPressed(MouseButton::LeftButton, hc)]);
        let s = step(&mut ui, &mut t, hc, vec![Event::PointerReleased(MouseButton::LeftButton, hc)]);
        check(scope::selected(&t).contains(&c), "the click did not select c");
        check(verify || s.views <= 1, &format!("the release built {} views", s.views));
        // to a screen reader the cards are groups named by their titles, their ports buttons
        ui.set_a11y(true);
        to(&mut ui, &mut t, away);
        let tree = ui.output().a11y;
        let named = |name: &str, role: pxui::a11y::Role| tree.nodes.iter().any(|n| n.role == role && &tree.strings[n.name.start as usize..n.name.end as usize] == name);
        check(named("c", pxui::a11y::Role::Group) && named("d", pxui::a11y::Role::Group), "the cards are not named groups");
        check(named("targets", pxui::a11y::Role::Button), "c's targets port is not a named button");
        ui.destroy();
    }
}

/// A folded zone and a chip show their hover through the patch path as a card does: no view built,
/// their ground's instances alone written, nothing allocated (checked with `verify_views` too).
fn window_graph_folded_hover() {
    for verify in [true, false] {
        let text = "(workspace w (graph g :context value (let* [k 6.0 ys (for [i (range 4)] (let* [a (* k (float i)) b (+ a 1.0)] b)) n (reduce (fn [acc x] (+ acc x)) 0.0 ys)] n)))";
        let ws = checked(text);
        let sc = Rc::new(flow_graph::projection::of_graph(&scope_catalog(), &ws, "g").unwrap_or_else(|e| panic!("{e}")));
        let mut t = scope::create(Some(W), Some(H));
        let saved = TestSaved { collapsed: vec![path(&["g", "ys"])], level: vec![(path(&["g", "k"]), (flow_graph::projection::Level::Chip, true))], ..Default::default() };
        scope::with_scope(saved_opts(saved), "g", sc, &mut t);
        scope::with_records(recorded(&ws), &mut t);
        let mut ui = golden_ui();
        ui.verify_views(verify);
        let count = std::cell::Cell::new(0);
        let to = |ui: &mut Ui, t: &mut Pane, at: (f64, f64)| {
            let fr = frame_at(count.get(), 1, at, vec![Event::PointerMoved(at)]);
            count.set(count.get() + 1);
            measured(ui, t, &fr)
        };
        let away = (-100.0, -100.0);
        for _ in 0..4 {
            to(&mut ui, &mut t, away);
        }
        let head = |t: &Pane, name: &str| {
            let (x, y, w, _) = scope::inspect::box_of(t, &path(&["g", name])).expect("card");
            (x + w / 2.0, y + 6.0)
        };
        let (zone, chip) = (head(&t, "ys"), head(&t, "k"));
        for at in [zone, away, chip, away] {
            to(&mut ui, &mut t, at);
        }
        let ground = 5 * std::mem::size_of::<pxui::draw::Instance>();
        for (at, what) in [(zone, "onto the folded zone"), (away, "off it"), (chip, "onto the chip"), (away, "off it")] {
            let s = to(&mut ui, &mut t, at);
            check(verify || s.views == 0, &format!("{what}: {} views built", s.views));
            check(verify || s.bytes <= ground, &format!("{what}: {} instance bytes", s.bytes));
            check(verify || s.allocs == 0, &format!("{what}: {} allocations", s.allocs));
        }
        ui.destroy();
    }
}

pub const CASES: &[(&str, fn())] = &[("gpu_graph_pixels", gpu_graph_pixels), ("window_graph_view_pan_zoom", window_graph_view_pan_zoom), ("window_graph_hover", window_graph_hover), ("window_graph_folded_hover", window_graph_folded_hover)];
