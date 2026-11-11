//! What each gesture on the graph pane costs a frame, the pane cached as the editor caches it: PXUI
//! CPU (`Ui::frame` phases), nodes declared, views built, content builds, instance bytes written and
//! allocations, over a 40-node graph at zoom 1 (one node a chip). Prints a table (min and median UI
//! CPU per frame) and asserts the counters: the pointer, a selection and the drags of cards and
//! wires build no content and allocate nothing once warm; a drop and an edit build it once.
use crate::support::*;
use flow_graph::projection::Level;
use input::{Event, MouseButton, Platform};
use pxui::ui::*;
use rays::frame as rframe;
use rays_graph::scope_pane as scope;
use rays_graph::scope_state::{Drag, Pane};
use std::rc::Rc;

const W: i64 = 1000;
const H: i64 = 700;
const INSTANCE: usize = std::mem::size_of::<pxui::draw::Instance>();

#[derive(Clone, Copy, Default)]
struct Cost {
    us: f64,
    nodes: usize,
    views: usize,
    builds: u64,
    bytes: usize,
    allocs: u64,
}

fn frame(count: i64, pointer: (f64, f64), buttons: &[MouseButton], events: Vec<Event>) -> rframe::Frame {
    let mut input = input::at_time(count as f64 / 60.0);
    input.dt = 1.0 / 60.0;
    (input.frame, input.tick) = (count, count);
    input.size = (W, H);
    input.pointer = pointer;
    input.buttons = buttons.to_vec();
    input.events = events;
    rframe::Frame { input, pixel_scale: (2.0, 2.0), fps: 60.0, mouse_delta: (0.0, 0.0) }
}

thread_local! { static CHANGES: std::cell::RefCell<Vec<rays_graph::scope_state::Change>> = std::cell::RefCell::new(Vec::with_capacity(16)); }

fn one(ui: &mut Ui, t: &mut Pane, fr: &rframe::Frame) -> Cost {
    ui.set_timing(true);
    let builds = scope::inspect::content_builds(t);
    mem_sys::local_reset();
    let before = mem_sys::local_alloc_count();
    ui.frame(fr, &mut |ui: &mut Ui| {
        let root = ui.box_("graph-root", BoxOpts::default().w(Size::Px(W as f32)).h(Size::Px(H as f32)).at((0.0, 0.0)));
        // the intents in a list kept from frame to frame, as the editor keeps its own
        CHANGES.with(|c| {
            let mut c = c.borrow_mut();
            c.clear();
            ui.within(&root, &mut |ui| scope::update_cached("graph-view", &[], t, ui, fr, &mut c));
        });
    });
    let allocs = mem_sys::local_alloc_count() - before;
    let us = ui.phase_times().iter().sum::<std::time::Duration>().as_secs_f64() * 1e6;
    let out = ui.output();
    let bytes = out.draw.iter().flat_map(|l| l.dirty.iter()).map(|(a, b)| (b - a) as usize).sum::<usize>() * INSTANCE;
    for i in 0..out.draw.len() {
        ui.drawn(i);
    }
    Cost { us, nodes: ui.built_nodes(), views: ui.rebuilt_views(), builds: scope::inspect::content_builds(t) - builds, bytes, allocs }
}

/// A gesture's frames: the worst of each counter and the UI CPU's min and median.
struct Run {
    name: String,
    worst: Cost,
    us: Vec<f64>,
}

impl Run {
    fn of(name: &str, costs: &[Cost]) -> Run {
        let mut worst = Cost::default();
        for c in costs {
            worst = Cost { us: 0.0, nodes: worst.nodes.max(c.nodes), views: worst.views.max(c.views), builds: worst.builds.max(c.builds), bytes: worst.bytes.max(c.bytes), allocs: worst.allocs.max(c.allocs) };
        }
        let mut us: Vec<f64> = costs.iter().map(|c| c.us).collect();
        us.sort_by(f64::total_cmp);
        Run { name: name.to_owned(), worst, us }
    }
    fn line(&self) -> String {
        let w = self.worst;
        format!("graph_costs {}: {:.0}/{:.0}/{:.0} us min/median/max, {} nodes, {} views, {} content builds, {} instance bytes, {} allocs (worst frame)", self.name, self.us[0], self.us[self.us.len() / 2], self.us[self.us.len() - 1], w.nodes, w.views, w.builds, w.bytes, w.allocs)
    }
}

fn window_graph_costs() {
    let ws = checked(&big_text(40));
    let sc = Rc::new(flow_graph::projection::of_graph(&scope_catalog(), &ws, "g").unwrap_or_else(|e| panic!("{e}")));
    let path = |name: &str| path(&["g", name]);
    let mut t = scope::create(Some(W), Some(H));
    // one node is a chip
    scope::with_scope(saved_opts(TestSaved { level: vec![(path("n2"), (Level::Chip, true))], ..Default::default() }), "g", sc, &mut t);
    scope::with_records(recorded(&ws), &mut t);
    let mut ui = Ui::create(Platform::Apple, Some(11), rays_shell::host::text(None));
    // measured as the app runs: no verifying rebuilds
    ui.verify_views(false);
    let mut count = 0;
    let mut go = |ui: &mut Ui, t: &mut Pane, at: (f64, f64), buttons: &[MouseButton], events: Vec<Event>| {
        count += 1;
        one(ui, t, &frame(count, at, buttons, events))
    };
    let away = (-100.0, -100.0);
    let rest = |ui: &mut Ui, t: &mut Pane, go: &mut dyn FnMut(&mut Ui, &mut Pane, (f64, f64), &[MouseButton], Vec<Event>) -> Cost| {
        for _ in 0..8 {
            go(ui, t, away, &[], vec![Event::PointerMoved(away)]);
        }
    };
    rest(&mut ui, &mut t, &mut go);
    let n = 30;
    let mut table: Vec<Run> = Vec::new();
    let run = |table: &mut Vec<Run>, name: &str, ui: &mut Ui, t: &mut Pane, frames: usize, f: &mut dyn FnMut(&mut Ui, &mut Pane, usize) -> Cost| {
        let costs: Vec<Cost> = (0..frames).map(|i| f(ui, t, i)).collect();
        table.push(Run::of(name, &costs));
        table.last().unwrap().worst
    };
    // panning: a right drag on the bare canvas
    let canvas = (8.0, H as f64 - 8.0);
    go(&mut ui, &mut t, canvas, &[MouseButton::RightButton], vec![Event::PointerMoved(canvas), Event::PointerPressed(MouseButton::RightButton, canvas)]);
    let pan = run(&mut table, "pan", &mut ui, &mut t, n, &mut |ui, t, i| {
        let p = (canvas.0 + 2.0 * (i + 1) as f64, canvas.1);
        go(ui, t, p, &[MouseButton::RightButton], vec![Event::PointerMoved(p)])
    });
    let end = (canvas.0 + 2.0 * (n + 1) as f64, canvas.1);
    go(&mut ui, &mut t, end, &[], vec![Event::PointerReleased(MouseButton::RightButton, end)]);
    rest(&mut ui, &mut t, &mut go);
    // zooming: wheel steps at the pointer
    let zoom = run(&mut table, "zoom", &mut ui, &mut t, n, &mut |ui, t, i| go(ui, t, end, &[], vec![Event::Scrolled((0.0, if i % 2 == 0 { 1.0 } else { -1.0 }))]));
    // the rest of the gestures at zoom 1
    (t.zoom, t.pan_x, t.pan_y) = (1.0, 12.0, 12.0);
    let (x, y, _, _) = scope::inspect::box_of(&t, &path("n3")).expect("n3");
    (t.pan_x, t.pan_y) = (t.pan_x + 300.0 - x, t.pan_y + 150.0 - y);
    rest(&mut ui, &mut t, &mut go);
    let head = |t: &Pane, name: &str| {
        let (x, y, w, _) = scope::inspect::box_of(t, &path(name)).expect("card");
        assert!(x > 0.0 && x + w < W as f64 && y > 0.0 && y < H as f64 - 24.0, "{name} is not in the pane");
        (x + w / 2.0, y + 6.0)
    };
    let (c1, c2, chip) = (head(&t, "n3"), head(&t, "n4"), head(&t, "n2"));
    let port = scope::inspect::output_socket(&t, &path("n7")).expect("n7's out-port");
    // the first hover after a build patches only what it shows: it never grows the layer
    let first = go(&mut ui, &mut t, c1, &[], vec![Event::PointerMoved(c1)]);
    let first_port = go(&mut ui, &mut t, port, &[], vec![Event::PointerMoved(port)]);
    // warm: each place hovered once
    for p in [c1, c2, port, chip, c1, c2, port, chip] {
        go(&mut ui, &mut t, p, &[], vec![Event::PointerMoved(p)]);
    }
    let card_hover = run(&mut table, "card hover", &mut ui, &mut t, n, &mut |ui, t, i| {
        let p = if i % 2 == 0 { c1 } else { c2 };
        go(ui, t, p, &[], vec![Event::PointerMoved(p)])
    });
    go(&mut ui, &mut t, port, &[], vec![Event::PointerMoved(port)]);
    let lit = t.highlighted.len();
    let port_hover = run(&mut table, "port hover", &mut ui, &mut t, n, &mut |ui, t, i| {
        let p = if i % 2 == 0 { port } else { c1 };
        go(ui, t, p, &[], vec![Event::PointerMoved(p)])
    });
    let chip_hover = run(&mut table, "chip hover", &mut ui, &mut t, n, &mut |ui, t, i| {
        let p = if i % 2 == 0 { chip } else { c1 };
        go(ui, t, p, &[], vec![Event::PointerMoved(p)])
    });
    // selecting: a press selects, the release nothing; neither allocates
    // warm: each card selected once (the selection's and the chains' kept buffers grow to fit), then a
    // rest past a double click's 0.35 s
    for p in [c1, c2] {
        go(&mut ui, &mut t, p, &[MouseButton::LeftButton], vec![Event::PointerMoved(p), Event::PointerPressed(MouseButton::LeftButton, p)]);
        go(&mut ui, &mut t, p, &[], vec![Event::PointerReleased(MouseButton::LeftButton, p)]);
    }
    for _ in 0..3 {
        rest(&mut ui, &mut t, &mut go);
    }
    let mut presses = Vec::new();
    let select = run(&mut table, "select", &mut ui, &mut t, n, &mut |ui, t, i| {
        let p = if (i / 2) % 2 == 0 { c1 } else { c2 };
        if i % 2 == 0 {
            let c = go(ui, t, p, &[MouseButton::LeftButton], vec![Event::PointerMoved(p), Event::PointerPressed(MouseButton::LeftButton, p)]);
            presses.push(c);
            c
        } else {
            let c = go(ui, t, p, &[], vec![Event::PointerReleased(MouseButton::LeftButton, p)]);
            assert!(c.allocs == 0, "a release that selected nothing allocated {}", c.allocs);
            c
        }
    });
    let press_allocs = presses.iter().map(|c| c.allocs).max().unwrap_or(0);
    // dragging a card: its view follows the pointer, its wires are drawn where it goes (a press
    // 0.35 s after the last, not a double click)
    for _ in 0..3 {
        rest(&mut ui, &mut t, &mut go);
    }
    go(&mut ui, &mut t, c1, &[MouseButton::LeftButton], vec![Event::PointerMoved(c1), Event::PointerPressed(MouseButton::LeftButton, c1)]);
    for i in 0..3 {
        let p = (c1.0, c1.1 + 2.0 * (i + 1) as f64);
        go(&mut ui, &mut t, p, &[MouseButton::LeftButton], vec![Event::PointerMoved(p)]);
    }
    let drag = run(&mut table, "card drag", &mut ui, &mut t, n, &mut |ui, t, i| {
        let p = (c1.0, c1.1 + 6.0 + 2.0 * (i + 1) as f64);
        go(ui, t, p, &[MouseButton::LeftButton], vec![Event::PointerMoved(p)])
    });
    assert!(matches!(t.drag, Some(Drag::Moving { moved: true, slow: false, .. })), "the drag did not move the card by its view");
    let end = (c1.0, c1.1 + 6.0 + 2.0 * (n + 1) as f64);
    // the drop: the card stays where it was let go until the geometry has it there (the frame after
    // here, with no host to move it): one content build
    let mut drop = vec![go(&mut ui, &mut t, end, &[], vec![Event::PointerReleased(MouseButton::LeftButton, end)])];
    for _ in 0..3 {
        drop.push(go(&mut ui, &mut t, away, &[], vec![Event::PointerMoved(away)]));
    }
    table.push(Run::of("drop", &drop));
    let drop_builds: u64 = drop.iter().map(|c| c.builds).sum();
    rest(&mut ui, &mut t, &mut go);
    // dragging a wire from an out-port
    let port = scope::inspect::output_socket(&t, &path("n7")).expect("n7's out-port");
    go(&mut ui, &mut t, port, &[], vec![Event::PointerMoved(port)]);
    go(&mut ui, &mut t, port, &[MouseButton::LeftButton], vec![Event::PointerPressed(MouseButton::LeftButton, port)]);
    assert!(matches!(t.drag, Some(Drag::Wiring(_))), "the press on n7's out-port did not start a wire");
    for i in 0..3 {
        let p = (port.0 + 3.0 * (i + 1) as f64, port.1 + 2.0 * (i + 1) as f64);
        go(&mut ui, &mut t, p, &[MouseButton::LeftButton], vec![Event::PointerMoved(p)]);
    }
    let wire = run(&mut table, "wire drag", &mut ui, &mut t, n, &mut |ui, t, i| {
        let p = (port.0 + 9.0 + 3.0 * (i + 1) as f64, port.1 + 6.0 + 2.0 * (i + 1) as f64);
        go(ui, t, p, &[MouseButton::LeftButton], vec![Event::PointerMoved(p)])
    });
    let end = (port.0 + 9.0 + 3.0 * (n + 1) as f64, port.1 + 6.0 + 2.0 * (n + 1) as f64);
    go(&mut ui, &mut t, end, &[], vec![Event::PointerReleased(MouseButton::LeftButton, end)]);
    rest(&mut ui, &mut t, &mut go);
    // a rubber band from the ground above n3 across n3 and n4: the band's view, the selection it
    // makes through the patch path (and the chains' wires); a frame that changes no selection
    // allocates nothing
    let from = (c1.0 - 90.0, c1.1 - 30.0);
    let end = (from.0 + 20.0 * (n + 1) as f64, from.1 + 6.0 * (n + 1) as f64);
    // warm: the band drawn once (the selection's kept buffers grow to what it selects)
    go(&mut ui, &mut t, from, &[], vec![Event::PointerMoved(from)]);
    go(&mut ui, &mut t, from, &[MouseButton::LeftButton], vec![Event::PointerPressed(MouseButton::LeftButton, from)]);
    for i in 0..n {
        let p = (from.0 + 20.0 * (i + 1) as f64, from.1 + 6.0 * (i + 1) as f64);
        go(&mut ui, &mut t, p, &[MouseButton::LeftButton], vec![Event::PointerMoved(p)]);
    }
    go(&mut ui, &mut t, end, &[], vec![Event::PointerReleased(MouseButton::LeftButton, end)]);
    for _ in 0..3 {
        rest(&mut ui, &mut t, &mut go);
    }
    go(&mut ui, &mut t, from, &[], vec![Event::PointerMoved(from)]);
    go(&mut ui, &mut t, from, &[MouseButton::LeftButton], vec![Event::PointerPressed(MouseButton::LeftButton, from)]);
    assert!(matches!(t.drag, Some(Drag::Marquee { .. })), "the press above n3 did not start a rubber band");
    let mut quiet_band = 0;
    let band = run(&mut table, "marquee", &mut ui, &mut t, n, &mut |ui, t, i| {
        let p = (from.0 + 20.0 * (i + 1) as f64, from.1 + 6.0 * (i + 1) as f64);
        let before = scope::selected(t).len();
        let c = go(ui, t, p, &[MouseButton::LeftButton], vec![Event::PointerMoved(p)]);
        if scope::selected(t).len() == before {
            quiet_band = quiet_band.max(c.allocs);
        }
        c
    });
    assert!(scope::selected(&t).contains(&path("n3")) && scope::selected(&t).contains(&path("n4")), "the band did not select n3 and n4");
    go(&mut ui, &mut t, end, &[], vec![Event::PointerReleased(MouseButton::LeftButton, end)]);
    rest(&mut ui, &mut t, &mut go);
    // an edit applied by the host (a value a sketch or the inspector changed): one content build
    let value = flow_syntax::syntax::parse("[7 0 0]").expect("vector").remove(0);
    let key = flow_graph::edit_tree::ArgKey::Kw("translate".to_owned());
    scope::with_arguments(&[(path("n3"), key, value)], &mut t);
    let edit = vec![go(&mut ui, &mut t, away, &[], vec![]), go(&mut ui, &mut t, away, &[], vec![])];
    table.push(Run::of("edit", &edit));
    let edit_builds: u64 = edit.iter().map(|c| c.builds).sum();
    for line in &table {
        eprintln!("{}", line.line());
    }
    eprintln!("graph_costs first card hover after a build: {} instance bytes; first port hover: {} views, {} instance bytes", first.bytes, first_port.views, first_port.bytes);
    eprintln!("graph_costs select press: {press_allocs} allocs");
    let quiet = |name: &str, c: Cost, views: usize, bytes: usize| {
        assert!(c.builds == 0, "{name}: {} content builds", c.builds);
        assert!(c.views <= views, "{name}: {} views built (at most {views})", c.views);
        assert!(c.bytes <= bytes, "{name}: {} instance bytes (at most {bytes})", c.bytes);
        assert!(c.allocs == 0, "{name}: {} allocations", c.allocs);
    };
    quiet("pan", pan, 0, 0);
    quiet("zoom", zoom, 0, 0);
    // a card's ground, its row band and their partner on the card left
    quiet("card hover", card_hover, 0, 8 * INSTANCE);
    // the lit wires' views (each widened in place) under the cards, the wires view around them and
    // the hover's view of their ends
    quiet("port hover", port_hover, 2 + lit, 128 * INSTANCE);
    quiet("chip hover", chip_hover, 0, 8 * INSTANCE);
    assert!(first.views == 0 && first.bytes <= 8 * INSTANCE, "the first card hover after a build: {} views, {} bytes", first.views, first.bytes);
    assert!(first_port.views <= 2 + lit && first_port.bytes <= 128 * INSTANCE, "the first port hover after a build: {} views, {} bytes", first_port.views, first_port.bytes);
    // a selection: the cards' looks, the views of the wires its chain lights or dims and the wires view
    // around them
    let wires = scope::inspect::anchored_views(&t);
    assert!(select.builds == 0, "select: {} content builds", select.builds);
    assert!(select.views <= 1 + wires, "select: {} views built", select.views);
    // the Selected intent carries nothing (the pane holds the selection), the selection, the move it may
    // begin and the chains of the selection are kept buffers, a press is routed with a kept list
    assert!(press_allocs == 0, "a selecting press allocated {press_allocs}");
    // a card's drag: its view's offset and the view of its wires
    quiet("card drag", drag, 1, 64 * INSTANCE);
    // the pulled wire's view and the hover's, the wires view and the views of the wires the pointer
    // passes (they light)
    quiet("wire drag", wire, 3 + lit, 64 * INSTANCE);
    assert!(band.builds == 0 && band.views <= 2 + wires, "the rubber band: {} content builds, {} views", band.builds, band.views);
    assert!(quiet_band == 0, "a band frame that selected nothing new allocated {quiet_band}");
    assert!(band.allocs == 0, "a band frame allocated {}", band.allocs);
    assert!(drop_builds == 1, "the drop built the content {drop_builds} times");
    assert!(edit_builds == 1, "the edit built the content {edit_builds} times");
    // (the drop's: the geometry again at the cards' new place, the wires routed; the edit's: the
    // cards built again, their fields' values)
    let (drop_allocs, edit_allocs) = (drop.iter().map(|c| c.allocs).sum::<u64>(), edit.iter().map(|c| c.allocs).sum::<u64>());
    assert!(drop_allocs <= 2000 && edit_allocs <= 700, "the drop allocated {drop_allocs}, the edit {edit_allocs}");
    ui.destroy();
}

pub const CASES: &[(&str, fn())] = &[("window_graph_costs", window_graph_costs)];
