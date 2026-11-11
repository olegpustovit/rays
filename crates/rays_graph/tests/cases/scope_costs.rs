//! What each gesture on the graph pane costs a frame, the pane cached as the editor caches it: PXUI
//! CPU (`Ui::frame` phases), nodes declared, views built, instance bytes written and allocations, over
//! a 40-node graph. Prints a table (the assertions are `scope_pixels`'s).
use crate::support::*;
use input::{Event, MouseButton, Platform};
use pxui::ui::*;
use rays::frame as rframe;
use rays_graph::scope_pane as scope;
use rays_graph::scope_state::{Pane, ScopeOpts};
use std::rc::Rc;

const W: i64 = 1000;
const H: i64 = 700;

struct Cost {
    us: f64,
    nodes: usize,
    views: usize,
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

fn one(ui: &mut Ui, t: &mut Pane, fr: &rframe::Frame) -> Cost {
    ui.set_timing(true);
    mem_sys::local_reset();
    let before = mem_sys::local_alloc_count();
    ui.frame(fr, &mut |ui: &mut Ui| {
        let root = ui.box_("graph-root", BoxOpts::default().w(Size::Px(W as f32)).h(Size::Px(H as f32)).at((0.0, 0.0)));
        ui.within(&root, &mut |ui| scope::update_cached("graph-view", &[], t, ui, fr, &mut Vec::new()));
    });
    let allocs = mem_sys::local_alloc_count() - before;
    let us = ui.phase_times().iter().sum::<std::time::Duration>().as_secs_f64() * 1e6;
    let out = ui.output();
    let bytes = out.draw.iter().flat_map(|l| l.dirty.iter()).map(|(a, b)| (b - a) as usize).sum::<usize>() * std::mem::size_of::<pxui::draw::Instance>();
    for i in 0..out.draw.len() {
        ui.drawn(i);
    }
    Cost { us, nodes: ui.built_nodes(), views: ui.rebuilt_views(), bytes, allocs }
}

fn window_graph_costs() {
    let ws = checked(&big_text(40));
    let sc = Rc::new(flow_graph::projection::of_graph(&scope_catalog(), &ws, "g").unwrap_or_else(|e| panic!("{e}")));
    let mut t = scope::create(Some(W), Some(H));
    scope::with_scope(ScopeOpts::default(), "g", sc, &mut t);
    scope::with_records(recorded(&ws), &mut t);
    let path = |name: &str| path(&["g", name]);
    let mut ui = Ui::create(Platform::Apple, Some(11), rays_shell::host::text(None));
    // measured as the app runs: no verifying rebuilds
    ui.verify_views(false);
    let mut count = 0;
    let mut go = |ui: &mut Ui, t: &mut Pane, at: (f64, f64), buttons: &[MouseButton], events: Vec<Event>| {
        count += 1;
        one(ui, t, &frame(count, at, buttons, events))
    };
    let away = (-100.0, -100.0);
    for _ in 0..6 {
        go(&mut ui, &mut t, away, &[], vec![Event::PointerMoved(away)]);
    }
    let head = |t: &Pane, name: &str| {
        let (x, y, w, _) = scope::inspect::box_of(t, &path(name)).expect("card");
        (x + w / 2.0, y + 6.0)
    };
    let canvas = (8.0, H as f64 - 8.0);
    let n = 30;
    let mut table = Vec::new();
    let mut run = |name: &str, ui: &mut Ui, t: &mut Pane, go: &mut dyn FnMut(&mut Ui, &mut Pane, usize) -> Cost| {
        let mut sum = Cost { us: 0.0, nodes: 0, views: 0, bytes: 0, allocs: 0 };
        for i in 0..n {
            let c = go(ui, t, i);
            (sum.us, sum.nodes, sum.views, sum.bytes, sum.allocs) = (sum.us + c.us, sum.nodes + c.nodes, sum.views + c.views, sum.bytes + c.bytes, sum.allocs + c.allocs);
        }
        table.push(format!("graph_costs {name}: {:.0} us, {} nodes, {} views, {} instance bytes, {} allocs a frame", sum.us / n as f64, sum.nodes / n, sum.views as f64 / n as f64, sum.bytes / n, sum.allocs / n as u64));
    };
    // panning: a right drag on the bare canvas
    go(&mut ui, &mut t, canvas, &[MouseButton::RightButton], vec![Event::PointerMoved(canvas), Event::PointerPressed(MouseButton::RightButton, canvas)]);
    run("pan", &mut ui, &mut t, &mut |ui, t, i| {
        let p = (canvas.0 + 2.0 * (i + 1) as f64, canvas.1);
        go(ui, t, p, &[MouseButton::RightButton], vec![Event::PointerMoved(p)])
    });
    let end = (canvas.0 + 2.0 * (n + 1) as f64, canvas.1);
    go(&mut ui, &mut t, end, &[], vec![Event::PointerReleased(MouseButton::RightButton, end)]);
    for _ in 0..6 {
        go(&mut ui, &mut t, end, &[], vec![]);
    }
    // zooming: wheel steps at the pointer, then the frames until it rests
    run("zoom", &mut ui, &mut t, &mut |ui, t, i| go(ui, t, end, &[], vec![Event::Scrolled((0.0, if i % 2 == 0 { 1.0 } else { -1.0 }))]));
    for _ in 0..12 {
        go(&mut ui, &mut t, away, &[], vec![Event::PointerMoved(away)]);
    }
    // where the pan and zoom left the cards
    let (c1, c2, port) = (head(&t, "n3"), head(&t, "n4"), scope::inspect::output_socket(&t, &path("n3")).expect("n3's out-port"));
    // warm: each place hovered once (a card's first patch after a build may grow its range)
    for p in [c1, c2, port, c1, c2, port] {
        go(&mut ui, &mut t, p, &[], vec![Event::PointerMoved(p)]);
    }
    run("card hover", &mut ui, &mut t, &mut |ui, t, i| {
        let p = if i % 2 == 0 { c1 } else { c2 };
        go(ui, t, p, &[], vec![Event::PointerMoved(p)])
    });
    run("port hover", &mut ui, &mut t, &mut |ui, t, i| {
        let p = if i % 2 == 0 { port } else { c1 };
        go(ui, t, p, &[], vec![Event::PointerMoved(p)])
    });
    run("select", &mut ui, &mut t, &mut |ui, t, i| {
        let p = if (i / 2) % 2 == 0 { c1 } else { c2 };
        if i % 2 == 0 {
            go(ui, t, p, &[MouseButton::LeftButton], vec![Event::PointerMoved(p), Event::PointerPressed(MouseButton::LeftButton, p)])
        } else {
            go(ui, t, p, &[], vec![Event::PointerReleased(MouseButton::LeftButton, p)])
        }
    });
    go(&mut ui, &mut t, c1, &[MouseButton::LeftButton], vec![Event::PointerMoved(c1), Event::PointerPressed(MouseButton::LeftButton, c1)]);
    run("card drag", &mut ui, &mut t, &mut |ui, t, i| {
        let p = (c1.0 + 2.0 * (i + 1) as f64, c1.1);
        go(ui, t, p, &[MouseButton::LeftButton], vec![Event::PointerMoved(p)])
    });
    let end = (c1.0 + 2.0 * (n + 1) as f64, c1.1);
    go(&mut ui, &mut t, end, &[], vec![Event::PointerReleased(MouseButton::LeftButton, end)]);
    for _ in 0..4 {
        go(&mut ui, &mut t, away, &[], vec![Event::PointerMoved(away)]);
    }
    let port = scope::inspect::output_socket(&t, &path("n3")).expect("n3's out-port");
    go(&mut ui, &mut t, port, &[MouseButton::LeftButton], vec![Event::PointerMoved(port), Event::PointerPressed(MouseButton::LeftButton, port)]);
    run("wire drag", &mut ui, &mut t, &mut |ui, t, i| {
        let p = (port.0 + 3.0 * (i + 1) as f64, port.1 + 2.0 * (i + 1) as f64);
        go(ui, t, p, &[MouseButton::LeftButton], vec![Event::PointerMoved(p)])
    });
    for line in &table {
        eprintln!("{line}");
    }
    ui.destroy();
}

pub const CASES: &[(&str, fn())] = &[("window_graph_costs", window_graph_costs)];
