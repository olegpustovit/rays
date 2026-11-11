//! Layout (P5): weights, min and max, aspect, gaps, edges, alignment, baselines, wrap, grid, stack,
//! anchored popups, both-axis scroll with sticky headers, right-to-left and the splitter.
use crate::support::*;
use input::Event;
use pxui::geom::color;
use pxui::ui::*;
use std::cell::RefCell;

fn at(t: f64, events: Vec<Event>) -> input::frame::Frame {
    frame_at(vec![], 1.0, t, events)
}

fn px(w: f32, h: f32) -> Node {
    Node { w: Size::Px(w), h: Size::Px(h), ..Node::DEFAULT }
}

/// One frame of `build` in a 320 x 240 window: the rectangles of the handles it returns.
fn lay(ui: &mut Ui, t: f64, events: Vec<Event>, build: &mut dyn FnMut(&mut Ui) -> Vec<Handle>) -> Vec<Rect> {
    let handles = RefCell::new(Vec::new());
    ui.frame(&at(t, events), &mut |ui| *handles.borrow_mut() = build(ui));
    handles.borrow().iter().map(|h| ui.rect(h)).collect()
}

/// `parent` holding `kids` (built in order).
fn holder(ui: &mut Ui, parent: Node, kids: &[Node]) -> Vec<Handle> {
    let p = ui.node("p", parent);
    let saved = ui.enter(&p);
    let mut out = vec![p];
    for (i, k) in kids.iter().enumerate() {
        out.push(ui.node(&format!("k{i}"), *k));
    }
    ui.leave(saved);
    out
}

fn once(parent: Node, kids: &[Node]) -> Vec<Rect> {
    let mut ui = new_ui(None);
    let r = lay(&mut ui, 0.0, vec![], &mut |ui| holder(ui, parent, kids));
    ui.destroy();
    r
}

fn eq(r: Rect, x: f32, y: f32, w: f32, h: f32, what: &str) {
    let close = |a: f32, b: f32| (a - b).abs() < 0.01;
    check(close(r.x, x) && close(r.y, y) && close(r.w, w) && close(r.h, h), &format!("{what}: {r:?}, expected ({x}, {y}, {w}, {h})"));
}

const ROW: Node = Node { w: Size::Px(300.0), h: Size::Px(40.0), axis: Axis::Row, ..Node::DEFAULT };

pub fn layout_grow_shrink() {
    let grow = |g: f32| Node { w: Size::Grow, h: Size::Px(10.0), grow: g, ..Node::DEFAULT };
    let r = once(ROW, &[grow(1.0), grow(2.0), px(60.0, 10.0)]);
    eq(r[1], 0.0, 0.0, 80.0, 10.0, "grow 1");
    eq(r[2], 80.0, 0.0, 160.0, 10.0, "grow 2");
    eq(r[3], 240.0, 0.0, 60.0, 10.0, "fixed after the growers");
    let shrink = |w: f32, s: f32| Node { shrink: s, ..px(w, 10.0) };
    let r = once(Node { w: Size::Px(100.0), ..ROW }, &[shrink(80.0, 1.0), shrink(40.0, 3.0)]);
    eq(r[1], 0.0, 0.0, 72.0, 10.0, "shrink 1 of 80");
    eq(r[2], 72.0, 0.0, 28.0, 10.0, "shrink 3 of 40");
}

pub fn layout_min_max_aspect() {
    let grow = Node { w: Size::Grow, h: Size::Px(10.0), ..Node::DEFAULT };
    let r = once(ROW, &[Node { max_w: 50.0, ..grow }, grow, Node { min_w: 30.0, ..px(10.0, 10.0) }]);
    eq(r[1], 0.0, 0.0, 50.0, 10.0, "max_w holds a grower");
    eq(r[2], 50.0, 0.0, 220.0, 10.0, "the other grower takes the rest");
    eq(r[3], 270.0, 0.0, 30.0, 10.0, "min_w beats w");
    let r = once(Node { w: Size::Px(300.0), h: Size::Fit, ..Node::DEFAULT }, &[Node { w: Size::Px(160.0), h: Size::Fit, aspect: 2.0, ..Node::DEFAULT }, Node { w: Size::Grow, h: Size::Fit, aspect: 3.0, max_h: 50.0, ..Node::DEFAULT }]);
    eq(r[1], 0.0, 0.0, 160.0, 80.0, "aspect from a fixed width");
    eq(r[2], 0.0, 80.0, 300.0, 50.0, "aspect from the stretched width, held by max_h");
}

pub fn layout_gap_padding_margin() {
    let column = Node { w: Size::Px(200.0), h: Size::Fit, padding: Edges { l: 5.0, t: 7.0, r: 9.0, b: 11.0 }, gap: 4.0, ..Node::DEFAULT };
    let r = once(column, &[px(20.0, 10.0), Node { margin: Edges { l: 3.0, t: 2.0, r: 0.0, b: 6.0 }, ..px(20.0, 10.0) }, Node { w: Size::Grow, h: Size::Px(10.0), margin: Edges { l: 1.0, r: 2.0, ..Edges::ZERO }, ..Node::DEFAULT }]);
    eq(r[1], 5.0, 7.0, 20.0, 10.0, "padding start");
    eq(r[2], 8.0, 23.0, 20.0, 10.0, "gap and margin");
    eq(r[3], 6.0, 43.0, 183.0, 10.0, "stretched inside padding and margins");
    eq(r[0], 0.0, 0.0, 200.0, 7.0 + 10.0 + 4.0 + 18.0 + 4.0 + 10.0 + 11.0, "fit height counts gaps, margins and padding");
}

pub fn layout_justify_align() {
    let kids = [px(20.0, 10.0), px(40.0, 20.0)];
    let x = |j: Justify| {
        let r = once(Node { justify: j, ..ROW }, &kids);
        (r[1].x, r[2].x)
    };
    check(x(Justify::Start) == (0.0, 20.0), "start");
    check(x(Justify::Center) == (120.0, 140.0), "center");
    check(x(Justify::End) == (240.0, 260.0), "end");
    check(x(Justify::SpaceBetween) == (0.0, 260.0), "space-between");
    check(x(Justify::SpaceAround) == (60.0, 200.0), "space-around");
    check(x(Justify::SpaceEvenly) == (80.0, 180.0), "space-evenly");
    let y = |a: Align, own: Option<Align>| {
        let r = once(Node { align: a, ..ROW }, &[Node { h: Size::Fit, align_self: own, ..px(20.0, 0.0) }, px(40.0, 20.0)]);
        (r[1].y, r[1].h, r[2].y)
    };
    check(y(Align::Start, None) == (0.0, 0.0, 0.0), "align start");
    check(y(Align::Center, None) == (20.0, 0.0, 10.0), "align center");
    check(y(Align::End, None) == (40.0, 0.0, 20.0), "align end");
    check(y(Align::Stretch, None) == (0.0, 40.0, 0.0), "align stretch fills a Fit height");
    check(y(Align::Start, Some(Align::End)) == (40.0, 0.0, 0.0), "align-self overrides");
}

/// Mixed font sizes in a row aligned on their baselines: one baseline.
pub fn layout_baseline() {
    let mut ui = new_ui(Some(13));
    let text = |size: i64| Node { w: Size::Px(60.0), h: Size::Px(size as f32 + 4.0), look: Look { font: Font { size, ..Font::BODY }, text: color::black(), ..Look::NONE }, ..Node::DEFAULT };
    let r = lay(&mut ui, 0.0, vec![], &mut |ui| {
        let p = ui.node("p", Node { align: Align::Baseline, ..ROW });
        let saved = ui.enter(&p);
        let small = ui.node("s", text(10));
        ui.text(&small, "small");
        let big = ui.node("b", text(30));
        ui.text(&big, "Big");
        ui.leave(saved);
        vec![small, big]
    });
    let base = |ui: &mut Ui, size: i64, h: f32, t: &str| {
        let m = ui.text_metrics(&kit_style(size, 0.0), 0.0, t);
        text_top_for(13, Some(size), 0.0, h) + m.baseline
    };
    let (bs, bb) = (base(&mut ui, 10, 14.0, "small"), base(&mut ui, 30, 34.0, "Big"));
    check(r[1].y == 0.0 && r[0].y > 0.0, "the deeper baseline sets the line");
    check((r[0].y + bs - (r[1].y + bb)).abs() < 0.01, &format!("baselines differ: {} vs {}", r[0].y + bs, r[1].y + bb));
    ui.destroy();
}

pub fn layout_wrap() {
    let kid = px(40.0, 10.0);
    let r = once(Node { w: Size::Px(100.0), h: Size::Fit, axis: Axis::Row, wrap: true, gap: 10.0, row_gap: 5.0, ..Node::DEFAULT }, &[kid, kid, Node { h: Size::Px(12.0), ..kid }, kid]);
    eq(r[1], 0.0, 0.0, 40.0, 10.0, "first");
    eq(r[2], 50.0, 0.0, 40.0, 10.0, "second on the first line");
    eq(r[3], 0.0, 15.0, 40.0, 12.0, "third wraps below the row gap");
    eq(r[4], 50.0, 15.0, 40.0, 10.0, "fourth beside it");
    eq(r[0], 0.0, 0.0, 100.0, 27.0, "fit height of two lines");
}

pub fn layout_grid() {
    const COLS: &[Track] = &[Track::Px(50.0), Track::Fr(1.0), Track::Fit];
    let grid = Node { w: Size::Px(300.0), h: Size::Fit, grid: Some(Grid { cols: COLS, rows: &[] }), gap: 10.0, row_gap: 4.0, ..Node::DEFAULT };
    let cell = Node { w: Size::Grow, h: Size::Px(20.0), ..Node::DEFAULT };
    let r = once(grid, &[cell, cell, Node { w: Size::Px(70.0), ..cell }, Node { cell: Cell::DEFAULT.span(2, 1), h: Size::Px(30.0), ..cell }, Node { cell: Cell::at(2, 2), ..cell }]);
    eq(r[1], 0.0, 0.0, 50.0, 20.0, "fixed track");
    eq(r[2], 60.0, 0.0, 160.0, 20.0, "fractional track takes what is left");
    eq(r[3], 230.0, 0.0, 70.0, 20.0, "content track");
    eq(r[4], 0.0, 24.0, 220.0, 30.0, "a span covers two tracks and the gap");
    eq(r[5], 230.0, 58.0, 70.0, 20.0, "an explicit cell");
    eq(r[0], 0.0, 0.0, 300.0, 78.0, "fit height of three rows");
}

/// A stack overlaps its children and paints them in z order.
pub fn layout_stack_order() {
    let mut ui = new_ui(None);
    let (red, blue) = (color::rgb(200, 0, 0), color::rgb(0, 0, 200));
    let r = lay(&mut ui, 0.0, vec![], &mut |ui| {
        let p = ui.node("p", Node { w: Size::Px(100.0), h: Size::Px(60.0), axis: Axis::Stack, justify: Justify::Center, align: Align::End, ..Node::DEFAULT });
        let saved = ui.enter(&p);
        let top = ui.node("top", Node { z: 2, look: Look { fill: red, ..Look::NONE }, ..px(40.0, 20.0) });
        let under = ui.node("under", Node { w: Size::Grow, h: Size::Grow, look: Look { fill: blue, ..Look::NONE }, ..Node::DEFAULT });
        ui.leave(saved);
        vec![top, under]
    });
    eq(r[0], 30.0, 40.0, 40.0, 20.0, "centred across, at the end down");
    eq(r[1], 0.0, 0.0, 100.0, 60.0, "a Grow child fills the stack");
    let colors: Vec<[u8; 4]> = ui.output().draw[0].instances.iter().map(|i| i.color).collect();
    let first = |c: color::T| colors.iter().position(|k| *k == [c.r as u8, c.g as u8, c.b as u8, 255]).unwrap();
    check(first(blue) < first(red), "the higher z painted first");
    ui.destroy();
}

/// A popup by an anchor flips at each window edge and stays inside it.
pub fn layout_anchored_flip() {
    let bounds = Rect::new(0.0, 0.0, 320.0, 240.0);
    let p = |side: Side| Placement { side, align: Align::Start, gap: 2.0, margin: 4.0, flip: true };
    let size = (100.0, 50.0);
    eq(place_anchored(Rect::new(10.0, 10.0, 30.0, 20.0), size, p(Side::Below), bounds), 10.0, 32.0, 100.0, 50.0, "below");
    eq(place_anchored(Rect::new(10.0, 200.0, 30.0, 20.0), size, p(Side::Below), bounds), 10.0, 148.0, 100.0, 50.0, "bottom edge flips above");
    eq(place_anchored(Rect::new(10.0, 10.0, 30.0, 20.0), size, p(Side::Above), bounds), 10.0, 32.0, 100.0, 50.0, "top edge flips below");
    eq(place_anchored(Rect::new(250.0, 100.0, 30.0, 20.0), size, p(Side::Right), bounds), 148.0, 100.0, 100.0, 50.0, "right edge flips left");
    eq(place_anchored(Rect::new(20.0, 100.0, 30.0, 20.0), size, p(Side::Left), bounds), 52.0, 100.0, 100.0, 50.0, "left edge flips right");
    eq(place_anchored(Rect::new(280.0, 10.0, 30.0, 20.0), size, p(Side::Below), bounds), 216.0, 32.0, 100.0, 50.0, "kept inside across");
    // the engine primitive places a Fit popup in layout, by its laid-out size
    let mut ui = new_ui(None);
    let r = lay(&mut ui, 0.0, vec![], &mut |ui| {
        let pop = ui.anchored("pop", Rect::new(10.0, 200.0, 30.0, 20.0), p(Side::Below), Size::Fit, Size::Fit, &mut |ui| {
            ui.node("body", px(100.0, 50.0));
        });
        vec![pop]
    });
    eq(r[0], 10.0, 148.0, 100.0, 50.0, "ui.anchored flips a Fit popup");
    ui.destroy();
}

/// A box scrolling both ways keeps its sticky header at the top.
pub fn layout_scroll_sticky() {
    let mut ui = new_ui(None);
    let mut build = |ui: &mut Ui| {
        let p = ui.node("p", Node { flags: SCROLL | SCROLL_X | CLIP, scroll_step: 10.0, at: Some((0.0, 0.0)), ..px(100.0, 100.0) });
        let saved = ui.enter(&p);
        let head = ui.node("head", Node { flags: STICKY, w: Size::Px(300.0), h: Size::Px(20.0), ..Node::DEFAULT });
        let body = ui.node("body", px(300.0, 300.0));
        ui.leave(saved);
        vec![p, head, body]
    };
    lay(&mut ui, 0.0, vec![Event::PointerMoved((50.0, 50.0))], &mut build);
    let r = lay(&mut ui, 0.1, vec![Event::Scrolled((-3.0, -4.0))], &mut build);
    eq(r[2], -30.0, 20.0 - 40.0, 300.0, 300.0, "content scrolled on both axes");
    eq(r[1], -30.0, 0.0, 300.0, 20.0, "the sticky header stays at the top, scrolled sideways");
    ui.destroy();
}

/// Right to left mirrors a row's flow and its start padding.
pub fn layout_rtl() {
    let mut ui = new_ui(None);
    ui.set_dir(Direction::Rtl);
    let r = lay(&mut ui, 0.0, vec![], &mut |ui| holder(ui, Node { padding: Edges { l: 10.0, ..Edges::ZERO }, gap: 5.0, ..ROW }, &[px(20.0, 10.0), px(40.0, 10.0), Node { at: Some((0.0, 0.0)), ..px(5.0, 5.0) }]));
    // the root mirrors too: the row sits at the window's right
    eq(r[0], 20.0, 0.0, 300.0, 40.0, "the row at the window's start (right)");
    eq(r[1], 290.0, 0.0, 20.0, 10.0, "first child at the right, inside the start padding");
    eq(r[2], 245.0, 0.0, 40.0, 10.0, "second to its left");
    eq(r[3], 20.0, 0.0, 5.0, 5.0, "an at offset stays physical");
    ui.destroy();
}

/// Dragging the splitter's grip returns the ratio the pointer makes.
pub fn layout_split_drag() {
    let mut ui = new_ui(None);
    let ratio = std::cell::Cell::new(0.25);
    let panes = RefCell::new((Rect::default(), Rect::default()));
    let mut build = |ui: &mut Ui| {
        let host = ui.node("host", Node { at: Some((0.0, 0.0)), ..px(201.0, 100.0) });
        let saved = ui.enter(&host);
        let (mut a, mut b) = (None, None);
        let got = ui.split("split", Axis::Row, ratio.get(), 20.0, 30.0, &mut |ui| a = Some(ui.node("a", Node { h: Size::Grow, ..Node::DEFAULT })), &mut |ui| b = Some(ui.node("b", Node { h: Size::Grow, ..Node::DEFAULT })));
        if let Some(r) = got {
            ratio.set(r);
        }
        ui.leave(saved);
        *panes.borrow_mut() = (ui.rect(&a.unwrap()), ui.rect(&b.unwrap()));
    };
    for (t, events) in [(0.0, vec![]), (0.1, vec![Event::PointerMoved((50.0, 50.0))]), (0.2, vec![press((50, 50))]), (0.3, vec![Event::PointerMoved((120.0, 50.0))]), (0.4, vec![release((120, 50))]), (0.5, vec![])] {
        ui.frame(&at(t, events), &mut build);
    }
    check((ratio.get() - 0.6).abs() < 0.001, &format!("ratio {}", ratio.get()));
    let (a, b) = *panes.borrow();
    eq(a, 0.0, 0.0, 120.0, 100.0, "pane a takes the ratio");
    eq(b, 121.0, 0.0, 80.0, 100.0, "pane b the rest after the grip");
    // past b's minimum the ratio holds it
    for (t, events) in [(0.6, vec![press((120, 50))]), (0.7, vec![Event::PointerMoved((199.0, 50.0))]), (0.8, vec![release((199, 50))]), (0.9, vec![])] {
        ui.frame(&at(t, events), &mut build);
    }
    check((ratio.get() - 170.0 / 200.0).abs() < 0.001, &format!("min_b: ratio {}", ratio.get()));
    ui.destroy();
}

/// A kept view's nodes are not laid out again; a rebuilt one's are.
pub fn layout_cached_view_not_rerun() {
    let mut ui = new_ui(None);
    let rev = std::cell::Cell::new(1);
    let mut build = |ui: &mut Ui| {
        ui.view("v", rev.get(), Size::Px(200.0), Size::Px(100.0), &mut |ui| {
            for i in 0..20 {
                ui.node(&format!("n{i}"), px(10.0, 4.0));
            }
        });
    };
    for t in 0..3 {
        ui.frame(&at(t as f64 * 0.1, vec![]), &mut build);
    }
    let kept = ui.arranged_nodes();
    rev.set(2);
    ui.frame(&at(0.4, vec![]), &mut build);
    let rebuilt = ui.arranged_nodes();
    check(kept + 20 == rebuilt, &format!("kept view laid out {kept} nodes, rebuilt {rebuilt}"));
    ui.destroy();
}

/// Flex, wrap, grid, stack, baseline and sticky layout allocate nothing once warm.
pub fn layout_warm_zero_alloc() {
    const COLS: &[Track] = &[Track::Px(40.0), Track::Fr(1.0), Track::Fit];
    let mut ui = new_ui(None);
    let mut build = |ui: &mut Ui| {
        let kinds = [
            Node { axis: Axis::Row, wrap: true, gap: 4.0, row_gap: 2.0, justify: Justify::SpaceBetween, align: Align::Baseline, ..px(200.0, 60.0) },
            Node { grid: Some(Grid { cols: COLS, rows: &[] }), gap: 3.0, ..px(200.0, 60.0) },
            Node { axis: Axis::Stack, justify: Justify::Center, ..px(200.0, 60.0) },
            Node { flags: SCROLL | SCROLL_X | CLIP, ..px(200.0, 60.0) },
        ];
        for (k, n) in kinds.iter().enumerate() {
            let p = ui.node_keyed(int_key(7, k as i64), *n);
            let saved = ui.enter(&p);
            for i in 0..12 {
                let c = ui.node_keyed(int_key(p.box_key, i), Node { flags: if i == 0 { STICKY } else { 0 }, z: (i % 3) as i16, grow: 1.0 + i as f32, shrink: 1.0, margin: Edges::all(1.0), ..px(30.0, 10.0 + i as f32) });
                ui.text(&c, "x");
            }
            ui.leave(saved);
        }
    };
    for t in 0..5 {
        ui.frame(&at(t as f64 * 0.1, vec![]), &mut build);
    }
    mem_sys::local_reset();
    let before = mem_sys::local_alloc_count();
    for t in 5..25 {
        ui.frame(&at(t as f64 * 0.1, vec![]), &mut build);
    }
    let allocs = mem_sys::local_alloc_count() - before;
    check(allocs == 0, &format!("{allocs} allocations in 20 warm layout frames"));
    ui.destroy();
}

pub const CASES: &[(&str, fn())] = &[
    ("layout_warm_zero_alloc", layout_warm_zero_alloc),
    ("layout_grow_shrink", layout_grow_shrink),
    ("layout_min_max_aspect", layout_min_max_aspect),
    ("layout_gap_padding_margin", layout_gap_padding_margin),
    ("layout_justify_align", layout_justify_align),
    ("layout_baseline", layout_baseline),
    ("layout_wrap", layout_wrap),
    ("layout_grid", layout_grid),
    ("layout_stack_order", layout_stack_order),
    ("layout_anchored_flip", layout_anchored_flip),
    ("layout_scroll_sticky", layout_scroll_sticky),
    ("layout_rtl", layout_rtl),
    ("layout_split_drag", layout_split_drag),
    ("layout_cached_view_not_rerun", layout_cached_view_not_rerun),
];
