//! Hybrid views (layer 2): cached builds, retained runs, damage, view offsets and virtual lists.
use crate::support::*;
use input::Event;
use pxui::geom::color;
use pxui::ui::*;
use std::cell::Cell;

fn at(t: f64, events: Vec<Event>) -> input::frame::Frame {
    frame_at(vec![], 2.0, t, events)
}

fn chip(ui: &mut Ui, key: &str, fill: color::T, flags: Flags) -> Handle {
    ui.node(key, Node { flags, w: Size::Px(40.0), h: Size::Px(20.0), look: Look { fill, ..Look::NONE }, ..Node::DEFAULT })
}

const RED: color::T = color::T { r: 200, g: 0, b: 0, a: 255 };
const BLUE: color::T = color::T { r: 0, g: 0, b: 200, a: 255 };

/// The P5 exit criterion: bumping one view's revision runs only that view's build.
pub fn window_pxui_view_rebuilds_one() {
    let mut ui = new_ui(None);
    let (a_runs, b_runs, rev_a) = (Cell::new(0), Cell::new(0), Cell::new(1u64));
    let mut build = |ui: &mut Ui| {
        ui.view("a", rev_a.get(), Size::Px(100.0), Size::Px(50.0), &mut |ui| {
            a_runs.set(a_runs.get() + 1);
            chip(ui, "x", RED, NO_FLAGS);
        });
        ui.view("b", 1, Size::Px(100.0), Size::Px(50.0), &mut |ui| {
            b_runs.set(b_runs.get() + 1);
            chip(ui, "y", BLUE, NO_FLAGS);
        });
    };
    ui.frame(&at(0.0, vec![]), &mut build);
    let (instances, revision) = (ui.output().draw[0].instances.clone(), ui.draw_revision());
    ui.frame(&at(0.1, vec![]), &mut build);
    check(a_runs.get() == 1 && b_runs.get() == 1, "a clean view rebuilt");
    check(ui.built_nodes() == 3, &format!("a frame of two kept views declared {} nodes", ui.built_nodes()));
    check(ui.output().draw[0].instances == instances && ui.draw_revision() == revision, "kept views changed the output");
    check(ui.output().damage.is_none(), "an idle frame has damage");
    rev_a.set(2);
    ui.frame(&at(0.2, vec![]), &mut build);
    check(a_runs.get() == 2 && b_runs.get() == 1, &format!("a revision bump ran builds a {} b {}", a_runs.get(), b_runs.get()));
    // the kept view's node lives on undeclared
    let y = ui.box_of_key(key_of(ui.box_of_key(key_of(0x2c1b3c6d, "b")).map_or(0, |b| b.box_key), "y"));
    check(y.is_some_and(|y| ui.live(&y)), "a kept view's node was pruned");
    // the view stops being declared: its nodes go with it
    ui.frame(&at(0.3, vec![]), &mut |ui| {
        ui.view("a", 2, Size::Px(100.0), Size::Px(50.0), &mut |ui| {
            chip(ui, "x", RED, NO_FLAGS);
        });
    });
    check(!y.is_some_and(|y| ui.live(&y)), "a dropped view's node lived on");
    ui.destroy();
}

/// A dirty parent rebuilds; the clean view inside it is still kept.
pub fn window_pxui_view_nested() {
    let mut ui = new_ui(None);
    let (outer, inner, rev) = (Cell::new(0), Cell::new(0), Cell::new(1u64));
    let mut build = |ui: &mut Ui| {
        ui.view("outer", rev.get(), Size::Px(200.0), Size::Px(100.0), &mut |ui| {
            outer.set(outer.get() + 1);
            chip(ui, "o", RED, NO_FLAGS);
            ui.view("inner", 1, Size::Px(100.0), Size::Px(50.0), &mut |ui| {
                inner.set(inner.get() + 1);
                chip(ui, "i", BLUE, NO_FLAGS);
            });
        });
    };
    ui.frame(&at(0.0, vec![]), &mut build);
    let instances = ui.output().draw[0].instances.clone();
    rev.set(2);
    ui.frame(&at(0.1, vec![]), &mut build);
    check(outer.get() == 2 && inner.get() == 1, "the clean inner view rebuilt with its parent");
    check(ui.output().draw[0].instances == instances, "a parent rebuild changed the kept child's output");
    ui.destroy();
}

/// Hover and click reach nodes of a kept view and rebuild only that view.
pub fn window_pxui_view_input() {
    let mut ui = new_ui(None);
    let (a_runs, b_runs) = (Cell::new(0), Cell::new(0));
    let (hovered, clicked) = (Cell::new(false), Cell::new(false));
    let mut build = |ui: &mut Ui| {
        ui.view("a", 1, Size::Px(100.0), Size::Px(50.0), &mut |ui| {
            a_runs.set(a_runs.get() + 1);
            let h = chip(ui, "x", RED, CLICKABLE);
            let s = ui.signal(&h);
            hovered.set(s.hovered);
            clicked.set(clicked.get() || s.clicked);
        });
        ui.view("b", 1, Size::Px(100.0), Size::Px(50.0), &mut |ui| {
            b_runs.set(b_runs.get() + 1);
            chip(ui, "y", BLUE, CLICKABLE);
        });
    };
    ui.frame(&at(0.0, vec![move_to((300, 230))]), &mut build);
    ui.frame(&at(0.1, vec![]), &mut build);
    check((a_runs.get(), b_runs.get()) == (1, 1), "warm frames rebuilt");
    ui.frame(&at(0.2, vec![move_to((10, 10))]), &mut build);
    check(a_runs.get() == 2 && b_runs.get() == 1 && hovered.get(), "hover entering a kept view's node");
    ui.frame(&at(0.3, vec![move_to((12, 11))]), &mut build);
    check(a_runs.get() == 2, "motion inside one node rebuilt its view");
    ui.frame(&at(0.4, vec![press((12, 11)), release((12, 11))]), &mut build);
    check(a_runs.get() == 3 && b_runs.get() == 1 && clicked.get(), "a click in a kept view");
    ui.frame(&at(0.5, vec![move_to((300, 230))]), &mut build);
    check(a_runs.get() == 4 && b_runs.get() == 1 && !hovered.get(), "hover leaving a kept view's node");
    ui.destroy();
}

/// Offset and opacity change one table entry: no instance rewritten, damage reported, hit testing
/// and `rect` follow.
pub fn window_pxui_view_offset() {
    let mut ui = new_ui(None);
    let (runs, view, node) = (Cell::new(0), Cell::new(None), Cell::new(None));
    let mut build = |ui: &mut Ui| {
        view.set(Some(ui.view("a", 1, Size::Px(100.0), Size::Px(50.0), &mut |ui| {
            runs.set(runs.get() + 1);
            node.set(Some(chip(ui, "x", RED, CLICKABLE)));
        })));
    };
    ui.frame(&at(0.0, vec![move_to((300, 230))]), &mut build);
    ui.frame(&at(0.1, vec![]), &mut build);
    let (v, n) = (view.get().unwrap(), node.get().unwrap());
    let (instances, revision) = (ui.output().draw[0].instances.clone(), ui.draw_revision());
    ui.view_offset(&v, 30.0, 10.0);
    check(ui.output().draw[0].instances == instances, "an offset rewrote instances");
    check(ui.draw_revision() == revision + 1, "an offset kept the revision");
    let entry = ui.output().views.iter().find(|(_, e)| e.offset == [30.0, 10.0]);
    check(entry.is_some(), "no view entry carries the offset");
    check(ui.output().draw[0].instances.iter().any(|i| Some(i.view) == entry.map(|e| e.0)), "the view's instances do not name its entry");
    check(ui.output().damage == Some(Rect::new(0.0, 0.0, 130.0, 60.0)), &format!("offset damage {:?}", ui.output().damage));
    check(ui.rect(&n) == Rect::new(30.0, 10.0, 40.0, 20.0), &format!("rect after an offset {:?}", ui.rect(&n)));
    ui.view_opacity(&v, 0.5);
    check(ui.output().views.iter().any(|(_, e)| e.opacity == 0.5) && ui.output().draw[0].instances == instances, "opacity");
    // the old place no longer hits; the new one does, and the frame keeps the offset
    ui.frame(&at(0.2, vec![move_to((5, 5))]), &mut build);
    check(!ui.signal(&n).hovered, "the node hit at its old place");
    ui.frame(&at(0.3, vec![move_to((35, 15))]), &mut build);
    check(ui.signal(&n).hovered && runs.get() == 2, "the node missed at its offset place");
    check(ui.output().views.iter().any(|(_, e)| e.offset == [30.0, 10.0]), "a frame dropped the offset");
    ui.destroy();
}

/// Damage: none for an idle frame; one node's rectangle for a one-node change; old and new for a
/// patch that moves it.
pub fn window_pxui_view_damage() {
    let mut ui = new_ui(None);
    let (fill, rev, node) = (Cell::new(RED), Cell::new(1u64), Cell::new(None));
    let mut build = |ui: &mut Ui| {
        ui.view("v", rev.get(), Size::Px(100.0), Size::Px(60.0), &mut |ui| {
            chip(ui, "first", BLUE, NO_FLAGS);
            node.set(Some(chip(ui, "second", fill.get(), NO_FLAGS)));
        });
        chip(ui, "root", BLUE, NO_FLAGS);
    };
    ui.frame(&at(0.0, vec![]), &mut build);
    ui.frame(&at(0.1, vec![]), &mut build);
    check(ui.output().damage.is_none(), "an idle frame has damage");
    for i in 0..ui.output().draw.len() {
        ui.drawn(i);
    }
    (fill.set(color::rgb(0, 160, 0)), rev.set(2));
    ui.frame(&at(0.2, vec![]), &mut build);
    check(ui.output().damage == Some(Rect::new(0.0, 20.0, 40.0, 20.0)), &format!("one node's damage {:?}", ui.output().damage));
    check(ui.output().draw[0].dirty.len() == 1 && ui.output().draw[0].dirty[0].1 - ui.output().draw[0].dirty[0].0 == 1, &format!("dirty {:?}", ui.output().draw[0].dirty));
    let n = node.get().unwrap();
    check(ui.patch_fill(&n, BLUE) && ui.output().damage == Some(Rect::new(0.0, 20.0, 40.0, 20.0)), "patch damage");
    check(ui.patch_offset(&n, 5.0, 0.0) && ui.output().damage == Some(Rect::new(0.0, 20.0, 45.0, 20.0)), &format!("move damage {:?}", ui.output().damage));
    ui.destroy();
}

/// A kit panel in a view: kept and rebuilt output are the same instances.
pub fn window_pxui_view_kit_cached() {
    let runs = Cell::new(0);
    let panel = |ui: &mut Ui, rev: u64| {
        ui.view("kit", rev, Size::Px(310.0), Size::Px(220.0), &mut |ui| {
            runs.set(runs.get() + 1);
            ui.panel(PanelOpts::default(), "panel", &mut |ui| {
                ui.label("PXUI");
                ui.toggle("Animate", true);
                ui.text_field("Caption", "Functional UI");
                ui.button("Quit");
            });
        });
    };
    let (mut kept, mut fresh) = (new_ui(None), new_ui(None));
    for t in 0..4 {
        kept.frame(&at(t as f64, vec![]), &mut |ui| panel(ui, 1));
        fresh.frame(&at(t as f64, vec![]), &mut |ui| panel(ui, t));
    }
    check(runs.get() == 1 + 4, &format!("the kit panel view ran {} builds", runs.get()));
    check(kept.output().draw[0].instances == fresh.output().draw[0].instances, "kept and rebuilt kit panels differ");
    check(kept.output().views == fresh.output().views, "kept and rebuilt view entries differ");
    kept.destroy();
    fresh.destroy();
}

/// 100,000 rows build only the rows in view; a row keeps its state while it scrolls.
pub fn window_pxui_list() {
    let mut ui = new_ui(None);
    let (built, cells) = (Cell::new(0), Cell::new(None));
    let mut build = |ui: &mut Ui| {
        built.set(0);
        ui.list("rows", 100_000, 24.0, &mut |ui, i| {
            built.set(built.get() + 1);
            let cell = ui.node("cell", Node { h: Size::Px(24.0), ..Node::DEFAULT });
            if i == 5 {
                cells.set(Some(cell));
            }
        });
    };
    ui.frame(&at(0.0, vec![move_to((100, 100))]), &mut build);
    ui.frame(&at(0.1, vec![]), &mut build);
    // 240 points of 24-point rows: 10 visible, one partly, and 4 of overscan past each edge
    check(built.get() <= 10 + 1 + 2 * 4, &format!("{} rows built", built.get()));
    check(ui.built_nodes() <= 3 + 2 * built.get(), &format!("{} nodes for {} rows", ui.built_nodes(), built.get()));
    let cell = cells.get().unwrap();
    ui.set_state(&cell, 42);
    ui.frame(&at(0.2, vec![Event::Scrolled((0.0, -3.0))]), &mut build);
    check(built.get() <= 10 + 1 + 2 * 4, &format!("{} rows built after scrolling", built.get()));
    let again = cells.get().unwrap();
    check(again.box_slot == cell.box_slot && ui.live(&cell) && ui.state(&again, 0) == 42, "a row lost its state scrolling");
    check(ui.rect(&again).y == 5.0 * 24.0 - 72.0, &format!("row 5 at {:?} after three rows of scroll", ui.rect(&again)));
    ui.destroy();
}

/// A forgotten revision: verify mode re-runs the clean view and names it.
pub fn window_pxui_view_verify() {
    let fill = Cell::new(RED);
    let mut ui = new_ui(None);
    let mut build = |ui: &mut Ui| {
        ui.view("stale-view", 1, Size::Px(100.0), Size::Px(50.0), &mut |ui| {
            chip(ui, "x", fill.get(), NO_FLAGS);
        });
    };
    ui.frame(&at(0.0, vec![]), &mut build);
    ui.verify_views(true);
    ui.frame(&at(0.1, vec![]), &mut build);
    fill.set(BLUE);
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| { ui.frame(&at(0.2, vec![]), &mut build); }));
    let message = result.err().and_then(|e| e.downcast_ref::<String>().cloned()).unwrap_or_default();
    check(message.contains("stale-view"), &format!("verify did not name the stale view: {message:?}"));
}

/// Allocations and CPU per frame for ten views of 50 kit widgets: all kept, one rebuilt, all
/// rebuilt (docs/pxui-baseline.md).
pub fn window_pxui_view_costs() {
    const NAMES: [&str; 10] = ["v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9"];
    let revisions = std::cell::RefCell::new([1u64; 10]);
    let build = |ui: &mut Ui| {
        let revisions = *revisions.borrow();
        for (v, rev) in revisions.iter().enumerate() {
            ui.view(NAMES[v], *rev, Size::Px(300.0), Size::Px(25.0 * 24.0), &mut |ui| {
                for w in 0..25 {
                    ui.label("LABEL");
                    ui.button(if w % 2 == 0 { "Even" } else { "Odd" });
                }
            });
        }
    };
    let mut ui = new_ui(None);
    let mut t = 0.0;
    let mut measure = |ui: &mut Ui, bump: usize, name: &str, zero: bool| {
        let (n, mut ns) = (100u64, 0u128);
        for _ in 0..10 {
            t += 1.0 / 60.0;
            revisions.borrow_mut().iter_mut().take(bump).for_each(|r| *r += 1);
            ui.frame(&at(t, vec![]), &mut |ui| build(ui));
        }
        mem_sys::local_reset();
        let before = mem_sys::local_alloc_count();
        for _ in 0..n {
            t += 1.0 / 60.0;
            revisions.borrow_mut().iter_mut().take(bump).for_each(|r| *r += 1);
            let start = std::time::Instant::now();
            ui.frame(&at(t, vec![]), &mut |ui| build(ui));
            ns += start.elapsed().as_nanos();
        }
        let allocs = mem_sys::local_alloc_count() - before;
        eprintln!("PXUI_VIEWS {name}: {:.1} us per frame, {:.1} allocs per frame, {} nodes built", ns as f64 / n as f64 / 1e3, allocs as f64 / n as f64, ui.built_nodes());
        if zero {
            check(allocs == 0, &format!("{name}: {allocs} allocations in {n} warm frames"));
        }
    };
    measure(&mut ui, 0, "all kept", true);
    measure(&mut ui, 1, "one of 50 widgets rebuilt", true);
    measure(&mut ui, 10, "all 500 widgets rebuilt", false);
    ui.destroy();
}

pub const CASES: &[(&str, fn())] = &[
    ("window_pxui_view_rebuilds_one", window_pxui_view_rebuilds_one),
    ("window_pxui_view_nested", window_pxui_view_nested),
    ("window_pxui_view_input", window_pxui_view_input),
    ("window_pxui_view_offset", window_pxui_view_offset),
    ("window_pxui_view_damage", window_pxui_view_damage),
    ("window_pxui_view_kit_cached", window_pxui_view_kit_cached),
    ("window_pxui_list", window_pxui_list),
    ("window_pxui_view_verify", window_pxui_view_verify),
    ("window_pxui_view_costs", window_pxui_view_costs),
];
