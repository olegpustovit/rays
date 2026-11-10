//! Retained nodes (layer 1): stable handles, direct patches between builds, and the frame arena.
use crate::support::*;
use pxui::geom::color;
use pxui::ui::*;
use std::cell::Cell;
use std::rc::Rc;

fn frame(ui: &mut Ui, t: f64, f: &mut dyn FnMut(&mut Ui)) {
    ui.frame(&frame_at(vec![], 2.0, t, vec![]), f);
}

fn chip(ui: &mut Ui, key: &str, text: &str, fill: color::T) -> Handle {
    let look = Look { fill, text: color::black(), ..Look::NONE };
    let h = ui.node(key, Node { w: Size::Px(80.0), h: Size::Px(20.0), look, inset: Edges { l: 4.0, ..Edges::default() }, ..Node::DEFAULT });
    ui.text(&h, text);
    h
}

fn has_color(ui: &Ui, c: color::T) -> bool {
    ui.output().draw[0].instances.iter().any(|i| i.color == [c.r as u8, c.g as u8, c.b as u8, c.a as u8])
}

pub fn window_pxui_node_handles() {
    let mut ui = new_ui(None);
    let (mut first, mut again) = (None, None);
    frame(&mut ui, 0.0, &mut |ui| first = Some(chip(ui, "a", "one", color::white())));
    frame(&mut ui, 0.1, &mut |ui| again = Some(chip(ui, "a", "one", color::white())));
    let (a, again) = (first.unwrap(), again.unwrap());
    check(a.box_slot == again.box_slot && a.generation == again.generation && ui.live(&a), "a node's handle changed across frames");
    // the owner stops declaring it: the handle is stale, and a new node in its slot is not reached
    frame(&mut ui, 0.2, &mut |_| {});
    check(!ui.live(&a) && !ui.patch_text(&a, "x") && ui.rect(&a).w == 0.0, "a stale handle reached its old slot");
    let mut b = None;
    frame(&mut ui, 0.3, &mut |ui| b = Some(chip(ui, "b", "two", color::white())));
    let b = b.unwrap();
    check(b.box_slot == a.box_slot && !ui.live(&a) && ui.live(&b), "the slot was not reused under a new generation");
    check(!ui.patch_fill(&a, color::rgb(1, 2, 3)) && !has_color(&ui, color::rgb(1, 2, 3)), "a stale patch reached the new node");
    check(!ui.signal(&a).hovered, "a stale handle has a signal");
    ui.destroy();
}

pub fn window_pxui_node_patch() {
    let mut ui = new_ui(None);
    let builds = Cell::new(0);
    let (red, blue) = (color::rgb(200, 0, 0), color::rgb(0, 0, 200));
    let handles = Cell::new(None);
    let mut build = |ui: &mut Ui| {
        builds.set(builds.get() + 1);
        handles.set(Some((chip(ui, "clock", "12:00", red), chip(ui, "after", "x", color::white()))));
    };
    frame(&mut ui, 0.0, &mut build);
    frame(&mut ui, 0.1, &mut build);
    let (clock, after) = handles.get().unwrap();
    let (built, revision, count) = (builds.get(), ui.draw_revision(), ui.output().draw[0].instances.len());
    check(ui.patch_fill(&clock, blue), "patch_fill");
    check(has_color(&ui, blue) && !has_color(&ui, red), "patch_fill did not rewrite the fill instance");
    check(ui.output().draw[0].instances.len() == count, "an in-place patch changed the instance count");
    // a longer text shifts the later node's range; patching that node still hits its own instances
    check(ui.patch_text(&clock, "12:00:01"), "patch_text");
    check(ui.output().draw[0].instances.len() == count + 3, "patch_text did not add the new glyphs");
    let green = color::rgb(0, 160, 0);
    check(ui.patch_fill(&after, green) && has_color(&ui, green) && has_color(&ui, blue), "a shifted range missed its node");
    check(builds.get() == built && ui.draw_revision() > revision, "a patch ran the build or kept the revision");
    check(!ui.wants_frame(&[], 10.0), "a pure patch asked for a frame");
    // the owner rebuilds the node: the build's value wins over the patch
    frame(&mut ui, 0.2, &mut build);
    check(has_color(&ui, red) && !has_color(&ui, blue) && ui.output().draw[0].instances.len() == count, "the patch outlived the node's next build");
    ui.destroy();
}

thread_local! { static DROPS: Cell<usize> = const { Cell::new(0) }; }

struct Counted(#[allow(dead_code)] u64);
impl std::ops::Drop for Counted {
    fn drop(&mut self) {
        DROPS.with(|d| d.set(d.get() + 1));
    }
}

pub fn window_pxui_arena_closures() {
    let mut ui = new_ui(None);
    let ran = Rc::new(Cell::new(0));
    DROPS.with(|d| d.set(0));
    for t in 0..3 {
        let ran = ran.clone();
        frame(&mut ui, t as f64, &mut |ui| {
            let h = ui.node("c", Node { w: Size::Px(10.0), h: Size::Px(10.0), ..Node::DEFAULT });
            let (byte, word, wide, counted, ran) = (7u8, 9u64, 11u128, Counted(5), ran.clone());
            ui.canvas(&h, move |_, _| {
                let aligned = |p: usize, a: usize| p % a == 0;
                assert!(aligned(&word as *const u64 as usize, 8) && aligned(&wide as *const u128 as usize, align_of::<u128>()), "a capture is misaligned");
                assert!(byte == 7 && word == 9 && wide == 11 && counted.0 == 5, "a capture was corrupted");
                ran.set(ran.get() + 1);
            });
            let one = ui.node("u8", Node { w: Size::Px(1.0), h: Size::Px(1.0), ..Node::DEFAULT });
            ui.canvas_over(&one, move |_, _| assert!(byte == 7));
        });
        check(DROPS.with(|d| d.get()) == t + 1, "a canvas closure was not dropped exactly once after its frame");
    }
    check(ran.get() == 3, "a canvas closure did not run once per frame");
    ui.destroy();
    check(DROPS.with(|d| d.get()) == 3, "destroy dropped a closure twice");
}

/// The first frame of a new canvas node in a warm UI allocates nothing: the closure goes into the
/// frame arena and the node reuses a freed slot.
pub fn window_pxui_arena_first_frame() {
    let mut ui = new_ui(None);
    let kit = |ui: &mut Ui, extra: Option<&str>| {
        ui.panel(PanelOpts::default(), "panel", &mut |ui| {
            ui.label("PXUI");
            ui.button("Quit");
            if let Some(key) = extra {
                let h = ui.node(key, Node { h: Size::Px(10.0), ..Node::DEFAULT });
                let ink = color::rgb(1, 2, 3);
                ui.canvas(&h, move |paint, r| paint.fill(r.x, r.y, r.w, r.h, ink));
            }
        });
    };
    for i in 0..10 {
        frame(&mut ui, i as f64, &mut |ui| kit(ui, Some("old")));
    }
    for i in 10..20 {
        frame(&mut ui, i as f64, &mut |ui| kit(ui, None));
    }
    mem_sys::local_reset();
    let before = mem_sys::local_alloc_count();
    frame(&mut ui, 20.0, &mut |ui| kit(ui, Some("new")));
    let allocs = mem_sys::local_alloc_count() - before;
    check(has_color(&ui, color::rgb(1, 2, 3)), "the new canvas did not paint");
    check(allocs == 0, &format!("the first frame of a new canvas node allocated {allocs} times"));
    ui.destroy();
}

pub const CASES: &[(&str, fn())] = &[
    ("window_pxui_node_handles", window_pxui_node_handles),
    ("window_pxui_node_patch", window_pxui_node_patch),
    ("window_pxui_arena_closures", window_pxui_arena_closures),
    ("window_pxui_arena_first_frame", window_pxui_arena_first_frame),
];
