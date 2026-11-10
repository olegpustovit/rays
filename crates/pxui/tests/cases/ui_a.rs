//! Scale-parameterized interaction, layout and retained state contracts.
use crate::steps::*;
use crate::support::*;
use input::{Event, Key, MouseButton, ScrollPhase};
use pxui::ui::*;
use rays::scene::layer;
use std::cell::{Cell, RefCell};
use std::rc::Rc;

fn parse(text: &str) -> Option<f64> {
    (text).parse::<f64>().ok()
}

fn field_ui() -> Ui {
    new_ui(Some(11))
}

fn scale_1(scale: f64) {
    let lab = |m: &str| label(scale, m);
    let mut ui = field_ui();
    let roots = |ui: &mut Ui, events: Vec<Event>| -> (Signal, Signal, i64) {
        let mut out = None;
        ui.frame(&frame_at(vec![], scale, 0.5, events), &mut |ui| {
            let root = ui.box_("root", BoxOpts::default().flags(CLICKABLE).w(Size::Px(100.0)).h(Size::Px(100.0)));
            let saved = ui.enter(&root);
            let child = ui.box_("child", BoxOpts::default().flags(CLICKABLE).w(Size::Px(30.0)).h(Size::Px(30.0)).at((10.0, 10.0)));
            ui.leave(saved);
            out = Some((ui.signal(&root), ui.signal(&child), key(&root)));
        });
        out.unwrap()
    };
    let (_, _, root_key) = roots(&mut ui, vec![]);
    if ui.last_press_within(&frame_at(vec![], scale, 0.5, vec![press((15, 15))]), &[root_key]) != Some(root_key) {
        fail(&lab("hit-tree preview missed a child press"));
    }
    let (root, child, _) = roots(&mut ui, vec![press((15, 15))]);
    if root.pressed || root.subtree_press != Some(0) || !child.pressed || child.subtree_press != Some(0) {
        fail(&lab("child press did not reach its hit ancestor"));
    }
    let (root, child, _) = roots(&mut ui, vec![release((15, 15)), press((80, 80))]);
    if !root.pressed || root.subtree_press != Some(1) || child.pressed {
        fail(&lab("blank root press did not select the root"));
    }
    ui.destroy();
}

fn scale_2(scale: f64) {
    let lab = |m: &str| label(scale, m);
    // Toggle commits on release inside, never on press alone.
    let mut ui = field_ui();
    let value = Cell::new(false);
    let mut build = |ui: &mut Ui| value.set(ui.toggle("Enabled", value.get()));
    settle(scale, &mut ui, &mut build);
    step(scale, &mut ui, vec![press((210, row(0)))], &mut build);
    if value.get() {
        fail(&lab("toggle fired before release"));
    }
    step(scale, &mut ui, vec![release((210, row(0)))], &mut build);
    if !value.get() {
        fail(&lab("toggle did not react to a click"));
    }
    step(scale, &mut ui, vec![press((210, row(0))), release((300, row(0)))], &mut build);
    if !value.get() {
        fail(&lab("toggle fired after release outside"));
    }
    step(scale, &mut ui, vec![press((237, row(0))), release((236, row(0)))], &mut build);
    if !value.get() {
        fail(&lab("half-open bounds accepted a press on the right edge"));
    }
    ui.destroy();
}

fn scale_3(scale: f64) {
    let lab = |m: &str| label(scale, m);
    // Compact inspector sliders use the visible track position, including on a simple click, and
    // Option-click opens the numeric editor.
    let compact = RefCell::new("0".to_owned());
    let mut build = |ui: &mut Ui| {
        let current = compact.borrow().clone();
        let o = ValueFieldOpts {
            fraction: Some(parse(&current).unwrap_or(0.0)),
            slide: Some(Rc::new(|fraction| cfmt::g(fraction, 17))),
            ..ValueFieldOpts::default()
        };
        let (value, _) = ui.value_field((20.0, 20.0), 101.0, 21.0, &o, &|text| parse(text).is_some(), "compact", &current);
        *compact.borrow_mut() = value;
    };
    let mut ui = field_ui();
    ui.frame(&frame_at(vec![], scale, 0.0, vec![]), &mut build);
    // the first page of 1024 x 256 texels: alpha only on the host
    if ui.bytes().atlas != 1024 * 256 {
        fail(&lab("the glyph mirror is not alpha only"));
    }
    // two panes texts, frame after frame: each is coloured once
    let coloured = Rc::new(Cell::new(0));
    let counter = coloured.clone();
    let counting = Language {
        colorize: Rc::new(move |_| {
            counter.set(counter.get() + 1);
            Vec::new()
        }),
        brackets: Rc::new(|_| Vec::new()),
        indent: Rc::new(|_, _| String::new()),
        pairs: Vec::new(),
        complete: Rc::new(|_, _| Vec::new()),
        describe: Rc::new(|_, _| None),
        number_at: Rc::new(|_, _| None),
        context_at: Rc::new(|_, _| CodeContext::Code),
        comment: ";".to_owned(),
        rewrite: None,
    };
    for _ in 0..3 {
        ui.text_colors(&counting, "(first)");
        ui.text_colors(&counting, "(second)");
    }
    if coloured.get() != 2 {
        fail(&lab("a text a pane shows again was coloured again"));
    }
    ui.frame(&frame_at(vec![], scale, 0.5, vec![press((70, 30)), release((70, 30))]), &mut build);
    if (parse(&compact.borrow()).unwrap() - 0.5).abs() > 0.02 || ui.text_input_focused() {
        fail(&lab("compact slider click did not jump to the pointer"));
    }
    ui.frame(&frame_at(vec![Key::Alt], scale, 1.0, vec![press((90, 30)), release((90, 30))]), &mut build);
    if !ui.text_input_focused() {
        fail(&lab("Option-click did not open compact numeric entry"));
    }
    // The track keeps the pointer from press to release, even outside the field; a lost window or
    // a cancelled pointer ends the capture.
    *compact.borrow_mut() = "0".to_owned();
    let mut ui2 = field_ui();
    let mut tick = |ui: &mut Ui, time: f64, events: Vec<Event>| { ui.frame(&frame_at(vec![], scale, time, events), &mut build); };
    tick(&mut ui2, 0.0, vec![]);
    tick(&mut ui2, 0.5, vec![press((50, 30))]);
    tick(&mut ui2, 1.0, vec![move_to((400, 30))]);
    if parse(&compact.borrow()).unwrap() != 1.0 {
        fail(&lab("slide drag did not capture beyond the field"));
    }
    tick(&mut ui2, 1.5, vec![move_to((30, 30))]);
    if parse(&compact.borrow()).unwrap() >= 0.5 {
        fail(&lab("slide did not follow the captured pointer"));
    }
    tick(&mut ui2, 2.0, vec![release((30, 30))]);
    tick(&mut ui2, 2.5, vec![press((50, 30))]);
    let before = compact.borrow().clone();
    tick(&mut ui2, 3.0, vec![Event::FocusLost, move_to((400, 30))]);
    if *compact.borrow() != before {
        fail(&lab("window focus loss did not cancel capture"));
    }
    tick(&mut ui2, 3.5, vec![press((50, 30))]);
    let before = compact.borrow().clone();
    tick(&mut ui2, 4.0, vec![Event::PointerCancelled(MouseButton::LeftButton), move_to((400, 30))]);
    if *compact.borrow() != before {
        fail(&lab("pointer cancellation did not stop capture"));
    }
    ui.destroy();
    ui2.destroy();
}

fn scale_4(scale: f64) {
    let lab = |m: &str| label(scale, m);
    // Buttons fire on release inside only.
    let clicks = Cell::new(0);
    let mut build = |ui: &mut Ui| {
        if ui.button("Apply") {
            clicks.set(clicks.get() + 1);
        }
    };
    let mut ui = field_ui();
    settle(scale, &mut ui, &mut build);
    step(scale, &mut ui, vec![press((20, row(0))), move_to((300, row(0))), release((300, row(0)))], &mut build);
    if clicks.get() != 0 {
        fail(&lab("button fired after release outside"));
    }
    step(scale, &mut ui, vec![press((20, row(0))), release((20, row(0)))], &mut build);
    if clicks.get() != 1 {
        fail(&lab("button did not fire on release inside"));
    }
    ui.destroy();
}

fn scale_5(scale: f64) {
    let lab = |m: &str| label(scale, m);
    // A collapsed inspector section builds none of its rows; a click on its header opens it.
    let built = Cell::new(false);
    let opened = Cell::new(None::<bool>);
    let mut build = |ui: &mut Ui| {
        built.set(false);
        ui.inspector_section("Advanced", false, None, "Advanced", &mut |ui| {
            built.set(true);
            ui.toggle("Nested", false);
        });
        opened.set(ui.expanded("Advanced"));
    };
    let mut ui = field_ui();
    settle(scale, &mut ui, &mut build);
    if built.get() || opened.get() != Some(false) {
        fail(&lab("a collapsed section built its rows"));
    }
    step(scale, &mut ui, vec![press((20, row(0))), release((20, row(0)))], &mut build);
    settle(scale, &mut ui, &mut build);
    if !built.get() || opened.get() != Some(true) {
        fail(&lab("a click did not open its section"));
    }
    ui.destroy();
}

fn scale_6(scale: f64) {
    let lab = |m: &str| label(scale, m);
    // A numeric field edits as text: Enter commits, invalid text stays open, Escape cancels, a
    // pasted line loses its break, the caret takes insertions and an outside press commits.
    let count = RefCell::new("2".to_owned());
    let mut build = |ui: &mut Ui| {
        let current = count.borrow().clone();
        let (value, _) = ui.value_field((20.0, 20.0), 101.0, 21.0, &ValueFieldOpts::default(), &|text| parse(text).is_some(), "count", &current);
        *count.borrow_mut() = value;
    };
    let mut ui = field_ui();
    let clock = Cell::new(0.0);
    let fstep = |ui: &mut Ui, keys: Vec<Key>, dt: f64, events: Vec<Event>, build: &mut dyn FnMut(&mut Ui)| {
        clock.set(clock.get() + dt);
        rays_shell::host::frame(ui, &frame_at(keys, scale, clock.get(), events), build);
    };
    let open_field = |ui: &mut Ui, build: &mut dyn FnMut(&mut Ui)| {
        fstep(ui, vec![], 0.5, vec![press((70, 30)), release((70, 30))], build);
        fstep(ui, vec![], 0.1, vec![press((70, 30)), release((70, 30))], build);
    };
    fstep(&mut ui, vec![], 0.5, vec![], &mut build);
    open_field(&mut ui, &mut build);
    if !ui.text_input_focused() {
        fail(&lab("a click did not open the field"));
    }
    fstep(&mut ui, vec![], 0.5, vec![Event::TextInput("27".into()), key_down(Key::Enter)], &mut build);
    if *count.borrow() != "27" || ui.text_input_focused() {
        fail(&lab(&format!("typed number was not committed: {}", count.borrow())));
    }
    open_field(&mut ui, &mut build);
    fstep(&mut ui, vec![], 0.5, vec![Event::TextInput("x".into()), key_down(Key::Enter)], &mut build);
    if *count.borrow() != "27" || !ui.text_input_focused() {
        fail(&lab("invalid number text was committed or dismissed"));
    }
    fstep(&mut ui, vec![], 0.5, vec![key_down(Key::Escape), Event::TextInput("4".into())], &mut build);
    if *count.borrow() != "27" || ui.text_input_focused() {
        fail(&lab("Escape did not cancel numeric editing"));
    }
    open_field(&mut ui, &mut build);
    with_clipboard(&mut || {
        // a copied line comes with its break: a field of one line takes the line
        rays::clipboard::set_text("12\n").unwrap_or_else(|m| fail(&m));
        fstep(&mut ui, vec![Key::Meta], 0.5, vec![key_down(kc('v'))], &mut build);
        fstep(&mut ui, vec![], 0.5, vec![key_down(Key::Enter)], &mut build);
        if *count.borrow() != "12" {
            fail(&lab("numeric editor refused a pasted line for its break"));
        }
        open_field(&mut ui, &mut build);
        rays::clipboard::set_text("31").unwrap_or_else(|m| fail(&m));
        fstep(&mut ui, vec![Key::Meta], 0.5, vec![key_down(kc('v'))], &mut build);
        fstep(&mut ui, vec![], 0.5, vec![key_down(Key::Enter)], &mut build);
        if *count.borrow() != "31" {
            fail(&lab("numeric editor did not paste valid text"));
        }
    });
    open_field(&mut ui, &mut build);
    fstep(
        &mut ui,
        vec![],
        0.5,
        vec![key_down(Key::ArrowRight), key_down(Key::ArrowLeft), Event::TextInput("2".into()), key_down(Key::Enter)],
        &mut build,
    );
    if *count.borrow() != "321" {
        fail(&lab("numeric editor ignored its insertion caret"));
    }
    open_field(&mut ui, &mut build);
    fstep(&mut ui, vec![], 0.5, vec![Event::TextInput("2.5".into())], &mut build);
    fstep(&mut ui, vec![], 0.5, vec![press((300, 200))], &mut build);
    if *count.borrow() != "2.5" {
        fail(&lab("typed number was not committed by an outside press"));
    }
    ui.destroy();
}

fn scale_7(scale: f64) {
    let lab = |m: &str| label(scale, m);
    // A bounded panel scrolls by whole rows and routes hits to scrolled rows.
    let toggles = RefCell::new([false; 4]);
    let mut scrolled = |ui: &mut Ui| {
        ui.panel(PanelOpts::default().at(0.0, 0.0).width(240.0).max_height(80.0), "scroll", &mut |ui| {
            for index in 0..4 {
                let v = toggles.borrow()[index];
                let n = ui.toggle(&format!("Row {}", index), v);
                toggles.borrow_mut()[index] = n;
            }
        });
    };
    let mut ui = field_ui();
    let mut scroll_step = |ui: &mut Ui, events: Vec<Event>| {
        let t = advance();
        ui.frame(&frame_at(vec![], scale, t, events), &mut scrolled);
    };
    scroll_step(&mut ui, vec![]);
    scroll_step(&mut ui, vec![move_to((100, 40)), Event::Scrolled((0.0, -2.0))]);
    // 102 points of rows in an 80-point panel scroll by at most 22.
    scroll_step(&mut ui, vec![press((210, 40)), release((210, 40))]);
    {
        let t = toggles.borrow();
        if !t[2] || t[0] || t[1] {
            fail(&lab("scrolled panel did not route the hit to the scrolled row"));
        }
    }
    ui.destroy();

    let fixed = RefCell::new([false; 4]);
    let mut ui = field_ui();
    let fixed_panel = |ui: &mut Ui, events: Vec<Event>| {
        let t = advance();
        ui.frame(&frame_at(vec![], scale, t, events), &mut |ui| {
            ui.panel(PanelOpts::default().at(0.0, 0.0).width(240.0).height(80.0), "fixed", &mut |ui| {
                for index in 0..4 {
                    let v = fixed.borrow()[index];
                    let n = ui.toggle(&format!("Fixed {}", index), v);
                    fixed.borrow_mut()[index] = n;
                }
            });
        });
    };
    fixed_panel(&mut ui, vec![]);
    fixed_panel(&mut ui, vec![move_to((100, 40)), Event::Scrolled((0.0, -2.0))]);
    fixed_panel(&mut ui, vec![press((210, 40)), release((210, 40))]);
    {
        let f = fixed.borrow();
        if !f[2] || f[0] || f[1] {
            fail(&lab("fixed-height inspector panel did not scroll its rows"));
        }
    }
    ui.destroy();

    let mut ui = field_ui();
    let child_y = Cell::new(0.0);
    let rubber = |ui: &mut Ui, events: Vec<Event>| {
        let t = advance();
        let mut child = None;
        ui.frame(&frame_at(vec![], scale, t, events), &mut |ui| {
            let parent = ui.box_(
                "rubber",
                BoxOpts::default().flags(SCROLL + CLIP).at((0.0, 0.0)).w(Size::Px(100.0)).h(Size::Px(80.0)).scroll_step(24.0),
            );
            let saved = ui.enter(&parent);
            child = Some(ui.box_("content", BoxOpts::default().w(Size::Px(100.0)).h(Size::Px(100.0))));
            ui.leave(saved);
        });
        child_y.set(ui.rect(&child.unwrap()).1);
    };
    rubber(&mut ui, vec![]);
    rubber(&mut ui, vec![move_to((50, 40)), Event::Scrolled((0.0, 1.0))]);
    if child_y.get() <= 0.0 {
        fail(&lab("scroll did not stretch past the top edge"));
    }
    rubber(&mut ui, vec![]);
    if child_y.get().abs() > 0.25 {
        fail(&lab("rubber scroll did not spring back to the edge"));
    }
    ui.destroy();

    let mut ui = field_ui();
    let scroll_dir = |ui: &mut Ui, events: Vec<Event>| -> UiBox {
        let t = advance();
        let mut parent = None;
        ui.frame(&frame_at(vec![], scale, t, events), &mut |ui| {
            let p = ui.box_(
                "direction",
                BoxOpts::default().flags(SCROLL + CLIP).at((0.0, 0.0)).w(Size::Px(100.0)).h(Size::Px(80.0)).scroll_step(24.0),
            );
            let saved = ui.enter(&p);
            ui.box_("long", BoxOpts::default().w(Size::Px(100.0)).h(Size::Px(240.0)));
            ui.leave(saved);
            parent = Some(p);
        });
        parent.unwrap()
    };
    let parent = scroll_dir(&mut ui, vec![]);
    ui.set_scroll_offset(&parent, 80.0);
    let parent = scroll_dir(&mut ui, vec![move_to((50, 40)), Event::Scrolled((0.0, 1.0))]);
    if ui.scroll_offset(&parent) != 56.0 {
        fail(&lab("positive natural scroll did not move the view up"));
    }
    let parent = scroll_dir(&mut ui, vec![Event::Scrolled((0.0, -1.0))]);
    if ui.scroll_offset(&parent) != 80.0 {
        fail(&lab("negative wheel scroll did not move the view down"));
    }
    ui.destroy();
}

struct Pad {
    ui: Ui,
    clock: f64,
    dt: f64,
    content: f64,
    h: Size,
}

impl Pad {
    fn new(content: f64, start: f64, h: Size, dt: f64, scale: f64) -> Pad {
        let mut p = Pad { ui: field_ui(), clock: 100.0, dt, content, h };
        let parent = p.step(scale, vec![move_to((50, 40))]);
        p.ui.set_scroll_offset(&parent, start);
        p.step(scale, vec![Event::TrackpadScrolled { delta: (0.0, 0.0), phase: ScrollPhase::Touched, time: 5.0 }]);
        p
    }

    fn step(&mut self, scale: f64, events: Vec<Event>) -> UiBox {
        self.clock += self.dt;
        let (content, h) = (self.content, self.h.clone());
        let mut parent = None;
        self.ui.frame(&frame_at(vec![], scale, self.clock, events), &mut |ui| {
            let p = ui.box_(
                "pad",
                BoxOpts::default().flags(SCROLL + CLIP).at((0.0, 0.0)).w(Size::Px(100.0)).h(h.clone()).scroll_step(24.0),
            );
            let saved = ui.enter(&p);
            ui.box_("long", BoxOpts::default().w(Size::Px(100.0)).h(Size::Px(content)));
            ui.leave(saved);
            parent = Some(p);
        });
        parent.unwrap()
    }
}

fn finger(time: f64, dy: f64) -> Vec<Event> {
    vec![
        Event::TrackpadScrolled { delta: (0.0, dy), phase: ScrollPhase::Moved, time },
        Event::Scrolled((0.0, dy * 0.1)),
    ]
}

fn lifted() -> Vec<Event> {
    vec![Event::TrackpadScrolled { delta: (0.0, 0.0), phase: ScrollPhase::Lifted, time: 5.04 }]
}

fn scale_8(scale: f64) {
    let lab = |m: &str| label(scale, m);
    // A trackpad (Trackpad_scrolled, each motion with the wheel steps SDL makes of it): the fingers
    // move the content point for point, a lift in motion coasts to rest, an edge is overshot and
    // returned to, a stretch is held until the lift, and content that fits stays.
    let glide = |dt: f64| -> f64 {
        let mut pad = Pad::new(2000.0, 0.0, Size::Px(80.0), dt, scale);
        pad.step(scale, finger(5.016, -10.0));
        let parent = pad.step(scale, finger(5.032, -10.0));
        if pad.ui.scroll_offset(&parent) != 20.0 {
            fail(&lab("trackpad fingers did not move the content point for point"));
        }
        let mut previous = 20.0;
        let mut stride = f64::INFINITY;
        let mut parent = pad.step(scale, lifted());
        for _ in 0..(3.0 / dt) as i64 {
            parent = pad.step(scale, vec![]);
            let offset = pad.ui.scroll_offset(&parent);
            // a pixel of slack: the coast is snapped to device pixels
            if offset < previous || offset - previous > stride + 1.0 {
                fail(&lab("trackpad coast did not slow down monotonically"));
            }
            stride = offset - previous;
            previous = offset;
        }
        if stride != 0.0 || pad.ui.scroll_position(&parent) != previous {
            fail(&lab("trackpad coast did not come to rest"));
        }
        pad.ui.destroy();
        previous
    };
    // the same flick at two frame rates: 625 points a second at the lift
    let slow = glide(1.0 / 60.0);
    let fast = glide(1.0 / 120.0);
    if slow < 150.0 || slow > 185.0 {
        fail(&lab("trackpad coast went the wrong distance"));
    }
    if (slow - fast).abs() > 2.0 {
        fail(&lab("trackpad coast depends on the frame rate"));
    }
    // a flick into the top edge overshoots it and returns to it exactly
    let mut pad = Pad::new(2000.0, 20.0, Size::Px(80.0), 1.0 / 60.0, scale);
    pad.step(scale, finger(5.016, 10.0));
    pad.step(scale, finger(5.032, 10.0));
    let mut parent = pad.step(scale, lifted());
    let mut deepest: f64 = 0.0;
    for _ in 0..120 {
        parent = pad.step(scale, vec![]);
        deepest = deepest.min(pad.ui.scroll_position(&parent));
    }
    if deepest > -1.0 || deepest < -80.0 {
        fail(&lab("trackpad coast did not bounce off the edge"));
    }
    if pad.ui.scroll_position(&parent) != 0.0 || pad.ui.scroll_offset(&parent) != 0.0 {
        fail(&lab("trackpad bounce did not settle on the edge"));
    }
    pad.ui.destroy();
    // a pull past the edge is held while the fingers rest and springs back when they lift
    let mut pad = Pad::new(2000.0, 0.0, Size::Px(80.0), 1.0 / 60.0, scale);
    pad.step(scale, finger(5.016, 200.0));
    let mut parent = pad.step(scale, vec![]);
    for _ in 0..60 {
        parent = pad.step(scale, vec![]);
    }
    if pad.ui.scroll_position(&parent) >= -1.0 {
        fail(&lab("held trackpad stretch let go"));
    }
    parent = pad.step(scale, lifted());
    for _ in 0..60 {
        parent = pad.step(scale, vec![]);
    }
    if pad.ui.scroll_position(&parent) != 0.0 {
        fail(&lab("trackpad stretch did not spring back"));
    }
    pad.ui.destroy();
    // a box whose height comes from its parent (a third of 240) stretches as a fixed one does
    let mut pad = Pad::new(2000.0, 0.0, Size::Pct(1.0 / 3.0), 1.0 / 60.0, scale);
    pad.step(scale, finger(5.016, 200.0));
    let parent = pad.step(scale, vec![]);
    if pad.ui.scroll_position(&parent) >= -1.0 {
        fail(&lab("a scroll box of relative height did not stretch"));
    }
    pad.ui.destroy();
    // content shorter than the box neither scrolls, stretches nor coasts
    let mut pad = Pad::new(40.0, 0.0, Size::Px(80.0), 1.0 / 60.0, scale);
    pad.step(scale, finger(5.016, -30.0));
    let mut parent = pad.step(scale, finger(5.032, 30.0));
    let mut ok = pad.ui.scroll_position(&parent) == 0.0;
    parent = pad.step(scale, lifted());
    ok = ok && pad.ui.scroll_position(&parent) == 0.0;
    parent = pad.step(scale, vec![Event::Scrolled((0.0, -3.0))]);
    ok = ok && pad.ui.scroll_position(&parent) == 0.0;
    for _ in 0..30 {
        parent = pad.step(scale, vec![]);
        ok = ok && pad.ui.scroll_position(&parent) == 0.0;
    }
    if !ok {
        fail(&lab("content that fits moved under a scroll"));
    }
    pad.ui.destroy();
}

fn scale_9(scale: f64) {
    let lab = |m: &str| label(scale, m);
    // A pinch goes where the wheel goes: to the scrollable box under the pointer, its factors
    // multiplied within a frame, and nowhere else.
    let mut ui = field_ui();
    let pinched = |ui: &mut Ui, events: Vec<Event>| -> f64 {
        let t = advance();
        let mut seen = 1.0;
        ui.frame(&frame_at(vec![], scale, t, events), &mut |ui| {
            let canvas = ui.box_("pinch", BoxOpts::default().flags(SCROLL + CLIP).at((0.0, 0.0)).w(Size::Px(100.0)).h(Size::Px(80.0)));
            seen = ui.signal(&canvas).pinch;
        });
        seen
    };
    pinched(&mut ui, vec![]);
    if pinched(&mut ui, vec![move_to((50, 40))]) != 1.0 {
        fail(&lab("a frame without a pinch reported one"));
    }
    if pinched(&mut ui, vec![move_to((50, 40)), Event::Pinched(1.25)]) != 1.25 {
        fail(&lab("a pinch over the canvas did not reach its signal"));
    }
    if (pinched(&mut ui, vec![move_to((50, 40)), Event::Pinched(1.1), Event::Pinched(0.5)]) - 0.55).abs() > 1e-9 {
        fail(&lab("two pinches in a frame were not multiplied"));
    }
    if pinched(&mut ui, vec![move_to((300, 200)), Event::Pinched(1.25)]) != 1.0 {
        fail(&lab("a pinch away from the canvas reached it"));
    }
    ui.destroy();
}

fn ime_cursor(ui: &Ui, lab: &dyn Fn(&str) -> String) -> i64 {
    match &layer::text_regions(&rays_shell::host::scene(&ui, None))[..] {
        [(_, _, _, _, true, cursor)] => *cursor,
        _ => fail(&lab("focused text field lost its IME region")),
    }
}

fn scale_10(scale: f64) {
    let lab = |m: &str| label(scale, m);
    // Text fields: UTF-8 entry, Backspace/Delete, focus kept on cancel.
    let title = RefCell::new(String::new());
    let mut build = |ui: &mut Ui| {
        let current = title.borrow().clone();
        *title.borrow_mut() = ui.text_field("Title", &current);
    };
    let mut ui = field_ui();
    settle(scale, &mut ui, &mut build);
    step(
        scale,
        &mut ui,
        vec![
            press((150, row(0))),
            release((150, row(0))),
            Event::TextEditing { text: "e".into(), start: 0, length: 1 },
            Event::TextInput("h\u{e9}".into()),
            key_down(Key::Backspace),
        ],
        &mut build,
    );
    if *title.borrow() != "h" {
        fail(&lab("text field did not accept UTF-8 input and backspace"));
    }
    let cursor_after_h = ime_cursor(&ui, &lab);
    if cursor_after_h <= 8 {
        fail(&lab("IME cursor stayed at the field origin"));
    }
    step(scale, &mut ui, vec![Event::PointerCancelled(MouseButton::LeftButton), Event::TextInput("!".into())], &mut build);
    if *title.borrow() != "h!" {
        fail(&lab("pointer cancellation dismissed text focus"));
    }
    if ime_cursor(&ui, &lab) <= cursor_after_h {
        fail(&lab("IME cursor did not follow the inserted text"));
    }
    step(scale, &mut ui, vec![Event::TextInput(" two words".into()), key_down(Key::Delete), Event::TextInput("!".into())], &mut build);
    if *title.borrow() != "h! two words!" || !ui.text_input_focused() {
        fail(&lab("focused text input lost spaces or changed at-end Delete"));
    }
    step(scale, &mut ui, vec![key_down(Key::ArrowLeft), key_down(Key::Backspace)], &mut build);
    if *title.borrow() != "h! two word!" {
        fail(&lab("Backspace ignored the moved caret"));
    }
    let cursor_before_end = ime_cursor(&ui, &lab);
    step(scale, &mut ui, vec![key_down(Key::End)], &mut build);
    if ime_cursor(&ui, &lab) <= cursor_before_end {
        fail(&lab("IME cursor ignored keyboard caret movement"));
    }
    with_clipboard(&mut || {
        rays::clipboard::set_text(" pasted").unwrap_or_else(|m| fail(&m));
        command_step(scale, &mut ui, 'v', &mut build);
        if *title.borrow() != "h! two word! pasted" {
            fail(&lab("Command-V did not paste into the focused text field"));
        }
        command_step(scale, &mut ui, 'c', &mut build);
        if rays::clipboard::get_text() != Ok(title.borrow().clone()) {
            fail(&lab("Command-C did not copy the focused text field"));
        }
        command_step(scale, &mut ui, 'x', &mut build);
        if !title.borrow().is_empty() || rays::clipboard::get_text() != Ok("h! two word! pasted".to_owned()) {
            fail(&lab("Command-X did not cut the focused text field"));
        }
        step(
            scale,
            &mut ui,
            vec![Event::TextInput("a\u{e9}\u{4e2d}z".into()), key_down(Key::ArrowLeft), key_down(Key::ArrowLeft), key_down(Key::Delete)],
            &mut build,
        );
        if *title.borrow() != "a\u{e9}z" {
            fail(&lab("Delete did not remove one UTF-8 character at the caret"));
        }
        modified_step(scale, &mut ui, vec![Key::Shift], Key::ArrowLeft, &mut build);
        command_step(scale, &mut ui, 'c', &mut build);
        if rays::clipboard::get_text() != Ok("\u{e9}".to_owned()) {
            fail(&lab("Command-C ignored the selected UTF-8 character"));
        }
        command_step(scale, &mut ui, 'x', &mut build);
        if *title.borrow() != "az" {
            fail(&lab("Command-X ignored selection bounds"));
        }
        command_step(scale, &mut ui, 'v', &mut build);
        if *title.borrow() != "a\u{e9}z" {
            fail(&lab("paste ignored the insertion caret"));
        }
    });
    step(scale, &mut ui, vec![press((125, row(0))), release((125, row(0))), Event::TextInput("!".into())], &mut build);
    if *title.borrow() != "!a\u{e9}z" {
        fail(&lab("mouse click did not place the text caret"));
    }
    step(scale, &mut ui, vec![press((300, 200))], &mut build);
    if ui.text_input_focused() {
        fail(&lab("an outside press kept text focus"));
    }
    ui.destroy();

    let short = RefCell::new("x".to_owned());
    let long = RefCell::new("long text".to_owned());
    let mut build = |ui: &mut Ui| {
        let s = short.borrow().clone();
        *short.borrow_mut() = ui.text_field("Short", &s);
        let l = long.borrow().clone();
        *long.borrow_mut() = ui.text_field("Long", &l);
    };
    let mut ui = field_ui();
    settle(scale, &mut ui, &mut build);
    step(scale, &mut ui, vec![press((150, row(1))), release((150, row(1)))], &mut build);
    step(scale, &mut ui, vec![], &mut build);
    if *short.borrow() != "x" || *long.borrow() != "long text" {
        fail(&lab("unfocused short field reused the longer field caret"));
    }
    ui.destroy();
}

fn scale_11(scale: f64) {
    let lab = |m: &str| label(scale, m);
    // a scrub field (a number dragged sideways) opens for typing on a double-click
    let number = RefCell::new("240".to_owned());
    let typing = Cell::new(false);
    let mut build = |ui: &mut Ui| {
        let current = number.borrow().clone();
        let o = ValueFieldOpts { scrub: Some(Rc::new(|origin, _, _| origin.to_owned())), ..ValueFieldOpts::default() };
        let (value, open) = ui.value_field((20.0, 20.0), 100.0, 21.0, &o, &|_| true, "scrub-field", &current);
        *number.borrow_mut() = value;
        typing.set(open);
    };
    let mut ui = field_ui();
    let clock = Cell::new(0.0);
    let tick = |ui: &mut Ui, events: Vec<Event>, build: &mut dyn FnMut(&mut Ui)| {
        clock.set(clock.get() + 0.05);
        ui.frame(&frame_at(vec![], scale, clock.get(), events), build);
    };
    tick(&mut ui, vec![], &mut build);
    tick(&mut ui, vec![move_to((60, 30))], &mut build);
    tick(&mut ui, vec![press((60, 30)), release((60, 30))], &mut build);
    if typing.get() {
        fail(&lab("one click on a scrub field opened typing"));
    }
    tick(&mut ui, vec![press((60, 30)), release((60, 30))], &mut build);
    tick(&mut ui, vec![], &mut build);
    if !typing.get() {
        fail(&lab("a double-click on a scrub field did not open typing"));
    }
    tick(&mut ui, vec![key_down(kc('a')), Event::TextInput("99".into())], &mut build);
    tick(&mut ui, vec![key_down(Key::Enter)], &mut build);
    tick(&mut ui, vec![], &mut build);
    if typing.get() || !number.borrow().ends_with("99") {
        fail(&lab(&format!("typing in a scrub field did not commit: {}", number.borrow())));
    }
    ui.destroy();

    let long_value = "abcdefghijklmnopqrstuvwxyz0123456789";
    let field = RefCell::new(long_value.to_owned());
    let mut build = |ui: &mut Ui| {
        let current = field.borrow().clone();
        let (value, _) = ui.value_field((20.0, 20.0), 100.0, 21.0, &ValueFieldOpts::default(), &|_| true, "long-field", &current);
        *field.borrow_mut() = value;
    };
    let mut ui = field_ui();
    let field_time = Cell::new(0.0);
    let field_step = |ui: &mut Ui, keys: Vec<Key>, events: Vec<Event>, build: &mut dyn FnMut(&mut Ui)| {
        field_time.set(field_time.get() + 0.5);
        rays_shell::host::frame(ui, &frame_at(keys, scale, field_time.get(), events), build);
    };
    field_step(&mut ui, vec![], vec![], &mut build);
    field_step(&mut ui, vec![], vec![press((40, 30)), release((40, 30))], &mut build);
    let cursor = |ui: &Ui| -> i64 {
        match &layer::text_regions(&rays_shell::host::scene(&ui, None))[..] {
            [(_, _, _, _, true, cursor)] => *cursor,
            regions => fail(&lab(&format!("long field lost its IME region ({})", regions.len()))),
        }
    };
    field_step(&mut ui, vec![Key::Meta], vec![key_down(Key::ArrowLeft)], &mut build);
    if cursor(&ui) > 12 {
        fail(&lab("Command-Left did not reveal the start"));
    }
    field_step(&mut ui, vec![Key::Meta], vec![key_down(Key::ArrowRight)], &mut build);
    if cursor(&ui) < 80 || cursor(&ui) > 100 {
        fail(&lab("Command-Right did not reveal the end"));
    }
    field_step(&mut ui, vec![Key::Meta], vec![key_down(Key::ArrowLeft)], &mut build);
    field_step(&mut ui, vec![], vec![press((21, 30))], &mut build);
    field_step(&mut ui, vec![], vec![move_to((220, 30))], &mut build);
    for _ in 0..30 {
        field_step(&mut ui, vec![], vec![], &mut build);
    }
    field_step(&mut ui, vec![], vec![release((220, 30))], &mut build);
    with_clipboard(&mut || {
        field_step(&mut ui, vec![Key::Meta], vec![key_down(kc('c'))], &mut build);
        if rays::clipboard::get_text() != Ok(long_value.to_owned()) {
            fail(&lab("edge drag did not select through the end"));
        }
    });
    ui.destroy();
}

fn with_scale(scale: f64) {
    reset_clock();
    scale_1(scale);
    scale_2(scale);
    scale_3(scale);
    scale_4(scale);
    scale_5(scale);
    scale_6(scale);
    scale_7(scale);
    scale_8(scale);
    scale_9(scale);
    scale_10(scale);
    scale_11(scale);
}

pub fn window_pxui_scale_1x() {
    with_scale(1.0);
}

pub fn window_pxui_scale_2x() {
    with_scale(2.0);
}

pub fn window_pxui_tooltip_rest() {
    let mut ui = field_ui();
    let key_name = RefCell::new("a".to_owned());
    let skip = Cell::new(false);
    let step = |ui: &mut Ui, key: Option<&str>, skipped: bool, time: f64, events: Vec<Event>| -> (bool, bool, bool) {
        let mut out = (false, false, false);
        key_name.replace(key.unwrap_or("a").to_owned());
        skip.set(skipped);
        ui.frame(&frame_at(vec![], 1.0, time, events), &mut |ui| {
            let parent = ui.box_("hover-parent", BoxOpts::default().flags(CLICKABLE).w(Size::Px(100.0)).h(Size::Px(100.0)));
            let saved = ui.enter(&parent);
            let child = ui.box_("hover-child", BoxOpts::default().flags(CLICKABLE).w(Size::Px(40.0)).h(Size::Px(40.0)));
            ui.leave(saved);
            let k = key_name.borrow().clone();
            let ready = !skip.get() && ui.hovered_within(&parent) && ui.hover_delay(&k);
            if ready {
                ui.tooltip(&k, "A shared tooltip.");
            }
            out = (ready, ui.hovered_within(&parent), ui.signal(&child).hovered);
        });
        out
    };
    step(&mut ui, None, false, 0.0, vec![]);
    let (ready, parent, child) = step(&mut ui, None, false, 0.1, vec![move_to((10, 10))]);
    if ready || !parent || !child {
        fail("hover ancestry did not use the shared hit tree");
    }
    if step(&mut ui, None, false, 0.479, vec![]).0 {
        fail("tooltip appeared before 380 ms");
    }
    if !step(&mut ui, None, false, 0.481, vec![]).0 {
        fail("tooltip did not appear after 380 ms");
    }
    if step(&mut ui, None, false, 0.5, vec![move_to((11, 10))]).0 {
        fail("pointer motion did not restart hover rest");
    }
    if !step(&mut ui, None, false, 0.881, vec![]).0 {
        fail("tooltip did not restart after movement");
    }
    step(&mut ui, None, false, 0.9, vec![Event::FocusLost]);
    if step(&mut ui, None, false, 0.91, vec![move_to((11, 10))]).0 {
        fail("focus loss kept hover rest");
    }
    if step(&mut ui, Some("b"), false, 1.3, vec![]).0 {
        fail("a different target inherited hover time");
    }
    if !step(&mut ui, Some("b"), false, 1.681, vec![]).0 {
        fail("changed target did not settle");
    }
    step(&mut ui, None, true, 1.7, vec![]);
    if step(&mut ui, Some("b"), false, 2.1, vec![]).0 {
        fail("leaving a target retained its timer");
    }
    if step(&mut ui, Some("b"), false, 2.6, vec![press((11, 10))]).0 {
        fail("pointer capture displayed a tooltip");
    }
    step(&mut ui, None, false, 2.7, vec![release((11, 10))]);
    if step(&mut ui, None, false, 1.0, vec![]).0 {
        fail("a backwards clock retained hover time");
    }
    let mut ready = false;
    ui.frame(&frame_at(vec![], 1.0, 1.5, vec![]), &mut |ui| {
        ui.modal(None, "hover-modal", &mut |ui| ui.label("Modal"));
        ready = ui.hover_delay("a");
    });
    if ready {
        fail("a newly opened popup kept a background tooltip");
    }
    ui.destroy();
}

pub fn window_pxui_hover_cursor() {
    // Hover: the topmost control under the pointer, from last frame rects.
    let mut ui = field_ui();
    let hovered = Cell::new(false);
    let mut build = |ui: &mut Ui| {
        ui.panel(PanelOpts::default().at(0.0, 0.0).width(240.0), "panel", &mut |ui| {
            let row = ui.box_("probe", BoxOpts::default().flags(CLICKABLE).w(Size::Grow).h(Size::Px(24.0)));
            hovered.set(ui.signal(&row).hovered);
        });
    };
    ui.frame(&frame_at(vec![], 2.0, 0.0, vec![]), &mut build);
    ui.frame(&frame_at(vec![], 2.0, 0.1, vec![move_to((40, 12))]), &mut build);
    if !hovered.get() || !ui.wants_pointer() {
        fail("hover did not follow the pointer");
    }
    ui.frame(&frame_at(vec![], 2.0, 0.2, vec![move_to((300, 200))]), &mut build);
    if hovered.get() || ui.wants_pointer() {
        fail("hover outlived the pointer");
    }
    // text fields show the I-beam under the pointer, and only there
    let mut field_ui = field_ui();
    let title = RefCell::new(String::new());
    let mut field = |ui: &mut Ui| {
        let t = title.borrow().clone();
        *title.borrow_mut() = ui.text_field("Title", &t);
    };
    field_ui.frame(&frame_at(vec![], 1.0, 0.6, vec![]), &mut field);
    field_ui.frame(&frame_at(vec![], 1.0, 0.7, vec![move_to((150, row(0)))]), &mut field);
    if field_ui.output().platform.cursor != Some(Cursor::Text) {
        fail("a text field did not request the I-beam");
    }
    field_ui.frame(&frame_at(vec![], 1.0, 0.8, vec![move_to((300, 200))]), &mut field);
    if field_ui.output().platform.cursor.is_some() {
        fail("the I-beam outlived hover");
    }
    let mut area_ui = new_ui(Some(11));
    let mut area = |ui: &mut Ui| {
        ui.text_area_submit((0.0, 0.0), 200.0, 100.0, &TextAreaOpts::default(), "area", "hello");
    };
    area_ui.frame(&frame_at(vec![], 1.0, 0.9, vec![]), &mut area);
    area_ui.frame(&frame_at(vec![], 1.0, 1.0, vec![move_to((50, 30))]), &mut area);
    if area_ui.output().platform.cursor != Some(Cursor::Text) {
        fail("a text area did not request the I-beam");
    }
    area_ui.destroy();
    field_ui.destroy();
    ui.destroy();
}

pub fn window_pxui_fit_box() {
    // A box with no extent of its own (Fit, its one child placed with at) shows the child.
    let mut ui = field_ui();
    let placed = |ui: &mut Ui, events: Vec<Event>| -> bool {
        let mut hovered = false;
        ui.frame(&frame_at(vec![], 1.0, 0.0, events), &mut |ui| {
            let group = ui.box_("group", BoxOpts::default().w(Size::Fit).h(Size::Fit));
            let saved = ui.enter(&group);
            let child = ui.box_("placed", BoxOpts::default().flags(CLICKABLE).at((40.0, 40.0)).w(Size::Px(20.0)).h(Size::Px(20.0)));
            ui.leave(saved);
            hovered = ui.signal(&child).hovered;
        });
        hovered
    };
    placed(&mut ui, vec![]);
    if !placed(&mut ui, vec![move_to((50, 50))]) {
        fail("a Fit box of placed children culled them");
    }
    ui.destroy();
}

/// P0 baseline: heap allocations and CPU per warm kit frame, plus draw batches (docs/pxui-baseline.md).
/// Measurement only; it asserts the frame is warm (second half allocates no more than the first frame did).
pub fn window_pxui_baseline() {
    let build = |ui: &mut Ui| {
        ui.panel(PanelOpts::default(), "panel", &mut |ui| {
            ui.label("PXUI");
            ui.toggle("Animate", true);
            ui.text_field("Caption", "Functional UI");
            ui.button("Quit");
        });
    };
    let mut ui = field_ui();
    for i in 0..30 {
        ui.frame(&frame_at(vec![], 2.0, i as f64 / 60.0, vec![]), &mut |ui| build(ui));
    }
    let (n, mut frame_ns) = (200u64, 0u128);
    let frames: Vec<_> = (0..n).map(|i| frame_at(vec![], 2.0, (30 + i) as f64 / 60.0, vec![])).collect();
    mem_sys::local_reset();
    let (c0, b0) = (mem_sys::local_alloc_count(), mem_sys::alloc_bytes_total());
    for f in &frames {
        let t = std::time::Instant::now();
        ui.frame(f, &mut |ui| build(ui));
        frame_ns += t.elapsed().as_nanos();
    }
    let frame_allocs = (mem_sys::local_alloc_count() - c0) as f64 / n as f64;
    let frame_bytes = (mem_sys::alloc_bytes_total() - b0) as f64 / n as f64;
    // lowering side: host::scene wraps the draw layers as scene nodes; stage_native lowers them to batches
    let (c1, b1) = (mem_sys::local_alloc_count(), mem_sys::alloc_bytes_total());
    let t = std::time::Instant::now();
    for _ in 0..n {
        std::hint::black_box(rays_shell::host::scene(&ui, None));
    }
    let scene_ns = t.elapsed().as_nanos();
    let scene_allocs = (mem_sys::local_alloc_count() - c1) as f64 / n as f64;
    let scene_bytes = (mem_sys::alloc_bytes_total() - b1) as f64 / n as f64;
    let mut batches = 0;
    if let Ok(staged) = layer::stage_native(320, 240, &rays_shell::host::scene(&ui, None)) {
        for l in &staged.layers {
            if let layer::NativeLayer::UiLayer(batch, _) = l {
                batches += batch.batches().len();
            }
        }
    }
    eprintln!("PXUI_BASELINE draw batches (kit panel, 1 layer): {batches}");
    eprintln!(
        "PXUI_BASELINE frame: {frame_allocs:.1} allocs {frame_bytes:.0} B {:.1} us | scene(): {scene_allocs:.1} allocs {scene_bytes:.0} B {:.1} us",
        frame_ns as f64 / n as f64 / 1e3,
        scene_ns as f64 / n as f64 / 1e3
    );
    ui.destroy();
}

pub fn window_pxui_batches() {
    // One panel paints in a handful of batches, not one draw per label.
    let mut ui = field_ui();
    ui.frame(&frame_at(vec![], 2.0, 0.0, vec![]), &mut |ui| {
        ui.panel(PanelOpts::default(), "panel", &mut |ui| {
            ui.label("PXUI");
            ui.toggle("Animate", true);
            ui.text_field("Caption", "Functional UI");
            ui.button("Quit");
        });
    });
    match layer::stage_native(320, 240, &rays_shell::host::scene(&ui, None)) {
        Ok(staged) => {
            let mut batches = 0;
            for layer in &staged.layers {
                if let layer::NativeLayer::UiLayer(batch, _) = layer {
                    batches += batch.batches().len();
                }
            }
            if batches == 0 || batches > 4 {
                fail(&format!("panel painted in {} batches", batches));
            }
        }
        Err(message) => fail(&message),
    }
    ui.destroy();
}

pub fn window_pxui_full_width_retained_state() {
    let mut ui = field_ui();
    for value in [i64::MIN, -(1i64 << 62), 0, i64::MAX] {
        ui.frame(&frame_at(vec![], 1.0, 0.0, vec![]), &mut |ui| {
            let b = ui.box_("full-width-state", BoxOpts::default());
            ui.set_state(&b, value);
            assert_eq!(ui.state(&b, 99), value);
        });
        ui.frame(&frame_at(vec![], 1.0, 0.1, vec![]), &mut |ui| {
            let b = ui.box_("full-width-state", BoxOpts::default());
            assert_eq!(ui.state(&b, 99), value);
        });
    }
    ui.destroy();
}

pub const CASES: &[(&str, fn())] = &[
    ("window_pxui_full_width_retained_state", window_pxui_full_width_retained_state),
    ("window_pxui_scale_1x", window_pxui_scale_1x),
    ("window_pxui_scale_2x", window_pxui_scale_2x),
    ("window_pxui_tooltip_rest", window_pxui_tooltip_rest),
    ("window_pxui_hover_cursor", window_pxui_hover_cursor),
    ("window_pxui_fit_box", window_pxui_fit_box),
    ("window_pxui_batches", window_pxui_batches),
    ("window_pxui_baseline", window_pxui_baseline),
];
