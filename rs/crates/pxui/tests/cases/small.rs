//! Ports of lib/pxui test_table.ml, test_keyboard.ml and test_input.ml.
use crate::support::*;
use input::{Event, Key, MouseButton, ScrollPhase};
use pxui::ui::*;
use rays::frame;
use std::cell::Cell;
use std::rc::Rc;

// ------------------------------------------------------------------ test_table

fn table_frame(count: i64, events: Vec<Event>) -> frame::t {
    let mut f = frame_at(Vec::new(), 1.0, count as f64 / 60.0, events);
    f.input.size = (360, 240);
    f
}

pub fn window_pxui_table() {
    let mut ui = new_ui(None);
    let cells = Rc::new(Cell::new(0i64));
    let first = Rc::new(Cell::new(i64::MAX));
    let clicked = Rc::new(Cell::new(None::<i64>));
    let mut build = {
        let (cells, first, clicked) = (cells.clone(), first.clone(), clicked.clone());
        move |ui: &mut Ui| {
            let (cells, first) = (cells.clone(), first.clone());
            let headers = vec!["index".to_owned(), "value".to_owned()];
            let cell = Rc::new(move |row: i64, column: i64| {
                cells.set(cells.get() + 1);
                first.set(first.get().min(row));
                (row + column).to_string()
            });
            let (c, _) = ui.table((0.0, 0.0), 300.0, 168.0, &headers, 1_000_000, cell, "table");
            clicked.set(c);
        }
    };
    ui.frame(&table_frame(0, vec![]), &mut build);
    cells.set(0);
    ui.frame(&table_frame(1, vec![]), &mut build);
    assert!(cells.get() > 0 && cells.get() <= 20);
    ui.frame(&table_frame(2, vec![press((30, 36)), release((30, 36))]), &mut build);
    assert!(clicked.get() == Some(0));
    cells.set(0);
    first.set(i64::MAX);
    ui.frame(&table_frame(3, vec![move_to((30, 100)), Event::Scrolled((0.0, -10000.0))]), &mut build);
    assert!(cells.get() >= 12 && cells.get() <= 20 && first.get() == 10000);
    cells.set(0);
    ui.frame(&table_frame(4, vec![]), &mut build);
    check(
        cells.get() >= 12 && cells.get() <= 20 && first.get() == 10000,
        &format!("scrolled table cells={} first={}", cells.get(), first.get()),
    );
    ui.frame(&table_frame(5, vec![press((30, 36)), release((30, 36))]), &mut build);
    assert!(clicked.get() == Some(10000));
    for count in 6..=15 {
        cells.set(0);
        first.set(i64::MAX);
        ui.frame(&table_frame(count, vec![move_to((30, 100)), Event::Scrolled((0.0, -3.0))]), &mut build);
        assert!(cells.get() >= 12 && cells.get() <= 20 && first.get() == 10000 + (count - 5) * 3);
    }
    cells.set(0);
    first.set(i64::MAX);
    ui.frame(
        &table_frame(
            16,
            vec![
                Event::Trackpad_scrolled { delta: (0.0, -72.0), phase: ScrollPhase::Moved, time: 16.0 / 60.0 },
                Event::Scrolled((0.0, -3.0)),
            ],
        ),
        &mut build,
    );
    assert!(cells.get() >= 12 && cells.get() <= 20 && first.get() == 10033);
    ui.frame(
        &table_frame(17, vec![Event::Trackpad_scrolled { delta: (0.0, -72.0), phase: ScrollPhase::Moved, time: 17.0 / 60.0 }]),
        &mut build,
    );
    ui.frame(
        &table_frame(18, vec![Event::Trackpad_scrolled { delta: (0.0, 0.0), phase: ScrollPhase::Lifted, time: 18.0 / 60.0 }]),
        &mut build,
    );
    cells.set(0);
    first.set(i64::MAX);
    ui.frame(&table_frame(19, vec![]), &mut build);
    assert!(cells.get() >= 12 && cells.get() <= 20 && first.get() > 10036);
    ui.destroy();
}

// ------------------------------------------------------------------ test_keyboard

fn keyboard_frame(keys: Vec<Key>, events: Vec<Event>) -> frame::t {
    let mut input = input::at_time(1.0);
    input.frame = 0;
    input.tick = 0;
    input.size = (500, 360);
    input.pointer = (-10.0, -10.0);
    input.keys = keys;
    input.events = events;
    frame::t { input, pixel_scale: (1.0, 1.0), fps: 60.0, mouse_delta: (0.0, 0.0) }
}

#[derive(Clone)]
struct Model {
    clicks: i64,
    enabled: bool,
    text: String,
}

fn controls(ui: &mut Ui, model: &mut Model) {
    let m = model.clone();
    let mut out = m.clone();
    ui.panel(PanelOpts::default().width(400.0), "controls", &mut |ui| {
        out.clicks = m.clicks + if ui.button("Run") { 1 } else { 0 };
        out.enabled = ui.toggle("Enabled", m.enabled);
        out.text = ui.text_field("Text", &m.text);
    });
    *model = out;
}

pub fn window_pxui_keyboard() {
    let mut ui = new_ui(Some(11));
    let mut model = Model { clicks: 0, enabled: false, text: "old".to_owned() };
    fn step(ui: &mut Ui, model: &mut Model, keys: Vec<Key>, events: Vec<Event>) {
        ui.frame(&keyboard_frame(keys, events), &mut |ui| controls(ui, model));
    }
    step(&mut ui, &mut model, vec![], vec![]);
    step(
        &mut ui,
        &mut model,
        vec![],
        vec![key_down(Key::Shift), key_down(Key::Tab), Event::Key_released(Key::Shift), Event::Text_input("!".into())],
    );
    check(model.text == "old!", "same-frame Shift release changed backwards Tab traversal");
    step(&mut ui, &mut model, vec![], vec![key_down(Key::Tab), key_down(Key::Enter)]);
    check(
        model.clicks == 1 && ui.input(None).input.events.is_empty(),
        "Tab/Enter did not activate the first control or leaked into the host",
    );
    step(&mut ui, &mut model, vec![], vec![key_down(Key::Tab), key_down(Key::Space)]);
    check(model.enabled, "Space did not toggle the focused control");
    // Rebuilding fewer controls prunes focus and keeps hidden controls inert.
    ui.frame(&keyboard_frame(vec![], vec![]), &mut |ui| ui.panel(PanelOpts::default().width(400.0), "controls", &mut |_| {}));
    check(!ui.text_input_focused(), "disappearing focused controls kept focus");
    step(&mut ui, &mut model, vec![], vec![key_down(Key::Space)]);
    check(model.enabled, "a control that had disappeared kept its keyboard activation");
    step(&mut ui, &mut model, vec![Key::Shift], vec![key_down(Key::Tab), Event::Text_input("!".into())]);
    check(model.text == "old!!", "Shift-Tab did not enter the last text field");
    step(
        &mut ui,
        &mut model,
        vec![],
        vec![key_down(Key::Meta), key_down(kc('a')), Event::Key_released(Key::Meta), Event::Text_input("new".into())],
    );
    check(model.text == "new", "same-frame Command release broke text select-all");
    step(
        &mut ui,
        &mut model,
        vec![],
        vec![key_down(Key::Shift), key_down(Key::ArrowLeft), Event::Key_released(Key::Shift), key_down(Key::Backspace)],
    );
    check(model.text == "ne", "same-frame Shift release broke text selection");
    step(&mut ui, &mut model, vec![], vec![key_down(Key::Tab), key_down(Key::Enter)]);
    check(model.clicks == 2, "Tab did not wrap to the first control");
    step(&mut ui, &mut model, vec![], vec![key_down(Key::Escape)]);
    check(!ui.text_input_focused(), "Escape did not return control keys to the host");
    step(&mut ui, &mut model, vec![], vec![key_down(Key::Tab)]);
    step(&mut ui, &mut model, vec![], vec![Event::Focus_lost, key_down(Key::Enter)]);
    check(model.clicks == 2 && !ui.text_input_focused(), "focus loss did not cancel keyboard activation");
    let mut menu = MenuResult::Open;
    let mut background = false;
    let mut popup = |ui: &mut Ui| {
        menu = ui.context_menu(
            (100.0, 100.0),
            &MenuOpts::default(),
            "menu",
            &[("Disabled".to_owned(), false), ("Enabled".to_owned(), true)],
        );
        ui.panel(PanelOpts::default(), "background", &mut |ui| background = ui.button("Background"));
    };
    ui.frame(&keyboard_frame(vec![], vec![]), &mut popup);
    ui.frame(&keyboard_frame(vec![], vec![key_down(Key::Tab), key_down(Key::Enter)]), &mut popup);
    check(menu == MenuResult::Pick(1) && !background, "modal traversal reached a disabled row or escaped to the background");
    ui.frame(&keyboard_frame(vec![], vec![Event::Focus_lost]), &mut |_| {});
    let picked = std::cell::RefCell::new(None);
    let mut picker = |ui: &mut Ui| {
        ui.modal(None, "picker", &mut |ui| {
            let rows = |_: &str| vec![("One".to_owned(), String::new()), ("Two".to_owned(), String::new())];
            *picked.borrow_mut() = Some(ui.picker(&PickerOpts::default(), "Search", "", &rows));
        });
    };
    ui.frame(&keyboard_frame(vec![], vec![]), &mut picker);
    *picked.borrow_mut() = None;
    ui.frame(&keyboard_frame(vec![], vec![key_down(Key::Tab), key_down(Key::Enter)]), &mut picker);
    check(*picked.borrow() == Some((String::new(), Pick::Pick(0))), "picker stole focus from its keyboard-activated row");
    ui.destroy();
}

// ------------------------------------------------------------------ test_input

fn input_frame(buttons: Vec<MouseButton>, keys: Vec<Key>, events: Vec<Event>) -> frame::t {
    let mut input = input::at_time(0.0);
    input.frame = 0;
    input.tick = 0;
    input.size = (320, 240);
    input.pointer = (10.0, 10.0);
    input.keys = keys;
    input.buttons = buttons;
    input.events = events;
    frame::t { input, pixel_scale: (1.0, 1.0), fps: 60.0, mouse_delta: (0.0, 0.0) }
}

fn plain(events: Vec<Event>) -> frame::t {
    input_frame(vec![], vec![], events)
}

pub fn window_pxui_input() {
    let mut ui = new_ui(Some(11));
    let body = |ui: &mut Ui| -> (i64, Signal, Signal) {
        let view = ui.box_("view", BoxOpts::default().flags(CLICKABLE).at((0.0, 0.0)).w(Size::Px(160.0)).h(Size::Px(240.0)));
        let saved = ui.enter(&view);
        let control = ui.box_("control", BoxOpts::default().flags(CLICKABLE + FOCUSABLE).at((30.0, 30.0)).w(Size::Px(50.0)).h(Size::Px(50.0)));
        ui.leave(saved);
        let graph = ui.box_("graph", BoxOpts::default().flags(CLICKABLE + SCROLL).at((160.0, 0.0)).w(Size::Px(160.0)).h(Size::Px(240.0)));
        (key_of_box(&view), ui.signal(&control), ui.signal(&graph))
    };
    let step = |ui: &mut Ui, popup: bool, f: frame::t| -> (frame::t, Signal, Signal) {
        let mut out = None;
        ui.frame(&f, &mut |ui| {
            if popup {
                ui.modal(None, "modal", &mut |ui| ui.label("Popup"));
            }
            out = Some(body(ui));
        });
        let (owner, control, graph) = out.unwrap();
        (ui.input(Some(owner)), control, graph)
    };
    step(&mut ui, false, plain(vec![]));
    let drag = vec![
        Event::Pointer_pressed(MouseButton::RightButton, (10.0, 10.0)),
        Event::Pointer_moved((220.0, 20.0)),
        Event::Pointer_released(MouseButton::RightButton, (220.0, 20.0)),
    ];
    let (inp, _, graph) = step(&mut ui, false, plain(drag.clone()));
    check(inp.input.events == drag && !graph.pressed, "viewport capture did not survive a pane crossing/release");
    let drag = vec![
        Event::Pointer_pressed(MouseButton::MiddleButton, (200.0, 10.0)),
        Event::Pointer_moved((10.0, 10.0)),
        Event::Pointer_released(MouseButton::MiddleButton, (10.0, 10.0)),
    ];
    let (inp, _, graph) = step(&mut ui, false, plain(drag));
    check(
        inp.input.events == vec![Event::Pointer_cancelled(MouseButton::MiddleButton)] && graph.pressed,
        "graph-owned drag reached the viewport",
    );
    let click = vec![press((40, 40)), release((40, 40)), key_down(Key::Enter)];
    let (inp, control, _) = step(&mut ui, false, input_frame(vec![], vec![Key::Shift], click.clone()));
    check(
        control.clicked && inp.input.events == vec![Event::Pointer_cancelled(MouseButton::LeftButton)] && inp.input.keys.is_empty(),
        "child widget input escaped to its viewport root",
    );
    let mut events = click.clone();
    events.push(Event::Scrolled((0.0, 1.0)));
    let (inp, control, _) = step(&mut ui, true, plain(events));
    check(
        !control.clicked
            && inp.input.buttons.is_empty()
            && inp.mouse_delta == (0.0, 0.0)
            && inp.input.events == vec![Event::Pointer_cancelled(MouseButton::LeftButton)],
        "new popup did not shield this frame's underlying controls",
    );
    let mut with_escape = vec![key_down(Key::Escape)];
    with_escape.extend(click.clone());
    let (inp, control, _) = step(&mut ui, true, plain(with_escape));
    check(
        !control.clicked && inp.input.events == vec![Event::Pointer_cancelled(MouseButton::LeftButton)],
        "popup dismissal leaked its remaining press/release events",
    );
    step(&mut ui, false, plain(vec![]));
    step(
        &mut ui,
        false,
        input_frame(vec![MouseButton::RightButton], vec![], vec![Event::Pointer_pressed(MouseButton::RightButton, (10.0, 10.0))]),
    );
    let (inp, _, _) = step(&mut ui, true, input_frame(vec![MouseButton::RightButton], vec![], vec![]));
    check(
        inp.input.events.contains(&Event::Pointer_cancelled(MouseButton::RightButton)),
        "popup opening did not cancel the viewport's captured gesture",
    );
    step(&mut ui, false, plain(vec![Event::Focus_lost]));
    let (inp, _, _) = step(&mut ui, false, plain(vec![Event::Focus_lost, Event::Pointer_cancelled(MouseButton::RightButton)]));
    check(
        inp.input.events == vec![Event::Focus_lost, Event::Pointer_cancelled(MouseButton::RightButton)],
        "cancellation events were consumed",
    );
    let mut only_view = |ui: &mut Ui| -> i64 {
        let b = ui.box_("only-view", BoxOpts::default().flags(CLICKABLE).at((0.0, 0.0)).w(Size::Px(160.0)).h(Size::Px(240.0)));
        key_of_box(&b)
    };
    ui.frame(&plain(vec![]), &mut |ui| {
        only_view(ui);
    });
    let unowned_click = vec![press((200, 10)), release((200, 10))];
    let mut owner = 0;
    ui.frame(&plain(unowned_click.clone()), &mut |ui| owner = only_view(ui));
    check(
        ui.input(Some(owner)).input.events == vec![Event::Pointer_cancelled(MouseButton::LeftButton)],
        "unowned pointer events escaped to an explicit viewport owner",
    );
    check(ui.input(None).input.events == unowned_click, "default input lost unowned pointer events");
    let modal_y = |ui: &mut Ui, label: &str| -> f64 {
        let mut row = None;
        ui.frame(&plain(vec![]), &mut |ui| {
            ui.modal(None, label, &mut |ui| row = Some(ui.box_("row", BoxOpts::default().h(Size::Px(24.0)))));
        });
        ui.rect(&row.expect("modal open")).1
    };
    modal_y(&mut ui, "height-original");
    let centred = modal_y(&mut ui, "height-original");
    ui.frame(&plain(vec![]), &mut |_| {});
    ui.frame(&plain(vec![]), &mut |_| {});
    check(modal_y(&mut ui, "height-original") == centred, "closed modal lost its remembered height");
    for index in 0..64 {
        let label = format!("height-{}", index);
        modal_y(&mut ui, &label);
        modal_y(&mut ui, &label);
    }
    check(modal_y(&mut ui, "height-original") != centred, "dynamic modal keys did not evict old cached heights");
    let probe = |ui: &mut Ui| -> Vec<Key> {
        let b = ui.box_("press-keys", BoxOpts::default().flags(CLICKABLE).w(Size::Px(100.0)).h(Size::Px(100.0)));
        ui.press_keys(&b)
    };
    let mut seen = Vec::new();
    ui.frame(&plain(vec![]), &mut |ui| seen = probe(ui));
    let mut pressed = Vec::new();
    ui.frame(
        &input_frame(vec![MouseButton::LeftButton], vec![Key::Shift], vec![key_down(Key::Shift), press((10, 10))]),
        &mut |ui| pressed = probe(ui),
    );
    let mut held = Vec::new();
    ui.frame(
        &input_frame(vec![MouseButton::LeftButton], vec![], vec![Event::Key_released(Key::Shift), move_to((20, 10))]),
        &mut |ui| held = probe(ui),
    );
    let mut released = Vec::new();
    ui.frame(&plain(vec![release((20, 10))]), &mut |ui| released = probe(ui));
    check(pressed == vec![Key::Shift] && held == pressed && released == pressed, "captured press keys changed during motion or release");
    // capture_unclaimed: a blocking (non-clickable) child under the ancestor takes the press away
    // from it only where the child is clickable
    let unclaimed = |ui: &mut Ui| -> (Signal, Signal) {
        let outer = ui.box_("outer", BoxOpts::default().flags(CAPTURE_UNCLAIMED + BLOCKING).at((0.0, 0.0)).w(Size::Px(200.0)).h(Size::Px(200.0)));
        let saved = ui.enter(&outer);
        let taker = ui.box_("taker", BoxOpts::default().flags(CLICKABLE).at((100.0, 0.0)).w(Size::Px(50.0)).h(Size::Px(50.0)));
        ui.leave(saved);
        let saved = ui.enter(&outer);
        ui.box_("plain", BoxOpts::default().flags(SCROLL).at((0.0, 100.0)).w(Size::Px(50.0)).h(Size::Px(50.0)));
        ui.leave(saved);
        (ui.signal(&outer), ui.signal(&taker))
    };
    ui.frame(&plain(vec![]), &mut |ui| {
        unclaimed(ui);
    });
    let mut r = None;
    ui.frame(
        &input_frame(vec![MouseButton::LeftButton], vec![], vec![press((110, 10)), move_to((120, 10))]),
        &mut |ui| r = Some(unclaimed(ui)),
    );
    let (outer, taker) = r.take().unwrap();
    check(taker.pressed && !outer.pressed && !outer.held, "ancestor captured a consumed press");
    ui.frame(&plain(vec![release((120, 10))]), &mut |ui| {
        unclaimed(ui);
    });
    ui.frame(
        &input_frame(vec![MouseButton::LeftButton], vec![], vec![press((10, 110)), move_to((30, 90))]),
        &mut |ui| r = Some(unclaimed(ui)),
    );
    let (outer, _) = r.take().unwrap();
    check(
        outer.pressed && outer.held && outer.dragging && outer.drag == (20.0, -20.0) && outer.press_point == (10.0, 110.0),
        "ancestor did not capture an unclaimed press",
    );
    ui.frame(&plain(vec![release((60, 90))]), &mut |ui| r = Some(unclaimed(ui)));
    let (outer, _) = r.take().unwrap();
    check(outer.released && !outer.held && outer.release_point == (60.0, 90.0), "unclaimed capture did not release");
    let _ = seen;
    ui.destroy();
}

fn key_of_box(b: &UiBox) -> i64 {
    key(b)
}

pub const CASES: &[(&str, fn())] = &[
    ("window_pxui_table", window_pxui_table),
    ("window_pxui_keyboard", window_pxui_keyboard),
    ("window_pxui_input", window_pxui_input),
];
