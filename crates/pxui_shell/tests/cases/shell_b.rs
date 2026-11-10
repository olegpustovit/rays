//! Ports of lib/pxui_shell/test_shell.ml, part two: the inspector rows and the tree list.
use crate::shell_a::{base, fr, new_ui};
use input::{Event, Key, MouseButton};
use param::{FieldView, FloatRange, Impact, KindView, Value};
use pxui::ui::*;
use pxui_shell::inspector::{self, FlowChange, FlowOpts, FlowRow};
use pxui_shell::tree::{self as T, Command, DropAt, Intent, Row};
use rays_math::color;

fn left_press(p: (f64, f64)) -> Event {
    Event::Pointer_pressed(MouseButton::LeftButton, p)
}

fn left_release(p: (f64, f64)) -> Event {
    Event::Pointer_released(MouseButton::LeftButton, p)
}

// ----------------------------------------------------------------------- inspector

pub fn window_shell_inspector_record() {
    // Inspector.record: a plain sketch record edited through its schema.
    let field = param::field("on".to_string(), param::toggle(), Value::Bool(false)).and_then(|f| f.label("On".to_string())).expect("field");
    let schema = param::schema("look".to_string(), vec![field]).expect("schema");
    let mut ui = new_ui();
    let click = vec![left_press((115.0, 15.0)), left_release((115.0, 15.0))];
    let record = |ui: &mut Ui, events: Vec<Event>, value: bool| -> Result<(Vec<Value>, param::Effects), String> {
        let mut out = Err("not built".to_string());
        ui.frame(&fr((400, 300), (0.0, 0.0), events), &mut |ui| {
            ui.panel(PanelOpts::default().at(0.0, 0.0).width(260.0), "record", &mut |ui| {
                out = inspector::record(ui, &schema, &[Value::Bool(value)]);
            });
        });
        out
    };
    match record(&mut ui, vec![], false) {
        Ok((values, effects)) if values == vec![Value::Bool(false)] && !param::has_effects(effects) => {}
        other => panic!("idle inspector frame changed the record: {other:?}"),
    }
    match record(&mut ui, click, false) {
        Ok((values, effects)) if values == vec![Value::Bool(true)] && effects.cook => {}
        other => panic!("inspector toggle did not edit the record: {other:?}"),
    }
    ui.destroy();
}

pub fn window_shell_inspector_folder_order() {
    use std::cell::RefCell;
    let row = |name: &str, folder: Option<&str>| {
        let mut field = cell(name, 0);
        field.vec3 = None;
        field.folder = folder.into_iter().map(str::to_owned).collect();
        field.kind = KindView::Choice(vec!["a".into(), "b".into()]);
        field.current = Value::Choice("a".into());
        field.default = field.current.clone();
        FlowRow { path: name.into(), fields: vec![field], shown: false, locked: true, drive: None, live: None }
    };
    let rows = [row("s1", Some("Size")), row("own1", None), row("root_named", Some("Root")), row("s2", Some("Size")), row("own2", None), row("tail", Some("Other"))];
    for (kind, expected) in [(Some("Root"), vec!["own1", "own2", "root_named", "s1", "s2", "tail"]), (None, vec!["own1", "own2", "s1", "s2", "root_named", "tail"])] {
        let mut ui = new_ui();
        let seen = RefCell::new(Vec::new());
        let opts = FlowOpts { kind_label: kind.map(str::to_owned), expanded: vec!["Root".into(), "Size".into(), "Other".into()], ..Default::default() };
        ui.frame(&fr((400, 600), (0., 0.), vec![]), &mut |ui| {
            ui.panel(PanelOpts::default().at(0., 0.).width(260.), "folders", &mut |ui| {
                let changes = inspector::flow_fields(ui, &opts, Some(&|name, _| seen.borrow_mut().push(name.to_owned())), &rows);
                assert!(changes.is_empty());
            });
        });
        assert_eq!(*seen.borrow(), expected, "one-level folders changed row/merge order");
        ui.destroy();
    }
}

fn cell(name: &str, index: i64) -> FieldView {
    FieldView {
        name: name.to_string(),
        label: name.to_string(),
        description: None,
        folder: vec![],
        impact: Impact::Cook,
        primary: false,
        unit: None,
        vec3: Some(("translate".to_string(), index)),
        kind: KindView::Floating(FloatRange { soft_min: -10.0, soft_max: 10.0, hard_min: None, hard_max: None }),
        default: Value::Float(0.0),
        current: Value::Float(1.0),
    }
}

fn build_rows(ui: &mut Ui, mouse: (f64, f64), events: Vec<Event>, label: &str, rows: &[FlowRow]) -> Vec<FlowChange> {
    let mut out = Vec::new();
    ui.frame(&fr((400, 300), mouse, events), &mut |ui| {
        ui.panel(PanelOpts::default().at(0.0, 0.0).width(260.0), label, &mut |ui| {
            out = inspector::flow_fields(ui, &FlowOpts::default(), None, rows);
        });
    });
    out
}

/// A click without a drag, or a drag of `by` points from `at`.
fn gesture(ui: &mut Ui, label: &str, rows: &[FlowRow], at: (f64, f64), by: Option<f64>) -> Vec<FlowChange> {
    match by {
        None => build_rows(ui, at, vec![left_release(at)], label, rows),
        Some(by) => {
            let to = (at.0 + by, at.1);
            let mut found = build_rows(ui, to, vec![Event::Pointer_moved(to)], label, rows);
            found.extend(build_rows(ui, to, vec![left_release(to)], label, rows));
            found
        }
    }
}

pub fn window_shell_vector_cells() {
    // a vector's cell has no track: a click without a drag writes nothing, a drag changes the value
    // from what it was
    let vector = vec![FlowRow {
        path: "translate".to_string(),
        fields: vec![cell("translate_x", 0), cell("translate_y", 1), cell("translate_z", 2)],
        shown: false,
        locked: false,
        drive: None,
        live: None,
    }];
    // one press at each place per UI: a second one there would be a double-click, which types
    let pass = |by: Option<f64>| -> Vec<bool> {
        let mut ui = new_ui();
        build_rows(&mut ui, (0.0, 0.0), vec![], "vector", &vector);
        let mut found = Vec::new();
        for x in [90, 110, 130, 150, 170, 190, 210, 230] {
            let at = (x as f64, 12.0);
            build_rows(&mut ui, at, vec![Event::Pointer_moved(at)], "vector", &vector);
            let mut changes = build_rows(&mut ui, at, vec![left_press(at)], "vector", &vector);
            changes.extend(gesture(&mut ui, "vector", &vector, at, by));
            build_rows(&mut ui, (0.0, 0.0), vec![], "vector", &vector);
            found.push(!changes.is_empty());
        }
        ui.destroy();
        found
    };
    assert!(!pass(None).contains(&true), "a click on a vector's cell wrote a value");
    assert!(pass(Some(40.0)).contains(&true), "no drag on a vector's cell changed it");
}

fn ranged_float(drags: bool) {
    // A ranged float is the same field (Kit.number): a click writes nothing, a drag moves the value
    // from what it was by a fraction, and a long value keeps its digits (no %.6g)
    let scalar = |value: f64| -> Vec<FlowRow> {
        let mut field = cell("radius", 0);
        field.vec3 = None;
        field.current = Value::Float(value);
        vec![FlowRow { path: "radius".to_string(), fields: vec![field], shown: false, locked: false, drive: None, live: None }]
    };
    let edits = |value: f64, by: Option<f64>| -> Vec<FlowChange> {
        let rows = scalar(value);
        let mut ui = new_ui();
        build_rows(&mut ui, (0.0, 0.0), vec![], "scalar", &rows);
        let at = (180.0, 12.0);
        build_rows(&mut ui, at, vec![Event::Pointer_moved(at)], "scalar", &rows);
        let mut found = build_rows(&mut ui, at, vec![left_press(at)], "scalar", &rows);
        found.extend(gesture(&mut ui, "scalar", &rows, at, by));
        ui.destroy();
        found
    };
    let floats = |changes: Vec<FlowChange>| -> Vec<f64> {
        changes
            .into_iter()
            .filter_map(|c| match c {
                FlowChange::Edited(_, Value::Float(v)) => Some(v),
                _ => None,
            })
            .collect()
    };
    assert!(edits(1.0, None).is_empty(), "a click on a ranged number wrote a value");
    if !drags {
        return;
    }
    match floats(edits(1.0, Some(5.0))).first() {
        Some(v) if v.fract() != 0.0 && (v - 1.5).abs() < 1e-9 => {}
        other => panic!("a drag on a float written 1 did not reach 1.5: {other:?}"),
    }
    match floats(edits(1234567.89, Some(-2.0))).first() {
        Some(v) if (v - 1234567.69).abs() < 1e-6 => {}
        other => panic!("a scrub of 1234567.89 lost its digits: {other:?}"),
    }
}

// ----------------------------------------------------------------------- tree

fn row(id: i64, depth: i64, label: &str) -> Row {
    Row { id, depth, label: label.to_string(), detail: String::new(), badge: ("S".to_string(), color::gray(90)), link: false, ghost: false, flags: vec![true] }
}

fn center(k: i64) -> (f64, f64) {
    (200.0, 24.0 + (24.0 * k as f64) + 12.0)
}

fn step(ui: &mut Ui, mut tree: T::Tree, mouse: (f64, f64), events: Vec<Event>, rows: &[Row]) -> (T::Tree, Vec<Intent>) {
    let f = fr((400, 300), mouse, events);
    let prefix = Intent::Delete(vec![i64::MIN]);
    let mut intents = vec![prefix.clone()];
    ui.frame(&f, &mut |ui| {
        T::update(&mut tree, ui, &f, (0, 0, 400, 300), "", &["vis".to_string()], rows, &[], &mut intents);
    });
    assert_eq!(intents.remove(0), prefix, "tree update replaced earlier intents");
    (tree, intents)
}

fn tree_gesture(ui: &mut Ui, t: T::Tree, from: (f64, f64), to: (f64, f64), rows: &[Row]) -> (T::Tree, Vec<Intent>) {
    let (t, _) = step(ui, t, from, vec![], rows);
    let (t, pressed) = step(ui, t, from, vec![left_press(from)], rows);
    let (t, moved) = step(ui, t, to, vec![Event::Pointer_moved(to)], rows);
    let (t, released) = step(ui, t, to, vec![left_release(to)], rows);
    let mut all = pressed;
    all.extend(moved);
    all.extend(released);
    (t, all)
}

fn expect(name: &str, got: Vec<Intent>, want: Vec<Intent>) {
    assert_eq!(got, want, "tree: {name}");
}

pub fn window_shell_tree_pointer_gestures() {
    // Tree: pointer gestures (dead zone, before/inside/after drops, toggle paint, double-click).
    let rows = vec![row(1, 0, "a"), row(2, 1, "b"), row(3, 1, "c"), row(4, 0, "d")];
    let mut ui = new_ui();
    let run = |t: T::Tree, c: Command| { let mut r8_tree = t; let mut r8_intents = Vec::new(); T::run_command(&mut r8_tree, &rows, &[], c, &mut r8_intents); (r8_tree, r8_intents) };
    let moves = |intents: Vec<Intent>| -> Vec<Intent> { intents.into_iter().filter(|i| matches!(i, Intent::Move { .. })).collect() };
    let chevron = (13.0, center(0).1);
    let (t, _) = step(&mut ui, T::create(), (0.0, 0.0), vec![], &rows);
    let (t, intents) = step(&mut ui, t, chevron, vec![left_press(chevron), left_release(chevron)], &rows);
    expect("chevron click does not select or drag", intents, vec![]);
    let (t, _) = run(t, Command::Down);
    let (_, intents) = run(t, Command::Down);
    expect("chevron click folds children", intents, vec![Intent::Select(vec![4])]);
    let (x, y) = center(0);
    let (t, intents) = tree_gesture(&mut ui, T::create(), (x, y), (x + 2.0, y + 1.0), &rows);
    expect("a 2-point drag stays inside the dead zone", moves(intents), vec![]);
    let drop = |k: i64, fraction: f64| {
        let (x, y) = center(k);
        (x, y - 12.0 + (24.0 * fraction))
    };
    let (t, intents) = tree_gesture(&mut ui, t, center(1), drop(3, 0.5), &rows);
    expect("the middle of a row drops inside", moves(intents), vec![Intent::Move { ids: vec![2], target: 4, drop: DropAt::Inside }]);
    let (t, intents) = tree_gesture(&mut ui, t, center(1), drop(3, 0.1), &rows);
    expect("the top quarter drops before", moves(intents), vec![Intent::Move { ids: vec![2], target: 4, drop: DropAt::Before }]);
    let (t, intents) = tree_gesture(&mut ui, t, center(1), drop(3, 0.9), &rows);
    expect("the bottom quarter drops after", moves(intents), vec![Intent::Move { ids: vec![2], target: 4, drop: DropAt::After }]);
    let flag = |k: i64| (386.0, center(k).1);
    let (t, intents) = tree_gesture(&mut ui, t, flag(0), flag(2), &rows);
    expect(
        "dragging down a toggle column paints each row it passes",
        intents.into_iter().filter(|i| matches!(i, Intent::Flag { .. })).collect(),
        vec![Intent::Flag { ids: vec![1], column: 0, value: false }, Intent::Flag { ids: vec![3], column: 0, value: false }],
    );
    let click = vec![left_press(center(3)), left_release(center(3))];
    let (t, _) = step(&mut ui, t, center(3), click.clone(), &rows);
    let (_, intents) = step(&mut ui, t, center(3), click, &rows);
    expect(
        "a double-click activates the row",
        intents.into_iter().filter(|i| matches!(i, Intent::Activate(_))).collect(),
        vec![Intent::Activate(4)],
    );
    ui.destroy();
}

pub fn window_shell_tree_shift_select() {
    let rows = vec![row(1, 0, "a"), row(2, 1, "b"), row(3, 1, "c"), row(4, 0, "d")];
    let mut ui = new_ui();
    let (t, _) = step(&mut ui, T::create(), (0.0, 0.0), vec![], &rows);
    let (t, _) = step(&mut ui, t, center(0), vec![left_press(center(0)), left_release(center(0))], &rows);
    let (_, intents) = step(
        &mut ui,
        t,
        center(2),
        vec![Event::Key_pressed(Key::Shift), left_press(center(2)), Event::Key_released(Key::Shift), left_release(center(2))],
        &rows,
    );
    expect("Shift selection uses the captured press keys", intents, vec![Intent::Select(vec![3, 1, 2])]);
    ui.destroy();
}

pub fn window_shell_tree_scroll_and_sticky() {
    let scrolled = |rows: &[Row], at: (f64, f64)| -> Vec<Intent> {
        let mut ui = new_ui();
        let mut t = T::create();
        let run = |ui: &mut Ui, t: T::Tree, pointer: (f64, f64), events: Vec<Event>| -> (T::Tree, Vec<Intent>) {
            let f = fr((400, 300), pointer, events);
            let mut taken = Some(t);
            let mut out = None;
            ui.frame(&f, &mut |ui| {
                out = Some({ let mut r8_tree = taken.take().unwrap(); let mut r8_intents = Vec::new(); T::update(&mut r8_tree, ui, &base(), (0, 0, 400, 96), "", &[], rows, &[], &mut r8_intents); (r8_tree, r8_intents) });
            });
            out.unwrap()
        };
        t = run(&mut ui, t, (200.0, 60.0), vec![]).0;
        t = run(&mut ui, t, (200.0, 60.0), vec![Event::Pointer_moved((200.0, 60.0)), Event::Scrolled((0.0, -2.0))]).0;
        t = run(&mut ui, t, (200.0, 60.0), vec![]).0;
        let (_, intents) = run(&mut ui, t, at, vec![left_press(at)]);
        ui.destroy();
        intents
    };
    let scroll_rows: Vec<Row> = (0..20).map(|index| row(index + 1, 0, &(index + 1).to_string())).collect();
    expect("list uses shared scroll for row hits", scrolled(&scroll_rows, (200.0, 60.0)), vec![Intent::Select(vec![4])]);
    // a sticky ancestor row takes its own click, not the row scrolled under it
    let family: Vec<Row> = (0..21).map(|index| row(index + 1, if index == 0 { 0 } else { 1 }, &(index + 1).to_string())).collect();
    expect("a click on a sticky ancestor selects it", scrolled(&family, (200.0, 36.0)), vec![Intent::Select(vec![1])]);
}

pub const CASES: &[(&str, fn())] = &[
    ("window_shell_inspector_record", window_shell_inspector_record),
    ("window_shell_inspector_folder_order", window_shell_inspector_folder_order),
    ("window_shell_vector_cells", window_shell_vector_cells),
    ("window_shell_ranged_float_click", window_shell_ranged_float_click),
    ("window_shell_ranged_float_drag", window_shell_ranged_float_drag),
    ("window_shell_tree_pointer_gestures", window_shell_tree_pointer_gestures),
    ("window_shell_tree_shift_select", window_shell_tree_shift_select),
    ("window_shell_tree_scroll_and_sticky", window_shell_tree_scroll_and_sticky),
];

pub fn window_shell_ranged_float_click() {
    ranged_float(false);
}

pub fn window_shell_ranged_float_drag() {
    ranged_float(true);
}
