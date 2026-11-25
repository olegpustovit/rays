//! Selected-node inspector rows apply a concrete parameter schema through Schema::apply_all.
use flow_check::catalog::Catalog;
use input::{Event, Key, MouseButton, Platform};
use param::{Impact, Value};
use pxui::ui::Ui;
use rays_shell::inspector::{self, FlowChange, FlowOpts, FlowRow};

fn fail(message: &str) -> ! {
    panic!("test_sop_ui: {message}")
}

fn check(condition: bool, message: &str) {
    if !condition {
        fail(message);
    }
}

fn parameters_schema() -> param::Schema {
    let f = |name: &str, kind: param::Kind, default: Value| param::field(name.to_string(), kind, default).expect("field");
    let folder = |field: param::Field, path: &[&str]| field.folder(path.iter().map(|s| s.to_string()).collect()).expect("folder");
    param::schema(
        "parameters".to_string(),
        vec![
            folder(f("count", param::integer(1, 5, Some(1), Some(10)).unwrap(), Value::Int(2)), &["Geometry"]).label("Count".to_string()).unwrap(),
            folder(f("enabled", param::toggle(), Value::Bool(true)), &["Geometry"]),
            folder(f("amount", param::floating(0., 1., None, None).unwrap(), Value::Float(0.5)), &["Look", "Noise"]).impact(Impact::View),
            folder(f("title", param::text(), Value::Text(b"Node SOP".as_slice().into())), &["Look"]).impact(Impact::Export),
            folder(f("mode", param::choice(vec!["Cube".into(), "Sphere".into()]).unwrap(), Value::Choice("Cube".into())), &["Geometry"]),
        ],
    )
    .expect("schema")
}

fn frame(time: f64, events: Vec<Event>) -> rays::frame::Frame {
    rays::frame::Frame { input: input::Frame { size: (320, 240), events, ..input::at_time(time) }, pixel_scale: (1., 1.), fps: 0., mouse_delta: (0., 0.) }
}

fn click(x: i64, y: i64) -> Vec<Event> {
    let p = (x as f64, y as f64);
    vec![Event::PointerPressed(MouseButton::LeftButton, p), Event::PointerReleased(MouseButton::LeftButton, p)]
}

/// the inspector's x on an operator row that reads the context symbol `t` disconnects it
pub fn reset_context_symbol() {
    let src = flow_syntax::syntax::parse("(workspace w (graph g :context host (let* [s (sin (* t 0.5))] s)))").unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    let catalog = Catalog { version: 1, kinds: Vec::new() };
    let w = match flow_check::workspace::check(&[], false, false, None, &catalog, &src) {
        (Some(w), ds) if ds.is_empty() => w,
        _ => fail("check"),
    };
    let scope = flow_graph::projection::of_graph_with(&|_| false, &catalog, &w, "g").expect("scope");
    let n = flow_graph::projection::find(&scope, &["g".into(), "s#0".into()]).expect("s#0");
    let a = n.rows[0].clone();
    let a2 = a.clone();
    let found = rays_editor::editor_inspect::arg_request(&src, &n, &move |_| Some((a2.clone(), Vec::new())), &FlowChange::Reset("@arg:a".to_string()));
    match found {
        Some(rays_editor::editor_model::Change::SyntaxEdit(flow_graph::flow_edit::Op::Disconnect { node, key, fallback, .. })) => {
            assert!(node == n.path && key == a.key && fallback.is_some());
        }
        other => fail(&format!("the x on a context-symbol row emitted no disconnect: {}", other.is_some())),
    }
}

pub fn wire_color() {
    let wire = rays_editor::editor_renderer::wire_color;
    assert!(wire(&rays_math::color::rgb(250, 250, 250)) == rays_math::color::hex_exn("#285f77").unwrap());
    assert!(wire(&rays_math::color::rgb(8, 8, 10)) == rays_math::color::hex_exn("#bed7e1").unwrap());
}

/// The selected-node inspector: toggle, choice, integer drag clamped to the soft range, reset.
pub fn inspector_rows() {
    let schema = parameters_schema();
    let mut values = schema.default().to_vec();
    check(schema.fields().len() == 5, "sop_params defaults or ignored-field policy is incorrect");
    let mut ui = rays_shell::host::create(Platform::Apple, Some(11), None);
    ui.set_a11y(true);
    let step = |ui: &mut Ui, values: &Vec<Value>, time: f64, events: Vec<Event>| -> (Vec<Value>, param::Effects) {
        let mut out = (values.clone(), param::NO_EFFECTS);
        let v = values.clone();
        let pointer = events.iter().rev().find_map(|event| match event {
            Event::PointerMoved(point) | Event::PointerPressed(_, point) | Event::PointerReleased(_, point) => Some(*point),
            _ => None,
        });
        let mut input = frame(time, events);
        input.input.pointer = pointer.unwrap_or_default();
        rays_shell::host::build(ui, &input, &mut |ui: &mut Ui| {
            ui.panel("inspector", &pxui::Overlay::DEFAULT.w_px(260.0), |ui: &mut Ui| {
                ui.id("selected", |ui: &mut Ui| {
                    ui.label("Selected shape", pxui::Label::BODY);
                    let views = schema.view(&v).expect("view");
                    let changes = inspector::fields(ui, &["Geometry".to_string()], None, false, views.clone());
                    if !changes.is_empty() {
                        out = schema.apply_all(&v, &changes).unwrap_or_else(|m| fail(&m));
                    }
                });
            });
        });
        out
    };
    let (unchanged, effects) = step(&mut ui, &values, 0., vec![]);
    check(unchanged == values && !effects.cook, "an idle inspector frame changed the graph");
    let row = |ui: &Ui, name| crate::support::shell::inspector_row(ui, 0.0, name);
    let middle = |row: pxui::Rect| (row.y + row.h / 2.0).round() as i64;
    let enabled = row(&ui, "enabled");
    let (v, effects) = step(&mut ui, &values, 0.5, click(enabled.x.round() as i64 + 110, middle(enabled)));
    values = v;
    let get = |values: &Vec<Value>, name: &str| -> Value { values[schema.fields().iter().position(|f| f.name() == name).unwrap()].clone() };
    check(effects.cook && get(&values, "enabled") == Value::Bool(false), "inspector toggle did not apply through the node schema");
    let mode = row(&ui, "mode");
    let (v, _) = step(&mut ui, &values, 1., click(mode.x.round() as i64 + 170, middle(mode)));
    values = v;
    let (v, _) = step(&mut ui, &values, 1.05, vec![]);
    values = v;
    let sphere = pxui::testing::find(&ui, pxui::Role::MenuItem, "Sphere").expect("Sphere menu item");
    let (v, _) = step(&mut ui, &values, 1.1, click((sphere.x + sphere.w / 2.0) as i64, middle(sphere)));
    values = v;
    check(get(&values, "mode") == Value::Choice("Sphere".into()), "inspector choice did not apply");
    let count = row(&ui, "count");
    let start = (f64::from(count.x) + 140.0, middle(count) as f64);
    let end = (start.0 + 260.0, start.1);
    let (v, _) = step(
        &mut ui,
        &values,
        1.5,
        vec![Event::PointerPressed(MouseButton::LeftButton, start), Event::PointerMoved(end), Event::PointerReleased(MouseButton::LeftButton, end)],
    );
    values = v;
    check(get(&values, "count") == Value::Int(5), "inspector integer drag did not clamp to the soft range");
    let defaults: Vec<(String, Value)> = schema.fields().iter().map(|f| (f.name().to_string(), f.default_value().clone())).collect();
    let (reset, effects) = schema.apply_all(&values, &defaults).unwrap_or_else(|m| fail(&m));
    check(effects.cook && get(&reset, "count") == Value::Int(2) && get(&reset, "mode") == Value::Choice("Cube".into()), "selected-node reset did not restore PPX defaults");
    let (normalized, _) = schema.apply_all(&schema.default().to_vec(), &[("count".to_string(), Value::Int(99))]).unwrap_or_else(|m| fail(&m));
    check(get(&normalized, "count") == Value::Int(10), "hard range normalization changed");
    rays_shell::host::destroy(&mut ui);
}

/// The Flow inspector's rows: pin, reset, expression, hex, folders.
pub fn flow_rows() {
    let schema = parameters_schema();
    let views = schema.view(schema.default()).expect("view");
    let count = pxui_view_without_folder(views.iter().find(|v| v.name == "count").expect("count").clone());
    let mut ui = rays_shell::host::create(Platform::Apple, Some(11), None);
    let row = |drive: Option<&str>, locked: bool| FlowRow { path: "count".into(), fields: vec![count.clone()], shown: true, locked, drive: drive.map(String::from), live: Some("3".into()) };
    let mut pointer = (0.0, 0.0);
    let mut step = |opts: &FlowOpts, rows: &[FlowRow], events: Vec<Event>| -> Vec<FlowChange> {
        let mut out = Vec::new();
        let mut settle = frame(2.0, vec![]);
        settle.input.pointer = pointer;
        rays_shell::host::build(&mut ui, &settle, &mut |ui: &mut Ui| {
            inspector::flow_fields(ui, opts, None, rows);
        });
        if let Some(point) = events.iter().rev().find_map(|event| match event {
            Event::PointerMoved(point) | Event::PointerPressed(_, point) | Event::PointerReleased(_, point) => Some(*point),
            _ => None,
        }) {
            pointer = point;
        }
        let mut input = frame(2.0, events);
        input.input.pointer = pointer;
        rays_shell::host::build(&mut ui, &input, &mut |ui: &mut Ui| {
            out = inspector::flow_fields(ui, opts, None, rows);
        });
        out
    };
    let pin = FlowOpts { pin_click: true, ..Default::default() };
    check(step(&pin, &[row(None, false)], click(14, 12)) == vec![FlowChange::Pinned("count".into(), false)], "Flow inspector pin did not emit the card-row request");
    // the cross shows on the hovered row: hover first
    step(&pin, &[row(Some("← Clock.t"), true)], vec![Event::PointerMoved((258., 12.))]);
    check(step(&pin, &[row(Some("← Clock.t"), true)], click(258, 12)) == vec![FlowChange::Reset("count".into())], "Flow inspector reset did not clear the drive");
    let expression_row = row(Some("=t"), true);
    step(&pin, &[expression_row.clone()], click(170, 12));
    let edits = step(&FlowOpts::default(), &[expression_row.clone()], vec![Event::KeyPressed(Key::End), Event::TextInput("*2".into()), Event::KeyPressed(Key::Enter)]);
    check(edits == vec![FlowChange::Expression("count".into(), "=t*2".into())], "Flow inspector expression field did not commit an edit");
    // folders
    let folder_row = FlowRow { fields: vec![views.iter().find(|v| v.name == "count").unwrap().clone()], ..row(None, false) };
    let open = FlowOpts { expanded: vec!["Geometry".into()], pin_click: true, ..Default::default() };
    check(step(&open, &[folder_row.clone()], click(14, 40)) == vec![FlowChange::Pinned("count".into(), false)], "Flow inspector did not show an initially expanded folder");
    step(&open, &[folder_row.clone()], click(10, 16));
    check(step(&open, &[folder_row.clone()], click(14, 40)).is_empty(), "Flow inspector collapsed folder kept its field interactive");
    step(&open, &[folder_row.clone()], click(10, 16));
    check(step(&open, &[folder_row], click(14, 40)) == vec![FlowChange::Pinned("count".into(), false)], "Flow inspector folder did not reopen");
    drop(step);
    rays_shell::host::destroy(&mut ui);
}

fn pxui_view_without_folder(mut v: param::FieldView) -> param::FieldView {
    v.folder = Vec::new();
    v
}
