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
    let mut ui = Ui::create(Platform::Apple, Some(11), rays_shell::host::text(None));
    let mut step = |values: &Vec<Value>, time: f64, events: Vec<Event>| -> (Vec<Value>, param::Effects) {
        let mut out = (values.clone(), param::NO_EFFECTS);
        let v = values.clone();
        ui.frame(&frame(time, events), &mut |ui: &mut Ui| {
            ui.panel(pxui::ui_kit::PanelOpts { x: 0., y: 0., width: 260., ..Default::default() }, "inspector", &mut |ui: &mut Ui| {
                ui.scope("selected", &mut |ui: &mut Ui| {
                    ui.label("Selected shape");
                    let views = schema.view(&v).expect("view");
                    let changes = inspector::fields(ui, &["Geometry".to_string()], None, views.clone());
                    out = if changes.is_empty() { (v.clone(), param::NO_EFFECTS) } else { schema.apply_all(&v, &changes).unwrap_or_else(|m| fail(&m)) };
                });
            });
        });
        out
    };
    let (unchanged, effects) = step(&values, 0., vec![]);
    check(unchanged == values && !effects.cook, "an idle inspector frame changed the graph");
    // The shared inspector rows are 24 points, below the label and the folder's section header
    let row = |index: i64| 24 + 40 + ((index - 2) * 24) + 12;
    let (v, effects) = step(&values, 0.5, click(110, row(3)));
    values = v;
    let get = |values: &Vec<Value>, name: &str| -> Value { values[schema.fields().iter().position(|f| f.name() == name).unwrap()].clone() };
    check(effects.cook && get(&values, "enabled") == Value::Bool(false), "inspector toggle did not apply through the node schema");
    let (v, _) = step(&values, 1., click(170, row(4)));
    values = v;
    let (v, _) = step(&values, 1.05, vec![]);
    values = v;
    let (v, _) = step(&values, 1.1, click(170, row(4) + 54));
    values = v;
    check(get(&values, "mode") == Value::Choice("Sphere".into()), "inspector choice did not apply");
    let (v, _) = step(
        &values,
        1.5,
        vec![Event::PointerPressed(MouseButton::LeftButton, (140., row(2) as f64)), Event::PointerMoved((400., row(2) as f64)), Event::PointerReleased(MouseButton::LeftButton, (400., row(2) as f64))],
    );
    values = v;
    check(get(&values, "count") == Value::Int(5), "inspector integer drag did not clamp to the soft range");
    let defaults: Vec<(String, Value)> = schema.fields().iter().map(|f| (f.name().to_string(), f.default_value().clone())).collect();
    let (reset, effects) = schema.apply_all(&values, &defaults).unwrap_or_else(|m| fail(&m));
    check(effects.cook && get(&reset, "count") == Value::Int(2) && get(&reset, "mode") == Value::Choice("Cube".into()), "selected-node reset did not restore PPX defaults");
    let (normalized, _) = schema.apply_all(&schema.default().to_vec(), &[("count".to_string(), Value::Int(99))]).unwrap_or_else(|m| fail(&m));
    check(get(&normalized, "count") == Value::Int(10), "hard range normalization changed");
    ui.destroy();
}

/// The Flow inspector's rows: pin, reset, expression, hex, folders.
pub fn flow_rows() {
    let schema = parameters_schema();
    let views = schema.view(schema.default()).expect("view");
    let count = pxui_view_without_folder(views.iter().find(|v| v.name == "count").expect("count").clone());
    let mut ui = Ui::create(Platform::Apple, Some(11), rays_shell::host::text(None));
    let row = |drive: Option<&str>, locked: bool| FlowRow { path: "count".into(), fields: vec![count.clone()], shown: true, locked, drive: drive.map(String::from), live: Some("3".into()) };
    let mut step = |opts: &FlowOpts, rows: &[FlowRow], events: Vec<Event>| -> Vec<FlowChange> {
        let mut out = Vec::new();
        ui.frame(&frame(2., vec![]), &mut |ui: &mut Ui| {
            inspector::flow_fields(ui, opts, None, rows);
        });
        ui.frame(&frame(2., events), &mut |ui: &mut Ui| {
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
    ui.destroy();
}

fn pxui_view_without_folder(mut v: param::FieldView) -> param::FieldView {
    v.folder = Vec::new();
    v
}
