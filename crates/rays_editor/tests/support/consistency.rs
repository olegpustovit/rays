//! Helpers of the t2 group: ports of test_support.ml, ws_fixture.ml, editor_prepare.ml and the
//! frame/click helpers of test_editor_input.ml the group's tests use.
#![allow(dead_code)]
use std::sync::Arc;

use editor_document::document;
use editor_document::lowering;
use editor_document::workspace_doc::{self, TextOptions};
use flow_graph::flow_edit::{self as fe, ArgKey, Op};
use flow_syntax::syntax::{self as syn, Syntax};
use input::{Event, Key, MouseButton};
use rays::frame;
use rays_editor::Editor;

pub type Ed = Editor::t;

pub fn s(parts: &[&str]) -> Vec<String> {
    parts.iter().map(|x| (*x).to_owned()).collect()
}

pub fn diag_text(ds: &[rays_diagnostic::Diagnostic]) -> String {
    ds.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("; ")
}

pub fn catalog() -> flow_check::catalog::Catalog {
    (*rays_editor::workspace_catalog(None).expect("catalog")).clone()
}

pub fn parse(text: &str) -> Syntax {
    match syn::parse(text) {
        Ok(mut forms) if forms.len() == 1 => forms.remove(0),
        other => panic!("{text}: {:?}", other.map(|f| f.len())),
    }
}

pub fn workspace_with(settings: Option<editor_document::settings::T>, text: &str) -> workspace_doc::T {
    workspace_doc::of_text(&TextOptions { settings, ..Default::default() }, &catalog(), text).unwrap_or_else(|ds| panic!("{}", diag_text(&ds)))
}

pub fn workspace(text: &str) -> workspace_doc::T {
    workspace_with(None, text)
}

/// `Ws_fixture.of_text ~factories`
pub fn workspace_factories(factories: &[sop::edit_graph::Factory], text: &str) -> workspace_doc::T {
    let catalog = rays_editor::workspace_catalog(Some(factories)).expect("catalog");
    workspace_doc::of_text(&TextOptions::default(), &catalog, text).unwrap_or_else(|ds| panic!("{}", diag_text(&ds)))
}

pub fn build_with(previous: Option<&document::T>, ws: &workspace_doc::T) -> document::T {
    let options = lowering::Options { previous: previous.cloned(), ..Default::default() };
    lowering::of_workspace(&options, ws).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)))
}

pub fn build(ws: &workspace_doc::T) -> document::T {
    build_with(None, ws)
}

/// `a == b` on workspaces: the checked workspace is one `Arc`.
pub fn same_ws(a: &workspace_doc::T, b: &workspace_doc::T) -> bool {
    Arc::ptr_eq(&a.checked, &b.checked)
}

pub fn set_arg(node: &[&str], key: &str, value: &str) -> Op {
    Op::SetArg { node: s(node), key: ArgKey::Kw(key.to_owned()), sub: Vec::new(), value: parse(value) }
}

pub fn set_pos(node: &[&str], value: &str) -> Op {
    Op::SetArg { node: s(node), key: ArgKey::Pos(0), sub: Vec::new(), value: parse(value) }
}

pub fn contains(text: &str, sub: &str) -> bool {
    text.contains(sub)
}

/// `Test_editor_input.frame`
pub fn frame_full(buttons: &[MouseButton], keys: &[Key], delta: (f64, f64), mouse: (f64, f64), events: Vec<Event>, count: i64) -> frame::t {
    frame::t {
        input: input::Frame {
            dt: 1. / 60.,
            frame: count,
            tick: count,
            size: (900, 640),
            pointer: mouse,
            keys: keys.to_vec(),
            buttons: buttons.to_vec(),
            events,
            ..input::at_time(count as f64 / 60.)
        },
        pixel_scale: (1., 1.),
        fps: 60.,
        mouse_delta: delta,
    }
}

pub fn frame_at(mouse: (f64, f64), events: Vec<Event>, count: i64) -> frame::t {
    frame_full(&[], &[], (0., 0.), mouse, events, count)
}

/// The consistency suite's `frame n`: the pointer at (450, 300), no events.
pub fn frame(n: i64) -> frame::t {
    frame_at((450., 300.), Vec::new(), n)
}

pub fn click_events(p: (f64, f64)) -> Vec<Event> {
    vec![Event::Pointer_pressed(MouseButton::LeftButton, p), Event::Pointer_released(MouseButton::LeftButton, p)]
}

pub fn undo_frame(point: (f64, f64), n: i64) -> frame::t {
    frame_full(&[], &[Key::Meta], (0., 0.), point, vec![Event::Key_pressed(Key::KeyChar('z'))], n)
}

pub fn temp_dir(tag: &str) -> String {
    let dir = std::env::temp_dir().join(format!("rays-{tag}-{}-{}", std::process::id(), std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).map(|d| d.as_nanos()).unwrap_or(0)));
    std::fs::create_dir_all(&dir).expect("temp dir");
    dir.to_string_lossy().into_owned()
}

pub fn with_dir<R>(f: impl FnOnce(&str) -> R) -> R {
    let dir = temp_dir("t2");
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| f(&dir)));
    let _ = std::fs::remove_dir_all(&dir);
    match result {
        Ok(r) => r,
        Err(e) => std::panic::resume_unwind(e),
    }
}

/// `create` with the OCaml tests' defaults: platform Apple, await, the surface prepare and a
/// scene3 draw of the prepared surface (`native`: the app's own prepare and draw).
pub fn create(ws: &workspace_doc::T, presets: Option<&str>, camera: Option<rays::easy_camera::t>, native: bool) -> Ed {
    let options = Editor::Create {
        platform: Some(input::Platform::Apple),
        ui_text_size: Some(if native { 13 } else { 11 }),
        presets: presets.map(str::to_owned),
        camera,
        await_: Some(true), domains: Some(1),
        ..Default::default()
    };
    Editor::create(options, ws, None, Editor::prepare_fn(), Editor::draw_fn()).expect("the editor opens")
}

pub fn step(e: Ed, n: i64) -> Ed {
    Editor::update(e, &frame(n))
}

pub fn edit(e: Ed, op: &Op) -> Ed {
    Editor::edit(e, op).unwrap_or_else(|m| panic!("edit refused: {m}"))
}

pub fn flat(x: &Syntax) -> String {
    flow_syntax::lisp::flat(x)
}

#[allow(unused)]
pub fn fe_unused(_: fe::Op) {}

// ---- Test_editor_transactions.factory (test_source) ----
use param::Value;
use sop::edit_graph::{self as edit_g, Factory};
use sop::node::{self, build, CookMode, RebuildFn};

pub fn value_schema() -> Arc<param::Schema> {
    let kind = param::integer(0, 100, None, None).unwrap();
    let field = param::field("value".into(), kind, Value::Int(0)).unwrap();
    Arc::new(param::schema("command-value".into(), vec![field]).unwrap())
}

fn snapshot(geometry: &rdk_core::geometry::T) -> node::T {
    let parameters = format!("data_id={};bytes={}", rdk_core::geometry::data_id(geometry), rdk_core::geometry::payload_bytes(geometry));
    let geometry = geometry.clone();
    build::make_geometry(
        build::Args { parameters: parameters.into_bytes(), cook_mode: CookMode::Generator, ..build::args("snapshot") },
        None,
        Arc::new(move |_, _, _| Ok(node::GeometryCooked { geometry: geometry.clone(), instances: None })),
    )
}

fn source(schema: &Arc<param::Schema>, value: i64) -> node::T {
    let s = schema.clone();
    let rebuild: RebuildFn = Arc::new(move |_label, _inputs, values| {
        let v = match values.first() {
            Some(Value::Int(v)) => *v,
            _ => 0,
        };
        Ok(source(&s, v))
    });
    let base = snapshot(&rdk_ops::line_geometry::points(&[(value as f64, 0., 0.)]).unwrap());
    node::parameterize(schema.clone(), vec![Value::Int(value)], rebuild, &base).unwrap()
}

pub fn test_factory() -> Factory {
    let schema = value_schema();
    let mut args = edit_g::factory_args("test_source", "Test source", &["Test"]);
    args.fields = schema.view(schema.default()).unwrap();
    edit_g::factory(args, 0, Arc::new(move |_| Ok(source(&schema, 0))))
}
