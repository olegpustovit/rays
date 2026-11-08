//! test_editor_transactions.ml: the node a click selects is the node an edit in the same frame
//! changes, undo agrees with the document, a changed graph with the same prepared value is drawn
//! again, and one pointer drag is one undo entry.
use super::t4::*;
use editor_document::workspace_doc;
use flow_graph::flow_edit as fe;
use flow_syntax::syntax as S;
use input::{Event, Key, MouseButton::*};
use param::Value;
use rays_editor::Editor;
use sop::edit_graph::{self as edit, Factory};
use sop::node::{self, build, CookMode, RebuildFn};
use std::cell::Cell;
use std::rc::Rc;
use std::sync::Arc;

/// Test_editor_commands.schema: one integer field "value" (0..100).
pub fn schema() -> Arc<param::Schema> {
    let kind = param::integer(0, 100, None, None).unwrap();
    let field = param::field("value".into(), kind, Value::Int(0)).unwrap();
    Arc::new(param::schema("command-value".into(), vec![field]).unwrap())
}

/// Lisp_sop.snapshot: a generator node holding an already built geometry.
pub fn snapshot(geometry: &rdk_core::geometry::T) -> node::T {
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

fn factory() -> Factory {
    let schema = schema();
    let mut args = edit::factory_args("test_source", "Test source", &["Test"]);
    args.fields = schema.view(schema.default()).unwrap();
    edit::factory(args, 0, Arc::new(move |_| Ok(source(&schema, 0))))
}

const TEXT: &str = r#"(workspace transactions
  (graph g :context sop
    (let* [first (sop/test_source :value 0)
           second (sop/test_source :value 1)]
      (sop/merge first second))))"#;

pub fn run() {
    if !crate::common::gpu() {
        return;
    }
    let mut factories = vec![factory()];
    factories.extend(sop::nodes::factories());
    let workspace: workspace_doc::T = of_text(Some(&factories), TEXT);
    let draws = Rc::new(Cell::new(0i64));
    let counter = draws.clone();
    let draw: Editor::Draw_fn = Rc::new(move |n, p| {
        counter.set(counter.get() + 1);
        Editor::draw(n, p)
    });
    let ed = Editor::create(
        Editor::Create { ui_text_size: Some(11), platform: Some(input::Platform::Apple), factories: Some(factories.clone()), ..Default::default() },
        &workspace,
        None,
        Editor::prepare_fn(),
        draw,
    )
    .expect("opens");
    let mut d = Driver::new(ed);
    let wait_draw = |d: &mut Driver, after: i64| {
        settle(
            || {
                d.step_at((0., 0.), vec![]);
                draws.get() > after
            },
            "draw",
        )
    };
    wait_draw(&mut d, 0);
    // the scene opens as a list: a click on the object's row selects it and [i] enters its graph
    let (gx, gy, _, _) = Editor::panes(d.e(), &frame0()).graph;
    let row = ((gx + 60) as f64, (gy + 24 + 12) as f64);
    d.step_at(row, vec![]);
    d.step_at(row, click(row));
    d.step_at((0., 0.), vec![chr('i')]);
    d.count += 30; // past the double-click interval
    // the lowered node whose value is n, by stable id
    let node_id = |d: &Driver, n: i64| -> i64 {
        let network = Editor::network(d.e()).unwrap();
        sop::edit_graph::inspect(&network)
            .into_iter()
            .find(|info| node::parameter_fields(&info.node).iter().any(|f| f.name == "value" && f.current == Value::Int(n)))
            .expect("a node with that value")
            .id
    };
    let first = node_id(&d, 0);
    let second = node_id(&d, 1);
    let value = |d: &Driver, id: i64| -> Value {
        let network = Editor::network(d.e()).unwrap();
        let n = sop::edit_graph::find(&network, id).unwrap();
        node::parameter_fields(&n).into_iter().find(|f| f.name == "value").unwrap().current
    };
    let text = |d: &Driver| workspace_doc::to_text(&Editor::workspace(d.e())).unwrap();
    let header = |d: &Driver, path: &[&str]| -> (f64, f64) {
        let path: Vec<String> = path.iter().map(|s| s.to_string()).collect();
        let (x, y, w, _) = Editor::node_box(d.e(), &path).expect("the node has no box in the graph pane");
        ((x + w / 2) as f64, (y + 10) as f64)
    };
    let undo = |d: &mut Driver| d.bk(&[], &[Key::Meta], (0., 0.), vec![chr('z')]);
    let set = |d: &mut Driver, path: &[&str], n: i64| {
        let op = fe::Op::SetArg { node: path.iter().map(|s| s.to_string()).collect(), key: fe::ArgKey::Kw("value".into()), sub: vec![], value: S::make(S::Draft::Num(n.to_string())) };
        let ed = d.ed.take().unwrap();
        d.ed = Some(Editor::edit(ed, &op).unwrap_or_else(|m| panic!("{m}")));
    };
    // select the second source with a click, then edit it: the selection follows the stable node
    d.step_at((0., 0.), vec![]);
    d.step_at((0., 0.), vec![]);
    let selected = header(&d, &["g", "second"]);
    d.step_at(selected, click(selected));
    d.step_at((0., 0.), vec![]);
    let before = text(&d);
    let before_draws = draws.get();
    set(&mut d, &["g", "second"], 7);
    assert!(value(&d, first) == Value::Int(0) && value(&d, second) == Value::Int(7), "selecting and editing in one frame changed the wrong stable node");
    assert!(Editor::can_undo(d.e()), "the edit did not enter document history");
    wait_draw(&mut d, before_draws);
    let before_draws = draws.get();
    undo(&mut d);
    assert!(value(&d, first) == Value::Int(0) && value(&d, second) == Value::Int(1) && Editor::can_redo(d.e()) && text(&d) == before, "undo did not restore the history present");
    wait_draw(&mut d, before_draws);
    assert!(draws.get() >= 3, "a changed graph with the same prepared value retained a stale drawing");
    // one drag is one undo entry, and two drags are two
    let drag = |d: &mut Driver, path: &[&str], dx: f64| {
        let at = header(d, path);
        d.bk(&[LeftButton], &[], at, vec![Event::Pointer_pressed(LeftButton, at)]);
        for step_dx in [dx / 4., dx / 2., dx * 3. / 4.] {
            let target = (at.0 + step_dx, at.1);
            d.bk(&[LeftButton], &[], target, vec![Event::Pointer_moved(target)]);
        }
        let target = (at.0 + dx, at.1);
        d.step_at(target, vec![Event::Pointer_released(LeftButton, target)]);
    };
    d.count += 90;
    let unmoved = text(&d);
    drag(&mut d, &["g", "second"], 24.);
    assert!(text(&d) != unmoved, "dragging a node did not change the saved layout");
    let first_move = text(&d);
    d.count += 90;
    drag(&mut d, &["g", "second"], 24.);
    undo(&mut d);
    assert!(text(&d) == first_move, "separate node drags merged after release");
    undo(&mut d);
    assert!(text(&d) == unmoved, "one drag did not undo as one entry");
    d.close();
}
