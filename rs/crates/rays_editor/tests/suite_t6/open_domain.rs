//! test_open_domain.ml. Not ported (no Rust counterpart worth the plumbing here): the Marshal round trips of the
//! built-in identities (Rust values are plain data), the painted-colour checks through `Scene.Layer.stage_native`
//! on a PXUI frame, and the Lisp text pane completion/help checks (`Lisp_text` takes a names table the OCaml test
//! does not need); everything else is asserted.
#![allow(dead_code, unused_imports)]
use std::sync::Arc;

use editor_document::workspace_doc as wd;
use flow_check::catalog::{Catalog, Kind};
use flow_check::op::{self, Op, Shape, Signature};
use flow_check::workspace as W;
use flow_graph::flow_edit::{ArgKey, Op as Gesture};
use flow_graph::projection as P;
use flow_syntax::context as C;
use flow_syntax::syntax::{self as S, Node};
use flow_syntax::ty::{self as T, Ty};
use flow_syntax::value::Value;
use input::{Event, Key, MouseButton};
use rays_editor::Editor;

use crate::support::*;

fn contains(text: &str, fragment: &str) -> bool {
    text.contains(fragment)
}

fn op_with(name: &str, signature: Signature, body_name: &'static str) -> Op {
    Op {
        name: name.into(),
        ctx: C::sop(),
        signature,
        out: Arc::new(|_| Ok(T::geometry())),
        out_args: None,
        any_num: false,
        choices: Vec::new(),
        shape: Shape::Scalar,
        live: false,
        check: Arc::new(|_| Ok(())),
        body: Arc::new(move |_, node, args| node(body_name, args)),
        category: "Toy".into(),
        arithmetic: None,
        packed_extension: None,
    }
}

fn of_text(ops: &[Op], catalog: &Catalog, text: &str) -> Result<wd::T, Vec<rays_diagnostic::Diagnostic>> {
    wd::of_text(&wd::TextOptions { ops: Some(std::sync::Arc::new(ops.to_vec())), ..Default::default() }, catalog, text)
}

fn doc(r: Result<wd::T, Vec<rays_diagnostic::Diagnostic>>) -> wd::T {
    r.unwrap_or_else(|ds| panic!("{}", diag_text(&ds)))
}

fn static_eval(w: &flow_check::workspace::Workspace, record: bool) -> flow_eval::Eval {
    flow_eval::static_with(w, record, None, None).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)))
}

/// a plan's authored nodes: an inlined body's own nodes sit under its card's node:kind site
fn authored(plan: &flow_eval::Plan) -> usize {
    plan.nodes.iter().filter(|n| !n.site.iter().any(|s| s.starts_with("node:"))).count()
}

fn set_arg(node: &[&str], key: &str, value: S::Syntax) -> Gesture {
    Gesture::SetArg { node: node.iter().map(|s| s.to_string()).collect(), key: ArgKey::Kw(key.into()), sub: Vec::new(), value }
}

pub fn open_domain() {
    let geometry = T::geometry();
    let context = C::sop();
    assert!(T::of_string("geometry") == Some(geometry.clone()) && T::of_syntax(&S::make(S::Draft::Sym("geometry".into()))) == Some(geometry.clone()), "built-in nominal type is absent from annotations");
    let nested = Ty::Record(vec![("x".into(), Ty::List(Box::new(geometry.clone())))]);
    assert!(T::of_string(&T::to_string(&nested)) == Some(nested.clone()), "nested nominal type did not round trip");
    assert!(T::fits(&geometry, &geometry) && !T::fits(&geometry, &Ty::Float) && T::join(&geometry, &Ty::Float).is_none(), "nominal type identity is not distinct");
    assert!(C::of_string("shader").is_err() && C::of_qualified("sop/toy_emit") == Some(context), "built-in context lookup is incorrect");
    let toy = op_with("sop/toy_emit", Signature { pos: vec![("number".into(), Ty::Float)], opt: vec![], rest: None, kw: vec![("gain".into(), Ty::Float)] }, "sop/toy_emit");
    let ops = vec![toy.clone()];
    let catalog = Catalog { version: 1, kinds: Vec::new() };
    let kernel = op_with(
        "sop/toy_kernel",
        Signature { pos: vec![], opt: vec![], rest: None, kw: vec![("field".into(), Ty::Fn(Some(Box::new(T::FnSignature { params: vec![Ty::Vec3], result: Ty::Float }))))] },
        "sop/toy_kernel",
    );
    let kernel_ops = vec![kernel, toy.clone()];
    let field_source = "(workspace w (graph g :context sop (let* [surface (sop/toy_kernel :field (fn [p] p.x))] surface)))";
    let field_doc = doc(of_text(&kernel_ops, &catalog, field_source));
    assert!(field_doc.checked.nodes.is_empty(), "narrow custom catalog acquired embedded SOP definitions");
    let field_scope = P::of_graph(&catalog, &field_doc.checked, "g").unwrap();
    assert!(field_scope.nodes.iter().any(|n| n.zone.as_ref().is_some_and(|z| z.kind == P::ZoneKind::Fn)), "external function port did not project as a zone");
    let field_plan = static_eval(&field_doc.checked, false);
    match field_plan.plan.nodes[0].args.iter().find(|(k, _)| k == "field").map(|(_, v)| v) {
        Some(Value::Fn(f)) => {
            let (params, body) = flow_eval::Residual::function_body(*f).expect("function body");
            assert!(params == vec![(W::Pattern::Name("p".into()), Some(Ty::Vec3))] && *body.ty() == Ty::Float, "external function port lost its specialization");
        }
        _ => panic!("external function port lost its callable"),
    }
    for body in ["1.0", "(fn [a b] a.x)"] {
        assert!(of_text(&kernel_ops, &catalog, &format!("(workspace w (graph g :context sop (sop/toy_kernel :field {body})))")).is_err(), "external function port accepted an invalid function");
    }
    let changed_field = wd::edit(&catalog, &field_doc, &set_arg(&["g", "surface"], "field", S::parse("(fn [p] (* p.x 2.0))").unwrap().remove(0))).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    assert!(contains(&wd::to_text(&changed_field).unwrap(), "(* p.x 2.0)"), "external function-port edit was not retained");
    let text = "(workspace w\n(defn make :context sop [(n : float)] (sop/toy_emit n :gain 2.0))\n(graph g :context sop (let* [first (sop/toy_emit 1.0 :gain 3.0) second (make 4.0)] second)))";
    let workspace = doc(of_text(&ops, &catalog, text));
    let evaluated = static_eval(&workspace.checked, true);
    assert!(evaluated.results.iter().find(|(n, _)| n == "g").map(|(_, v)| v.clone()) == Some(Value::Deferred(geometry.clone(), 1)) && evaluated.plan.nodes.len() == 2, "custom operator did not evaluate to its typed plan");
    assert!(evaluated.plan.nodes.iter().all(|n| n.ty == geometry && n.kind == "sop/toy_emit"), "custom plan lost its built-in result type");
    assert!(op::find(&ops, "toy_emit", &context).is_some_and(|o| o.name == ops[0].name), "bare context operator did not resolve");
    let scope = P::of_graph(&catalog, &workspace.checked, "g").unwrap();
    assert!(scope.nodes.iter().any(|n| n.path == ["g", "first"] && n.ty == geometry), "custom node did not project");
    let edited = wd::edit(&catalog, &workspace, &set_arg(&["g", "first"], "gain", S::make(S::Draft::Num("7.0".into())))).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    assert!(edited.checked.ops.len() == ops.len() && edited.checked.ops[0].name == "sop/toy_emit" && contains(&wd::to_text(&edited).unwrap(), ":gain 7.0"), "checked edit discarded custom operators");
    let reloaded = doc(of_text(&ops, &catalog, &wd::to_text(&edited).unwrap()));
    let plan = |w: &wd::T| format!("{:?}", static_eval(&w.checked, false).plan.nodes.iter().map(|n| (n.id, n.kind.clone(), n.site.clone(), n.args.clone())).collect::<Vec<_>>());
    assert!(plan(&edited) == plan(&reloaded), "custom operator edit did not survive reload");
    let mut changed = toy.clone();
    changed.category = "Changed".into();
    let replaced = doc(of_text(&[changed], &catalog, text));
    assert!(!P::same_graph(&workspace.checked, &replaced.checked, "g"), "changed declarations reused stale projection");
    let theme = pxui::theme::default();
    assert!(pxui_graph::node_menu::port_color(&theme, &geometry) == pxui::theme::ports(&theme).geometry, "custom operator ignored its built-in result color");
    let menu = pxui_graph::node_menu::create(None, 100, 100, pxui_graph::node_menu::of_ops(&ops, &context));
    assert!(pxui_graph::node_menu::keys(&menu, "toy") == ["=sop/toy_emit"], "menu omitted custom operator");
    for context in C::all() {
        let desc = C::descriptor(context);
        assert!(!desc.label.is_empty() && !desc.group.is_empty() && T::of_string(&T::to_string(&desc.result)) == Some(desc.result.clone()), "incomplete context descriptor: {}", desc.name);
    }
    assert!(op::validate(&[ops.clone(), ops.clone()].concat()).is_some(), "duplicate immutable operators accepted");
    assert!(wd::of_text(&wd::TextOptions::default(), &catalog, text).is_err(), "extra operator leaked into default checker");
    let kind = Kind { qualified: "sop/item".into(), context, slots: vec![], parameters: vec![], outputs: vec![], facts: None };
    let with_kind = Catalog { version: 1, kinds: vec![kind] };
    let declared = doc(of_text(&[], &with_kind, "(workspace w (graph g :context sop (sop/item)))"));
    assert!(static_eval(&declared.checked, false).results.iter().find(|(n, _)| n == "g").map(|(_, v)| v.clone()) == Some(Value::Deferred(geometry.clone(), 0)), "custom catalog item ignored its built-in context result type");
    let forms = S::parse("(workspace w (graph g :context sop (sop/missing)))").unwrap();
    assert!(!W::check(&[], false, false, None, &catalog, &forms).1.iter().any(|d| d.code == "E_NAMESPACE"), "built-in catalog prefix was not recognized");
    // the editor host: a menu insertion of the custom operator
    let host_op = op_with("sop/toy_box", Signature { pos: vec![], opt: vec![], rest: None, kw: vec![] }, "sop/box");
    let host_text = "(workspace host (graph g :context sop (let* [base (sop/box)] (sop/toy_box)))\n(graph editor :context editor (ui/workspace (ui/split \"horizontal\" (ui/graph \"g\") (ui/lisp)))))";
    let mut host_ops = flow_sop::operators::all().to_vec();
    host_ops.extend(ops.clone());
    host_ops.push(host_op);
    let workspace = rays_editor::Workspace::load(Some(host_ops.clone()), Vec::new(), None, host_text).unwrap_or_else(|ds| panic!("{}", diag_text(&ds)));
    let presets = std::env::temp_dir().join(format!("rays-domain-test-{}", std::process::id()));
    std::fs::create_dir_all(&presets).unwrap();
    let mut host = Editor::create(Editor::Create { presets: Some(presets.to_string_lossy().into_owned()), ..options() }, &workspace, None, Editor::prepare_fn(), Editor::draw_fn()).unwrap_or_else(|m| panic!("{m}"));
    let step = |host: Editor::t, events: Vec<Event>| Editor::update(host, &frame(events));
    host = step(host, vec![]);
    assert!(Editor::node_box(&host, &["g".to_owned(), "@result".to_owned()]).is_some(), "host omitted custom operator's card");
    let (x, y, width, height) = Editor::panes(&host, &frame(vec![])).graph;
    let point = ((x + width / 2) as f64, (y + height / 2) as f64);
    host = step(host, vec![Event::Pointer_moved(point), Event::Pointer_pressed(MouseButton::LeftButton, point), Event::Pointer_released(MouseButton::LeftButton, point)]);
    host = step(host, vec![Event::Key_pressed(Key::KeyChar('/'))]);
    host = step(host, vec![Event::Key_pressed(Key::KeyChar('a'))]);
    host = step(host, vec![Event::Text_input("sop/toy_box".into())]);
    host = step(host, vec![Event::Key_pressed(Key::Enter)]);
    let after = Editor::workspace(&host);
    let count = authored(&static_eval(&after.checked, false).plan);
    assert!(contains(&wd::to_text(&after).unwrap(), "sop/toy_box") && count == 3, "host menu did not insert custom operator ({count} nodes): {}", wd::to_text(&after).unwrap());
    let path = presets.join("saved.rays");
    std::fs::write(&path, wd::to_text(&Editor::workspace(&host)).unwrap()).unwrap();
    let loaded = editor_document::preset::load_with_ops(None, host_ops, path.to_str().unwrap(), None, &editor_document::settings::none()).unwrap_or_else(|m| panic!("{m}"));
    assert!(authored(&loaded.doc.workspace.lowered.evaluated.plan) == 3, "preset reload discarded custom operators");
    Editor::close(&host);
}
