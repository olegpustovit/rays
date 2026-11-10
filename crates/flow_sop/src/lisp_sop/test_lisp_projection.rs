//! Port of lib/flow_sop/test_lisp_projection.ml: the card's inspector rows and its factory node's
//! parameter metadata: this module lowers calls as that node.
use super::support::*;
use crate::{catalog, lisp_sop, manifest, operators};
use flow_check::catalog::Catalog;
use flow_check::workspace as w;
use flow_graph::flow_edit::{self as e, ArgKey, Op};
use flow_graph::projection as p;
use flow_syntax::{lisp, syntax};
use param::{Impact, Value};
use sop::edit_graph::{self as edit, Factory};
use sop::node::{self, RebuildFn};
use std::sync::Arc;

fn checked(catalog: &Catalog, forms: &[syntax::Syntax]) -> w::Workspace {
    match w::check(operators::all(), false, false, None, catalog, forms) {
        (Some(ws), _) => ws,
        (None, ds) => panic!("{}", ds.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("\n")),
    }
}

fn field(name: &str, label: &str, kind: param::Kind, default: f64, vec3: Option<(&str, i64)>) -> param::Field {
    let mut f = param::field(name.to_owned(), kind, Value::Float(default))
        .unwrap()
        .label(label.to_owned())
        .unwrap()
        .description(format!("Edit {label}"))
        .folder(vec!["Shape".to_owned()])
        .unwrap()
        .primary(true)
        .unit("m".to_owned())
        .impact(Impact::Cook);
    if let Some((group, component)) = vec3 {
        f = f.vec3(group.to_owned(), component).unwrap();
    }
    f
}

/// The OCaml schema, independent of the declaration, as a metadata oracle.
fn schema() -> Arc<param::Schema> {
    Arc::new(
        param::schema(
            "test_card".into(),
            vec![
                field("distance", "Distance", param::floating(-10.0, 10.0, Some(-20.0), Some(20.0)).unwrap(), 0.5, None),
                field("scale_x", "Scale X", param::floating(0.0, 10.0, None, None).unwrap(), 1.0, Some(("scale", 0))),
                field("scale_y", "Scale Y", param::floating(0.0, 10.0, None, None).unwrap(), 2.0, Some(("scale", 1))),
                field("scale_z", "Scale Z", param::floating(0.0, 10.0, None, None).unwrap(), 3.0, Some(("scale", 2))),
            ],
        )
        .unwrap(),
    )
}

fn definition(maximum: i64) -> String {
    format!(
        r#"(defnode sop/test_card :label "Test card" :category ["Test"]
  [(geo : geometry)
   (distance : float 0.5 :label "Distance" :description "Edit Distance"
     :folder ["Shape"] :primary true :unit "m" :min -10 :max {maximum} :hard_min -20 :hard_max 20)
   (scale_x : float 1 :label "Scale X" :description "Edit Scale X"
     :folder ["Shape"] :primary true :unit "m" :min 0 :max 10 :vec3 ["scale" 0])
   (scale_y : float 2 :label "Scale Y" :description "Edit Scale Y"
     :folder ["Shape"] :primary true :unit "m" :min 0 :max 10 :vec3 ["scale" 1])
   (scale_z : float 3 :label "Scale Z" :description "Edit Scale Z"
     :folder ["Shape"] :primary true :unit "m" :min 0 :max 10 :vec3 ["scale" 2])]
  geo)"#
    )
}

const GRAPH: &str = r#"(graph g :context sop
  (let* [src (sop/grid :columns 2 :rows 2)
         amount 2.0
         card (sop/test_card src :distance 2.0 :scale [1.0 2.0 3.0])]
    card))"#;

fn source(maximum: i64) -> String {
    format!("(workspace w {} {})", definition(maximum), GRAPH)
}

fn catalog_of(factories: &[Factory]) -> Catalog {
    catalog::of_factories(manifest::VERSION, &[], None, factories).unwrap()
}

fn native_node(schema: &Arc<param::Schema>, values: Vec<Value>, input: &node::T) -> node::T {
    let s = schema.clone();
    let rebuild: RebuildFn = Arc::new(move |_label, inputs, values| Ok(native_node(&s, values.to_vec(), &inputs[0])));
    node::parameterize(schema.clone(), values, rebuild, input).unwrap()
}

fn native_factory() -> Factory {
    let schema = schema();
    let mut args = edit::factory_args("test_card", "Test card", &["Test"]);
    args.slots = Some(vec!["geo".to_owned()]);
    args.fields = schema.view(schema.default()).unwrap();
    edit::factory(args, 1, Arc::new(move |inputs| Ok(native_node(&schema, schema_defaults(), &inputs[0]))))
}

fn schema_defaults() -> Vec<Value> {
    schema().default().to_vec()
}

/// Form ids and source spans vary with the declaration prefix; authored content and all card
/// metadata must remain structurally equal.
fn normalize(card: &p::Node) -> p::Node {
    let mut card = card.clone();
    for row in &mut card.rows {
        row.expr = row.expr.as_ref().map(|expr| parse(&lisp::flat(expr)).remove(0));
    }
    card
}

fn card(cat: &Catalog, workspace: &w::Workspace) -> p::Node {
    p::find(&p::of_graph(cat, workspace, "g").unwrap(), &["g".into(), "card".into()]).expect("the card")
}

fn path(parts: &[&str]) -> flow_check::workspace::Path {
    parts.iter().map(|s| (*s).into()).collect()
}

fn apply(cat: &Catalog, forms: &[syntax::Syntax], op: Op) -> Result<(Vec<syntax::Syntax>, w::Workspace), rays_diagnostic::Diagnostic> {
    e::apply_checked_with(operators::all(), false, None, cat, forms, &op)
}

fn num(text: &str) -> syntax::Syntax {
    let mut b = flow_syntax::arena::SyntaxBuilder::new();
    let name = b.intern(text);
    let id = b.push(0, syntax::Node::Num(name), rays_diagnostic::Span { start: 0, finish: 0 }, None);
    b.freeze().handle(id)
}

fn row_expr(cat: &Catalog, ws: &w::Workspace, key: &ArgKey) -> Option<String> {
    card(cat, ws).rows.iter().find(|r| r.key == *key).expect("the row").expr.as_ref().map(lisp::flat)
}

#[test]
fn lisp_card_matches_the_ocaml_schema_and_gestures_project_back() {
    let base = catalog_of(&lisp_sop::factories());
    let mut with_native = vec![native_factory()];
    with_native.extend(lisp_sop::factories());
    let native_catalog = catalog_of(&with_native);
    let forms = parse(&source(10));
    let workspace = checked(&base, &forms);
    let native = checked(&native_catalog, &parse(&format!("(workspace w {GRAPH})")));
    let actual = card(&base, &workspace);
    let expected = card(&native_catalog, &native);
    assert!(normalize(&actual) == normalize(&expected), "Lisp and OCaml cards differ");
    assert_eq!(actual.rows.iter().map(|r| r.label.clone()).collect::<Vec<_>>(), vec!["geo", "distance", "scale"], "Lisp card omits its schema rows");
    assert!(actual.rows[0].head && p::bypassable(&actual), "Lisp header geometry input differs");
    let full = |n: &p::Node| p::lines(&|_| None, p::Level::Full, n);
    assert!(full(&normalize(&actual)) == full(&normalize(&expected)), "Lisp full card and inspector folder rows differ");
    let n = lisp_sop::workspace_node(&[], &source(10));
    let native_fields = schema().view(&[Value::Float(2.0), Value::Float(1.0), Value::Float(2.0), Value::Float(3.0)]).unwrap();
    assert!(node::parameter_fields(&n) == native_fields, "Lisp inspector metadata differs from OCaml schema");
    let (edited, changed) = apply(&base, &forms, Op::SetArg { node: actual.path.clone(), key: ArgKey::Kw("distance".into()), sub: vec![], value: num("3.0") }).unwrap();
    assert_eq!(row_expr(&base, &changed, &ArgKey::Kw("distance".into())), Some("3.0".to_owned()), "scalar scrub did not project back");
    let (_, changed) = apply(&base, &edited, Op::SetArg { node: actual.path.clone(), key: ArgKey::Kw("scale".into()), sub: vec![1], value: num("4.0") }).unwrap();
    assert_eq!(row_expr(&base, &changed, &ArgKey::Kw("scale".into())), Some("[1.0 4.0 3.0]".to_owned()), "vector scrub did not project back");
    let (_, connected) = apply(&base, &forms, Op::Connect { node: actual.path.clone(), key: ArgKey::Kw("distance".into()), src: "amount".into(), iter: false }).unwrap();
    let drive = card(&base, &connected).rows.iter().find(|r| r.key == ArgKey::Kw("distance".into())).unwrap().clone();
    assert_eq!(p::sources(&drive), vec!["amount".to_owned()], "parameter drive did not project back");
    assert!(
        apply(&base, &forms, Op::SetArg { node: actual.path.clone(), key: ArgKey::Kw("distance".into()), sub: vec![], value: num("21") }).is_err(),
        "parameter edit ignored hard bound"
    );
    assert!(!p::same_graph(&workspace, &checked(&base, &parse(&source(9))), "g"), "changed Lisp schema retained stale projection");
    let (_, bypassed) = apply(&base, &forms, Op::ToggleBypass { node: actual.path.clone() }).unwrap();
    assert!(card(&base, &bypassed).bypass, "Lisp card bypass gesture failed");
}

#[test]
fn a_nested_lisp_node_is_a_card_with_typed_rows() {
    let base = catalog_of(&lisp_sop::factories());
    let nested = parse(&format!(
        "(workspace w {} (graph g :context sop\n    (sop/test_card (sop/test_card (sop/grid) :distance 1.0) :distance 2.0)))",
        definition(10)
    ));
    let workspace = checked(&base, &nested);
    let leaf = e::nested_leaf("@result", &ArgKey::Pos(0)).unwrap();
    let node_path: w::Path = vec!["g".into(), leaf.into()];
    let projected = p::of_graph(&base, &workspace, "g").unwrap();
    assert_eq!(projected.nodes.len(), 3, "nested Lisp node lacks a graph card");
    let node = p::find(&projected, &node_path).expect("the nested card");
    assert!(p::anonymous(&node) && node.rows.len() == 3, "nested Lisp card lost typed parameter rows");
    let (_, changed) = apply(&base, &nested, Op::SetArg { node: node_path.clone(), key: ArgKey::Kw("distance".into()), sub: vec![], value: num("4.0") }).unwrap();
    let node = p::find(&p::of_graph(&base, &changed, "g").unwrap(), &node_path).unwrap();
    let distance = node.rows.iter().find(|r| r.key == ArgKey::Kw("distance".into())).unwrap();
    assert_eq!(distance.expr.as_ref().map(lisp::flat), Some("4.0".to_owned()), "nested Lisp parameter gesture failed");
    let _ = path;
}
