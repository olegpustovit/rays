//! Checks of the pure parts of editor_inspect, editor_add, editor_menu and editor_panes (the ones
//! that need no `Editor`; the model-driven checks of test_sop_ui run once the loop is ported).
use crate::editor_add::{binding_name, reads, value_entries, value_expression, FORMS};
use crate::editor_inspect::{arg_row_in, expression_text, group_digits, inspector_paths, kind_label, operator_call, operator_rows, sub_entries, ARG_PREFIX};
use flow_check::workspace as w;
use flow_graph::projection as p;
use flow_syntax::context;
use flow_syntax::syntax::{self as s, Node as SNode};
use flow_syntax::ty::Ty;

fn path(parts: &[&str]) -> flow_check::workspace::Path {
    parts.iter().map(|p| (*p).into()).collect()
}

fn form(text: &str) -> s::Syntax {
    s::parse(text).unwrap().remove(0)
}

#[test]
fn digits_group_by_three() {
    assert_eq!(group_digits(0), "0");
    assert_eq!(group_digits(999), "999");
    assert_eq!(group_digits(1204), "1 204");
    assert_eq!(group_digits(-1234567), "-1 234 567");
}

#[test]
fn a_kind_without_a_folder_is_named_for_its_leaf() {
    assert_eq!(kind_label("sop/copy_to_points"), "Copy to points");
    assert_eq!(kind_label("box"), "Box");
}

#[test]
fn a_typed_expression_is_one_form() {
    let ok = expression_text("=(* 2 t)").unwrap();
    assert!(matches!(ok.node, SNode::List(items) if ok.range(items).len() == 3));
    assert_eq!(expression_text("=1 2").unwrap_err(), "Write one expression after the =");
    assert!(expression_text("=(").unwrap_err().starts_with("Expression: "));
}

#[test]
fn only_operator_calls_unfold() {
    assert!(operator_call(&form("(* 0.2 (sin t))")));
    assert!(!operator_call(&form("(sop/box)")));
    assert!(!operator_call(&form("(+)")));
    assert!(!operator_call(&form("3")));
}

#[test]
fn a_card_selected_with_its_row_is_the_row() {
    let card = path(&["g", "a"]);
    let row = path(&["g", "a", ":b"]);
    assert_eq!(inspector_paths(vec![card.clone(), row.clone()]), vec![row.clone()]);
    assert_eq!(inspector_paths(vec![row.clone(), card.clone()]), vec![row.clone()]);
    assert_eq!(inspector_paths(vec![card.clone()]), vec![card.clone()]);
    let two = vec![card.clone(), path(&["g", "c"])];
    assert_eq!(inspector_paths(two.clone()), two);
}

fn row(label: &str, text: &str) -> p::Row {
    let e = form(text);
    p::Row {
        label: label.to_owned(),
        key: flow_graph::flow_edit::ArgKey::Pos(1),
        ty: Some(Ty::Float),
        expr: Some(e),
        chip: p::Chip::Const,
        default: None,
        socket: false,
        kind: p::RowKind::Arg,
        control: p::Control::Plain,
        folder: String::new(),
        head: false,
        cells: vec![],
    }
}

#[test]
fn a_nested_call_gives_one_sub_row_per_leaf() {
    let r = row("b", "(* 0.2 (sin 3) t)");
    let entries = sub_entries(true, &r);
    // 0.2 and t are leaves of the product; the sine's 3 is a leaf one call down
    let names: Vec<String> = entries.iter().map(|(base, ..)| base.clone()).collect();
    assert_eq!(names, vec![format!("{ARG_PREFIX}b@1"), format!("{ARG_PREFIX}b@2.1"), format!("{ARG_PREFIX}b@3")]);
    // the sub-path addresses the child by index, the head being child 0
    assert_eq!(entries[1].2, vec![2, 1]);
    // a vector cell is its row's `.x`
    let found = arg_row_in(&entries, &format!("{ARG_PREFIX}b@1.x")).map(|(_, sub)| sub);
    assert_eq!(found, Some(vec![1]));
    assert!(sub_entries(true, &row("c", "3.0")).is_empty());
}

#[test]
fn a_selected_path_names_its_binding_unless_it_is_nested() {
    assert_eq!(binding_name(&path(&["g", "ring"])), Some("ring".to_owned()));
    assert_eq!(binding_name(&path(&["g", ":points"])), None);
    assert_eq!(binding_name(&path(&["g", "@result"])), None);
    assert_eq!(binding_name(&path(&["g", "a#0"])), None);
    assert!(reads("u", &form("(+ u 1)")));
    assert!(!reads("u", &form("(+ v 1)")));
}

#[test]
fn every_form_of_the_add_menu_parses_and_every_value_entry_makes_an_expression() {
    for (_, name, text, binding) in FORMS.iter() {
        assert!(s::parse(text).is_ok_and(|v| v.len() == 1), "{name}: {text}");
        assert!(!binding.is_empty());
    }
    let ctx = context::value();
    let ops = w::value_ops();
    assert!(!ops.is_empty());
    let entries = value_entries();
    assert!(entries.len() > FORMS.len() + 5);
    for e in &entries {
        assert!(e.key.starts_with('='), "{}", e.key);
        assert_eq!(e.category[0], "Value");
        let (expr, name) = value_expression(&[], None, &ctx, &e.key);
        assert!(!name.is_empty(), "{}", e.key);
        assert!(!matches!(&expr.node, SNode::Kw(_)), "{}", e.key);
    }
}

#[test]
fn operator_rows_split_vec2_and_vec4_literals() {
    let pin_none = |_: &p::Row| None;
    let n = p::Node {
        path: path(&["g"]),
        name: "g".into(),
        binds: vec![],
        head: "+".into(),
        rows: vec![row("a", "[1.0 2.0]"), row("b", "[1.0 2.0 3.0 4.0]")],
        outputs: vec![],
        ty: Ty::Float,
        note: None,
        bypass: false,
        macro_: None,
        lens: None,
        live: false,
        invariant: false,
        synthetic: false,
        zone: None,
        expr: false,
    };
    let rows = operator_rows(&n, &pin_none, false, false, false, false);
    assert_eq!(rows.len(), 2);
    assert_eq!(rows[0].1.fields.len(), 2);
    assert_eq!(rows[0].1.fields[1].name, format!("{ARG_PREFIX}a.y"));
    assert_eq!(rows[1].1.fields.len(), 4);
    assert_eq!(rows[1].1.fields[3].name, format!("{ARG_PREFIX}b.w"));
}

#[test]
fn a_selected_wire_feeds_the_first_input_of_its_type() {
    let (expr, name) = value_expression(&[], Some(("a".to_owned(), Ty::Float)), &context::value(), "=+");
    assert_eq!(name, "sum");
    assert!(reads("a", &expr));
    let (expr, _) = value_expression(&[], None, &context::value(), "=+");
    assert!(!reads("a", &expr));
}
