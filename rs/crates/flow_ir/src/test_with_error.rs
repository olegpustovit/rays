// Port of lib/flow_ir/test_with_error.ml.
// Not ported: (a) the `Effect.perform Bulk_static` block (flow_eval's `ResidualHandlers::bulk` returns
// an Option, so a handler cannot raise a diagnostic through the effect handler) and (b) `raise Exit`
// propagating out of `V.with_error` (Rust has no exceptions). The checked graph edit (flow_graph) is
// kept in `graph_edits`, compiled out until flow_ir lists flow_graph as a dev-dependency.
use crate::core_ir as C;
use crate::test_support::{self as ts, ok};
use flow_check::catalog::Catalog;
use flow_check::workspace as W;
use flow_eval as E;
use flow_syntax::syntax;
use flow_syntax::ty::Ty;
use flow_syntax::value::{self as V, ResidualId, Value};
use rays_diagnostic::{Diagnostic, Position, Span, Trace};
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::Arc;

fn catalog() -> Catalog { Catalog { version: 1, kinds: vec![] } }

fn source(body: &str) -> String { format!("(workspace errors (graph g :context value (let* [tested {body}] 0.)))") }

fn has_error(ds: &[Diagnostic]) -> bool { ds.iter().any(|d| d.severity == rays_diagnostic::Severity::ErrorSeverity) }

fn check(text: &str) -> W::Workspace {
    let forms = ok(syntax::parse(text));
    match W::check(&[], false, false, None, &catalog(), &forms) {
        (Some(workspace), ds) if !has_error(&ds) => workspace,
        (_, ds) => panic!("{}", ts::show_ds(&ds)),
    }
}

const MAPPER: &str = "(fn [failure] failure)";

fn diagnostic_ty() -> Ty { Ty::Record(vec![("code".into(), Ty::Text), ("message".into(), Ty::Text), ("cause".into(), Ty::List(Box::new(Ty::Text)))]) }

fn residual(body: &str) -> ResidualId {
    let workspace = check(&source(&format!("(with_error {body} {MAPPER})")));
    let evaluation = ok(E::static_with(&workspace, true, None, None));
    assert_eq!(evaluation.plan.nodes.len(), 0);
    let path = vec!["g".to_owned(), "tested".to_owned()];
    match &evaluation.records.iter().find(|(p, _)| *p == path).expect("tested record").1[0].1 {
        Value::Residual(r) => *r,
        _ => panic!("with_error was evaluated eagerly"),
    }
}

fn compare(body: &str) -> Result<Value, Diagnostic> {
    let r = residual(body);
    let program = ts::lower_strict(r).unwrap_or_else(|form| panic!("{form}"));
    let live = input::at_time(0.);
    let a = ts::run_simple(&program, &live);
    let b = E::Host::force_reference(&Value::Residual(r), &live, None, None, None);
    match (&a, &b) {
        (Ok(a), Ok(b)) => assert!(a == b),
        (Err(a), Err(b)) => assert!(a.code == b.code && a.message == b.message && a.cause == b.cause && a.hints == b.hints && a.trace == b.trace, "{a:?} / {b:?}"),
        _ => panic!("core and reference outcomes differ: {a:?} / {b:?}"),
    }
    a
}

fn error(code: &str, message: &str, cause: Option<&str>, result: Result<Value, Diagnostic>) {
    match result {
        Err(d) => assert!(d.code == code && d.message == message && d.cause.as_deref() == cause, "{d:?}"),
        Ok(v) => panic!("expected an error, got {v:?}"),
    }
}

#[test]
fn with_error_nil_is_no_geo() { assert!(compare(&format!("(with_error nil {MAPPER})")) == Ok(Value::No_geo)); }

#[test]
fn with_error_passes_a_value_without_running_the_mapper() { assert!(compare("(with_error 7 (fn [failure] (error \"E_MAPPER\" \"never\")))") == Ok(Value::Int(7))); }

#[test]
fn with_error_in_an_untaken_branch_is_not_evaluated() { assert!(compare(&format!("(if false (with_error (error \"E_BAD\" \"never\") {MAPPER}) 3)")) == Ok(Value::Int(3))); }

#[test]
fn with_error_maps_code_and_message_and_keeps_the_cause() {
    error(
        "E_MAPPED",
        "mapped 日本😀",
        Some("raw cause"),
        compare("(with_error (error \"E_CHILD\" \"child\" :cause \"raw cause\") (fn [failure] (assoc failure :code \"E_MAPPED\" :message \"mapped 日本😀\")))"),
    );
}

#[test]
fn with_error_mapper_failures_replace_the_error() {
    error("E_MAPPER", "mapper", None, compare("(with_error (error \"E_CHILD\" \"child\") (fn [failure] (error \"E_MAPPER\" \"mapper\")))"));
}

#[test]
fn with_error_leaves_cancellation_unchanged() {
    error(
        "E_CANCELLED",
        "cancelled unchanged",
        None,
        compare("(with_error (error \"E_CANCELLED\" \"cancelled unchanged\") (fn [failure] (error \"E_WRONG\" \"mapper ran\")))"),
    );
}

#[test]
fn with_error_nests() {
    error(
        "E_OUTER",
        "inner",
        None,
        compare("(with_error (with_error (error \"E_CHILD\" \"inner\") (fn [failure] (error \"E_MAPPER\" \"inner\"))) (fn [failure] (assoc failure :code \"E_OUTER\")))"),
    );
}

#[test]
fn with_error_passes_a_large_packed_array() {
    assert!(compare(&format!("(with_error (exact (map (fn [x] (+ x 1.0)) (array/float 32769 2.0))) {MAPPER})")) == Ok(Value::Float_array(Arc::new(vec![3.; 32769]))));
}

#[test]
fn with_error_in_an_unused_local_helper_still_fails_when_called() {
    error(
        "E_UNUSED",
        "unused helper",
        None,
        compare("(let* [guard (fn [x] (with_error (error \"E_UNUSED\" \"unused helper\") (fn [failure] failure))) unused (guard 1)] 0)"),
    );
}

#[test]
fn ill_typed_with_error_forms_are_rejected_by_the_checker() {
    let mut accepted = Vec::new();
    for body in [
        "(with_error 1 (fn [failure] {:code \"bad\"}))",
        "(with_error 1 (fn [failure] (assoc failure :message (str t))))",
        "(with_error (state [s 0] (+ s 1)) (fn [failure] failure))",
    ] {
        let forms = ok(syntax::parse(&source(body)));
        let (_, diagnostics) = W::check(&[], false, false, None, &catalog(), &forms);
        if !has_error(&diagnostics) { accepted.push(body); }
    }
    assert!(accepted.is_empty(), "accepted: {accepted:?}");
}

fn original() -> Diagnostic {
    // ponytail: OCaml's cause starts with the raw byte \255; Rust's cause is a String, so U+00FF stands in.
    rays_diagnostic::error(
        "E_CHILD".into(),
        "child".into(),
        Some(Span { start: 19, finish: 23 }),
        Some(Position { line: 2, col: 3 }),
        Some("\u{ff} raw 日本".into()),
        vec!["hint".into()],
        vec![Trace { node_id: 13, label: "child".into(), operation: "native.child".into() }],
    )
}

#[test]
fn diagnostic_envelopes_are_lossless() {
    let original = original();
    let source = C::Source { site: vec!["g".into()], owner: vec!["g".into()], instance: 0, prefix: vec![], iter: vec![], span: Some(Span { start: 0, finish: 5 }) };
    let statement = |id: i64, node: C::StatementNode| C::Statement { id, source: source.clone(), node };
    let program = C::Program {
        constants: vec![],
        locals: vec![diagnostic_ty(), Ty::Any, diagnostic_ty(), Ty::Any],
        body: vec![statement(
            0,
            C::StatementNode::Protect {
                body: vec![statement(1, C::StatementNode::Assign(1, C::Expr::Prim(C::Primitive::Fail_diagnostic(original.clone()), vec![])))],
                value: 1,
                failure: 0,
                handler: vec![statement(2, C::StatementNode::Assign(2, C::Expr::Local(0)))],
                mapped: 2,
                result: 3,
            },
        )],
        results: vec![3],
        state_slots: vec![],
        regions: vec![],
        call_slots: 0,
    };
    match ts::run_simple(&program, &input::at_time(0.)) {
        Err(actual) => assert!(actual == original),
        _ => panic!("expected the original diagnostic"),
    }
    let without_cause = Diagnostic { cause: None, ..original.clone() };
    match V::with_error(&|| V::raise_diagnostic(without_cause.clone()), &|payload| Ok(payload)) {
        Err(actual) => assert!(actual == without_cause),
        Ok(_) => panic!("expected an error"),
    }
    let called = AtomicI64::new(0);
    match V::with_error(&|| V::raise_diagnostic(original.clone()), &|_| {
        called.fetch_add(1, Ordering::SeqCst);
        Ok(Value::Record(vec![
            ("code".into(), Value::Text(b"E_NEW".to_vec())),
            ("message".into(), Value::Text(b"new".to_vec())),
            ("cause".into(), Value::List(vec![Value::Text(b"a".to_vec()), Value::Text(b"b".to_vec())])),
        ]))
    }) {
        Err(actual) => assert!(actual.code == "E_DIAGNOSTIC_MAP" && actual.trace == original.trace && actual.hints == original.hints && actual.cause == original.cause),
        Ok(_) => panic!("expected an error"),
    }
    assert_eq!(called.load(Ordering::SeqCst), 1);
    match V::with_error(&|| V::raise_diagnostic(original.clone()), &|_| Err(V::fail("E_CANCELLED", "mapper cancellation"))) {
        Err(actual) => assert!(actual.code == "E_CANCELLED" && actual.message == "mapper cancellation" && actual.span.is_none() && actual.hints.is_empty() && actual.trace.is_empty()),
        Ok(_) => panic!("expected an error"),
    }
}

// The checked graph edit (flow_graph), compiled out: see the header.
// checked-edit tests (flow_graph dev-dependency)
mod graph_edits {
    use super::*;
    use flow_graph::flow_edit::{apply_checked, ArgKey, Op};
    use flow_graph::projection;
    use flow_syntax::lisp;
    use flow_syntax::syntax::{make, Node};

    #[test]
    fn a_with_error_child_edits_through_the_graph() {
        let forms = ok(syntax::parse(&source("(let* [guarded (with_error (array/float 2 3.0) (fn [failure] failure))] guarded)")));
        let checked = check(&lisp::print(&forms).0);
        let scope = projection::of_graph(&catalog(), &checked, "g").unwrap();
        let card = projection::find(&scope, &["g".into(), "tested".into(), "guarded#0".into()]).expect("card");
        assert_eq!(card.head, "array/float");
        let (forms, checked) = ok(apply_checked(&catalog(), &forms, &Op::SetArg { node: card.path.clone(), key: ArgKey::Pos(1), sub: vec![], value: make(Node::Num("4.0".into())) }));
        let evaluation = ok(E::run(&checked, true, None, None, None, 0.));
        let path = vec!["g".to_owned(), "tested".to_owned()];
        assert!(evaluation.records.iter().find(|(p, _)| *p == path).unwrap().1[0].1 == Value::Float_array(Arc::new(vec![4., 4.])));
        let forms_again = ok(syntax::parse(&lisp::print(&forms).0));
        let (_, ds) = W::check(&[], false, false, None, &catalog(), &forms_again);
        assert!(!has_error(&ds));
        assert!(apply_checked(&catalog(), &forms, &Op::SetArg { node: card.path, key: ArgKey::Pos(0), sub: vec![], value: make(Node::Str(b"invalid".to_vec())) }).is_err());
    }
}
