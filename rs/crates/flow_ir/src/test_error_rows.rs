// Port of lib/flow_ir/test_error_rows.ml.
// The two checked graph-edit blocks of the OCaml file use flow_graph, which flow_ir does not list
// as a dependency; they are kept in `graph_edits` below, compiled out until a dev-dependency
// `flow_graph = { path = "../flow_graph" }` is added to flow_ir/Cargo.toml.
use crate::core_exec::ResolveFn;
use crate::test_support::{self as ts, ok};
use flow_check::catalog::Catalog;
use flow_check::workspace as W;
use flow_eval as E;
use flow_syntax::syntax;
use flow_syntax::ty::Ty;
use flow_syntax::value::{self as V, Value};
use rays_diagnostic::Diagnostic;
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::Arc;

const OCAML_MAX_INT: i64 = 4611686018427387903;

fn catalog() -> Catalog { Catalog { version: 1, kinds: vec![] } }

const MAPPER: &str = r##"(fn [index] {:code "E_ROW" :message (str "bad " index) :cause (list (str "raw 日本😀 " index))})"##;

fn source(body: &str) -> String {
    format!("(workspace rows (graph g :context value [(flags : (array int) (array/int 0))] (let* [tested {body}] 0.)))")
}

fn has_error(ds: &[Diagnostic]) -> bool { ds.iter().any(|d| d.severity == rays_diagnostic::Severity::ErrorSeverity) }

fn checked(text: &str) -> W::Workspace {
    let forms = ok(syntax::parse(text));
    match W::check(&[], false, false, None, &catalog(), &forms) {
        (Some(workspace), ds) if !has_error(&ds) => workspace,
        (_, ds) => panic!("{}", ts::show_ds(&ds)),
    }
}

fn evaluate(resolve: Option<ResolveFn>, flags: Value, body: &str) -> Result<Value, Diagnostic> {
    let workspace = checked(&source(body));
    let evaluation = ok(E::static_with(&workspace, true, Some(&[("g".to_owned(), vec![("flags".to_owned(), flags)])]), None));
    let path = vec!["g".to_owned(), "tested".to_owned()];
    let value = evaluation.records.iter().find(|(p, _)| *p == path).expect("tested record").1[0].1.clone();
    let Value::Residual(residual) = &value else { panic!("row diagnosis was eager") };
    let program = ts::lower_strict(*residual).unwrap_or_else(|form| panic!("{form}"));
    let live = input::at_time(0.);
    let native = ts::run(None, false, None, resolve.clone(), &program, &live);
    let reference = E::Host::force_reference(&value, &live, None, None, resolve);
    match (&native, &reference) {
        (Ok(a), Ok(b)) => assert!(a == b),
        (Err(a), Err(b)) => assert!(a.code == b.code && a.message == b.message && a.cause == b.cause),
        _ => panic!("native and reference outcomes differ: {native:?} / {reference:?}"),
    }
    native
}

fn resolve(grain: i64, calls: &Arc<AtomicI64>) -> ResolveFn {
    let calls = calls.clone();
    Arc::new(move |value: &Value| match value {
        Value::Source(name, ty, args) if name == "$core/diagnostic_index" && *ty == Ty::Int => match args.as_slice() {
            [(rows_name, rows), (policy_name, Value::Text(policy))] if rows_name == "rows" && policy_name == "policy" => {
                calls.fetch_add(1, Ordering::SeqCst);
                let grain = match policy.as_slice() {
                    b"first" => None,
                    b"range-last" => Some(grain),
                    _ => panic!("unexpected policy"),
                };
                match V::error_row_index(rows, grain, None)? {
                    Some(index) => Ok(Value::Int(index)),
                    None => panic!("no failing row"),
                }
            }
            _ => panic!("unexpected source arguments"),
        },
        _ => panic!("unexpected resolver request"),
    })
}

fn failure(index: i64, result: Result<Value, Diagnostic>) {
    match result {
        Err(d) => assert!(d.code == "E_ROW" && d.message == format!("bad {index}") && d.cause == Some(format!("raw 日本😀 {index}"))),
        Ok(v) => panic!("expected a row failure, got {v:?}"),
    }
}

fn range_last(body: &str) -> String { format!("(error_rows flags {body} :policy \"range-last\")") }

#[test]
fn clean_flags_never_ask_the_host_or_run_the_mapper() {
    let calls = Arc::new(AtomicI64::new(0));
    let empty = Value::Int_array(Arc::new(vec![]));
    let zero = Value::Int_array(Arc::new(vec![0; 32769]));
    for flags in [empty, zero] {
        assert!(evaluate(Some(resolve(97, &calls)), flags, &range_last(r##"(fn [index] (error "E_UNTAKEN" "mapper"))"##)) == Ok(Value::No_geo));
    }
    assert_eq!(calls.load(Ordering::SeqCst), 0);
}

#[test]
fn boundary_and_tail_selection_with_complete_immutable_flags() {
    let calls = Arc::new(AtomicI64::new(0));
    for bits in [vec![0usize, 1, 96, 97, 98], vec![96, 97, 98], vec![4095, 4096, 4097], vec![32768]] {
        let mut rows = vec![0i64; 32769];
        for i in &bits { rows[*i] = 1; }
        let before = rows.clone();
        let flags = Value::Int_array(Arc::new(rows.clone()));
        for grain in [1, 97, 4096, OCAML_MAX_INT] {
            let selected = V::error_row_index(&flags, Some(grain), None).unwrap().expect("a failing row");
            failure(selected, evaluate(Some(resolve(grain, &calls)), flags.clone(), &range_last(MAPPER)));
        }
        failure(bits[0] as i64, evaluate(None, flags.clone(), &format!("(error_rows flags {MAPPER})")));
        assert!(rows == before);
    }
}

#[test]
fn malformed_context_cancellation_and_laziness() {
    let one = || Value::Int_array(Arc::new(vec![1]));
    match evaluate(None, one(), &range_last(MAPPER)) {
        Err(d) => assert!(d.code == "E_DIAGNOSTIC_CONTEXT"),
        _ => panic!("expected E_DIAGNOSTIC_CONTEXT"),
    }
    // A malformed later flag cannot hide behind an earlier ordinary failure.
    match evaluate(None, Value::Int_array(Arc::new(vec![1, 0, 2])), &format!("(error_rows flags {MAPPER})")) {
        Err(d) => assert!(d.code == "E_DIAGNOSTIC_ROWS"),
        _ => panic!("expected E_DIAGNOSTIC_ROWS"),
    }
    assert!(evaluate(None, one(), &format!("(if (< t 0.) (let* [unused (error_rows flags {MAPPER})] 0) 0)")) == Ok(Value::Int(0)));
    let cancel: ResolveFn = Arc::new(|_| Err(rays_diagnostic::error("cancelled".into(), "cancel".into(), None, None, Some("raw cancel".into()), Vec::new(), Vec::new())));
    match evaluate(Some(cancel), one(), &range_last(MAPPER)) {
        Err(d) => assert!(d.code == "cancelled" && d.message == "cancel" && d.cause.as_deref() == Some("raw cancel")),
        _ => panic!("expected a cancellation"),
    }
    match evaluate(None, one(), r##"(error_rows flags (fn [index] (error "cancelled" "mapper cancel" :cause "original cancel")))"##) {
        Err(d) => assert!(d.code == "cancelled" && d.message == "mapper cancel" && d.cause.as_deref() == Some("original cancel")),
        _ => panic!("expected the mapper cancellation"),
    }
    match V::error_row_index(&Value::Float_array(Arc::new(vec![1.])), None, None) {
        Err(d) => assert!(d.code == "E_DIAGNOSTIC_ROWS"),
        Ok(_) => panic!("float rows were accepted"),
    }
    let polls = AtomicI64::new(0);
    let check = || -> Result<(), Diagnostic> {
        if polls.fetch_add(1, Ordering::SeqCst) + 1 == 3 { Err(V::fail("E_CANCELLED", "cancel scan")) } else { Ok(()) }
    };
    match V::error_row_index(&Value::Int_array(Arc::new(vec![0; 5001])), Some(97), Some(&check)) {
        Err(d) => assert!(d.code == "E_CANCELLED"),
        Ok(_) => panic!("the scan was not cancelled"),
    }
}

#[test]
fn ill_typed_error_rows_forms_are_rejected_by_the_checker() {
    let mut accepted = Vec::new();
    for body in [
        "(error_rows flags (fn [index] index))",
        "(error_rows flags (fn [index] {:code \"x\" :message (str t) :cause (list)}))",
        "(error_rows flags (fn [index] {:code \"x\" :message \"x\" :cause (list)}) :policy (str \"first\"))",
        "(with_error 1 (fn [error] (let* [unused (error_rows flags (fn [i] error))] error)))",
    ] {
        let forms = ok(syntax::parse(&source(body)));
        let (_, ds) = W::check(&[], false, false, None, &catalog(), &forms);
        if !has_error(&ds) { accepted.push(body); }
    }
    assert!(accepted.is_empty(), "accepted: {accepted:?}");
}

// The checked graph edits (flow_graph), compiled out: see the header.
// checked-edit tests (flow_graph dev-dependency)
mod graph_edits {
    use super::*;
    use flow_graph::flow_edit::{apply_checked, ArgKey, Op};
    use flow_graph::projection;
    use flow_syntax::lisp;
    use flow_syntax::syntax::{make, Node};

    #[test]
    fn a_positional_error_rows_argument_edits_through_the_graph() {
        let text = source(&format!("(let* [checked (error_rows (array/int 2 0) {MAPPER})] checked)"));
        let forms = ok(syntax::parse(&text));
        let workspace = checked(&text);
        let scope = projection::of_graph(&catalog(), &workspace, "g").unwrap();
        let card = projection::find(&scope, &["g".into(), "tested".into(), "checked#0".into()]).expect("card");
        assert_eq!(card.head, "array/int");
        let (forms, workspace) = ok(apply_checked(&catalog(), &forms, &Op::SetArg { node: card.path.clone(), key: ArgKey::Pos(1), sub: vec![], value: make(Node::Num("1".into())) }));
        let printed = lisp::print(&forms).0;
        checked(&printed);
        let result = ok(E::static_with(&workspace, true, Some(&[("g".to_owned(), vec![("flags".to_owned(), Value::Int_array(Arc::new(vec![])))])]), None));
        let path = vec!["g".to_owned(), "tested".to_owned()];
        let value = result.records.iter().find(|(p, _)| *p == path).unwrap().1[0].1.clone();
        failure(0, E::Host::force_reference(&value, &input::at_time(0.), None, None, None));
        assert!(apply_checked(&catalog(), &forms, &Op::SetArg { node: card.path, key: ArgKey::Pos(1), sub: vec![], value: make(Node::Str(b"wrong storage".to_vec())) }).is_err());
    }

    #[test]
    fn the_policy_keyword_edits_through_the_graph() {
        let text = source(&format!("(let* [checked (error_rows (array/int 2 1) {MAPPER} :policy \"first\")] checked)"));
        let forms = ok(syntax::parse(&text));
        let workspace = checked(&text);
        let scope = projection::of_graph(&catalog(), &workspace, "g").unwrap();
        let card = projection::find(&scope, &["g".into(), "tested".into(), "checked".into()]).expect("card");
        assert_eq!(card.head, "error_rows");
        let (forms, _) = ok(apply_checked(&catalog(), &forms, &Op::SetArg { node: card.path.clone(), key: ArgKey::Kw("policy".into()), sub: vec![], value: make(Node::Str(b"range-last".to_vec())) }));
        let calls = Arc::new(AtomicI64::new(0));
        failure(
            1,
            evaluate(
                Some(resolve(97, &calls)),
                Value::Int_array(Arc::new(vec![])),
                &format!("(error_rows (array/int 2 1) {MAPPER} :policy \"range-last\")"),
            ),
        );
        checked(&lisp::print(&forms).0);
        assert!(apply_checked(&catalog(), &forms, &Op::SetArg { node: card.path, key: ArgKey::Kw("policy".into()), sub: vec![], value: make(Node::Str(b"unknown".to_vec())) }).is_err());
    }
}
