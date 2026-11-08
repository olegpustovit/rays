// Port of lib/flow_ir/test_inner_sum.ml.
use crate::flow_ir::{self, Profile};
use crate::packed;
use crate::test_support::*;
use flow_check::workspace as w;
use flow_eval as e;
use flow_syntax::syntax;
use flow_syntax::ty::{FnSignature, Ty};
use flow_syntax::value::Value;
use rays_diagnostic::Diagnostic;
use std::sync::Arc;

const MAX_INT: i64 = i64::MAX;

fn checked(body: &str) -> w::Workspace {
    let forms = ok(syntax::parse(&format!("(workspace w (graph g :context host {body}))")));
    match w::check(&[], false, false, None, &catalog(), &forms) {
        (Some(ws), ds) if ds.is_empty() => ws,
        (_, ds) => panic!("{}", show_ds(&ds)),
    }
}

fn mapped(data: &str, bindings: &str, result: Ty, body: &str, indices: Vec<i64>) -> Value {
    let workspace = checked(&format!("(let* [data {data} {bindings} tested (fn [(i : int)] {body})] 0)"));
    let ev = ok(e::r#static(&workspace, true, None));
    let (_, records) = ev.records.iter().find(|(p, _)| p == &vec!["g".to_string(), "tested".to_string()]).unwrap();
    match &records[0].1 {
        Value::Fn(f) => ok(e::Host::map_function(None, None, &FnSignature { params: vec![Ty::Int], result }, *f, &[Value::Int_array(Arc::new(indices))])),
        _ => panic!("expected a function"),
    }
}
fn m(body: &str, indices: Vec<i64>) -> Value {
    mapped("(array/range 80 :type int)", "", Ty::Int, body, indices)
}

fn compile(value: &Value) -> Result<packed::T, Vec<Diagnostic>> {
    let r = residual(value);
    let term = e::Residual::residual_view(r).unwrap().term;
    packed::compile_result(&Default::default(), r, &term)
}

fn parity(value: &Value) {
    let live = live_at(0.);
    let reference = force_reference(value, &live);
    let p = compile(value).unwrap_or_else(|ds| panic!("{}", show_ds(&ds)));
    for domains in [1, 8] {
        let result = std::cell::RefCell::new(None);
        with_domains(domains, &|| {
            *result.borrow_mut() = Some(packed::force(&p, &live, &packed::ForceOptions { strict: true, ..Default::default() }));
        });
        let result = result.into_inner().unwrap();
        match (&reference, &result) {
            (Ok(a), Ok(b)) => assert_eq!(bits(a), bits(b)),
            (Err(a), Err(b)) => assert!(a.code == b.code && a.message == b.message && a.cause == b.cause, "walker {} {:?} vs packed {} {:?}", a.code, a.message, b.code, b.message),
            (a, b) => panic!("inner sum differs from the walker: {:?} vs {:?}", a.as_ref().map(bits), b.as_ref().map(bits)),
        }
    }
}

#[test]
fn counts_k_0_1_64() {
    for count in [0, 1, 64] {
        parity(&m(&format!("(array/sum (map (fn [(j : int)] (int/add (int/mul i 3) j)) (array/range {count} :type int)))"), vec![0, 1, MAX_INT]));
        parity(&m(&format!("(array/sum (array/slice data i {count}))"), vec![0, 1, 80 - count]));
    }
}

#[test]
fn empty_tables_and_helper_tables() {
    parity(&mapped("(array/int 0)", "", Ty::Int, "(array/sum (array/slice data i 0))", vec![0]));
    parity(&mapped("(array/int 0)", "", Ty::Int, "(array/sum (array/slice data i 0))", vec![1]));
    // 32769 rows cross the 1024-lane block boundary and the parallel grain
    parity(&mapped(
        "(array/range 80 :type int)",
        "add-slice (fn [(table : (array int)) (start : int)] (array/sum (array/slice table start 3)))",
        Ty::Int,
        "(add-slice data i)",
        (0..32769).map(|i| i % 77).collect(),
    ));
}

#[test]
fn exact_vector_bits() {
    for (ty, values) in [(Ty::Float, "-0.0"), (Ty::Vec2, "[-0.0 5e-324]"), (Ty::Vec3, "[-0.0 5e-324 -0.25]"), (Ty::Vec4, "[-0.0 5e-324 -0.25 0.75]")] {
        for count in [0, 1, 64] {
            parity(&mapped(&format!("(map (fn [j] {values}) (array/range 80))"), "", ty.clone(), &format!("(array/sum (array/slice data i {count}))"), vec![0, 1, 80 - count]));
            parity(&mapped("(array/range 80 :type int)", "", ty.clone(), &format!("(array/sum (map (fn [j] {values}) (array/range {count})))"), vec![0, 1, 2]));
        }
    }
}

#[test]
fn bounds_errors_and_guards() {
    for count in [0, 1, 64] {
        for first in [-1, 81] {
            parity(&m(&format!("(array/sum (array/slice data i {count}))"), vec![first]));
        }
    }
    parity(&m("(if (< i 2) (array/sum (array/slice data i 3))\n          (array/sum (array/slice data 100 0)))", vec![0, 1]));
    parity(&mapped(
        "(array/range 80 :type int)",
        "",
        Ty::Float,
        "(array/sum (map (fn [(j : int)]\n          (if (= j 2) (error \"E_LATE_MAP\" \"map before sum\") 1e308))\n          (array/range 3 :type int)))",
        vec![0],
    ));
}

#[test]
fn dynamic_or_oversized_counts_refuse() {
    for body in [
        "(array/sum (map (fn [j] j) (array/range 65 :type int)))",
        "(array/sum (array/slice data i 65))",
        "(array/sum (map (fn [j] j) (array/range i :type int)))",
        "(array/sum (array/slice data 0 i))",
    ] {
        match compile(&m(body, vec![1])) {
            Err(ds) => assert!(ds.iter().any(|d| d.code == "E_PACKED_CONSTANT"), "{body}"),
            Ok(_) => panic!("dynamic or oversized inner count compiled"),
        }
    }
}

#[test]
fn late_slice_diagnostic_matches_the_walker() {
    for count in [0, 3] {
        let workspace = checked(&format!("(let* [data (array/int 80)] (map (fn [(i : int)]
               (if (= i 4098) (array/sum (array/slice data 81 {count})) 0))
               (array/range 4101 :type int)))"));
        match (e::static_eval(&workspace), flow_ir::static_(&Default::default(), &workspace)) {
            (Err(reference), Err(actual)) => assert_eq!(format!("{:?}", reference), format!("{:?}", actual)),
            _ => panic!("expected the original late slice diagnostic"),
        }
    }
}

// Product defect: the static map of 4101 rows reports head_elements 4096 (OCaml: 0, the fast path ran every row).
#[test]
fn static_map_profile() {
    let workspace = checked("(map (fn [(i : int)]\n         (array/sum (map (fn [(j : int)] (+ i j)) (array/range 3 :type int))))\n         (array/range 4101 :type int))");
    let profile = Profile::create(Arc::new(|| 0.));
    let fast = ok(Profile::sample(&profile, &mut || flow_ir::static_(&flow_ir::StaticOptions { verify: true, ..Default::default() }, &workspace)));
    let reference = ok(e::static_eval(&workspace));
    assert_eq!(format!("{:?}", fast.results), format!("{:?}", reference.results));
    let maps = Profile::static_maps(&profile);
    assert!(maps.iter().any(|m| m.count == 4101 && m.head_elements == 0 && m.tail_elements == 0), "static maps (last): {:?}", maps.last().map(|m| (m.count, m.head_elements, m.tail_elements, m.deferred)));
}

// Product defect: a slice start of 2^62-1 (OCaml max_int) is refused by packed with "needs a
// non-negative whole length" where the walker says "An array slice must be within its source."
#[test]
fn slice_start_max_int() {
    for count in [0, 1, 64] {
        parity(&m(&format!("(array/sum (array/slice data i {count}))"), vec![MAX_INT]));
    }
}
