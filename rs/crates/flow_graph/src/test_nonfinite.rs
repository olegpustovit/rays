// Port of lib/flow_graph/test_nonfinite.ml.
use crate::flow_edit::{apply_checked, ArgKey, Op};
use crate::projection as p;
use crate::test_support::*;
use flow_check::catalog::{self, Parameter, TermNode};
use flow_check::port_type::PortType;
use flow_eval::Value;
use flow_syntax::syntax::{self as s, Node};
use flow_syntax::ty::Ty;
use flow_syntax::{lisp, value};
use param::{FloatRange, KindView, Value as PValue};
use rays_diagnostic::Severity;

fn same(expected: f64, v: &Value) -> bool {
    match v {
        Value::Float(actual) => {
            if expected.is_nan() { actual.is_nan() } else { expected == *actual }
        }
        _ => false,
    }
}

const NONFINITE: [(&str, f64); 3] = [("NaN", f64::NAN), ("Infinity", f64::INFINITY), ("-Infinity", f64::NEG_INFINITY)];

#[test]

fn nonfinite_literals_round_trip() {
    let source = parse("(workspace literals (graph g :context value (let* [x 0.5 ; keep this note\n] x)))");
    let parameter = Parameter {
        name: "size".into(),
        label: "Size".into(),
        ty: Some(PortType::Float),
        fields: vec![("size".into(), KindView::Floating(FloatRange { soft_min: 0.0, soft_max: 2.0, hard_min: Some(0.0), hard_max: Some(4.0) }), PValue::Float(1.0))],
        folder: vec![],
        primary: false,
        unit: None,
    };
    for (text, expected) in NONFINITE {
        assert!(s::number(text) && lisp::float(expected) == text);
        assert!(matches!(&parse(text)[0].node, Node::Num(n) if n == text));
        assert!(s::number_value(text).is_some_and(|f| f.is_nan() == expected.is_nan() && (f.is_nan() || f == expected)));
        let (source, w) =
            apply_checked(&catalog(), &source, &Op::SetArg { node: path(&["g", "x"]), key: ArgKey::Whole, sub: vec![], value: s::make(Node::Num(text.into())) }).unwrap();
        assert!(same(expected, &result(&w, 0.0)));
        let card = p::find(&p::of_graph(&catalog(), &w, "g").unwrap(), &path(&["g", "x"])).unwrap();
        assert!(card.ty == Ty::Float && p::value_card(&card));
        let row = card.rows[0].clone();
        assert!(row.key == ArgKey::Whole && row.chip == p::Chip::Const);
        let (printed, spans) = lisp::print(&source);
        assert!(printed.contains(';'));
        let reparsed = parse(&printed);
        assert!(lisp::print(&reparsed).0 == printed && same(expected, &result(&check(&reparsed), 0.0)));
        let literal = row.expr.clone().unwrap();
        let mut seven = literal.clone();
        seven.node = Node::Num("7.5".into());
        let (patched, spans) = lisp::patch_atoms(&(printed.clone(), spans), &[seven]).unwrap();
        assert_eq!(result(&check(&parse(&patched)), 0.0), Value::Float(7.5));
        let (restored, _) = lisp::patch_atoms(&(patched, spans), &[literal]).unwrap();
        assert!(same(expected, &result(&check(&parse(&restored)), 0.0)));
        let (_, finite) = apply_checked(&catalog(), &source, &Op::SetArg { node: card.path.clone(), key: ArgKey::Whole, sub: vec![], value: s::make(Node::Num("7.5".into())) }).unwrap();
        assert_eq!(result(&finite, 0.0), Value::Float(7.5));
        let mut codes: Vec<(Severity, String)> = Vec::new();
        catalog::validate_parameter(
            &mut |severity, code, _| codes.push((severity, code.to_owned())),
            &parameter,
            &catalog::Term { node: TermNode::Literal(PValue::Float(expected)), ty: Some(PortType::Float) },
        );
        assert_eq!(codes, vec![(Severity::ErrorSeverity, "E_NONFINITE".to_owned())]);
        match value::validate(&Value::Float_array(std::sync::Arc::new(vec![expected]))) {
            Ok(()) => panic!("validate accepted a nonfinite array"),
            Err(d) => assert_eq!(d.code, "E_NONFINITE"),
        }
        match value::coerce_to(&Ty::Int, &Value::Float(expected)) {
            Ok(_) => panic!("coerce accepted a nonfinite float"),
            Err(d) => assert_eq!(d.code, "E_NONFINITE"),
        }
    }
    for text in ["nan", "inf", "1e999"] {
        assert!(s::number_value(text).is_none());
    }
    for text in ["nan", "inf"] {
        assert!(!s::number(text));
    }
    let overflow = parse("(workspace overflow (graph g :context value 1e999))");
    let (w, ds) = flow_check::workspace::check(&[], false, false, None, &catalog(), &overflow);
    assert!(w.is_none() && ds.iter().any(|d| d.code == "E_TYPE"));
}

// the assertions of `nonfinite_literals_round_trip` that do not need the checker to accept a nonfinite literal
#[test]
fn nonfinite_primitives() {
    let parameter = Parameter {
        name: "size".into(),
        label: "Size".into(),
        ty: Some(PortType::Float),
        fields: vec![("size".into(), KindView::Floating(FloatRange { soft_min: 0.0, soft_max: 2.0, hard_min: Some(0.0), hard_max: Some(4.0) }), PValue::Float(1.0))],
        folder: vec![],
        primary: false,
        unit: None,
    };
    for (text, expected) in NONFINITE {
        assert!(s::number(text) && lisp::float(expected) == text);
        assert!(matches!(&parse(text)[0].node, Node::Num(n) if n == text));
        assert!(s::number_value(text).is_some_and(|f| f.is_nan() == expected.is_nan() && (f.is_nan() || f == expected)));
        let mut codes: Vec<(Severity, String)> = Vec::new();
        catalog::validate_parameter(
            &mut |severity, code, _| codes.push((severity, code.to_owned())),
            &parameter,
            &catalog::Term { node: TermNode::Literal(PValue::Float(expected)), ty: Some(PortType::Float) },
        );
        assert_eq!(codes, vec![(Severity::ErrorSeverity, "E_NONFINITE".to_owned())]);
        match value::validate(&Value::Float_array(std::sync::Arc::new(vec![expected]))) {
            Ok(()) => panic!("validate accepted a nonfinite array"),
            Err(d) => assert_eq!(d.code, "E_NONFINITE"),
        }
        match value::coerce_to(&Ty::Int, &Value::Float(expected)) {
            Ok(_) => panic!("coerce accepted a nonfinite float"),
            Err(d) => assert_eq!(d.code, "E_NONFINITE"),
        }
    }
    for text in ["nan", "inf", "1e999"] {
        assert!(s::number_value(text).is_none());
    }
    for text in ["nan", "inf"] {
        assert!(!s::number(text));
    }
    let overflow = parse("(workspace overflow (graph g :context value 1e999))");
    let (w, ds) = flow_check::workspace::check(&[], false, false, None, &catalog(), &overflow);
    assert!(w.is_none() && ds.iter().any(|d| d.code == "E_TYPE"));
}
