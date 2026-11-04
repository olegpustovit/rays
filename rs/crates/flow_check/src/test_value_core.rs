// Port of lib/flow/test_value_core.ml, assertion for assertion. The OCaml file reaches Value,
// Context and Symbol (flow_syntax) and Port_type and Param (here), so it lives in flow_check.
use crate::port_type::{can_connect, coerce, of_field_kind, PortType, PortValue};
use flow_syntax::context;
use flow_syntax::symbol;
use flow_syntax::ty;
use flow_syntax::value::{self, Value};
use param::KindView;

const OCAML_MAX_INT: i64 = (1 << 62) - 1;
const OCAML_MIN_INT: i64 = -(1 << 62);

#[test]
fn value_keys_contexts_and_symbols() {
    assert_eq!(value::key_of(&|id| id.0, &Value::Deferred(ty::image(), 3)), b"image:3".to_vec());
    assert_ne!(
        value::key_of(&|id| id.0, &Value::Deferred(ty::image(), 3)),
        value::key_of(&|id| id.0, &Value::Int(3))
    );
    for context in [context::sop(), context::value(), context::scene(), context::world(), context::settings()] {
        assert_eq!(context::of_string(context::name(context)), Ok(context));
    }
    assert!(context::supports_values(context::sop()) && !context::supports_values(context::world()));
    assert!(context::of_string("shader").is_err());
    assert!(symbol::valid_name("uv_sphere"));
    for name in ["", "9a", "A", "a-b", "a.b", "a/b"] {
        assert!(!symbol::valid_name(name));
    }
}

#[test]
fn coercions_follow_the_port_rules() {
    let cases = [
        (PortType::Float, PortValue::Int(3), PortValue::Float(3.0)),
        (PortType::Int, PortValue::Float(1.5), PortValue::Int(2)),
        (PortType::Int, PortValue::Float(-1.5), PortValue::Int(-1)),
        (PortType::Bool, PortValue::Float(-0.0), PortValue::Bool(false)),
        (PortType::Bool, PortValue::Int(-1), PortValue::Bool(true)),
        (PortType::Float, PortValue::Bool(true), PortValue::Float(1.0)),
        (PortType::Int, PortValue::Bool(false), PortValue::Int(0)),
        (PortType::Vec3, PortValue::Bool(true), PortValue::Vec3(1.0, 1.0, 1.0)),
        (PortType::Vec3, PortValue::Int(2), PortValue::Vec3(2.0, 2.0, 2.0)),
        (PortType::Vec3, PortValue::Float(3.0), PortValue::Vec3(3.0, 3.0, 3.0)),
        (PortType::Int, PortValue::Float(1e100), PortValue::Int(OCAML_MAX_INT)),
        (PortType::Int, PortValue::Float(-1e100), PortValue::Int(OCAML_MIN_INT)),
        (PortType::Int, PortValue::Int(OCAML_MAX_INT), PortValue::Int(OCAML_MAX_INT)),
        (PortType::Vec3, PortValue::Vec3(1.0, 2.0, 3.0), PortValue::Vec3(1.0, 2.0, 3.0)),
    ];
    for (target, input, expected) in cases {
        assert_eq!(coerce(&target, input), Ok(expected));
    }
    for target in [PortType::Float, PortType::Int, PortType::Bool, PortType::Geometry] {
        assert!(coerce(&target, PortValue::Vec3(1.0, 2.0, 3.0)).is_err());
    }
    for number in [f64::NAN, f64::INFINITY, f64::NEG_INFINITY] {
        assert!(coerce(&PortType::Int, PortValue::Float(number)).is_err());
    }
    assert!(can_connect(&PortType::Geometry, &PortType::Geometry));
    assert!(!can_connect(&PortType::Geometry, &PortType::Float));
    assert_eq!(of_field_kind(&KindView::Toggle), Some(PortType::Bool));
    assert_eq!(of_field_kind(&KindView::Choice(vec!["a".into()])), None);
    assert_eq!(of_field_kind(&KindView::Text), None);
}

#[test]
fn hard_bounds_clamp_a_coerced_integer() {
    let schema = param::schema(
        "bounded".into(),
        vec![param::field(
            "v".into(),
            param::integer(0, 4, Some(0), Some(4)).unwrap(),
            param::Value::Int(0),
        )
        .unwrap()],
    )
    .unwrap();
    let value = match coerce(&PortType::Int, PortValue::Float(1e100)).unwrap() {
        PortValue::Int(value) => value,
        _ => panic!("Int expected"),
    };
    let (bounded, _) = schema.apply_all(&[param::Value::Int(0)], &[("v".into(), param::Value::Int(value))]).unwrap();
    assert_eq!(bounded, vec![param::Value::Int(4)]);
}
