//! a node parameter port (node id, path) and the vec3 grouping of
//! a node's fields.
use flow_check::port_type::{self, PortType, PortValue};
use param::{FieldView, FloatRange, IntRange, Kind, KindView, Value};
use rays_diagnostic::Diagnostic;
use std::cmp::Ordering;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Port {
    pub node: i64,
    pub path: String,
}

/// Order ports by node ID, then unsigned path bytes.
pub fn compare(a: &Port, b: &Port) -> Ordering {
    a.node.cmp(&b.node).then_with(|| a.path.as_bytes().cmp(b.path.as_bytes()))
}

impl PartialOrd for Port {
    fn partial_cmp(&self, other: &Port) -> Option<Ordering> {
        Some(compare(self, other))
    }
}

impl Ord for Port {
    fn cmp(&self, other: &Port) -> Ordering {
        compare(self, other)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Scalar(Value),
    Vector(f64, f64, f64),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Parameter {
    pub path: String,
    pub fields: Vec<FieldView>,
    pub ty: Option<PortType>,
}

pub fn error(code: &str, message: &str) -> Diagnostic {
    rays_diagnostic::error(code.into(), message.into(), None, None, None, Vec::new(), Vec::new())
}

/// Group vec3 metadata, checking the same grouping invariants as the PPX. `infer` uses native
/// scene-schema suffix inference.
pub fn parameters(infer: bool, fields: &[FieldView]) -> Result<Vec<Parameter>, Diagnostic> {
    let groups = param::group_vec3(fields, infer).map_err(|message| error("E_VEC3", &message))?;
    Ok(groups
        .into_iter()
        .map(|fields| {
            let field = fields.first().expect("vec3 group is never empty").clone();
            let (path, ty) = match &field.vec3 {
                Some((name, _)) => (name.clone(), Some(PortType::Vec3)),
                None => (
                    field.name.clone(),
                    match (&field.kind, &field.current) {
                        (KindView::Choice(_), Value::Int(_)) => Some(PortType::Int),
                        _ => port_type::of_field_kind(&field.kind),
                    },
                ),
            };
            Parameter { path, fields, ty }
        })
        .collect())
}

fn components(parameter: &Parameter) -> Vec<Parameter> {
    match (&parameter.ty, parameter.fields.as_slice()) {
        (Some(PortType::Vec3), [x, y, z]) => ["x", "y", "z"]
            .iter()
            .zip([x, y, z])
            .map(|(axis, field)| Parameter {
                path: format!("{}.{}", parameter.path, axis),
                fields: vec![field.clone()],
                ty: Some(PortType::Float),
            })
            .collect(),
        _ => Vec::new(),
    }
}

pub fn find_parameter(parameters: &[Parameter], path: &str) -> Result<Parameter, Diagnostic> {
    if let Some(found) = parameters.iter().find(|parameter| parameter.path == path) {
        return Ok(found.clone());
    }
    for parameter in parameters {
        if let Some(found) = components(parameter).into_iter().find(|c| c.path == path) {
            return Ok(found);
        }
    }
    Err(error("E_PORT", &format!("No parameter port {path}")))
}

fn number(field: &FieldView) -> f64 {
    match &field.current {
        Value::Float(value) => *value,
        _ => unreachable!("vec3 component is a float field"),
    }
}

pub fn literal(parameter: &Parameter) -> Literal {
    match parameter.fields.as_slice() {
        [field] => Literal::Scalar(field.current.clone()),
        [x, y, z] => Literal::Vector(number(x), number(y), number(z)),
        _ => unreachable!("a port has one field or a vec3 group"),
    }
}

pub fn literal_changes(parameter: &Parameter, value: &Literal) -> Result<Vec<(String, Value)>, Diagnostic> {
    match (parameter.fields.as_slice(), value) {
        ([field], Literal::Scalar(value)) => Ok(vec![(field.name.clone(), value.clone())]),
        ([x, y, z], Literal::Vector(a, b, c)) => Ok(vec![
            (x.name.clone(), Value::Float(*a)),
            (y.name.clone(), Value::Float(*b)),
            (z.name.clone(), Value::Float(*c)),
        ]),
        _ => Err(error("E_TYPE", &format!("Literal does not match port {}", parameter.path))),
    }
}

fn type_error(message: String) -> Diagnostic {
    error("E_TYPE", &message)
}

fn normalize_float(field: &FieldView, number: f64) -> Result<f64, Diagnostic> {
    match &field.kind {
        KindView::Floating(range) => {
            match param::normalize_value(&Kind::Floating(FloatRange { ..*range }), &Value::Float(number)) {
                Ok(Value::Float(value)) => Ok(value),
                Ok(_) => unreachable!("a float stays a float"),
                Err(message) => Err(type_error(format!("{}: {}", field.name, message))),
            }
        }
        _ => Err(type_error(format!("Not a float field: {}", field.name))),
    }
}

fn normalize_int(kind: Kind, value: i64) -> Result<i64, Diagnostic> {
    match param::normalize_value(&kind, &Value::Int(value)) {
        Ok(Value::Int(value)) => Ok(value),
        Ok(_) => unreachable!("an int stays an int"),
        Err(message) => Err(type_error(message)),
    }
}

/// Coerce and use `Param.normalize_value` for hard bounds, matching writes through `Param.apply`.
/// The returned value is the effective live readout.
pub fn normalize(parameter: &Parameter, value: &PortValue) -> Result<(PortValue, Vec<(String, Value)>), Diagnostic> {
    let Some(target) = &parameter.ty else {
        return Err(error("E_TYPE", &format!("Port {} is literal-only", parameter.path)));
    };
    let value = port_type::coerce(target, *value)?;
    let mismatch = || error("E_TYPE", &format!("Value does not match port {}", parameter.path));
    match (parameter.fields.as_slice(), value) {
        ([field], PortValue::Float(value)) => {
            let value = normalize_float(field, value)?;
            Ok((PortValue::Float(value), vec![(field.name.clone(), Value::Float(value))]))
        }
        ([field], PortValue::Int(value)) => match &field.kind {
            KindView::Integer(range) => {
                let value = normalize_int(Kind::Integer(IntRange { ..*range }), value)?;
                Ok((PortValue::Int(value), vec![(field.name.clone(), Value::Int(value))]))
            }
            KindView::Choice(labels) if matches!(field.current, Value::Int(_)) => {
                let value = normalize_int(Kind::IndexChoice(labels.clone()), value)?;
                Ok((PortValue::Int(value), vec![(field.name.clone(), Value::Int(value))]))
            }
            _ => Err(error("E_TYPE", &format!("Not an integer field: {}", field.name))),
        },
        ([field], PortValue::Bool(value)) => Ok((PortValue::Bool(value), vec![(field.name.clone(), Value::Bool(value))])),
        ([x, y, z], PortValue::Vec3(a, b, c)) => {
            let a = normalize_float(x, a)?;
            let b = normalize_float(y, b)?;
            let c = normalize_float(z, c)?;
            Ok((
                PortValue::Vec3(a, b, c),
                vec![
                    (x.name.clone(), Value::Float(a)),
                    (y.name.clone(), Value::Float(b)),
                    (z.name.clone(), Value::Float(c)),
                ],
            ))
        }
        _ => Err(mismatch()),
    }
}

/// `"#rgb"`, `"#rrggbb"` or `"#rrggbbaa"` (alpha ignored) as a vec3.
pub fn color_of_text(text: &str) -> Option<(f64, f64, f64)> {
    let bytes = text.as_bytes();
    let digit = |c: u8| -> Option<i64> {
        match c {
            b'0'..=b'9' => Some((c - b'0') as i64),
            b'a'..=b'f' => Some((c - b'a') as i64 + 10),
            b'A'..=b'F' => Some((c - b'A') as i64 + 10),
            _ => None,
        }
    };
    let n = bytes.len();
    let channel = |i: usize| -> Option<f64> {
        match n {
            4 => digit(bytes[i]).map(|d| (17 * d) as f64 / 255.0),
            7 | 9 => match (digit(bytes[2 * i - 1]), digit(bytes[2 * i])) {
                (Some(a), Some(b)) => Some(((16 * a) + b) as f64 / 255.0),
                _ => None,
            },
            _ => None,
        }
    };
    if n == 0 || bytes[0] != b'#' {
        return None;
    }
    match (channel(1), channel(2), channel(3)) {
        (Some(r), Some(g), Some(b)) => Some((r, g, b)),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn colors() {
        assert_eq!(color_of_text("#fff"), Some((1.0, 1.0, 1.0)));
        assert_eq!(color_of_text("#ff000080"), Some((1.0, 0.0, 0.0)));
        assert_eq!(color_of_text("ff0000"), None);
        assert_eq!(color_of_text("#ff00"), None);
        assert_eq!(color_of_text("#gg0000"), None);
    }

    #[test]
    fn port_order_is_node_then_bytes() {
        let a = Port { node: 1, path: "b".into() };
        let b = Port { node: 2, path: "a".into() };
        assert!(a < b);
        let mut map = std::collections::BTreeMap::new();
        map.insert(b, 1);
        map.insert(a.clone(), 2);
        assert_eq!(map.keys().next(), Some(&a));
    }
}
