use flow_syntax::ty::{self, FnSignature, Ty};
use param::KindView;
use rays_diagnostic::{self, Diagnostic};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PortType {
    Geometry,
    Float,
    Int,
    Bool,
    Vec3,
    Image,
    Fn(FnSignature),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum PortValue {
    Float(f64),
    Int(i64),
    Bool(bool),
    Vec3(f64, f64, f64),
}

pub fn name(ty: &PortType) -> String {
    match ty {
        PortType::Geometry => "Geometry".into(),
        PortType::Float => "Float".into(),
        PortType::Int => "Int".into(),
        PortType::Bool => "Bool".into(),
        PortType::Vec3 => "Vec3".into(),
        PortType::Image => "Image".into(),
        PortType::Fn(signature) => ty::to_string(&Ty::Fn(Some(Box::new(signature.clone())))),
    }
}

pub fn of_field_kind(kind: &KindView) -> Option<PortType> {
    match kind {
        KindView::Floating(_) => Some(PortType::Float),
        KindView::Integer(_) => Some(PortType::Int),
        KindView::Toggle => Some(PortType::Bool),
        KindView::Text | KindView::Choice(_) => None,
    }
}

pub fn value_type(value: &PortValue) -> PortType {
    match value {
        PortValue::Float(_) => PortType::Float,
        PortValue::Int(_) => PortType::Int,
        PortValue::Bool(_) => PortType::Bool,
        PortValue::Vec3(..) => PortType::Vec3,
    }
}

pub fn can_connect(source: &PortType, target: &PortType) -> bool {
    match (source, target) {
        (PortType::Geometry, PortType::Geometry)
        | (PortType::Vec3, PortType::Vec3)
        | (PortType::Image, PortType::Image) => true,
        (PortType::Fn(source), PortType::Fn(target)) => {
            source.params == target.params && ty::fits(&source.result, &target.result)
        }
        (PortType::Image | PortType::Fn(_), _) | (_, PortType::Image | PortType::Fn(_)) => false,
        (PortType::Geometry, _) | (_, PortType::Geometry) | (PortType::Vec3, _) => false,
        (PortType::Float | PortType::Int | PortType::Bool,
         PortType::Float | PortType::Int | PortType::Bool | PortType::Vec3) => true,
    }
}

fn error(message: String) -> Diagnostic {
    rays_diagnostic::error("E_TYPE".into(), message, None, None, None, Vec::new(), Vec::new())
}

pub fn coerce(target: &PortType, value: PortValue) -> Result<PortValue, Diagnostic> {
    let source = value_type(&value);
    if !can_connect(&source, target) {
        return Err(error(format!("Cannot drive {} with {}", name(target), name(&source))));
    }
    if source == *target {
        return Ok(value);
    }
    let number = match value {
        PortValue::Float(x) => x,
        PortValue::Int(x) => x as f64,
        PortValue::Bool(x) => if x { 1.0 } else { 0.0 },
        PortValue::Vec3(..) => return Err(error(format!("Cannot drive {} with Vec3", name(target)))),
    };
    match target {
        PortType::Float => Ok(PortValue::Float(number)),
        PortType::Bool => Ok(PortValue::Bool(number != 0.0)),
        PortType::Vec3 => Ok(PortValue::Vec3(number, number, number)),
        PortType::Int => {
            if !number.is_finite() {
                return Err(error("A non-finite value cannot drive Int".into()));
            }
            let rounded = (number + 0.5).floor();
            const OCAML_INT_MAX: i64 = (1_i64 << 62) - 1;
            const OCAML_INT_MIN: i64 = -(1_i64 << 62);
            let value = if rounded >= OCAML_INT_MAX as f64 { OCAML_INT_MAX }
                else if rounded <= OCAML_INT_MIN as f64 { OCAML_INT_MIN }
                else { rounded as i64 };
            Ok(PortValue::Int(value))
        }
        PortType::Geometry | PortType::Image | PortType::Fn(_) => {
            Err(error(format!("Cannot drive {} with {}", name(target), name(&source))))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn coercion_rounds_ties_saturates_and_rejects_non_finite() {
        assert_eq!(coerce(&PortType::Int, PortValue::Float(1.5)), Ok(PortValue::Int(2)));
        assert_eq!(coerce(&PortType::Int, PortValue::Float(-1.5)), Ok(PortValue::Int(-1)));
        assert_eq!(coerce(&PortType::Int, PortValue::Float(f64::INFINITY)).unwrap_err().code, "E_TYPE");
        assert_eq!(coerce(&PortType::Int, PortValue::Float(f64::MAX)), Ok(PortValue::Int((1_i64 << 62) - 1)));
    }

    #[test]
    fn scalar_broadcast_and_non_numeric_connections() {
        assert_eq!(coerce(&PortType::Vec3, PortValue::Bool(true)), Ok(PortValue::Vec3(1.0, 1.0, 1.0)));
        assert!(!can_connect(&PortType::Image, &PortType::Float));
        assert_eq!(of_field_kind(&KindView::Text), None);
    }
}
