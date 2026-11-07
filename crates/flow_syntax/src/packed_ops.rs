use crate::{ty::Ty, value};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Binary { Add, Sub, Mul, Div, Mod, Pow, Min, Max, Lt, Le, Gt, Ge, Eq, And, Or, Atan2, Hypot, Ldexp }
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Unary { Sin, Cos, Sqrt, Abs, Not, Tan, Asin, Acos, Exp, Log, Sign, Rsqrt }
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Integer { Iadd, Isub, Imul, Idiv, Imod, Iand, Ior, Ixor, Ishl, Ishr }
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Comparison { Ilt, Ile, Igt, Ige, Ieq }
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Conversion { ToFloat, Truncate, Floor, Ceil, Round, CoerceInt, IntegerResult(String) }

pub const REGISTER_LIMIT: usize = 16_384;
pub fn supported_noise_octaves(n: i64) -> bool { (1..=64).contains(&n) }
pub fn supported_gpu_noise_octaves(n: i64) -> bool { (1..=32).contains(&n) }
pub fn finite_float32(x: f64) -> bool { (x as f32).is_finite() }

pub fn error_status(code: &str, message: &str) -> u32 {
    match (code, message) {
        ("E_LIST_RANGE", _) => 8,
        ("E_ARRAY_RANGE", "An array slice must be within its source.") => 16,
        ("E_ARRAY_RANGE", "array/slice needs a non-negative whole length.") => 32,
        _ => 1,
    }
}

pub fn scalar_width(ty: &Ty) -> Option<usize> {
    match ty { Ty::Float | Ty::Int | Ty::Bool => Some(1), Ty::Vec2 => Some(2), Ty::Vec3 => Some(3), Ty::Vec4 => Some(4), _ => None }
}
pub fn array_width(ty: &Ty) -> Option<usize> {
    match ty { Ty::Array(element) if matches!(element.as_ref(), Ty::Int | Ty::Float | Ty::Vec2 | Ty::Vec3 | Ty::Vec4) => scalar_width(element), _ => None }
}

const BINARIES: &[(&str, Binary)] = &[
    ("+", Binary::Add), ("-", Binary::Sub), ("*", Binary::Mul), ("/", Binary::Div), ("mod", Binary::Mod), ("pow", Binary::Pow), ("min", Binary::Min), ("max", Binary::Max),
    ("<", Binary::Lt), ("<=", Binary::Le), (">", Binary::Gt), (">=", Binary::Ge), ("=", Binary::Eq), ("and", Binary::And), ("or", Binary::Or), ("atan2", Binary::Atan2), ("hypot", Binary::Hypot), ("ldexp", Binary::Ldexp),
];
const UNARIES: &[(&str, Unary)] = &[
    ("sin", Unary::Sin), ("cos", Unary::Cos), ("sqrt", Unary::Sqrt), ("abs", Unary::Abs), ("not", Unary::Not), ("tan", Unary::Tan), ("asin", Unary::Asin), ("acos", Unary::Acos), ("exp", Unary::Exp), ("log", Unary::Log), ("sign", Unary::Sign), ("rsqrt", Unary::Rsqrt),
];
const INTEGERS: &[(&str, Integer)] = &[
    ("int/add", Integer::Iadd), ("int/sub", Integer::Isub), ("int/mul", Integer::Imul), ("int/div", Integer::Idiv), ("int/mod", Integer::Imod), ("int/and", Integer::Iand), ("int/or", Integer::Ior), ("int/xor", Integer::Ixor), ("int/shl", Integer::Ishl), ("int/shr", Integer::Ishr),
];
const COMPARISONS: &[(&str, Comparison)] = &[("int/lt", Comparison::Ilt), ("int/le", Comparison::Ile), ("int/gt", Comparison::Igt), ("int/ge", Comparison::Ige), ("int/eq", Comparison::Ieq)];
pub const NOISE_NAMES: &[&str] = &["noise3"];
const DERIVED_NAMES: &[&str] = &["length", "dot", "cross", "normalize", "fma"];

pub fn binary(name: &str) -> Option<Binary> { BINARIES.iter().find(|(n, _)| *n == name).map(|(_, op)| *op) }
pub fn unary(name: &str) -> Option<Unary> { UNARIES.iter().find(|(n, _)| *n == name).map(|(_, op)| *op) }
pub fn integer(name: &str) -> Option<Integer> { INTEGERS.iter().find(|(n, _)| *n == name).map(|(_, op)| *op) }
pub fn comparison(name: &str) -> Option<Comparison> { COMPARISONS.iter().find(|(n, _)| *n == name).map(|(_, op)| *op) }
pub fn integer_name(operation: Integer) -> &'static str { INTEGERS.iter().find(|(_, op)| *op == operation).expect("complete integer operation table").0 }
pub fn comparison_name(operation: Comparison) -> &'static str { COMPARISONS.iter().find(|(_, op)| *op == operation).expect("complete integer comparison table").0 }

#[inline]
pub fn integer_apply(operation: Integer, a: i64, b: i64) -> Result<i64, crate::Diagnostic> {
    let a = ocaml_int::wrap_int63(a);
    let b = ocaml_int::wrap_int63(b);
    if matches!(operation, Integer::Ishl | Integer::Ishr) && !(0..=62).contains(&b) {
        return Err(value::fail("E_RANGE", "Integer shift count must be within [0, 62]."));
    }
    let result = match operation {
        Integer::Iadd => ocaml_int::wrap_int63(a.wrapping_add(b)),
        Integer::Isub => ocaml_int::wrap_int63(a.wrapping_sub(b)),
        Integer::Imul => ocaml_int::wrap_int63(a.wrapping_mul(b)),
        Integer::Idiv if b == 0 => return Err(value::fail("E_RANGE", "Integer division by zero.")),
        Integer::Idiv => ocaml_int::wrap_int63(a / b),
        Integer::Imod if b == 0 => return Err(value::fail("E_RANGE", "Integer division by zero.")),
        Integer::Imod => ocaml_int::wrap_int63(a % b),
        Integer::Iand => ocaml_int::wrap_int63(a & b),
        Integer::Ior => ocaml_int::wrap_int63(a | b),
        Integer::Ixor => ocaml_int::wrap_int63(a ^ b),
        Integer::Ishl => ocaml_int::wrap_int63(a.wrapping_shl(b as u32)),
        Integer::Ishr => ocaml_int::lsr(a, b),
    };
    Ok(result)
}
#[inline]
pub fn comparison_apply(operation: Comparison, a: i64, b: i64) -> bool {
    let a = ocaml_int::wrap_int63(a);
    let b = ocaml_int::wrap_int63(b);
    match operation { Comparison::Ilt => a < b, Comparison::Ile => a <= b, Comparison::Igt => a > b, Comparison::Ige => a >= b, Comparison::Ieq => a == b }
}
pub fn rand_at(seed: i64, index: i64) -> f64 { rays_math::rand::float_at_int(seed, index) }

pub fn noise_names() -> Vec<String> { NOISE_NAMES.iter().map(|name| (*name).to_owned()).collect() }
pub fn names() -> Vec<String> {
    BINARIES.iter().map(|(name, _)| (*name).to_owned())
        .chain(UNARIES.iter().map(|(name, _)| (*name).to_owned()))
        .chain(NOISE_NAMES.iter().map(|name| (*name).to_owned()))
        .chain(DERIVED_NAMES.iter().map(|name| (*name).to_owned()))
        .chain(INTEGERS.iter().map(|(name, _)| (*name).to_owned()))
        .chain(COMPARISONS.iter().map(|(name, _)| (*name).to_owned()))
        .chain(std::iter::once("rand/at".to_owned())).collect()
}
pub fn supports(name: &str) -> bool { BINARIES.iter().any(|(n, _)| *n == name) || UNARIES.iter().any(|(n, _)| *n == name) || NOISE_NAMES.contains(&name) || DERIVED_NAMES.contains(&name) || INTEGERS.iter().any(|(n, _)| *n == name) || COMPARISONS.iter().any(|(n, _)| *n == name) || name == "rand/at" }

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn packed_names_widths_and_limits() {
        assert!(supports("noise3") && supports("rand/at") && !supports("exact"));
        assert_eq!(noise_names(), vec!["noise3"]);
        assert_eq!(names().len(), 52);
        assert_eq!(scalar_width(&Ty::Vec3), Some(3));
        assert_eq!(array_width(&Ty::Array(Box::new(Ty::Vec4))), Some(4));
        assert_eq!(array_width(&Ty::Array(Box::new(Ty::Bool))), None);
        assert!(supported_noise_octaves(64) && !supported_noise_octaves(65));
        assert!(supported_gpu_noise_octaves(32) && !supported_gpu_noise_octaves(33));
        assert!(finite_float32(1.0) && !finite_float32(f64::MAX));
        assert_eq!(error_status("E_ARRAY_RANGE", "An array slice must be within its source."), 16);
    }
    #[test]
    fn integer_operations_keep_ocaml_int_semantics() {
        assert_eq!(integer_apply(Integer::Iadd, (1_i64 << 62) - 1, 1).unwrap(), -(1_i64 << 62));
        assert_eq!(integer_apply(Integer::Ishl, 1, 62).unwrap(), -(1_i64 << 62));
        assert_eq!(integer_apply(Integer::Ishr, -1, 1).unwrap(), (1_i64 << 62) - 1);
        assert_eq!(integer_apply(Integer::Idiv, -7, 3).unwrap(), -2);
        assert_eq!(integer_apply(Integer::Idiv, 1, 0).unwrap_err().code, "E_RANGE");
        assert_eq!(integer_apply(Integer::Ishl, 1, 63).unwrap_err().code, "E_RANGE");
        assert!(comparison_apply(Comparison::Ilt, -1, 0));
        assert_eq!(integer_name(Integer::Ixor), "int/xor");
    }
}
