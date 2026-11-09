#![allow(non_camel_case_types)]
pub use rays_math::float::{min as float_min, max as float_max};
use crate::{ty::Ty, value, Diagnostic};

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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Reduction { Sum, Minimum, Maximum }

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ByteMode { Clamped, Unclamped }

#[derive(Clone, Debug, PartialEq)]
pub enum Primitive {
    Numeric(Binary),
    Math(Unary),
    Remainder,
    Floor,
    Ceil,
    Round,
    Int_convert,
    Float_convert,
    Integer(Integer),
    Integer_compare(Comparison),
    Equal,
    Show,
    Text_concat,
    Text_blank,
    Text_trim,
    Text_lower,
    Text_repr,
    Text_table_decode,
    Text_table_encode,
    Text_split,
    Text_tokens,
    Text_to_float,
    Text_to_int,
    Sequence_length,
    Sequence_get,
    Sequence_build,
    Sequence_slice,
    Sequence_concat,
    Array_max_length,
    Array_range,
    Array_int,
    Array_text,
    Array_float,
    Array_vec3,
    Array_vec4,
    Array_nth,
    Array_count,
    Array_slice,
    Array_concat,
    Array_reduce(Reduction),
    Array_segment(Reduction),
    Array_segment_argmax,
    Next_after,
    Array_scatter_add,
    Array_compact,
    Array_scan_sum,
    Array_grade,
    Array_index_of,
    Rand,
    Rand_at,
    Fma,
    Checked_mul,
    Checked_fma,
    Noise3,
    Color_parse,
    Color_byte(ByteMode),
    Coerce(Ty),
    Truthy,
    Error,
    Fail(String, String),
    Fail_diagnostic(Diagnostic),
}

pub const PRIMITIVES: &[(&str, Primitive)] = &[
    ("+", Primitive::Numeric(Binary::Add)),
    ("-", Primitive::Numeric(Binary::Sub)),
    ("*", Primitive::Numeric(Binary::Mul)),
    ("/", Primitive::Numeric(Binary::Div)),
    ("mod", Primitive::Numeric(Binary::Mod)),
    ("pow", Primitive::Numeric(Binary::Pow)),
    ("min", Primitive::Numeric(Binary::Min)),
    ("max", Primitive::Numeric(Binary::Max)),
    ("<", Primitive::Numeric(Binary::Lt)),
    ("<=", Primitive::Numeric(Binary::Le)),
    (">", Primitive::Numeric(Binary::Gt)),
    (">=", Primitive::Numeric(Binary::Ge)),
    ("=", Primitive::Numeric(Binary::Eq)),
    ("and", Primitive::Numeric(Binary::And)),
    ("or", Primitive::Numeric(Binary::Or)),
    ("atan2", Primitive::Numeric(Binary::Atan2)),
    ("hypot", Primitive::Numeric(Binary::Hypot)),
    ("ldexp", Primitive::Numeric(Binary::Ldexp)),
    ("sin", Primitive::Math(Unary::Sin)),
    ("cos", Primitive::Math(Unary::Cos)),
    ("sqrt", Primitive::Math(Unary::Sqrt)),
    ("abs", Primitive::Math(Unary::Abs)),
    ("not", Primitive::Math(Unary::Not)),
    ("tan", Primitive::Math(Unary::Tan)),
    ("asin", Primitive::Math(Unary::Asin)),
    ("acos", Primitive::Math(Unary::Acos)),
    ("exp", Primitive::Math(Unary::Exp)),
    ("log", Primitive::Math(Unary::Log)),
    ("sign", Primitive::Math(Unary::Sign)),
    ("rsqrt", Primitive::Math(Unary::Rsqrt)),
    ("int/add", Primitive::Integer(Integer::Iadd)),
    ("int/sub", Primitive::Integer(Integer::Isub)),
    ("int/mul", Primitive::Integer(Integer::Imul)),
    ("int/div", Primitive::Integer(Integer::Idiv)),
    ("int/mod", Primitive::Integer(Integer::Imod)),
    ("int/and", Primitive::Integer(Integer::Iand)),
    ("int/or", Primitive::Integer(Integer::Ior)),
    ("int/xor", Primitive::Integer(Integer::Ixor)),
    ("int/shl", Primitive::Integer(Integer::Ishl)),
    ("int/shr", Primitive::Integer(Integer::Ishr)),
    ("int/lt", Primitive::Integer_compare(Comparison::Ilt)),
    ("int/le", Primitive::Integer_compare(Comparison::Ile)),
    ("int/gt", Primitive::Integer_compare(Comparison::Igt)),
    ("int/ge", Primitive::Integer_compare(Comparison::Ige)),
    ("int/eq", Primitive::Integer_compare(Comparison::Ieq)),
    ("remainder", Primitive::Remainder),
    ("floor", Primitive::Floor),
    ("ceil", Primitive::Ceil),
    ("round", Primitive::Round),
    ("int", Primitive::Int_convert),
    ("float", Primitive::Float_convert),
    ("equal?", Primitive::Equal),
    ("show", Primitive::Show),
    ("text/concat", Primitive::Text_concat),
    ("text/blank?", Primitive::Text_blank),
    ("text/trim", Primitive::Text_trim),
    ("text/lower", Primitive::Text_lower),
    ("text/repr", Primitive::Text_repr),
    ("text/table_decode", Primitive::Text_table_decode),
    ("text/table_encode", Primitive::Text_table_encode),
    ("text/split", Primitive::Text_split),
    ("text/tokens", Primitive::Text_tokens),
    ("text/to_float", Primitive::Text_to_float),
    ("text/to_int", Primitive::Text_to_int),
    ("array/sum", Primitive::Array_reduce(Reduction::Sum)),
    ("array/min", Primitive::Array_reduce(Reduction::Minimum)),
    ("array/max", Primitive::Array_reduce(Reduction::Maximum)),
    ("array/segment_sum", Primitive::Array_segment(Reduction::Sum)),
    ("array/segment_min", Primitive::Array_segment(Reduction::Minimum)),
    ("array/segment_max", Primitive::Array_segment(Reduction::Maximum)),
    ("array/segment_argmax", Primitive::Array_segment_argmax),
    ("float/next_after", Primitive::Next_after),
    ("array/grade", Primitive::Array_grade),
    ("array/index_of", Primitive::Array_index_of),
    ("array/compact", Primitive::Array_compact),
    ("array/scan_sum", Primitive::Array_scan_sum),
    ("array/scatter_add", Primitive::Array_scatter_add),
    ("sequence/length", Primitive::Sequence_length),
    ("sequence/get", Primitive::Sequence_get),
    ("sequence/build", Primitive::Sequence_build),
    ("sequence/slice", Primitive::Sequence_slice),
    ("sequence/concat", Primitive::Sequence_concat),
    ("array/max_length", Primitive::Array_max_length),
    ("array/range", Primitive::Array_range),
    ("array/int", Primitive::Array_int),
    ("array/float", Primitive::Array_float),
    ("array/vec3", Primitive::Array_vec3),
    ("array/vec4", Primitive::Array_vec4),
    ("array/text", Primitive::Array_text),
    ("array/nth", Primitive::Array_nth),
    ("array/count", Primitive::Array_count),
    ("array/slice", Primitive::Array_slice),
    ("array/concat", Primitive::Array_concat),
    ("value/rand", Primitive::Rand),
    ("rand/at", Primitive::Rand_at),
    ("fma", Primitive::Fma),
    ("noise3", Primitive::Noise3),
    ("color/parse", Primitive::Color_parse),
    ("float/mul_checked", Primitive::Checked_mul),
    ("float/fma_checked", Primitive::Checked_fma),
    ("truthy", Primitive::Truthy),
    ("error", Primitive::Error),
];

pub fn primitive(name: &str) -> Option<Primitive> { PRIMITIVES.iter().find(|(n, _)| *n == name).map(|(_, op)| op.clone()) }
pub fn primitive_name(operation: &Primitive) -> &'static str {
    match operation {
        Primitive::Color_byte(_) => "color/byte",
        Primitive::Coerce(_) => "coerce",
        Primitive::Fail(..) | Primitive::Fail_diagnostic(_) => "fail",
        _ => PRIMITIVES.iter().find(|(_, op)| op == operation).expect("complete primitive table").0,
    }
}
pub fn binary(name: &str) -> Option<Binary> { if let Some(Primitive::Numeric(op)) = primitive(name) { Some(op) } else { None } }
pub fn unary(name: &str) -> Option<Unary> { if let Some(Primitive::Math(op)) = primitive(name) { Some(op) } else { None } }
pub fn integer(name: &str) -> Option<Integer> { if let Some(Primitive::Integer(op)) = primitive(name) { Some(op) } else { None } }
pub fn comparison(name: &str) -> Option<Comparison> { if let Some(Primitive::Integer_compare(op)) = primitive(name) { Some(op) } else { None } }
pub fn integer_name(operation: Integer) -> &'static str { primitive_name(&Primitive::Integer(operation)) }
pub fn comparison_name(operation: Comparison) -> &'static str { primitive_name(&Primitive::Integer_compare(operation)) }

#[inline]
pub fn integer_apply(operation: Integer, a: i64, b: i64) -> Result<i64, crate::Diagnostic> {
    if matches!(operation, Integer::Ishl | Integer::Ishr) && !(0..=63).contains(&b) {
        return Err(value::fail("E_RANGE", "Integer shift count must be within [0, 63]."));
    }
    Ok(match operation {
        Integer::Iadd => a.wrapping_add(b),
        Integer::Isub => a.wrapping_sub(b),
        Integer::Imul => a.wrapping_mul(b),
        Integer::Idiv if b == 0 => return Err(value::fail("E_RANGE", "Integer division by zero.")),
        Integer::Idiv => a.wrapping_div(b),
        Integer::Imod if b == 0 => return Err(value::fail("E_RANGE", "Integer division by zero.")),
        Integer::Imod => a.wrapping_rem(b),
        Integer::Iand => a & b,
        Integer::Ior => a | b,
        Integer::Ixor => a ^ b,
        Integer::Ishl => a.wrapping_shl(b as u32),
        Integer::Ishr => ((a as u64) >> b) as i64,
    })
}
#[inline]
pub fn comparison_apply(operation: Comparison, a: i64, b: i64) -> bool {
    match operation { Comparison::Ilt => a < b, Comparison::Ile => a <= b, Comparison::Igt => a > b, Comparison::Ige => a >= b, Comparison::Ieq => a == b }
}

/// `ldexp` with the exponent truncated toward zero (`i64`), matching the host `ldexp` op.
pub fn ldexp(value: f64, exponent: i64) -> f64 {
    let bits = value.to_bits();
    let sign = bits & 0x8000_0000_0000_0000;
    let encoded = ((bits >> 52) & 0x7ff) as i32;
    let fraction = bits & 0x000f_ffff_ffff_ffff;
    if encoded == 0x7ff || value == 0.0 {
        return value;
    }
    let (significand, base) = if encoded == 0 {
        let high = 63 - fraction.leading_zeros() as i32;
        let shift = 52 - high;
        (fraction << shift, high - 1074)
    } else {
        (fraction | (1 << 52), encoded - 1023)
    };
    let target = base as i128 + exponent as i128;
    if target > 1023 {
        return f64::from_bits(sign | (0x7ff << 52));
    }
    if target >= -1022 {
        return f64::from_bits(sign | (((target + 1023) as u64) << 52) | (significand & 0x000f_ffff_ffff_ffff));
    }
    if target < -1075 {
        return f64::from_bits(sign);
    }
    let shift = (-1022 - target) as u32;
    let quotient = significand >> shift;
    let remainder = significand & ((1u64 << shift) - 1);
    let halfway = 1u64 << (shift - 1);
    let rounded = quotient + if remainder > halfway || (remainder == halfway && quotient & 1 != 0) { 1 } else { 0 };
    f64::from_bits(sign | if rounded == (1u64 << 52) { 1u64 << 52 } else { rounded })
}

#[inline]
pub fn binary_apply(op: Binary, x: f64, y: f64) -> f64 {
    match op {
        Binary::Add => x + y,
        Binary::Sub => x - y,
        Binary::Mul => x * y,
        Binary::Div => if y == 0.0 { 0.0 } else { x / y },
        Binary::Mod => if y == 0.0 { 0.0 } else { ((x % y) + y) % y },
        Binary::Pow => x.abs().powf(y),
        Binary::Min => float_min(x, y),
        Binary::Max => float_max(x, y),
        Binary::Lt => if x < y { 1.0 } else { 0.0 },
        Binary::Le => if x <= y { 1.0 } else { 0.0 },
        Binary::Gt => if x > y { 1.0 } else { 0.0 },
        Binary::Ge => if x >= y { 1.0 } else { 0.0 },
        Binary::Eq => if x == y { 1.0 } else { 0.0 },
        Binary::And => if x != 0.0 && y != 0.0 { 1.0 } else { 0.0 },
        Binary::Or => if x != 0.0 || y != 0.0 { 1.0 } else { 0.0 },
        Binary::Atan2 => rays_math::libm::atan2(x, y),
        Binary::Hypot => x.hypot(y),
        Binary::Ldexp => ldexp(x, y.trunc() as i64),
    }
}

#[inline]
pub fn unary_apply(op: Unary, x: f64) -> f64 {
    match op {
        Unary::Sin => rays_math::libm::sin(x),
        Unary::Cos => rays_math::libm::cos(x),
        Unary::Sqrt => x.abs().sqrt(),
        Unary::Abs => x.abs(),
        Unary::Not => if x == 0.0 { 1.0 } else { 0.0 },
        Unary::Tan => rays_math::libm::tan(x),
        Unary::Asin => rays_math::libm::asin(x),
        Unary::Acos => rays_math::libm::acos(x),
        Unary::Exp => x.exp(),
        Unary::Log => x.ln(),
        Unary::Sign => if x > 0.0 { 1.0 } else if x < 0.0 { -1.0 } else { x },
        Unary::Rsqrt => 1.0 / x.abs().sqrt(),
    }
}

pub fn rand_at(seed: i64, index: i64) -> f64 { rays_math::rand::float_at_int(seed, index) }

const DERIVED_NAMES: &[&str] = &["length", "dot", "cross", "normalize"];
pub fn noise_names() -> Vec<String> { vec![primitive_name(&Primitive::Noise3).to_owned()] }
pub fn names() -> Vec<String> {
    let names = |filter: fn(&Primitive) -> bool| PRIMITIVES.iter().filter(move |(_, op)| filter(op)).map(|(name, _)| (*name).to_owned());
    names(|op| matches!(op, Primitive::Numeric(_) | Primitive::Math(_)))
        .chain(noise_names())
        .chain(DERIVED_NAMES.iter().map(|name| (*name).to_owned()))
        .chain(std::iter::once(primitive_name(&Primitive::Fma).to_owned()))
        .chain(names(|op| matches!(op, Primitive::Integer(_) | Primitive::Integer_compare(_))))
        .chain(std::iter::once(primitive_name(&Primitive::Rand_at).to_owned())).collect()
}
pub fn supports(name: &str) -> bool {
    DERIVED_NAMES.contains(&name) || matches!(primitive(name), Some(Primitive::Numeric(_) | Primitive::Math(_) | Primitive::Integer(_) | Primitive::Integer_compare(_) | Primitive::Noise3 | Primitive::Fma | Primitive::Rand_at))
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn primitive_names_round_trip_without_aliases() {
        let mut names = std::collections::HashSet::new();
        for (name, operation) in PRIMITIVES {
            assert!(names.insert(*name), "duplicate primitive {name}");
            assert_eq!(primitive(name).as_ref(), Some(operation), "{name}");
            assert_eq!(primitive_name(operation), *name);
        }
        assert!(primitive("unknown").is_none());
        assert!(binary("text/trim").is_none());
        assert_eq!(primitive_name(&Primitive::Coerce(Ty::Int)), "coerce");
        assert_eq!(primitive_name(&Primitive::Color_byte(ByteMode::Unclamped)), "color/byte");
    }

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
    fn integer_operations_are_native_i64() {
        let apply = |op, a, b| integer_apply(op, a, b).unwrap();
        assert_eq!(apply(Integer::Iadd, i64::MAX, 1), i64::MIN);
        assert_eq!(apply(Integer::Idiv, i64::MIN, -1), i64::MIN);
        assert_eq!(apply(Integer::Imod, i64::MIN, -1), 0);
        assert_eq!(apply(Integer::Ishl, 1, 63), i64::MIN);
        assert_eq!(apply(Integer::Ishr, -1, 1), i64::MAX);
        assert_eq!(apply(Integer::Idiv, -7, 3), -2);
        assert_eq!(integer_apply(Integer::Idiv, 1, 0).unwrap_err().code, "E_RANGE");
        assert_eq!(integer_apply(Integer::Ishl, 1, 64).unwrap_err().code, "E_RANGE");
        assert!(comparison_apply(Comparison::Ilt, -1, 0));
        assert_eq!(integer_name(Integer::Ixor), "int/xor");
    }
    #[test]
    fn float_min_max_signed_zeros() {
        assert!(float_min(-0.0, 0.0).is_sign_negative());
        assert!(float_max(0.0, -0.0).is_sign_positive());
        assert!(float_min(f64::NAN, 1.0).is_nan());
        assert!(float_max(1.0, f64::NAN).is_nan());
    }
}
