
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
    IntConvert,
    FloatConvert,
    Integer(Integer),
    IntegerCompare(Comparison),
    Equal,
    Show,
    TextConcat,
    TextBlank,
    TextTrim,
    TextLower,
    TextRepr,
    TextTableDecode,
    TextTableEncode,
    TextSplit,
    TextTokens,
    TextToFloat,
    TextToInt,
    SequenceLength,
    SequenceGet,
    SequenceBuild,
    SequenceSlice,
    SequenceConcat,
    ArrayMaxLength,
    ArrayRange,
    ArrayInt,
    ArrayText,
    ArrayFloat,
    ArrayVec3,
    ArrayVec4,
    ArrayNth,
    ArrayCount,
    ArraySlice,
    ArrayConcat,
    ArrayReduce(Reduction),
    ArraySegment(Reduction),
    ArraySegmentArgmax,
    NextAfter,
    ArrayScatterAdd,
    ArrayCompact,
    ArrayScanSum,
    ArrayGrade,
    ArrayIndexOf,
    Rand,
    RandAt,
    Fma,
    CheckedMul,
    CheckedFma,
    Noise3,
    ColorParse,
    ColorByte(ByteMode),
    Coerce(Ty),
    Truthy,
    Error,
    Fail(String, String),
    FailDiagnostic(Box<Diagnostic>),
}

pub const PRIMITIVES: &[(&str, Primitive)] = &[
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
    ("int/lt", Primitive::IntegerCompare(Comparison::Ilt)),
    ("int/le", Primitive::IntegerCompare(Comparison::Ile)),
    ("int/gt", Primitive::IntegerCompare(Comparison::Igt)),
    ("int/ge", Primitive::IntegerCompare(Comparison::Ige)),
    ("int/eq", Primitive::IntegerCompare(Comparison::Ieq)),
    ("remainder", Primitive::Remainder),
    ("floor", Primitive::Floor),
    ("ceil", Primitive::Ceil),
    ("round", Primitive::Round),
    ("int", Primitive::IntConvert),
    ("float", Primitive::FloatConvert),
    ("equal?", Primitive::Equal),
    ("show", Primitive::Show),
    ("text/concat", Primitive::TextConcat),
    ("text/blank?", Primitive::TextBlank),
    ("text/trim", Primitive::TextTrim),
    ("text/lower", Primitive::TextLower),
    ("text/repr", Primitive::TextRepr),
    ("text/table_decode", Primitive::TextTableDecode),
    ("text/table_encode", Primitive::TextTableEncode),
    ("text/split", Primitive::TextSplit),
    ("text/tokens", Primitive::TextTokens),
    ("text/to_float", Primitive::TextToFloat),
    ("text/to_int", Primitive::TextToInt),
    ("array/sum", Primitive::ArrayReduce(Reduction::Sum)),
    ("array/min", Primitive::ArrayReduce(Reduction::Minimum)),
    ("array/max", Primitive::ArrayReduce(Reduction::Maximum)),
    ("array/segment_sum", Primitive::ArraySegment(Reduction::Sum)),
    ("array/segment_min", Primitive::ArraySegment(Reduction::Minimum)),
    ("array/segment_max", Primitive::ArraySegment(Reduction::Maximum)),
    ("array/segment_argmax", Primitive::ArraySegmentArgmax),
    ("float/next_after", Primitive::NextAfter),
    ("array/grade", Primitive::ArrayGrade),
    ("array/index_of", Primitive::ArrayIndexOf),
    ("array/compact", Primitive::ArrayCompact),
    ("array/scan_sum", Primitive::ArrayScanSum),
    ("array/scatter_add", Primitive::ArrayScatterAdd),
    ("sequence/length", Primitive::SequenceLength),
    ("sequence/get", Primitive::SequenceGet),
    ("sequence/build", Primitive::SequenceBuild),
    ("sequence/slice", Primitive::SequenceSlice),
    ("sequence/concat", Primitive::SequenceConcat),
    ("array/max_length", Primitive::ArrayMaxLength),
    ("array/range", Primitive::ArrayRange),
    ("array/int", Primitive::ArrayInt),
    ("array/float", Primitive::ArrayFloat),
    ("array/vec3", Primitive::ArrayVec3),
    ("array/vec4", Primitive::ArrayVec4),
    ("array/text", Primitive::ArrayText),
    ("array/nth", Primitive::ArrayNth),
    ("array/count", Primitive::ArrayCount),
    ("array/slice", Primitive::ArraySlice),
    ("array/concat", Primitive::ArrayConcat),
    ("value/rand", Primitive::Rand),
    ("rand/at", Primitive::RandAt),
    ("fma", Primitive::Fma),
    ("noise3", Primitive::Noise3),
    ("color/parse", Primitive::ColorParse),
    ("float/mul_checked", Primitive::CheckedMul),
    ("float/fma_checked", Primitive::CheckedFma),
    ("truthy", Primitive::Truthy),
    ("error", Primitive::Error),
];

#[derive(Clone, Copy)]
pub enum Cpu { F1(fn(f64) -> f64), F2(fn(f64, f64) -> f64) }
#[derive(Clone, Copy)]
pub enum SpirvMath { Float(i64), Glsl(i64), Compare(i64), ReducedAngle(i64), AbsGlsl(i64), BoundedGlsl(i64), Special }

/// Each kernel math primitive owns its scalar meaning and backend spellings here.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum MathOutput { Arithmetic, Float, Bool }

pub struct MathPrim {
    pub name: &'static str,
    pub operation: Primitive,
    pub cpu: Cpu,
    pub output: MathOutput,
    pub msl: Option<&'static str>,
    pub spirv: SpirvMath,
    pub js: &'static str,
    pub wgsl: Option<&'static str>,
}

pub static MATH_PRIMS: &[MathPrim] = &[
    MathPrim { name: "+", operation: Primitive::Numeric(Binary::Add), cpu: Cpu::F2(|x, y| x + y), output: MathOutput::Arithmetic, msl: Some("({a}+{b})"), spirv: SpirvMath::Float(129), js: "{a} + {b}", wgsl: Some("({a} + {b})") },
    MathPrim { name: "-", operation: Primitive::Numeric(Binary::Sub), cpu: Cpu::F2(|x, y| x - y), output: MathOutput::Arithmetic, msl: Some("({a}-{b})"), spirv: SpirvMath::Float(131), js: "{a} - {b}", wgsl: Some("({a} - {b})") },
    MathPrim { name: "*", operation: Primitive::Numeric(Binary::Mul), cpu: Cpu::F2(|x, y| x * y), output: MathOutput::Arithmetic, msl: Some("({a}*{b})"), spirv: SpirvMath::Float(133), js: "{a} * {b}", wgsl: Some("({a} * {b})") },
    MathPrim { name: "/", operation: Primitive::Numeric(Binary::Div), cpu: Cpu::F2(|x, y| if y == 0.0 { 0.0 } else { x / y }), output: MathOutput::Arithmetic, msl: Some("({b}==0.0f?0.0f:{a}/{b})"), spirv: SpirvMath::Special, js: "{b} === 0 ? 0 : {a} / {b}", wgsl: Some("select({a} / {b}, 0.0, {b} == 0.0)") },
    MathPrim { name: "mod", operation: Primitive::Numeric(Binary::Mod), cpu: Cpu::F2(|x, y| if y == 0.0 { 0.0 } else { ((x % y) + y) % y }), output: MathOutput::Arithmetic, msl: Some("({b}==0.0f?0.0f:fmod(fmod({a},{b})+{b},{b}))"), spirv: SpirvMath::Special, js: "{b} === 0 ? 0 : (({a} % {b}) + {b}) % {b}", wgsl: Some("select((({a} % {b}) + {b}) % {b}, 0.0, {b} == 0.0)") },
    MathPrim { name: "pow", operation: Primitive::Numeric(Binary::Pow), cpu: Cpu::F2(|x, y| x.abs().powf(y)), output: MathOutput::Arithmetic, msl: Some("pow(fabs({a}),{b})"), spirv: SpirvMath::Special, js: "Math.pow(Math.abs({a}), {b})", wgsl: Some("wpow({a}, {b})") },
    MathPrim { name: "min", operation: Primitive::Numeric(Binary::Min), cpu: Cpu::F2(|x, y| float_min(x, y)), output: MathOutput::Arithmetic, msl: Some("min({a},{b})"), spirv: SpirvMath::Glsl(37), js: "Math.min({a}, {b})", wgsl: Some("min({a}, {b})") },
    MathPrim { name: "max", operation: Primitive::Numeric(Binary::Max), cpu: Cpu::F2(|x, y| float_max(x, y)), output: MathOutput::Arithmetic, msl: Some("max({a},{b})"), spirv: SpirvMath::Glsl(40), js: "Math.max({a}, {b})", wgsl: Some("max({a}, {b})") },
    MathPrim { name: "<", operation: Primitive::Numeric(Binary::Lt), cpu: Cpu::F2(|x, y| if x < y { 1.0 } else { 0.0 }), output: MathOutput::Bool, msl: Some("({a}<{b})"), spirv: SpirvMath::Compare(184), js: "({a} < {b} ? 1 : 0)", wgsl: Some("select(0.0, 1.0, ({a} < {b}))") },
    MathPrim { name: "<=", operation: Primitive::Numeric(Binary::Le), cpu: Cpu::F2(|x, y| if x <= y { 1.0 } else { 0.0 }), output: MathOutput::Bool, msl: Some("({a}<={b})"), spirv: SpirvMath::Compare(188), js: "({a} <= {b} ? 1 : 0)", wgsl: Some("select(0.0, 1.0, ({a} <= {b}))") },
    MathPrim { name: ">", operation: Primitive::Numeric(Binary::Gt), cpu: Cpu::F2(|x, y| if x > y { 1.0 } else { 0.0 }), output: MathOutput::Bool, msl: Some("({a}>{b})"), spirv: SpirvMath::Compare(186), js: "({a} > {b} ? 1 : 0)", wgsl: Some("select(0.0, 1.0, ({a} > {b}))") },
    MathPrim { name: ">=", operation: Primitive::Numeric(Binary::Ge), cpu: Cpu::F2(|x, y| if x >= y { 1.0 } else { 0.0 }), output: MathOutput::Bool, msl: Some("({a}>={b})"), spirv: SpirvMath::Compare(190), js: "({a} >= {b} ? 1 : 0)", wgsl: Some("select(0.0, 1.0, ({a} >= {b}))") },
    MathPrim { name: "=", operation: Primitive::Numeric(Binary::Eq), cpu: Cpu::F2(|x, y| if x == y { 1.0 } else { 0.0 }), output: MathOutput::Bool, msl: Some("({a}=={b})"), spirv: SpirvMath::Compare(180), js: "({a} === {b} ? 1 : 0)", wgsl: Some("select(0.0, 1.0, ({a} == {b}))") },
    MathPrim { name: "and", operation: Primitive::Numeric(Binary::And), cpu: Cpu::F2(|x, y| if x != 0.0 && y != 0.0 { 1.0 } else { 0.0 }), output: MathOutput::Bool, msl: Some("({a}!=0.0f&&{b}!=0.0f)"), spirv: SpirvMath::Special, js: "({a} !== 0 && {b} !== 0 ? 1 : 0)", wgsl: Some("select(0.0, 1.0, ({a} != 0.0 && {b} != 0.0))") },
    MathPrim { name: "or", operation: Primitive::Numeric(Binary::Or), cpu: Cpu::F2(|x, y| if x != 0.0 || y != 0.0 { 1.0 } else { 0.0 }), output: MathOutput::Bool, msl: Some("({a}!=0.0f||{b}!=0.0f)"), spirv: SpirvMath::Special, js: "({a} !== 0 || {b} !== 0 ? 1 : 0)", wgsl: Some("select(0.0, 1.0, ({a} != 0.0 || {b} != 0.0))") },
    MathPrim { name: "atan2", operation: Primitive::Numeric(Binary::Atan2), cpu: Cpu::F2(|x, y| rays_math::libm::atan2(x, y)), output: MathOutput::Float, msl: Some("atan2({a},{b})"), spirv: SpirvMath::Glsl(25), js: "Math.atan2({a}, {b})", wgsl: Some("atan2({a}, {b})") },
    MathPrim { name: "hypot", operation: Primitive::Numeric(Binary::Hypot), cpu: Cpu::F2(|x, y| x.hypot(y)), output: MathOutput::Float, msl: None, spirv: SpirvMath::Special, js: "Math.hypot({a}, {b})", wgsl: None },
    MathPrim { name: "ldexp", operation: Primitive::Numeric(Binary::Ldexp), cpu: Cpu::F2(|x, y| ldexp(x, y.trunc() as i64)), output: MathOutput::Float, msl: Some("ldexp({a},int({b}))"), spirv: SpirvMath::Special, js: "({a} * 2 ** Math.trunc({b}))", wgsl: Some("ldexp({a}, i32({b}))") },
    MathPrim { name: "sin", operation: Primitive::Math(Unary::Sin), cpu: Cpu::F1(|x| rays_math::libm::sin(x)), output: MathOutput::Float, msl: Some("sin({a})"), spirv: SpirvMath::ReducedAngle(13), js: "Math.sin({a})", wgsl: Some("sin({a})") },
    MathPrim { name: "cos", operation: Primitive::Math(Unary::Cos), cpu: Cpu::F1(|x| rays_math::libm::cos(x)), output: MathOutput::Float, msl: Some("cos({a})"), spirv: SpirvMath::ReducedAngle(14), js: "Math.cos({a})", wgsl: Some("cos({a})") },
    MathPrim { name: "sqrt", operation: Primitive::Math(Unary::Sqrt), cpu: Cpu::F1(|x| x.abs().sqrt()), output: MathOutput::Float, msl: Some("sqrt(fabs({a}))"), spirv: SpirvMath::AbsGlsl(31), js: "Math.sqrt(Math.abs({a}))", wgsl: Some("sqrt(abs({a}))") },
    MathPrim { name: "abs", operation: Primitive::Math(Unary::Abs), cpu: Cpu::F1(|x| x.abs()), output: MathOutput::Float, msl: Some("fabs({a})"), spirv: SpirvMath::Glsl(4), js: "Math.abs({a})", wgsl: Some("abs({a})") },
    MathPrim { name: "not", operation: Primitive::Math(Unary::Not), cpu: Cpu::F1(|x| if x == 0.0 { 1.0 } else { 0.0 }), output: MathOutput::Bool, msl: Some("({a}==0.0f)"), spirv: SpirvMath::Special, js: "({a} === 0 ? 1 : 0)", wgsl: Some("select(0.0, 1.0, ({a} == 0.0))") },
    MathPrim { name: "tan", operation: Primitive::Math(Unary::Tan), cpu: Cpu::F1(|x| rays_math::libm::tan(x)), output: MathOutput::Float, msl: Some("tan({a})"), spirv: SpirvMath::ReducedAngle(15), js: "Math.tan({a})", wgsl: Some("tan({a})") },
    MathPrim { name: "asin", operation: Primitive::Math(Unary::Asin), cpu: Cpu::F1(|x| rays_math::libm::asin(x)), output: MathOutput::Float, msl: Some("(fabs({a})>1.0f?as_type<float>(0x7fc00000u):asin({a}))"), spirv: SpirvMath::BoundedGlsl(16), js: "Math.asin({a})", wgsl: Some("select(asin(clamp({a}, -1.0, 1.0)), bitcast<f32>(0x7fc00000u), abs({a}) > 1.0)") },
    MathPrim { name: "acos", operation: Primitive::Math(Unary::Acos), cpu: Cpu::F1(|x| rays_math::libm::acos(x)), output: MathOutput::Float, msl: Some("(fabs({a})>1.0f?as_type<float>(0x7fc00000u):acos({a}))"), spirv: SpirvMath::BoundedGlsl(17), js: "Math.acos({a})", wgsl: Some("select(acos(clamp({a}, -1.0, 1.0)), bitcast<f32>(0x7fc00000u), abs({a}) > 1.0)") },
    MathPrim { name: "exp", operation: Primitive::Math(Unary::Exp), cpu: Cpu::F1(|x| x.exp()), output: MathOutput::Float, msl: Some("exp({a})"), spirv: SpirvMath::Glsl(27), js: "Math.exp({a})", wgsl: Some("exp({a})") },
    MathPrim { name: "log", operation: Primitive::Math(Unary::Log), cpu: Cpu::F1(|x| x.ln()), output: MathOutput::Float, msl: Some("({a}<=0.0f?as_type<float>(0x7fc00000u):log({a}))"), spirv: SpirvMath::Special, js: "Math.log({a})", wgsl: Some("select(log(select(1.0, {a}, {a} > 0.0)), bitcast<f32>(0x7fc00000u), {a} <= 0.0)") },
    MathPrim { name: "sign", operation: Primitive::Math(Unary::Sign), cpu: Cpu::F1(|x| if x > 0.0 { 1.0 } else if x < 0.0 { -1.0 } else { x }), output: MathOutput::Float, msl: Some("({a}>0.0f?1.0f:({a}<0.0f?-1.0f:{a}))"), spirv: SpirvMath::Special, js: "Math.sign({a})", wgsl: Some("select({a}, select(-1.0, 1.0, {a} > 0.0), {a} != 0.0)") },
    MathPrim { name: "rsqrt", operation: Primitive::Math(Unary::Rsqrt), cpu: Cpu::F1(|x| 1.0 / x.abs().sqrt()), output: MathOutput::Float, msl: Some("rsqrt(fabs({a}))"), spirv: SpirvMath::AbsGlsl(32), js: "(1 / Math.sqrt(Math.abs({a})))", wgsl: Some("inverseSqrt(abs({a}))") },
 ];

pub fn math_prim(operation: &Primitive) -> &'static MathPrim {
    match operation {
        Primitive::Numeric(operation) => &MATH_PRIMS[*operation as usize],
        Primitive::Math(operation) => &MATH_PRIMS[18 + *operation as usize],
        _ => unreachable!("math primitive"),
    }
}

pub fn math_source(template: &str, a: &str, b: &str) -> String { template.replace("{a}", a).replace("{b}", b) }

pub fn primitive(name: &str) -> Option<Primitive> { MATH_PRIMS.iter().find(|row| row.name == name).map(|row| row.operation.clone()).or_else(|| PRIMITIVES.iter().find(|(n, _)| *n == name).map(|(_, op)| op.clone())) }
pub fn primitive_name(operation: &Primitive) -> &'static str {
    match operation {
        Primitive::Numeric(_) | Primitive::Math(_) => math_prim(operation).name,
        Primitive::ColorByte(_) => "color/byte",
        Primitive::Coerce(_) => "coerce",
        Primitive::Fail(..) | Primitive::FailDiagnostic(_) => "fail",
        _ => PRIMITIVES.iter().find(|(_, op)| op == operation).expect("complete primitive table").0,
    }
}
pub fn binary(name: &str) -> Option<Binary> { if let Some(Primitive::Numeric(op)) = primitive(name) { Some(op) } else { None } }
pub fn unary(name: &str) -> Option<Unary> { if let Some(Primitive::Math(op)) = primitive(name) { Some(op) } else { None } }
pub fn integer(name: &str) -> Option<Integer> { if let Some(Primitive::Integer(op)) = primitive(name) { Some(op) } else { None } }
pub fn comparison(name: &str) -> Option<Comparison> { if let Some(Primitive::IntegerCompare(op)) = primitive(name) { Some(op) } else { None } }
pub fn integer_name(operation: Integer) -> &'static str { primitive_name(&Primitive::Integer(operation)) }
pub fn comparison_name(operation: Comparison) -> &'static str { primitive_name(&Primitive::IntegerCompare(operation)) }

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
    if let Cpu::F2(cpu) = math_prim(&Primitive::Numeric(op)).cpu { cpu(x, y) } else { unreachable!("binary primitive") }
}
#[inline]
pub fn unary_apply(op: Unary, x: f64) -> f64 {
    if let Cpu::F1(cpu) = math_prim(&Primitive::Math(op)).cpu { cpu(x) } else { unreachable!("unary primitive") }
}

pub fn rand_at(seed: i64, index: i64) -> f64 { rays_math::rand::float_at_int(seed, index) }

const DERIVED_NAMES: &[&str] = &["length", "dot", "cross", "normalize"];
pub fn noise_names() -> Vec<String> { vec![primitive_name(&Primitive::Noise3).to_owned()] }
pub fn names() -> Vec<String> {
    let names = |filter: fn(&Primitive) -> bool| PRIMITIVES.iter().filter(move |(_, op)| filter(op)).map(|(name, _)| (*name).to_owned());
    MATH_PRIMS.iter().map(|row| row.name.to_owned())
        .chain(noise_names())
        .chain(DERIVED_NAMES.iter().map(|name| (*name).to_owned()))
        .chain(std::iter::once(primitive_name(&Primitive::Fma).to_owned()))
        .chain(names(|op| matches!(op, Primitive::Integer(_) | Primitive::IntegerCompare(_))))
        .chain(std::iter::once(primitive_name(&Primitive::RandAt).to_owned())).collect()
}
pub fn supports(name: &str) -> bool {
    DERIVED_NAMES.contains(&name) || matches!(primitive(name), Some(Primitive::Numeric(_) | Primitive::Math(_) | Primitive::Integer(_) | Primitive::IntegerCompare(_) | Primitive::Noise3 | Primitive::Fma | Primitive::RandAt))
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn math_rows_keep_cpu_and_backend_names_together() {
        let mut names = std::collections::HashSet::new();
        for row in MATH_PRIMS {
            assert!(names.insert(row.name));
            assert_eq!(primitive(row.name), Some(row.operation.clone()));
            assert!(std::ptr::eq(math_prim(&row.operation), row));
            assert_eq!(primitive_name(&row.operation), row.name);
            assert!(row.js.contains("{a}"));
        }
        assert_eq!(math_prim(&Primitive::Numeric(Binary::Atan2)).msl, Some("atan2({a},{b})"));
        assert_eq!(binary_apply(Binary::Div, 2.0, 0.0), 0.0);
        assert_eq!(unary_apply(Unary::Sqrt, -4.0), 2.0);
    }

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
        assert_eq!(primitive_name(&Primitive::ColorByte(ByteMode::Unclamped)), "color/byte");
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
