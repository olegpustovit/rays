#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ScalarType { Float, Half, Int, Uint, Short, Ushort, Char, Uchar, Bool, Long, Ulong, Bfloat }

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ShaderDataType {
    NoType,
    Struct,
    Array,
    Scalar(ScalarType),
    Vector(ScalarType, u8),
    Matrix(ScalarType, u8, u8),
    TextureType,
    SamplerType,
    Pointer,
    Other(i64),
}

/// Decodes an MTLDataType code the way lib/metal's `Shader_type.of_code` does: each scalar code is
/// followed by its 2..4 vectors, and float and half also have their matrices (rows, columns).
pub fn shader_data_type(code: i64) -> ShaderDataType {
    use ScalarType::*;
    const SCALARS: [(i64, ScalarType); 12] = [
        (3, Float), (16, Half), (29, Int), (33, Uint), (37, Short), (41, Ushort),
        (45, Char), (49, Uchar), (53, Bool), (81, Long), (85, Ulong), (121, Bfloat),
    ];
    match code {
        0 => return ShaderDataType::NoType,
        1 => return ShaderDataType::Struct,
        2 => return ShaderDataType::Array,
        58 => return ShaderDataType::TextureType,
        59 => return ShaderDataType::SamplerType,
        60 => return ShaderDataType::Pointer,
        _ => {}
    }
    for (base, scalar) in SCALARS {
        if code == base { return ShaderDataType::Scalar(scalar); }
        if (base + 1..=base + 3).contains(&code) { return ShaderDataType::Vector(scalar, (code - base + 1) as u8); }
        if matches!(scalar, Float | Half) && (base + 4..=base + 12).contains(&code) {
            let k = (code - base - 4) as u8;
            return ShaderDataType::Matrix(scalar, 2 + k / 3, 2 + k % 3);
        }
    }
    ShaderDataType::Other(code)
}
