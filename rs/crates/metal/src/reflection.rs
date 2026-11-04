pub use crate::metal_argument_reflection_snapshot::{Member, ReflectedType};

pub(crate) fn from_native(value: metal_sys::native::reflection::ReflectedType) -> ReflectedType {
    use metal_sys::native::reflection::ReflectedType as Native;
    match value {
        Native::Scalar(data_type) => ReflectedType::Scalar(data_type),
        Native::Array { data_type, length, stride, argument_index_stride, element } => ReflectedType::Array {
            data_type, length, stride, argument_index_stride,
            element: element.map(|value| Box::new(from_native(*value))),
        },
        Native::Pointer { data_type, access, alignment, data_size, element_is_argument_buffer, element } => ReflectedType::Pointer {
            data_type, access, alignment, data_size, element_is_argument_buffer,
            element: element.map(|value| Box::new(from_native(*value))),
        },
        Native::Struct(members) => ReflectedType::Struct(members.into_iter().map(|member| Member {
            name: member.name,
            offset: member.offset,
            argument_index: member.argument_index,
            data_type: member.data_type,
            reflected_type: member.reflected_type.map(|value| Box::new(from_native(*value))),
        }).collect()),
        Native::TextureReference { data_type, access, texture_type, depth } => ReflectedType::TextureReference {
            data_type, access, texture_type, depth,
        },
        Native::TensorReference { data_type, access, tensor_data_type, index_type, dimensions } => ReflectedType::TensorReference {
            data_type, access, tensor_data_type, index_type, dimensions,
        },
    }
}

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
