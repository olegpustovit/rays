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
