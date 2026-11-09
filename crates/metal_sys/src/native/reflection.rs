use super::Error;
use std::ffi::c_void;

#[derive(Clone, Debug, PartialEq)]
pub enum ReflectedType {
    Scalar(i64),
    Array { data_type: i64, length: i64, stride: i64, argument_index_stride: i64, element: Option<Box<Self>> },
    Pointer { data_type: i64, access: i64, alignment: i64, data_size: i64, element_is_argument_buffer: bool, element: Option<Box<Self>> },
    Struct(Vec<Member>),
    TextureReference { data_type: i64, access: i64, texture_type: i64, depth: bool },
    TensorReference { data_type: i64, access: i64, tensor_data_type: i64, index_type: i64, dimensions: Option<Vec<i64>> },
}

#[derive(Clone, Debug, PartialEq)]
pub struct Member {
    pub name: String,
    pub offset: i64,
    pub argument_index: i64,
    pub data_type: i64,
    pub reflected_type: Option<Box<ReflectedType>>,
}

#[repr(C)]
struct RawReflectionNode {
    kind: i32,
    data_type: i64,
    access: i64,
    alignment: i64,
    data_size: i64,
    element_is_argument_buffer: u8,
    length: i64,
    stride: i64,
    argument_index_stride: i64,
    texture_type: i64,
    depth: u8,
    tensor_data_type: i64,
    index_type: i64,
    dimensions_present: u8,
    dimensions: *mut i64,
    dimension_count: usize,
    element: *mut RawReflectionNode,
    members: *mut RawReflectionMember,
    member_count: usize,
}

#[repr(C)]
struct RawReflectionMember {
    name: *mut std::ffi::c_char,
    offset: i64,
    argument_index: i64,
    data_type: i64,
    reflected_type: *mut RawReflectionNode,
}

const MAX_DEPTH: usize = 32;
const MAX_MEMBERS: usize = 65_536;

#[cfg(target_vendor = "apple")]
unsafe fn parse(node: *mut RawReflectionNode, depth: usize, remaining: &mut usize) -> Result<ReflectedType, Error> {
    use std::ffi::CStr;
    if node.is_null() || depth > MAX_DEPTH { return Err(Error::InvalidHandle); }
    let node = unsafe { &*node };
    match node.kind {
        0 => Ok(ReflectedType::Scalar(node.data_type)),
        1 => Ok(ReflectedType::Array {
            data_type: node.data_type,
            length: node.length,
            stride: node.stride,
            argument_index_stride: node.argument_index_stride,
            element: if node.element.is_null() { None } else { Some(Box::new(unsafe { parse(node.element, depth + 1, remaining) }?)) },
        }),
        2 => Ok(ReflectedType::Pointer {
            data_type: node.data_type,
            access: node.access,
            alignment: node.alignment,
            data_size: node.data_size,
            element_is_argument_buffer: node.element_is_argument_buffer != 0,
            element: if node.element.is_null() { None } else { Some(Box::new(unsafe { parse(node.element, depth + 1, remaining) }?)) },
        }),
        3 => {
            if node.member_count > *remaining || (node.member_count != 0 && node.members.is_null()) { return Err(Error::InvalidArgument("Metal reflection exceeds member limits")); }
            *remaining -= node.member_count;
            let mut output = Vec::with_capacity(node.member_count);
            if node.member_count != 0 {
                for member in unsafe { std::slice::from_raw_parts(node.members, node.member_count) } {
                    if member.name.is_null() { return Err(Error::InvalidHandle); }
                    let name = unsafe { CStr::from_ptr(member.name) }.to_string_lossy().into_owned();
                    let reflected_type = if member.reflected_type.is_null() { None } else { Some(Box::new(unsafe { parse(member.reflected_type, depth + 1, remaining) }?)) };
                    output.push(Member { name, offset: member.offset, argument_index: member.argument_index, data_type: member.data_type, reflected_type });
                }
            }
            Ok(ReflectedType::Struct(output))
        }
        4 => Ok(ReflectedType::TextureReference { data_type: node.data_type, access: node.access, texture_type: node.texture_type, depth: node.depth != 0 }),
        5 => {
            if node.dimensions_present == 0 {
                if !node.dimensions.is_null() || node.dimension_count != 0 { return Err(Error::InvalidHandle); }
                Ok(ReflectedType::TensorReference { data_type: node.data_type, access: node.access, tensor_data_type: node.tensor_data_type, index_type: node.index_type, dimensions: None })
            } else {
                if node.dimension_count > MAX_MEMBERS || (node.dimension_count != 0 && node.dimensions.is_null()) { return Err(Error::InvalidArgument("Metal tensor reflection exceeds dimension limits")); }
                let dimensions = if node.dimension_count == 0 { Vec::new() } else { unsafe { std::slice::from_raw_parts(node.dimensions, node.dimension_count) }.to_vec() };
                Ok(ReflectedType::TensorReference { data_type: node.data_type, access: node.access, tensor_data_type: node.tensor_data_type, index_type: node.index_type, dimensions: Some(dimensions) })
            }
        }
        _ => Err(Error::InvalidHandle),
    }
}

/// Snapshots a borrowed `MTLType` pointer returned by Metal reflection.
/// The pointer is consumed synchronously and never escapes the native wrapper.
pub(crate) unsafe fn snapshot(raw_type: *mut c_void) -> Result<Option<ReflectedType>, Error> {
    if raw_type.is_null() { return Ok(None); }
    #[cfg(target_vendor = "apple")]
    {
        let mut output = std::ptr::null_mut();
        let mut message = std::ptr::null();
        let status = unsafe { rays_metal_reflection_snapshot(raw_type, &mut output, &mut message) };
        if status != 0 {
            unsafe { rays_metal_reflection_free(output); }
            return Err(unsafe { Error::from_native(status, message) });
        }
        if output.is_null() { return Err(Error::InvalidHandle); }
        let mut remaining = MAX_MEMBERS;
        let result = unsafe { parse(output, 0, &mut remaining) };
        unsafe { rays_metal_reflection_free(output); }
        result.map(Some)
    }
    #[cfg(not(target_vendor = "apple"))]
    { let _ = raw_type; Err(Error::Unavailable) }
}

#[cfg(target_vendor = "apple")]
unsafe extern "C" {
    fn rays_metal_reflection_snapshot(raw_type: *mut c_void, output: *mut *mut RawReflectionNode, message: *mut *const std::ffi::c_char) -> i32;
    fn rays_metal_reflection_free(node: *mut RawReflectionNode);
}
