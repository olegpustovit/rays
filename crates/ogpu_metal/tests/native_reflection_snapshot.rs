pub use metal_sys::native::reflection::{Member, ReflectedType};
use std::any::Any;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Limits {
    pub max_depth: usize,
    pub max_members: usize,
}

pub const DEFAULT_LIMITS: Limits = Limits { max_depth: 32, max_members: 65_536 };

#[derive(Clone, Debug, PartialEq)]
pub struct Argument {
    pub name: String,
    pub index: i64,
    pub active: bool,
    pub access: i64,
    pub argument_type: i64,
    pub array_length: i64,
    pub buffer_alignment: i64,
    pub buffer_data_size: i64,
    pub buffer_data_type: i64,
    pub texture_data_type: i64,
    pub texture_type: i64,
    pub depth_texture: bool,
    pub threadgroup_memory_alignment: i64,
    pub threadgroup_memory_data_size: i64,
    pub buffer_pointer_type: Option<ReflectedType>,
    pub buffer_struct_type: Option<ReflectedType>,
}
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Kind { Scalar, Array, Pointer, Struct, TextureReference, TensorReference }


pub type Handle = Box<dyn Any>;

pub trait Raw {
    fn destroy(&self, handle: Handle);
    fn kind(&self, handle: &Handle) -> Result<Kind, String>;
    fn data_type(&self, handle: &Handle) -> Result<i64, String>;
    fn array_length(&self, handle: &Handle) -> Result<i64, String>;
    fn array_stride(&self, handle: &Handle) -> Result<i64, String>;
    fn argument_index_stride(&self, handle: &Handle) -> Result<i64, String>;
    fn element(&self, handle: &Handle) -> Result<Option<Handle>, String>;
    fn pointer_access(&self, handle: &Handle) -> Result<i64, String>;
    fn alignment(&self, handle: &Handle) -> Result<i64, String>;
    fn data_size(&self, handle: &Handle) -> Result<i64, String>;
    fn element_is_argument_buffer(&self, handle: &Handle) -> Result<bool, String>;
    fn members(&self, handle: &Handle) -> Result<Vec<Handle>, String>;
    fn member_name(&self, handle: &Handle) -> Result<String, String>;
    fn member_offset(&self, handle: &Handle) -> Result<i64, String>;
    fn member_argument_index(&self, handle: &Handle) -> Result<i64, String>;
    fn member_type(&self, handle: &Handle) -> Result<Option<Handle>, String>;
    fn texture_access(&self, handle: &Handle) -> Result<i64, String>;
    fn texture_type(&self, handle: &Handle) -> Result<i64, String>;
    fn is_depth_texture(&self, handle: &Handle) -> Result<bool, String>;
    fn tensor_access(&self, handle: &Handle) -> Result<i64, String>;
    fn tensor_data_type(&self, handle: &Handle) -> Result<i64, String>;
    fn tensor_index_type(&self, handle: &Handle) -> Result<i64, String>;
    fn tensor_dimensions(&self, handle: &Handle) -> Result<Option<Vec<i64>>, String>;
    fn argument_name(&self, handle: &Handle) -> Result<String, String>;
    fn argument_index(&self, handle: &Handle) -> Result<i64, String>;
    fn argument_active(&self, handle: &Handle) -> Result<bool, String>;
    fn argument_access(&self, handle: &Handle) -> Result<i64, String>;
    fn argument_type(&self, handle: &Handle) -> Result<i64, String>;
    fn argument_array_length(&self, handle: &Handle) -> Result<i64, String>;
    fn argument_buffer_alignment(&self, handle: &Handle) -> Result<i64, String>;
    fn argument_buffer_data_size(&self, handle: &Handle) -> Result<i64, String>;
    fn argument_buffer_data_type(&self, handle: &Handle) -> Result<i64, String>;
    fn argument_texture_data_type(&self, handle: &Handle) -> Result<i64, String>;
    fn argument_texture_type(&self, handle: &Handle) -> Result<i64, String>;
    fn argument_is_depth_texture(&self, handle: &Handle) -> Result<bool, String>;
    fn argument_threadgroup_memory_alignment(&self, handle: &Handle) -> Result<i64, String>;
    fn argument_threadgroup_memory_data_size(&self, handle: &Handle) -> Result<i64, String>;
    fn argument_buffer_pointer_type(&self, handle: &Handle) -> Result<Option<Handle>, String>;
    fn argument_buffer_struct_type(&self, handle: &Handle) -> Result<Option<Handle>, String>;
}

fn validate(limits: Limits) -> Result<(), String> {
    if limits.max_depth > i64::MAX as usize || limits.max_members > i64::MAX as usize {
        Err("Metal reflection limits exceed native bounds".to_owned())
    } else { Ok(()) }
}

fn with_owned(raw: &dyn Raw, handle: Option<Handle>, f: &mut dyn FnMut(&Handle) -> Result<ReflectedType, String>) -> Result<Option<ReflectedType>, String> {
    match handle {
        None => Ok(None),
        Some(handle) => {
            let result = f(&handle);
            raw.destroy(handle);
            result.map(Some)
        }
    }
}

pub fn reflected_type(raw: &dyn Raw, root: &Handle) -> Result<ReflectedType, String> {
    reflected_type_with_limits(raw, root, DEFAULT_LIMITS)
}

pub fn reflected_type_with_limits(raw: &dyn Raw, root: &Handle, limits: Limits) -> Result<ReflectedType, String> {
    validate(limits)?;
    let mut remaining = limits.max_members;
    type_at(raw, 0, root, limits, &mut remaining)
}

fn type_at(raw: &dyn Raw, depth: usize, handle: &Handle, limits: Limits, remaining: &mut usize) -> Result<ReflectedType, String> {
    if depth > limits.max_depth { return Err("Metal reflection exceeded max_depth".to_owned()); }
    let data_type = raw.data_type(handle)?;
    match raw.kind(handle)? {
        Kind::Scalar => Ok(ReflectedType::Scalar(data_type)),
        Kind::Array => {
            let length = raw.array_length(handle)?;
            let stride = raw.array_stride(handle)?;
            let argument_index_stride = raw.argument_index_stride(handle)?;
            let child = raw.element(handle)?;
            let element = with_owned(raw, child, &mut |child| type_at(raw, depth + 1, child, limits, remaining))?.map(Box::new);
            Ok(ReflectedType::Array { data_type, length, stride, argument_index_stride, element })
        }
        Kind::Pointer => {
            let access = raw.pointer_access(handle)?;
            let alignment = raw.alignment(handle)?;
            let data_size = raw.data_size(handle)?;
            let element_is_argument_buffer = raw.element_is_argument_buffer(handle)?;
            let child = raw.element(handle)?;
            let element = with_owned(raw, child, &mut |child| type_at(raw, depth + 1, child, limits, remaining))?.map(Box::new);
            Ok(ReflectedType::Pointer { data_type, access, alignment, data_size, element_is_argument_buffer, element })
        }
        Kind::Struct => {
            let members = raw.members(handle)?;
            if members.len() > *remaining {
                for member in members { raw.destroy(member); }
                return Err("Metal reflection exceeded max_members".to_owned());
            }
            *remaining -= members.len();
            let mut output = Vec::with_capacity(members.len());
            let mut iter = members.into_iter();
            while let Some(member) = iter.next() {
                let result = member_at(raw, depth + 1, &member, limits, remaining);
                raw.destroy(member);
                match result {
                    Ok(member) => output.push(member),
                    Err(error) => {
                        for remaining_member in iter { raw.destroy(remaining_member); }
                        return Err(error);
                    }
                }
            }
            Ok(ReflectedType::Struct(output))
        }
        Kind::TextureReference => Ok(ReflectedType::TextureReference {
            data_type,
            access: raw.texture_access(handle)?,
            texture_type: raw.texture_type(handle)?,
            depth: raw.is_depth_texture(handle)?,
        }),
        Kind::TensorReference => Ok(ReflectedType::TensorReference {
            data_type,
            access: raw.tensor_access(handle)?,
            tensor_data_type: raw.tensor_data_type(handle)?,
            index_type: raw.tensor_index_type(handle)?,
            dimensions: raw.tensor_dimensions(handle)?,
        }),
    }
}


fn member_at(raw: &dyn Raw, depth: usize, handle: &Handle, limits: Limits, remaining: &mut usize) -> Result<Member, String> {
    let name = raw.member_name(handle)?;
    let offset = raw.member_offset(handle)?;
    let argument_index = raw.member_argument_index(handle)?;
    let data_type = raw.data_type(handle)?;
    let child = raw.member_type(handle)?;
    let reflected_type = with_owned(raw, child, &mut |child| type_at(raw, depth, child, limits, remaining))?.map(Box::new);
    Ok(Member { name, offset, argument_index, data_type, reflected_type })
}


pub fn argument(raw: &dyn Raw, handle: &Handle) -> Result<Argument, String> {
    argument_with_limits(raw, handle, DEFAULT_LIMITS)
}

pub fn argument_with_limits(raw: &dyn Raw, handle: &Handle, limits: Limits) -> Result<Argument, String> {
    validate(limits)?;
    let name = raw.argument_name(handle)?;
    let index = raw.argument_index(handle)?;
    let active = raw.argument_active(handle)?;
    let access = raw.argument_access(handle)?;
    let argument_type = raw.argument_type(handle)?;
    let array_length = raw.argument_array_length(handle)?;
    let buffer_alignment = raw.argument_buffer_alignment(handle)?;
    let buffer_data_size = raw.argument_buffer_data_size(handle)?;
    let buffer_data_type = raw.argument_buffer_data_type(handle)?;
    let texture_data_type = raw.argument_texture_data_type(handle)?;
    let texture_type = raw.argument_texture_type(handle)?;
    let depth_texture = raw.argument_is_depth_texture(handle)?;
    let threadgroup_memory_alignment = raw.argument_threadgroup_memory_alignment(handle)?;
    let threadgroup_memory_data_size = raw.argument_threadgroup_memory_data_size(handle)?;
    let buffer_pointer_type = with_owned(raw, raw.argument_buffer_pointer_type(handle)?, &mut |value| reflected_type_with_limits(raw, value, limits))?;
    let buffer_struct_type = with_owned(raw, raw.argument_buffer_struct_type(handle)?, &mut |value| reflected_type_with_limits(raw, value, limits))?;
    Ok(Argument {
        name, index, active, access, argument_type, array_length, buffer_alignment, buffer_data_size,
        buffer_data_type, texture_data_type, texture_type, depth_texture, threadgroup_memory_alignment,
        threadgroup_memory_data_size, buffer_pointer_type, buffer_struct_type,
    })
}
