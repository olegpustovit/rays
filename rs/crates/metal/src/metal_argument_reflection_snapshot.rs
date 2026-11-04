use std::marker::PhantomData;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Limits {
    pub max_depth: usize,
    pub max_members: usize,
}

pub const DEFAULT_LIMITS: Limits = Limits { max_depth: 32, max_members: 65_536 };

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


pub trait Raw {
    type Handle;
    fn destroy(handle: Self::Handle);
    fn kind(handle: &Self::Handle) -> Result<Kind, String>;
    fn data_type(handle: &Self::Handle) -> Result<i64, String>;
    fn array_length(handle: &Self::Handle) -> Result<i64, String>;
    fn array_stride(handle: &Self::Handle) -> Result<i64, String>;
    fn argument_index_stride(handle: &Self::Handle) -> Result<i64, String>;
    fn element(handle: &Self::Handle) -> Result<Option<Self::Handle>, String>;
    fn pointer_access(handle: &Self::Handle) -> Result<i64, String>;
    fn alignment(handle: &Self::Handle) -> Result<i64, String>;
    fn data_size(handle: &Self::Handle) -> Result<i64, String>;
    fn element_is_argument_buffer(handle: &Self::Handle) -> Result<bool, String>;
    fn members(handle: &Self::Handle) -> Result<Vec<Self::Handle>, String>;
    fn member_name(handle: &Self::Handle) -> Result<String, String>;
    fn member_offset(handle: &Self::Handle) -> Result<i64, String>;
    fn member_argument_index(handle: &Self::Handle) -> Result<i64, String>;
    fn member_type(handle: &Self::Handle) -> Result<Option<Self::Handle>, String>;
    fn texture_access(handle: &Self::Handle) -> Result<i64, String>;
    fn texture_type(handle: &Self::Handle) -> Result<i64, String>;
    fn is_depth_texture(handle: &Self::Handle) -> Result<bool, String>;
    fn tensor_access(handle: &Self::Handle) -> Result<i64, String>;
    fn tensor_data_type(handle: &Self::Handle) -> Result<i64, String>;
    fn tensor_index_type(handle: &Self::Handle) -> Result<i64, String>;
    fn tensor_dimensions(handle: &Self::Handle) -> Result<Option<Vec<i64>>, String>;
    fn argument_name(handle: &Self::Handle) -> Result<String, String>;
    fn argument_index(handle: &Self::Handle) -> Result<i64, String>;
    fn argument_active(handle: &Self::Handle) -> Result<bool, String>;
    fn argument_access(handle: &Self::Handle) -> Result<i64, String>;
    fn argument_type(handle: &Self::Handle) -> Result<i64, String>;
    fn argument_array_length(handle: &Self::Handle) -> Result<i64, String>;
    fn argument_buffer_alignment(handle: &Self::Handle) -> Result<i64, String>;
    fn argument_buffer_data_size(handle: &Self::Handle) -> Result<i64, String>;
    fn argument_buffer_data_type(handle: &Self::Handle) -> Result<i64, String>;
    fn argument_texture_data_type(handle: &Self::Handle) -> Result<i64, String>;
    fn argument_texture_type(handle: &Self::Handle) -> Result<i64, String>;
    fn argument_is_depth_texture(handle: &Self::Handle) -> Result<bool, String>;
    fn argument_threadgroup_memory_alignment(handle: &Self::Handle) -> Result<i64, String>;
    fn argument_threadgroup_memory_data_size(handle: &Self::Handle) -> Result<i64, String>;
    fn argument_buffer_pointer_type(handle: &Self::Handle) -> Result<Option<Self::Handle>, String>;
    fn argument_buffer_struct_type(handle: &Self::Handle) -> Result<Option<Self::Handle>, String>;
}

struct Owned<R: Raw> { handle: Option<R::Handle>, _raw: PhantomData<R> }
impl<R: Raw> Owned<R> {
    fn new(handle: R::Handle) -> Self { Self { handle: Some(handle), _raw: PhantomData } }
    fn get(&self) -> &R::Handle { self.handle.as_ref().expect("owned reflection handle") }
}
impl<R: Raw> Drop for Owned<R> { fn drop(&mut self) { if let Some(handle) = self.handle.take() { R::destroy(handle); } } }

fn validate(limits: Limits) -> Result<(), String> {
    if limits.max_depth > i64::MAX as usize || limits.max_members > i64::MAX as usize {
        Err("Metal reflection limits exceed native bounds".to_owned())
    } else { Ok(()) }
}

fn with_owned<R: Raw, T>(handle: Option<R::Handle>, f: impl FnOnce(&R::Handle) -> Result<T, String>) -> Result<Option<T>, String> {
    match handle { None => Ok(None), Some(handle) => { let owned = Owned::<R>::new(handle); f(owned.get()).map(Some) } }
}

pub fn reflected_type<R: Raw>(root: &R::Handle) -> Result<ReflectedType, String> {
    reflected_type_with_limits::<R>(root, DEFAULT_LIMITS)
}

pub fn reflected_type_with_limits<R: Raw>(root: &R::Handle, limits: Limits) -> Result<ReflectedType, String> {
    validate(limits)?;
    let mut remaining = limits.max_members;
    type_at::<R>(0, root, limits, &mut remaining)
}

fn type_at<R: Raw>(depth: usize, handle: &R::Handle, limits: Limits, remaining: &mut usize) -> Result<ReflectedType, String> {
    if depth > limits.max_depth { return Err("Metal reflection exceeded max_depth".to_owned()); }
    let data_type = R::data_type(handle)?;
    match R::kind(handle)? {
        Kind::Scalar => Ok(ReflectedType::Scalar(data_type)),
        Kind::Array => {
            let length = R::array_length(handle)?;
            let stride = R::array_stride(handle)?;
            let argument_index_stride = R::argument_index_stride(handle)?;
            let child = R::element(handle)?;
            let element = with_owned::<R, _>(child, |child| type_at::<R>(depth + 1, child, limits, remaining))?.map(Box::new);
            Ok(ReflectedType::Array { data_type, length, stride, argument_index_stride, element })
        }
        Kind::Pointer => {
            let access = R::pointer_access(handle)?;
            let alignment = R::alignment(handle)?;
            let data_size = R::data_size(handle)?;
            let element_is_argument_buffer = R::element_is_argument_buffer(handle)?;
            let child = R::element(handle)?;
            let element = with_owned::<R, _>(child, |child| type_at::<R>(depth + 1, child, limits, remaining))?.map(Box::new);
            Ok(ReflectedType::Pointer { data_type, access, alignment, data_size, element_is_argument_buffer, element })
        }
        Kind::Struct => {
            let members = R::members(handle)?;
            if members.len() > *remaining {
                for member in members { R::destroy(member); }
                return Err("Metal reflection exceeded max_members".to_owned());
            }
            *remaining -= members.len();
            let mut output = Vec::with_capacity(members.len());
            let mut iter = members.into_iter();
            while let Some(member) = iter.next() {
                let owned = Owned::<R>::new(member);
                match member_at::<R>(depth + 1, owned.get(), limits, remaining) {
                    Ok(member) => output.push(member),
                    Err(error) => {
                        for remaining_member in iter { R::destroy(remaining_member); }
                        return Err(error);
                    }
                }
            }
            Ok(ReflectedType::Struct(output))
        }
        Kind::TextureReference => Ok(ReflectedType::TextureReference {
            data_type,
            access: R::texture_access(handle)?,
            texture_type: R::texture_type(handle)?,
            depth: R::is_depth_texture(handle)?,
        }),
        Kind::TensorReference => Ok(ReflectedType::TensorReference {
            data_type,
            access: R::tensor_access(handle)?,
            tensor_data_type: R::tensor_data_type(handle)?,
            index_type: R::tensor_index_type(handle)?,
            dimensions: R::tensor_dimensions(handle)?,
        }),
    }
}


fn member_at<R: Raw>(depth: usize, handle: &R::Handle, limits: Limits, remaining: &mut usize) -> Result<Member, String> {
    let name = R::member_name(handle)?;
    let offset = R::member_offset(handle)?;
    let argument_index = R::member_argument_index(handle)?;
    let data_type = R::data_type(handle)?;
    let child = R::member_type(handle)?;
    let reflected_type = with_owned::<R, _>(child, |child| type_at::<R>(depth, child, limits, remaining))?.map(Box::new);
    Ok(Member { name, offset, argument_index, data_type, reflected_type })
}


pub fn argument<R: Raw>(handle: &R::Handle) -> Result<Argument, String> {
    argument_with_limits::<R>(handle, DEFAULT_LIMITS)
}

pub fn argument_with_limits<R: Raw>(handle: &R::Handle, limits: Limits) -> Result<Argument, String> {
    validate(limits)?;
    let name = R::argument_name(handle)?;
    let index = R::argument_index(handle)?;
    let active = R::argument_active(handle)?;
    let access = R::argument_access(handle)?;
    let argument_type = R::argument_type(handle)?;
    let array_length = R::argument_array_length(handle)?;
    let buffer_alignment = R::argument_buffer_alignment(handle)?;
    let buffer_data_size = R::argument_buffer_data_size(handle)?;
    let buffer_data_type = R::argument_buffer_data_type(handle)?;
    let texture_data_type = R::argument_texture_data_type(handle)?;
    let texture_type = R::argument_texture_type(handle)?;
    let depth_texture = R::argument_is_depth_texture(handle)?;
    let threadgroup_memory_alignment = R::argument_threadgroup_memory_alignment(handle)?;
    let threadgroup_memory_data_size = R::argument_threadgroup_memory_data_size(handle)?;
    let buffer_pointer_type = with_owned::<R, _>(R::argument_buffer_pointer_type(handle)?, |value| reflected_type_with_limits::<R>(value, limits))?;
    let buffer_struct_type = with_owned::<R, _>(R::argument_buffer_struct_type(handle)?, |value| reflected_type_with_limits::<R>(value, limits))?;
    Ok(Argument {
        name, index, active, access, argument_type, array_length, buffer_alignment, buffer_data_size,
        buffer_data_type, texture_data_type, texture_type, depth_texture, threadgroup_memory_alignment,
        threadgroup_memory_data_size, buffer_pointer_type, buffer_struct_type,
    })
}
