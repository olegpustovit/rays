use super::{Error, Handle};
#[cfg(target_vendor = "apple")]
use super::NativeHandle;
#[cfg(target_vendor = "apple")]
use std::ffi::{c_char, CStr, CString, c_void};

#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct ColorAttachment {
    pub pixel_format: i32,
    pub blending_state: i32,
    pub source_rgb_blend_factor: i32,
    pub destination_rgb_blend_factor: i32,
    pub rgb_blend_operation: i32,
    pub source_alpha_blend_factor: i32,
    pub destination_alpha_blend_factor: i32,
    pub alpha_blend_operation: i32,
    pub write_mask: i32,
}
#[cfg(target_vendor = "apple")]
#[repr(C)]
struct RawResource { handle: *mut NativeHandle, kind: i32 }

pub fn render_encoder_use_resources(
    encoder: &Handle,
    resources: &[(i32, &Handle)],
    usage: i32,
    stages: i32,
) -> Result<(), Error> {
    ogpu_core::validation::validate_resource_use("Metal.RenderEncoder.use_resources", resources.len(), usage, stages)
        .map_err(|failure| Error::InvalidArgument(failure.message.into()))?;
    #[cfg(target_vendor = "apple")]
    {
        let resources = resources.iter().map(|(kind, handle)| {
            Ok(RawResource { handle: handle.as_native_ptr()?, kind: *kind })
        }).collect::<Result<Vec<_>, Error>>()?;
        let mut message = std::ptr::null();
        let status = unsafe {
            rays_metal_render_encoder_use_resources(
                encoder.as_native_ptr()?, resources.as_ptr(), resources.len(), usage, stages, &mut message,
            )
        };
        if status == 0 { Ok(()) } else { Err(unsafe { Error::from_native(status, message) }) }
    }
    #[cfg(not(target_vendor = "apple"))]
    { let _ = (encoder, resources, usage, stages); Err(Error::Unavailable) }
}

#[cfg(target_vendor = "apple")]
unsafe extern "C" {
    fn rays_metal_render_encoder_use_resources(
        encoder: *mut NativeHandle, resources: *const RawResource, count: usize,
        usage: i32, stages: i32, message: *mut *const c_char,
    ) -> i32;
}


#[cfg(target_vendor = "apple")]
#[repr(C)]
struct RawBinding {
    name: *mut c_char,
    binding_type: i32,
    access: i32,
    index: u64,
    used: u8,
    argument: u8,
    buffer_alignment: i64,
    buffer_data_size: i64,
    buffer_data_type: i32,
    texture_type: i32,
    texture_data_type: i32,
    depth: u8,
    array_length: i64,
    threadgroup_alignment: i64,
    threadgroup_data_size: i64,
    object_alignment: i64,
    object_data_size: i64,
    reflected_type: *mut c_void,
}

#[derive(Clone, Debug)]
pub struct Binding {
    pub name: String,
    pub binding_type: i32,
    pub access: i32,
    pub index: u64,
    pub used: bool,
    pub argument: bool,
    pub buffer_alignment: i64,
    pub buffer_data_size: i64,
    pub buffer_data_type: i32,
    pub texture_type: i32,
    pub texture_data_type: i32,
    pub depth: bool,
    pub array_length: i64,
    pub threadgroup_alignment: i64,
    pub threadgroup_data_size: i64,
    pub object_alignment: i64,
    pub object_data_size: i64,
    pub reflected_type: Option<super::reflection::ReflectedType>,
}

pub fn create_compiler(device: &Handle) -> Result<Handle, Error> {
    #[cfg(target_vendor = "apple")]
    {
        let mut output = std::ptr::null_mut();
        let mut message = std::ptr::null();
        let status = unsafe { rays_metal_compiler_create(device.as_native_ptr()?, &mut output, &mut message) };
        if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
        unsafe { Handle::from_native(output) }
    }
    #[cfg(not(target_vendor = "apple"))]
    { let _ = device; Err(Error::Unavailable) }
}

pub fn create_compute_pipeline(
    device: &Handle,
    function: &Handle,
    label: Option<&str>,
    reflection: bool,
) -> Result<(Handle, Vec<Binding>), Error> {
    #[cfg(target_vendor = "apple")]
    {
        let label = label.map(CString::new).transpose().map_err(|_| Error::InvalidArgument("Metal labels must not contain NUL".into()))?;
        let mut output = std::ptr::null_mut();
        let mut bindings = std::ptr::null_mut();
        let mut count = 0usize;
        let mut message = std::ptr::null();
        let status = unsafe {
            rays_metal_compute_pipeline_create_descriptor(
                device.as_native_ptr()?, function.as_native_ptr()?,
                label.as_ref().map_or(std::ptr::null(), |value| value.as_ptr()),
                u8::from(reflection), &mut output, &mut bindings, &mut count, &mut message,
            )
        };
        if status != 0 {
            unsafe { rays_metal_pipeline_reflection_free(bindings, count); }
            return Err(unsafe { Error::from_native(status, message) });
        }
        let result = unsafe { (Handle::from_native(output), bindings_from_raw(bindings, count)) };
        unsafe { rays_metal_pipeline_reflection_free(bindings, count); }
        Ok((result.0?, result.1?))
    }
    #[cfg(not(target_vendor = "apple"))]
    { let _ = (device, function, label, reflection); Err(Error::Unavailable) }
}

pub fn create_render_pipeline(compiler: &Handle, library: &Handle, profile: &ogpu_core::pipeline::AdmittedRenderPipeline) -> Result<(Handle, (Vec<Binding>, Vec<Binding>)), Error> {
    #[cfg(target_vendor = "apple")]
    {
        let options = profile.options();
        let label = options.label.as_deref();
        let vertex = profile.vertex();
        let fragment = options.fragment.as_deref();
        let reflection = options.reflection;
        let sample_count = options.raster_sample_count as u64;
        let color_attachments = options.color_attachments.as_ref().expect("Core admitted color attachments").iter().map(|attachment| ColorAttachment {
            pixel_format: super::pixel_format_code(attachment.format) as i32,
            blending_state: match attachment.blending { ogpu_core::pipeline::BlendState::Disabled => 0, ogpu_core::pipeline::BlendState::Enabled => 1 },
            source_rgb_blend_factor: blend_factor_code(attachment.source_rgb), destination_rgb_blend_factor: blend_factor_code(attachment.destination_rgb),
            rgb_blend_operation: blend_operation_code(attachment.rgb_operation), source_alpha_blend_factor: blend_factor_code(attachment.source_alpha),
            destination_alpha_blend_factor: blend_factor_code(attachment.destination_alpha), alpha_blend_operation: blend_operation_code(attachment.alpha_operation),
            write_mask: attachment.write_mask.iter().copied().fold(0, |bits, value| bits | color_write_bit(value)),
        }).collect::<Vec<_>>();
        let primitive_topology = match options.primitive_topology { ogpu_core::render_pass::Primitive::PointList => 1,
            ogpu_core::render_pass::Primitive::LineList | ogpu_core::render_pass::Primitive::LineStrip => 2, _ => 3 };
        let support_indirect_command_buffers = options.support_indirect_command_buffers;
        let label = label.map(CString::new).transpose().map_err(|_| Error::InvalidArgument("Metal labels must not contain NUL".into()))?;
        let vertex = CString::new(vertex).map_err(|_| Error::InvalidArgument("Metal function names must not contain NUL".into()))?;
        let fragment = fragment.map(CString::new).transpose().map_err(|_| Error::InvalidArgument("Metal function names must not contain NUL".into()))?;
        let mut output = std::ptr::null_mut();
        let mut message = std::ptr::null();
        let status = unsafe {
            rays_metal_compiler_create_render_pipeline(
                compiler.as_native_ptr()?, library.as_native_ptr()?,
                label.as_ref().map_or(std::ptr::null(), |value| value.as_ptr()),
                vertex.as_ptr(), fragment.as_ref().map_or(std::ptr::null(), |value| value.as_ptr()),
                u8::from(reflection), sample_count, color_attachments.as_ptr(), color_attachments.len(),
                primitive_topology, u8::from(support_indirect_command_buffers), &mut output, &mut message,
            )
        };
        if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
        let pipeline = unsafe { Handle::from_native(output)? };
        let reflection = if reflection {
            render_pipeline_reflection(&pipeline)?
        } else {
            (Vec::new(), Vec::new())
        };
        Ok((pipeline, reflection))
    }
    #[cfg(not(target_vendor = "apple"))]
    { let _ = (compiler, library, profile); Err(Error::Unavailable) }
}

pub fn render_pipeline_reflection(pipeline: &Handle) -> Result<(Vec<Binding>, Vec<Binding>), Error> {
    #[cfg(target_vendor = "apple")]
    {
        let mut vertex = std::ptr::null_mut();
        let mut vertex_count = 0usize;
        let mut fragment = std::ptr::null_mut();
        let mut fragment_count = 0usize;
        let mut message = std::ptr::null();
        let status = unsafe {
            rays_metal_render_pipeline_reflection(
                pipeline.as_native_ptr()?, &mut vertex, &mut vertex_count, &mut fragment, &mut fragment_count, &mut message,
            )
        };
        if status != 0 {
            unsafe { rays_metal_pipeline_reflection_free(vertex, vertex_count); }
            unsafe { rays_metal_pipeline_reflection_free(fragment, fragment_count); }
            return Err(unsafe { Error::from_native(status, message) });
        }
        let result = unsafe {
            let vertex = bindings_from_raw(vertex, vertex_count);
            let fragment = bindings_from_raw(fragment, fragment_count);
            vertex.and_then(|vertex| fragment.map(|fragment| (vertex, fragment)))
        };
        unsafe {
            rays_metal_pipeline_reflection_free(vertex, vertex_count);
            rays_metal_pipeline_reflection_free(fragment, fragment_count);
        }
        result
    }
    #[cfg(not(target_vendor = "apple"))]
    { let _ = pipeline; Err(Error::Unavailable) }
}

#[cfg(target_vendor = "apple")]
unsafe fn bindings_from_raw(pointer: *mut RawBinding, count: usize) -> Result<Vec<Binding>, Error> {
    if count != 0 && pointer.is_null() { return Err(Error::InvalidHandle); }
    if count == 0 { return Ok(Vec::new()); }
    let raw = unsafe { std::slice::from_raw_parts(pointer, count) };
    raw.iter().map(|binding| {
        if binding.name.is_null() { return Err(Error::InvalidHandle); }
        let name = unsafe { CStr::from_ptr(binding.name) }.to_string_lossy().into_owned();
        let reflected_type = unsafe { super::reflection::snapshot(binding.reflected_type) }?;
        Ok(Binding {
            name, binding_type: binding.binding_type, access: binding.access, index: binding.index,
            used: binding.used != 0, argument: binding.argument != 0,
            buffer_alignment: binding.buffer_alignment, buffer_data_size: binding.buffer_data_size,
            buffer_data_type: binding.buffer_data_type, texture_type: binding.texture_type,
            texture_data_type: binding.texture_data_type, depth: binding.depth != 0,
            array_length: binding.array_length, threadgroup_alignment: binding.threadgroup_alignment,
            threadgroup_data_size: binding.threadgroup_data_size, object_alignment: binding.object_alignment,
            object_data_size: binding.object_data_size, reflected_type,
        })
    }).collect()
}

pub fn copy_buffer_to_texture(
    encoder: &Handle, source: &Handle, source_offset: u64, source_bytes_per_row: u64, source_bytes_per_image: u64,
    destination: &Handle, destination_slice: u64, destination_level: u64,
    origin: (u64, u64, u64), size: (u64, u64, u64),
) -> Result<(), Error> {
    #[cfg(target_vendor = "apple")]
    {
        let mut message = std::ptr::null();
        let status = unsafe {
            rays_metal_blit_encoder_copy_buffer_to_texture(
                encoder.as_native_ptr()?, source.as_native_ptr()?, source_offset, source_bytes_per_row,
                source_bytes_per_image, destination.as_native_ptr()?, destination_slice, destination_level,
                origin.0, origin.1, origin.2, size.0, size.1, size.2, &mut message,
            )
        };
        if status == 0 { Ok(()) } else { Err(unsafe { Error::from_native(status, message) }) }
    }
    #[cfg(not(target_vendor = "apple"))]
    { let _ = (encoder, source, source_offset, source_bytes_per_row, source_bytes_per_image, destination, destination_slice, destination_level, origin, size); Err(Error::Unavailable) }
}

#[cfg(target_vendor = "apple")]
unsafe extern "C" {
    fn rays_metal_compute_pipeline_create_descriptor(
        device: *mut NativeHandle, function: *mut NativeHandle, label: *const c_char,
        reflection: u8, output: *mut *mut NativeHandle, bindings: *mut *mut RawBinding,
        binding_count: *mut usize, message: *mut *const c_char,
    ) -> i32;
    fn rays_metal_compiler_create(device: *mut NativeHandle, output: *mut *mut NativeHandle, message: *mut *const c_char) -> i32;
    fn rays_metal_compiler_create_render_pipeline(
        compiler: *mut NativeHandle, library: *mut NativeHandle, label: *const c_char, vertex: *const c_char,
        fragment: *const c_char, reflection: u8, sample_count: u64, color_attachments: *const ColorAttachment,
        color_attachment_count: usize, primitive_topology: i32, support_indirect_command_buffers: u8,
        output: *mut *mut NativeHandle, message: *mut *const c_char,
    ) -> i32;
    fn rays_metal_render_pipeline_reflection(
        pipeline: *mut NativeHandle, vertex: *mut *mut RawBinding, vertex_count: *mut usize,
        fragment: *mut *mut RawBinding, fragment_count: *mut usize, message: *mut *const c_char,
    ) -> i32;
    fn rays_metal_pipeline_reflection_free(bindings: *mut RawBinding, count: usize);
    fn rays_metal_blit_encoder_copy_buffer_to_texture(
        encoder: *mut NativeHandle, source: *mut NativeHandle, source_offset: u64, source_bytes_per_row: u64,
        source_bytes_per_image: u64, destination: *mut NativeHandle, destination_slice: u64,
        destination_level: u64, x: u64, y: u64, z: u64, width: u64, height: u64, depth: u64,
        message: *mut *const c_char,
    ) -> i32;
}

#[cfg(target_vendor = "apple")]
use ogpu_core::pipeline::{BlendFactor, BlendOperation, ColorWrite};
#[cfg(target_vendor = "apple")]
fn blend_factor_code(value: BlendFactor) -> i32 {
    match value {
        BlendFactor::Zero => 0, BlendFactor::One => 1, BlendFactor::SourceColor => 2, BlendFactor::OneMinusSourceColor => 3,
        BlendFactor::SourceAlpha => 4, BlendFactor::OneMinusSourceAlpha => 5, BlendFactor::DestinationColor => 6,
        BlendFactor::OneMinusDestinationColor => 7, BlendFactor::DestinationAlpha => 8, BlendFactor::OneMinusDestinationAlpha => 9,
        BlendFactor::SourceAlphaSaturated => 10, BlendFactor::Color => 11, BlendFactor::OneMinusColor => 12, BlendFactor::Alpha => 13,
        BlendFactor::OneMinusAlpha => 14, BlendFactor::Source1Color => 15, BlendFactor::OneMinusSource1Color => 16,
        BlendFactor::Source1Alpha => 17, BlendFactor::OneMinusSource1Alpha => 18,
    }
}
#[cfg(target_vendor = "apple")]
fn blend_operation_code(value: BlendOperation) -> i32 {
    match value { BlendOperation::Add => 0, BlendOperation::Subtract => 1, BlendOperation::ReverseSubtract => 2, BlendOperation::Min => 3, BlendOperation::Max => 4 }
}
#[cfg(target_vendor = "apple")]
fn color_write_bit(value: ColorWrite) -> i32 {
    match value { ColorWrite::Red => 8, ColorWrite::Green => 4, ColorWrite::Blue => 2, ColorWrite::Alpha => 1 }
}
