use super::{Error, Handle};
#[cfg(target_vendor = "apple")]
use super::NativeHandle;

#[repr(C)]
#[derive(Clone, Copy, Debug)]
struct IndirectCommandBufferDescriptor {
    pub command_types: i64,
    pub inherit_buffers: u8,
    pub inherit_pipeline_state: u8,
    pub max_vertex_buffer_bind_count: i64,
    pub max_fragment_buffer_bind_count: i64,
    pub max_kernel_buffer_bind_count: i64,
    pub support_ray_tracing: u8,
    pub support_dynamic_attribute_stride: u8,
    pub max_kernel_threadgroup_memory_bind_count: i64,
    pub max_object_buffer_bind_count: i64,
    pub max_mesh_buffer_bind_count: i64,
    pub max_object_threadgroup_memory_bind_count: i64,
    pub inherit_depth_stencil_state: u8,
    pub inherit_depth_bias: u8,
    pub inherit_depth_clip_mode: u8,
    pub inherit_cull_mode: u8,
    pub inherit_front_facing_winding: u8,
    pub inherit_triangle_fill_mode: u8,
    pub support_color_attachment_mapping: u8,
}

pub fn create_indirect_command_buffer(device: &Handle, profile: &ogpu_core::validation::AdmittedIcb) -> Result<Handle, Error> {
    #[cfg(target_vendor = "apple")]
    {
        let d = profile.descriptor();
        let descriptor = IndirectCommandBufferDescriptor {
            command_types: profile.command_types_bits(),
            inherit_buffers: u8::from(d.inherit_buffers),
            inherit_pipeline_state: u8::from(d.inherit_pipeline_state),
            max_vertex_buffer_bind_count: d.max_vertex_buffer_bind_count as i64,
            max_fragment_buffer_bind_count: d.max_fragment_buffer_bind_count as i64,
            max_kernel_buffer_bind_count: d.max_kernel_buffer_bind_count as i64,
            support_ray_tracing: u8::from(d.support_ray_tracing),
            support_dynamic_attribute_stride: u8::from(d.support_dynamic_attribute_stride),
            max_kernel_threadgroup_memory_bind_count: d.max_kernel_threadgroup_memory_bind_count as i64,
            max_object_buffer_bind_count: d.max_object_buffer_bind_count as i64,
            max_mesh_buffer_bind_count: d.max_mesh_buffer_bind_count as i64,
            max_object_threadgroup_memory_bind_count: d.max_object_threadgroup_memory_bind_count as i64,
            inherit_depth_stencil_state: u8::from(d.inherit_depth_stencil_state),
            inherit_depth_bias: u8::from(d.inherit_depth_bias),
            inherit_depth_clip_mode: u8::from(d.inherit_depth_clip_mode),
            inherit_cull_mode: u8::from(d.inherit_cull_mode),
            inherit_front_facing_winding: u8::from(d.inherit_front_facing_winding),
            inherit_triangle_fill_mode: u8::from(d.inherit_triangle_fill_mode),
            support_color_attachment_mapping: u8::from(d.support_color_attachment_mapping),
        };
        let max_command_count = profile.capacity() as u64;
        let options = (2 << 4) | (2 << 8);
        use std::ffi::c_char;
        let mut output = std::ptr::null_mut();
        let mut message: *const c_char = std::ptr::null();
        let status = unsafe {
            rays_metal_indirect_command_buffer_create(
                device.as_native_ptr()?, &descriptor, max_command_count, options, &mut output, &mut message,
            )
        };
        if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
        unsafe { Handle::from_native(output) }
    }
    #[cfg(not(target_vendor = "apple"))]
    { let _ = (device, profile); Err(Error::Unavailable) }
}

pub fn draw_indexed(
    command:&crate::registry::BoundRenderCommand,primitive:ogpu_core::render_pass::Primitive,index_buffer:&Handle,
    range:&ogpu_core::validation::AdmittedIndexRange,base_vertex:i64,base_instance:u64,
)->Result<(),Error>{
    ogpu_core::validation::validate_base_vertex("Metal.Indirect_command_buffer.draw_indexed",base_vertex)
        .map_err(|failure|Error::InvalidArgument(failure.message.into()))?;
    #[cfg(target_vendor="apple")] {
        use ogpu_core::render_pass::{Primitive,IndexType};
        let primitive=match primitive {Primitive::PointList=>0,Primitive::LineList=>1,Primitive::LineStrip=>2,Primitive::TriangleList=>3,Primitive::TriangleStrip=>4};
        let index_type=match range.index_type(){IndexType::Uint16=>0,IndexType::Uint32=>1};
        let mut message=std::ptr::null();
        let status=unsafe{rays_metal_indirect_render_draw_indexed(command.raw().as_native_ptr()?,command.parent().as_native_ptr()?,command.device_id(),primitive,range.count(),index_type,
            index_buffer.as_native_ptr()?,range.offset(),range.instances(),base_vertex,base_instance,&mut message)};
        if status==0 {Ok(())}else{Err(unsafe{Error::from_native(status,message)})}
    }
    #[cfg(not(target_vendor="apple"))] {let _=(command,primitive,index_buffer,range,base_vertex,base_instance);Err(Error::Unavailable)}
}

#[cfg(target_vendor = "apple")]
unsafe extern "C" {
    fn rays_metal_indirect_command_buffer_create(
        device: *mut NativeHandle,
        descriptor: *const IndirectCommandBufferDescriptor,
        max_command_count: u64,
        options: i64,
        output: *mut *mut NativeHandle,
        message: *mut *const std::ffi::c_char,
    ) -> i32;
    fn rays_metal_indirect_render_draw_indexed(
        command: *mut NativeHandle, icb: *mut NativeHandle, device_id:u64, primitive: i32, index_count: u64, index_type: i32,
        index_buffer: *mut NativeHandle, index_offset: u64, instance_count: u64,
        base_vertex: i64, base_instance: u64, message: *mut *const std::ffi::c_char,
    ) -> i32;
}

// ogpu_metal: execute a range of an indirect command buffer on a render encoder.
#[cfg(target_vendor = "apple")]
unsafe extern "C" {
    fn rays_metal_render_encoder_execute_icb_range(encoder: *mut NativeHandle, icb: *mut NativeHandle, location: u64, length: u64, message: *mut *const std::ffi::c_char) -> i32;
}
pub fn execute_icb_range(encoder: &Handle, icb: &Handle, location: u64, length: u64) -> Result<(), Error> {
    #[cfg(target_vendor = "apple")]
    {
        let mut message: *const std::ffi::c_char = std::ptr::null();
        let status = unsafe { rays_metal_render_encoder_execute_icb_range(encoder.as_native_ptr()?, icb.as_native_ptr()?, location, length, &mut message) };
        if status == 0 { Ok(()) } else { Err(unsafe { Error::from_native(status, message) }) }
    }
    #[cfg(not(target_vendor = "apple"))]
    { let _ = (encoder, icb, location, length); Err(Error::Unavailable) }
}
