use super::{Error, Handle};
#[cfg(target_vendor = "apple")]
use super::NativeHandle;

#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct IndirectCommandBufferDescriptor {
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

pub fn create_indirect_command_buffer(
    device: &Handle,
    descriptor: &IndirectCommandBufferDescriptor,
    max_command_count: u64,
    options: i64,
) -> Result<Handle, Error> {
    #[cfg(target_vendor = "apple")]
    {
        use std::ffi::c_char;
        let mut output = std::ptr::null_mut();
        let mut message: *const c_char = std::ptr::null();
        let status = unsafe {
            rays_metal_indirect_command_buffer_create(
                device.as_native_ptr()?, descriptor, max_command_count, options, &mut output, &mut message,
            )
        };
        if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
        unsafe { Handle::from_native(output) }
    }
    #[cfg(not(target_vendor = "apple"))]
    { let _ = (device, descriptor, max_command_count, options); Err(Error::Unavailable) }
}

pub fn draw_indexed(
    command: &Handle, primitive: i32, index_count: u64, index_type: i32, index_buffer: &Handle,
    index_offset: u64, instance_count: u64, base_vertex: i64, base_instance: u64, device_registry_id: u64,
) -> Result<(), Error> {
    #[cfg(target_vendor = "apple")]
    {
        use std::ffi::c_char;
        let mut message: *const c_char = std::ptr::null();
        let status = unsafe {
            rays_metal_indirect_render_draw_indexed(
                command.as_native_ptr()?, primitive, index_count, index_type, index_buffer.as_native_ptr()?,
                index_offset, instance_count, base_vertex, base_instance, device_registry_id, &mut message,
            )
        };
        if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
        Ok(())
    }
    #[cfg(not(target_vendor = "apple"))]
    {
        let _ = (command, primitive, index_count, index_type, index_buffer, index_offset, instance_count, base_vertex, base_instance, device_registry_id);
        Err(Error::Unavailable)
    }
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
        command: *mut NativeHandle, primitive: i32, index_count: u64, index_type: i32,
        index_buffer: *mut NativeHandle, index_offset: u64, instance_count: u64,
        base_vertex: i64, base_instance: u64, device_registry_id: u64, message: *mut *const std::ffi::c_char,
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
