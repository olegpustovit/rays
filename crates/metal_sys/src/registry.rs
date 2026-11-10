// Rust Metal bindings. Keep in sync with native/generated_metal.mm.
use super::{Error, Handle};
use std::ffi::{CStr, CString};
use std::mem::MaybeUninit;

#[allow(non_snake_case)] pub mod record {
    #[repr(C)]
    #[derive(Clone, Copy, Debug, Default, PartialEq)]
    pub struct Mtl_viewport {
        pub originX: f64,
        pub originY: f64,
        pub width: f64,
        pub height: f64,
        pub znear: f64,
        pub zfar: f64,
    }
    #[repr(C)]
    #[derive(Clone, Copy, Debug, Default, PartialEq)]
    pub struct Mtl_scissor_rect {
        pub x: u64,
        pub y: u64,
        pub width: u64,
        pub height: u64,
    }
    #[repr(C)]
    #[derive(Clone, Copy, Debug, Default, PartialEq)]
    pub struct Mtl_size_int {
        pub width: usize,
        pub height: usize,
        pub depth: usize,
    }
}
pub fn device_supports_family(receiver: &Handle, arg0: i64) -> Result<bool, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<u8> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_device_supports_family(receiver_ptr, arg0, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { output.assume_init() != 0 })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_device_supports_family"] fn ffi_rays_metal_device_supports_family(receiver: *mut super::NativeHandle, arg0: i64, output: *mut u8, message: *mut *const std::ffi::c_char) -> i32; }
pub fn device_max_threadgroup_memory_length(receiver: &Handle) -> Result<u64, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<u64> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_device_max_threadgroup_memory_length(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { output.assume_init() })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_device_max_threadgroup_memory_length"] fn ffi_rays_metal_device_max_threadgroup_memory_length(receiver: *mut super::NativeHandle, output: *mut u64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn render_pipeline_state_support_indirect_command_buffers(receiver: &Handle) -> Result<bool, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<u8> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_render_pipeline_state_support_indirect_command_buffers(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { output.assume_init() != 0 })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_render_pipeline_state_support_indirect_command_buffers"] fn ffi_rays_metal_render_pipeline_state_support_indirect_command_buffers(receiver: *mut super::NativeHandle, output: *mut u8, message: *mut *const std::ffi::c_char) -> i32; }
pub fn device_name(receiver: &Handle) -> Result<String, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<*const std::ffi::c_char> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_device_name(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { CStr::from_ptr(output.assume_init()).to_string_lossy().into_owned() })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_device_name"] fn ffi_rays_metal_device_name(receiver: *mut super::NativeHandle, output: *mut *const std::ffi::c_char, message: *mut *const std::ffi::c_char) -> i32; }
pub fn device_registry_id(receiver: &Handle) -> Result<u64, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<u64> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_device_registry_id(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { output.assume_init() })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_device_registry_id"] fn ffi_rays_metal_device_registry_id(receiver: *mut super::NativeHandle, output: *mut u64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn device_is_low_power(receiver: &Handle) -> Result<bool, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<u8> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_device_is_low_power(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { output.assume_init() != 0 })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_device_is_low_power"] fn ffi_rays_metal_device_is_low_power(receiver: *mut super::NativeHandle, output: *mut u8, message: *mut *const std::ffi::c_char) -> i32; }
pub fn device_is_removable(receiver: &Handle) -> Result<bool, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<u8> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_device_is_removable(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { output.assume_init() != 0 })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_device_is_removable"] fn ffi_rays_metal_device_is_removable(receiver: *mut super::NativeHandle, output: *mut u8, message: *mut *const std::ffi::c_char) -> i32; }
pub fn device_is_headless(receiver: &Handle) -> Result<bool, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<u8> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_device_is_headless(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { output.assume_init() != 0 })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_device_is_headless"] fn ffi_rays_metal_device_is_headless(receiver: *mut super::NativeHandle, output: *mut u8, message: *mut *const std::ffi::c_char) -> i32; }
pub fn device_has_unified_memory(receiver: &Handle) -> Result<bool, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<u8> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_device_has_unified_memory(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { output.assume_init() != 0 })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_device_has_unified_memory"] fn ffi_rays_metal_device_has_unified_memory(receiver: *mut super::NativeHandle, output: *mut u8, message: *mut *const std::ffi::c_char) -> i32; }
pub fn device_recommended_max_working_set_size(receiver: &Handle) -> Result<u64, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<u64> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_device_recommended_max_working_set_size(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { output.assume_init() })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_device_recommended_max_working_set_size"] fn ffi_rays_metal_device_recommended_max_working_set_size(receiver: *mut super::NativeHandle, output: *mut u64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn device_current_allocated_size(receiver: &Handle) -> Result<u64, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<u64> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_device_current_allocated_size(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { output.assume_init() })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_device_current_allocated_size"] fn ffi_rays_metal_device_current_allocated_size(receiver: *mut super::NativeHandle, output: *mut u64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn device_max_buffer_length(receiver: &Handle) -> Result<u64, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<u64> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_device_max_buffer_length(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { output.assume_init() })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_device_max_buffer_length"] fn ffi_rays_metal_device_max_buffer_length(receiver: *mut super::NativeHandle, output: *mut u64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn device_supports_raytracing(receiver: &Handle) -> Result<bool, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<u8> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_device_supports_raytracing(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { output.assume_init() != 0 })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_device_supports_raytracing"] fn ffi_rays_metal_device_supports_raytracing(receiver: *mut super::NativeHandle, output: *mut u8, message: *mut *const std::ffi::c_char) -> i32; }
pub fn device_supports_raytracing_from_render(receiver: &Handle) -> Result<bool, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<u8> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_device_supports_raytracing_from_render(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { output.assume_init() != 0 })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_device_supports_raytracing_from_render"] fn ffi_rays_metal_device_supports_raytracing_from_render(receiver: *mut super::NativeHandle, output: *mut u8, message: *mut *const std::ffi::c_char) -> i32; }
pub fn device_supports_dynamic_libraries(receiver: &Handle) -> Result<bool, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<u8> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_device_supports_dynamic_libraries(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { output.assume_init() != 0 })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_device_supports_dynamic_libraries"] fn ffi_rays_metal_device_supports_dynamic_libraries(receiver: *mut super::NativeHandle, output: *mut u8, message: *mut *const std::ffi::c_char) -> i32; }
pub fn device_supports_texture_sample_count(receiver: &Handle, arg0: u64) -> Result<bool, Error> {
    ogpu_core::validation::validate_sample_query("Metal.Device.supports_texture_sample_count",arg0).map_err(|failure|Error::InvalidArgument(failure.message.into()))?;
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<u8> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_device_supports_texture_sample_count(receiver_ptr, arg0, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { output.assume_init() != 0 })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_device_supports_texture_sample_count"] fn ffi_rays_metal_device_supports_texture_sample_count(receiver: *mut super::NativeHandle, arg0: u64, output: *mut u8, message: *mut *const std::ffi::c_char) -> i32; }
pub fn command_queue_create(receiver: &Handle) -> Result<Handle, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<*mut super::NativeHandle> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_command_queue_create(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { Handle::from_native(output.assume_init())? })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_command_queue_create"] fn ffi_rays_metal_command_queue_create(receiver: *mut super::NativeHandle, output: *mut *mut super::NativeHandle, message: *mut *const std::ffi::c_char) -> i32; }
pub fn command_buffer_create(receiver: &Handle) -> Result<Handle, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<*mut super::NativeHandle> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_command_buffer_create(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { Handle::from_native(output.assume_init())? })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_command_buffer_create"] fn ffi_rays_metal_command_buffer_create(receiver: *mut super::NativeHandle, output: *mut *mut super::NativeHandle, message: *mut *const std::ffi::c_char) -> i32; }
pub fn command_buffer_status(receiver: &Handle) -> Result<i64, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<i64> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_command_buffer_status(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { output.assume_init() })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_command_buffer_status"] fn ffi_rays_metal_command_buffer_status(receiver: *mut super::NativeHandle, output: *mut i64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn command_buffer_commit(receiver: &Handle) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_command_buffer_commit(receiver_ptr, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_command_buffer_commit"] fn ffi_rays_metal_command_buffer_commit(receiver: *mut super::NativeHandle, message: *mut *const std::ffi::c_char) -> i32; }
pub fn command_buffer_compute_encoder(receiver: &Handle) -> Result<Handle, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<*mut super::NativeHandle> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_command_buffer_compute_encoder(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { Handle::from_native(output.assume_init())? })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_command_buffer_compute_encoder"] fn ffi_rays_metal_command_buffer_compute_encoder(receiver: *mut super::NativeHandle, output: *mut *mut super::NativeHandle, message: *mut *const std::ffi::c_char) -> i32; }
pub fn command_buffer_blit_encoder(receiver: &Handle) -> Result<Handle, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<*mut super::NativeHandle> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_command_buffer_blit_encoder(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { Handle::from_native(output.assume_init())? })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_command_buffer_blit_encoder"] fn ffi_rays_metal_command_buffer_blit_encoder(receiver: *mut super::NativeHandle, output: *mut *mut super::NativeHandle, message: *mut *const std::ffi::c_char) -> i32; }
pub fn command_buffer_acceleration_encoder(receiver: &Handle) -> Result<Handle, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<*mut super::NativeHandle> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_command_buffer_acceleration_encoder(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { Handle::from_native(output.assume_init())? })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_command_buffer_acceleration_encoder"] fn ffi_rays_metal_command_buffer_acceleration_encoder(receiver: *mut super::NativeHandle, output: *mut *mut super::NativeHandle, message: *mut *const std::ffi::c_char) -> i32; }
pub fn command_buffer_render_encoder_from_pass(receiver: &Handle, arg0: &Handle) -> Result<Handle, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<*mut super::NativeHandle> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_command_buffer_render_encoder_from_pass(receiver_ptr, arg0.as_native_ptr()?, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { Handle::from_native(output.assume_init())? })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_command_buffer_render_encoder_from_pass"] fn ffi_rays_metal_command_buffer_render_encoder_from_pass(receiver: *mut super::NativeHandle, arg0: *mut super::NativeHandle, output: *mut *mut super::NativeHandle, message: *mut *const std::ffi::c_char) -> i32; }
pub fn compute_encoder_end(receiver: &Handle) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_compute_encoder_end(receiver_ptr, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_compute_encoder_end"] fn ffi_rays_metal_compute_encoder_end(receiver: *mut super::NativeHandle, message: *mut *const std::ffi::c_char) -> i32; }
pub fn render_encoder_end(receiver: &Handle) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_render_encoder_end(receiver_ptr, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_render_encoder_end"] fn ffi_rays_metal_render_encoder_end(receiver: *mut super::NativeHandle, message: *mut *const std::ffi::c_char) -> i32; }
pub fn blit_encoder_end(receiver: &Handle) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_blit_encoder_end(receiver_ptr, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_blit_encoder_end"] fn ffi_rays_metal_blit_encoder_end(receiver: *mut super::NativeHandle, message: *mut *const std::ffi::c_char) -> i32; }
pub fn acceleration_encoder_end(receiver: &Handle) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_acceleration_encoder_end(receiver_ptr, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_acceleration_encoder_end"] fn ffi_rays_metal_acceleration_encoder_end(receiver: *mut super::NativeHandle, message: *mut *const std::ffi::c_char) -> i32; }
pub fn render_encoder_set_pipeline(receiver: &Handle, arg0: &Handle) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_render_encoder_set_pipeline(receiver_ptr, arg0.as_native_ptr()?, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_render_encoder_set_pipeline"] fn ffi_rays_metal_render_encoder_set_pipeline(receiver: *mut super::NativeHandle, arg0: *mut super::NativeHandle, message: *mut *const std::ffi::c_char) -> i32; }
pub fn render_encoder_set_vertex_buffer(receiver: &Handle, arg0: &Handle, arg1: u64, arg2: u64) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_render_encoder_set_vertex_buffer(receiver_ptr, arg0.as_native_ptr()?, arg1, arg2, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_render_encoder_set_vertex_buffer"] fn ffi_rays_metal_render_encoder_set_vertex_buffer(receiver: *mut super::NativeHandle, arg0: *mut super::NativeHandle, arg1: u64, arg2: u64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn render_encoder_set_fragment_buffer(receiver: &Handle, arg0: &Handle, arg1: u64, arg2: u64) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_render_encoder_set_fragment_buffer(receiver_ptr, arg0.as_native_ptr()?, arg1, arg2, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_render_encoder_set_fragment_buffer"] fn ffi_rays_metal_render_encoder_set_fragment_buffer(receiver: *mut super::NativeHandle, arg0: *mut super::NativeHandle, arg1: u64, arg2: u64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn render_encoder_set_vertex_texture(receiver: &Handle, arg0: &Handle, arg1: u64) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_render_encoder_set_vertex_texture(receiver_ptr, arg0.as_native_ptr()?, arg1, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_render_encoder_set_vertex_texture"] fn ffi_rays_metal_render_encoder_set_vertex_texture(receiver: *mut super::NativeHandle, arg0: *mut super::NativeHandle, arg1: u64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn render_encoder_set_fragment_texture(receiver: &Handle, arg0: &Handle, arg1: u64) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_render_encoder_set_fragment_texture(receiver_ptr, arg0.as_native_ptr()?, arg1, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_render_encoder_set_fragment_texture"] fn ffi_rays_metal_render_encoder_set_fragment_texture(receiver: *mut super::NativeHandle, arg0: *mut super::NativeHandle, arg1: u64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn render_encoder_set_vertex_sampler(receiver: &Handle, arg0: &Handle, arg1: u64) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_render_encoder_set_vertex_sampler(receiver_ptr, arg0.as_native_ptr()?, arg1, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_render_encoder_set_vertex_sampler"] fn ffi_rays_metal_render_encoder_set_vertex_sampler(receiver: *mut super::NativeHandle, arg0: *mut super::NativeHandle, arg1: u64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn render_encoder_set_fragment_sampler(receiver: &Handle, arg0: &Handle, arg1: u64) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_render_encoder_set_fragment_sampler(receiver_ptr, arg0.as_native_ptr()?, arg1, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_render_encoder_set_fragment_sampler"] fn ffi_rays_metal_render_encoder_set_fragment_sampler(receiver: *mut super::NativeHandle, arg0: *mut super::NativeHandle, arg1: u64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn render_depth_stencil(receiver: &Handle, arg0: Option<&Handle>) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_render_depth_stencil(receiver_ptr, match arg0 { Some(value) => value.as_native_ptr()?, None => std::ptr::null_mut() }, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_render_depth_stencil"] fn ffi_rays_metal_render_depth_stencil(receiver: *mut super::NativeHandle, arg0: *mut super::NativeHandle, message: *mut *const std::ffi::c_char) -> i32; }
pub fn render_encoder_set_viewport(receiver: &Handle, admitted: ogpu_core::render_pass::AdmittedViewport) -> Result<(), Error> {
    let v=admitted.viewport();
    let arg0=record::Mtl_viewport {originX:v.x,originY:v.y,width:v.width,height:v.height,znear:v.znear,zfar:v.zfar};
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_render_encoder_set_viewport(receiver_ptr, arg0, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_render_encoder_set_viewport"] fn ffi_rays_metal_render_encoder_set_viewport(receiver: *mut super::NativeHandle, arg0: record::Mtl_viewport, message: *mut *const std::ffi::c_char) -> i32; }
pub fn render_encoder_set_scissor(receiver: &Handle, arg0: record::Mtl_scissor_rect) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_render_encoder_set_scissor(receiver_ptr, arg0, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_render_encoder_set_scissor"] fn ffi_rays_metal_render_encoder_set_scissor(receiver: *mut super::NativeHandle, arg0: record::Mtl_scissor_rect, message: *mut *const std::ffi::c_char) -> i32; }
pub fn render_encoder_set_cull_mode(receiver: &Handle, arg0: i64) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_render_encoder_set_cull_mode(receiver_ptr, arg0, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_render_encoder_set_cull_mode"] fn ffi_rays_metal_render_encoder_set_cull_mode(receiver: *mut super::NativeHandle, arg0: i64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn render_encoder_set_winding(receiver: &Handle, arg0: i64) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_render_encoder_set_winding(receiver_ptr, arg0, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_render_encoder_set_winding"] fn ffi_rays_metal_render_encoder_set_winding(receiver: *mut super::NativeHandle, arg0: i64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn render_encoder_draw_primitives(receiver: &Handle, arg0: i64, arg1: u64, arg2: u64, arg3: u64) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_render_encoder_draw_primitives(receiver_ptr, arg0, arg1, arg2, arg3, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_render_encoder_draw_primitives"] fn ffi_rays_metal_render_encoder_draw_primitives(receiver: *mut super::NativeHandle, arg0: i64, arg1: u64, arg2: u64, arg3: u64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn render_draw_indexed_basic(receiver: &Handle, arg0: i64, arg1: u64, arg2: i64, arg3: &Handle, arg4: u64) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_render_draw_indexed_basic(receiver_ptr, arg0, arg1, arg2, arg3.as_native_ptr()?, arg4, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_render_draw_indexed_basic"] fn ffi_rays_metal_render_draw_indexed_basic(receiver: *mut super::NativeHandle, arg0: i64, arg1: u64, arg2: i64, arg3: *mut super::NativeHandle, arg4: u64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn render_draw_indexed_instances(receiver: &Handle, arg0: i64, arg1: u64, arg2: i64, arg3: &Handle, arg4: u64, arg5: u64) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_render_draw_indexed_instances(receiver_ptr, arg0, arg1, arg2, arg3.as_native_ptr()?, arg4, arg5, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_render_draw_indexed_instances"] fn ffi_rays_metal_render_draw_indexed_instances(receiver: *mut super::NativeHandle, arg0: i64, arg1: u64, arg2: i64, arg3: *mut super::NativeHandle, arg4: u64, arg5: u64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn render_draw_indexed_indirect(receiver: &Handle, arg0: i64, arg1: i64, arg2: &Handle, arg3: u64, arg4: &Handle, arg5: u64) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_render_draw_indexed_indirect(receiver_ptr, arg0, arg1, arg2.as_native_ptr()?, arg3, arg4.as_native_ptr()?, arg5, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_render_draw_indexed_indirect"] fn ffi_rays_metal_render_draw_indexed_indirect(receiver: *mut super::NativeHandle, arg0: i64, arg1: i64, arg2: *mut super::NativeHandle, arg3: u64, arg4: *mut super::NativeHandle, arg5: u64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn buffer_create(receiver: &Handle, arg0: u64, arg1: i64) -> Result<Handle, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<*mut super::NativeHandle> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_buffer_create(receiver_ptr, arg0, arg1, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { Handle::from_native(output.assume_init())? })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_buffer_create"] fn ffi_rays_metal_buffer_create(receiver: *mut super::NativeHandle, arg0: u64, arg1: i64, output: *mut *mut super::NativeHandle, message: *mut *const std::ffi::c_char) -> i32; }
pub fn buffer_info_length(receiver: &Handle) -> Result<u64, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<u64> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_buffer_info_length(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { output.assume_init() })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_buffer_info_length"] fn ffi_rays_metal_buffer_info_length(receiver: *mut super::NativeHandle, output: *mut u64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn buffer_info_storage_mode(receiver: &Handle) -> Result<i64, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<i64> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_buffer_info_storageMode(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { output.assume_init() })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_buffer_info_storageMode"] fn ffi_rays_metal_buffer_info_storageMode(receiver: *mut super::NativeHandle, output: *mut i64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn buffer_info_cpu_cache_mode(receiver: &Handle) -> Result<i64, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<i64> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_buffer_info_cpuCacheMode(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { output.assume_init() })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_buffer_info_cpuCacheMode"] fn ffi_rays_metal_buffer_info_cpuCacheMode(receiver: *mut super::NativeHandle, output: *mut i64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn buffer_info_hazard_tracking_mode(receiver: &Handle) -> Result<i64, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<i64> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_buffer_info_hazardTrackingMode(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { output.assume_init() })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_buffer_info_hazardTrackingMode"] fn ffi_rays_metal_buffer_info_hazardTrackingMode(receiver: *mut super::NativeHandle, output: *mut i64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn set_buffer_label(receiver: &Handle, arg0: &str) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let arg0_cstring = CString::new(arg0).map_err(|_| Error::InvalidArgument("Metal strings must not contain NUL".into()))?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_set_buffer_label(receiver_ptr, arg0_cstring.as_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_set_buffer_label"] fn ffi_rays_metal_set_buffer_label(receiver: *mut super::NativeHandle, arg0: *const std::ffi::c_char, message: *mut *const std::ffi::c_char) -> i32; }
pub fn texture_is_shareable(receiver: &Handle) -> Result<bool, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<u8> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_texture_is_shareable(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { output.assume_init() != 0 })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_texture_is_shareable"] fn ffi_rays_metal_texture_is_shareable(receiver: *mut super::NativeHandle, output: *mut u8, message: *mut *const std::ffi::c_char) -> i32; }
// Only acquisition can bind a command to the allocation that produced it.
pub struct BoundRenderCommand { raw:Handle, parent:Handle, device_id:u64 }
impl BoundRenderCommand {
    pub(crate) fn raw(&self)->&Handle { &self.raw }
    pub(crate) fn parent(&self)->&Handle { &self.parent }
    pub(crate) fn device_id(&self)->u64 { self.device_id }
}
pub fn indirect_render_command(receiver: &Handle, arg0: u64) -> Result<BoundRenderCommand, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut device_id=0;
    let mut output: MaybeUninit<*mut super::NativeHandle> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_indirect_render_command(receiver_ptr, arg0, output.as_mut_ptr(), &mut device_id, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(BoundRenderCommand {raw:unsafe { Handle::from_native(output.assume_init())? },parent:receiver.clone(),device_id})
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_indirect_render_command"] fn ffi_rays_metal_indirect_render_command(receiver: *mut super::NativeHandle, arg0: u64, output: *mut *mut super::NativeHandle, device_id: *mut u64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn indirect_render_command_set_pipeline(receiver: &BoundRenderCommand, arg0: &Handle) -> Result<(), Error> {
    let receiver_ptr = receiver.raw.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_indirect_render_command_set_pipeline(receiver_ptr, arg0.as_native_ptr()?, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_indirect_render_command_set_pipeline"] fn ffi_rays_metal_indirect_render_command_set_pipeline(receiver: *mut super::NativeHandle, arg0: *mut super::NativeHandle, message: *mut *const std::ffi::c_char) -> i32; }
pub fn indirect_render_command_set_vertex_buffer(receiver: &BoundRenderCommand, arg0: &Handle, arg1: u64, arg2: usize) -> Result<(), Error> {
    let receiver_ptr = receiver.raw.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_indirect_render_command_set_vertex_buffer(receiver_ptr, arg0.as_native_ptr()?, arg1, arg2, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_indirect_render_command_set_vertex_buffer"] fn ffi_rays_metal_indirect_render_command_set_vertex_buffer(receiver: *mut super::NativeHandle, arg0: *mut super::NativeHandle, arg1: u64, arg2: usize, message: *mut *const std::ffi::c_char) -> i32; }
pub fn indirect_render_command_set_fragment_buffer(receiver: &BoundRenderCommand, arg0: &Handle, arg1: u64, arg2: usize) -> Result<(), Error> {
    let receiver_ptr = receiver.raw.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_indirect_render_command_set_fragment_buffer(receiver_ptr, arg0.as_native_ptr()?, arg1, arg2, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_indirect_render_command_set_fragment_buffer"] fn ffi_rays_metal_indirect_render_command_set_fragment_buffer(receiver: *mut super::NativeHandle, arg0: *mut super::NativeHandle, arg1: u64, arg2: usize, message: *mut *const std::ffi::c_char) -> i32; }
pub fn indirect_render_command_draw_primitives(receiver: &BoundRenderCommand, arg0: i64, arg1: u64, arg2: u64, arg3: u64, arg4: u64) -> Result<(), Error> {
    let receiver_ptr = receiver.raw.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_indirect_render_command_draw_primitives(receiver_ptr, arg0, arg1, arg2, arg3, arg4, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_indirect_render_command_draw_primitives"] fn ffi_rays_metal_indirect_render_command_draw_primitives(receiver: *mut super::NativeHandle, arg0: i64, arg1: u64, arg2: u64, arg3: u64, arg4: u64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn function_kind(receiver: &Handle) -> Result<i64, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<i64> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_function_kind(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { output.assume_init() })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_function_kind"] fn ffi_rays_metal_function_kind(receiver: *mut super::NativeHandle, output: *mut i64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn compute_pipeline_max_total_threads(receiver: &Handle) -> Result<u64, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<u64> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_compute_pipeline_max_total_threads(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { output.assume_init() })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_compute_pipeline_max_total_threads"] fn ffi_rays_metal_compute_pipeline_max_total_threads(receiver: *mut super::NativeHandle, output: *mut u64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn acceleration_structure_create(receiver: &Handle, size: std::num::NonZeroU64) -> Result<Handle, Error> {
    let arg0=size.get();
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<*mut super::NativeHandle> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_acceleration_structure_create(receiver_ptr, arg0, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { Handle::from_native(output.assume_init())? })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_acceleration_structure_create"] fn ffi_rays_metal_acceleration_structure_create(receiver: *mut super::NativeHandle, arg0: u64, output: *mut *mut super::NativeHandle, message: *mut *const std::ffi::c_char) -> i32; }
pub fn acceleration_encoder_copy_and_compact(receiver: &Handle, arg0: &Handle, arg1: &Handle) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_acceleration_encoder_copy_and_compact(receiver_ptr, arg0.as_native_ptr()?, arg1.as_native_ptr()?, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_acceleration_encoder_copy_and_compact"] fn ffi_rays_metal_acceleration_encoder_copy_and_compact(receiver: *mut super::NativeHandle, arg0: *mut super::NativeHandle, arg1: *mut super::NativeHandle, message: *mut *const std::ffi::c_char) -> i32; }
pub fn layer_next_drawable(receiver: &Handle) -> Result<Option<Handle>, Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<*mut super::NativeHandle> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_layer_next_drawable(receiver_ptr, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { let pointer = output.assume_init(); if pointer.is_null() { None } else { Some(Handle::from_native(pointer)?) } })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_layer_next_drawable"] fn ffi_rays_metal_layer_next_drawable(receiver: *mut super::NativeHandle, output: *mut *mut super::NativeHandle, message: *mut *const std::ffi::c_char) -> i32; }
pub fn command_buffer_present(receiver: &Handle, arg0: &Handle) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_command_buffer_present(receiver_ptr, arg0.as_native_ptr()?, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_command_buffer_present"] fn ffi_rays_metal_command_buffer_present(receiver: *mut super::NativeHandle, arg0: *mut super::NativeHandle, message: *mut *const std::ffi::c_char) -> i32; }
pub fn command_buffer_present_at_time(receiver: &Handle, arg0: &Handle, arg1: f64) -> Result<(), Error> {
    ogpu_core::surface::validate_present_time("Metal.Command_buffer.present",arg1).map_err(|failure|Error::InvalidArgument(failure.message.into()))?;
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_command_buffer_present_at_time(receiver_ptr, arg0.as_native_ptr()?, arg1, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_command_buffer_present_at_time"] fn ffi_rays_metal_command_buffer_present_at_time(receiver: *mut super::NativeHandle, arg0: *mut super::NativeHandle, arg1: f64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn command_buffer_present_after_duration(receiver: &Handle, arg0: &Handle, arg1: f64) -> Result<(), Error> {
    ogpu_core::surface::validate_present_time("Metal.Command_buffer.present",arg1).map_err(|failure|Error::InvalidArgument(failure.message.into()))?;
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_command_buffer_present_after_duration(receiver_ptr, arg0.as_native_ptr()?, arg1, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_command_buffer_present_after_duration"] fn ffi_rays_metal_command_buffer_present_after_duration(receiver: *mut super::NativeHandle, arg0: *mut super::NativeHandle, arg1: f64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn render_pass_descriptor_create() -> Result<Handle, Error> {
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<*mut super::NativeHandle> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_render_pass_descriptor_create(output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { Handle::from_native(output.assume_init())? })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_render_pass_descriptor_create"] fn ffi_rays_metal_render_pass_descriptor_create(output: *mut *mut super::NativeHandle, message: *mut *const std::ffi::c_char) -> i32; }
pub fn compute_encoder_set_pipeline(receiver: &Handle, arg0: &Handle) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_compute_encoder_set_pipeline(receiver_ptr, arg0.as_native_ptr()?, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_compute_encoder_set_pipeline"] fn ffi_rays_metal_compute_encoder_set_pipeline(receiver: *mut super::NativeHandle, arg0: *mut super::NativeHandle, message: *mut *const std::ffi::c_char) -> i32; }
pub fn compute_encoder_set_buffer(receiver: &Handle, arg0: &Handle, arg1: u64, arg2: usize) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_compute_encoder_set_buffer(receiver_ptr, arg0.as_native_ptr()?, arg1, arg2, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_compute_encoder_set_buffer"] fn ffi_rays_metal_compute_encoder_set_buffer(receiver: *mut super::NativeHandle, arg0: *mut super::NativeHandle, arg1: u64, arg2: usize, message: *mut *const std::ffi::c_char) -> i32; }
pub fn compute_encoder_set_texture(receiver: &Handle, arg0: &Handle, arg1: usize) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_compute_encoder_set_texture(receiver_ptr, arg0.as_native_ptr()?, arg1, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_compute_encoder_set_texture"] fn ffi_rays_metal_compute_encoder_set_texture(receiver: *mut super::NativeHandle, arg0: *mut super::NativeHandle, arg1: usize, message: *mut *const std::ffi::c_char) -> i32; }
pub fn compute_encoder_dispatch(receiver: &Handle, arg0: record::Mtl_size_int, arg1: record::Mtl_size_int) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_compute_encoder_dispatch(receiver_ptr, arg0, arg1, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_compute_encoder_dispatch"] fn ffi_rays_metal_compute_encoder_dispatch(receiver: *mut super::NativeHandle, arg0: record::Mtl_size_int, arg1: record::Mtl_size_int, message: *mut *const std::ffi::c_char) -> i32; }
pub fn compute_encoder_dispatch_indirect(receiver: &Handle, arg0: &Handle, arg1: u64, arg2: record::Mtl_size_int) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_compute_encoder_dispatch_indirect(receiver_ptr, arg0.as_native_ptr()?, arg1, arg2, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_compute_encoder_dispatch_indirect"] fn ffi_rays_metal_compute_encoder_dispatch_indirect(receiver: *mut super::NativeHandle, arg0: *mut super::NativeHandle, arg1: u64, arg2: record::Mtl_size_int, message: *mut *const std::ffi::c_char) -> i32; }
pub fn compute35_acceleration(receiver: &Handle, arg0: Option<&Handle>, arg1: u64) -> Result<(), Error> {
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_compute35_acceleration(receiver_ptr, match arg0 { Some(value) => value.as_native_ptr()?, None => std::ptr::null_mut() }, arg1, &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(())
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_compute35_acceleration"] fn ffi_rays_metal_compute35_acceleration(receiver: *mut super::NativeHandle, arg0: *mut super::NativeHandle, arg1: u64, message: *mut *const std::ffi::c_char) -> i32; }
pub fn shader_function_argument_encoder(receiver: &Handle, index: ogpu_core::validation::AdmittedSlot) -> Result<Handle, Error> {
    let arg0=index.index() as u64;
    let receiver_ptr = receiver.as_native_ptr()?;
    let mut message = std::ptr::null();
    let mut output: MaybeUninit<*mut super::NativeHandle> = MaybeUninit::uninit();
    #[cfg(target_vendor = "apple")] let status = unsafe { ffi_rays_metal_shader_function_argument_encoder(receiver_ptr, arg0, output.as_mut_ptr(), &mut message) };
    #[cfg(not(target_vendor = "apple"))] let status = 2;
    if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
    Ok(unsafe { Handle::from_native(output.assume_init())? })
}
#[cfg(target_vendor = "apple")] unsafe extern "C" { #[link_name = "rays_metal_shader_function_argument_encoder"] fn ffi_rays_metal_shader_function_argument_encoder(receiver: *mut super::NativeHandle, arg0: u64, output: *mut *mut super::NativeHandle, message: *mut *const std::ffi::c_char) -> i32; }
