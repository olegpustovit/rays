pub use super::{compile_library, create_function, library_function_names};

use super::{Error, Handle, NativeHandle};

pub fn argument_encoder_snapshot(handle: &Handle) -> Result<(Option<String>, u64, u64, u64), Error> {
    #[cfg(target_vendor = "apple")]
    {
        use std::ffi::{c_char, CStr};
        let mut label = std::ptr::null_mut();
        let mut length = 0u64;
        let mut alignment = 0u64;
        let mut device_id = 0u64;
        let mut message: *const c_char = std::ptr::null();
        let status = unsafe {
            rays_metal_argument_encoder_snapshot(
                handle.as_native_ptr()?, &mut label, &mut length, &mut alignment, &mut device_id, &mut message,
            )
        };
        if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
        let label = if label.is_null() { None } else {
            let value = unsafe { CStr::from_ptr(label) }.to_string_lossy().into_owned();
            unsafe { rays_metal_free_bytes(label); }
            Some(value)
        };
        Ok((label, length, alignment, device_id))
    }
    #[cfg(not(target_vendor = "apple"))]
    { let _ = handle; Err(Error::Unavailable) }
}

pub fn argument_encoder_set_buffer(
    encoder: &Handle, buffer: &Handle, offset: i64, start_offset: i64, array_element: i64,
) -> Result<(), Error> {
    #[cfg(target_vendor = "apple")]
    {
        use std::ffi::c_char;
        let mut message: *const c_char = std::ptr::null();
        let status = unsafe {
            rays_metal_argument_encoder_set_buffer(
                encoder.as_native_ptr()?, buffer.as_native_ptr()?, offset, start_offset, array_element, &mut message,
            )
        };
        if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
        Ok(())
    }
    #[cfg(not(target_vendor = "apple"))]
    { let _ = (encoder, buffer, offset, start_offset, array_element); Err(Error::Unavailable) }
}

pub fn argument_encoder_single(
    encoder: &Handle, tag: i32, resource: &Handle, offset: i64, index: i64,
) -> Result<(), Error> {
    #[cfg(target_vendor = "apple")]
    {
        use std::ffi::c_char;
        let mut message: *const c_char = std::ptr::null();
        let status = unsafe {
            rays_metal_argument_encoder_single(
                encoder.as_native_ptr()?, tag, resource.as_native_ptr()?, offset, index, &mut message,
            )
        };
        if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
        Ok(())
    }
    #[cfg(not(target_vendor = "apple"))]
    { let _ = (encoder, tag, resource, offset, index); Err(Error::Unavailable) }
}

#[cfg(target_vendor = "apple")]
unsafe extern "C" {
    fn rays_metal_argument_encoder_snapshot(
        encoder: *mut NativeHandle, label: *mut *mut std::ffi::c_char, encoded_length: *mut u64,
        alignment: *mut u64, device_registry_id: *mut u64, message: *mut *const std::ffi::c_char,
    ) -> i32;
    fn rays_metal_argument_encoder_set_buffer(
        encoder: *mut NativeHandle, buffer: *mut NativeHandle, offset: i64, start_offset: i64,
        array_element: i64, message: *mut *const std::ffi::c_char,
    ) -> i32;
    fn rays_metal_argument_encoder_single(
        encoder: *mut NativeHandle, tag: i32, resource: *mut NativeHandle, offset: i64, index: i64,
        message: *mut *const std::ffi::c_char,
    ) -> i32;
    fn rays_metal_free_bytes(bytes: *mut std::ffi::c_char);
}

/// A background compile of a library and its entry's compute pipeline (`library_warm`), which
/// only fills Metal's cache; released on drop, while or after it compiles.
pub struct Warm(*mut std::ffi::c_void);

impl Warm {
    /// None while compiling, then whether it compiled.
    pub fn state(&self) -> Option<bool> {
        #[cfg(target_vendor = "apple")]
        { match unsafe { rays_metal_warm_state(self.0) } { 0 => None, state => Some(state == 1) } }
        #[cfg(not(target_vendor = "apple"))]
        { Some(false) }
    }
}

impl Drop for Warm {
    fn drop(&mut self) {
        #[cfg(target_vendor = "apple")]
        unsafe { rays_metal_warm_release(self.0) }
    }
}

pub fn library_warm(device: &Handle, source: &str, entry: &str) -> Result<Warm, Error> {
    #[cfg(target_vendor = "apple")]
    {
        use std::ffi::{c_char, CString};
        let source = CString::new(source).map_err(|_| Error::InvalidArgument("Metal source may not contain NUL".into()))?;
        let entry = CString::new(entry).map_err(|_| Error::InvalidArgument("Metal entry may not contain NUL".into()))?;
        let mut output = std::ptr::null_mut();
        let mut message: *const c_char = std::ptr::null();
        let status = unsafe { rays_metal_library_warm(device.as_native_ptr()?, source.as_ptr(), entry.as_ptr(), &mut output, &mut message) };
        if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
        Ok(Warm(output))
    }
    #[cfg(not(target_vendor = "apple"))]
    { let _ = (device, source, entry); Err(Error::Unavailable) }
}

#[cfg(target_vendor = "apple")]
unsafe extern "C" {
    fn rays_metal_library_warm(device: *mut NativeHandle, source: *const std::ffi::c_char, entry: *const std::ffi::c_char, output: *mut *mut std::ffi::c_void, message: *mut *const std::ffi::c_char) -> i32;
    fn rays_metal_warm_state(warm: *mut std::ffi::c_void) -> i32;
    fn rays_metal_warm_release(warm: *mut std::ffi::c_void);
}
