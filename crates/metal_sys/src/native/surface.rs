use super::{Error, Handle};
#[cfg(target_vendor = "apple")]
use super::NativeHandle;
#[cfg(target_vendor = "apple")]
use std::ffi::c_char;
use std::ffi::c_void;
use std::ptr::NonNull;
#[cfg(target_vendor = "apple")]
use std::ptr::{null, null_mut};

#[cfg(target_vendor = "apple")]
unsafe extern "C" {
    #[link_name = "caml_rays_metal_layer_adopt_borrowed"]
    fn layer_adopt_borrowed(
        device: *mut NativeHandle,
        layer: *mut c_void,
        output: *mut *mut NativeHandle,
        message: *mut *const c_char,
    ) -> i32;
    #[link_name = "caml_rays_metal_layer_configure"]
    fn layer_configure(
        layer: *mut NativeHandle,
        width: u64,
        height: u64,
        format: i64,
        framebuffer_only: u8,
        maximum_drawables: u64,
        allows_timeout: u8,
        display_sync: u8,
        presents_with_transaction: u8,
        message: *mut *const c_char,
    ) -> i32;
    #[link_name = "caml_rays_metal_drawable_texture"]
    fn raw_drawable_texture(
        drawable: *mut NativeHandle,
        output: *mut *mut NativeHandle,
        width: *mut u64,
        height: *mut u64,
        format: *mut i64,
        message: *mut *const c_char,
    ) -> i32;
}

pub fn adopt_borrowed_layer(device: &Handle, layer: NonNull<c_void>) -> Result<Handle, Error> {
    #[cfg(target_vendor = "apple")]
    {
        let mut output = null_mut();
        let mut message = null();
        let status = unsafe {
            layer_adopt_borrowed(device.as_native_ptr()?, layer.as_ptr(), &mut output, &mut message)
        };
        if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
        unsafe { Handle::from_native(output) }
    }
    #[cfg(not(target_vendor = "apple"))]
    { let _ = (device, layer); Err(Error::Unavailable) }
}

pub fn configure_layer(
    layer: &Handle,
    width: u64,
    height: u64,
    format: i64,
    framebuffer_only: bool,
    maximum_drawables: u64,
    allows_timeout: bool,
    display_sync: bool,
    presents_with_transaction: bool,
) -> Result<(), Error> {
    #[cfg(target_vendor = "apple")]
    {
        let mut message = null();
        let status = unsafe {
            layer_configure(
                layer.as_native_ptr()?, width, height, format,
                u8::from(framebuffer_only), maximum_drawables, u8::from(allows_timeout),
                u8::from(display_sync), u8::from(presents_with_transaction), &mut message,
            )
        };
        if status == 0 { Ok(()) } else { Err(unsafe { Error::from_native(status, message) }) }
    }
    #[cfg(not(target_vendor = "apple"))]
    { let _ = (layer, width, height, format, framebuffer_only, maximum_drawables, allows_timeout, display_sync, presents_with_transaction); Err(Error::Unavailable) }
}

pub fn next_drawable(layer: &Handle) -> Result<Option<Handle>, Error> {
    crate::registry::layer_next_drawable(layer)
}

pub fn drawable_texture(drawable: &Handle) -> Result<(Handle, u64, u64, i64), Error> {
    #[cfg(target_vendor = "apple")]
    {
        let mut output = null_mut();
        let mut width = 0;
        let mut height = 0;
        let mut format = 0;
        let mut message = null();
        let status = unsafe {
            raw_drawable_texture(
                drawable.as_native_ptr()?, &mut output, &mut width, &mut height, &mut format,
                &mut message,
            )
        };
        if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
        Ok((unsafe { Handle::from_native(output)? }, width, height, format))
    }
    #[cfg(not(target_vendor = "apple"))]
    { let _ = drawable; Err(Error::Unavailable) }
}
