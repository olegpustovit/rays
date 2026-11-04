use std::ffi::{CStr, CString, c_char, c_int, c_void};

mod ffi {
    use super::{c_char, c_int, c_void};
    unsafe extern "C" {
        pub fn rays_img_get_error() -> *const c_char;
        pub fn rays_img_free(pointer: *mut c_void);
        pub fn rays_image_version() -> c_int;
        pub fn rays_image_compiled_version() -> c_int;
        pub fn rays_image_decode(
            data: *const c_void, length: usize, kind: *const c_char,
            width: *mut c_int, height: *mut c_int, pixels: *mut *mut u8,
        ) -> bool;
    }
}

fn last_error(fallback: &str) -> String {
    // SAFETY: SDL_GetError returns a borrowed, NUL-terminated thread-local string.
    let pointer = unsafe { ffi::rays_img_get_error() };
    if pointer.is_null() {
        return fallback.to_owned();
    }
    // SAFETY: non-null SDL error text is NUL-terminated and valid until the next SDL call.
    let message = unsafe { CStr::from_ptr(pointer) }.to_string_lossy().into_owned();
    if message.is_empty() { fallback.to_owned() } else { message }
}

pub fn linked_version() -> i32 {
    // SAFETY: pure version query.
    unsafe { ffi::rays_image_version() }
}

pub fn compiled_version() -> i32 {
    // SAFETY: returns the header version macro.
    unsafe { ffi::rays_image_compiled_version() }
}

/// Decodes `bytes` to tightly packed RGBA8 `(width, height, pixels)`; `kind` forces a decoder.
pub fn decode(bytes: &[u8], kind: Option<&str>) -> Result<(i32, i32, Vec<u8>), String> {
    let kind = match kind {
        Some(kind) => Some(CString::new(kind).map_err(|_| "image kind contains a NUL byte".to_owned())?),
        None => None,
    };
    let (mut width, mut height, mut pixels): (c_int, c_int, *mut u8) = (0, 0, std::ptr::null_mut());
    // SAFETY: `bytes` and `kind` outlive the call; the bridge copies the decoded surface into an
    // SDL_malloc allocation of exactly width * height * 4 bytes, which is copied out and freed below.
    let ok = unsafe {
        ffi::rays_image_decode(
            bytes.as_ptr().cast(),
            bytes.len(),
            kind.as_ref().map_or(std::ptr::null(), |kind| kind.as_ptr()),
            &mut width,
            &mut height,
            &mut pixels,
        )
    };
    if !ok {
        return Err(last_error("image decode failed"));
    }
    let length = width as usize * height as usize * 4;
    // SAFETY: success guarantees `pixels` points at `length` initialized bytes.
    let copy = unsafe { std::slice::from_raw_parts(pixels, length) }.to_vec();
    // SAFETY: the buffer was allocated with SDL_malloc by the bridge.
    unsafe { ffi::rays_img_free(pixels.cast()) };
    Ok((width, height, copy))
}
