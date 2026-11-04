use std::ffi::{CStr, CString, c_char, c_int, c_void};

mod ffi {
    use super::{c_char, c_int, c_void};
    unsafe extern "C" {
        pub fn rays_ttf_get_error() -> *const c_char;
        pub fn rays_ttf_free(pointer: *mut c_void);
        pub fn rays_ttf_version() -> c_int;
        pub fn rays_ttf_compiled_version() -> c_int;
        pub fn rays_ttf_init() -> bool;
        pub fn rays_ttf_quit();
        pub fn rays_ttf_was_init() -> c_int;
        pub fn rays_ttf_open_font(path: *const c_char, size: f64) -> *mut c_void;
        pub fn rays_ttf_close_font(font: *mut c_void);
        pub fn rays_ttf_font_metrics(font: *mut c_void, out: *mut c_int);
        pub fn rays_ttf_set_font_size_dpi(font: *mut c_void, size: f64, horizontal: c_int, vertical: c_int) -> bool;
        pub fn rays_ttf_set_font_hinting(font: *mut c_void, hinting: c_int);
        pub fn rays_ttf_set_font_wrap_alignment(font: *mut c_void, code: c_int);
        pub fn rays_ttf_glyph_metrics(font: *mut c_void, codepoint: u32, out: *mut c_int) -> bool;
        pub fn rays_ttf_render_blended(
            font: *mut c_void, text: *const c_char, length: usize,
            red: c_int, green: c_int, blue: c_int, alpha: c_int, wrap_width: c_int,
            width: *mut c_int, height: *mut c_int, pixels: *mut *mut u8,
        ) -> bool;
    }
}

fn last_error() -> String {
    // SAFETY: SDL_GetError returns a borrowed, NUL-terminated thread-local string.
    let pointer = unsafe { ffi::rays_ttf_get_error() };
    if pointer.is_null() {
        return "SDL3_ttf call failed".to_owned();
    }
    // SAFETY: non-null SDL error text is NUL-terminated and valid until the next SDL call.
    let message = unsafe { CStr::from_ptr(pointer) }.to_string_lossy().into_owned();
    if message.is_empty() { "SDL3_ttf call failed".to_owned() } else { message }
}

/// A font handle: the address SDL_ttf returned. The caller closes it exactly once.
pub type Font = usize;

fn raw(font: Font) -> *mut c_void {
    font as *mut c_void
}

pub fn linked_version() -> i32 {
    // SAFETY: pure version query.
    unsafe { ffi::rays_ttf_version() }
}

pub fn compiled_version() -> i32 {
    // SAFETY: returns the header version macro.
    unsafe { ffi::rays_ttf_compiled_version() }
}

pub fn init() -> Result<(), String> {
    // SAFETY: TTF_Init has no preconditions.
    if unsafe { ffi::rays_ttf_init() } { Ok(()) } else { Err(last_error()) }
}

pub fn quit() {
    // SAFETY: TTF_Quit has no preconditions.
    unsafe { ffi::rays_ttf_quit() }
}

pub fn was_init() -> i32 {
    // SAFETY: pure query.
    unsafe { ffi::rays_ttf_was_init() }
}

pub fn open_font(path: &str, size: f64) -> Result<Font, String> {
    let path = CString::new(path).map_err(|_| "font path contains a NUL byte".to_owned())?;
    // SAFETY: `path` is NUL-terminated and outlives the call.
    let font = unsafe { ffi::rays_ttf_open_font(path.as_ptr(), size) };
    if font.is_null() { Err(last_error()) } else { Ok(font as usize) }
}

pub fn close_font(font: Font) {
    // SAFETY: the caller closes a font returned by open_font exactly once.
    unsafe { ffi::rays_ttf_close_font(raw(font)) }
}

pub fn font_metrics(font: Font) -> (i32, i32, i32, i32) {
    let mut out = [0 as c_int; 4];
    // SAFETY: `font` is live per the caller; `out` has the four slots the bridge writes.
    unsafe { ffi::rays_ttf_font_metrics(raw(font), out.as_mut_ptr()) };
    (out[0], out[1], out[2], out[3])
}

pub fn set_font_size_dpi(font: Font, size: f64, horizontal: i32, vertical: i32) -> Result<(), String> {
    // SAFETY: `font` is live per the caller.
    if unsafe { ffi::rays_ttf_set_font_size_dpi(raw(font), size, horizontal, vertical) } {
        Ok(())
    } else {
        Err(last_error())
    }
}

pub fn set_font_hinting(font: Font, hinting: i32) {
    // SAFETY: `font` is live per the caller.
    unsafe { ffi::rays_ttf_set_font_hinting(raw(font), hinting) }
}

/// `code` is 0 left, 1 center, 2 right.
pub fn set_font_wrap_alignment(font: Font, code: i32) {
    assert!((0..=2).contains(&code), "alignment code out of range");
    // SAFETY: `font` is live per the caller and `code` was range-checked.
    unsafe { ffi::rays_ttf_set_font_wrap_alignment(raw(font), code) }
}

pub fn glyph_metrics(font: Font, codepoint: u32) -> Result<(i32, i32, i32, i32, i32), String> {
    let mut out = [0 as c_int; 5];
    // SAFETY: `font` is live per the caller; `out` has the five slots the bridge writes.
    if unsafe { ffi::rays_ttf_glyph_metrics(raw(font), codepoint, out.as_mut_ptr()) } {
        Ok((out[0], out[1], out[2], out[3], out[4]))
    } else {
        Err(last_error())
    }
}

/// Rasterizes UTF-8 `text` to RGBA8; `wrap_width < 0` means unwrapped.
pub fn render_blended(
    font: Font,
    text: &str,
    color: (i32, i32, i32, i32),
    wrap_width: i32,
) -> Result<(i32, i32, Vec<u8>), String> {
    let (mut width, mut height, mut pixels): (c_int, c_int, *mut u8) = (0, 0, std::ptr::null_mut());
    // SAFETY: `font` is live per the caller; the text pointer and length describe `text`.
    let ok = unsafe {
        ffi::rays_ttf_render_blended(
            raw(font),
            text.as_ptr().cast(),
            text.len(),
            color.0,
            color.1,
            color.2,
            color.3,
            wrap_width,
            &mut width,
            &mut height,
            &mut pixels,
        )
    };
    if !ok {
        return Err(last_error());
    }
    let length = width as usize * height as usize * 4;
    // SAFETY: success guarantees `pixels` points at `length` initialized bytes.
    let copy = unsafe { std::slice::from_raw_parts(pixels, length) }.to_vec();
    // SAFETY: the buffer was allocated with SDL_malloc by the bridge.
    unsafe { ffi::rays_ttf_free(pixels.cast()) };
    Ok((width, height, copy))
}
