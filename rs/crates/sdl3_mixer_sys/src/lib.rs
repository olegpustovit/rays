use std::ffi::{CStr, c_char, c_int, c_void};

mod ffi {
    use super::{c_char, c_int, c_void};
    unsafe extern "C" {
        pub fn rays_mix_get_error() -> *const c_char;
        pub fn rays_mixer_version() -> c_int;
        pub fn rays_mixer_compiled_version() -> c_int;
        pub fn rays_mixer_init() -> bool;
        pub fn rays_mixer_quit();
        pub fn rays_mixer_create_device() -> *mut c_void;
        pub fn rays_mixer_create_memory(sample_rate: c_int, channels: c_int) -> *mut c_void;
        pub fn rays_mixer_destroy_mixer(mixer: *mut c_void);
        pub fn rays_mixer_format(mixer: *mut c_void, out: *mut c_int) -> bool;
        pub fn rays_mixer_generate(mixer: *mut c_void, buffer: *mut u8, length: usize) -> c_int;
        pub fn rays_mixer_load_bytes(mixer: *mut c_void, data: *const c_void, length: usize) -> *mut c_void;
        pub fn rays_mixer_destroy_audio(audio: *mut c_void);
        pub fn rays_mixer_create_track(mixer: *mut c_void) -> *mut c_void;
        pub fn rays_mixer_destroy_track(track: *mut c_void);
        pub fn rays_mixer_set_track_audio(track: *mut c_void, audio: *mut c_void) -> bool;
        pub fn rays_mixer_set_track_gain(track: *mut c_void, gain: f64) -> bool;
        pub fn rays_mixer_play_track(track: *mut c_void, loops: c_int, fade_in_ms: c_int) -> bool;
        pub fn rays_mixer_stop_track(track: *mut c_void, fade_out_ms: c_int) -> bool;
        pub fn rays_mixer_track_playing(track: *mut c_void) -> bool;
    }
}

fn last_error() -> String {
    // SAFETY: SDL_GetError returns a borrowed, NUL-terminated thread-local string.
    let pointer = unsafe { ffi::rays_mix_get_error() };
    if pointer.is_null() {
        return "SDL3_mixer call failed".to_owned();
    }
    // SAFETY: non-null SDL error text is NUL-terminated and valid until the next SDL call.
    let message = unsafe { CStr::from_ptr(pointer) }.to_string_lossy().into_owned();
    if message.is_empty() { "SDL3_mixer call failed".to_owned() } else { message }
}

/// Mixer, audio and track handles: the addresses SDL_mixer returned. The caller owns and
/// destroys each exactly once, tracks and audio before their mixer.
pub type Handle = usize;

fn raw(handle: Handle) -> *mut c_void {
    handle as *mut c_void
}

fn pointer(result: *mut c_void) -> Result<Handle, String> {
    if result.is_null() { Err(last_error()) } else { Ok(result as usize) }
}

fn check(ok: bool) -> Result<(), String> {
    if ok { Ok(()) } else { Err(last_error()) }
}

pub fn linked_version() -> i32 {
    // SAFETY: pure version query.
    unsafe { ffi::rays_mixer_version() }
}

pub fn compiled_version() -> i32 {
    // SAFETY: returns the header version macro.
    unsafe { ffi::rays_mixer_compiled_version() }
}

pub fn init() -> Result<(), String> {
    // SAFETY: MIX_Init has no preconditions.
    check(unsafe { ffi::rays_mixer_init() })
}

pub fn quit() {
    // SAFETY: MIX_Quit has no preconditions.
    unsafe { ffi::rays_mixer_quit() }
}

pub fn create_device() -> Result<Handle, String> {
    // SAFETY: opens the default playback device; a null result is reported as an error.
    pointer(unsafe { ffi::rays_mixer_create_device() })
}

pub fn create_memory(sample_rate: i32, channels: i32) -> Result<Handle, String> {
    // SAFETY: plain value arguments.
    pointer(unsafe { ffi::rays_mixer_create_memory(sample_rate, channels) })
}

pub fn destroy_mixer(mixer: Handle) {
    // SAFETY: the caller destroys a live mixer exactly once.
    unsafe { ffi::rays_mixer_destroy_mixer(raw(mixer)) }
}

pub fn format(mixer: Handle) -> Result<(i32, i32), String> {
    let mut out = [0 as c_int; 2];
    // SAFETY: `mixer` is live per the caller; `out` has the two slots the bridge writes.
    check(unsafe { ffi::rays_mixer_format(raw(mixer), out.as_mut_ptr()) })?;
    Ok((out[0], out[1]))
}

/// Fills `buffer` with PCM and returns the number of mixed bytes.
pub fn generate(mixer: Handle, buffer: &mut [u8]) -> Result<i32, String> {
    // SAFETY: `mixer` is live per the caller; the pointer and length describe `buffer`.
    let mixed = unsafe { ffi::rays_mixer_generate(raw(mixer), buffer.as_mut_ptr(), buffer.len()) };
    if mixed < 0 { Err(last_error()) } else { Ok(mixed) }
}

pub fn load_bytes(mixer: Handle, bytes: &[u8]) -> Result<Handle, String> {
    // SAFETY: `mixer` is live per the caller; SDL reads `bytes` only during the call, which copies it.
    pointer(unsafe { ffi::rays_mixer_load_bytes(raw(mixer), bytes.as_ptr().cast(), bytes.len()) })
}

pub fn destroy_audio(audio: Handle) {
    // SAFETY: the caller destroys a live audio exactly once.
    unsafe { ffi::rays_mixer_destroy_audio(raw(audio)) }
}

pub fn create_track(mixer: Handle) -> Result<Handle, String> {
    // SAFETY: `mixer` is live per the caller.
    pointer(unsafe { ffi::rays_mixer_create_track(raw(mixer)) })
}

pub fn destroy_track(track: Handle) {
    // SAFETY: the caller destroys a live track exactly once.
    unsafe { ffi::rays_mixer_destroy_track(raw(track)) }
}

pub fn set_track_audio(track: Handle, audio: Handle) -> Result<(), String> {
    // SAFETY: both handles are live per the caller.
    check(unsafe { ffi::rays_mixer_set_track_audio(raw(track), raw(audio)) })
}

pub fn set_track_gain(track: Handle, gain: f64) -> Result<(), String> {
    // SAFETY: `track` is live per the caller.
    check(unsafe { ffi::rays_mixer_set_track_gain(raw(track), gain) })
}

pub fn play_track(track: Handle, loops: i32, fade_in_ms: i32) -> Result<(), String> {
    // SAFETY: `track` is live per the caller.
    check(unsafe { ffi::rays_mixer_play_track(raw(track), loops, fade_in_ms) })
}

pub fn stop_track(track: Handle, fade_out_ms: i32) -> Result<(), String> {
    // SAFETY: `track` is live per the caller.
    check(unsafe { ffi::rays_mixer_stop_track(raw(track), fade_out_ms) })
}

pub fn track_playing(track: Handle) -> bool {
    // SAFETY: `track` is live per the caller.
    unsafe { ffi::rays_mixer_track_playing(raw(track)) }
}
