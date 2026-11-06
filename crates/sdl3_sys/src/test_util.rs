use std::ffi::{CString, c_char};

pub fn push_event_trace() -> bool {
    // SAFETY: the helper only pushes SDL-owned test events into the active queue.
    unsafe { super::ffi::rays_sdl3_test_push_event_trace() }
}

pub fn mutate_event_sources() {
    // SAFETY: this mutates static test fixture buffers after their events are copied.
    unsafe { super::ffi::rays_sdl3_test_mutate_event_sources() }
}

pub fn polling_event_queue() -> bool {
    // SAFETY: reports whether the current native event pump is active.
    unsafe { super::ffi::rays_sdl3_test_polling_event_queue() }
}

pub fn queue_resize_during_poll() {
    // SAFETY: arms one synthetic pixel-size event for the next native event pump.
    unsafe { super::ffi::rays_sdl3_test_queue_resize_during_poll() }
}

pub fn push_resize_burst() -> bool {
    // SAFETY: this pushes a fixed sequence of synthetic SDL resize events.
    unsafe { super::ffi::rays_sdl3_test_push_resize_burst() }
}

pub fn dialog_reserve() -> i32 {
    // SAFETY: the helper reserves one internal dialog test slot.
    unsafe { super::ffi::rays_sdl3_test_dialog_reserve() }
}

pub fn dialog_abandon(id: i32) {
    // SAFETY: this releases the matching test slot if it is still open.
    unsafe { super::ffi::rays_sdl3_test_dialog_abandon(id) }
}

pub fn dialog_finish(id: i32, paths: Option<&[&str]>) -> bool {
    let strings: Option<Vec<_>> = paths.map(|paths| paths.iter().map(|path| CString::new(*path).ok()).collect());
    let Some(strings) = strings else { return false; };
    if strings.iter().any(Option::is_none) { return false; }
    let strings: Vec<_> = strings.into_iter().map(Option::unwrap).collect();
    let pointers: Vec<*const c_char> = strings.iter().map(|path| path.as_ptr()).collect();
    // SAFETY: the pointers remain valid until the callback thread has finished copying them.
    unsafe {
        super::ffi::rays_sdl3_test_dialog_finish(id, pointers.as_ptr(), pointers.len() as i32, false)
    }
}

pub fn dialog_fail(id: i32) -> bool {
    // SAFETY: the helper completes the matching slot with a copied failure message.
    unsafe { super::ffi::rays_sdl3_test_dialog_finish(id, std::ptr::null(), 0, true) }
}

pub fn dialog_finish_huge(id: i32) -> bool {
    // SAFETY: the helper passes a bounded 5000-entry list to exercise native selection limits.
    unsafe { super::ffi::rays_sdl3_test_dialog_finish_huge(id) }
}

pub fn post_scroll(points: i32, phase: i32) {
    // SAFETY: the helper posts one native trackpad event; it is a no-op off macOS.
    unsafe { super::ffi::rays_sdl3_test_post_scroll(points, phase) }
}
