pub use pxui_sdl::translate::{button, in_points, key, positive, scroll_phase, translate};
use sdl3::Event as SdlEvent;

use crate::runtime_input;
use input::Event;

pub fn push(value: &mut runtime_input::RuntimeInput, event: SdlEvent) -> Result<(), String> {
    let event = in_points(runtime_input::unit(value), event);
    match event {
        SdlEvent::MouseMotion { x, y, dx, dy } => {
            runtime_input::add_motion(value, dx, dy);
            runtime_input::push(value, Event::PointerMoved((x, y)))
        }
        event => match translate(event) {
            None => Ok(()),
            Some(event) => runtime_input::push(value, event),
        },
    }
}

// One bad event never drops the rest of the batch; the first error is returned.
pub fn push_all(value: &mut runtime_input::RuntimeInput, events: Vec<SdlEvent>) -> Result<(), String> {
    events.into_iter().fold(Ok(()), |first, event| match (push(value, event), first) {
        (Err(error), Ok(())) => Err(error),
        (_, first) => first,
    })
}

#[cfg(not(target_os = "linux"))]
pub fn pump(value: &mut runtime_input::RuntimeInput) -> Result<(), String> {
    match sdl3::event::poll_coalesced() {
        Err(error) => Err(error.to_string()),
        Ok(events) => push_all(value, events),
    }
}

#[cfg(not(target_os = "linux"))]
pub fn wait(timeout_ms: i32) -> Result<bool, String> {
    sdl3::event::wait(timeout_ms).map_err(|error| error.to_string())
}

/// Linux: the open Wayland windows' events (already in logical points) and relative motion.
#[cfg(target_os = "linux")]
pub fn pump(value: &mut runtime_input::RuntimeInput) -> Result<(), String> {
    let (mut events, mut motion) = (Vec::new(), (0.0, 0.0));
    crate::host::pump(&mut events, &mut motion);
    runtime_input::add_motion(value, motion.0, motion.1);
    events.into_iter().fold(Ok(()), |first, event| match (runtime_input::push(value, event), first) {
        (Err(error), Ok(())) => Err(error),
        (_, first) => first,
    })
}

#[cfg(target_os = "linux")]
pub fn wait(timeout_ms: i32) -> Result<bool, String> {
    Ok(crate::host::wait(timeout_ms))
}
