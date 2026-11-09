use crate::error::{self, Error, ErrorKind};
use crate::handle::{self, Device};
use native_layer_token_sys::portable::Token;
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::{Arc, Mutex};

static NEXT_SURFACE: AtomicI64 = AtomicI64::new(1);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Format {
    Rgba8Unorm,
    Bgra8Unorm,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PresentMode {
    Fifo,
    Immediate,
}

#[derive(Clone)]
pub struct Configuration {
    pub logical_width: i64,
    pub logical_height: i64,
    pub physical_width: i64,
    pub physical_height: i64,
    pub format: Format,
    pub present_mode: PresentMode,
    pub max_acquired: i64,
    pub layer: Option<Token>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Availability {
    Available,
    ForceTimeout,
    ForceOccluded,
    ForceDeviceLost,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum FrameState {
    Live,
    Presented,
    Discarded,
    Stale,
}

struct FrameData {
    id: i64,
    surface_id: i64,
    generation: i64,
    state: FrameState,
}

#[derive(Clone)]
pub struct Frame(Arc<Mutex<FrameData>>);

struct SurfaceData {
    id: i64,
    device: Device,
    config: Configuration,
    generation: i64,
    next_frame: i64,
    frames: Vec<Frame>,
    availability: Availability,
    dead: bool,
}

pub struct Surface(Arc<Mutex<SurfaceData>>);

pub enum AcquireResult {
    Acquired(Frame),
    Timeout,
    Occluded,
    DeviceLost,
}

fn invalid(operation: &str, message: &str) -> Error {
    error::make(operation, ErrorKind::InvalidArgument, message)
}

fn state_error(operation: &str, message: &str) -> Error {
    error::make(operation, ErrorKind::InvalidState, message)
}

pub fn validate_configuration(operation: &str, configuration: &Configuration) -> Result<(), Error> {
    if configuration.logical_width <= 0 || configuration.logical_height <= 0
        || configuration.physical_width <= 0 || configuration.physical_height <= 0
        || !(1..=3).contains(&configuration.max_acquired) {
        Err(invalid(operation, "surface dimensions/capacity are invalid"))
    } else { Ok(()) }
}

pub fn create(device: &Device, configuration: Configuration) -> Result<Surface, Error> {
    validate_configuration("Ogpu.Surface.create", &configuration)?;
    Ok(Surface(Arc::new(Mutex::new(SurfaceData {
        id: NEXT_SURFACE.fetch_add(1, Ordering::Relaxed),
        device: device.clone(),
        config: configuration,
        generation: 1,
        next_frame: 1,
        frames: Vec::new(),
        availability: Availability::Available,
        dead: false,
    }))))
}

fn invalidate(surface: &mut SurfaceData) {
    for frame in &surface.frames {
        let mut frame = frame.0.lock().unwrap();
        if frame.state == FrameState::Live {
            frame.state = FrameState::Stale;
        }
    }
    surface.frames.clear();
    surface.generation = surface.generation.wrapping_add(1);
}

pub fn configure(surface: &Surface, configuration: Configuration) -> Result<(), Error> {
    let mut surface = surface.0.lock().unwrap();
    if surface.dead {
        return Err(state_error("Ogpu.Surface.configure", "surface is destroyed"));
    }
    validate_configuration("Ogpu.Surface.configure", &configuration)?;
    invalidate(&mut surface);
    surface.config = configuration;
    Ok(())
}

pub fn acquire(surface: &Surface) -> Result<AcquireResult, Error> {
    let mut surface = surface.0.lock().unwrap();
    if surface.dead {
        return Err(state_error("Ogpu.Surface.acquire", "surface is destroyed"));
    }
    if handle::device_destroyed(&surface.device) {
        return Ok(AcquireResult::DeviceLost);
    }
    match surface.availability {
        Availability::ForceTimeout => return Ok(AcquireResult::Timeout),
        Availability::ForceOccluded => return Ok(AcquireResult::Occluded),
        Availability::ForceDeviceLost => return Ok(AcquireResult::DeviceLost),
        Availability::Available => {}
    }
    if surface.frames.len() as i64 >= surface.config.max_acquired {
        return Err(error::make(
            "Ogpu.Surface.acquire",
            ErrorKind::Capacity,
            "acquired-frame capacity reached",
        ));
    }
    let frame = Frame(Arc::new(Mutex::new(FrameData {
        id: surface.next_frame,
        surface_id: surface.id,
        generation: surface.generation,
        state: FrameState::Live,
    })));
    surface.next_frame = surface.next_frame.wrapping_add(1);
    surface.frames.push(frame.clone());
    Ok(AcquireResult::Acquired(frame))
}

fn finish(operation: &str, final_state: FrameState, surface: &Surface, frame: &Frame) -> Result<(), Error> {
    let mut surface = surface.0.lock().unwrap();
    if surface.dead {
        return Err(state_error(operation, "surface is destroyed"));
    }
    let mut frame_data = frame.0.lock().unwrap();
    if frame_data.surface_id != surface.id {
        return Err(error::make(
            operation,
            ErrorKind::CrossDevice,
            "frame belongs to another surface",
        ));
    }
    if frame_data.generation != surface.generation || frame_data.state == FrameState::Stale {
        return Err(error::make(
            operation,
            ErrorKind::StaleHandle,
            "frame generation is stale",
        ));
    }
    if frame_data.state != FrameState::Live {
        return Err(state_error(operation, "frame has already been consumed"));
    }
    frame_data.state = final_state;
    let id = frame_data.id;
    drop(frame_data);
    surface.frames.retain(|candidate| candidate.0.lock().unwrap().id != id);
    Ok(())
}

pub fn present(surface: &Surface, frame: &Frame) -> Result<(), Error> {
    finish("Ogpu.Surface.present", FrameState::Presented, surface, frame)
}

pub fn discard(surface: &Surface, frame: &Frame) -> Result<(), Error> {
    finish("Ogpu.Surface.discard", FrameState::Discarded, surface, frame)
}

pub fn generation(surface: &Surface) -> i64 {
    surface.0.lock().unwrap().generation
}

pub fn frame_id(frame: &Frame) -> i64 {
    frame.0.lock().unwrap().id
}

pub fn destroy(surface: &Surface) {
    let mut surface = surface.0.lock().unwrap();
    if !surface.dead {
        surface.dead = true;
        invalidate(&mut surface);
    }
}
