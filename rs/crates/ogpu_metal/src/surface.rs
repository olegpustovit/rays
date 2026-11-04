//! Ogpu_metal.Surface: a CAMetalLayer-backed surface, its acquired frames and the bounded
//! presentation slots that keep a presented source alive until its epoch completes.
use crate::{
    device::{fail, native_of, Device, R},
    presentation::{self, Presentation},
    texture::Texture,
};
use ogpu_core::{
    error::ErrorKind,
    surface::{self as portable_surface, AcquireResult, Configuration, Format, PresentMode},
    types::{TextureFormat, TextureUsage},
};
use std::{
    cell::{Cell, RefCell},
    rc::Rc,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum FrameState {
    Live,
    Presented,
    Discarded,
    Stale,
}

struct FrameInner {
    surface_serial: i64,
    portable: portable_surface::Frame,
    drawable: metal::Drawable,
    texture: metal::Texture,
    generation: i64,
    state: Cell<FrameState>,
}

#[derive(Clone)]
pub struct Frame(Rc<FrameInner>);

struct SurfaceInner {
    serial: i64,
    device: Device,
    layer: metal::Surface,
    portable: portable_surface::Surface,
    presentation: Presentation,
    readable_drawables_for_test: bool,
    frames: RefCell<Vec<Frame>>,
    in_flight_presentations: Cell<i64>,
    destroy_pending: Cell<bool>,
    dead: Cell<bool>,
}

#[derive(Clone)]
pub struct Surface(Rc<SurfaceInner>);

struct PendingPayload {
    owner: Surface,
    frame: Frame,
    source: Texture,
}

pub struct PendingPresentation {
    payload: RefCell<Option<PendingPayload>>,
    active: Cell<bool>,
    epoch: Cell<i64>,
}

pub enum Acquire {
    Acquired(Frame),
    Timeout,
    Occluded,
    DeviceLost,
}

thread_local! {
    static NEXT_SERIAL: Cell<i64> = const { Cell::new(1) };
}

fn metal_config(readable_drawables_for_test: bool, value: &Configuration) -> R<metal::surface::Configuration> {
    let format = match value.format {
        Format::Bgra8Unorm => metal::metal_format::Format::Bgra8Unorm,
        Format::Rgba8Unorm => {
            return fail("Ogpu_metal.Surface.configure", ErrorKind::InvalidArgument, "CAMetalLayer does not expose RGBA8 drawable format");
        }
    };
    Ok(metal::surface::Configuration {
        width: value.physical_width as usize,
        height: value.physical_height as usize,
        format,
        framebuffer_only: !readable_drawables_for_test,
        maximum_drawables: value.max_acquired.max(2) as usize,
        allows_timeout: true,
        display_sync: value.present_mode == PresentMode::Fifo,
        presents_with_transaction: false,
    })
}

pub fn create(device: &Device, layer: metal::Surface, config: &Configuration) -> R<Surface> {
    create_common(false, device, layer, config)
}

fn create_common(readable_drawables_for_test: bool, device: &Device, layer: metal::Surface, config: &Configuration) -> R<Surface> {
    let op = "Ogpu_metal.Surface.create";
    if device.destroyed() {
        return fail(op, ErrorKind::StaleHandle, "device is destroyed");
    }
    if !device.metal().same(layer.device()) {
        return fail(op, ErrorKind::CrossDevice, "layer belongs to another device");
    }
    let portable = portable_surface::create(device.handle(), config.clone())?;
    let native = match metal_config(readable_drawables_for_test, config) {
        Ok(n) => n,
        Err(e) => {
            portable_surface::destroy(&portable);
            return Err(e);
        }
    };
    let presentation = match presentation::create(device) {
        Ok(p) => p,
        Err(e) => {
            portable_surface::destroy(&portable);
            return Err(e);
        }
    };
    if let Err(e) = layer.configure(native) {
        presentation.destroy();
        portable_surface::destroy(&portable);
        return Err(crate::device::of_metal_error(op, &e));
    }
    device.attach_resource();
    Ok(Surface(Rc::new(SurfaceInner {
        serial: NEXT_SERIAL.with(|n| {
            n.set(n.get() + 1);
            n.get()
        }),
        device: device.clone(),
        layer,
        portable,
        presentation,
        readable_drawables_for_test,
        frames: RefCell::new(Vec::new()),
        in_flight_presentations: Cell::new(0),
        destroy_pending: Cell::new(false),
        dead: Cell::new(false),
    })))
}

fn release(frame: &Frame) {
    let _ = frame.0.texture.destroy();
    let _ = frame.0.drawable.destroy();
}

impl Frame {
    pub fn id(&self) -> i64 {
        portable_surface::frame_id(&self.0.portable)
    }
}

impl Surface {
    pub fn destroyed(&self) -> bool {
        self.0.dead.get() || self.0.destroy_pending.get()
    }
    pub fn generation(&self) -> i64 {
        portable_surface::generation(&self.0.portable)
    }
    pub fn in_flight_presentations(&self) -> i64 {
        self.0.in_flight_presentations.get()
    }
    pub fn same(&self, other: &Surface) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }

    fn stale_frames(&self) {
        for frame in self.0.frames.borrow().iter() {
            if frame.0.state.get() == FrameState::Live {
                frame.0.state.set(FrameState::Stale);
                release(frame);
            }
        }
        self.0.frames.borrow_mut().clear();
    }

    fn teardown(&self) {
        if !self.0.dead.get() {
            self.stale_frames();
            self.0.presentation.destroy();
            portable_surface::destroy(&self.0.portable);
            self.0.destroy_pending.set(false);
            self.0.dead.set(true);
            self.0.device.detach_resource();
        }
    }

    pub fn configure(&self, config: &Configuration) -> R<()> {
        let op = "Ogpu_metal.Surface.configure";
        if self.destroyed() {
            return fail(op, ErrorKind::StaleHandle, "surface is destroyed");
        }
        if self.0.in_flight_presentations.get() != 0 {
            return fail(op, ErrorKind::InvalidState, "surface has presentations in flight");
        }
        let native = metal_config(self.0.readable_drawables_for_test, config)?;
        // preflight the portable state with a probe
        let probe = portable_surface::create(self.0.device.handle(), config.clone())?;
        portable_surface::destroy(&probe);
        native_of(op, self.0.layer.configure(native))?;
        self.stale_frames();
        portable_surface::configure(&self.0.portable, config.clone())
    }

    pub fn acquire(&self) -> R<Acquire> {
        let op = "Ogpu_metal.Surface.acquire";
        if self.destroyed() {
            return fail(op, ErrorKind::StaleHandle, "surface is destroyed");
        }
        let portable = match portable_surface::acquire(&self.0.portable)? {
            AcquireResult::Timeout => return Ok(Acquire::Timeout),
            AcquireResult::Occluded => return Ok(Acquire::Occluded),
            AcquireResult::DeviceLost => return Ok(Acquire::DeviceLost),
            AcquireResult::Acquired(frame) => frame,
        };
        let drawable = match self.0.layer.acquire() {
            Err(e) => {
                let _ = portable_surface::discard(&self.0.portable, &portable);
                return Err(crate::device::of_metal_error(op, &e));
            }
            Ok(Err(_)) => {
                let _ = portable_surface::discard(&self.0.portable, &portable);
                return Ok(Acquire::Timeout);
            }
            Ok(Ok(drawable)) => drawable,
        };
        let texture = match drawable.texture() {
            Ok(t) => t,
            Err(e) => {
                let _ = drawable.destroy();
                let _ = portable_surface::discard(&self.0.portable, &portable);
                return Err(crate::device::of_metal_error(op, &e));
            }
        };
        let frame = Frame(Rc::new(FrameInner {
            surface_serial: self.0.serial,
            portable,
            drawable,
            texture,
            generation: self.generation(),
            state: Cell::new(FrameState::Live),
        }));
        self.0.frames.borrow_mut().insert(0, frame.clone());
        Ok(Acquire::Acquired(frame))
    }

    fn validate(&self, op: &str, frame: &Frame) -> R<()> {
        if self.destroyed() {
            fail(op, ErrorKind::StaleHandle, "surface is destroyed")
        } else if frame.0.surface_serial != self.0.serial {
            fail(op, ErrorKind::CrossDevice, "frame belongs to another surface")
        } else if frame.0.generation != self.generation() || frame.0.state.get() == FrameState::Stale {
            fail(op, ErrorKind::StaleHandle, "frame generation is stale")
        } else {
            match frame.0.state.get() {
                FrameState::Live => Ok(()),
                FrameState::Presented | FrameState::Discarded => fail(op, ErrorKind::InvalidState, "frame was already consumed"),
                FrameState::Stale => fail(op, ErrorKind::StaleHandle, "frame generation is stale"),
            }
        }
    }

    fn remove(&self, frame: &Frame) {
        self.0.frames.borrow_mut().retain(|candidate| !Rc::ptr_eq(&candidate.0, &frame.0));
    }

    pub fn discard(&self, frame: &Frame) -> R<()> {
        let op = "Ogpu_metal.Surface.discard";
        self.validate(op, frame)?;
        portable_surface::discard(&self.0.portable, &frame.0.portable)?;
        frame.0.state.set(FrameState::Discarded);
        self.remove(frame);
        release(frame);
        Ok(())
    }

    pub fn destroy(&self) {
        if !self.destroyed() {
            if self.0.in_flight_presentations.get() == 0 {
                self.teardown();
            } else {
                self.stale_frames();
                self.0.destroy_pending.set(true);
            }
        }
    }
}

// The bounded presentation slots.

pub fn create_pending_presentation() -> PendingPresentation {
    PendingPresentation { payload: RefCell::new(None), active: Cell::new(false), epoch: Cell::new(0) }
}

impl PendingPresentation {
    pub fn available(&self) -> bool {
        !self.active.get()
    }

    pub fn prepare_present(&self, surface: &Surface, frame: &Frame, source: &Texture) -> R<()> {
        let op = "Ogpu_metal.Surface.prepare_present";
        if !self.available() {
            return fail(op, ErrorKind::Capacity, "bounded presentation slot is already in use");
        }
        surface.validate(op, frame)?;
        let device = &surface.0.device;
        let descriptor = source.descriptor(device)?;
        let format = source.format(device)?;
        if format != TextureFormat::Rgba8Unorm {
            return fail(op, ErrorKind::InvalidArgument, "presentation source must use RGBA8 format");
        }
        if descriptor.sample_count != 1 || !descriptor.usage.contains(&TextureUsage::TextureBinding) {
            return fail(op, ErrorKind::InvalidArgument, "presentation source must be single-sample and texture-bindable");
        }
        let target = frame.0.texture.descriptor();
        if descriptor.width != target.width as i64 || descriptor.height != target.height as i64 {
            return fail(op, ErrorKind::InvalidArgument, "presentation source and drawable extents differ");
        }
        source.retain_submission()?;
        *self.payload.borrow_mut() = Some(PendingPayload { owner: surface.clone(), frame: frame.clone(), source: source.clone() });
        self.active.set(true);
        self.epoch.set(0);
        Ok(())
    }

    /// Encodes the blit and the drawable's presentation into `commands`.
    pub fn presentation_encoder(&self, commands: &metal::CommandBuffer) -> R<()> {
        match (self.active.get(), self.payload.borrow().as_ref()) {
            (true, Some(p)) => p.owner.0.presentation.encode(commands, Some(&p.frame.0.drawable), p.source.metal(), &p.frame.0.texture),
            _ => fail("Ogpu_metal.Surface.encode_present", ErrorKind::InvalidState, "presentation slot is not prepared"),
        }
    }

    fn clear(&self) {
        self.active.set(false);
        self.epoch.set(0);
    }

    pub fn rollback_present(&self) {
        if self.active.get() && self.epoch.get() == 0 {
            if let Some(p) = self.payload.borrow().as_ref() {
                p.source.release_submission();
            }
            self.clear();
        }
    }

    pub fn commit_present(&self, epoch: i64) {
        if self.active.get() && self.epoch.get() == 0 {
            if let Some(p) = self.payload.borrow().as_ref() {
                let _ = portable_surface::present(&p.owner.0.portable, &p.frame.0.portable);
                p.frame.0.state.set(FrameState::Presented);
                p.owner.remove(&p.frame);
                p.owner.0.in_flight_presentations.set(p.owner.0.in_flight_presentations.get() + 1);
                self.epoch.set(epoch);
            }
        }
    }

    fn complete_present(&self) {
        if self.active.get() && self.epoch.get() != 0 {
            let payload = self.payload.borrow();
            if let Some(p) = payload.as_ref() {
                release(&p.frame);
                p.source.release_submission();
                p.owner.0.in_flight_presentations.set(p.owner.0.in_flight_presentations.get() - 1);
                self.clear();
                if p.owner.0.in_flight_presentations.get() == 0 && p.owner.0.destroy_pending.get() {
                    p.owner.teardown();
                }
            }
        }
    }
}

pub fn complete_presentations_through(pending: &[PendingPresentation], epoch: i64) {
    for slot in pending {
        if slot.epoch.get() != 0 && slot.epoch.get() <= epoch {
            slot.complete_present();
        }
    }
}

pub fn clear_pending_presentations(pending: &[PendingPresentation]) {
    for slot in pending {
        if !slot.active.get() {
            *slot.payload.borrow_mut() = None;
        }
    }
}

impl Surface {
    /// Releases the borrowed layer's native handle once the surface is torn down.
    pub fn release_layer(&self) {
        let _ = self.0.layer.destroy();
    }
}
