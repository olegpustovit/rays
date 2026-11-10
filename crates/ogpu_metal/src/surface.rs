//! Ogpu_metal.Surface: a CAMetalLayer-backed surface, its acquired frames and the bounded
//! presentation slots that keep a presented source alive until its epoch completes.
use crate::native;
use ogpu_core::error::Error;
use crate::{
    device::{fail, Device},
    presentation::{self, Presentation},
    texture::Texture,
};
use ogpu_core::{
    error::ErrorKind,
    surface::AdmittedConfiguration,
};
use std::{
    cell::{Cell, RefCell},
    rc::Rc,
};

struct FrameInner {
    id: i64,
    drawable: native::Drawable,
    texture: native::Texture,
}

#[derive(Clone)]
pub struct Frame(Rc<FrameInner>);

struct SurfaceInner {
    device: Device,
    layer: native::Surface,
    presentation: Presentation,
    readable_drawables_for_test: bool,
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

pub fn create(device: &Device, layer: native::Surface) -> Result<Surface, Error> {
    let presentation = presentation::create(device)?;
    device.attach_resource();
    Ok(Surface(Rc::new(SurfaceInner {
        device: device.clone(),
        layer,
        presentation,
        readable_drawables_for_test: false,
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
        self.0.id
    }
}

impl Surface {
    pub fn destroyed(&self) -> bool {
        self.0.dead.get() || self.0.destroy_pending.get()
    }
    pub fn in_flight_presentations(&self) -> i64 {
        self.0.in_flight_presentations.get()
    }
    pub fn same(&self, other: &Surface) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }

    fn teardown(&self) {
        if !self.0.dead.get() {
            self.0.presentation.destroy();
            self.0.destroy_pending.set(false);
            self.0.dead.set(true);
            self.0.device.detach_resource();
        }
    }

    pub fn configure(&self, config: &AdmittedConfiguration) -> Result<(), Error> {
        if self.0.in_flight_presentations.get() != 0 {
            return Err(fail("Ogpu_metal.Surface.configure", ErrorKind::InvalidState, "surface has presentations in flight"));
        }
        self.0.layer.configure(config.layer_profile(!self.0.readable_drawables_for_test)?)
    }

    pub fn acquire(&self) -> Result<Acquire, Error> {
        let drawable = match self.0.layer.acquire()? {
            Err(_) => return Ok(Acquire::Timeout),
            Ok(drawable) => drawable,
        };
        let texture = match drawable.texture() {
            Ok(texture) => texture,
            Err(error) => { let _ = drawable.destroy(); return Err(error); }
        };
        Ok(Acquire::Acquired(Frame(Rc::new(FrameInner {
            id: NEXT_SERIAL.with(|next| { let id = next.get(); next.set(id + 1); id }),
            drawable, texture,
        }))))
    }

    pub fn discard(&self, frame: &Frame) -> Result<(), Error> {
        release(frame);
        Ok(())
    }

    pub fn destroy(&self) {
        if !self.destroyed() {
            if self.0.in_flight_presentations.get() == 0 {
                self.teardown();
            } else {
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

    pub fn prepare_present(&self, surface: &Surface, frame: &Frame, source: &Texture) -> Result<(), Error> {
        let op = "Ogpu_metal.Surface.prepare_present";
        if !self.available() {
            return Err(fail(op, ErrorKind::Capacity, "bounded presentation slot is already in use"));
        }
        source.retain_submission()?;
        *self.payload.borrow_mut() = Some(PendingPayload { owner: surface.clone(), frame: frame.clone(), source: source.clone() });
        self.active.set(true);
        self.epoch.set(0);
        Ok(())
    }

    /// Encodes the blit and the drawable's presentation into `commands`.
    pub fn presentation_encoder(&self, commands: &native::CommandBuffer) -> Result<(), Error> {
        match (self.active.get(), self.payload.borrow().as_ref()) {
            (true, Some(p)) => p.owner.0.presentation.encode(commands, Some(&p.frame.0.drawable), p.source.metal(), &p.frame.0.texture),
            _ => Err(fail("Ogpu_metal.Surface.encode_present", ErrorKind::InvalidState, "presentation slot is not prepared")),
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
