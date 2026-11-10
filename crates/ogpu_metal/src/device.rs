//! Ogpu_metal.Device: one Metal device with its OGPU capability profile and live-resource count.
use crate::native;
use ogpu_core::{
    caps::Caps,
    error::{self, Error, ErrorKind},
    handle,
};
use std::{cell::Cell, rc::Rc};

pub fn fail(operation: &str, kind: ErrorKind, message: &str) -> Error {
    error::make(operation, kind, message)
}

struct DeviceInner {
    metal: native::Device,
    handle: handle::Device,
    profile: Caps,
    generation: Cell<i64>,
    live_resources: Cell<i64>,
}

#[derive(Clone)]
pub struct Device(Rc<DeviceInner>);

impl Device {
    pub fn system_default() -> Result<Device, Error> {
        let metal = native::Device::system_default()?;
        let capabilities = metal.capabilities()?;
        Ok(Device(Rc::new(DeviceInner {
            metal,
            handle: handle::create_device(),
            profile: capabilities,
            generation: Cell::new(1),
            live_resources: Cell::new(0),
        })))
    }

    pub fn id(&self) -> i64 {
        handle::device_id(&self.0.handle)
    }
    pub fn capabilities(&self) -> Caps {
        self.0.profile
    }
    pub fn destroyed(&self) -> bool {
        handle::device_destroyed(&self.0.handle)
    }
    pub fn generation(&self) -> i64 {
        self.0.generation.get()
    }

    pub fn destroy(&self) -> Result<(), Error> {
        let operation = "Ogpu_metal.Device.destroy";
        if self.destroyed() {
            Ok(())
        } else if self.0.live_resources.get() != 0 {
            Err(fail(operation, ErrorKind::InvalidState, "device still owns live resources"))
        } else {
            self.0.metal.destroy()?;
            handle::destroy_device(&self.0.handle);
            self.0.generation.set(self.0.generation.get().wrapping_add(1));
            Ok(())
        }
    }

    pub fn metal(&self) -> &native::Device {
        &self.0.metal
    }
    pub fn handle(&self) -> &handle::Device {
        &self.0.handle
    }
    pub fn attach_resource(&self) {
        self.0.live_resources.set(self.0.live_resources.get() + 1);
    }
    pub fn detach_resource(&self) {
        self.0.live_resources.set(self.0.live_resources.get() - 1);
    }
    pub fn same(&self, other: &Device) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}
