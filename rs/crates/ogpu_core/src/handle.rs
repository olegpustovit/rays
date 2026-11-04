use crate::error::{self, Error, ErrorKind};
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::{Arc, Mutex};

static NEXT_DEVICE: AtomicI64 = AtomicI64::new(1);
static NEXT_HANDLE: AtomicI64 = AtomicI64::new(1);

struct DeviceState {
    id: i64,
    epoch: i64,
    dead: bool,
}

#[derive(Clone)]
pub struct Device(Arc<Mutex<DeviceState>>);

struct ResourceState {
    id: i64,
    device: Device,
    device_epoch: i64,
    generation: i64,
    dead: bool,
}

#[derive(Clone)]
pub struct ResourceHandle(Arc<Mutex<ResourceState>>);

pub fn create_device() -> Device {
    Device(Arc::new(Mutex::new(DeviceState {
        id: NEXT_DEVICE.fetch_add(1, Ordering::Relaxed),
        epoch: 1,
        dead: false,
    })))
}

pub fn device_id(device: &Device) -> i64 {
    device.0.lock().unwrap().id
}

pub fn destroy_device(device: &Device) {
    let mut value = device.0.lock().unwrap();
    if !value.dead {
        value.dead = true;
        value.epoch = value.epoch.wrapping_add(1);
    }
}

pub fn device_destroyed(device: &Device) -> bool {
    device.0.lock().unwrap().dead
}

pub fn create(device: &Device) -> ResourceHandle {
    ResourceHandle(Arc::new(Mutex::new(ResourceState {
        id: NEXT_HANDLE.fetch_add(1, Ordering::Relaxed),
        device: device.clone(),
        device_epoch: device.0.lock().unwrap().epoch,
        generation: 1,
        dead: false,
    })))
}

pub fn id(handle: &ResourceHandle) -> i64 {
    handle.0.lock().unwrap().id
}

pub fn destroy(handle: &ResourceHandle) {
    let mut value = handle.0.lock().unwrap();
    if !value.dead {
        value.dead = true;
        value.generation = value.generation.wrapping_add(1);
    }
}

pub fn destroyed(handle: &ResourceHandle) -> bool {
    handle.0.lock().unwrap().dead
}

fn validate(operation: &str, handle: &ResourceHandle) -> Result<(), Error> {
    let value = handle.0.lock().unwrap();
    if value.dead {
        Err(error::make(operation, ErrorKind::StaleHandle, "handle is destroyed"))
    } else {
        let device = value.device.0.lock().unwrap();
        if device.dead || value.device_epoch != device.epoch {
            Err(error::make(
                operation,
                ErrorKind::StaleHandle,
                "owning device generation is stale",
            ))
        } else {
            Ok(())
        }
    }
}

pub fn validate_for(operation: &str, device: &Device, handle: &ResourceHandle) -> Result<(), Error> {
    let owner_id = handle.0.lock().unwrap().device.0.lock().unwrap().id;
    if device_id(device) != owner_id {
        Err(error::make(
            operation,
            ErrorKind::CrossDevice,
            "handle belongs to another device",
        ))
    } else {
        validate(operation, handle)
    }
}
