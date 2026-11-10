//! Ogpu_metal.Queue: one command queue, its frames-in-flight epochs and GPU timings.
use crate::native;
use crate::device::{fail, Device};
use ogpu_core::error::{Error, ErrorKind};
use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    rc::Rc,
};

/// What a submitted command buffer keeps alive until it completes; the last drop of a destroyed
/// object finishes its destroy.
#[allow(dead_code)]
pub enum Retained {
    Buffer(crate::buffer::Buffer),
    Texture(crate::texture::Texture),
    Sampler(crate::sampler::Sampler),
    Pipeline(crate::pipeline::Pipeline),
    Accel(crate::acceleration::Acceleration),
}

struct Pending {
    epoch: i64,
    command: native::CommandBuffer,
    retained: Vec<Retained>,
}

struct QueueInner {
    device: Device,
    metal: native::CommandQueue,
    max_frames: i64,
    next_epoch: Cell<i64>,
    completed: Cell<i64>,
    active: Cell<i64>,
    pending: RefCell<Vec<Pending>>,
    durations: RefCell<Vec<(i64, f64)>>,
    dead: Cell<bool>,
}

#[derive(Clone)]
pub struct Queue(Rc<QueueInner>);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Receipt {
    pub epoch: i64,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct GpuTiming {
    pub supported: bool,
    pub duration_seconds: f64,
    pub sample_count: i64,
}

struct Accumulator {
    supported: bool,
    duration_seconds: f64,
    sample_count: i64,
    queues: i64,
}

thread_local! {
    static GPU_TIMINGS: RefCell<HashMap<i64, Accumulator>> = RefCell::new(HashMap::new());
}

pub fn gpu_timing_for_device(device: &Device) -> GpuTiming {
    GPU_TIMINGS.with(|table| match table.borrow().get(&device.id()) {
        Some(v) => GpuTiming { supported: v.supported, duration_seconds: v.duration_seconds, sample_count: v.sample_count },
        None => GpuTiming { supported: false, duration_seconds: 0., sample_count: 0 },
    })
}

fn record_gpu_duration(device: &Device, duration: f64) {
    if duration.is_finite() && duration > 0. {
        GPU_TIMINGS.with(|table| {
            if let Some(v) = table.borrow_mut().get_mut(&device.id()) {
                v.supported = true;
                v.duration_seconds += duration;
                v.sample_count += 1;
            }
        });
    }
}

/// The last few completed epochs' GPU durations, for profiling callers.
const DURATION_CAPACITY: usize = 16;

pub fn create(device: &Device) -> Result<Queue, Error> {
    let metal = native::CommandQueue::create(device.metal())?;
    GPU_TIMINGS.with(|table| {
        table
            .borrow_mut()
            .entry(device.id())
            .and_modify(|v| v.queues += 1)
            .or_insert(Accumulator { supported: false, duration_seconds: 0., sample_count: 0, queues: 1 });
    });
    device.attach_resource();
    Ok(Queue(Rc::new(QueueInner {
        device: device.clone(),
        metal,
        max_frames: 3,
        next_epoch: Cell::new(1),
        completed: Cell::new(0),
        active: Cell::new(0),
        pending: RefCell::new(Vec::new()),
        durations: RefCell::new(Vec::new()),
        dead: Cell::new(false),
    })))
}

impl Queue {
    pub fn metal(&self) -> &native::CommandQueue {
        &self.0.metal
    }

    pub fn completed_epoch(&self) -> i64 {
        self.0.completed.get()
    }

    pub fn gpu_duration(&self, epoch: i64) -> Option<f64> {
        self.0.durations.borrow().iter().find(|(e, _)| *e == epoch).map(|(_, d)| *d)
    }

    fn admit(&self) -> Result<i64, Error> {
        let operation = "Ogpu_metal.Queue.submit_native";
        let value = &self.0;
        if value.device.destroyed() {
            Err(fail(operation, ErrorKind::DeviceLost, "queue device is lost"))
        } else if value.active.get() >= value.max_frames {
            Err(fail(operation, ErrorKind::Capacity, "frames-in-flight capacity reached"))
        } else {
            let epoch = value.next_epoch.get();
            value.next_epoch.set(epoch + 1);
            value.active.set(value.active.get() + 1);
            Ok(epoch)
        }
    }

    fn complete_epoch(&self, epoch: i64) -> Result<(), Error> {
        let operation = "Ogpu_metal.Queue.complete_through";
        let value = &self.0;
        if epoch < value.completed.get() || epoch >= value.next_epoch.get() {
            Err(fail(operation, ErrorKind::InvalidArgument, "completion epoch is invalid"))
        } else {
            value.active.set((value.active.get() - (epoch - value.completed.get())).max(0));
            value.completed.set(epoch);
            Ok(())
        }
    }

    /// Admits an externally encoded, uncommitted native command buffer. On any failure the caller
    /// still owns `native` and `retained`; on success `retained` is emptied into the queue.
    pub fn submit_native(&self, native: &native::CommandBuffer, retained: &mut Vec<Retained>) -> Result<Receipt, Error> {
        let op = "Ogpu_metal.Queue.submit_native";
        let value = &self.0;
        if value.dead.get() {
            return Err(fail(op, ErrorKind::StaleHandle, "queue is destroyed"));
        }
        let epoch = self.admit()?;
        match native.commit() {
            Err(e) => {
                value.active.set(value.active.get() - 1);
                value.next_epoch.set(epoch);
                Err(e)
            }
            Ok(()) => {
                value.pending.borrow_mut().push(Pending { epoch, command: native.clone(), retained: std::mem::take(retained) });
                Ok(Receipt { epoch })
            }
        }
    }

    pub fn wait_through(&self, epoch: i64) -> Result<(), Error> {
        let op = "Ogpu_metal.Queue.wait_through";
        let value = &self.0;
        if epoch <= self.completed_epoch() {
            return Err(fail(op, ErrorKind::InvalidArgument, "completion epoch is invalid"));
        }
        let (ready, later): (Vec<Pending>, Vec<Pending>) = std::mem::take(&mut *value.pending.borrow_mut()).into_iter().partition(|p| p.epoch <= epoch);
        if ready.is_empty() {
            *value.pending.borrow_mut() = later;
            return Err(fail(op, ErrorKind::InvalidArgument, "epoch was not submitted"));
        }
        let mut first_error: Option<Error> = None;
        for pending in ready {
            let outcome = match pending.command.wait_until_completed() {
                Err(e) => Err(e),
                Ok(()) => {
                    if let Ok(d) = pending.command.diagnostics() {
                        if d.gpu_end_time >= d.gpu_start_time {
                            let duration = d.gpu_end_time - d.gpu_start_time;
                            record_gpu_duration(&value.device, duration);
                            let mut durations = value.durations.borrow_mut();
                            durations.insert(0, (pending.epoch, duration));
                            durations.truncate(DURATION_CAPACITY);
                        }
                    }
                    Ok(())
                }
            };
            // `wait_until_completed` returns only after a terminal native status, including its
            // error result, so the handle and all submission-owned cleanup are safe to release.
            let _ = pending.command.destroy();
            drop(pending.retained);
            if let (None, Err(failure)) = (&first_error, outcome) {
                first_error = Some(failure);
            }
        }
        *value.pending.borrow_mut() = later;
        let portable = self.complete_epoch(epoch);
        match first_error {
            Some(failure) => Err(failure),
            None => portable,
        }
    }

    pub fn poll_through(&self, epoch: i64) -> Result<bool, Error> {
        let op = "Ogpu_metal.Queue.poll_through";
        let value = &self.0;
        if epoch <= 0 {
            return Err(fail(op, ErrorKind::InvalidArgument, "completion epoch is invalid"));
        }
        if epoch <= value.completed.get() {
            return Ok(true);
        }
        if epoch >= value.next_epoch.get() {
            return Err(fail(op, ErrorKind::InvalidArgument, "epoch was not submitted"));
        }
        for pending in value.pending.borrow().iter() {
            if pending.epoch > epoch {
                break;
            }
            match pending.command.status()? {
                native::CommandBufferStatus::Completed | native::CommandBufferStatus::Error => {}
                _ => return Ok(false),
            }
        }
        self.wait_through(epoch)?;
        Ok(true)
    }

    pub fn destroy(&self) -> Result<(), Error> {
        let op = "Ogpu_metal.Queue.destroy";
        let value = &self.0;
        if value.dead.get() {
            Ok(())
        } else if !value.pending.borrow().is_empty() {
            Err(fail(op, ErrorKind::InvalidState, "queue has commands in flight"))
        } else {
            value.metal.destroy()?;
            value.dead.set(true);
            GPU_TIMINGS.with(|table| {
                let mut table = table.borrow_mut();
                let id = value.device.id();
                if let Some(v) = table.get_mut(&id) {
                    v.queues -= 1;
                    if v.queues <= 0 {
                        table.remove(&id);
                    }
                }
            });
            value.device.detach_resource();
            Ok(())
        }
    }
}
