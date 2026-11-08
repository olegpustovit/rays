//! Ogpu_metal.Command_control: what the encoders share while one command buffer records, and the
//! token tables of the device behind them.
use ogpu_core::error::Error;
use crate::{
    acceleration::Acceleration,
    buffer::Buffer,
    device::{fail, of_metal_error, Device},
    pipeline::Pipeline,
    queue::Cleanup,
    sampler::Sampler,
    texture::Texture,
};
use metal::{CompareFunction, DepthStencil, DepthStencilDescriptor};
use ogpu_core::{
    backend::DepthState,
    error::ErrorKind,
    render_pass::Comparison,
};
use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    rc::Rc,
};

#[derive(Clone)]
pub enum Resource {
    Buffer(Buffer),
    Texture(Texture),
}

pub struct IcbEntry {
    pub icb: metal::IndirectCommandBuffer,
    pub commands: RefCell<Vec<Option<metal::RenderCommand>>>,
}

/// The token tables of one device.
pub struct Tables {
    pub resources: RefCell<HashMap<i64, Resource>>,
    pub pipelines: RefCell<HashMap<i64, Pipeline>>,
    pub samplers: RefCell<HashMap<i64, Sampler>>,
    pub accels: RefCell<HashMap<i64, Acceleration>>,
    pub icbs: RefCell<HashMap<i64, Rc<IcbEntry>>>,
    /// Every depth-stencil state, made with the device (`create_depth_states`): eight comparisons
    /// by write or not. Made lazily they would outlive the first owner that drew with them.
    pub depth_states: RefCell<Vec<(DepthState, DepthStencil)>>,
    pub next: Cell<i64>,
}

impl Tables {
    pub fn new() -> Tables {
        Tables {
            resources: RefCell::new(HashMap::new()),
            pipelines: RefCell::new(HashMap::new()),
            samplers: RefCell::new(HashMap::new()),
            accels: RefCell::new(HashMap::new()),
            icbs: RefCell::new(HashMap::new()),
            depth_states: RefCell::new(Vec::new()),
            next: Cell::new(1),
        }
    }

    pub fn token(&self) -> i64 {
        let x = self.next.get();
        self.next.set(x + 1);
        x
    }

    pub fn resource(&self, token: i64) -> Option<Resource> {
        self.resources.borrow().get(&token).cloned()
    }

    /// Fill `depth_states` once, when the device is made.
    pub fn create_depth_states(&self, device: &Device) -> Result<(), Error> {
        use Comparison::*;
        let mut states = self.depth_states.borrow_mut();
        for depth_compare in [Never, Less, Equal, LessEqual, Greater, NotEqual, GreaterEqual, Always] {
            for depth_write in [false, true] {
                let descriptor = DepthStencilDescriptor {
                    depth_compare: metal_compare(depth_compare),
                    depth_write,
                    label: Some("ogpu-metal-depth-state".to_owned()),
                    ..DepthStencilDescriptor::default()
                };
                let native = DepthStencil::create(device.metal(), descriptor).map_err(|e| of_metal_error("Ogpu_metal.Backend.create_device", &e))?;
                states.push((DepthState { depth_compare, depth_write }, native));
            }
        }
        Ok(())
    }

    pub fn depth_state_for(&self, state: DepthState) -> Result<DepthStencil, Error> {
        match self.depth_states.borrow().iter().find(|(s, _)| *s == state) {
            Some((_, native)) => Ok(native.clone()),
            None => Err(fail("Ogpu_metal.Backend.depth_state", ErrorKind::InvalidState, "device has no depth states")),
        }
    }
}

impl Default for Tables {
    fn default() -> Self {
        Tables::new()
    }
}

fn metal_compare(value: Comparison) -> CompareFunction {
    match value {
        Comparison::Never => CompareFunction::Never,
        Comparison::Less => CompareFunction::Less,
        Comparison::Equal => CompareFunction::Equal,
        Comparison::LessEqual => CompareFunction::LessEqual,
        Comparison::Greater => CompareFunction::Greater,
        Comparison::NotEqual => CompareFunction::NotEqual,
        Comparison::GreaterEqual => CompareFunction::GreaterEqual,
        Comparison::Always => CompareFunction::Always,
    }
}

/// The encoder a recording command buffer has open; `abandon` ends it.
pub enum OpenEncoder {
    Compute(metal::ComputeEncoder),
    Accel(metal::AccelerationEncoder),
    Blit(metal::BlitEncoder),
    Render(metal::RenderEncoder, metal::RenderPassDescriptor),
}

impl OpenEncoder {
    pub fn end(&self) {
        match self {
            OpenEncoder::Compute(e) => drop(e.end_encoding()),
            OpenEncoder::Accel(e) => drop(e.end_encoding()),
            OpenEncoder::Blit(e) => drop(e.end_encoding()),
            OpenEncoder::Render(e, pass) => {
                let _ = e.end_encoding();
                let _ = pass.destroy();
            }
        }
    }
}

/// One recording command buffer: its native handle, the objects its commands retain and the open
/// encoder, if any.
pub struct CommandContext {
    pub operation: &'static str,
    pub device: Device,
    pub native: metal::CommandBuffer,
    pub tables: Rc<Tables>,
    pub commands_token: i64,
    pub open_encoder: RefCell<Option<OpenEncoder>>,
    pub finished: Cell<bool>,
    pub retained: RefCell<Vec<Cleanup>>,
}

impl CommandContext {
        pub fn recording(&self) -> Result<(), Error> {
        if self.finished.get() {
            Err(fail(self.operation, ErrorKind::InvalidState, "commands are finished"))
        } else {
            Ok(())
        }
    }

    /// Records `release` when `retained` succeeded.
    pub fn keep(&self, retained: Result<(), Error>, release: Cleanup) -> Result<(), Error> {
        retained?;
        self.retained.borrow_mut().push(release);
        Ok(())
    }

    /// One retain per buffer per command buffer, however many bindings reference it.
    pub fn keep_buffer(&self, buffer: &Buffer) -> Result<(), Error> {
        if buffer.retain_for(self.commands_token)? {
            let b = buffer.clone();
            self.retained.borrow_mut().push(Box::new(move || b.release_submission()));
        }
        Ok(())
    }

    pub fn find_buffer(&self, token: i64) -> Result<Buffer, Error> {
        match self.tables.resource(token) {
            Some(Resource::Buffer(buffer)) => Ok(buffer),
            _ => Err(fail(self.operation, ErrorKind::InvalidArgument, "buffer graph is incomplete")),
        }
    }

    pub fn find_accel(&self, token: i64) -> Result<Acceleration, Error> {
        match self.tables.accels.borrow().get(&token) {
            Some(structure) => Ok(structure.clone()),
            None => Err(fail(self.operation, ErrorKind::InvalidArgument, "acceleration structure graph is incomplete")),
        }
    }

    pub fn keep_accel(&self, structure: &Acceleration) -> Result<(), Error> {
        let release = structure.retain_submission()?;
        self.retained.borrow_mut().push(release);
        Ok(())
    }

    pub fn find_texture(&self, token: i64) -> Result<Texture, Error> {
        match self.tables.resource(token) {
            Some(Resource::Texture(texture)) => Ok(texture),
            _ => Err(fail(self.operation, ErrorKind::InvalidArgument, "texture graph is incomplete")),
        }
    }

    /// One retain per texture per command buffer, however many bindings reference it.
    pub fn keep_texture(&self, texture: &Texture) -> Result<(), Error> {
        if texture.retain_for(self.commands_token)? {
            let t = texture.clone();
            self.retained.borrow_mut().push(Box::new(move || t.release_submission()));
        }
        Ok(())
    }

    /// One retain per sampler per command buffer, however many bindings reference it.
    pub fn keep_sampler(&self, sampler: &Sampler) -> Result<(), Error> {
        if sampler.retain_for(self.commands_token)? {
            let s = sampler.clone();
            self.retained.borrow_mut().push(Box::new(move || s.release_submission()));
        }
        Ok(())
    }

    pub fn resource(&self, token: i64) -> Option<Resource> {
        self.tables.resource(token)
    }

    pub fn pipeline(&self, token: i64) -> Option<Pipeline> {
        self.tables.pipelines.borrow().get(&token).cloned()
    }

    pub fn sampler(&self, token: i64) -> Option<Sampler> {
        self.tables.samplers.borrow().get(&token).cloned()
    }

    pub fn icb(&self, token: i64) -> Option<metal::IndirectCommandBuffer> {
        self.tables.icbs.borrow().get(&token).map(|entry| entry.icb.clone())
    }

    pub fn depth_state(&self, state: DepthState) -> Result<DepthStencil, Error> {
        self.tables.depth_state_for(state)
    }
}
