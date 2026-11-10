//! Ogpu_metal.Command_control: what the encoders share while one command buffer records, and the
//! token tables of the device behind them.
use crate::native;
use ogpu_core::error::Error;
use crate::{
    acceleration::Acceleration,
    buffer::Buffer,
    device::{fail, Device},
    pipeline::Pipeline,
    queue::Retained,
    sampler::Sampler,
    texture::Texture,
};
use crate::native::{DepthStencil, DepthStencilDescriptor};
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
    pub icb: native::IndirectCommandBuffer,
    pub commands: RefCell<Vec<Option<native::RenderCommand>>>,
}

/// The token tables of one device.
pub struct Tables {
    pub resources: RefCell<HashMap<i64, Resource>>,
    pub pipelines: RefCell<HashMap<i64, Pipeline>>,
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
                    depth_compare: depth_compare,
                    depth_write,
                    label: Some("ogpu-metal-depth-state".to_owned()),
                    ..DepthStencilDescriptor::default()
                };
                let native = DepthStencil::create(device.metal(), descriptor)?;
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

/// The encoder a recording command buffer has open; `abandon` ends it.
pub enum OpenEncoder {
    Compute(native::ComputeEncoder),
    Accel(native::AccelerationEncoder),
    Blit(native::BlitEncoder),
    Render(native::RenderEncoder, native::RenderPassDescriptor),
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
    pub native: native::CommandBuffer,
    pub tables: Rc<Tables>,
    pub commands_token: i64,
    pub open_encoder: RefCell<Option<OpenEncoder>>,
    pub retained: RefCell<Vec<Retained>>,
}

impl CommandContext {
    /// One retain per buffer per command buffer, however many bindings reference it.
    pub fn keep_buffer(&self, buffer: &Buffer) -> Result<(), Error> {
        if buffer.retain_for(self.commands_token)? {
            self.retained.borrow_mut().push(Retained::Buffer(buffer.clone()));
        }
        Ok(())
    }

    pub fn keep_pipeline(&self, pipeline: &Pipeline) -> Result<(), Error> {
        if pipeline.destroyed() {
            return Err(fail("Ogpu_metal.Pipeline.retain", ErrorKind::StaleHandle, "pipeline is destroyed"));
        }
        self.retained.borrow_mut().push(Retained::Pipeline(pipeline.clone()));
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

    /// The structure and the buffers it holds now (a refit replaces them).
    pub fn keep_accel(&self, structure: &Acceleration) -> Result<(), Error> {
        if structure.destroyed() {
            return Err(fail("Ogpu_metal.Acceleration.retain", ErrorKind::StaleHandle, "acceleration structure is destroyed"));
        }
        let mut held = self.retained.borrow_mut();
        held.extend(structure.buffers().into_iter().map(Retained::Buffer));
        held.push(Retained::Accel(structure.clone()));
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
            self.retained.borrow_mut().push(Retained::Texture(texture.clone()));
        }
        Ok(())
    }

    /// One retain per sampler per command buffer, however many bindings reference it.
    pub fn keep_sampler(&self, sampler: &Sampler) -> Result<(), Error> {
        if sampler.retain_for(self.commands_token)? {
            self.retained.borrow_mut().push(Retained::Sampler(sampler.clone()));
        }
        Ok(())
    }

    pub fn resource(&self, token: i64) -> Option<Resource> {
        self.tables.resource(token)
    }

    pub fn pipeline(&self, token: i64) -> Option<Pipeline> {
        self.tables.pipelines.borrow().get(&token).cloned()
    }

    pub fn icb(&self, token: i64) -> Option<native::IndirectCommandBuffer> {
        self.tables.icbs.borrow().get(&token).map(|entry| entry.icb.clone())
    }

    pub fn depth_state(&self, state: DepthState) -> Result<DepthStencil, Error> {
        self.tables.depth_state_for(state)
    }
}
