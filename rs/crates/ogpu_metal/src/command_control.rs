//! Ogpu_metal.Command_control: what the encoders share while one command buffer records, and the
//! token tables of the device behind them.
use crate::{
    acceleration::Acceleration,
    buffer::Buffer,
    device::{fail, of_metal_error, Device, R},
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

/// Encoders reuse depth-stencil states by value; the least recently used is released once more than
/// `DEPTH_STATE_CAPACITY` distinct states exist.
pub const DEPTH_STATE_CAPACITY: usize = 64;

/// The token tables of one device.
pub struct Tables {
    pub resources: RefCell<HashMap<i64, Resource>>,
    pub pipelines: RefCell<HashMap<i64, Pipeline>>,
    pub samplers: RefCell<HashMap<i64, Sampler>>,
    pub accels: RefCell<HashMap<i64, Acceleration>>,
    pub icbs: RefCell<HashMap<i64, Rc<IcbEntry>>>,
    /// Most recently used first.
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

    pub fn depth_state_for(&self, device: &Device, state: DepthState) -> R<DepthStencil> {
        let operation = "Ogpu_metal.Backend.depth_state";
        let mut states = self.depth_states.borrow_mut();
        if let Some(index) = states.iter().position(|(s, _)| *s == state) {
            let entry = states.remove(index);
            let native = entry.1.clone();
            states.insert(0, entry);
            return Ok(native);
        }
        let descriptor = DepthStencilDescriptor {
            depth_compare: metal_compare(state.depth_compare),
            depth_write: state.depth_write,
            label: Some("ogpu-metal-depth-state".to_owned()),
            ..DepthStencilDescriptor::default()
        };
        let native = DepthStencil::create(device.metal(), descriptor).map_err(|e| of_metal_error(operation, &e))?;
        states.insert(0, (state, native.clone()));
        if states.len() > DEPTH_STATE_CAPACITY {
            if let Some((_, evicted)) = states.pop() {
                let _ = evicted.destroy();
            }
        }
        Ok(native)
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

/// One recording command buffer: its native handle, the objects its commands retain and the open
/// encoder, if any.
pub struct CommandContext {
    pub operation: &'static str,
    pub device: Device,
    pub native: metal::CommandBuffer,
    pub tables: Rc<Tables>,
    pub commands_token: i64,
    pub open_encoder: RefCell<Option<Box<dyn Fn()>>>,
    pub finished: Cell<bool>,
    pub retained: RefCell<Vec<Cleanup>>,
}

impl CommandContext {
    pub fn native_of<T>(&self, result: Result<T, metal::Error>) -> R<T> {
        result.map_err(|e| of_metal_error(self.operation, &e))
    }

    pub fn recording(&self) -> R<()> {
        if self.finished.get() {
            fail(self.operation, ErrorKind::InvalidState, "commands are finished")
        } else {
            Ok(())
        }
    }

    /// Records `release` when `retained` succeeded.
    pub fn keep(&self, retained: R<()>, release: Cleanup) -> R<()> {
        retained?;
        self.retained.borrow_mut().push(release);
        Ok(())
    }

    /// One retain per buffer per command buffer, however many bindings reference it.
    pub fn keep_buffer(&self, buffer: &Buffer) -> R<()> {
        if buffer.retain_for(self.commands_token)? {
            let b = buffer.clone();
            self.retained.borrow_mut().push(Box::new(move || b.release_submission()));
        }
        Ok(())
    }

    pub fn find_buffer(&self, token: i64) -> R<Buffer> {
        match self.tables.resource(token) {
            Some(Resource::Buffer(buffer)) => Ok(buffer),
            _ => fail(self.operation, ErrorKind::InvalidArgument, "buffer graph is incomplete"),
        }
    }

    pub fn find_accel(&self, token: i64) -> R<Acceleration> {
        match self.tables.accels.borrow().get(&token) {
            Some(structure) => Ok(structure.clone()),
            None => fail(self.operation, ErrorKind::InvalidArgument, "acceleration structure graph is incomplete"),
        }
    }

    pub fn keep_accel(&self, structure: &Acceleration) -> R<()> {
        let release = structure.retain_submission()?;
        self.retained.borrow_mut().push(release);
        Ok(())
    }

    pub fn find_texture(&self, token: i64) -> R<Texture> {
        match self.tables.resource(token) {
            Some(Resource::Texture(texture)) => Ok(texture),
            _ => fail(self.operation, ErrorKind::InvalidArgument, "texture graph is incomplete"),
        }
    }

    pub fn keep_texture(&self, texture: &Texture) -> R<()> {
        let t = texture.clone();
        self.keep(texture.retain_submission(), Box::new(move || t.release_submission()))
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

    pub fn depth_state(&self, state: DepthState) -> R<DepthStencil> {
        self.tables.depth_state_for(&self.device, state)
    }
}
