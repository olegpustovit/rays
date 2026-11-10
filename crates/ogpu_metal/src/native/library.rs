use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use crate::native::resource::{error, sys_error, enter_main, Device, Error, ErrorKind, Lifetime};
use metal_sys;

struct LibraryInner {
    raw: metal_sys::Handle,
    lifetime: Lifetime,
    device: Device,
    device_attached: AtomicBool,
}
impl Drop for LibraryInner {
    fn drop(&mut self) {
        self.lifetime.destroyed.store(true, Ordering::Release);
        if self.device_attached.swap(false, Ordering::AcqRel) {
            self.device.lifetime().dependents.fetch_sub(1, Ordering::AcqRel);
        }
    }
}

#[derive(Clone)]
pub struct Library(Arc<LibraryInner>);
impl Library {
    pub fn compile_source(device: &Device, shader: &ogpu_core::shader::Shader) -> Result<Self, Error> {
        const OP: &str = "Metal.Library.compile_source";
        enter_main(OP)?;
        device.lifetime().ensure_live(OP)?;
        let source = ogpu_core::shader::source(shader).ok_or_else(|| error(OP, ErrorKind::InvalidArgument, "shader backend is not Metal"))?;
        let label = ogpu_core::shader::label(shader);
        let raw = metal_sys::native::compile_library(device.raw(), source, label).map_err(|failure| sys_error(OP, failure))?;
        device.lifetime().dependents.fetch_add(1, Ordering::AcqRel);
        Ok(Self(Arc::new(LibraryInner {
            raw,
            lifetime: Lifetime::new(),
            device: device.clone(),
            device_attached: AtomicBool::new(true),
        })))
    }

    /// Compiles `source` and `entry`'s compute pipeline in Metal's background, so a later
    /// `compile_source` of the same source is a cache hit; poll the result, drop it any time.
    pub fn warm(device: &Device, shader: &ogpu_core::shader::Shader) -> Result<metal_sys::native::library::Warm, Error> {
        const OP: &str = "Metal.Library.warm";
        enter_main(OP)?;
        device.lifetime().ensure_live(OP)?;
        let source = ogpu_core::shader::source(shader).ok_or_else(|| error(OP, ErrorKind::InvalidArgument, "shader backend is not Metal"))?;
        let entry = ogpu_core::shader::entry_points(shader).first().ok_or_else(|| error(OP, ErrorKind::InvalidArgument, "warm library needs an entry point"))?;
        metal_sys::native::library::library_warm(device.raw(), source, &entry.name).map_err(|failure| sys_error(OP, failure))
    }

    pub fn function_names(&self) -> Result<Vec<String>, Error> {
        const OP: &str = "Metal.Library.function_names";
        self.0.lifetime.ensure_live(OP)?;
        metal_sys::native::library_function_names(&self.0.raw).map_err(|failure| sys_error(OP, failure))
    }

    pub fn destroy(&self) -> Result<(), Error> {
        self.0.lifetime.destroy_parent("Metal.Library.destroy")?;
        if self.0.device_attached.swap(false, Ordering::AcqRel) {
            self.0.device.lifetime().dependents.fetch_sub(1, Ordering::AcqRel);
        }
        Ok(())
    }

    pub(crate) fn raw(&self) -> &metal_sys::Handle { &self.0.raw }
    pub(crate) fn device(&self) -> &Device { &self.0.device }
    pub(crate) fn lifetime(&self) -> &Lifetime { &self.0.lifetime }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum FunctionKind { Vertex, Fragment, Kernel, Visible, Intersection, Mesh, Object, Unknown(i64) }

pub use ogpu_core::shader::{FunctionConstantValue, AdmittedFunction, AdmittedComputeOptions};

struct FunctionInner {
    raw: metal_sys::Handle,
    lifetime: Lifetime,
    library: Library,
    library_attached: AtomicBool,
}
impl Drop for FunctionInner {
    fn drop(&mut self) {
        self.lifetime.destroyed.store(true, Ordering::Release);
        if self.library_attached.swap(false, Ordering::AcqRel) {
            self.library.0.lifetime.dependents.fetch_sub(1, Ordering::AcqRel);
        }
    }
}

#[derive(Clone)]
pub struct Function(Arc<FunctionInner>);
impl Function {
    pub fn create(library: &Library, profile: &AdmittedFunction) -> Result<Self, Error> {
        const OP: &str = "Metal.Function.create";
        enter_main(OP)?;
        library.lifetime().ensure_live(OP)?;
        let raw = metal_sys::native::create_function(library.raw(), profile, None).map_err(|failure| sys_error(OP, failure))?;
        library.0.lifetime.dependents.fetch_add(1, Ordering::AcqRel);
        Ok(Self(Arc::new(FunctionInner { raw, lifetime: Lifetime::new(), library: library.clone(), library_attached: AtomicBool::new(true) })))
    }

    pub fn argument_encoder(&self, buffer_index: ogpu_core::validation::AdmittedSlot) -> Result<ShaderArgumentEncoder, Error> {
        const OP: &str = "Metal.Function.argument_encoder";
        self.0.lifetime.ensure_live(OP)?;
        let raw = metal_sys::registry::shader_function_argument_encoder(&self.0.raw, buffer_index).map_err(|failure| sys_error(OP, failure))?;
        let (_, encoded_length, alignment, registry_id) =
            match metal_sys::native::library::argument_encoder_snapshot(&raw).map_err(|failure| sys_error(OP, failure)) {
                Ok(snapshot) => snapshot,
                Err(failure) => return Err(failure),
            };
        if registry_id != self.device().registry_id() {
            return Err(error(OP, ErrorKind::CrossDevice, "argument encoder device disagrees with function"));
        }
        self.0.lifetime.dependents.fetch_add(1, Ordering::AcqRel);
        Ok(ShaderArgumentEncoder {
            raw,
            lifetime: Lifetime::new(),
            function: self.clone(),
            _device: self.device().clone(),
            encoded_length,
            alignment,
            retained: std::collections::HashMap::new(),
            function_attached: AtomicBool::new(true),
        })
    }


    pub fn kind(&self) -> Result<FunctionKind, Error> {
        const OP: &str = "Metal.Function.kind";
        self.0.lifetime.ensure_live(OP)?;
        let kind = metal_sys::registry::function_kind(&self.0.raw).map_err(|failure| sys_error(OP, failure))?;
        Ok(match kind {
            1 => FunctionKind::Vertex, 2 => FunctionKind::Fragment, 3 => FunctionKind::Kernel,
            5 => FunctionKind::Visible, 6 => FunctionKind::Intersection, 7 => FunctionKind::Mesh,
            8 => FunctionKind::Object, code => FunctionKind::Unknown(code),
        })
    }

    pub fn destroy(&self) -> Result<(), Error> {
        self.0.lifetime.destroy_parent("Metal.Function.destroy")?;
        if self.0.library_attached.swap(false, Ordering::AcqRel) {
            self.0.library.0.lifetime.dependents.fetch_sub(1, Ordering::AcqRel);
        }
        Ok(())
    }

    pub(crate) fn raw(&self) -> &metal_sys::Handle { &self.0.raw }
    pub(crate) fn device(&self) -> &Device { self.0.library.device() }
    pub(crate) fn lifetime(&self) -> &Lifetime { &self.0.lifetime }
}
#[derive(Clone)]
pub enum ShaderArgumentResource {
    Buffer(crate::native::resource::Buffer),
    Texture(crate::native::resource::Texture),
    Sampler(crate::native::descriptors::Sampler),
    AccelerationStructure(crate::native::acceleration::AccelerationStructure),
    IndirectCommandBuffer(crate::native::indirect::IndirectCommandBuffer),
    RenderPipeline(crate::native::pipeline::RenderPipeline),
    ComputePipeline(crate::native::resource::ComputePipeline),
    DepthStencil(crate::native::descriptors::DepthStencil),
}
impl ShaderArgumentResource {
    fn raw(&self) -> &metal_sys::Handle {
        match self {
            Self::Buffer(value) => value.raw(),
            Self::Texture(value) => value.raw(),
            Self::Sampler(value) => value.raw(),
            Self::AccelerationStructure(value) => value.raw(),
            Self::IndirectCommandBuffer(value) => value.raw(),
            Self::RenderPipeline(value) => value.raw(),
            Self::ComputePipeline(value) => value.raw(),
            Self::DepthStencil(value) => value.raw(),
        }
    }
    fn lifetime(&self) -> &Lifetime {
        match self {
            Self::Buffer(value) => value.lifetime(),
            Self::Texture(value) => value.lifetime(),
            Self::Sampler(value) => value.lifetime(),
            Self::AccelerationStructure(value) => value.lifetime(),
            Self::IndirectCommandBuffer(value) => value.lifetime(),
            Self::RenderPipeline(value) => value.lifetime(),
            Self::ComputePipeline(value) => value.lifetime(),
            Self::DepthStencil(value) => value.lifetime(),
        }
    }

}

pub struct ShaderArgumentEncoder {
    raw: metal_sys::Handle,
    lifetime: Lifetime,
    function: Function,
    _device: Device,
    encoded_length: u64,
    alignment: u64,
    retained: std::collections::HashMap<i64, ShaderArgumentResource>,
    function_attached: AtomicBool,
}
impl ShaderArgumentEncoder {
    pub fn encoded_length(&self) -> u64 { self.encoded_length }
    pub fn alignment(&self) -> u64 { self.alignment }

    pub fn set(&mut self, index: u64, resource: ShaderArgumentResource) -> Result<(), Error> {
        const OP: &str = "Metal.Shader_argument_encoder.set";
        let index = i64::try_from(index).map_err(|_| error(OP, ErrorKind::InvalidArgument, "argument index exceeds signed 64-bit range"))?;
        let tag = match &resource {
            ShaderArgumentResource::Buffer(_) => 0,
            ShaderArgumentResource::Texture(_) => 1,
            ShaderArgumentResource::Sampler(_) => 2,
            ShaderArgumentResource::AccelerationStructure(_) => 3,
            ShaderArgumentResource::IndirectCommandBuffer(_) => 4,
            ShaderArgumentResource::RenderPipeline(_) => 7,
            ShaderArgumentResource::ComputePipeline(_) => 8,
            ShaderArgumentResource::DepthStencil(_) => 9,
        };
        metal_sys::native::library::argument_encoder_single(&self.raw, tag, resource.raw(), 0, index).map_err(|failure| sys_error(OP, failure))?;
        if self.retained.get(&index).map_or(true, |old| !old.raw().same(resource.raw())) {
            if let Some(old) = self.retained.insert(index, resource.clone()) {
                old.lifetime().dependents.fetch_sub(1, Ordering::AcqRel);
            }
            resource.lifetime().dependents.fetch_add(1, Ordering::AcqRel);
        }
        Ok(())
    }

    pub fn set_argument_buffer(&mut self, buffer: &crate::native::resource::Buffer, offset: u64) -> Result<(), Error> {
        const OP: &str = "Metal.Shader_argument_encoder.set_argument_buffer";
        let offset = i64::try_from(offset).map_err(|_| error(OP, ErrorKind::InvalidArgument, "argument buffer offset exceeds signed 64-bit range"))?;
        metal_sys::native::library::argument_encoder_set_buffer(&self.raw, buffer.raw(), offset, 0, 0).map_err(|failure| sys_error(OP, failure))?;
        let key = -1;
        if self.retained.get(&key).map_or(true, |old| !old.raw().same(buffer.raw())) {
            if let Some(old) = self.retained.insert(key, ShaderArgumentResource::Buffer(buffer.clone())) {
                old.lifetime().dependents.fetch_sub(1, Ordering::AcqRel);
            }
            buffer.lifetime().dependents.fetch_add(1, Ordering::AcqRel);
        }
        Ok(())
    }

    pub fn destroy(&mut self) -> Result<(), Error> {
        self.lifetime.destroy_parent("Metal.Shader_argument_encoder.destroy")?;
        self.release_retained();
        if self.function_attached.swap(false, Ordering::AcqRel) {
            self.function.lifetime().dependents.fetch_sub(1, Ordering::AcqRel);
        }
        Ok(())
    }

    fn release_retained(&mut self) {
        for resource in self.retained.drain().map(|(_, resource)| resource) {
            resource.lifetime().dependents.fetch_sub(1, Ordering::AcqRel);
        }
    }
}
impl Drop for ShaderArgumentEncoder {
    fn drop(&mut self) {
        self.lifetime.destroyed.store(true, Ordering::Release);
        self.release_retained();
        if self.function_attached.swap(false, Ordering::AcqRel) {
            self.function.lifetime().dependents.fetch_sub(1, Ordering::AcqRel);
        }
    }
}
