use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use crate::native::resource::{error, sys_error, enter_main, validate_text, Device, Error, ErrorKind, Lifetime};
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
    pub fn compile_source(device: &Device, source: &str, label: Option<&str>) -> Result<Self, Error> {
        const OP: &str = "Metal.Library.compile_source";
        enter_main(OP)?;
        device.lifetime().ensure_live(OP)?;
        if source.is_empty() { return Err(error(OP, ErrorKind::InvalidArgument, "shader source is empty")); }
        validate_text(OP, source)?;
        if let Some(label) = label { validate_text(OP, label)?; }
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
    pub fn warm(device: &Device, source: &str, entry: &str) -> Result<metal_sys::native::library::Warm, Error> {
        const OP: &str = "Metal.Library.warm";
        device.lifetime().ensure_live(OP)?;
        validate_text(OP, source)?;
        validate_text(OP, entry)?;
        metal_sys::native::library::library_warm(device.raw(), source, entry).map_err(|failure| sys_error(OP, failure))
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

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum FunctionConstantValue {
    // The narrow integer kinds carry an i64 so an out-of-range value reaches the range check (as in lib/metal).
    Bool(bool), Int8(i64), Uint8(i64), Int16(i64), Uint16(i64), Int32(i32), Uint32(i64),
    Int64(i64), Uint64Bits(i64), Float16(f64), Float32(f64),
}
impl FunctionConstantValue {
    fn raw(self) -> Result<(i32, i64, f64), &'static str> {
        let within = |value: i64, low: i64, high: i64| (low..=high).contains(&value);
        Ok(match self {
            Self::Bool(value) => (0, i64::from(value), 0.0),
            Self::Int8(value) if within(value, -128, 127) => (1, value, 0.0),
            Self::Uint8(value) if within(value, 0, 255) => (2, value, 0.0),
            Self::Int16(value) if within(value, -32_768, 32_767) => (3, value, 0.0),
            Self::Uint16(value) if within(value, 0, 65_535) => (4, value, 0.0),
            Self::Int32(value) => (5, i64::from(value), 0.0),
            Self::Uint32(value) if within(value, 0, 0xffff_ffff) => (6, value, 0.0),
            Self::Int64(value) => (7, value, 0.0),
            Self::Uint64Bits(value) => (8, value, 0.0),
            Self::Float16(value) => (9, 0, value),
            Self::Float32(value) => (10, 0, value),
            Self::Int8(_) => return Err("int8 function constant is out of range"),
            Self::Uint8(_) => return Err("uint8 function constant is out of range"),
            Self::Int16(_) => return Err("int16 function constant is out of range"),
            Self::Uint16(_) => return Err("uint16 function constant is out of range"),
            Self::Uint32(_) => return Err("uint32 function constant is out of range"),
        })
    }
}

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
    pub fn find(library: &Library, name: &str) -> Result<Self, Error> {
        const OP: &str = "Metal.Function.find";
        Self::create(library, name, OP, &|| metal_sys::native::find_function(library.raw(), name).map_err(|failure| sys_error(OP, failure)))
    }

    pub fn specialize(library: &Library, constants: &[(String, FunctionConstantValue)], name: &str) -> Result<Self, Error> {
        const OP: &str = "Metal.Function.specialize";
        Self::create(library, name, OP, &|| {
            let mut raw_constants: Vec<(String, i32, i64, f64)> = Vec::with_capacity(constants.len());
            for (constant_name, value) in constants {
                if constant_name.is_empty() || constant_name.as_bytes().contains(&0) {
                    return Err(error(OP, ErrorKind::InvalidArgument, "function-constant names must be nonempty and contain no NUL byte"));
                }
                if raw_constants.iter().any(|(seen, _, _, _)| seen == constant_name) {
                    return Err(error(OP, ErrorKind::InvalidArgument, "function-constant list contains a duplicate name"));
                }
                let (tag, integral, floating) = value.raw().map_err(|message| error(OP, ErrorKind::InvalidArgument, message))?;
                raw_constants.push((constant_name.clone(), tag, integral, floating));
            }
            metal_sys::native::specialize_function(library.raw(), name, &raw_constants, None).map_err(|failure| sys_error(OP, failure))
        })
    }

    pub fn argument_encoder(&self, buffer_index: u64) -> Result<ShaderArgumentEncoder, Error> {
        const OP: &str = "Metal.Function.argument_encoder";
        enter_main(OP)?;
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
            device: self.device().clone(),
            encoded_length,
            alignment,
            retained: std::collections::HashMap::new(),
            function_attached: AtomicBool::new(true),
        })
    }

    fn create(
        library: &Library,
        name: &str,
        operation: &'static str,
        make: &dyn Fn() -> Result<metal_sys::Handle, Error>,
    ) -> Result<Self, Error> {
        enter_main(operation)?;
        library.lifetime().ensure_live(operation)?;
        if name.is_empty() || name.as_bytes().contains(&0) {
            return Err(error(operation, ErrorKind::InvalidArgument, "function name must be nonempty and contain no NUL byte"));
        }
        let raw = make()?;
        library.0.lifetime.dependents.fetch_add(1, Ordering::AcqRel);
        Ok(Self(Arc::new(FunctionInner {
            raw,
            lifetime: Lifetime::new(),
            library: library.clone(),
            library_attached: AtomicBool::new(true),
        })))
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
    fn device(&self) -> &Device {
        match self {
            Self::Buffer(value) => value.device(),
            Self::Texture(value) => value.device(),
            Self::Sampler(value) => value.device(),
            Self::AccelerationStructure(value) => value.device(),
            Self::IndirectCommandBuffer(value) => value.device(),
            Self::RenderPipeline(value) => value.device(),
            Self::ComputePipeline(value) => value.device(),
            Self::DepthStencil(value) => value.device(),
        }
    }
}

pub struct ShaderArgumentEncoder {
    raw: metal_sys::Handle,
    lifetime: Lifetime,
    function: Function,
    device: Device,
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
        self.lifetime.ensure_live(OP)?;
        let index = i64::try_from(index).map_err(|_| error(OP, ErrorKind::InvalidArgument, "argument index exceeds signed 64-bit range"))?;
        resource.lifetime().ensure_live(OP)?;
        if !self.device.same(resource.device()) {
            return Err(error(OP, ErrorKind::CrossDevice, "argument resource belongs to another device"));
        }
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
        self.lifetime.ensure_live(OP)?;
        buffer.lifetime().ensure_live(OP)?;
        if !self.device.same(buffer.device()) {
            return Err(error(OP, ErrorKind::CrossDevice, "argument buffer belongs to another device"));
        }
        if offset > buffer.length() {
            return Err(error(OP, ErrorKind::InvalidArgument, "argument buffer range is invalid"));
        }
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
