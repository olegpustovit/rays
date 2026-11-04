use native_layer_token::Token;
use std::error::Error as StdError;
use std::fmt;
use vulkan_sys as sys;

pub use sys::{
    GeometryKind,
    AccelerationSizes, Acquire, Binding, BlitDescriptor, BufferKind, DeviceFacts, DeviceMemory,
    PipelineDescriptor, Rect, SamplerDescriptor, Size, TextureDescriptor,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ErrorKind {
    NoAdapter,
    Unsupported,
    DeviceLost,
    OutOfHostMemory,
    OutOfDeviceMemory,
    InvalidInput(&'static str),
    InvalidHandle,
    WrongDevice,
    Vulkan(i32),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Error { pub operation: &'static str, pub kind: ErrorKind }
impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "Vulkan {}: {:?}", self.operation, self.kind) }
}
impl StdError for Error {}

fn error(operation: &'static str, error: sys::Error) -> Error {
    let kind = match error {
        sys::Error::NoDevice | sys::Error::MissingFeature => ErrorKind::NoAdapter,
        sys::Error::VulkanResult(-4) => ErrorKind::DeviceLost,
        sys::Error::VulkanResult(-1) => ErrorKind::OutOfHostMemory,
        sys::Error::VulkanResult(-2) => ErrorKind::OutOfDeviceMemory,
        sys::Error::VulkanResult(-8) => ErrorKind::NoAdapter,
        sys::Error::VulkanResult(code) => ErrorKind::Vulkan(code),
        sys::Error::InvalidInput(message) => ErrorKind::InvalidInput(message),
        sys::Error::InvalidHandle => ErrorKind::InvalidHandle,
        sys::Error::WrongDevice => ErrorKind::WrongDevice,
    };
    Error { operation, kind }
}
macro_rules! map_result {
    ($operation:expr, $result:expr) => {
        $result.map_err(|failure| error($operation, failure))
    };
}

pub struct Device(sys::Device);
impl Device {
    pub fn create(validation: bool, fatal_validation: bool) -> Result<Self, Error> { map_result!("device_create", sys::Device::create(validation, fatal_validation)).map(Self) }
    pub fn validation_messages() -> u32 { sys::Device::validation_messages() }
    pub fn facts(&self) -> Result<DeviceFacts, Error> { map_result!("device_facts", self.0.facts()) }
    pub fn memory(&self) -> Result<DeviceMemory, Error> { map_result!("device_memory", self.0.memory()) }
    pub fn queue(&self) -> Result<Queue, Error> { map_result!("queue_create", self.0.queue()).map(Queue) }
    pub fn buffer(&self, size: u64, kind: BufferKind) -> Result<Buffer, Error> { map_result!("buffer_create", self.0.buffer(size, kind)).map(Buffer) }
    pub fn shader(&self, bytes: &[u8]) -> Result<ShaderModule, Error> { map_result!("shader_create", self.0.shader(bytes)).map(ShaderModule) }
    pub fn texture(&self, descriptor: TextureDescriptor) -> Result<Texture, Error> { map_result!("texture_create", self.0.texture(descriptor)).map(Texture) }
    pub fn zeroed_view(&self, count: usize) -> Result<ZeroedView, Error> { map_result!("zeroed_view", self.0.zeroed_view(count)).map(ZeroedView) }
    pub fn sampler(&self, descriptor: SamplerDescriptor) -> Result<Sampler, Error> { map_result!("sampler_create", self.0.sampler(descriptor)).map(Sampler) }
    pub fn surface(&self, token: &Token, owner: i64, generation: i64, immediate: bool) -> Result<Surface, Error> {
        let hwnd = token.borrow_raw(owner, generation).ok_or_else(|| Error { operation: "surface_create", kind: ErrorKind::InvalidInput("native layer token is not live") })?;
        map_result!("surface_create", self.0.surface(hwnd, immediate)).map(|surface| Surface { inner: surface, _token: token.clone() })
    }
    pub fn acceleration_blas(&self, geometries: &[Geometry<'_>], allow_refit: bool) -> Result<AccelerationStructure, Error> {
        let native = geometries.iter().map(Geometry::native).collect::<Vec<_>>();
        map_result!("accel_create_blas", self.0.acceleration_blas(&native, allow_refit)).map(AccelerationStructure)
    }
    pub fn acceleration_tlas(&self, instances: &Buffer, offset: u64, count: u32, structures: &[AccelerationStructure], build_children: bool) -> Result<AccelerationStructure, Error> {
        let structures = structures.iter().map(|a| &a.0).collect::<Vec<_>>();
        map_result!("accel_create_tlas", self.0.acceleration_tlas(&instances.0, offset, count, &structures, build_children)).map(AccelerationStructure)
    }
    pub fn acceleration_sized(&self, size: u64, template: &AccelerationStructure) -> Result<AccelerationStructure, Error> {
        map_result!("accel_create_sized", self.0.acceleration_sized(size, &template.0)).map(AccelerationStructure)
    }
}

pub struct Queue(sys::Queue);
impl Queue {
    pub fn begin(&self) -> Result<Commands, Error> { map_result!("commands_begin", self.0.begin()).map(Commands) }
    pub fn completed(&self) -> Result<u64, Error> { map_result!("queue_completed", self.0.completed()) }
    pub fn wait(&self, epoch: u64) -> Result<(), Error> { map_result!("queue_wait", self.0.wait(epoch)) }
    pub fn poll(&self) { self.0.poll() }
    pub fn duration(&self, epoch: u64) -> Option<f64> { self.0.duration(epoch) }
    pub fn total_seconds(&self) -> f64 { self.0.total_seconds() }
}

pub struct Commands(sys::Commands);
impl Commands {
    pub fn commit(self) -> Result<u64, Error> { map_result!("commands_commit", self.0.commit()) }
    pub fn set_pipeline(&mut self, pipeline: &Pipeline) -> Result<(), Error> { map_result!("set_pipeline", self.0.set_pipeline(&pipeline.0)) }
    pub fn set_buffer(&mut self, index: u32, offset: u64, buffer: &Buffer) -> Result<(), Error> { map_result!("set_buffer", self.0.set_buffer(index, offset, &buffer.0)) }
    pub fn set_bytes(&mut self, index: u32, bytes: &[u8]) -> Result<(), Error> { map_result!("set_bytes", self.0.set_bytes(index, bytes)) }
    pub fn set_texture(&mut self, index: u32, texture: &Texture) -> Result<(), Error> { map_result!("set_texture", self.0.set_texture(index, &texture.0)) }
    pub fn set_sampler(&mut self, index: u32, sampler: &Sampler) -> Result<(), Error> { map_result!("set_sampler", self.0.set_sampler(index, &sampler.0)) }
    pub fn set_acceleration(&mut self, index: u32, accel: &AccelerationStructure) -> Result<(), Error> { map_result!("set_accel", self.0.set_acceleration(index, &accel.0)) }
    pub fn compute_begin(&mut self) { self.0.compute_begin() }
    pub fn dispatch(&mut self, x: u32, y: u32, z: u32) { self.0.dispatch(x, y, z) }
    pub fn dispatch_indirect(&mut self, buffer: &Buffer, offset: u64) -> Result<(), Error> { map_result!("dispatch_indirect", self.0.dispatch_indirect(&buffer.0, offset)) }
    pub fn encoder_end(&mut self) { self.0.encoder_end() }
    pub fn blit_buffer_to_texture(&mut self, buffer: &Buffer, texture: &Texture, args: BlitDescriptor) -> Result<(), Error> { map_result!("blit_buffer_to_texture", self.0.blit_buffer_to_texture(&buffer.0, &texture.0, args)) }
    pub fn render_begin(&mut self, colors: &[ColorAttachment<'_>], depth: Option<DepthAttachment<'_>>, size: Size) -> Result<(), Error> {
        let colors = colors.iter().map(ColorAttachment::native).collect::<Vec<_>>();
        let depth = depth.as_ref().map(DepthAttachment::native);
        map_result!("render_begin", self.0.render_begin(&colors, depth, size))
    }
    pub fn render_end(&mut self) { self.0.render_end() }
    pub fn viewport(&mut self, rect: Rect) { self.0.viewport(rect) }
    pub fn scissor(&mut self, rect: Rect) -> Result<(), Error> { map_result!("render_scissor", self.0.scissor(rect)) }
    pub fn cull(&mut self, mode: i32) -> Result<(), Error> { map_result!("render_cull", self.0.cull(mode)) }
    pub fn winding(&mut self, clockwise: bool) { self.0.winding(clockwise) }
    pub fn depth(&mut self, comparison: i32, write: bool) -> Result<(), Error> { map_result!("render_depth", self.0.depth(comparison, write)) }
    pub fn draw(&mut self, primitive: i32, first: u32, count: u32, instances: u32) -> Result<(), Error> { map_result!("render_draw", self.0.draw(primitive, first, count, instances)) }
    pub fn draw_indexed(&mut self, buffer: &Buffer, primitive: i32, index_type: i32, offset: u64, count: u32, instances: u32) -> Result<(), Error> { map_result!("render_draw_indexed", self.0.draw_indexed(&buffer.0, primitive, index_type, offset, count, instances)) }
    pub fn draw_indexed_indirect(&mut self, buffer: &Buffer, arguments: &Buffer, primitive: i32, index_type: i32, offset: u64, arguments_offset: u64) -> Result<(), Error> { map_result!("render_draw_indexed_indirect", self.0.draw_indexed_indirect(&buffer.0, &arguments.0, primitive, index_type, offset, arguments_offset)) }
    pub fn build_acceleration(&mut self, accel: &AccelerationStructure, scratch: &Buffer, offset: u64) -> Result<(), Error> { map_result!("accel_build", self.0.build_acceleration(&accel.0, &scratch.0, offset)) }
    pub fn refit_acceleration(&mut self, accel: &AccelerationStructure, geometries: &[Geometry<'_>], scratch: &Buffer, offset: u64) -> Result<(), Error> {
        let native = geometries.iter().map(Geometry::native).collect::<Vec<_>>();
        map_result!("accel_refit", self.0.refit_acceleration(&accel.0, &native, &scratch.0, offset))
    }
    pub fn compact_acceleration(&mut self, source: &AccelerationStructure, destination: &AccelerationStructure) -> Result<(), Error> { map_result!("accel_compact", self.0.compact_acceleration(&source.0, &destination.0)) }
    pub fn write_compacted_size(&mut self, accel: &AccelerationStructure, buffer: &Buffer, offset: u64) -> Result<(), Error> { map_result!("accel_write_compacted", self.0.write_compacted_size(&accel.0, &buffer.0, offset)) }
}

pub struct Buffer(sys::Buffer);
impl Buffer {
    pub fn size(&self) -> u64 { self.0.size() }
    pub fn zero(&self) -> Result<(), Error> { map_result!("buffer_zero", self.0.zero()) }
    pub fn write(&mut self, offset: u64, bytes: &[u8]) -> Result<(), Error> { map_result!("buffer_write", self.0.write(offset, bytes)) }
    pub fn read(&self, offset: u64, bytes: &mut [u8]) -> Result<(), Error> { map_result!("buffer_read", self.0.read(offset, bytes)) }
    pub fn flush(&self, bytes: &[u8]) -> Result<(), Error> { map_result!("buffer_flush", self.0.flush(bytes)) }
    pub fn invalidate(&self, bytes: &mut [u8]) -> Result<(), Error> { map_result!("buffer_invalidate", self.0.invalidate(bytes)) }
    pub fn flush_view(&self, view: &ZeroedView, byte_len: u64) -> Result<(), Error> { map_result!("buffer_flush", self.0.flush_view(&view.0, byte_len)) }
    pub fn invalidate_view(&self, view: &mut ZeroedView, byte_len: u64) -> Result<(), Error> { map_result!("buffer_invalidate", self.0.invalidate_view(&mut view.0, byte_len)) }
}

pub struct ShaderModule(sys::ShaderModule);
pub struct Pipeline(sys::Pipeline);
impl Pipeline {
    pub fn compute(device: &Device, module: &ShaderModule, entry: &str, bindings: &[Binding]) -> Result<Self, Error> { map_result!("compute_pipeline_create", sys::Pipeline::compute(&device.0, &module.0, entry, bindings)).map(Self) }
    pub fn render(device: &Device, vertex: &ShaderModule, fragment: Option<&ShaderModule>, bindings: &[Binding], descriptor: PipelineDescriptor) -> Result<Self, Error> {
        map_result!("render_pipeline_create", sys::Pipeline::render(&device.0, &vertex.0, fragment.map(|f| &f.0), bindings, descriptor)).map(Self)
    }
}

pub struct Texture(sys::Texture);
impl Texture {
    pub fn descriptor(&self) -> TextureDescriptor { self.0.descriptor() }
    pub fn read(&self, bytes: &mut [u8], bytes_per_row: u64, texel_bytes: u32) -> Result<(), Error> { map_result!("texture_read", self.0.read(bytes, bytes_per_row, texel_bytes)) }
}
pub struct ZeroedView(sys::ZeroedView);
impl ZeroedView {
    pub fn len(&self) -> usize { self.0.len() }
    pub fn is_empty(&self) -> bool { self.0.is_empty() }
    pub fn as_slice(&self) -> &[f32] { self.0.as_slice() }
    pub fn as_mut_slice(&mut self) -> &mut [f32] { self.0.as_mut_slice() }
}
pub struct Sampler(sys::Sampler);
pub struct Surface { inner: sys::Surface, _token: Token }
impl Surface {
    pub fn configure(&mut self, immediate: bool) -> Result<(), Error> { map_result!("surface_configure", self.inner.configure(immediate)) }
    pub fn acquire(&mut self) -> Result<Acquire, Error> { map_result!("surface_acquire", self.inner.acquire()) }
    pub fn discard(&mut self) { self.inner.discard() }
    pub fn present(&mut self, commands: &mut Commands, source: &Texture) -> Result<u64, Error> { map_result!("commit_present", self.inner.present(&mut commands.0, &source.0)) }
}
#[derive(Clone)]
pub struct AccelerationStructure(sys::AccelerationStructure);
impl AccelerationStructure { pub fn sizes(&self) -> AccelerationSizes { self.0.sizes() } }

pub struct Geometry<'a> {
    pub kind: sys::GeometryKind,
    pub buffer: &'a Buffer,
    pub offset: u64,
    pub stride: u64,
    pub count: u32,
    pub opaque: bool,
    pub duplicate_any_hit: bool,
}
impl Geometry<'_> {
    fn native(&self) -> sys::Geometry<'_> { sys::Geometry { kind:self.kind,buffer:&self.buffer.0,offset:self.offset,stride:self.stride,count:self.count,opaque:self.opaque,duplicate_any_hit:self.duplicate_any_hit } }
}
pub struct ColorAttachment<'a> { pub texture:&'a Texture,pub resolve:Option<&'a Texture>,pub load:i32,pub store:i32,pub clear:[f64;4] }
impl ColorAttachment<'_> {
    fn native(&self)->sys::ColorAttachment<'_>{sys::ColorAttachment{texture:&self.texture.0,resolve:self.resolve.map(|t|&t.0),load:self.load,store:self.store,clear:self.clear}}
}
pub struct DepthAttachment<'a>{pub texture:&'a Texture,pub load:i32,pub store:i32,pub clear:f64}
impl DepthAttachment<'_>{fn native(&self)->sys::DepthAttachment<'_>{sys::DepthAttachment{texture:&self.texture.0,load:self.load,store:self.store,clear:self.clear}}}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn safe_texture_constructor_creates_a_texture() {
        let device = match Device::create(false, false) {
            Ok(device) => device,
            Err(error) if error.kind == ErrorKind::NoAdapter => return,
            Err(error) => panic!("Vulkan device creation failed: {error}"),
        };
        let descriptor = TextureDescriptor {
            width: 1, height: 1, depth: 1, mip_levels: 1, samples: 1,
            format: 1, storage: false, attachment: true,
        };
        let texture = device.texture(descriptor).expect("safe texture creation");
        assert_eq!(texture.descriptor(), descriptor);
    }

    #[test]
    fn missing_vulkan_adapter_features_have_the_no_adapter_kind() {
        for failure in [sys::Error::NoDevice, sys::Error::MissingFeature, sys::Error::VulkanResult(-8)] {
            assert_eq!(error("device_create", failure).kind, ErrorKind::NoAdapter);
        }
    }
}
