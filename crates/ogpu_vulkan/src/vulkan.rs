//! Safe Vulkan device API over `vulkan_sys` (Windows and Linux).
#![forbid(unsafe_code)]

use native_layer_token_sys::portable::Token;
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
    Unsupported(&'static str),
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
        sys::Error::Unsupported(message) => ErrorKind::Unsupported(message),
        sys::Error::InvalidInput(message) => ErrorKind::InvalidInput(message),
        sys::Error::InvalidHandle => ErrorKind::InvalidHandle,
        sys::Error::WrongDevice => ErrorKind::WrongDevice,
    };
    Error { operation, kind }
}

pub struct Device(sys::Device);
impl Device {
    pub fn create(validation: bool, fatal_validation: bool) -> Result<Self, Error> { sys::Device::create(validation, fatal_validation).map_err(|failure| error("device_create", failure)).map(Self) }
    pub fn validation_messages() -> u32 { sys::Device::validation_messages() }
    pub fn facts(&self) -> Result<DeviceFacts, Error> { self.0.facts().map_err(|failure| error("device_facts", failure)) }
    pub fn memory(&self) -> Result<DeviceMemory, Error> { self.0.memory().map_err(|failure| error("device_memory", failure)) }
    pub fn queue(&self) -> Result<Queue, Error> { self.0.queue().map_err(|failure| error("queue_create", failure)).map(Queue) }
    pub fn buffer(&self, size: u64, kind: BufferKind) -> Result<Buffer, Error> { self.0.buffer(size, kind).map_err(|failure| error("buffer_create", failure)).map(Buffer) }
    pub fn shader(&self, bytes: &[u8]) -> Result<ShaderModule, Error> { self.0.shader(bytes).map_err(|failure| error("shader_create", failure)).map(ShaderModule) }
    pub fn texture(&self, descriptor: &ogpu_core::validation::AdmittedTexture) -> Result<Texture, Error> { self.0.texture(descriptor).map_err(|failure| error("texture_create", failure)).map(Texture) }
    pub fn zeroed_view(&self, count: usize) -> Result<ZeroedView, Error> { self.0.zeroed_view(count).map_err(|failure| error("zeroed_view", failure)).map(ZeroedView) }
    pub fn sampler(&self, descriptor: SamplerDescriptor) -> Result<Sampler, Error> { self.0.sampler(descriptor).map_err(|failure| error("sampler_create", failure)).map(Sampler) }
    pub fn surface(&self, token: &Token, owner: i64, generation: i64, immediate: bool) -> Result<Surface, Error> {
        let hwnd = token.borrow_raw(owner, generation).ok_or_else(|| Error { operation: "surface_create", kind: ErrorKind::InvalidInput("native layer token is not live") })?;
        self.0.surface(hwnd, immediate).map_err(|failure| error("surface_create", failure)).map(|surface| Surface { inner: surface, _token: token.clone() })
    }
    pub fn acceleration_blas(&self, geometries: &[Geometry], allow_refit: bool) -> Result<AccelerationStructure, Error> {
        let native = geometries.iter().map(Geometry::native).collect::<Vec<_>>();
        self.0.acceleration_blas(&native, allow_refit).map_err(|failure| error("accel_create_blas", failure)).map(AccelerationStructure)
    }
    pub fn acceleration_tlas(&self, instances: &Buffer, offset: u64, count: u32, structures: &[AccelerationStructure], build_children: bool) -> Result<AccelerationStructure, Error> {
        let structures = structures.iter().map(|a| &a.0).collect::<Vec<_>>();
        self.0.acceleration_tlas(&instances.0, offset, count, &structures, build_children).map_err(|failure| error("accel_create_tlas", failure)).map(AccelerationStructure)
    }
    pub fn acceleration_sized(&self, size: std::num::NonZeroU64, template: &AccelerationStructure) -> Result<AccelerationStructure, Error> {
        self.0.acceleration_sized(size, &template.0).map_err(|failure| error("accel_create_sized", failure)).map(AccelerationStructure)
    }
}

pub struct Queue(sys::Queue);
impl Queue {
    pub fn begin(&self) -> Result<Commands, Error> { self.0.begin().map_err(|failure| error("commands_begin", failure)).map(Commands) }
    pub fn completed(&self) -> Result<u64, Error> { self.0.completed().map_err(|failure| error("queue_completed", failure)) }
    pub fn wait(&self, epoch: u64) -> Result<(), Error> { self.0.wait(epoch).map_err(|failure| error("queue_wait", failure)) }
    pub fn poll(&self) { self.0.poll() }
    pub fn duration(&self, epoch: u64) -> Option<f64> { self.0.duration(epoch) }
    pub fn total_seconds(&self) -> f64 { self.0.total_seconds() }
}

pub struct Commands(sys::Commands);
impl Commands {
    pub fn commit(self) -> Result<u64, Error> { self.0.commit().map_err(|failure| error("commands_commit", failure)) }
    pub fn set_pipeline(&mut self, pipeline: &Pipeline) -> Result<(), Error> { self.0.set_pipeline(&pipeline.0).map_err(|failure| error("set_pipeline", failure)) }
    pub fn set_buffer(&mut self, index: u32, offset: u64, buffer: &Buffer) -> Result<(), Error> { self.0.set_buffer(index, offset, &buffer.0).map_err(|failure| error("set_buffer", failure)) }
    pub fn set_bytes(&mut self, index: u32, bytes: &[u8]) -> Result<(), Error> { self.0.set_bytes(index, bytes).map_err(|failure| error("set_bytes", failure)) }
    pub fn set_texture(&mut self, index: u32, texture: &Texture) -> Result<(), Error> { self.0.set_texture(index, &texture.0).map_err(|failure| error("set_texture", failure)) }
    pub fn set_sampler(&mut self, index: u32, sampler: &Sampler) -> Result<(), Error> { self.0.set_sampler(index, &sampler.0).map_err(|failure| error("set_sampler", failure)) }
    pub fn set_acceleration(&mut self, index: u32, accel: &AccelerationStructure) -> Result<(), Error> { self.0.set_acceleration(index, &accel.0).map_err(|failure| error("set_accel", failure)) }
    pub fn compute_begin(&mut self) { self.0.compute_begin() }
    pub fn dispatch(&mut self, x: u32, y: u32, z: u32) { self.0.dispatch(x, y, z) }
    pub fn dispatch_indirect(&mut self, buffer: &Buffer, offset: u64) -> Result<(), Error> { self.0.dispatch_indirect(&buffer.0, offset).map_err(|failure| error("dispatch_indirect", failure)) }
    pub fn encoder_end(&mut self) { self.0.encoder_end() }
    pub fn blit_buffer_to_texture(&mut self, buffer: &Buffer, texture: &Texture, args: BlitDescriptor) -> Result<(), Error> { self.0.blit_buffer_to_texture(&buffer.0, &texture.0, args).map_err(|failure| error("blit_buffer_to_texture", failure)) }
    pub fn render_begin(&mut self, colors: &[ColorAttachment], depth: Option<DepthAttachment>, size: Size) -> Result<(), Error> {
        let colors = colors.iter().map(ColorAttachment::native).collect::<Vec<_>>();
        let depth = depth.as_ref().map(DepthAttachment::native);
        self.0.render_begin(&colors, depth, size).map_err(|failure| error("render_begin", failure))
    }
    pub fn render_end(&mut self) { self.0.render_end() }
    pub fn viewport(&mut self, rect: Rect) { self.0.viewport(rect) }
    pub fn scissor(&mut self, rect: Rect) -> Result<(), Error> { self.0.scissor(rect).map_err(|failure| error("render_scissor", failure)) }
    pub fn cull(&mut self, mode: i32) -> Result<(), Error> { self.0.cull(mode).map_err(|failure| error("render_cull", failure)) }
    pub fn winding(&mut self, clockwise: bool) { self.0.winding(clockwise) }
    pub fn depth(&mut self, comparison: i32, write: bool) -> Result<(), Error> { self.0.depth(comparison, write).map_err(|failure| error("render_depth", failure)) }
    pub fn draw(&mut self, primitive: i32, first: u32, count: u32, instances: u32) -> Result<(), Error> { self.0.draw(primitive, first, count, instances).map_err(|failure| error("render_draw", failure)) }
    pub fn draw_indexed(&mut self, buffer: &Buffer, primitive: i32, index_type: i32, offset: u64, count: u32, instances: u32) -> Result<(), Error> { self.0.draw_indexed(&buffer.0, primitive, index_type, offset, count, instances).map_err(|failure| error("render_draw_indexed", failure)) }
    pub fn draw_indexed_indirect(&mut self, buffer: &Buffer, arguments: &Buffer, primitive: i32, index_type: i32, offset: u64, arguments_offset: u64) -> Result<(), Error> { self.0.draw_indexed_indirect(&buffer.0, &arguments.0, primitive, index_type, offset, arguments_offset).map_err(|failure| error("render_draw_indexed_indirect", failure)) }
    pub fn build_acceleration(&mut self, accel: &AccelerationStructure, scratch: &Buffer, offset: u64) -> Result<(), Error> { self.0.build_acceleration(&accel.0, &scratch.0, offset).map_err(|failure| error("accel_build", failure)) }
    pub fn refit_acceleration(&mut self, accel: &AccelerationStructure, geometries: &[Geometry], scratch: &Buffer, offset: u64) -> Result<(), Error> {
        let native = geometries.iter().map(Geometry::native).collect::<Vec<_>>();
        self.0.refit_acceleration(&accel.0, &native, &scratch.0, offset).map_err(|failure| error("accel_refit", failure))
    }
    pub fn compact_acceleration(&mut self, source: &AccelerationStructure, destination: &AccelerationStructure) -> Result<(), Error> { self.0.compact_acceleration(&source.0, &destination.0).map_err(|failure| error("accel_compact", failure)) }
    pub fn write_compacted_size(&mut self, accel: &AccelerationStructure, buffer: &Buffer, output:ogpu_core::validation::AdmittedCompactedSize) -> Result<(), Error> { self.0.write_compacted_size(&accel.0, &buffer.0, output).map_err(|failure| error("accel_write_compacted", failure)) }
}

#[derive(Clone)]
pub struct Buffer(sys::Buffer);
impl Buffer {
    pub fn size(&self) -> u64 { self.0.size() }
    pub fn zero(&self) -> Result<(), Error> { self.0.zero().map_err(|failure| error("buffer_zero", failure)) }
    pub fn write(&mut self, offset: u64, bytes: &[u8]) -> Result<(), Error> { self.0.write(offset, bytes).map_err(|failure| error("buffer_write", failure)) }
    pub fn read(&self, offset: u64, bytes: &mut [u8]) -> Result<(), Error> { self.0.read(offset, bytes).map_err(|failure| error("buffer_read", failure)) }
    pub fn host_ptr(&self) -> Option<std::ptr::NonNull<u8>> { self.0.host_ptr() }
    pub fn flush_range(&self, offset: u64, length: u64) -> Result<(), Error> { self.0.flush_range(offset, length).map_err(|failure| error("buffer_flush_range", failure)) }
    pub fn flush(&self, bytes: &[u8]) -> Result<(), Error> { self.0.flush(bytes).map_err(|failure| error("buffer_flush", failure)) }
    pub fn invalidate(&self, bytes: &mut [u8]) -> Result<(), Error> { self.0.invalidate(bytes).map_err(|failure| error("buffer_invalidate", failure)) }
    pub fn flush_view(&self, view: &ZeroedView, byte_len: u64) -> Result<(), Error> { self.0.flush_view(&view.0, byte_len).map_err(|failure| error("buffer_flush", failure)) }
    pub fn invalidate_view(&self, view: &mut ZeroedView, byte_len: u64) -> Result<(), Error> { self.0.invalidate_view(&mut view.0, byte_len).map_err(|failure| error("buffer_invalidate", failure)) }
}

pub struct ShaderModule(sys::ShaderModule);
pub struct Pipeline(sys::Pipeline);
impl Pipeline {
    pub fn compute(device: &Device, module: &ShaderModule, entry: &str, bindings: &[Binding]) -> Result<Self, Error> { sys::Pipeline::compute(&device.0, &module.0, entry, bindings).map_err(|failure| error("compute_pipeline_create", failure)).map(Self) }
    pub fn render(device: &Device, vertex: &ShaderModule, fragment: Option<&ShaderModule>, bindings: &[Binding], descriptor: PipelineDescriptor) -> Result<Self, Error> {
        sys::Pipeline::render(&device.0, &vertex.0, fragment.map(|f| &f.0), bindings, descriptor).map_err(|failure| error("render_pipeline_create", failure)).map(Self)
    }
}

#[derive(Clone)]
pub struct Texture(sys::Texture);
impl Texture {
    pub fn descriptor(&self) -> TextureDescriptor { self.0.descriptor() }
    pub fn write(&self, bytes: &[u8], args: BlitDescriptor) -> Result<(), Error> { self.0.write(bytes, args).map_err(|failure| error("texture_write", failure)) }
    pub fn read(&self, bytes: &mut [u8], bytes_per_row: u64, texel_bytes: u32) -> Result<(), Error> { self.0.read(bytes, bytes_per_row, texel_bytes).map_err(|failure| error("texture_read", failure)) }
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
    pub fn configure(&mut self, immediate: bool) -> Result<(), Error> { self.inner.configure(immediate).map_err(|failure| error("surface_configure", failure)) }
    pub fn acquire(&mut self) -> Result<Acquire, Error> { self.inner.acquire().map_err(|failure| error("surface_acquire", failure)) }
    pub fn discard(&mut self) { self.inner.discard() }
    pub fn present(&mut self, commands: &mut Commands, source: &Texture) -> Result<u64, Error> { self.inner.present(&mut commands.0, &source.0).map_err(|failure| error("commit_present", failure)) }
}
#[derive(Clone)]
pub struct AccelerationStructure(sys::AccelerationStructure);
impl AccelerationStructure { pub fn sizes(&self) -> AccelerationSizes { self.0.sizes() } }

pub struct Geometry {
    pub kind: sys::GeometryKind,
    pub buffer: Buffer,
    pub offset: u64,
    pub stride: u64,
    pub count: u32,
    pub opaque: bool,
    pub duplicate_any_hit: bool,
}
impl Geometry {
    fn native(&self) -> sys::Geometry { sys::Geometry { kind:self.kind,buffer:self.buffer.0.clone(),offset:self.offset,stride:self.stride,count:self.count,opaque:self.opaque,duplicate_any_hit:self.duplicate_any_hit } }
}
pub struct ColorAttachment { pub texture:Texture,pub resolve:Option<Texture>,pub load:i32,pub store:i32,pub clear:[f64;4] }
impl ColorAttachment {
    fn native(&self)->sys::ColorAttachment{sys::ColorAttachment{texture:self.texture.0.clone(),resolve:self.resolve.as_ref().map(|t|t.0.clone()),load:self.load,store:self.store,clear:self.clear}}
}
pub struct DepthAttachment{pub texture:Texture,pub load:i32,pub store:i32,pub clear:f64}
impl DepthAttachment{fn native(&self)->sys::DepthAttachment{sys::DepthAttachment{texture:self.texture.0.clone(),load:self.load,store:self.store,clear:self.clear}}}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn gpu_compacted_size_widths_preserve_adjacent_bytes() {
        use ogpu_core::validation::{admit_compacted_size_write, CompactedSizeType};
        let device = Device::create(true, true).expect("fatal-validation Vulkan device");
        let queue = device.queue().unwrap();
        let mut vertices = device.buffer(36, BufferKind::Shared).unwrap();
        let mut bytes = Vec::new();
        for value in [-1f32, -1., 0., 1., -1., 0., 0., 1., 0.] { bytes.extend_from_slice(&value.to_le_bytes()); }
        vertices.write(0, &bytes).unwrap();
        let accel = device.acceleration_blas(&[Geometry {
            kind: GeometryKind::Triangles, buffer: vertices, offset: 0, stride: 12,
            count: 3, opaque: true, duplicate_any_hit: false,
        }], false).unwrap();
        let scratch = device.buffer(accel.sizes().build_scratch_bytes, BufferKind::DeviceLocal).unwrap();
        let mut commands = queue.begin().unwrap();
        commands.build_acceleration(&accel, &scratch, 0).unwrap();
        queue.wait(commands.commit().unwrap()).unwrap();
        let mut output = device.buffer(24, BufferKind::Shared).unwrap();
        for (size_type, offset) in [(CompactedSizeType::Uint32, 4), (CompactedSizeType::Uint64, 8)] {
            output.write(0, &[0xa5; 24]).unwrap();
            let admitted = admit_compacted_size_write("test", offset, 24, size_type).unwrap();
            let mut commands = queue.begin().unwrap();
            commands.write_compacted_size(&accel, &output, admitted).unwrap();
            queue.wait(commands.commit().unwrap()).unwrap();
            let mut result = [0; 24];
            output.read(0, &mut result).unwrap();
            let start = admitted.offset() as usize;
            let end = start + admitted.bytes() as usize;
            assert!(result[..start].iter().chain(&result[end..]).all(|&byte| byte == 0xa5));
            let mut value = [0; 8];
            value[..end-start].copy_from_slice(&result[start..end]);
            assert!(u64::from_le_bytes(value) > 0 && u64::from_le_bytes(value) <= accel.sizes().structure_bytes);
        }
        assert_eq!(Device::validation_messages(), 0);
    }

    #[test]
    fn gpu_texture_constructor_consumes_core_admission() {
        use ogpu_core::{types::{StorageMode, TextureFormat, TextureKind, TextureUsage}, validation::{admit_texture, TextureProfile}};
        let device = Device::create(true, true).expect("fatal-validation Vulkan device");
        let caps = device.facts().unwrap().capabilities();
        let mut profile = TextureProfile::descriptor_2d(TextureFormat::Rgba8Unorm, 1, 1,
            StorageMode::Private, vec![TextureUsage::RenderAttachment], None);
        for (width, height, depth, mips, samples) in [(0,1,1,1,1),(1,1,1,2,1),(1,1,1,1,3)] {
            let mut invalid = profile.clone();
            invalid.descriptor.width=width; invalid.descriptor.height=height; invalid.descriptor.depth=depth;
            invalid.descriptor.mip_levels=mips; invalid.descriptor.sample_count=samples;
            assert!(admit_texture(&caps, invalid).is_err());
        }
        let mut wrong_caps=caps; wrong_caps.limits.max_texture_dimension_3d+=1;
        let wrong=admit_texture(&wrong_caps, profile.clone()).unwrap();
        assert!(matches!(device.texture(&wrong),Err(Error {kind:ErrorKind::WrongDevice,..})));
        let admitted=admit_texture(&caps,profile.clone()).unwrap();
        let texture=device.texture(&admitted).expect("same-device valid retry");
        assert_eq!(texture.descriptor(),TextureDescriptor {width:1,height:1,depth:1,mip_levels:1,samples:1,
            format:1,storage:false,attachment:true});
        profile.kind=TextureKind::TextureCube;
        let cube=admit_texture(&caps,profile.clone()).unwrap();
        assert!(matches!(device.texture(&cube),Err(Error {kind:ErrorKind::Unsupported(_),..})));
        profile.kind=TextureKind::Texture3d;
        let volume=admit_texture(&caps,profile).unwrap();
        device.texture(&volume).expect("one-slice 3D texture preserves admitted kind");
        let survivor=texture.clone();
        drop(texture);
        drop(device);
        // The last texture clone owns device lifetime through native destruction.
        drop(survivor);
        assert_eq!(Device::validation_messages(),0);
    }

    #[test]
    fn missing_vulkan_adapter_features_have_the_no_adapter_kind() {
        for failure in [sys::Error::NoDevice, sys::Error::MissingFeature, sys::Error::VulkanResult(-8)] {
            assert_eq!(error("device_create", failure).kind, ErrorKind::NoAdapter);
        }
    }
}
