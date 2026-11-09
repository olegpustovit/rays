use std::any::Any;

use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::Arc;

pub use ogpu_core::error::{Error, ErrorKind};
pub use ogpu_core::types::TextureFormat as Format;
pub(crate) use ogpu_core::error::make as error;

pub(crate) fn sys_error(operation: &str, value: metal_sys::Error) -> Error {
    match value {
        metal_sys::Error::Unavailable => error(operation, ErrorKind::Unsupported, "Metal is unavailable on this platform or OS version"),
        metal_sys::Error::WrongThread => error(operation, ErrorKind::WrongDomain, "Metal handles must be released on the platform main thread"),
        metal_sys::Error::InvalidArgument(message) => error(operation, ErrorKind::InvalidArgument, &message),
        metal_sys::Error::InvalidHandle => error(operation, ErrorKind::InvalidState, "invalid native Metal handle"),
        metal_sys::Error::Native(message) => error(operation, ErrorKind::DeviceLost, &message),
    }
}
pub(crate) fn enter_main(operation: &str) -> Result<(), Error> {
    metal_sys::drain_releases().map_err(|failure| sys_error(operation, failure))?;
    Ok(())
}

pub(crate) fn validate_text(operation: &str, value: &str) -> Result<(), Error> {
    if value.as_bytes().contains(&0) { Err(error(operation, ErrorKind::InvalidArgument, "string contains a NUL byte")) } else { Ok(()) }
}

pub(crate) struct Lifetime { pub(crate) destroyed: AtomicBool, pub(crate) dependents: Arc<AtomicUsize> }


impl Lifetime {
    pub(crate) fn new() -> Self { Self { destroyed: AtomicBool::new(false), dependents: Arc::new(AtomicUsize::new(0)) } }
    pub(crate) fn ensure_live(&self, operation: &str) -> Result<(), Error> {
        if self.destroyed.load(Ordering::Acquire) { Err(error(operation, ErrorKind::StaleHandle, "handle is destroyed")) } else { Ok(()) }
    }
    pub(crate) fn destroy_parent(&self, operation: &str) -> Result<(), Error> {
        if self.destroyed.load(Ordering::Acquire) { return Ok(()); }
        let dependents = self.dependents.load(Ordering::Acquire);
        if dependents != 0 { return Err(error(operation, ErrorKind::InvalidState, &format!("{dependents} Metal objects still depend on this handle"))); }
        self.destroyed.store(true, Ordering::Release);
        Ok(())
    }
    pub(crate) fn attach(&self) { self.dependents.fetch_add(1, Ordering::AcqRel); }
    pub(crate) fn detach(&self) { self.dependents.fetch_sub(1, Ordering::AcqRel); }
    pub(crate) fn dependent_counter(&self) -> Arc<AtomicUsize> { self.dependents.clone() }
}

pub(crate) struct DeviceInner { raw: metal_sys::Handle, lifetime: Lifetime, registry_id: u64 }
impl Drop for DeviceInner { fn drop(&mut self) { self.lifetime.destroyed.store(true, Ordering::Release); } }
#[derive(Clone)]
pub struct Device(pub(crate) Arc<DeviceInner>);

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DeviceFamily { Apple1, Apple2, Apple3, Apple4, Apple5, Apple6, Apple7, Apple8, Apple9, Apple10, Mac2, Common1, Common2, Common3, Metal3, Metal4 }
#[derive(Clone, Debug, PartialEq)]
pub struct DeviceInfo {
    pub name: String, pub registry_id: u64, pub low_power: bool, pub removable: bool, pub headless: bool,
    pub unified_memory: bool, pub recommended_max_working_set_size: u64, pub current_allocated_size: u64,
    pub max_buffer_length: u64, pub max_threadgroup_memory_length: u64, pub raytracing: bool,
    pub raytracing_from_render: bool, pub dynamic_libraries: bool,
}

impl Device {
    pub fn system_default() -> Result<Self, Error> {
        enter_main("Metal.Device.system_default")?;
        let raw = metal_sys::native::default_device().map_err(|failure| match failure {
            metal_sys::Error::Unavailable => error("Metal.Device.system_default", ErrorKind::NoAdapter, "no Metal adapter is available"),
            metal_sys::Error::Native(message) => error("Metal.Device.system_default", ErrorKind::NoAdapter, &message),
            other => sys_error("Metal.Device.system_default", other),
        })?;
        let registry_id = metal_sys::registry::device_registry_id(&raw).map_err(|failure| sys_error("Metal.Device.registry_id", failure))?;
        Ok(Self(Arc::new(DeviceInner { raw, lifetime: Lifetime::new(), registry_id })))
    }
    pub fn same(&self, other: &Self) -> bool { self.0.registry_id == other.0.registry_id }
    pub fn info(&self) -> Result<DeviceInfo, Error> {
        let operation = "Metal.Device.info";
        self.0.lifetime.ensure_live(operation)?;
        let raw = &self.0.raw;
        let name = metal_sys::registry::device_name(raw).map_err(|failure| sys_error(operation, failure))?;
        Ok(DeviceInfo {
            name: if name.is_empty() { "Unnamed Metal device".to_owned() } else { name },
            registry_id: self.0.registry_id,
            low_power: metal_sys::registry::device_is_low_power(raw).map_err(|failure| sys_error(operation, failure))?,
            removable: metal_sys::registry::device_is_removable(raw).map_err(|failure| sys_error(operation, failure))?,
            headless: metal_sys::registry::device_is_headless(raw).map_err(|failure| sys_error(operation, failure))?,
            unified_memory: metal_sys::registry::device_has_unified_memory(raw).map_err(|failure| sys_error(operation, failure))?,
            recommended_max_working_set_size: metal_sys::registry::device_recommended_max_working_set_size(raw).map_err(|failure| sys_error(operation, failure))?,
            current_allocated_size: metal_sys::registry::device_current_allocated_size(raw).map_err(|failure| sys_error(operation, failure))?,
            max_buffer_length: metal_sys::registry::device_max_buffer_length(raw).map_err(|failure| sys_error(operation, failure))?,
            max_threadgroup_memory_length: metal_sys::registry::device_max_threadgroup_memory_length(raw).map_err(|failure| sys_error(operation, failure))?,
            raytracing: metal_sys::registry::device_supports_raytracing(raw).map_err(|failure| sys_error(operation, failure))?,
            raytracing_from_render: metal_sys::registry::device_supports_raytracing_from_render(raw).map_err(|failure| sys_error(operation, failure))?,
            dynamic_libraries: metal_sys::registry::device_supports_dynamic_libraries(raw).map_err(|failure| sys_error(operation, failure))?,
        })
    }
    pub fn supports_family(&self, family: DeviceFamily) -> Result<bool, Error> {
        let operation = "Metal.Device.supports_family";
        self.0.lifetime.ensure_live(operation)?;
        let code = match family { DeviceFamily::Apple1 => 1001, DeviceFamily::Apple2 => 1002, DeviceFamily::Apple3 => 1003, DeviceFamily::Apple4 => 1004, DeviceFamily::Apple5 => 1005, DeviceFamily::Apple6 => 1006, DeviceFamily::Apple7 => 1007, DeviceFamily::Apple8 => 1008, DeviceFamily::Apple9 => 1009, DeviceFamily::Apple10 => 1010, DeviceFamily::Mac2 => 2002, DeviceFamily::Common1 => 3001, DeviceFamily::Common2 => 3002, DeviceFamily::Common3 => 3003, DeviceFamily::Metal3 => 5001, DeviceFamily::Metal4 => 5002 };
        metal_sys::registry::device_supports_family(&self.0.raw, code).map_err(|failure| sys_error(operation, failure))
    }
    pub fn supports_texture_sample_count(&self, sample_count: usize) -> Result<bool, Error> {
        let operation = "Metal.Device.supports_texture_sample_count";
        self.0.lifetime.ensure_live(operation)?;
        if sample_count == 0 { return Err(error(operation, ErrorKind::InvalidArgument, "texture sample count must be positive")); }
        metal_sys::registry::device_supports_texture_sample_count(&self.0.raw, sample_count as u64).map_err(|failure| sys_error(operation, failure))
    }
    pub(crate) fn supports_sampler_reduction(&self) -> Result<bool, Error> {
        let operation = "Metal.Device.supports_sampler_reduction";
        self.0.lifetime.ensure_live(operation)?;
        metal_sys::native::supports_sampler_reduction(&self.0.raw).map_err(|failure| sys_error(operation, failure))
    }
    pub fn destroy(&self) -> Result<(), Error> { self.0.lifetime.destroy_parent("Metal.Device.destroy") }
    pub fn is_destroyed(&self) -> bool { self.0.lifetime.destroyed.load(Ordering::Acquire) }
    pub(crate) fn raw(&self) -> &metal_sys::Handle { &self.0.raw }
    pub(crate) fn lifetime(&self) -> &Lifetime { &self.0.lifetime }
    pub(crate) fn registry_id(&self) -> u64 { self.0.registry_id }
    pub(crate) fn ensure_live(&self, operation: &str) -> Result<(), Error> { self.0.lifetime.ensure_live(operation) }
    pub(crate) fn attach_child(&self, operation: &str) -> Result<(), Error> {
        self.0.lifetime.ensure_live(operation)?;
        self.0.lifetime.attach();
        Ok(())
    }
    pub(crate) fn detach_child(&self) { self.0.lifetime.detach(); }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum StorageMode { Shared, Managed, Private, Memoryless }
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum CpuCacheMode { DefaultCache, WriteCombined }
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum HazardTrackingMode { Default, Untracked, Tracked }

struct BufferInner { raw: metal_sys::Handle, lifetime: Lifetime, device: Device, length: u64, storage: StorageMode, device_attached: AtomicBool }
impl Drop for BufferInner {
    fn drop(&mut self) {
        self.lifetime.destroyed.store(true, Ordering::Release);
        if self.device_attached.swap(false, Ordering::AcqRel) { self.device.0.lifetime.dependents.fetch_sub(1, Ordering::AcqRel); }
    }
}
#[derive(Clone)]
pub struct Buffer(Arc<BufferInner>);
pub struct MappedBuffer(metal_sys::native::MappedBuffer);
impl MappedBuffer {
    pub fn len(&self) -> usize { self.0.len() }
    pub fn is_empty(&self) -> bool { self.0.is_empty() }
    pub fn as_slice(&self) -> &[f32] { self.0.as_slice() }
    pub fn as_mut_slice(&mut self) -> &mut [f32] { self.0.as_mut_slice() }
}
fn storage_code(storage: StorageMode) -> i64 { match storage { StorageMode::Shared => 0, StorageMode::Managed => 1, StorageMode::Private => 2, StorageMode::Memoryless => 3 } }
fn cache_code(cache: CpuCacheMode) -> i64 { match cache { CpuCacheMode::DefaultCache => 0, CpuCacheMode::WriteCombined => 1 } }
fn verify_buffer(operation: &str, raw: &metal_sys::Handle, length: u64, storage: StorageMode, cache: CpuCacheMode) -> Result<(), Error> {
    let actual = (
        metal_sys::registry::buffer_info_length(raw).map_err(|failure| sys_error(operation, failure))?,
        metal_sys::registry::buffer_info_storageMode(raw).map_err(|failure| sys_error(operation, failure))?,
        metal_sys::registry::buffer_info_cpuCacheMode(raw).map_err(|failure| sys_error(operation, failure))?,
        metal_sys::registry::buffer_info_hazardTrackingMode(raw).map_err(|failure| sys_error(operation, failure))?,
    );
    if actual != (length, storage_code(storage), cache_code(cache), 2) {
        Err(error(operation, ErrorKind::DeviceLost, "Metal changed checked buffer properties during creation"))
    } else { Ok(()) }
}
fn options(storage: StorageMode, cache: CpuCacheMode, hazard: HazardTrackingMode) -> i64 {
    let hazard = match hazard { HazardTrackingMode::Default | HazardTrackingMode::Tracked => 2, HazardTrackingMode::Untracked => 1 };
    cache_code(cache) | (storage_code(storage) << 4) | (hazard << 8)
}
impl Buffer {
    pub fn create(device: &Device, length: u64, storage: StorageMode, cache: CpuCacheMode, label: Option<&str>) -> Result<Self, Error> {
        let operation = "Metal.Buffer.create";
        enter_main(operation)?;
        device.0.lifetime.ensure_live(operation)?;
        if length == 0 { return Err(error(operation, ErrorKind::InvalidArgument, "buffer length must be positive")); }
        if storage == StorageMode::Memoryless { return Err(error(operation, ErrorKind::InvalidArgument, "a buffer cannot be memoryless")); }
        if let Some(label) = label { validate_text(operation, label)?; }
        if length > metal_sys::registry::device_max_buffer_length(device.raw()).map_err(|failure| sys_error(operation, failure))? { return Err(error(operation, ErrorKind::InvalidArgument, "buffer length exceeds the device limit")); }
        let raw = metal_sys::registry::buffer_create(device.raw(), length, options(storage, cache, HazardTrackingMode::Default)).map_err(|failure| sys_error(operation, failure))?;
        verify_buffer(operation, &raw, length, storage, cache)?;
        if let Some(label) = label { metal_sys::registry::set_buffer_label(&raw, label).map_err(|failure| sys_error(operation, failure))?; }
        device.0.lifetime.dependents.fetch_add(1, Ordering::AcqRel);
        Ok(Self(Arc::new(BufferInner { raw, lifetime: Lifetime::new(), device: device.clone(), length, storage, device_attached: AtomicBool::new(true) })))
    }
    pub fn create_mapped(device: &Device, length: u64, label: Option<&str>) -> Result<(Self, MappedBuffer), Error> {
        let operation = "Metal.Buffer.create_mapped";
        enter_main(operation)?;
        device.0.lifetime.ensure_live(operation)?;
        if length == 0 || length % 4 != 0 { return Err(error(operation, ErrorKind::InvalidArgument, "mapped buffer length must be a positive multiple of four")); }
        if let Some(label) = label { validate_text(operation, label)?; }
        if length > metal_sys::registry::device_max_buffer_length(device.raw()).map_err(|failure| sys_error(operation, failure))? { return Err(error(operation, ErrorKind::InvalidArgument, "buffer length exceeds the device limit")); }
        let (raw, mapping) = metal_sys::native::create_mapped_buffer(device.raw(), length, options(StorageMode::Shared, CpuCacheMode::DefaultCache, HazardTrackingMode::Default)).map_err(|failure| sys_error(operation, failure))?;
        let actual_length = metal_sys::registry::buffer_info_length(&raw).map_err(|failure| sys_error(operation, failure))?;
        if actual_length < length { return Err(error(operation, ErrorKind::DeviceLost, "mapped buffer is shorter than its requested view")); }
        verify_buffer(operation, &raw, actual_length, StorageMode::Shared, CpuCacheMode::DefaultCache)?;
        if let Some(label) = label { metal_sys::registry::set_buffer_label(&raw, label).map_err(|failure| sys_error(operation, failure))?; }
        device.0.lifetime.dependents.fetch_add(1, Ordering::AcqRel);
        Ok((Self(Arc::new(BufferInner { raw, lifetime: Lifetime::new(), device: device.clone(), length: actual_length, storage: StorageMode::Shared, device_attached: AtomicBool::new(true) })), MappedBuffer(mapping)))
    }
    pub fn length(&self) -> u64 { self.0.length }
    pub fn storage_mode(&self) -> StorageMode { self.0.storage }
    pub fn write_bytes(&self, offset: u64, bytes: &[u8]) -> Result<(), Error> {
        let operation = "Metal.Buffer.write_bytes";
        metal_sys::native::write_buffer(&self.0.raw, offset, bytes).map_err(|failure| sys_error(operation, failure))
    }
    pub fn read_bytes(&self, offset: u64, length: usize) -> Result<Vec<u8>, Error> {
        let operation = "Metal.Buffer.read_bytes";
        metal_sys::native::read_buffer(&self.0.raw, offset, length).map_err(|failure| sys_error(operation, failure))
    }
    /// Captures the allocation, not its bytes. The caller must finish GPU writes before reading.
    /// The resulting read-only view may outlive this wrapper and never enters the main-thread API.
    pub fn owned_readback(&self) -> Result<std::sync::Arc<dyn Fn(i64, usize) -> Result<Vec<u8>, Error> + Send + Sync>, Error> {
        let operation = "Metal.Buffer.owned_readback";
        enter_main(operation)?;
        self.0.lifetime.ensure_live(operation)?;
        if self.0.storage != StorageMode::Shared {
            return Err(error(operation, ErrorKind::Unsupported, "readback requires shared buffer storage"));
        }
        let readback = metal_sys::native::shared_buffer_readback(&self.0.raw).map_err(|failure| sys_error(operation, failure))?;
        let allocation_length = self.0.length;
        Ok(Arc::new(move |offset, length| {
            let offset = u64::try_from(offset).map_err(|_| error(operation, ErrorKind::InvalidArgument, "negative read offset"))?;
            validate_range(operation, allocation_length, offset, length)?;
            readback.read(offset, length).map_err(|failure| sys_error(operation, failure))
        }))
    }
    pub fn destroy(&self) -> Result<(), Error> {
        self.0.lifetime.destroy_parent("Metal.Buffer.destroy")?;
        if self.0.device_attached.swap(false, Ordering::AcqRel) { self.0.device.0.lifetime.dependents.fetch_sub(1, Ordering::AcqRel); }
        Ok(())
    }
    pub fn is_destroyed(&self) -> bool { self.0.lifetime.destroyed.load(Ordering::Acquire) }
    pub(crate) fn raw(&self) -> &metal_sys::Handle { &self.0.raw }
    pub(crate) fn device(&self) -> &Device { &self.0.device }
    pub(crate) fn lifetime(&self) -> &Lifetime { &self.0.lifetime }
}
fn validate_range(operation: &str, total: u64, offset: u64, length: usize) -> Result<(), Error> {
    let length = u64::try_from(length).map_err(|_| error(operation, ErrorKind::InvalidArgument, "range length exceeds 64-bit bounds"))?;
    if offset > total || length > total - offset { Err(error(operation, ErrorKind::InvalidArgument, "range exceeds the buffer")) } else { Ok(()) }
}

pub mod release_queue {
    use super::{sys_error, Error};
    #[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
    pub struct Stats { pub pending: usize, pub live_handles: usize, pub total_created: u64, pub total_released: u64, pub external_deallocations: u64, pub external_deallocation_mismatches: u64, pub resident_bytes: u64 }
    pub fn drain() -> Result<usize, Error> { metal_sys::drain_releases().map_err(|failure| sys_error("Metal.Release_queue.drain", failure)) }
    pub fn stats() -> Result<Stats, Error> {
        let stats = metal_sys::handle_stats();
        Ok(Stats { pending: stats.pending, live_handles: stats.live_handles, total_created: stats.total_created, total_released: stats.total_released, external_deallocations: 0, external_deallocation_mismatches: 0, resident_bytes: 0 })
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TextureKind { Texture1d, Texture1dArray, Texture2d, Texture2dArray, Texture2dMultisample, TextureCube, TextureCubeArray, Texture3d, Texture2dMultisampleArray, TextureBuffer }
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum TextureUsage { ShaderRead, ShaderWrite, RenderTarget, PixelFormatView, ShaderAtomic }
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum CompressionType { Lossless, Lossy }
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum SwizzleChannel { Zero, One, Red, Green, Blue, Alpha }
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Swizzle { pub red: SwizzleChannel, pub green: SwizzleChannel, pub blue: SwizzleChannel, pub alpha: SwizzleChannel }
impl Swizzle { pub const IDENTITY: Self = Self { red: SwizzleChannel::Red, green: SwizzleChannel::Green, blue: SwizzleChannel::Blue, alpha: SwizzleChannel::Alpha }; }

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TextureDescriptor {
    pub kind: TextureKind,
    pub format: ogpu_core::types::TextureFormat,
    pub width: usize,
    pub height: usize,
    pub depth: usize,
    pub mip_levels: usize,
    pub sample_count: usize,
    pub array_length: usize,
    pub storage: StorageMode,
    pub cpu_cache: CpuCacheMode,
    pub hazard_tracking: HazardTrackingMode,
    pub usage: Vec<TextureUsage>,
    pub allow_gpu_optimized_contents: bool,
    pub compression: CompressionType,
    pub swizzle: Swizzle,
    pub label: Option<String>,
}

impl TextureDescriptor {
    pub fn descriptor_2d(format: ogpu_core::types::TextureFormat, width: usize, height: usize, storage: StorageMode, usage: Vec<TextureUsage>, label: Option<String>) -> Self {
        Self { kind: TextureKind::Texture2d, format, width, height, depth: 1, mip_levels: 1, sample_count: 1, array_length: 1, storage, cpu_cache: CpuCacheMode::DefaultCache, hazard_tracking: HazardTrackingMode::Default, usage, allow_gpu_optimized_contents: true, compression: CompressionType::Lossless, swizzle: Swizzle::IDENTITY, label }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct TextureRegion { pub x: usize, pub y: usize, pub z: usize, pub width: usize, pub height: usize, pub depth: usize }

pub(crate) struct TextureInner {
    pub(crate) raw: metal_sys::Handle,
    pub(crate) lifetime: Lifetime,
    pub(crate) device: Device,
    pub(crate) descriptor: TextureDescriptor,
    device_attached: AtomicBool,
    drawable_dependents: Option<Arc<AtomicUsize>>,
    drawable_attached: AtomicBool,
    _parent_keepalive: Option<Arc<dyn Any>>,
}
impl Drop for TextureInner {
    fn drop(&mut self) {
        self.lifetime.destroyed.store(true, Ordering::Release);
        if self.device_attached.swap(false, Ordering::AcqRel) { self.device.lifetime().detach(); }
        if self.drawable_attached.swap(false, Ordering::AcqRel) {
            if let Some(dependents) = &self.drawable_dependents { dependents.fetch_sub(1, Ordering::AcqRel); }
        }
    }
}
#[derive(Clone)]
pub struct Texture(Arc<TextureInner>);

fn texture_kind_code(kind: TextureKind) -> i32 {
    match kind { TextureKind::Texture1d => 0, TextureKind::Texture1dArray => 1, TextureKind::Texture2d => 2, TextureKind::Texture2dArray => 3, TextureKind::Texture2dMultisample => 4, TextureKind::TextureCube => 5, TextureKind::TextureCubeArray => 6, TextureKind::Texture3d => 7, TextureKind::Texture2dMultisampleArray => 8, TextureKind::TextureBuffer => 9 }
}
fn texture_usage_bit(usage: TextureUsage) -> i32 {
    match usage { TextureUsage::ShaderRead => 0x1, TextureUsage::ShaderWrite => 0x2, TextureUsage::RenderTarget => 0x4, TextureUsage::PixelFormatView => 0x10, TextureUsage::ShaderAtomic => 0x20 }
}
fn texture_swizzle_code(value: SwizzleChannel) -> i32 {
    match value { SwizzleChannel::Zero => 0, SwizzleChannel::One => 1, SwizzleChannel::Red => 2, SwizzleChannel::Green => 3, SwizzleChannel::Blue => 4, SwizzleChannel::Alpha => 5 }
}
fn texture_array(kind: TextureKind) -> bool { matches!(kind, TextureKind::Texture1dArray | TextureKind::Texture2dArray | TextureKind::TextureCubeArray | TextureKind::Texture2dMultisampleArray) }
fn texture_multisample(kind: TextureKind) -> bool { matches!(kind, TextureKind::Texture2dMultisample | TextureKind::Texture2dMultisampleArray) }
fn texture_slices(descriptor: &TextureDescriptor) -> Option<usize> {
    match descriptor.kind {
        TextureKind::Texture1dArray | TextureKind::Texture2dArray | TextureKind::Texture2dMultisampleArray => Some(descriptor.array_length),
        TextureKind::TextureCube => Some(6),
        TextureKind::TextureCubeArray => descriptor.array_length.checked_mul(6),
        _ => Some(1),
    }
}
fn texture_mip_size(size: usize, level: usize) -> usize { (size >> level.min(usize::BITS as usize - 1)).max(1) }
fn validate_texture_descriptor(device: &Device, descriptor: &TextureDescriptor) -> Result<(), Error> {
    let operation = "Metal.Texture.create";
    if descriptor.width == 0 || descriptor.height == 0 || descriptor.depth == 0 || descriptor.mip_levels == 0 || descriptor.sample_count == 0 || descriptor.array_length == 0 {
        return Err(error(operation, ErrorKind::InvalidArgument, "texture dimensions and counts must be positive"));
    }
    if (descriptor.kind == TextureKind::TextureBuffer && descriptor.width > 268_435_456) || (descriptor.kind != TextureKind::TextureBuffer && (descriptor.width > 16_384 || descriptor.height > 16_384 || descriptor.depth > 2_048)) {
        return Err(error(operation, ErrorKind::InvalidArgument, "texture dimensions exceed the binding's checked Metal limits"));
    }
    let max_dimension = descriptor.width.max(descriptor.height).max(descriptor.depth);
    let max_mips = (usize::BITS - max_dimension.leading_zeros()) as usize;
    if descriptor.mip_levels > max_mips { return Err(error(operation, ErrorKind::InvalidArgument, "texture mip count exceeds its dimensions")); }
    if descriptor.array_length >= 2_048 { return Err(error(operation, ErrorKind::InvalidArgument, "texture array length must be below 2048")); }
    if descriptor.usage.iter().copied().collect::<std::collections::HashSet<_>>().len() != descriptor.usage.len() { return Err(error(operation, ErrorKind::InvalidArgument, "texture usage contains duplicates")); }
    let writable = descriptor.usage.iter().any(|usage| matches!(usage, TextureUsage::ShaderWrite | TextureUsage::ShaderAtomic));
    if descriptor.swizzle != Swizzle::IDENTITY && writable { return Err(error(operation, ErrorKind::InvalidArgument, "texture swizzling is incompatible with shader-write and shader-atomic usage")); }
    if descriptor.storage == StorageMode::Memoryless && descriptor.usage.as_slice() != [TextureUsage::RenderTarget] { return Err(error(operation, ErrorKind::InvalidArgument, "a memoryless texture is a render target and nothing else")); }
    if descriptor.compression == CompressionType::Lossy { return Err(error(operation, ErrorKind::InvalidArgument, "pixel format does not support lossy texture compression")); }
    if let Some(label) = &descriptor.label { validate_text(operation, label)?; }
    match descriptor.kind {
        TextureKind::Texture1d | TextureKind::TextureBuffer if descriptor.height != 1 || descriptor.depth != 1 => return Err(error(operation, ErrorKind::InvalidArgument, "1D and buffer textures require height=depth=1")),
        TextureKind::Texture1dArray if descriptor.height != 1 || descriptor.depth != 1 => return Err(error(operation, ErrorKind::InvalidArgument, "1D-array textures require height=depth=1")),
        TextureKind::Texture2d | TextureKind::Texture2dArray | TextureKind::Texture2dMultisample | TextureKind::Texture2dMultisampleArray | TextureKind::TextureCube | TextureKind::TextureCubeArray if descriptor.depth != 1 => return Err(error(operation, ErrorKind::InvalidArgument, "2D, cube, and multisample textures require depth=1")),
        TextureKind::TextureCube | TextureKind::TextureCubeArray if descriptor.width != descriptor.height => return Err(error(operation, ErrorKind::InvalidArgument, "cube textures must be square")),
        TextureKind::Texture3d if descriptor.array_length != 1 => return Err(error(operation, ErrorKind::InvalidArgument, "3D textures cannot be arrays")),
        _ => {}
    }
    if texture_array(descriptor.kind) && descriptor.array_length < 2 { return Err(error(operation, ErrorKind::InvalidArgument, "array textures require at least two elements")); }
    if !texture_array(descriptor.kind) && descriptor.array_length != 1 { return Err(error(operation, ErrorKind::InvalidArgument, "non-array textures require array_length=1")); }
    if texture_multisample(descriptor.kind) && (descriptor.mip_levels != 1 || descriptor.sample_count == 1) { return Err(error(operation, ErrorKind::InvalidArgument, "multisample textures require one mip level and more than one sample")); }
    if !texture_multisample(descriptor.kind) && descriptor.sample_count != 1 { return Err(error(operation, ErrorKind::InvalidArgument, "non-multisample textures require sample_count=1")); }
    if descriptor.kind == TextureKind::TextureBuffer && descriptor.mip_levels != 1 { return Err(error(operation, ErrorKind::InvalidArgument, "buffer textures require exactly one mip level")); }
    if descriptor.sample_count != 1 && !device.supports_texture_sample_count(descriptor.sample_count)? { return Err(error(operation, ErrorKind::InvalidArgument, "device does not support the texture sample count")); }
    Ok(())
}

impl Texture {
    pub fn descriptor_2d(format: ogpu_core::types::TextureFormat, width: usize, height: usize, storage: StorageMode, usage: &[TextureUsage], label: Option<&str>) -> TextureDescriptor {
        TextureDescriptor::descriptor_2d(format, width, height, storage, usage.to_vec(), label.map(str::to_owned))
    }
    pub fn create(device: &Device, descriptor: TextureDescriptor) -> Result<Self, Error> {
        let operation = "Metal.Texture.create";
        enter_main(operation)?;
        device.0.lifetime.ensure_live(operation)?;
        validate_texture_descriptor(device, &descriptor)?;
        if descriptor.kind == TextureKind::TextureBuffer { return Err(error(operation, ErrorKind::InvalidArgument, "texture-buffer resources must be created from a buffer")); }
        let raw_descriptor = metal_sys::TextureDescriptor {
            texture_type: texture_kind_code(descriptor.kind), pixel_format: pixel_format_code(descriptor.format) as i32,
            width: descriptor.width as u64, height: descriptor.height as u64, depth: descriptor.depth as u64,
            mip_levels: descriptor.mip_levels as u64, sample_count: descriptor.sample_count as u64, array_length: descriptor.array_length as u64,
            storage_mode: match descriptor.storage { StorageMode::Shared => 0, StorageMode::Managed => 1, StorageMode::Private => 2, StorageMode::Memoryless => 3 },
            cpu_cache_mode: match descriptor.cpu_cache { CpuCacheMode::DefaultCache => 0, CpuCacheMode::WriteCombined => 1 },
            hazard_tracking_mode: match descriptor.hazard_tracking { HazardTrackingMode::Default => 2, HazardTrackingMode::Untracked => 1, HazardTrackingMode::Tracked => 2 },
            usage: descriptor.usage.iter().copied().fold(0, |bits, usage| bits | texture_usage_bit(usage)),
            allow_gpu_optimized_contents: u8::from(descriptor.allow_gpu_optimized_contents),
            compression_type: match descriptor.compression { CompressionType::Lossless => 0, CompressionType::Lossy => 1 },
            swizzle_red: texture_swizzle_code(descriptor.swizzle.red), swizzle_green: texture_swizzle_code(descriptor.swizzle.green),
            swizzle_blue: texture_swizzle_code(descriptor.swizzle.blue), swizzle_alpha: texture_swizzle_code(descriptor.swizzle.alpha),
        };
        if let Some(label) = descriptor.label.as_deref() { validate_text(operation, label)?; }
        let raw = metal_sys::native::create_texture(device.raw(), &raw_descriptor, descriptor.label.as_deref()).map_err(|failure| sys_error(operation, failure))?;
        let actual = metal_sys::native::texture_info(&raw).map_err(|failure| sys_error(operation, failure))?;
        let expected = [
            i64::from(texture_kind_code(descriptor.kind)),
            i64::from(pixel_format_code(descriptor.format)),
            descriptor.width as i64,
            descriptor.height as i64,
            descriptor.depth as i64,
            descriptor.mip_levels as i64,
            descriptor.sample_count as i64,
            descriptor.array_length as i64,
            descriptor.usage.iter().copied().fold(0, |bits, usage| bits | texture_usage_bit(usage)) as i64,
            storage_code(descriptor.storage),
            cache_code(descriptor.cpu_cache),
            match descriptor.hazard_tracking { HazardTrackingMode::Default | HazardTrackingMode::Tracked => 2, HazardTrackingMode::Untracked => 1 },
            i64::from(descriptor.allow_gpu_optimized_contents),
            match descriptor.compression { CompressionType::Lossless => 0, CompressionType::Lossy => 1 },
            i64::from(texture_swizzle_code(descriptor.swizzle.red)),
            i64::from(texture_swizzle_code(descriptor.swizzle.green)),
            i64::from(texture_swizzle_code(descriptor.swizzle.blue)),
            i64::from(texture_swizzle_code(descriptor.swizzle.alpha)),
        ];
        if actual != expected {
            const FIELDS: [&str; 18] = ["kind", "format", "width", "height", "depth", "mip_levels", "sample_count", "array_length", "usage", "storage", "cpu_cache", "hazard_tracking", "allow_gpu_optimized_contents", "compression", "swizzle_red", "swizzle_green", "swizzle_blue", "swizzle_alpha"];
            let differing = (0..FIELDS.len()).filter(|&index| actual[index] != expected[index])
                .map(|index| format!("{} expected {} got {}", FIELDS[index], expected[index], actual[index])).collect::<Vec<_>>().join("; ");
            return Err(error(operation, ErrorKind::DeviceLost, &format!("Metal changed checked texture properties during creation: {differing}")));
        }
        if metal_sys::registry::texture_is_shareable(&raw).map_err(|failure| sys_error(operation, failure))? {
            return Err(error(operation, ErrorKind::DeviceLost, "Metal changed the checked texture sharing mode during creation"));
        }
        device.attach_child(operation)?;
        // Metal reports a default hazard mode as tracked (both are checked as 2 above).
        let mut descriptor = descriptor;
        if descriptor.hazard_tracking == HazardTrackingMode::Default { descriptor.hazard_tracking = HazardTrackingMode::Tracked; }
        Ok(Self(Arc::new(TextureInner {
            raw,
            lifetime: Lifetime::new(),
            device: device.clone(),
            descriptor,
            device_attached: AtomicBool::new(true),
            drawable_dependents: None,
            drawable_attached: AtomicBool::new(false),
            _parent_keepalive: None,
        })))
    }

    pub(crate) fn from_drawable(
        device: Device,
        raw: metal_sys::Handle,
        width: u64,
        height: u64,
        format: ogpu_core::types::TextureFormat,
        drawable_dependents: Arc<AtomicUsize>,
        parent_keepalive: Arc<dyn Any>,
    ) -> Result<Self, Error> {
        let operation = "Metal.Drawable.texture";
        device.ensure_live(operation)?;
        let width = usize::try_from(width).map_err(|_| error(operation, ErrorKind::InvalidArgument, "drawable width exceeds platform bounds"))?;
        let height = usize::try_from(height).map_err(|_| error(operation, ErrorKind::InvalidArgument, "drawable height exceeds platform bounds"))?;
        let descriptor = TextureDescriptor::descriptor_2d(
            format, width, height, StorageMode::Private, vec![TextureUsage::RenderTarget], None,
        );
        drawable_dependents.fetch_add(1, Ordering::AcqRel);
        Ok(Self(Arc::new(TextureInner {
            raw,
            lifetime: Lifetime::new(),
            device,
            descriptor,
            device_attached: AtomicBool::new(false),
            drawable_dependents: Some(drawable_dependents),
            drawable_attached: AtomicBool::new(true),
            _parent_keepalive: Some(parent_keepalive),
        })))
    }
    pub fn device(&self) -> &Device { &self.0.device }
    pub fn descriptor(&self) -> &TextureDescriptor { &self.0.descriptor }
    pub fn is_destroyed(&self) -> bool { self.0.lifetime.destroyed.load(Ordering::Acquire) }
    pub fn read_bytes(&self, region: TextureRegion, mip_level: usize, slice: usize, bytes_per_row: usize, bytes_per_image: usize) -> Result<Vec<u8>, Error> {
        let operation = "Metal.Texture.read_bytes";
        self.0.lifetime.ensure_live(operation)?;
        let total = self.validate_transfer(operation, region, mip_level, slice, bytes_per_row, bytes_per_image)?;
        let destination = metal_sys::native::read_texture_bytes(&self.0.raw, total, bytes_per_row, bytes_per_image, (region.x as u64, region.y as u64, region.z as u64, region.width as u64, region.height as u64, region.depth as u64), mip_level as u64, slice as u64).map_err(|failure| sys_error(operation, failure))?;
        Ok(destination)
    }
    pub fn read_bytes_into(&self, region: TextureRegion, mip_level: usize, slice: usize, bytes_per_row: usize, bytes_per_image: usize, destination: &mut [u8]) -> Result<(), Error> {
        let operation = "Metal.Texture.read_bytes_into";
        self.0.lifetime.ensure_live(operation)?;
        let total = self.validate_transfer(operation, region, mip_level, slice, bytes_per_row, bytes_per_image)?;
        if destination.len() != total { return Err(error(operation, ErrorKind::InvalidArgument, "destination length does not match the complete pitched region")); }
        metal_sys::native::read_texture(&self.0.raw, destination, bytes_per_row, bytes_per_image, (region.x as u64, region.y as u64, region.z as u64, region.width as u64, region.height as u64, region.depth as u64), mip_level as u64, slice as u64).map_err(|failure| sys_error(operation, failure))
    }
    fn validate_transfer(&self, operation: &str, region: TextureRegion, mip_level: usize, slice: usize, bytes_per_row: usize, bytes_per_image: usize) -> Result<usize, Error> {
        let descriptor = &self.0.descriptor;
        if matches!(descriptor.storage, StorageMode::Private | StorageMode::Memoryless) { return Err(error(operation, ErrorKind::Unsupported, "private textures have no CPU transfer mapping")); }
        if texture_multisample(descriptor.kind) { return Err(error(operation, ErrorKind::Unsupported, "multisample textures do not support CPU transfer")); }
        if mip_level >= descriptor.mip_levels { return Err(error(operation, ErrorKind::InvalidArgument, "mip level is outside the texture")); }
        if slice >= texture_slices(descriptor).unwrap_or(usize::MAX) { return Err(error(operation, ErrorKind::InvalidArgument, "slice is outside the texture")); }
        if region.width == 0 || region.height == 0 || region.depth == 0 { return Err(error(operation, ErrorKind::InvalidArgument, "texture region dimensions must be positive")); }
        let width = texture_mip_size(descriptor.width, mip_level);
        let height = texture_mip_size(descriptor.height, mip_level);
        let depth = texture_mip_size(descriptor.depth, mip_level);
        if region.x > width || region.width > width-region.x || region.y > height || region.height > height-region.y || region.z > depth || region.depth > depth-region.z { return Err(error(operation, ErrorKind::InvalidArgument, "texture region exceeds the selected mip level")); }
        let texel_bytes = ogpu_core::types::texel_bytes(descriptor.format) as usize;
        let min_row = region.width.checked_mul(texel_bytes).ok_or_else(|| error(operation, ErrorKind::InvalidArgument, "texture row size overflow"))?;
        if bytes_per_row < min_row || bytes_per_row % texel_bytes != 0 { return Err(error(operation, ErrorKind::InvalidArgument, "texture row pitch is too small or not block-aligned")); }
        let min_image = bytes_per_row.checked_mul(region.height).ok_or_else(|| error(operation, ErrorKind::InvalidArgument, "texture image size overflow"))?;
        if bytes_per_image < min_image { return Err(error(operation, ErrorKind::InvalidArgument, "texture image pitch is smaller than its rows")); }
        bytes_per_image.checked_mul(region.depth).ok_or_else(|| error(operation, ErrorKind::InvalidArgument, "texture transfer size overflow"))
    }
    pub fn destroy(&self) -> Result<(), Error> {
        self.0.lifetime.destroy_parent("Metal.Texture.destroy")?;
        if self.0.device_attached.swap(false, Ordering::AcqRel) { self.0.device.lifetime().detach(); }
        if self.0.drawable_attached.swap(false, Ordering::AcqRel) {
            if let Some(dependents) = &self.0.drawable_dependents { dependents.fetch_sub(1, Ordering::AcqRel); }
        }
        Ok(())
    }
    pub(crate) fn raw(&self) -> &metal_sys::Handle { &self.0.raw }
    pub(crate) fn lifetime(&self) -> &Lifetime { &self.0.lifetime }
}
use std::cell::{Cell, RefCell};

struct CommandQueueInner { raw: metal_sys::Handle, lifetime: Lifetime, device: Device, device_attached: AtomicBool }
impl Drop for CommandQueueInner {
    fn drop(&mut self) {
        self.lifetime.destroyed.store(true, Ordering::Release);
        if self.device_attached.swap(false, Ordering::AcqRel) { self.device.0.lifetime.dependents.fetch_sub(1, Ordering::AcqRel); }
    }
}
#[derive(Clone)]
pub struct CommandQueue(Arc<CommandQueueInner>);
impl CommandQueue {
    pub fn create(device: &Device) -> Result<Self, Error> {
        let operation = "Metal.Command_queue.create";
        enter_main(operation)?;
        device.0.lifetime.ensure_live(operation)?;
        let raw = metal_sys::registry::command_queue_create(device.raw()).map_err(|failure| sys_error(operation, failure))?;
        device.0.lifetime.dependents.fetch_add(1, Ordering::AcqRel);
        Ok(Self(Arc::new(CommandQueueInner { raw, lifetime: Lifetime::new(), device: device.clone(), device_attached: AtomicBool::new(true) })))
    }
    pub fn destroy(&self) -> Result<(), Error> {
        self.0.lifetime.destroy_parent("Metal.Command_queue.destroy")?;
        if self.0.device_attached.swap(false, Ordering::AcqRel) { self.0.device.0.lifetime.dependents.fetch_sub(1, Ordering::AcqRel); }
        Ok(())
    }
    pub(crate) fn raw(&self) -> &metal_sys::Handle { &self.0.raw }
    pub(crate) fn device(&self) -> &Device { &self.0.device }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum CommandBufferStatus { NotEnqueued, Enqueued, Committed, Scheduled, Completed, Error, Unknown(i64) }
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum PresentTime { Immediate, AtTime(f64), AfterMinimumDuration(f64) }
#[derive(Clone, Debug, PartialEq)]
pub struct CommandBufferDiagnostics {
    pub error_options: u64,
    pub gpu_start_time: f64,
    pub gpu_end_time: f64,
    pub kernel_start_time: f64,
    pub kernel_end_time: f64,
    pub retained_references: bool,
}
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum CommandPhase { Recording, Submitted, Completed, Failed }

enum RetainedResource {
    Buffer(Buffer),
    Texture(Texture),
    ComputePipeline(ComputePipeline),
    RenderPipeline(crate::native::pipeline::RenderPipeline),
    Sampler(crate::native::descriptors::Sampler),
    DepthStencil(crate::native::descriptors::DepthStencil),
    AccelerationStructure(crate::native::acceleration::AccelerationStructure),
    IndirectCommandBuffer(crate::native::indirect::IndirectCommandBuffer),
    Drawable(crate::native::surface::Drawable),
}
impl RetainedResource {
    fn raw(&self) -> &metal_sys::Handle {
        match self {
            Self::Buffer(value) => value.raw(),
            Self::Texture(value) => value.raw(),
            Self::ComputePipeline(value) => value.raw(),
            Self::RenderPipeline(value) => value.raw(),
            Self::Sampler(value) => value.raw(),
            Self::DepthStencil(value) => value.raw(),
            Self::AccelerationStructure(value) => value.raw(),
            Self::IndirectCommandBuffer(value) => value.raw(),
            Self::Drawable(value) => value.raw(),
        }
    }
    fn dependents(&self) -> Arc<AtomicUsize> {
        match self {
            Self::Buffer(value) => value.lifetime().dependent_counter(),
            Self::Texture(value) => value.lifetime().dependent_counter(),
            Self::ComputePipeline(value) => value.lifetime().dependent_counter(),
            Self::RenderPipeline(value) => value.lifetime().dependent_counter(),
            Self::Sampler(value) => value.lifetime().dependent_counter(),
            Self::DepthStencil(value) => value.lifetime().dependent_counter(),
            Self::AccelerationStructure(value) => value.lifetime().dependent_counter(),
            Self::IndirectCommandBuffer(value) => value.lifetime().dependent_counter(),
            Self::Drawable(value) => value.dependent_counter(),
        }
    }
}
impl Drop for RetainedResource { fn drop(&mut self) { self.dependents().fetch_sub(1, Ordering::AcqRel); } }

struct CommandBufferInner {
    raw: metal_sys::Handle,
    lifetime: Lifetime,
    queue: CommandQueue,
    queue_attached: AtomicBool,
    phase: Cell<CommandPhase>,
    resources: RefCell<Vec<RetainedResource>>,
}
impl Drop for CommandBufferInner {
    fn drop(&mut self) {
        self.lifetime.destroyed.store(true, Ordering::Release);
        self.resources.get_mut().clear();
        if self.queue_attached.swap(false, Ordering::AcqRel) { self.queue.0.lifetime.dependents.fetch_sub(1, Ordering::AcqRel); }
    }
}
#[derive(Clone)]
pub struct CommandBuffer(Arc<CommandBufferInner>);

impl CommandBuffer {
    pub fn create(queue: &CommandQueue) -> Result<Self, Error> {
        let operation = "Metal.Command_buffer.create";
        enter_main(operation)?;
        queue.0.lifetime.ensure_live(operation)?;
        let raw = metal_sys::registry::command_buffer_create(queue.raw()).map_err(|failure| sys_error(operation, failure))?;
        queue.0.lifetime.dependents.fetch_add(1, Ordering::AcqRel);
        Ok(Self(Arc::new(CommandBufferInner { raw, lifetime: Lifetime::new(), queue: queue.clone(), queue_attached: AtomicBool::new(true), phase: Cell::new(CommandPhase::Recording), resources: RefCell::new(Vec::new()) })))
    }
    pub fn device(&self) -> &Device { self.0.queue.device() }
    pub(crate) fn ensure_recording(&self, operation: &str) -> Result<(), Error> {
        self.0.lifetime.ensure_live(operation)?;
        if self.0.phase.get() != CommandPhase::Recording {
            return Err(error(operation, ErrorKind::InvalidState, "command buffer is no longer recording"));
        }
        if self.0.lifetime.dependents.load(Ordering::Acquire) != 0 {
            return Err(error(operation, ErrorKind::InvalidState, "command buffer already has an open encoder"));
        }
        Ok(())
    }

    pub fn status(&self) -> Result<CommandBufferStatus, Error> {
        let operation = "Metal.Command_buffer.status";
        self.0.lifetime.ensure_live(operation)?;
        let status = metal_sys::registry::command_buffer_status(&self.0.raw).map_err(|failure| sys_error(operation, failure))?;
        let status = match status { 0 => CommandBufferStatus::NotEnqueued, 1 => CommandBufferStatus::Enqueued, 2 => CommandBufferStatus::Committed, 3 => CommandBufferStatus::Scheduled, 4 => CommandBufferStatus::Completed, 5 => CommandBufferStatus::Error, other => CommandBufferStatus::Unknown(other) };
        match status {
            CommandBufferStatus::Completed => { self.0.phase.set(CommandPhase::Completed); self.0.resources.borrow_mut().clear(); }
            CommandBufferStatus::Error => { self.0.phase.set(CommandPhase::Failed); self.0.resources.borrow_mut().clear(); }
            _ => {}
        }
        Ok(status)
    }
    pub fn diagnostics(&self) -> Result<CommandBufferDiagnostics, Error> {
        let operation = "Metal.Command_buffer.diagnostics";
        self.0.lifetime.ensure_live(operation)?;
        let value = metal_sys::native::presentation_command_snapshot(&self.0.raw).map_err(|failure| sys_error(operation, failure))?;
        Ok(CommandBufferDiagnostics { error_options: value.error_options, gpu_start_time: value.gpu_start_time, gpu_end_time: value.gpu_end_time, kernel_start_time: value.kernel_start_time, kernel_end_time: value.kernel_end_time, retained_references: value.retained_references != 0 })
    }
    pub fn commit(&self) -> Result<(), Error> {
        let operation = "Metal.Command_buffer.commit";
        enter_main(operation)?;
        self.0.lifetime.ensure_live(operation)?;
        self.ensure_recording(operation)?;
        metal_sys::registry::command_buffer_commit(&self.0.raw).map_err(|failure| sys_error(operation, failure))?;
        self.0.phase.set(CommandPhase::Submitted);
        Ok(())
    }
    pub fn wait_until_completed(&self) -> Result<(), Error> {
        let operation = "Metal.Command_buffer.wait_until_completed";
        self.0.lifetime.ensure_live(operation)?;
        let state = self.status()?;
        if matches!(state, CommandBufferStatus::NotEnqueued | CommandBufferStatus::Enqueued) { return Err(error(operation, ErrorKind::InvalidState, "command buffer has not been committed")); }
        metal_sys::native::wait_command_buffer(&self.0.raw).map_err(|failure| sys_error(operation, failure))?;
        self.0.resources.borrow_mut().clear();
        self.0.phase.set(CommandPhase::Completed);
        if let Some(message) = metal_sys::native::command_buffer_error(&self.0.raw).map_err(|failure| sys_error(operation, failure))? {
            self.0.phase.set(CommandPhase::Failed);
            return Err(error(operation, ErrorKind::DeviceLost, &message));
        }
        Ok(())
    }
    pub fn destroyed(&self) -> bool { self.0.lifetime.destroyed.load(Ordering::Acquire) }
    pub fn present(&self, drawable: &crate::native::surface::Drawable, time: PresentTime) -> Result<(), Error> {
        let operation = "Metal.Command_buffer.present";
        self.ensure_recording(operation)?;
        drawable.ensure_live(operation)?;
        if drawable.is_presentation_scheduled() { return Err(error(operation, ErrorKind::InvalidState, "drawable is already scheduled for presentation")); }
        if !self.device().same(drawable.device()) { return Err(error(operation, ErrorKind::CrossDevice, "drawable belongs to another Metal device")); }
        match time {
            PresentTime::Immediate => metal_sys::registry::command_buffer_present(&self.0.raw, drawable.raw()).map_err(|failure| sys_error(operation, failure)),
            PresentTime::AtTime(time) if time.is_finite() && time >= 0.0 => metal_sys::registry::command_buffer_present_at_time(&self.0.raw, drawable.raw(), time).map_err(|failure| sys_error(operation, failure)),
            PresentTime::AfterMinimumDuration(duration) if duration.is_finite() && duration >= 0.0 => metal_sys::registry::command_buffer_present_after_duration(&self.0.raw, drawable.raw(), duration).map_err(|failure| sys_error(operation, failure)),
            _ => Err(error(operation, ErrorKind::InvalidArgument, "presentation time must be finite and nonnegative")),
        }?;
        drawable.mark_presentation_scheduled();
        self.retain(RetainedResource::Drawable(drawable.clone()));
        Ok(())
    }
    pub fn destroy(&self) -> Result<(), Error> {
        self.0.lifetime.destroy_parent("Metal.Command_buffer.destroy")?;
        self.0.resources.borrow_mut().clear(); // a destroyed command buffer retains nothing (ogpu_metal abandons uncommitted work this way)
        if self.0.queue_attached.swap(false, Ordering::AcqRel) { self.0.queue.0.lifetime.dependents.fetch_sub(1, Ordering::AcqRel); }
        Ok(())
    }
    fn retain(&self, resource: RetainedResource) {
        resource.dependents().fetch_add(1, Ordering::AcqRel);
        let raw = resource.raw();
        let mut resources = self.0.resources.borrow_mut();
        if resources.iter().any(|held| held.raw().same(raw)) { drop(resource); }
        else { resources.push(resource); }
    }
    pub(crate) fn raw(&self) -> &metal_sys::Handle { &self.0.raw }
    pub(crate) fn lifetime(&self) -> &Lifetime { &self.0.lifetime }
    pub(crate) fn retain_buffer(&self, buffer: &Buffer) { self.retain(RetainedResource::Buffer(buffer.clone())); }
    pub(crate) fn retain_texture(&self, texture: &Texture) { self.retain(RetainedResource::Texture(texture.clone())); }
    pub(crate) fn retain_pipeline(&self, pipeline: &ComputePipeline) { self.retain(RetainedResource::ComputePipeline(pipeline.clone())); }
    pub(crate) fn retain_render_pipeline(&self, pipeline: &crate::native::pipeline::RenderPipeline) { self.retain(RetainedResource::RenderPipeline(pipeline.clone())); }
    pub(crate) fn retain_sampler(&self, sampler: &crate::native::descriptors::Sampler) { self.retain(RetainedResource::Sampler(sampler.clone())); }
    pub(crate) fn retain_depth_stencil(&self, state: &crate::native::descriptors::DepthStencil) { self.retain(RetainedResource::DepthStencil(state.clone())); }
    pub(crate) fn retain_acceleration(&self, structure: &crate::native::acceleration::AccelerationStructure) { self.retain(RetainedResource::AccelerationStructure(structure.clone())); }
    pub(crate) fn retain_indirect(&self, buffer: &crate::native::indirect::IndirectCommandBuffer) { self.retain(RetainedResource::IndirectCommandBuffer(buffer.clone())); }
}

struct ComputePipelineInner {
    raw: metal_sys::Handle,
    lifetime: Lifetime,
    device: Device,
    max_total_threads: usize,
    bindings: Option<Vec<crate::native::pipeline::ShaderBinding>>,
    device_attached: AtomicBool,
}
impl Drop for ComputePipelineInner {
    fn drop(&mut self) {
        self.lifetime.destroyed.store(true, Ordering::Release);
        if self.device_attached.swap(false, Ordering::AcqRel) { self.device.lifetime().detach(); }
    }
}
#[derive(Clone)]
pub struct ComputePipeline(Arc<ComputePipelineInner>);
impl ComputePipeline {
    pub fn create(function: &crate::native::library::Function, label: Option<&str>, reflection: bool) -> Result<Self, Error> {
        let operation = "Metal.Compute_pipeline.create";
        enter_main(operation)?;
        function.lifetime().ensure_live(operation)?;
        if let Some(label) = label { validate_text(operation, label)?; }
        let device = function.device();
        device.ensure_live(operation)?;
        let (raw, reflected) = metal_sys::native::pipeline::create_compute_pipeline(device.raw(), function.raw(), label, reflection).map_err(|failure| sys_error(operation, failure))?;
        let max_total_threads = usize::try_from(metal_sys::registry::compute_pipeline_max_total_threads(&raw).map_err(|failure| sys_error(operation, failure))?).map_err(|_| error(operation, ErrorKind::InvalidState, "Metal returned an out-of-range thread limit"))?;
        let bindings = reflection.then(|| reflected.into_iter().map(crate::native::pipeline::map_binding).collect());
        device.attach_child(operation)?;
        Ok(Self(Arc::new(ComputePipelineInner { raw, lifetime: Lifetime::new(), device: device.clone(), max_total_threads, bindings, device_attached: AtomicBool::new(true) })))
    }
    pub fn bindings(&self) -> Option<&[crate::native::pipeline::ShaderBinding]> { self.0.bindings.as_deref() }
    pub fn max_total_threads(&self) -> usize { self.0.max_total_threads }
    pub fn destroy(&self) -> Result<(), Error> {
        self.0.lifetime.destroy_parent("Metal.Compute_pipeline.destroy")?;
        if self.0.device_attached.swap(false, Ordering::AcqRel) { self.0.device.lifetime().detach(); }
        Ok(())
    }
    pub(crate) fn raw(&self) -> &metal_sys::Handle { &self.0.raw }
    pub(crate) fn lifetime(&self) -> &Lifetime { &self.0.lifetime }
    pub(crate) fn device(&self) -> &Device { &self.0.device }
}

struct ComputeEncoderInner { raw: metal_sys::Handle, lifetime: Lifetime, command_buffer: CommandBuffer, ended: AtomicBool }
impl Drop for ComputeEncoderInner {
    fn drop(&mut self) {
        if !self.ended.swap(true, Ordering::AcqRel) {
            let _ = metal_sys::registry::compute_encoder_end(&self.raw);
            self.command_buffer.0.lifetime.dependents.fetch_sub(1, Ordering::AcqRel);
        }
    }
}
#[derive(Clone)]
pub struct ComputeEncoder(Arc<ComputeEncoderInner>);
impl ComputeEncoder {
    pub fn create(command_buffer: &CommandBuffer) -> Result<Self, Error> {
        let operation = "Metal.Compute_encoder.create";
        enter_main(operation)?;
        command_buffer.ensure_recording(operation)?;
        let raw = metal_sys::registry::command_buffer_compute_encoder(command_buffer.raw()).map_err(|failure| sys_error(operation, failure))?;
        command_buffer.0.lifetime.dependents.fetch_add(1, Ordering::AcqRel);
        Ok(Self(Arc::new(ComputeEncoderInner { raw, lifetime: Lifetime::new(), command_buffer: command_buffer.clone(), ended: AtomicBool::new(false) })))
    }
    // Encoding arguments and state are validated by ogpu_core before driver entry.
    // Native resource retention below remains necessary through GPU completion.
    pub fn set_pipeline(&self, pipeline: &ComputePipeline) -> Result<(), Error> {
        let operation = "Metal.Compute_encoder.set_pipeline";
        metal_sys::registry::compute_encoder_set_pipeline(&self.0.raw, pipeline.raw()).map_err(|failure| sys_error(operation, failure))?;
        self.0.command_buffer.retain_pipeline(pipeline);
        Ok(())
    }
    pub fn set_buffer(&self, index: usize, offset: u64, buffer: &Buffer) -> Result<(), Error> {
        let operation = "Metal.Compute_encoder.set_buffer";
        metal_sys::registry::compute_encoder_set_buffer(&self.0.raw, buffer.raw(), offset, index).map_err(|failure| sys_error(operation, failure))?;
        self.0.command_buffer.retain_buffer(buffer);
        Ok(())
    }
    pub fn set_texture(&self, index: usize, texture: &Texture) -> Result<(), Error> {
        let operation = "Metal.Compute_encoder.set_texture";
        metal_sys::registry::compute_encoder_set_texture(&self.0.raw, texture.raw(), index).map_err(|failure| sys_error(operation, failure))?;
        self.0.command_buffer.retain_texture(texture);
        Ok(())
    }
    pub fn set_bytes(&self, index: usize, bytes: &[u8]) -> Result<(), Error> {
        metal_sys::native::set_compute_bytes(&self.0.raw, bytes, index as u64).map_err(|failure| sys_error("Metal.Compute_encoder.set_bytes", failure))
    }
    pub fn dispatch_threads(&self, threads: (usize, usize, usize), threadgroup: (usize, usize, usize)) -> Result<(), Error> {
        let make_size = |(width, height, depth)| metal_sys::registry::record::Mtl_size_int { width, height, depth };
        metal_sys::registry::compute_encoder_dispatch(&self.0.raw, make_size(threads), make_size(threadgroup)).map_err(|failure| sys_error("Metal.Compute_encoder.dispatch_threads", failure))
    }
    pub fn dispatch_threadgroups_indirect(&self, arguments: &Buffer, offset: u64, threadgroup: (usize, usize, usize)) -> Result<(), Error> {
        let size = metal_sys::registry::record::Mtl_size_int { width: threadgroup.0, height: threadgroup.1, depth: threadgroup.2 };
        metal_sys::registry::compute_encoder_dispatch_indirect(&self.0.raw, arguments.raw(), offset, size).map_err(|failure| sys_error("Metal.Compute_encoder.dispatch_threadgroups_indirect", failure))?;
        self.0.command_buffer.retain_buffer(arguments);
        Ok(())
    }
    pub fn end_encoding(&self) -> Result<(), Error> {
        metal_sys::registry::compute_encoder_end(&self.0.raw).map_err(|failure| sys_error("Metal.Compute_encoder.end_encoding", failure))?;
        if !self.0.ended.swap(true, Ordering::AcqRel) {
            self.0.lifetime.destroyed.store(true, Ordering::Release);
            self.0.command_buffer.0.lifetime.dependents.fetch_sub(1, Ordering::AcqRel);
        }
        Ok(())
    }
    pub fn set_acceleration_structure(&self, index: usize, structure: Option<&crate::native::acceleration::AccelerationStructure>) -> Result<(), Error> {
        metal_sys::registry::compute35_acceleration(&self.0.raw, structure.map(|a| a.raw()), index as u64).map_err(|failure| sys_error("Metal.Compute_encoder.set_acceleration_structure", failure))?;
        if let Some(a) = structure { self.0.command_buffer.retain_acceleration(a); }
        Ok(())
    }
    pub fn use_acceleration_structures(&self, structures: &[crate::native::acceleration::AccelerationStructure]) -> Result<(), Error> {
        let raws: Vec<metal_sys::Handle> = structures.iter().map(|a| a.raw().clone()).collect();
        metal_sys::native::use_accelerations(&self.0.raw, &raws).map_err(|failure| sys_error("Metal.Compute_encoder.use_acceleration_structures", failure))?;
        for a in structures { self.0.command_buffer.retain_acceleration(a); }
        Ok(())
    }
}

pub fn pixel_format_code(format: Format) -> u32 {
    match format { Format::R8Unorm => 10, Format::Rgba8Unorm => 70, Format::Bgra8Unorm => 80,
        Format::Bgra8UnormSrgb => 81, Format::Rgba16Float => 115, Format::Rgba32Float => 125, Format::Depth32Float => 252 }
}
