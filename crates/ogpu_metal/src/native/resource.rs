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
        metal_sys::Error::Unsupported(message) => error(operation, ErrorKind::Unsupported, &message),
        metal_sys::Error::InvalidArgument(message) => error(operation, ErrorKind::InvalidArgument, &message),
        metal_sys::Error::InvalidHandle => error(operation, ErrorKind::InvalidState, "invalid native Metal handle"),
        metal_sys::Error::Native(message) => error(operation, ErrorKind::DeviceLost, &message),
    }
}
pub(crate) fn enter_main(operation: &str) -> Result<(), Error> {
    metal_sys::drain_releases().map_err(|failure| sys_error(operation, failure))?;
    Ok(())
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

pub(crate) struct DeviceInner { raw: metal_sys::Handle, lifetime: Lifetime, registry_id: u64, capabilities: std::sync::OnceLock<ogpu_core::caps::Caps> }
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
        let raw = metal_sys::native::default_device().map_err(|failure| match failure {
            metal_sys::Error::Unavailable => error("Metal.Device.system_default", ErrorKind::NoAdapter, "no Metal adapter is available"),
            metal_sys::Error::Native(message) => error("Metal.Device.system_default", ErrorKind::NoAdapter, &message),
            other => sys_error("Metal.Device.system_default", other),
        })?;
        let registry_id = metal_sys::registry::device_registry_id(&raw).map_err(|failure| sys_error("Metal.Device.registry_id", failure))?;
        Ok(Self(Arc::new(DeviceInner { raw, lifetime: Lifetime::new(), registry_id, capabilities: std::sync::OnceLock::new() })))
    }
    pub fn capabilities(&self) -> Result<ogpu_core::caps::Caps, Error> {
        if let Some(caps) = self.0.capabilities.get() { return Ok(*caps); }
        let info = self.info()?;
        let mut sample_counts = 0;
        for count in [1usize, 2, 4, 8, 16, 32, 64] {
            if self.supports_texture_sample_count(count)? { sample_counts |= count as u32; }
        }
        let family = |f| self.supports_family(f);
        let large = family(DeviceFamily::Apple10)?;
        let curves = family(DeviceFamily::Apple9)?;
        let caps = ogpu_core::caps::Caps {
            limits: ogpu_core::caps::Limits { max_buffer_size: info.max_buffer_length as i64,
                // https://developer.apple.com/metal/Metal-Feature-Set-Tables.pdf
                max_texture_dimension_1d: if large { 32_768 } else { 16_384 },
                max_texture_dimension_2d: if large { 32_768 } else { 16_384 },
                max_texture_dimension_3d: 2_048,
                max_texture_dimension_cube: if large { 32_768 } else { 16_384 },
                max_texture_array_layers: 2_048, max_bind_groups: 4, sample_counts, color_sample_counts: sample_counts, depth_sample_counts: sample_counts },
            compute_pipeline: true, render_pipeline: true, render_requires_fragment: true, ray_tracing: info.raytracing,
            ray_tracing_curves: info.raytracing && curves, ray_tracing_motion: info.raytracing,
            ray_tracing_refit: true, memoryless_attachments: family(DeviceFamily::Apple7)?,
            unified_memory: info.unified_memory,
        };
        let _ = self.0.capabilities.set(caps);
        Ok(caps)
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

pub use ogpu_core::types::{StorageMode, CpuCacheMode, HazardTrackingMode};
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
    pub fn create(device: &Device, descriptor: &ogpu_core::validation::AdmittedBuffer) -> Result<Self, Error> {
        let operation = "Metal.Buffer.create";
        enter_main(operation)?;
        device.0.lifetime.ensure_live(operation)?;
        if !descriptor.admitted_for(&device.capabilities()?) {
            return Err(error(operation, ErrorKind::CrossDevice, "descriptor was admitted for different device capabilities"));
        }
        let length = descriptor.size as u64;
        let storage = descriptor.storage;
        let cache = descriptor.cpu_cache;
        let label = descriptor.label.as_deref();
        let raw = metal_sys::registry::buffer_create(device.raw(), length, options(storage, cache, HazardTrackingMode::Default)).map_err(|failure| sys_error(operation, failure))?;
        verify_buffer(operation, &raw, length, storage, cache)?;
        if let Some(label) = label { metal_sys::registry::set_buffer_label(&raw, label).map_err(|failure| sys_error(operation, failure))?; }
        device.0.lifetime.dependents.fetch_add(1, Ordering::AcqRel);
        Ok(Self(Arc::new(BufferInner { raw, lifetime: Lifetime::new(), device: device.clone(), length, storage, device_attached: AtomicBool::new(true) })))
    }
    pub fn create_mapped(device: &Device, descriptor: &ogpu_core::validation::AdmittedBuffer) -> Result<(Self, MappedBuffer), Error> {
        let operation = "Metal.Buffer.create_mapped";
        enter_main(operation)?;
        device.0.lifetime.ensure_live(operation)?;
        if !descriptor.admitted_for(&device.capabilities()?) {
            return Err(error(operation, ErrorKind::CrossDevice, "descriptor was admitted for different device capabilities"));
        }
        let length = descriptor.size as u64;
        let label = descriptor.label.as_deref();
        let (raw, mapping) = metal_sys::native::create_mapped_buffer(device.raw(), descriptor).map_err(|failure| sys_error(operation, failure))?;
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
        self.0.lifetime.ensure_live(operation)?;
        if self.0.storage != StorageMode::Shared {
            return Err(error(operation, ErrorKind::Unsupported, "readback requires shared buffer storage"));
        }
        let readback = metal_sys::native::shared_buffer_readback(&self.0.raw).map_err(|failure| sys_error(operation, failure))?;
        Ok(Arc::new(move |offset, length| {
            let offset = u64::try_from(offset).map_err(|_| error(operation, ErrorKind::InvalidArgument, "negative read offset"))?;
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
    pub(crate) fn lifetime(&self) -> &Lifetime { &self.0.lifetime }
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

pub use ogpu_core::types::{TextureKind, TextureUsage, CompressionType, Swizzle, SwizzleChannel};
pub use ogpu_core::validation::{TextureProfile as TextureDescriptor, TextureRegion, AdmittedTexture, TextureRead, TextureReadInto};

pub(crate) struct TextureInner {
    pub(crate) raw: metal_sys::Handle,
    pub(crate) lifetime: Lifetime,
    pub(crate) device: Device,
    pub(crate) descriptor: AdmittedTexture,
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

impl Texture {
    pub fn descriptor_2d(format: ogpu_core::types::TextureFormat, width: usize, height: usize, storage: StorageMode, usage: &[TextureUsage], label: Option<&str>) -> TextureDescriptor {
        TextureDescriptor::descriptor_2d(format, width, height, storage, usage.to_vec(), label.map(str::to_owned))
    }
    pub fn create(device: &Device, descriptor: AdmittedTexture) -> Result<Self, Error> {
        let operation = "Metal.Texture.create";
        enter_main(operation)?;
        device.0.lifetime.ensure_live(operation)?;
        if !descriptor.admitted_for(&device.capabilities()?) {
            return Err(error(operation, ErrorKind::CrossDevice, "descriptor was admitted for different device capabilities"));
        }
        let raw_descriptor = metal_sys::native::texture_descriptor(&descriptor);
        let raw = metal_sys::native::create_texture(device.raw(), &descriptor).map_err(|failure| sys_error(operation, failure))?;
        let actual = metal_sys::native::texture_info(&raw).map_err(|failure| sys_error(operation, failure))?;
        let d = raw_descriptor;
        let expected = [d.texture_type as i64, d.pixel_format as i64, d.width as i64, d.height as i64, d.depth as i64,
            d.mip_levels as i64, d.sample_count as i64, d.array_length as i64, d.usage as i64, d.storage_mode as i64,
            d.cpu_cache_mode as i64, d.hazard_tracking_mode as i64, d.allow_gpu_optimized_contents as i64,
            d.compression_type as i64, d.swizzle_red as i64, d.swizzle_green as i64, d.swizzle_blue as i64, d.swizzle_alpha as i64];
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
            format, width, height, StorageMode::Private, vec![TextureUsage::RenderAttachment], None,
        );
        let descriptor = ogpu_core::validation::admit_texture(&device.capabilities()?, descriptor)?;
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
    pub fn admitted_descriptor(&self) -> &AdmittedTexture { &self.0.descriptor }
    pub fn is_destroyed(&self) -> bool { self.0.lifetime.destroyed.load(Ordering::Acquire) }
    pub fn read_bytes(&self, read: &TextureRead) -> Result<Vec<u8>, Error> {
        const OP: &str = "Metal.Texture.read_bytes";
        self.validate_read_owner(OP, read)?;
        let o = read.region().origin(); let e = read.region().extent();
        metal_sys::native::read_texture_bytes(&self.0.raw, read.length(), read.bytes_per_row(), read.bytes_per_image(),
            (o.x as u64, o.y as u64, o.z as u64, e.width as u64, e.height as u64, e.depth as u64),
            read.mip_level() as u64, read.slice() as u64).map_err(|failure| sys_error(OP, failure))
    }
    pub fn read_bytes_into(&self, read: &TextureReadInto, destination: &mut [u8]) -> Result<(), Error> {
        const OP: &str = "Metal.Texture.read_bytes_into";
        read.bind_destination(destination.len())?;
        let read = read.layout();
        self.validate_read_owner(OP, read)?;
        let o = read.region().origin(); let e = read.region().extent();
        metal_sys::native::read_texture(&self.0.raw, destination, read.length(), read.bytes_per_row(), read.bytes_per_image(),
            (o.x as u64, o.y as u64, o.z as u64, e.width as u64, e.height as u64, e.depth as u64),
            read.mip_level() as u64, read.slice() as u64).map_err(|failure| sys_error(OP, failure))
    }
    fn validate_read_owner(&self, operation: &str, read: &TextureRead) -> Result<(), Error> {
        // A reusable layout can outlive admission. This checks allocation retirement,
        // never dimensions or pitches; the sys ABI still validates its opaque native handle.
        if self.0.descriptor.retired() {
            return Err(error(operation, ErrorKind::StaleHandle, "texture is destroyed"));
        }
        if !read.belongs_to(&self.0.descriptor) {
            return Err(error(operation, ErrorKind::InvalidArgument, "transfer belongs to another admitted texture"));
        }
        Ok(())
    }
    pub fn destroy(&self) -> Result<(), Error> {
        self.0.lifetime.destroy_parent("Metal.Texture.destroy")?;
        self.0.descriptor.retire();
        if self.0.device_attached.swap(false, Ordering::AcqRel) { self.0.device.lifetime().detach(); }
        if self.0.drawable_attached.swap(false, Ordering::AcqRel) {
            if let Some(dependents) = &self.0.drawable_dependents { dependents.fetch_sub(1, Ordering::AcqRel); }
        }
        Ok(())
    }
    pub(crate) fn raw(&self) -> &metal_sys::Handle { &self.0.raw }
    pub(crate) fn lifetime(&self) -> &Lifetime { &self.0.lifetime }
}
use std::cell::RefCell;

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
        Ok(Self(Arc::new(CommandBufferInner { raw, lifetime: Lifetime::new(), queue: queue.clone(), queue_attached: AtomicBool::new(true), resources: RefCell::new(Vec::new()) })))
    }
    pub fn device(&self) -> &Device { self.0.queue.device() }

    pub fn status(&self) -> Result<CommandBufferStatus, Error> {
        let operation = "Metal.Command_buffer.status";
        self.0.lifetime.ensure_live(operation)?;
        let status = metal_sys::registry::command_buffer_status(&self.0.raw).map_err(|failure| sys_error(operation, failure))?;
        let status = match status { 0 => CommandBufferStatus::NotEnqueued, 1 => CommandBufferStatus::Enqueued, 2 => CommandBufferStatus::Committed, 3 => CommandBufferStatus::Scheduled, 4 => CommandBufferStatus::Completed, 5 => CommandBufferStatus::Error, other => CommandBufferStatus::Unknown(other) };
        match status {
            CommandBufferStatus::Completed => { self.0.resources.borrow_mut().clear(); }
            CommandBufferStatus::Error => { self.0.resources.borrow_mut().clear(); }
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
        metal_sys::registry::command_buffer_commit(&self.0.raw).map_err(|failure| sys_error(operation, failure))?;

        Ok(())
    }
    pub fn wait_until_completed(&self) -> Result<(), Error> {
        let operation = "Metal.Command_buffer.wait_until_completed";
        self.0.lifetime.ensure_live(operation)?;
        let state = self.status()?;
        if matches!(state, CommandBufferStatus::NotEnqueued | CommandBufferStatus::Enqueued) { return Err(error(operation, ErrorKind::InvalidState, "command buffer has not been committed")); }
        metal_sys::native::wait_command_buffer(&self.0.raw).map_err(|failure| sys_error(operation, failure))?;
        self.0.resources.borrow_mut().clear();

        if let Some(message) = metal_sys::native::command_buffer_error(&self.0.raw).map_err(|failure| sys_error(operation, failure))? {

            return Err(error(operation, ErrorKind::DeviceLost, &message));
        }
        Ok(())
    }
    pub fn destroyed(&self) -> bool { self.0.lifetime.destroyed.load(Ordering::Acquire) }
    pub fn present(&self, drawable: &crate::native::surface::Drawable, time: PresentTime) -> Result<(), Error> {
        let operation = "Metal.Command_buffer.present";
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
    pub fn create(function: &crate::native::library::Function, options: &ogpu_core::shader::AdmittedComputeOptions) -> Result<Self, Error> {
        let operation = "Metal.Compute_pipeline.create";
        enter_main(operation)?;
        function.lifetime().ensure_live(operation)?;
        let label = options.label();
        let reflection = options.reflection();
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
    pub fn set_bytes(&self, index: usize, admitted: ogpu_core::validation::AdmittedBytes, bytes: &[u8]) -> Result<(), Error> {
        admitted.bind(bytes.len())?;
        metal_sys::native::set_compute_bytes(&self.0.raw, admitted, bytes, index as u64).map_err(|failure| sys_error("Metal.Compute_encoder.set_bytes", failure))
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
