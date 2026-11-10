use crate::caps::Caps;
use crate::error::{self, Error, ErrorKind};
use crate::types::{
    BufferDescriptor, SamplerDescriptor, TextureDescriptor, TextureFormat, TextureUsage,
};

pub use crate::types::{StorageMode, CpuCacheMode, HazardTrackingMode, TextureKind, CompressionType, Swizzle, SwizzleChannel};
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TextureProfile {
    pub descriptor: TextureDescriptor,
    pub kind: TextureKind,
    pub array_length: usize,
    pub storage: StorageMode,
    pub cpu_cache: CpuCacheMode,
    pub hazard_tracking: HazardTrackingMode,
    pub allow_gpu_optimized_contents: bool,
    pub compression: CompressionType,
    pub swizzle: Swizzle,
}
impl std::ops::Deref for TextureProfile {
    type Target = TextureDescriptor;
    fn deref(&self) -> &TextureDescriptor { &self.descriptor }
}
impl std::ops::DerefMut for TextureProfile {
    fn deref_mut(&mut self) -> &mut TextureDescriptor { &mut self.descriptor }
}
impl TextureProfile {
    pub fn descriptor_2d(format: TextureFormat, width: usize, height: usize, storage: StorageMode, usage: Vec<TextureUsage>, label: Option<String>) -> Self {
        Self { descriptor: TextureDescriptor { label, width: width as i64, height: height as i64, depth: 1,
            mip_levels: 1, sample_count: 1, format, usage }, kind: TextureKind::Texture2d, array_length: 1,
            storage, cpu_cache: CpuCacheMode::DefaultCache, hazard_tracking: HazardTrackingMode::Default,
            allow_gpu_optimized_contents: true, compression: CompressionType::Lossless, swizzle: Swizzle::IDENTITY }
    }
    pub fn ordinary(capabilities: &Caps, descriptor: TextureDescriptor, depth: bool, memoryless: bool) -> Self {
        let kind = if descriptor.sample_count > 1 { TextureKind::Texture2dMultisample }
            else if descriptor.depth > 1 { TextureKind::Texture3d } else { TextureKind::Texture2d };
        let storage = if memoryless { StorageMode::Memoryless }
            else if !depth && descriptor.sample_count == 1 && capabilities.unified_memory { StorageMode::Shared }
            else { StorageMode::Private };
        Self { descriptor, kind, array_length: 1, storage, cpu_cache: CpuCacheMode::DefaultCache,
            hazard_tracking: HazardTrackingMode::Default, allow_gpu_optimized_contents: true,
            compression: CompressionType::Lossless, swizzle: Swizzle::IDENTITY }
    }
}

/// Only Core admission constructs this immutable descriptor; all owners share the same shape.
#[derive(Clone, Debug)]
pub struct AdmittedTexture(Rc<AdmittedTextureInner>);
#[derive(Debug)]
struct AdmittedTextureInner { profile: TextureProfile, limits: crate::caps::Limits, retired: std::cell::Cell<bool> }
impl std::ops::Deref for AdmittedTexture {
    type Target = TextureProfile;
    fn deref(&self) -> &TextureProfile { &self.0.profile }
}
impl AdmittedTexture {
    pub fn admitted_for(&self, capabilities: &Caps) -> bool { self.0.limits == capabilities.limits }
    pub fn same(&self, other: &Self) -> bool { Rc::ptr_eq(&self.0, &other.0) }
    pub fn retire(&self) { self.0.retired.set(true); }
    pub fn retired(&self) -> bool { self.0.retired.get() }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BufferKind { Ordinary, Mapped, Readback }
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BufferProfile {
    pub descriptor: BufferDescriptor,
    pub storage: StorageMode,
    pub cpu_cache: CpuCacheMode,
    pub kind: BufferKind,
}
impl std::ops::Deref for BufferProfile {
    type Target = BufferDescriptor;
    fn deref(&self) -> &BufferDescriptor { &self.descriptor }
}

#[derive(Clone, Debug)]
pub struct AdmittedBuffer(Rc<AdmittedBufferInner>);
#[derive(Debug)]
struct AdmittedBufferInner { profile: BufferProfile, limits: crate::caps::Limits }
impl AdmittedBuffer { pub fn admitted_for(&self, capabilities: &Caps) -> bool { self.0.limits == capabilities.limits } }
impl std::ops::Deref for AdmittedBuffer {
    type Target = BufferProfile;
    fn deref(&self) -> &BufferProfile { &self.0.profile }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TextureRegion {
    pub x: usize, pub y: usize, pub z: usize,
    pub width: usize, pub height: usize, pub depth: usize,
}
impl TextureRegion {
    pub fn origin(self) -> crate::types::Origin { crate::types::Origin { x: self.x as i64, y: self.y as i64, z: self.z as i64 } }
    pub fn extent(self) -> crate::types::Extent { crate::types::Extent { width: self.width as i64, height: self.height as i64, depth: self.depth as i64 } }
}
/// A transfer layout belongs to the admitted allocation shape, not an interchangeable row count.
pub struct TextureRead {
    profile: AdmittedTexture,
    region: TextureRegion,
    mip_level: usize,
    slice: usize,
    bytes_per_row: usize,
    bytes_per_image: usize,
    length: usize,
}
impl TextureRead {
    pub fn belongs_to(&self, profile: &AdmittedTexture) -> bool { self.profile.same(profile) }
    pub fn region(&self) -> TextureRegion { self.region }
    pub fn mip_level(&self) -> usize { self.mip_level }
    pub fn slice(&self) -> usize { self.slice }
    pub fn bytes_per_row(&self) -> usize { self.bytes_per_row }
    pub fn bytes_per_image(&self) -> usize { self.bytes_per_image }
    pub fn length(&self) -> usize { self.length }
    pub fn into_destination(self, destination: &mut [u8]) -> Result<TextureReadInto, Error> {
        if destination.len() != self.length {
            return Err(invalid("Ogpu.Validation.admit_texture_read_into", "destination length does not match the transfer layout"));
        }
        Ok(TextureReadInto { read: self })
    }
}
pub struct TextureReadInto { read: TextureRead }
impl TextureReadInto {
    pub fn layout(&self) -> &TextureRead { &self.read }
    pub fn bind_destination(&self, length: usize) -> Result<(), Error> {
        // Bind a replacement borrow to the admitted layout; do not readmit that layout.
        if length != self.read.length() {
            Err(invalid("Ogpu.Validation.bind_texture_destination", "destination length does not match the transfer layout"))
        } else { Ok(()) }
    }
}

fn invalid(operation: &str, message: &str) -> Error {
    error::make(operation, ErrorKind::InvalidArgument, message)
}

pub fn validate_label(operation: &str, label: Option<&str>) -> Result<(), Error> {
    if label.is_some_and(|label| label.contains('\0')) {
        Err(invalid(operation, "label contains NUL"))
    } else {
        Ok(())
    }
}

fn validate_buffer_shape(
    operation: &str,
    max_size: i64,
    size: i64,
    usage_count: usize,
) -> Result<(), Error> {
    if max_size <= 0 || size <= 0 || size > max_size {
        Err(invalid(operation, "buffer size is outside device limits"))
    } else if usage_count == 0 {
        Err(invalid(operation, "buffer usage is empty"))
    } else {
        Ok(())
    }
}

pub fn admit_texture(capabilities: &Caps, mut profile: TextureProfile) -> Result<AdmittedTexture, Error> {
    validate_texture_profile(capabilities, &profile)?;
    if profile.hazard_tracking == HazardTrackingMode::Default { profile.hazard_tracking = HazardTrackingMode::Tracked; }
    Ok(AdmittedTexture(Rc::new(AdmittedTextureInner { profile, limits: capabilities.limits, retired: std::cell::Cell::new(false) })))
}

pub fn validate_texture_profile(capabilities: &Caps, profile: &TextureProfile) -> Result<(), Error> {
    let operation = "Ogpu.Validation.validate_texture_profile";
    let d = &profile.descriptor;
    let limits = capabilities.limits;
    validate_label(operation, d.label.as_deref())?;
    let maximum = match profile.kind {
        TextureKind::Texture1d | TextureKind::Texture1dArray => limits.max_texture_dimension_1d,
        TextureKind::Texture3d => limits.max_texture_dimension_3d,
        TextureKind::TextureCube | TextureKind::TextureCubeArray => limits.max_texture_dimension_cube,
        TextureKind::TextureBuffer => return Err(invalid(operation, "texture-buffer resources must be created from a buffer")),
        _ => limits.max_texture_dimension_2d,
    };
    if d.width <= 0 || d.height <= 0 || d.depth <= 0 || d.width > maximum || d.height > maximum
        || d.depth > limits.max_texture_dimension_3d || d.mip_levels <= 0 {
        return Err(invalid(operation, "texture extent or usage is outside device limits"));
    }
    if !limits.supports_texture_samples(d.format, d.sample_count) { return Err(invalid(operation, "texture sample count is unsupported")); }
    if d.width.checked_mul(d.height).and_then(|n| n.checked_mul(d.depth)).is_none() {
        return Err(invalid(operation, "texture texel cardinality overflows"));
    }
    if d.mip_levels > (i64::BITS - d.width.max(d.height).max(d.depth).leading_zeros()) as i64 {
        return Err(invalid(operation, "texture mip cardinality exceeds its extent"));
    }
    if profile.array_length == 0 || profile.array_length as u128 > limits.max_texture_array_layers as u128 {
        return Err(invalid(operation, "texture array length exceeds device limits"));
    }
    if d.usage.iter().enumerate().any(|(i, usage)| d.usage[..i].contains(usage)) {
        return Err(invalid(operation, "texture usage contains duplicates"));
    }
    let array = matches!(profile.kind, TextureKind::Texture1dArray | TextureKind::Texture2dArray | TextureKind::TextureCubeArray | TextureKind::Texture2dMultisampleArray);
    if (array && profile.array_length < 2) || (!array && profile.array_length != 1) {
        return Err(invalid(operation, "texture array cardinality does not match its kind"));
    }
    let multisample = matches!(profile.kind, TextureKind::Texture2dMultisample | TextureKind::Texture2dMultisampleArray);
    if (multisample && (d.mip_levels != 1 || d.sample_count == 1)) || (!multisample && d.sample_count != 1) {
        return Err(invalid(operation, "texture sample cardinality does not match its kind"));
    }
    match profile.kind {
        TextureKind::Texture1d | TextureKind::Texture1dArray if d.height != 1 || d.depth != 1 => return Err(invalid(operation, "1D textures require height=depth=1")),
        TextureKind::Texture3d => {},
        _ if d.depth != 1 => return Err(invalid(operation, "2D and cube textures require depth=1")),
        _ => {},
    }
    if matches!(profile.kind, TextureKind::TextureCube | TextureKind::TextureCubeArray) && d.width != d.height {
        return Err(invalid(operation, "cube textures must be square"));
    }
    if profile.swizzle != Swizzle::IDENTITY && d.usage.iter().any(|u| matches!(u, TextureUsage::StorageBinding | TextureUsage::TextureAtomic)) {
        return Err(invalid(operation, "texture swizzling is incompatible with writable usage"));
    }
    if profile.storage == StorageMode::Memoryless && d.usage != [TextureUsage::RenderAttachment] {
        return Err(invalid(operation, "a memoryless texture is attachment-only"));
    }
    if profile.compression == CompressionType::Lossy {
        return Err(invalid(operation, "pixel format does not support lossy texture compression"));
    }
    if profile.storage == StorageMode::Shared && multisample {
        return Err(invalid(operation, "shared multisample textures are unsupported"));
    }
    Ok(())
}

pub fn admit_buffer(capabilities: &Caps, profile: BufferProfile) -> Result<AdmittedBuffer, Error> {
    const OP: &str = "Ogpu.Validation.admit_buffer";
    validate_buffer(capabilities, &profile.descriptor)?;
    if profile.storage == StorageMode::Memoryless { return Err(invalid(OP, "a buffer cannot be memoryless")); }
    if profile.kind == BufferKind::Mapped && (profile.descriptor.size % 4 != 0 || profile.storage != StorageMode::Shared || profile.cpu_cache != CpuCacheMode::DefaultCache) {
        return Err(invalid(OP, "mapped buffer size must be a multiple of four in shared storage"));
    }
    if profile.kind == BufferKind::Readback && !profile.descriptor.usage.contains(&crate::types::BufferUsage::CopyDst) {
        return Err(invalid(OP, "readback buffers require CopyDst usage"));
    }
    Ok(AdmittedBuffer(Rc::new(AdmittedBufferInner { profile, limits: capabilities.limits })))
}

pub fn admit_texture_read(profile: &AdmittedTexture, region: TextureRegion, mip_level: usize, slice: usize,
    bytes_per_row: usize, bytes_per_image: usize) -> Result<TextureRead, Error> {
    const OP: &str = "Ogpu.Validation.admit_texture_read";
    if profile.retired() { return Err(error::make(OP, ErrorKind::StaleHandle, "texture is destroyed")); }
    texture_read_layout(profile, region, mip_level, slice, bytes_per_row, bytes_per_image)
}

pub fn admit_base_texture_read(profile: &AdmittedTexture, bytes_per_row: i64) -> Result<TextureRead, Error> {
    const OP: &str = "Ogpu.Validation.admit_base_texture_read";
    if profile.retired() { return Err(error::make(OP, ErrorKind::StaleHandle, "texture is destroyed")); }
    if profile.format == TextureFormat::Depth32Float {
        return Err(error::make(OP, ErrorKind::Unsupported, "depth textures are not host readable"));
    }
    let row = usize::try_from(bytes_per_row).map_err(|_| invalid(OP, "row pitch is invalid"))?;
    let image = bytes_per_row.checked_mul(profile.height).and_then(|n| usize::try_from(n).ok())
        .ok_or_else(|| invalid(OP, "row pitch is invalid"))?;
    texture_read_layout(profile, TextureRegion { x: 0, y: 0, z: 0, width: profile.width as usize,
        height: profile.height as usize, depth: 1 }, 0, 0, row, image)
}

fn texture_read_layout(profile: &AdmittedTexture, region: TextureRegion, mip_level: usize, slice: usize,
    bytes_per_row: usize, bytes_per_image: usize) -> Result<TextureRead, Error> {
    const OP: &str = "Ogpu.Validation.admit_texture_read";
    if matches!(profile.kind, TextureKind::Texture2dMultisample | TextureKind::Texture2dMultisampleArray) {
        return Err(error::make(OP, ErrorKind::Unsupported, "texture has no CPU transfer mapping"));
    }
    let slices = match profile.kind { TextureKind::TextureCube => 6,
        TextureKind::TextureCubeArray => profile.array_length.checked_mul(6).ok_or_else(|| invalid(OP, "slice cardinality overflow"))?,
        TextureKind::Texture1dArray | TextureKind::Texture2dArray | TextureKind::Texture2dMultisampleArray => profile.array_length,
        _ => 1 };
    if mip_level as u128 >= profile.mip_levels as u128 || slice >= slices {
        return Err(invalid(OP, "texture mip or slice is outside the allocation"));
    }
    let o = region.origin(); let e = region.extent();
    let mip = |n: i64| (n >> mip_level.min(62)).max(1);
    if o.x < 0 || o.y < 0 || o.z < 0 || e.width <= 0 || e.height <= 0 || e.depth <= 0
        || o.x > mip(profile.width) || e.width > mip(profile.width)-o.x
        || o.y > mip(profile.height) || e.height > mip(profile.height)-o.y
        || o.z > mip(profile.depth) || e.depth > mip(profile.depth)-o.z {
        return Err(invalid(OP, "texture region exceeds the selected mip level"));
    }
    let texel = crate::types::texel_bytes(profile.format) as usize;
    let min_row = (e.width as usize).checked_mul(texel).ok_or_else(|| invalid(OP, "row overflow"))?;
    let min_image = bytes_per_row.checked_mul(e.height as usize).ok_or_else(|| invalid(OP, "image overflow"))?;
    if bytes_per_row < min_row || bytes_per_row % texel != 0 || bytes_per_image < min_image {
        return Err(invalid(OP, "texture row or image pitch is invalid"));
    }
    let length = bytes_per_image.checked_mul(e.depth as usize).ok_or_else(|| invalid(OP, "transfer length overflow"))?;
    Ok(TextureRead { profile: profile.clone(), region, mip_level, slice, bytes_per_row, bytes_per_image, length })
}

pub fn validate_buffer(capabilities: &Caps, descriptor: &BufferDescriptor) -> Result<(), Error> {
    let operation = "Ogpu.Types.validate_buffer";
    validate_label(operation, descriptor.label.as_deref())?;
    validate_buffer_shape(
        operation,
        capabilities.limits.max_buffer_size,
        descriptor.size,
        descriptor.usage.len(),
    )
}

pub fn admit_ordinary_texture(capabilities: &Caps, mut descriptor: TextureDescriptor, depth: bool, memoryless: bool) -> Result<AdmittedTexture, Error> {
    const OP: &str = "Ogpu.Types.validate_texture";
    let operation = OP;
    if matches!(
        descriptor.format,
        TextureFormat::R8Unorm
            | TextureFormat::Bgra8Unorm
            | TextureFormat::Bgra8UnormSrgb
            | TextureFormat::Depth32Float
    ) {
        return Err(invalid(
            operation,
            "OGPU texture descriptors require an RGBA color format",
        ));
    }
    if descriptor.format != TextureFormat::Rgba8Unorm
        && (descriptor.usage.contains(&TextureUsage::RenderAttachment)
            || descriptor.sample_count > 1)
    {
        return Err(error::make(
            operation,
            ErrorKind::Unsupported,
            "float textures are not render attachments",
        ));
    }
    if descriptor.usage.is_empty() { return Err(invalid(OP, "texture usage is empty")); }
    if (depth || memoryless) && descriptor.usage != [TextureUsage::RenderAttachment] {
        return Err(invalid(OP, "depth and memoryless textures are attachment-only"));
    }
    if descriptor.sample_count > 1 && (descriptor.mip_levels != 1 || descriptor.depth != 1 || descriptor.usage != [TextureUsage::RenderAttachment]) {
        return Err(invalid(OP, "multisample textures require one attachment-only mip and depth slice"));
    }
    if depth { descriptor.format = TextureFormat::Depth32Float; }
    admit_texture(capabilities, TextureProfile::ordinary(capabilities, descriptor, depth, memoryless))
}

pub fn validate_texture(capabilities: &Caps, descriptor: &TextureDescriptor) -> Result<(), Error> {
    admit_ordinary_texture(capabilities, descriptor.clone(), false, false).map(|_| ())
}

pub fn validate_sampler(descriptor: &SamplerDescriptor) -> Result<(), Error> {
    admit_sampler(SamplerProfile::ordinary(descriptor.clone())).map(|_| ())
}

pub fn validate_layout(operation: &str, entries: &[(i64, Vec<i64>)]) -> Result<(), Error> {
    let mut entries = entries.to_vec();
    entries.sort_by_key(|(binding, _)| *binding);
    let mut previous = None;
    for (binding, visibility) in entries {
        if binding < 0 {
            return Err(invalid(operation, "binding is negative"));
        }
        if visibility.is_empty() {
            return Err(invalid(operation, "visibility is empty"));
        }
        if visibility.windows(2).any(|pair| pair[0] >= pair[1]) {
            return Err(invalid(operation, "visibility must be sorted and unique"));
        }
        if previous == Some(binding) {
            return Err(invalid(operation, "duplicate binding"));
        }
        previous = Some(binding);
    }
    Ok(())
}

pub fn validate_groups(operation: &str, max_groups: i64, groups: &[i64]) -> Result<(), Error> {
    let mut groups = groups.to_vec();
    groups.sort_unstable();
    if max_groups < 0 || groups.len() as i64 > max_groups {
        return Err(error::make(operation, ErrorKind::Capacity, "bind-group limit exceeded"));
    }
    for (expected, group) in groups.into_iter().enumerate() {
        if group != expected as i64 {
            return Err(invalid(operation, "groups must be contiguous from zero"));
        }
    }
    Ok(())
}

use crate::types::{AddressMode, MipFilter, SamplerFilter};
use crate::render_pass::Comparison as CompareFunction;
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BorderColor { TransparentBlack, OpaqueBlack, OpaqueWhite }
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ReductionMode { WeightedAverage, Minimum, Maximum }

#[derive(Clone, Debug)]
pub struct SamplerProfile {
    pub min_filter: SamplerFilter, pub mag_filter: SamplerFilter, pub mip_filter: MipFilter,
    pub max_anisotropy: i64, pub address_s: AddressMode, pub address_t: AddressMode,
    pub address_r: AddressMode, pub border_color: BorderColor, pub reduction_mode: ReductionMode,
    pub normalized_coordinates: bool, pub lod_min: f64, pub lod_max: f64,
    pub lod_average: bool, pub lod_bias: f64, pub compare_function: CompareFunction,
    pub support_argument_buffers: bool, pub label: Option<String>,
}
impl Default for SamplerProfile {
    fn default() -> Self { Self { min_filter: SamplerFilter::Nearest, mag_filter: SamplerFilter::Nearest, mip_filter: MipFilter::NoMip, max_anisotropy: 1, address_s: AddressMode::ClampToEdge, address_t: AddressMode::ClampToEdge, address_r: AddressMode::ClampToEdge, border_color: BorderColor::TransparentBlack, reduction_mode: ReductionMode::WeightedAverage, normalized_coordinates: true, lod_min: 0.0, lod_max: f32::MAX as f64, lod_average: false, lod_bias: 0.0, compare_function: CompareFunction::Never, support_argument_buffers: false, label: None } }
}

impl SamplerProfile {
    pub fn ordinary(d: SamplerDescriptor) -> Self {
        Self { label: d.label, min_filter: d.min_filter, mag_filter: d.mag_filter, mip_filter: d.mip_filter,
            address_s: d.address_u, address_t: d.address_v, lod_min: d.lod_min, lod_max: d.lod_max,
            max_anisotropy: d.max_anisotropy, support_argument_buffers: true, ..Self::default() }
    }
}
#[derive(Clone, Debug)]
pub struct AdmittedSampler(SamplerProfile);
impl std::ops::Deref for AdmittedSampler {
    type Target = SamplerProfile;
    fn deref(&self) -> &SamplerProfile { &self.0 }
}
pub fn admit_sampler(profile: SamplerProfile) -> Result<AdmittedSampler, Error> {
    const OP: &str = "Ogpu.Validation.admit_sampler";
    validate_label(OP,profile.label.as_deref())?;
    if !profile.lod_min.is_finite() || !profile.lod_max.is_finite() || !profile.lod_bias.is_finite()
        || !(profile.lod_min as f32).is_finite() || !(profile.lod_max as f32).is_finite() || !(profile.lod_bias as f32).is_finite()
        || profile.lod_min < 0. || profile.lod_max < profile.lod_min || !(1..=16).contains(&profile.max_anisotropy)
        || !(-16.0f32..=15.999f32).contains(&(profile.lod_bias as f32)) {
        return Err(invalid(OP,"sampler LOD/anisotropy limits are invalid"));
    }
    if !profile.normalized_coordinates && (profile.address_s != AddressMode::ClampToEdge || profile.address_t != AddressMode::ClampToEdge
        || profile.address_r != AddressMode::ClampToEdge || profile.mip_filter != MipFilter::NoMip || profile.max_anisotropy != 1) {
        return Err(invalid(OP,"unnormalized coordinates require clamp-to-edge, no mip filter and anisotropy one"));
    }
    Ok(AdmittedSampler(profile))
}

#[derive(Clone, Copy)]
pub struct AdmittedBytes(usize);
impl AdmittedBytes {
    pub fn length(self) -> usize { self.0 }
    pub fn bind(self, length: usize) -> Result<(), Error> {
        // Bind the current borrow; the 1..=4096 admission has already happened.
        if length != self.0 {
            Err(invalid("Ogpu.Validation.bind_bytes", "inline byte length does not match admission"))
        } else { Ok(()) }
    }
}
pub fn admit_bytes(operation:&str,bytes:&[u8])->Result<AdmittedBytes,Error>{
    if bytes.is_empty() || bytes.len()>4096 {Err(invalid(operation,"inline bytes must contain 1..=4096 bytes"))}
    else {Ok(AdmittedBytes(bytes.len()))}
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum IcbCommandType { IndirectDraw, IndirectDrawIndexed, IndirectConcurrentDispatch, IndirectConcurrentDispatchThreads }
impl IcbCommandType {
    fn bit(self) -> i64 {
        match self { Self::IndirectDraw => 1, Self::IndirectDrawIndexed => 2, Self::IndirectConcurrentDispatch => 32, Self::IndirectConcurrentDispatchThreads => 64 }
    }
}

#[derive(Clone, Debug)]
pub struct IcbDescriptor {
    pub command_types: Vec<IcbCommandType>,
    pub inherit_buffers: bool,
    pub inherit_pipeline_state: bool,
    pub max_vertex_buffer_bind_count: usize,
    pub max_fragment_buffer_bind_count: usize,
    pub max_kernel_buffer_bind_count: usize,
    pub support_ray_tracing: bool,
    pub support_dynamic_attribute_stride: bool,
    pub max_kernel_threadgroup_memory_bind_count: usize,
    pub max_object_buffer_bind_count: usize,
    pub max_mesh_buffer_bind_count: usize,
    pub max_object_threadgroup_memory_bind_count: usize,
    pub inherit_depth_stencil_state: bool,
    pub inherit_depth_bias: bool,
    pub inherit_depth_clip_mode: bool,
    pub inherit_cull_mode: bool,
    pub inherit_front_facing_winding: bool,
    pub inherit_triangle_fill_mode: bool,
    pub support_color_attachment_mapping: bool,
}
impl IcbDescriptor {
    pub fn new(command_types: Vec<IcbCommandType>) -> Self {
        Self {
            command_types,
            inherit_buffers: false,
            inherit_pipeline_state: false,
            max_vertex_buffer_bind_count: 0,
            max_fragment_buffer_bind_count: 0,
            max_kernel_buffer_bind_count: 0,
            support_ray_tracing: false,
            support_dynamic_attribute_stride: false,
            max_kernel_threadgroup_memory_bind_count: 0,
            max_object_buffer_bind_count: 0,
            max_mesh_buffer_bind_count: 0,
            max_object_threadgroup_memory_bind_count: 0,
            inherit_depth_stencil_state: true,
            inherit_depth_bias: true,
            inherit_depth_clip_mode: true,
            inherit_cull_mode: true,
            inherit_front_facing_winding: true,
            inherit_triangle_fill_mode: true,
            support_color_attachment_mapping: false,
        }
    }
}


#[derive(Clone, Debug)]
pub struct AdmittedIcb { capacity: usize, descriptor: IcbDescriptor }
impl AdmittedIcb {
    pub fn capacity(&self) -> usize { self.capacity }
    pub fn descriptor(&self) -> &IcbDescriptor { &self.descriptor }
    pub fn command_types_bits(&self) -> i64 { self.descriptor.command_types.iter().fold(0, |bits, command| bits | command.bit()) }
}
pub fn admit_icb(capacity: usize, descriptor: IcbDescriptor) -> Result<AdmittedIcb, Error> {
    const OP: &str = "Ogpu.Validation.admit_icb";
    if capacity == 0 { return Err(invalid(OP, "maximum command count must be positive")); }
    if descriptor.command_types.is_empty() { return Err(invalid(OP, "at least one indirect command type is required")); }
    if descriptor.command_types.windows(2).any(|pair| pair[0] as u8 >= pair[1] as u8) {
        return Err(invalid(OP, "indirect command types must be unique and ordered"));
    }
    if [descriptor.max_vertex_buffer_bind_count, descriptor.max_fragment_buffer_bind_count,
        descriptor.max_kernel_buffer_bind_count, descriptor.max_kernel_threadgroup_memory_bind_count,
        descriptor.max_object_buffer_bind_count, descriptor.max_mesh_buffer_bind_count,
        descriptor.max_object_threadgroup_memory_bind_count].iter().any(|&count| count > 31) {
        return Err(invalid(OP, "indirect binding counts must be in [0, 31]"));
    }
    Ok(AdmittedIcb { capacity, descriptor })
}

/// An index admitted for an ICB capacity; the native owner checks the capacity identity only.
#[derive(Clone, Copy, Debug)]
pub struct AdmittedIcbIndex { index: usize, capacity: usize }
impl AdmittedIcbIndex {
    pub fn index(self) -> usize { self.index }
    pub fn belongs_to_capacity(self, capacity: usize) -> bool { self.capacity == capacity }
}
pub fn admit_icb_index(capacity: usize, index: i64) -> Result<AdmittedIcbIndex, Error> {
    let index = usize::try_from(index).ok().filter(|&index| index < capacity)
        .ok_or_else(|| invalid("Ogpu.Validation.admit_icb_index", "indirect command index is out of range"))?;
    Ok(AdmittedIcbIndex { index, capacity })
}

#[derive(Clone, Copy, Debug)]
pub struct AdmittedSlot(u8);
impl AdmittedSlot { pub fn index(self) -> u8 { self.0 } }
pub fn admit_slot(operation: &str, index: i64) -> Result<AdmittedSlot, Error> {
    if !(0..31).contains(&index) { Err(invalid(operation, "binding slot is out of range")) }
    else { Ok(AdmittedSlot(index as u8)) }
}

pub fn admit_acceleration_size(operation: &str, size: u64) -> Result<std::num::NonZeroU64, Error> {
    std::num::NonZeroU64::new(size).ok_or_else(|| invalid(operation,"structure size must be positive"))
}

#[inline]
pub fn validate_acceleration_use(operation: &str, count: usize) -> Result<(), Error> {
    if count == 0 { Err(invalid(operation, "acceleration structure list must not be empty")) }
    else { Ok(()) }
}

pub fn validate_resource_use(operation: &str, count: usize, usage: i32, stages: i32) -> Result<(), Error> {
    if count == 0 || usage == 0 || stages == 0 {
        Err(invalid(operation, "resources, usage, and stages must be nonempty"))
    } else if usage & !7 != 0 || stages & !3 != 0 {
        Err(invalid(operation, "invalid render resource-use flags"))
    } else { Ok(()) }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CompactedSizeType { Uint32, Uint64 }
#[derive(Clone, Copy, Debug)]
pub struct AdmittedCompactedSize { offset:u64, size_type:CompactedSizeType }
impl AdmittedCompactedSize {
    pub fn offset(self)->u64 {self.offset}
    pub fn size_type(self)->CompactedSizeType {self.size_type}
    pub fn bytes(self)->u64 {match self.size_type {CompactedSizeType::Uint32=>4,CompactedSizeType::Uint64=>8}}
}
pub fn admit_compacted_size_write(operation:&str,offset:u64,destination_length:u64,size_type:CompactedSizeType)->Result<AdmittedCompactedSize,Error> {
    let output=AdmittedCompactedSize {offset,size_type};
    let bytes=output.bytes();
    if offset%bytes!=0 || offset>destination_length || bytes>destination_length-offset {
        return Err(invalid(operation,"compacted-size output range is invalid"));
    }
    Ok(output)
}
pub fn validate_sample_query(operation:&str,count:u64)->Result<(),Error> {
    if count==0 {Err(invalid(operation,"texture sample count must be positive"))} else {Ok(())}
}
pub fn validate_render_command_types(operation:&str,types:&[IcbCommandType])->Result<(),Error> {
    if types.iter().any(|t|matches!(t,IcbCommandType::IndirectDraw|IcbCommandType::IndirectDrawIndexed)) {Ok(())}
    else {Err(error::make(operation,ErrorKind::InvalidState,"descriptor does not enable render commands"))}
}

#[derive(Clone, Copy, Debug)]
pub struct AdmittedIndexRange { index_type:crate::render_pass::IndexType,offset:u64,count:std::num::NonZeroU64,instances:std::num::NonZeroU64 }
impl AdmittedIndexRange {
    #[inline]
    pub fn index_type(&self)->crate::render_pass::IndexType { self.index_type }
    #[inline]
    pub fn offset(&self)->u64 { self.offset }
    #[inline]
    pub fn count(&self)->u64 { self.count.get() }
    #[inline]
    pub fn instances(&self)->u64 { self.instances.get() }
    pub fn element_bytes(&self)->u64 { match self.index_type { crate::render_pass::IndexType::Uint16=>2,crate::render_pass::IndexType::Uint32=>4 } }
}
/// Native draws may include incomplete primitives; ordinary Core callers apply their topology policy separately.
pub fn admit_index_range(operation:&str,index_type:crate::render_pass::IndexType,offset:u64,count:u64,instances:u64)->Result<AdmittedIndexRange,Error>{
    let failure=||invalid(operation,"index range is invalid");
    let range=AdmittedIndexRange {index_type,offset,count:std::num::NonZeroU64::new(count).ok_or_else(failure)?,instances:std::num::NonZeroU64::new(instances).ok_or_else(failure)?};
    if offset%range.element_bytes()!=0 { return Err(failure()); }
    Ok(range)
}
#[inline]
pub fn validate_base_vertex(operation:&str,base_vertex:i64)->Result<(),Error>{
    if base_vertex<0 {Err(invalid(operation,"base vertex must be nonnegative"))}else{Ok(())}
}
