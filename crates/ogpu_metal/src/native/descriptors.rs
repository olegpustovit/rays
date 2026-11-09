use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

use crate::native::resource::{error, sys_error, enter_main, Device, Error, ErrorKind, Lifetime, Texture};
use metal_sys::native::descriptors as sys;

pub use ogpu_core::types::SamplerFilter as Filter;
pub use ogpu_core::types::MipFilter;
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum AddressMode { ClampToEdge, MirrorClampToEdge, Repeat, MirrorRepeat, ClampToZero, ClampToBorder }
pub use ogpu_core::render_pass::Comparison as CompareFunction;
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum StencilOperation { Keep, Zero, Replace, IncrementClamp, DecrementClamp, Invert, IncrementWrap, DecrementWrap }
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BorderColor { TransparentBlack, OpaqueBlack, OpaqueWhite }
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ReductionMode { WeightedAverage, Minimum, Maximum }

#[derive(Clone, Debug)]
pub struct SamplerDescriptor {
    pub min_filter: Filter, pub mag_filter: Filter, pub mip_filter: MipFilter,
    pub max_anisotropy: u8, pub address_s: AddressMode, pub address_t: AddressMode,
    pub address_r: AddressMode, pub border_color: BorderColor, pub reduction_mode: ReductionMode,
    pub normalized_coordinates: bool, pub lod_min: f32, pub lod_max: f32,
    pub lod_average: bool, pub lod_bias: f32, pub compare_function: CompareFunction,
    pub support_argument_buffers: bool, pub label: Option<String>,
}
impl Default for SamplerDescriptor {
    fn default() -> Self { Self { min_filter: Filter::Nearest, mag_filter: Filter::Nearest, mip_filter: MipFilter::NoMip, max_anisotropy: 1, address_s: AddressMode::ClampToEdge, address_t: AddressMode::ClampToEdge, address_r: AddressMode::ClampToEdge, border_color: BorderColor::TransparentBlack, reduction_mode: ReductionMode::WeightedAverage, normalized_coordinates: true, lod_min: 0.0, lod_max: f32::MAX, lod_average: false, lod_bias: 0.0, compare_function: CompareFunction::Never, support_argument_buffers: false, label: None } }
}
fn load_code(value: LoadAction) -> i32 { match value { LoadAction::DontCare => 0, LoadAction::Load => 1, LoadAction::Clear => 2 } }
fn compare(v: CompareFunction) -> i32 { v as i32 }
fn filter(v: Filter) -> i32 { v as i32 }
fn mip(v: MipFilter) -> i32 { v as i32 }
fn address(v: AddressMode) -> i32 { v as i32 }
fn operation(v: StencilOperation) -> i32 { v as i32 }
fn sampler_raw(d: &SamplerDescriptor) -> sys::SamplerDescriptor { sys::SamplerDescriptor { min_filter: filter(d.min_filter), mag_filter: filter(d.mag_filter), mip_filter: mip(d.mip_filter), max_anisotropy: d.max_anisotropy as i32, address_s: address(d.address_s), address_t: address(d.address_t), address_r: address(d.address_r), border_color: d.border_color as i32, reduction_mode: d.reduction_mode as i32, normalized_coordinates: d.normalized_coordinates as u8, lod_min: d.lod_min, lod_max: d.lod_max, lod_average: d.lod_average as u8, lod_bias: d.lod_bias, compare_function: compare(d.compare_function), support_argument_buffers: d.support_argument_buffers as u8 } }
struct SamplerInner { raw: metal_sys::Handle, lifetime: Lifetime, device: Device, device_attached: AtomicBool }
impl SamplerInner { fn detach_device(&self) { if self.device_attached.swap(false, Ordering::AcqRel) { self.device.lifetime().detach(); } } }
impl Drop for SamplerInner { fn drop(&mut self) { self.lifetime.destroyed.store(true, Ordering::Release); self.detach_device(); } }
#[derive(Clone)]
pub struct Sampler(Arc<SamplerInner>);
impl Sampler {
    pub fn create(device: &Device, d: SamplerDescriptor) -> Result<Self, Error> {
        let op = "Metal.Sampler.create";
        enter_main(op)?; device.ensure_live(op)?;
        if d.max_anisotropy == 0 || d.max_anisotropy > 16 || !d.lod_min.is_finite() || !d.lod_max.is_finite() || !d.lod_bias.is_finite() || d.lod_min < 0.0 || d.lod_max < d.lod_min || d.lod_max > f32::MAX || !(-16.0..=15.999).contains(&d.lod_bias) || d.label.as_deref().is_some_and(|x| x.as_bytes().contains(&0)) { return Err(error(op, ErrorKind::InvalidArgument, "invalid sampler descriptor")); }
        if !d.normalized_coordinates && (d.address_s != AddressMode::ClampToEdge || d.address_t != AddressMode::ClampToEdge || d.address_r != AddressMode::ClampToEdge || d.mip_filter != MipFilter::NoMip || d.max_anisotropy != 1) { return Err(error(op, ErrorKind::InvalidArgument, "unnormalized coordinates require clamp-to-edge, no mip filter and anisotropy one")); }
        if (d.reduction_mode != ReductionMode::WeightedAverage || d.lod_bias != 0.0) && !device.supports_sampler_reduction()? {
            return Err(error(op, ErrorKind::Unsupported, "sampler reduction modes and LOD bias require macOS 26 and Apple GPU family 10"));
        }
        let raw = sys::create_sampler(device.raw(), &sampler_raw(&d), d.label.as_deref()).map_err(|failure| sys_error(op, failure))?;
        device.lifetime().attach(); Ok(Self(Arc::new(SamplerInner { raw, lifetime: Lifetime::new(), device: device.clone(), device_attached: AtomicBool::new(true) })))
    }
    pub fn destroy(&self) -> Result<(), Error> { self.0.lifetime.destroy_parent("Metal.Sampler.destroy")?; self.0.detach_device(); Ok(()) }
    pub(crate) fn raw(&self) -> &metal_sys::Handle { &self.0.raw }
    pub(crate) fn lifetime(&self) -> &Lifetime { &self.0.lifetime }
    pub(crate) fn device(&self) -> &Device { &self.0.device }
}

#[derive(Clone, Copy, Debug)]
pub struct StencilFace { pub compare: CompareFunction, pub stencil_fail: StencilOperation, pub depth_fail: StencilOperation, pub pass: StencilOperation, pub read_mask: u32, pub write_mask: u32 }
#[derive(Clone, Debug)]
pub struct DepthStencilDescriptor { pub depth_compare: CompareFunction, pub depth_write: bool, pub front_face: Option<StencilFace>, pub back_face: Option<StencilFace>, pub label: Option<String> }
impl Default for DepthStencilDescriptor { fn default() -> Self { Self { depth_compare: CompareFunction::Always, depth_write: false, front_face: None, back_face: None, label: None } } }
fn face(v: StencilFace) -> sys::StencilFaceDescriptor { sys::StencilFaceDescriptor { compare_function: compare(v.compare), stencil_failure_operation: operation(v.stencil_fail), depth_failure_operation: operation(v.depth_fail), pass_operation: operation(v.pass), read_mask: v.read_mask, write_mask: v.write_mask } }
struct DepthStencilInner { raw: metal_sys::Handle, lifetime: Lifetime, device: Device, device_attached: AtomicBool }
impl DepthStencilInner { fn detach_device(&self) { if self.device_attached.swap(false, Ordering::AcqRel) { self.device.lifetime().detach(); } } }
impl Drop for DepthStencilInner { fn drop(&mut self) { self.lifetime.destroyed.store(true, Ordering::Release); self.detach_device(); } }
#[derive(Clone)]
pub struct DepthStencil(Arc<DepthStencilInner>);
impl DepthStencil {
    pub fn create(device: &Device, d: DepthStencilDescriptor) -> Result<Self, Error> {
        let op = "Metal.DepthStencil.create";
        enter_main(op)?; device.ensure_live(op)?; if d.label.as_deref().is_some_and(|x| x.as_bytes().contains(&0)) { return Err(error(op, ErrorKind::InvalidArgument, "label contains NUL")); }
        let raw_desc = sys::DepthStencilDescriptor { depth_compare_function: compare(d.depth_compare), depth_write_enabled: d.depth_write as u8, front_face_stencil: d.front_face.map(face).unwrap_or_default(), has_front_face_stencil: d.front_face.is_some() as u8, back_face_stencil: d.back_face.map(face).unwrap_or_default(), has_back_face_stencil: d.back_face.is_some() as u8 };
        let raw = sys::create_depth_stencil(device.raw(), &raw_desc, d.label.as_deref()).map_err(|failure| sys_error(op, failure))?; device.lifetime().attach(); Ok(Self(Arc::new(DepthStencilInner { raw, lifetime: Lifetime::new(), device: device.clone(), device_attached: AtomicBool::new(true) })))
    }
    pub fn destroy(&self) -> Result<(), Error> { self.0.lifetime.destroy_parent("Metal.DepthStencil.destroy")?; self.0.detach_device(); Ok(()) }
    pub(crate) fn raw(&self) -> &metal_sys::Handle { &self.0.raw }
    pub(crate) fn lifetime(&self) -> &Lifetime { &self.0.lifetime }
    pub(crate) fn device(&self) -> &Device { &self.0.device }
}

struct RenderPassInner { raw: metal_sys::Handle, lifetime: Lifetime, device: Device, device_attached: AtomicBool, width: u64, height: u64, samples: u64, attachments: std::sync::Mutex<(Option<Texture>, Option<Texture>, Option<Texture>)>, resolve: std::sync::Mutex<Option<Texture>> }
impl RenderPassInner {
    fn detach_device(&self) { if self.device_attached.swap(false, Ordering::AcqRel) { self.device.lifetime().detach(); } }
    fn release_attachments(&self) {
        let mut guard = self.attachments.lock().unwrap();
        let attachments = &mut *guard;
        if let Some(texture) = self.resolve.lock().unwrap().take() { texture.lifetime().detach(); }
        for texture in [&mut attachments.0, &mut attachments.1, &mut attachments.2] {
            if let Some(texture) = texture.take() { texture.lifetime().detach(); }
        }
    }
}
impl Drop for RenderPassInner { fn drop(&mut self) { self.lifetime.destroyed.store(true, Ordering::Release); self.release_attachments(); self.detach_device(); } }
#[derive(Clone)]
pub struct RenderPassDescriptor(Arc<RenderPassInner>);
pub use ogpu_core::render_pass::Load as LoadAction;
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum StoreAction { DontCare, Store, MultisampleResolve, StoreAndMultisampleResolve }
impl RenderPassDescriptor {
    pub fn create(device: &Device, width: u64, height: u64, sample_count: u64) -> Result<Self, Error> {
        let op = "Metal.RenderPassDescriptor.create";
        enter_main(op)?; device.ensure_live(op)?; let raw = sys::create_render_pass().map_err(|failure| sys_error(op, failure))?; if let Err(failure) = sys::set_render_pass_sizes(&raw, width, height, sample_count).map_err(|failure| sys_error(op, failure)) { drop(raw); return Err(failure); } device.lifetime().attach(); Ok(Self(Arc::new(RenderPassInner { raw, lifetime: Lifetime::new(), device: device.clone(), device_attached: AtomicBool::new(true), width, height, samples: sample_count, attachments: std::sync::Mutex::new((None,None,None)), resolve: std::sync::Mutex::new(None) })))
    }
    pub fn set_attachments(&self, color: &Texture, depth: Option<&Texture>, stencil: Option<&Texture>, clear: [f64; 4]) -> Result<(), Error> {
        let op = "Metal.RenderPassDescriptor.set_attachments";
        self.ensure_live(op)?;
        let compatible = |t: &Texture| !t.is_destroyed() && t.device().same(&self.0.device) &&
            t.descriptor().width as u64 == self.0.width && t.descriptor().height as u64 == self.0.height &&
            t.descriptor().sample_count as u64 == self.0.samples;
        let render_target = |t: &Texture| t.descriptor().usage.contains(&crate::native::resource::TextureUsage::RenderTarget);
        if !compatible(color) || !render_target(color) || (color.descriptor().format == ogpu_core::types::TextureFormat::Depth32Float) ||
            depth.is_some_and(|x| !compatible(x) || !render_target(x) || x.descriptor().format != ogpu_core::types::TextureFormat::Depth32Float) ||
            stencil.is_some() || !clear.iter().all(|v| v.is_finite()) {
            return Err(error(op, ErrorKind::InvalidArgument, "attachment format, usage, geometry, device, or lifetime is incompatible"));
        }
        sys::set_render_pass_attachments(&self.0.raw, color.raw(), depth.map(Texture::raw), stencil.map(Texture::raw), clear).map_err(|failure| sys_error(op, failure))?;
        let mut old = self.0.attachments.lock().unwrap();
        for t in [Some(color), depth, stencil].into_iter().flatten() { t.lifetime().attach(); }
        for t in [&old.0, &old.1, &old.2].into_iter().flatten() { t.lifetime().detach(); }
        *old = (Some(color.clone()), depth.cloned(), stencil.cloned());
        Ok(())
    }
    pub fn set_actions(&self, color_load: LoadAction, color_store: StoreAction, depth_load: LoadAction, depth_store: StoreAction, clear_depth: f64, stencil_load: LoadAction, stencil_store: StoreAction, clear_stencil: u32) -> Result<(), Error> {
        let op = "Metal.RenderPassDescriptor.set_actions";
        self.ensure_live(op)?;
        if !clear_depth.is_finite() || !(0.0..=1.0).contains(&clear_depth) || clear_stencil > 255
            || !matches!(depth_store, StoreAction::DontCare | StoreAction::Store)
            || !matches!(stencil_store, StoreAction::DontCare | StoreAction::Store)
        {
            return Err(error(op, ErrorKind::InvalidArgument, "invalid depth or stencil actions"));
        }
        sys::set_color_load(&self.0.raw, load_code(color_load)).map_err(|failure| sys_error(op, failure))?;
        sys::set_color_store(&self.0.raw, color_store as i32).map_err(|failure| sys_error(op, failure))?;
        sys::set_depth_stencil_actions(&self.0.raw, load_code(depth_load), depth_store as i32, clear_depth, load_code(stencil_load), stencil_store as i32, clear_stencil).map_err(|failure| sys_error(op, failure))
    }
    pub fn destroy(&self) -> Result<(), Error> { self.0.lifetime.destroy_parent("Metal.RenderPassDescriptor.destroy")?; self.0.release_attachments(); self.0.detach_device(); Ok(()) }
    pub fn width(&self) -> u64 { self.0.width }
    pub fn height(&self) -> u64 { self.0.height }
    pub fn sample_count(&self) -> u64 { self.0.samples }
    pub fn raw(&self) -> &metal_sys::Handle { &self.0.raw }
    pub fn device(&self) -> &Device { &self.0.device }
    pub(crate) fn lifetime(&self) -> &Lifetime { &self.0.lifetime }
    pub fn ensure_live(&self, operation: &str) -> Result<(), Error> { self.0.lifetime.ensure_live(operation) }
    pub fn color_attachment(&self) -> Option<Texture> { self.0.attachments.lock().unwrap().0.clone() }
    pub fn depth_attachment(&self) -> Option<Texture> { self.0.attachments.lock().unwrap().1.clone() }
    pub fn stencil_attachment(&self) -> Option<Texture> { self.0.attachments.lock().unwrap().2.clone() }
}

// ogpu_metal: the multisample resolve target of colour attachment 0.
impl RenderPassDescriptor {
    pub fn set_resolve_texture(&self, texture: Option<&Texture>) -> Result<(), Error> {
        let op = "Metal.RenderPassDescriptor.set_resolve_texture";
        self.ensure_live(op)?;
        if let Some(t) = texture {
            let d = t.descriptor();
            if t.is_destroyed() { return Err(error(op, ErrorKind::StaleHandle, "resolve texture is destroyed")); }
            if self.0.samples <= 1 { return Err(error(op, ErrorKind::InvalidArgument, "resolve texture requires a multisample render pass")); }
            if d.width as u64 != self.0.width || d.height as u64 != self.0.height || d.sample_count != 1 || !d.usage.contains(&crate::native::resource::TextureUsage::RenderTarget) {
                return Err(error(op, ErrorKind::InvalidArgument, "resolve texture dimensions, samples, or usage are incompatible"));
            }
            if !t.device().same(&self.0.device) { return Err(error(op, ErrorKind::CrossDevice, "resolve texture belongs to another device")); }
        }
        sys::set_render_pass_resolve(&self.0.raw, texture.map(Texture::raw)).map_err(|failure| sys_error(op, failure))?;
        let mut old = self.0.resolve.lock().unwrap();
        if let Some(t) = texture { t.lifetime().attach(); }
        if let Some(t) = old.as_ref() { t.lifetime().detach(); }
        *old = texture.cloned();
        Ok(())
    }
    pub fn resolve_texture(&self) -> Option<Texture> { self.0.resolve.lock().unwrap().clone() }
}
