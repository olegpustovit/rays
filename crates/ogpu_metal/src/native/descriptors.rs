pub use metal_sys::native::pixel_format_code;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

use crate::native::resource::{error, sys_error, enter_main, Device, Error, ErrorKind, Lifetime, Texture};
use metal_sys::native::descriptors as sys;

pub use ogpu_core::types::SamplerFilter as Filter;
pub use ogpu_core::types::MipFilter;
pub use ogpu_core::types::AddressMode;
pub use ogpu_core::render_pass::Comparison as CompareFunction;
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum StencilOperation { Keep, Zero, Replace, IncrementClamp, DecrementClamp, Invert, IncrementWrap, DecrementWrap }
pub use ogpu_core::validation::{BorderColor, ReductionMode, SamplerProfile as SamplerDescriptor, AdmittedSampler};
fn compare(v: CompareFunction) -> i32 { v as i32 }
fn operation(v: StencilOperation) -> i32 { v as i32 }

struct SamplerInner { raw: metal_sys::Handle, lifetime: Lifetime, device: Device, device_attached: AtomicBool }
impl SamplerInner { fn detach_device(&self) { if self.device_attached.swap(false, Ordering::AcqRel) { self.device.lifetime().detach(); } } }
impl Drop for SamplerInner { fn drop(&mut self) { self.lifetime.destroyed.store(true, Ordering::Release); self.detach_device(); } }
#[derive(Clone)]
pub struct Sampler(Arc<SamplerInner>);
impl Sampler {
    pub fn create(device: &Device, d: &AdmittedSampler) -> Result<Self, Error> {
        let op = "Metal.Sampler.create";
        enter_main(op)?; device.ensure_live(op)?;
        let raw = sys::create_sampler(device.raw(), d).map_err(|failure| sys_error(op, failure))?;
        device.lifetime().attach(); Ok(Self(Arc::new(SamplerInner { raw, lifetime: Lifetime::new(), device: device.clone(), device_attached: AtomicBool::new(true) })))
    }
    pub fn destroy(&self) -> Result<(), Error> { self.0.lifetime.destroy_parent("Metal.Sampler.destroy")?; self.0.detach_device(); Ok(()) }
    pub(crate) fn raw(&self) -> &metal_sys::Handle { &self.0.raw }
    pub(crate) fn lifetime(&self) -> &Lifetime { &self.0.lifetime }
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
        enter_main(op)?; device.ensure_live(op)?; ogpu_core::validation::validate_label(op, d.label.as_deref())?;
        let raw_desc = sys::DepthStencilDescriptor { depth_compare_function: compare(d.depth_compare), depth_write_enabled: d.depth_write as u8, front_face_stencil: d.front_face.map(face).unwrap_or_default(), has_front_face_stencil: d.front_face.is_some() as u8, back_face_stencil: d.back_face.map(face).unwrap_or_default(), has_back_face_stencil: d.back_face.is_some() as u8 };
        let raw = sys::create_depth_stencil(device.raw(), &raw_desc, d.label.as_deref()).map_err(|failure| sys_error(op, failure))?; device.lifetime().attach(); Ok(Self(Arc::new(DepthStencilInner { raw, lifetime: Lifetime::new(), device: device.clone(), device_attached: AtomicBool::new(true) })))
    }
    pub fn destroy(&self) -> Result<(), Error> { self.0.lifetime.destroy_parent("Metal.DepthStencil.destroy")?; self.0.detach_device(); Ok(()) }
    pub(crate) fn raw(&self) -> &metal_sys::Handle { &self.0.raw }
    pub(crate) fn lifetime(&self) -> &Lifetime { &self.0.lifetime }
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
pub use ogpu_core::render_pass::AttachmentStore as StoreAction;
impl RenderPassDescriptor {
    pub fn create(device: &Device, extent: &ogpu_core::validation::AdmittedTexture) -> Result<Self, Error> {
        let op = "Metal.RenderPassDescriptor.create";
        let (width,height,sample_count) = (extent.width as u64,extent.height as u64,extent.sample_count as u64);
        enter_main(op)?; device.ensure_live(op)?; let raw = sys::create_render_pass().map_err(|failure| sys_error(op, failure))?; if let Err(failure) = sys::set_render_pass_sizes(&raw, extent).map_err(|failure| sys_error(op, failure)) { drop(raw); return Err(failure); } device.lifetime().attach(); Ok(Self(Arc::new(RenderPassInner { raw, lifetime: Lifetime::new(), device: device.clone(), device_attached: AtomicBool::new(true), width, height, samples: sample_count, attachments: std::sync::Mutex::new((None,None,None)), resolve: std::sync::Mutex::new(None) })))
    }
    pub fn set_attachments(&self, color: &Texture, depth: Option<&Texture>, stencil: Option<&Texture>, clear: &ogpu_core::render_pass::ClearColor) -> Result<(), Error> {
        let op = "Metal.RenderPassDescriptor.set_attachments";
        if stencil.is_some() { return Err(error(op, ErrorKind::Unsupported, "stencil texture formats are not exposed")); }
        sys::set_render_pass_attachments(&self.0.raw, color.raw(), depth.map(Texture::raw), stencil.map(Texture::raw), clear).map_err(|failure| sys_error(op, failure))?;
        let mut old = self.0.attachments.lock().unwrap();
        for t in [Some(color), depth, stencil].into_iter().flatten() { t.lifetime().attach(); }
        for t in [&old.0, &old.1, &old.2].into_iter().flatten() { t.lifetime().detach(); }
        *old = (Some(color.clone()), depth.cloned(), stencil.cloned());
        Ok(())
    }
    pub fn set_actions(&self, actions: &ogpu_core::render_pass::AdmittedActions) -> Result<(), Error> {
        sys::set_render_pass_actions(&self.0.raw, actions).map_err(|failure| sys_error("Metal.RenderPassDescriptor.set_actions", failure))
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
        sys::set_render_pass_resolve(&self.0.raw, texture.map(Texture::raw)).map_err(|failure| sys_error(op, failure))?;
        let mut old = self.0.resolve.lock().unwrap();
        if let Some(t) = texture { t.lifetime().attach(); }
        if let Some(t) = old.as_ref() { t.lifetime().detach(); }
        *old = texture.cloned();
        Ok(())
    }
    pub fn resolve_texture(&self) -> Option<Texture> { self.0.resolve.lock().unwrap().clone() }
}
