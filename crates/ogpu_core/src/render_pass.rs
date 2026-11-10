#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Load {
    Load,
    Clear,
    DontCare,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Store {
    Store,
    Discard,
    Resolve,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Rect {
    pub x: i64,
    pub y: i64,
    pub width: i64,
    pub height: i64,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Cull {
    CullNone,
    CullFront,
    CullBack,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Comparison {
    Never,
    Less,
    Equal,
    LessEqual,
    Greater,
    NotEqual,
    GreaterEqual,
    Always,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum StencilOperation { Keep, Zero, Replace, IncrementClamp, DecrementClamp, Invert, IncrementWrap, DecrementWrap }
#[derive(Clone, Copy, Debug)]
pub struct StencilFace { pub compare: Comparison, pub stencil_fail: StencilOperation, pub depth_fail: StencilOperation, pub pass: StencilOperation, pub read_mask: u32, pub write_mask: u32 }
#[derive(Clone, Debug)]
pub struct DepthStencilDescriptor { pub depth_compare: Comparison, pub depth_write: bool, pub front_face: Option<StencilFace>, pub back_face: Option<StencilFace>, pub label: Option<String> }
impl Default for DepthStencilDescriptor { fn default() -> Self { Self { depth_compare: Comparison::Always, depth_write: false, front_face: None, back_face: None, label: None } } }

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Primitive {
    PointList,
    LineList,
    TriangleList,
    TriangleStrip,
    LineStrip,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IndexType {
    Uint16,
    Uint32,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum AttachmentStore { DontCare, Store, MultisampleResolve, StoreAndMultisampleResolve }
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ClearColor([f64;4]);
impl ClearColor {
    pub const BLACK: Self = Self([0.,0.,0.,1.]);
    pub fn array(self) -> [f64;4] { self.0 }
}
pub fn admit_clear_color(color: [f64;4]) -> Result<ClearColor, crate::error::Error> {
    if color.iter().all(|v| v.is_finite()) { Ok(ClearColor(color)) }
    else { Err(crate::error::make("Ogpu.RenderPass.admit_clear_color", crate::error::ErrorKind::InvalidArgument, "clear color is not finite")) }
}
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Actions {
    pub color_load: Load, pub color_store: AttachmentStore,
    pub depth_load: Load, pub depth_store: AttachmentStore, pub clear_depth: f64,
    pub stencil_load: Load, pub stencil_store: AttachmentStore, pub clear_stencil: u32,
}
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct AdmittedActions(Actions);
impl std::ops::Deref for AdmittedActions { type Target = Actions; fn deref(&self) -> &Actions { &self.0 } }
impl AdmittedActions {
    pub const PRESENT: Self = Self(Actions { color_load: Load::Clear, color_store: AttachmentStore::Store,
        depth_load: Load::DontCare, depth_store: AttachmentStore::DontCare, clear_depth: 1.,
        stencil_load: Load::DontCare, stencil_store: AttachmentStore::DontCare, clear_stencil: 0 });
}
pub fn admit_actions(a: Actions) -> Result<AdmittedActions, crate::error::Error> {
    if !a.clear_depth.is_finite() || !(0. ..=1.).contains(&a.clear_depth) || a.clear_stencil > 255
        || !matches!(a.depth_store, AttachmentStore::DontCare | AttachmentStore::Store)
        || !matches!(a.stencil_store, AttachmentStore::DontCare | AttachmentStore::Store) {
        Err(crate::error::make("Ogpu.RenderPass.admit_actions",crate::error::ErrorKind::InvalidArgument,"invalid render-pass actions"))
    } else { Ok(AdmittedActions(a)) }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Viewport { pub x:f64,pub y:f64,pub width:f64,pub height:f64,pub znear:f64,pub zfar:f64 }
#[derive(Clone, Copy, Debug)]
pub struct AdmittedViewport(ViewportSource);
#[derive(Clone, Copy, Debug)]
enum ViewportSource { Points(Rect), Native(Viewport) }
impl AdmittedViewport {
    pub fn viewport(self)->Viewport { match self.0 { ViewportSource::Native(v)=>v,ViewportSource::Points(r)=>Viewport {
        x:r.x as f64,y:r.y as f64,width:r.width as f64,height:r.height as f64,znear:0.,zfar:1. } } }
    pub fn points(self)->Option<Rect> { match self.0 { ViewportSource::Points(r)=>Some(r),_=>None } }
}
pub fn admit_viewport(v:Viewport)->Result<AdmittedViewport,crate::error::Error>{
    if ![v.x,v.y,v.width,v.height,v.znear,v.zfar].iter().all(|n|n.is_finite()) || v.width<=0. || v.height<=0.
        || !(0. ..=1.).contains(&v.znear) || !(0. ..=1.).contains(&v.zfar) || v.znear>v.zfar {
        Err(crate::error::make("Ogpu.RenderPass.admit_viewport",crate::error::ErrorKind::InvalidArgument,"viewport is non-finite, empty, or outside the depth range"))
    } else { Ok(AdmittedViewport(ViewportSource::Native(v))) }
}
pub fn admit_viewport_points(r:Rect)->Result<AdmittedViewport,crate::error::Error>{
    if r.width<=0 || r.height<=0 { Err(crate::error::make("Backend.set_viewport",crate::error::ErrorKind::InvalidArgument,"viewport is empty")) }
    else { Ok(AdmittedViewport(ViewportSource::Points(r))) }
}
