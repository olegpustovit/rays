use crate::{
    acceleration::{self, Acceleration, BufferRange, Descriptor as PortableDescriptor, Geometry as AccelGeometry},
    caps::{self, Caps, Feature},
    error::{self, Error, ErrorKind},
    handle::{self, Device as DeviceHandle, ResourceHandle},
    pipeline::{self, Blend, RenderDescriptor},
    render_pass::{Comparison, Cull, IndexType, Load, Primitive, Rect, Store},
    shader::{Binding, ConstantValue, Shader},
    surface::{self, Configuration},
    types::{self, BufferDescriptor, BufferUsage, Extent, Memory, Origin, SamplerDescriptor, TextureDescriptor, TextureFormat, TextureUsage},
};
pub use crate::acceleration::InstanceKind;
use std::{cell::{Cell, RefCell}, rc::{Rc, Weak}, sync::Arc};

pub type Token = i64;
pub type Floats = Rc<RefCell<Vec<f32>>>;

/// Owns the mapped allocation, not a snapshot. Invoke only after GPU completion and while
/// the caller guarantees exclusive immutable access; semantic free still ends normal use.
/// Destroying a buffer releases its base reader and mapping callbacks. An escaped reader retains
/// only its allocation independently, so keeping the dead buffer wrapper does not retain native data.
/// It never accesses owner-thread OGPU queues, encoders or SDL state. A device-local backend may
/// perform a native, device-lock-serialized staged transfer; shared-memory backends only read
/// immutable CPU-visible storage. Capturing this handle performs neither transfer nor byte copy.
pub type OwnedReadback = Arc<dyn Fn(i64, usize) -> Result<Vec<u8>, Error> + Send + Sync>;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Receipt { pub epoch: i64 }

pub struct DriverResource {
    pub token: Token,
    pub write: Box<dyn Fn(i64, &[u8]) -> Result<(), Error>>,
    pub read: Box<dyn Fn(i64, usize) -> Result<Vec<u8>, Error>>,
    pub read_into: Box<dyn Fn(i64, &mut [u8], usize, usize) -> Result<(), Error>>,
    pub destroy: Box<dyn Fn() -> Result<(), Error>>,
}

pub struct DriverMapped {
    pub mapped_resource: DriverResource,
    pub readback: OwnedReadback,
    pub view: Box<dyn Fn() -> Option<Floats>>,
    pub release_view: Box<dyn Fn()>,
    pub flush: Box<dyn Fn(usize) -> Result<(), Error>>,
    pub invalidate: Box<dyn Fn(usize) -> Result<(), Error>>,
}
struct DriverMapping {
    readback: OwnedReadback,
    view: Box<dyn Fn() -> Option<Floats>>,
    release_view: Box<dyn Fn()>,
    flush: Box<dyn Fn(usize) -> Result<(), Error>>,
    invalidate: Box<dyn Fn(usize) -> Result<(), Error>>,
}

pub struct DriverPipeline {
    pub pipeline_token: Token,
    pub max_threads: i64,
    pub destroy_pipeline: Box<dyn Fn() -> Result<(), Error>>,
}

#[derive(Clone)]
pub struct DriverFrame { pub frame_token: Token }

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct GpuTiming { pub gpu_seconds: f64 }

pub enum DriverAcquire {
    Acquired(DriverFrame),
    Timeout,
    Occluded,
    DeviceLost,
}

pub struct DriverSurface {
    pub configure: Box<dyn Fn(&Configuration) -> Result<(), Error>>,
    pub acquire_sync: Box<dyn Fn() -> Result<DriverAcquire, Error>>,
    pub discard: Box<dyn Fn(DriverFrame) -> Result<(), Error>>,
    pub destroy_surface: Box<dyn Fn() -> Result<(), Error>>,
}

pub struct DriverLibrary {
    pub create_compute_pipeline_in: Box<dyn Fn(&str, &[(String, ConstantValue)], &[Binding]) -> Result<DriverPipeline, Error>>,
    pub destroy_library: Box<dyn Fn() -> Result<(), Error>>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct AccelSizes {
    pub structure_size: i64,
    pub build_scratch_size: i64,
    pub refit_scratch_size: i64,
}

pub type DriverKeyframe = (Token, i64);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DriverGeometry {
    Triangles { vertices: Token, offset: i64, length: i64, vertex_stride: i64, vertex_count: i64 },
    MotionTriangles { keyframes: Vec<DriverKeyframe>, vertex_stride: i64, vertex_count: i64 },
    Boxes { boxes: Vec<DriverKeyframe>, stride: i64, count: i64, opaque: bool, duplicate: bool },
    Curves { control: Vec<DriverKeyframe>, control_stride: i64, control_count: i64, radii: Vec<DriverKeyframe>, radius_stride: i64, indices: Token, index_offset: i64, segment_count: i64, per_segment: i64, curve_type: i64, basis: i64, caps: i64 },
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct DriverMotion {
    pub motion_keyframes: i64,
    pub motion_start: f64,
    pub motion_end: f64,
    pub motion_start_border: i64,
    pub motion_end_border: i64,
}

#[derive(Clone)]
pub enum DriverAccelDescriptor {
    Blas { geometries: Vec<DriverGeometry>, motion: Option<DriverMotion>, refit: bool },
    Tlas { instances: Token, offset: i64, instance_count: i64, kind: InstanceKind, structures: Vec<Token>, motion_transforms: Option<(Token, i64, i64)> },
    Sized { size: i64, template: Token },
}

pub struct DriverAccel {
    pub accel_token: Token,
    pub accel_sizes: AccelSizes,
    pub destroy_accel: Box<dyn Fn() -> Result<(), Error>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Instance {
    pub transform: Vec<f64>,
    pub mask: i64,
    pub structure_index: i64,
}

pub struct DriverComputeEncoder {
    pub set_pipeline: Box<dyn Fn(Token) -> Result<(), Error>>,
    pub set_buffer: Box<dyn Fn(i64, i64, Token) -> Result<(), Error>>,
    pub set_bytes: Box<dyn Fn(i64, &[u8]) -> Result<(), Error>>,
    pub set_texture: Box<dyn Fn(i64, Token) -> Result<(), Error>>,
    pub set_accel: Box<dyn Fn(i64, Token) -> Result<(), Error>>,
    pub dispatch_threads: Box<dyn Fn((i64, i64, i64), (i64, i64, i64)) -> Result<(), Error>>,
    pub dispatch_indirect: Box<dyn Fn(Token, i64, (i64, i64, i64)) -> Result<(), Error>>,
    pub compute_use_accels: Box<dyn Fn(&[Token]) -> Result<(), Error>>,
    pub end_compute: Box<dyn Fn() -> Result<(), Error>>,
}

pub struct DriverAccelEncoder {
    pub build: Box<dyn Fn(Token, Token, i64) -> Result<(), Error>>,
    pub compact: Box<dyn Fn(Token, Token) -> Result<(), Error>>,
    pub write_compacted_size: Box<dyn Fn(Token, Token, i64) -> Result<(), Error>>,
    pub refit: Box<dyn Fn(Token, &DriverAccelDescriptor, Token, i64) -> Result<(), Error>>,
    pub end_accel: Box<dyn Fn() -> Result<(), Error>>,
}

pub struct DriverBlitEncoder {
    pub buffer_to_texture: Box<dyn Fn(Token, i64, i64, i64, Token, i64, Origin, Extent) -> Result<(), Error>>,
    pub end_blit: Box<dyn Fn() -> Result<(), Error>>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ShaderStage { Vertex, Fragment }
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Winding { Clockwise, CounterClockwise }
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct DepthState { pub depth_compare: Comparison, pub depth_write: bool }

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct DriverColorAttachment {
    pub color: Token,
    pub color_resolve: Option<Token>,
    pub color_load: Load,
    pub color_store: Store,
    pub color_clear: (f64, f64, f64, f64),
}
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct DriverDepthAttachment {
    pub depth: Token,
    pub depth_load: Load,
    pub depth_store: Store,
    pub depth_clear: f64,
}
#[derive(Clone, Debug, PartialEq)]
pub struct DriverRenderTarget {
    pub target_colors: Vec<DriverColorAttachment>,
    pub target_depth: Option<DriverDepthAttachment>,
    pub target_width: i64,
    pub target_height: i64,
    pub target_samples: i64,
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DriverBatchDraw {
    pub batch_pipeline: Token,
    pub batch_buffers: Vec<(ShaderStage, i64, Token, i64)>,
    pub batch_primitive: Primitive,
    pub batch_index: Option<(IndexType, Token, i64, i64)>,
    pub batch_vertex_start: i64,
    pub batch_vertex_count: i64,
    pub batch_instances: i64,
}

pub struct DriverRenderEncoder {
    pub render_set_pipeline: Box<dyn Fn(Token) -> Result<(), Error>>,
    pub set_stage_buffer: Box<dyn Fn(ShaderStage, i64, i64, Token) -> Result<(), Error>>,
    pub set_stage_texture: Box<dyn Fn(ShaderStage, i64, Token) -> Result<(), Error>>,
    pub set_stage_sampler: Box<dyn Fn(ShaderStage, i64, Token) -> Result<(), Error>>,
    pub set_viewport: Box<dyn Fn(Rect) -> Result<(), Error>>,
    pub set_scissor: Box<dyn Fn(Rect) -> Result<(), Error>>,
    pub set_cull: Box<dyn Fn(Cull) -> Result<(), Error>>,
    pub set_winding: Box<dyn Fn(Winding) -> Result<(), Error>>,
    pub set_depth_state: Box<dyn Fn(Option<DepthState>) -> Result<(), Error>>,
    pub draw: Box<dyn Fn(Primitive, i64, i64, i64) -> Result<(), Error>>,
    pub draw_indexed: Box<dyn Fn(Primitive, IndexType, Token, i64, i64, i64) -> Result<(), Error>>,
    pub draw_indexed_indirect: Box<dyn Fn(Primitive, IndexType, Token, i64, Token, i64) -> Result<(), Error>>,
    pub draw_batch: Box<dyn Fn(&[DriverBatchDraw]) -> Result<(), Error>>,
    pub use_resources: Box<dyn Fn(&[Token]) -> Result<(), Error>>,
    pub execute_icb: Box<dyn Fn(Token, i64, i64) -> Result<(), Error>>,
    pub end_render: Box<dyn Fn() -> Result<(), Error>>,
}

pub struct DriverSampler { pub sampler_token: Token, pub destroy_sampler: Box<dyn Fn() -> Result<(), Error>> }

pub struct DriverIcb {
    pub icb_token: Token,
    pub icb_set_pipeline: Box<dyn Fn(i64, Token) -> Result<(), Error>>,
    pub icb_set_buffer: Box<dyn Fn(i64, ShaderStage, i64, i64, Token) -> Result<(), Error>>,
    pub icb_draw: Box<dyn Fn(i64, Primitive, i64, i64, i64) -> Result<(), Error>>,
    pub icb_draw_indexed: Box<dyn Fn(i64, Primitive, IndexType, Token, i64, i64, i64) -> Result<(), Error>>,
    pub destroy_icb: Box<dyn Fn() -> Result<(), Error>>,
}

pub struct DriverArgument {
    pub argument_length: i64,
    pub argument_alignment: i64,
    pub argument_texture: Box<dyn Fn(Token, i64, i64, Token) -> Result<(), Error>>,
    pub argument_sampler: Box<dyn Fn(Token, i64, i64, Token) -> Result<(), Error>>,
    pub destroy_argument: Box<dyn Fn() -> Result<(), Error>>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct RenderPipelineOptions { pub blend: Blend, pub topology: Primitive, pub indirect: bool }

pub struct DriverCommands {
    pub compute_encoder: Box<dyn Fn() -> Result<DriverComputeEncoder, Error>>,
    pub accel_encoder: Box<dyn Fn() -> Result<DriverAccelEncoder, Error>>,
    pub blit_encoder: Box<dyn Fn() -> Result<DriverBlitEncoder, Error>>,
    pub render_encoder: Box<dyn Fn(&DriverRenderTarget) -> Result<DriverRenderEncoder, Error>>,
    pub commit: Box<dyn Fn() -> Result<Receipt, Error>>,
    pub commit_present: Box<dyn Fn(Token, DriverFrame) -> Result<Receipt, Error>>,
    pub abandon: Box<dyn Fn() -> Result<(), Error>>,
}

pub struct DriverQueue {
    pub complete_through: Box<dyn Fn(i64) -> Result<(), Error>>,
    pub poll_through: Box<dyn Fn(i64) -> Result<bool, Error>>,
    pub completed_epoch: Box<dyn Fn() -> i64>,
    pub begin_commands: Box<dyn Fn() -> Result<DriverCommands, Error>>,
    pub gpu_duration: Box<dyn Fn(i64) -> Option<f64>>,
    pub gpu_timing: Box<dyn Fn() -> GpuTiming>,
    pub destroy_queue: Box<dyn Fn() -> Result<(), Error>>,
}

pub struct DriverDevice {
    pub device_handle: DeviceHandle,
    pub capabilities: Caps,
    pub allocated_size: Box<dyn Fn() -> i64>,
    pub recommended_working_set: Box<dyn Fn() -> i64>,
    pub create_buffer: Box<dyn Fn(Memory, &BufferDescriptor) -> Result<DriverResource, Error>>,
    pub create_mapped_buffer: Box<dyn Fn(&BufferDescriptor) -> Result<DriverMapped, Error>>,
    pub create_texture: Box<dyn Fn(&TextureDescriptor) -> Result<DriverResource, Error>>,
    pub create_depth_texture: Box<dyn Fn(&TextureDescriptor) -> Result<DriverResource, Error>>,
    pub create_memoryless_texture: Box<dyn Fn(bool, &TextureDescriptor) -> Result<DriverResource, Error>>,
    pub create_library: Box<dyn Fn(&Shader) -> Result<DriverLibrary, Error>>,
    pub create_accel: Box<dyn Fn(&DriverAccelDescriptor) -> Result<DriverAccel, Error>>,
    pub instance_layout: Box<dyn Fn(InstanceKind) -> Vec<i64>>,
    pub create_sampler: Box<dyn Fn(&SamplerDescriptor) -> Result<DriverSampler, Error>>,
    pub create_render_pipeline: Box<dyn Fn(RenderPipelineOptions, &RenderDescriptor) -> Result<DriverPipeline, Error>>,
    pub create_icb: Box<dyn Fn(i64) -> Result<DriverIcb, Error>>,
    pub create_argument: Box<dyn Fn(Token, ShaderStage, i64) -> Result<DriverArgument, Error>>,
    pub create_queue: Box<dyn Fn() -> Result<DriverQueue, Error>>,
    pub create_surface: Box<dyn Fn(&Configuration) -> Result<DriverSurface, Error>>,
    pub destroy_device: Box<dyn Fn() -> Result<(), Error>>,
}

pub struct Driver { pub create_device: Box<dyn Fn() -> Result<DriverDevice, Error>> }

/// A backend factory record; selection belongs to the public OGPU layer.
pub struct Implementation {
    pub backend: String,
    pub create_driver: Box<dyn Fn() -> Driver>,
    /// Native handles still alive. Diagnostics only.
    pub live_handles: Box<dyn Fn() -> i64>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Status { Pending, Completed }
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CurveType { RoundCurve, FlatCurve }
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CurveBasis { Bspline, CatmullRom, LinearBasis, Bezier }
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CurveCaps { NoCaps, DiskCaps, SphereCaps }
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct GeometryOptions { pub opaque: bool, pub duplicate_intersection: bool }
pub const DEFAULT_GEOMETRY_OPTIONS: GeometryOptions = GeometryOptions { opaque: false, duplicate_intersection: false };
#[derive(Clone)]
pub struct Keyframe { pub buffer: Buffer, pub offset: i64 }
#[derive(Clone)]
pub enum Geometry {
    Triangles { vertices: Buffer, offset: i64, length: i64, vertex_stride: i64, vertex_count: i64 },
    MotionTriangles { keyframes: Vec<Keyframe>, vertex_stride: i64, vertex_count: i64 },
    BoundingBoxes { boxes: Vec<Keyframe>, stride: i64, count: i64, options: GeometryOptions },
    Curves { control_points: Vec<Keyframe>, control_stride: i64, control_point_count: i64, radii: Vec<Keyframe>, radius_stride: i64, indices: Buffer, index_offset: i64, segment_count: i64, control_points_per_segment: i64, curve_type: CurveType, basis: CurveBasis, caps: CurveCaps },
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Border { Clamp, Vanish }
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Motion { pub keyframes: i64, pub start_time: f64, pub end_time: f64, pub start_border: Border, pub end_border: Border }
#[derive(Clone)]
pub enum AccelDescriptor {
    Blas { geometries: Vec<Geometry> },
    MotionBlas { geometries: Vec<Geometry>, motion: Motion },
    TlasOf { instances: Buffer, offset: i64, instance_count: i64, kind: InstanceKind, structures: Vec<Accel>, motion_transforms: Option<(Buffer, i64, i64)> },
    Sized { size: i64, template: Accel },
}
#[derive(Clone, Debug, PartialEq)]
pub struct InstanceRecord { pub instance: Instance, pub user_id: i64 }
#[derive(Clone, Debug, PartialEq)]
pub struct MotionInstance { pub record: InstanceRecord, pub transforms_start: i64, pub transforms_count: i64, pub start_time: f64, pub end_time: f64, pub start_border: Border, pub end_border: Border }

#[derive(Clone)]
pub struct Device(Rc<DeviceInner>);
struct DeviceInner {
    raw: RefCell<Option<DriverDevice>>,
    capabilities: Caps,
    handle: DeviceHandle,
    children: Cell<i64>,
    dead: Cell<bool>,
    queues: RefCell<Vec<Weak<RefCell<QueueInner>>>>,
}
#[derive(Clone)]
struct Resource(Rc<ResourceInner>);
struct ResourceInner { raw: RefCell<Option<DriverResource>>, mapping: RefCell<Option<DriverMapping>>, token: Token, handle: ResourceHandle, device: Device, dead: Cell<bool> }
#[derive(Clone)]
pub struct Buffer { resource: Resource, buffer_descriptor: BufferDescriptor, memory: Memory }
#[derive(Clone)]
pub struct Texture { resource: Resource, texture_descriptor: TextureDescriptor }
#[derive(Clone)]
pub struct Pipeline(Rc<PipelineInner>);
struct PipelineInner { driver: RefCell<Option<DriverPipeline>>, token: Token, max_threads: i64, device: Device, library: Option<Library>, indirect: bool, shape: PipelineShape, dead: Cell<bool> }
#[derive(Clone, Copy, PartialEq, Eq)]
enum PipelineShape { Vertex, Compute }
#[derive(Clone)]
pub struct Library(Rc<LibraryInner>);
struct LibraryInner { raw: RefCell<Option<DriverLibrary>>, device: Device, dead: Cell<bool>, pipelines: Cell<i64> }
#[derive(Clone)]
pub struct Queue(Rc<RefCell<QueueInner>>);
struct QueueInner { raw: RefCell<Option<DriverQueue>>, device: Device, dead: bool, completed_epoch: i64, gpu_timing: GpuTiming }
#[derive(Clone)]
pub struct Surface(Rc<SurfaceInner>);
struct SurfaceInner { raw: RefCell<Option<DriverSurface>>, device: Device, dead: Cell<bool>, frames: Cell<i64>, configuration: RefCell<Configuration> }
#[derive(Clone)]
pub struct Frame(Rc<FrameInner>);
struct FrameInner { raw: DriverFrame, surface: Surface, consumed: Cell<bool> }
#[derive(Clone)]
pub struct Accel(Rc<AccelInner>);
struct AccelInner { raw: RefCell<Option<DriverAccel>>, token: Token, sizes: AccelSizes, device: Device, portable: RefCell<Acceleration>, structures: Vec<Accel>, dead: Cell<bool> }
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum CommandsState { Recording, Encoding, Committed, Abandoned }
#[derive(Clone)]
pub struct Commands(Rc<CommandsInner>);
struct CommandsInner { raw: RefCell<Option<DriverCommands>>, queue: Queue, state: Cell<CommandsState>, encoder: RefCell<Option<ActiveEncoder>> }
enum ActiveEncoder { Compute(Weak<ComputeInner>), Accel(Weak<AccelEncoderInner>), Blit(Weak<BlitInner>), Render(Weak<RenderInner>) }
#[derive(Clone)]
pub struct ComputeEncoder(Rc<ComputeInner>);
struct ComputeInner { raw: RefCell<Option<DriverComputeEncoder>>, commands: Commands, open: Cell<bool>, pipeline_set: Cell<bool> }
#[derive(Clone)]
pub struct AccelEncoder(Rc<AccelEncoderInner>);
struct AccelEncoderInner { raw: RefCell<Option<DriverAccelEncoder>>, commands: Commands, open: Cell<bool> }
#[derive(Clone)]
pub struct BlitEncoder(Rc<BlitInner>);
struct BlitInner { raw: RefCell<Option<DriverBlitEncoder>>, commands: Commands, open: Cell<bool> }
#[derive(Clone)]
pub struct Sampler(Rc<SamplerInner>);
struct SamplerInner { raw: RefCell<Option<DriverSampler>>, token: Token, device: Device, dead: Cell<bool> }
#[derive(Clone)]
pub struct Icb(Rc<IcbInner>);
struct IcbInner { raw: RefCell<Option<DriverIcb>>, token: Token, device: Device, capacity: i64, dead: Cell<bool> }
#[derive(Clone)]
pub struct Argument(Rc<ArgumentInner>);
struct ArgumentInner { raw: RefCell<Option<DriverArgument>>, length: i64, alignment: i64, device: Device, dead: Cell<bool> }
#[derive(Clone)]
pub struct RenderEncoder(Rc<RenderInner>);
struct RenderInner { raw: RefCell<Option<DriverRenderEncoder>>, commands: Commands, open: Cell<bool>, pipeline_set: Cell<bool>, shape: Cell<PipelineShape> }

// A dead wrapper keeps identity and validation metadata, never driver callbacks/native owners.
// Access follows the existing liveness/state guards; failed transitions retain the payload.
impl DeviceInner {
    fn raw(&self) -> std::cell::Ref<'_, DriverDevice> { std::cell::Ref::map(self.raw.borrow(), |raw| raw.as_ref().expect("live device payload")) }
}
impl ResourceInner {
    fn raw(&self) -> std::cell::Ref<'_, DriverResource> { std::cell::Ref::map(self.raw.borrow(), |raw| raw.as_ref().expect("live resource payload")) }
}
impl PipelineInner {
    fn driver(&self) -> std::cell::Ref<'_, DriverPipeline> { std::cell::Ref::map(self.driver.borrow(), |raw| raw.as_ref().expect("live pipeline payload")) }
}
impl LibraryInner {
    fn raw(&self) -> std::cell::Ref<'_, DriverLibrary> { std::cell::Ref::map(self.raw.borrow(), |raw| raw.as_ref().expect("live library payload")) }
}
impl QueueInner {
    fn raw(&self) -> std::cell::Ref<'_, DriverQueue> { std::cell::Ref::map(self.raw.borrow(), |raw| raw.as_ref().expect("live queue payload")) }
}
impl SurfaceInner {
    fn raw(&self) -> std::cell::Ref<'_, DriverSurface> { std::cell::Ref::map(self.raw.borrow(), |raw| raw.as_ref().expect("live surface payload")) }
}
impl AccelInner {
    fn raw(&self) -> std::cell::Ref<'_, DriverAccel> { std::cell::Ref::map(self.raw.borrow(), |raw| raw.as_ref().expect("live acceleration payload")) }
}
impl CommandsInner {
    fn raw(&self) -> std::cell::Ref<'_, DriverCommands> { std::cell::Ref::map(self.raw.borrow(), |raw| raw.as_ref().expect("recording commands payload")) }
}
impl ComputeInner {
    fn raw(&self) -> std::cell::Ref<'_, DriverComputeEncoder> { std::cell::Ref::map(self.raw.borrow(), |raw| raw.as_ref().expect("open compute encoder payload")) }
}
impl AccelEncoderInner {
    fn raw(&self) -> std::cell::Ref<'_, DriverAccelEncoder> { std::cell::Ref::map(self.raw.borrow(), |raw| raw.as_ref().expect("open acceleration encoder payload")) }
}
impl BlitInner {
    fn raw(&self) -> std::cell::Ref<'_, DriverBlitEncoder> { std::cell::Ref::map(self.raw.borrow(), |raw| raw.as_ref().expect("open blit encoder payload")) }
}
impl SamplerInner {
    fn raw(&self) -> std::cell::Ref<'_, DriverSampler> { std::cell::Ref::map(self.raw.borrow(), |raw| raw.as_ref().expect("live sampler payload")) }
}
impl IcbInner {
    fn raw(&self) -> std::cell::Ref<'_, DriverIcb> { std::cell::Ref::map(self.raw.borrow(), |raw| raw.as_ref().expect("live indirect command payload")) }
}
impl ArgumentInner {
    fn raw(&self) -> std::cell::Ref<'_, DriverArgument> { std::cell::Ref::map(self.raw.borrow(), |raw| raw.as_ref().expect("live argument payload")) }
}
impl RenderInner {
    fn raw(&self) -> std::cell::Ref<'_, DriverRenderEncoder> { std::cell::Ref::map(self.raw.borrow(), |raw| raw.as_ref().expect("open render encoder payload")) }
}
#[derive(Clone)]
pub struct ColorAttachment { pub texture: Texture, pub resolve: Option<Texture>, pub load: Load, pub store: Store, pub clear: (f64, f64, f64, f64) }
#[derive(Clone)]
pub struct DepthAttachment { pub depth_texture: Texture, pub depth_load: Load, pub depth_store: Store, pub depth_clear: f64 }
#[derive(Clone, Default)]
pub struct RenderTarget { pub colors: Vec<ColorAttachment>, pub depth: Option<DepthAttachment> }
#[derive(Clone)]
pub struct BatchDraw {
    pub pipeline: Pipeline,
    pub buffers: Vec<(ShaderStage, i64, Buffer, i64)>,
    pub primitive: Primitive,
    pub index: Option<(IndexType, Buffer, i64, i64)>,
    pub vertex_start: i64,
    pub vertex_count: i64,
    pub instances: i64,
}

fn make_error(operation: &str, kind: ErrorKind, message: &str) -> Error { error::make(operation, kind, message) }
fn live(operation: &str, device: &Device) -> Result<(), Error> {
    if device.0.dead.get() { Err(make_error(operation, ErrorKind::StaleHandle, "device is destroyed")) } else { Ok(()) }
}
fn make_resource(device: &Device, raw: DriverResource) -> Resource {
    Resource(Rc::new(ResourceInner { token: raw.token, raw: RefCell::new(Some(raw)), mapping: RefCell::new(None), handle: handle::create(&device.0.handle), device: device.clone(), dead: Cell::new(false) }))
}
fn resource_check(operation: &str, device: &Device, resource: &Resource) -> Result<(), Error> {
    if resource.0.dead.get() { Err(make_error(operation, ErrorKind::StaleHandle, "resource is destroyed")) }
    else if !Rc::ptr_eq(&resource.0.device.0, &device.0) { Err(make_error(operation, ErrorKind::CrossDevice, "resource belongs to another device")) }
    else { Ok(()) }
}
fn accel_check(operation: &str, device: &Device, value: &Accel) -> Result<(), Error> {
    if value.0.dead.get() { Err(make_error(operation, ErrorKind::StaleHandle, "acceleration structure is destroyed")) }
    else if !Rc::ptr_eq(&value.0.device.0, &device.0) { Err(make_error(operation, ErrorKind::CrossDevice, "acceleration structure belongs to another device")) }
    else { Ok(()) }
}
fn require(operation: &str, device: &Device, feature: Feature) -> Result<(), Error> { caps::require(Some(operation), &device.0.capabilities, &feature) }
fn check_slot(operation: &str, index: i64) -> Result<(), Error> {
    if !(0..=31).contains(&index) { Err(make_error(operation, ErrorKind::InvalidArgument, "binding slot is out of range")) } else { Ok(()) }
}
fn finite(value: f64) -> bool { value.is_finite() && value.abs() <= f32::MAX as f64 }
fn border_code(border: Border) -> i64 { match border { Border::Clamp => 0, Border::Vanish => 1 } }
fn is_built(value: &Accel) -> bool { acceleration::built(&value.0.portable.borrow()) }
pub fn create_device(driver: Driver) -> Result<Device, Error> {
    let raw = (driver.create_device)()?;
    caps::validate(&raw.capabilities)?;
    let handle = raw.device_handle.clone();
    Ok(Device(Rc::new(DeviceInner {
        capabilities: raw.capabilities, raw: RefCell::new(Some(raw)), handle, children: Cell::new(0), dead: Cell::new(false), queues: RefCell::new(Vec::new()),
    })))
}

pub fn capabilities(device: &Device) -> Caps { device.0.capabilities }
pub fn device_handle(device: &Device) -> &DeviceHandle { &device.0.handle }
pub fn allocated_size(device: &Device) -> i64 { device.0.raw.borrow().as_ref().map_or(0, |raw| (raw.allocated_size)()) }
pub fn recommended_working_set(device: &Device) -> i64 { device.0.raw.borrow().as_ref().map_or(0, |raw| (raw.recommended_working_set)()) }

pub fn create_buffer(device: &Device, descriptor: BufferDescriptor, memory: Option<Memory>) -> Result<Buffer, Error> {
    const OP: &str = "Backend.create_buffer";
    live(OP, device)?;
    crate::validation::validate_buffer(&device.0.capabilities, &descriptor)?;
    let memory = memory.unwrap_or(Memory::Shared);
    if memory == Memory::Memoryless {
        return Err(make_error(OP, ErrorKind::InvalidArgument, "a buffer cannot be memoryless"));
    }
    let raw = (device.0.raw().create_buffer)(memory, &descriptor)?;
    device.0.children.set(device.0.children.get() + 1);
    Ok(Buffer { resource: make_resource(device, raw), buffer_descriptor: descriptor, memory })
}

pub fn create_mapped_buffer(device: &Device, descriptor: BufferDescriptor) -> Result<Buffer, Error> {
    const OP: &str = "Backend.create_mapped_buffer";
    live(OP, device)?;
    crate::validation::validate_buffer(&device.0.capabilities, &descriptor)?;
    if descriptor.size % 4 != 0 {
        return Err(make_error(OP, ErrorKind::InvalidArgument, "mapped buffer size must be a multiple of 4"));
    }
    let mapped = (device.0.raw().create_mapped_buffer)(&descriptor)?;
    let DriverMapped { mapped_resource, readback, view, release_view, flush, invalidate } = mapped;
    let mapping = DriverMapping { readback, view, release_view, flush, invalidate };
    let resource = make_resource(device, mapped_resource);
    *resource.0.mapping.borrow_mut() = Some(mapping);
    device.0.children.set(device.0.children.get() + 1);
    Ok(Buffer { resource, buffer_descriptor: descriptor, memory: Memory::Shared })
}

fn sync_mapped(buffer: &Buffer, operation: &str, length: i64, invalidate: bool) -> Result<(), Error> {
    if buffer.resource.0.dead.get() { return Err(make_error(operation, ErrorKind::StaleHandle, "buffer is destroyed")); }
    let mapping = buffer.resource.0.mapping.borrow();
    let Some(mapped) = mapping.as_ref() else { return Err(make_error(operation, ErrorKind::InvalidArgument, "buffer is not mapped")); };
    if length < 0 || length > buffer.buffer_descriptor.size {
        return Err(make_error(operation, ErrorKind::InvalidArgument, "range exceeds the buffer"));
    }
    let Ok(length) = usize::try_from(length) else { return Err(make_error(operation, ErrorKind::InvalidArgument, "range exceeds the buffer")); };
    if invalidate { (mapped.invalidate)(length) } else { (mapped.flush)(length) }
}

pub fn flush_mapped(buffer: &Buffer, length: i64) -> Result<(), Error> {
    sync_mapped(buffer, "Backend.flush_mapped", length, false)
}
pub fn invalidate_mapped(buffer: &Buffer, length: i64) -> Result<(), Error> {
    sync_mapped(buffer, "Backend.invalidate_mapped", length, true)
}

/// Capture a lazy reader without reading or retaining the owner-thread buffer wrapper.
/// Callers must establish GPU completion and exclusive immutable access before invoking it.
pub fn owned_readback(buffer: &Buffer) -> Result<OwnedReadback, Error> {
    const OP: &str = "Backend.owned_readback";
    live(OP, &buffer.resource.0.device)?;
    resource_check(OP, &buffer.resource.0.device, &buffer.resource)?;
    let mapping = buffer.resource.0.mapping.borrow();
    let Some(mapping) = mapping.as_ref() else {
        return Err(make_error(OP, ErrorKind::InvalidArgument, "buffer is not mapped"));
    };
    let size = buffer.buffer_descriptor.size;
    let readback = mapping.readback.clone();
    Ok(Arc::new(move |offset, length| {
        if offset < 0 || offset > size || u64::try_from(length).map_or(true, |length| length > (size - offset) as u64) {
            return Err(make_error(OP, ErrorKind::InvalidArgument, "range exceeds the buffer"));
        }
        readback(offset, length)
    }))
}
pub fn mapped_view(buffer: &Buffer) -> Option<Floats> {
    if buffer.resource.0.dead.get() { None } else { buffer.resource.0.mapping.borrow().as_ref().and_then(|mapping| (mapping.view)()) }
}
pub fn release_mapped_view(buffer: &Buffer) -> Result<(), Error> {
    const OP: &str = "Backend.release_mapped_view";
    if buffer.resource.0.dead.get() { Err(make_error(OP, ErrorKind::StaleHandle, "buffer is destroyed")) }
    else if let Some(mapping) = buffer.resource.0.mapping.borrow().as_ref() { (mapping.release_view)(); Ok(()) }
    else { Err(make_error(OP, ErrorKind::InvalidArgument, "buffer is not mapped")) }
}
pub fn buffer_size(buffer: &Buffer) -> i64 { buffer.buffer_descriptor.size }

pub fn create_texture(device: &Device, descriptor: TextureDescriptor) -> Result<Texture, Error> {
    const OP: &str = "Backend.create_texture";
    live(OP, device)?;
    crate::validation::validate_texture(&device.0.capabilities, &descriptor)?;
    let raw = (device.0.raw().create_texture)(&descriptor)?;
    device.0.children.set(device.0.children.get() + 1);
    Ok(Texture { resource: make_resource(device, raw), texture_descriptor: descriptor })
}
pub fn create_depth_texture(device: &Device, descriptor: TextureDescriptor) -> Result<Texture, Error> {
    const OP: &str = "Backend.create_depth_texture";
    live(OP, device)?;
    crate::validation::validate_texture(&device.0.capabilities, &descriptor)?;
    if !descriptor.usage.contains(&TextureUsage::RenderAttachment)
        || descriptor.usage.iter().any(|usage| *usage != TextureUsage::RenderAttachment)
    {
        return Err(make_error(OP, ErrorKind::InvalidArgument, "depth textures are render-attachment only"));
    }
    let raw = (device.0.raw().create_depth_texture)(&descriptor)?;
    device.0.children.set(device.0.children.get() + 1);
    Ok(Texture { resource: make_resource(device, raw), texture_descriptor: descriptor })
}
pub fn create_memoryless_texture(device: &Device, depth: bool, descriptor: TextureDescriptor) -> Result<Texture, Error> {
    const OP: &str = "Backend.create_memoryless_texture";
    live(OP, device)?;
    require(OP, device, Feature::MemorylessAttachments)?;
    crate::validation::validate_texture(&device.0.capabilities, &descriptor)?;
    if descriptor.usage.as_slice() != &[TextureUsage::RenderAttachment] {
        return Err(make_error(OP, ErrorKind::InvalidArgument, "memoryless textures are render-attachment only"));
    }
    let raw = (device.0.raw().create_memoryless_texture)(depth, &descriptor)?;
    device.0.children.set(device.0.children.get() + 1);
    Ok(Texture { resource: make_resource(device, raw), texture_descriptor: descriptor })
}

pub fn create_queue(device: &Device) -> Result<Queue, Error> {
    const OP: &str = "Backend.create_queue";
    live(OP, device)?;
    let raw = (device.0.raw().create_queue)()?;
    device.0.children.set(device.0.children.get() + 1);
    let queue = Queue(Rc::new(RefCell::new(QueueInner { raw: RefCell::new(Some(raw)), device: device.clone(), dead: false, completed_epoch: 0, gpu_timing: GpuTiming { gpu_seconds: 0.0 } })));
    device.0.queues.borrow_mut().push(Rc::downgrade(&queue.0));
    Ok(queue)
}
pub fn create_surface(device: &Device, configuration: Configuration) -> Result<Surface, Error> {
    const OP: &str = "Backend.create_surface";
    live(OP, device)?;
    let portable = surface::create(&device.0.handle, configuration.clone())?;
    surface::destroy(&portable);
    let raw = (device.0.raw().create_surface)(&configuration)?;
    device.0.children.set(device.0.children.get() + 1);
    Ok(Surface(Rc::new(SurfaceInner {
        raw: RefCell::new(Some(raw)), device: device.clone(), dead: Cell::new(false), frames: Cell::new(0),
        configuration: RefCell::new(configuration),
    })))
}
pub fn buffer_id(buffer: &Buffer) -> i64 { handle::id(&buffer.resource.0.handle) }
pub fn texture_id(texture: &Texture) -> i64 { handle::id(&texture.resource.0.handle) }
pub fn write_buffer(buffer: &Buffer, offset: i64, bytes: &[u8]) -> Result<(), Error> {
    if buffer.resource.0.dead.get() { Err(make_error("Backend.write_buffer", ErrorKind::StaleHandle, "buffer is destroyed")) }
    else if buffer.memory == Memory::DeviceLocal { Err(make_error("Backend.write_buffer", ErrorKind::Unsupported, "device-local buffers are not host writable")) }
    else { (buffer.resource.0.raw().write)(offset, bytes) }
}
pub fn read_buffer(buffer: &Buffer, offset: i64, length: i64) -> Result<Vec<u8>, Error> {
    if buffer.resource.0.dead.get() { return Err(make_error("Backend.read_buffer", ErrorKind::StaleHandle, "buffer is destroyed")); }
    if buffer.memory == Memory::DeviceLocal { return Err(make_error("Backend.read_buffer", ErrorKind::Unsupported, "device-local buffers are not host readable")); }
    if length < 0 { return Err(make_error("Backend.read_buffer", ErrorKind::InvalidArgument, "read length is negative")); }
    let Ok(length) = usize::try_from(length) else { return Err(make_error("Backend.read_buffer", ErrorKind::InvalidArgument, "read length is invalid")); };
    (buffer.resource.0.raw().read)(offset, length)
}
pub fn read_texture(texture: &Texture, bytes_per_row: i64) -> Result<Vec<u8>, Error> {
    const OP: &str = "Backend.read_texture";
    let d = &texture.texture_descriptor;
    if texture.resource.0.dead.get() { return Err(make_error(OP, ErrorKind::StaleHandle, "texture is destroyed")); }
    if bytes_per_row <= 0 || d.height < 0 || d.height > i64::MAX / bytes_per_row {
        return Err(make_error(OP, ErrorKind::InvalidArgument, "row pitch is invalid"));
    }
    let Ok(length) = usize::try_from(bytes_per_row * d.height) else { return Err(make_error(OP, ErrorKind::InvalidArgument, "row pitch is invalid")); };
    (texture.resource.0.raw().read)(0, length)
}
pub fn read_texture_into(texture: &Texture, bytes_per_row: i64, destination: &mut [u8]) -> Result<(), Error> {
    const OP: &str = "Backend.read_texture_into";
    let d = &texture.texture_descriptor;
    if texture.resource.0.dead.get() { return Err(make_error(OP, ErrorKind::StaleHandle, "texture is destroyed")); }
    if bytes_per_row <= 0 || d.height < 0 || d.height > i64::MAX / bytes_per_row {
        return Err(make_error(OP, ErrorKind::InvalidArgument, "row pitch is invalid"));
    }
    let Ok(length) = usize::try_from(bytes_per_row * d.height) else { return Err(make_error(OP, ErrorKind::InvalidArgument, "row pitch is invalid")); };
    if destination.len() != length { return Err(make_error(OP, ErrorKind::InvalidArgument, "destination length does not match texture extent")); }
    (texture.resource.0.raw().read_into)(0, destination, 0, length)
}
pub fn complete_through(queue: &Queue, epoch: i64) -> Result<(), Error> {
    let q = queue.0.borrow();
    if q.dead { Err(make_error("Backend.complete_through", ErrorKind::StaleHandle, "queue is destroyed")) }
    else { (q.raw().complete_through)(epoch) }
}
pub fn poll_through(queue: &Queue, epoch: i64) -> Result<bool, Error> {
    let q = queue.0.borrow();
    if q.dead { Err(make_error("Backend.poll_through", ErrorKind::StaleHandle, "queue is destroyed")) }
    else { (q.raw().poll_through)(epoch) }
}
pub fn completed_epoch(queue: &Queue) -> i64 {
    let q = queue.0.borrow();
    if q.dead { q.completed_epoch } else { (q.raw().completed_epoch)() }
}
pub fn status(queue: &Queue, receipt: Receipt) -> Result<Status, Error> {
    if poll_through(queue, receipt.epoch)? { Ok(Status::Completed) } else { Ok(Status::Pending) }
}
pub fn configure(surface: &Surface, configuration: Configuration) -> Result<(), Error> {
    const OP: &str = "Backend.configure";
    if surface.0.dead.get() { return Err(make_error(OP, ErrorKind::StaleHandle, "surface is destroyed")); }
    if surface.0.frames.get() != 0 { return Err(make_error(OP, ErrorKind::InvalidState, "surface has acquired frames")); }
    (surface.0.raw().configure)(&configuration)?;
    *surface.0.configuration.borrow_mut() = configuration;
    Ok(())
}
pub fn acquire_sync(surface: &Surface) -> Result<AcquireResult, Error> {
    const OP: &str = "Backend.acquire_sync";
    if surface.0.dead.get() { return Err(make_error(OP, ErrorKind::StaleHandle, "surface is destroyed")); }
    match (surface.0.raw().acquire_sync)()? {
        DriverAcquire::Acquired(raw) => {
            surface.0.frames.set(surface.0.frames.get() + 1);
            Ok(AcquireResult::Acquired(Frame(Rc::new(FrameInner { raw, surface: surface.clone(), consumed: Cell::new(false) }))))
        }
        DriverAcquire::Timeout => Ok(AcquireResult::Timeout),
        DriverAcquire::Occluded => Ok(AcquireResult::Occluded),
        DriverAcquire::DeviceLost => Ok(AcquireResult::DeviceLost),
    }
}
#[derive(Clone)]
pub enum AcquireResult { Acquired(Frame), Timeout, Occluded, DeviceLost }

pub fn discard(frame: &Frame) -> Result<(), Error> {
    const OP: &str = "Backend.discard";
    if frame.0.consumed.get() {
        return Err(make_error(OP, ErrorKind::InvalidState, "frame is already consumed"));
    }
    let surface = &frame.0.surface;
    if surface.0.dead.get() {
        return Err(make_error(OP, ErrorKind::StaleHandle, "surface is destroyed"));
    }
    (surface.0.raw().discard)(frame.0.raw.clone())?;
    frame.0.consumed.set(true);
    surface.0.frames.set(surface.0.frames.get() - 1);
    Ok(())
}

fn destroy_resource(resource: &Resource) -> Result<(), Error> {
    if resource.0.dead.get() { return Ok(()); }
    (resource.0.raw().destroy)()?;
    resource.0.dead.set(true);
    resource.0.raw.borrow_mut().take();
    resource.0.mapping.borrow_mut().take();
    handle::destroy(&resource.0.handle);
    resource.0.device.0.children.set(resource.0.device.0.children.get() - 1);
    Ok(())
}
pub fn destroy_buffer(buffer: &Buffer) -> Result<(), Error> { destroy_resource(&buffer.resource) }
pub fn destroy_texture(texture: &Texture) -> Result<(), Error> { destroy_resource(&texture.resource) }
pub fn destroy_pipeline(pipeline: &Pipeline) -> Result<(), Error> {
    let p = &pipeline.0;
    if p.dead.get() { return Ok(()); }
    (p.driver().destroy_pipeline)()?;
    p.dead.set(true);
    p.driver.borrow_mut().take();
    p.device.0.children.set(p.device.0.children.get() - 1);
    if let Some(library) = &p.library { library.0.pipelines.set(library.0.pipelines.get() - 1); }
    Ok(())
}
pub fn destroy_queue(queue: &Queue) -> Result<(), Error> {
    let mut q = queue.0.borrow_mut();
    if q.dead { return Ok(()); }
    let completed_epoch = (q.raw().completed_epoch)();
    let gpu_timing = (q.raw().gpu_timing)();
    (q.raw().destroy_queue)()?;
    q.dead = true;
    q.completed_epoch = completed_epoch;
    q.gpu_timing = gpu_timing;
    q.raw.borrow_mut().take();
    let current = Rc::downgrade(&queue.0);
    q.device.0.queues.borrow_mut().retain(|entry| !Weak::ptr_eq(entry, &current));
    q.device.0.children.set(q.device.0.children.get() - 1);
    Ok(())
}
pub fn destroy_surface(surface: &Surface) -> Result<(), Error> {
    if surface.0.dead.get() { return Ok(()); }
    if surface.0.frames.get() != 0 { return Err(make_error("Backend.destroy_surface", ErrorKind::InvalidState, "surface has acquired frames")); }
    (surface.0.raw().destroy_surface)()?;
    surface.0.dead.set(true);
    surface.0.raw.borrow_mut().take();
    surface.0.device.0.children.set(surface.0.device.0.children.get() - 1);
    Ok(())
}
pub fn destroy_device(device: &Device) -> Result<(), Error> {
    if device.0.dead.get() { return Ok(()); }
    if device.0.children.get() != 0 { return Err(make_error("Backend.destroy_device", ErrorKind::InvalidState, "device has live children")); }
    (device.0.raw().destroy_device)()?;
    device.0.dead.set(true);
    device.0.raw.borrow_mut().take();
    handle::destroy_device(&device.0.handle);
    Ok(())
}
pub fn texture_driver_token(texture: &Texture) -> (i64, i64) {
    (handle::device_id(&texture.resource.0.device.0.handle), texture.resource.0.token)
}
pub fn texture_descriptor(texture: &Texture) -> &TextureDescriptor { &texture.texture_descriptor }
pub fn texture_destroyed(texture: &Texture) -> bool { texture.resource.0.dead.get() }

pub fn create_library(device: &Device, shader: &Shader) -> Result<Library, Error> {
    const OP: &str = "Backend.create_library";
    live(OP, device)?;
    let raw = (device.0.raw().create_library)(shader)?;
    device.0.children.set(device.0.children.get() + 1);
    Ok(Library(Rc::new(LibraryInner { raw: RefCell::new(Some(raw)), device: device.clone(), dead: Cell::new(false), pipelines: Cell::new(0) })))
}
pub fn create_compute_pipeline_from(
    library: &Library,
    entry: &str,
    constants: Option<Vec<(String, ConstantValue)>>,
    interface: &[Binding],
) -> Result<Pipeline, Error> {
    const OP: &str = "Backend.create_compute_pipeline_from";
    if library.0.dead.get() { return Err(make_error(OP, ErrorKind::StaleHandle, "library is destroyed")); }
    live(OP, &library.0.device)?;
    require(OP, &library.0.device, Feature::ComputePipeline)?;
    if entry.is_empty() || entry.as_bytes().contains(&0) {
        return Err(make_error(OP, ErrorKind::InvalidArgument, "entry name is empty or contains NUL"));
    }
    crate::shader::validate_bindings(interface)?;
    let constants = constants.unwrap_or_default();
    crate::shader::validate_constants(&constants)?;
    let raw = (library.0.raw().create_compute_pipeline_in)(entry, &constants, interface)?;
    let device = library.0.device.clone();
    device.0.children.set(device.0.children.get() + 1);
    library.0.pipelines.set(library.0.pipelines.get() + 1);
    Ok(Pipeline(Rc::new(PipelineInner {
        token: raw.pipeline_token, max_threads: raw.max_threads, driver: RefCell::new(Some(raw)), device, library: Some(library.clone()), indirect: false, shape: PipelineShape::Compute, dead: Cell::new(false),
    })))
}
pub fn destroy_library(library: &Library) -> Result<(), Error> {
    const OP: &str = "Backend.destroy_library";
    if library.0.dead.get() { return Ok(()); }
    if library.0.pipelines.get() > 0 { return Err(make_error(OP, ErrorKind::InvalidState, "library has live pipelines")); }
    (library.0.raw().destroy_library)()?;
    library.0.dead.set(true);
    library.0.raw.borrow_mut().take();
    library.0.device.0.children.set(library.0.device.0.children.get() - 1);
    Ok(())
}
pub fn max_total_threads(pipeline: &Pipeline) -> i64 { pipeline.0.max_threads }

fn valid_instance(instance: &Instance) -> bool {
    instance.transform.len() == 12 && instance.transform.iter().all(|value| finite(*value))
        && (0..=0xffff_ffff).contains(&instance.mask) && instance.structure_index >= 0
}
fn valid_record(record: &InstanceRecord) -> bool {
    valid_instance(&record.instance) && (0..=0xffff_ffff).contains(&record.user_id)
}
pub fn pack_instance_records(device: &Device, records: &[InstanceRecord]) -> Result<Vec<u8>, Error> {
    const OP: &str = "Backend.pack_instance_records";
    live(OP, device)?;
    if records.is_empty() { return Err(make_error(OP, ErrorKind::InvalidArgument, "instance array is empty")); }
    if !records.iter().all(valid_record) {
        return Err(make_error(OP, ErrorKind::InvalidArgument, "instance transform, mask, index, or user id is invalid"));
    }
    let layout = (device.0.raw().instance_layout)(InstanceKind::UserIdInstances);
    let (mut bytes, size) = acceleration::record_bytes(&layout, records.len());
    assert!(size != 0, "instance record size is zero");
    for (index, record) in records.iter().enumerate() {
        acceleration::pack_instance_record_into(
            &layout,
            &mut bytes,
            index * size,
            &record.instance.transform,
            record.instance.mask,
            record.instance.structure_index,
            record.user_id,
        );
    }
    Ok(bytes)
}
pub fn pack_motion_instances(device: &Device, instances: &[MotionInstance]) -> Result<Vec<u8>, Error> {
    const OP: &str = "Backend.pack_motion_instances";
    live(OP, device)?;
    if instances.is_empty() { return Err(make_error(OP, ErrorKind::InvalidArgument, "instance array is empty")); }
    if !instances.iter().all(|m| {
        valid_record(&m.record) && m.transforms_start >= 0 && m.transforms_count >= 2
            && m.start_time.is_finite() && m.end_time.is_finite() && m.start_time < m.end_time
    }) {
        return Err(make_error(OP, ErrorKind::InvalidArgument, "motion instance record, transform range, or time range is invalid"));
    }
    let layout = (device.0.raw().instance_layout)(InstanceKind::MotionInstances);
    let (mut bytes, size) = acceleration::record_bytes(&layout, instances.len());
    assert!(size != 0, "instance record size is zero");
    for (index, instance) in instances.iter().enumerate() {
        acceleration::pack_motion_instance_into(
            &layout,
            &mut bytes,
            index * size,
            instance.record.instance.mask,
            instance.record.instance.structure_index,
            instance.record.user_id,
            instance.transforms_start,
            instance.transforms_count,
            border_code(instance.start_border),
            border_code(instance.end_border),
            instance.start_time,
            instance.end_time,
        );
    }
    Ok(bytes)
}
pub fn pack_transforms(transforms: &[Vec<f64>]) -> Result<Vec<u8>, Error> {
    const OP: &str = "Backend.pack_transforms";
    if transforms.is_empty() { return Err(make_error(OP, ErrorKind::InvalidArgument, "transform array is empty")); }
    if !transforms.iter().all(|transform| transform.len() == 12 && transform.iter().all(|v| finite(*v))) {
        return Err(make_error(OP, ErrorKind::InvalidArgument, "transforms must be finite 3x4 rows"));
    }
    Ok(acceleration::pack_transforms(transforms))
}

fn range(buffer: &Buffer, offset: i64, length: i64) -> BufferRange {
    BufferRange { buffer: buffer.resource.0.handle.clone(), buffer_size: buffer.buffer_descriptor.size, offset, length }
}
fn span(stride: i64, count: i64) -> i64 {
    if count <= 0 || stride <= 0 || count > i64::MAX / stride { 0 } else { count * stride }
}
fn keyframe_ranges(stride: i64, count: i64, keyframes: &[Keyframe]) -> Vec<BufferRange> {
    keyframes.iter().map(|keyframe| range(&keyframe.buffer, keyframe.offset, span(stride, count))).collect()
}
fn portable_geometry(geometry: &Geometry) -> AccelGeometry {
    match geometry {
        Geometry::Triangles { vertices, offset, length, vertex_stride, vertex_count } =>
            AccelGeometry::Triangles { vertices: range(vertices, *offset, *length), vertex_stride: *vertex_stride, vertex_count: *vertex_count },
        Geometry::MotionTriangles { keyframes, vertex_stride, vertex_count } =>
            AccelGeometry::MotionTriangles { keyframes: keyframe_ranges(*vertex_stride, *vertex_count, keyframes), vertex_stride: *vertex_stride, vertex_count: *vertex_count },
        Geometry::BoundingBoxes { boxes, stride, count, .. } =>
            AccelGeometry::BoundingBoxes { boxes: keyframe_ranges(*stride, *count, boxes), stride: *stride, count: *count },
        Geometry::Curves { control_points, control_stride, control_point_count, radii, radius_stride, indices, index_offset, segment_count, control_points_per_segment, .. } =>
            AccelGeometry::Curves {
                control_points: keyframe_ranges(*control_stride, *control_point_count, control_points),
                control_stride: *control_stride, control_point_count: *control_point_count,
                radii: keyframe_ranges(*radius_stride, *control_point_count, radii), radius_stride: *radius_stride,
                indices: range(indices, *index_offset, span(4, *segment_count)), segment_count: *segment_count,
                control_points_per_segment: *control_points_per_segment,
            },
    }
}
fn portable_of(device: &Device, descriptor: &AccelDescriptor) -> PortableDescriptor {
    match descriptor {
        AccelDescriptor::Blas { geometries } =>
            PortableDescriptor::Blas { geometries: geometries.iter().map(portable_geometry).collect(), motion_keyframes: None },
        AccelDescriptor::MotionBlas { geometries, motion } =>
            PortableDescriptor::Blas { geometries: geometries.iter().map(portable_geometry).collect(), motion_keyframes: Some(motion.keyframes) },
        AccelDescriptor::TlasOf { instances, offset, instance_count, kind, structures, .. } => {
            let stride = (device.0.raw().instance_layout)(*kind).first().copied().unwrap_or(0);
            PortableDescriptor::Tlas {
                instances: range(instances, *offset, span(stride, *instance_count)),
                instance_stride: stride, instance_count: *instance_count,
                instance_kind: *kind,
                structures: structures.iter().map(|structure| acceleration::handle(&structure.0.portable.borrow())).collect(),
            }
        }
        AccelDescriptor::Sized { size, template } =>
            PortableDescriptor::Sized { size: *size, template: acceleration::handle(&template.0.portable.borrow()) },
    }
}
fn driver_keyframes(keyframes: &[Keyframe]) -> Vec<DriverKeyframe> {
    keyframes.iter().map(|keyframe| (keyframe.buffer.resource.0.token, keyframe.offset)).collect()
}
fn driver_geometry(geometry: &Geometry) -> DriverGeometry {
    match geometry {
        Geometry::Triangles { vertices, offset, length, vertex_stride, vertex_count } =>
            DriverGeometry::Triangles { vertices: vertices.resource.0.token, offset: *offset, length: *length, vertex_stride: *vertex_stride, vertex_count: *vertex_count },
        Geometry::MotionTriangles { keyframes, vertex_stride, vertex_count } =>
            DriverGeometry::MotionTriangles { keyframes: driver_keyframes(keyframes), vertex_stride: *vertex_stride, vertex_count: *vertex_count },
        Geometry::BoundingBoxes { boxes, stride, count, options } =>
            DriverGeometry::Boxes { boxes: driver_keyframes(boxes), stride: *stride, count: *count, opaque: options.opaque, duplicate: options.duplicate_intersection },
        Geometry::Curves { control_points, control_stride, control_point_count, radii, radius_stride, indices, index_offset, segment_count, control_points_per_segment, curve_type, basis, caps } =>
            DriverGeometry::Curves {
                control: driver_keyframes(control_points), control_stride: *control_stride, control_count: *control_point_count,
                radii: driver_keyframes(radii), radius_stride: *radius_stride,
                indices: indices.resource.0.token, index_offset: *index_offset, segment_count: *segment_count,
                per_segment: *control_points_per_segment,
                curve_type: match curve_type { CurveType::RoundCurve => 0, CurveType::FlatCurve => 1 },
                basis: match basis { CurveBasis::Bspline => 0, CurveBasis::CatmullRom => 1, CurveBasis::LinearBasis => 2, CurveBasis::Bezier => 3 },
                caps: match caps { CurveCaps::NoCaps => 0, CurveCaps::DiskCaps => 1, CurveCaps::SphereCaps => 2 },
            },
    }
}
fn driver_of(refit: bool, descriptor: &AccelDescriptor) -> DriverAccelDescriptor {
    match descriptor {
        AccelDescriptor::Blas { geometries } =>
            DriverAccelDescriptor::Blas { geometries: geometries.iter().map(driver_geometry).collect(), motion: None, refit },
        AccelDescriptor::MotionBlas { geometries, motion } =>
            DriverAccelDescriptor::Blas {
                geometries: geometries.iter().map(driver_geometry).collect(),
                motion: Some(DriverMotion {
                    motion_keyframes: motion.keyframes, motion_start: motion.start_time, motion_end: motion.end_time,
                    motion_start_border: border_code(motion.start_border), motion_end_border: border_code(motion.end_border),
                }), refit,
            },
        AccelDescriptor::TlasOf { instances, offset, instance_count, kind, structures, motion_transforms } =>
            DriverAccelDescriptor::Tlas {
                instances: instances.resource.0.token, offset: *offset, instance_count: *instance_count, kind: *kind,
                structures: structures.iter().map(|structure| structure.0.token).collect(),
                motion_transforms: motion_transforms.as_ref().map(|(buffer, offset, count)| (buffer.resource.0.token, *offset, *count)),
            },
        AccelDescriptor::Sized { size, template } =>
            DriverAccelDescriptor::Sized { size: *size, template: template.0.token },
    }
}
fn descriptor_buffers(descriptor: &AccelDescriptor) -> Vec<Buffer> {
    let mut buffers = Vec::new();
    match descriptor {
        AccelDescriptor::Blas { geometries } | AccelDescriptor::MotionBlas { geometries, .. } => {
            for geometry in geometries {
                match geometry {
                    Geometry::Triangles { vertices, .. } => buffers.push(vertices.clone()),
                    Geometry::MotionTriangles { keyframes, .. } => buffers.extend(keyframes.iter().map(|k| k.buffer.clone())),
                    Geometry::BoundingBoxes { boxes, .. } => buffers.extend(boxes.iter().map(|k| k.buffer.clone())),
                    Geometry::Curves { control_points, radii, indices, .. } => {
                        buffers.extend(control_points.iter().chain(radii).map(|k| k.buffer.clone()));
                        buffers.push(indices.clone());
                    }
                }
            }
        }
        AccelDescriptor::TlasOf { instances, motion_transforms, .. } => {
            buffers.push(instances.clone());
            if let Some((buffer, _, _)) = motion_transforms { buffers.push(buffer.clone()); }
        }
        AccelDescriptor::Sized { .. } => {}
    }
    buffers
}
fn descriptor_structures(descriptor: &AccelDescriptor) -> Vec<Accel> {
    match descriptor {
        AccelDescriptor::TlasOf { structures, .. } => structures.clone(),
        AccelDescriptor::Sized { template, .. } => vec![template.clone()],
        _ => Vec::new(),
    }
}
pub fn accel_sizes(value: &Accel) -> AccelSizes { value.0.sizes }
pub fn create_accel(device: &Device, descriptor: AccelDescriptor, refit: bool) -> Result<Accel, Error> {
    const OP: &str = "Backend.create_accel";
    live(OP, device)?;
    require(OP, device, Feature::RayTracing)?;
    if refit { require(OP, device, Feature::RayTracingRefit)?; }
    let uses_curves = match &descriptor {
        AccelDescriptor::Blas { geometries } | AccelDescriptor::MotionBlas { geometries, .. } =>
            geometries.iter().any(|geometry| matches!(geometry, Geometry::Curves { .. })),
        _ => false,
    };
    let uses_motion = matches!(&descriptor, AccelDescriptor::MotionBlas { .. })
        || matches!(&descriptor, AccelDescriptor::TlasOf { kind: InstanceKind::MotionInstances, .. });
    if uses_motion { require(OP, device, Feature::RayTracingMotion)?; }
    if uses_curves { require(OP, device, Feature::RayTracingCurves)?; }
    for buffer in descriptor_buffers(&descriptor) { resource_check(OP, device, &buffer.resource)?; }
    let structures = descriptor_structures(&descriptor);
    for structure in &structures {
        accel_check(OP, device, structure)?;
        if matches!(&descriptor, AccelDescriptor::TlasOf { .. })
            && matches!(acceleration::descriptor(&structure.0.portable.borrow()), PortableDescriptor::Tlas { .. })
        {
            return Err(make_error(OP, ErrorKind::InvalidArgument, "TLAS instances must reference bottom-level structures"));
        }
    }
    let motion_valid = match &descriptor {
        AccelDescriptor::MotionBlas { motion, .. } =>
            motion.keyframes >= 2 && motion.start_time.is_finite() && motion.end_time.is_finite() && motion.start_time < motion.end_time,
        AccelDescriptor::TlasOf { kind: InstanceKind::MotionInstances, motion_transforms: None, .. } => false,
        AccelDescriptor::TlasOf { motion_transforms: Some((_, offset, count)), .. } => *offset >= 0 && *count > 0,
        _ => true,
    };
    if !motion_valid { return Err(make_error(OP, ErrorKind::InvalidArgument, "motion keyframes, time range, or transform range is invalid")); }
    let portable_descriptor = portable_of(device, &descriptor);
    let portable = acceleration::create(&device.0.handle, true, refit, portable_descriptor)?;
    let raw = match (device.0.raw().create_accel)(&driver_of(refit, &descriptor)) {
        Ok(raw) => raw,
        Err(error) => { acceleration::destroy(&portable); return Err(error); }
    };
    device.0.children.set(device.0.children.get() + 1);
    Ok(Accel(Rc::new(AccelInner { token: raw.accel_token, sizes: raw.accel_sizes, raw: RefCell::new(Some(raw)), device: device.clone(), portable: RefCell::new(portable), structures, dead: Cell::new(false) })))
}
pub fn destroy_accel(value: &Accel) -> Result<(), Error> {
    if value.0.dead.get() { return Ok(()); }
    (value.0.raw().destroy_accel)()?;
    value.0.dead.set(true);
    value.0.raw.borrow_mut().take();
    acceleration::destroy(&value.0.portable.borrow());
    value.0.device.0.children.set(value.0.device.0.children.get() - 1);
    Ok(())
}

fn recording(operation: &str, commands: &Commands) -> Result<(), Error> {
    match commands.0.state.get() {
        CommandsState::Recording => Ok(()),
        CommandsState::Encoding => Err(make_error(operation, ErrorKind::InvalidState, "an encoder is still open")),
        CommandsState::Committed => Err(make_error(operation, ErrorKind::InvalidState, "commands are already committed")),
        CommandsState::Abandoned => Err(make_error(operation, ErrorKind::InvalidState, "commands were abandoned")),
    }
}
fn commands_device(commands: &Commands) -> Device { commands.0.queue.0.borrow().device.clone() }
pub fn begin_commands(queue: &Queue) -> Result<Commands, Error> {
    const OP: &str = "Backend.begin_commands";
    let q = queue.0.borrow();
    if q.device.0.dead.get() { return Err(make_error(OP, ErrorKind::StaleHandle, "device is destroyed")); }
    if q.dead { return Err(make_error(OP, ErrorKind::StaleHandle, "queue is destroyed")); }
    let raw = (q.raw().begin_commands)()?;
    Ok(Commands(Rc::new(CommandsInner { raw: RefCell::new(Some(raw)), queue: queue.clone(), state: Cell::new(CommandsState::Recording), encoder: RefCell::new(None) })))
}
pub fn compute_encoder(commands: &Commands) -> Result<ComputeEncoder, Error> {
    const OP: &str = "Backend.compute_encoder";
    recording(OP, commands)?;
    let device = commands_device(commands);
    require(OP, &device, Feature::ComputePipeline)?;
    let raw = (commands.0.raw().compute_encoder)()?;
    commands.0.state.set(CommandsState::Encoding);
    let encoder = ComputeEncoder(Rc::new(ComputeInner { raw: RefCell::new(Some(raw)), commands: commands.clone(), open: Cell::new(true), pipeline_set: Cell::new(false) }));
    *commands.0.encoder.borrow_mut() = Some(ActiveEncoder::Compute(Rc::downgrade(&encoder.0)));
    Ok(encoder)
}
fn open_compute(operation: &str, encoder: &ComputeEncoder) -> Result<(), Error> {
    if encoder.0.open.get() { Ok(()) } else { Err(make_error(operation, ErrorKind::InvalidState, "compute encoder is ended")) }
}
pub fn set_pipeline(encoder: &ComputeEncoder, pipeline: &Pipeline) -> Result<(), Error> {
    const OP: &str = "Backend.set_pipeline";
    open_compute(OP, encoder)?;
    if pipeline.0.dead.get() { return Err(make_error(OP, ErrorKind::StaleHandle, "pipeline is destroyed")); }
    let device = commands_device(&encoder.0.commands);
    if !Rc::ptr_eq(&pipeline.0.device.0, &device.0) { return Err(make_error(OP, ErrorKind::CrossDevice, "pipeline belongs to another device")); }
    (encoder.0.raw().set_pipeline)(pipeline.0.token)?;
    encoder.0.pipeline_set.set(true);
    Ok(())
}
pub fn set_buffer(encoder: &ComputeEncoder, index: i64, buffer: &Buffer) -> Result<(), Error> {
    const OP: &str = "Backend.set_buffer";
    open_compute(OP, encoder)?;
    check_slot(OP, index)?;
    let device = commands_device(&encoder.0.commands);
    resource_check(OP, &device, &buffer.resource)?;
    if buffer.buffer_descriptor.size <= 0 { return Err(make_error(OP, ErrorKind::InvalidArgument, "buffer is empty")); }
    (encoder.0.raw().set_buffer)(index, 0, buffer.resource.0.token)
}
pub fn set_bytes(encoder: &ComputeEncoder, index: i64, bytes: &[u8]) -> Result<(), Error> {
    const OP: &str = "Backend.set_bytes";
    open_compute(OP, encoder)?;
    check_slot(OP, index)?;
    if bytes.is_empty() || bytes.len() > 4096 { return Err(make_error(OP, ErrorKind::InvalidArgument, "inline bytes must be 1..4096 bytes")); }
    (encoder.0.raw().set_bytes)(index, bytes)
}
pub fn set_texture(encoder: &ComputeEncoder, index: i64, texture: &Texture) -> Result<(), Error> {
    const OP: &str = "Backend.set_texture";
    open_compute(OP, encoder)?;
    check_slot(OP, index)?;
    resource_check(OP, &commands_device(&encoder.0.commands), &texture.resource)?;
    if !texture.texture_descriptor.usage.iter().any(|usage| matches!(usage, TextureUsage::TextureBinding | TextureUsage::StorageBinding)) {
        return Err(make_error(OP, ErrorKind::InvalidArgument, "texture lacks shader binding usage"));
    }
    (encoder.0.raw().set_texture)(index, texture.resource.0.token)
}
pub fn set_accel(encoder: &ComputeEncoder, index: i64, accel: &Accel) -> Result<(), Error> {
    const OP: &str = "Backend.set_accel";
    open_compute(OP, encoder)?;
    check_slot(OP, index)?;
    accel_check(OP, &commands_device(&encoder.0.commands), accel)?;
    if !is_built(accel) { return Err(make_error(OP, ErrorKind::InvalidState, "acceleration structure has no encoded build")); }
    (encoder.0.raw().set_accel)(index, accel.0.token)
}
fn positive3(value: (i64, i64, i64)) -> bool { value.0 > 0 && value.1 > 0 && value.2 > 0 }
pub fn dispatch_threads(encoder: &ComputeEncoder, threads: (i64, i64, i64), threadgroup: (i64, i64, i64)) -> Result<(), Error> {
    const OP: &str = "Backend.dispatch_threads";
    open_compute(OP, encoder)?;
    if !encoder.0.pipeline_set.get() { return Err(make_error(OP, ErrorKind::InvalidState, "no compute pipeline is set")); }
    if !positive3(threads) || !positive3(threadgroup) { return Err(make_error(OP, ErrorKind::InvalidArgument, "dispatch dimensions must be positive")); }
    (encoder.0.raw().dispatch_threads)(threads, threadgroup)
}
pub fn dispatch_threadgroups_indirect(
    encoder: &ComputeEncoder, arguments: &Buffer, offset: i64, threadgroup: (i64, i64, i64),
) -> Result<(), Error> {
    const OP: &str = "Backend.dispatch_threadgroups_indirect";
    open_compute(OP, encoder)?;
    if !encoder.0.pipeline_set.get() { return Err(make_error(OP, ErrorKind::InvalidState, "no compute pipeline is set")); }
    if !positive3(threadgroup) { return Err(make_error(OP, ErrorKind::InvalidArgument, "dispatch dimensions must be positive")); }
    resource_check(OP, &commands_device(&encoder.0.commands), &arguments.resource)?;
    if offset < 0 || offset % 4 != 0 || offset > arguments.buffer_descriptor.size - 12 {
        return Err(make_error(OP, ErrorKind::InvalidArgument, "indirect arguments range is invalid"));
    }
    (encoder.0.raw().dispatch_indirect)(arguments.resource.0.token, offset, threadgroup)
}
pub fn end_compute(encoder: &ComputeEncoder) -> Result<(), Error> {
    const OP: &str = "Backend.end_compute";
    open_compute(OP, encoder)?;
    (encoder.0.raw().end_compute)()?;
    encoder.0.open.set(false);
    encoder.0.raw.borrow_mut().take();
    encoder.0.commands.0.encoder.borrow_mut().take();
    encoder.0.commands.0.state.set(CommandsState::Recording);
    Ok(())
}

pub fn accel_encoder(commands: &Commands) -> Result<AccelEncoder, Error> {
    const OP: &str = "Backend.accel_encoder";
    recording(OP, commands)?;
    require(OP, &commands_device(commands), Feature::RayTracing)?;
    let raw = (commands.0.raw().accel_encoder)()?;
    commands.0.state.set(CommandsState::Encoding);
    let encoder = AccelEncoder(Rc::new(AccelEncoderInner { raw: RefCell::new(Some(raw)), commands: commands.clone(), open: Cell::new(true) }));
    *commands.0.encoder.borrow_mut() = Some(ActiveEncoder::Accel(Rc::downgrade(&encoder.0)));
    Ok(encoder)
}
fn open_accel(operation: &str, encoder: &AccelEncoder) -> Result<(), Error> {
    if encoder.0.open.get() { Ok(()) } else { Err(make_error(operation, ErrorKind::InvalidState, "acceleration encoder is ended")) }
}
fn check_scratch(operation: &str, device: &Device, scratch: &Buffer, offset: i64, required: i64) -> Result<(), Error> {
    resource_check(operation, device, &scratch.resource)?;
    if offset < 0 || offset % 256 != 0 || required < 0 || required > scratch.buffer_descriptor.size
        || offset > scratch.buffer_descriptor.size - required
    {
        Err(make_error(operation, ErrorKind::InvalidArgument, "scratch range is invalid, unaligned, or insufficient"))
    } else { Ok(()) }
}
pub fn build_accel(encoder: &AccelEncoder, accel: &Accel, scratch: &Buffer, scratch_offset: Option<i64>) -> Result<(), Error> {
    const OP: &str = "Backend.build_accel";
    open_accel(OP, encoder)?;
    let device = commands_device(&encoder.0.commands);
    if accel.0.structures.iter().any(|structure| structure.0.dead.get() || !is_built(structure)) {
        return Err(make_error(OP, ErrorKind::InvalidState, "TLAS references a destroyed or unbuilt structure"));
    }
    accel_check(OP, &device, accel)?;
    let offset = scratch_offset.unwrap_or(0);
    check_scratch(OP, &device, scratch, offset, accel_sizes(accel).build_scratch_size)?;
    acceleration::build(&device.0.handle, &mut accel.0.portable.borrow_mut())?;
    (encoder.0.raw().build)(accel.0.token, scratch.resource.0.token, offset)
}
pub fn refit_accel(
    encoder: &AccelEncoder, accel: &Accel, descriptor: AccelDescriptor, scratch: &Buffer, scratch_offset: Option<i64>,
) -> Result<(), Error> {
    const OP: &str = "Backend.refit_accel";
    let device = commands_device(&encoder.0.commands);
    require(OP, &device, Feature::RayTracingRefit)?;
    if matches!(&descriptor, AccelDescriptor::TlasOf { .. } | AccelDescriptor::Sized { .. }) {
        return Err(make_error(OP, ErrorKind::InvalidArgument, "only a primitive structure refits"));
    }
    for buffer in descriptor_buffers(&descriptor) { resource_check(OP, &device, &buffer.resource)?; }
    open_accel(OP, encoder)?;
    accel_check(OP, &device, accel)?;
    let offset = scratch_offset.unwrap_or(0);
    check_scratch(OP, &device, scratch, offset, accel_sizes(accel).refit_scratch_size)?;
    acceleration::refit(&device.0.handle, &mut accel.0.portable.borrow_mut(), portable_of(&device, &descriptor))?;
    (encoder.0.raw().refit)(accel.0.token, &driver_of(true, &descriptor), scratch.resource.0.token, offset)
}
pub fn compact_accel(encoder: &AccelEncoder, src: &Accel, dst: &Accel) -> Result<(), Error> {
    const OP: &str = "Backend.compact_accel";
    open_accel(OP, encoder)?;
    let device = commands_device(&encoder.0.commands);
    accel_check(OP, &device, src)?;
    accel_check(OP, &device, dst)?;
    if accel_sizes(dst).structure_size <= 0 { return Err(make_error(OP, ErrorKind::InvalidArgument, "destination structure is empty")); }
    if Rc::ptr_eq(&src.0, &dst.0) {
        return Err(make_error(OP, ErrorKind::InvalidState, "compaction source and destination are the same structure"));
    }
    acceleration::compact_into(
        &device.0.handle, &mut src.0.portable.borrow_mut(), &mut dst.0.portable.borrow_mut(),
    )?;
    (encoder.0.raw().compact)(src.0.token, dst.0.token)
}
pub fn write_compacted_size(encoder: &AccelEncoder, accel: &Accel, dst: &Buffer, offset: Option<i64>) -> Result<(), Error> {
    const OP: &str = "Backend.write_compacted_size";
    open_accel(OP, encoder)?;
    let device = commands_device(&encoder.0.commands);
    accel_check(OP, &device, accel)?;
    resource_check(OP, &device, &dst.resource)?;
    let offset = offset.unwrap_or(0);
    if offset < 0 || offset % 8 != 0 || offset > dst.buffer_descriptor.size - 8 {
        return Err(make_error(OP, ErrorKind::InvalidArgument, "compacted size destination needs eight aligned bytes"));
    }
    acceleration::compacted_size(&device.0.handle, &accel.0.portable.borrow())?;
    (encoder.0.raw().write_compacted_size)(accel.0.token, dst.resource.0.token, offset)
}
pub fn end_accel(encoder: &AccelEncoder) -> Result<(), Error> {
    const OP: &str = "Backend.end_accel";
    open_accel(OP, encoder)?;
    (encoder.0.raw().end_accel)()?;
    encoder.0.open.set(false);
    encoder.0.raw.borrow_mut().take();
    encoder.0.commands.0.encoder.borrow_mut().take();
    encoder.0.commands.0.state.set(CommandsState::Recording);
    Ok(())
}

pub fn blit_encoder(commands: &Commands) -> Result<BlitEncoder, Error> {
    const OP: &str = "Backend.blit_encoder";
    recording(OP, commands)?;
    let raw = (commands.0.raw().blit_encoder)()?;
    commands.0.state.set(CommandsState::Encoding);
    let encoder = BlitEncoder(Rc::new(BlitInner { raw: RefCell::new(Some(raw)), commands: commands.clone(), open: Cell::new(true) }));
    *commands.0.encoder.borrow_mut() = Some(ActiveEncoder::Blit(Rc::downgrade(&encoder.0)));
    Ok(encoder)
}
fn open_blit(operation: &str, encoder: &BlitEncoder) -> Result<(), Error> {
    if encoder.0.open.get() { Ok(()) } else { Err(make_error(operation, ErrorKind::InvalidState, "blit encoder is ended")) }
}
pub fn end_blit(encoder: &BlitEncoder) -> Result<(), Error> {
    const OP: &str = "Backend.end_blit";
    open_blit(OP, encoder)?;
    (encoder.0.raw().end_blit)()?;
    encoder.0.open.set(false);
    encoder.0.raw.borrow_mut().take();
    encoder.0.commands.0.encoder.borrow_mut().take();
    encoder.0.commands.0.state.set(CommandsState::Recording);
    Ok(())
}
pub fn commit(commands: &Commands) -> Result<Receipt, Error> {
    recording("Backend.commit", commands)?;
    let receipt = (commands.0.raw().commit)()?;
    commands.0.state.set(CommandsState::Committed);
    commands.0.raw.borrow_mut().take();
    Ok(receipt)
}
pub fn abandon(commands: &Commands) -> Result<(), Error> {
    match commands.0.state.get() {
        CommandsState::Committed => Err(make_error("Backend.abandon", ErrorKind::InvalidState, "commands are already committed")),
        CommandsState::Abandoned => Ok(()),
        CommandsState::Recording | CommandsState::Encoding => {
            (commands.0.raw().abandon)()?;
            commands.0.state.set(CommandsState::Abandoned);
            if let Some(encoder) = commands.0.encoder.borrow_mut().take() {
                match encoder {
                    ActiveEncoder::Compute(encoder) => if let Some(encoder) = encoder.upgrade() { encoder.open.set(false); encoder.raw.borrow_mut().take(); },
                    ActiveEncoder::Accel(encoder) => if let Some(encoder) = encoder.upgrade() { encoder.open.set(false); encoder.raw.borrow_mut().take(); },
                    ActiveEncoder::Blit(encoder) => if let Some(encoder) = encoder.upgrade() { encoder.open.set(false); encoder.raw.borrow_mut().take(); },
                    ActiveEncoder::Render(encoder) => if let Some(encoder) = encoder.upgrade() { encoder.open.set(false); encoder.raw.borrow_mut().take(); },
                }
            }
            commands.0.raw.borrow_mut().take();
            Ok(())
        }
    }
}
pub fn gpu_duration(queue: &Queue, receipt: Receipt) -> Option<f64> {
    let q = queue.0.borrow();
    if q.dead { None } else { (q.raw().gpu_duration)(receipt.epoch) }
}
pub fn gpu_timing(queue: &Queue) -> GpuTiming {
    let q = queue.0.borrow();
    if q.dead { q.gpu_timing } else { (q.raw().gpu_timing)() }
}
pub fn compute_use_accels(encoder: &ComputeEncoder, accels: &[Accel]) -> Result<(), Error> {
    const OP: &str = "Backend.compute_use_accels";
    open_compute(OP, encoder)?;
    let device = commands_device(&encoder.0.commands);
    let mut tokens = Vec::with_capacity(accels.len());
    for accel in accels {
        accel_check(OP, &device, accel)?;
        if !is_built(accel) { return Err(make_error(OP, ErrorKind::InvalidState, "acceleration structure has no encoded build")); }
        tokens.push(accel.0.token);
    }
    if tokens.is_empty() { Ok(()) } else { (encoder.0.raw().compute_use_accels)(&tokens) }
}

pub fn create_sampler(device: &Device, descriptor: SamplerDescriptor) -> Result<Sampler, Error> {
    const OP: &str = "Backend.create_sampler";
    live(OP, device)?;
    crate::validation::validate_sampler(&descriptor)?;
    let raw = (device.0.raw().create_sampler)(&descriptor)?;
    device.0.children.set(device.0.children.get() + 1);
    Ok(Sampler(Rc::new(SamplerInner { token: raw.sampler_token, raw: RefCell::new(Some(raw)), device: device.clone(), dead: Cell::new(false) })))
}
pub fn destroy_sampler(sampler: &Sampler) -> Result<(), Error> {
    if sampler.0.dead.get() { return Ok(()); }
    (sampler.0.raw().destroy_sampler)()?;
    sampler.0.dead.set(true);
    sampler.0.raw.borrow_mut().take();
    sampler.0.device.0.children.set(sampler.0.device.0.children.get() - 1);
    Ok(())
}
pub fn create_render_pipeline(
    device: &Device,
    descriptor: &RenderDescriptor,
    blend: Option<Blend>,
    topology: Option<Primitive>,
    indirect: Option<bool>,
) -> Result<Pipeline, Error> {
    const OP: &str = "Backend.create_render_pipeline";
    live(OP, device)?;
    require(OP, device, Feature::RenderPipeline)?;
    pipeline::create_render(&device.0.capabilities, descriptor)?;
    let topology = topology.unwrap_or(Primitive::TriangleList);
    if topology != Primitive::TriangleList && topology != Primitive::PointList {
        return Err(make_error(OP, ErrorKind::InvalidArgument, "pipeline topology class must be triangles or points"));
    }
    let indirect = indirect.unwrap_or(false);
    let options = RenderPipelineOptions { blend: blend.unwrap_or(Blend::Replace), topology, indirect };
    let raw = (device.0.raw().create_render_pipeline)(options, descriptor)?;
    device.0.children.set(device.0.children.get() + 1);
    Ok(Pipeline(Rc::new(PipelineInner {
        token: raw.pipeline_token, max_threads: raw.max_threads, driver: RefCell::new(Some(raw)), device: device.clone(), library: None, indirect, shape: PipelineShape::Vertex, dead: Cell::new(false),
    })))
}
pub fn pipeline_indirect(pipeline: &Pipeline) -> bool { pipeline.0.indirect }
pub fn create_icb(device: &Device, max_commands: i64) -> Result<Icb, Error> {
    const OP: &str = "Backend.create_icb";
    live(OP, device)?;
    if max_commands <= 0 || max_commands > 65_536 {
        return Err(make_error(OP, ErrorKind::InvalidArgument, "indirect command capacity must be in 1..65536"));
    }
    let raw = (device.0.raw().create_icb)(max_commands)?;
    device.0.children.set(device.0.children.get() + 1);
    Ok(Icb(Rc::new(IcbInner { token: raw.icb_token, raw: RefCell::new(Some(raw)), device: device.clone(), capacity: max_commands, dead: Cell::new(false) })))
}
fn check_icb(operation: &str, icb: &Icb, index: i64) -> Result<(), Error> {
    if icb.0.dead.get() { Err(make_error(operation, ErrorKind::StaleHandle, "indirect command buffer is destroyed")) }
    else if index < 0 || index >= icb.0.capacity { Err(make_error(operation, ErrorKind::InvalidArgument, "indirect command index is out of range")) }
    else { Ok(()) }
}
pub fn icb_set_pipeline(icb: &Icb, index: i64, pipeline: &Pipeline) -> Result<(), Error> {
    const OP: &str = "Backend.icb_set_pipeline";
    check_icb(OP, icb, index)?;
    if pipeline.0.dead.get() { return Err(make_error(OP, ErrorKind::StaleHandle, "pipeline is destroyed")); }
    if !Rc::ptr_eq(&pipeline.0.device.0, &icb.0.device.0) { return Err(make_error(OP, ErrorKind::CrossDevice, "pipeline belongs to another device")); }
    (icb.0.raw().icb_set_pipeline)(index, pipeline.0.token)
}
pub fn icb_set_buffer(
    icb: &Icb, index: i64, stage: ShaderStage, slot: i64, offset: Option<i64>, buffer: &Buffer,
) -> Result<(), Error> {
    const OP: &str = "Backend.icb_set_buffer";
    check_icb(OP, icb, index)?;
    check_slot(OP, slot)?;
    resource_check(OP, &icb.0.device, &buffer.resource)?;
    let offset = offset.unwrap_or(0);
    if offset < 0 || offset >= buffer.buffer_descriptor.size { return Err(make_error(OP, ErrorKind::InvalidArgument, "buffer offset is out of range")); }
    (icb.0.raw().icb_set_buffer)(index, stage, slot, offset, buffer.resource.0.token)
}
fn valid_draw(operation: &str, primitive: Primitive, first: i64, count: i64, instances: i64) -> Result<(), Error> {
    if first < 0 || count <= 0 || instances <= 0 { return Err(make_error(operation, ErrorKind::InvalidArgument, "draw range is invalid")); }
    if primitive == Primitive::TriangleList && count % 3 != 0 {
        return Err(make_error(operation, ErrorKind::InvalidArgument, "triangle-list vertex count is not divisible by three"));
    }
    if primitive == Primitive::LineList && count % 2 != 0 {
        return Err(make_error(operation, ErrorKind::InvalidArgument, "line-list vertex count is not divisible by two"));
    }
    if primitive == Primitive::TriangleStrip && count < 3 {
        return Err(make_error(operation, ErrorKind::InvalidArgument, "triangle strip requires at least three vertices"));
    }
    Ok(())
}
fn valid_indexed(
    operation: &str, device: &Device, primitive: Primitive, index_type: IndexType,
    buffer: &Buffer, offset: i64, count: i64, instances: i64,
) -> Result<(), Error> {
    resource_check(operation, device, &buffer.resource)?;
    let stride = match index_type { IndexType::Uint16 => 2, IndexType::Uint32 => 4 };
    if count <= 0 || offset < 0 || offset % stride != 0 || offset > buffer.buffer_descriptor.size
        || count > (buffer.buffer_descriptor.size - offset) / stride || instances <= 0
    {
        return Err(make_error(operation, ErrorKind::InvalidArgument, "index range is invalid"));
    }
    if primitive == Primitive::TriangleList && count % 3 != 0 {
        return Err(make_error(operation, ErrorKind::InvalidArgument, "triangle-list index count is not divisible by three"));
    }
    if primitive == Primitive::LineList && count % 2 != 0 {
        return Err(make_error(operation, ErrorKind::InvalidArgument, "line-list index count is not divisible by two"));
    }
    Ok(())
}
pub fn icb_draw(
    icb: &Icb, index: i64, primitive: Primitive, first: i64, count: i64, instances: Option<i64>,
) -> Result<(), Error> {
    const OP: &str = "Backend.icb_draw";
    check_icb(OP, icb, index)?;
    let instances = instances.unwrap_or(1);
    valid_draw(OP, primitive, first, count, instances)?;
    (icb.0.raw().icb_draw)(index, primitive, first, count, instances)
}
pub fn icb_draw_indexed(
    icb: &Icb, index: i64, primitive: Primitive, index_type: IndexType,
    buffer: &Buffer, offset: i64, count: i64, instances: Option<i64>,
) -> Result<(), Error> {
    const OP: &str = "Backend.icb_draw_indexed";
    check_icb(OP, icb, index)?;
    let instances = instances.unwrap_or(1);
    valid_indexed(OP, &icb.0.device, primitive, index_type, buffer, offset, count, instances)?;
    (icb.0.raw().icb_draw_indexed)(index, primitive, index_type, buffer.resource.0.token, offset, count, instances)
}
pub fn destroy_icb(icb: &Icb) -> Result<(), Error> {
    if icb.0.dead.get() { return Ok(()); }
    (icb.0.raw().destroy_icb)()?;
    icb.0.dead.set(true);
    icb.0.raw.borrow_mut().take();
    icb.0.device.0.children.set(icb.0.device.0.children.get() - 1);
    Ok(())
}
pub fn create_argument(pipeline: &Pipeline, stage: ShaderStage, index: i64) -> Result<Argument, Error> {
    const OP: &str = "Backend.create_argument";
    if pipeline.0.dead.get() { return Err(make_error(OP, ErrorKind::StaleHandle, "pipeline is destroyed")); }
    live(OP, &pipeline.0.device)?;
    check_slot(OP, index)?;
    let raw = (pipeline.0.device.0.raw().create_argument)(pipeline.0.token, stage, index)?;
    pipeline.0.device.0.children.set(pipeline.0.device.0.children.get() + 1);
    Ok(Argument(Rc::new(ArgumentInner { length: raw.argument_length, alignment: raw.argument_alignment, raw: RefCell::new(Some(raw)), device: pipeline.0.device.clone(), dead: Cell::new(false) })))
}
pub fn argument_length(argument: &Argument) -> i64 { argument.0.length }
pub fn argument_alignment(argument: &Argument) -> i64 { argument.0.alignment }
fn argument_target(operation: &str, argument: &Argument, buffer: &Buffer, offset: i64) -> Result<(), Error> {
    if argument.0.dead.get() { return Err(make_error(operation, ErrorKind::StaleHandle, "argument encoder is destroyed")); }
    resource_check(operation, &argument.0.device, &buffer.resource)?;
    let alignment = argument.0.alignment.max(1);
    if offset < 0 || offset % alignment != 0 || argument.0.length < 0
        || argument.0.length > buffer.buffer_descriptor.size
        || offset > buffer.buffer_descriptor.size - argument.0.length
    {
        return Err(make_error(operation, ErrorKind::InvalidArgument, "argument buffer range is invalid or unaligned"));
    }
    Ok(())
}
pub fn argument_texture(argument: &Argument, buffer: &Buffer, offset: i64, slot: i64, texture: &Texture) -> Result<(), Error> {
    const OP: &str = "Backend.argument_texture";
    argument_target(OP, argument, buffer, offset)?;
    resource_check(OP, &argument.0.device, &texture.resource)?;
    (argument.0.raw().argument_texture)(buffer.resource.0.token, offset, slot, texture.resource.0.token)
}
pub fn argument_sampler(argument: &Argument, buffer: &Buffer, offset: i64, slot: i64, sampler: &Sampler) -> Result<(), Error> {
    const OP: &str = "Backend.argument_sampler";
    argument_target(OP, argument, buffer, offset)?;
    if sampler.0.dead.get() { return Err(make_error(OP, ErrorKind::StaleHandle, "sampler is destroyed")); }
    if !Rc::ptr_eq(&sampler.0.device.0, &argument.0.device.0) { return Err(make_error(OP, ErrorKind::CrossDevice, "sampler belongs to another device")); }
    (argument.0.raw().argument_sampler)(buffer.resource.0.token, offset, slot, sampler.0.token)
}
pub fn destroy_argument(argument: &Argument) -> Result<(), Error> {
    if argument.0.dead.get() { return Ok(()); }
    (argument.0.raw().destroy_argument)()?;
    argument.0.dead.set(true);
    argument.0.raw.borrow_mut().take();
    argument.0.device.0.children.set(argument.0.device.0.children.get() - 1);
    Ok(())
}

#[derive(Clone)]
pub enum ResourceUse { Buffer(Buffer), Texture(Texture) }
fn attachment_check(operation: &str, device: &Device, texture: &Texture) -> Result<(), Error> {
    resource_check(operation, device, &texture.resource)?;
    if !texture.texture_descriptor.usage.contains(&TextureUsage::RenderAttachment) {
        Err(make_error(operation, ErrorKind::InvalidArgument, "attachment texture lacks Render_attachment usage"))
    } else { Ok(()) }
}
pub fn render_encoder(commands: &Commands, target: &RenderTarget) -> Result<RenderEncoder, Error> {
    const OP: &str = "Backend.render_encoder";
    recording(OP, commands)?;
    let device = commands_device(commands);
    require(OP, &device, Feature::RenderPipeline)?;
    if target.colors.is_empty() { return Err(make_error(OP, ErrorKind::InvalidArgument, "render target needs one color attachment")); }
    if target.colors.len() > 1 { return Err(make_error(OP, ErrorKind::Unsupported, "multiple color attachments are not exposed")); }
    let color = &target.colors[0];
    attachment_check(OP, &device, &color.texture)?;
    let width = color.texture.texture_descriptor.width;
    let height = color.texture.texture_descriptor.height;
    let samples = color.texture.texture_descriptor.sample_count;
    let same_extent = |texture: &Texture| texture.texture_descriptor.width == width && texture.texture_descriptor.height == height;
    match &color.resolve {
        None if color.store == Store::Resolve =>
            return Err(make_error(OP, ErrorKind::InvalidArgument, "resolve store needs a resolve texture")),
        Some(_) if samples == 1 =>
            return Err(make_error(OP, ErrorKind::InvalidArgument, "single-sample attachment cannot resolve")),
        Some(_) if color.store == Store::Discard =>
            return Err(make_error(OP, ErrorKind::InvalidArgument, "a resolve texture needs a Store or Resolve store action")),
        Some(resolve) => {
            attachment_check(OP, &device, resolve)?;
            if !same_extent(resolve) || resolve.texture_descriptor.sample_count != 1 {
                return Err(make_error(OP, ErrorKind::InvalidArgument, "resolve texture extent or sample count differs"));
            }
        }
        _ => {}
    }
    if let Some(depth) = &target.depth {
        attachment_check(OP, &device, &depth.depth_texture)?;
        if !same_extent(&depth.depth_texture) || depth.depth_texture.texture_descriptor.sample_count != samples {
            return Err(make_error(OP, ErrorKind::InvalidArgument, "depth attachment extent or sample count differs"));
        }
        if depth.depth_store == Store::Resolve { return Err(make_error(OP, ErrorKind::InvalidArgument, "depth attachments cannot resolve")); }
        if !depth.depth_clear.is_finite() || depth.depth_clear < 0.0 || depth.depth_clear > 1.0 {
            return Err(make_error(OP, ErrorKind::InvalidArgument, "depth clear is outside [0,1]"));
        }
    }
    if ![color.clear.0, color.clear.1, color.clear.2, color.clear.3].iter().all(|value| value.is_finite()) {
        return Err(make_error(OP, ErrorKind::InvalidArgument, "clear color is not finite"));
    }
    let driver_target = DriverRenderTarget {
        target_colors: vec![DriverColorAttachment {
            color: color.texture.resource.0.token,
            color_resolve: color.resolve.as_ref().map(|texture| texture.resource.0.token),
            color_load: color.load, color_store: color.store, color_clear: color.clear,
        }],
        target_depth: target.depth.as_ref().map(|depth| DriverDepthAttachment {
            depth: depth.depth_texture.resource.0.token,
            depth_load: depth.depth_load, depth_store: depth.depth_store, depth_clear: depth.depth_clear,
        }),
        target_width: width, target_height: height, target_samples: samples,
    };
    let raw = (commands.0.raw().render_encoder)(&driver_target)?;
    commands.0.state.set(CommandsState::Encoding);
    let encoder = RenderEncoder(Rc::new(RenderInner {
        raw: RefCell::new(Some(raw)), commands: commands.clone(), open: Cell::new(true), pipeline_set: Cell::new(false), shape: Cell::new(PipelineShape::Vertex),
    }));
    *commands.0.encoder.borrow_mut() = Some(ActiveEncoder::Render(Rc::downgrade(&encoder.0)));
    Ok(encoder)
}

fn open_render(operation: &str, encoder: &RenderEncoder) -> Result<(), Error> {
    if encoder.0.open.get() { Ok(()) } else { Err(make_error(operation, ErrorKind::InvalidState, "render encoder is ended")) }
}
fn render_device(encoder: &RenderEncoder) -> Device { commands_device(&encoder.0.commands) }
pub fn set_render_pipeline(encoder: &RenderEncoder, pipeline: &Pipeline) -> Result<(), Error> {
    const OP: &str = "Backend.set_render_pipeline";
    open_render(OP, encoder)?;
    if pipeline.0.dead.get() { return Err(make_error(OP, ErrorKind::StaleHandle, "pipeline is destroyed")); }
    if !Rc::ptr_eq(&pipeline.0.device.0, &render_device(encoder).0) {
        return Err(make_error(OP, ErrorKind::CrossDevice, "pipeline belongs to another device"));
    }
    if pipeline.0.shape == PipelineShape::Compute {
        return Err(make_error(OP, ErrorKind::InvalidArgument, "compute pipelines cannot be set on a render encoder"));
    }
    (encoder.0.raw().render_set_pipeline)(pipeline.0.token)?;
    encoder.0.pipeline_set.set(true);
    encoder.0.shape.set(pipeline.0.shape);
    Ok(())
}
pub fn set_stage_buffer(
    encoder: &RenderEncoder, stage: ShaderStage, index: i64, offset: Option<i64>, buffer: &Buffer,
) -> Result<(), Error> {
    const OP: &str = "Backend.set_stage_buffer";
    open_render(OP, encoder)?;
    check_slot(OP, index)?;
    resource_check(OP, &render_device(encoder), &buffer.resource)?;
    let offset = offset.unwrap_or(0);
    if offset < 0 || offset >= buffer.buffer_descriptor.size { return Err(make_error(OP, ErrorKind::InvalidArgument, "buffer offset is out of range")); }
    (encoder.0.raw().set_stage_buffer)(stage, index, offset, buffer.resource.0.token)
}
pub fn set_stage_texture(encoder: &RenderEncoder, stage: ShaderStage, index: i64, texture: &Texture) -> Result<(), Error> {
    const OP: &str = "Backend.set_stage_texture";
    open_render(OP, encoder)?;
    check_slot(OP, index)?;
    resource_check(OP, &render_device(encoder), &texture.resource)?;
    if !texture.texture_descriptor.usage.contains(&TextureUsage::TextureBinding) {
        return Err(make_error(OP, ErrorKind::InvalidArgument, "texture lacks Texture_binding usage"));
    }
    (encoder.0.raw().set_stage_texture)(stage, index, texture.resource.0.token)
}
pub fn set_stage_sampler(encoder: &RenderEncoder, stage: ShaderStage, index: i64, sampler: &Sampler) -> Result<(), Error> {
    const OP: &str = "Backend.set_stage_sampler";
    open_render(OP, encoder)?;
    check_slot(OP, index)?;
    if sampler.0.dead.get() { return Err(make_error(OP, ErrorKind::StaleHandle, "sampler is destroyed")); }
    if !Rc::ptr_eq(&sampler.0.device.0, &render_device(encoder).0) {
        return Err(make_error(OP, ErrorKind::CrossDevice, "sampler belongs to another device"));
    }
    (encoder.0.raw().set_stage_sampler)(stage, index, sampler.0.token)
}
pub fn set_viewport(encoder: &RenderEncoder, rect: Rect) -> Result<(), Error> {
    const OP: &str = "Backend.set_viewport";
    open_render(OP, encoder)?;
    if rect.width <= 0 || rect.height <= 0 { return Err(make_error(OP, ErrorKind::InvalidArgument, "viewport is empty")); }
    (encoder.0.raw().set_viewport)(rect)
}
fn valid_rect(operation: &str, rect: Rect) -> Result<(), Error> {
    if rect.x < 0 || rect.y < 0 || rect.width <= 0 || rect.height <= 0 {
        Err(make_error(operation, ErrorKind::InvalidArgument, "rectangle is empty or negative"))
    } else { Ok(()) }
}
pub fn set_scissor(encoder: &RenderEncoder, rect: Rect) -> Result<(), Error> {
    const OP: &str = "Backend.set_scissor";
    open_render(OP, encoder)?;
    valid_rect(OP, rect)?;
    (encoder.0.raw().set_scissor)(rect)
}
pub fn set_cull(encoder: &RenderEncoder, cull: Cull) -> Result<(), Error> {
    open_render("Backend.set_cull", encoder)?;
    (encoder.0.raw().set_cull)(cull)
}
pub fn set_winding(encoder: &RenderEncoder, winding: Winding) -> Result<(), Error> {
    open_render("Backend.set_winding", encoder)?;
    (encoder.0.raw().set_winding)(winding)
}
pub fn set_depth_state(encoder: &RenderEncoder, state: Option<DepthState>) -> Result<(), Error> {
    open_render("Backend.set_depth_state", encoder)?;
    (encoder.0.raw().set_depth_state)(state)
}
fn vertex_pipeline(operation: &str, encoder: &RenderEncoder) -> Result<(), Error> {
    if !encoder.0.pipeline_set.get() { return Err(make_error(operation, ErrorKind::InvalidState, "no render pipeline is set")); }
    if encoder.0.shape.get() != PipelineShape::Vertex {
        return Err(make_error(operation, ErrorKind::InvalidState, "vertex draws need a vertex/fragment pipeline"));
    }
    Ok(())
}
pub fn draw(
    encoder: &RenderEncoder, primitive: Primitive, first: i64, count: i64, instances: Option<i64>,
) -> Result<(), Error> {
    const OP: &str = "Backend.draw";
    open_render(OP, encoder)?;
    vertex_pipeline(OP, encoder)?;
    let instances = instances.unwrap_or(1);
    valid_draw(OP, primitive, first, count, instances)?;
    (encoder.0.raw().draw)(primitive, first, count, instances)
}
pub fn draw_indexed(
    encoder: &RenderEncoder, primitive: Primitive, index_type: IndexType, buffer: &Buffer,
    offset: i64, count: i64, instances: Option<i64>,
) -> Result<(), Error> {
    const OP: &str = "Backend.draw_indexed";
    open_render(OP, encoder)?;
    if !encoder.0.pipeline_set.get() { return Err(make_error(OP, ErrorKind::InvalidState, "no render pipeline is set")); }
    let instances = instances.unwrap_or(1);
    valid_indexed(OP, &render_device(encoder), primitive, index_type, buffer, offset, count, instances)?;
    (encoder.0.raw().draw_indexed)(primitive, index_type, buffer.resource.0.token, offset, count, instances)
}
pub fn draw_indexed_indirect(
    encoder: &RenderEncoder, primitive: Primitive, index_type: IndexType, buffer: &Buffer,
    offset: i64, arguments: &Buffer, arguments_offset: i64,
) -> Result<(), Error> {
    const OP: &str = "Backend.draw_indexed_indirect";
    open_render(OP, encoder)?;
    if !encoder.0.pipeline_set.get() { return Err(make_error(OP, ErrorKind::InvalidState, "no render pipeline is set")); }
    let device = render_device(encoder);
    resource_check(OP, &device, &buffer.resource)?;
    resource_check(OP, &device, &arguments.resource)?;
    let stride = match index_type { IndexType::Uint16 => 2, IndexType::Uint32 => 4 };
    if offset < 0 || offset % stride != 0 || offset >= buffer.buffer_descriptor.size {
        return Err(make_error(OP, ErrorKind::InvalidArgument, "index offset is invalid"));
    }
    if arguments_offset < 0 || arguments_offset % 4 != 0 || arguments_offset > arguments.buffer_descriptor.size - 20 {
        return Err(make_error(OP, ErrorKind::InvalidArgument, "indirect arguments range is invalid"));
    }
    (encoder.0.raw().draw_indexed_indirect)(
        primitive, index_type, buffer.resource.0.token, offset,
        arguments.resource.0.token, arguments_offset,
    )
}
pub fn draw_batch(encoder: &RenderEncoder, draws: &[BatchDraw]) -> Result<(), Error> {
    const OP: &str = "Backend.draw_batch";
    open_render(OP, encoder)?;
    if draws.is_empty() { return Err(make_error(OP, ErrorKind::InvalidArgument, "draw batch is empty")); }
    let device = render_device(encoder);
    // lowered into storage kept between batches, inner buffer lists included: a batch per pass
    // per frame allocated a list per draw; an error leaves the scratch to be made again
    thread_local! { static LOWERED: std::cell::RefCell<Vec<DriverBatchDraw>> = const { std::cell::RefCell::new(Vec::new()) }; }
    let mut lowered = LOWERED.with(|l| std::mem::take(&mut *l.borrow_mut()));
    for (n, draw) in draws.iter().enumerate() {
        if draw.pipeline.0.dead.get() { return Err(make_error(OP, ErrorKind::StaleHandle, "pipeline is destroyed")); }
        if !Rc::ptr_eq(&draw.pipeline.0.device.0, &device.0) {
            return Err(make_error(OP, ErrorKind::CrossDevice, "pipeline belongs to another device"));
        }
        if n == lowered.len() {
            lowered.push(DriverBatchDraw { batch_pipeline: draw.pipeline.0.token, batch_buffers: Vec::new(), batch_primitive: draw.primitive, batch_index: None, batch_vertex_start: 0, batch_vertex_count: 0, batch_instances: 0 });
        }
        let mut buffers = std::mem::take(&mut lowered[n].batch_buffers);
        buffers.clear();
        for (stage, index, buffer, offset) in &draw.buffers {
            check_slot(OP, *index)?;
            resource_check(OP, &device, &buffer.resource)?;
            if *offset < 0 || *offset >= buffer.buffer_descriptor.size {
                return Err(make_error(OP, ErrorKind::InvalidArgument, "buffer offset is out of range"));
            }
            buffers.push((*stage, *index, buffer.resource.0.token, *offset));
        }
        let index = if let Some((index_type, buffer, offset, count)) = &draw.index {
            valid_indexed(OP, &device, draw.primitive, *index_type, buffer, *offset, *count, draw.instances)?;
            Some((*index_type, buffer.resource.0.token, *offset, *count))
        } else {
            valid_draw(OP, draw.primitive, draw.vertex_start, draw.vertex_count, draw.instances)?;
            None
        };
        lowered[n] = DriverBatchDraw {
            batch_pipeline: draw.pipeline.0.token,
            batch_buffers: buffers,
            batch_primitive: draw.primitive,
            batch_index: index,
            batch_vertex_start: draw.vertex_start,
            batch_vertex_count: draw.vertex_count,
            batch_instances: draw.instances,
        };
    }
    let result = (encoder.0.raw().draw_batch)(&lowered[..draws.len()]);
    LOWERED.with(|l| *l.borrow_mut() = lowered);
    result?;
    encoder.0.pipeline_set.set(true);
    Ok(())
}
pub fn use_resources(encoder: &RenderEncoder, resources: &[ResourceUse]) -> Result<(), Error> {
    const OP: &str = "Backend.use_resources";
    open_render(OP, encoder)?;
    let device = render_device(encoder);
    let mut tokens = Vec::with_capacity(resources.len());
    for resource in resources {
        match resource {
            ResourceUse::Buffer(buffer) => { resource_check(OP, &device, &buffer.resource)?; tokens.push(buffer.resource.0.token); }
            ResourceUse::Texture(texture) => { resource_check(OP, &device, &texture.resource)?; tokens.push(texture.resource.0.token); }
        }
    }
    (encoder.0.raw().use_resources)(&tokens)
}
pub fn execute_icb(encoder: &RenderEncoder, icb: &Icb, location: i64, length: i64) -> Result<(), Error> {
    const OP: &str = "Backend.execute_icb";
    open_render(OP, encoder)?;
    if !encoder.0.pipeline_set.get() { return Err(make_error(OP, ErrorKind::InvalidState, "no render pipeline is set")); }
    if icb.0.dead.get() { return Err(make_error(OP, ErrorKind::StaleHandle, "indirect command buffer is destroyed")); }
    if !Rc::ptr_eq(&icb.0.device.0, &render_device(encoder).0) {
        return Err(make_error(OP, ErrorKind::CrossDevice, "indirect command buffer belongs to another device"));
    }
    if location < 0 || length <= 0 || location > icb.0.capacity - length {
        return Err(make_error(OP, ErrorKind::InvalidArgument, "indirect command range is out of range"));
    }
    (encoder.0.raw().execute_icb)(icb.0.token, location, length)
}
pub fn end_render(encoder: &RenderEncoder) -> Result<(), Error> {
    const OP: &str = "Backend.end_render";
    open_render(OP, encoder)?;
    (encoder.0.raw().end_render)()?;
    encoder.0.open.set(false);
    encoder.0.raw.borrow_mut().take();
    encoder.0.commands.0.encoder.borrow_mut().take();
    encoder.0.commands.0.state.set(CommandsState::Recording);
    Ok(())
}

fn blit_range(size: i64, offset: i64, length: i64) -> bool {
    offset >= 0 && length > 0 && offset <= size && length <= size - offset
}
fn mip_dimension(size: i64, mip: i64) -> i64 {
    if mip < 0 || mip >= 63 { 1 } else { (size >> mip).max(1) }
}
fn texture_range(descriptor: &TextureDescriptor, mip: i64, origin: Origin, extent: Extent) -> bool {
    mip >= 0 && mip < descriptor.mip_levels && origin.x >= 0 && origin.y >= 0 && origin.z >= 0
        && extent.width > 0 && extent.height > 0 && extent.depth > 0
        && origin.x <= mip_dimension(descriptor.width, mip) - extent.width
        && origin.y <= mip_dimension(descriptor.height, mip) - extent.height
        && origin.z <= mip_dimension(descriptor.depth, mip) - extent.depth
}
fn valid_pitches(texel_bytes: i64, extent: Extent, row: i64, image: i64) -> bool {
    let Some(min_row) = extent.width.checked_mul(texel_bytes) else { return false; };
    let Some(min_image) = row.checked_mul(extent.height) else { return false; };
    extent.height > 0 && row >= min_row && row % 256 == 0 && image >= min_image
}
fn image_footprint(image: i64, depth: i64) -> Option<i64> {
    if image <= 0 || depth <= 0 { None } else { image.checked_mul(depth) }
}
fn buffer_texture_region(
    operation: &str, buffer_usage: BufferUsage, texture_usage: TextureUsage, buffer: &Buffer,
    offset: i64, row: i64, image: i64, texture: &Texture, mip: i64, origin: Origin, extent: Extent,
) -> Result<(), Error> {
    if !buffer.buffer_descriptor.usage.contains(&buffer_usage) || !texture.texture_descriptor.usage.contains(&texture_usage) {
        return Err(make_error(operation, ErrorKind::InvalidArgument, "buffer or texture usage does not permit the copy"));
    }
    if !texture_range(&texture.texture_descriptor, mip, origin, extent)
        || !valid_pitches(types::texel_bytes(texture.texture_descriptor.format), extent, row, image)
    {
        return Err(make_error(operation, ErrorKind::InvalidArgument, "texture region or row pitch is invalid"));
    }
    if image_footprint(image, extent.depth).is_some_and(|length| blit_range(buffer.buffer_descriptor.size, offset, length)) {
        Ok(())
    } else {
        Err(make_error(operation, ErrorKind::InvalidArgument, "buffer range does not cover the texture region"))
    }
}
pub fn buffer_to_texture(
    encoder: &BlitEncoder, src: &Buffer, offset: Option<i64>, bytes_per_row: i64, bytes_per_image: i64,
    dst: &Texture, mip: Option<i64>, origin: Option<Origin>, extent: Extent,
) -> Result<(), Error> {
    const OP: &str = "Backend.buffer_to_texture";
    open_blit(OP, encoder)?;
    let device = commands_device(&encoder.0.commands);
    resource_check(OP, &device, &src.resource)?;
    resource_check(OP, &device, &dst.resource)?;
    let offset = offset.unwrap_or(0);
    let mip = mip.unwrap_or(0);
    let origin = origin.unwrap_or(Origin { x: 0, y: 0, z: 0 });
    buffer_texture_region(OP, BufferUsage::CopySrc, TextureUsage::TextureCopyDst, src, offset, bytes_per_row, bytes_per_image, dst, mip, origin, extent)?;
    (encoder.0.raw().buffer_to_texture)(
        src.resource.0.token, offset, bytes_per_row, bytes_per_image, dst.resource.0.token, mip, origin, extent,
    )
}

fn validate_present(operation: &str, queue: &Queue, source: &Texture, frame: &Frame) -> Result<(), Error> {
    if frame.0.consumed.get() { return Err(make_error(operation, ErrorKind::InvalidState, "frame is already consumed")); }
    if frame.0.surface.0.dead.get() { return Err(make_error(operation, ErrorKind::StaleHandle, "surface is destroyed")); }
    let q = queue.0.borrow();
    if q.dead { return Err(make_error(operation, ErrorKind::StaleHandle, "queue is destroyed")); }
    if source.resource.0.dead.get() { return Err(make_error(operation, ErrorKind::StaleHandle, "source texture is destroyed")); }
    if !Rc::ptr_eq(&q.device.0, &frame.0.surface.0.device.0) {
        return Err(make_error(operation, ErrorKind::CrossDevice, "queue belongs to a different device"));
    }
    if !Rc::ptr_eq(&source.resource.0.device.0, &frame.0.surface.0.device.0) {
        return Err(make_error(operation, ErrorKind::CrossDevice, "source texture belongs to a different device"));
    }
    let descriptor = &source.texture_descriptor;
    let configuration = frame.0.surface.0.configuration.borrow();
    if descriptor.width != configuration.physical_width || descriptor.height != configuration.physical_height {
        return Err(make_error(operation, ErrorKind::InvalidArgument, "source texture extent does not match the configured physical surface extent"));
    }
    if descriptor.sample_count != 1 { return Err(make_error(operation, ErrorKind::InvalidArgument, "source texture must have sample_count=1")); }
    if descriptor.format != TextureFormat::Rgba8Unorm { return Err(make_error(operation, ErrorKind::InvalidArgument, "source texture must be Rgba8_unorm")); }
    if !descriptor.usage.contains(&TextureUsage::TextureBinding) {
        return Err(make_error(operation, ErrorKind::InvalidArgument, "source texture must have Texture_binding usage"));
    }
    Ok(())
}
pub fn commit_present(commands: &Commands, source: &Texture, frame: &Frame) -> Result<Receipt, Error> {
    const OP: &str = "Backend.commit_present";
    recording(OP, commands)?;
    validate_present(OP, &commands.0.queue, source, frame)?;
    let receipt = (commands.0.raw().commit_present)(source.resource.0.token, frame.0.raw.clone())?;
    commands.0.state.set(CommandsState::Committed);
    commands.0.raw.borrow_mut().take();
    frame.0.consumed.set(true);
    frame.0.surface.0.frames.set(frame.0.surface.0.frames.get() - 1);
    Ok(receipt)
}

#[cfg(test)]
mod ownership_tests {
    use super::*;

    fn device() -> Device {
        Device(Rc::new(DeviceInner {
            raw: RefCell::new(None), capabilities: caps::minimum_m1(), handle: handle::create_device(),
            children: Cell::new(0), dead: Cell::new(false), queues: RefCell::new(Vec::new()),
        }))
    }

    #[test]
    fn destroy_releases_callbacks_but_not_escaped_readback_and_rolls_back_failure() {
        let device = device();
        let owner = Rc::new(());
        let weak = Rc::downgrade(&owner);
        let read_owner = owner.clone();
        let fail = Rc::new(Cell::new(true));
        let destroy_fail = fail.clone();
        let raw = DriverResource {
            token: 42,
            write: Box::new(|_, _| Ok(())),
            read: Box::new(move |_, length| { let _ = &read_owner; Ok(vec![7; length]) }),
            read_into: Box::new(|_, destination, offset, length| { destination[offset..offset + length].fill(7); Ok(()) }),
            destroy: Box::new(move || {
                let _ = &owner;
                if destroy_fail.get() { Err(make_error("test.destroy", ErrorKind::InvalidState, "busy")) } else { Ok(()) }
            }),
        };
        let resource = make_resource(&device, raw);
        device.0.children.set(1);
        let view_owner = Rc::new(());
        let view_weak = Rc::downgrade(&view_owner);
        let allocation = Arc::new(vec![7u8; 8]);
        let allocation_weak = Arc::downgrade(&allocation);
        *resource.0.mapping.borrow_mut() = Some(DriverMapping {
            readback: Arc::new(move |offset, length| Ok(allocation[offset as usize..offset as usize + length].to_vec())),
            view: Box::new(move || { let _ = &view_owner; Some(Rc::new(RefCell::new(vec![1.0; 2]))) }),
            release_view: Box::new(|| {}),
            flush: Box::new(|_| Ok(())),
            invalidate: Box::new(|_| Ok(())),
        });
        let buffer = Buffer { resource, buffer_descriptor: BufferDescriptor { label: None, size: 8, usage: vec![BufferUsage::CopyDst] }, memory: Memory::Shared };
        let clone = buffer.clone();
        let id = buffer_id(&buffer);
        let readback = owned_readback(&buffer).unwrap();
        assert_eq!(destroy_buffer(&buffer).unwrap_err().kind, ErrorKind::InvalidState);
        assert_eq!(device.0.children.get(), 1);
        assert_eq!(read_buffer(&clone, 0, 8).unwrap(), [7; 8]);
        assert!(mapped_view(&clone).is_some());
        assert!(weak.upgrade().is_some() && view_weak.upgrade().is_some());
        fail.set(false);
        destroy_buffer(&buffer).unwrap();
        destroy_buffer(&clone).unwrap();
        assert_eq!(device.0.children.get(), 0);
        assert_eq!(buffer_id(&clone), id);
        assert_eq!(read_buffer(&clone, 0, 1).unwrap_err().kind, ErrorKind::StaleHandle);
        assert!(matches!(owned_readback(&clone), Err(error) if error.kind == ErrorKind::StaleHandle));
        assert!(mapped_view(&clone).is_none());
        assert!(weak.upgrade().is_none() && view_weak.upgrade().is_none());
        assert_eq!(readback(0, 8).unwrap(), [7; 8]);
        assert!(allocation_weak.upgrade().is_some());
        drop(readback);
        assert!(allocation_weak.upgrade().is_none(), "dead Buffer clones retained the base reader");
    }

    #[test]
    fn pipeline_destroy_releases_payload_and_preserves_parent_and_metadata() {
        let device = device();
        let owner = Rc::new(());
        let weak = Rc::downgrade(&owner);
        let library = Library(Rc::new(LibraryInner {
            raw: RefCell::new(Some(DriverLibrary {
                create_compute_pipeline_in: Box::new(|_, _, _| unreachable!()),
                destroy_library: Box::new(|| Ok(())),
            })),
            device: device.clone(), dead: Cell::new(false), pipelines: Cell::new(1),
        }));
        let fail = Rc::new(Cell::new(true));
        let destroy_fail = fail.clone();
        let pipeline = Pipeline(Rc::new(PipelineInner {
            driver: RefCell::new(Some(DriverPipeline {
                pipeline_token: 5, max_threads: 64,
                destroy_pipeline: Box::new(move || {
                    let _ = &owner;
                    if destroy_fail.get() { Err(make_error("test.destroy", ErrorKind::InvalidState, "busy")) } else { Ok(()) }
                }),
            })),
            token: 5, max_threads: 64, device: device.clone(), library: Some(library.clone()),
            indirect: false, shape: PipelineShape::Compute, dead: Cell::new(false),
        }));
        device.0.children.set(2);
        assert_eq!(destroy_library(&library).unwrap_err().kind, ErrorKind::InvalidState);
        assert_eq!(destroy_pipeline(&pipeline).unwrap_err().kind, ErrorKind::InvalidState);
        assert_eq!(device.0.children.get(), 2);
        assert_eq!(library.0.pipelines.get(), 1);
        assert!(weak.upgrade().is_some());
        fail.set(false);
        destroy_pipeline(&pipeline).unwrap();
        destroy_pipeline(&pipeline.clone()).unwrap();
        assert!(weak.upgrade().is_none());
        assert_eq!(max_total_threads(&pipeline), 64);
        assert_eq!(library.0.pipelines.get(), 0);
        assert_eq!(device.0.children.get(), 1);
        destroy_library(&library).unwrap();
        assert!(library.0.raw.borrow().is_none());
        assert_eq!(device.0.children.get(), 0);
    }

    #[test]
    fn terminal_commands_and_encoders_release_callbacks_only_after_success() {
        let device = device();
        let queue = Queue(Rc::new(RefCell::new(QueueInner {
            raw: RefCell::new(None), device, dead: false, completed_epoch: 0, gpu_timing: GpuTiming { gpu_seconds: 0.0 },
        })));
        let make_commands = |owner: Rc<()>, fail: Rc<Cell<bool>>| {
            let commit_owner = owner.clone();
            let commit_fail = fail.clone();
            Commands(Rc::new(CommandsInner {
                raw: RefCell::new(Some(DriverCommands {
                    compute_encoder: Box::new(|| unreachable!()), accel_encoder: Box::new(|| unreachable!()),
                    blit_encoder: Box::new(|| unreachable!()), render_encoder: Box::new(|_| unreachable!()),
                    commit: Box::new(move || {
                        let _ = &commit_owner;
                        if commit_fail.get() { Err(make_error("test.commit", ErrorKind::InvalidState, "busy")) } else { Ok(Receipt { epoch: 1 }) }
                    }),
                    commit_present: Box::new(|_, _| unreachable!()),
                    abandon: Box::new(move || {
                        let _ = &owner;
                        if fail.get() { Err(make_error("test.abandon", ErrorKind::InvalidState, "busy")) } else { Ok(()) }
                    }),
                })),
                queue: queue.clone(), state: Cell::new(CommandsState::Recording), encoder: RefCell::new(None),
            }))
        };
        let make_encoder = |commands: &Commands, owner: Rc<()>, fail: Rc<Cell<bool>>| {
            let encoder = BlitEncoder(Rc::new(BlitInner {
                raw: RefCell::new(Some(DriverBlitEncoder {
                    buffer_to_texture: Box::new(|_, _, _, _, _, _, _, _| unreachable!()),
                    end_blit: Box::new(move || {
                        let _ = &owner;
                        if fail.get() { Err(make_error("test.end", ErrorKind::InvalidState, "busy")) } else { Ok(()) }
                    }),
                })),
                commands: commands.clone(), open: Cell::new(true),
            }));
            commands.0.state.set(CommandsState::Encoding);
            *commands.0.encoder.borrow_mut() = Some(ActiveEncoder::Blit(Rc::downgrade(&encoder.0)));
            encoder
        };
        let fail = Rc::new(Cell::new(true));
        let owner = Rc::new(());
        let weak = Rc::downgrade(&owner);
        let commands = make_commands(owner, fail.clone());
        let encoder_owner = Rc::new(());
        let encoder_weak = Rc::downgrade(&encoder_owner);
        let encoder = make_encoder(&commands, encoder_owner, fail.clone());
        assert!(end_blit(&encoder).is_err());
        assert!(encoder.0.open.get() && encoder_weak.upgrade().is_some());
        fail.set(false);
        end_blit(&encoder).unwrap();
        assert!(encoder_weak.upgrade().is_none());
        fail.set(true);
        assert!(commit(&commands).is_err());
        assert!(weak.upgrade().is_some());
        fail.set(false);
        assert_eq!(commit(&commands).unwrap().epoch, 1);
        assert!(weak.upgrade().is_none());
        assert_eq!(commit(&commands).unwrap_err().kind, ErrorKind::InvalidState);

        let owner = Rc::new(());
        let weak = Rc::downgrade(&owner);
        let abandoned = make_commands(owner, fail.clone());
        let encoder_owner = Rc::new(());
        let encoder_weak = Rc::downgrade(&encoder_owner);
        let encoder = make_encoder(&abandoned, encoder_owner, fail.clone());
        fail.set(true);
        assert!(abandon(&abandoned).is_err());
        assert!(encoder.0.open.get() && encoder_weak.upgrade().is_some() && weak.upgrade().is_some());
        fail.set(false);
        abandon(&abandoned).unwrap();
        abandon(&abandoned).unwrap();
        assert!(encoder_weak.upgrade().is_none() && weak.upgrade().is_none());
        assert_eq!(end_blit(&encoder).unwrap_err().kind, ErrorKind::InvalidState);
    }
}
