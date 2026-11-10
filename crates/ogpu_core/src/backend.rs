use crate::validation::{AdmittedBuffer, AdmittedTexture, TextureRead, TextureReadInto, BufferProfile, BufferKind, StorageMode, CpuCacheMode};
use crate::{
    acceleration::{self, Acceleration, Descriptor as PortableDescriptor},
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

pub trait DriverResource {
    fn token(&self) -> Token;
    fn as_any(&self) -> &dyn std::any::Any;
    fn write(&self, offset: i64, bytes: &[u8]) -> Result<(), Error>;
    fn read(&self, offset: i64, length: usize) -> Result<Vec<u8>, Error>;
    fn read_into(&self, offset: i64, destination: &mut [u8], destination_offset: usize, length: usize) -> Result<(), Error>;
    fn read_texture(&self, read: &TextureRead) -> Result<Vec<u8>, Error> { self.read(0, read.length()) }
    fn read_texture_into(&self, read: &TextureReadInto, destination: &mut [u8]) -> Result<(), Error> {
        read.bind_destination(destination.len())?;
        self.read_into(0, destination, 0, read.layout().length())
    }
    fn destroy(&self) -> Result<(), Error>;
}

/// Lazy cross-thread read of mapped storage; host view sync lives in `ogpu_core`.
pub trait DriverMapping {
    fn readback(&self) -> OwnedReadback;
}

struct MappedHost {
    view: RefCell<Option<Floats>>,
    released: Cell<bool>,
    float_count: usize,
}

pub struct DriverMapped {
    pub mapped_resource: Box<dyn DriverResource>,
    pub mapping: Box<dyn DriverMapping>,
}

pub trait DriverPipeline {
    fn as_any(&self) -> &dyn std::any::Any;
    fn max_threads(&self) -> i64;
    fn destroy_pipeline(&self) -> Result<(), Error>;
}

/// One acquired frame: whatever the driver needs to present or discard it.
#[derive(Clone)]
pub struct DriverFrame(pub Rc<dyn std::any::Any>);

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct GpuTiming { pub gpu_seconds: f64 }

pub enum DriverAcquire {
    Acquired(DriverFrame),
    Timeout,
    Occluded,
    DeviceLost,
}

pub trait DriverSurface {
    fn configure(&self, configuration: &surface::AdmittedConfiguration) -> Result<(), Error>;
    fn acquire_sync(&self) -> Result<DriverAcquire, Error>;
    fn discard(&self, frame: DriverFrame) -> Result<(), Error>;
    fn destroy_surface(&self) -> Result<(), Error>;
}

pub trait DriverLibrary {
    fn create_compute_pipeline_in(&self, function: &crate::shader::AdmittedFunction, interface: &[Binding]) -> Result<Box<dyn DriverPipeline>, Error>;
    fn destroy_library(&self) -> Result<(), Error>;
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct AccelSizes {
    pub structure_size: i64,
    pub build_scratch_size: i64,
    pub refit_scratch_size: i64,
}

#[derive(Clone)]
pub enum DriverAccelDescriptor {
    Build { profile:acceleration::AdmittedBuild, buffers:Vec<Rc<dyn DriverResource>>, structures:Vec<Rc<dyn DriverAccel>> },
    Sized { size:std::num::NonZeroU64, template:Rc<dyn DriverAccel> },
}

pub trait DriverAccel {
    fn as_any(&self) -> &dyn std::any::Any;
    fn accel_sizes(&self) -> AccelSizes;
    fn destroy_accel(&self) -> Result<(), Error>;
}

#[derive(Clone, Debug, PartialEq)]
pub struct Instance {
    pub transform: Vec<f64>,
    pub mask: i64,
    pub structure_index: i64,
}

pub trait DriverComputeEncoder {
    fn set_pipeline(&self, pipeline: &dyn DriverPipeline) -> Result<(), Error>;
    fn set_buffer(&self, index: i64, offset: i64, buffer: &dyn DriverResource) -> Result<(), Error>;
    fn set_bytes(&self, index: i64, admitted: crate::validation::AdmittedBytes, bytes: &[u8]) -> Result<(), Error>;
    fn set_texture(&self, index: i64, texture: &dyn DriverResource) -> Result<(), Error>;
    fn set_accel(&self, index: i64, accel: &dyn DriverAccel) -> Result<(), Error>;
    fn dispatch_threads(&self, threads: (i64, i64, i64), threadgroup: (i64, i64, i64)) -> Result<(), Error>;
    fn dispatch_indirect(&self, arguments: &dyn DriverResource, offset: i64, threadgroup: (i64, i64, i64)) -> Result<(), Error>;
    fn compute_use_accels(&self, accels: &[Rc<dyn DriverAccel>]) -> Result<(), Error>;
    fn end_compute(&self) -> Result<(), Error>;
}

pub trait DriverAccelEncoder {
    fn build(&self, accel: &dyn DriverAccel, scratch: &dyn DriverResource, offset: i64) -> Result<(), Error>;
    fn compact(&self, source: &dyn DriverAccel, destination: &dyn DriverAccel) -> Result<(), Error>;
    fn write_compacted_size(&self, accel: &dyn DriverAccel, destination: &dyn DriverResource, output: crate::validation::AdmittedCompactedSize) -> Result<(), Error>;
    fn refit(&self, accel: &dyn DriverAccel, descriptor: &DriverAccelDescriptor, scratch: &dyn DriverResource, offset: i64) -> Result<(), Error>;
    fn end_accel(&self) -> Result<(), Error>;
}

pub trait DriverBlitEncoder {
    fn buffer_to_texture(&self, source: &dyn DriverResource, offset: i64, row: i64, image: i64, destination: &dyn DriverResource, mip: i64, origin: Origin, extent: Extent) -> Result<(), Error>;
    fn end_blit(&self) -> Result<(), Error>;
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ShaderStage { Vertex, Fragment }
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Winding { Clockwise, CounterClockwise }
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct DepthState { pub depth_compare: Comparison, pub depth_write: bool }

#[derive(Clone)]
pub struct DriverColorAttachment {
    pub color: Rc<dyn DriverResource>,
    pub color_resolve: Option<Rc<dyn DriverResource>>,
    pub color_load: Load,
    pub color_store: Store,
    pub color_clear: (f64, f64, f64, f64),
}
#[derive(Clone)]
pub struct DriverDepthAttachment {
    pub depth: Rc<dyn DriverResource>,
    pub depth_load: Load,
    pub depth_store: Store,
    pub depth_clear: f64,
}
#[derive(Clone)]
pub struct DriverRenderTarget(DriverRenderProfile);
#[derive(Clone)]
pub struct DriverRenderProfile {
    pub target_colors: Vec<DriverColorAttachment>,
    pub target_depth: Option<DriverDepthAttachment>,
    pub target_width: i64,
    pub target_height: i64,
    pub target_samples: i64,
    clear_color: crate::render_pass::ClearColor,
    actions: crate::render_pass::AdmittedActions,
}
impl std::ops::Deref for DriverRenderTarget { type Target = DriverRenderProfile; fn deref(&self) -> &DriverRenderProfile { &self.0 } }
impl DriverRenderTarget {
    pub fn clear_color(&self) -> &crate::render_pass::ClearColor { &self.0.clear_color }
    pub fn actions(&self) -> &crate::render_pass::AdmittedActions { &self.0.actions }
}
/// One lowered batch draw; the pipeline is shared with the core's own handle.
#[derive(Clone)]
pub struct DriverBatchDraw {
    pub batch_pipeline: Rc<dyn DriverPipeline>,
    pub batch_buffers: Vec<(ShaderStage, i64, Rc<dyn DriverResource>, i64)>,
    pub batch_primitive: Primitive,
    pub batch_index: Option<(IndexType, Rc<dyn DriverResource>, i64, i64)>,
    pub batch_vertex_start: i64,
    pub batch_vertex_count: i64,
    pub batch_instances: i64,
}

pub trait DriverRenderEncoder {
    fn render_set_pipeline(&self, pipeline: &dyn DriverPipeline) -> Result<(), Error>;
    fn set_stage_buffer(&self, stage: ShaderStage, index: i64, offset: i64, buffer: &dyn DriverResource) -> Result<(), Error>;
    fn set_stage_texture(&self, stage: ShaderStage, index: i64, texture: &dyn DriverResource) -> Result<(), Error>;
    fn set_stage_sampler(&self, stage: ShaderStage, index: i64, sampler: &dyn DriverSampler) -> Result<(), Error>;
    fn set_viewport(&self, rect: crate::render_pass::AdmittedViewport) -> Result<(), Error>;
    fn set_scissor(&self, rect: Rect) -> Result<(), Error>;
    fn set_cull(&self, cull: Cull) -> Result<(), Error>;
    fn set_winding(&self, winding: Winding) -> Result<(), Error>;
    fn set_depth_state(&self, state: Option<DepthState>) -> Result<(), Error>;
    fn draw(&self, primitive: Primitive, first: i64, count: i64, instances: i64) -> Result<(), Error>;
    fn draw_indexed(&self, primitive: Primitive, index_type: IndexType, buffer: &dyn DriverResource, offset: i64, count: i64, instances: i64) -> Result<(), Error>;
    fn draw_indexed_indirect(&self, primitive: Primitive, index_type: IndexType, buffer: &dyn DriverResource, offset: i64, arguments: &dyn DriverResource, arguments_offset: i64) -> Result<(), Error>;
    fn draw_batch(&self, draws: &[DriverBatchDraw]) -> Result<(), Error>;
    fn use_resources(&self, resources: &[Rc<dyn DriverResource>]) -> Result<(), Error>;
    fn execute_icb(&self, icb: &dyn DriverIcb, location: i64, length: i64) -> Result<(), Error>;
    fn end_render(&self) -> Result<(), Error>;
}

pub trait DriverSampler {
    fn as_any(&self) -> &dyn std::any::Any;
    fn destroy_sampler(&self) -> Result<(), Error>;
}

pub trait DriverIcb {
    fn as_any(&self) -> &dyn std::any::Any;
    fn icb_set_pipeline(&self, index: crate::validation::AdmittedIcbIndex, pipeline: &dyn DriverPipeline) -> Result<(), Error>;
    fn icb_set_buffer(&self, index: crate::validation::AdmittedIcbIndex, stage: ShaderStage, slot: i64, offset: i64, buffer: &dyn DriverResource) -> Result<(), Error>;
    fn icb_draw(&self, index: crate::validation::AdmittedIcbIndex, primitive: Primitive, first: i64, count: i64, instances: i64) -> Result<(), Error>;
    fn icb_draw_indexed(&self, index: crate::validation::AdmittedIcbIndex, primitive: Primitive, buffer: &dyn DriverResource, range:&crate::validation::AdmittedIndexRange) -> Result<(), Error>;
    fn destroy_icb(&self) -> Result<(), Error>;
}

pub trait DriverArgument {
    fn argument_length(&self) -> i64;
    fn argument_alignment(&self) -> i64;
    fn argument_texture(&self, buffer: &dyn DriverResource, offset: i64, slot: i64, texture: &dyn DriverResource) -> Result<(), Error>;
    fn argument_sampler(&self, buffer: &dyn DriverResource, offset: i64, slot: i64, sampler: &dyn DriverSampler) -> Result<(), Error>;
    fn destroy_argument(&self) -> Result<(), Error>;
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct RenderPipelineOptions { pub blend: Blend, pub topology: Primitive, pub indirect: bool }

pub trait DriverCommands {
    fn compute_encoder(&self) -> Result<Box<dyn DriverComputeEncoder>, Error>;
    fn accel_encoder(&self) -> Result<Box<dyn DriverAccelEncoder>, Error>;
    fn blit_encoder(&self) -> Result<Box<dyn DriverBlitEncoder>, Error>;
    fn render_encoder(&self, target: &DriverRenderTarget) -> Result<Box<dyn DriverRenderEncoder>, Error>;
    fn commit(&self) -> Result<Receipt, Error>;
    fn commit_present(&self, source: &dyn DriverResource, frame: DriverFrame) -> Result<Receipt, Error>;
    fn abandon(&self) -> Result<(), Error>;
}

pub trait DriverQueue {
    /// Waits for `epoch`; the core guarantees it was submitted and is not yet complete.
    fn complete_through(&self, epoch: i64) -> Result<(), Error>;
    /// Whether `epoch` (submitted, not yet complete) has finished, completing it when so.
    fn poll_through(&self, epoch: i64) -> Result<bool, Error>;
    fn begin_commands(&self) -> Result<Box<dyn DriverCommands>, Error>;
    /// The GPU seconds of a just-completed epoch; asked once, when the core completes it.
    fn gpu_duration(&self, epoch: i64) -> Option<f64>;
    fn destroy_queue(&self) -> Result<(), Error>;
}

pub trait DriverDevice {
    fn device_handle(&self) -> DeviceHandle;
    fn capabilities(&self) -> Caps;
    fn allocated_size(&self) -> i64;
    fn recommended_working_set(&self) -> i64;
    fn create_buffer(&self, descriptor: &AdmittedBuffer) -> Result<Box<dyn DriverResource>, Error>;
    fn create_mapped_buffer(&self, descriptor: &AdmittedBuffer) -> Result<DriverMapped, Error>;
    fn create_texture(&self, descriptor: &AdmittedTexture) -> Result<Box<dyn DriverResource>, Error>;
    fn create_library(&self, shader: &Shader) -> Result<Box<dyn DriverLibrary>, Error>;
    /// Compiles a library and its first entry's compute pipeline in the driver's background so a
    /// later `create_library` of the same shader is a cache hit; None where the driver cannot.
    fn warm_library(&self, shader: &Shader) -> Option<Warm>;
    fn create_accel(&self, descriptor: &DriverAccelDescriptor) -> Result<Box<dyn DriverAccel>, Error>;
    fn instance_layout(&self, kind: InstanceKind) -> Vec<i64>;
    fn create_sampler(&self, descriptor: &crate::validation::AdmittedSampler) -> Result<Box<dyn DriverSampler>, Error>;
    fn create_render_pipeline(&self, options: RenderPipelineOptions, descriptor: &RenderDescriptor, admitted: &pipeline::AdmittedRenderPipeline) -> Result<Box<dyn DriverPipeline>, Error>;
    fn create_icb(&self, profile: &crate::validation::AdmittedIcb) -> Result<Box<dyn DriverIcb>, Error>;
    fn create_argument(&self, pipeline: &dyn DriverPipeline, stage: ShaderStage, index: crate::validation::AdmittedSlot) -> Result<Box<dyn DriverArgument>, Error>;
    fn create_queue(&self) -> Result<Box<dyn DriverQueue>, Error>;
    fn create_surface(&self, configuration: &surface::AdmittedConfiguration) -> Result<Box<dyn DriverSurface>, Error>;
    fn destroy_device(&self) -> Result<(), Error>;
}

pub trait Driver {
    fn create_device(&self) -> Result<Box<dyn DriverDevice>, Error>;
}

/// A backend's name and entry points; selection belongs to the public OGPU layer.
#[derive(Clone, Copy)]
pub struct Implementation {
    pub backend: &'static str,
    /// A fresh driver per call.
    pub create_driver: fn() -> Box<dyn Driver>,
    /// Native handles still alive. Diagnostics only.
    pub live_handles: fn() -> i64,
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
    raw: RefCell<Option<Box<dyn DriverDevice>>>,
    capabilities: Caps,
    handle: DeviceHandle,
    children: Cell<i64>,
    dead: Cell<bool>,
    queues: RefCell<Vec<Weak<RefCell<QueueInner>>>>,
}
#[derive(Clone)]
struct Resource(Rc<ResourceInner>);
struct ResourceInner {
    raw: RefCell<Option<Rc<dyn DriverResource>>>,
    mapping: RefCell<Option<Box<dyn DriverMapping>>>,
    mapped: RefCell<Option<MappedHost>>,
    token: Token,
    handle: ResourceHandle,
    device: Device,
    dead: Cell<bool>,
}
#[derive(Clone)]
pub struct Buffer { resource: Resource, buffer_descriptor: AdmittedBuffer, memory: Memory }
#[derive(Clone)]
pub struct Texture { resource: Resource, texture_descriptor: AdmittedTexture }
#[derive(Clone)]
pub struct Pipeline(Rc<PipelineInner>);
struct PipelineInner { driver: RefCell<Option<Rc<dyn DriverPipeline>>>, max_threads: i64, device: Device, library: Option<Library>, indirect: bool, shape: PipelineShape, render: Option<(pipeline::ColorFormat, i64)>, dead: Cell<bool> }
#[derive(Clone, Copy, PartialEq, Eq)]
enum PipelineShape { Vertex, Compute }
#[derive(Clone)]
pub struct Library(Rc<LibraryInner>);
struct LibraryInner { raw: RefCell<Option<Box<dyn DriverLibrary>>>, device: Device, dead: Cell<bool>, pipelines: Cell<i64> }
#[derive(Clone)]
pub struct Queue(Rc<RefCell<QueueInner>>);
/// The queue owns the epoch counters, the frames-in-flight cap and the GPU timing ring; a driver
/// numbers its submissions and waits.
struct QueueInner { raw: RefCell<Option<Box<dyn DriverQueue>>>, device: Device, dead: bool, submitted: i64, completed_epoch: i64, gpu_timing: GpuTiming, durations: Vec<(i64, f64)> }
const MAX_FRAMES_IN_FLIGHT: i64 = 3;
/// The last few completed epochs' GPU durations kept for `gpu_duration`.
const DURATION_CAPACITY: usize = 16;
#[derive(Clone)]
pub struct Surface(Rc<SurfaceInner>);
struct SurfaceInner { raw: RefCell<Option<Box<dyn DriverSurface>>>, device: Device, dead: Cell<bool>, frames: Cell<i64>, configuration: RefCell<surface::AdmittedConfiguration> }
#[derive(Clone)]
pub struct Frame(Rc<FrameInner>);
struct FrameInner { raw: DriverFrame, surface: Surface, consumed: Cell<bool> }
#[derive(Clone)]
pub struct Accel(Rc<AccelInner>);
struct AccelInner { raw: RefCell<Option<Rc<dyn DriverAccel>>>, sizes: AccelSizes, device: Device, portable: RefCell<Acceleration>, structures: Vec<Accel>, dead: Cell<bool> }
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum CommandsState { Recording, Encoding, Committed, Abandoned }
#[derive(Clone)]
pub struct Commands(Rc<CommandsInner>);
struct CommandsInner { raw: RefCell<Option<Box<dyn DriverCommands>>>, queue: Queue, state: Cell<CommandsState>, encoder: RefCell<Option<ActiveEncoder>> }
enum ActiveEncoder { Compute(Weak<ComputeInner>), Accel(Weak<AccelEncoderInner>), Blit(Weak<BlitInner>), Render(Weak<RenderInner>) }
#[derive(Clone)]
pub struct ComputeEncoder(Rc<ComputeInner>);
struct ComputeInner { raw: RefCell<Option<Box<dyn DriverComputeEncoder>>>, commands: Commands, max_threads: Cell<i64> }
#[derive(Clone)]
pub struct AccelEncoder(Rc<AccelEncoderInner>);
struct AccelEncoderInner { raw: RefCell<Option<Box<dyn DriverAccelEncoder>>>, commands: Commands, open: Cell<bool> }
#[derive(Clone)]
pub struct BlitEncoder(Rc<BlitInner>);
struct BlitInner { raw: RefCell<Option<Box<dyn DriverBlitEncoder>>>, commands: Commands, open: Cell<bool> }
#[derive(Clone)]
pub struct Sampler(Rc<SamplerInner>);
struct SamplerInner { raw: RefCell<Option<Box<dyn DriverSampler>>>, device: Device, dead: Cell<bool> }
#[derive(Clone)]
pub struct Icb(Rc<IcbInner>);
struct IcbInner { raw: RefCell<Option<Box<dyn DriverIcb>>>, device: Device, capacity: i64, dead: Cell<bool> }
#[derive(Clone)]
pub struct Argument(Rc<ArgumentInner>);
struct ArgumentInner { raw: RefCell<Option<Box<dyn DriverArgument>>>, length: i64, alignment: i64, device: Device, dead: Cell<bool> }
#[derive(Clone)]
pub struct RenderEncoder(Rc<RenderInner>);
struct RenderInner { raw: RefCell<Option<Box<dyn DriverRenderEncoder>>>, commands: Commands, open: Cell<bool>, pipeline_set: Cell<bool>, indirect: Cell<bool>, target: (TextureFormat, u8), extent: (i64, i64) }

// A dead wrapper keeps identity and validation metadata, never driver callbacks/native owners.
// Access follows the existing liveness/state guards; failed transitions retain the payload.
impl DeviceInner {
    fn raw(&self) -> std::cell::Ref<'_, dyn DriverDevice> { std::cell::Ref::map(self.raw.borrow(), |raw| raw.as_deref().expect("live device payload")) }
}
impl ResourceInner {
    fn shared(&self) -> Rc<dyn DriverResource> { self.raw.borrow().clone().expect("live resource payload") }
    fn raw(&self) -> std::cell::Ref<'_, dyn DriverResource> { std::cell::Ref::map(self.raw.borrow(), |raw| raw.as_deref().expect("live resource payload")) }
}
impl PipelineInner {
    fn driver(&self) -> std::cell::Ref<'_, dyn DriverPipeline> { std::cell::Ref::map(self.driver.borrow(), |raw| raw.as_deref().expect("live pipeline payload")) }
}
impl LibraryInner {
    fn raw(&self) -> std::cell::Ref<'_, dyn DriverLibrary> { std::cell::Ref::map(self.raw.borrow(), |raw| raw.as_deref().expect("live library payload")) }
}
impl QueueInner {
    fn raw(&self) -> std::cell::Ref<'_, dyn DriverQueue> { std::cell::Ref::map(self.raw.borrow(), |raw| raw.as_deref().expect("live queue payload")) }
}
impl SurfaceInner {
    fn raw(&self) -> std::cell::Ref<'_, dyn DriverSurface> { std::cell::Ref::map(self.raw.borrow(), |raw| raw.as_deref().expect("live surface payload")) }
}
impl AccelInner {
    fn shared(&self) -> Rc<dyn DriverAccel> { self.raw.borrow().clone().expect("live acceleration payload") }
    fn raw(&self) -> std::cell::Ref<'_, dyn DriverAccel> { std::cell::Ref::map(self.raw.borrow(), |raw| raw.as_deref().expect("live acceleration payload")) }
}
impl CommandsInner {
    fn raw(&self) -> std::cell::Ref<'_, dyn DriverCommands> { std::cell::Ref::map(self.raw.borrow(), |raw| raw.as_deref().expect("recording commands payload")) }
}
impl ComputeInner {
    fn raw(&self) -> std::cell::Ref<'_, dyn DriverComputeEncoder> { std::cell::Ref::map(self.raw.borrow(), |raw| raw.as_deref().expect("open compute encoder payload")) }
}
impl AccelEncoderInner {
    fn raw(&self) -> std::cell::Ref<'_, dyn DriverAccelEncoder> { std::cell::Ref::map(self.raw.borrow(), |raw| raw.as_deref().expect("open acceleration encoder payload")) }
}
impl BlitInner {
    fn raw(&self) -> std::cell::Ref<'_, dyn DriverBlitEncoder> { std::cell::Ref::map(self.raw.borrow(), |raw| raw.as_deref().expect("open blit encoder payload")) }
}
impl SamplerInner {
    fn raw(&self) -> std::cell::Ref<'_, dyn DriverSampler> { std::cell::Ref::map(self.raw.borrow(), |raw| raw.as_deref().expect("live sampler payload")) }
}
impl IcbInner {
    fn raw(&self) -> std::cell::Ref<'_, dyn DriverIcb> { std::cell::Ref::map(self.raw.borrow(), |raw| raw.as_deref().expect("live indirect command payload")) }
}
impl ArgumentInner {
    fn raw(&self) -> std::cell::Ref<'_, dyn DriverArgument> { std::cell::Ref::map(self.raw.borrow(), |raw| raw.as_deref().expect("live argument payload")) }
}
impl RenderInner {
    fn raw(&self) -> std::cell::Ref<'_, dyn DriverRenderEncoder> { std::cell::Ref::map(self.raw.borrow(), |raw| raw.as_deref().expect("open render encoder payload")) }
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
fn make_resource(device: &Device, raw: Box<dyn DriverResource>) -> Resource {
    Resource(Rc::new(ResourceInner {
        token: raw.token(),
        raw: RefCell::new(Some(raw.into())),
        mapping: RefCell::new(None),
        mapped: RefCell::new(None),
        handle: handle::create(&device.0.handle),
        device: device.clone(),
        dead: Cell::new(false),
    }))
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
    crate::validation::admit_slot(operation,index).map(|_| ())
}
fn finite(value: f64) -> bool { value.is_finite() && value.abs() <= f32::MAX as f64 }
fn border_code(border: Border) -> i64 { match border { Border::Clamp => 0, Border::Vanish => 1 } }
fn is_built(value: &Accel) -> bool { acceleration::built(&value.0.portable.borrow()) }
pub fn create_device(driver: Box<dyn Driver>) -> Result<Device, Error> {
    let raw = driver.create_device()?;
    let capabilities = raw.capabilities();
    caps::validate(&capabilities)?;
    let handle = raw.device_handle();
    Ok(Device(Rc::new(DeviceInner {
        capabilities, raw: RefCell::new(Some(raw)), handle, children: Cell::new(0), dead: Cell::new(false), queues: RefCell::new(Vec::new()),
    })))
}

pub fn capabilities(device: &Device) -> Caps { device.0.capabilities }
pub fn device_handle(device: &Device) -> &DeviceHandle { &device.0.handle }
pub fn allocated_size(device: &Device) -> i64 { device.0.raw.borrow().as_ref().map_or(0, |raw| raw.allocated_size()) }
pub fn recommended_working_set(device: &Device) -> i64 { device.0.raw.borrow().as_ref().map_or(0, |raw| raw.recommended_working_set()) }

pub fn create_buffer(device: &Device, descriptor: BufferDescriptor, memory: Option<Memory>) -> Result<Buffer, Error> {
    const OP: &str = "Backend.create_buffer";
    live(OP, device)?;
    let memory = memory.unwrap_or(Memory::Shared);
    let storage = match memory { Memory::Shared => StorageMode::Shared, Memory::DeviceLocal => StorageMode::Private, Memory::Memoryless => StorageMode::Memoryless };
    let descriptor = crate::validation::admit_buffer(&device.0.capabilities,
        BufferProfile { descriptor, storage, cpu_cache: CpuCacheMode::DefaultCache, kind: BufferKind::Ordinary })?;
    let raw = device.0.raw().create_buffer(&descriptor)?;
    device.0.children.set(device.0.children.get() + 1);
    Ok(Buffer { resource: make_resource(device, raw), buffer_descriptor: descriptor, memory })
}

pub fn create_mapped_buffer(device: &Device, descriptor: BufferDescriptor) -> Result<Buffer, Error> {
    const OP: &str = "Backend.create_mapped_buffer";
    live(OP, device)?;
    let descriptor = crate::validation::admit_buffer(&device.0.capabilities,
        BufferProfile { descriptor, storage: StorageMode::Shared, cpu_cache: CpuCacheMode::DefaultCache, kind: BufferKind::Mapped })?;
    let mapped = device.0.raw().create_mapped_buffer(&descriptor)?;
    let resource = make_resource(device, mapped.mapped_resource);
    *resource.0.mapping.borrow_mut() = Some(mapped.mapping);
    *resource.0.mapped.borrow_mut() = Some(MappedHost {
        view: RefCell::new(None),
        released: Cell::new(false),
        float_count: (descriptor.size / 4) as usize,
    });
    device.0.children.set(device.0.children.get() + 1);
    Ok(Buffer { resource, buffer_descriptor: descriptor, memory: Memory::Shared })
}

fn floats_to_bytes(floats: &[f32], length: usize) -> Vec<u8> {
    let mut bytes: Vec<u8> = floats.iter().flat_map(|value| value.to_le_bytes()).collect();
    bytes.truncate(length);
    bytes
}

fn open_mapped_view(buffer: &Buffer) -> Result<Floats, Error> {
    const OP: &str = "Backend.mapped_view";
    if buffer.resource.0.dead.get() {
        return Err(make_error(OP, ErrorKind::StaleHandle, "buffer is destroyed"));
    }
    {
        let mut guard = buffer.resource.0.mapped.borrow_mut();
        let Some(host) = guard.as_mut() else {
            return Err(make_error(OP, ErrorKind::InvalidArgument, "buffer is not mapped"));
        };
        if let Some(view) = host.view.borrow().as_ref() {
            return Ok(view.clone());
        }
    }
    let (released, float_count) = {
        let guard = buffer.resource.0.mapped.borrow();
        let host = guard.as_ref().ok_or_else(|| make_error(OP, ErrorKind::InvalidArgument, "buffer is not mapped"))?;
        (host.released.get(), host.float_count)
    };
    let floats = if released {
        vec![f32::NAN; float_count]
    } else {
        let bytes = read_buffer(buffer, 0, (float_count * 4) as i64)?;
        bytes
            .chunks_exact(4)
            .map(|chunk| f32::from_le_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]))
            .collect()
    };
    let view = Rc::new(RefCell::new(floats));
    {
        let mut guard = buffer.resource.0.mapped.borrow_mut();
        let host = guard.as_mut().ok_or_else(|| make_error(OP, ErrorKind::InvalidArgument, "buffer is not mapped"))?;
        if host.view.borrow().is_none() {
            *host.view.borrow_mut() = Some(view.clone());
        } else if let Some(existing) = host.view.borrow().as_ref() {
            return Ok(existing.clone());
        }
    }
    Ok(view)
}

fn sync_mapped(buffer: &Buffer, operation: &str, length: i64, invalidate: bool) -> Result<(), Error> {
    if length < 0 || length > buffer.buffer_descriptor.size {
        return Err(make_error(operation, ErrorKind::InvalidArgument, "range exceeds the buffer"));
    }
    let Ok(length) = usize::try_from(length) else {
        return Err(make_error(operation, ErrorKind::InvalidArgument, "range exceeds the buffer"));
    };
    if invalidate {
        let view = open_mapped_view(buffer)?;
        if length > view.borrow().len() * 4 {
            return Err(make_error(operation, ErrorKind::InvalidArgument, "range exceeds the buffer"));
        }
        let bytes = read_buffer(buffer, 0, length as i64)?;
        for (slot, chunk) in view.borrow_mut().iter_mut().zip(bytes.chunks_exact(4)) {
            *slot = f32::from_le_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]);
        }
        Ok(())
    } else {
        let view = {
            let guard = buffer.resource.0.mapped.borrow();
            let Some(host) = guard.as_ref() else {
                return Err(make_error(operation, ErrorKind::InvalidArgument, "buffer is not mapped"));
            };
            host.view.borrow().clone()
        };
        let Some(view) = view else {
            return Err(make_error(operation, ErrorKind::InvalidArgument, "no host view was written"));
        };
        write_buffer(buffer, 0, &floats_to_bytes(&view.borrow(), length))
    }
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
    let readback = mapping.readback();
    Ok(Arc::new(move |offset, length| {
        if offset < 0 || offset > size || u64::try_from(length).map_or(true, |length| length > (size - offset) as u64) {
            return Err(make_error(OP, ErrorKind::InvalidArgument, "range exceeds the buffer"));
        }
        readback(offset, length)
    }))
}
pub fn mapped_view(buffer: &Buffer) -> Option<Floats> {
    if buffer.resource.0.dead.get() {
        None
    } else {
        open_mapped_view(buffer).ok()
    }
}
pub fn release_mapped_view(buffer: &Buffer) -> Result<(), Error> {
    const OP: &str = "Backend.release_mapped_view";
    if buffer.resource.0.dead.get() {
        return Err(make_error(OP, ErrorKind::StaleHandle, "buffer is destroyed"));
    }
    let mut guard = buffer.resource.0.mapped.borrow_mut();
    let Some(host) = guard.as_mut() else {
        return Err(make_error(OP, ErrorKind::InvalidArgument, "buffer is not mapped"));
    };
    host.released.set(true);
    if let Some(view) = host.view.borrow_mut().take() {
        view.borrow_mut().fill(f32::NAN);
    }
    Ok(())
}
pub fn buffer_size(buffer: &Buffer) -> i64 { buffer.buffer_descriptor.size }

fn create_texture_kind(operation: &str, device: &Device, descriptor: TextureDescriptor, depth: bool, memoryless: bool) -> Result<Texture, Error> {
    live(operation, device)?;
    if memoryless { require(operation, device, Feature::MemorylessAttachments)?; }
    let descriptor = crate::validation::admit_ordinary_texture(&device.0.capabilities, descriptor, depth, memoryless)?;
    let raw = device.0.raw().create_texture(&descriptor)?;
    device.0.children.set(device.0.children.get() + 1);
    Ok(Texture { resource: make_resource(device, raw), texture_descriptor: descriptor })
}
pub fn create_texture(device: &Device, descriptor: TextureDescriptor) -> Result<Texture, Error> {
    create_texture_kind("Backend.create_texture", device, descriptor, false, false)
}
pub fn create_depth_texture(device: &Device, descriptor: TextureDescriptor) -> Result<Texture, Error> {
    create_texture_kind("Backend.create_depth_texture", device, descriptor, true, false)
}
pub fn create_memoryless_texture(device: &Device, depth: bool, descriptor: TextureDescriptor) -> Result<Texture, Error> {
    create_texture_kind("Backend.create_memoryless_texture", device, descriptor, depth, true)
}

pub fn create_queue(device: &Device) -> Result<Queue, Error> {
    const OP: &str = "Backend.create_queue";
    live(OP, device)?;
    let raw = device.0.raw().create_queue()?;
    device.0.children.set(device.0.children.get() + 1);
    let queue = Queue(Rc::new(RefCell::new(QueueInner { raw: RefCell::new(Some(raw)), device: device.clone(), dead: false, submitted: 0, completed_epoch: 0, gpu_timing: GpuTiming { gpu_seconds: 0.0 }, durations: Vec::new() })));
    device.0.queues.borrow_mut().push(Rc::downgrade(&queue.0));
    Ok(queue)
}
pub fn create_surface(device: &Device, configuration: Configuration) -> Result<Surface, Error> {
    const OP: &str = "Backend.create_surface";
    live(OP, device)?;
    let configuration = surface::admit_configuration(OP, configuration)?;
    let raw = device.0.raw().create_surface(&configuration)?;
    device.0.children.set(device.0.children.get() + 1);
    Ok(Surface(Rc::new(SurfaceInner {
        raw: RefCell::new(Some(raw)), device: device.clone(), dead: Cell::new(false), frames: Cell::new(0),
        configuration: RefCell::new(configuration),
    })))
}
pub fn buffer_id(buffer: &Buffer) -> i64 { handle::id(&buffer.resource.0.handle) }
pub fn texture_id(texture: &Texture) -> i64 { handle::id(&texture.resource.0.handle) }
fn buffer_range(operation: &str, buffer: &Buffer, offset: i64, length: usize) -> Result<(), Error> {
    let size = buffer.buffer_descriptor.size;
    if offset < 0 || offset > size || length as u128 > (size - offset) as u128 {
        Err(make_error(operation, ErrorKind::InvalidArgument, "range exceeds the buffer"))
    } else { Ok(()) }
}
pub fn write_buffer(buffer: &Buffer, offset: i64, bytes: &[u8]) -> Result<(), Error> {
    const OP: &str = "Backend.write_buffer";
    if buffer.resource.0.dead.get() { return Err(make_error(OP, ErrorKind::StaleHandle, "buffer is destroyed")); }
    if buffer.memory == Memory::DeviceLocal { return Err(make_error(OP, ErrorKind::Unsupported, "device-local buffers are not host writable")); }
    buffer_range(OP, buffer, offset, bytes.len())?;
    buffer.resource.0.raw().write(offset, bytes)
}
pub fn read_buffer(buffer: &Buffer, offset: i64, length: i64) -> Result<Vec<u8>, Error> {
    const OP: &str = "Backend.read_buffer";
    if buffer.resource.0.dead.get() { return Err(make_error(OP, ErrorKind::StaleHandle, "buffer is destroyed")); }
    if buffer.memory == Memory::DeviceLocal { return Err(make_error(OP, ErrorKind::Unsupported, "device-local buffers are not host readable")); }
    let Ok(length) = usize::try_from(length) else { return Err(make_error(OP, ErrorKind::InvalidArgument, "read length is invalid")); };
    buffer_range(OP, buffer, offset, length)?;
    buffer.resource.0.raw().read(offset, length)
}
pub fn read_texture(texture: &Texture, bytes_per_row: i64) -> Result<Vec<u8>, Error> {
    let read = crate::validation::admit_base_texture_read(&texture.texture_descriptor, bytes_per_row)?;
    texture.resource.0.raw().read_texture(&read)
}
pub fn read_texture_into(texture: &Texture, bytes_per_row: i64, destination: &mut [u8]) -> Result<(), Error> {
    let read = crate::validation::admit_base_texture_read(&texture.texture_descriptor, bytes_per_row)?;
    let read = read.into_destination(destination)?;
    texture.resource.0.raw().read_texture_into(&read, destination)
}
/// Validates `epoch` against what was submitted; `Ok(true)` when it is already complete.
fn bound_epoch(operation: &str, queue: &Queue, epoch: i64) -> Result<bool, Error> {
    let q = queue.0.borrow();
    if q.dead { Err(make_error(operation, ErrorKind::StaleHandle, "queue is destroyed")) }
    else if epoch <= 0 || epoch > q.submitted { Err(make_error(operation, ErrorKind::InvalidArgument, "epoch was not submitted")) }
    else { Ok(epoch <= q.completed_epoch) }
}
/// Records the epochs finished through `epoch` and their GPU durations.
fn completed(queue: &Queue, epoch: i64) {
    let mut q = queue.0.borrow_mut();
    let found: Vec<(i64, f64)> = (q.completed_epoch + 1..=epoch).filter_map(|e| q.raw().gpu_duration(e).map(|d| (e, d))).collect();
    q.completed_epoch = epoch;
    for (e, d) in found {
        q.gpu_timing.gpu_seconds += d;
        q.durations.insert(0, (e, d));
    }
    q.durations.truncate(DURATION_CAPACITY);
}
pub fn complete_through(queue: &Queue, epoch: i64) -> Result<(), Error> {
    if bound_epoch("Backend.complete_through", queue, epoch)? { return Err(make_error("Backend.complete_through", ErrorKind::InvalidArgument, "completion epoch is invalid")); }
    let waited = queue.0.borrow().raw().complete_through(epoch);
    if waited.is_ok() { completed(queue, epoch); }
    waited
}
pub fn poll_through(queue: &Queue, epoch: i64) -> Result<bool, Error> {
    if bound_epoch("Backend.poll_through", queue, epoch)? { return Ok(true); }
    let done = queue.0.borrow().raw().poll_through(epoch)?;
    if done { completed(queue, epoch); }
    Ok(done)
}
pub fn completed_epoch(queue: &Queue) -> i64 {
    queue.0.borrow().completed_epoch
}
pub fn status(queue: &Queue, receipt: Receipt) -> Result<Status, Error> {
    if poll_through(queue, receipt.epoch)? { Ok(Status::Completed) } else { Ok(Status::Pending) }
}
pub fn configure(surface: &Surface, configuration: Configuration) -> Result<(), Error> {
    const OP: &str = "Backend.configure";
    if surface.0.dead.get() { return Err(make_error(OP, ErrorKind::StaleHandle, "surface is destroyed")); }
    if surface.0.frames.get() != 0 { return Err(make_error(OP, ErrorKind::InvalidState, "surface has acquired frames")); }
    let configuration = surface::admit_configuration(OP, configuration)?;
    surface.0.raw().configure(&configuration)?;
    *surface.0.configuration.borrow_mut() = configuration;
    Ok(())
}
pub fn acquire_sync(surface: &Surface) -> Result<AcquireResult, Error> {
    const OP: &str = "Backend.acquire_sync";
    if surface.0.dead.get() { return Err(make_error(OP, ErrorKind::StaleHandle, "surface is destroyed")); }
    if surface.0.frames.get() >= surface.0.configuration.borrow().max_acquired {
        return Err(make_error(OP, ErrorKind::Capacity, "acquired-frame capacity reached"));
    }
    match surface.0.raw().acquire_sync()? {
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
    surface.0.raw().discard(frame.0.raw.clone())?;
    frame.0.consumed.set(true);
    surface.0.frames.set(surface.0.frames.get() - 1);
    Ok(())
}

fn destroy_resource(resource: &Resource) -> Result<(), Error> {
    if resource.0.dead.get() { return Ok(()); }
    resource.0.raw().destroy()?;
    resource.0.dead.set(true);
    resource.0.raw.borrow_mut().take();
    resource.0.mapping.borrow_mut().take();
    resource.0.mapped.borrow_mut().take();
    handle::destroy(&resource.0.handle);
    resource.0.device.0.children.set(resource.0.device.0.children.get() - 1);
    Ok(())
}
pub fn destroy_buffer(buffer: &Buffer) -> Result<(), Error> { destroy_resource(&buffer.resource) }
pub fn destroy_texture(texture: &Texture) -> Result<(), Error> {
    destroy_resource(&texture.resource)?; texture.texture_descriptor.retire(); Ok(())
}
pub fn destroy_pipeline(pipeline: &Pipeline) -> Result<(), Error> {
    let p = &pipeline.0;
    if p.dead.get() { return Ok(()); }
    p.driver().destroy_pipeline()?;
    p.dead.set(true);
    p.driver.borrow_mut().take();
    p.device.0.children.set(p.device.0.children.get() - 1);
    if let Some(library) = &p.library { library.0.pipelines.set(library.0.pipelines.get() - 1); }
    Ok(())
}
pub fn destroy_queue(queue: &Queue) -> Result<(), Error> {
    let mut q = queue.0.borrow_mut();
    if q.dead { return Ok(()); }
    q.raw().destroy_queue()?;
    q.dead = true;
    q.raw.borrow_mut().take();
    let current = Rc::downgrade(&queue.0);
    q.device.0.queues.borrow_mut().retain(|entry| !Weak::ptr_eq(entry, &current));
    q.device.0.children.set(q.device.0.children.get() - 1);
    Ok(())
}
pub fn destroy_surface(surface: &Surface) -> Result<(), Error> {
    if surface.0.dead.get() { return Ok(()); }
    if surface.0.frames.get() != 0 { return Err(make_error("Backend.destroy_surface", ErrorKind::InvalidState, "surface has acquired frames")); }
    surface.0.raw().destroy_surface()?;
    surface.0.dead.set(true);
    surface.0.raw.borrow_mut().take();
    surface.0.device.0.children.set(surface.0.device.0.children.get() - 1);
    Ok(())
}
pub fn destroy_device(device: &Device) -> Result<(), Error> {
    if device.0.dead.get() { return Ok(()); }
    if device.0.children.get() != 0 { return Err(make_error("Backend.destroy_device", ErrorKind::InvalidState, "device has live children")); }
    device.0.raw().destroy_device()?;
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
    let raw = device.0.raw().create_library(shader)?;
    device.0.children.set(device.0.children.get() + 1);
    Ok(Library(Rc::new(LibraryInner { raw: RefCell::new(Some(raw)), device: device.clone(), dead: Cell::new(false), pipelines: Cell::new(0) })))
}
/// A background compile's state: None while compiling, then whether it compiled.
pub type Warm = Box<dyn Fn() -> Option<bool>>;

/// Starts `shader`'s compile in the driver's background (see `DriverDevice::warm_library`).
pub fn warm_library(device: &Device, shader: &Shader) -> Option<Warm> {
    live("Backend.warm_library", device).ok()?;
    device.0.raw().warm_library(shader)
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
    crate::shader::validate_bindings(interface)?;
    let constants = constants.unwrap_or_default();
    let function = crate::shader::admit_compute_function(entry.to_owned(), &constants)?;
    let raw = library.0.raw().create_compute_pipeline_in(&function, interface)?;
    let device = library.0.device.clone();
    device.0.children.set(device.0.children.get() + 1);
    library.0.pipelines.set(library.0.pipelines.get() + 1);
    Ok(Pipeline(Rc::new(PipelineInner {
        max_threads: raw.max_threads(), driver: RefCell::new(Some(raw.into())), device, library: Some(library.clone()), indirect: false, shape: PipelineShape::Compute, render: None, dead: Cell::new(false),
    })))
}
pub fn destroy_library(library: &Library) -> Result<(), Error> {
    const OP: &str = "Backend.destroy_library";
    if library.0.dead.get() { return Ok(()); }
    if library.0.pipelines.get() > 0 { return Err(make_error(OP, ErrorKind::InvalidState, "library has live pipelines")); }
    library.0.raw().destroy_library()?;
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
    let layout = device.0.raw().instance_layout(InstanceKind::UserIdInstances);
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
    let layout = device.0.raw().instance_layout(InstanceKind::MotionInstances);
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

fn span(stride: i64, count: i64) -> i64 {
    if count <= 0 || stride <= 0 || count > i64::MAX / stride { 0 } else { count * stride }
}
// Convert public arguments once; both state validation and drivers share the admitted layout.
fn admitted_build_of(device: &Device, descriptor: &AccelDescriptor, refit: bool)
    -> Result<(acceleration::AdmittedBuild, Vec<Buffer>, Vec<Accel>), Error> {
    use acceleration::{BuildGeometry as G, BuildIndex, BuildLayout, BuildMotion, BuildRange};
    const OP: &str = "Backend.create_accel";
    fn unsigned(value: i64) -> Result<u64, Error> {
        u64::try_from(value).map_err(|_| make_error(OP,ErrorKind::InvalidArgument,"acceleration layout arguments must be nonnegative"))
    }
    fn range(buffers: &mut Vec<Buffer>, lengths: &mut Vec<u64>, buffer: &Buffer, offset: i64, length: i64) -> Result<BuildRange, Error> {
        let value = BuildRange { binding:buffers.len(), offset:unsigned(offset)?, length:unsigned(length)? };
        lengths.push(unsigned(buffer.buffer_descriptor.size)?);
        buffers.push(buffer.clone());
        Ok(value)
    }
    fn frames(buffers: &mut Vec<Buffer>, lengths: &mut Vec<u64>, values: &[Keyframe], stride: i64, count: i64) -> Result<Vec<BuildRange>, Error> {
        values.iter().map(|frame| range(buffers,lengths,&frame.buffer,frame.offset,span(stride,count))).collect()
    }
    let mut buffers = Vec::new();
    let mut lengths = Vec::new();
    let mut structures = Vec::new();
    let layout = match descriptor {
        AccelDescriptor::Blas { geometries } | AccelDescriptor::MotionBlas { geometries, .. } => {
            // Preserve the portable surface's homogeneous BLAS policy. The direct advanced
            // metadata representation does not invent a restriction absent from its OS API.
            let kind = |g: &Geometry| match g {
                Geometry::Triangles { .. } | Geometry::MotionTriangles { .. } => 0,
                Geometry::BoundingBoxes { .. } => 1,
                Geometry::Curves { .. } => 2,
            };
            if let Some(first) = geometries.first()
                && geometries.iter().any(|g| kind(g) != kind(first)) {
                return Err(make_error(OP,ErrorKind::InvalidArgument,"BLAS geometries must all be one kind (triangles, bounding boxes, or curves)"));
            }
            let motion = match descriptor {
                AccelDescriptor::MotionBlas { motion, .. } => Some(BuildMotion {
                    keyframe_count:unsigned(motion.keyframes)?, start_time:motion.start_time,end_time:motion.end_time,
                    start_border:motion.start_border,end_border:motion.end_border,
                }),
                _ => None,
            };
            let mut converted = Vec::with_capacity(geometries.len());
            for geometry in geometries {
                let common = GeometryOptions { opaque:true,duplicate_intersection:false };
                let g = match geometry {
                    Geometry::Triangles { vertices,offset,length,vertex_stride,vertex_count } => {
                        if vertex_count % 3 != 0 {
                            return Err(make_error(OP,ErrorKind::InvalidArgument,"triangle vertices must form whole triangles"));
                        }
                        G::Triangles { vertices:vec![range(&mut buffers,&mut lengths,vertices,*offset,*length)?],
                            vertex_stride:unsigned(*vertex_stride)?,triangle_count:unsigned(*vertex_count)?/3,index:None,common }
                    }
                    Geometry::MotionTriangles { keyframes,vertex_stride,vertex_count } => {
                        // This source variant explicitly requests motion, including a malformed
                        // one-frame array; preserve its previous rejection in a static BLAS.
                        if motion.is_none() || vertex_count % 3 != 0 {
                            return Err(make_error(OP,ErrorKind::InvalidArgument,"motion triangles require a motion BLAS and whole triangles"));
                        }
                        G::Triangles { vertices:frames(&mut buffers,&mut lengths,keyframes,*vertex_stride,*vertex_count)?,
                            vertex_stride:unsigned(*vertex_stride)?,triangle_count:unsigned(*vertex_count)?/3,index:None,common }
                    }
                    Geometry::BoundingBoxes { boxes,stride,count,options } => G::BoundingBoxes {
                        boxes:frames(&mut buffers,&mut lengths,boxes,*stride,*count)?,stride:unsigned(*stride)?,count:unsigned(*count)?,common:*options,
                    },
                    Geometry::Curves { control_points,control_stride,control_point_count,radii,radius_stride,indices,index_offset,segment_count,control_points_per_segment,curve_type,basis,caps } => G::Curves {
                        control_points:frames(&mut buffers,&mut lengths,control_points,*control_stride,*control_point_count)?,
                        control_stride:unsigned(*control_stride)?,control_point_count:unsigned(*control_point_count)?,
                        radii:frames(&mut buffers,&mut lengths,radii,*radius_stride,*control_point_count)?,radius_stride:unsigned(*radius_stride)?,
                        index:BuildIndex { range:range(&mut buffers,&mut lengths,indices,*index_offset,span(4,*segment_count))?,uint16:false },
                        segment_count:unsigned(*segment_count)?,control_points_per_segment:u32::try_from(*control_points_per_segment)
                            .map_err(|_| make_error(OP,ErrorKind::InvalidArgument,"curve segment width exceeds its native encoding"))?,
                        curve_type:*curve_type,basis:*basis,end_caps:*caps,common,
                    },
                };
                converted.push(g);
            }
            BuildLayout::Primitive { geometries:converted,motion,refit,fast_build:false }
        }
        AccelDescriptor::TlasOf { instances,offset,instance_count,kind,structures:source,motion_transforms } => {
            let stride = device.0.raw().instance_layout(*kind).first().copied().unwrap_or(0);
            let instances = range(&mut buffers,&mut lengths,instances,*offset,span(stride,*instance_count))?;
            let transforms = match motion_transforms {
                None => None,
                Some((buffer,offset,count)) => Some((range(&mut buffers,&mut lengths,buffer,*offset,span(48,*count))?,unsigned(*count)?)),
            };
            structures.extend(source.iter().cloned());
            BuildLayout::Instances { instances,stride:unsigned(stride)?,count:unsigned(*instance_count)?,record_size:unsigned(stride)?,
                kind:*kind,structures:structures.len(),motion_transforms:transforms,refit }
        }
        AccelDescriptor::Sized { .. } => return Err(make_error(OP,ErrorKind::InvalidArgument,"a sized structure has no build descriptor")),
    };
    let admitted = acceleration::admit_build(&device.0.capabilities,lengths,layout)?;
    Ok((admitted,buffers,structures))
}

fn admitted_descriptors(device:&Device, descriptor:&AccelDescriptor, refit:bool) -> Result<(PortableDescriptor,DriverAccelDescriptor),Error> {
    match descriptor {
        AccelDescriptor::Sized { size,template } => {
            require("Backend.create_accel",device,Feature::RayTracing)?;
            if refit { return Err(make_error("Backend.create_accel",ErrorKind::InvalidArgument,"only a primitive structure is refittable")); }
            let size=crate::validation::admit_acceleration_size("Ogpu.Acceleration.create",
                u64::try_from(*size).map_err(|_|make_error("Ogpu.Acceleration.create",ErrorKind::InvalidArgument,"structure size must be positive"))?)?;
            Ok((PortableDescriptor::Sized { size,template:acceleration::handle(&template.0.portable.borrow()) },
                DriverAccelDescriptor::Sized { size,template:template.0.shared() }))
        }
        _ => {
            if refit && matches!(descriptor,AccelDescriptor::TlasOf { .. }) {
                return Err(make_error("Backend.create_accel",ErrorKind::InvalidArgument,"only a primitive structure is refittable"));
            }
            let (profile,buffers,structures)=admitted_build_of(device,descriptor,refit)?;
            let portable=PortableDescriptor::Build { profile:profile.clone(),
                buffers:buffers.iter().map(|b|b.resource.0.handle.clone()).collect(),
                structures:structures.iter().map(|a|acceleration::handle(&a.0.portable.borrow())).collect() };
            let driver=DriverAccelDescriptor::Build { profile,buffers:buffers.iter().map(|b|b.resource.0.shared()).collect(),
                structures:structures.iter().map(|a|a.0.shared()).collect() };
            Ok((portable,driver))
        }
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
    for buffer in descriptor_buffers(&descriptor) { resource_check(OP, device, &buffer.resource)?; }
    let structures = descriptor_structures(&descriptor);
    for structure in &structures {
        accel_check(OP, device, structure)?;
        if matches!(&descriptor, AccelDescriptor::TlasOf { .. })
            && matches!(acceleration::descriptor(&structure.0.portable.borrow()), PortableDescriptor::Build { profile,.. }
                if matches!(profile.layout(),acceleration::BuildLayout::Instances { .. }))
        {
            return Err(make_error(OP, ErrorKind::InvalidArgument, "TLAS instances must reference bottom-level structures"));
        }
    }
    let (portable_descriptor,driver_descriptor)=admitted_descriptors(device,&descriptor,refit)?;
    let portable=acceleration::create(&device.0.handle,portable_descriptor)?;
    let raw = match device.0.raw().create_accel(&driver_descriptor) {
        Ok(raw) => raw,
        Err(error) => { acceleration::destroy(&portable); return Err(error); }
    };
    device.0.children.set(device.0.children.get() + 1);
    Ok(Accel(Rc::new(AccelInner { sizes: raw.accel_sizes(), raw: RefCell::new(Some(raw.into())), device: device.clone(), portable: RefCell::new(portable), structures, dead: Cell::new(false) })))
}
pub fn destroy_accel(value: &Accel) -> Result<(), Error> {
    if value.0.dead.get() { return Ok(()); }
    value.0.raw().destroy_accel()?;
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
    let raw = q.raw().begin_commands()?;
    Ok(Commands(Rc::new(CommandsInner { raw: RefCell::new(Some(raw)), queue: queue.clone(), state: Cell::new(CommandsState::Recording), encoder: RefCell::new(None) })))
}
pub fn compute_encoder(commands: &Commands) -> Result<ComputeEncoder, Error> {
    const OP: &str = "Backend.compute_encoder";
    recording(OP, commands)?;
    let device = commands_device(commands);
    require(OP, &device, Feature::ComputePipeline)?;
    let raw = commands.0.raw().compute_encoder()?;
    commands.0.state.set(CommandsState::Encoding);
    let encoder = ComputeEncoder(Rc::new(ComputeInner { raw: RefCell::new(Some(raw)), commands: commands.clone(), max_threads: Cell::new(-1) }));
    *commands.0.encoder.borrow_mut() = Some(ActiveEncoder::Compute(Rc::downgrade(&encoder.0)));
    Ok(encoder)
}
fn open_compute(operation: &str, encoder: &ComputeEncoder) -> Result<(), Error> {
    // -1 is unbound; end/abandon use MIN. Nonnegative values are native pipeline limits.
    if encoder.0.max_threads.get() >= -1 { Ok(()) } else { Err(make_error(operation, ErrorKind::InvalidState, "compute encoder is ended")) }
}
pub fn set_pipeline(encoder: &ComputeEncoder, pipeline: &Pipeline) -> Result<(), Error> {
    const OP: &str = "Backend.set_pipeline";
    open_compute(OP, encoder)?;
    if pipeline.0.dead.get() { return Err(make_error(OP, ErrorKind::StaleHandle, "pipeline is destroyed")); }
    let device = commands_device(&encoder.0.commands);
    if !Rc::ptr_eq(&pipeline.0.device.0, &device.0) { return Err(make_error(OP, ErrorKind::CrossDevice, "pipeline belongs to another device")); }
    if pipeline.0.shape != PipelineShape::Compute {
        return Err(make_error(OP, ErrorKind::InvalidArgument, "render pipelines cannot be set on a compute encoder"));
    }
    encoder.0.raw().set_pipeline(&*pipeline.0.driver())?;
    encoder.0.max_threads.set(pipeline.0.max_threads);
    Ok(())
}
pub fn set_buffer(encoder: &ComputeEncoder, index: i64, buffer: &Buffer) -> Result<(), Error> {
    const OP: &str = "Backend.set_buffer";
    open_compute(OP, encoder)?;
    check_slot(OP, index)?;
    let device = commands_device(&encoder.0.commands);
    resource_check(OP, &device, &buffer.resource)?;
    if buffer.buffer_descriptor.size <= 0 { return Err(make_error(OP, ErrorKind::InvalidArgument, "buffer is empty")); }
    encoder.0.raw().set_buffer(index, 0, &*buffer.resource.0.raw())
}
pub fn set_bytes(encoder: &ComputeEncoder, index: i64, bytes: &[u8]) -> Result<(), Error> {
    const OP: &str = "Backend.set_bytes";
    open_compute(OP, encoder)?;
    check_slot(OP, index)?;
    let admitted=crate::validation::admit_bytes(OP,bytes)?;
    encoder.0.raw().set_bytes(index, admitted, bytes)
}
pub fn set_texture(encoder: &ComputeEncoder, index: i64, texture: &Texture) -> Result<(), Error> {
    const OP: &str = "Backend.set_texture";
    open_compute(OP, encoder)?;
    check_slot(OP, index)?;
    resource_check(OP, &commands_device(&encoder.0.commands), &texture.resource)?;
    if !texture.texture_descriptor.usage.iter().any(|usage| matches!(usage, TextureUsage::TextureBinding | TextureUsage::StorageBinding)) {
        return Err(make_error(OP, ErrorKind::InvalidArgument, "texture lacks shader binding usage"));
    }
    encoder.0.raw().set_texture(index, &*texture.resource.0.raw())
}
pub fn set_accel(encoder: &ComputeEncoder, index: i64, accel: &Accel) -> Result<(), Error> {
    const OP: &str = "Backend.set_accel";
    open_compute(OP, encoder)?;
    check_slot(OP, index)?;
    accel_check(OP, &commands_device(&encoder.0.commands), accel)?;
    if !is_built(accel) { return Err(make_error(OP, ErrorKind::InvalidState, "acceleration structure has no encoded build")); }
    encoder.0.raw().set_accel(index, &*accel.0.raw())
}
fn positive3(value: (i64, i64, i64)) -> bool { value.0 > 0 && value.1 > 0 && value.2 > 0 }
fn threadgroup(operation: &str, encoder: &ComputeEncoder, dimensions: (i64, i64, i64)) -> Result<(), Error> {
    let maximum = encoder.0.max_threads.get();
    if maximum < 0 { return Err(make_error(operation, ErrorKind::InvalidState, "no compute pipeline is set")); }
    if !positive3(dimensions) {
        return Err(make_error(operation, ErrorKind::InvalidArgument, "threadgroup dimensions must be positive"));
    }
    let count = dimensions.0.checked_mul(dimensions.1).and_then(|n| n.checked_mul(dimensions.2));
    if count.is_none_or(|count| count > maximum) {
        return Err(make_error(operation, ErrorKind::InvalidArgument, "threadgroup exceeds the pipeline's maximum total thread count"));
    }
    Ok(())
}
pub fn dispatch_threads(encoder: &ComputeEncoder, threads: (i64, i64, i64), group: (i64, i64, i64)) -> Result<(), Error> {
    const OP: &str = "Backend.dispatch_threads";
    open_compute(OP, encoder)?;
    threadgroup(OP, encoder, group)?;
    if !positive3(threads) { return Err(make_error(OP, ErrorKind::InvalidArgument, "dispatch dimensions must be positive")); }
    encoder.0.raw().dispatch_threads(threads, group)
}
pub fn dispatch_threadgroups_indirect(
    encoder: &ComputeEncoder, arguments: &Buffer, offset: i64, group: (i64, i64, i64),
) -> Result<(), Error> {
    const OP: &str = "Backend.dispatch_threadgroups_indirect";
    open_compute(OP, encoder)?;
    threadgroup(OP, encoder, group)?;
    resource_check(OP, &commands_device(&encoder.0.commands), &arguments.resource)?;
    if offset < 0 || offset % 4 != 0 || offset > arguments.buffer_descriptor.size - 12 {
        return Err(make_error(OP, ErrorKind::InvalidArgument, "indirect arguments range is invalid"));
    }
    encoder.0.raw().dispatch_indirect(&*arguments.resource.0.raw(), offset, group)
}
pub fn end_compute(encoder: &ComputeEncoder) -> Result<(), Error> {
    const OP: &str = "Backend.end_compute";
    open_compute(OP, encoder)?;
    encoder.0.raw().end_compute()?;
    encoder.0.max_threads.set(i64::MIN);
    encoder.0.raw.borrow_mut().take();
    encoder.0.commands.0.encoder.borrow_mut().take();
    encoder.0.commands.0.state.set(CommandsState::Recording);
    Ok(())
}

pub fn accel_encoder(commands: &Commands) -> Result<AccelEncoder, Error> {
    const OP: &str = "Backend.accel_encoder";
    recording(OP, commands)?;
    require(OP, &commands_device(commands), Feature::RayTracing)?;
    let raw = commands.0.raw().accel_encoder()?;
    commands.0.state.set(CommandsState::Encoding);
    let encoder = AccelEncoder(Rc::new(AccelEncoderInner { raw: RefCell::new(Some(raw)), commands: commands.clone(), open: Cell::new(true) }));
    *commands.0.encoder.borrow_mut() = Some(ActiveEncoder::Accel(Rc::downgrade(&encoder.0)));
    Ok(encoder)
}
fn open_accel(operation: &str, encoder: &AccelEncoder) -> Result<(), Error> {
    if encoder.0.open.get() { Ok(()) } else { Err(make_error(operation, ErrorKind::InvalidState, "acceleration encoder is ended")) }
}
fn check_scratch(operation: &str, device: &Device, scratch: &Buffer, offset: i64) -> Result<(), Error> {
    resource_check(operation, device, &scratch.resource)?;
    if offset < 0 || offset % 256 != 0 || offset > scratch.buffer_descriptor.size {
        Err(make_error(operation, ErrorKind::InvalidArgument, "scratch offset is invalid or unaligned"))
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
    check_scratch(OP, &device, scratch, offset)?;
    acceleration::validate_build(&device.0.handle, &accel.0.portable.borrow())?;
    encoder.0.raw().build(&*accel.0.raw(), &*scratch.resource.0.raw(), offset)?;
    acceleration::mark_built(&mut accel.0.portable.borrow_mut());
    Ok(())
}
pub fn refit_accel(
    encoder: &AccelEncoder, accel: &Accel, descriptor: AccelDescriptor, scratch: &Buffer, scratch_offset: Option<i64>,
) -> Result<(), Error> {
    const OP: &str = "Backend.refit_accel";
    let device = commands_device(&encoder.0.commands);
    if matches!(&descriptor, AccelDescriptor::TlasOf { .. } | AccelDescriptor::Sized { .. }) {
        return Err(make_error(OP, ErrorKind::InvalidArgument, "only a primitive structure refits"));
    }
    for buffer in descriptor_buffers(&descriptor) { resource_check(OP, &device, &buffer.resource)?; }
    open_accel(OP, encoder)?;
    accel_check(OP, &device, accel)?;
    let offset = scratch_offset.unwrap_or(0);
    // The new native descriptor determines refit scratch extent; the original size can be stale.
    // Core admits the logical buffer/offset; the native backend checks the newly queried extent.
    check_scratch(OP, &device, scratch, offset)?;
    let (portable,driver)=admitted_descriptors(&device,&descriptor,true)?;
    acceleration::refit(&device.0.handle, &mut accel.0.portable.borrow_mut(), &portable)?;
    encoder.0.raw().refit(&*accel.0.raw(), &driver, &*scratch.resource.0.raw(), offset)
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
    acceleration::validate_compaction(&device.0.handle, &src.0.portable.borrow(), &dst.0.portable.borrow())?;
    encoder.0.raw().compact(&*src.0.raw(), &*dst.0.raw())?;
    acceleration::mark_compacted(&mut dst.0.portable.borrow_mut());
    Ok(())
}
pub fn write_compacted_size(encoder: &AccelEncoder, accel: &Accel, dst: &Buffer, offset: Option<i64>) -> Result<(), Error> {
    const OP: &str = "Backend.write_compacted_size";
    open_accel(OP, encoder)?;
    let device = commands_device(&encoder.0.commands);
    accel_check(OP, &device, accel)?;
    resource_check(OP, &device, &dst.resource)?;
    let offset=u64::try_from(offset.unwrap_or(0)).map_err(|_|make_error(OP,ErrorKind::InvalidArgument,"compacted size destination needs eight aligned bytes"))?;
    let output=crate::validation::admit_compacted_size_write(OP,offset,dst.buffer_descriptor.size as u64,crate::validation::CompactedSizeType::Uint64)
        .map_err(|_|make_error(OP,ErrorKind::InvalidArgument,"compacted size destination needs eight aligned bytes"))?;
    acceleration::compacted_size(&device.0.handle, &accel.0.portable.borrow())?;
    encoder.0.raw().write_compacted_size(&*accel.0.raw(), &*dst.resource.0.raw(), output)
}
pub fn end_accel(encoder: &AccelEncoder) -> Result<(), Error> {
    const OP: &str = "Backend.end_accel";
    open_accel(OP, encoder)?;
    encoder.0.raw().end_accel()?;
    encoder.0.open.set(false);
    encoder.0.raw.borrow_mut().take();
    encoder.0.commands.0.encoder.borrow_mut().take();
    encoder.0.commands.0.state.set(CommandsState::Recording);
    Ok(())
}

pub fn blit_encoder(commands: &Commands) -> Result<BlitEncoder, Error> {
    const OP: &str = "Backend.blit_encoder";
    recording(OP, commands)?;
    let raw = commands.0.raw().blit_encoder()?;
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
    encoder.0.raw().end_blit()?;
    encoder.0.open.set(false);
    encoder.0.raw.borrow_mut().take();
    encoder.0.commands.0.encoder.borrow_mut().take();
    encoder.0.commands.0.state.set(CommandsState::Recording);
    Ok(())
}
/// A queue with `MAX_FRAMES_IN_FLIGHT` uncompleted submissions refuses another.
fn admit(operation: &str, queue: &Queue) -> Result<(), Error> {
    let q = queue.0.borrow();
    if q.dead { Err(make_error(operation, ErrorKind::StaleHandle, "queue is destroyed")) }
    else if q.submitted - q.completed_epoch >= MAX_FRAMES_IN_FLIGHT { Err(make_error(operation, ErrorKind::Capacity, "frames-in-flight capacity reached")) }
    else { Ok(()) }
}
pub fn commit(commands: &Commands) -> Result<Receipt, Error> {
    recording("Backend.commit", commands)?;
    admit("Backend.commit", &commands.0.queue)?;
    let receipt = commands.0.raw().commit()?;
    commands.0.queue.0.borrow_mut().submitted = receipt.epoch;
    commands.0.state.set(CommandsState::Committed);
    commands.0.raw.borrow_mut().take();
    Ok(receipt)
}
pub fn abandon(commands: &Commands) -> Result<(), Error> {
    match commands.0.state.get() {
        CommandsState::Committed => Err(make_error("Backend.abandon", ErrorKind::InvalidState, "commands are already committed")),
        CommandsState::Abandoned => Ok(()),
        CommandsState::Recording | CommandsState::Encoding => {
            commands.0.raw().abandon()?;
            commands.0.state.set(CommandsState::Abandoned);
            if let Some(encoder) = commands.0.encoder.borrow_mut().take() {
                match encoder {
                    ActiveEncoder::Compute(encoder) => if let Some(encoder) = encoder.upgrade() { encoder.max_threads.set(i64::MIN); encoder.raw.borrow_mut().take(); },
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
    queue.0.borrow().durations.iter().find(|(e, _)| *e == receipt.epoch).map(|(_, d)| *d)
}
pub fn gpu_timing(queue: &Queue) -> GpuTiming {
    queue.0.borrow().gpu_timing
}
pub fn compute_use_accels(encoder: &ComputeEncoder, accels: &[Accel]) -> Result<(), Error> {
    const OP: &str = "Backend.compute_use_accels";
    open_compute(OP, encoder)?;
    let device = commands_device(&encoder.0.commands);
    let mut shared = Vec::with_capacity(accels.len());
    for accel in accels {
        accel_check(OP, &device, accel)?;
        if !is_built(accel) { return Err(make_error(OP, ErrorKind::InvalidState, "acceleration structure has no encoded build")); }
        shared.push(accel.0.shared());
    }
    if shared.is_empty() { Ok(()) } else { encoder.0.raw().compute_use_accels(&shared) }
}

pub fn create_sampler(device: &Device, descriptor: SamplerDescriptor) -> Result<Sampler, Error> {
    const OP: &str = "Backend.create_sampler";
    live(OP, device)?;
    let descriptor = crate::validation::admit_sampler(crate::validation::SamplerProfile::ordinary(descriptor))?;
    let raw = device.0.raw().create_sampler(&descriptor)?;
    device.0.children.set(device.0.children.get() + 1);
    Ok(Sampler(Rc::new(SamplerInner { raw: RefCell::new(Some(raw)), device: device.clone(), dead: Cell::new(false) })))
}
pub fn destroy_sampler(sampler: &Sampler) -> Result<(), Error> {
    if sampler.0.dead.get() { return Ok(()); }
    sampler.0.raw().destroy_sampler()?;
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
    let admitted = pipeline::create_render(&device.0.capabilities, descriptor)?;
    let topology = topology.unwrap_or(Primitive::TriangleList);
    let indirect = indirect.unwrap_or(false);
    let options = RenderPipelineOptions { blend: blend.unwrap_or(Blend::Replace), topology, indirect };
    let admitted = admitted.with_options(options);
    let raw = device.0.raw().create_render_pipeline(options, descriptor, &admitted)?;
    device.0.children.set(device.0.children.get() + 1);
    Ok(Pipeline(Rc::new(PipelineInner {
        max_threads: raw.max_threads(), driver: RefCell::new(Some(raw.into())), device: device.clone(), library: None, indirect, shape: PipelineShape::Vertex, render: Some((descriptor.color_format, descriptor.sample_count)), dead: Cell::new(false),
    })))
}
pub fn pipeline_indirect(pipeline: &Pipeline) -> bool { pipeline.0.indirect }
pub fn create_icb(device: &Device, max_commands: i64) -> Result<Icb, Error> {
    const OP: &str = "Backend.create_icb";
    live(OP, device)?;
    let capacity = usize::try_from(max_commands).ok().filter(|&n| n <= 65_536)
        .ok_or_else(|| make_error(OP, ErrorKind::InvalidArgument, "indirect command capacity must be in 1..65536"))?;
    let mut descriptor = crate::validation::IcbDescriptor::new(vec![crate::validation::IcbCommandType::IndirectDraw, crate::validation::IcbCommandType::IndirectDrawIndexed]);
    descriptor.max_vertex_buffer_bind_count = 8;
    descriptor.max_fragment_buffer_bind_count = 8;
    let profile = crate::validation::admit_icb(capacity, descriptor)?;
    let raw = device.0.raw().create_icb(&profile)?;
    device.0.children.set(device.0.children.get() + 1);
    Ok(Icb(Rc::new(IcbInner { raw: RefCell::new(Some(raw)), device: device.clone(), capacity: max_commands, dead: Cell::new(false) })))
}
fn check_icb(operation: &str, icb: &Icb, index: i64) -> Result<crate::validation::AdmittedIcbIndex, Error> {
    if icb.0.dead.get() { return Err(make_error(operation, ErrorKind::StaleHandle, "indirect command buffer is destroyed")); }
    crate::validation::admit_icb_index(icb.0.capacity as usize,index)
}

pub fn icb_set_pipeline(icb: &Icb, index: i64, pipeline: &Pipeline) -> Result<(), Error> {
    const OP: &str = "Backend.icb_set_pipeline";
    let index = check_icb(OP, icb, index)?;
    if pipeline.0.dead.get() { return Err(make_error(OP, ErrorKind::StaleHandle, "pipeline is destroyed")); }
    if !Rc::ptr_eq(&pipeline.0.device.0, &icb.0.device.0) { return Err(make_error(OP, ErrorKind::CrossDevice, "pipeline belongs to another device")); }
    if pipeline.0.shape != PipelineShape::Vertex { return Err(make_error(OP, ErrorKind::InvalidArgument, "compute pipelines cannot draw")); }
    if !pipeline.0.indirect { return Err(make_error(OP, ErrorKind::Unsupported, "pipeline was not compiled for indirect command buffers")); }
    icb.0.raw().icb_set_pipeline(index, &*pipeline.0.driver())
}
pub fn icb_set_buffer(
    icb: &Icb, index: i64, stage: ShaderStage, slot: i64, offset: Option<i64>, buffer: &Buffer,
) -> Result<(), Error> {
    const OP: &str = "Backend.icb_set_buffer";
    let index = check_icb(OP, icb, index)?;
    check_slot(OP, slot)?;
    resource_check(OP, &icb.0.device, &buffer.resource)?;
    let offset = offset.unwrap_or(0);
    if offset < 0 || offset >= buffer.buffer_descriptor.size { return Err(make_error(OP, ErrorKind::InvalidArgument, "buffer offset is out of range")); }
    icb.0.raw().icb_set_buffer(index, stage, slot, offset, &*buffer.resource.0.raw())
}
fn valid_draw(operation: &str, primitive: Primitive, first: i64, count: i64, instances: i64) -> Result<(), Error> {
    if first < 0 || count <= 0 || instances <= 0 { return Err(make_error(operation, ErrorKind::InvalidArgument, "draw range is invalid")); }
    if primitive == Primitive::TriangleList && count % 3 != 0 {
        return Err(make_error(operation, ErrorKind::InvalidArgument, "triangle-list vertex count is not divisible by three"));
    }
    if primitive == Primitive::LineStrip && count < 2 {
        return Err(make_error(operation, ErrorKind::InvalidArgument, "line strips need at least two vertices"));
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
) -> Result<crate::validation::AdmittedIndexRange, Error> {
    resource_check(operation, device, &buffer.resource)?;
    let failure=||make_error(operation,ErrorKind::InvalidArgument,"index range is invalid");
    let offset=u64::try_from(offset).map_err(|_|failure())?;
    let count=u64::try_from(count).map_err(|_|failure())?;
    let instances=u64::try_from(instances).map_err(|_|failure())?;
    let range=crate::validation::admit_index_range(operation,index_type,offset,count,instances)?;
    let size=u64::try_from(buffer.buffer_descriptor.size).map_err(|_|failure())?;
    if offset>size || count>(size-offset)/range.element_bytes() {return Err(failure());}
    if primitive == Primitive::TriangleList && count % 3 != 0 {
        return Err(make_error(operation, ErrorKind::InvalidArgument, "triangle-list index count is not divisible by three"));
    }
    if primitive == Primitive::LineStrip && count < 2 {
        return Err(make_error(operation, ErrorKind::InvalidArgument, "line strips need at least two vertices"));
    }
    if primitive == Primitive::LineList && count % 2 != 0 {
        return Err(make_error(operation, ErrorKind::InvalidArgument, "line-list index count is not divisible by two"));
    }
    Ok(range)
}
pub fn icb_draw(
    icb: &Icb, index: i64, primitive: Primitive, first: i64, count: i64, instances: Option<i64>,
) -> Result<(), Error> {
    const OP: &str = "Backend.icb_draw";
    let index = check_icb(OP, icb, index)?;
    let instances = instances.unwrap_or(1);
    valid_draw(OP, primitive, first, count, instances)?;
    icb.0.raw().icb_draw(index, primitive, first, count, instances)
}
pub fn icb_draw_indexed(
    icb: &Icb, index: i64, primitive: Primitive, index_type: IndexType,
    buffer: &Buffer, offset: i64, count: i64, instances: Option<i64>,
) -> Result<(), Error> {
    const OP: &str = "Backend.icb_draw_indexed";
    let index = check_icb(OP, icb, index)?;
    let instances = instances.unwrap_or(1);
    let range=valid_indexed(OP, &icb.0.device, primitive, index_type, buffer, offset, count, instances)?;
    icb.0.raw().icb_draw_indexed(index, primitive, &*buffer.resource.0.raw(), &range)
}
pub fn destroy_icb(icb: &Icb) -> Result<(), Error> {
    if icb.0.dead.get() { return Ok(()); }
    icb.0.raw().destroy_icb()?;
    icb.0.dead.set(true);
    icb.0.raw.borrow_mut().take();
    icb.0.device.0.children.set(icb.0.device.0.children.get() - 1);
    Ok(())
}
pub fn create_argument(pipeline: &Pipeline, stage: ShaderStage, index: i64) -> Result<Argument, Error> {
    const OP: &str = "Backend.create_argument";
    if pipeline.0.dead.get() { return Err(make_error(OP, ErrorKind::StaleHandle, "pipeline is destroyed")); }
    live(OP, &pipeline.0.device)?;
    let index=crate::validation::admit_slot(OP,index)?;
    let raw = pipeline.0.device.0.raw().create_argument(&*pipeline.0.driver(), stage, index)?;
    pipeline.0.device.0.children.set(pipeline.0.device.0.children.get() + 1);
    Ok(Argument(Rc::new(ArgumentInner { length: raw.argument_length(), alignment: raw.argument_alignment(), raw: RefCell::new(Some(raw)), device: pipeline.0.device.clone(), dead: Cell::new(false) })))
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
    if slot < 0 { return Err(make_error(OP, ErrorKind::InvalidArgument, "argument slot is negative")); }
    resource_check(OP, &argument.0.device, &texture.resource)?;
    argument.0.raw().argument_texture(&*buffer.resource.0.raw(), offset, slot, &*texture.resource.0.raw())
}
pub fn argument_sampler(argument: &Argument, buffer: &Buffer, offset: i64, slot: i64, sampler: &Sampler) -> Result<(), Error> {
    const OP: &str = "Backend.argument_sampler";
    argument_target(OP, argument, buffer, offset)?;
    if slot < 0 { return Err(make_error(OP, ErrorKind::InvalidArgument, "argument slot is negative")); }
    if sampler.0.dead.get() { return Err(make_error(OP, ErrorKind::StaleHandle, "sampler is destroyed")); }
    if !Rc::ptr_eq(&sampler.0.device.0, &argument.0.device.0) { return Err(make_error(OP, ErrorKind::CrossDevice, "sampler belongs to another device")); }
    argument.0.raw().argument_sampler(&*buffer.resource.0.raw(), offset, slot, &*sampler.0.raw())
}
pub fn destroy_argument(argument: &Argument) -> Result<(), Error> {
    if argument.0.dead.get() { return Ok(()); }
    argument.0.raw().destroy_argument()?;
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
    if color.texture.texture_descriptor.format == TextureFormat::Depth32Float {
        return Err(make_error(OP, ErrorKind::InvalidArgument, "color attachment has a depth format"));
    }
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
            if !same_extent(resolve) || resolve.texture_descriptor.sample_count != 1 || resolve.texture_descriptor.format != color.texture.texture_descriptor.format {
                return Err(make_error(OP, ErrorKind::InvalidArgument, "resolve texture extent or sample count differs"));
            }
        }
        _ => {}
    }
    if let Some(depth) = &target.depth {
        attachment_check(OP, &device, &depth.depth_texture)?;
        if depth.depth_texture.texture_descriptor.format != TextureFormat::Depth32Float {
            return Err(make_error(OP, ErrorKind::InvalidArgument, "depth attachment requires a depth format"));
        }
        if !same_extent(&depth.depth_texture) || depth.depth_texture.texture_descriptor.sample_count != samples {
            return Err(make_error(OP, ErrorKind::InvalidArgument, "depth attachment extent or sample count differs"));
        }
    }
    let clear_color = crate::render_pass::admit_clear_color([color.clear.0,color.clear.1,color.clear.2,color.clear.3])?;
    use crate::render_pass::AttachmentStore;
    let color_store = match (&color.resolve,color.store) {
        (None,Store::Discard) => AttachmentStore::DontCare,
        (None,_) => AttachmentStore::Store,
        (Some(_),Store::Store) => AttachmentStore::StoreAndMultisampleResolve,
        (Some(_),_) => AttachmentStore::MultisampleResolve,
    };
    let (depth_load,depth_store,clear_depth) = match &target.depth {
        None => (Load::Clear,AttachmentStore::Store,1.),
        Some(d) => (d.depth_load,match d.depth_store { Store::Store => AttachmentStore::Store,
            Store::Discard => AttachmentStore::DontCare, Store::Resolve => AttachmentStore::MultisampleResolve },d.depth_clear),
    };
    let actions = crate::render_pass::admit_actions(crate::render_pass::Actions {
        color_load: color.load, color_store, depth_load, depth_store, clear_depth,
        stencil_load: Load::Clear, stencil_store: AttachmentStore::Store, clear_stencil: 0,
    })?;
    let driver_target = DriverRenderTarget(DriverRenderProfile {
        target_colors: vec![DriverColorAttachment {
            color: color.texture.resource.0.shared(),
            color_resolve: color.resolve.as_ref().map(|texture| texture.resource.0.shared()),
            color_load: color.load, color_store: color.store, color_clear: color.clear,
        }],
        target_depth: target.depth.as_ref().map(|depth| DriverDepthAttachment {
            depth: depth.depth_texture.resource.0.shared(),
            depth_load: depth.depth_load, depth_store: depth.depth_store, depth_clear: depth.depth_clear,
        }),
        target_width: width, target_height: height, target_samples: samples, clear_color, actions,
    });
    let raw = commands.0.raw().render_encoder(&driver_target)?;
    commands.0.state.set(CommandsState::Encoding);
    let encoder = RenderEncoder(Rc::new(RenderInner {
        raw: RefCell::new(Some(raw)), commands: commands.clone(), open: Cell::new(true), pipeline_set: Cell::new(false), indirect: Cell::new(false), target: (color.texture.texture_descriptor.format, samples.ilog2() as u8), extent: (width, height),
    }));
    *commands.0.encoder.borrow_mut() = Some(ActiveEncoder::Render(Rc::downgrade(&encoder.0)));
    Ok(encoder)
}

fn render_compatible(operation: &str, encoder: &RenderEncoder, pipeline: &Pipeline) -> Result<(), Error> {
    let Some((format, samples)) = pipeline.0.render else {
        return Err(make_error(operation, ErrorKind::InvalidArgument, "compute pipelines cannot be set on a render encoder"));
    };
    let format = match format {
        pipeline::ColorFormat::Rgba8Unorm => TextureFormat::Rgba8Unorm,
        pipeline::ColorFormat::Bgra8Unorm => TextureFormat::Bgra8Unorm,
    };
    // Validated sample counts are powers of two; the exponent is exact and fits padding.
    if (format, samples.ilog2() as u8) != encoder.0.target {
        return Err(make_error(operation, ErrorKind::InvalidArgument, "pipeline color format or sample count differs from the render pass"));
    }
    Ok(())
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
    render_compatible(OP, encoder, pipeline)?;
    encoder.0.raw().render_set_pipeline(&*pipeline.0.driver())?;
    encoder.0.pipeline_set.set(true);
    encoder.0.indirect.set(pipeline.0.indirect);
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
    encoder.0.raw().set_stage_buffer(stage, index, offset, &*buffer.resource.0.raw())
}
pub fn set_stage_texture(encoder: &RenderEncoder, stage: ShaderStage, index: i64, texture: &Texture) -> Result<(), Error> {
    const OP: &str = "Backend.set_stage_texture";
    open_render(OP, encoder)?;
    check_slot(OP, index)?;
    resource_check(OP, &render_device(encoder), &texture.resource)?;
    if !texture.texture_descriptor.usage.contains(&TextureUsage::TextureBinding) {
        return Err(make_error(OP, ErrorKind::InvalidArgument, "texture lacks Texture_binding usage"));
    }
    encoder.0.raw().set_stage_texture(stage, index, &*texture.resource.0.raw())
}
pub fn set_stage_sampler(encoder: &RenderEncoder, stage: ShaderStage, index: i64, sampler: &Sampler) -> Result<(), Error> {
    const OP: &str = "Backend.set_stage_sampler";
    open_render(OP, encoder)?;
    check_slot(OP, index)?;
    if sampler.0.dead.get() { return Err(make_error(OP, ErrorKind::StaleHandle, "sampler is destroyed")); }
    if !Rc::ptr_eq(&sampler.0.device.0, &render_device(encoder).0) {
        return Err(make_error(OP, ErrorKind::CrossDevice, "sampler belongs to another device"));
    }
    encoder.0.raw().set_stage_sampler(stage, index, &*sampler.0.raw())
}
pub fn set_viewport(encoder: &RenderEncoder, rect: Rect) -> Result<(), Error> {
    const OP: &str = "Backend.set_viewport";
    open_render(OP, encoder)?;
    let rect=crate::render_pass::admit_viewport_points(rect)?;
    encoder.0.raw().set_viewport(rect)
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
    let (width, height) = encoder.0.extent;
    if rect.x > width || rect.width > width - rect.x || rect.y > height || rect.height > height - rect.y {
        return Err(make_error(OP, ErrorKind::InvalidArgument, "scissor rectangle is outside the render pass"));
    }
    encoder.0.raw().set_scissor(rect)
}
pub fn set_cull(encoder: &RenderEncoder, cull: Cull) -> Result<(), Error> {
    open_render("Backend.set_cull", encoder)?;
    encoder.0.raw().set_cull(cull)
}
pub fn set_winding(encoder: &RenderEncoder, winding: Winding) -> Result<(), Error> {
    open_render("Backend.set_winding", encoder)?;
    encoder.0.raw().set_winding(winding)
}
pub fn set_depth_state(encoder: &RenderEncoder, state: Option<DepthState>) -> Result<(), Error> {
    open_render("Backend.set_depth_state", encoder)?;
    encoder.0.raw().set_depth_state(state)
}
fn vertex_pipeline(operation: &str, encoder: &RenderEncoder) -> Result<(), Error> {
    if !encoder.0.pipeline_set.get() { return Err(make_error(operation, ErrorKind::InvalidState, "no render pipeline is set")); }
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
    encoder.0.raw().draw(primitive, first, count, instances)
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
    encoder.0.raw().draw_indexed(primitive, index_type, &*buffer.resource.0.raw(), offset, count, instances)
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
    encoder.0.raw().draw_indexed_indirect(
        primitive, index_type, &*buffer.resource.0.raw(), offset,
        &*arguments.resource.0.raw(), arguments_offset,
    )
}
pub fn draw_batch(encoder: &RenderEncoder, draws: &[BatchDraw]) -> Result<(), Error> {
    const OP: &str = "Backend.draw_batch";
    open_render(OP, encoder)?;
    if draws.is_empty() { return Err(make_error(OP, ErrorKind::InvalidArgument, "draw batch is empty")); }
    let device = render_device(encoder);
    // lowered into storage kept between batches: the outer list and the inner buffer lists (a batch
    // per pass per frame allocated a list per draw); an error leaves the scratch to be made again.
    // Only the lists are kept; the driver handles in them are dropped with the batch.
    thread_local! { static LOWERED: std::cell::RefCell<(Vec<DriverBatchDraw>, Vec<Vec<(ShaderStage, i64, Rc<dyn DriverResource>, i64)>>)> = const { std::cell::RefCell::new((Vec::new(), Vec::new())) }; }
    let (mut lowered, mut lists) = LOWERED.with(|l| std::mem::take(&mut *l.borrow_mut()));
    lowered.clear();
    for draw in draws {
        if draw.pipeline.0.dead.get() { return Err(make_error(OP, ErrorKind::StaleHandle, "pipeline is destroyed")); }
        if !Rc::ptr_eq(&draw.pipeline.0.device.0, &device.0) {
            return Err(make_error(OP, ErrorKind::CrossDevice, "pipeline belongs to another device"));
        }
        render_compatible(OP, encoder, &draw.pipeline)?;
        let mut buffers = lists.pop().unwrap_or_default();
        buffers.clear();
        for (stage, index, buffer, offset) in &draw.buffers {
            check_slot(OP, *index)?;
            resource_check(OP, &device, &buffer.resource)?;
            if *offset < 0 || *offset >= buffer.buffer_descriptor.size {
                return Err(make_error(OP, ErrorKind::InvalidArgument, "buffer offset is out of range"));
            }
            buffers.push((*stage, *index, buffer.resource.0.shared(), *offset));
        }
        let index = if let Some((index_type, buffer, offset, count)) = &draw.index {
            valid_indexed(OP, &device, draw.primitive, *index_type, buffer, *offset, *count, draw.instances)?;
            Some((*index_type, buffer.resource.0.shared(), *offset, *count))
        } else {
            valid_draw(OP, draw.primitive, draw.vertex_start, draw.vertex_count, draw.instances)?;
            None
        };
        lowered.push(DriverBatchDraw {
            batch_pipeline: draw.pipeline.0.driver.borrow().clone().expect("live pipeline payload"),
            batch_buffers: buffers,
            batch_primitive: draw.primitive,
            batch_index: index,
            batch_vertex_start: draw.vertex_start,
            batch_vertex_count: draw.vertex_count,
            batch_instances: draw.instances,
        });
    }
    let result = encoder.0.raw().draw_batch(&lowered);
    lists.extend(lowered.drain(..).map(|draw| { let mut buffers = draw.batch_buffers; buffers.clear(); buffers }));
    LOWERED.with(|l| *l.borrow_mut() = (lowered, lists));
    result?;
    encoder.0.pipeline_set.set(true);
    encoder.0.indirect.set(draws.last().unwrap().pipeline.0.indirect);
    Ok(())
}
pub fn use_resources(encoder: &RenderEncoder, resources: &[ResourceUse]) -> Result<(), Error> {
    const OP: &str = "Backend.use_resources";
    open_render(OP, encoder)?;
    let device = render_device(encoder);
    // The handle list is kept between calls (one per pass per frame); the shared handles are dropped here.
    thread_local! { static USED: std::cell::RefCell<Vec<Rc<dyn DriverResource>>> = const { std::cell::RefCell::new(Vec::new()) }; }
    let mut used = USED.with(|u| std::mem::take(&mut *u.borrow_mut()));
    used.clear();
    let mut admitted = Ok(());
    for resource in resources {
        let (resource, shared) = match resource { ResourceUse::Buffer(b) => (&b.resource, b.resource.0.shared()), ResourceUse::Texture(t) => (&t.resource, t.resource.0.shared()) };
        if let Err(e) = resource_check(OP, &device, resource) { admitted = Err(e); break; }
        used.push(shared);
    }
    let result = admitted.and_then(|()| encoder.0.raw().use_resources(&used));
    used.clear();
    USED.with(|u| *u.borrow_mut() = used);
    result
}
pub fn execute_icb(encoder: &RenderEncoder, icb: &Icb, location: i64, length: i64) -> Result<(), Error> {
    const OP: &str = "Backend.execute_icb";
    open_render(OP, encoder)?;
    if !encoder.0.pipeline_set.get() { return Err(make_error(OP, ErrorKind::InvalidState, "no render pipeline is set")); }
    if !encoder.0.indirect.get() { return Err(make_error(OP, ErrorKind::Unsupported, "pipeline lacks indirect-command-buffer support")); }
    if icb.0.dead.get() { return Err(make_error(OP, ErrorKind::StaleHandle, "indirect command buffer is destroyed")); }
    if !Rc::ptr_eq(&icb.0.device.0, &render_device(encoder).0) {
        return Err(make_error(OP, ErrorKind::CrossDevice, "indirect command buffer belongs to another device"));
    }
    if location < 0 || length <= 0 || location > icb.0.capacity - length {
        return Err(make_error(OP, ErrorKind::InvalidArgument, "indirect command range is out of range"));
    }
    encoder.0.raw().execute_icb(&*icb.0.raw(), location, length)
}
pub fn end_render(encoder: &RenderEncoder) -> Result<(), Error> {
    const OP: &str = "Backend.end_render";
    open_render(OP, encoder)?;
    encoder.0.raw().end_render()?;
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
    if texture.texture_descriptor.sample_count != 1
        || !texture_range(&texture.texture_descriptor, mip, origin, extent)
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
    encoder.0.raw().buffer_to_texture(
        &*src.resource.0.raw(), offset, bytes_per_row, bytes_per_image, &*dst.resource.0.raw(), mip, origin, extent,
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
    if descriptor.depth != 1 || descriptor.mip_levels != 1 { return Err(make_error(operation, ErrorKind::InvalidArgument, "presentation source must be a single-mip 2D texture")); }
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
    admit(OP, &commands.0.queue)?;
    let receipt = commands.0.raw().commit_present(&*source.resource.0.raw(), frame.0.raw.clone())?;
    commands.0.queue.0.borrow_mut().submitted = receipt.epoch;
    commands.0.state.set(CommandsState::Committed);
    commands.0.raw.borrow_mut().take();
    frame.0.consumed.set(true);
    frame.0.surface.0.frames.set(frame.0.surface.0.frames.get() - 1);
    Ok(receipt)
}

#[cfg(test)]
mod ownership_tests {
    use super::*;

    /// Holds `_owner` so a test sees when the driver payload is dropped; `fail` makes the terminal call fail.
    struct Payload { _owner: Rc<()>, fail: Rc<Cell<bool>> }
    impl Payload {
        fn terminal(&self, operation: &str) -> Result<(), Error> {
            if self.fail.get() { Err(make_error(operation, ErrorKind::InvalidState, "busy")) } else { Ok(()) }
        }
    }
    impl DriverAccel for Payload {
        fn as_any(&self) -> &dyn std::any::Any { self }
        fn accel_sizes(&self) -> AccelSizes { AccelSizes {structure_size:64,build_scratch_size:64,refit_scratch_size:32} }
        fn destroy_accel(&self) -> Result<(), Error> { self.terminal("test.destroy") }
    }
    impl DriverResource for Payload {
        fn token(&self) -> Token { 42 }
        fn as_any(&self) -> &dyn std::any::Any { self }
        fn write(&self, _: i64, _: &[u8]) -> Result<(), Error> { Ok(()) }
        fn read(&self, _: i64, length: usize) -> Result<Vec<u8>, Error> { Ok(vec![7; length]) }
        fn read_into(&self, _: i64, destination: &mut [u8], offset: usize, length: usize) -> Result<(), Error> { destination[offset..offset + length].fill(7); Ok(()) }
        fn destroy(&self) -> Result<(), Error> { self.terminal("test.destroy") }
    }
    impl DriverPipeline for Payload {
        fn as_any(&self) -> &dyn std::any::Any { self }
        fn max_threads(&self) -> i64 { 64 }
        fn destroy_pipeline(&self) -> Result<(), Error> { self.terminal("test.destroy") }
    }
    impl DriverBlitEncoder for Payload {
        fn buffer_to_texture(&self, _: &dyn DriverResource, _: i64, _: i64, _: i64, _: &dyn DriverResource, _: i64, _: Origin, _: Extent) -> Result<(), Error> { unreachable!() }
        fn end_blit(&self) -> Result<(), Error> { self.terminal("test.end") }
    }
    impl DriverAccelEncoder for Payload {
        fn build(&self, _: &dyn DriverAccel, _: &dyn DriverResource, _: i64) -> Result<(), Error> { self.terminal("test.build") }
        fn compact(&self, _: &dyn DriverAccel, _: &dyn DriverAccel) -> Result<(), Error> { self.terminal("test.compact") }
        fn write_compacted_size(&self, _: &dyn DriverAccel, _: &dyn DriverResource, _: crate::validation::AdmittedCompactedSize) -> Result<(), Error> { unreachable!() }
        fn refit(&self, _: &dyn DriverAccel, _: &DriverAccelDescriptor, _: &dyn DriverResource, _: i64) -> Result<(), Error> { unreachable!() }
        fn end_accel(&self) -> Result<(), Error> { self.terminal("test.end") }
    }
    impl DriverCommands for Payload {
        fn compute_encoder(&self) -> Result<Box<dyn DriverComputeEncoder>, Error> { unreachable!() }
        fn accel_encoder(&self) -> Result<Box<dyn DriverAccelEncoder>, Error> { unreachable!() }
        fn blit_encoder(&self) -> Result<Box<dyn DriverBlitEncoder>, Error> { unreachable!() }
        fn render_encoder(&self, _: &DriverRenderTarget) -> Result<Box<dyn DriverRenderEncoder>, Error> { unreachable!() }
        fn commit(&self) -> Result<Receipt, Error> { self.terminal("test.commit").map(|()| Receipt { epoch: 1 }) }
        fn commit_present(&self, _: &dyn DriverResource, _: DriverFrame) -> Result<Receipt, Error> { unreachable!() }
        fn abandon(&self) -> Result<(), Error> { self.terminal("test.abandon") }
    }
    // Invalid input must stop at the portable boundary, before a backend sees a token.
    impl DriverComputeEncoder for Payload {
        fn set_pipeline(&self, _: &dyn DriverPipeline) -> Result<(), Error> { Ok(()) }
        fn set_buffer(&self, _: i64, _: i64, _: &dyn DriverResource) -> Result<(), Error> { Ok(()) }
        fn set_bytes(&self, _: i64, _: crate::validation::AdmittedBytes, _: &[u8]) -> Result<(), Error> { panic!("invalid bytes reached driver") }
        fn set_texture(&self, _: i64, _: &dyn DriverResource) -> Result<(), Error> { unreachable!() }
        fn set_accel(&self, _: i64, _: &dyn DriverAccel) -> Result<(), Error> { unreachable!() }
        fn dispatch_threads(&self, _: (i64, i64, i64), _: (i64, i64, i64)) -> Result<(), Error> { Ok(()) }
        fn dispatch_indirect(&self, _: &dyn DriverResource, _: i64, _: (i64, i64, i64)) -> Result<(), Error> { panic!("invalid indirect dispatch reached driver") }
        fn compute_use_accels(&self, _: &[Rc<dyn DriverAccel>]) -> Result<(), Error> { unreachable!() }
        fn end_compute(&self) -> Result<(), Error> { Ok(()) }
    }

    fn admitted_test_buffer(descriptor: BufferDescriptor) -> AdmittedBuffer {
        crate::validation::admit_buffer(&caps::minimum_m1(), BufferProfile { descriptor,
            storage: StorageMode::Shared, cpu_cache: CpuCacheMode::DefaultCache, kind: BufferKind::Ordinary }).unwrap()
    }
    fn admitted_test_texture(descriptor: TextureDescriptor) -> AdmittedTexture {
        crate::validation::admit_texture(&caps::minimum_m1(), crate::validation::TextureProfile::ordinary(
            &caps::minimum_m1(), descriptor, false, false)).unwrap()
    }

    #[test]
    fn encoding_arguments_are_validated_before_driver_entry() {
        #[cfg(target_pointer_width = "64")]
        {
            assert_eq!(std::mem::size_of::<ComputeInner>(), 40);
            assert_eq!(std::mem::size_of::<RenderInner>(), 56);
        }
        assert!(valid_draw("test", Primitive::LineStrip, 0, 1, 1).is_err());
        valid_draw("test", Primitive::LineStrip, 0, 2, 1).unwrap();
        let device = device();
        let payload = || Payload { _owner: Rc::new(()), fail: Rc::new(Cell::new(false)) };
        let queue = Queue(Rc::new(RefCell::new(QueueInner {
            raw: RefCell::new(None), device: device.clone(), dead: false, submitted: 0, durations: Vec::new(), completed_epoch: 0,
            gpu_timing: GpuTiming { gpu_seconds: 0. },
        })));
        let commands = Commands(Rc::new(CommandsInner {
            raw: RefCell::new(None), queue, state: Cell::new(CommandsState::Encoding), encoder: RefCell::new(None),
        }));
        let encoder = ComputeEncoder(Rc::new(ComputeInner {
            raw: RefCell::new(Some(Box::new(payload()))), commands: commands.clone(), max_threads: Cell::new(-1),
        }));
        let pipeline = |shape, render, indirect| Pipeline(Rc::new(PipelineInner {
            driver: RefCell::new(Some(Rc::new(payload()))), max_threads: 64, device: device.clone(),
            library: None, indirect, shape, render, dead: Cell::new(false),
        }));
        let compute = pipeline(PipelineShape::Compute, None, false);
        let render = pipeline(PipelineShape::Vertex, Some((pipeline::ColorFormat::Rgba8Unorm, 1)), false);
        let buffer = Buffer {
            resource: make_resource(&device, Box::new(payload())), memory: Memory::Shared,
            buffer_descriptor: admitted_test_buffer(BufferDescriptor { label: None, size: 16, usage: vec![BufferUsage::Storage] }),
        };
        let reject = |kind, result: Result<(), Error>| assert_eq!(result.unwrap_err().kind, kind);
        // No driver payload: every invalid range must stop before attempting driver entry.
        let icb=Icb(Rc::new(IcbInner {raw:RefCell::new(None),device:device.clone(),capacity:1,dead:Cell::new(false)}));
        for (offset,count,instances) in [(-1,3,1),(2,3,1),(0,0,1),(0,3,0),(0,4,1),(0,6,1)] {
            reject(ErrorKind::InvalidArgument,icb_draw_indexed(&icb,0,Primitive::TriangleList,IndexType::Uint32,&buffer,offset,count,Some(instances)));
        }
        // Empty ordinary use is a no-op, even with no pipeline; the fake Driver would panic.
        compute_use_accels(&encoder, &[]).unwrap();
        assert_eq!(crate::validation::validate_acceleration_use("test", 0).unwrap_err().kind, ErrorKind::InvalidArgument);
        crate::validation::validate_acceleration_use("test", usize::MAX).unwrap();
        reject(ErrorKind::InvalidState, dispatch_threads(&encoder, (1, 1, 1), (1, 1, 1)));
        reject(ErrorKind::InvalidArgument, set_pipeline(&encoder, &render));
        set_pipeline(&encoder, &compute).unwrap();
        set_buffer(&encoder, 30, &buffer).unwrap();
        for slot in [-1, 31, 32] { reject(ErrorKind::InvalidArgument, set_buffer(&encoder, slot, &buffer)); }
        let clone = buffer.clone();
        buffer.resource.0.dead.set(true);
        reject(ErrorKind::StaleHandle, set_buffer(&encoder, 0, &clone));
        buffer.resource.0.dead.set(false);
        let other = Buffer { resource: make_resource(&self::device(), Box::new(payload())), ..buffer.clone() };
        reject(ErrorKind::CrossDevice, set_buffer(&encoder, 0, &other));
        reject(ErrorKind::InvalidArgument, set_bytes(&encoder, 0, &[]));
        reject(ErrorKind::InvalidArgument, set_bytes(&encoder, 0, &[0; 4097]));
        for group in [(0, 1, 1), (65, 1, 1), (i64::MAX, 2, 2)] {
            reject(ErrorKind::InvalidArgument, dispatch_threads(&encoder, (1, 1, 1), group));
            reject(ErrorKind::InvalidArgument, dispatch_threadgroups_indirect(&encoder, &buffer, 0, group));
        }
        reject(ErrorKind::InvalidArgument, dispatch_threads(&encoder, (0, 1, 1), (1, 1, 1)));
        reject(ErrorKind::InvalidArgument, dispatch_threadgroups_indirect(&encoder, &buffer, 8, (1, 1, 1)));
        for offset in [-1, 9, i64::MAX] {
            reject(ErrorKind::InvalidArgument, write_buffer(&buffer, offset, &[1; 8]));
        }
        for (offset, length) in [(-1, 1), (0, -1), (15, 2), (0, i64::MAX), (i64::MAX, i64::MAX)] {
            assert_eq!(read_buffer(&buffer, offset, length).unwrap_err().kind, ErrorKind::InvalidArgument);
        }
        let private = Buffer { memory: Memory::DeviceLocal, ..buffer.clone() };
        reject(ErrorKind::Unsupported, write_buffer(&private, 0, &[1]));
        assert_eq!(read_buffer(&private, 0, 1).unwrap_err().kind, ErrorKind::Unsupported);
        clone.resource.0.dead.set(true);
        reject(ErrorKind::StaleHandle, write_buffer(&clone, 0, &[1]));
        assert_eq!(read_buffer(&clone, 0, 1).unwrap_err().kind, ErrorKind::StaleHandle);
        clone.resource.0.dead.set(false);
        compute.0.dead.set(true); // The already-bound submission retains its native pipeline.
        dispatch_threads(&encoder, (4, 1, 1), (64, 1, 1)).unwrap();
        end_compute(&encoder).unwrap();
        reject(ErrorKind::InvalidState, set_buffer(&encoder, 0, &buffer));
        reject(ErrorKind::InvalidState, compute_use_accels(&encoder, &[]));
        let icb = Icb(Rc::new(IcbInner { raw: RefCell::new(None), device: device.clone(), capacity: 1, dead: Cell::new(false) }));
        compute.0.dead.set(false);
        reject(ErrorKind::InvalidArgument, icb_set_pipeline(&icb, 0, &compute));
        reject(ErrorKind::Unsupported, icb_set_pipeline(&icb, 0, &render));
        let pass = RenderEncoder(Rc::new(RenderInner {
            raw: RefCell::new(None), commands, open: Cell::new(true), pipeline_set: Cell::new(false),
            indirect: Cell::new(false), target: (TextureFormat::Rgba8Unorm, 0), extent: (4, 4),
        }));
        pass.0.commands.0.state.set(CommandsState::Encoding);
        render_compatible("test", &pass, &render).unwrap();
        let error = set_scissor(&pass, Rect { x: 3, y: 0, width: 2, height: 4 }).unwrap_err();
        assert_eq!(error.kind, ErrorKind::InvalidArgument);
        assert_eq!(error.operation, "Backend.set_scissor");
        assert_eq!(error.message, "scissor rectangle is outside the render pass");
        // Command sequencing rejects before the fake DriverCommands payload is inspected.
        assert_eq!(blit_encoder(&pass.0.commands).err().unwrap().kind, ErrorKind::InvalidState);
        assert_eq!(compute_encoder(&pass.0.commands).err().unwrap().kind, ErrorKind::InvalidState);
        assert_eq!(commit(&pass.0.commands).unwrap_err().kind, ErrorKind::InvalidState);

        reject(ErrorKind::InvalidState, draw(&pass, Primitive::TriangleList, 0, 3, Some(1)));
        reject(ErrorKind::InvalidArgument, set_stage_buffer(&pass, ShaderStage::Vertex, 0, Some(17), &buffer));
        pass.0.pipeline_set.set(true);
        reject(ErrorKind::Unsupported, execute_icb(&pass, &icb, 0, 1));
        pass.0.indirect.set(true);
        icb.0.dead.set(true);
        reject(ErrorKind::StaleHandle, execute_icb(&pass, &icb, 0, 1));
        icb.0.dead.set(false);
        reject(ErrorKind::InvalidArgument, execute_icb(&pass, &icb, 1, 1));
        let argument = Argument(Rc::new(ArgumentInner {
            raw: RefCell::new(None), length: 0, alignment: 1, device: device.clone(), dead: Cell::new(false),
        }));
        let sampler = Sampler(Rc::new(SamplerInner { raw: RefCell::new(None), device: device.clone(), dead: Cell::new(false) }));
        reject(ErrorKind::InvalidArgument, argument_sampler(&argument, &buffer, 0, -1, &sampler));

        for wrong in [
            pipeline(PipelineShape::Vertex, Some((pipeline::ColorFormat::Bgra8Unorm, 1)), false),
            pipeline(PipelineShape::Vertex, Some((pipeline::ColorFormat::Rgba8Unorm, 4)), false),
            compute,
        ] { reject(ErrorKind::InvalidArgument, render_compatible("test", &pass, &wrong)); }
    }

    #[test]
    fn texture_and_sampler_arguments_are_checked_before_driver_entry() {
        let device = device();
        let texture = Texture {
            resource: make_resource(&device, Box::new(Payload { _owner: Rc::new(()), fail: Rc::new(Cell::new(false)) })),
            texture_descriptor: admitted_test_texture(TextureDescriptor { label: None, width: 4, height: 4, depth: 1, mip_levels: 1,
                sample_count: 1, format: TextureFormat::Rgba8Unorm, usage: vec![TextureUsage::RenderAttachment] }),
        };
        for row in [-1, 0, 15, 17, i64::MAX] {
            assert_eq!(read_texture(&texture, row).unwrap_err().kind, ErrorKind::InvalidArgument);
            assert_eq!(read_texture_into(&texture, row, &mut [0; 64]).unwrap_err().kind, ErrorKind::InvalidArgument);
        }
        for length in [63,65] {
            let mut destination = vec![0xa5; length];
            assert_eq!(read_texture_into(&texture, 16, &mut destination).unwrap_err().kind, ErrorKind::InvalidArgument);
            assert_eq!(destination, vec![0xa5; length]);
        }
        assert_eq!(read_texture(&texture, 16).unwrap(), [7; 64]);
        let buffer = Buffer { resource: texture.resource.clone(), memory: Memory::Shared,
            buffer_descriptor: admitted_test_buffer(BufferDescriptor { label: None, size: 1024, usage: vec![BufferUsage::CopySrc] }) };

        let mut depth = texture.clone();
        let mut descriptor = depth.texture_descriptor.descriptor.clone();
        descriptor.format = TextureFormat::Depth32Float;
        depth.texture_descriptor = admitted_test_texture(descriptor);
        assert_eq!(read_texture(&depth, 16).unwrap_err().kind, ErrorKind::Unsupported);
        let queue = Queue(Rc::new(RefCell::new(QueueInner { raw: RefCell::new(None), device: device.clone(), dead: false,
            submitted: 0, durations: Vec::new(), completed_epoch: 0, gpu_timing: GpuTiming { gpu_seconds: 0. } })));
        let commands = Commands(Rc::new(CommandsInner { raw: RefCell::new(None), queue,
            state: Cell::new(CommandsState::Recording), encoder: RefCell::new(None) }));
        let blit = BlitEncoder(Rc::new(BlitInner { raw: RefCell::new(None), commands: commands.clone(), open: Cell::new(false) }));
        assert_eq!(buffer_to_texture(&blit, &buffer, None, 256, 1024, &texture, None, None, Extent { width: 4, height: 4, depth: 1 }).unwrap_err().kind, ErrorKind::InvalidState);
        assert_eq!(end_blit(&blit).unwrap_err().kind, ErrorKind::InvalidState);
        let color = |texture| ColorAttachment { texture, resolve: None, load: Load::Clear, store: Store::Store, clear: (0.,0.,0.,1.) };
        assert_eq!(render_encoder(&commands, &RenderTarget { colors: vec![color(depth.clone())], depth: None }).err().unwrap().kind, ErrorKind::InvalidArgument);
        assert_eq!(render_encoder(&commands, &RenderTarget { colors: vec![color(texture.clone())], depth: Some(DepthAttachment {
            depth_texture: texture, depth_load: Load::Clear, depth_store: Store::Store, depth_clear: 1. }) }).err().unwrap().kind, ErrorKind::InvalidArgument);
        let sampler = SamplerDescriptor { label: None, min_filter: types::SamplerFilter::Nearest, mag_filter: types::SamplerFilter::Nearest,
            mip_filter: types::MipFilter::NoMip, address_u: types::AddressMode::ClampToEdge, address_v: types::AddressMode::ClampToEdge,
            lod_min: 0., lod_max: f64::MAX, max_anisotropy: 1 };
        assert_eq!(create_sampler(&device, sampler).err().unwrap().kind, ErrorKind::InvalidArgument);
    }

    #[test]
    fn acceleration_geometry_is_checked_before_native_descriptor_creation() {
        let mut device = device();
        let caps = &mut Rc::get_mut(&mut device.0).unwrap().capabilities;
        caps.ray_tracing = true;
        caps.ray_tracing_motion = true;
        caps.ray_tracing_curves = true;
        let buffer = Buffer {
            resource: make_resource(&device, Box::new(Payload { _owner: Rc::new(()), fail: Rc::new(Cell::new(false)) })),
            memory: Memory::Shared,
            buffer_descriptor: admitted_test_buffer(BufferDescriptor { label: None, size: 36, usage: vec![BufferUsage::Storage] }),
        };
        let invalid = AccelDescriptor::Blas { geometries: vec![Geometry::Triangles { vertices: buffer.clone(), offset: 0, length: 36, vertex_stride: 12, vertex_count: 6 }] };
        let error = create_accel(&device, invalid, false).err().expect("oversized triangle rejected before native entry");
        assert_eq!(error.kind, ErrorKind::InvalidArgument);
        assert_eq!(error.operation, "Ogpu.Acceleration.admit_build");
        for (start_time, end_time) in [(f64::MAX / 2., f64::MAX), (1., 1. + f64::EPSILON)] {
            let descriptor = AccelDescriptor::MotionBlas { geometries: Vec::new(), motion: Motion { keyframes: 2, start_time, end_time, start_border: Border::Clamp, end_border: Border::Clamp } };
            let error = create_accel(&device, descriptor, false).err().expect("narrowed motion time rejected");
            assert_eq!(error.kind, ErrorKind::InvalidArgument);
            assert_eq!(error.operation, "Ogpu.Acceleration.admit_build");
            assert_eq!(error.message, "motion keyframes or time range are invalid");
        }
        let mixed=AccelDescriptor::Blas {geometries:vec![
            Geometry::Triangles {vertices:buffer.clone(),offset:0,length:36,vertex_stride:12,vertex_count:3},
            Geometry::BoundingBoxes {boxes:vec![Keyframe {buffer:buffer.clone(),offset:0}],stride:24,count:1,options:DEFAULT_GEOMETRY_OPTIONS},
        ]};
        assert_eq!(create_accel(&device,mixed,false).err().unwrap().kind,ErrorKind::InvalidArgument);
        use acceleration::{BuildGeometry,BuildIndex,BuildLayout,BuildRange};
        let range=|length|BuildRange {binding:0,offset:0,length};
        let profile=acceleration::admit_build(&device.0.capabilities,vec![36],BuildLayout::Primitive {geometries:vec![BuildGeometry::Curves {
            control_points:vec![range(24)],control_stride:12,control_point_count:2,radii:vec![range(8)],radius_stride:4,
            index:BuildIndex {range:range(4),uint16:false},segment_count:1,control_points_per_segment:2,
            curve_type:CurveType::RoundCurve,basis:CurveBasis::LinearBasis,end_caps:CurveCaps::NoCaps,common:DEFAULT_GEOMETRY_OPTIONS,
        }],motion:None,refit:false,fast_build:false}).unwrap();
        let curve=acceleration::create(&device.0.handle,PortableDescriptor::Build {profile,buffers:vec![buffer.resource.0.handle.clone()],structures:vec![]})
            .expect("one index per curve segment");
        acceleration::destroy(&curve);
    }

    struct TestMapping { allocation: Arc<Vec<u8>>, _view_owner: Rc<()> }
    impl DriverMapping for TestMapping {
        fn readback(&self) -> OwnedReadback {
            let allocation = self.allocation.clone();
            Arc::new(move |offset, length| Ok(allocation[offset as usize..offset as usize + length].to_vec()))
        }
    }
    struct IdleLibrary;
    impl DriverLibrary for IdleLibrary {
        fn create_compute_pipeline_in(&self, _: &crate::shader::AdmittedFunction, _: &[Binding]) -> Result<Box<dyn DriverPipeline>, Error> { unreachable!() }
        fn destroy_library(&self) -> Result<(), Error> { Ok(()) }
    }

    fn device() -> Device {
        Device(Rc::new(DeviceInner {
            raw: RefCell::new(None), capabilities: caps::minimum_m1(), handle: handle::create_device(),
            children: Cell::new(0), dead: Cell::new(false), queues: RefCell::new(Vec::new()),
        }))
    }

    #[test]
    fn rejected_acceleration_build_and_compaction_can_retry() {
        let device=device();
        let queue=Queue(Rc::new(RefCell::new(QueueInner {
            raw: RefCell::new(None),device:device.clone(),dead:false,submitted: 0, durations: Vec::new(), completed_epoch: 0,gpu_timing:GpuTiming {gpu_seconds:0.},
        })));
        let commands=Commands(Rc::new(CommandsInner {
            raw:RefCell::new(None),queue,state:Cell::new(CommandsState::Encoding),encoder:RefCell::new(None),
        }));
        let fail=Rc::new(Cell::new(true));
        let encoder=AccelEncoder(Rc::new(AccelEncoderInner {
            raw:RefCell::new(Some(Box::new(Payload {_owner:Rc::new(()),fail:fail.clone()}))),commands,open:Cell::new(true),
        }));
        let resource=make_resource(&device,Box::new(Payload {_owner:Rc::new(()),fail:Rc::new(Cell::new(false))}));
        let scratch=Buffer {resource,buffer_descriptor:admitted_test_buffer(BufferDescriptor {
            label:None,size:256,usage:vec![BufferUsage::Storage],
        }),memory:Memory::Shared};
        let mut caps=device.0.capabilities;caps.ray_tracing=true;
        let profile=acceleration::admit_build(&caps,vec![256],acceleration::BuildLayout::Primitive {geometries:vec![
            acceleration::BuildGeometry::Triangles {vertices:vec![acceleration::BuildRange {binding:0,offset:0,length:36}],vertex_stride:12,
                triangle_count:1,index:None,common:DEFAULT_GEOMETRY_OPTIONS}],motion:None,refit:false,fast_build:false}).unwrap();
        let portable=acceleration::create(&device.0.handle,PortableDescriptor::Build {profile,buffers:vec![scratch.resource.0.handle.clone()],structures:vec![]}).unwrap();
        let destination=acceleration::create(&device.0.handle,PortableDescriptor::Sized {
            size:std::num::NonZeroU64::new(64).unwrap(),template:acceleration::handle(&portable),
        }).unwrap();
        let make=|portable| Accel(Rc::new(AccelInner {
            raw:RefCell::new(Some(Rc::new(Payload {_owner:Rc::new(()),fail:fail.clone()}))),sizes:AccelSizes {structure_size:64,build_scratch_size:64,refit_scratch_size:32},
            device:device.clone(),portable:RefCell::new(portable),structures:vec![],dead:Cell::new(false),
        }));
        let source=make(portable); let destination=make(destination);
        assert_eq!(build_accel(&encoder,&source,&scratch,None).unwrap_err().operation,"test.build");
        assert!(!is_built(&source),"rejected driver build marked the structure built");
        fail.set(false); build_accel(&encoder,&source,&scratch,None).unwrap();
        assert!(is_built(&source));
        assert_eq!(build_accel(&encoder,&source,&scratch,None).unwrap_err().kind,ErrorKind::InvalidState);
        fail.set(true);
        assert_eq!(compact_accel(&encoder,&source,&destination).unwrap_err().operation,"test.compact");
        assert!(is_built(&source)); assert!(!is_built(&destination),"rejected compaction filled its destination");
        fail.set(false); compact_accel(&encoder,&source,&destination).unwrap();
        assert!(is_built(&source)); assert!(is_built(&destination));
        assert_eq!(compact_accel(&encoder,&source,&destination).unwrap_err().kind,ErrorKind::InvalidState);
    }

    #[test]
    fn destroy_releases_callbacks_but_not_escaped_readback_and_rolls_back_failure() {
        let device = device();
        let owner = Rc::new(());
        let weak = Rc::downgrade(&owner);
        let fail = Rc::new(Cell::new(true));
        let resource = make_resource(&device, Box::new(Payload { _owner: owner, fail: fail.clone() }));
        device.0.children.set(1);
        let view_owner = Rc::new(());
        let view_weak = Rc::downgrade(&view_owner);
        let allocation = Arc::new(vec![7u8; 8]);
        let allocation_weak = Arc::downgrade(&allocation);
        *resource.0.mapping.borrow_mut() = Some(Box::new(TestMapping { allocation, _view_owner: view_owner }));
        *resource.0.mapped.borrow_mut() = Some(MappedHost { view: RefCell::new(None), released: Cell::new(false), float_count: 2 });
        let buffer = Buffer { resource, buffer_descriptor: admitted_test_buffer(BufferDescriptor { label: None, size: 8, usage: vec![BufferUsage::CopyDst] }), memory: Memory::Shared };
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
            raw: RefCell::new(Some(Box::new(IdleLibrary))),
            device: device.clone(), dead: Cell::new(false), pipelines: Cell::new(1),
        }));
        let fail = Rc::new(Cell::new(true));
        let pipeline = Pipeline(Rc::new(PipelineInner {
            driver: RefCell::new(Some(Rc::new(Payload { _owner: owner, fail: fail.clone() }))),
            max_threads: 64, device: device.clone(), library: Some(library.clone()),
            indirect: false, shape: PipelineShape::Compute, render: None, dead: Cell::new(false),
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
            raw: RefCell::new(None), device, dead: false, submitted: 0, durations: Vec::new(), completed_epoch: 0, gpu_timing: GpuTiming { gpu_seconds: 0.0 },
        })));
        let make_commands = |owner: Rc<()>, fail: Rc<Cell<bool>>| {
            Commands(Rc::new(CommandsInner {
                raw: RefCell::new(Some(Box::new(Payload { _owner: owner, fail }))),
                queue: queue.clone(), state: Cell::new(CommandsState::Recording), encoder: RefCell::new(None),
            }))
        };
        let make_encoder = |commands: &Commands, owner: Rc<()>, fail: Rc<Cell<bool>>| {
            let encoder = BlitEncoder(Rc::new(BlitInner {
                raw: RefCell::new(Some(Box::new(Payload { _owner: owner, fail }))),
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
