//! Faults and command observations wrap the selected native driver;
//! compute and raster commands still execute on the real GPU. Rust's !Send handles enforce the
//! oracle's owner-thread restrictions at compile time rather than transporting Rc across threads.
use crate::common;
use common::test_program as tp;
use flow_gpu::{image_sink as s, pipelines, run};
use flow_ir::packed::host as phost;
use flow_shader::emit;
use ogpu_core::backend::{self as b, Driver};
use ogpu_core::error::{self, ErrorKind};
use ogpu_core::types::{TextureFormat, TextureUsage};
use std::cell::{Cell, RefCell};
use std::rc::Rc;


// Handles are deliberately not Send: safe code cannot move them to a foreign owner thread.
const _: fn() = || {
    trait Ambiguous<A> { fn check() {} }
    impl<T: ?Sized> Ambiguous<()> for T {}
    impl<T: ?Sized + Send> Ambiguous<u8> for T {}
    let _ = <s::T as Ambiguous<_>>::check;
    let _ = <s::Output as Ambiguous<_>>::check;
    let _ = <run::Output as Ambiguous<_>>::check;
    let _ = <b::Device as Ambiguous<_>>::check;
    let _ = <b::Queue as Ambiguous<_>>::check;
};
#[derive(Default)]
struct State {
    fail_pipeline: Cell<bool>,
    fail_texture: Cell<bool>,
    fail_draw: Cell<bool>,
    fail_complete: Cell<bool>,
    pipelines: Cell<i64>,
    reads: Cell<i64>,
    texture_reads: Cell<i64>,
    abandoned: Cell<i64>,
    no_render: Cell<bool>,
    order: RefCell<Vec<&'static str>>,
}
fn injected<T>() -> Result<T, error::Error> {
    Err(error::make("test", ErrorKind::InvalidState, "injected image sink failure"))
}
type E = error::Error;
macro_rules! forward {
    ($($name:ident($($arg:ident: $ty:ty),*) -> $ret:ty;)*) => {
        $(fn $name(&self, $($arg: $ty),*) -> $ret { self.inner.$name($($arg),*) })*
    };
}
// `texture` selects which read counter a wrapped resource bumps.
struct Wrapped<T: ?Sized> { inner: Box<T>, st: Rc<State>, texture: bool }
fn wrap<T: ?Sized>(inner: Box<T>, st: &Rc<State>) -> Box<Wrapped<T>> { Box::new(Wrapped { inner, st: st.clone(), texture: false }) }
impl b::DriverResource for Wrapped<dyn b::DriverResource> {
    forward! {
        token() -> b::Token;
        write(offset: i64, bytes: &[u8]) -> Result<(), E>;
        read_into(offset: i64, destination: &mut [u8], destination_offset: usize, length: usize) -> Result<(), E>;
        destroy() -> Result<(), E>;
    }
    fn read(&self, offset: i64, length: usize) -> Result<Vec<u8>, E> {
        if self.texture {
            self.st.texture_reads.set(self.st.texture_reads.get() + 1);
        } else {
            assert_eq!(length, 4);
            self.st.reads.set(self.st.reads.get() + 1);
        }
        self.inner.read(offset, length)
    }
}
impl b::DriverPipeline for Wrapped<dyn b::DriverPipeline> {
    forward! { pipeline_token() -> b::Token; max_threads() -> i64; }
    fn destroy_pipeline(&self) -> Result<(), E> {
        self.inner.destroy_pipeline()?;
        self.st.pipelines.set(self.st.pipelines.get() - 1);
        Ok(())
    }
}
impl b::DriverRenderEncoder for Wrapped<dyn b::DriverRenderEncoder> {
    forward! {
        render_set_pipeline(pipeline: b::Token) -> Result<(), E>;
        set_stage_buffer(stage: b::ShaderStage, index: i64, offset: i64, buffer: b::Token) -> Result<(), E>;
        set_stage_texture(stage: b::ShaderStage, index: i64, texture: b::Token) -> Result<(), E>;
        set_stage_sampler(stage: b::ShaderStage, index: i64, sampler: b::Token) -> Result<(), E>;
        set_viewport(rect: ogpu_core::render_pass::AdmittedViewport) -> Result<(), E>;
        set_scissor(rect: ogpu_core::render_pass::Rect) -> Result<(), E>;
        set_cull(cull: ogpu_core::render_pass::Cull) -> Result<(), E>;
        set_winding(winding: b::Winding) -> Result<(), E>;
        set_depth_state(state: Option<b::DepthState>) -> Result<(), E>;
        draw_indexed(primitive: ogpu_core::render_pass::Primitive, index_type: ogpu_core::render_pass::IndexType, buffer: b::Token, offset: i64, count: i64, instances: i64) -> Result<(), E>;
        draw_indexed_indirect(primitive: ogpu_core::render_pass::Primitive, index_type: ogpu_core::render_pass::IndexType, buffer: b::Token, offset: i64, arguments: b::Token, arguments_offset: i64) -> Result<(), E>;
        draw_batch(draws: &[b::DriverBatchDraw]) -> Result<(), E>;
        use_resources(resources: &[b::Token]) -> Result<(), E>;
        execute_icb(icb: b::Token, location: i64, length: i64) -> Result<(), E>;
    }
    fn draw(&self, primitive: ogpu_core::render_pass::Primitive, first: i64, count: i64, instances: i64) -> Result<(), E> {
        self.st.order.borrow_mut().push("draw");
        assert_eq!((first, count), (0, 3));
        if self.st.fail_draw.get() { injected() } else { self.inner.draw(primitive, first, count, instances) }
    }
    fn end_render(&self) -> Result<(), E> {
        self.st.order.borrow_mut().push("end_render");
        self.inner.end_render()
    }
}
impl b::DriverCommands for Wrapped<dyn b::DriverCommands> {
    forward! {
        compute_encoder() -> Result<Box<dyn b::DriverComputeEncoder>, E>;
        accel_encoder() -> Result<Box<dyn b::DriverAccelEncoder>, E>;
        blit_encoder() -> Result<Box<dyn b::DriverBlitEncoder>, E>;
        commit_present(source: b::Token, frame: b::DriverFrame) -> Result<b::Receipt, E>;
    }
    fn commit(&self) -> Result<b::Receipt, E> {
        self.st.order.borrow_mut().push("commit");
        self.inner.commit()
    }
    fn abandon(&self) -> Result<(), E> {
        self.st.abandoned.set(self.st.abandoned.get() + 1);
        self.inner.abandon()
    }
    fn render_encoder(&self, target: &b::DriverRenderTarget) -> Result<Box<dyn b::DriverRenderEncoder>, E> {
        self.st.order.borrow_mut().push("render");
        assert_eq!(target.target_colors.len(), 1);
        Ok(wrap(self.inner.render_encoder(target)?, &self.st))
    }
}
impl b::DriverQueue for Wrapped<dyn b::DriverQueue> {
    forward! {
        poll_through(epoch: i64) -> Result<bool, E>;
        completed_epoch() -> i64;
        gpu_duration(epoch: i64) -> Option<f64>;
        gpu_timing() -> b::GpuTiming;
        destroy_queue() -> Result<(), E>;
    }
    fn complete_through(&self, epoch: i64) -> Result<(), E> {
        self.st.order.borrow_mut().push("complete");
        // Retire actual native work before injecting an error: cleanup may destroy its target.
        self.inner.complete_through(epoch)?;
        if self.st.fail_complete.get() { injected() } else { Ok(()) }
    }
    fn begin_commands(&self) -> Result<Box<dyn b::DriverCommands>, E> {
        Ok(wrap(self.inner.begin_commands()?, &self.st))
    }
}
impl b::DriverDevice for Wrapped<dyn b::DriverDevice> {
    forward! {
        device_handle() -> ogpu_core::handle::Device;
        allocated_size() -> i64;
        recommended_working_set() -> i64;
        create_mapped_buffer(descriptor: &ogpu_core::validation::AdmittedBuffer) -> Result<b::DriverMapped, E>;
        create_library(shader: &ogpu_core::shader::Shader) -> Result<Box<dyn b::DriverLibrary>, E>;
        warm_library(shader: &ogpu_core::shader::Shader) -> Option<b::Warm>;
        create_accel(descriptor: &b::DriverAccelDescriptor) -> Result<Box<dyn b::DriverAccel>, E>;
        instance_layout(kind: b::InstanceKind) -> Vec<i64>;
        create_sampler(descriptor: &ogpu_core::validation::AdmittedSampler) -> Result<Box<dyn b::DriverSampler>, E>;
        create_icb(profile: &ogpu_core::validation::AdmittedIcb) -> Result<Box<dyn b::DriverIcb>, E>;
        create_argument(pipeline: b::Token, stage: b::ShaderStage, index: ogpu_core::validation::AdmittedSlot) -> Result<Box<dyn b::DriverArgument>, E>;
        create_surface(configuration: &ogpu_core::surface::AdmittedConfiguration) -> Result<Box<dyn b::DriverSurface>, E>;
        destroy_device() -> Result<(), E>;
    }
    fn capabilities(&self) -> ogpu_core::caps::Caps {
        let mut caps = self.inner.capabilities();
        if self.st.no_render.get() { caps.render_pipeline = false; }
        caps
    }
    fn create_buffer(&self, descriptor: &ogpu_core::validation::AdmittedBuffer) -> Result<Box<dyn b::DriverResource>, E> {
        Ok(wrap(self.inner.create_buffer(descriptor)?, &self.st))
    }
    fn create_render_pipeline(&self, options: b::RenderPipelineOptions, descriptor: &ogpu_core::pipeline::RenderDescriptor, admitted: &ogpu_core::pipeline::AdmittedRenderPipeline) -> Result<Box<dyn b::DriverPipeline>, E> {
        if self.st.fail_pipeline.get() { return injected(); }
        let pipeline = self.inner.create_render_pipeline(options, descriptor, admitted)?;
        self.st.pipelines.set(self.st.pipelines.get() + 1);
        Ok(wrap(pipeline, &self.st))
    }
    fn create_texture(&self, descriptor: &ogpu_core::validation::AdmittedTexture) -> Result<Box<dyn b::DriverResource>, E> {
        if self.st.fail_texture.get() { return injected(); }
        let inner = self.inner.create_texture(descriptor)?;
        Ok(Box::new(Wrapped { inner, st: self.st.clone(), texture: true }))
    }
    fn create_queue(&self) -> Result<Box<dyn b::DriverQueue>, E> {
        Ok(wrap(self.inner.create_queue()?, &self.st))
    }
}
struct Wrap { base: Box<dyn Driver>, st: Rc<State> }
impl Driver for Wrap {
    fn create_device(&self) -> Result<Box<dyn b::DriverDevice>, E> { Ok(wrap(self.base.create_device()?, &self.st)) }
}
fn driver(state: Rc<State>) -> Box<dyn Driver> {
    Box::new(Wrap { base: ogpu_core::impl_::create_driver().unwrap(), st: state })
}
fn image_sink_transactions() {
    if !common::gpu() { return; }
    let before = ogpu_core::impl_::live_handles().unwrap();
    let unsupported = driver(Rc::new(State { no_render: Cell::new(true), ..State::default() }));
    let device = b::create_device(unsupported).unwrap();
    let queue = b::create_queue(&device).unwrap();
    assert!(s::create_owned(&device, &queue).is_err());
    b::destroy_queue(&queue).unwrap();
    b::destroy_device(&device).unwrap();
    assert_eq!(ogpu_core::impl_::live_handles().unwrap(), before);

    let st = Rc::new(State::default());
    let device = b::create_device(driver(st.clone())).unwrap();
    let queue = b::create_queue(&device).unwrap();
    st.fail_pipeline.set(true);
    assert!(s::create_owned(&device, &queue).is_err());
    assert_eq!(st.pipelines.get(), 0);
    st.fail_pipeline.set(false);
    let cache = pipelines::create(&|| 0.0, &device);
    let packed = tp::vector_fixtures(None, 195).into_iter().find(|(n, _)| n == "vec4").unwrap().1;
    let runner = run::create_owned(None, &device, &queue, &cache, &tp::ok(emit::kernel(&emit::Options::default(), &packed)));
    let sink = tp::ok(s::create_owned(&device, &queue));
    let inputs = tp::ok(phost::prepare(&packed, &Default::default(), &input::at_time(0.5)));
    let input = tp::ok(run::dispatch(&runner, &Default::default(), None, None, &inputs));
    let convert = |w, h| s::convert(&sink, w, h, &input);
    st.order.borrow_mut().clear();
    let first = tp::ok(convert(65, 3));
    assert_eq!(&*st.order.borrow(), &["render", "draw", "end_render", "commit", "complete"]);
    let first_texture = s::texture(&first).unwrap();
    let d = b::texture_descriptor(&first_texture);
    assert_eq!((d.width, d.height, d.format), (65, 3, TextureFormat::Rgba8Unorm));
    assert_eq!(d.usage, vec![TextureUsage::TextureBinding, TextureUsage::RenderAttachment, TextureUsage::TextureCopySrc]);
    let creates = (s::buffer_creations(&sink), s::texture_creations(&sink));
    let next = tp::ok(convert(65, 3));
    assert!(s::texture(&first).is_none());
    assert_eq!(b::texture_id(&s::texture(&next).unwrap()), b::texture_id(&first_texture));
    assert_eq!(creates, (s::buffer_creations(&sink), s::texture_creations(&sink)));
    for (w, h) in [(0, 3), (65, 0), (64, 3), (i64::MAX, 2)] { assert!(convert(w, h).is_err()); }
    assert_eq!(creates, (s::buffer_creations(&sink), s::texture_creations(&sink)));
    assert!(s::texture(&next).is_some());
    st.fail_texture.set(true);
    assert!(convert(39, 5).is_err());
    st.fail_texture.set(false);
    assert!(s::texture(&next).is_none() && !b::texture_destroyed(&first_texture));
    st.fail_draw.set(true);
    assert!(convert(65, 3).is_err());
    assert!(convert(39, 5).is_err());
    st.fail_draw.set(false);
    st.fail_complete.set(true);
    assert!(convert(39, 5).is_err());
    st.fail_complete.set(false);
    let recovered = tp::ok(convert(65, 3));
    assert_eq!(b::texture_id(&s::texture(&recovered).unwrap()), b::texture_id(&first_texture));
    assert_eq!(st.abandoned.get(), 2);
    let resized = tp::ok(convert(39, 5));
    assert!(s::texture(&recovered).is_none() && b::texture_destroyed(&first_texture));
    assert!(s::texture(&resized).is_some());
    assert_eq!((st.reads.get(), st.texture_reads.get()), (1, 0));
    assert_eq!(run::private::status_reads(&runner), st.reads.get());
    assert_eq!(run::private::readback_bytes(&runner), 0);
    let scalar = tp::fixtures(None, 195).into_iter().find(|(n, _)| n == "arithmetic").unwrap().1;
    let scalar_run = run::create_owned(None, &device, &queue, &cache, &tp::ok(emit::kernel(&Default::default(), &scalar)));
    let values = tp::ok(phost::prepare(&scalar, &Default::default(), &input::at_time(0.5)));
    let scalar_output = tp::ok(run::dispatch(&scalar_run, &Default::default(), None, None, &values));
    assert!(s::convert(&sink, 65, 3, &scalar_output).is_err());
    run::close(&scalar_run, false);
    assert!(s::texture(&resized).is_some());
    tp::ok(run::dispatch(&runner, &Default::default(), None, None, &inputs));
    assert!(convert(39, 5).is_err());
    s::close(&sink);
    s::close(&sink);
    assert!(s::texture(&resized).is_none() && convert(39, 5).is_err());
    run::close(&runner, false);
    pipelines::close(&cache);
    b::destroy_queue(&queue).unwrap();
    b::destroy_device(&device).unwrap();
    assert_eq!(ogpu_core::impl_::live_handles().unwrap(), before);
    assert_eq!(st.pipelines.get(), 0);
}
pub const CASES: &[(&str, fn())] = &[("image_sink_transactions", image_sink_transactions)];
