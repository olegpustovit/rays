//! Port of lib/flow_gpu/test_pipelines.ml: shared kernel sources compile once, the cache holds 64
//! pipelines and releases the evicted, `close` is idempotent and a closed cache refuses. The
//! portable mock cannot execute compute: this driver adds owned pipeline handles only.
mod common;
use common::test_program as tp;
use flow_gpu::pipelines;
use flow_shader::emit;
use ogpu::backend::{self as b, Driver, DriverDevice, DriverLibrary, DriverPipeline};
use ogpu::error::Error;
use std::cell::Cell;
use std::rc::Rc;

struct Counts { live: Rc<Cell<i64>>, next: Rc<Cell<i64>> }
struct Pipeline { token: b::Token, live: Rc<Cell<i64>> }
impl DriverPipeline for Pipeline {
    fn pipeline_token(&self) -> b::Token { self.token }
    fn max_threads(&self) -> i64 { 1024 }
    fn destroy_pipeline(&self) -> Result<(), Error> { self.live.set(self.live.get() - 1); Ok(()) }
}
struct Library { inner: Box<dyn DriverLibrary>, counts: Rc<Counts> }
impl DriverLibrary for Library {
    fn create_compute_pipeline_in(&self, _entry: &str, _constants: &[(String, ogpu::shader::ConstantValue)], _interface: &[ogpu::shader::Binding]) -> Result<Box<dyn DriverPipeline>, Error> {
        let Counts { live, next } = &*self.counts;
        live.set(live.get() + 1);
        next.set(next.get() + 1);
        Ok(Box::new(Pipeline { token: next.get(), live: live.clone() }))
    }
    fn destroy_library(&self) -> Result<(), Error> { self.inner.destroy_library() }
}
struct Dev { inner: Box<dyn DriverDevice>, counts: Rc<Counts> }
macro_rules! forward {
    ($($name:ident($($arg:ident: $ty:ty),*) -> $ret:ty;)*) => {
        $(fn $name(&self, $($arg: $ty),*) -> $ret { self.inner.$name($($arg),*) })*
    };
}
impl DriverDevice for Dev {
    forward! {
        device_handle() -> ogpu::handle::Device;
        allocated_size() -> i64;
        recommended_working_set() -> i64;
        create_buffer(memory: ogpu::types::Memory, descriptor: &ogpu::types::BufferDescriptor) -> Result<Box<dyn b::DriverResource>, Error>;
        create_mapped_buffer(descriptor: &ogpu::types::BufferDescriptor) -> Result<b::DriverMapped, Error>;
        create_texture(descriptor: &ogpu::types::TextureDescriptor) -> Result<Box<dyn b::DriverResource>, Error>;
        create_depth_texture(descriptor: &ogpu::types::TextureDescriptor) -> Result<Box<dyn b::DriverResource>, Error>;
        create_memoryless_texture(depth: bool, descriptor: &ogpu::types::TextureDescriptor) -> Result<Box<dyn b::DriverResource>, Error>;
        warm_library(shader: &ogpu::shader::Shader) -> Option<b::Warm>;
        create_accel(descriptor: &b::DriverAccelDescriptor) -> Result<Box<dyn b::DriverAccel>, Error>;
        instance_layout(kind: b::InstanceKind) -> Vec<i64>;
        create_sampler(descriptor: &ogpu::types::SamplerDescriptor) -> Result<Box<dyn b::DriverSampler>, Error>;
        create_render_pipeline(options: b::RenderPipelineOptions, descriptor: &ogpu::pipeline::RenderDescriptor) -> Result<Box<dyn DriverPipeline>, Error>;
        create_icb(max_commands: i64) -> Result<Box<dyn b::DriverIcb>, Error>;
        create_argument(pipeline: b::Token, stage: b::ShaderStage, index: i64) -> Result<Box<dyn b::DriverArgument>, Error>;
        create_queue() -> Result<Box<dyn b::DriverQueue>, Error>;
        create_surface(configuration: &ogpu::surface::Configuration) -> Result<Box<dyn b::DriverSurface>, Error>;
        destroy_device() -> Result<(), Error>;
    }
    fn capabilities(&self) -> ogpu::caps::Caps {
        let mut caps = self.inner.capabilities();
        caps.compute_pipeline = true;
        caps
    }
    fn create_library(&self, shader: &ogpu::shader::Shader) -> Result<Box<dyn DriverLibrary>, Error> {
        Ok(Box::new(Library { inner: self.inner.create_library(shader)?, counts: self.counts.clone() }))
    }
}
struct Wrap { base: Box<dyn Driver>, counts: Rc<Counts> }
impl Driver for Wrap {
    fn create_device(&self) -> Result<Box<dyn DriverDevice>, Error> {
        Ok(Box::new(Dev { inner: self.base.create_device()?, counts: self.counts.clone() }))
    }
}

#[test]
fn pipeline_ownership_on_mock() {
    ogpu::impl_::select(ogpu_mock::impl_::implementation());
    let (base, _control) = ogpu_mock::backend_mock::create();
    let live = Rc::new(Cell::new(0i64));
    let next = Rc::new(Cell::new(0i64));
    let driver: Box<dyn Driver> = Box::new(Wrap { base, counts: Rc::new(Counts { live: live.clone(), next }) });
    let device = b::create_device(driver).expect("device");
    let cache = pipelines::create(&|| 0.0, &device);
    let first = tp::fixtures(None, 1024).into_iter().find(|(n, _)| n == "arithmetic").unwrap().1;
    let same = tp::fixtures(None, 65536).into_iter().find(|(n, _)| n == "arithmetic").unwrap().1;
    let first_msl = tp::ok(emit::kernel(&emit::Options::default(), &first));
    tp::ok(pipelines::get(&cache, &first_msl));
    tp::ok(pipelines::get(&cache, &tp::ok(emit::kernel(&emit::Options::default(), &same))));
    assert_eq!(pipelines::Private::compilations(&cache), 1);
    for number in 1..=65 {
        let program = tp::compile(&format!("(map (fn [x] (+ x (+ t {number}))) (array/range 1024))"));
        tp::ok(pipelines::get(&cache, &tp::ok(emit::kernel(&emit::Options::default(), &program))));
    }
    assert_eq!(pipelines::Private::count(&cache), 64);
    assert_eq!(pipelines::Private::releases(&cache), 2);
    pipelines::close(&cache);
    assert_eq!(pipelines::Private::releases(&cache), 66);
    pipelines::close(&cache);
    assert_eq!(pipelines::Private::releases(&cache), 66);
    assert!(pipelines::get(&cache, &first_msl).is_err());
    b::destroy_device(&device).expect("destroy");
    assert_eq!(ogpu::impl_::live_handles().unwrap_or(0), 0);
    assert_eq!(live.get(), 0);
}
