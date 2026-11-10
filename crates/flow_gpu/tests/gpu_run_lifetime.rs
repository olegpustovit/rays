//! Run, Output and Pipelines are not Send; aliases retain native ownership across release and readback.
use crate::common;
use common::test_program as tp;
use flow_gpu::{pipelines, run};
use flow_ir::{flow_ir::gpu, packed::{self, host as phost}};
use flow_shader::emit;
use flow_syntax::value::Value;
use ogpu_core::{backend as b, error::{self, ErrorKind}};
use std::{cell::Cell, rc::Rc, sync::Arc};

#[derive(Default)]
struct Faults {
    completion: Cell<bool>, table: Cell<bool>, input: Cell<bool>, invalid: Cell<bool>,
    pipelines: Cell<i64>, submitted: Cell<i64>, tables: Cell<i64>, inputs: Cell<i64>,
    creates: Cell<i64>, working_set: Cell<Option<i64>>,
}
fn bump(c: &Cell<i64>) { c.set(c.get() + 1); }
fn injected(message: &str) -> error::Error { error::make("test", ErrorKind::InvalidState, message) }
type E = error::Error;
macro_rules! forward {
    ($($name:ident($($arg:ident: $ty:ty),*) -> $ret:ty;)*) => {
        $(fn $name(&self, $($arg: $ty),*) -> $ret { self.inner.$name($($arg),*) })*
    };
}
struct Faulty<T: ?Sized> { inner: Box<T>, f: Rc<Faults>, size: i64 }
fn wrap<T: ?Sized>(inner: Box<T>, f: &Rc<Faults>, size: i64) -> Faulty<T> { Faulty { inner, f: f.clone(), size } }
impl b::DriverResource for Faulty<dyn b::DriverResource> {
    forward! { token() -> b::Token; as_any() -> &dyn std::any::Any; destroy() -> Result<(), E>; }
    fn write(&self, offset: i64, bytes: &[u8]) -> Result<(), E> {
        let (size, w) = (self.size, &self.f);
        if size == 2048 {
            bump(&w.tables);
            if w.table.get() { return Err(injected("injected table upload")); }
        } else if size >= 4096 {
            bump(&w.inputs);
            let result = self.inner.write(offset, bytes);
            return if w.input.get() { Err(injected("injected partial input upload")) } else { result };
        }
        self.inner.write(offset, bytes)
    }
    fn read(&self, offset: i64, length: usize) -> Result<Vec<u8>, E> {
        if self.f.invalid.get() && length == 4 { Ok(1u32.to_le_bytes().to_vec()) } else { self.inner.read(offset, length) }
    }
    fn read_into(&self, offset: i64, bytes: &mut [u8], start: usize, length: usize) -> Result<(), E> {
        if self.f.invalid.get() && length == 4 {
            bytes[start..start + 4].copy_from_slice(&1u32.to_le_bytes()); Ok(())
        } else { self.inner.read_into(offset, bytes, start, length) }
    }
}
impl b::DriverMapping for Faulty<dyn b::DriverMapping> {
    forward! { readback() -> b::OwnedReadback; }
}
impl b::DriverPipeline for Faulty<dyn b::DriverPipeline> {
    forward! { as_any() -> &dyn std::any::Any; max_threads() -> i64; }
    fn destroy_pipeline(&self) -> Result<(), E> {
        let result = self.inner.destroy_pipeline();
        if result.is_ok() { self.f.pipelines.set(self.f.pipelines.get() - 1); }
        result
    }
}
impl b::DriverLibrary for Faulty<dyn b::DriverLibrary> {
    forward! { destroy_library() -> Result<(), E>; }
    fn create_compute_pipeline_in(&self, function: &ogpu_core::shader::AdmittedFunction, interface: &[ogpu_core::shader::Binding]) -> Result<Box<dyn b::DriverPipeline>, E> {
        let pipeline = self.inner.create_compute_pipeline_in(function, interface)?;
        bump(&self.f.pipelines);
        Ok(Box::new(wrap(pipeline, &self.f, 0)))
    }
}
impl b::DriverCommands for Faulty<dyn b::DriverCommands> {
    forward! {
        compute_encoder() -> Result<Box<dyn b::DriverComputeEncoder>, E>;
        accel_encoder() -> Result<Box<dyn b::DriverAccelEncoder>, E>;
        blit_encoder() -> Result<Box<dyn b::DriverBlitEncoder>, E>;
        render_encoder(target: &b::DriverRenderTarget) -> Result<Box<dyn b::DriverRenderEncoder>, E>;
        commit_present(source: &dyn b::DriverResource, frame: b::DriverFrame) -> Result<b::Receipt, E>;
        abandon() -> Result<(), E>;
    }
    // The native encoder and real pipeline limits are deliberately untouched.
    fn commit(&self) -> Result<b::Receipt, E> { bump(&self.f.submitted); self.inner.commit() }
}
impl b::DriverQueue for Faulty<dyn b::DriverQueue> {
    forward! {
        poll_through(epoch: i64) -> Result<bool, E>;
        gpu_duration(epoch: i64) -> Option<f64>;
        destroy_queue() -> Result<(), E>;
    }
    fn complete_through(&self, epoch: i64) -> Result<(), E> {
        if self.f.completion.get() { Err(injected("injected completion")) } else { self.inner.complete_through(epoch) }
    }
    fn begin_commands(&self) -> Result<Box<dyn b::DriverCommands>, E> {
        Ok(Box::new(wrap(self.inner.begin_commands()?, &self.f, 0)))
    }
}
impl b::DriverDevice for Faulty<dyn b::DriverDevice> {
    forward! {
        device_handle() -> ogpu_core::handle::Device;
        capabilities() -> ogpu_core::caps::Caps;
        allocated_size() -> i64;
        create_texture(descriptor: &ogpu_core::validation::AdmittedTexture) -> Result<Box<dyn b::DriverResource>, E>;
        warm_library(shader: &ogpu_core::shader::Shader) -> Option<b::Warm>;
        create_accel(descriptor: &b::DriverAccelDescriptor) -> Result<Box<dyn b::DriverAccel>, E>;
        instance_layout(kind: b::InstanceKind) -> Vec<i64>;
        create_sampler(descriptor: &ogpu_core::validation::AdmittedSampler) -> Result<Box<dyn b::DriverSampler>, E>;
        create_render_pipeline(options: b::RenderPipelineOptions, descriptor: &ogpu_core::pipeline::RenderDescriptor, admitted: &ogpu_core::pipeline::AdmittedRenderPipeline) -> Result<Box<dyn b::DriverPipeline>, E>;
        create_icb(profile: &ogpu_core::validation::AdmittedIcb) -> Result<Box<dyn b::DriverIcb>, E>;
        create_argument(pipeline: &dyn b::DriverPipeline, stage: b::ShaderStage, index: ogpu_core::validation::AdmittedSlot) -> Result<Box<dyn b::DriverArgument>, E>;
        create_surface(configuration: &ogpu_core::surface::AdmittedConfiguration) -> Result<Box<dyn b::DriverSurface>, E>;
        destroy_device() -> Result<(), E>;
    }
    fn recommended_working_set(&self) -> i64 { self.f.working_set.get().unwrap_or_else(|| self.inner.recommended_working_set()) }
    fn create_buffer(&self, descriptor: &ogpu_core::validation::AdmittedBuffer) -> Result<Box<dyn b::DriverResource>, E> {
        bump(&self.f.creates);
        let r = self.inner.create_buffer(descriptor)?;
        Ok(Box::new(wrap(r, &self.f, descriptor.size)))
    }
    fn create_mapped_buffer(&self, descriptor: &ogpu_core::validation::AdmittedBuffer) -> Result<b::DriverMapped, E> {
        let mapped = self.inner.create_mapped_buffer(descriptor)?;
        Ok(b::DriverMapped {
            mapped_resource: Box::new(wrap(mapped.mapped_resource, &self.f, descriptor.size)),
            mapping: Box::new(wrap(mapped.mapping, &self.f, descriptor.size)),
        })
    }
    fn create_library(&self, shader: &ogpu_core::shader::Shader) -> Result<Box<dyn b::DriverLibrary>, E> {
        Ok(Box::new(wrap(self.inner.create_library(shader)?, &self.f, 0)))
    }
    fn create_queue(&self) -> Result<Box<dyn b::DriverQueue>, E> {
        Ok(Box::new(wrap(self.inner.create_queue()?, &self.f, 0)))
    }
}
struct FaultyDriver { base: Box<dyn b::Driver>, f: Rc<Faults> }
impl b::Driver for FaultyDriver {
    fn create_device(&self) -> Result<Box<dyn b::DriverDevice>, E> {
        Ok(Box::new(wrap(self.base.create_device()?, &self.f, 0)))
    }
}
fn driver(f: Rc<Faults>) -> Box<dyn b::Driver> {
    Box::new(FaultyDriver { base: ogpu_core::impl_::create_driver().expect("native driver"), f })
}
fn fixture(name: &str, count: i64) -> packed::T {
    let body = match name {
        "arithmetic" => format!("(map (fn [x] (+ (* x 2) t)) (array/range {count}))"),
        "noise" => format!("(map (fn [x] (noise3 [(* x 0.02) t 0.25] :seed 31 :octaves 3)) (array/range {count}))"),
        _ => unreachable!("unknown lifetime fixture"),
    };
    tp::compile(&body)
}
fn prepare(p: &packed::T) -> phost::Inputs { tp::ok(phost::prepare(p, &phost::PrepareOptions::default(), &input::at_time(1.0))) }
fn dispatch(r: &run::T, v: &phost::Inputs) -> Result<run::Output, rays_diagnostic::Diagnostic> { run::dispatch(r, &run::Options::default(), None, None, v) }
fn kept(r: &run::T, v: &phost::Inputs) -> gpu::Planes { tp::ok(run::keep(&tp::ok(dispatch(r, v)), None)) }
fn settled(r: &run::T, v: &phost::Inputs) -> gpu::Planes { let p = kept(r, v); tp::ok(p.settle()); p }
fn len(p: Vec<Vec<f64>>) -> i64 { p.iter().map(|a| a.len() as i64).sum() }
fn pinned(r: &run::T, v: &phost::Inputs) -> (Arc<Vec<f64>>, run::Pin) {
    let p = settled(r, v);
    let stand_in = match p.resident.clone().unwrap() { Value::Floats(1..=4, a) => a, _ => panic!("no stand-in") };
    let pin = run::pin(&stand_in).expect("pin");
    p.release(true);
    (stand_in, pin)
}
struct Fixture {
    f: Rc<Faults>, device: b::Device, queue: b::Queue, cache: pipelines::T,
    kernel: emit::Kernel, r: run::T, values: phost::Inputs,
}
fn with_fixture(test: impl FnOnce(&Fixture)) {
    if !common::gpu() { return; }
    common::release_keepalive();
    let baseline = ogpu_core::impl_::live_handles().expect("native handle count");
    let f = Rc::new(Faults::default());
    let device = b::create_device(driver(f.clone())).expect("device");
    let queue = b::create_queue(&device).expect("queue");
    let cache = pipelines::create(&|| 0.0, &device);
    let packed = fixture("arithmetic", 1024);
    let kernel = tp::ok(emit::kernel(&emit::Options::default(), &packed));
    let r = run::create_owned(None, &device, &queue, &cache, &kernel);
    let values = prepare(&packed);
    let fixture = Fixture { f, device, queue, cache, kernel, r, values };
    test(&fixture);
    run::close(&fixture.r, true);
    pipelines::close(&fixture.cache);
    run::clear_thread_state();
    b::destroy_queue(&fixture.queue).expect("destroy queue");
    b::destroy_device(&fixture.device).expect("destroy device");
    assert_eq!(ogpu_core::impl_::live_handles().expect("native handle count"), baseline);
    assert_eq!(fixture.f.pipelines.get(), 0);
}
fn failed_commands_and_uploads() {
    with_fixture(|Fixture { f, r, values, .. }| {
        let first = tp::ok(dispatch(r, values));
        assert!(run::buffer(&first, None).is_some());
        tp::ok(dispatch(r, values));
        assert_eq!((f.inputs.get(), run::private::input_uploads(r)), (1, 1));
        let mut replacement = values.clone();
        replacement.arrays = values.arrays.iter().map(|a| Arc::new((**a).clone())).collect();
        Arc::make_mut(&mut replacement.arrays[0])[0] = 17.0;
        f.input.set(true);
        assert!(dispatch(r, &replacement).is_err());
        assert_eq!((f.inputs.get(), run::private::input_uploads(r)), (2, 1));
        f.input.set(false);
        tp::ok(dispatch(r, values)); assert_eq!(f.inputs.get(), 3);
        tp::ok(dispatch(r, &replacement)); tp::ok(dispatch(r, &replacement)); assert_eq!(f.inputs.get(), 4);
        let mut half = replacement.clone(); half.count = 512;
        tp::ok(dispatch(r, &half)); tp::ok(dispatch(r, &replacement));
        assert_eq!((f.inputs.get(), run::private::input_uploads(r)), (6, 5));
        assert_eq!(run::private::input_uploaded_bytes(r), 4 * 4096 + 2048);
        f.completion.set(true);
        let large = prepare(&fixture("arithmetic", 65536));
        assert!(dispatch(r, &large).is_err());
        assert!(run::buffer(&first, None).is_none() && run::readback(&first, None).is_err());
        assert_eq!((run::private::buffer_creations(r), f.submitted.get(), f.inputs.get()), (5, 8, 7));
        f.completion.set(false);
        let fresh = tp::ok(dispatch(r, &large)); assert!(run::buffer(&fresh, None).is_some());
        f.invalid.set(true);
        assert!(matches!(dispatch(r, &large), Err(d) if d.code == "E_KERNEL"));
        assert!(run::buffer(&fresh, None).is_none());
        f.invalid.set(false);
        let mut invalid = large.clone();
        invalid.arrays = large.arrays.iter().map(|a| Arc::new((**a).clone())).collect();
        Arc::make_mut(&mut invalid.arrays[0])[0] = 3.5e38;
        assert!(dispatch(r, &invalid).is_err()); assert!(run::buffer(&fresh, None).is_none());
        run::close(r, true); run::close(r, false);
        assert!(run::buffer(&fresh, None).is_none());
    });
}
fn retired_holders_and_discard() {
    with_fixture(|Fixture { f, r, values, .. }| {
        let output = tp::ok(dispatch(r, values));
        let buffer = run::buffer(&output, None).expect("dispatched buffer");
        let initial = tp::ok(run::keep(&output, None));
        tp::ok(initial.settle());
        let host_view = Rc::downgrade(&b::mapped_view(&buffer).expect("central host view"));
        initial.release(true); drop(initial);
        let creations = run::private::buffer_creations(r);
        let thaws = run::private::materialisations(r);
        for _ in 0..24 {
            let p = settled(r, values);
            p.release(true); drop(p);
        }
        assert_eq!(run::private::materialisations(r), thaws);
        assert!(run::private::held_buffers(r) <= 2);
        assert!(run::private::buffer_creations(r) - creations <= 4);
        assert!(host_view.upgrade().is_none(), "recycling must release the central host view");
        f.working_set.set(Some(4));
        let oldest = settled(r, values); let expected = oldest.count * oldest.width;
        oldest.release(true);
        let before = run::private::materialisations(r);
        let later: Vec<_> = (0..8).map(|_| settled(r, values)).collect();
        assert!(run::private::materialisations(r) > before);
        assert_eq!(len(oldest.materialise().unwrap()), expected);
        for p in later { p.release(false); }
        f.working_set.set(None);
        let thaws = run::private::materialisations(r);
        let live = settled(r, values); run::close(r, true);
        assert_eq!(run::private::materialisations(r), thaws);
        assert!(live.materialise().is_err());
        run::close(r, false);
    });
}
fn clobber_and_idle_ageing() {
    with_fixture(|Fixture { device, queue, cache, kernel, values, .. }| {
        mem::configure(&|key| (key == "RAYS_MEM_CLOBBER").then(|| "1".into())).unwrap();
        let free_owner = mem::cache_in(mem::Pool::Device, "flow_gpu.free");
        let free_before = mem::bytes(&free_owner);
        let pool = run::pool();
        let pooled = run::create_owned(Some(&pool), device, queue, cache, kernel);
        let dead = settled(&pooled, values); dead.release(false); drop(dead);
        run::private::pool_drain(&pool);
        let floats = run::private::pool_free_floats(&pool); assert!(!floats.is_empty());
        for a in floats { assert!(a.borrow().iter().all(|x| x.is_nan())); }
        assert!(mem::bytes(&free_owner) > free_before);
        for _ in 0..1100 { run::private::pool_drain(&pool); }
        assert_eq!(run::private::pool_free_buffers(&pool), 0);
        assert_eq!(mem::bytes(&free_owner), free_before);
        mem::configure(&|_| None).unwrap();
        run::close(&pooled, true); run::close_pool(&pool);
    });
}
fn native_free_budget_size_classes() {
    with_fixture(|Fixture { f, device, queue, cache, kernel, values, .. }| {
        f.working_set.set(Some(2048 << 20));
        let pool = run::pool();
        let pooled = run::create_owned(Some(&pool), device, queue, cache, kernel);
        for _ in 0..5 { run::private::pool_give(&pool, device, 64 << 20); }
        let small = settled(&pooled, values); small.release(false); drop(small);
        run::private::pool_drain(&pool); assert_eq!(run::private::pool_free_buffers(&pool), 6);
        for _ in 0..2 { run::private::pool_give(&pool, device, 128 << 20); }
        let over = settled(&pooled, values); over.release(false); drop(over);
        run::private::pool_drain(&pool); assert_eq!(run::private::pool_free_buffers(&pool), 7);
        f.working_set.set(None); run::close(&pooled, true); run::close_pool(&pool);
    });
}
fn evicted_and_pinned_holders() {
    with_fixture(|Fixture { device, queue, cache, kernel, values, .. }| {
        let pool = run::pool();
        let evicted = run::create_owned(Some(&pool), device, queue, cache, kernel);
        let retained = kept(&evicted, values); let unread = kept(&evicted, values);
        retained.release(true); run::close(&evicted, false);
        assert_eq!(run::private::materialisations(&evicted), 0);
        assert_eq!(len(retained.materialise().unwrap()), retained.count * retained.width);
        run::close_pool(&pool);
        assert!(unread.materialise().is_err());
        let pool = run::pool();
        let pooled = run::create_owned(Some(&pool), device, queue, cache, kernel);
        let (stand_in, (_, _, valid, unpin)) = pinned(&pooled, values);
        assert!(run::pin(&stand_in).is_none());
        assert!(valid() && run::private::pool_held(&pool) == 1);
        unpin(); assert!(!valid() && run::private::pool_held(&pool) == 0);
        let (_, (_, _, valid, _)) = pinned(&pooled, values);
        assert!(valid()); run::close(&pooled, true); run::close_pool(&pool); assert!(!valid());
    });
}
fn slot_shrink_hysteresis() {
    with_fixture(|Fixture { device, queue, cache, kernel, values, .. }| {
        let big = prepare(&fixture("arithmetic", 400_000));
        let shrinking = run::create_owned(None, device, queue, cache, kernel);
        tp::ok(dispatch(&shrinking, &big)); let grown = run::private::buffer_creations(&shrinking);
        for _ in 0..run::private::SLOT_SHRINK_AFTER { tp::ok(dispatch(&shrinking, values)); }
        assert!(run::private::buffer_creations(&shrinking) > grown);
        let stable = run::private::buffer_creations(&shrinking);
        for _ in 0..16 { tp::ok(dispatch(&shrinking, values)); }
        assert_eq!(run::private::buffer_creations(&shrinking), stable); run::close(&shrinking, true);
    });
}
fn failed_table_upload_retries() {
    with_fixture(|Fixture { f, device, queue, cache, .. }| {
        let packed = fixture("noise", 1024);
        let noise = run::create_owned(None, device, queue, cache, &tp::ok(emit::kernel(&emit::Options::default(), &packed)));
        let values = prepare(&packed);
        f.table.set(true); assert!(dispatch(&noise, &values).is_err()); assert_eq!(f.tables.get(), 1);
        f.table.set(false); tp::ok(dispatch(&noise, &values)); assert_eq!(f.tables.get(), 2);
        tp::ok(dispatch(&noise, &values)); assert_eq!(f.tables.get(), 2); run::close(&noise, false);
    });
}
fn bounds_and_foreign_owned_readback() {
    with_fixture(|Fixture { f, device, queue, cache, .. }| {
        let packed = tp::compile("(map (fn [x] [x t 0.0]) (array/range 1024))");
        let sea = run::create_owned(None, device, queue, cache, &tp::ok(emit::kernel(&emit::Options::default(), &packed)));
        let values = prepare(&packed);
        let first = settled(&sea, &values); let c0 = f.creates.get();
        assert_eq!(first.bounds(), Some(vec![0., 1., 0., 1023., 1., 0.]));
        let read = run::private::readback_bytes(&sea);
        assert_eq!((f.creates.get() - c0, read), (2, 24));
        first.bounds(); assert_eq!(run::private::readback_bytes(&sea), read);
        let second = settled(&sea, &values); let c1 = f.creates.get();
        second.bounds(); assert_eq!(f.creates.get(), c1);
        assert_eq!(run::private::readback_bytes(&sea), 2 * read);
        assert_eq!(run::private::bounds_readback_bytes(&sea), 2 * read);
        // The owner blocks in join: no Run::service/pumping may be necessary.
        let first = std::thread::spawn(move || { let data = first.materialise().unwrap(); assert_eq!(len(data), 1024 * 3); first }).join().unwrap();
        assert_eq!(run::private::readback_bytes(&sea), 2 * read + 1024 * 3 * 4);
        first.materialise().unwrap(); assert_eq!(run::private::readback_bytes(&sea), 2 * read + 1024 * 3 * 4);
        assert_eq!(run::private::bounds_readback_bytes(&sea), 2 * read);
        first.release(false); second.release(false); run::close(&sea, false);
    });
}

fn reduced_bounds_cover_every_point() {
    with_fixture(|Fixture { device, queue, cache, .. }| {
        for count in [1, 1023, 1024, 1025, 16383, 16384, 16385, 1_000_003] {
            let packed = tp::compile(&format!("(map (fn [x] [(- 17.0 x) x (- t x)]) (array/range {count}))"));
            let sea = run::create_owned(None, device, queue, cache, &tp::ok(emit::kernel(&emit::Options::default(), &packed)));
            let output = settled(&sea, &prepare(&packed));
            let last = (count - 1) as f64;
            assert_eq!(output.bounds(), Some(vec![17. - last, 0., 1. - last, 17., last, 1.]), "count {count}");
            assert_eq!(run::private::bounds_readback_bytes(&sea), 24);
            output.release(false);
            run::close(&sea, false);
        }
    });
}
pub const CASES: &[(&str, fn())] = &[
        ("failed_commands_and_uploads", failed_commands_and_uploads),
        ("retired_holders_and_discard", retired_holders_and_discard),
        ("clobber_and_idle_ageing", clobber_and_idle_ageing),
        ("native_free_budget_size_classes", native_free_budget_size_classes),
        ("evicted_and_pinned_holders", evicted_and_pinned_holders),
        ("slot_shrink_hysteresis", slot_shrink_hysteresis),
        ("failed_table_upload_retries", failed_table_upload_retries),
        ("bounds_and_foreign_owned_readback", bounds_and_foreign_owned_readback),
        ("reduced_bounds_cover_every_point", reduced_bounds_cover_every_point),
    ];
