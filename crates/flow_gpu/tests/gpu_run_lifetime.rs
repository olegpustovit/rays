//! Native port of lib/flow_gpu/test_run_lifetime.ml.
//! Run, Output and Pipelines are !Send: Rust excludes the OCaml wrong-domain owner moves.
mod common;
use common::test_program as tp;
use flow_gpu::{pipelines, run};
use flow_ir::{flow_ir::Gpu, packed::{self, host as phost}};
use flow_shader::emit;
use flow_syntax::value::Value;
use ogpu::{backend as b, error::{self, ErrorKind}};
use std::{cell::Cell, rc::Rc, sync::Arc};

#[derive(Default)]
struct Faults {
    completion: Cell<bool>, table: Cell<bool>, input: Cell<bool>, invalid: Cell<bool>,
    pipelines: Cell<i64>, submitted: Cell<i64>, tables: Cell<i64>, inputs: Cell<i64>,
    creates: Cell<i64>, views: Cell<i64>, releases: Cell<i64>, working_set: Cell<Option<i64>>,
}
fn bump(c: &Cell<i64>) { c.set(c.get() + 1); }
fn injected(message: &str) -> error::Error { error::make("test", ErrorKind::InvalidState, message) }
fn resource(mut r: b::DriverResource, size: i64, f: Rc<Faults>) -> b::DriverResource {
    let write = r.write;
    let w = f.clone();
    r.write = Box::new(move |offset, bytes| {
        if size == 2048 {
            bump(&w.tables);
            if w.table.get() { return Err(injected("injected table upload")); }
        } else if size >= 4096 {
            bump(&w.inputs);
            let result = write(offset, bytes);
            return if w.input.get() { Err(injected("injected partial input upload")) } else { result };
        }
        write(offset, bytes)
    });
    let read = r.read;
    let flags = f.clone();
    r.read = Box::new(move |offset, length| {
        if flags.invalid.get() && length == 4 { Ok(1u32.to_le_bytes().to_vec()) } else { read(offset, length) }
    });
    let read_into = r.read_into;
    r.read_into = Box::new(move |offset, bytes, start, length| {
        if f.invalid.get() && length == 4 {
            bytes[start..start + 4].copy_from_slice(&1u32.to_le_bytes()); Ok(())
        } else { read_into(offset, bytes, start, length) }
    });
    r
}
fn driver(f: Rc<Faults>) -> b::Driver {
    let base = ogpu::impl_::create_driver().expect("native driver");
    b::Driver { create_device: Box::new(move || {
        let mut raw = (base.create_device)()?;
        let working = raw.recommended_working_set;
        let w = f.clone();
        raw.recommended_working_set = Box::new(move || w.working_set.get().unwrap_or_else(&working));
        let create = raw.create_buffer;
        let w = f.clone();
        raw.create_buffer = Box::new(move |memory, descriptor| {
            bump(&w.creates);
            create(memory, descriptor).map(|r| resource(r, descriptor.size, w.clone()))
        });
        let create = raw.create_mapped_buffer;
        let w = f.clone();
        raw.create_mapped_buffer = Box::new(move |descriptor| {
            let mut mapped = create(descriptor)?;
            mapped.mapped_resource = resource(mapped.mapped_resource, descriptor.size, w.clone());
            let view = mapped.view;
            let v = w.clone();
            mapped.view = Box::new(move || { bump(&v.views); view() });
            let release = mapped.release_view;
            let v = w.clone();
            mapped.release_view = Box::new(move || { bump(&v.releases); release() });
            let flush = mapped.flush;
            let size = descriptor.size;
            let v = w.clone();
            mapped.flush = Box::new(move |length| {
                if size >= 4096 {
                    bump(&v.inputs);
                    let result = flush(length);
                    if v.input.get() { Err(injected("injected partial input upload")) } else { result }
                } else { flush(length) }
            });
            Ok(mapped)
        });
        let create = raw.create_library;
        let w = f.clone();
        raw.create_library = Box::new(move |shader| {
            let mut library = create(shader)?;
            let create = library.create_compute_pipeline_in;
            let w = w.clone();
            library.create_compute_pipeline_in = Box::new(move |entry, constants, interface| {
                let mut pipeline = create(entry, constants, interface)?;
                bump(&w.pipelines);
                let destroy = pipeline.destroy_pipeline;
                let w = w.clone();
                pipeline.destroy_pipeline = Box::new(move || { let result = destroy(); if result.is_ok() { w.pipelines.set(w.pipelines.get() - 1); } result });
                Ok(pipeline)
            });
            Ok(library)
        });
        let create = raw.create_queue;
        let w = f.clone();
        raw.create_queue = Box::new(move || {
            let mut queue = create()?;
            let complete = queue.complete_through;
            let v = w.clone();
            queue.complete_through = Box::new(move |epoch| if v.completion.get() { Err(injected("injected completion")) } else { complete(epoch) });
            let begin = queue.begin_commands;
            let v = w.clone();
            queue.begin_commands = Box::new(move || {
                let mut commands = begin()?;
                let commit = commands.commit;
                let v = v.clone();
                commands.commit = Box::new(move || { bump(&v.submitted); commit() });
                // The native encoder and real pipeline limits are deliberately untouched.
                Ok(commands)
            });
            Ok(queue)
        });
        Ok(raw)
    }) }
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
fn kept(r: &run::T, v: &phost::Inputs) -> Gpu::Planes { tp::ok(run::keep(&tp::ok(dispatch(r, v)), None)) }
fn settled(r: &run::T, v: &phost::Inputs) -> Gpu::Planes { let p = kept(r, v); tp::ok((p.settle)()); p }
fn len(p: Vec<Vec<f64>>) -> i64 { p.iter().map(|a| a.len() as i64).sum() }
fn pinned(r: &run::T, v: &phost::Inputs) -> (Arc<Vec<f64>>, run::Pin, Box<dyn Fn(bool) + Send + Sync>) {
    let p = settled(r, v);
    let stand_in = match p.resident.unwrap() { Value::Float_array(a) | Value::Vec2_array(a) | Value::Vec3_array(a) | Value::Vec4_array(a) => a, _ => panic!("no stand-in") };
    let pin = run::pin(&stand_in).expect("pin");
    (p.release)(true);
    (stand_in, pin, p.release)
}
struct Fixture {
    f: Rc<Faults>, device: b::Device, queue: b::Queue, cache: pipelines::T,
    kernel: emit::Kernel, r: run::T, values: phost::Inputs,
}
fn with_fixture(test: impl FnOnce(&Fixture)) {
    if !common::gpu() { return; }
    common::release_keepalive();
    let baseline = ogpu::impl_::live_handles().expect("native handle count");
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
    assert_eq!(ogpu::impl_::live_handles().expect("native handle count"), baseline);
    assert_eq!(fixture.f.pipelines.get(), 0);
}
fn failed_commands_and_uploads() {
    with_fixture(|Fixture { f, r, values, .. }| {
        let first = tp::ok(dispatch(r, values));
        assert!(run::buffer(&first, None).is_some());
        tp::ok(dispatch(r, values));
        assert_eq!((f.inputs.get(), run::Private::input_uploads(r)), (1, 1));
        let mut replacement = values.clone();
        replacement.arrays = values.arrays.iter().map(|a| Arc::new((**a).clone())).collect();
        Arc::make_mut(&mut replacement.arrays[0])[0] = 17.0;
        f.input.set(true);
        assert!(dispatch(r, &replacement).is_err());
        assert_eq!((f.inputs.get(), run::Private::input_uploads(r)), (2, 1));
        f.input.set(false);
        tp::ok(dispatch(r, values)); assert_eq!(f.inputs.get(), 3);
        tp::ok(dispatch(r, &replacement)); tp::ok(dispatch(r, &replacement)); assert_eq!(f.inputs.get(), 4);
        let mut half = replacement.clone(); half.count = 512;
        tp::ok(dispatch(r, &half)); tp::ok(dispatch(r, &replacement));
        assert_eq!((f.inputs.get(), run::Private::input_uploads(r)), (6, 5));
        assert_eq!(run::Private::input_uploaded_bytes(r), 4 * 4096 + 2048);
        f.completion.set(true);
        let large = prepare(&fixture("arithmetic", 65536));
        assert!(dispatch(r, &large).is_err());
        assert!(run::buffer(&first, None).is_none() && run::readback(&first, None).is_err());
        assert_eq!((run::Private::buffer_creations(r), f.submitted.get(), f.inputs.get()), (5, 8, 7));
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
        tp::ok(dispatch(r, values));
        let creations = run::Private::buffer_creations(r);
        let thaws = run::Private::materialisations(r);
        let releases = f.releases.get();
        let mut views = 0;
        for i in 0..24 {
            let p = settled(r, values);
            if i == 0 { views = f.views.get(); }
            (p.release)(true); drop(p);
        }
        assert_eq!(run::Private::materialisations(r), thaws);
        assert!(run::Private::held_buffers(r) <= 2);
        assert!(run::Private::buffer_creations(r) - creations <= 4);
        assert!(f.views.get() == views && f.releases.get() > releases);
        f.working_set.set(Some(4));
        let oldest = settled(r, values); let expected = oldest.count * oldest.width;
        (oldest.release)(true);
        let before = run::Private::materialisations(r);
        let later: Vec<_> = (0..8).map(|_| settled(r, values)).collect();
        assert!(run::Private::materialisations(r) > before);
        assert_eq!(len((oldest.materialise)()), expected);
        for p in later { (p.release)(false); }
        f.working_set.set(None);
        let thaws = run::Private::materialisations(r);
        let live = settled(r, values); run::close(r, true);
        assert_eq!(run::Private::materialisations(r), thaws);
        assert!(std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| (live.materialise)())).is_err());
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
        let dead = settled(&pooled, values); (dead.release)(false); drop(dead);
        run::Private::pool_drain(&pool);
        let floats = run::Private::pool_free_floats(&pool); assert!(!floats.is_empty());
        for a in floats { assert!(a.borrow().iter().all(|x| x.is_nan())); }
        assert!(mem::bytes(&free_owner) > free_before);
        for _ in 0..1100 { run::Private::pool_drain(&pool); }
        assert_eq!(run::Private::pool_free_buffers(&pool), 0);
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
        for _ in 0..5 { run::Private::pool_give(&pool, device, 64 << 20); }
        let small = settled(&pooled, values); (small.release)(false); drop(small);
        run::Private::pool_drain(&pool); assert_eq!(run::Private::pool_free_buffers(&pool), 6);
        for _ in 0..2 { run::Private::pool_give(&pool, device, 128 << 20); }
        let over = settled(&pooled, values); (over.release)(false); drop(over);
        run::Private::pool_drain(&pool); assert_eq!(run::Private::pool_free_buffers(&pool), 7);
        f.working_set.set(None); run::close(&pooled, true); run::close_pool(&pool);
    });
}
fn evicted_and_pinned_holders() {
    with_fixture(|Fixture { device, queue, cache, kernel, values, .. }| {
        let pool = run::pool();
        let evicted = run::create_owned(Some(&pool), device, queue, cache, kernel);
        let retained = kept(&evicted, values); let unread = kept(&evicted, values);
        (retained.release)(true); run::close(&evicted, false);
        assert_eq!(run::Private::materialisations(&evicted), 0);
        assert_eq!(len((retained.materialise)()), retained.count * retained.width);
        run::close_pool(&pool);
        assert!(std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| (unread.materialise)())).is_err());
        let pool = run::pool();
        let pooled = run::create_owned(Some(&pool), device, queue, cache, kernel);
        let (stand_in, (_, _, valid, unpin), release) = pinned(&pooled, values);
        assert!(run::pin(&stand_in).is_none());
        assert!(valid() && run::Private::pool_held(&pool) == 1);
        unpin(); assert!(!valid() && run::Private::pool_held(&pool) == 0); release(true);
        let (_, (_, _, valid, _), _) = pinned(&pooled, values);
        assert!(valid()); run::close(&pooled, true); run::close_pool(&pool); assert!(!valid());
    });
}
fn slot_shrink_hysteresis() {
    with_fixture(|Fixture { device, queue, cache, kernel, values, .. }| {
        let big = prepare(&fixture("arithmetic", 400_000));
        let shrinking = run::create_owned(None, device, queue, cache, kernel);
        tp::ok(dispatch(&shrinking, &big)); let grown = run::Private::buffer_creations(&shrinking);
        for _ in 0..run::Private::slot_shrink_after { tp::ok(dispatch(&shrinking, values)); }
        assert!(run::Private::buffer_creations(&shrinking) > grown);
        let stable = run::Private::buffer_creations(&shrinking);
        for _ in 0..16 { tp::ok(dispatch(&shrinking, values)); }
        assert_eq!(run::Private::buffer_creations(&shrinking), stable); run::close(&shrinking, true);
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
        assert_eq!((first.bounds)(), Some(vec![0., 1., 0., 1023., 1., 0.]));
        let read = run::Private::readback_bytes(&sea);
        assert_eq!((f.creates.get() - c0, read), (2, 24));
        (first.bounds)(); assert_eq!(run::Private::readback_bytes(&sea), read);
        let second = settled(&sea, &values); let c1 = f.creates.get();
        (second.bounds)(); assert_eq!(f.creates.get(), c1);
        assert_eq!(run::Private::readback_bytes(&sea), 2 * read);
        assert_eq!(run::Private::bounds_readback_bytes(&sea), 2 * read);
        // The owner blocks in join: no Run::service/pumping may be necessary.
        let materialise = first.materialise;
        let materialise = std::thread::spawn(move || { let data = materialise(); assert_eq!(len(data), 1024 * 3); materialise }).join().unwrap();
        assert_eq!(run::Private::readback_bytes(&sea), 2 * read + 1024 * 3 * 4);
        materialise(); assert_eq!(run::Private::readback_bytes(&sea), 2 * read + 1024 * 3 * 4);
        assert_eq!(run::Private::bounds_readback_bytes(&sea), 2 * read);
        (first.release)(false); (second.release)(false); run::close(&sea, false);
    });
}

fn reduced_bounds_cover_every_point() {
    with_fixture(|Fixture { device, queue, cache, .. }| {
        for count in [1, 1023, 1024, 1025, 16383, 16384, 16385, 1_000_003] {
            let packed = tp::compile(&format!("(map (fn [x] [(- 17.0 x) x (- t x)]) (array/range {count}))"));
            let sea = run::create_owned(None, device, queue, cache, &tp::ok(emit::kernel(&emit::Options::default(), &packed)));
            let output = settled(&sea, &prepare(&packed));
            let last = (count - 1) as f64;
            assert_eq!((output.bounds)(), Some(vec![17. - last, 0., 1. - last, 17., last, 1.]), "count {count}");
            assert_eq!(run::Private::bounds_readback_bytes(&sea), 24);
            (output.release)(false);
            run::close(&sea, false);
        }
    });
}
fn main() {
    common::window::run(&[
        ("failed_commands_and_uploads", failed_commands_and_uploads),
        ("retired_holders_and_discard", retired_holders_and_discard),
        ("clobber_and_idle_ageing", clobber_and_idle_ageing),
        ("native_free_budget_size_classes", native_free_budget_size_classes),
        ("evicted_and_pinned_holders", evicted_and_pinned_holders),
        ("slot_shrink_hysteresis", slot_shrink_hysteresis),
        ("failed_table_upload_retries", failed_table_upload_retries),
        ("bounds_and_foreign_owned_readback", bounds_and_foreign_owned_readback),
        ("reduced_bounds_cover_every_point", reduced_bounds_cover_every_point),
    ]);
}
