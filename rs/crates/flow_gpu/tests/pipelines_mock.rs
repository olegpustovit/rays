//! Port of lib/flow_gpu/test_pipelines.ml: shared kernel sources compile once, the cache holds 64
//! pipelines and releases the evicted, `close` is idempotent and a closed cache refuses. The
//! portable mock cannot execute compute: this driver adds owned pipeline handles only.
mod common;
use common::test_program as tp;
use flow_gpu::pipelines;
use flow_shader::emit;
use ogpu::backend::{self as b, Driver, DriverPipeline};
use std::cell::Cell;
use std::rc::Rc;

#[test]
fn pipeline_ownership_on_mock() {
    ogpu::impl_::select(ogpu_mock::impl_::implementation());
    let (base, _control) = ogpu_mock::backend_mock::create();
    let live = Rc::new(Cell::new(0i64));
    let next = Rc::new(Cell::new(0i64));
    let (l1, n1) = (live.clone(), next.clone());
    let driver = Driver {
        create_device: Box::new(move || {
            let mut raw = (base.create_device)()?;
            raw.capabilities.compute_pipeline = true;
            let inner = raw.create_library;
            let (live, next) = (l1.clone(), n1.clone());
            raw.create_library = Box::new(move |shader| {
                let mut library = inner(shader)?;
                let (live, next) = (live.clone(), next.clone());
                library.create_compute_pipeline_in = Box::new(move |_entry, _constants, _interface| {
                    live.set(live.get() + 1);
                    next.set(next.get() + 1);
                    let live = live.clone();
                    Ok(DriverPipeline {
                        max_threads: 1024,
                        pipeline_token: next.get(),
                        destroy_pipeline: Box::new(move || {
                            live.set(live.get() - 1);
                            Ok(())
                        }),
                    })
                });
                Ok(library)
            });
            Ok(raw)
        }),
    };
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
