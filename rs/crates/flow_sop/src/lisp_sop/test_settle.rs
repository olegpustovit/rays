//! Port of lib/flow_sop/test_settle.ml. A fused write placed on a mock device leaves its dispatch in
//! flight: the session settles it at a cook's end, P's bounds are the device's while P is unread, a
//! failed status recooks that write on the CPU (the result is the CPU cook's), and a result the
//! session drops (evicted by a later cook) releases its device outputs at the end of the first cook
//! whose root, and the previous cook's, do not hold them.
use super::support::*;
use crate::lower;
use flow_ir::flow_ir::Gpu;
use rdk::geometry as geo;
use std::sync::atomic::{AtomicBool, AtomicI64, Ordering};
use std::sync::Arc;

fn text(y: f64) -> String {
    format!(
        "(workspace w (graph g :context sop (-> (sop/grid :counts \"Point counts\" :connectivity \"Points\" :columns 8 :rows 8) (sop/attribute_noise :name \"noise\" :seed 7) (sop/transform :translate [0.0 {} 0.0]))))",
        flow_syntax::lisp::float(y)
    )
}

struct Counters {
    settles: AtomicI64,
    releases: AtomicI64,
    bounded: AtomicI64,
    fail: AtomicBool,
}

/// every output reads as 42s: a device result is recognisable, a CPU one is not
fn backend(c: &Arc<Counters>) -> Arc<Gpu::Backend> {
    let c = c.clone();
    Arc::new(Gpu::Backend {
        residency: Box::new(|_| None),
        cost: Box::new(|_, _, _, _| Some(0.0)),
        prepare: Box::new(move |_, _| {
            let c = c.clone();
            Ok(Arc::new(Gpu::Kernel {
                run: Box::new(|inputs| Ok(Gpu::Value { identity: 1, count: inputs.count, width: 1, output: 0, stamp: 0, gpu_seconds: None })),
                readback: Box::new(|_| Err(rays_diagnostic::error("E_GPU".into(), "no readback".into(), None, None, None, Vec::new(), Vec::new()))),
                planes: Box::new(move |v: &Gpu::Value| {
                    let (c1, c2, c3) = (c.clone(), c.clone(), c.clone());
                    let (count, width) = (v.count, v.width);
                    Ok(Gpu::Planes {
                        count,
                        width,
                        materialise: Box::new(move || (0..width).map(|_| vec![42.0; count as usize]).collect()),
                        resident: None,
                        settle: Box::new(move || {
                            c1.settles.fetch_add(1, Ordering::SeqCst);
                            if c1.fail.load(Ordering::SeqCst) {
                                Err(rays_diagnostic::error("E_KERNEL".into(), "mock status".into(), None, None, None, Vec::new(), Vec::new()))
                            } else {
                                Ok(())
                            }
                        }),
                        release: Box::new(move |_| {
                            c2.releases.fetch_add(1, Ordering::SeqCst);
                        }),
                        bounds: Box::new(move || {
                            c3.bounded.fetch_add(1, Ordering::SeqCst);
                            if width == 3 { Some(vec![42.0; 6]) } else { None }
                        }),
                    })
                }),
            }))
        }),
    })
}

fn node(gpu: Option<&Gpu::Config>, y: f64) -> sop::node::T {
    let options = lower::Options { gpu: gpu.cloned().unwrap_or_else(Gpu::default), ..lower::Options::default() };
    lower_text(options, &text(y)).1
}

fn positions(g: &geo::T) -> Vec<(f64, f64, f64)> {
    let p = geo::positions(g);
    (0..rdk::packed::Float3::length(p)).map(|i| rdk::packed::Float3::get(p, i).unwrap()).collect()
}

#[test]
#[ignore = "needs the device write placement of attribute_kernel (settle/release/bounds) and flow_gpu"]
fn in_flight_device_writes_settle_at_a_cooks_end() {
    let c = Arc::new(Counters { settles: AtomicI64::new(0), releases: AtomicI64::new(0), bounded: AtomicI64::new(0), fail: AtomicBool::new(false) });
    let gpu = Gpu::Config { backend: Some(backend(&c)), fast: true, policy: Gpu::Policy::Qualification, readback_per_float: None };
    let ctx = sop::context::create(sop::context::Options { time: Some(0.0), ..Default::default() }).unwrap();
    let session = || new_session(2, 1 << 24);
    let cpu = positions(&cook(&mut session(), &ctx, &node(None, 1.0)));
    // placed and settled: the device's values
    let mut s = session();
    let placed = cook(&mut s, &ctx, &node(Some(&gpu), 1.0));
    assert!(c.settles.load(Ordering::SeqCst) == 1 && rdk::packed::Float3::resident(geo::positions(&placed)));
    // its bounds are the device's, P left where it is; once read, a scan of the same values
    let p = geo::positions(&placed);
    assert!(rdk::packed::Float3::bounds(p).unwrap() == Some(vec![42.0; 6]) && c.bounded.load(Ordering::SeqCst) == 1);
    assert!(rdk::packed::Float3::resident(p));
    assert!(positions(&placed).iter().all(|p| *p == (42.0, 42.0, 42.0)));
    assert!(rdk::packed::Float3::bounds(p).unwrap() == Some(vec![42.0; 6]) && c.bounded.load(Ordering::SeqCst) == 1);
    // a toggled edit evicts the other write: its outputs are released once neither the cook's root
    // nor the previous cook's holds them
    cook(&mut s, &ctx, &node(Some(&gpu), 2.0));
    assert_eq!(c.releases.load(Ordering::SeqCst), 0);
    cook(&mut s, &ctx, &node(Some(&gpu), 1.0));
    assert!(c.releases.load(Ordering::SeqCst) > 0);
    // a failed status: the write recooks on the CPU, settled once, never released twice
    c.fail.store(true, Ordering::SeqCst);
    c.settles.store(0, Ordering::SeqCst);
    let recooked = cook(&mut session(), &ctx, &node(Some(&gpu), 1.0));
    assert_eq!(c.settles.load(Ordering::SeqCst), 1);
    assert!(!rdk::packed::Float3::resident(geo::positions(&recooked)));
    assert!(positions(&recooked) == cpu);
}
