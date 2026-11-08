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
fn backend(c: &Arc<Counters>) -> Arc<dyn Gpu::Backend> {
    Arc::new(Mock(c.clone()))
}

struct Mock(Arc<Counters>);
struct MockPlanes { c: Arc<Counters>, count: i64, width: i64 }

impl Gpu::Backend for Mock {
    fn cost(&self, _: Option<&flow_ir::packed::Inputs>, _: bool, _: &flow_ir::packed::T, _: i64) -> Option<f64> { Some(0.0) }
    fn prepare(&self, _: bool, _: &flow_ir::packed::T) -> Result<Arc<dyn Gpu::Kernel>, rays_diagnostic::Diagnostic> { Ok(Arc::new(Mock(self.0.clone()))) }
}

impl Gpu::Kernel for Mock {
    fn run(&self, inputs: &flow_ir::packed::Inputs) -> Result<Gpu::Value, rays_diagnostic::Diagnostic> {
        Ok(Gpu::Value { identity: 1, count: inputs.count, width: 1, output: 0, stamp: 0, gpu_seconds: None })
    }
    fn readback(&self, _: &Gpu::Value) -> Result<flow_syntax::value::Value, rays_diagnostic::Diagnostic> {
        Err(rays_diagnostic::error("E_GPU".into(), "no readback".into(), None, None, None, Vec::new(), Vec::new()))
    }
    fn planes(&self, v: &Gpu::Value) -> Result<Gpu::Planes, rays_diagnostic::Diagnostic> {
        Ok(Gpu::Planes { count: v.count, width: v.width, resident: None, ops: Box::new(MockPlanes { c: self.0.clone(), count: v.count, width: v.width }) })
    }
}

impl Gpu::PlanesOps for MockPlanes {
    fn materialise(&self) -> Result<Vec<Vec<f64>>, rays_diagnostic::Diagnostic> { Ok((0..self.width).map(|_| vec![42.0; self.count as usize]).collect()) }
    fn settle(&self) -> Result<(), rays_diagnostic::Diagnostic> {
        self.c.settles.fetch_add(1, Ordering::SeqCst);
        if self.c.fail.load(Ordering::SeqCst) {
            Err(rays_diagnostic::error("E_KERNEL".into(), "mock status".into(), None, None, None, Vec::new(), Vec::new()))
        } else {
            Ok(())
        }
    }
    fn release(&self, _: bool) { self.c.releases.fetch_add(1, Ordering::SeqCst); }
    fn bounds(&self) -> Option<Vec<f64>> {
        self.c.bounded.fetch_add(1, Ordering::SeqCst);
        if self.width == 3 { Some(vec![42.0; 6]) } else { None }
    }
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
