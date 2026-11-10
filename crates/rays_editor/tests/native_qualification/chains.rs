//! Qualify native grid and fused-chain behavior at the authored fixture sizes.
#[path = "chains_frames.rs"]
mod frames;
#[path = "chains_scrub.rs"]
mod scrub;
pub fn scrub_child() -> bool { scrub::child() }
use flow_ir::{flow_ir::gpu, packed};
use flow_syntax::value::Value;
use rays_diagnostic::Diagnostic;
use flow_native::workspace_gpu as wg;
use flow_sop::{lower, value_lane};
use rdk_core::{attribute as a, geometry as g, packed::float3 as p};
use std::sync::{Arc, atomic::{AtomicUsize, Ordering}};

pub fn cases() -> Vec<(&'static str, fn())> {
    vec![("native_grid_gpu", grid), ("native_chain_gpu", chain), ("native_chain_frames_offset", || frames::run(0)), ("native_chain_frames_octaves", || frames::run(1)), ("native_chain_frames_int_octaves", || frames::run(2)),
        ("native_chain_frames_triangles", || frames::run(3)), ("native_chain_frames_normals", || frames::run(4)), ("native_chain_frames_bare", || frames::run(5)), ("native_scrub_gpu", scrub::run)]
}
fn ok<T>(v: Result<T, rays_diagnostic::Diagnostic>) -> T { v.unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d))) }
fn side() -> i64 { std::env::var("GRID_SIDE").map(|s| s.parse().expect("GRID_SIDE integer")).unwrap_or(256) }
fn lowered(text: &str, gpu: Option<gpu::Config>) -> lower::T {
    ok(lower::workspace(None, Vec::new(), lower::Options { gpu: gpu.unwrap_or_else(gpu::default), ..Default::default() }, &ok(flow_syntax::syntax::parse(text))))
}
fn node(text: &str, gpu: Option<gpu::Config>, after: Option<&str>) -> sop::node::T {
    let l = lowered(text, gpu);
    let graph = &l.graphs[0];
    let id = if let Some(name) = after {
        let write = |n: &&flow_eval::Node, name: &str| n.kind == "sop/with_attr" && n.args.iter().any(|(k,v)| k == "attribute" && matches!(v, flow_syntax::value::Value::Text(s) if s.as_ref() == name.as_bytes()));
        let first = l.plan.nodes.iter().find(|n| write(n, name)).expect("first write");
        let last = l.plan.nodes.iter().find(|n| n.id > first.id && write(n, "N")).expect("last write");
        l.compiled[&last.id]
    } else { graph.root.expect("root") };
    sop::edit_graph::compile_node(&graph.network.geometry, id).unwrap()
}
fn cook_in(session: &mut sop::session::T, node: &sop::node::T) -> g::T {
    let context = sop::context::create(sop::context::Options { time: Some(0.), domains: Some(4), ..Default::default() }).unwrap();
    sop::payload::geometry(&ok(sop::session::cook(session, None, &context, node)).payload).unwrap()
}
fn cook(node: &sop::node::T) -> g::T {
    let mut s = sop::session::create(4, 1 << 30).unwrap();
    let out = cook_in(&mut s, node);
    sop::session::close(&mut s);
    out
}
fn vec3(geo: &g::T, name: &str) -> p::T {
    match a::storage(&g::find_attribute(a::Owner::Point, name, geo).expect(name)).unwrap() { a::Storage::Float3(v) => v, _ => panic!("{name} not vec3") }
}
fn same(a: &p::T, b: &p::T, tolerance: f64) {
    assert_eq!(p::length(a), p::length(b));
    for i in 0..p::length(a) {
        let (x,y,z) = p::get(a,i).unwrap(); let (u,v,w) = p::get(b,i).unwrap();
        assert!((x-u).abs() <= tolerance && (y-v).abs() <= tolerance && (z-w).abs() <= tolerance, "point {i}: {x} {y} {z} != {u} {v} {w}");
    }
}
fn counted(config: &gpu::Config, count: &Arc<AtomicUsize>) -> gpu::Config {
    gpu::Config { backend: Some(Arc::new(Counted { inner: config.backend.clone().unwrap(), count: count.clone() })), ..config.clone() }
}
/// A backend whose kernels count their dispatches.
struct Counted { inner: Arc<dyn gpu::Backend>, count: Arc<AtomicUsize> }
struct CountedKernel { inner: Arc<dyn gpu::Kernel>, count: Arc<AtomicUsize> }
impl gpu::Backend for Counted {
    fn residency(&self, fast: bool) -> Option<Box<dyn gpu::Residency>> { self.inner.residency(fast) }
    fn cost(&self, i: Option<&packed::Inputs>, f: bool, p: &packed::T, n: i64) -> Option<f64> { self.inner.cost(i,f,p,n) }
    fn prepare(&self, fast: bool, p: &packed::T) -> Result<Arc<dyn gpu::Kernel>, Diagnostic> {
        Ok(Arc::new(CountedKernel { inner: self.inner.prepare(fast,p)?, count: self.count.clone() }))
    }
}
impl gpu::Kernel for CountedKernel {
    fn run(&self, i: &packed::Inputs) -> Result<gpu::Value, Diagnostic> { self.count.fetch_add(1,Ordering::SeqCst); self.inner.run(i) }
    fn readback(&self, v: &gpu::Value) -> Result<Value, Diagnostic> { self.inner.readback(v) }
    fn planes(&self, v: &gpu::Value) -> Result<gpu::Planes, Diagnostic> { self.inner.planes(v) }
}
fn grid() {
    assert!(crate::common::gpu(), "native GPU required");
    let n = side();
    let text = format!("(workspace g (graph g :context sop (sop/grid :counts \"Point counts\" :connectivity \"Points\" :columns {n} :rows {n} :size 3.0 :rotate 0.3)))");
    let start=std::time::Instant::now();let cpu_node=node(&text,None,None);let cpu_seconds=start.elapsed();let cpu=cook(&cpu_node);
    for force in [false,true] {
        let gpu = wg::create(None); if force { wg::qualification(&gpu); }
        wg::with_backend(&gpu,false,&mut |config| {
            drop(cook(&node(&text,Some(config.clone()),None)));
            let before=wg::image_stats(&gpu).0.readback_bytes;let start=std::time::Instant::now();let placed_node=node(&text,Some(config.clone()),None);let seconds=start.elapsed();let placed=cook(&placed_node);
            assert_eq!(g::point_count(&placed),n*n);
            for i in 0..n*n { let (x,y,z)=p::get(g::positions(&cpu),i).unwrap(); let (u,v,w)=p::get(g::positions(&placed),i).unwrap();
                for (a,b) in [(x,u),(y,v),(z,w)] { assert!((a-b).abs() <= 1e-4*(1.+a.abs())); }
            }
            println!("{} lowering {:?} (CPU {:?}), readback {} bytes",if force {"forced"}else{"measured"},seconds,cpu_seconds,wg::image_stats(&gpu).0.readback_bytes-before);
        }); wg::close(&gpu);
    }
}
fn chain() {
    assert!(crate::common::gpu(), "native GPU required");
    let n=side();
    let first=format!("(-> (sop/grid :counts \"Point counts\" :connectivity \"Points\" :columns {n} :rows {n}) (sop/attribute_noise :name \"noise\" :seed 7) (sop/transform :translate [0.0 1.0 0.0]))");
    let single=format!("(workspace chain (graph g :context sop {first}))");
    let consecutive=format!("(workspace chain (graph g :context sop (let* [a {first} b (-> a (sop/attribute_noise :name \"n2\" :seed 3) (sop/transform :translate [1.0 0.0 0.0]))] (sop/merge b (sop/attribute_noise a :name \"n2\")))))");
    let gathered=format!("(workspace chain (graph g :context sop (let* [a {first} n (sop/count a :points) pa (sop/attr a :P) b (-> a (sop/with_attr \"q\" (exact (map (fn [(i : int)] (array/nth pa (int/sub n (int/add i 1)))) (array/range n :type int)))) (sop/attribute_noise :name \"n2\" :seed 3) (sop/transform :translate [1.0 0.0 0.0]))] (sop/merge b (sop/attribute_noise a :name \"n2\")))))");
    let gpu=wg::create(None); wg::qualification(&gpu);
    wg::with_backend(&gpu,true,&mut |config| {
        let dispatch=Arc::new(AtomicUsize::new(0)); let config=counted(config,&dispatch);
        let stats=|| wg::image_stats(&gpu).0;
        for (text,after,expected,attribute) in [(&single,None,1,"P"),(&consecutive,Some("n2"),2,"P"),(&gathered,Some("q"),2,"q")] {
            let cpu_node=node(text,None,after); let placed=node(text,Some(config.clone()),after);
            let mut runs=Vec::new(); for _ in 0..5 { let t=std::time::Instant::now(); drop(cook(&cpu_node)); runs.push(t.elapsed()); } runs.sort(); println!("CPU median {:?}",runs[2]);
            let cpu=cook(&cpu_node); drop(cook(&placed)); dispatch.store(0,Ordering::SeqCst);
            let before=stats(); let device=cook(&placed); let after_stats=stats();
            let count=dispatch.load(Ordering::SeqCst); assert!(count == expected || (expected == 1 && count == 2));
            assert_eq!(after_stats.input_uploads,before.input_uploads); assert_eq!(after_stats.readback_bytes,before.readback_bytes); assert_eq!(after_stats.materialisations,before.materialisations);
            assert!(p::resident(g::positions(&device)));
            let mut runs=Vec::new(); for _ in 0..5 { let t=std::time::Instant::now(); drop(cook(&placed)); runs.push(t.elapsed()); } runs.sort(); println!("GPU median {:?}",runs[2]);
            if attribute == "P" {
                let before=stats(); p::get(g::positions(&device),0).unwrap(); assert_eq!(stats().materialisations,before.materialisations+1); p::get(g::positions(&device),1).unwrap(); assert_eq!(stats().materialisations,before.materialisations+1);
                same(g::positions(&cpu),g::positions(&device),1e-5);
            } else { same(&vec3(&cpu,"q"),&vec3(&device,"q"),1e-5); }
            if text == &single {
                let noise=|g:&g::T| match a::storage(&g::find_attribute(a::Owner::Point,"noise",g).unwrap()).unwrap() { a::Storage::Float(v)=>v,_=>panic!("noise") };
                for (a,b) in noise(&cpu).iter().zip(noise(&device).iter()) { assert!((a-b).abs()<=1e-5); }
                same(&vec3(&cpu,"N"),&vec3(&device,"N"),0.);
            }
            if text == &consecutive {
                let mut session=sop::session::create(1,1<<30).unwrap(); drop(cook_in(&mut session,&placed));
                let before=stats(); let freed=cook_in(&mut session,&placed); let after=stats(); assert!(after.freed-before.freed>=2); assert_eq!(after.materialisations,before.materialisations); assert_eq!(p::get(g::positions(&freed),7).unwrap(),p::get(g::positions(&device),7).unwrap()); sop::session::close(&mut session);
            }
        }
        let edit=|y:i64| node(&single.replace("[0.0 1.0 0.0]",&format!("[0.0 {y}.0 0.0]")),Some(config.clone()),None);
        let (a,b)=(edit(1),edit(2)); let mut session=sop::session::create(2,1<<30).unwrap();
        for i in 0..16 { drop(cook_in(&mut session,if i%2==0 {&a}else{&b})); }
        let before=stats(); dispatch.store(0,Ordering::SeqCst); let mut kept=Vec::new();
        for i in 0..16 { let out=cook_in(&mut session,if i%2==0 {&a}else{&b}); assert!(p::resident(g::positions(&out))); kept.push(out); }
        let after=stats(); assert_eq!(dispatch.load(Ordering::SeqCst),16); assert!(after.buffer_creations-before.buffer_creations<=48); assert_eq!(after.readback_bytes,before.readback_bytes); assert_eq!(after.input_uploads,before.input_uploads); assert_eq!(after.materialisations,before.materialisations);
        same(g::positions(&kept[0]),g::positions(&kept[14]),0.); same(&vec3(&kept[0],"N"),&vec3(&kept[14],"N"),0.);
        for name in ["noise"] { let read=|g:&g::T| a::storage(&g::find_attribute(a::Owner::Point,name,g).unwrap()).unwrap(); match (read(&kept[0]),read(&kept[14])) { (a::Storage::Float(a),a::Storage::Float(b))=>assert_eq!(a,b),_=>panic!("storage") } }
        drop(kept);
        for y in 3..=6 { let before=stats(); drop(cook_in(&mut session,&edit(y))); let after=stats(); if y>3 { assert!(after.buffer_creations-before.buffer_creations<=6); assert!(after.held_buffers<=30); } }
        sop::session::close(&mut session);
    });
    assert_eq!(wg::image_stats(&gpu).0.held_buffers,0); wg::close(&gpu);
}
