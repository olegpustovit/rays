//! Native measured placement, calibration and immutable generation qualification.
use super::isogrid::{self, ok};
use flow_native::workspace_gpu as gpu;
use flow_ir::{flow_ir::{gpu as ir_gpu, executor, profile, cost, Placement, PlacementReason as Reason}, packed};
use flow_sop::{lower, value_lane};
use flow_syntax::value::Value;
use rays_diagnostic::Diagnostic;
use rdk_core::{geometry as g, packed::float3};
use rays::{canvas, scene3};
use rays_math::vec3;
use std::sync::{Arc, Mutex, atomic::{AtomicBool, Ordering}};

fn compile_source(source: &str) -> packed::T {
    let forms = ok(flow_syntax::syntax::parse(source));
    let (workspace, diagnostics) = flow_check::workspace::check(&[], false, false, None, &flow_check::catalog::Catalog {version:1,kinds:vec![]}, &forms);
    assert!(diagnostics.is_empty(),"{}",diagnostics.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("; "));
    let evaluated = ok(flow_eval::static_eval(&workspace.unwrap()));
    let value = &evaluated.results.iter().find(|(name,_)| name=="g").unwrap().1;
    executor::packed_root(&ok(executor::compile(value,&Default::default()))).unwrap()
}
fn compile(body: &str) -> packed::T {
    compile_source(&format!("(workspace placement (graph g :context host (map (fn [i] {body}) (array/range 32768))))"))
}
fn inputs(program: &packed::T, index: bool, time: f64) -> packed::Inputs {
    ok(packed::host::prepare(program,&packed::host::PrepareOptions { fast:true,index,..Default::default() },&input::at_time(time)))
}
fn with_owner(run: impl Fn(&gpu::WorkspaceGpu,&ir_gpu::Config)) {
    assert!(crate::common::gpu(),"native qualification requires Vulkan");
    let baseline = ogpu_core::impl_::live_handles().unwrap();
    let owner = gpu::create(None);
    gpu::with_backend(&owner,true,&mut |config| run(&owner,config));
    gpu::close(&owner);
    assert_eq!(ogpu_core::impl_::live_handles().unwrap(),baseline);
}
fn calibration() {
    let program = compile("(+ (sin (+ i t)) 0.25)");
    with_owner(|owner,config| {
        assert_eq!(config.policy,ir_gpu::Policy::Measured);
        let backend = config.backend.as_ref().unwrap();
        let stats = || gpu::image_stats(owner).0;
        let cost = |p: &packed::T, i: Option<&packed::Inputs>| backend.cost(i,true,p,32768);
        let before = stats();
        let seconds = cost(&program,None);
        assert!(seconds.is_some_and(|s| s.is_finite() && s>=0.));
        let measured = stats();
        assert_eq!(measured.payload_readback_bytes,before.payload_readback_bytes);
        assert!(measured.status_readback_bytes>before.status_readback_bytes);
        assert_eq!(cost(&program,None),seconds);
        assert_eq!(stats(),measured);
        let input = compile_source("(workspace input (graph g :context host [(data : (array float) (array/float 32768))] (map (fn [x] (+ x t)) data)))");
        assert_eq!(packed::host::view(&input).widths.len(),1);
        let cpu_seconds = cost(&input,None);
        assert!(cpu_seconds.is_some());
        let transferred = stats();
        assert_eq!(transferred.input_uploaded_bytes-measured.input_uploaded_bytes,4*32768*4);
        assert_eq!(transferred.payload_readback_bytes,measured.payload_readback_bytes);
        assert_eq!(cost(&input,None),cpu_seconds);
        assert_eq!(stats(),transferred);
        let ranged = compile("(+ (float i) t)");
        let integers = compile_source("(workspace input (graph g :context host [(data : (array int) (array/int 32768))] (map (fn [x] (+ (float x) t)) data)))");
        assert!(cost(&ranged,None).is_some());
        let range_done = stats();
        let integer_cost = cost(&integers,None);
        assert!(integer_cost.is_some());
        let integer_done = stats();
        assert_eq!(integer_done.input_uploaded_bytes-range_done.input_uploaded_bytes,4*32768*4);
        assert_eq!(integer_done.payload_readback_bytes,range_done.payload_readback_bytes);
        assert_eq!(cost(&integers,None),integer_cost);
        assert_eq!(stats(),integer_done);
        let table = compile_source("(workspace table (graph g :context host [(data : (array float) (array/float 1024 0.25))] (map (fn [i] (+ (array/nth data (int (mod i 1024))) t)) (array/range 32768))))");
        let prepared = inputs(&table,true,0.);
        assert!(prepared.tables.iter().any(|values| values.len()==1024));
        let before_table = stats();
        let table_cost = cost(&table,Some(&prepared));
        assert!(table_cost.is_some());
        let table_done = stats();
        assert_eq!(table_done.input_uploaded_bytes-before_table.input_uploaded_bytes,4*1024*4);
        assert_eq!(table_done.payload_readback_bytes,before_table.payload_readback_bytes);
        assert_eq!(cost(&table,Some(&prepared)),table_cost);
        assert_eq!(stats(),table_done);
        let larger = packed::Inputs { tables: prepared.tables.iter().map(|v| Arc::new(v.iter().chain(v.iter()).copied().collect())).collect(),..prepared.clone() };
        let larger_cost = cost(&table,Some(&larger));
        assert!(larger_cost.is_some());
        let larger_done = stats();
        assert_eq!(larger_done.input_uploaded_bytes-table_done.input_uploaded_bytes,4*2048*4);
        assert_eq!(cost(&table,Some(&larger)),larger_cost);
        assert_eq!(stats(),larger_done);
        let mixed = compile_source("(workspace mixed (graph g :context host [(a : (array float) (array/float 32768 1.)) (b : (array float) (array/float 32768 2.))] (map (fn [x y] (+ (+ x y) t)) a b)))");
        let mixed_inputs = inputs(&mixed,false,0.);
        assert_eq!(mixed_inputs.arrays.len(),2);
        let before_cpu_mix = stats();
        assert!(cost(&mixed,Some(&mixed_inputs)).is_some());
        let cpu_mix = stats();
        assert_eq!(cpu_mix.input_uploaded_bytes-before_cpu_mix.input_uploaded_bytes,4*2*32768*4);
        let producer = ok(backend.prepare(true,&ranged));
        let producer_inputs = inputs(&ranged,true,0.);
        let kept = ok(producer.planes(&ok(producer.run(&producer_inputs))));
        ok(kept.settle());
        let placeholder = match kept.resident.as_ref().unwrap() { Value::Floats(1, v)=>v.clone(),_=>panic!("float resident expected") };
        let held_inputs = packed::Inputs { arrays:vec![placeholder,mixed_inputs.arrays[1].clone()],..mixed_inputs.clone() };
        let before_held = stats();
        let held_cost = cost(&mixed,Some(&held_inputs));
        assert!(held_cost.is_some());
        let held_done = stats();
        assert_eq!(held_done.input_uploaded_bytes-before_held.input_uploaded_bytes,4*32768*4);
        assert_eq!(held_done.payload_readback_bytes,before_held.payload_readback_bytes);
        assert_eq!(cost(&mixed,Some(&held_inputs)),held_cost);
        assert_eq!(stats(),held_done);
        assert_eq!(kept.materialise().unwrap()[0][32767],32767.);
        kept.release(false);
        let recovered_kernel = compile("(if (> t 0.) (* (* (+ i 1.) 1e20) 1e20) 0.)");
        assert_eq!(cost(&recovered_kernel,Some(&inputs(&recovered_kernel,true,1.))),None);
        let good_frame = inputs(&recovered_kernel,true,0.);
        let recovered_cost = cost(&recovered_kernel,Some(&good_frame));
        assert!(recovered_cost.is_some());
        let recovered = stats();
        assert_eq!(cost(&recovered_kernel,Some(&good_frame)),recovered_cost);
        assert_eq!(stats(),recovered);
        let before_rejection = stats();
        let failing = compile("(* (* (+ (+ i t) 1.) 1e20) 1e20)");
        assert_eq!(cost(&failing,None),None);
        let rejected = stats();
        assert_eq!(rejected.payload_readback_bytes,before_rejection.payload_readback_bytes);
        assert!(rejected.status_readback_bytes>before_rejection.status_readback_bytes);
        assert_eq!(cost(&failing,None),None);
        assert_eq!(stats(),rejected);
    });
}
fn wave() -> String { (0..16).fold("(+ (float i) t)".into(),|x,_| format!("(sin {x})")) }
fn lower_source(config: &ir_gpu::Config, source: &str) -> (lower::T,sop::node::T) {
    let lowered = ok(lower::workspace(None,vec![],lower::Options { gpu:config.clone(),defer:true,..Default::default() },&ok(flow_syntax::syntax::parse(source))));
    let graph = &lowered.graphs[0];
    let node = sop::edit_graph::compile_node(&graph.network.geometry,graph.root.unwrap()).unwrap();
    (lowered,node)
}
fn consistent(p: &Placement) -> bool { p.reason==cost::choose(p.cpu_seconds,p.device_seconds,false,p.cpu_consumers.unwrap()) }
fn chain_source(n: i64) -> String {
    format!("(workspace chain (graph g :context sop (sop/mesh_planes :points {{:x (exact (map (fn [i] {}) (array/range {n}))) :y (array/float {n}) :z (array/float {n})}} :vertex_points (array/int 0) :primitive_offsets (array/int 1) :kinds (array/int 0))))",wave())
}
fn chain_counterexamples() {
    with_owner(|_owner,config| {
        assert_eq!(config.policy,ir_gpu::Policy::Measured);
        let mut session = sop::session::create(0,0).unwrap();
        let mut lowered_by_size = std::collections::HashMap::new();
        let mut cook = |n,readers| {
            let (lowered,node) = lowered_by_size.entry(n).or_insert_with(|| lower_source(config,&chain_source(n)));
            let context = sop::context::create(sop::context::Options {domains:Some(1),..Default::default()}).unwrap();
            let output = ok(sop::session::cook(&mut session,readers,&context,node));
            let geometry = ok(sop::payload::geometry(&output.payload));
            (float3::resident(g::positions(&geometry)),profile::placements(&lowered.profile)[0].clone())
        };
        let (resident,p) = cook(65536,None);
        assert!(!resident && p.reason==Reason::Missing_consumer && p.cpu_consumers.is_none());
        let mut settle = |n,readers| {
            for _ in 0..4 { cook(n,Some(readers)); }
            for i in 0..=4 {
                let result = cook(n,Some(readers));
                if result.1.cpu_seconds.is_some() && result.1.device_seconds.is_some() || i==4 {return result;}
            }
            unreachable!()
        };
        let (_,free) = settle(65536,0);
        assert!(free.cpu_seconds.is_some() && free.device_seconds.is_some() && consistent(&free));
        let (_,one) = settle(65536,1);
        let (_,two) = settle(65536,2);
        assert_eq!(one.cpu_consumers,Some(1)); assert_eq!(two.cpu_consumers,Some(2));
        assert!(consistent(&one) && consistent(&two));
        let (_,small) = settle(32,1);
        assert!(consistent(&small),"small chain: {small:?}");
        let (_,grown) = cook(65536,Some(1));
        assert_ne!(grown.count,small.count);
        for (n,readers,reason) in [(32,1,Reason::Cpu_consumer),(32,0,Reason::Cpu_measured_cheaper),(65536,0,Reason::Device_measured_cheaper)] {
            let backend = config.backend.as_ref().unwrap();
            let device_ran = Arc::new(AtomicBool::new(false));
            let on_device = Arc::new(AtomicBool::new(false));
            let wrapped = Arc::new(Observed { ran: device_ran.clone(), on: on_device.clone(), inner: backend.clone() });
            let now = Mutex::new(0.);
            let lowered = ok(lower::workspace(None,vec![],lower::Options {
                gpu:ir_gpu::Config {backend:Some(wrapped),..config.clone()},defer:true,
                clock:Arc::new(move || {let mut now=now.lock().unwrap();*now+=if device_ran.swap(false,Ordering::SeqCst) {1e-3} else {1e-6*n as f64};*now}),..Default::default()
            },&ok(flow_syntax::syntax::parse(&chain_source(n)))));
            let graph = &lowered.graphs[0];
            let node = sop::edit_graph::compile_node(&graph.network.geometry,graph.root.unwrap()).unwrap();
            let context = sop::context::create(sop::context::Options {domains:Some(1),..Default::default()}).unwrap();
            for _ in 0..6 {
                on_device.store(false,Ordering::SeqCst);
                ok(sop::session::cook(&mut session,Some(readers),&context,&node));
            }
            let p = profile::placements(&lowered.profile)[0].clone();
            assert_eq!(p.reason,reason,"{n} points priced: {p:?}");
            assert_eq!(on_device.load(Ordering::SeqCst),cost::on_device(reason));
        }
        sop::session::close(&mut session);
        for (side,readers,fresh) in [(512,0,true),(512,1,false),(8,0,false)] {
            let source = format!("(workspace chain (graph g :context sop (-> (sop/grid :counts \"Point counts\" :connectivity \"Points\" :columns {side} :rows {side}) (sop/attribute_noise :name \"noise\" :seed 7 :offset [(get (frame/input) :t) 0.0 0.0]) (sop/transform :translate [0.0 1.0 0.0]))))");
            let lowered = ok(lower::workspace(None,vec![],lower::Options {gpu:config.clone(),..Default::default()},&ok(flow_syntax::syntax::parse(&source))));
            let graph = &lowered.graphs[0];
            let lane = value_lane::create(None);
            let mut session = sop::session::create(4,1<<30).unwrap();
            for frame in 0..6 {
                let time = frame as f64/60.; let live = input::at_time(time);
                let resolved = ok(value_lane::resolve(Some(config),Some(&live),&lane,time,&graph.network));
                let node = sop::edit_graph::compile_node(&resolved.geometry,graph.root.unwrap()).unwrap();
                let context = sop::context::create(sop::context::Options {time:Some(time),input:Some(live),..Default::default()}).unwrap();
                let geometry = ok(sop::payload::geometry(&ok(sop::session::cook(&mut session,Some(readers),&context,&node)).payload));
                let p = profile::placements(&lowered.profile).into_iter().find(|p| p.owner.0 != vec!["sop/mesh_planes".to_owned()]).unwrap();
                assert_eq!(p.cpu_consumers,Some(readers));
                let resident = float3::resident(g::positions(&geometry));
                if fresh && frame<2 { assert!(p.reason==Reason::No_cost_evidence && p.cpu_seconds.is_none() && resident); }
                else if frame>=4 { assert!(p.cpu_seconds.is_some() && p.device_seconds.is_some()); assert!(consistent(&p)); assert_eq!(resident,cost::on_device(p.reason)); }
                if side==512 && readers==0 && frame==5 {assert_eq!(p.reason,Reason::Device_measured_cheaper);}
            }
            sop::session::close(&mut session);
        }
        let evidence = &config.evidence;
        flow_sop::attribute_kernel::display_sink::observe(evidence,false,1_000_000,1.);
        let base = flow_sop::attribute_kernel::display_sink::rates(evidence).0;
        for seconds in [2.,1.,3.] {flow_sop::attribute_kernel::display_sink::observe_draw(evidence,1_000_000,seconds);}
        assert!((flow_sop::attribute_kernel::display_sink::rates(evidence).0-base-1e-6).abs()<=1e-12);
    });
}
fn retained_generations() {
    with_owner(|owner,config| {
        let config = ir_gpu::Config {readback_per_float:None,..config.clone()};
        let source = format!("(workspace generations (graph g :context sop (sop/with_attr (sop/mesh_planes :points {{:x (array/float 262144) :y (array/float 262144) :z (array/float 262144)}} :vertex_points (array/int 0) :primitive_offsets (array/int 1) :kinds (array/int 0)) :P (exact (map (fn [i] [(+ (+ (- (/ (mod i 256) 256) 0.5) (* t 0.002)) (* {} 0.01)) (- (/ (floor (/ i 256)) 256) 0.5) 0]) (array/range 262144))))))",wave());
        let (small,small_node) = lower_source(&config,&source.replace("262144","32"));
        let mut small_session = sop::session::create(0,0).unwrap();
        isogrid::cook(&mut small_session,&small_node,0.); isogrid::cook(&mut small_session,&small_node,0.);
        let geometry = isogrid::cook(&mut small_session,&small_node,0.);
        assert!(!float3::resident(g::positions(&geometry)));
        let placements = profile::placements(&small.profile);
        assert!(placements.iter().any(|p| matches!(p.reason,Reason::Cpu_consumer|Reason::Cpu_measured_cheaper)));
        assert!(!placements.iter().any(|p| cost::on_device(p.reason)));
        sop::session::close(&mut small_session);
        let (lowered,node) = lower_source(&config,&source);
        let mut session = sop::session::create(4,1<<26).unwrap();
        let canvas = canvas::create(64,64).unwrap(); let other=canvas::create(64,64).unwrap();
        let camera = rays::camera::orthographic(2.,vec3::create(0.,0.,3.),vec3::zero()).unwrap();
        let cook = |session: &mut sop::session::T,node: &sop::node::T,time| {
            let geometry = isogrid::cook(session,node,time);
            assert!(float3::resident(g::positions(&geometry)),"Measured lifetime fixture chose CPU: {:?}",profile::placements(&lowered.profile));
            let picture = isogrid::picture(owner,&geometry);
            let mesh = scene3::walk::drawings(&picture)[0].mesh.clone();
            let key = rays::mesh::view::device(&mesh).unwrap().0;
            (geometry,key,isogrid::frame(&camera,picture),time)
        };
        for time in [-2.,-1.] {isogrid::cook(&mut session,&node,time);}
        let mut previous = cook(&mut session,&node,0.);
        let mut expected = isogrid::draw(&canvas,&previous.2);
        assert!(expected.iter().any(|c| i64::from(c.r)+i64::from(c.g)+i64::from(c.b)>0));
        let mut warm = None; let mut heap=0;
        for i in 1..=200 {
            let next = cook(&mut session,&node,i as f64);
            isogrid::draw(&other,&next.2);
            assert_eq!(isogrid::draw(&canvas,&previous.2),expected);
            gpu::frame(owner,Some(&[previous.1.clone(),next.1.clone()]));
            assert!(gpu::held_displays(owner)<=6);
            previous=next;
            expected=isogrid::draw(&canvas,&previous.2);
            if i==16 {warm=Some(gpu::image_stats(owner).0);heap=mem_sys::allocated_bytes() as i64;}
        }
        let first=warm.unwrap();let last=gpu::image_stats(owner).0;
        let heap_delta=mem_sys::allocated_bytes() as i64-heap;
        assert_eq!(last.pipeline_compilations,first.pipeline_compilations);
        assert_eq!(last.runners_created,first.runners_created);
        assert!(last.buffer_creations<=first.buffer_creations+4);
        assert!(last.status_reads>=first.status_reads+184);
        assert!(last.held_buffers<=first.held_buffers+4);
        assert!(heap_delta<=262144*std::mem::size_of::<usize>() as i64,"retained bytes {heap_delta}");
        assert_eq!(last.payload_readback_bytes,0);
        assert!(profile::placements(&lowered.profile).iter().any(|p| p.cpu_consumers==Some(0) && cost::on_device(p.reason)));
        let cancel=Arc::new(rdk_core::cancel::create());rdk_core::cancel::cancel(&cancel);
        let context=sop::context::create(sop::context::Options {domains:Some(1),cancel:Some(cancel),time:Some(201.),..Default::default()}).unwrap();
        match sop::session::cook(&mut session,Some(0),&context,&node) {Err(d)=>assert!(d.code=="E_CANCELLED" || d.code=="cancelled"),Ok(_)=>panic!("cancelled placement cook published geometry")}
        assert_eq!(gpu::image_stats(owner).0.payload_readback_bytes,0);
        let recovered=cook(&mut session,&node,201.);isogrid::draw(&other,&recovered.2);
        assert_eq!(isogrid::draw(&canvas,&previous.2),expected);
        let (_,edited_node)=lower_source(&config,&source.replace("0.002","0.004"));
        let edited=cook(&mut session,&edited_node,202.);isogrid::draw(&other,&edited.2);
        assert_eq!(isogrid::draw(&canvas,&previous.2),expected);
        assert_eq!(gpu::image_stats(owner).0.payload_readback_bytes,0);
        let read=|geometry: &g::T,time: f64,scale: f64| {
            let displacement=(0..16).fold(time,|x,_| x.sin())*0.01;
            let (x,y,z)=float3::get(g::positions(geometry),0).unwrap();
            assert!((x-(-0.5+time*scale+displacement)).abs()<1e-6 && y== -0.5 && z==0.);
        };
        read(&previous.0,previous.3,0.002);
        gpu::frame(owner,Some(&[]));gpu::frame(owner,None);gpu::frame(owner,None);
        assert_eq!(gpu::held_displays(owner),0);
        for (generation,scale) in [(&recovered,0.002),(&edited,0.004)] {
            let geometry = &generation.0;
            let time = generation.3;
            std::thread::scope(|scope| {
                let workers:Vec<_>=(0..4).map(|_| scope.spawn(|| { for _ in 0..2 { read(geometry,time,scale); } })).collect();
                // Lazy materialisation routes back through the initial-domain service.
                while workers.iter().any(|w| !w.is_finished()) {gpu::pump(owner,true);std::thread::yield_now();}
                for worker in workers {worker.join().unwrap();}
            });
            let bytes=gpu::image_stats(owner).0.payload_readback_bytes;
            read(&generation.0,generation.3,scale);
            assert_eq!(gpu::image_stats(owner).0.payload_readback_bytes,bytes);
        }
        canvas::destroy(&canvas).unwrap();canvas::destroy(&other).unwrap();sop::session::close(&mut session);
    });
}
fn isogrid_generations() {
    let workspace=isogrid::workspace();
    with_owner(|owner,config| {
        let mut session=sop::session::create(0,1<<28).unwrap();
        let checksum=|geometry: &g::T| {
            let positions=g::positions(geometry);let mut sum=0.;
            for i in 0..float3::length(positions) {let (x,y,z)=float3::get(positions,i).unwrap();sum=sum+x*1.0001+y*1.0002+z*1.0003;}
            (g::point_count(geometry),sum)
        };
        let mut expected=Vec::new();
        for recipe in [3,0] {
            let (_,node)=isogrid::recipe_node(&workspace,&ir_gpu::Config {fast:true,..ir_gpu::default()},recipe);
            expected.push(checksum(&isogrid::cook(&mut session,&node,0.)));
        }
        let (_,node_a)=isogrid::recipe_node(&workspace,config,3);
        let a=isogrid::placed(&mut session,&node_a);
        let before_display=gpu::image_stats(owner).0;
        let picture=isogrid::picture(owner,&a);
        assert_eq!(gpu::display_refusal(owner),None);
        assert!(rays::mesh::view::device(&scene3::walk::drawings(&picture)[0].mesh).is_some());
        assert_eq!(gpu::image_stats(owner).0.payload_readback_bytes,before_display.payload_readback_bytes);
        assert_eq!(gpu::image_stats(owner).0.input_uploaded_bytes,before_display.input_uploaded_bytes);
        let (_,node_b)=isogrid::recipe_node(&workspace,config,0);
        let b=isogrid::placed(&mut session,&node_b);
        let (_,node_a2)=isogrid::recipe_node(&workspace,config,3);
        let a2=isogrid::placed(&mut session,&node_a2);
        let before=gpu::image_stats(owner).0.payload_readback_bytes;
        assert_eq!(checksum(&a),expected[0]);assert_eq!(checksum(&b),expected[1]);assert_eq!(checksum(&a2),expected[0]);
        let read=gpu::image_stats(owner).0.payload_readback_bytes;
        assert!(read>before);
        assert_eq!(checksum(&a),expected[0]);assert_eq!(gpu::image_stats(owner).0.payload_readback_bytes,read);
        sop::session::close(&mut session);
    });
}
pub fn cases() -> Vec<(&'static str, fn())> {
    vec![("native_placement_calibration",calibration),("native_placement_chain_counterexamples",chain_counterexamples),("native_placement_retained_generations",retained_generations),("native_placement_isogrid_generations",isogrid_generations)]
}

/// A backend that records that a residency scope was opened, otherwise forwarding to `inner`.
struct Observed { ran: Arc<AtomicBool>, on: Arc<AtomicBool>, inner: Arc<dyn ir_gpu::Backend> }
impl ir_gpu::Backend for Observed {
    fn residency(&self, fast: bool) -> Option<Box<dyn ir_gpu::Residency>> { self.ran.store(true,Ordering::SeqCst); self.on.store(true,Ordering::SeqCst); self.inner.residency(fast) }
    fn cost(&self, i: Option<&packed::Inputs>, fast: bool, p: &packed::T, n: i64) -> Option<f64> { self.inner.cost(i,fast,p,n) }
    fn prepare(&self, fast: bool, p: &packed::T) -> Result<Arc<dyn ir_gpu::Kernel>, Diagnostic> { self.inner.prepare(fast,p) }
}
