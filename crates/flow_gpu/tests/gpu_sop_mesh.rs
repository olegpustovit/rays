//! lib/flow_sop/test_mesh_resident.ml: all 42 attempts, exact topology/readback/ownership oracles.
//! Arc-backed placeholders are immutable; exact publication/readback checks detect accidental host reads.
mod common;
use flow_gpu::{host as h,run};
use common::{Observe,Seen};
use flow_ir::{flow_ir::Gpu,core_exec,packed};
use rays_diagnostic::Diagnostic;
use flow_sop::{lower,value_lane,topology_kernel,topology_kernel_impl};
use flow_syntax::{syntax,ty::Ty,value::Value};
use rdk_core::cancel;
use rdk_core::{geometry as g, packed::Float3, topology};
use sop::{context as ctx,edit_graph as edit,node,payload,session};
use std::sync::{Arc,atomic::{AtomicBool,AtomicUsize,Ordering}};
fn source(host:bool,case:&str)->String{
    let domain=if case=="overflow"{"selected"}else{"(array/range count :type int)"};
    let vertex=match case{"vertex"=>"(if (int/eq i 1025) 1026 i)","negative"=>"(if (int/eq i 1025) -1 i)",_=>"i"};
    let offset=match case{"start"=>"(if (int/eq i 0) 1 (int/mul i 3))","terminal"=>"(if (int/eq i n) (int/sub count 1) (int/mul i 3))","order"=>"(if (int/eq i 1) 9 (int/mul i 3))","minimum"=>"(if (int/eq i 1) 2 (int/mul i 3))",_=>"(int/mul i 3)"};
    let vertices=if case=="points"{"(array/range (int/mod (frame/index) 342) :type int)".into()}else{format!("(map (fn [(i : int)] (int/add {vertex} (int/mod (frame/index) 1))) {domain})")};
    let kinds=match case{"points"=>"(array/int (int/mod (frame/index) 342) 0)","length"=>"(array/int (int/sub n 1) 0)","kind"=>"(map (fn [(i : int)] (if (int/eq i 0) 3 0)) (array/range n :type int))",_=>"(array/int n 0)"};
    let offsets=match case{"points"=>"(array/range (int/add (int/mod (frame/index) 342) 1) :type int)".into(),"missing"=>"(filter (fn [(i : int)] (int/lt i (int/mod (frame/index) 342))) (array/range (int/add n 1) :type int))".into(),_=>format!("(map (fn [(i : int)] (int/add {offset} (int/mod (frame/index) 1))) (array/range (int/add n 1) :type int))")};
    let extra=if case=="overflow"{"selected (filter (fn [(i : int)] (int/lt i count)) (array/range 8196 :type int))"}else{""};
    let result=match case{"cpu"=>"(sop/clean mesh :remove_degenerate false :remove_nan_points false :remove_unused_points false :reverse_winding true)","twice"=>"(sop/mesh :points (if (int/eq (sop/count mesh :points) count) (exact points) (error \"E_TEST\" \"first mesh count\")) :vertex_points (exact vertices) :primitive_offsets (exact offsets) :kinds (exact kinds))",_=>"mesh"};
    let body=format!("(let* [n (frame/index) count (int/mul n 3) {extra} points (map (fn [(i : int)] [(+ (float i) t) 0. 0.]) {domain}) vertices {vertices} offsets {offsets} kinds {kinds} mesh (sop/mesh :points (exact points) :vertex_points (exact vertices) :primitive_offsets (exact offsets) :kinds (exact kinds))] {result})");
    let body=if host{format!("(if (int/gt (sop/count (sop/mesh :points (array/vec3 1) :vertex_points (array/int 0) :primitive_offsets (array/int 1) :kinds (array/int 0)) :points) 0) {body} (sop/mesh :points (array/vec3 0) :vertex_points (array/int 0) :primitive_offsets (array/int 1) :kinds (array/int 0)))")}else{body};
    format!("(workspace resident_mesh (graph g :context sop {body}))")
}
fn read(geometry:&g::T,count:i64,case:&str){
    let t=topology::View::view(g::topology(geometry)).unwrap();let prims=if case=="points"{0}else{count};
    assert_eq!(t.vertex_points.as_slice(),(0..3*prims).map(|i|if case=="cpu"{3*(i/3)+2-i%3}else{i}).collect::<Vec<_>>());
    assert_eq!(t.primitive_offsets.as_slice(),(0..=prims).map(|i|3*i).collect::<Vec<_>>());assert_eq!(t.primitive_kinds.as_ref(),vec![0;prims as usize]);
    for i in 0..3*count{assert_eq!(Float3::get(g::positions(geometry),i).unwrap(),(i as f64,0.,0.));}
}
/// A residency whose executor is another's; the rest is the base's.
struct Wrapped{base:Box<dyn Gpu::Residency>,executor:Arc<dyn core_exec::Resident>}
impl Gpu::Residency for Wrapped{
    fn executor(&self)->&Arc<dyn core_exec::Resident>{&self.executor}
    fn retain_uint32(&self,v:&Value)->Option<Gpu::Integers>{self.base.retain_uint32(v)}
    fn retain_float(&self,v:&Value)->Option<Gpu::Planes>{self.base.retain_float(v)}
    fn pack_planes(&self,v:&[Value;3])->Result<Option<Value>,Diagnostic>{self.base.pack_planes(v)}
    fn incidence(&self)->Gpu::IncidenceBuilder{self.base.incidence()}
    fn validate_topology(&self,n:i64,v:&Value,o:&Value,k:&Value,max:i64)->Option<Result<(),Diagnostic>>{self.base.validate_topology(n,v,o,k,max)}
    fn ready(&self)->bool{self.base.ready()}
}
/// A backend whose residencies are observed: once the device has dispatched, `cancel_case` cancels
/// the cook at its next settle or count, and a close is counted.
struct Watched{base:Box<dyn Gpu::Backend>,cancel:Arc<cancel::T>,closes:Arc<AtomicUsize>,cancel_case:bool}
impl Gpu::Backend for Watched{
    fn residency(&self,fast:bool)->Option<Box<dyn Gpu::Residency>>{
        let base=self.base.residency(fast)?;
        // Retained-stream callbacks share this executor; observe it by delegation.
        let dispatched=AtomicBool::new(false);let(cancel,closes,cancel_case)=(self.cancel.clone(),self.closes.clone(),self.cancel_case);
        let executor=Arc::new(Observe{base:base.executor().clone(),one_by_one:true,seen:Box::new(move|seen|match seen{
            Seen::Invoke(Some(_))=>dispatched.store(true,Ordering::SeqCst),
            Seen::Settle|Seen::Count(Some(_)) if cancel_case&&dispatched.load(Ordering::SeqCst)=>cancel::cancel(&cancel),
            Seen::Close if dispatched.load(Ordering::SeqCst)=>{closes.fetch_add(1,Ordering::SeqCst);}
            _=>{}
        })});
        Some(Box::new(Wrapped{base,executor}))
    }
    fn cost(&self,inputs:Option<&packed::Inputs>,fast:bool,packed:&packed::T,count:i64)->Option<f64>{self.base.cost(inputs,fast,packed,count)}
    fn prepare(&self,fast:bool,packed:&packed::T)->Result<Arc<dyn Gpu::Kernel>,Diagnostic>{self.base.prepare(fast,packed)}
}
fn run_case(test:&str,empty_offset:bool){
    assert!(common::gpu(),"native SOP mesh qualification requires a real GPU");common::release_keepalive();let baseline=ogpu::impl_::live_handles().unwrap();
    assert!(common::gpu());let gpu=common::lease();let host=h::create(&||0.,&gpu);
    let host_call=test.starts_with("host");let case=match test{"host"=>"valid","host_twice"=>"twice","host_cpu"=>"cpu","host_cancel"=>"cancel","host_negative"=>"negative","host_dynamic"=>"dynamic","host_overflow"=>"overflow","host_vertex"=>"vertex","host_points"=>"points",c=>c};
    let bad=!matches!(case,"valid"|"dynamic"|"cpu"|"cancel"|"twice"|"points"|"overflow");
    if empty_offset{
        let backend=h::backend(&host);let scope=backend.residency(true).expect("native residency");let offset=scope.executor().fill(&Ty::Array(Box::new(Ty::Int)),1,&Value::Int(1)).unwrap().unwrap();
        let args=vec![("points".into(),Value::Vec3_array(Arc::new(vec![]))),("vertex_points".into(),Value::Int_array(Arc::new(vec![]))),("primitive_offsets".into(),offset),("kinds".into(),Value::Int_array(Arc::new(vec![])))];
        let d=topology_kernel_impl::mesh_resident(None,&*scope,&args).expect("resident path").err().expect("malformed empty offset accepted");assert_eq!((d.code.as_str(),d.message.as_str()),("E_TOPOLOGY","Topology: first primitive offset must be zero"));scope.executor().close();assert_eq!((h::stats(&host).payload_readback_bytes,h::stats(&host).validation_readback_bytes),(0,36));
    }
    let cancel=Arc::new(cancel::create());let closes=Arc::new(AtomicUsize::new(0));
    let backend=Watched{base:h::backend(&host),cancel:cancel.clone(),closes:closes.clone(),cancel_case:case=="cancel"};
    let config=Gpu::Config{backend:Some(Arc::new(backend)),fast:true,policy:Gpu::Policy::Qualification,readback_per_float:None,..Gpu::default()};
    let lowered=lower::workspace(None,vec![],lower::Options{defer:true,gpu:config.clone(),..Default::default()},&syntax::parse(&source(host_call,case)).unwrap()).unwrap_or_else(|d|panic!("{}",rays_diagnostic::to_string(&d)));
    let graph=&lowered.graphs[0];let resolved=value_lane::resolve(Some(&config),None,&value_lane::create(None),0.,&graph.network).unwrap();let root=edit::compile_node(&resolved.geometry,graph.root.unwrap()).unwrap();if host_call{assert_eq!(node::operation(&root),"flow.geometry_select");}
    let mut session=session::create(4,1<<24).unwrap();let mut held=vec![];let mut replays=0;
    let counts:&[i64]=if matches!(case,"dynamic"|"overflow"){&[0,342,683,1025,0,342]}else{&[342]};
    for &count in counts{
        session::clear(&mut session);let before=h::stats(&host);let closed=closes.load(Ordering::SeqCst);let context=ctx::create(ctx::Options{cancel:Some(cancel.clone()),domains:Some(1),time:Some(0.),frame:Some(count),..Default::default()}).unwrap();let result=session::cook(&mut session,None,&context,&root);
        let result=if case=="cancel"{assert!(cancel::is_cancelled(&cancel));let d=result.err().expect("cancelled resident cook published");assert!(d.code=="E_CANCELLED"||d.code=="cancelled");assert!(session::cached(&session,&context,&root).is_none());assert_eq!((h::stats(&host).payload_readback_bytes,h::stats(&host).validation_readback_bytes),(0,0));let retry=ctx::create(ctx::Options{domains:Some(1),time:Some(0.),frame:Some(count),..Default::default()}).unwrap();session::cook(&mut session,None,&retry,&root)}else{result};
        if bad{
            let d=result.err().expect("malformed GPU mesh published");let message=match case{"vertex"|"negative"=>"Topology: vertex references an invalid point","missing"=>"Topology: primitive offsets must contain the initial zero","start"=>"Topology: first primitive offset must be zero","terminal"=>"Topology: final primitive offset must equal vertex count","order"=>"Topology: primitive offsets are not ordered","minimum"=>"Topology: primitive has too few vertices for its kind","length"=>"Topology: primitive kind count must match primitive count","kind"=>"Primitive kinds are 0 (polygon), 1 (open polyline) or 2 (closed polyline).",_=>unreachable!()};
            assert_eq!((d.code.as_str(),d.message.as_str()),(if case=="kind"{"E_TOPOLOGY_KIND"}else{"E_TOPOLOGY"},message));assert!(session::cached(&session,&context,&root).is_none());let stats=h::stats(&host);assert_eq!((stats.payload_readback_bytes,stats.validation_readback_bytes),(0,if case=="negative"{0}else{36}));
        }else{
            let output=result.unwrap_or_else(|d|panic!("{test}/{count}: {}",rays_diagnostic::error_to_string(&d)));let geometry=payload::geometry(&output.payload).unwrap();replays+=(closes.load(Ordering::SeqCst)-closed).saturating_sub(1);let stats=h::stats(&host);
            assert_eq!(stats.payload_readback_bytes-before.payload_readback_bytes,if case=="cpu"{19156}else{0},"{test} publication payload");let validation=stats.validation_readback_bytes-before.validation_readback_bytes;let expected=if count==0{0}else if case=="twice"{72}else{36};assert!(validation==expected||(count==0&&validation==36),"{test}/{count} validation={validation}, expected={expected}");
            assert_eq!((g::point_count(&geometry),g::primitive_count(&geometry)),(3*count,if case=="points"{0}else{count}));
            if count>0&&case!="cpu"{assert!(topology::resident(g::topology(&geometry)));assert!(Float3::resident(g::positions(&geometry)));let(a,b,c)=topology_kernel::resident(&geometry).expect("resident streams");for stream in[a,b,c]{if stream.count>0{let(_,n,valid,unpin)=run::pin_integers(&stream).unwrap();assert_eq!(n,stream.count);assert!(valid());unpin();assert!(!valid());}}assert_eq!(h::stats(&host).payload_readback_bytes,stats.payload_readback_bytes);}
            held.push((geometry,count));
        }
    }
    if case=="overflow"{assert!(replays>0,"counted mesh never replayed capacity");}
    parallel::run_unit(Some(4),&||parallel::for_(1,0,7,&|_|{for(geometry,count)in &held{read(geometry,*count,case)}Ok(())})).unwrap();let bytes=h::stats(&host).payload_readback_bytes;for(geometry,count)in &held{read(geometry,*count,case)}assert_eq!(h::stats(&host).payload_readback_bytes,bytes);
    drop(held);session::close(&mut session);drop(root);drop(resolved);drop(lowered);drop(config);h::close(&host);drop(host);drop(gpu);common::release_keepalive();assert_eq!(ogpu::impl_::live_handles().unwrap(),baseline);
}
macro_rules! scenario{($name:ident,$case:literal)=>{fn $name(){run_case($case,false)}}}
fn valid_01(){run_case("valid",true)}
scenario!(valid_02,"valid");scenario!(valid_03,"valid");scenario!(valid_04,"valid");scenario!(valid_05,"valid");scenario!(valid_06,"valid");scenario!(valid_07,"valid");scenario!(valid_08,"valid");scenario!(valid_09,"valid");scenario!(valid_10,"valid");scenario!(valid_11,"valid");scenario!(valid_12,"valid");scenario!(valid_13,"valid");scenario!(valid_14,"valid");scenario!(valid_15,"valid");scenario!(valid_16,"valid");scenario!(valid_17,"valid");scenario!(valid_18,"valid");scenario!(valid_19,"valid");scenario!(valid_20,"valid");
scenario!(vertex,"vertex");scenario!(negative,"negative");scenario!(missing,"missing");scenario!(start,"start");scenario!(terminal,"terminal");scenario!(order,"order");scenario!(minimum,"minimum");scenario!(length,"length");scenario!(kind,"kind");scenario!(dynamic,"dynamic");scenario!(cpu,"cpu");scenario!(cancel,"cancel");scenario!(host,"host");scenario!(host_twice,"host_twice");scenario!(host_cpu,"host_cpu");scenario!(host_cancel,"host_cancel");scenario!(host_vertex,"host_vertex");scenario!(host_points,"host_points");scenario!(host_negative,"host_negative");scenario!(host_dynamic,"host_dynamic");scenario!(overflow,"overflow");scenario!(host_overflow,"host_overflow");
fn static_topology_stays_resident() {
    assert!(common::gpu());
    let gpu = common::lease();
    let host = h::create(&|| 0., &gpu);
    let config = Gpu::Config { backend: Some(h::backend(&host).into()), fast: true, policy: Gpu::Policy::Qualification, ..Gpu::default() };
    let text = "(workspace static_mesh (graph g :context sop (sop/mesh :points (exact (map (fn [(i : int)] [(+ (float i) t) 0. 0.]) (array/range 15000 :type int))) :vertex_points (exact (array/range 15000 :type int)) :primitive_offsets (exact (map (fn [(i : int)] (int/mul i 3)) (array/range 5001 :type int))) :kinds (array/int 5000))))";
    let lowered = lower::workspace(None, Vec::new(), lower::Options { gpu: config, defer: true, ..Default::default() }, &syntax::parse(text).unwrap()).unwrap();
    let graph = &lowered.graphs[0];
    let root = edit::compile_node(&graph.network.geometry, graph.root.unwrap()).unwrap();
    let mut session = session::create(0, 0).unwrap();
    let mut previous = None;
    for frame in 0..4 {
        let context = ctx::create(ctx::Options { time: Some(frame as f64), domains: Some(1), ..Default::default() }).unwrap();
        let out = session::cook(&mut session, Some(0), &context, &root).unwrap();
        let geometry = payload::geometry(&out.payload).unwrap();
        assert!(Float3::resident(g::positions(&geometry)));
        assert!(topology_kernel::resident(&geometry).is_some());
        if let Some(old) = &previous {
            assert_eq!(topology::data_id(g::topology(old)), topology::data_id(g::topology(&geometry)), "animated points rebuilt immutable topology");
        }
        assert_eq!(h::stats(&host).payload_readback_bytes, 0);
        assert_eq!(h::stats(&host).input_uploaded_bytes, 0);
        previous = Some(geometry);
    }
    drop(previous);
    session::close(&mut session);
    h::close(&host);
}
fn main(){common::window::run(&[("static_topology_stays_resident",static_topology_stays_resident),("valid_01",valid_01),("valid_02",valid_02),("valid_03",valid_03),("valid_04",valid_04),("valid_05",valid_05),("valid_06",valid_06),("valid_07",valid_07),("valid_08",valid_08),("valid_09",valid_09),("valid_10",valid_10),("valid_11",valid_11),("valid_12",valid_12),("valid_13",valid_13),("valid_14",valid_14),("valid_15",valid_15),("valid_16",valid_16),("valid_17",valid_17),("valid_18",valid_18),("valid_19",valid_19),("valid_20",valid_20),("vertex",vertex),("negative",negative),("missing",missing),("start",start),("terminal",terminal),("order",order),("minimum",minimum),("length",length),("kind",kind),("dynamic",dynamic),("cpu",cpu),("cancel",cancel),("host",host),("host_twice",host_twice),("host_cpu",host_cpu),("host_cancel",host_cancel),("host_vertex",host_vertex),("host_points",host_points),("host_negative",host_negative),("host_dynamic",host_dynamic),("overflow",overflow),("host_overflow",host_overflow)]);}
