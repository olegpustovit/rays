//! lib/flow_sop/test_mesh_resident.ml: all 42 attempts, exact topology/readback/ownership oracles.
//! Arc-backed placeholders are immutable; exact publication/readback checks detect accidental host reads.
mod common;
use flow_gpu::{host as h,run};
use flow_ir::{flow_ir::Gpu,core_exec};
use flow_sop::{lower,value_lane,topology_kernel,topology_kernel_impl};
use flow_syntax::{syntax,ty::Ty,value::Value};
use rdk::{geometry as g,topology,packed::Float3};
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
fn run_case(test:&str,empty_offset:bool){
    assert!(common::gpu(),"native SOP mesh qualification requires a real GPU");common::release_keepalive();let baseline=ogpu::impl_::live_handles().unwrap();
    assert!(common::gpu());let gpu=common::lease();let host=h::create(&||0.,&gpu);
    let host_call=test.starts_with("host");let case=match test{"host"=>"valid","host_twice"=>"twice","host_cpu"=>"cpu","host_cancel"=>"cancel","host_negative"=>"negative","host_dynamic"=>"dynamic","host_overflow"=>"overflow","host_vertex"=>"vertex","host_points"=>"points",c=>c};
    let bad=!matches!(case,"valid"|"dynamic"|"cpu"|"cancel"|"twice"|"points"|"overflow");
    if empty_offset{
        let backend=h::backend(&host);let scope=(backend.residency)(true).expect("native residency");let offset=(scope.executor.fill)(&Ty::Array(Box::new(Ty::Int)),1,&Value::Int(1)).unwrap().unwrap();
        let args=vec![("points".into(),Value::Vec3_array(Arc::new(vec![]))),("vertex_points".into(),Value::Int_array(Arc::new(vec![]))),("primitive_offsets".into(),offset),("kinds".into(),Value::Int_array(Arc::new(vec![])))];
        let d=topology_kernel_impl::mesh_resident(None,&scope,&args).expect("resident path").err().expect("malformed empty offset accepted");assert_eq!((d.code.as_str(),d.message.as_str()),("E_TOPOLOGY","Topology: first primitive offset must be zero"));(scope.executor.close)();assert_eq!((h::stats(&host).payload_readback_bytes,h::stats(&host).validation_readback_bytes),(0,36));
    }
    let cancel=Arc::new(rdk::cancel::create());let closes=Arc::new(AtomicUsize::new(0));let backend=h::backend(&host);let residency=backend.residency;let cancel_wrap=cancel.clone();let closes_wrap=closes.clone();let cancel_case=case=="cancel";
    let backend=Gpu::Backend{residency:Box::new(move|fast|residency(fast).map(|scope|{
        // Retained-stream callbacks share this executor; observe it by delegation.
        let dispatched=Arc::new(AtomicBool::new(false));let base=scope.executor;let invoke=base.clone();let settle=base.clone();let count=base.clone();let close=base.clone();
        let inv=dispatched.clone();let set=dispatched.clone();let cnt=dispatched.clone();let cancel_set=cancel_wrap.clone();let cancel_count=cancel_wrap.clone();let closes=closes_wrap.clone();
        let executor=core_exec::Resident{
            invoke:Box::new(move|p,args,locals,live|{let result=(invoke.invoke)(p,args,locals,live)?;if result.is_some(){inv.store(true,Ordering::SeqCst);}Ok(result)}),
            settle:Box::new(move||{(settle.settle)()?;if cancel_case&&set.load(Ordering::SeqCst){rdk::cancel::cancel(&cancel_set);}Ok(())}),
            count:Box::new(move|v|{let n=(count.count)(v)?;if cancel_case&&cnt.load(Ordering::SeqCst)&&n.is_some(){rdk::cancel::cancel(&cancel_count);}Ok(n)}),
            close:Box::new(move||{if dispatched.load(Ordering::SeqCst){closes.fetch_add(1,Ordering::SeqCst);}(close.close)()}),
            fill:Box::new({let b=base.clone();move|ty,n,v|(b.fill)(ty,n,v)}),
            unfold:Box::new({let b=base.clone();move|p,args,n,v,locals,live|(b.unfold)(p,args,n,v,locals,live)}),
            compact:Box::new({let b=base.clone();move|v|(b.compact)(v)}),
            coerce:Box::new({let b=base.clone();move|ty,v|(b.coerce)(ty,v)}),
            concat:Box::new({let b=base.clone();move|a,c|(b.concat)(a,c)}),
            finish:Box::new({let b=base.clone();move|v|(b.finish)(v)}),
            keep:Box::new({let b=base.clone();move|v|(b.keep)(v)}),
            capacity:Box::new({let b=base.clone();move|v|(b.capacity)(v)}),
            range:Box::new(move|ty,v,div,n|(base.range)(ty,v,div,n))};
        Gpu::Residency{executor:Arc::new(executor),..scope}
    })),..backend};
    let config=Gpu::Config{backend:Some(Arc::new(backend)),fast:true,policy:Gpu::Policy::Qualification,readback_per_float:None};
    let lowered=lower::workspace(None,vec![],lower::Options{defer:true,gpu:config.clone(),..Default::default()},&syntax::parse(&source(host_call,case)).unwrap()).unwrap_or_else(|d|panic!("{}",rays_diagnostic::to_string(&d)));
    let graph=&lowered.graphs[0];let resolved=value_lane::resolve(Some(&config),None,&value_lane::create(None),0.,&graph.network).unwrap();let root=edit::compile_node(&resolved.geometry,graph.root.unwrap()).unwrap();if host_call{assert_eq!(node::operation(&root),"flow.geometry_select");}
    let mut session=session::create(4,1<<24).unwrap();let mut held=vec![];let mut replays=0;
    let counts:&[i64]=if matches!(case,"dynamic"|"overflow"){&[0,342,683,1025,0,342]}else{&[342]};
    for &count in counts{
        session::clear(&mut session);let before=h::stats(&host);let closed=closes.load(Ordering::SeqCst);let context=ctx::create(ctx::Options{cancel:Some(cancel.clone()),domains:Some(1),time:Some(0.),frame:Some(count),..Default::default()}).unwrap();let result=session::cook(&mut session,None,&context,&root);
        let result=if case=="cancel"{assert!(rdk::cancel::is_cancelled(&cancel));let d=result.err().expect("cancelled resident cook published");assert!(d.code=="E_CANCELLED"||d.code=="cancelled");assert!(session::cached(&session,&context,&root).is_none());assert_eq!((h::stats(&host).payload_readback_bytes,h::stats(&host).validation_readback_bytes),(0,0));let retry=ctx::create(ctx::Options{domains:Some(1),time:Some(0.),frame:Some(count),..Default::default()}).unwrap();session::cook(&mut session,None,&retry,&root)}else{result};
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
fn main(){common::window::run(&[("valid_01",valid_01),("valid_02",valid_02),("valid_03",valid_03),("valid_04",valid_04),("valid_05",valid_05),("valid_06",valid_06),("valid_07",valid_07),("valid_08",valid_08),("valid_09",valid_09),("valid_10",valid_10),("valid_11",valid_11),("valid_12",valid_12),("valid_13",valid_13),("valid_14",valid_14),("valid_15",valid_15),("valid_16",valid_16),("valid_17",valid_17),("valid_18",valid_18),("valid_19",valid_19),("valid_20",valid_20),("vertex",vertex),("negative",negative),("missing",missing),("start",start),("terminal",terminal),("order",order),("minimum",minimum),("length",length),("kind",kind),("dynamic",dynamic),("cpu",cpu),("cancel",cancel),("host",host),("host_twice",host_twice),("host_cpu",host_cpu),("host_cancel",host_cancel),("host_vertex",host_vertex),("host_points",host_points),("host_negative",host_negative),("host_dynamic",host_dynamic),("overflow",overflow),("host_overflow",host_overflow)]);}
