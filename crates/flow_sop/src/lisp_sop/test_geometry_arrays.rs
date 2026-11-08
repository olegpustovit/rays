//! Assertion map: lib/flow_sop/test_geometry_arrays.ml C1/C4 (both public mesh representations).
use super::support::*;
use crate::{attribute_kernel as ak, catalog, lisp_sop, lower, operators, value_lane};
use flow_ir::flow_ir::Executor;
use flow_syntax::{ty::{self, Ty}, value::{Value, source_value}};
use rdk_core::{attribute::{self, Owner, Storage}, geometry as geo, packed::Float3, topology};
use sop::{context as ctx, edit_graph as edit, node, session};
use std::sync::Arc;

fn ints(xs: Vec<i64>) -> Value { Value::Int_array(Arc::new(xs)) }
fn floats(xs: Vec<f64>) -> Value { Value::Float_array(Arc::new(xs)) }
fn fixture(which: usize) -> geo::T {
    use topology::PrimitiveKind::{Polygon as P, OpenPolyline as O, ClosedPolyline as C};
    let (n, vp, po, kinds) = match which {
        0 => (0, vec![], vec![0], vec![]),
        1 => (5, vec![0,1,2,2,1,3], vec![0,3,6], vec![P,P]),
        2 => (3, vec![0,1,1,2,1], vec![0,5], vec![O]),
        3 => (4, vec![3,2,1,0], vec![0,4], vec![C]),
        4 => (6, vec![0,1,2,0,1,3,1,0,4,0,1,5], vec![0,3,6,9,12], vec![P,O,C,P]),
        _ => panic!("fixture index"),
    };
    geo::create(Float3::of_owned((0..n).map(|i|i as f64).collect(),(0..n).map(|i|if i%2==0 {-0.0} else {0.25}).collect(),vec![2.0;n]).unwrap(),topology::create_owned(n as i64,vp,po,kinds).unwrap(),None,None,None).unwrap()
}
fn fields(g: &geo::T, planes: bool) -> Vec<(String, Value)> {
    let t = topology::View::view(geo::topology(g)).unwrap();
    let p = Float3::View::view(geo::positions(g)).unwrap();
    let points = if planes { Value::Record(vec![("x".into(),floats(p.x.to_vec())),("y".into(),floats(p.y.to_vec())),("z".into(),floats(p.z.to_vec()))]) }
        else { Value::Vec3_array(Arc::new((0..p.x.len()).flat_map(|i|[p.x[i],p.y[i],p.z[i]]).collect())) };
    vec![("points".into(),points),("vertex_points".into(),ints(t.vertex_points.to_vec())),("primitive_offsets".into(),ints(t.primitive_offsets.to_vec())),("kinds".into(),ints(t.primitive_kinds.iter().map(|i|*i as i64).collect()))]
}
fn source_arrays(planes: bool) -> String {
    let points = if planes { "{:x (array float) :y (array float) :z (array float)} {:x (array/float 0) :y (array/float 0) :z (array/float 0)}" } else {"(array vec3) (array/vec3 0)"};
    let op = if planes {"sop/mesh_planes"} else {"sop/mesh"};
    format!("(workspace arrays (graph g :context sop [(points : {points}) (vertex_points : (array int) (array/int 0)) (primitive_offsets : (array int) (array/int 1)) (kinds : (array int) (array/int 0))] ({op} :points points :vertex_points vertex_points :primitive_offsets primitive_offsets :kinds kinds)))")
}
fn compile(l: &lower::T) -> node::T { let g=&l.graphs[0]; edit::compile_node(&g.network.geometry,g.root.unwrap()).unwrap() }
fn arrays(reference: bool, planes: bool, values: Vec<(String,Value)>) -> node::T {
    let opts=lower::Options {reference,inputs:Some(vec![("g".into(),values)]),..Default::default()};
    compile(&lower::workspace(None,vec![],opts,&parse(&source_arrays(planes))).unwrap_or_else(|d|panic!("{}",rays_diagnostic::to_string(&d))))
}
fn authored(reference: bool, input: &geo::T, source: &str) -> node::T {
    let snapshot=lisp_sop::snapshot(input);
    let mut factories=vec![edit::factory(edit::factory_args("ext_src","Source", &["Test"]),0,Arc::new(move |_|Ok(snapshot.clone())))];
    factories.extend(sop::nodes::factories());
    compile(&lower::workspace(Some(factories),vec![],lower::Options {reference,..Default::default()},&parse(source)).unwrap_or_else(|d|panic!("{}",rays_diagnostic::to_string(&d))))
}
fn rebuild(planes:bool) -> &'static str {
    if planes {"(let* [topology (sop/topology geo) positions (sop/attr geo :P)] (sop/mesh_planes :points {:x (exact (map (fn [(p : vec3)] p.x) positions)) :y (exact (map (fn [(p : vec3)] p.y) positions)) :z (exact (map (fn [(p : vec3)] p.z) positions))} :vertex_points topology.vertex_points :primitive_offsets topology.primitive_offsets :kinds topology.kinds))"}
    else {"(let* [topology (sop/topology geo)] (sop/mesh :points (sop/attr geo :P) :vertex_points topology.vertex_points :primitive_offsets topology.primitive_offsets :kinds topology.kinds))"}
}
fn wrapped(body:&str) -> String { format!("(workspace authored (defnode sop/rebuild [(geo : geometry)] {body}) (graph g :context sop (sop/rebuild (sop/ext_src))))") }
fn round_trip(which: usize, planes:bool) {
    let expected=fixture(which); let before=geometry_bytes(&expected);
    for reference in [false,true] {
        let mut input=fields(&expected,planes);
        let n=arrays(reference,planes,input.clone()); let rebuilt=authored(reference,&expected,&wrapped(rebuild(planes)));
        for domains in [1,4,8] { for grain in [1,97,4096] {
            let mut s=new_session(64,64_000_000); let c=context(domains,grain);
            assert_eq!(geometry_bytes(&cook(&mut s,&c,&n)),before);
            let misses=session::stats(&s).misses;
            cook(&mut s,&c,&n); assert_eq!(session::stats(&s).misses,misses,"unchanged mesh missed");
            assert_eq!(geometry_bytes(&cook(&mut s,&c,&rebuilt)),before);
            session::close(&mut s);
        }}
        let actual=cook_once(1,&n);
        fn mutate(v:&mut Value) { match v {Value::Record(fs)=>for (_,v) in fs {mutate(v)},Value::Int_array(xs)=>if !xs.is_empty(){Arc::make_mut(xs)[0]=99},Value::Float_array(xs)|Value::Vec3_array(xs)=>if !xs.is_empty(){Arc::make_mut(xs)[0]=99.0},_=>()}}
        for (_,v) in &mut input {mutate(v)}
        assert_eq!(geometry_bytes(&actual),before,"mesh borrowed mutable Flow input");
    }
}
macro_rules! fixture_test {($name:ident,$n:expr,$planes:expr)=>{#[test] fn $name(){round_trip($n,$planes)}}}
fixture_test!(arrays_empty,0,false); fixture_test!(arrays_shared_polygons,1,false);
fixture_test!(arrays_open_repeated,2,false); fixture_test!(arrays_closed,3,false); fixture_test!(arrays_mixed,4,false);
fixture_test!(planes_empty,0,true); fixture_test!(planes_shared_polygons,1,true);
fixture_test!(planes_open_repeated,2,true); fixture_test!(planes_closed,3,true); fixture_test!(planes_mixed,4,true);
fn large(planes:bool) {
    let n=if planes {65537}else{32769};
    let original=rdk_ops::line_geometry::points(&(0..n).map(|i|(i as f64,if i%2==0{-0.0}else{f64::from_bits(1)},-2.0)).collect::<Vec<_>>()).unwrap();
    let before=geometry_bytes(&original);
    let body=if planes {rebuild(true).to_owned()} else {rebuild(false).replace("(sop/attr geo :P)","(exact (map (fn [(position : vec3)] position) (sop/attr geo :P)))")};
    for reference in [false,true] {let root=authored(reference,&original,&wrapped(&body));
        let direct=if planes {Some(arrays(reference,true,fields(&original,true)))}else{None};
        for domains in [1,4,8] {for grain in [1,97,4096] {
            let mut s=new_session(64,64_000_000);let c=context(domains,grain);
            assert_eq!(geometry_bytes(&cook(&mut s,&c,&root)),before);
            if let Some(direct)=&direct {assert_eq!(geometry_bytes(&cook(&mut s,&c,direct)),before);let misses=session::stats(&s).misses;cook(&mut s,&c,direct);assert_eq!(session::stats(&s).misses,misses);}
            session::close(&mut s);
        }}
    }
}
#[test] fn arrays_multiblock_signed_zero_subnormal(){large(false)}
#[test] fn planes_multiblock_signed_zero_subnormal(){large(true)}
fn captured(planes:bool) {
    let original=fixture(1);
    let body=if planes {r#"(let* [topology (topology_of geo) p (sop/attr geo :P)] (sop/mesh_planes :points {:x (exact (map (fn [(v : vec3)] (let* [mapped (field v)] mapped.x)) p)) :y (exact (map (fn [(v : vec3)] (let* [mapped (field v)] mapped.y)) p)) :z (exact (map (fn [(v : vec3)] (let* [mapped (field v)] mapped.z)) p))} :vertex_points topology.vertex_points :primitive_offsets topology.primitive_offsets :kinds topology.kinds))"#}
        else {"(let* [topology (topology_of geo)] (sop/mesh :points (exact (map field (sop/attr geo :P))) :vertex_points topology.vertex_points :primitive_offsets topology.primitive_offsets :kinds topology.kinds))"};
    let source=format!("(workspace capture (defn topology_of :context sop [(geo : geometry)] (sop/topology geo)) (defnode sop/capture [(geo : geometry) (field : (fn [vec3] vec3))] {body}) (graph g :context sop (sop/capture (sop/ext_src) (fn [(p : vec3)] p))))");
    for reference in [false,true] {let n=authored(reference,&original,&source);for domains in [1,8] {assert_eq!(geometry_bytes(&cook_once(domains,&n)),geometry_bytes(&original));}}
}
#[test] fn arrays_typed_helper_function_capture(){captured(false)}
#[test] fn planes_typed_helper_function_capture(){captured(true)}

fn evaluate(body:&str) -> Value {
    let forms=parse(&format!("(workspace query (graph g :context sop [(geo : geometry nil)] (let* [tested {body}] geo)))"));
    let cat=catalog::of_factories(1,&[],None,&sop::nodes::factories()).unwrap();
    let (checked,ds)=catalog::check_workspace(operators::all(),true,None,&cat,&forms);
    assert!(ds.is_empty(),"{ds:?}");
    let ev=flow_eval::static_with(&checked.unwrap(),true,Some(&vec![("g".into(),vec![("geo".into(),Value::Deferred(ty::geometry(),7))])]),None).unwrap();
    ev.records.iter().find(|(p,_)|p==&vec!["g".to_owned(),"tested".to_owned()]).unwrap().1[0].1.clone()
}
fn lookup(g:&geo::T) -> ak::GeometryLookup {let g=g.clone();Arc::new(move |id|if id==7{Some(g.clone())}else{None})}
fn force(g:&geo::T,v:&Value) -> Value {let lookup=lookup(g);flow_eval::Host::force_reference(v,&input::at_time(0.0),None,None,Some(Arc::new(move |v|ak::resolve(None,&lookup,v)))).unwrap()}
#[test]
fn topology_queries_keep_planes_origins_and_immutable_readers() {
    let original=fixture(1);let before=geometry_bytes(&original);let queried=evaluate("(sop/topology geo)");
    let expected=Value::Record(fields(&original,false).into_iter().skip(1).collect());assert_eq!(force(&original,&queried),expected);
    let native=topology::View::view(geo::topology(&original)).unwrap();
    let Value::Record(queried_fields)=force(&original,&queried) else {panic!("topology record expected")};
    for (name,plane) in [("vertex_points",&native.vertex_points),("primitive_offsets",&native.primitive_offsets)] {
        let Value::Int_array(values)=&queried_fields.iter().find(|(key,_)|key==name).unwrap().1 else {panic!("topology integer plane expected")};
        assert!(Arc::ptr_eq(values,plane),"topology query copied native {name} storage");
    }
    for read in ["(map (fn [(i : int)] (int/add i 1)) topo.vertex_points)","(array/grade topo.vertex_points)","(array/scan_sum topo.vertex_points)","(array/slice topo.primitive_offsets 0 1)","(array/concat topo.vertex_points topo.primitive_offsets)","(array/compact (map (fn [(i : int)] (if (= i 0) 1 0)) topo.vertex_points))","(array/segment_sum topo.vertex_points topo.primitive_offsets)","(array/scatter_add topo.vertex_points topo.vertex_points 64)","(array/sum topo.vertex_points)"] {
        force(&original,&evaluate(&format!("(let* [topo (sop/topology geo)] {read})")));assert_eq!(geometry_bytes(&original),before,"{read}");
    }
    let source_node = lisp_sop::snapshot(&original);
    let mut origins=vec![];
    for field in ["vertex_points","primitive_offsets","kinds"] {
        let mut pair=vec![];
        for expression in [format!("(let* [topology (sop/topology geo)] (map (fn [(i : int)] i) topology.{field}))"),format!("(map (fn [(i : int)] i) (get (sop/topology geo) :{field}))")] {
            let value=evaluate(&expression);assert_eq!(ak::sources(&value),vec![7]);
            let program=ak::prepare(None,true,None,None,&[7],std::slice::from_ref(&source_node),&value).unwrap();
            pair.push(flow_ir::packed::count_origin(Executor::packed_region(&program).as_ref().unwrap()).expect("topology count origin"));
        }
        assert_eq!(pair[0],pair[1]);origins.push(pair[0].clone());
    }
    assert!(origins[0]!=origins[1]&&origins[0]!=origins[2]&&origins[1]!=origins[2]);
    let p=Executor::compile(&queried,&Executor::CompileOptions{strict:true,..Default::default()}).unwrap();
    for domains in [1,8] {parallel::run_unit(Some(domains),&||{let lookup=lookup(&original);let opts=Executor::ForceOptions{strict:true,resolve:Some(Arc::new(move|v|ak::resolve(None,&lookup,v))),..Default::default()};assert_eq!(Executor::force(&p,&opts,&input::at_time(0.0)).unwrap(),expected);Ok(())}).unwrap();}
}

#[test]
fn topology_host_source_really_refuses_web_export() {
    let Value::Residual(r)=evaluate("(if (= (frame/index) 0) (get (sop/topology geo) :kinds) (array/int 0))") else {panic!("dynamic topology source was folded")};
    let core=flow_ir::core_lower::lower(None,None,true,None,None,r).unwrap();
    assert_eq!(flow_web::js::frame(&Default::default(),&core).unwrap_err().code,"E_WEB_SOURCE");
}
fn replace(mut xs:Vec<(String,Value)>,key:&str,value:Value)->Vec<(String,Value)>{xs.iter_mut().find(|(n,_)|n==key).unwrap().1=value;xs}
fn errors(planes:bool) {
    let original=fixture(1);let base=fields(&original,planes);
    let cases=vec![
        ("primitive_offsets",ints(vec![]),"Topology: primitive offsets must contain the initial zero"),
        ("primitive_offsets",ints(vec![1,3,6]),"Topology: first primitive offset must be zero"),
        ("primitive_offsets",ints(vec![0,3,5]),"Topology: final primitive offset must equal vertex count"),
        ("kinds",ints(vec![0]),"Topology: primitive kind count must match primitive count"),
        ("vertex_points",ints(vec![0,1,2,2,1,9007199254740993]),"Topology: vertex references an invalid point"),
        ("primitive_offsets",ints(vec![0,7,6]),"Topology: primitive offsets are not ordered"),
        ("primitive_offsets",ints(vec![0,2,6]),"Topology: primitive has too few vertices for its kind"),
    ];
    for reference in [false,true] {
        for (key,value,message) in &cases {
            let n=arrays(reference,planes,replace(base.clone(),key,value.clone()));
            for domains in [1,8] {let mut s=new_session(64,64_000_000);let d=try_cook(&mut s,&context(domains,97),&n).err().expect("malformed topology succeeded");assert_eq!((d.code.as_str(),d.message.as_str(),d.cause.as_ref()),("E_TOPOLOGY",*message,None));session::close(&mut s);}
        }
        for kind in [-1,3,9007199254740993] {let n=arrays(reference,planes,replace(base.clone(),"kinds",ints(vec![kind,0])));let mut s=new_session(64,64_000_000);assert_eq!(try_cook(&mut s,&context(1,97),&n).err().unwrap().code,"E_TOPOLOGY_KIND");session::close(&mut s);}
    }
    for domains in [1,8] {
        let n=arrays(false,planes,base.clone());let cancel=Arc::new(rdk_core::cancel::create());rdk_core::cancel::cancel(&cancel);
        let c=ctx::create(ctx::Options{domains:Some(domains),cancel:Some(cancel),..Default::default()}).unwrap();let mut s=new_session(64,64_000_000);
        let d=try_cook(&mut s,&c,&n).err().expect("cancelled mesh succeeded");assert!(d.code=="cancelled"||d.code=="E_CANCELLED");session::close(&mut s);
    }
}
#[test]fn arrays_native_topology_errors_exact_ints_cancellation(){errors(false)}
#[test]fn planes_native_topology_errors_exact_ints_cancellation(){errors(true)}
#[test]
fn unavailable_topology_sources_refuse(){
    let lookup:ak::GeometryLookup=Arc::new(|_|None);
    for g in [Value::No_geo,Value::Deferred(ty::geometry(),7),Value::Deferred(Ty::Float,7)] {
        let source=source_value("sop/topology", Ty::Array(Box::new(Ty::Int)), vec![("geometry".into(),g),("component".into(),Value::Text(b"vertex_points".to_vec()))]);
        assert_eq!(ak::resolve(None,&lookup,&source).unwrap_err().code,"E_DATA_SOURCE");
    }
}
fn lazy(planes:bool){
    let original=fixture(1);
    let bad=if planes {r#"(sop/mesh_planes :points {:x (error "E_X" "selected x" :cause "x cause") :y (array/float 0) :z (array/float 0)} :vertex_points (error "E_VP" "later vertices") :primitive_offsets (array/int 1) :kinds (array/int 0))"#}
        else {r#"(sop/mesh :points (error "E_POINTS" "selected points" :cause "point cause") :vertex_points (array/int 0) :primitive_offsets (array/int 0) :kinds (array/int 0))"#};
    for reference in [false,true] {
        let untaken=if planes {r#"(sop/mesh_planes :points (error "E_UNTAKEN" "untaken") :vertex_points (array/int 0) :primitive_offsets (array/int 0) :kinds (array/int 0))"#}
            else {r#"(sop/mesh :points (sop/attr geo "missing") :vertex_points (array/int 0) :primitive_offsets (array/int 0) :kinds (array/int 0))"#};
        let n=authored(reference,&original,&wrapped(&format!("(if false {untaken} {})",rebuild(planes))));
        assert_eq!(geometry_bytes(&cook_once(1,&n)),geometry_bytes(&original));
        let n=authored(reference,&original,&wrapped(bad));let mut s=new_session(64,64_000_000);let d=try_cook(&mut s,&context(1,97),&n).err().expect("selected error succeeded");
        let expected=if planes {("E_X","selected x","x cause")}else{("E_POINTS","selected points","point cause")};
        assert_eq!((d.code.as_str(),d.message.as_str(),d.cause.as_deref()),(expected.0,expected.1,Some(expected.2.as_bytes())));session::close(&mut s);
    }
}
#[test]fn arrays_lazy_branches_preserve_selected_causes(){lazy(false)}
#[test]fn planes_lazy_branches_preserve_selected_causes(){lazy(true)}
#[test]
fn plane_input_finite_count_and_scalar_capacity_boundaries(){
    let original=fixture(1);let base=fields(&original,true);
    for reference in [false,true] {
        for domains in [1,4,8] {let values=replace(base.clone(),"points",Value::Record(vec![("x".into(),floats(vec![])),("y".into(),floats(vec![0.])),("z".into(),floats(vec![]))]));let n=arrays(reference,true,values);let mut s=new_session(64,64_000_000);assert_eq!(try_cook(&mut s,&context(domains,97),&n).err().unwrap().code,"E_ARRAY_COUNT");session::close(&mut s);}
        for axis in ["x","y","z"] {for bad in [f64::NAN,f64::INFINITY,f64::NEG_INFINITY] {
            let mut values=base.clone();let Value::Record(p)=&mut values[0].1 else{unreachable!()};let Value::Float_array(xs)=&mut p.iter_mut().find(|(n,_)|n==axis).unwrap().1 else{unreachable!()};Arc::make_mut(xs)[0]=bad;
            let result=lower::workspace(None,vec![],lower::Options{reference,inputs:Some(vec![("g".into(),values)]),..Default::default()},&parse(&source_arrays(true)));
            assert_eq!(result.err().expect("nonfinite plane input accepted").code,"E_NONFINITE");
        }}
    }
    let max=flow_check::op::MAX_WOSIZE;let n=max/3+1;assert!(n<=max&&n*3>max);
    assert_eq!(flow_check::op::array_count(None,"array/float",&Value::Int(max as i64)).unwrap(),max);
    assert_eq!(flow_check::op::array_count(None,"array/float",&Value::Int(max as i64+1)).unwrap_err().code,"E_ARRAY_RANGE");
}
fn live_arrays(planes:bool,stateful:bool){
    let coordinate=if stateful{"(state [previous 0.0] (+ previous (frame/dt)))"}else{"t"};
    let points=if planes {"{:x (exact (map (fn [(i : int)] coordinate) (array/range 1 :type int))) :y (array/float 1) :z (array/float 1)}"}else{"(exact (map (fn [(i : int)] [coordinate 0.0 0.0]) (array/range 1 :type int)))"};
    let op=if planes{"sop/mesh_planes"}else{"sop/mesh"};
    let source=format!("(workspace animated (graph g :context sop (let* [coordinate {coordinate}] ({op} :points {points} :vertex_points (array/int 0) :primitive_offsets (array/int 1) :kinds (array/int 0)))))");
    for reference in [false,true] {let (l,_)=lower_text(lower::Options{reference,..Default::default()},&source);let graph=&l.graphs[0];let state=flow_eval::create_state();let lane=value_lane::create(Some(state.clone()));let mut s=new_session(64,64_000_000);
        for (time,dt) in [(0.,0.25),(1.,0.5),(2.,0.125)] {let live=input::Frame{dt,frame:time as i64,tick:time as i64,..input::at_time(time)};let resolved=value_lane::resolve(None,Some(&live),&lane,time,&graph.network).unwrap();let n=edit::compile_node(&resolved.geometry,graph.root.unwrap()).unwrap();let stamp=flow_eval::state_stamp(&state);let c=ctx::create(ctx::Options{domains:Some(8),time:Some(time),input:Some(live),..Default::default()}).unwrap();let actual=cook(&mut s,&c,&n);assert_eq!(Float3::get(geo::positions(&actual),0).unwrap(),(if stateful{dt}else{time},0.,0.));assert_eq!(flow_eval::state_stamp(&state),stamp,"cook mutated supplied state");}
        session::close(&mut s);
    }
}
#[test]fn arrays_live_time(){live_arrays(false,false)}
#[test]fn arrays_live_state(){live_arrays(false,true)}
#[test]fn planes_live_time(){live_arrays(true,false)}
#[test]fn planes_live_state(){live_arrays(true,true)}
fn zone(planes:bool){
    let body=if planes{"(sop/mesh_planes :points {:x (array/float 1 p.x) :y (array/float 1 p.y) :z (array/float 1 p.z)} :vertex_points (array/int 0) :primitive_offsets (array/int 1) :kinds (array/int 0))"}else{"(sop/mesh :points (array/vec3 1 p) :vertex_points (array/int 0) :primitive_offsets (array/int 1) :kinds (array/int 0))"};
    let source=format!("(workspace zone (graph g :context sop (sop/merge (for [p (sop/point_list (sop/ext_src))] {body}))))");
    let expected=rdk_ops::line_geometry::points(&[(0.,0.,0.),(2.,0.,0.),(4.,0.,0.)]).unwrap();
    for reference in [false,true]{let n=authored(reference,&expected,&source);for domains in [1,8]{assert_eq!(fields(&cook_once(domains,&n),false),fields(&expected,false));}}
}
#[test]fn arrays_zone_element_capture(){zone(false)}
#[test]fn planes_zone_element_capture(){zone(true)}
fn rollback(planes:bool){
    let (code,cause)=if planes{("E_PLANE_STATE","plane state cause")}else{("E_MESH_STATE","state cause")};
    let value=format!("(if (> step 0.0) (error {code:?} \"selected state\" :cause {cause:?}) {})",if planes{"0.0"}else{"[0.0 0.0 0.0]"});
    let map=format!("(exact (map (fn [(i : int)] {value}) (array/range 1 :type int)))");
    let points=if planes{format!("{{:x {map} :y (array/float 1) :z (array/float 1)}}")}else{map};
    let op=if planes{"sop/mesh_planes"}else{"sop/mesh"};
    let source=format!("(workspace rollback (graph g :context sop (let* [step (state [previous 0.0] (+ previous (frame/dt)))] ({op} :points {points} :vertex_points (array/int 0) :primitive_offsets (array/int 1) :kinds (array/int 0)))))");
    for reference in [false,true]{let(l,_)=lower_text(lower::Options{reference,..Default::default()},&source);let g=&l.graphs[0];let state=flow_eval::create_state();let lane=value_lane::create(Some(state.clone()));let live=input::Frame{dt:0.5,frame:1,tick:1,..input::at_time(1.)};let resolved=value_lane::resolve(None,Some(&live),&lane,1.,&g.network).unwrap();let n=edit::compile_node(&resolved.geometry,g.root.unwrap()).unwrap();let stamp=flow_eval::state_stamp(&state);
        for domains in [1,8]{let c=ctx::create(ctx::Options{domains:Some(domains),time:Some(1.),input:Some(live.clone()),..Default::default()}).unwrap();let mut s=new_session(64,64_000_000);let d=try_cook(&mut s,&c,&n).err().expect("selected state error succeeded");assert_eq!((d.code.as_str(),d.cause.as_deref()),(code,Some(cause.as_bytes())));assert_eq!(flow_eval::state_stamp(&state),stamp);session::close(&mut s);}
    }
}
#[test]fn arrays_failed_state_rolls_back(){rollback(false)}
#[test]fn planes_failed_state_rolls_back(){rollback(true)}
#[test]
fn captured_coordinate_planes_share_position_owner_count(){
    let coords=evaluate("(let* [p (sop/attr geo :P)] {:x (exact (map (fn [(point : vec3)] point.x) p)) :y (exact (map (fn [(point : vec3)] point.y) p)) :z (exact (map (fn [(point : vec3)] point.z) p))})");
    assert_eq!(ak::sources(&coords),vec![7]);assert_eq!(ak::reads(&coords),vec!["P"]);
    let Value::Record(planes)=coords else{panic!("record expected")};let mut origins=vec![];
    let input = lisp_sop::snapshot(&fixture(1));
    for(_,v)in planes{let p=ak::prepare(None,true,None,None,&[7],std::slice::from_ref(&input),&v).unwrap();assert!(Executor::packed_region(&p).is_none());assert!(Executor::root_is_packed_or_readback(&p));origins.push(flow_ir::packed::count_origin(&Executor::packed_root(&p).unwrap()).unwrap());}
    assert_eq!(origins[0],origins[1]);assert_eq!(origins[1],origins[2]);
}
#[test]
fn compact_gather_exclusive_scan_builds_native_mesh(){
    let source=r#"(workspace compact_mesh (defnode sop/compact_mesh [] (let* [points (array/vec3 6 [1 -0. 2]) mask (exact (map (fn [(i : int)] (if (int/eq (int/mod i 2) 0) 1 0)) (array/range 6 :type int))) kept (array/compact mask) selected (exact (map (fn [(i : int)] (array/nth points i)) kept)) counts (array/int 1 3)] (sop/mesh :points selected :vertex_points (array/range 3 :type int) :primitive_offsets (array/scan_sum counts) :kinds (array/int 1 0)))) (graph g :context sop (sop/compact_mesh)))"#;
    let expected=geo::create(Float3::of_owned(vec![1.;3],vec![-0.;3],vec![2.;3]).unwrap(),topology::create_owned(3,vec![0,1,2],vec![0,3],vec![topology::PrimitiveKind::Polygon]).unwrap(),None,None,None).unwrap();
    for reference in [false,true]{let(_,n)=lower_text(lower::Options{reference,..Default::default()},source);for domains in [1,4,8]{for grain in [1,97,4096]{let mut s=new_session(64,64_000_000);assert_eq!(geometry_bytes(&cook(&mut s,&context(domains,grain),&n)),geometry_bytes(&expected));session::close(&mut s);}}}
}
fn cards(planes:bool){
    use flow_graph::{flow_edit::{self as e,ArgKey,Op},projection as p};
    let text=if planes {r#"(workspace cards (graph g :context sop (let* [coords {:x (array/float 0) :y (array/float 0) :z (array/float 0)} mesh (sop/mesh_planes :points {:x (array/float 0) :y (array/float 0) :z (array/float 0)} :vertex_points (array/int 0) :primitive_offsets (array/int 1) :kinds (array/int 0))] mesh)))"#}
        else {r#"(workspace cards (graph g :context sop (let* [src (sop/line :points 2) topology (sop/topology src) pts (array/vec3 0) vp (array/int 0) po (array/int 1) ks (array/int 0) mesh (sop/mesh :points pts :vertex_points vp :primitive_offsets po :kinds ks)] mesh)))"#};
    let forms=parse(text);let cat=catalog::of_factories(1,&[],None,&sop::nodes::factories()).unwrap();
    let checker=|c:&flow_check::catalog::Catalog,fs:&[flow_syntax::syntax::Syntax]|catalog::check_workspace(operators::all(),true,None,c,fs);
    let(w,ds)=checker(&cat,&forms);assert!(ds.is_empty(),"{ds:?}");let checked=w.unwrap();
    let scope=p::of_graph(&cat,&checked,"g").unwrap();let path=vec!["g".into(),"mesh".into()];let card=p::find(&scope,&path).unwrap();
    assert_eq!(card.rows.iter().map(|r|r.label.as_str()).collect::<Vec<_>>(),vec!["points","vertex_points","primitive_offsets","kinds"]);
    let apply=|op|e::apply_checked_with(operators::all(),true,Some(&checker),&cat,&forms,&op);
    let set=|key:&str,sub:Vec<i64>,value:&str|Op::SetArg{node:path.clone(),key:ArgKey::Kw(key.into()),sub,value:parse(value).remove(0)};
    if planes {
        let plane_ty=Ty::Record(["x","y","z"].into_iter().map(|n|(n.into(),Ty::Array(Box::new(Ty::Float)))).collect());
        let row=card.rows.iter().find(|r|r.label=="points").unwrap();assert_eq!(row.ty,Some(plane_ty.clone()));assert!(row.socket);
        assert!(e::default_for(&plane_ty,"points",None).is_some());
        apply(set("points",vec![],"{:x (array/float 1) :y (array/float 1) :z (array/float 1)}")).unwrap();
        apply(set("points",vec![1],"(array/float 0)")).unwrap();
        apply(Op::Connect{node:path.clone(),key:ArgKey::Kw("points".into()),src:"coords".into(),iter:false}).unwrap();
        let before=flow_syntax::lisp::print(&forms).unwrap().0;
        for bad in ["(array/vec3 0)","{:x (array/float 0) :y (array/float 0)}","{:x (array/int 0) :y (array/float 0) :z (array/float 0)}","{:x (array/float 0) :y (array/float 0) :z (array/float 0) :extra (array/float 0)}"]{assert!(apply(set("points",vec![],bad)).is_err(),"{bad}");}
        assert_eq!(flow_syntax::lisp::print(&forms).unwrap().0,before);
        let saved=parse(&before);lower::workspace(None,vec![],Default::default(),&saved).unwrap();
        let(saved_checked,ds)=checker(&cat,&saved);assert!(ds.is_empty());assert_eq!(saved_checked.unwrap().source.as_ref(),&saved);
    }else{
        let topology=p::find(&scope,&vec!["g".into(),"topology".into()]).unwrap();
        assert_eq!(topology.outputs,["vertex_points","primitive_offsets","kinds"].into_iter().map(|n|(n.into(),Ty::Array(Box::new(Ty::Int)))).collect::<Vec<_>>());
        apply(set("points",vec![],"(array/vec3 0)")).unwrap();
        apply(Op::Connect{node:path.clone(),key:ArgKey::Kw("vertex_points".into()),src:"vp".into(),iter:false}).unwrap();
        let before=flow_syntax::lisp::print(&forms).unwrap().0;assert!(apply(set("kinds",vec![],"(array/text 0)")).is_err());assert_eq!(flow_syntax::lisp::print(&forms).unwrap().0,before);
        assert!(e::default_for(&Ty::Array(Box::new(Ty::Int)),"value",None).is_some());
        for bad in ["(workspace bad (graph g :context sop (sop/mesh :points (array/vec3 0))))","(workspace bad (graph g :context sop (sop/topology 3)))"]{assert!(!checker(&cat,&parse(bad)).1.is_empty());}
    }
    for key in ["vertex_points","primitive_offsets","kinds"]{apply(set(key,vec![],if key=="primitive_offsets"{"(array/int 1)"}else{"(array/int 0)"})).unwrap();}
    let starter=if planes{"(sop/mesh_planes :points {:x (array/float 0) :y (array/float 0) :z (array/float 0)} :vertex_points (array/int 0) :primitive_offsets (array/int 1) :kinds (array/int 0))"}else{"(sop/mesh :points (array/vec3 0) :vertex_points (array/int 0) :primitive_offsets (array/int 1) :kinds (array/int 0))"};
    apply(Op::AddNode{scope:vec!["g".into()],name:"empty_mesh".into(),expr:parse(starter).remove(0)}).unwrap();
    assert_eq!(geometry_bytes(&cook_once(1,&lisp_sop::node(&[],starter))),geometry_bytes(&fixture(0)));
}
#[test]fn arrays_cards_defaults_checked_edits_starter(){cards(false)}
#[test]fn planes_cards_defaults_checked_edits_save_starter(){cards(true)}
fn deferred_source(result:&str)->String{
    format!(r#"(workspace cpu_deferred (graph g :context sop (let* [points (map (fn [(i : int)] [i 0 0]) (array/range 5001 :type int)) normals (map (fn [(i : int)] [0 1 0]) (array/range 5001 :type int)) colors (map (fn [(n : vec3)] [n.y 0 0]) normals) mesh (sop/mesh :points (exact points) :vertex_points (array/int 0) :primitive_offsets (array/int 1 0) :kinds (array/int 0)) shaded (sop/with_attr mesh "N" (exact normals) :owner "point" :type vec3) colored (sop/with_attr shaded "Cd" (exact colors) :owner "point" :type vec3)] {result})))"#)
}
fn attribute_writes(root:&node::T)->usize{fn visit(n:&node::T,seen:&mut std::collections::HashSet<i64>)->usize{if !seen.insert(node::id(n)){return 0}usize::from(node::operation(n)=="flow.with_attr")+node::inputs(n).iter().map(|n|visit(n,seen)).sum::<usize>()}visit(root,&mut Default::default())}
#[test]
fn deferred_static_mesh_writes_fold_without_frame_or_gpu_probes(){
    struct NoGpu;
    impl Gpu::Backend for NoGpu{
        fn cost(&self,_:Option<&flow_ir::packed::Inputs>,_:bool,_:&flow_ir::packed::T,_:i64)->Option<f64>{panic!("static deferred data reached GPU cost")}
        fn prepare(&self,_:bool,_:&flow_ir::packed::T)->Result<Arc<dyn Gpu::Kernel>,rays_diagnostic::Diagnostic>{panic!("static deferred data reached GPU prepare")}
    }
    use flow_ir::flow_ir::Gpu;
    let gpu=Gpu::Config{fast:true,backend:Some(Arc::new(NoGpu)),..Gpu::default()};
    let(l,n)=lower_text(lower::Options{defer:true,gpu:gpu.clone(),..Default::default()},&deferred_source("colored"));
    assert_eq!(attribute_writes(&n),0);let g=&l.graphs[0];assert!(g.network.frame_nodes.is_empty());
    let(_,moving)=lower_text(lower::Options{defer:true,..Default::default()},&deferred_source(r#"(sop/with_attr colored "heat" (exact (map (fn [(n : vec3)] (+ n.y t)) normals)) :owner "point" :type float)"#));assert_eq!(attribute_writes(&moving),2);
    let resolved=value_lane::resolve(Some(&gpu),None,&value_lane::create(None),0.,&g.network).unwrap();let n=edit::compile_node(&resolved.geometry,g.root.unwrap()).unwrap();let mut first=None;
    for domains in [1,8]{let actual=cook_once(domains,&n);assert_eq!(geo::point_count(&actual),5001);let p=Float3::View::view(geo::positions(&actual)).unwrap();assert_eq!(p.x[5000],5000.);let Storage::Float3(data)=attribute::storage(&geo::find_attribute(Owner::Point,"N",&actual).unwrap()).unwrap()else{panic!("normal storage")};assert!(Float3::View::view(&data).unwrap().y.iter().all(|y|*y==1.));let bytes=geometry_bytes(&actual);match &first{None=>first=Some(bytes),Some(expected)=>assert_eq!(&bytes,expected)}}
}
#[test]
fn deferred_malformed_mesh_publishes_no_cache_entry(){
    let source=r#"(workspace malformed (graph g :context sop (sop/mesh :points (exact (map (fn [(i : int)] [i 0 0]) (array/range 5001 :type int))) :vertex_points (array/int 3 6000) :primitive_offsets (exact (map (fn [(i : int)] (* i 3)) (array/range 2 :type int))) :kinds (array/int 1 0))))"#;
    let(_,n)=lower_text(lower::Options{defer:true,..Default::default()},source);
    for domains in [1,8]{let mut s=new_session(64,64_000_000);let d=try_cook(&mut s,&context(domains,97),&n).err().expect("malformed deferred mesh published");assert_eq!((d.code.as_str(),d.message.as_str()),("E_TOPOLOGY","Topology: vertex references an invalid point"));assert_eq!(session::stats(&s).retained_entries,0);session::close(&mut s);}
}

#[test]
fn text_attribute_eval_read_write_preserves_every_byte() {
    let bytes: Vec<u8> = (0..=255).collect();
    let rows = vec![bytes.clone(), bytes.iter().rev().copied().collect(), vec![255], Vec::new(), [b"\xe7\x8c\xab".as_slice(), &[255]].concat()];
    let input = geo::with_attribute(attribute::create_owned("bytes", Owner::Point, Storage::Text(rows.clone())).unwrap(), &fixture(1)).unwrap();
    let lookup: ak::GeometryLookup = Arc::new({ let input = input.clone(); move |_| Some(input.clone()) });
    let source = source_value("sop/attr", Ty::Array(Box::new(Ty::Text)), vec![
        ("geometry".into(), Value::Deferred(ty::geometry(), 7)),
        ("attribute".into(), Value::Text(b"bytes".to_vec())),
        ("type".into(), Value::Text(b"text".to_vec())),
    ]);
    assert_eq!(ak::resolve(None, &lookup, &source).unwrap(), Value::Text_array(Arc::new(rows.clone())));
    let literal = flow_syntax::lisp::quote_string(&bytes);
    let cases = [
        (r#"(exact (map (fn [(value : text)] (str value "\000")) (sop/attr geo "bytes" :type text)))"#.to_owned(), rows.iter().map(|row| [row.as_slice(), &[0]].concat()).collect()),
        (format!("(array/text (sop/count geo :points) {literal})"), vec![bytes; rows.len()]),
    ];
    for (values, expected) in cases {
        let expected = geo::with_attribute(attribute::create_owned("copied", Owner::Point, Storage::Text(expected)).unwrap(), &input).unwrap();
        for reference in [false, true] {
            let root = authored(reference, &input, &wrapped(&format!("(sop/with_attr geo \"copied\" {values} :type text)")));
            for domains in [1, 8] {
                let mut s = new_session(64, 64_000_000);
                assert_eq!(geometry_bytes(&cook(&mut s, &context(domains, 1), &root)), geometry_bytes(&expected));
                session::close(&mut s);
            }
        }
    }
}

#[test]
fn attribute_identifiers_refuse_non_utf8() {
    let op = operators::all().into_iter().find(|op| op.name == "sop/with_attr").unwrap();
    for (field, code) in [("attribute", "E_ATTR_NAME"), ("owner", "E_ATTR_OWNER"), ("type", "E_ATTR_TYPE")] {
        let mut args = vec![("attribute".to_owned(), Value::Text(b"bytes".to_vec()))];
        if field == "attribute" {
            args[0].1 = Value::Text(vec![255]);
        } else {
            args.push((field.to_owned(), Value::Text(vec![255])));
        }
        assert_eq!((op.check)(&args).unwrap_err().code, code);
    }
}
