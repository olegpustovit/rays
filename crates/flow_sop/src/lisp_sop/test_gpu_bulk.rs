//! : the source's explicit float32 mock measures placement, not native correctness.
use super::support::*;
use crate::{attribute_kernel,lower,value_lane};
use flow_ir::{flow_ir::{gpu,PlacementReason,profile},packed};
use flow_syntax::value::Value;
use rdk_core::{geometry as geo, packed::float3};
use sop::{context as ctx,edit_graph as edit,payload,session};
use std::sync::{Arc,Mutex,atomic::{AtomicUsize,Ordering}};
#[derive(Default)]
struct Calls{dispatches:AtomicUsize,planes:AtomicUsize,time:Mutex<f64>}
struct Mock{cost:Option<f64>,calls:Arc<Calls>,owner:std::thread::ThreadId}
struct MockKernel{program:packed::T,calls:Arc<Calls>,owner:std::thread::ThreadId,result:Arc<Mutex<Option<Vec<Value>>>>}
fn backend(cost:Option<f64>,calls:&Arc<Calls>)->Arc<dyn gpu::Backend>{
    Arc::new(Mock{cost,calls:calls.clone(),owner:std::thread::current().id()})
}
impl gpu::Backend for Mock{
    fn cost(&self,_:Option<&packed::Inputs>,_:bool,_:&packed::T,_:i64)->Option<f64>{self.cost}
    fn prepare(&self,_:bool,program:&packed::T)->Result<Arc<dyn gpu::Kernel>,rays_diagnostic::Diagnostic>{
        assert_eq!(std::thread::current().id(),self.owner,"prepare must stay on its owning domain");
        Ok(Arc::new(MockKernel{program:program.clone(),calls:self.calls.clone(),owner:self.owner,result:Arc::new(Mutex::new(None))}))
    }
}
impl gpu::Kernel for MockKernel{
    fn run(&self,inputs:&packed::Inputs)->Result<gpu::Value,rays_diagnostic::Diagnostic>{
        assert_eq!(std::thread::current().id(),self.owner,"dispatch must stay on its owning domain");let stamp=self.calls.dispatches.fetch_add(1,Ordering::SeqCst)+1;
        let count=inputs.count;let resolve=Arc::new(move|_:&Value|Ok(Value::Int(count)));
        let options=packed::ForceOptions{resolve:Some(resolve),..Default::default()};
        let values=packed::force_all(&self.program,&input::at_time(*self.calls.time.lock().unwrap()),&options)?;
        let width=match &values[0]{Value::Floats(1, _)=>1,Value::Floats(2, _)=>2,Value::Floats(3, _)=>3,Value::Floats(4, _)=>4,_=>panic!("float output expected")};
        *self.result.lock().unwrap()=Some(values);
        Ok(gpu::Value{identity:1,count,width,output:0,stamp:stamp as i64,gpu_seconds:None})
    }
    fn readback(&self,v:&gpu::Value)->Result<Value,rays_diagnostic::Diagnostic>{
        let values=self.result.lock().unwrap();let value=&values.as_ref().unwrap()[v.output as usize];
        let round=|xs:&Arc<Vec<f64>>|Arc::new(xs.iter().map(|x|(*x as f32)as f64).collect());
        Ok(match value{Value::Floats(1, xs)=>Value::Floats(1, round(xs)),Value::Floats(2, xs)=>Value::Floats(2, round(xs)),Value::Floats(3, xs)=>Value::Floats(3, round(xs)),Value::Floats(4, xs)=>Value::Floats(4, round(xs)),_=>panic!("float output expected")})
    }
    fn planes(&self,v:&gpu::Value)->Result<gpu::Planes,rays_diagnostic::Diagnostic>{self.calls.planes.fetch_add(1,Ordering::SeqCst);gpu::planes_of(&|v|self.readback(v),v)}
}
fn ticks()->lower::Clock{let n=Arc::new(AtomicUsize::new(0));Arc::new(move||n.fetch_add(1,Ordering::SeqCst)as f64+1.)}
fn positions(backend:Option<Arc<dyn gpu::Backend>>)->float3::T{
    let text="(workspace g (graph g :context sop (sop/grid :counts \"Point counts\" :connectivity \"Points\" :columns 100 :rows 100 :size 3.0 :rotate 0.3)))";
    let gpu=backend.map_or_else(gpu::default,|b|gpu::Config{backend:Some(b),fast:true,..gpu::default()});
    let(_,n)=lower_text(lower::Options{gpu,clock:ticks(),..Default::default()},text);
    let mut s=new_session(0,0);let c=context(4,97);let mut last=None;
    for _ in 0..6{let out=session::cook(&mut s,Some(1),&c,&n).unwrap();last=Some(geo::positions(&payload::geometry(&out.payload).unwrap()).clone());}
    session::close(&mut s);last.unwrap()
}
fn placement(cost:Option<f64>,expected:usize){
    let cpu=positions(None);let calls=Arc::new(Calls::default());let output=positions(Some(backend(cost,&calls)));
    assert_eq!(calls.dispatches.load(Ordering::SeqCst),expected);
    for i in 0..10000{let(a,b,c)=float3::get(&cpu,i).unwrap();let(x,y,z)=float3::get(&output,i).unwrap();for(e,a)in[(a,x),(b,y),(c,z)]{if expected==0{assert_eq!(e.to_bits(),a.to_bits())}else{assert!((e-a).abs()<=1e-4*(1.+e.abs()));}}}
}
#[test]fn measured_slower_gpu_never_dispatches(){placement(Some(1e6),0)}
#[test]fn measured_cheaper_gpu_fuses_one_dispatch_per_cook(){placement(Some(0.),6)}
#[test]fn measured_unpriced_gpu_remains_device_first(){placement(None,6)}
fn captured(points:bool,capture:bool,time:f64){
    let calls=Arc::new(Calls::default());*calls.time.lock().unwrap()=time;
    let gpu=gpu::Config{backend:Some(backend(Some(0.),&calls)),fast:true,..gpu::default()};
    let source=if points{"(sop/points :points 32)"}else{"(sop/mesh_planes :points {:x (array/float 32) :y (array/float 32) :z (array/float 32)} :vertex_points (array/int 0) :primitive_offsets (array/int 1) :kinds (array/int 0))"};
    let text=format!("(workspace readers (graph g :context sop (sop/with_attr {source} :P (exact (map (fn [i] [(+ i t) 0 0]) (array/range 32))))))");
    let(l,_)=lower_text(lower::Options{gpu:gpu.clone(),clock:ticks(),..Default::default()},&text);let graph=&l.graphs[0];
    let resolved=value_lane::resolve(Some(&gpu),None,&value_lane::create(None),time,&graph.network).unwrap();let root=edit::compile_node(&resolved.geometry,graph.root.unwrap()).unwrap();let root=if capture{attribute_kernel::materialized_source(&root)}else{root};
    let mut s=new_session(0,0);let context=ctx::create(ctx::Options{domains:Some(1),time:Some(time),..Default::default()}).unwrap();
    if capture{cook(&mut s,&context,&root);cook(&mut s,&context,&root);}
    calls.planes.store(0,Ordering::SeqCst);let geometry=cook(&mut s,&context,&root);
    if points{for(name,indexed)in[("sourcepoint",false),("sourceindex",true)]{let rdk_core::attribute::Storage::Int(xs)=rdk_core::attribute::storage(&geo::find_attribute(rdk_core::attribute::Owner::Point,name,&geometry).unwrap()).unwrap()else{panic!("source integer attribute")};assert_eq!(*xs,(0..32).map(|i|if indexed{i}else{-1}).collect::<Vec<_>>());}}
    assert_eq!(calls.planes.load(Ordering::SeqCst),usize::from(capture));
    if capture{assert!(profile::placements(&l.profile).iter().any(|p|p.cpu_consumers==Some(1)&&p.reason==PlacementReason::DeviceMeasuredCheaper));}
    assert_eq!(float3::get(geo::positions(&geometry),31).unwrap(),(31.+time,0.,0.));session::close(&mut s);
}
#[test]fn opaque_mesh_declines_resident_record(){captured(false,false,0.)}
#[test]fn capture_mesh_supplies_one_cpu_reader(){captured(false,true,0.);captured(false,true,1.)}
#[test]fn opaque_points_declines_resident_record(){captured(true,false,0.)}
#[test]fn capture_points_supplies_one_cpu_reader(){captured(true,true,0.);captured(true,true,1.)}
