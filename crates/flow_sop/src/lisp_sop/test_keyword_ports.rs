//! lib/flow_sop/test_keyword_ports.ml: native Fn/Image factory, manifest, edits, captures and cache.
use super::support::*;
use crate::{catalog,manifest,lower,operators,attribute_kernel as ak};
use flow_graph::{flow_edit::{self as e,ArgKey,Op},projection as p};
use flow_syntax::{syntax,ty::{Ty,FnSignature},value::Value};
use flow_ir::flow_ir::executor;
use sop::{edit_graph as edit,node::{self,build,CookMode},kernel::{self,Column},payload,context as ctx,session};
use std::sync::Arc;
fn factory()->edit::Factory{
    let mut args=edit::factory_args("test_field","Test field",&["Test"]);args.slots=Some(vec!["field".into(),"picture".into()]);args.keyword_inputs=vec!["field".into(),"picture".into()];args.input_types=Some(vec!["fn(vec3,float)->float".into(),"image".into()]);
    edit::factory(args,2,Arc::new(|inputs|Ok(build::make(build::Args{inputs:inputs.to_vec(),cook_mode:CookMode::Generic,..build::args("test_field")},None,Arc::new(|_,context,inputs|{
        payload::image(&inputs[1])?;let kernel=payload::kernel(&inputs[0])?;let run=(kernel::prepare(&kernel))(&[Column::Vec3s(vec![2.,0.,0.]),Column::Floats(vec![0.])])?;
        let Column::Floats(xs)=run(context)? else{panic!("field returns floats")};
        let geometry=rdk_ops::line_geometry::points(&xs.into_iter().map(|x|(x,0.,0.)).collect::<Vec<_>>()).unwrap();
        Ok(node::Cooked{payload:payload::T::Geometry(geometry),instances:None})
    })))))
}
fn factories()->Vec<edit::Factory>{let mut fs=vec![factory()];fs.extend(sop::nodes::factories());fs}
fn lowered(reference:bool,previous:Option<&lower::T>,text:&str)->lower::T{
    lower::workspace(Some(factories()),vec![],lower::Options{reference,compiled_ids:previous.map(|l|l.compiled_ids.clone()).unwrap_or_default(),sites:previous.map(|l|l.sites.clone()).unwrap_or_default(),..Default::default()},&parse(text)).unwrap_or_else(|d|panic!("{}",rays_diagnostic::to_string(&d)))
}
fn root(l:&lower::T)->node::T{let g=&l.graphs[0];edit::compile_node(&g.network.geometry,g.root.unwrap()).unwrap()}
fn points(s:&mut session::T,domains:i64,time:f64,n:&node::T)->Vec<(f64,f64,f64)>{let c=ctx::create(ctx::Options{domains:Some(domains),time:Some(time),..Default::default()}).unwrap();let g=cook(s,&c,n);(0..rdk_core::geometry::point_count(&g)).map(|i|rdk_core::packed::float3::get(rdk_core::geometry::positions(&g),i).unwrap()).collect()}
#[test]
fn keyword_manifest_defaults_projection_checked_gestures(){
    let fs=factories();let cat=catalog::of_factories(manifest::VERSION,&[],None,&fs).unwrap();let(text,_)=manifest::generate(&[],None,&fs).unwrap();let(loaded,_)=flow_check::catalog::catalog_of_manifest(&text).unwrap();assert_eq!(cat,loaded);
    let forms=parse("(workspace w (graph g :context sop (let* [arg0 1] nil)))");let kind=&cat.kinds[0];
    let mut default="(sop/test_field".to_owned();for parameter in &kind.parameters{let ty=match &parameter.ty{Some(flow_check::port_type::PortType::Fn(sig))=>Ty::Fn(Some(Box::new(sig.clone()))),Some(flow_check::port_type::PortType::Image)=>flow_syntax::ty::image(),_=>panic!("typed keyword")};let fresh=|base:&str|e::fresh_name(&forms,"g",base);let expression=e::default_for(&ty,&parameter.name,Some(&fresh)).unwrap();default.push_str(&format!(" :{} {}",parameter.name,flow_syntax::lisp::flat(&expression)));}default.push(')');
    let checker=|c:&flow_check::catalog::Catalog,fs:&[syntax::Syntax]|catalog::check_workspace(operators::all(),false,None,c,fs);
    let(added,w)=e::apply_checked_with(operators::all(),false,Some(&checker),&cat,&forms,&Op::AddNode{scope:vec!["g".into()],name:"surface".into(),expr:parse(&default).remove(0)}).unwrap();
    assert!(p::of_graph(&cat,&w,"g").unwrap().nodes.iter().any(|n|n.zone.as_ref().is_some_and(|z|z.kind==p::ZoneKind::Fn)));
    e::apply_checked_with(operators::all(),false,Some(&checker),&cat,&added,&Op::SetArg{node:vec!["g".into(),"surface".into()],key:ArgKey::Kw("field".into()),sub:vec![],value:parse("(fn [p amount] (+ p.x amount))").remove(0)}).unwrap();
    for cat in [&cat,&loaded]{let checked=|args:&str|checker(cat,&parse(&format!("(workspace w (graph g :context sop (sop/test_field {args})))"))).0.is_some();assert!(checked(":field (fn [p amount] (+ p.x amount)) :picture (image/noise :width 2 :height 2)"));for bad in [":field (fn [p] p.x) :picture (image/noise)",":picture (image/noise)",":field (fn [p amount] (+ p.x amount))"]{assert!(!checked(bad));}}
    let input=crate::lisp_sop::node(&[],"(-> (sop/box) (sop/null))");let built=edit::instantiate(&factory(),&[input.clone(),input.clone()]).unwrap();assert!(node::inputs(&built).iter().all(|n|node::ptr_eq(n,&input)));
}
fn text(offset:i64)->String{format!("(workspace w (defn consume :context sop [(f : fn)] (sop/test_field :field f :picture (image/noise :width 1 :height 1))) (graph g :context sop (let* [a (let* [offset (+ {offset} t) field (fn [p amount] (+ p.x offset))] (consume field)) b (let* [offset (+ 4 t) field (fn [p amount] (+ p.x offset))] (sop/test_field :field field :picture (image/noise :width 1 :height 1)))] (sop/merge a b))))")}
fn live_cache(domains:i64){
    let first=lowered(false,None,&text(7));let functions=first.plan.nodes.iter().filter(|n|n.kind=="sop/test_field").map(|n|{let Value::Fn(f)=n.args.iter().find(|(k,_)|k=="field").unwrap().1 else{panic!("function input")};f}).collect::<Vec<_>>();assert_eq!(functions.len(),2);
    let values=std::sync::Mutex::new(vec![Value::No_geo;functions.len()]);
    parallel::run_unit(Some(domains),&||parallel::for_(1,0,functions.len()as i64-1,&|i|{
        let v=flow_eval::host::map_function(None,None,&FnSignature{params:vec![Ty::Vec3,Ty::Float],result:Ty::Vec3},functions[i as usize],&[Value::Floats(3, Arc::new(vec![2.,0.,0.])),Value::Floats(1, Arc::new(vec![0.]))]).unwrap();
        let p=ak::prepare(None,false,None,None,&[],&[],&v).unwrap();let packed=executor::packed_region(&p).unwrap();let live=input::at_time(0.5);
        let a=flow_ir::packed::force(&packed,&live,&Default::default()).unwrap();let b=executor::force(&p,&executor::ForceOptions{reference:true,..Default::default()},&live).unwrap();assert_eq!(a,b);
        values.lock().unwrap()[i as usize]=a;Ok(())
    })).unwrap();
    assert_eq!(*values.lock().unwrap(),vec![Value::Floats(3, Arc::new(vec![9.5;3])),Value::Floats(3, Arc::new(vec![6.5;3]))]);
    let n=root(&first);let mut s=new_session(32,65536);assert_eq!(points(&mut s,domains,0.,&n),vec![(9.,0.,0.),(6.,0.,0.)]);let cooks=session::stats(&s).cooks;assert_eq!(points(&mut s,domains,0.,&n),vec![(9.,0.,0.),(6.,0.,0.)]);assert_eq!(session::stats(&s).cooks,cooks);assert_eq!(points(&mut s,domains,0.5,&n),vec![(9.5,0.,0.),(6.5,0.,0.)]);assert_eq!(points(&mut s,domains,0.5,&root(&lowered(true,None,&text(7)))),vec![(9.5,0.,0.),(6.5,0.,0.)]);let edited=root(&lowered(false,Some(&first),&text(10)));assert_eq!(node::id(&edited),node::id(&n));assert_eq!(points(&mut s,domains,0.,&edited),vec![(12.,0.,0.),(6.,0.,0.)]);session::close(&mut s);
}
#[test]fn keyword_live_cache_one_domain(){live_cache(1)}
#[test]fn keyword_live_cache_eight_domains(){live_cache(8)}
fn capture(zone:bool){let source=if zone{r#"(workspace w (graph g :context sop (let* [source (sop/curve (list [1 0 0] [2 0 0])) samples (for [p (sop/point_list source)] (sop/test_field :field (fn [v amount] (+ v.x p.x)) :picture (image/noise :width 1 :height 1)))] (sop/merge samples))))"#}else{r#"(workspace w (graph g :context sop (let* [source (sop/curve (list [1 0 0] [2 0 0])) sample {:bias (array/sum (sop/attr source :P))} field (fn [p amount] (+ p.x sample.bias.x))] (sop/test_field :field field :picture (image/noise :width 1 :height 1)))))"#};let expected=if zone{vec![(3.,0.,0.),(4.,0.,0.)]}else{vec![(5.,0.,0.)]};for reference in [false,true]{let n=root(&lowered(reference,None,source));for domains in [1,8]{let mut s=new_session(32,65536);assert_eq!(points(&mut s,domains,0.,&n),expected);session::close(&mut s);}}}
#[test]fn keyword_cooked_geometry_capture(){capture(false)}
#[test]fn keyword_zone_element_capture(){capture(true)}
