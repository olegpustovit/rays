//! lib/flow_sop/test_math_helpers.ml: exact scalar/FMA bytes, four-ULP compound native gate.
use crate::{catalog, operators};
use flow_syntax::{syntax::{self,Node,Syntax},value::Value};
use rays_math::{mat4 as m,vec3 as v,quat as q};
use std::sync::LazyLock;
static SOURCES: LazyLock<Vec<Syntax>>=LazyLock::new(||{
    let forms=syntax::parse(include_str!("../../../../../lib/flow_sop/nodes/prelude.rays")).unwrap();
    forms[0].children().iter().map(|&id| forms[0].child(id)).filter(|f| {
        let children = f.children();
        syntax::head(f) == Some("defn") && !matches!(children, [_, _, k, s, ..] if matches!(f.arena().row(*k).node, Node::Kw(name) if f.name(name) == "context") && matches!(f.arena().row(*s).node, Node::Sym(name) if f.name(name) == "sop"))
    }).collect()
});
fn evaluate(body:&str)->Result<Value,rays_diagnostic::Diagnostic>{
    let forms=syntax::parse(&format!("(workspace math_helpers (graph result :context host {body}))")).unwrap();
    let (w,ds)=catalog::check_workspace(operators::all(),false,Some(&SOURCES),&flow_check::catalog::Catalog{version:1,kinds:vec![]},&forms);
    assert!(ds.is_empty(),"{ds:?}");
    Ok(flow_eval::r#static(&w.unwrap(),false,None)?.results.into_iter().find(|(n,_)|n=="result").unwrap().1)
}
fn call(name:&str,args:&[String])->Value{evaluate(&format!("({name} {})",args.join(" "))).unwrap_or_else(|d|panic!("{}",rays_diagnostic::to_string(&d)))}
fn num(x:f64)->String{flow_syntax::lisp::float(x)}
fn vec(xs:&[f64])->String{format!("[{}]",xs.iter().map(|x|num(*x)).collect::<Vec<_>>().join(" "))}
fn triple(x:v::T)->String{vec(&[x.x,x.y,x.z])}
fn quat(x:q::T)->String{vec(&[x.x,x.y,x.z,x.w])}
fn matrix(x:&m::T)->String{format!("{{{}}}",(0..4).map(|r|format!(":r{r} {}",vec(&(0..4).map(|c|m::get(x,r,c).unwrap()).collect::<Vec<_>>()))).collect::<Vec<_>>().join(" "))}
fn values(x:Value)->Vec<f64>{match x{Value::Float(x)=>vec![x],Value::Vec3(x,y,z)=>vec![x,y,z],Value::Vec4(x,y,z,w)=>vec![x,y,z,w],_=>panic!("numeric helper result expected")}}
fn distance(a:f64,b:f64)->f64{
    if a==b{return 0.0} if !a.is_finite()||!b.is_finite(){return f64::INFINITY}
    let x=a.abs().max(b.abs());let bits=x.to_bits();let e=((bits>>52)&0x7ff) as i32;
    let exponent=if e==0{-1074}else{e-1023-52};(a-b).abs()/2f64.powi(exponent)
}
fn same(ulps:u32,expected:&[f64],actual:Value){let actual=values(actual);assert_eq!(actual.len(),expected.len());for (a,b) in expected.iter().zip(actual){if ulps==0{assert_eq!(a.to_bits(),b.to_bits(),"expected {a:.17}, got {b:.17}")}else{assert!(distance(*a,b)<=ulps as f64,"expected {a:.17}, got {b:.17}: {} ULP",distance(*a,b));}}}
fn check_v(expected:v::T,actual:Value){same(4,&[expected.x,expected.y,expected.z],actual)}
fn check_q(expected:q::T,actual:Value){same(4,&[expected.x,expected.y,expected.z,expected.w],actual)}
fn check_m(ulps:u32,expected:&m::T,actual:Value){let Value::Record(fields)=actual else{panic!("matrix record expected")};for r in 0..4{same(ulps,&(0..4).map(|c|m::get(expected,r,c).unwrap()).collect::<Vec<_>>(),fields.iter().find(|(n,_)|n==&format!("r{r}")).unwrap().1.clone());}}
fn vectors()->[v::T;4]{[v::zero(),v::create(-0.0,2.0,-3.0),v::create(0.3,-0.7,1.2),v::create(1e-150,-2e-150,3e-150)]}
fn matrices()->Vec<m::T>{vec![m::identity(),m::of_rows((2.,3.,-1.,4.),(5.,-2.,6.,-3.),(1.,4.,3.,2.),(0.2,-0.1,0.4,2.)),m::of_rows((0.,1.,0.,0.),(1.,0.,0.,0.),(0.,0.,2.,0.),(0.,0.,0.,1.)),m::mul(&m::translation(v::create(2.,-4.,7.)),&m::rotation(v::create(1.,2.,3.),0.7)),m::scaling(v::create(0.,1.,2.)),m::scaling(v::create(1e-12,1.,1.)),m::scaling(v::create(1e-12f64.next_up(),1.,1.)),m::of_rows((1.,2.,3.,4.),(2.,4.,6.,8.),(0.,0.,1.,0.),(0.,0.,0.,1.)),m::of_rows((-0.,0.,0.,0.),(0.,-0.,0.,0.),(0.,0.,-0.,0.),(0.,0.,0.,-0.))]}
#[test]
fn scalar_math_exact_bytes_and_nonfinite_refusals(){
    let tiny=f64::from_bits(1);
    let cases:Vec<(&str,fn(f64)->f64,Vec<f64>)>=vec![
        ("tan",f64::tan,vec![0.,-0.,0.37,std::f64::consts::PI,1e19]),
        ("asin",f64::asin,vec![-1.,-0.37,-0.,0.,0.37,1.]),("acos",f64::acos,vec![-1.,-0.37,-0.,0.,0.37,1.]),
        ("exp",f64::exp,vec![-700.,-0.,0.,0.37,700.]),("log",f64::ln,vec![tiny,1e-300,1.,f64::MAX]),
        ("sign",|x|if x>0.{1.}else if x<0.{-1.}else{x},vec![-f64::MAX,-tiny,-0.,0.,tiny,f64::MAX])];
    for (name,op,xs) in cases{for x in xs{same(0,&[op(x)],call(name,&[num(x)]));}}
    for (a,b) in [(0.,0.),(-0.,-0.),(0.,-0.),(-0.,0.),(1.,-1.),(-1.,-1.),(1e308,1e308),(1e-300,1e-300),(tiny,tiny)]{same(0,&[f64::atan2(a,b)],call("atan2",&[num(a),num(b)]));same(0,&[f64::hypot(a,b)],call("hypot",&[num(a),num(b)]));}
    for body in ["(asin 1.0000000000000002)","(acos -1.0000000000000002)","(log 0.0)","(log -1.0)","(exp 1000.0)"]{assert_eq!(evaluate(body).unwrap_err().code,"E_NONFINITE","{body}");}
    for (name,n) in [("floor",-2),("ceil",-1),("round",-2)]{assert_eq!(call(name,&["-1.5".into()]),Value::Int(n));}
    for x in [0.,-0.,0.1,-0.1,1.5,-1.5,tiny,-tiny,4503599627370496.,-4503599627370496.,4503599627370496.0f64.next_down(),(-4503599627370496.0f64).next_up(),f64::MAX,-f64::MAX]{same(0,&[x.floor()],call("float_floor",&[num(x)]));same(0,&[x.ceil()],call("float_ceil",&[num(x)]));}
}
#[test]
fn vector_native_and_explicit_fma_contract(){
    for a in vectors(){check_v(v::normalize(a),call("vec3_normalize",&[triple(a)]));check_v(v::normalize(a),call("normalize",&[triple(a)]));
        check_m(4,&m::translation(a),call("mat4_translation",&[triple(a)]));check_m(4,&m::scaling(a),call("mat4_scaling",&[triple(a)]));
        for b in vectors(){for name in ["vec3_dot","dot"]{same(4,&[v::dot(a,b)],call(name,&[triple(a),triple(b)]));}
            let fused=[(-a.z).mul_add(b.y,a.y*b.z),(-a.x).mul_add(b.z,a.z*b.x),(-a.y).mul_add(b.x,a.x*b.y)];
            for name in ["vec3_cross","cross"]{same(0,&fused,call(name,&[triple(a),triple(b)]));}
            let unfused=v::cross(a,b);same(0,&[unfused.x,unfused.y,unfused.z],evaluate(&format!("(let* [a {} b {}] [(- (* a.y b.z) (* a.z b.y)) (- (* a.z b.x) (* a.x b.z)) (- (* a.x b.y) (* a.y b.x))])",triple(a),triple(b))).unwrap());
        }
    }
}
#[test]
fn rotation_native_four_ulp_gate(){
    check_m(4,&m::identity(),call("mat4_identity",&[]));
    for angle in [0.,-0.,0.37,std::f64::consts::PI,-std::f64::consts::PI]{for (name,f) in [("mat4_rotation_x",m::rotation_x as fn(f64)->m::T),("mat4_rotation_y",m::rotation_y),("mat4_rotation_z",m::rotation_z)]{check_m(4,&f(angle),call(name,&[num(angle)]));}for axis in vectors(){check_m(4,&m::rotation(axis,angle),call("mat4_rotation",&[triple(axis),num(angle)]));}}
}
fn matrix_case(index:usize){
    let ms=matrices();let a=&ms[index];check_m(4,&m::transpose(a),call("mat4_transpose",&[matrix(a)]));
    for b in [&ms[0],&ms[1]]{let row=|r|{let mut x=[0.;4];for c in 0..4{for k in 0..4{x[c]=m::get(a,r,k).unwrap().mul_add(m::get(b,k,c as i64).unwrap(),x[c]);}}(x[0],x[1],x[2],x[3])};let fused=m::of_rows(row(0),row(1),row(2),row(3));check_m(0,&fused,call("mat4_mul",&[matrix(a),matrix(b)]));}
    for p in vectors(){check_v(m::transform_point(a,p),call("mat4_transform_point",&[matrix(a),triple(p)]));check_v(m::transform_direction(a,p),call("mat4_transform_direction",&[matrix(a),triple(p)]));let(x,y,z,w)=m::transform(a,(p.x,p.y,p.z,-0.5));same(4,&[x,y,z,w],call("mat4_transform",&[matrix(a),vec(&[p.x,p.y,p.z,-0.5])]));}
    let Value::List(xs)=call("mat4_inverse",&[matrix(a)])else{panic!("inverse list expected")};match m::inverse(a){None=>assert!(xs.is_empty()),Some(expected)=>{assert_eq!(xs.len(),1);check_m(4,&expected,xs[0].clone());}}
}
macro_rules! matrix_test{($name:ident,$i:expr)=>{#[test]fn $name(){matrix_case($i)}}}
matrix_test!(matrix_identity,0);matrix_test!(matrix_asymmetric,1);matrix_test!(matrix_swap,2);matrix_test!(matrix_translation_rotation,3);matrix_test!(matrix_singular,4);matrix_test!(matrix_inverse_threshold,5);matrix_test!(matrix_inverse_above_threshold,6);matrix_test!(matrix_dependent_rows,7);matrix_test!(matrix_signed_zero,8);
#[test]
fn projective_threshold_and_quaternion_products(){
    for w in [0.,1e-12,-1e-12,1e-12f64.next_up(),2.]{let a=m::of_rows((1.,0.,0.,2.),(0.,1.,0.,3.),(0.,0.,1.,4.),(0.,0.,0.,w));let p=v::create(0.3,0.4,0.5);check_v(m::transform_point(&a,p),call("mat4_transform_point",&[matrix(&a),triple(p)]));}
    let qs=[q::create(0.,0.,0.,1.),q::create(-0.,0.,0.,0.),q::create(1e-12,0.,0.,0.),q::create(1e-12f64.next_up(),0.,0.,0.),q::create(0.2,-0.7,0.3,1.2)];
    for a in qs{check_q(q::normalize(a),call("quat_normalize",&[quat(a)]));for b in qs{check_q(q::mul(a,b),call("quat_mul",&[quat(a),quat(b)]));}}
}
#[test]
fn quaternion_overflow_signed_zero_and_native_thresholds(){
    let neighbors=|x:f64|[x.next_down(),x,x.next_up()];
    let mut samples=vec![-0.,0.,f64::from_bits(1),1e-12,1.,1e308];for x in [f64::MAX.sqrt(),2f64.powi(511),2f64.powi(512)]{samples.extend(neighbors(x));}
    for x in samples{for(y,z,w)in[(0.,0.,-0.),(x,-x,x),(0.,0.,-x),(x*0.5,0.,-x*0.5)]{let a=q::create(x,y,z,w);let expected=q::normalize(a);let actual=values(call("quat_normalize",&[quat(a)]));for(e,a)in[expected.x,expected.y,expected.z,expected.w].iter().zip(actual){if *e==0.{assert_eq!(e.to_bits(),a.to_bits())}else{assert!(distance(*e,a)<=4.,"{}: {e} vs {a}",quat(a_to_quat(x,y,z,w)));}}}}
    for left in neighbors(f64::MAX).into_iter().filter(|x|x.is_finite()){for right in neighbors(2f64.powi(970)){assert_eq!(evaluate(&format!("(>= (+ (* {} 0.25) (* {} 0.25)) 4.49423283715579e307)",num(left),num(right))).unwrap(),Value::Bool(!(left+right).is_finite()));}}
}
fn a_to_quat(x:f64,y:f64,z:f64,w:f64)->q::T{q::create(x,y,z,w)}
