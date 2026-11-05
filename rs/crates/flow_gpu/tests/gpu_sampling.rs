//! Port of lib/flow_gpu/test_sampling.ml: independent CPU arithmetic and native GPU parity.
mod common;
#[path = "common/pathtrace_reference.rs"]
mod reference_math;
use reference_math::*;
use common::test_program as tp;
use flow_gpu::{pipelines, run};
use flow_ir::packed::{self, host as phost};
use flow_shader::emit;
use flow_syntax::value::Value;
const N: usize = 4096;
const HELPERS: &str = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/../../../lib/rays_pathtracer/pathtrace_helpers.rays"));
const PRELUDE: &str = r#"[seed (pixel-seed (int/mod i 1000) (int/div i 1000))
 a (pcg-next seed) b (pcg-next a.next) c (pcg-next b.next) d (pcg-next c.next)
 r1 (unit-float a.value) r2 (unit-float b.value) r3 (unit-float c.value) r4 (unit-float d.value)
 eye [(- (* r1 4.) 2.) (* r2 2.) (+ 5. r3)] fwd (normalize [(- r2 0.5) (- r3 0.5) -1.])
 rgt (normalize (cross fwd [0. 1. 0.])) upv (cross rgt fwd)
 tan-half (+ 0.4 (* r4 0.3)) aspect (+ 1. (* r1 0.5)) lens (if (> r2 0.5) (* 0.1 r3) 0.) focus (+ 3. r4)
 cr (camera-ray eye lens fwd rgt upv tan-half aspect focus (- (* r2 2.) 1.) (- (* r3 2.) 1.) r4 r1)
 ns (normalize [(- (* r1 2.) 1.) (- (* r2 2.) 1.) (+ r3 0.1)]) v (normalize [r2 r3 (+ r1 0.2)])
 a2 (+ 0.01 (* r4 0.5)) ps (+ 0.05 (* r1 0.9)) bsm (bsdf-sample ns v ps a2 r1 r2 r3)
 ls (light-sample [(- (* r1 2.) 1.) (- (* r2 2.) 1.) 3.] [1. r3 0.] [0. 1. r4] (if (> r4 0.5) 1. 0.) [0. 0. 0.] r2 r3)]"#;
fn vec4(v: Value) -> Vec<f64> { match v { Value::Vec4_array(a) => (*a).clone(), _ => panic!("vec4 array expected") } }
fn sampling() {
    let bodies = [
        "[(float (int/and seed 16777215)) (float (int/and (int/shr seed 8) 16777215)) (+ cr.o.x (* 0. t)) cr.d.x]",
        "[cr.o.y cr.o.z cr.d.y (+ cr.d.z (* 0. t))]",
        "[bsm.l.x bsm.l.y bsm.l.z (+ bsm.h.x (* 0. t))]",
        "[bsm.h.y bsm.h.z ls.dist (+ ls.cosl (* 0. t))]",
        "[ls.wi.x ls.wi.y ls.wi.z (+ ls.dist2 (* 0. t))]",
    ];
    let live = input::at_time(1.0);
    let programs: Vec<_> = bodies.iter().map(|body| tp::compile_with(None,true,HELPERS,&format!("(map (fn [(i : int)] (let* {PRELUDE} {body})) (array/range {N} :type int))"))).collect();
    let cpu: Vec<_> = programs.iter().map(|p| vec4(tp::ok(packed::force(p,&live,&Default::default())))).collect();
    for i in 0..N {
        let state = (i as u32%1000+1).wrapping_mul(0x9e3779b9) ^ (i as u32/1000+1).wrapping_mul(0x85ebca6b);
        let seed = pcg(pcg(state).0).0;
        let check = |name,k: usize,c: usize,b| close(name,cpu[k][4*i+c],b,1e-9);
        check("seed",0,0,(seed&0xffffff) as f64); check("seed",0,1,((seed>>8)&0xffffff) as f64);
        let (_, [r1,r2,r3,r4]) = randoms(seed);
        let eye = [r1*4.0-2.0,r2*2.0,5.0+r3];
        let fwd = normalize([r2-0.5,r3-0.5,-1.0]);
        let rgt = normalize(cross(fwd,[0.0,1.0,0.0])); let up = cross(rgt,fwd);
        let tan = 0.4+r4*0.3; let aspect = 1.0+r1*0.5; let focus = 3.0+r4;
        let lens = if r2>0.5 {0.1*r3} else {0.0};
        let d = normalize(add(fwd,add(scale((r2*2.0-1.0)*(tan*aspect),rgt),scale((r3*2.0-1.0)*tan,up))));
        let aim = add(eye,scale(focus/dot(d,fwd),d));
        let radius = lens*r4.sqrt(); let angle = 6.28318530717959*r1;
        let o = add(eye,add(scale(radius*angle.cos(),rgt),scale(radius*angle.sin(),up)));
        let (o,d) = if lens>0.0 {(o,normalize(sub(aim,o)))} else {(eye,d)};
        check("camera o.x",0,2,o[0]); check("camera d.x",0,3,d[0]);
        check("camera o.y",1,0,o[1]); check("camera o.z",1,1,o[2]); check("camera d.y",1,2,d[1]); check("camera d.z",1,3,d[2]);
        let ns = normalize([r1*2.0-1.0,r2*2.0-1.0,r3+0.1]); let v = normalize([r2,r3,r1+0.2]);
        let a2 = 0.01+r4*0.5; let ps = 0.05+r1*0.9; let (bt,bb) = basis(ns); let phi = 6.28318530717959*r1;
        let (l,h) = if r3<ps {
            let ct = ((1.0-r2)/(1.0+(a2-1.0)*r2)).sqrt(); let st = (1.0-ct*ct).max(0.0).sqrt();
            let h = normalize(add(scale(st*phi.cos(),bt),add(scale(st*phi.sin(),bb),scale(ct,ns))));
            (sub(scale(2.0*dot(v,h),h),v),h)
        } else {
            let rr = r2.sqrt(); let l = normalize(add(scale(rr*phi.cos(),bt),add(scale(rr*phi.sin(),bb),scale((1.0-r2).max(0.0).sqrt(),ns))));
            (l,normalize(add(l,v)))
        };
        for c in 0..3 { check("bsdf l",2,c,l[c]); }
        check("bsdf h.x",2,3,h[0]); check("bsdf h.y",3,0,h[1]); check("bsdf h.z",3,1,h[2]);
        let lo = [r1*2.0-1.0,r2*2.0-1.0,3.0]; let lu = [1.0,r3,0.0]; let lv = [0.0,1.0,r4];
        let lp = add(lo,add(scale(r2,lu),scale(r3,lv))); let ln = normalize(cross(lu,lv));
        let dist2 = dot(lp,lp).max(1e-8); let dist = dist2.sqrt(); let w = scale(1.0/dist,lp); let cl = dot(ln,w);
        check("light dist",3,2,dist); check("light cos",3,3,if r4>0.5 {-cl} else {cl.abs()});
        for c in 0..3 { check("light wi",4,c,w[c]); } check("light dist2",4,3,dist2);
    }
    if !common::gpu() { return; }
    let lease = common::lease(); let pipes = pipelines::create(&||0.0,&renderer::renderer::gpu_device(&lease));
    for (k,p) in programs.iter().enumerate() {
        let runner = run::create(None,&lease,&pipes,&tp::ok(emit::kernel(&emit::Options {fast:true,..Default::default()},p)));
        let values = tp::ok(phost::prepare(p,&phost::PrepareOptions {fast:true,..Default::default()},&live));
        let gpu = vec4(tp::ok(run::readback(&tp::ok(run::dispatch(&runner,&Default::default(),None,None,&values)),None)));
        assert_eq!(gpu.len(),cpu[k].len());
        let worst = gpu.iter().zip(&cpu[k]).fold(0.0f64,|m,(v,e)|m.max((v-e).abs()/e.abs().max(1.0)));
        println!("pathtrace sampling kernel {k}: {N} rows, GPU vs CPU max relative error {worst:.3e}");
        assert!(worst<=2e-3,"kernel {k}: {worst}"); run::close(&runner,false);
    }
    pipelines::close(&pipes);
}
fn main() { common::window::run(&[("sampling",sampling)]); }
