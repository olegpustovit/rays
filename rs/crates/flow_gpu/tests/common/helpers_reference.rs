use super::reference_math::*;
use std::f64::consts::PI;
pub fn check(cpu: &[Vec<f64>]) {
    for i in 0..4096 {
        let (w, [r1,r2,r3,r4]) = randoms((i as u32+1).wrapping_mul(2654435761));
        let at = |k: usize,c: usize| cpu[k][4*i+c];
        let check = |name,k,c,b| close(name,at(k,c),b,1e-9);
        let a2 = 0.01+r4*0.5;
        close("pcg",at(0,0),(w&0xffffff) as f64,0.0);
        check("d_ggx",0,1,d_ggx(r1,a2));
        check("v_smith",0,2,v_smith(r2,r3,a2));
        let ns = normalize([r1*2.0-1.0,r2*2.0-1.0,r3+0.1]);
        let v = normalize([r2,r3,r1+0.2]);
        let l = normalize([r3,r1,r2+0.2]);
        let h = normalize(add(l,v));
        let nol = dot(ns,l).max(0.0);
        let noh = dot(ns,h).max(0.0);
        let voh = dot(v,h).max(1e-4);
        check("bsdf_pdf",1,3,r4*(d_ggx(noh,a2)*noh/(4.0*voh))+(1.0-r4)*nol/PI);
        check("fresnel",1,0,r1+(1.0-r1)*(1.0-r4).powi(5));
        let (bt,bb) = basis(ns);
        let df = [r1,r2,r3];
        let f0 = [r4*0.5,r3*0.5,r2*0.5];
        for c in 0..3 {
            let f = f0[c]+(1.0-f0[c])*(1.0-voh).powi(5);
            check("eval_brdf",2,c,df[c]*(1.0-f)/PI+d_ggx(noh,a2)*v_smith(dot(ns,v).max(1e-4),dot(ns,l).max(1e-4),a2)*f);
            check("basis",3,c,bt[c]);
        }
        check("power_weight",2,3,1.0/(1.0+(r2/r1).powi(2)));
        check("basis",3,3,bb[0]); check("basis",4,0,bb[1]); check("basis",4,1,bb[2]);
        let lo = [r1*2.0-1.0,r2*2.0-1.0,3.0];
        let lu = [1.0,r3,0.0]; let lv = [0.0,1.0,r4];
        let ray = normalize([r1-0.5,r2-0.5,1.0]);
        let n = cross(lu,lv); let dn = dot(ray,n); let nn = dot(n,n);
        let t = dot(lo,n)/dn; let p = sub(scale(t,ray),lo);
        let ra = dot(cross(p,lv),n)/nn; let rb = dot(cross(lu,p),n)/nn;
        let c = -dn/nn.sqrt(); let cosl = if r4>0.5 {c} else {c.abs()};
        let ok = dn.abs()>=1e-12 && nn>0.0 && t>1e-4 && t<10.0 && ra>=0.0 && ra<=1.0 && rb>=0.0 && rb<=1.0 && cosl>1e-4;
        check("rect_hit",4,2,if ok {t} else {-1.0}); check("rect_cos",4,3,cosl);
        let u0 = ns[0].atan2(-ns[2])/(2.0*PI)+0.5;
        check("world_uv",5,0,if u0>=1.0 {u0-1.0} else {u0});
        check("world_uv",5,1,ns[1].max(-1.0).min(1.0).acos()/PI);
        let phi = (r1-0.5)*2.0*PI; let theta = r2*PI;
        check("world_dir",5,2,theta.sin()*phi.sin()); check("world_dir",5,3,-theta.sin()*phi.cos());
    }
}
