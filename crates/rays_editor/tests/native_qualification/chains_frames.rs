//! All six eight-frame chain cases, including triangle tracing and device-computed normals.
use super::*;
use rays_pathtracer::rays_pathtracer as pt;
use rays_math::{vec3,color};
use rdk_rays::{surface,geometry_view,rays_mesh};
use std::rc::Rc;

struct Animation { lower:lower::T, lane:value_lane::Lane, session:sop::session::T, gpu:Option<Gpu::Config> }
impl Animation {
    fn new(text:&str,gpu:Option<Gpu::Config>)->Self { Self {lower:lowered(text,gpu.clone()),lane:value_lane::create(None),session:sop::session::create(64,1<<30).unwrap(),gpu} }
    fn cook(&mut self,frame:i64)->g::T {
        let time=frame as f64/60.; let live=input::at_time(time); let graph=&self.lower.graphs[0];
        let resolved=ok(value_lane::resolve(self.gpu.as_ref(),Some(&live),&self.lane,time,&graph.network));
        let node=sop::edit_graph::compile_node(&resolved.geometry,graph.root.unwrap()).unwrap();
        let context=sop::context::create(sop::context::Options {time:Some(time),input:Some(live),..Default::default()}).unwrap();
        sop::payload::geometry(&ok(sop::session::cook(&mut self.session,None,&context,&node)).payload).unwrap()
    }
}
impl Drop for Animation {fn drop(&mut self){sop::session::close(&mut self.session);}}
fn binding(gpu:&wg::t,g:&g::T)->Option<surface::DeviceBinding> {wg::display(gpu,g).map(|(k,n,i,c,f)|(k,n,i,c,Rc::from(f)))}
fn scene(g:&g::T,gpu:Option<&wg::t>)->rays::scene3::t {
    let surface=surface::of_geometry(None,g).unwrap();
    match gpu {Some(gpu)=>surface::scene3(None,Some(&|g|binding(gpu,g)),&surface).unwrap(),None=>surface::scene3(None,None,&surface).unwrap()}
}
fn picture(canvas:&rays::canvas::t,camera:&rays::camera::t,g:&g::T,gpu:Option<&wg::t>,world:Option<&rays::world::baked>)->(Vec<u8>,std::time::Duration) {
    let start=std::time::Instant::now();let scene=scene(g,gpu); let scene=world.map(|w|rays::scene3::with_world(w.clone(),&scene)).unwrap_or(scene);
    rays::canvas::render(None,canvas,&[rays::scene::clear(color::black()),rays::scene::view3d(None,camera,scene)]).unwrap();
    let elapsed=start.elapsed();(rays::canvas::pixels(canvas).unwrap().into_iter().flat_map(|c|[c.r as u8,c.g as u8,c.b as u8,c.a as u8]).collect(),elapsed)
}
fn mesh(scene:&rays::scene3::t)->rays::mesh::t {let mut m=None;rays::scene3::Walk::iter_batches(&mut |d,_|m=Some(d.mesh.clone()),scene);m.unwrap()}
fn trace(tracer:&mut Option<pt::t>,mesh:rays::mesh::t,camera:&rays::camera::t)->(Vec<u8>,std::time::Duration) {
    let start=std::time::Instant::now();let material=pt::material(None,None,None,None,pt::Linear_color::rgb(0.8,0.7,0.6));
    if let Some(t)=tracer {pt::replace_mesh(t,&pt::mesh(None,None,&[(mesh,material)]).unwrap()).unwrap();}
    else {*tracer=Some(pt::create(&Default::default(),128,128,&pt::scene {objects:vec![(mesh,material)],spheres:vec![],strands:vec![],environment:pt::environment {sky:pt::Linear_color::rgb(0.6,0.7,0.9),ground:pt::Linear_color::rgb(0.3,0.3,0.3),panels:vec![]},lights:vec![]}).unwrap());}
    let t=tracer.as_mut().unwrap();pt::reset(t);pt::render(t,camera).unwrap();pt::flush(t).unwrap();let elapsed=start.elapsed();(rays::image::pixels(&pt::image(t)).unwrap(),elapsed)
}
fn normal_check(cpu:&g::T,device:&g::T) {
    let topo=rdk_core::topology::View::view(g::topology(device)).unwrap();let positions=g::positions(device);let mut sums=vec![[0.;3];g::point_count(device) as usize];
    for prim in 0..topo.primitive_kinds.len() {
        let first=topo.primitive_offsets[prim] as usize;let last=topo.primitive_offsets[prim+1] as usize;
        let (ax,ay,az)=p::get(positions,topo.vertex_points[first]).unwrap();let mut sum=[0.;3];
        for j in first+1..last-1 {
            let (bx,by,bz)=p::get(positions,topo.vertex_points[j]).unwrap();let (cx,cy,cz)=p::get(positions,topo.vertex_points[j+1]).unwrap();
            let (ux,uy,uz)=(bx-ax,by-ay,bz-az);let (vx,vy,vz)=(cx-ax,cy-ay,cz-az);
            sum[0]+=uy*vz-uz*vy;sum[1]+=uz*vx-ux*vz;sum[2]+=ux*vy-uy*vx;
        }
        for j in first..last {let s=&mut sums[topo.vertex_points[j] as usize];for c in 0..3 {s[c]+=sum[c];}}
    }
    let cn=vec3(cpu,"N");let dn=vec3(device,"N");
    same(&cn,&dn,1e-4);
    for (i,s) in sums.iter().enumerate() {let l=(s[0]*s[0]+s[1]*s[1]+s[2]*s[2]).sqrt();let (x,y,z)=p::get(&dn,i as i64).unwrap();for (a,b) in [(s[0]/l,x),(s[1]/l,y),(s[2]/l,z)] {assert!((a-b).abs()<=1e-6);}}
}
pub(super) fn run() {
    assert!(crate::common::gpu(),"native GPU required");
    let cases=[
        (":offset [(get (frame/input) :t) 0.0 0.0]",":translate [0.0 1.0 0.0]","Points"),
        (":octaves 3 :lacunarity (+ 1.5 (get (frame/input) :t)) :roughness (+ 0.25 (* (get (frame/input) :t) 0.5))",":translate [0.0 1.0 0.0]","Points"),
        (":octaves (int (+ 2.0 (* (get (frame/input) :t) 60.0))) :roughness 0.6",":translate [0.0 1.0 0.0]","Points"),
        (":offset [0.5 0.0 0.0]",":translate [0.0 (get (frame/input) :t) 0.0] :shear_xy (* (get (frame/input) :t) 0.5)","Triangles"),
        (":offset [0.5 0.0 0.0]",":translate [0.0 (get (frame/input) :t) 0.0] :shear_xy (* (get (frame/input) :t) 0.5) :recompute_normals true","Triangles"),
        (":offset [0.5 0.0 0.0]",":translate [0.0 (get (frame/input) :t) 0.0] :shear_xy (* (get (frame/input) :t) 0.5)","Bare")];
    let override_case=std::env::var("DRIVE").ok().map(|d|(d,std::env::var("XDRIVE").unwrap_or_else(|_|":translate [0.0 1.0 0.0]".into()),std::env::var("CONNECTIVITY").unwrap_or_else(|_|"Points".into())));
    let cases=match override_case.as_ref(){Some((d,x,c))=>vec![(d.as_str(),x.as_str(),c.as_str())],None=>cases.to_vec()};
    for (drive,xdrive,connectivity) in cases {
        let bare=connectivity=="Bare";let triangles=connectivity!="Points";let recompute=xdrive.contains("recompute_normals");let connectivity=if triangles {"Triangles"}else{"Points"};let n=side();
        let grid=format!("(sop/grid :counts \"Point counts\" :connectivity \"{connectivity}\" :columns {n} :rows {n}) (sop/attribute_noise :name \"noise\" :seed 7 {drive})");
        let body=if recompute {format!("(let* [d (-> {grid} (sop/noise_displace :amplitude 0.2 :frequency 3.0)) n (sop/with_attr d \"N\" (exact (map (fn [(p : vec3)] [0.0 1.0 0.0]) (sop/attr d :P))) :owner \"point\" :type vec3)] (sop/transform n {xdrive}))")}
            else if bare {format!("(-> {grid} (sop/noise_displace :amplitude 0.2 :frequency 3.0) (sop/transform {xdrive}))")}
            else {format!("(-> {grid} (sop/transform {xdrive}))")};
        let text=format!("(workspace chain (graph g :context sop {body}))");let mut cpu=Animation::new(&text,None);
        let gpu=wg::create(None);wg::qualification(&gpu);
        wg::with_backend(&gpu,true,&mut |config|{
            let dispatch=Arc::new(AtomicUsize::new(0));let config=counted(config,&dispatch);let mut device=Animation::new(&text,Some(config));
            let canvas=rays::canvas::create(256,256).unwrap();let host_canvas=rays::canvas::create(256,256).unwrap();
            let camera=rays::camera::perspective(None,None,None,vec3::create(0.4,2.2,1.3),vec3::create(0.,1.,0.)).unwrap();
            let world=if triangles {let mut w=rays::world::bake(Some(1),None,64,32,&rays::world::default()).unwrap();w.sun=Some(rays::world::sun {direction:vec3::normalize(vec3::create(0.3,1.,0.2)),radiance:rays::world::rgb(4.,4.,4.),angular_radius:0.01});Some(w)}else{None};
            let mut device_tracer=None;let mut host_tracer=None;
            let mut gpu_times=Vec::new();let mut cpu_times=Vec::new();let mut display_times=Vec::new();let mut host_times=Vec::new();let mut trace_times=Vec::new();let mut host_trace_times=Vec::new();
            for frame in 0..8 {
                let before=wg::image_stats(&gpu).0;let calls=dispatch.load(Ordering::SeqCst);let start=std::time::Instant::now();let d=device.cook(frame);if frame>0 {gpu_times.push(start.elapsed());}let after=wg::image_stats(&gpu).0;
                if bare {assert!(g::find_attribute(a::Owner::Point,"N",&d).is_none());}
                if frame==0 && !recompute && !bare {assert_eq!(after.input_uploaded_bytes-before.input_uploaded_bytes,n*n*12);assert_eq!(dispatch.load(Ordering::SeqCst)-calls,1);}
                if frame>0 {assert_eq!(after.pipeline_compilations,before.pipeline_compilations);assert_eq!(after.input_uploads,before.input_uploads);assert_eq!(dispatch.load(Ordering::SeqCst)-calls,if recompute {2}else{1});}
                assert_eq!(after.readback_bytes,before.readback_bytes);assert_eq!(after.materialisations,before.materialisations);assert!(p::resident(g::positions(&d)));
                let bound=wg::displays(&gpu);let (shown,seconds)=picture(&canvas,&camera,&d,Some(&gpu),None);if frame>0 {display_times.push(seconds);}let drawn=wg::image_stats(&gpu).0;
                assert_eq!(wg::displays(&gpu),bound+1);assert_eq!(drawn.readback_bytes,after.readback_bytes);assert_eq!(drawn.materialisations,after.materialisations);assert!(p::resident(g::positions(&d)));
                let bounds=geometry_view::bounds(&d);let boxed=wg::image_stats(&gpu).0;assert!(boxed.readback_bytes-drawn.readback_bytes<=24576);assert_eq!(boxed.materialisations,drawn.materialisations);assert!(p::resident(g::positions(&d)));
                let sunlit=world.as_ref().map(|w|{let (pixels,_)=picture(&canvas,&camera,&d,Some(&gpu),Some(w));let stats=wg::image_stats(&gpu).0;assert!(stats.readback_bytes-boxed.readback_bytes<=24576);assert_eq!(stats.materialisations,boxed.materialisations);assert!(p::resident(g::positions(&d)));assert_ne!(pixels,shown);pixels});
                if triangles {
                    let mesh=mesh(&scene(&d,Some(&gpu)));assert!(rays::mesh::View::device(&mesh).is_some());let before=wg::image_stats(&gpu).0;
                    let (dp,seconds)=trace(&mut device_tracer,mesh.clone(),&camera);if frame>0 {trace_times.push(seconds);}assert_eq!(pt::Private::refits(device_tracer.as_ref().unwrap()),frame);assert!(dp.iter().enumerate().filter(|(i,c)|i%4!=3&&**c>8).count()>=dp.len()/8);
                    let after=wg::image_stats(&gpu).0;assert_eq!(after.readback_bytes,before.readback_bytes);assert_eq!(after.materialisations,before.materialisations);assert!(p::resident(g::positions(&d)));
                    if !bare {let host=rays_mesh::to_trace_mesh(&rays_mesh::of_mesh(&mesh).unwrap()).unwrap();let (hp,seconds)=trace(&mut host_tracer,host,&camera);assert_eq!(dp,hp);if frame>0 {host_trace_times.push(seconds);}}
                }
                let (host,seconds)=picture(&host_canvas,&camera,&d,None,None);if frame>0 {host_times.push(seconds);}let tolerance=if bare {1}else{0};assert_eq!(shown.len(),host.len());for (a,b) in shown.iter().zip(&host) {assert!((*a as i32-*b as i32).abs()<=tolerance);}
                assert!(shown.chunks_exact(4).any(|p|p!=[0,0,0,255]));
                if let (Some(w),Some(lit))=(&world,sunlit) {let (host,_)=picture(&host_canvas,&camera,&d,None,Some(w));for (a,b) in lit.iter().zip(host) {assert!((*a as i32-b as i32).abs()<=tolerance);}}
                let mut lo=[f64::INFINITY;3];let mut hi=[f64::NEG_INFINITY;3];for i in 0..n*n {let (x,y,z)=p::get(g::positions(&d),i).unwrap();for (c,v) in [x,y,z].into_iter().enumerate(){lo[c]=lo[c].min(v);hi[c]=hi[c].max(v);}}
                assert_eq!(bounds,Some((vec3::create(lo[0],lo[1],lo[2]),vec3::create(hi[0],hi[1],hi[2]))));
                let start=std::time::Instant::now();let c=cpu.cook(frame);if frame>0 {cpu_times.push(start.elapsed());}same(g::positions(&c),g::positions(&d),1e-5);let noise=|g:&g::T|match a::storage(&g::find_attribute(a::Owner::Point,"noise",g).unwrap()).unwrap(){a::Storage::Float(v)=>v,_=>panic!("noise")};for (a,b) in noise(&c).iter().zip(noise(&d)){assert!((a-b).abs()<=1e-5);}
                if recompute {normal_check(&c,&d);}
                wg::frame(&gpu,None);
            }
            for (name,times) in [("CPU cook",&mut cpu_times),("GPU cook",&mut gpu_times),("device display",&mut display_times),("host display",&mut host_times),("device trace",&mut trace_times),("host trace",&mut host_trace_times)] {if !times.is_empty(){times.sort();println!("{drive} {xdrive}: {name} median {:?}",times[times.len()/2]);}}
            if let Some(t)=&mut device_tracer {pt::destroy(t);}if let Some(t)=&mut host_tracer {pt::destroy(t);}drop(device_tracer);drop(host_tracer);
            assert!(wg::held_displays(&gpu)>0);for _ in 0..2 {wg::frame(&gpu,Some(&[]));}assert_eq!(wg::held_displays(&gpu),0);drop(device);wg::frame(&gpu,None);assert_eq!(wg::image_stats(&gpu).0.held_buffers,0);
            rays::canvas::destroy(&canvas).unwrap();rays::canvas::destroy(&host_canvas).unwrap();
        });wg::close(&gpu);
    }
}
