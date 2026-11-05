//! Real-device dynamic topology, state trails and editor-holder qualification.
use flow_native::workspace_gpu as wg;
use flow_ir::flow_ir::Gpu;
use flow_sop::{lower,value_lane};
use rdk::{geometry as g, attribute as a, packed::Float3 as p};
use std::{sync::{Arc,Weak},rc::Rc};
fn ok<T>(r:Result<T,rays_diagnostic::Diagnostic>)->T { r.unwrap_or_else(|d|panic!("{}",rays_diagnostic::to_string(&d))) }
const MESH: &str = r###"(workspace live_mesh
  (defn cell :context value [(index : int) (side : int)]
    [(int/div index (* side side)) (mod (int/div index side) side) (mod index side)])
  (defn occupied :context value [(voxels : (array int)) (side : int) (p : vec3)]
    (if (and (and (>= p.x 0) (< p.x side))
             (and (and (>= p.y 0) (< p.y side)) (and (>= p.z 0) (< p.z side))))
        (array/nth voxels (+ (+ (* (int p.x) (* side side)) (* (int p.y) side)) (int p.z)))
        0))
  (graph city :context sop [(side : int %d)]
    (let* [clock (* t 6.0)
           voxels (map (fn [(index : int)]
                         (let* [p (cell index side)]
                           (if (< (mod (+ p.x (+ p.y p.z)) 7) (+ 2 (mod clock 3))) 1 0)))
                       (array/range (* (* side side) side) :type int))
           directions (list [1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1])
           corners (list [1 0 0] [1 1 0] [1 1 1] [1 0 1]
                         [0 0 1] [0 1 1] [0 1 0] [0 0 0]
                         [0 1 1] [1 1 1] [1 1 0] [0 1 0]
                         [0 0 0] [1 0 0] [1 0 1] [0 0 1]
                         [1 0 1] [1 1 1] [0 1 1] [0 0 1]
                         [0 0 0] [0 1 0] [1 1 0] [1 0 0])
           faces (array/compact
                   (map (fn [(face : int)]
                          (let* [index (int/div face 6)
                                 p (cell index side)]
                            (if (and (> (array/nth voxels index) 0)
                                     (= (occupied voxels side (+ p (nth directions (mod face 6)))) 0)) 1 0)))
                        (array/range (* (* side side) (* side 6)) :type int)))
           face_count (array/count faces)
           vertices (array/range (* face_count 4) :type int)
           positions (map (fn [(vertex : int)]
                            (let* [face (array/nth faces (int/div vertex 4))
                                   p (cell (int/div face 6) side)]
                              (+ p (nth corners (+ (* (mod face 6) 4) (mod vertex 4)))))) vertices)
           normals (map (fn [(vertex : int)]
                          (nth directions (mod (array/nth faces (int/div vertex 4)) 6))) vertices)
           colors (map (fn [(vertex : int)]
                         (let* [face (array/nth faces (int/div vertex 4))]
                           [(/ (array/nth voxels (int/div face 6)) 2.0) 0.5 0.25])) vertices)
           mesh (sop/mesh :points (exact positions) :vertex_points (exact vertices)
                          :primitive_offsets (exact (map (fn [(face : int)] (* face 4)) (array/range (+ face_count 1) :type int)))
                          :kinds (exact (array/int face_count 0)))
           shaded (sop/with_attr mesh "N" (exact normals) :owner "point" :type vec3)]
      (sop/with_attr shaded "Cd" (exact colors) :owner "point" :type vec3))))"###;
const HISTORY: &str = r###"(workspace history
  (graph g :context sop
    (let* [heads 16384 passes 6
           starts (map (fn [(id : int)] [(float id) 0.0 (floor (* t 60.0))]) (array/range heads :type int))
           trail (iterate passes {:head starts :points starts}
                   (fn [(walk : {:head (array vec3) :points (array vec3)})]
                     (let* [head (map (fn [(p : vec3)] (+ p [0.0 1.0 0.5])) walk.head)]
                       {:head head :points (array/concat walk.points head)})))
           count (array/count trail.points)
           triangles (int/div count 3)]
      (sop/mesh :points (exact trail.points) :vertex_points (exact (array/range (* triangles 3) :type int))
                :primitive_offsets (exact (map (fn [(f : int)] (* f 3)) (array/range (+ triangles 1) :type int)))
                :kinds (exact (array/int triangles 0))))))"###;
const TRAIL: &str = r###"(workspace trail
  (graph g :context sop
    (let* [heads 16384 count (* heads 8)
           trail (state [previous (exact (map (fn [(k : int)] [(float (int/mod k heads)) 0.0 0.0])
                                              (array/range count :type int)))]
                   (map (fn [(k : int)]
                          (if (< k heads) (+ (array/nth previous k) [0.0 1.0 0.5])
                              (array/nth previous (int/sub k heads))))
                        (array/range count :type int)))
           triangles (int/div count 3)]
      (sop/mesh :points (exact (map (fn [(p : vec3)] (* p 2.0)) trail)) :vertex_points (exact (array/range (* triangles 3) :type int))
                :primitive_offsets (exact (map (fn [(f : int)] (* f 3)) (array/range (+ triangles 1) :type int)))
                :kinds (exact (array/int triangles 0))))))"###;

pub fn cases()->Vec<(&'static str,fn())> {
    vec![("native_live_mesh",live_mesh),("native_holder_frames",holder_frames),("native_orrery_frames",orrery_frames)]
}
fn text(side:i64)->String { MESH.replace("%d",&side.to_string()) }
fn lower_text(text:&str,gpu:Option<Gpu::Config>,defer:bool)->lower::T {
    ok(lower::workspace(None,editor_document::kinds::descriptors(),lower::Options {gpu:gpu.unwrap_or_else(Gpu::default),defer,..Default::default()},&ok(flow_syntax::syntax::parse(text))))
}
fn cook(text:&str,gpu:Option<Gpu::Config>,times:&[f64],graph_name:Option<&str>,defer:bool,mut each:impl FnMut(usize,&g::T)) {
    let l=lower_text(text,gpu.clone(),defer);
    let graph=graph_name.map(|name|l.graphs.iter().find(|g|g.name==name).expect(name)).unwrap_or(&l.graphs[0]);
    let lane=value_lane::create(None); let mut session=sop::session::create(64,1<<30).unwrap();
    for (frame,&time) in times.iter().enumerate() {
        let live=input::Frame {dt:1./60.,frame:frame as i64,tick:frame as i64,..input::at_time(time)};
        let resolved=ok(value_lane::resolve(gpu.as_ref(),Some(&live),&lane,time,&graph.network));
        let node=sop::edit_graph::compile_node(&resolved.geometry,graph.root.unwrap()).unwrap();
        let context=sop::context::create(sop::context::Options {time:Some(time),frame:Some(frame as i64),input:Some(live),..Default::default()}).unwrap();
        let out=ok(sop::session::cook(&mut session,Some(0),&context,&node));
        each(frame,&sop::payload::geometry(&out.payload).unwrap());
    }
    sop::session::close(&mut session);
}
fn attribute(geo:&g::T,name:&str)->a::T { g::find_attribute(a::Owner::Point,name,geo).expect(name) }
fn vec3(geo:&g::T,name:&str)->p::T {
    match a::storage(&attribute(geo,name)).unwrap() { a::Storage::Float3(v)=>v,_=>panic!("{name} not vec3") }
}
fn same(a:&p::T,b:&p::T) {
    assert_eq!(p::length(a),p::length(b));
    for i in 0..p::length(a) {
        let (x,y,z)=p::get(a,i).unwrap(); let (u,v,w)=p::get(b,i).unwrap();
        assert!((x-u).abs()<=1e-6 && (y-v).abs()<=1e-6 && (z-w).abs()<=1e-6,"point {i}");
    }
}
fn binding(gpu:&wg::t,geo:&g::T)->Option<rdk_rays::surface::DeviceBinding> {
    wg::display(gpu,geo).map(|(key,n,indices,color,keep)|(key,n,indices,color,Rc::from(keep)))
}
fn resident(gpu:&wg::t,config:&Gpu::Config,side:i64,times:&[f64],compare:bool,displayed:bool) {
    let text=text(side); let mut cpu=Vec::new();
    if compare { cook(&text,None,times,None,false,|_,g|cpu.push((g::positions(g).clone(),vec3(g,"N"),vec3(g,"Cd"),g::primitive_count(g)))); }
    let mut uploads=0;
    cook(&text,Some(config.clone()),times,None,false,|frame,g|{
        assert!(p::resident(g::positions(g)),"side {side} frame {frame}: P left device");
        for name in ["N","Cd"] { assert!(a::holder(&attribute(g,name)).is_some(),"{name} left device"); }
        let stats=wg::image_stats(gpu).0;
        if frame>0 { assert_eq!(stats.input_uploads,uploads); } uploads=stats.input_uploads;
        if displayed { assert!(g::point_count(g)>4_000_000); assert!(wg::display(gpu,g).is_some(),"{:?}",wg::display_refusal(gpu)); }
        if compare { let (p,n,cd,count)=&cpu[frame]; assert_eq!(g::primitive_count(g),*count); same(g::positions(g),p); same(&vec3(g,"N"),n); same(&vec3(g,"Cd"),cd); }
    });
}
fn traced(gpu:&wg::t,config:&Gpu::Config) {
    use rays_pathtracer::rays_pathtracer as pt;
    let camera=rays::camera::perspective(None,None,None,rays_math::vec3::create(20.,18.,22.),rays_math::vec3::create(4.,4.,4.)).unwrap();
    let picture=|mesh:rays::mesh::t| {
        let scene=pt::scene { objects:vec![(mesh,pt::material(None,None,None,None,pt::Linear_color::rgb(1.,1.,1.)))],spheres:Vec::new(),strands:Vec::new(),environment:pt::environment {sky:pt::Linear_color::rgb(0.5,0.5,0.5),ground:pt::Linear_color::rgb(0.5,0.5,0.5),panels:Vec::new()},lights:Vec::new() };
        let mut tracer=pt::create(&Default::default(),64,64,&scene).unwrap();
        pt::reset(&mut tracer);
        for _ in 0..16 { pt::render(&mut tracer,&camera).unwrap(); pt::flush(&mut tracer).unwrap(); }
        let bytes=rays::image::pixels(&pt::image(&tracer)).unwrap(); pt::destroy(&mut tracer); bytes
    };
    let mut host=None;
    cook(&text(8),None,&[1./6.],None,false,|_,g|host=Some(picture(rdk_rays::rays_mesh::to_trace_mesh(g).unwrap())));
    let qualified=Gpu::Config {policy:Gpu::Policy::Qualification,..config.clone()};
    cook(&text(8),Some(qualified),&[1./6.],None,false,|_,g|{
        let surface=rdk_rays::surface::of_geometry(None,g).unwrap();
        let scene=rdk_rays::surface::scene3(None,Some(&|g|binding(gpu,g)),&surface).unwrap();
        let mut mesh=None; rays::scene3::Walk::iter_batches(&mut |d,_|mesh=Some(d.mesh.clone()),&scene);
        let mesh=mesh.unwrap(); assert!(rays::mesh::View::device(&mesh).is_some(),"{:?}",wg::display_refusal(gpu));
        let before=wg::image_stats(gpu).0.payload_readback_bytes; let device=picture(mesh); assert_eq!(wg::image_stats(gpu).0.payload_readback_bytes,before);
        let host=host.as_ref().unwrap(); let mean=|p:&[u8],c:usize|p.chunks_exact(4).map(|p|p[c] as f64).sum::<f64>()/(p.len()/4) as f64;
        for route in [&device,host] { assert!(mean(route,0)-mean(route,2)>=2.); }
        for c in 0..3 { assert!((mean(&device,c)-mean(host,c)).abs()<=2.); }
    });
}
fn live_mesh() {
    assert!(super::common::gpu(),"native GPU required");
    let gpu=wg::create(None);
    wg::with_backend(&gpu,true,&mut |config|{
        resident(&gpu,config,8,&[0.,1./6.],true,false);
        traced(&gpu,config);
        assert!(flow_ir::fallback::seen().is_empty(),"{:?}",flow_ir::fallback::seen());
        let broken=text(16).replace("(mod vertex 4)","(mod vertex (+ 4 (* 0 (array/count vertices))))");
        cook(&broken,Some(config.clone()),&[0.],None,false,|_,_|{});
        assert!(flow_ir::fallback::seen().iter().any(|(site,reason,_)|site.ends_with("positions")&&!reason.is_empty()));
        flow_ir::fallback::reset();
        let sea=std::fs::read_to_string(concat!(env!("CARGO_MANIFEST_DIR"),"/../../../sketches/gpu_sea.rays")).unwrap().replace("(side : int 1000)","(side : int 256)");
        let before=wg::image_stats(&gpu).0; let mut uploads=0;
        cook(&sea,Some(config.clone()),&(0..=6).map(|i|i as f64/60.).collect::<Vec<_>>(),Some("sea"),true,|frame,g|{
            if frame>=3 { assert!(p::resident(g::positions(g))); assert!(a::holder(&attribute(g,"N")).is_some()); let now=wg::image_stats(&gpu).0.input_uploads; if frame>3 { assert_eq!(now,uploads); } uploads=now; }
        });
        assert_eq!(wg::image_stats(&gpu).0.payload_readback_bytes,before.payload_readback_bytes);
        assert!(flow_ir::fallback::seen().is_empty(),"{:?}",flow_ir::fallback::seen());
        for (source,frames,threshold) in [(HISTORY,5,3),(TRAIL,7,4)] {
            let times=(0..frames).map(|i|i as f64/60.).collect::<Vec<_>>(); let mut cpu=Vec::new();
            cook(source,None,&times,None,false,|_,g|cpu.push(g::positions(g).clone()));
            cook(source,Some(config.clone()),&times,None,false,|frame,g| { if frame>=threshold {assert!(p::resident(g::positions(g)));} same(g::positions(g),&cpu[frame]); });
        }
        let root=std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../../sketches");
        let swarm=std::fs::read_to_string(root.join("swarm.rays")).unwrap().replace("(import \"swarm_lib.rays\")","").replacen("(workspace swarm",&format!("(workspace swarm\n{}",std::fs::read_to_string(root.join("swarm_lib.rays")).unwrap()),1).replace("(tubes : int 4000) (points : int 257)","(tubes : int 300) (points : int 32)");
        flow_ir::fallback::reset();
        cook(&swarm,Some(config.clone()),&(0..=5).map(|i|i as f64/60.).collect::<Vec<_>>(),Some("swirling"),true,|frame,g|{
            if frame>=3 { assert!(p::resident(g::positions(g))); assert!(wg::display(&gpu,g).is_some(),"{:?}",wg::display_refusal(&gpu)); }
        });
        assert!(flow_ir::fallback::seen().is_empty(),"{:?}",flow_ir::fallback::seen());
        resident(&gpu,config,128,&[0.,0.01,0.02,0.03,0.04],false,true);
    });
    wg::close(&gpu);
}
fn holder_frames() {
    use rays_editor::{Editor,Workspace};
    assert!(super::common::gpu(),"native GPU required");
    for (name,side,topology) in [("gpu_sea",128,false),("isogrid_city",8,true)] {
        let source=std::fs::read_to_string(format!("{}/../../../sketches/{name}.rays",env!("CARGO_MANIFEST_DIR"))).unwrap();
        // Replace the numeric side default only; keep the sketch itself and all six frames.
        let start=source.find("(side : int ").unwrap()+12; let end=start+source[start..].find(')').unwrap();
        let source=format!("{}{}{}",&source[..start],side,&source[end..]);
        let doc=Workspace::load(None,Vec::new(),None,&source).unwrap_or_else(|d|panic!("{d:?}"));
        let canvas=rays::canvas::create(320,240).unwrap(); let gpu=wg::create(None); wg::qualification(&gpu);
        let mut editor=Editor::create(Editor::Create {gpu_owner:Some(gpu.clone()),ui_text_size:Some(11),await_:Some(true),domains:Some(1),..Default::default()},&doc,None,Editor::prepare_fn(),Editor::draw_fn()).unwrap();
        let mut previous:Option<(Arc<Vec<f64>>,Option<[Weak<Gpu::Integers>;3]>)>=None; let mut gone=0; let mut meshes=0;
        for tick in 1..=6 {
            let frame=rays::frame::t {input:input::Frame {dt:1./60.,frame:tick,tick,size:(320,240),pointer:(-100.,-100.),..input::at_time(tick as f64/60.)},pixel_scale:(1.,1.),fps:60.,mouse_delta:(0.,0.)};
            editor=Editor::update(editor,&frame); rays::canvas::render(Some(1),&canvas,&Editor::scene(&editor,&frame)).unwrap(); editor=Editor::after_present(editor,&frame);
            let now=editor.core.cook.pieces.iter().find_map(|piece|{
                let g=&piece.prepared.geometry;
                flow_sop::attribute_kernel::resident_vec3(g,"P").map(|p|(p,flow_sop::topology_kernel::resident(g).map(|(a,b,c)|[Arc::downgrade(&a),Arc::downgrade(&b),Arc::downgrade(&c)])))
            });
            if let (Some((old,streams)),Some((current,_)))=(&previous,&now) {
                if !Arc::ptr_eq(old,current) {
                    assert!(flow_gpu::run::pin(old).is_none(),"{name} frame {tick}: old P retained"); gone+=1;
                    if let Some(streams)=streams { meshes+=1; assert!(streams.iter().all(|s|s.upgrade().is_none()),"{name} frame {tick}: old topology retained"); }
                }
            }
            if now.is_some() {previous=now;}
        }
        Editor::close(&editor); wg::close(&gpu); rays::canvas::destroy(&canvas).unwrap(); assert!(gone>=3,"{name}: only {gone} resident frames"); if topology {assert!(meshes>=3);}
    }
}

fn orrery_frames() {
    use rays_editor::{Editor,Workspace,Timeline};
    assert!(super::common::gpu(),"native GPU required");
    let source=std::fs::read_to_string(concat!(env!("CARGO_MANIFEST_DIR"),"/../../../sketches/ws_orrery.rays")).unwrap();
    let doc=Workspace::load(None,Vec::new(),None,&source).unwrap_or_else(|d|panic!("{d:?}"));
    let canvas=rays::canvas::create(320,240).unwrap(); let gpu=wg::create(None);
    let mut editor=Editor::create(Editor::Create {gpu_owner:Some(gpu.clone()),ui_text_size:Some(11),await_:Some(true),domains:Some(1),..Default::default()},&doc,None,Editor::prepare_fn(),Editor::draw_fn()).unwrap();
    let frame=|tick|rays::frame::t {input:input::Frame {dt:1./60.,frame:tick,tick,size:(320,240),pointer:(-100.,-100.),..input::at_time(tick as f64/60.)},pixel_scale:(1.,1.),fps:60.,mouse_delta:(0.,0.)};
    let document=editor.core.doc.clone(); let mut first=None;
    for tick in 1..=30 {
        let f=frame(tick); editor=Editor::update(editor,&f);
        assert!(editor.core.cook.error.is_none(),"{:?}",editor.core.cook.error);
        assert!(Arc::ptr_eq(&document,&editor.core.doc),"playback re-lowered the document");
        let geometry=&editor.core.cook.pieces[0].prepared.geometry;
        let positions=g::positions(geometry).clone();
        if tick==1 {first=Some(positions);} else if tick==30 {
            assert!((0..p::length(&positions)).any(|i|p::get(&positions,i).unwrap()!=p::get(first.as_ref().unwrap(),i).unwrap()),"orrery did not animate");
        }
        rays::canvas::render(Some(1),&canvas,&Editor::scene(&editor,&f)).unwrap();
        editor=Editor::after_present(editor,&f);
    }
    editor.core.timeline=Timeline::toggle_pause(&editor.core.timeline).0;
    let mut f=frame(31); f.input.dt=0.;
    editor=Editor::update(editor,&f);
    rays::canvas::render(Some(1),&canvas,&Editor::scene(&editor,&f)).unwrap();
    editor=Editor::after_present(editor,&f);
    let compiled=editor.core.cook.compiled.clone();
    let prepared=editor.core.cook.pieces[0].prepared.clone();
    let misses=rays_editor::cook::stats(&editor.core.cook).misses;
    let warm=wg::image_stats(&gpu).0;
    let renderer=rays::canvas::Private::native_stats(&canvas).unwrap();
    assert!(renderer.pipeline_cache_entries>0,"native renderer did not use GPU pipelines");
    for tick in 32..=61 {
        f.input.frame=tick; f.input.tick=tick; f.input.t=tick as f64/60.;
        editor=Editor::update(editor,&f);
        assert!(Arc::ptr_eq(&document,&editor.core.doc),"idle frame re-lowered the document");
        assert_eq!(editor.core.cook.compiled.len(),compiled.len());
        for (id,(_,old)) in &compiled {
            assert!(Arc::ptr_eq(old,&editor.core.cook.compiled[id].1),"idle frame recompiled object {id}");
        }
        assert!(Rc::ptr_eq(&prepared,&editor.core.cook.pieces[0].prepared),"idle frame re-prepared geometry");
        assert_eq!(rays_editor::cook::stats(&editor.core.cook).misses,misses,"idle frame recooked geometry");
        rays::canvas::render(Some(1),&canvas,&Editor::scene(&editor,&f)).unwrap();
        editor=Editor::after_present(editor,&f);
    }
    let after=wg::image_stats(&gpu).0;
    assert_eq!(after.pipeline_compilations,warm.pipeline_compilations,"idle frame compiled a GPU kernel");
    assert_eq!(after.runners_created,warm.runners_created,"idle frame created a GPU runner");
    assert_eq!(rays::canvas::Private::native_stats(&canvas).unwrap().pipeline_cache_entries,renderer.pipeline_cache_entries,"idle frame compiled a render pipeline");
    Editor::close(&editor); wg::close(&gpu); rays::canvas::destroy(&canvas).unwrap();
}
