//! Real-device dynamic topology, state trails and editor-holder qualification.
use flow_native::workspace_gpu as wg;
use flow_ir::flow_ir::Gpu;
use flow_sop::{lower,value_lane};
use rdk_core::{geometry as g, attribute as a, packed::Float3 as p};
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
    vec![("native_voxel_neighbors",voxel_neighbors),("native_swarm_frames",swarm_frames),("native_live_mesh",live_mesh),("native_holder_frames",holder_frames),("native_sea_frames",sea_frames),("native_particles_frames",particles_frames),("native_sea_initial_close",sea_initial_close),("native_orrery_frames",orrery_frames)]
}

fn voxel_neighbors() {
    assert!(crate::common::gpu());
    let prefix=MESH.split("faces (array/compact").next().unwrap().replace("%d","8");
    let text=format!(r#"{prefix} result (map (fn [(face : int)] (let* [index (int/div face 6) p (cell index side)
      delta (nth directions (mod face 6))]
      [(float (array/nth voxels index)) (float (if (and (> (array/nth voxels index) 0) (= (occupied voxels side (+ p delta)) 0)) 1 0)) delta.x])) (array/range (* (* side side) (* side 6)) :type int))]
      (sop/mesh :points (exact result) :vertex_points (array/int 0) :primitive_offsets (array/int 1) :kinds (array/int 0)))))"#);
    let mut host=None;
    cook(&text,None,&[0.],None,false,|_,g|host=Some(g::positions(g).clone()));
    let gpu=wg::create(None);wg::qualification(&gpu);
    wg::with_backend(&gpu,true,&mut |config|cook(&text,Some(config.clone()),&[0.],None,true,|_,g|same(g::positions(g),host.as_ref().unwrap())));
    wg::close(&gpu);
}

fn swarm_frames() {
    assert!(crate::common::gpu(),"native GPU required");
    let library=include_str!("../../../../sketches/swarm_lib.rays");
    // An off-centre circle passes through points whose tangent is radial. Re-seeding
    // every ring from its position flips the frame there and turns the tube inside out.
    let text=format!(r#"(workspace frames {library}
      (graph g :context sop
        (let* [positions (map (fn [(i : int)]
                                (let* [step (int/div i 2) a (* (float step) (/ 6.283185307179586 256.0))]
                                  (if (= (mod i 2) 0)
                                    [(+ 2.0 (cos a)) (sin a) (* t 0.1)]
                                    [0.0 (+ 1.0 (+ (* (float step) 0.03) (* t 0.1))) 0.0])))
                              (array/range 514 :type int))]
          (tube-sweep positions 2 257 5 0.05
            {{:a [(+ 0.8 (* t 0.01)) 0.4 0.2] :b [0.0 0.0 0.0] :c [1.0 1.0 1.0] :d [0.0 0.0 0.0]}}))))"#);
    let times=[0.,1./6.];
    let mut hosts=Vec::new();
    cook(&text,None,&times,None,false,|_,g|hosts.push(g.clone()));
    for host in &hosts {
      let normals=vec3(host,"N");
      for tube in 0..2 { for ring in 1..257 {
        let (x,y,z)=p::get(&normals,(tube*257+ring-1)*5).unwrap();
        let (u,v,w)=p::get(&normals,(tube*257+ring)*5).unwrap();
        assert!((u*u+v*v+w*w-1.).abs()<1e-6,"collapsed tube frame");
        assert!(x*u+y*v+z*w>0.98,"tube {tube} frame flipped at ring {ring}");
      }}
    }
    let topo=rdk_core::topology::View::view(g::topology(&hosts[0])).unwrap();
    let mut edges=std::collections::BTreeMap::new();
    for triangle in topo.vertex_points.chunks_exact(3) {
        for (a,b) in [(triangle[0],triangle[1]),(triangle[1],triangle[2]),(triangle[2],triangle[0])] {
            let (count,winding)=edges.entry((a.min(b),a.max(b))).or_insert((0,0));
            *count+=1; *winding+=if a<b {1}else{-1};
        }
    }
    assert!(edges.values().all(|e|*e==(2,0)),"tube must be closed with consistent winding");
    let gpu=wg::create(None); wg::qualification(&gpu);
    let canvas=rays::canvas::create(128,128).unwrap();
    let camera=rays::camera::perspective(None,None,None,rays_math::vec3::create(2.,0.,5.),rays_math::vec3::create(2.,0.,0.)).unwrap();
    let picture=|geo:&g::T,resident:bool| {
        let surface=rdk_rays::surface::of_geometry(None,geo).unwrap();
        let display=|g:&g::T|binding(&gpu,g);
        let scene=rdk_rays::surface::scene3(None,if resident {Some(&display)}else{None},&surface).unwrap();
        rays::canvas::render(None,&canvas,&[rays::scene::clear(rays_math::color::black()),rays::scene::view3d(None,&camera,scene)]).unwrap();
        rays::canvas::pixels(&canvas).unwrap()
    };
    wg::with_backend(&gpu,true,&mut |config|cook(&text,Some(config.clone()),&times,None,true,|frame,g|{
        let host=&hosts[frame];
        let expected=picture(host,false);
        assert!(expected.iter().any(|p|p.r>0));
        assert!(p::resident(g::positions(g)),"{:?}",flow_ir::fallback::seen());
        assert!(flow_sop::topology_kernel::resident(g).is_some(),"tube topology left the GPU");
        assert!(wg::display(&gpu,g).is_some(),"{:?}",wg::display_refusal(&gpu));
        let before=wg::image_stats(&gpu).0.payload_readback_bytes;
        let actual=picture(g,true);
        assert_eq!(wg::image_stats(&gpu).0.payload_readback_bytes,before);
        for (a,b) in actual.iter().zip(&expected) {
            for (a,b) in [(a.r,b.r),(a.g,b.g),(a.b,b.b)] {assert!((a-b).abs()<=1,"resident raster differs from host mesh");}
        }
        same(g::positions(g),g::positions(host));
        let actual_normals=vec3(g,"N"); let expected_normals=vec3(host,"N");
        for i in 0..p::length(&actual_normals) {
            let (x,y,z)=p::get(&actual_normals,i).unwrap(); let (u,v,w)=p::get(&expected_normals,i).unwrap();
            // Tangents subtract nearby float32 positions before normalizing.
            assert!((x-u).abs()<1e-5 && (y-v).abs()<1e-5 && (z-w).abs()<1e-5,"normal {i}");
        }
    }));
    rays::canvas::destroy(&canvas).unwrap(); wg::close(&gpu);
}

fn particles_frames() {
    use rays_editor::{Editor, Workspace};
    assert!(crate::common::gpu(), "native GPU required");
    let doc = Workspace::load(None, Vec::new(), None, include_str!("../../../../sketches/flow_particles_gpu.rays")).unwrap_or_else(|d| panic!("{d:?}"));
    let canvas = rays::canvas::create(800, 600).unwrap();
    let gpu = wg::create(None);
    let mut editor = Editor::create(Editor::Create { gpu_owner: Some(gpu.clone()), await_: Some(true), domains: Some(1), ..Default::default() }, &doc, None, Editor::prepare_fn(), Editor::draw_fn()).unwrap();
    let mut warm = None;
    let mut pixels = None;
    for tick in 1..=8 {
        let frame = rays::frame::t { input: input::Frame { dt: 1./60., frame: tick, tick, size: (800,600), pointer: (-100.,-100.), ..input::at_time(tick as f64/6.) }, pixel_scale: (1.,1.), fps: 60., mouse_delta: (0.,0.) };
        let allocated = mem_sys::alloc_bytes_total();
        let started = std::time::Instant::now();
        editor = Editor::update(editor, &frame);
        let updated = started.elapsed();
        assert!(editor.context_error.is_none(), "{:?}", editor.context_error);
        assert!(editor.core.cook.error.is_none(), "{:?}", editor.core.cook.error);
        assert!(editor.core.cook.pieces.is_empty(), "hidden exact SOP surface cooked behind the particle canvas");
        assert_eq!(editor.canvases.len(), 1);
        assert!(flow_native::drawing::borrows_gpu(editor.canvases[0].1.prepared.as_ref().unwrap()), "particle positions left the device");
        rays::canvas::render(None, &canvas, &Editor::scene(&editor, &frame)).unwrap();
        let stats = wg::image_stats(&gpu).0;
        assert_eq!(stats.payload_readback_bytes, 0, "display-only positions were read back");
        let frame_bytes = mem_sys::alloc_bytes_total() - allocated;
        if let Some((uploads, buffers, pipelines)) = warm {
            assert_eq!(stats.input_uploads, uploads, "immutable particle indices uploaded again");
            assert_eq!(stats.buffer_creations, buffers, "animation allocated more GPU buffers");
            assert_eq!(stats.pipeline_compilations, pipelines, "animation compiled another shader");
            assert!(frame_bytes <= 1_100_000, "particle frame allocated {frame_bytes} B");
        }
        if tick == 3 { warm = Some((stats.input_uploads, stats.buffer_creations, stats.pipeline_compilations)); }
        println!("particles frame {tick}: update {updated:?}, total {:?}, allocated {frame_bytes} B", started.elapsed());
        if tick == 1 { pixels = Some(rays::canvas::pixels(&canvas).unwrap()); }
        editor = Editor::after_present(editor, &frame);
    }
    let first = pixels.unwrap();
    let last = rays::canvas::pixels(&canvas).unwrap();
    assert!(last.iter().filter(|p| p.g > 80).count() > 1000, "particle canvas is blank");
    assert!(first.iter().zip(&last).filter(|(a,b)| a != b).count() > 1000, "particle animation is frozen");
    Editor::close(&editor);
    wg::close(&gpu);
    rays::canvas::destroy(&canvas).unwrap();
}

fn sea_initial_close() {
    use rays_editor::{Editor,Workspace};
    assert!(crate::common::gpu(),"native GPU required");
    let doc=Workspace::load(None,Vec::new(),None,include_str!("../../../../sketches/gpu_sea.rays")).unwrap_or_else(|d|panic!("{d:?}"));
    let gpu=wg::create(None);
    let editor=Editor::create(Editor::Create {gpu_owner:Some(gpu.clone()),await_:Some(false),domains:Some(1),..Default::default()},&doc,None,Editor::prepare_fn(),Editor::draw_fn()).unwrap();
    let frame=rays::frame::t {input:input::Frame {dt:1./60.,frame:1,tick:1,size:(320,240),pointer:(-100.,-100.),..input::at_time(1./60.)},pixel_scale:(1.,1.),fps:60.,mouse_delta:(0.,0.)};
    let editor=Editor::update(editor,&frame);
    let started=std::time::Instant::now();
    while matches!(rays_editor::cook::status(&editor.core.cook),sop::async_cook::Status::Cooking {queued:true,..}) && started.elapsed()<std::time::Duration::from_millis(100) {
        std::thread::sleep(std::time::Duration::from_millis(1));
    }
    assert!(matches!(rays_editor::cook::status(&editor.core.cook),sop::async_cook::Status::Cooking {queued:false,..}),"initial sea cook did not start");
    let started=std::time::Instant::now();
    Editor::close(&editor); wg::close(&gpu);
    println!("sea initial close {:?}",started.elapsed());
    assert!(started.elapsed()<std::time::Duration::from_millis(250),"closing waited for the initial sea cook");
}

fn sea_frames() {
    use rays_editor::{Editor,Workspace};
    assert!(crate::common::gpu(),"native GPU required");
    let started=std::time::Instant::now();
    let source=include_str!("../../../../sketches/gpu_sea.rays");
    let doc=Workspace::load(None,Vec::new(),None,source).unwrap_or_else(|d|panic!("{d:?}"));
    let canvas=rays::canvas::create(320,240).unwrap();
    let gpu=wg::create(None);
    let mut editor=Editor::create(Editor::Create {gpu_owner:Some(gpu.clone()),await_:Some(true),domains:Some(1),..Default::default()},&doc,None,Editor::prepare_fn(),Editor::draw_fn()).unwrap();
    println!("sea open {:?}",started.elapsed());
    let mut previous=None;
    for tick in 1..=6 {
        let started=std::time::Instant::now();
        let frame=rays::frame::t {input:input::Frame {dt:1./60.,frame:tick,tick,size:(320,240),pointer:(-100.,-100.),..input::at_time(tick as f64/60.)},pixel_scale:(1.,1.),fps:60.,mouse_delta:(0.,0.)};
        editor=Editor::update(editor,&frame);
        assert!(editor.core.cook.error.is_none(),"{:?}",editor.core.cook.error);
        let geometry=&editor.core.cook.pieces[0].prepared.geometry;
        assert_eq!(g::point_count(geometry),1_000_000);
        assert_eq!(g::primitive_count(geometry),1_996_002);
        assert!(p::resident(g::positions(geometry)),"sea positions left the GPU");
        assert!(a::holder(&attribute(geometry,"N")).is_some(),"sea normals left the GPU");
        assert!(flow_sop::topology_kernel::resident(geometry).is_some(),"sea topology left the GPU");
        rays::canvas::render(Some(1),&canvas,&Editor::scene(&editor,&frame)).unwrap();
        assert!(wg::displays(&gpu)>0,"sea mesh did not bind the resident display");
        let stats=wg::image_stats(&gpu).0;
        assert_eq!(stats.payload_readback_bytes,0);
        assert_eq!(stats.bounds_readback_bytes,tick*24);
        assert_eq!(stats.input_uploaded_bytes,0,"sea generated geometry through a host staging array");
        if let Some((uploads,buffers))=previous {
            assert_eq!(stats.input_uploads,uploads,"animation uploaded immutable inputs again");
            if tick>3 {assert_eq!(stats.buffer_creations,buffers,"steady animation allocated more GPU buffers");}
        }
        previous=Some((stats.input_uploads,stats.buffer_creations));
        println!("sea frame {tick} {:?}: uploaded {} B, payload read {} B, bounds read {} B",started.elapsed(),stats.input_uploaded_bytes,stats.payload_readback_bytes,stats.bounds_readback_bytes);
        editor=Editor::after_present(editor,&frame);
    }
    Rc::make_mut(&mut editor.core.cook).await_=false;
    let frame=rays::frame::t {input:input::Frame {dt:1./60.,frame:7,tick:7,size:(320,240),pointer:(-100.,-100.),..input::at_time(7./60.)},pixel_scale:(1.,1.),fps:60.,mouse_delta:(0.,0.)};
    editor=Editor::update(editor,&frame);
    println!("sea closing {:?}",rays_editor::cook::status(&editor.core.cook));
    let started=std::time::Instant::now();
    Editor::close(&editor); wg::close(&gpu); rays::canvas::destroy(&canvas).unwrap();
    println!("sea close {:?}",started.elapsed());
    assert!(started.elapsed()<std::time::Duration::from_millis(250),"closing waited for an obsolete sea cook");
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
        assert!((x-u).abs()<=1e-6 && (y-v).abs()<=1e-6 && (z-w).abs()<=1e-6,"point {i}: ({x}, {y}, {z}) != ({u}, {v}, {w})");
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
        if compare { let (p,n,cd,count)=&cpu[frame]; assert_eq!(g::primitive_count(g),*count,"side {side} frame {frame}"); same(g::positions(g),p); same(&vec3(g,"N"),n); same(&vec3(g,"Cd"),cd); }
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
    assert!(crate::common::gpu(),"native GPU required");
    let gpu=wg::create(None); wg::qualification(&gpu);
    wg::with_backend(&gpu,true,&mut |config|{
        resident(&gpu,config,8,&[0.,1./6.],true,false);
        traced(&gpu,config);
        assert!(flow_ir::fallback::seen().is_empty(),"{:?}",flow_ir::fallback::seen());
        // Sorting within a per-vertex map remains a CPU-only form.
        let broken=text(16).replace("(mod vertex 4)","(nth (sort-by (fn [(n : int)] n) (list (mod vertex 4))) 0)");
        cook(&broken,Some(config.clone()),&[0.],None,false,|_,_|{});
        assert!(flow_ir::fallback::seen().iter().any(|(site,reason,_)|site.ends_with("positions")&&!reason.is_empty()));
        flow_ir::fallback::reset();
        let sea=std::fs::read_to_string(concat!(env!("CARGO_MANIFEST_DIR"),"/../../sketches/gpu_sea.rays")).unwrap().replace("(side : int 1000)","(side : int 256)");
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
        let root=std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../sketches");
        let swarm=std::fs::read_to_string(root.join("swarm.rays")).unwrap().replace("(import \"swarm_lib.rays\")","").replacen("(workspace swarm",&format!("(workspace swarm\n{}",std::fs::read_to_string(root.join("swarm_lib.rays")).unwrap()),1).replace("(tubes : int 4000)","(tubes : int 300)").replace("(points : int 257)","(points : int 32)");
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
    assert!(crate::common::gpu(),"native GPU required");
    for (name,side,topology) in [("gpu_sea",128,false),("isogrid_city",8,true)] {
        let source=std::fs::read_to_string(format!("{}/../../sketches/{name}.rays",env!("CARGO_MANIFEST_DIR"))).unwrap();
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
    assert!(crate::common::gpu(),"native GPU required");
    let source=std::fs::read_to_string(concat!(env!("CARGO_MANIFEST_DIR"),"/../../sketches/ws_orrery.rays")).unwrap();
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
