//! Native IsoGrid viewport qualification, ported from test_isogrid_viewport.ml.
use flow_native::workspace_gpu as gpu;
use flow_ir::flow_ir::Gpu;
use rdk::{geometry as g, packed::Float3, topology};
use rays::{canvas, scene, scene3};
use rays_math::{color, vec3};
use flow_sop::lower;

pub(super) const FROZEN: &str = "75686b00542de41aba26b5ff4cc63f88805924f4a625e8ad44c033d66664eeda";
pub(super) fn ok<T>(r: Result<T, rays_diagnostic::Diagnostic>) -> T {
    r.unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)))
}
pub(super) fn text() -> String {
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../../sketches/isogrid.rays");
    let text = std::fs::read_to_string(path).unwrap();
    assert_eq!(editor_document::lowering::sha256(&text), FROZEN);
    text
}
pub(super) fn workspace() -> editor_document::workspace_doc::T {
    rays_editor::Workspace::load(None, Vec::new(), None, &text()).unwrap_or_else(|ds| panic!("{}", ds.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("\n")))
}
pub(super) fn recipe_node(workspace: &editor_document::workspace_doc::T, config: &Gpu::Config, recipe: i64) -> (lower::T, sop::node::T) {
    let catalog = ok(rays_editor::workspace_catalog(None));
    let value = ok(flow_syntax::syntax::parse(&recipe.to_string())).remove(0);
    let workspace = ok(editor_document::workspace_doc::edit(&catalog, workspace, &flow_graph::flow_edit::Op::SetArg {
        node: vec!["volume".into(), "recipe-switch".into()], key: flow_graph::flow_edit::ArgKey::Whole, sub: vec![], value,
    }));
    let lowered = ok(lower::of_checked(None, lower::Options { gpu: config.clone(), defer: true, inputs: Some((*workspace.inputs).clone()), ..Default::default() }, &workspace.checked));
    let graph = lowered.graphs.iter().find(|g| g.default && g.name == "sculpture").unwrap();
    let node = sop::edit_graph::compile_node(&graph.network.geometry, graph.root.unwrap()).unwrap();
    (lowered, node)
}
pub(super) fn cook(session: &mut sop::session::T, node: &sop::node::T, time: f64) -> g::T {
    let context = sop::context::create(sop::context::Options { domains: Some(1), time: Some(time), ..Default::default() }).unwrap();
    let output = ok(sop::session::cook(session, Some(0), &context, node));
    ok(sop::payload::geometry(&output.payload))
}
pub(super) fn resident(geometry: &g::T) -> bool {
    let attr = |name| g::find_attribute(rdk::attribute::Owner::Point, name, geometry).is_some_and(|a| rdk::attribute::resident(&a));
    Float3::resident(g::positions(geometry)) && topology::resident(g::topology(geometry)) && attr("N") && attr("Cd")
}
pub(super) fn placed(session: &mut sop::session::T, node: &sop::node::T) -> g::T {
    for attempt in 0..=8 {
        let geometry = cook(session, node, 0.);
        if resident(&geometry) { return geometry; }
        assert!(attempt < 8, "IsoGrid mesh did not become wholly resident");
    }
    unreachable!()
}
pub(super) fn picture(owner: &gpu::t, geometry: &g::T) -> scene3::t {
    let surface = rdk_rays::surface::of_geometry(None, geometry).unwrap();
    let display = |g: &g::T| gpu::display(owner, g).map(|(key, count, indices, color, keep)| (key, count, indices, color, std::rc::Rc::from(keep)));
    rdk_rays::surface::scene3(None, Some(&display), &surface).unwrap()
}
pub(super) fn frame(camera: &rays::camera::t, picture: scene3::t) -> scene::t {
    vec![scene::clear(color::black()), scene::view3d(None, camera, picture)]
}
pub(super) fn draw(canvas: &canvas::t, frame: &[scene::node]) -> Vec<color::T> {
    canvas::render(None, canvas, frame).unwrap();
    canvas::pixels(canvas).unwrap()
}
fn run(owner: &gpu::t, config: &Gpu::Config, workspace: &editor_document::workspace_doc::T, recipe: i64) {
    let publication = gpu::image_stats(owner).0;
    let (_lowered, node) = recipe_node(workspace, config, recipe);
    let mut session = sop::session::create(0, 1 << 28).unwrap();
    let geometry = placed(&mut session, &node);
    let before = gpu::image_stats(owner).0;
    assert_eq!(before.payload_readback_bytes, publication.payload_readback_bytes);
    let surface = rdk_rays::surface::of_geometry(None, &geometry).unwrap();
    assert_eq!(gpu::image_stats(owner).0.payload_readback_bytes, before.payload_readback_bytes);
    let display = |g: &g::T| gpu::display(owner, g).map(|(key, count, indices, color, keep)| (key, count, indices, color, std::rc::Rc::from(keep)));
    let picture = rdk_rays::surface::scene3(None, Some(&display), &surface).unwrap();
    assert_eq!(gpu::display_refusal(owner), None);
    let mesh = scene3::Walk::drawings(&picture)[0].mesh.clone();
    assert!(rays::mesh::View::device(&mesh).is_some());
    assert_eq!(rays::mesh::View::device_index_count(&mesh), Some(g::primitive_count(&geometry) * 6));
    assert_eq!(rays::mesh::View::device_color_width(&mesh), 3);
    let bound = gpu::image_stats(owner).0;
    assert_eq!(bound.payload_readback_bytes, before.payload_readback_bytes);
    assert_eq!(bound.input_uploaded_bytes, before.input_uploaded_bytes);
    assert_eq!(bound.validation_readback_bytes, before.validation_readback_bytes + 4);
    let camera = rays::camera::perspective(Some(36. * std::f64::consts::PI / 180.), None, None, vec3::create(10.,8.,10.), vec3::zero()).unwrap();
    let original = frame(&camera, picture.clone());
    let staged = scene::Layer::stage_native(256, 256, &original).unwrap();
    let entry = &staged.scene3[0].entries[0];
    assert!(entry.draw.mesh.vertices.borrow().is_empty() && entry.draw.mesh.indices.borrow().is_empty());
    assert!(entry.vertex_attributes.as_ref().is_some_and(|(_, bytes)| bytes.borrow().is_empty()));
    let canvas = canvas::create(256, 256).unwrap();
    let host_canvas = canvas::create(256, 256).unwrap();
    canvas::render(None, &canvas, &original).unwrap();
    let warm = gpu::image_stats(owner).0;
    let renderer = canvas::Private::native_stats(&canvas).unwrap();
    assert!(renderer.uploaded_bytes < 65536);
    let allocated = mem_sys::alloc_bytes_total();
    for _ in 0..3 { canvas::render(None, &canvas, &original).unwrap(); }
    let frame_allocation = (mem_sys::alloc_bytes_total() - allocated) / 3;
    assert!(frame_allocation < 131072, "{frame_allocation} allocated bytes per warm frame");
    let after = gpu::image_stats(owner).0;
    let rendered = canvas::Private::native_stats(&canvas).unwrap();
    assert_eq!(rendered.uploaded_bytes, renderer.uploaded_bytes);
    assert_eq!(rendered.pipeline_cache_entries, renderer.pipeline_cache_entries);
    assert_eq!(after.payload_readback_bytes, before.payload_readback_bytes);
    assert_eq!(after.buffer_creations, warm.buffer_creations);
    assert_eq!(after.pipeline_compilations, warm.pipeline_compilations);
    assert_eq!(after.validation_readback_bytes, warm.validation_readback_bytes);
    assert!(topology::resident(g::topology(&geometry)) && Float3::resident(g::positions(&geometry)));
    let pixels = canvas::pixels(&canvas).unwrap();
    assert!(pixels.iter().filter(|c| i64::from(c.r) + i64::from(c.g) + i64::from(c.b) > 0).count() > 1024);
    let bounds = rdk_rays::geometry_view::bounds(&geometry);
    assert_eq!(gpu::image_stats(owner).0.payload_readback_bytes, before.payload_readback_bytes);
    assert!(gpu::image_stats(owner).0.bounds_readback_bytes - after.bounds_readback_bytes <= 24576);
    let orbit = rays::camera::perspective(None,None,None,vec3::create(-10.,5.,7.),vec3::zero()).unwrap();
    let (lo, hi) = bounds.unwrap();
    let easy = rays::easy_camera::create(None,None,None,None,None,None).unwrap();
    let framed = rays::easy_camera::camera(&rays::easy_camera::frame_bounds(lo, hi, &easy)).unwrap();
    let mut world = rays::world::bake(Some(1), None, 32,16,&rays::world::default()).unwrap();
    world.sun = Some(rays::world::sun { direction: vec3::normalize(vec3::create(0.3,1.,0.2)), radiance: rays::world::rgb(4.,4.,4.), angular_radius: 0.01 });
    let mut variants = Vec::new();
    for (name, camera, shadow) in [("orbit", orbit, false), ("frame", framed, false), ("shadow", camera.clone(), true)] {
        let adapted = if shadow { scene3::with_world(world.clone(), &picture) } else { picture.clone() };
        let variant = frame(&camera, adapted);
        canvas::render(None, &canvas, &variant).unwrap();
        let first = canvas::Private::native_stats(&canvas).unwrap();
        canvas::render(None, &canvas, &variant).unwrap();
        let next = canvas::Private::native_stats(&canvas).unwrap();
        assert_eq!(first.uploaded_bytes,next.uploaded_bytes);
        assert_eq!(first.sun_shadow_passes,next.sun_shadow_passes);
        variants.push((name,camera,shadow,canvas::pixels(&canvas).unwrap()));
    }
    assert_eq!(gpu::image_stats(owner).0.payload_readback_bytes,before.payload_readback_bytes);
    let summary = rdk_rays::geometry_view::summary(&geometry);
    assert_eq!(summary.points,g::point_count(&geometry));
    assert_eq!(summary.prims,g::primitive_count(&geometry));
    for owner in 0..4 { rdk_rays::geometry_view::count(owner,&geometry); }
    assert_eq!(gpu::image_stats(owner).0.payload_readback_bytes,before.payload_readback_bytes);
    let consumer_start = gpu::image_stats(owner).0.payload_readback_bytes;
    if recipe == 0 {
        let index = rdk_rays::geometry_view::surface(&geometry);
        assert!(index.is_some());
        assert!(!topology::resident(g::topology(&geometry)));
        assert!(!Float3::resident(g::positions(&geometry)));
        let top = g::topology(&geometry);
        let (first,last) = topology::primitive_vertex_range(top,0).unwrap();
        assert_eq!(last-first,4);
        let point = |i| { let (x,y,z) = Float3::get(g::positions(&geometry),topology::point_of_vertex(top,first+i).unwrap()).unwrap(); vec3::create(x,y,z) };
        let center = vec3::scale(vec3::add(vec3::add(point(0),point(1)),vec3::add(point(2),point(3))),0.25);
        let normal = vec3::normalize(vec3::cross(vec3::sub(point(1),point(0)),vec3::sub(point(2),point(0))));
        let cast = || rdk_rays::geometry_view::cast_material(index.as_ref(),&geometry,vec3::add(center,vec3::scale(normal,0.001)),vec3::scale(normal,-1.));
        let hit = cast();
        assert!(hit.as_ref().is_some_and(|(distance,_)| (distance-0.001).abs()<1e-6));
        let read = gpu::image_stats(owner).0.payload_readback_bytes;
        assert_eq!(cast(),hit);
        assert_eq!(gpu::image_stats(owner).0.payload_readback_bytes,read);
    } else {
        let cells = || (0..4).map(|owner| {
            let count = rdk_rays::geometry_view::count(owner,&geometry);
            rdk_rays::geometry_view::columns(owner,&geometry).into_iter().map(|(name,cell)| (name,if count==0 {vec![]} else {[0,count/2,count-1].into_iter().map(cell).collect::<Vec<_>>() })).collect::<Vec<_>>()
        }).collect::<Vec<_>>();
        let first = cells();
        assert!(first[0].iter().any(|(name,_)| name=="Cd.x"));
        assert!(!Float3::resident(g::positions(&geometry)));
        assert!(topology::resident(g::topology(&geometry)));
        let read = gpu::image_stats(owner).0.payload_readback_bytes;
        assert_eq!(cells(),first);
        assert_eq!(gpu::image_stats(owner).0.payload_readback_bytes,read);
    }
    assert!(gpu::image_stats(owner).0.payload_readback_bytes > consumer_start);
    let trace_start = gpu::image_stats(owner).0.payload_readback_bytes;
    let material = rays_pathtracer::rays_pathtracer::material(None,None,None,None,rays_pathtracer::rays_pathtracer::Linear_color::rgb(0.8,0.7,0.6));
    rays_pathtracer::rays_pathtracer::mesh(None,None,&[(mesh.clone(),material)]).unwrap();
    assert_eq!(gpu::image_stats(owner).0.payload_readback_bytes,trace_start);
    let host_scene = rdk_rays::surface::scene3(None,None,&rdk_rays::surface::of_geometry(None,&geometry).unwrap()).unwrap();
    assert_eq!(draw(&host_canvas,&frame(&camera,host_scene.clone())),pixels);
    assert!(gpu::image_stats(owner).0.payload_readback_bytes > before.payload_readback_bytes);
    let read_for_trace = gpu::image_stats(owner).0.payload_readback_bytes;
    let host_mesh = scene3::Walk::drawings(&host_scene)[0].mesh.clone();
    for mode in [scene3::render_mode::Wireframe,scene3::render_mode::Vertices] {
        let mode_frame = |mesh| frame(&camera,scene3::create(None,None,None,None,vec![scene3::mesh(None,None,Some(mode),Some(scene3::cull::Cull_none),None,mesh)]).unwrap());
        assert_eq!(draw(&canvas,&mode_frame(mesh.clone())),draw(&host_canvas,&mode_frame(host_mesh.clone())));
    }
    assert_eq!(gpu::image_stats(owner).0.payload_readback_bytes,read_for_trace);
    for (name,camera,shadow,expected) in variants {
        let adapted = if shadow { scene3::with_world(world.clone(),&host_scene) } else {host_scene.clone()};
        assert_eq!(draw(&host_canvas,&frame(&camera,adapted)),expected,"recipe {recipe} {name}");
    }
    assert_eq!(bounds,rdk_rays::geometry_view::bounds(&geometry));
    let read = gpu::image_stats(owner).0.payload_readback_bytes;
    assert_eq!(draw(&canvas,&original),pixels);
    assert_eq!(gpu::image_stats(owner).0.payload_readback_bytes,read);
    assert_eq!(draw(&host_canvas,&frame(&camera,host_scene)),pixels);
    if let Ok(directory) = std::env::var("RAYS_S9_IMAGES") {
        std::fs::create_dir_all(&directory).unwrap();
        canvas::save_png(&canvas,&std::path::Path::new(&directory).join(format!("recipe-{recipe}-device.png")).to_string_lossy()).unwrap();
        canvas::save_png(&host_canvas,&std::path::Path::new(&directory).join(format!("recipe-{recipe}-host.png")).to_string_lossy()).unwrap();
    }
    canvas::destroy(&canvas).unwrap();
    canvas::destroy(&host_canvas).unwrap();
    gpu::frame(owner,Some(&[])); gpu::frame(owner,None); gpu::frame(owner,None);
    sop::session::close(&mut session);
}
fn viewport(recipe: i64) {
    assert!(crate::common::gpu(),"native qualification requires Vulkan");
    let workspace = workspace();
    let baseline = ogpu::impl_::live_handles().unwrap();
    let owner = gpu::create(None);
    gpu::with_backend(&owner,true,&mut |config| {
        let config = Gpu::Config { policy: Gpu::Policy::Qualification, ..config.clone() };
        run(&owner,&config,&workspace,recipe);
    });
    gpu::close(&owner);
    assert_eq!(ogpu::impl_::live_handles().unwrap(),baseline);
    text();
}
pub fn cases() -> Vec<(&'static str, fn())> {
    vec![("native_isogrid_viewport_recipe_0",|| viewport(0)),("native_isogrid_viewport_recipe_1",|| viewport(1)),("native_isogrid_viewport_recipe_2",|| viewport(2)),("native_isogrid_viewport_recipe_3",|| viewport(3)),("native_isogrid_viewport_recipe_4",|| viewport(4)),("native_isogrid_viewport_recipe_5",|| viewport(5)),("native_isogrid_viewport_recipe_6",|| viewport(6)),("native_isogrid_viewport_recipe_7",|| viewport(7))]
}
