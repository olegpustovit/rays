//! Port of lib/flow_gpu/test_image_sink_native.ml: real GPU conversion, qualified publication,
//! retained image/mesh consumers, expiry, resize and borrowed ownership. The native fixture
//! selects the installed Vulkan/Metal implementation; no mock executes a producer or consumer.
//! Owner-only Rc values are !Send, so Rust rejects the oracle's wrong-domain transfers statically.
mod common;
use common::test_program as tp;
use flow_gpu::{host, image_sink as s, pipelines, run};
use flow_ir::flow_ir::{Executor as ex, Gpu};
use flow_ir::packed::{self, host as phost};
use flow_shader::emit;
use flow_syntax::value::Value;
use ogpu_core::backend as b;
use ogpu_core::error::ErrorKind;
use rays::{canvas, image, scene, scene3, texture};
use rays_math::{color, vec3};
use renderer::renderer as rr;
use runtime_resources::Image as i;
use std::rc::Rc;

// Borrowed resources cannot cross a thread boundary in safe Rust; do not weaken that contract.
const _: fn() = || {
    trait Ambiguous<A> { fn check() {} }
    impl<T: ?Sized> Ambiguous<()> for T {}
    impl<T: ?Sized + Send> Ambiguous<u8> for T {}
    let _ = <s::Output as Ambiguous<_>>::check;
    let _ = <i::t as Ambiguous<_>>::check;
    let _ = <texture::t as Ambiguous<_>>::check;
};

fn packed(width: i64, height: i64, body: &str) -> (packed::T, ex::Program) {
    let w = tp::check_workspace(&format!("(workspace image (graph g :context image (image/map (fn [uv] {body}))))"));
    let evaluated = tp::ok(flow_eval::r#static(&w, true, None));
    let node = &evaluated.plan.nodes[0];
    let function = match &node.args.iter().find(|(k, _)| k == "function").unwrap().1 { Value::Fn(f) => *f, _ => panic!("function") };
    let kernel = tp::ok(flow_sop::image_kernel::prepare(None, None, None, 0, width, height, function, &[], &[]));
    let program = flow_sop::image_kernel::program(&kernel);
    (ex::packed_region(&program).unwrap(), program)
}
fn prepare(p: &packed::T, time: f64) -> phost::Inputs {
    tp::ok(phost::prepare(p, &Default::default(), &input::at_time(time)))
}
fn runner(gpu: &rr::Gpu, cache: &pipelines::T, p: &packed::T) -> run::T {
    run::create(None, gpu, cache, &tp::ok(emit::kernel(&Default::default(), p)))
}
fn dispatch(r: &run::T, p: &packed::T, time: f64) -> run::Output {
    tp::ok(run::dispatch(r, &Default::default(), None, None, &prepare(p, time)))
}
fn pixels(output: &s::Output, width: i64) -> Vec<u8> {
    b::read_texture(&s::texture(output).unwrap(), width * 4).unwrap()
}
fn expected(program: &ex::Program, width: i64, height: i64, time: f64) -> Vec<u8> {
    let values = match tp::ok(ex::force(program, &Default::default(), &input::at_time(time))) { Value::Vec4_array(v) => v, _ => panic!("vec4 pixels") };
    let context = sop::context::create(Default::default()).unwrap();
    let image = tp::ok(sop::image::of_vec4(&context, width, height, &values));
    sop::image::rgba8(&image).unwrap().to_vec()
}
fn parity(name: &str, width: i64, height: i64, time: f64, actual: &[u8], expected: &[u8]) {
    assert_eq!(actual.len(), expected.len());
    let mut maximum = 0;
    let mut channels = 0;
    let mut pixels = 0;
    for (a, e) in actual.chunks_exact(4).zip(expected.chunks_exact(4)) {
        let mut different = false;
        for (&a, &e) in a.iter().zip(e) {
            let d = a.abs_diff(e);
            maximum = maximum.max(d);
            if d != 0 { channels += 1; different = true; }
        }
        if different { pixels += 1; }
    }
    println!("image_sink,{name},{width},{height},{time:.2},{maximum},{channels},{pixels}");
    assert!(maximum <= 1, "{name}: max channel difference {maximum}");
}
fn source(output: &s::Output) -> i::gpu_source_fn {
    let output = output.clone();
    Rc::new(move || s::texture(&output))
}
fn snapshot(canvas: &canvas::t) -> Vec<u8> {
    let captured = canvas::to_image(canvas).unwrap();
    let bytes = image::pixels(&captured).unwrap();
    image::destroy(&captured).unwrap();
    bytes
}
fn render(canvas: &canvas::t, nodes: &[scene::node]) { canvas::render(None, canvas, nodes).unwrap(); }
fn checked(nodes: &[scene::node]) -> Rc<scene::Layer::staged_native> {
    scene::Layer::stage_native_render_checked(None, 65, 17, nodes).unwrap_or_else(|e| {
        let mut message = String::new();
        scene::Layer::pp_native_error(&mut message, &e);
        panic!("{message}")
    })
}

fn native_pixel_parity() {
    if !common::gpu() { return; }
    let gpu = common::lease();
    let cache = pipelines::create(&|| 0.0, &rr::gpu_device(&gpu));
    let sink = tp::ok(s::create(&gpu));
    println!("test,fixture,width,height,time,max_channel_difference,differing_channels,differing_pixels");
    for (name, body) in [
        ("gradient", "[uv.x uv.y 0.5 1]"),
        ("live", "[(+ uv.x (* t 0.25)) uv.y 0.5 1]"),
        ("ties", "[(/ 0.5 255) (/ 1.5 255) (/ 2.5 255) (/ 127.5 255)]"),
        ("clip", "[(- uv.x 1) (+ uv.y 1) -1 2]"),
    ] {
        let (p, program) = packed(65, 3, body);
        let r = runner(&gpu, &cache, &p);
        let mut previous = None;
        for time in [0.0, 0.5, 1.0] {
            let output = dispatch(&r, &p, time);
            let converted = tp::ok(s::convert(&sink, 65, 3, &output));
            if let Some(old) = previous { assert!(s::texture(&old).is_none()); }
            let actual = pixels(&converted, 65);
            let expected = expected(&program, 65, 3, time);
            parity(name, 65, 3, time, &actual, &expected);
            if name == "ties" { for pixel in actual.chunks_exact(4) { assert_eq!(pixel, &[0, 2, 2, 128]); } }
            if name == "clip" { assert_eq!(actual, expected); }
            let creates = (s::buffer_creations(&sink), s::texture_creations(&sink));
            tp::ok(s::convert(&sink, 65, 3, &output));
            assert_eq!(creates, (s::buffer_creations(&sink), s::texture_creations(&sink)));
            assert!(s::texture(&converted).is_none());
            dispatch(&r, &p, time);
            assert!(s::convert(&sink, 65, 3, &output).is_err());
            previous = Some(converted);
        }
        run::close(&r, false);
        assert_eq!((s::buffer_creations(&sink), s::texture_creations(&sink)), (1, 1));
    }
    let (p, _) = packed(17, 5, "[uv.x uv.y t 1]");
    let r = runner(&gpu, &cache, &p);
    let output = dispatch(&r, &p, 0.25);
    let resized = tp::ok(s::convert(&sink, 17, 5, &output));
    let texture = s::texture(&resized).unwrap();
    let descriptor = b::texture_descriptor(&texture);
    assert_eq!((descriptor.width, descriptor.height), (17, 5));
    assert_eq!((s::buffer_creations(&sink), s::texture_creations(&sink)), (1, 2));
    s::close(&sink);
    s::close(&sink);
    assert!(s::texture(&resized).is_none() && s::convert(&sink, 17, 5, &output).is_err());
    run::close(&r, false);
    pipelines::close(&cache);
}

fn qualified_publication() {
    if !common::gpu() { return; }
    let w = tp::check_workspace("(workspace image
        (defn render :context image [(bias : float)]
          (let* [picture (image/map (fn [uv] [(+ uv.x bias) uv.y (* t 0.5) 1]))] picture))
        (defn invoke :context image [(alias : fn)] (alias 0.125))
        (graph g :context image (invoke :alias render)))");
    let lowered = tp::ok(flow_sop::lower::of_checked(Some(vec![]), Default::default(), &w));
    let node = lowered.plan.nodes.iter().find(|n| n.kind == "image/map").unwrap();
    let path = lowered.image_sites[node.id as usize].clone().unwrap();
    assert_eq!(path, vec!["def:render", "picture"]);
    assert_ne!(path, node.site);
    let function = match &node.args.iter().find(|(k, _)| k == "function").unwrap().1 { Value::Fn(f) => *f, _ => panic!("function") };
    let kernel = tp::ok(flow_sop::image_kernel::prepare(Some((node.inst, node.site.clone(), node.iter.clone())), Some(path), Some(lowered.approx.clone()), 0, 65, 17, function, &[], &[]));
    let program = flow_sop::image_kernel::program(&kernel);
    let gpu = common::lease();
    let host = host::create(&|| 0.0, &gpu);
    let sink = tp::ok(s::create(&gpu));
    let config = Gpu::Config { backend: Some(host::backend(&host).into()), policy: Gpu::Policy::Qualification, ..Gpu::default() };
    let produce = |time| {
        let value = match tp::ok(ex::try_display(None, None, false, &config, &program, &input::at_time(time))) { Some(ex::Displayed::Gpu(v)) => v, _ => panic!("qualified GPU image") };
        let output = tp::ok(host::output(&host, &value)).unwrap();
        tp::ok(s::convert(&sink, 65, 17, &output))
    };
    let mut image = None;
    let mut snapshots = Vec::new();
    for time in [0.0, 0.5, 1.0] {
        let converted = produce(time);
        let current = match &image {
            None => {
                let source = source(&converted);
                let before = mem_sys::alloc_bytes_total();
                let current = i::of_gpu(65, 17, source).unwrap();
                assert!(mem_sys::alloc_bytes_total() - before < 4096);
                image = Some(current.clone());
                current
            }
            Some(current) => {
                assert!(i::gpu_snapshot(current).is_err());
                let (identity, generation, reads) = (i::identity(current), i::generation(current), i::readbacks(current));
                i::replace_gpu_source(current, 65, 17, source(&converted)).unwrap();
                assert_eq!((i::identity(current), i::generation(current)), (identity, generation + 1));
                assert_eq!(i::readbacks(current), reads);
                current.clone()
            }
        };
        assert_eq!(i::cpu_storage_bytes(&current), 0);
        let (w, h, _, texture) = i::gpu_snapshot(&current).unwrap().unwrap();
        assert_eq!((w, h), (65, 17));
        assert_eq!(b::texture_id(&texture), b::texture_id(&s::texture(&converted).unwrap()));
        let actual = pixels(&converted, 65);
        let expected = expected(&program, 65, 17, time);
        parity("qualified_named", 65, 17, time, &actual, &expected);
        let before = i::readbacks(&current);
        assert_eq!(i::pixels(&current).unwrap(), actual);
        assert_eq!(i::readbacks(&current), before + 1);
        let (_, _, _, bytes, lease) = i::borrow_snapshot(&current).unwrap();
        assert_eq!(&bytes[..], &actual[..]);
        assert_eq!(i::readbacks(&current), before + 2);
        let saved = bytes.to_vec();
        snapshots.push((bytes, saved, lease));
        assert_eq!(i::cpu_storage_bytes(&current), 0);
        for (bytes, saved, _) in &snapshots { assert_eq!(&bytes[..], &saved[..]); }
        parity("published_named", 65, 17, time, &actual, &expected);
    }
    let current = image.unwrap();
    let canvas = canvas::create(65, 17).unwrap();
    let image = image::of_resource(current.clone()).unwrap();
    let node = scene::image(&image, (0, 0), None, None).unwrap();
    let mut builder = scene_command::display_list::Builder::create();
    let rect = scene_command::render_ir::Rect { x: 0.0, y: 0.0, width: 65.0, height: 17.0 };
    builder.image(1, rect, rect).unwrap();
    let segment = builder.publish(scene_command::display_list::fresh_id().unwrap(), 0).unwrap();
    for (index, nodes) in [vec![node.clone()], vec![scene::display_list(Some(vec![(1, image.clone())]), segment).unwrap()], vec![node.clone(), scene::Layer::layer_break(), node.clone()]].into_iter().enumerate() {
        let rendering = scene::Layer::stage_native_render(65, 17, &nodes).unwrap();
        if nodes.len() > 1 || matches!(rendering.layers.as_slice(), [scene::Layer::native_layer::Scene2_segment(..)]) { assert!(rendering.resources.is_empty()); }
        let coordinator = rr::create_offscreen(&rr::Configuration { logical_width: 65, logical_height: 17, drawable_width: 65, drawable_height: 17, ..rr::default_configuration() }).unwrap();
        let staged = scene::Layer::stage_native(65, 17, &nodes).unwrap();
        let lower = || rr::lower_scene2(&coordinator, 1, &|id| staged.resources.iter().find(|(key, _)| *key == id).map(|(_, resource)| resource.clone()), &staged.scene2);
        lower().unwrap();
        lower().unwrap();
        render(&canvas, &nodes);
        render(&canvas, &nodes);
        let old_pixels = snapshot(&canvas);
        let (before, generation) = (i::readbacks(&current), i::generation(&current));
        let converted = produce(0.25 + index as f64 * 0.125);
        assert_eq!(i::generation(&current), generation);
        assert!(matches!(lower(), Err(e) if e.kind == ErrorKind::InvalidState));
        assert!(canvas::render(None, &canvas, &nodes).is_err());
        assert_eq!(i::readbacks(&current), before);
        i::replace_gpu_source(&current, 65, 17, source(&converted)).unwrap();
        assert_eq!(i::generation(&current), generation + 1);
        lower().unwrap();
        render(&canvas, &nodes);
        let pixels = snapshot(&canvas);
        assert_ne!(pixels, old_pixels);
        let fresh = canvas::create(65, 17).unwrap();
        render(&fresh, &nodes);
        assert_eq!(snapshot(&fresh), pixels);
        canvas::destroy(&fresh).unwrap();
        rr::destroy(&coordinator).unwrap();
    }
    canvas::destroy(&canvas).unwrap();
    let (p, _) = packed(17, 5, "[uv.x uv.y t 1]");
    let cache = pipelines::create(&|| 0.0, &rr::gpu_device(&gpu));
    let r = runner(&gpu, &cache, &p);
    let converted = tp::ok(s::convert(&sink, 17, 5, &dispatch(&r, &p, 0.25)));
    let identity = i::identity(&current);
    i::replace_gpu_source(&current, 17, 5, source(&converted)).unwrap();
    assert_eq!(i::identity(&current), identity);
    assert_eq!(i::size(&current).unwrap(), (17, 5));
    assert_eq!(i::cpu_storage_bytes(&current), 0);
    s::close(&sink);
    assert!(i::gpu_snapshot(&current).is_err() && i::pixels(&current).is_err());
    run::close(&r, false);
    pipelines::close(&cache);
    i::destroy(&current).unwrap();
    for (_, _, lease) in &mut snapshots { i::release_snapshot(lease); }
    host::close(&host);
}

fn image_scene(image: &image::t) -> Vec<scene::node> {
    vec![scene::clear(color::black()), scene::image(image, (0, 0), None, None).unwrap()]
}
fn mesh_scene(texture: &texture::t, viewport: Option<(i64, i64, i64, i64)>) -> Vec<scene::node> {
    let mesh = rays::mesh::plane(2.0 * 65.0 / 17.0, 2.0).unwrap();
    let camera = rays::camera::orthographic(2.0, vec3::create(0.0, 0.0, 2.0), vec3::zero()).unwrap();
    let mesh = scene3::mesh(Some(rays::material::matte(color::white())), Some(scene3::textured(Some(texture::filter::Nearest), texture.clone())), None, Some(scene3::cull::Cull_none), None, mesh);
    let world = scene3::create(None, Some(color::white()), None, None, vec![mesh]).unwrap();
    vec![scene::clear(color::black()), scene::view3d(viewport, &camera, world)]
}
fn mixed(image: &image::t, texture: &texture::t) -> Vec<scene::node> {
    vec![scene::clear(color::black()), scene::group(vec![scene::image(image, (0, 0), None, None).unwrap()]), scene::Layer::layer_break(), scene::group(mesh_scene(texture, Some((33, 0, 32, 17))).into_iter().skip(1).collect())]
}
fn verify_resident(canvas: &canvas::t, current: &i::t, scenes: &[Vec<scene::node>], converted: &s::Output, width: i64, height: i64, time: f64) {
    let bytes = pixels(converted, width);
    let reference_image = image::upload_rgba(None, width, height, &bytes).unwrap();
    let colors = bytes.chunks_exact(4).map(|p| color::rgba(p[0] as i64, p[1] as i64, p[2] as i64, p[3] as i64)).collect();
    let reference_texture = texture::create_owned(width, height, colors).unwrap();
    let references = [image_scene(&reference_image), mesh_scene(&reference_texture, None), mixed(&reference_image, &reference_texture)];
    for (index, (nodes, reference)) in scenes.iter().zip(references).enumerate() {
        let reads = i::readbacks(current);
        render(canvas, nodes);
        render(canvas, nodes);
        let uploaded = canvas::Private::native_stats(canvas).unwrap().uploaded_bytes;
        render(canvas, nodes);
        assert_eq!(canvas::Private::native_stats(canvas).unwrap().uploaded_bytes, uploaded);
        assert_eq!(i::readbacks(current), reads);
        let actual = snapshot(canvas);
        let fresh = canvas::create(65, 17).unwrap();
        render(&fresh, &reference);
        assert_eq!(snapshot(&fresh), actual);
        canvas::destroy(&fresh).unwrap();
        println!("image_sink,resident_{}_source{}x{},65,17,{time:.2},0,0,0", ["image", "mesh", "mixed"][index], width, height);
    }
    image::destroy(&reference_image).unwrap();
}
fn expire(canvas: &canvas::t, current: &i::t, scenes: &[Vec<scene::node>]) {
    for nodes in scenes {
        match scene::Layer::stage_native_render_checked(None, 65, 17, nodes) {
            Err(scene::Layer::native_error::Resource(error)) => assert_eq!(error.kind, ErrorKind::Destroyed),
            Ok(_) => (),
            Err(e) => { let mut message = String::new(); scene::Layer::pp_native_error(&mut message, &e); panic!("resident expiry staging: {message}"); }
        }
        let reads = i::readbacks(current);
        assert!(canvas::render(None, canvas, nodes).is_err());
        assert_eq!(i::readbacks(current), reads);
    }
}
fn resident_consumers() {
    if !common::gpu() { return; }
    common::release_keepalive();
    let before = ogpu_core::impl_::live_handles().unwrap();
    assert!(common::gpu());
    let gpu = common::lease();
    let cache = pipelines::create(&|| 0.0, &rr::gpu_device(&gpu));
    let sink = tp::ok(s::create(&gpu));
    let produce = |width, height, time| {
        let (p, _) = packed(width, height, "[uv.x uv.y t 1]");
        let r = runner(&gpu, &cache, &p);
        let output = tp::ok(s::convert(&sink, width, height, &dispatch(&r, &p, time)));
        run::close(&r, false);
        output
    };
    let first = produce(65, 17, 0.25);
    let current = i::of_gpu(65, 17, source(&first)).unwrap();
    let view = texture::of_image(&current).unwrap();
    let identity = texture::Private::identity(&view);
    assert_eq!(texture::size(&view).unwrap(), (65, 17));
    assert!(texture::pixels(&view).is_err());
    assert!(texture::sample(&view, 0.5, 0.5).is_err());
    assert!(texture::generate_mipmaps(&view).is_err());
    assert!(texture::Private::levels(&view).is_err());
    assert!(texture::subsection(0, 0, 1, 1, &view).is_err());
    assert_eq!((i::cpu_storage_bytes(&current), i::readbacks(&current)), (0, 0));
    let canvas = canvas::create(65, 17).unwrap();
    let displayed = image::of_resource(current.clone()).unwrap();
    let meshed = mesh_scene(&view, None);
    let mut scenes = vec![image_scene(&displayed), meshed.clone(), mixed(&displayed, &view)];
    let staged = checked(&scenes[1]);
    assert_eq!(staged.mesh_images.len(), 1);
    assert_eq!(i::identity(&staged.mesh_images[0]), i::identity(&current));
    let sampled = staged.scene3[0].entries[0].texture.as_ref().unwrap();
    assert!(sampled.gpu.is_some() && sampled.levels[0].bytes.is_empty());
    verify_resident(&canvas, &current, &scenes, &first, 65, 17, 0.25);
    assert_eq!(i::readbacks(&current), 0);
    let (_, _, _, saved, mut lease) = i::borrow_snapshot(&current).unwrap();
    let saved_copy = saved.to_vec();
    let generation = i::generation(&current);
    let second = produce(65, 17, 0.75);
    assert_eq!(i::generation(&current), generation);
    expire(&canvas, &current, &scenes);
    i::replace_gpu_source(&current, 65, 17, source(&second)).unwrap();
    verify_resident(&canvas, &current, &scenes, &second, 65, 17, 0.75);
    let resized = produce(17, 5, 0.5);
    expire(&canvas, &current, &scenes);
    i::replace_gpu_source(&current, 17, 5, source(&resized)).unwrap();
    i::replace(&current, 17, 5, &vec![255; 17 * 5 * 4]).unwrap();
    assert!(texture::of_image(&current).is_err());
    assert!(matches!(scene::Layer::stage_native_render_checked(None, 65, 17, &meshed), Err(scene::Layer::native_error::Message(message)) if message.ends_with("borrowed GPU texture needs an explicit CPU snapshot")));
    assert_eq!(i::readbacks(&current), 1);
    i::replace_gpu_source(&current, 17, 5, source(&resized)).unwrap();
    assert_eq!(texture::Private::identity(&view), identity);
    assert_eq!(texture::size(&view).unwrap(), (17, 5));
    let resized_image = image::of_resource(current.clone()).unwrap();
    scenes = vec![image_scene(&resized_image), meshed, mixed(&resized_image, &view)];
    verify_resident(&canvas, &current, &scenes, &resized, 17, 5, 0.5);
    assert_eq!(&saved[..], &saved_copy[..]);
    let device = b::create_device(ogpu_core::impl_::create_driver().unwrap()).unwrap();
    let texture = b::create_texture(&device, ogpu_core::types::TextureDescriptor { label: None, width: 17, height: 5, depth: 1, mip_levels: 1, sample_count: 1, format: ogpu_core::types::TextureFormat::Rgba8Unorm, usage: vec![ogpu_core::types::TextureUsage::TextureBinding, ogpu_core::types::TextureUsage::TextureCopySrc] }).unwrap();
    let foreign = texture.clone();
    i::replace_gpu_source(&current, 17, 5, Rc::new(move || Some(foreign.clone()))).unwrap();
    for nodes in &scenes { assert!(matches!(canvas::render(None, &canvas, nodes), Err(message) if message.ends_with("GPU image belongs to another renderer"))); }
    assert_eq!(i::readbacks(&current), 1);
    b::destroy_texture(&texture).unwrap();
    b::destroy_device(&device).unwrap();
    i::replace_gpu_source(&current, 17, 5, source(&resized)).unwrap();
    let texture = s::texture(&resized).unwrap();
    i::destroy(&current).unwrap();
    assert!(!b::texture_destroyed(&texture));
    expire(&canvas, &current, &scenes);
    assert_eq!(&saved[..], &saved_copy[..]);
    canvas::destroy(&canvas).unwrap();
    i::release_snapshot(&mut lease);
    s::close(&sink);
    pipelines::close(&cache);
    common::release_keepalive();
    assert_eq!(ogpu_core::impl_::live_handles().unwrap(), before);
}
fn main() {
    common::window::run(&[("native_pixel_parity", native_pixel_parity), ("qualified_publication", qualified_publication), ("resident_consumers", resident_consumers)]);
}
