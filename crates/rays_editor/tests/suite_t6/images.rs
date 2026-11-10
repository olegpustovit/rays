//!  and . Every case needs the Vulkan device (the owner
//! opens a GPU service), so each starts with `common::gpu()`.
#![allow(dead_code, unused_imports)]
use std::cell::RefCell;
use std::sync::atomic::{AtomicI64, Ordering};
use std::rc::Rc;
use std::sync::Arc;

use crate::common;
use super::support::{load, options, repo_root};
use flow_eval::{Plan, State};
use flow_syntax::{ty, value::Value};
use rays_diagnostic::Diagnostic;
use rays_editor::{editor, workspace};
use sop::context::dependencies as deps;
use sop::{image as simage, node, payload};

type Ed = editor::Editor;

fn ok<T>(r: Result<T, Diagnostic>) -> T {
    r.unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)))
}

fn editor_with(doc: &editor_document::workspace_doc::T, o: editor::Create) -> Ed {
    editor::create(o, doc, None, editor::prepare_fn(), editor::draw_fn()).unwrap_or_else(|m| panic!("editor did not open: {m}"))
}

fn editor(doc: &editor_document::workspace_doc::T) -> Ed {
    editor_with(doc, options())
}

/// ~domains
fn editor_d(doc: &editor_document::workspace_doc::T, domains: i64) -> Ed {
    editor_with(doc, editor::Create { domains: Some(domains), ..options() })
}

fn workspace(producer: &str) -> editor_document::workspace_doc::T {
    load(&format!("(workspace images (graph img :context image {producer}) (graph picture :context draw (draw/image (ref img) :at [1 2 0] :scale 2.0 :angle 0.1)))"))
}

fn image_value(id: i64) -> Value {
    Value::Deferred(ty::image(), id)
}

fn node_of(plan: &Plan, kind: &str) -> flow_eval::Node {
    plan.nodes.iter().find(|n| n.kind == kind).cloned().unwrap_or_else(|| panic!("no {kind} node"))
}

fn live(time: f64) -> input::Frame {
    input::at_time(time)
}

/// `Editor.Private.with_images ?state ?live ?plan owner (fun ~image ~texture -> ...)`, answering the closure's value.
fn with_images<R>(
    ed: &Ed,
    state: Option<&State>,
    live: Option<&input::Frame>,
    plan: Option<&Plan>,
    mut f: impl FnMut(&dyn Fn(&Value) -> Result<rays::image::Image, Diagnostic>, &dyn Fn(&Value) -> Result<rays::texture::Texture, Diagnostic>) -> R,
) -> R {
    let out: RefCell<Option<R>> = RefCell::new(None);
    editor::private::with_images(ed, state, live, plan, &mut |image, texture| {
        *out.borrow_mut() = Some(f(image, texture));
    });
    out.into_inner().expect("with_images ran")
}

fn payload_of(ed: &Ed, v: &Value, live_: Option<&input::Frame>) -> Result<simage::T, Diagnostic> {
    editor::private::image_payload(ed, v, None, live_)
}

fn bytes_of(i: &simage::T) -> Vec<u8> {
    simage::rgba8(i).expect("rgba8 storage").to_vec()
}

fn rgba(i: &simage::T) -> Vec<u8> {
    match simage::rgba8(i) {
        Some(b) => b.to_vec(),
        None => simage::storage(i).unwrap().iter().map(|v| (v * 255.).round() as u8).collect(),
    }
}

fn pixels(i: &rays::image::Image) -> Vec<u8> {
    rays::image::pixels(i).expect("pixels")
}

/// Image captures: effective owner seed/grain/domains match independent cooks and exact domain bytes.
fn capture_context() {
    if !common::gpu() {
        return;
    }
    let observed = Arc::new(AtomicI64::new(0));
    let source = {
        let observed = observed.clone();
        move || -> node::T {
            let observed = observed.clone();
            let dependencies = [deps::Fact::Seed, deps::Fact::Grain, deps::Fact::Domains].iter().fold(deps::STATIC, |a, f| deps::union(a, deps::one(*f)));
            node::build::make(
                node::build::Args { cook_mode: node::CookMode::Generator, dependencies, version: 1, ..node::build::args("image_context") },
                None,
                Arc::new(move |_, context, _| {
                    observed.store(sop::context::domains(context), Ordering::SeqCst);
                    let point = (sop::context::seed(context) as f64 / 256., sop::context::grain(context) as f64 / 1024., 0.);
                    node::build::cook(&flow_sop::lisp_sop::snapshot(&rdk_ops::line_geometry::points(&[point]).unwrap()), context, &[])
                }),
            )
        }
    };
    let factory = {
        let source = source.clone();
        sop::edit_graph::factory(sop::edit_graph::factory_args("image_context", "Image context", &["Test"]), 0, Arc::new(move |_| Ok(source())))
    };
    let mut factories = vec![factory];
    factories.extend(sop::nodes::factories());
    let doc = workspace::load(
        None,
        Vec::new(),
        Some(&factories),
        "(workspace context\n    (graph mesh :context sop (let* [geo (sop/image_context)\n      p (array/sum (sop/attr geo :P))\n      img (image/map (fn [uv] [(+ uv.x p.x) p.y 0.5 1]) :width 65 :height 17)] geo)))",
    )
    .unwrap_or_else(|ds| panic!("{}", super::support::diag_text(&ds)));
    let run = |seed: i64, domains: i64| -> Vec<u8> {
        let grain = 257;
        let owner = editor_with(&doc, editor::Create { factories: Some(factories.clone()), seed: Some(seed), grain: Some(grain), domains: Some(domains), ..options() });
        let plan = editor::private::image_plan(&owner);
        let n = node_of(&plan, "image/map");
        observed.store(0, Ordering::SeqCst);
        let payload = ok(payload_of(&owner, &image_value(n.id), None));
        assert_eq!(observed.load(Ordering::SeqCst), domains);
        let f = match n.args.iter().find(|(k, _)| k == "function").map(|(_, v)| v.clone()) {
            Some(Value::Fn(f)) => f,
            _ => panic!("function"),
        };
        let sources = flow_sop::attribute_kernel::sources(&Value::Fn(f));
        let kernel = ok(flow_sop::image_kernel::prepare(None, None, None, 0, 65, 17, f, &sources, &[source()]));
        let mut session = sop::session::create(0, 0).unwrap();
        let context = sop::context::create(sop::context::Options { seed: Some(seed), grain: Some(grain), domains: Some(domains), ..Default::default() }).unwrap();
        let out = sop::session::cook(&mut session, None, &context, &flow_sop::image_kernel::node(None, &kernel)).unwrap_or_else(|e| panic!("{}", rays_diagnostic::error_to_string(&e)));
        let expected = payload::image(&out.payload).unwrap();
        sop::session::close(&mut session);
        let bytes = bytes_of(&payload);
        assert!(simage::rgba8(&expected) == Some(&bytes[..]) && bytes[1] == 64);
        editor::close(&owner);
        bytes
    };
    assert!(run(17, 1) == run(17, 8) && run(49, 1) == run(49, 8) && run(17, 1) != run(49, 1));
}

fn function_of(n: &flow_eval::Node) -> Value {
    n.args.iter().find(|(k, _)| k == "function").map(|(_, v)| v.clone()).expect("function arg")
}

fn result_of(evaluated: &flow_eval::Eval, name: &str) -> Value {
    evaluated.results.iter().find(|(n, _)| n == name).map(|(_, v)| v.clone()).unwrap_or_else(|| panic!("no result {name}"))
}

/// Captures of a non-default SOP instance: current live source, stable payload identity, one/eight-domain bytes.
fn capture_nondefault() {
    if !common::gpu() {
        return;
    }
    let doc = load("(workspace captures\n    (graph mesh :context sop [(offset : float 0.015625)]\n      (let* [geo (sop/box :consolidate_points true :center [(+ offset (* t 0.015625)) 0 0])\n             p (reduce + [0 0 0] (sop/attr geo :P))\n             img (image/map (fn [uv] [(+ uv.x p.x) uv.y 0.5 1]) :width 7 :height 3)] geo))\n    (graph other :context sop (ref mesh :offset 0.03125)))");
    let run = |domains: i64| -> (Vec<u8>, Vec<u8>) {
        let owner = editor_d(&doc, domains);
        let plan = editor::private::image_plan(&owner);
        let instance = plan.instances.iter().position(|i| i.graph == "mesh" && !i.default).expect("instance") as i64;
        let producer = plan.nodes.iter().find(|n| n.kind == "image/map" && n.inst == instance).expect("producer").clone();
        let image = image_value(producer.id);
        assert!(!flow_eval::frame_dependent(&function_of(&producer)));
        let at = |time: f64| ok(payload_of(&owner, &image, Some(&live(time))));
        let first = at(0.);
        let bytes = bytes_of(&first);
        assert_eq!(bytes[0], 82);
        assert!(simage::ptr_eq(&at(0.), &first));
        let later = at(1.);
        let changed = bytes_of(&later);
        assert!(changed != bytes && simage::rgba8(&first) == Some(&bytes[..]));
        assert!(simage::ptr_eq(&at(1.), &later) && editor::private::image_stats(&owner) == (0, 0));
        editor::close(&owner);
        (bytes, changed)
    };
    assert!(run(1) == run(8));
}

/// Direct nested resource resolution and stateful source snapshot parity without mutating caller state.
fn capture_nested_stateful() {
    if !common::gpu() {
        return;
    }
    let nested = load("(workspace nested\n    (graph mesh :context sop\n      (let* [base (sop/box :consolidate_points true :center [0.015625 0 0])\n             noise (image/map (fn [uv] [uv.x uv.y 0.5 1]) :width 3 :height 2)\n             textured (sop/with_attr base :uv (exact (map (fn [p] [0.25 0.5 0]) (sop/attr base :P))))\n             geo (sop/attr_from_image textured noise :attribute \"sample\")\n             p (reduce + [0 0 0] (sop/attr geo :P))\n             img (image/map (fn [uv] [(+ uv.x p.x) uv.y 0.5 1]) :width 7 :height 3)]\n        geo)))");
    let owner = editor(&nested);
    let plan = editor::private::image_plan(&owner);
    let node = plan.nodes.iter().find(|n| n.kind == "image/map" && n.site == vec!["mesh".to_owned(), "img".to_owned()]).expect("img node").clone();
    let image = ok(payload_of(&owner, &image_value(node.id), None));
    assert!(simage::width(&image) == 7 && simage::height(&image) == 3);
    editor::close(&owner);

    let doc = load("(workspace stateful\n    (graph mesh :context sop\n      (let* [bias (state [n 0.015625] (+ n 0.015625))\n             geo (sop/box :consolidate_points true :center [bias 0 0])\n             p (reduce + [0 0 0] (sop/attr geo :P))\n             img (image/map (fn [uv] [(+ uv.x p.x) uv.y 0.5 1]) :width 7 :height 3)]\n        geo)))");
    let state = flow_eval::create_state();
    let stamp = flow_eval::state_stamp(&state);
    let snapshot = |first: bool, domains: i64| -> Vec<u8> {
        let owner = editor_d(&doc, domains);
        let node = node_of(&editor::private::image_plan(&owner), "image/map");
        let image = image_value(node.id);
        let at = |frame: i64| ok(editor::private::image_payload(&owner, &image, Some(&state), Some(&input::Frame { frame, ..live(frame as f64) })));
        if first {
            at(0);
        }
        let result = at(1);
        assert_eq!(flow_eval::state_stamp(&state), stamp);
        let bytes = bytes_of(&result);
        editor::close(&owner);
        bytes
    };
    let sequential = snapshot(true, 1);
    assert!(sequential == snapshot(false, 1) && sequential == snapshot(true, 8));
}

type Cmd = String;

fn is_image_command(c: &String) -> bool {
    c.starts_with("Image(")
}

fn render_picture(plan: &Plan, picture: &Value, live_: &input::Frame, size: (i64, i64), image: Rc<dyn Fn(&Value) -> Result<rays::image::Image, Diagnostic>>) -> Vec<Cmd> {
    let prepared = ok(flow_native::drawing::prepare(&flow_native::drawing::PrepareOptions::default(), plan, picture));
    let scene = ok(flow_native::drawing::render_prepared(&prepared, live_, size, &flow_native::drawing::RenderOptions { image: Some(image), ..Default::default() }));
    rays::scene::layer::commands(None, &scene).expect("commands").iter().map(|c| format!("{c:?}")).collect()
}

/// image/map: initial-domain drawing, live callable captures, stable image identity and close.
fn image_map_drawing() {
    if !common::gpu() {
        return;
    }
    for producer in [
        "(image/map (fn [uv] [(+ uv.x (* t 0.25)) uv.y 0.5 1]) :width 65 :height 3)",
        "(let* [bias (* t 0.25)] (image/map (fn [uv] [(+ uv.x bias) uv.y 0.5 1]) :width 65 :height 3))",
    ] {
        let doc = workspace(producer);
        let owner = editor(&doc);
        let evaluated = ok(flow_eval::static_eval(&doc.checked));
        let image_value = result_of(&evaluated, "img");
        let image_at = |time: f64| with_images(&owner, None, Some(&live(time)), Some(&evaluated.plan), |image, _| ok(image(&image_value)));
        let first = image_at(0.);
        assert_eq!(rays::image::get_size(&first).unwrap(), (65, 3));
        let px = pixels(&first);
        assert_eq!(&px[0..4], &[2, 42, 128, 255]);
        assert!(rays::image::same(&image_at(0.), &first));
        assert!(rays::image::same(&image_at(1.), &first));
        assert!(pixels(&first) != px);
        let picture = result_of(&evaluated, "picture");
        let (o2, plan2) = (owner.clone(), evaluated.plan.clone());
        let image: Rc<dyn Fn(&Value) -> Result<rays::image::Image, Diagnostic>> = Rc::new(move |v| with_images(&o2, None, Some(&live(1.)), Some(&plan2), |image, _| image(v)));
        let commands = render_picture(&evaluated.plan, &picture, &live(1.), (65, 3), image);
        assert!(commands.iter().any(is_image_command));
        assert_eq!(editor::private::image_stats(&owner), (1, 0));
        editor::close(&owner);
        assert_eq!(editor::private::image_stats(&owner), (1, 1));
    }
}

/// image/map: replan, body edit and resize replace pixels under one owned image identity.
fn image_map_replan() {
    if !common::gpu() {
        return;
    }
    let doc = workspace("(image/map (fn [uv] [uv.x uv.y 0.5 1]) :width 65 :height 3)");
    let owner = editor(&doc);
    let resolve = |doc: &editor_document::workspace_doc::T| {
        let evaluated = ok(flow_eval::static_eval(&doc.checked));
        let v = result_of(&evaluated, "img");
        with_images(&owner, None, Some(&live(0.)), Some(&evaluated.plan), |image, _| ok(image(&v)))
    };
    let image = resolve(&doc);
    let resized = workspace("(image/map (fn [uv] [uv.x uv.y 0.5 1]) :width 17 :height 5)");
    assert!(rays::image::same(&resolve(&resized), &image) && rays::image::get_size(&image).unwrap() == (17, 5));
    let previous = pixels(&image);
    let edited = workspace("(image/map (fn [uv] [uv.y uv.x 0.5 1]) :width 17 :height 5)");
    assert!(rays::image::same(&resolve(&edited), &image));
    assert!(pixels(&image) != previous);
    assert_eq!(editor::private::image_stats(&owner), (1, 0));
    editor::close(&owner);
    assert_eq!(editor::private::image_stats(&owner), (1, 1));
}

fn sample_png() -> String {
    format!("{}/../../crates/sdl3/fixtures/sample.png", env!("CARGO_MANIFEST_DIR").replace('\\', "/"))
}

/// images: two-pixel CPU noise, drawing modes, one load/upload, exact ownership and missing-file diagnostic.
fn images_noise_and_load() {
    if !common::gpu() {
        return;
    }
    let doc = workspace("(image/noise :width 2 :height 2 :frequency 0.3 :seed 31)");
    let evaluated = ok(flow_eval::static_eval(&doc.checked));
    let value = result_of(&evaluated, "img");
    let owner = editor(&doc);
    let image = ok(editor::private::image(&owner, &value));
    assert_eq!(rays::image::get_size(&image).unwrap(), (2, 2));
    assert!(rays::image::same(&ok(editor::private::image(&owner, &value)), &image));
    let oracle = pixels(&image);
    // UI image painting is covered by PXUI; this case checks image ownership and bytes.
    assert_eq!(pixels(&image), oracle);
    let picture = result_of(&evaluated, "picture");
    let commands: RefCell<Option<Vec<Cmd>>> = RefCell::new(None);
    for domains in [1, 1, 1, 8] {
        parallel::run_unit(Some(domains), &|| {
            let o3 = owner.clone();
            let actual = render_picture(&evaluated.plan, &picture, &live(0.), (32, 32), Rc::new(move |v| editor::private::image(&o3, v)));
            assert!(actual.iter().any(is_image_command));
            match &*commands.borrow() {
                None => {}
                Some(expected) => {
                    assert!(&actual == expected);
                    assert_eq!(pixels(&image), oracle);
                    return Ok(());
                }
            }
            *commands.borrow_mut() = Some(actual);
            assert_eq!(pixels(&image), oracle);
            Ok(())
        }).unwrap();
    }
    assert_eq!(editor::private::image_stats(&owner), (1, 0));
    editor::close(&owner);
    assert_eq!(editor::private::image_stats(&owner), (1, 1));

    let png = sample_png();
    let doc = load(&format!("(workspace images (graph a :context image (image/load \"{png}\")) (graph b :context image (image/load \"{png}\")) (graph picture :context draw (draw/merge (draw/image (ref a)) (draw/image (ref b)))))"));
    let owner = editor(&doc);
    let evaluated = ok(flow_eval::static_eval(&doc.checked));
    let a = ok(editor::private::image(&owner, &result_of(&evaluated, "a")));
    let b = ok(editor::private::image(&owner, &result_of(&evaluated, "b")));
    assert!(rays::image::same(&a, &b) && editor::private::image_stats(&owner) == (1, 0));
    editor::close(&owner);
    assert_eq!(editor::private::image_stats(&owner), (1, 1));

    let doc = workspace(&format!("(image/load \"{png}\")"));
    let owner = editor(&doc);
    let evaluated = ok(flow_eval::static_eval(&doc.checked));
    let img = result_of(&evaluated, "img");
    let first = ok(editor::private::image(&owner, &img));
    assert!(rays::image::same(&ok(editor::private::image(&owner, &img)), &first));
    assert_eq!(editor::private::image_stats(&owner), (1, 0));
    editor::close(&owner);
    assert_eq!(editor::private::image_stats(&owner), (1, 1));

    let doc = workspace("(image/load \"missing-rgbaa-image-file.png\")");
    let owner = editor(&doc);
    let evaluated = ok(flow_eval::static_eval(&doc.checked));
    let img = result_of(&evaluated, "img");
    assert!(matches!(editor::private::image(&owner, &img), Err(d) if d.code == "E_IMAGE"));
    assert_eq!(editor::private::image_stats(&owner), (0, 0));
    editor::close(&owner);
}

/// Resources snapshot before worker submission, exact one/eight-domain payload identity.
fn lane_resources_snapshot() {
    if !common::gpu() {
        return;
    }
    let doc = workspace("(image/load \"unused.png\")");
    let lowered = ok(flow_sop::lower::of_checked(None, flow_sop::lower::Options::default(), &doc.checked));
    let graph = lowered.graphs[0].clone();
    let lane = flow_sop::value_lane::create(None);
    assert!(matches!(flow_sop::value_lane::resolve(None, None, &lane, 0., &graph.network), Err(d) if d.code == "E_IMAGE"));
    let payload_ = simage::create(1, 1, &[0.2, 0.3, 0.4, 1.]).unwrap();
    let calls = Arc::new(AtomicI64::new(0));
    let (calls2, payload2, plan_id, compiled) = (calls.clone(), payload_.clone(), lowered.plan.id, lowered.compiled.clone());
    let resolver: flow_sop::lower::ImageResolver = Arc::new(move |_gpu, context, plan, _state, _live, _v| {
        assert_eq!(plan.id, plan_id);
        let context = context.expect("context");
        assert!(context.compiled == compiled);
        calls2.fetch_add(1, Ordering::SeqCst);
        Ok(payload2.clone())
    });
    flow_sop::lower::with_images(None, None, resolver, &mut || {
        flow_sop::value_lane::reset(false, &lane);
        let resolved = ok(flow_sop::value_lane::resolve(None, None, &lane, 1., &graph.network));
        let n = sop::edit_graph::compile_node(&resolved.geometry, graph.root.unwrap()).unwrap();
        let mut session = sop::session::create(16, 65536).unwrap();
        for domains in [1, 8] {
            let context = sop::context::create(sop::context::Options { domains: Some(domains), ..Default::default() }).unwrap();
            let out = sop::session::cook(&mut session, None, &context, &n).unwrap_or_else(|e| panic!("{}", rays_diagnostic::error_to_string(&e)));
            assert!(simage::ptr_eq(&payload::image(&out.payload).unwrap(), &payload_));
        }
        sop::session::close(&mut session);
    });
    assert_eq!(calls.load(Ordering::SeqCst), 1);
}

/// Scene image textures preserve affine, cull, blend, depth and scene settings.
fn scene_textures_preserved() {
    use rays::scene3 as s3;
    let texture = s3::textured(None, rays::texture::init(2, 2, &|_, _| rays_math::color::white()).unwrap());
    let plane = s3::plane(Some(s3::Cull::CullFront), 2., 3.).unwrap();
    let scene = s3::create(None, Some(rays_math::color::red()), None, Some(4), vec![s3::translate(rays_math::vec3::create(2., 3., 4.), vec![s3::with_blend(s3::Blend::Add, vec![s3::with_depth(s3::depth_state(Some(false)), vec![plane])])])]).unwrap();
    let textured = s3::walk::with_texture(&texture, &scene);
    let (a, b) = (s3::walk::drawings(&scene), s3::walk::drawings(&textured));
    assert_eq!(a.len(), b.len());
    for (original, t) in a.iter().zip(b.iter()) {
        assert!(t.texture.is_some());
        assert!(original.texture.is_none());
        assert!(original.cull == t.cull && original.blend == t.blend && original.depth == t.depth && original.transform == t.transform);
    }
    assert_eq!(s3::walk::samples(&textured), 4);
    assert!(s3::walk::ambient(&textured) == rays_math::color::red());
}

/// Image noise live stamps: the owner's pixels change with time and are all released at close.
fn image_oracle_noise_owner() {
    if !common::gpu() {
        return;
    }
    let producer = "(image/noise :width 2 :height 2 :frequency (+ 0.3 (* 0.01 t)) :seed 31)";
    let document = load(&format!("(workspace image_oracle (graph image :context image {producer})\n    (graph grid :context sop\n      (sop/attr_from_image (sop/grid :rows 2 :columns 2 :uv_attribute \"uv\") (ref image)))\n    (graph picture :context draw (draw/image (ref image)))\n    (graph scene :context scene (scene/geometry (ref grid) :texture (ref image))))"));
    let owner = editor(&document);
    let evaluated = ok(flow_eval::static_eval(&document.checked));
    let img = result_of(&evaluated, "image");
    let px = |time: f64| with_images(&owner, None, Some(&live(time)), Some(&evaluated.plan), |image, _| pixels(&ok(image(&img))));
    assert!(px(0.) != px(10.));
    editor::close(&owner);
    let (created, destroyed) = editor::private::image_stats(&owner);
    assert!(created > 0 && created == destroyed);
}

/// Image capacity: exact-only entries reserve recursive parents, failed resolution consumes no slot or runtime image.
fn capacity() {
    if !common::gpu() {
        return;
    }
    let maps: String = (0..64).map(|i| format!("(graph m{i} :context image (image/map (fn [uv] [uv.x uv.y 0.5 1]) :width 1 :height 1))")).collect::<Vec<_>>().join(" ");
    let doc = load(&format!("(workspace capacity {maps} (graph drawing :context draw (draw/image (ref m63))) (graph rendered :context image (image/render (ref drawing) :width 1 :height 1)))"));
    let owner = editor(&doc);
    let plan = editor::private::image_plan(&owner);
    let nodes: Vec<&flow_eval::Node> = plan.nodes.iter().filter(|n| n.kind == "image/map").collect();
    let exact = |n: &flow_eval::Node| payload_of(&owner, &image_value(n.id), None);
    let retained = ok(exact(nodes[0]));
    for n in &nodes[1..63] {
        ok(exact(n));
    }
    assert_eq!(editor::private::image_stats(&owner), (0, 0));
    let render = node_of(&plan, "image/render");
    let refused = || assert!(matches!(exact(&render), Err(d) if d.code == "E_IMAGE"));
    refused();
    ok(exact(nodes[63]));
    refused();
    assert!(simage::ptr_eq(&ok(exact(nodes[0])), &retained));
    assert_eq!(editor::private::image_stats(&owner), (0, 0));
    editor::close(&owner);
}

// ----  ----

use runtime_resources::image as I;

fn handles() -> i64 {
    ogpu_core::impl_::live_handles().expect("live handles")
}

fn frame_at(time: f64, size: (i64, i64), count: i64, pointer: (f64, f64)) -> rays::frame::Frame {
    rays::frame::Frame { input: input::Frame { dt: 1., frame: count, tick: count, size, pointer, ..input::at_time(time) }, pixel_scale: (1., 1.), fps: 1., mouse_delta: (0., 0.) }
}

fn temp_dir(tag: &str) -> std::path::PathBuf {
    let dir = std::env::temp_dir().join(format!("rays-{tag}-{}-{}", std::process::id(), std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).map(|d| d.as_nanos()).unwrap_or(0)));
    std::fs::create_dir_all(&dir).expect("temp dir");
    dir
}

fn res(i: &rays::image::Image) -> runtime_resources::image::Image {
    rays::image::resource(i)
}

/// Canvas playback refreshes image/map functions with captured time.
fn canvas_playback() {
    if !common::gpu() {
        return;
    }
    let doc = load("(workspace animated\n    (graph picture :context draw\n      (let* [bias (* t 0.2)]\n        (draw/image (image/map (fn [uv] [bias uv.y 0 1]) :width 64 :height 64))))\n    (graph editor :context editor (ui/workspace (ui/canvas (ref picture)))))");
    let mut owner = editor_d(&doc, 1);
    let canvas = rays::canvas::create(900, 640).unwrap();
    let mut sample = |count: i64| {
        let frame = frame_at(count as f64, (900, 640), count, (-100., -100.));
        owner = editor::update(owner.clone(), &frame);
        rays::canvas::render(None, &canvas, &editor::scene(&owner, &frame)).unwrap();
        rays::canvas::pixel(&canvas, 10, 40).unwrap()
    };
    let first = sample(0);
    assert!(first != sample(1));
    editor::close(&owner);
    let _ = rays::canvas::destroy(&canvas);
}

fn rgba_diff_max(actual: &[u8], expected: &[u8]) -> i32 {
    actual.iter().zip(expected).map(|(a, b)| (*a as i32 - *b as i32).abs()).max().unwrap_or(0)
}

/// Native geometry captures: P/Cd, two instances, exact CPU domains, resident GPU images, parent freshness.
fn native_geometry_captures() {
    if !common::gpu() {
        return;
    }
    let before = handles();
    let doc = load("(workspace captured\n    (graph drawing :context draw [(image : image (image/noise :width 65 :height 17))] (draw/image image))\n    (graph mesh :context sop [(offset : float 0.015625)]\n      (let* [base (sop/box :consolidate_points true :center [(+ offset (* t 0.015625)) 0 0])\n             geo (sop/with_attr base :Cd (exact (map (fn [p] [0.015625 0.03125 0.0625]) (sop/attr base :P))))\n             p (reduce + [0 0 0] (sop/attr geo :P))\n             c (reduce + [0 0 0] (sop/attr geo :Cd))\n             img (image/map (fn [uv] [(+ uv.x p.x) (+ uv.y c.x) c.y 1]) :width 65 :height 17)\n             parent (image/render (ref drawing :image img) :width 65 :height 17)]\n        geo))\n    (graph other :context sop (ref mesh :offset 0.03125))\n    (graph backdrop :context draw (draw/background \"#102030\"))\n    (graph fixed :context image (image/render (ref backdrop) :width 65 :height 17)))");
    let display = editor_d(&doc, 8);
    let cpu1 = editor_d(&doc, 1);
    let cpu8 = editor_d(&doc, 8);
    editor::private::gpu_qualification(&display);
    let initial = editor::private::image_gpu_stats(&display).0;
    let value = |owner: &Ed, override_: bool, kind: &str, graph: &str| -> Value {
        let plan = editor::private::image_plan(owner);
        let n = plan.nodes.iter().find(|n| n.kind == kind && plan.instances[n.inst as usize].graph == graph && plan.instances[n.inst as usize].default != override_).expect("node");
        image_value(n.id)
    };
    let exact = |owner: &Ed, time: f64, override_: bool, kind: &str, graph: &str| ok(payload_of(owner, &value(owner, override_, kind, graph), Some(&live(time))));
    let resolve = |time: f64, override_: bool, kind: &str, graph: &str| with_images(&display, None, Some(&live(time)), None, |image, _| ok(image(&value(&display, override_, kind, graph))));
    let fixed = resolve(0., false, "image/render", "fixed");
    let generation = |i: &rays::image::Image| I::generation(&res(i));
    let fixed_generation = generation(&fixed);
    let images: Vec<rays::image::Image> = (0..2).map(|i| resolve(0., i == 1, "image/map", "mesh")).collect();
    let saved = exact(&cpu1, 0., true, "image/map", "mesh");
    let saved_bytes = rgba(&saved);
    let parent = resolve(0., false, "image/render", "mesh");
    let parent_generation = generation(&parent);
    for time in [0., 1., 2.] {
        let before_stats = editor::private::image_gpu_stats(&display).0;
        for (i, image) in images.iter().enumerate() {
            let override_ = i == 1;
            assert!(rays::image::same(&resolve(time, override_, "image/map", "mesh"), image));
            assert_eq!(I::cpu_storage_bytes(&res(image)), 0);
            let expected = rgba(&exact(&cpu1, time, override_, "image/map", "mesh"));
            assert!(expected == rgba(&exact(&cpu8, time, override_, "image/map", "mesh")));
            let actual = pixels(image);
            let maximum = rgba_diff_max(&actual, &expected);
            println!("workspace_geometry_capture,override={override_},time={time:.2},max={maximum}");
            assert!(maximum <= 1);
        }
        assert!(rays::image::same(&resolve(time, false, "image/render", "mesh"), &parent));
        let actual = pixels(&parent);
        let expected = rgba(&exact(&cpu1, time, false, "image/render", "mesh"));
        assert!(expected == rgba(&exact(&cpu8, time, false, "image/render", "mesh")));
        assert!(rgba_diff_max(&actual, &expected) <= 1);
        assert!(rays::image::same(&resolve(time, false, "image/render", "fixed"), &fixed) && generation(&fixed) == fixed_generation);
        let after = editor::private::image_gpu_stats(&display).0;
        println!("workspace_geometry_capture,time={time:.2},status_reads={}", after.status_reads - before_stats.status_reads);
        // SOP factories retain their creation-time Measured policy: these small captures stay on CPU. Each warmed
        // frame runs two GPU image kernels; the first cooks measure the CPU, and the frame that first decides
        // calibrates the device (time 1).
        if time == 2. {
            assert_eq!(after.status_reads - before_stats.status_reads, 2);
        }
    }
    assert!(generation(&parent) > parent_generation && rgba(&saved) == saved_bytes);
    let stats = editor::private::image_gpu_stats(&display).0;
    println!("workspace_geometry_capture,startup_status_reads={},status_reads={},readback_bytes={}", initial.status_reads, stats.status_reads - initial.status_reads, stats.readback_bytes - initial.readback_bytes);
    assert_eq!(stats.readback_bytes - initial.readback_bytes, 0);
    for o in [&display, &cpu1, &cpu8] {
        editor::close(o);
    }
    assert_eq!(handles(), before);
}

/// Native nested image/map: frame and same-frame state captures refresh rendered pixels.
fn native_nested_map() {
    if !common::gpu() {
        return;
    }
    for stateful in [false, true] {
        let bias = if stateful { "(state [previous 0.0] (+ previous 0.2))" } else { "(* t 0.1)" };
        let channel = if stateful { "0.5" } else { "bias" };
        let scale = if stateful { " :scale (+ 1 (ref bias))" } else { "" };
        let doc = load(&format!("(workspace mapped\n      (graph bias :context value {bias})\n      (graph img :context image (let* [bias (ref bias)]\n        (image/map (fn [uv] [uv.x uv.y {channel} 1]) :width 65 :height 3)))\n      (graph drawing :context draw (draw/image (ref img){scale}))\n      (graph rendered :context image (image/render (ref drawing) :width 65 :height 3)))"));
        let owner = editor_d(&doc, 1);
        let evaluated = ok(flow_eval::static_eval(&doc.checked));
        let image_value = result_of(&evaluated, "rendered");
        let snapshot = |state: &State, live_: &input::Frame| {
            with_images(&owner, Some(state), Some(live_), Some(&evaluated.plan), |image, _| {
                let image = ok(image(&image_value));
                let px = pixels(&image);
                (image, px)
            })
        };
        let state = flow_eval::create_state();
        let live0 = live(0.);
        let live1 = input::Frame { frame: 1, ..live(1.) };
        if stateful {
            ok(flow_eval::run(&doc.checked, false, None, Some(&state), Some(&live0), 0.));
        }
        let (first, zero) = snapshot(&state, &live0);
        if stateful {
            ok(flow_eval::run(&doc.checked, false, None, Some(&state), Some(&live1), 1.));
        }
        let (second, one) = snapshot(&state, &live1);
        assert!(rays::image::same(&first, &second) && zero != one);
        if stateful {
            let fresh = flow_eval::create_state();
            ok(flow_eval::run(&doc.checked, false, None, Some(&fresh), Some(&live1), 1.));
            let (_, different) = snapshot(&fresh, &live1);
            assert!(different != one);
            let (_, restored) = snapshot(&state, &live1);
            assert!(restored == one);
        }
        assert_eq!(editor::private::image_stats(&owner), (2, 0));
        editor::close(&owner);
        assert_eq!(editor::private::image_stats(&owner), (2, 2));
    }
}

/// Native Canvas image snapshots: repeat cache, exact one/eight-domain bytes, close ownership, deterministic export.
fn native_rendered_snapshots() {
    if !common::gpu() {
        return;
    }
    let doc = load("(workspace rendered (graph drawing :context draw (draw/merge (draw/background \"#102030\") (draw/circle [4 4 0] 2 :fill \"#a0b0c0\"))) (graph rendered :context image (image/render (ref drawing) :width 8 :height 8)) (graph picture :context draw (draw/image (ref rendered))))");
    let owner = editor_d(&doc, 1);
    let evaluated = ok(flow_eval::static_eval(&doc.checked));
    let rendered = result_of(&evaluated, "rendered");
    let image = ok(editor::private::image(&owner, &rendered));
    let expected = pixels(&image);
    assert_eq!(rays::image::get_size(&image).unwrap(), (8, 8));
    assert!(rays::image::same(&ok(editor::private::image(&owner, &rendered)), &image));
    assert_eq!(editor::private::image_stats(&owner), (1, 0));
    for domains in [1, 8] {
        let fresh = editor_d(&doc, domains);
        let actual = ok(editor::private::image(&fresh, &rendered));
        assert!(pixels(&actual) == expected);
        editor::close(&fresh);
    }
    editor::close(&owner);
    assert_eq!(editor::private::image_stats(&owner), (1, 1));
    let (first, second) = (temp_dir("rendered-image"), temp_dir("rendered-image"));
    for d in [&first, &second] {
        if !export_or_skip(d, &doc) { let _ = std::fs::remove_dir_all(&first); let _ = std::fs::remove_dir_all(&second); return; }
    }
    let mut names: Vec<_> = std::fs::read_dir(&first).unwrap().map(|e| e.unwrap().file_name()).collect();
    names.sort();
    assert!(!names.is_empty());
    for name in names {
        assert!(std::fs::read(first.join(&name)).unwrap() == std::fs::read(second.join(&name)).unwrap());
    }
    let _ = std::fs::remove_dir_all(&first);
    let _ = std::fs::remove_dir_all(&second);
}

/// Legacy image stamps: display A, exact B, display A restores A pixels.
fn legacy_stamps() {
    if !common::gpu() {
        return;
    }
    let doc = load("(workspace stamp (graph img :context image\n    (image/noise :width 4 :height 4 :frequency (+ 0.1 (* t 0.01)) :seed 31)))");
    let owner = editor_d(&doc, 1);
    let node = editor::private::image_plan(&owner).nodes[0].clone();
    let value = image_value(node.id);
    let display = |time: f64| with_images(&owner, None, Some(&live(time)), None, |image, _| pixels(&ok(image(&value))));
    let first = display(0.);
    ok(payload_of(&owner, &value, Some(&live(10.))));
    assert!(display(0.) == first);
    editor::close(&owner);
}

/// Resident Canvas dimensions: omitted axes, nested resize, fixed cache, independent CPU stamps, zero handles.
fn resident_dimensions() {
    if !common::gpu() {
        return;
    }
    let baseline = handles();
    let doc = load("(workspace dimensions\n    (graph drawing :context draw (draw/background \"#102030\"))\n    (graph child :context image (image/render (ref drawing)))\n    (graph parent_drawing :context draw (draw/image (ref child)))\n    (graph parent :context image (image/render (ref parent_drawing) :width 80 :height 40))\n    (graph fixed :context image (image/render (ref drawing) :width 7 :height 5))\n    (graph width_only :context image (image/render (ref drawing) :width 9))\n    (graph height_only :context image (image/render (ref drawing) :height 11)))");
    let owner = editor_d(&doc, 1);
    let plan = editor::private::image_plan(&owner);
    let value = |name: &str| plan.instances.iter().find(|i| i.graph == name && i.default).expect("instance").result.clone();
    let size = |w: i64, h: i64| input::Frame { size: (w, h), ..live(0.) };
    let display = |s: &input::Frame, name: &str| with_images(&owner, None, Some(s), None, |image, _| ok(image(&value(name))));
    let (a, b) = (size(65, 17), size(35, 33));
    let child = display(&a, "child");
    let parent = display(&a, "parent");
    let fixed = display(&a, "fixed");
    let width = display(&a, "width_only");
    let height = display(&a, "height_only");
    let generation = |i: &rays::image::Image| I::generation(&res(i));
    let (parent_before, fixed_before) = (generation(&parent), generation(&fixed));
    assert_eq!(editor::private::image_render_stats(&owner), (5, 0, 0, 0));
    assert!(rays::image::same(&display(&b, "child"), &child) && rays::image::get_size(&child).unwrap() == (35, 33));
    assert!(rays::image::same(&display(&b, "parent"), &parent) && generation(&parent) > parent_before);
    assert!(rays::image::same(&display(&b, "fixed"), &fixed) && generation(&fixed) == fixed_before);
    assert!(rays::image::same(&display(&b, "width_only"), &width) && rays::image::get_size(&width).unwrap() == (9, 33));
    assert!(rays::image::same(&display(&b, "height_only"), &height) && rays::image::get_size(&height).unwrap() == (35, 11));
    assert_eq!(editor::private::image_render_stats(&owner), (8, 3, 0, 0));
    let parent_bytes = pixels(&parent);
    let offset = ((20 * 80) + 50) * 4;
    assert!(parent_bytes[offset..offset + 4] == [0, 0, 0, 0]);
    let (child_generation, fixed_generation) = (generation(&child), generation(&fixed));
    let exact = |s: &input::Frame, name: &str| ok(payload_of(&owner, &value(name), Some(s)));
    let child_a = exact(&a, "child");
    let fixed_a = exact(&a, "fixed");
    assert_eq!((simage::width(&child_a), simage::height(&child_a)), (65, 17));
    let child_b = exact(&b, "child");
    assert_eq!((simage::width(&child_b), simage::height(&child_b)), (35, 33));
    assert!(simage::ptr_eq(&exact(&b, "fixed"), &fixed_a));
    assert!(generation(&child) == child_generation && generation(&fixed) == fixed_generation);
    assert_eq!((simage::width(&child_a), simage::height(&child_a)), (65, 17));
    assert_eq!(editor::private::image_render_stats(&owner), (11, 6, 3, 3));
    editor::close(&owner);
    assert_eq!(editor::private::image_render_stats(&owner), (11, 11, 3, 3));
    assert_eq!(handles(), baseline);
}

/// Resident Canvas failed publication: resized candidates close once, prior allocation retained until owner close.
fn failed_publication() {
    if !common::gpu() {
        return;
    }
    let baseline = handles();
    let doc = |width: i64| load(&format!("(workspace failed_publication\n    (graph drawing :context draw (draw/background \"#102030\"))\n    (graph rendered :context image (image/render (ref drawing) :width {width} :height 8)))"));
    let owner = editor_d(&doc(8), 1);
    let request = |width: i64| {
        let d = doc(width);
        let evaluated = ok(flow_eval::static_eval(&d.checked));
        let v = result_of(&evaluated, "rendered");
        with_images(&owner, None, None, Some(&evaluated.plan), |image, _| image(&v))
    };
    let image = ok(request(8));
    assert_eq!(editor::private::image_render_stats(&owner), (1, 0, 0, 0));
    // Force replacement publication to fail after a resized candidate renders.
    let _ = rays::image::destroy(&image);
    assert!(matches!(request(9), Err(d) if d.code == "E_IMAGE"));
    assert_eq!(editor::private::image_render_stats(&owner), (2, 1, 0, 0));
    assert!(matches!(request(9), Err(d) if d.code == "E_IMAGE"));
    assert_eq!(editor::private::image_render_stats(&owner), (3, 2, 0, 0));
    editor::close(&owner);
    assert_eq!(editor::private::image_render_stats(&owner), (3, 3, 0, 0));
    assert_eq!(handles(), baseline);
}

/// Nested image/render exactness: CPU rounding survives an already-resident GPU child without reading or replacing it.
fn nested_exact() {
    if !common::gpu() {
        return;
    }
    let doc = load("(workspace nested_exact\n    (graph img :context image (image/map (fn [uv] [uv.x uv.y 0.499999999 1]) :width 320 :height 240))\n    (graph drawing :context draw (draw/image (ref img)))\n    (graph rendered :context image (image/render (ref drawing) :width 320 :height 240))\n    (graph picture :context draw (draw/image (ref rendered)))\n    (graph settings :context settings (settings/config :width 320 :height 240)))");
    let display = editor_d(&doc, 1);
    editor::private::gpu_qualification(&display);
    let value = |owner: &Ed, kind: &str| image_value(node_of(&editor::private::image_plan(owner), kind).id);
    let image = with_images(&display, None, None, None, |image, _| ok(image(&value(&display, "image/map"))));
    let resource = res(&image);
    assert_eq!(I::cpu_storage_bytes(&resource), 0);
    let generation = I::generation(&resource);
    let actual = ok(payload_of(&display, &value(&display, "image/render"), None));
    let bytes = rgba(&actual);
    for i in 0..(320 * 240) {
        assert_eq!(bytes[i * 4 + 2], 127);
    }
    for domains in [1, 8] {
        let oracle = editor_d(&doc, domains);
        let expected = ok(payload_of(&oracle, &value(&oracle, "image/render"), None));
        assert!(rgba(&expected) == bytes);
        editor::close(&oracle);
    }
    assert!(I::generation(&resource) == generation && I::readbacks(&resource) == 0 && I::cpu_storage_bytes(&resource) == 0);
    editor::close(&display);
    let dir = temp_dir("export-exact-image");
    if !export_or_skip(&dir, &doc) {
        let _ = std::fs::remove_dir_all(&dir);
        return;
    }
    let files: Vec<_> = std::fs::read_dir(&dir).unwrap().map(|e| e.unwrap().path()).collect();
    assert_eq!(files.len(), 2);
    for f in &files {
        let image = rays::image::load(f.to_str().unwrap()).unwrap();
        // an export renders at the logical size whatever the display's density
        assert_eq!(rays::image::get_size(&image).unwrap(), (320, 240));
        assert!(pixels(&image) == bytes);
        let _ = rays::image::destroy(&image);
    }
    let _ = std::fs::remove_dir_all(&dir);
}

pub fn cases() -> Vec<(&'static str, fn())> {
    vec![
        ("images_capture_context", capture_context),
        ("images_capture_nondefault", capture_nondefault),
        ("images_capture_nested_stateful", capture_nested_stateful),
        ("images_map_drawing", image_map_drawing),
        ("images_map_replan", image_map_replan),
        ("images_noise_and_load", images_noise_and_load),
        ("images_lane_resources_snapshot", lane_resources_snapshot),
        ("images_scene_textures_preserved", scene_textures_preserved),
        ("images_oracle_noise_owner", image_oracle_noise_owner),
        ("images_capacity", capacity),
        ("images_native_canvas_playback", canvas_playback),
        ("images_native_geometry_captures", native_geometry_captures),
        ("images_native_nested_map", native_nested_map),
        ("images_native_rendered_snapshots", native_rendered_snapshots),
        ("images_native_legacy_stamps", legacy_stamps),
        ("images_native_resident_dimensions", resident_dimensions),
        ("images_native_failed_publication", failed_publication),
        ("images_native_nested_exact", nested_exact),
        ("images_native_workspace_gpu_images", native_workspace_gpu_images),
        ("images_native_retained_canvas", native_retained_canvas),
    ]
}

/// Exports run the windowed sketch loop and save its screen, so they need a real window; this suite
/// forces SDL's dummy driver (gpu_editor.rs), which has none: SDL says "not supported", or, creating
/// a Metal window, "... not available in current SDL video driver (dummy) ...".
fn export_or_skip(dir: &std::path::Path, doc: &editor_document::workspace_doc::T) -> bool {
    match workspace::export(Some("picture"), Some(60), dir.to_str().unwrap(), 2, doc) {
        Ok(()) => true,
        Err(d) if d.message.contains("not supported") || d.message.contains("video driver (dummy)") => {
            println!("SKIPPED export half: {} (dummy video driver has no window)", d.message);
            false
        }
        Err(d) => panic!("{}", rays_diagnostic::to_string(&d)),
    }
}


// ---- the two reload-driven resident cases ----

fn plane_scene(source_image: &rays::image::Image, texture: &rays::texture::Texture) -> Vec<rays::scene::Node> {
    use rays::scene3 as s3;
    let mesh = rays::mesh::plane(2. * 65. / 17., 2.).unwrap();
    let camera = rays::camera::orthographic(2., rays_math::vec3::create(0., 0., 2.), rays_math::vec3::zero()).unwrap();
    let scene3 = s3::create(None, Some(rays_math::color::white()), None, None, vec![s3::mesh(Some(rays::material::matte(rays_math::color::white())), Some(s3::textured(Some(rays::texture::Filter::Nearest), texture.clone())), None, Some(s3::Cull::CullNone), None, mesh)]).unwrap();
    vec![rays::scene::clear(rays_math::color::black()), rays::scene::image(source_image, (0, 0), None, None).unwrap(), rays::scene::layer::layer_break(), rays::scene::view3d(Some((33, 0, 32, 17)), &camera, scene3)]
}

fn snapshot(canvas: &rays::canvas::Canvas) -> Vec<u8> {
    let image = rays::canvas::to_image(canvas).unwrap();
    let px = pixels(&image);
    let _ = rays::image::destroy(&image);
    px
}

fn texture_of(bytes: &[u8], width: i64, height: i64) -> rays::texture::Texture {
    rays::texture::init(width, height, &|x, y| {
        let o = ((y * width + x) * 4) as usize;
        rays_math::color::rgba(bytes[o] as i64, bytes[o + 1] as i64, bytes[o + 2] as i64, bytes[o + 3] as i64)
    })
    .unwrap()
}

fn frame_for(time: f64, size: (i64, i64)) -> rays::frame::Frame {
    rays::frame::Frame { input: input::Frame { size, frame: time as i64, tick: time as i64, ..input::at_time(time) }, pixel_scale: (1., 1.), fps: 60., mouse_delta: (0., 0.) }
}

fn is_destroyed(r: Result<Option<(i64, i64, i64, ogpu_core::backend::Texture)>, ogpu_core::error::Error>) -> bool {
    matches!(r, Err(e) if e.kind == ogpu_core::error::ErrorKind::Destroyed)
}

/// Workspace GPU images: exact-first independent CPU snapshots, live display, mesh/image parity, replan/resize,
/// authority transitions and ownership.
fn native_workspace_gpu_images() {
    if !common::gpu() {
        return;
    }
    let before = handles();
    let source = temp_dir("workspace-gpu-image").join("resident.rays");
    let text = |width: i64, height: i64, body: &str| format!("(workspace resident (graph image :context image (let* [bias (* t 0.5)] (image/map (fn [uv] {body}) :width {width} :height {height}))))");
    let initial = text(65, 17, "[uv.x uv.y bias 1]");
    std::fs::write(&source, &initial).unwrap();
    let doc = load(&initial);
    let owner = RefCell::new(editor::create(options_d1(), &doc, Some(rays_editor::source_file::at(source.to_str().unwrap(), &editor_document::lowering::sha256(&initial))), editor::prepare_fn(), editor::draw_fn()).unwrap());
    editor::private::gpu_qualification(&owner.borrow());
    let value = || {
        let plan = editor::private::image_plan(&owner.borrow());
        let n = node_of(&plan, "image/map");
        (n.clone(), image_value(n.id))
    };
    let exact = |time: f64| ok(payload_of(&owner.borrow(), &value().1, Some(&live(time))));
    let resolve = |time: f64| with_images(&owner.borrow(), None, Some(&live(time)), None, |image, texture| (ok(image(&value().1)), ok(texture(&value().1))));
    let saved = exact(0.);
    let saved_bytes = rgba(&saved);
    assert_eq!(editor::private::image_stats(&owner.borrow()), (0, 0));
    let (image, texture) = resolve(0.);
    let identity = rays::texture::private::identity(&texture);
    let resource = res(&image);
    assert!(I::cpu_storage_bytes(&resource) == 0 && I::readbacks(&resource) == 0);
    let generation = I::generation(&resource);
    assert!(simage::ptr_eq(&exact(0.), &saved) && I::generation(&resource) == generation);
    println!("test,width,height,time,max_channel_difference,differing_channels,differing_pixels");
    let canvas = rays::canvas::create(65, 17).unwrap();
    let verify = |time: f64| {
        let (actual_image, actual_texture) = resolve(time);
        assert!(rays::image::same(&actual_image, &image) && rays::texture::private::identity(&actual_texture) == identity);
        assert!(I::cpu_storage_bytes(&resource) == 0 && I::readbacks(&resource) == 0);
        let generation = I::generation(&resource);
        let payload = exact(time);
        assert!(I::generation(&resource) == generation && I::readbacks(&resource) == 0);
        assert!(rgba(&saved) == saved_bytes);
        let (width, height) = rays::image::get_size(&image).unwrap();
        let (node, _) = value();
        let f = match function_of(&node) {
            Value::Fn(f) => f,
            _ => panic!("function"),
        };
        let kernel = ok(flow_sop::image_kernel::prepare(None, None, None, 0, width, height, f, &[], &[]));
        for domains in [1, 8] {
            let context = sop::context::create(sop::context::Options { input: Some(live(time)), time: Some(time), domains: Some(domains), ..Default::default() }).unwrap();
            let mut session = sop::session::create(0, 0).unwrap();
            let cooked = sop::session::cook(&mut session, None, &context, &flow_sop::image_kernel::node(None, &kernel)).unwrap_or_else(|e| panic!("{}", rays_diagnostic::error_to_string(&e)));
            assert!(rgba(&payload::image(&cooked.payload).unwrap()) == rgba(&payload));
            sop::session::close(&mut session);
        }
        let displayed = plane_scene(&actual_image, &actual_texture);
        rays::canvas::render(None, &canvas, &displayed).unwrap();
        rays::canvas::render(None, &canvas, &displayed).unwrap();
        let uploaded = rays::canvas::private::native_stats(&canvas).unwrap().uploaded_bytes;
        rays::canvas::render(None, &canvas, &displayed).unwrap();
        assert_eq!(rays::canvas::private::native_stats(&canvas).unwrap().uploaded_bytes, uploaded);
        let actual = snapshot(&canvas);
        let bytes = rgba(&payload);
        let reference = rays::image::upload_rgba(None, width, height, &bytes).unwrap();
        let t = texture_of(&bytes, width, height);
        rays::canvas::render(None, &canvas, &plane_scene(&reference, &t)).unwrap();
        let expected = snapshot(&canvas);
        let maximum = rgba_diff_max(&actual, &expected);
        let channels = actual.iter().zip(&expected).filter(|(a, b)| a != b).count();
        let px = actual.chunks(4).zip(expected.chunks(4)).filter(|(a, b)| a != b).count();
        println!("workspace_gpu_image,{width},{height},{time:.2},{maximum},{channels},{px}");
        assert!(maximum <= 1);
        let _ = rays::image::destroy(&reference);
        assert_eq!(I::readbacks(&resource), 0);
    };
    verify(0.);
    verify(1.);
    let reload = |time: f64, contents: &str| {
        std::fs::write(&source, contents).unwrap();
        let next = editor::update(owner.borrow().clone(), &frame_for(time, (65, 17)));
        *owner.borrow_mut() = next;
    };
    reload(10., &text(35, 33, "[uv.y uv.x (* t 0.5) 1]"));
    verify(0.5);
    assert_eq!(rays::image::get_size(&image).unwrap(), (35, 33));
    assert_eq!(editor::private::image_stats(&owner.borrow()), (1, 0));
    // Hypot is deliberately CPU-only; fallback must not depend on an arbitrary size threshold in the placement policy.
    reload(20., &text(65, 3, "[(hypot uv.x 0) uv.y 0.5 1]"));
    let (cpu_image, cpu_texture) = resolve(0.);
    assert!(rays::image::same(&cpu_image, &image) && rays::texture::private::image(&cpu_texture).is_none());
    assert!(matches!(I::gpu_snapshot(&resource), Ok(None)));
    reload(30., &text(65, 17, "[uv.x uv.y (* t 0.5) 1]"));
    verify(0.5);
    reload(40., &text(65, 17, "[(if (> uv.x 0) 0 (* (* uv.x 1e20) (* uv.x 1e20))) uv.y 0.5 1]"));
    verify(0.5);
    reload(45., &text(65, 17, "[(if (< uv.x 0) 0 (* (* uv.x 1e20) (* uv.x 1e20))) uv.y 0.5 1]"));
    let v = value().1;
    for _ in 0..2 {
        let failed = with_images(&owner.borrow(), None, Some(&live(0.)), None, |image, _| image(&v));
        assert!(matches!(failed, Err(d) if d.code == "E_KERNEL"));
    }
    assert_eq!(I::readbacks(&resource), 0);
    reload(50., &text(65, 17, "[uv.x uv.y 0.5 1]"));
    verify(0.5);
    assert!(rgba(&saved) == saved_bytes);
    let _ = rays::canvas::destroy(&canvas);
    editor::close(&owner.borrow());
    let _ = std::fs::remove_dir_all(source.parent().unwrap());
    assert_eq!(editor::private::image_stats(&owner.borrow()), (1, 1));
    assert_eq!(handles(), before);
}

fn options_d1() -> editor::Create {
    editor::Create { domains: Some(1), ..options() }
}

/// Resident Canvas live route: mixed mesh/image roundtrip parity, stable identity, isolation, replan/resize,
/// failure/recovery, snapshots and teardown.
fn native_retained_canvas() {
    if !common::gpu() {
        return;
    }
    let baseline = handles();
    let source = temp_dir("retained-canvas").join("retained.rays");
    let text = |width_expression: Option<&str>, width: i64, height: i64, position: &str| {
        let w = width_expression.map(str::to_owned).unwrap_or_else(|| width.to_string());
        format!("(workspace retained\n    (graph drawing :context draw (draw/merge (draw/background \"#102030\")\n      (draw/translate [{position} 0 0] (draw/rect [0 2 0] [16 5 0] :fill \"#a0b0c0\"))))\n    (graph rendered :context image (image/render (ref drawing) :width {w} :height {height})))")
    };
    let initial = text(None, 65, 17, "(* t 16)");
    std::fs::write(&source, &initial).unwrap();
    let owner = RefCell::new(editor::create(options_d1(), &load(&initial), Some(rays_editor::source_file::at(source.to_str().unwrap(), &editor_document::lowering::sha256(&initial))), editor::prepare_fn(), editor::draw_fn()).unwrap());
    let destination = rays::canvas::create(65, 17).unwrap();
    let value = || editor::private::image_plan(&owner.borrow()).instances.iter().find(|i| i.graph == "rendered").expect("instance").result.clone();
    let exact = |time: f64| ok(payload_of(&owner.borrow(), &value(), Some(&live(time))));
    let saved = exact(0.);
    let saved_bytes = rgba(&saved);
    assert_eq!(editor::private::image_stats(&owner.borrow()), (0, 0));
    let resolve = |time: f64| with_images(&owner.borrow(), None, Some(&live(time)), None, |image, texture| (ok(image(&value())), ok(texture(&value()))));
    let (image, texture) = resolve(0.);
    let identity = rays::texture::private::identity(&texture);
    let resource = res(&image);
    let initial_generation = I::generation(&resource);
    assert_eq!(editor::private::image_render_stats(&owner.borrow()), (2, 1, 1, 1));
    for frame in 0..20 {
        let (actual, view) = resolve(frame as f64 / 20.);
        assert!(rays::image::same(&actual, &image) && rays::texture::private::identity(&view) == identity);
    }
    assert_eq!(editor::private::image_render_stats(&owner.borrow()), (2, 1, 1, 1));
    assert!(I::generation(&resource) > initial_generation);
    assert!(I::cpu_storage_bytes(&resource) == 0 && I::readbacks(&resource) == 0);
    let verify = |time: f64| {
        let (actual, view) = resolve(time);
        assert!(rays::image::same(&actual, &image) && rays::texture::private::identity(&view) == identity);
        let generation = I::generation(&resource);
        let payload = exact(time);
        assert!(I::generation(&resource) == generation && I::readbacks(&resource) == 0);
        assert!(rgba(&saved) == saved_bytes);
        let displayed = plane_scene(&actual, &view);
        rays::canvas::render(None, &destination, &displayed).unwrap();
        rays::canvas::render(None, &destination, &displayed).unwrap();
        let uploaded = rays::canvas::private::native_stats(&destination).unwrap().uploaded_bytes;
        rays::canvas::render(None, &destination, &displayed).unwrap();
        assert_eq!(rays::canvas::private::native_stats(&destination).unwrap().uploaded_bytes, uploaded);
        let shown = snapshot(&destination);
        let (width, height) = (simage::width(&payload), simage::height(&payload));
        let bytes = rgba(&payload);
        let oracle = rays::image::upload_rgba(None, width, height, &bytes).unwrap();
        rays::canvas::render(None, &destination, &plane_scene(&oracle, &texture_of(&bytes, width, height))).unwrap();
        assert!(snapshot(&destination) == shown);
        let _ = rays::image::destroy(&oracle);
        println!("workspace_retained_canvas,{width},{height},{time:.2},0,0,0");
    };
    verify(0.);
    verify(0.5);
    let reload = |time: f64, contents: &str| {
        std::fs::write(&source, contents).unwrap();
        let next = editor::update(owner.borrow().clone(), &frame_for(time, (65, 17)));
        *owner.borrow_mut() = next;
    };
    reload(10., &text(None, 35, 33, "(* t 16)"));
    verify(0.5);
    assert_eq!(rays::image::get_size(&image).unwrap(), (35, 33));
    for (stage, contents) in [(20., text(None, 35, 33, "(* t 1e19)")), (30., text(Some("(+ 35 (int (* t 1e19)))"), 35, 33, "(* t 16)"))] {
        load(&contents);
        reload(stage, &contents);
        if stage == 30. {
            let _ = resolve(0.);
        }
        let generation = I::generation(&resource);
        let stats = editor::private::image_render_stats(&owner.borrow());
        for _ in 0..2 {
            let r = with_images(&owner.borrow(), None, Some(&live(2.)), None, |image, _| image(&value()));
            assert!(r.is_err());
        }
        assert!(I::generation(&resource) == generation && editor::private::image_render_stats(&owner.borrow()) == stats);
        assert!(is_destroyed(I::gpu_snapshot(&resource)));
        verify(0.);
    }
    assert!(I::cpu_storage_bytes(&resource) == 0 && I::readbacks(&resource) == 0);
    let (_, _, captures, readbacks) = editor::private::image_render_stats(&owner.borrow());
    editor::close(&owner.borrow());
    let (created, destroyed, captures_after, readbacks_after) = editor::private::image_render_stats(&owner.borrow());
    assert!(created == destroyed && captures == captures_after && readbacks == readbacks_after);
    assert!(rgba(&saved) == saved_bytes);
    let _ = rays::canvas::destroy(&destination);
    let _ = std::fs::remove_dir_all(source.parent().unwrap());
    assert_eq!(handles(), baseline);
}

