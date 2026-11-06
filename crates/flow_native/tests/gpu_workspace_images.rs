//! Tests of the workspace image owner on `sketches/flow_image.rays` (a noise image drawn as a
//! picture): resolving an image node, keeping it while fresh, and drawing it through the
//! `Drawing` image callback. (The OCaml suite reaches the owner only through the editor tests.)
//! Image resources need the process main thread (SDL), so this target runs from its own main.
mod common;
use flow_eval as e;
use flow_native::{drawing, workspace_gpu, workspace_images as wi, workspace_resources};
use flow_sop::lower;
use flow_syntax::value::Value;
use rays::scene;
use std::rc::Rc;
fn ok<A>(r: Result<A, rays_diagnostic::Diagnostic>) -> A {
    r.unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)))
}

fn load() -> lower::T {
    let path = concat!(env!("CARGO_MANIFEST_DIR"), "/../../sketches/flow_image.rays");
    let (_, doc) = web_export::web_export::load(path);
    let doc = doc.unwrap_or_else(|ds| panic!("{}", ds.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("\n")));
    ok(lower::of_checked(None, lower::Options { inputs: Some((*doc.inputs).clone()), ..Default::default() }, &doc.checked))
}

fn result(lowered: &lower::T, name: &str) -> Value {
    lowered.evaluated.results.iter().find(|(n, _)| n == name).map(|(_, v)| v.clone()).unwrap_or_else(|| panic!("{name}"))
}

fn noise_image_is_resolved_and_kept() {
    if !common::gpu() {
        return;
    }
    let lowered = load();
    let ws = workspace_gpu::create(None);
    let resources = workspace_resources::create();
    let images = wi::create(None, None, None, &ws, &resources);
    wi::bind(&images, &lowered);
    let plan = &lowered.evaluated.plan;
    let value = result(&lowered, "image");
    let state = e::create_state();
    let live = input::Frame { size: (320, 240), ..input::at_time(0.) };
    let a = ok(wi::image(None, false, &images, &state, &live, plan, &value));
    assert_eq!(rays::image::dimensions(&a), (256, 256));
    let b = ok(wi::image(None, false, &images, &state, &live, plan, &value));
    assert!(rays::image::same(&a, &b), "a fresh image is not made again");
    let Value::Deferred(_, id) = value else { panic!("deferred") };
    assert!(wi::peek(&images, plan, id).is_some());
    let tex = ok(wi::texture(None, &images, &state, &live, plan, &value));
    assert_eq!(rays::texture::size(&tex), Ok((256, 256)));
    wi::close(&images);
    workspace_resources::close(&resources);
    workspace_gpu::close(&ws);
    assert!(wi::peek(&images, plan, id).is_none());
}


fn drawing_resolves_its_images_through_the_owner() {
    if !common::gpu() {
        return;
    }
    let lowered = load();
    let ws = workspace_gpu::create(None);
    let resources = workspace_resources::create();
    let images = wi::create(None, None, None, &ws, &resources);
    wi::bind(&images, &lowered);
    let plan = &lowered.evaluated.plan;
    let picture = result(&lowered, "picture");
    let prepared = ok(drawing::prepare(&drawing::PrepareOptions::default(), plan, &picture));
    let state = e::create_state();
    let live = input::Frame { size: (320, 240), ..input::at_time(0.) };
    let (images2, state2, live2, plan2) = (images.clone(), state.clone(), live.clone(), Rc::new(plan.clone()));
    let image: drawing::image_resolver = Rc::new(move |value| wi::image(None, false, &images2, &state2, &live2, &plan2, value));
    let s = ok(drawing::render_prepared(&prepared, &live, (320, 240), &drawing::RenderOptions { state: Some(state), image: Some(image), ..Default::default() }));
    assert!(s.iter().any(|n| matches!(n, scene::node::Image(_))), "the image is drawn");
    wi::close(&images);
    workspace_resources::close(&resources);
    workspace_gpu::close(&ws);
}

fn lower_text(text: &str) -> lower::T {
    let forms = ok(flow_syntax::syntax::parse(text));
    ok(lower::workspace(None, Vec::new(), lower::Options::default(), &forms))
}

/// The display path: a pixel map runs on the GPU or the CPU and publishes a resident image; a render
/// draws into a canvas whose frame the image borrows; `exact` freezes either.
fn display_producers_publish_resident_images() {
    if !common::gpu() {
        return;
    }
    let lowered = lower_text(
        "(workspace w (graph pic :context draw (draw/circle [10 10 0] 5)) (graph mapped :context image (image/map (fn [uv] [uv.x uv.y 0.5 1]) :width 16 :height 16)) (graph rendered :context image (image/render (ref pic) :width 16 :height 12)) (graph frozen :context image (exact (image/map (fn [uv] [uv.x uv.y 0.5 1]) :width 8 :height 8))))",
    );
    let ws = workspace_gpu::create(None);
    let resources = workspace_resources::create();
    let images = wi::create(None, None, None, &ws, &resources);
    wi::bind(&images, &lowered);
    let plan = &lowered.evaluated.plan;
    let state = e::create_state();
    let live = input::Frame { size: (320, 240), ..input::at_time(0.) };
    for (name, size) in [("mapped", (16, 16)), ("rendered", (16, 12)), ("frozen", (8, 8))] {
        let value = result(&lowered, name);
        let image = ok(wi::image(None, true, &images, &state, &live, plan, &value));
        assert_eq!(rays::image::dimensions(&image), size, "{name}");
        let again = ok(wi::image(None, true, &images, &state, &live, plan, &value));
        assert!(rays::image::same(&image, &again), "{name} is kept while fresh");
        let cpu = ok(wi::image(None, false, &images, &state, &live, plan, &value));
        assert_eq!(rays::image::dimensions(&cpu), size, "{name} host copy");
    }
    wi::close(&images);
    workspace_resources::close(&resources);
    workspace_gpu::close(&ws);
}

fn main() {
    common::window::run(&[("noise_image_is_resolved_and_kept", noise_image_is_resolved_and_kept), ("drawing_resolves_its_images_through_the_owner", drawing_resolves_its_images_through_the_owner), ("display_producers_publish_resident_images", display_producers_publish_resident_images)]);
}
