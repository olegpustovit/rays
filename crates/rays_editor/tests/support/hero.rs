use std::path::Path;

use editor_document::{document, kind_graph, kinds, lowering, scene_sync, workspace_doc};
use flow_graph::flow_edit::Op;
use rays_editor::workspace;

fn load() -> workspace_doc::T {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let path = root.join("sketches/hero_ui.rays");
    let source = include_str!("../../../../sketches/hero_ui.rays")
        .replace("sketches/hero_ui/", &format!("{}/sketches/hero_ui/", root.to_string_lossy().replace('\\', "/")));
    workspace::load_file(path.to_str().unwrap(), &source).unwrap()
}

fn render(doc: &workspace_doc::T, camera: Option<&str>, key: &str) -> Vec<u8> {
    let path = std::env::temp_dir().join(format!("rays-hero-{key}-{}.png", std::process::id()));
    workspace::render_scene_png(camera, Some((144, 144)), Some(8), 0, path.to_str().unwrap(), doc).unwrap();
    let pixels = read(&path);
    std::fs::remove_file(path).unwrap();
    pixels
}

fn read(path: &Path) -> Vec<u8> {
    let image = rays::image::load(path.to_str().unwrap()).unwrap();
    let pixels = rays::image::pixels(&image).unwrap();
    rays::image::destroy(&image).unwrap();
    pixels
}

pub fn named_camera() {
    assert!(crate::common::gpu());
    let ws = load();
    let browser = lowering::of_workspace(&lowering::Options::default(), &ws).unwrap();
    let scene = &browser.scene;
    let names: Vec<_> = scene.iter().filter(|n| n.kind == "camera").map(|n| n.label.as_str()).collect();
    assert_eq!(names.len(), 14);
    for prefix in ["close", "tele"] {
        for name in ["usage", "usage-square", "permission", "permission-square", "diff", "topology", "sessions"] {
            assert!(names.contains(&format!("{prefix}/{name}").as_str()), "{prefix}/{name}");
        }
    }
    let id = scene.iter().find(|n| n.label == "tele/usage-square").unwrap().id;
    let (doc, _) = scene_sync::write(None, &browser, document::Level::Scene, &scene_sync::Edit::Camera(Some(id)))
        .unwrap().unwrap();
    assert_eq!(kind_graph::find(&doc.scene, doc.active_camera.unwrap()).unwrap().label, "tele/usage-square");
    assert_eq!((doc.root.width, doc.root.height, doc.root.max_spp), (2400, 2400, 16));
    let restored = lowering::restore(None, &doc, &document::snapshot(&browser)).unwrap();
    assert_eq!(kind_graph::find(&restored.scene, restored.active_camera.unwrap()).unwrap().label, "tele/permission");

    let actual = render(&document::text(&doc).text_ws, None, "named");
    let proof = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../fixtures/hero_ui/tele-usage-square.png");
    let expected = read(&proof);
    assert_eq!(actual.len(), expected.len());
    let error = actual.iter().zip(expected).map(|(&a, b)| f64::from(a.abs_diff(b))).sum::<f64>() / actual.len() as f64;
    assert!(error <= 0.65, "render differs from proof: mean channel error {error}");
}

pub fn material_edit() {
    assert!(crate::common::gpu());
    let ws = load();
    let value = flow_syntax::arena_parse::parse("0.0").unwrap().remove(0);
    let edited = workspace_doc::edit(&kinds::shared(None).unwrap(), &ws, &Op::SetInputDefault {
        form: "screen".into(),
        input: "strength".into(),
        value,
    }).unwrap();
    let actual = render(&edited, Some("tele/usage-square"), "material");
    let proof = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../fixtures/hero_ui/tele-usage-square.png");
    let expected = read(&proof);
    let bright = expected.iter().enumerate().filter(|&(i, &v)| i % 4 != 3 && v > 80).count();
    let darkened = expected.iter().zip(&actual).enumerate()
        .filter(|&(i, (&before, &after))| i % 4 != 3 && before > 80 && after < 32).count();
    assert!(bright > 64 && darkened * 10 >= bright * 9, "{darkened}/{bright} bright channels darkened");
}

pub fn drawable_preview() {
    use rays_editor::{editor_renderer, viewport};
    use rays_editor::rays_editor::editor;
    use rays_math::vec3;
    use rays_pathtracer::rays_pathtracer as trace;

    assert!(crate::common::gpu());
    let mut at = rays::frame::Frame {
        input: input::Frame { size: (960, 640), ..input::at_time(0.) },
        pixel_scale: (2., 2.),
        fps: 60.,
        mouse_delta: (0., 0.),
    };
    let mut e = editor::create(editor::Create { await_: Some(true), ..Default::default() }, &load(),
        None, editor::prepare_fn(), editor::draw_fn()).unwrap();
    for tick in 1..=4 {
        at.input.frame = tick;
        at.input.tick = tick;
        at.input.t = tick as f64 / 60.;
        e = editor::update(e, &at);
        if e.extra.renderer.slots.iter().any(|slot| slot.tracer.is_some()) {
            break;
        }
    }
    assert!(e.extra.renderer.slots.iter().any(|slot| slot.tracer.is_some()),
        "no traced viewport: cook={:?}, scene={:?}, renderer={:?}", e.core.cook.error, e.context_error, e.extra.renderer.error);
    let key = e.extra.renderer.slots[0].keys[0].clone();
    let world = e.views.iter().find(|(k, _)| k == &key).map(|(_, scene)| scene.clone())
        .or_else(|| e.rendered.clone()).unwrap();
    let gate = editor::film(&e, &at);
    let pixels = (2 * gate.2, 2 * gate.3);
    assert_eq!(editor::slot_of(&e, &key).unwrap().film, pixels);

    let screen = e.drawn.values().flat_map(|drawn| rays::scene3::walk::drawings(&drawn.rendered))
        .find(|drawing| drawing.texture.is_some()).unwrap();
    assert_eq!(rays::mesh::vertex_count(&screen.mesh), 4);

    let flush = |e: &editor::Editor| {
        let slot = editor_renderer::slot_of(&e.extra.renderer, &key).unwrap();
        trace::flush(&mut slot.tracer.as_ref().unwrap().borrow_mut()).unwrap();
    };
    flush(&e);
    let moved = rays::camera::perspective(None, None, None, vec3::create(0., 10., 5.), vec3::zero()).unwrap();
    let view = (key.clone(), gate, moved, world);
    e.extra = viewport::render(e.extra.clone(), at.pixel_scale, 1.0, Some(&key), vec![view.clone()]);
    assert_eq!(editor::slot_of(&e, &key).unwrap().film, (gate.2, gate.3));
    flush(&e);
    e.extra = viewport::render(e.extra.clone(), at.pixel_scale, 1.2, Some(&key), vec![view]);
    assert_eq!(editor::slot_of(&e, &key).unwrap().film, pixels);
    editor::close(&e);
}
