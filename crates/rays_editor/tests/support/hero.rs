use std::path::Path;

use editor_document::{document, kind_graph, kinds, lowering, scene_sync, workspace_doc};
use flow_graph::flow_edit::Op;
use rays_editor::workspace;

fn load() -> workspace_doc::T {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let path = root.join("sketches/hero_ui.rays");
    let source = include_str!("../../../../sketches/hero_ui.rays")
        // The small image mesh isolates the camera and material gestures from export build time.
        .replace("(preview : bool true) ", "")
        .replace("(let* [\n           tele_usage", "(let* [preview true\n           tele_usage")
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
    assert_eq!((doc.root.width, doc.root.height, doc.root.max_spp), (480, 480, 16));
    let restored = lowering::restore(None, &doc, &document::snapshot(&browser)).unwrap();
    assert_eq!(kind_graph::find(&restored.scene, restored.active_camera.unwrap()).unwrap().label, "tele/permission");

    let actual = render(&document::text(&doc).text_ws, None, "named");
    let proof = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../sketches/hero_ui/tele-usage-proof.png");
    let expected = read(&proof);
    assert_eq!(actual.len(), expected.len());
    let error = actual.iter().zip(expected).map(|(&a, b)| f64::from(a.abs_diff(b))).sum::<f64>() / actual.len() as f64;
    assert!(error <= 0.65, "render differs from proof: mean channel error {error}");
}

pub fn material_edit() {
    assert!(crate::common::gpu());
    let ws = load();
    let value = flow_syntax::arena_parse::parse("false").unwrap().remove(0);
    let edited = workspace_doc::edit(&kinds::shared(None).unwrap(), &ws, &Op::SetInputDefault {
        form: "screen".into(),
        input: "vertex_emission".into(),
        value,
    }).unwrap();
    let actual = render(&edited, Some("tele/usage-square"), "material");
    let proof = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../sketches/hero_ui/tele-usage-proof.png");
    let expected = read(&proof);
    let changed = actual.iter().zip(expected).filter(|(a, b)| a.abs_diff(*b) > 32).count();
    assert!(changed > actual.len() / 8, "only {changed} channels changed after disabling vertex tint");
}
