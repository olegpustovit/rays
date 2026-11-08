//! test_editor_document.ml: validation, dump, preset round trip, delete-all through real UI events.
use super::t4::*;
use editor_document::{document, document_edit, kind_graph, lowering, objects, preset, workspace_doc};
use flow_ir::flow_ir::Gpu;
use input::Key;
use rays_editor::Editor;
use std::sync::Arc;

const TEXT: &str = r#"(workspace document
  (graph geo1 :context sop (sop/box :size [1 1 1]))
  (graph scene :context scene
    (scene/merge (scene/geometry (ref geo1) :name "geo1") (scene/camera :name "camera1"))))"#;

pub fn run() {
    let t0 = std::time::Instant::now();
    let lap = |s: &str| if std::env::var("T4_TRACE").is_ok() { eprintln!("[t4 doc] {s} {:.2}", t0.elapsed().as_secs_f64()) };
    if !crate::common::gpu() {
        return;
    }
    let directory = temp_dir("document");
    lap("start");
    let workspace = of_text(None, TEXT);
    let doc = lowering::of_workspace(&Default::default(), &workspace).unwrap_or_else(|d| panic!("{d:?}"));
    let backend = Arc::new(Gpu::Backend {
        residency: Box::new(|_| None),
        cost: Box::new(|_, _, _, _| None),
        prepare: Box::new(|_, _| Err(rays_diagnostic::error("E_TEST_GPU".into(), "unused test backend".into(), None, None, None, vec![], vec![]))),
    });
    let gpu = Gpu::Config { backend: Some(backend.clone()), fast: true, policy: Gpu::Policy::Qualification, readback_per_float: None };
    let carries_gpu = |d: &document::T| {
        let g = &d.workspace.lowered.gpu;
        g.fast && g.policy == Gpu::Policy::Qualification && g.backend.as_ref().is_some_and(|b| Arc::ptr_eq(b, &backend))
    };
    lap("lowered");
    let gpu_doc = lowering::of_workspace(&lowering::Options { gpu: Some(gpu.clone()), ..Default::default() }, &workspace).unwrap_or_else(|d| panic!("{d:?}"));
    assert!(carries_gpu(&gpu_doc), "workspace lowering discarded its explicit GPU config");
    let reloaded = document_edit::text_edit(None, None, &gpu_doc, &workspace_doc::to_text(&workspace).unwrap()).unwrap();
    assert!(carries_gpu(&reloaded), "a real workspace text reload discarded its GPU config");
    assert!(document::validate(&doc).is_ok(), "a lowered workspace is a valid document");
    let scene = document::scene_graph(&doc);
    assert!(!scene.is_empty() && document::object_network(&doc, objects::ids("geometry", &scene)[0]).is_some(), "the lowering has no geometry object with a network");
    // an object must own its network, and a network its object
    let mut no_networks = doc.clone();
    std::sync::Arc::make_mut(&mut no_networks.networks).clear();
    assert!(document::validate(&no_networks).is_err(), "an object without its network was accepted");
    let network = doc.networks.values().next().unwrap().clone();
    let mut orphan = doc.clone();
    std::sync::Arc::make_mut(&mut orphan.networks).insert(9_999, network);
    assert!(document::validate(&orphan).is_err(), "a network without its object was accepted");
    let mut bad_camera = doc.clone();
    bad_camera.active_camera = Some(9_999);
    assert!(document::validate(&bad_camera).is_err(), "an active camera that is not a camera object was accepted");
    assert!(document::dump(&doc) == document::dump(&doc) && document::dump(&doc).contains("object "), "the document dump is not deterministic or lost an object");
    // the preset text is the workspace text: it loads back to the same document
    let view = flow_syntax::syntax::make(flow_syntax::syntax::Draft::Map(vec![]));
    let saved = preset::save(&directory, "round", &doc, &view).unwrap();
    let loaded = preset::load(Some(gpu.clone()), &saved, None, &doc.settings).unwrap();
    assert!(workspace_doc::to_text(&loaded.doc.workspace.ws).unwrap() == workspace_doc::to_text(&workspace).unwrap() && loaded.doc.networks.len() == doc.networks.len(), "a saved preset did not load back");
    assert!(carries_gpu(&loaded.doc), "preset loading discarded its explicit GPU config");
    std::fs::remove_file(&saved).unwrap();

    // Deleting every scene object through real UI events empties the scene and the preview, and
    // the document still saves.
    let options = Editor::Create { await_: Some(true), ui_text_size: Some(11), domains: Some(1), presets: Some(directory.clone()), platform: Some(input::Platform::Apple), ..Default::default() };
    lap("preset");
    let ed = Editor::create(options, &workspace, None, Editor::prepare_fn(), Editor::draw_fn()).expect("the editor opens");
    lap("created");
    let mut d = Driver::new(ed);
    let mut frames = 0;
    while Editor::prepared(d.e()).is_none() && frames < 200 {
        d.step_at((450., 320.), vec![]);
        frames += 1;
    }
    assert!(Editor::prepared(d.e()).is_some(), "cook did not settle");
    d.step_at((450., 320.), vec![]);
    // the scene opens as a list: delete its first row, twice (geo1, then the camera)
    lap("settled");
    let gframe = frame(&Opts::default(), (450., 320.), vec![], 0);
    let (gx, gy, _, _) = Editor::panes(d.e(), &gframe).graph;
    let mouse = ((gx + 60) as f64, (gy + 24 + 12) as f64);
    for _ in 0..2 {
        d.count += 30; // past the double-click interval
        d.step_at(mouse, vec![]);
        d.step_at(mouse, click(mouse));
        d.step_at(mouse, vec![key(Key::Delete)]);
        d.step_at(mouse, vec![]);
    }
    lap("deleted");
    let labels: Vec<String> = Editor::document(d.e()).iter().map(|n: &kind_graph::Node| n.label.clone()).collect();
    assert!(labels.is_empty() && Editor::prepared(d.e()).is_none(), "delete-all did not clear the scene and preview: {labels:?}, prepared {}", Editor::prepared(d.e()).is_some());
    d.step_at((450., 320.), vec![key(Key::KeyChar('/')), chr('s')]);
    d.step_at((450., 320.), vec![key(Key::Enter)]);
    d.step_at((450., 320.), vec![]);
    assert_eq!(preset::list(&directory).len(), 1, "the document did not save a preset");
    d.close();
    let _ = std::fs::remove_dir_all(&directory);
}
