//! Tests for lowering_a / shell_eval_a: the lowering parts of crates/rays_editor/test/test_editor_document.ml
//! and test_fast_path.ml, plus the module's own checks. The parts of those files that drive the
//! editor UI (delete-all through real events) or `Scene_sync` (camera write-back) are not here.
use crate::document::{self, Home};
use crate::document_edit;
use crate::kind_graph as kg;
use crate::kinds;
use crate::lowering::{self, Options};
use crate::preset;
use crate::workspace_doc::{self, TextOptions};
use flow_check::catalog::Catalog;
use flow_graph::flow_edit::{self as fe, ArgKey, Op};
use flow_ir::flow_ir::Gpu;
use flow_syntax::syntax::{self as s, Node};
use std::sync::Arc;

fn factories() -> Vec<sop::edit_graph::Factory> {
    sop::nodes::factories()
}

fn catalog() -> Catalog {
    kinds::catalog(flow_sop_catalog::manifest::VERSION, &factories()).expect("catalog")
}

fn ws_of_text(text: &str) -> workspace_doc::T {
    match workspace_doc::of_text(&TextOptions::default(), &catalog(), text) {
        Ok(w) => w,
        Err(ds) => panic!("{}", ds.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("; ")),
    }
}

fn lower(previous: Option<&document::T>, ws: &workspace_doc::T) -> document::T {
    let options = Options { factories: Some(factories()), previous: previous.cloned(), ..Default::default() };
    match lowering::of_workspace(&options, ws) {
        Ok(d) => d,
        Err(d) => panic!("{}", rays_diagnostic::to_string(&d)),
    }
}

fn lower_err(text: &str) -> rays_diagnostic::Diagnostic {
    match lowering::of_workspace(&Options { factories: Some(factories()), ..Default::default() }, &ws_of_text(text)) {
        Ok(_) => panic!("expected a lowering error"),
        Err(d) => d,
    }
}

// ---- sha256 ----

#[test]
fn sha256_known_vectors() {
    assert_eq!(lowering::sha256(""), "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855");
    assert_eq!(lowering::sha256("abc"), "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad");
    assert_eq!(lowering::sha256("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"), "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1");
}

// ---- test_editor_document.ml: the document boundary ----

const DOCUMENT_TEXT: &str = r#"(workspace document
  (graph geo1 :context sop (sop/box :size [1 1 1]))
  (graph scene :context scene
    (scene/merge (scene/geometry (ref geo1) :name "geo1") (scene/camera :name "camera1"))))"#;

struct UnusedBackend;
impl Gpu::Backend for UnusedBackend {
    fn prepare(&self, _: bool, _: &flow_ir::packed::T) -> Result<Arc<dyn Gpu::Kernel>, rays_diagnostic::Diagnostic> {
        Err(rays_diagnostic::error("E_TEST_GPU".to_owned(), "unused test backend".to_owned(), None, None, None, Vec::new(), Vec::new()))
    }
}

fn test_backend() -> Arc<dyn Gpu::Backend> { Arc::new(UnusedBackend) }

#[test]
fn document_boundary_validation_dump_preset_gpu() {
    let workspace = ws_of_text(DOCUMENT_TEXT);
    let doc = lower(None, &workspace);
    let backend = test_backend();
    let gpu = Gpu::Config { backend: Some(backend.clone()), fast: true, policy: Gpu::Policy::Qualification, readback_per_float: None };
    let carries_gpu = |doc: &document::T| {
        let l = &doc.workspace.lowered.gpu;
        l.fast && l.policy == Gpu::Policy::Qualification && l.backend.as_ref().map_or(false, |b| Arc::ptr_eq(b, &backend))
    };
    let gpu_doc = lowering::of_workspace(&Options { factories: Some(factories()), gpu: Some(gpu.clone()), ..Default::default() }, &workspace).expect("gpu lowering");
    assert!(carries_gpu(&gpu_doc), "workspace lowering discarded its explicit GPU config");
    let reloaded = document_edit::text_edit(None, Some(&factories()), &gpu_doc, &workspace_doc::to_text(&workspace).unwrap()).expect("reload");
    assert!(carries_gpu(&reloaded), "a real workspace text reload discarded its GPU config");
    assert!(document::validate(&doc).is_ok(), "a lowered workspace is a valid document");
    let scene = document::scene_graph(&doc);
    let geometry = crate::objects::ids("geometry", &scene);
    assert!(!scene.is_empty() && document::object_network(&doc, geometry[0]).is_some(), "the lowering has no geometry object with a network");
    // an object must own its network, and a network its object
    assert!(document::validate(&document::T { networks: Default::default(), ..doc.clone() }).is_err(), "an object without its network was accepted");
    let network = doc.networks.values().next().expect("a network").clone();
    let mut extra = (*doc.networks).clone();
    extra.insert(9_999, network);
    assert!(document::validate(&document::T { networks: std::sync::Arc::new(extra), ..doc.clone() }).is_err(), "a network without its object was accepted");
    assert!(document::validate(&document::T { active_camera: Some(9_999), ..doc.clone() }).is_err(), "an active camera that is not a camera object was accepted");
    assert!(document::dump(&doc) == document::dump(&doc) && document::dump(&doc).contains("object "), "the document dump is not deterministic or lost an object");
    // the preset text is the workspace text: it loads back to the same document
    let directory = std::env::temp_dir().join(format!("rays-document-contract-{}", std::process::id()));
    std::fs::create_dir_all(&directory).expect("temp dir");
    let dir = directory.to_string_lossy().into_owned();
    let saved = preset::save(&dir, "round", &doc, &s::make(s::Draft::Map(Vec::new().into()))).expect("save");
    let loaded = preset::load(Some(gpu), &saved, Some(&factories()), &doc.settings).expect("load");
    assert!(
        workspace_doc::to_text(&loaded.doc.workspace.ws).unwrap() == workspace_doc::to_text(&workspace).unwrap() && loaded.doc.networks.len() == doc.networks.len(),
        "a saved preset did not load back"
    );
    assert!(carries_gpu(&loaded.doc), "preset loading discarded its explicit GPU config");
    std::fs::remove_dir_all(&directory).ok();
}

#[test]
fn lowering_reads_objects_root_camera_and_homes() {
    let doc = lower(None, &ws_of_text(DOCUMENT_TEXT));
    let labels: Vec<&str> = doc.scene.iter().map(|n| n.label.as_str()).collect();
    assert_eq!(labels, vec!["geo1", "camera1"]);
    assert_eq!(doc.networks.len(), 1);
    let cam = doc.scene.iter().find(|n| n.kind == "camera").unwrap().id;
    assert_eq!(doc.active_camera, Some(cam), "a scene graph's first camera is the render camera");
    // each object is written at a path of the scene graph (an argument of the merge)
    assert_eq!(doc.homes.objects.len(), 2);
    assert!(doc.homes.objects.iter().all(|(_, h)| matches!(h, Home::InlineIn(..))), "{:?}", doc.homes.objects);
    assert_eq!(lowering::graph_of(&doc.workspace.ws, flow_syntax::context::scene()).map(|g| g.name), Some("scene".to_owned()));
}

#[test]
fn unchanged_scene_and_network_are_kept() {
    // AGENTS.md "Physical identity": an edit elsewhere recomposes nothing
    let ws = ws_of_text(DOCUMENT_TEXT);
    let doc = lower(None, &ws);
    let again = lower(Some(&doc), &ws);
    assert!(kg::same(&doc.scene, &again.scene));
    let (a, b) = (doc.networks.values().next().unwrap(), again.networks.values().next().unwrap());
    assert!(flow_sop_catalog::network::ptr_eq(&a.graph, &b.graph), "the unchanged network was replaced");
    assert!(crate::settings::same_values(&doc.settings, &again.settings));
    assert_eq!(doc.scene.iter().map(|n| n.id).collect::<Vec<_>>(), again.scene.iter().map(|n| n.id).collect::<Vec<_>>(), "ids stay");
}

#[test]
fn settings_graph_gives_the_window() {
    let plain = ws_of_text(DOCUMENT_TEXT);
    assert!(!lowering::has_settings(&plain));
    assert_eq!(lowering::window(&plain).unwrap(), kinds::window_default());
    let text = r#"(workspace w
  (graph geo :context sop (sop/box))
  (graph settings :context settings (settings/config :title "Hello" :width 640 :height 480 :fps 30 :precision "exact")))"#;
    let ws = ws_of_text(text);
    assert!(lowering::has_settings(&ws));
    let window = lowering::window(&ws).unwrap();
    assert_eq!((window.title.as_str(), window.width, window.height, window.fps, window.fast), ("Hello", 640, 480, 30, false));
    let doc = lower(None, &ws);
    assert_eq!(lowering::document_window(&doc).unwrap(), window);
    assert!(!lowering::fast(&doc));
    assert!(doc.homes.settings.is_some());
}

#[test]
fn world_layers_and_loop_copies() {
    let text = r#"(workspace w
  (graph sky :context world (world/gradient :name "top"))
  (graph scene :context scene
    (let* [g (scene/camera :name "c")]
      (scene/merge g (scene/world (ref sky)) (for [i (range 2)] (scene/light :name "lamp")))))) "#;
    let doc = lower(None, &ws_of_text(text));
    assert_eq!(doc.scene.iter().filter(|n| n.kind == "light").count(), 2);
    let copies = doc.homes.objects.iter().filter(|(_, h)| matches!(h, Home::Copy { .. })).count();
    assert_eq!(copies, 2, "{:?}", doc.homes.objects);
    assert_eq!(doc.homes.world_graph.as_deref(), Some("sky"));
    let world = doc.scene.iter().find(|n| n.kind == "world").expect("world object");
    assert_eq!(doc.layers.get(&world.id).map(|l| l.len()), Some(1));
    assert_eq!(doc.homes.layers.len(), 1);
}

#[test]
fn scene_refusals() {
    let two = r#"(workspace w
  (graph sky :context world (world/gradient))
  (graph scene :context scene (scene/root (scene/merge (scene/world (ref sky)) (scene/world (ref sky))))))"#;
    assert_eq!(lower_err(two).code, "E_SCENE_WORLD");
    let nested = r#"(workspace w
  (graph scene :context scene (scene/merge (scene/root (scene/camera)) (scene/camera))))"#;
    assert_eq!(lower_err(nested).code, "E_SCENE_ROOT");
    let parent = r#"(workspace w
  (graph scene :context scene (scene/merge (scene/camera :name "a") (scene/light :name "b" :parent "missing"))))"#;
    assert_eq!(lower_err(parent).code, "E_LOWER");
}

#[test]
fn scene_reads_is_static_without_drives() {
    let doc = lower(None, &ws_of_text(DOCUMENT_TEXT));
    assert_eq!(lowering::scene_reads(&doc), sop::context::dependencies::STATIC);
    let (graph, errors) = lowering::resolve_scene(None, None, None, &doc, 0.5);
    assert!(errors.is_empty() && kg::same(&graph, &doc.scene));
}

#[test]
fn restore_gives_the_snapshot_back() {
    let ws = ws_of_text(DOCUMENT_TEXT);
    let doc = lower(None, &ws);
    let snap = document::snapshot(&doc);
    let again = lowering::restore(Some(&factories()), &doc, &snap).expect("restore");
    assert!(kg::same(&again.scene, &doc.scene));
    assert_eq!(again.homes, doc.homes);
    assert_eq!(again.active_camera, doc.active_camera);
}

// ---- test_fast_path.ml ----

const SEA: &str = r#"(workspace sea
  (graph g :context sop [(height : float 0.5)]
    (let* [a (sop/uv_sphere :radius 0.5)
           l (sop/with_attr a :P (exact (map (fn [(p : vec3)] (+ p [0.0 height 0.0])) (sop/attr a :P))))
           b (sop/transform l :translate [1 0 0])]
      b))
  (graph scene :context scene
    (let* [body (scene/geometry (ref g) :name "body")
           cam (scene/camera :name "cam" :eye [7.0 4.0 7.0] :target [0 0 0] :follow_viewport true)]
      (scene/root (scene/merge body cam) :width 960 :height 640))))"#;

type View = Vec<(String, String, Vec<(String, param::Value)>)>;

fn view(d: &document::T) -> View {
    d.scene.iter().map(|n| (n.label.clone(), n.kind.clone(), kg::fields(n).into_iter().map(|f| (f.name, f.current)).collect())).collect()
}

fn source(doc: &document::T) -> String {
    workspace_doc::to_text(&doc.workspace.ws).unwrap()
}

fn same_as_full(what: &str, doc: &document::T) {
    let full = lower(None, &ws_of_text(&source(doc)));
    assert!(view(doc) == view(&full) && doc.root == full.root, "{what}: differs from a full lowering");
}

fn parse1(text: &str) -> s::Syntax {
    s::parse(text).expect("parse").remove(0)
}

fn plan_key(doc: &document::T) -> Vec<(String, Vec<String>, Vec<i64>, String)> {
    doc.workspace.lowered.plan.nodes.iter().map(|n| (n.kind.clone(), n.site.clone(), n.iter.clone(), flow_eval::structure(&flow_syntax::value::Value::Record(n.args.clone())))).collect()
}

fn fast_catalog() -> Catalog {
    kinds::catalog(1, &factories()).expect("catalog")
}

#[test]
fn fast_path_literal_scrub_equals_full_lowering() {
    // SEA reduced to a box (any chain through sop/transform or with_attr hangs flow_sop::lower::of_checked;
    // the faithful text is `fast_path_literal_scrub_with_closure` below)
    let text = r#"(workspace sea
  (graph g :context sop (let* [b (sop/box :size [1 1 1])] b))
  (graph scene :context scene
    (let* [body (scene/geometry (ref g) :name "body")
           cam (scene/camera :name "cam" :eye [7.0 4.0 7.0] :target [0 0 0] :follow_viewport true)]
      (scene/root (scene/merge body cam) :width 960 :height 640))))"#;
    let doc = lower(None, &ws_of_text(text));
    same_as_full("initial", &doc);
    // a literal scrub of a card
    let op = Op::SetArg { node: vec!["g".into(), "b".into()], key: ArgKey::Kw("size".into()), sub: vec![0], value: parse1("3") };
    let ws = workspace_doc::edit(&fast_catalog(), &doc.workspace.ws, &op).expect("edit");
    let scrubbed = lower(Some(&doc), &ws);
    same_as_full("literal scrub", &scrubbed);
    assert!(source(&scrubbed).contains("[3 1 1]"), "{}", source(&scrubbed));
}

fn sketch(file: &str) -> String {
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../sketches").join(file);
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{}: {e}", path.display()))
}

fn scrubs(file: &str, steps: &[(Vec<String>, &str, Vec<i64>, &str)]) {
    let mut doc = lower(None, &ws_of_text(&sketch(file)));
    for (node, key, sub, value) in steps {
        let op = Op::SetArg { node: node.clone(), key: ArgKey::Kw((*key).to_owned()), sub: sub.clone(), value: parse1(value) };
        let ws = workspace_doc::edit(&fast_catalog(), &doc.workspace.ws, &op).expect("edit");
        let next = lower(Some(&doc), &ws);
        let full = lower(None, &ws_of_text(&source(&next)));
        let what = format!("{file} {key} {value}");
        assert!(plan_key(&next) == plan_key(&full), "{what}: plan differs from a full lowering");
        same_as_full(&what, &next);
        doc = next;
    }
}

fn names(v: &[&str]) -> Vec<String> {
    v.iter().map(|x| (*x).to_owned()).collect()
}

#[test]
fn fast_path_literal_scrub_with_closure() {
    let doc = lower(None, &ws_of_text(SEA));
    same_as_full("initial", &doc);
    let op = Op::SetArg { node: vec!["g".into(), "b".into()], key: ArgKey::Kw("translate".into()), sub: vec![0], value: parse1("3") };
    let ws = workspace_doc::edit(&fast_catalog(), &doc.workspace.ws, &op).expect("edit");
    same_as_full("literal scrub", &lower(Some(&doc), &ws));
}

#[test]
#[ignore = "upstream: flow_eval static evaluation of sop/uv_sphere (gpu_sea, orrery) takes 20-40 s per lowering in a debug build, not a deadlock (parity_backlog: flow_eval static_with cost)"]
fn fast_path_inline_scrubs_gpu_sea() {
    let waves = names(&["sea", "waves"]);
    scrubs(
        "gpu_sea.rays",
        &[
            (waves.clone(), "seed", vec![], "9"),
            (waves.clone(), "octaves", vec![], "5"),
            (waves.clone(), "seed", vec![], "11"),
            (waves.clone(), "seed", vec![], "9"),
            // a literal inside a live argument
            (waves.clone(), "lacunarity", vec![1], "2.25"),
            (vec!["sea".to_owned(), fe::nested_leaf("waves", &ArgKey::Pos(0)).expect("nested leaf")], "width", vec![], "7.5"),
        ],
    );
}

#[test]
#[ignore = "upstream: flow_eval static evaluation of sop/uv_sphere (gpu_sea, orrery) takes 20-40 s per lowering in a debug build, not a deadlock (parity_backlog: flow_eval static_with cost)"]
fn fast_path_inline_scrubs_orrery() {
    let base = names(&["orrery", "base"]);
    let sun = names(&["orrery", "sun"]);
    scrubs(
        "ws_orrery.rays",
        &[
            (base.clone(), "amplitude", vec![], "0.07"),
            (sun.clone(), "segments", vec![], "18"),
            (base.clone(), "amplitude", vec![], "0.05"),
            (sun.clone(), "segments", vec![], "16"),
            (names(&["orrery", "glow"]), "color", vec![1, 1], "0.1"),
        ],
    );
}

#[test]
fn live_sop_argument_lowers() {
    let ws = ws_of_text("(workspace driven\n    (graph g :context sop (sop/box :size [(+ 1 t) 1 1]))\n    (graph scene :context scene (scene/geometry (ref g))))");
    lower(None, &ws);
}

#[test]
fn live_sop_arguments_lower_for_transform_grid_sin() {
    for body in [
        "(let* [b (sop/box)] (sop/transform b :shear_xy (sin t)))",
        "(sop/grid :size (+ 1 t))",
    ] {
        let ws = ws_of_text(&format!("(workspace live\n    (graph g :context sop {body})\n    (graph scene :context scene (scene/geometry (ref g))))"));
        if let Err(d) = lowering::of_workspace(&Options { factories: Some(factories()), ..Default::default() }, &ws) { panic!("{body}: {}", rays_diagnostic::to_string(&d)); }
    }
}
