//! test_fast_path.ml: a camera write in a workspace holding a [fn] closure takes the literal fast
//! path (no lowering, no evaluation) and the document is the one a full lowering gives. The
//! gpu_sea / ws_orrery scrub sweeps and the Gc.live_words bound are SKIPPED (see gpu_suite_t4).
use super::t4::*;
use editor_document::{document, kind_graph as kg, lowering, scene_sync as sync, workspace_doc};
use flow_graph::flow_edit as E;
use flow_syntax::phase_timer::SampleGuard;
use param::Value;

const TEXT: &str = r#"(workspace sea
  (graph g :context sop [(height : float 0.5)]
    (let* [a (sop/uv_sphere :radius 0.5)
           l (sop/with_attr a :P (exact (map (fn [(p : vec3)] (+ p [0.0 height 0.0])) (sop/attr a :P))))
           b (sop/transform l :translate [1 0 0])]
      b))
  (graph scene :context scene
    (let* [body (scene/geometry (ref g) :name "body")
           cam (scene/camera :name "cam" :eye [7.0 4.0 7.0] :target [0 0 0] :follow_viewport true)]
      (scene/root (scene/merge body cam) :width 960 :height 640))))"#;

fn lower(previous: Option<document::T>, ws: &workspace_doc::T) -> document::T {
    lowering::of_workspace(&lowering::Options { previous, ..Default::default() }, ws).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)))
}
fn source(d: &document::T) -> String {
    workspace_doc::to_text(&d.workspace.ws).unwrap()
}
/// lowering and evaluation calls made by [f]
fn work<R>(f: impl FnOnce() -> R) -> (R, i64) {
    let t0 = std::time::Instant::now();
    let guard = SampleGuard::start_with_clock(Box::new(move || t0.elapsed().as_secs_f64()));
    let r = f();
    let p = guard.finish();
    (r, flow_syntax::phase_timer::calls(&p, "lower") + flow_syntax::phase_timer::calls(&p, "evaluate"))
}
fn same_as_full(what: &str, doc: &document::T) {
    let full = lower(None, &of_text(None, &source(doc)));
    let view = |d: &document::T| d.scene.iter().map(|n| (n.label.clone(), n.kind.clone(), kg::fields(n).into_iter().map(|f| (f.name, f.current)).collect::<Vec<_>>())).collect::<Vec<_>>();
    assert!(view(doc) == view(&full) && doc.root == full.root, "{what}: differs from a full lowering");
}
fn set_eye(doc: &document::T, cam: i64, changes: &[(&str, f64)]) -> document::T {
    let changes: Vec<(String, Value)> = changes.iter().map(|(k, v)| (k.to_string(), Value::Float(*v))).collect();
    document::T { scene: kg::set_fields(&doc.scene, cam, &changes).unwrap().0, ..doc.clone() }
}

pub fn cameras() {
    let doc = lower(None, &of_text(None, TEXT));
    let cam = doc.scene.iter().find(|n| n.label == "cam").unwrap().id;
    let moved = set_eye(&doc, cam, &[("eye_x", 6.5), ("eye_y", 3.25), ("eye_z", 7.125)]);
    let (saved, n) = work(|| sync::reconcile(None, Some(false), &doc, &moved).unwrap());
    assert!(n == 0, "a camera write lowered or evaluated ({n})");
    same_as_full("camera write", &saved);
    // Changed ancestors stay shared with their records, so the next root edit reaches the scene.
    let mut root = saved.root.clone();
    root.width = 640;
    let resized = sync::write(None, &saved, document::Level::Scene, &sync::Edit::Root(root)).unwrap().expect("a root write").0;
    assert!(resized.root.width == 640, "a camera write left the root's recorded value detached");
    same_as_full("root edit after camera write", &resized);
    // a literal scrub of a card inside the closure-holding graph
    let op = E::Op::SetArg { node: vec!["g".into(), "b".into()], key: E::ArgKey::Kw("translate".into()), sub: vec![0], value: flow_syntax::syntax::parse("3").unwrap().remove(0) };
    let ws = workspace_doc::edit(&rays_editor::workspace_catalog(None).unwrap(), &saved.workspace.ws, &op).unwrap();
    let scrubbed = lower(Some(saved.clone()), &ws);
    same_as_full("literal scrub", &scrubbed);
}
