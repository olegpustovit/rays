//! test_editor_consistency.ml: cross-representation checks of authored text, derived state, saved
//! state and the picture.
use std::sync::Arc;

use crate::common;
use super::support::*;
use editor_core::panels::Panel;
use editor_document::document;
use editor_document::kind_graph as kg;
use editor_document::layout_edit;
use editor_document::lowering;
use editor_document::objects;
use editor_document::preset;
use editor_document::scene_sync;
use editor_document::settings;
use editor_document::workspace_doc as wd;
use flow_graph::flow_edit::{ArgKey, Op};
use input::{Event, Key};
use param::Value;
use rays_editor::Editor;

pub const CASES: &[(&str, fn())] = &[
    ("consistency_frozen_driven", frozen_driven),
    ("consistency_settings_authority", settings_authority),
    ("consistency_settings_rebuild", settings_rebuild),
    ("consistency_settings_presets", settings_presets),
    ("consistency_settings_graph_writeback", settings_graph_writeback),
    ("consistency_startup_settings", startup_settings),
    ("consistency_frozen_row0", frozen_row0),
    ("consistency_frozen_row1", frozen_row1),
    ("consistency_frozen_row2", frozen_row2),
    ("consistency_frozen_row3", frozen_row3),
    ("consistency_frozen_row4", frozen_row4),
    ("consistency_frozen_editor", frozen_editor),
    ("consistency_scene_picture", scene_picture),
    ("consistency_empty_preview", empty_preview),
    ("consistency_preview_identity", preview_identity),
    ("consistency_preview_lights", preview_lights),
    ("consistency_live_lights", live_lights),
    ("consistency_document_forms", document_forms),
];

pub const PLAIN: &str = "(workspace consistency (graph g :context sop (sop/box)))";

fn settings_schema() -> Arc<param::Schema> {
    Arc::new(
        param::schema(
            "consistency".to_owned(),
            vec![
                param::field("amount".to_owned(), param::floating(0., 10., None, None).unwrap(), Value::Float(0.)).unwrap().label("Amount".to_owned()).unwrap(),
                param::field("mode".to_owned(), param::text(), Value::Text(b"default".to_vec())).unwrap().label("Mode".to_owned()).unwrap(),
            ],
        )
        .unwrap(),
    )
}

pub fn settings_of(x: f64, mode: &str) -> settings::T {
    settings::make(&settings_schema(), vec![Value::Float(x), Value::Text(mode.as_bytes().to_vec())])
}

pub fn got(s: &settings::T) -> Vec<Value> {
    settings::get(&settings_schema(), s).unwrap()
}

fn vals(x: f64, mode: &str) -> Vec<Value> {
    vec![Value::Float(x), Value::Text(mode.as_bytes().to_vec())]
}

/// `with_editor`: a fresh editor, one frame, `f`, close.
pub fn with_editor(ws: &wd::T, camera: Option<rays::easy_camera::t>, f: impl FnOnce(&mut Ed)) {
    if !common::gpu() {
        return;
    }
    with_dir(|presets| {
        let mut e = step(create(ws, Some(presets), camera, false), 1);
        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| f(&mut e)));
        Editor::close(&e);
        if let Err(p) = result {
            std::panic::resume_unwind(p);
        }
    });
}

pub fn upd(e: &mut Ed, f: impl FnOnce(Ed) -> Ed) {
    *e = f(e.clone());
}

fn settings_authority() {
    let ws = workspace_with(Some(settings_of(0., "default")), &format!("{PLAIN}\n(settings :amount 3.0)"));
    with_editor(&ws, None, |e| {
        assert_eq!(got(&Editor::settings(e)), vals(3., "default"), "opening a checked workspace discarded its saved settings");
        upd(e, |e| Editor::set_settings(e, &settings_of(7., "changed")));
        assert_eq!(got(&Editor::workspace(e).settings), vals(7., "changed"), "set_settings left Workspace_doc.settings behind");
        upd(e, |e| edit(e, &Op::SetGraph { name: "g".into(), form: parse("(graph g :context sop (sop/box :size [2 2 2]))") }));
        assert_eq!(got(&Editor::workspace(e).settings), vals(7., "changed"), "a graph edit rolled settings back");
    });
}

fn settings_rebuild() {
    let old = build(&workspace_with(Some(settings_of(2., "old")), PLAIN));
    let edited = workspace_with(Some(old.settings.clone()), &format!("{PLAIN}\n(settings :amount 4.0)"));
    let doc = build_with(Some(&old), &edited);
    assert_eq!(got(&doc.settings), vals(4., "default"), "lowering preferred previous settings over explicit text or retained omitted fields");
    let mut ws = edited.clone();
    ws.settings = settings_of(9., "explicit");
    assert_eq!(got(&build_with(Some(&doc), &ws).settings), vals(9., "explicit"), "lowering ignored authored workspace settings");
}

fn settings_presets() {
    with_dir(|dir| {
        let doc = build(&workspace_with(Some(settings_of(0., "default")), PLAIN));
        let path = preset::save(dir, "defaults", &doc, &parse("{}")).unwrap();
        let loaded = preset::load(None, &path, None, &settings_of(8., "running")).unwrap();
        assert_eq!(got(&loaded.doc.settings), vals(0., "default"), "loading a preset inherited current values for omitted default settings");
    });
}

fn settings_graph_writeback() {
    let ws = workspace("(workspace settings (graph config :context settings (settings/config :fps 60)))");
    with_editor(&ws, None, |e| {
        let schema = Arc::new(
            param::schema(
                "fps".to_owned(),
                vec![param::field("fps".to_owned(), param::integer(1, 120, None, None).unwrap(), Value::Int(60)).unwrap().label("FPS".to_owned()).unwrap()],
            )
            .unwrap(),
        );
        upd(e, |e| Editor::set_settings(e, &settings::make(&schema, vec![Value::Int(90)])));
        let loaded = build(&workspace(&wd::to_text(&Editor::workspace(e)).unwrap()));
        assert!(settings::fields(&loaded.settings).iter().any(|f| f.name == "fps" && f.current == Value::Int(90)), "set_settings changed a graph-owned value without writing the settings graph");
    });
}

fn startup_settings() {
    let ws = workspace("(workspace startup (graph config :context settings (settings/config)))");
    with_editor(&ws, None, |e| {
        let fields = settings::fields(&build(&Editor::workspace(e)).settings);
        assert!(fields.len() == 6 && fields.iter().all(|f| f.label.ends_with("(on restart)") || f.label.ends_with("(live)")), "workspace startup fields do not explain when edits apply");
        for (i, (name, literal)) in [("title", "\"Changed title\""), ("width", "900"), ("height", "640"), ("fps", "90"), ("seed", "42")].iter().enumerate() {
            let before = Editor::workspace(e);
            upd(e, |e| edit(e, &Op::SetArg { node: s(&["config", "@result"]), key: ArgKey::Kw((*name).to_owned()), sub: vec![], value: parse(literal) }));
            let after = Editor::workspace(e);
            let reloaded = workspace(&wd::to_text(&after).unwrap());
            let w = |x: &wd::T| lowering::window(x).map(|w| format!("{w:?}")).map_err(|d| rays_diagnostic::to_string(&d));
            assert_eq!(w(&reloaded), w(&after), "startup setting did not survive reload: {name}");
            assert_ne!(w(&after), w(&before), "startup setting did not update saved configuration: {name}");
            upd(e, |e| Editor::update(e, &undo_frame((450., 300.), i as i64 + 2)));
            assert!(same_ws(&Editor::workspace(e), &before), "startup setting undo did not restore its source: {name}");
        }
    });
}

pub type Staged = Vec<Vec<(Option<Vec<u8>>, Vec<u8>)>>;

/// The 3D layers of the staged picture: per entry its transform uniforms and vertices.
pub fn layers(e: &Ed) -> Staged {
    let staged = rays::scene::Layer::stage_native(900, 640, &Editor::scene(e, &frame(1))).unwrap_or_else(|m| panic!("{m}"));
    staged
        .layers
        .iter()
        .filter_map(|l| match l {
            rays::scene::Layer::native_layer::Scene3_layer(p) => Some(
                p.entries
                    .iter()
                    .map(|entry| (entry.draw.state.transform_uniforms.as_ref().map(|b| b.borrow().clone()), entry.draw.mesh.vertices.borrow().clone()))
                    .collect(),
            ),
            _ => None,
        })
        .collect()
}

fn geometry_len(e: &Ed) -> Option<(i64, i64)> {
    Editor::prepared(e).map(|g| (rdk::geometry::point_count(&g), rdk::geometry::primitive_count(&g)))
}

pub const SCENE_TEXT: &str = r#"(workspace picture
  (graph g :context sop (sop/box))
  (graph scene :context scene
    (let* [object (scene/geometry (ref g) :name "box")
           lamp (scene/light :intensity 1)] (scene/merge object lamp)))
  (graph editor :context editor (ui/workspace (ui/viewport (ref scene)))))"#;

const FROZEN: [(&str, &str, &str); 5] = [
    ("scene", "(scene/light :width (+ 1 t))", "scene/light.width"),
    ("scene", "(scene/geometry (ref g) :translate [t 0 0])", "scene/geometry.translate"),
    ("world", "(world/sky :turbidity (+ 2 t))", "world/sky.turbidity"),
    ("settings", "(settings/config :fps (+ 60 (int t)))", "settings/config.fps"),
    ("editor", "(ui/workspace (ui/split-at \"horizontal\" (+ 0.4 (* 0.1 t)) (ui/graph) (ui/inspector)))", "ui/split-at.ratio"),
];

fn frozen_row(i: usize) {
    let (context, body, field) = FROZEN[i];
    {
        let ws = workspace(&format!("(workspace time (graph g :context sop (sop/box)) (graph target :context {context} {body}))"));
        let at = |time: f64| {
            let r = flow_eval::run(&ws.checked, false, None, None, None, time).unwrap();
            format!("{:?}", r.results.iter().find(|(n, _)| n == "target").map(|(_, v)| v.clone()))
        };
        assert_ne!(at(0.), at(2.), "fixture did not vary at two times: {field}");
        let diagnostic = match lowering::of_workspace(&Default::default(), &ws) {
            Err(d) => d,
            Ok(_) => panic!("frozen field was accepted: {field}"),
        };
        assert!(diagnostic.code == "E_CONTEXT_TIME" && diagnostic.message.contains(field), "frozen-time refusal did not identify the field: {}", rays_diagnostic::to_string(&diagnostic));
        assert!(lowering::window(&ws).is_err(), "startup window checking ignored a frozen context");
        let reloaded = workspace(&wd::to_text(&ws).unwrap());
        assert!(lowering::of_workspace(&Default::default(), &reloaded).is_err(), "serialized reload bypassed frozen-time refusal");
    }
}

fn frozen_row0() { frozen_row(0) }
fn frozen_row1() { frozen_row(1) }
fn frozen_row2() { frozen_row(2) }
fn frozen_row3() { frozen_row(3) }
fn frozen_row4() { frozen_row(4) }

fn frozen_editor() {
    with_editor(&workspace(SCENE_TEXT), None, |e| {
        let before = Editor::workspace(e);
        let label = Editor::undo_label(e);
        for (node, key, expr) in [(vec!["scene", "object"], "translate", "[t 0 0]"), (vec!["scene", "lamp"], "width", "(+ 1 t)")] {
            match Editor::edit(e.clone(), &set_arg(&node, key, expr)) {
                Ok(_) => panic!("host installed a frozen time field"),
                Err(message) => assert!(message.contains("E_CONTEXT_TIME") && same_ws(&Editor::workspace(e), &before) && Editor::undo_label(e) == label, "frozen-time refusal changed source/history"),
            }
        }
        upd(e, |e| edit(e, &set_arg(&["scene", "object"], "translate", "[2 0 0]")));
        assert!(lowering::of_workspace(&Default::default(), &workspace(&wd::to_text(&Editor::workspace(e)).unwrap())).is_ok(), "literal replacement did not survive reload");
        upd(e, |e| Editor::update(e, &undo_frame((450., 300.), 2)));
        assert!(same_ws(&Editor::workspace(e), &before), "literal replacement did not undo in one entry");
    });
}

fn scene_picture() {
    with_editor(&workspace(SCENE_TEXT), None, |e| {
        let before = layers(e);
        let prepared = geometry_len(e);
        upd(e, |e| edit(e, &set_arg(&["scene", "object"], "translate", "[3 0 0]")));
        upd(e, |e| step(e, 2));
        assert_eq!(geometry_len(e), prepared, "moving an object changed its SOP output");
        assert!(layers(e) != before, "a scene-only host edit left the picture stale");
        let after = layers(e);
        upd(e, |e| step(e, 3));
        assert!(layers(e) == after, "an idle frame changed the picture");
    });
}

pub const PREVIEW_TEXT: &str = r#"(workspace previews
  (graph g :context sop (sop/box))
  (graph scene :context scene [(lamp : float 1)]
    (let* [object (scene/geometry (ref g)) light (scene/light :intensity lamp)]
      (scene/merge object light)))
  (graph empty :context scene (scene/merge))
  (graph editor :context editor
    (let* [main (ui/viewport (ref scene))
           comparison (ui/viewport (ref scene :lamp 2))
           blank (ui/viewport (ref empty))]
      (ui/workspace (ui/tile main comparison blank)))))"#;

pub fn shell(doc: &document::T) -> &document::Shell {
    doc.shell.as_ref().expect("shell")
}

pub fn view_keys(doc: &document::T) -> Vec<String> {
    editor_core::panels::leaves(&shell(doc).tree).into_iter().filter_map(|(_, p)| if let Panel::View(k) = p { Some(k) } else { None }).collect()
}

fn empty_preview() {
    let doc = build(&workspace(PREVIEW_TEXT));
    assert!(shell(&doc).views.iter().any(|(k, v)| k == "v:\"editor\"/\"blank\"" && v.is_empty()), "an empty scene instance lost its viewport identity");
    with_editor(&workspace(PREVIEW_TEXT), None, |e| {
        assert_eq!(layers(e).iter().map(|l| l.len()).collect::<Vec<_>>(), vec![1, 1, 0], "an empty preview rendered the primary scene");
    });
}

pub fn edit_doc(doc: &document::T, op: &Op) -> document::T {
    let ws = wd::edit(&catalog(), &doc.workspace.ws, op).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    build_with(Some(doc), &ws)
}

fn edit_layout_doc(doc: &document::T, op: &layout_edit::Op) -> document::T {
    let ws = wd::edit_layout(&catalog(), &doc.workspace.ws, op).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    build_with(Some(doc), &ws)
}

fn source_of<'a>(doc: &'a document::T, key: &str) -> &'a document::PreviewSource {
    &shell(doc).preview_sources.iter().find(|(k, _)| k == key).unwrap_or_else(|| panic!("no preview source {key}")).1
}

fn preview_identity() {
    let doc = build(&workspace(PREVIEW_TEXT));
    let names = view_keys(&doc);
    let mut sorted = names.clone();
    sorted.sort();
    sorted.dedup();
    assert!(names.len() == 3 && sorted.len() == 3, "named viewport keys are not unique");
    let comparison = "v:\"editor\"/\"comparison\"";
    let provenance = source_of(&doc, comparison);
    assert!(
        provenance.editor_graph == "editor" && matches!(&provenance.panel, Some(document::Origin::Bound(n)) if n == "comparison") && provenance.scene_ref.as_ref().map(flat).as_deref() == Some("(ref scene :lamp 2)"),
        "comparison preview lost the authored ref/override origin"
    );
    let changed = edit_doc(&doc, &set_pos(&["editor", "comparison"], "(ref scene :lamp 4)"));
    let changed_source = source_of(&changed, comparison);
    let main_ref = |d: &document::T| source_of(d, "v:\"editor\"/\"main\"").scene_ref.as_ref().map(flat);
    assert!(
        format!("{:?}", changed_source.instance) != format!("{:?}", provenance.instance) && changed_source.scene_ref.as_ref().map(flat).as_deref() == Some("(ref scene :lamp 4)") && main_ref(&changed) == main_ref(&doc),
        "editing an instance lost its override or changed another viewport's ref"
    );
    let swapped = edit_doc(&doc, &set_pos(&["editor", "@result"], "(ui/tile blank comparison main)"));
    let mut rev = names.clone();
    rev.reverse();
    let mut a = shell(&swapped).views.clone();
    let mut b = shell(&doc).views.clone();
    a.sort();
    b.sort();
    assert!(view_keys(&swapped) == rev && a == b, "reordering named panels transferred preview membership");
    let docked = edit_layout_doc(&swapped, &layout_edit::Op::DockPanel { node: s(&["editor", "comparison"]), target: s(&["editor", "main"]), side: layout_edit::Side::Right });
    let mut dk = view_keys(&docked);
    dk.sort();
    assert_eq!(dk, sorted, "docking changed named viewport identity: {}", view_keys(&docked).join(","));
    let reloaded = build(&workspace(&wd::to_text(&docked.workspace.ws).unwrap()));
    assert_eq!(view_keys(&reloaded), view_keys(&docked), "reload changed named viewport identity");
    let renamed = edit_doc(&docked, &Op::Rename { node: s(&["editor", "comparison"]), to_: "alternate".into() });
    assert!(view_keys(&renamed).contains(&"v:\"editor\"/\"alternate\"".to_owned()) && !view_keys(&renamed).contains(&comparison.to_owned()), "renaming retained the old authored viewport key");
    let closed = edit_layout_doc(&renamed, &layout_edit::Op::ClosePanel { node: s(&["editor", "alternate"]) });
    assert!(
        !shell(&closed).preview_sources.iter().any(|(k, _)| k == "v:\"editor\"/\"alternate\"") && shell(&closed).views.iter().all(|(k, _)| view_keys(&closed).contains(k)),
        "closed preview left orphan membership/provenance"
    );
    let id = shell(&doc).views.iter().find(|(k, _)| k == comparison).unwrap().1[0];
    let (scene, _) = kg::set_fields(&doc.scene, id, &[("translate_x".to_owned(), Value::Float(3.))]).unwrap();
    let mut after = doc.clone();
    after.scene = scene;
    assert!(matches!(scene_sync::reconcile(None, None, &doc, &after), Err(m) if m.to_string().contains("viewport scene reference")), "derived preview edit was not explicitly refused");
    let repeated = edit_doc(&doc, &set_pos(&["editor", "@result"], "(ui/tile main main)"));
    assert_eq!(view_keys(&repeated), vec!["v0".to_owned(), "v1".to_owned()], "repeated named panels did not get distinct placement fallback keys");
    let inline = edit_doc(&doc, &set_pos(&["editor", "@result"], "(ui/tile (ui/viewport (ref scene)) (ui/viewport (ref empty)))"));
    assert!(view_keys(&inline) == vec!["v0".to_owned(), "v1".to_owned()] && shell(&inline).preview_sources.iter().all(|(_, src)| src.scene_ref.is_some()), "inline preview fallback lost its authored ref");
}

fn preview_lights() {
    with_editor(&workspace(PREVIEW_TEXT), None, |e| {
        let before = layers(e);
        upd(e, |e| edit(e, &set_pos(&["editor", "comparison"], "(ref scene :lamp 5)")));
        upd(e, |e| step(e, 2));
        let after = layers(e);
        assert!(after[0] == before[0], "a comparison viewport changed the primary scene's lighting");
        assert!(after[1] != before[1], "a comparison viewport did not update its own lighting");
    });
}

fn live_lights() {
    let ws = workspace(
        r#"(workspace live_lights
    (graph g :context sop (sop/box))
    (graph scene :context scene [(lamp : float 1)]
      (scene/merge (scene/geometry (ref g))
        (scene/light :intensity (+ lamp t) :color [(+ 0.1 (* 0.1 t)) 0.2 0.3])))
    (graph editor :context editor
      (let* [main (ui/viewport (ref scene)) comparison (ui/viewport (ref scene :lamp 2))]
        (ui/workspace (ui/tile main comparison)))))"#,
    );
    let doc = build(&ws);
    let main = "v:\"editor\"/\"main\"";
    assert!(!shell(&doc).views.iter().any(|(k, _)| k == main) && doc.scene_drives.len() == 2, "live residuals turned the primary viewport into a duplicate read-only preview");
    let resolve = |doc: &document::T, time: f64| {
        let (scene, errors) = lowering::resolve_scene(None, None, None, doc, time);
        assert!(errors.is_empty(), "valid live fields produced diagnostics");
        scene
    };
    let intensities = |g: &kg::T| objects::lights(true, g).iter().map(|l| l.intensity).collect::<Vec<_>>();
    let zero = resolve(&doc, 0.);
    let two = resolve(&doc, 2.);
    assert!(intensities(&zero) == vec![1., 2.] && intensities(&two) == vec![3., 4.], "light residuals did not resolve independently at two times");
    let reloaded = build(&workspace(&wd::to_text(&ws).unwrap()));
    assert!(intensities(&resolve(&reloaded, 2.)) == intensities(&two), "live light source did not survive serialization");
    with_editor(&ws, None, |e| {
        let source = Editor::workspace(e);
        let prepared = Editor::prepared(e);
        let before_layers = layers(e);
        let lights = Editor::lights(e);
        let before_time = rays_editor::Timeline::time(&Editor::timeline(e));
        let mut live = frame(2);
        live.input.dt = 2.;
        upd(e, |e| Editor::update(e, &live));
        assert!(same_ws(&Editor::workspace(e), &source) && Editor::undo_label(e).is_none(), "timeline lighting changed authored state or history");
        assert!(prepared.is_some() && Editor::prepared(e).is_some(), "timeline lighting re-prepared geometry");
        assert!(format!("{:?}", Editor::lights(e)) != format!("{:?}", lights) && layers(e) != before_layers, "timeline lighting did not reach staged rendering");
        assert_eq!(rays_editor::Timeline::time(&Editor::timeline(e)), before_time + 2., "live lighting used runtime time instead of the editor timeline");
        let frozen = |n: i64| {
            let mut f = frame(n);
            f.input.dt = 0.;
            f
        };
        let before = layers(e);
        upd(e, |e| edit(e, &set_pos(&["editor", "comparison"], "(ref scene :lamp 5)")));
        upd(e, |e| Editor::update(e, &frozen(3)));
        let after = layers(e);
        assert!(after[0] == before[0] && after[1] != before[1], "a live comparison override changed primary lighting");
        let mut f = frozen(4);
        f.input.keys = vec![Key::Meta];
        f.input.events = vec![Event::Key_pressed(Key::KeyChar('z'))];
        upd(e, |e| Editor::update(e, &f));
        assert!(layers(e) == before, "one undo did not restore live comparison lighting");
    });
}

fn document_forms() {
    for extra in ["\n(workspace other)", "\n(settngs :amount 4)", "\n(layout)\n(layout)", "\n(settings)\n(settings)"] {
        assert!(wd::of_text(&Default::default(), &catalog(), &format!("{PLAIN}{extra}")).is_err(), "a document silently discarded {extra}");
    }
    let mut layout = editor_document::layout_by_path::T::default();
    layout.at.insert(s(&["g"]), (20., 30.));
    let with_layout = wd::of_text(&wd::TextOptions { layout: Some(layout.clone()), ..Default::default() }, &catalog(), PLAIN).unwrap();
    assert!(with_layout.layout.at == layout.at, "omitting layout discarded its fallback");
    let explicit = wd::of_text(&wd::TextOptions { layout: Some(layout), ..Default::default() }, &catalog(), &format!("{PLAIN}\n(layout)")).unwrap();
    assert!(editor_document::layout_by_path::is_empty(&explicit.layout), "an explicit empty layout did not clear its fallback");
}

fn frozen_driven() {
    let driven = workspace("(workspace driven\n    (graph g :context sop (sop/box :size [(+ 1 t) 1 1]))\n    (graph scene :context scene (scene/geometry (ref g))))");
    assert!(lowering::of_workspace(&Default::default(), &driven).is_ok(), "a live SOP reference was mistaken for a frozen scene field");
}

pub const SKIPPED: &[(&str, &str)] = &[];
