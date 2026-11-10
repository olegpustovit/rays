//! Tests marked slow lower many documents; a lowering
//! costs ~200 ms in a debug build (flow_eval static evaluation), so they run with RAYS_SLOW=1.
//!
//! Not ported: `qualification_roots` (reads Flow.Workspace physical identity/qualification of the
//! lowering itself), the "through the editor" block that needs `Rays_editor.Editor`, and the loop
//! that lowers every small sketch of the repository (needs `Rays_editor.Source.read_imports`).
use crate::document::{self, Home, Level};
use crate::kind_graph as kg;
use crate::kinds;
use crate::layers;
use crate::lowering::{self, Options};
use crate::objects::{self, root};
use crate::scene_sync::{self as sync, Edit};
use crate::settings;
use crate::workspace_doc::{self, TextOptions};
use flow_check::catalog::Catalog;
use flow_graph::flow_edit::{ArgKey, Op};
use flow_syntax::syntax::{self as s, Syntax};
use param::Value;
use sop::edit_graph::Factory;

type Doc = document::T;

fn check(condition: bool, message: &str) {
    if !condition {
        panic!("{message}");
    }
}

fn factories() -> Vec<Factory> {
    sop::nodes::factories()
}

fn catalog() -> std::sync::Arc<Catalog> {
    kinds::shared(None).expect("catalog")
}

fn contains(text: &str, piece: &str) -> bool {
    text.contains(piece)
}

const TEXT: &str = r#"(workspace sync
  (graph g :context sop (sop/box))
  (graph scene :context scene
    (let* [body (scene/geometry (ref g) :name "body")
           arm (scene/geometry (ref g) :name "arm" :translate [1 0 0])
           cam (scene/camera :name "cam" :eye [0 2 6])
           side (scene/camera :name "side" :eye [6 2 0])
           studio (scene/world (ref world) :name "studio" :exposure 0.5)
           all (scene/merge body arm cam side (scene/light :name "fill") studio)]
      all))
  (graph world :context world
    (world/sun (world/sky :name "sky") :name "sun"))
  (graph settings :context settings (settings/config :title "t" :width 640)))"#;

fn ws_of_text(text: &str) -> workspace_doc::T {
    match workspace_doc::of_text(&TextOptions::default(), &catalog(), text) {
        Ok(w) => w,
        Err(ds) => panic!("{}", ds.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("; ")),
    }
}

fn lower(previous: Option<&Doc>, workspace: &workspace_doc::T) -> Doc {
    let options = Options { factories: Some(factories()), previous: previous.cloned(), ..Options::default() };
    match lowering::of_workspace(&options, workspace) {
        Ok(doc) => doc,
        Err(d) => panic!("{}", rays_diagnostic::to_string(&d)),
    }
}

fn open_text(text: &str) -> Doc {
    lower(None, &ws_of_text(text))
}

fn scene(doc: &Doc) -> kg::T {
    document::scene_graph(doc)
}

fn node_id(doc: &Doc, label: &str) -> i64 {
    scene(doc).iter().find(|i| i.label == label).unwrap_or_else(|| panic!("no object {label}")).id
}

type Fields = Vec<(String, Value)>;

fn fields(node: &kg::Node) -> Fields {
    kg::fields(node).into_iter().map(|f| (f.name, f.current)).collect()
}

fn label_of(g: &[kg::Node], id: i64) -> Option<String> {
    kg::find(g, id).map(|n| n.label)
}

/// A document as labels say it: ids are allowed to differ.
#[derive(PartialEq, Debug)]
struct Snapshot {
    objects: Vec<(String, String, Option<String>, Fields)>,
    layers: Vec<(String, String, Fields)>,
    camera: Option<String>,
    settings: Fields,
}

fn snapshot(doc: &Doc) -> Snapshot {
    let graph = scene(doc);
    let mut objects: Vec<(String, String, Option<String>, Fields)> =
        graph.iter().map(|i| (i.label.clone(), i.kind.clone(), objects::parent(&graph, i.id).and_then(|p| label_of(&graph, p)), fields(i))).collect();
    objects.sort_by(|a, b| (&a.0, &a.1, &a.2).cmp(&(&b.0, &b.1, &b.2)));
    let mut layers = Vec::new();
    for wid in objects::ids("world", &graph) {
        if let Some(l) = doc.layers.get(&wid) {
            for node in kg::stack(l) {
                layers.push((node.label.clone(), node.kind.clone(), fields(&node)));
            }
        }
    }
    let camera = doc.active_camera.and_then(|c| label_of(&graph, c));
    let mut st: Fields = settings::fields(&doc.settings).into_iter().map(|f| (f.name, f.current)).collect();
    st.sort_by(|a, b| a.0.cmp(&b.0));
    Snapshot { objects, layers, camera, settings: st }
}

fn layer_labels(doc: &Doc) -> Vec<String> {
    snapshot(doc).layers.into_iter().map(|(l, _, _)| l).collect()
}

fn source(doc: &Doc) -> String {
    workspace_doc::to_text(&doc.workspace.ws).unwrap()
}

/// The text of `doc`, opened again, says the same.
fn same_after_reload(doc: &Doc, what: &str) {
    check(snapshot(&open_text(&source(doc))) == snapshot(doc), &format!("{what}: the saved text is not the document"));
}

fn with_scene(doc: &Doc, graph: kg::T) -> Doc {
    Doc { scene: graph, ..doc.clone() }
}

fn set_id(doc: &Doc, id: i64, values: &[(String, Value)]) -> Doc {
    with_scene(doc, kg::set_fields(&scene(doc), id, values).unwrap().0)
}

fn set(doc: &Doc, label: &str, values: &[(String, Value)]) -> Doc {
    set_id(doc, node_id(doc, label), values)
}

fn reconcile(adopt: Option<bool>, before: &Doc, after: &Doc) -> Result<Doc, String> {
    sync::reconcile(Some(&factories()), adopt, before, after)
}

fn ok(result: Result<Doc, String>) -> Doc {
    match result {
        Ok(doc) => doc,
        Err(message) => panic!("reconcile: {message}"),
    }
}

fn float(name: &str, x: f64) -> (String, Value) {
    (name.to_owned(), Value::Float(x))
}

fn world_network(doc: &Doc) -> (i64, kg::T) {
    let wid = objects::ids("world", &scene(doc))[0];
    (wid, doc.layers[&wid].clone())
}

fn layer_id(doc: &Doc, label: &str) -> i64 {
    world_network(doc).1.iter().find(|i| i.label == label).unwrap().id
}

/// An edit the list, a key or a panel asks of objects of the text.
fn write(level: Level, doc: &Doc, edit: &Edit) -> Result<Doc, String> {
    match sync::write(Some(&factories()), doc, level, edit)? {
        Some((doc, _)) => Ok(doc),
        None => panic!("the text did not take the edit"),
    }
}

/// ... which lowers to what the same edit of the derived objects is.
fn wrote(what: &str, level: Level, doc: &Doc, edit: &Edit, after: &Doc) -> Doc {
    let written = ok(write(level, doc, edit));
    check(snapshot(&written) == snapshot(after), &format!("{what}: the text-first edit is not the edit of the derived document:\n{}", source(&written)));
    written
}

/// A name, a parent and a field are also what `reconcile` writes for a camera the host moves: the
/// same text.
fn text_first(what: &str, doc: &Doc, level: Level, edit: &Edit, after: &Doc) {
    match sync::write(Some(&factories()), doc, level, edit) {
        Ok(Some((text_first, _))) => check(
            source(&text_first) == source(&ok(reconcile(None, doc, after))),
            &format!("{what}: the text-first edit and the reconciled one wrote different text:\n{}", source(&text_first)),
        ),
        Ok(None) => panic!("{what}: the text did not take the edit"),
        Err(message) => panic!("{what}: {message}"),
    }
}

fn mk(node: s::Draft) -> Syntax {
    s::make(node)
}

fn sym(name: &str) -> Syntax {
    mk(s::Draft::Sym(name.to_owned()))
}

fn call(head: &str, args: Vec<Syntax>) -> Syntax {
    let mut items = vec![sym(head)];
    items.extend(args);
    let mut builder = s::SyntaxBuilder::new();
    let children = items.iter().map(|form| builder.import(form)).collect();
    let root = builder.push_draft(0, s::Draft::List(children), rays_diagnostic::Span { start: 0, finish: 0 }, None);
    builder.freeze().handle(root)
}

fn edit_ws(doc: &Doc, op: &Op) -> Result<workspace_doc::T, String> {
    workspace_doc::edit(&catalog(), &doc.workspace.ws, op).map_err(|d| rays_diagnostic::to_string(&d))
}

/// Apply an edit and lower the resulting document.
fn edit(doc: &Doc, op: &Op) -> Doc {
    lower(Some(doc), &edit_ws(doc, op).unwrap())
}

fn names(doc: &Doc) -> Vec<String> {
    let mut v: Vec<String> = scene(doc).iter().map(|i| i.label.clone()).collect();
    v.sort();
    v
}

fn strs(v: &[&str]) -> Vec<String> {
    v.iter().map(|s| (*s).to_owned()).collect()
}

fn light_kind() -> kg::Kind {
    objects::light::kind()
}

// ---- pure helpers (no lowering) ----

#[test]
fn value_syntax_prints_numbers_flags_and_vectors() {
    let geometry = kg::instance(None, "g", &objects::geometry::kind());
    let all = kg::fields(&geometry);
    let groups = param::group_vec3(&all, true).unwrap();
    let translate = groups.iter().find(|g| g.len() == 3 && g[0].name == "translate_x").expect("translate group");
    assert_eq!(flow_syntax::lisp::flat(&sync::value_syntax(translate)), "[0.0 0.0 0.0]");
    let visible = all.iter().find(|f| f.name == "visible").unwrap().clone();
    assert_eq!(flow_syntax::lisp::flat(&sync::value_syntax(&[visible])), "true");
    let scale = groups.iter().find(|g| g.len() == 3 && g[0].name == "scale_x").unwrap();
    assert_eq!(flow_syntax::lisp::flat(&sync::value_syntax(scale)), "[1.0 1.0 1.0]");
    let int_field = kg::fields(&kg::instance(None, "r", &objects::root::kind())).into_iter().find(|f| f.name == "width").unwrap();
    assert_eq!(flow_syntax::lisp::flat(&sync::value_syntax(&[int_field])), "1920");
    let choice = kg::fields(&kg::instance(None, "r", &objects::root::kind())).into_iter().find(|f| f.name == "renderer").unwrap();
    assert_eq!(flow_syntax::lisp::flat(&sync::value_syntax(&[choice])), "\"Raster\"");
}

#[test]
fn a_camera_still_takes_no_render_size() {
    // "a camera still takes a render size" (the checker refuses it)
    let result = workspace_doc::of_text(
        &TextOptions::default(),
        &kinds::catalog(1, &factories()).unwrap(),
        r#"(workspace old
    (graph g :context sop (sop/box))
    (graph scene :context scene
      (scene/merge (scene/geometry (ref g)) (scene/camera :width 1600 :height 900 :max_spp 512))))"#,
    );
    check(result.is_err(), "a camera still takes a render size");
}

// ---- run (): fields, inline, rename, reparent, delete, camera, World, settings, host, add ----

#[test]
fn camera_write_back_keeps_precision_and_identity() {
    let doc = open_text(TEXT);
    // Following the viewport writes camera coordinates every frame. Rounding them makes the camera
    // diverge far enough to flash its own frustum when zoomed out.
    for distance in [6.123456789012345, 10000.123456789, 1e7] {
        let after = set(
            &doc,
            "cam",
            &[float("eye_x", distance * 0.123456789012345), float("eye_y", distance * 0.987654321098765), float("eye_z", distance), float("target_x", 0.123456789012345)],
        );
        let saved = ok(reconcile(Some(false), &doc, &after));
        check(snapshot(&saved) == snapshot(&after), "camera write-back rounded its coordinates");
        // the cook and every prepared piece are keyed by the settings and the networks physically:
        // a camera move that rebuilt either would recook and re-prepare the scene every frame
        check(settings::same_values(&saved.settings, &doc.settings), "a camera write-back rebuilt the unchanged settings");
        check(
            saved.networks.iter().all(|(id, network)| doc.networks.get(id).map_or(false, |n| flow_sop_catalog::network::ptr_eq(&n.graph, &network.graph))),
            "a camera write-back rebuilt an unchanged network",
        );
        same_after_reload(&saved, "camera precision");
    }
}

#[test]
fn fields_inline_and_flags() {
    let doc = open_text(TEXT);
    let arm = node_id(&doc, "arm");
    let fill = node_id(&doc, "fill");
    // a field of a bound object
    let edited = ok(reconcile(None, &doc, &set(&doc, "arm", &[float("translate_x", 4.), float("translate_y", 2.)])));
    check(contains(&source(&edited), ":translate [4.0 2.0 0.0]"), "a transform edit did not reach the text");
    check(node_id(&edited, "arm") == arm, "an edit changed an object's id");
    // physical `lowered !=` : the text changed, so it was lowered again
    check(source(&edited) != source(&doc), "the edited text was not lowered again");
    same_after_reload(&edited, "transform");
    // the inspector's and the handles' route writes the fields text first (the derived object is
    // never edited): the same text as the reconciled edit, ranges applied, ids kept
    let direct = |doc: &Doc, label: &str, values: &[(String, Value)]| -> Doc {
        match sync::set_fields(Some(&factories()), doc, Level::Scene, node_id(doc, label), values) {
            Ok(Some((written, _, _))) => written,
            Ok(None) => panic!("{label} has no text"),
            Err(message) => panic!("{message}"),
        }
    };
    let cases: Vec<(&str, Fields)> = vec![
        ("arm", vec![float("translate_x", 4.), float("translate_y", 2.)]),
        ("fill", vec![float("intensity", 99.)]),
        ("cam", vec![float("fov", 500.)]),
    ];
    for (label, values) in &cases {
        let written = direct(&doc, label, values);
        check(
            source(&written) == source(&ok(reconcile(None, &doc, &set(&doc, label, values)))),
            &format!("fields written text first differ from the reconciled edit of {label}: {}", source(&written)),
        );
        check(node_id(&written, label) == node_id(&doc, label), &format!("fields written text first changed the id of {label}"));
    }
    let hidden: Fields = vec![("visible".to_owned(), Value::Bool(false))];
    text_first("flags", &doc, Level::Scene, &Edit::Fields(vec![(arm, hidden.clone()), (fill, hidden.clone())]), &set(&set(&doc, "arm", &hidden), "fill", &hidden));
    // an inline object is unfolded into a binding first
    let edited = ok(reconcile(None, &doc, &set(&doc, "fill", &[float("intensity", 99.)])));
    check(contains(&source(&edited), ":intensity 99.0"), "an inline object's edit did not reach the text");
    check(node_id(&edited, "fill") == fill, "an inline object lost its id when unfolded");
    check(edited.homes.objects.iter().filter(|(id, _)| *id == fill).any(|(_, h)| matches!(h, Home::BoundAt(_))), "the unfolded object has no binding");
    same_after_reload(&edited, "inline object");
}

#[test]
fn rename_reparent_and_unparent() {
    let doc = open_text(TEXT);
    let arm = node_id(&doc, "arm");
    // rename keeps the id, and the children's parent names follow
    let renamed = with_scene(&doc, kg::update(&scene(&doc), arm, &|n| kg::Node { label: "forearm".to_owned(), ..n }));
    let edited = ok(reconcile(None, &doc, &renamed));
    text_first("rename", &doc, Level::Scene, &Edit::Rename(arm, "forearm".to_owned()), &renamed);
    check(contains(&source(&edited), ":name \"forearm\"") && node_id(&edited, "forearm") == arm, "a rename did not keep the id");
    same_after_reload(&edited, "rename");
    // reparent keeping the world placement, rename the parent, then unparent
    let body = node_id(&doc, "body");
    let under = with_scene(&doc, kg::update(&scene(&doc), arm, &|n| kg::Node { input: Some(body), ..n }));
    let edited = ok(reconcile(None, &doc, &under));
    // a list's reparent keeps the world placement: the transform is computed before the text is written
    let placed = open_text(&TEXT.replace(":name \"body\"", ":name \"body\" :translate [0 3 0] :rotate [0 90 0]"));
    {
        let arm = node_id(&placed, "arm");
        let body = node_id(&placed, "body");
        let moved = with_scene(&placed, objects::reparent(&scene(&placed), &[arm], Some(body)));
        text_first("reparent", &placed, Level::Scene, &Edit::Reparent(vec![arm], Some(body)), &moved);
        let saved = ok(reconcile(None, &placed, &moved));
        check(
            contains(&source(&saved), ":parent \"body\"")
                && objects::parent_world(&scene(&saved), arm) != objects::parent_world(&scene(&placed), arm)
                && !contains(&source(&saved), ":translate [1 0 0]"),
            &format!("a reparent kept the local transform: {}", source(&saved)),
        );
        text_first("outdent", &saved, Level::Scene, &Edit::Outdent(vec![arm]), &with_scene(&saved, objects::outdent(&scene(&saved), &[arm])));
    }
    check(contains(&source(&edited), ":parent \"body\""), "a reparent did not reach the text");
    same_after_reload(&edited, "reparent");
    let torso = with_scene(&edited, kg::update(&scene(&edited), body, &|n| kg::Node { label: "torso".to_owned(), ..n }));
    let edited2 = ok(reconcile(None, &edited, &torso));
    text_first("parent rename", &edited, Level::Scene, &Edit::Rename(body, "torso".to_owned()), &torso);
    match sync::write(Some(&factories()), &doc, Level::Scene, &Edit::Rename(arm, "body".to_owned())) {
        Err(_) => {}
        Ok(_) => panic!("a rename to a name another object has was written"),
    }
    check(contains(&source(&edited2), ":parent \"torso\"") && !contains(&source(&edited2), ":parent \"body\""), "renaming a parent left its child pointing at the old name");
    same_after_reload(&edited2, "parent rename");
    let free = with_scene(&edited, kg::update(&scene(&edited), arm, &|n| kg::Node { input: None, ..n }));
    let edited2 = ok(reconcile(None, &edited, &free));
    check(!contains(&source(&edited2), ":parent"), "unparenting left the parent keyword");
    same_after_reload(&edited2, "unparent");
}

#[test]
fn delete_camera_world_and_settings() {
    let doc = open_text(TEXT);
    let arm = node_id(&doc, "arm");
    let fill = node_id(&doc, "fill");
    // delete: a bound object leaves its binding and the merge; an inline one leaves the merge
    let gone = with_scene(&doc, kg::remove(&[arm, fill], &scene(&doc)));
    let edited = wrote("delete", Level::Scene, &doc, &Edit::Delete(vec![arm, fill]), &gone);
    check(!contains(&source(&edited), "\"arm\"") && !contains(&source(&edited), "\"fill\""), "a deleted object stayed in the text");
    check(contains(&source(&edited), "scene/merge body cam side"), "a deleted object stayed in the merge");
    same_after_reload(&edited, "delete");
    // the render camera
    let side = node_id(&doc, "side");
    let edited = wrote("render camera", Level::Scene, &doc, &Edit::Camera(Some(side)), &Doc { active_camera: Some(side), ..doc.clone() });
    check(contains(&source(&edited), ":active true") && edited.active_camera == Some(side), "the render camera did not reach the text");
    let reopened = open_text(&source(&edited));
    check(reopened.active_camera == Some(node_id(&reopened, "side")), "the render camera did not survive a reload");
    // the World node, a layer of an inline stack, a layer rename
    let (wid, network) = world_network(&doc);
    let sun = layer_id(&doc, "sun");
    let world_node = |graph: &kg::T, values: &[(String, Value)]| kg::set_fields(graph, wid, values).unwrap().0;
    let edited = wrote(
        "World key",
        Level::Scene,
        &doc,
        &Edit::Fields(vec![(wid, vec![float("exposure", -1.)])]),
        &with_scene(&doc, world_node(&scene(&doc), &[float("exposure", -1.)])),
    );
    check(contains(&source(&edited), ":exposure -1.0"), "a World node edit did not reach the text");
    same_after_reload(&edited, "world node");
    let layer_edit = |doc: &Doc, id: i64, values: &[(String, Value)]| {
        let (wid, n) = world_network(doc);
        document::with_layers(doc, wid, kg::set_fields(&n, id, values).unwrap().0)
    };
    let edited = wrote("layer key", Level::Inside(wid), &doc, &Edit::Fields(vec![(sun, vec![float("intensity", 321.)])]), &layer_edit(&doc, sun, &[float("intensity", 321.)]));
    check(contains(&source(&edited), ":intensity 321.0"), "a World layer edit did not reach the text");
    check(layer_id(&edited, "sun") == sun, "a World layer edit changed its id");
    same_after_reload(&edited, "world layer");
    // drag a layer (azimuth, elevation) then undo-like: the document before is untouched
    check(snapshot(&doc) == snapshot(&open_text(TEXT)), "an edit changed its input");
    // delete a layer: the stack closes over it
    let edited = ok(write(Level::Inside(wid), &doc, &Edit::Delete(vec![sun])));
    check(!contains(&source(&edited), "world/sun"), "a deleted World layer stayed in the text");
    same_after_reload(&edited, "world layer delete");
    // reorder the stack: sky above sun
    let sky = layer_id(&doc, "sky");
    let swapped = kg::update(&network, sun, &|n| kg::Node { input: None, ..n });
    let swapped = kg::update(&swapped, sky, &|n| kg::Node { input: Some(sun), ..n });
    let swapped = document::with_layers(&doc, wid, swapped);
    let edited = wrote("restack", Level::Inside(wid), &doc, &Edit::Restack(vec![sun, sky]), &swapped);
    same_after_reload(&edited, "world restack");
    check(layer_labels(&edited) == strs(&["sky", "sun"]), "the World stack was not rewritten");
    // a preset replaces the layers: the World graph is written whole
    let world = scene_core::world::presets().into_iter().find(|(n, _)| n == "white room").unwrap().1;
    let preset_network = layers::graph_of_world(&world).unwrap();
    let values = layers::of_world(&world).1;
    let preset = with_scene(&document::with_layers(&doc, wid, preset_network.clone()), world_node(&scene(&doc), &values));
    let edited = wrote("World preset", Level::Inside(wid), &doc, &Edit::Layers(preset_network, values), &preset);
    check(contains(&source(&edited), "world/room"), "a World preset did not reach the text");
    same_after_reload(&edited, "world preset");
    // deleting the World takes the member out of the scene; its graph stays
    let worldless = document::prune(&with_scene(&doc, kg::remove(&[wid], &scene(&doc))));
    let edited = wrote("World delete", Level::Scene, &doc, &Edit::Delete(vec![wid]), &worldless);
    check(!contains(&source(&edited), "(scene/world") && objects::ids("world", &scene(&edited)).is_empty(), "a deleted World stayed in the scene");
    same_after_reload(&edited, "world delete");
    check(objects::ids("world", &scene(&open_text(&source(&edited)))).is_empty(), "a deleted World came back on reload");
    // settings graph
    let new_settings = settings::apply(&doc.settings, &[("width".to_owned(), Value::Int(800))]).unwrap().0;
    let edited = wrote("settings", Level::Scene, &doc, &Edit::Settings(new_settings.clone()), &Doc { settings: new_settings.clone(), ..doc.clone() });
    // a workspace with no settings graph keeps them itself
    let plain = open_text("(workspace plain (graph g :context sop (sop/box)))");
    match sync::write(Some(&factories()), &plain, Level::Scene, &Edit::Settings(new_settings.clone())) {
        Ok(Some((kept, _))) => check(
            settings::same_values(&kept.settings, &new_settings) && settings::same_values(&kept.workspace.ws.settings, &new_settings),
            "settings with no graph were not kept by the workspace",
        ),
        _ => panic!("settings with no graph were refused"),
    }
    check(contains(&source(&edited), ":width 800"), "a settings edit did not reach the text");
    same_after_reload(&edited, "settings");
}

#[test]
fn twins_and_host_objects() {
    // a name two objects share is refused whole
    let twins = open_text(
        r#"(workspace twins
    (graph scene :context scene
      (scene/merge (scene/light :name "a") (scene/light :name "a") (scene/light :name "b"))))"#,
    );
    let b = node_id(&twins, "b");
    let first = scene(&twins).iter().find(|i| i.label == "a").unwrap().id;
    match reconcile(None, &twins, &with_scene(&twins, kg::update(&scene(&twins), b, &|n| kg::Node { input: Some(first), ..n }))) {
        Err(_) => {}
        Ok(_) => panic!("a parent named like another object was written"),
    }
    // objects only the host made: the first explicit edit writes them
    let bare = open_text("(workspace bare (graph g :context sop (sop/box)))");
    let with_light = |doc: &Doc| {
        let graph = kg::add(&scene(doc), kg::instance(None, "light1", &light_kind()));
        with_scene(doc, kg::add(&graph, kg::instance(None, "camera1", &objects::camera::kind())))
    };
    let bare = with_light(&bare);
    check(!contains(&source(&bare), "scene/"), "the host's objects were written before any edit");
    let light1 = node_id(&bare, "light1");
    check(
        matches!(sync::set_fields(Some(&factories()), &bare, Level::Scene, light1, &[float("intensity", 12.)]), Ok(None)) && !sync::in_text(&bare, Level::Scene, light1),
        "an object only the host made was taken to have text",
    );
    let edited = ok(reconcile(None, &bare, &set(&bare, "light1", &[float("intensity", 12.)])));
    check(
        contains(&source(&edited), "graph scene")
            && contains(&source(&edited), "scene/light")
            && contains(&source(&edited), ":intensity 12.0")
            && contains(&source(&edited), "scene/geometry (ref g)")
            && contains(&source(&edited), "scene/camera"),
        "an edited host object was not written to a scene graph",
    );
    same_after_reload(&edited, "adoption");
    // a camera that follows the viewport is not such an edit
    let followed = ok(reconcile(Some(false), &bare, &set(&bare, "camera1", &[float("eye_x", 3.)])));
    check(!contains(&source(&followed), "scene/") && snapshot(&followed) == snapshot(&set(&bare, "camera1", &[float("eye_x", 3.)])), "a follow-the-viewport move was written or lost");
    // a scene graph is authoritative for cameras and lights: what it does not say is not there,
    // and the host seeds nothing
    let with_graphs = |extra: &str| ws_of_text(&format!("(workspace bare (graph g :context sop (sop/box)){extra})"));
    check(names(&lower(Some(&bare), &with_graphs(""))) == strs(&["camera1", "g", "light1"]), "a workspace with no scene graph lost the host's camera and light");
    check(
        names(&lower(Some(&bare), &with_graphs(" (graph scene :context scene (scene/merge (scene/geometry (ref g))))"))) == strs(&["g"]),
        "a scene graph kept the host's camera or light",
    );
    check(names(&lower(Some(&bare), &with_graphs(" (graph scene :context scene (scene/merge))"))).is_empty(), "an empty scene graph kept an object");
    // deleting a host-made camera or light adopts the other host objects into a scene graph and
    // removes the one; the saved text has no seed to bring it back
    let without = |doc: &Doc, label: &str| document::prune(&with_scene(doc, kg::remove(&[node_id(doc, label)], &scene(doc))));
    let no_camera = ok(reconcile(None, &bare, &without(&bare, "camera1")));
    check(
        contains(&source(&no_camera), "graph scene") && contains(&source(&no_camera), "scene/light") && !contains(&source(&no_camera), "scene/camera"),
        "a deleted host camera was not written as absent",
    );
    check(names(&no_camera) == strs(&["g", "light1"]) && names(&open_text(&source(&no_camera))) == strs(&["g", "light1"]), "a deleted host camera came back on reload");
    same_after_reload(&no_camera, "host camera delete");
    let no_light = ok(reconcile(None, &bare, &without(&bare, "light1")));
    check(
        !contains(&source(&no_light), "scene/light") && contains(&source(&no_light), "scene/camera") && names(&open_text(&source(&no_light))) == strs(&["camera1", "g"]),
        "a deleted host light came back on reload",
    );
    // adopted, the others are objects of the text
    let nothing = ok(write(Level::Scene, &no_camera, &Edit::Delete(vec![node_id(&no_camera, "light1")])));
    check(names(&open_text(&source(&nothing))) == strs(&["g"]), "deleting the last host objects was not written");
}

#[test]
fn adding_by_key() {
    let doc = open_text(TEXT);
    let added = edit(
        &doc,
        &Op::AddNode { scope: vec!["scene".into()], name: "lamp".to_owned(), expr: call("scene/light", vec![mk(s::Draft::Kw("name".to_owned())), mk(s::Draft::Str(b"lamp".to_vec()))]) },
    );
    check(
        contains(&source(&added), "scene/merge body arm cam side (scene/light :name \"fill\") lamp") || contains(&source(&added), "lamp)"),
        "a new object did not join the scene's merge",
    );
    check(scene(&added).iter().any(|i| i.label == "lamp"), "a new object is not in the scene");
    let layered = edit(&doc, &Op::AddNode { scope: vec!["world".into()], name: "haze".to_owned(), expr: call("world/gradient", vec![]) });
    check(layer_labels(&layered) == strs(&["Gradient", "sun", "sky"]), "a new World layer is not on top of the stack");
    same_after_reload(&layered, "added layer");
    let twice = edit(&added, &Op::DeleteNodes { nodes: vec![vec!["scene".into(), "lamp".into()]] });
    check(snapshot(&twice) == snapshot(&doc), "deleting what was added did not give the scene back");
    check(edit_ws(&layered, &Op::DeleteNodes { nodes: vec![vec!["world".into(), "haze".into()]] }).is_ok(), "a World layer could not be deleted by its binding");
}

// ---- the copies of a loop are one template ----

fn lamps(doc: &Doc) -> Vec<kg::Node> {
    scene(doc).into_iter().filter(|i| i.kind == "light").collect()
}

fn field(i: &kg::Node, name: &str) -> Option<Value> {
    kg::field(i, name)
}

fn refused(result: Result<Doc, String>) -> String {
    match result {
        Err(message) => message,
        Ok(_) => panic!("an edit the loop cannot take was written"),
    }
}

fn fl(x: f64) -> Option<Value> {
    Some(Value::Float(x))
}

fn sorted_ids(doc: &Doc) -> Vec<i64> {
    let mut v: Vec<i64> = lamps(doc).iter().map(|i| i.id).collect();
    v.sort();
    v
}

fn xs_of(doc: &Doc) -> Vec<Option<Value>> {
    lamps(doc).iter().map(|i| field(i, "translate_x")).collect()
}

fn loop_case(what: &str, text: &str) {
    let doc = open_text(text);
    check(lamps(&doc).len() == 4, &format!("{what}: the loop did not make four lamps"));
    check(doc.homes.objects.iter().filter(|(_, h)| document::loop_of(h).is_some()).count() == 4, &format!("{what}: the copies have no home in the loop"));
    let third = lamps(&doc)[2].id;
    // a literal field: the template is edited, every copy changes, the text says it once
    let edited = ok(reconcile(None, &doc, &set_id(&doc, third, &[float("intensity", 77.)])));
    check(lamps(&edited).iter().all(|i| field(i, "intensity") == fl(77.)), &format!("{what}: an edit of one copy did not change them all"));
    check(
        lamps(&edited).len() == 4 && contains(&source(&edited), ":intensity 77.0") && !contains(&source(&edited), ":intensity 30"),
        &format!("{what}: the template was not edited"),
    );
    check(snapshot(&open_text(&source(&edited))) == snapshot(&edited), &format!("{what}: the saved text is not the document"));
    let note = match sync::write(Some(&factories()), &doc, Level::Scene, &Edit::Fields(vec![(third, vec![float("intensity", 77.)])])) {
        Ok(Some((_, note))) => note,
        _ => None,
    };
    check(
        note.as_ref().map_or(false, |n| contains(n, "loop template") && contains(n, "4 copies")),
        &format!("{what}: the status does not say the loop template and the copies: {}", note.clone().unwrap_or_else(|| "none".to_owned())),
    );
    // a field the loop computes from its variable has no value of its own
    let message = refused(reconcile(None, &doc, &set_id(&doc, third, &[float("translate_x", 9.)])));
    check(
        contains(&message, "translate is computed by the loop") && contains(&message, "(now i)"),
        &format!("{what}: a computed component was not refused with its expression: {message}"),
    );
    match sync::set_fields(Some(&factories()), &doc, Level::Scene, third, &[float("translate_x", 9.)]) {
        Err(direct) => check(direct == message, &format!("{what}: the text-first route refuses otherwise: {direct}")),
        Ok(_) => panic!("{what}: the text-first route wrote a computed component"),
    }
    // a component the loop does not compute is the template's: every copy follows
    let lifted = ok(reconcile(None, &doc, &set_id(&doc, third, &[float("translate_y", 5.)])));
    check(
        contains(&source(&lifted), ":translate [i 5.0 0]") && lamps(&lifted).iter().all(|i| field(i, "translate_y") == fl(5.)) && xs_of(&lifted) == xs_of(&doc),
        &format!("{what}: a literal component of a computed vector was not edited in the template: {}", source(&lifted)),
    );
    // rename: the template's :name
    let renamed = with_scene(&doc, kg::update(&scene(&doc), third, &|n| kg::Node { label: "spot".to_owned(), ..n }));
    let renamed = ok(reconcile(None, &doc, &renamed));
    check(
        lamps(&renamed).iter().all(|i| i.label == "spot") && contains(&source(&renamed), ":name \"spot\"") && !contains(&source(&renamed), ":name \"lamp\""),
        &format!(
            "{what}: a rename did not reach the template :name: {}{}",
            source(&renamed),
            lamps(&renamed).iter().map(|i| i.label.clone()).collect::<Vec<_>>().join(",")
        ),
    );
    // delete one copy: the loop skips that iteration, the others keep their iteration, id and place
    let xs = |v: &[f64]| -> Vec<Option<Value>> { v.iter().map(|x| fl(*x)).collect() };
    let gone = ok(write(Level::Scene, &doc, &Edit::Delete(vec![third])));
    check(
        contains(&source(&gone), ":skip [2]") && !contains(&source(&gone), "take") && !contains(&source(&gone), "drop"),
        &format!("{what}: copy 3 was not skipped: {}", source(&gone)),
    );
    check(xs_of(&open_text(&source(&gone))) == xs(&[0., 1., 3.]), &format!("{what}: the copies after a deleted one moved"));
    check(sorted_ids(&gone) == sorted_ids(&doc).into_iter().filter(|i| *i != third).collect::<Vec<_>>(), &format!("{what}: the other copies changed id"));
    same_after_reload(&gone, &format!("{what} delete a copy"));
    // repeated deletes accumulate in one list; a literal edit still reaches every remaining copy
    let again = ok(write(Level::Scene, &gone, &Edit::Delete(vec![lamps(&gone)[1].id])));
    check(
        contains(&source(&again), ":skip [1 2]") && xs_of(&open_text(&source(&again))) == xs(&[0., 3.]),
        &format!("{what}: a second delete did not accumulate: {}", source(&again)),
    );
    let edited_after = ok(reconcile(None, &again, &set_id(&again, lamps(&again)[0].id, &[float("intensity", 5.)])));
    check(
        lamps(&edited_after).len() == 2 && contains(&source(&edited_after), ":skip [1 2]") && lamps(&edited_after).iter().all(|i| field(i, "intensity") == fl(5.)),
        &format!("{what}: an edit after a delete lost the skip or a copy"),
    );
    same_after_reload(&again, &format!("{what} delete twice"));
    let first = lamps(&doc)[0].id;
    let gone = ok(write(Level::Scene, &doc, &Edit::Delete(vec![first])));
    check(contains(&source(&gone), ":skip [0]") && !contains(&source(&gone), "concat"), &format!("{what}: deleting the first copy"));
    let gone2 = ok(write(Level::Scene, &doc, &Edit::Delete(vec![lamps(&doc)[1].id, lamps(&doc)[3].id])));
    check(xs_of(&open_text(&source(&gone2))) == xs(&[0., 2.]), &format!("{what}: two copies deleted together"));
}

#[test]
fn loop_copies_bound() {
    loop_case(
        "bound",
        r#"(workspace lamps
    (graph g :context sop (sop/box))
    (graph scene :context scene
      (let* [body (scene/geometry (ref g) :name "body")
             row (for [i (range 4)] (scene/light :name "lamp" :intensity 30 :translate [i 2 0]))]
        (scene/merge body row))))"#,
    );
}

#[test]
fn loop_copies_inline() {
    loop_case(
        "inline",
        r#"(workspace lamps
      (graph g :context sop (sop/box))
      (graph scene :context scene
        (scene/merge (scene/geometry (ref g) :name "body")
                     (for [i (range 4)] (scene/light :name "lamp" :intensity 30 :translate [i 2 0])))))"#,
    );
}

#[test]
fn loop_several_clauses() {
    // several clauses: one iteration of the product goes, the flat iteration index is what is listed
    let grid = open_text(
        r#"(workspace grid
    (graph g :context sop (sop/box))
    (graph scene :context scene
      (scene/merge (scene/geometry (ref g) :name "body")
                   (for [i (range 2) j (range 3)] (scene/light :name "lamp" :translate [i j 0])))))"#,
    );
    check(lamps(&grid).len() == 6, "the two-clause loop did not make six lamps");
    let xy = |doc: &Doc| -> Vec<(Option<Value>, Option<Value>)> { lamps(doc).iter().map(|i| (field(i, "translate_x"), field(i, "translate_y"))).collect() };
    let cell = |i: f64, j: f64| (fl(i), fl(j));
    let victim = lamps(&grid)[4].id;
    let gone = ok(write(Level::Scene, &grid, &Edit::Delete(vec![victim])));
    check(contains(&source(&gone), ":skip [4]"), &format!("a product iteration was not skipped: {}", source(&gone)));
    check(xy(&open_text(&source(&gone))) == vec![cell(0., 0.), cell(0., 1.), cell(0., 2.), cell(1., 0.), cell(1., 2.)], "a two-clause delete removed the wrong copy");
    check(sorted_ids(&gone) == sorted_ids(&grid).into_iter().filter(|i| *i != victim).collect::<Vec<_>>(), "a two-clause delete changed the other ids");
    same_after_reload(&gone, "two-clause delete");
    // a bound loop too
    let bound_grid = open_text(
        r#"(workspace grid
    (graph g :context sop (sop/box))
    (graph scene :context scene
      (let* [body (scene/geometry (ref g) :name "body")
             lamps (for [i (range 2) j (range 3)] (scene/light :name "lamp" :translate [i j 0]))]
        (scene/merge body lamps))))"#,
    );
    let gone = ok(write(Level::Scene, &bound_grid, &Edit::Delete(vec![lamps(&bound_grid)[4].id])));
    check(
        lamps(&gone).len() == 5 && contains(&source(&gone), "lamps (for [i (range 2)") && contains(&source(&gone), ":skip [4]"),
        &format!("a bound two-clause delete: {}", source(&gone)),
    );
    same_after_reload(&gone, "bound two-clause delete");
}

#[test]
fn loop_copy_that_makes_two_objects() {
    let pairs = open_text(
        r#"(workspace pairs
    (graph g :context sop (sop/box))
    (graph scene :context scene
      (scene/merge (scene/geometry (ref g) :name "body")
                   (for [i (range 3)]
                     (scene/merge (scene/light :name "a" :translate [i 0 0]) (scene/light :name "b" :translate [i 1 0]))))))"#,
    );
    check(lamps(&pairs).len() == 6, "the loop of pairs did not make six lamps");
    let at = |doc: &Doc, label: &str, i: f64| lamps(doc).into_iter().find(|l| l.label == label && field(l, "translate_x") == fl(i));
    let id_at = |doc: &Doc, label: &str, i: f64| at(doc, label, i).unwrap().id;
    let half = ok(write(Level::Scene, &pairs, &Edit::Delete(vec![id_at(&pairs, "a", 0.)])));
    check(
        contains(&source(&half), ":skip [[0 0]]") && !contains(&source(&half), "for [i (range 3)] :skip"),
        &format!("half a copy: the merge did not skip its argument: {}", source(&half)),
    );
    check(lamps(&half).len() == 5 && at(&half, "a", 0.).is_none() && at(&half, "b", 0.).is_some() && at(&half, "a", 1.).is_some(), "deleting one object of a copy took its sibling or another copy");
    check(snapshot(&open_text(&source(&half))) == snapshot(&half), "half a copy: the saved text is not the document");
    let ids_before: Vec<i64> = sorted_ids(&pairs).into_iter().filter(|i| *i != id_at(&pairs, "a", 0.)).collect();
    check(sorted_ids(&half) == ids_before, "half a copy: the other objects changed id");
    // then the other half: its iteration has nothing left, so the loop skips it
    let other = ok(write(Level::Scene, &half, &Edit::Delete(vec![id_at(&half, "b", 0.)])));
    check(lamps(&other).len() == 4 && contains(&source(&other), ":skip [0]"), &format!("the second half of a copy: {}", source(&other)));
    same_after_reload(&other, "both halves");
    // both at once, in another iteration
    let both = ok(write(Level::Scene, &pairs, &Edit::Delete(vec![id_at(&pairs, "a", 1.), id_at(&pairs, "b", 1.)])));
    check(lamps(&both).len() == 4 && contains(&source(&both), ":skip [1]") && !contains(&source(&both), "[[1 0]]"), &format!("both objects of a copy: {}", source(&both)));
    // the siblings bound by name in a let* body
    let named = open_text(
        r#"(workspace pairs
    (graph g :context sop (sop/box))
    (graph scene :context scene
      (scene/merge (scene/geometry (ref g) :name "body")
                   (for [i (range 3)]
                     (let* [a (scene/light :name "a" :translate [i 0 0])
                            b (scene/light :name "b" :translate [i 1 0])]
                       (scene/merge a b))))))"#,
    );
    let half = ok(write(Level::Scene, &named, &Edit::Delete(vec![id_at(&named, "b", 2.)])));
    check(
        lamps(&half).len() == 5 && at(&half, "b", 2.).is_none() && at(&half, "a", 2.).is_some() && contains(&source(&half), ":skip [[2 1]]"),
        &format!("a bound pair, one deleted: {}", source(&half)),
    );
    same_after_reload(&half, "bound pair half");
    // a merge that loses an argument renumbers its skip (here b, which it skipped, and d, which it skips, move up)
    let renum = open_text(
        r#"(workspace r (graph scene :context scene
    (let* [a (scene/light :name "a") b (scene/light :name "b") c (scene/light :name "c") d (scene/light :name "d")]
      (scene/merge a b c d :skip [1 3]))))"#,
    );
    check(lamps(&renum).len() == 2, "a merge :skip did not leave two lamps");
    let edit_text = |doc: &Doc, op: &Op| workspace_doc::to_text(&edit_ws(doc, op).unwrap()).unwrap();
    let after = edit_text(&renum, &Op::DeleteNodes { nodes: vec![vec!["scene".into(), "b".into()]] });
    check(contains(&after, ":skip [2]"), &format!("deleting an argument did not renumber the skip: {after}"));
    let after = edit_text(&renum, &Op::DeleteNodes { nodes: vec![vec!["scene".into(), "a".into()]] });
    check(contains(&after, ":skip [0 2]"), &format!("deleting an earlier argument did not shift the skip: {after}"));
}

// a loop inside a loop: the copies nest, the rules are the same, and a delete is exact at any depth
#[test]
fn nested_loops() {
    let text = r#"(workspace nest
    (graph g :context sop (sop/box))
    (graph scene :context scene
      (scene/merge (scene/geometry (ref g) :name "body")
                   (for [i (range 3)]
                     (scene/merge (for [j (range 2)] (scene/light :name "lamp" :intensity 30 :translate [i j 0])))))))"#;
    let doc = open_text(text);
    check(lamps(&doc).len() == 6, "the nested loop did not make six lamps");
    let pick = |n: usize| lamps(&doc)[n].id;
    let edited = ok(reconcile(None, &doc, &set_id(&doc, pick(3), &[float("intensity", 77.)])));
    check(
        lamps(&edited).iter().all(|i| field(i, "intensity") == fl(77.)) && contains(&source(&edited), ":intensity 77.0") && !contains(&source(&edited), ":intensity 30"),
        "a literal edit did not reach the inner template",
    );
    same_after_reload(&edited, "nested edit");
    let message = refused(reconcile(None, &doc, &set_id(&doc, pick(3), &[float("translate_x", 9.)])));
    check(contains(&message, "computed by the loop") && contains(&message, "(now i)"), &format!("a computed field of a nested copy was not refused with its expression: {message}"));
    let renamed = ok(reconcile(None, &doc, &with_scene(&doc, kg::update(&scene(&doc), pick(3), &|n| kg::Node { label: "spot".to_owned(), ..n }))));
    check(
        lamps(&renamed).iter().all(|i| i.label == "spot") && contains(&source(&renamed), ":name \"spot\"") && !contains(&source(&renamed), ":name \"lamp\""),
        &format!("a nested rename did not reach the template: {}", source(&renamed)),
    );
    same_after_reload(&renamed, "nested rename");
    let ij = |doc: &Doc| -> Vec<(Option<Value>, Option<Value>)> { lamps(doc).iter().map(|i| (field(i, "translate_x"), field(i, "translate_y"))).collect() };
    let cell = |i: f64, j: f64| (fl(i), fl(j));
    let all: [(i64, i64); 6] = [(0, 0), (0, 1), (1, 0), (1, 1), (2, 0), (2, 1)];
    let cells = |without: &[(i64, i64)]| -> Vec<(Option<Value>, Option<Value>)> { all.iter().filter(|c| !without.contains(c)).map(|(i, j)| cell(*i as f64, *j as f64)).collect() };
    // one inner copy goes alone: only that object, every other outer copy untouched
    for n in [0usize, 3, 5] {
        let (i, j) = all[n];
        let gone = ok(write(Level::Scene, &doc, &Edit::Delete(vec![pick(n)])));
        check(contains(&source(&gone), &format!(":skip [[{i} {j}]]")), &format!("nested delete {n}: {}", source(&gone)));
        check(ij(&gone) == cells(&[(i, j)]) && ij(&open_text(&source(&gone))) == cells(&[(i, j)]), &format!("deleting inner copy ({i},{j}) took another object"));
        check(sorted_ids(&gone) == sorted_ids(&doc).into_iter().filter(|x| *x != pick(n)).collect::<Vec<_>>(), "a nested delete changed the other ids");
        same_after_reload(&gone, "nested delete");
    }
    // repeated deletes accumulate in the inner loop's list
    let first = ok(write(Level::Scene, &doc, &Edit::Delete(vec![pick(3)])));
    let second = ok(write(Level::Scene, &first, &Edit::Delete(vec![lamps(&first)[0].id])));
    check(contains(&source(&second), ":skip [[0 0] [1 1]]") && lamps(&second).len() == 4, &format!("nested deletes did not accumulate: {}", source(&second)));
    same_after_reload(&second, "nested twice");
    // every object of one outer copy: the outer iteration goes
    let outer = ok(write(Level::Scene, &doc, &Edit::Delete(vec![pick(2), pick(3)])));
    check(
        ij(&outer) == cells(&[(1, 0), (1, 1)]) && contains(&source(&outer), ":skip [1]") && !contains(&source(&outer), "[[1"),
        &format!("an outer copy was not skipped: {}", source(&outer)),
    );
    same_after_reload(&outer, "outer copy");
    // three levels, several clauses in the innermost: the innermost loop that makes the object is the one that skips
    let deep = open_text(
        r#"(workspace deep
    (graph g :context sop (sop/box))
    (graph scene :context scene
      (scene/merge (scene/geometry (ref g) :name "body")
        (for [i (range 2)]
          (scene/merge
            (for [j (range 2)]
              (scene/merge
                (for [a (range 2) b (range 2)]
                  (scene/light :name "lamp" :translate [i j (+ (* 2 a) b)])))))))))"#,
    );
    check(lamps(&deep).len() == 16, "the three-level loop did not make sixteen lamps");
    let z = |doc: &Doc| -> Vec<(Option<Value>, Option<Value>, Option<Value>)> {
        lamps(doc).iter().map(|i| (field(i, "translate_x"), field(i, "translate_y"), field(i, "translate_z"))).collect()
    };
    let pick3 = |n: usize| lamps(&deep)[n].id;
    // lamp number 11 is i=1 j=0 a=1 b=1, the innermost iteration (1 0 3)
    let gone = ok(write(Level::Scene, &deep, &Edit::Delete(vec![pick3(11)])));
    check(
        contains(&source(&gone), ":skip [[1 0 3]]") && lamps(&gone).len() == 15 && z(&open_text(&source(&gone))) == z(&gone) && !z(&gone).contains(&z(&deep)[11]),
        &format!("a three-level delete: {}", source(&gone)),
    );
    check(sorted_ids(&gone) == sorted_ids(&deep).into_iter().filter(|x| *x != pick3(11)).collect::<Vec<_>>(), "a three-level delete changed the other ids");
    same_after_reload(&gone, "three-level delete");
    // all of (1 0): the middle iteration goes, not four inner ones
    let middle = ok(write(Level::Scene, &deep, &Edit::Delete([8usize, 9, 10, 11].iter().map(|n| pick3(*n)).collect())));
    check(
        lamps(&middle).len() == 12 && contains(&source(&middle), ":skip [[1 0]]") && !contains(&source(&middle), "[[1 0 "),
        &format!("a middle iteration was not skipped as one: {}", source(&middle)),
    );
    same_after_reload(&middle, "three-level middle");
    // an empty loop stays a loop: every object deleted
    let none = ok(write(Level::Scene, &doc, &Edit::Delete((0..6).map(pick).collect())));
    check(lamps(&none).is_empty() && lamps(&open_text(&source(&none))).is_empty(), "deleting every copy left objects");
    same_after_reload(&none, "all copies");
}

// ---- the root and the World as a merge member ----

fn diagnostic(text: &str) -> Option<(String, String)> {
    let options = Options { factories: Some(factories()), ..Options::default() };
    match lowering::of_workspace(&options, &ws_of_text(text)) {
        Err(d) => Some((d.code.clone(), d.message.clone())),
        Ok(_) => None,
    }
}

fn refused_code(code: &str, text: &str) -> String {
    match diagnostic(text) {
        Some((c, message)) => {
            check(c == code, &format!("expected {code}, got {c}: {message}"));
            message
        }
        None => panic!("expected {code}, the text was accepted"),
    }
}

const ROOT_TEXT: &str = r#"(workspace studio
  (graph g :context sop (sop/box))
  (graph sky :context world (world/sun (world/sky :name "sky") :name "sun"))
  (graph scene :context scene
    (let* [body (scene/geometry (ref g) :name "body")
           cam (scene/camera :name "cam")
           side (scene/camera :name "side" :eye [6 2 0])
           world (scene/world (ref sky) :name "Bloom" :exposure -0.5)
           all (scene/merge body cam side world)]
      (scene/root all :camera side :renderer "Path traced" :width 800 :height 600 :max_spp 64))))"#;

/// A text on one line: the printer breaks long calls.
fn flat(text: &str) -> String {
    text.replace('\n', " ").split(' ').filter(|p| !p.is_empty()).collect::<Vec<_>>().join(" ")
}

fn sketch(name: &str) -> String {
    std::fs::read_to_string(concat!(env!("CARGO_MANIFEST_DIR"), "/../../sketches/").to_owned() + name).expect("sketch")
}

#[test]
fn root_settings_and_camera() {
    use root::Renderer as R;
    // a scene without a root renders as the defaults; a camera carries no render size
    let old = open_text(TEXT);
    check(old.root == root::default() && old.homes.root.is_none(), "a scene without a root did not get the default root");
    // a root and a World member
    let doc = open_text(ROOT_TEXT);
    check(
        doc.root.renderer == R::PathTraced && doc.root.width == 800 && doc.root.height == 600 && doc.root.max_spp == 64 && doc.homes.root.is_some(),
        "the root's settings were not read",
    );
    check(doc.active_camera == Some(node_id(&doc, "side")), "the root's :camera is not the active camera");
    let wid = objects::ids("world", &scene(&doc))[0];
    check(
        doc.homes.objects.iter().any(|(i, _)| *i == wid) && doc.homes.world_graph.as_deref() == Some("sky") && doc.layers.contains_key(&wid) && doc.homes.layers.len() == 2,
        "scene/world did not read its world graph's layers",
    );
    check(kg::find(&scene(&doc), wid).unwrap().label == "Bloom", "the World lost its name");
    same_after_reload(&doc, "root and World");
    // a root edit round-trips Save, one undo entry's worth of text
    let edited = ok(write(Level::Scene, &doc, &Edit::Root(root::Parameters { width: 1024, bounces: 8, ..doc.root.clone() })));
    check(
        edited.root.width == 1024
            && edited.root.bounces == 8
            && contains(&flat(&source(&edited)), ":renderer \"Path traced\" :width 1024 :height 600 :max_spp 64 :bounces 8"),
        &format!("a root edit did not reach the text: {}", source(&edited)),
    );
    check(open_text(&source(&edited)).root == edited.root, "a root edit did not round-trip Save");
    // Raster -> Wireframe -> Raster (the default: the keyword goes) displays what a fresh lowering does
    let plain = open_text(
        r#"(workspace studio
  (graph g :context sop (sop/box))
  (graph scene :context scene
    (let* [body (scene/geometry (ref g) :name "body")]
      (scene/root body :width 800 :height 600 :max_spp 64))))"#,
    );
    let wire = ok(write(Level::Scene, &plain, &Edit::Root(root::Parameters { renderer: R::Wireframe, ..plain.root.clone() })));
    check(wire.root.renderer == R::Wireframe, "Wireframe was not shown");
    let back = ok(write(Level::Scene, &wire, &Edit::Root(root::Parameters { renderer: R::Raster, ..wire.root.clone() })));
    check(back.root.renderer == R::Raster && open_text(&source(&back)).root == back.root, &format!("Raster did not come back: {}", source(&back)));
    let again = ok(write(Level::Scene, &back, &Edit::Root(root::Parameters { renderer: R::Wireframe, ..back.root.clone() })));
    let again = ok(write(Level::Scene, &again, &Edit::Root(root::Parameters { renderer: R::Raster, ..again.root.clone() })));
    check(again.root.renderer == R::Raster && open_text(&source(&again)).root == again.root, &format!("Raster did not come back the second time: {}", source(&again)));
    // the printer keeps the camera, renderer, size, samples order
    let tidy = ok(write(Level::Scene, &edited, &Edit::Root(root::Parameters { round_samples: 2, ..edited.root.clone() })));
    check(contains(&flat(&source(&tidy)), ":bounces 8 :round_samples 2"), "the root printed its keywords out of order");
    // the render camera is written on the root
    let cam = wrote("root camera", Level::Scene, &doc, &Edit::Camera(Some(node_id(&doc, "cam"))), &Doc { active_camera: Some(node_id(&doc, "cam")), ..doc.clone() });
    check(
        contains(&flat(&source(&cam)), ":camera cam :renderer \"Path traced\" :width 800") && !contains(&source(&cam), ":camera side") && cam.active_camera == Some(node_id(&cam, "cam")),
        &format!("the camera was not written on the root: {}", source(&cam)),
    );
    // a part gets a default root; the first edit of a root setting writes one over its result
    let part = open_text(TEXT);
    let written = ok(write(Level::Scene, &part, &Edit::Root(root::Parameters { width: 640, renderer: R::Wireframe, ..part.root.clone() })));
    check(
        written.homes.root.is_some()
            && written.root.width == 640
            && contains(&source(&written), "(scene/root")
            && contains(&flat(&source(&written)), ":renderer \"Wireframe\" :width 640")
            && open_text(&source(&written)).root == written.root,
        &format!("the root was not written: {}", source(&written)),
    );
}

#[test]
fn root_of_saved_sketches() {
    use root::Renderer as R;
    // shattered_studio carries a root, written by the editor
    let studio = lower(None, &ws_of_text(&sketch("shattered_studio.rays")));
    check(studio.root.width == 1600 && studio.root.height == 1600 && studio.root.max_spp == 512 && studio.homes.root.is_some(), "shattered_studio (saved once by the editor) does not carry its render size on a scene/root");
    let sea = open_text(&sketch("gpu_sea.rays"));
    check(sea.homes.root.is_some(), "gpu_sea has a root home");
    let sea_w = ok(write(Level::Scene, &sea, &Edit::Root(root::Parameters { renderer: R::Wireframe, ..sea.root.clone() })));
    let sea_r = ok(write(Level::Scene, &sea_w, &Edit::Root(root::Parameters { renderer: R::Raster, ..sea_w.root.clone() })));
    check(sea_r.root.renderer == R::Raster, &format!("gpu_sea Raster: {}", source(&sea_r)));
}

#[test]
fn world_member_edits() {
    let doc = open_text(ROOT_TEXT);
    // the World is a merge member: its keywords edit like an object's, its layers its graph's
    let (wid, network) = world_network(&doc);
    let sun = layer_id(&doc, "sun");
    let sky = layer_id(&doc, "sky");
    let world_node = |graph: &kg::T, values: &[(String, Value)]| kg::set_fields(graph, wid, values).unwrap().0;
    let edited = wrote(
        "World member",
        Level::Scene,
        &doc,
        &Edit::Fields(vec![(wid, vec![float("exposure", 1.), float("rotation", 20.)])]),
        &with_scene(&doc, world_node(&scene(&doc), &[float("exposure", 1.), float("rotation", 20.)])),
    );
    check(
        contains(&flat(&source(&edited)), "(scene/world (ref sky) :name \"Bloom\" :exposure 1.0 :rotation 20.0)"),
        &format!("a World member's edit did not reach its call: {}", source(&edited)),
    );
    same_after_reload(&edited, "World member");
    let layer_edit = |doc: &Doc, id: i64, values: &[(String, Value)]| {
        let (wid, n) = world_network(doc);
        document::with_layers(doc, wid, kg::set_fields(&n, id, values).unwrap().0)
    };
    let edited = wrote("World member layer", Level::Inside(wid), &doc, &Edit::Fields(vec![(sun, vec![float("intensity", 321.)])]), &layer_edit(&doc, sun, &[float("intensity", 321.)]));
    check(contains(&source(&edited), ":intensity 321.0") && layer_id(&edited, "sun") == sun, "a layer of the World's graph was not edited");
    same_after_reload(&edited, "World member layer");
    // a deleted top layer hands the graph's result to the layer below; the stack reorders
    let edited = ok(write(Level::Inside(wid), &doc, &Edit::Delete(vec![sun])));
    check(
        !contains(&source(&edited), "world/sun") && contains(&source(&edited), "world/sky") && !objects::ids("world", &scene(&edited)).is_empty(),
        &format!("a deleted top layer: {}", source(&edited)),
    );
    same_after_reload(&edited, "World member layer delete");
    let swapped = kg::update(&network, sun, &|n| kg::Node { input: None, ..n });
    let swapped = kg::update(&swapped, sky, &|n| kg::Node { input: Some(sun), ..n });
    let swapped = document::with_layers(&doc, wid, swapped);
    let edited = wrote("World member restack", Level::Inside(wid), &doc, &Edit::Restack(vec![sun, sky]), &swapped);
    check(layer_labels(&edited) == strs(&["sky", "sun"]) && contains(&source(&edited), "(scene/world (ref sky)"), &format!("a restack: {}", source(&edited)));
    same_after_reload(&edited, "World member restack");
    {
        let world = scene_core::world::presets().into_iter().find(|(n, _)| n == "white room").unwrap().1;
        let network = layers::graph_of_world(&world).unwrap();
        let values = layers::of_world(&world).1;
        wrote(
            "World member preset",
            Level::Inside(wid),
            &doc,
            &Edit::Layers(network.clone(), values.clone()),
            &with_scene(&document::with_layers(&doc, wid, network), world_node(&scene(&doc), &values)),
        );
    }
    // deleting the World leaves its merge input, its binding and its graph (nothing else reads it)
    let worldless = document::prune(&with_scene(&doc, kg::remove(&[wid], &scene(&doc))));
    let edited = wrote("World member delete", Level::Scene, &doc, &Edit::Delete(vec![wid]), &worldless);
    check(
        objects::ids("world", &scene(&edited)).is_empty()
            && !contains(&source(&edited), "scene/world")
            && !contains(&source(&edited), "graph sky")
            && contains(&source(&edited), "scene/root"),
        &format!("a deleted World: {}", source(&edited)),
    );
    same_after_reload(&edited, "World member delete");
}

#[test]
fn root_world_camera_refusals() {
    refused_code(
        "E_SCENE_ROOT",
        r#"(workspace a
    (graph g :context sop (sop/box))
    (graph scene :context scene
      (scene/root (scene/merge (scene/root (scene/merge (scene/geometry (ref g))))))))"#,
    );
    refused_code(
        "E_SCENE_ROOT",
        r#"(workspace a
    (graph g :context sop (sop/box))
    (graph scene :context scene
      (scene/merge (scene/geometry (ref g)) (scene/root (scene/merge)))))"#,
    );
    let message = refused_code(
        "E_SCENE_WORLD",
        r#"(workspace a
    (graph g :context sop (sop/box))
    (graph sky :context world (world/sky))
    (graph scene :context scene
      (let* [one (scene/world (ref sky) :name "one")
             two (scene/world (ref sky) :name "two")]
        (scene/root (scene/merge (scene/geometry (ref g)) one two)))))"#,
    );
    check(contains(&message, "one") && contains(&message, "two"), &format!("E_SCENE_WORLD did not name both bindings: {message}"));
    refused_code(
        "E_SCENE_CAMERA",
        r#"(workspace a
    (graph g :context sop (sop/box))
    (graph scene :context scene
      (let* [elsewhere (scene/camera :name "away")]
        (scene/root (scene/merge (scene/geometry (ref g)) (scene/camera)) :camera elsewhere))))"#,
    );
}

// the ops of a gesture, applied together: all or none
fn apply_ops(doc: &Doc, ops: &[Op]) -> Result<Doc, String> {
    let mut doc = doc.clone();
    for op in ops {
        let workspace = workspace_doc::edit(&catalog(), &doc.workspace.ws, op).map_err(|d| rays_diagnostic::to_string(&d))?;
        let options = Options { factories: Some(factories()), previous: Some(doc.clone()), ..Options::default() };
        doc = lowering::of_workspace(&options, &workspace).map_err(|d| rays_diagnostic::to_string(&d))?;
    }
    Ok(doc)
}

fn graph_names(doc: &Doc) -> Vec<String> {
    doc.workspace.ws.checked.graphs.iter().map(|g| g.name.clone()).collect()
}

fn geometries(doc: &Doc) -> usize {
    objects::ids("geometry", &scene(doc)).len()
}

fn path(parts: &[&str]) -> flow_check::workspace::Path {
    parts.iter().map(|part| (*part).into()).collect()
}

#[test]
fn compose_add_geometry_and_world() {
    // the composition gestures: one op list each, one undo entry (the ops apply together)
    let doc = open_text(ROOT_TEXT);
    let added = apply_ops(&doc, &sync::add_geometry(&doc, None)).unwrap();
    check(
        geometries(&added) == geometries(&doc) + 1
            && graph_names(&added).contains(&"shape".to_owned())
            && contains(&flat(&source(&added)), "(scene/geometry (ref shape))")
            && contains(&flat(&source(&added)), "(scene/merge body cam side world object)"),
        &format!("add geometry: {}", source(&added)),
    );
    same_after_reload(&added, "add geometry");
    let again = apply_ops(&added, &sync::add_geometry(&added, Some("shape"))).unwrap();
    check(
        geometries(&again) == geometries(&added) + 1 && graph_names(&again) == graph_names(&added) && contains(&flat(&source(&again)), "(scene/geometry (ref shape) :name \"shape 2\")"),
        &format!("add geometry of an existing graph: {}", source(&again)),
    );
    // a failing step leaves nothing behind
    check(
        apply_ops(&doc, &[sync::add_geometry(&doc, None).remove(0), Op::RemoveGraph { name: "nope".to_owned() }]).is_err(),
        "a failed gesture was applied",
    );
    // one World only
    check(sync::add_world(&doc).is_err(), "a second World was offered");
    let bare = open_text(
        r#"(workspace a (graph g :context sop (sop/box))
    (graph scene :context scene (scene/root (scene/merge (scene/geometry (ref g))))))"#,
    );
    let worlded = apply_ops(&bare, &sync::add_world(&bare).unwrap()).unwrap();
    check(
        !objects::ids("world", &scene(&worlded)).is_empty()
            && contains(&flat(&source(&worlded)), "(scene/world (ref sky))")
            && contains(&flat(&source(&worlded)), "(graph sky :context world")
            && contains(&flat(&source(&worlded)), "(world/sun (world/sky))"),
        &format!("add World: {}", source(&worlded)),
    );
    same_after_reload(&worlded, "add World");
    match apply_ops(
        &worlded,
        &[Op::AddNode { scope: path(&["scene"]), name: "again".to_owned(), expr: call("scene/world", vec![call("ref", vec![sym("sky")])]) }],
    ) {
        Err(message) => check(contains(&message, "E_SCENE_WORLD") || contains(&message, "World"), &format!("one-World add: {message}")),
        Ok(_) => panic!("a second World was added to one root"),
    }
    let delete_world = |doc: &Doc| ok(write(Level::Scene, doc, &Edit::Delete(objects::ids("world", &scene(doc)))));
    let deleted = delete_world(&worlded);
    check(
        objects::ids("world", &scene(&deleted)).is_empty() && !graph_names(&deleted).contains(&"sky".to_owned()) && !contains(&source(&deleted), "world/none"),
        "deleting a World did not remove its orphan graph",
    );
    same_after_reload(&deleted, "delete orphan World");
    let saved = s::parse("(graph saved :context world (ref sky))").unwrap().remove(0);
    let shared = apply_ops(&worlded, &[Op::SetGraph { name: "saved".to_owned(), form: saved }]).unwrap();
    let sky_text = |doc: &Doc| {
        let graph = doc.workspace.ws.checked.graphs.iter().find(|g| g.name == "sky").unwrap();
        flow_syntax::lisp::print(&[graph.form.clone()]).unwrap().0
    };
    let retained = delete_world(&shared);
    check(
        objects::ids("world", &scene(&retained)).is_empty()
            && graph_names(&retained).contains(&"saved".to_owned())
            && sky_text(&retained) == sky_text(&shared)
            && !contains(&source(&retained), "world/none"),
        "deleting a World changed the graph another reference still uses",
    );
    same_after_reload(&retained, "delete shared World");
}

#[test]
fn compose_delete_refcount_and_group() {
    // take a wire out of the merge: the object stays as an unwired binding, and wires back
    let rock = open_text(
        r#"(workspace rocks
    (graph rock :context sop (sop/box))
    (graph scatter :context sop (sop/box))
    (graph scene :context scene
      (let* [left (scene/geometry (ref rock))
             right (scene/geometry (ref rock) :translate [3 0 0] :name "right")
             pebbles (scene/geometry (ref scatter))
             all (scene/merge left right pebbles)]
        (scene/root all))))"#,
    );
    let out = apply_ops(&rock, &[Op::Disconnect { node: path(&["scene", "all"]), key: ArgKey::Pos(2), fallback: None }]).unwrap();
    check(
        geometries(&out) == 2 && contains(&flat(&source(&out)), "pebbles (scene/geometry (ref scatter))") && contains(&flat(&source(&out)), "(scene/merge left right)"),
        &format!("take out of the scene: {}", source(&out)),
    );
    let back = apply_ops(&out, &[Op::Connect { node: path(&["scene", "all"]), key: ArgKey::Pos(2), src: "pebbles".to_owned(), iter: false }]).unwrap();
    check(geometries(&back) == 3, "a wire taken out did not wire back");
    // delete an object: its SOP graph goes only when nothing else references it
    let remove = |doc: &Doc, label: &str| ok(write(Level::Scene, doc, &Edit::Delete(vec![node_id(doc, label)])));
    let no_pebbles = remove(&rock, "scatter");
    check(
        !graph_names(&no_pebbles).contains(&"scatter".to_owned()) && graph_names(&no_pebbles).contains(&"rock".to_owned()) && !contains(&source(&no_pebbles), "pebbles"),
        &format!("delete frees an unshared graph: {}", source(&no_pebbles)),
    );
    same_after_reload(&no_pebbles, "delete object");
    let no_left = remove(&rock, "rock");
    check(graph_names(&no_left).contains(&"rock".to_owned()) && geometries(&no_left) == 2, "deleting one of two objects of a graph removed the graph");
    let no_right = remove(&no_left, "right");
    check(
        !graph_names(&no_right).contains(&"rock".to_owned()) && graph_names(&no_right).contains(&"scatter".to_owned()),
        &format!("deleting the last object of a graph kept it: {}", source(&no_right)),
    );
    // group into a new merge: between the selection and the old one
    let grouped = apply_ops(&rock, &[Op::GroupMerge { nodes: vec![path(&["scene", "left"]), path(&["scene", "right"])], name: "pair".to_owned() }]).unwrap();
    check(
        contains(&flat(&source(&grouped)), "pair (scene/merge left right)") && contains(&flat(&source(&grouped)), "all (scene/merge pair pebbles)") && geometries(&grouped) == 3,
        &format!("group: {}", source(&grouped)),
    );
    same_after_reload(&grouped, "group");
}

#[test]
fn compose_world_member_layer_edits_are_surgical() {
    // E9: deleting or moving one layer of a World member is an edit of that binding: the graph's
    // input, its comment, the expression and the other names stay
    let skies = open_text(
        r#"(workspace skies
    (graph g :context sop (sop/box))
    (graph sky :context world [(warmth : float 0.5)]
      ;; the stack, bottom first
      (let* [base (world/sky :name "base")
             haze (world/gradient base :name "haze")
             sun (world/sun haze :name "sun" :intensity (* warmth 4))]
        sun))
    (graph scene :context scene (scene/root (scene/merge (scene/geometry (ref g)) (scene/world (ref sky))))))"#,
    );
    let kept = |what: &str, doc: &Doc| {
        let text = flat(&source(doc));
        check(
            contains(&text, "[(warmth : float 0.5)]") && contains(&text, "; the stack, bottom first") && contains(&text, ":intensity (* warmth 4)") && contains(&text, "base (world/sky"),
            &format!("{what} rewrote the world graph: {}", source(doc)),
        );
    };
    let (wid, network) = world_network(&skies);
    let haze = layer_id(&skies, "haze");
    let sun = layer_id(&skies, "sun");
    let base = layer_id(&skies, "base");
    let edited = ok(write(Level::Inside(wid), &skies, &Edit::Delete(vec![haze])));
    kept("deleting a layer", &edited);
    check(!contains(&source(&edited), "haze") && contains(&flat(&source(&edited)), "(world/sun base"), &format!("a deleted layer of a World member: {}", source(&edited)));
    same_after_reload(&edited, "member layer delete");
    let swapped = kg::update(&network, haze, &|n| kg::Node { input: None, ..n });
    let swapped = kg::update(&swapped, sun, &|n| kg::Node { input: None, ..n });
    let swapped = kg::update(&swapped, haze, &|n| kg::Node { input: Some(sun), ..n });
    let swapped = kg::update(&swapped, sun, &|n| kg::Node { input: Some(base), ..n });
    let swapped = document::with_layers(&skies, wid, swapped);
    let edited = wrote("member layer move", Level::Inside(wid), &skies, &Edit::Restack(vec![base, sun, haze]), &swapped);
    kept("moving a layer", &edited);
    check(layer_labels(&edited) == strs(&["haze", "sun", "base"]), &format!("a moved layer of a World member: {}", source(&edited)));
    same_after_reload(&edited, "member layer move");
    // the first edit of a render setting writes a root beside a binding already named root
    let rooted = open_text(
        r#"(workspace r (graph g :context sop (sop/box))
    (graph scene :context scene (let* [root (scene/geometry (ref g))] (scene/merge root))))"#,
    );
    let written = ok(write(Level::Scene, &rooted, &Edit::Root(root::Parameters { width: 640, ..rooted.root.clone() })));
    check(
        written.homes.root.is_some() && written.root.width == 640 && contains(&flat(&source(&written)), "(scene/root (scene/merge root) :width 640)"),
        &format!("a scene that binds root refused its first render setting: {}", source(&written)),
    );
    // a rename to a name another object has is refused: [:parent] reads names
    let family = open_text(
        r#"(workspace f
    (graph scene :context scene
      (scene/merge (scene/light :name "a") (scene/light :name "b") (scene/light :name "c" :parent "b"))))"#,
    );
    let a = node_id(&family, "a");
    match reconcile(None, &family, &with_scene(&family, kg::update(&scene(&family), a, &|n| kg::Node { label: "b".to_owned(), ..n }))) {
        Err(message) => check(contains(&message, "named"), &message),
        Ok(_) => panic!("a rename to another object's name was written"),
    }
    // an object the host made is still its graph's after another graph's edit lowered the text again
    let pair = open_text("(workspace pair (graph g :context sop (sop/box)) (graph h :context sop (sop/box)))");
    let relowered = apply_ops(
        &pair,
        &[Op::SetArg { node: path(&["h", "@result"]), key: ArgKey::Kw("uniform_scale".to_owned()), sub: vec![], value: mk(s::Draft::Num("2.0".to_owned())) }],
    )
    .unwrap();
    let moved = ok(reconcile(None, &relowered, &set(&relowered, "g", &[float("translate_x", 1.)])));
    check(
        contains(&flat(&source(&moved)), "(scene/geometry (ref g) :translate [1.0 0.0 0.0])") && contains(&source(&moved), "(scene/geometry (ref h))"),
        &format!("a host object lost its graph after a re-lowering: {}", source(&moved)),
    );
}

// Several scene graphs, each with a root of its own: a viewport renders as the root of the scene it
// shows, with that scene's World
const INSTANCES_TEXT: &str = r#"(workspace days
  (graph g :context sop (sop/box))
  (graph noon :context world (world/sun (world/sky :name "noon") :name "sun"))
  (graph dusk :context world (world/sun (world/sky :name "dusk") :name "sun"))
  (graph set :context scene
    (scene/merge (scene/geometry (ref g) :name "body") (scene/camera :name "hero")))
  (graph day :context scene
    (let* [cam (scene/camera :name "noon-cam" :eye [0 1 8])
           all (scene/merge (ref set) cam (scene/world (ref noon) :name "Noon"))]
      (scene/root all :camera cam :renderer "Raster" :width 800 :height 450)))
  (graph night :context scene
    (let* [cam (scene/camera :name "dusk-cam" :eye [4 2 6] :fov 30)
           all (scene/merge (ref set) cam (scene/world (ref dusk) :name "Dusk" :exposure -2))]
      (scene/root all :camera cam :renderer "Path traced" :max_spp 1024 :bounces 12
                  :width 1600 :height 900)))
  (graph editor :context editor
    (ui/workspace (ui/split_at "horizontal" 0.5 (ui/viewport (ref day)) (ui/viewport (ref night))))))"#;

#[test]
fn instances_each_viewport_has_its_root_and_world() {
    use root::Renderer as R;
    let doc = open_text(INSTANCES_TEXT);
    let shell = doc.shell.as_ref().expect("shell");
    check(shell.preview_sources.len() == 2, "two viewports, two scene instances");
    let roots: Vec<Option<document::ViewRoot>> = shell.preview_sources.iter().map(|(_, src)| lowering::instance_root(&src.instance)).collect();
    // each instance's root, its size and its own camera object: the two differ
    match roots.as_slice() {
        [Some(day), Some(night)] => {
            check(day.params.renderer == R::Raster, "day renders raster");
            check(
                night.params.renderer == R::PathTraced && night.params.max_spp == 1024 && night.params.bounces == 12,
                &format!("night's root was not read: {} spp, {} bounces", night.params.max_spp, night.params.bounces),
            );
            check((day.params.width, day.params.height) == (800, 450) && (night.params.width, night.params.height) == (1600, 900), "each root's own resolution");
            match (&day.camera, &night.camera) {
                (Some(d), Some(n)) => {
                    check(scene_core::camera::position(d) == rays_math::vec3::create(0., 1., 8.), "day looks from its own camera");
                    check(scene_core::camera::position(n) == rays_math::vec3::create(4., 2., 6.), "night looks from its own camera");
                    check(scene_core::camera::projection(n) != scene_core::camera::projection(d), "and with its own lens");
                }
                _ => panic!("each root names its camera"),
            }
        }
        _ => panic!("each viewport's scene instance names its root"),
    }
    // the document stores them per viewport, beside the Worlds
    match doc.view_roots.as_slice() {
        [(_, day), (_, night)] => check(
            day.camera.is_some() && night.camera.is_some() && day.params.width == 800 && night.params.width == 1600,
            "the document holds each viewport's root and camera",
        ),
        _ => panic!("each viewport over a root has an entry in the document"),
    }
    check(lowering::instance_root(&flow_syntax::value::struct_value("scene/merge", flow_syntax::ty::scene(), vec![].into())).is_none(), "a part has no root of its own");
    // each viewport draws its own World: the document's scene (a part) has none, the two roots do
    check(objects::ids("world", &scene(&doc)).is_empty(), "the part holds no World");
    match doc.view_worlds.as_slice() {
        [(_, Some(day)), (_, Some(night))] => {
            check(day.node.label == "Noon" && night.node.label == "Dusk", "each viewport's World node is its own");
            let dw = layers::to_world(None, &day.node, &day.layers);
            let nw = layers::to_world(None, &night.node, &night.layers);
            check(dw.is_some() && nw.is_some(), "both Worlds bake");
            check(dw != nw, "and they differ (exposure, layers)");
        }
        _ => panic!("each viewport over a root names its World"),
    }
    // objects are plain data: an object parented under its own child is refused, in the derived
    // scene and in the text
    let a = kg::instance(None, "a", &objects::geometry::kind());
    let b = kg::Node { input: Some(a.id), ..kg::instance(None, "b", &objects::geometry::kind()) };
    check(
        objects::reparent(&[a.clone(), b.clone()], &[a.id], Some(b.id)).iter().map(|n| n.input).collect::<Vec<_>>() == vec![None, Some(a.id)],
        "a reparent under a child made a cycle",
    );
    let options = Options { factories: Some(factories()), ..Options::default() };
    check(
        lowering::of_workspace(
            &options,
            &ws_of_text(
                r#"(workspace c
    (graph g :context sop (sop/box))
    (graph scene :context scene (scene/merge (scene/geometry (ref g) :name "a" :parent "b")
      (scene/geometry (ref g) :name "b" :parent "a"))))"#,
            ),
        )
        .is_err(),
        "two objects parented under each other lowered",
    );
}

#[test]
fn scene_angles_are_radians_in_schema_and_runtime() {
    let angle = std::f64::consts::FRAC_PI_2;
    let doc = open_text(&format!("(workspace radians (graph geometry :context sop (sop/box)) (graph scene :context scene (scene/merge (scene/geometry (ref geometry) :name \"body\" :rotate [0 0 {angle}]) (scene/camera :name \"camera\" :fov 1.0))))"));
    let graph = scene(&doc);
    let geometry = graph.iter().find(|node| node.label == "body").unwrap();
    assert_eq!(objects::geometry(geometry).unwrap().rotate_z, angle);
    let fields = kg::fields(geometry);
    assert_eq!(fields.iter().find(|field| field.name == "rotate_z").unwrap().unit.as_deref(), Some("rad"));
    let camera = graph.iter().find(|node| node.label == "camera").unwrap();
    let fields = kg::fields(camera);
    let fov = fields.iter().find(|field| field.name == "fov").unwrap();
    assert_eq!(fov.current, Value::Float(1.0));
    assert_eq!(fov.unit.as_deref(), Some("rad"));
}
