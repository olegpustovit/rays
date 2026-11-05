//! The editor-driven end of lib/rays_editor/test/test_scene_sync.ml: a workspace with no scene graph gets one when
//! an object is added through `Editor::edit`. (The rest of the file is Document/Scene_sync work, ported in
//! editor_document.)
use crate::t5c::*;
use flow_graph::flow_edit as fe;
use flow_syntax::syntax::{Draft, SyntaxBuilder};
use rays_editor::Editor;

fn check(c: bool, m: &str) {
    if !c {
        panic!("{m}");
    }
}

pub fn add_by_key() {
    let ws = load("(workspace bare (graph g :context sop (sop/box)))");
    let env = Editor::create(Editor::Create { platform: Some(input::Platform::Apple), ui_text_size: Some(11), ..Default::default() }, &ws, None, Editor::prepare_fn(), Editor::draw_fn()).unwrap_or_else(|m| panic!("{m}"));
    let mut builder = SyntaxBuilder::new();
    let head = builder.push_draft(0, Draft::Sym("scene/light".into()), Default::default(), None);
    let id = builder.push_draft(0, Draft::List(vec![head]), Default::default(), None);
    let env = Editor::edit(env, &fe::Op::AddNode { scope: vec!["scene".into()], name: "lamp".into(), expr: builder.freeze().handle(id) }).unwrap_or_else(|m| panic!("{m}"));
    let saved = editor_document::workspace_doc::to_text(&Editor::workspace(&env)).unwrap();
    check(saved.contains("graph scene") && saved.contains("scene/geometry (ref g)") && saved.contains("scene/camera") && saved.contains("lamp"), &format!("adding to a missing scene graph did not write it: {saved}"));
    check(Editor::undo_label(&env).as_deref() == Some("Add node"), "adding an object is not one entry");
    Editor::close(&env);
}
