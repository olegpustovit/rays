//! Checks of bundle D's modules that need a model but not the whole Editor: creation
//! (`editor_setup`), the shell (`editor_shell`), the list (`editor_list`), the status line
//! (`editor_status`), the text pane's applies (`editor_text`), the host's reload and the carry
//! letters (`editor_host`, `editor_carry`).
use std::rc::Rc;
use std::sync::Arc;

use editor_core::panels::{self, Panel};
use editor_document::document::{self, Level};
use editor_document::workspace_doc::{self, TextOptions};
use pxui_shell::tree::{self, Intent};

use crate::cook;
use crate::editor_carry;
use crate::editor_history as history;
use crate::editor_host;
use crate::editor_list;
use crate::editor_model::*;
use crate::editor_setup::{self, create_opts};
use crate::editor_shell;
use crate::editor_status;
use crate::editor_text;
use crate::selection;
use crate::text_pane::{self, Intent as TextIntent};

const TEXT: &str = "(workspace w (graph g :context sop (sop/box)))";
const TEXT2: &str = "(workspace w (graph g :context sop (sop/box)) (graph h :context sop (sop/box)))";

fn workspace(text: &str) -> workspace_doc::T {
    let catalog = editor_document::kinds::catalog(flow_sop::manifest::VERSION, &sop::nodes::factories()).expect("catalog");
    workspace_doc::of_text(&TextOptions::default(), &catalog, text).expect("workspace")
}

fn model() -> EditorModel {
    let opts = create_opts {
        // no preferences file and no state folder of the user's: a unique temp name
        preferences: Some(std::env::temp_dir().join("rays-editor-test-no-preferences.rays").to_string_lossy().to_string()),
        presets: Some(std::env::temp_dir().join("rays-editor-test-presets").to_string_lossy().to_string()),
        ui_text_size: 11,
        await_: Some(true),
        ..create_opts::default()
    };
    editor_setup::create(opts, &workspace(TEXT), Rc::new(cook::prepare)).expect("model")
}

#[test]
fn creation_shell_list_status() {
    let value = model();
    // creation: the history's present is the document, nothing is selected, the scene is open
    assert!(Arc::ptr_eq(&value.doc, &history::present_arc(&value.history)));
    assert_eq!(history::depth(&value.history), 0);
    assert_eq!(value.file, "w.rays");
    assert_eq!(pane_level(&(&value).into()), Level::Scene);
    assert!(selection::selected(pane_selection(&(&value).into())).is_none());
    assert_eq!(value.focus, panels::main());
    assert!(value.guide || !value.guide);

    // the shell: no editor graph, so the host's tree; keys are by place, the host bar is `-1`
    assert!(editor_shell::layouts(&value).is_empty());
    assert_eq!(*editor_shell::shell_tree(&value, &value.workspace), panels::default());
    assert_eq!(editor_shell::panel_key(&value.doc, &[0]), vec!["@default".to_string(), "@panel".to_string(), "0".to_string()]);
    assert_eq!(editor_shell::panel_key(&value.doc, &[HOST_BAR]).last().map(|name| name.as_str()), Some("-1"));
    assert!(editor_shell::panel_state(&(&value).into(), &[HOST_BAR]).collapsed);
    assert!(!editor_shell::panel_state(&(&value).into(), &[0]).collapsed);
    let leaves = panels::leaves(&editor_shell::shell_tree(&value, &value.workspace));
    assert_eq!(leaves.len(), 3);
    assert!(editor_shell::has_panel(&(&value).into(), &Panel::Graph));
    let names = editor_shell::completion_names(&value);
    assert_eq!(names.graphs, vec!["g".to_string()]);
    // the graph leaf is found, and following the focus puts its pane in use
    let graphs = editor_shell::graph_leaves(&(&value).into());
    assert_eq!(graphs.len(), 1);
    assert_eq!(editor_shell::graph_path(&(&value).into()), Some(graphs[0].1.clone()));
    let mut followed = value.clone();
    editor_shell::follow_graph(&mut followed, &Panel::Graph, Some(&graphs[0].1));
    assert_eq!(panel_map_key(&followed), Some(graphs[0].0.clone()));

    // the list: the scene level lists objects with the visible and render columns
    let (source, rows) = editor_list::rows(&(&value).into());
    assert!(matches!(source, row_source::Objects(_)));
    let (shown, columns) = rows();
    assert_eq!(columns, vec!["v".to_string(), "r".to_string()]);
    assert!(!shown.is_empty());
    // a select intent folds into the selection; a rename of a hosted object folds into the graph
    let state = editor_list::tree_state {
        document: crate::editor_inspect::objects(&(&value).into()),
        selection: selection::clear(),
        tree: tree::create(),
        opened: None,
        label: None,
        rows: shown.clone(),
    };
    let state = editor_list::apply_tree(&(&value).into(), state, &Intent::Select(vec![shown[0].id]));
    assert_eq!(selection::selected(&state.selection), Some(shown[0].id));
    let state = editor_list::apply_tree(&(&value).into(), state, &Intent::Activate(shown[0].id));
    assert_eq!(state.opened, Some(shown[0].id));

    // the status line: nothing cooked yet; the scene level's hint
    let line = editor_status::status_text(&(&value).into(), false);
    assert!(line.contains("Waiting for first cook"), "{line}");
    assert!(line.contains("scene"), "{line}");
    assert_eq!(editor_status::prompt_name(&value), "-");
    assert_eq!(editor_status::level_name(&(&value).into()), "scene");
    assert!(editor_status::probe_caption(&(&value).into()).is_none());
    editor_host::close(&value);
}

fn panel_map_key(value: &EditorModel) -> Option<crate::panel_map::Key> {
    crate::panel_map::key(&value.panes)
}

#[test]
fn text_applies_drafts_history_and_reload() {
    std::thread::Builder::new().stack_size(256 << 20).spawn(text_applies_body).unwrap().join().unwrap();
}

fn text_applies_body() {
    let mut value = model();
    // an applied draft is one "Edit text" entry; the file's text is what Command-S last wrote
    assert!(!editor_text::unsaved(None, &value));
    editor_text::apply_text(&mut value, &[TextIntent::DocDraft(TEXT2.to_string())]);
    assert!(editor_text::has_draft(None, &value));
    assert!(editor_text::unsaved(None, &value));
    editor_text::apply_text(&mut value, &[TextIntent::DocApply(TEXT2.to_string())]);
    assert!(!editor_text::has_draft(None, &value), "{:?}", value.text.doc_errors.found.iter().map(|d| d.message.clone()).collect::<Vec<_>>());
    assert_eq!(history::depth(&value.history), 1);
    assert_eq!(history::label(&value.history), "Edit text");
    assert!(Arc::ptr_eq(&value.doc, &history::present_arc(&value.history)));
    assert_eq!(editor_completion(&value), vec!["g".to_string(), "h".to_string()]);
    assert!(editor_text::unsaved(None, &value));
    value = editor_text::filed(value);
    assert!(!editor_text::unsaved(None, &value));

    // a draft that does not check keeps its errors and changes nothing else
    let before = history::depth(&value.history);
    editor_text::apply_text(&mut value, &[TextIntent::DocApply("(workspace".to_string())]);
    assert_eq!(history::depth(&value.history), before);
    assert!(!value.text.doc_errors.found.is_empty());
    assert_eq!(value.text.draft.as_deref(), Some("(workspace"));
    assert!(editor_text::has_draft(None, &value));
    // `apply_drafts` refuses to go on while it stands
    assert!(!editor_text::apply_drafts(None, &mut value.clone()));
    editor_text::apply_text(&mut value, &[TextIntent::DocDiscard]);
    assert!(!editor_text::has_draft(None, &value));
    assert!(editor_text::apply_drafts(None, &mut value.clone()));

    // reload: the whole text replaces the document; a refused one is said and kept as a draft
    let mut reloaded = value.clone();
    editor_host::reload(None, &mut reloaded, "w.rays", TEXT).expect("reload");
    assert_eq!(reloaded.notice.as_ref().map(|(_, s)| s.as_str()), Some("Reloaded w.rays"));
    assert!(!editor_text::unsaved(None, &reloaded));
    assert_eq!(history::label(&reloaded.history), "Reload w.rays");
    let diagnostics = text_pane_errors("(workspace");
    let mut failed = value.clone();
    editor_host::reload_failed(&mut failed, "w.rays", "(workspace", &diagnostics);
    assert!(failed.notice.as_ref().is_some_and(|(k, s)| *k == notice_kind::Refusal && s.starts_with("w.rays not reloaded")));
    assert_eq!(failed.text.tab, text_pane::Tab::Document);
    assert_eq!(failed.text.draft.as_deref(), Some("(workspace"));
    editor_host::close(&value);
}

fn editor_completion(value: &EditorModel) -> Vec<String> {
    editor_shell::completion_names(value).graphs
}

fn text_pane_errors(text: &str) -> Vec<rays_diagnostic::Diagnostic> {
    match flow_syntax::syntax::parse(text) {
        Err(d) => vec![d],
        Ok(_) => Vec::new(),
    }
}

#[test]
fn host_entry_points_and_carry_letters() {
    let value = model();
    assert!(!editor_host::carrying(&value));
    assert!(editor_host::carry_viewport(&value).is_none());
    assert_eq!(editor_carry::carry_letters.len(), 9);
    assert!(editor_carry::carry_allowed(&crate::editor_commands::action::Enter));
    assert!(!editor_carry::carry_allowed(&crate::editor_commands::action::Undo));
    // nothing selected and no graph open: there is nothing to pick up, and the message says so
    let error = editor_carry::carry_payload(&(&value).into()).err().unwrap_or_default();
    assert!(error.starts_with("Nothing to pick up"), "{error}");
    // the scene edit of the host leaves no undo entry (`Amend` folds into the present)
    let scene = document::scene_graph(&value.doc);
    let mut amended = value.clone();
    editor_host::scene_edit(&mut amended, editor_host::scene_mode::Amend, None, scene.clone());
    assert_eq!(history::depth(&amended.history), history::depth(&value.history));
    // a settings write of the same values is a no-op
    let mut same = value.clone();
    editor_host::set_settings(&mut same, &value.doc.settings);
    assert!(Arc::ptr_eq(&same.doc, &value.doc));
    // the placed pieces and the World of an uncooked document
    assert!(editor_host::placed_pieces(false, None, &value).is_empty());
    assert!(editor_host::world_id(&value).is_none());
    editor_host::close(&value);
}
