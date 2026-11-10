//! The model retains Arc<Document> snapshots and frames borrow ModelView. Ui is shared through Rc<RefCell>. Vocabulary is lazy, inspector results are sketch-owned, and level values use an association list because Document::level has no Ord.

use std::any::Any;
use std::cell::{OnceCell, RefCell};
use std::collections::BTreeMap;
use std::rc::Rc;
use std::sync::Arc;

use editor_core::panels::{self, Panel};
use editor_core::router;
use editor_document::document::{self, Level, Snapshot, T as Document};
use editor_document::kind_graph as kg;
use editor_document::layout_by_path::PathMap;
use editor_document::layout_edit;
use editor_document::settings;
use editor_document::workspace_doc;
use flow_check::catalog::Catalog;
use flow_check::workspace as w;
use flow_graph::flow_edit::{ArgKey, MacroDraft, Op};
use flow_graph::probe::{self, Probe};
use flow_graph::projection as fprojection;
use flow_native::probe_run;
use flow_native::workspace_gpu;
use flow_sop::network as net;
use flow_syntax::phase_timer::PhaseTimer;
use flow_syntax::syntax::Syntax;
use flow_syntax::ty::Ty;
use input::{Frame as Input, Key};
use param::{Effects, FieldView, Value};
use pxui::ui::Ui;
use rays_graph::node_menu;
use rays_graph::scope_state as scope;
use rays_shell::prompt as shell_prompt;
use rays_shell::{layout, timeline_bar, tree};
use rays::frame;
use sop::edit_graph::Factory;

use crate::carry as carry_mod;
use crate::cook;
use crate::editor_commands;
use crate::editor_history as history;
use crate::lisp_text;
use crate::navigator;
use crate::panel_map;
use crate::pick;
use crate::selection;
use crate::text_pane;
use crate::timeline;

pub type Bounds = cook::Bounds;

/// The panels' text: 13 points, the size of macOS control text (11 is its small size, and the kit
/// face is narrow).  A core's `text_size` is its default (tests pass 11, the geometry the kit was
/// drawn with).  Command +/-/0 step from it and return to it.
pub const DEFAULT_TEXT_SIZE: i64 = 13;

/// How the graph pane shows the open network.
/// What a message says: information (a dot in its echo tip) or a refusal (the error ink).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NoticeKind {
    Info,
    Refusal,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Projection {
    GraphView,
    ListView,
    TextView,
}
pub use Projection::*;

/// Level-keyed values use an association list.
pub type LevelMap = Vec<(Level, Projection)>;

pub fn level_map_empty() -> LevelMap {
    Vec::new()
}

pub fn level_map_singleton(level: Level, view: Projection) -> LevelMap {
    vec![(level, view)]
}

pub fn level_map_find(map: &LevelMap, level: Level) -> Option<Projection> {
    map.iter().find(|(l, _)| *l == level).map(|(_, v)| *v)
}

pub fn level_map_add(mut map: LevelMap, level: Level, view: Projection) -> LevelMap {
    map.retain(|(l, _)| *l != level);
    map.push((level, view));
    map
}

#[derive(Clone, Debug, PartialEq)]
pub struct Preview {
    pub path: w::Path,
    pub chain: Vec<w::Path>,
}

/// What the workspace pane was last laid out from: the document, the probes and the graph, the
/// recording evaluation of the checked source, and what its footers read (the cook's geometry
/// counts, the time of a live document).
#[derive(Clone)]
pub struct ScopeKey {
    pub ws: workspace_doc::T,
    pub probe_map: PathMap<i64>,
    pub graph: String,
    pub evaluated: Option<Arc<flow_eval::Eval>>,
    pub summaries: Vec<cook::Summary>,
    pub time: Option<Input>,
    pub executions: probe_run::Reports,
    pub records: Option<Rc<RefCell<Probe>>>,
    pub force: probe::Force,
    pub scope: Rc<fprojection::Scope>,
    /// every geometry node with its (object, compiled node): what a footer could ask the cook to count
    pub targets: Vec<(w::Path, (i64, i64))>,
    /// when the probes were last forced
    pub stamp: f64,
    /// the quiet time a moving clock alone must leave after that: a few frames of the cost
    pub gap: f64,
    /// the last frame forced them, so the time since `stamp` was that frame
    pub fresh_last: bool,
}

/// What a graph panel keeps of its own while another one has the focus.
#[derive(Clone)]
pub struct GraphPane {
    pub view: scope::Pane,
    pub laid: Option<Rc<ScopeKey>>,
    pub shown: Option<String>,
    /// level and pane graph, latest first
    pub route: Vec<(Level, Option<String>)>,
    pub views: LevelMap,
    pub at: Level,
    /// its navigation level and that level's selection
    pub picked: selection::Selection,
}

/// What a panel keeps of its own besides its PXUI state while another panel of its kind is the one
/// in use: its list, its text pane, its outline (flow.md 11.11).
#[derive(Clone)]
pub struct Local {
    pub rows: tree::Tree,
    pub code: text_pane::State,
    pub nav: navigator::State,
    pub sheet_owner: i64,
}

/// The start keywords the editor last followed, by panel key.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Started {
    pub on: Option<panel_map::Key>,
    pub views: Vec<(panel_map::Key, String)>,
    pub tabs: Vec<(panel_map::Key, String)>,
}

#[derive(Clone)]
pub enum Prompt {
    Keys,
    Saving(String),
    /// command search query
    Palette(String),
    /// / j: graph search query
    Jumping(String),
    /// Command-P: binding search query
    Symbols(String),
    /// Shift-F12: the symbol, and the line search query
    Referencing(String, String),
    /// Command-L: line[:column]
    Going(String),
    Browsing { query: String, presets: Vec<(String, f64)>, last_state: Option<f64> },
    /// x or n: the menu of panel kinds at the pointer
    Picking { split: bool, at: (f64, f64) },
    /// x after a kind: the split follows the pointer until a click places it; Tab turns it
    Splitting { panel: Panel, axis: panels::Axis },
    /// the make-macro dialog
    MakingMacro { nodes: Vec<w::Path>, draft: MacroDraft, state: shell_prompt::Macro },
}

#[derive(Clone)]
pub enum PromptIntent {
    SavePresetFile(String),
    LoadPresetFile(String),
    LoadLastState,
    /// the dialog's answer: one workspace gesture
    EditSource(Op),
    DeletePresetFile { name: String, query: String },
    DeleteLastState(String),
    RunAction(editor_commands::Action),
    /// / j picked a graph
    Go(String),
    /// / j picked a node of the graph shown: select it and frame it
    FindNode(w::Path),
}

pub type TimelineIntent = timeline_bar::Intent;

/// A traced viewport's readout: the film in pixels, the samples per pixel against their cap, the
/// bounces, and the seconds the accumulation has taken (counted from the time `since` its samples
/// last restarted, frozen once the cap is reached).
#[derive(Clone, Debug, PartialEq)]
pub struct Trace {
    pub film: (i64, i64),
    pub samples: i64,
    pub cap: i64,
    pub bounces: i64,
    pub seconds: f64,
    pub since: f64,
}

/// Transient shell presentation: the fallback tree before the first panel edit, a splitter draft,
/// and the default-tree override ("Restore layout"). Saved disclosure and floating bounds come from
/// the document's layout.
#[derive(Clone)]
pub struct Shell {
    pub tree: std::rc::Rc<panels::Panels>,
    pub hidden: Vec<Panel>,
    /// the split being dragged, its size
    pub live: Option<(layout::Path, panels::Size)>,
    pub window_live: Option<(layout::Path, layout::Bounds)>,
    pub restored: bool,
}

/// What a level's list rows are made from: a geometry object's network, or the scene's objects or
/// the World's layers; a cached row set is kept while its source is the same.
#[derive(Clone)]
pub enum RowSource {
    Network(net::Network),
    Objects(kg::T),
}

pub fn same_source(a: &RowSource, b: &RowSource) -> bool {
    match (a, b) {
        (RowSource::Network(a), RowSource::Network(b)) => net::ptr_eq(a, b),
        (RowSource::Objects(a), RowSource::Objects(b)) => kg::same(a, b),
        _ => false,
    }
}

/// What a selection names: a scene object or World layer of the list, or the lowered SOP node the
/// graph pane has selected.  Handles and the World map read its fields.
#[derive(Clone)]
pub struct Picked {
    pub id: i64,
    pub label: String,
    pub kind: String,
    pub fields: Vec<FieldView>,
}

pub fn of_object(node: &kg::Node) -> Picked {
    Picked { id: node.id, label: node.label.clone(), kind: node.kind.clone(), fields: kg::fields(node) }
}

pub fn of_sop(node: &sop::node::T) -> Picked {
    Picked {
        id: sop::node::id(node),
        label: sop::node::label(node).to_string(),
        kind: sop::node::operation(node).to_string(),
        fields: sop::node::parameter_fields(node),
    }
}

/// What a pane asks of the document: a gesture on the workspace text, a parameter or the name of a
/// scene object or World layer (the inspector), a message.
#[derive(Clone)]
pub enum Change {
    FollowSource(w::Path),
    DockPanels(layout::Path, layout::Path, rays_shell::chrome::Side),
    PanelState(layout::Path, panels::State),
    SelectLayout(String),
    SyntaxEdit(Op),
    /// a gesture on the editor graph's panels and layouts, applied beside the text ones
    LayoutEdit(layout_edit::Op),
    /// several rewrites that make one gesture (a new SOP graph and its object): all or none, one
    /// history entry with this label
    SyntaxBatch(String, Vec<Op>),
    /// the expression at `key` of the call at `home` is written in place: the call is bound to a
    /// name first, then the expression, and `make` gives the gesture on that name (several
    /// rewrites, one history entry)
    SyntaxInline { home: document::Home, key: ArgKey, make: Rc<dyn Fn(&w::Path) -> Op> },
    /// `Syntax_inline` for a layout gesture
    LayoutInline { home: document::Home, key: ArgKey, make: Rc<dyn Fn(&w::Path) -> layout_edit::Op> },
    /// an expression typed in a row of a scene object or World layer: written to the argument of
    /// the call that holds it (for a loop's copy, of the loop's template)
    ObjectArg { node: i64, key: String, sub: Vec<i64>, expr: Syntax },
    /// a row of a node's card pinned onto it or off it (`None`: the default rule), a layout edit
    PinRow { node: w::Path, label: String, pin: Option<bool> },
    /// an information message: the echo tip with the dot
    Notice(String),
    /// a refusal: the echo tip in the error ink
    Declined(String),
    SetParameter { node: i64, path: String, value: Value },
    Rename { node: i64, label: String },
}

/// Inspector callbacks return a sketch-owned result; the requesting loop downcasts it.
pub type PanelResult = Rc<dyn Any>;

#[derive(Clone)]
pub struct FrameResult {
    pub workspace: Shell,
    pub focus: Panel,
    /// the focused leaf itself, for its panel keys
    pub focus_path: Option<layout::Path>,
    pub pane_keys: Vec<(i64, (Panel, Option<layout::Path>))>,
    pub scope_view: scope::Pane,
    pub scope_changes: Vec<scope::Change>,
    pub selection: selection::Selection,
    pub menu: Option<node_menu::Menu>,
    /// the node menu's entry picked this frame
    pub menu_pick: Option<String>,
    pub tree: tree::Tree,
    pub outline: navigator::State,
    pub outline_intents: Vec<navigator::Intent>,
    /// the open level's objects or layers after this frame's derived edits; none inside a geometry
    /// object
    pub document: kg::T,
    pub edit_error: Option<String>,
    pub effects: Effects,
    pub timeline_intents: Vec<TimelineIntent>,
    pub frame_request: Option<i64>,
    pub prompt: Option<Prompt>,
    pub prompt_intent: Option<PromptIntent>,
    pub panel: Option<PanelResult>,
    /// a viewport handle holds the pointer
    pub grab: bool,
    pub settings: settings::T,
    /// a node asked to be entered
    pub opened: Option<i64>,
    pub live_cook: bool,
    /// names this frame's document change in history
    pub label: String,
    pub changes: Vec<Change>,
    pub tree_intents: Vec<tree::Intent>,
    pub text_intents: Vec<text_pane::Intent>,
    pub open_graph: Option<i64>,
    pub settings_changes: Vec<(String, Value)>,
    pub graph_panes: Vec<(panel_map::Key, GraphPane)>,
    pub locals: Vec<(panel_map::Key, Local)>,
    /// the text panes that are not the one in use: what each showed and asked this frame
    pub other_texts: Vec<(panel_map::Key, text_pane::Shown, Vec<text_pane::Intent>)>,
    pub handle_changes: Option<(i64, Vec<(String, Value)>)>,
    /// a header tool or an outline row that means a command
    pub bar_action: Option<editor_commands::Action>,
    /// a click on the graph header's Graph / List / Text
    pub view_pick: Option<Projection>,
    /// where a carried payload is hovered or released (`true`), from panes that are not the graph's
    pub drops: Vec<(carry_mod::Place, bool)>,
}

/// The tags `Pick::tint` highlights: those of the merge inputs made by the selected node at the
/// current probes.  Cached by what it was computed from.
#[derive(Clone)]
pub struct LitCache {
    pub site: w::Path,
    pub at: PathMap<i64>,
    pub evaluated: Arc<flow_eval::Eval>,
    pub scope: fprojection::Scope,
    pub tags: pick::Set,
}

/// A carry in flight (carry.rs, flow.md "Carry"): the payload, how it was picked up, the document
/// that was the history's present then (the cancel restores it, physically) and what the panes saw
/// under the pointer last frame.  While a target is hot the document shown (`doc`) is the edit
/// applied to a scratch copy; the history is not touched until the put.
#[derive(Clone)]
pub enum CarryPreview {
    Showing { place: carry_mod::Place, doc: Arc<Document>, what: String },
    /// the put would take longer than the preview budget: said, not shown
    HeldBack { place: carry_mod::Place, doc: Arc<Document>, what: String, reason: String },
    Refused { place: carry_mod::Place, reason: String },
}

#[derive(Clone)]
pub struct CarryReport {
    pub over: carry_mod::Place,
    pub dropped: bool,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Via {
    Pointer,
    Keys,
}

#[derive(Clone)]
pub struct Carry {
    pub payload: carry_mod::Payload,
    pub via: Via,
    pub original: Arc<Document>,
    /// the pieces cooked for `original`: a surface is picked on them, not on the preview
    pub settled: Vec<cook::Piece>,
    pub report: Option<CarryReport>,
    /// the key route's letters, computed for the pane graph named
    pub targets: Option<(Option<String>, Vec<(String, carry_mod::Place, String)>)>,
    /// the key route's letter
    pub chosen: Option<carry_mod::Place>,
    /// where the pointer was at pick-up or the last letter: it hovers only once it moves
    pub anchor: Option<(f64, f64)>,
    pub preview: Option<CarryPreview>,
    /// the node the pointer rests on, since
    pub resting: Option<(carry_mod::Place, f64)>,
    /// a reminder the strip shows ahead of the prompt until a letter is chosen
    pub hint: Option<String>,
}

/// The catalog of the editor's factories, shared with every other asker (`kinds::shared`).
pub fn flow_catalog(value: &EditorModel) -> Option<Arc<Catalog>> {
    editor_document::kinds::shared(Some(&value.factories)).ok()
}

/// Build the text pane vocabulary on first use.
#[derive(Clone)]
pub struct LazyVocab {
    cell: Rc<OnceCell<lisp_text::Vocab>>,
    init: Rc<dyn Fn() -> lisp_text::Vocab>,
}

impl LazyVocab {
    pub fn new(init: Rc<dyn Fn() -> lisp_text::Vocab>) -> LazyVocab {
        LazyVocab { cell: Rc::new(OnceCell::new()), init }
    }
    pub fn force(&self) -> &lisp_text::Vocab {
        self.cell.get_or_init(|| (self.init)())
    }
}

#[derive(Clone)]
pub struct EditorModel {
    pub preferences: String,
    /// the panels' default text size, Command-0 returns to it
    pub text_size: i64,
    pub guide: bool,
    pub hud: Option<(String, f64)>,
    /// preset directory
    pub presets: String,
    /// stable sketch identity, separate from named presets
    pub state_name: String,
    /// sketch name recorded in presets
    pub name: String,
    pub prompt: Option<Prompt>,
    /// the last message and what it was: information or a refusal
    pub notice: Option<(NoticeKind, String)>,
    /// when the notice last changed: the echo tip shows it for a while
    pub notice_at: f64,
    /// always the history's present
    pub doc: Arc<Document>,
    /// the workspace owner's immutable execution policy
    pub gpu: workspace_gpu::Policy,
    /// the document as its source file has it: an external change may replace it
    pub filed: document::Text,
    /// the graph panels' own canvas, level, selection, graph and route, by panel key; the one in
    /// use is what the pane-in-use accessors below read
    pub panes: panel_map::PanelMap,
    /// the workspace text pane: tab, drafts, errors (view state)
    pub text: text_pane::State,
    /// in the World, the view pane shows the lat-long map
    pub map_view: bool,
    /// cook while a drag holds the pointer
    pub live_cook: bool,
    /// the rows of the lists drawn last frame, cached by network, display node and active camera
    pub rows: Rc<Vec<(RowSource, (Option<i64>, Option<i64>), (Vec<tree::Row>, Vec<String>))>>,
    /// the SOP catalog
    pub factories: Vec<Factory>,
    /// the node menu, while it is open
    pub menu: Option<node_menu::Menu>,
    /// the iteration each zone shows: view state, not history
    pub probes: PathMap<i64>,
    /// viewport requests, outside the document and history
    pub previews: BTreeMap<i64, Preview>,
    pub viewed: BTreeMap<i64, (net::Network, i64)>,
    /// the drawing or image node the canvas panes show instead of their own picture; view state,
    /// outside the document and history
    pub canvas_preview: Option<Preview>,
    /// its plan node and type, resolved by `sync_views`
    pub canvas_viewed: Option<(Ty, i64)>,
    pub view_key: Option<(Arc<flow_eval::Eval>, PathMap<i64>, BTreeMap<i64, Preview>, Option<Preview>)>,
    /// the highlight of the selected node at the probes, see `lit_tags`
    pub lit: Option<LitCache>,
    pub edit_phases: PhaseTimer,
    /// nodes to select once the pane shows their graph
    pub select_later: Vec<w::Path>,
    pub open_import: Option<String>,
    /// what the text pane completes and describes
    pub lisp_vocab: LazyVocab,
    pub tree: tree::Tree,
    /// the Navigator panel: its search (view state)
    pub outline: navigator::State,
    pub ui: Rc<RefCell<Ui>>,
    pub workspace: Shell,
    pub timeline: timeline::Timeline,
    pub live_frame: Input,
    /// shared with retained sketch snapshots and edit rollback states
    pub cook: std::rc::Rc<cook::Cook>,
    pub edit_error: Option<String>,
    pub status_fps: Option<i64>,
    pub status_fps_at: f64,
    /// the step of the last frame, and how many frames in a row had exactly it:
    pub last_dt: f64,
    /// three or more is a fixed step, the timeline header says so
    pub steady: i64,
    pub history: history::EditorHistory,
    pub focus: Panel,
    /// which leaf of that kind: every leaf is an instance, and / o and / l act on the one clicked,
    /// not the first
    pub focus_path: Option<layout::Path>,
    pub pane_keys: Rc<Vec<(i64, (Panel, Option<layout::Path>))>>,
    pub leader: router::State,
    pub held_keys: Vec<Key>,
    /// keys held while the sketch had no keyboard: unseen by it until released
    pub sketch_blocked: Vec<Key>,
    /// frames since the window became key again (capped), -1 while it is not
    pub window_focus: i64,
    pub timeline_frames: i64,
    /// picked in the palette, run next frame
    pub queued: Vec<editor_commands::Action>,
    /// the panel `tree` belongs to: a list panel, or a graph panel in list view
    pub list_at: Option<panel_map::Key>,
    /// the panel `text` belongs to: a lisp panel, or a graph panel in text view
    pub text_at: Option<panel_map::Key>,
    /// the panel `outline` belongs to
    pub outline_at: Option<panel_map::Key>,
    /// the other panels' own, by panel key
    pub locals: Vec<(panel_map::Key, Local)>,
    pub started: Rc<Started>,
    /// the object a framing cook is running for: its bounds come back in its own space
    pub framing: Option<i64>,
    pub carry: Option<Carry>,
    /// seconds a carry's preview may take to apply or cook before it is only described
    pub carry_budget: f64,
    /// wall seconds, for the carry budget, the probe rate and phase timings; injected
    pub clock: cook::Clock,
    /// what each traced viewport's readout says: the film, the samples, the bounces and the seconds
    /// the accumulation took
    pub traces: Vec<(String, Trace)>,
    /// what the status strip calls the document: its source file, else its name
    pub file: String,
    /// the render frame of each viewport that shows less than its pane
    pub gates: Vec<(String, (i64, i64, i64, i64))>,
    /// the selected object in the focused view: its screen box and name
    pub selected_box: Option<(String, (i64, i64, i64, i64), String)>,
    /// the active viewport's header: looking through, and its renderer (0 solid, 1 wire, 2 traced);
    /// none for a 2D view
    pub view_tools: Option<(bool, i64)>,
}

/// A frame reads the shared model with the pane and transient UI state it is showing.
#[derive(Clone, Copy)]
pub struct ModelView<'a> {
    pub model: &'a EditorModel,
    pub doc: &'a Arc<Document>,
    pub pane: &'a GraphPane,
    pub key: Option<&'a panel_map::Key>,
    pub workspace: &'a Shell,
    pub focus: &'a Panel,
    pub selection: &'a selection::Selection,
    pub status_fps: Option<i64>,
    pub guide: bool,
    pub leader: &'a router::State,
}

impl<'a> From<&'a EditorModel> for ModelView<'a> {
    fn from(model: &'a EditorModel) -> Self {
        let pane = panel_map::active(&model.panes);
        Self { model, doc: &model.doc, pane, key: panel_map::key_ref(&model.panes), workspace: &model.workspace,
            focus: &model.focus, selection: &pane.picked, status_fps: model.status_fps,
            guide: model.guide, leader: &model.leader }
    }
}

impl<'a> From<&'a Rc<EditorModel>> for ModelView<'a> {
    fn from(model: &'a Rc<EditorModel>) -> Self { Self::from(model.as_ref()) }
}

impl std::ops::Deref for ModelView<'_> {
    type Target = EditorModel;
    fn deref(&self) -> &Self::Target { self.model }
}

// ---- the pane in use ----

/// The pane in use: the focused graph panel's own canvas, level and selection.
pub fn pane<'a>(value: &ModelView<'a>) -> &'a GraphPane {
    value.pane
}
/// the workspace document's graph pane
pub fn pane_view<'a>(value: &ModelView<'a>) -> &'a scope::Pane {
    &pane(value).view
}
/// what it was last laid out from
pub fn pane_laid<'a>(value: &ModelView<'a>) -> Option<&'a ScopeKey> {
    pane(value).laid.as_deref()
}
/// a scene, world or settings graph shown instead of the level's own
pub fn pane_shown<'a>(value: &ModelView<'a>) -> Option<&'a String> {
    pane(value).shown.as_ref()
}
/// where `u` returns to: level and pane graph, latest first
pub fn pane_route<'a>(value: &ModelView<'a>) -> &'a [(Level, Option<String>)] {
    &pane(value).route
}
pub fn pane_views<'a>(value: &ModelView<'a>) -> &'a LevelMap {
    &pane(value).views
}
pub fn pane_level<'a>(value: &ModelView<'a>) -> Level {
    pane(value).at
}
pub fn pane_selection<'a>(value: &ModelView<'a>) -> &'a selection::Selection {
    value.selection
}
/// The pane in use, to edit its view state in place.
pub fn pane_mut(value: &mut EditorModel) -> &mut GraphPane {
    panel_map::active_mut(&mut value.panes)
}
pub fn set_selection(value: &mut EditorModel, picked: selection::Selection) {
    panel_map::active_mut(&mut value.panes).picked = picked;
}
pub fn with_selection(mut value: EditorModel, picked: selection::Selection) -> EditorModel {
    set_selection(&mut value, picked);
    value
}

/// What `reduce` hands back for a frame.
#[derive(Clone)]
pub struct Update {
    pub effects: Effects,
    pub prepared_changed: bool,
    /// objects, lights, or the World changed: recompose
    pub scene_changed: bool,
    /// A framing request finished: `Some(None)` had no geometry.
    pub framed: Option<Option<Bounds>>,
    /// A preset loaded this frame; its environment view settings.
    pub loaded_view: Option<Syntax>,
    pub actions: Vec<editor_commands::Action>,
    pub panel: Option<PanelResult>,
    /// The frame without leader-consumed events, for the environment's own input handling.
    pub input: frame::Frame,
}

// Document identity.

/// The same workspace document, physically: the checked source, its inputs, and the rest by value.
pub fn same_workspace(a: &workspace_doc::T, b: &workspace_doc::T) -> bool {
    Arc::ptr_eq(&a.checked, &b.checked)
        && Arc::ptr_eq(&a.inputs, &b.inputs)
        && a.layout == b.layout
        && settings::same_values(&a.settings, &b.settings)
        && a.extra == b.extra
        && a.fragment == b.fragment
        && a.imports == b.imports
}

/// Two documents are unchanged when no editable field differs.
pub fn same_document(a: &Document, b: &Document) -> bool {
    same_workspace(&a.workspace.ws, &b.workspace.ws)
        && settings::same_values(&a.settings, &b.settings)
        && a.active_camera == b.active_camera
        && kg::same(&a.scene, &b.scene)
        && a.networks.len() == b.networks.len()
        && a.networks.iter().all(|(id, na)| {
            b.networks.get(id).is_some_and(|nb| na.context == nb.context && na.displayed == nb.displayed && net::ptr_eq(&na.graph, &nb.graph))
        })
        && a.layers.len() == b.layers.len()
        && a.layers.iter().all(|(id, la)| b.layers.get(id).is_some_and(|lb| kg::same(la, lb)))
}

// ---- vocabulary ----

/// the parts it reads, never the document: this lazy outlives the document it was made from
pub fn vocabulary(factories: &[Factory], doc: &Document) -> LazyVocab {
    let ops: Vec<flow_check::op::Op> = (*doc.workspace.ws.checked.ops).clone();
    let lowered: Vec<Factory> = doc.workspace.lowered.factories.clone();
    let factories: Vec<Factory> = factories.to_vec();
    LazyVocab::new(Rc::new(move || {
        let mut all: Vec<Factory> = factories.clone();
        all.extend(lowered.iter().cloned());
        let mut descriptors = flow_sop::catalog::with_library(all.iter().map(flow_sop::catalog::descriptor).collect());
        descriptors.extend(editor_document::kinds::descriptors());
        lisp_text::vocab(ops.clone(), &descriptors)
    }))
}

pub fn vocabulary_for(value: &EditorModel, doc: &Document) -> LazyVocab {
    let previous = &value.doc.workspace.ws.checked;
    let next = &doc.workspace.ws.checked;
    let same_nodes = previous.nodes.len() == next.nodes.len()
        && previous.nodes.iter().zip(next.nodes.iter()).all(|(a, b)| a.kind == b.kind && a.fields == b.fields && a.label == b.label && a.category == b.category);
    if Arc::ptr_eq(&previous.ops, &next.ops) && same_nodes {
        value.lisp_vocab.clone()
    } else {
        vocabulary(&value.factories, doc)
    }
}

/// An undo entry of a document (what `History` keeps).
pub fn snapshot(doc: &Document) -> Snapshot {
    document::snapshot(doc)
}

/// The host bar uses usize::MAX internally and prints -1 in its panel key; it sits above panel paths.
pub const HOST_BAR: usize = usize::MAX;

pub fn path_index_text(i: usize) -> String {
    if i == HOST_BAR { "-1".to_string() } else { i.to_string() }
}
