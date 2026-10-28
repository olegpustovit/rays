open Rays
open Sop
open Editor_document

type bounds = Cook.bounds

(* The panels' text: 13 points, the size of macOS control text (11 is its small size, and the kit
   face is narrow).  A core's [text_size] is its default (tests pass 11, the geometry the kit was
   drawn with).  Command +/-/0 step from it and return to it. *)
let default_text_size = 13

module Level_map = Map.Make (struct
  type t = Document.level

  let compare = Stdlib.compare
end)

(* How the graph pane shows the open network. *)
(* what a message says: information (a dot in its echo tip) or a refusal (the error ink) *)
type notice_kind = Info | Refusal
type projection = Graph_view | List_view | Text_view
type preview = { path : Flow.Workspace.path; chain : Flow.Workspace.path list }

(* What the workspace pane was last laid out from: the document, the probes and
   the graph, the recording evaluation of the checked source, and what its
   footers read (the cook's geometry counts, the time of a live document). *)
type scope_key = {
  ws : Workspace_doc.t;
  probe_map : int Layout_by_path.Path_map.t;
  graph : string;
  evaluated : Flow.Eval.t option;
  summaries : Cook.summary list;
  time : Input.t option;
  executions : Flow_native.Probe_run.reports;
  records : Flow_graph.Probe.t option;
  force : Flow_graph.Probe.force;
  scope : Flow_graph.Projection.scope;
  targets : (int * int) list; (* (object, compiled node) the cook is asked to count *)
  stamp : float; (* when the probes were last forced *)
  gap : float;
      (* the quiet time a moving clock alone must leave after that: a few frames of the cost *)
  fresh_last : bool; (* the last frame forced them, so the time since [stamp] was that frame *)
}

(* What a graph panel keeps of its own while another one has the focus. *)
type graph_pane = {
  view : Pxui_graph.Scope.t;
  laid : scope_key option;
  shown : string option;
  route : (Document.level * string option) list;
  views : projection Level_map.t;
  at : Document.level;
  picked : Selection.t; (* its navigation level and that level's selection *)
}

(* What a panel keeps of its own besides its PXUI state while another panel of its kind is the
   one in use: its list, its text pane, its outline (flow.md 11.11). *)
type local = {
  rows : Pxui_shell.Tree.t;
  code : Text_pane.state;
  nav : Navigator.state;
  sheet_owner : int;
}

(* The start keywords the editor last followed, by panel key. *)
type started = {
  on : string list option;
  views : (string list * string) list;
  tabs : (string list * string) list;
}

type prompt =
  | Keys
  | Saving of string
  | Palette of string (* command search query *)
  | Jumping of string (* / j: graph search query *)
  | Symbols of string (* Command-P: binding search query *)
  | Referencing of string * string (* Shift-F12: the symbol, and the line search query *)
  | Going of string (* Command-L: line[:column] *)
  | Browsing of { query : string; presets : (string * float) list; last_state : float option }
  | Picking of { split : bool; at : float * float }
    (* x or n: the menu of panel kinds at the pointer *)
  | Splitting of { panel : Pxui_shell.Layout.panel; axis : Pxui_shell.Layout.axis }
    (* x after a kind: the split follows the pointer until a click places it; Tab turns it *)
  | Making_macro of {
      nodes : Flow.Workspace.path list;
      draft : Flow_graph.Flow_edit.macro_draft;
      state : Pxui_shell.Prompt.macro;
    }
(* the make-macro dialog *)

type prompt_intent =
  | Save_preset_file of string
  | Load_preset_file of string
  | Load_last_state
  | Edit_source of Flow_graph.Flow_edit.op (* the dialog's answer: one workspace gesture *)
  | Delete_preset_file of { name : string; query : string }
  | Delete_last_state of string
  | Run_action of Editor_commands.action
  | Go of string (* / j picked a graph *)
  | Find_node of
      Flow.Workspace.path (* / j picked a node of the graph shown: select it and frame it *)

type timeline_intent = Pxui_shell.Timeline_bar.intent =
  | Pause_toggle
  | Stop_playback
  | Reset_playback
  | Seek_playback of int64
  | Set_end of int

(* A traced viewport's readout: the film in pixels, the samples per pixel against their cap, the
   bounces, and the seconds the accumulation has taken (counted from the time [since] its samples
   last restarted, frozen once the cap is reached). *)
type trace = {
  film : int * int;
  samples : int;
  cap : int;
  bounces : int;
  seconds : float;
  since : float;
}

(* Transient shell presentation: the fallback tree before the first panel edit,
   a splitter draft, and the default-tree override ("Restore layout"). Saved
   disclosure and floating bounds come from the document's layout. *)
type shell = {
  tree : Pxui_shell.Layout.t;
  hidden : Pxui_shell.Layout.panel list;
  live : (Pxui_shell.Layout.path * Editor_core.Panels.size) option;
      (* the split being dragged, its size *)
  window_live : (Pxui_shell.Layout.path * Pxui_shell.Layout.bounds) option;
  restored : bool;
}

(* What a level's list rows are made from: a geometry object's network, or the scene's objects or
   the World's layers; a cached row set is kept while its source is physically the same. *)
type row_source = Network of Flow_sop.Network.t | Objects of Kind_graph.t

let same_source a b =
  match (a, b) with Network a, Network b -> a == b | Objects a, Objects b -> a == b | _ -> false

(* What a selection names: a scene object or World layer of the list, or the lowered SOP node the
   graph pane has selected.  Handles and the World map read its fields. *)
type picked = { id : int; label : string; kind : string; fields : Param.field_view list }

let of_object (node : Kind_graph.node) =
  { id = node.id; label = node.label; kind = node.kind; fields = Kind_graph.fields node }

let of_sop node =
  {
    id = Sop.Node.id node;
    label = Sop.Node.label node;
    kind = Sop.Node.operation node;
    fields = Sop.Node.parameter_fields node;
  }

(* What a pane asks of the document: a gesture on the workspace text, a parameter or the name
   of a scene object or World layer (the inspector), a message. *)
type change =
  | Follow_source of Flow.Workspace.path
  | Dock_panels of
      Pxui_shell.Layout.path * Pxui_shell.Layout.path * [ `Left | `Right | `Top | `Bottom ]
  | Panel_state of Pxui_shell.Layout.path * Editor_core.Panels.state
  | Select_layout of string
  | Syntax_edit of Flow_graph.Flow_edit.op
  | Layout_edit of Layout_edit.op
      (** a gesture on the editor graph's panels and layouts, applied beside the text ones *)
  | Syntax_batch of string * Flow_graph.Flow_edit.op list
      (** several rewrites that make one gesture (a new SOP graph and its object): all or none, one
          history entry with this label *)
  | Syntax_inline of {
      home : Document.home;
      key : Flow_graph.Flow_edit.arg_key;
      make : Flow.Workspace.path -> Flow_graph.Flow_edit.op;
    }
      (** the expression at [key] of the call at [home] is written in place: the call is bound to a
          name first, then the expression, and [make] gives the gesture on that name (several
          rewrites, one history entry) *)
  | Layout_inline of {
      home : Document.home;
      key : Flow_graph.Flow_edit.arg_key;
      make : Flow.Workspace.path -> Layout_edit.op;
    }  (** {!Syntax_inline} for a layout gesture *)
  | Object_arg of { node : int; key : string; sub : int list; expr : Flow.Syntax.t }
      (** an expression typed in a row of a scene object or World layer: written to the argument of
          the call that holds it (for a loop's copy, of the loop's template) *)
  | Pin_row of { node : Flow.Workspace.path; label : string; pin : bool option }
      (** a row of a node's card pinned onto it or off it ([None]: the default rule), a layout edit
      *)
  | Notice of string  (** an information message: the echo tip with the dot *)
  | Declined of string  (** a refusal: the echo tip in the error ink *)
  | Set_parameter of { node : int; path : string; value : Param.value }
  | Rename of { node : int; label : string }

type 'panel frame_result = {
  workspace : shell;
  focus : Pxui_shell.Layout.panel;
  focus_path : Pxui_shell.Layout.path option; (* the focused leaf itself, for its panel keys *)
  pane_keys : (int * (Pxui_shell.Layout.panel * Pxui_shell.Layout.path option)) list;
  scope_view : Pxui_graph.Scope.t;
  scope_changes : Pxui_graph.Scope.change list;
  selection : Selection.t;
  menu : Pxui_graph.Node_menu.t option;
  menu_pick : string option; (* the node menu's entry picked this frame *)
  tree : Pxui_shell.Tree.t;
  outline : Navigator.state;
  outline_intents : Navigator.intent list;
  document : Kind_graph.t;
  (* the open level's objects or layers after this frame's derived edits; none inside a geometry
     object *)
  edit_error : string option;
  effects : Param.effects;
  timeline_intents : timeline_intent list;
  frame_request : int option;
  prompt : prompt option;
  prompt_intent : prompt_intent option;
  panel : 'panel option;
  grab : bool; (* a viewport handle holds the pointer *)
  settings : Settings.t;
  opened : int option; (* a node asked to be entered *)
  live_cook : bool;
  label : string; (* names this frame's document change in history *)
  changes : change list;
  tree_intents : Pxui_shell.Tree.intent list;
  text_intents : Text_pane.intent list;
  open_graph : int option;
  settings_changes : (string * Param.value) list;
  graph_panes : (string list * graph_pane) list;
  locals : (string list * local) list;
  other_texts : (string list * Text_pane.shown * Text_pane.intent list) list;
  (* the text panes that are not the one in use: what each showed and asked this frame *)
  handle_changes : (int * (string * Param.value) list) option;
  bar_action : Editor_commands.action option;
      (* a header tool or an outline row that means a command *)
  view_pick : projection option; (* a click on the graph header's Graph / List / Text *)
  drops : (Carry.place * bool) list;
      (* where a carried payload is hovered or released ([true]), from panes that are not the graph's *)
}

(* The tags {!Pick.tint} highlights: those of the merge inputs made by the selected
   node at the current probes.  Cached by what it was computed from. *)
type lit_cache = {
  site : Flow.Workspace.path;
  at : int Layout_by_path.Path_map.t;
  lowered : Flow_sop.Lower.t;
  scope : Flow_graph.Projection.scope;
  tags : Pick.Set.t;
}

(* A carry in flight (carry.ml, flow.md "Carry"): the payload, how it was picked up, the document
   that was the history's present then (the cancel restores it, physically) and what the panes
   saw under the pointer last frame.  While a target is hot the document shown ([doc]) is the edit
   applied to a scratch copy; the history is not touched until the put. *)
type carry_preview =
  | Showing of { place : Carry.place; doc : Document.t; what : string }
  | Held_back of { place : Carry.place; doc : Document.t; what : string; reason : string }
    (* the put would take longer than the preview budget: said, not shown *)
  | Refused of { place : Carry.place; reason : string }

type carry_report = { over : Carry.place; dropped : bool }

type carry = {
  payload : Carry.payload;
  via : [ `Pointer | `Keys ];
  original : Document.t;
  settled : Cook.piece list;
      (* the pieces cooked for [original]: a surface is picked on them, not on the preview *)
  report : carry_report option;
  targets : (string option * (string * Carry.place * string) list) option;
      (* the key route's letters, computed for the pane graph named *)
  chosen : Carry.place option; (* the key route's letter *)
  anchor : (float * float) option;
      (* where the pointer was at pick-up or the last letter: it hovers only once it moves *)
  preview : carry_preview option;
  resting : (Carry.place * float) option; (* the node the pointer rests on, since *)
  hint : string option; (* a reminder the strip shows ahead of the prompt until a letter is chosen *)
}

type t = {
  preferences : string;
  text_size : int; (* the panels' default text size, Command-0 returns to it *)
  guide : bool;
  hud : (string * float) option;
  presets : string; (* preset directory *)
  state_name : string; (* stable sketch identity, separate from named presets *)
  name : string; (* sketch name recorded in presets *)
  prompt : prompt option;
  notice : (notice_kind * string) option;
      (* the last message and what it was: information or a refusal *)
  notice_at : float; (* when the notice last changed: the echo tip shows it for a while *)
  doc : Document.t; (* always the history's present *)
  gpu : Flow_native.Workspace_gpu.policy; (* the workspace owner's immutable execution policy *)
  filed : Document.t;
      (* the document as its source file has it: an external change may replace it *)
  panes : graph_pane Panel_map.t;
  (* the graph panels' own canvas, level, selection, graph and route, by panel key; the one in use
     is what the pane-in-use accessors below read *)
  text : Text_pane.state; (* the workspace text pane: tab, drafts, errors (view state) *)
  map_view : bool; (* in the World, the view pane shows the lat-long map *)
  live_cook : bool; (* cook while a drag holds the pointer *)
  rows : (row_source * (int option * int option) * (Pxui_shell.Tree.row array * string list)) list;
  (* the rows of the lists drawn last frame, cached by network, display node and active camera *)
  factories : Edit_graph.factory list; (* the SOP catalog *)
  menu : Pxui_graph.Node_menu.t option; (* the node menu, while it is open *)
  probes : int Layout_by_path.Path_map.t;
      (* the iteration each zone shows: view state, not history *)
  previews : preview Document.Int_map.t; (* viewport requests, outside the document and history *)
  viewed : (Flow_sop.Network.t * int) Document.Int_map.t;
  canvas_preview : preview option;
      (* the drawing or image node the canvas panes show instead of their own picture; view state,
    outside the document and history *)
  canvas_viewed : (Flow.Ty.t * int) option; (* its plan node and type, resolved by [sync_views] *)
  view_key :
    (Flow_sop.Lower.t * int Layout_by_path.Path_map.t * preview Document.Int_map.t * preview option)
    option;
  lit : lit_cache option; (* the highlight of the selected node at the probes, see {!lit_tags} *)
  edit_phases : Flow.Phase_timer.t;
  select_later : Flow.Workspace.path list; (* nodes to select once the pane shows their graph *)
  open_import : string option;
  flow_catalog : Flow.Catalog.catalog option Lazy.t;
  lisp_vocab : Lisp_text.vocab Lazy.t; (* what the text pane completes and describes *)
  tree : Pxui_shell.Tree.t;
  outline : Navigator.state; (* the Navigator panel: its search (view state) *)
  ui : Pxui.Ui.t;
  workspace : shell;
  timeline : Timeline.t;
  live_frame : Input.t;
  cook : Cook.t;
  edit_error : string option;
  status_fps : int option;
  status_fps_at : float;
  last_dt : float; (* the step of the last frame, and how many frames in a row had exactly it: *)
  steady : int; (* three or more is a fixed step, the timeline header says so *)
  history : Document.t Editor_core.History.t;
  focus : Pxui_shell.Layout.panel;
  focus_path : Pxui_shell.Layout.path option;
      (* which leaf of that kind: every leaf is an
    instance, and / o and / l act on the one clicked, not the first *)
  pane_keys : (int * (Pxui_shell.Layout.panel * Pxui_shell.Layout.path option)) list;
  leader : Editor_commands.state;
  held_keys : Input.key list;
  sketch_blocked : Input.key list;
      (* keys held while the sketch had no keyboard: unseen by it until released *)
  window_focus : int; (* frames since the window became key again (capped), -1 while it is not *)
  timeline_frames : int;
  queued : Editor_commands.action list; (* picked in the palette, run next frame *)
  list_at : string list option;
      (* the panel [tree] belongs to: a list panel, or a graph panel in list view *)
  text_at : string list option;
      (* the panel [text] belongs to: a lisp panel, or a graph panel in text view *)
  outline_at : string list option; (* the panel [outline] belongs to *)
  locals : (string list * local) list; (* the other panels' own, by panel key *)
  started : started;
  framing : int option;
      (* the object a framing cook is running for: its bounds come back in its own space *)
  carry : carry option;
  carry_budget : float;
      (* seconds a carry's preview may take to apply or cook before it is only described *)
  clock : unit -> float;
      (* wall seconds, for the carry budget, the probe rate and phase timings; injected *)
  traces : (string * trace) list;
      (* what each traced viewport's readout says: the film, the samples,
    the bounces and the seconds the accumulation took *)
  file : string; (* what the status strip calls the document: its source file, else its name *)
  gates : (string * (int * int * int * int)) list;
      (* the render frame of each viewport that shows less than its pane *)
  selected_box : (string * (int * int * int * int) * string) option;
      (* the selected object in the focused view: its screen box and name *)
  view_tools : (bool * int) option;
      (* the active viewport's header: looking through, and its renderer (0 solid, 1 wire, 2 traced);
    none for a 2D view *)
      (* each traced viewport's header text (resolution, film step, samples), set by the host after
     it renders: view state, not history *)
}

(* The pane in use: the focused graph panel's own canvas, level and selection. *)
let pane value = Panel_map.active value.panes
let pane_view value = (pane value).view (* the workspace document's graph pane *)
let pane_laid value = (pane value).laid (* what it was last laid out from *)

let pane_shown value =
  (pane value).shown (* a scene, world or settings graph shown instead of the level's own *)

let pane_route value =
  (pane value).route (* where [u] returns to: level and pane graph, latest first *)

let pane_views value = (pane value).views
let pane_level value = (pane value).at
let pane_selection value = (pane value).picked
let with_pane value f = { value with panes = Panel_map.update f value.panes }
let with_selection value picked = with_pane value (fun pane -> { pane with picked })

type 'panel update = {
  core : t;
  effects : Param.effects;
  prepared_changed : bool;
  scene_changed : bool; (* objects, lights, or the World changed: recompose *)
  framed : bounds option option;  (** A framing request finished: [Some None] had no geometry. *)
  loaded_view : Flow.Syntax.t option;
      (** A preset loaded this frame; its environment view settings. *)
  actions : Editor_commands.action list;
  panel : 'panel option;
  (* The frame without leader-consumed events, for the environment's own
     input handling. *)
  input : Frame.t;
}

let vocabulary factories (doc : Document.t) =
  lazy
    (Lisp_text.vocab ~ops:doc.workspace.ws.checked.ops
       (Flow_sop.Catalog.with_library
          (List.map Flow_sop.Catalog.descriptor (factories @ doc.workspace.lowered.factories))
       @ Kinds.descriptors))

let vocabulary_for value (doc : Document.t) =
  let previous = value.doc.Document.workspace.ws.checked and next = doc.workspace.ws.checked in
  if
    previous.ops == next.ops
    && (previous.nodes == next.nodes
       || List.equal
            (fun (a : Flow.Workspace.node_definition) (b : Flow.Workspace.node_definition) ->
              a.kind = b.kind && a.fields = b.fields && a.label = b.label && a.category = b.category)
            previous.nodes next.nodes)
  then value.lisp_vocab
  else vocabulary value.factories doc
