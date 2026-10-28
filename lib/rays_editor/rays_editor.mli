(** The reusable graph-driven procedural sketch environment.

    A responsive view/graph/inspector workspace, persistent graph navigation,
    generated node inspection, bounded asynchronous cooking, status UI, and
    finite native execution. Overlay callbacks receive a frame and coordinates
    local to the current view pane. *)

(** The workspace document a sketch can open the editor on: a checked workspace, its
    layout by path and its settings (see [Editor_document.Workspace_doc]). *)
module Workspace_doc = Editor_document.Workspace_doc

module Timeline = Timeline

val workspace_catalog : ?factories:Sop.Edit_graph.factory list -> unit ->
  (Flow.Check.catalog, Diagnostic.t) result
(** The catalog a workspace text is checked against ({!Workspace_doc.of_text}):
    the SOP factories (default [Sop_catalog.Editor.factories]) plus the scene,
    world and settings kinds generated from the editor's schemas. *)

type window = Editor_document.Contexts.window =
  { title : string; width : int; height : int; fps : int; seed : int; fast : bool }

(** Sketch-owned settings in the editor document. *)
(** The [.rays] file a running sketch came from: polled for edits, saved over. *)
module Source : sig
  type t
  val at : file:string -> digest:string -> t
  (** The file, whose text has SHA-256 [digest]. *)

  val find : path:string -> digest:string -> t option
  (** [path] under the first [dune-project] not inside [_build], walking up from the
      executable and then the working directory; [None] when there is no such file. *)

  val file : t -> string
  val read_imports : file:string -> string -> ((string * string) list, Diagnostic.t list) result
  (** Resolve document-level relative imports beside [file]. No transitive imports. *)
  val poll : now:float -> t -> t * string option
  (** Read content/digest at most once per half second of [now], including
      preserved-mtime and inode replacements. Return changed text once;
      the editor reports read errors without replacing its document. *)

  val save : t -> string -> (t, [ `Changed | `Failed of string ]) result
  (** Atomically replace the file with the text, only while its digest is still the
      remembered one ([`Changed] otherwise); the write does not reload. *)
  (** The digest check precedes the rename; a competing writer between these
      operations requires a cooperative protocol to prevent overwrite. *)
end

module Settings : sig
  type t
  val none : t
  val make : 'record Param.schema -> 'record -> t
  (** Settings drawn in the inspector while no node is selected. *)

  val get : 'record Param.schema -> t -> 'record
  (** Read the current record back; raises [Invalid_argument] when [schema]
      does not describe it. *)
end

(** Editor internals exposed for tests and diagnostics.
    Layout and chrome live in [Pxui_shell]; sketches use [Editor]. *)
module Private : sig
  module Spreadsheet : sig
    val count : Rdk.Attribute.owner -> Rdk.Geometry.t -> int
    val columns : Rdk.Attribute.owner -> Rdk.Geometry.t -> (string * (int -> string)) array
    val view : Pxui.Ui.t -> bounds:int * int * int * int -> owner:int -> Rdk.Geometry.t option -> int option
  end
  (** Unstable test and diagnostic hooks. These are outside the supported
      sketch API and may change without compatibility shims. *)
  (** Helix-style leader keys. [/] (while no text field is focused) opens a
      centered which-key panel; the next key runs a binding from the global
      scope or from the focused pane, the one last clicked. Escape, [/], an
      unknown key, a click, or window focus loss cancel it. *)
  module Render_budget : sig
    val film : through:bool -> resolution:int * int -> gate:int * int -> (int * int) * int
    (** The path tracer's film for a root's resolution and a viewport's rectangle in drawable
        pixels.  Looking [through] the render camera the rectangle is the gate: the resolution
        divided by the first of 1, 2, 4, 8 that does not exceed it (the eighth when none does),
        and that divisor.  A free view is traced as it is drawn: the rectangle itself, step 1. *)

    val next_turn : order:string list -> last:string option -> string list -> string option
    (** Of the viewports that want the shared sample budget, the one after [last] in [order],
        cyclically. *)
  end

  module Leader : sig
    type action =
      | Save_preset | Browse_presets | Save_source | Reload_source | Open_source
      | Toggle_timeline | Toggle_graph | Toggle_inspector | Hide_ui | Open_camera
      | Play_pause | Reset | Stop
      | Add_node | Frame_tile | Frame_camera
      | Look_through | Look_through_camera | Fly | Tool of int
      | Render_mode of int
      | Undo | Redo
      | Panel_split of Pxui_shell.Layout.axis | Panel_close | Panel_retype of Pxui_shell.Layout.panel
      | Panel_pick | Window_pick
      | Panel_split_at of Pxui_shell.Layout.path * Pxui_shell.Layout.axis * float * Pxui_shell.Layout.panel
      | Toggle_map | Ui_scale of int | Restore_layout | Enter | Up | Go_world | Back of int | Peek | Pick_up | Jump
      | Text_command of Text_pane.command | Goto_symbol | Goto_line
      | Layout_switch of int | Layout_new | Layout_remove | Window_new of Editor_core.Panels.panel | Float_toggle
      | World_emit | World_reseed | World_time of float | World_play | World_preset of int
      | Scope_command of Pxui_graph.Scope.command
      | List_command of Pxui_shell.Tree.command
      | Guide_toggle | Guide_keys
      | Command_palette
      | Copy_lisp

    type command = (Pxui_shell.Layout.panel, action) Editor_core.Command.t

    type state = Editor_core.Router.state = Idle | Pending of string

    val scope : Pxui_shell.Layout.panel -> Pxui_shell.Layout.panel
    (** The scope a focused panel's commands use: every viewport is [View ""], the list and
        lisp panels are the graph pane's. *)

    val keymap : command list
    (** The single table behind dispatch, which-key, and the palette. *)

    val text_keys : (string * Editor_core.Keymap.trigger) list
    (** The text area's chords, listed on the key sheet and never in the keymap. *)

  end

  (** [timeline_frames] (default 240) is the scrub range of the timeline bar,
      extended while playback runs past it. [name] (default the workspace name; [run]
      uses the window title) is recorded in presets, which live in [presets]
      (default [~/.rays/<name>]): [/ s] saves the full document under a
      typed name (prefilled with the time), [/ b] searches, loads (Enter, one
      undo entry), and deletes (Delete twice) them. Edits also save one recovery
      file under [<presets>/state], at most twice a second and on close, using
      the source file's absolute path (or workspace name) as its identity.
      [/ b] offers it as "Last edited state". Opening an unchanged sketch
      preserves its recovery file; autosave keeps source files untouched.
      See {!Preset}. *)

  (** Effect- and dependency-aware cook scheduler. It fires initially, after a
      committed cook parameter change, after [force], and whenever the sketch
      clock changed and any reachable node declares [Time] or [Frame]. While a
      primary-pointer edit is held it cooks only when [live] (the inspector's
      "Live update while dragging", on by default) and the worker is idle;
      otherwise only the latest desired cook is retained until release. *)
  module Document : sig
    type t
    val scene_graph : t -> Sop.Edit_graph.t
    val object_network : t -> int -> (Flow_sop.Network.t * int option) option
    (** An object's immutable network and optional geometry display node.
        A missing owner returns [None]; an empty geometry network has no display. *)
  end

  (** Presets: the workspace as an s-expression file (source, layout,
      settings) plus the environment view. Only workspace documents save. *)
  module Preset : sig
    type loaded = { doc : Document.t; view : Flow.Syntax.t }
    val sanitize : string -> string
    val default_name : unit -> string
    val path : directory:string -> name:string -> string
    val save : directory:string -> name:string -> doc:Document.t ->
      view:Flow.Syntax.t -> (string, string) result
    val list : directory:string -> (string * float) list
    val delete : directory:string -> name:string -> (unit, string) result
    val load : ?gpu:Flow_ir.Gpu.config -> path:string -> factories:Sop.Edit_graph.factory list ->
      settings:Settings.t -> unit -> (loaded, string) result
  end

  module Schedule : sig
    type t
    val initial : t
    val step :
      ?live:bool -> t -> graphs:Sop.Graph.t list -> effects:Param.effects ->
      context_changed:bool -> force:bool -> busy:bool -> frame:Rays.Frame.t ->
      t * bool
  end

  (** Host cook boundary, exposed for deterministic worker tests and measurements. *)
  module Pick : sig
    module Set : Set.S with type elt = int
    val tags : Rdk.Geometry.t -> int array option
    (** The [__flow_src] tag of every primitive, when the geometry has them. *)
    val tint : Sop.Session.output -> Set.t ->
      (Sop.Session.output, Diagnostic.t) result
    (** The primitives whose tag is in the set take the selection tint, the
        rest are dimmed (a vertex [Cd]); the empty set changes nothing. *)
  end

  (** The workspace text pane's pure part: the text each tab shows. *)
  module Text_pane : sig
    type tab = Selection | Graph | Document
    type shown = {
      graph : string;
      text : string;
      mark : (int * int) option;
      key : string list;
      applied : string Lazy.t;
      body : (Flow.Syntax.t * (int * Diagnostic.span) list) option;
      source : Flow.Syntax.t list;
      spans : Flow.Lisp.spans;
      applied_spans : Flow.Lisp.spans Lazy.t;
    }
    val binding_at : shown -> int -> string list option
    (** The innermost binding or inline node at a byte in any tab. *)

    val reference_target : Flow.Syntax.t list -> from:string list -> string -> string list option
    val definition_at : shown -> int -> string list option
    val make_shown : Flow.Syntax.t list -> string -> string list option -> tab -> shown
    val scrub_op : shown -> int * int -> string -> Flow_graph.Flow_edit.op option
    val line_of : string -> Diagnostic.t -> int option
    type find = Text_pane.find = { query : string; replace : string; case : bool; replacing : bool; at : int; grab : bool }
    type request = Text_pane.request = { select : int * int; splice : (int * int * string) list }
    val find_current : find -> (int * int) list -> (int * int) option
    val find_step : find -> (int * int) list -> [ `Next | `Previous ] -> find
    val find_replace : find -> (int * int) list -> all:bool -> (request * find) option
    val find_all : case:bool -> string -> string -> (int * int) list
    (** The (start, stop) of every non-overlapping match of a query. *)
    val symbols : string -> (string * string * int) list
    (** The bindings of a text: (name, kind, byte of the name). *)
    val symbol_at : string -> int -> string option
    (** The symbol a byte touches. *)
    val references : string -> string -> (int * string) list
    (** The byte and line text of every use of a symbol. *)
    val line_target : string -> string -> int option
    (** The byte of ["line"] or ["line:column"] (1-based). *)
    val binding : Flow.Syntax.t list -> string list -> (Flow.Syntax.t option * Flow.Syntax.t) option
    val paste_ops : Flow.Syntax.t list -> graph:string -> scope:string list -> string ->
      (Flow_graph.Flow_edit.op list, string) result
    (** Pasted text as [Add_node]s with fresh names, paired by position, renamed at once. *)
  end

  (** The editor's Lisp as the text area's language (tests of its indentation and lexing). *)
  module Lisp_text : sig
    type vocab
    val vocab : ?ops:Flow.Op.t list -> Flow_sop.Catalog.descriptor list -> vocab
    val indent : string -> int -> string
    val context_at : string -> int -> [ `Code | `String | `Comment ]
    (** What a character typed at a byte lands in. *)
    val brackets : string -> (int * int) list
    type names = { graphs : string list; materials : string list; cameras : string list; layouts : string list }
    (** What the document knows beyond the text shown: every graph, the material graphs, the
        cameras of the scene, the layout names (the values of [:material], [:camera], [:active]). *)
    val no_names : names
    val complete : ?names:names -> ?imports:(string * string) list -> vocab -> string -> int -> Pxui.Ui.completion list
    (** The ranked completions for the token ending at the caret. *)
    val ref_at : string -> int -> string option
    (** The graph named by the [(ref name)] form at a byte. *)
    val color_chips : string -> (int * int * Rays_math.Color.t) list
    (** The ["#rrggbb"] literals of a text, as byte ranges with their colour. *)
    val describe : vocab -> string -> int -> (int * int * string) option
    val number_at : string -> int -> (int * int) option
    val parinfer_text : string -> int -> string * int
    (** Parinfer's indent mode: the text with its closing brackets inferred from indentation,
        and where the caret lands. *)
    val language : ?vocab:vocab -> ?names:names -> ?imports:(string * string) list -> ?parinfer:bool -> Pxui.Theme.t -> Pxui.Ui.language
  end

  (** The host bars: where their buttons sit. *)
  module Bars : sig
    type tool = Add | Repeat | Iterate | Fn | Macro | Defn | Outline
    val tool_rect : ?outline:bool -> Pxui.Ui.t -> header:int * int * int * int -> from:float -> tool -> (float * float * float * float) option
  end

  (** The Navigator panel: its rows and where each is drawn. *)
  module Navigator : sig
    type state
    val initial : state
    val with_query : string -> state -> state
    val open_graph : string -> state -> state
    type obj = { depth : int; letter : string; name : string; detail : string;
                 visible : bool option; render : bool option; lead : bool; inert : bool; chosen : bool;
                 home : string list option }
    type params = {
      workspace : Flow.Workspace.t;
      active : string option;
      scope : Flow_graph.Projection.scope option;
      records : Flow_graph.Probe.t option;
      probes : string list -> int;
      selected : string list list;
      chips : (string * Rays_math.Color.t) list;
      objects : obj list;
      root_detail : string;
      layouts : (string list * int) option;
      notes : (string * string) list;
    }
    type row
    val chips : Flow.Eval.t -> (string * Rays_math.Color.t) list
    val rows : ?wide:bool -> state -> params -> row array
    val describe : row -> string
    val row_rects : state -> params -> bounds:int * int * int * int ->
      (row * (float * float * float * float)) array
  end

  module Cook : sig
    type bounds = Rays_math.Vec3.t * Rays_math.Vec3.t
    type 'prepared piece = {
      id : int;
      graph : Sop.Graph.t;
      prepared : 'prepared;
      bounds : bounds option;
      settings : Settings.t;
      context : string;
      output : Sop.Session.output;
      lit : Pick.Set.t;
      surface : Rdk.Surface_index.t option Lazy.t;
    }
    type 'prepared t
    type 'prepared update = {
      cook : 'prepared t;
      edit_error : string option;
      prepared_changed : bool;
      framed : bounds option option;
    }
    val create : prepare:(Settings.t -> Sop.Session.output -> ('a, string) result) ->
      seed:int64 -> grain:int -> ?domains:int -> ?await:bool -> max_entries:int ->
      max_payload_bytes:int -> unit -> ('a t, string) result
    (** [await] (default: false) makes [update] block on
        the cook it submits, so a fixed-step run shows exactly frame n. *)
    val status : 'a t -> Sop.Async_cook.status
    val set_volatile : 'a t -> (int -> bool) -> unit
    (** Pass [Flow_sop.Lower.is_volatile lowered] after each lowering (W3). *)
    val stats : 'a t -> Sop.Session.stats
    val pieces : 'a t -> 'a piece list
    val applied : 'a t -> int -> Flow_sop.Value_lane.resolved option
    val force : 'a t -> 'a t
    val geometry : 'a t -> object_id:int -> node_id:int -> Flow_graph.Probe.geometry option
    (** The counts of a compiled node an object's last [~probes] asked for. *)
    val pick : 'a piece -> origin:Rays_math.Vec3.t -> direction:Rays_math.Vec3.t ->
      (float * int) option
    (** The nearest displayed primitive under a ray in the piece's own space:
        its distance and provenance tag. *)
    val update : ?gpu:Flow_ir.Gpu.config -> ?input:Input.t -> ?live:bool -> ?probes:(int * int) list -> ?lit:Pick.Set.t ->
      ?pump:(unit -> unit) ->
      'a t -> settings:Settings.t ->
      objects:(int * Flow_sop.Network.t * int) list -> edit_error:string option ->
      effects:Param.effects -> timeline_changes:Timeline.change list ->
      timeline:Timeline.t -> frame:Rays.Frame.t ->
      frame_request:(int * int) option -> 'a update
    val close : 'a t -> unit
  end
end

(** The renderer choice a sketch offers in its settings block: one shared
    field, so the control reads the same in every sketch. The sketch's
    [prepare] reads it back through its own schema. *)
module Renderer : sig
  type t = Path_traced | Raster | Wireframe

  val field :
    default:t -> get:('record -> t) -> set:(t -> 'record -> 'record) ->
    'record Param.field
  (** The ["renderer"] field labelled "Renderer". *)

  val wire_mesh : Rdk.Geometry.t -> (Rays.Mesh.t, string) result
  (** Every polygon edge once, as a line mesh for the wireframe renderer. *)

  val wire_color : Rays_math.Color.t -> Rays_math.Color.t
  (** A contrasting blue wire color for a light or dark viewport background. *)
end

module Editor : sig
  type 'prepared t
  module Private : sig
    val gpu_qualification : 'prepared t -> unit
    (** Explicit native benchmark selection. Production requires measured GPU costs. *)
    val status_text : 'prepared t -> string
    (** The status strip's text: a cook or edit error reads "Cook rejected: ..." in it. *)
    val live_frame : 'prepared t -> Input.t
    (** The frame facts the sketch read last frame: the pointer only while it is the picture's. *)
    val inspector_subject : 'prepared t -> Flow.Workspace.path option
    (** The node the inspector shows for the selection, when it is exactly one. *)
    val new_graph : 'prepared t -> string -> Flow_graph.Flow_edit.op
    (** The [Set_graph] a "New graph" of this context name ([sop], [scene], ...) writes. *)
    val menu_forms : (string * string) list
    (** The language forms of the add menu with the Lisp each is added as. *)
    val lisp_completions : 'prepared t -> string -> Pxui.Ui.completion list
    (** Completions from the vocabulary currently used by the workspace text panes. *)
    val image_plan : 'prepared t -> Flow.Eval.plan
    val image_payload : ?state:Flow.Eval.state -> ?live:Input.t ->
      'prepared t -> Flow.Eval.value -> (Sop.Image.t,Diagnostic.t) result
    (** Inspect the owned plan and request an independent immutable CPU image cook. *)
    val image_stats : 'prepared t -> int * int
    val image_render_stats : 'prepared t -> int * int * int * int
    (** Canvases created/destroyed, successful captures and actual GPU pixel readbacks. *)

    val canvas_scenes : 'prepared t -> Rays.Scene.t list
    (** The picture each canvas pane drew last frame, in layout order. *)
    val image_gpu_stats : 'prepared t -> Flow_gpu.Host.Private.stats * int * int * int * int
    (** Cumulative Host totals, sinks created/closed and sink buffer/texture creations,
        including evicted runners and failed sinks; inspect outside frame measurements. *)

    val image : 'prepared t -> Flow.Eval.value -> (Rays.Image.t, Diagnostic.t) result
    val with_images : ?state:Flow.Eval.state -> ?live:Input.t -> ?plan:Flow.Eval.plan ->
      'prepared t ->
      (image:(Flow.Eval.value -> (Rays.Image.t, Diagnostic.t) result) ->
       texture:(Flow.Eval.value -> (Rays.Texture.t, Diagnostic.t) result) -> 'a) -> 'a
    (** Scoped production image ownership and initial-domain SOP snapshots for qualification. *)
    val host_stats : 'prepared t -> bool * int * int * int
    (** Quit requested, effects fired, samples created and samples destroyed. *)
  end
  val create :
    ?name:string ->
    ?presets:string ->
    ?factories:Sop.Edit_graph.factory list ->
    ?settings:Settings.t ->
    ?camera:Rays.Easy_camera.t ->
    ?seed:int64 ->
    ?grain:int ->
    ?domains:int ->
    ?max_entries:int ->
    ?max_payload_bytes:int ->
    ?await:bool ->
    ?carry_budget:float ->
    ?preferences:string ->
    ?ui_text_size:int ->
    ?ui_font:string ->
    workspace:Workspace_doc.t ->
    ?source:Source.t ->
    prepare:(Settings.t -> Sop.Session.output -> ('prepared, string) result) ->
    draw:(Sop.Node.t -> 'prepared -> Rays.Scene.D3.t) ->
    unit -> ('prepared t, string) result
  (** [factories] is the SOP catalog (default [Sop_catalog.Editor.factories]);
      passing a non-empty list replaces it (prepend custom SOPs to
      [Sop_catalog.Editor.factories] to extend).
      [await] (default: false) makes each
      [update] block on the cook it submits, so a fixed-step run or a test sees the
      settled result of every frame instead of racing the worker. [carry_budget] (default 0.5)
      is the seconds a carry's preview may take, to apply or to cook, before it is only described
      (the target is lit, the strip says what a release writes, no picture).
      [prepare] runs on the cook worker domain with submission settings.
      It must only do pure CPU work on immutable/disjointly owned data;
      SDL, Metal, textures, fonts, audio, UI and runtime caches stay on the
      initial domain. [draw] run on the initial domain. *)

  val update : 'prepared t -> Rays.Frame.t -> 'prepared t

  val update_with :
    'prepared t -> Rays.Frame.t -> inspector:(Pxui.Ui.t -> 'a) ->
    'prepared t * 'a option
  (** [update], also building sketch-owned kit widgets below the camera and
      render sections shown while no node is selected. The result is [None]
      on frames where that panel is not built. *)

  (* Call from [Sketch.run_state ~after_present] when driving the environment
      manually so PNG requests save the completed frame. *)
  val after_present : 'prepared t -> Rays.Frame.t -> 'prepared t

  val settings : 'prepared t -> Settings.t
  val set_settings : 'prepared t -> Settings.t -> 'prepared t
  (** Replace the sketch settings from code: one undo step and a fresh cook,
      since [prepare] receives them. Inspector edits do the same. *)

  (** The workspace keeps one [Editor_core.History] history of the editable document:
      graph edits and inspector commits are entries, continuous slider drags
      collapse into one, and Command/Ctrl-Z, Shift-Command/Ctrl-Z, and
      Ctrl-Y step it. *)
  val can_undo : 'prepared t -> bool
  val can_redo : 'prepared t -> bool
  val undo_label : 'prepared t -> string option
  (** The label of the edit undo would revert ("Repeat", "Connect", ...). *)

  val redo_label : 'prepared t -> string option

  val workspace : 'prepared t -> Workspace_doc.t
  (** The document the editor is open on: every document is a workspace. *)

  val probe : 'prepared t -> Flow.Workspace.path -> int option
  (** The iteration the zone at that path shows (view state, kept by path across
      edits and reloads, never in history). *)

  val set_probe : 'prepared t -> Flow.Workspace.path -> int -> 'prepared t

  val node_box : 'prepared t -> Flow.Workspace.path -> (int * int * int * int) option
  (** The rectangle, in window points, of the node at a path in the graph pane as last laid out
      (tests and tools that click on a node). *)

  val edit : 'prepared t -> Flow_graph.Flow_edit.op -> ('prepared t, string) result
  (** One gesture on the workspace: rewrite the source, re-check, lower into
      the scene's objects, recook, and record one history entry named by the
      op. An error changes nothing. *)

  val edit_phases : 'prepared t -> Flow.Phase_timer.t
  (** Phase durations and invocation counts of the last document edit. *)

  val carrying : 'prepared t -> (string * string) option
  (** The payload in flight (kind and value, [("material", "(ref cobalt)")]), picked up by a
      press on a Navigator row or by [y]: every document change waits for the put, one history
      entry named "Put"; while a target is hot the panels show the edit on a scratch document,
      and Escape, a release over nothing or a focus loss restores the one that was there. *)

  val carry_line : 'prepared t -> string option
  (** What the status strip says about the carry: the target letters, the preview's words, or
      the checker's reason for a refusal. *)

  val scene : 'prepared t -> Rays.Frame.t -> Rays.Scene.t
  val close : 'prepared t -> unit
  val crash_dump : 'prepared t -> string -> unit
  val document : 'prepared t -> Sop.Edit_graph.t
  val selected_node : 'prepared t -> Sop.Node.t option
  val displayed_node : 'prepared t -> Sop.Node.t
  val prepared : 'prepared t -> 'prepared option
  val camera : 'prepared t -> Rays.Easy_camera.t
  (** The interactive viewport camera. *)

  val renderer : 'prepared t -> Renderer.t
  val set_renderer : 'prepared t -> Renderer.t -> 'prepared t
  (** Every 3D editor offers raster, wireframe and path tracing in its inspector.
      [set_renderer] applies at the next update. The choice is saved with viewport
      state, and uses a sketch's renderer setting when it supplies one. A
      [scene/root] in the workspace text names the renderer and wins over both. *)

  val render_camera : 'prepared t -> Rays.Camera.t
  (** The ACTIVE camera object's view and lens (the viewport camera when the
      document has none). A workspace with no scene graph starts with a default camera
      following the viewport; a scene graph is authoritative (an empty one
      means no camera, and deleting the host's camera writes one). With follow viewport on, viewport motion writes
      the node as view state (no undo entry) and node edits or undo move the
      viewport. PNG export and look-through use it. *)

  type render_settings = { width : int; height : int; max_spp : int }

  val render_settings : 'prepared t -> render_settings
  (** The scene root's output resolution in pixels (its aspect also frames
      look-through) and the samples per pixel at which a sketch's progressive
      renderer stops; the defaults when the scene graph has no [scene/root]. A viewport
      over another scene instance with a [scene/root] of its own renders at that root's
      resolution and cap, through that root's camera (see {!viewport_camera}). *)

  val film : 'prepared t -> Rays.Frame.t -> int * int * int * int
  (** The rect inside the view pane (pane-relative) that the render fills:
      the whole pane, or the root's aspect (its resolution, the gate) fitted and centred while
      looking through it or while the path tracer renders. *)

  type slot = { film : int * int; step : int; samples : int; max_spp : int; viewports : int }

  val slot : 'prepared t -> string -> slot option
  (** The path tracer slot serving the viewport with this key (see {!viewport_camera}), when it
      has one: the size in drawable pixels of its film, the step of the root's resolution that
      is (1, 2, 4 or 8: the film is the resolution divided by it), the samples per pixel
      accumulated, the viewport's root's cap, and how many viewports the slot serves.
      Viewports asking for the same scene, camera, film and renderer settings share one slot. *)

  val view_camera : 'prepared t -> Rays.Camera.t
  (** What the view pane shows this frame, for a sketch's own renderer:
      [render_camera] while looking through it, otherwise the viewport camera
      with the ACTIVE camera's lens (focused on the orbit target unless the
      camera follows the viewport). With follow viewport off, the view orbits
      freely around the camera object, whose frustum, aim line, and eye
      marker are drawn over the sketch renderer's picture; clicking the eye selects
      it, and W shows a handle sliding its target along the view direction,
      its focus distance. The Viewport section pairs the look-through toggle
      with "Camera follows viewport" (the ACTIVE camera's own parameter, one
      undo entry), so a fixed camera is set up by looking through it and
      orbiting. *)

  val viewport_camera : 'prepared t -> string -> Rays.Camera.t
  (** What the viewport with this key (editor graph/binding for unique named
      panels, placement for inline/looped/repeated panels, ["main"] for the
      default layout) shows: each viewport keeps its own orbit, a copy of the focused one's at
      the first focus, and the wheel, drag and fly of the focused viewport move only it.
      Looking through, a viewport over a scene instance with a [scene/root] sees through the
      camera that root names; any other sees through {!render_camera}. *)

  val flying : 'prepared t -> bool
  (** [/ w] with the view focused: held W/S/A/D/Q/E fly the viewport
      camera ([Easy_camera.fly]) with the pointer captured through
      [Sketch.set_relative_mouse]. Escape or focus loss exits; Space or [/] exits and
      opens the leader. *)

  val look_through : 'prepared t -> bool
  (** The view shows [render_camera] ([/ v], or the Camera panel toggle);
      orbit input is frozen unless the active camera follows the viewport. *)

  val timeline : 'prepared t -> Timeline.t
  val panes : 'prepared t -> Rays.Frame.t -> Pxui_shell.Layout.panes

  (** {2 Scene}

      The document is a scene of objects above the SOP networks. The code
      [graph] is the geometry object [geo1], and [?world] the World. The graph pane shows the open level as a graph
      or a list ([/ l]): [i], a double-click, or activating a list row
      enters a geometry object's SOP network or the World's layer stack, and
      [u] goes back up; [/ e] opens the World (created on first use), and
      [/ a] opens the add menu of the open level (objects, SOPs, or World
      layers): hover a category for its submenu, or type to search. Object transforms place each object's cook as a
      whole, so moving one never re-cooks SOPs. [graph], [document],
      [prepared], and [displayed_node] describe the open (else the first)
      geometry object. *)

  val level : 'prepared t -> string option
  (** The object whose network is open; [None] at the scene level. *)

  val scene_document : 'prepared t -> Sop.Edit_graph.t
  (** Objects as nodes: input 0 is the parent, parameters the transform. *)

  val objects : 'prepared t -> (Rays_math.Mat4.t * 'prepared) list
  (** Visible, renderable geometry objects' latest cooks at their world
      transforms, e.g. for a sketch's own path tracer. *)

  val lights : 'prepared t -> Rays.Light.t list
  (** Visible, renderable light objects in world space. *)

  val world : 'prepared t -> Rays.World.baked option
  (** This frame's World bake (preview size during a gesture or playback),
      or [None] without a World. *)

  val run :
    ?inputs:(string * (string * Flow.Eval.value) list) list ->
    ?name:string ->
    ?factories:Sop.Edit_graph.factory list ->
    ?camera:Rays.Easy_camera.t ->
    ?seed:int64 ->
    ?grain:int ->
    ?max_entries:int ->
    ?max_payload_bytes:int ->
    config:Rays.Sketch.config ->
    workspace:Workspace_doc.t ->
    ?source:Source.t ->
    prepare:(Settings.t -> Sop.Session.output -> ('prepared, string) result) ->
    draw:(Sop.Node.t -> 'prepared -> Rays.Scene.D3.t) ->
    unit -> (unit, Rays.Sketch.error) result
end

(** The frame reducer without a UI frame: an unstable hook for tests, like {!Private}.
    [step e actions frame] reduces one frame in which no pane was built: the keys are [actions],
    the list's selection is [select] (scene object ids), and [preview] puts a payload in flight
    whose hot target shows that edit on a scratch document, as a carry does. *)
module Reduce : sig
  val open_import : 'a Editor.t -> string -> 'a Editor.t
  val spreadsheet : 'a Editor.t -> int list -> int * Rdk.Geometry.t option
  val select_path : 'a Editor.t -> string list -> 'a Editor.t
  (** Select the checked lexical path; unstable test/diagnostic hook. *)
  val view : 'a Editor.t -> string list -> 'a Editor.t
  (** [v] on the checked lexical path, as the graph pane requests it; unstable test hook. *)
  val step : 'prepared Editor.t -> ?select:int list -> ?preview:Flow_graph.Flow_edit.op ->
    Private.Leader.action list -> Rays.Frame.t -> 'prepared Editor.t
end

(** A [.rays] sketch as a program.  Command-S rewrites the file when it is
    still the text the document came from (comments intact), else saves a preset;
    an edit of the file from any editor reloads the running document as one history
    entry, "Reload <file>", and a file that does not check leaves the last good
    document with the diagnostics in the status bar and the text pane. *)
module Workspace : sig
  type process = { max_frames : int option; crash_dir : string option; profile : bool;
    preferences : string option; ui_font : string option }
  (** What the process environment says, parsed once by the executable (bin/rays.ml); the
      libraries never read it. [no_process] is all defaults. *)
  val no_process : process

  type source = { path : string; digest : string }
  (** Where a workspace came from: [path] relative to the project root, [digest]
      the SHA-256 of its text. *)

  val load : ?ops:Flow.Op.t list -> ?imports:(string * string) list -> ?factories:Sop.Edit_graph.factory list -> string ->
    (Workspace_doc.t, Diagnostic.t list) result
  (** Parse and check a [.rays] text against {!workspace_catalog}. *)

  val export : ?graph:string -> ?fps:int -> directory:string -> frames:int ->
    Workspace_doc.t -> (unit, Diagnostic.t) result
  (** Export a draw graph (the first by default) through [Sketch.export_state].
      Each invocation starts a fresh fold and pins every frame fact. *)

  val sop_graphs : ?factories:Sop.Edit_graph.factory list -> Workspace_doc.t ->
    ((string * Sop.Graph.t) list, string) result
  (** Every [sop] graph of the document with its result compiled, by name, for cooking without
      an editor (a check, a batch). A graph without a result node is left out. *)

  val declared_camera : ?factories:Sop.Edit_graph.factory list -> Workspace_doc.t ->
    Rays.Easy_camera.t -> Rays.Easy_camera.t
  (** The camera of the scene's first camera node, or the default base camera. *)

  val main : ?text:string -> ?process:process -> string -> (unit, Diagnostic.t list) result
  (** [main path] opens the [.rays] file at [path] and its imports beside it in the editor
      ([rays FILE.rays]); [text] opens that text instead, saved to [path] (the empty editor). On a
      failure it returns diagnostics without printing or exiting. *)
end
