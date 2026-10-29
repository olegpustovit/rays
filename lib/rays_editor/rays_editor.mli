(** The reusable graph-driven procedural sketch environment.

    A responsive view/graph/inspector workspace, persistent graph navigation, generated node
    inspection, bounded asynchronous cooking, status UI, and finite native execution. Overlay
    callbacks receive a frame and coordinates local to the current view pane. *)

module Workspace_doc = Editor_document.Workspace_doc
(** The workspace document a sketch can open the editor on: a checked workspace, its layout by path
    and its settings (see [Editor_document.Workspace_doc]). *)

module Timeline = Timeline

val workspace_catalog :
  ?factories:Sop.Edit_graph.factory list -> unit -> (Flow.Catalog.catalog, Diagnostic.t) result
(** The catalog a workspace text is checked against ({!Workspace_doc.of_text}): the SOP factories
    (default [Sop.Nodes.factories]) plus the scene, world and settings kinds generated from the
    editor's schemas. *)

type window = Editor_document.Lowering.window = {
  title : string;
  width : int;
  height : int;
  fps : int;
  seed : int;
  fast : bool;
}

(** Sketch-owned settings in the editor document. *)

(** The [.rays] file a running sketch came from: polled for edits, saved over. *)
module Source : sig
  type t

  val at : file:string -> digest:string -> t
  (** The file, whose text has SHA-256 [digest]. *)

  val find : path:string -> digest:string -> t option
  (** [path] under the first [dune-project] not inside [_build], walking up from the executable and
      then the working directory; [None] when there is no such file. *)

  val file : t -> string

  val read_imports : file:string -> string -> ((string * string) list, Diagnostic.t list) result
  (** Resolve document-level relative imports beside [file]. No transitive imports. *)

  val poll : now:float -> t -> t * string option
  (** Read content/digest at most once per half second of [now], including preserved-mtime and inode
      replacements. Return changed text once; the editor reports read errors without replacing its
      document. *)

  val save : t -> string -> (t, [ `Changed | `Failed of string ]) result
  (** Atomically replace the file with the text, only while its digest is still the remembered one
      ([`Changed] otherwise); the write does not reload. *)

  (** The digest check precedes the rename; a competing writer between these operations requires a
      cooperative protocol to prevent overwrite. *)
end

module Settings : sig
  type t = Editor_document.Settings.t

  val none : t

  val make : 'record Param.schema -> 'record -> t
  (** Settings drawn in the inspector while no node is selected. *)

  val get : 'record Param.schema -> t -> 'record
  (** Read the current record back; raises [Invalid_argument] when [schema] does not describe it. *)
end

(** The renderer choice a sketch offers in its settings block: one shared field, so the control
    reads the same in every sketch. The sketch's [prepare] reads it back through its own schema. *)
module Renderer : sig
  type t = Path_traced | Raster | Wireframe

  val field :
    default:t -> get:('record -> t) -> set:(t -> 'record -> 'record) -> 'record Param.field
  (** The ["renderer"] field labelled "Renderer". *)

  val wire_color : Rays_math.Color.t -> Rays_math.Color.t
  (** A contrasting blue wire color for a light or dark viewport background. *)
end

module Editor : sig
  type prepared = Cook.prepared

  val prepare : Settings.t -> Sop.Session.output -> (prepared, string) result
  (** What the app prepares of a cooked object: its geometry and its surface. *)

  val draw : Sop.Node.t -> prepared -> Rays.Scene.D3.t
  (** The object's 3D scene, pieces exploded as its node says. *)

  type t

  module Private : sig
    val gpu_qualification : t -> unit
    (** Explicit native benchmark selection. Production requires measured GPU costs. *)

    val resolved_scene : t -> Editor_document.Kind_graph.t

    val status_text : t -> string
    (** The status strip's text: a cook or edit error reads "Cook rejected: ..." in it. *)

    val live_frame : t -> Input.t
    (** The frame facts the sketch read last frame: the pointer only while it is the picture's. *)

    val inspector_subject : t -> Flow.Workspace.path option
    (** The node the inspector shows for the selection, when it is exactly one. *)

    val new_graph : t -> string -> Flow_graph.Flow_edit.op
    (** The [Set_graph] a "New graph" of this context name ([sop], [scene], ...) writes. *)

    val menu_forms : (string * string) list
    (** The language forms of the add menu with the Lisp each is added as. *)

    val lisp_completions : t -> string -> Pxui.Ui.completion list
    (** Completions from the vocabulary currently used by the workspace text panes. *)

    val image_plan : t -> Flow.Eval.plan

    val image_payload :
      ?state:Flow.Eval.state ->
      ?live:Input.t ->
      t ->
      Flow.Eval.value ->
      (Sop.Image.t, Diagnostic.t) result
    (** Inspect the owned plan and request an independent immutable CPU image cook. *)

    val image_stats : t -> int * int

    val image_render_stats : t -> int * int * int * int
    (** Canvases created/destroyed, successful captures and actual GPU pixel readbacks. *)

    val canvas_scenes : t -> Rays.Scene.t list
    (** The picture each canvas pane drew last frame, in layout order. *)

    val image_gpu_stats : t -> Flow_gpu.Host.stats * int * int * int * int
    (** Cumulative Host totals, sinks created/closed and sink buffer/texture creations, including
        evicted runners and failed sinks; inspect outside frame measurements. *)

    val image : t -> Flow.Eval.value -> (Rays.Image.t, Diagnostic.t) result

    val with_images :
      ?state:Flow.Eval.state ->
      ?live:Input.t ->
      ?plan:Flow.Eval.plan ->
      t ->
      (image:(Flow.Eval.value -> (Rays.Image.t, Diagnostic.t) result) ->
      texture:(Flow.Eval.value -> (Rays.Texture.t, Diagnostic.t) result) ->
      'a) ->
      'a
    (** Scoped production image ownership and initial-domain SOP snapshots for qualification. *)

    val host_stats : t -> bool * int * int * int
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
    prepare:(Settings.t -> Sop.Session.output -> (prepared, string) result) ->
    draw:(Sop.Node.t -> prepared -> Rays.Scene.D3.t) ->
    unit ->
    (t, string) result
  (** [factories] is the SOP catalog (default [Sop.Nodes.factories]); passing a non-empty list
      replaces it (prepend custom SOPs to [Sop.Nodes.factories] to extend). [await] (default: false)
      makes each [update] block on the cook it submits, so a fixed-step run or a test sees the
      settled result of every frame instead of racing the worker. [carry_budget] (default 0.5) is
      the seconds a carry's preview may take, to apply or to cook, before it is only described (the
      target is lit, the strip says what a release writes, no picture). [prepare] runs on the cook
      worker domain with submission settings. It must only do pure CPU work on immutable/disjointly
      owned data; SDL, Metal, textures, fonts, audio, UI and runtime caches stay on the initial
      domain. [draw] run on the initial domain. *)

  val update : t -> Rays.Frame.t -> t

  val update_with : t -> Rays.Frame.t -> inspector:(Pxui.Ui.t -> 'a) -> t * 'a option
  (** [update], also building sketch-owned kit widgets below the camera and render sections shown
      while no node is selected. The result is [None] on frames where that panel is not built. *)

  (* Call from [Sketch.run_state ~after_present] when driving the environment
      manually so PNG requests save the completed frame. *)
  val after_present : t -> Rays.Frame.t -> t
  val settings : t -> Settings.t

  val set_settings : t -> Settings.t -> t
  (** Replace the sketch settings from code: one undo step and a fresh cook, since [prepare]
      receives them. Inspector edits do the same. *)

  val can_undo : t -> bool
  (** The workspace keeps one [Editor_core.History] history of the editable document: graph edits
      and inspector commits are entries, continuous slider drags collapse into one, and
      Command/Ctrl-Z, Shift-Command/Ctrl-Z, and Ctrl-Y step it. *)

  val can_redo : t -> bool

  val undo_label : t -> string option
  (** The label of the edit undo would revert ("Repeat", "Connect", ...). *)

  val redo_label : t -> string option

  val workspace : t -> Workspace_doc.t
  (** The document the editor is open on: every document is a workspace. *)

  val probe : t -> Flow.Workspace.path -> int option
  (** The iteration the zone at that path shows (view state, kept by path across edits and reloads,
      never in history). *)

  val set_probe : t -> Flow.Workspace.path -> int -> t

  val node_box : t -> Flow.Workspace.path -> (int * int * int * int) option
  (** The rectangle, in window points, of the node at a path in the graph pane as last laid out
      (tests and tools that click on a node). *)

  val edit : t -> Flow_graph.Flow_edit.op -> (t, string) result
  (** One gesture on the workspace: rewrite the source, re-check, lower into the scene's objects,
      recook, and record one history entry named by the op. An error changes nothing. *)

  val edit_layout : t -> Editor_document.Layout_edit.op -> (t, string) result
  (** {!edit} for a layout gesture ({!Editor_document.Layout_edit}). *)

  val edit_phases : t -> Flow.Phase_timer.t
  (** Phase durations and invocation counts of the last document edit. *)

  val carrying : t -> (string * string) option
  (** The payload in flight (kind and value, [("material", "(ref cobalt)")]), picked up by a press
      on a Navigator row or by [y]: every document change waits for the put, one history entry named
      "Put"; while a target is hot the panels show the edit on a scratch document, and Escape, a
      release over nothing or a focus loss restores the one that was there. *)

  val carry_line : t -> string option
  (** What the status strip says about the carry: the target letters, the preview's words, or the
      checker's reason for a refusal. *)

  val scene : t -> Rays.Frame.t -> Rays.Scene.t
  val close : t -> unit
  val crash_dump : t -> string -> unit

  val document : t -> Editor_document.Kind_graph.t
  (** The open level's objects or World layers; none inside a geometry object. *)

  val network : t -> Sop.Edit_graph.t option
  (** The lowered network of the open geometry object. *)

  type picked = Editor_model.picked = {
    id : int;
    label : string;
    kind : string;
    fields : Param.field_view list;
  }
  (** A scene object or World layer of the list, or the SOP node the graph pane has selected. *)

  val selected_node : t -> picked option
  val displayed_node : t -> Sop.Node.t
  val prepared : t -> Rdk.Geometry.t option

  val camera : t -> Rays.Easy_camera.t
  (** The interactive viewport camera. *)

  val renderer : t -> Renderer.t

  val set_renderer : t -> Renderer.t -> t
  (** Every 3D editor offers raster, wireframe and path tracing in its inspector. [set_renderer]
      applies at the next update. The choice is saved with viewport state, and uses a sketch's
      renderer setting when it supplies one. A [scene/root] in the workspace text names the renderer
      and wins over both. *)

  val render_camera : t -> Rays.Camera.t
  (** The ACTIVE camera object's view and lens (the viewport camera when the document has none). A
      workspace with no scene graph starts with a default camera following the viewport; a scene
      graph is authoritative (an empty one means no camera, and deleting the host's camera writes
      one). With follow viewport on, viewport motion writes the node as view state (no undo entry)
      and node edits or undo move the viewport. PNG export and look-through use it. *)

  type render_settings = { width : int; height : int; max_spp : int }

  val render_settings : t -> render_settings
  (** The scene root's output resolution in pixels (its aspect also frames look-through) and the
      samples per pixel at which a sketch's progressive renderer stops; the defaults when the scene
      graph has no [scene/root]. A viewport over another scene instance with a [scene/root] of its
      own renders at that root's resolution and cap, through that root's camera (see
      {!viewport_camera}). *)

  val film : t -> Rays.Frame.t -> int * int * int * int
  (** The rect inside the view pane (pane-relative) that the render fills: the whole pane, or the
      root's aspect (its resolution, the gate) fitted and centred while looking through it or while
      the path tracer renders. *)

  type slot = { film : int * int; step : int; samples : int; max_spp : int; viewports : int }

  val slot : t -> string -> slot option
  (** The path tracer slot serving the viewport with this key (see {!viewport_camera}), when it has
      one: the size in drawable pixels of its film, the step of the root's resolution that is (1, 2,
      4 or 8: the film is the resolution divided by it), the samples per pixel accumulated, the
      viewport's root's cap, and how many viewports the slot serves. Viewports asking for the same
      scene, camera, film and renderer settings share one slot. *)

  val view_camera : t -> Rays.Camera.t
  (** What the view pane shows this frame, for a sketch's own renderer: [render_camera] while
      looking through it, otherwise the viewport camera with the ACTIVE camera's lens (focused on
      the orbit target unless the camera follows the viewport). With follow viewport off, the view
      orbits freely around the camera object, whose frustum, aim line, and eye marker are drawn over
      the sketch renderer's picture; clicking the eye selects it, and W shows a handle sliding its
      target along the view direction, its focus distance. The Viewport section pairs the
      look-through toggle with "Camera follows viewport" (the ACTIVE camera's own parameter, one
      undo entry), so a fixed camera is set up by looking through it and orbiting. *)

  val viewport_camera : t -> string -> Rays.Camera.t
  (** What the viewport with this key (editor graph/binding for unique named panels, placement for
      inline/looped/repeated panels, ["main"] for the default layout) shows: each viewport keeps its
      own orbit, a copy of the focused one's at the first focus, and the wheel, drag and fly of the
      focused viewport move only it. Looking through, a viewport over a scene instance with a
      [scene/root] sees through the camera that root names; any other sees through {!render_camera}.
  *)

  val flying : t -> bool
  (** [/ w] with the view focused: held W/S/A/D/Q/E fly the viewport camera ([Easy_camera.fly]) with
      the pointer captured through [Sketch.set_relative_mouse]. Escape or focus loss exits; Space or
      [/] exits and opens the leader. *)

  val look_through : t -> bool
  (** The view shows [render_camera] ([/ v], or the Camera panel toggle); orbit input is frozen
      unless the active camera follows the viewport. *)

  val timeline : t -> Timeline.t
  val panes : t -> Rays.Frame.t -> Pxui_shell.Layout.panes

  (** {2 Scene}

      The document is a scene of objects above the SOP networks. The code [graph] is the geometry
      object [geo1], and [?world] the World. The graph pane shows the open level as a graph or a
      list ([/ l]): [i], a double-click, or activating a list row enters a geometry object's SOP
      network or the World's layer stack, and [u] goes back up; [/ e] opens the World (created on
      first use), and [/ a] opens the add menu of the open level (objects, SOPs, or World layers):
      hover a category for its submenu, or type to search. Object transforms place each object's
      cook as a whole, so moving one never re-cooks SOPs. [graph], [document], [prepared], and
      [displayed_node] describe the open (else the first) geometry object. *)

  val level : t -> string option
  (** The object whose network is open; [None] at the scene level. *)

  val scene_document : t -> Editor_document.Kind_graph.t
  (** The scene's objects: an object's input is its parent, its values the transform. *)

  val objects : t -> (Rays_math.Mat4.t * prepared) list
  (** Visible, renderable geometry objects' latest cooks at their world transforms, e.g. for a
      sketch's own path tracer. *)

  val lights : t -> Rays.Light.t list
  (** Visible, renderable light objects in world space. *)

  val world : t -> Rays.World.baked option
  (** This frame's World bake (preview size during a gesture or playback), or [None] without a
      World. *)

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
    prepare:(Settings.t -> Sop.Session.output -> (prepared, string) result) ->
    draw:(Sop.Node.t -> prepared -> Rays.Scene.D3.t) ->
    unit ->
    (unit, Rays.Sketch.error) result
end

(** The frame reducer without a UI frame: an unstable hook for tests, like {!Private}.
    [step e actions frame] reduces one frame in which no pane was built: the keys are [actions], the
    list's selection is [select] (scene object ids), and [preview] puts a payload in flight whose
    hot target shows that edit on a scratch document, as a carry does. *)
module Reduce : sig
  val open_import : Editor.t -> string -> Editor.t
  val spreadsheet : Editor.t -> int list -> int * Rdk.Geometry.t option

  val select_path : Editor.t -> string list -> Editor.t
  (** Select the checked lexical path; unstable test/diagnostic hook. *)

  val view : Editor.t -> string list -> Editor.t
  (** [v] on the checked lexical path, as the graph pane requests it; unstable test hook. *)

  val step :
    Editor.t ->
    ?select:int list ->
    ?preview:Flow_graph.Flow_edit.op ->
    Editor_commands.action list ->
    Rays.Frame.t ->
    Editor.t
end

(** A [.rays] sketch as a program. Command-S rewrites the file when it is still the text the
    document came from (comments intact), else saves a preset; an edit of the file from any editor
    reloads the running document as one history entry, "Reload <file>", and a file that does not
    check leaves the last good document with the diagnostics in the status bar and the text pane. *)
module Workspace : sig
  type process = {
    max_frames : int option;
    crash_dir : string option;
    profile : bool;
    preferences : string option;
    ui_font : string option;
  }
  (** What the process environment says, parsed once by the executable (bin/rays.ml); the libraries
      never read it. [no_process] is all defaults. *)

  val no_process : process

  type source = { path : string; digest : string }
  (** Where a workspace came from: [path] relative to the project root, [digest] the SHA-256 of its
      text. *)

  val load :
    ?ops:Flow.Op.t list ->
    ?imports:(string * string) list ->
    ?factories:Sop.Edit_graph.factory list ->
    string ->
    (Workspace_doc.t, Diagnostic.t list) result
  (** Parse and check a [.rays] text against {!workspace_catalog}. *)

  val export :
    ?graph:string ->
    ?fps:int ->
    directory:string ->
    frames:int ->
    Workspace_doc.t ->
    (unit, Diagnostic.t) result
  (** Export a draw graph (the first by default) through [Sketch.export_state]. Each invocation
      starts a fresh fold and pins every frame fact. *)

  val sop_graphs :
    ?factories:Sop.Edit_graph.factory list ->
    Workspace_doc.t ->
    ((string * Sop.Node.t) list, string) result
  (** Every [sop] graph of the document with its result compiled, by name, for cooking without an
      editor (a check, a batch). A graph without a result node is left out. *)

  val declared_camera :
    ?factories:Sop.Edit_graph.factory list ->
    Workspace_doc.t ->
    Rays.Easy_camera.t ->
    Rays.Easy_camera.t
  (** The camera of the scene's first camera node, or the default base camera. *)

  val main : ?text:string -> ?process:process -> string -> (unit, Diagnostic.t list) result
  (** [main path] opens the [.rays] file at [path] and its imports beside it in the editor
      ([rays FILE.rays]); [text] opens that text instead, saved to [path] (the empty editor). On a
      failure it returns diagnostics without printing or exiting. *)
end
