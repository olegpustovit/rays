(* Private editor environment interface. *)
open Environment_view

module V = Viewport
type layout = Pxui_shell.Layout.t
type 'prepared t = {
  core : 'prepared Core_model.t;
  camera : V.camera;
  cameras : (string * V.camera) list;
  viewing : string option;
  control : V.control;
  draw : Sop.Node.t -> 'prepared -> V.rendered;
  rendered : V.rendered option;
  views : (string * V.rendered) list;
  drawn :
    (Sop.Node.t * 'prepared * V.rendered * Rays.Texture.t option)
    Editor_document.Document.Int_map.t;
  composed :
    (Sop.Edit_graph.t * Editor_document.Document.level *
     (string * int list) list)
    option;
  resolved :
    (Editor_document.Document.t * Input.t * Sop.Edit_graph.t) option;
  context_error : string option;
  baked : Rays.World.baked option;
  baked_from : (Rays.World.t * Rays.World.baked) option;
  baked_views : (string * (Rays.World.t * Rays.World.baked)) list;
  map : (Rays.World.baked * Image.t) option;
  render_status : string option;
  pending_render : V.request option;
  background : Rays_math.Color.t;
  extra : V.extra;
  hidden_scene_cache : (V.rendered, V.view) hidden_scene_cache option;
  world_drag : World_map.world_drag option;
  pick_press : (float * float) option;
  source : Source_file.t option;
  held : string option;
  open_dialog : int option;
  refused : string option;
  opened : Editor_document.Document.t;
  state_owned : bool;
  state_checked : float;
  saved_doc : Editor_document.Document.t;
  saved_view : Flow.Syntax.t;
  state_error : string option;
  canvases : (string * canvas_picture) list;
  host : Flow_native.Workspace_host.t;
}
val create :
  ?inputs:(string * (string * Flow.Eval.value) list) list ->
  ?name:string ->
  ?presets:string ->
  ?factories:Sop.Edit_graph.factory list ->
  ?gpu_owner:Flow_native.Workspace_gpu.t ->
  ?settings:Editor_document.Settings.t ->
  ?lights:Rays.Light.t list ->
  ?camera:V.camera ->
  ?background:Rays_math.Color.t ->
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
  ?opened:Editor_document.Document.t ->
  workspace:Editor_document.Workspace_doc.t ->
  ?source:Source_file.t ->
  prepare:(Editor_document.Settings.t ->
           Sop.Session.output -> ('a, string) result) ->
  draw:(Sop.Node.t -> 'a -> V.rendered) ->
  unit -> ('a t, string) result
val document : 'a t -> Sop.Edit_graph.t
val prepared : 'a t -> 'a option
val camera : 'a t -> V.camera
val extra : 'a t -> V.extra
val timeline : 'a t -> Timeline.t
val selected_node : 'a t -> Sop.Node.t option
val displayed_node : 'a t -> Sop.Node.t
val panes : 'a t -> Rays.Frame.t -> Pxui_shell.Layout.panes
val can_undo : 'a t -> bool
val can_redo : 'a t -> bool
val undo_label : 'a t -> string option
val redo_label : 'a t -> string option
val workspace : 'a t -> Editor_document.Workspace_doc.t
val probe : 'a t -> Pxui_graph.Scope.path -> int option
val set_probe : 'a t -> Pxui_graph.Scope.path -> int -> 'a t
val node_box :
  'a t -> Pxui_graph.Scope.path -> (int * int * int * int) option
val edit : 'a t -> Flow_graph.Flow_edit.op -> ('a t, string) result
val edit_phases : 'a t -> Flow.Phase_timer.t
val carrying : 'a t -> (string * string) option
val carry_line : 'a t -> string option
val level : 'a t -> string option
val scene_document : 'a t -> Sop.Edit_graph.t
val same_context :
  Editor_document.Document.t -> Editor_document.Document.t -> bool
val lights : 'a t -> Rays.Light.t list
val world : 'a t -> Rays.World.baked option
val objects : 'a t -> (Rays_math.Mat4.t * 'a) list
val settings : 'a t -> Editor_document.Settings.t
val set_settings : 'a t -> Editor_document.Settings.t -> 'a t
val focus_key : 'a t -> string
val look_key : 'a t -> string -> string
val camera_of : 'a t -> string -> V.view
val view_camera : 'a t -> V.view
val follow_focus : 'a t -> Rays.Frame.t -> 'a t
val paint_view :
  'a t ->
  key:string ->
  int * int * int * int -> V.view -> V.rendered -> Rays.Scene.node list
val viewport_camera : 'a t -> string -> V.view
val focused_image : 'a t -> V.rendered option
val film : 'a t -> Rays.Frame.t -> int * int * int * int
val refresh_hidden : 'a t -> Rays.Frame.t -> 'a t
val composition_key :
  'a Core_model.t ->
  'b -> 'b * Editor_document.Document.level * (string * int list) list
val compose :
  'a t ->
  ('a, 'b) Core_model.update ->
  baked:Rays.World.baked option ->
  baked_views:(string * (Rays.World.t * Rays.World.baked)) list ->
  scene:Sop.Edit_graph.t ->
  V.rendered option * (string * V.rendered) list *
  (Sop.Node.t * 'a * V.rendered * Rays.Texture.t option)
  Editor_document.Document.Int_map.t * string list
