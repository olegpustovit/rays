(* Private editor environment interface. *)
open Loop_view
module V = Viewport

type layout = Pxui_shell.Layout.t

type t = {
  core : Editor_model.t;
  camera : V.camera;
  cameras : (string * V.camera) list;
  viewing : string option;
  control : V.control;
  draw : Sop.Node.t -> Cook.prepared -> V.rendered;
  rendered : V.rendered option;
  views : (string * V.rendered) list;
  drawn :
    (Sop.Node.t * Cook.prepared * V.rendered * Rays.Texture.t option)
    Editor_document.Document.Int_map.t;
  composed :
    (Editor_document.Kind_graph.t * Editor_document.Document.level * (string * int list) list)
    option;
  resolved : (Editor_document.Document.t * Input.t * Editor_document.Kind_graph.t) option;
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
  prepare:(Editor_document.Settings.t -> Sop.Session.output -> (Cook.prepared, string) result) ->
  draw:(Sop.Node.t -> Cook.prepared -> V.rendered) ->
  unit ->
  (t, string) result

val document : t -> Editor_document.Kind_graph.t
val network : t -> Sop.Edit_graph.t option
val prepared : t -> Rdk.Geometry.t option
val camera : t -> V.camera
val extra : t -> V.extra
val timeline : t -> Timeline.t
val selected_node : t -> Editor_model.picked option
val displayed_node : t -> Sop.Node.t
val panes : t -> Rays.Frame.t -> Pxui_shell.Layout.panes
val can_undo : t -> bool
val can_redo : t -> bool
val undo_label : t -> string option
val redo_label : t -> string option
val workspace : t -> Editor_document.Workspace_doc.t
val probe : t -> Pxui_graph.Scope.path -> int option
val set_probe : t -> Pxui_graph.Scope.path -> int -> t
val node_box : t -> Pxui_graph.Scope.path -> (int * int * int * int) option
val edit : t -> Flow_graph.Flow_edit.op -> (t, string) result
val edit_layout : t -> Editor_document.Layout_edit.op -> (t, string) result
val edit_phases : t -> Flow.Phase_timer.t
val carrying : t -> (string * string) option
val carry_line : t -> string option
val level : t -> string option
val scene_document : t -> Editor_document.Kind_graph.t
val same_context : Editor_document.Document.t -> Editor_document.Document.t -> bool
val lights : t -> Rays.Light.t list
val world : t -> Rays.World.baked option
val objects : t -> (Rays_math.Mat4.t * Cook.prepared) list
val settings : t -> Editor_document.Settings.t
val set_settings : t -> Editor_document.Settings.t -> t
val focus_key : t -> string
val look_key : t -> string -> string
val camera_of : t -> string -> V.view
val view_camera : t -> V.view
val follow_focus : t -> Rays.Frame.t -> t

val paint_view :
  t -> key:string -> int * int * int * int -> V.view -> V.rendered -> Rays.Scene.node list

val viewport_camera : t -> string -> V.view
val focused_image : t -> V.rendered option
val film : t -> Rays.Frame.t -> int * int * int * int
val refresh_hidden : t -> Rays.Frame.t -> t

val composition_key :
  Editor_model.t -> 'b -> 'b * Editor_document.Document.level * (string * int list) list

val display_keys : ('a * 'b * V.rendered * 'c) Editor_document.Document.Int_map.t -> string list
(** The device display keys the drawn scenes draw ({!Flow_native.Workspace_gpu.frame}'s [keep]). *)

val compose :
  t ->
  'b Editor_model.update ->
  baked:Rays.World.baked option ->
  baked_views:(string * (Rays.World.t * Rays.World.baked)) list ->
  scene:Editor_document.Kind_graph.t ->
  V.rendered option
  * (string * V.rendered) list
  * (Sop.Node.t * Cook.prepared * V.rendered * Rays.Texture.t option)
    Editor_document.Document.Int_map.t
  * string list
