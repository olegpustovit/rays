(* Private editor environment interface. *)
module Image = Rays.Image
val set_ui_cursor : Pxui.Ui.t -> bool -> (unit, string) result
type ('rendered, 'camera) hidden_scene_cache = {
  width : int;
  height : int;
  rendered : 'rendered option;
  camera : 'camera;
  background : Rays_math.Color.t;
  view_visible : bool;
  scene : Rays.Scene.t;
}
type canvas_picture = {
  size : int * int;
  dynamic : bool;
  scene : Rays.Scene.t;
  plan : Flow.Eval.plan;
  prepared : Flow_native.Drawing.prepared option;
  preview : (Flow.Ty.t * int) option;
}
val bake_world :
  ?view:[> `Only of string | `Primary ] ->
  previous:(Rays.World.t * Rays.World.baked) option ->
  'a Editor_model.t -> live:bool -> (Rays.World.t * Rays.World.baked) option
val view_keys : 'a Editor_model.t -> string list
val save_status :
  save:('a -> (unit, string) result) ->
  filename:('a -> string) -> 'a option -> 'b option -> string option
val hidden_only : ui_visible:bool -> 'a Editor_model.t -> bool
val hidden_entry :
  background:Rays_math.Color.t ->
  rendered:'a option ->
  camera:'b ->
  paint_view:(key:string ->
              int * int * int * int -> 'b -> 'a -> Rays.Scene.node list) ->
  cache:('a, 'b) hidden_scene_cache option ->
  'c Editor_model.t -> Rays.Frame.t -> ('a, 'b) hidden_scene_cache
val compose_view :
  ?map:Image.t ->
  canvases:(string * canvas_picture) list ->
  ui_visible:bool ->
  background:Rays_math.Color.t ->
  rendered:'a option ->
  focused:'a option ->
  views:(string * 'a) list ->
  camera:'b ->
  camera_of:(string -> 'b) ->
  paint_view:(key:string ->
              Pxui_shell.Layout.bounds -> 'b -> 'a -> Rays.Scene.node list) ->
  guides:(Pxui_shell.Layout.bounds -> Rays.Scene.node list) ->
  cache:('a, 'b) hidden_scene_cache option ->
  'c Editor_model.t -> Rays.Frame.t -> Rays.Scene.t
