type node type t=node list type blend=Replace|Alpha|Add|Multiply
val empty:t  val group:t->node val clear:Color.t->node
val point : at:(int*int) -> ?color:Color.t -> unit -> node
val line : from_:(int*int) -> to_:(int*int) -> ?color:Color.t -> ?width:int -> unit -> node
val rect : at:(int*int) -> w:int -> h:int -> ?fill:Color.t -> ?stroke:Color.t -> unit -> node
val rounded_rect : at:(int*int) -> w:int -> h:int -> radius:int -> ?fill:Color.t -> ?stroke:Color.t -> unit -> node
val circle : at:(int*int) -> radius:int -> ?fill:Color.t -> ?stroke:Color.t -> unit -> node
val polygon : (int*int) list -> ?fill:Color.t -> ?stroke:Color.t -> unit -> node
val polyline : (int*int) list -> ?color:Color.t -> unit -> node
val path : ?fill:Color.t -> Path.t -> node
val text : at:(int*int) -> ?color:Color.t -> ?size:int -> string -> node
val image : Image.t -> at:(int*int) -> ?scale:float -> ?angle:float -> unit -> node
val view3d : ?viewport:(int*int*int*int) -> camera:Camera.t -> Scene3.t -> node

(** Pure IME metadata in logical points; [cursor] is the non-negative caret
    offset from the region's left edge. *)
val text_input_region : at:(int*int) -> w:int -> h:int -> ?focused:bool -> ?cursor:int -> unit -> node
val translate : int -> int -> t -> node
val rotate : float -> t -> node
val scale : float -> float -> t -> node
val clip : at:(int*int) -> w:int -> h:int -> t -> node
val render : t -> unit
module Private : sig
  (** The same picture: what the nodes draw, ignoring render caches; resources and images
      compare by identity.  A sketch window presents only a picture that is not the same. *)
  val same : t -> t -> bool
  val shapes : Scene_command.Shape_batch.t -> node
  (* Packed primitives share the enclosing Scene transform, clip and blend. *)
  val geometry : Scene_command.Render_ir.geometry -> node
  (* Indexed path geometry as the shared tessellation produced it. *)
  val transform : Scene_command.Render_ir.transform -> t -> node
  (* An exact affine view transform: the six coefficients are the command's own. *)
  module Ui_batch = Scene_command.Ui_batch
  val layer_break : node

  val display_list : ?images:(int * Image.t) list ->
    Scene_command.Display_list.t -> node
  (** A retained packed 2D segment; public code uses {!Ink}. *)

  val ui : ?images:(int * Image.t) list -> Ui_batch.t -> node
  (** A native-only PXUI instance layer. Like [view3d], it ignores enclosing
      Scene transforms and clips: the batch carries its own. Every batch
      texture id must be bound in [images]. *)

  type native_layer =
    | Scene2_layer of Scene_command.Render_ir.t *
        (int * Rays_execution.resource) list
    | Scene2_segment of Scene_command.Display_list.t *
        (int * Rays_execution.resource) list
    | Scene3_layer of Scene_execution.prepared_scene3
    | Ui_layer of Ui_batch.t *
        (int * Rays_execution.resource) list
  type staged_native = {
    clear : float * float * float * float;
    scene2 : Scene_command.Render_ir.t;
    resources : (int * Rays_execution.resource) list;
    scene3 : Scene_execution.prepared_scene3 list;
    layers : native_layer list;
    mesh_images : Runtime_resources.Image.t list;
    retained : (string * int64) option;
  }
  val native_segment_version : Scene_command.Display_list.t ->
    (int * Rays_execution.resource) list -> int64
  val stage_native : width:int -> height:int -> t -> (staged_native,string) result
  val stage_native_render : width:int -> height:int -> t ->
    (staged_native,string) result
  type native_error = Message of string | Resource of Runtime_resources.error
  val pp_native_error : Format.formatter -> native_error -> unit
  val stage_native_render_checked : ?density:int -> width:int -> height:int -> t ->
    (staged_native,native_error) result
  val to_ir : t -> (Scene_command.Render_ir.t,string) result
  val commands : ?density:int -> t -> Scene_command.Render_ir.command array
  val stage : width:int -> height:int -> t ->
    (Scene_command.Render_ir.t * (int * Rays_execution.resource) list, string) result
  val install_renderer : (t -> unit) -> unit
  val text_regions : t -> (int*int*int*int*bool*int) list
  val release : t -> unit
end
