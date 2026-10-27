(** Packed, renderer-neutral UI instance lists.

    One instance is sixteen little-endian 32-bit words (64 bytes) drawn as a
    single screen-aligned quad by the native UI pipeline:

    {v
    word  rect           textured       wire segment    grid
    0-3   x0 y0 x1 y1    x0 y0 x1 y1    p0.x p0.y p1.x p1.y   x0 y0 x1 y1
    4-7   -              u0 v0 u1 v1    p2.x p2.y p3.x p3.y   ox oy step dot
    8     fill rgba      tint rgba      rgba            dot rgba
    9     border rgba    -              -               -
    10    kind (u32)
    11    radius         -              -               -
    12    border width   -              width           -
    13    anti-alias     -              -               -
    14-15 -              -              t0 t1           -
    v}

    Textured instances carry texel coordinates, normalized by the bound
    texture's size when sampled, so a texture may grow without invalidating
    earlier instances. Colors use Rays's packed [0xRRGGBBAA]. Coordinates are logical points
    in screen space; a batch clips them to [clip]. A radius-0, non-anti-aliased rect
    covers exactly the pixels a filled or 1-point-stroked triangle rectangle
    covers under the top-left rule, at any integer density. *)

type kind = Rect | Textured | Wire | Grid

type clip = { x : float; y : float; width : float; height : float }

(** Instances from [first] up to (excluding) [first + count] share one clip and texture
    resource ([0] when the batch samples no texture). *)
type batch = {
  first : int;
  count : int;
  clip : clip option;
  texture : int;
}

type t
val marks : t -> Marks.gpu_token option

val instance_bytes : int
val instances : t -> bytes
(** Read-only view of the packed instances. Do not mutate. *)

val count : t -> int
val batches : t -> batch array
val textures : t -> int list
val empty : t

val float : t -> instance:int -> word:int -> float

module Builder : sig
  type batch_table = t
  type t

  val create : ?capacity:int -> unit -> t
  val reset : t -> unit

  val set_clip : t -> clip option -> unit

  val rect :
    t -> x:float -> y:float -> width:float -> height:float ->
    ?color:int32 -> ?border_color:int32 -> ?border:float -> ?radius:float ->
    ?anti_alias:bool -> unit -> unit
  (** Hard coverage by default; [radius > 0] implies anti-aliasing. *)

  val textured :
    t -> texture:int -> x:float -> y:float -> width:float -> height:float ->
    u0:float -> v0:float -> u1:float -> v1:float -> color:int32 -> unit

  val wire :
    t -> float * float -> float * float -> float * float -> float * float ->
    width:float -> color:int32 -> unit
  (** Cubic Bézier, split into a length-dependent number of instanced
      segments that the GPU evaluates and strokes with round joins. *)

  val grid :
    t -> x:float -> y:float -> width:float -> height:float ->
    origin_x:float -> origin_y:float -> spacing:float -> dot:float ->
    color:int32 -> unit
  (** One quad whose fragment stage draws a dot every [spacing] points. *)

  val shape_rect : t -> x:float -> y:float -> width:float -> height:float ->
    fill:int32 -> stroke:int32 -> unit -> unit

  val circle : t -> x:float -> y:float -> radius:float ->
    ?fill:int32 -> ?stroke:int32 -> ?stroke_width:float -> unit -> unit
  val line : t -> x0:float -> y0:float -> x1:float -> y1:float ->
    color:int32 -> width:float -> unit
  type floats = float array * int
  val circles : t -> count:int -> position:floats -> radius:floats ->
    fill:int32 -> stroke:int32 -> stroke_width:floats -> unit
  val rects : t -> count:int -> position:floats -> size:floats ->
    fill:int32 -> stroke:int32 -> unit
  val lines : t -> count:int -> from:floats -> to_:floats ->
    color:int32 -> width:floats -> unit

  val publish : t -> batch_table
  (** Close the open batch and copy every instance into an immutable table.
      Call [reset] before building the next frame. *)
end

module Private : sig
  val copy : t -> t
  val valid_shapes : t -> bool
  val of_marks : Marks.gpu_token -> t
  val marks_identity : Marks.gpu_token -> int
  val marks_count : Marks.gpu_token -> int
  val marks_stamp : Marks.gpu_token -> int64
end
