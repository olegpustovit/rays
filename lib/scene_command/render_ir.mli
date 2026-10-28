(** Immutable renderer-neutral command streams. *)
type blend = Source_over | Copy | Replace | Alpha | Add | Multiply | Screen | Subtract

type rect = { x : float; y : float; width : float; height : float }
type transform = { xx : float; xy : float; yx : float; yy : float; tx : float; ty : float }
type geometry = { vertices : float array; indices : int array; color : int32 }
type image = { resource_id : int; source : rect; destination : rect }
type glyph = { glyph_id : int; x : float; y : float }
type glyphs = { resource_id : int; color : int32; glyphs : glyph array }

type command =
  | Clear of int32
  | Set_blend of blend
  | Push_clip of rect
  | Pop_clip
  | Push_transform of transform
  | Pop_transform
  | Geometry of geometry
  | Shapes of Ui_batch.t
  | Image of image
  | Glyphs of glyphs

type t

type error =
  | Non_finite
  | Invalid_extent
  | Invalid_cardinality
  | Invalid_index of int
  | Invalid_resource_id of int
  | Invalid_glyph_id of int
  | Unbalanced_clip
  | Unbalanced_transform
  | Complexity_limit

val create : command array -> (t, error) result

(* Unique identity assigned to each validated immutable IR at construction. *)
val identity : t -> int

val commands_readonly : t -> command array
(** Borrowed validated command storage for synchronous audited consumers. The returned array and
    every nested payload must be treated as read-only and must not escape the dynamic extent of the
    consuming operation. *)

(* Validates and takes ownership of a freshly allocated command graph. The
   caller must retain no mutable aliases to arrays nested in [input]. *)
val create_owned : command array -> (t, error) result

(* Stable, explicitly little-endian, versioned encoding. *)
val serialize : t -> bytes
