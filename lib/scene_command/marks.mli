(** The run-time drawing contract ([specification/marks.md]): one ordered table
    of entries, pure data with no dependencies. *)

(** Reference to a per-frame table the caller owns: [count] elements of
    [width] floats each. [id] must be positive. *)
type buffer = { id : int; count : int; width : int }

(** [Device] is a per-frame GPU array the caller owns, laid out as a [Buffer] of the same
    [count] and [width] (float32 elements). A [Device] colour is float r, g, b in 0..1 like a
    colour [Buffer] (width 3), opaque. *)
type 'a attr = Constant of 'a | Buffer of buffer | Device of buffer

(** Colours are packed [0xRRGGBBAA]; floats are logical points. *)
type kind =
  | Rect of { position : (float * float) attr; size : (float * float) attr;
              fill : int32 attr; stroke : int32 attr; stroke_width : float attr }
  | Circle of { position : (float * float) attr; radius : float attr;
                fill : int32 attr; stroke : int32 attr; stroke_width : float attr }
  | Line of { position : (float * float) attr; end_ : (float * float) attr;
              stroke : int32 attr; stroke_width : float attr }
  | Point of { position : (float * float) attr; fill : int32 }

type entry =
  | Mark_set of { count : int; kind : kind }
  | Geometry of { vertices : float array; indices : int array; color : int32 }
  | Image of { resource : int; x : int; y : int; scale : float; angle : float }
  | Text of { x : int; y : int; size : int; color : int32; text : string }
  | Push_transform of { xx : float; xy : float; yx : float; yy : float;
                        tx : float; ty : float }
  | Pop_transform
  | Push_clip of { x : float; y : float; width : float; height : float }
  | Pop_clip

(** Painter order, first entry drawn first. *)
type t = entry array

type gpu_token

val gpu_token : identity:int -> count:int -> stamp:int64 -> gpu_token
val gpu_identity : gpu_token -> int
val gpu_count : gpu_token -> int
val gpu_stamp : gpu_token -> int64

val max_count : int
val max_entries : int

(** The first violated rule of the contract, as text. *)
val validate : t -> (unit, string) result

(** Stable, versioned ("RMK1"), explicitly little-endian encoding. *)
val serialize : t -> bytes
