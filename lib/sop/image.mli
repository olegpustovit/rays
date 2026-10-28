type t
(** Immutable, renderer-independent row-major images, backed by float samples in [0,1] or owned
    RGBA8 bytes. *)

val create : width:int -> height:int -> rgba:float array -> (t, Diagnostic.t) result
val width : t -> int
val height : t -> int
val data_id : t -> int
val payload_bytes : t -> int

val storage : t -> float array
(** Borrowed immutable float storage, or an explicitly materialized float array for an RGBA8 image.
    No materialized copy is retained by the image. *)

val rgba8 : t -> bytes option
(** Borrowed immutable bytes when the image has RGBA8 storage. *)

val of_owned_rgba8 : width:int -> height:int -> bytes -> (t, Diagnostic.t) result
(** Transfers sole ownership after checking dimensions and byte cardinality. *)

val of_owned_rgba : width:int -> height:int -> float array -> (t, Diagnostic.t) result
(** Transfers sole ownership after validating dimensions and samples. *)

val of_vec4 :
  context:Context.t -> width:int -> height:int -> float array -> (t, Diagnostic.t) result
(** Convert finite packed channels to one owned RGBA8 buffer: clamp to [0,1], scale by 255, round
    ties to even. Uses disjoint 16,384-pixel chunks in the caller's shared pool; cancellation is
    checked between chunks. *)
