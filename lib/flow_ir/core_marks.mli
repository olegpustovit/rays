(** Execution of draw statements into the mark table ([specification/marks.md]). Every check of a
    value and every conversion is drawing.ml's, so the table encodes the same picture. *)
module Buffers : sig
  type t
  (** The per-frame table of CPU attribute arrays that mark sets refer to by id. The caller creates
      it, reads from it while it adapts the marks, and clears it each frame. *)

  val create : unit -> t
  val clear : t -> unit

  val find : t -> int -> float array
  (** The data of a buffer id of the current frame; [Not_found] otherwise. *)

  val device : t -> int -> int * int * int
  (** A [Device] buffer id of the current frame: the host's [token] (see
      {!Core_marks.device_value}), the element [count] and the [width]. [Not_found] otherwise. *)
end

type sink

val sink : ?image:(Flow.Eval.value -> int) -> size:int * int -> Buffers.t -> sink
(** [size] is the canvas (for [draw/background]); [image] resolves an image node to a host resource
    id (positive) or raises [Diagnostic.Error]. *)

val is_device : Flow.Eval.value -> bool
(** A [device_value]: it names an entry of one draw's buffers. *)

val device_value : sink -> token:int -> count:int -> width:int -> Flow.Eval.value
(** The placeholder for a region the host ran on the GPU. [token] is the host's own handle, returned
    by [Buffers.device]. Only a positions, sizes, radius, from or to attribute of a mark set may
    consume it ({!Core_exec.draw}); the set then carries it as a [Device] buffer. *)

val compile : Core.draw -> Flow.Eval.value array -> sink -> unit
(** The statement, resolved once; run it on the locals. *)

val finish : sink -> Scene_command.Marks.t
(** The entries in painter order, checked by [Marks.validate] ([E_DRAW_RANGE] otherwise). *)
