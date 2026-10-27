type t
type output
val create : Renderer.gpu -> Pipelines.t -> Emit.msl -> t
val dispatch : ?wait:bool -> ?reset_status:bool -> ?commands:Ogpu.Backend.commands ->
  ?count:(Ogpu.Backend.buffer * int * int) -> ?into:(Ogpu.Backend.buffer * Ogpu.Backend.buffer) -> ?bound:(int -> Ogpu.Backend.buffer option) ->
  ?bound_table:(int -> Ogpu.Backend.buffer option) -> t -> Flow_ir.Packed.Private.inputs ->
  (output, Diagnostic.t) result
(** [~wait:false] leaves the dispatch in flight, without its status read; [~commands] only encodes it
    into the caller's command buffer (epoch 0 until the caller commits it); [~reset_status:false]
    keeps ORed error flags from earlier encodings on this runner. [~count:(cell, mul, div)]
    bounds a fast kernel's count by [cell * mul / div] on the device; [~into:(output, cell)] writes a
    one-output fast kernel's output and count into the caller's buffers; [bound i] and
    [bound_table u] supply device buffers already holding input [i] or uniform [u]'s table.
    Input arrays are borrowed immutable storage. Replace an array when its
    contents change; mutating an uploaded array is unsupported. Each input
    slot retains only its last successfully uploaded array and covered length,
    until buffer replacement or close. Frame/uniform/status writes remain live. *)

val select_output : index:int -> output -> output option
(** Select a fused producer's output; an invalid index returns [None]. The
    selected view shares the producer's lifetime and dispatch generation. *)

(** [index] overrides the selected output (initially the first). *)
val readback : ?index:int -> output -> (Flow.Eval.value, Diagnostic.t) result
val buffer : ?index:int -> output -> Ogpu.Backend.buffer option
val count_record : output -> Ogpu.Backend.buffer option
(* A fast kernel's 24-byte count record {n, 6n, 1, 0, 0, 0} (see {!Emit.msl}); [None] for an exact
   kernel or a superseded output. *)
val count : output -> int
val epoch : output -> int64
val width : output -> int
val gpu_seconds : output -> float option
val close : t -> unit
val compact : Ogpu.Backend.commands -> Pipelines.t -> mask:Ogpu.Backend.buffer -> count:int ->
  count_in:Ogpu.Backend.buffer option -> kept:Ogpu.Backend.buffer -> total:Ogpu.Backend.buffer ->
  (unit, Diagnostic.t) result
(* Encodes into [commands] the indices of [mask]'s first elements that are nonzero, as floats into
   [kept], and their count into the cell [total]. The mask's count is [count], or at most the uint
   in [count_in]. Nothing is committed or read. *)
val read_count : Ogpu.Backend.queue -> Ogpu.Backend.buffer -> epoch:int64 -> (int, Diagnostic.t) result
val read_floats : Ogpu.Backend.queue -> Ogpu.Backend.buffer -> epoch:int64 -> length:int ->
  (float array, Diagnostic.t) result
(* [length] floats of a buffer, once the queue has completed [epoch]. *)
module Private : sig
  val diagnostic : Ogpu.Error.t -> Diagnostic.t
  val buffer_creations : t -> int
  val input_uploads : t -> int
  val input_uploaded_bytes : t -> int
  val status_reads : t -> int
  val check_status : t -> epoch:int64 -> (unit, Diagnostic.t) result
  val readback_bytes : t -> int
  val create_owned : Ogpu.Backend.device -> Ogpu.Backend.queue -> Pipelines.t -> Emit.msl -> t
end
