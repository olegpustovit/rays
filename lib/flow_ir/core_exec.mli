(** Initial-domain execution of the structured frame core, using native primitive
    bodies. State statements need [root], the residual whose state plane they use.
    A region runs through [force] (default [Packed.force]). With [root], failures
    restore evaluation state. [~strict:true] or a custom [resolve] returns the
    compiled diagnostic; otherwise the tree walker reruns that residual to
    supply its own diagnostic and span. *)
type prepared
(** A program with its statements, constants and operators resolved once. *)

val footprint : Core.program -> Core.statement -> (int array * int array) option
(** A drawing's loop memory: the locals a statement reads from outside itself and the locals it
    writes, or [None] when it reads the frame, state, a host or a random stream, draws, or calls a
    region with hidden inputs. [prepare ~remember_loops] keeps a top-level loop's or region call's
    last 4 results by these reads; the web printer emits the same memory ({!Flow_web.Js}). *)

val resident_live : Core.program -> Core.statement -> int array option
(** A top-level loop a resident fold may keep on the device (its body holds only regions, scalars,
    ifs, compaction, retyping and counts, and it has a {!footprint}): the writes later statements
    read, which its end reads back; [None] for any other statement. The web printer uses the same
    rule. *)

val prepare : ?fused:bool -> ?remember_loops:bool -> Core.program -> prepared
(* [~fused:false] runs every program through the general statements: the reference for the
   scalar fast path and pure packed Counted block kernels. [~remember_loops:true] (a drawing) keeps
   each top-level loop's last 4 results keyed by the values it reads from outside itself, and skips
   it when they repeat; a loop that reads the frame, state, a host or a random stream, or draws, always
   runs. *)
val has_regions : prepared -> bool  (* the program invokes a region, so [execute] needs a [force] *)
val fast : prepared -> (Frame_input.t -> (Flow.Eval.value, Flow.Diagnostic.t) result) option
(* Stateless attempt for the evaluator's owner-checked shortcut. Errors must reenter the
    normal evaluator to restore the walker's diagnostics. *)
val execute : ?root:Flow.Eval.residual -> ?strict:bool ->
  ?force:(Packed.t -> (Flow.Eval.value, Flow.Diagnostic.t) result) ->
  ?resolve:(Flow.Eval.value -> (Flow.Eval.value, Flow.Diagnostic.t) result) -> prepared -> live:Frame_input.t ->
  (Flow.Eval.value, Flow.Diagnostic.t) result
val run : ?root:Flow.Eval.residual -> ?strict:bool ->
  ?force:(Packed.t -> (Flow.Eval.value, Flow.Diagnostic.t) result) ->
  ?resolve:(Flow.Eval.value -> (Flow.Eval.value, Flow.Diagnostic.t) result) -> Core.program -> live:Frame_input.t ->
  (Flow.Eval.value, Flow.Diagnostic.t) result
(** [prepare] then [execute]. *)

module Buffers = Core_marks.Buffers
type resident = {
  invoke : Packed.t -> live:Frame_input.t -> Flow.Eval.value option;
  compact : Flow.Eval.value -> Flow.Eval.value option;
  coerce : Flow.Ty.t -> Flow.Eval.value -> Flow.Eval.value option;
  finish : Flow.Eval.value -> Flow.Eval.value;
  close : unit -> unit;
}
(** A resident fold's device. Inside a top-level loop of a drawing whose body holds only regions,
    scalar statements, [array/compact], retyping and [array/count] ([~remember_loops] programs), a
    region's array may stay on the device: [invoke] runs a region there and returns a placeholder (an
    array of its length the device knows), [compact] and [coerce] work on placeholders, [finish] reads
    one back. A declined region gets its placeholder inputs back first; the loop's arrays are read back
    as it ends, then [close]. *)

val draw : ?root:Flow.Eval.residual ->
  ?force:(Packed.t -> (Flow.Eval.value, Flow.Diagnostic.t) result) ->
  ?resolve:(Flow.Eval.value -> (Flow.Eval.value, Flow.Diagnostic.t) result) ->
  ?device:(Packed.t -> ((int * int * int) option, Flow.Diagnostic.t) result) ->
  ?resident:resident ->
  ?image:(Flow.Eval.value -> (int, Flow.Diagnostic.t) result) -> prepared -> live:Frame_input.t ->
  size:int * int -> buffers:Buffers.t -> (Scene_command.Marks.t, Flow.Diagnostic.t) result
(** Run a draw program ({!Core_draw.lower}) into one mark table in painter order. CPU attribute
    arrays are registered in [buffers], which the caller owns, clears per frame and reads while
    it adapts the table. [root] is the residual whose state the program reads. [image] resolves
    an image node to a host resource id. A failure is the core's own diagnostic.

    [device] offers each region whose results are read only as the positions, sizes, radius,
    from, to, fill, stroke or color of a mark set (the others are the frame program's): [Some (token, count, width)]
    means the host ran it on the GPU, and the mark set carries a [Marks.Device] buffer whose id
    {!Buffers.device} resolves to [token]; [None] runs the region on the CPU as usual. *)
