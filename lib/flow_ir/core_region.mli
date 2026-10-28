exception Unsupported
(** Lowering of packed forms (map, reduce, numeric loop, array/sum) to core regions. Refusals keep
    the [E_PACKED_*] codes and spans. *)

val lower :
  ?fusion:bool ->
  ?dynamic:bool ->
  ?count_source:Core.Region.count_source ->
  Flow.Eval.residual ->
  Flow.Workspace.term ->
  (Core.Region.t, Diagnostic.t list) result
(** The unfused region of one form; [fusion] and [dynamic] are recorded on it. *)

val rebind : Core.Region.t -> Flow.Eval.residual -> Core.Region.t option
(** A template region ([dynamic]) with the next step's captures, or [None] when its function body,
    residual or fused scope changed. *)

(** Helpers shared with [Packed] and [Core_fuse]. *)
val instruction_key :
  Core.Region.instruction ->
  [ `Constant of int64
  | `Guarded_constant of (int * bool) list * int64
  | `Instruction of Core.Region.instruction ]
(** Sharing preserves the binary64 bits of constants, including signed zero. *)

val finish_loops :
  (int, int) Hashtbl.t -> Core.Region.instruction array -> Core.Region.instruction array
(** Fill each loop header's closing register after its body has been emitted. *)

val width_of : Flow.Ty.t -> int
val components : Flow.Eval.value -> float array
val count_of : Flow.Eval.residual -> Flow.Workspace.term -> int option
