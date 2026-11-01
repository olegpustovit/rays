val mesh_points : Flow.Eval.value -> (int * (unit -> Rdk.Packed.Float3.t), Diagnostic.t) result
val mesh : (string * Flow.Eval.value) list -> (Rdk.Geometry.t, Diagnostic.t) result

val resident :
  Rdk.Geometry.t ->
  (Flow_ir.Gpu.integers * Flow_ir.Gpu.integers * Flow_ir.Gpu.integers) option
(** Unread resident vertex points, primitive offsets and kinds, with their logical
    lengths. Counts and holder lookup do not materialise topology. *)
