val mesh_points : Flow.Eval.value -> (int * (unit -> Rdk.Packed.Float3.t), Diagnostic.t) result
val mesh : (string * Flow.Eval.value) list -> (Rdk.Geometry.t, Diagnostic.t) result
