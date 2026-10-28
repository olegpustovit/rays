(* Cooked immutable group boundary; native RDK owns membership and order. *)
type owner = Ordinary of Rdk.Group.owner | Edge
val owner : string -> (owner, Diagnostic.t) result
val read : owner:owner -> name:string -> Rdk.Geometry.t -> (Flow.Eval.value, Diagnostic.t) result
val write : ?order:int array -> owner:owner -> name:string -> int array -> Rdk.Geometry.t ->
  (Rdk.Geometry.t, Diagnostic.t) result
val resolve : geometry:(int -> Rdk.Geometry.t option) -> Flow.Eval.value ->
  (Flow.Eval.value, Diagnostic.t) result
