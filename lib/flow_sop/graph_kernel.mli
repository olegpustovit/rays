(* Private boundary for the pure graph/ sources: [graph/shortest_paths],
   [graph/color] and [graph/chains] resolve from their array arguments alone,
   with owned results, and never read geometry. *)
val resolve : Flow.Eval.value -> (Flow.Eval.value, Diagnostic.t) result
