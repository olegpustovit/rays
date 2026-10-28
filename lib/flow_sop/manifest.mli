val version : int
(** Deterministic catalog snapshot shared by the generator and runtime check. *)

val generate :
  ?extra:Catalog.descriptor list ->
  ?nodes:Flow.Defnode.t list ->
  Sop.Edit_graph.factory list ->
  (string * string, Diagnostic.t) result
(** The checked-in S-expression and its digest. *)
