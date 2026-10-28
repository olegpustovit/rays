(* Pure checked Lisp normalizers used by parameter defaults and native schemas. *)
val for_node :
  Flow.Workspace.t ->
  Flow.Workspace.node_definition ->
  (string * (string -> (string, Diagnostic.t) result)) list

val checked : Flow.Workspace.t -> (Flow.Workspace.t, Diagnostic.t) result
