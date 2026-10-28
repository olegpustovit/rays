(** A literal edited in place: the edit re-checks the workspace and patches the lowering instead of
    lowering again. *)

type change = {
  path : Flow.Workspace.path;
  field : string;
  kind : string;
  authored : int;
  before : Flow.Eval.value;
  after : Flow.Eval.value;
  expr : Flow.Syntax.t;
}

val patch :
  Flow.Check.catalog -> Flow.Workspace.t -> Flow_graph.Flow_edit.op ->
  (Flow.Workspace.t * change, Diagnostic.t) result option
(** [None] when the op is not a single literal change. *)

val lower : Flow_sop.Lower.t -> change -> (Flow_sop.Lower.t, Diagnostic.t) result option
