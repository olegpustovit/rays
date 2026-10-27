(** Static 2D workspace export, with its JavaScript/WGSL runtime embedded. *)

type lowered = private {
  program : Flow_ir.Core.program;
  span : Diagnostic.span;
  root : Flow.Eval.residual option;
  plan : Flow.Eval.plan;
}

val lower : Editor_document.Workspace_doc.t -> (lowered, Diagnostic.t) result
val files : Editor_document.Workspace_doc.t -> lowered ->
  ((string * string) list, Diagnostic.t) result
val load : string -> string * (Editor_document.Workspace_doc.t, Diagnostic.t list) result
