(** Validation glue, linked only by workspace checks. Uses the caller's actual catalog. *)
val check : ?directory:string -> ?commands:bool -> factories:Sop.Edit_graph.factory list ->
  name:string -> Editor_document.Workspace_doc.t -> unit
(** Compare all plan arguments, instances, results, states and records through
    IR/reference at four times and one/eight domains. A directory also compares
    authored payloads and native pixels of every cooked/drawing result, including
    images and scene image textures. [commands] prepares the same commands and
    checks owned image bytes without requiring a native rendering device.
    Audits each qualified path against observed authored producers with fused/
    unfused packed compilation and pure emission; reports pending/refused forms. *)

val report_approx : name:string -> Editor_document.Workspace_doc.t -> unit
(** Qualify and print the complete path set using actual inputs/captures and the
    caller's catalog, auditing fused/unfused emission and reporting all reasons. *)
