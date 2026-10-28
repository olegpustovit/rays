(** Adapt native factories and embedded Lisp declarations to the context-generic Flow
    checker/printer descriptor. *)

type descriptor = {
  qualified : string;
  key : string;
  operation : string;
  label : string;
  category : string list;
  slots : (string * Sop.Edit_graph.input_requirement) list;
  slot_types : string list;
      (** Optional serialized types in physical input order. Empty keeps the context defaults. *)
  keyword_inputs : string list;  (** Required fn/image inputs exposed as keyword parameters. *)
  fields : Param.field_view list;
}
(** One catalog kind: a SOP factory, or a scene, world or settings kind that the editor document
    generates from its own schemas ([Editor_document.Lowering]). The namespace of [qualified] gives
    the context. *)

val descriptor : Sop.Edit_graph.factory -> descriptor
(** The [sop/] kind of a factory. *)

val with_library : descriptor list -> descriptor list
(** [present] then every embedded library node it lacks, whether or not a workspace uses it: what
    the editor's node menu and completion offer (a workspace lowers only the library definitions it
    reaches). *)

val of_factories :
  version:int ->
  ?extra:descriptor list ->
  ?nodes:Flow.Defnode.t list ->
  Sop.Edit_graph.factory list ->
  (Flow.Catalog.catalog, Diagnostic.t) result

val sources : unit -> (Flow.Syntax.t list, Diagnostic.t) result
(** Embedded node-library definitions, sorted by source filename. *)

val node_definitions : unit -> (Flow.Defnode.t list, Diagnostic.t) result

module Private : sig
  val reachable_sources : Flow.Syntax.t list -> Flow.Syntax.t list -> Flow.Syntax.t list
  val definitions_of_sources : Flow.Syntax.t list -> (Flow.Defnode.t list, Diagnostic.t) result
end

val check_workspace :
  ?ops:Flow.Op.t list ->
  ?library:bool ->
  ?sources:Flow.Syntax.t list ->
  Flow.Catalog.catalog ->
  Flow.Syntax.t list ->
  Flow.Workspace.t option * Diagnostic.t list
(** Check with the embedded definitions registered in [catalog] in scope. Authored form IDs stay
    intact; library forms receive a disjoint ID space and stay out of [Workspace.source]. [sources]
    overrides the embedded definitions for callers providing their own node library, regardless of
    catalog membership. *)
