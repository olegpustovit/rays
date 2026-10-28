(** Node-owned schemas and cook identity for checked Lisp geometry bodies. *)
val input : int -> (Sop.Payload.t, Diagnostic.t) result
(** Immutable cooked input in the enclosing body's caller-domain scope. *)

val node :
  ?unbound_function:bool -> ?geometry_inputs:bool list ->
  operation:string -> version:int -> label:string -> fields:Param.field_view list ->
  normalizers:(string * (string -> (string, Diagnostic.t) result)) list ->
  source:string ->
  build:(label:string -> context:Sop.Context.t -> Sop.Node.t list -> (string * Flow.Eval.value) list ->
    ((Sop.Context.t -> (Sop.Node.t, Diagnostic.t) result) *
     Sop.Context.Dependencies.t * Sop.Node.facts, string) result) ->
  Sop.Node.t list -> (Sop.Node.t, string) result

(** [geometry_inputs] flags, per connected input, a slot that must carry geometry: a cook that finds
    another payload there fails with [E_PAYLOAD] before the body runs, as a native geometry node does. *)

val with_initial_changes : operation:string -> (string * Param.value) list -> (unit -> 'a) -> 'a
(** Run an instantiation whose first Lisp node of [operation] starts from these parameter changes
    instead of its defaults, so a node that is about to be edited is built once, not twice. *)
