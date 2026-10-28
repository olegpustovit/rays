module Int_map : Map.S with type key = int
(** SOP geometry with the live arguments of a lowered workspace graph. Layout and probes stay in the
    editor document; resolution never changes these literal records. *)

type t = private {
  geometry : Sop.Edit_graph.t;
  drives : Flow.Eval.value Port.Map.t;
      (** a workspace argument that depends on [t] ({!Flow.Eval.is_live}), evaluated by
          {!Value_lane.resolve} for each time (a scalar, a vec3, a colour text, or a list of vec3
          for a text-encoded list parameter), keyed (node, argument name). Compare with [==]. *)
  states : Flow.Eval.value list;
      (** Frame folds of this workspace, advanced before its live parameters. *)
  frame_nodes :
    (network:t -> Flow.Eval.state -> Input.t -> Sop.Node.t -> (Sop.Node.t, Diagnostic.t) result)
    Int_map.t;
      (** Static-plan-bounded geometry zones rebuilt with an immutable fold snapshot on the initial
          domain, before worker submission. *)
  profile : Flow_ir.Profile.t option;
  reference : bool;
  approx : Flow.Workspace.Paths.t;
}

val of_geometry : Sop.Edit_graph.t -> t
val with_profile : Flow_ir.Profile.t -> t -> t

val with_reference : bool -> t -> t
(** Independent reference execution for parity checks and inspected cones. *)

val with_approx : Flow.Workspace.Paths.t -> t -> t
val with_states : Flow.Eval.value list -> t -> t

val with_frame_nodes :
  (network:t -> Flow.Eval.state -> Input.t -> Sop.Node.t -> (Sop.Node.t, Diagnostic.t) result)
  Int_map.t ->
  t ->
  t

val with_drives : Flow.Eval.value Port.Map.t -> t -> (t, Diagnostic.t) result
val validate : t -> (unit, Diagnostic.t) result
val parameter : t -> Port.t -> (Port.parameter, Diagnostic.t) result
val with_geometry : Sop.Edit_graph.t -> t -> (t, Diagnostic.t) result

val apply_parameters : node_id:int -> (string * Param.value) list -> t -> (t, Diagnostic.t) result
(** Apply validated node fields without changing slots or drive targets. *)

val remove_nodes : int list -> t -> (t, Diagnostic.t) result
