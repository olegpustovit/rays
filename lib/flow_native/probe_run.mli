(** What the graph pane's probes run on, kept out of the editor so it reaches no [flow_ir]. *)

type reports
(** The profile of the executed groups (a snapshot of at most 512). *)

val reports : Flow_ir.Profile.t -> reports
val no_reports : reports

val same_count : reports -> reports -> bool
(** A probe that first runs adds a group: the count is what tells a footer to refresh. *)

val force : unit -> Flow_graph.Probe.force
(** The probes' forcing function over the frame/region compiler, with its own bounded program cache.
*)

val find :
  reports ->
  instance:int option ->
  Flow.Workspace.path ->
  probes:int list ->
  Flow_graph.Probe.execution option
(** Where the node at a path ran, in the given graph instance at the given iterations. *)
