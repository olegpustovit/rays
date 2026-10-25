(** Cook-context constants available to Lisp node bodies. [depends ~op] is whether a reachable,
    unpruned call of the zero-state operator [op] (default [sop/context_seed]) remains. *)
val depends : ?op:string -> ?inputs:(string * Flow.Eval.value) list -> ?defs:Flow.Workspace.graph list -> Flow.Workspace.graph -> bool
val specialize : ?provenance:int -> context:Sop.Context.t -> label:string -> Flow.Workspace.graph -> Flow.Workspace.graph

(* Own declaration and transitively referenced checked declarations/helpers. *)
val declaration_source : Flow.Workspace.t -> Flow.Workspace.node_definition -> string

(* Authored source plus the declarations the checked workspace's graphs reach: the key of
   prepared kernels, instead of the whole library. *)
val kernel_source : Flow.Workspace.t -> string

(** Whether [specialize] can change the graph: it names a context or provenance operator. A graph
    for which this is false is its own specialization. *)
val uses_context : Flow.Workspace.graph -> bool

(** B.F: whether a Lisp node body is one [let*] with a single [sop/with_attr in0 :P] and no scans,
    folds, errors or branches on cooked data. *)
val fusable : Flow.Workspace.node_definition -> bool

(** [fuse x u ~baked]: [x] with [u]'s [P] kernel folded in; [baked] are [u]'s parameter values. *)
val fuse : Flow.Workspace.node_definition -> Flow.Workspace.node_definition ->
  baked:(string * Flow.Eval.value) list -> Flow.Workspace.node_definition option

val reachable_defs : Flow.Workspace.t -> Flow.Workspace.graph list -> Flow.Workspace.graph list
(** The checked helpers the graphs reach, directly or through the Lisp nodes they call, in declaration order. *)
