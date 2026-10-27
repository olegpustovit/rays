(** Topology-preserving attribute metadata operations over cooked geometry. *)
val delete : owner:string -> pattern:string -> Sop.Node.t -> Sop.Node.t

val attribute_owner : string -> (Rdk.Attribute.owner option, Diagnostic.t) result
val group_owner : string -> (Rdk.Group_ops.owner option, Diagnostic.t) result
val attr_conflict : string -> (Rdk.Attribute_ops.rename_conflict, Diagnostic.t) result
val group_conflict : string -> (Rdk.Group_ops.rename_conflict, Diagnostic.t) result
val rename : owner:string -> pattern:string -> replacement:string -> conflict:string -> Sop.Node.t -> Sop.Node.t
val swap : owner:string -> source:string -> destination:string -> method_:string -> Sop.Node.t -> Sop.Node.t
val delete_attributes : ?reference:Sop.Node.t -> inverse:bool -> point:string -> vertex:string -> primitive:string -> detail:string -> Sop.Node.t -> Sop.Node.t
val rename_group : owner:string -> pattern:string -> replacement:string -> conflict:string -> Sop.Node.t -> Sop.Node.t
val delete_group : owner:string -> pattern:string -> unused:bool -> Sop.Node.t -> Sop.Node.t
val rename_edge : from:string -> into:string -> Sop.Node.t -> Sop.Node.t
val delete_edge : name:string -> Sop.Node.t -> Sop.Node.t
val type_of : owner:Rdk.Attribute.owner -> name:string -> Rdk.Geometry.t -> (Flow.Eval.value, Diagnostic.t) result
val names : owner:Rdk.Attribute.owner -> pattern:string -> Rdk.Geometry.t -> (Flow.Eval.value, Diagnostic.t) result
val entries : owner:Rdk.Attribute.owner option -> component:string -> Rdk.Geometry.t -> (Flow.Eval.value, Diagnostic.t) result
val resolve : geometry:(int -> Rdk.Geometry.t option) -> Flow.Eval.value -> (Flow.Eval.value, Diagnostic.t) result
val glob_rewrite : pattern:string -> replacement:string -> string ->
  (string, Diagnostic.t) result
val glob_rewrite_set : pattern:string -> replacement:string -> string ->
  (string, Diagnostic.t) result
val glob_rewrite_set_matches : pattern:string -> replacement:string -> string ->
  (bool, Diagnostic.t) result
val glob_literal : string -> (string, Diagnostic.t) result
