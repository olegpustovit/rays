(** Selection promotion over packed topology. *)
type t =
  | Selected_points of Group.t
  | Selected_vertices of Group.t
  | Selected_primitives of Group.t
  | Selected_edges of Edge_group.t

val validate : operation:string -> Topology.t -> t option -> (unit, string) result

val promote :
  ?cancel:Cancel.t ->
  grain:int ->
  ?name:string ->
  destination:Group.owner ->
  t ->
  Topology.t ->
  (Group.t, string) result
