(** View conforming two-complex assembled from refined Boolean faces. *)

type t

val build : ?cancel:Cancel.t -> Boolean_constraints.t -> Boolean_refinement.t -> (t, Error.t) result
val vertex_count : t -> int
val approximate_vertex : t -> int -> float * float * float
val facet_count : t -> int
val facet_vertex : t -> int -> int -> int
val facet_member_range : t -> int -> int * int
val member_side : t -> int -> Boolean_constraints.side
val member_face : t -> int -> int
val member_triangle : t -> int -> int
val member_winding : t -> int -> int
val edge_count : t -> int
val edge_first : t -> int -> int
val edge_second : t -> int -> int
val edge_incident_range : t -> int -> int * int
val edge_incident_facet : t -> int -> int
val edge_incident_local : t -> int -> int

module View : sig
  val constraints : t -> Boolean_constraints.t
  val vertex : t -> int -> Implicit_point.t
end
