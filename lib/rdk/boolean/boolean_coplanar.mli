(** Private exact overlap arrangements for coplanar Boolean face pairs. *)

type overlap_kind = Empty | Point | Segment | Polygon
type t

val build :
  ?cancel:Cancel.t -> grain:int -> Boolean_constraints.t ->
  (t, Error.t) result
(** Intersect every exact-coplanar candidate pair independently. Results keep
    the stable pair order from {!Boolean_constraints}. *)

val pair_count : t -> int
val first_triangle : t -> int -> int
val second_triangle : t -> int -> int
val kind : t -> int -> overlap_kind
val point_count : t -> int -> int
val boundary_count : t -> int -> int
val boundary_first : t -> int -> int -> int
val boundary_second : t -> int -> int -> int

module Private : sig
  val constraints : t -> Boolean_constraints.t
  val point : t -> int -> int -> Implicit_point.t
  val boundary_support_first : t -> int -> int -> int
  val boundary_support_second : t -> int -> int -> int
  val left_pair_range : t -> int -> int * int
  val right_pair_range : t -> int -> int * int
  val left_pair : t -> int -> int
  val right_pair : t -> int -> int
end
