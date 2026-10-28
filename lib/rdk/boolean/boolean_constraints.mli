(** View exact triangle-pair classification and per-face constraint plans. *)

type side = Left | Right
type t

val build :
  ?cancel:Cancel.t ->
  ?resolve_left_self_intersections:bool ->
  ?resolve_right_self_intersections:bool ->
  ?skip_opposite_self_pairs:bool ->
  grain:int ->
  left:Geometry.t ->
  right:Geometry.t ->
  unit ->
  (t, Error.t) result
(** Build a deterministic exact non-coplanar arrangement plan. Broad-phase AABBs are followed by
    exact predicates; ordinary classification is parallel over stable candidate ranges.
    Self-intersection resolution is an explicit per-input policy and defaults to [false], preserving
    the allocation-minimal clean-input path. Shared-point omission is reserved for callers that
    separately certify the omitted local vertex fans. *)

val point_count : t -> int
val constraint_count : t -> int
val constraint_first : t -> int -> int
val constraint_second : t -> int -> int
val constraint_first_side : t -> int -> side
val constraint_second_side : t -> int -> side
val constraint_first_triangle : t -> int -> int
val constraint_second_triangle : t -> int -> int
val left_triangle_count : t -> int
val right_triangle_count : t -> int
val left_constraint_range : t -> int -> int * int
val right_constraint_range : t -> int -> int * int
val left_constraint : t -> int -> int
val right_constraint : t -> int -> int
val coplanar_pair_count : t -> int
val coplanar_first_side : t -> int -> side
val coplanar_second_side : t -> int -> side
val coplanar_first_triangle : t -> int -> int
val coplanar_second_triangle : t -> int -> int
val degenerate_pair_count : t -> int

module View : sig
  val left_geometry : t -> Geometry.t
  val right_geometry : t -> Geometry.t
  val resolve_left_self_intersections : t -> bool
  val resolve_right_self_intersections : t -> bool
  val left_surface : t -> Surface_index.t
  val right_surface : t -> Surface_index.t
  val point : t -> int -> Implicit_point.t
  val source : t -> Implicit_point.source
  val source_point_count : t -> int
  val source_point : t -> int -> Implicit_point.t
  val left_triangle_point : t -> int -> int -> int
  val right_triangle_point : t -> int -> int -> int
  val triangle_point : t -> side -> int -> int -> int
end
