(** Packed BVH over the linear pieces accepted by Intersection Analysis. *)

type t
type kind = Triangle | Segment

val create :
  ?cancel:Cancel.t -> ?grain:int -> ?primitives:Group.t -> Geometry.t ->
  (t, Error.t) result
(** Selected polygon primitives must be triangles. Open and closed polygon
    curves are expanded into stable segment pieces without materializing new
    geometry. Referenced positions must be finite, triangles non-degenerate,
    and segments non-zero. *)

val piece_count : t -> int

module View : sig
  val kind : t -> int -> kind
  val primitive : t -> int -> int
  val local : t -> int -> int

  val point : t -> int -> int -> int
  (** Original source point number for a piece corner. *)

  val curve_u : t -> int -> int -> float
  (** Global polygon-curve parameter at segment endpoint 0 or 1. *)

  val positions : t -> Packed.Float3.View.view

  val overlapping_pairs :
    ?cancel:Cancel.t -> grain:int -> tolerance:float -> t -> t ->
    int array * int array

  val overlapping_self_pairs :
    ?cancel:Cancel.t -> grain:int -> tolerance:float -> t ->
    int array * int array
  (** Each unordered piece pair is returned once. Unlike surface
      triangulation, different pieces from one curve primitive remain eligible
      because they can genuinely self-intersect. *)

  val find_overlapping_self_pair :
    ?cancel:Cancel.t -> grain:int -> tolerance:float -> t ->
    (int -> int -> bool) -> (int * int) option
  (** Return the lexicographically first unordered AABB-overlapping pair for
      which the pure predicate returns [true]. Candidate storage is O(1) per
      worker range rather than O(candidate count); stable ranges and a final
      ordered reduction make the diagnostic domain-count independent. *)
end
