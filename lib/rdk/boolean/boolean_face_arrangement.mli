(** Exact planar splitting of one face's non-coplanar Boolean constraints. *)

type side = Boolean_constraints.side = Left | Right
type t

val build :
  ?cancel:Cancel.t -> ?coplanar:Boolean_coplanar.t ->
  Boolean_constraints.t -> side:side -> triangle:int ->
  (t, Error.t) result
(** Insert exact T-junction and proper segment/segment intersections, using a
    TPI, line-plane, or projected line-line construction for crossings, then
    split every constraint into stable non-crossing subsegments. When supplied,
    exact coplanar overlap boundaries join the same per-face arrangement.

    Certified implicit-point intervals feed a two-axis sweep before exact
    predicates. Sparse faces take O((s + p) log (s + p) + c) broad-phase work,
    where [c] is the conservative AABB candidate count; interval-heavy faces
    retain an O(s^2 + s*p) worst case because their true arrangement can be
    quadratic. Candidate storage is capped; dense sweeps switch to a packed
    stable-index BVH that streams candidates in oracle order with O(s + p)
    index/scratch storage rather than materializing every pair. Exact initial
    point canonicalization is O(p log p); constructed events are interned in a
    packed exact-coordinate AVL index and per-segment split incidences are
    deduplicated in an integer pair set. Repeated multi-way observations reuse
    one exact event. *)

val point_count : t -> int
val segment_count : t -> int
val segment_first : t -> int -> int
val segment_second : t -> int -> int

module View : sig
  val point : t -> int -> Implicit_point.t
  val point_handle : t -> int -> int
end
