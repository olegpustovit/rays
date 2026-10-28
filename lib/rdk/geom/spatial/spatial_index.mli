(** Immutable balanced index over packed 3D points.

    Construction uses an in-place median partition, skips globally flat axes, and stores one packed
    point-index permutation plus at most three active axis codes. Queries are deterministic: equal
    distances are ordered by the original point index. *)

type t

val create :
  ?cancel:Cancel.t -> ?grain:int -> ?points:Group.t -> Packed.Float3.t -> (t, Error.t) result
(** Build an index in expected O(n log n) time and O(n) auxiliary storage. Deterministic quickselect
    has an O(n squared) adversarial worst case. Non-finite selected source positions are rejected.
    [points] restricts the index to a matching point group. Disjoint tree ranges build in parallel
    at or above [grain], without changing the packed permutation. Empty inputs/selections are valid.
*)

module View : sig
  val nearest_k_many_into :
    ?cancel:Cancel.t ->
    ?points:Group.t ->
    grain:int ->
    t ->
    queries:Packed.Float3.t ->
    max_distance_squared:float ->
    capacity:int ->
    indices:int array ->
    distances_squared:float array ->
    counts:int array ->
    unit
  (** Query every packed point into fixed-width disjoint slices. [indices] and [distances_squared]
      need [query_count * capacity] elements and [counts] needs [query_count]. The traversal
      allocates no per-query or per-visited-point objects. *)

  val nearest_distances_many_into :
    ?cancel:Cancel.t ->
    ?points:Group.t ->
    grain:int ->
    t ->
    queries:Packed.Float3.t ->
    max_distance_squared:float ->
    distances_squared:float array ->
    unit
  (** Fill only nearest squared distance, retaining [infinity] for misses and unselected queries.
      This avoids point-index and count planes when a field operation does not consume nearest-point
      provenance. *)
end
