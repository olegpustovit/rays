(** Deterministic packed polygon-surface acceleration.

    Construction triangulates finite simple polygons in stable ear-clipping
    order without materializing another geometry, builds a median-split AABB
    hierarchy, and retains original primitive/corner identities. *)

type t

type hit = {
  primitive : int;
  distance : float;

}

type vertex_selection = All_triangle_vertices | Any_triangle_vertex

type ray_direction_mode =
  | Ray_forward
  | Ray_reverse
  | Ray_bidirectional_closest
  | Ray_bidirectional_farthest

type ray_surface_hit = Ray_first_surface | Ray_last_surface

(** [primitives] restricts construction to a matching primitive group.
    [vertices] additionally retains emitted triangles whose source corners all
    or any belong to a matching vertex group. Primitives without a selected
    corner are skipped before polygon validation. An empty eligible surface is
    valid and every query misses. *)
val create :
  ?cancel:Cancel.t -> ?grain:int -> ?primitives:Group.t ->
  ?vertices:Group.t -> ?vertex_selection:vertex_selection -> Geometry.t ->
  (t, Error.t) result
val triangle_count : t -> int

val raycast :
  t -> origin:Rays_math.Vec3.t -> direction:Rays_math.Vec3.t ->
  (hit option, Error.t) result
(** Intersect a two-sided polygon surface with a world-space ray. Direction is
    robustly normalized, so returned distance and distance bounds use world
    units. Bidirectional selection compares unsigned distances; exact ties
    prefer the forward direction. Equal-distance triangle ties select lower
    source primitive and internal triangle numbers. *)

module View : sig
  type ray_directions =
    | Constant_direction of { x : float; y : float; z : float }
    | Per_query_directions of Packed.Float3.View.view

  type sample_combine =
    | Sample_average
    | Sample_median
    | Sample_shortest
    | Sample_longest

  val create_validated_triangles :
    ?cancel:Cancel.t -> grain:int -> Geometry.t -> (t, Error.t) result
  (** Build directly from geometry already certified finite, nondegenerate,
      polygon-triangle-only, and three-corners-per-primitive by the caller.
      This is a narrow internal ownership boundary, not a validation bypass
      for arbitrary inputs. *)

  val closest_many_into :
    ?cancel:Cancel.t -> ?selection:Group.t -> ?position_indices:int array ->
    grain:int -> t ->
    queries:Packed.Float3.t ->
    max_distance_squared:float -> primitives:int array -> triangles:int array ->
    barycentric_a:float array -> barycentric_b:float array ->
    barycentric_c:float array -> distances_squared:float array -> unit
  (** Fill one stable output slot per selected query. [position_indices]
      optionally maps query elements to shared point positions. [selection]
      must match the query-element count; unselected and missed slots retain the caller's
      initialized primitive/triangle [-1], zero barycentrics, and [infinity].
      Supplied arrays must cover every query. *)

  val closest_distances_many_into :
    ?cancel:Cancel.t -> ?selection:Group.t -> grain:int -> t -> queries:Packed.Float3.t ->
    max_distance_squared:float -> distances_squared:float array -> unit
  (** Fill only closest squared distance, retaining [infinity] for misses and
      unselected queries. Per-range traversal scratch is fixed size; no
      primitive, triangle, or barycentric output planes are allocated. *)

  val triangle_vertex : t -> int -> int -> int
  (** Original source vertex index for local corner 0, 1, or 2 of one
      internal triangle. *)

  val triangle_point : t -> int -> int -> int
  (** Original source point index referenced by one internal triangle corner. *)

  val triangle_primitive : t -> int -> int
  (** Original source primitive for one internal triangle. *)

  val overlapping_triangle_pairs :
    ?cancel:Cancel.t -> grain:int -> tolerance:float -> t -> t ->
    int array * int array
  (** Return deterministic internal-triangle pairs whose axis-aligned bounds
      overlap within the non-negative world-space [tolerance]. Construction is
      two-pass and allocation-bounded by the exact candidate count;
      independent left-triangle ranges are parallel. *)

  val overlapping_self_triangle_pairs :
    ?cancel:Cancel.t -> grain:int -> tolerance:float -> t ->
    int array * int array
  (** Return each unordered pair of bounds-overlapping internal triangles once,
      excluding pairs generated from the same source primitive. This removes
      triangulation diagonals before self-intersection narrow-phase testing. *)

  val self_pairs_disjoint_topology :
    ?cancel:Cancel.t -> grain:int -> tolerance:float -> t ->
    int array * int array
  (** Return AABB/K-DOP candidates that do not share a topology point. BVH
      nodes whose complete subtree shares one query point are pruned before
      descent. The caller must validate the omitted local vertex fans. *)

  val raycast_many_into :
    ?cancel:Cancel.t -> ?selection:Group.t -> grain:int -> t ->
    queries:Packed.Float3.t -> directions:ray_directions ->
    min_distance:float -> max_distance:float -> tolerance:float ->
    direction_mode:ray_direction_mode -> surface_hit:ray_surface_hit ->
    primitives:int array -> triangles:int array ->
    barycentric_a:float array -> barycentric_b:float array ->
    barycentric_c:float array -> distances:float array -> unit
  (** Allocation-bounded batch ray query. Each worker owns fixed scratch;
      selected queries write disjoint slots. Missed/unselected slots are reset
      to primitive/triangle [-1], zero weights, and [infinity]. *)

  val raycast_samples_into :
    ?cancel:Cancel.t -> ?selection:Group.t -> grain:int -> t ->
    queries:Packed.Float3.t -> directions:ray_directions ->
    min_distance:float -> max_distance:float -> tolerance:float ->
    direction_mode:ray_direction_mode -> surface_hit:ray_surface_hit ->
    samples:int -> jitter_scale:float -> seed:int -> combine:sample_combine ->
    primitives:int array -> triangles:int array ->
    barycentric_a:float array -> barycentric_b:float array ->
    barycentric_c:float array -> distances:float array ->
    direction_signs:int array -> hit_counts:int array ->
    normal_x:float array option -> normal_y:float array option ->
    normal_z:float array option -> unit
  (** Combine a bounded deterministic cone of rays per selected query. Scratch
      is O(samples) per worker range; published arrays remain O(queries).
      Average mode reports the hit nearest the mean as scalar primitive/triangle
      provenance and returns the number of contributing rays separately. *)

  val raycast_average_drivers_into :
    ?cancel:Cancel.t -> ?selection:Group.t -> grain:int -> t ->
    queries:Packed.Float3.t -> directions:ray_directions ->
    min_distance:float -> max_distance:float -> tolerance:float ->
    surface_hit:ray_surface_hit -> samples:int -> jitter_scale:float -> seed:int ->
    direction_signs:int array -> hit_counts:int array -> offsets:int array ->
    vertex_numbers:int array -> vertex_weights:float array -> unit
  (** Repeat only the chosen directional sample set and write exact averaged
      source-vertex CSR drivers. The caller supplies offsets derived from
      [hit_counts]; every hit contributes three barycentric weights divided by
      the point's successful-ray count. *)
end
