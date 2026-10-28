(** Exact arrangement of projected planar constraint segments. *)

type t

val build :
  ?cancel:Cancel.t ->
  ?grain:int ->
  split_crossings:bool ->
  x:float array -> y:float array ->
  segment_points:int array ->
  ?embedded_points:int array ->
  ?segment_winding:int array ->
  unit ->
  (t, string) result
(** Split proper crossings, constraint endpoints, and [embedded_points] lying
    on constraint segments into a non-crossing PSLG. Repeated indices are an
    error; exact-coordinate duplicates collapse to their lowest point number
    (normally callers pass the representatives from {!Delaunay2}). Candidate
    point/segment pairs are found by a two-pass
    packed BVH query; exact orientation alone decides incidence. Proper
    intersections are retained as
    exact homogeneous line-line constructions; rounded coordinates are only
    presentation/acceleration values. [segment_winding] is signed directed
    polygon-boundary multiplicity and is propagated/aggregated onto canonical
    atomic constraints. Expected work is
    O((segments + embedded_points) log segments + incidences), with
    O(segments + embedded_points + incidences) auxiliary storage. [grain]
    controls deterministic disjoint point-query ranges. *)

val point_count : t -> int
val split_point_count : t -> int
(* Public packed accessors below return defensive copies. Exact predicates
   must use [View]; approximate coordinates never decide topology. *)
val split_source_first : t -> int -> int
val split_source_second : t -> int -> int
val split_source_parameter : t -> int -> float

module View : sig
  type view = {
    x : float array;
    y : float array;
    insert_points : int array;
    constraint_points : int array;
    constraint_winding : int array;
  }

  val view : t -> view
  (** Borrowed packed planes for audited RDK kernels. Do not mutate them. *)

  val point : t -> int -> Implicit_point.t
  (** Borrow an exact explicit or constructed arrangement point. *)

  val orient2d : t -> int -> int -> int -> Predicates.sign
  val incircle : t -> int -> int -> int -> int -> Predicates.sign
end
