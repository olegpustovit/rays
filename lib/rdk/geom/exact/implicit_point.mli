(** View exact homogeneous points used by mesh-arrangement kernels. *)

type source

type projection = XY | YZ | ZX

type t

type construction =
  | Explicit of int
  | Rounded of { x : float; y : float; z : float }
  | Line_plane of {
      line_start : int;
      line_end : int;
      plane_a : int;
      plane_b : int;
      plane_c : int;
    }
  | Line_line of {
      projection : projection;
      first_start : int;
      first_end : int;
      second_start : int;
      second_end : int;
    }
  | Triple_plane of {
      first_a : int; first_b : int; first_c : int;
      second_a : int; second_b : int; second_c : int;
      third_a : int; third_b : int; third_c : int;
    }
  | Midpoint of { first : t; second : t }
  | Centroid3 of { first : t; second : t; third : t }
  | Circumcenter2_xy of { first : t; second : t; third : t }

type error =
  | Coordinate_plane_size_mismatch
  | Non_finite_coordinate of int
  | Index_out_of_bounds of int
  | Parallel_line_and_plane
  | Parallel_lines
  | Dependent_planes
  | Point_source_mismatch
  | Degenerate_triangle
  | Non_finite_approximation

val source : x:float array -> y:float array -> z:float array ->
  (source, error) result
(** Validate and borrow packed coordinate planes. They must remain immutable
    for the lifetime of every point constructed from the source. *)

val construction : t -> construction
val source_coordinate : source -> int -> float * float * float

val constant_components : source -> int -> int -> int
(** Number of exactly equal binary64 coordinate components on two source
    vertices. Used to choose the best-conditioned equivalent LPI recipe. *)

val source_triangle_projection : source -> int -> int -> int -> projection
val source_segment_contains :
  source -> projection:projection -> first:int -> second:int -> t -> bool
(** Exact, allocation-bounded source-feature ancestry queries. The segment
    query accepts any implicit point over the same source and includes both
    endpoints. *)

val barycentric_source_triangle :
  source -> a:int -> b:int -> c:int -> t -> float * float * float
(** Exact-construction barycentric weights against three explicit source
    points without retaining temporary explicit point objects. *)

val explicit : source -> int -> (t, error) result

val rounded : reference:t -> x:float -> y:float -> z:float -> (t, error) result
(** Attach an explicitly rounded finite construction to an existing source
    arena. Later predicates are exact for those binary64 coordinates. *)

val line_plane : source ->
  line_start:int -> line_end:int ->
  plane_a:int -> plane_b:int -> plane_c:int ->
  (t, error) result
(** Construct the exact intersection of an infinite line and plane as a
    normalized homogeneous point. The source recipe is retained for ancestry;
    no Cartesian rounding participates in later predicates. *)

val line_line : source -> projection:projection ->
  first_start:int -> first_end:int ->
  second_start:int -> second_end:int ->
  (t, error) result
(** Exact intersection of two non-parallel coplanar lines in a selected
    non-degenerate projection. *)

val triple_plane : source ->
  first_a:int -> first_b:int -> first_c:int ->
  second_a:int -> second_b:int -> second_c:int ->
  third_a:int -> third_b:int -> third_c:int ->
    (t, error) result
(** Construct the exact common point of three independent planes. *)

val centroid3 : t -> t -> t -> (t, error) result
(** Construct the exact arithmetic centroid of three implicit points. *)

val midpoint : t -> t -> (t, error) result
(** Construct the exact arithmetic midpoint of two implicit points. *)

val diametral_dot_xy : first:t -> second:t -> t -> Predicates.sign
(** Exact sign of [(point-first) dot (point-second)] in XY. A negative result
    means [point] strictly encroaches the segment's open diametral disk. *)

val approximate : t -> float * float * float
(** Finite Cartesian approximation for acceleration and presentation only. *)

val bounds : t -> (float * float) * (float * float) * (float * float)
(** Certified Cartesian intervals, primarily for kernel diagnostics. *)

val equal : t -> t -> bool
val compare_x : t -> t -> int
val compare_y : t -> t -> int
val compare_z : t -> t -> int
(** Exact homogeneous comparisons returning [-1], [0], or [1]. *)

val orient2d_xy : t -> t -> t -> Predicates.sign
val orient2d_yz : t -> t -> t -> Predicates.sign
val orient2d_zx : t -> t -> t -> Predicates.sign
val orient3d : t -> t -> t -> t -> Predicates.sign
(** Exact predicates supporting any mixture of explicit, LPI, and TPI points
    created from the same packed source. *)

val radial_dot : t -> t -> t -> t -> Predicates.sign
(** Exact sign of the dot product between the last two vectors after
    projection perpendicular to the directed first-two-point edge. *)

(** Exact positive-infinitesimal direction [(1, epsilon, epsilon^2)]. *)
val normal_dot_symbolic : t -> t -> t -> Predicates.sign

type symbolic_ray_triangle =
  | Symbolic_miss
  | Symbolic_hit of Predicates.sign
  | Symbolic_origin_boundary
  | Symbolic_degenerate

type axis_ray_triangle =
  | Axis_miss
  | Axis_hit of Predicates.sign
  | Axis_boundary
  | Axis_parallel

val symbolic_ray_source_triangle :
  source -> a:int -> b:int -> c:int -> t -> symbolic_ray_triangle
(** Allocation-minimal equivalent over one packed explicit source triangle and
    one implicit query. *)

val axis_ray_source_triangle :
  source -> a:int -> b:int -> c:int -> axis:int -> positive:bool -> t ->
  axis_ray_triangle
(** Allocation-minimal exact ray/triangle decision for signed coordinate axis
    [0], [1], or [2]. [Axis_parallel] means the carrier has zero projected
    normal for that direction; [Axis_boundary] asks the caller to try another
    exact direction. *)

val incircle_xy : t -> t -> t -> t -> Predicates.sign
val incircle_yz : t -> t -> t -> t -> Predicates.sign
val incircle_zx : t -> t -> t -> t -> Predicates.sign
(** Exact projected incircle determinant. For a positively oriented first
    triangle, [Positive] means the fourth point is inside its circumcircle. *)
