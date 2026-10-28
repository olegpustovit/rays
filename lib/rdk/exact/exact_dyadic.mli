(** View exact dyadic arithmetic for binary64 predicate fallback. *)

type t

val is_zero : t -> bool
val of_float : float -> t
val add : t -> t -> t
val negate : t -> t
val subtract : t -> t -> t
val multiply : t -> t -> t
val compare_zero : t -> int
(** Return [-1], [0], or [1]. *)

val to_scaled_float : t -> float * int
(** [to_scaled_float value] returns [(mantissa, exponent)] approximating
    [value = mantissa *. 2 ** exponent]. The mantissa is finite and has
    magnitude at least [0.5] and less than [1.0] for nonzero values. This conversion is only for
    spatial acceleration and final materialization; combinatorial decisions
    must continue to use exact arithmetic. *)

val to_scaled_interval : t -> float * float * int
(** Certified outward-rounded [(lower, upper, exponent)] representation of
    [value]. Both bounds are scaled by [2 ** exponent]. *)

module Scratch : sig
  val orient2d :
    ax:float -> ay:float -> bx:float -> by:float -> cx:float -> cy:float -> int
  val orient3d :
    ax:float -> ay:float -> az:float ->
    bx:float -> by:float -> bz:float ->
    cx:float -> cy:float -> cz:float ->
    dx:float -> dy:float -> dz:float -> int
  val incircle :
    ax:float -> ay:float -> bx:float -> by:float ->
    cx:float -> cy:float -> dx:float -> dy:float -> int
  val polygon_area_packed :
    x:float array -> y:float array -> points:int array ->
    first:int -> count:int -> int
  (* Exact sign of twice a polygon's shoelace area. *)
  val compare_products : t -> t -> t -> t -> int
  val compare_homogeneous_float :
    numerator:t -> denominator:t -> value:float -> int
  val orient2d_explicit_explicit_homogeneous :
    ax:float -> ay:float -> bx:float -> by:float ->
    cx:t -> cy:t -> cw:t -> int
  val barycentric_explicit_triangle_homogeneous :
    ax:float -> ay:float -> az:float ->
    bx:float -> by:float -> bz:float ->
    cx:float -> cy:float -> cz:float ->
    px:t -> py:t -> pz:t -> pw:t -> float * float * float
  val orient2d_homogeneous :
    ax:t -> ay:t -> aw:t -> bx:t -> by:t -> bw:t ->
    cx:t -> cy:t -> cw:t -> int
  val orient3d_homogeneous :
    ax:t -> ay:t -> az:t -> aw:t ->
    bx:t -> by:t -> bz:t -> bw:t ->
    cx:t -> cy:t -> cz:t -> cw:t ->
    dx:t -> dy:t -> dz:t -> dw:t -> int
  val incircle_homogeneous :
    ax:t -> ay:t -> aw:t -> bx:t -> by:t -> bw:t ->
    cx:t -> cy:t -> cw:t -> dx:t -> dy:t -> dw:t -> int
  (* Exact sign for the positive-infinitesimal direction
     [(1, epsilon, epsilon^2)]. *)
  (* Exact sign of the triangle normal dotted with the
     positive-infinitesimal direction [(1, epsilon, epsilon^2)]. *)
  val normal_dot_symbolic_homogeneous :
    fx:t -> fy:t -> fz:t -> fw:t ->
    sx:t -> sy:t -> sz:t -> sw:t ->
    tx:t -> ty:t -> tz:t -> tw:t -> int
  val axis_ray_triangle_explicit_homogeneous :
    axis:int -> positive:bool ->
    ax:float -> ay:float -> az:float ->
    bx:float -> by:float -> bz:float ->
    cx:float -> cy:float -> cz:float ->
    qx:t -> qy:t -> qz:t -> qw:t -> int
  (* Codes: 0 miss, 1 negative-oriented hit, 2 positive-oriented hit,
     3 boundary-degenerate ray, 4 parallel carrier. *)
  val symbolic_ray_triangle_explicit_homogeneous :
    ax:float -> ay:float -> az:float ->
    bx:float -> by:float -> bz:float ->
    cx:float -> cy:float -> cz:float ->
    qx:t -> qy:t -> qz:t -> qw:t -> int
  (* Codes: 0 miss, 1 negative-oriented hit, 2 positive-oriented hit,
     3 query on the closed triangle, 4 degenerate triangle. *)
  val radial_dot_homogeneous :
    ex0:t -> ey0:t -> ez0:t -> ew0:t ->
    ex1:t -> ey1:t -> ez1:t -> ew1:t ->
    lx:t -> ly:t -> lz:t -> lw:t ->
    rx:t -> ry:t -> rz:t -> rw:t -> int
  (** Domain-local packed exact predicate temporaries. Results are comparison
      signs [-1], [0], or [1]; no arena-backed value escapes the call. *)
end
