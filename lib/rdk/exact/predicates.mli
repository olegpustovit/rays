(** Filtered exact geometric predicates over finite IEEE-754 coordinates. *)

type sign = Negative | Zero | Positive

type segment_segment =
  | Segments_disjoint
  | Segments_point
  | Segments_overlap
  | Segments_degenerate

val orient2d :
  ax:float -> ay:float ->
  bx:float -> by:float ->
  cx:float -> cy:float ->
  sign
(** Exact sign of the oriented area determinant [(a-c) x (b-c)].
    The ordinary case uses a floating-point filter; ambiguous,
    overflowing, and underflowing cases fall back to exact dyadic arithmetic.
    Raises [Invalid_argument] when any coordinate is non-finite. *)

val orient3d :
  ax:float -> ay:float -> az:float ->
  bx:float -> by:float -> bz:float ->
  cx:float -> cy:float -> cz:float ->
  dx:float -> dy:float -> dz:float ->
  sign
(** Exact sign of the determinant of [a-d], [b-d], and [c-d].
    Positive and negative correspond to the determinant's mathematical sign.
    The fast path does not allocate; exact fallback storage is bounded by the
    exponent range of binary64 inputs. Raises [Invalid_argument] for a
    non-finite coordinate. *)

val orient2d_packed :
  x:float array -> y:float array -> int -> int -> int -> sign
(** Allocation-free hot-path variant of [orient2d] over packed coordinate
    planes and point indices. Exact fallback allocates bounded scratch. *)

val orient3d_packed :
  x:float array -> y:float array -> z:float array ->
  int -> int -> int -> int -> sign
(** Allocation-free hot-path variant of [orient3d] over packed coordinate
    planes and point indices. Exact fallback allocates bounded scratch. *)

val incircle_packed :
  x:float array -> y:float array -> int -> int -> int -> int -> sign
(** Allocation-free hot-path variant of [incircle] over packed coordinate
    planes and point indices. Exact fallback uses bounded domain-local scratch. *)

val polygon_area_sign_packed :
  x:float array -> y:float array -> points:int array ->
  first:int -> count:int -> sign
(** Exact sign of twice the signed shoelace area of one packed polygon range.
    A conservative floating-point filter handles ordinary inputs; ambiguous,
    overflowing, and underflowing sums use domain-local exact dyadics. *)

val segment_segment_packed :
  x:float array -> y:float array -> z:float array ->
  left_a:int -> left_b:int -> right_a:int -> right_b:int -> segment_segment
(** Exact closed-segment contact in 3D over finite binary64 coordinates.
    Point contact and positive-length collinear overlap are distinct. Skew
    segments are rejected through exact coplanarity before a stable exact 2D
    projection. Degenerate zero-length inputs are reported explicitly. *)

module View : sig
  val exact_orient2d_arena :
    ax:float -> ay:float -> bx:float -> by:float -> cx:float -> cy:float -> sign
  val exact_orient3d_arena :
    ax:float -> ay:float -> az:float ->
    bx:float -> by:float -> bz:float ->
    cx:float -> cy:float -> cz:float ->
    dx:float -> dy:float -> dz:float -> sign
  val exact_incircle_arena :
    ax:float -> ay:float -> bx:float -> by:float ->
    cx:float -> cy:float -> dx:float -> dy:float -> sign

  val triangle_feature_vertex_local : int -> int
  val triangle_feature_edge_local : int -> int
  (** Decode the internal triangle-feature transport used by
      [triangle_triangle_features_into]. Return [-1] when the feature is not a
      vertex/edge respectively. *)

  val segment_triangle_code_packed :
    x:float array -> y:float array -> z:float array ->
    segment_start:int -> segment_end:int ->
    triangle_a:int -> triangle_b:int -> triangle_c:int ->
    int
  (** Allocation-free encoding of [segment_triangle_packed]. This code is an
      internal transport value; do not persist or interpret it outside RDK. *)

  val triangle_triangle_features_into :
    x:float array -> y:float array -> z:float array ->
    left_a:int -> left_b:int -> left_c:int ->
    right_a:int -> right_b:int -> right_c:int ->
    int array -> int
  (** Fill up to two canonical feature pairs as encoded [left; right] entries.
      The output needs length four. Returns [-2] for a degenerate triangle,
      [-1] for coplanar triangles, or the event count [0..2]. The encoding is
      private and allocation-free on the certified predicate path. *)

  val coplanar_triangles_contact_packed :
    x:float array -> y:float array -> z:float array ->
    left_a:int -> left_b:int -> left_c:int ->
    right_a:int -> right_b:int -> right_c:int -> bool
  (** Exact closed contact between two certified nondegenerate coplanar
      triangles. This is the allocation-bounded verifier path after
      [triangle_triangle_features_into] returns [-1]. *)
end
