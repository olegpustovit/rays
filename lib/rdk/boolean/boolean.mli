(** Exact treatment-aware polygon Boolean products. *)

type seam_points = Shared_seam_points | Split_seam_points
type seam_output = Seam_curves | Coincident_patches

type params = {
  cancel : Cancel.t option;
  grain : int;
  operation : Boolean_solid.operation;
  left_treatment : Boolean_solid.treatment;
  right_treatment : Boolean_solid.treatment;
  resolve_left_self_intersections : bool;
  resolve_right_self_intersections : bool;
  point_conflict : Boolean_payload.point_conflict;
  point_tolerance : float;
  tiny_seam_threshold : float;
  cleanup_max_batches : int;
  strict_cleanup : bool;
  seam_points : seam_points;
  detriangulation : Boolean_materialization.detriangulation;
  assume_flat : bool;
  require_closed : bool option;
  piece_attribute : string option;
  left_piece_group : string option;
  overlap_piece_group : string option;
  right_piece_group : string option;
  right : Geometry.t;
}

val run : params -> Geometry.t -> (Geometry.t, Error.t) result

(** Build one exact two-operand arrangement and evaluate a polygon Boolean. Both operands may be
    oriented solids or zero-volume surfaces. Point/vertex/primitive attributes and ordinary/native
    groups transfer through exact source ancestry; conflicting point fields are rejected or promoted
    explicitly. Standard point/vertex [N] is interpolated, normalized, and negated whenever
    extraction reverses its source facet, so it remains aligned with output winding. Surface
    subtraction emits paired opposite-facing cut walls. A finite non-negative tiny-seam threshold
    enables bounded, independently certified contraction batches; strict cleanup refuses to publish
    while candidates remain after the explicit batch bound. Independently of that optional cleanup,
    exact vertices that cannot be represented distinctly in binary64 pass through a mandatory
    bounded repair: only zero-area closed/pair-canceling subcomplexes or ULP-scale edges are
    considered, ancestry is composed through every change, and the complete rounded
    incidence/self-contact verifier remains the publication gate.

    [piece_attribute] optionally writes a dense, deterministic primitive integer attribute
    identifying the exact oriented Weiler cell bounded by each output face. Unlike split seam
    points, this keeps all exterior and cut-wall polygons of one closed fragment under one identity
    while allowing adjacent fragments to share seam points.

    Output is triangle-only by default. Detriangulation reconstructs only unchanged source polygons
    or every coplanar same-source polygon and never crosses an exact seam. Seam points may remain
    shared or be duplicated per incident primitive component. Closed validation defaults on only for
    two solid operands. All phases honor cancellation and stable packed ordering; parallel face work
    and payload transfer are exact across domain counts.

    [Shatter] requires two solids and emits A-only, overlap, and B-only boundaries in that stable
    order. Optional distinct primitive-group names identify the three products; passing [None]
    suppresses a group. Shared walls are intentionally duplicated between adjacent products.

    Broad phase is expected O((A+B) log(A+B)+C) for input triangles [A]/[B] and candidate contacts
    [C]. Exact refinement, radial assembly, payload, and materialization are output-sensitive in the
    refined arrangement size; the unavoidable worst case is quadratic when every source/refined AABB
    overlaps. Packed topology/ancestry and spatial indexes are linear in input plus refined output,
    excluding exact-construction limb storage. Cleanup is bounded by [cleanup_max_batches], with
    each batch linearithmic in current surface edges plus exact rounded verification candidates. *)

val seam :
  ?cancel:Cancel.t ->
  ?grain:int ->
  ?output:seam_output ->
  ?left_treatment:Boolean_solid.treatment ->
  ?right_treatment:Boolean_solid.treatment ->
  ?resolve_left_self_intersections:bool ->
  ?resolve_right_self_intersections:bool ->
  ?left_self_group:string option ->
  ?between_group:string option ->
  ?right_self_group:string option ->
  ?coincident_group:string option ->
  right:Geometry.t ->
  Geometry.t ->
  (Geometry.t, Error.t) result
(** Build the same exact arrangement and materialize either deterministic chained seam curves or
    coincident-area triangles. Curve primitives may be named independently as left
    self-intersection, between-input, and right self-intersection products. The coincident output
    can receive one complete primitive group. No approximate proximity edge is introduced. *)
