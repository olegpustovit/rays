(** Deterministic convex hull construction over packed point positions. *)

val run :
  ?cancel:Cancel.t ->
  ?grain:int ->
  ?selection:Element_selection.t ->
  ?preserve_point_payload:bool ->
  ?source_point_attribute:string ->
  ?hull_group:string ->
  Geometry.t ->
  (Geometry.t, Error.t) result
(** Build the exact combinatorial convex hull of the selected input points.
    One unique point produces a free point, collinear inputs produce one open
    endpoint segment, coplanar inputs produce one closed convex polygon, and a
    full-dimensional input produces an outward closed triangle surface.

    Exact predicates own every affine, visibility, and horizon decision.
    Floating-point face distance is used only to choose a legal pivot from an
    already exactly classified outside set. *)
