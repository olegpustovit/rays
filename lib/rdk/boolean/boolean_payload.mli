(** Private primitive-owned payload transfer for exact Boolean extraction. *)

val copy_primitives :
  ?cancel:Cancel.t -> grain:int -> Boolean_extract.ancestry ->
  (Geometry.t, Error.t) result
(** Copy the union of primitive attribute and primitive group schemas from the
    two exact source operands. Every output facet reads its selected source
    primitive; a field missing on that operand receives its storage default.
    Same-name attributes with different storage are rejected. *)

type point_conflict =
  | Reject
  | Promote_to_vertex

val copy_points_and_vertices :
  ?cancel:Cancel.t -> grain:int -> point_conflict:point_conflict ->
  point_tolerance:float ->
  Boolean_extract.ancestry -> Geometry.t -> (Geometry.t, Error.t) result
(** Interpolate point/vertex payload through exact corner barycentrics. Point
    payload must agree at shared output points under [point_tolerance] or be
    explicitly promoted. The tolerance is finite, non-negative, and applies
    only to numeric agreement; discrete values and groups agree exactly.
    Fixed-width numeric storage interpolates, [N] Float3 normalizes, discrete
    storage chooses the stable dominant source corner, and equal-width
    Float-array rows interpolate componentwise. Native edge groups are unioned
    through all exact coincident-member and split-edge ancestry. *)
