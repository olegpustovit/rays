val polygon_area_vector :
  ?cancel:Cancel.t ->
  grain:int ->
  need_inverse:bool ->
  first_failure:bool ->
  ?primitives:Group.t ->
  operation:string ->
  Geometry.t ->
  ((float array * float array * float array) * float array, string) result
(** Compute raw polygon fan area vectors; the optional inverse lengths are used by owner-aware
    normal weighting. [first_failure] preserves the compatibility path's first-error reporting
    within each work range. *)

val compute :
  ?cancel:Cancel.t ->
  ?grain:int ->
  ?primitives:Group.t ->
  operation:string ->
  Geometry.t ->
  (float array * float array * float array, string) result
