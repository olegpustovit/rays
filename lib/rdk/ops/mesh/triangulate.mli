(** Convert selected polygon primitives to triangles while preserving their
    point, vertex, primitive, and detail data. Curves are refused when selected. *)
val run :
  ?cancel:Cancel.t -> ?grain:int -> ?primitives:Group.t -> Geometry.t ->
  (Geometry.t, Error.t) result
