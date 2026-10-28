type params = { cancel : Cancel.t option; grain : int option; primitives : Group.t option }
(** Convert selected polygon primitives to triangles while preserving their point, vertex,
    primitive, and detail data. Curves are refused when selected. *)

val run : params -> Geometry.t -> (Geometry.t, Error.t) result
