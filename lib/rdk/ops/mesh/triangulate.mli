(** Convert selected polygon primitives to triangles while preserving their
    point, vertex, primitive, and detail data. Curves are refused when selected. *)
type params = {
  cancel : Cancel.t option;
  grain : int option;
  primitives : Group.t option;
}

val run : params -> Geometry.t -> (Geometry.t, Error.t) result