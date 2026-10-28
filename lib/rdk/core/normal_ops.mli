(** Packed point and vertex normals. *)
type weighting = Vertex_angle | Each_vertex | Face_area

type params = {
  cancel : Cancel.t option;
  grain : int option;
  selection : Element_selection.t option;
  primitives : Group.t option;
  owner : Attribute.owner;
  weighting : weighting;
  cusp_angle : float;
  attribute : string;
}

val run : params -> Geometry.t -> (Geometry.t, Error.t) result
