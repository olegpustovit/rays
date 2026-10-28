(** Packed point and vertex normals. *)
type weighting = Vertex_angle | Each_vertex | Face_area

val run :
  ?cancel:Cancel.t -> ?grain:int -> ?selection:Element_selection.t ->
  ?primitives:Group.t -> ?owner:Attribute.owner -> ?weighting:weighting ->
  ?cusp_angle:float -> ?attribute:string -> Geometry.t -> (Geometry.t, Error.t) result
