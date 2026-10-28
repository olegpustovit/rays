val raw :
  ?cancel:Cancel.t -> ?grain:int -> ?edges:Edge_group.t ->
  ?connectivity_attribute:string -> ?position:Fuse_reduce.position ->
  ?remove_degenerate_primitives:bool -> ?recompute_point_normals:bool ->
  Geometry.t -> (Geometry.t, string) result

type params = {
  cancel : Cancel.t option;
  grain : int option;
  edges : Edge_group.t option;
  connectivity_attribute : string option;
  position : Fuse_reduce.position option;
  remove_degenerate_primitives : bool option;
  recompute_point_normals : bool option;
}

val run : params -> Geometry.t -> (Geometry.t, Error.t) result