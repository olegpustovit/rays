val raw :
  ?cancel:Cancel.t -> ?grain:int -> ?edges:Edge_group.t ->
  ?connectivity_attribute:string -> ?position:Fuse_reduce.position ->
  ?remove_degenerate_primitives:bool -> ?recompute_point_normals:bool ->
  Geometry.t -> (Geometry.t, string) result

val run :
  ?cancel:Cancel.t -> ?grain:int -> ?edges:Edge_group.t ->
  ?connectivity_attribute:string -> ?position:Fuse_reduce.position ->
  ?remove_degenerate_primitives:bool -> ?recompute_point_normals:bool ->
  Geometry.t -> (Geometry.t, Error.t) result
