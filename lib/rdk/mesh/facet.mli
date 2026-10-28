
val run :
  ?cancel:Cancel.t -> ?grain:int ->
  ?selection:Deform.selection -> ?pre_compute_normals:bool -> ?make_normals_unit_length:bool ->
  ?unique_points:bool -> ?consolidate_distance:float ->
  ?consolidate_normals_distance:float -> ?remove_inline_points:bool ->
  ?inline_distance:float -> ?orient_polygons:bool -> ?cusp_angle:float ->
  ?remove_degenerate:bool -> ?make_planar:bool ->
  ?post_compute_normals:bool -> ?reverse_normals:bool ->
  Geometry.t -> (Geometry.t, Error.t) result
