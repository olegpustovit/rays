type boundary = Smooth_free | Smooth_unshared
val run :
  ?cancel:Rdk_core.Cancel.t ->
  ?grain:int ->
  ?constrained_points:Rdk_core.Group.t ->
  ?boundary:boundary ->
  ?iterations:int ->
  ?mode:Attribute_ops.blur_mode ->
  ?recompute_normals:bool ->
  attributes:string ->
  Rdk_core.Geometry.t -> (Rdk_core.Geometry.t, Rdk_core.Error.t) result
