type boundary = Smooth_free | Smooth_unshared | Smooth_group_boundary
val run :
  ?cancel:Rdk_core.Cancel.t ->
  ?grain:int ->
  ?primitives:Rdk_core.Group.t ->
  ?constrained_points:Rdk_core.Group.t ->
  ?boundary:boundary ->
  ?iterations:int ->
  ?method_:Rdk_attrib.Attribute_ops.blur_method ->
  ?mode:Rdk_attrib.Attribute_ops.blur_mode ->
  ?weight_attribute:string ->
  ?recompute_normals:bool ->
  attributes:string ->
  Rdk_core.Geometry.t -> (Rdk_core.Geometry.t, Rdk_core.Error.t) result
