type boundary_mode = Circle | Preserve

val solve :
  ?cancel:Cancel.t ->
  ?grain:int ->
  ?name:string ->
  ?seams:Group.t ->
  ?edge_seams:Edge_group.t ->
  ?uv_tolerance:float ->
  ?iterations:int ->
  ?tolerance:float ->
  boundary_mode ->
  Geometry.t ->
  (Geometry.t, string) result
