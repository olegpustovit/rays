type fuse_metric = Euclidean | Componentwise
type fuse_using = Point_snap.using =
  | Least_target_point
  | Closest_target_point
type fuse_match_condition = Point_snap.match_condition =
  | Equal_attribute_values
  | Unequal_attribute_values
type fuse_targeting = Point_snap.targeting =
  | Near_points
  | Specified_points of string
type grid_rounding = Grid_nearest | Grid_down | Grid_up

val fuse :
  ?cancel:Rdk_core.Cancel.t ->
  ?grain:int ->
  ?selection:Rdk_core.Group.t ->
  ?target_selection:Rdk_core.Group.t ->
  ?targeting:fuse_targeting ->
  ?using:fuse_using ->
  ?tolerance:float ->
  ?position:Fuse_reduce.position ->
  ?weight_attribute:string ->
  ?attributes:Fuse_reduce.attributes ->
  ?attribute_rules:Fuse_reduce.attribute_rule list ->
  ?group_rules:Fuse_reduce.group_rule list ->
  ?metric:fuse_metric ->
  ?inclusive:bool ->
  ?match_attributes:bool ->
  ?radius_attribute:string ->
  ?match_attribute:string ->
  ?match_condition:fuse_match_condition ->
  ?match_tolerance:float ->
  ?modify_target:bool ->
  ?fuse_points:bool ->
  ?keep_fused_points:bool ->
  ?snapped_group:string ->
  ?snapped_destination_attribute:string ->
  ?remove_degenerate_primitives:bool ->
  ?remove_unused_points_from_degenerate_primitives:bool ->
  ?remove_all_unused_points:bool ->
  ?target:Rdk_core.Geometry.t ->
  Rdk_core.Geometry.t -> (Rdk_core.Geometry.t, Rdk_core.Error.t) result

