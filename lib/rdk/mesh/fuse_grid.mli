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

type params = {
  cancel : Rdk_core.Cancel.t option;
  grain : int;
  selection : Rdk_core.Group.t option;
  target_selection : Rdk_core.Group.t option;
  targeting : fuse_targeting;
  using : fuse_using;
  tolerance : float;
  position : Fuse_reduce.position;
  weight_attribute : string option;
  attributes : Fuse_reduce.attributes;
  attribute_rules : Fuse_reduce.attribute_rule list;
  group_rules : Fuse_reduce.group_rule list;
  metric : fuse_metric;
  inclusive : bool;
  match_attributes : bool;
  radius_attribute : string option;
  match_attribute : string option;
  match_condition : fuse_match_condition;
  match_tolerance : float;
  modify_target : bool;
  fuse_points : bool;
  keep_fused_points : bool;
  snapped_group : string option;
  snapped_destination_attribute : string option;
  remove_degenerate_primitives : bool;
  remove_unused_points_from_degenerate_primitives : bool;
  remove_all_unused_points : bool;
  target : Rdk_core.Geometry.t option;
}

val fuse : params -> Rdk_core.Geometry.t -> (Rdk_core.Geometry.t, Rdk_core.Error.t) result

val weld :
  ?cancel:Rdk_core.Cancel.t -> grain:int -> ?selection:Rdk_core.Group.t -> tolerance:float ->
  Rdk_core.Geometry.t -> (Rdk_core.Geometry.t, Rdk_core.Error.t) result
(** [fuse] of the points within [tolerance], averaging positions and keeping the first attributes. *)
