type fuse_metric = Euclidean | Componentwise

type params = {
  cancel : Rdk_core.Cancel.t option;
  grain : int;
  selection : Rdk_core.Group.t option;
  target_selection : Rdk_core.Group.t option;
  targeting : Point_snap.targeting;
  using : Point_snap.using;
  tolerance : float;
  position : Fuse_reduce.position;
  weight_attribute : string option;
  attributes : Fuse_rules.default_attributes;
  attribute_rules : Fuse_rules.attribute_rule list;
  group_rules : Fuse_rules.group_rule list;
  metric : fuse_metric;
  inclusive : bool;
  match_attributes : bool;
  radius_attribute : string option;
  match_attribute : string option;
  match_condition : Point_snap.match_condition;
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

type weld_params = {
  cancel : Rdk_core.Cancel.t option;
  grain : int;
  selection : Rdk_core.Group.t option;
  tolerance : float;
}

val weld : weld_params -> Rdk_core.Geometry.t -> (Rdk_core.Geometry.t, Rdk_core.Error.t) result
(** [fuse] of the points within [tolerance], averaging positions and keeping the first attributes. *)
