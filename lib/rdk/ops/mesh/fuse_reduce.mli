type position =
  | First_position
  | Least_point_position
  | Greatest_point_position
  | Average_position
  | Minimum_position
  | Maximum_position
  | Mode_position
  | Median_position
  | Sum_position
  | Sum_squares_position
  | Root_mean_square_position
  | Weighted_average_position
  | Weighted_sum_position
  | Minimum_weight_position
  | Maximum_weight_position






type params = {
  cancel : Cancel.t option;
  grain : int;
  position : position;
  weight_attribute : string option;
  attributes : Fuse_rules.default_attributes;
  attribute_rules : Fuse_rules.attribute_rule list;
  group_rules : Fuse_rules.group_rule list;
  compact : bool;
  rewire : bool;
  remap_edge_groups : bool;
}

val apply : params -> Point_clusters.clusters -> Geometry.t -> (Geometry.t, string) result