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

type attributes = Fuse_rules.default_attributes = Keep_first | Average_numeric

type attribute_method = Fuse_rules.attribute_method =
  | Attribute_average
  | Attribute_least_point
  | Attribute_greatest_point
  | Attribute_maximum
  | Attribute_minimum
  | Attribute_mode
  | Attribute_median
  | Attribute_sum
  | Attribute_sum_squares
  | Attribute_root_mean_square
  | Attribute_concatenate
  | Attribute_weighted_average
  | Attribute_weighted_sum
  | Attribute_minimum_weight
  | Attribute_maximum_weight
  | Attribute_concatenate_weight_order

type attribute_rule = Fuse_rules.attribute_rule = {
  pattern : string;
  method_ : attribute_method;
  weight_attribute : string option;
}

type group_method = Fuse_rules.group_method =
  | Group_least_point
  | Group_greatest_point
  | Group_union
  | Group_intersection
  | Group_most_common

type group_rule = Fuse_rules.group_rule = {
  group_pattern : string;
  group_method : group_method;
}

val apply :
  ?cancel:Cancel.t ->
  grain:int ->
  position:position ->
  ?weight_attribute:string ->
  attributes:attributes ->
  attribute_rules:attribute_rule list ->
  group_rules:group_rule list ->
  compact:bool ->
  rewire:bool ->
  ?remap_edge_groups:bool ->
  Point_clusters.clusters ->
  Geometry.t ->
  (Geometry.t, string) result
