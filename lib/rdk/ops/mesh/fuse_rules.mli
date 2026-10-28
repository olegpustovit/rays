type default_attributes = Keep_first | Average_numeric

type attribute_method =
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

type attribute_rule = {
  pattern : string;
  method_ : attribute_method;
  weight_attribute : string option;
}

type group_method =
  | Group_least_point
  | Group_greatest_point
  | Group_union
  | Group_intersection
  | Group_most_common

type group_rule = { group_pattern : string; group_method : group_method }

val validate :
  attribute_rules:attribute_rule list ->
  group_rules:group_rule list ->
  Geometry.t -> (unit, string) result

val apply :
  ?cancel:Cancel.t ->
  grain:int ->
  default_attributes:default_attributes ->
  attribute_rules:attribute_rule list ->
  group_rules:group_rule list ->
  compact:bool ->
  rewire:bool ->
  Point_clusters.clusters ->
  Geometry.t ->
  Attribute.t list * Group.t list
(** Apply point-attribute and point-group cluster policies. Internal validation
    failures raise [Invalid_argument] for the enclosing Fuse result boundary. *)

(** Shared with [Fuse_reduce]. *)

val select : 'a array -> int array -> 'a array
(** [select source mapping] is [source.(mapping.(i))] for each [i]. *)

val expand : Point_clusters.clusters -> bool -> 'a array -> 'a array
(** Per-cluster values as given when compact, else one per source point. *)

val compare_float_entry : float array -> int array -> int -> int -> int
(** Order two slots by value, then by point index. *)

val sort_range :
  ('a array -> int array -> int -> int -> int) ->
  'a array -> int array -> int -> int -> unit
(** [sort_range compare values points first last] heap-sorts the slots
    [first .. last - 1] of both planes together, in place. *)
