type using = Least_target_point | Closest_target_point
type match_condition = Equal_attribute_values | Unequal_attribute_values

type targeting =
  | Near_points
  | Specified_points of string

val plan :
  ?cancel:Cancel.t ->
  grain:int ->
  ?queries:Group.t ->
  ?targets:Group.t ->
  targeting:targeting ->
  using:using ->
  tolerance:float ->
  metric:Point_clusters.metric ->
  inclusive:bool ->
  ?radius_attribute:string ->
  ?match_attribute:string ->
  match_condition:match_condition ->
  match_tolerance:float ->
  source:Geometry.t ->
  target:Geometry.t ->
  unit ->
  (int array, string) result
(** Return one target point number per source point, or [-1] for no snap.
    Only selected queries are evaluated and only selected targets are eligible.
    Equal candidates are resolved deterministically by the requested policy. *)
