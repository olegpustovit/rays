(** Packed projection of selected points onto collision geometry. *)

type method_ = Ray_minimum_distance | Ray_project
type direction = Ray_vector of Rays_math.Vec3.t | Ray_normal | Ray_attribute of string
type combine = Ray_average | Ray_median | Ray_shortest | Ray_longest

type params = {
  grain : int;
  selection : Element_selection.t option;
  collision_primitives : Group.t option;
  method_ : method_;
  direction : direction;
  direction_mode : Surface_index.ray_direction_mode;
  surface_hit : Surface_index.ray_surface_hit;
  samples : int;
  jitter_scale : float;
  seed : int;
  combine : combine;
  min_distance : float;
  max_distance : float option;
  tolerance : float;
  scale : float;
  lift : float;
  distance_attribute : string option;
  primitive_attribute : string option;
  source_vertex_numbers_attribute : string option;
  source_vertex_weights_attribute : string option;
  hit_group : string option;
  normal_attribute : string option;
  point_pattern : string option;
  vertex_pattern : string option;
  primitive_pattern : string option;
  detail_pattern : string option;
  match_groups : bool;
}
(** Every field is required: the SOP declaration owns the defaults and the tests build values with
    [Rdk_test_support.ray]. *)

val run :
  ?cancel:Cancel.t ->
  params ->
  source:Geometry.t ->
  collision:Geometry.t ->
  unit ->
  (Geometry.t, Error.t) result
