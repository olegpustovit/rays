(** Packed projection of selected points onto collision geometry. *)

type method_ = Ray_minimum_distance | Ray_project
type direction =
  | Ray_vector of Rays_math.Vec3.t
  | Ray_normal
  | Ray_attribute of string
type direction_mode = Rdk_spatial.Surface_index.ray_direction_mode =
  | Ray_forward
  | Ray_reverse
  | Ray_bidirectional_closest
  | Ray_bidirectional_farthest
type surface_hit = Rdk_spatial.Surface_index.ray_surface_hit =
  | Ray_first_surface | Ray_last_surface
type combine = Ray_average | Ray_median | Ray_shortest | Ray_longest

(** Every field is required: the SOP declaration owns the defaults and the
    tests build values with [Rdk_test_support.ray]. *)
type params = {
  grain : int;
  selection : Deform.selection option;
  collision_primitives : Group.t option;
  method_ : method_;
  direction : direction;
  direction_mode : direction_mode;
  surface_hit : surface_hit;
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

val run :
  ?cancel:Cancel.t -> params -> source:Geometry.t -> collision:Geometry.t ->
  unit -> (Geometry.t, Error.t) result
