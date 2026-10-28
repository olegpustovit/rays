type keep = Above | Below | All

type params = {
  cancel : Rdk_core.Cancel.t option;
  grain : int;
  keep : keep;
  snapping_tolerance : float;
  fill : bool;
  split_connectivity : bool;
  clip_attribute : string;
  distance : float;
  selection : Deform.selection option;
  replace_existing_groups : bool;
  clipped_edge_group : string option;
  cap_group : string option;
  clipped_group : string option;
  above_group : string option;
  below_group : string option;
  origin : Rays_math.Vec3.t;
  normal : Rays_math.Vec3.t;
}

val clip : params -> Rdk_core.Geometry.t -> (Rdk_core.Geometry.t, Rdk_core.Error.t) result
