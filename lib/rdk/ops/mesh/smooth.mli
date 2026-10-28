type boundary = Smooth_free | Smooth_unshared

type params = {
  cancel : Rdk_core.Cancel.t option;
  grain : int option;
  constrained_points : Rdk_core.Group.t option;
  boundary : boundary;
  iterations : int;
  mode : Attribute_blur.mode;
  recompute_normals : bool;
  attributes : string;
}

val run : params -> Rdk_core.Geometry.t -> (Rdk_core.Geometry.t, Rdk_core.Error.t) result
