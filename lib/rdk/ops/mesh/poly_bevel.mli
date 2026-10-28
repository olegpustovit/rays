type shape = Bevel_chamfer | Bevel_round of { convexity : float }

type params = {
  cancel : Rdk_core.Cancel.t option;
  grain : int option;
  edges : Rdk_core.Edge_group.t option;
  shape : shape;
  divisions : int;
  point_scale_attribute : string option;
  ignore_flat_angle : float option;
  clamp_overlap : bool;
  edge_group : string option;
  corner_group : string option;
  offset_group : string option;
  recompute_point_normals : bool;
  distance : float;
}

val run : params -> Rdk_core.Geometry.t -> (Rdk_core.Geometry.t, Error.t) result
