
type rest_mode = Store_rest | Extract_rest | Swap_rest
type rest_normals = No_rest_normals | Rest_normals_if_present | Rest_normals_always

type velocity_approximation = Backward_difference | Central_difference | Forward_difference
type velocity_initialization =
  | Compute_from_deformation
  | Keep_incoming
  | Set_value of Rays_math.Vec3.t
  | From_attribute of { name : string; scale : float }
type velocity_unmatched = Velocity_unmatched_error | Velocity_unmatched_zero
