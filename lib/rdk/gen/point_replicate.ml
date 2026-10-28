
type shape =
  | Replicate_point
  | Replicate_box
  | Replicate_sphere
  | Replicate_disk
  | Replicate_line
  | Replicate_custom

type velocity_stretch =
  | Replicate_no_velocity_stretch
  | Replicate_scaled_velocity
  | Replicate_velocity_only

let error message = Error ("Rdk_gen.Point_replicate.point_replicate: " ^ message)

type quasi_data = {
  offset_u : float array;
  offset_v : float array;
  offset_w : float array;
  sequence_v : float array;
  sequence_w : float array;
}

