type feature =
  | Buffer
  | Texture
  | Sampler
  | Compute_pipeline
  | Render_pipeline
  | Queue
  | Surface
  | Ray_tracing
  | Ray_tracing_curves
  | Ray_tracing_motion
  | Unknown of string

type limits = {
  max_buffer_size : int64;
  max_texture_dimension_2d : int;
  max_bind_groups : int;
  max_sample_count : int;
}

type t = {
  limits : limits;
  compute_pipeline : bool;
  render_pipeline : bool;
  ray_tracing : bool;
  ray_tracing_curves : bool;
  ray_tracing_motion : bool;
}

let minimum_m1 =
  {
    limits =
      {
        max_buffer_size = 268_435_456L;
        max_texture_dimension_2d = 16_384;
        max_bind_groups = 4;
        max_sample_count = 4;
      };
    compute_pipeline = true;
    render_pipeline = true;
    ray_tracing = false;
    ray_tracing_curves = false;
    ray_tracing_motion = false;
  }

let validate value =
  let limits = value.limits in
  if
    limits.max_buffer_size <= 0L
    || limits.max_texture_dimension_2d <= 0
    || limits.max_bind_groups <= 0 || limits.max_sample_count <= 0
  then Error (Error.make "Ogpu.Caps.validate" Error.Invalid_argument "limits must be positive")
  else Ok ()

let has value = function
  | Buffer | Texture | Sampler | Queue | Surface -> true
  | Compute_pipeline -> value.compute_pipeline
  | Render_pipeline -> value.render_pipeline
  | Ray_tracing -> value.ray_tracing
  | Ray_tracing_curves -> value.ray_tracing_curves
  | Ray_tracing_motion -> value.ray_tracing_motion
  | Unknown _ -> false

let require ?(operation = "Ogpu.Caps.require") value feature =
  if has value feature then Ok ()
  else
    Error
      (Error.make operation Error.Unsupported "operation is unavailable on this adapter profile")
