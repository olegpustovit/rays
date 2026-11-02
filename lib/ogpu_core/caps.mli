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
  | Ray_tracing_refit
  | Memoryless_attachments
      (** Render attachments with no memory: their contents exist only inside one pass (tile memory).
          A backend without them draws to ordinary attachments. *)
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
  ray_tracing_refit : bool;
  memoryless_attachments : bool;
  unified_memory : bool;  (** Host and device bytes are one pool (Apple silicon): the process footprint already counts the device. *)
}

val minimum_m1 : t
val validate : t -> (unit, Error.t) result
val has : t -> feature -> bool
val require : ?operation:string -> t -> feature -> (unit, Error.t) result
