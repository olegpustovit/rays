type texture_profile = {
  format : Types.texture_format;
  storage : Types.memory;
  width : int;
  height : int;
  depth : int;
  mip_levels : int;
  sample_count : int;
  usage : Types.texture_usage list;
}

val validate_label : operation:string -> string option -> (unit, Error.t) result
val validate_buffer : Caps.t -> Types.buffer_descriptor -> (unit, Error.t) result
val validate_texture : Caps.t -> Types.texture_descriptor -> (unit, Error.t) result
val validate_sampler : Types.sampler_descriptor -> (unit, Error.t) result

val validate_texture_shape :
  operation:string ->
  max_dimension:int ->
  max_samples:int ->
  width:int ->
  height:int ->
  depth:int ->
  mip_levels:int ->
  sample_count:int ->
  usage_count:int ->
  (unit, Error.t) result

val validate_texture_profile : Caps.t -> texture_profile -> (unit, Error.t) result
val validate_layout : operation:string -> (int * int list) list -> (unit, Error.t) result
val validate_groups : operation:string -> max_groups:int -> int list -> (unit, Error.t) result
