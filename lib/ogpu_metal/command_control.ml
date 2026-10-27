type resource = Buffer of Buffer.t | Texture of Texture.t

type t = {
  operation : string;
  device : Device.t;
  native : Metal.Command_buffer.t;
  native_of : 'a. ('a, Metal.error) result -> ('a, Ogpu_core.Error.t) result;
  open_encoder : (unit -> unit) option ref;
  recording : unit -> (unit, Ogpu_core.Error.t) result;
  keep :
    (unit -> (unit, Ogpu_core.Error.t) result) -> (unit -> unit) -> (unit, Ogpu_core.Error.t) result;
  keep_buffer : Buffer.t -> (unit, Ogpu_core.Error.t) result;
  find_buffer : int64 -> (Buffer.t, Ogpu_core.Error.t) result;
  find_accel : int64 -> (Acceleration.t, Ogpu_core.Error.t) result;
  keep_accel : Acceleration.t -> (unit, Ogpu_core.Error.t) result;
  find_texture : int64 -> (Texture.t, Ogpu_core.Error.t) result;
  keep_texture : Texture.t -> (unit, Ogpu_core.Error.t) result;
  resource : int64 -> resource option;
  pipeline : int64 -> Pipeline.t option;
  sampler : int64 -> Sampler.t option;
  table : int64 -> (Metal.Intersection_function_table.t, Ogpu_core.Error.t) result;
  icb : int64 -> Metal.Indirect_command_buffer.t option;
  depth_state : Ogpu_core.Backend.depth_state -> (Metal.Depth_stencil.t, Ogpu_core.Error.t) result;
}

let error operation kind message = Error (Ogpu_core.Error.make operation kind message)
