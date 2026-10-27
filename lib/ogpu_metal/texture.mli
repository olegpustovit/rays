type format = Ogpu_core.Types.texture_format = R8_unorm | Rgba8_unorm | Bgra8_unorm | Bgra8_unorm_srgb | Rgba16_float | Rgba32_float | Depth32_float
type memory = Ogpu_core.Types.memory = Shared | Device_local
type t

val create : Device.t -> memory:memory -> format:format ->
  Ogpu_core.Types.texture_descriptor ->
  (t, Ogpu_core.Error.t) result

val descriptor : Device.t -> t -> (Ogpu_core.Types.texture_descriptor, Ogpu_core.Error.t) result
val format : Device.t -> t -> (format, Ogpu_core.Error.t) result
val read_bytes : Device.t -> t -> mip_level:int -> bytes_per_row:int ->
  (bytes, Ogpu_core.Error.t) result
val read_bytes_into : Device.t -> t -> mip_level:int -> bytes_per_row:int ->
  destination:bytes -> (unit, Ogpu_core.Error.t) result
val destroy : t -> (unit, Ogpu_core.Error.t) result

module Private : sig
  val metal : t -> Metal.Texture.t
  val retain_submission : t -> (unit,Ogpu_core.Error.t) result
  val release_submission : t -> unit
end
