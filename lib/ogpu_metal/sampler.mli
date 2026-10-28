type filter = Ogpu_core.Types.sampler_filter = Nearest | Linear
type mip_filter = Ogpu_core.Types.mip_filter = No_mip | Nearest_mip | Linear_mip
type address_mode = Ogpu_core.Types.address_mode = Clamp_to_edge | Repeat | Mirror_repeat
type descriptor = Ogpu_core.Types.sampler_descriptor
type t

val create : Device.t -> descriptor -> (t, Ogpu_core.Error.t) result
val destroy : t -> (unit, Ogpu_core.Error.t) result

module Private : sig
  val metal : t -> Metal.Sampler.t
  val retain_submission : t -> (unit, Ogpu_core.Error.t) result
  val release_submission : t -> unit
end
