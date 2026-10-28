type t
(** Metal acceleration structures over the generic [Metal.Acceleration_structure.Build] descriptors,
    encoded on a caller-owned acceleration encoder. Sized structures (copy and compaction targets)
    carry no descriptor. *)

val create :
  Device.t ->
  Ogpu_core.Backend.driver_accel_descriptor ->
  resolve_buffer:(int64 -> (Buffer.t, Ogpu_core.Error.t) result) ->
  resolve_structure:(int64 -> (t, Ogpu_core.Error.t) result) ->
  (t, Ogpu_core.Error.t) result

val sizes : t -> Metal.Acceleration_structure.sizes

val encode_build :
  Metal.Acceleration_encoder.t ->
  Device.t ->
  t ->
  scratch:Buffer.t ->
  scratch_offset:int64 ->
  (unit, Ogpu_core.Error.t) result

val encode_compact :
  Metal.Acceleration_encoder.t -> Device.t -> src:t -> dst:t -> (unit, Ogpu_core.Error.t) result

val encode_compacted_size :
  Metal.Acceleration_encoder.t ->
  Device.t ->
  t ->
  destination:Buffer.t ->
  offset:int64 ->
  (unit, Ogpu_core.Error.t) result

val destroy : t -> (unit, Ogpu_core.Error.t) result

module Private : sig
  val metal : t -> Metal.Acceleration_structure.t

  val retain_submission : t -> (unit -> unit, Ogpu_core.Error.t) result
  (** Retains the structure and its buffers for one submission; the returned closure releases them
      and completes a deferred destroy. *)
end
