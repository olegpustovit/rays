type t
type receipt = { epoch : int64 }
type gpu_timing = { supported : bool; duration_seconds : float; sample_count : int64 }

val gpu_timing_for_device : Device.t -> gpu_timing
val create : Device.t -> (t, Ogpu_core.Error.t) result

val submit_native :
  t -> Metal.Command_buffer.t -> retained:(unit -> unit) list -> (receipt, Ogpu_core.Error.t) result
(** Commits an externally encoded native command buffer; [retained] runs at completion. The queue
    owns [native] only after success. *)

val gpu_duration : t -> int64 -> float option
val wait_through : t -> int64 -> (unit, Ogpu_core.Error.t) result
val poll_through : t -> int64 -> (bool, Ogpu_core.Error.t) result
val completed_epoch : t -> int64
val destroy : t -> (unit, Ogpu_core.Error.t) result

module Private : sig
  val metal : t -> Metal.Command_queue.t
end
