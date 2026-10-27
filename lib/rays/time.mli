(* Private per-sketch realtime clock and frame pacing. *)

type t
val create : target_fps:int -> vsync_enabled:bool -> t
val now : t -> float
val delta : t -> float
val update : t -> unit
val limit_frame_rate : t -> unit
