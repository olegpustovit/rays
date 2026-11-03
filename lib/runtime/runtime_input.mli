type event = Input.event

val platform : Input.platform
(** The host's modifier conventions, read once from SDL. The only place the platform is read. *)
type snapshot = { pointer : float * float; mouse_delta : float * float; visible : bool }
type t

val create : max_events:int -> logical_width:int -> logical_height:int -> (t, string) result
val push : t -> event -> (unit, string) result
val drain : t -> event list
val begin_frame : t -> unit
val set_relative : t -> bool -> unit

val relative : t -> bool
(** In relative mode [mouse_delta] sums {!add_motion} deltas (SDL relative motion) instead of
    absolute pointer differences. *)

val add_motion : t -> dx:float -> dy:float -> unit

val set_unit : t -> float -> unit
(** Window coordinates per logical point; SDL positions and motion are divided by it as they are
    pushed (1 on macOS, the monitor's scaling on Windows). A non-positive value is ignored. *)

val unit : t -> float
val set_extent : t -> logical_width:int -> logical_height:int -> (unit, string) result
val snapshot : t -> snapshot
