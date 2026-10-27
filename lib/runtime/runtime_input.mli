type event = Input.event



type snapshot = {
  pointer : float * float;
  mouse_delta : float * float;
  visible : bool;
}

type t

val create : max_events:int ->
  logical_width:int -> logical_height:int -> (t, string) result
val push : t -> event -> (unit, string) result
val drain : t -> event list
val begin_frame : t -> unit

val set_relative : t -> bool -> unit
val relative : t -> bool
(** In relative mode [mouse_delta] sums {!add_motion} deltas (SDL relative
    motion) instead of absolute pointer differences. *)

val add_motion : t -> dx:float -> dy:float -> unit
val set_extent : t -> logical_width:int -> logical_height:int -> (unit, string) result
val snapshot : t -> snapshot
