(** Deterministic sketch-owned playback clock.

    The runtime frame clock always advances, while this clock can pause, stop, and reset without
    touching global state. It reads no keys: the host drives {!val-toggle_pause}, {!val-stop}, and
    {!val-reset} from its keymap. *)

type mode = Playing | Paused | Stopped
type t
type change = Advanced | Paused_now | Resumed | Stopped_now | Reset_now | Seeked

val create : unit -> t
val update : t -> Rays.Frame.t -> t * change list
val toggle_pause : t -> t * change list
val stop : t -> t * change list
val reset : t -> t * change list

val seek : t -> frame:int64 -> t * change list
(** Jump to [frame] (clamped at 0) and pause; time is [frame] times the mean step observed so far,
    or 1/60 s before any. [Seeked] changes the context. *)

val mode : t -> mode
val time : t -> float
val frame : t -> int64
val changed_context : change list -> bool
