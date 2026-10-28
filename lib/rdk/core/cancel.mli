(** Shareable cancellation token for long-running pure geometry work. *)

type t
exception Cancelled

val create : unit -> t
val cancel : t -> unit
val is_cancelled : t -> bool
val check : t -> unit
val check_opt : t option -> unit

val poll : t option -> int -> int -> unit
(** [poll cancel i mask] is [check_opt cancel] when [i land mask = 0]; [mask] is a power of two minus one. *)
