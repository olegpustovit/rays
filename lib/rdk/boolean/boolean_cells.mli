(** View exact-axis shell winding classification for a Weiler complex. *)

type t

val build :
  ?cancel:Cancel.t ->
  ?track_left:bool -> ?track_right:bool ->
  Boolean_complex.t -> Boolean_weiler.t ->
  (t, Error.t) result
(** An operand whose [track_*] flag is false remains part of the arrangement,
    but contributes zero to volumetric winding. This is the exact surface
    treatment used by the Boolean product layer. *)

val shell_count : t -> int
val left_winding : t -> int -> int
val right_winding : t -> int -> int
(** Signed generalized winding values. Non-zero means inside for the default
    solid interpretation; signs preserve source orientation diagnostics. *)

module View : sig
  val weiler : t -> Boolean_weiler.t
end
