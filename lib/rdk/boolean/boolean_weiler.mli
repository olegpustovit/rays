(** View Weiler half-facet adjacency and local shell graph. *)

type side = Negative | Positive
type t

val build :
  ?cancel:Cancel.t -> Boolean_complex.t -> Boolean_radial.t ->
  (t, Error.t) result

val half_facet : int -> side -> int

val shell_count : t -> int
val half_facet_shell : t -> int -> int

val facet_left_winding : t -> int -> int
val facet_right_winding : t -> int -> int
(** Signed operand contributions relative to the complex facet's stored
    orientation. Crossing Negative -> Positive subtracts these values. *)

module View : sig
  val complex : t -> Boolean_complex.t
end
