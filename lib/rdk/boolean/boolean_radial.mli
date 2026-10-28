(** View exact radial ordering of surface charts around complex edges. *)

type t

val build : ?cancel:Cancel.t -> Boolean_complex.t -> (t, Error.t) result
val incident_range : t -> int -> int * int
val incident_facet : t -> int -> int
val incident_local : t -> int -> int
(** Incident charts are in deterministic cyclic order about the complex
    edge's [first -> second] direction. *)

module View : sig
  val complex : t -> Boolean_complex.t
end
