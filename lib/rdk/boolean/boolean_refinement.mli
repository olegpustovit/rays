(** Parallel deterministic per-face Boolean constraint refinement. *)

type t

val build :
  ?cancel:Cancel.t -> ?coplanar:Boolean_coplanar.t ->
  grain:int -> Boolean_constraints.t ->
  (t, Error.t) result

val left_face : t -> int -> Boolean_face_cdt.t option
val right_face : t -> int -> Boolean_face_cdt.t option

module Private : sig
  val constraints : t -> Boolean_constraints.t
end
