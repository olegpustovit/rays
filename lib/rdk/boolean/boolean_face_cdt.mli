(** Deterministic exact-predicate constrained Delaunay refinement of one face. *)

type t
type workspace

val create_workspace : unit -> workspace
(** Exclusively owned scratch reused across sequential face refinements. *)

val build :
  ?cancel:Cancel.t ->
  ?workspace:workspace ->
  Boolean_constraints.t ->
  Boolean_face_arrangement.t ->
  side:Boolean_constraints.side ->
  triangle:int ->
  (t, Error.t) result
(** Insert the arrangement's exact-unique points, recover every constraint, and canonicalize
    unconstrained diagonals with exact incircle predicates. Delegates to the shared [Planar_cdt]
    strip/cavity recovery kernel. Storage is O(points + triangles + constraints + queued local
    edges). *)

val point_count : t -> int
val triangle_count : t -> int
val triangle_point : t -> int -> int -> int

module View : sig
  val point : t -> int -> Implicit_point.t
  val global_point_token : t -> int -> int
  val source_winding : t -> int
end
