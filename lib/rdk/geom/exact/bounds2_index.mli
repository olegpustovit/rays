(** Packed immutable BVH over two-dimensional closed bounds. *)

type t

val create :
  ?cancel:Cancel.t ->
  min_x:float array -> min_y:float array ->
  max_x:float array -> max_y:float array -> unit -> t
(** Borrow equal-length immutable bound planes and build a deterministic median
    tree. Bounds may be infinite for conservative queries, but not NaN, and
    every minimum must be at most its maximum. *)

val query :
  ?cancel:Cancel.t -> t ->
  min_x:float -> min_y:float -> max_x:float -> max_y:float ->
  (int -> unit) -> unit
(** Visit every source bound overlapping the closed query rectangle. Visit
    order is an implementation detail; consumers that select one item must
    apply their own stable tie break. *)

val candidate_pairs : ?cancel:Cancel.t -> t -> (int -> int -> unit) -> unit
(** Visit every unordered pair of overlapping source bounds exactly once,
    with the lower source index first. *)
