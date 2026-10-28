(** Shared deterministic simple-polygon ear clipping. *)

type scratch

val create_scratch : unit -> scratch
(** Reusable single-caller workspace. *)

val find_ear :
  distinct:(int -> int -> int -> int -> bool) ->
  u:float array -> v:float array -> base:int -> orientation:float ->
  remaining:int array -> active:int -> int
(** [find_projected_ear] with a caller-chosen test for which ring points may
    block an ear: [distinct p a b c] is true when point [p] differs from the
    ear corners. *)

val find_projected_ear :
  u:float array -> v:float array -> base:int -> orientation:float ->
  remaining:int array -> active:int -> int
(** Return the first valid ear slot in a prepared projected polygon, or [-1].
    [base] offsets both coordinate and remaining-index planes. *)

val primitive :
  ?cancel:Cancel.t ->
  positions:Packed.Float3.View.view ->
  topology:Topology.View.view ->
  ?scratch:scratch ->
  int ->
  emit:(int -> int -> int -> int -> unit) ->
  (unit, string) result
(** Emit source vertex indices for one polygon in stable ear-clipping order.
    Curves, non-finite positions, degenerate projections, and non-simple
    polygons return an error. The first callback argument is the zero-based
    triangle ordinal within the primitive. Time is O(c²) and scratch is O(c),
    where [c] is the polygon corner count. *)
