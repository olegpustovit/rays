(** Lightweight unique-edge lookup for topology-changing output builders.
    Edge order exactly matches {!Topology_index}, but only endpoint and hash
    planes are retained. *)

type t

val create : ?cancel:Cancel.t -> Topology.View.view -> t
val count : t -> int
val endpoints : t -> int -> int * int
val find : t -> a:int -> b:int -> int
