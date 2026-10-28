(** Immutable topology-affine groups of unique undirected edges.

    Edge indices belong to a specific {!Topology_index.t}; unlike point, vertex, and primitive
    groups they cannot be interpreted safely after a topology change without an explicit endpoint
    remap. *)

type t
type edge_group = t

val init :
  ?grain:int -> topology:Topology.t -> index:Topology_index.t -> name:string -> (int -> bool) -> t
(** Build a packed edge bitset. [index] must have been derived from [topology]. *)

val name : t -> string
val topology_data_id : t -> int
val length : t -> int
val data_id : t -> int
val with_name : string -> t -> t
val payload_bytes : t -> int
val mem : int -> t -> bool
val cardinality : t -> int
val union : t -> t -> (t, string) result
val intersection : t -> t -> (t, string) result
val difference : t -> t -> (t, string) result
val symmetric_difference : t -> t -> (t, string) result

(* Invert membership while preserving topology affinity and clearing packed
   padding bits. *)
val iter : (int -> unit) -> t -> unit

val remap :
  ?cancel:Cancel.t ->
  source_index:Topology_index.t ->
  target_topology:Topology.t ->
  target_index:Topology_index.t ->
  point_map:int array ->
  t ->
  (t, string) result
(** Remap by canonical edge endpoints. [point_map] maps every source point to a target point, or
    [-1] when deleted. Collapsed or absent target edges are omitted and many source edges may merge
    into one target edge. *)

val replicate_exact_copies :
  ?cancel:Cancel.t ->
  source_topology:Topology.t ->
  source_index:Topology_index.t ->
  target_topology:Topology.t ->
  copies:int ->
  t ->
  (t, string) result
(** Replicate into exact copy-major topology without constructing the target reverse-topology index.
    The target is validated as [copies] concatenated copies of the source before edge ordinals are
    replicated. *)

module Builder : sig
  type t

  val create : topology:Topology.t -> index:Topology_index.t -> name:string -> t
  val set : t -> int -> bool -> unit
  val freeze : t -> edge_group
end

module View : sig
  val of_owned_bits : topology:Topology.t -> edge_count:int -> name:string -> bytes -> t
  (** Trusted ownership-transfer boundary for kernels that prove the target unique-edge order
      without constructing a {!Topology_index.t}. *)
end
