(** Explicit, bounded graph-evaluation state. A session is single-caller: it
    may run independent branches and RDK kernels on the shared pool, but
    concurrent calls to [cook] on the same session are not supported. Worker
    cache metadata is local; parent mutation joins in input order. *)

type t

type node_timing = {
  node_id : int;
  label : string;
  operation : string;
  input_points : int;
  points : int;
  seconds : float;
  cache_hit : bool;
}

type stats = {
  cooks : int;
  hits : int;
  misses : int;
  evictions : int;
  volatile_hits : int;  (* subset of [hits] *)
  volatile_misses : int;  (* subset of [misses] *)
  retained_entries : int;
  volatile_entries : int;  (* one slot per volatile node, outside the LRU *)
  retained_payload_bytes : int;
  last_node : node_timing option;
}

type output = {
  payload : Payload.t;
  instances : Rays_math.Mat4.t array option;
  (** A packed result (e.g. Copy to Points with Pack and instance): draw
      the [Geometry] payload once per transform. A node that consumes it receives it
      materialized. *)
}

val create : max_entries:int -> max_payload_bytes:int -> (t, string) result

val set_volatile : t -> (int -> bool) -> unit
(** [set_volatile session predicate] (by node id; default none) marks nodes whose cache key changes
    every cook (time-driven nodes and what is downstream of them). A volatile
    node keeps exactly one entry, its latest, replaced in place; it is never
    counted in or evicted from the LRU, so it cannot push static entries out.
    Its lookups count in [hits] and [misses] and in [volatile_hits] and
    [volatile_misses]; its payload is not counted in [retained_payload_bytes]
    (bounded by the volatile node count; ponytail: one slot, so scrubbing
    back recooks). Replaces the previous predicate (a new lowering) and drops
    the slots of nodes that stopped being volatile. *)

val cook : t -> context:Context.t -> Node.t -> (output, Diagnostic.t) result

val node_seconds : t -> int -> float option
(** The seconds node [id] took the last time it was really cooked (its own work, not its inputs';
    a cache hit leaves it).  Safe to read from another domain; at most 4,096 nodes are kept. *)

val stats : t -> stats
val clear : t -> unit
val close : t -> unit

val set_parallel_override : t -> bool option -> unit
(** Force or disable the coarse branch gate for exactness checks. [None]
    restores measured placement. One-domain contexts remain sequential. *)

module Private : sig
  val fanouts : t -> int
  val cache_keys : t -> string list
  val clear_cache_keep_timings : t -> unit
  (** Cache-cold benchmarks after learning branch costs; counters are kept. *)
end
