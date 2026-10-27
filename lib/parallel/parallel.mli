(** Safe coarse-grained parallel computation for sketches.

    Functions passed here must not call SDL, [Graphics], [Scene.render], or
    mutate shared unprotected state. *)

val recommended_domains : unit -> int

val run : ?domains:int -> (unit -> 'a) -> 'a
(** Run a computation with a cached process-wide work-stealing pool. Pools are
    reused by domain count and torn down at process exit. Nested calls reuse
    the current execution context. [domains] includes the calling domain;
    [domains:1] forces every nested [Parallel] helper to execute sequentially. *)

val release_current_domain_pools : unit -> unit
(** Tear down cached pools created by the calling domain. Long-lived background
    coordinator domains must call this once, after their last parallel job and
    before the domain exits. Ordinary sketches never call it. *)

val map_array : ?grain:int -> ('a -> 'b) -> 'a array -> 'b array
(** Parallel order-preserving array map. Output slots are disjoint and no
    per-element option boxes are allocated. [grain] is both the sequential
    cutoff and the stable minimum chunk size. Pool context is installed once
    per chunk, not once per element. *)

val init_array : ?grain:int -> int -> (int -> 'a) -> 'a array
(** Parallel deterministic [Array.init] with a sequential cutoff. *)

val for_ :
  ?chunk_size:int -> start:int -> finish:int -> (int -> unit) -> unit
(** Execute the inclusive integer range in stable contiguous chunks. Pool
    context and scheduler bookkeeping are paid once per chunk. *)

val both : (unit -> 'a) -> (unit -> 'b) -> 'a * 'b
(** Evaluate two sufficiently expensive computations in parallel. *)
