(** Safe coarse-grained parallel computation for sketches.

    Functions passed here must not call SDL, [Graphics], [Scene.render], or mutate shared
    unprotected state. *)

val recommended_domains : unit -> int

val run : ?domains:int -> (unit -> 'a) -> 'a
(** Run a computation with at most [domains] participants (the calling domain included) in every
    nested [Parallel] helper. One process-wide set of [recommended_domains () - 1] worker domains,
    started at first use and joined at exit, serves every calling domain. [domains:1] makes every
    nested helper sequential. *)

val map_array : ?grain:int -> ('a -> 'b) -> 'a array -> 'b array
(** Parallel order-preserving array map. Output slots are disjoint and no per-element option boxes
    are allocated. [grain] is both the sequential cutoff and the minimum chunk size; a longer
    array is cut into about 16 chunks per participant. A raised exception is the one the lowest
    failing index raises, as [Array.map] would. *)

val init_array : ?grain:int -> int -> (int -> 'a) -> 'a array
(** Parallel deterministic [Array.init] with a sequential cutoff. *)

val for_ : ?chunk_size:int -> start:int -> finish:int -> (int -> unit) -> unit
(** Execute the inclusive integer range in contiguous chunks of at least [chunk_size] (the
    sequential cutoff), about 16 per participant. Context and scheduling are paid once per chunk. *)

val both : (unit -> 'a) -> (unit -> 'b) -> 'a * 'b
(** Evaluate two sufficiently expensive computations in parallel. *)
