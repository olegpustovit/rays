(** Immutable deterministic random generators.

    Every sampling function returns the sampled value and the next generator. A generator can
    therefore be reproduced, stored in a model, or split for independent multicore work without
    shared mutation. *)

type t

val seed : int -> t
val seed64 : int64 -> t

(* Uniform sample in [0, 1). *)
val float : t -> float * t

val float_at : t -> index:int -> float
(** Stateless uniform sample greater than or equal to zero and less than one, keyed by [index]. This
    is the allocation-free deterministic path for disjoint parallel element ranges; it does not
    advance or mutate the base generator. *)

val float_at_int : int -> index:int -> float
(** Allocation-free stateless sample for a signed OCaml integer seed. *)

(* Uniform integer in [0, bound). *)
val int : bound:int -> t -> int * t
(* Uniform integer in the inclusive range. *)

val shuffle : 'a list -> t -> 'a list * t
