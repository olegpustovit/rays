(** Optional, domain-local edit instrumentation. The host supplies its clock;
    Flow acquires no platform dependency. Durations exclude nested phases. *)
type entry = { phase : string; calls : int; seconds : float }
type t = { total : float; entries : entry list }
val empty : t
val sample : clock:(unit -> float) -> (unit -> 'a) -> 'a * t
(** Nested samples contribute to the enclosing sample and return its cumulative
    entries. The outermost sample supplies the clock and restores instrumentation
    on exceptions. Backward clock readings are clamped. Phase names belong to
    callers; entries are sorted by name. *)

val measure : string -> (unit -> 'a) -> 'a

val without : (unit -> 'a) -> 'a
(** Internal compilation within a measured phase contributes to that phase's
    duration without adding workspace invocation counts. Restored on exceptions. *)

val calls : t -> string -> int
val seconds : t -> string -> float
val summary : t -> string
