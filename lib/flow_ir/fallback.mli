(** Every time work meant for the device runs on the CPU instead, here, with why: the one place to
    ask. Bounded to 64 distinct [site, reason] pairs; a measured choice of the CPU is not a fallback. *)

val note : site:string -> string -> unit
val seen : unit -> (string * string * int) list
(** Each [site, reason] and how many times, sorted. *)

val reset : unit -> unit
val summary : unit -> string option
(** The first entry and how many more, for one status line. *)
