open Ogpu_core

type control

val create : unit -> Backend.driver * control
val live_counts : control -> int * int * int * int * int
