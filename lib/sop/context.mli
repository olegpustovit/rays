(** Target-neutral facts available while cooking a procedural graph. *)

module Cancel = Rdk.Cancel

module Dependencies : sig
  type fact = Frame | Time | Seed | Domains | Grain | Input
  type t

  val static : t
  val one : fact -> t
  val union : t -> t -> t
  val mem : fact -> t -> bool
  val to_list : t -> fact list
  val to_string : t -> string
end

type t

val create :
  ?frame:int64 ->
  ?time:float ->
  ?input:Input.t ->
  ?seed:int64 ->
  ?domains:int ->
  ?grain:int ->
  ?cancel:Cancel.t ->
  unit ->
  (t, string) result

val frame : t -> int64
val time : t -> float

val input : t -> Input.t
(** Full logical frame. Its time and index follow this context's clock. *)

val seed : t -> int64
val domains : t -> int
val grain : t -> int
val cancel_token : t -> Cancel.t
val cancelled : t -> bool

val cache_projection : Dependencies.t -> t -> string
(** Exact cache-key projection for the declared facts. Static dependencies always return the same
    projection, independent of the context. *)
