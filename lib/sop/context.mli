(** Target-neutral facts available while cooking a procedural graph. *)

module Cancel = Rdk.Cancel

module Dependencies : sig
  type fact = Frame | Time | Seed | Domains | Grain | Input | Pointer | Keys | Size | Step
  (** [Input] is the whole host frame (events, [frame/input]); [Pointer], [Keys], [Size] and
      [Step] (dt, tick, frame index) are the slices a graph can read alone. *)
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

val defer : t -> node:int -> (unit -> (unit, Diagnostic.t) result) -> unit
(** Node [node] left device work in flight (from any domain): [settle] waits for it and checks its
    status. {!Session} settles before it cooks any node and at a cook's end, so no CPU reader sees
    an unsettled output. *)

val settle : ?all:bool -> t -> int list
(** Runs the deferred checks this domain registered ([all]: every one), oldest first, and returns
    the nodes whose device work failed; those are {!device_refused} from then on, and the session
    recooks them. A parallel branch settles only its own work, so it never waits on a request
    another branch's domain would have to serve. *)

val device_refused : t -> int -> bool
(** The node's device work failed in this context: it cooks on the CPU. *)

val refusals : t -> int
(** How many device failures this context has settled. *)

val input_part : Dependencies.fact -> Input.t -> string
(** The slice of the host input a fact reads ("" for a fact that is not an input slice); [Time] is
    the clock. Exact bits. *)

val input_changed : Dependencies.t -> Input.t -> Input.t -> bool
(** Whether any input slice the dependencies read differs between two frames. *)

val cache_projection : Dependencies.t -> t -> string
(** Exact cache-key projection for the declared facts. Static dependencies always return the same
    projection, independent of the context. *)
