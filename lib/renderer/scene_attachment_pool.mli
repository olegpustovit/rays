type kind = Color | Depth
type t

val create :
  device:Ogpu.Backend.device ->
  configuration:Ogpu.Surface.configuration ->
  sample_counts:int list ->
  t

val acquire :
  ?passing:bool -> t -> kind -> samples:int -> (Ogpu.Backend.texture, Ogpu.Error.t) result
(** [acquire pool kind ~samples] allocates the exact attachment on its first use and returns the
    same live texture on later uses. [passing] asks for one whose contents no other pass reads:
    memoryless where the device has them, an ordinary attachment otherwise. *)

val acquire_many :
  t -> (kind * int * bool) list -> (Ogpu.Backend.texture list, Ogpu.Error.t) result
(** Each of (kind, samples, passing). *)

val destroy : t -> unit
val allocated : t -> (kind * int * bool * int64) list
