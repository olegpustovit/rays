type kind = Color | Depth
type t

val create :
  device:Ogpu.Backend.device ->
  configuration:Ogpu.Surface.configuration ->
  sample_counts:int list ->
  t

(** [acquire pool kind ~samples] allocates the exact attachment on its first
    use and returns the same live texture on later uses. *)
val acquire : t -> kind -> samples:int -> (Ogpu.Backend.texture, Ogpu.Error.t) result
val acquire_many : t -> (kind * int) list -> (Ogpu.Backend.texture list, Ogpu.Error.t) result

val destroy : t -> unit
val allocated : t -> (kind * int * int64) list
