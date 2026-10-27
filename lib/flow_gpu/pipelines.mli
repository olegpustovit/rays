type t
type compiled = {pipeline : Ogpu.Backend.pipeline}
val create : clock:(unit -> float) -> Ogpu.Backend.device -> t
val get : t -> Emit.msl -> (compiled, Diagnostic.t) result
val close : t -> unit
module Private : sig
  val count : t -> int
  val compilations : t -> int
  val releases : t -> int
end
