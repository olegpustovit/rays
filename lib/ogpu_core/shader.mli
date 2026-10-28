type stage = Vertex | Fragment | Compute
type binding_kind = Uniform_buffer | Storage_buffer | Sampled_texture | Storage_texture | Sampler
  | Acceleration_structure
type entry_point = { name : string; stage : stage }
type binding =
  { group : int; binding : int; kind : binding_kind; visibility : stage list }
type constant_value = Bool of bool | Int32 of int32 | Uint32 of int32 | Float32 of float
type descriptor =
  { backend : string
  ; label : string option
  ; bytes : bytes
  ; entry_points : entry_point list
  ; bindings : binding list
  }
type t

val create : descriptor -> (t, Error.t) result
val of_source : descriptor -> (t, Error.t) result
val backend : t -> string
val label : t -> string option
val bytes : t -> bytes
val entry_points : t -> entry_point list
val bindings : t -> binding list

(** Checks one entry point's binding interface or constant list on its own,
    for pipelines created from a shared library. *)
val validate_bindings : binding list -> (unit, Error.t) result
val validate_constants : (string * constant_value) list -> (unit, Error.t) result
