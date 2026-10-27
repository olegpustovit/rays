(** Owned bulk functions supplied by a language host. Packed columns are borrowed
    immutable inputs; the result owns its storage. *)
type column = Floats of float array | Vec3s of float array
type runner = Context.t -> (column, Diagnostic.t) result
type t
val create : ?geometry_capture:(int -> Rdk.Geometry.t option) ->
  payload_bytes:int -> (column list -> (runner, Diagnostic.t) result) -> t
val data_id : t -> int
val payload_bytes : t -> int
val prepare : t -> column list -> (runner, Diagnostic.t) result
module Private : sig
  val geometry_capture : t -> int -> Rdk.Geometry.t option
  (** Borrow an immutable geometry retained by the host's prepared function.
      IDs belong to that function's original capture scope. *)
  val remap_geometry_captures : sources:(int * int) list -> t -> t
  (** Rebind capture lookup to another plan's IDs. Prepared execution and owned
      payload identity remain the original function's. *)
end
