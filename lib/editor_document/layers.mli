(** The World's layers as scene-graph kinds ([world/gradient], [world/sky], ...) and the stack
    they make: a {!Rays.World.t} read from, and written to, a network of layer nodes. *)

module Settings : sig
  val factory : Sop.Edit_graph.factory
  (** The World's own settings node, a scene kind. *)
end

val catalog : Sop.Edit_graph.factory list
(** One factory per layer kind. *)

val to_world : ?time:float -> Sop.Node.t -> Document.network -> Rays.World.t option

val of_world :
  Rays.World.t ->
  (Sop.Edit_graph.factory * string * (string * Param.value) list) list * (string * Param.value) list

val network_of_world : Rays.World.t -> (Document.network, string) result
