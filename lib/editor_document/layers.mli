(** The World's layers as plain kinds ([world/gradient], [world/sky], ...) and the stack they
    make: a {!Rays.World.t} read from, and written to, a {!Kind_graph.t} of layers. *)

module Settings : sig
  val kind : Kind_graph.Kind.t
  (** The World's own settings, a scene kind ([scene/world]). *)
end

val kinds : Kind_graph.Kind.t list
(** One kind per layer. *)

val to_world : ?time:float -> Kind_graph.node -> Kind_graph.t -> Rays.World.t option
(** The World a world object and its layers describe; [time] (seconds) advances its time of day
    by its day cycle. *)

val of_world :
  Rays.World.t ->
  (Kind_graph.Kind.t * string * (string * Param.value) list) list * (string * Param.value) list
(** The layers (bottom first: kind, name, values) and the world object's values of a World. *)

val graph_of_world : Rays.World.t -> (Kind_graph.t, string) result
(** The layers of a World, each on the one below it. *)
