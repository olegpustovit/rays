(** The zone cook step: a loop over the elements of cooked geometry.

    A zone node has the collection as input 0 (and any outer geometry its body reads as further
    inputs). When the session cooks it, {!Node.Build.expand} derives the elements of the collection,
    asks [body] for the sub-graph of each one, cooks those through the same session (so an element
    whose sub-graph is unchanged is a cache hit) and merges the results, writing
    [source_base + element index] into [source_attribute]. *)

type kind =
  | Points
  | Pieces
      (** A point is its position; a piece is one primitive, or the primitives that share a value of
          the key attribute. *)

type element = {
  index : int;  (** position in the ordered list *)
  key : int;  (** the [key] attribute's value, or the source index *)
  position : float * float * float;  (** a point's position; a piece's first point *)
  piece : Rdk.Geometry.t option;  (** the geometry of a piece *)
  digest : string;  (** identity of a piece's content ([""] for a point) *)
}

val max_elements : int
(** 4,096, as for every loop. *)

val elements : kind -> ?key:string -> Rdk.Geometry.t -> (element array, Diagnostic.t) result
(** Ordered by [key] (an int or float attribute of the points, or of the primitives for pieces) when
    the geometry carries it, else by index (ties keep index order). *)

val element_node : element -> Node.t
(** The geometry of a piece as a node, keyed by its content digest so the nodes fed by it hit the
    session cache while it is unchanged. *)

val node :
  ?report:(element array -> unit) ->
  ?live:bool ->
  ?reads:Context.Dependencies.t ->
  ?stamp:string ->
  kind:kind ->
  ?key:string ->
  ?select:int ->
  source_attribute:string ->
  source_base:int ->
  body:(inputs:Node.t array -> context:Context.t -> element -> Node.t) ->
  inputs:Node.t array ->
  unit ->
  Node.t
(** [report] receives the elements of each cook that runs (not of a cache hit). A [live] zone (its
    body reads [t]) depends on the context's time, so its key changes with it and [body] gets the
    frame context of the cook. [stamp] identifies an immutable frame-fold snapshot captured by
    [body], invalidating cached cooks when the snapshot changes. [body] gets the node's current
    inputs (a rebuilt node keeps its closures, so it must not capture the [inputs] it was made with)
    and must be pure. [select] expands only that element for a viewport preview, keeping its
    original source tag. [report] still receives every element. An absent index yields empty
    geometry. *)
