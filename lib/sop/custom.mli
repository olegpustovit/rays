(** Parameterized custom SOP authoring.

    This is the OCaml wrangle-like extension boundary: user code receives
    immutable exposed parameters, a declared cook context, and immutable RDK
    inputs. The builder automatically creates stable cache identity, attaches
    node-owned inspector metadata, and rebuilds the cook closure after edits.
    It is deliberately typed OCaml/RDK code rather than a VEX interpreter. *)

val node :
  ?label:string ->
  operation:string ->
  schema:'parameters Param.schema ->
  values:'parameters ->
  Node.t list ->
  (label:string -> inputs:Node.t list -> parameters:'parameters -> Node.t) ->
  Node.t
(** Attach node-owned parameters to any existing SOP composition without
    writing the recursive [Node.parameterize] reconstruction boilerplate. The
    callback must build the unparameterized local operator from its supplied
    label, current graph inputs, and immutable parameter record. *)

val plain :
  ?label:string ->
  ?version:int ->
  ?parameters:string ->
  ?cook_mode:Node.cook_mode ->
  ?dependencies:Context.Dependencies.t ->
  operation:string ->
  Node.t list ->
  (context:Context.t -> Rdk.Geometry.t array ->
   (Rdk.Geometry.t, string) result) ->
  Node.t
(** A node without inspector parameters: [parameters] is its cache key text.
    The cook sees a copy of the input array and a cancelled context yields a
    ["cancelled"] diagnostic before it runs. *)
