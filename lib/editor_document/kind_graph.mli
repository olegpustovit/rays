(** The scene's objects and the World's layers as plain data (cleanup decision 9): each is an
    instance of a kind, a typed {!Param} schema with its descriptor, holding its values and at
    most one input (an object's parent, a layer's layer below).  No SOP node, session or cook. *)

module Kind : sig
  type t = { key : string; label : string; category : string list; defaults : Settings.t }
  (** [key] is the kind's name without its context ([light] for [scene/light]). *)

  val make : key:string -> label:string -> category:string list -> 'record Param.schema -> t
  val fields : t -> Param.field_view list
  (** The schema's fields at their defaults. *)
end

type node = { id : int; kind : string; label : string; values : Settings.t; input : int option }
(** [kind] is the {!Kind.t.key}; [values] the typed record. *)

type t = node list
(** In lowering order. *)

val empty : t
val instance : ?id:int -> label:string -> Kind.t -> node
(** A node of the kind at its defaults, with no input; a fresh id unless [id] is given. *)

val fields : node -> Param.field_view list
val field : node -> string -> Param.value option
val find : t -> int -> node option
val ids : string -> t -> int list
(** The nodes of one kind. *)

val add : t -> node -> t
val replace : t -> node -> t
val update : t -> int -> (node -> node) -> t
val remove : int list -> t -> t
(** The nodes gone, and every input that named one of them. *)

val apply : node -> (string * Param.value) list -> (node * Param.effects, string) result
(** Field values checked by the kind's schema. *)

val set_fields : t -> int -> (string * Param.value) list -> (t * Param.effects, string) result

val stack : t -> node list
(** From the node no other takes as input down its inputs: a World's layers, top first. *)

val same : t -> t -> bool
(** The same ids, kinds, labels, inputs and values in the same order. *)
