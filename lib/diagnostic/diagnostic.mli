(** Shared structured diagnostics for the language and geometry layers. *)
type span = { start : int; finish : int }
type severity = Error_severity | Warning
type position = { line : int; col : int }
type trace = { node_id : int; label : string; operation : string }
type t = {
  code : string;
  severity : severity;
  position : position option;
  message : string;
  span : span option;
  cause : string option;
  hints : string list;
  trace : trace list;
}

exception Error of t

val position_of_offset : string -> int -> position
val error : ?span:span -> ?position:position -> ?cause:string -> ?hints:string list -> ?trace:trace list -> code:string -> string -> t
val warning : ?span:span -> code:string -> string -> t
val prepend_trace : trace -> t -> t
val to_string : t -> string
val error_to_string : t -> string
val report : file:string -> source:string -> t -> string
