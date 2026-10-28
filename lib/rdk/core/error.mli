(** Structured failures at public RDK operation and mesh boundaries. *)

type t

val make :
  ?hints:string list -> operation:string -> code:string -> string -> t
val of_string : operation:string -> code:string -> string -> t
val guard : operation:string -> code:string -> (unit -> ('a, string) result) ->
  ('a, t) result
exception Fail of string
(** A failure message raised inside {!guard}, which wraps it as the outer
    operation's error. *)

val fail : string -> 'a
val get : ('a, t) result -> 'a
(** Unwrap a typed operation inside a {!guard} body: re-raises cancellation,
    raises {!Fail} with the message. *)

val operation : t -> string
val code : t -> string
val message : t -> string
val hints : t -> string list
val to_string : t -> string
