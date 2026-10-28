(** Structured failures at public RDK operation and mesh boundaries. *)

type t

val make : ?hints:string list -> operation:string -> code:string -> string -> t

val guard :
  ?cancelled:string ->
  ?invalid_argument:string ->
  operation:string ->
  code:string ->
  (unit -> ('a, string) result) ->
  ('a, t) result

exception Fail of string
(** A failure message raised inside {!guard}, which wraps it as the outer operation's error. *)

exception Rdk of t
(** A complete failure (operation, code, message) raised anywhere under {!guard} or {!run}, which
    return it unchanged. *)

val fail_with : operation:string -> code:string -> string -> 'a

val run : ?cancelled:string -> operation:string -> code:string -> (unit -> 'a) -> ('a, t) result
(** Straight-line {!guard}: [Invalid_argument] and {!Fail} become [code]. *)

val fail : string -> 'a

val get : ('a, t) result -> 'a
(** Unwrap a typed operation inside a {!guard} body: re-raises cancellation, raises {!Fail} with the
    message. *)

val operation : t -> string
val code : t -> string
val message : t -> string
val hints : t -> string list
val to_string : t -> string
