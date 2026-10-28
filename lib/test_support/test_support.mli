val fail : ('a, unit, string, 'b) format4 -> 'a
val check : bool -> string -> unit
val ok : ('a, Diagnostic.t) result -> 'a
