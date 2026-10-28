(** Parinfer's indent mode over workspace Lisp text. *)

val rewrite : string -> int -> string * int
(** [rewrite text caret] infers each line's closing brackets from the indentation of the code
    lines that follow, drops closers that match nothing, and returns the text with where [caret]
    lands in it. Strings and comments are left alone; on the caret's line the trail starts no
    earlier than the caret. *)
