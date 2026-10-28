(** The pure core of the Lisp text editor: tokens with matched brackets, Lisp indentation and
    the reading of the text around a byte.  Colours, completion and the UI stay in the editor. *)

type kind = Comment | Str | Num | Kw | Meta | Head | Sym | Open of int | Close of int | Unmatched
type token = { kind : kind; start : int; stop : int }

val lex : string -> token array * (int * int) list * (char * int) list
(** The tokens in order, the matched (open, close) token-start pairs, and the brackets still open
    at the end of the text (innermost first, with their start). *)

val indent : string -> int -> string
(** [indent text caret]: the spaces a new line at [caret] starts with. *)

val word : string -> token -> string
val index_at : token array -> int -> int
val items : token array -> int -> int list
val item_stop : token array -> int -> int
val head_before : string -> token array -> int -> string option

val binders : string list

val bound : string -> token array -> int -> (string * string) list * string list * string list
(** The names bound before a byte (with what they are), the defn names, the graph names. *)

val kw_value : string -> token array -> int -> string -> string option
val context_of : string -> token array -> (char * int) list -> string
