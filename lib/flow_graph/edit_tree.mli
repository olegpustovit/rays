(** The syntax-tree helpers a layout edit needs, public so the editor's own edits
    ([Editor_document.Layout_edit]) rewrite a workspace the way {!Flow_edit} does without the
    layout knowledge living here.  Only what those edits use: scopes ([let*] bindings and a
    result), call arguments, fresh names, one source rewrite that is checked. *)

module S = Flow.Syntax

type arg_key = Edit_base.arg_key =
  | Whole | Pos of int | Kw of string | Field of string | Bv of int * int | Arm of int

(** {1 Failing} *)

val fail : ?code:string -> ('a, unit, string, 'b) format4 -> 'a
(** Refuse the edit with an [E_EDIT] diagnostic and this message. *)

(** {1 Building and reading forms} *)

val mk : ?notes:string list -> ?meta:string list -> S.node -> S.t
val sym : string -> S.t
val kwf : string -> S.t
val num : int -> S.t
val call : string -> S.t list -> S.t
val head_sym : S.t -> string option
val map_children : (S.t -> S.t) -> S.t -> S.t
val keep_notes : S.t -> S.t -> S.t
(** [keep_notes old v]: [v], with the notes of [old] when it has none. *)

val sym_list : S.t -> string list
(** The names a form reads, first use first. *)

val dedup : string list -> string list
val count_refs : string -> S.t -> int
val rename_ref : string -> string -> S.t -> S.t
val rename_place : string -> string -> S.t -> S.t
(** {!rename_ref} that leaves an [:of old] following the panel itself. *)

val pat_key : S.t -> string
(** The name a binding pattern writes. *)

val has_prefix : prefix:'a list -> 'a list -> bool

(** {1 Call arguments} *)

val positional : S.t list -> S.t list
val kw_get : S.t list -> string -> S.t option
val arg_get : S.t -> arg_key -> S.t option
val arg_set : S.t -> arg_key -> S.t option -> S.t

(** {1 Scopes and paths} *)

type scope = Edit_base.scope = { form : S.t; vec : S.t; ps : (S.t * S.t) list; res : S.t }
(** A [let*] (or a bare expression): its bindings in order and its result. *)

val scope_of : S.t -> scope option
val ensure : S.t -> scope
(** The scope of an expression, a bare one wrapped in an empty [let*]. *)

val rebuild : scope -> (S.t * S.t) list -> S.t -> S.t
val collapse : scope -> (S.t * S.t) list -> S.t -> S.t
val reorder : S.t -> S.t
(** Bindings in dependency order. *)

val insert_at : 'a list -> int -> 'a list -> 'a list
val find_pair : scope -> string -> int option
val first_pair_index : scope -> string -> int

val edit_scope : S.t list -> string list -> (S.t -> S.t) -> S.t list
(** [edit_scope src scope_path f]: the source with the scope at this path rewritten by [f]. *)

val split_node : string list -> string list * string
(** A node path as its scope path and its leaf. *)

val get_node : S.t -> string -> S.t
val set_node : S.t -> string -> S.t -> S.t

val workspace_parts : S.t list -> S.t * S.t list
val root_name : S.t -> string option
val map_items : S.t list -> (S.t list -> S.t list) -> S.t list
val last_child : S.t -> S.t

(** {1 Fresh names} *)

val fresh : string list ref -> string -> string
val root_used : S.t list -> string -> string list ref

(** {1 Applying} *)

val apply_rewrite :
  ?ops:Flow.Op.t list -> ?library:bool ->
  ?checker:(Flow.Check.catalog -> S.t list -> Flow.Workspace.t option * Diagnostic.t list) ->
  Flow.Check.catalog -> (unit -> S.t list) ->
  (S.t list * Flow.Workspace.t, Diagnostic.t) result
(** The rewrite, printed and parsed again, then checked as {!Flow_edit.apply_checked} does:
    whatever it raises is a refused edit, never a crash. *)
