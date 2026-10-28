(** The graph panels' own state, by panel key, with the key of the one in use.

    Every graph panel keeps its canvas, level, selection, graph and route here; "the pane in use" is
    only the key the map points at, so switching the focus changes a key and copies nothing. The key
    [[]] is the pane of a document that shows no graph panel yet: the first panel adopts it. *)

type 'a t

val make : 'a -> 'a t
(** One pane, in use, with no panel yet. *)

val active : 'a t -> 'a
(** The pane in use. *)

val key : 'a t -> string list option
(** The panel the pane in use belongs to; none before the first graph panel. *)

val leaf : 'a t -> int list option
(** Where that panel's leaf was at the last frame. *)

val set_leaf : int list option -> 'a t -> 'a t

val update : ('a -> 'a) -> 'a t -> 'a t
(** The pane in use, changed. *)

val others : 'a t -> (string list * 'a) list
(** The panes not in use. *)

val activate : string list -> create:(unit -> 'a) -> 'a t -> 'a t
(** The panel's pane is the one in use; [create] makes it when the map has none. *)

val adopt : string list -> 'a t -> 'a t
(** The pane of no panel becomes this panel's. *)

val set : string list -> 'a -> 'a t -> 'a t
(** The panel's pane, added or replaced (in use or not). *)

val with_others : (string list * 'a) list -> 'a t -> 'a t
(** These are the panes not in use now. *)

val retain : (string list -> bool) -> 'a t -> 'a t
(** The panes of the panels that pass, and the one in use. *)
