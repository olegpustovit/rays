(** Pure editor state shared by the sketch host and presentation adapters. *)

module Store = Store
module Panels = Panels

(** Bounded immutable undo history with explicit edit merge rules. *)
module History : sig
  type ('a, 'e) t

  type merge =
    | Step
    | Gesture of string
    | Burst of { key : string; at : float; window : float }
    | Repair

  val create :
    ?capacity:int ->
    ?byte_capacity:int ->
    ?bytes:('e -> int) ->
    keep:('a -> 'e) ->
    'a ->
    ('a, 'e) t
  (** The present is a live ['a]; an entry is only what [keep] takes from one, which restores it when
      undo or redo steps there. [bytes] is an entry's size; [byte_capacity] bounds their sum over the
      entries, oldest going first, the newest always staying, as {!byte_depth} says. *)

  val present : ('a, 'e) t -> 'a

  (* [Step] adds an undo entry; matching [Gesture] operation/target keys merge until [seal];
      matching [Burst] keys merge while edits stay within [window] seconds;
      [Repair] changes the current entry without adding a step. *)
  val record : ?merge:merge -> ?label:string -> 'a -> ('a, 'e) t -> ('a, 'e) t
  (** [label] (default ["Edit"]) names the entry, e.g. ["Connect"]. *)

  val label : ('a, 'e) t -> string
  (** The label of the edit that produced [present]: what [undo] reverts. *)

  val redo_label : ('a, 'e) t -> string option

  val seal : ?gesture_only:bool -> ('a, 'e) t -> ('a, 'e) t
  (** [gesture_only] preserves timed bursts when a pointer gesture ends. *)

  val undo : restore:('e -> ('a, 'x) result) -> ('a, 'e) t -> (('a, 'e) t, 'x) result option
  (** [None] with nothing to undo. [restore] makes the entry live again, unless it is the last
      present to become an entry, which stays live beside the entries. [Error]: nothing changed. *)

  val redo : restore:('e -> ('a, 'x) result) -> ('a, 'e) t -> (('a, 'e) t, 'x) result option
  val can_undo : ('a, 'e) t -> bool
  val can_redo : ('a, 'e) t -> bool
  val depth : ('a, 'e) t -> int

  val byte_depth : ('a, 'e) t -> int
  (** What the byte cap counts: the entries to undo and to redo. *)

  val retained : ('a, 'e) t -> 'e list * 'a option
  (** The entries, and the one live value kept beside them: what the ledger walks, since entries
      share what an edit did not touch and a sum of their sizes counts it again. *)

  val shed : keep:int -> ('a, 'e) t -> ('a, 'e) t
  (** The oldest undo entries gone, down to [keep] (at least one). *)

  val release : ('a, 'e) t -> ('a, 'e) t
  (** No entries left, and none charged: the editor is closing. *)

  val restart : 'a -> ('a, 'e) t -> ('a, 'e) t
  (** A new document: [release], with this present. *)
end

module Keymap : sig
  type trigger = Leader of string | Chord of Input.key * Input.key list

  val label : Input.platform -> trigger -> string
  (** Shared key spelling for guides, which-key and command feedback. *)
end

module Guide_context : sig
  type t = Canvas | Node | Multi | Hints | Leader | Search | List | Text

  val name : t -> string
end

(** Named editor commands: the one table behind key routing, which-key, and the command palette.
    Pure data; the host decides what [action] does. *)
module Command : sig
  type ('scope, 'action) t = {
    id : string;  (** stable, e.g. ["edit.undo"]; entries sharing an id are one command *)
    label : string;  (** shown in which-key and the palette *)
    trigger : Keymap.trigger option;  (** [None]: palette only *)
    scope : 'scope option;  (** [None]: global; else only while that pane has focus *)
    guide : Guide_context.t list;  (** contexts where the guide strip lists this command *)
    action : 'action;
  }

  val make :
    ?trigger:Keymap.trigger ->
    ?scope:'scope ->
    ?guide:Guide_context.t list ->
    id:string ->
    label:string ->
    'action ->
    ('scope, 'action) t

  val for_guide :
    ('scope, 'action) t list -> focus:'scope -> context:Guide_context.t -> ('scope, 'action) t list
  (** Applicable commands in their original table order. *)
end

module Router : sig
  (** Tab cancels leader routing, runs its exact-modifier host binding or passes to UI traversal,
      then hands the remaining ordered events to the UI (including a same-frame /). *)
  type state = Idle | Pending of string  (** leader keys typed so far *)

  val fly : Rays.Frame.t -> bool * Rays.Frame.t
  (** In fly mode, keep pointer/window events and pass / to the leader (Space exits too, but is not
      passed on) router after ending the mode. Escape ends fly without opening a shortcut. *)

  val step :
    ?previous_keys:Input.key list ->
    platform:Input.platform ->
    ('scope, 'action) Command.t list ->
    focus:'scope ->
    text_focus:bool ->
    frame:Rays.Frame.t ->
    state ->
    state * ('scope, 'action) Command.t list * Rays.Frame.t
  (** Returns the exact matched entries in event order, retaining each alias's trigger, scope and
      label for feedback. Commands without a trigger never match a key. Modifiers follow event
      order. Supply the previous frame's keys for changed modifiers and focus cancellation; omitted
      previous state assumes no modifiers were held. *)
end
