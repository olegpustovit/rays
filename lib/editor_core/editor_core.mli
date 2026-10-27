(** Pure editor state shared by the sketch host and presentation adapters. *)

module Store = Store
module Panels = Panels

(** Bounded immutable undo history with explicit edit merge rules. *)
module History : sig
  type 'a t
  type merge =
    | Step
    | Gesture of string
    | Burst of { key : string; at : float; window : float }
    | Repair
  val create : ?capacity:int -> 'a -> 'a t
  val present : 'a t -> 'a
  (* [Step] adds an undo entry; matching [Gesture] operation/target keys merge until [seal];
      matching [Burst] keys merge while edits stay within [window] seconds;
      [Repair] changes the current entry without adding a step. *)
  val record : ?merge:merge -> ?label:string -> 'a -> 'a t -> 'a t
  (** [label] (default ["Edit"]) names the entry, e.g. ["Connect"]. *)

  val label : 'a t -> string
  (** The label of the edit that produced [present]: what [undo] reverts. *)

  val redo_label : 'a t -> string option
  val seal : ?gesture_only:bool -> 'a t -> 'a t
  (** [gesture_only] preserves timed bursts when a pointer gesture ends. *)

  val undo : 'a t -> 'a t option
  val redo : 'a t -> 'a t option
  val can_undo : 'a t -> bool
  val can_redo : 'a t -> bool
  val depth : 'a t -> int
end

module Keymap : sig
  type trigger = Leader of string | Chord of Input.key * Input.key list
  val label : trigger -> string
  (** Shared key spelling for guides, which-key and command feedback. *)
end

module Guide_context : sig
  type t = Canvas | Node | Multi | Hints | Leader | Search | List | Text
  val name : t -> string
end

(** Named editor commands: the one table behind key routing, which-key, and
    the command palette. Pure data; the host decides what [action] does. *)
module Command : sig
  type ('scope, 'action) t = {
    id : string;  (** stable, e.g. ["edit.undo"]; entries sharing an id are one command *)
    label : string;  (** shown in which-key and the palette *)
    trigger : Keymap.trigger option;  (** [None]: palette only *)
    scope : 'scope option;  (** [None]: global; else only while that pane has focus *)
    guide : Guide_context.t list;  (** contexts where the guide strip lists this command *)
    action : 'action;
  }

  val make : ?trigger:Keymap.trigger -> ?scope:'scope -> ?guide:Guide_context.t list ->
    id:string -> label:string ->
    'action -> ('scope, 'action) t
  val for_guide : ('scope, 'action) t list -> focus:'scope -> context:Guide_context.t ->
    ('scope, 'action) t list
  (** Applicable commands in their original table order. *)
end

module Router : sig
  (** Tab cancels leader routing, runs its exact-modifier host binding or
      passes to UI traversal, then hands the remaining ordered events to
      the UI (including a same-frame /). *)
  type state = Idle | Pending of string  (** leader keys typed so far *)

  (** In fly mode, keep pointer/window events and pass / to the leader (Space exits too, but is not passed on)
      router after ending the mode. Escape ends fly without opening a shortcut. *)
  val fly : Rays.Frame.t -> bool * Rays.Frame.t

  val step : ?previous_keys:Input.key list ->
    ('scope, 'action) Command.t list -> focus:'scope ->
    text_focus:bool -> frame:Rays.Frame.t -> state ->
    state * ('scope, 'action) Command.t list * Rays.Frame.t
  (** Returns the exact matched entries in event order, retaining each alias's
      trigger, scope and label for feedback. Commands without a trigger never match a key. Modifiers follow event
      order. Supply the previous frame's keys for changed modifiers and focus
      cancellation; omitted previous state assumes no modifiers were held. *)
end

(** The one model of a number field (a card's row, the inspector's control, a vector's cell): how
    a drag changes its text.  The kind is the parameter's, never the literal's spelling, so a float
    written [1] scrubs to [1.5]. *)
module Number : sig
  type kind = Int | Float

  val valid : kind -> string -> bool
  (** Text a field of this kind commits: an integer, or a finite float. *)

  val fraction : float * float -> string -> float option
  (** Where the value sits in a soft range, 0 to 1, for the field's position line. *)

  val show : float -> string
  (** A float as a field displays it: ten significant digits, never an exponent.  What is written
      is {!Flow.Lisp.float}. *)

  val scrub : kind -> ?range:float * float -> string -> float -> bool -> string
  (** [scrub kind ?range origin dx fine]: the text after a drag of [dx] points from the text it
      began at.  An integer moves by one every 6 points; a float by 0.05 a point, or by a
      two-hundredth of its soft range, a tenth of that when [fine] (Shift).  The result is rounded
      to the step's digits (the origin's when it wrote more) and printed by {!Flow.Lisp.float}, so
      [1234567.89] stays exact and no exponent appears.  With a [range] the drag stops at its ends
      (typing goes past them; a value already outside is not pulled in).  A drag shorter than one
      step returns [origin] itself: a click writes nothing. *)
end
