(** Ownership-aware compiled bindings to SDL3: the window, input and presentation surface Rays's
    runtime uses. Every call runs on the initial domain and returns a typed error otherwise; no
    callback into OCaml is ever installed (see specification/sdl3.md). *)

type error_kind =
  | Sdl_error
  | Wrong_domain
  | Destroyed
  | Parent_has_dependents
  | Incompatible_version
  | Invalid_argument
  | Unsupported

type error = private { operation : string; kind : error_kind; message : string }
type rect = { x : int; y : int; width : int; height : int }

val pp_error : Format.formatter -> error -> unit

type version = { major : int; minor : int; patch : int }

val compiled_version : version
(** The version macro of the headers this binding was compiled against. *)

val linked_version : unit -> version

val validate_version :
  ?library:string ->
  ?compiled:version ->
  release:bool ->
  linked:version ->
  unit ->
  (unit, error) result

val check_version : ?release:bool -> unit -> (unit, error) result

type rgba = { width : int; height : int; pixels : bytes }
(** Decoded pixels: tightly packed RGBA8 rows owned by the caller. *)

module Thread : sig
  val is_initial_domain : unit -> bool
  val is_sdl_main_thread : unit -> bool
end

module Init : sig
  type subsystem = Video | Events

  val init : subsystem list -> (unit, error) result
  val quit_subsystems : subsystem list -> (unit, error) result
end

(** SDL's own hints, set by name in the stubs. *)
module Hint : sig
  val control_click_is_right_click : bool -> (unit, error) result
  (** macOS: let SDL turn a Control-click into a right click. *)
end

module Window : sig
  type t

  type flag =
    | Hidden
    | High_pixel_density
    | Metal
        (** What SDL's window flags say about showing. SDL sends an event when a window becomes
            occluded and none when it stops, so a caller that saw [Event.Occluded] reads [state] to
            learn it is uncovered again. *)

  type state = { hidden : bool; minimized : bool; occluded : bool }

  val create :
    title:string -> width:int -> height:int -> ?flags:flag list -> unit -> (t, error) result

  val size : t -> (int * int, error) result
  val size_in_pixels : t -> (int * int, error) result
  val pixel_density : t -> (float, error) result
  val position : t -> (int * int, error) result
  val title : t -> (string, error) result
  val center : t -> (unit, error) result
  val set_size : t -> width:int -> height:int -> (unit, error) result
  val set_resizable : t -> bool -> (unit, error) result
  val set_background : t -> red:float -> green:float -> blue:float -> (unit, error) result

  (* macOS: the NSWindow background (sRGB, 0..1). Every window is created
     with its title hidden and a transparent title bar, so this is the colour
     behind the traffic lights. Nothing happens on another video driver. *)
  val set_relative_mouse : t -> bool -> (unit, error) result
  val show : t -> (unit, error) result
  val raise_window : t -> (unit, error) result
  val hide : t -> (unit, error) result
  val restore : t -> (unit, error) result
  val state : t -> (state, error) result
  val sync : t -> (unit, error) result
  val destroy : t -> (unit, error) result
end

module Cursor : sig
  type shape = Default | Text | Ew_resize | Ns_resize
  type t

  val create : shape -> (t, error) result
  val set : t -> (unit, error) result
  val destroy : t -> (unit, error) result
end

module Clipboard : sig
  val set_text : string -> (unit, error) result
  val get_text : unit -> (string, error) result
end

module Text_input : sig
  val start : Window.t -> (unit, error) result
  val stop : Window.t -> (unit, error) result
  val set_area : Window.t -> rect -> cursor:int -> (unit, error) result
end

module Key : sig
  (** A key as the stubs name it with SDL's key macros. A letter or digit is [Char] in lower case on
      any layout (by the key's position when the layout's own character is not ASCII); [Unknown]
      carries SDL's keycode. *)
  type t =
    | Char of char
    | Arrow_up
    | Arrow_down
    | Arrow_left
    | Arrow_right
    | Space
    | Enter
    | Escape
    | Backspace
    | Tab
    | Shift
    | Control
    | Alt
    | Meta
    | F1
    | F2
    | F3
    | F4
    | F5
    | F6
    | F7
    | F8
    | F9
    | F10
    | F11
    | F12
    | Home
    | End
    | Page_up
    | Page_down
    | Insert
    | Delete
    | Unknown of int

  type modifier =
    | Shift_held
    | Control_held
    | Alt_held
    | Meta_held
    | Num_lock
    | Caps_lock
    | Scroll_lock
end

(** Native file dialogs. A dialog opens and returns at once with its id; the outcome arrives later
    as an [Event.Dialog] with the same id, through the event poll on the initial domain. SDL may run
    its callback on another thread; the stubs only copy the outcome into a fixed number of native
    slots there (no OCaml runs), so at most 8 dialogs can be open at once. *)
module Dialog : sig
  type kind = Open_file | Open_files | Save_file | Open_folder

  type filter = { name : string; pattern : string }
  (** [pattern] lists extensions separated by semicolons ("png;jpg"), or "*". *)

  val show :
    Window.t -> ?filters:filter list -> ?default_location:string -> kind -> (int, error) result
end

module Event : sig
  type mouse_button = Left | Middle | Right | X1 | X2
  type wheel_direction = Normal | Flipped
  type pinch_phase = Began | Updated | Ended
  type scroll_phase = Scroll_began | Scroll_changed | Scroll_ended | Scroll_momentum

  type window_change =
    | Shown
    | Hidden
    | Minimized
    | Restored
    | Occluded
    | Focus_gained
    | Focus_lost
    | Close_requested
    | Resized of int * int  (** logical points *)
    | Pixel_size_changed of int * int  (** drawable pixels *)

  type drop_change =
    | Drop_begin
    | Drop_position  (** a drag is over the window at [x], [y] *)
    | Drop_complete
    | File of string

  type dialog_outcome =
    | Chosen of string list  (** the chosen paths, UTF-8 *)
    | Cancelled
    | Failed of string

  (** The events Rays reads; every other kind SDL queues is skipped in the stubs and never copied.
  *)
  type t =
    | Quit
    | Window of window_change
    | Key of { key : Key.t; modifiers : Key.modifier list; down : bool; repeat : bool }
    | Text_input of string
    | Text_editing of { text : string; start : int; length : int }
    | Mouse_motion of { x : float; y : float; dx : float; dy : float }
    | Mouse_button of { button : mouse_button; down : bool; x : float; y : float }
    | Mouse_wheel of {
        x : float;
        y : float;
        direction : wheel_direction;
        mouse_x : float;
        mouse_y : float;
        integer_x : int;
        integer_y : int;
      }
    | Pinch of { phase : pinch_phase; scale : float }
    | Drop of { change : drop_change; x : float; y : float }
    | Dialog of { id : int; outcome : dialog_outcome }
        (** a file dialog [Dialog.show] opened has finished *)
    | Scroll of { x : float; y : float; phase : scroll_phase; seconds : float }
        (** macOS: a trackpad scroll as AppKit phases it, which [Mouse_wheel] cannot say. [x], [y]
            are points in [Mouse_wheel]'s directions and [seconds] the event's own clock.
            [Scroll_began] is the fingers touching, [Scroll_changed] their motion, [Scroll_ended]
            the lift and [Scroll_momentum] the system's inertia after it. Every one that moves also
            arrives as a [Mouse_wheel] of a tenth of it, in the same poll. *)

  val poll_coalesced : unit -> (t list, error) result
  (** Drain the native queue without building a motion/resize flood. Pointer motion and each kind of
      window size event keep only the latest sample (motion sums the dropped samples' relative
      [dx]/[dy]); clicks, keys, text, focus, and quit stay ordered. *)

  val wait : timeout_ms:int -> (bool, error) result
  (** Block until an event is queued or [timeout_ms] pass, leaving it for the next poll; [true] when
      one is. An idle window waits here instead of drawing the same frame. *)
end

module Metal_view : sig
  type t
  type layer = Native_layer_token.t

  val create : Window.t -> (t, error) result
  val layer : t -> (layer, error) result
  val destroy : t -> (unit, error) result
end
