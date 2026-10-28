(** Renderer-independent input vocabulary and immutable logical frame facts. *)
type key =
  | KeyChar of char
  | ArrowUp
  | ArrowDown
  | ArrowLeft
  | ArrowRight
  | Space
  | Enter
  | Escape
  | Backspace
  | Tab
  | Shift
  | Ctrl
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
  | PageUp
  | PageDown
  | Insert
  | Delete
  | Unknown of int

type mouse_button = LeftButton | RightButton | MiddleButton | MouseX1 | MouseX2
type scroll_phase = Touched | Moved | Lifted | Momentum

type event =
  | Key_pressed of key
  | Key_released of key
  | Pointer_moved of (float * float)
  | Pointer_pressed of mouse_button * (float * float)
  | Pointer_released of mouse_button * (float * float)
  | Pointer_cancelled of mouse_button
  | Scrolled of (float * float)
  | Text_input of string
  | Text_editing of { text : string; start : int; length : int }
  | File_dropped of string
  | File_drag_moved of (float * float)
  | File_drag_ended
  | Pinched of float
  | File_dialog of { id : int; result : (string list, string) result }
  | Dialog_opened of int
      (** A workspace host started a native dialog; its actual returned identity. *)
  | Resized of (int * int)
  | Focus_lost
  | Focus_gained
  | Closed
  | Pixel_size_changed of (int * int)
  | Visibility_changed of bool
  | Trackpad_scrolled of { delta : float * float; phase : scroll_phase; time : float }

type t = {
  t : float;
  dt : float;
  frame : int;
  tick : int;
  size : int * int;
  pointer : float * float;
  buttons : mouse_button list;
  keys : key list;
  events : event list;
}
(** [frame] follows animation playback; [tick] counts logical host frames even while playback is
    paused. Fixed-step exports set both to their frame count. *)

val at_time : float -> t
(** A deterministic frame with the given time and other fields zero or empty. Hosts supply the full
    record; this constructor is for time-only tools. *)

val key_name : key -> string
val button_name : mouse_button -> string

val scroll_phase_name : scroll_phase -> string
(** Canonical names at the Lisp and web serialization boundaries. *)

val keys_before : previous:key list -> held:key list -> event list -> key list
(** Recover the initial held keys from the previous frame and final snapshot. *)

val keys_after : key list -> event -> key list
val validate : t -> (unit, string) result

val identity : t -> string
(** Exact identity of every frame field, including ordered events. *)

val equal : t -> t -> bool
(** Equality includes the exact bits of floating-point facts. *)

module Gate : sig
  type frame = t
  (** What a hosted sketch sees of a frame. The pointer is available only over its picture, and the
      keyboard only while a viewer or canvas has focus and nothing else owns it. *)

  type t = {
    prompt : bool;
    menu : bool;
    leader : bool;
    text_focus : bool;
    focus_on_picture : bool;
    pointer_on_picture : bool;
    window_key : bool;
    pressed : bool;
    previous : frame;
    blocked : key list;
  }

  val gate : t -> frame -> frame * key list
  (** Held keys stay hidden until released; a pointer away from the picture keeps its last place
      unless a press that began on the picture is still held. *)
end
