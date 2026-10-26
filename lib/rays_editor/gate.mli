(** What the hosted sketch sees of a frame: the pointer only over its picture, the keyboard only
    while a viewer or canvas has the focus and nothing else (prompt, menu, leader, text field)
    owns it. Pure, so the Esc / q matrix is a table test. *)

type t = {
  prompt : bool; menu : bool; leader : bool; text_focus : bool;
  focus_on_picture : bool;  (** a viewer or canvas has the focus *)
  pointer_on_picture : bool;  (** the pane under the pointer is a viewer or canvas, or none *)
  window_key : bool;  (** the window has been the key window for a frame: not hovered from behind another app, not the click that raised it *)
  pressed : bool;  (** a button went down this frame *)
  previous : Frame_input.t;  (** what the sketch saw last frame: its pointer and held buttons *)
  blocked : string list;  (** the keys hidden last frame *)
}

val gate : t -> Frame_input.t -> Frame_input.t * string list
(** The frame the sketch sees and the keys now hidden from it: held keys stay hidden until
    released, a pointer away from the picture keeps its last place unless a press that began on
    the picture is still held. *)
