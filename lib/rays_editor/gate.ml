(* What the sketch sees of a frame.  The sketch gets the pointer only on its picture, and only while
   the window is the key window (a pointer passing over it from another app, or the click that
   brings it forward, is not the sketch's): elsewhere
   (or under a menu or prompt) the pointer stays where the picture last had it, no button is down
   and its events are dropped; a press that began on the picture is the sketch's until its release,
   and one that began elsewhere never becomes its drag.  It gets the keyboard only while a viewer
   or canvas has the focus and no prompt, menu, half-typed leader or text field is open: the Esc
   that closes one of those, and the letters typed in the graph or text pane, are not for it.  A
   key held while the keyboard was away stays hidden until it is released. *)
type t = {
  prompt : bool; menu : bool; leader : bool; text_focus : bool;
  focus_on_picture : bool;  (* a viewer or canvas has the focus *)
  pointer_on_picture : bool;  (* the pane under the pointer is a viewer or canvas, or none *)
  window_key : bool;  (* the window has been the key window for a frame: not hovered from behind, not the click that raised it *)
  pressed : bool;  (* a button went down this frame *)
  previous : Frame_input.t;  (* what the sketch saw last frame *)
  blocked : string list;  (* the keys hidden last frame *)
}

let pointer_event = function
  | Frame_input.Pointer_moved _ | Pointer_pressed _ | Pointer_released _ | Pointer_cancelled _
  | Scrolled _ | Pinched _ | Trackpad_scrolled _ -> true
  | _ -> false

let key_event = function
  | Frame_input.Key_pressed _ | Key_released _ | Text_input _ | Text_editing _ -> true
  | _ -> false

(* the frame the sketch sees, and the keys now hidden from it *)
let gate g (live : Frame_input.t) =
  let on_picture = not g.prompt && not g.menu && g.pointer_on_picture && g.window_key in
  let live =
    if g.previous.buttons <> [] || ((live.buttons = [] || g.pressed) && on_picture) then live
    else { live with pointer = g.previous.pointer; buttons = [];
      events = List.filter (fun e -> not (pointer_event e)) live.events } in
  if g.prompt || g.menu || g.leader || g.text_focus || not g.focus_on_picture then
    { live with keys = []; events = List.filter (fun e -> not (key_event e)) live.events }, live.keys
  else
    let blocked = List.filter (fun k -> List.mem k g.blocked) live.keys in
    { live with keys = List.filter (fun k -> not (List.mem k blocked)) live.keys;
      events = List.filter (function
        | Frame_input.Key_pressed k | Key_released k -> not (List.mem k blocked)
        | _ -> true) live.events }, blocked
