open Rays

let key = function
  | Input.KeyChar c -> String.make 1 (Char.lowercase_ascii c)
  | ArrowUp -> "up" | ArrowDown -> "down" | ArrowLeft -> "left" | ArrowRight -> "right"
  | Space -> "space" | Enter -> "enter" | Escape -> "escape" | Backspace -> "backspace" | Tab -> "tab"
  | Shift -> "shift" | Ctrl -> "ctrl" | Alt -> "alt" | Meta -> "meta"
  | F1 -> "f1" | F2 -> "f2" | F3 -> "f3" | F4 -> "f4" | F5 -> "f5" | F6 -> "f6"
  | F7 -> "f7" | F8 -> "f8" | F9 -> "f9" | F10 -> "f10" | F11 -> "f11" | F12 -> "f12"
  | Home -> "home" | End -> "end" | PageUp -> "pageup" | PageDown -> "pagedown"
  | Insert -> "insert" | Delete -> "delete" | Unknown n -> "unknown:" ^ string_of_int n
let button = function
  | Input.LeftButton -> "left" | RightButton -> "right" | MiddleButton -> "middle"
  | MouseX1 -> "x1" | MouseX2 -> "x2"
let event = function
  | Event.KeyPressed k -> Frame_input.Key_pressed (key k)
  | KeyReleased k -> Key_released (key k)
  | MouseMoved (x, y) -> Pointer_moved (x, y)
  | MousePressed (b, p) -> Pointer_pressed (button b, p)
  | MouseReleased (b, p) -> Pointer_released (button b, p)
  | PointerCancelled b -> Pointer_cancelled (button b)
  | MouseScrolled (x, y) -> Scrolled (x, y)
  | TextInput s -> Text_input s
  | TextEditing {text; start; length} -> Text_editing {text; start; length}
  | FileDropped s -> File_dropped s
  | FileDragMoved (x, y) -> File_drag_moved (x, y)
  | FileDragEnded -> File_drag_ended
  | MousePinched x -> Pinched x
  | FileDialog {id; result} -> File_dialog {id; result}
  | WindowResized (w, h) -> Resized (w, h)
  | WindowFocusLost | WindowFocusGained -> Focus_lost | WindowClosed -> Closed  (* gained never reaches here: [of_frame] drops it *)
  | TrackpadScrolled {delta; phase; time} -> Trackpad_scrolled {delta; time;
      phase = (match phase with Touched -> "touched" | Moved -> "moved" | Lifted -> "lifted" | Momentum -> "momentum")}

let of_frame (f : Frame.t) : Frame_input.t = {
  t = f.time; dt = f.dt;
  frame = f.count; tick = f.count; size = f.size; pointer = f.mouse;
  buttons = List.map button f.mouse_buttons; keys = List.map key f.keys;
  (* the window's focus is the editor's: a sketch reads only its loss *)
  events = List.map event (List.filter (( <> ) Event.WindowFocusGained) f.events);
}
