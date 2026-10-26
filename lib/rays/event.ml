type scroll_phase = Touched | Moved | Lifted | Momentum
type t = KeyPressed of Input.key | KeyReleased of Input.key | MouseMoved of (float*float)
  | MousePressed of Input.mouse_button*(float*float) | MouseReleased of Input.mouse_button*(float*float)
  | PointerCancelled of Input.mouse_button | MouseScrolled of (float*float) | TextInput of string
  | TextEditing of {text:string;start:int;length:int} | FileDropped of string
  | FileDragMoved of (float*float) | FileDragEnded | MousePinched of float
  | FileDialog of {id:int;result:(string list,string) result}
  | WindowResized of (int*int) | WindowFocusLost | WindowFocusGained | WindowClosed
  | TrackpadScrolled of {delta:float*float;phase:scroll_phase;time:float}
module Private = struct
  (* The binding names a key once, from SDL's keycode; this is only the move
     to Rays's own variant. *)
  let key_of_runtime : Runtime_input.key -> Input.key = function
    | Char character -> KeyChar character
    | Arrow_up -> ArrowUp | Arrow_down -> ArrowDown
    | Arrow_left -> ArrowLeft | Arrow_right -> ArrowRight
    | Space -> Space | Enter -> Enter | Escape -> Escape
    | Backspace -> Backspace | Tab -> Tab
    | Shift -> Shift | Control -> Ctrl | Alt -> Alt | Meta -> Meta
    | F1 -> F1 | F2 -> F2 | F3 -> F3 | F4 -> F4 | F5 -> F5 | F6 -> F6
    | F7 -> F7 | F8 -> F8 | F9 -> F9 | F10 -> F10 | F11 -> F11 | F12 -> F12
    | Home -> Home | End -> End | Page_up -> PageUp | Page_down -> PageDown
    | Insert -> Insert | Delete -> Delete
    | Unknown code -> Unknown code
  let add key keys = if List.mem key keys then keys else key :: keys
  let keys_after keys = function
    | KeyPressed key -> add key keys
    | KeyReleased key -> List.filter (( <> ) key) keys
    | WindowFocusLost -> []
    | _ -> keys

  let keys_before ~previous ~held events =
    List.fold_left (fun keys event -> match event with
      | KeyPressed key | KeyReleased key ->
          if List.mem key previous then add key keys else List.filter (( <> ) key) keys
      | WindowFocusLost -> previous
      | _ -> keys) held events
end
