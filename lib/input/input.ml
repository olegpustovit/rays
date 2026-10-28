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

let at_time t =
  {
    t;
    dt = 0.;
    frame = 0;
    tick = 0;
    size = (0, 0);
    pointer = (0., 0.);
    buttons = [];
    keys = [];
    events = [];
  }

let key_name = function
  | KeyChar c -> String.make 1 (Char.lowercase_ascii c)
  | ArrowUp -> "up"
  | ArrowDown -> "down"
  | ArrowLeft -> "left"
  | ArrowRight -> "right"
  | Space -> "space"
  | Enter -> "enter"
  | Escape -> "escape"
  | Backspace -> "backspace"
  | Tab -> "tab"
  | Shift -> "shift"
  | Ctrl -> "ctrl"
  | Alt -> "alt"
  | Meta -> "meta"
  | F1 -> "f1"
  | F2 -> "f2"
  | F3 -> "f3"
  | F4 -> "f4"
  | F5 -> "f5"
  | F6 -> "f6"
  | F7 -> "f7"
  | F8 -> "f8"
  | F9 -> "f9"
  | F10 -> "f10"
  | F11 -> "f11"
  | F12 -> "f12"
  | Home -> "home"
  | End -> "end"
  | PageUp -> "pageup"
  | PageDown -> "pagedown"
  | Insert -> "insert"
  | Delete -> "delete"
  | Unknown n -> "unknown:" ^ string_of_int n

let button_name = function
  | LeftButton -> "left"
  | RightButton -> "right"
  | MiddleButton -> "middle"
  | MouseX1 -> "x1"
  | MouseX2 -> "x2"

let scroll_phase_name = function
  | Touched -> "touched"
  | Moved -> "moved"
  | Lifted -> "lifted"
  | Momentum -> "momentum"

let add key keys = if List.mem key keys then keys else key :: keys

let keys_after keys = function
  | Key_pressed key -> add key keys
  | Key_released key -> List.filter (( <> ) key) keys
  | Focus_lost -> []
  | _ -> keys

let keys_before ~previous ~held events =
  List.fold_left
    (fun keys event ->
      match event with
      | Key_pressed key | Key_released key ->
          if List.mem key previous then add key keys else List.filter (( <> ) key) keys
      | Focus_lost -> previous
      | _ -> keys)
    held events

let validate v =
  let x, y = v.pointer and w, h = v.size in
  let pair (x, y) = Float.is_finite x && Float.is_finite y in
  let event = function
    | Pointer_moved (x, y) | Scrolled (x, y) | File_drag_moved (x, y) -> pair (x, y)
    | Pointer_pressed (_, p) | Pointer_released (_, p) -> pair p
    | Pinched f -> Float.is_finite f
    | Trackpad_scrolled { delta; time; _ } -> pair delta && Float.is_finite time
    | Text_editing { start; length; _ } -> start >= 0 && length >= 0
    | Resized (w, h) -> w >= 0 && h >= 0
    | _ -> true
  in
  if
    (not (Float.is_finite v.t && Float.is_finite v.dt && Float.is_finite x && Float.is_finite y))
    || not (List.for_all event v.events)
  then Error "Frame facts must be finite; event dimensions and text ranges must be non-negative."
  else if v.dt < 0. || v.frame < 0 || v.tick < 0 || w < 0 || h < 0 then
    Error "Frame step, index and dimensions must be non-negative."
  else Ok ()

let identity v = Marshal.to_string v []

let equal a b =
  let bits x y = Int64.bits_of_float x = Int64.bits_of_float y in
  a == b
  || bits a.t b.t && bits a.dt b.dt && a.frame = b.frame && a.tick = b.tick && a.size = b.size
     && bits (fst a.pointer) (fst b.pointer)
     && bits (snd a.pointer) (snd b.pointer)
     && a.buttons = b.buttons && a.keys = b.keys
     && (a.events == b.events
        || (a.events = b.events && Marshal.to_string a.events [] = Marshal.to_string b.events []))

module Gate = struct
  type frame = t

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

  let pointer_event = function
    | Pointer_moved _ | Pointer_pressed _ | Pointer_released _ | Pointer_cancelled _ | Scrolled _
    | Pinched _ | Trackpad_scrolled _ ->
        true
    | _ -> false

  let key_event = function
    | Key_pressed _ | Key_released _ | Text_input _ | Text_editing _ -> true
    | _ -> false

  let gate g (live : frame) =
    let on_picture = (not g.prompt) && (not g.menu) && g.pointer_on_picture && g.window_key in
    let live =
      if g.previous.buttons <> [] || ((live.buttons = [] || g.pressed) && on_picture) then live
      else
        {
          live with
          pointer = g.previous.pointer;
          buttons = [];
          events = List.filter (fun e -> not (pointer_event e)) live.events;
        }
    in
    if g.prompt || g.menu || g.leader || g.text_focus || not g.focus_on_picture then
      ( { live with keys = []; events = List.filter (fun e -> not (key_event e)) live.events },
        live.keys )
    else
      let blocked = List.filter (fun k -> List.mem k g.blocked) live.keys in
      ( {
          live with
          keys = List.filter (fun k -> not (List.mem k blocked)) live.keys;
          events =
            List.filter
              (function Key_pressed k | Key_released k -> not (List.mem k blocked) | _ -> true)
              live.events;
        },
        blocked )
end
