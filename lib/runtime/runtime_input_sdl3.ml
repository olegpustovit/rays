let button : Sdl3.Event.mouse_button -> Input.mouse_button = function
  | Left -> LeftButton
  | Middle -> MiddleButton
  | Right -> RightButton
  | X1 -> MouseX1
  | X2 -> MouseX2

let key : Sdl3.Key.t -> Input.key = function
  | Char character -> KeyChar character
  | Arrow_up -> ArrowUp
  | Arrow_down -> ArrowDown
  | Arrow_left -> ArrowLeft
  | Arrow_right -> ArrowRight
  | Space -> Space
  | Enter -> Enter
  | Escape -> Escape
  | Backspace -> Backspace
  | Tab -> Tab
  | Shift -> Shift
  | Control -> Ctrl
  | Alt -> Alt
  | Meta -> Meta
  | F1 -> F1
  | F2 -> F2
  | F3 -> F3
  | F4 -> F4
  | F5 -> F5
  | F6 -> F6
  | F7 -> F7
  | F8 -> F8
  | F9 -> F9
  | F10 -> F10
  | F11 -> F11
  | F12 -> F12
  | Home -> Home
  | End -> End
  | Page_up -> PageUp
  | Page_down -> PageDown
  | Insert -> Insert
  | Delete -> Delete
  | Unknown code -> Unknown code

let scroll_phase : Sdl3.Event.scroll_phase -> Input.scroll_phase = function
  | Scroll_began -> Touched
  | Scroll_changed -> Moved
  | Scroll_ended -> Lifted
  | Scroll_momentum -> Momentum

(* A zero or negative size is what a minimizing window can report; the next
   real size follows, so it is dropped rather than treated as an error. *)
let positive width height = width > 0 && height > 0

let translate : Sdl3.Event.t -> Runtime_input.event option = function
  | Mouse_motion { x; y; _ } -> Some (Pointer_moved (x, y))
  | Mouse_button { button = raw; down; x; y } ->
      let value = button raw in
      Some (if down then Pointer_pressed (value, (x, y)) else Pointer_released (value, (x, y)))
  | Mouse_wheel { x; y; _ } -> Some (Scrolled (x, y))
  | Scroll { x; y; phase; seconds } ->
      Some (Trackpad_scrolled { delta = (x, y); phase = scroll_phase phase; time = seconds })
  | Key { key = raw; down; _ } ->
      let value = key raw in
      Some (if down then Key_pressed value else Key_released value)
  | Text_input text -> Some (Text_input text)
  | Text_editing { text; start; length } -> Some (Text_editing { text; start; length })
  | Window (Resized (width, height)) when positive width height -> Some (Resized (width, height))
  | Window (Pixel_size_changed (width, height)) when positive width height ->
      Some (Pixel_size_changed (width, height))
  | Window (Resized _ | Pixel_size_changed _) -> None
  | Window Focus_lost -> Some Focus_lost
  | Window Focus_gained -> Some Focus_gained
  | Window (Shown | Restored) -> Some (Visibility_changed true)
  (* SDL sends an event when a window becomes occluded and none when it stops:
     the runtime reads the window's state to see it end (Runtime.visible). *)
  | Window (Hidden | Minimized | Occluded) -> Some (Visibility_changed false)
  | Window Close_requested -> Some Closed
  | Quit -> Some Closed
  | Pinch { phase = Updated; scale } -> Some (Pinched scale)
  | Pinch { phase = Began | Ended; _ } -> None
  | Drop { change = File path; _ } -> Some (File_dropped path)
  | Drop { change = Drop_position; x; y } -> Some (File_drag_moved (x, y))
  | Drop { change = Drop_complete; _ } -> Some File_drag_ended
  | Drop { change = Drop_begin; _ } -> None
  | Dialog { id; outcome = Chosen paths } -> Some (File_dialog { id; result = Ok paths })
  | Dialog { id; outcome = Cancelled } -> Some (File_dialog { id; result = Ok [] })
  | Dialog { id; outcome = Failed message } -> Some (File_dialog { id; result = Error message })

let push value (event : Sdl3.Event.t) =
  match event with
  | Mouse_motion { x; y; dx; dy } ->
      Runtime_input.add_motion value ~dx ~dy;
      Runtime_input.push value (Pointer_moved (x, y))
  | _ -> (
      match translate event with None -> Ok () | Some event -> Runtime_input.push value event)

(* One bad event never drops the rest of the batch; the first error is
   returned. *)
let push_all value events =
  List.fold_left
    (fun first event ->
      match (push value event, first) with (Error _ as error), Ok () -> error | _ -> first)
    (Ok ()) events

let pump value =
  match Sdl3.Event.poll_coalesced () with
  | Error error -> Error (Format.asprintf "%a" Sdl3.pp_error error)
  | Ok events -> push_all value events

let wait ~timeout_ms =
  Result.map_error (Format.asprintf "%a" Sdl3.pp_error) (Sdl3.Event.wait ~timeout_ms)
