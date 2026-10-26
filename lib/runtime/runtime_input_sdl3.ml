let button : Sdl3.Event.mouse_button -> Runtime_input.mouse_button = function
  | Left -> Left
  | Middle -> Middle
  | Right -> Right
  | X1 -> X1
  | X2 -> X2

(* A zero or negative size is what a minimizing window can report; the next
   real size follows, so it is dropped rather than treated as an error. *)
let positive width height = width > 0 && height > 0

let translate : Sdl3.Event.t -> Runtime_input.event option = function
  | Mouse_motion { x; y; _ } -> Some (Pointer_moved (x, y))
  | Mouse_button { button = raw; down; x; y } ->
      let value = button raw in
      Some (if down then Pointer_pressed (value, x, y) else Pointer_released (value, x, y))
  | Mouse_wheel { x; y; _ } -> Some (Wheel (x, y))
  | Scroll { x; y; phase; seconds } -> Some (Scroll { x; y; phase; seconds })
  | Key { key; down; _ } ->
      let event = { Runtime_input.key;  } in
      Some (if down then Key_pressed event else Key_released event)
  | Text_input text -> Some (Text_input text)
  | Text_editing { text; start; length } -> Some (Text_editing { text; start; length })
  | Window (Resized (width, height)) when positive width height ->
      Some (Resized (width, height))
  | Window (Pixel_size_changed (width, height)) when positive width height ->
      Some (Pixel_size_changed (width, height))
  | Window (Resized _ | Pixel_size_changed _) -> None
  | Window Focus_lost -> Some Focus_lost
  | Window Focus_gained -> Some Focus_gained
  | Window (Shown | Restored) -> Some (Visibility_changed true)
  (* SDL sends an event when a window becomes occluded and none when it stops:
     the runtime reads the window's state to see it end (Runtime.visible). *)
  | Window (Hidden | Minimized | Occluded) -> Some (Visibility_changed false)
  | Window Close_requested -> Some Quit
  | Quit -> Some Quit
  | Pinch { phase = Updated; scale } -> Some (Pinch scale)
  | Pinch { phase = Began | Ended; _ } -> None
  | Drop { change = File path; _ } -> Some (File_dropped path)
  | Drop { change = Drop_position; x; y } -> Some (File_dragged (x, y))
  | Drop { change = Drop_complete; _ } -> Some File_drag_ended
  | Drop { change = Drop_begin; _ } -> None
  | Dialog { id; outcome = Chosen paths } -> Some (Dialog_closed { id; result = Ok paths })
  | Dialog { id; outcome = Cancelled } -> Some (Dialog_closed { id; result = Ok [] })
  | Dialog { id; outcome = Failed message } -> Some (Dialog_closed { id; result = Error message })

let push value (event : Sdl3.Event.t) =
  match event with
  | Mouse_motion { x; y; dx; dy } ->
      Runtime_input.add_motion value ~dx ~dy;
      Runtime_input.push value (Pointer_moved (x, y))
  | _ -> (
      match translate event with
      | None -> Ok ()
      | Some event -> Runtime_input.push value event)

(* One bad event never drops the rest of the batch; the first error is
   returned. *)
let push_all value events =
  List.fold_left
    (fun first event ->
      match (push value event, first) with
      | (Error _ as error), Ok () -> error
      | _ -> first)
    (Ok ()) events

let pump value =
  match Sdl3.Event.poll_coalesced () with
  | Error error -> Error (Format.asprintf "%a" Sdl3.pp_error error)
  | Ok events -> push_all value events

let wait ~timeout_ms = Result.map_error (Format.asprintf "%a" Sdl3.pp_error) (Sdl3.Event.wait ~timeout_ms)
