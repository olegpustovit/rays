(* Per-sketch native input source and the held state folded into each frame. *)

module KeySet = Set.Make (struct
  type t = Input.key

  let compare = compare
end)

module ButtonSet = Set.Make (struct
  type t = Input.mouse_button

  let compare = compare
end)

type t = {
  source : Runtime_input.t;
  mutable pressed_keys : KeySet.t;
  mutable pressed_buttons : ButtonSet.t;
  mutable mouse : float * float;
  mutable delta : float * float;
}

let create () =
  match Runtime_input.create ~max_events:4096 ~logical_width:1 ~logical_height:1 with
  | Ok source ->
      Ok
        {
          source;
          pressed_keys = KeySet.empty;
          pressed_buttons = ButtonSet.empty;
          mouse = (0., 0.);
          delta = (0., 0.);
        }
  | Error error -> Error error

let move state x y =
  let cx, cy = state.mouse and dx, dy = state.delta in
  state.delta <- (dx +. x -. cx, dy +. y -. cy);
  state.mouse <- (x, y)

let apply state : Input.event -> unit = function
  | Key_pressed key -> state.pressed_keys <- KeySet.add key state.pressed_keys
  | Key_released key -> state.pressed_keys <- KeySet.remove key state.pressed_keys
  | Pointer_moved (x, y) -> move state x y
  | Pointer_pressed (button, (x, y)) ->
      move state x y;
      state.pressed_buttons <- ButtonSet.add button state.pressed_buttons
  | Pointer_released (button, (x, y)) ->
      move state x y;
      state.pressed_buttons <- ButtonSet.remove button state.pressed_buttons
  | Pointer_cancelled button ->
      state.pressed_buttons <- ButtonSet.remove button state.pressed_buttons
  | Focus_lost ->
      state.pressed_keys <- KeySet.empty;
      state.pressed_buttons <- ButtonSet.empty
  | _ -> ()

let configure state ~logical_width ~logical_height =
  match Runtime_input.set_extent state.source ~logical_width ~logical_height with
  | Ok () -> Ok ()
  | Error _ as error -> error

let set_relative state enabled = Runtime_input.set_relative state.source enabled

let set_visible state visible =
  ignore (Runtime_input.push state.source (Input.Visibility_changed visible))

type polled = { events : Input.event list; window_changed : bool; visible : bool }

let wait _state ~timeout_ms = ignore (Runtime_input_sdl3.wait ~timeout_ms)

let poll state =
  state.delta <- (0., 0.);
  Runtime_input.begin_frame state.source;
  let pumped = Runtime_input_sdl3.pump state.source in
  let native = Runtime_input.drain state.source in
  let window_changed =
    List.exists (function Input.Resized _ | Pixel_size_changed _ -> true | _ -> false) native
  in
  let events =
    List.filter
      (function Input.Pixel_size_changed _ | Visibility_changed _ -> false | _ -> true)
      native
    |> List.map (fun event ->
        apply state event;
        event)
  in
  if Runtime_input.relative state.source then
    state.delta <- (Runtime_input.snapshot state.source).mouse_delta;
  Result.map
    (fun () -> { events; window_changed; visible = (Runtime_input.snapshot state.source).visible })
    pumped

let mouse state = state.mouse
let mouse_delta state = state.delta
let keys state = KeySet.elements state.pressed_keys
let buttons state = ButtonSet.elements state.pressed_buttons
