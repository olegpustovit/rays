type event = Input.event

open Input

type snapshot = {
  pointer : float * float;
  mouse_delta : float * float;

  visible : bool;

}

(* All-float records store their fields unboxed, so per-event updates below
   assign in place instead of allocating tuples and boxed floats. *)
type pair = { mutable px : float; mutable py : float }

type t = {
  max_events : int;
  events : event Queue.t;
  pointer : pair;
  mouse_delta : pair;
  mutable visible : bool;
  (* Relative (captured) pointer mode: motion deltas come from the device's
     relative motion, since absolute positions stop at the window edge. *)
  mutable relative : bool;
}

let create ~max_events ~logical_width ~logical_height =
  if max_events <= 0 then Error "max_events must be positive"
  else if logical_width <= 0 || logical_height <= 0 then Error "logical dimensions must be positive"
  else
    Ok
      {
        max_events;
        events = Queue.create ();
        pointer = { px = 0.; py = 0. };
        mouse_delta = { px = 0.; py = 0. };
        visible = true;
        relative = false;
      }

let apply value = function
  | Pointer_moved (x, y) ->
      let pointer = value.pointer and delta = value.mouse_delta in
      if not value.relative then begin
        delta.px <- delta.px +. x -. pointer.px;
        delta.py <- delta.py +. y -. pointer.py
      end;
      pointer.px <- x;
      pointer.py <- y
  | Pointer_pressed (_, (x, y)) | Pointer_released (_, (x, y)) ->
      value.pointer.px <- x;
      value.pointer.py <- y
  | Visibility_changed visible -> value.visible <- visible
  | Text_input _ | Text_editing _ | File_dropped _ | File_drag_moved _ | File_drag_ended
  | Pixel_size_changed _ | Pinched _ | Trackpad_scrolled _ | Focus_gained | Closed | File_dialog _ | Dialog_opened _
  | Pointer_cancelled _ | Scrolled _ | Key_pressed _ | Key_released _ | Focus_lost | Resized _ -> ()

let push value event =
  match event with
  | File_dropped path when path = "" || String.contains path '\000' ->
      Error "file-drop path is malformed"
  | (Resized (width, height) | Pixel_size_changed (width, height))
    when width <= 0 || height <= 0 ->
      Error "resize dimensions must be positive"
  | Pinched scale when not (Float.is_finite scale) || scale <= 0. ->
      Error "pinch scale must be finite and positive"
  | Trackpad_scrolled { delta = (x, y); time = seconds; _ }
    when not (Float.is_finite x && Float.is_finite y && Float.is_finite seconds) ->
      Error "scroll gesture must be finite"
  | _ ->
      apply value event;
      if Queue.length value.events = value.max_events then ignore (Queue.take value.events);
      Queue.add event value.events;
      Ok ()

let drain value =
  let events = List.rev (Queue.fold (fun acc event -> event :: acc) [] value.events) in
  Queue.clear value.events;
  events

let set_relative value relative = value.relative <- relative
let relative value = value.relative

let add_motion value ~dx ~dy =
  if value.relative then begin
    value.mouse_delta.px <- value.mouse_delta.px +. dx;
    value.mouse_delta.py <- value.mouse_delta.py +. dy
  end

let begin_frame value =
  value.mouse_delta.px <- 0.;
  value.mouse_delta.py <- 0.

let set_extent _value ~logical_width ~logical_height =
  if logical_width <= 0 || logical_height <= 0 then Error "logical dimensions must be positive"
  else Ok ()

let snapshot value =
  {
    pointer = (value.pointer.px, value.pointer.py);
    mouse_delta = (value.mouse_delta.px, value.mouse_delta.py);

    visible = value.visible;

  }
