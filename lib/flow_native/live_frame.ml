open Rays

let key = Input.key_name
let button = Input.button_name

let of_frame (f : Frame.t) : Input.t =
  {
    f.input with
    (* the window's focus is the editor's: a sketch reads only its loss *)
    events = List.filter (( <> ) Input.Focus_gained) f.input.events;
  }
