type t = { input : Input.t; pixel_scale : float * float; fps : float; mouse_delta : float * float }

let mouse_down button frame = List.mem button frame.input.buttons
let has_event predicate frame = List.exists predicate frame.input.events
