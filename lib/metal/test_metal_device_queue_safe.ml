open Metal

let get = function Ok value -> value | Error error -> failwith (Format.asprintf "%a" pp_error error)

let run () = match Device.system_default() with
| Error _ -> print_endline "Device queue: skipped (no device)"
| Ok device ->
  let queue=get(Command_queue.create device)in
  get(Command_queue.destroy queue);
  get(Device.destroy device);print_endline"Device queue: ownership passed"
