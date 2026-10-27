open Metal
let get = function Ok value -> value | Error error -> failwith error.message
let reject = function Error _ -> () | Ok _ -> failwith "expected rejection"
let run () =
  let device = get (Device.system_default ()) in
  let queue = get (Command_queue.create device) in
  let commands = get (Command_buffer.create queue ()) in
  let blit = get (Blit_encoder.create commands) in
  let source = get (Buffer.create ~device ~length:64L ~storage:Buffer.Shared ()) in
  let destination = get (Buffer.create ~device ~length:64L ~storage:Buffer.Shared ()) in
  get (Blit_encoder.end_encoding blit);
  get (Command_buffer.commit commands);
  get (Command_buffer.wait_until_completed commands);
  get (Command_buffer.destroy commands);
  get (Buffer.destroy destination); get (Buffer.destroy source);
  get (Command_queue.destroy queue); get (Device.destroy device);
  print_endline "BlitCommand safe conformance: ok"
