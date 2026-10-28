open Ogpu.Binding

let fail message = raise (Failure message)
let ok = function Ok value -> value | Error error -> fail (Ogpu.Error.to_string error)

let expect kind = function
  | Error (error : Ogpu.Error.t) when error.kind = kind -> ()
  | Error error -> fail ("unexpected error: " ^ Ogpu.Error.to_string error)
  | Ok _ -> fail "expected rejection"

let run () =
  let caps = Ogpu.Caps.minimum_m1 in
  let device = Ogpu.Handle.create_device () in
  let foreign = Ogpu.Handle.create_device () in
  let _buffer = Ogpu.Handle.create ~device
  and texture = Ogpu.Handle.create ~device
  and _sampler = Ogpu.Handle.create ~device
  and _foreign_buffer = Ogpu.Handle.create ~device:foreign in
  expect Ogpu.Error.Invalid_argument
    (Ogpu.Binding.create_layout
       [
         { binding = 0; kind = Buffer; visibility = [ Vertex ] };
         { binding = 0; kind = Texture; visibility = [ Fragment ] };
       ]);
  expect Ogpu.Error.Invalid_argument
    (Ogpu.Binding.create_layout [ { binding = 0; kind = Buffer; visibility = [] } ]);
  expect Ogpu.Error.Invalid_argument
    (Ogpu.Binding.create_layout [ { binding = 0; kind = Buffer; visibility = [ Vertex; Vertex ] } ]);
  Ogpu.Handle.destroy texture;
  let too_many =
    List.init (caps.limits.max_bind_groups + 1) (fun group ->
        (group, ok (Ogpu.Binding.create_layout [])))
  in
  expect Ogpu.Error.Capacity
    (Ogpu.Binding.create_pipeline_layout ~device ~capabilities:caps too_many);
  expect Ogpu.Error.Invalid_argument
    (Ogpu.Binding.create_pipeline_layout ~device ~capabilities:caps
       [ (1, ok (Ogpu.Binding.create_layout [])) ]);
  expect Ogpu.Error.Invalid_argument
    (Ogpu.Binding.create_pipeline_layout ~device ~capabilities:caps
       [ (0, ok (Ogpu.Binding.create_layout [])); (0, ok (Ogpu.Binding.create_layout [])) ]);
  Ogpu.Handle.destroy_device device;
  Ogpu.Handle.destroy_device foreign;
  print_endline "OGPU immutable binding tables passed"
