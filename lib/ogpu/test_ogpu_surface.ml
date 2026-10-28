let fail message = raise (Failure message)
let ok = function Ok value -> value | Error e -> fail (Ogpu.Error.to_string e)

let expect kind = function
  | Error (e : Ogpu.Error.t) when e.kind = kind -> ()
  | _ -> fail "unexpected surface error"

let config =
  {
    Ogpu.Surface.logical_width = 64;
    logical_height = 48;
    physical_width = 128;
    physical_height = 96;
    format = Bgra8_unorm;
    present_mode = Fifo;
    max_acquired = 2;
    layer = None;
  }

let frame = function Ogpu.Surface.Acquired frame -> frame | _ -> fail "expected acquired frame"

let run () =
  let device = Ogpu.Handle.create_device () in
  expect Ogpu.Error.Invalid_argument
    (Ogpu.Surface.create device { config with max_acquired = 0; layer = None });
  let surface = ok (Ogpu.Surface.create device config) in
  let first = frame (ok (Ogpu.Surface.acquire surface))
  and second = frame (ok (Ogpu.Surface.acquire surface)) in
  expect Ogpu.Error.Capacity (Ogpu.Surface.acquire surface);
  ok (Ogpu.Surface.present surface first);
  expect Ogpu.Error.Invalid_state (Ogpu.Surface.present surface first);
  ok (Ogpu.Surface.discard surface second);
  expect Ogpu.Error.Invalid_state (Ogpu.Surface.discard surface second);
  let other = ok (Ogpu.Surface.create device config) in
  let foreign = frame (ok (Ogpu.Surface.acquire other)) in
  expect Ogpu.Error.Cross_device (Ogpu.Surface.present surface foreign);
  Ogpu.Surface.destroy surface;
  Ogpu.Surface.destroy surface;
  expect Ogpu.Error.Invalid_state (Ogpu.Surface.acquire surface);
  print_endline "OGPU surface lifecycle contract passed"
