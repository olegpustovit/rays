open Sdl3_mixer

let fail message = failwith ("SDL3_mixer device-failure test: " ^ message)
let get = function Ok value -> value | Error error -> fail (Format.asprintf "%a" pp_error error)

let run () =
  get (Init.init ());
  (match Mixer.create_device () with
  | Error { kind = Mixer_error; message; _ } when message <> "" -> ()
  | Ok mixer ->
      get (Mixer.destroy mixer);
      fail "invalid SDL audio driver unexpectedly opened a device mixer"
  | Error _ -> fail "device failure returned the wrong typed error");
  get (Init.quit ());
  print_endline "SDL3_mixer typed device failure passed"
