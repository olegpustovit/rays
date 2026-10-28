let tests =
  [
    ("test_sdl3_mixer", Test_sdl3_mixer.run);
    ("test_sdl3_mixer_device_failure", Test_sdl3_mixer_device_failure.run);
  ]

let () =
  match Array.to_list Sys.argv with
  | [ _; name ] -> (
      match List.assoc_opt name tests with
      | Some run -> run ()
      | None -> failwith ("unknown test: " ^ name))
  | _ -> failwith "usage: test_main <name>"
