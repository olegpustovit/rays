let tests =
  [
    ("test_sdl3", Test_sdl3.run);
    ("test_sdl3_metal", Test_sdl3_metal.run);
    ("test_sdl3_stress", Test_sdl3_stress.run);
    ("test_sdl3_failure", Test_sdl3_failure.run);
    ("test_sdl3_events", Test_sdl3_events.run);
  ]

let () =
  match Array.to_list Sys.argv with
  | [ _; name ] -> (
      match List.assoc_opt name tests with
      | Some run -> run ()
      | None -> failwith ("unknown test: " ^ name))
  | _ -> failwith "usage: test_main <name>"
