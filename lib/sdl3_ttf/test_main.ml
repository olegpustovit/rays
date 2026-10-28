let tests =
  [
    ("test_sdl3_ttf", Test_sdl3_ttf.run);
    ("test_sdl3_ttf_discovery_failure", Test_sdl3_ttf_discovery_failure.run);
  ]

let () =
  match Array.to_list Sys.argv with
  | [ _; name ] -> (
      match List.assoc_opt name tests with
      | Some run -> run ()
      | None -> failwith ("unknown test: " ^ name))
  | _ -> failwith "usage: test_main <name>"
