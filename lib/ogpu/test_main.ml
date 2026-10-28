let tests =
  [
    ("test_ogpu_foundation", Test_ogpu_foundation.run);
    ("test_ogpu_shader", Test_ogpu_shader.run);
    ("test_ogpu_surface", Test_ogpu_surface.run);
    ("test_ogpu_binding", Test_ogpu_binding.run);
    ("test_ogpu_pipeline", Test_ogpu_pipeline.run);
    ("test_ogpu_acceleration", Test_ogpu_acceleration.run);
    ("test_ogpu_validation", Test_ogpu_validation.run);
  ]

let () =
  match Array.to_list Sys.argv with
  | [ _; name ] -> (
      match List.assoc_opt name tests with
      | Some run -> run ()
      | None -> failwith ("unknown test: " ^ name))
  | _ -> failwith "usage: test_main <name>"
