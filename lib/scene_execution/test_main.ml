let tests =
  [
    ("test_prepared_scene3", Test_prepared_scene3.run);
    ("bench_prepared_scene3", Test_prepared_scene3.bench);
    ("test_automatic_scratch", Test_automatic_scratch.run);
    ("test_retained_eviction", Test_retained_eviction.run);
    ("test_offscreen_metal", Test_offscreen_metal.run);
    ("test_scene_execution_metal", Test_scene_execution_metal.run);
  ]

let () =
  match Array.to_list Sys.argv with
  | [ _; name ] -> (
      match List.assoc_opt name tests with
      | Some run -> run ()
      | None -> failwith ("unknown test: " ^ name))
  | _ -> failwith "usage: test_main <name>"
