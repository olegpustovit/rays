let tests =
  [
    ("test_pathtracer", fun () -> Test_pathtracer.run ());
    ("test_pathtracer_exact_m1", Test_pathtracer.run_exact_m1);
    ("test_gpu_film", Test_gpu_film.run);
    ("test_world", Test_world.run);
    ("test_parity", Test_parity.run);
    ("test_scene", Test_scene.run);
    ("bench_scene", Test_scene.bench);
    ("bench_world", Test_world.bench);
  ]

let () =
  match Array.to_list Sys.argv with
  | [ _; name ] -> (
      match List.assoc_opt name tests with
      | Some run -> run ()
      | None -> failwith ("unknown test: " ^ name))
  | _ -> failwith "usage: test_main <name>"
