let tests =
  [
    ("test_offscreen", Test_offscreen.run);
    ("test_scene2_coordinates", Test_scene2_coordinates.run);
    ("test_dense_scene2", Test_dense_scene2.run);
    ("test_ui_pipeline", Test_ui_pipeline.run);
    ("test_marks_native", Test_marks_native.run);
    ("test_batch_merge", Test_batch_merge.run);
    ("test_shapes_lowering", Test_shapes_lowering.run);
    ("test_runtime_render", Test_runtime_render.run);
    ("test_scene2_argument", Test_scene2_argument.run);
    ("test_scene3_instances", Test_scene3_instances.run);
    ("test_scene3_lighting", Test_scene3_lighting.run);
  ]

let () =
  match Array.to_list Sys.argv with
  | [ _; name ] -> (
      match List.assoc_opt name tests with
      | Some run -> run ()
      | None -> failwith ("unknown test: " ^ name))
  | _ -> failwith "usage: test_main <name>"
