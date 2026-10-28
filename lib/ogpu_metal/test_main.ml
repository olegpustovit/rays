let tests =
  [
    ("test_ogpu_metal_foundation", Test_ogpu_metal_foundation.run);
    ("test_ogpu_metal_texture", Test_ogpu_metal_texture.run);
    ("test_ogpu_metal_pipeline", Test_ogpu_metal_pipeline.run);
    ("test_ogpu_metal_acceleration", Test_ogpu_metal_acceleration.run);
    ("test_ogpu_metal_capabilities", Test_ogpu_metal_capabilities.run);
    ("test_ogpu_metal_sampler_pixels", Test_ogpu_metal_sampler_pixels.run);
    ("test_pipeline_argument_buffer", Test_pipeline_argument_buffer.run);
  ]

let () =
  match Array.to_list Sys.argv with
  | [ _; name ] -> (
      match List.assoc_opt name tests with
      | Some run -> run ()
      | None -> failwith ("unknown test: " ^ name))
  | _ -> failwith "usage: test_main <name>"
