let tests =
  [
    ("test_metal_argument_reflection_snapshot", Test_metal_argument_reflection_snapshot.run);
    ("test_metal_device_safe", Test_metal_device_safe.run);
    ("test_metal_texture_safe", Test_metal_texture_safe.run);
    ("test_metal_sampler_safe", Test_metal_sampler_safe.run);
    ("test_metal_function_constants_safe", Test_metal_function_constants_safe.run);
    ("test_metal_render_pipeline_safe", Test_metal_render_pipeline_safe.run);
    ("test_metal_resource_bounds", Test_metal_resource_bounds.run);
    ("test_metal_blit_safe", Test_metal_blit_safe.run);
    ("test_metal_device_library_safe", Test_metal_device_library_safe.run);
    ("test_metal_device_queue_safe", Test_metal_device_queue_safe.run);
    ("test_metal_render_encoder_safe", Test_metal_render_encoder_safe.run);
    ("test_metal_compute_encoder_safe", Test_metal_compute_encoder_safe.run);
    ("test_metal_command_buffer_safe", Test_metal_command_buffer_safe.run);
    ("test_metal_compute_pipeline_safe", Test_metal_compute_pipeline_safe.run);
    ("test_metal_render_icb_argument", Test_metal_render_icb_argument.run);
  ]

(* Each test runs in its own process: they assert on process-wide handle counts. *)
let () =
  match Array.to_list Sys.argv with
  | [ _; name ] -> (
      match List.assoc_opt name tests with
      | Some run -> run ()
      | None -> failwith ("unknown test: " ^ name))
  | _ -> failwith "usage: test_main <name>"
