let tests = [
  "test_light", Test_light.run;
  "dump_light", Test_light.dump;
  "bench_light", Test_light.bench;
  "test_easy_camera", Test_easy_camera.run;
  "dump_easy_camera", Test_easy_camera.dump;
  "bench_easy_camera", Test_easy_camera.bench;
  "test_scene_rounded_cache", Test_scene_rounded_cache.run;
  "test_automatic_text_cache", Test_automatic_text_cache.run;
  "test_scene3_float32", Test_scene3_float32.run;
  "test_scene3_float32_native", Test_scene3_float32.run_native;
  "test_scene_display_list", Test_scene_display_list.run;
  "test_canvas_native", Test_canvas_native.run;
  "test_world", Test_world.run;
  "test_rays_api", Test_rays_api.run;
  "test_scene3_native_lowering", Test_scene3_native_lowering.run;
  "test_scene3_native_lowering_native", Test_scene3_native_lowering.run_native;
  "test_world_raster", Test_world_raster.run;
  "test_retained_view_native", Test_retained_view_native.run;
]

let () =
  match Array.to_list Sys.argv with
  | _ :: name :: _ ->
      (match List.assoc_opt name tests with
       | Some run -> run ()
       | None -> failwith ("unknown test: " ^ name))
  | _ -> failwith "usage: test_main <name>"
