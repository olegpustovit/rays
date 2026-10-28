let tests =
  [
    ("test_runtime_resources", Test_runtime_resources.run);
    ("test_resources_font", Test_resources_font.run);
    ("test_resources_audio", Test_resources_audio.run);
    ("test_png_rows", Test_png_rows.run);
  ]

let () =
  match Array.to_list Sys.argv with
  | [ _; name ] -> (
      match List.assoc_opt name tests with
      | Some run -> run ()
      | None -> failwith ("unknown test: " ^ name))
  | _ -> failwith "usage: test_main <name>"
