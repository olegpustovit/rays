let tests =
  [
    ("test_runtime_visibility", Test_runtime_visibility.run);
    ("test_runtime_input", Test_runtime_input.run);
  ]

let () =
  match Array.to_list Sys.argv with
  | [ _; name ] -> (
      match List.assoc_opt name tests with
      | Some run -> run ()
      | None -> failwith ("unknown test: " ^ name))
  | _ -> failwith "usage: test_main <name>"
