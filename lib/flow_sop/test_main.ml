let tests =
  [
    ("test_projection", Test_projection.run);
    ("test_workspace_edit", Test_workspace_edit.run);
    ("test_workspace_cook", Test_workspace_cook.run);
    ("test_workspace_zone", Test_workspace_zone.run);
  ]

let () =
  match Array.to_list Sys.argv with
  | _ :: name :: _ -> (
      match List.assoc_opt name tests with
      | Some run -> run ()
      | None -> failwith ("unknown test: " ^ name))
  | _ -> failwith "usage: test_main <name>"
