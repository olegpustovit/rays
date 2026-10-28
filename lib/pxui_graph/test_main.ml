let tests =
  [ ("test_pxui_graph", Test_pxui_graph.run); ("bench_scope_big", Test_pxui_graph.bench_scope_big) ]

let () =
  (* pointer positions are the 11-point kit's, not its 13-point default *)
  match Array.to_list Sys.argv with
  | _ :: name :: _ -> (
      match List.assoc_opt name tests with
      | Some run -> run ()
      | None -> failwith ("unknown test: " ^ name))
  | _ -> failwith "usage: test_main <name>"
