let () =
  match Array.to_list Sys.argv with
  | [ _; "dependency_gate" ] -> Dependency_gate.run ()
  | _ -> failwith "usage: test_main dependency_gate"
