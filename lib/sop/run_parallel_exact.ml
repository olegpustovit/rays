let () =
  match Array.to_list Sys.argv with
  | [ _ ] -> Test_parallel_exact.run ()
  | [ _; section ] -> Test_parallel_exact.run ~section ()
  | _ -> invalid_arg "run_parallel_exact [section]"
