let tests =
  [
    ("test_ui_parity", Test_ui_parity.run);
    ("test_ui_commands", Test_ui_parity.commands);
    ("test_ui", Test_ui.run);
    ("test_input", Test_input.run);
    ("test_keyboard", Test_keyboard.run);
    ("test_table", Test_table.run);
  ]

let () =
  match Array.to_list Sys.argv with
  | [ _; name ] -> (
      match List.assoc_opt name tests with
      | Some run -> run ()
      | None -> failwith ("unknown test: " ^ name))
  | _ -> failwith "usage: test_main <name>"
