let () =
  let source = In_channel.with_open_bin Sys.argv.(1) In_channel.input_all in
  Printf.printf "let source = %S\n" source
