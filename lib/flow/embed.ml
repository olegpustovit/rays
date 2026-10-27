(* Embeds files as a sorted OCaml string table, so installed executables need no source tree.
   Usage: embed.exe FILE... *)
let () =
  let files = List.sort compare (List.tl (Array.to_list Sys.argv)) in
  print_string "let files = [\n";
  List.iter (fun f -> Printf.printf "  %S, %S;\n" (Filename.basename f) (In_channel.with_open_bin f In_channel.input_all)) files;
  print_string "]\n"
