(* One file: [let source = ...]. Several: a value per file, named from the file (ui.vert.spv: ui_vert). *)
let read file = In_channel.with_open_bin file In_channel.input_all

let () =
  match Array.to_list Sys.argv with
  | [ _; file ] -> Printf.printf "let source = %S\n" (read file)
  | _ :: files ->
      List.iter
        (fun file ->
          let name = Filename.chop_suffix (Filename.basename file) ".spv" in
          let name = String.map (function '.' -> '_' | c -> c) name in
          Printf.printf "let %s = %S\n" name (read file))
        files
  | [] -> ()
