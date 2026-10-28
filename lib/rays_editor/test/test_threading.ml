(* Printing a workspace threads chains with [->]; reading the print gives back the same forms.
   Every checked-in workspace (sketches, gallery, cases) prints, re-reads to the same flat forms
   and prints the same again. *)
let fail message = failwith ("test_threading: " ^ message)

let rec files dir =
  Array.fold_left
    (fun acc name ->
      let path = Filename.concat dir name in
      if Sys.is_directory path then acc @ files path
      else if Filename.check_suffix name ".rays" || Filename.check_suffix name ".lisp" then
        acc @ [ path ]
      else acc)
    [] (Sys.readdir dir)

let () =
  let threaded = ref 0 in
  let paths = List.concat_map files (List.tl (Array.to_list Sys.argv)) in
  List.iter
    (fun path ->
      begin
        let text = In_channel.with_open_bin path In_channel.input_all in
        let forms =
          match Flow.Syntax.parse text with
          | Ok f -> f
          | Error d -> fail (path ^ ": " ^ Diagnostic.to_string d)
        in
        let printed, _ = Flow.Lisp.print forms in
        let again =
          match Flow.Syntax.parse printed with
          | Ok f -> f
          | Error d -> fail (path ^ " reprint: " ^ Diagnostic.to_string d)
        in
        if List.map Flow.Lisp.flat forms <> List.map Flow.Lisp.flat again then
          fail (path ^ ": the print reads differently");
        if fst (Flow.Lisp.print again) <> printed then fail (path ^ ": the print is not stable");
        let n = String.length printed in
        let rec count at acc =
          if at + 3 > n then acc
          else count (at + 1) (if String.sub printed at 3 = "(->" then acc + 1 else acc)
        in
        threaded := !threaded + count 0 0
      end)
    paths;
  if !threaded = 0 then fail "no checked-in file threads a chain (shattered_studio shows it)";
  Printf.printf "threading: %d files print and re-read unchanged, %d chains threaded\n"
    (List.length paths) !threaded
