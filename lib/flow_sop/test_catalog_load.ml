(* A process that loads the catalog and checks a small workspace holds the library's index, not its
   forms: 129 node files parsed or unmarshalled whole were 36 MB live in every editor. *)
let () =
  let catalog =
    match Flow_sop.Catalog.of_factories ~version:1 [] with
    | Ok catalog -> catalog
    | Error diagnostic -> failwith (Diagnostic.to_string diagnostic)
  in
  let forms =
    match Flow.Syntax.parse "(workspace w (graph g :context draw (draw/background \"#000000\")))" with
    | Ok forms -> forms
    | Error diagnostic -> failwith (Diagnostic.to_string diagnostic)
  in
  (match Flow_sop.Catalog.check_workspace catalog forms with
  | Some _, _ -> ()
  | None, diagnostics ->
      failwith (String.concat "; " (List.map Diagnostic.to_string diagnostics)));
  Gc.full_major ();
  let live = (Gc.stat ()).live_words * 8 in
  if live > 12_000_000 then
    failwith (Printf.sprintf "a small workspace's check holds %d bytes live: the library's forms" live);
  ignore (Sys.opaque_identity catalog)
