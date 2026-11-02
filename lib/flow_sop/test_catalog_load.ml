(* Loading the node library does not parse its text: every process that loads the catalog pays this.
   Parsing allocated 150 MB on the minor heap and promoted 100 MB of it. *)
let () =
  let minor = Gc.minor_words () in
  let _, promoted, _ = Gc.counters () in
  let sources =
    match Flow_sop.Catalog.sources () with
    | Ok sources -> sources
    | Error diagnostic -> failwith (Diagnostic.to_string diagnostic)
  in
  let _, promoted', _ = Gc.counters () in
  let minor_bytes = (Gc.minor_words () -. minor) *. 8. in
  let promoted_bytes = (promoted' -. promoted) *. 8. in
  if minor_bytes > 4e6 || promoted_bytes > 4e6 then
    failwith
      (Printf.sprintf "loading the library allocated %.0f bytes minor, promoted %.0f" minor_bytes
         promoted_bytes);
  assert (sources <> [])
