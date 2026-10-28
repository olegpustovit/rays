(* One executable for the rdk microbenches: bench_rdk.exe <op> [args]; tunables are RAYS_* variables. *)
let benches =
  [
    ("convex_hull", Bench_convex_hull.run);
    ("predicates", Bench_predicates.run);
    ("topology", Bench_topology.run);
  ]

let () =
  match Array.to_list Sys.argv with
  | _ :: name :: _ when List.mem_assoc name benches -> (List.assoc name benches) ()
  | _ ->
      prerr_endline ("usage: bench_rdk <" ^ String.concat "|" (List.map fst benches) ^ ">");
      exit 2
