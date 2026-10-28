(* One executable for the rdk microbenches: bench_rdk.exe <op>; tunables are RAYS_* variables. *)
let benches = [
  "circle_from_edges", Bench_circle_from_edges.run; "convex_hull", Bench_convex_hull.run;
  "delaunay2", Bench_delaunay2.run; "extract_centroid", Bench_extract_centroid.run;
  "extract_point_curve", Bench_extract_point_curve.run; "graph_color", Bench_graph_color.run;
  "laplacian", Bench_laplacian.run; "predicates", Bench_predicates.run ]

let () =
  match Array.to_list Sys.argv with
  | [_; name] when List.mem_assoc name benches -> (List.assoc name benches) ()
  | _ -> prerr_endline ("usage: bench_rdk <" ^ String.concat "|" (List.map fst benches) ^ ">"); exit 2
