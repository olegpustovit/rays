(* G.1 on the real device: a 1,000,000-point grid is lowered on the CPU, under the measured policy and
   with placement forced. Positions agree within float32 tolerance; the timings are printed. *)
let ok = function Ok value -> value | Error d -> failwith (Diagnostic.to_string d)
let side = try int_of_string (Sys.getenv "GRID_SIDE") with Not_found -> 1000
let text = Printf.sprintf "(workspace g (graph g :context sop (sop/grid :counts \"Point counts\" :connectivity \"Points\" :columns %d :rows %d :size 3.0 :rotation 0.3)))" side side
let positions run =
  let start = Unix.gettimeofday () in
  let lowered = run (fun gpu -> Flow_sop.Lower.workspace ~gpu ~factories:Sop.Nodes.factories
    (Flow.Syntax.parse text |> ok) |> ok) in
  let lowering = Unix.gettimeofday () -. start in
  let graph = List.hd lowered.Flow_sop.Lower.graphs in
  let node = Sop.Edit_graph.compile_node graph.network.geometry ~node_id:(Option.get graph.root) |> Result.get_ok in
  let session = Sop.Session.create ~max_entries:4 ~max_payload_bytes:(1 lsl 30) |> Result.get_ok in
  Fun.protect ~finally:(fun () -> Sop.Session.close session) (fun () ->
    match Sop.Session.cook session ~context:(Sop.Context.create ~domains:4 ~time:0. () |> Result.get_ok) node with
    | Ok output -> Rdk.Geometry.positions (Result.get_ok (Sop.Payload.geometry output.payload)), lowering
    | Error d -> failwith (Diagnostic.error_to_string d))
let () =
  let count = side * side in
  let cpu, cpu_seconds = positions (fun f -> f Flow_ir.Gpu.default) in
  let reads gpu = let host, _, _, _, _ = Flow_native.Workspace_gpu.image_stats gpu in host.readback_bytes in
  let run name force =
    let gpu = Flow_native.Workspace_gpu.create () in
    if force then Flow_native.Workspace_gpu.qualification gpu;
    Fun.protect ~finally:(fun () -> Flow_native.Workspace_gpu.close gpu) (fun () ->
      (* the first lowering compiles pipelines and measures; the second is steady state *)
      ignore (positions (fun f -> Flow_native.Workspace_gpu.with_backend gpu f));
      let before = reads gpu in
      let placed, seconds = positions (fun f -> Flow_native.Workspace_gpu.with_backend gpu f) in
      let bytes = reads gpu - before in
      for i = 0 to count - 1 do
        let (x, y, z) = Rdk.Packed.Float3.get cpu i and (gx, gy, gz) = Rdk.Packed.Float3.get placed i in
        let near a b = Float.abs (a -. b) <= 1e-4 *. (1. +. Float.abs a) in
        assert (near x gx && near y gy && near z gz)
      done;
      Printf.printf "%-9s lowering %.3f s (cpu %.3f s), readback %d bytes (%s)\n%!" name seconds cpu_seconds bytes
        (if bytes > 0 then "GPU" else "CPU")) in
  run "measured" false;
  run "forced" true
