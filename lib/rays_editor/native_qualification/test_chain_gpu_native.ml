(* On the real device: the fused four-deformer chain over a 1,000,000-point grid is cooked on the CPU,
   then from a worker domain (as Async_cook does) with the interface domain serving the device. The
   positions agree within float32 tolerance; the cook times are printed. *)
let ok = function Ok value -> value | Error d -> failwith (Diagnostic.to_string d)
let side = try int_of_string (Sys.getenv "GRID_SIDE") with Not_found -> 1000
let stages = [ (0.3, 0.7, 0); (-0.2, 1.3, 3); (0.15, 2.1, 5); (0.05, 0.4, 7) ]

let text =
  Printf.sprintf
    "(workspace fused (graph g :context sop (let* [g (sop/grid :counts \"Point counts\" \
     :connectivity \"Points\" :columns %d :rows %d) %s] d4)))"
    side side
    (String.concat " "
       (List.mapi
          (fun i (amplitude, frequency, seed) ->
            Printf.sprintf "d%d (sop/noise_displace %s :amplitude %g :frequency %g :seed %d)" (i + 1)
              (if i = 0 then "g" else Printf.sprintf "d%d" i)
              amplitude frequency seed)
          stages))

let cook node =
  let session = Sop.Session.create ~max_entries:4 ~max_payload_bytes:(1 lsl 30) |> Result.get_ok in
  Fun.protect
    ~finally:(fun () -> Sop.Session.close session)
    (fun () ->
      let start = Unix.gettimeofday () in
      match
        Sop.Session.cook session
          ~context:(Sop.Context.create ~domains:4 ~time:0. () |> Result.get_ok)
          node
      with
      | Ok output ->
          ( Rdk.Geometry.positions (Result.get_ok (Sop.Payload.geometry output.payload)),
            Unix.gettimeofday () -. start )
      | Error d -> failwith (Diagnostic.error_to_string d))

let () =
  let lowered = Flow_sop.Lower.workspace (Flow.Syntax.parse text |> ok) |> ok in
  let graph = List.hd lowered.graphs in
  let node =
    Sop.Edit_graph.compile_node graph.network.geometry ~node_id:(Option.get graph.root)
    |> Result.get_ok
  in
  let count = side * side in
  let median f =
    let runs = List.init 5 (fun _ -> f ()) in
    (List.nth (List.sort compare (List.map snd runs)) 2, fst (List.hd runs))
  in
  let cpu_seconds, cpu = median (fun () -> cook node) in
  let base_text =
    Printf.sprintf
      "(workspace b (graph g :context sop (sop/grid :counts \"Point counts\" :connectivity \
       \"Points\" :columns %d :rows %d)))"
      side side
  in
  let base = Flow_sop.Lower.workspace (Flow.Syntax.parse base_text |> ok) |> ok in
  let base_graph = List.hd base.graphs in
  let grid_seconds, _ =
    median (fun () ->
        cook
          (Sop.Edit_graph.compile_node base_graph.network.geometry
             ~node_id:(Option.get base_graph.root)
          |> Result.get_ok))
  in
  Printf.printf "grid alone cooks in %.3f s on the CPU\n%!" grid_seconds;
  let reads gpu =
    let host, _, _, _, _ = Flow_native.Workspace_gpu.image_stats gpu in
    host.readback_bytes
  in
  let run name force =
    let gpu = Flow_native.Workspace_gpu.create () in
    if force then Flow_native.Workspace_gpu.qualification gpu;
    Fun.protect
      ~finally:(fun () -> Flow_native.Workspace_gpu.close gpu)
      (fun () ->
        let worker () =
          Flow_native.Workspace_gpu.with_backend gpu (fun _gpu ->
              let finished = Atomic.make false in
              let domain =
                Domain.spawn (fun () ->
                    Fun.protect
                      ~finally:(fun () ->
                        Atomic.set finished true;
                        Parallel.release_current_domain_pools ())
                      (fun () -> cook node))
              in
              while not (Atomic.get finished) do
                Flow_native.Workspace_gpu.pump gpu;
                Domain.cpu_relax ()
              done;
              Domain.join domain)
        in
        ignore (worker ());
        let before = reads gpu in
        let seconds, placed = median worker in
        let bytes = reads gpu - before in
        let worst = ref 0. in
        for i = 0 to count - 1 do
          let x, y, z = Rdk.Packed.Float3.get cpu i
          and gx, gy, gz = Rdk.Packed.Float3.get placed i in
          List.iter
            (fun (a, b) -> worst := Float.max !worst (Float.abs (a -. b)))
            [ (x, gx); (y, gy); (z, gz) ]
        done;
        assert (!worst <= 1e-3);
        Printf.printf
          "%-9s cook %.3f s (cpu %.3f s), readback %d bytes (%s), largest difference %.2e\n%!" name
          seconds cpu_seconds bytes
          (if bytes > 0 then "GPU" else "CPU")
          !worst)
  in
  run "measured" false;
  run "forced" true
