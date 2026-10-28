(* One session toggling an edit of grid > attribute_noise > transform (1M points, GRID_SIDE): the
   median recook when results die at once, and when the caller keeps the last 16 alive. Prints one
   line per scenario: median, min, buffers created and outputs read back over the measured cooks,
   buffers held after; then the major collections and heap size. *)
let ok = function Ok value -> value | Error d -> failwith (Diagnostic.to_string d)
let side = try int_of_string (Sys.getenv "GRID_SIDE") with Not_found -> 1000
let cooks = try int_of_string (Sys.getenv "COOKS") with Not_found -> 32

let edit gpu y =
  let text =
    Printf.sprintf
      "(workspace chain (graph g :context sop (-> (sop/grid :counts \"Point counts\" :connectivity \
       \"Points\" :columns %d :rows %d) (sop/attribute_noise :name \"noise\" :seed 7) \
       (sop/transform :translate [0.0 %g 0.0]))))"
      side side y
  in
  let lowered = Flow_sop.Lower.workspace ~gpu (Flow.Syntax.parse text |> ok) |> ok in
  let graph = List.hd lowered.graphs in
  Sop.Edit_graph.compile_node graph.network.geometry ~node_id:(Option.get graph.root)
  |> Result.get_ok

let () =
  let gpu = Flow_native.Workspace_gpu.create () in
  Fun.protect
    ~finally:(fun () -> Flow_native.Workspace_gpu.close gpu)
    (fun () ->
      Flow_native.Workspace_gpu.with_backend ~fast:true gpu (fun config ->
          let stats () =
            let host, _, _, _, _ = Flow_native.Workspace_gpu.image_stats gpu in
            host
          in
          let run name keep =
            let session =
              Sop.Session.create ~max_entries:2 ~max_payload_bytes:(1 lsl 30) |> Result.get_ok
            in
            Fun.protect
              ~finally:(fun () -> Sop.Session.close session)
              (fun () ->
                let a = edit config 1.0 and b = edit config 2.0 in
                let kept = ref [] in
                let cook i =
                  let start = Unix.gettimeofday () in
                  let output =
                    Sop.Session.cook session
                      ~context:(Sop.Context.create ~time:0. () |> Result.get_ok)
                      (if i mod 2 = 0 then a else b)
                    |> ok
                  in
                  let geometry = Result.get_ok (Sop.Payload.geometry output.payload) in
                  if keep then kept := List.take 16 (geometry :: !kept);
                  Unix.gettimeofday () -. start
                in
                for i = 0 to cooks - 1 do
                  ignore (cook i)
                done;
                let before = stats () in
                let majors = (Gc.quick_stat ()).major_collections in
                let times = List.init cooks (fun i -> cook i) |> List.sort compare in
                let after = stats () in
                Printf.printf
                  "%s: median %.4f s, min %.4f s, %d buffers created, %d read back, %d held\n%!"
                  name
                  (List.nth times (cooks / 2))
                  (List.hd times)
                  (after.buffer_creations - before.buffer_creations)
                  (after.materialisations - before.materialisations)
                  after.held_buffers;
                Printf.printf "  %d major collections, heap %d MB\n%!"
                  ((Gc.quick_stat ()).major_collections - majors)
                  ((Gc.quick_stat ()).heap_words / 131072);
                ignore (Sys.opaque_identity !kept))
          in
          run "results die" false;
          run "16 kept alive" true))
