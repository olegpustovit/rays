(* An animated chain: grid > attribute_noise (offset driven by frame/input's t) > transform, 1M
   points (GRID_SIDE), cooked frame after frame as the editor does (Value_lane.resolve, then one
   session cook). DRIVE and XDRIVE replace the noise's and add to the transform's arguments; FRAMES (12);
   CONNECTIVITY (Points) the grid's.
   Prints per frame: seconds (and the resolve's), dispatches, buffers created, uploads, pipelines
   compiled, whether P is resident, the seconds and bytes read back of P's bounds (the selected box
   and framing, Geometry_view.bounds), the seconds reading P, and noise[1] (a read back); NODES=1
   also each node's own seconds in the session. *)
let ok = function Ok value -> value | Error d -> failwith (Diagnostic.to_string d)
let side = try int_of_string (Sys.getenv "GRID_SIDE") with Not_found -> 1000

let drive =
  Option.value (Sys.getenv_opt "DRIVE") ~default:":offset [(get (frame/input) :t) 0.0 0.0]"

let text =
  Printf.sprintf
    "(workspace chain (graph g :context sop (-> (sop/grid :counts \"Point counts\" :connectivity \
     %S :columns %d :rows %d) (sop/attribute_noise :name \"noise\" :seed 7 %s) \
     (sop/transform :translate [0.0 1.0 0.0] %s))))"
    (Option.value (Sys.getenv_opt "CONNECTIVITY") ~default:"Points")
    side side drive
    (Option.value (Sys.getenv_opt "XDRIVE") ~default:"")

let () =
  let gpu = Flow_native.Workspace_gpu.create () in
  Fun.protect
    ~finally:(fun () -> Flow_native.Workspace_gpu.close gpu)
    (fun () ->
      Flow_native.Workspace_gpu.with_backend ~fast:true gpu (fun config ->
          let dispatches = ref 0 in
          let backend = Option.get config.Flow_ir.Gpu.backend in
          let config =
            {
              config with
              backend =
                Some
                  {
                    backend with
                    prepare =
                      (fun ~fast packed ->
                        Result.map
                          (fun (kernel : Flow_ir.Gpu.kernel) ->
                            {
                              kernel with
                              run =
                                (fun inputs ->
                                  incr dispatches;
                                  kernel.run inputs);
                            })
                          (backend.prepare ~fast packed));
                  };
            }
          in
          let lowered = Flow_sop.Lower.workspace ~gpu:config (Flow.Syntax.parse text |> ok) |> ok in
          let graph = List.hd lowered.graphs in
          let lane = Flow_sop.Value_lane.create () in
          let session =
            Sop.Session.create ~max_entries:64 ~max_payload_bytes:(1 lsl 30) |> Result.get_ok
          in
          let stats () =
            let host, _, _, _, _ = Flow_native.Workspace_gpu.image_stats gpu in
            host
          in
          for frame = 0 to (try int_of_string (Sys.getenv "FRAMES") with Not_found -> 12) - 1 do
            let time = float frame /. 60. in
            let before = stats () and calls = !dispatches in
            let start = Unix.gettimeofday () in
            let live = Input.at_time time in
            let resolved =
              Flow_sop.Value_lane.resolve ~gpu:config ~live lane ~time graph.network |> ok
            in
            let resolve_seconds = Unix.gettimeofday () -. start in
            let node =
              Sop.Edit_graph.compile_node resolved.geometry ~node_id:(Option.get graph.root)
              |> Result.get_ok
            in
            let output =
              Sop.Session.cook session
                ~context:(Sop.Context.create ~time ~input:live () |> Result.get_ok)
                node
              |> ok
            in
            let geometry = Result.get_ok (Sop.Payload.geometry output.payload) in
            let seconds = Unix.gettimeofday () -. start in
            let after = stats () in
            (if Sys.getenv_opt "NODES" <> None then
               let rec walk n =
                 Option.iter
                   (fun s ->
                     Printf.printf "  node %d %s %s: %.4f\n" (Sop.Node.id n) (Sop.Node.label n)
                       (Sop.Node.operation n) s)
                   (Sop.Session.node_seconds session (Sop.Node.id n));
                 List.iter walk (Sop.Node.inputs n)
               in
               walk node);
            let resident = Rdk.Packed.Float3.resident (Rdk.Geometry.positions geometry) in
            let start = Unix.gettimeofday () in
            ignore (Rdk_rays.Geometry_view.bounds geometry);
            let bounds_seconds = Unix.gettimeofday () -. start in
            let bounded = stats () in
            let start = Unix.gettimeofday () in
            ignore (Rdk.Packed.Float3.get (Rdk.Geometry.positions geometry) 0);
            let read_seconds = Unix.gettimeofday () -. start in
            Printf.printf
              "frame %2d: %.4f s (resolve %.4f s), %d dispatches, %d buffers, %d uploads (%d \
               bytes), %d compiled, P %s, bounds %.4f s (%d bytes, %d outputs read), read P %.4f \
               s, noise[1] %.4f\n\
               %!"
              frame seconds resolve_seconds (!dispatches - calls)
              (after.buffer_creations - before.buffer_creations)
              (after.input_uploads - before.input_uploads)
              (after.input_uploaded_bytes - before.input_uploaded_bytes)
              (after.pipeline_compilations - before.pipeline_compilations)
              (if resident then "resident" else "host")
              bounds_seconds
              (bounded.readback_bytes - after.readback_bytes)
              (bounded.materialisations - after.materialisations)
              read_seconds
              (match Rdk.Geometry.find_attribute ~owner:Point "noise" geometry with
              | Some a -> (
                  match Rdk.Attribute.View.storage a with
                  | Rdk.Attribute.Float v -> v.(1)
                  | _ -> nan)
              | None -> nan)
          done;
          Sop.Session.close session))
