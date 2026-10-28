(* An animated chain: grid > attribute_noise (offset driven by frame/input's t) > transform, 1M
   points (GRID_SIDE), cooked frame after frame as the editor does (Value_lane.resolve, then one
   session cook). Prints per frame: seconds, dispatches, uploads, pipelines compiled, P resident. *)
let ok = function Ok value -> value | Error d -> failwith (Diagnostic.to_string d)
let side = try int_of_string (Sys.getenv "GRID_SIDE") with Not_found -> 1000

let text =
  Printf.sprintf
    "(workspace chain (graph g :context sop (-> (sop/grid :counts \"Point counts\" :connectivity \
     \"Points\" :columns %d :rows %d) (sop/attribute_noise :name \"noise\" :seed 7 :offset [(get \
     (frame/input) :t) 0.0 0.0]) (sop/transform :translate [0.0 1.0 0.0]))))"
    side side

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
          Flow_sop.Port.Map.iter
            (fun (port : Flow_sop.Port.t) _ ->
              Printf.printf "drive node %d %s (%s)\n" port.node port.path
                (Option.value ~default:"?"
                   (Sop.Edit_graph.node_factory_key graph.network.geometry ~node_id:port.node)))
            graph.network.drives;
          List.iter
            (fun (info : Sop.Edit_graph.node_info) -> Printf.printf "node %d %s %s\n" info.id info.operation (String.sub info.parameters 0 (min 300 (String.length info.parameters))))
            (Sop.Edit_graph.inspect graph.network.geometry);
          let lane = Flow_sop.Value_lane.create () in
          let session =
            Sop.Session.create ~max_entries:64 ~max_payload_bytes:(1 lsl 30) |> Result.get_ok
          in
          let stats () =
            let host, _, _, _, _ = Flow_native.Workspace_gpu.image_stats gpu in
            host
          in
          for frame = 0 to 11 do
            let time = float frame /. 60. in
            let before = stats () and calls = !dispatches in
            let start = Unix.gettimeofday () in
            let live = Input.at_time time in
            let resolved =
              Flow_sop.Value_lane.resolve ~gpu:config ~live lane ~time graph.network |> ok
            in
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
            Printf.printf
              "frame %2d: %.4f s, %d dispatches, %d uploads (%d bytes), %d compiled, P %s, \
               noise[1] %.4f\n\
               %!"
              frame seconds (!dispatches - calls)
              (after.input_uploads - before.input_uploads)
              (after.input_uploaded_bytes - before.input_uploaded_bytes)
              (after.pipeline_compilations - before.pipeline_compilations)
              (if Rdk.Packed.Float3.resident (Rdk.Geometry.positions geometry) then "resident"
               else "host")
              (match Rdk.Geometry.find_attribute ~owner:Point "noise" geometry with
              | Some a -> (
                  match Rdk.Attribute.View.storage a with
                  | Rdk.Attribute.Float v -> v.(1)
                  | _ -> nan)
              | None -> nan)
          done;
          Sop.Session.close session))
