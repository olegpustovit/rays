(* An animated chain: grid > attribute_noise (offset driven by frame/input's t) > transform, 1M
   points (GRID_SIDE), cooked frame after frame as the editor does (Value_lane.resolve, then one
   session cook). DRIVE and XDRIVE replace the noise's and add to the transform's arguments; FRAMES (12);
   CONNECTIVITY (Points) the grid's; TEXT replaces the whole workspace; CPU=1 lowers without the
   device; QUALIFICATION=1 the fixed device rule instead of the measured policy; READERS (0) the
   root's known CPU readers, as the editor's display declares; NOREAD=1 leaves the result unread.
   Prints per frame: seconds (and the resolve's), dispatches, buffers created, uploads, pipelines
   compiled, whether P is resident, the seconds and bytes read back of P's bounds (the selected box
   and framing, Geometry_view.bounds), the seconds reading P, and noise[1] (a read back), then each
   placement with its reason and seconds, the median cook of the second half last; NODES=1 also each
   node's own seconds in the session. *)
let ok = function Ok value -> value | Error d -> failwith (Diagnostic.to_string d)
let side = try int_of_string (Sys.getenv "GRID_SIDE") with Not_found -> 1000

let drive =
  Option.value (Sys.getenv_opt "DRIVE") ~default:":offset [(get (frame/input) :t) 0.0 0.0]"

let text =
  Printf.sprintf
    "(workspace chain (graph g :context sop (-> (sop/grid :counts \"Point counts\" :connectivity \
     %S :columns %d :rows %d) (sop/attribute_noise :name \"noise\" :seed 7 %s) (sop/transform \
     :translate [0.0 1.0 0.0] %s))))"
    (Option.value (Sys.getenv_opt "CONNECTIVITY") ~default:"Points")
    side side drive
    (Option.value (Sys.getenv_opt "XDRIVE") ~default:"")
  |> fun text -> Option.value (Sys.getenv_opt "TEXT") ~default:text

(* TIMEOUT=seconds: past it, say what left the device so far and stop, rather than hang *)
let () =
  Option.iter
    (fun seconds ->
      Sys.set_signal Sys.sigalrm
        (Sys.Signal_handle
           (fun _ ->
             List.iter
               (fun (site, reason, count) -> Printf.printf "off_gpu %s: %s (%d)\n" site reason count)
               (Flow_ir.Fallback.seen ());
             Printf.printf "timeout after %s s\n%!" seconds;
             exit 2));
      ignore (Unix.alarm (int_of_string seconds)))
    (Sys.getenv_opt "TIMEOUT")

let () =
  let gpu = Flow_native.Workspace_gpu.create () in
  if Sys.getenv_opt "QUALIFICATION" <> None then Flow_native.Workspace_gpu.qualification gpu;
  let readers = try int_of_string (Sys.getenv "READERS") with Not_found -> 0 in
  let noread = Sys.getenv_opt "NOREAD" <> None in
  let times = ref [] in
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
          let lowered =
            (* DEFER=1: lowered as the editor lowers it *)
            Flow_sop.Lower.workspace ~defer:(Sys.getenv_opt "DEFER" <> None) ~extra:Editor_document.Kinds.descriptors
              ?gpu:(if Sys.getenv_opt "CPU" = None then Some config else None)
              (Flow.Syntax.parse text |> ok)
            |> ok
          in
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
            (* a frame as the editor gives it: a step and an index, so state folds advance *)
            let live = { (Input.at_time time) with dt = 1. /. 60.; frame; tick = frame } in
            let resolved =
              Flow_sop.Value_lane.resolve
                ?gpu:(if Sys.getenv_opt "CPU" = None then Some config else None)
                ~live lane ~time graph.network
              |> ok
            in
            let resolve_seconds = Unix.gettimeofday () -. start in
            let node =
              Sop.Edit_graph.compile_node resolved.geometry ~node_id:(Option.get graph.root)
              |> Result.get_ok
            in
            let output =
              Sop.Session.cook ~cpu_consumers:readers session
                ~context:(Sop.Context.create ~frame:(Int64.of_int frame) ~time ~input:live () |> Result.get_ok)
                node
              |> ok
            in
            let geometry = Result.get_ok (Sop.Payload.geometry output.payload) in
            let seconds = Unix.gettimeofday () -. start in
            times := seconds :: !times;
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
            (* each route measures what it claims *)
            if Sys.getenv_opt "QUALIFICATION" <> None && Sys.getenv_opt "CPU" = None && not resident then
              failwith "the Qualification route left the device";
            if Sys.getenv_opt "CPU" <> None && resident then failwith "the CPU route reached the device";
            let start = Unix.gettimeofday () in
            if not noread then ignore (Rdk_rays.Geometry_view.bounds geometry);
            let bounds_seconds = Unix.gettimeofday () -. start in
            let bounded = stats () in
            let start = Unix.gettimeofday () in
            if not noread then ignore (Rdk.Packed.Float3.get (Rdk.Geometry.positions geometry) 0);
            let read_seconds = Unix.gettimeofday () -. start in
            (* P[0], so a frame that did not move shows *)
            if not noread then begin
              let ps = Rdk.Geometry.positions geometry in
              let x, y, z = Rdk.Packed.Float3.get ps 0 in
              (* PSUM=1: a checksum of every point (reads them all back) *)
              let sum = ref 0. in
              if Sys.getenv_opt "PSUM" <> None then
              for k = 0 to Rdk.Packed.Float3.length ps - 1 do
                let a, b, c = Rdk.Packed.Float3.get ps k in
                sum := !sum +. a +. (2. *. b) +. (3. *. c)
              done;
              Printf.printf "  P[0] %.6f %.6f %.6f sum %.4f\n" x y z !sum

            end;
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
              | _ when noread -> nan
              | Some a -> (
                  match Rdk.Attribute.View.storage a with
                  | Rdk.Attribute.Float v -> v.(1)
                  | _ -> nan)
              | None -> nan);
            List.iter
              (fun (p : Flow_ir.placement) ->
                let seconds = Option.fold ~none:"unknown" ~some:(Printf.sprintf "%.6f") in
                Printf.printf "  placement %s count=%s reason=%s cpu_s=%s device_s=%s readers=%s\n%!"
                  (String.concat "/" (fst p.owner))
                  (Option.fold ~none:"unknown" ~some:string_of_int p.count)
                  (Flow_ir.Cost.reason_name p.reason) (seconds p.cpu_seconds) (seconds p.device_seconds)
                  (Option.fold ~none:"unknown" ~some:string_of_int p.cpu_consumers))
              (Flow_ir.Profile.placements lowered.profile)
          done;
          let late = List.filteri (fun i _ -> i < max 1 (List.length !times / 2)) !times in
          (let s = Gc.quick_stat () in
           Printf.printf "gc: heap %d MB, major cycles %d, minor %d\n%!" (s.heap_words / 131072)
             s.major_collections s.minor_collections;
           let s = Gc.stat () in
           Printf.printf "gc: live %d MB\n%!" (s.live_words / 131072));
          if Sys.getenv_opt "ROOTS" <> None then
            List.iter (fun (name, v) -> Printf.printf "  reach %s %d MB\n%!" name (Obj.reachable_words v / 131072))
              [ ("lowered", Obj.repr lowered); ("lane", Obj.repr lane); ("session", Obj.repr session) ];
          Printf.printf "median_cook_s=%.6f over last %d frames\n%!"
            (List.nth (List.sort Float.compare late) (List.length late / 2))
            (List.length late);
          List.iter
            (fun (site, reason, count) -> Printf.printf "off_gpu %s: %s (%d)\n%!" site reason count)
            (Flow_ir.Fallback.seen ());
          Sop.Session.close session))
