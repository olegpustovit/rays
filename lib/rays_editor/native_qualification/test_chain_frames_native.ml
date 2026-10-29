(* On the real device: the animated chain of bench_chain_frames as assertions. grid (GRID_SIDE
   squared Points; 512 in @runtest-native) > attribute_noise with its offset driven by frame/input's
   t > transform, cooked frame after frame as the editor does (Value_lane.resolve, one session
   cook). The frame-driven offset is the fused write's data, a frame input of its one region: after
   the first frame no pipeline is compiled and no source uploaded, each frame is one dispatch whose
   P stays on the device and nothing is read back while it cooks; its noise and P agree with the CPU
   cook of the same frame within float32 tolerance. Each frame is then displayed as the editor
   does (Surface.of_geometry, Surface.scene3 with Workspace_gpu.display, a Scene3 points draw) with
   P bound where the device holds it: nothing is read back, and its pixels equal the host display
   of the same geometry read back. The median frame on the CPU and the GPU, and the median device
   display (lowering and drawing a 256 x 256 canvas) are printed. *)
let ok = function Ok value -> value | Error d -> failwith (Diagnostic.to_string d)
let side = try int_of_string (Sys.getenv "GRID_SIDE") with Not_found -> 1000
let frames = 8

let text =
  Printf.sprintf
    "(workspace chain (graph g :context sop (-> (sop/grid :counts \"Point counts\" :connectivity \
     \"Points\" :columns %d :rows %d) (sop/attribute_noise :name \"noise\" :seed 7 :offset [(get \
     (frame/input) :t) 0.0 0.0]) (sop/transform :translate [0.0 1.0 0.0]))))"
    side side

(* [cook frame] cooks the chain at that frame, as the editor does *)
let animate ?gpu () =
  let lowered = Flow_sop.Lower.workspace ?gpu (Flow.Syntax.parse text |> ok) |> ok in
  let graph = List.hd lowered.graphs in
  let lane = Flow_sop.Value_lane.create () in
  let session = Sop.Session.create ~max_entries:64 ~max_payload_bytes:(1 lsl 30) |> Result.get_ok in
  let cook frame =
    let time = float frame /. 60. in
    let live = Input.at_time time in
    let start = Unix.gettimeofday () in
    let resolved = Flow_sop.Value_lane.resolve ?gpu ~live lane ~time graph.network |> ok in
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
    (Result.get_ok (Sop.Payload.geometry output.payload), Unix.gettimeofday () -. start)
  in
  (cook, fun () -> Sop.Session.close session)

let noise geometry =
  match Rdk.Geometry.find_attribute ~owner:Point "noise" geometry with
  | Some attribute -> (
      match Rdk.Attribute.View.storage attribute with
      | Rdk.Attribute.Float data -> data
      | _ -> failwith "noise is not float storage")
  | None -> failwith "no noise attribute"

let median times = List.nth (List.sort compare times) (List.length times / 2)

let camera =
  Rays.Camera.perspective
    ~at:(Rays_math.Vec3.create 0.4 2.2 1.3)
    ~target:(Rays_math.Vec3.create 0. 1. 0.) ()

(* the picture of a display, as the editor draws a cooked object, and the seconds it took to
   prepare and draw (not to read the picture) *)
let picture canvas ?device geometry =
  let start = Unix.gettimeofday () in
  let surface = Rdk_rays.Surface.of_geometry geometry |> Result.get_ok in
  let scene = Rdk_rays.Surface.scene3 ?device surface in
  (match
     Rays.Canvas.render canvas Rays.Scene.[ clear Rays_math.Color.black; view3d ~camera scene ]
   with
  | Ok () -> ()
  | Error message -> failwith message);
  let seconds = Unix.gettimeofday () -. start in
  (Rays.Canvas.pixels canvas |> Result.get_ok, seconds)

let () =
  let count = side * side in
  let cpu_cook, cpu_close = animate () in
  let gpu = Flow_native.Workspace_gpu.create () in
  Fun.protect
    ~finally:(fun () ->
      cpu_close ();
      Flow_native.Workspace_gpu.close gpu)
    (fun () ->
      Flow_native.Workspace_gpu.with_backend ~fast:true gpu (fun config ->
          let dispatches = ref 0 in
          let backend = Option.get config.Flow_ir.Gpu.backend in
          let counted : Flow_ir.Gpu.config =
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
          let stats () =
            let host, _, _, _, _ = Flow_native.Workspace_gpu.image_stats gpu in
            host
          in
          let gpu_cook, gpu_close = animate ~gpu:counted () in
          Fun.protect ~finally:gpu_close (fun () ->
              let worst = ref 0. and cpu_times = ref [] and gpu_times = ref [] in
              let canvas = Rays.Canvas.create_exn ~width:256 ~height:256
              and host_canvas = Rays.Canvas.create_exn ~width:256 ~height:256
              and display_times = ref []
              and host_times = ref [] in
              for frame = 0 to frames - 1 do
                let before = stats () and calls = !dispatches in
                let device, seconds = gpu_cook frame in
                let after = stats () in
                let fail what = failwith (Printf.sprintf "frame %d: %s" frame what) in
                if frame > 0 then begin
                  gpu_times := seconds :: !gpu_times;
                  if after.pipeline_compilations <> before.pipeline_compilations then
                    fail "a pipeline was compiled";
                  if after.input_uploads <> before.input_uploads then
                    fail
                      (Printf.sprintf "%d sources were uploaded"
                         (after.input_uploads - before.input_uploads));
                  if !dispatches - calls <> 1 then
                    fail (Printf.sprintf "%d dispatches, not one" (!dispatches - calls))
                end;
                if
                  after.readback_bytes <> before.readback_bytes
                  || after.materialisations <> before.materialisations
                then fail "the cook read its outputs back";
                if not (Rdk.Packed.Float3.resident (Rdk.Geometry.positions device)) then
                  fail "P is not device-resident";
                (* the display binds P where it is *)
                let bound = Flow_native.Workspace_gpu.displays gpu in
                let shown, seconds =
                  picture canvas ~device:(Flow_native.Workspace_gpu.display gpu) device
                in
                if frame > 0 then display_times := seconds :: !display_times;
                let drawn = stats () in
                if Flow_native.Workspace_gpu.displays gpu <> bound + 1 then
                  fail "the display did not bind P on the device";
                if
                  drawn.readback_bytes <> after.readback_bytes
                  || drawn.materialisations <> after.materialisations
                then fail "the display read P back";
                if not (Rdk.Packed.Float3.resident (Rdk.Geometry.positions device)) then
                  fail "the display materialised P";
                (* the same geometry read back and drawn from the host: the same pixels *)
                let host, seconds = picture host_canvas device in
                if frame > 0 then host_times := seconds :: !host_times;
                if host <> shown then fail "the device display differs from the host display";
                if Array.for_all (fun c -> c = Rays_math.Color.black) shown then
                  fail "the display drew nothing";
                let cpu, cpu_seconds = cpu_cook frame in
                if frame > 0 then cpu_times := cpu_seconds :: !cpu_times;
                let close a b = worst := Float.max !worst (Float.abs (a -. b)) in
                let p = Rdk.Geometry.positions cpu and q = Rdk.Geometry.positions device in
                let n = noise cpu and m = noise device in
                for i = 0 to count - 1 do
                  let x, y, z = Rdk.Packed.Float3.get p i
                  and gx, gy, gz = Rdk.Packed.Float3.get q i in
                  close x gx;
                  close y gy;
                  close z gz;
                  close n.(i) m.(i)
                done
              done;
              (* float32 rounding of positions within [-0.5, 1.5] and of noise in [0, 1] *)
              if !worst > 1e-5 then failwith (Printf.sprintf "GPU differs from CPU by %.3e" !worst);
              ignore (Rays.Canvas.destroy canvas);
              ignore (Rays.Canvas.destroy host_canvas);
              Printf.printf
                "device display of %d resident points, nothing read back, the host display's \
                 pixels: %.4f s a frame against %.4f s reading P back (medians of %d)\n\
                 %!"
                count (median !display_times) (median !host_times) (frames - 1);
              Printf.printf
                "animated grid > attribute_noise (offset t) > transform, %d points: frame CPU %.4f \
                 s, GPU %.4f s (medians of %d), one dispatch, nothing compiled, uploaded or read \
                 back, largest difference %.2e\n\
                 %!"
                count (median !cpu_times) (median !gpu_times) (frames - 1) !worst)))
