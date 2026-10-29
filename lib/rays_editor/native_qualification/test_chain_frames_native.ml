(* On the real device: the animated chain of bench_chain_frames as assertions. grid (GRID_SIDE
   squared Points, or CONNECTIVITY Triangles: a lit mesh with its point N; 512 in @runtest-native) >
   attribute_noise with its offset driven by frame/input's
   t > transform, cooked frame after frame as the editor does (Value_lane.resolve, one session
   cook). The frame-driven offset is the fused write's data, a frame input of its one region: after
   the first frame no pipeline is compiled and no source uploaded, each frame is one dispatch whose
   P stays on the device and nothing is read back while it cooks; its noise and P agree with the CPU
   cook of the same frame within float32 tolerance. Each frame is then displayed as the editor
   does (Surface.of_geometry, Surface.scene3 with Workspace_gpu.display, a Scene3 points or
   triangles draw) with P (and N) bound where the device holds them: nothing is read back, and its pixels equal the host display
   of the same geometry read back; its bounds are reduced on the device (24 KB read, P unread) and
   equal the host scan exactly. A lit mesh is also drawn under a World sun: the sun's fit reads
   the device mesh's bounds (24 KB), its host copy is never made (P stays unread), and its pixels
   equal the host display's under the same World. Removing the node (its scenes draw no display)
   releases every binding two Workspace_gpu.frames later, without a collection, and once its results
   die the pool holds no buffer. The median frame on the CPU and the GPU, and the median device
   display (lowering and drawing a 256 x 256 canvas) are printed. *)
let ok = function Ok value -> value | Error d -> failwith (Diagnostic.to_string d)
let side = try int_of_string (Sys.getenv "GRID_SIDE") with Not_found -> 1000
let frames = 8

(* DRIVE, XDRIVE and CONNECTIVITY give one case; without DRIVE the four @runtest-native cases run
   in one process, sharing the loaded catalog: the offset driven; lacunarity and roughness
   driven; the octave count driven (2 to 9 over the frames), uniforms of the one kernel; and a lit
   mesh whose transform shears and translates (identity at frame 0, then a live matrix) *)
let cases =
  match Sys.getenv_opt "DRIVE" with
  | Some drive ->
      [
        ( drive,
          Option.value (Sys.getenv_opt "XDRIVE") ~default:":translate [0.0 1.0 0.0]",
          Option.value (Sys.getenv_opt "CONNECTIVITY") ~default:"Points" );
      ]
  | None ->
      [
        (":offset [(get (frame/input) :t) 0.0 0.0]", ":translate [0.0 1.0 0.0]", "Points");
        ( ":octaves 3 :lacunarity (+ 1.5 (get (frame/input) :t)) :roughness (+ 0.25 (* (get \
           (frame/input) :t) 0.5))",
          ":translate [0.0 1.0 0.0]",
          "Points" );
        ( ":octaves (int (+ 2.0 (* (get (frame/input) :t) 60.0))) :roughness 0.6",
          ":translate [0.0 1.0 0.0]",
          "Points" );
        ( ":offset [0.5 0.0 0.0]",
          ":translate [0.0 (get (frame/input) :t) 0.0] :shear_xy (* (get (frame/input) :t) 0.5)",
          "Triangles" );
      ]

let run (drive, xdrive, connectivity) =
  let text =
    Printf.sprintf
      "(workspace chain (graph g :context sop (-> (sop/grid :counts \"Point counts\" :connectivity \
       %S :columns %d :rows %d) (sop/attribute_noise :name \"noise\" :seed 7 %s) (sop/transform \
       %s))))"
      connectivity side side drive xdrive
  in
  (* [cook frame] cooks the chain at that frame, as the editor does *)
  let animate ?gpu () =
    let lowered = Flow_sop.Lower.workspace ?gpu (Flow.Syntax.parse text |> ok) |> ok in
    let graph = List.hd lowered.graphs in
    let lane = Flow_sop.Value_lane.create () in
    let session =
      Sop.Session.create ~max_entries:64 ~max_payload_bytes:(1 lsl 30) |> Result.get_ok
    in
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
  in
  let noise geometry =
    match Rdk.Geometry.find_attribute ~owner:Point "noise" geometry with
    | Some attribute -> (
        match Rdk.Attribute.View.storage attribute with
        | Rdk.Attribute.Float data -> data
        | _ -> failwith "noise is not float storage")
    | None -> failwith "no noise attribute"
  in
  let median times = List.nth (List.sort compare times) (List.length times / 2) in
  let camera =
    Rays.Camera.perspective
      ~at:(Rays_math.Vec3.create 0.4 2.2 1.3)
      ~target:(Rays_math.Vec3.create 0. 1. 0.) ()
    (* the picture of a display, as the editor draws a cooked object, and the seconds it took to
   prepare and draw (not to read the picture) *)
  in
  let picture canvas ?device ?world geometry =
    let start = Unix.gettimeofday () in
    let surface = Rdk_rays.Surface.of_geometry geometry |> Result.get_ok in
    let scene = Rdk_rays.Surface.scene3 ?device surface in
    let scene = Option.fold ~none:scene ~some:(fun w -> Rays.Scene.D3.with_world w scene) world in
    (match
       Rays.Canvas.render canvas Rays.Scene.[ clear Rays_math.Color.black; view3d ~camera scene ]
     with
    | Ok () -> ()
    | Error message -> failwith message);
    let seconds = Unix.gettimeofday () -. start in
    (Rays.Canvas.pixels canvas |> Result.get_ok, seconds)
    (* a World with a sun, for a lit mesh: its shadow fit reads the device mesh's bounds *)
  in
  let world =
    if connectivity <> "Triangles" then None
    else
      Some
        {
          (Rays.World.bake ~domains:1 ~width:64 ~height:32 Rays.World.default) with
          sun =
            Some
              {
                direction = Rays_math.Vec3.normalize (Rays_math.Vec3.create 0.3 1. 0.2);
                radiance = Rays.World.rgb 4. 4. 4.;
                angular_radius = 0.01;
              };
        }
  in
  (* the one mesh a displayed surface draws, and the path tracer over it: created on the first
     frame, its mesh replaced on later ones (a device mesh of the same topology refits in place),
     then one full-quality frame traced and waited for; its pixels and the seconds the update and
     the trace took *)
  let trace_mesh scene =
    let found = ref None in
    Rays.Scene.D3.Walk.iter_batches
      (fun (drawing : Rays.Scene.D3.Walk.drawing) _ -> found := Some drawing.mesh)
      scene;
    Option.get !found
  in
  let module P = Rays_pathtracer in
  let traced = function Ok value -> value | Error message -> failwith message in
  let material = P.material (P.Linear_color.rgb 0.8 0.7 0.6) in
  let device_tracer = ref None and host_tracer = ref None in
  let trace tracer mesh =
    let start = Unix.gettimeofday () in
    let t =
      match !tracer with
      | Some t ->
          traced (P.replace_mesh t (traced (P.mesh [ (mesh, material) ])));
          t
      | None ->
          let t =
            traced
              (P.create ~width:128 ~height:128
                 {
                   P.objects = [ (mesh, material) ];
                   spheres = [];
                   strands = [];
                   environment =
                     {
                       sky = P.Linear_color.rgb 0.6 0.7 0.9;
                       ground = P.Linear_color.rgb 0.3 0.3 0.3;
                       panels = [];
                     };
                   lights = [];
                 })
          in
          tracer := Some t;
          t
    in
    P.reset t;
    traced (P.render t camera);
    traced (P.flush t);
    let seconds = Unix.gettimeofday () -. start in
    (Bytes.copy (traced (Rays.Image.pixels (P.image t))), seconds)
  in
  (* both trace the same float32 corners and normals through the same kernel *)
  let trace_tolerance = 0 in
  let trace_times = ref [] and host_trace_times = ref [] in
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
                (* the grid's P is made on the device from the element index: the first frame
                   uploads its N alone (count float32 vec3s), in the one dispatch *)
                if frame = 0 then begin
                  if after.input_uploaded_bytes - before.input_uploaded_bytes <> count * 12 then
                    fail
                      (Printf.sprintf "%d bytes uploaded, not the grid's N alone"
                         (after.input_uploaded_bytes - before.input_uploaded_bytes));
                  if !dispatches - calls <> 1 then
                    fail (Printf.sprintf "%d dispatches, not one" (!dispatches - calls))
                end;
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
                (* its box (the selection box, framing) is reduced on the device: 24 KB read *)
                let box = Rdk_rays.Geometry_view.bounds device and boxed = stats () in
                if
                  boxed.readback_bytes - drawn.readback_bytes > 24_576
                  || boxed.materialisations <> drawn.materialisations
                  || not (Rdk.Packed.Float3.resident (Rdk.Geometry.positions device))
                then fail "the bounds read P back";
                (* lit by a World sun: the sun's fit reads the device mesh's bounds (24 KB at
                   most), never its host copy, which would read P back *)
                let sunlit =
                  Option.map
                    (fun world ->
                      let lit =
                        fst
                          (picture canvas
                             ~device:(Flow_native.Workspace_gpu.display gpu)
                             ~world device)
                      and seen = stats () in
                      if
                        seen.readback_bytes - boxed.readback_bytes > 24_576
                        || seen.materialisations <> boxed.materialisations
                        || not (Rdk.Packed.Float3.resident (Rdk.Geometry.positions device))
                      then fail "the World sun's fit made the device mesh's host copy";
                      if lit = shown then fail "the World did not light the display";
                      (world, lit))
                    world
                in
                (* path traced from the device: the corners gathered where P and N are and the
                   structure refit (rebuilt on the first frame), nothing read back and no host copy
                   made; then traced as today, from the host copy the editor converts *)
                if connectivity = "Triangles" then begin
                  let device_mesh =
                    trace_mesh
                      (Rdk_rays.Surface.scene3
                         ~device:(Flow_native.Workspace_gpu.display gpu)
                         (Rdk_rays.Surface.of_geometry device |> Result.get_ok))
                  in
                  if Rays.Mesh.View.device device_mesh = None then fail "no device mesh to trace";
                  let before = stats () in
                  let device_pixels, device_seconds = trace device_tracer device_mesh in
                  let traced = stats () in
                  let tracer = Option.get !device_tracer in
                  if P.Private.refits tracer <> frame then
                    fail
                      (Printf.sprintf "%d refits after %d frames of one topology"
                         (P.Private.refits tracer) frame);
                  let lit = ref 0 in
                  Bytes.iteri
                    (fun i c -> if i mod 4 <> 3 && Char.code c > 8 then incr lit)
                    device_pixels;
                  if !lit < Bytes.length device_pixels / 8 then fail "the trace shows nothing";
                  if
                    traced.readback_bytes <> before.readback_bytes
                    || traced.materialisations <> before.materialisations
                    || not (Rdk.Packed.Float3.resident (Rdk.Geometry.positions device))
                  then fail "path tracing the device mesh read P back or made its host copy";
                  let host_mesh =
                    Rdk_rays.Rays_mesh.of_mesh device_mesh
                    |> Result.get_ok |> Rdk_rays.Rays_mesh.to_trace_mesh |> Result.get_ok
                  in
                  let host_pixels, host_seconds = trace host_tracer host_mesh in
                  let largest = ref 0 in
                  Bytes.iteri
                    (fun i c ->
                      largest :=
                        max !largest (abs (Char.code c - Char.code (Bytes.get host_pixels i))))
                    device_pixels;
                  if !largest > trace_tolerance then
                    fail
                      (Printf.sprintf "the device trace differs from the host copy's by %d" !largest);
                  if frame > 0 then begin
                    trace_times := device_seconds :: !trace_times;
                    host_trace_times := host_seconds :: !host_trace_times
                  end
                end;
                (* the same geometry read back and drawn from the host: the same pixels *)
                let host, seconds = picture host_canvas device in
                (* min and max are exact in float32: the device's box is the host scan's, bit for bit *)
                let p = Rdk.Geometry.positions device in
                let lo = Array.make 3 Float.infinity and hi = Array.make 3 Float.neg_infinity in
                for i = 0 to count - 1 do
                  let x, y, z = Rdk.Packed.Float3.get p i in
                  Array.iteri
                    (fun c v ->
                      lo.(c) <- Float.min lo.(c) v;
                      hi.(c) <- Float.max hi.(c) v)
                    [| x; y; z |]
                done;
                if
                  box
                  <> Some
                       ( Rays_math.Vec3.create lo.(0) lo.(1) lo.(2),
                         Rays_math.Vec3.create hi.(0) hi.(1) hi.(2) )
                then fail "the device's bounds differ from the host's";
                if frame > 0 then host_times := seconds :: !host_times;
                if host <> shown then fail "the device display differs from the host display";
                Option.iter
                  (fun (world, lit) ->
                    if fst (picture host_canvas ~world device) <> lit then
                      fail "the sunlit device display differs from the host display")
                  sunlit;
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
                done;
                Flow_native.Workspace_gpu.frame gpu
              done;
              (* the node removed: its scenes draw no display, so two frames later every binding is
                 released (no collection needed), and once its results die the pool holds nothing *)
              Option.iter P.destroy !device_tracer;
              Option.iter P.destroy !host_tracer;
              if !trace_times <> [] then
                Printf.printf
                  "path traced %d resident points (128 x 128, 1 spp): update and trace %.4f s a \
                   frame from the device (corners gathered, structure refit, nothing read back) \
                   against %.4f s from the host copy (medians of %d)\n\
                   %!"
                  count (median !trace_times) (median !host_trace_times) (frames - 1);
              if Flow_native.Workspace_gpu.held_displays gpu = 0 then failwith "no display is held";
              for _ = 0 to 2 do
                Flow_native.Workspace_gpu.frame ~keep:[] gpu
              done;
              if Flow_native.Workspace_gpu.held_displays gpu <> 0 then
                failwith "a removed display is still bound two frames later";
              gpu_close ();
              Gc.full_major ();
              Flow_native.Workspace_gpu.frame gpu;
              if (stats ()).held_buffers <> 0 then
                failwith
                  (Printf.sprintf "%d buffers still held after the node's removal"
                     (stats ()).held_buffers);
              (* float32 rounding of positions within [-0.5, 1.5] and of noise in [0, 1] *)
              if !worst > 1e-5 then failwith (Printf.sprintf "GPU differs from CPU by %.3e" !worst);
              ignore (Rays.Canvas.destroy canvas);
              ignore (Rays.Canvas.destroy host_canvas);
              Printf.printf
                "device display of %d resident points (%s), nothing read back, the host display's \
                 pixels: %.4f s a frame against %.4f s reading P back (medians of %d)\n\
                 %!"
                count connectivity (median !display_times) (median !host_times) (frames - 1);
              Printf.printf
                "animated grid > attribute_noise %s > transform %s, %d points: frame CPU %.4f s, \
                 GPU %.4f s (medians of %d), one dispatch, nothing compiled, uploaded or read \
                 back, largest difference %.2e\n\
                 %!"
                drive xdrive count (median !cpu_times) (median !gpu_times) (frames - 1) !worst)))

let () = List.iter run cases
