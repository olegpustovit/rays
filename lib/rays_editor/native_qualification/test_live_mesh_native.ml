(* A mesh whose faces depend on t, with point N and Cd, under the ordinary Measured policy: P and both
   attributes are resident from the first cook (no CPU cook to learn from, which at this shape would
   cost minutes), later frames upload nothing, and each frame equals the CPU cook of the same time.
   The large case's face mask and vertices are past 4M, the native limits the web's dispatch reach
   and an old draw bound used to set, and its display borrows the device's streams. *)
let ok = function Ok value -> value | Error d -> failwith (Diagnostic.to_string d)

let text side =
  Printf.sprintf
    {|(workspace live_mesh
  (defn cell :context value [(index : int) (side : int)]
    [(int/div index (* side side)) (mod (int/div index side) side) (mod index side)])
  (defn occupied :context value [(voxels : (array int)) (side : int) (p : vec3)]
    (if (and (and (>= p.x 0) (< p.x side))
             (and (and (>= p.y 0) (< p.y side)) (and (>= p.z 0) (< p.z side))))
        (array/nth voxels (+ (+ (* (int p.x) (* side side)) (* (int p.y) side)) (int p.z)))
        0))
  (graph city :context sop [(side : int %d)]
    (let* [clock (* t 6.0)
           voxels (map (fn [(index : int)]
                         (let* [p (cell index side)]
                           (if (< (mod (+ p.x (+ p.y p.z)) 7) (+ 2 (mod clock 3))) 1 0)))
                       (array/range (* (* side side) side) :type int))
           directions (list [1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1])
           corners (list [1 0 0] [1 1 0] [1 1 1] [1 0 1]
                         [0 0 1] [0 1 1] [0 1 0] [0 0 0]
                         [0 1 1] [1 1 1] [1 1 0] [0 1 0]
                         [0 0 0] [1 0 0] [1 0 1] [0 0 1]
                         [1 0 1] [1 1 1] [0 1 1] [0 0 1]
                         [0 0 0] [0 1 0] [1 1 0] [1 0 0])
           faces (array/compact
                   (map (fn [(face : int)]
                          (let* [index (int/div face 6)
                                 p (cell index side)]
                            (if (and (> (array/nth voxels index) 0)
                                     (= (occupied voxels side (+ p (nth directions (mod face 6)))) 0)) 1 0)))
                        (array/range (* (* side side) (* side 6)) :type int)))
           face_count (array/count faces)
           vertices (array/range (* face_count 4) :type int)
           positions (map (fn [(vertex : int)]
                            (let* [face (array/nth faces (int/div vertex 4))
                                   p (cell (int/div face 6) side)]
                              (+ p (nth corners (+ (* (mod face 6) 4) (mod vertex 4)))))) vertices)
           normals (map (fn [(vertex : int)]
                          (nth directions (mod (array/nth faces (int/div vertex 4)) 6))) vertices)
           colors (map (fn [(vertex : int)]
                         (let* [face (array/nth faces (int/div vertex 4))]
                           [(/ (array/nth voxels (int/div face 6)) 2.0) 0.5 0.25])) vertices)
           mesh (sop/mesh :points (exact positions) :vertex_points (exact vertices)
                          :primitive_offsets (exact (map (fn [(face : int)] (* face 4)) (array/range (+ face_count 1) :type int)))
                          :kinds (exact (array/int face_count 0)))
           shaded (sop/with_attr mesh "N" (exact normals) :owner "point" :type vec3)]
      (sop/with_attr shaded "Cd" (exact colors) :owner "point" :type vec3))))|}
    side

let attribute geometry name =
  match Rdk.Geometry.find_attribute ~owner:Point name geometry with
  | Some a -> a
  | None -> failwith ("missing attribute " ^ name)

let vec3 geometry name =
  match Rdk.Attribute.View.storage (attribute geometry name) with
  | Rdk.Attribute.Float3 values -> values
  | _ -> failwith (name ^ " is not vec3")

let same_vec3 what a b =
  let module P = Rdk.Packed.Float3 in
  if P.length a <> P.length b then
    failwith (Printf.sprintf "%s: %d points on the device, %d on the CPU" what (P.length a) (P.length b));
  for i = 0 to P.length a - 1 do
    let ax, ay, az = P.get a i and bx, by, bz = P.get b i in
    if Float.abs (ax -. bx) > 1e-6 || Float.abs (ay -. by) > 1e-6 || Float.abs (az -. bz) > 1e-6 then
      failwith
        (Printf.sprintf "%s[%d]: device (%g %g %g), CPU (%g %g %g)" what i ax ay az bx by bz)
  done

let cook ?gpu text frames ~each =
  let lowered = Flow_sop.Lower.workspace ?gpu (Flow.Syntax.parse text |> ok) |> ok in
  let graph = List.hd lowered.graphs in
  let lane = Flow_sop.Value_lane.create () in
  let session = Sop.Session.create ~max_entries:64 ~max_payload_bytes:(1 lsl 30) |> Result.get_ok in
  Fun.protect
    ~finally:(fun () -> Sop.Session.close session)
    (fun () ->
      List.iter
        (fun time ->
          let live = Input.at_time time in
          let resolved = Flow_sop.Value_lane.resolve ?gpu ~live lane ~time graph.network |> ok in
          let node =
            Sop.Edit_graph.compile_node resolved.geometry ~node_id:(Option.get graph.root)
            |> Result.get_ok
          in
          let output =
            Sop.Session.cook ~cpu_consumers:0 session
              ~context:(Sop.Context.create ~time ~input:live () |> Result.get_ok)
              node
            |> ok
          in
          each time (Result.get_ok (Sop.Payload.geometry output.payload)))
        frames)

let resident_case gpu config ~side ~frames ~compare ~displayed =
  let text = text side in
  let uploads () =
    let host, _, _, _, _ = Flow_native.Workspace_gpu.image_stats gpu in
    host.input_uploads
  in
  let cpu = Hashtbl.create 8 in
  if compare then
    cook text frames ~each:(fun time geometry ->
        Hashtbl.replace cpu time
          ( Rdk.Geometry.positions geometry,
            vec3 geometry "N",
            vec3 geometry "Cd",
            Rdk.Geometry.primitive_count geometry ));
  let first = ref true in
  let before = ref 0 in
  cook ~gpu:config text frames ~each:(fun time geometry ->
      if not (Rdk.Packed.Float3.resident (Rdk.Geometry.positions geometry)) then
        failwith (Printf.sprintf "side %d, t %g: P left the device" side time);
      List.iter
        (fun name ->
          if Rdk.Attribute.holder (attribute geometry name) = None then
            failwith (Printf.sprintf "side %d, t %g: %s left the device" side time name))
        [ "N"; "Cd" ];
      if !first then first := false
      else if uploads () <> !before then
        failwith
          (Printf.sprintf "side %d, t %g: %d inputs uploaded after the first frame" side time
             (uploads () - !before));
      if displayed then begin
        if Rdk.Geometry.point_count geometry <= 4_000_000 then
          failwith (Printf.sprintf "side %d: only %d points" side (Rdk.Geometry.point_count geometry));
        if Flow_native.Workspace_gpu.display gpu geometry = None then
          failwith
            (Printf.sprintf "side %d: display refused: %s" side
               (Option.value ~default:"" (Flow_native.Workspace_gpu.display_refusal gpu)))
      end;
      before := uploads ();
      if compare then begin
        let p, n, cd, primitives = Hashtbl.find cpu time in
        if primitives <> Rdk.Geometry.primitive_count geometry then
          failwith
            (Printf.sprintf "t %g: %d faces on the device, %d on the CPU" time
               (Rdk.Geometry.primitive_count geometry) primitives);
        same_vec3 "P" (Rdk.Geometry.positions geometry) p;
        same_vec3 "N" (vec3 geometry "N") n;
        same_vec3 "Cd" (vec3 geometry "Cd") cd
      end)

(* A live mesh path traced: its indices, positions, normals and colours are gathered where the device
   holds them, with no payload read back, and its picture is the host route's. *)
let traced_case gpu config =
  let module P = Rays_pathtracer in
  let got = function Ok value -> value | Error message -> failwith message in
  let time = 1. /. 6. and text = text 8 in
  let camera =
    Rays.Camera.perspective ~at:(Rays_math.Vec3.create 20. 18. 22.)
      ~target:(Rays_math.Vec3.create 4. 4. 4.) ()
  in
  let picture mesh =
    let material = P.material (P.Linear_color.rgb 1. 1. 1.) in
    let tracer =
      got
        (P.create ~width:64 ~height:64
           {
             P.objects = [ (mesh, material) ];
             spheres = [];
             strands = [];
             environment =
               {
                 sky = P.Linear_color.rgb 0.5 0.5 0.5;
                 ground = P.Linear_color.rgb 0.5 0.5 0.5;
                 panels = [];
               };
             lights = [];
           })
    in
    Fun.protect
      ~finally:(fun () -> P.destroy tracer)
      (fun () ->
        P.reset tracer;
        for _ = 1 to 16 do
          got (P.render tracer camera);
          got (P.flush tracer)
        done;
        Bytes.copy (got (Rays.Image.pixels (P.image tracer))))
  in
  let mean pixels channel =
    let sum = ref 0 in
    for i = 0 to (Bytes.length pixels / 4) - 1 do
      sum := !sum + Char.code (Bytes.get pixels ((4 * i) + channel))
    done;
    float !sum /. float (Bytes.length pixels / 4)
  in
  let host = ref None in
  cook text [ time ] ~each:(fun _ geometry ->
      host := Some (picture (got (Result.map_error Rdk.Error.to_string (Rdk_rays.Rays_mesh.to_trace_mesh geometry)))));
  let payload () =
    let stats, _, _, _, _ = Flow_native.Workspace_gpu.image_stats gpu in
    stats.payload_readback_bytes
  in
  cook ~gpu:config text [ time ] ~each:(fun _ geometry ->
      let scene =
        Rdk_rays.Surface.scene3
          ~device:(Flow_native.Workspace_gpu.display gpu)
          (Result.get_ok (Rdk_rays.Surface.of_geometry geometry))
      in
      let mesh = ref None in
      Rays.Scene.D3.Walk.iter_batches
        (fun (drawing : Rays.Scene.D3.Walk.drawing) _ -> mesh := Some drawing.mesh)
        scene;
      let mesh = Option.get !mesh in
      if Rays.Mesh.View.device mesh = None then failwith "no device mesh to trace";
      let before = payload () in
      let device = picture mesh in
      if payload () <> before then
        failwith (Printf.sprintf "the trace read back %d payload bytes" (payload () - before));
      let host = Option.get !host in
      (* the colours arrive: the mesh is yellow under a grey sky *)
      List.iter
        (fun (route, pixels) ->
          if mean pixels 0 -. mean pixels 2 < 2. then
            failwith (Printf.sprintf "%s route lost the colours: r %.1f b %.1f" route (mean pixels 0) (mean pixels 2)))
        [ ("device", device); ("host", host) ];
      List.iter
        (fun channel ->
          let difference = Float.abs (mean device channel -. mean host channel) in
          if difference > 2. then
            failwith (Printf.sprintf "channel %d: device and host differ by %.2f of 255" channel difference))
        [ 0; 1; 2 ])

(* A kernel that cannot compile says so, by name and reason, and a clean chain says nothing. *)
let reporting config =
  let module F = Flow_ir.Fallback in
  let clean = F.seen () in
  if clean <> [] then
    failwith
      (String.concat "; " (List.map (fun (site, reason, _) -> site ^ ": " ^ reason) clean));
  let broken =
    Str.global_replace (Str.regexp_string "(mod vertex 4)")
      "(mod vertex (+ 4 (* 0 (array/count vertices))))" (text 8)
  in
  cook ~gpu:config broken [ 0. ] ~each:(fun _ _ -> ());
  let named (site, reason, _) = String.ends_with ~suffix:"positions" site && reason <> "" in
  if not (List.exists named (F.seen ())) then
    failwith
      ("a refused kernel left no report: "
      ^ String.concat "; " (List.map (fun (site, reason, _) -> site ^ ": " ^ reason) (F.seen ())));
  F.reset ()

(* sketches/gpu_sea.rays lowered as the editor lowers it (deferred static maps, Lowering.of_workspace):
   every card stays one program, nothing falls off the device, no payload is read back, and once the
   measured placement has its CPU sample the mesh and its normals stay resident with nothing uploaded.
   Any silent fall fails here by name. *)
let sea_route gpu config =
  let module F = Flow_ir.Fallback in
  let side = 256 in
  let text =
    let text = In_channel.with_open_bin "../../../sketches/gpu_sea.rays" In_channel.input_all in
    Str.global_replace (Str.regexp_string "(side : int 1000)") (Printf.sprintf "(side : int %d)" side) text
  in
  let stats () =
    let host, _, _, _, _ = Flow_native.Workspace_gpu.image_stats gpu in
    host
  in
  F.reset ();
  let lowered =
    Flow_sop.Lower.workspace ~gpu:config ~defer:true ~extra:Editor_document.Kinds.descriptors
      (Flow.Syntax.parse text |> ok)
    |> ok
  in
  let graph = List.find (fun (g : Flow_sop.Lower.graph) -> g.name = "sea") lowered.graphs in
  let lane = Flow_sop.Value_lane.create () in
  let session = Sop.Session.create ~max_entries:64 ~max_payload_bytes:(1 lsl 30) |> Result.get_ok in
  let before = stats () in
  Fun.protect
    ~finally:(fun () -> Sop.Session.close session)
    (fun () ->
      let uploads = ref 0 in
      for frame = 0 to 6 do
        let time = float frame /. 60. in
        let live = Input.at_time time in
        let resolved = Flow_sop.Value_lane.resolve ~gpu:config ~live lane ~time graph.network |> ok in
        let node =
          Sop.Edit_graph.compile_node resolved.geometry ~node_id:(Option.get graph.root)
          |> Result.get_ok
        in
        let output =
          Sop.Session.cook ~cpu_consumers:0 session
            ~context:(Sop.Context.create ~time ~input:live () |> Result.get_ok)
            node
          |> ok
        in
        let geometry = Result.get_ok (Sop.Payload.geometry output.payload) in
        (* frame 2 is the placement's CPU sample of the chain *)
        if frame >= 3 then begin
          if not (Rdk.Packed.Float3.resident (Rdk.Geometry.positions geometry)) then
            failwith (Printf.sprintf "gpu_sea frame %d: P left the device" frame);
          if Rdk.Attribute.holder (attribute geometry "N") = None then
            failwith (Printf.sprintf "gpu_sea frame %d: N left the device" frame);
          if frame > 3 && (stats ()).input_uploads <> !uploads then
            failwith (Printf.sprintf "gpu_sea frame %d: an input was uploaded" frame);
          uploads := (stats ()).input_uploads
        end
      done);
  let after = stats () in
  if after.payload_readback_bytes <> before.payload_readback_bytes then
    failwith
      (Printf.sprintf "gpu_sea read back %d payload bytes"
         (after.payload_readback_bytes - before.payload_readback_bytes));
  match F.seen () with
  | [] -> ()
  | off ->
      failwith
        ("gpu_sea left the device: "
        ^ String.concat "; " (List.map (fun (site, reason, _) -> site ^ ": " ^ reason) off))

let () =
  let gpu = Flow_native.Workspace_gpu.create () in
  Fun.protect
    ~finally:(fun () -> Flow_native.Workspace_gpu.close gpu)
    (fun () ->
      Flow_native.Workspace_gpu.with_backend ~fast:true gpu (fun config ->
          (* t 0 and 1/6 give different face counts *)
          resident_case gpu config ~side:8 ~frames:[ 0.; 1. /. 6. ] ~compare:true ~displayed:false;
          (* 12.6M mask elements, over 4M vertices. Its third cook samples the CPU, whose minutes a budget
             abandons: every frame comes from the device *)
          traced_case gpu config;
          reporting config;
          sea_route gpu config;
          resident_case gpu config ~side:128 ~frames:[ 0.; 0.01; 0.02; 0.03; 0.04 ] ~compare:false ~displayed:true));
  print_endline "Live mesh: P, N and Cd resident from the first cook; no uploads; equal to the CPU"
