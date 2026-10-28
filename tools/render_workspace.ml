(* A finite native beauty render of a workspace's authored scene and camera. *)
open Rays
open Sop
open Editor_document
module P = Rays_pathtracer

let get = function Ok value -> value | Error message -> failwith message
let benchmark, arguments = match List.tl (Array.to_list Sys.argv) with
  | "--benchmark" :: rest -> true, rest
  | rest -> false, rest
let path, output, options = match arguments with
  | path :: output :: options when List.length options <= 2 -> path, output, options
  | _ -> failwith "usage: render_workspace [--benchmark] FILE.rays OUTPUT [SPP [SIZE]]"
let text = In_channel.with_open_bin path In_channel.input_all
let imports = match Rays_editor.Source.read_imports ~file:path text with
  | Ok imports -> imports | Error ds -> failwith (String.concat "\n" (List.map Diagnostic.to_string ds))
let workspace = match Rays_editor.Workspace.load ~imports text with
  | Ok w -> w | Error ds -> failwith (String.concat "\n" (List.map Diagnostic.to_string ds))
let document = Contexts.of_workspace ~factories:Sop.Nodes.factories workspace
  |> Result.map_error Diagnostic.to_string |> get
let scene = document.scene
let camera_node = match Objects.ids "camera" scene with
  | [] -> prerr_endline (path ^ ": the scene has no camera to render through (add a scene/camera)"); exit 1
  | id :: _ -> Kind_graph.find scene id |> Option.get
let camera = Objects.Camera.of_node camera_node |> Option.get |> fst
let settings = Objects.Root.render document.root
let spp = match options with value :: _ -> int_of_string value | [] -> settings.max_spp
let width, height = match options with
  | [_; size] -> let size = int_of_string size in size, size
  | _ -> settings.width, settings.height
let () = if spp < 1 || width < 1 || height < 1 then failwith "positive SPP and dimensions required"
let linear (c : Rays_math.Color.t) = P.Linear_color.rgb (float c.r /. 255.) (float c.g /. 255.) (float c.b /. 255.)
let material (m : Material.t) = P.material
  ~roughness:(sqrt (2. /. (m.shininess +. 2.))) ~emission:(linear m.emissive) (linear m.diffuse)

type model = { tracer : P.t; canvas : Canvas.t option; setup_seconds : float }

let init _ =
  let started = Unix.gettimeofday () in
  let session = Session.create ~max_entries:64 ~max_payload_bytes:(256 * 1024 * 1024) |> get in
  let placed = ref [] and first = ref None in
  Fun.protect ~finally:(fun () -> Session.close session) (fun () ->
    List.iter (fun id ->
      let object_node = Kind_graph.find scene id |> Option.get in
      if Objects.visible object_node && Objects.flag "render" object_node then begin
        let network = Document.Int_map.find id document.networks in
        let resolved = Flow_sop.Value_lane.resolve (Flow_sop.Value_lane.create ())
            ~time:0. network.graph |> Result.map_error Diagnostic.to_string |> get in
        let node = Edit_graph.compile_node resolved.geometry
            ~node_id:(Option.get network.displayed) |> get in
        let cooked = Session.cook session ~context:(Context.create ~domains:1 ~grain:2 () |> get) node
            |> Result.map_error Diagnostic.error_to_string |> get in
        Printf.printf "%s: %d primitives, %d points\n%!" object_node.label
          (Rdk.Geometry.primitive_count (Result.get_ok (Sop.Payload.geometry cooked.payload))) (Rdk.Geometry.point_count (Result.get_ok (Sop.Payload.geometry cooked.payload)));
        let geometry = Sop.Payload.geometry cooked.payload
            |> Result.map_error Diagnostic.error_to_string |> get in
        let surface = Rdk_rays.Surface.of_geometry ?instances:cooked.instances geometry |> get in
        let explosion = Rdk_rays.Packed_pieces.explosion
            ~operation:(Node.operation node) ~fields:(Node.parameter_fields node) in
        let drawing = Rdk_rays.Surface.scene3 ?explosion surface in
        Scene.D3.Private.iter_batches (fun (d : Scene.D3.Private.drawing) transforms ->
          let material = material d.material in
          let geometry = Rdk_rays.Rays_mesh.of_mesh d.mesh
              |> Result.map_error Rdk.Error.to_string |> get in
          let trace_mesh = Rdk_rays.Rays_mesh.to_trace_mesh geometry
              |> Result.map_error Rdk.Error.to_string |> get in
          if !first = None then first := Some (trace_mesh, material);
          let mesh = match transforms with
            | None -> P.mesh [trace_mesh, material] |> get
            | Some transforms ->
                let prototype = P.mesh [trace_mesh, material] |> get in
                P.mesh_instanced ~prototype transforms |> get in
          placed := (Rays_math.Mat4.mul (Objects.world scene id) d.transform, mesh) :: !placed) drawing
      end) (Objects.ids "geometry" scene));
  let mesh = P.scene_mesh (List.rev !placed) |> get in
  let tracer = P.create ~spp:4 ~profile:(Sys.getenv_opt "RAYS_PATHTRACER_PROFILE" = Some "1")
      ~bounces:(if benchmark then document.root.bounces else 6)
      ~round_samples:(if benchmark then document.root.round_samples else 4) ~width ~height
      {P.objects = [Option.get !first]; spheres = []; strands = [];
       environment = {sky = P.Linear_color.rgb 0.22 0.24 0.25;
         ground = P.Linear_color.rgb 0.12 0.13 0.14; panels = []};
       lights = List.map P.light_of (Objects.lights ~render:true scene)} |> get in
  P.replace_mesh tracer mesh |> get;
  List.iter (fun id ->
    let node = Kind_graph.find scene id |> Option.get in
    match Document.layers document id with
    | None -> ()
    | Some layers -> Option.iter (fun world ->
        P.set_world tracer (Some (World.bake ~domains:1 ~width:512 ~height:256 world)) |> get)
        (Layers.to_world node layers)) (Objects.ids "world" scene);
  P.reset tracer;
  {tracer; canvas = (if benchmark then None else Some (Canvas.create_exn ~width ~height));
   setup_seconds = Unix.gettimeofday () -. started}

let render_samples tracer =
  while P.samples tracer < spp do
    P.render tracer camera |> get;
    P.flush tracer |> get
  done

let run_benchmark model =
  let report = Buffer.create 1024 in
  Printf.bprintf report "# scene=%s,width=%d,height=%d,spp_per_frame=4,bounces=%d,round_samples=%d,setup_s=%.9f\n"
    path width height document.root.bounces document.root.round_samples model.setup_seconds;
  Buffer.add_string report "round,samples,wall_s,cpu_s,allocated_bytes,rgba_sha256\n";
  (* Compile and warm the same scene before five deterministic measured rounds.
     RAYS_PATHTRACER_PROFILE=1 logs each GPU band between these markers. *)
  P.render model.tracer camera |> get;
  P.flush model.tracer |> get;
  for round = 1 to 5 do
    P.reset model.tracer;
    Printf.eprintf "pathtracer_benchmark begin %d\n%!" round;
    let allocated = Gc.allocated_bytes () and cpu = Sys.time ()
    and started = Unix.gettimeofday () in
    render_samples model.tracer;
    let seconds = Unix.gettimeofday () -. started
    and cpu_seconds = Sys.time () -. cpu
    and allocated_bytes = Gc.allocated_bytes () -. allocated in
    Printf.eprintf "pathtracer_benchmark end %d\n%!" round;
    let rgba = Rays.Image.pixels (P.image model.tracer) |> get in
    let digest = Contexts.sha256 (Bytes.to_string rgba) in
    let row = Printf.sprintf "%d,%d,%.9f,%.9f,%.0f,%s\n"
      round (P.samples model.tracer) seconds cpu_seconds allocated_bytes digest in
    Buffer.add_string report row;
    print_string row;
    flush stdout
  done;
  Runtime_resources.write_text ~filename:output (Buffer.contents report) |> get

let () =
  let model = init () in
  Fun.protect ~finally:(fun () -> Option.iter (fun canvas->ignore(Canvas.destroy canvas)) model.canvas; P.destroy model.tracer) (fun () ->
    if benchmark then run_benchmark model else begin
      render_samples model.tracer;
      let canvas = Option.get model.canvas in
      Canvas.render canvas [Scene.image (P.image model.tracer) ~at:(0,0) ()] |> get;
      Canvas.save_png canvas output |> get;
      Printf.printf "Saved %s (%dx%d, %d spp)\n%!" output width height (P.samples model.tracer)
    end)
