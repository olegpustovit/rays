open Rays
open Sop

type ui_mode = Visible | Hidden
type preview = Pieces of Rdk_rays.Packed_pieces.t | Mesh of Mesh.t

let explosion node = Rdk_rays.Packed_pieces.explosion
    ~operation:(Sop.Node.operation node) ~fields:(Sop.Node.parameter_fields node)
let mesh_for_node node pieces =
  Rdk_rays.Packed_pieces.mesh_for_explosion (explosion node) pieces

type gc_snapshot = {
  allocated_bytes : float;
  minor_words : float;
  promoted_words : float;
  major_words : float;
  major_collections : int;
  heap_words : int;
  top_heap_words : int;
}

type result = {
  frames : int;
  wall_seconds : float;
  user_seconds : float;
  system_seconds : float;
  allocated_bytes : float;
  minor_bytes : float;
  promoted_bytes : float;
  major_bytes : float;
  major_collections : int;
  ending_heap_bytes : int;
  peak_heap_bytes : int;
  starting_rss_kib : int option;
  ending_rss_kib : int option;
  peak_sampled_rss_kib : int option;
  median_frame_seconds : float;
  p95_frame_seconds : float;
  p99_frame_seconds : float;
}

module Allocation_profile = struct
  let enabled = Sys.getenv_opt "RAYS_RENDERER_MEMPROF" = Some "1"
  let sampling_rate = 1e-4
  let samples : (string, int) Hashtbl.t = Hashtbl.create 128
  let lock = Mutex.create ()
  let profile = ref None

  let name allocation =
    match Printexc.backtrace_slots allocation.Gc.Memprof.callstack with
    | None -> "unknown"
    | Some slots ->
        Array.to_list slots
        |> List.filter_map Printexc.Slot.name
        |> List.filter (fun name ->
          not (String.starts_with ~prefix:"camlGc__Memprof" name))
        |> List.to_seq |> Seq.take 4 |> List.of_seq
        |> function [] -> "unknown" | names -> String.concat " <- " names

  let record allocation =
    let name = name allocation in
    Mutex.lock lock;
    Hashtbl.replace samples name
      (allocation.Gc.Memprof.n_samples
       + Option.value (Hashtbl.find_opt samples name) ~default:0);
    Mutex.unlock lock;
    None

  let start () = if enabled && Option.is_none !profile then
    profile := Some (Gc.Memprof.start ~sampling_rate ~callstack_size:24 {
      Gc.Memprof.null_tracker with alloc_minor = record; alloc_major = record })

  let stop () = match !profile with
    | None -> ()
    | Some value ->
        Gc.Memprof.stop ();
        Gc.Memprof.discard value;
        profile := None;
        let entries = Hashtbl.fold (fun name count values ->
          (count, name) :: values) samples []
          |> List.sort (fun (left, _) (right, _) -> Int.compare right left) in
        List.iteri (fun index (count, name) -> if index < 20 then
          Printf.eprintf "memprof %.1f MiB %s\n%!"
            (float_of_int count /. sampling_rate
             *. float_of_int (Sys.word_size / 8) /. 1_048_576.) name) entries
end

type model = {
  environment : preview Rays_editor.Editor.t;
  launched_at : float;
  hidden_toggled : bool;
  ready_at : float option;
  measuring : bool;
  started_at : float;
  started_times : Unix.process_times option;
  started_gc : gc_snapshot option;
  started_rss_kib : int option;
  sampled_peak_rss_kib : int option;
  next_rss_sample_at : float;
  last_frame_at : float;
  frame_times : float array;
  frame_count : int;
  result : result option;
  pieces : int option;
  triangles : int option;
  render_vertices : int option;
  drawable_width : int;
  drawable_height : int;
  pixel_scale_x : float;
  pixel_scale_y : float;
}

let integer_environment name default =
  match Sys.getenv_opt name with
  | None -> default
  | Some value -> max 1 (int_of_string value)

let float_environment name default =
  match Sys.getenv_opt name with
  | None -> default
  | Some value ->
      let value = float_of_string value in
      if not (Float.is_finite value) || value <= 0. then
        invalid_arg (name ^ " must be finite and positive");
      value

let ui_mode =
  if Array.length Sys.argv <> 2 then
    invalid_arg "bench_shattered_renderer: expected visible or hidden";
  match String.lowercase_ascii Sys.argv.(1) with
  | "visible" -> Visible
  | "hidden" -> Hidden
  | value -> invalid_arg ("bench_shattered_renderer: unknown mode " ^ value)

let ui_mode_name = function Visible -> "visible" | Hidden -> "hidden"
let domains = integer_environment "RAYS_SHATTER_DOMAINS" 1
let grain = integer_environment "RAYS_SHATTER_GRAIN" 2
let warmup_seconds = float_environment "RAYS_RENDERER_BENCH_WARMUP" 3.
let measure_seconds = float_environment "RAYS_RENDERER_BENCH_SECONDS" 30.
let cook_timeout_seconds = float_environment "RAYS_SHATTER_COOK_TIMEOUT" 180.

let result_exn = function
  | Ok value -> value
  | Error message -> failwith message

let gc_snapshot () =
  let quick = Gc.quick_stat () in
  {
    allocated_bytes = Gc.allocated_bytes ();
    minor_words = quick.minor_words;
    promoted_words = quick.promoted_words;
    major_words = quick.major_words;
    major_collections = quick.major_collections;
    heap_words = quick.heap_words;
    top_heap_words = quick.top_heap_words;
  }

let bytes_of_words words = words *. float_of_int (Sys.word_size / 8)
let int_bytes_of_words words = words * (Sys.word_size / 8)

let resident_kib () =
  let arguments = [|"/bin/ps"; "-o"; "rss="; "-p";
    string_of_int (Unix.getpid ())|] in
  try
    let channel = Unix.open_process_args_in arguments.(0) arguments in
    Fun.protect ~finally:(fun () -> ignore (Unix.close_process_in channel))
      (fun () ->
        let value = input_line channel |> String.trim in
        if value = "" then None else Some (int_of_string value))
  with End_of_file | Failure _ | Unix.Unix_error _ -> None

let maximum_option left right = match left, right with
  | None, value | value, None -> value
  | Some left, Some right -> Some (max left right)

let percentile values length fraction =
  if length = 0 then 0.
  else begin
    let copy = Array.sub values 0 length in
    Array.sort Float.compare copy;
    let index = int_of_float (Float.ceil (fraction *. float_of_int length)) - 1
      |> max 0 |> min (length - 1) in
    copy.(index)
  end

(* The shattered-cube sketch's document, run from the repository root. *)
let workspace () =
  let path = "sketches/shattered_cube.rays" in
  match Rays_editor.Workspace.load (In_channel.with_open_bin path In_channel.input_all) with
  | Ok workspace -> workspace
  | Error diagnostics -> failwith (String.concat "; " (List.map Diagnostic.to_string diagnostics))

let material = Material.create ~diffuse:(Rays_math.Color.hex_exn "#f2b36d")
    ~ambient:(Rays_math.Color.hex_exn "#422006") ~specular:Rays_math.Color.white ~shininess:48. ()

let lights = [
  Light.directional ~direction:(Rays_math.Vec3.create (-1.) (-1.5) (-2.))
    ~diffuse:(Rays_math.Color.hex_exn "#fff7ed") ();
  Light.directional ~direction:(Rays_math.Vec3.create 1.2 0.4 (-0.8))
    ~diffuse:(Rays_math.Color.hex_exn "#7dd3fc") ~intensity:0.55 ();
]

let prepare output =
  match Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Primitive "piece"
      (Result.get_ok (Sop.Payload.geometry output.Session.payload)) with
  | Some attribute ->
      (match Rdk.Attribute.Private.storage attribute with
       | Rdk.Attribute.Int _ | Text _ ->
           Rdk_rays.Packed_pieces.of_geometry ~piece_attribute:"piece"
             (Result.get_ok (Sop.Payload.geometry output.payload))
           |> Result.map (fun pieces -> Pieces pieces)
       | _ -> Rdk_rays.Rays_mesh.to_mesh (Result.get_ok (Sop.Payload.geometry output.payload))
           |> Result.map (fun mesh -> Mesh mesh)
           |> Result.map_error Rdk.Error.to_string)
  | None -> Rdk_rays.Rays_mesh.to_mesh (Result.get_ok (Sop.Payload.geometry output.payload))
      |> Result.map (fun mesh -> Mesh mesh)
      |> Result.map_error Rdk.Error.to_string

let scene3 node preview =
  let mesh = match preview with
    | Pieces pieces -> mesh_for_node node pieces
    | Mesh mesh -> mesh in
  let primitive_mode = Mesh.mode mesh in
  let shading = match Node.operation node with
    | "box" | "grid" | "platonic" -> Scene.D3.Flat
    | _ -> Scene.D3.Smooth in
  let preview_material = match primitive_mode with
    | Mesh.Points | Lines | Line_strip | Line_loop ->
        Material.unlit (Rays_math.Color.hex_exn "#fbbf74")
    | Triangles | Triangle_strip | Triangle_fan -> material in
  let cull = match preview, primitive_mode with
    | Pieces _, (Mesh.Triangles | Triangle_strip | Triangle_fan) ->
        Scene.D3.Cull_back
    | _ -> Scene.D3.Cull_none in
  let drawing = Scene.D3.mesh ~cull ~shading
      ~material:preview_material mesh in
  Scene.D3.create ~samples:1 ~lights [drawing]

let overlay graph preview (frame : Frame.t) =
  let pieces = match preview with
    | None -> "waiting for first cook"
    | Some (Pieces pieces) -> Printf.sprintf "%d closed pieces"
        (Rdk_rays.Packed_pieces.piece_count pieces)
    | Some (Mesh mesh) -> Printf.sprintf "%d preview vertices"
        (Mesh.vertex_count mesh) in
  Scene.[
    text ~at:(20, 18) "SOP Shattered Cube";
    text ~at:(20, 43) (Printf.sprintf "%d nodes · %s"
      (List.length (Graph.inspect graph)) pieces);
    text ~at:(20, (snd frame.input.size) - 82)
      "Select source-switch for Cube / Dodecahedron · VIEW displays any SOP";
  ]

let cardinality environment =
  match Rays_editor.Editor.prepared environment with
  | None -> None
  | Some (Mesh _) -> failwith "shattered-cube fixture lost its piece attribute"
  | Some (Pieces pieces) ->
      let mesh = mesh_for_node
          (Rays_editor.Editor.displayed_node environment) pieces in
      let piece_count = Rdk_rays.Packed_pieces.piece_count pieces
      and triangles = Mesh.Private.triangle_count mesh
      and render_vertices = Mesh.vertex_count mesh in
      if piece_count <> 18_278 || triangles <> 278_368
          || render_vertices <> 835_104 then
        failwith (Printf.sprintf
          "shattered-cube cardinality changed: pieces=%d triangles=%d render_vertices=%d"
          piece_count triangles render_vertices);
      Some (piece_count, triangles, render_vertices)

let init (frame : Frame.t) =
  let environment = Rays_editor.Editor.create
      ~camera:(Easy_camera.create ~target:Rays_math.Vec3.zero ~distance:6.8
        ~azimuth:0.72 ~elevation:0.42 ())
      ~seed:7349L ~grain ~domains ~max_entries:24
      ~max_payload_bytes:(256 * 1024 * 1024)
      ~workspace:(workspace ()) ~prepare:(fun _ -> prepare)
      ~draw:scene3 () |> result_exn in
  let now = Unix.gettimeofday () in
  {
    environment;
    launched_at = now;
    hidden_toggled = false;
    ready_at = None;
    measuring = false;
    started_at = 0.;
    started_times = None;
    started_gc = None;
    started_rss_kib = None;
    sampled_peak_rss_kib = None;
    next_rss_sample_at = 0.;
    last_frame_at = now;
    frame_times = Array.make 16_384 0.;
    frame_count = 0;
    result = None;
    pieces = None;
    triangles = None;
    render_vertices = None;
    drawable_width = int_of_float (float (fst frame.input.size) *. fst frame.pixel_scale);
    drawable_height = int_of_float (float (snd frame.input.size) *. snd frame.pixel_scale);
    pixel_scale_x = fst frame.pixel_scale;
    pixel_scale_y = snd frame.pixel_scale;
  }

let finish model now =
  Allocation_profile.stop ();
  let started_gc = Option.get model.started_gc
  and started_times = Option.get model.started_times in
  let ending_gc = gc_snapshot () and ending_times = Unix.times () in
  let ending_rss_kib = resident_kib () in
  let result = {
    frames = model.frame_count;
    wall_seconds = now -. model.started_at;
    user_seconds = ending_times.tms_utime -. started_times.tms_utime;
    system_seconds = ending_times.tms_stime -. started_times.tms_stime;
    allocated_bytes = ending_gc.allocated_bytes -. started_gc.allocated_bytes;
    minor_bytes = bytes_of_words (ending_gc.minor_words -. started_gc.minor_words);
    promoted_bytes = bytes_of_words
        (ending_gc.promoted_words -. started_gc.promoted_words);
    major_bytes = bytes_of_words (ending_gc.major_words -. started_gc.major_words);
    major_collections = ending_gc.major_collections - started_gc.major_collections;
    ending_heap_bytes = int_bytes_of_words ending_gc.heap_words;
    peak_heap_bytes = int_bytes_of_words ending_gc.top_heap_words;
    starting_rss_kib = model.started_rss_kib;
    ending_rss_kib;
    peak_sampled_rss_kib = maximum_option model.sampled_peak_rss_kib
        ending_rss_kib;
    median_frame_seconds = percentile model.frame_times model.frame_count 0.5;
    p95_frame_seconds = percentile model.frame_times model.frame_count 0.95;
    p99_frame_seconds = percentile model.frame_times model.frame_count 0.99;
  } in
  Sketch.quit ();
  { model with result = Some result }

let update model (frame : Frame.t) =
  let now = Unix.gettimeofday () in
  if Option.is_none model.ready_at
      && now -. model.launched_at > cook_timeout_seconds then
    failwith "shattered-cube benchmark cook timed out";
  let frame, hidden_toggled = match ui_mode, model.hidden_toggled with
    | Hidden, false ->
        { frame with input = { frame.input with events = Input.Key_pressed (Input.KeyChar 'h')
            :: frame.input.events } }, true
    | (Visible, _ | Hidden, true) -> frame, model.hidden_toggled in
  let environment = Rays_editor.Editor.update model.environment frame in
  let model = { model with environment; hidden_toggled;
      drawable_width = int_of_float (float (fst frame.input.size) *. fst frame.pixel_scale);
      drawable_height = int_of_float (float (snd frame.input.size) *. snd frame.pixel_scale);
      pixel_scale_x = fst frame.pixel_scale; pixel_scale_y = snd frame.pixel_scale } in
  let model = match model.ready_at with
    | Some _ -> model
    | None ->
        (match cardinality environment with
         | None -> model
         | Some (pieces, triangles, render_vertices) ->
             { model with ready_at = Some now; pieces = Some pieces;
               triangles = Some triangles; render_vertices = Some render_vertices }) in
  match model.ready_at with
  | None -> model
  | Some ready_at when not model.measuring
      && now -. ready_at >= warmup_seconds ->
      Gc.full_major ();
      Allocation_profile.start ();
      let rss = resident_kib () in
      { model with measuring = true; started_at = now;
        started_times = Some (Unix.times ()); started_gc = Some (gc_snapshot ());
        started_rss_kib = rss; sampled_peak_rss_kib = rss;
        next_rss_sample_at = now +. 1.; last_frame_at = now; frame_count = 0 }
  | Some _ when model.measuring
      && now -. model.started_at >= measure_seconds -> finish model now
  | Some _ when model.measuring ->
      let frame_count = model.frame_count + 1 in
      if frame_count > Array.length model.frame_times then
        failwith "shattered renderer frame sample capacity exceeded";
      model.frame_times.(frame_count - 1) <- now -. model.last_frame_at;
      let sampled_peak_rss_kib, next_rss_sample_at =
        if now >= model.next_rss_sample_at then
          maximum_option model.sampled_peak_rss_kib (resident_kib ()), now +. 1.
        else model.sampled_peak_rss_kib, model.next_rss_sample_at in
      { model with frame_count; last_frame_at = now; sampled_peak_rss_kib;
        next_rss_sample_at }
  | Some _ -> model

let target_name () = "native"

let option_int_json = Option.fold ~none:"null" ~some:string_of_int

let print_result model result =
  let profile = Option.value ~default:"unknown"
      (Sys.getenv_opt "RAYS_BENCH_PROFILE") in
  Printf.printf
    "{\"schema\":1,\"benchmark\":\"shattered_renderer\",\"scenario\":\"shattered-%s\",\"target\":\"%s\",\"profile\":\"%s\",\"width\":1200,\"height\":760,\"drawable_width\":%d,\"drawable_height\":%d,\"pixel_scale\":[%.6f,%.6f],\"domains\":%d,\"grain\":%d,\"warmup_seconds\":%.6f,\"requested_measure_seconds\":%.6f,\"pieces\":%s,\"triangles\":%s,\"render_vertices\":%s,\"frames\":%d,\"wall_seconds\":%.9f,\"frames_per_second\":%.6f,\"median_frame_seconds\":%.9f,\"p95_frame_seconds\":%.9f,\"p99_frame_seconds\":%.9f,\"user_seconds\":%.9f,\"system_seconds\":%.9f,\"cpu_percent\":%.6f,\"allocated_bytes\":%.0f,\"minor_bytes\":%.0f,\"promoted_bytes\":%.0f,\"major_bytes\":%.0f,\"major_collections\":%d,\"ending_heap_bytes\":%d,\"peak_heap_bytes\":%d,\"starting_rss_kib\":%s,\"ending_rss_kib\":%s,\"peak_sampled_rss_kib\":%s,\"legacy_gpu_duration_seconds\":null,\"legacy_gpu_utilization_percent\":null,\"legacy_draw_count\":null,\"legacy_upload_bytes\":null}\n%!"
    (ui_mode_name ui_mode) (target_name ()) profile model.drawable_width
    model.drawable_height model.pixel_scale_x model.pixel_scale_y domains grain
    warmup_seconds measure_seconds (option_int_json model.pieces)
    (option_int_json model.triangles) (option_int_json model.render_vertices)
    result.frames result.wall_seconds
    (float_of_int result.frames /. result.wall_seconds)
    result.median_frame_seconds result.p95_frame_seconds result.p99_frame_seconds
    result.user_seconds result.system_seconds
    ((result.user_seconds +. result.system_seconds) /. result.wall_seconds *. 100.)
    result.allocated_bytes result.minor_bytes result.promoted_bytes
    result.major_bytes result.major_collections result.ending_heap_bytes
    result.peak_heap_bytes (option_int_json result.starting_rss_kib)
    (option_int_json result.ending_rss_kib)
    (option_int_json result.peak_sampled_rss_kib)

let () =
  let final = Result.get_ok(Sketch.run_state
      ~config:{ Sketch.default_config with width = 1200; height = 760;
        title = "Rays shattered-cube renderer benchmark"; domains = Some domains }
      ~init ~update
      ~view:(fun model frame ->
        Rays_editor.Editor.scene model.environment frame)
      ~on_stop:(fun model -> Rays_editor.Editor.close model.environment) ()) in
  match final.result with
  | None -> failwith "shattered renderer stopped before producing a result"
  | Some result -> print_result final result
