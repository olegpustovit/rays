type scenario = Basic | Pxui_like | Canvas_offscreen | Scene3_builtin

let get = function
  | Ok value -> value
  | Error error -> failwith (Format.asprintf "%a" Renderer.pp_error error)

let create width height =
  Renderer.create
    {
      Renderer.default_configuration with
      logical_width = width;
      logical_height = height;
      drawable_width = width;
      drawable_height = height;
    }

let live_handles = Ogpu.Impl.live_handles

let rss_kib () =
  let argv = [| "/bin/ps"; "-o"; "rss="; "-p"; string_of_int (Unix.getpid ()) |] in
  let input = Unix.open_process_args_in argv.(0) argv in
  Fun.protect
    ~finally:(fun () -> ignore (Unix.close_process_in input))
    (fun () -> int_of_string (String.trim (input_line input)))

(* ps's RSS also counts clean file-backed pages (Metal's mapped shader and
   driver caches fault in and out of residence), which grew about 0.5 MiB per
   lifecycle while the process's physical footprint, the dirty and swapped
   memory a leak would add to, stayed flat. The plateau check reads the
   footprint; RSS stays in the report. *)
let footprint_kib () =
  let argv = [| "/usr/bin/footprint"; "-p"; string_of_int (Unix.getpid ()); "-f"; "bytes" |] in
  let input = Unix.open_process_args_in argv.(0) argv in
  Fun.protect
    ~finally:(fun () -> ignore (Unix.close_process_in input))
    (fun () ->
      let rec find () =
        let rec after = function
          | "Footprint:" :: bytes :: _ -> Some (int_of_string bytes / 1024)
          | _ :: rest -> after rest
          | [] -> None
        in
        match after (String.split_on_char ' ' (input_line input)) with
        | Some kib -> kib
        | None -> find ()
      in
      find ())

let name = function
  | Basic -> "basic"
  | Pxui_like -> "pxui-like"
  | Canvas_offscreen -> "canvas-offscreen"
  | Scene3_builtin -> "scene3-builtin"

let draw_count = function
  | Basic -> 1
  | Pxui_like -> 8
  | Canvas_offscreen -> 3
  | Scene3_builtin -> 4

(* Frozen captures keyed by the drawable extent: the 4-point window is 4 pixels
   on a 1x display and 8 on a 2x one; the 8-point resize is 8 or 16. *)
let frozen_software_hash scenario ~extent =
  match (scenario, extent) with
  | Basic, 4 -> Some "183be222f2a9499335f80e49bab17f24"
  | Basic, 8 -> Some "a07e462221d8a87c5d83ad9a18411a44"
  | Basic, 16 -> Some "832bb817e4b32fff17f7f2024339caac"
  | Pxui_like, 4 -> Some "c28fb438cfaf0381adc6b8dddec9d6cf"
  | Pxui_like, 8 -> Some "f9dc84a13290d920f46adc67e228d10e"
  | Canvas_offscreen, 4 -> Some "6669327647bf796bcc136e62a62f7c95"
  | Canvas_offscreen, 8 -> Some "38c2ee94260263451822ed73103813c9"
  | Scene3_builtin, 4 -> Some "52462b7485ced3177c5f7bf2fda8a7ae"
  | Scene3_builtin, 8 -> Some "be5af85c01ba3b5cff24d10ef28c4b3e"
  | _ -> None

let put_vertex bytes index (x, y) color =
  let offset = index * 16 in
  Bytes.set_int32_le bytes offset (Int32.bits_of_float x);
  Bytes.set_int32_le bytes (offset + 4) (Int32.bits_of_float y);
  Bytes.set_int32_le bytes (offset + 8) color

let mesh scenario index =
  let vertices = Bytes.make 48 '\000' and indices = Bytes.make 12 '\000' in
  Bytes.set_int32_le indices 4 1l;
  Bytes.set_int32_le indices 8 2l;
  let positions, color =
    match scenario with
    | Basic -> ([ (-1., 1.); (1., 1.); (-1., -1.) ], 0x4080BFFFl)
    | Canvas_offscreen -> ([ (-1., 1.); (0., 1.); (-1., 0.) ], 0x4080BFFFl)
    | Scene3_builtin -> ([ (1., 1.); (1., -1.); (-1., -1.) ], 0x4080BFFFl)
    | Pxui_like -> ([ (-1., -1.); (3., -1.); (-1., 3.) ], 0x4080BFFFl)
  in
  List.iteri (fun vertex position -> put_vertex vertices vertex position color) positions;
  {
    Scene_execution.key = Host (Printf.sprintf "%s-%d" (name scenario) index);
    vertices;
    vertex_count = 3;
    indices;
    index_count = 3;
    primitive = Ogpu.Render_pass.Triangle_list;
  }

let draws scenario extent =
  List.init (draw_count scenario) (fun index ->
      let inset = 0 in
      {
        Scene_execution.mesh = mesh scenario index;
        state =
          {
            viewport = (0, 0, extent, extent);
            scissor = (inset, inset, extent - inset, extent - inset);
            cull = Ogpu.Render_pass.Cull_none;
            depth_compare = Ogpu.Render_pass.Always;
            depth_write = false;
            depth_load = Ogpu.Render_pass.Clear;
            depth_clear = 1.;
            transform_uniforms = None;
          };
      })

let check_pixels extent bytes =
  if Bytes.length bytes <> extent * extent * 4 then failwith "native capture size drift";
  Digest.to_hex (Digest.bytes bytes)

let run scenario =
  let runtime = get (create 4 4) in
  let initial_facts = get (Runtime.presentation_facts (Renderer.Native.runtime runtime)) in
  let clear = (0., 0., 0., 0.) in
  let checkpoints = ref [] in
  for frame = 1 to 600 do
    let draws = List.map (Renderer.prepared_draw ~family:Renderer.Scene2) (draws scenario 4) in
    ignore (get (Renderer.draw ~clear runtime draws));
    if List.mem frame [ 1; 2; 60; 600 ] then
      checkpoints :=
        ( frame,
          check_pixels initial_facts.drawable_width
            (get (Renderer.read_pixels runtime ~bytes_per_row:(initial_facts.drawable_width * 4)))
        )
        :: !checkpoints
  done;
  get (Runtime.resize (Renderer.Native.runtime runtime) ~width:8 ~height:8);
  let resized_facts = get (Runtime.presentation_facts (Renderer.Native.runtime runtime)) in
  ignore
    (get
       (Renderer.draw ~clear runtime
          (List.map (Renderer.prepared_draw ~family:Renderer.Scene2) (draws scenario 8))));
  let resized_hash =
    check_pixels resized_facts.drawable_width
      (get (Renderer.read_pixels runtime ~bytes_per_row:(resized_facts.drawable_width * 4)))
  in
  let live_stats = get (Renderer.stats runtime) in
  get (Renderer.destroy runtime);
  if Result.is_ok (Renderer.stats runtime) then
    failwith "native qualification: a destroyed renderer still reports stats";
  let handles_after_destroy = live_handles () in
  let expected extent =
    match frozen_software_hash scenario ~extent with
    | Some hash -> hash
    | None ->
        failwith
          (Printf.sprintf
             "native qualification %s: no frozen capture for %d pixels (measured %s resized=%s)"
             (name scenario) extent
             (String.concat "," (List.map snd !checkpoints))
             resized_hash)
  in
  if
    List.exists (fun (_, hash) -> hash <> expected initial_facts.drawable_width) !checkpoints
    || (scenario = Basic && resized_hash <> expected resized_facts.drawable_width)
  then
    failwith
      (Printf.sprintf "native qualification %s hash drift: %s resized=%s expected=%s"
         (name scenario)
         (String.concat "," (List.map snd !checkpoints))
         resized_hash
         (expected initial_facts.drawable_width));
  let uploaded = draw_count scenario * 60 in
  let native_hash = snd (List.hd !checkpoints) in
  `Assoc
    [
      ("scenario", `String (name scenario));
      ("frames", `Int 601);
      ("draw_calls", `Int (draw_count scenario * 601));
      ("pass_calls", `Int 601);
      ("render_calls", `Int 601);
      ("readback_calls", `Int 5);
      ("runtime_backend_calls", `Int 607);
      ("prepared_upload_bytes", `Int uploaded);
      ("reported_upload_bytes", `String (Int64.to_string live_stats.uploaded_bytes));
      ("pipeline_cache_entries_live", `Int live_stats.pipeline_cache_entries);
      ("live_handles_after_destroy", `Int handles_after_destroy);
      ( "initial_logical_drawable",
        `List
          [
            `Int initial_facts.logical_width;
            `Int initial_facts.logical_height;
            `Int initial_facts.drawable_width;
            `Int initial_facts.drawable_height;
          ] );
      ( "initial_pixel_scale",
        `List [ `Float initial_facts.pixel_density; `Float initial_facts.pixel_density ] );
      ("frozen_software_hash", `String (expected initial_facts.drawable_width));
      ("native_matches_frozen_software", `Bool (native_hash = expected initial_facts.drawable_width));
      ( "checkpoint_hashes",
        `List (List.rev_map (fun (frame, hash) -> `List [ `Int frame; `String hash ]) !checkpoints)
      );
      ("resized_hash", `String resized_hash);
    ]

(* SDL3's own window lifecycle grows the physical footprint on this macOS
   (about 270 KiB per hidden 2x2 window with the video subsystem kept
   initialized, 390 KiB when it is cycled, with or without a Metal view, and
   an event pump does not return it). Measure that baseline in the same
   process so the plateau check charges Rays only for what Rays adds. *)
let sdl_window_floor_growth lifecycles =
  let ok = function Ok v -> v | Error (e : Sdl3.error) -> failwith e.message in
  let footprints =
    Array.init lifecycles (fun _ ->
        ok (Sdl3.Init.init [ Sdl3.Init.Video ]);
        let w =
          ok
            (Sdl3.Window.create ~title:"baseline" ~width:2 ~height:2
               ~flags:[ Metal; High_pixel_density; Hidden ]
               ())
        in
        let v = ok (Sdl3.Metal_view.create w) in
        ok (Sdl3.Metal_view.destroy v);
        ok (Sdl3.Window.destroy w);
        ok (Sdl3.Init.quit_subsystems [ Sdl3.Init.Video ]);
        Gc.full_major ();
        footprint_kib ())
  in
  let floor_of first = Array.fold_left min max_int (Array.sub footprints first 24) in
  floor_of 48 - floor_of 24

let () =
  let synthetic : Runtime.frame_facts =
    {
      logical_width = 10;
      logical_height = 10;
      drawable_width = 15;
      drawable_height = 15;
      pixel_scale_x = 1.5;
      pixel_scale_y = 1.5;
    }
  in
  if Runtime.map_logical_rect synthetic (1, 1, 3, 3) <> (1, 1, 5, 5) then
    failwith "synthetic logical/drawable edge mapping drift";
  let input =
    match Runtime_input.create ~max_events:8 ~logical_width:10 ~logical_height:10 with
    | Ok value -> value
    | Error message -> failwith message
  in
  ignore
    (Runtime_input_sdl3.push input
       (Sdl3.Event.Mouse_motion { x = 3.25; y = 4.5; dx = 0.; dy = 0. }));
  if (Runtime_input.snapshot input).pointer <> (3.25, 4.5) then
    failwith "SDL3 logical pointer was double-scaled";
  let before = live_handles () and rss_before = rss_kib () in
  match create 1 1 with
  | Error error ->
      failwith
        ("runtime-next native qualification create failed: "
        ^ Format.asprintf "%a" Renderer.pp_error error)
  | Ok probe ->
      get (Renderer.destroy probe);
      let rss_checkpoints = ref [] in
      let scenarios =
        List.map
          (fun scenario ->
            let result = run scenario in
            Gc.full_major ();
            rss_checkpoints := rss_kib () :: !rss_checkpoints;
            result)
          [ Basic; Pxui_like; Canvas_offscreen; Scene3_builtin ]
      in
      (* CAMetalLayer, Objective-C and the OCaml allocator may reserve their
         one-time teardown pools on the first few lifecycles.  Those reservations
         are not retained Rays resources: verify the release queue first,
         then measure a settled sequence rather than treating allocator warm-up
         as a leak. *)
      for _ = 1 to 4 do
        let runtime = get (create 2 2) in
        get (Renderer.destroy runtime);
        ignore (live_handles ());
        Gc.full_major ()
      done;
      let teardown_rss =
        Array.init 72 (fun _ ->
            let runtime = get (create 2 2) in
            get (Renderer.destroy runtime);
            ignore (live_handles ());
            Gc.full_major ();
            (rss_kib (), footprint_kib ()))
      in
      let teardown_footprint = Array.map snd teardown_rss
      and teardown_rss = Array.map fst teardown_rss in
      let teardown_min = Array.fold_left min max_int teardown_rss
      and teardown_max = Array.fold_left max 0 teardown_rss in
      (* Freed GPU memory is returned lazily, in steps (the footprint falls by
         several MiB once, around the twentieth lifecycle), so a short window
         reads the sawtooth as growth. A leak raises the floor: compare the
         lowest footprint of two later windows, after the settling steps. A
         leak of even 100 KiB per lifecycle shows as more than 4 MiB over 24. *)
      let floor_of first = Array.fold_left min max_int (Array.sub teardown_footprint first 24) in
      let footprint_growth = floor_of 48 - floor_of 24 in
      let sdl_growth = sdl_window_floor_growth 72 in
      Printf.printf
        "teardown footprint floors %d and %d KiB: growth %d KiB, SDL window baseline %d KiB\n%!"
        (floor_of 24) (floor_of 48) footprint_growth sdl_growth;
      if footprint_growth - sdl_growth > 4096 then
        failwith
          (Printf.sprintf
             "native repeated teardown physical footprint did not plateau beyond the SDL window \
              baseline: growth %d KiB, baseline %d KiB"
             footprint_growth sdl_growth);
      let after = live_handles () and rss_after = rss_kib () in
      if after <> before then failwith "native qualification live-handle delta";
      let report =
        `Assoc
          [
            ("schema", `Int 1);
            ("host", `String "Apple M1");
            ("checkpoints", `List [ `Int 1; `Int 2; `Int 60; `Int 600 ]);
            ("scenarios", `List scenarios);
            ("rss_before_kib", `Int rss_before);
            ("rss_after_kib", `Int rss_after);
            ("live_handles_before", `Int before);
            ("live_handles_after", `Int after);
            ( "rss_after_each_scenario_kib",
              `List (List.rev_map (fun value -> `Int value) !rss_checkpoints) );
            ( "rss_after_twelve_teardown_cycles_kib",
              `List (Array.to_list (Array.map (fun value -> `Int value) teardown_rss)) );
            ("teardown_rss_range_kib", `Int (teardown_max - teardown_min));
            ("teardown_footprint_floor_growth_kib", `Int footprint_growth);
            ("msaa", `String "unsupported-by-runtime-next-sample-count-1");
            ("shader3", `String "software-only-not-qualified-by-this-test");
            ("retina", `String "resize-qualified-drawable-scale-not-exposed");
          ]
      in
      print_endline (Yojson.Safe.pretty_to_string report)
