module Mixer3 = Sdl3_mixer

let fail format = Printf.ksprintf failwith format

let sdl = function
  | Ok value -> value
  | Error error -> fail "%s" (Format.asprintf "%a" Sdl3.pp_error error)

let mixer = function
  | Ok value -> value
  | Error error -> fail "%s" (Format.asprintf "%a" Mixer3.pp_error error)

type measurement =
  { name : string
  ; iterations : int
  ; ffi_calls : int
  ; wall_seconds : float
  ; allocated_bytes : float
  ; minor_bytes : float
  ; promoted_bytes : float
  ; major_bytes : float
  ; major_collections : int
  ; ending_heap_bytes : int
  ; peak_heap_bytes : int
  }

let gc_bytes words = words *. float_of_int (Sys.word_size / 8)

let measure ~name ~iterations ~calls_per_iteration operation =
  Gc.full_major ();
  let before = Gc.quick_stat () in
  let allocated_before = Gc.allocated_bytes () in
  let started = Unix.gettimeofday () in
  for index = 0 to iterations - 1 do
    operation index
  done;
  let wall_seconds = Unix.gettimeofday () -. started in
  let allocated_bytes = Gc.allocated_bytes () -. allocated_before in
  let after = Gc.quick_stat () in
  { name
  ; iterations
  ; ffi_calls = iterations * calls_per_iteration
  ; wall_seconds
  ; allocated_bytes
  ; minor_bytes = gc_bytes (after.minor_words -. before.minor_words)
  ; promoted_bytes = gc_bytes (after.promoted_words -. before.promoted_words)
  ; major_bytes = gc_bytes (after.major_words -. before.major_words)
  ; major_collections = after.major_collections - before.major_collections
  ; ending_heap_bytes = after.heap_words * (Sys.word_size / 8)
  ; peak_heap_bytes = after.top_heap_words * (Sys.word_size / 8)
  }

let measurement_json value =
  `Assoc
    [ "name", `String value.name
    ; "iterations", `Int value.iterations
    ; "ffi_calls", `Int value.ffi_calls
    ; "wall_seconds", `Float value.wall_seconds
    ; "nanoseconds_per_iteration",
      `Float (value.wall_seconds *. 1e9 /. float_of_int value.iterations)
    ; "allocated_bytes", `Float value.allocated_bytes
    ; "allocated_bytes_per_iteration",
      `Float (value.allocated_bytes /. float_of_int value.iterations)
    ; "minor_bytes", `Float value.minor_bytes
    ; "promoted_bytes", `Float value.promoted_bytes
    ; "major_bytes", `Float value.major_bytes
    ; "major_collections", `Int value.major_collections
    ; "ending_heap_bytes", `Int value.ending_heap_bytes
    ; "peak_heap_bytes", `Int value.peak_heap_bytes
    ]

type arguments =
  { iterations : int
  ; window_iterations : int
  ; audio_iterations : int
  ; decode_iterations : int
  }

let parse_arguments () =
  let iterations = ref 100_000 in
  let window_iterations = ref 100_000 in
  let audio_iterations = ref 100_000 in
  let decode_iterations = ref 20_000 in
  let index = ref 1 in
  let usage () =
    fail
      "usage: %s [--iterations <n>] [--window-iterations <n>] \
       [--audio-iterations <n>] [--decode-iterations <n>]"
      Sys.argv.(0)
  in
  let positive name =
    if !index + 1 >= Array.length Sys.argv then usage ();
    let raw = Sys.argv.(!index + 1) in
    index := !index + 2;
    match int_of_string_opt raw with
    | Some value when value > 0 -> value
    | Some _ | None -> fail "%s must be a positive integer, got %S" name raw
  in
  while !index < Array.length Sys.argv do
    match Sys.argv.(!index) with
    | "--iterations" -> iterations := positive "iterations"
    | "--window-iterations" ->
        window_iterations := positive "window iterations"
    | "--audio-iterations" -> audio_iterations := positive "audio iterations"
    | "--decode-iterations" -> decode_iterations := positive "decode iterations"
    | _ -> usage ()
  done;
  { iterations = !iterations
  ; window_iterations = !window_iterations
  ; audio_iterations = !audio_iterations
  ; decode_iterations = !decode_iterations
  }

let () =
  let arguments = parse_arguments () in
  sdl (Sdl3.Init.init [ Sdl3.Init.Video; Sdl3.Init.Events ]);
  mixer (Mixer3.Init.init ());
  let version =
    measure ~name:"version_query" ~iterations:arguments.iterations
      ~calls_per_iteration:1 (fun _ -> ignore (Sdl3.linked_version ()))
  in
  let events =
    measure ~name:"event_poll" ~iterations:arguments.iterations
      ~calls_per_iteration:1 (fun _ -> ignore (sdl (Sdl3.Event.poll_coalesced ())))
  in
  let windows =
    measure ~name:"hidden_window_create_query_destroy"
      ~iterations:arguments.window_iterations ~calls_per_iteration:3 (fun index ->
        let window =
          sdl
            (Sdl3.Window.create
               ~title:(Printf.sprintf "Rays SDL3 bench %d" index)
               ~width:16 ~height:16 ~flags:[ Sdl3.Window.Hidden ] ())
        in
        ignore (sdl (Sdl3.Window.size_in_pixels window));
        sdl (Sdl3.Window.destroy window))
  in
  (* 80 samples of 8 kHz mono PCM: the smallest sound the track cycle plays *)
  let wav =
    let samples = 80 in
    let bytes = Bytes.make (44 + (samples * 2)) '\000' in
    let u16 offset value = Bytes.set_uint16_le bytes offset value in
    let u32 offset value = Bytes.set_int32_le bytes offset (Int32.of_int value) in
    Bytes.blit_string "RIFF" 0 bytes 0 4; u32 4 (36 + (samples * 2));
    Bytes.blit_string "WAVEfmt " 0 bytes 8 8; u32 16 16; u16 20 1; u16 22 1;
    u32 24 8_000; u32 28 16_000; u16 32 2; u16 34 16;
    Bytes.blit_string "data" 0 bytes 36 4; u32 40 (samples * 2);
    for sample = 0 to samples - 1 do
      u16 (44 + (sample * 2)) (if sample mod 16 < 8 then 8_000 else 0xE0C0)
    done;
    bytes
  in
  let audio =
    measure ~name:"memory_audio_track_cycle"
      ~iterations:arguments.audio_iterations ~calls_per_iteration:10 (fun _ ->
        let mixer_value =
          mixer (Mixer3.Mixer.create_memory ~sample_rate:8_000 ~channels:1)
        in
        let audio_value = mixer (Mixer3.Audio.load_bytes mixer_value wav) in
        let track = mixer (Mixer3.Music.create mixer_value) in
        mixer (Mixer3.Music.set_audio track audio_value);
        mixer (Mixer3.Music.play track ());
        ignore (mixer (Mixer3.Mixer.generate mixer_value ~frames:8));
        mixer (Mixer3.Music.stop track ());
        mixer (Mixer3.Music.destroy track);
        mixer (Mixer3.Audio.destroy audio_value);
        mixer (Mixer3.Mixer.destroy mixer_value))
  in
  (* The pixels a decoded image or a rendered text label hand the resource
     layer: the native decode into the caller's buffer. *)
  let png = "lib/sdl3_image/fixtures/sample.png" in
  let decode =
    measure ~name:"image_decode_png" ~iterations:arguments.decode_iterations
      ~calls_per_iteration:1 (fun _ ->
        match Sdl3_image.load_file png with
        | Ok _ -> ()
        | Error error -> fail "%s" (Format.asprintf "%a" Sdl3_image.pp_error error))
  in
  let ttf = function
    | Ok value -> value
    | Error error -> fail "%s" (Format.asprintf "%a" Sdl3_ttf.pp_error error)
  in
  ttf (Sdl3_ttf.Init.init ());
  let font =
    ttf (Sdl3_ttf.Font.open_file ~path:(ttf (Sdl3_ttf.Font.system_path ())) ~size:18.)
  in
  let text =
    measure ~name:"text_render_rgba" ~iterations:arguments.decode_iterations
      ~calls_per_iteration:1 (fun _ ->
        match ttf (Sdl3_ttf.Font.render_blended font ~color:(255, 255, 255, 255) "Rays flow 123") with
        | None -> fail "empty render"
        | Some _ -> ())
  in
  ttf (Sdl3_ttf.Font.destroy font);
  ttf (Sdl3_ttf.Init.quit ());
  mixer (Mixer3.Init.quit ());
  sdl (Sdl3.Init.quit_subsystems [ Sdl3.Init.Video; Sdl3.Init.Events ]);
  let linked = Sdl3.linked_version () in
  let mixer_linked = Mixer3.linked_version () in
  let output =
    `Assoc
      [ "schema", `Int 1
      ; "benchmark", `String "sdl3_ffi"
      ; "ocaml_version", `String Sys.ocaml_version
      ; "word_size", `Int Sys.word_size
      ; ( "sdl3_version"
        , `String
            (Printf.sprintf "%d.%d.%d" linked.major linked.minor linked.patch) )
      ; ( "sdl3_mixer_version"
        , `String
            (Printf.sprintf "%d.%d.%d" mixer_linked.major mixer_linked.minor
               mixer_linked.patch) )
      ; ( "measurements"
        , `List
            (List.map measurement_json
               [ version; events; windows; audio; decode; text ]) )
      ]
  in
  print_endline (Yojson.Safe.to_string output)
