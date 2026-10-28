type mode = Address | Undefined | Leaks

type test = {
  label : string;
  executable : string;
  arguments : string list;
  environment : (string * string) list;
}

type process_result = { status : Unix.process_status; stdout : string; stderr : string }

let fail format = Printf.ksprintf failwith format

let rec remove_directory path =
  Sys.readdir path
  |> Array.iter (fun name ->
      let entry = Filename.concat path name in
      if Sys.is_directory entry then remove_directory entry else Sys.remove entry);
  Unix.rmdir path

let with_temp_directory prefix f =
  let path = Filename.temp_dir prefix "" in
  Fun.protect ~finally:(fun () -> remove_directory path) (fun () -> f path)

let environment_name entry =
  match String.index_opt entry '=' with Some index -> String.sub entry 0 index | None -> entry

let environment_with replacements removals =
  let replaced = List.map fst replacements @ removals in
  let inherited =
    Unix.environment () |> Array.to_list
    |> List.filter (fun entry -> not (List.mem (environment_name entry) replaced))
  in
  Array.of_list (List.map (fun (name, value) -> name ^ "=" ^ value) replacements @ inherited)

let run ~environment program arguments =
  with_temp_directory "rays-memory-command-" (fun directory ->
      let stdout_path = Filename.concat directory "stdout" in
      let stderr_path = Filename.concat directory "stderr" in
      let flags = [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] in
      let stdout_fd = Unix.openfile stdout_path flags 0o600 in
      let stderr_fd = Unix.openfile stderr_path flags 0o600 in
      let pid =
        Fun.protect
          ~finally:(fun () ->
            Unix.close stdout_fd;
            Unix.close stderr_fd)
          (fun () ->
            Unix.create_process_env program
              (Array.of_list (program :: arguments))
              environment Unix.stdin stdout_fd stderr_fd)
      in
      let _, status = Unix.waitpid [] pid in
      { status; stdout = Sdl3_lock.read_file stdout_path; stderr = Sdl3_lock.read_file stderr_path })

let status_string = function
  | Unix.WEXITED code -> Printf.sprintf "exit %d" code
  | Unix.WSIGNALED signal -> Printf.sprintf "signal %d" signal
  | Unix.WSTOPPED signal -> Printf.sprintf "stopped %d" signal

let lowercase = String.lowercase_ascii

let contains ~needle value =
  let needle_length = String.length needle in
  let value_length = String.length value in
  let rec search index =
    index + needle_length <= value_length
    && (String.sub value index needle_length = needle || search (index + 1))
  in
  needle_length = 0 || search 0

let has_nonzero_leak_summary output =
  String.split_on_char '\n' output
  |> List.exists (fun line ->
      contains ~needle:" total leaked bytes" line
      && not (contains ~needle:" 0 total leaked bytes" line))

let reject_output test mode result =
  let output = lowercase (result.stdout ^ "\n" ^ result.stderr) in
  let forbidden =
    match mode with
    | Address ->
        [
          "error: addresssanitizer";
          "addresssanitizer: check failed";
          "leaksanitizer";
          "failed to munmap";
        ]
    | Undefined -> [ "runtime error:"; "undefinedbehaviorsanitizer" ]
    | Leaks -> [ "root leak"; "leak of" ]
  in
  match List.find_opt (fun marker -> contains ~needle:marker output) forbidden with
  | None when mode <> Leaks || not (has_nonzero_leak_summary output) -> ()
  | None ->
      fail "Leaks %s output reports leaked bytes:\n%s%s" test.label result.stdout result.stderr
  | Some marker ->
      fail "%s %s output contains %S:\n%s%s"
        (match mode with Address -> "ASan" | Undefined -> "UBSan" | Leaks -> "Leaks")
        test.label marker result.stdout result.stderr

let existing_executable path =
  if not (Sys.file_exists path && not (Sys.is_directory path)) then
    fail "missing test executable: %s" path;
  Unix.realpath path

let test_matrix ~artifacts ~fixtures =
  let test ?(arguments = []) ?(environment = []) label relative =
    {
      label;
      executable = existing_executable (Filename.concat artifacts relative);
      arguments;
      environment;
    }
  in
  [
    test "core ownership" "lib/sdl3/test_sdl3.exe" ~environment:[ ("SDL_VIDEODRIVER", "dummy") ];
    test "typed events" "lib/sdl3/test_sdl3_events.exe"
      ~environment:[ ("SDL_VIDEODRIVER", "dummy") ];
    test "core constructor failures" "lib/sdl3/test_sdl3_failure.exe"
      ~environment:[ ("SDL_VIDEODRIVER", "rays_missing_video_driver") ];
    test "100000-cycle lifecycle stress" "lib/sdl3/test_sdl3_stress.exe"
      ~environment:[ ("SDL_VIDEODRIVER", "dummy") ];
    test "native CAMetalLayer lifecycle" "lib/sdl3/test_sdl3_metal.exe";
    test "image decoder parity" "lib/sdl3_image/test_sdl3_image.exe" ~arguments:[ fixtures ]
      ~environment:[ ("SDL_VIDEODRIVER", "dummy") ];
    test "font parity" "lib/sdl3_ttf/test_sdl3_ttf.exe"
      ~environment:[ ("SDL_VIDEODRIVER", "dummy") ];
    test "font constructor failure" "lib/sdl3_ttf/test_sdl3_ttf_discovery_failure.exe"
      ~environment:[ ("RAYS_UI_FONT", "/definitely/missing/rays-font.ttf") ];
    test "mixer parity" "lib/sdl3_mixer/test_sdl3_mixer.exe"
      ~environment:[ ("SDL_AUDIODRIVER", "dummy") ];
    test "mixer constructor failure" "lib/sdl3_mixer/test_sdl3_mixer_device_failure.exe"
      ~environment:[ ("SDL_AUDIODRIVER", "rays_missing_audio_driver") ];
  ]

let base_removals =
  [ "ASAN_OPTIONS"; "UBSAN_OPTIONS"; "SDL_VIDEODRIVER"; "SDL_AUDIODRIVER"; "RAYS_UI_FONT" ]

let run_test mode ~asan_suppressions test =
  let program, arguments, replacements =
    match mode with
    | Address ->
        let options =
          [
            "abort_on_error=1";
            "halt_on_error=1";
            "detect_leaks=0";
            "strict_string_checks=1";
            "use_sigaltstack=0";
            "suppressions=" ^ asan_suppressions;
          ]
        in
        ( test.executable,
          test.arguments,
          ("ASAN_OPTIONS", String.concat ":" options) :: test.environment )
    | Undefined ->
        ( test.executable,
          test.arguments,
          ("UBSAN_OPTIONS", "halt_on_error=1:print_stacktrace=1") :: test.environment )
    | Leaks ->
        ( "/usr/bin/leaks",
          [ "--quiet"; "--atExit"; "--"; "/usr/bin/env"; test.executable ] @ test.arguments,
          test.environment )
  in
  let result = run ~environment:(environment_with replacements base_removals) program arguments in
  (match result.status with
  | Unix.WEXITED 0 -> ()
  | status ->
      fail "%s failed with %s:\n%s%s" test.label (status_string status) result.stdout result.stderr);
  reject_output test mode result;
  Printf.printf "%s: passed\n%!" test.label

let mode = ref None
let artifacts = ref None
let fixtures = ref None
let asan_suppressions = ref None

let parse_mode value =
  mode :=
    Some
      (match value with
      | "address" -> Address
      | "undefined" -> Undefined
      | "leaks" -> Leaks
      | _ -> fail "--mode must be address, undefined, or leaks")

let arguments =
  [
    ("--mode", Arg.String parse_mode, "MODE address, undefined, or leaks");
    ( "--artifacts",
      Arg.String (fun value -> artifacts := Some value),
      "DIR Dune context directory containing lib/" );
    ( "--fixtures",
      Arg.String (fun value -> fixtures := Some value),
      "DIR SDL3_image fixture directory" );
    ( "--asan-suppressions",
      Arg.String (fun value -> asan_suppressions := Some value),
      "FILE required targeted system-library suppressions in address mode" );
  ]

let required name = function
  | Some value -> Unix.realpath value
  | None -> fail "%s is required" name

let () =
  try
    Arg.parse arguments
      (fun value -> fail "unexpected argument %S" value)
      "check_sdl3_memory --mode MODE --artifacts DIR --fixtures DIR [--asan-suppressions FILE]";
    let mode = match !mode with Some mode -> mode | None -> fail "--mode is required" in
    let artifacts = required "--artifacts" !artifacts in
    let fixtures = required "--fixtures" !fixtures in
    let asan_suppressions =
      match (mode, !asan_suppressions) with
      | Address, Some path -> Unix.realpath path
      | Address, None -> fail "--asan-suppressions is required in address mode"
      | (Undefined | Leaks), _ -> ""
    in
    if mode = Leaks && not (Sys.file_exists "/usr/bin/leaks") then
      fail "/usr/bin/leaks is unavailable";
    let tests = test_matrix ~artifacts ~fixtures in
    List.iter (run_test mode ~asan_suppressions) tests;
    Printf.printf "%d SDL3 %s memory checks passed\n%!" (List.length tests)
      (match mode with
      | Address -> "AddressSanitizer"
      | Undefined -> "UndefinedBehaviorSanitizer"
      | Leaks -> "Instruments Leaks")
  with Failure message ->
    prerr_endline message;
    exit 1
