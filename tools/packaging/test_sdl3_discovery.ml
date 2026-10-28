type component = {
  executable : string;
  key : string;
  package : string;
  library : string;
  environment_prefix : string;
  private_library : string;
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

let mkdir path = if not (Sys.file_exists path) then Unix.mkdir path 0o755

let environment_name entry =
  match String.index_opt entry '=' with Some index -> String.sub entry 0 index | None -> entry

let environment_with replacements removals =
  let replaced = List.map fst replacements @ removals in
  let inherited =
    Unix.environment () |> Array.to_list
    |> List.filter (fun entry -> not (List.mem (environment_name entry) replaced))
  in
  Array.of_list (List.map (fun (name, value) -> name ^ "=" ^ value) replacements @ inherited)

let run ~directory ~environment executable =
  let stdout_path = Filename.concat directory "stdout" in
  let stderr_path = Filename.concat directory "stderr" in
  let flags = [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] in
  let stdout_fd = Unix.openfile stdout_path flags 0o600 in
  let stderr_fd = Unix.openfile stderr_path flags 0o600 in
  let previous = Sys.getcwd () in
  let pid =
    Fun.protect
      ~finally:(fun () ->
        Unix.chdir previous;
        Unix.close stdout_fd;
        Unix.close stderr_fd)
      (fun () ->
        Unix.chdir directory;
        Unix.create_process_env executable [| executable |] environment Unix.stdin stdout_fd
          stderr_fd)
  in
  let _, status = Unix.waitpid [] pid in
  { status; stdout = Sdl3_lock.read_file stdout_path; stderr = Sdl3_lock.read_file stderr_path }

let status_string = function
  | Unix.WEXITED code -> Printf.sprintf "exit %d" code
  | Unix.WSIGNALED signal -> Printf.sprintf "signal %d" signal
  | Unix.WSTOPPED signal -> Printf.sprintf "stopped %d" signal

let expect_success component result =
  match result.status with
  | Unix.WEXITED 0 -> ()
  | status ->
      fail "%s discovery failed with %s:\n%s%s" component.package (status_string status)
        result.stdout result.stderr

let expect_failure result =
  match result.status with
  | Unix.WEXITED 0 -> fail "discovery unexpectedly succeeded"
  | Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _ -> ()

let atoms contents =
  let length = String.length contents in
  let rec quoted buffer index =
    if index >= length then fail "unterminated quoted S-expression atom";
    match contents.[index] with
    | '"' -> (Buffer.contents buffer, index + 1)
    | '\\' when index + 1 < length ->
        Buffer.add_char buffer contents.[index + 1];
        quoted buffer (index + 2)
    | character ->
        Buffer.add_char buffer character;
        quoted buffer (index + 1)
  in
  let rec bare buffer index =
    if index >= length || List.mem contents.[index] [ ' '; '\t'; '\r'; '\n'; '('; ')' ] then
      (Buffer.contents buffer, index)
    else (
      Buffer.add_char buffer contents.[index];
      bare buffer (index + 1))
  in
  let rec loop result index =
    if index >= length then List.rev result
    else
      match contents.[index] with
      | ' ' | '\t' | '\r' | '\n' | '(' | ')' -> loop result (index + 1)
      | '"' ->
          let atom, next = quoted (Buffer.create 32) (index + 1) in
          loop (atom :: result) next
      | _ ->
          let atom, next = bare (Buffer.create 32) index in
          loop (atom :: result) next
  in
  loop [] 0

let output_flags directory name = Sdl3_lock.read_file (Filename.concat directory name) |> atoms

let require_flag label flag flags =
  if not (List.mem flag flags) then fail "%s lacks %S in [%s]" label flag (String.concat "; " flags)

let reject_flag label flag flags =
  if List.mem flag flags then
    fail "%s unexpectedly contains %S in [%s]" label flag (String.concat "; " flags)

let contains ~needle value =
  let needle_length = String.length needle in
  let value_length = String.length value in
  let rec search index =
    index + needle_length <= value_length
    && (String.sub value index needle_length = needle || search (index + 1))
  in
  needle_length = 0 || search 0

(* What the fake pkg-config reports: two patch releases above the floor, so the
   default run proves a newer SDL than the lock describes stays green. *)
let fake_version lock component =
  let major, minor, patch = (Sdl3_lock.find lock component.key).Sdl3_lock.floor in
  (major, minor, patch + 2)

let package_file ~root ~lock component =
  let include_directory = Filename.concat root (component.package ^ "-include") in
  let library_directory = Filename.concat root (component.package ^ "-lib") in
  mkdir include_directory;
  mkdir library_directory;
  Sdl3_lock.write_file (Filename.concat library_directory ("lib" ^ component.library ^ ".a")) "";
  let requires = if component.package = "sdl3" then "" else "Requires: sdl3\n" in
  let contents =
    Printf.sprintf
      "prefix=%s\n\
       includedir=%s\n\
       libdir=%s\n\n\
       Name: %s\n\
       Description: Rays hermetic discovery fixture\n\
       Version: %s\n\
       %sLibs: -L${libdir} -l%s\n\
       Libs.private: -l%s\n\
       Cflags: -I${includedir}\n"
      root include_directory library_directory component.package
      (Sdl3_lock.version_string (fake_version lock component))
      requires component.library component.private_library
  in
  Sdl3_lock.write_file (Filename.concat root (component.package ^ ".pc")) contents;
  (include_directory, library_directory)

let run_case_full ~root ~mode ?sanitizers ?include_override ?library_override component =
  with_temp_directory
    ("rays-discover-" ^ component.package ^ "-")
    (fun directory ->
      let replacements =
        [ ("PKG_CONFIG_PATH", root); ("PKG_CONFIG_LIBDIR", root); ("RAYS_SDL3_LINK_MODE", mode) ]
        @ (match sanitizers with Some value -> [ ("RAYS_SDL3_SANITIZERS", value) ] | None -> [])
        @ (match include_override with
          | Some value -> [ (component.environment_prefix ^ "_INCLUDE_DIR", value) ]
          | None -> [])
        @
        match library_override with
        | Some value -> [ (component.environment_prefix ^ "_LIB_DIR", value) ]
        | None -> []
      in
      let removals =
        [
          "PKG_CONFIG";
          "PKG_CONFIG_ARGN";
          "RAYS_SDL3_SANITIZERS";
          "RAYS_SDL3_INCLUDE_DIR";
          "RAYS_SDL3_LIB_DIR";
          "RAYS_SDL3_IMAGE_INCLUDE_DIR";
          "RAYS_SDL3_IMAGE_LIB_DIR";
          "RAYS_SDL3_TTF_INCLUDE_DIR";
          "RAYS_SDL3_TTF_LIB_DIR";
          "RAYS_SDL3_MIXER_INCLUDE_DIR";
          "RAYS_SDL3_MIXER_LIB_DIR";
        ]
      in
      let result =
        run ~directory ~environment:(environment_with replacements removals) component.executable
      in
      expect_success component result;
      ( output_flags directory "c_flags.sexp",
        output_flags directory "c_library_flags.sexp",
        Sdl3_lock.read_file (Filename.concat directory "sdl3_probed.h") ))

let run_case ~root ~mode ?sanitizers ?include_override ?library_override component =
  let cflags, libraries, _ =
    run_case_full ~root ~mode ?sanitizers ?include_override ?library_override component
  in
  (cflags, libraries)

let test_dynamic ~lock root fixtures components =
  List.iter2
    (fun component (include_directory, library_directory) ->
      let cflags, libraries, probed = run_case_full ~root ~mode:"dynamic" component in
      let expected =
        Printf.sprintf "#define RAYS_SDL3_PROBED_VERSION %d\n"
          (Sdl3_lock.version_number (fake_version lock component))
      in
      if probed <> expected then
        fail "%s probed version header was %S, expected %S" component.package probed expected;
      require_flag (component.package ^ " dynamic cflags") ("-I" ^ include_directory) cflags;
      require_flag (component.package ^ " dynamic libraries") ("-L" ^ library_directory) libraries;
      require_flag (component.package ^ " dynamic libraries") ("-l" ^ component.library) libraries;
      reject_flag
        (component.package ^ " dynamic libraries")
        ("-l" ^ component.private_library)
        libraries;
      if component.package <> "sdl3" then
        reject_flag (component.package ^ " dynamic libraries") "-lSDL3" libraries)
    components fixtures

let test_static root fixtures components =
  List.iter2
    (fun component (_, library_directory) ->
      let _, libraries = run_case ~root ~mode:"static" component in
      require_flag
        (component.package ^ " static libraries")
        (Filename.concat library_directory ("lib" ^ component.library ^ ".a"))
        libraries;
      require_flag
        (component.package ^ " static libraries")
        ("-l" ^ component.private_library)
        libraries;
      if component.package <> "sdl3" then (
        reject_flag
          (component.package ^ " static libraries")
          (Filename.concat (snd (List.hd fixtures)) "libSDL3.a")
          libraries;
        reject_flag (component.package ^ " static libraries") "-lcore_fixture" libraries))
    components fixtures

let test_overrides root components =
  List.iter
    (fun component ->
      (* Explicit headers may describe another install than pkg-config does,
         so no version is claimed for them. *)
      let _, _, probed =
        let include_directory = Filename.concat root (component.package ^ "-probe-i") in
        mkdir include_directory;
        run_case_full ~root ~mode:"dynamic" ~include_override:include_directory component
      in
      if probed <> "#define RAYS_SDL3_PROBED_VERSION 0\n" then
        fail "%s explicit headers still claimed a probed version: %S" component.package probed;
      let include_directory = Filename.concat root (component.package ^ "-override-i") in
      let library_directory = Filename.concat root (component.package ^ "-override-l") in
      mkdir include_directory;
      mkdir library_directory;
      Sdl3_lock.write_file (Filename.concat library_directory ("lib" ^ component.library ^ ".a")) "";
      let cflags, libraries =
        run_case ~root ~mode:"dynamic" ~include_override:include_directory
          ~library_override:library_directory component
      in
      (match cflags with
      | first :: _ when first = "-I" ^ include_directory -> ()
      | _ -> fail "%s explicit include override did not take precedence" component.package);
      require_flag
        (component.package ^ " explicit dynamic libraries")
        ("-L" ^ library_directory) libraries;
      let _, static_libraries =
        run_case ~root ~mode:"static" ~library_override:library_directory component
      in
      require_flag
        (component.package ^ " explicit static libraries")
        (Filename.concat library_directory ("lib" ^ component.library ^ ".a"))
        static_libraries)
    components

let test_sanitizers root component =
  let cflags, libraries = run_case ~root ~mode:"dynamic" ~sanitizers:"address" component in
  require_flag "address sanitizer C flags" "-fsanitize=address" cflags;
  require_flag "address sanitizer C flags" "-fno-omit-frame-pointer" cflags;
  require_flag "address sanitizer link flags" "-fsanitize=address" libraries;
  let cflags, libraries =
    run_case ~root ~mode:"dynamic" ~sanitizers:"undefined,address" component
  in
  require_flag "combined sanitizer C flags" "-fsanitize=address,undefined" cflags;
  require_flag "combined sanitizer C flags" "-fno-sanitize-recover=undefined" cflags;
  require_flag "combined sanitizer link flags" "-fsanitize=address,undefined" libraries;
  require_flag "combined sanitizer link flags" "-fno-sanitize-recover=undefined" libraries

let test_invalid_mode root component =
  with_temp_directory "rays-discover-invalid-" (fun directory ->
      let environment =
        environment_with
          [
            ("PKG_CONFIG_PATH", root); ("PKG_CONFIG_LIBDIR", root); ("RAYS_SDL3_LINK_MODE", "hybrid");
          ]
          [ "PKG_CONFIG"; "PKG_CONFIG_ARGN" ]
      in
      let result = run ~directory ~environment component.executable in
      expect_failure result;
      if not (contains ~needle:"must be dynamic or static" result.stderr) then
        fail "invalid-mode diagnostic was not preserved: %s" result.stderr)

let usage () =
  fail "usage: %s <sdl3.lock> <core-discover> <image-discover> <ttf-discover> <mixer-discover>"
    Sys.argv.(0)

let () =
  try
    if Array.length Sys.argv <> 6 then usage ();
    let lock = Sdl3_lock.read Sys.argv.(1) in
    let executable index = Unix.realpath Sys.argv.(index + 1) in
    let components =
      [
        {
          executable = executable 1;
          key = "sdl3";
          package = "sdl3";
          library = "SDL3";
          environment_prefix = "RAYS_SDL3";
          private_library = "core_fixture";
        };
        {
          executable = executable 2;
          key = "sdl3_image";
          package = "sdl3-image";
          library = "SDL3_image";
          environment_prefix = "RAYS_SDL3_IMAGE";
          private_library = "image_fixture";
        };
        {
          executable = executable 3;
          key = "sdl3_ttf";
          package = "sdl3-ttf";
          library = "SDL3_ttf";
          environment_prefix = "RAYS_SDL3_TTF";
          private_library = "ttf_fixture";
        };
        {
          executable = executable 4;
          key = "sdl3_mixer";
          package = "sdl3-mixer";
          library = "SDL3_mixer";
          environment_prefix = "RAYS_SDL3_MIXER";
          private_library = "mixer_fixture";
        };
      ]
    in
    with_temp_directory "rays-pkg-config-" (fun root ->
        let root = Unix.realpath root in
        let fixtures = List.map (package_file ~root ~lock) components in
        test_dynamic ~lock root fixtures components;
        test_static root fixtures components;
        test_overrides root components;
        test_sanitizers root (List.hd components);
        test_invalid_mode root (List.hd components));
    Printf.printf
      "SDL3 core/image/ttf/mixer dynamic, static, explicit-path, and sanitizer discovery passed\n%!"
  with Failure message ->
    prerr_endline message;
    exit 1
