module C = Configurator.V1

type package = { name : string; header : string; header_version : string; linked_version : string }

let packages =
  [
    {
      name = "sdl3";
      header = "SDL3/SDL.h";
      header_version = "SDL_MAJOR_VERSION, SDL_MINOR_VERSION, SDL_MICRO_VERSION";
      linked_version = "SDL_GetVersion()";
    };
    {
      name = "sdl3-image";
      header = "SDL3_image/SDL_image.h";
      header_version = "SDL_IMAGE_MAJOR_VERSION, SDL_IMAGE_MINOR_VERSION, SDL_IMAGE_MICRO_VERSION";
      linked_version = "IMG_Version()";
    };
    {
      name = "sdl3-ttf";
      header = "SDL3_ttf/SDL_ttf.h";
      header_version = "SDL_TTF_MAJOR_VERSION, SDL_TTF_MINOR_VERSION, SDL_TTF_MICRO_VERSION";
      linked_version = "TTF_Version()";
    };
    {
      name = "sdl3-mixer";
      header = "SDL3_mixer/SDL_mixer.h";
      header_version = "SDL_MIXER_MAJOR_VERSION, SDL_MIXER_MINOR_VERSION, SDL_MIXER_MICRO_VERSION";
      linked_version = "MIX_Version()";
    };
  ]

let fail format = Printf.ksprintf failwith format
let read_file = Sdl3_lock.read_file

let rec remove_directory path =
  Sys.readdir path
  |> Array.iter (fun name ->
      let entry = Filename.concat path name in
      if Sys.is_directory entry then remove_directory entry else Sys.remove entry);
  Unix.rmdir path

let with_temp_directory prefix f =
  let path = Filename.temp_dir prefix "" in
  Fun.protect ~finally:(fun () -> remove_directory path) (fun () -> f path)

let command_text program arguments =
  with_temp_directory "rays-command-" (fun directory ->
      let stdout_path = Filename.concat directory "stdout" in
      let stderr_path = Filename.concat directory "stderr" in
      let output_flags = [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] in
      let stdout_fd = Unix.openfile stdout_path output_flags 0o600 in
      let stderr_fd = Unix.openfile stderr_path output_flags 0o600 in
      let pid =
        Fun.protect
          ~finally:(fun () ->
            Unix.close stdout_fd;
            Unix.close stderr_fd)
          (fun () ->
            Unix.create_process_env program
              (Array.of_list (program :: arguments))
              (Unix.environment ()) Unix.stdin stdout_fd stderr_fd)
      in
      let _, status = Unix.waitpid [] pid in
      let stdout = String.trim (read_file stdout_path) in
      let stderr = String.trim (read_file stderr_path) in
      match status with
      | Unix.WEXITED 0 -> stdout
      | Unix.WEXITED code ->
          fail "%s exited %d: %s"
            (String.concat " " (program :: arguments))
            code
            (if stderr = "" then stdout else stderr)
      | Unix.WSIGNALED signal -> fail "%s was killed by signal %d" program signal
      | Unix.WSTOPPED signal -> fail "%s was stopped by signal %d" program signal)

let parse_version = Sdl3_lock.parse_version
let version_string = Sdl3_lock.version_string
let stable = Sdl3_lock.stable

let words value =
  String.split_on_char ' ' value
  |> List.concat_map (String.split_on_char '\n')
  |> List.concat_map (String.split_on_char '\t')
  |> List.filter (fun value -> value <> "")

let find_package name =
  match List.find_opt (fun package -> package.name = name) packages with
  | Some package -> package
  | None ->
      fail "unknown package %S (expected %s)" name
        (String.concat ", " (List.map (fun package -> package.name) packages))

let compiler () =
  match Sys.getenv_opt "CC" with
  | Some value when String.trim value <> "" -> (
      match words value with
      | [ compiler ] -> compiler
      | _ -> fail "CC must name one compiler executable, got %S" value)
  | Some _ | None -> "cc"

let source package =
  Printf.sprintf
    "#include <%s>\n\
     #include <stdio.h>\n\
     int main(void) {\n\
     int linked = %s;\n\
     printf(\"%%d.%%d.%%d %%d.%%d.%%d\\n\",\n\
     %s,\n\
     linked / 1000000, (linked / 1000) %% 1000, linked %% 1000);\n\
     return 0;\n\
     }\n"
    package.header package.linked_version package.header_version

let write_file = Sdl3_lock.write_file

let validate package minimum =
  let config = C.create "check-sdl3-conf" in
  let pkg_config =
    match C.Pkg_config.get config with
    | Some pkg_config -> pkg_config
    | None -> fail "pkg-config is unavailable"
  in
  let flags =
    match C.Pkg_config.query pkg_config ~package:package.name with
    | Some flags -> flags
    | None -> fail "pkg-config package %S is unavailable" package.name
  in
  let pkg_version = command_text "pkg-config" [ "--modversion"; package.name ] |> parse_version in
  if compare pkg_version minimum < 0 then
    fail "%s %s is older than required %s" package.name (version_string pkg_version)
      (version_string minimum);
  if not (stable pkg_version) then
    fail "%s %s is a development release" package.name (version_string pkg_version);
  let versions =
    with_temp_directory
      ("rays-conf-" ^ package.name ^ "-")
      (fun directory ->
        let source_path = Filename.concat directory "probe.c" in
        let executable = Filename.concat directory "probe" in
        write_file source_path (source package);
        ignore
          (command_text (compiler ())
             (flags.cflags @ [ source_path ] @ flags.libs @ [ "-o"; executable ]));
        command_text executable [] |> words)
  in
  let header_version, linked_version =
    match versions with
    | [ header_version; linked_version ] ->
        (parse_version header_version, parse_version linked_version)
    | _ -> fail "%s probe returned malformed versions: %s" package.name (String.concat " " versions)
  in
  let same_series (major, minor, _) (other_major, other_minor, _) =
    major = other_major && minor = other_minor
  in
  if
    compare header_version minimum < 0
    || compare linked_version header_version < 0
    || not (same_series linked_version header_version)
  then
    fail "%s header/runtime mismatch: header %s, linked %s" package.name
      (version_string header_version) (version_string linked_version);
  if not (stable header_version && stable linked_version) then
    fail "%s header/runtime includes a development release: %s/%s" package.name
      (version_string header_version) (version_string linked_version);
  Printf.printf "%s pkg-config/compile/link/runtime probe passed: header %s, linked %s\n%!"
    package.name (version_string header_version) (version_string linked_version)

let usage () =
  let names = String.concat "|" (List.map (fun package -> package.name) packages) in
  fail "usage: %s --lock <sdl3.lock> <%s>" Sys.argv.(0) names

let () =
  try
    if Array.length Sys.argv <> 4 || Sys.argv.(1) <> "--lock" then usage ();
    let package = find_package Sys.argv.(3) in
    let lock = Sdl3_lock.read Sys.argv.(2) in
    let entry = Sdl3_lock.find lock (Sdl3_lock.key_of_package package.name) in
    validate package entry.Sdl3_lock.floor
  with Failure message ->
    prerr_endline message;
    exit 1
