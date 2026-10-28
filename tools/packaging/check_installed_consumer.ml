type process_result = { status : Unix.process_status; stdout : string; stderr : string }

let fail format = Printf.ksprintf failwith format

let rec remove_directory path =
  Sys.readdir path
  |> Array.iter (fun name ->
      let entry = Filename.concat path name in
      if Sys.is_directory entry then remove_directory entry else Sys.remove entry);
  Unix.rmdir path

let with_temp_directory prefix f =
  let path = Filename.temp_dir prefix "" |> Unix.realpath in
  Fun.protect ~finally:(fun () -> remove_directory path) (fun () -> f path)

let with_source_build_directory root f =
  let path = Filename.temp_file ~temp_dir:root "_build-packaging-check-" "" in
  Sys.remove path;
  Unix.mkdir path 0o755;
  let relative = Filename.basename path in
  Fun.protect ~finally:(fun () -> remove_directory path) (fun () -> f ~path ~relative)

let environment_name entry =
  match String.index_opt entry '=' with Some index -> String.sub entry 0 index | None -> entry

let environment_with replacements =
  let replaced = List.map fst replacements in
  let inherited =
    Unix.environment () |> Array.to_list
    |> List.filter (fun entry -> not (List.mem (environment_name entry) replaced))
  in
  Array.of_list (List.map (fun (name, value) -> name ^ "=" ^ value) replacements @ inherited)

let run ?environment ~directory program arguments =
  with_temp_directory "rays-packaging-command-" (fun output_directory ->
      let stdout_path = Filename.concat output_directory "stdout" in
      let stderr_path = Filename.concat output_directory "stderr" in
      let flags = [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] in
      let stdout_fd = Unix.openfile stdout_path flags 0o600 in
      let stderr_fd = Unix.openfile stderr_path flags 0o600 in
      let previous = Sys.getcwd () in
      let process_environment =
        match environment with Some environment -> environment | None -> Unix.environment ()
      in
      let pid =
        Fun.protect
          ~finally:(fun () ->
            Unix.chdir previous;
            Unix.close stdout_fd;
            Unix.close stderr_fd)
          (fun () ->
            Unix.chdir directory;
            Unix.create_process_env program
              (Array.of_list (program :: arguments))
              process_environment Unix.stdin stdout_fd stderr_fd)
      in
      let _, status = Unix.waitpid [] pid in
      { status; stdout = Sdl3_lock.read_file stdout_path; stderr = Sdl3_lock.read_file stderr_path })

let status_string = function
  | Unix.WEXITED code -> Printf.sprintf "exit %d" code
  | Unix.WSIGNALED signal -> Printf.sprintf "signal %d" signal
  | Unix.WSTOPPED signal -> Printf.sprintf "stopped %d" signal

let command ?environment ~directory program arguments =
  let result = run ?environment ~directory program arguments in
  match result.status with
  | Unix.WEXITED 0 -> String.trim result.stdout
  | status ->
      fail "%s %s failed with %s:\n%s%s" program (String.concat " " arguments)
        (status_string status) result.stdout result.stderr

let starts_with ~prefix value =
  let prefix_length = String.length prefix in
  String.length value >= prefix_length && String.sub value 0 prefix_length = prefix

let install_and_check ~root ~profile =
  with_temp_directory "rays-installed-consumer-" (fun temporary ->
      with_source_build_directory root (fun ~path:_ ~relative:source_build ->
          let prefix = Filename.concat temporary "prefix" in
          let consumer = Filename.concat temporary "consumer" in
          Unix.mkdir prefix 0o755;
          Unix.mkdir consumer 0o755;
          ignore
            (command ~directory:root "dune"
               [
                 "build";
                 "--root";
                 root;
                 "--build-dir";
                 source_build;
                 "--profile";
                 profile;
                 "@install";
               ]);
          ignore
            (command ~directory:root "dune"
               [
                 "install";
                 "--root";
                 root;
                 "--build-dir";
                 source_build;
                 "--prefix";
                 prefix;
                 "--relocatable";
                 "--profile";
                 profile;
                 "rays";
               ]);
          Sdl3_lock.write_file
            (Filename.concat consumer "dune-project")
            "(lang dune 3.17)\n(name rays_sdl3_installed_consumer)\n";
          Sdl3_lock.write_file (Filename.concat consumer "dune")
            "(executable\n\
            \ (name main)\n\
            \ (libraries rays.sdl3 rays.sdl3_image rays.sdl3_ttf rays.sdl3_mixer))\n";
          Sdl3_lock.write_file
            (Filename.concat consumer "main.ml")
            (Sdl3_lock.read_file (Filename.concat root "tools/packaging/installed_consumer.ml"));
          let library_path = Filename.concat prefix "lib" in
          let stublib_path = Filename.concat library_path "stublibs" in
          let inherited_stublibs =
            match Sys.getenv_opt "CAML_LD_LIBRARY_PATH" with
            | Some value when value <> "" -> ":" ^ value
            | Some _ | None -> ""
          in
          let environment =
            environment_with
              [
                ("OCAMLPATH", library_path);
                ("CAML_LD_LIBRARY_PATH", stublib_path ^ inherited_stublibs);
              ]
          in
          let packages = [ "rays.sdl3"; "rays.sdl3_image"; "rays.sdl3_ttf"; "rays.sdl3_mixer" ] in
          List.iter
            (fun package ->
              let resolved =
                command ~environment ~directory:consumer "ocamlfind" [ "query"; package ]
                |> Unix.realpath
              in
              if not (starts_with ~prefix:(Unix.realpath library_path ^ "/") resolved) then
                fail "%s resolved outside the isolated prefix: %s" package resolved;
              if starts_with ~prefix:(root ^ "/") resolved then
                fail "%s resolved through the source checkout: %s" package resolved)
            packages;
          ignore
            (command ~environment ~directory:consumer "dune"
               [ "build"; "--profile"; profile; "main.exe" ]);
          let executable = Filename.concat consumer "_build/default/main.exe" in
          let output = command ~environment ~directory:consumer executable [] in
          if output <> "installed SDL3 consumer passed" then
            fail "installed consumer returned unexpected output: %S" output;
          Printf.printf "isolated %s install and external SDL3 consumer passed at %s\n%!" profile
            prefix))

let root = ref None
let profile = ref "dev"

let arguments =
  [
    ("--root", Arg.String (fun value -> root := Some value), "PATH Rays checkout root");
    ("--profile", Arg.Set_string profile, "PROFILE Dune profile (default: dev)");
  ]

let () =
  try
    Arg.parse arguments
      (fun value -> fail "unexpected argument %S" value)
      "check_installed_consumer --root PATH [--profile dev|release]";
    let root =
      match !root with Some path -> Unix.realpath path | None -> fail "--root is required"
    in
    if not (List.mem !profile [ "dev"; "release" ]) then fail "--profile must be dev or release";
    install_and_check ~root ~profile:!profile
  with Failure message ->
    prerr_endline message;
    exit 1
