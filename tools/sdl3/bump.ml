(* dune exec tools/sdl3/bump.exe: record the SDL releases installed here as
   the ones the qualification aliases last passed on.

   Runs @lib/sdl3/qualification, @lib/sdl3_image/qualification,
   @lib/sdl3_ttf/qualification and @lib/sdl3_mixer/qualification against
   whatever pkg-config reports, and only if all four pass rewrites each
   component's [tested] in packaging/sdl3.lock. The diff is one line per
   component that moved. Nothing else changes: [floor] is a deliberate edit,
   and a layout change fails the build before this runs (accept it with
   generate.exe accept). *)

let fail format = Printf.ksprintf failwith format

let command_output program arguments =
  let channel = Unix.open_process_args_in program (Array.of_list (program :: arguments)) in
  let output = In_channel.input_all channel in
  match Unix.close_process_in channel with
  | Unix.WEXITED 0 -> String.trim output
  | _ -> fail "%s failed" (String.concat " " (program :: arguments))

let run program arguments =
  let pid =
    Unix.create_process program
      (Array.of_list (program :: arguments))
      Unix.stdin Unix.stdout Unix.stderr
  in
  match Unix.waitpid [] pid with
  | _, Unix.WEXITED 0 -> ()
  | _ -> fail "%s failed" (String.concat " " (program :: arguments))

let components =
  [
    ("sdl3", "sdl3", "lib/sdl3");
    ("sdl3_image", "sdl3-image", "lib/sdl3_image");
    ("sdl3_ttf", "sdl3-ttf", "lib/sdl3_ttf");
    ("sdl3_mixer", "sdl3-mixer", "lib/sdl3_mixer");
  ]

let () =
  try
    let root =
      match Array.to_list Sys.argv with
      | [ _ ] -> "."
      | [ _; "--root"; root ] -> root
      | _ -> fail "usage: bump.exe [--root <repository>]"
    in
    let lock_path = Filename.concat root "packaging/sdl3.lock" in
    let lock = Sdl3_lock.read lock_path in
    let installed =
      List.map
        (fun (key, package, _) ->
          let version =
            Sdl3_lock.parse_version (command_output "pkg-config" [ "--modversion"; package ])
          in
          if not (Sdl3_lock.stable version) then
            fail "%s %s is a development release" package (Sdl3_lock.version_string version);
          if compare version (Sdl3_lock.find lock key).Sdl3_lock.floor < 0 then
            fail "%s %s is older than the floor in sdl3.lock" package
              (Sdl3_lock.version_string version);
          (key, version))
        components
    in
    run "dune"
      ("build" :: "--root" :: root
      :: List.map (fun (_, _, directory) -> "@" ^ directory ^ "/qualification") components);
    let text =
      List.fold_left
        (fun text (key, version) ->
          if (Sdl3_lock.find lock key).Sdl3_lock.tested = version then text
          else begin
            Printf.printf "%s tested %s -> %s\n%!" key
              (Sdl3_lock.version_string (Sdl3_lock.find lock key).Sdl3_lock.tested)
              (Sdl3_lock.version_string version);
            Sdl3_lock.set_tested text ~key version
          end)
        (Sdl3_lock.read_file lock_path) installed
    in
    Sdl3_lock.write_file lock_path text;
    print_endline "packaging/sdl3.lock is current"
  with Failure message | Sys_error message ->
    prerr_endline message;
    exit 1
