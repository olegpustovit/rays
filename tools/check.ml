(* Launch the built executable directly: bootstrapping through [dune exec]
   would acquire Dune's lock before we can queue behind another agent. *)
let rec workspace path =
  if Sys.file_exists (Filename.concat path "dune-project") then path
  else
    let parent = Filename.dirname path in
    if parent = path then failwith "check: run inside the Rays workspace"
    else workspace parent

(* [limit] seconds is a hard stop: the child gets SIGINT (dune stops its tests), then SIGKILL after 5 s.
   Exit 124 says it was cut, not that a test failed. *)
let run ?limit program arguments =
  let pid = Unix.create_process program (Array.of_list (program :: arguments))
      Unix.stdin Unix.stdout Unix.stderr in
  let start = Unix.gettimeofday () in
  let rec wait interrupted =
    match Unix.waitpid [Unix.WNOHANG] pid with
    | 0, _ ->
        let elapsed = Unix.gettimeofday () -. start in
        (match limit with
         | Some limit when interrupted = None && elapsed > limit ->
             Printf.eprintf "check: stopped after %.0f s (RAYS_CHECK_TIMEOUT); the slow test is the last one running\n%!" limit;
             Unix.kill pid Sys.sigint; wait (Some elapsed)
         | _ ->
             (match interrupted with
              | Some at when elapsed -. at > 5. -> Unix.kill pid Sys.sigkill
              | _ -> ());
             Unix.sleepf 0.2; wait interrupted)
    | _, status -> status
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> wait interrupted in
  let status = wait None in
  let cut = Option.is_some limit && Unix.gettimeofday () -. start > Option.get limit in
  match status with
  | _ when cut -> 124
  | Unix.WEXITED code -> code
  | Unix.WSIGNALED _ | Unix.WSTOPPED _ -> 1

let () =
  Sys.chdir (workspace (Sys.getcwd ()));
  let arguments = Array.to_list Sys.argv |> List.tl in
  let ship = arguments = ["--ship"] in
  let arguments = if ship then ["@all"; "@runtest"; "@smoke"]
    else if arguments = [] then ["@runtest"] else arguments in
  (* Keep this inode outside _build and never unlink it: queued processes
     must keep locking the same file even across [dune clean]. *)
  let lock = Unix.openfile ".rays-check.lock"
      [Unix.O_CREAT; Unix.O_RDWR; Unix.O_CLOEXEC] 0o600 in
  let code = Fun.protect ~finally:(fun () -> Unix.close lock) (fun () ->
    (try Unix.lockf lock Unix.F_TLOCK 0 with
     | Unix.Unix_error ((Unix.EACCES | Unix.EAGAIN), _, _) ->
         prerr_endline "check: waiting for another agent's validation";
         Unix.lockf lock Unix.F_LOCK 0);
    (* a validation is cut at 3 minutes (RAYS_CHECK_TIMEOUT, seconds; 0 for none); --ship has no cut *)
    let limit = match Sys.getenv_opt "RAYS_CHECK_TIMEOUT" with
      | Some "0" -> None
      | Some seconds -> float_of_string_opt seconds
      | None -> if ship then None else Some 180. in
    let code = run ?limit "dune" ("build" :: "--root" :: "." :: arguments) in
    if code = 0 && ship then run "git" ["diff"; "--check"] else code) in
  exit code
