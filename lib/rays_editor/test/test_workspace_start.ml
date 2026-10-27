(* A workspace start through [Rays_editor.Workspace.run], window and all.

   start: one million marks that do not read [t] (sketches/million_marks with its drift term
   removed) for one frame. The workspace is evaluated once (the window, the camera and the editor
   read one document; it was three evaluations), and the first frame costs under 500 bytes a mark
   on the initial domain (it was about 1,040: three evaluations, four copies of the 64-byte
   instances and a tuple and boxed floats per mark; it is about 290 now, the evaluation, the
   instances and the index table once each).

   reload: the document the start shares is not a cache. The program is built from one text and
   its file holds another whose static map (5,000 elements, so the bulk static path) has other
   values; the file is taken on the first poll and the value the host graph then names is the
   file's, in a PNG whose name only that evaluation can spell. *)
let fail message = prerr_endline message; exit 1
let clock = Unix.gettimeofday

(* [rays]'s entry point, with a built text where its file is *)
let main ~frames ~path text = match Rays_editor.Workspace.main ~text ~process:{ Rays_editor.Workspace.no_process with max_frames = Some frames } path with
  | Ok () -> ()
  | Error diagnostics -> fail (String.concat "\n" (List.map Diagnostic.to_string diagnostics))

let errors () =
  let path = Filename.temp_file "rays-missing-workspace" ".rays" in
  Sys.remove path;
  let refuses code = function
    | Error diagnostics ->
        if not (List.exists (fun (diagnostic : Diagnostic.t) -> diagnostic.code = code) diagnostics)
        then fail ("workspace start did not return " ^ code)
    | Ok () -> fail "workspace start accepted invalid input" in
  refuses "E_IO" (Rays_editor.Workspace.main path);
  refuses "E_UNCLOSED" (Rays_editor.Workspace.main ~text:"(workspace" path);
  print_endline "workspace start: I/O and syntax errors return without exiting"

let replace ~sub ~by text =
  let n = String.length sub in
  let rec find i = if i + n > String.length text then fail ("the sketch no longer contains " ^ sub)
    else if String.sub text i n = sub then i else find (i + 1) in
  let at = find 0 in
  String.sub text 0 at ^ by ^ String.sub text (at + n) (String.length text - at - n)

let start sketch =
  let text = In_channel.with_open_bin sketch In_channel.input_all
    |> replace ~sub:"(* 0.1 (sin (+ t (* x 6.0))))" ~by:"0.0" in
  let marks = 1_000_000 in
  if not (String.length (replace ~sub:(Printf.sprintf "count %d" marks) ~by:"" text) < String.length text) then
    fail "the sketch no longer draws 1,000,000 marks";
  let before = Gc.allocated_bytes () in
  (* no file at this path: the program has no source to poll *)
  let (), phases = Flow.Phase_timer.sample ~clock (fun () -> main ~frames:1 ~path:"test/static_million_marks.rays" text) in
  let per_mark = (Gc.allocated_bytes () -. before) /. float marks in
  let evaluations = Flow.Phase_timer.calls phases "evaluate" in
  if evaluations <> 1 then fail (Printf.sprintf "a start evaluated the workspace %d times" evaluations);
  (* 398 before the node library; its 128 definitions add a fixed ~75 MB (75 a mark) *)
  if per_mark > 500. then fail (Printf.sprintf "a static start allocated %.0f bytes a mark" per_mark);
  Printf.printf "workspace start: 1,000,000 static marks, 1 evaluation, %.0f bytes a mark for the first frame\n" per_mark

let reload () =
  let text scale effects = Printf.sprintf {|(workspace reload_static
  (graph actions :context host
    (let* [table (map (fn [i] (* i %d)) (array/range 5000))
           name (str "seen-" (int (array/nth table 4999)) ".png")]
      %s))
  (graph picture :context draw (draw/background "#11151c"))
  (graph window :context settings (settings/config :title "reload" :width 200 :height 120))
  (graph editor :context editor
    (ui/workspace (ui/canvas (ref picture) :focus true) :effects (ref actions))))
|} scale effects in
  let built = text 2 "(list (host/save_png name false))" in
  let filed = text 3 "(list (host/save_png name (= (mod (frame/index) 10) 0)) (host/quit (> (frame/index) 100)))" in
  let directory = Filename.temp_dir "rays-start" "" in
  let write name contents = Out_channel.with_open_bin (Filename.concat directory name) (fun channel ->
    Out_channel.output_string channel contents) in
  (* a project root (where the source is looked for) and a home of its own (presets, autosave) *)
  write "dune-project" "(lang dune 3.17)\n";
  write "sketch.rays" filed;
  let home = Filename.concat directory "home" in
  Unix.mkdir home 0o755;
  Unix.putenv "HOME" home;
  let previous = Sys.getcwd () in
  Sys.chdir directory;
  let rec remove path =
    if Sys.is_directory path then begin
      Array.iter (fun name -> remove (Filename.concat path name)) (Sys.readdir path); Unix.rmdir path
    end else Sys.remove path in
  Fun.protect ~finally:(fun () -> Sys.chdir previous; remove directory) (fun () ->
    let (), phases = Flow.Phase_timer.sample ~clock (fun () -> main ~frames:600 ~path:"sketch.rays" built) in
    let seen name = Sys.file_exists (Filename.concat directory name) in
    if seen "seen-9998.png" then fail "the built text saved a picture: its effect is off";
    if not (seen "seen-14997.png") then
      fail "the reloaded file's static value was not seen: no picture named by it";
    if not (String.equal (In_channel.with_open_bin "sketch.rays" In_channel.input_all) filed) then
      fail "the run rewrote the source file";
    let evaluations = Flow.Phase_timer.calls phases "evaluate" in
    if evaluations <> 2 then
      fail (Printf.sprintf "a start and one reload evaluated the workspace %d times" evaluations);
    print_endline "workspace start: a file that differs from the built text reloads and its static values are the ones seen (2 evaluations)")

let () = match Array.to_list Sys.argv with
  | [_; "errors"] -> errors ()
  | [_; "start"; sketch] -> start sketch
  | [_; "reload"] -> reload ()
  | _ -> fail "usage: test_workspace_start (errors | start SKETCH | reload)"
