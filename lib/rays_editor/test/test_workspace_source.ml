(* W11 part B: the source file of a .rays sketch: found from the executable or the working
   directory, saved over only while it is what the document came from (atomic, comments intact,
   no self reload), polled twice a second, reloaded as one history entry that keeps view state
   by path, and refused text leaving the last good document. *)
open Rays
module Doc = Editor_document.Workspace_doc
module E3 = Rays_editor.Editor
module Source = Rays_editor.Source

let fail message = failwith ("test_workspace_source: " ^ message)
let check condition message = if not condition then fail message
open Test_text

let sha = Editor_document.Contexts.sha256
let catalog = Editor_document.Kinds.catalog ~version:1 Sop_catalog.Editor.factories |> Result.get_ok

let sketch radius = Printf.sprintf {|;; the header
(workspace w
  ;; one sphere
  (graph g :context sop []
    (sop/uv_sphere :radius %s :segments 8 :rings 4))) ; the tail
|} radius
let text0 = sketch "0.5" and text1 = sketch "0.7"
let typo = {|(workspace w (graph g :context sop [] (sop/uv_spher :radius 0.5)))|}

let clock = ref 1_700_000_000.
(* a write that always moves the mtime, whatever the file system's resolution *)
let write file text =
  Out_channel.with_open_bin file (fun channel -> output_string channel text);
  clock := !clock +. 10.;
  Unix.utimes file !clock !clock

let directory () = Filename.temp_dir "rays-source" ""
let rec remove_tree dir =
  Array.iter (fun f -> let p = Filename.concat dir f in
    if Sys.is_directory p then remove_tree p
    else Sys.remove p) (Sys.readdir dir);
  Unix.rmdir dir

let printed doc = let t = Doc.to_text doc in if String.ends_with ~suffix:"\n" t then t else t ^ "\n"

let run_files () =
  let dir = directory () in
  let file = Filename.concat dir "sketch.rays" in
  write file text0;
  (* Save: digest match writes the canonical text, comments intact, and re-reading it is the text *)
  let source = Source.at ~file ~digest:(sha text0) in
  let doc = Result.get_ok (Doc.of_text catalog text0) in
  let saved = printed doc in
  let source = match Source.save source saved with Ok s -> s | Error _ -> fail "save refused" in
  check (read file = saved) "the file is the written text";
  check (has (read file) "; the header" && has (read file) "; one sphere" && has (read file) "; the tail")
    ("comments survive: " ^ read file);
  check (printed (Result.get_ok (Doc.of_text catalog (read file))) = saved) "the re-read prints the same";
  check (Sys.readdir dir = [| "sketch.rays" |]) "no temporary file is left";
  check ((Unix.stat file).st_perm land 0o044 <> 0) "the file stays readable";
  (* Save does not reload: the write moved the mtime, the digest is the remembered one *)
  write file (read file);
  check (snd (Source.poll ~now:100. source) = None) "own write reloaded";
  (* digest mismatch: another writer changed the file, so Save refuses *)
  write file text1;
  check (Source.save source "x" = Error `Changed) "saved over another writer's text";
  check (read file = text1) "the refused save left the file alone";
  (* the poll: one content read per half second *)
  let source = Source.at ~file ~digest:(sha text0) in
  let source, changed = Source.poll ~now:0. source in
  check (changed = Some text1) "a file that already differed at startup did not reload on the first poll";
  check (snd (Source.poll ~now:1. source) = None) "the startup difference was reported twice";
  let _, same = Source.poll ~now:0. (Source.at ~file ~digest:(sha text1)) in
  check (same = None) "a file that matches the built text reloaded";
  let source = Source.at ~file ~digest:(sha text1) in
  let source, _ = Source.poll ~now:0. source in
  write file text0;
  let source, early = Source.poll ~now:0.3 source in
  check (early = None) "polled again within half a second";
  let source, late = Source.poll ~now:0.6 source in
  check (late = Some text0) "the change was not seen after half a second";
  let source, again = Source.poll ~now:1.2 source in
  check (again = None) "the same change reported twice";
  let stamp = (Unix.stat file).st_mtime in
  Out_channel.with_open_bin file (fun channel -> output_string channel text1);
  Unix.utimes file stamp stamp;
  let source, changed = Source.poll ~now:1.8 source in
  check (changed = Some text1) "a preserved-mtime content edit was missed";
  let replacement = Filename.concat dir "replacement.rays" in
  write replacement text0;
  Unix.utimes replacement stamp stamp;
  let inode = (Unix.stat file).st_ino in
  Unix.rename replacement file;
  check ((Unix.stat file).st_ino <> inode) "replacement did not change the inode";
  let _, changed = Source.poll ~now:2.4 source in
  check (changed = Some text0) "a same-mtime inode replacement was missed";
  remove_tree dir

let run_find () =
  let start = Sys.getcwd () in
  let dir = directory () in
  Fun.protect ~finally:(fun () -> Sys.chdir start; remove_tree dir) (fun () ->
    Out_channel.with_open_bin (Filename.concat dir "dune-project") ignore;
    Unix.mkdir (Filename.concat dir "sk") 0o755;
    Unix.mkdir (Filename.concat dir "_build") 0o755;
    Out_channel.with_open_bin (Filename.concat dir "_build/dune-project") ignore;
    write (Filename.concat dir "sk/x.rays") text0;
    let expect from =
      Sys.chdir from;
      match Source.find ~path:"sk/x.rays" ~digest:"" with
      | Some s -> check (Unix.realpath (Source.file s) = Unix.realpath (Filename.concat dir "sk/x.rays"))
                    "the source file is joined to the project root"
      | None -> fail ("no source from " ^ from) in
    expect (Filename.concat dir "sk");
    expect (Filename.concat dir "_build");  (* a dune-project under _build is not the root *)
    Sys.chdir start;
    check (Source.find ~path:"sk/nowhere.rays" ~digest:"" = None) "a missing file is no source";
    (* an internal directory is what the temp project's cleanup needs empty *)
    Sys.remove (Filename.concat dir "_build/dune-project"); Unix.rmdir (Filename.concat dir "_build");
    Sys.remove (Filename.concat dir "sk/x.rays"); Unix.rmdir (Filename.concat dir "sk");
    Sys.remove (Filename.concat dir "dune-project"))

(* ---- through the editor ---- *)

let editor ?presets ?(imports = []) ~source text =
  E3.create ~ui_text_size:11 ~await:true ~workspace:(Result.get_ok (Doc.of_text ~imports catalog text)) ?presets ~source
    ~prepare:(fun _ output -> Rdk_rays.Rays_mesh.to_mesh (Result.get_ok (Sop.Payload.geometry output.Sop.Session.payload))
      |> Result.map_error Rdk.Error.to_string)
    ~draw:(fun _ mesh -> Scene.D3.create [ Scene.D3.mesh mesh ]) ()
  |> function Ok e -> e | Error m -> fail m

let source_text e = fst (Flow.Lisp.print (E3.workspace e).Doc.source)
let cook_line e = Test_workspace_shell.dump_line e "cook"

let run_editor () =
  let dir = directory () in
  let file = Filename.concat dir "sketch.rays" in
  write file text0;
  let e = ref (editor ~presets:(Filename.concat dir "presets")
      ~source:(Source.at ~file ~digest:(sha text0)) text0) and count = ref 0 in
  let step ?keys events =
    incr count; e := E3.update !e (Test_editor_input.frame ?keys (450., 300.) events !count) in
  let run_for seconds = for _ = 1 to int_of_float (seconds *. 60.) do step [] done in
  (* the editor awaits each frame's cook: nothing is cooking after a frame (a check, not a wait) *)
  let settle () = step [] in
  step []; step []; settle ();
  let history = E3.undo_label !e in
  (* Command-S over the matching source *)
  step ~keys:[ Input.Meta ] [ Input.Key_pressed (Input.KeyChar 's') ];
  settle ();
  check (has (cook_line !e) "Saved sketch.rays") ("the status says it saved: " ^ cook_line !e);
  let written = read file in
  check (written = printed (E3.workspace !e)) "Save wrote the document's text";
  check (has written "; the header" && has written "; the tail") "the saved file keeps its comments";
  check (not (Sys.file_exists (Filename.concat dir "presets"))) "no preset was written";
  run_for 1.5;
  check (E3.undo_label !e = history) "Save reloaded its own write";
  (* an edit from another editor reloads as one history entry; probes stay by path *)
  let zone = [ "g"; "zone" ] in
  e := E3.set_probe !e zone 3;
  write file text1;
  run_for 1.;
  settle ();
  check (has (source_text !e) "0.7") ("the file's text is the document: " ^ source_text !e);
  check (E3.undo_label !e = Some "Reload sketch.rays") "one history entry named Reload sketch.rays";
  check (E3.probe !e zone = Some 3) "the probe survived by path";
  check (has (cook_line !e) "Reloaded sketch.rays" || has (cook_line !e) "checked") (cook_line !e);
  (* text that does not check keeps the last good document and says why *)
  write file typo;
  run_for 1.;
  settle ();
  check (has (source_text !e) "0.7") "the refused text replaced the document";
  check (E3.undo_label !e = Some "Reload sketch.rays") "the refused text made a history entry";
  check (has (cook_line !e) "not reloaded" && has (cook_line !e) "line 1") ("the status names the failure: " ^ cook_line !e);
  (* undoing the typo in the other editor (the text the document has) clears it *)
  write file text1;
  run_for 1.;
  settle ();
  check (has (cook_line !e) "Reloaded") ("the recovered file reloads: " ^ cook_line !e);
  (* a (layout ...) form edited into the file is the new layout; a file without one keeps it *)
  write file (text1 ^ {|(layout (node ["g" "@result"] :at [10 20]) (frame ["g"] "Sphere" :at [0 0] :size [200 100]))|});
  run_for 1.;
  settle ();
  let layout () = (E3.workspace !e).Doc.layout in
  check (Editor_document.Layout_by_path.Path_map.mem [ "g"; "@result" ] (layout ()).at
         && not (Editor_document.Layout_by_path.Path_map.is_empty (layout ()).frames))
    "the layout form of the file was ignored";
  write file text1;
  run_for 1.;
  settle ();
  check (Editor_document.Layout_by_path.Path_map.mem [ "g"; "@result" ] (layout ()).at)
    "a reload without a layout form dropped the layout";
  let before = E3.workspace !e and label = E3.undo_label !e in
  Sys.remove file;
  run_for 1.;
  check (E3.workspace !e == before && E3.undo_label !e = label)
    "unreadable source changed the document/history";
  check (has (cook_line !e) "Source unreadable") ("source read failure was silent: " ^ cook_line !e);
  step ~keys:[Input.Meta] [Input.Key_pressed (Input.KeyChar 's')];
  check (has (cook_line !e) "Not saved:") "unreadable source was reported as an external edit";
  check (not (Sys.file_exists file)) "Save recreated an unreadable source";
  write file text1;
  run_for 1.;
  check (has (cook_line !e) "Source readable again") ("source recovery was not reported: " ^ cook_line !e);
  check (E3.workspace !e == before && E3.undo_label !e = label)
    "same-content source recovery made a document change";
  E3.close !e;
  (* a file edited since the build reloads on the first poll, as one history entry *)
  write file text1;
  e := editor ~presets:(Filename.concat dir "presets")
      ~source:(Source.at ~file ~digest:(sha text0)) text0;
  count := 0;
  step []; step []; run_for 1.; settle ();
  check (has (source_text !e) "0.7") ("the edited file was not loaded at startup: " ^ source_text !e);
  check (E3.undo_label !e = Some "Reload sketch.rays") "the startup reload is one history entry";
  E3.close !e;
  (* a file that differs and does not check keeps the built text; Save then falls back to a preset *)
  write file typo;
  e := editor ~presets:(Filename.concat dir "presets")
      ~source:(Source.at ~file ~digest:(sha text0)) text0;
  count := 0;
  step []; step []; run_for 1.; settle ();
  check (has (cook_line !e) "not reloaded") ("the refused startup file says so: " ^ cook_line !e);
  step ~keys:[ Input.Meta ] [ Input.Key_pressed (Input.KeyChar 's') ];
  settle ();
  check (has (cook_line !e) "saved as preset") ("the status says why: " ^ cook_line !e);
  check (read file = typo) "the changed source file was not overwritten";
  check (List.length (Editor_document.Preset.list ~directory:(Filename.concat dir "presets")) = 1) "one preset was written";
  E3.close !e;
  remove_tree dir

(* X5/E8: an external change never replaces unsaved work.  The file's text waits while the
   document has an edit the file does not, the strip says so, and it loads once the edit is undone. *)
let run_external_over_dirty () =
  let dir = directory () in
  let file = Filename.concat dir "sketch.rays" in
  write file text0;
  let e = ref (editor ~presets:(Filename.concat dir "presets") ~source:(Source.at ~file ~digest:(sha text0)) text0)
  and count = ref 0 in
  let step ?keys events =
    incr count; e := E3.update !e (Test_editor_input.frame ?keys (450., 300.) events !count) in
  let run_for seconds = for _ = 1 to int_of_float (seconds *. 60.) do step [] done in
  step []; step [];
  e := Result.get_ok (E3.edit !e (Flow_graph.Flow_edit.Set_arg {
    node = ["g"; "@result"]; key = Kw "segments"; sub = []; value = Flow.Syntax.make (Num "12") }));
  let label = E3.undo_label !e in
  write file text1;
  run_for 1.5;
  check (has (source_text !e) ":segments 12" && has (source_text !e) "0.5" && E3.undo_label !e = label)
    ("an external change replaced a document with unsaved edits: " ^ source_text !e);
  check (has (cook_line !e) "changed on disk") ("the kept edits were not announced: " ^ cook_line !e);
  step ~keys:[ Input.Meta ] [ Input.Key_pressed (Input.KeyChar 'z') ];
  run_for 1.;
  check (has (source_text !e) "0.7" && E3.undo_label !e = Some "Reload sketch.rays")
    ("the file's text did not load once the edits were undone: " ^ source_text !e);
  E3.close !e;
  remove_tree dir

let run_autosave () =
  let dir = directory () in
  let file = Filename.concat dir "sketch.rays" and presets = Filename.concat dir "presets" in
  write file text0;
  let create () = editor ~presets ~source:(Source.at ~file ~digest:(sha text0)) text0 in
  let e = ref (create ()) and count = ref 0 in
  let step ?(keys = []) events =
    incr count; e := E3.update !e (Test_editor_input.frame ~keys (200., 300.) events !count) in
  step []; step [];
  let state = Editor_document.Preset.path ~directory:(Filename.concat presets "state")
    ~name:(sha ("file:" ^ Unix.realpath file)) in
  check (not (Sys.file_exists state)) "opening a sketch wrote over its recovery state";
  e := Result.get_ok (E3.edit !e (Flow_graph.Flow_edit.Set_arg {
    node = ["g"; "@result"]; key = Kw "radius"; sub = []; value = Flow.Syntax.make (Num "1.25") }));
  e := E3.set_renderer !e Rays_editor.Renderer.Wireframe;
  step [Input.Pointer_moved (200., 300.); Input.Scrolled (0., -2.)];
  let saved_eye = Camera.position (Easy_camera.camera (E3.camera !e)) in
  E3.close !e;
  check (has (read state) ":radius 1.25" && has (read state) "; the header"
         && has (read state) "(view") "close did not flush the edited document and viewport";
  check (read file = text0) "autosave rewrote the source file";
  check (Array.length (Sys.readdir (Filename.dirname state)) = 1) "autosave left multiple snapshots or temporary files";
  let snapshot = read state and stamp = (Unix.stat state).st_mtime in
  e := create (); count := 0;
  for _ = 1 to 70 do step [] done;
  check (read state = snapshot && (Unix.stat state).st_mtime = stamp)
    "an unchanged restarted sketch overwrote the last edited state";
  (* E7: orbiting in the pristine document does not write over the earlier session's unsaved work *)
  step [Input.Pointer_moved (200., 300.); Input.Scrolled (0., -2.)];
  for _ = 1 to 70 do step [] done;
  check (read state = snapshot) "orbiting after a restart overwrote the last edited state";
  step [Input.Key_pressed (Input.KeyChar '/'); Input.Key_pressed (Input.KeyChar 'b')];
  step [Input.Key_pressed Input.Enter];
  check (has (source_text !e) ":radius 1.25" && E3.undo_label !e = Some "Restore last edited state")
    ("/ b did not restore the last edited state as one undo entry: "
      ^ Option.value ~default:"-" (E3.undo_label !e) ^ "; " ^ cook_line !e ^ "; " ^ source_text !e);
  check (Rays_math.Vec3.nearly_equal saved_eye (Camera.position (Easy_camera.camera (E3.camera !e))) ~eps:1e-9)
    "recovery did not restore the viewport";
  check (E3.renderer !e = Rays_editor.Renderer.Wireframe) "recovery did not restore the shared renderer choice";
  step ~keys:[Input.Meta] [Input.Key_pressed (Input.KeyChar 'z')];
  check (has (source_text !e) ":radius 0.5") "undo did not revert recovery";
  E3.close !e;
  (* A corrupt recovery file is reported without replacing the good document or the file. *)
  write state typo;
  e := create (); count := 0;
  step [];
  step [Input.Key_pressed (Input.KeyChar '/'); Input.Key_pressed (Input.KeyChar 'b')];
  step [Input.Key_pressed Input.Enter];
  check (has (source_text !e) ":radius 0.5" && has (cook_line !e) "rejected")
    "a broken autosave replaced the live document or hid its error";
  step [Input.Key_pressed (Input.KeyChar '/'); Input.Key_pressed (Input.KeyChar 'b')];
  step [Input.Key_pressed Input.Delete]; step [Input.Key_pressed Input.Delete];
  check (not (Sys.file_exists state)) "Delete twice did not remove the recovery file";
  E3.close !e;
  check (not (Sys.file_exists state)) "closing an unchanged sketch recreated deleted recovery";
  (* Failed writes keep the edit and retry after the destination becomes usable. *)
  Unix.rmdir (Filename.dirname state);
  write (Filename.dirname state) "blocked";
  e := create (); count := 0;
  step [];
  e := Result.get_ok (E3.edit !e (Flow_graph.Flow_edit.Set_arg {
    node = ["g"; "@result"]; key = Kw "radius"; sub = []; value = Flow.Syntax.make (Num "2") }));
  for _ = 1 to 35 do step [] done;
  check (has (Test_workspace_shell.dump_line !e "autosave") "Autosave failed"
         && has (source_text !e) ":radius 2")
    "a failed periodic save lost the edit or hid its error";
  Sys.remove (Filename.dirname state);
  for _ = 1 to 35 do step [] done;
  check (has (read state) ":radius 2" && Test_workspace_shell.dump_line !e "autosave" = "-")
    "autosave did not retry and clear its error while the editor remained open";
  E3.close !e;
  remove_tree dir

(* The start keywords (flow.md 11.11) through the two routes that replace the document from a
   file: a changed keyword is followed, an unchanged one leaves what the user chose in the UI. *)
let run_start_keywords () =
  let dir = directory () in
  let file = Filename.concat dir "sketch.rays" and presets = Filename.concat dir "presets" in
  let text ?(radius = "0.5") view tab = Printf.sprintf {|(workspace w
  (graph g :context sop []
    (sop/uv_sphere :radius %s :segments 8 :rings 4))
  (graph scene :context scene (scene/merge (scene/geometry (ref g))))
  (graph editor :context editor
    (let* [net (ui/graph "g" :view "%s" :focus true)
           code (ui/lisp :tab "%s")]
      (ui/workspace (ui/split "horizontal" net code)))))
|} radius view tab in
  let first = text "list" "graph" in
  write file first;
  let e = ref (editor ~presets ~source:(Source.at ~file ~digest:(sha first)) first) and count = ref 0 in
  let step ?keys events =
    incr count; e := E3.update !e (Test_editor_input.frame ?keys (100., 300.) events !count) in
  let run_for seconds = for _ = 1 to int_of_float (seconds *. 60.) do step [] done in
  let line = Test_workspace_shell.dump_line in
  let show_graph () =
    step [ Input.Key_pressed (Input.KeyChar '/'); Input.Key_pressed (Input.KeyChar 'l') ];
    step [ Input.Key_pressed (Input.KeyChar 'g') ]; step [] in
  step []; step [];
  check (line !e "projection" = "list" && has (line !e "panels") "lisp code: graph tab")
    ("the sketch opens as its keywords say: " ^ line !e "projection" ^ " / " ^ line !e "panels");
  (* a reload that leaves the keywords alone leaves the view the user chose *)
  show_graph ();
  check (line !e "projection" = "graph") "/ l g did not show the graph";
  write file (text ~radius:"0.7" "list" "graph");
  run_for 1.; step [];
  check (E3.undo_label !e = Some "Reload sketch.rays" && has (source_text !e) "0.7") "the file did not reload";
  check (line !e "projection" = "graph") "a reload that left :view alone reset the view the user chose";
  (* a reload that changes them is followed *)
  write file (text ~radius:"0.7" "text" "document");
  run_for 1.; step [];
  check (line !e "projection" = "text" && has (line !e "panels") "lisp code: document tab")
    ("a reload did not follow the changed keywords: " ^ line !e "projection" ^ " / " ^ line !e "panels");
  (* a preset: saved with :view "text", loaded over a document that says "list" *)
  step [ Input.Key_pressed (Input.KeyChar '/'); Input.Key_pressed (Input.KeyChar 's') ];
  step [ Input.Key_pressed Input.Enter ]; step [];
  let name = match Editor_document.Preset.list ~directory:presets with
    | [ (name, _) ] -> name | _ -> fail "/ s did not save one preset" in
  e := Result.get_ok (E3.edit !e (Flow_graph.Flow_edit.Set_arg {
    node = [ "editor"; "net" ]; key = Kw "view"; sub = []; value = Flow.Syntax.make (Str "list") }));
  step []; step [];
  check (line !e "projection" = "list") "an edit of :view was not followed";
  let load () =
    step [ Input.Key_pressed (Input.KeyChar '/'); Input.Key_pressed (Input.KeyChar 'b') ];
    step [ Input.Text_input name; Input.Key_pressed Input.Enter ]; step []; step [] in
  load ();
  check (E3.undo_label !e = Some "Load preset" && line !e "projection" = "text")
    ("a preset load did not follow its :view: " ^ line !e "projection");
  (* the same preset again changes no keyword: the user's view stays *)
  show_graph ();
  load ();
  check (E3.undo_label !e = Some "Load preset" && line !e "projection" = "graph")
    "a preset load that left :view alone reset the view the user chose";
  E3.close !e;
  remove_tree dir

let run_imports () =
  let dir = directory () in
  let file = Filename.concat dir "sketch.rays" and lib = Filename.concat dir "lib.rays" in
  let library radius = "(workspace library (graph shared :context sop (sop/uv_sphere :radius " ^ radius ^ " :segments 8 :rings 4)))\n" in
  let text = "(workspace main (graph local :context sop (ref shared)))\n(import \"lib.rays\")\n" in
  write file text; write lib (library "0.5");
  let e = ref (editor ~imports:["lib.rays", library "0.5"]
      ~source:(Source.at ~file ~digest:(sha text)) text) and count = ref 0 in
  Fun.protect ~finally:(fun () -> E3.close !e; remove_tree dir) (fun () ->
    let step ?keys events = incr count; e := E3.update !e
        (Test_editor_input.frame ?keys (450., 300.) events !count) in
    let poll () = for _ = 1 to 65 do step [] done in
    poll ();
    check (E3.undo_label !e = None) "unchanged imported text reloaded at startup";
    write lib (library "0.7"); poll ();
    check (has (source_text !e) "0.7" && E3.undo_label !e = Some "Reload sketch.rays") "changed import did not reload";
    let document = E3.workspace !e in
    poll ();
    check (E3.workspace !e == document) "import reloaded repeatedly (normalized digest)";
    let unchanged = read lib in
    step ~keys:[Input.Meta] [Input.Key_pressed (Input.KeyChar 's')];
    check (read lib = unchanged && not (has (read file) "(graph shared")) "save wrote imported forms or file";
    e := Result.get_ok (E3.edit !e (Flow_graph.Flow_edit.Set_note {node = ["local"; "@result"]; text = "local edit"}));
    write lib (library "0.9"); poll ();
    check (has (source_text !e) "0.7" && has (cook_line !e) "changed on disk") "import replaced unsaved work";
    step ~keys:[Input.Meta] [Input.Key_pressed (Input.KeyChar 'z')]; poll ();
    check (has (source_text !e) "0.9") "held imported change did not load after undo";
    Sys.remove lib; poll ();
    check (has (cook_line !e) "E_IMPORT" || has (cook_line !e) "not reloaded") "missing import was silent";
    write lib (library "0.9"); poll ();
    check (E3.workspace !e != document) "recreated import did not recover")

let run_library () =
  let dir = directory () in
  let file = Filename.concat dir "sketch.rays" and lib = Filename.concat dir "lib.rays" in
  let library n = "; editable library\n(defn twice :context value [(x : float)] (* x " ^ n ^ "))\n" in
  let text = "(workspace main (graph local :context value (twice 3)))\n(import \"lib.rays\")\n" in
  write file text; write lib (library "2");
  let e = ref (editor ~imports:["lib.rays", library "2"] ~source:(Source.at ~file ~digest:(sha text)) text) in
  Fun.protect ~finally:(fun () -> E3.close !e; remove_tree dir) (fun () ->
    e := E3.update (Rays_editor.Reduce.open_import !e "lib.rays") (Test_editor_input.frame (450., 300.) [] 0);
    check ((E3.workspace !e).fragment && (E3.workspace !e).imports = []) "open library did not switch to editable definitions";
    e := E3.edit !e (Flow_graph.Flow_edit.Set_arg {node = ["def:twice"; "@result"]; key = Pos 1; sub = []; value = Flow.Syntax.make (Num "4")}) |> Result.get_ok;
    e := E3.update !e (Test_editor_input.frame ~keys:[Input.Meta] (450., 300.) [Input.Key_pressed (Input.KeyChar 's')] 1);
    check (has (read lib) "(* x 4)" && has (read lib) "editable library" && not (has (read lib) "workspace")) "library save added a wrapper or lost edit/comment";
    check (read file = text) "open library saved over importer";
    write lib (library "5");
    for count = 2 to 70 do e := E3.update !e (Test_editor_input.frame (450., 300.) [] count) done;
    check (has (Doc.to_text (E3.workspace !e)) "(* x 5)") "bare definition library did not reload")

let run_embedded_nodes () =
  let text = {|;; embedded node call
(workspace w (graph g :context sop
  (let* [source (sop/line)
         shifted (sop/noise_displace source :amplitude 0.5)] shifted)))
|} in
  let contains_node doc = List.exists (fun (node : Flow.Workspace.node_definition) ->
    node.kind.qualified = "sop/noise_displace") doc.Doc.checked.nodes in
  let doc = Doc.of_text catalog text |> Result.get_ok in
  check (contains_node doc) "embedded definition absent at load";
  let doc = Doc.edit catalog doc (Flow_graph.Flow_edit.Set_arg {
    node=["g"; "shifted"]; key=Kw "amplitude"; sub=[]; value=Flow.Syntax.make (Num "0.75")})
    |> Result.get_ok in
  check (Option.is_some doc.literal) "embedded scalar scrub missed literal edit path";
  check (contains_node doc) "scalar scrub lost embedded definition";
  check (not (has (printed doc) "(defnode")) "scalar scrub copied library forms into saved source";
  let doc = Doc.edit catalog doc (Flow_graph.Flow_edit.Set_note {
    node=["g"; "shifted"]; text="edited node"}) |> Result.get_ok in
  check (doc.literal=None && contains_node doc) "structural gesture lost embedded definition";
  let saved = printed doc in
  check (has saved ":amplitude 0.75" && has saved "; edited node") "edits absent from saved source";
  check (not (has saved "(defnode") && not (has saved "sop/context_seed"))
    "saving authored calls copied embedded library into document";
  let dir = directory () in
  Fun.protect ~finally:(fun () -> remove_tree dir) (fun () ->
    let file = Filename.concat dir "sketch.rays" in
    write file text;
    ignore (Source.save (Source.at ~file ~digest:(sha text)) saved |> Result.get_ok);
    let reopened = Doc.of_text catalog (read file) |> Result.get_ok in
    check (contains_node reopened && printed reopened=saved)
      "saved embedded call did not reopen with identical authored source")

let run_library_navigation () =
  let parse text = Flow.Syntax.parse text |> Result.get_ok in
  let sources = parse {|(defn embedded_helper :context value [(x : float)] (+ x 1))
    (defmacro embedded_macro [x] `(+ ~x 1))|} in
  let authored = parse {|(workspace w
    (defn local_helper :context value [(x : float)] (* x 2))
    (defmacro local_macro [x] `(* ~x 2))
    (graph g :context value (embedded_helper (embedded_macro (local_helper 1)))))|} in
  let workspace = match Flow_sop.Catalog.check_workspace ~sources catalog authored with
    | Some workspace, [] -> workspace
    | _, diagnostics -> fail (String.concat "\n" (List.map Diagnostic.to_string diagnostics)) in
  check (List.length workspace.defs=2 && List.length workspace.macros=2)
    "library helper fixture lost checked definitions";
  let module N = Rays_editor.Private.Navigator in
  let params = N.{workspace; active=None; scope=None; records=None; probes=(fun _ -> 0);
    selected=[]; chips=[]; objects=[]; root_detail=""; layouts=None; notes=[]} in
  let descriptions state = N.rows state params |> Array.to_list |> List.map N.describe
    |> String.concat "\n" in
  let rows = descriptions N.initial in
  check (has rows "local_helper" && has rows "local_macro") "authored definitions missing from navigation";
  check (not (has rows "embedded_helper") && not (has rows "embedded_macro"))
    "embedded helpers exposed as editable navigation rows";
  check (not (has (descriptions (N.with_query "embedded" N.initial)) "embedded_helper"))
    "search exposed an embedded helper as an editable definition"

let run_vocabulary_refresh () =
  let text field = match field with
    | None -> "(workspace w (graph g :context sop (sop/line)))\n"
    | Some field -> Printf.sprintf {|(workspace w
      (defnode sop/vocab_card :label "Vocabulary card" :category ["Test"]
        [(geo : geometry) (%s : float 0.5)] geo)
      (graph g :context sop (sop/vocab_card (sop/line) :%s 0.5)))
|} field field in
  let dir = directory () in
  let file = Filename.concat dir "sketch.rays" in
  write file (text None);
  let e = ref (editor ~source:(Source.at ~file ~digest:(sha (text None))) (text None))
  and count = ref 0 in
  Fun.protect ~finally:(fun () -> E3.close !e; remove_tree dir) (fun () ->
    let frame () = incr count; Test_editor_input.frame (450., 300.) [] !count in
    let poll () = for _ = 1 to 65 do e := E3.update !e (frame ()) done in
    let labels suffix =
      E3.Private.lisp_completions !e ("(workspace w (graph g :context sop (" ^ suffix)
      |> List.map (fun (completion : Pxui.Ui.completion) -> completion.label) in
    let head () = List.mem "sop/vocab_card" (labels "sop/vocab_") in
    check (not (head ())) "new vocabulary kind existed before declaration";
    write file (text (Some "distance")); poll ();
    check (head () && List.mem ":distance" (labels "sop/vocab_card geo :dis"))
      "reload did not add local kind and parameter completions";
    e := Rays_editor.Reduce.step !e [Rays_editor.Private.Leader.Undo] (frame ());
    check (not (head ())) "undo retained removed declaration completion";
    e := Rays_editor.Reduce.step !e [Rays_editor.Private.Leader.Redo] (frame ());
    check (head ()) "redo did not restore declaration completion";
    write file (text (Some "amount")); poll ();
    check (List.mem ":amount" (labels "sop/vocab_card geo :am")
      && not (List.mem ":distance" (labels "sop/vocab_card geo :dis")))
      "schema edit kept old keyword vocabulary";
    e := Result.get_ok (E3.edit !e (Flow_graph.Flow_edit.Set_arg {
      node=["g";"@result"]; key=Kw "amount"; sub=[]; value=Flow.Syntax.make (Num "0.75")}));
    check (head () && List.mem ":amount" (labels "sop/vocab_card geo :am"))
      "scalar scrub lost declaration vocabulary";
    e := Rays_editor.Reduce.step !e [Rays_editor.Private.Leader.Undo] (frame ());
    write file (text None); poll ();
    check (not (head ())) "reload retained removed local kind completion")

let run () = run_files (); run_find (); run_editor (); run_external_over_dirty (); run_autosave (); run_start_keywords (); run_imports (); run_library (); run_embedded_nodes (); run_library_navigation (); run_vocabulary_refresh ();
  print_endline "workspace source tests passed"
