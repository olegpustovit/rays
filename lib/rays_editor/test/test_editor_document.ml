(* The document boundary: a lowered workspace validates and a broken one is refused, the dump
   is deterministic, and both public hosts save every document (there is no other kind) after
   the scene's objects are deleted through real UI events. *)
open Rays
open Sop

module Preset = Editor_document.Preset
module Document = Editor_document.Document

let check condition message = if not condition then failwith message
let key k = Input.Key_pressed k
let char c = key (Input.KeyChar c)

let frame ?(keys = []) ?(mouse = (450., 320.)) events count : Frame.t = {
  input = { (Input.at_time (float count /. 60.)) with dt = 1. /. 60.;
    frame = count; tick = count; size = 900, 640; pointer = mouse; keys; events };
  pixel_scale = 1., 1.; fps = 60.; mouse_delta = 0., 0. }

let text = {|(workspace document
  (graph geo1 :context sop (sop/box :size [1 1 1]))
  (graph scene :context scene
    (scene/merge (scene/geometry (ref geo1) :name "geo1") (scene/camera :name "camera1"))))|}

let contains text piece =
  let n = String.length piece in
  let rec at i = i + n <= String.length text && (String.sub text i n = piece || at (i + 1)) in
  at 0

let run () =
  let directory = Filename.temp_dir "rays-document-contract" "" in
  Fun.protect ~finally:(fun () ->
    let state = Filename.concat directory "state" in
    if Sys.file_exists state then begin
      Array.iter (fun name -> Sys.remove (Filename.concat state name)) (Sys.readdir state);
      Unix.rmdir state
    end;
    Array.iter (fun name -> Sys.remove (Filename.concat directory name)) (Sys.readdir directory);
    Unix.rmdir directory) (fun () ->
  let factories = Sop_catalog.Editor.factories in
  let workspace = Ws_fixture.of_text text in
  let doc = match Editor_document.Contexts.of_workspace ~factories workspace with
    | Ok doc -> doc | Error d -> failwith (Diagnostic.to_string d) in
  let backend : Flow_ir.Gpu.backend = {
    cost = (fun ~fast:_ _ ~count:_ -> None);
    prepare = (fun ~fast:_ _ -> Error (Diagnostic.error ~code:"E_TEST_GPU" "unused test backend")) } in
  let gpu : Flow_ir.Gpu.config = { backend = Some backend; fast = true;
    policy = Flow_ir.Gpu.Qualification } in
  let carries_gpu doc =
    let lowered = (doc.Document.workspace.lowered).Flow_sop.Lower.gpu in
    lowered.fast && lowered.policy = Flow_ir.Gpu.Qualification
    && Option.fold ~none:false ~some:(fun actual -> actual == backend) lowered.backend in
  let gpu_doc = match Editor_document.Contexts.of_workspace ~factories ~gpu workspace with
    | Ok doc -> doc | Error d -> failwith (Diagnostic.to_string d) in
  check (carries_gpu gpu_doc) "workspace lowering discarded its explicit GPU config";
  let reloaded = Editor_document.Document_edit.text_edit ~factories gpu_doc
      (Editor_document.Workspace_doc.to_text workspace) |> Result.get_ok in
  check (carries_gpu reloaded) "a real workspace text reload discarded its GPU config";
  check (Result.is_ok (Document.validate doc)) "a lowered workspace is a valid document";
  check (Edit_graph.inspect (Document.scene_graph doc) <> []
    && Option.is_some (Document.object_network doc
      (List.hd (Editor_document.Objects.ids "geometry" (Document.scene_graph doc)))))
    "the lowering has no geometry object with a network";
  (* an object must own its network, and a network its object *)
  check (Result.is_error (Document.validate { doc with networks = Document.Int_map.empty }))
    "an object without its network was accepted";
  let network = snd (List.hd (Document.Int_map.bindings doc.networks)) in
  check (Result.is_error (Document.validate
    { doc with networks = Document.Int_map.add 9_999 network doc.networks }))
    "a network without its object was accepted";
  check (Result.is_error (Document.validate { doc with active_camera = Some 9_999 }))
    "an active camera that is not a camera object was accepted";
  check (Document.dump doc = Document.dump doc && contains (Document.dump doc) "object ")
    "the document dump is not deterministic or lost an object";
  (* the preset text is the workspace text: it loads back to the same document *)
  let saved = Preset.save ~directory ~name:"round" ~doc ~view:(Flow.Syntax.make (Flow.Syntax.Map [])) |> Result.get_ok in
  (match Preset.load ~gpu ~path:saved ~factories ~settings:doc.settings () with
   | Ok loaded ->
       check (Editor_document.Workspace_doc.to_text (loaded.doc.workspace.ws)
              = Editor_document.Workspace_doc.to_text workspace
              && List.length (Document.Int_map.bindings loaded.doc.networks)
                 = List.length (Document.Int_map.bindings doc.networks))
         "a saved preset did not load back";
       check (carries_gpu loaded.doc) "preset loading discarded its explicit GPU config"
   | Error message -> failwith message);
  Sys.remove saved;
  (* Deleting every scene object through real UI events empties the scene and the
     preview, and the document still saves. *)
  let exercise ~create ~update ~close ~document:current_document ~prepared ~panes =
    let value = ref (create ()) and count = ref 0 in
    Fun.protect ~finally:(fun () -> close !value) (fun () ->
    let step ?keys ?mouse events =
      incr count; value := update !value (frame ?keys ?mouse events !count) in
    (* the editors await each frame's cook: a bounded number of frames, not a wait on the clock *)
    let settle predicate =
      let frames = ref 0 in
      while not (predicate !value) && !frames < 200 do step []; incr frames done;
      check (predicate !value) "cook did not settle" in
    settle (fun env -> prepared env <> None);
    step [];
    begin
      (* the scene opens as a list: delete its first row, twice (geo1, then the camera) *)
      let gx, gy, _, _ = (panes !value (frame [] 0)).Pxui_shell.Layout.graph in
      let mouse = float (gx + 60), float (gy + 24 + 12) in
      let delete () =
        count := !count + 30; (* past the double-click interval *)
        step ~mouse [];
        step ~mouse [Input.Pointer_pressed (Input.LeftButton, mouse);
          Input.Pointer_released (Input.LeftButton, mouse)];
        step ~mouse [key Input.Delete]; step [] in
      delete (); delete ();
      check (Edit_graph.inspect (current_document !value) = [] && prepared !value = None)
        (Printf.sprintf "delete-all did not clear the scene and preview: %s, prepared %b"
          (String.concat "," (List.map (fun (i : Edit_graph.node_info) -> i.label)
            (Edit_graph.inspect (current_document !value)))) (prepared !value <> None))
    end;
    step [key (Input.KeyChar '/'); char 's']; step [key Input.Enter]; step [];
    check (List.length (Preset.list ~directory) = 1) "the document did not save a preset";
    List.iter (fun (name, _) -> Sys.remove (Preset.path ~directory ~name)) (Preset.list ~directory)) in
  let prepares = Atomic.make 0 in
  let prepare _ _ = Atomic.incr prepares; Ok (Atomic.get prepares) in
  let module E3 = Rays_editor.Editor in
  exercise
    ~create:(fun () -> E3.create ~ui_text_size:11 ~await:true ~workspace ~presets:directory ~factories ~prepare
      ~draw:(fun _ _ -> Scene.D3.create []) () |> Result.get_ok)
    ~update:E3.update ~close:E3.close ~document:E3.document ~prepared:E3.prepared ~panes:E3.panes;
  print_endline "editor document: validation, dump, preset round trip, delete-all passed")
