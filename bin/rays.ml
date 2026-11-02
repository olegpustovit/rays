(* rays: an empty editor, a .rays workspace opened natively, or check, fmt and web.
   The catalog is the in-process one. *)
module S = Flow.Syntax
module D = Diagnostic
module Doc = Editor_document.Workspace_doc

let read path = In_channel.with_open_bin path In_channel.input_all

let die fmt =
  Printf.ksprintf
    (fun m ->
      prerr_endline m;
      exit 1)
    fmt

let catalog () =
  match Editor_document.Kinds.catalog ~version:Flow_sop.Manifest.version Sop.Nodes.factories with
  | Ok c -> c
  | Error d -> die "rays: catalog: %s" (D.to_string d)

let check_meshes (lowered : Flow_sop.Lower.t) =
  let check_root instance fallback =
    match
      List.find_opt (fun (graph : Flow_sop.Lower.graph) -> graph.instance = instance) lowered.graphs
    with
    | None -> Error fallback
    | Some { root = None; _ } -> Ok ()
    | Some graph ->
        let ( let* ) = Result.bind in
        let* resolved =
          Flow_sop.Value_lane.resolve (Flow_sop.Value_lane.create ()) ~time:0. graph.network
        in
        let* node =
          Result.map_error (D.error ~code:"E_LOWER")
            (Sop.Edit_graph.compile_node resolved.geometry ~node_id:(Option.get graph.root))
        in
        let session =
          Sop.Session.create ~max_entries:16 ~max_payload_bytes:(1 lsl 28) |> Result.get_ok
        in
        Fun.protect
          ~finally:(fun () -> Sop.Session.close session)
          (fun () ->
            Result.map ignore
              (Sop.Session.cook session ~context:(Sop.Context.create () |> Result.get_ok) node))
  in
  let result =
    Array.fold_left
      (fun result (node : Flow.Eval.node) ->
        Result.bind result (fun () ->
            if
              List.mem node.kind [ "sop/mesh"; "sop/mesh_planes" ]
              && not (Flow.Eval.is_live (Flow.Eval.Record node.args))
            then
              match Flow_sop.Topology_kernel.mesh node.args with
              | Ok _ -> Ok ()
              (* On failure, the ordinary cook applies authored error mappers and
               leaves an unused, unprotected constructor uncooked. *)
              | Error diagnostic -> check_root node.inst diagnostic
            else Ok ()))
      (Ok ()) lowered.plan.nodes
  in
  Array.fold_left
    (fun (index, result) (instance : Flow.Eval.instance) ->
      let result =
        Result.bind result (fun () ->
            match instance.result with
            | Flow.Eval.Residual residual
              when (Flow.Eval.Residual.residual_view residual).term.ty = Flow.Ty.geometry
                   && not
                        (Flow.Eval.frame_dependent instance.result
                        || Flow.Eval.state_dependent instance.result) ->
                (* Protected geometry is a residual root: its constructor is not a
               static plan node. Cook through the boundary so its mapper runs. *)
                check_root index (D.error ~code:"E_LOWER" "Missing static geometry graph")
            | _ -> Ok ())
      in
      (index + 1, result))
    (0, result) lowered.plan.instances
  |> snd

(* The checked document and every diagnostic to print. Warnings fail the file
   unless the workspace form carries ^:allow-warnings.  The document is also
   evaluated and lowered as a window opens it, so a file that passes opens. *)
let check_text ?(imports = []) text =
  let allow =
    match S.parse text with
    | Ok forms ->
        List.exists
          (fun (f : S.t) -> S.head f = Some "workspace" && List.mem "allow-warnings" f.meta)
          forms
    | Error _ -> false
  in
  match Doc.check_text ~imports (catalog ()) text with
  | Error ds -> (None, ds, allow)
  | Ok (doc, warnings) -> (
      match
        Editor_document.Lowering.of_workspace ~defer:false ~factories:Sop.Nodes.factories doc
      with
      | Ok lowered -> (
          match check_meshes lowered.workspace.lowered with
          | Ok () -> (Some doc, warnings, allow)
          | Error d -> (None, warnings @ [ d ], allow))
      | Error d -> (None, warnings @ [ d ], allow))

(* The text, the document when the file reads, and whether it passes. *)
let check_file path =
  let text = try read path with Sys_error m -> die "rays: %s" m in
  let doc, ds, allow =
    match Rays_editor.Source.read_imports ~file:path text with
    | Ok imports -> check_text ~imports text
    | Error ds -> (None, ds, false)
  in
  List.iter (fun d -> prerr_endline (D.report ~file:path ~source:text d)) ds;
  let failed =
    doc = None
    || List.exists (fun (d : D.t) -> d.severity = Diagnostic.Error_severity || not allow) ds
  in
  (text, doc, not failed)

let check files =
  let ok =
    List.fold_left
      (fun ok f ->
        let _, _, passed = check_file f in
        passed && ok)
      true files
  in
  if not ok then exit 1

let fmt path =
  (* a file with warnings still formats *)
  match check_file path with
  | _, Some doc, _ -> print_string (Doc.to_text doc)
  | _, None, _ -> exit 1

(* The web export: static files in DIR (serve them with `npx serve DIR`, which reads the
   cross-origin isolation headers from serve.json). A refusal prints its diagnostic. *)
let web path dir =
  let text, doc, passed = check_file path in
  if not passed then exit 1;
  let doc = Option.get doc in
  match Result.bind (Web_export.lower doc) (Web_export.files doc) with
  | Error d ->
      prerr_endline (D.report ~file:path ~source:text d);
      exit 1
  | Ok files ->
      List.iter
        (fun (name, contents) ->
          let file = Filename.concat dir name in
          match Editor_core.Store.write_text ~filename:file contents with
          | Ok () -> ()
          | Error message -> die "rays: web: %s" message)
        files

(* what a bare [rays] opens; Command-S writes it to ./untitled.rays *)
let empty =
  {|(workspace untitled
  (graph picture :context draw
    (draw/background "#11151c"))
  (graph editor :context editor
    (let* [canvas (ui/canvas (ref picture) :focus true)
           network (ui/graph "picture")
           inspector (ui/inspector :of network)
           edit (ui/split "horizontal" network inspector :second_size 260)]
      (ui/workspace (ui/split "vertical" canvas edit :second_size 280)))))
|}

(* The only place the process environment is read: libraries take these as arguments. *)
let process () =
  let var name = match Sys.getenv_opt name with Some "" | None -> None | some -> some in
  let max_frames =
    Option.map
      (fun text ->
        match int_of_string_opt text with
        | Some n -> n
        | None -> die "rays: RAYS_MAX_FRAMES must be an integer")
      (var "RAYS_MAX_FRAMES")
  in
  {
    Rays_editor.Workspace.max_frames;
    crash_dir = var "RAYS_CRASH_DIR";
    profile = Sys.getenv_opt "RAYS_PROFILE" <> None;
    preferences = var "RAYS_EDITOR_PREFERENCES";
    ui_font = var "RAYS_UI_FONT";
  }

let native ?text path =
  let text =
    match text with
    | Some text -> text
    | None -> ( try read path with Sys_error message -> die "rays: %s" message)
  in
  match Rays_editor.Workspace.main ~text ~process:(process ()) path with
  | Ok () -> ()
  | Error diagnostics ->
      List.iter
        (fun diagnostic -> prerr_endline (D.report ~file:path ~source:text diagnostic))
        diagnostics;
      exit 1

(* The ledger of a workspace's real frames, without a window. *)
let mem path ~frames =
  let text = try read path with Sys_error message -> die "rays: %s" message in
  let workspace =
    match
      Result.bind
        (Rays_editor.Source.read_imports ~file:path text)
        (fun imports -> Rays_editor.Workspace.load ~imports text)
    with
    | Ok workspace -> workspace
    | Error diagnostics ->
        List.iter (fun d -> prerr_endline (D.report ~file:path ~source:text d)) diagnostics;
        exit 1
  in
  match Rays_editor.Measure.run ~early:(min 30 frames) ~frames workspace with
  | Error message -> die "rays: %s" message
  | Ok outcome ->
      print_string (Mem.Report.to_string outcome.final.report);
      Printf.printf "frames %d: live %s -> %s, allocated %s, promoted %s a frame, readback %s\n" frames
        (Mem.Report.format_bytes outcome.early.live)
        (Mem.Report.format_bytes outcome.final.live)
        (Mem.Report.format_bytes (int_of_float (outcome.final.allocated /. float frames)))
        (Mem.Report.format_bytes (int_of_float (outcome.final.promoted /. float frames)))
        (Mem.Report.format_bytes outcome.readback_bytes);
      List.iter
        (fun (site, reason, n) -> Printf.printf "left the device: %s, %s (%d)\n" site reason n)
        outcome.fallbacks;
      Option.iter print_endline outcome.memory_error;
      Option.iter print_endline outcome.cook_error

let () =
  (match Mem.configure Sys.getenv_opt with Ok () -> () | Error message -> die "rays: %s" message);
  match List.tl (Array.to_list Sys.argv) with
  | [] when Sys.file_exists "untitled.rays" -> native "untitled.rays"
  | [] -> native ~text:empty "untitled.rays"
  | [ path ] when Filename.check_suffix path ".rays" -> native path
  | "check" :: (_ :: _ as files) -> check files
  | [ "fmt"; f ] -> fmt f
  | [ "mem"; f ] -> mem f ~frames:120
  | [ "mem"; f; "--frames"; n ] -> (
      match int_of_string_opt n with
      | Some frames when frames > 0 -> mem f ~frames
      | _ -> die "rays: --frames takes a positive count")
  | [ "web"; f; "-o"; dir ] -> web f dir
  | _ -> die "usage: rays [FILE.rays] | rays check FILE... | rays fmt FILE | rays mem FILE [--frames N] | rays web FILE -o DIR"
