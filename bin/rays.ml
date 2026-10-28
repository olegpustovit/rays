(* rays: an empty editor, a .rays workspace opened natively, or check, fmt and web.
   The catalog is the in-process one. *)
module S = Flow.Syntax
module D = Diagnostic
module Doc = Editor_document.Workspace_doc

let read path = In_channel.with_open_bin path In_channel.input_all

let die fmt = Printf.ksprintf (fun m -> prerr_endline m; exit 1) fmt

let catalog () =
  match Editor_document.Kinds.catalog ~version:Flow_sop.Manifest.version Sop.Nodes.factories with
  | Ok c -> c
  | Error d -> die "rays: catalog: %s" (D.to_string d)

(* The checked document and every diagnostic to print. Warnings fail the file
   unless the workspace form carries ^:allow-warnings.  The document is also
   evaluated and lowered as a window opens it, so a file that passes opens. *)
let check_text ?(imports = []) text =
  let allow = match S.parse text with
    | Ok forms -> List.exists (fun (f : S.t) -> S.head f = Some "workspace" && List.mem "allow-warnings" f.meta) forms
    | Error _ -> false in
  match Doc.check_text ~imports (catalog ()) text with
  | Error ds -> None, ds, allow
  | Ok (doc, warnings) ->
      (match Editor_document.Contexts.of_workspace ~factories:Sop.Nodes.factories doc with
       | Ok _ -> Some doc, warnings, allow
       | Error d -> None, warnings @ [ d ], allow)

(* The text, the document when the file reads, and whether it passes. *)
let check_file path =
  let text = try read path with Sys_error m -> die "rays: %s" m in
  let doc, ds, allow = match Rays_editor.Source.read_imports ~file:path text with
    | Ok imports -> check_text ~imports text
    | Error ds -> None, ds, false in
  List.iter (fun d -> prerr_endline (D.report ~file:path ~source:text d)) ds;
  let failed = doc = None || List.exists (fun (d : D.t) -> d.severity = Diagnostic.Error_severity || not allow) ds in
  text, doc, not failed

let check files =
  let ok = List.fold_left (fun ok f -> let _, _, passed = check_file f in passed && ok) true files in
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
  | Error d -> prerr_endline (D.report ~file:path ~source:text d); exit 1
  | Ok files ->
      List.iter (fun (name, contents) ->
        let file = Filename.concat dir name in
        match Editor_core.Store.write_text ~filename:file contents with
        | Ok () -> ()
        | Error message -> die "rays: web: %s" message) files

(* what a bare [rays] opens; Command-S writes it to ./untitled.rays *)
let empty = {|(workspace untitled
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
  let max_frames = Option.map (fun text -> match int_of_string_opt text with
    | Some n -> n | None -> die "rays: RAYS_MAX_FRAMES must be an integer") (var "RAYS_MAX_FRAMES") in
  { Rays_editor.Workspace.max_frames; crash_dir = var "RAYS_CRASH_DIR";
    profile = Sys.getenv_opt "RAYS_PROFILE" <> None;
    preferences = var "RAYS_EDITOR_PREFERENCES"; ui_font = var "RAYS_UI_FONT" }

let native ?text path =
  let text = match text with Some text -> text | None ->
    try read path with Sys_error message -> die "rays: %s" message in
  match Rays_editor.Workspace.main ~text ~process:(process ()) path with
  | Ok () -> ()
  | Error diagnostics ->
      List.iter (fun diagnostic -> prerr_endline (D.report ~file:path ~source:text diagnostic)) diagnostics;
      exit 1

let () = match List.tl (Array.to_list Sys.argv) with
  | [] when Sys.file_exists "untitled.rays" -> native "untitled.rays"
  | [] -> native ~text:empty "untitled.rays"
  | [ path ] when Filename.check_suffix path ".rays" -> native path
  | "check" :: (_ :: _ as files) -> check files
  | [ "fmt"; f ] -> fmt f
  | [ "web"; f; "-o"; dir ] -> web f dir
  | _ -> die "usage: rays [FILE.rays] | rays check FILE... | rays fmt FILE | rays web FILE -o DIR"
