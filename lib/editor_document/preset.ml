let ( let* ) = Result.bind
module S = Flow.Syntax
let mk = S.make

type loaded = { doc : Document.t; view : S.t }

let sanitize name = String.map (function
  | ('A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '-') as character -> character
  | _ -> '_') (String.trim name)

let default_name ~now =
  let time = Unix.localtime now in
  Printf.sprintf "%04d-%02d-%02d_%02d-%02d-%02d" (time.tm_year + 1900)
    (time.tm_mon + 1) time.tm_mday time.tm_hour time.tm_min time.tm_sec

let suffix = ".rays"
let path ~directory ~name = Filename.concat directory (sanitize name ^ suffix)

let rec finite (f : S.t) = match f.node with
  | S.Num n -> (match float_of_string_opt n with Some x -> Float.is_finite x | None -> false)
  | S.Vec l | S.Map l | S.List l -> List.for_all finite l
  | _ -> true

(* ---- files ---- *)

let flow_catalog factories = Kinds.catalog ~version:1 factories

(* the text of the document: a settings graph owns the settings; else the document's are the
   sketch's, saved beside the text *)
let text (doc : Document.t) =
  let workspace = doc.workspace.ws in
  Workspace_doc.to_text (if Contexts.has_settings workspace then workspace
    else { workspace with settings = doc.settings })

let save ~directory ~name ~(doc : Document.t) ~view =
  if sanitize name = "" then Error "preset name is empty" else
  let* () = if finite view then Ok () else Error "viewport contains nonfinite values" in
  let text = text doc in
  let view_text = fst (Flow.Lisp.print [ mk (S.List [ mk (S.Sym "view"); view ]) ]) in
  let target = path ~directory ~name in
  Editor_core.Store.write_text ~filename:target (text ^ "\n" ^ view_text)
  |> Result.map (fun () -> target)

let list ~directory =
  match Sys.readdir directory with
  | exception Sys_error _ -> []
  | files ->
      Array.to_list files
      |> List.filter_map (fun file ->
        if not (Filename.check_suffix file suffix) then None
        else match Unix.stat (Filename.concat directory file) with
          | { Unix.st_mtime; _ } -> Some (Filename.chop_suffix file suffix, st_mtime)
          | exception Unix.Unix_error _ -> None)
      |> List.sort (fun (a, at) (b, bt) ->
        let order = Float.compare bt at in if order <> 0 then order else String.compare a b)

let delete ~directory ~name =
  try Sys.remove (path ~directory ~name); Ok () with Sys_error message -> Error message

let load_with_ops ?gpu ~ops ~path ?(factories = Sop.Nodes.factories) ~settings () =
  let* text = Editor_core.Store.read_text ~filename:path in
  let diagnostics ds = String.concat "\n" (List.map Diagnostic.to_string ds) in
  let* catalog = Result.map_error Diagnostic.to_string (flow_catalog factories) in
  let* workspace = Result.map_error diagnostics
      (Workspace_doc.of_text ~ops ~settings:(Settings.defaults settings) catalog text) in
  (* the view is the preset's own form, not the document's *)
  let workspace = { workspace with extra = [] } in
  let* view = match S.parse text with
    | Error d -> Error (Diagnostic.to_string d)
    | Ok forms ->
        (match List.find_opt (fun (f : S.t) -> match f.node with
           | S.List ({ S.node = S.Sym "view"; _ } :: _) -> true | _ -> false) forms with
         | Some { S.node = S.List [ _; v ]; _ } -> Ok v
         | Some _ -> Error "view: expected one value"
         | None -> Ok (mk (S.Map []))) in
  let* doc = Result.map_error Diagnostic.to_string (Contexts.of_workspace ?gpu ~factories workspace) in
  let* () = Document.validate doc in
  Ok { doc; view }

let load ?gpu ~path ?(factories = Sop.Nodes.factories) ~settings () = load_with_ops ?gpu ~ops:Flow_sop.Operators.all ~path ~factories ~settings ()
