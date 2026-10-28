(* The .rays file a running sketch was built from: found once, polled for edits,
   saved over.  Immutable; the caller keeps the returned value.  [digest] is the
   SHA-256 of the text the running document last came from (built from, reloaded
   or saved), so an edit from any other writer differs from it. *)

type t = { file : string; digest : string; observed : string option; polled : float;
           error : string option; imports : (string * t) list }

let sha = Editor_document.Contexts.sha256
let stat file = try Some (Unix.stat file) with Unix.Unix_error _ -> None
let read file = Editor_core.Store.read_text ~filename:file

let interval = 0.5

(* the file as it is now, whose text has [digest] *)
let synced ~file ~digest =
  { file; digest; observed = Some digest; polled = neg_infinity; error = None; imports = [] }

(* The first poll reads the file, so a file that already differs from
   the text the sketch was built from (edited since the build) reloads at once. *)
let at ~file ~digest = { (synced ~file ~digest) with observed = None }

(* the first [dune-project] at or above [dir] that is not inside a [_build] *)
let rec root dir =
  if not (List.mem "_build" (String.split_on_char '/' dir))
     && Sys.file_exists (Filename.concat dir "dune-project") then Some dir
  else let parent = Filename.dirname dir in if parent = dir then None else root parent

let find ~path ~digest =
  let absolute p = if Filename.is_relative p then Filename.concat (Sys.getcwd ()) p else p in
  List.find_map (fun start ->
    Option.bind (root start) (fun dir ->
      let file = Filename.concat dir path in
      if Sys.file_exists file then Some (at ~file ~digest) else None))
    [ Filename.dirname (absolute Sys.executable_name); Sys.getcwd () ]
  (* ponytail: a sketch launched from outside its project has no source and only presets *)

let file t = t.file

let read_imports ~file text =
  Result.bind (Editor_document.Workspace_doc.import_paths text) (fun paths ->
    List.fold_left (fun result path -> Result.bind result (fun imports ->
      match read (Filename.concat (Filename.dirname file) path) with
      | Ok text -> Ok (imports @ [path, text])
      | Error message -> Error [Diagnostic.error ~code:"E_IMPORT" (path ^ ": " ^ message)])) (Ok []) paths)

let with_imports t imports = {t with imports = List.map (fun (path, text) -> path,
  at ~file:(Filename.concat (Filename.dirname t.file) path) ~digest:(sha text)) imports}

(* One content read per interval, independent of mtime/inode. [observed]
   suppresses repeated attempts to reload the same refused text. *)
let rec poll ~now t =
  if now -. t.polled < interval then t, None
  else
    let t = { t with polled = now } in
    match read t.file with
    | Error message -> { t with error = Some message }, None
    | Ok text ->
        let digest = sha text in
        let changed = t.observed <> Some digest && (t.observed <> None || digest <> t.digest) in
        let imports, imported_change = List.fold_left (fun (imports, changed) (path, source) ->
          let old_error = source.error in
          let source, fresh = poll ~now source in
          (path, source) :: imports, changed || fresh <> None || source.error <> old_error) ([], false) t.imports in
        let imports = List.rev imports in
        { t with observed = Some digest; error = None; imports }, (if changed || imported_change then Some text else None)

let accepted t text = { t with digest = sha text }

(* Only over the text the document came from; the file keeps its permissions
   (a temporary file is created 0600). The digest check and rename cannot
   exclude another editor writing between them; that requires a cooperative
   writer protocol, not an atomic rename alone. *)
let save t text =
  let text = if String.ends_with ~suffix:"\n" text then text else text ^ "\n" in
  match read t.file with
  | Ok disk when sha disk = t.digest ->
      let perm = Option.fold ~none:0o644 ~some:(fun s -> s.Unix.st_perm) (stat t.file) in
      Result.map (fun () ->
        (try Unix.chmod t.file perm with Unix.Unix_error _ -> ());
        { (synced ~file:t.file ~digest:(sha text)) with polled = t.polled; imports = t.imports })
        (Editor_core.Store.write_text ~filename:t.file text)
      |> Result.map_error (fun m -> `Failed m)
  | Ok _ -> Error `Changed
  | Error message -> Error (`Failed message)
