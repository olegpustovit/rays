open Rays
open Editor_document
open Environment_model

(* The source file, at most every half second: a changed text reloads the document.  Unsaved work
   is never replaced: while the document differs from the file's last text or a text pane holds a
   draft, the file's new text waits ([held]) and the strip says how to choose.  It loads once the
   edits are undone, or at once by "Reload sketch from its file" (the palette).  Nothing is polled
   while a payload is carried: the document shown is a preview then. *)
let open_import value (update : (_, _) Core_model.update) = match update.core.Core_model.open_import, value.source with
  | Some path, Some source ->
      let core = {update.core with Core_model.open_import = None} in
      if Core_text.unsaved core then
        value, {update with core = {core with Core_model.notice = Some (Core_model.Refusal,
          "Save or undo your changes before opening " ^ path ^ ".")}}
      else
        let file = if Filename.is_relative path
          then Filename.concat (Filename.dirname (Source_file.file source)) path else path in
        (match Source_file.read file with
         | Error message -> value, {update with core = {core with Core_model.notice = Some (Core_model.Refusal, message)}}
         | Ok original ->
             let text = original in
             (match Core_host.reload ~imports:[] core ~name:(Filename.basename file) text with
              | Error ds -> value, {update with core = Core_host.reload_failed core ~name:path text ds}
              | Ok core ->
                  let core = {core with Core_model.file = Filename.basename file} in
                  {value with source = Some (Source_file.at ~file ~digest:(Source_file.sha original));
                    held = None; refused = None}, {update with core; scene_changed = true}))
  | _ -> value, update

(* Command-O: the system dialog over the sketch's folder; its answer, a frame later, is opened
   the way an import is (unsaved work refuses it). *)
let open_dialog value (update : (_, _) Core_model.update) events =
  let notice kind text = { update with core = { update.core with Core_model.notice = Some (kind, text) } } in
  let value, update = match value.source with
    | _ when not (List.mem Leader.Open_source update.actions) -> value, update
    | None -> value, notice Core_model.Refusal "This editor was not opened from a .rays file."
    | Some source ->
        (match Sketch.show_file_dialog ~filters:[ "Rays sketch", [ "rays" ] ]
                 ~default_location:(Filename.dirname (Source_file.file source)) Sketch.Open_file with
         | Ok id -> { value with open_dialog = Some id }, update
         | Error message -> value, notice Core_model.Refusal ("Open: " ^ message)) in
  List.fold_left (fun (value, (update : (_, _) Core_model.update)) -> function
    | Input.File_dialog { id; result } when value.open_dialog = Some id ->
        { value with open_dialog = None },
        (match result with
         | Ok (path :: _) -> { update with core = { update.core with Core_model.open_import = Some path } }
         | Ok [] -> update
         | Error message -> notice Core_model.Refusal ("Open: " ^ message))
    | _ -> value, update) (value, update) events

let reload_source value (update : (_, _) Core_model.update) ~now = match value.source with
  | Some previous when not (Core_host.carrying update.core) ->
      let file, read = Source_file.poll ~now previous in
      let name = Filename.basename (Source_file.file file) in
      let core = if file.error = previous.error then update.core else
        { update.core with Core_model.notice = Some (match file.error with
          | Some message -> Core_model.Refusal, "Source unreadable: " ^ message
          | None -> Core_model.Info, "Source readable again: " ^ name) } in
      let forced = List.mem Leader.Reload_source update.actions in
      let text = match read, value.held with
        | Some _, _ -> read
        | None, None when forced -> Result.to_option (Editor_core.Store.read_text ~filename:(Source_file.file file))
        | None, held -> held in
      let value = { value with source = Some file } in
      (match text with
       | None -> value, { update with core }
       | Some text when not forced && Core_text.unsaved ?except:value.refused core ->
           { value with held = Some text },
           { update with core = if read = None then core else
             { core with Core_model.notice = Some (Core_model.Info, name ^ " changed on disk · your unsaved edits are kept · \
               undo them or run \"Reload sketch from its file\" to take the file; Command-S saves yours as a preset") } }
       | Some text ->
           (match Result.bind (Source_file.read_imports ~file:(Source_file.file file) text)
              (fun imports -> Core_host.reload ~imports core ~name text) with
            | Ok core ->
                { value with source = Some (Source_file.with_imports (Source_file.accepted file text)
                    (Editor_document.Workspace_doc.import_texts (core.Core_model.doc.workspace.ws))); held = None; refused = None },
                { update with core; scene_changed = true }
            | Error diagnostics ->
                { value with held = None; refused = Some text },
                { update with core = Core_host.reload_failed core ~name text diagnostics }))
  | _ -> value, update

(* Command-S: over the source file while it is what the document came from, else a preset. *)
let save_source core source view =
  let notice ?(kind = Core_model.Info) text = { core with Core_model.notice = Some (kind, text) } in
  let preset why = match Preset.save ~directory:core.Core_model.presets ~name:(Preset.default_name ())
      ~doc:core.doc ~view with
    | Ok path -> notice (why ^ "; saved as preset " ^ Filename.basename path)
    | Error message -> notice ~kind:Core_model.Refusal ("Not saved: " ^ message) in
  let text = Preset.text core.doc in
  match source with
  | None -> preset "no source file", source
  | Some file ->
      (match Source_file.save file text with
       | Ok file -> Core_text.filed (notice ("Saved " ^ Filename.basename (Source_file.file file))), Some file
       | Error `Changed -> preset "source changed since build", Some file
       | Error (`Failed message) -> notice ~kind:Core_model.Refusal ("Not saved: " ^ message), Some file)

(* One recovery file per sketch. Polling at 2 Hz coalesces live drags and
   keeps printing and filesystem work out of idle frames; close flushes. *)
let autosave ?(force = false) value ~now =
  if not force && now >= value.state_checked && now -. value.state_checked < 0.5 then value else
  let view = V.section value.camera value.extra in
  let value = { value with state_checked = now } in
  (* a preview document is never saved *)
  if (value.core.doc == value.saved_doc && view = value.saved_view) || Core_host.carrying value.core then value else
  (* the recovery file of an earlier session holds its unsaved work: orbiting in a document that
     is still the one opened never writes over it *)
  if not value.state_owned && Core_text.same_text value.core.doc value.opened
     && Sys.file_exists (Core_setup.state_file value.core)
  then { value with saved_doc = value.core.doc; saved_view = view } else
  match Preset.save ~directory:(Core_setup.state_directory value.core) ~name:value.core.state_name
      ~doc:value.core.doc ~view with
  | Ok _ -> { value with saved_doc = value.core.doc; saved_view = view; state_error = None; state_owned = true }
  | Error message -> { value with state_error = Some ("Autosave failed: " ^ message) }
