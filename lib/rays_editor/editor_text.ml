open Editor_model
open Editor_document
open Editor_inspect
open Editor_shell
open Editor_scope
open Editor_panes
open Editor_actions

(* Every editor path records the same immutable document. No-op/rejected
   edits leave both the present and the merge state untouched. *)
let commit ?(label = "Edit") ?(merge = Editor_core.History.Step) doc history =
  if doc == Editor_core.History.present history then history
  else Editor_core.History.record ~label ~merge doc history

let parameter_gesture operation level id values =
  operation ^ "/" ^ level_key level ^ "/" ^ string_of_int id ^ "/"
  ^ String.concat "/" (List.sort_uniq String.compare (List.map fst values))

(* A workspace edit installed as the present: the source and its lowering are
   one history entry, the open level is re-resolved and the cook told about
   the new volatile nodes. *)
let install value doc ~label ~merge =
  let level = Document.resolve_level doc (pane_level value) in
  let value' = { value with doc; lisp_vocab = vocabulary_for value doc;
    workspace = unrestore value.doc doc value.workspace } in
  Cook.set_volatile value.cook (Flow_sop.Lower.is_volatile (doc.workspace.lowered));
  with_pane { value' with history = commit ~label ~merge doc value.history } (fun pane ->
    { pane with at = level; picked = if level = pane.at then pane.picked else Selection.empty })

(* The text pane's applies: the whole workspace text, or one
   binding's expression; atomic, one history entry "Edit text". *)
let text_edit ?imports ?(label = "Edit text") ?(merge = Editor_core.History.Step) value text =
  Result.map (fun doc -> install value doc ~label ~merge)
    (Editor_document.Document_edit.text_edit ?imports ~factories:value.factories value.doc text)

(* Selection patches root bindings into one candidate graph, checked and
   installed once. Omitted bindings stay and the shown result cannot change.
   Without a closure the tab shows the graph form, replaced whole. *)
let binding_edit ?(merge = Editor_core.History.Step) value path text =
  let at_line_1 (d : Diagnostic.t) = { d with position = Some { line = 1; col = 0 }; span = None } in
  let ws = value.doc.Document.workspace.ws in
  match Text_pane.graph_op ws.source ~graph:(List.hd path) ~selection:path text with
  | Error d -> Error [ (if d.position = None then at_line_1 d else d) ]
  | Ok op ->
      (match Editor_document.Document_edit.syntax_edit_result ~factories:value.factories value.doc op with
       | Ok doc -> Ok (install value doc ~label:"Edit text" ~merge)
       | Error d -> Error [ at_line_1 d ])

(* The Graph tab's apply: the draft must be the one graph (or function) form, which replaces the
   graph's; atomic like every apply. *)
let graph_edit ?(merge = Editor_core.History.Step) value name text =
  let ws = value.doc.Document.workspace.ws in
  match Text_pane.graph_op ws.source ~graph:name text with
  | Error d ->
      Error [ (if d.position = None then { d with position = Some { line = 1; col = 0 } } else d) ]
  | Ok op ->
      (match Editor_document.Document_edit.syntax_edit_result ~factories:value.factories value.doc op with
       | Ok doc -> Ok (install value doc ~label:"Edit text" ~merge)
       | Error d -> Error [ { d with position = Some { line = 1; col = 0 }; span = None } ])

(* Fold the pane's intents: drafts live in [value.text] (view state); a
   refused apply keeps the draft and its errors and changes nothing else. *)
let scrub_merge = Editor_core.History.Gesture "text-scrub"

let select_source value path = match path with
  | [] -> value
  | graph :: _ when String.starts_with ~prefix:"macro:" graph -> value
  | graph :: _ ->
      let locals = List.map (fun (key, (local : local)) ->
        key, {local with code = {local.code with target = None}}) value.locals in
      let next = sync_scope (go {value with text = {value.text with target = None}; locals} graph) in
      match pane_laid next with
      | Some {scope; graph = current; _} when current = graph &&
          (Flow_graph.Projection.find scope path <> None
           || List.exists (fun (input : Flow_graph.Projection.input) -> input.path = path) scope.inputs) ->
          if Pxui_graph.Scope.selected (pane_view next) = [path] then next else
          with_pane {next with select_later = []} (fun pane ->
            { pane with view = fst (Pxui_graph.Scope.run_command (Pxui_graph.Scope.select [path] pane.view)
              Pxui_graph.Scope.Frame_selection) })
      | _ when List.length path = 1 -> with_pane next (fun pane -> { pane with view = Pxui_graph.Scope.select [] pane.view })
      | _ -> value

let apply_text value intents =
  List.fold_left (fun value intent ->
    let intent, literal = match intent with
      | Text_pane.Literal_scrub {source; op; position; fallback} -> fallback, Some (source, op, position)
      | intent -> intent, None in
    let text = match intent with
      | Text_pane.Doc_scrub _ | Graph_scrub _ | Binding_scrub _ -> value.text
      | _ -> {value.text with scrubbing = false;
          cache = (if value.text.scrubbing then None else value.text.cache)} in
    let with_text text = { value with text } in
    let workspace = value.doc.Document.workspace.ws in
    let base old = Some (Option.value ~default:workspace old) in
    (* a Graph tab's slot: each graph keeps its own draft, base and errors *)
    let slot graph = Text_pane.graph_slot text graph in
    let put graph = function
      | Some s -> { text with Text_pane.graphs = (graph, s) :: List.remove_assoc graph text.Text_pane.graphs }
      | None -> { text with graphs = List.remove_assoc graph text.graphs } in
    let failed graph draft errors = Some { Text_pane.gtext = draft;
      gbase = base (Option.bind (slot graph) (fun s -> s.Text_pane.gbase)); gerrors = { at = draft; found = errors } } in
    (* a draft begun on an older document: merged with what changed since on the syntax tree
       ({!Text_pane.merge3}); only a value both changed keeps the draft as a conflict *)
    let checked ?(metadata = false) ~shown ~draft old apply =
      match old with
      | Some (previous : Editor_document.Workspace_doc.t) when previous.source != workspace.source
          || (metadata && (previous.layout != workspace.layout || previous.settings != workspace.settings)) ->
          (match Text_pane.merge3 ~base:(shown previous) ~mine:draft ~theirs:(shown workspace) with
           | Some merged -> apply merged
           | None ->
               Error [Diagnostic.error ~position:{line = 1; col = 0} ~code:"E_DRAFT_CONFLICT"
                 "The document changed in the same place as this draft. Your draft is kept; discard it and reapply your edits to the current text."])
      | _ -> apply draft in
    let whole = Editor_document.Workspace_doc.to_text in
    let of_graph graph (ws : Editor_document.Workspace_doc.t) =
      (Text_pane.make_shown ws.source graph None Text_pane.Graph).text in
    let checked_binding ?(merge = Editor_core.History.Step) path ~draft old apply =
      match old with
      | Some (previous : Workspace_doc.t) when previous.source != workspace.source ->
          (* Selection can patch a root binding outside the printed upstream closure.
             Merge the complete candidate graph so a concurrent edit of that binding
             cannot disappear just because it was outside the printed closure. *)
          (match Text_pane.graph_op previous.source ~graph:(List.hd path) ~selection:path draft with
           | Error d -> Error [d]
           | Ok (Flow_graph.Flow_edit.Set_graph {name; form}) ->
               let mine = fst (Flow.Lisp.print [form]) in
               (match Text_pane.merge3 ~base:(of_graph name previous) ~mine ~theirs:(of_graph name workspace) with
                | Some merged -> graph_edit ~merge value name merged
                | None -> Error [Diagnostic.error ~position:{line = 1; col = 0} ~code:"E_DRAFT_CONFLICT"
                    "The document changed in the same place as this draft. Your draft is kept; discard it and reapply your edits to the current text."])
           | Ok _ -> apply draft)
      | _ -> apply draft in
    let scrub apply draft = match literal with
      | Some (source, op, position) when source == workspace.source ->
          Editor_document.Document_edit.syntax_edit_result ~factories:value.factories value.doc op
          |> Result.map (fun doc -> install value doc ~label:"Edit text" ~merge:scrub_merge)
          |> Result.map_error (fun d -> [{d with Diagnostic.position = Some position; span = None}])
      | _ -> apply draft in
    let finished done_ draft previous current = done_ && current = Some draft
      && Option.fold ~none:false ~some:(fun previous -> previous == workspace) previous in
    match intent with
    | Text_pane.Tab tab -> with_text { text with tab }
    | Menu menu -> with_text { text with menu }
    | Toggle_wrap -> with_text { text with wrap = not text.wrap }
    | Toggle_parinfer -> with_text { text with parinfer = not text.parinfer }
    | Picker picker -> with_text { text with picker }
    | Open_graph graph -> go value graph
    | Select_binding path ->
        select_source {value with text = {text with target = None}} path
    | Jump_definition path ->
        let next = select_source value path in
        {next with text = {text with target = Some path; nonce = text.nonce + 1}}
    | References_of _ -> value  (* opens the references prompt, see [Editor_reduce] *)
    | Carry_over _ -> value  (* read by the carry's report, not an edit *)
    | Find_set (find, reveal) -> with_text { text with find; nonce = text.nonce + (if reveal then 1 else 0) }
    | Request request -> with_text { text with request }
    | Resolved -> with_text { text with pending = [] }
    | Doc_draft draft -> with_text { text with draft = Some draft; doc_base = base text.doc_base; scrubbing = false }
    | Doc_discard -> with_text { text with draft = None; doc_base = None; doc_errors = Text_pane.no_errors }
    | Doc_apply draft ->
        (match checked ~metadata:true ~shown:whole ~draft text.doc_base (text_edit value) with
         | Ok value -> { value with text = { text with draft = None; doc_base = None; doc_errors = Text_pane.no_errors } }
         | Error found -> with_text { text with draft = Some draft; doc_base = base text.doc_base; doc_errors = { at = draft; found } })
    | Binding_draft (path, draft) ->
        with_text { text with binding_draft = Some (path, draft); binding_base = base (match text.binding_draft with Some (old, _) when old = path -> text.binding_base | _ -> None) }
    | Binding_discard -> with_text { text with binding_draft = None; binding_base = None; binding_errors = Text_pane.no_errors }
    | Graph_draft (graph, draft) ->
        let old = slot graph in
        with_text (put graph (Some { Text_pane.gtext = draft; gbase = base (Option.bind old (fun s -> s.Text_pane.gbase));
          gerrors = (match old with Some s -> s.gerrors | None -> Text_pane.no_errors) }))
    | Graph_discard graph -> with_text (put graph None)
    | Live live -> with_text { text with live = Some live }
    | Graph_apply (graph, draft) ->
        (match checked ~shown:(of_graph graph) ~draft (Option.bind (slot graph) (fun s -> s.gbase)) (graph_edit value graph) with
         | Ok value -> { value with text = put graph None }
         | Error found -> with_text (put graph (failed graph draft found)))
    | Binding_apply (path, draft) ->
        (match checked_binding path ~draft text.binding_base (binding_edit value path) with
         | Ok value -> { value with text = { text with binding_draft = None; binding_base = None; binding_errors = Text_pane.no_errors } }
         | Error found ->
             with_text { text with binding_draft = Some (path, draft); binding_base = base text.binding_base; binding_errors = { at = draft; found } })
    (* a dragged number: the text applies on every frame of the drag as one "Edit text" entry
       (the gesture seals on release); the draft stays until the drag ends so the editor keeps
       the text it is dragging in *)
    | Doc_scrub (draft, done_) ->
        (match (if finished done_ draft text.doc_base text.draft then Ok value else
          checked ~metadata:true ~shown:whole ~draft text.doc_base (scrub (text_edit ~merge:scrub_merge value))) with
         | Ok value -> { value with text = { text with draft = (if done_ then None else Some draft); doc_base = (if done_ then None else Some (value.doc.workspace.ws)); doc_errors = Text_pane.no_errors; scrubbing = not done_ && Option.is_some literal; cache = (if done_ then None else text.cache) } }
         | Error found -> with_text { text with draft = Some draft; doc_base = base text.doc_base; doc_errors = { at = draft; found }; scrubbing = false })
    | Graph_scrub (graph, draft, done_) ->
        let gbase = Option.bind (slot graph) (fun s -> s.Text_pane.gbase) in
        (match (if finished done_ draft gbase (Option.map (fun s -> s.Text_pane.gtext) (slot graph)) then Ok value else
          checked ~shown:(of_graph graph) ~draft gbase (scrub (graph_edit ~merge:scrub_merge value graph))) with
         | Ok value ->
             let kept = if done_ then None else Some { Text_pane.gtext = draft; gbase = Some (value.doc.workspace.ws); gerrors = Text_pane.no_errors } in
             { value with text = { (put graph kept) with scrubbing = not done_ && Option.is_some literal; cache = (if done_ then None else text.cache) } }
         | Error found -> with_text { (put graph (failed graph draft found)) with scrubbing = false })
    | Binding_scrub (path, draft, done_) ->
        (match (if finished done_ draft text.binding_base (Option.bind text.binding_draft (fun (p,d) -> if p = path then Some d else None)) then Ok value else
          checked_binding ~merge:scrub_merge path ~draft text.binding_base (scrub (binding_edit ~merge:scrub_merge value path))) with
         | Ok value -> { value with text = { text with binding_draft = (if done_ then None else Some (path, draft)); binding_base = (if done_ then None else Some (value.doc.workspace.ws)); binding_errors = Text_pane.no_errors; scrubbing = not done_ && Option.is_some literal; cache = (if done_ then None else text.cache) } }
         | Error found -> with_text { text with binding_draft = Some (path, draft); binding_base = base text.binding_base; binding_errors = { at = draft; found }; scrubbing = false })
    | Literal_scrub _ -> value) value intents

(* The intents of a text pane that is not the one in use fold into its own state. *)
let apply_text_at value key intents =
  if intents = [] then value else
  let mine = value.text in
  let v = apply_text { value with text = (local_of value key).code } intents in
  let mine = if List.exists (function Text_pane.Select_binding _ | Jump_definition _ -> true | _ -> false) intents
    then {mine with target = None} else mine in
  { v with text = mine; locals = put_local key (fun l -> { l with code = v.text }) v.locals }

(* The unapplied drafts of a text pane, as the applies its Check & apply would make. *)
let draft_applies ?except (text : Text_pane.state) =
  (match text.draft with Some d when Some d <> except -> [ Text_pane.Doc_apply d ] | _ -> [])
  @ List.map (fun (g, (s : Text_pane.slot)) -> Text_pane.Graph_apply (g, s.gtext)) text.graphs
  @ (match text.binding_draft with Some (p, d) -> [ Text_pane.Binding_apply (p, d) ] | None -> [])

(* Some text pane holds text that is not in the document ([except]: the text of a refused
   reload, which is the file's and not the user's). *)
let has_draft ?except value =
  draft_applies ?except value.text <> []
  || List.exists (fun (_, (l : local)) -> draft_applies ?except l.code <> []) value.locals

(* What Command-S writes: the workspace text and, without a settings graph, the settings. *)
let same_text (a : Document.t) (b : Document.t) =
  a.workspace.ws == b.workspace.ws && a.settings == b.settings

(* Work a reload of the source file would lose: edits since the file's text, or a draft. *)
let unsaved ?except value = not (same_text value.doc value.filed) || has_draft ?except value

(* The document is the file's text now (saved over it). *)
let filed value = { value with filed = value.doc }

(* Command-S with a draft: every draft is applied first (each one its "Edit text" entry); a draft
   that does not check stays with its errors, and [Error] says the save must wait. *)
let apply_drafts ?except value =
  let value = apply_text value (draft_applies ?except value.text) in
  let value = List.fold_left (fun v (key, (l : local)) -> apply_text_at v key (draft_applies ?except l.code))
    value value.locals in
  if has_draft ?except value then Error value else Ok value
