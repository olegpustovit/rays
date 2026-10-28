open Editor_model
open Rays
open Sop
open Editor_document
open Editor_panes
open Editor_setup
open Editor_panels
open Editor_list
open Editor_inspect
open Editor_actions
open Editor_shell
open Editor_scope
open Editor_text
open Editor_carry

(* The one reduction phase of a frame: what the panes asked for ([result]) and the keys ([actions])
   become the next model.  Nothing here builds UI: the panes have finished constructing their
   boxes, and a test can run it on a frame result it made itself. *)
(* a row of the Navigator opens its graph in the graph panel *)
let outline_step ~created ~(result : _ frame_result) value' =
  (* a row of the Navigator opens its graph in the graph panel, selecting a node there *)
  let outlined = List.find_map (function
    | Navigator.Open { graph; node } -> Some (graph, node)
    | _ -> None) result.outline_intents in
  (* / j, or a material just made, shows its graph like a row of the outline *)
  let outlined = match outlined, result.prompt_intent, created with
    | None, Some (Go graph), _ | None, _, Some graph -> Some (graph, None)
    | _ -> outlined in
  let value' = { value' with outline = result.outline;
    select_later = (match outlined with Some (_, Some node) -> [ node ] | _ -> value'.select_later);
    text = if List.exists (function Navigator.Macro _ -> true | _ -> false) result.outline_intents
      then { value'.text with Text_pane.tab = Text_pane.Document } else value'.text } in
  (* a node of the graph already shown is selected now; of another graph, once the pane shows it *)
  let value' = match outlined with
    | Some (graph, Some node) when graph_name value' = Some graph
        && (match pane_laid value' with Some k -> k.graph = graph | None -> false) ->
        with_pane { value' with select_later = [] } (fun pane ->
          { pane with view = fst (Pxui_graph.Scope.run_command (Pxui_graph.Scope.select [ node ] pane.view)
            Pxui_graph.Scope.Frame_selection) })
    | _ -> value' in
  let value' = match outlined with Some (graph, _) -> go value' graph | None -> value' in
  value', outlined

(* Entering and leaving levels: the graph a reference follows, the level i / u / list activation open *)
let level_target ~(value : t) ~value' ~level ~(doc : Document.t) ~selection ~actions (result : _ frame_result) =
  (* Entering and leaving levels: i / double-click / list activation, u. *)
  (* i also follows a reference of the node selected in the pane (a material, an object's graph) *)
  let followed = match List.find_map (function
      | Pxui_graph.Scope.Activated path -> Some path | _ -> None) result.scope_changes with
    | Some path -> follow_target ~path value
    | None when List.mem Editor_commands.Enter actions ->
        (match follow_target value with Some _ as found -> found | None -> viewport_target value)
    | None -> None in
  let target = match List.find_opt (function
      | Editor_commands.Enter | Up | Go_world -> true | _ -> false) actions, result.opened with
    | _ when followed <> None -> None
    | Some Editor_commands.Up, _ when (pane_route value) <> [] -> None
    | Some Editor_commands.Up, _ -> (match level with
      | Document.Inside _ -> Some Document.Scene
      | Scene -> None)
    | Some Editor_commands.Go_world, _ ->
        Option.map (fun id -> Document.Inside id)
          (List.find_opt (enterable value') (Objects.ids "world" doc.scene))
    | Some Editor_commands.Enter, _ | _, Some _ ->
        let candidate = match result.opened with
          | Some id -> Some id | None -> Selection.selected selection in
        (match level, candidate with
         | Document.Scene, Some id when enterable value' id -> Some (Document.Inside id)
         | _ -> None)
    | _ -> None in
  followed, target

(* the history key of a gesture still in progress, if the frame's change belongs to one *)
let gesture_key ~(value : t) ~(frame : Frame.t) ~loaded ~world_added ~world_label (result : _ frame_result) =
  let dragging = Frame.mouse_down Input.LeftButton frame || Frame.has_event (function
    | Input.Pointer_released (Input.LeftButton, _) -> true | _ -> false) frame in
  let gesture = if not dragging || Option.is_some loaded
      || world_added || world_label <> None then None else
    match List.find_map (function
      | Set_parameter { node; path; _ } ->
          Some (Printf.sprintf "graph.scrub:%s:%d:%s" (level_key (pane_level value)) node path)
      | Syntax_edit op -> Flow_graph.Flow_edit.gesture op
      | Layout_edit op -> Layout_edit.gesture op
      | Panel_state (path, _) -> Some ("panel:" ^ String.concat "/" (panel_key value.doc path))
      | Dock_panels (path, _, _) -> Some ("panel:" ^ String.concat "/" (panel_key value.doc path))
      | _ -> None) result.changes with
    | Some key -> Some key
    | None ->
    let graph = List.filter_map (function
      | change when intent_label change <> None -> Some []
      | _ -> None) result.changes in
    match graph, result.tree_intents, result.handle_changes,
        result.settings_changes with
    | [], [], Some (id, changes), [] ->
        Some (parameter_gesture "handle" (pane_level value) id changes)
    | [], [], None, (_ :: _ as changes) ->
        Some (parameter_gesture "settings" (pane_level value) (-1) changes)
    | [], (Pxui_shell.Tree.Flag { column; _ } :: _ as intents), None, []
      when List.for_all (function Pxui_shell.Tree.Flag _ -> true | _ -> false) intents ->
        (* a stroke down a flag column of the list is one entry *)
        Some (Printf.sprintf "list.flag:%s:%d" (level_key (pane_level value)) column)
    | _ -> None in
  gesture

(* the graph pane's layout gestures: moving, collapsing, levels, frames, probes *)
let layout_gestures ~(value : t) ~loaded ~added next result scope_changes =
  let module M = Layout_by_path.Path_map in
  let next, result, probes = if Option.is_some loaded then next, result, value.probes else
    List.fold_left (fun (next, result, probes) -> function
      | Pxui_graph.Scope.Zone_collapsed { zone; collapsed } ->
          (* opening a frame that starts collapsed is stored, so it stays open *)
          (* an expression starts folded: opening it is stored too *)
          let default = Flow_graph.Flow_edit.nested (List.nth zone (List.length zone - 1)) in
          Editor_document.Document_edit.layout_edit next (fun l -> { l with collapsed =
            if collapsed then M.add zone true l.collapsed
            else if default then M.add zone false l.collapsed else M.remove zone l.collapsed }),
          { (result : _ frame_result) with label = if collapsed then "Collapse zone" else "Expand zone" }, probes
      | Moved placed ->
          Editor_document.Document_edit.layout_edit next (fun l -> { l with at = List.fold_left (fun at (path, x, y) ->
            M.add path (x, y) at) l.at placed }),
          { result with label = (if added = [] then "Move" else result.label) }, probes
      | Level_set changes ->
          Editor_document.Document_edit.layout_edit next (fun l -> List.fold_left (fun (l : Layout_by_path.t) (path, level, pinned) ->
            { l with
              level = (match level with Some v -> M.add path v l.level | None -> M.remove path l.level);
              pinned = if pinned then M.add path true l.pinned else M.remove path l.pinned }) l changes),
          { result with label = "Detail level" }, probes
      | Probe_set { zone; index } -> next, result, M.add zone index probes
      | Frames_set { scope; frames } ->
          Editor_document.Document_edit.layout_edit next (fun l -> { l with frames =
            if frames = [] then M.remove scope l.frames
            else M.add scope (List.map (fun (title, at, size) -> { Layout_by_path.title; at; size }) frames) l.frames }),
          { result with label = "Frame" }, probes
      | Display_set _ -> next, result, probes
      | Syntax_edit _ | Layout_active _ | Selected _ | Notice _ | Macro_requested _ | Defn_requested _
      | Copy_requested _ | Paste_requested | Menu_requested _ | Open_import _ | Activated _ | Drop_over _ | Dropped _ ->
          next, result, probes)
      (next, result, value.probes) scope_changes in
  next, result, probes

(* the scene and World edits act on derived objects: each difference is written to
   the text (a refused one changes nothing) *)
let reconciled ~(value : t) ~before next (result : _ frame_result) =
  match Editor_document.Document_edit.reconcile ~factories:value.factories before next with
  | Ok doc -> doc, result
  | Error message ->
      before, { (result : _ frame_result) with edit_error = Some message }

(* what a frame's gestures leave besides the document *)
type gesture_log = {
  added : Flow.Workspace.path list; created : string option; renamed : (string * string) option;
  refused : string option; note : string option }

(* the frame's changes and gestures written to the document, text first: the first refusal leaves
   none applied.  Returns the document, the result, the nodes added, a graph made, a graph renamed
   and the note the echo shows *)
let write_gestures ~(value : t) ~loaded ~present ~tree_edits ~in_text ~node_effects next (result : _ frame_result) =
  let next, result = if Option.is_some loaded then next, result else
    reconciled ~value ~before:present next result in
  (* Workspace gestures: one rewrite of the source per gesture, lowered into
     the document, one history entry named by the op.  A frame's gestures land together: the
     first one refused leaves none of them applied, and a later one cannot hide its message. *)
  let gestures = next, result in
  let first_note g note = if g.note = None then { g with note } else g in
  let set_fields ((next : Document.t), (result : _ frame_result), g) node values =
    match Editor_document.Scene_sync.set_fields ~factories:value.factories next (pane_level value) ~node values with
    | Ok (Some (doc, effects, home)) ->
        doc, { result with edit_error = None;
               effects = Param.union_effects result.effects (node_effects effects) },
        first_note g (Editor_document.Scene_sync.template_note next home)
    | Ok None -> next, result, g
    | Error message -> next, result, { g with refused = Some message } in
  let start = { added = []; created = None; renamed = None; refused = None; note = None } in
  let next, result, g = if Option.is_some loaded then next, result, start else
    List.fold_left (fun ((next : Document.t), (result : _ frame_result), g) edit ->
      if g.refused <> None then next, result, g else
      match Editor_document.Scene_sync.write ~factories:value.factories next (pane_level value) edit with
      | Ok (Some (doc, note)) -> doc, { result with edit_error = None }, first_note g note
      | Ok None -> next, result, g
      | Error message -> next, result, { g with refused = Some message }) (next, result, start) tree_edits in
  (* a viewport handle's values *)
  let next, result, g = match result.handle_changes with
    | Some (id, values) when Option.is_none loaded && in_text id -> set_fields (next, result, g) id values
    | _ -> next, result, g in
  let next, result, g = if Option.is_some loaded then next, result, g else
    List.fold_left (fun ((next : Document.t), result, g) change ->
      let refuse message = next, result, { g with refused = Some message } in
      (* bind what is written in place (the call holding it, then the expression), then
         the gesture on its name *)
      let inline ~home ~key ~make ~apply ~label next (result : _ frame_result) =
          let ( let* ) = Result.bind in
          let bound =
            let* doc, node = Editor_document.Scene_sync.bind_home ~factories:value.factories next home in
            let* doc = Editor_document.Document_edit.syntax_edit ~factories:value.factories doc
              (Flow_graph.Flow_edit.Unfold { node; key; sub = [] }) in
            (match Flow_graph.Flow_edit.arg_text (doc.workspace.ws).source node key with
             | Some { Flow.Syntax.node = Sym name; _ } ->
                 let path = List.rev (name :: List.tl (List.rev node)) in
                 let* doc = apply ~factories:value.factories doc (make path) in
                 Ok (doc, make path)
             | _ -> Error "The panel could not be named.") in
          (match bound with
           | Ok (doc, op) ->
               doc, { result with label = label op;
                 effects = Param.union_effects result.effects Editor_document.Document_edit.cook_effects }, g
           | Error message -> refuse message) in
      if g.refused <> None then next, result, g else match change with
      | Set_parameter { node; path; value = field } when in_text node ->
          set_fields (next, result, g) node [ path, field ]
      | Dock_panels (source, target, side) ->
          (match dock_panels ~factories:value.factories next source target side with
           | Ok doc -> doc, { (result : _ frame_result) with label = "Arrange panel"; edit_error = None }, g
           | Error message -> refuse message)
      | Panel_state (path, state) ->
          let key = panel_key next path in
          let doc = Editor_document.Document_edit.layout_edit next (fun layout ->
            { layout with panels = Layout_by_path.Path_map.add key state layout.panels }) in
          doc, { (result : _ frame_result) with label =
            (if state.collapsed then "Collapse panel" else "Arrange panel"); edit_error = None }, g
      | Select_layout name when Option.map (fun (g : Flow.Workspace.graph) -> g.name)
          (Workspace_doc.editor_graph (next.workspace.ws)) = Some name -> next, result, g
      | Select_layout name ->
          (match Editor_document.Document_edit.select_layout ~factories:value.factories next name with
           | Ok doc -> doc, { (result : _ frame_result) with label = "Switch layout"; edit_error = None }, g
           | Error message -> refuse message)
      | Syntax_edit op ->
          (match Editor_document.Document_edit.syntax_edit ~factories:value.factories next op with
           | Ok doc ->
               (* a math node a wire folded into its reader opens as a card, where it was *)
               let doc = match op with
                 | Flow_graph.Flow_edit.Connect { node; key; src; _ } when node <> [] ->
                     let sp = List.rev (List.tl (List.rev node)) in
                     let leaf = Flow_graph.Flow_edit.nested_leaf (List.nth node (List.length node - 1)) key in
                     (match Flow_graph.Flow_edit.arg_text (doc.workspace.ws).source node key with
                      | Some e when Flow_graph.Flow_edit.value_call e ->
                          let at = Layout_by_path.Path_map.find_opt (sp @ [ src ]) (next.workspace.ws).layout.at in
                          Editor_document.Document_edit.layout_edit doc (fun l ->
                            { l with collapsed = Layout_by_path.Path_map.add (sp @ [ leaf ]) false l.collapsed;
                            at = (match at with Some p -> Layout_by_path.Path_map.add (sp @ [ leaf ]) p l.at | None -> l.at) })
                      | _ -> doc)
                 | _ -> doc in
               let g = match op with
                 | Flow_graph.Flow_edit.Add_node { scope; name; _ } -> { g with added = [ scope @ [ name ] ] }
                 | Rename_graph { name; to_ } -> { g with renamed = Some (name, to_) }
                 | Duplicate { nodes } -> { g with added = Flow_graph.Flow_edit.duplicated (next.workspace.ws).source nodes }
                 | _ -> g in
               doc, { (result : _ frame_result) with label = Flow_graph.Flow_edit.label op; edit_error = None;
               effects = Param.union_effects result.effects Editor_document.Document_edit.cook_effects },
                 first_note g (Option.map (fun words -> "Wrote " ^ words) (Echo.words op))
           | Error message -> refuse message)
      | Layout_edit op ->
          (match Editor_document.Document_edit.layout_op ~factories:value.factories next op with
           | Ok doc ->
               doc, { (result : _ frame_result) with label = Layout_edit.label op; edit_error = None;
               effects = Param.union_effects result.effects Editor_document.Document_edit.cook_effects }, g
           | Error message -> refuse message)
      | Syntax_batch (label, ops) ->
          (match Editor_document.Document_edit.syntax_batch ~factories:value.factories next ops with
           | Ok doc ->
               let g = List.fold_left (fun g -> function
                 | Flow_graph.Flow_edit.Add_node { scope; name; _ } -> { g with added = [ scope @ [ name ] ] }
                 | Set_graph { name; _ } when label = "New material" || label = "New graph" -> { g with created = Some name }
                 | _ -> g) g ops in
               doc, { (result : _ frame_result) with label; edit_error = None;
                 effects = Param.union_effects result.effects Editor_document.Document_edit.cook_effects },
                   first_note g (Option.map (fun words -> "Wrote " ^ words) (Echo.batch ops))
           | Error message -> refuse message)
      | Pin_row { node; label; pin } ->
          let module PM = Layout_by_path.Path_map in
          let module SM = Layout_by_path.String_map in
          let doc = Editor_document.Document_edit.layout_edit next (fun l ->
            let rows = Option.value ~default:SM.empty (PM.find_opt node l.rows) in
            let rows = match pin with Some v -> SM.add label v rows | None -> SM.remove label rows in
            { l with rows = if SM.is_empty rows then PM.remove node l.rows else PM.add node rows l.rows }) in
          doc, { (result : _ frame_result) with label = (match pin with
            | Some false -> "Unpin row from card" | _ -> "Pin row to card"); edit_error = None }, g
      | Object_arg { node; key; sub; expr } ->
          (* the expression typed in a row is the argument of the call that holds the object (of a
             loop's template: every copy changes) *)
          let ( let* ) = Result.bind in
          let home = match pane_level value with
            | Document.Scene -> List.assoc_opt node next.Document.homes.objects
            | Inside _ -> List.assoc_opt node next.homes.layers in
          (match home with
           | None -> refuse "That object is not in the text yet: change one of its values, which writes it."
           | Some home ->
               let op path = Flow_graph.Flow_edit.Set_arg { node = path; key = Kw key; sub; value = expr } in
               (match (let* doc, path = Editor_document.Scene_sync.bind_home ~factories:value.factories next home in
                       Editor_document.Document_edit.syntax_edit ~factories:value.factories doc (op path)) with
                | Ok doc ->
                                        doc, { result with label = "Edit expression";
                      effects = Param.union_effects result.effects Editor_document.Document_edit.cook_effects },
                      { g with note = Editor_document.Scene_sync.template_note next home }
                | Error message -> refuse message))
      | Syntax_inline { home; key; make } ->
          inline ~home ~key ~make ~apply:(fun ~factories -> Editor_document.Document_edit.syntax_edit ~factories)
            ~label:Flow_graph.Flow_edit.label next result
      | Layout_inline { home; key; make } ->
          inline ~home ~key ~make ~apply:(fun ~factories -> Editor_document.Document_edit.layout_op ~factories)
            ~label:Layout_edit.label next result
      | _ -> next, result, g) (next, result, g) result.changes in
  let next, result, g = match g.refused with
    | None -> next, result, g
    | Some _ as edit_error ->
        fst gestures, { (snd gestures : _ frame_result) with edit_error },
        { g with added = []; created = None; renamed = None } in
  next, result, g.added, g.created, g.renamed, g.note

(* the prompt a frame leaves open, the notice, and a preset it loaded *)
let prompt_step ~(value : t) ~view_state (result : _ frame_result) =
  let prompt, notice, loaded = match result.prompt_intent with
    | None -> result.prompt, (match List.find_map (function
        | Notice message -> Some (Info, message) | Declined message -> Some (Refusal, message)
        | _ -> None) result.changes with
        | Some _ as notice -> notice | None -> value.notice), None
    | Some (Run_action _ | Edit_source _ | Go _ | Find_node _) -> result.prompt, value.notice, None
    | Some (Save_preset_file name) ->
        let notice = match Preset.save ~directory:value.presets ~name
            ~doc:value.doc ~view:(view_state result.panel) with
          | Ok path -> Info, "Saved preset " ^ Filename.basename path
          | Error message -> Refusal, "Preset not saved: " ^ message in
        result.prompt, Some notice, None
    | Some ((Load_preset_file _ | Load_last_state) as intent) ->
        let path, restored = match intent with
          | Load_preset_file name -> Preset.path ~directory:value.presets ~name, "Loaded preset " ^ name
          | _ -> state_file value, "Restored last edited state" in
        let rejected = if intent = Load_last_state then "Last edited state rejected: " else "Preset rejected: " in
        (match Preset.load_with_ops ~gpu:value.gpu ~ops:(value.doc.workspace.ws).checked.ops ~path
            ~factories:value.factories ~settings:value.doc.settings () with
         | Ok preset -> result.prompt, Some (Info, restored), Some preset
         | Error message -> result.prompt, Some (Refusal, rejected ^ message), None)
    | Some ((Delete_preset_file _ | Delete_last_state _) as intent) ->
        let directory, name, query, description = match intent with
          | Delete_preset_file {name; query} -> value.presets, name, query, "preset " ^ name
          | Delete_last_state query -> state_directory value, value.state_name, query, "last edited state"
          | _ -> assert false in
        let notice = match Preset.delete ~directory ~name with
          | Ok () -> Info, "Deleted " ^ description
          | Error message -> Refusal, "Preset not deleted: " ^ message in
        browse value query,
        Some notice, None in
  (* [m] in the workspace pane: the make-macro dialog over the selection, its first two
     literals ticked as in the study. *)
  let prompt, notice = match List.find_map (function
      | Pxui_graph.Scope.Macro_requested nodes -> Some nodes | _ -> None) result.scope_changes with
    | Some nodes ->
        (match Flow_graph.Flow_edit.macro_draft (value.doc.Document.workspace.ws).source nodes with
         | Ok draft ->
             Some (Making_macro { nodes; draft; state = { name = draft.name;
               holes = Array.of_list (List.mapi (fun i _ -> i < 2, "p" ^ string_of_int (i + 1)) draft.literals) } }),
             notice
         | Error d -> prompt, Some (Refusal, d.Diagnostic.message))
    | None -> prompt, notice in
  let prompt = match List.find_map (function Text_pane.References_of name -> Some name | _ -> None) result.text_intents with
    | Some name -> Some (Referencing (name, "")) | None -> prompt in
  prompt, notice, loaded

let carry_report_of ~(value : t) ~carrying ~text ~text_shown (result : _ frame_result) =
  (* A payload in flight: what the panes saw under the pointer becomes the carry's report, and
     nothing else edits the document until the put (navigation and selection only). *)
  let carry_report = if not carrying then None else begin
    let place_of_path = function [ graph ] -> Carry.Graph graph | path -> Carry.Node path in
    let scope = List.filter_map (function
      | Pxui_graph.Scope.Drop_over { path; _ } -> Some (place_of_path path, false)
      | Dropped { path; _ } -> Some (place_of_path path, true)
      | _ -> None) result.scope_changes in
    let text_drops (text : Text_pane.state) (shown : Text_pane.shown) intents = List.filter_map (function
      | Text_pane.Carry_over (byte, dropped) ->
          let drafted = match text.tab with
            | Text_pane.Document -> text.draft <> None
            | Graph -> Text_pane.graph_slot text shown.graph <> None
            | Selection -> (match text.binding_draft with Some (p, _) -> p = shown.key | None -> false) in
          let target = if drafted then Carry.Drafted else match text.tab with
            | Document -> Carry.Whole | Graph -> Graph_text shown.graph | Selection -> Selection_text shown.key in
          Some (Carry.Text { target; text = shown.text; byte }, dropped)
      | _ -> None) intents in
    let text_drops = Option.fold ~none:[] ~some:(fun shown -> text_drops text shown result.text_intents) text_shown
      @ List.concat_map (fun (key, shown, intents) -> text_drops (local_of { value with locals = result.locals } key).code shown intents)
          result.other_texts in
    match scope @ text_drops @ result.drops with
    | (over, dropped) :: _ -> Some { over; dropped }
    | [] -> None
  end in
  carry_report

let pick_up ~(value : t) ~(value' : t) ~actions ~(frame : Frame.t) ~carry_report =
  (* [y]: the payload is held by the handle (no pointer capture); a press or Enter puts it *)
  let carry, pick_error = match value.carry with
    | Some c -> Some { c with report = carry_report }, None
    | None when List.mem Editor_commands.Pick_up actions ->
        (match carry_payload value' with
         | Ok payload ->
             Pxui.Ui.carry value'.ui ~kind:payload.kind ~value:payload.value ();
             Some { payload; via = `Keys; original = value'.doc; settled = value'.cook.Cook.pieces;
                    report = None; targets = None;
                    chosen = None; anchor = Some frame.input.pointer; preview = None; resting = None; hint = None }, None
         | Error message -> None, Some message)
    | None -> None, None in
  carry, pick_error

(* what the camera is asked to frame: the object a [F] in a graph names, or every cooked object *)
let framing_step ~(value : t) ~value' ~(doc : Document.t) ~(cooked : Cook.update) ~frame_request ~actions
    ~(frame : Frame.t) (result : _ frame_result) =
  (* Framing: local bounds move into the world with their object; at the
     scene level [F] frames every cooked object. *)
  let world_bounds id bounds = Cook.transformed (Objects.world doc.scene id) bounds in
  (* a framing cook may finish frames after its request: the object is remembered until it does *)
  let framing = match frame_request with Some (id, _) -> Some id | None -> value.framing in
  let framed = match cooked.framed, framing with
    | Some (Some bounds), Some id -> Some (Some (world_bounds id bounds))
    | framed, _ when List.mem Editor_commands.Frame_camera actions && (pane_level value') = Document.Scene ->
        ignore framed;
        let view = match active_view value' (geometry value' result.workspace frame) with
          | Some { panel = View key; _ } when Option.fold ~none:false
              ~some:(fun shell -> List.mem_assoc key shell.Document.views) doc.shell -> `Only key
          | _ -> `Primary in
        Some (List.fold_left (fun union (piece : Cook.piece) ->
          if not (view_wants value' view piece.id) then union else
          match piece.bounds, union with
          | None, union -> union
          | Some bounds, None -> Some (world_bounds piece.id bounds)
          | Some bounds, Some box -> Some (Cook.union box (world_bounds piece.id bounds))) None cooked.cook.pieces)
    | framed, _ -> framed in
  framing, framed

let projections_of ~outlined ~actions (result : _ frame_result) (value' : t) =
  (* the graph panel's own projection: list and text views are panels of their own now (/ l
     retypes), so opening a graph is the only thing that changes it *)
  let projections = if result.open_graph <> None || outlined <> None then
      Level_map.add (pane_level value') Graph_view (pane_views value')
    else match result.view_pick with
    | Some view when view <> Text_view || graph_name value' <> None -> Level_map.add (pane_level value') view (pane_views value')
    | Some _ -> (pane_views value')
    | None -> match List.find_map (function
        | Editor_commands.Panel_retype (Pxui_shell.Layout.Graph | List | Lisp as p) when value'.focus = Graph -> Some p
        | _ -> None) actions with
      | Some Pxui_shell.Layout.Graph -> Level_map.add (pane_level value') Graph_view (pane_views value')
      | Some List -> Level_map.add (pane_level value') List_view (pane_views value')
      | Some _ when graph_name value' <> None -> Level_map.add (pane_level value') Text_view (pane_views value')
      | _ -> (pane_views value') in
  projections

(* Command-Z / Shift-Command-Z: the history step, its notice and the document it leaves *)
let undo_step ~(value : t) ~carrying ~actions ~notice history =
  let stepped = if List.mem Editor_commands.Redo actions then Editor_core.History.redo history
    else if List.mem Editor_commands.Undo actions then Editor_core.History.undo history else None in
  let notice = match stepped with
    | None when List.mem Editor_commands.Redo actions -> Some (Refusal, "Redo: nothing to redo")
    | None when List.mem Editor_commands.Undo actions -> Some (Refusal, "Undo: nothing to undo")
    | None -> notice
    | Some _ when List.mem Editor_commands.Redo actions ->
        Option.map (fun label -> Info, "Redo " ^ label) (Editor_core.History.redo_label history)
    | Some _ -> Some (Info, "Undo " ^ Editor_core.History.label history) in
  let history, doc, undone = match stepped with
    | Some history -> history, Editor_core.History.present history, true
    | None -> history, (if carrying then value.doc else Editor_core.History.present history), false in
  history, doc, undone, notice

(* the pointer and keys of the sketch go to the pane in focus, else the first viewer *)
let view_input ~(value : t) (result : _ frame_result) =
  let is_view = function Pxui_shell.Layout.View _ -> true | _ -> false in
  let owner = match List.find_opt (fun (_, (panel, _)) -> panel = result.focus && is_view panel)
      result.pane_keys with
    | Some (key, _) -> Some key
    | None -> List.find_map (fun (key, (panel, _)) -> if is_view panel then Some key else None)
        result.pane_keys in
  let input = Pxui.Ui.input ?owner value.ui in
  input

let timeline_step ~(value : t) ~timeline ~timeline_changes (result : _ frame_result) =
  let timeline, timeline_changes = List.fold_left (fun (timeline, changes) intent ->
    let next, emitted = match intent with
      | Pause_toggle -> Timeline.toggle_pause timeline
      | Stop_playback -> Timeline.stop timeline
      | Reset_playback -> Timeline.reset timeline
      | Seek_playback frame -> Timeline.seek timeline ~frame
      | Set_end _ -> timeline, [] in
    next, changes @ emitted) (timeline, timeline_changes) result.timeline_intents in
  (* the End field of a tall timeline: the scrub range (view state, like the playhead) *)
  let value = { value with timeline_frames = List.fold_left (fun frames -> function
    | Set_end last -> max 1 last | _ -> frames) value.timeline_frames result.timeline_intents } in
  timeline, timeline_changes, value

(* what the frame around the panes hands the reducer *)
module Frame_input = struct
  type 'panel t = {
  carry_changed : bool; all_ui_visible : bool; view_state : 'panel option -> Flow.Syntax.t;
  carrying : bool; held_keys : Input.key list; leader : Editor_commands.state; frame : Frame.t;
  hud : (string * float) option; actions : Editor_commands.action list; guide : bool; copied : (notice_kind * string) option;
  steady : int; status_fps : int option; status_fps_at : float; timeline : Timeline.t;
  timeline_changes : Timeline.change list; graph_shown : bool; text : Text_pane.state;
  text_shown : Text_pane.shown option;
  row_sets : (row_source * (int option * int option) * (Pxui_shell.Tree.row array * string list)) list;
  rows : Pxui_shell.Tree.row array }
end

let reduce ?gpu ?(pump=fun()->()) (input : _ Frame_input.t) value (result : _ frame_result) =
  let Frame_input.{ carry_changed; all_ui_visible; view_state; carrying; held_keys; leader; frame; hud; actions; guide;
        copied; steady; status_fps; status_fps_at; timeline; timeline_changes; graph_shown; text; text_shown;
        row_sets; rows } = input in
  Flow.Phase_timer.measure "reduce" (fun () ->
  let result = match result.prompt_intent with
    | Some (Edit_source op) -> { result with changes = result.changes @ [ Syntax_edit op ] }
    | _ -> result in
  (* i on a collapsed function or loop opens it in place, as a double click does *)
  let result, actions = match Pxui_graph.Scope.selected (pane_view value) with
    | [ zone ] when List.mem Editor_commands.Enter actions && Pxui_graph.Scope.collapsed_zone (pane_view value) zone ->
        { result with scope_changes = result.scope_changes @ [ Pxui_graph.Scope.Zone_collapsed { zone; collapsed = false } ] },
        List.filter (( <> ) Editor_commands.Enter) actions
    | _ -> result, actions in
  let carry_report = carry_report_of ~value ~carrying ~text ~text_shown result in
  let result = if not carrying then result else
    { result with changes = []; handle_changes = None; settings_changes = []; text_intents = [];
      other_texts = []; menu_pick = None;
      tree_intents = List.filter (function Pxui_shell.Tree.Select _ -> true | _ -> false) result.tree_intents;
      scope_changes = List.filter (function
        | Pxui_graph.Scope.Selected _ | Activated _ | Drop_over _ | Dropped _ | Notice _ -> true
        | _ -> false) result.scope_changes;
      prompt_intent = (match result.prompt_intent with Some (Go _ as intent) -> Some intent | _ -> None) } in
  let guide_error = if guide = value.guide then None else
    match save_guide value.preferences guide with
    | Ok () -> None | Error message -> Some (Refusal, "Guide preference not saved: " ^ message) in
  (* The one reduction phase: panes have finished constructing their boxes. *)
  (* the fields of an object the text declares are written to the text (below, with the frame's
     gestures); only an object the host made is edited as a derived node, for reconcile to adopt *)
  let in_text node = Editor_document.Scene_sync.in_text value.doc (pane_level value) node in
  let document, edit_error, editor_effects = List.fold_left apply_change
      (objects value, value.edit_error, Param.no_effects)
      (List.filter (function Set_parameter { node; _ } | Rename { node; _ } -> not (in_text node) | _ -> true)
         result.changes) in
  (* what a list or the inspector's name row asks of an object the text has is written to the text
     (below); the derived network is edited only for the others *)
  let tree_edits = List.filter_map (tree_edit value rows) result.tree_intents
    @ List.filter_map (function
        | Rename { node; label } when in_text node -> Some (Editor_document.Scene_sync.Rename (node, label))
        | _ -> None) result.changes in
  let document, selection, tree, opened, tree_label, _ = List.fold_left
      (apply_tree value) (document, (result.selection : Selection.t), result.tree, result.opened, None, rows)
      result.tree_intents in
  let apply_changes (document, effects, error) = function
    | None -> document, effects, error
    | Some (node_id, values) ->
        (match Kind_graph.set_fields document node_id values with
         | Ok (document, changed) -> document, Param.union_effects effects changed, error
         | Error message -> document, effects, Some message) in
  let document, parameter_effects, edit_error =
    apply_changes (document, Param.no_effects, edit_error)
      (match result.handle_changes with Some (id, _) when in_text id -> None | handle -> handle) in
  let settings, settings_effects, edit_error = match result.settings_changes with
    | [] -> value.doc.settings, Param.no_effects, edit_error
    | changes -> (match Settings.apply value.doc.settings changes with
      | Ok (settings, effects) -> settings, effects, edit_error
      | Error message -> value.doc.settings, Param.no_effects, Some message) in
  let node_effects effects = match pane_level value with
    | Document.Inside id when kind value id = Some "geometry" -> effects
    | Inside _ | Scene -> { effects with Param.cook = false } in
  let label = match List.find_map intent_label result.changes, tree_label with
    | Some label, _ | None, Some label -> label
    | None, None when settings != value.doc.settings -> "Settings"
    | None, None -> match result.handle_changes with
      | Some (id, _) ->
          "Edit " ^ Option.fold ~none:(string_of_int id) ~some:(fun (node : Kind_graph.node) -> node.label)
            (Kind_graph.find document id)
      | None -> "Edit" in
  let result = { result with document; selection; tree; opened; edit_error;
    effects = Param.union_effects (node_effects
      (Param.union_effects editor_effects parameter_effects)) settings_effects;
    settings; label } in
  let timeline, timeline_changes, value = timeline_step ~value ~timeline ~timeline_changes result in
  let prompt, notice, loaded = prompt_step ~value ~view_state result in
  (* Shared undo stack: every document change (a parameter, a workspace gesture, an
     inspector commit) becomes one history entry; Command/Ctrl-Z undoes,
     Shift-Command/Ctrl-Z or Ctrl-Y redoes. *)
  let present = value.doc in
  let next = match loaded with
    | Some (preset : Preset.loaded) -> preset.doc
    | None ->
        (* the host's objects or layers as this frame's derived edits leave them *)
        if result.document == objects value || network value <> None then present
        else
          let doc = Document.with_kinds present (pane_level value) result.document in
          if (pane_level value) = Document.Scene then Document.prune doc else doc in
  (* Enter and row activation share camera selection and look-through. *)
  let entered_camera = if (pane_level value) <> Document.Scene || Option.is_some loaded || carrying then None
    else
      let candidate = match result.opened with
        | Some _ as id -> id
        | None when List.mem Editor_commands.Enter actions -> Selection.selected result.selection
        | None -> None in
      match candidate with
      | Some id when kind value id = Some "camera" && view_wants value `Primary id -> Some id
      | _ -> None in
  let tree_edits = if result.settings == value.doc.settings then tree_edits
    else tree_edits @ [ Editor_document.Scene_sync.Settings result.settings ] in
  let tree_edits = match entered_camera with
    | Some id when next.active_camera <> Some id -> tree_edits @ [ Editor_document.Scene_sync.Camera (Some id) ]
    | _ -> tree_edits in
  let next, result, added, created, renamed, edit_note =
    write_gestures ~value ~loaded ~present ~tree_edits ~in_text ~node_effects next result in
  (* The workspace pane's layout gestures: moving an item and collapsing a zone
     edit the layout keys (one history entry each); the probe is view state and
     never reaches history. *)
  (* a node added from the menu lands where the menu was opened *)
  let scope_changes = match added, result.menu_pick, value.menu with
    | [ path ], Some _, Some menu ->
        let scope = List.take (max 0 (List.length path - 1)) path in
        let mx, my = Pxui_graph.Node_menu.position menu in
        (match Pxui_graph.Scope.scope_point (pane_view value) ~scope (float mx, float my) with
         | Some (x, y) -> Pxui_graph.Scope.Moved [ path, x, y ] :: result.scope_changes
         | None -> result.scope_changes)
    | _ -> result.scope_changes in
  let next, result, probes = layout_gestures ~value ~loaded ~added next result scope_changes in
  let before_world = next in
  (* a World key is written to the text (below); only a World the host made is edited here *)
  let next, world_label, world_edits = if in_world value
    then world_keys value next result.selection actions else next, None, [] in
  (* / e opens the World, writing one (a [scene/world] member and its world graph) on first use. *)
  let next, world_added = match Objects.ids "world" next.scene with
    | [] when List.mem Editor_commands.Go_world actions ->
        (match Result.bind (Editor_document.Scene_sync.add_world next)
                 (Editor_document.Document_edit.syntax_batch ~factories:value.factories next) with
         | Ok doc -> doc, true | Error _ -> next, false)
    | _ -> next, false in
  let next, result = if Option.is_some loaded then next, result
    else reconciled ~value ~before:before_world next result in
  let next, result = if Option.is_some loaded then next, result else
    List.fold_left (fun ((next : Document.t), (result : _ frame_result)) (level, edit) ->
      if result.edit_error <> None then next, result else
      match Editor_document.Scene_sync.write ~factories:value.factories next level edit with
      | Ok (Some (doc, _)) -> doc, result
      | Ok None -> next, result
      | Error message -> next, { result with edit_error = Some message }) (next, result) world_edits in
  let input = view_input ~value result in
  let gesture = gesture_key ~value ~frame ~loaded ~world_added ~world_label result in
  let history = if next == present then value.history
    else commit
        ~label:(if result.prompt_intent = Some Load_last_state && Option.is_some loaded then "Restore last edited state"
          else if Option.is_some loaded then "Load preset"
          else if world_added then "Add World"
          else if world_label <> None then Option.get world_label
          else result.label)
        ~merge:(if List.exists (function Layout_edit (Layout_edit.Set_layout _) -> true | _ -> false) result.changes
          then Editor_core.History.Burst { key = "layout"; at = frame.input.t; window = 1.5 }  (* keys: no drag seals it *)
          else Option.fold ~none:Editor_core.History.Step
            ~some:(fun key -> Editor_core.History.Gesture key) gesture) next value.history in
  let history, doc, undone, notice = undo_step ~value ~carrying ~actions ~notice history in
  let effects = if undone || Option.is_some loaded || carry_changed
    then Param.union_effects result.effects Editor_document.Document_edit.cook_effects else result.effects in
  (* The open level must still exist after undo or a preset load. *)
  let level = Document.resolve_level doc
      (if Option.is_some loaded then Document.Scene else (pane_level value)) in
  let value' = with_pane { value with doc; lisp_vocab = vocabulary_for value doc;
    workspace = unrestore value.doc doc result.workspace } (fun pane -> { pane with at = level }) in
  (* the selection keeps the nodes that still exist in the open network *)
  let selection =
    if level <> (pane_level value) || Option.is_some loaded then Selection.empty
    else
      let exists = lists value' in
      match List.filter exists (Selection.selected_nodes result.selection) with
      | ids when ids = Selection.selected_nodes result.selection -> result.selection
      | ids -> Selection.select_nodes ids in
  let followed, target = level_target ~value ~value' ~level ~doc ~selection ~actions result in
  let selection = match result.open_graph with
    | Some id when lists value' id ->
        Selection.select id
    | Some _ | None -> selection in
  (* / e selects the scene's World, then enters it *)
  let selection = match target with
    | Some (Document.Inside id) when List.mem Editor_commands.Go_world actions && level = Document.Scene ->
        Selection.select id
    | _ -> selection in
  let value' = with_pane { value' with menu = result.menu; tree = result.tree; probes;
    open_import = (match List.find_map (function Pxui_graph.Scope.Open_import file -> Some file | _ -> None) scope_changes with
      | Some _ as file -> file | None -> value.open_import) } (fun pane -> { pane with picked = selection;
    (* a node added from the menu is the selection *)
    view = (match added with
      | [] ->
          (* a row chosen in the list of a geometry object selects the node that made it in the
             graph pane, whose inspector edits the text *)
          (match List.find_map (function Pxui_shell.Tree.Select [ id ] -> Some id | _ -> None)
                   result.tree_intents with
           | Some id when (match pane_level value with
               | Document.Inside oid -> kind value oid = Some "geometry" | Scene -> false) ->
               (match node_path value id with
                | Some path -> Pxui_graph.Scope.select [ path ] result.scope_view
                | None -> result.scope_view)
           | _ -> result.scope_view)
      | paths -> Pxui_graph.Scope.select paths result.scope_view) }) in
  let value' = match target with
    | Some level when level <> (pane_level value') -> open_level (remember value') level
    | Some level -> open_level value' level
    | None -> value' in
  (* u goes back along the route taken *)
  let pops = List.fold_left (fun n -> function Editor_commands.Up -> max n 1 | Back k -> max n k | _ -> n) 0 actions in
  let value' = match List.drop (max 0 (pops - 1)) (pane_route value') with
    | (back_level, pane_graph) :: rest when pops > 0 && target = None && followed = None ->
        let back_level = Document.resolve_level doc back_level in
        with_pane (open_level (with_pane value' (fun pane -> { pane with route = rest })) back_level)
          (fun pane -> { pane with shown = pane_graph })
    | _ -> value' in
  let value' = match followed with Some graph -> go value' graph | None -> value' in
  (* In the World, / m flips the view pane to the lat-long map. *)
  let map_view = if List.mem Editor_commands.Toggle_map actions && in_world value'
    then not value'.map_view else value'.map_view && in_world value' in
  let value' = { value' with map_view } in
  (* a renamed graph keeps being the one shown, and the one [u] returns to *)
  let value' = match renamed with
    | Some (old, now) ->
        let name = Option.map (fun g -> if g = old then now else g) in
        with_pane value' (fun pane -> { pane with shown = name pane.shown;
          route = List.map (fun (level, g) -> level, name g) pane.route })
    | None -> value' in
  let value', outlined = outline_step ~created ~result value' in
  let value' = match result.prompt_intent with Some (Find_node path) -> select_source value' path | _ -> value' in
  let projections = projections_of ~outlined ~actions result value' in
  let selection_changed = Pxui_graph.Scope.selected result.scope_view <> Pxui_graph.Scope.selected (pane_view value) || outlined <> None in
  let text = if selection_changed then {text with target = None} else text in
  let locals = if selection_changed then List.map (fun (key, (local : local)) ->
      key, {local with code = {local.code with target = None}}) result.locals else result.locals in
  let value' = with_pane { value' with text;
    rows = row_sets;
    live_cook = result.live_cook } (fun pane -> { pane with views = projections }) in
  let value' = List.fold_left (fun value -> function
    | Follow_source path -> select_source value path
    | _ -> value) {value' with locals} result.changes in
  let value' = apply_text { value' with timeline; history } result.text_intents in
  (* each other text pane's intents fold into its own state *)
  let value' = List.fold_left (fun v (key, _, intents) -> apply_text_at v key intents)
    value' result.other_texts |> sync_scope in
  let value' = List.fold_left (fun v -> function
    | Pxui_graph.Scope.Display_set path -> view_node v path
    | _ -> v) value' scope_changes |> sync_scope in
  let doc = value'.doc in
  let frame_request = match result.frame_request, (pane_level value') with
    | Some node, Inside id when kind value' id = Some "geometry" -> Some (id, node)
    | _ -> None in
  if doc.workspace != value.doc.workspace || value'.viewed != value.viewed then begin
    let roots = Document.Int_map.fold (fun _ (_, root) roots ->
      Flow_sop.Network.Int_map.add root () roots) value'.viewed Flow_sop.Network.Int_map.empty in
    Cook.set_volatile value.cook (fun id -> Flow_sop.Lower.is_volatile (doc.workspace.lowered) id
      || Flow_sop.Network.Int_map.mem id roots)
  end;
  let probes = match pane_laid value' with Some k when scope_name value' <> None -> k.targets | _ -> [] in
  let lit, lit_cache = lit_tags value' in
  let cooked = Cook.update ~gpu:(Option.value gpu ~default:value.gpu) ~input:value'.live_frame ~live:result.live_cook ~probes ~lit ~pump
      value.cook ~settings:doc.settings
      ~objects:(geometry_objects value')
      ~edit_error:result.edit_error ~effects
      ~timeline_changes ~timeline ~frame ~frame_request in
  let framing, framed = framing_step ~value ~value' ~doc ~cooked ~frame_request ~actions ~frame result in
  let document_changed = doc != value.doc in
  let carry, pick_error = pick_up ~value ~value' ~actions ~frame ~carry_report in
  (* a cook that gave back every piece it had (the text changed, the objects did not) leaves
     nothing to recompose, and no accumulating render to restart *)
  let prepared_changed = cooked.prepared_changed
    && not (List.equal ( == ) (Cook.pieces cooked.cook) (Cook.pieces value.cook)) in
  let new_notice = if guide_error <> None then guide_error
    else if pick_error <> None then Option.map (fun m -> Refusal, m) pick_error
    else if copied <> None then copied
    else if document_changed && Option.is_none loaded && not undone then Option.map (fun m -> Info, m) edit_note
    else notice in
  { core = { value' with timeline; cook = cooked.cook; lit = lit_cache; edit_error = cooked.edit_error;
      status_fps; status_fps_at; last_dt = frame.input.dt; steady; guide; hud; focus = result.focus; focus_path = result.focus_path;
      pane_keys = result.pane_keys; panes = Panel_map.with_others result.graph_panes value'.panes; leader; held_keys;
      prompt;
      queued = (match result.prompt_intent with Some (Run_action action) -> [action]
        | _ when all_ui_visible && List.mem Editor_commands.Add_node actions && not graph_shown
            && has_panel value' Pxui_shell.Layout.Graph -> [Editor_commands.Add_node]
        | _ -> []);
      carry; framing = (if cooked.framed <> None then None else framing);
      notice = new_notice;
      notice_at = if new_notice <> value'.notice then frame.input.t else value'.notice_at };
    effects; prepared_changed;
    scene_changed = carry_changed || doc.scene != value.doc.scene || prepared_changed;
    framed;
    loaded_view = Option.map (fun (preset : Preset.loaded) -> preset.view) loaded;
    actions = (if Option.is_some entered_camera && entered_camera = doc.active_camera
      then actions @ [Editor_commands.Look_through_camera] else actions); panel = result.panel;
    input })

(* One frame of the editor.  A carry takes its turn first (its keys, the ends of the gesture, the
   preview document the panels then read) and the frame the rest of the editor sees has the keys
   it took removed. *)

(* One reduction with no panes built, for tests of the reducer: the keys are [actions], the list's
   selection is [select], and [preview] puts a payload in flight whose hot target shows that edit
   on a scratch document (as {!carry_step} leaves the model while a target is hot). *)
let reduce_idle ?select ?preview value actions (frame : Frame.t) =
  let value = {value with live_frame = Flow_native.Live_frame.of_frame frame} in
  let value = match preview with
    | None -> value
    | Some op ->
        (match Editor_document.Document_edit.syntax_edit ~factories:value.factories value.doc op with
         | Error message -> invalid_arg message
         | Ok doc ->
             { value with doc; lisp_vocab = vocabulary_for value doc;
               carry = Some { payload = { Carry.kind = "sop"; value = "" }; via = `Keys;
                 original = value.doc; settled = value.cook.Cook.pieces; report = None; targets = None;
                 chosen = None; anchor = None; resting = None; hint = None;
                 preview = Some (Showing { place = Carry.Graph ""; doc; what = "" }) } }) in
  let selection = match select with
    | Some ids -> Selection.select_nodes ids | None -> (pane_selection value) in
  let result = { workspace = value.workspace; focus = value.focus; focus_path = value.focus_path;
    pane_keys = value.pane_keys; scope_view = (pane_view value); scope_changes = []; selection;
    menu = value.menu; menu_pick = None; tree = value.tree; outline = value.outline; outline_intents = [];
    document = objects value; edit_error = value.edit_error; effects = Param.no_effects;
    timeline_intents = []; frame_request = None; prompt = value.prompt; prompt_intent = None;
    panel = None; grab = false; settings = value.doc.settings; opened = None; live_cook = value.live_cook;
    label = "Edit"; changes = []; tree_intents = []; text_intents = []; open_graph = None;
    settings_changes = []; graph_panes = Panel_map.others value.panes; locals = value.locals; other_texts = [];
    handle_changes = None; bar_action = None; view_pick = None; drops = [] } in
  let update : unit update = reduce Frame_input.{ carry_changed = false; all_ui_visible = false;
      view_state = (fun _ -> Flow.Syntax.make (Flow.Syntax.Map [])); carrying = value.carry <> None;
      held_keys = value.held_keys; leader = value.leader; frame; hud = value.hud; actions; guide = value.guide;
      copied = None; steady = value.steady; status_fps = value.status_fps; status_fps_at = value.status_fps_at;
      timeline = value.timeline; timeline_changes = []; graph_shown = false; text = value.text;
      text_shown = None; row_sets = []; rows = [||] } value result in
  update.core
