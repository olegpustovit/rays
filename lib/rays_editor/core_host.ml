open Core_model
open Rays
open Sop
open Editor_document
open Core_text
open Core_reduce
open Core_inspect
open Core_list
open Core_panes

(* Environment-owned scene edits (camera bookkeeping, a camera following the viewport): view
   state, never an undo entry.  [`Reset] starts the history, [`Amend] folds into the present
   entry and keeps what redo would restore. *)
let scene_edit value mode ?(active_camera = value.doc.active_camera) scene =
  if value.carry <> None then value else
  let edited = { value.doc with scene } in
  (* a camera that follows the viewport is an edit of the text when the text declares it *)
  match Result.bind (Editor_document.Document_edit.reconcile ~factories:value.factories ~adopt:false value.doc edited) (fun doc ->
      if active_camera = doc.active_camera then Ok doc else
      Editor_document.Scene_sync.write ~factories:value.factories doc Document.Scene (Camera active_camera)
      |> Result.map (Option.fold ~none:doc ~some:fst)) with
  | Error message -> { value with edit_error = Some message }
  | Ok doc ->
  let history = match mode with
    | `Reset -> Editor_core.History.create doc
    | `Amend -> commit ~merge:Repair doc value.history in
  (* camera bookkeeping is not unsaved work: a document that was the file's still is *)
  { value with doc; history; filed = if same_text value.doc value.filed then doc else value.filed }

let machinery ?under value ~all_ui_visible =
  if all_ui_visible || value.leader <> Leader.Idle then Pxui.Ui.scene ?under value.ui
  else []

let close value =
  Pxui.Ui.destroy value.ui;
  Cook.close value.cook

(* A sketch-driven settings change: one undo step and a fresh cook, since
   [prepare] reads the settings. *)
let set_settings value settings =
  if settings == value.doc.settings || value.carry <> None then value else
  match Editor_document.Scene_sync.write ~factories:value.factories value.doc Document.Scene (Settings settings) with
  | Error message -> { value with edit_error = Some message }
  | Ok None -> value
  | Ok (Some (doc, _)) ->
      { value with doc; cook = Cook.force value.cook;
        history = commit ~label:"Settings" doc value.history }

(* An edit of the render settings (the root): written to the root's call, or the first edit writes
   a root over the scene's result; one undo step, merged while scrubbed. *)
let set_root value root =
  if root = value.doc.root || value.carry <> None then value else
  match Editor_document.Scene_sync.write ~factories:value.factories value.doc Document.Scene (Root root) with
  | Error message -> { value with edit_error = Some message }
  | Ok None -> value
  | Ok (Some (doc, _)) ->
      { value with doc;
        history = commit ~label:"Render settings" ~merge:(Gesture "render settings") doc value.history }

(* A host-driven gesture on a workspace document ([Editor.edit], W11's
   entry): the same reduction as a [Syntax_edit] intent, committed as one
   history entry named by the op ([Gesture] merge for a scrub). *)
let syntax_edit value op =
  let result, phases = Flow.Phase_timer.sample ~clock:Unix.gettimeofday (fun () ->
  Result.map (fun doc -> install value doc ~label:(Flow_graph.Flow_edit.label op)
    ~merge:(Option.fold ~none:Editor_core.History.Step
      ~some:(fun key -> Editor_core.History.Gesture key) (Flow_graph.Flow_edit.gesture op)))
    (Editor_document.Document_edit.syntax_edit_result ~factories:value.factories value.doc op)
  |> Result.map_error Diagnostic.to_string) in
  Result.map (fun value -> { value with edit_phases = phases }) result

(* The scene's World at timeline [time] (the day cycle advances with it). *)
let world ?(view = `Primary) value ~time =
  let own = match view with
    | `Only key -> List.assoc_opt key value.doc.Document.view_worlds
    | _ -> None in
  match own with
  | Some None -> None
  | Some (Some (w : Document.view_world)) -> Layers.to_world ~time w.node w.layers
  | None ->
  match List.filter (view_wants value `Primary) (Objects.ids "world" (scene value)) with
  | id :: _ ->
      Option.bind (Kind_graph.find (scene value) id) (fun node ->
        Option.bind (Document.layers value.doc id) (Layers.to_world ~time node))
  | [] -> None

(* Visible objects' world transforms with their latest cook.  Objects that only a viewport
   over another scene instance draws ([Document.shell]'s views) are left out unless asked
   for: [`All], or [`Only key] for that viewport. *)
(* Lights and cameras follow the same instance membership as the geometry. *)
let scene_for_view ?scene:override ?(view = `Primary) value =
  let scene = Option.value override ~default:(scene value) in
  let unwanted = List.filter_map (fun (node : Kind_graph.node) ->
    if view_wants value view node.id then None else Some node.id) scene in
  if unwanted = [] then scene else Kind_graph.remove unwanted scene

let placed_pieces ?(render = false) ?(view = `Primary) value =
  List.filter_map (fun (piece : _ Cook.piece) ->
    match Kind_graph.find (scene value) piece.id with
    | Some node when view_wants value view piece.id && Objects.visible node
        && (not render || Objects.flag "render" node) ->
        Some (Objects.world (scene value) piece.id, piece)
    | Some _ | None -> None) (pieces value)

let world_id value = match Objects.ids "world" (scene value) with
  | id :: _ -> Some id | [] -> None

(* A parameter edit made in the view (a map drag, a World rotation): one
   undo entry per pointer gesture. *)
let edit_node value level node_id values ~label =
  if value.carry <> None then value else
  let written doc =
    Cook.set_volatile value.cook (Flow_sop.Lower.is_volatile (doc.Document.workspace.lowered));
    { value with doc;
      history = commit ~label ~merge:(Gesture (parameter_gesture label level node_id values)) doc value.history } in
  (* an object the text declares takes the values in its text; one only the host made is edited
     as a derived node, which the reconciliation adopts *)
  match Editor_document.Scene_sync.set_fields ~factories:value.factories value.doc level ~node:node_id values with
  | Ok (Some (doc, _, _)) -> written doc
  | Error message -> { value with edit_error = Some message }
  | Ok None ->
  match Document.kinds value.doc level with
  | None -> value
  | Some graph ->
      match Kind_graph.set_fields graph node_id values with
      | Error _ -> value
      | Ok (graph, _) ->
          let edited = Document.with_kinds value.doc level graph in
          (match Editor_document.Document_edit.reconcile ~factories:value.factories value.doc edited with
           | Error message -> { value with edit_error = Some message }
           | Ok doc -> written doc)

(* A click in the view: the primitive under the ray, its
   [__flow_src] tag, the merge input that made it (`Lower.provenance`), then
   the selection is that node and every enclosing zone probes that iteration,
   so the highlight ({!lit_tags}), the selectors and the inspector agree.  A
   click on nothing deselects.  A merge of merges keeps the innermost tag, so
   the origin is exact through nested collecting merges.  Geometry drawn as
   instances is picked per instance ({!Cook.pick}), and hits compare by the
   ray parameter, so any scale is exact. *)
(* The graph of a [shop_materialpath]: the material graph of that name, else the one whose
   [:name] it is. *)
let material_graph value path =
  let ws = value.doc.Document.workspace.ws in
  let materials = List.filter_map (fun (g : Flow.Workspace.graph) ->
    if g.context = Flow.Context.material then Some g.name else None) ws.checked.graphs in
  if List.mem path materials then Some path
  else match value.scope_key with
    | Some { evaluated = Some ev; _ } ->
        List.find_map (fun (graph, v) -> match v with
          | Flow.Eval.Struct ("material/standard", _, fields)
            when List.mem graph materials && List.assoc_opt "name" fields = Some (Flow.Eval.Text path) -> Some graph
          | _ -> None) ev.results
    | _ -> None

let pick ?view ?(alt = false) value ~origin ~direction =
  (* the focused viewport's own scene instance, when it has objects of its own *)
  let view = match view, value.doc.Document.shell with
    | Some key, Some shell when List.mem_assoc key shell.views -> `Only key
    | _ -> `Primary in
  let nearest_by pick = List.fold_left (fun best (matrix, piece) ->
    match Rays_math.Mat4.inverse matrix with
    | None -> best
    | Some inverse ->
        match pick piece ~origin:(Rays_math.Mat4.transform_point inverse origin)
            ~direction:(Rays_math.Mat4.transform_direction inverse direction), best with
        | Some (distance, _), Some (nearer, _, _) when distance >= nearer -> best
        | Some (distance, hit), _ -> Some (distance, hit, piece.Cook.id)
        | None, _ -> best) None (placed_pieces ~view value) in
  match value.doc.Document.workspace, value.scope_key with
  | _ when alt ->
      (* Alt-click: the material on that primitive *)
      (match nearest_by Cook.pick_material with
       | Some (_, Some path, _) ->
           (match material_graph value path with
            | Some graph -> go value graph
            | None -> { value with notice = Some (Refusal, "No material graph for " ^ path) })
       | _ -> value)
  | { Document.ws; lowered; _ }, Some { scope; _ } when scope_name value <> None ->
      let nearest = nearest_by Cook.pick in
      let hit = Option.bind nearest (fun (_, tag, owner) ->
        let original = Flow_sop.Lower.origin lowered tag in
        match Document.Int_map.find_opt owner value.previews with
        | Some preview when original = None || List.exists (fun path ->
            List.exists (fun (zone : Flow_sop.Lower.zone) -> zone.site = path) lowered.zones) preview.chain ->
            let root = snd (Document.Int_map.find owner value.viewed) in
            Some Flow_sop.Lower.{merge = root; input = 0; source = root; site = preview.path;
              iter = List.map (fun path -> Option.value ~default:0
                (Layout_by_path.Path_map.find_opt path value.probes)) preview.chain}
        | _ -> original) in
      (match hit with
       | Some o ->
           (* geometry of another graph (a viewport over another scene instance draws its own
              objects): the pane shows that graph, the node is selected there once it does *)
           let root = List.hd o.site in
           let value, scope = if root = (match value.scope_key with Some k -> k.graph | None -> root)
             then value, scope
             else match Lazy.force value.flow_catalog with
               | Some catalog -> { value with pane_graph = Some root },
                   Flow_graph.Projection.of_graph catalog ws.checked root
               | None -> value, scope in
           (match Hashtbl.find_opt (Flow_graph.Probe.chains scope) o.site with
            | Some chain ->
                (* the outermost collapsed loop stands for what is inside it *)
                let collapsed zone = Layout_by_path.Path_map.find_opt zone ws.layout.collapsed = Some true in
                let shown = match List.find_opt collapsed chain with Some zone -> zone | None -> o.site in
                let probes = List.fold_left2 (fun probes zone index ->
                  Layout_by_path.Path_map.add zone index probes) value.probes chain
                  (if List.compare_lengths chain o.iter = 0 then o.iter
                   else List.map (fun _ -> 0) chain) in
                let in_pane = root = (match value.scope_key with Some k -> k.graph | None -> root) in
                { value with probes;
                  select_later = (if in_pane then [] else [ shown ]);
                  scope_view = if in_pane then Pxui_graph.Scope.select [ shown ] value.scope_view
                    else value.scope_view }
            | None -> value)
       | None -> { value with scope_view = Pxui_graph.Scope.clear_selection value.scope_view })
  | _ -> value

(* A changed source file: the whole text replaces the document as one
   history entry; layout, settings, probes and the selection (all keyed by path)
   stay.  A refused text changes nothing but the pane: it shows the file's text
   with the diagnostics, and the status says the last good document is kept. *)
let reload ?imports value ~name text =
  Result.map (fun value ->
    Cook.reset_state value.cook;
    let clean (text : Text_pane.state) =
      { text with draft = None; doc_base = None; binding_base = None; doc_errors = Text_pane.no_errors; binding_draft = None;
        binding_errors = Text_pane.no_errors; graphs = [] } in
    { value with notice = Some (Info, "Reloaded " ^ name); text = clean value.text; filed = value.doc;
      locals = List.map (fun (key, l) -> key, { l with code = clean l.code }) value.locals })
    (text_edit ?imports ~label:("Reload " ^ name) value text)

let reload_failed value ~name text diagnostics =
  let first = match diagnostics with
    | [] -> ""
    | d :: _ -> (match Text_pane.line_of text d with
        | Some line -> Printf.sprintf ": line %d, %s" line d.Diagnostic.message
        | None -> ": " ^ d.Diagnostic.message) in
  { value with notice = Some (Refusal, Printf.sprintf "%s not reloaded%s" name first);
    text = { value.text with tab = Text_pane.Document; draft = Some text; doc_base = Some (value.doc.workspace.ws); doc_errors = { at = text; found = diagnostics } } }

(* The carry's hovered viewport has been looked at by the host: the surface under the pointer
   (the object it belongs to and the material graph its primitive reads) is where a material goes,
   and the scene where a SOP graph does.  [view] is the viewport's own scene instance. *)
let carry_over_surface ?view value ~origin ~direction =
  match value.carry with
  | Some ({ report = Some { over = Carry.Viewport _; dropped }; payload = { kind = ("material" | "sop"); _ }; _ } as c) ->
      let view = match view, value.doc.Document.shell with
        | Some key, Some shell when List.mem_assoc key shell.views -> `Only key
        | _ -> `Primary in
      let nearest = List.fold_left (fun best (matrix, (piece : _ Cook.piece)) ->
        match Rays_math.Mat4.inverse matrix with
        | None -> best
        | Some inverse ->
            match Cook.pick_material piece ~origin:(Rays_math.Mat4.transform_point inverse origin)
                ~direction:(Rays_math.Mat4.transform_direction inverse direction), best with
            | Some (distance, _), Some (nearer, _, _) when distance >= nearer -> best
            | Some (distance, path), _ -> Some (distance, piece.id, path)
            | None, _ -> best) None (placed_pieces ~view { value with cook = { value.cook with Cook.pieces = c.settled } }) in
      let place = match nearest with
        | Some (_, object_, path) ->
            Carry.Surface { object_; material = Option.bind path (material_graph value) }
        | None ->
            (* nothing under the pointer: a SOP graph goes to the scene, a material has no place *)
            (match List.find_map (fun (g : Flow.Workspace.graph) ->
               if g.context = Flow.Context.scene then Some g.name else None)
               (value.doc.Document.workspace.ws).checked.graphs with
             | Some graph when c.payload.kind = "sop" -> Carry.Graph graph
             | _ -> Carry.Viewport "") in
      { value with carry = Some { c with report = Some { over = place; dropped } } }
  | _ -> value

(* A pointer carry is over a viewport: the host should look through the viewport's camera. *)
let carry_viewport value = match value.carry with
  | Some { report = Some { over = Carry.Viewport key; _ }; payload = { kind = ("material" | "sop"); _ }; _ } -> Some key
  | _ -> None

let carrying value = value.carry <> None
