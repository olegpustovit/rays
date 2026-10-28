open Core_model
open Rays
open Sop
open Editor_document
open Core_host
open Core_inspect
open Core_panes
open Core_shell
open Core_status

let inspect_panel ~image ~camera_panel ~inspector_visible ~unchanged ~resized
    ~geometry:(g : Pxui_shell.Layout.geometry) ~ui
    (value, scope_view, selection, scope_active) inspector_root bounds =
  let ws = value.doc.Document.workspace.ws in
  let document = objects value in
  let selected_ids = Selection.selected_nodes selection in
  let lowered_level = match value.level with
    | Document.Inside id -> kind value id = Some "geometry" | Scene -> false in
  (* a row of a geometry object's list selects its node in the pane: the inspector follows it *)
  let scope_selected = if (match value.scope_key with Some k -> Some k.graph = graph_name value | None -> false)
      || (lowered_level && value.scope_key <> None)
    then inspector_paths (Pxui_graph.Scope.selected scope_view) else [] in
  let window = List.exists (fun (l : Pxui_shell.Layout.leaf) -> l.floating && l.panel = Inspector && l.body = bounds) g.leaves in
  (* a window keeps its 1-point border on the left, right and bottom: its panel lies inside *)
  let bounds = if window then (let x, y, w, h = bounds in x, y, max 1 w, max 1 (h - 1)) else bounds in
  let inspector_panel ui bounds build = inspector_panel ~window ui bounds build in
  on_sheet ui window
  @@ fun () -> Pxui.Ui.within ui inspector_root (fun () -> match selected_ids with
  | _ when not inspector_visible ->
      None, [], [], value.live_cook, [], []
  | _ when scope_selected <> [] ->
      (match scope_selected with
        | [ path ] ->
            (* the inspector reports each choice control it builds: the material one is a place
               a carried payload can be put, gathered here and returned with the rows' requests *)
            let drops = ref [] in
            let requests, moves = inspector_panel ui bounds (fun () ->
              workspace_inspector ~image ~window value ui ~width:(float (let _, _, w, _ = bounds in max 1 w)) path ~resized
                ~follows:(fun path -> follow_target ~path value)
                ~on_choice:(fun name box -> if name = "@ref:material" then
                  Option.iter (fun d -> drops := (Carry.Node path, (match d with
                    | Pxui.Ui.Dropped _ -> true | Hover _ -> false)) :: !drops)
                    (Pxui.Ui.drop_target ui box))) in
            None, requests, [], value.live_cook, moves, List.rev !drops
        | paths -> inspector_panel ui bounds (fun () ->
            ignore (Pxui.Ui.inspector_header ui ~key:"multi-header"
              ~title:(Printf.sprintf "%d nodes" (List.length paths)) ~detail:"Selected" ()));
            None, [], [], value.live_cook, [], [])
  | [] ->
      (* Sketch settings above the environment's camera/render panel. *)
      let changes, live, panel = inspector_panel ui bounds (fun () ->
        let node_count, label = match network value with
          | Some network -> List.length (Edit_graph.inspect network.graph.geometry),
              (fun id -> Option.map Node.label (Edit_graph.find network.graph.geometry ~node_id:id))
          | None -> List.length document,
              (fun id -> Option.map (fun (node : Kind_graph.node) -> node.label) (Kind_graph.find document id)) in
        let display = Option.fold ~none:"none" ~some:(fun id ->
          Option.value ~default:("#" ^ string_of_int id) (label id)) (displayed value) in
        let title, detail = match value.scope_key with
          | Some k when scope_active && Some k.graph = graph_name value ->
              let rec count (s : Flow_graph.Projection.scope) = List.fold_left
                (fun n (x : Flow_graph.Projection.node) ->
                  n + 1 + Option.fold ~none:0 ~some:(fun (z : Flow_graph.Projection.zone) -> count z.scope) x.zone)
                0 s.nodes in
              let zones = List.length (Flow_graph.Projection.zones k.scope) in
              k.graph, Printf.sprintf "%d node%s · %d loop%s · select a node to edit it" (count k.scope)
                (if count k.scope = 1 then "" else "s") zones (if zones = 1 then "" else "s")
          | _ -> level_name value, Printf.sprintf "%d node%s · display %s" node_count (if node_count = 1 then "" else "s") display in
        ignore (Pxui.Ui.inspector_header ui ~key:"network-header" ~title ~detail ());
        Pxui.Ui.inspector_body ui (fun () ->
        (* the row sits under a section named for the graph, as every row of the sheet does *)
        let live = Option.value ~default:value.live_cook (Pxui.Ui.inspector_section ui
            ~key:"live-cook-section" ~expanded:true title (fun () ->
              Pxui.Ui.inspector_toggle ui ~key:"live-cook" ~label:"Live update" value.live_cook)) in
        let changes = match Settings.fields unchanged |> List.filter (fun (field : Param.field_view) ->
            field.name <> "renderer") with
          | [] -> []
          | fields ->
              Pxui.Ui.scope ui "sketch-settings" (fun () ->
                let _, _, width, _ = bounds in
                Option.value ~default:[] (Pxui.Ui.inspector_section ui
                  ~key:"sketch-settings-section" ~expanded:true "Settings"
                  (fun () -> Pxui_shell.Inspector.fields ui
                    ~width:(float width) fields))) in
        changes, live, camera_panel ())) in
      Some panel, [], changes, live, [], []
  | _ :: _ :: _ ->
      let () = inspector_panel ui bounds (fun () ->
        ignore (Pxui.Ui.inspector_header ui ~key:"multi-header"
          ~title:(Printf.sprintf "%d nodes" (List.length selected_ids))
          ~detail:"Selected" ())) in
      None, [], [], value.live_cook, [], []
  | [node_id] when lowered_level ->
      (* the nodes of a geometry object are the lowering of its graph: the graph pane's node is the
         one to edit (its arguments are the text), and its inspector is the graph pane's *)
      let drops = ref [] in
      let requests, moves = inspector_panel ui bounds (fun () ->
        match node_path value node_id with
        | Some path ->
            workspace_inspector ~image ~window value ui ~width:(float (let _, _, w, _ = bounds in max 1 w)) path ~resized
              ~follows:(fun path -> follow_target ~path value)
        | None ->
            Pxui.Ui.inspector_message ui ~key:"flow-derived"
              "Made from the graph's text. Select the node in the graph pane to edit its arguments.";
            [], []) in
      None, requests, [], value.live_cook, moves, List.rev !drops
  | [node_id] ->
      let fields, label, kind = match Kind_graph.find document node_id with
        | Some node -> Kind_graph.fields node, node.label, node.kind
        | None -> [], "#" ^ string_of_int node_id, "unknown" in
      let preview = value.level = Document.Scene && Option.fold ~none:false
        ~some:(fun shell -> List.exists (fun (_, ids) -> List.mem node_id ids) shell.Document.views)
        value.doc.shell in
      let changes = inspector_panel ui bounds (fun () ->
        Pxui.Ui.scope ui (Printf.sprintf "node.%d" node_id) (fun () ->
          let _, _, inspector_width, _ = bounds in
          let inspector_width = float (max 1 inspector_width) in
          let input = Option.bind (Kind_graph.find document node_id) (fun (node : Kind_graph.node) -> node.input) in
          let input_count = if kind = "camera" || kind = "root" || kind = "world" then 0 else 1 in
          (* the head: the kind as the sheet writes it (scene/camera), VIEW while displayed,
             the node's number; the name edited in place; what is true of the node *)
          let facts = List.filter_map Fun.id [
            if value.doc.Document.active_camera = Some node_id then Some "active camera" else None;
            if input_count > 0 then Some (Printf.sprintf "%d input%s" input_count (if input_count = 1 then "" else "s"))
            else None;
            if preview then Some "preview instance" else None ] in
          let head = Pxui.Ui.inspector_header ui
              ~key:"flow-inspector-header"
              ~kind:(Printf.sprintf "%s/%s" (if value.level = Document.Scene then "scene" else "world") kind)
              ?badge:(if displayed value = Some node_id then Some "view" else None)
              ~index:(Printf.sprintf "NO. %04d" node_id)
              ?rename:(if preview then None else Some (fun text -> String.trim text <> ""))
              ~title:label
              ~detail:(match facts with
                | [] -> Printf.sprintf "%d parameter%s" (List.length fields) (if List.length fields = 1 then "" else "s")
                | facts -> String.concat " · " facts) () in
          let renamed = head.renamed in
          let rename = if renamed = label || preview then [] else
            [Rename {node = node_id; label = renamed}] in
          rename @ Pxui.Ui.inspector_body ui (fun () ->
          if input_count > 0 then
            ignore (Pxui.Ui.inspector_section ui
              ~key:"flow-input-heading" ~expanded:true "Inputs"
              (fun () ->
                let source = match input with
                  | None -> "empty"
                  | Some id -> Option.fold
                      ~none:("#" ^ string_of_int id)
                      ~some:(fun (node : Kind_graph.node) -> node.label)
                      (Kind_graph.find document id) in
                Pxui.Ui.inspector_readout ui ~width:inspector_width
                  ~key:"flow-input-0" ~label:"in0" ("← " ^ source)));
          if preview then Pxui.Ui.inspector_message ui ~key:"flow-preview"
            "Preview instance. Edit its viewport scene reference or source graph.";
          let edits = match object_rows ~locked:preview fields with
            | Error diagnostic ->
                Pxui.Ui.inspector_message ui ~key:"flow-diagnostic"
                  (Diagnostic.to_string diagnostic); []
            | Ok [] -> []
            | Ok rows ->
                let expanded = fields |> List.filter_map (fun field ->
                  match field.Param.folder with [] -> None
                  | first :: _ -> Some first) |> List.sort_uniq String.compare in
                Pxui_shell.Inspector.flow_fields ui ~expanded
                  ~kind_label:(kind_label kind) ~width:inspector_width rows in
          List.filter_map (function
            | Pxui_shell.Inspector.Edited (path, value) ->
                Some (Set_parameter {node = node_id; path; value})
            | Pxui_shell.Inspector.Expression (path, text) ->
                (* a row is a field of the node; its argument is the parameter holding it (a
                   vector's field is one component of it) *)
                let parameters = Result.value ~default:[] (Flow_sop.Port.parameters ~infer:true fields) in
                let key_sub = List.find_map (fun (p : Flow_sop.Port.parameter) ->
                  match List.find_index (fun (f : Param.field_view) -> f.name = path) p.fields with
                  | Some i -> Some (p.path, if List.length p.fields = 3 then [ i ] else [])
                  | None ->
                      (* a vector's component row is named [row.axis] *)
                      Option.map (fun i -> p.path, [ i ])
                        (List.find_index (fun axis -> p.path ^ "." ^ axis = path) [ "x"; "y"; "z" ])
                      |> fun found -> if List.length p.fields = 3 then found else None) parameters in
                (match expression_text text, key_sub with
                 | Ok expr, Some (key, sub) -> Some (Object_arg { node = node_id; key; sub; expr })
                 | Error message, _ -> Some (Declined message)
                 | Ok _, None -> None)
            | Follow path ->
                let home = match value.level with
                  | Document.Scene -> List.assoc_opt node_id value.doc.homes.objects
                  | Inside _ -> List.assoc_opt node_id value.doc.homes.layers in
                Option.bind home (fun (home : Document.home) -> match home with
                  | Bound_at from ->
                      Option.bind (Flow_graph.Flow_edit.arg_text ws.source from (Kw path)) (fun expr ->
                        match expr.Flow.Syntax.node with
                        | Sym name -> Option.map (fun path -> Follow_source path)
                            (Text_pane.reference_target ws.source ~from name)
                        | _ -> None)
                  | _ -> None)
            | Pinned _ | Reset _ -> None) edits))) in
      None, changes, [], value.live_cook, [], [])
