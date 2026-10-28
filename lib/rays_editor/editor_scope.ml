open Editor_model
open Editor_document
open Editor_shell
open Editor_panes
open Editor_inspect

(* the plan node of a drawing or image at [path], with its type *)
let picture_node records path ~probes =
  List.find_map (fun ty -> Option.map (fun id -> ty, id) (Flow_graph.Probe.plan_node ~ty records path ~probes))
    Flow.Ty.[ drawing; image ]

(* Probe inputs use the same exact frame/region compiler as the canvas. *)
let probe_force = Flow_native.Probe_run.force

(* The probes of a live graph are forced again only when what a frame can change in them did: its
   time, size, pointer and buttons.  [tick], [dt] and the key and event lists change every frame, so
   comparing them re-ran every probe of an open graph on each frame of a drag or a scroll
   (measured at about 100 ms a frame on recursive_rectangles).  ponytail: a graph that reads
   frame/tick or frame/dt shows a stale probe while the timeline is paused; its canvas still animates. *)
let steady_equal (a : Input.t) (b : Input.t) =
  let steady (f : Input.t) = { f with tick = 0; dt = 0.; keys = []; events = [] } in
  Input.equal (steady a) (steady b)

let sync_views value =
  let lowered = value.doc.Document.workspace.lowered in
  match value.view_key with
  | Some (previous, probes, previews, canvas) when previous == lowered && probes == value.probes
      && previews == value.previews && canvas == value.canvas_preview -> value
  | _ when Document.Int_map.is_empty value.previews && value.canvas_preview = None ->
      { value with viewed = Document.Int_map.empty; canvas_viewed = None;
        view_key = Some (lowered, value.probes, value.previews, value.canvas_preview) }
  | _ ->
      let records = Flow_graph.Probe.make ~dynamic:(Flow_sop.Lower.zone_count lowered) lowered.evaluated in
      let probes_of (preview : preview) = List.map (fun path -> Option.value ~default:0
        (Layout_by_path.Path_map.find_opt path value.probes)) preview.chain in
      let previews, viewed = Document.Int_map.fold (fun object_id (preview : preview) (previews, viewed) ->
        let probes = probes_of preview in
        let viewed_node = Option.bind (Flow_graph.Probe.plan_node records preview.path ~probes) (fun node ->
          Option.bind (Document.Int_map.find_opt object_id value.doc.Document.networks)
            (fun network -> lowered.preview ~node ~probes network.graph)) in
        match viewed_node with
        | Some viewed_node ->
            Document.Int_map.add object_id preview previews,
            Document.Int_map.add object_id viewed_node viewed
        | None -> previews, viewed) value.previews (Document.Int_map.empty, Document.Int_map.empty) in
      let canvas_preview, canvas_viewed = match value.canvas_preview with
        | None -> None, None
        | Some preview ->
            let shown = picture_node records preview.path ~probes:(probes_of preview) in
            (match shown with Some _ -> value.canvas_preview, shown | None -> None, None) in
      { value with previews; viewed; canvas_preview; canvas_viewed;
        view_key = Some (lowered, value.probes, previews, canvas_preview) }

let view_node value path =
  match pane_laid value with
  | Some key ->
      let chain = Option.value ~default:[] (Hashtbl.find_opt (Flow_graph.Probe.chains key.scope) path) in
      let probes = List.map (fun path -> Option.value ~default:0
        (Layout_by_path.Path_map.find_opt path value.probes)) chain in
      let node = Option.bind key.records (fun records -> Flow_graph.Probe.plan_node records path ~probes) in
      let lowered = value.doc.workspace.lowered in
      let owner = Option.bind node (fun node ->
        let owns id = Option.bind (Document.Int_map.find_opt id value.doc.networks)
          (fun network -> lowered.preview ~node ~probes network.graph) <> None in
        match pane_level value with
        | Inside id when owns id -> Some id
        | _ -> Document.Int_map.fold (fun id _ found ->
            match found with Some _ -> found | None -> if owns id then Some id else None)
            value.doc.networks None) in
      (match owner with
       | Some owner -> sync_views { value with previews = Document.Int_map.add owner { path; chain } value.previews }
       | None ->
           let picture = Option.bind key.records (fun records -> picture_node records path ~probes) in
           if picture = None then value
           else match value.canvas_preview with
             | Some held when held.path = path -> sync_views { value with canvas_preview = None }
             | _ -> sync_views { value with canvas_preview = Some { path; chain } })
  | None -> value

(* Lay the workspace pane out again when the document, the probes or the
   graph changed; the footers are rebuilt when the recording evaluation, the
   cook's geometry counts or (a live document) the time changed.  The
   evaluation is retained by lowering, never repeated by a pane or a frame. *)
let sync_scope value =
  let value = sync_views value in
  let value = match pane_laid value with
    | Some key -> with_pane value (fun pane -> { pane with view = Pxui_graph.Scope.with_display (viewed_path value key.scope) pane.view })
    | None -> value in
  match graph_name value, value.doc.Document.workspace, Lazy.force value.flow_catalog with
  | Some name, { Document.ws; lowered; _ }, Some catalog ->
      let module M = Layout_by_path.Path_map in
      let previous = (pane_laid value) in
      let arguments = Option.bind previous (fun k -> if k.graph <> name || k.ws.layout.collapsed != ws.layout.collapsed then None else
        match Workspace_doc.literal_changes ~previous:k.ws ws with
        | Some _ as changes -> changes
        | None when Flow_graph.Projection.same_graph k.ws.checked ws.checked name -> Some []
        | None -> None) in
      let edits = Option.value ~default:[] arguments |> List.map (fun (change : Literal_edit.change) ->
        change.path, Flow_graph.Flow_edit.Kw change.field, change.expr) in
      let same k = k.graph = name && k.ws.layout == ws.layout && Option.is_some arguments in
      let moved = match previous with
        | Some k -> not (same k && k.probe_map == value.probes) | None -> true in
      let evaluated = match previous with
        | Some k when Option.fold ~none:false ~some:(( == ) lowered.evaluated) k.evaluated -> k.evaluated
        | _ -> Some lowered.evaluated in
      let time = if not (Flow.Workspace.Paths.is_empty ws.checked.live)
        then Some {value.live_frame with Input.t = Timeline.time value.timeline;
          frame = Int64.to_int (Timeline.frame value.timeline)} else None in
      let now = value.clock () in
      let summaries = value.cook.Cook.summaries in
      let executions = Flow_native.Probe_run.reports lowered.profile in
      (* a moving clock or pointer re-forces the probes at most five times a second: a forced frame sets the quiet time to four
         times its own length, so forcing never takes more than a fifth of the time (a pointer read by
         a deep fold re-forced 0.1 s of probes every frame of a move) *)
      (* the executions are a list forcing a probe rewrites, so it is never physically the same twice:
         comparing it by identity made every frame fresh, and each fresh frame forced the probes
         again.  Their count changes when a probe first runs, which is when a footer needs them. *)
      let quiet (k : scope_key) = Float.min 2. (Float.max 0.2 (4. *. (now -. k.stamp))) in
      let fresh = match previous with
        | Some k -> not (k.evaluated == evaluated && k.summaries == summaries
            && Flow_native.Probe_run.same_count k.executions executions
            && (Option.equal steady_equal k.time time
              || (now -. k.stamp < k.gap && Option.is_some k.time = Option.is_some time)))
        | None -> true in
      if not moved && not fresh then
        (* the first call after a forced frame learns what that frame cost *)
        with_pane value (fun pane -> { pane with laid = Option.map (fun k ->
            { k with fresh_last = false;
                     gap = if k.fresh_last then quiet k else k.gap })
            pane.laid })
      else begin
        let stamp = match previous with Some k when not fresh -> k.stamp | _ -> now in
        let gap = match previous with
          | Some k when k.fresh_last && fresh -> quiet k
          | Some k -> k.gap
          | None -> 0.2 in
        let scope = match previous with
          | Some k when Option.is_some arguments ->
              if edits = [] then k.scope else Flow_graph.Projection.with_arguments edits k.scope
          | _ -> Flow_graph.Projection.of_graph ~folded:(fun p -> M.find_opt p ws.layout.collapsed <> Some false) catalog ws.checked name in
        let scope_view = if not moved then
          Pxui_graph.Scope.with_arguments edits (pane_view value) else begin
          let layout = ws.layout in
          let display = viewed_path value scope in
          let layouts = match value.doc.Document.shell with
            | Some { switch = Some _; _ } when Option.map (fun (g : Flow.Workspace.graph) -> g.name)
                (Workspace_doc.editor_graph (value.doc.Document.workspace.ws)) = Some name -> List.map fst (layouts value)
            | _ -> [] in
          Flow.Phase_timer.measure "layout" (fun () -> Pxui_graph.Scope.with_scope ~layouts ~key:name scope (pane_view value)
            ~imported:(Workspace_doc.imported_file ws)
            ~at:(fun path -> M.find_opt path layout.at)
            ~level:(fun path -> match M.find_opt path layout.level, M.find_opt path layout.pinned with
              | None, None -> None
              | level, pinned -> Some (Option.value ~default:Flow_graph.Projection.Card level,
                                       Option.value ~default:false pinned))
            ~pin:(fun path label -> Option.bind (M.find_opt path layout.rows) (Layout_by_path.String_map.find_opt label))
            (* a function or loop starts open, its body in view: c folds it *)
            ~collapsed:(fun path -> M.find_opt path layout.collapsed = Some true)
            ~probe:(fun path -> Option.value ~default:0 (M.find_opt path value.probes))
            ~frames:(fun path -> List.map (fun (f : Layout_by_path.frame) -> f.title, f.at, f.size)
              (Option.value ~default:[] (M.find_opt path layout.frames)))
            ?display) end in
        let geometry id = Option.bind (Flow_sop.Network.Int_map.find_opt id lowered.compiled)
          (fun node_id -> Option.bind (node_owner value node_id) (fun object_id ->
            Cook.geometry value.cook ~object_id ~node_id)) in
        (* a pick in another graph's geometry asked for its node to be selected here *)
        let scope_view, select_later =
          if value.select_later <> [] && List.for_all (fun p -> List.hd p = name) value.select_later
          then Pxui_graph.Scope.select value.select_later scope_view, [] else scope_view, value.select_later in
        let element zone k = Option.map (fun (name, (x, y, z)) -> [ name, Flow.Eval.Vec3 (x, y, z) ])
          (Flow_sop.Lower.zone_element lowered zone k) in
        let resolve = Flow_sop.Attribute_kernel.resolve ~geometry:(fun id ->
          Option.bind (Flow_sop.Network.Int_map.find_opt id lowered.compiled) (fun node_id ->
            Option.bind (node_owner value node_id) (fun object_id ->
              Cook.source value.cook ~object_id ~node_id))) in
        let execution path ~probes =
          let instance = Array.find_index (fun (i : Flow.Eval.instance) -> i.graph = name && i.default)
            lowered.plan.instances in
          Flow_native.Probe_run.find executions ~instance path ~probes in
        let force = match previous with
          | Some k when k.evaluated == evaluated -> k.force
          | _ -> probe_force () in
        (* a moved node or a dragged pane changes the layout, not the values: the records keep their
           memo of footers and probes (a rebuild each frame of a drag cost about 0.8 s here) *)
        let reuse = match previous with Some k when not fresh -> k.records | _ -> None in
        let records = if reuse <> None then reuse else Option.map (Flow_graph.Probe.make ~force ~state:value.cook.state
          ?live:time ~geometry ~resolve ~execution
          ~bulk_calls:(Flow_sop.Lower.field_calls ~state:value.cook.state
            ~live:(Option.value ~default:(Input.at_time 0.) time) ~resolve lowered)
          ~image:(Flow_sop.Lower.image_metadata lowered.plan)
          ~dynamic:(Flow_sop.Lower.zone_count lowered) ~element) evaluated in
        let scope_view = match records with
          | Some records when fresh || moved -> Pxui_graph.Scope.with_records records scope_view
          | _ -> scope_view in
        let targets = match records, previous with
          | Some _, Some k when not moved && (k.evaluated == evaluated ||
              Option.fold ~none:false ~some:(fun ev -> ev.Flow.Eval.records == lowered.evaluated.records) k.evaluated) -> k.targets
          | Some records, _ ->
              List.filter_map (fun id ->
                Option.bind (Flow_sop.Network.Int_map.find_opt id lowered.compiled) (fun node_id ->
                  Option.map (fun object_id -> object_id, node_id) (node_owner value node_id)))
                (Flow_graph.Probe.geometry_targets records scope
                   ~probe:(fun path -> Option.value ~default:0 (M.find_opt path value.probes)))
          | None, _ -> [] in
        with_pane { value with select_later } (fun pane -> { pane with view = scope_view;
          laid = Some { ws; probe_map = value.probes; graph = name;
            evaluated; summaries; executions; time; records; force; scope; targets; stamp; gap; fresh_last = fresh } })
      end
  | _ -> value

(* The inspector's subject for a scene object or World layer: the graph projection of the graph
   the text binds it in, and its path there.  The pane's own projection when it shows that graph,
   else one made for the inspector alone (ponytail: laid out again each frame the inspector shows
   such an object; the scene graphs are small). [None] for what the text does not bind by name
   (a host object, a loop's copy, an inline call): those keep the derived-node sheet. *)
let scene_target value id =
  let homes = value.doc.Document.homes in
  let home = List.assoc_opt id (match pane_level value with
    | Document.Scene -> homes.objects | Inside _ -> homes.layers) in
  match home, Lazy.force value.flow_catalog with
  | Some (Document.Bound_at (graph :: _ :: _ as path)), Some catalog ->
      let laid = match pane_laid value with
        | Some k when k.graph = graph -> Some k
        | _ ->
            let ws = value.doc.Document.workspace.ws in
            let evaluated = value.doc.Document.workspace.lowered.evaluated in
            Some { ws; probe_map = value.probes; graph; evaluated = Some evaluated; summaries = [];
                   time = None; executions = Flow_native.Probe_run.no_reports;
                   records = Some (Flow_graph.Probe.make ~state:value.cook.state evaluated);
                   force = probe_force (); targets = []; stamp = 0.; gap = 0.; fresh_last = false;
                   scope = Flow_graph.Projection.of_graph catalog ws.checked graph
                     ~folded:(fun p -> Layout_by_path.Path_map.find_opt p ws.layout.collapsed <> Some false) } in
      Option.bind laid (fun (k : scope_key) ->
        Option.map (fun _ -> k, path) (Flow_graph.Projection.find k.scope path))
  | _ -> None
