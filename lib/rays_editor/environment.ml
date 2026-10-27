(* The editor's frame coordinator and resource lifetime. *)
open Rays
open Editor_document
open Sop
open World_map
include Environment_source

let update_with value frame ~inspector =
  let value = follow_focus value frame in
  let ui = value.core.Core.ui in
  let raw_frame = frame in
  let extra, frame = V.begin_frame value.extra frame in
  let visible = V.ui_visible value.control in
  let camera_panel () = V.panel ui ~control:value.control ~camera:value.camera
      ~extra ~inspector in
  let view_handles ui ~selected ~space ~bounds =
    (* a handle is where the picture shows its point: in the view's film, not its pane *)
    V.handles ui ~selected ~scene:(Core.scene value.core) ~space (view_camera value)
      extra ~bounds:(V.film extra ~key:(focus_key value) bounds) in
  let _,lowered=value.core.doc.workspace in
  Workspace_images.bind value.host.images lowered;
  let live={(Live_frame.of_frame frame) with t=Timeline.time(Core.timeline value.core)}in
  let image_errors=ref [] in
  let preview image=match Workspace_images.image value.host.images ~state:value.core.cook.state ~live lowered.plan image with
    |Ok _->()|Error diagnostic->image_errors:=Flow.Diagnostic.to_string diagnostic:: !image_errors in
  (match value.core.scope_key with
   |Some{records=Some records;scope;_}->
       List.iter(fun path->let chains=Flow_graph.Probe.chains scope in
         let probes=List.map(fun zone->Option.value ~default:0(Layout_by_path.Path_map.find_opt zone value.core.probes))
           (Option.value ~default:[](Hashtbl.find_opt chains path))in
         match Flow_graph.Probe.at records path ~probes with
         |Some(Flow_graph.Probe.Value(Flow.Eval.Deferred(Flow.Ty.Named "image",_)as image))->
             preview image
         |Some(Flow_graph.Probe.Image image)->
             preview(Flow.Eval.Deferred(Flow.Ty.image,image.node))
         |_->())(Pxui_graph.Scope.selected value.core.scope_view)
   |_->());
  let update = Flow_sop.Lower.with_images
    ~metadata:(fun plan id->Option.map Rays.Image.get_size(Workspace_images.peek value.host.images plan id))
    (Workspace_images.payload value.host.images)(fun()->Workspace_gpu.with_backend value.host.gpu(fun()->
    Core.update ~image:(Workspace_images.peek value.host.images lowered.plan)
      ~host_events:(Workspace_host.take_events value.host) value.core ~all_ui_visible:visible
      ~text_focus:(Pxui.Ui.text_input_focused ui) ~camera_panel ~view_handles
      ~render_status:(match value.status (Core.prepared value.core), value.render_status with
        | Some sketch, Some render -> Some (sketch ^ " · " ^ render)
        | sketch, None -> sketch | None, render -> render)
      ~error_status:(if !image_errors<>[] then Some(String.concat "; "(List.rev !image_errors))else match value.state_error with
        | Some _ as error -> error
        | None -> match value.context_error, V.render_status value.extra with
          | Some live, Some renderer -> Some (live ^ "; " ^ renderer)
          | Some _ as error, None | None, (Some _ as error) -> error
          | None, None -> None)
      ~view_state:(function
        | Some (_, camera, _, extra, _) -> V.section camera extra
        | None -> V.section value.camera extra) frame)) in
  let value, update = open_dialog value update frame.Frame.events in
  let value, update = open_import value update in
  let value, update = reload_source value update ~now:frame.Frame.time in
  let focused = follow_focus { value with core = update.core } frame in
  let core = update.core and panes = Core.panes update.core frame in
  let control, camera, requests, extra, inspected = match update.panel with
    | Some (control, camera, requests, extra, inspected) ->
        control, (if focused.viewing <> value.viewing then focused.camera else camera), requests, extra, Some inspected
    | None -> value.control, focused.camera, [], extra, None in
  let camera, extra = match update.loaded_view with
    | Some json -> V.restore camera extra json
    | None -> camera, extra in
  let previous_camera = if update.loaded_view = None then focused.camera else camera in
  let control, extra, render_status = List.fold_left
      (fun (control, extra, status) -> function
        | Leader.Hide_ui -> V.toggle_ui control, extra, status
        | Open_camera -> V.open_camera control, extra, status
        | action ->
            let extra, notice = V.apply_action camera extra action in
            control, extra, (if notice = None then status else notice))
      (control, extra, value.render_status) update.actions in
  let core = V.on_doc ~previous:value.core core camera in
  (* Command-S with a draft in a text pane applies it first; one that does not check keeps the
     save waiting, since the file would not have what the pane shows *)
  let core, source = if not (List.mem Leader.Save_source update.actions) then core, value.source else
    match Core.apply_drafts ?except:value.refused core with
    | Ok core -> save_source core value.source (V.section camera extra)
    | Error core ->
        { core with Core.notice = Some (Core.Refusal,
            "Not saved: the text pane's draft does not check · fix or discard it, then save") }, value.source in
  let value = if source != value.source then { value with held = None } else value in
  let area = if visible then panes.view
    else 0, 0, frame.Frame.width, frame.height in
  (* Latch the World operation and target at the owned press. Movement and
     the final release position are reduced before sealing its history. *)
  let seal core = { core with Core.history = Editor_core.History.seal core.Core.history } in
  let world_drag = if update.loaded_view <> None
      || core.Core.level <> value.core.Core.level
      || core.Core.map_view <> value.core.Core.map_view
      || List.exists (function Leader.Undo | Redo | Hide_ui -> true | _ -> false) update.actions
    then None else value.world_drag in
  let modifiers = ref (Event.Private.keys_before ~previous:value.core.Core.held_keys
    ~held:update.input.keys update.input.events) in
  let core, world_drag, events, consumed = List.fold_left
      (fun (core, drag, events, consumed) event ->
        modifiers := Event.Private.keys_after !modifiers event;
        match event, drag with
        | Event.MousePressed (Input.LeftButton, point), None ->
            (match world_operation core area point ~shift:(List.mem Input.Shift !modifiers) with
             | Some operation -> seal core, Some { operation; point; area },
                 Event.PointerCancelled Input.LeftButton :: events, true
             | None -> core, drag, event :: events, consumed)
        | Event.MouseMoved point, Some drag ->
            move_world core drag point, Some { drag with point }, events, true
        | Event.MouseReleased (Input.LeftButton, point), Some drag ->
            seal (move_world core drag point), None,
              Event.PointerCancelled Input.LeftButton :: events, true
        | (Event.PointerCancelled Input.LeftButton | Event.WindowFocusLost), _ ->
            seal core, None, event :: events, consumed || drag <> None
        | _ -> core, drag, event :: events, consumed)
      (core, world_drag, [], false) update.input.events in
  let input = { update.input with events = List.rev events;
    mouse_buttons = if world_drag = None then update.input.mouse_buttons
      else List.filter (( <> ) Input.LeftButton) update.input.mouse_buttons;
    mouse_delta = if consumed then 0., 0. else update.input.mouse_delta } in
  (* A press and release within 4 points is a click: the view picks. *)
  let pick_press, clicks = List.fold_left (fun (press, clicks) -> function
    | Event.MousePressed (Input.LeftButton, point) -> Some point, clicks
    | MouseReleased (Input.LeftButton, (qx, qy)) ->
        (match press with
         | Some (px, py) when Float.hypot (qx -. px) (qy -. py) < 4. ->
             None, (qx, qy) :: clicks
         | _ -> None, clicks)
    | PointerCancelled Input.LeftButton | WindowFocusLost -> None, clicks
    | _ -> press, clicks) (value.pick_press, []) input.events in
  let core = if core.Core.map_view then core else
    List.fold_left (fun core at ->
      let film = V.film extra ~key:(focus_key focused) area in
      let _, _, w, h = film in
      if w <= 0 || h <= 0 then core else
      let picking = { focused with core; camera; extra } in
      match V.pick_ray ~viewport:film (view_camera picking) at with
      | Some (origin, direction) -> Core.pick ?view:focused.viewing ~alt:(List.mem Input.Alt !modifiers) core ~origin ~direction
      | None -> core) core (List.rev clicks) in
  (* a payload carried over a viewport: the surface under the pointer is the place *)
  let core = match Core.carry_viewport core with
    | Some key when not core.Core.map_view ->
        (match List.assoc_opt key (Core.view_bodies core raw_frame) with
         | Some bounds ->
             let picking = { focused with core; camera; extra } in
             let film = V.film extra ~key:(look_key picking key) bounds in
             let _, _, w, h = film in
             if w <= 0 || h <= 0 then core else
             (match V.pick_ray ~viewport:film (camera_of picking key) raw_frame.Frame.mouse with
              | Some (origin, direction) -> Core.carry_over_surface ~view:key core ~origin ~direction
              | None -> core)
         | None -> core)
    | _ -> core in
  let camera, extra = if core.Core.map_view then camera, V.release extra
    else V.navigate ~area control camera extra core ~raw_frame ~input in
  let camera, render_status = match update.framed with
    | Some (Some (min, max)) ->
        let _, _, w, h = panes.view in
        if w > 0 && h > 0 then
          V.frame_bounds ~viewport:panes.view ~min ~max camera, render_status
        else camera, render_status
    | Some None -> camera, Some "Nothing to frame: no cooked points"
    | None -> camera, render_status in
  let core, camera, extra = V.on_view core ~previous:previous_camera ~key:(focus_key focused) camera extra
      ~stepped:(List.exists (function Leader.Undo | Redo -> true | _ -> false) update.actions) in
  let ends_pointer = function
      | Event.MouseReleased (Input.LeftButton, _) | PointerCancelled Input.LeftButton
      | WindowFocusLost -> true | _ -> false in
  let core = if not (Frame.mouse_down Input.LeftButton raw_frame)
      || Frame.has_event ends_pointer raw_frame || Frame.has_event ends_pointer update.input
    then { core with Core.history = Editor_core.History.seal ~gesture_only:true core.Core.history }
    else core in
  let live = world_drag <> None
    || Timeline.mode (Core.timeline core) = Timeline.Playing in
  let baked_from = bake_world ~previous:value.baked_from core ~live in
  let baked = Option.map snd baked_from in
  let baked_views = List.filter_map (fun key ->
    Option.map (fun bake -> key, bake)
      (bake_world ~view:(`Only key) ~previous:(List.assoc_opt key value.baked_views) core ~live))
    (view_keys core) in
  (* the same bakes as last frame: nothing to recompose for *)
  let baked_views = if List.length baked_views = List.length value.baked_views
      && List.for_all2 (fun (k, (_, b)) (k', (_, b')) -> k = k' && b == b') baked_views value.baked_views
    then value.baked_views else baked_views in
  let time = Timeline.time (Core.timeline core) in
  let frame_input = {core.live_frame with Frame_input.t = time;
    frame = Int64.to_int (Timeline.frame (Core.timeline core))} in
  let scene, context_error = match value.resolved with
    | Some (doc, at, scene) when same_context doc core.Core.doc && Frame_input.equal at frame_input -> scene, value.context_error
    | _ ->
        let previous = match value.resolved with
          | Some (doc, _, scene) when same_context doc core.doc -> Some scene
          | _ -> None in
        let scene, errors = Contexts.resolve_scene ?previous ~state:core.cook.state ~live:frame_input
          core.doc ~time in
        scene, (if errors = [] then None else Some (String.concat "; "
          (List.map Flow.Diagnostic.to_string errors))) in
  let context_error=if !image_errors=[] then context_error else Some(String.concat "; "(List.rev !image_errors)) in
  let canvases, context_error = List.fold_left (fun (pictures, error) (leaf : Pxui_shell.Layout.leaf) ->
    match leaf.panel with
    | Canvas key ->
        let _, _, w, h = if not (V.ui_visible control) && core.focus = Canvas key
          then (0, 0, raw_frame.width, raw_frame.height) else leaf.body in
        (* a picture's frame is its pane: frame/width and frame/height read the canvas, not the window *)
        let frame_input = {frame_input with Frame_input.size = (w, h)} in
        let drawing = Option.bind core.doc.Document.shell (fun shell -> List.assoc_opt key shell.canvases) in
        let _, lowered = core.doc.workspace in
        let preview = core.Core.canvas_viewed in
        let drawing = match preview with
          | Some (ty, id) when ty = Flow.Ty.drawing -> Some (Flow.Eval.Deferred (ty, id))
          | _ -> drawing in
        Workspace_images.bind value.host.images lowered;
        let previous = List.assoc_opt key value.canvases in
        let same_plan = Option.fold ~none:false ~some:(fun picture -> picture.plan == lowered.plan) previous in
        let dynamic = match previous with
          | _ when preview <> None -> true
          | Some picture when same_plan && picture.preview = preview -> picture.dynamic
          | _ -> lowered.states <> [] || Array.exists (fun (n : Flow.Eval.node) ->
              List.exists (fun (_, v) -> Flow.Eval.is_live v || Flow.Eval.frame_dependent v) n.args) lowered.plan.nodes in
        (match drawing with
         | None when preview = None -> pictures, error
         | _ when (match preview with Some (ty, _) -> ty = Flow.Ty.image | None -> false) ->
             let id = snd (Option.get preview) in
             (match Workspace_images.image value.host.images ~state:core.cook.state ~live:frame_input lowered.plan
                (Flow.Eval.Deferred (Flow.Ty.image, id)) with
              | Ok img ->
                  let iw, ih = Rays.Image.get_size img in
                  let scale = Float.min (float w /. float iw) (float h /. float ih) in
                  (key, {size = (w, h); dynamic; plan = lowered.plan; scene = [Rays.Scene.rect ~at:(0, 0) ~w ~h ~fill:Color.black (); Rays.Scene.image img ~at:(0, 0) ~scale ()]; prepared = None; preview}) :: pictures, error
              | Error d -> (key, Option.value ~default:{size = (w, h); dynamic; plan = lowered.plan; scene = []; prepared = None; preview} previous) :: pictures,
                  Some (Flow.Diagnostic.to_string d))
         | None -> pictures, error
         | Some _ when same_plan && not dynamic && Option.fold ~none:false
             ~some:(fun picture -> picture.size = (w, h) && picture.preview = preview) previous ->
             (key, Option.get previous) :: pictures, error
         | Some drawing ->
             let prepared = match previous with
               | Some {prepared = Some p; preview = held; _} when same_plan && held = preview -> Ok p
               | _ -> Drawing.prepare ~profile:lowered.profile
                   ~approx:(fst core.doc.workspace).checked.approx ~states:lowered.states lowered.plan drawing in
             (match Result.bind prepared (fun p -> Result.map (fun scene -> p, scene)
                 (Workspace_gpu.with_backend ~fast:(Editor_document.Contexts.fast core.doc) value.host.gpu(fun()->Drawing.render_prepared ~state:core.cook.state
                   ~gpu:(Workspace_gpu.marks value.host.gpu) ?resident:(Workspace_gpu.resident value.host.gpu)
                   ~image:(Workspace_images.image value.host.images ~state:core.cook.state ~live:frame_input lowered.plan)
                   p ~live:frame_input ~size:(w, h)))) with
              | Ok (prepared, scene) -> (key, {size = (w, h); dynamic; plan = lowered.plan; scene;
                  prepared = Some prepared; preview}) :: pictures, error
              | Error d ->
                  (* the last picture stays up, unless its mark sets bind GPU outputs this failed
                     render may already have replaced *)
                  let previous = Option.map (fun p -> match p.prepared with
                    | Some prepared when Drawing.borrows_gpu prepared -> {p with scene = []}
                    | _ -> p) previous in
                  (key, Option.value ~default:{size = (w, h); dynamic; plan = lowered.plan; scene = []; prepared = None; preview} previous) :: pictures,
                  Some (Flow.Diagnostic.to_string d)))
    | _ -> pictures, error) ([], context_error) (Core.geometry core core.workspace raw_frame).leaves in
  let rendered, views, drawn,texture_errors = compose { value with core } { update with core } ~baked ~baked_views ~scene in
  let context_error=if texture_errors=[] then context_error else Some(String.concat "; " texture_errors)in
  let rendering = {focused with core; extra; camera; rendered; views} in
  let bodies = if V.ui_visible control then Core.view_bodies core raw_frame
    else ["@hidden", (0, 0, raw_frame.width, raw_frame.height)] in
  let extra = V.render extra ~pixel_scale:raw_frame.pixel_scale ~focus:(Some (focus_key rendering))
    (List.filter_map (fun (key, bounds) ->
      let scene = if key = "@hidden" then focused_image rendering else
        match List.assoc_opt key views with Some scene -> Some scene | None -> rendered in
      Option.map (fun scene ->
        key, V.film extra ~key:(look_key rendering key) bounds, camera_of rendering key, scene) scene) bodies) in
  let core = { core with Core.view_tools = V.header_tools extra } in
  (* what the panes draw over a view: the gate where it is not the whole pane, and the
     selected object's box in the focused view *)
  let core = { core with
    Core.gates = List.filter_map (fun (key, bounds) ->
      let film = V.film extra ~key:(look_key rendering key) bounds in
      if film = bounds || key = "@hidden" then None else Some (key, film)) bodies;
    selected_box = Option.bind (Core.selected_bounds core) (fun (world, box, name) ->
      let key = focus_key rendering in
      Option.bind (List.assoc_opt key bodies) (fun bounds ->
        Option.map (fun rect -> key, rect, name)
          (V.screen_box (camera_of rendering key) ~bounds:(V.film extra ~key:(look_key rendering key) bounds) ~world box))) } in
  (* the render clock: a restart of the samples starts it, reaching the cap stops it *)
  let core = { core with Core.traces = List.filter_map (fun (key, _) ->
    Option.map (fun (film, samples, cap, bounces) ->
      let now = frame.Frame.time in
      let seconds, since = match List.assoc_opt key value.core.Core.traces with
        | Some (t : Core.trace) when samples >= t.samples && t.film = film ->
            (if t.samples >= cap then t.seconds else now -. t.since), t.since
        | _ -> 0., now in
      key, { Core.film; samples; cap; bounces; seconds; since })
      (V.trace extra ~key:(look_key rendering key))) bodies } in
  let map = match baked, value.map with
    | Some baked, Some (source, image) when baked == source || not core.Core.map_view ->
        ignore baked; Some (source, image)
    | Some baked, previous when core.Core.map_view ->
        let width, height, rgba = map_rgba baked in
        (match Image.upload_rgba ?into:(Option.map snd previous) ~width ~height ~rgba () with
         | Ok image -> Some (baked, image)
         | Error _ -> previous)
    | _, previous -> previous in
  let pending_render = match List.rev requests with
    | request :: _ -> Some request | [] -> None in
  let render_status = if pending_render <> None && rendered = None then
      Some "Render unavailable until the first cook completes" else render_status in
  let value = refresh_hidden { value with core; camera; cameras = focused.cameras; viewing = focused.viewing;
    control; rendered; views; drawn;
    composed = Some (composition_key core scene); resolved = Some (core.doc, frame_input, scene);
    context_error; canvases; baked; baked_from; baked_views; map; world_drag; pick_press;
    pending_render; render_status; extra; source } raw_frame in
  let workspace,lowered=value.core.doc.workspace in
  let value=match Workspace_host.update value.host ~state:value.core.cook.state
      ~live:frame_input workspace lowered.plan with
    |Ok()->value|Error diagnostic->{value with context_error=Some(Flow.Diagnostic.to_string diagnostic)}in
  autosave value ~now:frame.Frame.time, inspected

let update value frame = fst (update_with value frame ~inspector:ignore)

let after_present value frame =
  let value=match Workspace_host.save_pending value.host Canvas.save_screen_png with
    |Ok()->value|Error diagnostic->{value with context_error=Some(Flow.Diagnostic.to_string diagnostic)}in
  match save_status ~save:V.save ~filename:V.filename
      value.pending_render value.rendered with
  | Some status -> refresh_hidden
      { value with render_status = Some status; pending_render = None } frame
  | None -> value

let scene value frame =
  compose_view ?map:(Option.map snd value.map) ~ui_visible:(V.ui_visible value.control)
    ~canvases:value.canvases
    ~background:value.background ~rendered:value.rendered ~views:value.views
    ~focused:(focused_image value)
    ~camera:(view_camera value) ~camera_of:(camera_of value) ~paint_view:(paint_view value)
    ~film:(V.film value.extra ~key:(focus_key value)) ~overlay:value.overlay
    ~guides:(fun bounds -> if value.core.Core.map_view then [] else
      V.guides ~scene:(Core.scene value.core)
      ~selected:(Core.selected_node value.core) ~space:(Core.space value.core)
      (view_camera value) value.extra ~pane:bounds
      ~bounds:(V.film value.extra ~key:(focus_key value) bounds))
    ~cache:value.hidden_scene_cache value.core frame

(* A crash report's editor part: the document as a loadable preset and a
   summary of the view state and undo history. *)
let crash_dump value directory =
  let core = value.core in
  (* document.txt is the text of any document; document.rays is the
     loadable preset, written for workspace documents *)
  Out_channel.with_open_text (Filename.concat directory "document.txt") (fun channel ->
    output_string channel (Document.dump core.doc));
  ignore (Preset.save ~directory ~name:"document" ~doc:core.doc
    ~view:(V.section value.camera value.extra));
  let history = core.history in
  Out_channel.with_open_text (Filename.concat directory "editor.txt") (fun channel ->
    Printf.fprintf channel
      "level: %s\nprojection: %s\npane graph: %s\ngraph panels: %s\nwindows: %s\npanels: %s\nroute: %s\ntext: %s\nmap view: %b\ngraph pan: %b\nguide: %b\nkey hud: %s\nselected: %s\nscope selected: %s\nfocus: %s\nprompt: %s\n\
       undo: %s (%d entries)\nredo: %s\ncook: %s\nedit error: %s\nrenderer: %s\nlive scene: %s\nautosave: %s\n\
       load document.rays with / b (workspace documents only) after copying it to %s\n"
      (Core.level_name core)
      (match Core.projection core with Core.List_view -> "list"
        | Graph_view -> "graph" | Text_view -> "text")
      (Option.value ~default:"-" (Core.graph_name core))
      (Core.graph_panels core) (Core.windows core) (Core.panels_line core)
      (Core.route core)
      (Text_pane.summary core.text)
      core.map_view (Pxui_graph.Scope.Private.grabbed core.scope_view) core.guide (Option.fold ~none:"-" ~some:fst core.hud)
      (Option.fold ~none:"none" ~some:(fun node ->
        Printf.sprintf "%s (#%d, %s)" (Node.label node) (Node.id node) (Node.operation node))
        (Core.selected_node core))
      (match Pxui_graph.Scope.selected core.scope_view with
       | [] -> "-"
       | paths -> String.concat ", " (List.map (String.concat "/") paths))
      (Leader.pane_name core.focus) (Core.prompt_name core)
      (Editor_core.History.label history) (Editor_core.History.depth history)
      (Option.value ~default:"-" (Editor_core.History.redo_label history))
      (Core.status_text core)
      (Option.value ~default:"-" core.edit_error)
      (Option.value ~default:"-" (V.render_status value.extra))
      (Option.value ~default:"-" value.context_error)
      (Option.value ~default:"-" value.state_error) core.presets;
    Printf.fprintf channel "last edit: %.3f ms\nphases: %s\n"
      (core.edit_phases.total *. 1000.) (Flow.Phase_timer.summary core.edit_phases))

let close value =
  let value = autosave ~force:true value ~now:value.state_checked in
  Option.iter prerr_endline value.state_error;
  Option.iter (fun (_, image) -> Image.destroy image) value.map;
  Workspace_host.close value.host;
  V.close value.extra;
  Core.close value.core

let run ?inputs ?name ?presets ?factories ?settings ?lights
    ?camera ?background ?seed ?grain ?domains ?max_entries ?max_payload_bytes
    ?opened ~config ~workspace ?source ~prepare ~draw ?overlay ?status () =
  let name = Option.value name ~default:(Workspace_doc.name workspace) in
  (* [opened], the caller's document of this workspace, is handed to the editor once and not
     kept by the loop: after an edit or a reload only the history decides how long it lives. *)
  let handed = ref opened in
  let init _frame =
    let opened = !handed in
    handed := None;
    let value=create ?inputs ~name ?presets ?factories ?settings
      ?lights
      ?camera ?background ?seed ?grain ?domains ?max_entries ?max_payload_bytes
      ?opened ~workspace ?source ~prepare ~draw ?overlay ?status () |> Result.get_ok in
    value.host.deterministic<-(match config.Sketch.clock with Fixed _->true|Realtime->false);value in
  let update value frame =
    let value = update value frame in
    (* a cursor the platform refuses is said in the strip; the editor runs on *)
    match set_ui_cursor value.core.ui (V.ui_visible value.control) with
    | Ok () -> value
    | Error message -> { value with render_status = Some ("Cursor: " ^ message) } in
  ignore (Sketch.run_state ~config ~init ~update ~view:scene
    ~after_present ~crash_dump ~on_stop:close ())
