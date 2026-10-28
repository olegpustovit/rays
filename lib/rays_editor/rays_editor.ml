
open Editor_document
module Settings = Settings
module Source = Source_file
module Workspace_doc = Workspace_doc
module Timeline = Timeline

let workspace_catalog ?(factories = Sop.Nodes.factories) () =
  Kinds.catalog ~version:Flow_sop.Manifest.version factories

type window = Lowering.window =
  { title : string; width : int; height : int; fps : int; seed : int; fast : bool }
let workspace_window = Lowering.window

module Renderer = Renderer

module Editor = struct
  open Loop_view
  open Loop_model
  open Loop_source
  let update_with = Editor_loop.update_with
  let update = Editor_loop.update
  let after_present = Editor_loop.after_present
  let scene = Editor_loop.scene
  let crash_dump = Editor_loop.crash_dump
  let close = Editor_loop.close
  let run = Editor_loop.run
  type 'prepared t = 'prepared Loop_model.t
  let camera = Loop_model.camera
  let can_redo = Loop_model.can_redo
  let can_undo = Loop_model.can_undo
  let carry_line = Loop_model.carry_line
  let carrying = Loop_model.carrying
  let displayed_node = Loop_model.displayed_node
  let document = Loop_model.document
  let edit = Loop_model.edit
  let edit_layout = Loop_model.edit_layout
  let edit_phases = Loop_model.edit_phases
  let level = Loop_model.level
  let lights = Loop_model.lights
  let node_box = Loop_model.node_box
  let objects = Loop_model.objects
  let panes = Loop_model.panes
  let prepared = Loop_model.prepared
  let probe = Loop_model.probe
  let redo_label = Loop_model.redo_label
  let scene_document = Loop_model.scene_document
  let network = Loop_model.network
  type picked = Core_model.picked = { id : int; label : string; kind : string; fields : Param.field_view list }
  let selected_node = Loop_model.selected_node
  let set_probe = Loop_model.set_probe
  let set_settings = Loop_model.set_settings
  let settings = Loop_model.settings
  let timeline = Loop_model.timeline
  let undo_label = Loop_model.undo_label
  let viewport_camera = Loop_model.viewport_camera
  let workspace = Loop_model.workspace
  let world = Loop_model.world
  module Private = struct
    let gpu_qualification value=Flow_native.Workspace_gpu.qualification value.Loop_model.host.gpu
    let image_plan value=(value.Loop_model.core.doc.workspace.lowered).plan
    let image_payload ?state ?live value image=
      let core=value.Loop_model.core in
      Flow_native.Workspace_images.bind value.host.images (core.doc.workspace.lowered);
      Flow_native.Workspace_images.payload value.host.images (image_plan value)
        ~state:(Option.value ~default:core.cook.state state)
        ~live:(Option.value ~default:core.live_frame live) image
    let image_stats value = let resources=value.Loop_model.host.resources in
      resources.images_created,resources.images_destroyed
    let status_text value=Core_status.status_text value.Loop_model.core
    let live_frame value=value.Loop_model.core.live_frame
    let inspector_subject value=Core_panels.inspector_subject value.Loop_model.core
    let new_graph value context=snd (Core_inspect.new_graph value.Loop_model.core context)
    let menu_forms = List.map (fun (_, form, text, _) -> form, text) Core_add.forms
    let lisp_completions value text =
      Lisp_text.complete (Lazy.force value.Loop_model.core.lisp_vocab) text (String.length text)
    let image_render_stats value=Flow_native.Workspace_images.render_stats value.Loop_model.host.images
    let canvas_scenes value=List.map (fun (_,(p:Loop_view.canvas_picture))->p.scene) value.Loop_model.canvases
    let image_gpu_stats value=Flow_native.Workspace_gpu.image_stats value.Loop_model.host.gpu
    let host_stats value = let host=value.Loop_model.host in
      host.quit_requested,host.fired,host.resources.samples_created,host.resources.samples_destroyed
    let image value image = let core=value.Loop_model.core in
      Flow_native.Workspace_images.bind value.host.images (core.doc.workspace.lowered);
      let gpu=Flow_native.Workspace_gpu.config ~fast:(Editor_document.Lowering.fast core.doc) value.host.gpu in
      Flow_native.Workspace_images.image ~gpu value.host.images ~state:core.cook.state ~live:core.live_frame
        (core.doc.workspace.lowered).plan image
    let with_images ?state ?live ?plan value run =
      let core = value.Loop_model.core in
      let state = Option.value ~default:core.cook.state state
      and live = Option.value ~default:core.live_frame live
      and plan = Option.value ~default:(core.doc.workspace.lowered).plan plan in
      Flow_native.Workspace_images.bind value.host.images (core.doc.workspace.lowered);
      Flow_native.Workspace_gpu.with_backend ~fast:(Editor_document.Lowering.fast core.doc) value.host.gpu (fun gpu ->
        Flow_sop.Lower.with_images ~gpu
          ~metadata:(fun plan id -> Option.map Rays.Image.dimensions (Flow_native.Workspace_images.peek value.host.images plan id))
          (Flow_native.Workspace_images.payload value.host.images) (fun () ->
            run ~image:(Flow_native.Workspace_images.image ~gpu value.host.images ~state ~live plan)
              ~texture:(Flow_native.Workspace_images.texture ~gpu value.host.images ~state ~live plan)))
  end

  type render_settings = Objects.Root.render = { width : int; height : int; max_spp : int }
  let render_camera value = (extra value).Viewport.document_camera
  let view_camera = view_camera
  let render_settings value = Objects.Root.render (extra value).Viewport.root
  let film = film

  type slot = { film : int * int; step : int; samples : int; max_spp : int; viewports : int }
  let slot value key =
    Option.map (fun (info : Renderer.info) ->
      { film = info.size; step = info.step; samples = info.samples; max_spp = info.cap;
        viewports = info.viewports })
      (Renderer.info (extra value).Viewport.renderer ~key)
  let flying value = (extra value).Viewport.fly <> None
  let look_through value = Viewport.look_through (extra value)
  let renderer value = (extra value).Viewport.renderer.mode
  let set_renderer value mode =
    {value with extra = {value.extra with Viewport.renderer_request = Some mode}}

  let create ?name ?presets ?factories ?settings ?camera ?seed ?grain ?domains
      ?max_entries ?max_payload_bytes ?await ?carry_budget ?preferences ?ui_text_size ?ui_font ~workspace ?source ~prepare ~draw () =
    Loop_model.create ?name ?presets ?factories ?settings ?camera ?seed ?grain ?domains
      ?max_entries ?max_payload_bytes ?await ?carry_budget ?preferences ?ui_text_size ?ui_font ~workspace ?source ~prepare ~draw ()

  let run ?inputs ?name ?factories ?camera ?seed ?grain ?max_entries
      ?max_payload_bytes ~config ~workspace ?source ~prepare ~draw () =
    Editor_loop.run ?inputs ?name ?factories ?camera ?seed ?grain ?max_entries
      ?max_payload_bytes ~config ~workspace ?source ~prepare ~draw ()

end

module Reduce = struct
  let open_import (editor : _ Editor.t) file =
    {editor with Loop_model.core = {editor.core with Core_model.open_import = Some file}}
  let spreadsheet (editor : _ Editor.t) path =
    let key = Core_shell.panel_key editor.core.doc path in
    let pane = Core_panes.shown_as editor.core (key, path, Pxui_shell.Layout.Spreadsheet) in
    (Core_panes.local_of editor.core key).sheet_owner, Core_inspect.spreadsheet_source pane
  let select_path (editor : _ Editor.t) path = match path with
    | graph :: _ -> {editor with Loop_model.core = { (Core_panes.go editor.core graph) with Core_model.select_later = [path] }}
    | [] -> editor
  let view (editor : _ Editor.t) path =
    {editor with Loop_model.core = Core_scope.view_node editor.core path}
  let step (e : _ Editor.t) ?select ?preview actions frame =
    { e with Loop_model.core = Core_reduce.reduce_idle ?select ?preview e.Loop_model.core actions frame }
end

module Private = struct
  module Spreadsheet = Spreadsheet
  module Render_budget = struct let film = Renderer.film let next_turn = Renderer.next_turn end
  module Editor_commands = Editor_commands module Schedule = Schedule
  module Document = Document module Preset = Preset
  module Pick = Pick
  module Cook = Cook
  module Text_pane = Text_pane
  module Lisp_text = Lisp_text
  module Navigator = Navigator
  module Bars = Bars
end

module Workspace = struct
  type source = { path : string; digest : string }
  type process = { max_frames : int option; crash_dir : string option; profile : bool;
    preferences : string option; ui_font : string option }
  let no_process = { max_frames = None; crash_dir = None; profile = false; preferences = None; ui_font = None }

  let load ?ops ?imports ?factories text =
    match workspace_catalog ?factories () with
    | Error d -> Error [ d ]
    | Ok catalog -> Workspace_doc.of_text ?ops ?imports catalog text

  (* A hand-written host: the checked document and its file, or diagnostics. *)
  (* The viewport starts where the scene's first camera is (else the default orbit); the host's
     light below is a default of a workspace with no scene graph (Lowering.of_workspace).
     Piece preparation is shared with the hand-written shattered-cube host. *)
  (* Every [sop] graph of the document, lowered and compiled, for headless cooking. *)
  let sop_graphs ?(factories = Sop.Nodes.factories) (doc : Workspace_doc.t) =
    let ( let* ) = Result.bind in
    let* lowered = Result.map_error Diagnostic.to_string
        (Flow_sop.Lower.workspace ~extra:Kinds.descriptors ~inputs:doc.inputs ~factories doc.source) in
    List.fold_right (fun (g : Flow_sop.Lower.graph) rest ->
      let* rest = rest in
      match g.root with
      | None -> Ok rest
      | Some node_id ->
          let* graph = Sop.Edit_graph.compile_node g.network.geometry ~node_id in
          Ok ((g.name, graph) :: rest)) lowered.graphs (Ok [])

  let document_camera (document : Document.t) base =
    let scene = document.scene in
    match Option.bind (List.nth_opt (Objects.ids "camera" scene) 0) (fun node_id ->
        Option.bind (Kind_graph.find scene node_id) Objects.Camera.of_node) with
    | Some (view, _) -> Rays.Easy_camera.of_view ~eye:(Rays.Camera.position view)
        ~target:(Rays.Camera.target view) base
    | None -> base

  let declared_camera ?(factories = Sop.Nodes.factories) doc base =
    match Lowering.of_workspace ~factories doc with
    | Error _ -> base
    | Ok document -> document_camera document base

  (* One static evaluation a start: the document the editor opens is built here, and the window
     and the camera are read from it. A document that does not build leaves each reader the
     answer it had alone (the window's own evaluation, the base camera, the editor's error). An
     explicit empty factory list is the one case where the camera and the editor never read the
     same document, so it shares nothing. *)
  let run ?inputs ?factories ?source ?(process = no_process) doc =
    let doc = match inputs with None -> doc | Some inputs -> {doc with Workspace_doc.inputs} in
    let gpu_owner = Flow_native.Workspace_gpu.create () in
    Fun.protect ~finally:(fun () -> Flow_native.Workspace_gpu.close gpu_owner) (fun () ->
      let gpu=Flow_native.Workspace_gpu.config gpu_owner in
      let source = Option.bind source (fun { path; digest } -> Source.find ~path ~digest)
        |> Option.map (fun source -> Source_file.with_imports source (Workspace_doc.import_texts doc)) in
      let opened = match factories with
        | Some [] -> None
        | _ -> Result.to_option (Lowering.of_workspace
            ~gpu ~factories:(Option.value factories ~default:Sop.Nodes.factories) doc) in
      let window=match opened with Some document->Lowering.document_window document
        |None->workspace_window doc in
      Result.bind window (fun window ->
        let config = { Rays.Sketch.default_config with width = window.width; height = window.height;
                       title = window.title; fps = Some window.fps } in
        let prepare _ (output : Sop.Session.output) =
          let ( let* ) = Result.bind in
          let* geometry = Sop.Payload.geometry output.payload
            |> Result.map_error Diagnostic.error_to_string in
          Rdk_rays.Surface.of_geometry ?instances:output.instances geometry in
        let scene3 node surface =
          let explosion = Rdk_rays.Packed_pieces.explosion
            ~operation:(Sop.Node.operation node) ~fields:(Sop.Node.parameter_fields node) in
          Rdk_rays.Surface.scene3 ?explosion surface in
        let lights = [ Rays.Light.directional ~direction:(Rays_math.Vec3.create (-1.) (-1.4) (-0.8))
                         ~diffuse:Rays_math.Color.white () ] in
        let base = Rays.Easy_camera.create ~target:Rays_math.Vec3.zero ~distance:3.6 ~azimuth:0.4 ~elevation:0.6 () in
        let camera = match opened, factories with
          | Some document, _ -> document_camera document base
          | None, Some [] -> declared_camera ~factories:[] doc base
          | None, _ -> base in
        Editor_loop.run ?inputs ~gpu_owner ?max_frames:process.max_frames ?crash_dir:process.crash_dir ~profile:process.profile ?preferences:process.preferences ?ui_font:process.ui_font ~config ~lights ~camera ?factories ~seed:(Int64.of_int window.seed) ?opened
          ~workspace:doc ?source ~prepare ~draw:scene3 ()
          |> Result.map_error (fun error -> Diagnostic.error ~code:"E_RUNTIME"
            (Rays.Sketch.error_to_string error))))

  let export ?graph ?(fps = 60) ~directory ~frames doc =
    let factories = Sop.Nodes.factories in
    let ( let* ) = Result.bind in
    let* () = if fps > 0 && frames > 0 then Ok () else
      Error (Diagnostic.error ~code:"E_EXPORT_RANGE" "Export frame count and fps must be positive.") in
    let* window = workspace_window doc in
    let* lowered = Flow_sop.Lower.of_checked ~reference:true ~factories ~inputs:doc.inputs doc.checked in
    let evaluated=lowered.evaluated in
    let* ()=Flow_native.Workspace_host.export_check doc evaluated.plan in
    let chosen = List.find_opt (fun (g : Flow.Workspace.graph) ->
      g.context = Flow.Context.draw && Option.fold ~none:true ~some:((=) g.name) graph) doc.checked.graphs in
    match chosen with
    | None -> Error (Diagnostic.error ~code:"E_DRAW_GRAPH" "Export needs a draw graph.")
    | Some graph ->
        let state = Flow.Eval.create_state () in
        let host=Flow_native.Workspace_host.create ~seed:(Int64.of_int window.seed)()in
        Flow_native.Workspace_images.bind host.images lowered;
        let value = List.assoc graph.name evaluated.results in
        let* prepared = Flow_native.Drawing.prepare ~states:evaluated.states evaluated.plan value in
        let view () frame =
          let live = {(Input.at_time frame.Rays.Frame.input.t) with
            dt = 1. /. float fps; frame = frame.input.frame; tick = frame.input.tick;
            size = (window.width, window.height)} in
          match Flow_native.Drawing.render_prepared ~state prepared
            ~image:(Flow_native.Workspace_images.image ~display:false host.images ~state ~live evaluated.plan) ~live ~size:live.size with
          | Ok scene -> (match Flow_native.Workspace_host.export_update host ~state ~live doc evaluated.plan with
              |Ok()->scene|Error d->Flow.Value.raise_diagnostic d)
          | Error d -> Flow.Value.raise_diagnostic d in
        let config = {Rays.Sketch.default_config with width = window.width; height = window.height;
          title = window.title;  clock = Rays.Sketch.Fixed (1. /. float fps)} in
        let after_present () _=match Flow_native.Workspace_host.save_pending host Rays.Canvas.save_screen_png with
          |Ok()->()|Error d->Flow.Value.raise_diagnostic d in
        (try match Rays.Sketch.export_state ~config ~fps ~directory ~frames
           ~init:(fun _ -> ()) ~update:(fun () _ -> ()) ~view ~after_present
           ~on_stop:(fun()->Flow_native.Workspace_host.close host) () with
           |Ok _->Ok()
           |Error error->Error(Diagnostic.error~code:"E_EXPORT"(Rays.Sketch.error_to_string error))
         with Diagnostic.Error d -> Error d)

  let main ?text ?process path =
    let ( let* ) = Result.bind in
    let* text = match text with Some text -> Ok text | None ->
      try Ok (In_channel.with_open_bin path In_channel.input_all)
      with Sys_error message -> Error [Diagnostic.error ~code:"E_IO" message] in
    let* imports = Source_file.read_imports ~file:path text in
    let* doc = load ~imports text in
    Result.map_error(fun diagnostic->[diagnostic])
      (run ?process ~source:{ path; digest = Lowering.sha256 text } doc)
end
