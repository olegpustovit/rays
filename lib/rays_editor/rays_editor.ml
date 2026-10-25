
open Editor_document
module Settings = Settings
module Source = Source_file
module Workspace_doc = Workspace_doc
module Drawing = Drawing
module Live_frame = Live_frame
module Packed_pieces = Packed_pieces
module Surface = Surface
module Timeline = Timeline

let workspace_catalog ?(factories = Sop_catalog.Editor.factories) () =
  Contexts.catalog ~version:Flow_sop.Manifest.version factories

type window = Contexts.window =
  { title : string; width : int; height : int; fps : int; seed : int }
let workspace_window = Contexts.window

module Renderer = Renderer

module Editor = struct
  include Environment
  module Private = struct
    let gpu_qualification value=Workspace_gpu.qualification value.Environment.host.gpu
    let image_plan value=(snd value.Environment.core.doc.workspace).plan
    let image_payload ?state ?live value image=
      let core=value.Environment.core in
      Workspace_images.bind value.host.images (snd core.doc.workspace);
      Workspace_images.payload value.host.images (image_plan value)
        ~state:(Option.value ~default:core.cook.state state)
        ~live:(Option.value ~default:core.live_frame live) image
    let image_stats value = let resources=value.Environment.host.resources in
      resources.images_created,resources.images_destroyed
    let status_text value=Core.status_text value.Environment.core
    let live_frame value=value.Environment.core.live_frame
    let inspector_subject value=Core.inspector_subject value.Environment.core
    let new_graph value context=snd (Core.new_graph value.Environment.core context)
    let menu_forms = List.map (fun (_, form, text, _) -> form, text) Core.forms
    let lisp_completions value text =
      Lisp_text.complete (Lazy.force value.Environment.core.lisp_vocab) text (String.length text)
    let image_render_stats value=Workspace_images.render_stats value.Environment.host.images
    let canvas_scenes value=List.map (fun (_,(p:Environment.canvas_picture))->p.scene) value.Environment.canvases
    let image_gpu_stats value=Workspace_gpu.image_stats value.Environment.host.gpu
    let host_stats value = let host=value.Environment.host in
      host.quit_requested,host.fired,host.resources.samples_created,host.resources.samples_destroyed
    let image value image = let core=value.Environment.core in
      Workspace_images.bind value.host.images (snd core.doc.workspace);
      Workspace_images.image value.host.images ~state:core.cook.state ~live:core.live_frame
        (snd core.doc.workspace).plan image
    let with_images ?state ?live ?plan value run =
      let core = value.Environment.core in
      let state = Option.value ~default:core.cook.state state
      and live = Option.value ~default:core.live_frame live
      and plan = Option.value ~default:(snd core.doc.workspace).plan plan in
      Workspace_images.bind value.host.images (snd core.doc.workspace);
      Flow_sop.Lower.with_images
        ~metadata:(fun plan id -> Option.map Rays.Image.get_size (Workspace_images.peek value.host.images plan id))
        (Workspace_images.payload value.host.images) (fun () ->
          run ~image:(Workspace_images.image value.host.images ~state ~live plan)
            ~texture:(Workspace_images.texture value.host.images ~state ~live plan))
  end

  type render_settings = Objects.Root.render = { width : int; height : int; max_spp : int }
  let render_camera value = (extra value).Viewport.document_camera
  let view_camera value = view_camera value
  let render_settings value = Objects.Root.render (extra value).Viewport.root
  let film value frame = film value frame

  type slot = { film : int * int; step : int; samples : int; max_spp : int; viewports : int }
  let slot value key =
    Option.map (fun (info : Renderer.info) ->
      { film = info.size; step = info.step; samples = info.samples; max_spp = info.cap;
        viewports = info.viewports })
      (Renderer.info (extra value).Viewport.renderer ~key)
  let take_export value = take_export value
  let set_render_status value status = set_render_status value status
  let flying value = (extra value).Viewport.fly <> None
  let look_through value = Viewport.look_through (extra value)
  let renderer value = (extra value).Viewport.renderer.mode
  let set_renderer value mode =
    {value with extra = {value.extra with Viewport.renderer_request = Some mode}}

  let create ?name ?presets ?factories ?settings ?lights ?camera
      ?background ?seed ?grain ?domains ?max_entries ?max_payload_bytes ?await ?carry_budget ~workspace ?source ~prepare ~scene3
      ?overlay ?status () =
    create ?name ?presets ?factories ?settings ?lights ?camera
      ?background ?seed ?grain ?domains ?max_entries ?max_payload_bytes ?await ?carry_budget ~workspace ?source ~prepare
      ~draw:scene3 ?overlay ?status ()

  let run ?inputs ?name ?factories ?lights ?camera
      ?seed ?grain ?max_entries ?max_payload_bytes ~config ~workspace ?source ~prepare
      ~scene3 ?overlay () =
    run ?inputs ?name ?factories ?lights ?camera
      ?seed ?grain ?max_entries ?max_payload_bytes ~config ~workspace ?source ~prepare
      ~draw:scene3 ?overlay ()
end

module Reduce = struct
  let open_import (editor : _ Editor.t) file =
    {editor with Environment.core = {editor.core with Core.open_import = Some file}}
  let spreadsheet (editor : _ Editor.t) path =
    let key = Core.panel_key editor.core.doc path in
    let pane = Core.shown_as editor.core (key, path, Pxui_shell.Layout.Spreadsheet) in
    (Core.local_of editor.core key).sheet_owner, Core.spreadsheet_source pane
  let select_path (editor : _ Editor.t) path = match path with
    | graph :: _ -> {editor with Environment.core = { (Core.go editor.core graph) with Core.select_later = [path] }}
    | [] -> editor
  let view (editor : _ Editor.t) path =
    {editor with Environment.core = Core.view_node editor.core path}
  let step (e : _ Editor.t) ?select ?preview actions frame =
    { e with Environment.core = Core.reduce_idle ?select ?preview e.Environment.core actions frame }
end

module Private = struct
  module Spreadsheet = Spreadsheet
  module Render_budget = struct let film = Renderer.film let next_turn = Renderer.next_turn end
  module Leader = Leader module Schedule = Schedule
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

  let load ?ops ?imports ?factories text =
    match workspace_catalog ?factories () with
    | Error d -> Error [ d ]
    | Ok catalog -> Workspace_doc.of_text ?ops ?imports catalog text

  (* A hand-written host: the checked document and its file, or the diagnostics and exit 1. *)
  let open_text ?factories ~path ~digest text =
    let source = Source.find ~path ~digest in
    let imports = match source with
      | Some source -> Source_file.read_imports ~file:(Source.file source) text
      | None -> Ok [] in
    match Result.bind imports (fun imports -> load ?factories ~imports text) with
    | Error ds ->
        List.iter (fun d -> prerr_endline (Flow.Diagnostic.report ~file:path ~source:text d)) ds;
        exit 1
    | Ok doc -> doc, Option.map (fun source -> Source_file.with_imports source (Workspace_doc.import_texts doc)) source

  (* The viewport starts where the scene's first camera is (else the default orbit); the host's
     light below is a default of a workspace with no scene graph (Contexts.of_workspace).
     Piece preparation is shared with the hand-written shattered-cube host. *)
  (* Every [sop] graph of the document, lowered and compiled, for headless cooking. *)
  let sop_graphs ?(factories = Sop_catalog.Editor.factories) (doc : Workspace_doc.t) =
    let ( let* ) = Result.bind in
    let* lowered = Result.map_error Flow.Diagnostic.to_string
        (Flow_sop.Lower.workspace ~extra:Contexts.descriptors ~inputs:doc.inputs ~factories doc.source) in
    List.fold_right (fun (g : Flow_sop.Lower.graph) rest ->
      let* rest = rest in
      match g.root with
      | None -> Ok rest
      | Some node_id ->
          let* graph = Sop.Edit_graph.compile_node g.network.geometry ~node_id in
          Ok ((g.name, graph) :: rest)) lowered.graphs (Ok [])

  let document_camera (document : Document.t) base =
    let scene = document.scene.graph.Flow_sop.Network.geometry in
    match Option.bind (List.nth_opt (Objects.ids "camera" scene) 0) (fun node_id ->
        Option.bind (Sop.Edit_graph.find scene ~node_id) Objects.Camera.of_node) with
    | Some (view, _) -> Rays.Easy_camera.of_view ~eye:(Rays.Camera.position view)
        ~target:(Rays.Camera.target view) base
    | None -> base

  let declared_camera ?(factories = Sop_catalog.Editor.factories) doc base =
    match Contexts.of_workspace ~factories doc with
    | Error _ -> base
    | Ok document -> document_camera document base

  (* One static evaluation a start: the document the editor opens is built here, and the window
     and the camera are read from it. A document that does not build leaves each reader the
     answer it had alone (the window's own evaluation, the base camera, the editor's error). An
     explicit empty factory list is the one case where the camera and the editor never read the
     same document, so it shares nothing. *)
  let run ?inputs ?factories ?source doc =
    let doc = match inputs with None -> doc | Some inputs -> {doc with Workspace_doc.inputs} in
    let source = Option.bind source (fun { path; digest } -> Source.find ~path ~digest)
      |> Option.map (fun source -> Source_file.with_imports source (Workspace_doc.import_texts doc)) in
    let opened = match factories with
      | Some [] -> None
      | _ -> Result.to_option (Contexts.of_workspace
          ~factories:(Option.value factories ~default:Sop_catalog.Editor.factories) doc) in
    Result.map (fun window ->
    let config = { Rays.Sketch.default_config with width = window.width; height = window.height;
                   title = window.title; fps = Some window.fps } in
    let prepare _ = Surface.of_output in
    let scene3 = Surface.scene3 in
    let lights = [ Rays.Light.directional ~direction:(Rays.Vec3.create (-1.) (-1.4) (-0.8))
                     ~diffuse:Rays.Color.white () ] in
    let base = Rays.Easy_camera.create ~target:Rays.Vec3.zero ~distance:3.6 ~azimuth:0.4 ~elevation:0.6 () in
    let camera = match opened, factories with
      | Some document, _ -> document_camera document base
      | None, Some [] -> declared_camera ~factories:[] doc base
      | None, _ -> base in
    Environment.run ?inputs ~config ~lights ~camera ?factories ~seed:(Int64.of_int window.seed) ?opened
      ~workspace:doc ?source ~prepare ~draw:scene3 ())
      (match opened with Some document -> Contexts.document_window document | None -> workspace_window doc)

  let export ?graph ?(fps = 60) ~directory ~frames doc =
    let factories = Sop_catalog.Editor.factories in
    let ( let* ) = Result.bind in
    let* () = if fps > 0 && frames > 0 then Ok () else
      Error (Flow.Diagnostic.error ~code:"E_EXPORT_RANGE" "Export frame count and fps must be positive.") in
    let* window = workspace_window doc in
    let* lowered = Flow_sop.Lower.of_checked ~reference:true ~factories ~inputs:doc.inputs doc.checked in
    let evaluated=lowered.evaluated in
    let* ()=Workspace_host.export_check doc evaluated.plan in
    let chosen = List.find_opt (fun (g : Flow.Workspace.graph) ->
      g.context = Flow.Context.draw && Option.fold ~none:true ~some:((=) g.name) graph) doc.checked.graphs in
    match chosen with
    | None -> Error (Flow.Diagnostic.error ~code:"E_DRAW_GRAPH" "Export needs a draw graph.")
    | Some graph ->
        let state = Flow.Eval.create_state () in
        let host=Workspace_host.create ~seed:(Int64.of_int window.seed)()in
        Workspace_images.bind host.images lowered;
        let value = List.assoc graph.name evaluated.results in
        let* prepared = Drawing.prepare ~states:evaluated.states evaluated.plan value in
        let view () frame =
          let live = {(Frame_input.at_time frame.Rays.Frame.time) with
            dt = 1. /. float fps; frame = frame.count; tick = frame.count; size = (window.width, window.height)} in
          match Drawing.render_prepared ~state prepared
            ~image:(Workspace_images.image ~display:false host.images ~state ~live evaluated.plan) ~live ~size:live.size with
          | Ok scene -> (match Workspace_host.export_update host ~state ~live doc evaluated.plan with
              |Ok()->scene|Error d->Flow.Value.raise_diagnostic d)
          | Error d -> Flow.Value.raise_diagnostic d in
        let config = {Rays.Sketch.default_config with width = window.width; height = window.height;
          title = window.title;  clock = Rays.Sketch.Fixed (1. /. float fps)} in
        let after_present () _=match Workspace_host.save_pending host Rays.Canvas.save_screen_png with
          |Ok()->()|Error d->Flow.Value.raise_diagnostic d in
        (try ignore (Rays.Sketch.export_state ~config ~fps ~directory ~frames
           ~init:(fun _ -> ()) ~update:(fun () _ -> ()) ~view ~after_present
           ~on_stop:(fun()->Workspace_host.close host) ()); Ok ()
         with Flow.Value.Fail (code, message, span) -> Error (Flow.Diagnostic.error ?span ~code message)
           | Flow.Value.Diagnostic_fail diagnostic -> Error diagnostic)

  let main ?imports ~path ~digest ~catalog text =
    let expected = Contexts.catalog_digest Sop_catalog.Editor.factories in
    if catalog <> expected then
      prerr_endline (path ^ ": built against another catalog; checking the source again");
    match load ?imports text with
    | Error ds ->
        List.iter (fun d -> prerr_endline (Flow.Diagnostic.report ~file:path ~source:text d)) ds;
        exit 1
    | Ok doc ->
        (match run ~source:{ path; digest } doc with
         | Ok () -> ()
         | Error d -> prerr_endline (Flow.Diagnostic.report ~file:path ~source:text d); exit 1)
end
