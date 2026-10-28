open Rays
open Editor_document
open Sop

module CC = struct
  module Ui = Pxui.Ui

  type visibility = {
    prefix : string;
    ui_visible : bool;
    filename : string;
    open_camera : bool option;
  }

  let make prefix =
    if prefix = "" then invalid_arg "Pxui camera control: prefix is empty";
    { prefix; ui_visible = true; filename = "_out/rays-render.png"; open_camera = None }

  let toggle_ui control = { control with ui_visible = not control.ui_visible }

  (* Show the UI and toggle the Camera section, or open it if the UI was hidden. *)
  let open_camera control =
    { control with ui_visible = true; open_camera = Some (not control.ui_visible) }

  let number ui key label ~range:(low, high) value =
    let row, x, y, w = Ui.inspector_row ui ~key ~label () in
    Ui.within ui row (fun () ->
      let label_hit = Ui.box ui ~flags:Ui.clickable ~at:(Ui.inspector_label_x ui, 2.)
          ~w:(Ui.Px (if y > 20. then w else Float.max 1. (x -. Ui.inspector_label_x ui -. 8.))) ~h:(Ui.Px 20.) "label-edit" in
      let edit = (Ui.signal ui label_hit).double_clicked in
      let text = Printf.sprintf "%.6g" value in
      let slide fraction = Printf.sprintf "%.6g"
        (low +. fraction *. (high -. low)) in
      let shown = text in
      let text, _ = Ui.value_field ui ~at:(x, y) ~w ~h:20.
          ~fraction:((value -. low) /. (high -. low)) ~slide ~edit
          ~valid:(fun text -> Option.fold ~none:false ~some:Float.is_finite
            (float_of_string_opt text)) (key ^ "-value") text in
      (* the text shows 6 digits: an unedited field leaves the value as it is *)
      if text = shown then value else
      match float_of_string_opt text with
      | Some number when Float.is_finite number -> number
      | _ -> value)

  let text ui key label value =
    let row, x, y, w = Ui.inspector_row ui ~key ~label () in
    Ui.within ui row (fun () ->
      fst (Ui.value_field ui ~at:(x, y) ~w ~h:20.
        ~left:true ~valid:(fun _ -> true) (key ^ "-value") value))

  let render_section control ui =
    let filename, save = Option.value ~default:(control.filename, false)
        (Ui.inspector_section ui ~key:"Render" "Render" (fun () ->
          let filename = text ui "Output" "Output" control.filename in
          filename, Ui.inspector_button ui ~key:"Save" "Render / save PNG")) in
    { control with filename },
    if save then [filename] else []

  (* [open_camera] opens the section explicitly while hidden, otherwise it
     toggles. *)
  let camera_section control ui build =
    let set_expanded = match control.open_camera with
      | None -> None
      | Some true -> Some true
      | Some false -> Some (not (Option.value ~default:false (Ui.expanded ui "Camera"))) in
    Ui.inspector_section ui ~key:"Camera" ?set_expanded "Camera" build

  type t = visibility
  type render_request = { filename : string }

  let create () = make "camera"

  let widgets control ui ~camera =
    Ui.scope ui control.prefix (fun () ->
      let camera = Option.value ~default:camera (camera_section control ui (fun () ->
        let degrees = Easy_camera.fov_y camera *. 180. /. Float.pi in
        let edited = number ui "FOV" "FOV" ~range:(15., 120.) degrees in
        let fov = if edited = degrees then Easy_camera.fov_y camera
          else edited *. Float.pi /. 180. in
        let distance = number ui "Distance" "Distance" ~range:(0.1, 100.)
            (Easy_camera.distance camera) in
        let near = number ui "Near clip" "Near clip" ~range:(0.01, 10.) (Easy_camera.near camera) in
        let far = number ui "Far clip" "Far clip" ~range:(10., 5000.) (Easy_camera.far camera) in
        let inertia = Ui.inspector_toggle ui ~key:"Inertia" ~label:"Inertia"
            (Easy_camera.inertia camera) in
        let reset = Ui.inspector_button ui ~key:"Reset camera" "Reset camera" in
        let current = Easy_camera.fov_y camera in
        let fov = if fov > 0. && fov < Float.pi then fov else current in
        let distance = if distance > 0. then distance else Easy_camera.distance camera in
        let near = if near > 0. then near else Easy_camera.near camera in
        let far = Float.max (near +. 0.01) far in
        let camera = camera
          |> Easy_camera.with_fov_y fov |> Easy_camera.with_distance distance
          |> Easy_camera.with_clip ~near ~far |> Easy_camera.with_inertia inertia in
        if reset then Easy_camera.reset camera else camera)) in
      let control, saves = render_section { control with open_camera = None } ui in
      control, camera, List.map (fun filename -> { filename }) saves)

  let navigate ?control_area camera (frame : Frame.t) =
    let area = Option.value control_area ~default:(0, 0, (fst frame.input.size), (snd frame.input.size)) in
    camera
    |> Easy_camera.with_control_area (Some area)
    |> Fun.flip Easy_camera.update frame

  let ui_visible (control : t) = control.ui_visible
  let save request = Canvas.save_screen_png request.filename
end

type camera = Easy_camera.t
type control = CC.t
type rendered = Scene.D3.t
type request = CC.render_request
type view = Camera.t

(* The view's handle tool: W translate, E rotate, R scale, Escape none. *)
type tool = Move | Turn | Grow | No_tool

(* What the view draws over the render while the UI shows. *)
type show = { cameras : bool; axes : bool; handles : bool }

(* Look-through, the fly speed while the pointer is captured, the
   active camera node's view and the free viewport's view with its lens
   (both refreshed each update, the latter physically stable while
   unchanged so hidden-scene caching holds), and the guides. *)
type extra = { looking : string list;  (* the viewports that look through their render camera, by key *)
               key : string;  (* the focused viewport: the one the toggle and the navigation mean *)
               started : string list;  (* the [:look_through true] viewports last followed *)
               fly : float option; relative_grab : bool;
               document_camera : Camera.t;  (* the ACTIVE camera's view: a viewport with no root of its own *)
               view_roots : (string * Document.view_root) list;  (* the viewports with a root of their own *)
               free_view : Camera.t; viewing : Easy_camera.t;  (* the orbit camera [free_view] shows *)
               free_camera : Camera.t;  (* the camera whose lens [free_view] carries *)
               root : Objects.Root.parameters;  (* the document's root settings, the renderer as shown *)
               root_request : Objects.Root.parameters option;  (* the Render section's edit *)
               following : bool option;  (* the ACTIVE camera follows the viewport *)
               follow_request : bool option;  (* the Viewport panel's toggle, applied on the next update *)
               written : Camera.t option;  (* the view the viewport last wrote to the camera node *)
               show : show; tool : tool; renderer : Renderer.state;
               renderer_request : Renderer.t option }

let default_camera () = Easy_camera.create ~target:Rays_math.Vec3.zero ~distance:7. ()
let create_control () = CC.create ()
let ui_visible = CC.ui_visible
let toggle_ui = CC.toggle_ui
let open_camera = CC.open_camera
let set_relative enabled = ignore (Sketch.set_relative_mouse enabled)

(* The one way out of a pointer grab: the orbit drag's relative mouse and the fly mode end
   together, on every path that stops navigating. *)
let release extra =
  if extra.relative_grab || extra.fly <> None then begin
    set_relative false; { extra with relative_grab = false; fly = None }
  end else extra

(* ---- camera objects: scene nodes with operation "camera" (Objects.Camera). *)

let camera_ids scene = Objects.ids "camera" scene

let follows node = match Objects.Camera.of_node node with
  | Some (_, follow_viewport) -> follow_viewport | None -> false

let node_camera node = Option.map fst (Objects.Camera.of_node node)

let view_parameters easy =
  let camera = Easy_camera.camera easy in
  let eye = Camera.position camera and target = Camera.target camera in
  Objects.Camera.to_values ~eye ~target ~fov_y:(Easy_camera.fov_y easy)

let same_view a b =
  Rays_math.Vec3.nearly_equal (Camera.position a) (Camera.position b) ~eps:1e-6
  && Rays_math.Vec3.nearly_equal (Camera.target a) (Camera.target b) ~eps:1e-6
  && (match Camera.projection a, Camera.projection b with
    | Perspective a, Perspective b -> Float.abs (a.fov_y -. b.fov_y) < 1e-6
    | _ -> false)

let active_node (core : _ Core_model.t) = Option.bind core.doc.active_camera
    (Kind_graph.find (Core_inspect.scene core))

(* A default camera, following the viewport. *)
let add_default_camera scene easy =
  Result.to_option (Result.map (fun (node, _) -> Kind_graph.add scene node)
    (Kind_graph.apply (Kind_graph.instance ~label:"camera1" Objects.Camera.kind)
      (("follow_viewport", Param.Bool_value true)
       :: Objects.Camera.lens_values Camera.pinhole @ view_parameters easy)))

let seed_scene easy scene =
  if camera_ids scene <> [] then scene
  else Option.value ~default:scene (add_default_camera scene easy)

(* One ACTIVE camera whenever any exists; a nonempty scene of a workspace with no scene graph
   losing the last camera re-adds the default within the same undo entry. An intentionally
   empty scene stays empty. *)
let sync_cameras ~mode (core : _ Core_model.t) easy =
  let core = if camera_ids (Core_inspect.scene core) <> []
      || Core_inspect.scene core = []
      (* a scene graph is authoritative: no camera object is a camera it does not declare *)
      || Lowering.graph_of (core.doc.workspace.ws) Flow.Context.scene <> None then core
    else match add_default_camera (Core_inspect.scene core) easy with
      | Some scene -> Core_host.scene_edit core mode scene
      | None -> core in
  let ids = List.filter (Core_inspect.view_wants core `Primary) (camera_ids (Core_inspect.scene core)) in
  let active = match core.doc.active_camera with
    | Some id when List.mem id ids -> Some id
    | Some _ | None -> (match ids with id :: _ -> Some id | [] -> None) in
  if active = core.doc.active_camera then core
  else Core_host.scene_edit core `Amend ~active_camera:active (Core_inspect.scene core)

(* How the viewport [key] renders: through which camera and as which root says.  A viewport over
   its own scene instance reads that instance's root; any other renders as the document. *)
type look = { through : Camera.t; settings : Objects.Root.parameters }

let look extra key = match List.assoc_opt key extra.view_roots with
  | Some { Document.params; camera } ->
      { through = Option.value camera ~default:extra.document_camera; settings = params }
  | None -> { through = extra.document_camera; settings = extra.root }

let render_camera_of core easy = match active_node core with
  | Some node -> Option.value (node_camera node) ~default:(Easy_camera.camera easy)
  | None -> Easy_camera.camera easy

(* The viewport camera carrying the ACTIVE camera's lens, focused on the
   orbit target unless that camera follows the viewport. *)
let free_view_of ~render_camera ~previous camera =
  let view = Easy_camera.camera camera in
  let lens = Camera.lens render_camera in
  let view = Camera.with_lens (if same_view render_camera view then lens
                               else { lens with focus_distance = None }) view in
  match previous with Some previous when previous = view -> previous | _ -> view

(* the renderer the document's root names, when its text has a root *)
let of_root = function
  | Objects.Root.Raster -> Renderer.Raster | Wireframe -> Renderer.Wireframe
  | Path_traced -> Renderer.Path_traced
let to_root = function
  | Renderer.Raster -> Objects.Root.Raster | Wireframe -> Wireframe | Path_traced -> Path_traced
let root_renderer core =
  if core.Core_model.doc.homes.root = None then None else Some (of_root core.doc.root.renderer)

let renderer_setting core = List.find_map (fun (field : Param.field_view) ->
  match field.name, field.current with
  | "renderer", Param.Choice_value label -> Renderer.of_label label
  | _ -> None) (Settings.fields (Core_list.settings core))

let looks extra key = List.mem key extra.looking
let look_through extra = looks extra extra.key
let set_look extra on =
  { extra with looking = (if on then [ extra.key ] else []) @ List.filter (( <> ) extra.key) extra.looking }

let init core camera =
  let core = sync_cameras ~mode:`Reset core camera in
  let render_camera = render_camera_of core camera in
  (* the viewports whose [:look_through true] says so; the focus starts in the first viewport *)
  let looking = Option.fold ~none:[] ~some:(fun (s : Document.shell) -> s.start.looking) core.Core_model.doc.shell in
  let key = match core.Core_model.focus, Option.map (fun (s : Document.shell) -> Editor_core.Panels.leaves s.tree) core.doc.shell with
    | View key, _ -> key
    | _, Some leaves -> Option.value ~default:"main"
        (List.find_map (function _, Editor_core.Panels.View key -> Some key | _ -> None) leaves)
    | _, None -> "main" in
  core, { looking; key; started = looking; fly = None; relative_grab = false; document_camera = render_camera;
          view_roots = core.doc.view_roots;
          free_view = free_view_of ~render_camera ~previous:None camera; viewing = camera;
          free_camera = render_camera; root = core.doc.root; root_request = None;
          following = Option.map follows (active_node core);
          follow_request = None; written = None;
          show = { cameras = true; axes = true; handles = true }; tool = Move;
          renderer = {Renderer.empty with mode = Option.value
            (match root_renderer core with Some mode -> Some mode | None -> renderer_setting core)
            ~default:Renderer.Raster;
            custom = root_renderer core = None && renderer_setting core <> None}; renderer_request = None }

let begin_frame extra (frame : Frame.t) = match extra.fly with
  | None ->
      let lost = List.exists (function Input.Focus_lost -> true | _ -> false) frame.input.events in
      (if lost then release extra else extra), frame
  | Some _ ->
      let ended, frame = Editor_core.Router.fly frame in
      (if ended then release extra else extra), frame

let panel ui ~control ~camera ~extra ~inspector =
  let extra = Option.value ~default:extra
      (Pxui.Ui.inspector_section ui ~key:"Viewport" ~expanded:true "Viewport" (fun () ->
        (* the label column is 0.3 of the panel less 16: the rows say it short, their keys stay *)
        let short = function
          | "Look through render camera" -> "Look through" | "Camera follows viewport" -> "Follows view"
          | "Cameras and lights" -> "Cameras" | "Selected node handles" -> "Handles" | label -> label in
        let toggle label value = Pxui.Ui.inspector_toggle ui ~key:label ~label:(short label) value in
        let through = toggle "Look through render camera" (look_through extra) in
        (* The ACTIVE camera's own parameter, here so a look-through can be
           set up by orbiting: on, the viewport drives the camera. *)
        let follow_request = match extra.following with
          | Some following ->
              let wanted = toggle "Camera follows viewport" following in
              if wanted <> following then Some wanted else None
          | None -> None in
        let cameras = toggle "Cameras and lights" extra.show.cameras in
        let axes = toggle "Axis gizmo" extra.show.axes in
        let handles = toggle "Selected node handles" extra.show.handles in
        { (set_look extra through) with follow_request; show = { cameras; axes; handles } })) in
  let control, camera, requests = CC.widgets control ui ~camera in
  (* the scene's root: renderer, size and samples *)
  let root = Option.value ~default:extra.root
    (Pxui.Ui.inspector_section ui ~key:"renderer-section" ~expanded:true "Scene root" (fun () ->
      match Pxui_shell.Inspector.record ui Objects.Root.parameters_schema extra.root with
      | Ok (root, _) -> root | Error _ -> extra.root)) in
  let extra = if root = extra.root then extra else {extra with root_request = Some root} in
  control, camera, requests, extra, inspector ui

let section camera extra =
  let form = Editor_core.Store.Viewport.encode camera ~look_through:(look_through extra) in
  match form.Flow.Syntax.node with
  | Map fields -> {form with node = Map (fields @
      [{form with node = Kw "renderer"}; {form with node = Str (Renderer.label
        (Option.value ~default:extra.renderer.mode extra.renderer_request))}])}
  | _ -> form
let restore camera extra json =
  let camera, look_through = Editor_core.Store.Viewport.decode camera json in
  let rec find = function
    | {Flow.Syntax.node = Kw "renderer"; _} :: {node = Str label; _} :: _ -> Renderer.of_label label
    | _ :: _ :: rest -> find rest | _ -> None in
  let mode = match json.Flow.Syntax.node with Map fields -> find fields | _ -> None in
  camera, { (set_look extra look_through) with renderer_request = mode }

let apply_action camera extra = function
  | Editor_commands.Tool 0 -> { extra with tool = No_tool }, Some "Orbit (handles hidden)"
  | Editor_commands.Tool 1 -> { extra with tool = Move }, Some "Translate (W)"
  | Tool 2 -> { extra with tool = Turn }, Some "Rotate (E)"
  | Tool 3 -> { extra with tool = Grow }, Some "Scale (R)"
  | Editor_commands.Look_through -> set_look extra (not (look_through extra)), None
  | Editor_commands.Look_through_camera -> set_look extra true, None
  (* the header's tabs: the same request as the Scene root section's renderer row *)
  | Editor_commands.Render_mode index ->
      { extra with root_request = Some { extra.root with renderer = List.nth [ Objects.Root.Raster; Wireframe; Path_traced ] (max 0 (min 2 index)) } }, None
  | Fly when extra.fly = None ->
      set_relative true;
      { extra with fly = Some (Float.max 0.5 (Easy_camera.distance camera *. 0.5)) },
      Some "Flying: WASD/QE move, Shift x4, wheel speed, Esc exits"
  | _ -> extra, None

let on_doc ~previous core camera =
  if Core_inspect.scene core == Core_inspect.scene previous
      && core.Core_model.doc.active_camera = previous.Core_model.doc.active_camera
  then core else sync_cameras ~mode:`Amend core camera

let navigate ~area control camera extra core ~(raw_frame : Frame.t) ~(input : Frame.t) =
  let active = active_node core in
  (* A fixed render camera owns the view while look-through is enabled. *)
  if look_through extra && active <> None
      && not (Option.fold ~none:false ~some:follows active) then camera, release extra
  else match extra.fly with
    | Some speed ->
        let camera, speed = Easy_camera.fly ~speed camera
          { raw_frame with input = input.input; mouse_delta = input.mouse_delta } in
        camera, { extra with fly = Some speed }
    | None ->
        let (vx, vy, vw, vh) = area in
        let inside = Pxui.Ui.contains (float vx, float vy, float vw, float vh) in
        let pressed = List.exists (function
          | Input.Pointer_pressed (_, pt) -> inside pt
          | _ -> false) input.input.events in
        let released = List.exists (function
          | Input.Pointer_released _ | Input.Focus_lost -> true
          | _ -> false) input.input.events in
        let grabbing = if pressed then true else if released || input.input.buttons = [] then false else extra.relative_grab in
        if grabbing <> extra.relative_grab then set_relative grabbing;
        let camera = CC.navigate ~control_area:area camera input in
        camera, { extra with relative_grab = grabbing }

let frame_bounds ~viewport:_ ~min ~max camera = Easy_camera.frame_bounds ~min ~max camera

(* Follow-viewport motion writes the camera node as view state (no undo entry); node edits and
   undo pull the viewport back to the document.  On a frame that stepped the history ([stepped])
   nothing is written: the document just restored is the one to show. *)
let on_view core ~previous ~key camera extra ~stepped =
  (* a change in the Render section: an authored root takes it; without one only the renderer
     is the viewport's own choice, and any other setting writes the root *)
  let core, extra = match extra.root_request with
    | None -> core, extra
    | Some root when root_renderer core = None
        && { root with renderer = core.Core_model.doc.root.renderer } = core.doc.root ->
        core, { extra with root_request = None; renderer_request = Some (of_root root.renderer) }
    | Some root -> Core_host.set_root core root, { extra with root_request = None } in
  let core, mode = match extra.renderer_request, renderer_setting core with
    | _ when root_renderer core <> None -> core, Option.get (root_renderer core)
    | Some mode, Some current when mode = current -> core, mode
    | Some mode, Some _ ->
        (match Settings.apply (Core_list.settings core) ["renderer", Param.Choice_value (Renderer.label mode)] with
         | Ok (settings, _) -> Core_host.set_settings core settings, mode
         | Error _ -> core, extra.renderer.mode)
    | Some mode, None -> core, mode
    | None, Some mode -> core, mode
    | None, None -> core, extra.renderer.mode in
  let extra = {extra with renderer = {extra.renderer with mode;
      custom = root_renderer core = None && renderer_setting core <> None};
    renderer_request = None; root = { core.Core_model.doc.root with renderer = to_root mode } } in
  let core = match extra.follow_request, active_node core with
    | Some value, Some node ->
        Core_host.edit_node core Document.Scene node.Kind_graph.id
          ["follow_viewport", Param.Bool_value value] ~label:"Follow viewport"
    | _ -> core in
  let written = ref extra.written in
  let active = active_node core in
  let decoded = Option.bind active Objects.Camera.of_node in
  let core, camera = match active, decoded with
    | Some node, Some (node_view, true) ->
        let moved = not stepped && not (same_view (Easy_camera.camera previous)
            (Easy_camera.camera camera)) in
        if moved || not (same_view node_view (Easy_camera.camera camera)) then
          if moved then
            match Kind_graph.set_fields (Core_inspect.scene core) node.Kind_graph.id
                (view_parameters camera) with
            | Ok (scene, _) ->
                written := Some (Easy_camera.camera camera);
                Core_host.scene_edit core `Amend scene, camera
            | Error _ -> core, camera
          else if (match !written with Some w -> same_view w node_view | None -> false) then
            (* the node shows what a viewport last wrote: another viewport took over, and
               keeps its own orbit *)
            core, camera
          else begin
            written := Some node_view;
            core,
            (match Camera.projection node_view with
             | Perspective { fov_y; _ } -> Easy_camera.with_fov_y fov_y camera
             | _ -> camera)
            |> Easy_camera.of_view ~eye:(Camera.position node_view)
                 ~target:(Camera.target node_view)
          end
        else core, camera
    | _ -> core, camera in
  (* Reuse this frame's decode. A following viewport can write a new immutable
     node above; only that replacement needs another schema/camera conversion. *)
  let current = active_node core in
  let decoded = if Option.equal (==) current active then decoded
    else Option.bind current Objects.Camera.of_node in
  let extra = { extra with document_camera = Option.fold
      ~none:(Easy_camera.camera camera) ~some:fst decoded;
    view_roots = core.Core_model.doc.view_roots } in
  (* the focused viewport's free view carries the lens of its own camera *)
  let free_camera = (look extra key).through in
  (* a viewport whose [:look_through] changed in the text follows it; the others stay as they are *)
  let said = Option.fold ~none:[] ~some:(fun (s : Document.shell) -> s.start.looking) core.Core_model.doc.shell in
  let extra = if said = extra.started then extra else
    { extra with started = said;
      looking = List.filter (fun k -> List.mem k said || List.mem k extra.started = List.mem k said) extra.looking
                @ List.filter (fun k -> not (List.mem k extra.started) && not (List.mem k extra.looking)) said } in
  core, camera, { extra with key; free_camera;
    free_view = free_view_of ~render_camera:free_camera ~previous:(Some extra.free_view) camera;
    viewing = camera; following = Option.map
      (fun _ -> Option.fold ~none:false ~some:snd decoded) current;
    follow_request = None; written = !written }

(* The view shows the render camera while looking through it and on the
   frame whose framebuffer a PNG request captures; otherwise the viewport
   camera carries the ACTIVE camera's lens, focused on the orbit target
   unless the node follows the viewport. *)
let view_camera camera extra ~key ~pending =
  let { through; _ } = look extra key in
  if looks extra key || pending then through
  else if camera == extra.viewing && through == extra.free_camera then extra.free_view
  else free_view_of ~render_camera:through ~previous:None camera  (* another viewport's own orbit *)

(* Looking through the camera, the render fills the largest rect of the
   camera's aspect (its render resolution) centred in the pane; otherwise
   the whole pane. *)
let pick_ray ~viewport view at =
  Camera.screen_ray ~viewport view ~at

(* A viewport shows the root's gate, not the whole pane, only while looking through the render
   camera.  Its own free view fills the pane whatever draws it: Solid, Wire and Traced are the
   same picture of the same camera in the same rectangle, so switching one for another moves
   nothing and the guides, handles and brackets stay on what they mark. *)
let film extra ~key (x, y, width, height) =
  if not (looks extra key) then x, y, width, height else
  let { settings; _ } = look extra key in
  let aspect = float settings.width /. float (max 1 settings.height) in
  if width < 1000 && width > 160 && height > 160 then begin
    (* the workspace sheet's frame (640 x 360 in 902 x 485): a side margin of 14.6 percent of the
       body, 32 above, and clear of the gizmo (76 over the foot) and the readout row *)
    let side = int_of_float (Float.round (0.1463 *. float width)) in
    let top = 32 and bottom = 93 in
    let avail_w = width - (2 * side) and avail_h = max 1 (height - top - bottom) in
    let w = min avail_w (int_of_float (Float.round (float avail_h *. aspect))) in
    let h = min avail_h (int_of_float (Float.round (float avail_w /. aspect))) in
    let w = max 1 w and h = max 1 h in
    x + ((width - w) / 2), y + top, w, h
  end else
  (* viewport.html keeps the frame clear of the label row above it (40 points) and of the readouts'
     row under it (36), and of 12 points at each side *)
  let top = if height > 160 then 40 else 0 and bottom = if height > 160 then 36 else 0
  and side = if width > 160 then 12 else 0 in
  let x, y, width, height = x + side, y + top, width - (2 * side), height - top - bottom in
  let w = min width (int_of_float (Float.round (float height *. aspect))) in
  let h = min height (int_of_float (Float.round (float width /. aspect))) in
  let w = max 1 w and h = max 1 h in
  x + ((width - w) / 2), y + ((height - h) / 2), w, h

(* Each viewport renders as its own root says.  Looking through the render camera, the tracer's
   film is the root's resolution in a step (1, 1/2, 1/4, 1/8) that fits the viewport's gate in
   drawable pixels, so the film changes only when a resize crosses a step; the image is scaled to
   the gate when painted.  A free view is not the render: its film is the pane itself, pixel for
   pixel ({!Renderer.film}).  The sample cap is read here, each frame, and is not part of the
   renderer's setting: raising one continues the accumulation.  [views] carry each viewport's
   rectangle ({!film}). *)
let render extra ~pixel_scale:(scale_x, scale_y) ~focus views =
  let views = List.map (fun (key, (_, _, width, height), camera, scene) ->
    let { settings; _ } = look extra key in
    let gate = int_of_float (Float.round (float width *. scale_x)),
               int_of_float (Float.round (float height *. scale_y)) in
    let film, step = Renderer.film ~through:(looks extra key) ~resolution:(settings.width, settings.height) ~gate in
    { Renderer.key; film; step; camera; scene; cap = settings.max_spp;
      setting = { mode = of_root settings.renderer; bounces = settings.bounces;
                  round_samples = settings.round_samples } }) views in
  {extra with renderer = Renderer.update extra.renderer
    ~mode:extra.renderer.mode ~custom:extra.renderer.custom ~focus views}

(* A traced viewport's header: the root's resolution, the film's step of it and the samples *)
let header_tools extra =
  Some (look_through extra, match extra.renderer.mode with Renderer.Raster -> 0 | Wireframe -> 1 | Path_traced -> 2)
(* a traced viewport's film in pixels, samples, cap and bounces *)
let trace extra ~key =
  let { settings; _ } = look extra key in
  Option.map (fun (i : Renderer.info) -> i.size, i.samples, i.cap, settings.bounces)
    (Renderer.info extra.renderer ~key)
let render_status extra = extra.renderer.error
let paint extra ~key viewport camera rendered = Renderer.paint extra.renderer ~key viewport camera rendered

(* Objects under their world transforms. Light objects light the scene when
   there are any; otherwise the first object's own lights do. *)
let compose ~scene ~world pieces =
  let lights = Objects.lights scene in
  let base = match pieces with (_, _, first) :: _ -> first | [] -> Scene.D3.empty in
  let module P = Scene.D3.Private in
  (match world with Some baked -> Scene.D3.with_world baked | None -> Fun.id) @@
  Scene.D3.create ~lights:(if lights <> [] || Objects.ids "light" scene <> [] then lights
      else P.lights base)
    ~ambient:(P.ambient base)
    ~separate_specular:(P.separate_specular base) ~samples:(P.samples base)
    (List.map (fun (matrix, ghost, scene3) ->
      let placed = Scene.D3.transform matrix (Scene.D3.nodes scene3) in
      (* ponytail: ghosts screen-blend without writing depth; a real
         override material would need a Scene3 material-override node. *)
      if ghost then Scene.D3.with_depth (Scene.D3.depth_state ~write:false ())
          [Scene.D3.with_blend Scene.D3.Screen [placed]]
      else placed) pieces)

(* ---- guides and handles, in screen points over the view. *)

let axes = [| Rays_math.Vec3.create 1. 0. 0.; Rays_math.Vec3.create 0. 1. 0.; Rays_math.Vec3.create 0. 0. 1. |]
let axis_names = [| "x"; "y"; "z" |]
(* kit rev 3: the axes wear port colours (x bool, y int, z float), guides are ink-3 and the
   active camera and the lights the accent *)
let axis_colors = Array.map Rays_math.Color.hex_exn [| "#b0435f"; "#3b7d4e"; "#285f77" |]
let guide_color = Rays_math.Color.hex_exn "#8f9492"
let active_color = Rays_math.Color.hex_exn "#f0481f"

let project bounds camera point = Option.map (fun (screen : Rays_math.Vec3.t) ->
  screen.x, screen.y) (Camera.world_to_screen ~viewport:bounds camera point)

let screen_box view ~bounds ~world box =
  let corners = ref [] in
  Cook.iter_corners world box (fun point -> corners := project bounds view point :: !corners);
  let corners = !corners in
  if List.exists Option.is_none corners then None else
  let xs = List.filter_map (Option.map fst) corners and ys = List.filter_map (Option.map snd) corners in
  let min = List.fold_left Float.min infinity and max = List.fold_left Float.max neg_infinity in
  let x0 = Float.to_int (min xs) and y0 = Float.to_int (min ys) in
  Some (x0, y0, Float.to_int (max xs) - x0, Float.to_int (max ys) - y0)

let line ?(width = 1) color (x0, y0) (x1, y1) =
  Scene.line ~from_:(Float.to_int x0, Float.to_int y0)
    ~to_:(Float.to_int x1, Float.to_int y1) ~color ~width ()

(* a line of a width in whole or half points: Scene lines have whole widths only, so the stroke is a
   filled quad of the line's length *)
let thick_line width color (x0, y0) (x1, y1) =
  let dx = x1 -. x0 and dy = y1 -. y0 in
  let length = Float.hypot dx dy in
  if length < 1e-6 then Scene.rect ~at:(Float.to_int x0, Float.to_int y0) ~w:0 ~h:0 () else
  let nx = -. dy /. length *. width /. 2. and ny = dx /. length *. width /. 2. in
  Scene.path ~fill:color
    (Path.of_commands [|
      Path.Move_to { x = x0 +. nx; y = y0 +. ny };
      Path.Line_to { x = x1 +. nx; y = y1 +. ny };
      Path.Line_to { x = x1 -. nx; y = y1 -. ny };
      Path.Line_to { x = x0 -. nx; y = y0 -. ny };
      Path.Close;
    |])

let segment bounds view color a b =
  match project bounds view a, project bounds view b with
  | Some a, Some b -> [line color a b]
  | _ -> []

(* A pyramid half a unit deep with the view's aspect, and its aim. *)
let frustum bounds view color camera =
  let eye = Camera.position camera and target = Camera.target camera in
  let forward = Rays_math.Vec3.normalize (Rays_math.Vec3.sub target eye) in
  let side = Rays_math.Vec3.cross forward (Camera.up camera) in
  if Rays_math.Vec3.length side < 1e-6 then [] else
  let right = Rays_math.Vec3.normalize side in
  let up = Rays_math.Vec3.cross right forward in
  let fov_y = match Camera.projection camera with
    | Perspective { fov_y; _ } -> fov_y | _ -> 1. in
  let _, _, width, height = bounds in
  let half_h = 0.5 *. Float.tan (fov_y /. 2.) in
  let half_w = half_h *. float_of_int width /. float_of_int (max 1 height) in
  let centre = Rays_math.Vec3.add eye (Rays_math.Vec3.scale forward 0.5) in
  let corner sx sy = Rays_math.Vec3.add centre (Rays_math.Vec3.add (Rays_math.Vec3.scale right (sx *. half_w))
      (Rays_math.Vec3.scale up (sy *. half_h))) in
  let corners = [| corner (-1.) (-1.); corner 1. (-1.); corner 1. 1.; corner (-1.) 1. |] in
  let edge = segment bounds view color in
  List.concat (List.init 4 (fun index ->
    edge eye corners.(index) @ edge corners.(index) corners.((index + 1) mod 4))
    @ [edge (corner 0. 1.) (corner 0. 1.6); edge eye target])

(* Handles on the selected node's xyz parameter triples, found by name:
   W translates position-like triples, E rotates [rotate_*], R scales
   [scale_*]; Escape hides them so a drag only orbits. A camera's target
   also slides along the view direction: its focus distance. *)
let handle_prefixes = ["translate"; "center"; "origin"; "eye"; "target"]
let arrow_length = 60.

(* A handle: its box key, colour, tool, screen base and tip, [unit] (one
   world unit along its axis on screen, so a drag maps back to a delta),
   and the parameter edits a delta makes. A rotation turns its whole soft
   range per 360 points. *)
type arrow = { name : string; color : Rays_math.Color.t; tool : tool;
               base : float * float; tip : float * float; unit : float * float;
               edits : float -> (string * Param.value) list }

let arrows node view bounds space tool =
  let project point = project bounds view (Rays_math.Mat4.transform_point space point) in
  let values = node.Core_model.fields in
  let find name = List.find_map (fun (field : Param.field_view) ->
    match field.current with
    | Float_value value when field.name = name -> Some value
    | _ -> None) values in
  let range name = List.find_map (fun (field : Param.field_view) ->
    match field.kind with
    | Floating_view range when field.name = name -> Some (range.soft_max -. range.soft_min)
    | _ -> None) values in
  let triple prefix = match find (prefix ^ "_x"), find (prefix ^ "_y"), find (prefix ^ "_z") with
    | Some x, Some y, Some z -> Some (x, y, z) | _ -> None in
  let pivot = List.find_map triple handle_prefixes in
  let arrow ~name ~color at direction ~per_pixel edits =
    match project at, project (Rays_math.Vec3.add at direction) with
    | Some ((bx, by) as base), Some (ux, uy) ->
        let dx = ux -. bx and dy = uy -. by in
        let length = Float.hypot dx dy in
        if length < 1e-3 then None
        else Some { name; color; tool; base; unit = dx, dy;
                    edits = (fun delta -> edits (delta *. per_pixel));
                    tip = bx +. dx /. length *. arrow_length,
                          by +. dy /. length *. arrow_length }
    | _ -> None in
  let handles prefix (x, y, z) at =
    List.filter_map (fun axis ->
      let name = prefix ^ "_" ^ axis_names.(axis) and value = [| x; y; z |].(axis) in
      arrow ~name ~color:axis_colors.(axis) at axes.(axis)
        ~per_pixel:(if tool = Turn then Option.value ~default:360. (range name) /. 360. else 1.)
        (fun delta -> [name, Param.Float_value (value +. delta)])) [0; 1; 2] in
  let at = match pivot with Some (x, y, z) -> Rays_math.Vec3.create x y z | None -> Rays_math.Vec3.zero in
  match tool with
  | No_tool -> []
  | Move ->
      let distance = match triple "eye", triple "target" with
        | Some (ex, ey, ez), Some (tx, ty, tz) ->
            let target = Rays_math.Vec3.create tx ty tz in
            let direction = Rays_math.Vec3.sub target (Rays_math.Vec3.create ex ey ez) in
            if Rays_math.Vec3.length direction < 1e-6 then [] else
            let direction = Rays_math.Vec3.normalize direction in
            Option.to_list (arrow ~name:"target_distance" ~color:active_color target
              direction ~per_pixel:1. (fun delta ->
                let p = Rays_math.Vec3.add target (Rays_math.Vec3.scale direction delta) in
                ["target_x", Param.Float_value p.x; "target_y", Param.Float_value p.y;
                 "target_z", Param.Float_value p.z]))
        | _ -> [] in
      List.concat_map (fun prefix -> match triple prefix with
        | Some ((x, y, z) as values) -> handles prefix values (Rays_math.Vec3.create x y z)
        | None -> []) handle_prefixes @ distance
  | Turn -> Option.fold ~none:[] ~some:(fun values -> handles "rotate" values at) (triple "rotate")
  | Grow -> Option.fold ~none:[] ~some:(fun values -> handles "scale" values at) (triple "scale")

let pick_size = 16.

(* Handle drags edit the selected node; a click on a camera guide's eye
   selects that camera object. *)
let handles ui ~selected ~scene ~space view extra ~bounds =
  let edits, grab = match selected with
    | Some node when extra.show.handles ->
        List.fold_left (fun (edits, grab) arrow ->
          let tx, ty = arrow.tip in
          let box = Pxui.Ui.box ui ~flags:Pxui.Ui.clickable ~w:(Pxui.Ui.Px 14.)
              ~h:(Pxui.Ui.Px 14.) ~at:(tx -. 7., ty -. 7.) ("handle-" ^ arrow.name) in
          let signal = Pxui.Ui.signal ui box in
          let mx, my = signal.drag and ux, uy = arrow.unit in
          let delta = match arrow.tool with
            | Turn -> (mx *. ux +. my *. uy) /. Float.hypot ux uy
            | Move | Grow | No_tool -> (mx *. ux +. my *. uy) /. (ux *. ux +. uy *. uy) in
          (if signal.held && delta <> 0. then arrow.edits delta @ edits else edits),
          grab || signal.held || signal.released)
          ([], false) (arrows node view bounds space extra.tool)
    | Some _ | None -> [], false in
  let grab, picked = if not extra.show.cameras then grab, None else
    List.fold_left (fun (grab, picked) node_id ->
      match Option.bind (Kind_graph.find scene node_id) node_camera with
      | Some camera when not (same_view camera view) ->
          (match project bounds view (Camera.position camera) with
           | Some (x, y) ->
               let box = Pxui.Ui.box ui ~flags:Pxui.Ui.clickable ~w:(Pxui.Ui.Px pick_size)
                   ~h:(Pxui.Ui.Px pick_size) ~at:(x -. pick_size /. 2., y -. pick_size /. 2.)
                   ("pick-camera-" ^ string_of_int node_id) in
               let signal = Pxui.Ui.signal ui box in
               grab || signal.held, (if signal.clicked then Some node_id else picked)
           | None -> grab, picked)
      | Some _ | None -> grab, picked) (grab, None) (camera_ids scene) in
  edits, grab, picked

(* The axis gizmo (viewport.html): its origin 48 points from the pane's left and 44 from its foot,
   an axis is 40 points at full length in its port colour, and its letter a label at the tip (the
   sheet's offsets: y 6 right, x 3 right, z 8 left, each a few points below the tip). *)
let gizmo (x, y, width, height) view =
  (* a body about as wide as the workspace sheet's draws that sheet's gizmo: its origin 40 from the
     left and 36 from the foot, axes 36 long, no letters *)
  let compact = width < 1000 in
  let cx = float_of_int x +. (if compact then 40. else 48.)
  and cy = float_of_int (y + height) -. (if compact then 36. else 44.) in
  let length = if compact then 36. else 40. in
  let origin = Camera.world_to_camera view Rays_math.Vec3.zero in
  List.concat (List.init 3 (fun axis ->
    let d = Rays_math.Vec3.sub (Camera.world_to_camera view axes.(axis)) origin in
    let tip = cx +. d.x *. length, cy -. d.y *. length in
    let dx, dy = [| 3., 4.; 6., 4.; -8., 7. |].(axis) in
    thick_line 1.5 axis_colors.(axis) (cx, cy) tip
    :: (if compact then [] else
        [Scene.text ~at:(Float.to_int (fst tip +. dx), Float.to_int (snd tip +. dy) - 6) ~size:Pxui.Theme.label_size
           ~color:axis_colors.(axis) (String.uppercase_ascii axis_names.(axis))])))

(* A light object: a marker at its position, a line to its target. *)
let light_guide bounds view color (light : Light.t) =
  let marker (x, y) = [Scene.circle ~at:(Float.to_int x, Float.to_int y) ~radius:5
      ~fill:color ()] in
  (* a light whose marker is off the pane draws no stray line *)
  let inside (x, y) = let bx, by, bw, bh = bounds in x >= float bx && x <= float (bx + bw) && y >= float by && y <= float (by + bh) in
  let aim at direction = match project bounds view at with
    | Some point when inside point -> marker point @ segment bounds view color at
        (Rays_math.Vec3.add at (Rays_math.Vec3.scale direction 1.5))
    | _ -> [] in
  match light.kind with
  | Directional { direction } -> aim (Rays_math.Vec3.scale direction (-4.)) direction
  | Point { position; _ } ->
      (match project bounds view position with Some point -> marker point | None -> [])
  | Spot { position; direction; _ } -> aim position direction
  | Area { position; direction; width; height; _ } ->
      let side = Rays_math.Vec3.cross direction (Rays_math.Vec3.create 0. 1. 0.) in
      let side = if Rays_math.Vec3.length side < 1e-6 then Rays_math.Vec3.create 1. 0. 0. else Rays_math.Vec3.normalize side in
      let up = Rays_math.Vec3.normalize (Rays_math.Vec3.cross side direction) in
      let corner sx sy = Rays_math.Vec3.add position (Rays_math.Vec3.add (Rays_math.Vec3.scale side (sx *. width /. 2.))
          (Rays_math.Vec3.scale up (sy *. height /. 2.))) in
      let corners = [| corner (-1.) (-1.); corner 1. (-1.); corner 1. 1.; corner (-1.) 1. |] in
      aim position direction @ List.concat (List.init 4 (fun index ->
        segment bounds view color corners.(index) corners.((index + 1) mod 4)))

(* [bounds] is the rectangle the view is drawn in ({!film}): what is projected lies on the
   picture.  The axis gizmo belongs to the [pane]'s corner. *)
let guides ~scene ~selected ~space view extra ~pane ~bounds =
  let cameras = if not extra.show.cameras then [] else
    List.concat_map (fun node_id ->
      match Option.bind (Kind_graph.find scene node_id) node_camera with
      | Some camera when not (same_view camera view) ->
          let active = Camera.position camera = Camera.position extra.document_camera
            && Camera.target camera = Camera.target extra.document_camera in
          let color = if active then active_color else guide_color in
          let eye = match project bounds view (Camera.position camera) with
            | Some (x, y) -> [Scene.circle ~at:(Float.to_int x, Float.to_int y) ~radius:4 ~fill:color ()]
            | None -> [] in
          eye @ frustum bounds view color camera
      | Some _ | None -> []) (camera_ids scene) in
  let lights = if not extra.show.cameras then [] else
    List.concat_map (light_guide bounds view active_color) (Objects.lights scene) in
  let handles = match selected with
    | Some node when extra.show.handles ->
        List.concat_map (fun arrow ->
          let color = arrow.color in
          let at = Float.to_int (fst arrow.tip), Float.to_int (snd arrow.tip) in
          line ~width:2 color arrow.base arrow.tip :: (match arrow.tool with
            | Turn -> [Scene.circle ~at ~radius:7 ~stroke:color (); Scene.circle ~at ~radius:6 ~stroke:color ()]
            | Grow -> [Scene.rect ~at:(fst at - 5, snd at - 5) ~w:10 ~h:10 ~fill:color ()]
            | Move | No_tool -> [Scene.circle ~at ~radius:5 ~fill:color ()]))
          (arrows node view bounds space extra.tool)
    | Some _ | None -> [] in
  cameras @ lights @ (if extra.show.axes then gizmo pane view else []) @ handles

let save = CC.save
let filename request = request.CC.filename
let close extra = Renderer.close extra.renderer; ignore (release extra)
