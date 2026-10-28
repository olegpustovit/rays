open Rays
open Editor_document
open Sop
open World_map
open Environment_view

(* The one environment: [Core] plus the 3D viewport. *)
module V = Viewport
type layout = Pxui_shell.Layout.t

type 'prepared t = {
  core : 'prepared Core_model.t;
  camera : V.camera;  (* the orbit of the focused viewport *)
  cameras : (string * V.camera) list;  (* the orbits of the other viewports, by key *)
  viewing : string option;  (* the viewport [camera] belongs to *)
  control : V.control;
  draw : Graph.t -> 'prepared -> V.rendered;
  rendered : V.rendered option;
  views : (string * V.rendered) list;  (* viewports over another scene instance *)
  drawn : (Graph.t * 'prepared * V.rendered * Texture.t option) Document.Int_map.t;  (* per object *)
  composed : (Edit_graph.t * Document.level * (string * int list) list) option;
  resolved : (Document.t * Input.t * Edit_graph.t) option;
  context_error : string option;
  (* The scene, open level (ghosts), and viewport membership used by the picture. *)
  baked : World.baked option;
  baked_from : (World.t * World.baked) option;
  baked_views : (string * (World.t * World.baked)) list;  (* the World of each viewport over another scene instance *)
  map : (World.baked * Image.t) option;  (* the lat-long view's upload *)
  render_status : string option;
  pending_render : V.request option;
  background : Rays_math.Color.t;
  extra : V.extra;
  hidden_scene_cache : (V.rendered, V.view) hidden_scene_cache option;
  world_drag : world_drag option;
  pick_press : (float * float) option;  (* a left press in the view that may become a click *)
  source : Source_file.t option;  (* the .rays the document came from: polled, saved over *)
  held : string option;  (* the file's changed text, waiting while the document has unsaved work *)
  open_dialog : int option;  (* Command-O's system dialog, until it answers *)
  refused : string option;  (* the file's text that did not check: the Document tab shows it *)
  opened : Document.t;  (* the document this session started from *)
  state_owned : bool;  (* this session wrote the recovery file *)
  state_checked : float;
  saved_doc : Document.t;
  saved_view : Flow.Syntax.t;
  state_error : string option;
  canvases : (string * canvas_picture) list;
  host : Flow_native.Workspace_host.t;
}

(* Scene objects a sketch starts with: its lights as light objects. *)
let seed_lights lights scene =
  List.fold_left (fun (scene, index) light ->
    let added = Result.bind (Edit_graph.instantiate_optional Objects.Light.factory [None])
        (fun node ->
          let node = Node.relabel (Printf.sprintf "light%d" index) node in
          Result.bind (Edit_graph.add_node ~factory:Objects.Light.factory
              ~inputs:[|None|] node scene) (fun scene ->
            Result.map fst (Edit_graph.apply_parameters scene ~node_id:(Node.id node)
              (Objects.light_values light)))) in
    (match added with Ok scene -> scene | Error _ -> scene), index + 1)
    (scene, 1) lights |> fst

let create ?inputs ?name ?presets ?factories ?gpu_owner
    ?settings ?(lights = [])
    ?(camera = V.default_camera ()) ?(background = Rays_math.Color.hex_exn "#f4f5f0")
    ?seed ?grain ?domains ?max_entries ?max_payload_bytes ?await ?carry_budget ?preferences ?ui_text_size ?ui_font ?opened ~workspace ?source ~prepare ~draw
    () =
  let workspace = match inputs with None -> workspace
    | Some inputs -> {workspace with Workspace_doc.inputs} in
  let source = Option.map (fun source -> Source_file.with_imports source
      (Workspace_doc.import_texts workspace)) source in
  let name = Option.value name ~default:(Workspace_doc.name workspace) in
  let state_key = match source with
    | None -> "workspace:" ^ Workspace_doc.name workspace
    | Some source ->
        let file = Source_file.file source in
        let file = try Unix.realpath file with Unix.Unix_error _ ->
          if Filename.is_relative file then Filename.concat (Sys.getcwd ()) file else file in
        "file:" ^ file in
  let host=Flow_native.Workspace_host.create ?seed ?grain ?domains ?gpu:gpu_owner () in
  let gpu=Flow_native.Workspace_gpu.config host.gpu in
  let created=Core_setup.create ~gpu ?settings
      ~seed_scene:(fun factories scene ->
        V.seed_scene camera factories (seed_lights lights scene))
      ~name ~state_key ?presets ?factories ?seed ?grain ?domains
      ?max_entries ?max_payload_bytes ?await ?carry_budget ?preferences ?ui_text_size ?ui_font ?opened ~workspace ~prepare () in
  match created with
  | Error error -> Flow_native.Workspace_host.close host; Error error
  | Ok core -> Ok (
    let core, extra = V.init core camera in
    let core = match source with
      | Some source -> { core with Core_model.file = Filename.basename (Source_file.file source) }
      | None -> core in
    { core; camera; control = V.create_control (); draw;
      rendered = None; views = []; drawn = Document.Int_map.empty; composed = None;
      resolved = None; context_error = None; canvases = []; host; baked = None; baked_from = None; baked_views = []; map = None;
      render_status = None; pending_render = None;
      background; extra; hidden_scene_cache = None; world_drag = None; pick_press = None; source; held = None; open_dialog = None; refused = None; opened = core.doc; state_owned = false; cameras = []; viewing = None;
      state_checked = neg_infinity; saved_doc = core.doc; saved_view = V.section camera extra; state_error = None })

let document value = Core_inspect.document value.core
let prepared value = Core_list.prepared value.core
let camera value = value.camera
let extra value = value.extra
let timeline value = Core_list.timeline value.core
let selected_node value = Core_list.selected_node value.core
let displayed_node value = Core_list.displayed_node value.core
let panes value frame = Core_shell.panes value.core frame
let can_undo value = Editor_core.History.can_undo value.core.Core_model.history
let can_redo value = Editor_core.History.can_redo value.core.Core_model.history
let undo_label value = if can_undo value
  then Some (Editor_core.History.label value.core.Core_model.history) else None
let redo_label value = Editor_core.History.redo_label value.core.Core_model.history
let workspace value = value.core.Core_model.doc.workspace.ws
let probe value zone = Layout_by_path.Path_map.find_opt zone value.core.Core_model.probes
let set_probe value zone index =
  { value with core = { value.core with Core_model.probes = Layout_by_path.Path_map.add zone index value.core.Core_model.probes } }
let node_box value path =
  Option.map (fun (x, y, w, h) -> int_of_float x, int_of_float y, int_of_float w, int_of_float h)
    (Pxui_graph.Scope.Private.box_of value.core.Core_model.scope_view path)
let edit value op = Result.map (fun core -> { value with core }) (Core_host.syntax_edit value.core op)

let edit_phases value = value.core.Core_model.edit_phases
let carrying value = Option.map (fun (c : _ Core_model.carry) -> c.payload.Carry.kind, c.payload.value) value.core.Core_model.carry
let carry_line value = Option.map Core_status.carry_line value.core.Core_model.carry
let level value = match value.core.Core_model.level with
  | Document.Scene -> None
  | Inside id -> Some (Option.fold ~none:"" ~some:Node.label
      (Edit_graph.find (Core_inspect.scene value.core) ~node_id:id))
let scene_document value = Core_inspect.scene value.core
let same_context (a : Document.t) (b : Document.t) =
  a.scene.graph.geometry == b.scene.graph.geometry && a.scene_drives == b.scene_drives
    && a.scene_textures == b.scene_textures
let resolved_scene value = match value.resolved with
  | Some (doc, _, scene) when same_context doc value.core.Core_model.doc -> scene
  | _ -> Core_inspect.scene value.core
let lights value = Objects.lights ~render:true (resolved_scene value)
let world value = value.baked
let objects value = List.map (fun (matrix, (piece : _ Cook.piece)) ->
    matrix, piece.prepared) (Core_host.placed_pieces ~render:true value.core)

let settings value = Core_list.settings value.core
let set_settings value settings =
  { value with core = Core_host.set_settings value.core settings }

(* the viewport the focus is in; the default layout's one before any focus *)
let focus_key value = Option.value value.viewing ~default:"main"

(* the viewport whose root a view is read from: the hidden scene shows the focused viewport *)
let look_key value key = if key = "@hidden" then focus_key value else key

(* the view of the viewport [key]: its own orbit, the focused one's being [camera], through
   the camera of its own root *)
let camera_of value key =
  let key = look_key value key in
  let orbit = if Some key = value.viewing then value.camera
    else Option.value (List.assoc_opt key value.cameras) ~default:value.camera in
  V.view_camera orbit value.extra ~key ~pending:(value.pending_render <> None)

let view_camera value = camera_of value (focus_key value)

(* Focus moves to another viewport: the orbit of the one left is kept, the new one starts from
   the orbit it last had, else from a copy of the one in use. *)
let follow_focus value frame =
  let core = value.core in
  let g = Core_shell.geometry core core.Core_model.workspace frame in
  let keys = List.filter_map (fun (leaf : Pxui_shell.Layout.leaf) -> match leaf.panel with
    | View key -> Some key | _ -> None) g.leaves in
  let value = { value with cameras = List.filter (fun (key, _) -> List.mem key keys) value.cameras } in
  match Core_shell.active_view core g with
  | Some { panel = View key; _ } when Some key <> value.viewing ->
      (match value.viewing with
       | None ->
           (* the first focus: every other viewport starts as a copy of the orbit in use *)
           let others = List.filter_map (fun (leaf : Pxui_shell.Layout.leaf) -> match leaf.panel with
             | View k when k <> key -> Some (k, value.camera) | _ -> None) g.leaves in
           { value with viewing = Some key; cameras = others }
       | Some previous ->
           let kept = if List.mem previous keys
             then (previous, value.camera) :: List.remove_assoc previous value.cameras
             else value.cameras in
           { value with viewing = Some key;
             camera = Option.value (List.assoc_opt key kept) ~default:value.camera;
             cameras = List.remove_assoc key kept })
  | _ -> value

let paint_view value ~key viewport view rendered =
  V.paint value.extra ~key (V.film value.extra ~key:(look_key value key) viewport) view rendered

let viewport_camera = camera_of

let focused_image value = match value.viewing with
  | Some key -> (match List.assoc_opt key value.views with Some image -> Some image | None -> value.rendered)
  | None -> value.rendered

let film value frame =
  let (x, y, _, _) as pane = (Core_shell.panes value.core frame).view in
  let fx, fy, w, h = V.film value.extra ~key:(focus_key value) pane in
  fx - x, fy - y, w, h

(* The hidden-scene cache is model state: refreshed here, read by [scene]. *)
let refresh_hidden value frame =
  let hidden_scene_cache =
    if hidden_only ~ui_visible:(V.ui_visible value.control) value.core then
      Some (hidden_entry ~background:value.background ~rendered:(focused_image value)
        ~camera:(view_camera value) ~paint_view:(paint_view value)
        ~cache:value.hidden_scene_cache value.core frame)
    else None in
  { value with hidden_scene_cache }

(* Recompose when a cook, the scene, or the World changed. Each object's
   drawing is reused while its graph and prepared value are physically the same, so
   moving an object only re-places it. *)
let composition_key core scene = scene, core.Core_model.level,
  (match core.Core_model.doc.Document.shell with Some shell -> shell.views | None -> [])

let compose value (update : (_, _) Core_model.update) ~baked ~baked_views ~scene =
  let live={update.core.live_frame with t=Timeline.time(Core_list.timeline update.core);
    frame=Int64.to_int(Timeline.frame(Core_list.timeline update.core))}in
  let lowered=update.core.doc.workspace.lowered in
  Flow_native.Workspace_images.bind value.host.images lowered;
  let plan=lowered.plan in
  let textures,errors=Document.Int_map.fold(fun id source (textures,errors)->
    match Flow_native.Workspace_images.texture value.host.images ~state:update.core.cook.state ~live plan source with
    |Ok texture->Document.Int_map.add id texture textures,errors
    |Error diagnostic->textures,Diagnostic.to_string diagnostic::errors)
    update.core.doc.scene_textures (Document.Int_map.empty,[])in
  let texture_changed=Document.Int_map.exists(fun id(_,_,_,previous)->
    not(Option.equal (==) previous (Document.Int_map.find_opt id textures)))value.drawn in
  let scene, level, views = composition_key update.core scene in
  let same = match value.composed with
    | Some (s, l, v) -> s == scene && l = level && v = views
    | None -> false in
  if not (texture_changed || update.prepared_changed || update.scene_changed || update.effects.view
      || update.effects.export || not same || baked != value.baked || baked_views != value.baked_views
      || value.rendered = None)
  then value.rendered, value.views, value.drawn,errors
  else
    let placed = Core_host.placed_pieces ~view:`All update.core in
    let drawn = List.fold_left (fun drawn (_, (piece : _ Cook.piece)) ->
        let texture=Document.Int_map.find_opt piece.id textures in
        match Document.Int_map.find_opt piece.id value.drawn with
        | Some (graph, prepared, rendered,previous) when graph == piece.graph && prepared == piece.prepared
            && Option.equal (==) previous texture ->
            Document.Int_map.add piece.id (graph, prepared, rendered,texture) drawn
        | Some _ | None ->
            let scene=value.draw piece.graph piece.prepared in
            let scene=Option.fold ~none:scene ~some:(fun texture->Scene.D3.Private.with_texture(Scene.D3.textured texture)scene)texture in
            Document.Int_map.add piece.id (piece.graph, piece.prepared, scene,texture) drawn)
        Document.Int_map.empty placed in
    let waiting = placed = [] && Core_list.geometry_objects update.core <> []
        && Core_list.pieces update.core = [] in
    let ghost id = match update.core.Core_model.level with
      | Document.Inside open_id -> Core_inspect.kind update.core open_id = Some "geometry"
          && id <> open_id
      | Scene -> false in
    let compose_pieces ?(world = baked) view pieces = V.compose ~scene:(Core_host.scene_for_view ~scene ~view update.core)
      ~world
      (List.map (fun (matrix, (piece : _ Cook.piece)) ->
         let _, _, rendered,_ = Document.Int_map.find piece.id drawn in
         matrix, ghost piece.id, rendered) pieces) in
    (* a viewport over another scene instance: its own objects, its own World.  Viewports over
       the very same instance with the same World show one picture: it is composed once, so
       the renderer sees one scene and can give them one tracer *)
    let instance_of key = Option.bind update.core.Core_model.doc.Document.shell (fun shell ->
      Option.map (fun (source : Document.preview_source) -> source.instance)
        (List.assoc_opt key shell.preview_sources)) in
    let views = if waiting then [] else List.rev_map (fun (key, scene, _, _) -> key, scene)
      (List.fold_left (fun composed key ->
        let world = Option.map snd (List.assoc_opt key baked_views) in
        let instance = instance_of key in
        let same_instance = function Some a -> Option.fold ~none:false ~some:(( == ) a) instance | None -> false in
        let same_world other = match world, other with
          | None, None -> true | Some a, Some b -> a == b | Some _, None | None, Some _ -> false in
        let twin = List.find_map (fun (_, scene, other, other_world) ->
          if same_instance other && same_world other_world then Some scene else None) composed in
        let scene = match twin with
          | Some scene -> scene
          | None ->
              let own = match update.core.Core_model.doc.Document.shell with
                | Some shell -> List.mem_assoc key shell.views | None -> false in
              let view = if own then `Only key else `Primary in
              compose_pieces ~world:world view (Core_host.placed_pieces ~view update.core) in
        (key, scene, instance, world) :: composed) [] (view_keys update.core)) in
    (if waiting then None else Some (compose_pieces `Primary (Core_host.placed_pieces update.core))),
    views, drawn,errors
