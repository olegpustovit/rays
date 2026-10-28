open Editor_model
open Editor_document
open Editor_shell
open Editor_panes
open Editor_list

(* ---- creation ---- *)

(* The document a workspace starts from: one geometry object per [sop] graph
   beside the sketch's own scene objects ([seed_scene]). [opened] is the caller's
   [Lowering.of_workspace ~factories workspace], when it already built it to read the window
   and the camera (Workspace.run): the workspace is evaluated once a start. *)
let workspace_doc ?gpu ?opened ~factories ~seed_scene workspace =
  let ( let* ) = Result.bind in
  let flow r = Result.map_error Diagnostic.to_string r in
  let* doc = match opened with
    | Some doc -> Ok doc
    | None -> flow (Editor_document.Lowering.of_workspace ~factories ?gpu workspace) in
  let declared = doc.scene in
  (* the host's camera and lights are defaults of a workspace with no scene graph; a scene
     graph is authoritative (it may say there are none) *)
  let has_scene = Editor_document.Lowering.graph_of workspace Flow.Context.scene <> None in
  let scene = if has_scene then declared else seed_scene declared in
  let doc = { doc with scene;
    active_camera = if has_scene then doc.active_camera else List.nth_opt (Objects.ids "camera" scene) 0 } in
  Ok doc

let preferences_file = function
  | Some path when path <> "" -> path
  | Some _ | None -> Filename.concat (Filename.concat
      (Option.value ~default:"." (Sys.getenv_opt "HOME")) ".rays") "preferences.rays"

let read_preferences filename =
  Editor_core.Store.Settings.load ~sketch:"rays-editor" filename

let save_guide filename guide =
  let ( let* ) = Result.bind in
  let* values = if Sys.file_exists filename then read_preferences filename else Ok [] in
  Editor_core.Store.Settings.save ~sketch:"rays-editor" filename
    (("guide", Editor_core.Store.Settings.Bool guide) :: List.remove_assoc "guide" values)

let state_directory value = Filename.concat value.presets "state"
let state_file value = Preset.path ~directory:(state_directory value) ~name:value.state_name
let browse value query =
  let last_state = match Unix.stat (state_file value) with
    | { Unix.st_kind = Unix.S_REG; st_mtime; _ } -> Some st_mtime
    | _ | exception Unix.Unix_error _ -> None in
  Some (Browsing { query; presets = Preset.list ~directory:value.presets; last_state })

let create ?gpu ?settings
    ?(seed_scene = fun scene -> scene)
    ?(name = "sketch") ?presets ?(state_key = name) ?(factories = [])
    ?(seed = 0L) ?(grain = 16_384)
    ?domains ?(max_entries = 512)
    ?(max_payload_bytes = 256 * 1024 * 1024) ?await ?(carry_budget = 0.5) ?(clock = Unix.gettimeofday)
    ?preferences ?(ui_text_size = default_text_size) ?ui_font ?opened ~workspace ~prepare () =
  let factories = if factories = [] then Sop.Nodes.factories else factories in
  let settings = Option.value settings ~default:workspace.Workspace_doc.settings in
  let gpu = Option.value ~default:Flow_ir.Gpu.default gpu in
  let opened = workspace_doc ~gpu ?opened ~factories ~seed_scene
      { workspace with Workspace_doc.settings } in
  Result.bind opened (fun doc ->
  Result.map (fun cook ->
      let workspace = { tree = Pxui_shell.Layout.default; hidden = [ Pxui_shell.Layout.Timeline ]; live = None; window_live = None;
                        restored = false } in
      let presets = match presets with
        | Some directory -> directory
        | None -> Filename.concat (Filename.concat
            (Option.value ~default:"." (Sys.getenv_opt "HOME")) ".rays")
            (Preset.sanitize name) in
      let level = Document.Scene in
      let preferences = preferences_file preferences in
      let guide = match read_preferences preferences with
        | Ok values -> Option.value ~default:true (Editor_core.Store.Settings.bool values "guide")
        | Error _ -> true in
      let state_name = Lowering.sha256 state_key in
      let value = { preferences; text_size = ui_text_size; guide; hud = None; presets; state_name; name; prompt = None; notice = None; notice_at = 0.;
        doc; gpu; filed = doc;
        panes = Panel_map.make { view = Pxui_graph.Scope.create (); laid = None; shown = None; route = [];
                                 views = Level_map.empty; at = level; picked = Selection.empty };
        text = Text_pane.initial; map_view = false;
        rows = []; live_cook = true;
        factories;
        menu = None;
        probes = Layout_by_path.Path_map.empty; lit = None;
        previews = Document.Int_map.empty; viewed = Document.Int_map.empty; canvas_preview = None; canvas_viewed = None; view_key = None;
        edit_phases = Flow.Phase_timer.empty; select_later = []; open_import = None;
        flow_catalog = lazy (Result.to_option (Editor_document.Kinds.catalog
          ~version:Flow_sop.Manifest.version factories));
        lisp_vocab = vocabulary factories doc;
        tree = Pxui_shell.Tree.create (); outline = Navigator.initial; held_keys = []; sketch_blocked = []; window_focus = 2;
        ui = (let ui = Pxui.Ui.create ?font_path:ui_font () in Pxui.Ui.set_font_size ui ui_text_size; ui); workspace;
        timeline = Timeline.create (); live_frame = Input.at_time 0.; cook;
        edit_error = None; status_fps = None;
        status_fps_at = Float.neg_infinity; last_dt = 0.; steady = 0;
        history = Editor_core.History.create doc;
        focus = Editor_core.Panels.main; focus_path = None; pane_keys = []; leader = Editor_commands.Idle;
        timeline_frames = 240; queued = [];
        list_at = None; text_at = None; outline_at = None; locals = [];
        started = { on = None; views = []; tabs = [] };
        framing = None; carry = None; carry_budget; clock; traces = []; view_tools = None; gates = []; selected_box = None;
        file = (doc.Document.workspace.ws).checked.name ^ ".rays" } in
      Cook.set_volatile cook (Flow_sop.Lower.is_volatile (doc.workspace.lowered));
      (* the panels open as their start keywords say; the first graph pane (the focused leaf, else
         the first) opens on the graph it pins *)
      let on = match Option.bind doc.Document.shell (fun s -> s.start.focus) with
        | Some path -> Some path | None -> None in
      let value = follow_graph value ~focus:(if on = None then value.focus else Pxui_shell.Layout.Graph) ~focus_path:on in
      let value = with_pane value (fun pane -> { pane with views = Option.fold ~none:Level_map.empty ~some:(start_views doc level) (graph_path value) }) in
      let value = follow_hosts value ~focus_path:on (panel_hosts value) in
      follow_start value)
    (Cook.create ~prepare ~seed ~grain ?domains ?await ~clock ~max_entries
      ~max_payload_bytes ()))
