(* Cross-representation checks: authored text, derived state, saved state and the picture. *)
open Rays
module D = Editor_document
module E = Rays_editor.Editor
module F = Flow_graph.Flow_edit
module S = Flow.Syntax

let check condition message = if not condition then failwith message
let factories = Sop_catalog.Editor.factories
let catalog = D.Contexts.catalog ~version:1 factories |> Result.get_ok
let parse text = match S.parse text with Ok [form] -> form | _ -> failwith text
let workspace ?settings text = D.Workspace_doc.of_text ?settings catalog text |> Result.get_ok
let build ?previous ws = D.Contexts.of_workspace ~factories ?previous ws |> Result.get_ok
let frame n = Test_editor_input.frame (450., 300.) [] n
let step e n = E.update e (frame n)
let create ?camera ?settings ?presets ?source ws = E.create ~ui_text_size:11 ?camera ?settings ?presets ?source ~await:true ~workspace:ws
  ~prepare:(fun _ output -> Rdk_rays.Rays_mesh.to_mesh (Result.get_ok (Sop.Payload.geometry output.Sop.Session.payload))
    |> Result.map_error Rdk.Error.to_string)
  ~draw:(fun _ mesh -> Scene3.create [Scene3.mesh mesh]) () |> Result.get_ok

let with_dir f =
  let dir = Filename.temp_dir "rays-consistency" "" in
  Fun.protect ~finally:(fun () -> Test_workspace_source.remove_tree dir) (fun () -> f dir)
let with_editor ?camera ws f = with_dir (fun presets ->
  let e = ref (create ?camera ~presets ws) in
  Fun.protect ~finally:(fun () -> E.close !e) (fun () -> e := step !e 1; f e))

let settings_schema = Param.(schema ~name:"consistency" ~default:(0., "default")
  [field ~name:"amount" ~label:"Amount" ~kind:(floating ~min:0. ~max:10. ()) ~default:0.
     ~get:fst ~set:(fun x (_, s) -> x, s) ();
   field ~name:"mode" ~label:"Mode" ~kind:Text ~default:"default"
     ~get:snd ~set:(fun s (x, _) -> x, s) ()])
let settings x = D.Settings.make settings_schema x
let plain = "(workspace consistency (graph g :context sop (sop/box)))"

let settings_authority () =
  let ws = workspace ~settings:(settings (0., "default")) (plain ^ "\n(settings :amount 3.0)") in
  with_editor ws (fun e ->
    check (Rays_editor.Settings.get settings_schema (E.settings !e) = (3., "default"))
      "opening a checked workspace discarded its saved settings";
    e := E.set_settings !e (Rays_editor.Settings.make settings_schema (7., "changed"));
    check (D.Settings.get settings_schema (E.workspace !e).settings = (7., "changed"))
      "set_settings left Workspace_doc.settings behind";
    e := E.edit !e (F.Set_graph {name = "g"; form = parse "(graph g :context sop (sop/box :size [2 2 2]))"}) |> Result.get_ok;
    check (D.Settings.get settings_schema (E.workspace !e).settings = (7., "changed"))
      "a graph edit rolled settings back")

let settings_rebuild () =
  let old = build (workspace ~settings:(settings (2., "old")) plain) in
  let edited = workspace ~settings:old.settings (plain ^ "\n(settings :amount 4.0)") in
  let doc = build ~previous:old edited in
  check (D.Settings.get settings_schema doc.settings = (4., "default"))
    "lowering preferred previous settings over explicit text or retained omitted fields";
  let ws = {edited with settings = settings (9., "explicit")} in
  check (D.Settings.get settings_schema (build ~previous:doc ws).settings = (9., "explicit"))
    "lowering ignored authored workspace settings"

let settings_presets () = with_dir (fun dir ->
  let doc = build (workspace ~settings:(settings (0., "default")) plain) in
  let path = D.Preset.save ~directory:dir ~name:"defaults" ~doc ~view:(parse "{}") |> Result.get_ok in
  let loaded = D.Preset.load ~path ~factories ~settings:(settings (8., "running")) () |> Result.get_ok in
  check (D.Settings.get settings_schema loaded.doc.settings = (0., "default"))
    "loading a preset inherited current values for omitted default settings")

let settings_graph_writeback () =
  let ws = workspace "(workspace settings (graph config :context settings (settings/config :fps 60)))" in
  with_editor ws (fun e ->
    let schema = Param.(schema ~name:"fps" ~default:60
      [field ~name:"fps" ~label:"FPS" ~kind:(integer ~min:1 ~max:120 ()) ~default:60
        ~get:Fun.id ~set:(fun x _ -> x) ()]) in
    e := E.set_settings !e (Rays_editor.Settings.make schema 90);
    let loaded = build (workspace (D.Workspace_doc.to_text (E.workspace !e))) in
    check (List.exists (fun (f : Param.field_view) ->
      f.name = "fps" && f.current = Int_value 90) (D.Settings.fields loaded.settings))
      "set_settings changed a graph-owned value without writing the settings graph")

let startup_settings () =
  with_editor (workspace "(workspace startup (graph config :context settings (settings/config)))") (fun e ->
    let fields = D.Settings.fields (build (E.workspace !e)).settings in
    check (List.length fields = 6 && List.for_all (fun (f : Param.field_view) ->
      String.ends_with ~suffix:"(on restart)" f.label || String.ends_with ~suffix:"(live)" f.label) fields)
      "workspace startup fields do not explain when edits apply";
    List.iteri (fun i (name, literal) ->
      let before = E.workspace !e in
      e := E.edit !e (F.Set_arg {node = ["config"; "@result"]; key = Kw name; sub = [];
        value = parse literal}) |> Result.get_ok;
      let after = E.workspace !e in
      let reloaded = workspace (D.Workspace_doc.to_text after) in
      check (D.Contexts.window reloaded = D.Contexts.window after)
        ("startup setting did not survive reload: " ^ name);
      check (D.Contexts.window after <> D.Contexts.window before)
        ("startup setting did not update saved configuration: " ^ name);
      e := E.update !e (Test_editor_input.frame ~keys:[Input.Meta] (450., 300.)
        [Input.Key_pressed (Input.KeyChar 'z')] (i + 2));
      check (E.workspace !e == before) ("startup setting undo did not restore its source: " ^ name))
      ["title", "\"Changed title\""; "width", "900"; "height", "640"; "fps", "90"; "seed", "42"])

let layers e =
  match Scene.Private.stage_native ~width:900 ~height:640 (E.scene e (frame 1)) with
  | Error m -> failwith m
  | Ok staged -> List.filter_map (function
      | Scene.Private.Scene3_layer s -> Some (Array.to_list s.entries |> List.map (fun entry ->
          entry.Scene_execution.draw.state.transform_uniforms, entry.draw.mesh.vertices))
      | _ -> None) staged.layers

let scene_text = {|(workspace picture
  (graph g :context sop (sop/box))
  (graph scene :context scene
    (let* [object (scene/geometry (ref g) :name "box")
           lamp (scene/light :intensity 1)] (scene/merge object lamp)))
  (graph editor :context editor (ui/workspace (ui/viewport (ref scene)))))|}

let frozen_context_time () =
  let cases = [
    "scene", "(scene/light :width (+ 1 t))", "scene/light.width";
    "scene", "(scene/geometry (ref g) :translate [t 0 0])", "scene/geometry.translate";
    "world", "(world/sky :turbidity (+ 2 t))", "world/sky.turbidity";
    "settings", "(settings/config :fps (+ 60 (int t)))", "settings/config.fps";
    "editor", "(ui/workspace (ui/split-at \"horizontal\" (+ 0.4 (* 0.1 t)) (ui/graph) (ui/inspector)))",
      "ui/split-at.ratio"] in
  List.iter (fun (context, body, field) ->
    let ws = workspace (Printf.sprintf "(workspace time (graph g :context sop (sop/box)) (graph target :context %s %s))"
      context body) in
    let at time = (Flow.Eval.run ~time ws.checked |> Result.get_ok).results |> List.assoc "target" in
    check (at 0. <> at 2.) ("fixture did not vary at two times: " ^ field);
    let diagnostic = match D.Contexts.of_workspace ~factories ws with
      | Error d -> d | Ok _ -> failwith ("frozen field was accepted: " ^ field) in
    check (diagnostic.code = "E_CONTEXT_TIME" && Test_text_pane.contains diagnostic.message field)
      ("frozen-time refusal did not identify the field: " ^ Diagnostic.to_string diagnostic);
    check (Result.is_error (D.Contexts.window ws)) "startup window checking ignored a frozen context";
    let reloaded = workspace (D.Workspace_doc.to_text ws) in
    check (Result.is_error (D.Contexts.of_workspace ~factories reloaded))
      "serialized reload bypassed frozen-time refusal") cases;
  let driven = workspace {|(workspace driven
    (graph g :context sop (sop/box :size [(+ 1 t) 1 1]))
    (graph scene :context scene (scene/geometry (ref g))))|} in
  check (Result.is_ok (D.Contexts.of_workspace ~factories driven))
    "a live SOP reference was mistaken for a frozen scene field";
  with_editor (workspace scene_text) (fun e ->
    let before = E.workspace !e and label = E.undo_label !e in
    List.iter (fun (node, key, expr) ->
      match E.edit !e (F.Set_arg {node; key = Kw key; sub = []; value = parse expr}) with
      | Ok _ -> failwith "host installed a frozen time field"
      | Error message ->
          check (Test_text_pane.contains message "E_CONTEXT_TIME" && E.workspace !e == before
            && E.undo_label !e = label) "frozen-time refusal changed source/history")
      [["scene"; "object"], "translate", "[t 0 0]";
       ["scene"; "lamp"], "width", "(+ 1 t)"];
    e := E.edit !e (F.Set_arg {node = ["scene"; "object"]; key = Kw "translate";
      sub = []; value = parse "[2 0 0]"}) |> Result.get_ok;
    check (Result.is_ok (D.Contexts.of_workspace ~factories
      (workspace (D.Workspace_doc.to_text (E.workspace !e)))))
      "literal replacement did not survive reload";
    e := E.update !e (Test_editor_input.frame ~keys:[Input.Meta] (450., 300.)
      [Input.Key_pressed (Input.KeyChar 'z')] 2);
    check (E.workspace !e == before) "literal replacement did not undo in one entry")

let scene_picture () = with_editor (workspace scene_text) (fun e ->
  let before = layers !e and prepared = E.prepared !e in
  e := E.edit !e (F.Set_arg {node = ["scene"; "object"]; key = Kw "translate"; sub = [];
    value = parse "[3 0 0]"}) |> Result.get_ok;
  e := step !e 2;
  check (E.prepared !e = prepared) "moving an object changed its SOP output";
  check (layers !e <> before) "a scene-only host edit left the picture stale";
  let after = layers !e in
  e := step !e 3;
  check (layers !e = after) "an idle frame changed the picture")

let preview_text = {|(workspace previews
  (graph g :context sop (sop/box))
  (graph scene :context scene [(lamp : float 1)]
    (let* [object (scene/geometry (ref g)) light (scene/light :intensity lamp)]
      (scene/merge object light)))
  (graph empty :context scene (scene/merge))
  (graph editor :context editor
    (let* [main (ui/viewport (ref scene))
           comparison (ui/viewport (ref scene :lamp 2))
           blank (ui/viewport (ref empty))]
      (ui/workspace (ui/tile main comparison blank)))))|}

let empty_preview () =
  let doc = build (workspace preview_text) in
  check (List.assoc_opt "v:\"editor\"/\"blank\"" (Option.get doc.shell).views = Some [])
    "an empty scene instance lost its viewport identity";
  with_editor (workspace preview_text) (fun e ->
    check (List.map List.length (layers !e) = [1; 1; 0])
      "an empty preview rendered the primary scene")

let preview_identity () =
  let doc = build (workspace preview_text) in
  let shell doc = Option.get doc.D.Document.shell in
  let keys doc = Editor_core.Panels.leaves (shell doc).tree |> List.filter_map
    (function _, Editor_core.Panels.View key -> Some key | _ -> None) in
  let edit doc op =
    let ws = D.Workspace_doc.edit catalog (fst doc.D.Document.workspace) op |> Result.get_ok in
    D.Contexts.of_workspace ~factories ~previous:doc ws |> Result.get_ok in
  let names = keys doc in
  check (List.length names = 3 && List.length (List.sort_uniq String.compare names) = 3)
    "named viewport keys are not unique";
  let comparison = "v:\"editor\"/\"comparison\"" in
  let provenance = List.assoc comparison (shell doc).preview_sources in
  check (provenance.editor_graph = "editor" && provenance.panel = Some (D.Document.Bound "comparison")
    && Option.map Flow.Lisp.flat provenance.scene_ref = Some "(ref scene :lamp 2)")
    "comparison preview lost the authored ref/override origin";
  let changed = edit doc (F.Set_arg {node = ["editor"; "comparison"]; key = Pos 0; sub = [];
    value = parse "(ref scene :lamp 4)"}) in
  let changed_source = List.assoc comparison (shell changed).preview_sources in
  check (changed_source.instance <> provenance.instance
    && Option.map Flow.Lisp.flat changed_source.scene_ref = Some "(ref scene :lamp 4)"
    && Option.map Flow.Lisp.flat (List.assoc "v:\"editor\"/\"main\"" (shell changed).preview_sources).scene_ref
       = Option.map Flow.Lisp.flat (List.assoc "v:\"editor\"/\"main\"" (shell doc).preview_sources).scene_ref)
    "editing an instance lost its override or changed another viewport's ref";
  let swapped = edit doc (F.Set_arg {node = ["editor"; "@result"]; key = Pos 0; sub = [];
    value = parse "(ui/tile blank comparison main)"}) in
  check (keys swapped = List.rev names && List.sort compare (shell swapped).views = List.sort compare (shell doc).views)
    "reordering named panels transferred preview membership";
  let docked = edit swapped (F.Dock_panel {node = ["editor"; "comparison"];
    target = ["editor"; "main"]; side = `Right}) in
  check (List.sort String.compare (keys docked) = List.sort String.compare names)
    ("docking changed named viewport identity: " ^ String.concat "," (keys docked)
     ^ "\n" ^ D.Workspace_doc.to_text (fst docked.workspace));
  let reloaded = build (workspace (D.Workspace_doc.to_text (fst docked.workspace))) in
  check (keys reloaded = keys docked) "reload changed named viewport identity";
  let renamed = edit docked (F.Rename {node = ["editor"; "comparison"]; to_ = "alternate"}) in
  check (List.mem "v:\"editor\"/\"alternate\"" (keys renamed) && not (List.mem comparison (keys renamed)))
    "renaming retained the old authored viewport key";
  let closed = edit renamed (F.Close_panel {node = ["editor"; "alternate"]}) in
  check (not (List.mem_assoc "v:\"editor\"/\"alternate\"" (shell closed).preview_sources)
    && List.for_all (fun (key, _) -> List.mem key (keys closed)) (shell closed).views)
    "closed preview left orphan membership/provenance";
  let id = List.hd (List.assoc comparison (shell doc).views) in
  let graph, _ = Sop.Edit_graph.apply_parameters doc.scene.graph.geometry ~node_id:id
    ["translate_x", Param.Float_value 3.] |> Result.get_ok in
  let after = {doc with scene = {doc.scene with graph = Flow_sop.Network.with_geometry graph doc.scene.graph |> Result.get_ok}} in
  check (match D.Scene_sync.reconcile ~factories doc after with
    | Error message -> Test_text_pane.contains message "viewport scene reference"
    | Ok _ -> false) "derived preview edit was not explicitly refused";
  let repeated = edit doc (F.Set_arg {node = ["editor"; "@result"]; key = Pos 0; sub = [];
    value = parse "(ui/tile main main)"}) in
  check (keys repeated = ["v0"; "v1"])
    "repeated named panels did not get distinct placement fallback keys";
  let inline = edit doc (F.Set_arg {node = ["editor"; "@result"]; key = Pos 0; sub = [];
    value = parse "(ui/tile (ui/viewport (ref scene)) (ui/viewport (ref empty)))"}) in
  check (keys inline = ["v0"; "v1"] && List.for_all (fun (_, source) -> source.D.Document.scene_ref <> None)
    (shell inline).preview_sources) "inline preview fallback lost its authored ref"

let preview_orbits () =
  let ws = workspace preview_text in
  with_editor ~camera:(Easy_camera.with_inertia false (Easy_camera.create ~distance:7. ())) ws (fun e ->
    let count = ref 1 in
    let update ?(buttons = []) ?(delta = (0., 0.)) point events =
      incr count; e := E.update !e (Test_editor_input.frame ~buttons ~delta point events !count) in
    let center key =
      let doc = build (E.workspace !e) in
      let g = Pxui_shell.Layout.geometry ~hidden:[Timeline] (Option.get doc.shell).tree (frame 1) in
      let x, y, w, h = (Pxui_shell.Layout.find g (View key) |> Option.get).body in
      float (x + w / 2), float (y + h / 2) in
    let focus key = let point = center key in
      update point [Input.Pointer_moved point];
      update point [Input.Pointer_pressed (Input.LeftButton, point); Input.Pointer_released (Input.LeftButton, point)] in
    let position key = Camera.position (E.viewport_camera !e key) in
    let main = "v:\"editor\"/\"main\"" and comparison = "v:\"editor\"/\"comparison\"" in
    focus comparison;
    let initial = position main in
    let x, y = center comparison in
    let point = x, y +. 40. in
    update ~buttons:[Input.LeftButton] point [Input.Pointer_pressed (Input.LeftButton, point)];
    for k = 1 to 6 do
      let point = x +. float k *. 15., y +. 40. +. float k *. 2. in
      update ~buttons:[Input.LeftButton] ~delta:(15., 2.) point [Input.Pointer_moved point]
    done;
    let point = x +. 90., y +. 52. in
    update point [Input.Pointer_released (Input.LeftButton, point)];
    let orbited = position comparison in
    let same a b = Rays_math.Vec3.nearly_equal a b ~eps:1e-9 in
    check (not (same initial orbited) && same (position main) initial)
      "viewport orbit fixture did not produce isolated cameras";
    let edit op = e := E.edit !e op |> Result.get_ok; update (0., 0.) [] in
    let preserved () = check (same (position main) initial && same (position comparison) orbited)
      "layout change transferred or reset named viewport orbits" in
    edit (F.Set_arg {node = ["editor"; "@result"]; key = Pos 0; sub = [];
      value = parse "(ui/tile blank comparison main)"});
    preserved ();
    edit (F.Dock_panel {node = ["editor"; "comparison"]; target = ["editor"; "main"]; side = `Right});
    preserved ();
    let editor_form = Rays_editor.Private.Text_pane.make_shown (E.workspace !e).source "editor" None Graph in
    edit (F.Set_graph {name = "editor"; form = parse editor_form.text});
    preserved ();
    focus main;
    edit (F.Close_panel {node = ["editor"; "comparison"]});
    let original_form = Rays_editor.Private.Text_pane.make_shown ws.source "editor" None Graph in
    edit (F.Set_graph {name = "editor"; form = parse original_form.text});
    focus comparison;
    check (same (position comparison) initial && not (same (position comparison) orbited))
      "closing a preview left an orphan orbit that reappeared on re-add")

let preview_camera_world () =
  let ws = workspace {|(workspace cameras
    (graph g :context sop (sop/box))
    (graph scene :context scene [(eye : vec3 [0 0 6])]
      (scene/merge (scene/geometry (ref g))
        (scene/camera :name "camera" :eye eye :active true)
        (scene/world (ref world) :exposure 0.5)))
    (graph world :context world (world/sky))
    (graph editor :context editor
      (let* [main (ui/viewport (ref scene))
             comparison (ui/viewport (ref scene :eye [6 0 0]))]
        (ui/workspace (ui/tile main comparison)))))|} in
  with_editor ws (fun e ->
    let before = Camera.position (E.render_camera !e) and world = E.world !e |> Option.get in
    check (Rays_math.Vec3.nearly_equal before (Rays_math.Vec3.create 0. 0. 6.) ~eps:1e-6)
      "primary camera/World policy did not resolve authored values";
    let key = "v:\"editor\"/\"comparison\"" in
    let doc = build (E.workspace !e) in
    let ids = List.assoc key (Option.get doc.shell).views in
    let comparison = List.find_map (fun id ->
      Option.bind (Sop.Edit_graph.find doc.scene.graph.geometry ~node_id:id)
        (fun node -> Option.map fst (D.Objects.Camera.of_node node))) ids |> Option.get in
    check (Rays_math.Vec3.nearly_equal (Camera.position comparison) (Rays_math.Vec3.create 6. 0. 0.) ~eps:1e-6)
      "the comparison instance lost its independently authored camera";
    let geometry = Pxui_shell.Layout.geometry ~hidden:[Timeline]
      (Option.get doc.shell).tree (frame 2) in
    let x, y, w, h = (Pxui_shell.Layout.find geometry (View key) |> Option.get).body in
    let point = float (x + w / 2), float (y + h / 2) in
    e := E.update !e (Test_editor_input.frame point
      [Input.Pointer_pressed (Input.LeftButton, point); Input.Pointer_released (Input.LeftButton, point)] 2);
    e := E.edit !e (F.Set_arg {node = ["editor"; "comparison"]; key = Pos 0; sub = [];
      value = parse "(ref scene :eye [8 0 0])"}) |> Result.get_ok;
    e := step !e 3;
    check (Camera.position (E.render_camera !e) = before && Option.get (E.world !e) == world)
      "focusing/editing a comparison camera changed the global primary camera or World";
    e := E.update !e (Test_editor_input.frame ~keys:[Input.Meta] point
      [Input.Key_pressed (Input.KeyChar 'z')] 4);
    check (Camera.position (E.render_camera !e) = before && Option.get (E.world !e) == world)
      "undoing a comparison camera edit changed the primary camera or World")

let preview_lights () = with_editor (workspace preview_text) (fun e ->
  let before = layers !e in
  e := E.edit !e (F.Set_arg {node = ["editor"; "comparison"]; key = Pos 0; sub = [];
    value = parse "(ref scene :lamp 5)"}) |> Result.get_ok;
  e := step !e 2;
  let after = layers !e in
  check (List.hd after = List.hd before) "a comparison viewport changed the primary scene's lighting";
  check (List.nth after 1 <> List.nth before 1) "a comparison viewport did not update its own lighting")

let live_lights () =
  let ws = workspace {|(workspace live_lights
    (graph g :context sop (sop/box))
    (graph scene :context scene [(lamp : float 1)]
      (scene/merge (scene/geometry (ref g))
        (scene/light :intensity (+ lamp t) :color [(+ 0.1 (* 0.1 t)) 0.2 0.3])))
    (graph editor :context editor
      (let* [main (ui/viewport (ref scene)) comparison (ui/viewport (ref scene :lamp 2))]
        (ui/workspace (ui/tile main comparison)))))|} in
  let doc = build ws in
  let main = "v:\"editor\"/\"main\"" in
  check (not (List.mem_assoc main (Option.get doc.shell).views)
    && D.Document.Int_map.cardinal doc.scene_drives = 2)
    "live residuals turned the primary viewport into a duplicate read-only preview";
  let resolve doc time = let scene, errors = D.Contexts.resolve_scene doc ~time in
    check (errors = []) "valid live fields produced diagnostics"; scene in
  let zero = resolve doc 0. and two = resolve doc 2. in
  let intensities graph = D.Objects.lights ~render:true graph |> List.map (fun light -> light.Light.intensity) in
  check (intensities zero = [1.; 2.] && intensities two = [3.; 4.])
    "light residuals did not resolve independently at two times";
  let reloaded = build (workspace (D.Workspace_doc.to_text ws)) in
  check (intensities (resolve reloaded 2.) = intensities two)
    "live light source did not survive serialization";
  with_editor ws (fun e ->
    let source = E.workspace !e and prepared = E.prepared !e in
    let scene = E.scene_document !e in
    let before = layers !e and lights = E.lights !e in
    let before_time = Rays_editor.Timeline.time (E.timeline !e) in
    let live = frame 2 in
    e := E.update !e { live with input = { live.input with dt = 2. } };
    check (E.workspace !e == source && E.undo_label !e = None)
      "timeline lighting changed authored state or history";
    check (Option.get (E.prepared !e) == Option.get prepared)
      "timeline lighting re-prepared geometry";
    check (E.scene_document !e == scene)
      "timeline lighting changed authored object identities or scene fields";
    check (E.lights !e <> lights && layers !e <> before)
      "timeline lighting did not reach staged rendering";
    check (Rays_editor.Timeline.time (E.timeline !e) = before_time +. 2.)
      "live lighting used runtime time instead of the editor timeline";
    let frozen_frame n = let frame = frame n in { frame with input = { frame.input with dt = 0. } } in
    let before = layers !e in
    e := E.edit !e (F.Set_arg {node = ["editor"; "comparison"]; key = Pos 0; sub = [];
      value = parse "(ref scene :lamp 5)"}) |> Result.get_ok;
    e := E.update !e (frozen_frame 3);
    let after = layers !e in
    check (List.hd after = List.hd before && List.nth after 1 <> List.nth before 1)
      "a live comparison override changed primary lighting";
    let frozen = frozen_frame 4 in
    e := E.update !e { frozen with input = { frozen.input with keys = [Input.Meta];
      events = [Input.Key_pressed (Input.KeyChar 'z')] } };
    check (layers !e = before) "one undo did not restore live comparison lighting")

let live_light_failure () = with_editor
  (workspace {|(workspace failure
    (graph g :context sop (sop/box))
    (graph scene :context scene (scene/merge (scene/geometry (ref g))
      (scene/light :intensity (pow 10 (* 400 (sin t)))))))|}) (fun e ->
    let source = E.workspace !e and good = layers !e in
    let live_frame = frame 2 in e := E.update !e { live_frame with input = { live_frame.input with dt = 1. } };
    check (E.workspace !e == source && E.undo_label !e = None)
      "a failed live light expression changed authored state";
    check (layers !e = good)
      "a failed live light expression replaced the last successful scene";
    with_dir (fun directory ->
      let report () = E.crash_dump !e directory;
        In_channel.with_open_bin (Filename.concat directory "editor.txt") In_channel.input_all in
      check (Test_text_pane.contains (report ()) "E_CONTEXT_LIVE"
        && Test_text_pane.contains (report ()) "intensity")
        "a live light failure lacked a field diagnostic";
      let recovered_frame = frame 3 in
      e := E.update !e { recovered_frame with input = { recovered_frame.input with dt = 4. } };
      check (layers !e <> good && not (Test_text_pane.contains (report ()) "E_CONTEXT_LIVE"))
        "a recovered live light expression retained stale output or its diagnostic"))

let live_light_determinism () =
  let ws = workspace {|(workspace seeded
    (graph g :context sop (sop/noise_displace
      (sop/uv_sphere :segments 16 :rings 8) :seed 7 :amplitude 0.1 :frequency 1))
    (graph scene :context scene (scene/merge (scene/geometry (ref g))
      (scene/light :intensity (+ 1 t) :color [(+ 0.1 (* 0.1 t)) 0.2 0.3]))))|} in
  let replay domains = with_dir (fun presets ->
    let prepares = ref 0 and drawings = ref 0 in
    let e = ref (E.create ~ui_text_size:11 ~workspace:ws ~presets ~await:true ~domains ~seed:42L
      ~prepare:(fun _ output -> incr prepares;
        Ok (Test_support.Workspace_cook.geometry_bytes (Result.get_ok (Sop.Payload.geometry output.Sop.Session.payload))))
      ~draw:(fun _ _ -> incr drawings; Scene3.empty) () |> Result.get_ok) in
    Fun.protect ~finally:(fun () -> E.close !e) (fun () ->
      let samples = List.mapi (fun i dt ->
        let frame = frame (i + 1) in
        e := E.update !e { frame with input = { frame.input with dt } };
        Option.get (E.prepared !e), Marshal.to_string (E.lights !e) [Marshal.No_sharing])
        [0.; 1. /. 60.; 0.5; 2.] in
      check (!prepares = 1) "live lighting recooked or re-prepared unchanged seeded geometry";
      check (!drawings = 1) "live lighting rebuilt unchanged geometry drawings";
      samples)) in
  let sequential = replay 1 and parallel = replay 3 in
  check (sequential = parallel)
    "live context playback changed seeded output between one and three domains";
  check (snd (List.hd sequential) <> snd (List.nth sequential 3))
    "determinism fixture did not vary its live lighting"

let live_light_failure_isolation () = with_editor
  (workspace {|(workspace isolated
    (graph g :context sop (sop/box))
    (graph scene :context scene [(bad : bool false)]
      (scene/merge (scene/geometry (ref g))
        (scene/light :intensity (if bad (pow 10 (* 400 (sin t))) (+ 1 t)))))
    (graph editor :context editor
      (let* [main (ui/viewport (ref scene))
             comparison (ui/viewport (ref scene :bad true))]
        (ui/workspace (ui/tile main comparison)))))|}) (fun e ->
    let before = layers !e in
    let live_frame = frame 2 in e := E.update !e { live_frame with input = { live_frame.input with dt = 1. } };
    let failed = layers !e in
    check (List.hd failed <> List.hd before && List.nth failed 1 = List.nth before 1)
      "one failed preview light froze a healthy sibling or discarded its own last output";
    with_dir (fun directory ->
      E.crash_dump !e directory;
      let report = In_channel.with_open_bin (Filename.concat directory "editor.txt") In_channel.input_all in
      check (Test_text_pane.contains report "E_CONTEXT_LIVE"
        && Test_text_pane.contains report "comparison")
        "a failed preview light lacked viewport attribution");
    let recovered_frame = frame 3 in
    e := E.update !e { recovered_frame with input = { recovered_frame.input with dt = 4. } };
    check (List.nth (layers !e) 1 <> List.nth failed 1)
      "a recovered preview light retained stale output")

let document_forms () =
  List.iter (fun extra -> check (Result.is_error (D.Workspace_doc.of_text catalog (plain ^ extra)))
    ("a document silently discarded " ^ extra))
    ["\n(workspace other)"; "\n(settngs :amount 4)"; "\n(layout)\n(layout)"; "\n(settings)\n(settings)"];
  let layout = D.Layout_by_path.{empty with at = Path_map.singleton ["g"] (20., 30.)} in
  check ((D.Workspace_doc.of_text ~layout catalog plain |> Result.get_ok).layout = layout)
    "omitting layout discarded its fallback";
  check (D.Layout_by_path.is_empty
    (D.Workspace_doc.of_text ~layout catalog (plain ^ "\n(layout)") |> Result.get_ok).layout)
    "an explicit empty layout did not clear its fallback"

let lisp_apply () =
  let text = Test_text_pane.replace scene_text ~from:"(ui/viewport (ref scene))"
    ~by:"(ui/split \"horizontal\" (ui/lisp) (ui/viewport (ref scene)))" in
  let ws = workspace ~settings:(settings (3., "kept"))
    (text ^ "\n(layout (node [\"g\"] :at [24 48]))") in
  with_editor ws (fun e ->
    let geometry = Pxui_shell.Layout.geometry ~hidden:[Timeline]
      (Option.get (build ws).shell).tree (frame 1) in
    let leaf = Pxui_shell.Layout.find geometry Lisp |> Option.get in
    let x, y, w, h = leaf.body and count = ref 1 in
    let run ?(mouse = (450., 300.)) ?(keys = []) events =
      incr count; e := E.update !e (Test_editor_input.frame ~keys mouse events !count) in
    let click p = run ~mouse:p [Input.Pointer_moved p];
      run ~mouse:p [Input.Pointer_pressed (Input.LeftButton, p); Input.Pointer_released (Input.LeftButton, p)] in
    click (Test_text_pane.tab_at ~right:(x + w - 32) ~top:y 2); (* Document tab *)
    click (float (x + 180), float (y + 80));
    (* Applying the text already shown keeps metadata; a subsequent source reload sees it too. *)
    run ~keys:[Input.Meta] [Input.Key_pressed (Input.KeyChar 'a')];
    run [Input.Text_input (Test_text_pane.replace (D.Workspace_doc.to_text ws)
      ~from:"(sop/box)" ~by:"(sop/box :size [2 2 2])")];
    let before = E.prepared !e and picture = layers !e in
    click (float (x + 32), float (y + h - 40));
    check (E.undo_label !e = Some "Edit text") "Lisp apply made no document edit";
    check (E.prepared !e <> before && layers !e <> picture)
      "Lisp applied after the frame's cook or composition";
    check ((E.workspace !e).layout = ws.layout
      && D.Settings.get settings_schema (E.workspace !e).settings = (3., "kept"))
      "Lisp apply lost layout or sketch settings")

let hidden_preview () = with_editor (workspace preview_text) (fun e ->
  (* Focus the authored empty preview, then hide the UI. The fullscreen image remains empty. *)
  let doc = build (workspace preview_text) in
  let geometry = Pxui_shell.Layout.geometry ~hidden:[Timeline]
    (Option.get doc.shell).tree (frame 1) in
  let leaf = Pxui_shell.Layout.find geometry (View "v:\"editor\"/\"blank\"") |> Option.get in
  let x, y, w, h = leaf.body in
  let p = float (x + w / 2), float (y + h / 2) in
  e := E.update !e (Test_editor_input.frame p
    [Input.Pointer_pressed (Input.LeftButton, p); Input.Pointer_released (Input.LeftButton, p)] 2);
  e := E.update !e (Test_editor_input.frame p
    [Input.Key_pressed (Input.KeyChar '/'); Input.Key_pressed (Input.KeyChar 'h')] 3);
  check (List.map List.length (layers !e) = [0])
    "hiding the UI switched the focused preview to the primary scene")

let focused_framing () = with_editor (workspace preview_text) (fun e ->
  let count = ref 1 in
  let frame_view key =
    let doc = build (E.workspace !e) in
    let g = Pxui_shell.Layout.geometry ~hidden:[Timeline] (Option.get doc.shell).tree (frame 1) in
    let x, y, w, h = (Pxui_shell.Layout.find g (View key) |> Option.get).body in
    let p = float (x + w / 2), float (y + h / 2) in
    incr count; e := E.update !e (Test_editor_input.frame p
      [Input.Pointer_pressed (Input.LeftButton, p); Input.Pointer_released (Input.LeftButton, p)] !count);
    let before = Camera.position (E.viewport_camera !e key) in
    incr count; e := E.update !e (Test_editor_input.frame p [Input.Key_pressed (Input.KeyChar 'f')] !count);
    before, Camera.position (E.viewport_camera !e key) in
  let before, after = frame_view "v:\"editor\"/\"main\"" in
  check (not (Rays_math.Vec3.nearly_equal before after ~eps:1e-9))
    "viewport-only layout could not frame its populated scene";
  let before, after = frame_view "v:\"editor\"/\"blank\"" in
  check (Rays_math.Vec3.nearly_equal before after ~eps:1e-9)
    "framing an empty focused viewport used another scene's bounds")

let named_inspector () = with_dir (fun presets ->
  let factories = Test_editor_transactions.factory :: factories in
  let ws = Ws_fixture.of_text ~factories {|(workspace named
    (graph g :context sop (let* [a (sop/test_source :value 1)] a))
    (graph editor :context editor
      (ui/workspace (ui/split-at "horizontal" 0.6 (ui/graph "g") (ui/inspector)))))|} in
  let e = ref (E.create ~ui_text_size:11 ~presets ~await:true ~workspace:ws ~factories ~prepare:(fun _ _ -> Ok ())
    ~draw:(fun _ _ -> Scene3.empty) () |> Result.get_ok) in
  Fun.protect ~finally:(fun () -> E.close !e) (fun () ->
    e := step !e 1;
    let x, y, w, _ = E.node_box !e ["g"; "a"] |> Option.get in
    let p = float (x + w / 2), float (y + 10) in
    e := E.update !e (Test_editor_input.frame p
      [Input.Pointer_pressed (Input.LeftButton, p); Input.Pointer_released (Input.LeftButton, p)] 2);
    e := step !e 3;
    check (E.level !e = None) "fixture left the scene level";
    let doc = D.Contexts.of_workspace ~factories (E.workspace !e) |> Result.get_ok in
    let tree = (Option.get doc.shell).tree in
    let leaf = Pxui_shell.Layout.find
      (Pxui_shell.Layout.geometry ~hidden:[Timeline] tree (frame 1)) Inspector |> Option.get in
    let ix, iy, iw, _ = leaf.body in
    let from = float ix +. max 96. (min 150. (float iw *. 0.34)) +. 10., float (iy + 190) in
    let to_ = fst from +. 50., snd from in
    e := E.update !e (Test_editor_input.frame ~buttons:[Input.LeftButton] from
      [Input.Pointer_pressed (Input.LeftButton, from)] 4);
    e := E.update !e (Test_editor_input.frame ~buttons:[Input.LeftButton] to_
      [Input.Pointer_moved to_] 5);
    e := E.update !e (Test_editor_input.frame to_ [Input.Pointer_released (Input.LeftButton, to_)] 6);
    check (E.undo_label !e = Some "Edit value")
      "a named SOP pane at the scene level had no editable inspector fields"))

let named_handles () =
  let ws = workspace {|(workspace handles
    (graph g :context sop (let* [a (sop/box :center [0 0 0])] a))
    (graph scene :context scene (scene/geometry (ref g) :translate [1 0 0]))
    (graph editor :context editor (ui/workspace
      (ui/split-at "horizontal" 0.5 (ui/viewport (ref scene)) (ui/graph "g")))))|} in
  with_editor ~camera:(Easy_camera.with_inertia false (Easy_camera.create ~distance:7. ())) ws (fun e ->
    let count = ref 1 in
    let update ?(buttons = []) ?(delta = (0., 0.)) p events =
      incr count; e := E.update !e (Test_editor_input.frame ~buttons ~delta p events !count) in
    let x, y, width, _ = E.node_box !e ["g"; "a"] |> Option.get in
    let p = float (x + width / 2), float (y + 10) in
    update p [Input.Pointer_pressed (Input.LeftButton, p); Input.Pointer_released (Input.LeftButton, p)];
    update p [];
    check (E.level !e = None && E.selected_node !e <> None)
      "named handle selection did not resolve a SOP node independently of scene navigation";
    let bounds = (E.panes !e (frame !count)).view in
    let camera = E.viewport_camera !e "v0" in
    let screen x = Camera.world_to_screen ~viewport:bounds camera (Rays_math.Vec3.create x 0. 0.) |> Option.get in
    let base = screen 1. and unit = screen 2. in
    let dx = unit.x -. base.x and dy = unit.y -. base.y in
    let length = Float.hypot dx dy in
    let tip = base.x +. dx /. length *. 60., base.y +. dy /. length *. 60. in
    let target = fst tip +. dx /. length *. 30., snd tip +. dy /. length *. 30. in
    let before = E.workspace !e in
    update ~buttons:[Input.LeftButton] tip [Input.Pointer_pressed (Input.LeftButton, tip)];
    update ~buttons:[Input.LeftButton] ~delta:(fst target -. fst tip, snd target -. snd tip)
      target [Input.Pointer_moved target];
    update target [Input.Pointer_released (Input.LeftButton, target)];
    let after = E.workspace !e in
    let binding ws = Option.map (fun (_, v) -> Flow.Lisp.flat v)
      (Rays_editor.Private.Text_pane.binding ws.D.Workspace_doc.source ["g"; "a"]) in
    check (after != before && binding after <> binding before)
      "named SOP handle did not write its owning graph argument";
    let scene_value = List.assoc "scene" (Flow.Eval.run ~time:0. after.checked |> Result.get_ok).results in
    check (match scene_value with Flow.Eval.Struct (_, _, args) ->
      List.assoc_opt "translate" args = Some (Flow.Eval.Vec3 (1., 0., 0.)) | _ -> false)
      "SOP handle accidentally edited the scene object's placement")

let overlapping_renderers () =
  let ws = workspace {|(workspace overlap
    (graph g :context sop [(size : float 1)] (sop/box :size [size size size]))
    (graph scene :context scene [(size : float 1)] (scene/geometry (ref g :size size)))
    (graph editor :context editor
      (let* [main (ui/viewport (ref scene))
             one (ui/viewport (ref scene :size 2))
             two (ui/viewport (ref scene :size 3))]
        (ui/workspace (ui/tile main one two)))))
    (layout (panel ["editor" "one"] :window [150 120 320 280])
            (panel ["editor" "two"] :window [150 120 320 280]))|} in
  with_editor ws (fun e ->
    e := step (E.set_renderer !e Rays_editor.Renderer.Wireframe) 2;
    let pictures = layers !e in
    check (List.length pictures = 3) "an overlapping viewport did not render";
    check (List.nth pictures 1 <> List.nth pictures 2)
      "coincident floating bounds made two renderers paint the same slot")

(* the viewport reads the renderer and the render settings from the scene's root, which wins over
   a viewport request; without a root the request stands *)
let root_viewport () =
  let ws = workspace {|(workspace rooted
    (graph g :context sop (sop/box))
    (graph scene :context scene
      (scene/root (scene/merge (scene/geometry (ref g)))
        :renderer "Wireframe" :width 800 :height 600 :max_spp 32)))|} in
  with_editor ws (fun e ->
    e := step !e 2;
    check (E.renderer !e = Rays_editor.Renderer.Wireframe) "the viewport did not take the root's renderer";
    let settings = E.render_settings !e in
    check (settings.width = 800 && settings.height = 600 && settings.max_spp = 32)
      "the render settings are not the root's";
    e := step (E.set_renderer !e Rays_editor.Renderer.Raster) 3;
    check (E.renderer !e = Rays_editor.Renderer.Wireframe) "a viewport request overrode the root's renderer");
  with_editor (workspace plain) (fun e ->
    e := step (E.set_renderer !e Rays_editor.Renderer.Wireframe) 2;
    check (E.renderer !e = Rays_editor.Renderer.Wireframe) "without a root the viewport request stands")

let stale_inspector_draft () = with_dir (fun presets ->
  let factories = Test_editor_transactions.factory :: factories in
  let ws = Ws_fixture.of_text ~factories {|(workspace drafts
    (graph g :context sop (let* [a (sop/test_source :value 1)] a))
    (graph editor :context editor (ui/workspace
      (ui/split-at "horizontal" 0.6
        (ui/split-at "vertical" 0.5 (ui/graph "g") (ui/lisp)) (ui/inspector)))))|} in
  let e = ref (E.create ~ui_text_size:11 ~presets ~await:true ~workspace:ws ~factories ~prepare:(fun _ _ -> Ok ())
    ~draw:(fun _ _ -> Scene3.empty) () |> Result.get_ok) in
  let count = ref 0 in
  let update ?(keys = []) ?(buttons = []) p events =
    incr count; e := E.update !e (Test_editor_input.frame ~keys ~buttons p events !count) in
  let click p = update p [Input.Pointer_pressed (Input.LeftButton, p); Input.Pointer_released (Input.LeftButton, p)] in
  Fun.protect ~finally:(fun () -> E.close !e) (fun () ->
    e := step !e 1;
    let x, y, w, _ = E.node_box !e ["g"; "a"] |> Option.get in
    click (float (x + w / 2), float (y + 10));
    let tree = (Option.get (D.Contexts.of_workspace ~factories (E.workspace !e)
      |> Result.get_ok).shell).tree in
    let panes = Pxui_shell.Layout.geometry ~hidden:[Timeline] tree (frame 1) in
    let lx, ly, _, lh = (Pxui_shell.Layout.find panes Lisp |> Option.get).body in
    let area = float (lx + 150), float (ly + 70) in
    click area;
    update ~keys:[Input.Meta] area [Input.Key_pressed (Input.KeyChar 'a')];
    update area [Input.Text_input "(let* [a (sop/test_source :value 2)] a)"];
    let ix, iy, iw, _ = (Pxui_shell.Layout.find panes Inspector |> Option.get).body in
    let from = float ix +. max 96. (min 150. (float iw *. 0.34)) +. 10., float (iy + 190) in
    let to_ = fst from +. 50., snd from in
    let before = E.workspace !e in
    update ~buttons:[Input.LeftButton] from [Input.Pointer_pressed (Input.LeftButton, from)];
    update ~buttons:[Input.LeftButton] to_ [Input.Pointer_moved to_];
    update to_ [Input.Pointer_released (Input.LeftButton, to_)];
    let edited = E.workspace !e and label = E.undo_label !e in
    check (edited != before && label = Some "Edit value") "fixture inspector did not change the source";
    click (float (lx + 60), float (ly + lh - 36));
    check (E.workspace !e == edited && E.undo_label !e = label)
      "an old Selection draft overwrote an inspector edit";
    E.crash_dump !e presets;
    let report = In_channel.with_open_bin (Filename.concat presets "editor.txt") In_channel.input_all in
    check (Test_text_pane.contains report "draft yes" && Test_text_pane.contains report "E_DRAFT_CONFLICT")
      "inspector conflict did not retain the draft and explain the refusal"))

let failed_renderer_modes () = with_dir (fun presets ->
  let ws = workspace {|(workspace errors
    (graph g :context sop [(size : float 1)] (sop/box :size [size size size]))
    (graph scene :context scene [(size : float 1)] (scene/geometry (ref g :size size)))
    (graph editor :context editor (ui/workspace
      (ui/tile (ui/viewport (ref scene)) (ui/viewport (ref scene :size 2))))))|} in
  let e = ref (E.create ~ui_text_size:11 ~await:true ~presets ~workspace:ws
    ~prepare:(fun _ output -> Rdk_rays.Rays_mesh.to_mesh (Result.get_ok (Sop.Payload.geometry output.Sop.Session.payload))
      |> Result.map_error Rdk.Error.to_string)
    ~draw:(fun _ mesh -> Scene3.create [Scene3.mesh (Mesh.with_mode Mesh.Lines mesh)]) () |> Result.get_ok) in
  let report () = E.crash_dump !e presets;
    In_channel.with_open_bin (Filename.concat presets "editor.txt") In_channel.input_all in
  Fun.protect ~finally:(fun () -> E.close !e) (fun () ->
    e := step (E.set_renderer !e Rays_editor.Renderer.Wireframe) 1;
    check (List.length (layers !e) = 2) "fixture wireframes did not render";
    e := step (E.set_renderer !e Rays_editor.Renderer.Path_traced) 2;
    let doc = build (E.workspace !e) in
    let keys = Editor_core.Panels.leaves (Option.get doc.shell).tree
      |> List.filter_map (function _, Editor_core.Panels.View key -> Some key | _ -> None) in
    let text = report () in
    check (List.length keys = 2 && List.for_all (fun key ->
      Test_text_pane.contains text ("Renderer [" ^ key ^ "]")) keys)
      "renderer errors were not attributed to every failed viewport";
    check (Test_text_pane.contains text "no output" && layers !e = [])
      "failed path-tracing mode silently retained wireframe output";
    e := step (E.set_renderer !e Rays_editor.Renderer.Wireframe) 3;
    check (List.length (layers !e) = 2 && Test_text_pane.contains (report ()) "renderer: -")
      "switching mode did not recover the outputs and clear renderer errors"))

let run_native () = with_dir (fun directory ->
  let ws = workspace {|(workspace failures
    (graph g :context sop [(size : float 1)] (sop/box :size [size size size]))
    (graph scene :context scene [(size : float 1)] (scene/geometry (ref g :size size)))
    (graph editor :context editor
      (let* [main (ui/viewport (ref scene))
             comparison (ui/viewport (ref scene :size 1.5))
             empty (ui/viewport (ref scene :size 3))]
        (ui/workspace (ui/tile main comparison empty)))))|} in
  let path = Filename.concat directory "failure.png" in
  let primary_bounds = ref (0, 0, 0, 0) in
  let report e =
    E.crash_dump e directory;
    In_channel.with_open_bin (Filename.concat directory "editor.txt") In_channel.input_all in
  ignore (Result.get_ok (Sketch.run_state ~max_frames:16
    ~config:{Sketch.default_config with width = 900; height = 640; domains = Some 1;
      title = "Preview failure isolation"}
    ~init:(fun _ -> E.create ~ui_text_size:11 ~await:true ~workspace:ws
      ~presets:(Filename.concat directory "presets")
      ~prepare:(fun _ output -> Rdk_rays.Rays_mesh.to_mesh (Result.get_ok (Sop.Payload.geometry output.Sop.Session.payload))
        |> Result.map_error Rdk.Error.to_string)
      ~draw:(fun _ mesh ->
        let size = abs_float (Option.get (Mesh.vertex 0 mesh)).Rays_math.Vec3.x *. 2. in
        if size > 2.5 then Scene3.create [Scene3.group []]
        else Scene3.create [Scene3.mesh ~material:(Material.unlit (Rays_math.Color.rgb 200 30 20))
          (if size > 1.5 then Mesh.with_mode Mesh.Lines mesh else mesh)]) () |> Result.get_ok)
    ~update:(fun e frame ->
      let e = if List.mem frame.input.frame [3; 10; 13] then
        E.edit e (F.Set_arg {node = ["editor"; "comparison"]; key = Pos 0; sub = [];
          value = parse (if frame.input.frame = 10 then "(ref scene :size 1.5)" else "(ref scene :size 2)")})
        |> Result.get_ok else e in
      let mode = if frame.input.frame >= 15 then Rays_editor.Renderer.Wireframe else Path_traced in
      E.update (E.set_renderer e mode) frame)
    ~view:E.scene
    ~after_present:(fun e frame ->
      if frame.input.frame = 8 then begin
        primary_bounds := (E.panes e frame).view;
        check (Canvas.save_screen_png path = Ok ()) "failure capture failed";
        let text = report e in
        check (Test_text_pane.contains text "Renderer [" && Test_text_pane.contains text "stale output retained")
          "failed renderer did not identify the viewport and retained stale output"
      end;
      if List.mem frame.input.frame [12; 16] then
        check (Test_text_pane.contains (report e) "renderer: -")
          "renderer failure persisted after recovery or mode switching";
      E.after_present e frame)
    ~on_stop:E.close ()));
  let image = Image.load_exn path in
  Fun.protect ~finally:(fun () -> ignore(Image.destroy image)) (fun () ->
    let pixels = Image.Private.pixels image |> Result.get_ok in
    let red = ref 0 in
    let x, y, width, height = !primary_bounds in
    let image_width = Result.get_ok (Image.get_width image)
    and image_height = Result.get_ok (Image.get_height image) in
    let scale_x = float image_width /. 900.
    and scale_y = float image_height /. 640. in
    for py = int_of_float (float y *. scale_y) to int_of_float (float (y + height) *. scale_y) - 1 do
    for px = int_of_float (float x *. scale_x) to int_of_float (float (x + width) *. scale_x) - 1 do
      let offset = py * image_width + px in
      let r = Char.code (Bytes.get pixels (offset * 4))
      and g = Char.code (Bytes.get pixels (offset * 4 + 1)) in
      if r > 110 && r > g + 40 then incr red
    done done;
    check (!red > 30) "a failed comparison renderer blanked the healthy viewport"))

let enter_camera () =
  let ws = workspace {|(workspace cameras
    (graph g :context sop (sop/box))
    (graph scene :context scene
      (scene/merge (scene/geometry (ref g))
        (scene/camera :name "near" :eye [2 3 6])
        (scene/camera :name "telephoto" :eye [13 8.8 14.8] :fov 20))))|} in
  with_editor ws (fun e ->
    let count = ref 1 in
    let step ?(mouse = (450., 300.)) events =
      incr count; e := E.update !e (Test_editor_input.frame mouse events !count) in
    let click button (x, y) = [Input.Pointer_pressed (button, (x, y));
      Input.Pointer_released (button, (x, y))] in
    let gx, gy, _, _ = (E.panes !e (frame 0)).graph in
    let in_list = float (gx + 60), float (gy + 350) in
    step ~mouse:in_list (click Input.LeftButton in_list);
    let key k = Input.Key_pressed k in
    step [key Input.Home];
    let rec select name index =
      if Option.map Sop.Node.label (E.selected_node !e) = Some name then index
      else (check (index < 4) ("no camera row " ^ name);
        step [key Input.ArrowDown]; select name (index + 1)) in
    let row = select "telephoto" 0 in
    step [key (Input.KeyChar 'i')];
    check (E.look_through !e && E.view_camera !e = E.render_camera !e
      && Camera.position (E.render_camera !e) = Rays_math.Vec3.create 13. 8.8 14.8)
      "camera Enter did not choose and look through the selected camera";
    step [key (Input.KeyChar 'i')];
    check (E.look_through !e) "repeated camera Enter disabled look-through";
    let in_view = 100., 300. in
    step ~mouse:in_view (click Input.LeftButton in_view);
    step ~mouse:in_view [key (Input.KeyChar '/'); key (Input.KeyChar 'v')];
    check (not (E.look_through !e)) "camera fixture did not leave look-through";
    let at = float (gx + 60), float (gy + 24 + row * 24 + 12) in
    step ~mouse:at (click Input.RightButton at);
    step ~mouse:at [];
    let item = fst at +. 20., snd at +. 12. in
    step ~mouse:item (click Input.LeftButton item);
    check (E.look_through !e && E.view_camera !e = E.render_camera !e)
      "right-click Enter on the camera did not enable look-through")

let no_viewport_click () =
  let ws = workspace {|
(workspace no_vp
  (graph g :context sop (sop/box))
  (graph editor :context editor
    (let* [o (ui/outline)
           g (ui/graph)
           i (ui/inspector)]
      (ui/workspace (ui/tile o g i)))))
|} in
  with_editor ws (fun e ->
    let click = [Input.Pointer_pressed (Input.LeftButton, (15., 15.));
                 Input.Pointer_released (Input.LeftButton, (15., 15.))] in
    let frame = frame 2 in
    let f = { frame with input = { frame.input with events = click; pointer = (15., 15.) } } in
    e := E.update !e f)

(* exact precision: the canvas's rect positions stay host instances the check reads *)
let compiled_probe_panels () =
  let text = {|(workspace probes
    (graph picture :context draw
      (let* [count 10000
             positions (state [previous (array/vec3 count [0 10 0])]
                         (map (fn [p] (+ p [1 0 0])) previous))
             shifted (map (fn [p] (+ p [1 0 0])) positions)]
        (draw/rects shifted [1 1 0])))
    (graph config :context settings (settings/config :precision "exact"))
    (graph editor :context editor
      (ui/workspace (ui/split "horizontal" (ui/canvas (ref picture))
        (ui/split "vertical" (ui/graph "picture") (ui/lisp :tab "graph"))))))|} in
  with_editor (workspace text) (fun e ->
    let first_x () = E.Private.canvas_scenes !e |> List.hd |> Scene.Private.commands
      |> Array.to_list |> List.find_map (function
        | Scene_command.Render_ir.Shapes shapes when Scene_command.Ui_batch.count shapes > 1 ->
            Some (Scene_command.Ui_batch.count shapes,
              Int32.float_of_bits (Bytes.get_int32_le (Scene_command.Ui_batch.instances shapes) 0))
        | _ -> None) |> Option.get in
    let previous = ref (snd (first_x ())) in
    for count = 2 to 13 do
      e := step !e count;
      let n, x = first_x () in
      check (n = 10000 && x = !previous +. 1.) "panel probes advanced the canvas fold more than once";
      previous := x
    done;
    Gc.full_major ();
    let before = Gc.allocated_bytes () in
    for count = 14 to 21 do e := step !e count done;
    let bytes = (Gc.allocated_bytes () -. before) /. 8. in
    check (bytes < 8_000_000.)
      (Printf.sprintf "live panel probes rebuilt packed inputs through the reference walker (%.0f B/frame)" bytes);
    e := E.edit !e (F.Set_graph {name="picture"; form=parse {|(graph picture :context draw
      (let* [count 7000
             positions (state [previous (array/vec3 count [0 20 0])]
                         (map (fn [p] (+ p [2 0 0])) previous))
             shifted (map (fn [p] (+ p [3 0 0])) positions)]
        (draw/rects shifted [1 1 0])))|}}) |> Result.get_ok;
    e := step !e 22;
    let n, x = first_x () in
    check ((n, x) = (7000, 5.))
      (Printf.sprintf "source edits reused a stale probe program or advanced the new fold (%d, %.1f)" n x);
    let frame = frame 23 in
    e := E.update !e { frame with input = { frame.input with keys=[Input.Shift]; events=[Input.Key_pressed (Input.KeyChar 'p')] } };
    let n, x = first_x () in
    check ((n, x) = (7000, 5.)) (Printf.sprintf "timeline reset retained old probe state (%d, %.1f)" n x);
    e := step !e 24;
    check (first_x () = (7000, 7.)) "the reset fold did not resume exactly once")

let run () =
  let failures = List.filter_map (fun (name, run) ->
    try run (); None with exn -> Some (name ^ ": " ^ Printexc.to_string exn))
    ["compiled live panel probes", compiled_probe_panels;
     "settings opening and edits", settings_authority; "settings lowering", settings_rebuild;
     "preset defaults", settings_presets; "settings graph writeback", settings_graph_writeback;
     "startup settings", startup_settings; "frozen context time", frozen_context_time;
     "scene picture", scene_picture; "empty preview", empty_preview;
     "preview identity and provenance", preview_identity;
     "preview orbit identity", preview_orbits;
     "preview camera and World ownership", preview_camera_world;
     "preview lighting", preview_lights;
     "live lighting", live_lights; "live lighting failure", live_light_failure;
     "live lighting determinism and reuse", live_light_determinism;
     "live lighting failure isolation", live_light_failure_isolation;
     "document forms", document_forms; "Lisp applies before rendering", lisp_apply;
     "fullscreen preview", hidden_preview; "named graph inspector", named_inspector;
     "focused viewport framing", focused_framing;
     "named graph handles", named_handles;
     "stale inspector draft", stale_inspector_draft;
     "root renderer and render settings", root_viewport;
     "renderer failure modes", failed_renderer_modes;
     "overlapping renderers", overlapping_renderers;
     "camera Enter and row menu", enter_camera;
     "no viewport click", no_viewport_click] in
  if failures <> [] then failwith (String.concat "\n" failures);
  print_endline "editor consistency: settings, saved defaults, scene pictures, and isolated previews passed"
