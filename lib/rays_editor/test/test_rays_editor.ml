open Rays
open Sop
open Test_support

let pointer (x, y) = float x, float y
let mouse_press (button, point) = Input.Pointer_pressed (button, pointer point)
let mouse_release (button, point) = Input.Pointer_released (button, pointer point)
let mouse_move point = Input.Pointer_moved (pointer point)

let frame ?(width = 900) ?(height = 640) ?mouse ?(events = []) count : Frame.t = {
  input = { (Input.at_time (float_of_int count /. 60.)) with dt = 1. /. 60.;
    frame = count; tick = count; size = width, height;
    pointer = (let x, y = Option.value ~default:(width / 2, height / 2) mouse in
      float x, float y); events };
  pixel_scale = 1., 1.; fps = 60.; mouse_delta = 0., 0.;
}

let width (_, _, width, _) = width
let center (x, y, width, height) = x + (width / 2), y + (height / 2)
(* the middle of a card's header: the middle of the card may be a row, or the [+ N more] row *)
let card_header (x, y, width, _) = x + (width / 2), y + 3  (* inside the header at any zoom *)
(* the label of a card's first row (left of its value field), at any zoom: a press there is the tile's *)
let card_row (x, y, width, _) = x + (width * 20 / 196), y + (width * 36 / 196)

(* False in the window-free [runtest] pass: the editor logic runs, and only
   checks that stage through Metal or open a window are skipped. *)
let native = ref true

(* The packed PXUI instances of a composed scene, for exact comparisons. *)
let ui_bytes scene =
  if not !native then "" else
  match Scene.Layer.stage_native_render ~width:900 ~height:640 scene with
  | Error message -> fail message
  | Ok staged -> String.concat "" (List.filter_map (function
      | Scene.Layer.Ui_layer (batch, _) ->
          Some (Bytes.to_string (Scene_command.Ui_batch.instances batch))
      | _ -> None) staged.layers)

(* The scene opens as a list: a click on the geo1 row selects it and [i] enters its graph. *)
let enter_geo1 ~update ~panes environment count =
  let gx, gy, _, _ = (panes environment (frame count)).Pxui_shell.Layout.graph in
  let point = gx + 60, gy + 24 + 12 in
  let click = [mouse_press (Input.LeftButton, point); mouse_release (Input.LeftButton, point)] in
  let environment = update environment (frame ~mouse:point (count + 1)) in
  let environment = update environment (frame ~mouse:point ~events:click (count + 2)) in
  update environment (frame ~events:[Input.Key_pressed (Input.KeyChar 'i')] (count + 3))

(* A geometry graph of two nodes, the second the result. *)
let text = {|(workspace inspectable
  (graph geo1 :context sop
    (let* [source (sop/box :size [2 2 2])
           output (sop/transform source :translate [5 0 0])]
      output)))|}

let fixture () = Ws_fixture.of_text text

(* The line of the crash report's [name] (level, projection, camera, scope selected ...). *)
let report_line dump environment name =
  let directory = Filename.temp_dir "rays-editor-report" "" in
  Fun.protect ~finally:(fun () ->
    Array.iter (fun f -> Sys.remove (Filename.concat directory f)) (Sys.readdir directory);
    Unix.rmdir directory) (fun () ->
    dump environment directory;
    let read file = In_channel.with_open_bin (Filename.concat directory file) In_channel.input_all in
    let lines = String.split_on_char '\n' (read "editor.txt" ^ read "document.txt") in
    match List.find_opt (fun l -> String.starts_with ~prefix:(name ^ ": ") l
        || String.starts_with ~prefix:(name ^ " ") l) lines with
    | Some l -> l
    | None -> fail ("no " ^ name ^ " line in the report"))

  let enter3 environment count = enter_geo1 ~update:Rays_editor.Editor.update
      ~panes:Rays_editor.Editor.panes environment count
  let text_of environment = Editor_document.Workspace_doc.to_text
      (Rays_editor.Editor.workspace environment)
  let has text piece =
    let n = String.length piece in
    let rec at i = i + n <= String.length text && (String.sub text i n = piece || at (i + 1)) in
    at 0

  let cameras environment = List.filter (fun (info : Editor_document.Kind_graph.node) -> info.kind = "camera")
      (Rays_editor.Editor.scene_document environment)
  (* the id of the ACTIVE camera, from the crash report's "camera N" line *)
  let active environment =
    match String.split_on_char ' ' (report_line Rays_editor.Editor.crash_dump environment "camera") with
    | [ _; "-" ] -> []
    | [ _; id ] -> [ int_of_string id ]
    | _ -> fail "unreadable camera line"
  let near a b = Rays_math.Vec3.nearly_equal a b ~eps:1e-6
  let eye environment = Camera.position (Rays_editor.Editor.render_camera environment)
  let viewport_eye environment =
    Camera.position (Easy_camera.camera (Rays_editor.Editor.camera environment))

let wait count environment =
  let deadline = Unix.gettimeofday () +. 60. in
  let rec go count environment =
    let environment = Rays_editor.Editor.update environment (frame count) in
    match Rays_editor.Editor.prepared environment with
    | Some _ -> environment
    | None when Unix.gettimeofday () < deadline ->
        Unix.sleepf 0.001;
        go (count + 1) environment
    | None -> fail "sketch environment did not publish its initial async cook" in
  go count environment

let camera_nodes () =
  (* Camera nodes: a default camera following the viewport joins a document
     without one, exactly one is ACTIVE, deleting the last re-adds it inside
     the same undo entry, and a viewport drag is one coalesced undo entry. *)
  let environment = Rays_editor.Editor.create ~ui_text_size:11 ~await:true ~workspace:(fixture ())
      ~camera:(Easy_camera.create ~distance:6. ~inertia:false ())
     
      ~max_entries:4 ~max_payload_bytes:(16 * 1024 * 1024)
      ~prepare:Editor_prepare.surface
      ~draw:(fun _ surface -> Rdk_rays.Surface.scene3 surface.Rays_editor__.Cook.surface) ()
    |> Result.get_ok in
  check (List.length (cameras environment) = 1 && List.length (active environment) = 1)
    "Editor did not add one ACTIVE default camera";
  (* Cameras are scene objects: rows of the scene list. *)
  let environment = List.fold_left (fun environment count ->
      Rays_editor.Editor.update environment (frame count)) environment [0; 1; 2] in
  check (not (Rays_editor.Editor.can_undo environment)
      && near (eye environment) (viewport_eye environment))
    "an idle following camera wrote undo entries or drifted from the viewport";
  (* The default camera object is a pinhole; the view camera
     is the viewport with that lens while nothing looks through. *)
  let view_lens environment = Camera.lens (Rays_editor.Editor.view_camera environment) in
  check ((Camera.lens (Rays_editor.Editor.render_camera environment)).aperture = Camera.pinhole.aperture
      && (view_lens environment).aperture = Camera.pinhole.aperture
      && near (Camera.position (Rays_editor.Editor.view_camera environment))
           (viewport_eye environment))
    "the default camera object did not carry its lens into the view camera";
  let idle = Rays_editor.Editor.update environment (frame 3) in
  check (Rays_editor.Editor.view_camera idle == Rays_editor.Editor.view_camera environment)
    "an idle frame rebuilt the view camera (a sketch tracer would restart each frame)";
  let start_eye = eye environment in
  let vx, vy, vw, vh = (Rays_editor.Editor.panes environment (frame 3)).view in
  let px, py = vx + (vw / 2), vy + (vh / 2) in
  let environment = List.fold_left (fun environment (count, events) ->
      let frame = frame ~events count in
      Rays_editor.Editor.update environment { frame with input = { frame.input with buttons = [Input.LeftButton] } })
    environment [
      4, [mouse_press (Input.LeftButton, (px, py))];
      5, [mouse_move (px + 30, py)];
      6, [mouse_move (px + 60, py + 10)];
      7, [mouse_move (px + 90, py + 20)]] in
  let environment = Rays_editor.Editor.update environment
      (frame ~events:[mouse_release (Input.LeftButton, (px + 90, py + 20))] 8) in
  check (not (near (eye environment) start_eye)
      && near (eye environment) (viewport_eye environment))
    "the following camera did not track a viewport orbit";
  let command key count = let frame = frame ~events:[Input.Key_pressed (Input.KeyChar key)] count in
    { frame with input = { frame.input with keys = [Input.Meta] } } in
  (* navigation is view state (E12): the orbit leaves no undo entry, and an undo with nothing to
     undo leaves the view where it is *)
  check (not (Rays_editor.Editor.can_undo environment)) "a viewport orbit made an undo entry";
  let orbited = eye environment in
  let environment = Rays_editor.Editor.update environment (command 'z' 9) in
  let environment = Rays_editor.Editor.update environment (frame 10) in
  check (near (eye environment) orbited && near (viewport_eye environment) orbited
      && not (Rays_editor.Editor.can_undo environment))
    "an undo with nothing to undo moved the viewport";
  let environment = Rays_editor.Editor.update environment
      (frame ~events:[Input.Key_pressed Input.Home] 10) in
  let environment = Rays_editor.Editor.update environment (frame 10) in
  let camera_id = List.hd (active environment) in
  (* the camera is the second row of the scene list (geo1 is the first) *)
  let gx, gy, _, _ = (Rays_editor.Editor.panes environment (frame 10)).Pxui_shell.Layout.graph in
  let at = gx + 60, gy + 24 + 24 + 12 in
  let environment = Rays_editor.Editor.update environment (frame ~mouse:at 11) in
  let environment = Rays_editor.Editor.update environment (frame ~mouse:at ~events:[
      mouse_press (Input.LeftButton, at); mouse_release (Input.LeftButton, at)] 12) in
  let environment = Rays_editor.Editor.update environment
      (frame ~events:[Input.Key_pressed Input.Delete] 16) in
  (* the host's camera is an object like any: deleting the last one is written (a scene graph
     that declares no camera), not re-seeded, and one undo gives it back *)
  check (cameras environment = [] && active environment = []
         && has (text_of environment) "graph scene" && not (has (text_of environment) "scene/camera"))
    "deleting the last camera was not written as no camera";
  let environment = Rays_editor.Editor.update environment (command 'z' 17) in
  check (List.map (fun (info : Editor_document.Kind_graph.node) -> info.id) (cameras environment) = [camera_id])
    "undo after deleting the camera did not restore the original in one step";
  (* Fly: / w (view focused) flies, held W moves forward, Escape exits;
     Space exits and arms the leader in the same frame. *)
  let key k = Input.Key_pressed k in
  let fly_view environment = let vx, vy, _, _ = (Rays_editor.Editor.panes
      environment (frame 18)).view in vx + 20, vy + 20 in
  let environment = Rays_editor.Editor.update environment (frame ~events:[
      mouse_press (Input.RightButton, fly_view environment);
      mouse_release (Input.RightButton, fly_view environment)] 18) in
  let environment = Rays_editor.Editor.update environment
      (frame ~events:[key (Input.KeyChar '/'); key (Input.KeyChar 'w')] 18) in
  check (Rays_editor.Editor.flying environment) "/ w did not enter fly mode";
  let before = viewport_eye environment in
  let environment = Rays_editor.Editor.update environment
        (let frame = frame ~events:[key (Input.KeyChar 'w')] 18 in
          { frame with input = { frame.input with keys = [Input.KeyChar 'w'] } }) in
  check (not (near (viewport_eye environment) before)
      && Rays_editor.Editor.selected_node environment = None)
    "held W did not fly the viewport";
  let environment = Rays_editor.Editor.update environment
      (frame ~events:[key Input.Escape] 18) in
  check (not (Rays_editor.Editor.flying environment)) "Escape did not exit fly mode";
  let environment = Rays_editor.Editor.update environment
      (frame ~events:[key (Input.KeyChar '/'); key (Input.KeyChar 'w')] 18) in
  let environment = Rays_editor.Editor.update environment
      (frame ~events:[key (Input.KeyChar '/')] 18) in
  let environment = Rays_editor.Editor.update environment
      (frame ~events:[key (Input.KeyChar 'g')] 18) in
  check (not (Rays_editor.Editor.flying environment)
      && width (Rays_editor.Editor.panes environment (frame 18)).graph = 0)
    "Space did not exit fly mode into the leader";
  let environment = Rays_editor.Editor.update environment
      (frame ~events:[key (Input.KeyChar '/'); key (Input.KeyChar 'g')] 18) in
  (* F frames the displayed geometry in the viewport even when another node is selected. *)
  let environment = enter3 environment 400 in
  let environment = Rays_editor.Editor.update environment (frame 404) in
  let source_at = match Rays_editor.Editor.node_box environment [ "geo1"; "source" ] with
    | Some rect -> card_header rect | None -> fail "the source node has no box" in
  let environment = Rays_editor.Editor.update environment (frame ~mouse:source_at 405) in
  let environment = Rays_editor.Editor.update environment (frame ~mouse:source_at ~events:[
      mouse_press (Input.LeftButton, source_at); mouse_release (Input.LeftButton, source_at)] 406) in
  let target environment = Easy_camera.target (Rays_editor.Editor.camera environment) in
  let view_at = fly_view environment in
  let environment = Rays_editor.Editor.update environment (frame ~events:[
      mouse_press (Input.LeftButton, view_at); mouse_release (Input.LeftButton, view_at)] 407) in
  let environment = Rays_editor.Editor.update environment
      (frame ~events:[key (Input.KeyChar 'f')] 408) in
  (* Framing waits for bounds when the display cook is still running. *)
  let deadline = Unix.gettimeofday () +. 60. in
  let rec framed count environment =
    if near (target environment) (Rays_math.Vec3.create 5. 0. 0.)
        || Unix.gettimeofday () > deadline then environment
    else (Unix.sleepf 0.001;
      framed (count + 1) (Rays_editor.Editor.update environment (frame count))) in
  let environment = framed 409 environment in
  check (near (target environment) (Rays_math.Vec3.create 5. 0. 0.))
    "viewport-focused F did not focus on the displayed node's cached bounds";
  Rays_editor.Editor.close environment

let sketch_settings () =
  (* Sketch settings live in the document: [prepare] reads them, a change
     recooks, and undo restores the previous value (voxel_wall's renderer). *)
  let mode_schema = Param.(schema ~name:"mode" ~default:0
    [ field ~name:"mode" ~label:"Mode" ~kind:(integer ~min:0 ~max:3 ())
        ~default:0 ~get:Fun.id ~set:(fun mode _ -> mode) () ]) in
  let module Settings = Rays_editor.Settings in
  let seen = Atomic.make (-1) in
  let environment = Rays_editor.Editor.create ~ui_text_size:11 ~await:true ~workspace:(fixture ())
      ~settings:(Settings.make mode_schema 0)
      ~max_entries:4 ~max_payload_bytes:(16 * 1024 * 1024)
      ~prepare:(fun settings output -> Atomic.set seen (Settings.get mode_schema settings); Editor_prepare.empty settings output)
      ~draw:(fun _ _ -> Scene.D3.create []) () |> Result.get_ok in
  let deadline = Unix.gettimeofday () +. 60. in
  let rec wait_mode expected count environment =
    let environment = Rays_editor.Editor.update environment (frame count) in
    if Atomic.get seen = expected && Rays_editor.Editor.prepared environment <> None then environment
    else if Unix.gettimeofday () < deadline then
      (Unix.sleepf 0.001; wait_mode expected (count + 1) environment)
    else fail "a settings change did not re-run prepare" in
  let environment = wait_mode 0 0 environment in
  let environment = wait_mode 1 100
      (Rays_editor.Editor.set_settings environment (Settings.make mode_schema 1)) in
  let undo count = let frame = frame ~events:[Input.Key_pressed (Input.KeyChar 'z')] count in
    { frame with input = { frame.input with keys = [Input.Meta] } } in
  let environment = wait_mode 0 200
      (Rays_editor.Editor.update environment (undo 199)) in
  check (Settings.get mode_schema (Rays_editor.Editor.settings environment) = 0)
    "undo did not restore the sketch settings";
  Rays_editor.Editor.close environment

let preset_and_look_through () =
  (* Every document is a workspace, so every document saves: the prompt owns the keyboard
     while it is open, Enter writes the workspace text, and / b loads it back. *)
  let module Preset = Editor_document.Preset in
  let presets = Filename.temp_dir "sketch-ui-workspace-presets" "" in
  let mesh_scene (prepared : Rays_editor__.Cook.prepared) = Rdk_rays.Surface.scene3 prepared.surface in
  let environment = Rays_editor.Editor.create ~ui_text_size:11 ~await:true ~workspace:(fixture ()) ~presets
      ~camera:(Easy_camera.create ~distance:6. ~inertia:false ())
     
      ~max_entries:4 ~max_payload_bytes:(16 * 1024 * 1024)
      ~prepare:Editor_prepare.surface
      ~draw:(fun _graph mesh -> mesh_scene mesh) () |> Result.get_ok in
  let environment = wait 0 environment in
  let key k = Input.Key_pressed k in
  let environment = Rays_editor.Editor.update environment
      (frame ~events:[key (Input.KeyChar '/'); key (Input.KeyChar 's')] 50) in
  let graph_width = width (Rays_editor.Editor.panes environment (frame 50)).graph in
  let environment = Rays_editor.Editor.update environment
      (frame ~events:[key (Input.KeyChar '/'); key (Input.KeyChar 'g')] 50) in
  check (width (Rays_editor.Editor.panes environment (frame 50)).graph
      = graph_width) "open preset prompt let a workspace shortcut toggle the graph";
  let environment = Rays_editor.Editor.update environment
      (frame ~events:[key Input.Enter] 51) in
  let saved = Preset.list ~directory:presets in
  check (List.length saved = 1) "/ s did not save a preset of the document";
  let original = Editor_document.Workspace_doc.to_text (Rays_editor.Editor.workspace environment) in
  check (has (In_channel.with_open_bin (Preset.path ~directory:presets ~name:(fst (List.hd saved)))
                In_channel.input_all) original)
    "the preset file is not the workspace text";
  let environment = match Rays_editor.Editor.edit environment
      (Flow_graph.Flow_edit.Rename { node = [ "geo1"; "output" ]; to_ = "moved" }) with
    | Ok environment -> environment | Error message -> fail message in
  check (Editor_document.Workspace_doc.to_text (Rays_editor.Editor.workspace environment) <> original)
    "the rename did not change the document";
  let phases = Rays_editor.Editor.edit_phases environment in
  check (Flow.Phase_timer.calls phases "check" = 1
      && Flow.Phase_timer.calls phases "evaluate" = 1
      && Flow.Phase_timer.calls phases "lower" = 1
      && List.for_all (fun (e : Flow.Phase_timer.entry) -> e.seconds >= 0.) phases.entries)
    "the editor edit did not perform exactly one check/evaluation/lowering";
  check (has (report_line Rays_editor.Editor.crash_dump environment "phases") "check")
    "the crash report omitted the last edit's phases";
  let environment = Rays_editor.Editor.update environment
      (frame ~events:[key (Input.KeyChar '/'); key (Input.KeyChar 'b')] 52) in
  let environment = Rays_editor.Editor.update environment
      (frame ~events:[key Input.Enter] 53) in
  check (Editor_document.Workspace_doc.to_text (Rays_editor.Editor.workspace environment) = original
      && Rays_editor.Editor.undo_label environment = Some "Load preset")
    "/ b did not load the saved preset as one history entry";
  (* Looking through the camera fits the film to its aspect (the render
     resolution); otherwise the whole pane. *)
  let environment = Rays_editor.Editor.update environment
      (frame ~events:[key (Input.KeyChar '/'); key (Input.KeyChar 'v')] 52) in
  let environment = Rays_editor.Editor.update environment (frame 53) in
  check (Rays_editor.Editor.look_through environment
      && near (Camera.position (Rays_editor.Editor.view_camera environment)) (eye environment))
    "/ v did not look through the render camera";
  let settings = Rays_editor.Editor.render_settings environment in
  let _, _, pane_w, pane_h = (Rays_editor.Editor.panes environment (frame 54)).view in
  let fx, fy, fw, fh = Rays_editor.Editor.film environment (frame 54) in
  check (settings.width = 1920 && settings.height = 1080 && settings.max_spp = 256)
    "the scene root did not carry default render settings";
  (* the frame keeps clear of the label row above it (40), the readouts' row under it (36) and 12
     at each side ([Viewport.film]), the film's aspect inside what is left, centred there *)
  (* a body under 1000 wide follows the workspace sheet: 14.63 percent at each side, 32 above, 93 below *)
  let narrow = pane_w < 1000 in
  let side = if narrow then int_of_float (Float.round (0.1463 *. float pane_w)) else 12 in
  let top = if narrow then 32 else 40 and bottom = if narrow then 93 else 36 in
  let area_w = pane_w - (2 * side) and area_h = pane_h - top - bottom in
  check (fw <= area_w && fh <= area_h && (fw = area_w || fh = area_h)
      && abs (fw * 1080 - fh * 1920) <= 1920 && fx = side + ((area_w - fw) / 2)
      && fy = (if narrow then top else top + ((area_h - fh) / 2)))
    "look-through did not letterbox the film to the camera's aspect";
  (* / v toggles look-through off: the view camera is the free viewport
     (a following camera keeps it at the viewport). *)
  let environment = Rays_editor.Editor.update environment
      (frame ~events:[key (Input.KeyChar '/'); key (Input.KeyChar 'v')] 54) in
  check (not (Rays_editor.Editor.look_through environment)
      && near (Camera.position (Rays_editor.Editor.view_camera environment))
           (viewport_eye environment))
    "with look-through off the view camera did not follow the free viewport";
  check (Rays_editor.Editor.film environment (frame 54) = (0, 0, pane_w, pane_h))
    "without look-through the film did not fill the view pane";
  let environment = Rays_editor.Editor.update environment
      (frame ~events:[key (Input.KeyChar '/'); key (Input.KeyChar 'v')] 54) in
  check (Rays_editor.Editor.look_through environment) "/ v did not restore look-through";
  Rays_editor.Editor.close environment

let native_smoke () =
  (* Finite native smoke: the relative-pointer boundary toggles on a live
     window and is released when the sketch stops. *)
  if !native then begin
  let toggled = ref [] in
  let directory = Filename.temp_dir "sketch-ui-fly" "" in
  let nested_capture = Filename.concat directory "nested/capture.png" in
  (* One window: relative pointer, nested capture, and after_present. *)
  let final = Result.get_ok(Sketch.run_state ~max_frames:2
      ~config:{ Sketch.default_config with width = 120; height = 80 }
      ~init:(fun _ -> 0)
      ~update:(fun value (frame : Frame.t) ->
        toggled := Sketch.set_relative_mouse (frame.input.frame = 1) :: !toggled;
        if frame.input.frame = 2 then begin
          check (value = 11) "after_present model was not used on the next frame";
          check (Canvas.save_screen_png nested_capture = Ok ())
            "native capture did not create its output directory"
        end;
        value + 1)
      ~view:(fun _ _ -> [Scene.clear Rays_math.Color.black])
      ~after_present:(fun value _ -> value + 10) ()) in
  check (final = 22) "after_present model was not returned";
  check (!toggled = [Ok (); Ok ()]
      && Sketch.set_relative_mouse true <> Ok ())
    "native relative-pointer toggle failed or outlived the sketch";
  check (Sys.file_exists nested_capture)
    "native capture did not write its nested PNG"
  end

let layout_panes () =
  let module Editor_commands = Rays_editor__.Editor_commands in
  List.iter (fun (graph : _ Editor_core.Command.t) ->
    check (List.exists (fun (command : Editor_commands.command) ->
      command.id = graph.id && command.trigger = graph.trigger
      && command.action = Editor_commands.Scope_command graph.action) Editor_commands.keymap)
      "graph command missing from the host keymap") Pxui_graph.Scope.bindings;
  let camera_binding key action = List.exists (fun (command : Editor_commands.command) ->
    command.trigger = Some (Editor_core.Keymap.Leader key)
    && command.action = action) Editor_commands.keymap in
  check (camera_binding "h" Editor_commands.Hide_ui
      && camera_binding "c" Editor_commands.Open_camera)
    "camera visibility commands missing from the host keymap";
  let module Layout = Pxui_shell.Layout in
  let workspace = Pxui_shell.Layout.default in
  let panes ?hidden tree frame = Layout.(panes (geometry ?hidden tree frame)) in
  let initial = panes workspace (frame ~width:1000 0) in
  let header = (Option.get (Layout.find (Layout.geometry workspace (frame ~width:1000 0))
    Editor_core.Panels.main)).header in
  (* a header is one 24-point row under a 4-point margin *)
  check (header = (0, 4, width initial.view, 24))
    "workspace header is not a compact single line";
  check (abs (width initial.view - 449) <= 1
      && abs (width initial.graph - 349) <= 1
      && abs (width initial.inspector - 200) <= 1)
    "workspace defaults are not 45/35/20 after splitter space";
  let splitter_x = width initial.view + 2 in
  let ui = Pxui.Ui.create ~font_size:11 () in
  let workspace_step workspace frame =
    Pxui.Ui.frame ui frame (fun ui -> let a = Pxui_shell.Chrome.update workspace ui frame in
      a @ Pxui_shell.Chrome.splitters workspace ui frame) in
  ignore (workspace_step workspace (frame ~width:1000 0));
  let dragged = workspace_step workspace
      (frame ~width:1000 ~events:[
        mouse_press (Input.LeftButton, (splitter_x, 200));
        mouse_move (splitter_x + 80, 200);
        mouse_release (Input.LeftButton, (splitter_x + 80, 200))] 1) in
  let resized = List.fold_left (fun tree -> function
    | Pxui_shell.Chrome.Resize { node; size } -> Editor_core.Panels.set_size node size tree
    | _ -> tree) workspace dragged in
  let resized_panes = panes resized (frame ~width:1000 2) in
  check (List.mem Pxui_shell.Chrome.Settled dragged && resized != workspace
      && width resized_panes.view > width initial.view
      && width resized_panes.graph < width initial.graph)
    "workspace splitter did not resize its adjacent columns";
  let wider = panes resized (frame ~width:1200 3) in
  check (width wider.view > width resized_panes.view)
    "workspace splitter ratio did not survive a window resize";
  Pxui.Ui.destroy ui;
  let collapsed_panes = panes ~hidden:[Layout.Timeline; Inspector] resized (frame 5) in
  check (width collapsed_panes.inspector = 0)
    "inspector toggle did not collapse the third column"

let run () =
  layout_panes ();

  let environment = Rays_editor.Editor.create ~ui_text_size:11 ~await:true ~workspace:(fixture ())
     
      ~max_entries:4 ~max_payload_bytes:(16 * 1024 * 1024)
      ~prepare:Editor_prepare.surface
      ~draw:(fun _ surface -> Rdk_rays.Surface.scene3 surface.Rays_editor__.Cook.surface) ()
    |> Result.get_ok in
  let environment = wait 0 environment in
  let environment = enter3 environment 0 in
  check (Rays_editor.Editor.level environment = Some "geo1")
    "entering geo1 from the scene list did not open its SOP network";
  (match Sys.getenv_opt "RAYS_UI_PREVIEW" with
   | Some directory ->
       Result.get_ok(Sketch.export ~directory ~prefix:"workspace" ~frames:1
         ~config:{ Sketch.default_config with width=900; height=640 }
         (Rays_editor.Editor.scene environment))
   | None -> ());
  let environment = Rays_editor.Editor.update environment
      (frame ~events:[Input.Key_pressed (Input.KeyChar '/');
        Input.Key_pressed (Input.KeyChar 'h')] 1) in
  let hidden_scene = Rays_editor.Editor.scene environment (frame 1) in
  check (Rays_editor.Editor.scene environment (frame 2) == hidden_scene)
    "unchanged hidden 3D scene composition was rebuilt";
  let environment = Rays_editor.Editor.update environment
      (frame ~events:[Input.Key_pressed (Input.KeyChar '/');
        Input.Key_pressed (Input.KeyChar 'h')] 2) in
  (* Graph focus shows the Flow guide; the ordinary status/FPS strip remains
     under View focus. Compare sampling in the pane that displays it. *)
  let environment = Rays_editor.Editor.update environment (frame 3) in
  let environment = Rays_editor.Editor.update environment
      (frame ~mouse:(100,500) ~events:[mouse_press (Input.LeftButton,(100,500));
        mouse_release (Input.LeftButton,(100,500))] 4) in
  let at count fps={ (frame count) with fps }in
  let status environment frame=ui_bytes(Rays_editor.Editor.scene environment frame)in
  let environment=Rays_editor.Editor.update environment(at 1_000 60.)in
  let fps_initial=status environment(at 1_000 60.)in
  let environment=Rays_editor.Editor.update environment(at 1_030 120.)in
  let fps_before_deadline=status environment(at 1_030 120.)in
  let environment=Rays_editor.Editor.update environment(at 1_061 120.)in
  let fps_after_deadline=status environment(at 1_061 120.)in
  if !native then check(fps_before_deadline=fps_initial&&fps_after_deadline<>fps_initial)
    "FPS status text ignored its one-second sampling deadline";
  check (Rays_editor.Editor.selected_node environment = None)
    "camera/render controls should own an unselected inspector";
  let current_frame = frame 10 in
  let panes = Rays_editor.Editor.panes environment current_frame in
  check (Easy_camera.control_area (Rays_editor.Editor.camera environment)
      = Some panes.view)
    "3D camera gestures are not confined to the view column";
  let inspector_x, inspector_y, _, _ = panes.inspector in
  let camera_header = inspector_x + 32, inspector_y + 15 in
  let collapsed_scene = ui_bytes (Rays_editor.Editor.scene environment current_frame) in
  let blank_inspector = inspector_x + 4, inspector_y + 200 in
  let environment = Rays_editor.Editor.update environment
      (frame ~events:[mouse_press (Input.LeftButton, blank_inspector);
        mouse_release (Input.LeftButton, blank_inspector);
        Input.Key_pressed (Input.KeyChar '/'); Input.Key_pressed (Input.KeyChar 'w')] 10) in
  check (not (Rays_editor.Editor.flying environment))
    "same-frame inspector click routed a view-only shortcut";
  let environment = Rays_editor.Editor.update environment
      (frame ~events:[mouse_press (Input.LeftButton, camera_header)] 11) in
  let environment = Rays_editor.Editor.update environment
      (frame ~events:[mouse_release (Input.LeftButton, camera_header)] 12) in
  let environment = Rays_editor.Editor.update environment
      (frame ~events:[mouse_move (10, 100)] 13) in
  let expanded_scene = ui_bytes (Rays_editor.Editor.scene environment (frame 13)) in
  if !native then check (expanded_scene <> collapsed_scene)
    "workspace camera accordion lost its armed press before the release frame";
  let environment = Rays_editor.Editor.update environment
      (frame ~events:[mouse_press (Input.LeftButton, camera_header)] 14) in
  let environment = Rays_editor.Editor.update environment
      (frame ~events:[mouse_release (Input.LeftButton, camera_header)] 15) in
  let environment = Rays_editor.Editor.update environment
      (frame ~events:[mouse_move (10, 100)] 16) in
  let render_header = inspector_x + 32, inspector_y + 39 in
  let environment = Rays_editor.Editor.update environment
      (frame ~events:[mouse_press (Input.LeftButton, render_header)] 17) in
  let environment = Rays_editor.Editor.update environment
      (frame ~events:[mouse_release (Input.LeftButton, render_header)] 18) in
  let environment = Rays_editor.Editor.update environment
      (frame ~events:[mouse_move (10, 100)] 19) in
  if !native then
    check (ui_bytes (Rays_editor.Editor.scene environment (frame 19)) <> collapsed_scene)
    "workspace render accordion lost its armed press before the release frame";
  let environment = Rays_editor.Editor.update environment
      (frame ~events:[Input.Key_pressed (Input.KeyChar '/');
        Input.Key_pressed (Input.KeyChar 'w')] 19) in
  check (not (Rays_editor.Editor.flying environment))
    "inspector child press did not focus its pane";
  (match Sys.getenv_opt "RAYS_UI_PREVIEW" with
   | Some directory -> Result.get_ok(Sketch.export ~directory ~prefix:"workspace-render"
       ~frames:1 ~config:{ Sketch.default_config with width=900; height=640 }
       (Rays_editor.Editor.scene environment))
   | None -> ());
  let box path = match Rays_editor.Editor.node_box environment path with
    | Some rect -> rect | None -> fail "the node has no box in the graph pane" in
  (* the header: the middle of the card is a row (or the [+ N more] row, which opens it) *)
  let point = (let x, y, w, _ = box [ "geo1"; "source" ] in x + w / 2, y + 12) in
  let selection_frame = frame ~mouse:point ~events:[
      mouse_press (Input.LeftButton, point);
      mouse_release (Input.LeftButton, point)] 20 in
  let environment = Rays_editor.Editor.update environment selection_frame in
  let scope_line environment = report_line Rays_editor.Editor.crash_dump environment "scope selected" in
  check (scope_line environment = "scope selected: geo1/source")
    "graph selection did not select the clicked node";
  (match Sys.getenv_opt "RAYS_UI_PREVIEW" with
   | Some directory -> Result.get_ok(Sketch.export ~directory ~prefix:"workspace-inspector"
       ~frames:1 ~config:{ Sketch.default_config with width=900; height=640 }
       (Rays_editor.Editor.scene environment))
   | None -> ());
  let environment = Rays_editor.Editor.update environment
      (frame ~events:[Input.Key_pressed Input.Home] 20) in
  let environment = Rays_editor.Editor.update environment (frame 20) in
  let camera_frame = frame ~events:[Input.Key_pressed (Input.KeyChar '/');
      Input.Key_pressed (Input.KeyChar 'c')] 21 in
  let environment = Rays_editor.Editor.update environment camera_frame in
  check (scope_line environment = "scope selected: -")
    "/ c did not restore the camera/render inspector";
  let box environment path = match Rays_editor.Editor.node_box environment path with
    | Some rect -> rect | None -> fail "the node has no box in the graph pane" in
  (* Shared undo stack: an added node is one entry; Command-Z removes it,
     Shift-Command-Z brings it back. *)
  let source_point = card_row (box environment [ "geo1"; "source" ]) in
  let environment = Rays_editor.Editor.update environment (frame ~mouse:source_point ~events:[
      mouse_press (Input.LeftButton, source_point);
      mouse_release (Input.LeftButton, source_point)] 23) in
  let chord ?(shift = false) key count =
    let frame = frame ~events:[Input.Key_pressed (Input.KeyChar key)] count in
    { frame with input = { frame.input with
        keys = Input.Meta :: (if shift then [Input.Shift] else []) } } in
  let environment = Rays_editor.Editor.update environment
      (frame ~mouse:source_point ~events:[Input.Key_pressed (Input.KeyChar '/');
        Input.Key_pressed (Input.KeyChar 'a')] 24) in
  let environment = Rays_editor.Editor.update environment
      (frame ~mouse:source_point ~events:[Input.Text_input "null"; Input.Key_pressed Input.Enter] 25) in
  check (has (text_of environment) "(sop/null source)")
    "the leader add-node menu did not add a node wired to the selection";
  check (Rays_editor.Editor.can_undo environment) "the added node did not enter the undo stack";
  let environment = Rays_editor.Editor.update environment (chord 'z' 26) in
  check (not (has (text_of environment) "sop/null")) "Command-Z did not undo the added node";
  check (Rays_editor.Editor.can_redo environment) "undo did not leave a redo entry";
  let environment = Rays_editor.Editor.update environment (chord ~shift:true 'z' 27) in
  check (has (text_of environment) "(sop/null source)") "Shift-Command-Z did not redo the added node";
  let environment = Rays_editor.Editor.update environment (chord 'z' 28) in
  check (not (has (text_of environment) "sop/null")) "second undo failed after redo";
  (* Tile positions are document state: one drag is one undo entry. *)
  let tile_x environment path = let x, _, _, _ = box environment path in x in
  let x0 = tile_x environment [ "geo1"; "source" ] and sx, sy = source_point in
  let environment = List.fold_left (fun environment (count, mouse, events) ->
      let frame = frame ~mouse ~events count in
      Rays_editor.Editor.update environment { frame with input = { frame.input with
        buttons = if count > 30 then [Input.LeftButton] else [] } })
    environment [
      30, (sx, sy), [];  (* hover first: hit testing uses the last frame *)
      31, (sx, sy), [mouse_press (Input.LeftButton, (sx, sy))];
      32, (sx + 20, sy), [mouse_move (sx + 20, sy)];
      33, (sx + 40, sy), [mouse_move (sx + 40, sy)]] in
  let environment = Rays_editor.Editor.update environment
      (frame ~mouse:(sx + 40, sy)
         ~events:[mouse_release (Input.LeftButton, (sx + 40, sy))] 34) in
  check (tile_x environment [ "geo1"; "source" ] <> x0) "dragging a node did not move it";
  let environment = Rays_editor.Editor.update environment (chord 'z' 35) in
  let environment = Rays_editor.Editor.update environment (frame 36) in  (* the pane lays out again *)
  check (tile_x environment [ "geo1"; "source" ] = x0)
    "undo did not restore the dragged node position";
  (* A multi-node drag moves every selected node, and undo and redo restore every one. *)
  let paths = [ [ "geo1"; "source" ]; [ "geo1"; "output" ] ] in
  let xs env = List.map (tile_x env) paths in
  let xs0 = xs environment in
  let ox, oy = card_header (box environment [ "geo1"; "output" ]) in
  let held = [Input.LeftButton] in
  let environment = List.fold_left (fun environment (count, mouse, buttons, keys, events) ->
      let frame = frame ~mouse ~events count in
      Rays_editor.Editor.update environment { frame with input = { frame.input with buttons; keys } })
    environment [
      (* well past the double-click interval of the earlier presses on these nodes *)
      140, (ox, oy), [], [], [];
      141, (ox, oy), held, [Input.Shift], [mouse_press (Input.LeftButton, (ox, oy))];
      142, (ox, oy), [], [Input.Shift], [mouse_release (Input.LeftButton, (ox, oy))];
      143, (sx, sy), [], [], [];
      144, (sx, sy), held, [], [mouse_press (Input.LeftButton, (sx, sy))];
      145, (sx + 30, sy), held, [], [mouse_move (sx + 30, sy)];
      146, (sx + 30, sy), [], [], [mouse_release (Input.LeftButton, (sx + 30, sy))]] in
  let xs1 = xs environment in
  check (List.for_all2 (fun a b -> a <> b) xs0 xs1)
    "dragging a multi-node selection did not move every node";
  let environment = Rays_editor.Editor.update environment (chord 'z' 147) in
  let environment = Rays_editor.Editor.update environment (frame 148) in
  check (xs environment = xs0) "undo did not restore every dragged node";
  let environment = Rays_editor.Editor.update environment (chord ~shift:true 'z' 149) in
  let environment = Rays_editor.Editor.update environment (frame 150) in
  check (xs environment = xs1) "redo did not restore every dragged node";
  let environment = Rays_editor.Editor.update environment (chord 'z' 151) in
  (* / c clears the selection the drag made, as before this check. *)
  let environment = Rays_editor.Editor.update environment (frame ~events:[
      Input.Key_pressed (Input.KeyChar '/'); Input.Key_pressed (Input.KeyChar 'c')] 152) in
  let deadline = Unix.gettimeofday () +. 60. in
  let rec wait_source count environment =
    let environment = Rays_editor.Editor.update environment (frame count) in
    match Option.bind (Rays_editor.Editor.prepared environment)
        Editor_prepare.centroid with
    | Some center when abs_float (center.Rays_math.Vec3.x -. 5.) < 1. -> environment
    | _ when Unix.gettimeofday () < deadline ->
        Unix.sleepf 0.001;
        wait_source (count + 1) environment
    | _ -> fail "display-node cook did not publish the graph's result"
  in
  let environment = wait_source 153 environment in
  check (Rays_editor.Editor.scene environment current_frame <> [])
    "sketch environment produced an empty composed scene";
  (* The whole workspace paints in batch groups, not one draw per label. *)
  if !native then begin
    let batches = match Scene.Layer.stage_native_render ~width:900 ~height:640
        (Rays_editor.Editor.scene environment current_frame) with
      | Ok staged -> List.fold_left (fun total -> function
          | Scene.Layer.Ui_layer (batch, _) ->
              total + Array.length (Scene_command.Ui_batch.batches batch)
          | _ -> total) 0 staged.layers
      | Error message -> fail message in
    Printf.printf "workspace UI batches: %d\n" batches;
    (* Header drag/disclosure shapes and the common renderer controls add groups;
       retain a fixed draw budget for the complete workspace. *)
    check (batches > 0 && batches <= 32) "workspace UI draws were not batched"
  end;
  let _, _, graph_width, graph_height =
    (Rays_editor.Editor.panes environment (frame 229)).graph in
  let graph_x, graph_y, _, _ =
    (Rays_editor.Editor.panes environment (frame 229)).graph in
  let menu_point = graph_x + (graph_width / 2), graph_y + (graph_height / 2) in
  let add environment count =
    let environment = Rays_editor.Editor.update environment
        (frame ~mouse:menu_point ~events:[Input.Key_pressed (Input.KeyChar '/');
          Input.Key_pressed (Input.KeyChar 'a')] count) in
    Rays_editor.Editor.update environment
      (frame ~mouse:menu_point ~events:[Input.Text_input "null";
        Input.Key_pressed Input.Enter] (count + 1)) in
  let environment = add environment 230 in
  check (has (text_of environment) "(sop/null output)"
      && Node.operation (Rays_editor.Editor.displayed_node environment) = "transform")
    ("workspace could not add a node reading the result without moving the display: "
     ^ Node.operation (Rays_editor.Editor.displayed_node environment) ^ "\n" ^ text_of environment);
  check (scope_line environment = "scope selected: geo1/null")
    "the added node was not selected";
  let environment = Rays_editor.Editor.update environment
      (frame ~events:[Input.Key_pressed Input.Backspace] 232) in
  check (not (has (text_of environment) "sop/null")) "Backspace did not delete the added node";
  let environment = add environment 234 in
  let environment = Rays_editor.Editor.update environment
      (frame ~events:[Input.Key_pressed (Input.KeyChar '/');
        Input.Key_pressed (Input.KeyChar 'g')] 236) in
  let environment = Rays_editor.Editor.update environment
      (frame ~events:[Input.Key_pressed Input.Backspace] 237) in
  check (has (text_of environment) "sop/null")
    "hidden graph still accepted a delete shortcut";
  let environment = Rays_editor.Editor.update environment
      (frame ~events:[Input.Key_pressed (Input.KeyChar '/');
        Input.Key_pressed (Input.KeyChar 'g')] 238) in
  (* / t shows the timeline bar; dragging its scrub slider seeks and
     pauses the shared clock. *)
  let environment = Rays_editor.Editor.update environment
      (frame ~events:[Input.Key_pressed (Input.KeyChar '/');
        Input.Key_pressed (Input.KeyChar 't')] 240) in
  let tx, ty, tw, th = (Rays_editor.Editor.panes environment (frame 241)).timeline in
  check (th > 0 && tw = 900) "/ t did not show the full-width timeline bar";
  let environment = Rays_editor.Editor.update environment (frame 241) in
  let scrub_y = ty + (th / 2) and scrub_x = tx + tw - 60 in
  let environment = Rays_editor.Editor.update environment
      (frame ~events:[mouse_press (Input.LeftButton, (scrub_x, scrub_y))] 242) in
  let environment = Rays_editor.Editor.update environment
      (frame ~events:[mouse_move (tx + tw - 10, scrub_y);
        mouse_release (Input.LeftButton, (tx + tw - 10, scrub_y))] 243) in
  let clock = Rays_editor.Editor.timeline environment in
  check (Rays_editor.Timeline.mode clock = Rays_editor.Timeline.Paused
      && Rays_editor.Timeline.frame clock >= 200L)
    "dragging the timeline scrub did not seek and pause";
  let environment = Rays_editor.Editor.update environment
      (frame ~events:[Input.Key_pressed (Input.KeyChar '/')] 244) in
  (match Sys.getenv_opt "RAYS_UI_PREVIEW" with
   | Some directory -> Result.get_ok(Sketch.export ~directory ~prefix:"workspace-leader"
       ~frames:1 ~config:{ Sketch.default_config with width=900; height=640 }
       (Rays_editor.Editor.scene environment))
   | None -> ());
  let environment = Rays_editor.Editor.update environment
      (frame ~events:[Input.Key_pressed Input.Escape] 245) in
  Rays_editor.Editor.close environment;
  camera_nodes ();
  sketch_settings ();
  preset_and_look_through ();
  native_smoke ();
  print_endline (if !native then "sketch ui tests passed"
    else "sketch ui logic tests passed (window-free)")

let run_logic () = native := false; run ()
