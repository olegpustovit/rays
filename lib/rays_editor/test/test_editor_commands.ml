open Rays
module Command = Editor_core.Command
module Keymap = Editor_core.Keymap
module Settings = Rays_editor.Settings

let check condition message = if not condition then failwith message
let key k = Input.Key_pressed k
let char c = key (Input.KeyChar c)
let schema = Param.(schema ~name:"command-value" ~default:0
  [field ~name:"value" ~label:"Value" ~kind:(integer ~min:0 ~max:100 ())
    ~default:0 ~get:Fun.id ~set:(fun value _ -> value) ()])

(* v on a drawing or image node shows it in the canvas panes; v again restores their pictures *)
let canvas_preview () =
  let module E3 = Rays_editor.Editor in
  let text = {|(workspace v
    (graph image :context image
      (image/map (fn [uv] [uv.x uv.y 0.5 1]) :width 64 :height 8))
    (graph picture :context draw
      (draw/merge (draw/background "#0b0b0e") (draw/image (ref image) :at [0 0 0])))
    (graph settings :context settings (settings/config :title "v" :width 900 :height 640))
    (graph editor :context editor
      (let* [preview (ui/canvas (ref picture))
             network (ui/graph "image" :focus true)]
        (ui/workspace (ui/split "vertical" preview network :second_size 264)))))|} in
  let workspace = match Rays_editor.Workspace.load text with
    | Ok workspace -> workspace
    | Error ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds)) in
  let editor = Result.get_ok (E3.create ~ui_text_size:11 ~workspace ~await:true ~domains:1 ~prepare:Editor_prepare.empty
    ~draw:(fun _ _ -> Rays.Scene.D3.empty) ()) in
  Fun.protect ~finally:(fun () -> E3.close editor) (fun () ->
    let count = ref 0 in
    let step e = incr count; E3.update e (Test_editor_input.frame (450., 300.) [] !count) in
    (* the widths of the images a picture draws: the default draws its image at 64, the preview fits the pane *)
    let widths scene = Array.to_list (Scene.Private.commands scene) |> List.filter_map (function
      | Scene_command.Render_ir.Image i -> Some i.destination.width | _ -> None) in
    let e = ref (step (step (step editor))) in
    let shown () = List.map widths (E3.Private.canvas_scenes !e) in
    let default = shown () in
    check (default <> [] && List.for_all (fun w -> w = [ 64. ]) default) "the default picture is not the 64 point image";
    e := step (Rays_editor.Reduce.select_path !e [ "image"; "@result" ]);
    e := step (Rays_editor.Reduce.view !e [ "image"; "@result" ]);
    e := step !e;
    check (List.length (shown ()) = List.length default && List.for_all (function [ w ] -> w > 64. | _ -> false) (shown ()))
      "v on an image node did not show exactly its fitted image in the canvas pane";
    e := step (Rays_editor.Reduce.view !e [ "image"; "@result" ]);
    e := step !e;
    check (shown () = default) "v again did not restore the canvas pane's own picture";
    e := step (Rays_editor.Reduce.select_path !e [ "picture"; "@result#0" ]);
    e := step (Rays_editor.Reduce.view !e [ "picture"; "@result#0" ]);
    check (List.for_all ((=) []) (shown ()))
      "View on an inline background did not replace the canvas picture";
    e := step (Rays_editor.Reduce.view !e [ "picture"; "@result#0" ]);
    check (shown () = default) "View on the inline background again did not restore the picture");
  ()

(* frame/width and frame/height in a canvas pane are the pane's size, not the window's *)
let canvas_frame_size () =
  let module E3 = Rays_editor.Editor in
  let text = {|(workspace f
    (graph picture :context draw
      (draw/image (image/map (fn [uv] [uv.x uv.y 0.5 1]) :width (frame/width) :height (frame/height))
        :at [0 0 0]))
    (graph settings :context settings (settings/config :title "f" :width 900 :height 640))
    (graph editor :context editor
      (let* [preview (ui/canvas (ref picture))
             network (ui/graph "picture" :focus true)]
        (ui/workspace (ui/split "horizontal" preview network :second_size 500)))))|} in
  let workspace = match Rays_editor.Workspace.load text with
    | Ok workspace -> workspace
    | Error ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds)) in
  let editor = Result.get_ok (E3.create ~ui_text_size:11 ~workspace ~await:true ~domains:1 ~prepare:Editor_prepare.empty
    ~draw:(fun _ _ -> Rays.Scene.D3.empty) ()) in
  Fun.protect ~finally:(fun () -> E3.close editor) (fun () ->
    let step e count = E3.update e (Test_editor_input.frame (450., 300.) [] count) in
    let e = step (step (step editor 1) 2) 3 in
    let sizes = List.concat_map (fun scene -> Array.to_list (Scene.Private.commands scene)
      |> List.filter_map (function
        | Scene_command.Render_ir.Image i -> Some (i.destination.width, i.destination.height) | _ -> None))
      (E3.Private.canvas_scenes e) in
    check (match sizes with [ w, h ] -> w > 0. && w < 500. && h > 0. && h < 640. | _ -> false)
      ("frame/width and frame/height in a canvas pane are not the pane's size: "
       ^ String.concat ", " (List.map (fun (w, h) -> Printf.sprintf "%.0fx%.0f" w h) sizes)))

(* every language form of the add menu starts as an expression that checks *)
let menu_forms () =
  let forms = Rays_editor.Editor.Private.menu_forms in
  List.iter (fun form -> check (List.mem_assoc form forms) ("the add menu has no " ^ form))
    [ "if"; "cond"; "case"; "for"; "fold"; "scan"; "sum"; "fn"; "map"; "filter"; "reduce"; "sort-by";
      "list"; "record"; "let*"; "state" ];
  List.iter (fun (form, text) ->
    let source = Printf.sprintf "(workspace m (graph g :context value (let* [added %s] 0)))" text in
    match Rays_editor.Workspace.load source with
    | Ok _ -> ()
    | Error ds -> failwith (form ^ ": " ^ String.concat "; " (List.map Diagnostic.to_string ds))) forms

let has text piece =
  let n = String.length piece in
  let rec at i = i + n <= String.length text && (String.sub text i n = piece || at (i + 1)) in
  at 0

let with_editor text f =
  let module E3 = Rays_editor.Editor in
  let workspace = match Rays_editor.Workspace.load text with
    | Ok workspace -> workspace
    | Error ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds)) in
  let editor = Result.get_ok (E3.create ~ui_text_size:11 ~workspace ~await:true ~domains:1 ~prepare:Editor_prepare.empty
    ~draw:(fun _ _ -> Rays.Scene.D3.empty) ()) in
  Fun.protect ~finally:(fun () -> E3.close editor) (fun () -> f editor)

let world_command () =
  let module E3 = Rays_editor.Editor in
  let module L = Rays_editor.Private.Editor_commands in
  with_editor {|(workspace world_command
    (graph g :context sop (sop/box))
    (graph scene :context scene (scene/root (scene/merge (scene/geometry (ref g))))))|}
    (fun editor ->
      let editor = E3.update editor (Test_editor_input.frame (450., 300.) [] 0) in
      let before = E3.workspace editor in
      let step editor actions count = Rays_editor.Reduce.step editor actions
          (Test_editor_input.frame (450., 300.) [] count) in
      let created = step editor [L.Go_world] 1 in
      let source = Editor_document.Workspace_doc.to_text (E3.workspace created) in
      check (has source "(scene/world (ref sky))" && has source "(graph sky :context world"
        && has source "(world/sun (world/sky))" && E3.undo_label created = Some "Add World")
        "/ e did not write the World and its graph as one Add World entry";
      let repeated = step created [L.Go_world] 2 in
      check (E3.workspace repeated == E3.workspace created)
        "opening an existing World rewrote the document";
      let undone = step repeated [L.Undo] 3 in
      check (E3.workspace undone == before) "one undo did not restore the World-free document")

(* A Shift-drag in the view rotates the World: the press latches the operation, the moves edit
   the text, the release seals; the whole drag is one undo step. *)
let world_drag () =
  let module E3 = Rays_editor.Editor in
  let module L = Rays_editor.Private.Editor_commands in
  with_editor {|(workspace world_drag
    (graph g :context sop (sop/box))
    (graph sky :context world (world/sun (world/sky)))
    (graph scene :context scene (scene/root (scene/merge (scene/geometry (ref g)) (scene/world (ref sky))))))|}
    (fun editor ->
      let count = ref 0 in
      let editor = ref (E3.update editor (Test_editor_input.frame (450., 300.) [] 0)) in
      let step ?(keys = []) ?(buttons = []) mouse events =
        incr count; editor := E3.update !editor (Test_editor_input.frame ~keys ~buttons mouse events !count) in
      let text () = Editor_document.Workspace_doc.to_text (E3.workspace !editor) in
      let before = text () in
      let x, y, w, h = (E3.panes !editor (Test_editor_input.frame (450., 300.) [] 0)).Pxui_shell.Layout.view in
      let p = float (x + w / 2), float (y + h / 2) in
      let at dx = fst p +. dx, snd p in
      let shift = [Input.Shift] and left = [Input.LeftButton] in
      step ~keys:shift (at 0.) [Input.Pointer_moved (at 0.)];
      step ~keys:shift ~buttons:left (at 0.) [Input.Pointer_pressed (Input.LeftButton, at 0.)];
      step ~keys:shift ~buttons:left (at 20.) [Input.Pointer_moved (at 20.)];
      step ~keys:shift ~buttons:left (at 40.) [Input.Pointer_moved (at 40.)];
      step ~keys:shift (at 40.) [Input.Pointer_released (Input.LeftButton, at 40.)];
      check (text () <> before) "a Shift-drag in the view did not rotate the World";
      check (E3.undo_label !editor = Some "Rotate World") "the drag's undo entry is not Rotate World";
      let undone = Rays_editor.Reduce.step !editor [L.Undo] (Test_editor_input.frame (450., 300.) [] (!count + 1)) in
      check (Editor_document.Workspace_doc.to_text (E3.workspace undone) = before)
        "one undo did not undo the whole World drag")

(* an image-context workspace is not a geometry object: the cook has nothing to reject *)
let image_cook () =
  let module E3 = Rays_editor.Editor in
  let text = {|(workspace k
    (graph image :context image
      (let* [bias (* 0.2 (+ 1 (sin t)))
             pixels (image/map (fn [uv] [(+ uv.x bias) uv.y 0.5 1]) :width 16 :height 16)]
        pixels))
    (graph picture :context draw (draw/image (ref image) :at [0 0 0]))
    (graph settings :context settings (settings/config :title "k" :width 900 :height 600)))|} in
  with_editor text (fun editor ->
    let e = ref editor in
    for count = 1 to 2 do e := E3.update !e (Test_editor_input.frame (450., 300.) [] count) done;
    let status = E3.Private.status_text !e in
    check (not (has status "Cook rejected") && not (has status "E_PAYLOAD"))
      ("an image graph made the cook reject: " ^ status))

(* New graph: each context's default body checks, a scene reads the SOP graph, and Delete refuses
   a graph that is read *)
let new_graphs () =
  let module E3 = Rays_editor.Editor in
  let text = {|(workspace n
    (graph settings :context settings (settings/config :title "n" :width 900 :height 600)))|} in
  let contexts = [ "sop"; "scene"; "draw"; "image"; "value"; "material" ] in
  with_editor text (fun editor ->
    let names e = List.map (fun (g : Flow.Workspace.graph) -> g.name)
      (E3.workspace e).Editor_document.Workspace_doc.checked.graphs in
    let add e context = match E3.edit e (E3.Private.new_graph e context) with
      | Ok e -> e | Error m -> failwith ("New graph " ^ context ^ " refused: " ^ m) in
    let e = List.fold_left add editor contexts in
    List.iter (fun n -> check (List.mem n (names e)) ("New graph did not make " ^ n)) contexts;
    check (List.mem "sop_2" (names (add e "sop"))) "a second New graph sop is not named afresh";
    let source = fst (Flow.Lisp.print (E3.workspace e).Editor_document.Workspace_doc.source) in
    check (has source "(ref sop)") "the new scene does not read the SOP graph";
    let e = ref e in
    for count = 1 to 3 do e := E3.update !e (Test_editor_input.frame (450., 300.) [] count) done;
    check (not (has (E3.Private.status_text !e) "rejected"))
      ("the new graphs do not cook: " ^ E3.Private.status_text !e);
    (match E3.edit !e (Flow_graph.Flow_edit.Remove_graph { name = "sop" }) with
     | Ok _ -> failwith "a read SOP graph was deleted"
     | Error m -> check (has m "scene") ("the refusal does not name the scene: " ^ m));
    match E3.edit !e (Flow_graph.Flow_edit.Remove_graph { name = "draw" }) with
    | Ok removed -> check (not (List.mem "draw" (names removed))) "an unread graph stayed"
    | Error m -> failwith ("an unread graph was refused: " ^ m))

(* Every chord the key sheet lists as the text area's own must change the text, selection or caret of a
   real [Ui.text_area_submit] with the editor's Lisp language: the sheet cannot list a key nothing handles. *)
let text_keys_are_handled () =
  let module L = Rays_editor.Private.Editor_commands in
  let language = Rays_editor.Private.Lisp_text.language Pxui.Theme.default in
  check (L.text_keys <> []) "no text keys";
  List.iter (fun (label, trigger) ->
    let key, mods = match trigger with
      | Editor_core.Keymap.Chord (k, m) -> k, m | _ -> failwith "text key without a chord" in
    let ui = Pxui.Ui.create ~font_size:11 () in
    Fun.protect ~finally:(fun () -> Pxui.Ui.destroy ui) (fun () ->
      let text = ref "(a (b c))\n(d (e))" and sel = ref (0, 0) and caret = ref 0 and time = ref 0. in
      let step ?(keys = []) events =
        time := !time +. 0.5;
        let frame = Test_editor_input.frame (0., 0.) events 0 in
        ignore (Pxui.Ui.frame ui { frame with input = { frame.input with keys } } (fun ui ->
          let t, _ = Pxui.Ui.text_area_submit ui ~at:(0., 0.) ~w:300. ~h:96. ~language
            ~on_select:(fun r -> sel := r) ~on_caret:(fun c -> caret := c) "area" !text in
          text := t)) in
      step [];
      step [Input.Pointer_pressed (Input.LeftButton, (60., 31.));
        Input.Pointer_released (Input.LeftButton, (60., 31.))];
      let before = !text, !sel, !caret in
      step ~keys:mods [Input.Key_pressed key];
      check (before <> (!text, !sel, !caret))
        (Printf.sprintf "key sheet lists %s but the text area ignores it" label))) L.text_keys

let run () =
  text_keys_are_handled ();
  let open Editor_core.Guide_context in
  let module L = Rays_editor.Private.Editor_commands in
  List.iter (fun focus ->
    List.iter (fun (events, expected) ->
      let _, actions, _ = Editor_core.Router.step L.keymap ~focus ~text_focus:false
        ~frame:(Test_editor_input.frame (500.,400.) events 1) Idle in
      check (List.map (fun (c : _ Command.t) -> c.action) actions = [expected]) "guide key failed outside or inside graph focus")
      [key Input.Shift :: [char '/'], L.Guide_toggle;
       [char '?'], L.Guide_toggle; [key (Input.KeyChar '/'); char '?'], L.Guide_keys])
    Pxui_shell.Layout.[View ""; View "v1.0"; Graph; List; Lisp; Inspector; Outline; Timeline];
  (* Tab is the graph's own key for the add menu ("Tab add after" on the strip), and no other pane's *)
  List.iter (fun (focus, expected) ->
    let _, actions, _ = Editor_core.Router.step L.keymap ~focus ~text_focus:false
      ~frame:(Test_editor_input.frame (500.,400.) [key Input.Tab] 1) Idle in
    check (List.map (fun (c : _ Command.t) -> c.action) actions = expected) "Tab is not the graph's add key alone")
    Pxui_shell.Layout.[Graph, [L.Add_node]; View "", []; Outline, []; Inspector, []];
  check (Keymap.label (Chord (Input.KeyChar '/', [Input.Shift])) = "?"
      && Keymap.label (Chord (Input.ArrowLeft, [])) = "←"
      && Keymap.label (Chord (Input.KeyChar 'z', [Input.Shift; Input.Meta])) = "⌘⇧Z")
    "shared key labels lost a modifier or alias";
  let module Scope = Pxui_graph.Scope in
  let ids context = Command.for_guide Scope.bindings ~focus:() ~context
    |> List.map (fun (c : _ Command.t) -> c.id) in
  (* the strip lists a key where it does something: walking and framing on the empty canvas;
     editing needs a selection; duplicate and delete take several nodes, rename and view one *)
  check (List.mem "scope.walk.left" (ids Canvas) && List.mem "scope.frame-all" (ids Canvas)
         && not (List.mem "scope.delete" (ids Canvas)) && not (List.mem "scope.duplicate" (ids Canvas))
         && List.mem "scope.delete" (ids Node) && List.mem "scope.rename" (ids Node)
         && List.mem "scope.display" (ids Node) && List.mem "scope.fold" (ids Node)
         && List.mem "scope.duplicate" (ids Multi) && List.mem "scope.repeat" (ids Multi)
         && not (List.mem "scope.display" (ids Multi)) && not (List.mem "scope.fold" (ids Multi))
         && ids Search = [] && ids Text = [] && ids List = [] && ids Leader = [] && ids Hints = [])
    "the workspace pane's guide lost or misplaced a key for its contexts";
  check (List.for_all (fun (c : _ Command.t) -> c.guide <> []) Scope.bindings)
    "a workspace pane key is listed in no guide context";
  let host_ids focus context = Command.for_guide L.keymap ~focus ~context
    |> List.map (fun (c : _ Command.t) -> c.id) in
  check (List.mem "guide.toggle" (host_ids (Pxui_shell.Layout.View "") Canvas)
      && not (List.mem "graph.add" (host_ids (Pxui_shell.Layout.View "") Canvas))
      && List.mem "panel.list" (host_ids Pxui_shell.Layout.Graph Leader)
      && List.mem "preset.save" (host_ids Pxui_shell.Layout.Graph Leader))
    "host guide lost a context or ignored command scope";
  (* Enter needs a selected object, up a level does not; the list has both *)
  check (List.mem "scene.enter" (host_ids Pxui_shell.Layout.Graph Node)
         && not (List.mem "scene.enter" (host_ids Pxui_shell.Layout.Graph Canvas))
         && List.mem "scene.up" (host_ids Pxui_shell.Layout.Graph Canvas)
         && List.mem "scene.enter" (host_ids Pxui_shell.Layout.Graph List)
         && List.mem "guide.keys" (host_ids Pxui_shell.Layout.Graph Hints)
         && List.mem "list.up" (host_ids Pxui_shell.Layout.Graph List)
         && not (List.mem "list.up" (host_ids Pxui_shell.Layout.Graph Canvas)))
    "the host guide contexts changed";
  (* the list walks with j and k as with the arrows *)
  let list_step key_char expected =
    let _, actions, _ = Editor_core.Router.step L.keymap ~focus:Pxui_shell.Layout.Graph ~text_focus:false
      ~frame:(Test_editor_input.frame (500., 400.) [ char key_char ] 1) Idle in
    List.exists (fun (c : _ Command.t) -> c.action = L.List_command expected) actions in
  check (list_step 'j' Pxui_shell.Tree.Down && list_step 'k' Pxui_shell.Tree.Up) "j and k do not walk the list";
  (* focus is a panel; commands are scoped by its kind: every viewport is one scope, and the
     list and lisp panels are graph-pane projections *)
  let module P = Pxui_shell.Layout in
  check (List.for_all (fun panel -> L.scope panel = P.Graph) [P.Graph; P.List; P.Lisp]
      && L.scope (P.View "v1.2") = P.View "" && L.scope (P.View "main") = L.scope (P.View "v0")
      && L.scope P.Inspector = P.Inspector && L.scope P.Outline = P.Outline)
    "panel kinds do not map to the scopes commands use";
  check (host_ids (L.scope P.Lisp) Canvas = host_ids P.Graph Canvas
      && host_ids (L.scope (P.View "v3")) Canvas = host_ids (P.View "") Canvas
      && host_ids (L.scope P.Outline) Canvas <> host_ids P.Graph Canvas)
    "a lisp or second viewport panel routed to the wrong scope";
  let table = List.map (fun (command : _ Command.t) ->
    {command with scope = Some Pxui_shell.Layout.Graph}) Scope.bindings in
  List.iter (fun (pressed, modifiers, expected) ->
    let input = Test_editor_input.frame ~keys:[] (500.,400.)
      (List.map key modifiers @ [key pressed]) 1 in
    let route focus = let _, actions, _ = Editor_core.Router.step table ~focus
      ~text_focus:false ~frame:input Idle in List.map (fun (c : _ Command.t) -> c.action) actions in
    check (route Pxui_shell.Layout.Graph = [expected]) "a graph pane key routed the wrong action";
    check (route (Pxui_shell.Layout.View "") = [] && route Pxui_shell.Layout.Inspector = [])
      "a graph pane key escaped graph scope")
    Scope.[Input.ArrowLeft, [], Walk Left; Input.ArrowDown, [], Walk Down;
      Input.ArrowUp, [], Walk Up; Input.ArrowRight, [], Walk Right;
      Input.KeyChar 'x', [], Delete; Input.Delete, [], Delete; Input.Backspace, [], Delete;
      Input.KeyChar 'b', [], Bypass; Input.KeyChar 'r', [], Wrap_repeat;
      Input.KeyChar 'r', [Input.Shift], Wrap_iterate; Input.KeyChar 'c', [], Collapse;
      Input.KeyChar 'm', [], Make_macro; Input.Home, [], Frame_all];
  let workspace = Ws_fixture.box () in
  let module E3 = Rays_editor.Editor in
  let directory = Filename.temp_dir "rays-guide" "" in
  let filename = Filename.concat directory "preferences.rays" in
  let create () = E3.create ~ui_text_size:11 ~preferences:filename ~workspace ~prepare:Editor_prepare.empty ~draw:(fun _ _ -> Scene.D3.empty) ()
    |> Result.get_ok in
  let current = ref (create ()) in
  Fun.protect ~finally:(fun () ->
    E3.close !current;
    Array.iter (fun file -> Sys.remove (Filename.concat directory file)) (Sys.readdir directory);
    Unix.rmdir directory) (fun () ->
    let step events count = current := E3.update !current
      (Test_editor_input.frame (100.,300.) events count) in
    let enabled () = Editor_core.Store.Settings.load ~sketch:"rays-editor" filename
      |> Result.get_ok |> fun values -> Editor_core.Store.Settings.bool values "guide" in
    step [] 0; step [char '?'] 1;
    check (enabled () = Some false) "guide did not default on and persist off";
    check (not (E3.can_undo !current)) "guide preference entered document history";
    E3.close !current; current := create (); step [] 2; step [char '?'] 3;
    check (enabled () = Some true) "a new host did not load the saved guide preference";
    (* A sheet owns input until shared modal dismissal, then shortcuts resume. *)
    step [key (Input.KeyChar '/'); char '?'] 4; step [char '?'] 5;
    check (enabled () = Some true) "guide key escaped the key-sheet modal";
    step [key Input.Escape] 6; step [char '?'] 7;
    check (enabled () = Some false) "key-sheet dismissal kept keyboard focus";
    (* Toggle preserves unrelated preferences, and Hide follows the same save path. *)
    Editor_core.Store.Settings.save ~sketch:"rays-editor" filename
      ["guide", Bool false; "other", Int 7] |> Result.get_ok;
    step [key Input.Shift; char '/'; Input.Key_released Input.Shift] 8;
    (* the strip's keys are the sheets' own and carry no "toggle guide" pair: the key is the control,
       and it saves through the same path, keeping the other preferences *)
    step [char '?'] 9;
    let values = Editor_core.Store.Settings.load ~sketch:"rays-editor" filename |> Result.get_ok in
    check (Editor_core.Store.Settings.bool values "guide" = Some false
      && Editor_core.Store.Settings.int values "other" = Some 7)
      "Hide did not save off or discarded another preference";
    let contents = "invalid saved preferences" in
    Out_channel.with_open_bin filename (fun out -> output_string out contents);
    step [char '?'] 10;
    check (In_channel.with_open_bin filename In_channel.input_all = contents)
      "guide toggle overwrote an unreadable preference file";
    E3.crash_dump !current directory;
    let dump () = In_channel.with_open_bin (Filename.concat directory "editor.txt")
      In_channel.input_all in
    let contains text piece = let n = String.length piece in
      List.exists (fun i -> String.sub text i n = piece)
        (List.init (max 0 (String.length text-n+1)) Fun.id) in
    check (contains (dump ()) "key hud: ? · toggle guide\n")
      "key HUD did not use the routed command label";
    step [key Input.Ctrl; char 'z'; Input.Key_released Input.Ctrl] 13;
    E3.crash_dump !current directory;
    check (contains (dump ()) "key hud: ⌃Z · undo\n")
      "key HUD displayed a different alias from the routed chord";
    step [] 200; E3.crash_dump !current directory;
    check (contains (dump ()) "key hud: -\n") "key HUD outlived 1.5 seconds");
  canvas_preview ();
  menu_forms ();
  image_cook ();
  new_graphs ();
  world_command ();
  world_drag ();
  print_endline "editor commands: the host guide and key routing"
