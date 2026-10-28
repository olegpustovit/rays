open Rays

open Editor_core.Keymap
type action =
  | Save_preset | Browse_presets
  | Save_source  (* Command-S: rewrite the sketch's .rays, else a preset *)
  | Reload_source  (* the palette: take the source file's text over unsaved edits *)
  | Open_source  (* Command-O: choose a .rays in the system dialog and open it in this window *)
  | Toggle_timeline | Toggle_graph | Toggle_inspector | Hide_ui | Open_camera
  | Play_pause | Reset | Stop
  | Add_node
  | Frame_tile | Frame_camera
  | Look_through | Look_through_camera | Fly | Tool of int  (* 0 none, 1 translate, 2 rotate, 3 scale *)
  | Render_mode of int  (* the viewport header's tabs: 0 solid, 1 wire, 2 traced *)
  | Undo | Redo
  | Panel_split of Pxui_shell.Layout.axis | Panel_close | Panel_retype of Pxui_shell.Layout.panel
      (* / o ...: the focused panel, as the header menu does *)
  | Panel_pick | Window_pick
      (* x, n: a menu of panel kinds at the pointer; 1-8 or a click picks, then x places a split, n opens a window *)
  | Panel_split_at of Pxui_shell.Layout.path * Pxui_shell.Layout.axis * float * Pxui_shell.Layout.panel
      (* the pending split, committed where the pointer clicked *)
  | Toggle_map  (* / m: in the World, the view pane flips to the lat-long map *)
  | Ui_scale of int  (* Command +/-/0: the kit text of every panel but the graph and viewports *)
  | Restore_layout | Enter | Up | Go_world
  | Back of int  (* Esc, or a click on an earlier part of the graph header's route: that many steps back, only along the route *)
  | Peek  (* I: the followed graph in a floating window *)
  | Pick_up  (* y: carry the open material or SOP graph, or the selected object's graph (see carry.ml) *)
  | Jump  (* / j: a filter over every graph *)
  | Text_command of Text_pane.command
      (* Command-F, -Option-F, -G, -Shift-G, -E and F12: the find bar, its matches, the definition under the caret *)
  | Goto_symbol  (* Command-P: a filter over the bindings of the text *)
  | Goto_line  (* Command-L: a line and column of the text *)
  | Layout_switch of int | Layout_new | Layout_remove
      (* / [ 0..9, n, x: the layouts of the editor graph's switch *)
  | Window_new of Pxui_shell.Layout.panel | Float_toggle
      (* / n g/l/t/i/u/m/w: a floating window in the active layout; / o f floats or docks the focused panel *)
  | World_emit | World_reseed | World_time of float | World_play | World_preset of int
  | Scope_command of Pxui_graph.Scope.command  (* the workspace pane, see [Core_panes.scope_name] *)
  | List_command of Pxui_shell.Tree.command
  | Guide_toggle | Guide_keys
  | Command_palette
  | Copy_lisp  (* the palette: the workspace text, as Command-S writes it, on the clipboard *)

(* A command's scope is a kind of panel: any viewport is [View ""], and the list and the
   lisp panel are the graph pane's other projections. *)
type command = (Pxui_shell.Layout.panel, action) Editor_core.Command.t

let scope : Pxui_shell.Layout.panel -> Pxui_shell.Layout.panel = function
  | View _ -> View "" | List | Lisp -> Graph | panel -> panel

type state = Editor_core.Router.state = Idle | Pending of string

let command ?trigger ?scope ?(guide = []) ~id ~label action =
  let guide = match trigger with Some (Leader _) -> Editor_core.Guide_context.Leader :: guide
    | _ -> guide in
  Editor_core.Command.make ?trigger ?scope ~guide ~id ~label action
let graph = Pxui_shell.Layout.Graph and view = Pxui_shell.Layout.View ""

(* One table drives dispatch, which-key, and the command palette. *)
let keymap = [
  command ~id:"guide.toggle" ~label:"toggle guide"
    ~guide:Editor_core.Guide_context.[Canvas; Node; Multi; Hints; Leader; List]
    ~trigger:(Chord (Input.KeyChar '/', [Input.Shift])) Guide_toggle;
  command ~id:"guide.toggle" ~label:"toggle guide"
    ~trigger:(Chord (Input.KeyChar '?', [])) Guide_toggle;
  command ~id:"guide.keys" ~label:"all keys"
    ~guide:Editor_core.Guide_context.[Canvas; Node; Multi; Hints; List] ~trigger:(Leader "?") Guide_keys;
  command ~id:"preset.save" ~label:"save preset" ~trigger:(Leader "s") Save_preset;
  command ~id:"preset.browse" ~label:"browse presets" ~trigger:(Leader "b") Browse_presets;
  command ~id:"workspace.toggle-timeline" ~label:"toggle timeline" ~trigger:(Leader "t")
    Toggle_timeline;
  command ~id:"workspace.toggle-graph" ~label:"toggle graph" ~trigger:(Leader "g") Toggle_graph;
  command ~id:"workspace.toggle-inspector" ~label:"toggle inspector" ~trigger:(Leader "i")
    Toggle_inspector;
  command ~id:"workspace.hide-ui" ~label:"hide all UI" ~trigger:(Leader "h") Hide_ui;
  command ~id:"workspace.camera-section" ~label:"camera section" ~trigger:(Leader "c")
    Open_camera;
  command ~id:"timeline.play-pause" ~label:"play / pause" ~trigger:(Leader "p") Play_pause;
  command ~id:"timeline.play-pause" ~label:"play / pause" ~trigger:(Chord (Input.Space, [])) Play_pause;
  command ~id:"timeline.reset" ~label:"reset" ~trigger:(Leader "r") Reset;
  command ~id:"timeline.reset" ~label:"reset"
    ~trigger:(Chord (Input.KeyChar 'p', [Input.Shift])) Reset;
  command ~id:"sketch.stop" ~label:"stop" ~trigger:(Leader "q") Stop;
  command ~id:"workspace.command-palette" ~label:"command palette" ~trigger:(Leader "/")
    Command_palette;
  command ~id:"file.copy-lisp" ~label:"Copy workspace as Lisp" Copy_lisp;
  command ~id:"file.reload" ~label:"Reload sketch from its file" Reload_source;
  command ~id:"world.map" ~label:"3D / map (World)" ~trigger:(Leader "m") Toggle_map;
  command ~id:"workspace.restore-layout" ~label:"restore layout" ~trigger:(Leader "z")
    Restore_layout;
  command ~id:"panel.split-right" ~label:"split panel, side by side" ~trigger:(Leader "ov")
    (Panel_split `H);
  command ~id:"panel.split-below" ~label:"split panel, stacked" ~trigger:(Leader "oh")
    (Panel_split `V);
  command ~id:"panel.close" ~label:"close panel" ~trigger:(Leader "ox") Panel_close;
  command ~id:"panel.pick-split" ~label:"split panel at the pointer (pick a kind)"
    ~trigger:(Leader "x") Panel_pick;
  command ~id:"window.pick" ~label:"new window (pick a kind)" ~trigger:(Leader "n") Window_pick;
  command ~id:"panel.float" ~label:"float / dock panel" ~trigger:(Leader "of") Float_toggle;
  (* / [: the first nine rows are the layouts, named from their panels when which-key draws *)
]
@ List.init 10 (fun i ->
  command ~id:("layout." ^ string_of_int i) ~label:"layout" ~trigger:(Leader ("[" ^ string_of_int i))
    (Layout_switch i))
@ [
  command ~id:"layout.new" ~label:"layout: new, from this one" ~trigger:(Leader "[n") Layout_new;
  command ~id:"layout.remove" ~label:"layout: remove this one" ~trigger:(Leader "[x") Layout_remove;
  (* / l: the focused panel becomes one of the kinds (a document without an editor graph
     gets one written from its layout first) *)
  command ~id:"panel.graph" ~label:"panel: graph" ~trigger:(Leader "lg") (Panel_retype Graph);
  command ~id:"panel.list" ~label:"panel: list" ~trigger:(Leader "ll") (Panel_retype List);
  command ~id:"panel.lisp" ~label:"panel: lisp text" ~trigger:(Leader "lt") (Panel_retype Lisp);
  command ~id:"panel.inspector" ~label:"panel: inspector" ~trigger:(Leader "li")
    (Panel_retype Inspector);
  command ~id:"panel.outline" ~label:"panel: outline" ~trigger:(Leader "lu")
    (Panel_retype Outline);
  command ~id:"panel.timeline" ~label:"panel: timeline" ~trigger:(Leader "lm")
    (Panel_retype Timeline);
  command ~id:"panel.viewport" ~label:"panel: viewport" ~trigger:(Leader "lw")
    (Panel_retype (View ""));
  command ~id:"panel.canvas" ~label:"panel: canvas" ~trigger:(Leader "lc")
    (Panel_retype (Canvas ""));
  command ~id:"scene.world" ~label:"World" ~trigger:(Leader "e") Go_world;
  command ~id:"graph.add-node" ~label:"add (menu)" ~trigger:(Leader "a") Add_node;
  (* the graph's own key for the same menu: with a node selected the pick is wired after it *)
  command ~id:"graph.add-after" ~label:"add after" ~trigger:(Chord (Input.Tab, [])) ~scope:graph Add_node;
  command ~guide:Editor_core.Guide_context.[Node; List]
    ~id:"world.emit" ~label:"dome / light" ~trigger:(Chord (Input.KeyChar 't', []))
    ~scope:graph World_emit;
  command ~guide:Editor_core.Guide_context.[Node; List]
    ~id:"world.reseed" ~label:"reseed" ~trigger:(Chord (Input.KeyChar 'n', []))
    ~scope:graph World_reseed;
  command ~guide:Editor_core.Guide_context.[Canvas; Node; Multi; List]
    ~id:"world.earlier" ~label:"time -30 min" ~trigger:(Chord (Input.KeyChar '[', []))
    ~scope:graph (World_time (-0.5));
  command ~guide:Editor_core.Guide_context.[Canvas; Node; Multi; List]
    ~id:"world.later" ~label:"time +30 min" ~trigger:(Chord (Input.KeyChar ']', []))
    ~scope:graph (World_time 0.5);
  command ~guide:Editor_core.Guide_context.[Canvas; Node; Multi; List]
    ~id:"world.play" ~label:"play day cycle" ~trigger:(Chord (Input.KeyChar 'd', []))
    ~scope:graph World_play;
] @ List.mapi (fun index (name, _) ->
  command ~guide:Editor_core.Guide_context.[Canvas; Node; Multi; List]
    ~id:("world.preset." ^ string_of_int (index + 1)) ~label:("preset " ^ name)
    ~trigger:(Chord (Input.KeyChar (Char.chr (Char.code '1' + index)), []))
    ~scope:graph (World_preset index)) World.presets
@ [
  command ~guide:Editor_core.Guide_context.[Node; List] ~id:"scene.enter" ~label:"follow / enter"
    ~trigger:(Chord (Input.KeyChar 'i', [])) Enter;
  command ~guide:Editor_core.Guide_context.[Node; List] ~id:"scene.peek" ~label:"peek (floating graph)"
    ~trigger:(Chord (Input.KeyChar 'i', [Input.Shift])) Peek;
  command ~id:"scene.jump" ~label:"jump to graph" ~trigger:(Leader "j") Jump;
  command ~id:"text.definition" ~label:"go to definition" ~trigger:(Chord (Input.F12, [])) (Text_command Definition);
  command ~id:"text.references" ~label:"find references" ~trigger:(Chord (Input.F12, [Input.Shift])) (Text_command References);
  (* carry: y picks up; the target letters, Enter and Escape are read by [Editor_frame] while it lasts *)
  command ~guide:Editor_core.Guide_context.[Canvas; Node; Multi; List] ~id:"carry.pick-up"
    ~label:"pick up (carry it onto a place)" ~trigger:(Chord (Input.KeyChar 'y', [])) Pick_up;
  command ~guide:Editor_core.Guide_context.[Canvas; Node; Multi; List] ~id:"scene.up" ~label:"back (up a level)"
    ~trigger:(Chord (Input.KeyChar 'u', [])) Up;
  command ~guide:Editor_core.Guide_context.[Canvas; Node; Multi] ~id:"scene.back" ~label:"back along the route"
    ~scope:graph ~trigger:(Chord (Input.Escape, [])) (Back 1);
  (* Rare: the graph context menu and the palette, no leader key. *)
  command ~id:"graph.frame-tile" ~label:"frame displayed tile" ~trigger:(Leader "f")
    ~scope:graph Frame_tile;
  command ~guide:Editor_core.Guide_context.[List] ~id:"graph.frame-tile" ~label:"reveal list selection"
    ~trigger:(Chord (Input.KeyChar 'f', [])) ~scope:graph Frame_tile;
  command ~guide:Editor_core.Guide_context.[Canvas; Node; Multi] ~id:"view.frame-camera"
    ~label:"frame the displayed node"
    ~trigger:(Chord (Input.KeyChar 'f', [])) ~scope:view Frame_camera;
] @ List.concat_map (fun modifier -> [
  command ~id:"file.save" ~label:"save sketch" ~trigger:(Chord (Input.KeyChar 's', [modifier])) Save_source;
  command ~id:"file.open" ~label:"open sketch" ~trigger:(Chord (Input.KeyChar 'o', [modifier])) Open_source;
  command ~id:"edit.undo" ~label:"undo" ~trigger:(Chord (Input.KeyChar 'z', [modifier])) Undo;
  command ~id:"edit.redo" ~label:"redo"
    ~trigger:(Chord (Input.KeyChar 'z', [modifier; Input.Shift])) Redo;
  command ~id:"edit.redo" ~label:"redo" ~trigger:(Chord (Input.KeyChar 'y', [modifier])) Redo;
  command ~id:"text.find" ~label:"find in text" ~trigger:(Chord (Input.KeyChar 'f', [modifier])) (Text_command Find);
  command ~id:"text.replace" ~label:"find and replace"
    ~trigger:(Chord (Input.KeyChar 'f', [modifier; Input.Alt])) (Text_command Find_replace);
  command ~id:"text.next" ~label:"next match" ~trigger:(Chord (Input.KeyChar 'g', [modifier])) (Text_command Find_next);
  command ~id:"text.previous" ~label:"previous match"
    ~trigger:(Chord (Input.KeyChar 'g', [modifier; Input.Shift])) (Text_command Find_previous);
  command ~id:"text.find-selection" ~label:"find the selection"
    ~trigger:(Chord (Input.KeyChar 'e', [modifier])) (Text_command Find_selection);
  command ~id:"text.symbol" ~label:"go to symbol" ~trigger:(Chord (Input.KeyChar 'p', [modifier])) Goto_symbol;
  command ~id:"text.line" ~label:"go to line" ~trigger:(Chord (Input.KeyChar 'l', [modifier])) Goto_line;
  command ~id:"ui.larger" ~label:"larger panel text" ~trigger:(Chord (Input.KeyChar '=', [modifier])) (Ui_scale 1);
  command ~id:"ui.larger" ~label:"larger panel text" ~trigger:(Chord (Input.KeyChar '+', [modifier])) (Ui_scale 1);
  command ~id:"ui.smaller" ~label:"smaller panel text" ~trigger:(Chord (Input.KeyChar '-', [modifier])) (Ui_scale (-1));
  command ~id:"ui.reset-size" ~label:"default panel text size" ~trigger:(Chord (Input.KeyChar '0', [modifier])) (Ui_scale 0)])
  [Input.Meta; Input.Ctrl]
@ List.map (fun (c : _ Editor_core.Command.t) ->
  { c with scope = Some graph; action = Scope_command c.action }) Pxui_graph.Scope.bindings
@ List.map (fun (c : _ Editor_core.Command.t) ->
  { c with scope = Some graph; guide = [Editor_core.Guide_context.List];
    action = List_command c.action }) Pxui_shell.Tree.bindings
@ [
  command ~id:"view.fly" ~label:"fly (WASD, Q/E, Esc)" ~trigger:(Leader "w") ~scope:view Fly;
  command ~id:"view.look-through" ~label:"look through render camera" ~trigger:(Leader "v")
    ~scope:view Look_through;
  command ~guide:Editor_core.Guide_context.[Canvas; Node; Multi] ~id:"view.translate" ~label:"move handles"
    ~trigger:(Chord (Input.KeyChar 'w', [])) ~scope:view (Tool 1);
  command ~guide:Editor_core.Guide_context.[Canvas; Node; Multi] ~id:"view.rotate" ~label:"rotate handles"
    ~trigger:(Chord (Input.KeyChar 'e', [])) ~scope:view (Tool 2);
  command ~guide:Editor_core.Guide_context.[Canvas; Node; Multi] ~id:"view.scale" ~label:"scale handles"
    ~trigger:(Chord (Input.KeyChar 'r', [])) ~scope:view (Tool 3);
  command ~guide:Editor_core.Guide_context.[Canvas; Node; Multi] ~id:"view.orbit" ~label:"orbit only (hide handles)"
    ~trigger:(Chord (Input.Escape, [])) ~scope:view (Tool 0);
]

(* The keys the text area handles itself (Command-/, Option-Up/Down, Command-Option-Up, Command-Shift-\\):
   listed on the key sheet only.  They are not in [keymap], so the router never takes them from the
   field. *)
let text_keys =
  [ "toggle comment on lines", Chord (Input.KeyChar '/', [Input.Meta]);
    "move lines up or down", Chord (Input.ArrowUp, [Input.Alt]);
    "duplicate lines up or down", Chord (Input.ArrowUp, [Input.Alt; Input.Shift]);
    "select the enclosing form", Chord (Input.ArrowUp, [Input.Meta; Input.Alt]);
    "jump to the matching bracket", Chord (Input.KeyChar '\\', [Input.Meta; Input.Shift]) ]

(* The leader sheet's sections ([Pxui_shell.Which_key.panel]): what a command is for, from its id. *)
let group (c : command) =
  let id = c.id in
  let starts prefix = String.starts_with ~prefix id in
  if id = "graph.add-node" || starts "window." then "Add"
  else if starts "panel." || starts "workspace.toggle-" || id = "workspace.hide-ui" then "Panel"
  else if starts "text." then "Text"
  else if starts "layout." || id = "workspace.restore-layout" then "Layout"
  else if starts "timeline." || id = "sketch.stop" then "Time"
  else if starts "preset." || starts "file." || id = "guide.keys" || id = "workspace.command-palette" then "File"
  else "Go"

(* The key sheet's sections ([Pxui_shell.Which_key.sheet]): a pane's own keys under the pane, the
   rest as the leader sheet has them. *)
let sheet_group (c : command) = match c.scope with
  | Some Pxui_shell.Layout.Graph -> "Graph"
  | Some (View _) -> "Viewport"
  | Some _ | None -> group c

(* The keys of a section in the sheet's [09] order, not the keymap's *)
let order = function
  | "Add" -> [ "a"; "n" ]
  | "Panel" -> [ "o"; "l" ]
  | "Layout" -> [ "[" ]
  | "Go" -> [ "j"; "e" ]
  | "Time" -> [ "p" ]
  | "File" -> [ "s"; "b"; "?"; "/" ]
  | _ -> []

(* What a leader key that continues into more keys stands for *)
let describe_prefix = function
  | "o" -> Some "split, close, float"
  | "l" -> Some "panel kind"
  | "n" -> Some "window"
  | "[" -> Some "switch layout"
  | _ -> None

let pane_name = function
  | Pxui_shell.Layout.Canvas _ -> "Canvas" | Pxui_shell.Layout.View _ -> "View" | Graph -> "Graph" | List -> "List" | Lisp -> "Lisp"
  | Inspector -> "Inspector" | Spreadsheet -> "Spreadsheet" | Outline -> "Outline" | Timeline -> "Timeline"
