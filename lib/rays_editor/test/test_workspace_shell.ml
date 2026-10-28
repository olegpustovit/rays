(* W10 part B: the editor graph lowers to the shell tree; viewports over other
   instances of the scene get objects of their own; layout gestures are edits of
   the editor graph with history entries; "Restore layout" survives a layout that
   hides everything. *)
open Rays
open Sop
module Doc = Editor_document.Workspace_doc
module Document = Editor_document.Document
module Contexts = Editor_document.Contexts
module Kinds = Editor_document.Kinds
module Panels = Editor_core.Panels
module Layout = Pxui_shell.Layout
module E = Flow_graph.Flow_edit
module S = Flow.Syntax
module E3 = Rays_editor.Editor

let fail message = failwith ("test_workspace_shell: " ^ message)
let check condition message = if not condition then fail message
let factories = Sop_catalog.Editor.factories
let catalog = Kinds.catalog ~version:1 factories |> Result.get_ok
let show ds = String.concat "; " (List.map Diagnostic.to_string ds)
open Test_text

let of_text text = match Doc.of_text catalog text with Ok d -> d | Error ds -> fail (show ds)
let build ?previous ws = Contexts.of_workspace ~factories ?previous ws
let build_ok ws = match build ws with Ok d -> d | Error d -> fail (Diagnostic.to_string d)
let replace text from by =
  let n = String.length from in
  let rec at i = if i + n > String.length text then fail ("no " ^ from)
    else if String.sub text i n = from then i else at (i + 1) in
  let i = at 0 in
  String.sub text 0 i ^ by ^ String.sub text (i + n) (String.length text - i - n)
let shell_of doc = match doc.Document.shell with Some s -> s | None -> fail "no editor graph"
let leaves tree = Panels.leaves tree

(* the variations workspace with its editor graph replaced *)
let with_editor body =
  let text = case "variations" in
  let start = let rec find i = if String.sub text i 22 = "(graph editor :context" then i else find (i + 1) in find 0 in
  String.sub text 0 start ^ "(graph editor :context editor\n" ^ body ^ "))\n"

let expect_error name text needle =
  match Doc.of_text catalog text with
  | Error ds -> check (has (show ds) needle) (name ^ ": " ^ show ds)
  | Ok ws -> (match build ws with
      | Error d -> check (has (Diagnostic.to_string d) needle) (name ^ ": " ^ Diagnostic.to_string d)
      | Ok _ -> fail (name ^ " was accepted"))

(* ---- lowering ---- *)

let run_lowering () =
  let doc = build_ok (of_text (case "variations")) in
  let shell = shell_of doc in
  check (Panels.to_string shell.tree = "(h outline (h (v graph lisp) (tile view view view view)))")
    ("the variations shell: " ^ Panels.to_string shell.tree);
  (match shell.tree with
   | Split { size = `Ratio ratio; b = Split { size = `Ratio inner; a = Split { size = `Ratio left; _ }; _ }; _ } ->
       check (ratio = 0.13 && inner = 0.5 && left = 0.58) "ratios come from the editor graph"
   | _ -> fail "root split");
  (* four viewports: the default scene instance, and one scene instance for each override *)
  let keys = List.filter_map (fun (_, p) -> match p with Panels.View k -> Some k | _ -> None) (leaves shell.tree) in
  check (keys = [ "v1.1.0"; "v1.1.1"; "v1.1.2"; "v1.1.3" ]) ("viewport keys are tile indices: " ^ String.concat "," keys);
  check (List.map fst shell.views = [ "v1.1.1"; "v1.1.2"; "v1.1.3" ]
         && List.for_all (fun (_, ids) -> List.length ids = 1) shell.views)
    "the default instance is the primary scene; each override has an object of its own";
  let ids = List.concat_map snd shell.views in
  check (List.length (List.sort_uniq compare ids) = 3) "the objects of the overrides are distinct";
  (* origins: named panels can be edited, looped ones say where they come from (E1) *)
  let origin path = List.assoc_opt path shell.origins in
  check (origin [ 0 ] = Some (Document.Bound "outline") && origin [ 1; 0; 0 ] = Some (Document.Bound "network")
         && origin [ 1; 0 ] = Some (Document.Bound "left") && origin [] = Some (Document.Bound "panels"))
    "named panels keep their binding";
  check (origin [ 1; 1; 2 ] = Some (Document.Loop (Document.Bound_at [ "editor"; "sheet" ], Flow_graph.Flow_edit.Pos 0)) && origin [ 1; 1 ] = Some (Document.Bound "sheet"))
    "a panel made by a loop names the loop";
  (* a count change keeps the first panels' keys *)
  let more = build_ok (of_text (replace (case "variations") "(range 4)" "(range 6)")) in
  let keys6 = List.filter_map (fun (_, p) -> match p with Panels.View k -> Some k | _ -> None)
      (leaves (shell_of more).tree) in
  check (List.take 4 keys6 = keys && List.length keys6 = 6) "panel keys follow the iteration index";
  check (List.length (Editor_document.Document.scene_graph more |> Sop.Edit_graph.inspect) > 4) "more objects for more viewports";
  (* the default shell of a workspace without an editor graph, and a named graph panel *)
  let plain = build_ok (of_text (case "tree")) in
  check (plain.shell = None) "no editor graph, no shell";
  let named = build_ok (of_text (with_editor "    (ui/workspace (ui/split \"vertical\" (ui/graph \"scene\") (ui/viewport (ref scene))))")) in
  check ((shell_of named).named = [ [ 0 ], "scene" ]) "ui/graph names its graph";
  check (Panels.to_string (shell_of named).tree = "(v graph view)") "ui/split is an even split";
  (match (shell_of named).tree with Split { size = `Ratio ratio; _ } -> check (ratio = 0.5) "ui/split halves" | _ -> fail "split");
  (* negative: what the checker or the evaluator refuses leaves no layout *)
  expect_error "a split axis" (with_editor "    (ui/workspace (ui/split \"diagonal\" (ui/graph) (ui/lisp)))") "axis";
  expect_error "a computed ratio out of range"
    (with_editor "    (let* [r (* 3 0.5)] (ui/workspace (ui/split-at \"horizontal\" r (ui/graph) (ui/lisp))))") "0.1";
  expect_error "an empty tile" (with_editor "    (ui/workspace (ui/tile (for [i (range 0)] (ui/graph))))") "tile";
  (* a selected layout that names no editor graph is dropped, not an error (a removed graph) *)
  check ((of_text (with_editor "    (ui/workspace (ui/graph))" ^ "\n(layout (editor \"missing\"))")).layout.editor = None)
    "a dangling editor layout was kept"

(* ---- through the editor ---- *)

let frame ?(buttons = []) mouse events count = Test_editor_input.frame ~buttons mouse events count
(* every editor here awaits the cook its frame submits: what a frame shows is settled, never racing
   the worker *)
let editor ?camera ?presets text =
  E3.create ~ui_text_size:11 ?camera ?presets ~await:true ~workspace:(of_text text)
    ~prepare:(fun _ output -> Rdk_rays.Rays_mesh.to_mesh (Result.get_ok (Sop.Payload.geometry output.Sop.Session.payload))
      |> Result.map_error Rdk.Error.to_string)
    ~draw:(fun _ mesh -> Scene.D3.create [ Scene.D3.mesh mesh ]) ()
  |> function Ok e -> e | Error m -> fail m

let source e = fst (Flow.Lisp.print (E3.workspace e).Doc.source)

let dump_line e key =
  let directory = Filename.temp_dir "rays-shell-dump" "" in
  Fun.protect ~finally:(fun () ->
    Array.iter (fun f -> Sys.remove (Filename.concat directory f)) (Sys.readdir directory);
    Unix.rmdir directory) (fun () ->
    E3.crash_dump e directory;
    let text = In_channel.with_open_bin (Filename.concat directory "editor.txt") In_channel.input_all in
    let lines = String.split_on_char '\n' text in
    match List.find_opt (fun l -> String.starts_with ~prefix:(key ^ ": ") l) lines with
    | Some l -> String.sub l (String.length key + 2) (String.length l - String.length key - 2)
    | None -> fail ("no " ^ key ^ " in the dump"))

let run_editor () =
  let e = ref (editor (case "variations")) and count = ref 0 in
  let step ?(buttons = []) ?(mouse = (450., 300.)) events =
    incr count; e := E3.update !e (frame ~buttons mouse events !count) in
  step []; step [];
  (* the editor awaits each cook, so after a frame nothing is cooking: a check, not a wait *)
  let settle () =
    let line = dump_line !e "cook" in
    check (not (String.starts_with ~prefix:"Cooking" line || String.starts_with ~prefix:"Waiting" line
                || has line "skipping frames")) ("the editor is still cooking: " ^ line) in
  settle ();
  let click ?(button = Input.LeftButton) (x, y) =
    step ~mouse:(x, y) [ Input.Pointer_moved (x, y) ];
    step ~buttons:[ button ] ~mouse:(x, y) [ Input.Pointer_pressed (button, (x, y)) ];
    step ~mouse:(x, y) [ Input.Pointer_released (button, (x, y)) ] in
  let drag ~from ~to_ =
    step ~mouse:from [ Input.Pointer_moved from ];
    step ~buttons:[ Input.LeftButton ] ~mouse:from [ Input.Pointer_pressed (Input.LeftButton, from) ];
    step ~buttons:[ Input.LeftButton ] ~mouse:to_ [ Input.Pointer_moved to_ ];
    step ~mouse:to_ [ Input.Pointer_released (Input.LeftButton, to_) ];
    step [] in
  (* drag the outline | rest gutter: one rewrite of the editor graph, one history entry.
     At 900 points the outline's 120-point minimum lifts its 13% to 169. *)
  drag ~from:(169.5, 300.) ~to_:(300., 300.);
  check (has (source !e) "(ui/split-at \"horizontal\" 0.3333 outline right)") ("the drag rewrote the outline split: " ^ source !e);
  check (E3.undo_label !e = Some "Resize panel") "one history entry named Resize panel";
  (* the header menu of the graph panel: split side by side *)
  let hx = 400. in
  click ~button:Input.RightButton (hx, 10.);
  step [];
  click (420., 28.);
  step [];
  check (E3.undo_label !e = Some "Split panel") ("the menu split the graph panel: " ^ Option.value ~default:"-" (E3.undo_label !e));
  check (has (source !e) "network_a" && has (source !e) "network_b") "the split bound two new panels";
  (* a panel made by a loop cannot be split: the status says where it comes from *)
  let geometry = Layout.geometry ~hidden:[ Layout.Timeline ] (shell_of (build_ok (E3.workspace !e)))
      .tree (frame (0., 0.) [] 0) in
  let leaf = Option.get (Layout.find geometry (Layout.View "v1.1.2")) in
  let hx, hy, hw, _ = leaf.header in
  let depth = E3.undo_label !e in
  click ~button:Input.RightButton (float (hx + (hw / 2)), float (hy + 10));
  step [];
  click (float (hx + (hw / 2) + 20), float (hy + 10 + 18));
  step [];
  check (E3.undo_label !e = depth) "a looped panel did not change the graph";
  settle ();
  check (has (dump_line !e "cook") "made by a loop in sheet") ("the status names the loop: " ^ dump_line !e "cook" ^ " / " ^ dump_line !e "edit error");
  (* focus is a panel: a press in a panel focuses it *)
  let focus_after (x, y) =
    click (x, y); step []; dump_line !e "focus" in
  check (focus_after (30., 300.) = "Outline") "a press in the outline focuses it";
  check (focus_after (float (hx + 30), float (hy + 100)) = "View") "a press in a viewport focuses a viewport";
  E3.close !e

(* / o ...: the focused panel is split, closed or retyped by keys, the header menu's edits *)
let run_panel_keys () =
  let started () =
    let e = ref (editor (case "variations")) and count = ref 0 in
    let step ?(mouse = (300., 150.)) events = incr count; e := E3.update !e (frame mouse events !count) in
    step []; step [];
    (* a press in the graph panel focuses it *)
    step [ Input.Pointer_moved (300., 150.) ];
    step [ Input.Pointer_pressed (Input.LeftButton, (300., 150.)) ];
    step [ Input.Pointer_released (Input.LeftButton, (300., 150.)) ];
    step [];
    check (dump_line !e "focus" = "Graph") ("focus is the graph panel: " ^ dump_line !e "focus");
    e, step in
  let key k = Input.Key_pressed k and ch c = Input.Key_pressed (Input.KeyChar c) in
  let e, step = started () in
  step [ key (Input.KeyChar '/'); ch 'o'; ch 'v' ]; step [];
  check (E3.undo_label !e = Some "Split panel" && has (source !e) "network_a") "/ o v split the focused panel";
  step [ key (Input.KeyChar '/'); ch 'o'; ch 'x' ]; step [];
  check (E3.undo_label !e = Some "Close panel") "/ o x closed it again";
  (* a document without an editor graph gets one written from its layout before the retype *)
  let e = ref (editor "(workspace w (graph g :context sop (sop/box)))") and count = ref 0 in
  let step ?(mouse = (600., 300.)) events = incr count; e := E3.update !e (frame mouse events !count) in
  step []; step [];
  let gx, gy, _, _ = (E3.panes !e (frame (0., 0.) [] 0)).graph in
  let p = float (gx + 40), float (gy + 40) in
  step ~mouse:p [ Input.Pointer_moved p ];
  step ~mouse:p [ Input.Pointer_pressed (Input.LeftButton, p) ];
  step ~mouse:p [ Input.Pointer_released (Input.LeftButton, p) ];
  step ~mouse:p [];
  check (not (has (source !e) "(graph editor")) "the plain workspace has no editor graph";
  step ~mouse:p [ key (Input.KeyChar '/'); ch 'l'; ch 'i' ]; step ~mouse:p [];
  check (has (source !e) "(graph editor :context editor" && has (source !e) "network (ui/inspector)"
         && has (source !e) "preview (ui/viewport (ref scene))" && has (source !e) "(graph scene :context scene"
         && E3.undo_label !e = Some "Retype panel")
    ("retyping wrote the editor graph (and the scene it views) first: " ^ source !e);
  E3.close !e;
  (* the sketch reads the pointer only on its picture: a press on the graph panel is not its
     press, a drag begun there never becomes one, and a drag begun on the picture stays its own *)
  let e = ref (editor (with_editor "    (let* [a (ui/graph) b (ui/viewport (ref scene)) both (ui/split-at \"horizontal\" 0.5 a b)] (ui/workspace both))")) and count = ref 0 in
  let step ?(buttons = []) mouse events = incr count; e := E3.update !e (frame ~buttons mouse events !count) in
  let live () = E3.Private.live_frame !e in
  let panel = (200., 300.) and picture = (700., 300.) and held = [ Input.LeftButton ] in
  step picture []; step picture [ Input.Pointer_moved picture ];
  check ((live ()).pointer = picture) "the picture has the pointer";
  step panel [ Input.Pointer_moved panel ];
  step ~buttons:held panel [ Input.Pointer_pressed (Input.LeftButton, panel) ];
  check ((live ()).pointer = picture && (live ()).buttons = [] && (live ()).events = [])
    "a press on a panel reached the sketch";
  step ~buttons:held picture [ Input.Pointer_moved picture ];
  check ((live ()).buttons = []) "a drag begun on a panel became the sketch's over the picture";
  step picture [ Input.Pointer_released (Input.LeftButton, picture) ]; step picture [];
  step ~buttons:held picture [ Input.Pointer_pressed (Input.LeftButton, picture) ];
  check ((live ()).buttons = [ Input.LeftButton ] && (live ()).events <> []) "a press on the picture did not reach the sketch";
  step ~buttons:held panel [ Input.Pointer_moved panel ];
  check ((live ()).buttons = [ Input.LeftButton ] && (live ()).pointer = panel) "a drag begun on the picture was cut at its edge";
  step panel [ Input.Pointer_released (Input.LeftButton, panel) ];
  check ((live ()).events <> []) "the sketch missed the release of its own drag";
  step panel [];
  check ((live ()).pointer = panel && (live ()).buttons = []) "the pointer left the picture where the drag ended";
  E3.close !e;
  (* the sketch has the keyboard only while its picture is focused, and a key held across the change
     (the Esc that left a panel) stays hidden until it is released *)
  let e = ref (editor (with_editor "    (let* [a (ui/graph) b (ui/viewport (ref scene)) both (ui/split-at \"horizontal\" 0.5 a b)] (ui/workspace both))")) and count = ref 0 in
  let step ?(keys = []) mouse events = incr count; e := E3.update !e (Test_editor_input.frame ~keys mouse events !count) in
  let seen () = (E3.Private.live_frame !e).keys in
  let esc = [ Input.Escape ] and panel = (200., 300.) and picture = (700., 300.) in
  let click ?keys at = step ?keys at [ Input.Pointer_pressed (Input.LeftButton, at) ]; step ?keys at [ Input.Pointer_released (Input.LeftButton, at) ]; step ?keys at [] in
  click picture; step ~keys:esc picture [];
  check (seen () = [ Input.Escape ]) "the focused picture did not get a held Esc";
  click ~keys:esc panel; step ~keys:esc panel [];
  check (seen () = []) "a panel's Esc reached the sketch";
  click ~keys:esc picture; step ~keys:esc picture [];
  check (seen () = []) "an Esc held from the panel reached the sketch after the focus changed";
  step picture []; step ~keys:esc picture [];
  check (seen () = [ Input.Escape ]) "a fresh Esc was hidden from the focused picture";
  E3.close !e

(* panels written in place, and panels made by a loop, are edited through the keys too: an
   inline panel is bound to a name first (one history entry), a loop's panels are retyped
   through their template *)
let run_unbound_panels () =
  let started text focus_at =
    let e = ref (editor text) and count = ref 0 in
    let step ?(mouse = focus_at) events = incr count; e := E3.update !e (frame mouse events !count) in
    step []; step [];
    step [ Input.Pointer_moved focus_at ];
    step [ Input.Pointer_pressed (Input.LeftButton, focus_at) ];
    step [ Input.Pointer_released (Input.LeftButton, focus_at) ];
    step [];
    e, step in
  let key k = Input.Key_pressed k and ch c = Input.Key_pressed (Input.KeyChar c) in
  let inline = with_editor "    (ui/workspace (ui/split-at \"vertical\" 0.5 (ui/graph) (ui/viewport (ref scene))))" in
  let e, step = started inline (300., 100.) in
  check (dump_line !e "focus" = "Graph") ("focus is the inline graph panel: " ^ dump_line !e "focus");
  step [ key (Input.KeyChar '/'); ch 'l'; ch 'i' ]; step [];
  check (E3.undo_label !e = Some "Retype panel" && has (source !e) "(ui/inspector)" && has (source !e) "(ui/viewport (ref scene))")
    ("an inline panel was retyped: " ^ Option.value ~default:"-" (E3.undo_label !e) ^ "\n" ^ source !e);
  E3.close !e;
  let e, step = started inline (300., 100.) in
  step [ key (Input.KeyChar '/'); ch 'o'; ch 'v' ]; step [];
  check (E3.undo_label !e = Some "Split panel" && has (source !e) "graph_a" || has (source !e) "ui/split-at \"horizontal\" 0.5")
    ("an inline panel was split: " ^ Option.value ~default:"-" (E3.undo_label !e) ^ "\n" ^ source !e);
  E3.close !e;
  (* a loop's panels: retyping edits the template, splitting them says why it cannot *)
  let e, step = started (case "variations") (800., 100.) in
  check (dump_line !e "focus" = "View") "focus is a looped viewport";
  step [ key (Input.KeyChar '/'); ch 'o'; ch 'x' ]; step [];
  check (E3.undo_label !e = None && has (dump_line !e "cook") "copies made by a loop")
    ("closing a looped panel said why not: " ^ Option.value ~default:"-" (E3.undo_label !e) ^ " / " ^ dump_line !e "cook");
  step [ key (Input.KeyChar '/'); ch 'l'; ch 'l' ]; step [];
  check (E3.undo_label !e = Some "Retype panel" && has (source !e) "(ui/list)")
    ("retyping a looped panel edits its template: " ^ Option.value ~default:"-" (E3.undo_label !e));
  E3.close !e

(* Wires from anything, in one step: a node added after a nested node (a call written in another's
   input) names it first, and a node added on a wire pulled into empty canvas sits between the
   wire's source and its sink *)
let run_wire_gestures () =
  let count = ref 0 in
  let started text =
    let e = ref (editor text) in
    let step ?(buttons = []) mouse events = incr count; e := E3.update !e (frame ~buttons mouse events !count) in
    step (450., 300.) []; step (450., 300.) [];
    e, step in
  let key k = Input.Key_pressed k and ch c = Input.Key_pressed (Input.KeyChar c) in
  let pick step text = step (450., 300.) [ key (Input.KeyChar '/'); ch 'a' ]; step (450., 300.) [ Input.Text_input text ];
    step (450., 300.) [ key Input.Enter ]; step (450., 300.) [] in
  let nested = "(workspace w (graph g :context sop (let* [m (sop/transform (sop/box))] m)))" in
  let e, step = started nested in
  e := Rays_editor.Reduce.select_path !e [ "g"; "m#0" ];
  pick step "transform";
  check (E3.undo_label !e = Some "Add node" && not (has (source !e) "(sop/transform (sop/box))")
    && has (source !e) "(sop/box)" && has (source !e) "(sop/transform box)")
    ("a node after a nested node: " ^ source !e);
  incr count;
  let undo_frame = frame (450., 300.) [ ch 'z' ] !count in
  e := E3.update !e { undo_frame with input = { undo_frame.input with keys = [ Input.Meta ] } };
  check (has (source !e) "(sop/transform (sop/box))" && E3.undo_label !e = None)
    ("one undo took the name and the node back: " ^ source !e);
  E3.close !e;
  let e, step = started "(workspace w (graph g :context sop (let* [a (sop/box) m (sop/transform a)] m)))" in
  let gx, gy, gw, gh = (E3.panes !e (frame (0., 0.) [] 0)).graph in
  let p = float (gx + 40), float (gy + 40) in
  step p [ Input.Pointer_moved p ]; step ~buttons:[ Input.LeftButton ] p [ Input.Pointer_pressed (Input.LeftButton, p) ];
  step p [ Input.Pointer_released (Input.LeftButton, p) ];
  step p [ Input.Key_pressed Input.Home ]; step p [ ch 'i' ]; step p [];
  check (E3.level !e = Some "g") "the graph is open";
  let box path = Option.get (E3.node_box !e path) in
  let ax, ay, aw, _ = box [ "g"; "a" ] and mx, my, _, _ = box [ "g"; "m" ] in
  (* the wire runs from a's out-port to m's header in-port *)
  let z = float (mx - ax) /. 288. in
  let from = float (ax + aw), float ay +. 11.75 *. z and into = float mx, float my +. 11.75 *. z in
  let mid = (fst from +. fst into) /. 2., (snd from +. snd into) /. 2. in
  let empty = float (gx + gw - 30), float (gy + gh - 30) in
  let held = [ Input.LeftButton ] in
  step mid [ Input.Pointer_moved mid ];
  step ~buttons:held mid [ Input.Pointer_pressed (Input.LeftButton, mid) ];
  step ~buttons:held empty [ Input.Pointer_moved empty ];
  step empty [ Input.Pointer_released (Input.LeftButton, empty) ];
  step empty [];
  step empty [ Input.Text_input "transform" ]; step empty [ key Input.Enter ]; step empty [];
  check (E3.undo_label !e = Some "Insert node" && has (source !e) "(sop/transform a)"
    && has (source !e) "(sop/transform transform)")
    ("a node inserted on a pulled wire: " ^ source !e);
  E3.close !e

(* Value nodes are bindings: the add menu offers a number, the time, a vector and every operator, and
   each becomes one [Add_node] with the expression written *)
let run_values () =
  let started ?text () =
    let opening = Option.is_none text in
    let text = Option.value ~default:"(workspace w (graph g :context sop (sop/box)))" text in
    let e = ref (editor text) and count = ref 0 in
    let step ?(mouse = (450., 300.)) events = incr count; e := E3.update !e (frame mouse events !count) in
    step []; step [];
    let gx, gy, _, _ = (E3.panes !e (frame (0., 0.) [] 0)).graph in
    let p = float (gx + 40), float (gy + 40) in
    step ~mouse:p [ Input.Pointer_moved p ];
    step ~mouse:p [ Input.Pointer_pressed (Input.LeftButton, p) ];
    step ~mouse:p [ Input.Pointer_released (Input.LeftButton, p) ];
    if opening then begin
      step ~mouse:p [ Input.Key_pressed Input.Home ];
      step ~mouse:p [ Input.Key_pressed (Input.KeyChar 'i') ]
    end;
    step ~mouse:p [];
    e, (fun events -> step ~mouse:p events) in
  let key k = Input.Key_pressed k and ch c = Input.Key_pressed (Input.KeyChar c) in
  let add ?workspace ?(pick=(fun _->0)) text =
    let e, step = started ?text:workspace () in
    if workspace = None then check (E3.level !e = Some "g") "the box graph is open";
    step [ key (Input.KeyChar '/'); ch 'a' ]; step [ Input.Text_input text ];
    for _=1 to pick !e do step [key Input.ArrowDown] done;
    step [ key Input.Enter ]; step [];
    !e in
  let e = add "number" in
  check (E3.undo_label e = Some "Add node" && has (source e) "value 1.0") ("a number binding: " ^ source e);
  E3.close e;
  let e = add "time" in
  check (has (source e) "time t") ("a time binding: " ^ source e);
  E3.close e;
  let e = add "sin" in
  check (has (source e) "(sin 0.5)") ("an operator binding with typed defaults: " ^ source e);
  E3.close e;
  let e = add "str" in
  check (has (source e) "(str \"text\")") ("a str binding: " ^ source e);
  E3.close e;
  check(not(List.exists(fun factory->Edit_graph.factory_key factory="line")factories))
    "Line production menu still uses an OCaml registration";
  let e=add "line" in
  Fun.protect ~finally:(fun()->E3.close e)(fun()->
    check(E3.undo_label e=Some "Add node" && has(source e)"(sop/line)")
      ("Line actual zero-input menu insertion: "^source e);
    let ws=E3.workspace e in let doc=build_ok ws in let lowered=doc.Document.workspace.lowered in
    let pane=Flow_graph.Projection.of_graph catalog ws.checked "g" in
    check(List.exists(fun(card:Flow_graph.Projection.node)->card.head="sop/line")pane.nodes)
      "Line menu insertion has no geometry card";
    ignore lowered);
  check(not(List.exists(fun factory->Edit_graph.factory_key factory="points")factories))
    "Points production menu still uses an OCaml registration";
  let pick_points editor=
    let doc=build_ok(E3.workspace editor) in
    let lowered=doc.Document.workspace.lowered in
    let module Menu=Pxui_graph.Node_menu in
    let entries=List.map(fun(d:Flow_sop.Catalog.descriptor)->Menu.{key=d.key;
      label=d.label;category=d.category;
      arity=List.length d.slots-List.length d.keyword_inputs;context="sop";output=Flow.Ty.geometry;off=None})
      (Flow_sop.Catalog.with_library(List.map Flow_sop.Catalog.descriptor(factories@lowered.factories))) in
    Menu.Private.keys (Menu.create ~x:0 ~y:0 entries) ~query:"points"
    |>List.find_index((=)"points") |>Option.get in
  let e=add ~pick:pick_points "points" in
  Fun.protect ~finally:(fun()->E3.close e)(fun()->
    check(E3.undo_label e=Some "Add node" && has(source e)"(sop/points)")
      ("Points actual zero-input menu insertion: "^source e);
    let ws=E3.workspace e in let doc=build_ok ws in let lowered=doc.Document.workspace.lowered in
    let pane=Flow_graph.Projection.of_graph catalog ws.checked "g" in
    check(List.exists(fun(card:Flow_graph.Projection.node)->card.head="sop/points")pane.nodes)
      "Points menu insertion has no geometry card";
    ignore lowered);
  List.iter(fun(name,expression)->let e=add name in
    check(E3.undo_label e=Some "Add node" && has(source e)expression)
      ("ordered integer array menu insertion: "^source e);E3.close e)
    ["array/compact","(array/compact (array/int 0))";
     "array/scan_sum","(array/scan_sum (array/int 0))"];
  List.iter(fun(name,expression)->let e=add name in
    check(E3.undo_label e=Some "Add node" && has(source e)expression)
      ("grade/index menu insertion: "^source e);E3.close e)
    ["array/grade","(array/grade (array/float 4))";
     "array/index_of","(array/index_of (array/int 0) (array/int 0))"];
  let e = add "array/scatter_add" in

  check (E3.undo_label e = Some "Add node"
    && has (source e) "(array/scatter_add (array/int 0) (array/float 0) 0)")
    ("empty ordered scatter menu insertion: " ^ source e);
  E3.close e;
  let e = add "sop/mesh" in

  check (E3.undo_label e = Some "Add node" && has (source e) "(sop/mesh"
    && has (source e) ":points (array/vec3 0)"
    && has (source e) ":vertex_points (array/int 0)"
    && has (source e) ":primitive_offsets (array/int 1)"
    && has (source e) ":kinds (array/int 0)")
    ("required mesh keyword arrays were absent from production menu insertion: " ^ source e);
  E3.close e;
  let e = add "sop/mesh_planes" in
  check (E3.undo_label e = Some "Add node" && has (source e) "(sop/mesh_planes"
    && has (source e) ":points {:x (array/float 0)"
    && has (source e) ":y (array/float 0)"
    && has (source e) ":z (array/float 0)}"
    && has (source e) ":primitive_offsets (array/int 1)")
    ("required typed mesh planes were absent from production menu insertion: " ^ source e);
  E3.close e;
  let e = add "sop/with_attr" in
  Fun.protect ~finally:(fun()->E3.close e)(fun()->
    check(E3.undo_label e=Some "Add node" && has(source e)"(sop/with_attr (sop/mesh"
      && has(source e)"\"attribute\"" && has(source e)"(array/vec3 0)")
      ("ordinary full attribute menu starter lost typed legacy array/geometry defaults: "^source e);
    let saved=of_text(source e) in
    let doc=build_ok saved in
    let lowered=doc.Document.workspace.lowered in
    let graph=List.find(fun(graph:Flow_sop.Lower.graph)->graph.name="g")lowered.graphs in
    let info=List.find(fun(info:Edit_graph.node_info)->info.operation="flow.with_attr")
      (Edit_graph.inspect graph.network.geometry) in
    let node=Edit_graph.compile_node graph.network.geometry ~node_id:info.id|>Result.get_ok in
    let session=Session.create ~max_entries:32 ~max_payload_bytes:1_000_000|>Result.get_ok in
    Fun.protect ~finally:(fun()->Session.close session)(fun()->
      let output=Session.cook session ~context:(Context.create()|>Result.get_ok)node|>Result.get_ok in
      let geo=Payload.geometry output.payload|>Result.get_ok in
      check(Rdk.Geometry.point_count geo=0)"ordinary attribute menu starter did not cook actual empty geometry";
      let attr=Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Point "attribute"geo|>Option.get in
      check(Rdk.Attribute.Private.storage attr|>function Rdk.Attribute.Float3 data->Rdk.Packed.Float3.length data=0|_->false)
        "ordinary attribute menu starter lost vec3 storage"));
  let e = add "sop/remap" in
  check (E3.undo_label e = Some "Add node" && has (source e) "(sop/remap (sop/mesh"
    && has (source e) ":points (array/int 0)"
    && has (source e) ":vertices (array/int 0)"
    && has (source e) ":primitives (array/int 0)")
    ("required remap maps and cooked empty geometry defaults were absent from production menu insertion: " ^ source e);
  E3.close e;
  let e = add "attr_from_image" in

  check (E3.undo_label e = Some "Add node" && has (source e) "sop/attr_from_image"
         && has (source e) ":image (image/noise :width 1 :height 1)")
    ("a mixed SOP has a real image default through the add menu: " ^ source e);
  E3.close e;
  List.iter (fun op -> let e = add op in
    check (E3.undo_label e = Some "Add node" && has (source e) ("(" ^ op))
      ("a frame/array operator has checked menu defaults: " ^ source e);
    E3.close e) ["frame/dt"; "array/count"; "array/nth"; "array/sum"];
  let workspace = "(workspace w (graph g :context draw (draw/merge)) \
    (graph editor :context editor (ui/workspace (ui/graph \"g\" :focus true))))" in
  List.iter (fun (op : Flow.Op.t) -> if op.ctx = Flow.Context.draw then begin
    let e = add ~workspace op.name in
    check (E3.undo_label e = Some "Add node" && has (source e) ("(" ^ op.name))
      ("a drawing operator is added through the menu: " ^ source e);
    E3.close e
  end) Flow.Op.all

(* Command-D duplicates the selected nodes; v changes only the viewport. *)
let run_duplicate_and_view () =
  List.iter (fun text ->
  let e = ref (editor text) and count = ref 0 in
  let step ?(mouse = (450., 300.)) ?(keys = []) events =
    incr count; e := E3.update !e (Test_editor_input.frame ~keys mouse events !count) in
  step []; step [];
  let centre () = Option.bind (E3.prepared !e) Mesh.centroid
    |> Option.map (fun p -> p.Rays_math.Vec3.x) in
  check (centre () = Some 20.) "the initial viewport did not show the graph result";
  step ~mouse:(150., 300.) [Input.Pointer_moved (150., 300.)];
  step ~mouse:(150., 300.) [Input.Scrolled (0., -1.)];
  step [];
  let gx, gy, _, _ = (E3.panes !e (frame (0., 0.) [] 0)).graph in
  let p = float (gx + 40), float (gy + 40) in
  step ~mouse:p [ Input.Pointer_moved p ];
  step ~mouse:p [ Input.Pointer_pressed (Input.LeftButton, p) ];
  step ~mouse:p [ Input.Pointer_released (Input.LeftButton, p) ];
  step ~mouse:p [ Input.Key_pressed Input.Home ];
  step ~mouse:p [ Input.Key_pressed (Input.KeyChar 'i') ];
  step ~mouse:p [];
  let selected () = dump_line !e "scope selected" in
  step ~mouse:p [ Input.Key_pressed Input.ArrowRight ];
  check (selected () <> "-") "a walk key selected a node";
  let node = selected () in
  let before = E3.workspace !e and label = E3.undo_label !e in
  step ~mouse:p [ Input.Key_pressed (Input.KeyChar 'v') ];
  check (E3.workspace !e == before && E3.undo_label !e = label)
    "v changed the workspace or history";
  check (not (has (fst (Flow.Lisp.print [ Editor_document.Layout_by_path.to_syntax (E3.workspace !e).Doc.layout ])) "(display"))
    "the viewed node is not saved in the layout";
  step ~mouse:p [];
  check (Node.operation (E3.displayed_node !e) = "box" && centre () = Some 0.)
    ("v marked the node but did not show it in the viewport: " ^ node);
  let x, y, w, _ = Option.get (E3.node_box !e ["g"; "b"]) in
  let target = float (x + w / 2), float (y + 3) in
  step ~mouse:target [Input.Pointer_moved target];
  step ~mouse:target [Input.Pointer_pressed (Input.LeftButton, target); Input.Pointer_released (Input.LeftButton, target)];
  step ~mouse:target [Input.Key_pressed (Input.KeyChar 'v')];
  step ~mouse:p [];
  check (centre () = Some 20.) "viewing the result did not restore its geometry";
  step ~mouse:p ~keys:[ Input.Meta ] [ Input.Key_pressed (Input.KeyChar 'd') ];
  check (E3.undo_label !e = Some "Duplicate" && has (source !e) "_2") ("Command-D made no copy: " ^ source !e);
  check (selected () <> node && selected () <> "-") "the copy is the selection";
  E3.close !e) [
    "(workspace w (graph g :context sop (let* [a (sop/box) b (sop/transform a :translate [20 0 0])] b)))";
    "(workspace w (graph g :context sop (let* [a (sop/box) b (sop/transform a :translate [20 0 0])] b))
       (graph scene :context scene (scene/merge (scene/geometry (ref g))
         (scene/camera :eye [0 0 6] :follow_viewport true))))"
  ]

(* An inline result is a geometry node too: viewing and editing it must follow the current
   lowering, including when an edit inserts a new plan node before the viewed one. *)
(* A preview follows the loop's selector and never writes a result into the source. *)
let run_loop_view () =
  let e = ref (editor {|(workspace loop_view
    (graph g :context sop
      (let* [a (sop/box)
             rows (for [i (range 3)] (sop/transform a :translate [(+ 5 (* i 10)) 0 0]))]
        (sop/merge rows)))
    (graph scene :context scene
      (scene/merge (scene/geometry (ref g))
        (scene/camera :eye [0 0 6] :follow_viewport true))))|}) in
  Fun.protect ~finally:(fun () -> E3.close !e) (fun () ->
    let count = ref 0 in
    let step ?(mouse = (450., 300.)) events =
      incr count; e := E3.update !e (frame mouse events !count) in
    let centre what expected =
      let actual = Option.map (fun p -> p.Rays_math.Vec3.x) (Option.bind (E3.prepared !e) Mesh.centroid) in
      check (match actual with Some x -> Float.abs (x -. expected) < 1e-4 | None -> false)
        (Printf.sprintf "%s: expected the geometry at %.1f, got %s" what expected
           (Option.fold ~none:"none" ~some:string_of_float actual)) in
    let click point =
      step ~mouse:point [Input.Pointer_moved point];
      step ~mouse:point [Input.Pointer_pressed (Input.LeftButton, point); Input.Pointer_released (Input.LeftButton, point)] in
    let key k = step [Input.Key_pressed k] in
    step []; step []; centre "the result" 15.;
    let gx, gy, _, _ = (E3.panes !e (frame (0., 0.) [] 0)).graph in
    click (float (gx + 40), float (gy + 40));
    key Input.Home; key (Input.KeyChar 'i'); step [];
    let inner = ["g"; "rows"; "@result"] in
    let x, y, w, _ = Option.get (E3.node_box !e inner) in
    click (float (x + w / 2), float (y + 3));
    check (dump_line !e "scope selected" = "g/rows/@result") "the node inside the loop could not be selected";
    let before = E3.workspace !e and label = E3.undo_label !e in
    key (Input.KeyChar 'v'); step [];
    check (E3.workspace !e == before && E3.undo_label !e = label)
      "viewing a loop body changed the document or history";
    centre "iteration zero" 5.;
    e := E3.set_probe !e ["g"; "rows"] 1; step [];
    centre "iteration one" 15.;
    e := E3.set_probe !e ["g"; "rows"] 2; step [];
    centre "iteration two" 25.)

let run_result_view () =
  let e = ref (editor {|(workspace result_view
    (graph g :context sop
      (let* [a (sop/box) b (sop/transform a :translate [5 0 0])
             result (sop/transform b :translate [15 0 0])]
        result))
    (graph scene :context scene
      (scene/merge (scene/geometry (ref g))
        (scene/camera :eye [0 0 6] :follow_viewport true))))|}) in
  Fun.protect ~finally:(fun () -> E3.close !e) (fun () ->
    let count = ref 0 in
    let step ?(mouse = (450., 300.)) events =
      incr count; e := E3.update !e (frame mouse events !count) in
    let centre expected =
      let actual = Option.map (fun p -> p.Rays_math.Vec3.x) (Option.bind (E3.prepared !e) Mesh.centroid) in
      check (actual = Some expected)
        (Printf.sprintf "VIEW kept stale geometry: expected %.1f, got %s" expected
           (Option.fold ~none:"none" ~some:string_of_float actual)) in
    let click point =
      step ~mouse:point [Input.Pointer_moved point];
      step ~mouse:point [Input.Pointer_pressed (Input.LeftButton, point); Input.Pointer_released (Input.LeftButton, point)] in
    let select name =
      let x, y, w, _ = Option.get (E3.node_box !e ["g"; name]) in
      click (float (x + w * (20 + 35 * (!count mod 4)) / 196), float (y + 3));  (* a different point each time: two presses on one are a double-click, which renames *)
      check (dump_line !e "scope selected" = "g/" ^ name) "the result node could not be selected" in
    let key k = step [Input.Key_pressed k] in
    let edit op = e := (match E3.edit !e op with Ok e -> e | Error m -> fail m); step [] in
    let parsed text = match S.parse text with Ok [e] -> e | _ -> fail "bad result-view expression" in
    step []; step []; centre 20.;
    let gx, gy, _, _ = (E3.panes !e (frame (0., 0.) [] 0)).graph in
    click (float (gx + 40), float (gy + 40));
    key Input.Home; key (Input.KeyChar 'i'); step [];
    select "b"; key (Input.KeyChar 'v'); step []; centre 5.;
    select "result"; key (Input.KeyChar 'v'); step [];
    centre 20.;
    edit (E.Set_arg {node = ["g"; "b"]; key = E.Kw "translate"; sub = []; value = parsed "[9 0 0]"});
    centre 24.;
    edit (E.Set_arg {node = ["g"; "result"]; key = E.Kw "translate"; sub = []; value = parsed "[30 0 0]"});
    centre 39.;
    select "b"; key (Input.KeyChar 'v'); step []; centre 9.;
    edit (E.Set_arg {node = ["g"; "b"]; key = E.Whole; sub = [];
      value = parsed "(sop/transform (sop/transform a :translate [2 0 0]) :translate [9 0 0])"});
    centre 11.;
    let vx, vy, _, _ = (E3.panes !e (frame (0., 0.) [] 0)).view in
    click (float (vx + 10), float (vy + 10)); key (Input.KeyChar 'f'); step [];
    check (Float.abs ((Easy_camera.target (E3.camera !e)).Rays_math.Vec3.x -. 11.) < 1e-6)
      "viewport F framed the graph result instead of the viewed node";
    let gx, gy, _, _ = (E3.panes !e (frame (0., 0.) [] 0)).graph in
    click (float (gx + 10), float (gy + 10)); key (Input.KeyChar 'u'); step [];
    check (dump_line !e "level" = "scene") "u did not leave the geometry network";
    centre 11.;
    click (float (gx + 40), float (gy + 40));
    key (Input.KeyChar 'i'); step [];
    select "result"; key (Input.KeyChar 'v'); step []; centre 41.)

let run_geometry_view () =
  let e = ref (editor {|(workspace geometry_view
    (graph g :context sop
      (let* [field (sop/curve (list [0 0 0] [10 0 0] [20 0 0]))
             dots (for [p (sop/point_list field)]
               (let* [moved (sop/transform (sop/box) :translate p)]
                 (sop/transform moved :translate [0 2 0]))) ]
        (sop/merge dots)))
    (graph scene :context scene
      (scene/merge (scene/geometry (ref g))
        (scene/camera :eye [0 0 6] :follow_viewport true))))|}) in
  Fun.protect ~finally:(fun () -> E3.close !e) (fun () ->
    let count = ref 0 in
    let step ?(mouse = (450., 300.)) events =
      incr count; e := E3.update !e (frame mouse events !count) in
    let click p = step ~mouse:p [Input.Pointer_moved p];
      step ~mouse:p [Input.Pointer_pressed (Input.LeftButton, p); Input.Pointer_released (Input.LeftButton, p)] in
    let centre expected =
      let x = Option.map (fun p -> p.Rays_math.Vec3.x) (Option.bind (E3.prepared !e) Mesh.centroid) in
      check (x = Some expected) "the geometry template did not preview the selected element" in
    step []; step []; centre 10.;
    let gx, gy, _, _ = (E3.panes !e (frame (0., 0.) [] 0)).graph in
    click (float (gx + 40), float (gy + 40));
    step [Input.Key_pressed Input.Home]; step [Input.Key_pressed (Input.KeyChar 'i')]; step [];
    let x, y, w, _ = Option.get (E3.node_box !e ["g";"dots";"moved"]) in
    click (float (x + w / 2), float (y + 3));
    let before = E3.workspace !e and label = E3.undo_label !e in
    step [Input.Key_pressed (Input.KeyChar 'v')]; step []; centre 0.;
    e := E3.set_probe !e ["g";"dots"] 2; step []; centre 20.;
    check (E3.workspace !e == before && E3.undo_label !e = label)
      "geometry preview and its selector changed the source or history";
    let vx, vy, _, _ = (E3.panes !e (frame (0., 0.) [] 0)).view in
    click (float (vx + 10), float (vy + 10)); step [Input.Key_pressed (Input.KeyChar 'f')]; step [];
    check (Float.abs ((Easy_camera.target (E3.camera !e)).Rays_math.Vec3.x -. 20.) < 1e-6)
      "viewport F did not frame the geometry preview";
    let panes = E3.panes !e (frame (0., 0.) [] 0) in
    let point = Camera.world_to_screen ~viewport:panes.view (E3.view_camera !e) (Rays_math.Vec3.create 20. 0. 0.) |> Option.get in
    click (point.x, point.y); step [];
    check (dump_line !e "scope selected" = "g/dots/moved" && E3.probe !e ["g";"dots"] = Some 2)
      "picking a preview selected the authored result or changed its iteration")

(* the inspector moves a list item up (Move_item), as the row arrow does *)
let run_movers () =
  let text = "(workspace w (graph g :context sop (let* [widths (list 1 2 3) r (for [i (range 2)] (sop/box :size [(nth widths i) 1 1]))] (sop/merge r))))" in
  let attempt y =
    let e = ref (editor text) and count = ref 0 in
    let step ?(mouse = (450., 300.)) events = incr count; e := E3.update !e (frame mouse events !count) in
    step []; step [];
    let gx, gy, _, _ = (E3.panes !e (frame (0., 0.) [] 0)).graph in
    let p = float (gx + 40), float (gy + 40) in
    step ~mouse:p [ Input.Pointer_moved p ];
    step ~mouse:p [ Input.Pointer_pressed (Input.LeftButton, p) ];
    step ~mouse:p [ Input.Pointer_released (Input.LeftButton, p) ];
    step ~mouse:p [ Input.Key_pressed Input.Home ];
    step ~mouse:p [ Input.Key_pressed (Input.KeyChar 'i') ];
    step ~mouse:p [];
    let bx, by, bw, bh = Option.get (E3.node_box !e [ "g"; "widths" ]) in
    let c = float (bx + bw / 2), float (by + 8) in
    step ~mouse:c [ Input.Pointer_moved c ];
    step ~mouse:c [ Input.Pointer_pressed (Input.LeftButton, c) ];
    step ~mouse:c [ Input.Pointer_released (Input.LeftButton, c) ];
    step ~mouse:c [];
    ignore bh;
    let ix, _, iw, _ = (E3.panes !e (frame (0., 0.) [] 0)).inspector in
    let q = float (ix + iw / 2), y in
    step ~mouse:q [ Input.Pointer_moved q ];
    step ~mouse:q [ Input.Pointer_pressed (Input.LeftButton, q) ];
    step ~mouse:q [ Input.Pointer_released (Input.LeftButton, q) ];
    step ~mouse:q [];
    let label = E3.undo_label !e and moved = has (source !e) "(list 1 3 2" || has (source !e) "(list 2 1 3" in
    E3.close !e; label, moved in
  let label, moved = attempt 210. in
  check (label = Some "Move item" && moved) "the inspector's Move item button did not move a list item up"

let run_frame_key () =
  (* the graph pane's keys reach the document: Shift-G frames the walked-to node (layout data, one entry);
     the rosette is entered as in test_text_pane's W9 scenario *)
  let e = ref (editor (case "rosette")) in
  let key k = Input.Key_pressed k in
  for n = 1 to 24 do
    let click p = [ Input.Pointer_moved p; Input.Pointer_pressed (Input.LeftButton, p); Input.Pointer_released (Input.LeftButton, p) ] in
    let events = match n with
      | 4 -> click (900., 300.) | 6 -> [ key Input.ArrowDown ] | 8 -> [ key (Input.KeyChar 'i') ]
      | 16 -> [ key Input.ArrowRight ] | 18 -> [ key Input.Shift; key (Input.KeyChar 'g') ] | _ -> [] in
    let f : Frame.t = { input = { (Input.at_time (float n /. 60.)) with
      dt = 1. /. 60.; frame = n; tick = n; size = 1400, 800; pointer = 640., 360.;
      keys = (if n = 18 then [ Input.Shift ] else []); events };
      pixel_scale = 1., 1.; fps = 60.; mouse_delta = 0., 0. } in
    e := E3.update !e f
  done;
  check (E3.undo_label !e = Some "Frame") ("g made a frame: " ^ Option.value ~default:"-" (E3.undo_label !e));
  check (not (Editor_document.Layout_by_path.Path_map.is_empty (E3.workspace !e).Doc.layout.frames))
    "the frame is in the layout";
  E3.close !e

let panes_graph e = let _, _, w, _ = (E3.panes e (frame (0., 0.) [] 0)).graph in w

(* the layout sections need the panels of variations, not its sphere lowerings *)
let cheap_variations = lazy (replace (replace (case "variations") "(sop/uv_sphere :radius 0.06 :segments 8 :rings 4)" "(sop/box)") "(range 4)" "(range 1)")

let run_ops () =
  let e = editor (Lazy.force cheap_variations) in
  let ok label op =
    match E3.edit e op with
    | Ok e' -> check (E3.undo_label e' = Some label) ("label " ^ label ^ ": " ^ Option.value ~default:"-" (E3.undo_label e')); e'
    | Error m -> fail (label ^ ": " ^ m) in
  let tree e = (shell_of (build_ok (E3.workspace e))).tree in
  let sh = tree e in
  (* resize *)
  let r = ok "Resize panel" (E.Set_layout_size { node = [ "editor"; "left" ]; size = `Ratio 0.3 }) in
  check (has (source r) "(ui/split-at \"vertical\" 0.3 network code)") "Set_layout_ratio";
  check (match (tree (ok "Resize panel" (E.Set_layout_size { node = [ "editor"; "left" ]; size = `Ratio 5. }))) with
      | Split { b = Split { a = Split { size; _ }; _ }; _ } -> size = `Ratio 0.9 | _ -> false) "a ratio clamps to 0.9";
  (* a ui/split (no ratio) becomes ui/split-at *)
  let plain = editor (with_editor "    (let* [a (ui/graph) b (ui/lisp) s (ui/split \"horizontal\" a b)] (ui/workspace s))") in
  (match E3.edit plain (E.Set_layout_size { node = [ "editor"; "s" ]; size = `Ratio 0.25 }) with
   | Ok p -> check (has (source p) "(ui/split-at \"horizontal\" 0.25 a b)") "ui/split gets a ratio"
   | Error m -> fail m);
  (* split, close, retype *)
  let s = ok "Split panel" (E.Split_panel { node = [ "editor"; "network" ]; axis = `V; ratio = 0.5; kind = None }) in
  check (Panels.to_string (tree s) = "(h outline (h (v (v graph lisp) lisp) (tile view)))") ("split: " ^ Panels.to_string (tree s));
  check (has (source s) "(ui/split-at \"vertical\" 0.5 network_a network_b)") "the new split";
  let c = ok "Close panel" (E.Close_panel { node = [ "editor"; "network" ] }) in
  check (Panels.to_string (tree c) = "(h outline (h lisp (tile view)))") ("close: " ^ Panels.to_string (tree c));
  let t = ok "Retype panel" (E.Set_panel_kind { node = [ "editor"; "network" ]; kind = "list" }) in
  check (Panels.to_string (tree t) = "(h outline (h (v list lisp) (tile view)))") ("retype: " ^ Panels.to_string (tree t));
  let sheet = ok "Retype panel" (E.Set_panel_kind { node = [ "editor"; "network" ]; kind = "spreadsheet" }) in
  check (Panels.to_string (tree sheet) = "(h outline (h (v spreadsheet lisp) (tile view)))") "retype spreadsheet";
  let saved = Doc.to_text (E3.workspace sheet) in
  check (Doc.to_text (of_text saved) = saved) "retyped spreadsheet did not round trip";
  let v = ok "Retype panel" (E.Set_panel_kind { node = [ "editor"; "outline" ]; kind = "viewport" }) in
  check (has (source v) "outline (ui/viewport (ref scene))") "a viewport is over the first scene graph";
  let docked = ok "Dock panel" (E.Dock_panel {node = ["editor"; "network"]; target = ["editor"; "outline"]; side = `Right}) in
  check (Panels.to_string (tree docked) = "(h (h outline graph) (h lisp (tile view)))")
    ("docking duplicated a panel or left a gap: " ^ Panels.to_string (tree docked));
  check (has (source docked) "(ui/split-at \"horizontal\" 0.5 outline network)")
    "docking was not reflected in the editor Lisp";
  (* undo restores the tree with the source *)
  check (Panels.to_string (tree e) = Panels.to_string sh) "undo target";
  (* refusals leave everything alone *)
  let refuse name op needle = match E3.edit e op with
    | Error m -> check (has m needle) (name ^ ": " ^ m)
    | Ok _ -> fail (name ^ " was accepted") in
  refuse "close the workspace" (E.Close_panel { node = [ "editor"; "shell" ] }) "Keep at least one";
  refuse "an unknown panel type" (E.Set_panel_kind { node = [ "editor"; "network" ]; kind = "dashboard" }) "Unknown panel type";
  refuse "resize a panel" (E.Set_layout_size { node = [ "editor"; "network" ]; size = `Ratio 0.4 }) "not a split";
  refuse "no such binding" (E.Split_panel { node = [ "editor"; "nothing" ]; axis = `H; ratio = 0.5; kind = None }) "no longer exists";
  let floating = editor (with_editor
    "(let* [main (ui/graph) outline (ui/outline) window (ui/floating outline) tiles (ui/tile main window)] (ui/workspace tiles))") in
  (match E3.edit floating (E.Close_panel {node = ["editor"; "outline"]}) with
   | Ok closed ->
       check (Panels.to_string (tree closed) = "(tile graph)") "closing a floating panel left its wrapper";
       check (not (has (source closed) "ui/floating")) "empty floating wrapper remained in Lisp";
       (match E3.edit closed (E.Close_panel {node = ["editor"; "main"]}) with
        | Error message -> check (has message "Keep at least one") "last panel refusal"
        | Ok _ -> fail "closing the last panel was accepted")
   | Error message -> fail message);
  let switched = editor (with_editor
    "(let* [main (ui/graph) outline (ui/outline) window (ui/floating outline)] (ui/workspace (ui/switch (ui/split-at \"horizontal\" 0.5 main window) (ui/lisp) :active 0)))") in
  (match E3.edit switched (E.Close_panel {node = ["editor"; "outline"]}) with
   | Ok closed ->
       check (not (has (source closed) "ui/floating") && has (source closed) "(ui/switch main (ui/lisp)")
         "closing a floating panel did not clean its switch branch";
       (match E3.edit closed (E.Close_panel {node = ["editor"; "main"]}) with
        | Error message -> check (has message "every layout") "empty switch branch refusal"
        | Ok _ -> fail "closing the last panel in a switch branch was accepted")
   | Error message -> fail message);
  (* a panel that follows a graph panel keeps following it when a window docks on that panel
     or is closed: :of names the panel, never the split it ends up in *)
  let followed = editor (with_editor
    "(let* [network (ui/graph) inspector (ui/inspector :of network)] (ui/workspace (ui/split-at \"horizontal\" 0.5 network inspector)))") in
  (match E3.edit followed (E.Layout_window {graph = "editor"; kind = "outline"}) with
   | Error message -> fail ("new window: " ^ message)
   | Ok windowed ->
       (match E3.edit windowed (E.Dock_panel {node = ["editor"; "inspector"]; target = ["editor"; "network"]; side = `Left}) with
        | Ok docked -> check (has (source docked) ":of network)") ("docking on a followed panel renamed :of: " ^ source docked)
        | Error message -> fail ("dock on a followed panel: " ^ message)));
  E3.close followed;
  E3.close switched; E3.close floating; E3.close plain; E3.close e

(* an editor graph that hides everything is valid; Restore layout brings the default back *)
let run_restore () =
  let e = ref (editor (Lazy.force cheap_variations)) and count = ref 0 in
  let step ?(mouse = (450., 300.)) events = incr count; e := E3.update !e (frame mouse events !count) in
  step []; step [];
  check (panes_graph !e > 0) "the shell starts with a graph panel";
  (* an invalid editor graph is refused whole: the layout and the document stay *)
  let before = source !e in
  (match E3.edit !e (E.Set_arg { node = [ "editor"; "panels" ]; key = Pos 1; sub = []; value = S.make (S.Num "0.95") }) with
   | Error _ -> ()
   | Ok _ -> fail "a ratio of 0.95 was accepted");
  check (source !e = before && panes_graph !e > 0) "a refused edit keeps the layout";
  (* a valid one that leaves only an outline: nothing to edit the layout with *)
  let bricked = E3.edit !e (E.Set_arg { node = [ "editor"; "shell" ]; key = Pos 0; sub = []; value =
    S.make (S.List [ S.make (S.Sym "ui/outline") ]) }) |> Result.get_ok in
  e := bricked;
  step [];
  check (panes_graph !e = 0) "the bricked shell has no graph panel";
  let key k = Input.Key_pressed k in
  step [ key (Input.KeyChar '/'); key (Input.KeyChar 'z') ]; step [];
  check (panes_graph !e > 0) "/ z restores the default layout";
  check (has (dump_line !e "cook") "Default layout") ("the status says so: " ^ dump_line !e "cook");
  check (source !e = source bricked) "the document is untouched by Restore";
  (* the editor graph is repaired from the restored shell: the layout edit ends the restore *)
  let repaired = E3.edit !e (E.Set_arg { node = [ "editor"; "shell" ]; key = Pos 0; sub = []; value = S.make (S.Sym "panels") })
    |> Result.get_ok in
  e := repaired; step [];
  check (panes_graph !e > 0 && not (has (dump_line !e "cook") "Default layout")) "editing the editor graph ends the restore";
  (* pressing it twice returns to the editor graph's layout *)
  e := bricked; step [];
  step [ key (Input.KeyChar '/'); key (Input.KeyChar 'z') ]; step [];
  step [ key (Input.KeyChar '/'); key (Input.KeyChar 'z') ]; step [];
  check (panes_graph !e = 0) "the second / z returns to the editor graph";
  E3.close !e

(* one orbit camera per viewport: the drag of the focused viewport moves only it, and focusing
   another keeps what each one showed *)
let run_camera_zoom () =
  let e = ref (editor ~camera:(Easy_camera.create ~inertia:false ())
    "(workspace zoom (graph g :context sop (sop/box)) (graph scene :context scene (scene/merge (scene/geometry (ref g)) (scene/camera :eye [0 0 12345.123456789] :follow_viewport true :active true))))") in
  let step events count = e := E3.update !e (frame (200., 200.) events count) in
  step [] 0; step [] 1;
  step [Input.Pointer_moved (200., 200.); Input.Pointer_pressed (Input.LeftButton, (200., 200.));
    Input.Pointer_released (Input.LeftButton, (200., 200.))] 2;
  for count = 3 to 15 do
    step [Input.Scrolled (0., -1.)] (2 * count);
    let viewport = Easy_camera.camera (E3.camera !e) and saved = E3.render_camera !e in
    check (Rays_math.Vec3.nearly_equal (Camera.position viewport) (Camera.position saved) ~eps:1e-6)
      "zooming out made the following camera lag behind the viewport";
    let eye = Camera.position viewport in
    step [] ((2 * count) + 1);
    check (Rays_math.Vec3.nearly_equal eye (Camera.position (Easy_camera.camera (E3.camera !e))) ~eps:1e-6)
      "an idle frame snapped the viewport to rounded camera coordinates"
  done;
  E3.close !e

let run_cameras () =
  let e = ref (editor ~camera:(Easy_camera.with_inertia false (Easy_camera.create ~target:Rays_math.Vec3.zero ~distance:7. ())) (case "variations")) and count = ref 0 in
  let step ?(buttons = []) ?(delta = (0., 0.)) (x, y) events =
    incr count; e := E3.update !e (Test_editor_input.frame ~buttons ~delta (x, y) events !count) in
  for _ = 1 to 8 do step (450., 300.) [] done;
  let geometry () = Layout.geometry ~hidden:[ Layout.Timeline ] (shell_of (build_ok (E3.workspace !e))).tree
      (frame (0., 0.) [] 0) in
  let center key =
    let leaf = Option.get (Layout.find (geometry ()) (Layout.View key)) in
    let x, y, w, h = leaf.body in float (x + (w / 2)), float (y + (h / 2)) in
  let position key = Camera.position (E3.viewport_camera !e key) in
  let keys = [ "v1.1.0"; "v1.1.1"; "v1.1.2"; "v1.1.3" ] in
  let click key =
    let p = center key in
    step p [ Input.Pointer_moved p ];
    step ~buttons:[ Input.LeftButton ] p [ Input.Pointer_pressed (Input.LeftButton, p) ];
    step p [ Input.Pointer_released (Input.LeftButton, p) ]; step p [] in
  (* a press at the point of the click just before would be a double click *)
  let orbit key delta =
    let x, y = let x, y = center key in x, y +. 40. in
    step (x, y) [ Input.Pointer_moved (x, y) ];
    step ~buttons:[ Input.LeftButton ] (x, y) [ Input.Pointer_pressed (Input.LeftButton, (x, y)) ];
    for k = 1 to 6 do
      let p = x +. float k *. fst delta, y +. float k *. snd delta in
      step ~buttons:[ Input.LeftButton ] ~delta p [ Input.Pointer_moved p ]
    done;
    let last = x +. 6. *. fst delta, y +. 6. *. snd delta in
    step last [ Input.Pointer_released (Input.LeftButton, last) ]; step last [] in
  let same a b = Rays_math.Vec3.nearly_equal a b ~eps:1e-9 in
  click "v1.1.1";
  let start = List.map (fun k -> k, position k) keys in
  check (List.for_all (fun (_, p) -> same p (snd (List.hd start))) start) "viewports start from one orbit";
  orbit "v1.1.1" (20., 0.);
  check (not (same (position "v1.1.1") (List.assoc "v1.1.1" start))) "a drag did not orbit the focused viewport";
  List.iter (fun k -> check (same (position k) (List.assoc k start)) (k ^ " moved with another viewport's drag"))
    [ "v1.1.0"; "v1.1.2"; "v1.1.3" ];
  let first = position "v1.1.1" in
  click "v1.1.2";
  check (same (position "v1.1.1") first) "focusing another viewport moved the one left";
  orbit "v1.1.2" (-20., 10.);
  check (not (same (position "v1.1.2") (List.assoc "v1.1.2" start)) && not (same (position "v1.1.2") first))
    "the second viewport did not orbit on its own";
  check (same (position "v1.1.1") first && same (position "v1.1.0") (List.assoc "v1.1.0" start))
    "the second drag disturbed the first viewport";
  click "v1.1.1";
  check (same (position "v1.1.1") first) "returning to a viewport lost its orbit";
  (* a click on the geometry of a viewport over another scene instance selects the node that made
     it: the pane shows that graph and the node is selected there *)
  let graph_pane =
    let geometry = Layout.geometry ~hidden:[ Layout.Timeline ] (shell_of (build_ok (E3.workspace !e))).tree
        (frame (0., 0.) [] 0) in
    let x, y, _, _ = (Option.get (Layout.find geometry Layout.Graph)).body in
    float (x + 20), float (y + 200) in
  step graph_pane [ Input.Pointer_moved graph_pane ];
  step ~buttons:[ Input.LeftButton ] graph_pane [ Input.Pointer_pressed (Input.LeftButton, graph_pane) ];
  step graph_pane [ Input.Pointer_released (Input.LeftButton, graph_pane) ];
  let key k = Input.Key_pressed k and ch c = Input.Key_pressed (Input.KeyChar c) in
  step graph_pane [ key (Input.KeyChar '/'); ch 'l'; ch 'g' ]; step graph_pane [];
  check (has (dump_line !e "projection") "graph") ("the pane is not showing a graph: " ^ dump_line !e "projection");
  let scope_selected () = dump_line !e "scope selected" in
  click "v1.1.2";
  check (dump_line !e "pane graph" = "garden" && scope_selected () <> "-")
    (Printf.sprintf "a click in another instance's viewport selected nothing (pane %s, selected %s)"
       (dump_line !e "pane graph") (scope_selected ()));
  E3.close !e

(* two roots, two Worlds, two viewports: the editor composes and draws a view for each *)
let run_instances () =
  let text = {|(workspace days
  (graph g :context sop (sop/box))
  (graph noon :context world (world/sun (world/sky :name "noon") :name "sun"))
  (graph dusk :context world (world/sun (world/sky :name "dusk") :name "sun"))
  (graph set :context scene
    (scene/merge (scene/geometry (ref g) :name "body") (scene/camera :name "hero")))
  (graph day :context scene
    (scene/root (scene/merge (ref set) (scene/world (ref noon) :name "Noon")) :renderer "Raster"))
  (graph night :context scene
    (scene/root (scene/merge (ref set) (scene/world (ref dusk) :name "Dusk" :exposure -2))
                :renderer "Path traced" :max_spp 1024 :bounces 12))
  (graph editor :context editor
    (ui/workspace (ui/split-at "horizontal" 0.5 (ui/viewport (ref day)) (ui/viewport (ref night))))))|} in
  let e = ref (editor text) in
  for c = 1 to 12 do e := E3.update !e (frame (450., 300.) [] c) done;
  check (List.length (E3.objects !e) >= 1 && Option.is_some (E3.prepared !e))
    (Printf.sprintf "the editor did not cook (cook %s)" (dump_line !e "cook"));
  check (not (has (dump_line !e "edit error") "Failure")) ("an error composing the views: " ^ dump_line !e "edit error");
  E3.close !e

(* the frame draws one 3D layer per viewport, each over its own scene instance *)
let run_views () =
  let e = ref (editor (case "variations")) in
  for c = 1 to 12 do e := E3.update !e (frame (450., 300.) [] c) done;
  check (List.length (E3.objects !e) >= 1 && Option.is_some (E3.prepared !e))
    (Printf.sprintf "the editor did not cook (%d objects, prepared %b, cook %s)" (List.length (E3.objects !e))
       (Option.is_some (E3.prepared !e)) (dump_line !e "cook"));
  for c = 200 to 230 do e := E3.update !e (frame (450., 300.) [] c) done;
  let scene = E3.scene !e (frame (450., 300.) [] 231) in
  Fun.protect ~finally:(fun () -> E3.close !e) (fun () ->
  match Scene.Private.stage_native ~width:900 ~height:640 scene with
  | Error m -> fail m
  | Ok staged ->
      let viewports = List.length (List.filter (function Scene.Private.Scene3_layer _ -> true | _ -> false) staged.layers) in
      check (viewports = 4) (Printf.sprintf "four viewports draw four 3D layers, got %d" viewports))

(* ---- the copies of a loop are one template, through the editor ---- *)

let count_of text piece =
  let n = String.length piece in
  let rec go i acc = if i + n > String.length text then acc
    else go (i + 1) (if String.sub text i n = piece then acc + 1 else acc) in
  go 0 0

(* the list's rows, walked with the arrows: the row under the focus is the selection *)
let select_row e count k =
  let step ?(mouse = (450., 300.)) events = incr count; e := E3.update !e (frame mouse events !count) in
  step []; step [];
  let gx, gy, _, _ = (E3.panes !e (frame (0., 0.) [] 0)).graph in
  let p = float (gx + 40), float (gy + 400) in
  step ~mouse:p [ Input.Pointer_pressed (Input.LeftButton, p); Input.Pointer_released (Input.LeftButton, p) ];
  step [ Input.Key_pressed Input.Home ];
  for _ = 1 to k do step [ Input.Key_pressed Input.ArrowDown ] done;
  step []

(* With RAYS_UI_PREVIEW=<dir> the editor is drawn into <dir>/<name>.png, without a window: the
   picture a pointer position in these tests is read off. *)
let preview e name = match Sys.getenv_opt "RAYS_UI_PREVIEW" with
  | None -> ()
  | Some directory ->
      let canvas = Rays.Canvas.create_exn ~width:900 ~height:640 in
      Result.get_ok (Rays.Canvas.render canvas (E3.scene !e (frame (0., 0.) [] 0)));
      ignore (Rays.Canvas.save_png canvas (Filename.concat directory (name ^ ".png")));
      ignore(Rays.Canvas.destroy canvas)

(* a drag across the inspector's slider at [row] points under the panel's top *)
let drag_slider e count row =
  let step ?(buttons = []) ?(mouse = (450., 300.)) events = incr count; e := E3.update !e (frame ~buttons mouse events !count) in
  let ix, iy, _, _ = (E3.panes !e (frame (0., 0.) [] 0)).inspector in
  (* the first field of a vector row starts at the control column: 12 + 6 + 8 + 56 + 8 = 90 in a
     180-wide panel; a point 10 inside it *)
  let at x = float (ix + 100 + x), float (iy + row) in
  step ~mouse:(at 0) ~buttons:[ Input.LeftButton ] [ Input.Pointer_pressed (Input.LeftButton, at 0) ];
  List.iter (fun x -> step ~mouse:(at x) ~buttons:[ Input.LeftButton ] []) [ 4; 8; 12; 16 ];
  step ~mouse:(at 20) [ Input.Pointer_released (Input.LeftButton, at 20) ];
  step []

let objects e = Edit_graph.inspect (E3.scene_document e)
let field_of (i : Edit_graph.node_info) name = List.find_map (fun (f : Param.field_view) ->
  if f.name = name then Some f.current else None) (Node.parameter_fields i.node)

let run_loop_copies () =
  (* copies that differ only by the graph they place: every field of the template is a literal *)
  let text = {|(workspace copies
    (graph g :context sop [(seed : int 1)] (sop/box :size [seed 1 1]))
    (graph scene :context scene
      (let* [row (for [i (range 3)] (scene/geometry (ref g :seed (+ i 1))))]
        (scene/merge row))))|} in
  let e = ref (editor text) and count = ref 0 in
  select_row e count 1;
  preview e "loop-copies";
  let x () = List.map (fun i -> field_of i "translate_x") (objects !e) in
  check (List.length (objects !e) = 3 && List.for_all (fun v -> v = Some (Param.Float_value 0.)) (x ()))
    "the loop did not make three objects at the origin";
  (* the head is 118 tall at the test's 11 points, then Inputs (4 + 24, 24), Geometry (16 + 24, two switch
     rows) and Transform (16 + 24): the translate row is 118 + 52 + 88 + 40 = 298 down, its middle 12 more *)
  drag_slider e count 310;
  let moved = x () in
  check (List.for_all (fun v -> v = List.hd moved && v <> Some (Param.Float_value 0.)) moved)
    "dragging a literal field of one copy did not move all three";
  check (count_of (source !e) ":translate [" = 1) ("the template was not edited once: " ^ source !e);
  check (E3.undo_label !e = Some "Set translate_x") "the drag is not one history entry named for the field";
  check (has (dump_line !e "cook") "loop template" && has (dump_line !e "cook") "3 copies")
    ("the status does not say the template changed: " ^ dump_line !e "cook");
  let saved = source !e in
  E3.close !e;
  e := editor saved;
  check (x () = moved) "the saved text does not reopen with the same copies";
  (* one undo gives them all back *)
  let undo_frame = frame (450., 300.) [ Input.Key_pressed (Input.KeyChar 'z') ] (incr count; !count) in
  e := E3.update !e { undo_frame with input = { undo_frame.input with keys = [ Input.Meta ] } };
  E3.close !e;
  (* a rename from the list is the template's :name: every copy takes it *)
  let e = ref (editor text) and count = ref 0 in
  select_row e count 2;
  let step events = incr count; e := E3.update !e (frame (450., 300.) events !count) in
  step [ Input.Key_pressed Input.F2 ]; step [ Input.Text_input "spot" ]; step [ Input.Key_pressed Input.Enter ]; step [];
  check (List.for_all (fun (i : Edit_graph.node_info) -> i.label = "gspot") (objects !e)
         && count_of (source !e) ":name \"gspot\"" = 1 && E3.undo_label !e = Some "Rename")
    ("a rename of one copy did not rename the template: " ^ source !e ^ String.concat "," (List.map (fun (i : Edit_graph.node_info) -> i.label) (objects !e)));
  E3.close !e;
  (* a copy computed from its index has no value of its own *)
  let text = {|(workspace lamps
    (graph g :context sop (sop/box))
    (graph scene :context scene
      (let* [row (for [i (range 3)] (scene/light :name "lamp" :translate [i 2 0]))]
        (scene/merge (scene/geometry (ref g)) row))))|} in
  let e = ref (editor text) and count = ref 0 in
  select_row e count 2;
  let before = source !e in
  preview e "loop-light";
  (* the head 118, Inputs (4 + 24, 24), Light (16 + 24, five rows) and Transform (16 + 24), then 12 into
     the translate row: 118 + 52 + 160 + 40 + 12 = 382 *)
  drag_slider e count 382;
  check (source !e = before && E3.undo_label !e = None) "a computed field of a copy was written";
  check (has (dump_line !e "edit error") "computed by the loop") ("no reason for the refused edit: " ^ dump_line !e "edit error");
  (* deleting one copy skips its iteration: the others stay where they are *)
  select_row e count 2;
  e := E3.update !e (frame (450., 300.) [ Input.Key_pressed Input.Delete ] (incr count; !count));
  let lamps () = List.filter (fun (i : Edit_graph.node_info) -> i.operation = "light") (objects !e) in
  check (List.length (lamps ()) = 2 && has (source !e) ":skip [1]" && E3.undo_label !e = Some "Delete")
    ("deleting a copy did not skip it in the loop: " ^ source !e);
  check (List.map (fun i -> field_of i "translate_x") (lamps ())
         = [ Some (Param.Float_value 0.); Some (Param.Float_value 2.) ])
    "the copies after a deleted one moved";
  E3.close !e;
  (* a loop with two clauses: the same, one entry, and one undo gives the copy back *)
  let text = {|(workspace grid
    (graph g :context sop (sop/box))
    (graph scene :context scene
      (let* [row (for [i (range 2) j (range 3)] (scene/light :translate [i j 0]))]
        (scene/merge (scene/geometry (ref g)) row))))|} in
  let e = ref (editor text) and count = ref 0 in
  select_row e count 2;
  let before = source !e in
  e := E3.update !e (frame (450., 300.) [ Input.Key_pressed Input.Delete ] (incr count; !count));
  check (has (source !e) ":skip [1]" && List.length (objects !e) = 6 && E3.undo_label !e = Some "Delete"
         && dump_line !e "prompt" = "-")
    ("a copy of a two-clause loop was not deleted exactly: " ^ source !e);
  let saved = source !e in
  let undo_frame = frame (450., 300.) [ Input.Key_pressed (Input.KeyChar 'z') ] (incr count; !count) in
  e := E3.update !e { undo_frame with input = { undo_frame.input with keys = [ Input.Meta ] } };
  check (source !e = before && List.length (objects !e) = 7) "one undo did not give the deleted copy back";
  E3.close !e;
  e := editor saved;
  check (List.length (objects !e) = 6 && source !e = saved) "the saved text does not reopen without the copy";
  E3.close !e;
  print_endline "workspace shell: a loop's copies are one template (edit, computed refusal, exact delete, undo) ok"

(* an expression typed in a row of a copy is the template's argument: every copy follows it *)
let run_loop_expression () =
  let text = {|(workspace lamps
    (graph g :context sop (sop/box))
    (graph scene :context scene
      (let* [row (for [i (range 3)] (scene/light :name "lamp" :translate [i 2 0]))]
        (scene/merge (scene/geometry (ref g)) row))))|} in
  let e = ref (editor text) and count = ref 0 in
  select_row e count 2;
  let step ?(mouse = (450., 300.)) events = incr count; e := E3.update !e (frame mouse events !count) in
  let ix, iy, _, _ = (E3.panes !e (frame (0., 0.) [] 0)).inspector in
  (* Option-click on the translate X field of the lamp: its row is 12 + 6 + 8 + 56 + 8 = 90 from
     the panel's left; vertically the head (118), Inputs (28, 24), Light (40, five rows) and Transform (40)
     and half the translate row (12) above it, as in the preview *)
  let label = float (ix + 100), float (iy + 118 + 28 + 24 + 40 + 120 + 40 + 12) in
  step ~mouse:label [ Input.Pointer_moved label ];
  let alt ?(buttons = []) events = incr count;
    e := E3.update !e (Test_editor_input.frame ~buttons ~keys:[ Input.Alt ] label events !count) in
  alt [ Input.Pointer_pressed (Input.LeftButton, label) ] ~buttons:[ Input.LeftButton ];
  alt [ Input.Pointer_released (Input.LeftButton, label) ];
  step ~mouse:label [ Input.Key_pressed (Input.KeyChar 'a') ];
  step ~mouse:label [ Input.Text_input "=(* i 3)" ];
  step ~mouse:label [ Input.Key_pressed Input.Enter ];
  step ~mouse:label [];
  let lamps () = List.filter (fun (i : Edit_graph.node_info) -> i.operation = "light") (objects !e) in
  check (has (source !e) ":translate [(* i 3) 2 0]") ("the expression did not reach the template: " ^ source !e);
  check (List.map (fun i -> field_of i "translate_x") (lamps ())
         = List.map (fun v -> Some (Param.Float_value v)) [ 0.; 3.; 6. ])
    "the copies did not follow the expression";
  check (E3.undo_label !e = Some "Edit expression" && dump_line !e "edit error" = "-"
         && has (dump_line !e "cook") "loop template") "the expression edit is not one entry with its status";
  let saved = source !e in
  E3.close !e;
  e := editor saved;
  check (List.map (fun i -> field_of i "translate_x") (lamps ())
         = List.map (fun v -> Some (Param.Float_value v)) [ 0.; 3.; 6. ]) "the saved text does not reopen with the same copies";
  E3.close !e;
  print_endline "workspace shell: an expression typed in a copy edits the template ok"

let run_panel_states () =
  let directory = Filename.temp_dir "rays-panel-state" "" in
  let e = ref (editor ~presets:directory "(workspace panels (graph g :context sop (sop/box)))")
  and count = ref 0 in
  let step ?(buttons = []) ?(keys = []) mouse events = incr count;
    e := E3.update !e (Test_editor_input.frame ~buttons ~keys mouse events !count) in
  let click point = step point [Input.Pointer_moved point];
    step point [Input.Pointer_pressed (Input.LeftButton, point); Input.Pointer_released (Input.LeftButton, point)];
    step point [] in
  step (450., 300.) []; step (450., 300.) [];
  let geometry = Layout.geometry ~hidden:[Layout.Timeline] Layout.default (frame (0., 0.) [] 0) in
  let splitter = List.find (fun (s : Layout.splitter) -> s.node = Some []) geometry.splitters in
  let sx, sy, sw, sh = splitter.bounds in
  let grip = float (sx + sw / 2), float (sy + sh / 2) in
  let moved = fst grip +. 30., snd grip in
  let before_split = Doc.to_text (E3.workspace !e) in
  step grip [Input.Pointer_moved grip];
  step ~buttons:[Input.LeftButton] grip [Input.Pointer_pressed (Input.LeftButton, grip)];
  step ~buttons:[Input.LeftButton] moved [Input.Pointer_moved moved];
  check (Doc.to_text (E3.workspace !e) = before_split && E3.undo_label !e = None)
    "a held splitter drag committed authored state before release";
  step moved [Input.Pointer_released (Input.LeftButton, moved)]; step moved [];
  check ((E3.workspace !e).layout.editor = None && has (source !e) "ui/split-at"
         && E3.undo_label !e = Some "Resize panel")
    "resizing the default shell did not write its editor graph";
  let saved_shell = (build_ok (E3.workspace !e)).Document.shell in
  check ((build_ok (of_text (Doc.to_text (E3.workspace !e)))).Document.shell = saved_shell)
    "the first splitter edit did not round-trip through Lisp";
  step ~keys:[Input.Meta] moved [Input.Key_pressed (Input.KeyChar 'z')];
  check ((build_ok (E3.workspace !e)).Document.shell = None)
    "undoing the first splitter edit did not restore the default shell";
  click (889., 11.);
  let state key = Option.value ~default:Editor_core.Panels.default_state
    (Editor_document.Layout_by_path.Path_map.find_opt ["editor"; key] (E3.workspace !e).layout.panels) in
  check ((state "inspector").collapsed && E3.undo_label !e = Some "Collapse panel"
         && (let _, _, w, _ = (E3.panes !e (frame (0., 0.) [] 0)).inspector in w = 0))
    "collapsing a panel did not persist or remove its body";
  let saved = Doc.to_text (E3.workspace !e) in
  check ((of_text saved).layout.panels = (E3.workspace !e).layout.panels)
    "panel state did not round-trip through Lisp";
  click (889., 11.);
  check (not (state "inspector").collapsed) "the collapsed header could not expand the panel";
  let gx, gy, _, _ = (E3.panes !e (frame (0., 0.) [] 0)).graph in
  let start = float (gx + 8), float (gy - 11) in
  let moved = fst start +. 30., snd start +. 70. in
  step start [Input.Pointer_moved start];
  step ~buttons:[Input.LeftButton] start [Input.Pointer_pressed (Input.LeftButton, start)];
  step ~buttons:[Input.LeftButton] moved [Input.Pointer_moved moved];
  step ~buttons:[Input.LeftButton] (fst moved +. 10., snd moved) [Input.Pointer_moved (fst moved +. 10., snd moved)];
  step moved [Input.Pointer_released (Input.LeftButton, moved)]; step moved [];
  check ((state "network").window <> None && E3.undo_label !e = Some "Arrange panel")
    "the panel drag handle did not undock the panel";
  let window = Option.get (state "network").window in
  let x, y, w, h = window in
  let grip = float (x + 8), float (y + 11) in
  let to_ = fst grip +. 35., snd grip +. 30. in
  let before_move = Doc.to_text (E3.workspace !e) in
  let before_bounds = (E3.panes !e (frame (0., 0.) [] 0)).graph in
  step grip [Input.Pointer_moved grip];
  step ~buttons:[Input.LeftButton] grip [Input.Pointer_pressed (Input.LeftButton, grip)];
  step ~buttons:[Input.LeftButton] to_ [Input.Pointer_moved to_];
  check (Doc.to_text (E3.workspace !e) = before_move)
    "a held floating-window drag committed authored state before release";
  step to_ [Input.Pointer_cancelled Input.LeftButton];
  check (Doc.to_text (E3.workspace !e) = before_move
    && (E3.panes !e (frame (0., 0.) [] 0)).graph = before_bounds)
    "cancelling a floating-window drag retained draft bounds or changed authored state";
  step grip [Input.Pointer_moved grip];
  step ~buttons:[Input.LeftButton] grip [Input.Pointer_pressed (Input.LeftButton, grip)];
  step ~buttons:[Input.LeftButton] to_ [Input.Pointer_moved to_];
  step to_ [Input.Pointer_released (Input.LeftButton, to_)]; step to_ [];
  check ((state "network").window = Some (x + 35, y + 30, w, h))
    "an underlying pane covered the floating drag handle";
  step ~keys:[Input.Meta] to_ [Input.Key_pressed (Input.KeyChar 'z')];
  check ((state "network").window = Some window) "a second window drag merged with the previous gesture";
  let visible_h = min h (640 - y) in
  let corner = float (x + w - 4), float (y + visible_h - 4) in
  let to_ = fst corner -. 35., snd corner -. 40. in
  let before_resize = Doc.to_text (E3.workspace !e) in
  step corner [Input.Pointer_moved corner];
  step ~buttons:[Input.LeftButton] corner [Input.Pointer_pressed (Input.LeftButton, corner)];
  step ~buttons:[Input.LeftButton] to_ [Input.Pointer_moved to_];
  check (Doc.to_text (E3.workspace !e) = before_resize)
    "a held floating-window resize committed authored state before release";
  step to_ [Input.Pointer_released (Input.LeftButton, to_)]; step to_ [];
  check ((state "network").window = Some (x, y, w - 35, visible_h - 40))
    "the floating window resize handle did not persist its size";
  step ~keys:[Input.Meta] to_ [Input.Key_pressed (Input.KeyChar 'z')];
  step ~keys:[Input.Meta] moved [Input.Key_pressed (Input.KeyChar 'z')];
  check ((state "network").window = None && not (state "inspector").collapsed)
    "one undo did not revert the whole window drag";
  (* Drop onto the viewport's lower edge: a docked split, preserved panel types, one undo. *)
  let before = Doc.to_text (E3.workspace !e) in
  let gx, gy, _, _ = (E3.panes !e (frame (0., 0.) [] 0)).graph in
  let start = float (gx + 8), float (gy - 11) and target = (180., 580.) in
  step start [Input.Pointer_moved start];
  step ~buttons:[Input.LeftButton] start [Input.Pointer_pressed (Input.LeftButton, start)];
  step ~buttons:[Input.LeftButton] target [Input.Pointer_moved target];
  step ~buttons:[Input.LeftButton] target [];
  step target [Input.Pointer_released (Input.LeftButton, target)]; step target [];
  check (has (source !e) "preview network)" && (state "network").window = None
         && dump_line !e "edit error" = "-")
    ("the edge drop did not dock the panel: " ^ dump_line !e "edit error" ^ "\n" ^ source !e);
  step ~keys:[Input.Meta] target [Input.Key_pressed (Input.KeyChar 'z')];
  check (Doc.to_text (E3.workspace !e) = before) "one undo did not revert the docking gesture";
  step target [Input.Key_pressed (Input.KeyChar '/'); Input.Key_pressed (Input.KeyChar 'g')]; step target [];
  check ((state "network").collapsed) "the leader graph toggle was not saved";
  step target [Input.Key_pressed (Input.KeyChar '/'); Input.Key_pressed (Input.KeyChar 'a')];
  check (not (state "network").collapsed) "Add did not expand the saved collapsed graph";
  step target [];
  step target [Input.Key_pressed Input.Escape]; step target [];
  step target [Input.Key_pressed (Input.KeyChar '/'); Input.Key_pressed (Input.KeyChar 't')]; step target [];
  check (Option.fold ~none:false ~some:(fun (state : Panels.state) -> not state.collapsed)
      (Editor_document.Layout_by_path.Path_map.find_opt ["editor"; "@panel"; "-1"]
        (E3.workspace !e).layout.panels))
    "the leader timeline toggle was not saved";
  let prepared = E3.prepared !e in
  e := E3.set_renderer !e Rays_editor.Renderer.Wireframe;
  step target [];
  check (E3.renderer !e = Rays_editor.Renderer.Wireframe && Option.equal ( == ) (E3.prepared !e) prepared)
    "the shared wireframe choice recooked the geometry";
  e := E3.set_renderer !e Rays_editor.Renderer.Raster;
  step target [];
  check (E3.renderer !e = Rays_editor.Renderer.Raster && Option.equal ( == ) (E3.prepared !e) prepared)
    "the shared raster choice recooked the geometry";
  E3.close !e;
  Array.iter (fun name -> let path = Filename.concat directory name in
    if Sys.is_directory path then begin
      Array.iter (fun name -> Sys.remove (Filename.concat path name)) (Sys.readdir path); Unix.rmdir path
    end else Sys.remove path) (Sys.readdir directory);
  Unix.rmdir directory

let run_ref_picker () =
  let text = {|(workspace ref_pick
    (graph scene :context scene
      (let* [geom (scene/geometry (ref g1))]
        (scene/merge geom)))
    (graph g1 :context sop (sop/box))
    (graph g2 :context sop (sop/grid))
    (graph editor :context editor
      (ui/workspace (ui/split "horizontal" (ui/graph "scene") (ui/inspector)))))|} in
  let e = ref (editor text) and count = ref 0 in
  let step ?(mouse = (450., 300.)) events = incr count; e := E3.update !e (frame mouse events !count) in
  step []; step [];
  let ix, iy, _, _ = (E3.panes !e (frame (0., 0.) [] 0)).inspector in
  let click point =
    step ~mouse:point [ Input.Pointer_moved point ];
    step ~mouse:point [ Input.Pointer_pressed (Input.LeftButton, point); Input.Pointer_released (Input.LeftButton, point) ] in
  let bx, by, _, _ = Option.get (E3.node_box !e [ "scene"; "geom" ]) in
  let node_pt = float (bx + 60), float (by + 12) in
  click node_pt;
  step [];
  check (dump_line !e "scope selected" = "scene/geom") ("the scene geometry node was not selected, got: " ^ dump_line !e "scope selected");
  preview e "ref-picker";
  let btn = float (ix + 180), float (iy + 190) in
  click btn;
  step [];
  preview e "ref-picker-open";
  let opt = float (ix + 180), float (iy + 245) in
  click opt;
  step [];
  check (has (source !e) "(ref g2)") ("inspector ref picker did not change ref to g2: " ^ source !e);
  let undo_frame = frame (450., 300.) [ Input.Key_pressed (Input.KeyChar 'z') ] (incr count; !count) in
  e := E3.update !e { undo_frame with input = { undo_frame.input with keys = [ Input.Meta ] } };
  step [];
  check (has (source !e) "(ref g1)") ("undo did not restore ref to g1: " ^ source !e);
  E3.close !e

(* / a on a scene graph: World, Merge and Geometry are one gesture each, one undo entry *)
let run_compose () =
  let text = {|(workspace compose
    (graph g1 :context sop (sop/box))
    (graph scene :context scene
      (let* [a (scene/geometry (ref g1))
             b (scene/geometry (ref g1) :translate [3 0 0] :name "b")]
        (scene/root (scene/merge a b))))
    (graph editor :context editor (ui/workspace (ui/split "horizontal" (ui/graph "scene") (ui/inspector)))))|} in
  let key k = Input.Key_pressed k and ch c = Input.Key_pressed (Input.KeyChar c) in
  let pick typed =
    let e = ref (editor text) and count = ref 0 in
    let step ?(keys = []) events = incr count; e := E3.update !e (Test_editor_input.frame ~keys (450., 300.) events !count) in
    step []; step [];
    let before = source !e in
    step [ key (Input.KeyChar '/'); ch 'a' ]; step [ Input.Text_input typed ]; step [ key Input.Enter ]; step [];
    let after = source !e and label = E3.undo_label !e in
    step ~keys:[ Input.Meta ] [ ch 'z' ]; step [];
    let undone = source !e in
    E3.close !e;
    before, after, label, undone in
  let flat t = String.concat " " (List.filter (( <> ) "") (String.split_on_char ' '
    (String.map (function '\n' -> ' ' | c -> c) t))) in
  let before, after, label, undone = pick "World" in
  check (has (flat after) "(scene/world (ref sky))" && has (flat after) "(graph sky :context world" && label = Some "Add World"
         && undone = before) ("/ a World: " ^ after);
  let before, after, label, undone = pick "Geometry" in
  check (has (flat after) "(graph shape :context sop" && has (flat after) "(scene/geometry (ref shape))"
         && label = Some "Add geometry" && undone = before) ("/ a Geometry: " ^ after);
  let before, after, label, undone = pick "Merge" in
  check (has (flat after) "(scene/merge)" && undone = before && label <> None) ("/ a Merge: " ^ after);
  (* / e selects the scene's World, a merge member, and enters it *)
  let e = ref (editor {|(workspace sky
    (graph g1 :context sop (sop/box))
    (graph sky :context world (world/sun (world/sky)))
    (graph scene :context scene
      (scene/root (scene/merge (scene/geometry (ref g1)) (scene/world (ref sky) :name "Dome")))))|}) and count = ref 0 in
  let step events = incr count; e := E3.update !e (frame (450., 300.) events !count) in
  step []; step [];
  step [ key (Input.KeyChar '/'); ch 'e' ]; step [];
  check (E3.level !e = Some "Dome" && not (has (source !e) "graph world"))
    "/ e did not enter the World of a scene/world member";
  E3.close !e;
  (* / e on a scene with a root and no World makes one: a member of the merge and a world graph *)
  let e = ref (editor {|(workspace plain
    (graph g1 :context sop (sop/box))
    (graph scene :context scene (scene/root (scene/merge (scene/geometry (ref g1))))))|}) and count = ref 0 in
  let step events = incr count; e := E3.update !e (frame (450., 300.) events !count) in
  step []; step [];
  step [ key (Input.KeyChar '/'); ch 'e' ]; step [];
  check (has (flat (source !e)) "(scene/world (ref sky)" && has (source !e) "graph sky :context world"
         && has (source !e) "scene/root" && E3.undo_label !e = Some "Add World")
    ("/ e did not write the new World: " ^ source !e);
  E3.close !e;
  print_endline "workspace shell: / a adds World, Geometry (with its SOP graph) and Merge, one undo entry each; / e enters a World member"

(* A switch holds the layouts of one editor graph: lowering, the names read from the panels, the
   edits, one merged undo entry, and the keys. *)
let switch_text = {|
  (workspace sw
    (graph g :context sop (sop/box))
    (graph scene :context scene
      (scene/merge (scene/geometry (ref g)) (scene/camera :eye [0 0 6])))
    (graph editor :context editor
      (let* [preview (ui/viewport (ref scene))
             network (ui/graph)
             inspector (ui/inspector)
             code (ui/lisp)
             build (ui/split-at "horizontal" 0.4 preview (ui/split-at "horizontal" 0.62 network inspector))
             write (ui/split-at "horizontal" 0.46 preview (ui/split-at "vertical" 0.5 network code))
             look preview]
        (ui/workspace (ui/switch build write look :active 0)))))
|}

let run_layouts () =
  let labels text = match (shell_of (build_ok (of_text text))).switch with
    | Some sw -> Panels.labels sw.layouts, sw.active | None -> fail "no switch" in
  check (labels switch_text = ([ "View | Graph | Inspector"; "View | Graph / Lisp"; "View" ], 0))
    "the layouts are named from their panels";
  let shell = shell_of (build_ok (of_text switch_text)) in
  check (Panels.to_string shell.tree = "(h view (h graph inspector))" && shell.origins <> []
         && List.assoc_opt [] shell.origins = Some (Document.Bound "build"))
    "the active layout is the tree, its origin the binding the switch names";
  let shell = shell_of (build_ok (of_text (replace switch_text ":active 0" ":active 1"))) in
  check (Panels.to_string shell.tree = "(h view (v graph lisp))") "the switch is transparent: :active picks the tree";
  expect_error "an active layout out of range" (replace switch_text ":active 0" ":active 3") "active layout";
  (* edits *)
  let ws = of_text switch_text in
  let edit ws op = match Doc.edit catalog ws op with Ok ws -> ws | Error d -> fail (Diagnostic.to_string d) in
  let text ws = fst (Flow.Lisp.print ws.Doc.source) in
  let switched = edit ws (E.Set_layout { graph = "editor"; index = 2 }) in
  check (has (text switched) ":active 2") "Set_layout writes :active";
  let added = edit switched (E.Layout_new { graph = "editor" }) in
  check (has (text added) "(ui/switch build write look layout :active 3)" && has (text added) "layout preview")
    ("a new layout copies the active one: " ^ text added);
  let labelled = labels (text added) in
  check (labelled = ([ "View | Graph | Inspector"; "View | Graph / Lisp"; "View"; "View \xc2\xb7 View 100% (1)"; ], 3)
         || fst labelled = [ "View | Graph | Inspector"; "View | Graph / Lisp"; "View \xc2\xb7 View 100%"; "View \xc2\xb7 View 100% (2)" ])
    ("the copy is named like its twin: " ^ String.concat " / " (fst labelled));
  let removed = edit added (E.Layout_remove { graph = "editor" }) in
  check (has (text removed) "(ui/switch build write look :active" && not (has (text removed) "layout preview"))
    ("removing the current layout drops its input and binding: " ^ text removed);
  let one = edit (edit (edit removed (E.Set_layout { graph = "editor"; index = 0 })) (E.Layout_remove { graph = "editor" }))
      (E.Layout_remove { graph = "editor" }) in
  (match Doc.edit catalog one (E.Layout_remove { graph = "editor" }) with
   | Error d -> check (has d.Diagnostic.message "last layout") "the last layout stays"
   | Ok _ -> fail "removed the last layout");
  (* a window joins the active layout only; it floats and docks *)
  let floated = edit ws (E.Layout_float { graph = "editor"; at = [ 1; 0 ] }) in
  check (Panels.label (shell_of (build_ok floated)).tree = "View | Inspector + Graph") ("a panel floats: " ^ text floated);
  let docked = edit floated (E.Layout_float { graph = "editor"; at = [ 1; 0 ] }) in
  check (Panels.label (shell_of (build_ok docked)).tree = "View | Inspector | Graph") ("a window docks: " ^ text docked);
  (* a switch inside a split edits in place *)
  let nested = of_text (with_editor "    (ui/workspace (ui/split-at \"horizontal\" 0.5 (ui/graph) (ui/switch (ui/lisp) (ui/inspector) :active 1)))") in
  let n_set = edit nested (E.Set_layout { graph = "editor"; index = 0 }) in
  check (has (text n_set) "(ui/switch (ui/lisp) (ui/inspector) :active 0)" || has (text n_set) ":active 0")
    ("Set_layout reaches a switch inside a split: " ^ text n_set);
  check (Panels.to_string (shell_of (build_ok n_set)).tree = "(h graph lisp)") "and the column swaps";
  (* an older file: several editor graphs become the layouts of one switch *)
  let old_text =
    let t = with_editor "    (let* [a (ui/graph) b (ui/lisp) s (ui/split \"horizontal\" a b)] (ui/workspace s))" in
    String.sub t 0 (String.length t - 2)
    ^ "\n  (graph second :context editor\n    (let* [a (ui/lisp) b (ui/inspector) s (ui/split \"vertical\" a b)] (ui/workspace s))))\n" in
  let old = of_text old_text in
  let merged = edit old (E.Merge_layouts { graph = "editor" }) in
  check (not (has (text merged) "(graph second") && has (text merged) "second_a" && has (text merged) "(ui/switch")
    ("the other editor graph joins the switch: " ^ text merged);
  (match (shell_of (build_ok merged)).switch with
   | Some sw -> check (List.length sw.layouts = 2 && List.map Panels.label sw.layouts = [ "Graph | Lisp"; "Lisp / Inspector" ])
       ("the merged layouts: " ^ String.concat " / " (List.map Panels.label sw.layouts))
   | None -> fail "no switch after merging");
  (match Doc.edit catalog merged (E.Merge_layouts { graph = "editor" }) with
   | Error _ -> () | Ok _ -> fail "merged with nothing to merge");
  (* a workspace without a switch gets one on the first new layout *)
  let plain = of_text (replace (replace switch_text "(ui/switch build write look :active 0)" "build") "write (" "write (") in
  let wrapped = edit plain (E.Layout_new { graph = "editor" }) in
  check (has (text wrapped) "(ui/workspace switch)" && has (text wrapped) "(ui/switch build layout :active 1)")
    ("a switch is written around the tree: " ^ text wrapped);
  (* one merged undo entry, by keys *)
  let e = ref (editor switch_text) and count = ref 0 in
  let step ?(keys = []) events = incr count; e := E3.update !e (Test_editor_input.frame ~keys (300., 150.) events !count) in
  step []; step [];
  let key k = Input.Key_pressed k and ch c = Input.Key_pressed (Input.KeyChar c) in
  step [ key (Input.KeyChar '/'); ch '[' ]; step [];  (* which-key lists the layouts *)
  step [ ch '1' ]; step [];
  check (has (source !e) ":active 1" && E3.undo_label !e = Some "Layout") "/ [ 1 switches the layout";
  step [ key (Input.KeyChar '/'); ch '['; ch '2' ]; step [];
  check (has (source !e) ":active 2") "/ [ 2 switches again";
  step ~keys:[ Input.Meta ] [ ch 'z' ]; step [];
  check (has (source !e) ":active 0") "one undo returns past both switches";
  step [ key (Input.KeyChar '/'); ch '['; ch 'n' ]; step [];
  check (has (source !e) ":active 3" && E3.undo_label !e = Some "New layout") "/ [ n copies the layout";
  step [ key (Input.KeyChar '/'); ch '['; ch 'x' ]; step [];
  check (E3.undo_label !e = Some "Remove layout" && not (has (source !e) "layout preview")) "/ [ x removes it";
  step [ key (Input.KeyChar '/'); ch 'n' ]; step [ ch '4' ]; step [];
  check (has (source !e) "(ui/floating (ui/inspector))" && E3.undo_label !e = Some "New window") "/ n 4 opens an inspector window";
  (* the layout's one docked panel cannot float: refused, nothing written, the entry stays *)
  let before = source !e in
  step [ key (Input.KeyChar '/'); ch 'o'; ch 'f' ]; step [];
  check (E3.undo_label !e = Some "New window" && source !e = before)
    ("/ o f on a layout's only docked panel is refused: " ^ Option.value ~default:"-" (E3.undo_label !e));
  (* in a layout of several panels the focused one floats, as one entry named Float panel *)
  step [ key (Input.KeyChar '/'); ch '['; ch '0' ]; step [];
  let gx, gy, gw, gh = (E3.panes !e (Test_editor_input.frame (0., 0.) [] !count)).graph in
  let at = (float gx +. float gw /. 2., float gy +. float gh /. 2.) in
  let press events = incr count; e := E3.update !e (Test_editor_input.frame at events !count) in
  press [ Input.Pointer_moved at ]; press [ Input.Pointer_pressed (Input.LeftButton, at) ];
  press [ Input.Pointer_released (Input.LeftButton, at) ]; press [];
  press [ key (Input.KeyChar '/'); ch 'o'; ch 'f' ]; press [];
  check (E3.undo_label !e = Some "Float panel" && has (source !e) "(ui/floating")
    ("/ o f floats the focused panel: " ^ Option.value ~default:"-" (E3.undo_label !e) ^ "\n" ^ source !e)

(* The inspector with nothing selected has a Scene root section over the root's settings: a press
   on one of its controls writes the root in the text as one "Render settings" entry. *)
let run_root_section () =
  let e = ref (editor {|(workspace studio
  (graph g :context sop (sop/box))
  (graph scene :context scene
    (let* [body (scene/geometry (ref g) :name "body")
           all (scene/merge body)]
      (scene/root all :renderer "Raster" :width 800 :height 600 :max_spp 64))))|}) and count = ref 0 in
  let step ?(mouse = (450., 300.)) events = incr count; e := E3.update !e (frame mouse events !count) in
  for _ = 1 to 6 do step [] done;
  let before = E3.workspace !e in
  check (E3.undo_label !e = None && has (source !e) ":width 800") "the root is in the text, nothing to undo";
  let ix, iy, iw, ih = (E3.panes !e (frame (0., 0.) [] !count)).inspector in
  check (iw > 0 && ih > 0) "the inspector panel is showing";
  (* the Renderer row of the Scene root section: the header (85 points at the test's 11-point
     text), the live-update switch, the Viewport section and its four switches, then the Camera,
     Render and Scene root section headers.  A press opens its menu under the field; the second
     row of the menu is Wireframe. *)
  let hit = ref None in
  (* now: the head is 90 (12 + 41 + 4 + 24 + 8 + 1), the switch row has 4 above it, the open Scene root
     follows the closed Render with 16 of space *)
  (* the live-update switch sits under its own section header now: 24 more *)
  let row = iy + 90 + 24 + 4 + 24 + 40 + 96 + 40 + 24 + 40 in
  let press at =
    step ~mouse:at [ Input.Pointer_moved at ];
    step ~mouse:at [ Input.Pointer_pressed (Input.LeftButton, at); Input.Pointer_released (Input.LeftButton, at) ];
    step ~mouse:at [] in
  let x = float (ix + (iw * 3 / 4)) in
  press (x, float (row + 12));
  press (x, float (row + 23 + 6 + 24 + 12));
  if E3.undo_label !e = Some "Render settings" then hit := Some row;
  check (!hit <> None) "no control of the inspector wrote the root";
  check (E3.workspace !e != before && has (source !e) "(scene/root all") "the root stayed in the text, edited";
  incr count;
  e := E3.update !e (Test_editor_input.frame ~keys:[ Input.Meta ] (450., 300.) [ Input.Key_pressed (Input.KeyChar 'z') ] !count);
  check (E3.workspace !e == before) "one undo returns the root";
  E3.close !e

(* b on a scene object takes it out of the render without removing it (:visible), Alt-Down moves a
   merge's input, and each gesture prints what it wrote in the status strip. *)
let run_hide_and_order () =
  let text = {|(workspace hide
    (graph g1 :context sop (sop/box))
    (graph scene :context scene
      (let* [a (scene/geometry (ref g1) :name "a")
             b (scene/geometry (ref g1) :translate [3 0 0] :name "b")
             all (scene/merge a b)]
        (scene/root all)))
    (graph editor :context editor (ui/workspace (ui/split "horizontal" (ui/graph "scene") (ui/inspector)))))|} in
  let e = ref (editor text) and count = ref 0 in
  let step ?(keys = []) ?(mouse = (450., 300.)) events =
    incr count; e := E3.update !e (Test_editor_input.frame ~keys mouse events !count) in
  let key k = Input.Key_pressed k and ch c = Input.Key_pressed (Input.KeyChar c) in
  step []; step [];
  let click path =
    let bx, by, bw, bh = Option.get (E3.node_box !e path) in
    let at = float bx +. float bw /. 2., float by +. float bh -. 6. in
    step ~mouse:at [ Input.Pointer_moved at ];
    step ~mouse:at [ Input.Pointer_pressed (Input.LeftButton, at) ];
    step ~mouse:at [ Input.Pointer_released (Input.LeftButton, at) ]; step ~mouse:at [] in
  click [ "scene"; "a" ];
  step [ ch 'b' ]; step [];
  check (has (source !e) "(scene/geometry (ref g1) :name \"a\" :visible false)" && E3.undo_label !e = Some "Edit value")
    ("b wrote :visible false on the object: " ^ source !e);
  check (has (dump_line !e "cook") "Wrote :visible false on scene/a")
    ("the strip says what was written: " ^ dump_line !e "cook");
  step [ ch 'b' ]; step [];
  check (has (source !e) ":visible true") ("b again shows it: " ^ source !e);
  step ~keys:[ Input.Meta ] [ ch 'z' ]; step ~keys:[ Input.Meta ] [ ch 'z' ]; step [];
  check (not (has (source !e) ":visible")) "two undos give the text back";
  (* Alt-Down on a hovered merge input swaps it with the next *)
  let bx, by, bw, bh = Option.get (E3.node_box !e [ "scene"; "all" ]) in
  click [ "scene"; "all" ];
  let moved = ref false in
  let y = ref (by + 4) in
  while not !moved && !y < by + bh do
    let at = float (bx + bw / 2), float !y in
    step ~mouse:at [ Input.Pointer_moved at ];
    step ~keys:[ Input.Alt ] ~mouse:at [ key Input.ArrowDown ]; step ~mouse:at [];
    if has (source !e) "(scene/merge b a)" then moved := true;
    y := !y + 3
  done;
  check !moved ("Alt-Down moved a merge input: " ^ source !e);
  check (E3.undo_label !e = Some "Move item") "one entry named Move item";
  E3.close !e

(* The palette's "Copy workspace as Lisp" puts the text Command-S writes on the clipboard.  A
   window-free run has no SDL clipboard: then the row still runs and says why it could not. *)
let run_copy_lisp () =
  let e = ref (editor {|(workspace copied
  (graph g :context sop (sop/box))
  (graph scene :context scene (scene/merge (scene/geometry (ref g)))))|}) and count = ref 0 in
  let step events = incr count; e := E3.update !e (frame (450., 300.) events !count) in
  let key k = Input.Key_pressed k in
  step []; step [];
  (* whether this run has a clipboard is asked first, so each case has one outcome that passes *)
  let available = Result.is_ok (Rays.Clipboard.set_text "probe") in
  step [ key (Input.KeyChar '/'); Input.Key_pressed (Input.KeyChar '/') ]; step [];
  step [ Input.Text_input "copy workspace" ]; step [ key Input.Enter ]; step [];
  let note = dump_line !e "cook" in
  if available then begin
    check (has note "Copied the workspace as Lisp") ("the palette row did not copy: " ^ note);
    match Rays.Clipboard.get_text () with
    | Ok clip -> check (has clip "(workspace copied" && has clip "(scene/geometry (ref g))")
        ("the clipboard does not hold the workspace text: " ^ clip)
    | Error message -> fail ("the clipboard took the text and gives none back: " ^ message)
  end else check (has note "Clipboard: ") ("without a clipboard the palette row did not say so: " ^ note);
  check (E3.undo_label !e = None) "copying changes nothing";
  E3.close !e

(* ---- the studio layout (flow.md 11.11): fixed sizes, strips, start state, several graph panels ---- *)

let sketch name = In_channel.with_open_bin
    (Filename.concat "../../../sketches" (name ^ ".rays")) In_channel.input_all

let run_studio () =
  let text = sketch "ws_layout" in
  let ws = of_text text in
  let doc = build_ok ws in
  let shell = shell_of doc in
  check (Panels.to_string shell.tree = "(h outline (h (v (v view timeline) graph) (v inspector lisp)))")
    ("the studio tree: " ^ Panels.to_string shell.tree);
  (* the target at three window sizes: every fixed side keeps its points *)
  List.iter (fun (w, h) ->
    let sample_frame = frame (0., 0.) [] 0 in
    let g = Layout.geometry ~hidden:[] shell.tree { sample_frame with input = { sample_frame.input with size = w, h } } in
    let whole pick = match List.find_opt (fun (l : Layout.leaf) -> pick l.panel) g.leaves with
      | Some { frame; _ } -> frame
      | None -> fail "the studio lost a panel" in
    let expect what got want = check (got = want) (Printf.sprintf "studio at %dx%d: %s" w h what) in
    let tree = h - 25 and middle = w - 216 - 320 - 2 in
    expect "outline" (whole (( = ) Layout.Outline)) (0, 0, 216, tree);
    expect "viewport" (whole (function Layout.View _ -> true | _ -> false)) (217, 0, middle, tree - 336 - 24 - 2);
    expect "timeline" (whole (( = ) Layout.Timeline)) (217, tree - 336 - 24 - 1, middle, 24);
    expect "graph" (whole (( = ) Layout.Graph)) (217, tree - 336, middle, 336);
    expect "inspector" (whole (( = ) Layout.Inspector)) (w - 320, 0, 320, tree - 300 - 1);
    expect "lisp" (whole (( = ) Layout.Lisp)) (w - 320, tree - 300, 320, 300);
    expect "status" g.status_at (0, tree, w, 25))
    [ 1440, 900; 1920, 1080; 1100, 700 ];
  (* what the panels' keywords open the editor with *)
  let preview = match List.map fst shell.preview_sources with [ key ] -> key | _ -> fail "one viewport" in
  check (shell.named = [ [ 1; 0; 1 ], "garden" ] && shell.start.focus = Some [ 1; 0; 1 ]
         && shell.start.looking = [ preview ] && shell.start.tabs = [ [ 1; 1; 1 ], "graph" ])
    "the start state of the studio's panels";
  (* print and re-read *)
  let again = shell_of (build_ok (of_text (Doc.to_text ws))) in
  check (again.tree = shell.tree && again.named = shell.named && again.start = shell.start
         && again.switch = shell.switch)
    "the studio did not round-trip through its printed text";
  let apply ws op = match Doc.edit catalog ws op with
    | Ok ws -> ws | Error d -> fail (E.label op ^ ": " ^ Diagnostic.to_string d) in
  let text_of ws = fst (Flow.Lisp.print ws.Doc.source) in
  let refused name ws op needle = match Doc.edit catalog ws op with
    | Error d -> check (has (Diagnostic.to_string d) needle) (name ^ ": " ^ Diagnostic.to_string d)
    | Ok _ -> fail (name ^ " was accepted") in
  (* a gutter drag writes what the split is sized by *)
  let sized = apply ws (E.Set_layout_size { node = [ "editor"; "studio" ]; size = `First 260 }) in
  check (has (text_of sized) "(ui/split \"horizontal\" outline work :first_size 260)") "a fixed side is written in points";
  let ratio = apply ws (E.Set_layout_size { node = [ "editor"; "stage" ]; size = `Ratio 0.61803 }) in
  check (has (text_of ratio) "(ui/split-at \"vertical\" 0.618 preview timeline)") "a ratio keeps four decimals";
  refused "two fixed sides" ws (E.Set_arg { node = [ "editor"; "studio" ]; key = E.Kw "second_size"; sub = [];
                                            value = S.make (S.Num "100") }) "one side";
  (* the keyword rows of the cards are the graph's view of it *)
  let rows name = match Flow_graph.Projection.find (Flow_graph.Projection.of_graph catalog ws.checked "editor") [ "editor"; name ] with
    | Some (n : Flow_graph.Projection.node) -> List.map (fun (r : Flow_graph.Projection.row) -> r.label) n.rows
    | None -> fail ("no card " ^ name) in
  check (List.mem "first_size" (rows "studio") && List.mem "second_size" (rows "studio")
         && List.mem "look_through" (rows "preview") && List.mem "focus" (rows "network")
         && List.mem "view" (rows "network") && List.mem "tab" (rows "code"))
    "the layout keywords are not rows of their cards";
  (* the layout commands take a graph whose result is a binding of the workspace *)
  let laid = List.fold_left apply ws [
    E.Layout_new { graph = "editor" }; E.Layout_window { graph = "editor"; kind = "inspector" };
    E.Layout_float { graph = "editor"; at = [ 0; 0 ] }; E.Set_layout { graph = "editor"; index = 0 };
    E.Layout_remove { graph = "editor" } ] in
  check (has (text_of laid) "shell (ui/workspace") "the workspace binding was not kept";
  check (not (has (text_of laid) "(-> ")) "a layout was printed as a thread";
  ignore (build_ok (of_text (Doc.to_text laid)));
  (* diagnostics *)
  let bad name body needle = expect_error name (with_editor ("    " ^ body)) needle in
  bad "an unknown graph" "(ui/workspace (ui/graph \"nope\"))" "E_UNKNOWN_GRAPH";
  bad "a fixed side of nothing" "(ui/workspace (ui/split \"vertical\" (ui/graph) (ui/lisp) :first_size 0))" "1 point";
  bad "an unknown view" "(ui/workspace (ui/graph :view \"tree\"))" "view";
  (* through the editor: the start state, the points a drag writes, two graph panels *)
  let e = ref (editor text) and count = ref 0 in
  let step ?(buttons = []) mouse events = incr count; e := E3.update !e (frame ~buttons mouse events !count) in
  let click point = step point [ Input.Pointer_moved point ];
    step ~buttons:[ Input.LeftButton ] point [ Input.Pointer_pressed (Input.LeftButton, point) ];
    step point [ Input.Pointer_released (Input.LeftButton, point) ]; step point [] in
  step (450., 100.) []; step (450., 100.) [];
  check (dump_line !e "focus" = "Graph" && dump_line !e "pane graph" = "garden") "the focus did not start in the graph panel";
  check (String.starts_with ~prefix:"graph tab" (dump_line !e "text")) ("the lisp panel's tab: " ^ dump_line !e "text");
  check (E3.look_through !e) "the viewport did not start looking through the render camera";
  step (216.5, 100.) [ Input.Pointer_moved (216.5, 100.) ];
  step ~buttons:[ Input.LeftButton ] (216.5, 100.) [ Input.Pointer_pressed (Input.LeftButton, (216.5, 100.)) ];
  step ~buttons:[ Input.LeftButton ] (180.2, 100.) [ Input.Pointer_moved (180.2, 100.) ];
  step (180.2, 100.) [ Input.Pointer_released (Input.LeftButton, (180.2, 100.)) ]; step (180.2, 100.) [];
  check (has (source !e) "(ui/split \"horizontal\" outline work :first_size 180)" && E3.undo_label !e = Some "Resize panel")
    ("the gutter of a fixed split did not write points: " ^ source !e);
  (* the second layout: a floating graph on the material beside the docked graph, each its own pane *)
  e := (match E3.edit !e (E.Set_layout { graph = "editor"; index = 1 }) with Ok e -> e | Error m -> fail m);
  step (450., 100.) []; step (450., 100.) [];
  check (dump_line !e "graph panels" = "garden, clay") ("the graph panels: " ^ dump_line !e "graph panels");
  check (dump_line !e "windows" = "graph 560 90 520 360, view 250 470 380 260, inspector 1090 60 320 420, lisp 640 500 420 300")
    ("the windows: " ^ dump_line !e "windows");
  click (500., 200.);
  check (dump_line !e "pane graph" = "clay") "a press in the floating graph did not make it the pane in use";
  click (230., 420.);
  check (dump_line !e "pane graph" = "garden" && dump_line !e "graph panels" = "garden, clay")
    "the docked graph did not keep its own graph";
  E3.close !e;
  (* every panel is an instance (flow.md 11.11): at 1440x900 the second layout has an inspector
     and a text pane floating beside the docked ones; each draws, scrolls and types on its own *)
  let e = ref (editor (replace text ":active 0" ":active 1")) and count = ref 0 and probes = ref [] in
  let big mouse events = incr count; let base_frame = frame mouse events !count in
    { base_frame with input = { base_frame.input with size = 1440, 900 } } in
  (* the sketch's own rows end every inspector: a tall filler whose place says where that inspector
     has scrolled to *)
  let step ?(buttons = []) mouse events =
    probes := [];
    let frame = big mouse events in
    e := fst (E3.update_with !e { frame with input = { frame.input with buttons } } ~inspector:(fun ui ->
      let filler = Pxui.Ui.box ui ~h:(Pxui.Ui.Px 1500.) "filler" in
      let _, y, _, _ = Pxui.Ui.rect ui filler in probes := !probes @ [ y ])) in
  let click point = step point [ Input.Pointer_moved point ];
    step ~buttons:[ Input.LeftButton ] point [ Input.Pointer_pressed (Input.LeftButton, point) ];
    step point [ Input.Pointer_released (Input.LeftButton, point) ]; step point [] in
  let rest point = step point []; step point []; step point [] in
  (* the floating inspector's labels run to x 1199 (window 1090 + 109): a point left of that is no field *)
  let docked = 1300., 530. and floating = 1110., 300. in
  rest (700., 200.);
  let at_rest = match !probes with [ a; b ] -> a, b | l -> fail (Printf.sprintf "%d inspectors drew their rows, not 2" (List.length l)) in
  let scroll point = step point [ Input.Pointer_moved point ];
    for _ = 1 to 6 do step point [ Input.Scrolled (0., -8.) ] done; rest point;
    match !probes with [ a; b ] -> a, b | _ -> fail "an inspector stopped drawing" in
  let d1, f1 = scroll floating in
  check (d1 = fst at_rest && f1 <> snd at_rest)
    (Printf.sprintf "the floating inspector did not scroll on its own: %g %g, then %g %g" (fst at_rest) (snd at_rest) d1 f1);
  let d2, f2 = scroll docked in
  check (d2 <> d1 && f2 = f1) "the docked inspector did not scroll on its own";
  (* a click in a floating panel focuses it *)
  click floating;
  check (has (dump_line !e "panels") "inspector details*") ("a click did not focus the floating inspector: " ^ dump_line !e "panels");
  (* typing in the floating text pane drafts there, not in the docked one *)
  let pane name = List.find (fun l -> String.starts_with ~prefix:("lisp " ^ name) l)
    (List.map String.trim (String.split_on_char ';' (dump_line !e "panels"))) in
  check (has (pane "code") "graph tab, draft no" && has (pane "notes") "document tab, draft no")
    ("each text pane opens on its own tab: " ^ dump_line !e "panels");
  click (800., 660.);
  step (800., 660.) [ Input.Text_input " " ]; rest (800., 660.);
  check (has (pane "notes") "lisp notes*: document tab, draft yes" && has (pane "code") "graph tab, draft no")
    ("typing in one text pane reached the other: " ^ dump_line !e "panels");
  click (1280., 720.);
  step (1280., 720.) [ Input.Text_input " " ]; rest (1280., 720.);
  check (has (pane "code") "lisp code*: graph tab, draft yes" && has (pane "notes") "document tab, draft yes")
    ("the docked text pane does not draft on its own: " ^ dump_line !e "panels");
  E3.close !e;
  ()

(* slow: the start keywords followed again, and the Size row *)
let run_studio_keywords () =
  let text = sketch "ws_layout" in
  (* the start keywords are followed again when their value in the text changes, and only then *)
  let e = ref (editor text) and count = ref 0 in
  let big mouse events = incr count; let base_frame = frame mouse events !count in
    { base_frame with input = { base_frame.input with size = 1440, 900 } } in
  let step ?(buttons = []) mouse events = let frame = big mouse events in
    e := E3.update !e { frame with input = { frame.input with buttons } } in
  let click point = step point [ Input.Pointer_moved point ];
    step ~buttons:[ Input.LeftButton ] point [ Input.Pointer_pressed (Input.LeftButton, point) ];
    step point [ Input.Pointer_released (Input.LeftButton, point) ]; step point [] in
  let at = 600., 700. in
  let rest () = step at []; step at [] in
  let set name key value =
    e := (match E3.edit !e (E.Set_arg { node = [ "editor"; name ]; key = E.Kw key; sub = []; value = S.make value }) with
      | Ok e -> e | Error m -> fail m);
    rest () in
  rest ();
  check (dump_line !e "projection" = "graph" && E3.look_through !e) "the studio opens as its keywords say";
  set "network" "view" (S.Str "list");
  check (dump_line !e "projection" = "list") "a changed :view was not followed";
  step at [ Input.Key_pressed (Input.KeyChar '/'); Input.Key_pressed (Input.KeyChar 'l') ];
  step at [ Input.Key_pressed (Input.KeyChar 'g') ]; rest ();
  check (dump_line !e "projection" = "graph") "/ l g did not show the graph";
  e := (match E3.edit !e (E.Set_layout_size { node = [ "editor"; "stage" ]; size = `Ratio 0.4 }) with Ok e -> e | Error m -> fail m);
  rest ();
  check (dump_line !e "projection" = "graph") "an edit that left :view alone reset the view the user chose";
  set "network" "view" (S.Str "graph"); set "preview" "look_through" (S.Sym "false");
  check (not (E3.look_through !e)) "a changed :look_through was not followed";
  (* the Size row of a split's card converts it, keeping the sizes it shows: the editor graph in
     the pane, the studio card selected, the row's menu *)
  step at [ Input.Key_pressed (Input.KeyChar '/'); Input.Key_pressed (Input.KeyChar 'j') ];
  step at [ Input.Text_input "editor" ]; step at [ Input.Key_pressed Input.Enter ]; rest ();
  (let x, y, w, h = Option.get (E3.node_box !e [ "editor"; "studio" ]) in click (float (x + w / 2), float (y + max 1 (h / 2))));
  check (dump_line !e "scope selected" = "editor/studio") ("the studio card is not where the test clicks: " ^ dump_line !e "scope selected");
  (* the 320-wide inspector starts at y 24; the split's head is 153 tall (chips 12 + 24 + 4, the 40-point
     name in 48, 4, the detail line, 8, the 20-point buttons, 8 and its rule), then the Split section (4 above its
     header, 24) and the Size row, whose middle is 24 + 153 + 28 + 12 = 217; its menu opens 1 below the field *)
  (* Dismiss in the status strip: the graph background now clears selection. *)
  let pick dy = click (700., 887.); click (1390., 217.); click (1390., 217. +. dy) in
  pick 27.;
  check (has (source !e) "studio (ui/split-at \"horizontal\" 0.1505 outline work)" && E3.undo_label !e = Some "Resize panel")
    ("the Size row did not size the split by its ratio: " ^ source !e);
  pick 75.;
  check (has (source !e) "studio (ui/split \"horizontal\" outline work :second_size 1223)")
    ("the Size row did not fix the second side at its points: " ^ source !e);
  E3.close !e;
  ()

let run_studio_tail () =
  (* a graph panel in text view is live without the focus, and takes a gesture on the first press *)
  let e = ref (editor (with_editor "    (ui/workspace (ui/split \"horizontal\" (ui/graph \"garden\" :focus true) (ui/graph \"scene\" :view \"text\")))"))
  and count = ref 0 in
  let step ?(buttons = []) mouse events = incr count; e := E3.update !e (frame ~buttons mouse events !count) in
  step (100., 300.) []; step (100., 300.) [];
  check (dump_line !e "pane graph" = "garden" && dump_line !e "projection" = "graph"
         && has (dump_line !e "panels") "graph 0*; graph 1: selection tab, draft no")
    ("the second graph panel does not hold its own text pane: " ^ dump_line !e "panels");
  let point = 650., 300. in
  step point [ Input.Pointer_moved point ];
  step ~buttons:[ Input.LeftButton ] point [ Input.Pointer_pressed (Input.LeftButton, point) ];
  step point [ Input.Pointer_released (Input.LeftButton, point) ];
  step point [ Input.Text_input " " ]; step point []; step point [];
  check (dump_line !e "pane graph" = "scene" && has (dump_line !e "panels") "graph 1*: selection tab, draft yes")
    ("the first press in an unfocused text view did not start an edit there: " ^ dump_line !e "panels");
  E3.close !e;
  (* a float that is the bound name takes the window of its binding *)
  let floated = editor (with_editor "    (let* [win (ui/floating (ui/inspector))]\n      (ui/workspace (ui/split \"horizontal\" (ui/graph) win)))"
    ^ "(layout (panel [\"editor\" \"win\"] :collapsed false :window [100 80 300 200]))") in
  check (dump_line floated "windows" = "inspector 100 80 300 200") ("a bound float's window: " ^ dump_line floated "windows");
  E3.close floated;
  print_endline "workspace shell: the studio layout: fixed sizes, strips, start keywords, size conversion, bound workspace, panel instances ok"

(* ---- panels tied to a graph panel, bindings used twice, views as said, second lists and outlines ---- *)

let run_panels () =
  let lowered body = shell_of (build_ok (of_text (with_editor body))) in
  (* :of ties an inspector, a list or a text pane to the graph panel a binding names *)
  let tied = "    (let* [a (ui/graph \"garden\" :focus true)\n           b (ui/graph \"scene\")\n           ia (ui/inspector :of a)\n           ib (ui/inspector :of b)]\n      (ui/workspace (ui/split \"horizontal\" (ui/split \"vertical\" a b) (ui/split \"vertical\" ia ib))))" in
  check ((lowered tied).follows = [ [ 1; 1 ], [ 0; 1 ]; [ 1; 0 ], [ 0; 0 ] ]) "the inspectors are tied to their graph panels";
  expect_error "an :of that is not a graph panel"
    (with_editor "    (let* [c (ui/lisp)] (ui/workspace (ui/split \"vertical\" c (ui/inspector :of c))))") "E_PANEL_OF";
  expect_error "an :of that is not a binding"
    (with_editor "    (ui/workspace (ui/split \"vertical\" (ui/graph) (ui/list :of (ui/graph))))") "E_PANEL_OF";
  expect_error "an :of outside the layout shown"
    (with_editor "    (let* [g (ui/graph) h (ui/graph)] (ui/workspace (ui/split \"vertical\" h (ui/lisp :of g))))") "E_PANEL_OF";
  let rows = match Flow_graph.Projection.find (Flow_graph.Projection.of_graph catalog (of_text (with_editor tied)).checked "editor") [ "editor"; "ia" ] with
    | Some (n : Flow_graph.Projection.node) -> List.map (fun (r : Flow_graph.Projection.row) -> r.label) n.rows
    | None -> fail "no card ia" in
  check (List.mem "of" rows) "the :of keyword is not a row of the inspector's card";
  let retied = match Doc.edit catalog (of_text (with_editor tied))
      (E.Connect { node = [ "editor"; "ia" ]; key = E.Kw "of"; src = "b"; iter = false }) with
    | Ok ws -> shell_of (build_ok ws) | Error d -> fail (Diagnostic.to_string d) in
  check (List.assoc [ 1; 0 ] retied.follows = [ 0; 1 ]) "a wire to the of row did not tie the inspector to the other graph panel";
  let e = ref (editor (with_editor tied)) and count = ref 0 and drawn = ref 0 in
  let step ?(buttons = []) mouse events = incr count; drawn := 0;
    e := fst (E3.update_with !e (frame ~buttons mouse events !count) ~inspector:(fun _ -> incr drawn)) in
  let click point = step point [ Input.Pointer_moved point ];
    step ~buttons:[ Input.LeftButton ] point [ Input.Pointer_pressed (Input.LeftButton, point) ];
    step point [ Input.Pointer_released (Input.LeftButton, point) ]; step point [] in
  step (100., 100.) []; step (100., 100.) [];
  check (!drawn = 2) "with nothing selected both inspectors show the sketch's rows";
  (* a node selected in graph a: its inspector shows the node, b's still the sketch's rows *)
  (* the graph input's header (cards sit on the 24-point lattice now: ask where it is) *)
  let click_node path = let x, y, w, h = Option.get (E3.node_box !e path) in click (float (x + w / 2), float (y + max 1 (h / 2))) in
  click_node [ "garden"; ":seed" ];
  check (dump_line !e "scope selected" = "garden/:seed" && !drawn = 1)
    ("the inspector tied to a did not follow a's selection: " ^ dump_line !e "scope selected");
  (* graph b takes the focus: a's inspector keeps showing a's node *)
  click (200., 600.);
  check (dump_line !e "pane graph" = "scene" && dump_line !e "scope selected" = "-" && !drawn = 1)
    "the inspector tied to a followed the focus to b";
  (* a press in the tied inspector makes its graph panel the one in use *)
  click (880., 300.);
  check (dump_line !e "pane graph" = "garden" && dump_line !e "scope selected" = "garden/:seed"
         && has (dump_line !e "panels") "inspector ia*")
    "a press in a tied inspector did not bring its graph panel into use";
  E3.close !e;
  (* one binding used twice is two instances, keyed by place; the binding's saved entry is what
     each starts from *)
  let twice = with_editor "    (let* [g (ui/graph \"garden\")] (ui/workspace (ui/split \"horizontal\" g g)))" in
  check ((lowered "    (let* [g (ui/graph \"garden\")] (ui/workspace (ui/split \"horizontal\" g g)))").repeated = [ "g" ])
    "a binding used twice is not marked";
  let e = ref (editor (twice ^ "(layout (panel [\"editor\" \"g\"] :collapsed false :window [10 30 430 400])\n        (panel [\"editor\" \"@panel\" \"1\"] :collapsed false :window [450 30 430 400]))")) and count = ref 0 in
  let step ?(buttons = []) mouse events = incr count; e := E3.update !e (frame ~buttons mouse events !count) in
  let click point = step point [ Input.Pointer_moved point ];
    step ~buttons:[ Input.LeftButton ] point [ Input.Pointer_pressed (Input.LeftButton, point) ];
    step point [ Input.Pointer_released (Input.LeftButton, point) ]; step point [] in
  step (700., 500.) []; step (700., 500.) [];
  check (dump_line !e "windows" = "graph 10 30 430 400, graph 450 30 430 400")
    ("the saved entries of a binding used twice: " ^ dump_line !e "windows");
  check (dump_line !e "panels" = "graph 0; graph 1") ("the leaves of one binding are not two instances: " ^ dump_line !e "panels");
  (let x, y, w, h = Option.get (E3.node_box !e [ "garden"; ":seed" ]) in
   click (float (x + w / 2), float (y + max 1 (h / 2))));
  let first = dump_line !e "scope selected" in
  click (860., 410.);
  check (first <> "-" && dump_line !e "scope selected" = "-" && dump_line !e "panels" = "graph 0; graph 1*")
    ("the second leaf shares the first one's selection: " ^ first ^ " / " ^ dump_line !e "scope selected");
  E3.close !e;
  (* a graph panel shows the view it says, with a lisp panel beside it *)
  let e = editor (with_editor "    (ui/workspace (ui/split \"horizontal\" (ui/graph \"garden\" :view \"text\" :focus true) (ui/lisp)))") in
  let e = E3.update (E3.update e (frame (700., 500.) [] 1)) (frame (700., 500.) [] 2) in
  check (dump_line e "projection" = "text" && has (dump_line e "panels") "graph 0*: selection tab")
    ("a graph panel beside a lisp panel does not show its text view: " ^ dump_line e "panels");
  E3.close e;
  (* two lists and two outlines: each its own filter, focus row and search; a click focuses *)
  let e = ref (editor (with_editor "    (let* [oa (ui/outline) ob (ui/outline) la (ui/list) lb (ui/list)]\n      (ui/workspace (ui/split \"horizontal\" (ui/split \"vertical\" oa ob)\n        (ui/split \"horizontal\" (ui/split \"vertical\" la lb) (ui/viewport (ref scene))))))")) and count = ref 0 in
  let step ?(buttons = []) mouse events = incr count; e := E3.update !e (frame ~buttons mouse events !count) in
  let click point = step point [ Input.Pointer_moved point ];
    step ~buttons:[ Input.LeftButton ] point [ Input.Pointer_pressed (Input.LeftButton, point) ];
    step point [ Input.Pointer_released (Input.LeftButton, point) ]; step point [] in
  step (800., 500.) []; step (800., 500.) [];
  let typed point text = click point; step point [ Input.Text_input text ]; step point [ Input.Key_pressed Input.Enter ]; step point [] in
  let panels () = dump_line !e "panels" in
  typed (100., 38.) "zz";
  check (has (panels ()) "outline oa*: search \"zz\"; outline ob: search \"\"") ("the first outline's search: " ^ panels ());
  typed (100., 344.) "q";
  check (has (panels ()) "outline oa: search \"zz\"; outline ob*: search \"q\"")
    ("a search typed in one outline shows in the other: " ^ panels ());
  click (550., 60.);
  step (550., 60.) [ Input.Key_pressed (Input.KeyChar 's') ]; step (550., 60.) [ Input.Text_input "g" ]; step (550., 60.) [];
  (* the focused row is a node id, which depends on how many nodes were allocated before it *)
  let any_id line = let text = panels () in List.exists (fun id -> has text (line (string_of_int id))) (List.init 65536 Fun.id) in
  check (any_id (fun id -> "list la*: focus " ^ id ^ ", 0 folded, filter \"g\"; list lb: focus -, 0 folded, filter -"))
    ("the first list's focus row and filter: " ^ panels ());
  step (550., 60.) [ Input.Key_pressed Input.Enter ]; step (550., 60.) [];
  click (550., 370.);
  check (any_id (fun id -> "list la: focus " ^ id ^ ", 0 folded, filter \"g\"; list lb*: focus " ^ id ^ ", 0 folded, filter -"))
    ("the second list does not keep its own filter: " ^ panels ());
  E3.close !e;
  print_endline "workspace shell: tied panels, repeated bindings, views as said, second lists and outlines ok"

(* a graph gesture on an object of the scene the host composes (no scene graph in the text yet)
   writes that scene first instead of refusing with "No graph or definition scene." *)
let run_host_scene_edit () =
  let e = editor "(workspace studio (graph g :context sop (sop/box)))" in
  let value = match S.parse "false" with Ok [ v ] -> v | _ -> fail "bad literal" in
  (match E3.edit e (Flow_graph.Flow_edit.Set_arg { node = [ "scene"; "camera1" ]; key = Kw "follow_viewport"; sub = []; value }) with
   | Ok e -> check (has (source e) "(graph scene" && not (has (source e) ":follow_viewport true"))
       ("the host camera's edit did not reach the text: " ^ source e)
   | Error m -> fail ("an edit of the host's camera was refused: " ^ m));
  E3.close e

(* panel keys one after another: the focus follows the leaf a split moved and a retype changed, so
   the next key still acts on it *)
let run_panel_chain () =
  let at = 200., 300. in
  let e = ref (editor "(workspace studio (graph g :context sop (sop/box)))") and count = ref 0 in
  let step events = incr count; e := E3.update !e (frame at events !count) in
  for _ = 1 to 6 do step [] done;
  step [ Input.Pointer_moved at ];
  step [ Input.Pointer_pressed (Input.LeftButton, at); Input.Pointer_released (Input.LeftButton, at) ];
  let keys cs = List.iter (fun c -> step [ Input.Key_pressed (if c = ' ' then (Input.KeyChar '/') else Input.KeyChar c) ]; step []) cs;
    step [] in
  keys [ ' '; 'o'; 'v' ];
  keys [ ' '; 'l'; 'u' ];
  check (E3.undo_label !e = Some "Retype panel" && has (source !e) "(ui/outline)")
    ("a retype after splitting a viewport did nothing: " ^ source !e);
  keys [ ' '; 'o'; 'v' ];
  check (E3.undo_label !e = Some "Split panel" && has (dump_line !e "panels") "outline preview_a_a*")
    ("a split after a retype did nothing: " ^ dump_line !e "panels");
  E3.close !e

(* The undo and input fixes of the audit's phase 3, each through the editor. *)
let run_undo_and_input () =
  let plain = "(workspace studio (graph g :context sop (let* [a (sop/box)] a)))" in
  let session ?(text = plain) at =
    let e = ref (editor text) and count = ref 0 in
    let step ?(keys = []) ?(buttons = []) ?(mouse = at) events = incr count;
      e := E3.update !e (Test_editor_input.frame ~keys ~buttons mouse events !count) in
    for _ = 1 to 4 do step [] done;
    e, count, step in
  let press c = Input.Key_pressed (if c = ' ' then (Input.KeyChar '/') else Input.KeyChar c) in
  (* E12: navigation is view state.  It leaves no entry, does not swallow the next undo and does
     not drop what redo restores. *)
  let e, _, step = session (200., 300.) in
  let orbit () = step [ Input.Pointer_moved (200., 300.); Input.Scrolled (0., -2.) ]; for _ = 1 to 20 do step [] done in
  let eye () = Camera.position (Easy_camera.camera (E3.camera !e)) in
  let start = eye () in
  orbit ();
  check (eye () <> start) "the wheel did not move the viewport";
  check (not (E3.can_undo !e)) ("navigating made an undo entry: " ^ Option.value ~default:"-" (E3.undo_label !e));
  e := (match E3.edit !e (E.Set_arg { node = [ "g"; "a" ]; key = E.Kw "uniform_scale"; sub = [];
      value = S.make (S.Num "2.0") }) with Ok e -> e | Error m -> fail m);
  step [];
  let label = E3.undo_label !e in
  orbit ();
  check (E3.undo_label !e = label) "navigating after an edit made an undo entry";
  step ~keys:[ Input.Meta ] [ press 'z' ];
  check (not (has (source !e) "uniform_scale") && E3.can_redo !e) "the first undo after navigating was swallowed";
  orbit ();
  check (E3.can_redo !e) "navigating dropped what redo restores";
  E3.close !e;
  (* E13: a stroke down the visibility column is one undo entry *)
  let lights = {x|(workspace lights (graph scene :context scene
    (scene/merge (scene/light :name "a") (scene/light :name "b") (scene/light :name "c"))))|x} in
  let e, count, step = session ~text:lights (200., 300.) in
  let gx, gy, gw, _ = (E3.panes !e (frame (0., 0.) [] 0)).graph in
  let flag row = float (gx + gw - 38), float (gy + 24 + (24 * row) + 12) in
  count := !count + 30;
  step ~mouse:(flag 0) [ Input.Pointer_moved (flag 0) ];
  step ~buttons:[ Input.LeftButton ] ~mouse:(flag 0) [ Input.Pointer_pressed (Input.LeftButton, flag 0) ];
  step ~buttons:[ Input.LeftButton ] ~mouse:(flag 1) [ Input.Pointer_moved (flag 1) ];
  step ~buttons:[ Input.LeftButton ] ~mouse:(flag 2) [ Input.Pointer_moved (flag 2) ];
  step ~mouse:(flag 2) [ Input.Pointer_released (Input.LeftButton, flag 2) ]; step [];
  check (count_of (source !e) ":visible false" = 3) ("the stroke did not hide three rows: " ^ source !e);
  step ~keys:[ Input.Meta ] [ press 'z' ];
  check (count_of (source !e) ":visible false" = 0) ("one undo did not restore the stroke: " ^ source !e);
  E3.close !e;
  (* E14: a gutter drag cut short by a focus loss leaves no size for a later click to commit *)
  let e, _, step = session ~text:(case "variations") (450., 300.) in
  let gutter = 169.5, 300. and away = 300., 300. in
  step ~mouse:gutter [ Input.Pointer_moved gutter ];
  step ~buttons:[ Input.LeftButton ] ~mouse:gutter [ Input.Pointer_pressed (Input.LeftButton, gutter) ];
  step ~buttons:[ Input.LeftButton ] ~mouse:away [ Input.Pointer_moved away ];
  step ~mouse:away [ Input.Focus_lost ]; step ~mouse:away [];
  let before = source !e in
  step ~mouse:gutter [ Input.Pointer_moved gutter ];
  step ~buttons:[ Input.LeftButton ] ~mouse:gutter [ Input.Pointer_pressed (Input.LeftButton, gutter) ];
  step ~mouse:gutter [ Input.Pointer_released (Input.LeftButton, gutter) ]; step [];
  check (source !e = before && E3.undo_label !e = None)
    ("a click on a gutter committed the size of an interrupted drag: " ^ Option.value ~default:"-" (E3.undo_label !e));
  E3.close !e;
  (* E19: the focus of a closed panel goes to its neighbour, so the next panel key acts *)
  let inspector = 800., 300. in
  let e, _, step = session inspector in
  step [ Input.Pointer_moved inspector ];
  step [ Input.Pointer_pressed (Input.LeftButton, inspector); Input.Pointer_released (Input.LeftButton, inspector) ];
  let keys cs = List.iter (fun c -> step [ press c ]; step []) cs; step [] in
  keys [ ' '; 'o'; 'x' ];
  check (E3.undo_label !e = Some "Close panel") ("the inspector was not closed: " ^ Option.value ~default:"-" (E3.undo_label !e));
  keys [ ' '; 'o'; 'v' ];
  check (E3.undo_label !e = Some "Split panel") "panel keys were dead after the focused panel closed";
  E3.close !e;
  (* E21: fly mode ends when the view stops navigating (a fixed camera, looked through) *)
  let e, _, step = session (200., 300.) in
  step [ Input.Pointer_moved (200., 300.) ];
  step [ Input.Pointer_pressed (Input.LeftButton, (200., 300.)); Input.Pointer_released (Input.LeftButton, (200., 300.)) ];
  List.iter (fun c -> step [ press c ]; step []) [ ' '; 'v' ];
  List.iter (fun c -> step [ press c ]; step []) [ ' '; 'w' ];
  check (E3.flying !e && E3.look_through !e) "the view is not flying through its camera";
  e := (match E3.edit !e (E.Set_arg { node = [ "scene"; "camera1" ]; key = E.Kw "follow_viewport"; sub = [];
      value = S.make (S.Sym "false") }) with Ok e -> e | Error m -> fail m);
  step []; step [];
  check (not (E3.flying !e)) "fly mode outlived the navigation it belongs to";
  E3.close !e;
  (* E20: a name field left open in the graph pane does not hold the keyboard once the panel shows
     its list *)
  let e, count, step = session (450., 300.) in
  let gx, gy, gw, _ = (E3.panes !e (frame (0., 0.) [] 0)).graph in
  let row = float (gx + 60), float (gy + 24 + 12) in
  let click at = step ~mouse:at [ Input.Pointer_moved at ];
    step ~mouse:at [ Input.Pointer_pressed (Input.LeftButton, at); Input.Pointer_released (Input.LeftButton, at) ];
    step ~mouse:at [] in
  click row; step [ press 'i' ]; step [];
  count := !count + 30;
  let x, y, w, h = Option.get (E3.node_box !e [ "g"; "a" ]) in
  click (float (x + (w / 2)), float (y + min 10 (h / 2)));
  step [ Input.Key_pressed Input.F2 ]; step [];
  step [ press 'u' ]; step [];
  check (E3.level !e <> None) "F2 opened no name field: u left the object";
  (* the header's List tab: found by trying, the tabs end 36 points from the header's right edge *)
  let listed = List.exists (fun dx ->
    dump_line !e "projection" = "list"
    || (count := !count + 30; click (float (gx + gw - 36 - dx), float (gy - 12)); dump_line !e "projection" = "list"))
    [ 40; 46; 52; 58; 64; 70 ] in
  check listed ("the header's List tab was not found: " ^ dump_line !e "projection");
  List.iter (fun c -> step [ press c ]; step []) [ ' '; 'l'; 'g' ]; step [];
  check (dump_line !e "projection" = "graph")
    "a name field open in the graph pane kept the keyboard after the panel turned to its list";
  E3.close !e;
  (* E20: a pan in progress gives the pointer back when the panel turns to its list *)
  let e, count, step = session (450., 300.) in
  let gx, gy, gw, gh = (E3.panes !e (frame (0., 0.) [] 0)).graph in
  let row = float (gx + 60), float (gy + 24 + 12) in
  step ~mouse:row [ Input.Pointer_moved row ];
  step ~mouse:row [ Input.Pointer_pressed (Input.LeftButton, row); Input.Pointer_released (Input.LeftButton, row) ];
  step ~mouse:row [ press 'i' ]; step [];
  count := !count + 30;
  let canvas = float (gx + gw - 40), float (gy + gh - 40) and away = float (gx + gw - 80), float (gy + gh - 60) in
  step ~mouse:canvas [ Input.Pointer_moved canvas ];
  step ~buttons:[ Input.MiddleButton ] ~mouse:canvas [ Input.Pointer_pressed (Input.MiddleButton, canvas) ];
  step ~buttons:[ Input.MiddleButton ] ~mouse:away [ Input.Pointer_moved away ];
  check (dump_line !e "graph pan" = "true") "a middle drag on the canvas is not a pan";
  List.iter (fun c -> step ~buttons:[ Input.MiddleButton ] ~mouse:away [ press c ]; step ~buttons:[ Input.MiddleButton ] ~mouse:away [])
    [ ' '; 'l'; 'l' ];
  check (dump_line !e "projection" = "list" && dump_line !e "graph pan" = "false")
    ("a pan in progress kept the pointer after the panel turned to its list: " ^ dump_line !e "projection");
  E3.close !e

(* E15, on the reducer alone: while a payload is carried the document shown is a preview, and
   entering a camera then must not make it the render camera in the history. *)
let run_carry_reaches_no_history () =
  let e = editor {x|(workspace cams
  (graph g :context sop (sop/box))
  (graph scene :context scene
    (let* [cam (scene/camera :name "cam")
           side (scene/camera :name "side" :eye [6 2 0])
           all (scene/merge (scene/geometry (ref g)) cam side)]
      (scene/root all :camera cam))))|x} in
  let e = E3.update (E3.update e (frame (450., 300.) [] 1)) (frame (450., 300.) [] 2) in
  let side = (List.find (fun (i : Edit_graph.node_info) -> i.label = "side") (Edit_graph.inspect (E3.scene_document e))).id in
  let preview = E.Set_arg { node = [ "scene"; "side" ]; key = E.Kw "fov"; sub = []; value = S.make (S.Num "40.0") } in
  let before = source e in
  let stepped = Rays_editor.Reduce.step e ~select:[ side ] ~preview
      [ Rays_editor.Private.Leader.Enter ] (frame (450., 300.) [] 3) in
  check (E3.carrying stepped <> None) "the reducer dropped the carry";
  check (not (E3.can_undo stepped))
    ("entering a camera during a carry reached the history: " ^ Option.value ~default:"-" (E3.undo_label stepped));
  (* without the carry the same keys do make the camera the render camera: the test reaches the path *)
  let entered = Rays_editor.Reduce.step e ~select:[ side ] [ Rays_editor.Private.Leader.Enter ] (frame (450., 300.) [] 3) in
  check (E3.can_undo entered && has (source entered) ":camera side" && source e = before)
    ("Enter on a camera did not make it the render camera: " ^ source entered);
  E3.close e

(* E16: a frame's gestures land together.  Closing the only panel of a document is refused and
   leaves the document as it was. *)
let run_atomic_frame () =
  let at = 200., 300. in
  let e = ref (E3.create ~ui_text_size:11 ~await:true
      ~workspace:(of_text "(workspace solo (graph g :context sop (sop/box)) (graph editor :context editor (ui/workspace (ui/graph))))")
      ~prepare:(fun _ _ -> Ok ()) ~draw:(fun _ () -> Scene.D3.empty) () |> Result.get_ok) and count = ref 0 in
  let step events = incr count; e := E3.update !e (frame at events !count) in
  for _ = 1 to 4 do step [] done;
  step [ Input.Pointer_moved at ];
  step [ Input.Pointer_pressed (Input.LeftButton, at); Input.Pointer_released (Input.LeftButton, at) ];
  let before = source !e and label = E3.undo_label !e in
  List.iter (fun c -> step [ Input.Key_pressed (if c = ' ' then (Input.KeyChar '/') else Input.KeyChar c) ]; step []) [ ' '; 'o'; 'x' ];
  step [];
  check (dump_line !e "edit error" <> "-") ("closing the only panel was not refused: " ^ source !e);
  check (source !e = before && E3.undo_label !e = label)
    ("a refused gesture left the frame's earlier gesture applied: " ^ source !e);
  E3.close !e

(* E2: a graph panel that is not the one in use is inside an object; an undo from the other panel
   removes that object.  The panel goes back to the scene instead of reading a network that is gone. *)
let run_undo_under_pane () =
  let text = {|(workspace two
  (graph g :context sop (sop/box))
  (graph h :context sop (sop/box :size [2 2 2]))
  (graph scene :context scene (let* [a (scene/geometry (ref g))] (scene/merge a)))
  (graph editor :context editor
    (ui/workspace (ui/split "horizontal" (ui/graph :focus true) (ui/graph)))))|} in
  let e = ref (editor text) and count = ref 0 in
  let step ?(keys = []) mouse events = incr count;
    e := E3.update !e (Test_editor_input.frame ~keys mouse events !count) in
  let click point = step point [ Input.Pointer_moved point ];
    step point [ Input.Pointer_pressed (Input.LeftButton, point); Input.Pointer_released (Input.LeftButton, point) ];
    step point [] in
  for _ = 1 to 4 do step (200., 500.) [] done;
  let expr = match S.parse "(scene/geometry (ref h))" with Ok [ v ] -> v | _ -> fail "bad form" in
  e := (match E3.edit !e (E.Add_node { scope = [ "scene" ]; name = "b"; expr }) with
    | Ok e -> e | Error m -> fail m);
  step (200., 500.) [];
  (* the second panel lists the scene: enter the object made from h, its second row *)
  count := !count + 30;
  let row = 700., float (24 + 24 + 12 + 24) in
  click row; step row [ Input.Key_pressed (Input.KeyChar 'i') ]; step row [];
  check (has (dump_line !e "graph panels") "h") ("the second graph panel did not enter the object: " ^ dump_line !e "graph panels");
  (* the first panel takes the focus and undoes the object *)
  count := !count + 30;
  click (200., 500.);
  step ~keys:[ Input.Meta ] (200., 500.) [ Input.Key_pressed (Input.KeyChar 'z') ];
  step (200., 500.) []; step (200., 500.) [];
  check (not (has (source !e) "(ref h)") && not (has (dump_line !e "graph panels") "h"))
    ("undo left the other panel inside a removed object: " ^ dump_line !e "graph panels");
  E3.close !e

let run_spreadsheet () =
  let module Sheet = Rays_editor.Private.Spreadsheet in
  let text = {|(workspace sheet
    (graph g :context sop (sop/grid :counts "Point counts" :rows 2 :columns 3))
    (graph h :context sop (sop/box))
    (graph editor :context editor (let* [a (ui/graph "g" :focus true) b (ui/graph "h")
      sa (ui/spreadsheet :of a) sb (ui/spreadsheet :of b)]
      (ui/workspace (ui/split "vertical" (ui/split "horizontal" a b) (ui/split "horizontal" sa sb))))))|} in
  let doc = of_text text in
  check (Doc.to_text (of_text (Doc.to_text doc)) = Doc.to_text doc) "spreadsheet layout did not round trip";
  let shell = shell_of (build_ok doc) in
  check (shell.follows = [[1;1],[0;1]; [1;0],[0;0]]) "spreadsheet :of did not follow its graph panel";
  let e = ref (editor text) and count = ref 0 in
  Fun.protect ~finally:(fun () -> E3.close !e) (fun () ->
    let step ?(events = []) mouse = incr count; e := E3.update !e (frame mouse events !count) in
    e := Rays_editor.Reduce.select_path !e ["g"; "@result"];
    step (0.,0.); step (0.,0.);
    let owner, geometry = Rays_editor.Reduce.spreadsheet !e [1;0] in
    let geometry = match geometry with Some g -> g | None -> fail "selected cooked node absent from spreadsheet" in
    let counts = List.map (fun owner -> Sheet.count owner geometry) [Rdk.Attribute.Point; Vertex; Primitive; Detail] in
    check (owner = 0 && counts = [6;12;4;1]) ("spreadsheet owner row counts: " ^ String.concat "," (List.map string_of_int counts) ^ "; owner=" ^ string_of_int owner);
    check (snd (Rays_editor.Reduce.spreadsheet !e [1;1]) = None) "spreadsheet followed the other graph selection";
    let layouts = Layout.geometry shell.tree (frame (0.,0.) [] !count) in
    let leaf = List.find (fun (leaf : Layout.leaf) -> leaf.path = [1;0]) layouts.leaves in
    let x,y,w,_ = leaf.body in
    let ui = Pxui.Ui.create () in
    let detail_width = Pxui.Ui.text_width ui "Detail" in Pxui.Ui.destroy ui;
    let point = float (x+w) -. 8. -. detail_width /. 2., float y +. 12. in
    step point; step ~events:[Input.Pointer_pressed (Input.LeftButton, point); Input.Pointer_released (Input.LeftButton, point)] point;
    check (fst (Rays_editor.Reduce.spreadsheet !e [1;0]) = 3) "spreadsheet Detail tab did not route shared input";
    check (fst (Rays_editor.Reduce.spreadsheet !e [1;1]) = 0) "spreadsheet owner state leaked to another pane";
    e := E3.edit !e (E.Set_arg {node = ["g"; "@result"]; key = Kw "columns"; sub = []; value = S.make (Num "4")}) |> Result.get_ok;
    step (0.,0.); step (0.,0.);
    let geometry = snd (Rays_editor.Reduce.spreadsheet !e [1;0]) |> Option.get in
    check (Rdk.Geometry.point_count geometry = 8 && fst (Rays_editor.Reduce.spreadsheet !e [1;0]) = 3) "spreadsheet recook kept stale geometry or reset owner";
    let columns = Sheet.columns Rdk.Attribute.Point geometry in
    check (Array.length columns >= 4 && List.init 4 (fun i -> fst columns.(i)) = ["index";"P.x";"P.y";"P.z"]) "canonical position tuple columns";
    let attr = Rdk.Attribute.create_owned ~name:"label" ~owner:Point (Text (Array.make 8 "sample")) |> Result.get_ok in
    let geometry = Rdk.Geometry.with_attribute attr geometry |> Result.get_ok in
    let group = Rdk.Group.init ~owner:Point ~name:"odd" 8 (fun i -> i mod 2 = 1) in
    let geometry = Rdk.Geometry.with_group group geometry |> Result.get_ok in
    let columns = Sheet.columns Rdk.Attribute.Point geometry |> Array.to_list in
    check ((List.assoc "label" columns) 7 = "sample" && (List.assoc "group:odd" columns) 0 = "0"
      && (List.assoc "group:odd" columns) 7 = "1") "text values/group membership columns";
    print_endline "workspace spreadsheet: owner counts, tied selections, pane-local tabs, recooks and layout roundtrip passed")

(* a value operator has no catalog parameters: the inspector shows its input rows, and a drag on a literal
   one is a Set_arg on that input; a click on the name a row reads selects that binding *)
let run_op_inspector () =
  let text = {|(workspace ops
    (graph g :context value (let* [a 0.6 b (* 3.0 a)] (+ a b)))
    (graph editor :context editor (ui/workspace (ui/split "horizontal" (ui/graph "g") (ui/inspector)))))|} in
  let e = ref (editor text) and count = ref 0 in
  let step ?(buttons = []) ?(mouse = (450., 300.)) events = incr count; e := E3.update !e (frame ~buttons mouse events !count) in
  step []; step [];
  let click point =
    step ~mouse:point [ Input.Pointer_moved point ];
    step ~mouse:point [ Input.Pointer_pressed (Input.LeftButton, point); Input.Pointer_released (Input.LeftButton, point) ] in
  let bx, by, _, _ = Option.get (E3.node_box !e [ "g"; "b" ]) in
  click (float (bx + 60), float (by + 12));
  step [];
  check (dump_line !e "scope selected" = "g/b") ("op node not selected: " ^ dump_line !e "scope selected");
  let before = source !e in
  (* the first row (a, 3.0) is a slider 186 points under the panel body's top, from 603 across in the 450 panel, press at 650 *)
  let ix, iy, _, _ = (E3.panes !e (frame (0., 0.) [] 0)).inspector in
  let at x = float (ix + 200 + x), float (iy + 186) in
  step ~mouse:(at 0) ~buttons:[ Input.LeftButton ] [ Input.Pointer_pressed (Input.LeftButton, at 0) ];
  List.iter (fun x -> step ~mouse:(at x) ~buttons:[ Input.LeftButton ] []) [ 20; 40; 60; 80 ];
  step ~mouse:(at 90) [ Input.Pointer_released (Input.LeftButton, at 90) ];
  step [];
  check (source !e <> before && not (has (source !e) "(* 3.0 a)") && has (source !e) "(* ") ("the inspector showed no editable input row of a * node: " ^ source !e);
  let label = E3.undo_label !e and authored = source !e in
  click (float (ix + 210), float (iy + 210)); step [];
  check (dump_line !e "scope selected" = "g/a" && E3.Private.inspector_subject !e = Some ["g"; "a"])
    ("a wired inspector value did not select its source: " ^ dump_line !e "scope selected");
  check (source !e = authored && E3.undo_label !e = label) "following a wire edited the document";
  E3.close !e

(* a nested operator argument shows as sub-rows, one per literal leaf: a drag on the 3.0 of the product in b is a
   Set_arg with the child path [1]; Unfold makes the nested call its own binding *)
let run_nested_inspector () =
  let text = {|(workspace ops
    (graph g :context value (let* [a 1.0 b (+ 2.0 (* 3.0 a))] (+ a b)))
    (graph editor :context editor (ui/workspace (ui/split "horizontal" (ui/graph "g") (ui/inspector)))))|} in
  let e = ref (editor text) and count = ref 0 in
  let step ?(buttons = []) ?(mouse = (450., 300.)) events = incr count; e := E3.update !e (frame ~buttons mouse events !count) in
  step []; step [];
  let click point =
    step ~mouse:point [ Input.Pointer_moved point ];
    step ~mouse:point [ Input.Pointer_pressed (Input.LeftButton, point); Input.Pointer_released (Input.LeftButton, point) ] in
  let bx, by, _, _ = Option.get (E3.node_box !e [ "g"; "b" ]) in
  click (float (bx + 60), float (by + 12));
  step [];
  check (dump_line !e "scope selected" = "g/b") ("op node not selected: " ^ dump_line !e "scope selected");
  let before = source !e in
  let ix, iy, _, _ = (E3.panes !e (frame (0., 0.) [] 0)).inspector in
  let at dy x = float (ix + 200 + x), float (iy + dy) in
  let drag dy =
    step ~mouse:(at dy 0) ~buttons:[ Input.LeftButton ] [ Input.Pointer_pressed (Input.LeftButton, at dy 0) ];
    List.iter (fun x -> step ~mouse:(at dy x) ~buttons:[ Input.LeftButton ] []) [ 20; 40; 60; 80 ];
    step ~mouse:(at dy 90) [ Input.Pointer_released (Input.LeftButton, at dy 90) ];
    step [] in
  drag 281;
  let after = source !e in
  check (after <> before && has after "(+ 2.0 (* " && has after "a 1.0" && has after " a))" && not (has after "(* 3.0 a)"))
    ("the nested * input was not editable from the inspector: " ^ after);
  let b_line = List.find (fun l -> has l "b (+") (String.split_on_char '\n' after) in
  check (has b_line "(+ 2.0 (* " && not (has b_line "(* 3.0 a)") && has b_line " a))" && E3.undo_label !e = Some "Edit value")
    ("only the nested 3.0 may change: " ^ b_line);
  click (at 154 0);
  step [];
  let unfolded = source !e in
  check (has unfolded "product (* " && has unfolded "b (+ 2.0 product)")
    ("Unfold did not make the nested call its own binding: " ^ unfolded);
  E3.close !e

(* a click on the expression chip of a row (the nested `* 3.0 a` in b) makes that expression the inspector's
   subject: the pane selects the card and the row [b @ [":" ^ label]], the inspector shows the `*` call's
   leaves, and a drag on the 3.0 is a Set_arg with the child path [1] *)
let run_expression_chip () =
  let text = {|(workspace ops
    (graph g :context value (let* [a 1.0 b (+ 2.0 (* 3.0 a))] (+ a b)))
    (graph editor :context editor (ui/workspace (ui/split "horizontal" (ui/graph "g") (ui/inspector)))))|} in
  let e = ref (editor text) and count = ref 0 in
  let step ?(buttons = []) ?(mouse = (450., 300.)) events = incr count; e := E3.update !e (frame ~buttons mouse events !count) in
  step []; step [];
  let click point =
    step ~mouse:point [ Input.Pointer_moved point ];
    step ~mouse:point [ Input.Pointer_pressed (Input.LeftButton, point); Input.Pointer_released (Input.LeftButton, point) ] in
  let bx, by, bw, _ = Option.get (E3.node_box !e [ "g"; "b" ]) in
  let z = float bw /. Flow_graph.Projection.node_width in
  (* the second line of the card (2.0, then the call), at the right where its text is *)
  let chip = float bx +. (Flow_graph.Projection.node_width -. 35.) *. z,
             float by +. (Flow_graph.Projection.body_top +. 1.5 *. Flow_graph.Projection.row_height) *. z in
  click chip;
  step [];
  check (dump_line !e "scope selected" = "g/b, g/b/:b")
    ("the chip did not select its row: " ^ dump_line !e "scope selected");
  let before = source !e in
  let ix, iy, _, _ = (E3.panes !e (frame (0., 0.) [] 0)).inspector in
  let at dy x = float (ix + 200 + x), float (iy + dy) in
  let drag dy =
    step ~mouse:(at dy 0) ~buttons:[ Input.LeftButton ] [ Input.Pointer_pressed (Input.LeftButton, at dy 0) ];
    List.iter (fun x -> step ~mouse:(at dy x) ~buttons:[ Input.LeftButton ] []) [ 20; 40; 60; 80 ];
    step ~mouse:(at dy 90) [ Input.Pointer_released (Input.LeftButton, at dy 90) ];
    step [] in
  drag 190;
  let after = source !e in
  check (after <> before && has after "(+ 2.0 (* " && not (has after "(* 3.0 a)"))
    ("the 3.0 of the * was not editable from the row's inspector: " ^ after);
  click (at 214 0); step [];
  check (dump_line !e "scope selected" = "g/a")
    ("a nested inspector reference followed its parent expression: " ^ dump_line !e "scope selected");
  E3.close !e

let run_context_menus () =
  let exercise body check_menu =
    let presets = Filename.temp_dir "rays-context-presets" "" in
    let e = ref (editor ~presets ("(workspace menus (graph g :context value 0) (graph editor :context editor " ^ body ^ "))"))
    and count = ref 0 in
    let step ?(buttons = []) point events =
      incr count; e := E3.update !e (frame ~buttons point events !count) in
    let hover point = step point [Input.Pointer_moved point] in
    let click button point =
      hover point;
      step ~buttons:[button] point [Input.Pointer_pressed (button, point)];
      step point [Input.Pointer_released (button, point)] in
    step (450., 300.) []; step (450., 300.) [];
    Fun.protect ~finally:(fun () -> E3.close !e) (fun () -> check_menu e step hover click) in
  exercise
    "(let* [outline (ui/outline) main (ui/graph \"g\")] (ui/workspace (ui/split \"horizontal\" outline main :first_size 216)))"
    (fun e step hover click ->
      let origin = 20., 220. in
      click Input.RightButton origin;
      hover (40., 239.); step (40., 239.) [];
      let choice = 189., 335. in
      hover choice; step choice [];
      click Input.LeftButton choice; step choice [];
      check (E3.undo_label !e = Some "New graph" && has (source !e) "(graph value :context value")
        ("Outline's New graph submenu did not create its selected context: " ^ source !e));
  exercise
    "(let* [main (ui/graph \"g\") time (ui/timeline)] (ui/workspace (ui/split \"vertical\" main time :second_size 24)))"
    (fun e step _hover click ->
      let geometry = Layout.geometry ~hidden:[] (shell_of (build_ok (E3.workspace !e))).tree (frame (0., 0.) [] 0) in
      let x, y, _, _ = (Option.get (Layout.find geometry Timeline)).body in
      let origin = float (x + 500), float (y + 12) in
      click Input.RightButton origin; step origin [];
      let choice = fst origin +. 20., min (snd origin) (640. -. 335.) +. 19. in
      click Input.LeftButton choice; step choice [];
      check (E3.undo_label !e = Some "Split panel" && has (source !e) "time_a (ui/timeline)"
        && has (source !e) "time_b (ui/lisp)")
        ("the timeline context menu did not open at the pointer: " ^ source !e);
      check (Rays_editor.Timeline.mode (E3.timeline !e) = Playing) "a timeline right-click sought the playhead");
  exercise "(ui/workspace (ui/graph \"g\"))"
    (fun e step _hover click ->
      (* / t reveals the implicit strip when there is no authored timeline leaf. *)
      step (450., 300.) [Input.Key_pressed (Input.KeyChar '/'); Input.Key_pressed (Input.KeyChar 't')];
      step (450., 300.) [];
      let _, y, _, _ = (E3.panes !e (frame (0., 0.) [] 0)).timeline in
      let origin = 500., float (y + 12) in
      click Input.RightButton origin; step origin [];
      click Input.LeftButton (520., min (snd origin) (640. -. 86.) +. 19.);
      step origin [];
      check (Rays_editor.Timeline.mode (E3.timeline !e) = Paused)
        "the implicit timeline had no context menu at the pointer")

(* Bypass has no compiled node of its own: the inspector must use the checked
   Lisp declaration, including a default which has no authored argument row. *)
let run_lisp_inspector () =
  let text = {|(workspace lisp_card
    (defnode sop/local_card :label "Local card" :category ["Test"]
      [(geo : geometry) (distance : float 0.5 :min -10 :max 10)] geo)
    (graph g :context sop
      (let* [src (sop/grid :columns 2 :rows 2) card ^:bypass (sop/local_card src)] card))
    (graph editor :context editor
      (ui/workspace (ui/split "horizontal" (ui/graph "g") (ui/inspector)))))|} in
  let e = ref (editor text) and count = ref 0 in
  Fun.protect ~finally:(fun () -> E3.close !e) (fun () ->
    e := Rays_editor.Reduce.select_path !e ["g"; "card"];
    let step ?(buttons = []) ?(mouse = (450., 300.)) events =
      incr count; e := E3.update !e (frame ~buttons mouse events !count) in
    step []; step [];
    check (E3.Private.inspector_subject !e = Some ["g"; "card"])
      "Lisp card did not reach the inspector";
    let ix, iy, _, _ = (E3.panes !e (frame (0., 0.) [] 0)).inspector in
    let at x = float (ix + 200 + x), float (iy + 186) in
    step ~mouse:(at 0) ~buttons:[Input.LeftButton]
      [Input.Pointer_pressed (Input.LeftButton, at 0)];
    List.iter (fun x -> step ~mouse:(at x) ~buttons:[Input.LeftButton] []) [20; 40; 60; 80];
    step ~mouse:(at 90) [Input.Pointer_released (Input.LeftButton, at 90)];
    step [];
    check (has (source !e) "(sop/local_card src :distance")
      ("bypassed Lisp node lost its unset inspector parameter: " ^ source !e);
    step ~mouse:(150., 300.) [Input.Pointer_moved (150., 300.)];
    step ~mouse:(150., 300.)
      [Input.Key_pressed (Input.KeyChar '/'); Input.Key_pressed (Input.KeyChar 'a')];
    step ~mouse:(150., 300.) [Input.Text_input "local_card"];
    step ~mouse:(150., 300.) [Input.Key_pressed Input.Enter];
    step ~mouse:(150., 300.) [];
    check (E3.undo_label !e = Some "Add node" && count_of (source !e) "(sop/local_card" = 2)
      ("local Lisp kind was absent from the add menu: " ^ source !e))

let run_array_capacity_menu () =
  let text={|(workspace capacity_menu
    (graph g :context host (let* [limit 0] limit))
    (graph editor :context editor (ui/workspace (ui/graph "g"))))|}in
  let e=ref(editor text)and count=ref 0 in
  Fun.protect ~finally:(fun()->E3.close !e)(fun()->
    let step events=incr count;e:=E3.update !e(frame(150.,300.)events !count)in
    step [];step [];
    step[Input.Pointer_moved(150.,300.)];
    step[Input.Key_pressed(Input.KeyChar '/');Input.Key_pressed(Input.KeyChar 'a')];
    step[Input.Text_input "array/max_length"];step[Input.Key_pressed Input.Enter];step[];
    check(E3.undo_label !e=Some "Add node" && has(source !e)"(array/max_length)")
      ("Array capacity add menu default invalid: "^source !e);
    let ws=E3.workspace !e in
    ignore(build_ok ws);
    let pane=Flow_graph.Projection.of_graph catalog ws.checked "g" in
    let card=List.find(fun(n:Flow_graph.Projection.node)->n.head="array/max_length")pane.nodes in
    check(card.ty=Flow.Ty.Int)"Array capacity menu output was not Int";
    let row=List.find(fun(r:Flow_graph.Projection.row)->r.key=E.Kw "type")card.rows in
    check(row.control=Flow_graph.Projection.Choice && not row.socket)"Array capacity selector lost its literal choice row";
    let value=List.assoc card.path(Result.get_ok(Flow.Eval.static ~record:true ws.checked)).records|>List.hd|>snd in
    check(value=Flow.Eval.Int Sys.max_floatarray_length)"Array capacity menu lost exact native Int")

(* i on a call of a defn opens the definition in the graph panel *)
let run_open_defn () =
  let text = {|(workspace d
  (defn twice :context value [(x : float)] (* x 2.0))
  (graph g :context value (let* [a 1.0 b (twice a)] b))
  (graph editor :context editor (ui/workspace (ui/graph :focus true))))|} in
  let e = ref (editor text) and count = ref 0 in
  let step ?(keys = []) mouse events = incr count;
    e := E3.update !e (Test_editor_input.frame ~keys mouse events !count) in
  for _ = 1 to 4 do step (200., 300.) [] done;
  e := Rays_editor.Reduce.select_path !e [ "g"; "b" ];
  let p = 200., 300. in
  step p []; step p [ Input.Key_pressed (Input.KeyChar 'i') ]; step p [];
  check (has (dump_line !e "graph panels") "def:twice") ("i on a defn call did not open it: " ^ dump_line !e "graph panels");
  (* i on a collapsed function opens it in place (functions start open: the layout folds this one) *)
  let e = ref (editor {|(workspace z
  (graph g :context value (let* [a (map (fn [i] (* i 2.0)) (array/range 3)) b (map (fn [x] (+ x 1.0)) a)] (array/nth b 0)))
  (graph editor :context editor (ui/workspace (ui/graph :focus true))))
(layout (node ["g" "a#0"] :collapsed true))|}) in
  let step mouse events = incr count; e := E3.update !e (Test_editor_input.frame mouse events !count) in
  for _ = 1 to 4 do step (200., 300.) [] done;
  e := Rays_editor.Reduce.select_path !e [ "g"; "a#0" ];
  step (200., 300.) []; step (200., 300.) [ Input.Key_pressed (Input.KeyChar 'i') ]; step (200., 300.) [];
  check (Editor_document.Layout_by_path.Path_map.find_opt [ "g"; "a#0" ] (E3.workspace !e).Doc.layout.collapsed = Some false)
    "i on a collapsed function did not open it"

(* the graph header's Outline button splits an Outline in to the right, and is gone once there is one *)
let run_outline_button () =
  let e = ref (editor {|(workspace o
  (graph g :context value (let* [a 1.0] a))
  (graph editor :context editor (ui/workspace (ui/graph "g" :focus true))))|}) and count = ref 0 in
  let step mouse events = incr count; e := E3.update !e (frame mouse events !count) in
  for _ = 1 to 4 do step (200., 300.) [] done;
  let leaf () = match Contexts.of_workspace ~factories (E3.workspace !e) with
    | Ok d -> Layout.find (Layout.geometry (Option.get d.Document.shell).tree (frame (0., 0.) [] 0)) Layout.Graph
    | Error d -> fail (Diagnostic.to_string d) in
  let header = (Option.get (leaf ())).header in
  let ui = Pxui.Ui.create ~font_size:11 () in
  let from = Pxui_shell.Chrome.tools_start ui ~focused:true ~floating:false ~collapsed:false
    ("Graph\t" ^ dump_line !e "pane graph" ^ " / value") in
  let x, y = match Rays_editor.Private.Bars.tool_rect ~outline:true ui ~header ~from Outline with
    | Some (x, y, w, h) -> x +. w /. 2., y +. h /. 2. | None -> fail "the Outline button is cut off" in
  Pxui.Ui.destroy ui;
  step (x, y) [ Input.Pointer_moved (x, y) ];
  step (x, y) [ Input.Pointer_pressed (Input.LeftButton, (x, y)); Input.Pointer_released (Input.LeftButton, (x, y)) ];
  step (x, y) []; step (x, y) [];
  check (has (source !e) "(ui/outline)")
    "the Outline button did not split an Outline in";
  E3.close !e

(* The default loop runs every section but the slow ones (1.5-12 s each); RAYS_SLOW=1 runs all. *)
let slow = ["run_studio_keywords"; "run_duplicate_and_view"; "run_views"; "run_result_view"; "run_geometry_view"; "run_wire_gestures"; "run_ops"; "run_panels"; "run_unbound_panels"; "run_cameras"; "run_editor"; "run_values"; "run_frame_key"; "run_undo_and_input"]
let run () =
  let all = Sys.getenv_opt "RAYS_SLOW" = Some "1" in
  let only = Sys.getenv_opt "RAYS_SECTION" in
  List.iter (fun (name, section) -> if only <> None && only <> Some name || not all && only = None && List.mem name slow then () else (let t = Unix.gettimeofday () in section (); Printf.printf "section %s %.1f s\n%!" name (Unix.gettimeofday () -. t)))
    [ "run_wire_gestures", run_wire_gestures; "run_ops", run_ops;
      "run_array_capacity_menu", run_array_capacity_menu; "run_lisp_inspector", run_lisp_inspector; "run_context_menus", run_context_menus; "run_expression_chip", run_expression_chip; "run_nested_inspector", run_nested_inspector; "run_op_inspector", run_op_inspector; "run_spreadsheet", run_spreadsheet; "run_panels", run_panels; "run_studio", run_studio; "run_studio_tail", run_studio_tail; "run_studio_keywords", run_studio_keywords; "run_copy_lisp", run_copy_lisp; "run_hide_and_order", run_hide_and_order; "run_root_section", run_root_section; "run_layouts", run_layouts; "run_compose", run_compose; "run_ref_picker", run_ref_picker; "run_result_view", run_result_view; "run_loop_view", run_loop_view; "run_geometry_view", run_geometry_view; "run_panel_states", run_panel_states; "run_camera_zoom", run_camera_zoom; "run_cameras", run_cameras; "run_lowering", run_lowering; "run_panel_keys", run_panel_keys; "run_unbound_panels", run_unbound_panels; "run_values", run_values; "run_duplicate_and_view", run_duplicate_and_view; "run_movers", run_movers; "run_frame_key", run_frame_key; "run_loop_copies", run_loop_copies; "run_loop_expression", run_loop_expression; "run_editor", run_editor; "run_restore", run_restore; "run_views", run_views; "run_instances", run_instances; "run_host_scene_edit", run_host_scene_edit; "run_panel_chain", run_panel_chain;
      "run_undo_under_pane", run_undo_under_pane; "run_atomic_frame", run_atomic_frame; "run_undo_and_input", run_undo_and_input; "run_carry_reaches_no_history", run_carry_reaches_no_history; "run_open_defn", run_open_defn; "run_outline_button", run_outline_button
    ]


(* Native VIEW regression over the reported sketch, including its piece renderer and a following
   camera. Moving the camera rebuilds the lowering while preserving an unchanged object network. *)
let run_view_native () =
  let directory = Filename.temp_dir "rays-view" "" in
  let prefix = Option.value ~default:(Filename.concat directory "view")
      (Sys.getenv_opt "RAYS_VIEW_PNG") in
  let text = In_channel.with_open_bin "../../../sketches/shattered_cube.rays" In_channel.input_all in
  let rendered = ref "" and vertices = ref [] and original = ref [] and edited = ref []
  and returned = ref false in
  ignore (Result.get_ok (Sketch.run_state ~max_frames:37
    ~config:{Sketch.default_config with width = 1200; height = 760;
      title = "Shattered Cube · VIEW regression"; domains = Some 1}
    ~init:(fun _ -> E3.create ~ui_text_size:11 ~await:true ~workspace:(of_text text)
      ~presets:(Filename.concat directory "presets") ~seed:7349L ~grain:2
      ~camera:(Easy_camera.create ~target:Rays_math.Vec3.zero ~distance:6.8 ~azimuth:0.72 ~elevation:0.42 ())
      ~prepare:(fun _ output ->
        match Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Primitive "piece" (Result.get_ok (Sop.Payload.geometry output.Session.payload)) with
        | Some attribute when (match Rdk.Attribute.Private.storage attribute with
            | Rdk.Attribute.Int _ | Text _ -> true | _ -> false) ->
            Rdk_rays.Packed_pieces.of_geometry ~piece_attribute:"piece" (Result.get_ok (Sop.Payload.geometry output.payload))
            |> Result.map (fun pieces -> `Pieces pieces)
        | _ -> Rdk_rays.Rays_mesh.to_mesh (Result.get_ok (Sop.Payload.geometry output.payload))
            |> Result.map (fun mesh -> `Mesh (mesh, output.instances))
            |> Result.map_error Rdk.Error.to_string)
      ~draw:(fun node preview ->
        rendered := Node.operation node;
        let mesh, transforms = match preview with
          | `Pieces pieces ->
              let explosion = Rdk_rays.Packed_pieces.explosion
                  ~operation:(Node.operation node) ~fields:(Node.parameter_fields node) in
              Rdk_rays.Packed_pieces.mesh_for_explosion explosion pieces, None
          | `Mesh (mesh, transforms) -> mesh, transforms in
        vertices := Mesh.vertices mesh;
        let shading = if Node.operation node = "box" then Scene.D3.Flat else Smooth in
        let material = Material.create ~diffuse:(Rays_math.Color.hex_exn "#f2b36d") () in
        let drawing = match transforms with
          | Some transforms -> Scene.D3.instances_array ~cull:Scene.D3.Cull_none ~shading ~material mesh transforms
          | None -> Scene.D3.mesh ~cull:Scene.D3.Cull_none ~shading ~material mesh in
        Scene.D3.create [drawing]) () |> Result.get_ok)
    ~update:(fun e (frame : Frame.t) ->
      let e = if frame.input.frame = 28 then
          E3.edit e (E.Set_arg {node = ["shattered"; "@result"]; key = E.Kw "amount";
            sub = []; value = S.make (S.Num "0.8")}) |> Result.get_ok
        else e in
      let gx, gy, gw, _ = (E3.panes e frame).graph in
      let graph = float (gx + 40), float (gy + 40) and view = 150., 300. in
      let mouse = if frame.input.frame = 24 then
          let x, y, w, h = Option.get (E3.node_box e ["shattered"; "@result"]) in
          (* a wide graph in a narrow pane sits at the zoom floor, the last column on the pane's edge *)
          float (min (x + w / 2) (gx + gw - 6)), float (y + min 12 (max 1 (h / 2)))
        else if List.mem frame.input.frame [3; 4; 18; 19; 20; 21; 22] then view else graph in
      let key k = Input.Key_pressed k in
      let events = match frame.input.frame with
        | 3 | 6 | 18 | 23 -> [Input.Pointer_moved mouse]
        | 4 | 19 -> [Input.Scrolled (0., -1.)]
        | 7 -> [Input.Pointer_pressed (Input.LeftButton, mouse)]
        | 8 -> [Input.Pointer_released (Input.LeftButton, mouse)]
        | 9 -> [key Input.Home] | 10 -> [key (Input.KeyChar 'i')]
        | 12 -> [key Input.ArrowRight] | 14 | 25 -> [key (Input.KeyChar 'v')]
        | 24 -> [Input.Pointer_moved mouse; Input.Pointer_pressed (Input.LeftButton, mouse);
            Input.Pointer_released (Input.LeftButton, mouse)]
        | 32 -> [key (Input.KeyChar 'u')]
        | _ -> [] in
      E3.update e { frame with input = { frame.input with pointer = mouse; events; keys = []; buttons = [] } })
    ~view:E3.scene
    ~after_present:(fun e (frame : Frame.t) ->
      if List.mem frame.input.frame [5; 16; 22; 26; 30; 36] then begin
        let operation = if frame.input.frame = 16 || frame.input.frame = 22 then "box" else "exploded_view" in
        check (Node.operation (E3.displayed_node e) = operation && !rendered = operation)
          (Printf.sprintf "Shattered Cube frame %d rendered %s instead of %s" frame.input.frame !rendered operation);
        check (Canvas.save_screen_png (Printf.sprintf "%s-%d.png" prefix frame.input.frame) = Ok ())
          "VIEW screenshot failed";
        if frame.input.frame = 26 then begin
          original := !vertices;
          check (Node.operation (E3.displayed_node e) = "exploded_view")
            "native v did not preview the result node"
        end;
        if frame.input.frame = 30 then begin
          edited := !vertices;
          check (!edited <> !original) "editing the result left the rendered geometry cached"
        end;
        if frame.input.frame = 36 then begin
          check (dump_line e "level" = "scene" && !vertices = !edited)
            "returning up showed the old rendered result";
          returned := true
        end
      end;
      E3.after_present e frame)
    ~on_stop:E3.close ()));
  check !returned "native VIEW regression stopped before returning to the scene";
  if Sys.getenv_opt "RAYS_VIEW_PNG" = None then
    List.iter (fun n -> Sys.remove (Printf.sprintf "%s-%d.png" prefix n)) [5; 16; 22; 26; 30; 36];
  let state = Filename.concat directory "presets/state" in
  if Sys.file_exists state then begin
    Array.iter (fun name -> Sys.remove (Filename.concat state name)) (Sys.readdir state);
    Unix.rmdir state; Unix.rmdir (Filename.dirname state)
  end;
  Unix.rmdir directory;
  print_endline "Shattered Cube: native VIEW of the result, camera movement, parameter edit and returning up passed"

(* Native: a real window draws Variations' four viewports, each its own scene instance (the
   frame's 3D layers were once cached per frame, so only the first drew). *)
let run_renderers_native ?(authored = false) () =
  let directory = Filename.temp_dir "rays-renderer" "" in
  let workspace = of_text (Printf.sprintf {|
    (workspace renderer
      (graph g :context sop (sop/box))
      (graph scene :context scene
        (scene/merge (scene/geometry (ref g)) (scene/camera :eye [0 0 6])))
      (graph layout :context editor
        (let* [main (ui/viewport (ref scene))
               floating (ui/viewport (ref scene))
               network (ui/graph)
               side (ui/split-at "vertical" 0.5 network %s)]
          (ui/workspace (ui/split-at "horizontal" 0.5 main side)))))
    %s
  |} (if authored then "(ui/floating floating)" else "floating")
    (if authored then "" else "(layout (panel [\"layout\" \"floating\"] :collapsed false :window [450 120 320 280]))")) in
  let samples = ref [] in
  ignore (Result.get_ok (Sketch.run_state ~max_frames:46
    ~config:{Sketch.default_config with width = 900; height = 640; title = "shared renderer"}
    ~init:(fun _ -> E3.create ~ui_text_size:11 ~await:true ~workspace ~presets:(Filename.concat directory "presets")
      ~prepare:(fun _ output -> Rdk_rays.Rays_mesh.to_mesh (Result.get_ok (Sop.Payload.geometry output.Session.payload))
        |> Result.map_error Rdk.Error.to_string)
      ~draw:(fun _ mesh -> Scene.D3.create [Scene.D3.mesh
        ~material:(Material.unlit (Rays_math.Color.rgb 200 20 200)) mesh]) () |> Result.get_ok)
    ~update:(fun e (frame : Frame.t) ->
      let mode = if frame.input.frame < 10 || frame.input.frame >= 40 then Rays_editor.Renderer.Raster
        else if frame.input.frame < 25 then Wireframe else Path_traced in
      let e = if E3.renderer e = mode then e else E3.set_renderer e mode in
      let e = E3.update e frame in
      check (E3.renderer e = mode) "the shared renderer did not switch";
      e)
    ~view:E3.scene
    ~after_present:(fun e (frame : Frame.t) ->
      if List.mem frame.input.frame [5; 15; 35; 45] then begin
        let path = Filename.concat directory (Printf.sprintf "%d.png" frame.input.frame) in
        check (Canvas.save_screen_png path = Ok ()) "renderer screenshot failed";
        samples := path :: !samples
      end;
      E3.after_present e frame)
    ~on_stop:E3.close ()));
  let mesh_pixels path =
    let image = Rays.Image.load_exn path in
    let pixels = Rays.Image.Private.pixels image |> Result.get_ok in
    let w = Result.get_ok (Rays.Image.get_width image)
    and h = Result.get_ok (Rays.Image.get_height image) in
    let scale = float w /. 900. in
    let counts = Array.make 2 0 in
    Array.iteri (fun i (x, y, width, height) ->
      for py = int_of_float (float y *. scale) to min (h - 1) (int_of_float (float (y + height) *. scale) - 1) do
        for px = int_of_float (float x *. scale) to min (w - 1) (int_of_float (float (x + width) *. scale) - 1) do
          let offset = 4 * (py * w + px) in
          let r = Char.code (Bytes.get pixels offset) and g = Char.code (Bytes.get pixels (offset + 1)) in
          let b = Char.code (Bytes.get pixels (offset + 2)) in
          (* the mesh's magenta, a colour the kit's guides, brackets and labels over the view never use *)
          if r > 110 && b > 110 && g < 100 && g + 40 < r then counts.(i) <- counts.(i) + 1
        done
      done) [|(0, 50, 440, 450); (450, 142, 320, 258)|];
    ignore(Rays.Image.destroy image); counts in
  List.iter (fun frame ->
    let counts = mesh_pixels (Filename.concat directory (Printf.sprintf "%d.png" frame)) in
    check (counts.(0) > 30 && counts.(1) > 30)
      (Printf.sprintf "renderer frame %d missed a docked or floating viewport (%d, %d)" frame counts.(0) counts.(1))) [5; 35; 45];
  let wire = mesh_pixels (Filename.concat directory "15.png") in
  check (wire.(0) < 30 && wire.(1) < 30)
    (Printf.sprintf "wireframe still painted filled faces (%d, %d)" wire.(0) wire.(1));
  List.iter Sys.remove !samples;
  let state = Filename.concat directory "presets/state" in
  if Sys.file_exists state then begin Array.iter (fun name -> Sys.remove (Filename.concat state name)) (Sys.readdir state);
    Unix.rmdir state; Unix.rmdir (Filename.dirname state) end;
  Unix.rmdir directory

let run_native () =
  let directory = Filename.temp_dir "rays-variations" "" in
  let path = match Sys.getenv_opt "RAYS_SHELL_PNG" with Some p -> p | None -> Filename.concat directory "variations.png" in
  let frames = 80 in
  let rects = ref [] in
  ignore (Result.get_ok (Sketch.run_state ~max_frames:frames
    ~config:{ Sketch.default_config with width = 900; height = 640; title = "variations" }
    ~init:(fun _ -> editor (case "variations"))
    ~update:(fun e (frame : Frame.t) -> Unix.sleepf 0.004; E3.update e frame)
    ~view:E3.scene
    ~after_present:(fun e (frame : Frame.t) ->
      if frame.input.frame = frames then begin
        let tree = (shell_of (build_ok (E3.workspace e))).tree in
        rects := List.filter_map (fun (l : Layout.leaf) -> match l.panel with
          | Layout.View _ -> let x, y, w, h = l.body in Some (x, y, w, h, (fst frame.input.size))
          | _ -> None) (Layout.geometry ~hidden:[ Layout.Timeline ] tree frame).leaves;
        check (Canvas.save_screen_png path = Ok ()) "the window did not save its screen"
      end;
      E3.after_present e frame)
    ~on_stop:E3.close ()));
  let image = Rays.Image.load_exn path in
  let pixels = Result.get_ok (Rays.Image.Private.pixels image) in
  let image_width = Result.get_ok (Rays.Image.get_width image) in
  let scale = float image_width /. float (match !rects with (_, _, _, _, w) :: _ -> w | [] -> fail "no viewports") in
  check (List.length !rects = 4) "four viewport panels";
  let drawn (x, y, w, h, _) =
    let count = ref 0 and bits = Buffer.create 64 in
    for py = int_of_float (float y *. scale) to int_of_float (float (y + h) *. scale) - 1 do
      for px = int_of_float (float x *. scale) to int_of_float (float (x + w) *. scale) - 1 do
        let o = 4 * (py * image_width + px) in
        let r = Char.code (Bytes.get pixels o) and g = Char.code (Bytes.get pixels (o + 1))
        and b = Char.code (Bytes.get pixels (o + 2)) in
        let hit = r < 110 && g < 110 && b < 110 in  (* the unlit garden against the pale panel *)
        if hit then incr count;
        Buffer.add_char bits (if hit then '1' else '0')
      done
    done;
    !count, Digest.string (Buffer.contents bits) in
  let regions = List.map drawn !rects in
  List.iteri (fun i (count, _) -> check (count > 30) (Printf.sprintf "viewport %d drew nothing (%d dark pixels)" i count)) regions;
  check (List.length (List.sort_uniq compare (List.map snd regions)) = 4) "each viewport shows its own seed's garden";
  if Sys.getenv_opt "RAYS_SHELL_PNG" = None then Sys.remove path;
  Unix.rmdir directory;
  run_renderers_native ();
  run_renderers_native ~authored:true ()

(* ---- two roots in one frame: each viewport renders as its own root says ---- *)

(* Two scene graphs with a root, a camera, a size and a World each, in one window. *)
let roots_text ?(renderer = "Path traced") () = Printf.sprintf {|(workspace roots
  (graph g :context sop (sop/box))
  (graph noon :context world (world/sun (world/sky :name "noon") :name "sun"))
  (graph dusk :context world (world/sun (world/sky :name "dusk") :name "sun"))
  (graph set :context scene (scene/merge (scene/geometry (ref g) :name "body")))
  (graph day :context scene
    (let* [cam (scene/camera :name "noon-cam" :eye [0 1 8])
           all (scene/merge (ref set) cam (scene/world (ref noon) :name "Noon"))
           root (scene/root all :camera cam :renderer "%s" :width 800 :height 450 :max_spp 4096)]
      root))
  (graph night :context scene
    (let* [cam (scene/camera :name "dusk-cam" :eye [5 2 6])
           all (scene/merge (ref set) cam (scene/world (ref dusk) :name "Dusk" :exposure -2))
           root (scene/root all :camera cam :renderer "%s" :width 640 :height 640 :max_spp 16 :bounces 12)]
      root))
  (graph editor :context editor
    (ui/workspace (ui/split-at "horizontal" 0.5 (ui/viewport (ref day)) (ui/viewport (ref night))))))|}
  renderer renderer

let eye_of = function
  | "day" -> Rays_math.Vec3.create 0. 1. 8. | _ -> Rays_math.Vec3.create 5. 2. 6.

(* Without a GPU: looking through the camera, each viewport sees through the camera its own root
   names, at its own gate; the renderer's film steps and turns are pure. *)
let run_roots () =
  let module B = Rays_editor.Private.Render_budget in
  check (B.film ~through:true ~resolution:(1600, 900) ~gate:(1700, 1000) = ((1600, 900), 1)) "a gate over the resolution is step 1";
  check (B.film ~through:true ~resolution:(1600, 900) ~gate:(1600, 900) = ((1600, 900), 1)) "a gate at the resolution is step 1";
  check (B.film ~through:true ~resolution:(1600, 900) ~gate:(1599, 899) = ((800, 450), 2)) "one pixel under is a half";
  check (B.film ~through:true ~resolution:(1600, 900) ~gate:(800, 450) = ((800, 450), 2)) "the half fits its gate exactly";
  check (B.film ~through:true ~resolution:(1600, 900) ~gate:(799, 449) = ((400, 225), 4)) "then a quarter";
  check (B.film ~through:true ~resolution:(1600, 900) ~gate:(300, 169) = ((200, 112), 8)) "then an eighth";
  check (B.film ~through:true ~resolution:(1600, 900) ~gate:(10, 10) = ((200, 112), 8)) "never smaller than an eighth";
  (* a resize inside a step leaves the film alone *)
  let step gate = B.film ~through:true ~resolution:(1600, 900) ~gate in
  check (List.for_all (fun w -> step (w, w * 9 / 16) = ((400, 225), 4)) (List.init 200 (fun i -> 400 + i)))
    "every gate from 400 to 599 points wide gets the same film";
  (* a free view is traced in place: its film is its pane, whatever the root's resolution *)
  check (List.for_all (fun gate -> B.film ~through:false ~resolution:(1920, 1080) ~gate = (gate, 1))
           [ 1396, 1960; 640, 360; 3000, 2000 ])
    "a traced free view does not fill its pane pixel for pixel";
  let order = [ "a"; "b"; "c"; "d" ] in
  check (B.next_turn ~order ~last:None [ "b"; "c"; "d" ] = Some "b") "the first wanting viewport takes the first turn";
  check (B.next_turn ~order ~last:(Some "b") [ "b"; "c"; "d" ] = Some "c") "the next takes the next";
  check (B.next_turn ~order ~last:(Some "d") [ "b"; "c" ] = Some "b") "the rotation wraps";
  check (B.next_turn ~order ~last:(Some "c") [ "b" ] = Some "b") "a lone one repeats";
  check (B.next_turn ~order ~last:(Some "a") [] = None) "no one wants, no turn";
  let e = ref (editor (roots_text ~renderer:"Raster" ())) in
  let keys = List.map fst (shell_of (build_ok (E3.workspace !e))).preview_sources in
  let day, night = match keys with [ d; n ] -> d, n | _ -> fail "two viewports, two roots" in
  let count = ref 0 in
  let step ?(events = []) () = incr count; e := E3.update !e (frame (100., 100.) events !count) in
  step (); step ();
  (* the free view is drawn in its whole pane; only looking through the camera gates it *)
  let film () = E3.film !e (frame (100., 100.) [] !count) in
  let fx, fy, pane_w, pane_h = film () in
  check ((fx, fy) = (0, 0) && pane_w > 0 && pane_h > 0) "a free view does not fill its pane";
  step ~events:[ Input.Key_pressed (Input.KeyChar '/'); Input.Key_pressed (Input.KeyChar 'v') ] (); step (); step ();
  check (E3.look_through !e) "/ v did not look through the camera";
  let gx, gy, gate_w, gate_h = film () in
  check (gx > 0 && gy > 0 && gate_w < pane_w && gate_h < pane_h && abs ((gate_w * 450) - (gate_h * 800)) <= 800)
    "looking through the camera does not show the root's 800 x 450 gate inside the pane";
  let near a b = Rays_math.Vec3.nearly_equal a b ~eps:1e-4 in
  check (near (Camera.position (E3.viewport_camera !e day)) (eye_of "day"))
    "the first viewport does not look through its own root's camera";
  (* look-through is the focused viewport's own: the other keeps its orbit until its panel says so *)
  check (not (near (Camera.position (E3.viewport_camera !e night)) (eye_of "night")))
    "/ v looked through a viewport that does not have the focus";
  let through = editor (replace (roots_text ~renderer:"Raster" ()) "(ui/viewport (ref night))"
    "(ui/viewport (ref night) :look_through true)") in
  let through = E3.update (E3.update through (frame (100., 100.) [] 1)) (frame (100., 100.) [] 2) in
  check (near (Camera.position (E3.viewport_camera through night)) (eye_of "night")
         && not (E3.look_through through))
    "the second viewport does not look through its own root's camera as its :look_through says";
  E3.close through;
  check (E3.slot !e day = None && E3.slot !e night = None) "a raster viewport has no tracer slot";
  E3.close !e;
  print_endline "workspace shell: film steps and turns, each viewport looks through its own root's camera ok"

(* Native: both roots path trace in one window.  Each slot has its root's film and camera, the dusk
   is darker than noon, a cap raised continues the accumulation, a splitter drag inside a film step
   keeps the samples (and one that crosses a step restarts them), and an edit of the second root
   restarts only its slot. *)
let run_roots_native () =
  let width = 1200 in
  let directory = Filename.temp_dir "rays-roots" "" in
  let png = Filename.concat directory "roots.png" in
  (* look-through is each viewport's own: the first takes / v, the second says so in the text *)
  let text = replace (roots_text ()) "(ui/viewport (ref night))" "(ui/viewport (ref night) :look_through true)" in
  let doc = build_ok (of_text text) in
  let shell = shell_of doc in
  let day, night = match List.map fst shell.preview_sources with [ d; n ] -> d, n | _ -> fail "two viewports" in
  (* At 1200 points the new viewport margins still let the half-size films fit;
     the 200-point drag then crosses only the second film's step. *)
  let sample_frame = frame (0., 0.) [] 0 in
  let geometry = Layout.geometry ~hidden:[ Layout.Timeline ] shell.tree
      { sample_frame with input = { sample_frame.input with size = width, 640 } } in
  let pane key = List.find_map (fun (l : Layout.leaf) -> if l.panel = Layout.View key then Some l.body else None)
      geometry.leaves |> Option.get in
  let splitter = List.find (fun (s : Layout.splitter) -> s.node = Some []) geometry.splitters in
  let sx, sy, sw, sh = splitter.bounds in
  let grip = float (sx + sw / 2), float (sy + sh / 2) in
  let drag_at start ~from ~by c = (* the frames of one splitter drag, from [from] points right of the grip *)
    let grip = fst grip +. from, snd grip in
    let target = fst grip +. by, snd grip in
    match c - start with
    | 0 -> Some (grip, [], [ Input.Pointer_moved grip ])
    | 1 -> Some (grip, [ Input.LeftButton ], [ Input.Pointer_pressed (Input.LeftButton, grip) ])
    | 2 -> Some (target, [ Input.LeftButton ], [ Input.Pointer_moved target ])
    | 3 -> Some (target, [], [ Input.Pointer_released (Input.LeftButton, target) ])
    | _ -> None in
  let slots = Hashtbl.create 8 and cameras = ref [] in
  let need e key = match E3.slot e key with Some slot -> slot | None -> fail ("no tracer slot for " ^ key) in
  let edit e arg value =
    match E3.edit e (E.Set_arg { node = [ "night"; "root" ]; key = E.Kw arg; sub = []; value = S.make (S.Num value) }) with
    | Ok e -> e | Error message -> fail message in
  ignore (Result.get_ok (Sketch.run_state ~max_frames:200
    ~config:{ Sketch.default_config with width; height = 640; title = "two roots" }
    ~init:(fun _ -> E3.create ~ui_text_size:11 ~await:true ~workspace:(of_text text)
      ~presets:(Filename.concat directory "presets")
      ~prepare:(fun _ output -> Rdk_rays.Rays_mesh.to_mesh (Result.get_ok (Sop.Payload.geometry output.Session.payload))
        |> Result.map_error Rdk.Error.to_string)
      ~draw:(fun _ mesh -> Scene.D3.create [ Scene.D3.mesh mesh ]) () |> Result.get_ok)
    ~update:(fun e (frame : Frame.t) ->
      let c = frame.input.frame in
      let e = if c = 61 then edit e "max_spp" "4096"      (* the cap rises: the film goes on *)
        else if c = 131 then edit e "bounces" "6"          (* the setting changes: the slot restarts *)
        else e in
      let mouse, buttons, events =
        match drag_at 101 ~from:0. ~by:10. c, drag_at 161 ~from:10. ~by:200. c with
        | Some move, _ | None, Some move -> move
        | None, None -> (100., 100.), [], (if c = 2 then [ Input.Key_pressed (Input.KeyChar '/'); Input.Key_pressed (Input.KeyChar 'v') ] else []) in
      E3.update e { frame with input = { frame.input with pointer = mouse; buttons; keys = []; events };
        mouse_delta = 0., 0. })
    ~view:E3.scene
    ~after_present:(fun e (frame : Frame.t) ->
      if List.mem frame.input.frame [ 60; 100; 130; 160; 195 ] then
        Hashtbl.replace slots frame.input.frame (need e day, need e night);
      if frame.input.frame = 60 then begin
        cameras := [ day, Camera.position (E3.viewport_camera e day); night, Camera.position (E3.viewport_camera e night) ];
        check (Canvas.save_screen_png png = Ok ()) "the window did not save its screen"
      end;
      E3.after_present e frame)
    ~on_stop:E3.close ()));
  let at c = Hashtbl.find slots c in
  let day60, night60 = at 60 and day100, night100 = at 100 and day130, night130 = at 130
  and day160, night160 = at 160 and day195, night195 = at 195 in
  let describe (s : E3.slot) = Printf.sprintf "%dx%d 1/%d %d/%d" (fst s.film) (snd s.film) s.step s.samples s.max_spp in
  (* each slot's film is its own root's resolution in a step; its camera is its root's *)
  let steps = [ 1; 2; 4; 8 ] in
  check (List.mem day60.step steps && day60.film = (800 / day60.step, 450 / day60.step))
    ("the first slot's film is not its root's 800x450 in a step: " ^ describe day60);
  check (List.mem night60.step steps && night60.film = (640 / night60.step, 640 / night60.step))
    ("the second slot's film is not its root's 640x640 in a step: " ^ describe night60);
  check (day60.max_spp = 4096 && night60.max_spp = 16) "each slot carries its own root's cap";
  List.iter (fun (key, eye) ->
    let want = eye_of (if key = day then "day" else "night") in
    check (Rays_math.Vec3.nearly_equal eye want ~eps:1e-4)
      (Printf.sprintf "a pane's camera is not its root's authored camera (%s: %g %g %g, authored %g %g %g)"
         key eye.Rays_math.Vec3.x eye.y eye.z want.Rays_math.Vec3.x want.y want.z)) !cameras;
  check (night60.samples >= 16 && day60.samples >= 16)
    (Printf.sprintf "the slots did not accumulate: %s, %s" (describe day60) (describe night60));
  (* the cap read each frame: raised, the second slot goes on from where it stopped *)
  check (night100.max_spp = 4096 && night100.samples > night60.samples && night100.film = night60.film)
    (Printf.sprintf "raising a cap did not continue the accumulation: %s then %s" (describe night60) (describe night100));
  check (day100.samples > day60.samples) "the first slot stopped accumulating";
  (* a splitter drag inside a step keeps the samples *)
  check (day130.film = day100.film && night130.film = night100.film)
    "a splitter drag inside a step changed a film";
  check (day130.samples > day100.samples && night130.samples > night100.samples)
    (Printf.sprintf "a splitter drag inside a step restarted the accumulation: %s, %s then %s, %s"
       (describe day100) (describe night100) (describe day130) (describe night130));
  (* editing the second root restarts its slot and leaves the first alone *)
  check (night160.samples < night130.samples)
    (Printf.sprintf "editing a root's setting did not restart its slot: %s then %s"
       (describe night130) (describe night160));
  check (day160.samples > day130.samples)
    (Printf.sprintf "editing the second root disturbed the first slot: %s then %s" (describe day130) (describe day160));
  (* a drag across a step restarts: the second viewport's film is smaller, the first one's is not *)
  check (night195.step > night160.step && night195.film <> night160.film && night195.samples <= 40)
    (Printf.sprintf "a drag across a step kept the second film: %s then %s" (describe night160) (describe night195));
  check (day195.film = day160.film && day195.samples > day160.samples)
    (Printf.sprintf "a drag across the second film's step disturbed the first: %s then %s"
       (describe day160) (describe day195));
  (* what the window shows: both panes drawn, the pictures differ, dusk is darker than noon *)
  let image = Rays.Image.load_exn png in
  let pixels = Result.get_ok (Rays.Image.Private.pixels image) in
  let image_width = Result.get_ok (Rays.Image.get_width image) in
  let scale = float image_width /. float width in
  let region (x, y, w, h) (resolution_w, resolution_h) =
    (* the inner part of the gate, which is centred in the pane *)
    let aspect = float resolution_w /. float resolution_h in
    let gw = min (float w) (float h *. aspect) and gh = min (float h) (float w /. aspect) in
    let cx = float x +. float w /. 2. and cy = float y +. float h /. 2. in
    let x0 = int_of_float ((cx -. gw *. 0.3) *. scale) and x1 = int_of_float ((cx +. gw *. 0.3) *. scale)
    and y0 = int_of_float ((cy -. gh *. 0.3) *. scale) and y1 = int_of_float ((cy +. gh *. 0.3) *. scale) in
    let sum = ref 0. and count = ref 0 and differing = ref 0 and bits = Buffer.create 256 in
    for py = y0 to y1 - 1 do
      for px = x0 to x1 - 1 do
        let o = 4 * (py * image_width + px) in
        let r = Char.code (Bytes.get pixels o) and g = Char.code (Bytes.get pixels (o + 1))
        and b = Char.code (Bytes.get pixels (o + 2)) in
        sum := !sum +. (0.2126 *. float r +. 0.7152 *. float g +. 0.0722 *. float b);
        incr count;
        if abs (r - 244) + abs (g - 245) + abs (b - 240) > 30 then incr differing;
        Buffer.add_char bits (Char.chr (r lsr 3 lsl 3))
      done
    done;
    !sum /. float (max 1 !count), !differing * 100 / max 1 !count, Digest.string (Buffer.contents bits) in
  let day_mean, day_cover, day_digest = region (pane day) (800, 450)
  and night_mean, night_cover, night_digest = region (pane night) (640, 640) in
  check (day_cover > 20 && night_cover > 20)
    (Printf.sprintf "a pane is blank (%d%% and %d%% drawn)" day_cover night_cover);
  check (day_digest <> night_digest) "both panes show the same picture";
  check (night_mean < day_mean)
    (Printf.sprintf "dusk is not darker than noon (mean %.1f and %.1f)" night_mean day_mean);
  Sys.remove png;
  let state = Filename.concat directory "presets/state" in
  if Sys.file_exists state then begin
    Array.iter (fun name -> Sys.remove (Filename.concat state name)) (Sys.readdir state);
    Unix.rmdir state; Unix.rmdir (Filename.dirname state) end;
  Unix.rmdir directory;
  print_endline "workspace shell: two roots in one frame, each with its own film, camera and cap ok"

(* Native: four traced viewports over three roots (two of them over one, both looking through its
   render camera: the same render.  Two free views are two pictures, each its own pane).  The two share a slot;
   the focused one renders every frame and the others take turns, so over a run it gets about
   twice the samples of each of the two that alternate. *)
let run_budget_native () =
  let root name eye = Printf.sprintf {|  (graph %s :context scene
    (let* [cam (scene/camera :name "cam" :eye %s)
           all (scene/merge (ref set) cam)
           root (scene/root all :camera cam :renderer "Path traced" :width 800 :height 450 :max_spp 4096)]
      root))
|} name eye in
  let text = {|(workspace budget
  (graph g :context sop (sop/box))
  (graph set :context scene (scene/merge (scene/geometry (ref g) :name "body")))
|} ^ root "a" "[0 1 8]" ^ root "b" "[5 2 6]" ^ root "c" "[-5 2 6]" ^ {|  (graph editor :context editor
    (ui/workspace (ui/split-at "horizontal" 0.5
      (ui/split-at "vertical" 0.5 (ui/viewport (ref a) :look_through true) (ui/viewport (ref b)))
      (ui/split-at "vertical" 0.5 (ui/viewport (ref c)) (ui/viewport (ref a) :look_through true))))))|} in
  let directory = Filename.temp_dir "rays-budget" "" in
  let keys = List.map fst (shell_of (build_ok (of_text text))).preview_sources in
  let a, b, c, a_again = match keys with [ a; b; c; d ] -> a, b, c, d | _ -> fail "four viewports" in
  let final = ref None in
  ignore (Result.get_ok (Sketch.run_state ~max_frames:120
    ~config:{ Sketch.default_config with width = 900; height = 640; title = "budget" }
    ~init:(fun _ -> E3.create ~ui_text_size:11 ~await:true ~workspace:(of_text text)
      ~presets:(Filename.concat directory "presets")
      ~prepare:(fun _ output -> Rdk_rays.Rays_mesh.to_mesh (Result.get_ok (Sop.Payload.geometry output.Session.payload))
        |> Result.map_error Rdk.Error.to_string)
      ~draw:(fun _ mesh -> Scene.D3.create [ Scene.D3.mesh mesh ]) () |> Result.get_ok)
    ~update:(fun e (frame : Frame.t) ->
      E3.update e { frame with input = { frame.input with pointer = (450., 20.); buttons = []; keys = []; events = [] };
        mouse_delta = 0., 0. })
    ~view:E3.scene
    ~after_present:(fun e (frame : Frame.t) ->
      if frame.input.frame = 120 then final := Some (List.map (fun key -> key, E3.slot e key) [ a; b; c; a_again ]);
      E3.after_present e frame)
    ~on_stop:E3.close ()));
  let slot key = match Option.bind !final (List.assoc_opt key) with
    | Some (Some slot) -> slot | Some None | None -> fail ("no slot for " ^ key) in
  let sa = slot a and sb = slot b and sc = slot c and sd = slot a_again in
  let describe (s : E3.slot) = Printf.sprintf "%d spp (%d viewports)" s.samples s.viewports in
  check (sa.viewports = 2 && sd.viewports = 2 && sa.samples = sd.samples)
    ("two viewports over one root did not share a slot: " ^ describe sa ^ ", " ^ describe sd);
  check (sb.viewports = 1 && sc.viewports = 1) "a viewport over another root shared a slot";
  check (sa.samples > 80) ("the focused slot did not render every frame: " ^ describe sa);
  check (float sa.samples >= 1.5 *. float sb.samples && float sa.samples >= 1.5 *. float sc.samples)
    (Printf.sprintf "the focused slot did not get more of the budget: %s, %s, %s" (describe sa) (describe sb) (describe sc));
  check (sb.samples > 20 && sc.samples > 20 && abs (sb.samples - sc.samples) * 4 <= max sb.samples sc.samples)
    (Printf.sprintf "the others did not take turns: %s and %s" (describe sb) (describe sc));
  let state = Filename.concat directory "presets/state" in
  if Sys.file_exists state then begin
    Array.iter (fun name -> Sys.remove (Filename.concat state name)) (Sys.readdir state);
    Unix.rmdir state; Unix.rmdir (Filename.dirname state) end;
  Unix.rmdir directory;
  print_endline "workspace shell: a shared slot, the focused slot every frame, the others in turns ok"
