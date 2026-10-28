(* Bloom studio: the ws_bloom sketch through the editor, the way the study's proposal shows it.
   The Navigator lists the document and opens a graph or a node from a click, the host bars
   run the commands the graph pane has keys for, and Make defn writes a reusable function. *)
open Rays
module E3 = Rays_editor.Editor
module N = Rays_editor.Private.Navigator
module Bars = Rays_editor.Private.Bars
module Layout = Pxui_shell.Layout
module Doc = Editor_document.Workspace_doc
module Document = Editor_document.Document

let fail message = failwith ("test_bloom_studio: " ^ message)
let check condition message = if not condition then fail message
open Test_text

(* 3 and 2 petals instead of 12 and 7: each edit re-cooks the flower, and the checks are about rows and clicks *)
let bloom () =
  let replace a b s = Str.global_replace (Str.regexp_string a) b s in
  In_channel.with_open_bin "../../../sketches/ws_bloom.rays" In_channel.input_all
  |> replace "(petals : int 12)" "(petals : int 3)" |> replace ":petals 7 :seed 2" ":petals 2 :seed 2"
let factories = Sop.Nodes.factories
let catalog = Editor_document.Kinds.catalog ~version:1 factories |> Result.get_ok

(* tall enough to show the whole Navigator without scrolling *)
(* wide enough for the outline to be the wide sheet's (300 points): node rows, data flow *)
let width = 2400 and height = 1400
let frame ?(buttons = []) ?(keys = []) mouse events count =
  let frame = Test_editor_input.frame ~buttons ~keys mouse events count in
  { frame with input = { frame.input with size = width, height } }

let editor () =
  let workspace = match Doc.of_text catalog (bloom ()) with
    | Ok d -> d | Error ds -> fail (String.concat "; " (List.map Diagnostic.to_string ds)) in
  E3.create ~ui_text_size:11 ~await:true ~workspace
    ~prepare:Editor_prepare.surface
    ~draw:(fun _ surface -> Rdk_rays.Surface.scene3 surface.Rays_editor.Private.Cook.surface) ()
  |> function Ok e -> e | Error m -> fail m

let dump_line = Test_workspace_shell.dump_line
let source e = fst (Flow.Lisp.print (E3.workspace e).Doc.source)

let run () =
  let e = ref (editor ()) and count = ref 0 in
  let step ?(buttons = []) ?(keys = []) ?(mouse = (450., 300.)) events =
    incr count; e := E3.update !e (frame ~buttons ~keys mouse events !count) in
  let click ?(button = Input.LeftButton) (x, y) =
    step ~mouse:(x, y) [ Input.Pointer_moved (x, y) ];
    step ~buttons:[ button ] ~mouse:(x, y) [ Input.Pointer_pressed (button, (x, y)) ];
    step ~mouse:(x, y) [ Input.Pointer_released (button, (x, y)) ];
    step [] in
  for _ = 1 to 12 do step [] done;
  (* the sketch opens on flower's network,  *)
  check (dump_line !e "projection" = "graph" && dump_line !e "pane graph" = "flower")
    ("the sketch opens on the network of flower: " ^ dump_line !e "projection" ^ " / " ^ dump_line !e "pane graph");
  let shell () = match (E3.workspace !e |> fun w -> Editor_document.Lowering.of_workspace ~factories w) with
    | Ok d -> Option.get d.Document.shell | Error d -> fail (Diagnostic.to_string d) in
  let geometry () = Layout.geometry ~hidden:[ Layout.Timeline ] (shell ()).tree (frame (0., 0.) [] 0) in
  let outline () = (Option.get (Layout.find (geometry ()) Layout.Outline)).body in
  let graph_leaf () = Option.get (Layout.find (geometry ()) Layout.Graph) in
    (* the Navigator lists the document *)
  let ws = E3.workspace !e in
  let params ?(active = "flower") scope : N.params = { workspace = ws.Doc.checked;
    active = Some active; scope; records = None; probes = (fun _ -> 0); selected = [];
    chips = [];
    (* as many object and layout rows as the panel draws: a row's place depends on them *)
    objects = (List.map (fun _ -> { N.depth = 0; letter = ""; name = ""; detail = ""; visible = None; render = None;
        lead = false; inert = false; chosen = false; home = None }) (E3.scene_document !e));
    root_detail = ""; layouts = Option.map (fun (sw : Document.switch) -> Editor_core.Panels.labels sw.layouts, sw.active) (shell ()).switch; notes = [] } in
  let scope = Some (Flow_graph.Projection.of_graph catalog ws.checked "flower") in
  let lines = Array.to_list (Array.map N.describe (N.rows (N.open_graph "flower" N.initial) (params scope))) in
  let listed text = List.exists (fun l -> has l text) lines in
  check (listed "input petals = 3" && listed "input seed = 7") ("the inputs of flower: " ^ String.concat " | " lines);
  check (listed "> flower · " && listed "1 loop" && listed "R root (scene)") "the composition lists every graph, the open one marked";
  check (listed "for ring") "the open graph's loop is a tree row";
  check (listed "half · defn" && listed "×2" && listed "petal · defn" && listed "×1" && listed "λ twice · 1 use")
    "the reusable functions and macros count their uses";
  check (listed "read by scene") "the data flow names who reads flower";
  (* a click on a row of the Navigator opens that graph in the pane *)
  let row_centre text =
    let active = dump_line !e "pane graph" in
    let scope = Some (Flow_graph.Projection.of_graph catalog ws.checked active) in
    let rects = N.row_rects (N.open_graph "flower" N.initial) (params ~active scope) ~bounds:(outline ()) in
    let row, (x, y, w, h) = match List.find_opt (fun (r, _) -> has (N.describe r) text) (Array.to_list rects) with
      | Some found -> found | None -> fail ("no Navigator row " ^ text) in
    ignore row;
    x +. 20. +. (w /. 8.), y +. h /. 2. in
  (* the open graph's chevron unfolds its node rows *)
  (let active = dump_line !e "pane graph" in
   let scope = Some (Flow_graph.Projection.of_graph catalog ws.checked active) in
   let rects = N.row_rects (N.open_graph "flower" N.initial) (params ~active scope) ~bounds:(outline ()) in
   match List.find_opt (fun (r, _) -> has (N.describe r) "flower · ") (Array.to_list rects) with
   | Some (_, (x, y, _, h)) -> click (x +. 5., y +. h /. 2.)
   | None -> fail "no flower row");
  click (row_centre "R root (scene)");
  click (row_centre "flower · ");
  click (row_centre "half · defn");
  check (dump_line !e "pane graph" = "def:half") ("a function opens in the pane: " ^ dump_line !e "pane graph");
  check (dump_line !e "projection" = "graph") "the function shows as a network";
  click (row_centre "R root (scene)");
  check (dump_line !e "pane graph" = "scene") "a graph row opens that graph";
  click (row_centre "flower · ");
  check (dump_line !e "pane graph" = "flower") "and back to flower";
  (* a node row opens its graph and selects the node there *)
  click (row_centre "wobble");
  check (dump_line !e "scope selected" = "flower/ring/wobble")
    ("a node row selects it in the pane: " ^ dump_line !e "scope selected");
  (* v on a node of the shown graph views it in the viewport without entering the object *)
  click (row_centre "heart");
  (* a Shift-click on empty canvas focuses the pane and keeps the selection *)
  let gx, gy, gw, _ = (graph_leaf ()).body in
  let p = float (gx + gw - 40), float (gy + 40) in  (* the top right: nothing is laid out there *)
  step ~mouse:p [ Input.Pointer_moved p ];
  step ~keys:[ Input.Shift ] ~buttons:[ Input.LeftButton ] ~mouse:p [ Input.Pointer_pressed (Input.LeftButton, p) ];
  step ~keys:[ Input.Shift ] ~mouse:p [ Input.Pointer_released (Input.LeftButton, p) ];
  step ~mouse:p [];
  check (dump_line !e "scope selected" = "flower/heart") ("heart stays selected: " ^ dump_line !e "scope selected");
  let before = E3.workspace !e and label = E3.undo_label !e in
  step ~mouse:p [ Input.Key_pressed (Input.KeyChar 'v') ];
  step ~mouse:p []; step [];
  check (E3.workspace !e == before && E3.undo_label !e = label) "v changed the source or history";
  check (Sop.Node.label (E3.displayed_node !e) = "uv_sphere")
    ("the viewport shows the viewed node at the scene level: " ^ Sop.Node.label (E3.displayed_node !e));
  (* an input's slider writes the graph input's default: one history entry, the text follows *)
  let slider_y, slider_x, slider_w =
    let rects = N.row_rects (N.open_graph "flower" N.initial) (params scope) ~bounds:(outline ()) in
    match List.find_opt (fun (r, _) -> has (N.describe r) "input petals") (Array.to_list rects) with
    | Some (_, (x, y, w, h)) -> y +. (h /. 2.), x +. 76., w -. 86.
    | None -> fail "no petals slider" in
  let from = (slider_x +. (slider_w *. 0.3), slider_y) and to_ = (slider_x +. (slider_w *. 0.9), slider_y) in
  step ~mouse:from [ Input.Pointer_moved from ];
  step ~buttons:[ Input.LeftButton ] ~mouse:from [ Input.Pointer_pressed (Input.LeftButton, from) ];
  step ~buttons:[ Input.LeftButton ] ~mouse:to_ [ Input.Pointer_moved to_ ];
  step ~mouse:to_ [ Input.Pointer_released (Input.LeftButton, to_) ];
  step [];
  check (not (has (source !e) "(petals : int 3)") && has (source !e) "(petals : int ")
    ("the slider changed the input's default: " ^ source !e);
  check (E3.undo_label !e = Some "Input default") ("named in history: " ^ Option.value ~default:"-" (E3.undo_label !e));
  check (dump_line !e "edit error" = "-") "and checks";
  (* a double click on the input's name types in its field; Enter writes the default, an int *)
  let name_at = (slider_x -. 60., slider_y) in
  click name_at; click name_at;
  step [ Input.Text_input "7" ]; step [ Input.Key_pressed Input.Enter ]; step [];
  check (has (source !e) "(petals : int 7)") ("a typed default is written as an int: " ^ source !e);
  check (E3.undo_label !e = Some "Input default") "the typed default is a history entry";
  (* the search field lists matches instead of the tree *)
  let matches = Array.to_list (Array.map N.describe
      (N.rows (N.with_query "wob" N.initial) (params scope))) in
  check (List.exists (fun l -> has l "wobble") matches && not (List.exists (fun l -> has l "scene · scene") matches))
    "a search shows the matching bindings";
  (* the toolbar: Defn makes a reusable function from the selection, as the D key does *)
  let header = (graph_leaf ()).header in
  (* the header names the route taken as a breadcrumb, scene / flower after the clicks above,
     and ends with the graph's context *)
  let ui = Pxui.Ui.create ~font_size:11 () in
  let from = Pxui_shell.Chrome.tools_start ui ~focused:true ~floating:false ~collapsed:false
    ("Graph\t" ^ String.concat " / " (List.map String.trim (String.split_on_char '>' (dump_line !e "route"))) ^ " / sop") in
  let tool t = match Bars.tool_rect ui ~header ~from t with
    | Some (x, y, w, h) -> x +. (w /. 2.), y +. (h /. 2.) | None -> let hx, _, hw, _ = header in fail (Printf.sprintf "the toolbar is cut off (header %d+%d, tools from %.0f)" hx hw from) in
  let before = source !e in
  click (row_centre "heart");
  check (dump_line !e "scope selected" = "flower/heart") "the heart is selected";
  click (tool Bars.Defn);
  check (E3.undo_label !e = Some "Make reusable function") ("the defn tool made a function: "
    ^ Option.value ~default:"-" (E3.undo_label !e) ^ " / " ^ dump_line !e "edit error");
  check (has (source !e) "(defn heart_2 :context sop []") ("a defn with its own name: " ^ source !e);
  check (source !e <> before) "the text changed";
  E3.close !e;
  print_endline "bloom studio: navigator rows and clicks, toolbar defn ok"
