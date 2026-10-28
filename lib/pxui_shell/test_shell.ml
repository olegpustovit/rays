open Rays

let frame : Frame.t = {
  input = { (Input.at_time 0.) with frame = 0; tick = 0; size = 400, 300 };
  pixel_scale = 1., 1.; fps = 0.; mouse_delta = 0., 0.;
}

let bindings = Editor_core.[
  Command.make ~id:"save" ~label:"Save" ~trigger:(Keymap.Leader "s") "save";
  Command.make ~id:"view" ~label:"View" ~trigger:(Keymap.Leader "v") ~scope:"view" "view";]

let instances focus =
  let ui = Pxui.Ui.create ~font_size:11 () in
  ignore (Pxui.Ui.frame ui frame (fun ui ->
    Pxui_shell.Which_key.panel ui bindings ~prefix:"" ~focus ~focus_name:"View"));
  match Scene.Layer.stage_native ~width:400 ~height:300 (Pxui.Ui.scene ui) with
  | Error message -> failwith message
  | Ok staged -> List.fold_left (fun total -> function
      | Scene.Layer.Ui_layer (batch, _) ->
          total + Scene_command.Ui_batch.count batch
      | _ -> total) 0 staged.layers

let status_instances height =
  let ui = Pxui.Ui.create ~font_size:11 () in
  ignore (Pxui.Ui.frame ui frame (fun ui ->
    Pxui_shell.Status_bar.draw ui ~bounds:(0, 272, 400, height)
      ~text:"Cook complete" ~fps:(Some 60) ()));
  match Scene.Layer.stage_native ~width:400 ~height:300 (Pxui.Ui.scene ui) with
  | Error message -> failwith message
  | Ok staged -> List.fold_left (fun total -> function
      | Scene.Layer.Ui_layer (batch, _) ->
          total + Scene_command.Ui_batch.count batch
      | _ -> total) 0 staged.layers

let () =
  let ui = Pxui.Ui.create ~font_size:11 () in
  let pane events = Pxui.Ui.frame ui { frame with input = { frame.input with events } } (fun ui ->
    let root = Pxui_shell.Chrome.pane_root ui frame
      ~bounds:(100, 20, 100, 80) "pane" in
    let child = Pxui.Ui.within ui root (fun () ->
      Pxui.Ui.box ui ~flags:Pxui.Ui.clickable
        ~w:(Pxui.Ui.Px 20.) ~h:(Pxui.Ui.Px 20.)
        ~at:(110., 30.) "child") in
    Pxui.Ui.signal ui root, Pxui.Ui.signal ui child) in
  ignore (pane []);
  let root, child = pane [Input.Pointer_pressed (Input.LeftButton, (115., 35.))] in
  if root.subtree_press <> Some 0 || not child.pressed then
    failwith "pane root missed its child control press";
  let root, _ = pane [Input.Pointer_released (Input.LeftButton, (115., 35.));
    Input.Pointer_pressed (Input.LeftButton, (5., 5.))] in
  if root.subtree_press <> None then
    failwith "pane root accepted a press outside its hit bounds";
  Pxui.Ui.destroy ui;
  let layout = Pxui_shell.Layout.default in
  let wide_frame = { frame with input = { frame.input with size = 1000, 300 } } in
  let panes = Pxui_shell.Layout.(panes (geometry layout wide_frame)) in
  let _, _, view_width, _ = panes.view
  and _, _, graph_width, _ = panes.graph
  and _, _, inspector_width, _ = panes.inspector in
  if (view_width, graph_width, inspector_width) <> (449, 349, 200) then
    failwith "shell layout defaults are not 45/35/20";
  let divider_x = view_width in
  let hit_ui = Pxui.Ui.create ~font_size:11 () in
  let chrome events = ignore (Pxui.Ui.frame hit_ui { wide_frame with input = { wide_frame.input with events } }
    (fun ui -> ignore (Pxui_shell.Chrome.update layout ui wide_frame);
      Pxui_shell.Chrome.splitters layout ui wide_frame)) in
  chrome [];
  chrome [Input.Pointer_moved (float (divider_x - 2), 100.)];
  if Pxui.Ui.cursor hit_ui <> Some `Horizontal_resize then
    failwith "thin splitter lost its wider resize hit area";
  Pxui.Ui.destroy hit_ui;
  let inspector_hidden = [Pxui_shell.Layout.Timeline; Inspector] in
  let _, _, inspector_width, _ =
    (Pxui_shell.Layout.(panes (geometry ~hidden:inspector_hidden layout wide_frame))).inspector in
  let hidden_both = Pxui_shell.Layout.[Timeline; Inspector; Graph] in
  let _, _, view_width, _ = (Pxui_shell.Layout.(panes (geometry ~hidden:hidden_both layout wide_frame))).view in
  if inspector_width <> 0 || view_width <> 1000 then
    failwith "collapsed graph and inspector did not vanish";
  (* the default three columns match the retired fixed layout at these sizes and states *)
  let golden (w, h) hidden (view, graph_w, inspector_w, status, timeline) =
    let f = { frame with input = { frame.input with size = w, h } } in
    let p = Pxui_shell.Layout.(panes (geometry ~hidden layout f)) in
    let width (_, _, w, _) = w in
    if p.view <> view || width p.graph <> graph_w || width p.inspector <> inspector_w
       || p.status <> status || p.timeline <> timeline then
      failwith (Printf.sprintf "default layout changed at %dx%d" w h) in
  let module L = Pxui_shell.Layout in
  (* a header is a 4-point margin and a 24-point row: the bodies start at 28; the status strip is a
     hairline and a 24-point bar, the strip under a tree without a timeline the same *)
  golden (800, 600) [L.Timeline] ((0, 28, 359, 547), 279, 160, (0, 575, 800, 25), (0, 575, 800, 0));
  golden (1280, 800) [L.Timeline; Graph] ((0, 28, 885, 747), 0, 394, (0, 775, 1280, 25), (0, 775, 1280, 0));
  golden (1920, 1080) [L.View "main"] ((0, 28, 24, 1002), 1205, 689, (0, 1055, 1920, 25), (0, 1030, 1920, 25));
  golden (1000, 300) [L.Inspector] ((0, 28, 561, 222), 438, 0, (0, 275, 1000, 25), (0, 250, 1000, 25));
  golden (500, 400) [L.Timeline] ((0, 28, 224, 347), 174, 100, (0, 375, 500, 25), (0, 375, 500, 0));
  (* every tree covers the area above the timeline exactly once, gutters included *)
  let views n = L.Tile (List.init n (fun i -> L.Leaf (L.View (string_of_int i)))) in
  let trees = [
    "default", L.default;
    "sheet", L.Split { axis = `V; size = `Ratio 0.13; a = Leaf Outline; b =
      Split { axis = `H; size = `Ratio 0.5; a = Split { axis = `V; size = `Ratio 0.58; a = Leaf Graph; b = Leaf Lisp };
              b = views 4 } };
    "tile of five", views 5;
    "vertical stack", Split { axis = `V; size = `Ratio 0.3; a = Leaf Graph; b =
      Split { axis = `V; size = `Ratio 0.5; a = Leaf Inspector; b = Leaf List } };
    "nested", Split { axis = `H; size = `Ratio 0.3; a = Split { axis = `V; size = `Ratio 0.4; a = Leaf Graph;
      b = Leaf List }; b = Split { axis = `H; size = `Ratio 0.5; a = Leaf Inspector; b = Leaf (View "z") } } ] in
  List.iter (fun (name, tree) ->
    List.iter (fun (w, h) ->
      let f = { frame with input = { frame.input with size = w, h } } in
      let g = L.geometry tree f in
      let seen = Array.make_matrix w h 0 in
      let cover (x, y, cw, ch) = for i = x to x + cw - 1 do for j = y to y + ch - 1 do
        if i >= 0 && j >= 0 && i < w && j < h then seen.(i).(j) <- seen.(i).(j) + 1
        else failwith (name ^ ": a rectangle leaves the frame") done done in
      List.iter (fun (l : L.leaf) ->
        (* the frame is the margin, the header row and the body, once *)
        let fx, fy, fw, fh = l.frame and hx, hy, hw, hh = l.header and bx, by, bw, bh = l.body in
        if (hx, hw) <> (fx, fw) || (bx, bw) <> (fx, fw) || hy < fy || by <> hy + hh || by + bh > fy + fh
           || (not l.floating && (hy - fy) + hh + bh <> fh && bh > 0) then
          failwith (name ^ ": a leaf's header and body do not tile its frame");
        cover l.frame) g.leaves;
      cover g.status_at;
      List.iter (fun (s : L.splitter) -> cover s.bounds) g.splitters;
      Array.iter (Array.iter (fun n -> if n <> 1 then
        failwith (Printf.sprintf "%s at %dx%d: a point is covered %d times" name w h n))) seen)
      [ 1000, 600; 640, 480; 333, 217 ]) trees;
  (* floats overlay the rectangle of their parent and come last *)
  let floated = L.Split { axis = `H; size = `Ratio 0.5; a = Leaf Graph; b = Float (Leaf Inspector) } in
  let g = L.geometry floated wide_frame in
  if List.map (fun (l : L.leaf) -> l.floating) g.leaves <> [false; true] then
    failwith "authored floats lost their painting precedence";
  (match List.map (fun (l : L.leaf) -> l.panel) g.leaves, List.rev g.leaves with
   | [ Graph; Inspector ], { body = bx, by, bw, bh; _ } :: _ ->
       let gx, _, gw, _ = (Option.get (L.find g Graph)).body in
       if gx <> 0 || gw <> 1000 || bx <= 0 || bx + bw >= 1000 || by <= 28 || bh <= 0 then
         failwith "a float is not an inset overlay over a full-size sibling"
   | _ -> failwith "float leaves are not last");
  (* collapsed, an authored float that was never moved is a short tab, not its whole window *)
  let folded = L.geometry ~state:(fun path -> { Editor_core.Panels.default_state with
    collapsed = path = [ 1; 0 ] }) floated wide_frame in
  (match List.rev folded.leaves with
   | { frame = _, _, w, h; body = _, _, _, 0; _ } :: _ when w <= 150 && h <= 40 -> ()
   | _ -> failwith "a collapsed float kept its window size");
  (* fixed sides keep their points at every window size (flow.md 11.11): the outline column,
     the right column, the graph and the lisp panel; the timeline leaf is its 24-point strip
     with no header; the status strip spans the window *)
  let fixed axis size a b = L.Split { axis; size; a; b } in
  let target = fixed `H (`First 216) (Leaf Outline)
    (fixed `H (`Second 320)
      (fixed `V (`Second 336) (fixed `V (`Ratio 0.5) (Leaf (View "main")) (Leaf Timeline)) (Leaf Graph))
      (fixed `V (`Second 300) (Leaf Inspector) (Leaf Lisp))) in
  List.iter (fun (w, h) ->
    let g = L.geometry ~hidden:[] target { frame with input = { frame.input with size = w, h } } in
    let whole panel = match List.find_opt (fun (l : L.leaf) -> l.panel = panel) g.leaves with
      | Some { frame; _ } -> frame
      | None -> failwith "the target lost a panel" in
    let expect what got want = if got <> want then failwith (Printf.sprintf "target at %dx%d: %s" w h what) in
    let tree = h - 25 in
    expect "outline" (whole Outline) (0, 0, 216, tree);
    expect "viewport" (whole (View "main")) (217, 0, w - 216 - 320 - 2, tree - 336 - 24 - 2);
    expect "timeline" (whole Timeline) (217, tree - 336 - 24 - 1, w - 216 - 320 - 2, 24);
    expect "graph" (whole Graph) (217, tree - 336, w - 216 - 320 - 2, 336);
    expect "inspector" (whole Inspector) (w - 320, 0, 320, tree - 300 - 1);
    expect "lisp" (whole Lisp) (w - 320, tree - 300, 320, 300);
    expect "status" g.status_at (0, tree, w, 25);
    expect "no second timeline strip" g.timeline_at (0, tree, w, 0);
    (match List.find_opt (fun (l : L.leaf) -> l.panel = Timeline) g.leaves with
     | Some { header = _, _, _, 0; _ } -> ()
     | _ -> failwith "a docked timeline has a header"))
    [ 1440, 900; 1920, 1080; 1100, 700 ];
  (* too small for both: the other side keeps its minimum, then the fixed side shrinks to a point *)
  let narrow = fixed `H (`First 216) (Leaf Outline) (Leaf Graph) in
  let widths w = List.map (fun (l : L.leaf) -> let _, _, lw, _ = l.header in lw)
    (L.geometry narrow { frame with input = { frame.input with size = w, 300 } }).leaves in
  if widths 300 <> [ 119; 180 ] || widths 100 <> [ 1; 98 ] then failwith "a fixed side did not yield";
  (* a collapsed panel in a stack is its 24-point header, with no margin over it; the next panel's
     header sits under its own 4-point margin *)
  let stack = fixed `V (`Ratio 0.5) (Leaf Inspector) (Leaf Lisp) in
  (match (L.geometry stack wide_frame
      ~state:(fun path -> { Editor_core.Panels.default_state with collapsed = path = [ 0 ] })).leaves with
   | [ { header = _, 0, _, 24; body = _, _, _, 0; _ }; { header = _, 29, _, _; _ } ] -> ()
   | _ -> failwith "a collapsed strip in a stack is not its header");
  (* a header is a 24-point row under a 4-point margin; a docked timeline under 90 points is its
     strip alone, a taller one has a header like any panel (timeline.html) *)
  (match (L.geometry (fixed `V (`Second 97) (Leaf (View "main")) (Leaf Timeline)) ~hidden:[] wide_frame).leaves with
   | [ _; { panel = Timeline; frame = fx, fy, fw, 97; header = hx, hy, hw, 24; body = bx, by, bw, bh; _ } ] ->
       if (hx, hy, hw) <> (fx, fy + 4, fw) || (bx, by, bw) <> (fx, fy + 28, fw) || bh <> 97 - 28 then
         failwith "a tall timeline's header and body are not under the 4-point margin"
   | _ -> failwith "a tall timeline has no header row");
  (match (L.geometry (fixed `V (`Second 24) (Leaf (View "main")) (Leaf Timeline)) ~hidden:[] wide_frame).leaves with
   | [ _; { panel = Timeline; header = _, _, _, 0; body = _, _, _, 24; _ } ] -> ()
   | _ -> failwith "a 24-point timeline is not its strip");
  (* where a header's tools begin: after the title, 8, the 1-point rule between 4-point margins, 8;
     the focus square adds 12; a window sets its path in the label size *)
  let tools_ui = Pxui.Ui.create ~font_size:11 () in
  let from ?(focused = false) ?(floating = false) text =
    Pxui_shell.Chrome.tools_start tools_ui ~focused ~floating ~collapsed:false text in
  if abs_float (from ~focused:true "Graph\tgarden / sop" -. from "Graph\tgarden / sop" -. 12.) > 1e-6
     || from "Graph\tgarden / sop" <= from "Graph" +. 25. then
    failwith "a header's tools do not start after its focus square, title and breadcrumb";
  if abs_float (from "Graph" -. (12. +. Pxui_shell.Kit.cap_width tools_ui "Graph" +. 25.)) > 1e-6 then
    failwith "a title alone is not 12 in, its label and 25 to the tools";
  Pxui.Ui.destroy tools_ui;
  (* a nested split takes its share of what its parent leaves it *)
  (match (L.geometry (L.Split { axis = `V; size = `Ratio 0.25; a = Leaf Graph; b = Leaf Lisp })
      wide_frame).leaves with
   | [ a; b ] -> let _, _, _, ah = a.body and _, _, _, bh = b.body in
       if ah + 22 >= bh + 22 then failwith "a vertical split ignored its ratio"
   | _ -> failwith "vertical split lost a panel");
  (* dragging a splitter reports a ratio, and the header menu reports its intents *)
  let drag_ui = Pxui.Ui.create ~font_size:11 () in
  let chrome tree ?(mouse = 0., 0.) events =
    let f = { wide_frame with input = { wide_frame.input with events; pointer = mouse; size = 1000, 600 } } in
    Pxui.Ui.frame drag_ui f (fun ui -> let a = Pxui_shell.Chrome.update tree ui f in
      a @ Pxui_shell.Chrome.splitters tree ui f) in
  ignore (chrome L.default []);
  let x = 449. in
  ignore (chrome L.default ~mouse:(x, 100.) [Input.Pointer_moved (x, 100.)]);
  ignore (chrome L.default ~mouse:(x, 100.) [Input.Pointer_pressed (Input.LeftButton, (x, 100.))]);
  let moved = chrome L.default ~mouse:(300., 100.) [Input.Pointer_moved (300., 100.)] in
  (match moved with
   | [ Pxui_shell.Chrome.Resize { node = []; size = `Ratio ratio } ] ->
       if Float.abs (ratio -. 0.3) > 0.01 then failwith "splitter ratio does not follow the pointer"
   | _ -> failwith "a drag did not report one resize of the outer split");
  let released = chrome L.default ~mouse:(300., 100.) [Input.Pointer_released (Input.LeftButton, (300., 100.))] in
  if not (List.mem Pxui_shell.Chrome.Settled released) then failwith "a drag did not settle";
  (* the gutter of a fixed split reports whole points of its fixed side *)
  let sized = fixed `H (`Second 320) (Leaf Graph) (Leaf Inspector) in
  ignore (chrome sized ~mouse:(679., 100.) [Input.Pointer_moved (679., 100.)]);
  ignore (chrome sized ~mouse:(679., 100.) [Input.Pointer_pressed (Input.LeftButton, (679., 100.))]);
  (match chrome sized ~mouse:(599.4, 100.) [Input.Pointer_moved (599.4, 100.)] with
   | [ Pxui_shell.Chrome.Resize { node = []; size = `Second 400 } ] -> ()
   | _ -> failwith "a fixed split's gutter did not report points");
  ignore (chrome sized ~mouse:(599.4, 100.) [Input.Pointer_released (Input.LeftButton, (599.4, 100.))]);
  (* a right-click on a gutter sizes its split another way, keeping the sizes it shows *)
  let menu_pick row =
    ignore (chrome sized ~mouse:(679., 100.) [Input.Pointer_moved (679., 100.)]);
    ignore (chrome sized ~mouse:(679., 100.) [Input.Pointer_pressed (Input.RightButton, (679., 100.))]);
    ignore (chrome sized ~mouse:(679., 100.) [Input.Pointer_released (Input.RightButton, (679., 100.))]);
    let at = 700., 112. +. 24. *. float row in
    ignore (chrome sized ~mouse:at [Input.Pointer_moved at]);
    chrome sized ~mouse:at [Input.Pointer_pressed (Input.LeftButton, at); Input.Pointer_released (Input.LeftButton, at)] in
  (match menu_pick 1 with
   | [ Pxui_shell.Chrome.Resize { node = []; size = `First 679 }; Settled ] -> ()
   | intents -> failwith (Printf.sprintf "the gutter menu did not fix the first side (%d intents)" (List.length intents)));
  (match menu_pick 0 with
   | [ Pxui_shell.Chrome.Resize { node = []; size = `Ratio r }; Settled ] when Float.abs (r -. (679.5 /. 999.)) < 1e-6 -> ()
   | _ -> failwith "the gutter menu did not size the split by its ratio");
  (* the header menu no longer holds the size rows: the gutter's right-click above does *)
  let gutter = List.hd (L.geometry sized { wide_frame with input = { wide_frame.input with size = 1000, 600 } }).splitters in
  if L.sides gutter <> (679, 320) || L.resized gutter `Second <> `Second 320 then
    failwith "a gutter does not report the sides of its split";
  ignore (chrome L.default ~mouse:(600., 10.) [Input.Pointer_moved (600., 10.)]);
  ignore (chrome L.default ~mouse:(600., 10.) [Input.Pointer_pressed (Input.RightButton, (600., 10.))]);
  ignore (chrome L.default ~mouse:(600., 10.) [Input.Pointer_released (Input.RightButton, (600., 10.))]);
  ignore (chrome L.default ~mouse:(620., 28.) [Input.Pointer_moved (620., 28.)]);
  let pick = [Input.Pointer_pressed (Input.LeftButton, (620., 28.));
    Input.Pointer_released (Input.LeftButton, (620., 28.))] in
  (match chrome L.default ~mouse:(620., 28.) pick with
   | [ Pxui_shell.Chrome.Split_panel ([ 1; 0 ], `H) ] -> ()
   | intents -> failwith (Printf.sprintf "the header menu did not split the graph panel (%d intents)" (List.length intents)));
  let collapse = [Input.Pointer_pressed (Input.LeftButton, (985., 10.));
    Input.Pointer_released (Input.LeftButton, (985., 10.))] in
  ignore (chrome L.default ~mouse:(985., 10.) [Input.Pointer_moved (985., 10.)]);
  if not (List.mem (Pxui_shell.Chrome.Toggle [1; 1]) (chrome L.default ~mouse:(985., 10.) collapse)) then
    failwith "the collapse button did not toggle the inspector";
  Pxui.Ui.destroy drag_ui;
  if instances "view" <= instances "other" then
    failwith "focused leader bindings were not drawn";
  if status_instances 24 <= 0 || status_instances 0 <> 0 then
    failwith "status strip visibility or drawing failed";
  let ui = Pxui.Ui.create ~font_size:11 () in
  let intents = Pxui.Ui.frame ui frame (fun ui ->
    Pxui_shell.Timeline_bar.draw ui ~bounds:(0, 270, 400, 30)
      ~playing:false ~frame:12L ~time:0.2 ~max_frame:240 ()) in
  if intents <> [] then failwith "idle timeline emitted a playback request";
  (* the frame field: found by pressing along the strip until a control takes the keyboard *)
  let bar ?(mouse = 0., 0.) events = Pxui.Ui.frame ui
    { frame with input = { frame.input with events; pointer = mouse } } (fun ui ->
    Pxui_shell.Timeline_bar.draw ui ~bounds:(0, 270, 400, 30)
      ~playing:false ~frame:12L ~time:0.2 ~max_frame:240 ()) in
  let press p = ignore (bar ~mouse:p [ Input.Pointer_moved p ]);
    ignore (bar ~mouse:p [ Input.Pointer_pressed (Input.LeftButton, p); Input.Pointer_released (Input.LeftButton, p) ]);
    ignore (bar ~mouse:p []) in
  let rec find x = if x > 400. then failwith "the timeline has no frame field" else begin
    press (x, 285.);
    if Pxui.Ui.text_input_focused ui then x else find (x +. 6.) end in
  let x = find 3. in
  let enter text key =
    if not (Pxui.Ui.text_input_focused ui) then press (x, 285.);
    ignore (bar ~mouse:(x, 285.) [ Input.Text_input text ]);
    let sought = bar ~mouse:(x, 285.) [ Input.Key_pressed key ] @ bar ~mouse:(x, 285.) [] in
    List.filter_map (function Pxui_shell.Timeline_bar.Seek_playback n -> Some n | _ -> None) sought in
  if enter "7" Input.Enter <> [ 7L ] then failwith "a typed frame did not seek";
  if enter "9" Input.Escape <> [] then failwith "Escape did not cancel the typed frame";
  if Pxui.Ui.text_input_focused ui then failwith "Escape left the frame field open";
  if enter "99999" Input.Enter <> [ 240L ] then failwith "a frame past the end did not clamp to the last frame";
  if enter "-5" Input.Enter <> [ 0L ] then failwith "a negative frame did not clamp to 0";
  if enter "x" Input.Enter <> [] then failwith "text that is not a frame was committed";
  ignore (bar [ Input.Key_pressed Input.Escape ]);
  let query = Pxui.Ui.frame ui frame (fun ui ->
    Pxui_shell.Prompt.name ui ~key:"name" ~title:"Save" ~description:"Name it" ~label:"Name"
      ~query:"draft") in
  if query <> Some ("draft", `None) then
    failwith "name prompt lost its initial query";
  (* the make-macro dialog keeps the host's state until something is pressed *)
  let macro = { Pxui_shell.Prompt.name = "shifted"; holes = [| true, "p1"; false, "p2"; true, "p3" |] } in
  let answer = Pxui.Ui.frame ui frame (fun ui ->
    Pxui_shell.Prompt.macro ui ~key:"macro" ~title:"Make a macro" ~literals:[| "0.5"; "12"; "\"a\"" |]
      ~free:[ "seed" ] macro) in
  if answer <> Some (macro, `None) then failwith "macro prompt changed its state or submitted on its own";
  (* a window saved past the right edge is drawn inside the frame, and a drag moves it from
     where it is drawn: the pointer is followed at once *)
  let window_ui = Pxui.Ui.create ~font_size:11 () in
  let past = { Editor_core.Panels.default_state with window = Some (900, 50, 300, 200) } in
  let windowed ?(mouse = 0., 0.) events =
    let f = { wide_frame with input = { wide_frame.input with events; pointer = mouse; size = 1000, 600 } } in
    Pxui.Ui.frame window_ui f (fun ui ->
      Pxui_shell.Chrome.update ~state:(fun path -> if path = [] then past else Editor_core.Panels.default_state)
        (L.Leaf L.Graph) ui f) in
  ignore (windowed []);
  ignore (windowed ~mouse:(750., 67.) [Input.Pointer_moved (750., 67.)]);
  ignore (windowed ~mouse:(750., 67.) [Input.Pointer_pressed (Input.LeftButton, (750., 67.))]);
  (match List.filter_map (function Pxui_shell.Chrome.Window_drag (_, (x, _, w, _), _) -> Some (x, w) | _ -> None)
      (windowed ~mouse:(650., 67.) [Input.Pointer_moved (650., 67.)]) with
   | [ (600, 300) ] -> ()
   | [ (x, _) ] -> failwith (Printf.sprintf "a window saved past the edge did not follow the pointer: x %d" x)
   | _ -> failwith "a header drag did not move its window");
  Pxui.Ui.destroy window_ui;
  (* the parts of a docked header's breadcrumb before its last are buttons *)
  let crumb_ui = Pxui.Ui.create ~font_size:11 () in
  let crumbed ?(mouse = 0., 0.) events =
    let f = { wide_frame with input = { wide_frame.input with events; pointer = mouse; size = 1000, 600 } } in
    Pxui.Ui.frame crumb_ui f (fun ui ->
      Pxui_shell.Chrome.update ~title:(fun _ -> "Graph\tfirst / second / third / sop") (L.Leaf L.Graph) ui f) in
  ignore (crumbed []);
  let hit = ref [] in
  List.iter (fun x ->
    let p = (float x, 10.) in
    ignore (crumbed ~mouse:p [ Input.Pointer_moved p ]);
    ignore (crumbed ~mouse:p [ Input.Pointer_pressed (Input.LeftButton, p) ]);
    List.iter (function Pxui_shell.Chrome.Crumb (_, i) -> if not (List.mem i !hit) then hit := i :: !hit | _ -> ())
      (crumbed ~mouse:p [ Input.Pointer_released (Input.LeftButton, p) ])) (List.init 40 (fun i -> 20 + (i * 6)));
  if List.sort compare !hit <> [ 0; 1; 2 ] then
    failwith (Printf.sprintf "breadcrumb clicks reported %s, not the first three parts"
      (String.concat "," (List.map string_of_int (List.sort compare !hit))));
  Pxui.Ui.destroy crumb_ui;
  let hidden = Pxui.Ui.create ~font_size:11 () in
  ignore (Pxui.Ui.frame hidden frame (fun ui ->
    Pxui_shell.Status_bar.draw ui ~bounds:(0, 272, 400, 28) ~text:"Overlay" ~fps:None ()));
  if Pxui.Ui.scene hidden = [] then
    failwith "PXUI did not draw the status bar";
  Pxui.Ui.destroy hidden;
  if Sys.getenv_opt "RAYS_BENCH_LAYOUT" = Some "1" then begin
    let benchmark_frame = { frame with input = { frame.input with size = 1000, 300 } } in
    let start_alloc = Gc.allocated_bytes () and start = Sys.time () in
    let width_sum = ref 0 in
    for _ = 1 to 200_000 do
      let panes = Pxui_shell.Layout.(panes (geometry layout benchmark_frame)) in
      let _, _, width, _ = panes.view in
      width_sum := !width_sum + width
    done;
    Printf.printf "layout geometry: %.4fs %.0f bytes %d\n%!"
      (Sys.time () -. start) (Gc.allocated_bytes () -. start_alloc) !width_sum
  end;
  (* Inspector.record: a plain sketch record edited through its schema. *)
  let schema = Param.(schema ~name:"look" ~default:false
    [ field ~name:"on" ~label:"On" ~kind:Toggle ~default:false
        ~get:Fun.id ~set:(fun on _ -> on) () ]) in
  let ui = Pxui.Ui.create ~font_size:11 () in
  let click = [ Input.Pointer_pressed (Input.LeftButton, (115., 15.));
                Input.Pointer_released (Input.LeftButton, (115., 15.)) ] in
  let record events value = Pxui.Ui.frame ui { frame with input = { frame.input with events } } (fun ui ->
    Pxui.Ui.panel ui ~x:0. ~y:0. ~width:260. "record" (fun () ->
      Pxui_shell.Inspector.record ui schema value)) in
  (match record [] false with
   | Ok (false, effects) when not (Param.has_effects effects) -> ()
   | _ -> failwith "idle inspector frame changed the record");
  (match record click false with
   | Ok (true, effects) when effects.cook -> ()
   | _ -> failwith "inspector toggle did not edit the record");
  Pxui.Ui.destroy ui;
  (* Tree: WAI-ARIA keys through [run_command], and pointer gestures (dead
     zone, before/inside/after drops, toggle paint, double-click). *)
  let module T = Pxui_shell.Tree in
  let row id depth label = { T.id; depth; label; detail = ""; badge = "S", Rays_math.Color.gray 90; link = false;
    ghost = false; flags = [true] } in
  let rows = [| row 1 0 "a"; row 2 1 "b"; row 3 1 "c"; row 4 0 "d" |] in
  let expect name got want = if got <> want then failwith ("tree: " ^ name) in
  let run t command = T.run_command t rows ~selected:[] command in
  let t, intents = run (T.create ()) T.Down in
  expect "Down from nothing selects the first row" intents [T.Select [1]];
  let t, intents = run t T.Down in
  expect "Down moves to the child" intents [T.Select [2]];
  let t2, intents = run t T.Extend_down in
  expect "Shift-Down extends from the anchor" intents [T.Select [3; 2]];
  ignore t2;
  let t, intents = run t T.Collapse in
  expect "Left on a leaf goes to its parent" intents [T.Select [1]];
  let t, intents = run t T.Collapse in
  expect "Left on an open parent folds it" intents [];
  let t, intents = run t T.Down in
  expect "a folded parent hides its children" intents [T.Select [4]];
  let t, _ = run t T.Up in
  let t, intents = run t T.Expand in
  expect "Right unfolds" intents [];
  let t, intents = run t T.Expand in
  expect "Right on an open parent goes to its first child" intents [T.Select [2]];
  let _, intents = run t T.Last in
  expect "End selects the last row" intents [T.Select [4]];
  let _, intents = T.run_command t rows ~selected:[2; 3] T.Indent_rows in
  expect "Tab reparents the selection" intents [T.Indent [2; 3]];
  let _, intents = run t T.Outdent_rows in
  expect "Shift-Tab outdents the focus" intents [T.Outdent [2]];
  let _, intents = run t T.Move_up in
  expect "Alt-Up reorders" intents [T.Reorder { ids = [2]; delta = -1 }];
  let _, intents = run t T.Hide in
  expect "h hides the focused row" intents [T.Flag { ids = [2]; column = 0; value = false }];
  let _, intents = run t T.Activate_row in
  expect "the enter key activates" intents [T.Activate 2];
  let filtering, _ = run t T.Filter in
  expect "/ opens the filter" (T.editing filtering) true;
  let ui = Pxui.Ui.create ~font_size:11 () in
  let center k = 200., 24. +. (24. *. float_of_int k) +. 12. in
  let step t ?(mouse = 0., 0.) events =
    Pxui.Ui.frame ui { frame with input = { frame.input with events; pointer = mouse } } (fun ui ->
      T.update t ui { frame with input = { frame.input with events; pointer = mouse } } ~bounds:(0, 0, 400, 300)
        ~columns:["vis"] rows ~selected:[]) in
  let gesture t from_ to_ =
    let t, _ = step t ~mouse:from_ [] in
    let t, pressed = step t ~mouse:from_ [Input.Pointer_pressed (Input.LeftButton, from_)] in
    let t, moved = step t ~mouse:to_ [Input.Pointer_moved to_] in
    let t, released = step t ~mouse:to_ [Input.Pointer_released (Input.LeftButton, to_)] in
    t, pressed @ moved @ released in
  let moves intents = List.filter (function T.Move _ -> true | _ -> false) intents in
  let chevron = 13., snd (center 0) in
  let t, _ = step (T.create ()) [] in
  let t, intents = step t ~mouse:chevron
      [Input.Pointer_pressed (Input.LeftButton, chevron);
       Input.Pointer_released (Input.LeftButton, chevron)] in
  expect "chevron click does not select or drag" intents [];
  let t, _ = run t T.Down in
  let _, intents = run t T.Down in
  expect "chevron click folds children" intents [T.Select [4]];
  let x, y = center 0 in
  let t, intents = gesture (T.create ()) (x, y) (x +. 2., y +. 1.) in
  expect "a 2-point drag stays inside the dead zone" (moves intents) [];
  let drop k fraction = let x, y = center k in x, y -. 12. +. (24. *. fraction) in
  let t, intents = gesture t (center 1) (drop 3 0.5) in
  expect "the middle of a row drops inside"
    (moves intents) [T.Move { ids = [2]; target = 4; drop = T.Inside }];
  let t, intents = gesture t (center 1) (drop 3 0.1) in
  expect "the top quarter drops before"
    (moves intents) [T.Move { ids = [2]; target = 4; drop = T.Before }];
  let t, intents = gesture t (center 1) (drop 3 0.9) in
  expect "the bottom quarter drops after"
    (moves intents) [T.Move { ids = [2]; target = 4; drop = T.After }];
  let flag k = 386., snd (center k) in
  let t, intents = gesture t (flag 0) (flag 2) in
  expect "dragging down a toggle column paints each row it passes"
    (List.filter (function T.Flag _ -> true | _ -> false) intents)
    [T.Flag { ids = [1]; column = 0; value = false };
     T.Flag { ids = [3]; column = 0; value = false }];
  let click = [Input.Pointer_pressed (Input.LeftButton, center 3);
    Input.Pointer_released (Input.LeftButton, center 3)] in
  let t, _ = step t ~mouse:(center 3) click in
  let _, intents = step t ~mouse:(center 3) click in
  expect "a double-click activates the row"
    (List.filter (function T.Activate _ -> true | _ -> false) intents) [T.Activate 4];
  let scroll_rows = Array.init 20 (fun index -> row (index + 1) 0
      (string_of_int (index + 1))) in
  let scroll_ui = Pxui.Ui.create ~font_size:11 () in
  let scroll_step t events = Pxui.Ui.frame scroll_ui { frame with input = { frame.input with events;
      pointer = 200., 60. } } (fun ui ->
    T.update t ui frame ~bounds:(0, 0, 400, 96)
      ~columns:[] scroll_rows ~selected:[]) in
  let t, _ = scroll_step (T.create ()) [] in
  let t, _ = scroll_step t [Input.Pointer_moved (200., 60.);
    Input.Scrolled (0., -2.)] in
  let t, _ = scroll_step t [] in
  let _, intents = scroll_step t [Input.Pointer_pressed (Input.LeftButton, (200., 60.))] in
  expect "list uses shared scroll for row hits" intents [T.Select [4]];
  Pxui.Ui.destroy scroll_ui;
  (* a sticky ancestor row takes its own click, not the row scrolled under it *)
  let family = Array.init 21 (fun index -> row (index + 1) (if index = 0 then 0 else 1) (string_of_int (index + 1))) in
  let sticky_ui = Pxui.Ui.create ~font_size:11 () in
  let sticky_step t ?(at = 200., 60.) events = Pxui.Ui.frame sticky_ui { frame with input = { frame.input with events; pointer = at } } (fun ui ->
    T.update t ui frame ~bounds:(0, 0, 400, 96) ~columns:[] family ~selected:[]) in
  let t, _ = sticky_step (T.create ()) [] in
  let t, _ = sticky_step t [Input.Pointer_moved (200., 60.); Input.Scrolled (0., -2.)] in
  let t, _ = sticky_step t [] in
  let _, intents = sticky_step t ~at:(200., 36.) [Input.Pointer_pressed (Input.LeftButton, (200., 36.))] in
  expect "a click on a sticky ancestor selects it" intents [T.Select [1]];
  Pxui.Ui.destroy sticky_ui;
  let t, _ = step (T.create ()) [] in
  let t, _ = step t ~mouse:(center 0) [Input.Pointer_pressed (Input.LeftButton, center 0);
    Input.Pointer_released (Input.LeftButton, center 0)] in
  let _, intents = step t ~mouse:(center 2) [Input.Key_pressed Input.Shift;
    Input.Pointer_pressed (Input.LeftButton, center 2); Input.Key_released Input.Shift;
    Input.Pointer_released (Input.LeftButton, center 2)] in
  expect "Shift selection uses the captured press keys" intents [T.Select [3; 1; 2]];
  Pxui.Ui.destroy ui;
  (* a vector's cell has no track: a click without a drag writes nothing, a drag changes the
     value from what it was *)
  let cell name index : Param.field_view =
    { name; label = name; description = None; folder = []; impact = Param.Cook; primary = false;
      unit = None; vec3 = Some ("translate", index);
      kind = Param.Floating_view { soft_min = -10.; soft_max = 10.; hard_min = None; hard_max = None };
      default = Param.Float_value 0.; current = Param.Float_value 1. } in
  let vector = [ { Pxui_shell.Inspector.path = "translate";
    fields = [ cell "translate_x" 0; cell "translate_y" 1; cell "translate_z" 2 ]; shown = false; locked = false;
    drive = None; live = None } ] in
  (* one press at each place per UI: a second one there would be a double-click, which types *)
  let pass gesture =
    let vector_ui = Pxui.Ui.create ~font_size:11 () in
    let cells ?(mouse = 0., 0.) events = Pxui.Ui.frame vector_ui { frame with input = { frame.input with events; pointer = mouse } } (fun ui ->
      Pxui.Ui.panel ui ~x:0. ~y:0. ~width:260. "vector" (fun () ->
        Pxui_shell.Inspector.flow_fields ui vector)) in
    ignore (cells []);
    let found = List.map (fun x ->
      let at = float x, 12. in
      ignore (cells ~mouse:at [Input.Pointer_moved at]);
      let pressed = cells ~mouse:at [Input.Pointer_pressed (Input.LeftButton, at)] in
      let found = pressed @ gesture cells at in
      ignore (cells []); found <> []) [ 90; 110; 130; 150; 170; 190; 210; 230 ] in
    Pxui.Ui.destroy vector_ui; found in
  if List.mem true (pass (fun cells at -> cells ~mouse:at [Input.Pointer_released (Input.LeftButton, at)])) then
    failwith "a click on a vector's cell wrote a value";
  if not (List.mem true (pass (fun cells (x, y) ->
      let moved = cells ~mouse:(x +. 40., y) [Input.Pointer_moved (x +. 40., y)] in
      moved @ cells ~mouse:(x +. 40., y) [Input.Pointer_released (Input.LeftButton, (x +. 40., y))])))
  then failwith "no drag on a vector's cell changed it";
  (* A ranged float is the same field (Kit.number): a click writes nothing, a drag moves the
     value from what it was by a fraction, and a long value keeps its digits (no %.6g) *)
  let scalar value = [ { Pxui_shell.Inspector.path = "radius";
    fields = [ { (cell "radius" 0) with vec3 = None; current = Param.Float_value value } ];
    shown = false; locked = false; drive = None; live = None } ] in
  let edits value gesture =
    let ui = Pxui.Ui.create ~font_size:11 () in
    let build ~mouse events = Pxui.Ui.frame ui { frame with input = { frame.input with events; pointer = mouse } } (fun ui ->
      Pxui.Ui.panel ui ~x:0. ~y:0. ~width:260. "scalar" (fun () ->
        Pxui_shell.Inspector.flow_fields ui (scalar value))) in
    ignore (build ~mouse:(0., 0.) []);
    let at = 180., 12. in
    ignore (build ~mouse:at [Input.Pointer_moved at]);
    let pressed = build ~mouse:at [Input.Pointer_pressed (Input.LeftButton, at)] in
    let found = pressed @ gesture build at in
    Pxui.Ui.destroy ui; found in
  let drag by build (x, y) =
    let moved = build ~mouse:(x +. by, y) [Input.Pointer_moved (x +. by, y)] in
    moved @ build ~mouse:(x +. by, y) [Input.Pointer_released (Input.LeftButton, (x +. by, y))] in
  let floats = List.filter_map (function
    | Pxui_shell.Inspector.Edited (_, Param.Float_value v) -> Some v | _ -> None) in
  if edits 1. (fun build at -> build ~mouse:at [Input.Pointer_released (Input.LeftButton, at)]) <> [] then
    failwith "a click on a ranged number wrote a value";
  (match floats (edits 1. (drag 5.)) with
   | v :: _ when not (Float.is_integer v) && Float.abs (v -. 1.5) < 1e-9 -> ()
   | _ -> failwith "a drag on a float written 1 did not reach 1.5");
  (match floats (edits 1234567.89 (drag (-2.))) with
   | v :: _ when Float.abs (v -. 1234567.69) < 1e-6 -> ()
   | _ -> failwith "a scrub of 1234567.89 lost its digits");
  let module N = Param.Number in
  assert (N.scrub N.Float "1" 10. false = "1.5" && N.scrub N.Float "1" 0.04 false = "1"
          && N.scrub N.Float "1234567.89" 2. false = "1234567.99" && N.scrub N.Int "3" 12. false = "5"
          && N.scrub N.Float ~range:(0., 2.) "1" 500. false = "2.0" && N.scrub N.Int ~range:(0., 5.) "9" 6. false = "9"
          && N.show 1e-7 = "0.0000001" && N.show 0.5 = "0.5");
  (* E18: the key sheet lists every command a key reaches, once, under its host's section *)
  let command ?scope id trigger = Editor_core.Command.make ~id ~label:id ~trigger ?scope id in
  let keymap = Editor_core.Keymap.[
    command "file.save" (Chord (Input.KeyChar 's', [ Input.Meta ]));
    command "file.save" (Chord (Input.KeyChar 's', [ Input.Ctrl ]));
    command "scope.delete" ~scope:"graph" (Chord (Input.Delete, []));
    command "scope.delete" ~scope:"graph" (Chord (Input.KeyChar 'x', []));
    command "panel.close" (Leader "ox");
    Editor_core.Command.make ~id:"palette.only" ~label:"no key" "palette" ] in
  let sections = Pxui_shell.Which_key.sheet_sections
      ~category:(fun (c : _ Editor_core.Command.t) -> if c.scope <> None then "Graph"
        else if String.starts_with ~prefix:"panel." c.id then "Panel" else "File") keymap in
  assert (List.map fst sections = [ "Panel"; "File"; "Graph" ]);
  assert (List.concat_map (fun (_, rows) -> List.map snd rows) sections
          = [ "panel.close"; "file.save"; "scope.delete" ]);
  assert (List.assoc "Graph" sections = [ "x / \xe2\x8c\xa6", "scope.delete" ]);
  print_endline "pxui shell tests passed"

(* Echo: a refusal is a tip in the error ink (no dot), information has its dot; neither is drawn
   for no tips, and a card under a tip does not change what is drawn. *)
let () =
  let tip_instances ?avoid tips =
    let ui = Pxui.Ui.create ~font_size:11 () in
    ignore (Pxui.Ui.frame ui frame (fun ui ->
      Pxui_shell.Status_bar.tips ui ~bounds:(0, 0, 400, 300) ?avoid tips));
    match Scene.Layer.stage_native ~width:400 ~height:300 (Pxui.Ui.scene ui) with
    | Error message -> failwith message
    | Ok staged -> List.fold_left (fun total -> function
        | Scene.Layer.Ui_layer (batch, _) -> total + Scene_command.Ui_batch.count batch
        | _ -> total) 0 staged.layers in
  let refusal = [ "Cannot delete the root", `Refusal ] and info = [ "Saved", `Info ] in
  if tip_instances [] <> 0 then failwith "no tips drew something";
  if tip_instances refusal = 0 then failwith "a refusal tip drew nothing";
  if tip_instances refusal = tip_instances info then
    failwith "a refusal tip is drawn like information (it has no dot)";
  if tip_instances ~avoid:[ 12., 250., 100., 40. ] refusal <> tip_instances refusal then
    failwith "a tip over a card changed what it draws"

(* A header's right end: the groups stand outermost first inside the header and never over one
   another; the title's room shrinks first, then the last group goes. *)
let () =
  let leaf width : Pxui_shell.Layout.leaf =
    { path = [ 0 ]; panel = Graph; frame = 100, 0, width, 200; header = 100, 4, width, 24;
      body = 100, 28, width, 172; floating = false } in
  let slots width = Pxui_shell.Chrome.header_slots (leaf width) [ 100.; 150. ] in
  (match slots 600 with
   | [ Some views; Some tabs ], limit ->
       if views <> 664. then failwith "the first group does not end before the collapse button";
       if tabs <> views -. 108. || limit <> tabs -. 158. then failwith "header groups are not 8 apart"
   | _ -> failwith "a wide header dropped a group");
  (match slots 290 with
   | [ Some views; None ], limit -> if limit <> views -. 108. then failwith "a dropped group kept its room"
   | _ -> failwith "a narrow header did not drop its last group alone");
  (match slots 130 with
   | [ None; None ], limit -> if limit <> 194. then failwith "a header with no groups ends its title at the button"
   | _ -> failwith "a group stands in a header too narrow for it");
  (* whatever the width, no group starts left of the margin or ends over the button, and
     the title ends before the groups *)
  for width = 0 to 700 do
    let placed, limit = slots width in
    List.iter2 (fun slot w -> Option.iter (fun right ->
      if right -. w < 104. || right > float (100 + width - 36) || limit > right -. w
      then failwith "a header group leaves its header") slot) placed [ 100.; 150. ]
  done
