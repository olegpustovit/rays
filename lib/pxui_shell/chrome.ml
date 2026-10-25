open Rays

  open Layout
  type intent =
    | Resize of { node : path; size : Editor_core.Panels.size }
    | Settled
    | Toggle of path
    | Window of path * bounds option
    | Window_drag of path * bounds * bool
    | Dragging of path * bool
    | Dock_panel of path * path * [ `Left | `Right | `Top | `Bottom ]
    | Split_panel of path * axis
    | Split_panel_at of path * axis * float * panel
    | Close_panel of path
    | Retype_panel of path * panel
    | Crumb of path * int

  let floating ui ?(flags = Pxui.Ui.none) (x, y, width, height) label =
    Pxui.Ui.box ui ~flags ~w:(Pxui.Ui.Px (float_of_int width))
      ~h:(Pxui.Ui.Px (float_of_int height)) ~at:(float_of_int x, float_of_int y) label

  let pane_root ui (frame : Frame.t) ~bounds:(x, y, width, height) label =
    Pxui.Ui.box ui ~flags:Pxui.Ui.clickable
      ~w:(Pxui.Ui.Px (float_of_int frame.width))
      ~h:(Pxui.Ui.Px (float_of_int frame.height)) ~at:(0., 0.)
      ~hit:(fun _ -> float x, float y, float width, float height) label

  let key path = String.concat "." (List.map string_of_int path)
  (* a title's path is its parts joined by " / "; a bare slash ("fixed dt 1/60") is part of a word *)
  let split_crumbs sub =
    let n = String.length sub in
    let rec go from i acc =
      if i + 3 > n then List.rev (String.sub sub from (n - from) :: acc)
      else if String.sub sub i 3 = " / " then go (i + 3) (i + 3) (String.sub sub from (i - from) :: acc)
      else go from (i + 1) acc in
    List.map String.trim (go 0 0 [])
  (* a path too long for its room loses its head, not its subject: "… / sop".  Each part keeps its
     index in the title; the dropped head is (-1, "…") *)
  let fit_crumbs width ~room parts =
    let dots = (-1, "\xe2\x80\xa6") in
    let rec fitted parts =
      let row = List.fold_left (fun w (_, part) -> w +. width part +. 8.)
          (float (List.length parts - 1) *. (width "/" +. 8.) -. 8.) parts in
      let over = row > room in
      match parts with
      | [ head; last ] when over && head = dots -> [ last ]
      | head :: _ :: (_ :: _ as rest) when over && head = dots -> fitted (dots :: rest)
      | _ :: (_ :: _ as rest) when over -> fitted (dots :: rest)
      | parts -> parts in
    fitted (List.mapi (fun i part -> i, part) parts)
  (* "Title<TAB>a / b": the kind and its breadcrumb *)
  let split_title text = match String.index_opt text '\t' with
    | Some i -> String.sub text 0 i, String.sub text (i + 1) (String.length text - i - 1)
    | None -> text, ""
  let has_crumbs sub = List.length (split_crumbs sub) > 1
  let retypes = [ "Graph", Graph; "List", List; "Lisp", Lisp; "Inspector", Inspector; "Spreadsheet", Spreadsheet;
                  "Outline", Outline; "Timeline", Timeline; "Viewport", View ""; "Canvas", Canvas "" ]

  let drag_origin ui box bounds signal =
    if signal.Pxui.Ui.pressed then begin
      let x, y, w, h = bounds in
      Pxui.Ui.set_text_state ui box (Some (Printf.sprintf "%d %d %d %d" x y w h))
    end;
    match Option.map (String.split_on_char ' ') (Pxui.Ui.text_state ui box) with
    | Some parts -> (match List.filter_map int_of_string_opt parts with
        | [x; y; w; h] -> x, y, w, h | _ -> bounds)
    | None -> bounds

  let panel_menu ?(state = fun _ -> Editor_core.Panels.default_state) ?(key_of = fun _ -> "")
      ui (l : leaf) box ~at =
    let module Ui = Pxui.Ui in
    Option.iter (fun (x, y) ->
      Ui.set_state ui box 1;
      Ui.set_text_state ui box (Some (Printf.sprintf "%g %g" x y))) at;
    if Ui.state ui box ~default:0 <> 1 then [] else
    let x, y, w, h = l.frame in
    let at = match Option.map (String.split_on_char ' ') (Ui.text_state ui box) with
      | Some [mx; my] -> (match float_of_string_opt mx, float_of_string_opt my with
          | Some mx, Some my -> mx, my | _ -> float x, float (y + h))
      | _ -> float x, float (y + h) in
    let rows = ["Split right", true; "Split down", true;
      (if (state l.path).window = None then "Float" else "Dock"), true; "Close", true; "", false]
      @ List.map (fun (name, _) -> name, true) retypes in
    let last key = if key = "" then "" else String.sub key (String.length key - 1) 1 in
    let keys = [key_of "panel.split-right"; key_of "panel.split-below"; key_of "panel.float";
      key_of "panel.close"; ""] @ List.map (fun (_, panel) -> last (key_of (match panel with
        | Graph -> "panel.graph" | List -> "panel.list" | Lisp -> "panel.lisp"
        | Inspector -> "panel.inspector" | Spreadsheet -> "panel.spreadsheet" | Outline -> "panel.outline"
        | Canvas _ -> "panel.canvas" | Timeline -> "panel.timeline" | View _ -> "panel.viewport"))) retypes in
    let current = 5 + Option.value ~default:0 (List.find_index (fun (_, panel) ->
      match panel, l.panel with View _, View _ | Canvas _, Canvas _ -> true | a, b -> a = b) retypes) in
    match Ui.context_menu ui ~at ~width:232. ~keys ~selected:current ~lead_from:5
        ("workspace-menu-" ^ key l.path) rows with
    | `Open -> []
    | `Dismiss -> Ui.set_state ui box 0; []
    | `Pick i -> Ui.set_state ui box 0;
        [match i with
         | 0 -> Split_panel (l.path, `H) | 1 -> Split_panel (l.path, `V)
         | 2 -> Window (l.path, if (state l.path).window <> None then None else Some (x, y, max 120 w, max 80 h))
         | 3 -> Close_panel l.path
         | i -> Retype_panel (l.path, snd (List.nth retypes (i - 5)))]

  (* Chrome of the retained workspace, painted and hit through PXUI boxes:
     panel backgrounds, splitters, and header bars with a collapse button and a
     right-click menu (split, close, retype). *)
  (* The one layout of a header's right end: the groups of tabs standing there, outermost first and
     [widths] wide, end 8 points apart before the collapse button (a window's dock and close).  The
     right edge of each, and where the title must end.  The title gives way first (its
     breadcrumb, then its label and the focus square: tabs are how a pane is used, the title only
     names it); a group that would not fit in the header is left out, with those after it. *)
  let header_slots (l : leaf) widths =
    let x, _, w, _ = l.header in
    let left = float x +. 4. in
    let right = ref (float (x + w) -. (if l.floating then 64. else 36.)) and fits = ref true in
    let slots = List.map (fun width ->
      fits := !fits && !right -. width >= left;
      if !fits then (let at = !right in right := at -. width -. 8.; Some at) else None) widths in
    slots, !right

  let update ?(state = fun _ -> Editor_core.Panels.default_state) ?(hidden = [ Timeline ]) ?(title = fun (l : leaf) -> Editor_core.Panels.name l.panel) ?(groups = fun (_ : leaf) -> [])
      ?(key_of = fun _ -> "") ?focus tree ui (frame : Frame.t) =
    let module Ui = Pxui.Ui in
    let geometry = geometry ~state ~hidden tree frame in
    let theme = Ui.theme ui in
    (* the window being carried: its edge is the accent and four brackets stand round it *)
    let moving = ref None in
    let edge_color path = if !moving = Some path then theme.accent else Pxui.Theme.border theme in
    List.iteri (fun order l -> match l.panel with
      (* a view paints its own picture; a window over one still has its line-3 edge and the
         sheet fill of its title row's margin *)
      | View _ | Canvas _ | Timeline when l.floating ->
          let box = floating ui l.frame ("workspace-window-edge" ^ key l.path) in
          Ui.to_front ui ~order box;
          Ui.draw_over ui box (fun paint (x, y, w, h) ->
            let line = edge_color l.path in
            let _, hy, _, _ = l.header and _, fy, _, _ = l.frame in
            Ui.Paint.fill paint ~x ~y ~w ~h:(float (hy - fy)) theme.input;
            (* a viewport window: the sheet's white round the picture and a line-2 frame on it *)
            (match l.panel with
             | View _ when (let _, _, bw, _ = l.body in bw > 0) ->
                 let bx, by, bw, bh = l.body in
                 let bx = float bx and by = float by and bw = float bw and bh = float bh in
                 Ui.Paint.fill paint ~x ~y:(float hy) ~w ~h:(by -. float hy) theme.input;
                 Ui.Paint.fill paint ~x ~y:by ~w:(bx -. x) ~h:(y +. h -. by) theme.input;
                 Ui.Paint.fill paint ~x:(bx +. bw) ~y:by ~w:(x +. w -. bx -. bw) ~h:(y +. h -. by) theme.input;
                 Ui.Paint.fill paint ~x:(bx) ~y:(by +. bh) ~w:bw ~h:(y +. h -. by -. bh) theme.input;
                 Ui.Paint.frame paint ~x:bx ~y:by ~w:bw ~h:bh (Pxui.Theme.edge theme)
             | _ -> ());
            Ui.Paint.fill paint ~x ~y ~w ~h:1. line;
            Ui.Paint.fill paint ~x ~y ~w:1. ~h line; Ui.Paint.fill paint ~x:(x +. w -. 1.) ~y ~w:1. ~h line;
            Ui.Paint.fill paint ~x ~y:(y +. h -. 1.) ~w ~h:1. line)
      | View _ | Canvas _ | Timeline -> ()
      | p ->
          let box = floating ui l.frame ("workspace-" ^ String.lowercase_ascii (Editor_core.Panels.name p)
                                        ^ key l.path) in
          if l.floating then Ui.to_front ui ~order box;
          Ui.draw ui box (fun paint (x, y, w, h) ->
            (* a window is a sheet with one line-3 edge; a docked panel is the ground *)
            if l.floating then begin
              Ui.Paint.fill paint ~x ~y ~w ~h theme.input;
              Ui.Paint.stroke paint ~x:(x +. 0.5) ~y:(y +. 0.5) ~w:(w -. 1.) ~h:(h -. 1.) (edge_color l.path)
            end else Ui.Paint.fill paint ~x ~y ~w ~h theme.panel))
      geometry.leaves;
    let intents = ref [] in
    let emit i = intents := i :: !intents in
    (* where the drag tip goes: over the right of the window being moved *)
    let tip = ref None in
    List.iteri (fun n (s : splitter) ->
      let box = floating ui s.bounds ("workspace-gutter-" ^ string_of_int n) in
      Ui.draw ui box (fun paint (x, y, w, h) ->
        Ui.Paint.fill paint ~x ~y ~w ~h (Pxui.Theme.edge theme))) geometry.splitters;
    let headed = List.filter (fun (_, (l : leaf)) -> let _, _, _, h = l.header in h > 0)
      (List.mapi (fun order l -> order, l) geometry.leaves) in
    let headers = List.map (fun (order, (l : leaf)) ->
      let x, y, w, h = l.header in
      let label = String.lowercase_ascii (Editor_core.Panels.name l.panel) ^ key l.path in
      let box = floating ui ~flags:Ui.(clickable + clip) l.header ("workspace-header-" ^ label) in
      if l.floating then Ui.to_front ui ~order box;
      let size = min h 20 in
      (* docked: the collapse chevron, a 20-point button 8 points from the edge.  A window: dock,
         then close. *)
      let windowed = (state l.path).window <> None && not (state l.path).collapsed in
      let button = floating ui ~flags:Ui.(clickable + tab_stop_marked)
          (x + max 0 (w - 8 - size), y + ((h - size) / 2), size, size) ("workspace-collapse-" ^ label) in
      if l.floating then Ui.to_front ui ~order button;
      if (Ui.signal ui button).clicked then emit (if windowed then Close_panel l.path else Toggle l.path);
      let tools = 8 + size + (if windowed then size + 8 else 0) in
      if windowed then begin
        let dock = floating ui ~flags:Ui.(clickable + tab_stop_marked)
            (x + max 0 (w - tools), y + ((h - size) / 2), size, size) ("workspace-dock-" ^ label) in
        Ui.to_front ui ~order dock;
        let signal = Ui.signal ui dock in
        if signal.clicked then emit (Window (l.path, None));
        Ui.draw ui dock (fun paint (x, y, w, h) ->
          if signal.hovered then Ui.Paint.fill paint ~x ~y ~w ~h (Pxui.Theme.hover_fill theme);
          if Ui.focused ui dock then Ui.Paint.fill paint ~x ~y:(y +. h -. 1.) ~w ~h:1. theme.accent;
          (* the dock mark: a chevron-down in ink-2, as the sheet's window rows *)
          Ui.Paint.chevron paint ~at:(x +. (w /. 2.), y +. (h /. 2.)) `Down (Pxui.Theme.ink_2 theme))
      end;
      let grip = floating ui ~flags:Ui.(clickable + blocking)
          (x, y, max 0 (w - tools), h) ("workspace-drag-" ^ label) in
      if l.floating then Ui.to_front ui ~order grip;
      let drag = Ui.signal ui grip in
      (* a window is dragged from where it is drawn (the layout keeps it inside the frame, whatever
         its saved place says), with the size it has when it is not collapsed *)
      let original = match (state l.path).window with
        | Some (_, _, ww, wh) when l.floating -> let fx, fy, _, _ = l.frame in fx, fy, ww, wh
        | Some window -> window
        | None -> if l.floating then l.frame else (x, y, max 120 w, max 80 (let _, _, _, bh = l.body in h + bh)) in
      let ox, oy, ow, oh = drag_origin ui grip original drag in
      (* Escape puts the panel back where the drag began (a docked one stays docked) *)
      let cancelled = (Ui.state ui grip ~default:0 = 1 && not drag.pressed)
                      || (drag.held && Ui.key_pressed ui Rays.Input.Escape) in
      Ui.set_state ui grip (if cancelled && not drag.released then 1 else 0);
      if drag.held || drag.released then begin
        let px, py = if drag.released then drag.release_point else drag.pointer in
        let sx, sy = drag.press_point in
        if Float.hypot (px -. sx) (py -. sy) >= 4. then begin
          if cancelled then begin
            if (state l.path).window <> None then emit (Window_drag (l.path, (ox, oy, ow, oh), drag.released))
          end else begin
            emit (Dragging (l.path, drag.released));
            (* as far as the layout shows it ({!Layout.geometry}): a place past the edge would be
               saved and not drawn, and the next drag would start from where the window is not *)
            let wx = max 0 (min (frame.width - min frame.width ow) (ox + int_of_float (Float.round (px -. sx))))
            and wy = max 0 (min (frame.height - header_height) (oy + int_of_float (Float.round (py -. sy)))) in
            if not drag.released then (tip := Some (wx, wy, ow); moving := Some l.path);
            emit (Window_drag (l.path, (wx, wy, ow, oh), drag.released))
          end
        end
      end;
      (* the whole empty header drags; a click that did not move opens the menu *)
      let still = Float.hypot (fst drag.release_point -. fst drag.press_point)
          (snd drag.release_point -. snd drag.press_point) < 4. in
      let at = if Ui.context_clicked drag then Some drag.release_point
        else if drag.clicked && still then Some (float x, float (y + h)) else None in
      List.iter emit (panel_menu ~state ~key_of ui l box ~at);
      (* the parts of a docked breadcrumb before its last are buttons: a click is [Crumb] of its index *)
      let main, sub = split_title (title l) in
      if sub <> "" && not l.floating && not (state l.path).collapsed && float w >= 60. then begin
        let width part = Ui.text_width ui part in
        let focused = focus = Some l.path in
        let limit = snd (header_slots l (groups l)) in
        let tx = float x +. 12. +. (if focused then 12. else 0.) +. Kit.cap_width ui main +. 8. in
        let tx = ref tx in
        let parts = fit_crumbs width ~room:(limit -. !tx) (split_crumbs sub) in
        List.iteri (fun n (index, part) ->
          let pw = width part in
          if index >= 0 && n < List.length parts - 1 && !tx +. pw <= limit then begin
            let b = floating ui ~flags:Ui.clickable (int_of_float !tx, y, int_of_float (Float.ceil pw), h)
                ("workspace-crumb-" ^ label ^ "-" ^ string_of_int index) in
            if l.floating then Ui.to_front ui ~order b;
            if (Ui.signal ui b).clicked then emit (Crumb (l.path, index))
          end;
          tx := !tx +. pw +. 8. +. (width "/" +. 8.)) parts
      end;
      l, title l, box, button, grip) headed in
    (* the tip of a panel being moved: a 20-point sheet with a line-2 edge, the label size in ink *)
    Option.iter (fun (wx, wy, ww) ->
      let words = "release to dock \xc2\xb7 esc keeps it floating" in
      let tw = Ui.text_width ui ~size:(Kit.cap_size ui) words in
      let w = tw +. 14. in
      let tx = Float.min (float (frame.width - 4) -. w) (float wx +. (0.625 *. float ww))
      and ty = Float.max 4. (float wy -. 30.) in
      let box = Ui.box ui ~w:(Ui.Px w) ~h:(Ui.Px 20.) ~at:(tx, ty) "workspace-drag-tip" in
      Ui.to_front ui ~order:max_int box;
      Ui.draw ui box (fun paint (x, y, w, h) ->
        Ui.Paint.fill paint ~x ~y ~w ~h theme.input;
        Ui.Paint.stroke paint ~x:(x +. 0.5) ~y:(y +. 0.5) ~w:(w -. 1.) ~h:(h -. 1.) (Pxui.Theme.edge theme);
        Ui.Paint.text paint ~size:(Kit.cap_size ui) ~at:(x +. 7., Kit.cap_y ui y h) ~color:theme.foreground words)) !tip;
    Option.iter (fun path ->
      List.iter (fun (l : leaf) -> if l.path = path then begin
        let fx, fy, fw, fh = l.frame in
        let box = floating ui (fx - 8, fy - 8, fw + 16, fh + 16) "workspace-moving" in
        Ui.to_front ui ~order:(max_int - 1) box;
        (* the padding box of the window, brackets 4 points outside it *)
        Ui.draw ui box (fun paint _ ->
          Ui.Paint.brackets paint ~x:(float fx +. 1.) ~y:(float fy +. 1.) ~w:(float fw -. 2.) ~h:(float fh -. 2.) theme.accent)
      end) geometry.leaves) !moving;
    List.iter (fun ((l : leaf), text, box, button, _grip) ->
      let collapse_hovered = (Ui.signal ui button).hovered in
      (* the keyboard's mark is the sheet's: an accent line over the last row of the button *)
      if Ui.focused ui button then Ui.draw_over ui button (fun paint (x, y, w, h) ->
        Ui.Paint.fill paint ~x ~y:(y +. h -. 1.) ~w ~h:1. theme.accent);
      let panel_state = state l.path in
      let collapsed = panel_state.collapsed || List.mem l.panel hidden in
      let windowed = panel_state.window <> None && not panel_state.collapsed in
      let focused = focus = Some l.path in
      let text_box = Ui.within ui box (fun () -> Ui.box ui ~w:Ui.Grow ~h:Ui.Grow "header-text") in
      Ui.draw ui text_box (fun paint (x, y, w, h) ->
        Ui.Paint.fill paint ~x ~y ~w ~h (if l.floating then theme.input else theme.panel);
        let ink_2 = Pxui.Theme.ink_2 theme and ink_3 = Pxui.Theme.ink_3 theme in
        (* "Title<TAB>a / b": the kind as a label, then the breadcrumb, its last part in ink *)
        let main, sub = split_title text in
        if w >= 60. then begin
          let tx = ref (x +. 12.) in
          (* the accent square marks the focused pane, docked or floating *)
          (* the title ends before what follows it: the pane's tabs, then the window's tools; a
             square or a label with no room is left out whole, and the breadcrumb with it *)
          let limit = snd (header_slots l (groups l)) in
          if focused then begin
            if !tx +. 6. <= limit then
              Ui.Paint.fill paint ~x:!tx ~y:(y +. (h /. 2.) -. 3.) ~w:6. ~h:6. theme.accent;
            tx := !tx +. 12.
          end;
          let labelled = !tx +. Ui.Paint.cap_width paint main <= limit in
          let limit = if labelled then limit else !tx in
          if labelled then
            Ui.Paint.cap paint ~at:(!tx, Kit.cap_y ui y h) ~color:(if focused then theme.foreground else ink_2) main;
          tx := !tx +. Ui.Paint.cap_width paint main +. 8.;
          let ty = Kit.text_y ui y h in
          let cut ?size part = Ui.ellipsis ~width:(Ui.Paint.text_width paint ?size) ~limit:(Float.max 0. (limit -. !tx)) part in
          let put color part = let part = cut part in
            if part <> "" then Ui.Paint.text paint ~at:(!tx, ty) ~color part;
            tx := !tx +. Ui.Paint.text_width paint part +. 8. in
          let rec crumbs = function
            | [] -> ()
            | [ (_, last) ] -> put (if has_crumbs sub then theme.foreground else ink_2) last
            | (_, part) :: rest -> put ink_2 part; put ink_3 "/"; crumbs rest in
          let crumbs parts = crumbs (fit_crumbs (fun part -> Ui.Paint.text_width paint part) ~room:(limit -. !tx) parts) in
          (* a window's title row: the path is one string in ink-2 at the label size *)
          if sub <> "" && l.floating && not collapsed then begin
            (* the subject follows the kind, 8 points on (windows.html's title row) *)
            let size = Kit.cap_size ui in
            Ui.Paint.text paint ~size ~at:(!tx, Kit.cap_y ui y h)
              ~color:(Rays.Color.with_alpha ink_2 179) (cut ~size sub)
          end
          else if sub <> "" && not collapsed then crumbs (split_crumbs sub);
          if collapsed then put ink_3 "collapsed"
        end;
        let cx = x +. w -. 18. and cy = y +. (h /. 2.) in
        if collapse_hovered then Ui.Paint.fill paint ~x:(cx -. 10.) ~y:(cy -. 10.)
          ~w:20. ~h:20. (Pxui.Theme.hover_fill theme);
        if windowed then begin
          (* the window's close mark is the label-size multiplication sign, centred in its button *)
          let size = Kit.cap_size ui in
          Ui.Paint.text paint ~size ~color:ink_2
            ~at:(cx -. (Ui.Paint.text_width paint ~size "\xc3\x97" /. 2.), Kit.cap_y ui y h) "\xc3\x97"
        end else Ui.Paint.chevron paint ~at:(cx, cy) (if collapsed then `Down else `Left)
                                     (if l.floating then ink_2 else theme.foreground))) headers;
    List.rev !intents

  (* where a header's tools begin: after its title, the 8-point gap, the 1 x 12 rule between
     two 4-point margins and the next 8-point gap (the title is laid out as [update] draws it) *)
  let tools_start ui ~focused ~floating ~collapsed text =
    let main, sub = split_title text in
    let x = 12. +. (if focused then 12. else 0.) +. Kit.cap_width ui main in
    let x = if sub = "" then x
      else if floating then x +. 8. +. Pxui.Ui.text_width ui ~size:(Kit.cap_size ui) sub
      else List.fold_left (fun x part -> x +. 8. +. Pxui.Ui.text_width ui part) x
          (List.concat (List.mapi (fun i part -> if i = 0 then [ part ] else [ "/"; part ])
             (split_crumbs sub))) in
    let x = if collapsed then x +. 8. +. Pxui.Ui.text_width ui "collapsed" else x in
    x +. 25.

  let drop_targets ui ~dragging ~(geometry : geometry) ~state =
    match dragging with
    | None -> []
    | Some (source, released) ->
        List.concat_map (fun (leaf : leaf) ->
          if leaf.path = source || leaf.floating
              || (state leaf.path).Editor_core.Panels.collapsed then [] else
          let x, y, w, h = leaf.body in
          let ex = min 36 (w / 3) and ey = min 36 (h / 3) in
          (* the pane's own box sits under its four edges, so the pane under the pointer is known *)
          let pane = floating ui ~flags:Pxui.Ui.clickable (x, y, w, h) ("workspace-drop-pane-" ^ key leaf.path) in
          Pxui.Ui.to_front ui ~order:max_int pane;
          let pane_hovered = (Pxui.Ui.signal ui pane).hovered in
          let edges = List.map (fun (side, rect) ->
            let suffix = match side with `Left -> "left" | `Right -> "right" | `Top -> "top" | `Bottom -> "bottom" in
            let box = floating ui ~flags:Pxui.Ui.(clickable + blocking) rect
              ("workspace-drop-" ^ key leaf.path ^ suffix) in
            Pxui.Ui.to_front ui ~order:max_int box;
            side, suffix, box, (Pxui.Ui.signal ui box).hovered)
            [ `Left, (x, y + ey, ex, max 0 (h - 2 * ey));
              `Right, (x + w - ex, y + ey, ex, max 0 (h - 2 * ey));
              `Top, (x, y, w, ey); `Bottom, (x, y + h - ey, w, ey)] in
          (* only the pane under the pointer shows its four bars; the nearest turns accent and the
             half it would take is tinted *)
          let under = pane_hovered || List.exists (fun (_, _, _, hovered) -> hovered) edges in
          let theme = Pxui.Ui.theme ui in
          let fx = float x and fy = float y and fw = float w and fh = float h in
          List.filter_map (fun (side, suffix, box, hovered) ->
            if under then Pxui.Ui.draw ui box (fun paint _ ->
              let module P = Pxui.Ui.Paint in
              let color = if hovered then theme.accent else Pxui.Theme.border theme in
              (match side with
               | `Top -> P.fill paint ~x:(fx +. (fw /. 2.) -. 24.) ~y:(fy +. 6.) ~w:48. ~h:4. color
               | `Bottom -> P.fill paint ~x:(fx +. (fw /. 2.) -. 24.) ~y:(fy +. fh -. 10.) ~w:48. ~h:4. color
               | `Left -> P.fill paint ~x:(fx +. 6.) ~y:(fy +. (fh /. 2.) -. 24.) ~w:4. ~h:48. color
               | `Right -> P.fill paint ~x:(fx +. fw -. 10.) ~y:(fy +. (fh /. 2.) -. 24.) ~w:4. ~h:48. color);
              if hovered then begin
                let hx, hy, hw, hh = match side with
                  | `Top -> fx, fy, fw, fh /. 2. | `Bottom -> fx, fy +. (fh /. 2.), fw, fh /. 2.
                  | `Left -> fx, fy, fw /. 2., fh | `Right -> fx +. (fw /. 2.), fy, fw /. 2., fh in
                P.fill paint ~x:hx ~y:hy ~w:hw ~h:hh (Pxui.Theme.tint theme);
                P.dashed_rect paint ~x:(hx +. 0.5) ~y:(hy +. 0.5) ~w:(hw -. 1.) ~h:(hh -. 1.) theme.accent;
                let label = "split " ^ suffix in
                P.cap paint ~color:theme.accent
                  ~at:(hx +. ((hw -. P.cap_width paint label) /. 2.), hy +. (hh /. 2.) -. 6.) label
              end);
            if released && hovered then Some (Dock_panel (source, leaf.path, side)) else None) edges)
          geometry.leaves

  (* The draggable gutters, wider than they are drawn.  Build them after the panes so
     they sit on top of the neighbours' hit rectangles. *)
  let splitters ?(state = fun _ -> Editor_core.Panels.default_state) ?(hidden = [ Timeline ]) tree ui (frame : Frame.t) =
    let module Ui = Pxui.Ui in
    let intents = ref [] in
    List.iteri (fun n (s : splitter) -> match s.node with
      | None -> ()
      | Some node ->
          let x, y, width, height = s.bounds in
          let gx, gy = if s.axis = `H then 3, 0 else 0, 3 in
          let box = Ui.box ui ~flags:Ui.(clickable + blocking)
              ~at:(float x, float y) ~w:(Ui.Px (float width)) ~h:(Ui.Px (float height))
              ~hit:(fun _ -> float (x - gx), float (y - gy),
                float (width + (2 * gx)), float (height + (2 * gy)))
              ("workspace-grip-" ^ string_of_int n) in
          let signal = Ui.signal ui box in
          if signal.hovered || signal.held then Ui.request_cursor ui
            (if s.axis = `H then `Horizontal_resize else `Vertical_resize);
          if signal.held then begin
            Ui.draw ui box (fun paint (x, y, w, h) ->
              Ui.Paint.fill paint ~x ~y ~w ~h (Ui.theme ui).accent);
            (* the ratio it makes, in a tip beside the gutter: "58 / 42" *)
            let first, second = sides s in
            let a = int_of_float (Float.round (100. *. float first /. float (max 1 (first + second)))) in
            let tw = Kit.cap_width ui (Printf.sprintf "%d / %d" a (100 - a)) in
            let px, py = signal.pointer in
            let tx, ty = if s.axis = `H then float (x + 8), py -. 10. else px +. 8., float (y + 8) in
            let tip = Ui.box ui ~w:(Ui.Px tw) ~h:(Ui.Px 20.) ~at:(tx, ty) "workspace-gutter-tip" in
            Ui.to_front ui ~order:max_int tip;
            Ui.draw ui tip (fun paint (x, y, _, h) ->
              Ui.Paint.ratio paint ~at:(x, Kit.cap_y ui y h) a (100 - a))
          end;
          let dx, dy = signal.drag in
          if (signal.held || signal.released) && (if s.axis = `H then dx else dy) <> 0. then begin
            let px, py = signal.pointer in
            let along = if s.axis = `H then px else py in
            (* a fixed split is dragged in whole points of its fixed side, any other by ratio *)
            let size : Editor_core.Panels.size = match s.size with
              | `Ratio _ -> `Ratio ((along -. float s.start) /. float (max 1 s.span))
              | `First _ -> `First (int_of_float (Float.round along) - s.start)
              | `Second _ -> `Second (s.start + s.span - splitter_width - int_of_float (Float.round along)) in
            intents := Resize { node; size = Editor_core.Panels.clamp_size size } :: !intents
          end;
          if signal.released then intents := Settled :: !intents;
          (* a right-click sizes the split another way, keeping what it shows *)
          let clicked = Ui.context_clicked signal in
          (* the menu opens where the gutter was clicked *)
          if clicked then (let px, py = signal.release_point in
            Ui.set_text_state ui box (Some (Printf.sprintf "%g %g" px py)));
          let opened = Ui.state ui box ~default:0 = 1 || clicked in
          Ui.set_state ui box (if opened then 1 else 0);
          if opened then begin
            let at = match Option.map (String.split_on_char ' ') (Ui.text_state ui box) with
              | Some [ px; py ] -> float_of_string px, float_of_string py
              | _ -> float x, float y in
            match Ui.context_menu ui ~at ("workspace-size-menu-" ^ string_of_int n) (size_rows s) with
            | `Open -> ()
            | `Dismiss -> Ui.set_state ui box 0
            | `Pick i -> Ui.set_state ui box 0;
                intents := Settled :: Resize { node; size = resized s (List.nth size_ways i) } :: !intents
          end)
      (geometry ~state ~hidden tree frame).splitters;
    List.iteri (fun order (leaf : leaf) -> match (state leaf.path).window with
      | None -> ()
      | Some _ when not (state leaf.path).collapsed ->
          let bounds = leaf.frame in
          let x, y, w, h = bounds in
          let box = floating ui ~flags:Ui.(clickable + blocking)
            (x + w - 12, y + h - 12, 12, 12) ("workspace-window-size-" ^ key leaf.path) in
          Ui.to_front ui ~order box;
          let signal = Ui.signal ui box in
          let x, y, w, h = drag_origin ui box bounds signal in
          if signal.held || signal.released then begin
            let px, py = if signal.released then signal.release_point else signal.pointer in
            let sx, sy = signal.press_point in
            if px <> sx || py <> sy then intents := Window_drag (leaf.path, (x, y,
              max 120 (w + int_of_float (Float.round (px -. sx))),
              max 80 (h + int_of_float (Float.round (py -. sy)))), signal.released) :: !intents
          end;
          Ui.draw ui box (fun paint (x, y, w, h) ->
            let ink = Pxui.Theme.ink_3 (Ui.theme ui) in
            (* an 8-point corner of two 1-point lines, 4 points in from the window's edges *)
            Ui.Paint.fill paint ~x:(x +. w -. 5.) ~y:(y +. h -. 12.) ~w:1. ~h:8. ink;
            Ui.Paint.fill paint ~x:(x +. w -. 12.) ~y:(y +. h -. 5.) ~w:8. ~h:1. ink)
      | Some _ -> ()) (geometry ~state ~hidden tree frame).leaves;
    List.rev !intents
