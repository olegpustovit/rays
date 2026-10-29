(* The graph pane's frame gestures; private state, commands and painting compose
   here on the shared PXUI handle. The public Scope interface is unchanged. *)
open Rays
open Scope_wires
open Scope_state
open Scope_commands
open Scope_paint

(* Keep these tiny transforms local in the frame and painter: opaque dev builds
   otherwise box their float results at every module boundary. *)
let sx t gx = float t.x +. t.pan_x +. (gx *. t.zoom)
let sy t gy = float t.y +. t.pan_y +. (gy *. t.zoom)
let left_button (s : Ui.signal) = s.button = Some Input.LeftButton

(* the number field of a row, the kit's ([Param.Number], as [Pxui_shell.Kit.number]): the kind
   is the row's type, and only a literal with no typed row behind it is read by its spelling *)
let num_field ui ~at ~w ~h ?size ?kind ?range ?(nonfinite = false) label text =
  let module N = Param.Number in
  let nonfinite = nonfinite && kind <> Some N.Int && range = None in
  let kind =
    match kind with
    | Some kind -> kind
    | None ->
        if
          String.exists (fun c -> c = '.' || c = 'e') text
          || List.mem text [ "NaN"; "Infinity"; "-Infinity" ]
        then N.Float
        else N.Int
  in
  let valid s =
    match Flow.Syntax.number_value s with Some f -> Float.is_finite f || nonfinite | None -> false
  in
  let text' = Ui.number_field ui ~at ~w ~h ?size ~kind ?range ~valid label text in
  if text' <> text && valid text' then Some text' else None

(* what a row's field is: its type's kind (a vector's cells are floats) and its soft range *)
let kind_of (r : P.row) =
  match r.ty with
  | Some (Ty.Float | Ty.Vec2 | Ty.Vec3 | Ty.Vec4) -> Some Param.Number.Float
  | Some Ty.Int -> Some Param.Number.Int
  | _ -> None

let range_of (r : P.row) = match r.control with P.Range (lo, hi) -> Some (lo, hi) | _ -> None

(* the row of a card at a point of it ([px], [py] relative to the card, graph units): the header's
   in-port row at the top left (anywhere in the header with [header]), else the row of the line
   there; nothing for a folder label or a card drawn as a chip or point *)
let row_at ?(header = 14.) (p : P.placed) (n : P.node) ~shown ~px ~py =
  if shown = P.Point || shown = P.Chip then
    if px < header && py < P.head_height then List.find_index (fun (r : P.row) -> r.head) n.rows
    else None
  else if py < P.body_top then
    if px < header then List.find_index (fun (r : P.row) -> r.head) n.rows else None
  else
    let k =
      int_of_float
        (Float.floor ((py -. P.body_top -. (note_rows n *. P.row_height)) /. P.row_height))
    in
    if k < 0 || k >= Array.length p.lines then None
    else match p.lines.(k) with P.Row (i, _) -> Some i | _ -> None

(* the nodes of one scope a rubber band touches (an expanded zone only when it is inside it) *)
let marquee_hits t (rx, ry, rw, rh) =
  let z = t.zoom in
  let hits =
    Array.fold_left
      (fun acc ((p : P.placed), ax, ay) ->
        match p.item with
        | P.Item _ ->
            let x, y, w, h = (sx t ax, sy t ay, p.w *. z, p.h *. z) in
            let touches = x < rx +. rw && x +. w > rx && y < ry +. rh && y +. h > ry in
            let inside = x >= rx && y >= ry && x +. w <= rx +. rw && y +. h <= ry +. rh in
            if ((p.inner = None || p.collapsed) && touches) || inside then p.path :: acc else acc
        | _ -> acc)
      [] t.geo.items
  in
  match List.rev hits with
  | [] -> Path_set.empty
  | first :: _ as hits ->
      let scope = scope_of_path first in
      Path_set.of_list (List.filter (fun p -> scope_of_path p = scope) hits)

let valid_name s =
  s <> ""
  && not
       (String.exists (fun c -> List.mem c [ ' '; '('; ')'; '['; ']'; '{'; '}'; '"'; ';'; '\'' ]) s)

let single_form text = match Flow.Syntax.parse text with Ok [ form ] -> Some form | _ -> None

let context_items t path =
  match (t.imported path, node_of t path) with
  | Some file, _ -> [ ("Open " ^ file, true) ]
  | None, None -> []
  | None, Some n ->
      let parent, arm = conditional_target n in
      let conditional =
        Option.fold ~none:false
          ~some:(fun (n : P.node) -> List.mem n.head [ "cond"; "case" ])
          (node_of t parent)
      in
      let zone = match n.zone with Some { kind = P.Let; _ } | None -> None | Some _ -> Some n in
      [
        ("Fold into its use", true);
        ("Unfold a call", true);
        ("Hoist out", n.invariant);
        ( (match zone with
          | Some n -> if t.collapsed n.path then "Expand zone" else "Collapse zone"
          | None -> "Collapse zone"),
          zone <> None );
        ("Toggle bypass", true);
        ("Repeat (loop)", true);
        ("Iterate (feed back)", true);
        ("Make function", true);
        ("Make macro", true);
        ("Make defn", true);
        ("Delete", true);
        ("Wrap conditional", true);
        ("Add arm", conditional);
        ( (if parent = n.path then "Delete first arm" else "Delete arm"),
          conditional && (parent = n.path || arm >= 0) );
        ("Wrap in map", true);
        ("Wrap in filter", true);
        ("Inline function", n.zone = None && n.macro = None && not (String.contains n.head '/'));
        ("Move up in text", true);
        ("Move down in text", true);
      ]

let context_command = function
  | 0 -> Fold_into
  | 1 -> Unfold
  | 2 -> Hoist
  | 3 -> Collapse
  | 4 -> Bypass
  | 5 -> Wrap_repeat
  | 6 -> Wrap_iterate
  | 7 -> Make_fn
  | 8 -> Make_macro
  | 9 -> Make_defn
  | 11 -> Wrap_if
  | 12 -> Add_arm
  | 13 -> Delete_arm
  | 14 -> Wrap_map
  | 15 -> Wrap_filter
  | 16 -> Inline_fn
  | 17 -> Item_up
  | 18 -> Item_down
  | _ -> Delete

(* the glyph of an expression row opens it as cards; any other inline call unfolds into a binding *)
let unfold_row (n : P.node) (r : P.row) =
  match (r.expr, r.key) with
  (* only an input of a call holds a nested card: a record field or a binding unfolds into a binding *)
  | Some e, (E.Pos _ | E.Kw _ | E.Arm _) when E.value_call e ->
      [
        Zone_collapsed
          {
            zone =
              List.mapi
                (fun i s -> if i = List.length n.path - 1 then E.nested_leaf n.name r.key else s)
                n.path;
            collapsed = false;
          };
      ]
  | _ -> [ Syntax_edit (E.Unfold { node = n.path; key = r.key; sub = [] }) ]

(* the footer of a card at the iterations pinned in its zones: its value is the ghost beside the out-port *)
let footer_of t records (n : P.node) =
  let chain = Option.value ~default:[] (Hashtbl.find_opt t.chains n.path) in
  Flow_graph.Probe.footer records n ~probes:(List.map t.probe chain)

(* the frame's paint: grid and wires under the cards, the cards, then the overlays *)
let paint_frame (t : t) ui ~z ~fs ~layer ~canvas ~(canvas_signal : Ui.signal) ~tiles ~zones
    ~wires_in_view ~footers ~chain ~mouse ~hover =
  (* the frame's paint *)
  let selected = t.selected in
  let snapshot = t in
  let row_hover =
    match hover with Some ((n : P.node), _, _, i) -> Some (n.path, i) | None -> None
  in
  (* a scene object nothing reads (taken out of its merge) is dimmed; a wire from it brings it back *)
  let read = t.geo.read in
  let dimmed (n : P.node) =
    (not n.synthetic) && n.zone = None
    && String.starts_with ~prefix:"scene/" n.head
    && n.head <> "scene/merge" && n.head <> "scene/root"
    &&
    match box_at t n.path with
    | Some (ax, ay, w, _) -> not (Hashtbl.mem read (ax +. w, ay +. wire_head_y))
    | None -> false
  in
  Ui.draw ui layer (fun paint (rx, ry, rw, rh) ->
      paint_background paint snapshot ~viewport:(rx, ry, rw, rh) ~wires:wires_in_view zones);
  List.iter
    (fun ((p : P.placed), ax, ay, tile, _, _, _) ->
      let path = p.path in
      let isel = Path_set.mem path selected in
      let hovered = t.drag = None && t.carry_hot = None && Ui.hovered_within ui tile in
      Ui.draw ui tile (fun paint (x, y, w, h) ->
          match p.item with
          | P.Input _ when p.shown = P.Point ->
              paint_point paint ui snapshot ~z ~fs p ~selected:isel (x, y, w, h)
          | P.Input i -> paint_input paint ui snapshot ~z ~fs i ~selected:isel (x, y, w, h)
          | Return -> (
              match snapshot.scope with
              | Some s -> paint_return paint ui snapshot ~z ~fs s ~selected:isel (x, y, w, h)
              | None -> ())
          | Item n -> (
              (match n.zone with
              | Some zn when not p.collapsed ->
                  paint_zone_frame paint ui snapshot ~z ~fs
                    ?footer:(Hashtbl.find_opt footers n.path) n zn ~selected:isel
                    ~out_wired:(Hashtbl.mem read (out_anchor p ax ay))
                    (x, y, w, h) ~probe:(snapshot.probe n.path) ~count:(count_of snapshot n.path)
              | _ ->
                  let rh =
                    match row_hover with Some (rp, i) when rp = n.path -> Some i | _ -> None
                  in
                  paint_node paint ui snapshot ~z ~fs ?footer:(Hashtbl.find_opt footers n.path)
                    ?lens_step:(lens_of snapshot n.path) ~hovered
                    ~carry:
                      (match snapshot.carry_hot with Some (hp, _) -> hp = n.path | None -> false)
                    ~out_wired:(Hashtbl.mem read (out_anchor p ax ay))
                    ~shown:p.shown
                    ?failed:(List.assoc_opt n.path snapshot.failed)
                    p n ~selected:isel ~row_hover:rh (x, y, w, h);
                  if dimmed n then
                    Ui.Paint.fill paint ~x ~y ~w ~h
                      (Rays_math.Color.with_alpha snapshot.theme.panel 150));
              (* the chain of the selection stays; the rest of the cards recede *)
              match chain with
              | Some c
                when (not (node_lit c path))
                     && match p.item with P.Item { zone = Some _; _ } -> p.collapsed | _ -> true ->
                  Ui.Paint.fill paint ~x ~y ~w ~h
                    (Rays_math.Color.with_alpha snapshot.theme.panel 170)
              | _ -> ())))
    tiles;
  let drawn_items = List.length tiles in
  let drawn_zones = List.length zones in
  (* the rubber band and the expanded chip, over the tiles *)
  let band = match t.drag with Some (Wiring w) -> Some w | _ -> None in
  let marks =
    if t.carry_lit = [] && t.carry_hot = None then []
    else
      List.filter_map
        (fun ((p : P.placed), ax, ay, _, _, _, _) ->
          let rect = (sx t ax, sy t ay, p.w *. z, p.h *. z) in
          match (List.assoc_opt p.path t.carry_lit, t.carry_hot) with
          | letter, Some (hot, ok) when hot = p.path -> Some (rect, letter, Some ok)
          | Some letter, _ -> Some (rect, Some letter, None)
          | None, _ -> None)
        tiles
  in
  let canvas_hot =
    match t.carry_hot with Some ([ g ], ok) when g = t.key -> Some ok | _ -> None
  in
  (* the view flag of the displayed node: a tip below it after the pointer rests there *)
  let tip_at, tip_open =
    match t.display with
    | Some path -> (
        match item_at t path with
        | Some (({ item = P.Item _; _ } as p), ax, ay) when p.shown <> P.Point ->
            let fx = sx t ax +. ((p.w -. head_pad -. 12.) *. z) and fy = sy t ay +. (6. *. z) in
            let over = Ui.contains (fx, fy, 12. *. z, 12. *. z) mouse in
            ( Some (sx t ax +. (116. *. z), sy t ay +. (32. *. z)),
              over && Ui.hover_delay ui ~key:"scope-flag" )
        | _ -> (None, false))
    | None -> (None, false)
  in
  (* the red [t] of a header: a tip after the pointer rests on it, saying what it is *)
  let live_tip =
    List.find_map
      (fun ((p : P.placed), ax, ay, tile, _, _, _) ->
        match p.item with
        | P.Item { live = true; _ } when p.shown <> P.Point && Ui.hovered_within ui tile ->
            let right = sx t ax +. ((p.w -. head_pad) *. z) in
            let mx, my = mouse in
            let top = sy t ay in
            if
              mx >= right -. (14. *. z)
              && mx <= right +. (2. *. z)
              && my >= top
              && my < top +. (P.head_height *. z)
              && Ui.hover_delay ui ~key:"scope-live"
            then Some (right -. 150., top +. (P.head_height *. z) +. 2.)
            else None
        | _ -> None)
      tiles
  in
  let hint_chip paint ~bg (x, y) letter =
    let letter = String.uppercase_ascii letter in
    let size = max 8 (Ui.font_size ui - 2) in
    let lw = Float.max 14. (Ui.Paint.text_width paint ~size letter +. 6.) in
    Ui.Paint.fill paint ~x:(x -. 7.) ~y:(y -. 7.) ~w:lw ~h:14. bg;
    let tw = Ui.Paint.text_width paint ~size letter in
    Ui.Paint.text paint
      ~at:(x -. 7. +. ((lw -. tw) /. 2.), Ui.text_top ui ~size (y -. 7.) 14.)
      ~size ~color:snapshot.theme.foreground letter
  in
  Ui.draw_over ui canvas (fun paint (cx, cy, cw, ch) ->
      let refused = Pxui.Theme.invalid in
      Option.iter
        (fun ok ->
          Ui.Paint.dashed_rect paint ~x:(cx +. 1.5) ~y:(cy +. 1.5) ~w:(cw -. 3.) ~h:(ch -. 3.)
            (if ok then snapshot.theme.accent else refused))
        canvas_hot;
      List.iter
        (fun ((x, y, w, h), letter, hot) ->
          let color = match hot with Some false -> refused | _ -> snapshot.theme.accent in
          (* a drop target: a dashed accent edge on the card *)
          if hot <> None then
            Ui.Paint.dashed_rect paint ~x:(x +. 0.5) ~y:(y +. 0.5) ~w:(w -. 1.) ~h:(h -. 1.) color
          else Ui.Paint.stroke paint ~x:(x +. 0.5) ~y:(y +. 0.5) ~w:(w -. 1.) ~h:(h -. 1.) color;
          (* a letter of the key route: the hint colour under ink, at the card's top left (-7, -7) *)
          Option.iter
            (fun letter ->
              hint_chip paint
                ~bg:(if hot = Some false then refused else (Pxui.Theme.ports snapshot.theme).hint)
                (x, y) letter)
            letter)
        marks;
      (* letter hints: a chip per target still matching what was typed, the rest of its letters shown *)
      Option.iter
        (fun (h : hinting) ->
          List.iter
            (fun (tg : hint_target) ->
              if String.starts_with ~prefix:h.typed tg.label then begin
                let rest =
                  String.sub tg.label (String.length h.typed)
                    (String.length tg.label - String.length h.typed)
                in
                hint_chip paint ~bg:(Pxui.Theme.ports snapshot.theme).hint
                  (sx snapshot (fst tg.pos), sy snapshot (snd tg.pos))
                  rest
              end)
            h.targets)
        snapshot.hinting;
      Option.iter
        (fun (fx, fy) ->
          let size = max 4 (Ui.font_size ui - 2) in
          let label = "t  live: reads time or input, re-cooked every frame" in
          let tw = Ui.Paint.text_width paint ~size label +. 14. in
          frame_in paint ~x:fx ~y:fy ~w:tw ~h:20. ~fill:snapshot.theme.input
            (Pxui.Theme.edge snapshot.theme);
          Ui.Paint.text paint
            ~at:(fx +. 7., Ui.text_top ui ~size fy 20.)
            ~size ~color:snapshot.theme.foreground label)
        live_tip;
      (* the displayed node's tip, once the pointer has rested on its flag *)
      if tip_open then
        Option.iter
          (fun (fx, fy) ->
            let size = max 4 (Ui.font_size ui - 2) in
            let label = "displayed" and key = "V" in
            let lw = Ui.Paint.text_width paint ~size label
            and kw = Ui.Paint.text_width paint ~size key in
            let tw = 2. +. 12. +. lw +. 6. +. kw in
            frame_in paint ~x:fx ~y:fy ~w:tw ~h:20. ~fill:snapshot.theme.input
              (Pxui.Theme.edge snapshot.theme);
            let ty = Ui.text_top ui ~size fy 20. in
            Ui.Paint.text paint ~at:(fx +. 7., ty) ~size ~color:snapshot.theme.foreground label;
            Ui.Paint.text paint
              ~at:(fx +. 7. +. lw +. 6., ty)
              ~size ~color:(Pxui.Theme.ink_3 snapshot.theme) key)
          tip_at;
      List.iter
        (fun w ->
          paint_polyline ~clip:(cx, cy, cw, ch) paint ~width:2. snapshot.theme.foreground
            (List.map (fun (x, y) -> (sx snapshot x, sy snapshot y)) w.pts);
          List.iter
            (fun (x, y) ->
              Ui.Paint.circle paint
                ~at:(sx snapshot x, sy snapshot y)
                ~radius:6. ~stroke:snapshot.theme.foreground ())
            [ w.a; w.b ])
        snapshot.highlighted;
      (match band with
      | Some w ->
          Option.iter
            (fun (a, b) -> Ui.Paint.line paint ~from_:a ~to_:b ~width:1.5 (ty_color snapshot w.ty))
            (clip_segment w.from mouse (cx, cy, cw, ch))
      | None -> ());
      (match snapshot.drag with
      | Some (Marquee _) ->
          let x0, y0 = canvas_signal.press_point and mx, my = mouse in
          frame_in paint ~x:(Float.min x0 mx) ~y:(Float.min y0 my)
            ~w:(abs_float (mx -. x0))
            ~h:(abs_float (my -. y0))
            ~fill:(Pxui.Theme.tint snapshot.theme) snapshot.theme.accent
      | _ -> ());
      match hover with
      | Some (n, ax, ay, i) -> (
          let r = List.nth n.rows i in
          match r.chip with
          | P.Inline { glyph; text } ->
              let size = max 4 (fs - 2) in
              let full = shown_glyph glyph ^ " " ^ text in
              let w = Ui.Paint.text_width paint ~size full +. (12. *. z) in
              let k =
                match item_at snapshot n.path with
                | Some (q, _, _) ->
                    Option.value ~default:0 (Hashtbl.find_opt (line_of_row q.lines) i)
                | None -> 0
              in
              let x = sx snapshot (ax +. value_x P.node_width)
              and y = sy snapshot (rows_top n ay +. (float k *. P.row_height)) in
              if w > field_w *. z then begin
                frame_in paint ~x
                  ~y:(y +. (2. *. z))
                  ~w ~h:(20. *. z) ~fill:snapshot.theme.input (Pxui.Theme.edge snapshot.theme);
                Ui.Paint.text paint
                  ~at:(x +. (6. *. z), Ui.text_top ui ~size (y +. (2. *. z)) (20. *. z))
                  ~size ~color:snapshot.theme.foreground full
              end
          | _ -> ())
      | None -> ());
  (drawn_items, drawn_zones)

(* a gesture step returns the changes it asks for with the state it leaves *)
let collect f =
  let acc = ref [] in
  let t = f (fun c -> acc := c :: !acc) in
  (t, List.rev !acc)

let context_step (t : t) ui ~tiles ~(canvas_signal : Ui.signal) =
  collect @@ fun emit ->
  (* the context menu *)
  let t =
    match t.context with
    | Some _ -> t
    | None -> (
        match
          List.find_map
            (fun ((p : P.placed), _, _, _, (s : Ui.signal), _, _) ->
              if Ui.context_clicked s then Some (s.release_point, p.path) else None)
            tiles
        with
        | Some ((x, y), path) ->
            (* the menu acts on the nodes: a wire selected before is no longer the selection *)
            let selected =
              if Path_set.mem path t.selected then t.selected else Path_set.singleton path
            in
            if not (Path_set.equal selected t.selected) then
              emit (Selected (Path_set.elements selected));
            { t with context = Some ((x, y), path); selected; selected_wire = None }
        | None -> t)
  in
  (if t.context = None && Ui.context_clicked canvas_signal then
     let x, y = canvas_signal.release_point in
     emit (Menu_requested (x, y)));
  let t =
    match t.context with
    | None -> t
    | Some (at, path) -> (
        match Ui.context_menu ui ~at "pxui-scope-context" (context_items t path) with
        | `Open -> t
        | `Dismiss -> { t with context = None }
        | `Pick i ->
            (match t.imported path with
            | Some file -> emit (Open_import file)
            | None -> List.iter emit (action_changes t (context_command i)));
            { t with context = None })
  in
  t

let tile_gestures (t : t) ui ~z ~finish_wire ~tiles =
  let lens_next = ref t.lens in
  let t, changes =
    collect @@ fun emit ->
    let left = left_button in
    (* tile presses select and start a move; sockets start a wire *)
    let t =
      List.fold_left
        (fun t ((p : P.placed), _, _, tile, (s : Ui.signal), (sub : _), fields) ->
          let outs, dels, taps, _ = sub in
          List.iter emit fields;
          (* a click on a read name or an expression chip selects what it names *)
          let t =
            match List.find_map (function Selected paths -> Some paths | _ -> None) fields with
            | Some paths -> select paths t
            | None -> t
          in
          let t =
            if s.pressed && left s && t.context = None then begin
              let additive = List.mem Input.Shift (Ui.press_keys ui tile) in
              let selected =
                if additive then
                  if Path_set.mem p.path t.selected then Path_set.remove p.path t.selected
                  else Path_set.add p.path t.selected
                else if
                  Path_set.mem p.path t.selected
                  && Path_set.for_all (Paths.mem t.geo.slot) t.selected
                then t.selected
                else Path_set.singleton p.path
              in
              emit (Selected (Path_set.elements selected));
              {
                t with
                selected;
                selected_wire = None;
                drag =
                  Some
                    (Moving
                       {
                         paths = parents_removed (Path_set.elements selected);
                         dx = 0.;
                         dy = 0.;
                         moved = false;
                       });
              }
            end
            else t
          in
          let t =
            if s.double_clicked && left s && t.context = None && t.editing = None then begin
              let _, top, _, _ = Ui.rect ui tile in
              match p.item with
              | P.Item n when (not n.synthetic) && snd s.press_point < top +. (P.head_height *. z)
                ->
                  { t with editing = Some (Name p.path) }
              (* a folded function or loop opens in place; anything else follows what it references *)
              | P.Item n when n.zone <> None && t.collapsed p.path ->
                  emit (Zone_collapsed { zone = p.path; collapsed = false });
                  t
              | P.Item n when not n.synthetic ->
                  emit (Activated p.path);
                  t
              | P.Input _ -> { t with editing = Some (Default p.path) }
              | _ -> t
            end
            else t
          in
          let t =
            match t.drag with
            | Some (Moving m) when s.held && left s && Path_set.mem p.path t.selected ->
                let ddx, ddy = s.drag in
                let dx = m.dx +. (ddx /. z) and dy = m.dy +. (ddy /. z) in
                {
                  t with
                  drag =
                    Some (Moving { m with dx; dy; moved = m.moved || Float.hypot dx dy > 3. /. z });
                }
            | _ -> t
          in
          let t =
            match t.drag with
            | Some (Moving m) when s.released && left s ->
                if m.moved then
                  emit
                    (Moved
                       (List.filter_map
                          (fun path ->
                            match rel_at t path with
                            | Some (rx, ry) -> Some (path, P.snap (rx +. m.dx), P.snap (ry +. m.dy))
                            | None -> None)
                          m.paths));
                { t with drag = None }
            | _ -> t
          in
          (* zone controls *)
          let t =
            List.fold_left
              (fun t (b, on) ->
                let bs = Ui.signal ui b in
                (match (p.item, on) with
                | P.Item n, `Toggle when bs.clicked ->
                    emit (Zone_collapsed { zone = n.path; collapsed = not p.collapsed })
                | P.Item n, `Replace when bs.clicked ->
                    emit (Syntax_edit (E.Inline_macro { node = n.path }))
                | P.Item n, `Lens when bs.clicked ->
                    lens_next :=
                      if List.mem_assoc n.path !lens_next then List.remove_assoc n.path !lens_next
                      else
                        (n.path, match n.lens with Some l -> Array.length l.steps - 1 | None -> 0)
                        :: !lens_next
                | P.Item n, `Lens_step i when bs.clicked ->
                    lens_next := (n.path, i) :: List.remove_assoc n.path !lens_next
                | P.Item n, `Step d when bs.clicked ->
                    let count = count_of t n.path in
                    let index = max 0 (min (count - 1) (t.probe n.path + d)) in
                    if count > 0 && index <> t.probe n.path then
                      emit (Probe_set { zone = n.path; index })
                | P.Item n, `Track when bs.pressed || bs.held ->
                    let count = count_of t n.path in
                    let x, _, w, _ = Ui.rect ui b in
                    if count > 0 && w > 0. then begin
                      let px = fst (if bs.released then bs.release_point else bs.pointer) in
                      let index =
                        max 0 (min (count - 1) (int_of_float ((px -. x) /. w *. float count)))
                      in
                      if index <> t.probe n.path then emit (Probe_set { zone = n.path; index })
                    end
                | _ -> ());
                t)
              t taps
          in
          List.iter
            (fun (b, ops) ->
              if (Ui.signal ui b).clicked then List.iter (fun op -> emit (Syntax_edit op)) (ops ()))
            dels;
          (* wires *)
          let t =
            List.fold_left
              (fun t (b, src, ty) ->
                let bs = Ui.signal ui b in
                let t =
                  if bs.pressed && left bs then
                    let iter =
                      match t.scope with
                      | Some scope ->
                          List.exists
                            (fun (n : P.node) ->
                              match n.zone with
                              | Some zn ->
                                  List.exists
                                    (fun (r : P.rail_row) -> r.role = P.Var && List.mem src r.names)
                                    zn.rail
                              | None -> false)
                            (P.zones scope)
                      | None -> false
                    in
                    (* Alt on an out-port lifts every wire it feeds *)
                    let lift =
                      if not (List.mem Input.Alt (Ui.press_keys ui b)) then []
                      else
                        Array.fold_left
                          (fun acc (w : wire) ->
                            match (w.from, w.target) with
                            | Some f, Some (path, key, _) when f = p.path -> (path, key) :: acc
                            | _ -> acc)
                          [] t.geo.wires
                    in
                    {
                      t with
                      drag =
                        Some
                          (Wiring
                             {
                               src;
                               ty;
                               iter;
                               from =
                                 (let x, y, w, h = Ui.rect ui b in
                                  (x +. (w /. 2.), y +. (h /. 2.)));
                               lift;
                             });
                    }
                  else t
                in
                if bs.released && left bs then begin
                  match t.drag with
                  | Some (Wiring w) ->
                      finish_wire ~emit t w ~src_path:p.path ~release_point:bs.release_point
                  | _ -> { t with drag = None }
                end
                else t)
              t outs
          in
          t)
        t tiles
    in
    t
  in
  (t, changes, !lens_next)

let wire_pull (t : t) ui ~finish_wire ~hits ~wires_in_view =
  collect @@ fun emit ->
  let left = left_button in
  (* a wire pulled past 6 points is a new wire from its source: the same drag as from the out-port *)
  let t =
    if Option.is_some t.drag || t.context <> None then t
    else
      List.find_map
        (fun i ->
          let w = t.geo.wires.(i) in
          match (w.target, w.from) with
          | Some _, Some from ->
              List.find_map
                (fun b ->
                  let s = Ui.signal ui b in
                  let px, py = s.pointer and qx, qy = s.press_point in
                  if s.held && left s && Float.hypot (px -. qx) (py -. qy) > 6. then
                    Some
                      ( w,
                        from,
                        if List.mem Input.Alt (Ui.press_keys ui b) then
                          Option.to_list (Option.map (fun (p, k, _) -> (p, k)) w.target)
                        else [] )
                  else None)
                (hits i)
          | _ -> None)
        wires_in_view
      |> function
      | Some (w, from, lift) -> (
          match Option.map (fun ((p : P.placed), _, _) -> p.item) (item_at t from) with
          | Some (P.Item ({ zone = None; _ } as n)) when n.binds = [ n.name ] ->
              {
                t with
                drag =
                  Some
                    (Wiring
                       {
                         src = n.name;
                         ty = w.ty;
                         iter = false;
                         from = (sx t (fst w.a), sy t (snd w.a));
                         lift;
                       });
              }
          | Some (P.Input i) ->
              {
                t with
                drag =
                  Some
                    (Wiring
                       {
                         src = i.name;
                         ty = w.ty;
                         iter = false;
                         from = (sx t (fst w.a), sy t (snd w.a));
                         lift;
                       });
              }
          | _ -> t)
      | None -> t
  in
  (* ...and let go, it finishes like an out-port's wire *)
  let t =
    match (t.drag, t.selected_wire) with
    | Some (Wiring w), Some _
      when List.exists
             (fun i ->
               List.exists
                 (fun b ->
                   let s = Ui.signal ui b in
                   s.released && left s)
                 (hits i))
             wires_in_view -> (
        let from_wire =
          List.find_map
            (fun i ->
              List.find_map
                (fun b ->
                  let s = Ui.signal ui b in
                  if s.released && left s then Some (t.geo.wires.(i), s.release_point) else None)
                (hits i))
            wires_in_view
        in
        match from_wire with
        | Some (wire, release_point) ->
            finish_wire ~emit t w ~src_path:(Option.value ~default:[] wire.from) ~release_point
        | None -> t)
    | _ -> t
  in
  t

(* a wire let go: on an input row it connects; on empty canvas it opens the add menu after its
   source (pulled off an existing wire, the host inserts the new node on the selected wire) *)
let finish_wire ui ~z ~mouse ~emit ~tiles (t : t) (w : wiring) ~src_path ~release_point =
  let target =
    List.find_map
      (fun ((tp : P.placed), tax, tay, ttile, _, _, _) ->
        if not (Ui.hovered_within ui ttile) then None
        else
          let mx, my = mouse in
          let gx = (mx -. sx t 0.) /. z and gy = (my -. sy t 0.) /. z in
          let lx = gx -. tax and ly = gy -. tay in
          match tp.item with
          | P.Return -> Option.map (fun (s : P.scope) -> (s.path @ [ "@result" ], E.Whole)) t.scope
          | Input _ -> None
          | Item n -> (
              match n.zone with
              | None -> (
                  (* anywhere on the header is the in-port of the first geometry slot *)
                  match row_at ~header:infinity tp n ~shown:tp.shown ~px:lx ~py:ly with
                  | Some i when i < List.length n.rows ->
                      let r = List.nth n.rows i in
                      if r.socket then Some (n.path, r.key) else None
                  | _ -> None)
              | Some zn when tp.collapsed -> (
                  let i = int_of_float ((ly -. P.head_height) /. P.row_height) in
                  match List.nth_opt zn.rail i with
                  | Some { key = Some key; _ } when ly >= P.head_height -> Some (n.path, key)
                  | _ -> None)
              | Some zn ->
                  let extras = P.extra_rails zn in
                  if ly < P.head_height then
                    match P.label_row zn with
                    | Some { key = Some key; _ } when lx < 24. -> Some (n.path, key)
                    | _ -> None
                  else if ly < P.rail_top n -. 28. then
                    match
                      List.nth_opt extras (int_of_float ((ly -. P.head_height) /. P.row_height))
                    with
                    | Some { key = Some key; _ } when lx < 24. -> Some (n.path, key)
                    | _ -> None
                  else if lx > tp.w -. 24. && ly >= P.rail_top n then
                    Some (zn.scope.path @ [ "@result" ], E.Whole)
                  else None))
      tiles
  in
  (* a wire let go on empty canvas: the add menu there, after the node it came from *)
  let on_canvas =
    target = None
    && List.nth_opt (List.rev src_path) 0 = Some w.src
    && not (List.exists (fun (_, _, _, ttile, _, _, _) -> Ui.hovered_within ui ttile) tiles)
  in
  (* an Alt-drag moves the wire: it comes off the inputs it was lifted from *)
  let off = unwire_at t (List.filter (fun i -> Some i <> target) w.lift) in
  (match target with
  | Some (node, key) ->
      emit (Syntax_edit (E.Connect { node; key; src = w.src; iter = w.iter }));
      List.iter (fun op -> emit (Syntax_edit op)) off
  | None
    when w.lift <> []
         && not (List.exists (fun (_, _, _, ttile, _, _, _) -> Ui.hovered_within ui ttile) tiles) ->
      List.iter (fun op -> emit (Syntax_edit op)) off
  | None when on_canvas ->
      emit (Selected [ src_path ]);
      let x, y = release_point in
      emit (Menu_requested (x, y))
  | None -> emit (Notice "Drop the wire on an input row"));
  { t with drag = None; selected = (if on_canvas then Path_set.singleton src_path else t.selected) }

(* the pointer: pan, a press on a wire or the canvas, the rubber band *)
let pointer_step (t : t) ui (frame : Frame.t) ~canvas ~(canvas_signal : Ui.signal) ~tiles ~hits
    ~wires_in_view =
  collect @@ fun emit ->
  (* interactions *)
  let left (s : Ui.signal) = s.button = Some Input.LeftButton in
  let non_left_pressed (s : Ui.signal) =
    s.pressed && s.button <> Some Input.LeftButton && s.button <> None
  in
  let non_left_held (s : Ui.signal) =
    s.held && s.button <> Some Input.LeftButton && s.button <> None
  in
  let non_left_released (s : Ui.signal) =
    s.released && s.button <> Some Input.LeftButton && s.button <> None
  in
  let all_signals =
    canvas_signal :: List.map (fun (_, _, _, _, (s : Ui.signal), _, _) -> s) tiles
  in
  let panning =
    List.exists (fun (s : Ui.signal) -> non_left_held s || non_left_released s) all_signals
  in
  let pan_pressed = List.exists non_left_pressed all_signals in
  let pan_released = List.exists non_left_released all_signals in
  let lost = List.exists (function Input.Focus_lost -> true | _ -> false) frame.input.events in
  let panning_grab =
    if pan_pressed || panning then true
    else if pan_released || lost || frame.input.buttons = [] then false
    else t.panning_grab
  in
  if panning_grab <> t.panning_grab then ignore (Rays.Sketch.set_relative_mouse panning_grab);
  let t = { t with panning_grab } in
  let t =
    if not panning then t
    else begin
      let dx, dy =
        if canvas_signal.held || canvas_signal.released then canvas_signal.drag
        else
          List.fold_left
            (fun (dx, dy) (_, _, _, _, (s : Ui.signal), _, _) ->
              let a, b = s.drag in
              (dx +. a, dy +. b))
            (0., 0.) tiles
      in
      { t with pan_x = t.pan_x +. dx; pan_y = t.pan_y +. dy }
    end
  in
  let tile_pressed = List.exists (fun (_, _, _, _, (s : Ui.signal), _, _) -> s.pressed) tiles in
  let wire_pressed =
    if t.context <> None || tile_pressed || canvas_signal.subtree_press = None then None
    else
      List.find_map
        (fun i ->
          if
            List.exists
              (fun b ->
                let s = Ui.signal ui b in
                s.pressed && left s)
              (hits i)
          then Some i
          else None)
        wires_in_view
  in
  let t =
    match wire_pressed with
    | Some i ->
        if not (Path_set.is_empty t.selected) then emit (Selected []);
        { t with selected = Path_set.empty; selected_wire = t.geo.wires.(i).target }
    | None -> t
  in
  let t =
    if
      canvas_signal.pressed && left canvas_signal && t.context = None && (not tile_pressed)
      && Option.is_none wire_pressed
    then begin
      let additive = List.mem Input.Shift (Ui.press_keys ui canvas) in
      if (not additive) && not (Path_set.is_empty t.selected) then emit (Selected []);
      let base = if additive then t.selected else Path_set.empty in
      { t with selected = base; selected_wire = None; drag = Some (Marquee { base }) }
    end
    else t
  in
  (* the rubber band selects the nodes of one scope it touches *)
  let t =
    match t.drag with
    | Some (Marquee m) when canvas_signal.held || canvas_signal.released ->
        let px, py =
          if canvas_signal.released then canvas_signal.release_point else canvas_signal.pointer
        in
        let x0, y0 = canvas_signal.press_point in
        let rw = abs_float (px -. x0) and rh = abs_float (py -. y0) in
        let selected =
          if rw < 3. && rh < 3. then m.base
          else Path_set.union m.base (marquee_hits t (Float.min x0 px, Float.min y0 py, rw, rh))
        in
        if not (Path_set.equal selected t.selected) then
          emit (Selected (Path_set.elements selected));
        { t with selected; drag = (if canvas_signal.released then None else t.drag) }
    | _ -> t
  in
  t

let hover_step (t : t) ui ~z ~mouse ~tiles ~hits ~wires_in_view =
  (* hovered row and chip expansion *)
  let hover =
    List.find_map
      (fun ((p : P.placed), ax, ay, tile, _, _, _) ->
        match p.item with
        | P.Item ({ zone = None; _ } as n) when Ui.hovered_within ui tile -> (
            let px = (fst mouse -. sx t ax) /. z and py = (snd mouse -. sy t ay) /. z in
            match row_at p n ~shown:p.shown ~px ~py with
            | Some i when i < List.length n.rows -> Some (n, ax, ay, i)
            | _ -> None)
        | _ -> None)
      tiles
  in
  let hovered_row =
    Option.map (fun ((n : P.node), _, _, i) -> (n.path, (List.nth n.rows i).P.key)) hover
  in
  let port_centre box =
    if Ui.hovered_within ui box then
      let x, y, w, h = Ui.rect ui box in
      Some (x +. (w /. 2.), y +. (h /. 2.))
    else None
  in
  let hovered_port =
    List.find_map
      (fun (_, _, _, _, _, (outs, _, _, inputs), _) ->
        match List.find_map (fun (box, _, _) -> port_centre box) outs with
        | Some _ as port -> port
        | None -> List.find_map port_centre inputs)
      tiles
  in
  let same_port point =
    match hovered_port with
    | None -> false
    | Some (x, y) ->
        abs_float (sx t (fst point) -. x) <= (1.5 *. z) +. 0.5
        && abs_float (sy t (snd point) -. y) <= (1.5 *. z) +. 0.5
  in
  let highlighted = ref [] in
  List.iter
    (fun i ->
      let w = t.geo.wires.(i) in
      if
        t.context = None
        && (List.exists (Ui.hovered_within ui) (hits i) || same_port w.a || same_port w.b)
      then highlighted := w :: !highlighted)
    wires_in_view;
  let t = { t with hovered_row; highlighted = !highlighted } in
  (t, hover)

let frames_step (t : t) ui ~z ~edit_field ~canvas =
  collect @@ fun emit ->
  let local (x, y) = (x -. float t.x, y -. float t.y) in
  (* frames: a title strip, a delete cross and a resize corner, over the tiles (a zone's tile
     covers its whole body) and clear of the nodes, which keep 12 points inside the frame *)
  let frame_boxes =
    Ui.within ui canvas (fun () ->
        List.concat_map
          (fun (scope, (ox, oy)) ->
            List.mapi
              (fun i ((title, (fx, fy), (fw, fh)) : fr) ->
                let x = sx t (ox +. fx) and y = sy t (oy +. fy) in
                let w = fw *. z and h = fh *. z in
                let box k (bx, by) (bw, bh) =
                  Ui.box ui ~flags:Ui.clickable ~w:(Ui.Px bw) ~h:(Ui.Px bh)
                    ~at:(local (bx, by))
                    (Printf.sprintf "fr:%s:%d:%s" (String.concat "/" scope) i k)
                in
                ( (scope, i, title, (x, y, w, h)),
                  ( box "t" (x, y) (Float.max 8. (w -. 20.), 18. *. z),
                    box "x" (x +. w -. 18., y +. 2.) (16., 16.),
                    box "r" (x +. w -. 12., y +. h -. 12.) (12., 12.) ) ))
              (t.frames scope))
          t.geo.origins)
  in
  let t =
    List.fold_left
      (fun t ((scope, i, title, (x, y, w, _)), (tb, xb, rb)) ->
        let ts = Ui.signal ui tb and xs = Ui.signal ui xb and rs = Ui.signal ui rb in
        let all = t.frames scope in
        (match t.editing with
        | Some (Title (sp, k)) when sp = scope && k = i ->
            List.iter emit
              (Ui.within ui canvas (fun () ->
                   edit_field
                     ~at:(local (x +. 4., y +. 2.))
                     ~w:(Float.max 40. (w -. 24.))
                     ~h:(16. *. z) (Printf.sprintf "frt%d" i) title
                     (fun s -> s <> "" && not (String.contains s '"'))
                     (fun v ->
                       Frames_set
                         {
                           scope;
                           frames =
                             List.mapi
                               (fun j ((_, at, size) as f : fr) ->
                                 if j = i then (v, at, size) else f)
                               all;
                         })))
        | _ -> ());
        if xs.clicked then begin
          emit (Frames_set { scope; frames = List.filteri (fun j _ -> j <> i) all });
          t
        end
        else if ts.double_clicked && t.editing = None then
          { t with drag = None; editing = Some (Title (scope, i)) }
        else if rs.pressed && left_button rs then
          { t with drag = Some (Sizing { scope; index = i; dw = 0.; dh = 0. }) }
        else
          match t.drag with
          | None
            when ts.held && left_button ts && t.editing = None
                 && Float.hypot
                      (fst ts.pointer -. fst ts.press_point)
                      (snd ts.pointer -. snd ts.press_point)
                    > 3. ->
              (* a drag by the title (a click or a double-click stays one): the nodes of the scope
             whose centres lie inside the frame travel with it *)
              let ox, oy = Option.value ~default:(0., 0.) (List.assoc_opt scope t.geo.origins) in
              let _, (fx, fy), (fw, fh) = List.nth (t.frames scope) i in
              let inside path =
                scope_of_path path = scope
                &&
                match box_at t path with
                | Some (bx, by, bw, bh) ->
                    let cx = bx +. (bw /. 2.) and cy = by +. (bh /. 2.) in
                    cx >= ox +. fx && cx <= ox +. fx +. fw && cy >= oy +. fy && cy <= oy +. fy +. fh
                | None -> false
              in
              let paths =
                Array.fold_left
                  (fun found ((p : P.placed), _, _) ->
                    if inside p.path then p.path :: found else found)
                  [] t.geo.items
              in
              let dx = (fst ts.pointer -. fst ts.press_point) /. z
              and dy = (snd ts.pointer -. snd ts.press_point) /. z in
              { t with drag = Some (Carrying { scope; index = i; paths; dx; dy }) }
          | Some (Carrying c) when c.scope = scope && c.index = i && (ts.held || ts.released) ->
              let ddx, ddy = ts.drag in
              let t =
                {
                  t with
                  drag = Some (Carrying { c with dx = c.dx +. (ddx /. z); dy = c.dy +. (ddy /. z) });
                }
              in
              if ts.released then begin
                (match t.drag with
                | Some (Carrying c) when Float.hypot c.dx c.dy > 3. /. z ->
                    emit
                      (Moved
                         (List.filter_map
                            (fun path ->
                              match rel_at t path with
                              | Some (rx, ry) ->
                                  Some (path, P.snap (rx +. c.dx), P.snap (ry +. c.dy))
                              | None -> None)
                            c.paths));
                    emit (Frames_set { scope; frames = frame_list t scope })
                | _ -> ());
                { t with drag = None }
              end
              else t
          | Some (Sizing s) when s.scope = scope && s.index = i && (rs.held || rs.released) ->
              let dx, dy = rs.drag in
              let t =
                {
                  t with
                  drag =
                    Some
                      (Sizing { scope; index = i; dw = s.dw +. (dx /. z); dh = s.dh +. (dy /. z) });
                }
              in
              if rs.released then begin
                emit (Frames_set { scope; frames = frame_list t scope });
                { t with drag = None }
              end
              else t
          | _ -> t)
      t frame_boxes
  in
  t

(* a tile's ports and taps: out-ports, deletable inputs, zone controls *)
let tile_ports (t : t) ui ~z ~fs (p : P.placed) tile =
  let shown = p.shown in
  let sub =
    Ui.within ui tile (fun () ->
        let box ?(flags = Ui.clickable) name (dx, dy) (bw, bh) =
          Ui.box ui ~flags ~w:(Ui.Px (bw *. z)) ~h:(Ui.Px (bh *. z)) ~at:(dx *. z, dy *. z) name
        in
        (* dels hold thunks: unwire probes the last value, only worth it on a click *)
        let outs = ref [] and dels = ref [] and taps = ref [] and inputs = ref [] in
        let input name pos =
          let b = box name (fst pos -. 7., snd pos -. 7.) (14., 14.) in
          inputs := b :: !inputs;
          b
        in
        let out name src ty pos =
          outs := (box name (fst pos -. 7., snd pos -. 7.) (14., 14.), src, ty) :: !outs
        in
        let tap name pos size on = taps := (box name pos size, on) :: !taps in
        (match p.item with
        | P.Input i ->
            out "out" i.name (Some i.ty)
              (if shown = P.Point then (p.w, P.point_size /. 2.) else (p.w -. 1., port_y))
        | Return -> ignore (input "in" (0., P.body_top +. 12.))
        | Item n -> (
            let node_src = if n.binds = [ n.name ] then Some n.name else None in
            let plain_for =
              match n.zone with Some { kind = P.For; _ } -> not p.collapsed | _ -> false
            in
            Option.iter
              (fun s ->
                if not plain_for then
                  out "out" s (Some n.ty)
                    (if shown = P.Point then (p.w, P.point_size /. 2.) else (p.w -. 1., port_y)))
              node_src;
            (* the header's in-port: the first geometry slot *)
            (match List.find_opt (fun (r : P.row) -> r.head) n.rows with
            | Some r when n.zone = None ->
                let sink =
                  input "d:head"
                    (if shown = P.Point then (0., P.point_size /. 2.) else (1., port_y))
                in
                if wired r then dels := (sink, fun () -> unwire t n r) :: !dels
            | _ -> ());
            match n.zone with
            | Some zn when not p.collapsed ->
                (* the label row's variable: the collection's in-port at the edge, the variable's
                 out-port in the left strip once something reads it *)
                Option.iter
                  (fun (r : P.rail_row) ->
                    if r.key <> None && zn.kind <> P.For then begin
                      let sink = input "rd:label" (1., port_y) in
                      match (r.key, r.expr) with
                      | Some key, Some e when E.free_names e <> [] ->
                          dels :=
                            ( sink,
                              fun () -> [ E.Disconnect { node = n.path; key; fallback = None } ] )
                            :: !dels
                      | _ -> ()
                    end;
                    if zone_uses zn r.names then
                      out "rail:label" (List.hd r.names) r.ty (begin_port n zn r))
                  (P.label_row zn);
                List.iteri
                  (fun i (r : P.rail_row) ->
                    let cy = P.head_height +. (float i *. P.row_height) +. 12. in
                    if r.role <> P.Capture then
                      out ("rail:" ^ string_of_int i) (List.hd r.names) r.ty (begin_port n zn r);
                    (* wired rail rows can be disconnected from their socket *)
                    let sink =
                      if r.key <> None then Some (input ("rd:" ^ string_of_int i) (0., cy))
                      else None
                    in
                    match (r.key, r.expr, sink) with
                    | Some key, Some e, Some sink when E.free_names e <> [] ->
                        dels :=
                          (sink, fun () -> [ E.Disconnect { node = n.path; key; fallback = None } ])
                          :: !dels
                    | _ -> ())
                  (P.extra_rails zn);
                if zn.kind <> P.Let && zn.kind <> P.State && zn.kind <> P.Branch then begin
                  (* the selector of the label row: a button each side of the reading *)
                  let sx, sy, sw, sh, _ =
                    selector_geo ~measure:t.measure ~z ~fs ~w:(p.w *. z) ~count:(count_of t n.path)
                      ~probe:(t.probe n.path)
                  in
                  let sx = sx /. z and sy = sy /. z and sw = sw /. z and sh = sh /. z in
                  tap "prev" (sx -. 4., sy) (16., sh) (`Step (-1));
                  tap "next" (sx +. sw -. 12., sy) (16., sh) (`Step 1);
                  tap "track" (sx +. 12., sy) (sw -. 24., sh) `Track
                end;
                if zn.kind <> P.Let then tap "toggle" (0., 0.) (64., P.head_height) `Toggle
            | Some zn when p.collapsed ->
                List.iteri
                  (fun i (r : P.rail_row) ->
                    if r.key <> None || r.role = P.Capture then
                      ignore
                        (input
                           ("rd:" ^ string_of_int i)
                           (0., P.body_top +. ((float i +. 0.5) *. P.row_height))))
                  zn.rail;
                if zn.kind <> P.Let then tap "toggle" (head_pad, 0.) (64., P.head_height) `Toggle
            | Some _ -> ()
            | None ->
                (match n.lens with
                | Some l when shown <> P.Point && shown <> P.Chip -> (
                    tap "lens" (p.w -. 22., 2.) (20., 20.) `Lens;
                    match lens_of t n.path with
                    | Some step ->
                        let lh = P.lens_height l ~step in
                        let top = lens_top p.h lh in
                        let len = Array.length l.steps in
                        for i = 0 to len do
                          let (bx, by), size = lens_button_box ~len i in
                          tap ("ls" ^ string_of_int i) (bx, top +. by) size (`Lens_step i)
                        done;
                        let (rx, ry), size = lens_replace_box lh in
                        tap "lr" (rx, top +. ry) size `Replace
                    | None -> ())
                | _ -> ());
                if shown = P.Card || shown = P.Full then begin
                  let top = rows_top n 0. in
                  let nlines = Array.length p.lines in
                  List.iteri
                    (fun j (f, ty) ->
                      let src = if n.binds = [ n.name ] then n.name ^ "." ^ f else f in
                      out
                        ("o:" ^ string_of_int j)
                        src (Some ty)
                        (p.w, top +. ((float (nlines + j) +. 0.5) *. P.row_height)))
                    n.outputs;
                  Array.iteri
                    (fun k -> function
                      | P.Row (i, r) ->
                          let ry = top +. (float k *. P.row_height) in
                          if r.socket then begin
                            let sink = input ("d:" ^ string_of_int i) (0., ry +. 12.) in
                            if wired r then dels := (sink, fun () -> unwire t n r) :: !dels
                          end
                      | P.Folder _ -> ())
                    p.lines
                end));
        (List.rev !outs, List.rev !dels, List.rev !taps, !inputs))
  in
  sub

(* the live editors of a row: a number, a vector of numbers, a flag *)
let row_fields_of ui ~z (p : P.placed) (n : P.node) (r : P.row) ~name ~ry ~size =
  let at = value_x p.w *. z and vw = field_w *. z and h = field_h *. z in
  let ay = (ry +. 4.) *. z in
  let nonfinite = match r.key with E.Kw _ -> false | _ -> true in
  match (r.chip, r.expr) with
  | P.Const, Some { node = S.Num text; _ } -> (
      match
        num_field ui ~at:(at, ay) ~w:vw ~h ~size ?kind:(kind_of r) ?range:(range_of r) ~nonfinite
          ("f" ^ name) text
      with
      | Some text' ->
          [
            Syntax_edit
              (E.Set_arg { node = n.path; key = r.key; sub = []; value = S.make (S.Num text') });
          ]
      | None -> [])
  | P.Const, Some { node = S.Vec elems; _ }
    when List.length elems >= 2 && List.length elems <= 4 && all_numbers elems ->
      let k = List.length elems in
      (* a vector is its fields 8 points apart, 112 wide, at the row's right *)
      let at = (p.w -. head_pad -. vec_w) *. z and gap = 8. *. z in
      let cw = ((vec_w *. z) -. (float (k - 1) *. gap)) /. float k in
      List.concat
        (List.mapi
           (fun c (e : S.t) ->
             match e.node with
             | S.Num text -> (
                 match
                   num_field ui
                     ~at:(at +. (float c *. (cw +. gap)), ay)
                     ~w:cw ~h ~size ?kind:(kind_of r) ~nonfinite (Printf.sprintf "f%s.%d" name c)
                     text
                 with
                 | Some text' ->
                     [
                       Syntax_edit
                         (E.Set_arg
                            {
                              node = n.path;
                              key = r.key;
                              sub = [ c ];
                              value = S.make (S.Num text');
                            });
                     ]
                 | None -> [])
             | _ -> [])
           elems)
  | P.Const, Some { node = S.Sym (("true" | "false") as b); _ } ->
      let toggle =
        Ui.box ui
          ~flags:Ui.(clickable + tab_stop)
          ~w:(Ui.Px (switch_w *. z))
          ~h:(Ui.Px (switch_h *. z))
          ~at:((p.w -. head_pad -. switch_w) *. z, (ry +. 5.) *. z)
          ("b" ^ name)
      in
      if (Ui.signal ui toggle).clicked then
        [
          Syntax_edit
            (E.Set_arg
               {
                 node = n.path;
                 key = r.key;
                 sub = [];
                 value = S.make (S.Sym (if b = "true" then "false" else "true"));
               });
        ]
      else []
  | _ -> []

(* a wire's hit boxes: its segments cut to [clip], as 16-point rectangles in the shared hit tree *)
let wire_boxes (t : t) ui ~wire_hits ?(origin = (0., 0.)) name (cx, cy, cw, ch) i =
  let hits i = Option.value ~default:[] (Cells.find_opt wire_hits i) in
  let w = t.geo.wires.(i) in
  let count = ref 0 in
  let prefix = name ^ ":" ^ string_of_int i ^ ":" in
  let rec segments j = function
    | (ax, ay) :: ((bx, by) :: _ as rest) ->
        (match
           clip_segment
             (sx t ax -. float t.x, sy t ay -. float t.y)
             (sx t bx -. float t.x, sy t by -. float t.y)
             (cx -. 4., cy -. 4., cw +. 8., ch +. 8.)
         with
        | None -> ()
        | Some ((ax, ay), (bx, by)) ->
            let steps =
              if ax = bx || ay = by then 1
              else max 1 (int_of_float (Float.hypot (bx -. ax) (by -. ay) /. 16.))
            in
            let prefix = prefix ^ string_of_int j ^ "-" in
            for s = 0 to steps - 1 do
              let u0 = float s /. float steps and u1 = float (s + 1) /. float steps in
              let x0 = ax +. ((bx -. ax) *. u0)
              and y0 = ay +. ((by -. ay) *. u0)
              and x1 = ax +. ((bx -. ax) *. u1)
              and y1 = ay +. ((by -. ay) *. u1) in
              incr count;
              Cells.replace wire_hits i
                (Ui.box ui ~flags:Ui.clickable
                   ~at:(Float.min x0 x1 -. 4. -. fst origin, Float.min y0 y1 -. 4. -. snd origin)
                   ~w:(Ui.Px (abs_float (x1 -. x0) +. 8.))
                   ~h:(Ui.Px (abs_float (y1 -. y0) +. 8.))
                   (prefix ^ string_of_int !count)
                :: hits i)
            done);
        segments (j + 1) rest
    | _ -> ()
  in
  if w.target <> None && not (tagged t w) then segments 0 w.pts

(* one visible node: its tile box, its ports and the live widgets of its rows *)
let build_tile (t : t) ui ~z ~fs
    ~(wire_boxes : ?origin:float * float -> string -> float * float * float * float -> int -> unit)
    ~edit_field ~finished ~opened ((p : P.placed), ax, ay) =
  let local (x, y) = (x -. float t.x, y -. float t.y) in
  let bx, by = (sx t ax, sy t ay) in
  let key = "t:" ^ String.concat "/" p.path in
  let w = p.w *. z and h = p.h *. z in
  let shown = p.shown in
  let tile = Ui.box ui ~flags:Ui.clickable ~w:(Ui.Px w) ~h:(Ui.Px h) ~at:(local (bx, by)) key in
  (* an expanded zone's tile covers its body: the wires that end inside it get their boxes again
         here, over the tile and under the cards built after it *)
  (match p.item with
  | P.Item { zone = Some _; _ } when not p.collapsed ->
      let lx, ly = local (bx, by) in
      let x0 = Float.max 0. lx and y0 = Float.max 0. ly in
      let clip =
        ( x0,
          y0,
          Float.min (float t.width) (lx +. w) -. x0,
          Float.min (float t.height) (ly +. h) -. y0 )
      in
      Ui.within ui tile (fun () ->
          List.iter
            (wire_boxes ~origin:(lx, ly) "zwire" clip)
            (Option.value ~default:[] (Hashtbl.find_opt t.geo.inside p.path)))
  | _ -> ());
  (* the live editors of a row: a number, a vector of numbers, a flag *)
  let row_fields = row_fields_of ui ~z p in
  let sub = tile_ports t ui ~z ~fs p tile in
  (* fields, inside the tile *)
  let fields =
    match p.item with
    | P.Item ({ zone = None; _ } as n) when z >= 0.5 && P.value_card n ->
        (* a literal binding: its value is the field of its header, at body size *)
        Ui.within ui tile (fun () ->
            List.concat_map (fun (r : P.row) -> row_fields n r ~name:"v" ~ry:0. ~size:fs) n.rows)
    | P.Item ({ zone = None; _ } as n) when z >= 0.5 && (shown = P.Card || shown = P.Full) ->
        Ui.within ui tile (fun () ->
            let top = rows_top n 0. in
            List.concat
              (List.mapi
                 (fun k line ->
                   match line with
                   | P.Row (i, r) ->
                       row_fields n r ~name:(string_of_int i)
                         ~ry:(top +. (float k *. P.row_height))
                         ~size:(max 4 (fs - 2))
                   | _ -> [])
                 (Array.to_list p.lines)))
    | P.Input ({ default = Some { node = S.Num text; _ }; _ } as i)
      when z >= 0.5 && t.editing <> Some (Default p.path) ->
        (* a value node: its number is a field in the header, at body size *)
        Ui.within ui tile (fun () ->
            match
              num_field ui
                ~at:(value_x p.w *. z, 4. *. z)
                ~w:(field_w *. z) ~h:(field_h *. z) ~size:fs "default" text
            with
            | Some text' ->
                [
                  Syntax_edit
                    (E.Set_input_default
                       { form = List.hd i.path; input = i.name; value = S.make (S.Num text') });
                ]
            | None -> [])
    | _ -> []
  in
  let add_clicks =
    match p.item with
    | P.Item ({ zone = None; _ } as n) when z >= 0.5 && (shown = P.Card || shown = P.Full) ->
        Ui.within ui tile (fun () ->
            let top = rows_top n 0. in
            List.concat
              (List.mapi
                 (fun k line ->
                   match line with
                   | P.Row (i, r)
                     when r.kind = P.Add || match r.chip with P.Inline _ -> true | _ -> false -> (
                       let b =
                         Ui.box ui
                           ~flags:Ui.(clickable + tab_stop)
                           ~w:
                             (Ui.Px
                                (if r.kind = P.Add then (p.w -. label_x -. head_pad) *. z
                                 else 24. *. z))
                           ~h:(Ui.Px (16. *. z))
                           ~at:
                             ( (if r.kind = P.Add then label_x else value_x p.w) *. z,
                               (top +. (float k *. P.row_height) +. 4.) *. z )
                           ("a" ^ string_of_int i)
                       in
                       (* a click on the expression's text (not its glyph, which unfolds) makes the row the
                   inspector's subject: the holder stays selected, the row is [holder @ [":" ^ label]] *)
                       let subject =
                         match r.chip with
                         | P.Inline _ ->
                             let cb =
                               Ui.box ui ~flags:Ui.clickable
                                 ~w:(Ui.Px ((field_w -. 24.) *. z))
                                 ~h:(Ui.Px (16. *. z))
                                 ~at:
                                   ( (value_x p.w +. 24.) *. z,
                                     (top +. (float k *. P.row_height) +. 4.) *. z )
                                 ("ex" ^ string_of_int i)
                             in
                             let s = Ui.signal ui cb in
                             (* a double click expands the call in place, as the glyph does *)
                             if s.double_clicked then unfold_row n r
                             else if s.clicked then
                               [ Selected [ n.path; n.path @ [ ":" ^ r.label ] ] ]
                             else []
                         | _ -> []
                       in
                       subject
                       @
                       if not (Ui.signal ui b).clicked then []
                       else
                         match (r.kind, r.chip) with
                         | P.Add, _ -> (
                             match r.key with
                             | E.Field _ ->
                                 (* the first [fN] the record does not have *)
                                 let rec free i =
                                   let name = "f" ^ string_of_int i in
                                   if List.exists (fun (r : P.row) -> r.key = E.Field name) n.rows
                                   then free (i + 1)
                                   else name
                                 in
                                 [
                                   Syntax_edit
                                     (E.Add_field
                                        { node = n.path; name = free 1; value = S.make (S.Num "0") });
                                 ]
                             | E.Kw "" ->
                                 opened := Some (Output n.path);
                                 [] (* a new output is named first *)
                             | _ when n.head = "list" || n.head = "str" ->
                                 [ Syntax_edit (E.Add_item { node = n.path }) ]
                             | key -> (
                                 (* a type with no literal (geometry, a list, a function, a scene) is wired *)
                                 match r.ty with
                                 | Some (Flow.Ty.Named "geometry") ->
                                     [ Notice "Wire a node onto this row" ]
                                 | Some ty -> (
                                     match E.default_for ty r.label with
                                     | Some value ->
                                         [
                                           Syntax_edit
                                             (E.Set_arg { node = n.path; key; sub = []; value });
                                         ]
                                     | None -> [ Notice "Wire a node onto this row" ])
                                 | None ->
                                     [
                                       Syntax_edit
                                         (E.Set_arg
                                            {
                                              node = n.path;
                                              key;
                                              sub = [];
                                              value = S.make (S.Num "0");
                                            });
                                     ]))
                         | _, P.Inline _ -> unfold_row n r
                         | _ -> [])
                   | P.Row (i, (({ chip = P.Name s; _ } : P.row) as r))
                     when Hashtbl.mem t.folds (n.path, i) ->
                       (* the fold button of a wired row, where [paint_value] draws its glyph *)
                       let left, fw, _, _, _ =
                         wired_geo ~measure:t.measure t ~z ~fs ~x:0. ~w:(p.w *. z) ~fold:true
                           (wired_name r s)
                       in
                       let b =
                         Ui.box ui
                           ~flags:Ui.(clickable + tab_stop)
                           ~w:(Ui.Px fw)
                           ~h:(Ui.Px (16. *. z))
                           ~at:
                             (left -. fw -. (2. *. z), (top +. (float k *. P.row_height) +. 4.) *. z)
                           ("fold" ^ string_of_int i)
                       in
                       if (Ui.signal ui b).clicked then
                         [ Syntax_edit (E.Fold_into { node = Hashtbl.find t.folds (n.path, i) }) ]
                       else []
                   | P.Row (i, { chip = P.Name s; _ }) when not (E.nested s) -> (
                       (* a row that reads a binding: a click on its name selects that binding *)
                       let root = List.hd (String.split_on_char '.' s) in
                       let rec target k =
                         if k < 0 then None
                         else
                           let cand = List.take k n.path @ [ root ] in
                           if node_of t cand <> None || input_of t cand <> None then Some cand
                           else target (k - 1)
                       in
                       match target (List.length n.path - 1) with
                       | None -> []
                       | Some cand ->
                           let left, width = ref_chip_geo t ~z ~fs ~w:(p.w *. z) s in
                           let b =
                             Ui.box ui
                               ~flags:Ui.(clickable + tab_stop)
                               ~w:(Ui.Px width)
                               ~h:(Ui.Px (16. *. z))
                               ~at:(left, (top +. (float k *. P.row_height) +. 4.) *. z)
                               ("ref" ^ string_of_int i)
                           in
                           if (Ui.signal ui b).clicked then [ Selected [ cand ] ] else [])
                   | _ -> [])
                 (Array.to_list p.lines)))
    | _ -> []
  in
  let editors =
    match (t.editing, p.item) with
    | Some (Name path), P.Item n when path = n.path ->
        Ui.within ui tile (fun () ->
            edit_field
              ~at:(21. *. z, 3. *. z)
              ~w:((p.w -. 21. -. head_pad) *. z)
              ~h:(18. *. z) "name" (P.title n) valid_name
              (fun v -> Syntax_edit (E.Rename { node = n.path; to_ = v })))
    | Some (Output path), P.Item n when path = n.path -> (
        match
          Array.find_index
            (function P.Row (_, { kind = P.Add; key = E.Kw ""; _ }) -> true | _ -> false)
            p.lines
        with
        | Some k ->
            Ui.within ui tile (fun () ->
                edit_field
                  ~at:(label_x *. z, (rows_top n 0. +. (float k *. P.row_height) +. 4.) *. z)
                  ~w:((p.w -. label_x -. head_pad) *. z)
                  ~h:(field_h *. z) "output" "" valid_name
                  (fun v ->
                    Syntax_edit
                      (E.Add_field { node = n.path; name = v; value = S.make (S.Num "0") })))
        | None ->
            finished := true;
            [])
    | Some (Default path), P.Input i when path = i.path ->
        Ui.within ui tile (fun () ->
            let text = match i.default with Some d -> Flow.Lisp.flat d | None -> "" in
            edit_field
              ~at:(value_x p.w *. z, 4. *. z)
              ~w:(field_w *. z) ~h:(field_h *. z) "default" text
              (fun v -> single_form v <> None)
              (fun v ->
                Syntax_edit
                  (E.Set_input_default
                     { form = List.hd i.path; input = i.name; value = Option.get (single_form v) })))
    | _ -> []
  in
  let movers =
    match p.item with
    | P.Item n when P.reorderable n && z >= 0.5 && (shown = P.Card || shown = P.Full) ->
        Ui.within ui tile (fun () ->
            let top = rows_top n 0. in
            List.concat
              (List.mapi
                 (fun k line ->
                   match line with
                   | P.Row (i, ({ kind = P.Rest; key = E.Pos j; _ } : P.row)) when j >= 1 ->
                       let b =
                         Ui.box ui
                           ~flags:Ui.(clickable + tab_stop)
                           ~w:(Ui.Px (16. *. z))
                           ~h:(Ui.Px (16. *. z))
                           ~at:((p.w -. 22.) *. z, (top +. (float k *. P.row_height) +. 4.) *. z)
                           ("mv" ^ string_of_int i)
                       in
                       if (Ui.signal ui b).clicked then
                         [ Syntax_edit (E.Move_item { node = n.path; pos = j }) ]
                       else []
                   | _ -> [])
                 (Array.to_list p.lines)))
    | _ -> []
  in
  (* a switch's rows: a click on a layout's name makes it the active one *)
  let switchers =
    match (p.item, List.assoc_opt p.path t.switches) with
    | P.Item n, Some active when z >= 0.5 && (shown = P.Card || shown = P.Full) ->
        Ui.within ui tile (fun () ->
            let top = rows_top n 0. in
            List.concat
              (List.mapi
                 (fun k line ->
                   match line with
                   | P.Row (i, ({ key = E.Pos j; kind; _ } : P.row))
                     when kind <> P.Add && j <> active ->
                       let b =
                         Ui.box ui
                           ~flags:Ui.(clickable + tab_stop)
                           ~w:(Ui.Px ((p.w -. 24.) *. z))
                           ~h:(Ui.Px (16. *. z))
                           ~at:(12. *. z, (top +. (float k *. P.row_height) +. 4.) *. z)
                           ("sw" ^ string_of_int i)
                       in
                       if (Ui.signal ui b).clicked then
                         [ Layout_active { graph = t.key; index = j } ]
                       else []
                   | _ -> [])
                 (Array.to_list p.lines)))
    | _ -> []
  in
  (p, ax, ay, tile, Ui.signal ui tile, sub, fields @ add_clicks @ editors @ movers @ switchers)

(* a payload in flight: the innermost node under the pointer (a zone's tile covers its body),
   else the canvas, is where it would be put; the host decides whether it takes it *)
let drop_change (t : t) ui canvas tiles =
  match Ui.drop_target ui canvas with
  | None -> None
  | Some _ ->
      let area (p : P.placed) = p.w *. p.h in
      let found =
        List.fold_left
          (fun best ((p : P.placed), _, _, tile, _, _, _) ->
            match (Ui.drop_target ui tile, best) with
            | Some _, Some (q, _) when area q <= area p -> best
            | Some d, _ -> Some (p, d)
            | None, _ -> best)
          None tiles
      in
      let path, drop =
        match (found, Ui.drop_target ui canvas) with
        | Some ((p : P.placed), d), _ -> (p.path, d)
        | None, Some d -> ([ t.key ], d)
        | None, None -> ([ t.key ], Ui.Hover { Ui.kind = ""; value = "" })
      in
      Some
        (match drop with
        | Ui.Hover { kind; value } -> Drop_over { path; kind; value }
        | Dropped { kind; value } -> Dropped { path; kind; value })

let update ?(pressed = []) t ui (frame : Frame.t) =
  (* the first view shows the whole graph, at zoom 1 when it fits (Home frames everything) *)
  let t = if t.framed then t else { (frame_all t) with framed = true } in
  let changes = ref [] in
  let emit c = changes := c :: !changes in
  let canvas =
    Ui.box ui
      ~flags:Ui.(clickable + scroll + clip + blocking)
      ~w:(Ui.Px (float t.width))
      ~h:(Ui.Px (float t.height))
      ~at:(float t.x, float t.y)
      "pxui-scope"
  in
  let canvas_signal = Ui.signal ui canvas in
  let t, hint_changes = step_hints t ~pressed ~clicked:(canvas_signal.subtree_press <> None) in
  List.iter emit hint_changes;
  (* the grid, zones and wires paint in a child, so the canvas's clip holds them too *)
  let layer =
    Ui.within ui canvas (fun () ->
        Ui.box ui
          ~w:(Ui.Px (float t.width))
          ~h:(Ui.Px (float t.height))
          ~at:(0., 0.) "pxui-scope-layer")
  in
  let t =
    let _, wheel = canvas_signal.scroll in
    let pinch = canvas_signal.pinch in
    if (wheel = 0. && pinch = 1.) || t.context <> None then t
    else begin
      let mx, my = frame.input.pointer in
      (* the wheel steps by a tenth; a pinch carries its own factor *)
      let zoom = Float.max 0.25 (Float.min 2.5 (t.zoom *. (1. +. (wheel *. 0.1)) *. pinch)) in
      let k = zoom /. t.zoom in
      {
        t with
        zoom;
        pan_x = mx -. float t.x -. ((mx -. float t.x -. t.pan_x) *. k);
        pan_y = my -. float t.y -. ((my -. float t.y -. t.pan_y) *. k);
      }
    end
  in
  let t = { t with measure = (fun size s -> Ui.text_width ui ~size s) } in
  let t =
    match t.scope with
    | Some scope when font_of t.zoom <> t.geo_fs ->
        (* a point's box is as wide as its name, drawn at the zoom's font *)
        if Array.exists (fun ((p : P.placed), _, _) -> p.shown = P.Point) t.geo.items then
          regeo t scope t.layout ~shift:no_shift
        else { t with geo_fs = font_of t.zoom }
    | _ -> t
  in
  (* a drag in progress moves its items in a fresh geometry *)
  let t =
    match (t.drag, t.scope) with
    | Some (Moving m), Some scope when m.moved ->
        let moving = Path_set.of_list m.paths in
        regeo t scope t.layout ~shift:(fun p ->
            if Path_set.mem p moving then (m.dx, m.dy) else (0., 0.))
    | Some (Carrying c), Some scope when Float.hypot c.dx c.dy > 0. ->
        let moving = Path_set.of_list c.paths in
        regeo t scope t.layout ~shift:(fun p ->
            if Path_set.mem p moving then (c.dx, c.dy) else (0., 0.))
    | _ -> t
  in
  let z = t.zoom in
  (* text follows the zoom down to 5 points (a 6-point row at zoom 0.25 holds it) *)
  let fs = font_of z in
  let viewport = (float t.x, float t.y, float t.width, float t.height) in
  (* what the pane shows, grown by [margin] screen points, in graph units: the indices answer it *)
  let in_view margin =
    ( (-.t.pan_x -. margin) /. z,
      (-.t.pan_y -. margin) /. z,
      (float t.width -. t.pan_x +. margin) /. z,
      (float t.height -. t.pan_y +. margin) /. z )
  in
  let visible =
    List.filter_map
      (fun i ->
        let (((p : P.placed), ax, ay) as item) = t.geo.items.(i) in
        let x = sx t ax -. 20.
        and y = sy t ay -. 20.
        and w = (p.w *. z) +. 40.
        and h = (p.h *. z) +. 40. in
        let vx, vy, vw, vh = viewport in
        if x < vx +. vw && x +. w > vx && y < vy +. vh && y +. h > vy then Some item else None)
      (index_query t.geo.tiles (in_view 20.))
  in
  let wires_in_view = index_query t.geo.reach (in_view 4.) in
  let zones =
    List.filter_map
      (fun ((p : P.placed), ax, ay) ->
        match p.item with
        | P.Item ({ zone = Some zn; _ } as n) when not p.collapsed -> Some (n, zn, p, ax, ay)
        | _ -> None)
      visible
  in
  let mouse = frame.input.pointer in
  (* Segment rectangles live in the shared hit tree, behind the cards; ports built below take
     hover precedence.  A segment is cut to [clip] (pane points) first, so what is out of view
     costs one clip; a diagonal is then a run of 16-point boxes. *)
  let chain = chains_of t in
  let wire_hits = Cells.create 16 in
  let hits i = Option.value ~default:[] (Cells.find_opt wire_hits i) in
  let wire_boxes = wire_boxes t ui ~wire_hits in
  Ui.within ui canvas (fun () ->
      List.iter (wire_boxes "wire" (0., 0., float t.width, float t.height)) wires_in_view);
  (* footers: only for what is in view, and not when too small to read *)
  let footers = Hashtbl.create 16 in
  (match t.records with
  | Some records when z >= 0.4 ->
      List.iter
        (fun ((p : P.placed), _, _) ->
          match p.item with
          | P.Item n when match n.zone with Some _ -> p.collapsed | None -> true ->
              Hashtbl.replace footers n.path (footer_of t records n)
          | _ -> ())
        visible
  | _ -> ());
  let finished = ref false and opened = ref None in
  let edit_field ~at ~w ~h key current valid commit =
    let v, open_ =
      Ui.value_field ui ~at ~w ~h ~size:fs ~edit:true
        ~left:(float_of_string_opt current = None)
        ~valid key current
    in
    if not open_ then finished := true;
    if (not open_) && v <> current && valid v then [ commit v ] else []
  in
  (* tiles *)
  let tiles =
    Ui.within ui canvas (fun () ->
        List.map (build_tile t ui ~z ~fs ~wire_boxes ~edit_field ~finished ~opened) visible)
  in
  let t = if !finished then { t with editing = None } else t in
  Option.iter emit (drop_change t ui canvas tiles);
  let t, frame_changes = frames_step t ui ~z ~edit_field ~canvas in
  List.iter emit frame_changes;
  let t = if !finished then { t with editing = None } else t in
  let t = match !opened with Some _ as editing -> { t with editing } | None -> t in
  let t, hover = hover_step t ui ~z ~mouse ~tiles ~hits ~wires_in_view in
  let t, pointer_changes =
    pointer_step t ui frame ~canvas ~canvas_signal ~tiles ~hits ~wires_in_view
  in
  List.iter emit pointer_changes;
  (* a wire let go: on an input row it connects; on empty canvas it opens the add menu after its
     source (pulled off an existing wire, the host inserts the new node on the selected wire) *)
  let finish_wire = finish_wire ui ~z ~mouse ~tiles in
  let t, pull_changes = wire_pull t ui ~finish_wire ~hits ~wires_in_view in
  let t, gesture_changes, lens_next = tile_gestures t ui ~z ~finish_wire ~tiles in
  let t, context_changes = context_step t ui ~tiles ~canvas_signal in
  List.iter emit (pull_changes @ gesture_changes @ context_changes);
  let drawn_items, drawn_zones =
    paint_frame t ui ~z ~fs ~layer ~canvas ~canvas_signal ~tiles ~zones ~wires_in_view ~footers
      ~chain ~mouse ~hover
  in
  let t = { t with stats = { t.stats with drawn_items; drawn_zones } } in
  (* an opened or stepped expansion panel changes its card's size: lay out again for the next frame *)
  let t = if lens_next == t.lens then t else relayout { t with lens = lens_next } in
  (t, List.rev !changes)

(* ------------------------------------------------------------- test hooks *)

let selected_wire t = Option.map (fun (path, key, _) -> (path, key)) t.selected_wire

module Inspect = struct
  let ghost t path =
    match (t.records, node_of t path, item_at t path) with
    | Some records, Some ({ zone = None; _ } as n), Some (p, _, _) when p.shown <> P.Full ->
        Some (footer_of t records n).value
    | _ -> None

  let grabbed t = t.panning_grab
  let macro_step = lens_of

  let highlighted_connections t =
    List.map
      (fun w -> ((sx t (fst w.a), sy t (snd w.a)), (sx t (fst w.b), sy t (snd w.b))))
      t.highlighted

  let box_of t path =
    box_at t path |> Option.map (fun (x, y, w, h) -> (sx t x, sy t y, w *. t.zoom, h *. t.zoom))

  let selector t path =
    let visible =
      match node_of t path with
      | Some { zone = Some { kind = P.Branch | Let | State; _ }; _ }
      | Some { zone = None; _ }
      | None ->
          false
      | Some _ -> not (t.collapsed path)
    in
    if not visible then None
    else
      Option.map
        (fun (x, y, w, _) ->
          let z = t.zoom in
          let fs = font_of z in
          let sx, sy, sw, sh, _ =
            selector_geo ~measure:t.measure ~z ~fs ~w ~count:(count_of t path) ~probe:(t.probe path)
          in
          let x = x +. sx and y = y +. sy in
          ( (x -. (4. *. z), y, 16. *. z, sh),
            (x +. (12. *. z), y, sw -. (24. *. z), sh),
            (x +. sw -. (12. *. z), y, 16. *. z, sh) ))
        (box_of t path)

  let output_socket t path =
    Option.map
      (fun ((p : P.placed), ax, ay) ->
        let x, y = out_anchor p ax ay in
        if p.shown = P.Point then (sx t x, sy t y)
        else (sx t (x -. 1.), sy t (y -. wire_head_y +. port_y)))
      (item_at t path)

  let row_center t path i =
    match (node_of t path, item_at t path) with
    | Some n, Some (p, x, y) -> (
        match List.nth_opt n.rows i with
        | Some { head = true; _ } -> Some (sx t (x +. 100.), sy t (y +. 12.))
        | Some _ ->
            Option.map
              (fun k ->
                (sx t (x +. 100.), sy t (rows_top n y +. ((float k +. 0.5) *. P.row_height))))
              (Hashtbl.find_opt (line_of_row p.lines) i)
        | None -> None)
    | _ -> None

  let expr_chip t path i =
    match (node_of t path, item_at t path) with
    | Some n, Some (p, x, y) -> (
        match (List.nth_opt n.rows i, Hashtbl.find_opt (line_of_row p.lines) i) with
        | Some { chip = P.Inline _; _ }, Some k ->
            let z = t.zoom in
            Some
              ( sx t x +. ((value_x p.w +. 24. +. ((field_w -. 24.) /. 2.)) *. z),
                sy t (rows_top n y +. ((float k +. 0.5) *. P.row_height)) )
        | _ -> None)
    | _ -> None

  let ref_chip t path i =
    match (node_of t path, item_at t path) with
    | Some n, Some (p, x, y) -> (
        match (List.nth_opt n.rows i, Hashtbl.find_opt (line_of_row p.lines) i) with
        | Some { chip = P.Name s; _ }, Some k ->
            let z = t.zoom in
            let left, width = ref_chip_geo t ~z ~fs:(font_of z) ~w:(p.w *. z) s in
            Some
              ( sx t x +. left +. (width /. 2.),
                sy t (rows_top n y +. ((float k +. 0.5) *. P.row_height)) )
        | _ -> None)
    | _ -> None

  let fold_button t path i =
    match (node_of t path, item_at t path) with
    | Some n, Some (p, x, y) when Hashtbl.mem t.folds (path, i) -> (
        match (List.nth_opt n.rows i, Hashtbl.find_opt (line_of_row p.lines) i) with
        | Some ({ chip = P.Name s; _ } as r), Some k ->
            let z = t.zoom in
            let left, fw, _, _, _ =
              wired_geo ~measure:t.measure t ~z ~fs:(font_of z) ~x:0. ~w:(p.w *. z) ~fold:true
                (wired_name r s)
            in
            Some
              ( sx t x +. left -. (fw /. 2.) -. (2. *. z),
                sy t (rows_top n y +. ((float k +. 0.5) *. P.row_height)) )
        | _ -> None)
    | _ -> None

  let tile_point t path (dx, dy) (w, h) =
    box_at t path
    |> Option.map (fun (x, y, _, _) ->
        (sx t x +. ((dx +. (w /. 2.)) *. t.zoom), sy t y +. ((dy +. (h /. 2.)) *. t.zoom)))

  let lens_toggle t path =
    match (node_of t path, box_at t path) with
    | Some { lens = Some _; _ }, Some (_, _, w, _) -> tile_point t path (w -. 22., 2.) (20., 20.)
    | _ -> None

  let lens_step_button t path i =
    match (node_of t path, lens_of t path, box_at t path) with
    | Some { lens = Some l; _ }, Some step, Some (_, _, _, h) when i <= Array.length l.steps ->
        let (bx, by), size = lens_button_box ~len:(Array.length l.steps) i in
        tile_point t path (bx, lens_top h (P.lens_height l ~step) +. by) size
    | _ -> None

  let lens_replace t path =
    match (node_of t path, lens_of t path, box_at t path) with
    | Some { lens = Some l; _ }, Some step, Some (_, _, _, h) ->
        let lh = P.lens_height l ~step in
        let (rx, ry), size = lens_replace_box lh in
        tile_point t path (rx, lens_top h lh +. ry) size
    | _ -> None

  (* every port of a plain node at the level it is shown, in screen points: the in end, the out
     end, a card's row sockets and named outputs *)
  let ports t path =
    match item_at t path with
    | Some (p, ax, ay) ->
        let s (x, y) = (sx t x, sy t y) in
        let rows =
          match p.item with
          | P.Item ({ zone = None; _ } as n) when p.shown = P.Card || p.shown = P.Full ->
              List.concat
                (List.mapi
                   (fun k line ->
                     match line with
                     | P.Row (_, r) when r.socket ->
                         [ s (ax, rows_top n ay +. (float k *. P.row_height) +. wire_row_y) ]
                     | _ -> [])
                   (Array.to_list p.lines))
              @ List.mapi
                  (fun j _ ->
                    s
                      ( ax +. p.w,
                        rows_top n ay
                        +. ((float (Array.length p.lines + j) +. 0.5) *. P.row_height)
                        +. 0.75 ))
                  n.outputs
          | _ -> []
        in
        s (in_anchor p ax ay) :: s (out_anchor p ax ay) :: rows
    | None -> []

  let chain t =
    Option.map (fun (up, down) -> (Path_set.elements up, Path_set.elements down)) (chains_of t)

  let tagged_wires t = Array.fold_left (fun n w -> if tagged t w then n + 1 else n) 0 t.geo.wires
  let wire_count t = Array.length t.geo.wires

  (* the nodes whose footer the pane draws: in view and not too small to read.  A zone's run count
     reads the geometry its calls take (a map over [sop/attr waves]), so while an open zone is in
     view every geometry node of the scope is counted, on screen or not. *)
  let footer_paths t =
    if t.zoom < 0.4 then []
    else
      let z = t.zoom in
      let zone_open = ref false in
      let seen =
        List.filter_map
          (fun i ->
            match t.geo.items.(i) with
            | { P.item = P.Item n; collapsed; _ }, _, _
              when match n.zone with Some _ -> collapsed | None -> true ->
                Some n.path
            | { P.item = P.Item { zone = Some _; _ }; _ }, _, _ ->
                zone_open := true;
                None
            | _ -> None)
          (index_query t.geo.tiles
             ( -.t.pan_x /. z,
               -.t.pan_y /. z,
               (float t.width -. t.pan_x) /. z,
               (float t.height -. t.pan_y) /. z ))
      in
      if not !zone_open then seen
      else
        Array.fold_left
          (fun acc -> function
            | { P.item = P.Item ({ zone = None; _ } as n); _ }, _, _ when n.ty = Flow.Ty.geometry ->
                n.path :: acc
            | _ -> acc)
          seen t.geo.items

  let wire_points t i =
    if i < 0 || i >= Array.length t.geo.wires then []
    else List.map (fun (x, y) -> (sx t x, sy t y)) t.geo.wires.(i).pts

  let wire_target t i =
    if i < 0 || i >= Array.length t.geo.wires then None
    else Option.map (fun (p, k, _) -> (p, k)) t.geo.wires.(i).target

  let wire_midpoint t i =
    if i < 0 || i >= Array.length t.geo.wires then None
    else
      let w = t.geo.wires.(i) in
      let seg ((x0, y0), (x1, y1)) = Float.hypot (x1 -. x0) (y1 -. y0) in
      let rec pairs = function a :: (b :: _ as rest) -> (a, b) :: pairs rest | _ -> [] in
      let ps = pairs w.pts in
      let at ((x0, y0), (x1, y1)) k =
        Some (sx t (x0 +. ((x1 -. x0) *. k)), sy t (y0 +. ((y1 -. y0) *. k)))
      in
      (* curves are sampled, so choose halfway along their full length *)
      if List.length ps > 5 then
        let rec walk left = function
          | p :: rest when left > seg p && rest <> [] -> walk (left -. seg p) rest
          | p :: _ -> at p (if seg p > 0. then Float.min 1. (left /. seg p) else 0.)
          | [] -> None
        in
        walk (List.fold_left (fun a p -> a +. seg p) 0. ps /. 2.) ps
      else
        match
          List.fold_left
            (fun best p -> match best with Some b when seg b >= seg p -> best | _ -> Some p)
            None ps
        with
        | Some p -> at p 0.5
        | None -> None
end
