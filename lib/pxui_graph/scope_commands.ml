open Rays
open Scope_wires
include Scope_state

let parents_removed paths =
  let set = Path_set.of_list paths in
  (* a path above this one is among them *)
  let rec under = function
    | [] | [ _ ] -> false
    | q -> let up = List.rev (List.tl (List.rev q)) in Path_set.mem up set || under up in
  List.filter (fun q -> not (under q)) paths

let first_inline (n : P.node) =
  List.find_opt (fun (r : P.row) -> match r.chip with
    | P.Inline _ -> true | P.Name leaf -> E.nested leaf | _ -> false) n.rows

let inline_rail (n : P.node) = match n.zone with
  | None -> None
  | Some z -> List.find_opt (fun (r : P.rail_row) -> match r.expr with
      | Some { S.node = S.List _; _ } -> r.key <> None | _ -> false) z.rail

let wired (r : P.row) = P.sources r <> []

(* taking a wire off a row: the nested node written there stays, as a binding of its own *)
let unwire (n : P.node) (r : P.row) =
  (match r.chip with
   | P.Name leaf when E.nested leaf -> [ E.Unfold { node = n.path; key = r.key; sub = [] } ]
   | _ -> [])
  @ [ E.Disconnect { node = n.path; key = r.key; fallback = fallback r } ]

(* ---------------------------------------------------------------- frames *)

let scope_of_path path = List.rev (List.tl (List.rev path))

(* a frame's size while its corner is dragged *)
let frame_list t scope : fr list = match t.drag with
  | Some (Sizing s) when s.scope = scope ->
      List.mapi (fun i ((title, at, (w, h)) : fr) ->
        if i = s.index then title, at, (Float.max 48. (w +. s.dw), Float.max 32. (h +. s.dh))
        else title, at, (w, h)) (t.frames scope)
  | Some (Carrying c) when c.scope = scope ->
      List.mapi (fun i ((title, (x, y), size) : fr) ->
        if i = c.index then title, (x +. c.dx, y +. c.dy), size else title, (x, y), size) (t.frames scope)
  | _ -> t.frames scope

(* a frame around the selected nodes of one scope, in that scope's coordinates *)
let new_frame t =
  match List.filter (fun p -> Paths.mem t.geo.slot p) (selected t) with
  | [] -> None
  | first :: _ as paths ->
      let scope = scope_of_path first in
      let boxes = List.filter_map (fun p ->
        if scope_of_path p = scope then box_at t p else None) paths in
      let ox, oy = Option.value ~default:(0., 0.) (List.assoc_opt scope t.geo.origins) in
      let x0 = List.fold_left (fun a (x, _, _, _) -> Float.min a x) infinity boxes
      and y0 = List.fold_left (fun a (_, y, _, _) -> Float.min a y) infinity boxes
      and x1 = List.fold_left (fun a (x, _, w, _) -> Float.max a (x +. w)) neg_infinity boxes
      and y1 = List.fold_left (fun a (_, y, _, h) -> Float.max a (y +. h)) neg_infinity boxes in
      Some (scope, ("frame", (x0 -. 12. -. ox, y0 -. 24. -. oy), (x1 -. x0 +. 24., y1 -. y0 +. 36.)))

let input_of t path = match t.scope with
  | Some s -> List.find_opt (fun (i : P.input) -> i.path = path) s.inputs
  | None -> None

(* ------------------------------------------------------------- commands *)

(* [Some hidden] when pressing b hides or shows the node: it has a :visible argument and no bypass *)
let hide_row (n : P.node) =
  if P.bypassable n then None else
  List.find_map (fun (r : P.row) -> match r.key, r.kind, r.ty with
    | E.Kw "visible", P.Arg, Some Ty.Bool ->
        Some (match r.expr with Some { S.node = S.Sym "false"; _ } -> true | _ -> false)
    | _ -> None) n.rows

let conditional_target (n : P.node) = match List.rev n.path with
  | leaf :: outer -> (match E.leaf_keys leaf with
      | Some (holder, [E.Arm index]) -> List.rev outer @ [holder], index
      | _ -> n.path, -1)
  | [] -> n.path, -1

let action_changes t command =
  let nodes = selected_nodes t in
  let paths = List.map (fun (n : P.node) -> n.path) (List.filter (fun (n : P.node) -> not n.synthetic) nodes) in
  let one f = match nodes with [ n ] -> f n | _ -> [ Notice "Select one node" ] in
  let edit op = [ Syntax_edit op ] in
  let wrap loop what =
    if paths = [] then [ Notice ("Select nodes to " ^ what) ] else edit (E.Wrap { nodes = paths; loop }) in
  match command with
  | Delete ->
      (match t.hovered_row with
       | Some (path, key) when (match node_of t path with
           | Some n -> List.exists (fun (r : P.row) -> r.key = key && (wired r || r.kind = P.Rest)) n.rows | None -> false) ->
           (* a wire comes off; an item of a variadic input (a list's, a merge's) goes with what it holds *)
           let n = Option.get (node_of t path) in
           let r = List.find (fun (r : P.row) -> r.key = key) n.rows in
           List.map (fun op -> Syntax_edit op) (unwire n r)
       | _ -> (match t.selected_wire with
           | Some (path, key, fb) ->
               (match Option.bind (node_of t path) (fun n ->
                  Option.map (fun r -> n, r) (List.find_opt (fun (r : P.row) -> r.key = key) n.rows)) with
                | Some (n, r) -> List.map (fun op -> Syntax_edit op) (unwire n r)
                | None -> edit (E.Disconnect { node = path; key; fallback = fb }))
           | None -> if paths = [] then [] else edit (E.Delete_nodes { nodes = paths })))
  | Fold_into -> one (fun n -> edit (E.Fold_into { node = n.path }))
  | Unfold -> one (fun n ->
      let parent, arm = conditional_target n in
      if parent <> n.path then edit (E.Unfold {node = parent; key = E.Arm arm; sub = []}) else
      match t.hovered_row, first_inline n with
      | Some (p, key), _ when p = n.path -> edit (E.Unfold { node = n.path; key; sub = [] })
      | _, Some r -> edit (E.Unfold { node = n.path; key = r.key; sub = [] })
      | _ -> (match inline_rail n with
          | Some { key = Some key; _ } -> edit (E.Unfold { node = n.path; key; sub = [] })
          | _ -> [ Notice "Nothing to unfold here" ]))
  | Unfold_all -> one (fun n ->
      match List.filter_map (fun (r : P.row) ->
        if (match r.chip with P.Inline _ -> true | P.Name leaf -> E.nested leaf | _ -> false)
        then Some (E.Unfold { node = n.path; key = r.key; sub = [] }) else None) n.rows with
      | [] -> [ Notice "Nothing to unfold here" ]
      | ops -> List.concat_map edit ops)
  | Hoist -> one (fun n -> edit (E.Hoist { node = n.path }))
  | Bypass -> one (fun n ->
      (* a node that cannot pass an input through but has a boolean :visible (a scene object)
         takes itself out of the render without leaving the graph *)
      match hide_row n with
      | Some hidden ->
          edit (E.Set_arg { node = n.path; key = E.Kw "visible"; sub = []
                          ; value = S.make (S.Sym (if hidden then "true" else "false")) })
      | None -> edit (E.Toggle_bypass { node = n.path }))
  | Wrap_repeat -> wrap E.For "repeat"
  | Wrap_iterate -> wrap E.Fold "iterate"
  | Wrap_if -> wrap E.If "wrap"
  | Wrap_map -> wrap E.Map "map"
  | Wrap_filter -> wrap E.Filter "filter"
  | Inline_fn -> one (fun n -> edit (E.Inline_defn { node = n.path }))
  | Add_arm -> one (fun n -> let node, after = conditional_target n in edit (E.Add_arm {node; after}))
  | Delete_arm -> one (fun n -> let node, index = conditional_target n in
      edit (E.Delete_arm {node; index = if node = n.path then 0 else index}))
  | Make_fn -> if paths = [] then [ Notice "Select nodes to make a function" ] else edit (E.Make_local_fn { nodes = paths })
  | Make_macro -> if paths = [] then [ Notice "Select nodes to make a macro" ] else [ Macro_requested paths ]
  | Make_defn -> if paths = [] then [ Notice "Select nodes to make a reusable function" ] else [ Defn_requested paths ]
  | Collapse ->
      List.filter_map (fun (n : P.node) -> match n.zone with
        | Some { kind = P.Let; _ } | None -> None
        | Some _ -> Some (Zone_collapsed { zone = n.path; collapsed = not (t.collapsed n.path) })) nodes
  | Probe_step delta ->
      (* with nothing selected, one stepper pins the iteration of every zone of the graph *)
      let nodes = if nodes <> [] then nodes else
        List.filter_map (fun ((p : P.placed), _, _) -> match p.item with P.Item n -> Some n | _ -> None)
          (Array.to_list t.geo.items) in
      List.filter_map (fun (n : P.node) -> match n.zone with
        | Some { kind = P.Let; _ } | None -> None
        | Some _ ->
            let count = count_of t n.path in
            if count <= 0 then None else
            let index = max 0 (min (count - 1) (t.probe n.path + delta)) in
            if index = t.probe n.path then None else Some (Probe_set { zone = n.path; index })) nodes
  | Frame_all | Tidy | Toggle_tags | Frame_selection | Walk _ | Edit_name | Make_frame
  | Open_level | Point_level | Open_all | Point_all | Show_hints -> []
  | Duplicate -> if paths = [] then [ Notice "Select nodes to duplicate" ] else edit (E.Duplicate { nodes = paths })
  | Copy -> if paths = [] then [ Notice "Select nodes to copy" ] else [ Copy_requested paths ]
  | Cut -> if paths = [] then [ Notice "Select nodes to cut" ]
      else Copy_requested paths :: edit (E.Delete_nodes { nodes = paths })
  | Paste -> [ Paste_requested ]
  | Display -> one (fun n ->
      if not (List.mem n.ty Flow.Ty.[ geometry; drawing; image ]) then
        [ Notice "Only a geometry, drawing or image node can be viewed" ]
      else [ Display_set n.path ])
  | Item_up | Item_down ->
      (match t.hovered_row with
       | Some (path, E.Pos i) when (match node_of t path with
           | Some n -> P.reorderable n | None -> false) ->
           edit (E.Move_item { node = path; pos = if command = Item_up then i else i + 1 })
       | _ ->
           (* off a list item, the same keys move the selected binding in the text (cards stay put) *)
           one (fun n -> edit (E.Move_binding { node = n.path; by = if command = Item_up then -1 else 1 })))

let frame_selection t =
  match List.filter_map (box_at t) (selected t) with
  | [] -> frame_all t
  | boxes ->
      let x0 = List.fold_left (fun a (x, _, _, _) -> Float.min a x) infinity boxes
      and y0 = List.fold_left (fun a (_, y, _, _) -> Float.min a y) infinity boxes
      and x1 = List.fold_left (fun a (x, _, w, _) -> Float.max a (x +. w)) neg_infinity boxes
      and y1 = List.fold_left (fun a (_, y, _, h) -> Float.max a (y +. h)) neg_infinity boxes in
      let w = Float.max 1. (x1 -. x0) and h = Float.max 1. (y1 -. y0) in
      let zoom = Float.max 0.25 (Float.min 1.5
        (Float.min ((float t.width -. 40.) /. w) ((float t.height -. 40.) /. h))) in
      { t with zoom; pan_x = (float t.width -. w *. zoom) /. 2. -. x0 *. zoom;
               pan_y = (float t.height -. h *. zoom) /. 2. -. y0 *. zoom }

(* ---- levels (flow.md 6.4) ---- *)

let level_up = function P.Point -> P.Chip | Chip -> Card | Card | Full -> P.Full

(* the nodes a level applies to: cards, not zones or value cards *)
let level_of (p : P.placed) = match p.item with
  | P.Item n when n.zone = None && not (P.value_card n) -> Some p.level
  | _ -> None
let levelled t path = Option.bind (item_at t path) (fun (p, _, _) -> level_of p)

let level_command t command =
  let chosen = match command with
    | Open_all | Point_all ->
        Array.to_list t.geo.items |> List.filter_map (fun ((p : P.placed), _, _) -> Option.map (fun l -> p.path, l) (level_of p))
    | _ -> List.filter_map (fun p -> Option.map (fun l -> p, l) (levelled t p)) (selected t) in
  if chosen = [] then t, [ Notice "Select a card to change its level" ] else
  (* by path: every node may be chosen, so no list is searched per node *)
  let before = Hashtbl.create 16 and now = Hashtbl.create 16 in
  List.iter (fun (p, l) -> Hashtbl.replace before p l) (List.rev t.back);
  List.iter (fun (p, _) -> Hashtbl.replace now p ()) chosen;
  let back_of p = Option.value ~default:P.Card (Hashtbl.find_opt before p) in
  let changes, back = match command with
    | Open_level -> List.map (fun (p, l) -> p, Some (level_up l), true) chosen, t.back
    | Open_all -> List.map (fun (p, _) -> p, None, false) chosen, t.back
    | Point_level | Point_all | _ ->
        let all_points = List.for_all (fun (_, l) -> l = P.Point) chosen in
        if all_points then
          List.map (fun (p, _) -> let l = back_of p in p, (if l = P.Card then None else Some l), false) chosen, t.back
        else
          List.map (fun (p, _) -> p, Some P.Point, false) chosen,
          List.map (fun (p, l) -> p, (if l = P.Point then back_of p else l)) chosen
          @ List.filter (fun (p, _) -> not (Hashtbl.mem now p)) t.back in
  { t with back }, [ Level_set changes ]

(* ---- letter hints (flow.md 7.5): [w] labels every input the selected output can connect to ---- *)

let hint_alphabet = "asdfghjklqwertyuiopzxcvbnm"
let hint_labels n =
  if n <= 26 then List.init n (fun i -> String.make 1 hint_alphabet.[i])
  else List.init (min n 676) (fun i -> Printf.sprintf "%c%c" hint_alphabet.[i / 26] hint_alphabet.[i mod 26])

(* the nodes of [scope] that [name] depends on: connecting from [name] to them would close a cycle *)
let upstream_of t (src : P.node) =
  let nodes = match t.scope with
    | Some scope ->
        let rec all (s : P.scope) = List.concat_map (fun (n : P.node) ->
          n :: (match n.zone with Some z -> all z.scope | None -> [])) s.nodes in
        all scope
    | None -> [] in
  let binders = Hashtbl.create 64 in
  List.iter (fun (m : P.node) -> List.iter (fun b -> Hashtbl.add binders b m) m.binds) nodes;
  let seen = Hashtbl.create 16 in
  let rec visit (n : P.node) =
    if not (Hashtbl.mem seen n.path) then begin
      Hashtbl.replace seen n.path ();
      List.iter (fun (r : P.row) -> List.iter (fun name ->
        List.iter visit (Hashtbl.find_all binders (root_name name))) (P.sources r)) n.rows
    end in
  visit src; seen

let start_hints t =
  match selected_nodes t with
  | [ src ] when src.zone = None && not src.synthetic ->
      let up = upstream_of t src in
      let scope = scope_of_path src.path in
      let sx, sy = match box_at t src.path with Some (x, y, _, _) -> x, y | None -> 0., 0. in
      let candidates = Array.to_list t.geo.items |> List.filter_map (fun ((p : P.placed), ax, ay) -> match p.item with
        | P.Item n when n.zone = None && p.path <> src.path && scope_of_path p.path = scope
                        && not (Hashtbl.mem up n.path) ->
            let lines = line_of_row p.lines in
            let rows = List.filter_map (fun (i, (r : P.row)) ->
              match r.ty with
              | Some ty when r.socket && r.kind <> P.Add && Ty.fits src.ty ty
                             && (r.head || Hashtbl.mem lines i) ->
                  let y = if r.head then ay +. 12. else
                    rows_top n ay +. (float (Hashtbl.find lines i) +. 0.5) *. P.row_height in
                  Some (r, (ax, y))
              | _ -> None) (List.mapi (fun i r -> i, r) n.rows) in
            if rows = [] then None else Some (n, (ax, ay), rows)
        | _ -> None) in
      let by_distance = List.sort (fun (_, (ax, ay), _) (_, (bx, by), _) ->
        compare (Float.hypot (ax -. sx) (ay -. sy)) (Float.hypot (bx -. sx) (by -. sy))) candidates in
      let by_distance = List.filteri (fun i _ -> i < 676) by_distance in
      let labels = hint_labels (List.length by_distance) in
      let targets = List.map2 (fun label ((n : P.node), at, rows) ->
        match rows with
        | [ ((r : P.row), _) ] -> { label; node = n.path; key = Some r.key; pos = at }
        | _ -> { label; node = n.path; key = None; pos = at }) labels by_distance in
      if targets = [] then t, [ Notice "Nothing the selected output can connect to" ]
      else { t with hinting = Some { src = src.name; src_ty = src.ty; typed = ""; targets; stage = None; armed = false } }, []
  | _ -> t, [ Notice "Select one node to connect from" ]

(* a letter, Backspace or Escape while hints are up; [None] when it was not a hint key *)
let step_hints t (frame : Frame.t) =
  let pressed = List.filter (fun k -> not (List.mem k (fst t.held))) frame.keys in
  let clicked = frame.mouse_buttons <> [] && not (snd t.held) in
  let t = { t with held = (frame.keys, frame.mouse_buttons <> []) } in
  match t.hinting with
  | None -> t, []
  | Some h when not h.armed -> { t with hinting = Some { h with armed = true } }, []  (* the key that began it *)
  | Some h ->
      let h_ref = ref h and out = ref [] and cancel = ref false in
      if clicked then cancel := true;
      List.iter (function
        | Input.Escape -> cancel := true
        | Input.Backspace ->
            let h = !h_ref in
            if h.typed <> "" then h_ref := { h with typed = String.sub h.typed 0 (String.length h.typed - 1) }
            else if h.stage <> None then h_ref := { h with stage = None }
            else cancel := true
        | Input.KeyChar c when String.contains hint_alphabet (Char.lowercase_ascii c) && !out = [] ->
            let h = !h_ref in
            let typed = h.typed ^ String.make 1 (Char.lowercase_ascii c) in
            let live = List.filter (fun (tg : hint_target) -> String.starts_with ~prefix:typed tg.label) h.targets in
            (match List.find_opt (fun (tg : hint_target) -> tg.label = typed) live with
             | Some { node; key = Some key; _ } when h.stage = None ->
                 out := [ Syntax_edit (E.Connect { node; key; src = h.src; iter = false }) ]; cancel := true
             | Some { node; key = None; _ } when h.stage = None ->
                 (* a node with several inputs: its inputs get letters of their own *)
                 let rows = match node_of t node with
                   | Some n -> List.filter_map (fun (r : P.row) ->
                       match r.ty with
                       | Some ty when r.socket && r.kind <> P.Add && Ty.fits h.src_ty ty -> Some r | _ -> None) n.rows
                   | None -> [] in
                 let labels = hint_labels (List.length rows) in
                 let ax, ay = match box_at t node with Some (x, y, _, _) -> x, y | None -> 0., 0. in
                 let n = Option.get (node_of t node) in
                 let lines = match item_at t node with
                   | Some (p, _, _) -> line_of_row p.lines | None -> Hashtbl.create 1 in
                 let targets = List.map2 (fun label (r : P.row) ->
                   let i = Option.value ~default:0 (List.find_index (fun x -> x == r) n.rows) in
                   let y = if r.head then ay +. 12. else match Hashtbl.find_opt lines i with
                     | Some k -> rows_top n ay +. (float k +. 0.5) *. P.row_height | None -> ay +. 12. in
                   { label; node; key = Some r.key; pos = (ax, y) }) labels rows in
                 h_ref := { h with typed = ""; targets; stage = Some node }
             | Some { node; key = Some key; _ } ->
                 out := [ Syntax_edit (E.Connect { node; key; src = h.src; iter = false }) ]; cancel := true
             | _ -> if live = [] then () else h_ref := { h with typed })
        | _ -> ()) pressed;
      if !cancel then { t with hinting = None }, !out else { t with hinting = Some !h_ref }, !out

let run_command t = function
  | Frame_all -> frame_all t, []
  | Tidy -> tidy t, []
  | Toggle_tags -> { t with tags = not t.tags }, [ Notice (if t.tags then "wire tags off" else "wire tags on") ]
  | (Open_level | Point_level | Open_all | Point_all) as command -> level_command t command
  | Show_hints -> start_hints t
  | Frame_selection -> frame_selection t, []
  | Edit_name ->
      (match selected t with
       | [ path ] when (match node_of t path with Some n -> not n.synthetic | None -> false) ->
           { t with editing = Some (Name path) }, []
       | [ path ] when input_of t path <> None -> { t with editing = Some (Default path) }, []
       | _ -> t, [ Notice "Select one node or graph input to edit" ])
  | Make_frame ->
      (match new_frame t with
       | Some (scope, frame) -> t, [ Frames_set { scope; frames = t.frames scope @ [ frame ] } ]
       | None -> t, [ Notice "Select nodes to frame" ])
  | Walk direction ->
      let center (p : P.placed) (_, ax, ay) = ax +. p.w /. 2., ay +. p.h /. 2. in
      let current = match selected t with
        | path :: _ -> item_at t path
        | [] -> None in
      (match current with
       | None -> (match Array.to_list t.geo.items with
           | ((p : P.placed), _, _) :: _ -> select [ p.path ] t, [ Selected [ p.path ] ]
           | [] -> t, [])
       | Some (cp, cx, cy) ->
           let ox, oy = center cp (cp, cx, cy) in
           let best = Array.fold_left (fun best ((p : P.placed), ax, ay) ->
             if p.path = cp.path then best else
             let x, y = center p (p, ax, ay) in
             let dx = x -. ox and dy = y -. oy in
             let along, across = match direction with
               | Right -> dx, Float.abs dy | Left -> -. dx, Float.abs dy
               | Down -> dy, Float.abs dx | Up -> -. dy, Float.abs dx in
             if along <= 1. then best else
             let cost = along +. 2. *. across in
             match best with Some (c, _) when c <= cost -> best | _ -> Some (cost, p.path)) None t.geo.items in
           (match best with
            | Some (_, path) -> select [ path ] t, [ Selected [ path ] ]
            | None -> t, []))
  | command -> t, action_changes t command

let bindings =
  let open Editor_core.Keymap in
  let open Editor_core.Guide_context in
  (* the guide strip lists a key where it does something: on the empty canvas, with one node
     selected, or with several *)
  let make ~guide id label action key modifiers =
    Editor_core.Command.make ~id:("scope." ^ id) ~label ~guide ~trigger:(Chord (key, modifiers)) action in
  let ch c = Input.KeyChar c in
  let any = [ Canvas; Node; Multi ] and some = [ Node; Multi ] and one = [ Node ] in
  [ make ~guide:some "delete" "delete" Delete Input.Delete [];
    make ~guide:some "delete" "delete" Delete Input.Backspace [];
    make ~guide:some "delete" "delete" Delete (ch 'x') [];
    make ~guide:one "fold" "fold into use" Fold_into (ch 'f') [ Input.Shift ];
    make ~guide:one "unfold" "unfold a call" Unfold (ch 'u') [ Input.Shift ];
    make ~guide:one "unfold-all" "unfold every call of the card" Unfold_all (ch 'u') [ Input.Shift; Input.Alt ];
    make ~guide:one "hoist" "hoist out" Hoist (ch 'h') [ Input.Shift ];
    make ~guide:one "bypass" "toggle bypass" Bypass (ch 'b') [];
    make ~guide:some "macro" "make macro" Make_macro (ch 'm') [];
    make ~guide:some "repeat" "repeat (loop)" Wrap_repeat (ch 'r') [];
    make ~guide:some "iterate" "iterate (feed back)" Wrap_iterate (ch 'r') [ Input.Shift ];
    make ~guide:some "conditional" "wrap conditional" Wrap_if (ch 'i') [Input.Shift];
    make ~guide:some "map" "wrap in map" Wrap_map (ch 'm') [Input.Shift];
    make ~guide:some "filter" "wrap in filter" Wrap_filter (ch 'l') [Input.Shift];
    make ~guide:one "inline-fn" "inline a function call" Inline_fn (ch 'i') [Input.Shift; Input.Alt];
    make ~guide:one "add-arm" "add conditional arm" Add_arm (ch 'a') [Input.Alt];
    make ~guide:one "delete-arm" "delete first arm" Delete_arm Input.Delete [Input.Alt];
    make ~guide:some "function" "make function" Make_fn (ch 'l') [];
    make ~guide:some "defn" "make reusable function (defn)" Make_defn (ch 'd') [];
    make ~guide:some "collapse" "collapse or expand zone" Collapse (ch 'c') [];
    make ~guide:any "probe-prev" "previous iteration (every zone with nothing selected)" (Probe_step (-1)) (ch '[') [];
    make ~guide:any "probe-next" "next iteration (every zone with nothing selected)" (Probe_step 1) (ch ']') [];
    make ~guide:any "frame-all" "frame all" Frame_all Input.Home [];
    make ~guide:any "tidy" "tidy: recompute every card position" Tidy (ch 't') [ Input.Shift ];
    make ~guide:any "tags" "toggle name tags on long wires" Toggle_tags (ch 'y') [ Input.Shift ];
    make ~guide:any "frame-selection" "frame the selection" Frame_selection (ch 'f') [];
    make ~guide:one "hints" "letter hints: connect the selected output" Show_hints (ch 'w') [];
    make ~guide:some "open" "open one level" Open_level (ch 'o') [];
    make ~guide:some "point" "point, or back" Point_level (ch 'p') [];
    make ~guide:any "open-all" "every node to card" Open_all (ch 'o') [ Input.Shift ];
    make ~guide:any "point-all" "every node to a point, or back" Point_all (ch 'k') [ Input.Shift ];
    make ~guide:one "rename" "rename node / edit input default" Edit_name Input.F2 [];
    make ~guide:some "frame" "frame the selection (titled box)" Make_frame (ch 'g') [ Input.Shift ];
    make ~guide:some "duplicate" "duplicate" Duplicate (ch 'd') [ Input.Meta ];
    make ~guide:some "duplicate" "duplicate" Duplicate (ch 'd') [ Input.Ctrl ];
    make ~guide:some "copy" "copy as text" Copy (ch 'c') [ Input.Meta ];
    make ~guide:some "copy" "copy as text" Copy (ch 'c') [ Input.Ctrl ];
    make ~guide:some "cut" "cut" Cut (ch 'x') [ Input.Meta ];
    make ~guide:some "cut" "cut" Cut (ch 'x') [ Input.Ctrl ];
    make ~guide:any "paste" "paste bindings" Paste (ch 'v') [ Input.Meta ];
    make ~guide:any "paste" "paste bindings" Paste (ch 'v') [ Input.Ctrl ];
    make ~guide:one "display" "view in the viewport" Display (ch 'v') [];
    make ~guide:one "item-up" "move list item up, or the binding in the text" Item_up Input.ArrowUp [ Input.Alt ];
    make ~guide:one "item-down" "move list item down, or the binding in the text" Item_down Input.ArrowDown [ Input.Alt ] ]
  @ List.map (fun (direction, arrow, name) ->
      make ~guide:any ("walk." ^ name) ("walk " ^ name) (Walk direction) arrow [])
    [ Left, Input.ArrowLeft, "left"; Down, Input.ArrowDown, "down";
      Up, Input.ArrowUp, "up"; Right, Input.ArrowRight, "right" ]
