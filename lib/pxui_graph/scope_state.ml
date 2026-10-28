(* The graph pane of a workspace document: draws one
   [Flow_graph.Projection.scope] with its zones, rails, iteration selectors,
   chips and output rows on the shared PXUI handle, and returns typed
   requests.  It never edits: a gesture is a [Flow_edit.op] the host reduces.
   Every interactive element is a [Ui.box]; a tile's row under the pointer is
   found from the pointer and the tile's rectangle, not from a second hit
   tree. *)
open Rays
module P = Flow_graph.Projection
module E = Flow_graph.Flow_edit
module S = Flow.Syntax
module Ty = Flow.Ty
module Ui = Pxui.Ui

type path = P.path
module Path_set = Set.Make (struct type t = path let compare = compare end)

type change =
  | Syntax_edit of E.op
  | Layout_active of { graph : string; index : int }
      (** a click on a layout of an editor graph's switch: the host makes it the active one *)
  | Probe_set of { zone : path; index : int }
  | Zone_collapsed of { zone : path; collapsed : bool }
  | Selected of path list
  | Moved of (path * float * float) list
  | Level_set of (path * P.level option * bool) list
      (** nodes opened or pointed: their new level ([None]: back to the default, [Card]) and
          whether it is pinned (it then ignores the zoom caps) *)
  | Copy_requested of path list  (** the host puts these bindings' text on the clipboard *)
  | Paste_requested  (** the host adds the clipboard's bindings here *)
  | Menu_requested of float * float  (** a right-click on empty canvas: the host opens its add menu there *)
  | Open_import of string
  | Macro_requested of path list  (** the host opens the make-macro dialog over these nodes *)
  | Defn_requested of path list  (** the host types the outside names and writes the [defn] *)
  | Frames_set of { scope : path; frames : (string * (float * float) * (float * float)) list }
      (** the frames of one scope after a gesture (create, resize, retitle, delete) *)
  | Display_set of path  (** preview this geometry node in the viewport, or this drawing or image node in the canvas panes, at the zone selectors *)
  | Activated of path
  | Drop_over of { path : path; kind : string; value : string }
  | Dropped of { path : path; kind : string; value : string }
  | Notice of string

type direction = Left | Down | Up | Right
type command =
  | Delete | Fold_into | Unfold | Unfold_all | Hoist | Bypass | Wrap_repeat | Wrap_iterate | Wrap_if | Wrap_map | Wrap_filter | Inline_fn | Add_arm | Delete_arm | Make_fn | Make_macro | Make_defn
  | Collapse | Probe_step of int | Frame_all | Tidy | Toggle_tags | Walk of direction
  | Edit_name  (** rename the selected node, or edit the default of a selected graph input *)
  | Item_up | Item_down  (** move the hovered list item *)
  | Make_frame  (** a titled frame around the selected nodes *)
  | Duplicate | Display | Copy | Cut | Paste
  | Frame_selection  (** [f]: pan and zoom to the selected nodes (all, with none selected) *)
  | Open_level  (** [o]: the selection one level more detailed, pinned *)
  | Point_level  (** [p]: the selection to points, or back to its previous level *)
  | Open_all  (** [⇧O]: every node to card *)
  | Point_all  (** [⇧K]: every node to a point, or every node back *)
  | Show_hints  (** [w]: letter hints; a letter connects the selected node's output *)

open Scope_wires

type stats = {
  nodes : int; zones : int; rows : int;
  drawn_items : int; drawn_zones : int;
}

(* ---------------------------------------------------------------- state *)

(* [lift]: the inputs an Alt-drag picked the wire up from; letting go takes it off them *)
type wiring = { src : string; ty : Ty.t option; iter : bool; from : float * float; lift : (path * E.arg_key) list }
type drag =
  | Moving of { paths : path list; dx : float; dy : float; moved : bool }
  | Wiring of wiring
  | Marquee of { base : Path_set.t }  (* from the canvas press point to the pointer, screen space *)
  | Sizing of { scope : path; index : int; dw : float; dh : float }  (* a frame's corner *)
  | Carrying of { scope : path; index : int; paths : path list; dx : float; dy : float }
      (* a frame by its title, with the nodes inside it *)

(* the text field open over the pane: a node's name, a graph input's default, a frame's title *)
type editing = Name of path | Default of path | Title of path * int
             | Output of path  (* the name of a new output, over the [+ output] row of a [values] *)

type fr = string * (float * float) * (float * float)

(* letter hints (flow.md 7.5): [w] labels every input the selected node's output can connect to *)
type hint_target = { label : string; node : path; key : E.arg_key option; pos : float * float }
type hinting = { src : string; src_ty : Ty.t; typed : string; targets : hint_target list; stage : path option; armed : bool }

type t = {
  x : int; y : int; width : int; height : int;
  theme : Pxui.theme; guide : bool;
  key : string;
  scope : P.scope option;
  imported : path -> string option;
  at : path -> (float * float) option;
  level_at : path -> (P.level * bool) option;  (* a node's saved level and whether it is pinned *)
  pin_at : path -> string -> bool option;  (* a node's row pinned onto (or off) its card, by label *)
  collapsed : path -> bool;
  lens : (path * int) list;  (* macro calls whose expansion panel is open, and its step *)
  probe : path -> int;
  records : Flow_graph.Probe.t option;
  chains : (path, path list) Hashtbl.t;  (* the iterating zones around each node *)
  folds : (path * int, path) Hashtbl.t;  (* a wired row (node, row index) whose one source can fold into it: that node *)
  counts : (path, int) Hashtbl.t;  (* iterations each zone ran, under the probes *)
  frames : path -> (string * (float * float) * (float * float)) list;
  display : path option;  (* the node the viewport shows instead of the graph's result *)
  framed : bool;
  layout : P.layout;
  geo : geo;
  geo_fs : int;  (* the font size point names were measured at *)
  pan_x : float; pan_y : float; zoom : float;
  selected : Path_set.t;
  selected_wire : (path * E.arg_key * S.t option) option;
  panning_grab : bool;
  hovered_row : (path * E.arg_key) option;
  highlighted : wire list;
  drag : drag option;
  editing : editing option;
  context : ((float * float) * path) option;
  stats : stats;
  switches : (path * int) list;  (* the [ui/switch] nodes and their active layout *)
  carry_lit : (path * string) list;  (* the places a carried payload can be put, with their letters *)
  carry_hot : (path * bool) option;  (* the place under the pointer, and whether it takes the payload *)
  failed : (path * string) list;  (* nodes a cook or the checker refused, with the diagnostic's code *)
  hinting : hinting option;
  back : (path * P.level) list;  (* the level a node had before [p] pointed it *)
  measure : int -> string -> float;  (* text width in points, from the last built frame *)
  tags : bool;  (* a long wire is a named tag at its use site, unless its chain is lit (flow.md 6.4) *)
  chain : (Path_set.t * Scope_wires.wire array * (Path_set.t * Path_set.t)) option ref;
      (* one slot: the chains of the last selection over the last wires *)
}

(* the geometry of the scope *)
let font_of zoom = max 5 (int_of_float (Float.round (13. *. zoom)))
let regeo t scope layout ~shift =
  let fs = font_of t.zoom in
  { t with geo_fs = fs;
    geo = compute ~name_w:(fun s -> t.measure fs s /. t.zoom) ~shown:shown_of scope layout ~shift }

let no_stats = { nodes = 0; zones = 0; rows = 0; drawn_items = 0; drawn_zones = 0 }
let create ?(width = 640) ?(height = 360) () = {
  x = 0; y = 0; width; height; theme = Pxui.default_theme; guide = false; key = ""; scope = None; imported = (fun _ -> None);
  at = (fun _ -> None); level_at = (fun _ -> None); pin_at = (fun _ _ -> None); collapsed = (fun _ -> false); lens = []; probe = (fun _ -> 0); records = None;
  chains = Hashtbl.create 1; folds = Hashtbl.create 1; counts = Hashtbl.create 1;
  frames = (fun _ -> []); display = None; framed = true;
  layout = { P.placed = []; w = 0.; h = 0. }; geo = empty_geo; geo_fs = 13; pan_x = 12.; pan_y = 12.; zoom = 1.;
  selected = Path_set.empty; selected_wire = None; panning_grab = false;
  hovered_row = None; highlighted = []; drag = None; editing = None; context = None; stats = no_stats; switches = [];
  carry_lit = []; carry_hot = None; failed = []; hinting = None; back = [];
  measure = (fun size s -> 0.6 *. float (size * String.length s)); tags = true; chain = ref None }

(* the nodes upstream and downstream of the selection (it included), along the wires; [None] with
   nothing selected.  A wire is lit when both its ends are in the same side.
   ponytail: one pass over the wires per selection or geometry change; index them if a 10k-wire graph drags. *)
let chains_of t =
  if Path_set.is_empty t.selected then None else
  match !(t.chain) with
  | Some (sel, wires, r) when sel == t.selected && wires == t.geo.wires -> Some r
  | _ ->
      let reach next =
        let seen = ref t.selected and todo = ref (Path_set.elements t.selected) in
        while !todo <> [] do
          let batch = !todo in
          todo := [];
          List.iter (fun p -> Array.iter (fun w -> match next w with
            | Some (from, into) when path_equal from p && not (Path_set.mem into !seen) ->
                seen := Path_set.add into !seen; todo := into :: !todo
            | _ -> ()) t.geo.wires) batch
        done;
        !seen in
      let up = reach (fun w -> match w.into, w.from with Some i, Some f -> Some (i, f) | _ -> None)
      and down = reach (fun w -> match w.into, w.from with Some i, Some f -> Some (f, i) | _ -> None) in
      t.chain := Some (t.selected, t.geo.wires, (up, down));
      Some (up, down)
let wire_lit (up, down) (w : wire) = match w.from, w.into with
  | Some f, Some i -> (Path_set.mem f up && Path_set.mem i up) || (Path_set.mem f down && Path_set.mem i down)
  | _ -> false
let node_lit (up, down) p = Path_set.mem p up || Path_set.mem p down

(* a wire longer than three columns reads as a name tag at its use site unless its chain is lit (flow.md 6.4) *)
let tagged t (w : wire) =
  t.tags && w.target <> None && abs_float (fst w.b -. fst w.a) > tag_span
  && not (match chains_of t with Some c -> wire_lit c w | None -> false)

let with_bounds ~x ~y ~width ~height t =
  if t.x = x && t.y = y && t.width = width && t.height = height then t
  else { t with x; y; width; height }
(* the pane is not drawn this frame: nothing it began stays open, and the pointer is given back *)
let suspend t =
  if t.panning_grab then ignore (Rays.Sketch.set_relative_mouse false);
  if t.editing = None && t.hinting = None && t.drag = None && t.context = None && not t.panning_grab && t.highlighted = [] then t
  else { t with editing = None; hinting = None; drag = None; context = None; highlighted = []; panning_grab = false }
let with_guide guide t = if t.guide = guide then t else { t with guide }
let with_theme theme t = if t.theme == theme then t else { t with theme }
let with_failed failed t = if t.failed = failed then t else { t with failed }
let with_carry ~lit ~hot t =
  if t.carry_lit = lit && t.carry_hot = hot then t else { t with carry_lit = lit; carry_hot = hot }
let selected t = Path_set.elements t.selected
let editing t = t.editing <> None || t.hinting <> None
let select paths t = { t with selected = Path_set.of_list paths; selected_wire = None }
let clear_selection t =
  if Path_set.is_empty t.selected && t.selected_wire = None then t
  else { t with selected = Path_set.empty; selected_wire = None }
let stats t = t.stats
let zoom t = t.zoom
(* a screen point as a position inside [scope], snapped to the 24-point dot lattice (where a node
   added from the menu opened there goes) *)
let scope_point t ~scope (mx, my) =
  Option.map (fun (ox, oy) ->
    let snap = P.snap in
    snap ((mx -. float t.x -. t.pan_x) /. t.zoom -. ox), snap ((my -. float t.y -. t.pan_y) /. t.zoom -. oy))
    (List.assoc_opt scope t.geo.origins)

let count_scope (s : P.scope) =
  let rec go (s : P.scope) = List.fold_left (fun (n, z, r) (node : P.node) ->
    let n', z', r' = match node.zone with Some zn -> go zn.scope | None -> 0, 0, 0 in
    n + 1 + n', z + (if node.zone <> None then 1 else 0) + z', r + List.length node.rows + r')
    (0, 0, 0) s.nodes in
  go s

let frame_all t =
  if t.layout.placed = [] then t else
  let w = Float.max 1. t.layout.w and h = Float.max 1. t.layout.h in
  let zoom = Float.max 0.25 (Float.min 1. (Float.min (float t.width /. w) (float t.height /. h))) in
  (* whole points, so the hairlines of the cards land on pixels *)
  { t with zoom; pan_x = Float.round ((float t.width -. w *. zoom) /. 2.);
    pan_y = Float.round (Float.max 8. ((float t.height -. h *. zoom) /. 2.)) }

let count_of t path = Option.value ~default:0 (Hashtbl.find_opt t.counts path)

(* the iteration counts follow the scope, the probes and what was recorded *)
let refresh t = match t.scope, t.records with
  | Some scope, Some records ->
      let counts = Hashtbl.create 16 in
      List.iter (fun (p, c) -> Hashtbl.replace counts p c) (Flow_graph.Probe.counts records scope ~probe:t.probe);
      { t with counts }
  | _ -> { t with counts = Hashtbl.create 1 }

let no_shift _ = 0., 0.
let lens_of t path = List.assoc_opt path t.lens

(* the cards reserve a footer row once the host has probe records to show *)
let lay ?(sticky = true) t scope ~at ~collapsed =
  (* the cards stay where they were (flow.md 6.1): an edit places only the nodes it adds *)
  (* a card whose saved position was dropped (an undone drag) is a full layout again *)
  let sticky = sticky && not (List.exists (fun (b : P.placed) -> t.at b.path <> None && at b.path = None) t.layout.P.placed) in
  let prev = if not sticky || t.layout.P.placed = [] then (fun _ -> None) else begin
    let h = Hashtbl.create 64 in
    List.iter (fun (b : P.placed) -> Hashtbl.replace h b.path (b.x, b.y, b.w, b.h)) t.layout.P.placed;
    Hashtbl.find_opt h end in
  P.layout ~foot:(t.records <> None) ~at ~prev ~collapsed ~lens:(lens_of t)
    ~level:(fun p -> match t.level_at p with Some (l, _) -> l | None -> P.Card) ~pin:t.pin_at scope

(* the rows a fold button is drawn on (flow.md 7.7): a row wired from one named node of its scope
   or an enclosing one, read nowhere else.  The edit checks again and says why when it refuses.
   ponytail: uses are counted by name over the whole graph, so a name shadowed in another scope
   gets no button (the context menu still folds it); count per scope if that bites. *)
let fold_sources (scope : P.scope) =
  let uses = Hashtbl.create 64 and binders = Hashtbl.create 64 and out = Hashtbl.create 16 in
  let add name =
    let k = root_name name in
    Hashtbl.replace uses k (1 + Option.value ~default:0 (Hashtbl.find_opt uses k)) in
  let rec count (s : P.scope) =
    (match s.result with P.Link target -> add target | _ -> ());
    List.iter (fun (n : P.node) ->
      if n.binds = [ n.name ] && not n.synthetic && not (E.nested n.name) then Hashtbl.replace binders n.path ();
      List.iter (fun (r : P.row) -> List.iter add (P.sources r)) n.rows;
      Option.iter (fun (z : P.zone) ->
        List.iter (fun (r : P.rail_row) -> Option.iter (fun e -> List.iter add (E.free_names e)) r.expr) z.rail;
        count z.scope) n.zone) s.nodes in
  count scope;
  let rec binder sc name = match sc with
    | [] -> None
    | _ when Hashtbl.mem binders (sc @ [ name ]) -> Some (sc @ [ name ])
    | _ -> binder (List.rev (List.tl (List.rev sc))) name in
  let rec mark (s : P.scope) =
    List.iter (fun (n : P.node) ->
      List.iteri (fun i (r : P.row) -> match r.chip, P.sources r with
        | P.Name _, [ src ] when not r.head && Hashtbl.find_opt uses src = Some 1 ->
            Option.iter (Hashtbl.replace out (n.path, i)) (binder (List.rev (List.tl (List.rev n.path))) src)
        | _ -> ()) n.rows;
      Option.iter (fun (z : P.zone) -> mark z.scope) n.zone) s.nodes in
  mark scope; out

(* a switch's rows read as the names of its layouts; [switches] are its nodes with the active input *)
let switch_nodes (scope : P.scope) = List.filter_map (fun (n : P.node) ->
  if n.head <> "ui/switch" || n.zone <> None then None else
  Some (n.path, match List.find_opt (fun (r : P.row) -> r.key = E.Kw "active") n.rows with
    | Some { expr = Some { S.node = S.Num s; _ }; _ } -> Option.value ~default:0 (int_of_string_opt s)
    | _ -> 0)) scope.nodes

let with_scope ?(at = fun _ -> None) ?(level = fun _ -> None) ?(pin = fun _ _ -> None) ?(collapsed = fun _ -> false) ?(probe = fun _ -> 0)
    ?(frames = fun _ -> []) ?(imported = fun _ -> None) ?display ?(layouts = []) ~key scope t =
  let scope = if layouts = [] then scope else
    { scope with P.nodes = List.map (fun (n : P.node) ->
        let inputs = List.filter (fun (r : P.row) -> match r.key with E.Pos _ -> r.kind <> P.Add | _ -> false) n.rows in
        if n.head <> "ui/switch" || List.length inputs <> List.length layouts then n
        else { n with rows = List.map (fun (r : P.row) -> match r.key with
          | E.Pos i when r.kind <> P.Add -> { r with label = List.nth layouts i } | _ -> r) n.rows }) scope.P.nodes } in
  let t = { t with level_at = level; pin_at = pin } in
  (* letter hints, an open context menu and the hover name nodes and wires of the scope they were made over *)
  let t = match t.scope with
    | Some previous when previous == scope -> t
    | _ -> { t with hinting = None; context = None; highlighted = [] } in
  let layout = lay ~sticky:(key = t.key) t scope ~at ~collapsed in
  let earlier = if key = t.key then t.layout.P.placed else [] in
  let n, z, r = count_scope scope in
  let t = { t with scope = Some scope; imported; at; collapsed; probe; frames; display; layout; switches = switch_nodes scope;
    chains = Flow_graph.Probe.chains scope; folds = fold_sources scope;
    stats = { t.stats with nodes = n; zones = z; rows = r } } in
  let t = regeo t scope layout ~shift:no_shift in
  (* a card an edit added (an unfolded call lands a column left of its owner) that falls outside the view
     brings the pane to it, nothing else moves *)
  let t = if earlier = [] || List.length layout.P.placed <= List.length earlier || t.width = 0 then t else begin
    let known = Hashtbl.create 64 in
    List.iter (fun (b : P.placed) -> Hashtbl.replace known b.path ()) earlier;
    match List.find_opt (fun (b : P.placed) -> not (Hashtbl.mem known b.path)) layout.P.placed with
    | None -> t
    | Some b ->
        let fit pan size lo hi = (* the pan that puts [lo, hi] (graph units) inside [0, size] with a 24-point margin *)
          if pan +. lo *. t.zoom < 8. then 24. -. lo *. t.zoom
          else if pan +. hi *. t.zoom > size -. 8. then size -. 24. -. hi *. t.zoom else pan in
        { t with pan_x = fit t.pan_x (float t.width) b.x (b.x +. b.w); pan_y = fit t.pan_y (float t.height) b.y (b.y +. b.h) }
  end in
  (* an edit that removed or moved a node drops it from the selection *)
  let t = { (refresh t) with selected = Path_set.filter (fun p ->
    Paths.mem t.geo.slot p
    (* a row of a node (the inspector's subject, [holder @ [":b"]]) lives while its holder does *)
    || (match List.rev p with
        | last :: (_ :: _ as holder) -> String.starts_with ~prefix:":" last && Paths.mem t.geo.slot (List.rev holder)
        | _ -> false)) t.selected } in
  let selected_wire = match t.selected_wire with
    | Some (sp, sk, _) when Array.exists (fun w -> match w.target with Some (tp, tk, _) -> sp = tp && sk = tk | None -> false) t.geo.wires ->
        t.selected_wire
    | _ -> None in
  let t = { t with selected_wire } in
  let t = match t.editing with
    | Some (Name p | Default p | Output p) when not (Paths.mem t.geo.slot p) -> { t with editing = None }
    | _ -> t in
  if key <> t.key then begin
    { (suspend t) with key; framed = false; selected = Path_set.empty; selected_wire = None }
  end else t

(* the panel of a macro call opened or stepped: its card changes size; [~sticky:false] (Tidy)
   puts every card back to the computed layout, dragged ones too *)
let relayout ?sticky t = match t.scope with
  | None -> t
  | Some scope ->
      let layout = lay ?sticky t scope ~at:t.at ~collapsed:t.collapsed in
      regeo { t with layout } scope layout ~shift:no_shift

let tidy t = relayout ~sticky:false t

let with_display display t = if display = t.display then t else { t with display }

let with_arguments changes t = match t.scope with
  | None -> t
  | Some scope ->
      let changes = List.filter (fun (path, _, _) -> Paths.mem t.geo.slot path) changes in
      if changes = [] then t else
      let scope = P.with_arguments changes scope in
      let nodes = Paths.create (Array.length t.geo.items) in
      let rec index (scope : P.scope) = List.iter (fun (node : P.node) ->
        Paths.replace nodes node.path node; Option.iter (fun zone -> index zone.P.scope) node.zone) scope.nodes in
      index scope;
      let places = Paths.create (Array.length t.geo.items) in
      let rec patch_layout (layout : P.layout) =
        let placed = List.map (fun (placed : P.placed) ->
          let item = match placed.item with
            | Item node -> let next = Option.value ~default:node (Paths.find_opt nodes node.path) in
                if next == node then placed.item else P.Item next
            | item -> item in
          let lines = match placed.item, item with
            | P.Item old, P.Item node when old.rows != node.rows ->
                let rows = Array.of_list node.rows in Array.map (function
                | P.Row (i, _) -> P.Row (i, rows.(i))
                | line -> line) placed.lines
            | _ -> placed.lines in
          let inner = match placed.inner with
            | Some layout -> let next = patch_layout layout in
                if next == layout then placed.inner else Some next
            | None -> None in
          let placed = if item == placed.item && lines == placed.lines && inner == placed.inner then placed
            else {placed with item; lines; inner} in
          Paths.replace places placed.path placed; placed) layout.placed in
        if List.for_all2 ( == ) placed layout.placed then layout else {layout with placed} in
      let layout = patch_layout t.layout in
      let items = Array.map (fun ((placed : P.placed), x, y) ->
        Option.value ~default:placed (Paths.find_opt places placed.path), x, y) t.geo.items in
      {t with scope = Some scope; layout; geo = {t.geo with items}}

let with_records records t =
  match t.records with
  | Some previous when Flow_graph.Probe.same_eval previous records -> { t with records = Some records }
  | Some _ -> refresh { t with records = Some records }
  | None -> relayout (refresh { t with records = Some records })

(* ------------------------------------------------------------- lookups *)

let node_of t path = Option.bind t.scope (fun s -> P.find s path)
let collapsed_zone t path = t.collapsed path && (match node_of t path with Some n -> n.zone <> None | None -> false)
let item_at t path = Option.map (fun i -> t.geo.items.(i)) (Paths.find_opt t.geo.slot path)
(* an item's box in graph units, and where it sits inside its scope *)
let box_at t path = Option.map (fun ((p : P.placed), ax, ay) -> ax, ay, p.w, p.h) (item_at t path)
let rel_at t path = Option.map (fun ((p : P.placed), _, _) -> p.x, p.y) (item_at t path)
let selected_nodes t = List.filter_map (fun p -> node_of t p) (selected t)
