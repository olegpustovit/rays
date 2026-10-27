module P = Flow_graph.Projection
module E = Flow_graph.Flow_edit
module S = Flow.Syntax
module Ty = Flow.Ty

type path = P.path

(* ------------------------------------------------------------ geometry *)

(* tables by path: the last segment is hashed (it names the node; the ones before it its scopes),
   and segments are compared as strings, so no lookup walks a path polymorphically *)
let path_equal (a : path) (b : path) = a == b || List.equal String.equal a b
module Paths = Hashtbl.Make (struct
  type t = path
  let equal = path_equal
  let rec hash : t -> int = function [] -> 0 | [ leaf ] -> Hashtbl.hash leaf | _ :: rest -> hash rest
end)

(* bucket grids are keyed by cell *)
module Cells = Hashtbl.Make (Int)

(* a cell's key: one int (cells are far fewer than 32768 high) *)
let cell_key cx cy = cx * 65536 + cy
let tag_span = 3. *. 288.

(* ---- what is in view: a bucket grid over boxes (the tiles, the wires' bounds), built once per
   geometry, so a frame asks the cells its viewport covers and not every node of the graph ---- *)

type index = { buckets : int list Cells.t; large : int list }  (* a box over many cells is asked every time *)

let index_cell = 256.
let index_of n box =
  let buckets = Cells.create 64 and large = ref [] in
  let c v = int_of_float (Float.floor (v /. index_cell)) in
  for i = n - 1 downto 0 do
    let x0, y0, x1, y1 = box i in
    if (c x1 - c x0 + 1) * (c y1 - c y0 + 1) > 64 then large := i :: !large
    else
      for cx = c x0 to c x1 do
        for cy = c y0 to c y1 do
          let key = cell_key cx cy in
          Cells.replace buckets key (i :: Option.value ~default:[] (Cells.find_opt buckets key))
        done
      done
  done;
  { buckets; large = !large }

(* the boxes that may meet the rectangle, in index order, each once *)
let index_query ix (x0, y0, x1, y1) =
  let c v = int_of_float (Float.floor (v /. index_cell)) in
  let found = ref ix.large in
  if (c x1 - c x0 + 1) * (c y1 - c y0 + 1) >= Cells.length ix.buckets then
    Cells.iter (fun _ l -> found := List.rev_append l !found) ix.buckets
  else
    for cx = c x0 to c x1 do
      for cy = c y0 to c y1 do
        match Cells.find_opt ix.buckets (cell_key cx cy) with
        | Some l -> found := List.rev_append l !found
        | None -> ()
      done
    done;
  List.sort_uniq Int.compare !found

type src = { pos : float * float; ty : Ty.t option; owner : path option }
type wire = {
  a : float * float; b : float * float; ty : Ty.t option;
  target : (path * E.arg_key * S.t option) option;
  pts : (float * float) list;  (* the drawn polyline, [a] .. [b]: straight, or bent clear of the cards *)
  refers : bool;  (* a name read inside a formula, not the value a row takes: drawn faint until lit *)
  from : path option;  (* the node (or zone) it reads from, and the one it ends at: the dataflow chains *)
  into : path option;
}

type geo = {
  items : (P.placed * float * float) array;  (* absolute top-left, graph units *)
  wires : wire array;
  origins : (path * (float * float)) list;  (* where each scope's own coordinates start *)
  inside : (path, int list) Hashtbl.t;  (* the wires that end at a node of a zone's body, by the zone's path *)
  slot : int Paths.t;  (* an item's place in [items]: its box and its position inside its scope come from there *)
  tiles : index;  (* over the items' boxes *)
  reach : index;  (* over the wires' bounds *)
  read : (float * float, unit) Hashtbl.t;  (* the points a wire starts from: an out-port there is wired *)
}

let empty_geo = { items = [||]; wires = [||]; origins = [];
                  inside = Hashtbl.create 1; slot = Paths.create 1; tiles = { buckets = Cells.create 1; large = [] };
                  reach = { buckets = Cells.create 1; large = [] }; read = Hashtbl.create 1 }
let root_name s = match String.index_opt s '.' with Some i -> String.sub s 0 i | None -> s
(* what a row reads once its wire is taken off: a named argument goes back to its schema default,
   written so the row stays on the card; a positional one takes a value of its type; an item of a
   variadic input goes (nothing is written in its place) *)
let fallback (r : P.row) = match r.key with
  | _ when r.kind = P.Rest -> None
  | E.Kw _ | E.Field _ ->
      Option.bind r.default (fun d -> match Flow.Syntax.parse d with Ok [ form ] -> Some form | _ -> None)
  | _ -> Option.bind r.ty (fun ty -> E.default_for ty r.label)

(* the sheet's wire ends: a header port's wire runs 1.5 points from y + 11, a row port's from the
   row's centre line (kit [.wire], measured on graph@2x.png) *)
let wire_head_y = 11.75
let wire_row_y = 12.75

(* a loop's begin and end bars (Houdini's block begin/end): hairlines 20 points inside its edges.
   Each variable's out-port sits on the begin bar, level with its name: a rail row's on that row,
   the label's own variable in the air under the rails, named beside it.  The body's result comes in
   on the end bar at the first card row.  A plain [for] keeps the sheet's label port.  Tile-relative. *)
let bar_x = 20.5
let begin_port (n : P.node) (z : P.zone) (r : P.rail_row) =
  let label = P.label_row z in
  if z.kind = P.For && (match label with Some l -> l == r | None -> false) then 12., 36. else
  match List.find_index (fun x -> x == r) (P.extra_rails z) with
  | Some k -> bar_x, P.head_height +. 24. *. float k +. 12.
  | None -> bar_x, P.rail_top n -. 14.
let end_port (n : P.node) ~w = w -. bar_x, P.rail_top n +. 12.

let note_rows (n : P.node) = if n.note <> None then 1. else 0.
let rows_top (n : P.node) ay = ay +. P.body_top +. note_rows n *. P.row_height

(* where the rows are: the line each row of a card sits on, by row index *)
let line_of_row (lines : P.line array) =
  let tbl = Hashtbl.create 8 in
  Array.iteri (fun k -> function P.Row (i, _) -> Hashtbl.replace tbl i k | _ -> ()) lines;
  tbl

(* the part of a segment inside a rectangle, as the parameters of its two ends (Liang-Barsky);
   [t0 > t1] when it misses. Painting and the hit boxes cut a wire to what is shown (the GPU's diagonal line ignores the
   clip of the box that paints it). *)
let clip_span x0 y0 x1 y1 ~xmin ~ymin ~xmax ~ymax =
  let dx = x1 -. x0 and dy = y1 -. y0 in
  let t0 = ref 0. and t1 = ref 1. and ok = ref true in
  let clip p q =
    if p = 0. then (if q < 0. then ok := false)
    else begin
      let r = q /. p in
      if p < 0. then (if r > !t1 then ok := false else if r > !t0 then t0 := r)
      else if r < !t0 then ok := false else if r < !t1 then t1 := r
    end in
  clip (-. dx) (x0 -. xmin); clip dx (xmax -. x0); clip (-. dy) (y0 -. ymin); clip dy (ymax -. y0);
  if !ok then !t0, !t1 else 1., 0.

let clip_segment (x0, y0) (x1, y1) (rx, ry, rw, rh) =
  let t0, t1 = clip_span x0 y0 x1 y1 ~xmin:rx ~ymin:ry ~xmax:(rx +. rw) ~ymax:(ry +. rh) in
  if t0 <= t1 then Some ((x0 +. (x1 -. x0) *. t0, y0 +. (y1 -. y0) *. t0), (x0 +. (x1 -. x0) *. t1, y0 +. (y1 -. y0) *. t1))
  else None


(* A wire is a 12-point lead out of and into each port, as the kit's port stubs, and a
   cubic between them whose pull is capped, so a long wire stays taut.  With no room for the leads
   (a begin bar's wire to the card beside it) it is a gentle S; a wire running back swings out by
   its height.  Sampled as the polyline the pane paints and picks. *)
let curve_points ?(drop = false) (ax, ay) (bx, by) =
  (* from a begin bar's port: down along the bar, then level into the row, clear of the port's name *)
  if drop && by > ay +. 12. && bx > ax then
    let k = Float.min (by -. ay) (bx -. ax) in
    List.init 17 (fun i ->
      let t = float i /. 16. in
      let u = 1. -. t in
      let b0 = u *. u *. u and b1 = 3. *. u *. u *. t and b2 = 3. *. u *. t *. t and b3 = t *. t *. t in
      (b0 +. b1) *. ax +. b2 *. (bx -. k) +. b3 *. bx, b0 *. ay +. b1 *. (ay +. k) +. (b2 +. b3) *. by)
  else
  let dx = bx -. ax in
  let lead = if Float.abs dx < 24. then 0. else 12. in
  let x0 = ax +. lead and x1 = bx -. lead in
  let k = if dx >= 24. then Float.min 96. (Float.max 12. ((x1 -. x0) /. 2.))
    else if dx > -24. then 12.
    else Float.min 160. (Float.max 48. (Float.abs (by -. ay) /. 2.)) in
  let cubic = List.init 17 (fun i ->
    let t = float i /. 16. in
    let u = 1. -. t in
    let b0 = u *. u *. u and b1 = 3. *. u *. u *. t and b2 = 3. *. u *. t *. t and b3 = t *. t *. t in
    b0 *. x0 +. b1 *. (x0 +. k) +. b2 *. (x1 -. k) +. b3 *. x1, (b0 +. b1) *. ay +. (b2 +. b3) *. by) in
  if lead = 0. then cubic else (ax, ay) :: cubic @ [ (bx, by) ]

(* where a card's out-port wire starts, a node's rows are read and a zone's rail rows end *)
let out_anchor (p : P.placed) ax ay =
  if p.shown = P.Point then ax +. p.w, ay +. P.point_size /. 2. else ax +. p.w, ay +. wire_head_y
let in_anchor (p : P.placed) ax ay =
  if p.shown = P.Point then ax, ay +. P.point_size /. 2. else ax, ay +. wire_head_y

(* the level a card is drawn at: the one it was given, at every zoom.  A zone, the return card
   and a frame keep their card. *)
let shown_of (p : P.placed) = match p.item with
  | P.Item { zone = Some _; _ } | P.Return -> P.Card
  | P.Item _ | P.Input _ -> p.level

(* a point's box: the disc and its name; its wires meet the disc on the left and the name's end on
   the right *)
let point_name (p : P.placed) = match p.item with
  | P.Item n -> P.point_title n | P.Input i -> i.name | P.Return -> "return"
(* [name_w] is the name's real width in graph units at the font the zoom draws it in *)
let point_box_w ~name_w name = P.point_size +. 8. +. name_w name

(* [shift] moves a placed item by its path (a drag in progress); a zone's
   children follow through the origin they are laid out from. *)
let compute ~name_w ~shown (scope : P.scope) (layout : P.layout) ~shift =
  let items = ref [] and count = ref 0 and slot = Paths.create 64 in
  let origins = ref [ (scope.path, (0., 0.)) ] in
  let rec walk ox oy (l : P.layout) =
    List.iter (fun (p : P.placed) ->
      let dx, dy = shift p.path in
      let ax = ox +. p.x +. dx and ay = oy +. p.y +. dy in
      (* one geometry per node at the level it is shown: the box, the ports and the wires' ends *)
      let lv = shown p in
      let p, ay = match lv with
        | P.Point -> { p with shown = lv; w = point_box_w ~name_w (point_name p); h = P.point_size },
                     if p.level = P.Point && (match p.item with P.Input _ -> false | _ -> true)
                     then ay else ay +. 5.
        | P.Chip -> { p with shown = lv; h = P.head_height }, ay
        | _ -> { p with shown = lv }, ay in
      Paths.replace slot p.path !count; incr count;
      items := (p, ax, ay) :: !items;
      match p.inner, p.item with
      | Some inner, P.Item n ->
          origins := (n.path, (ax +. P.zone_pad_x, ay +. P.rail_top n)) :: !origins;
          walk (ax +. P.zone_pad_x) (ay +. P.rail_top n) inner
      | _ -> ()) l.placed in
  walk 0. 0. layout;
  let items_list = List.rev !items in
  let wires = ref [] in
  (* the begin bars' ports: their wires leave down the bar *)
  let bars = Hashtbl.create 8 in
  let items = Array.of_list items_list in
  let at path : (P.placed * float * float) option = Option.map (fun i -> items.(i)) (Paths.find_opt slot path) in
  (* a card or function stacked above the card it feeds (Projection.layout's chain column) is
     joined to it by a spine: straight down from its bottom, along the card's left edge, into the
     input's socket, not a wire out of its right side and back across it *)
  let spine (s : src) into (bx, by) = match s.owner, into with
    | Some o, [ i ] ->
        (match at o, at i with
         | Some ((op : P.placed), ox, oy), Some (_, _, iy)
           when bx >= ox -. 1. && bx <= ox +. op.w -. 24. && iy >= oy +. op.h
                && iy -. (oy +. op.h) <= 3. *. P.lattice ->
             (* into the first row along the edge; lower, 12 points out, clear of the sockets above,
                and round into the row *)
             if by <= iy +. P.body_top +. P.row_height then Some [ (bx, oy +. op.h); (bx, by) ]
             else let x = bx -. 12. in
               Some [ (x, oy +. op.h); (x, by -. 6.); (x +. 1.8, by -. 1.8); (x +. 6., by); (bx, by) ]
         | _ -> None)
    | _ -> None in
  let wire ?target ?(refers = false) (s : src option) b ~into =
    match s, Option.bind s (fun s -> spine s into b) with
    | Some s, Some pts ->
        let a = List.hd pts and b = List.nth pts (List.length pts - 1) in
        wires := { a; b; ty = s.ty; target; pts; refers; from = s.owner; into = (match into with i :: _ -> Some i | [] -> None) } :: !wires
    | Some s, None ->
        let pts = curve_points ~drop:(Hashtbl.mem bars s.pos) s.pos b in
        wires := { a = s.pos; b; ty = s.ty; target; pts; refers; from = s.owner; into = (match into with i :: _ -> Some i | [] -> None) } :: !wires
    | None, _ -> () in
  let resolve chain name =
    List.find_map (fun tbl -> match Hashtbl.find_opt tbl name with
      | Some s -> Some s | None -> Hashtbl.find_opt tbl (root_name name)) chain in
  let names_of (e : S.t option) = match e with Some e -> E.free_names e | None -> [] in
  (* a plain [for] has no out-port of its own: what it yields wires from the yielded card *)
  let inner_out (n : P.node) = match n.zone with
    | Some ({ kind = P.For; _ } as z) when (match at n.path with Some (p, _, _) -> not p.collapsed | None -> true) ->
        let target = match z.scope.result with
          | P.Link l -> List.find_opt (fun (m : P.node) -> List.mem (root_name l) m.binds) z.scope.nodes
                        |> Option.map (fun (m : P.node) -> m.path)
          | P.Node p -> Some p | P.Literal _ -> None in
        Option.bind target (fun path -> Option.map (fun (p, ax, ay) -> out_anchor p ax ay, path) (at path))
    | _ -> None in
  let declared (s : P.scope) =
    let tbl = Hashtbl.create 16 in
    List.iter (fun (i : P.input) -> match at i.path with
      | Some (p, ax, ay) ->
          Hashtbl.replace tbl i.name { pos = out_anchor p ax ay; ty = Some i.ty; owner = Some i.path }
      | _ -> ()) s.inputs;
    List.iter (fun (n : P.node) -> match at n.path with
      | Some (p, ax, ay) ->
          let w = p.w in
          let lines = Array.length p.lines in
          let main, main_owner = match inner_out n with
            | Some (pos, owner) -> pos, owner
            | None -> out_anchor p ax ay, n.path in
          (* a point or a chip has the one out end; its named outputs are listed on a card *)
          let out j = if p.shown = P.Point || p.shown = P.Chip then main
            else (ax +. w, rows_top n ay +. (float (lines + j) +. 0.5) *. P.row_height +. 0.75) in
          (match n.binds with
           | [ b ] when b = n.name ->
               Hashtbl.replace tbl b { pos = main; ty = Some n.ty; owner = Some main_owner };
               List.iteri (fun j (f, ty) ->
                 Hashtbl.replace tbl (b ^ "." ^ f) { pos = out j; ty = Some ty; owner = Some n.path }) n.outputs
           | _ -> List.iteri (fun j (f, ty) ->
               Hashtbl.replace tbl f { pos = out j; ty = Some ty; owner = Some n.path }) n.outputs)
      | _ -> ()) s.nodes;
    tbl in
  let rec scope_wires chain (s : P.scope) =
    let chain = declared s :: chain in
    List.iter (fun (n : P.node) -> match at n.path with
      | Some (p, ax, ay) ->
          let w = p.w in
          (match n.zone with
           | None ->
               let lines = line_of_row p.lines in
               List.iteri (fun i (r : P.row) ->
                 let target_pos =
                   if r.head then in_anchor p ax ay
                   else match p.shown with
                     | P.Point | P.Chip -> in_anchor p ax ay
                     | _ ->
                         (match Hashtbl.find_opt lines i with
                          | Some k -> ax, rows_top n ay +. float k *. P.row_height +. wire_row_y
                          | None -> ax, ay +. wire_head_y) in
                 (* a binding cannot read itself: a name that resolves to this node is one from outside
                    its scope (a [ref] to a graph of the same name), not a wire *)
                 (match P.sources r with
                  | [] -> ()
                  | names ->
                      (* the fallback parses the row's default: only for a row that is wired *)
                      let target = Some (n.path, r.key, fallback r) in
                      List.iter (fun name -> match resolve chain name with
                        | Some { owner = Some o; _ } when o = n.path -> ()
                        | src -> wire ?target ~refers:(match r.chip with P.Inline _ -> true | _ -> false) src target_pos ~into:[ n.path ]) names)) n.rows
           | Some z ->
               let collapsed = p.collapsed in
               let label = P.label_row z in
               let extras = P.extra_rails z in
               let rail_pos (r : P.rail_row) =
                 if collapsed then
                   let i = Option.value ~default:0 (List.find_index (fun x -> x == r) z.rail) in
                   ax, ay +. P.body_top +. float i *. P.row_height +. wire_row_y
                 else match label with
                   | Some l when l == r -> ax, ay +. wire_head_y
                   | _ ->
                       let i = Option.value ~default:0 (List.find_index (fun x -> x == r) extras) in
                       ax, ay +. P.head_height +. float i *. P.row_height +. wire_row_y in
               List.iter (fun (r : P.rail_row) ->
                 let plain_label = not collapsed && z.kind = P.For && (match label with Some l -> l == r | None -> false) in
                 if (collapsed || r.role <> P.Capture) && not plain_label then begin
                   let names = if r.role = P.Capture then [ r.name ] else names_of r.expr in
                   let target = match r.key with Some key -> Some (n.path, key, None) | None -> None in
                   List.iter (fun name -> wire ?target (resolve chain name) (rail_pos r) ~into:[ n.path ]) names
                 end) z.rail;
               if not collapsed then begin
                 (* the loop's variables: a port in the zone's left strip (the label's), or at the
                    right of the rail row *)
                 let rail = Hashtbl.create 8 in
                 List.iter (fun (r : P.rail_row) ->
                   if r.role <> P.Capture then begin
                     let pos = let bx, by = begin_port n z r in ax +. bx, ay +. by in
                     let s = { pos; ty = r.ty; owner = Some n.path } in
                     Hashtbl.replace bars pos ();
                     List.iter (fun name -> Hashtbl.replace rail name s) r.names
                   end) z.rail;
                 let inner = rail :: chain in
                 scope_wires inner z.scope;
                 let inner_chain = declared z.scope :: inner in
                 let source = match z.scope.result with
                   | P.Link l -> resolve inner_chain l
                   | P.Node _ -> resolve inner_chain "@result"
                   | P.Literal _ -> None in
                 (* the body's result ends at the end bar; a fold's goes round to its accumulator
                    from there, which the bar says, so no wire runs back across the body *)
                 if z.kind <> P.For then wire source (let ex, ey = end_port n ~w in ax +. ex, ay +. ey) ~into:[ n.path ]
               end)
      | _ -> ()) s.nodes in
  scope_wires [] scope;
  (match scope.inputs with
   | [] -> ()
   | _ ->
       let chain = [ declared scope ] in
       (match at (scope.path @ [ "@return" ]) with
        | Some (_, ax, ay) ->
            let source = match scope.result with
              | P.Link l -> resolve chain l
              | P.Node _ -> resolve chain "@result"
              | P.Literal _ -> None in
            let target = (ax, ay +. P.body_top +. wire_row_y) in
            wire source target ~into:[ scope.path @ [ "@return" ] ]
        | None -> ()));
  let wires = Array.of_list (List.rev !wires) in
  let inside = Hashtbl.create 16 in
  Array.iteri (fun i w -> match w.target with
    | Some (_ :: _ :: _ :: _ as path, _, _) ->
        let zone = List.rev (List.tl (List.rev path)) in
        Hashtbl.replace inside zone (i :: Option.value ~default:[] (Hashtbl.find_opt inside zone))
    | _ -> ()) wires;
  let tiles = index_of (Array.length items) (fun i -> let (p : P.placed), ax, ay = items.(i) in ax, ay, ax +. p.w, ay +. p.h) in
  let reach = index_of (Array.length wires) (fun i ->
    List.fold_left (fun (x0, y0, x1, y1) (x, y) -> Float.min x0 x, Float.min y0 y, Float.max x1 x, Float.max y1 y)
      (infinity, infinity, neg_infinity, neg_infinity) wires.(i).pts) in
  let read = Hashtbl.create (max 16 (Array.length wires)) in
  Array.iter (fun (w : wire) -> Hashtbl.replace read w.a ()) wires;
  { items; wires; origins = !origins; inside; slot; tiles; reach; read }
