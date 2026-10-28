(* W4 part A: the graph pane's projection of every fixture (nodes, zones,
   rows, chips), liveness and invariance flags, and the layout. *)
open Flow_graph
module P = Projection
module S = Flow.Syntax
module E = Flow_edit

let fail message = failwith ("test_projection: " ^ message)
let check condition message = if not condition then fail message
let read path = In_channel.with_open_bin path In_channel.input_all
let cases = "../specification/workspace/cases"
let catalog = Editor_document.Kinds.catalog ~version:1 Sop.Nodes.factories |> Result.get_ok
let fixtures = [ "bloom"; "facade"; "garland"; "kit"; "orrery"; "rosette";
                 "sunflower"; "tiles"; "tree"; "tunnel"; "variations"; "wave" ]

let workspace_of text = match S.parse text with
  | Error d -> fail (Diagnostic.to_string d)
  | Ok forms -> (match Flow.Workspace.check catalog forms with
      | Some w, _ -> w
      | None, ds -> fail (String.concat "; " (List.map Diagnostic.to_string ds)))
let load name = workspace_of (read (Filename.concat cases (name ^ ".lisp")))

let () =
  let text = "(workspace noise (graph g :context value (let* [field (noise3 [t 0.7 -0.2])] field)))" in
  let document = Editor_document.Workspace_doc.of_text catalog text |> Result.get_ok in
  let checked = document.checked in
  let card = P.find (P.of_graph catalog checked "g") ["g"; "field"] |> Option.get in
  check (card.ty = Flow.Ty.Float && List.length card.rows = 5) "noise3 position, seed, octaves, lacunarity and gain must be editable rows";
  let expr = S.parse "[0.3 0.7 -0.2]" |> Result.get_ok |> List.hd in
  let edited = Editor_document.Workspace_doc.edit catalog document
    (E.Set_arg {node = ["g"; "field"]; key = E.Pos 0; sub = []; value = expr}) |> Result.get_ok in
  let value = Flow.Eval.run ~time:7. edited.checked |> Result.get_ok in
  check (match List.assoc "g" value.results with Flow.Eval.Float x -> x >= 0. && x <= 1. | _ -> false)
    "noise3 row edit did not evaluate";
  ignore (Flow_sop.Lower.workspace ~factories:Sop.Nodes.factories document.source |> Result.get_ok);
  let reloaded = Editor_document.Workspace_doc.of_text catalog
    (Editor_document.Workspace_doc.to_text edited) |> Result.get_ok in
  check (Flow.Eval.run ~time:7. reloaded.checked = Ok value) "noise3 was lost on document reload"

let scope w g = P.of_graph catalog w g
let node w g path = match P.find (scope w g) (g :: path) with
  | Some n -> n | None -> fail (String.concat "/" (g :: path) ^ " is not projected")
let zone (n : P.node) = match n.zone with Some z -> z | None -> fail (n.name ^ " is not a zone")
let flat = function Some e -> Flow.Lisp.flat e | None -> ""

(* every node, zone and row of every graph, as (graph, nodes, zones, rows) *)
let rec counts (s : P.scope) =
  List.fold_left (fun (n, z, r) (node : P.node) ->
    let n', z', r' = match node.zone with Some zn -> counts zn.scope | None -> 0, 0, 0 in
    n + 1 + n', z + (if node.zone <> None then 1 else 0) + z', r + List.length node.rows + r')
    (0, 0, 0) s.nodes

let snapshot = [
  "bloom", [ "flower", (12, 1, 84); "scene", (6, 0, 60); "world", (2, 0, 11); "settings", (2, 0, 7);
             "editor", (10, 0, 29); "half", (1, 0, 2); "petal", (2, 0, 53) ];
  "facade", [ "facade", (14, 2, 89) ];
  "garland", [ "garland", (19, 5, 90); "ring", (6, 1, 45) ]; (* inline functions and nested maps *)
  "kit", [ "kit", (16, 2, 112); "window", (3, 0, 30) ];
  "orrery", [ "orrery", (20, 1, 117) ]; (* conditional arms are inputs, not scopes *)
  "rosette", [ "rosette", (4, 0, 32) ];
  "sunflower", [ "sunflower", (9, 1, 36) ];
  "tiles", [ "tiles", (10, 1, 70) ];
  "tree", [ "tree", (9, 1, 184) ];
  "tunnel", [ "rings", (4, 1, 61) ];
  "variations", [ "garden", (6, 0, 70); "scene", (1, 0, 9); "editor", (8, 0, 22) ];
  "wave", [ "wave", (7, 2, 46) ];
]

let snapshots () =
  List.iter (fun (name, expected) ->
    let w = load name in
    let got = List.map (fun (g : Flow.Workspace.graph) -> g.name, counts (scope w g.name)) (w.graphs @ w.defs) in
    check (got = expected) (name ^ ": node, zone and row counts changed: " ^ String.concat "; "
      (List.map (fun (g, (n, z, r)) -> Printf.sprintf "%S, (%d, %d, %d)" g n z r) got)))
    snapshot

let kinds (n : P.node) = List.map (fun (r : P.row) -> r.label, r.kind) n.rows

let rows () =
  (* a keyword written before a positional argument: the input is still input 0, and wired *)
  let first = node (workspace_of "(workspace w (graph g :context sop (let* [a (sop/box) b (sop/transform :translate [1 2 3] a)] b)))") "g" [ "b" ] in
  (match List.find_opt (fun (r : P.row) -> r.key = E.Pos 0) first.rows with
   | Some r -> check (flat r.expr = "a" && P.sources r = [ "a" ]) "a keyword-first call lost its input"
   | None -> fail "a keyword-first call has no input row");
  check (List.exists (fun (r : P.row) -> r.key = E.Kw "translate" && flat r.expr = "[1 2 3]") first.rows)
    "a keyword-first call lost its keyword";
  let w = load "bloom" in
  (* a zone: rail with the loop variable, captures, and the yield *)
  let ring = node w "flower" [ "ring" ] in
  let z = zone ring in
  check (z.kind = P.For && z.yield_label = "collect" && ring.head = "for") "ring is a for zone";
  check (List.map (fun (r : P.rail_row) -> r.name, r.role) z.rail = [ "i", P.Var; "petals", P.Capture; "seed", P.Capture ])
    "ring rail: the variable, then the names it captures";
  let i = List.hd z.rail in
  check (i.key = Some (E.Bv (1, 1)) && flat i.expr = "(range petals)" && i.ty = Some Flow.Ty.Int)
    "the loop variable edits its collection through Bv";
  (* the four bindings, the two calls nested in them, and the result last *)
  check (List.length z.scope.nodes = 7
         && (List.nth z.scope.nodes 6).synthetic && z.scope.result = P.Node [ "flower"; "ring"; "@result" ])
    "the body's result is a synthetic node";
  let names = List.map (fun (n : P.node) -> n.name) z.scope.nodes in
  check (List.mem "wobble#1" names && List.mem "tint#:color" names) "a call written in an input is a node";
  let hsv = node w "flower" [ "ring"; "tint#:color" ] in
  check (P.anonymous hsv && P.title hsv = "hsv" && hsv.head = "value/hsv" && not hsv.synthetic)
    "a nested node is titled by its kind";
  (* a call: a rest slot with its add row, chips *)
  let bloom = node w "flower" [ "bloom" ] in
  check (kinds bloom = [ "input", P.Rest; "+ input", P.Add; "source_attribute", P.Arg; "source_base", P.Arg ])
    "merge is one rest slot, an add row, then its parameters";
  (match bloom.rows with
   | a :: add :: _ ->
       check (a.key = E.Pos 0 && a.chip = P.Name "ring" && a.ty = Some Flow.Ty.geometry) "merge input wire";
       check (add.key = E.Pos 1 && add.chip = P.No_value) "the add row goes one past the last input"
   | _ -> fail "merge rows");
  let tint = node w "flower" [ "ring"; "tint" ] in
  check (List.map (fun (r : P.row) -> r.label) tint.rows = [ "in0"; "group"; "owner"; "color"; "alpha" ])
    "set_color rows: the slot, then the parameters";
  let color = List.find (fun (r : P.row) -> r.label = "color") tint.rows in
  check (color.key = E.Kw "color" && color.chip = P.Name "tint#:color" && P.sources color = [ "tint#:color" ])
    "the row a nested node is written in is wired from it";
  let group = List.find (fun (r : P.row) -> r.label = "group") tint.rows in
  check (group.kind = P.Group_reader && group.chip = P.No_value && group.default = Some "") "group reader row";
  let leaf = node w "flower" [ "ring"; "leaf" ] in
  check (leaf.head = "petal" && List.map (fun (r : P.row) -> r.key) leaf.rows = [ E.Kw "length"; E.Kw "width" ]
         && (List.hd leaf.rows).default = Some "1.0" && (List.hd leaf.rows).ty = Some Flow.Ty.Float)
    "a defn call lists its inputs with their defaults";
  (* graph inputs, and the root scope *)
  let s = scope w "flower" in
  check (List.map (fun (i : P.input) -> i.name, flat i.default) s.inputs = [ "petals", "12"; "seed", "7" ]
         && s.result = P.Link "result") "root inputs and result link";
  (* macros: holes and binders *)
  let r = load "rosette" in
  let outer = node r "rosette" [ "outer" ] in
  check (outer.macro = Some "radial" && kinds outer = [ "i", P.Binder; "n", P.Hole; "body", P.Hole ]) "macro call rows";
  let outer_ = List.hd outer.rows in
  check (outer_.chip = P.Name "k") "the caller supplies the binder's name";
  check ((node r "rosette" [ "soft" ]).bypass && not outer.bypass) "bypass flag";
  (* records: output rows, patterns *)
  let k = load "kit" in
  let window = node k "kit" [ "big" ] in
  check (List.map fst window.outputs = [ "frame"; "pane"; "area" ]) "a values result has output rows";
  let pat = node k "kit" [ "[left mid right]" ] in
  check (List.map fst pat.outputs = [ "left"; "mid"; "right" ] && pat.binds = [ "left"; "mid"; "right" ]) "destructuring outputs";
  let tower = node k "kit" [ "tower" ] in
  let acc = List.hd (zone tower).rail in
  check ((zone tower).kind = P.Fold && acc.role = P.Acc && acc.key = Some (E.Bv (1, 1))
         && (zone tower).yield_label = "next") "fold accumulator row";
  let result = node k "kit" [ "tower"; "@result" ] in
  check (result.head = "record" && List.exists (fun (r : P.row) -> r.kind = P.Add && r.key = E.Field "") result.rows
         && List.exists (fun (r : P.row) -> r.key = E.Field "shape") result.rows) "record rows are fields plus an add row";
  let part = node k "kit" [ "tower"; "part" ] in
  check (part.head = "cond" && List.mem ("else", P.Arg) (kinds part)) "cond rows";
  (* an arm is an input of its conditional: a name there is a wired row, not a zone *)
  check (List.map (fun (r : P.row) -> P.sources r) (List.filter (fun (r : P.row) ->
      match r.key with E.Arm _ -> true | _ -> false) part.rows)
    = [ [ "big" ]; [ "small" ]; [ "small" ] ]
    && P.find (scope k "kit") [ "kit"; "tower"; "part#then" ] = None) "kit cond arms are rows of the cond card";
  let style = zone (node k "kit" [ "style" ]) in
  check (style.kind = P.Fn && style.yield_label = "return"
         && (List.hd style.rail).role = P.Param) "a bound fn is a zone with a param rail";
  (* let* scopes are zones too *)
  let f = load "facade" in
  let marked = node f "facade" [ "marked" ] in
  check ((zone marked).kind = P.Let && (zone marked).yield_label = "result"
         && List.map (fun (r : P.rail_row) -> r.name, r.role) (zone marked).rail = [ "glass", P.Capture ])
    "a nested let* is a zone that captures";
  (* a note travels with its binding *)
  let g = load "garland" in
  check ((node g "garland" [ "beads" ]).note = None) "no note";
  (match Flow.Syntax.parse "(workspace w (graph g :context sop (let* [; a note\n a (sop/box)] a)))" with
   | Ok forms ->
       let w = match Flow.Workspace.check catalog forms with Some w, _ -> w | _ -> fail "note" in
       check ((node w "g" [ "a" ]).note = Some "a note") "note carried"
   | Error _ -> fail "parse")

let live_names (s : P.scope) =
  let rec go (s : P.scope) = List.concat_map (fun (n : P.node) ->
    (if n.live then [ n.name ] else []) @ (match n.zone with Some z -> go z.scope | None -> [])) s.nodes in
  go s

let rec invariant_names (s : P.scope) = List.concat_map (fun (n : P.node) ->
  (if n.invariant then [ n.name ] else []) @ (match n.zone with Some z -> invariant_names z.scope | None -> [])) s.nodes

let flags () =
  let w = load "orrery" in
  let s = scope w "orrery" in
  check (live_names s = [ "spin"; "pulse"; "sun"; "glow#:color"; "glow"; "moons_each"; "a"; "bob"; "moon#:center"; "moon"; "@result#:color"; "@result"; "orbit"; "system" ])
    ("Orrery: live paths changed: " ^ String.concat ", " (live_names s));
  let base = node w "orrery" [ "base" ] and moons = node w "orrery" [ "moons_each" ] in
  check ((not base.live) && moons.live && (zone moons).scope.nodes <> []) "a live zone";
  List.iter (fun name ->
    let w = load name in
    List.iter (fun (g : Flow.Workspace.graph) ->
      if name <> "wave" then check (live_names (scope w g.name) = [])
        (name ^ "/" ^ g.name ^ " has no live node")) (w.graphs @ w.defs)) [ "sunflower"; "tree"; "tiles"; "bloom" ];
  check (live_names (scope (load "wave") "wave") <> []) "Wave is live";
  check (invariant_names (scope (load "sunflower") "sunflower") = [ "turn" ]) "Sunflower: turn is the same each seed";
  check (invariant_names (scope (load "tree") "tree") = [ "up" ]) "Tree: up is the same each level"

(* ---- layout ---- *)

(* absolute (x, y, w, h) of every placed item, zones' children included, outer first *)
let place (l : P.layout) =
  let rec go ox oy (l : P.layout) = List.concat_map (fun (p : P.placed) ->
    let ax = ox +. p.x and ay = oy +. p.y in
    (p.path, (ax, ay, p.w, p.h))
    :: (match p.inner, p.item with
        | Some inner, P.Item n -> go (ax +. P.zone_pad_x) (ay +. P.rail_top n) inner
        | _ -> [])) l.placed in
  go 0. 0. l

let overlaps (ax, ay, aw, ah) (bx, by, bw, bh) = ax < bx +. bw && bx < ax +. aw && ay < by +. bh && by < ay +. ah
let inside (px, py, pw, ph) (x, y, w, h) = x >= px && y >= py && x +. w <= px +. pw && y +. h <= py +. ph

let rec check_layout name (l : P.layout) =
  let rec pairs = function [] -> [] | x :: r -> List.map (fun y -> x, y) r @ pairs r in
  List.iter (fun ((a : P.placed), (b : P.placed)) ->
    check (not (overlaps (a.x, a.y, a.w, a.h) (b.x, b.y, b.w, b.h)))
      (Printf.sprintf "%s: %s overlaps %s" name (String.concat "/" a.path) (String.concat "/" b.path))) (pairs l.placed);
  List.iter (fun (p : P.placed) ->
    check (inside (0., 0., l.w, l.h) (p.x, p.y, p.w, p.h)) (name ^ ": a node is outside its scope");
    Option.iter (fun (inner : P.layout) ->
      (* the zone's body has room for its whole inner layout, between its padding *)
      check (P.zone_pad_x +. inner.w +. P.zone_pad_x <= p.w && P.rail_top (match p.item with P.Item n -> n | _ -> fail "item") +. inner.h <= p.h)
        (name ^ ": a zone is smaller than its body");
      check_layout name inner) p.inner) l.placed

let layout () =
  List.iter (fun name ->
    let w = load name in
    List.iter (fun (g : Flow.Workspace.graph) ->
      let s = scope w g.name in
      let l = P.layout s in
      check_layout (name ^ "/" ^ g.name) l;
      check (P.layout s = l) (name ^ "/" ^ g.name ^ ": layout is deterministic");
      (* absolute boxes: every zone child lies inside its zone *)
      let abs = place l in
      List.iter (fun (z : P.node) ->
        let zb = List.assoc z.path abs in
        let zn = zone z in
        List.iter (fun (c : P.node) ->
          match List.assoc_opt c.path abs with
          | Some cb -> check (inside zb cb) (name ^ ": " ^ String.concat "/" c.path ^ " leaves its zone")
          | None -> fail ("unplaced " ^ String.concat "/" c.path)) zn.scope.nodes) (P.zones s)) (w.graphs @ w.defs)) fixtures;
  let w = load "sunflower" in
  let s = scope w "sunflower" in
  let l = P.layout s in
  let head = [ "sunflower"; "head" ] in
  (* inputs come first, the return last, and a node sits right of what it reads *)
  let x path = let (x, _, _, _) = List.assoc path (place l) in x in
  check (x [ "sunflower"; ":seeds" ] < x [ "sunflower"; "seeds_each" ]
         && x [ "sunflower"; "seeds_each" ] < x head)
    "columns follow the dependencies";
  (* a graph whose result is a node has no return card: the displayed node is the result *)
  check (not (List.exists (fun (q : P.placed) -> q.item = P.Return) l.placed)) "no return card for a linked result";
  (* a position override moves one node; a collapsed zone is a card *)
  let moved = P.layout ~at:(fun p -> if p = head then Some (500., 300.) else None) s in
  let (hx, hy, _, _) = List.assoc head (place moved) in
  check (hx = 500. && hy = 300.) "at overrides the position";
  let z = [ "sunflower"; "seeds_each" ] in
  let (_, _, _, open_h) = List.assoc z (place l) in
  let collapsed = P.layout ~collapsed:(fun p -> p = z) s in
  let (_, _, cw, ch) = List.assoc z (place collapsed) in
  check (cw = P.node_width && ch < open_h && List.length (place collapsed) < List.length (place l))
    "a collapsed zone is a card without its children";
  (* the study's card geometry, on the 24-point grid *)
  let p = List.find (fun (p : P.placed) -> p.path = head) l.placed in
  check (p.w = P.node_width && p.h = P.card_height ~rows:(float (Array.length p.lines)) ~extra:0.) "node size";
  (* cards and columns sit on the dot lattice; the card's rows are the exposure rule's *)
  List.iter (fun (q : P.placed) ->
    match q.item with
    | P.Item { zone = None; _ } | P.Input _ ->
        check (Float.rem q.x P.lattice = 0. && Float.rem q.y P.lattice = 0.)
          ("off the lattice: " ^ String.concat "/" q.path)
    | _ -> ()) l.placed;
  check (P.card_height ~rows:0. ~extra:0. = P.head_height
         && P.card_height ~rows:3. ~extra:0. = 100.
         && (let full foot = P.layout ~foot ~level:(fun _ -> P.Full) s |> fun l ->
               (List.find (fun (q : P.placed) -> q.path = head) l.placed).h in
             full true = full false +. P.foot_height
             && (P.layout ~foot:true s |> fun l -> (List.find (fun (q : P.placed) -> q.path = head) l.placed).h) = p.h))
    "kit card heights: header only 24, three rows 100, a footer row 24 more on a full card only"

(* a loop over the points or pieces of geometry says how its elements are ordered *)
let zone_order () =
  let order text = let w = workspace_of text in
    match (scope w "g").nodes with
    | n :: _ -> (match n.zone with Some z -> z.order | None -> fail "not a zone")
    | [] -> fail "no node" in
  check (order "(workspace w (graph g :context sop (let* [z (for [p (sop/point_list (sop/grid))] (sop/box))] (sop/merge z))))" = Some "by index")
    "a loop over points is ordered by index";
  check (order "(workspace w (graph g :context sop (let* [z (for [p (sop/piece_list (sop/grid) :key \"id\")] (sop/box))] (sop/merge z))))" = Some "by id")
    "a loop over pieces keyed by an attribute is ordered by it";
  check ((match (scope (workspace_of "(workspace w (graph g :context sop (let* [pts (sop/point_list (sop/grid)) z (for [p pts] (sop/box))] (sop/merge z))))") "g").nodes with
             | nodes -> List.exists (fun (n : P.node) -> match n.zone with
                 | Some z -> z.order = Some "by index" | None -> false) nodes))
    "a loop over a bound point list is ordered by index too";
  check (order "(workspace w (graph g :context sop (let* [z (for [i (range 3)] (sop/box))] (sop/merge z))))" = None)
    "a loop over a range has no order to show"

(* a row pinned onto a card shows there, and one pinned off leaves it: [lines ~pin] and
   [row_shown] read the layout's per-row flags *)
let pins () =
  let w = workspace_of "(workspace w (graph g :context sop (let* [s (sop/scatter (sop/grid) :count 50)] (sop/merge s))))" in
  let n = node w "g" [ "s" ] in
  let group = match List.find_opt (fun (r : P.row) -> r.label = "group") n.rows with
    | Some r -> r | None -> fail "scatter has no group row" in
  let count = match List.find_opt (fun (r : P.row) -> r.label = "count") n.rows with
    | Some r -> r | None -> fail "scatter has no count row" in
  let on_card pin = Array.exists (function P.Row (_, (r : P.row)) -> r.label = "group" | _ -> false)
    (P.lines ~pin P.Card n) in
  check (not (P.row_shown group)) "an unset group is off the card by default";
  check (P.row_shown ~pin:true group) "a pin shows it";
  check (not (on_card (fun _ -> None))) "the card leaves it out";
  check (on_card (fun label -> if label = "group" then Some true else None)) "a pinned row is on the card";
  check (P.row_shown count && not (P.row_shown ~pin:false count)) "a pin of false takes a written row off the card";
  (* what the text says is drawn: a default written out stays on the card *)
  let radius w = List.find (fun (r : P.row) -> r.label = "radius") (node w "g" [ "s" ]).rows in
  let bare = radius (workspace_of "(workspace w (graph g :context sop (let* [s (sop/uv_sphere)] (sop/merge s))))") in
  let written = radius (workspace_of (Printf.sprintf
    "(workspace w (graph g :context sop (let* [s (sop/uv_sphere :radius %s)] (sop/merge s))))" (Option.get bare.default))) in
  check (not (P.row_shown bare) && P.row_shown written) "a written default is not on the card"

let run () =
  let schema = Param.schema ~name:"switch" ~default:0
      [Param.field ~name:"input" ~kind:(Param.index_choice ["a"; "b"])
        ~default:0 ~get:Fun.id ~set:(fun value _ -> value) ()] in
  let ports = Flow_sop.Port.parameters (Param.view schema 0) |> Result.get_ok in
  let port = Flow_sop.Port.find_parameter ports "input" |> Result.get_ok in
  check (port.ty = Some Flow.Port_type.Int) "index choice must expose an integer port";
  check (Flow_sop.Port.normalize port (Flow.Port_type.Int_value 1)
      = Ok (Flow.Port_type.Int_value 1, ["input", Param.Int_value 1]))
    "index choice must accept an integer drive";
  check (Result.is_error (Flow_sop.Port.normalize port (Flow.Port_type.Int_value 2)))
    "index choice must refuse out-of-range drives";
  let source = "(workspace w (graph g :context sop (let* [a (sop/box) b (sop/grid) s (sop/switch a b :input 1)] s)))" in
  let selected = node (workspace_of source) "g" ["s"] in
  let row = List.find (fun (r : P.row) -> r.key = E.Kw "input") selected.rows in
  check (row.ty = Some Flow.Ty.Int && row.socket && flat row.expr = "1")
    "Switch selection must project as an editable integer row";
  let forms = S.parse source |> Result.get_ok in
  let zero = S.parse "0" |> Result.get_ok |> List.hd in
  let edited = E.apply catalog forms
      (E.Set_arg {node = ["g"; "s"]; key = E.Kw "input"; sub = []; value = zero})
      |> Result.get_ok in
  let workspace = match Flow.Workspace.check catalog edited with
    | Some workspace, _ -> workspace | _ -> fail "Switch selection edit failed to check" in
  let selected = node workspace "g" ["s"] in
  check (List.exists (fun (r : P.row) -> r.key = E.Kw "input" && flat r.expr = "0") selected.rows)
    "Switch selection must edit through Flow_edit";
  let transform_source = "(workspace w (graph g :context sop (let* [a (sop/box) transformed (sop/transform a :mode \"Matrix\" :m03 2 :shear_xy 0.1 :pivot [1 2 3])] transformed)))" in
  let transformed = node (workspace_of transform_source) "g" ["transformed"] in
  List.iter (fun keyword -> check (List.exists (fun (r : P.row) -> r.key = E.Kw keyword) transformed.rows)
      ("Transform projects " ^ keyword)) ["mode";"m03";"shear_xy";"pivot"];
  let edited = E.apply catalog (S.parse transform_source |> Result.get_ok)
      (E.Set_arg {node = ["g";"transformed"];key = E.Kw "m03";sub = [];value = S.parse "4" |> Result.get_ok |> List.hd})
      |> Result.get_ok in
  let workspace = match Flow.Workspace.check catalog edited with
    | Some workspace, _ -> workspace | _ -> fail "Transform matrix field edit failed to check" in
  check (List.exists (fun (r : P.row) -> r.key = E.Kw "m03" && flat r.expr = "4") (node workspace "g" ["transformed"]).rows)
    "Transform matrix fields edit through Flow_edit";
  let randomize_source = "(workspace w (graph g :context sop (let* [source (sop/box) random (sop/attribute_randomize source :distribution \"Custom discrete\" :entries \"0\\t1\\n1\\t2\" :selection_owner \"Vertex\" :minimum_vector [0 0 0])] random)))" in
  let random = node (workspace_of randomize_source) "g" ["random"] in
  List.iter (fun keyword -> check (List.exists (fun (r : P.row) -> r.key = E.Kw keyword) random.rows)
      ("Randomize projects " ^ keyword)) ["entries";"selection_owner";"selection_group";"minimum_vector"];
  let text = S.parse "\"2\\t1\"" |> Result.get_ok |> List.hd in
  let edited = E.apply catalog (S.parse randomize_source |> Result.get_ok)
      (E.Set_arg {node=["g";"random"];key=E.Kw "entries";sub=[];value=text}) |> Result.get_ok in
  let workspace = match Flow.Workspace.check catalog edited with
    | Some workspace, _ -> workspace | _ -> fail "Randomize table edit failed to check" in
  check (List.exists (fun (r : P.row) -> r.key = E.Kw "entries" && r.expr <> None) (node workspace "g" ["random"]).rows)
    "Randomize tables edit through Flow_edit";
  let interpolate_source = "(workspace w (graph g :context sop (let* [source (sop/box) target (sop/box) interpolated (sop/attribute_interpolate source target :attributes \"point\\tCd\\tcolor\" :driver \"Point weights\" :threshold 0.01)] interpolated)))" in
  let interpolated = node (workspace_of interpolate_source) "g" ["interpolated"] in
  List.iter (fun keyword -> check (List.exists (fun (r : P.row) -> r.key = E.Kw keyword) interpolated.rows)
    ("Interpolate projects " ^ keyword)) ["attributes";"driver";"numbers_attribute";"computed_owner"];
  let edited = E.apply catalog (S.parse interpolate_source |> Result.get_ok)
    (E.Set_arg {node=["g";"interpolated"];key=E.Kw "attributes";sub=[];
      value=S.parse "\"vertex\\tN\\tnormal\"" |> Result.get_ok |> List.hd}) |> Result.get_ok in
  let workspace = match Flow.Workspace.check catalog edited with
    | Some workspace,_ -> workspace | _ -> fail "Interpolate table edit failed to check" in
  check (List.exists (fun (r : P.row) -> r.key=E.Kw "attributes" && r.expr <> None)
    (node workspace "g" ["interpolated"]).rows) "Interpolate table edits through Flow_edit";
  let transfer_source = "(workspace w (graph g :context sop (let* [source (sop/box) target (sop/box) transferred (sop/attribute_transfer source target :use_names true :names \"Cd\\nN\" :distance_mode \"Auto\")] transferred)))" in
  let transferred=node (workspace_of transfer_source) "g" ["transferred"] in
  List.iter (fun keyword -> check (List.exists (fun (r : P.row) -> r.key=E.Kw keyword) transferred.rows)
    ("Transfer projects " ^ keyword)) ["use_names";"names";"distance_mode"];
  let edited=E.apply catalog (S.parse transfer_source |> Result.get_ok)
    (E.Set_arg {node=["g";"transferred"];key=E.Kw "names";sub=[];
      value=S.parse "\"Cd\"" |> Result.get_ok |> List.hd}) |> Result.get_ok in
  let workspace=match Flow.Workspace.check catalog edited with
    | Some workspace,_ -> workspace | _ -> fail "Transfer exact names edit failed to check" in
  check (List.exists (fun (r : P.row) -> r.key=E.Kw "names" && flat r.expr="\"Cd\"")
    (node workspace "g" ["transferred"]).rows) "Transfer exact names edit through Flow_edit";
  pins ();
  zone_order ();
  snapshots ();
  rows ();
  flags ();
  layout ();
  print_endline "projection tests passed"
