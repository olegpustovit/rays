open Flow
open Flow_graph

let catalog = Catalog.{ version = 1; kinds = [] }
let ok = function Ok x -> x | Error d -> failwith (Diagnostic.to_string d)
let form text = List.hd (ok (Syntax.parse text))

let check source =
  match Workspace.check catalog source with
  | Some w, [] -> w
  | _, ds -> failwith (String.concat ";" (List.map Diagnostic.to_string ds))

let result checked = List.assoc "g" (ok (Eval.run ~time:0. checked)).results
let first value = match value with Eval.Float_array a -> a.(0) | _ -> failwith "not a float array"
let row (card : Projection.node) key = List.find (fun (r : Projection.row) -> r.key = key) card.rows

let rec find head (scope : Projection.scope) =
  List.find_map
    (fun (node : Projection.node) ->
      if node.head = head then Some node
      else Option.bind node.zone (fun zone -> find head zone.scope))
    scope.nodes

let () =
  let source =
    ok
      (Syntax.parse
         "(workspace passes (graph g :context host (let* [count 3 seed (array/float 4 1.0) tested \
          (iterate count seed (fn [p] (map (fn [x] (* x 0.5)) p)))] tested)))")
  in
  let checked = check source in
  let pane = Projection.of_graph catalog checked "g" in
  let card = Option.get (Projection.find pane [ "g"; "tested" ]) in
  (* a card like a fold's: typed rows for the count, the start, the function and the optional stop *)
  assert (card.head = "iterate" && card.ty = Ty.Array Ty.Float);
  assert ((row card (Pos 0)).ty = Some Ty.Int && (row card (Pos 0)).chip = Name "count");
  assert ((row card (Pos 1)).ty = Some (Ty.Array Ty.Float) && (row card (Pos 1)).chip = Name "seed");
  assert ((row card (Pos 2)).ty = Some (Ty.Fn None));
  assert ((row card (Kw "until")).expr = None && (row card (Kw "until")).socket);
  (* the function is a nested fn zone whose parameter has the start's type *)
  let fn = Option.get (find "fn" pane) in
  let zone = Option.get fn.zone in
  assert (zone.kind = Fn && (List.hd zone.rail).ty = Some (Ty.Array Ty.Float));
  assert (first (result checked) = 0.125);
  let edit op = ok (Flow_edit.apply_checked catalog source op) in
  (* the same gestures as any call: set a literal, wire a name, add and remove :until *)
  let _, literal = edit (Set_arg { node = card.path; key = Pos 0; sub = []; value = form "5" }) in
  assert (first (result literal) = 1.0 /. 32.);
  let _, zero = edit (Set_arg { node = card.path; key = Pos 0; sub = []; value = form "0" }) in
  assert (first (result zero) = 1.0);
  let _, wired = edit (Connect { node = card.path; key = Pos 0; src = "count"; iter = false }) in
  assert (first (result wired) = 0.125);
  let _, reseeded =
    edit (Set_arg { node = card.path; key = Pos 1; sub = []; value = form "(array/float 4 8.0)" })
  in
  assert (first (result reseeded) = 1.0);
  let stopped, until =
    edit (Set_arg { node = card.path; key = Kw "until"; sub = []; value = form "(fn [a b] true)" })
  in
  assert (first (result until) = 0.5);
  let saved, _ = Lisp.print stopped in
  let loaded = check (ok (Syntax.parse saved)) in
  assert (first (result loaded) = 0.5);
  let with_until = Projection.of_graph catalog until "g" in
  let card_until = Option.get (Projection.find with_until [ "g"; "tested" ]) in
  assert ((row card_until (Kw "until")).expr <> None);
  let _, removed =
    ok
      (Flow_edit.apply_checked catalog stopped
         (Disconnect { node = card.path; key = Kw "until"; fallback = None }))
  in
  assert (first (result removed) = 0.125);
  assert (
    Result.is_error
      (Flow_edit.apply_checked catalog source
         (Set_arg { node = card.path; key = Pos 0; sub = []; value = form "\"bad\"" })));
  assert (
    Result.is_error
      (Flow_edit.apply_checked catalog source
         (Set_arg { node = card.path; key = Pos 2; sub = []; value = form "3" })));
  (* a new iterate node from the menu *)
  let _, added =
    ok
      (Flow_edit.apply_checked catalog source
         (Add_node
            {
              scope = [ "g" ];
              name = "again";
              expr = form "(iterate 2 tested (fn [q] (map (fn [y] (+ y 1.0)) q)))";
            }))
  in
  assert (
    (Option.get (Projection.find (Projection.of_graph catalog added "g") [ "g"; "again" ])).ty
    = Ty.Array Ty.Float);
  print_endline
    "iterate graph: fold-like card, typed rows, nested fn zone, set/wire/add/remove gestures and \
     saved-source round trip"
