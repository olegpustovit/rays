open Flow
open Flow_graph

let catalog = Catalog.{ version = 1; kinds = [] }
let ok = function Ok x -> x | Error d -> failwith (Diagnostic.to_string d)
let source body = ok (Syntax.parse ("(workspace w (graph g :context host (let* " ^ body ^ ")))"))
let text src = fst (Lisp.print src)

let has s sub =
  let n = String.length sub in
  let rec go i = i + n <= String.length s && (String.sub s i n = sub || go (i + 1)) in
  go 0

let edit src op = fst (ok (Flow_edit.apply_checked catalog src op))

let connect src node name =
  edit src (Connect { node = [ "g"; node ]; key = Pos 0; src = name; iter = false })

let () =
  let src = source "[sum (+ 1.0 2.0) x (max 0.0 0.0) y (max 0.0 0.0)] x" in
  (* an unnamed math node with one reader is written in it *)
  let one = connect src "x" "sum" in
  assert (has (text one) "(max (+ 1.0 2.0) 0.0)" && not (has (text one) "sum"));
  (* a second reader makes it a binding again *)
  let two = connect one "y" "x#0" in
  assert (has (text two) "sum_2 (+ 1.0 2.0)" && has (text two) "(max sum_2 0.0)");
  (* a node the user named stays a binding *)
  let named = connect (source "[half (+ 1.0 2.0) x (max 0.0 0.0)] x") "x" "half" in
  assert (has (text named) "half (+ 1.0 2.0)" && has (text named) "(max half 0.0)");
  (* disconnecting keeps the expression as a binding *)
  let off =
    edit one
      (Disconnect
         { node = [ "g"; "x" ]; key = Pos 0; fallback = Some (List.hd (ok (Syntax.parse "0.5"))) })
  in
  assert (has (text off) "sum_2 (+ 1.0 2.0)" && has (text off) "(max 0.5 0.0)");
  (* deleting the second reader leaves one: it folds *)
  let del = edit two (Delete_nodes { nodes = [ [ "g"; "y" ] ] }) in
  assert (has (text del) "(max (+ 1.0 2.0) 0.0)" && not (has (text del) "sum"));
  (* the projection draws the open expression as cards, the folded one as a chip *)
  let w = match Workspace.check catalog one with Some w, [] -> w | _ -> failwith "check" in
  let card (s : Projection.scope) = Projection.find s [ "g"; "x#0" ] in
  assert (card (Projection.of_graph catalog w "g") = None);
  let n = Option.get (card (Projection.of_graph ~folded:(fun _ -> false) catalog w "g")) in
  assert (n.expr && n.head = "+")

let () =
  (* a deleted node with readers dissolves into them; it is never refused *)
  let src = source "[a (+ 1.0 2.0) b (max a 0.0) c (max a 1.0)] (max b c)" in
  let del = edit src (Delete_nodes { nodes = [ [ "g"; "a" ] ] }) in
  assert (
    has (text del) "b (max (+ 1.0 2.0) 0.0)"
    && has (text del) "c (max (+ 1.0 2.0) 1.0)"
    && not (has (text del) " a "))

let () =
  (* l: a function of a node that reads nothing, or of a local function call, is refused *)
  let refused src nodes =
    Result.is_error (Flow_edit.apply_checked catalog src (Make_local_fn { nodes }))
  in
  assert (refused (source "[a (+ 1.0 2.0)] a") [ [ "g"; "a" ] ]);
  let made = edit (source "[a 1.0 b (+ a 2.0)] b") (Make_local_fn { nodes = [ [ "g"; "b" ] ] }) in
  assert (has (text made) "b_fn (fn [in_a]" && has (text made) "b (b_fn a)");
  assert (refused made [ [ "g"; "b" ] ])

let () =
  (* the call of a local fn is wired from it *)
  let made = edit (source "[a 1.0 b (+ a 2.0)] b") (Make_local_fn { nodes = [ [ "g"; "b" ] ] }) in
  let w = match Workspace.check catalog made with Some w, [] -> w | _ -> failwith "check" in
  let n = Option.get (Projection.find (Projection.of_graph catalog w "g") [ "g"; "b" ]) in
  assert (
    List.exists
      (fun (r : Projection.row) -> r.label = "fn" && Projection.sources r = [ "b_fn" ])
      n.rows);
  (* and its inputs are named by the fn's parameters *)
  assert (
    List.exists
      (fun (r : Projection.row) -> r.label = "in_a" && Projection.sources r = [ "a" ])
      n.rows)

let () =
  (* Repeat in a drawing graph collects the drawings and merges them *)
  let src =
    ok (Syntax.parse "(workspace w (graph g :context draw (let* [c (draw/circle [0 0 0] 5)] c)))")
  in
  let cat = Catalog.{ version = 1; kinds = [] } in
  let r =
    fst (ok (Flow_edit.apply_checked cat src (Wrap { nodes = [ [ "g"; "c" ] ]; loop = For })))
  in
  assert (has (text r) "c (draw/merge c_each)")

let () =
  (* a chain step trades places along the chain input; deleting a step hands its place to that input *)
  let src =
    ok
      (Syntax.parse
         "(workspace w (graph g :context draw (draw/translate [1 2 0] (draw/rotate 0.3 \
          (draw/circle [0 0 0] 5)))))")
  in
  let cat = Catalog.{ version = 1; kinds = [] } in
  let ed op = fst (ok (Flow_edit.apply_checked cat src op)) in
  let flat t =
    String.concat " "
      (List.filter (( <> ) "")
         (String.split_on_char ' ' (String.map (function '\n' -> ' ' | c -> c) t)))
  in
  let swapped = flat (text (ed (Move_step { node = [ "g"; "@result#1" ]; earlier = false }))) in
  assert (has swapped "(draw/rotate 0.3 (draw/translate [1 2 0] (draw/circle [0 0 0] 5)))");
  assert (flat (text (ed (Move_step { node = [ "g"; "@result" ]; earlier = true }))) = swapped);
  assert (
    Result.is_error
      (Flow_edit.apply_checked cat src
         (Move_step { node = [ "g"; "@result#1#1" ]; earlier = true })));
  let gone = flat (text (ed (Delete_nodes { nodes = [ [ "g"; "@result#1" ] ] }))) in
  assert (has gone "(draw/translate [1 2 0] (draw/circle [0 0 0] 5))")

let () =
  (* the context symbol [t] written in an input is a card of its own, wired to the row; the
     text is unchanged and disconnecting the wire writes the fallback literal *)
  let src =
    ok (Syntax.parse "(workspace w (graph g :context host (let* [s (sin (* t 0.5))] s)))")
  in
  let w =
    match Workspace.check catalog src with
    | Some w, [] -> w
    | _, ds -> failwith (String.concat ";" (List.map Diagnostic.to_string ds))
  in
  let scope = Projection.of_graph ~folded:(fun _ -> false) catalog w "g" in
  let t = Option.get (Projection.find scope [ "g"; "s#0#0" ]) in
  assert (t.head = "t" && t.expr && t.rows = []);
  let mul = Option.get (Projection.find scope [ "g"; "s#0" ]) in
  let a = List.hd mul.rows in
  assert (a.chip = Projection.Name "s#0#0" && Projection.sources a = [ "s#0#0" ]);
  assert (has (text src) "(sin (* t 0.5))");
  let off =
    edit src
      (Disconnect
         { node = [ "g"; "s#0" ]; key = Pos 0; fallback = Some (List.hd (ok (Syntax.parse "0.5"))) })
  in
  assert (has (text off) "(sin (* 0.5 0.5))")
