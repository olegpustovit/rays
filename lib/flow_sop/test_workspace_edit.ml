(* W3: every graph gesture of the study as a pure rewrite of the workspace
   text: apply the op, compare the printed text.  A refused edit changes
   nothing. *)
open Flow_graph
module S = Flow.Syntax
module E = Flow_edit

let fail message = failwith ("test_workspace_edit: " ^ message)
let check condition message = if not condition then fail message
let read path = In_channel.with_open_bin path In_channel.input_all
let cases = "../specification/workspace/cases"
let catalog = Editor_document.Contexts.catalog ~version:1 Sop.Nodes.factories |> Result.get_ok

let parse text = match S.parse text with
  | Ok forms -> forms | Error d -> fail (Diagnostic.to_string d)
let canon forms = fst (Flow.Lisp.print forms)
let norm text = canon (parse text)

let apply text op =
  match E.apply catalog (parse text) op with
  | Ok forms -> canon forms
  | Error d -> fail (E.label op ^ ": " ^ Diagnostic.to_string d)

let same name text op expected =
  let got = try apply text op with Failure m -> fail (name ^ ": " ^ m) and want = norm expected in
  if got <> want then fail (Printf.sprintf "%s\n--- got\n%s--- want\n%s" name got want)

let refused ?(code = "") name text op =
  match E.apply catalog (parse text) op with
  | Ok forms -> fail (name ^ " was accepted:\n" ^ canon forms)
  | Error d -> check (code = "" || d.code = code) (name ^ ": code " ^ d.code ^ " message " ^ d.message)

let has text sub =
  let n = String.length sub in
  let rec at i = i + n <= String.length text && (String.sub text i n = sub || at (i + 1)) in
  at 0
let g body = "(workspace w\n  (graph g :context sop [(n : int 4)]\n    " ^ body ^ "))"
let v body = "(workspace w\n  (graph g :context value [(n : int 4)]\n    " ^ body ^ "))"
let base = g "(let* [a (sop/uv_sphere :radius 0.5)\n           b (sop/transform a :translate [1 2 3])\n           c (sop/subdivide b :iterations 1)]\n      c)"
let num s = S.make (S.Num s)
let sym s = S.make (S.Sym s)
let node l = "g" :: l

let part1 () = (* scrub *)
  same "scrub" base (E.Set_arg { node = node [ "a" ]; key = Kw "radius"; sub = []; value = num "0.8" })
    (g "(let* [a (sop/uv_sphere :radius 0.8) b (sop/transform a :translate [1 2 3]) c (sop/subdivide b :iterations 1)] c)");
  same "vector component" base (E.Set_arg { node = node [ "b" ]; key = Kw "translate"; sub = [ 1 ]; value = num "9" })
    (g "(let* [a (sop/uv_sphere :radius 0.5) b (sop/transform a :translate [1 9 3]) c (sop/subdivide b :iterations 1)] c)");
  same "missing keyword is added" base (E.Set_arg { node = node [ "b" ]; key = Kw "scale"; sub = []; value = num "2" })
    (g "(let* [a (sop/uv_sphere :radius 0.5) b (sop/transform a :translate [1 2 3] :scale 2) c (sop/subdivide b :iterations 1)] c)");
  let scrub v = E.Set_arg { node = node [ "a" ]; key = Kw "radius"; sub = []; value = num v } in
  check (E.gesture (scrub "1") = E.gesture (scrub "2") && E.gesture (scrub "1") <> None) "scrub merges into one gesture";
  check (E.gesture (E.Toggle_bypass { node = node [ "a" ] }) = None) "other ops do not merge";
  check (E.gesture (E.Set_input_default { form = "g"; input = "n"; value = num "1" })
         = E.gesture (E.Set_input_default { form = "g"; input = "n"; value = num "2" })
         && E.gesture (E.Set_input_default { form = "g"; input = "n"; value = num "1" }) <> None
         && E.gesture (E.Set_input_default { form = "g"; input = "n"; value = num "1" })
            <> E.gesture (E.Set_input_default { form = "g"; input = "m"; value = num "1" }))
    "scrubbing an input default is one gesture per input";
  refused ~code:"E_UNKNOWN_PARAM" "unknown keyword" base
    (E.Set_arg { node = node [ "a" ]; key = Kw "nope"; sub = []; value = num "1" })

let part2 () = (* connect, disconnect, input defaults *)
  let fwd = g "(let* [a (sop/uv_sphere) c (sop/subdivide a) b (sop/transform a)] c)" in
  same "connect reorders" fwd (E.Connect { node = node [ "c" ]; key = Pos 0; src = "b"; iter = false })
    (g "(let* [a (sop/uv_sphere) b (sop/transform a) c (sop/subdivide b)] c)");
  refused ~code:"E_GRAPH_CYCLE" "connect cycle" (g "(let* [a (sop/transform nil) b (sop/transform a)] b)")
    (E.Connect { node = node [ "a" ]; key = Pos 0; src = "b"; iter = false });
  (* a binding named like the graph it [ref]s does not read itself: no cycle on reorder *)
  let shared fov = "(workspace w\n  (graph cage :context sop (sop/box))\n  (graph scene :context scene\n    (let* [cage (scene/geometry (ref cage) :name \"cage\") camera (scene/camera :fov " ^ fov ^ ")]\n      (scene/merge cage camera))))" in
  same "set arg beside a self-named graph ref" (shared "30")
    (E.Set_arg { node = [ "scene"; "camera" ]; key = Kw "fov"; sub = []; value = num "40" }) (shared "40");
  same "connect to result" fwd (E.Connect { node = node [ "@result" ]; key = Whole; src = "b"; iter = false })
    (g "(let* [a (sop/uv_sphere) c (sop/subdivide a) b (sop/transform a)] b)");
  let loop = g "(let* [ring (for [i (range n)] (sop/uv_sphere :radius 0.5)) m (sop/merge ring)] m)" in
  same "iteration step" loop (E.Connect { node = [ "g"; "ring"; "@result" ]; key = Kw "radius"; src = "i"; iter = true })
    (g "(let* [ring (for [i (range n)] (sop/uv_sphere :radius (* i 0.5))) m (sop/merge ring)] m)");
  same "iteration by name" loop (E.Connect { node = [ "g"; "ring"; "@result" ]; key = Kw "radius"; src = "i"; iter = false })
    (g "(let* [ring (for [i (range n)] (sop/uv_sphere :radius i)) m (sop/merge ring)] m)");
  same "disconnect keyword" (g "(let* [k 0.4 a (sop/uv_sphere :radius k)] a)")
    (E.Disconnect { node = node [ "a" ]; key = Kw "radius"; fallback = None })
    (g "(let* [k 0.4 a (sop/uv_sphere)] a)");
  same "disconnect keyword onto a written default" (g "(let* [k 0.4 a (sop/uv_sphere :radius k)] a)")
    (E.Disconnect { node = node [ "a" ]; key = Kw "radius"; fallback = Some (num "1.0") })
    (g "(let* [k 0.4 a (sop/uv_sphere :radius 1.0)] a)");
  same "disconnect positional with fallback" base
    (E.Disconnect { node = node [ "b" ]; key = Pos 0; fallback = E.default_for Flow.Ty.geometry "geo" })
    (g "(let* [a (sop/uv_sphere :radius 0.5) b (sop/transform nil :translate [1 2 3]) c (sop/subdivide b :iterations 1)] c)");
  same "input default" base (E.Set_input_default { form = "g"; input = "n"; value = num "8" })
    (String.concat "" [ "(workspace w (graph g :context sop [(n : int 8)] (let* [a (sop/uv_sphere :radius 0.5) ";
      "b (sop/transform a :translate [1 2 3]) c (sop/subdivide b :iterations 1)] c)))" ]);
  refused "missing input" base (E.Set_input_default { form = "g"; input = "zz"; value = num "8" })

let part3 () = (* unfold and fold round trip *)
  let conditional = g "(let* [b (if true (sop/uv_sphere :radius 0.5) (sop/box))] b)" in
  let unfolded_conditional = g "(let* [uv_sphere (sop/uv_sphere :radius 0.5) b (if true uv_sphere (sop/box))] b)" in
  same "unfold conditional arm" conditional (E.Unfold {node = node ["b"]; key = Arm 0; sub = []}) unfolded_conditional;
  same "fold into conditional arm" unfolded_conditional (E.Fold_into {node = node ["uv_sphere"]}) conditional;
  let nested = g "(let* [b (sop/transform (sop/uv_sphere :radius 0.5) :translate [1 2 3])] b)" in
  let unfolded = g "(let* [uv_sphere (sop/uv_sphere :radius 0.5) b (sop/transform uv_sphere :translate [1 2 3])] b)" in
  same "unfold" nested (E.Unfold { node = node [ "b" ]; key = Pos 0; sub = [] }) unfolded;
  same "fold" unfolded (E.Fold_into { node = node [ "uv_sphere" ] }) nested;
  same "unfold a loop" (g "(let* [m (sop/merge (for [i (range 3)] (sop/uv_sphere)))] m)")
    (E.Unfold { node = node [ "m" ]; key = Pos 0; sub = [] })
    (g "(let* [m_each (for [i (range 3)] (sop/uv_sphere)) m (sop/merge m_each)] m)");
  same "unfold part of a vector" (g "(let* [b (sop/transform nil :translate [(+ 1 2) 0 0])] b)")
    (E.Unfold { node = node [ "b" ]; key = Kw "translate"; sub = [ 0 ] })
    (g "(let* [sum_2 (+ 1 2) b (sop/transform nil :translate [sum_2 0 0])] b)");
  same "fold into the result" (g "(let* [a (sop/uv_sphere) b (sop/transform a)] b)") (E.Fold_into { node = node [ "b" ] })
    (g "(let* [a (sop/uv_sphere)] (sop/transform a))");
  check (has (apply (g "(let* [a (sop/uv_sphere)\n ; keep me\n b (sop/transform a)] (sop/subdivide b))") (E.Fold_into { node = node [ "b" ] })) "keep me")
    "fold dropped the binding's note";
  refused "fold needs one use" (g "(let* [a (sop/uv_sphere) b (sop/transform a) c (sop/merge a b)] c)")
    (E.Fold_into { node = node [ "a" ] });
  refused "unfold an atom" base (E.Unfold { node = node [ "c" ]; key = Kw "iterations"; sub = [] })

(* a call written in an input (a step of a ->) is a node: its leaf is the holder's, #, the input *)
let nested_nodes () =
  let chain = g "(-> (sop/uv_sphere :radius 0.5) (sop/transform :translate [1 2 3]) (sop/subdivide :iterations 1) (sop/normals))" in
  let sub = node [ "@result#0" ] and tr = node [ "@result#0#0" ] and ball = node [ "@result#0#0#0" ] in
  same "set an input of a step" chain
    (E.Set_arg { node = sub; key = Kw "iterations"; sub = []; value = num "2" })
    (g "(-> (sop/uv_sphere :radius 0.5) (sop/transform :translate [1 2 3]) (sop/subdivide :iterations 2) (sop/normals))");
  same "set a vector part of a deep step" chain
    (E.Set_arg { node = tr; key = Kw "translate"; sub = [ 1 ]; value = num "7" })
    (g "(-> (sop/uv_sphere :radius 0.5) (sop/transform :translate [1 7 3]) (sop/subdivide :iterations 1) (sop/normals))");
  check (has (apply chain (E.Toggle_bypass { node = sub })) "^:bypass (sop/subdivide") "a step takes the bypass flag";
  same "naming a step binds it" chain (E.Rename { node = sub; to_ = "smooth" })
    (g "(let* [smooth (sop/subdivide (sop/transform (sop/uv_sphere :radius 0.5) :translate [1 2 3]) :iterations 1)] (sop/normals smooth))");
  same "a deleted step hands its place to its input" chain (E.Delete_nodes { nodes = [ sub ] })
    (g "(sop/normals (sop/transform (sop/uv_sphere :radius 0.5) :translate [1 2 3]))");
  same "two deleted steps, the inner first" chain (E.Delete_nodes { nodes = [ sub; tr ] })
    (g "(sop/normals (sop/uv_sphere :radius 0.5))");
  same "a deleted source leaves nil" chain (E.Delete_nodes { nodes = [ ball ] })
    (g "(-> nil (sop/transform :translate [1 2 3]) (sop/subdivide :iterations 1) (sop/normals))");
  (* wires never delete nodes: the output of a step read twice is named, a step a wire replaces stays *)
  let two = g "(let* [m (sop/merge (sop/transform (sop/uv_sphere)))] m)" in
  same "the output of a step feeds a second input" two
    (E.Connect { node = node [ "m" ]; key = Pos 1; src = "m#0#0"; iter = false })
    (g "(let* [uv_sphere (sop/uv_sphere) m (sop/merge (sop/transform uv_sphere) uv_sphere)] m)");
  same "a wire onto a step's place keeps the step" (g "(let* [a (sop/box) m (sop/normals (sop/transform (sop/uv_sphere)))] m)")
    (E.Connect { node = node [ "m" ]; key = Pos 0; src = "a"; iter = false })
    (g "(let* [a (sop/box) transform (sop/transform (sop/uv_sphere)) m (sop/normals a)] m)");
  same "viewing a step makes it the result" chain
    (E.Connect { node = node [ "@result" ]; key = Whole; src = "@result#0"; iter = false })
    (g "(let* [subdivide (sop/subdivide (sop/transform (sop/uv_sphere :radius 0.5) :translate [1 2 3]) :iterations 1) normals (sop/normals subdivide)] subdivide)");
  refused "a step cannot be folded" chain (E.Fold_into { node = sub });
  refused "a step is not duplicated" chain (E.Duplicate { nodes = [ sub ] });
  check (E.remap (E.Rename { node = sub; to_ = "smooth" }) tr = Some (node [ "smooth#0" ])
         && E.remap (E.Delete_nodes { nodes = [ sub ] }) tr = Some sub
         && E.remap (E.Delete_nodes { nodes = [ sub ] }) sub = None
         && E.remap (E.Rename { node = node [ "b" ]; to_ = "z" }) (node [ "b#0" ]) = Some (node [ "z#0" ]))
    "layout keys follow a renamed or deleted step"

let part4 () = (* repeat, iterate *)
  let two = g "(let* [a (sop/uv_sphere :radius 0.5) b (sop/transform a :scale 0.8)] b)" in
  same "repeat geometry" two (E.Wrap { nodes = [ node [ "a" ] ]; loop = For })
    (g "(let* [a_each (for [i (range 6)] (sop/uv_sphere :radius 0.5)) a (sop/merge a_each) b (sop/transform a :scale 0.8)] b)");
  same "repeat numbers" (g "(let* [x (+ 1 2) y (* x 2) a (sop/uv_sphere :radius y)] a)")
    (E.Wrap { nodes = [ node [ "x" ] ]; loop = For })
    (g "(let* [x (sum [i (range 6)] (+ 1 2)) y (* x 2) a (sop/uv_sphere :radius y)] a)");
  same "iterate" two (E.Wrap { nodes = [ node [ "b" ] ]; loop = Fold })
    (g "(let* [a (sop/uv_sphere :radius 0.5) b (fold [prev a] [i (range 4)] (sop/transform prev :scale 0.8))] b)");
  refused "iterate needs an input to feed back" (g "(let* [a (sop/uv_sphere)] a)")
    (E.Wrap { nodes = [ node [ "a" ] ]; loop = Fold });
  refused "repeat needs one result" (g "(let* [a (sop/uv_sphere) b (sop/transform a) c (sop/merge a b)] c)")
    (E.Wrap { nodes = [ node [ "a" ]; node [ "b" ] ]; loop = For });
  same "repeat two bindings" (g "(let* [a (sop/uv_sphere) b (sop/transform a) c (sop/merge b)] c)")
    (E.Wrap { nodes = [ node [ "a" ]; node [ "b" ] ]; loop = For })
    (g "(let* [b_each (for [i (range 6)] (let* [a (sop/uv_sphere) b (sop/transform a)] b)) b (sop/merge b_each) c (sop/merge b)] c)")

let part5 () = (* hoist *)
  let src = g "(let* [ring (for [i (range n)] (let* [u (/ i n) k (sop/uv_sphere :radius 0.2) s (sop/transform k :translate [u 0 0])] s)) m (sop/merge ring)] m)" in
  same "hoist" src (E.Hoist { node = [ "g"; "ring"; "k" ] })
    (g "(let* [k (sop/uv_sphere :radius 0.2) ring (for [i (range n)] (let* [u (/ i n) s (sop/transform k :translate [u 0 0])] s)) m (sop/merge ring)] m)");
  refused "hoist a loop-dependent node" src (E.Hoist { node = [ "g"; "ring"; "u" ] });
  refused "hoist from the top" src (E.Hoist { node = node [ "m" ] });
  same "hoist the only binding" (g "(let* [ring (for [i (range 3)] (let* [k (sop/uv_sphere)] k)) m (sop/merge ring)] m)")
    (E.Hoist { node = [ "g"; "ring"; "k" ] })
    (g "(let* [k (sop/uv_sphere) ring (for [i (range 3)] k) m (sop/merge ring)] m)")

let part6 () = (* rename keeps notes and rewrites uses; layout keys follow *)
  let src = g "(let* [a (sop/uv_sphere :radius 0.5)\n           ; the moved one\n           b (sop/transform a :translate [1 2 3])] b)" in
  let want = g "(let* [a (sop/uv_sphere :radius 0.5)\n           ; the moved one\n           moved (sop/transform a :translate [1 2 3])] moved)" in
  same "rename" src (E.Rename { node = node [ "b" ]; to_ = "moved" }) want;
  same "rename a source" (g "(let* [a (sop/uv_sphere) b (sop/transform a)] b)") (E.Rename { node = node [ "a" ]; to_ = "ball" })
    (g "(let* [ball (sop/uv_sphere) b (sop/transform ball)] b)");
  refused "rename to a used name" src (E.Rename { node = node [ "b" ]; to_ = "a" });
  refused "rename to an operator" src (E.Rename { node = node [ "b" ]; to_ = "sin" });
  refused "rename to a bad name" src (E.Rename { node = node [ "b" ]; to_ = "Two" });
  let op = E.Rename { node = [ "g"; "ring" ]; to_ = "petals" } in
  check (E.remap op [ "g"; "ring"; "u" ] = Some [ "g"; "petals"; "u" ]) "rename remaps nested layout keys";
  check (E.remap op [ "g"; "other" ] = Some [ "g"; "other" ]) "rename leaves other keys";
  check (E.remap (E.Hoist { node = [ "g"; "ring"; "k" ] }) [ "g"; "ring"; "k" ] = Some [ "g"; "k" ]) "hoist remaps";
  check (E.remap (E.Delete_nodes { nodes = [ [ "g"; "a" ] ] }) [ "g"; "a" ] = None) "delete drops layout keys"

let part7 () = (* functions and macros *)
  same "make lambda" base (E.Make_local_fn { nodes = [ node [ "b" ] ] })
    (g "(let* [a (sop/uv_sphere :radius 0.5) b_fn (fn [in_a] (sop/transform in_a :translate [1 2 3])) b (b_fn a) c (sop/subdivide b :iterations 1)] c)");
  let made = String.concat "" [ "(workspace w\n  (defmacro shifted [dx a] `(sop/transform ~a :translate [~dx 2 3]))\n";
    "  (graph g :context sop [(n : int 4)] (let* [a (sop/uv_sphere :radius 0.5) b (shifted 1 a) c (sop/subdivide b :iterations 1)] c)))" ] in
  same "make macro" base (E.Make_macro { nodes = [ node [ "b" ] ]; name = "shifted"; holes = [ [ 3; 0 ], "dx" ] }) made;
  same "inline macro" made (E.Inline_macro { node = node [ "b" ] })
    (String.concat "" [ "(workspace w\n  (defmacro shifted [dx a] `(sop/transform ~a :translate [~dx 2 3]))\n";
      "  (graph g :context sop [(n : int 4)] (let* [a (sop/uv_sphere :radius 0.5) b (sop/transform a :translate [1 2 3]) c (sop/subdivide b :iterations 1)] c)))" ]);
  let lits = E.literals (S.make (S.List [ sym "sop/transform"; sym "a"; S.make (S.Kw "translate");
    S.make (S.Vec [ num "1"; num "2"; num "3" ]) ])) in
  check (List.map fst lits = [ [ 3; 0 ]; [ 3; 1 ]; [ 3; 2 ] ]) "literals of an expression";
  refused "macro name taken" base (E.Make_macro { nodes = [ node [ "b" ] ]; name = "range"; holes = [] });
  (* several bindings: their own names become fresh names in the template *)
  let two = g "(let* [a (sop/uv_sphere :radius 0.5) b (sop/transform a) c (sop/subdivide b)] c)" in
  let out = apply two (E.Make_macro { nodes = [ node [ "a" ]; node [ "b" ] ]; name = "pair"; holes = [ [ 1; 1; 2 ], "r" ] }) in
  check (has out "a#") ("multi-binding macro uses fresh names:\n" ^ out)

(* W9: what the make-macro dialog answers is one Make_macro, and its draft lists the holes' candidates *)
let dialog () =
  let src = parse base in
  let draft = match E.macro_draft src [ node [ "b" ] ] with
    | Ok d -> d | Error d -> fail (Diagnostic.to_string d) in
  check (List.map fst draft.literals = [ [ 3; 0 ]; [ 3; 1 ]; [ 3; 2 ] ] && draft.free = [ "a" ]
         && draft.name = "b_tpl") "the draft of b";
  let op = E.macro_op draft ~nodes:[ node [ "b" ] ] ~name:"shifted" [| true, "dx"; false, "dy"; true, "dz" |] in
  check (op = E.Make_macro { nodes = [ node [ "b" ] ]; name = "shifted"; holes = [ [ 3; 0 ], "dx"; [ 3; 2 ], "dz" ] })
    "unticked literals are not holes";
  let out = apply base op in
  check (has out "(defmacro shifted [dx dz a]" && has out "b (shifted 1 3 a)") ("the dialog's macro:\n" ^ out);
  (match E.macro_draft src [ node [ "b" ]; node [ "@result" ] ] with
   | Error d -> check (d.code = "E_EDIT") "a result cannot be templated"
   | Ok _ -> fail "a result was templated");
  (* notes: typing merges into one history entry per node *)
  check (E.gesture (E.Set_note { node = node [ "b" ]; text = "x" }) = E.gesture (E.Set_note { node = node [ "b" ]; text = "xy" })
         && E.gesture (E.Set_note { node = node [ "b" ]; text = "x" }) <> None
         && E.gesture (E.Set_note { node = node [ "b" ]; text = "x" }) <> E.gesture (E.Set_note { node = node [ "c" ]; text = "x" }))
    "note typing is one gesture per node"

let part8 () = (* bypass and notes *)
  let by = apply base (E.Toggle_bypass { node = node [ "b" ] }) in
  check (norm by = norm (g "(let* [a (sop/uv_sphere :radius 0.5) b ^:bypass (sop/transform a :translate [1 2 3]) c (sop/subdivide b :iterations 1)] c)")) "bypass on";
  same "bypass off" by (E.Toggle_bypass { node = node [ "b" ] }) base;
  refused "bypass a scope" (g "(let* [a (let* [x 1] x)] (sop/uv_sphere))") (E.Toggle_bypass { node = node [ "a" ] });
  let noted = apply base (E.Set_note { node = node [ "b" ]; text = "the moved one\nsecond line" }) in
  check (norm noted = norm (g "(let* [a (sop/uv_sphere :radius 0.5)\n           ; the moved one\n           ; second line\n           b (sop/transform a :translate [1 2 3]) c (sop/subdivide b :iterations 1)] c)")) "note";
  same "note removed" noted (E.Set_note { node = node [ "b" ]; text = " " }) base;
  (* a note travels with its binding through a rename and a reorder *)
  let renamed = apply noted (E.Rename { node = node [ "b" ]; to_ = "moved" }) in
  check (has renamed "; the moved one\n") "note survives rename";
  let fwd = g "(let* [a (sop/uv_sphere)\n           ; keep me\n           c (sop/subdivide a)\n           b (sop/transform a)] c)" in
  let moved = apply fwd (E.Connect { node = node [ "c" ]; key = Pos 0; src = "b"; iter = false }) in
  check (norm moved = norm (g "(let* [a (sop/uv_sphere) b (sop/transform a)\n           ; keep me\n           c (sop/subdivide b)] c)")) "note follows its binding on reorder";
  let root_note = apply base (E.Set_note { node = [ "g" ]; text = "the graph" }) in
  check (has root_note "; the graph\n  (graph g") "graph note prints";
  let with_result = apply base (E.Set_note { node = node [ "@result" ]; text = "out" }) in
  check (has with_result "; out\n      c)") "result note"

let part9 () = (* lists and records *)
  let list = v "(let* [xs (list 0.2 0.4 0.6) k (count xs)] k)" in
  same "list continues a float step" list (E.Add_item { node = node [ "xs" ] })
    (v "(let* [xs (list 0.2 0.4 0.6 0.8) k (count xs)] k)");
  same "list continues an int step" (v "(let* [xs (list 1 2 3) k (count xs)] k)") (E.Add_item { node = node [ "xs" ] })
    (v "(let* [xs (list 1 2 3 4) k (count xs)] k)");
  same "list of one repeats" (v "(let* [xs (list 5) k (count xs)] k)") (E.Add_item { node = node [ "xs" ] })
    (v "(let* [xs (list 5 5) k (count xs)] k)");
  same "text adds a part" (v "(let* [s (str \"a\" n) k (count (list 1))] k)") (E.Add_item { node = node [ "s" ] })
    (v "(let* [s (str \"a\" n \" \") k (count (list 1))] k)");
  same "move up" list (E.Move_item { node = node [ "xs" ]; pos = 2 })
    (v "(let* [xs (list 0.2 0.6 0.4) k (count xs)] k)");
  refused "move the first" list (E.Move_item { node = node [ "xs" ]; pos = 0 });
  (* the items are the positional arguments wherever the keywords are written, and :skip follows them *)
  let merged = "(workspace w\n  (graph g :context sop (sop/box))\n  (graph scene :context scene\n    (let* [a (scene/geometry (ref g)) b (scene/light) c (scene/light) all (scene/merge :skip [0] a b c)] all)))" in
  let moved = apply merged (E.Move_item { node = [ "scene"; "all" ]; pos = 1 }) in
  check (has moved "(scene/merge :skip [1] b a c)") ("move up after a keyword\n" ^ moved);
  refused "move past the items" merged (E.Move_item { node = [ "scene"; "all" ]; pos = 3 });
  let record = g "(let* [r {:size 0.5 :count 6} a (sop/uv_sphere :radius r.size)] a)" in
  same "record field" record (E.Add_field { node = node [ "r" ]; name = "depth"; value = num "2" })
    (g "(let* [r {:size 0.5 :count 6 :depth 2} a (sop/uv_sphere :radius r.size)] a)");
  refused ~code:"E_EDIT" "an existing field" record (E.Add_field { node = node [ "r" ]; name = "size"; value = num "2" });
  same "a new output of values" (v "(let* [r (values :a 1)] r.a)") (E.Add_field { node = node [ "r" ]; name = "b"; value = num "0" })
    (v "(let* [r (values :a 1 :b 0)] r.a)");
  same "keyword field" base (E.Add_field { node = node [ "b" ]; name = "scale"; value = num "2" })
    (g "(let* [a (sop/uv_sphere :radius 0.5) b (sop/transform a :translate [1 2 3] :scale 2) c (sop/subdivide b :iterations 1)] c)");
  same "record field scrub" record (E.Set_arg { node = node [ "r" ]; key = Field "size"; sub = []; value = num "0.9" })
    (g "(let* [r {:size 0.9 :count 6} a (sop/uv_sphere :radius r.size)] a)")

let part10 () = (* add and delete *)
  same "add node" base (E.Add_node { scope = [ "g" ]; name = "extra"; expr = S.make (S.List [ sym "sop/uv_sphere" ]) })
    (g "(let* [a (sop/uv_sphere :radius 0.5) b (sop/transform a :translate [1 2 3]) c (sop/subdivide b :iterations 1) extra (sop/uv_sphere)] c)");
  same "add node to a bare body" (g "(sop/uv_sphere)")
    (E.Add_node { scope = [ "g" ]; name = "x"; expr = num "1" })
    (g "(let* [x 1] (sop/uv_sphere))");
  refused "add a used name" base (E.Add_node { scope = [ "g" ]; name = "a"; expr = num "1" });
  same "delete an unused node" (g "(let* [a (sop/uv_sphere) u (sop/uv_sphere :radius 2) b (sop/transform a)] b)")
    (E.Delete_nodes { nodes = [ node [ "u" ] ] }) (g "(let* [a (sop/uv_sphere) b (sop/transform a)] b)");
  same "delete a used node dissolves it into its reader" base (E.Delete_nodes { nodes = [ node [ "a" ] ] })
    (g "(let* [b (sop/transform (sop/uv_sphere :radius 0.5) :translate [1 2 3]) c (sop/subdivide b :iterations 1)] c)");
  (* a duplicated loop: its inner names are bound in both copies, and each copy's unused node can go *)
  let twins = g "(let* [r1 (for [i (range 2)] (let* [u (sop/uv_sphere) v (sop/uv_sphere)] v)) r2 (for [i (range 2)] (let* [u (sop/uv_sphere) v (sop/uv_sphere)] v)) m (sop/merge r1 r2)] m)" in
  check (not (has (apply twins (E.Delete_nodes { nodes = [ [ "g"; "r1"; "u" ] ] })) "r1 (for [i (range 2)] (let* [u"))
    "an unused node of a duplicated loop was not deleted";
  (* a node its own scope reads dissolves into that scope only; the twin keeps its own *)
  let gone = apply twins (E.Delete_nodes { nodes = [ [ "g"; "r2"; "v" ] ] }) in
  check (has gone "r2 (for [i (range 2)] (let* [u (sop/uv_sphere)] (sop/uv_sphere)))" && has gone "v (sop/uv_sphere)] v)")
    ("a node its own scope reads did not dissolve into that scope only: " ^ gone);
  same "delete a chain" (g "(let* [a (sop/uv_sphere) u (sop/transform a) v (sop/transform u) b (sop/transform a)] b)")
    (E.Delete_nodes { nodes = [ node [ "u" ]; node [ "v" ] ] }) (g "(let* [a (sop/uv_sphere) b (sop/transform a)] b)")

let part10b () = (* duplicate; positional arguments in any order; scene and World graphs *)
  let three = g "(let* [a (sop/uv_sphere) b (sop/transform a :translate [1 2 3]) c (sop/subdivide b)] c)" in
  same "duplicate one" three (E.Duplicate { nodes = [ node [ "b" ] ] })
    (g "(let* [a (sop/uv_sphere) b (sop/transform a :translate [1 2 3]) b_2 (sop/transform a :translate [1 2 3]) c (sop/subdivide b)] c)");
  same "duplicate a chain: the copies read each other" three (E.Duplicate { nodes = [ node [ "b" ]; node [ "c" ] ] })
    (g "(let* [a (sop/uv_sphere) b (sop/transform a :translate [1 2 3]) c (sop/subdivide b) b_2 (sop/transform a :translate [1 2 3]) c_2 (sop/subdivide b_2)] c)");
  same "duplicate twice takes fresh names"
    (g "(let* [a (sop/uv_sphere) a_2 (sop/uv_sphere) b (sop/transform a)] b)")
    (E.Duplicate { nodes = [ node [ "a" ] ] })
    (g "(let* [a (sop/uv_sphere) a_3 (sop/uv_sphere) a_2 (sop/uv_sphere) b (sop/transform a)] b)");
  refused "duplicate the result" three (E.Duplicate { nodes = [ node [ "@result" ] ] });
  refused "duplicate across scopes" (g "(let* [ring (for [i (range n)] (let* [u (sop/uv_sphere)] u)) m (sop/merge ring)] m)")
    (E.Duplicate { nodes = [ node [ "ring" ]; [ "g"; "ring"; "u" ] ] });
  check (E.duplicated (parse three) [ node [ "b" ]; node [ "c" ] ] = [ node [ "b_2" ]; node [ "c_2" ] ]
         && E.duplicated (parse three) [ node [ "@result" ] ] = []) "the copies' paths";
  check (E.label (E.Duplicate { nodes = [] }) = "Duplicate") "duplicate label";
  (* names for a batch: each joins the used ones, so the next is distinct *)
  let used = ref [ "box" ] in
  let first = E.fresh_among used "Box" in
  check (first = "box_2" && E.fresh_among used "sop/box" = "box_3" && List.mem "box_3" !used)
    "names of one batch are not distinct";
  (* a keyword before the positional argument is still an argument of the call *)
  let mixed = "(workspace w\n  (graph world :context world\n    (world/sun :name \"w\" (world/sky :name \"s\"))))" in
  check (has (apply mixed (E.Set_arg { node = [ "world"; "@result" ]; key = Pos 0; sub = [];
    value = S.make (S.List [ sym "world/sky" ]) })) "(world/sky)") "a positional argument after a keyword pair is found";
  (* a scene object joins the merge, a World layer goes on top of the stack, deleting takes them out *)
  let scene = "(workspace w\n  (graph g :context sop (sop/box))\n  (graph scene :context scene\n    (let* [a (scene/geometry (ref g)) all (scene/merge a)] all)))" in
  let added = apply scene (E.Add_node { scope = [ "scene" ]; name = "lamp"; expr = S.make (S.List [ sym "scene/light" ]) }) in
  check (has added "(scene/merge a lamp)" && has added "lamp (scene/light)") ("a new object did not join the merge\n" ^ added);
  check (has (apply added (E.Delete_nodes { nodes = [ [ "scene"; "lamp" ] ] })) "(scene/merge a)"
         && not (has (apply added (E.Delete_nodes { nodes = [ [ "scene"; "lamp" ] ] })) "lamp")) "a deleted object stayed in the merge";
  let single = "(workspace w\n  (graph g :context sop (sop/box))\n  (graph scene :context scene (scene/geometry (ref g))))" in
  check (has (apply single (E.Add_node { scope = [ "scene" ]; name = "lamp"; expr = S.make (S.List [ sym "scene/light" ]) }))
           "(scene/merge (scene/geometry (ref g)) lamp)") "a scene that is one object becomes a merge";
  let world = "(workspace w\n  (graph world :context world\n    (let* [sky (world/sky)] sky)))" in
  let layered = apply world (E.Add_node { scope = [ "world" ]; name = "sun"; expr = S.make (S.List [ sym "world/sun" ]) }) in
  check (has layered "sun (world/sun sky)" && has layered "sun)))") ("a new layer is not on top\n" ^ layered);
  let closed = apply layered (E.Delete_nodes { nodes = [ [ "world"; "sun" ] ] }) in
  check (has closed "sky)))" && not (has closed "world/sun")) ("the stack did not close over a deleted layer\n" ^ closed);
  let graph = S.make (S.List [ sym "graph"; sym "g"; S.make (S.Kw "context"); sym "sop"; S.make (S.List [ sym "sop/box" ]) ]) in
  check (has (apply scene (E.Set_graph { name = "g"; form = graph })) "(graph g :context sop"
         && has (apply "(workspace w (graph g :context sop (sop/box)))"
           (E.Set_graph { name = "extra"; form = S.make (S.List [ sym "graph"; sym "extra"; S.make (S.Kw "context"); sym "sop"; S.make (S.List [ sym "sop/box" ]) ]) })) "(graph extra")
    "Set_graph replaces a graph or appends one";
  refused "Set_graph with another name" scene (E.Set_graph { name = "other"; form = graph })

let part11 () = (* atomic: a refused edit changes nothing; labels *)
  let before = norm base in
  (match E.apply catalog (parse base) (E.Rename { node = node [ "b" ]; to_ = "a" }) with
   | Error _ -> () | Ok _ -> fail "expected a refusal");
  check (canon (parse base) = before) "source unchanged after a refusal";
  (* an exception inside a rewrite (here [List.nth] on a [for] with no body) is a refusal too *)
  let broken = parse (g "(let* [a (for)] a)") in
  (match E.apply catalog broken (E.Set_arg { node = node [ "a" ]; key = Kw "skip"; sub = []; value = num "1" }) with
   | Error d -> check (d.code = "E_EDIT" && has d.message "The edit failed") ("a raising rewrite: " ^ d.message)
   | Ok _ -> fail "a raising rewrite was accepted"
   | exception e -> fail ("a rewrite raised " ^ Printexc.to_string e));
  check (canon broken = norm (g "(let* [a (for)] a)")) "source unchanged after a raising rewrite";
  (* an [:active] outside the switch's layouts is refused by name, not by the last resort *)
  List.iter (fun active ->
    let text = Printf.sprintf "(workspace w\n  (graph editor :context editor\n    (let* [a (ui/graph) b (ui/lisp)] (ui/workspace (ui/switch a b :active %s)))))" active in
    List.iter (fun op -> match E.apply catalog (parse text) op with
      | Error d -> check (has d.message "not one of the switch's layouts") (E.label op ^ " :active " ^ active ^ ": " ^ d.message)
      | Ok _ -> fail (E.label op ^ " accepted :active " ^ active))
      [ E.Layout_remove { graph = "editor" }; E.Layout_new { graph = "editor" };
        E.Layout_window { graph = "editor"; kind = "inspector" } ]) [ "5"; "-1" ];
  List.iter (fun (op, label) -> check (E.label op = label) ("label " ^ label))
    [ E.Wrap { nodes = []; loop = For }, "Repeat"; E.Wrap { nodes = []; loop = Fold }, "Iterate";
      E.Unfold { node = []; key = Whole; sub = [] }, "Unfold";
      E.Make_macro { nodes = []; name = ""; holes = [] }, "Make macro";
      E.Rename { node = []; to_ = "" }, "Rename"; E.Hoist { node = [] }, "Move out";
      E.Toggle_bypass { node = [] }, "Bypass"; E.Fold_into { node = [] }, "Fold" ]

let part12 () = (* the 12 fixtures: notes survive, edits round trip *)
  let files = Sys.readdir cases |> Array.to_list |> List.filter (fun f -> Filename.check_suffix f ".lisp")
    |> List.sort compare in
  check (List.length files = 12) "12 fixtures";
  List.iter (fun file ->
    let text = read (Filename.concat cases file) in
    let forms = parse text in
    let graph = match forms with
      | [ { S.node = S.List (_ :: _ :: items); _ } ] ->
          List.find_map (fun (i : S.t) -> match i.node with
            | S.List ({ S.node = S.Sym "graph"; _ } :: { S.node = S.Sym n; _ } :: _) -> Some n
            | _ -> None) items |> Option.get
      | _ -> fail (file ^ ": shape") in
    (* a note on the graph and back leaves the text unchanged *)
    let noted = match E.apply catalog forms (E.Set_note { node = [ graph ]; text = "edited" }) with
      | Ok f -> f | Error d -> fail (file ^ ": " ^ Diagnostic.to_string d) in
    check (canon noted <> canon forms) (file ^ ": note prints");
    (match E.apply catalog noted (E.Set_note { node = [ graph ]; text = "" }) with
     | Ok f -> check (canon f = canon forms) (file ^ ": note round trip")
     | Error d -> fail (file ^ ": " ^ Diagnostic.to_string d))) files;
  (* bloom: rename a graph binding and back; the comments survive *)
  let bloom = parse (read (Filename.concat cases "bloom.lisp")) in
  let there = E.Rename { node = [ "flower"; "bloom" ]; to_ = "blossom" } in
  match E.apply catalog bloom there with
  | Error d -> fail (Diagnostic.to_string d)
  | Ok renamed ->
      check (canon renamed <> canon bloom) "bloom renamed";
      (match E.apply catalog renamed (E.Rename { node = [ "flower"; "blossom" ]; to_ = "bloom" }) with
       | Ok back -> check (canon back = canon bloom) "bloom rename round trip"
       | Error d -> fail (Diagnostic.to_string d))

let part13 () = (* Move a binding, wrap in map / filter, inline a function. *)
  let order = v "(let* [a 1 b 2 c (+ a b)] c)" in
  same "move down" order (E.Move_binding { node = node [ "a" ]; by = 1 }) (v "(let* [b 2 a 1 c (+ a b)] c)");
  same "move up" order (E.Move_binding { node = node [ "b" ]; by = -1 }) (v "(let* [b 2 a 1 c (+ a b)] c)");
  refused "move past a reader" order (E.Move_binding { node = node [ "c" ]; by = -1 });
  refused "move off the end" order (E.Move_binding { node = node [ "a" ]; by = -1 });
  same "wrap in map" (v "(let* [a (+ n 1) r (* 0.5 (count a))] r)") (E.Wrap { nodes = [ node [ "a" ] ]; loop = Map })
    (v "(let* [items (range 6) a (map (fn [i] (+ n 1)) items) r (* 0.5 (count a))] r)");
  same "wrap in filter" (v "(let* [a (< n 1) r (* 0.5 (count a))] r)") (E.Wrap { nodes = [ node [ "a" ] ]; loop = Filter })
    (v "(let* [items (range 6) a (filter (fn [i] (< n 1)) items) r (* 0.5 (count a))] r)");
  same "wrap in map over a selected source" (v "(let* [xs (range 3) a (+ xs 1) r (count a)] r)")
    (E.Wrap { nodes = [ node [ "xs" ]; node [ "a" ] ]; loop = Map })
    (v "(let* [xs (range 3) a (map (fn [i] (+ i 1)) xs) r (count a)] r)");
  let twice = "(workspace w\n  (defn twice :context value [(x : int) (y : int)] (- x (* y 2)))\n  (graph g :context value [(n : int 4)] (let* [y 3 a (twice y n)] a)))" in
  same "inline defn" twice (E.Inline_defn { node = node [ "a" ] })
    "(workspace w\n  (defn twice :context value [(x : int) (y : int)] (- x (* y 2)))\n  (graph g :context value [(n : int 4)] (let* [y 3 a (- y (* n 2))] a)))";
  refused "inline a non-function" twice (E.Inline_defn { node = node [ "y" ] })

let run () =
  List.iter (fun f -> f ()) [ part1; part2; part3; nested_nodes; part4; part5; part6; part7; dialog; part8; part9; part10; part10b; part11; part12; part13 ]
