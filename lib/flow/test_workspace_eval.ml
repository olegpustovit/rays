open Flow

let read path = In_channel.with_open_bin path In_channel.input_all
let catalog = match Catalog.catalog_of_manifest (read "../flow_sop/flow_manifest.sexp") with
  | Ok (catalog, _) -> catalog
  | Error d -> failwith (Diagnostic.to_string d)
let cases = "../../specification/workspace/cases"

let parse source = match Syntax.parse source with
  | Ok forms -> forms
  | Error d -> failwith (Diagnostic.to_string d)
let errors ds = List.filter (fun (d : Diagnostic.t) -> d.severity = Diagnostic.Error_severity) ds
let show_ds ds = String.concat "; " (List.map Diagnostic.to_string ds)
let check source = match Workspace.check catalog (parse source) with
  | Some ws, ds when errors ds = [] -> ws
  | _, ds -> failwith (source ^ "\n  rejected: " ^ show_ds ds)

let sop b = "(workspace w (graph g :context sop " ^ b ^ "))"
let value b = "(workspace w (graph g :context value " ^ b ^ "))"

(* each case is one check.cjs test (or one register rule); failures are collected *)
let passed = ref 0 and failed = ref []
let t name f =
  match f () with
  | () -> incr passed
  | exception e -> failed := (name, Printexc.to_string e) :: !failed

(* ---- running ---- *)
let ok = function Ok r -> r | Error d -> failwith (Diagnostic.to_string d)
let static ?record ?inputs ws = ok (Eval.static ?record ?inputs ws)
let run ?record ?inputs ?(time = 0.) ws = ok (Eval.run ?record ?inputs ~time ws)
let result (r : Eval.t) name = List.assoc name r.results
let g_value ?time src = result (run ?time (check src)) "g"

let rec same (a : Eval.value) (b : Eval.value) = match a, b with
  | (Int _ | Float _), (Int _ | Float _) ->
      let n = function Eval.Int n -> float_of_int n | Float f -> f | _ -> nan in
      n a = n b
  | List x, List y -> Array.length x = Array.length y && Array.for_all2 same x y
  | Record x, Record y ->
      List.length x = List.length y && List.for_all2 (fun (k, u) (l, v) -> k = l && same u v) x y
  | Vec3 (a, b, c), Vec3 (x, y, z) -> a = x && b = y && c = z
  | _ -> a = b
let is src want =
  let got = g_value src in
  if not (same got want) then
    failwith (Printf.sprintf "%s: got %s, want %s" src (Eval.show got) (Eval.show want))
let ints l = Eval.List (Array.of_list (List.map (fun n -> Eval.Int n) l))

(* an error at run time: the code, and text in the message *)
let err ?(text = "") ?inputs src code =
  let has (d : Diagnostic.t) =
    let n = String.length text and m = String.length d.message in
    let rec at i = i + n <= m && (String.sub d.message i n = text || at (i + 1)) in at 0 in
  match Eval.static ?inputs (check src) with
  | Error d when d.code = code && has d -> ()
  | Error d -> failwith (Printf.sprintf "%s\n  expected %s %S, got %s" src code text (Diagnostic.to_string d))
  | Ok _ -> failwith (src ^ "\n  ran; expected " ^ code)

let records ?(time = 0.) src =
  (run ~record:true ~time (check src)).records
let recs rs path = match List.assoc_opt path rs with Some l -> l | None -> failwith ("no records at " ^ String.concat "/" path)
let first_at rs path = snd (List.hd (recs rs path))
let bound src name = first_at (records src) [ "g"; name ]

let nodes_of (r : Eval.t) kind = List.filter (fun (n : Eval.node) -> n.kind = kind) (Array.to_list r.plan.nodes)
let arg (n : Eval.node) k = List.assoc k n.args
let count_of r kind = List.length (nodes_of r kind)

let () =
  if Array.length Sys.argv=2 && Sys.argv.(1)="--bench-records" then begin
    let v=result (static (check (value
      "(let* [a (fold [a {:n 0.}] [x (array/range 4096)] (assoc a :n (+ a.n t)))] a.n)"))) "g" in
    let force time=ignore (ok (Eval.force v ~live:(Input.at_time time))) in
    force 0.1;
    let samples=List.init 3 (fun _ ->
      Gc.full_major ();
      let bytes=Gc.allocated_bytes () and start=Unix.gettimeofday () in
      for i=1 to 30 do force (float i /. 100.) done;
      Unix.gettimeofday () -. start,Gc.allocated_bytes () -. bytes) in
    let seconds,bytes=List.nth (List.sort (fun (a,_) (b,_) -> Float.compare a b) samples) 1 in
    Printf.printf "Record fold: %.6fs, %.0f allocated bytes; 30 frames x 4096 items; domains=1; median of 3\n" seconds bytes;
    exit 0
  end

module Seen_bench_term = Hashtbl.Make (struct
  type t = Workspace.term
  let equal = ( == )
  let hash = Hashtbl.hash
end)

let legacy_kind_calls root =
  let seen = Seen_bench_term.create 256 and out = ref [] in
  let rec calls (term : Workspace.term) =
    if not (Seen_bench_term.mem seen term) then begin
      Seen_bench_term.add seen term ();
      match term.node with
      | Workspace.Call {kind; args; _} -> out := kind :: !out; fields args
      | Op {args; _} | Record args -> fields args
      | Call_fn {args; body; _} -> List.iter calls args; Option.iter calls body
      | Graph_ref {inputs; _} -> fields inputs
      | Let (bindings, body) -> List.iter (fun (_, value) -> calls value) bindings; calls body
      | State {init; step; _} -> calls init; calls step
      | Loop {accs; clauses; body; _} -> List.iter (fun (_, value) -> calls value) (accs @ clauses); calls body
      | With_error (a, b) -> calls a; calls b
      | Error_rows {rows; mapper; _} -> calls rows; calls mapper
      | If (a, b, c) -> calls a; calls b; calls c
      | Cond (arms, default) -> List.iter (fun (a, b) -> calls a; calls b) arms; calls default
      | Case (scrutinee, arms, default) -> calls scrutinee; List.iter (fun (_, value) -> calls value) arms; calls default
      | Fn {body; capture; _} -> calls body; Option.iter calls capture
      | Hof (_, terms) | Vec terms | List_lit terms | Str terms | List_op (_, terms) -> List.iter calls terms
      | Get (value, _) | Bypass value | Expanded {body = value; _} -> calls value
      | Assoc (value, fields_) -> calls value; fields fields_
      | Lit _ | Text _ | Nil | Time | Ref_binding _ | Fn_ref _ -> ()
    end
  and fields fields_ = List.iter (fun (_, value) -> calls value) fields_ in
  calls root;
  List.sort_uniq String.compare !out

let () = if Array.length Sys.argv=2 && Sys.argv.(1)="--bench-kind-calls" then begin
  let files = Sys.readdir cases |> Array.to_list
    |> List.filter (fun file -> Filename.check_suffix file ".lisp") |> List.sort String.compare in
  let layout = read "../../sketches/ws_layout.rays" |> parse
    |> List.find (fun form -> Syntax.head form = Some "workspace") in
  let layout = match Workspace.check catalog [layout] with
    | Some workspace, ds when errors ds = [] -> workspace
    | _, ds -> failwith (show_ds ds) in
  let workspaces = layout
    :: List.map (fun file -> check (read (Filename.concat cases file))) files in
  let terms = List.concat_map (fun (ws : Workspace.t) ->
    List.map (fun (node : Workspace.node_definition) -> node.body.body) ws.nodes
    @ List.map (fun (graph : Workspace.graph) -> graph.body) (ws.graphs @ ws.defs)) workspaces in
  let current () = List.map Workspace.kind_calls terms
  and legacy () = List.map legacy_kind_calls terms in
  let expected = legacy () in
  assert (current () = expected);
  let measure run =
    let samples = List.init 9 (fun _ ->
      Gc.full_major ();
      let allocated = Gc.allocated_bytes () and start = Unix.gettimeofday () in
      for _ = 1 to 100 do ignore (run ()) done;
      Unix.gettimeofday () -. start, Gc.allocated_bytes () -. allocated) in
    List.nth (List.sort (fun (a, _) (b, _) -> Float.compare a b) samples) 4 in
  let seconds, bytes = measure current and legacy_seconds, legacy_bytes = measure legacy in
  let rec count n term = List.fold_left count (n + 1) (Workspace.children term) in
  let size = List.fold_left count 0 terms in
  Printf.printf "Workspace kind_calls ws_layout + all workspace cases: %d terms; 100 full passes; domains=1; median of 9\nold %.6fs %.0f allocated bytes\nshared %.6fs %.0f allocated bytes\n"
    size legacy_seconds legacy_bytes seconds bytes;
  exit 0
end

(* IR walk, for the static types *)
let rec walk f (x : Workspace.term) =
  f x;
  (* This lookup tests the declared function body, not call-site specializations
     retained by Call_fn/Fn for evaluation. *)
  let children = match x.node with
    | Workspace.Call_fn {args; _} -> args
    | Workspace.Fn {body; _} -> [body]
    | _ -> Workspace.children x in
  List.iter (walk f) children
let ty_at (ws : Workspace.t) path =
  let found = ref None in
  List.iter (fun (g : Workspace.graph) ->
    walk (fun x -> if x.path = Some path then found := Some x.ty) g.body) (ws.graphs @ ws.defs);
  match !found with Some ty -> Ty.to_string ty | None -> failwith ("no term at " ^ String.concat "/" path)

let fixtures () =
  Sys.readdir cases |> Array.to_list |> List.filter (fun f -> Filename.check_suffix f ".lisp")
  |> List.sort compare
  |> List.map (fun f -> (Filename.remove_extension f, read (Filename.concat cases f)))

let () = (* the 12 fixtures run, deterministically, with unique plan keys, at any time *)
  List.iter (fun (name, source) -> t ("case " ^ name ^ " runs") (fun () ->
    let ws = check source in
    let a = run ws and b = run ws in
    if a <> b then failwith "two runs differ";
    assert (Array.length a.authored = Array.length a.plan.nodes);
    let forms = Hashtbl.create 128 in
    let rec remember (form : Syntax.t) = Hashtbl.replace forms form.id (); List.iter remember (Syntax.children form) in
    List.iter remember ws.source;
    assert (Array.for_all (fun id -> id = 0 || Hashtbl.mem forms id) a.authored);
    assert (a.authored = (static ws).authored);
    let keys = Array.to_list a.plan.nodes |> List.map (fun (n : Eval.node) -> (n.inst, n.site, n.iter)) in
    if List.length (List.sort_uniq compare keys) <> List.length keys then failwith "plan keys collide";
    (* structure never depends on t: only argument values change *)
    let c = run ~time:3.1 ws in
    let shape (r : Eval.t) = Array.map (fun (n : Eval.node) -> (n.kind, n.site, n.iter)) r.plan.nodes in
    if shape a <> shape c then failwith "structure changed with t";
    if Array.length a.plan.nodes = 0 && name <> "kit" && name <> "variations" && name <> "tiles" then
      failwith "no nodes")) (fixtures ())


(* ---- check.cjs: value cases ---- *)

let () = (* the first block *)
  t "declared nested data is concrete; generic list and record selection stays lazy" (fun () ->
    let host body="(workspace w (graph g :context host "^body^"))" in
    let encode=Option.get(Op.find "text/table_encode" Context.value) in
    let consume : Op.t={encode with name="test/table_record";
      signature={encode.signature with pos=["data",Ty.Record[
        "rows",List(List Text);"label",Text]]};
      body=(fun ~live ~node args->match List.assoc "data" args with
        | Value.Record fields->(match List.assoc "label" fields,
            encode.body ~live ~node ["rows",List.assoc "rows" fields]with
            |Value.Text label,Value.Text table->Value.Text(label^table)|_->assert false)
        |_->assert false)} in
    let forms=parse(host "(test/table_record {:rows (list (list (str t) \"猫\")) :label (str t)})") in
    let workspace=match Workspace.check ~ops:[consume] catalog forms with
      |Some workspace,[]->workspace|_,ds->failwith(show_ds ds) in
    assert(result(run ~time:0.25 workspace)"g"=Eval.Text "0.250.25\t猫");
    assert(g_value ~time:0.25 (host
      "(text/table_encode (list (list (str t) \"猫\")))")=Eval.Text "0.25\t猫");
    List.iter(fun body->assert(g_value ~time:0.25(value body)=Eval.Float 0.25))
      ["(first (list t (array/nth (array/float 0) (floor t))))";
       "(nth (list t (array/nth (array/float 0) (floor t))) 0)";
       "(get {:selected t :unused (array/nth (array/float 0) (floor t))} :selected)"]);
  t "sum" (fun () -> is (value "(sum [k (range 4)] (* k 2))") (Eval.Int 12));
  t "int truncates, round is half away from zero, ceil and float" (fun () ->
    is (value "(int -2.7)") (Eval.Int (-2)); is (value "(round 2.5)") (Eval.Int 3);
    is (value "(round -2.5)") (Eval.Int (-3)); is (value "(ceil 2.1)") (Eval.Int 3);
    is (value "(float 3)") (Eval.Float 3.); is (value "(/ (float 3) 2)") (Eval.Float 1.5));
  t "sum of nothing is 0" (fun () -> is (value "(sum [k (range 0)] 1.5)") (Eval.Int 0));
  t "fold" (fun () -> is (value "(fold [a 1] [i (range 5)] (* a 2))") (Eval.Int 32));
  t "fold of nothing is its initial value" (fun () -> is (value "(fold [a 7] [i (range 0)] (* a 2))") (Eval.Int 7));
  t "scan collects each step" (fun () ->
    is (value "(let* [xs (scan [a 1] [i (range 3)] (+ a i))] (count xs))") (Eval.Int 3);
    assert (same (bound (value "(let* [z (scan [a 1] [i (range 4)] (* a 2))] 1)") "z") (ints [ 2; 4; 8; 16 ]));
    assert (same (bound (value "(let* [z (scan [a 1] [i (range 0)] (* a 2))] 1)") "z") (ints [])));
  t "product is row-major, last clause fastest" (fun () ->
    assert (same (bound (value "(let* [z (for [x (range 2) y (range 3)] (+ (* x 10) y))] (count z))") "z")
      (ints [ 0; 1; 2; 10; 11; 12 ])));
  t "later clauses read earlier names" (fun () ->
    is (value "(let* [z (for [x (range 4) y (range x)] y)] (count z))") (Eval.Int 6));
  t "graph inputs and ref overrides" (fun () ->
    let ws = check "(workspace w (graph a :context value [(n : int 2)] (* n 3)) (graph b :context value (+ (ref a) (ref a :n 5))))" in
    let r = static ws in
    assert (same (result r "b") (Eval.Int 21));
    (* a, b, and a with n=5: an override tuple is one cached instance *)
    assert (Array.length r.plan.instances = 3));
  t "rand is a pure hash" (fun () ->
    let a = g_value (value "(value/rand 3 4 5)") and b = g_value (value "(value/rand 3 4 5)") in
    match a, b with
    | Eval.Float x, Eval.Float y -> assert (x = y && x >= 0. && x < 1.)
    | _ -> failwith "not a float");
  t "per-iteration records carry their index" (fun () ->
    let bloom = read (Filename.concat cases "bloom.lisp") in
    let rs = records bloom in
    let r = recs rs [ "flower"; "ring"; "u" ] in
    assert (List.length r = 12 && fst (List.nth r 3) = [ 3 ]));
  t "loops tag what they make" (fun () ->
    let r = run (check (read (Filename.concat cases "bloom.lisp"))) in
    assert (Array.exists (fun (n : Eval.node) ->
      n.iter = [ 5 ] && List.take 2 n.site = [ "flower"; "ring" ]) r.plan.nodes));
  t "only the taken branch runs" (fun () ->
    (* the study runs a literal (range 9999) in the untaken arm; the checker rejects that literal,
       so the count is a graph input *)
    let src b = "(workspace w (graph g :context value [(n : int 9999)] " ^ b ^ "))" in
    is (src "(if (< 1 2) 1 (count (range n)))") (Eval.Int 1);
    err (src "(if (> 1 2) 1 (count (range n)))") "E_ITER_BOUND" ~text:"exceeds 4,096")

let () = (* 1. function values *)
  t "fn: let-bound function is called positionally" (fun () ->
    is (value "(let* [f (fn [a b] (+ a b))] (f 1 2))") (Eval.Int 3));
  t "fn: annotated parameters coerce" (fun () ->
    assert (bound (value "(let* [f (fn [(a : float) b] (* a b)) r (f 1 2)] 1)") "r" = Eval.Float 2.));
  t "fn: closures capture the lexical environment" (fun () ->
    is (value "(let* [k 10 f (fn [x] (+ x k))] (f 1))") (Eval.Int 11));
  t "fn: a local function wins over an operator of the same name" (fun () ->
    is (value "(let* [min (fn [a b] (+ a b))] (min 1 2))") (Eval.Int 3));
  t "fn: calls are iteration frames with per-call records" (fun () ->
    let src = value "(let* [f (fn [a] (let* [s (* a 2)] (+ s 1))) x (f 5) y (sum [i (range 3)] (f i))] (+ x y))" in
    let ws = check src in
    let rs = (run ~record:true ws).records in
    let at p = List.map (fun (it, v) -> (it, v)) (recs rs p) in
    assert (at [ "g"; "f"; "@result" ]
            = [ ([ 0 ], Eval.Int 11); ([ 0; 1 ], Eval.Int 1); ([ 1; 2 ], Eval.Int 3); ([ 2; 3 ], Eval.Int 5) ]);
    assert (List.map snd (at [ "g"; "f"; ":a" ]) = [ Eval.Int 5; Int 0; Int 1; Int 2 ]);
    assert (List.nth (at [ "g"; "f"; "s" ]) 3 = ([ 2; 3 ], Eval.Int 4));
    (* the IR types a fn body with its declared types, or Any: call-site refinement (int) is not kept *)
    assert (ty_at ws [ "g"; "f"; "s" ] = "float" && ty_at ws [ "g"; "f" ] = "fn"));
  t "fn: inline functions are recorded at their nested card path" (fun () ->
    let rs = records (value "(let* [xs (map (fn [x] (* x 2)) (list 1 2))] (count xs))") in
    assert (List.map snd (recs rs [ "g"; "xs#0"; ":x" ]) = [ Eval.Int 1; Int 2 ]));
  t "draw: inline calls are recorded at their graph card paths" (fun () ->
    let ws = check {|(workspace w (graph g :context draw
      (draw/merge (draw/background "#0b0b0e") (draw/background "#ffffff"))))|} in
    let evaluated = static ~record:true ws in
    List.iter (fun path ->
      assert (List.exists (function _, Eval.Deferred (ty, _) -> ty = Ty.drawing | _ -> false)
        (recs evaluated.records path))) [["g"; "@result#0"]; ["g"; "@result#1"]]);
  t "draw: repeated inline macros keep independent image sites" (fun () ->
    let ws = check {|(workspace w
      (defmacro panel [color]
        `(draw/image (image/map (fn [uv#] ~color) :width 2 :height 2)))
      (graph g :context draw
        (draw/merge (panel [1 0 0 1]) (panel [0 1 0 1]) (panel [0 0 1 1]))))|} in
    let evaluated = static ~record:true ws in
    let images = Array.to_list evaluated.plan.nodes
      |> List.filter (fun (n : Eval.node) -> n.kind = "image/map") in
    let sites = List.map (fun (n : Eval.node) -> n.inst, n.site, n.iter) images in
    assert (List.length images = 3 && List.length (List.sort_uniq compare sites) = 3));
  t "fn: map is typed statically through the function body" (fun () ->
    let ws = check (sop "(let* [cs (map (fn [x] (sop/box :size x)) (list 1 2))] (sop/merge cs))") in
    assert (ty_at ws [ "g"; "cs" ] = "list:geometry" && ty_at ws [ "g"; "cs#0"; "@result" ] = "geometry"));
  t "fn: map over a let-bound fn tags shapes with the call index" (fun () ->
    let r = run (check (sop "(let* [mk (fn [r] (sop/box :size r)) cs (map mk (list 0.1 0.2 0.3))] (sop/merge cs))")) in
    assert (List.map (fun (n : Eval.node) -> (n.site, n.iter)) (nodes_of r "sop/box")
            = List.map (fun k -> ([ "g"; "mk"; "@result" ], [ k ])) [ 0; 1; 2 ]));
  t "fn: maps inside opaque expressions keep distinct sites" (fun () ->
    let r = run (check (sop "(let* [unused (list (count (map (fn [x] (sop/box :size x)) (list 0.1))) (count (map (fn [x] (sop/box :size x)) (list 0.2))))] (sop/box))")) in
    let sites = List.map (fun (n : Eval.node) -> n.site, n.iter) (nodes_of r "sop/box") in
    assert (List.length sites = 3 && List.length (List.sort_uniq compare sites) = 3));
  t "fn: defn and operator names are function values" (fun () ->
    is "(workspace w (defn twice :context value [(x : float)] (* x 2)) (graph g :context value (+ (reduce + 0 (map twice (list 1 2 3))) (first (map sin (list 0))))))" (Eval.Int 12));
  t "reduce: an int seed keeps a float sum" (fun () ->
    is (value "(reduce + 0 (list 0.5 0.25))") (Eval.Float 0.75));
  t "fn: user HOF with a fn-typed defn input" (fun () ->
    let ring = "(defn ring :context sop [(n : int 8) (make : fn)] (sop/merge (map make (range n))))" in
    let r = run (check ("(workspace w " ^ ring ^ " (defn mark :context sop [(i : int)] (sop/box :size (* 0.1 (+ i 1)))) (graph g :context sop (sop/merge (ring :n 3 :make mark) (let* [mk (fn [i] (sop/box))] (ring :n 2 :make mk)))))")) in
    assert (count_of r "sop/box" = 5 && count_of r "sop/merge" = 3);
    (* the two ring call sites are distinct subtrees; mk's two calls are iterations 0 and 1 *)
    assert (List.sort compare (List.map (fun (n : Eval.node) -> n.iter) (nodes_of r "sop/box"))
            = [ [ 0 ]; [ 0 ]; [ 1 ]; [ 1 ]; [ 2 ] ]));
  t "fn: defn inputs typed as lists and records" (fun () ->
    is "(workspace w (defn tot :context value [(xs : (list float)) (r : {:k float})] (* r.k (reduce + 0 xs))) (graph g :context value (tot (list 1 2) {:k 2 :extra 1})))" (Eval.Int 6));
  t "hof: map zips up to three lists to the shortest" (fun () ->
    assert (same (bound (value "(let* [z (map (fn [a b c] (+ a (+ b c))) (list 1 2 3) (list 10 20) (list 100 200 300))] 1)") "z") (ints [ 111; 222 ])));
  t "hof: filter" (fun () ->
    assert (same (bound (value "(let* [z (filter (fn [x] (> x 1)) (list 1 2 3))] 1)") "z") (ints [ 2; 3 ])));
  t "hof: sort-by is stable on a numeric key" (fun () ->
    let l a b = Eval.List [| Eval.Int a; Int b |] in
    assert (same (bound (value "(let* [z (sort-by (fn [p] (first p)) (list (list 2 0) (list 1 1) (list 2 2) (list 1 3)))] 1)") "z")
      (Eval.List [| l 1 1; l 1 3; l 2 0; l 2 2 |])));
  t "hof: map of an empty list keeps its static type" (fun () ->
    let src = sop "(let* [z (map (fn [x] (sop/box :size x)) (list))] (sop/merge z))" in
    assert (same (bound src "z") (ints []));
    assert (ty_at (check src) [ "g"; "z" ] = "list:geometry"))

let () = (* 2. lists and destructuring *)
  t "list: int and float promote to float" (fun () ->
    let src = value "(let* [z (list 1 2.5)] 1)" in
    assert (bound src "z" = Eval.List [| Float 1.; Float 2.5 |]);
    assert (ty_at (check src) [ "g"; "z" ] = "list:float"));
  t "list: (list) is a list of any" (fun () ->
    let src = value "(let* [z (list)] 1)" in
    assert (bound src "z" = Eval.List [||] && ty_at (check src) [ "g"; "z" ] = "list:any"));
  t "list: first last rest nth concat reverse take drop" (fun () ->
    assert (bound (value "(let* [s (str (first (list 4 5)) (last (list 4 5)) (rest (list 1 2 3)) (nth (list 7 8) 1) (concat (list 1) (list) (list 2 3)) (reverse (list 1 2)) (take 2 (list 1 2 3)) (drop 2 (list 1 2 3)))] 1)") "s"
            = Eval.Text "45[2 3]8[1 2 3][2 1][1 2][3]"));
  t "list: nth out of range names index and length" (fun () ->
    err (value "(nth (list 1 2) 5)") "E_LIST_RANGE" ~text:"index 5 is out of range for a list of length 2";
    err (value "(nth (list 1 2) -1)") "E_LIST_RANGE");
  t "list: first of an empty list is an error" (fun () ->
    err (value "(first (list))") "E_LIST_RANGE" ~text:"empty list";
    err (value "(last (list))") "E_LIST_RANGE");
  t "pattern: [a b] in let* records whole and parts" (fun () ->
    let src = value "(let* [[a b] (list 1 2)] (+ a b))" in
    is src (Eval.Int 3);
    let rs = records src in
    assert (same (first_at rs [ "g"; "[a b]" ]) (ints [ 1; 2 ]) && first_at rs [ "g"; "b" ] = Eval.Int 2));
  t "pattern: [x y z] over a vec3 and {:keys} over a record" (fun () ->
    is (value "(let* [[x y z] [1 2 3] {:keys [a b]} {:a 10 :b 20}] (+ (* x a) (* z b)))") (Eval.Int 70));
  t "pattern: in zone clauses and fn parameters" (fun () ->
    let src = value "(let* [f (fn [[a b]] (* a b)) s (sum [[a b] (list (list 1 2) (list 3 4))] (+ (f (list a b)) 0))] s)" in
    is src (Eval.Int 14);
    let rs = records src in
    assert (same (snd (List.nth (recs rs [ "g"; "s"; ":[a b]" ]) 1)) (ints [ 3; 4 ]));
    assert (List.length (recs rs [ "g"; "s"; ":a" ]) = 2 && List.length (recs rs [ "g"; "s"; ":b" ]) = 2);
    assert (List.length (recs rs [ "g"; "f"; ":[a b]" ]) = 2));
  t "pattern: zoneVars reports the pattern and its names" (fun () ->
    let ws = check (value "(let* [z (for [[a b] (list (list 1 2)) {:keys [c]} (list {:c 1})] a)] 1)") in
    match (List.hd ws.graphs).body.node with
    | Workspace.Let ([ (_, { node = Workspace.Loop { clauses = [ (Workspace.Seq [ Name "a"; Name "b" ], _); (Keys [ "c" ], _) ]; _ }; _ }) ], _) -> ()
    | _ -> failwith "unexpected clause patterns")

let () = (* 3. records *)
  t "record: literal, dotted access and chains" (fun () ->
    let src = value "(let* [r {:a 1 :b {:c [1 2 3]}}] (+ r.a r.b.c.y))" in
    is src (Eval.Int 3);
    assert (ty_at (check src) [ "g"; "r" ] = "rec{a:int,b:rec{c:vec3}}"));
  t "record: get and assoc" (fun () -> is (value "(let* [r {:a 1} s (assoc r :a 5 :c 2)] (+ (get s :a) s.c))") (Eval.Int 7));
  t "record: values is a record literal" (fun () ->
    assert (bound (value "(let* [r (values :a 1 :b 2.5)] 1)") "r" = Eval.Record [ ("a", Int 1); ("b", Float 2.5) ]));
  t "record: a record accumulator carries several values" (fun () ->
    is (value "(let* [s (fold [{:keys [a b]} {:a 0 :b 1}] [i (range 10)] {:a b :b (+ a b)})] s.a)") (Eval.Int 55))

let () = (* 4. cond, case, str *)
  t "cond picks the first true arm" (fun () -> is (value "(cond (< 2 1) 1 (> 2 1) 2 :else 3)") (Eval.Int 2));
  t "cond is lazy at run time" (fun () ->
    is "(workspace w (graph g :context value [(n : int 9999)] (cond true 1 :else (count (range n)))))" (Eval.Int 1));
  t "case matches literals" (fun () ->
    is (value "(case 2 1 10 2 20 :else 0)") (Eval.Int 20);
    is (value "(case 9 1 10 :else 0)") (Eval.Int 0);
    is (value "(case \"b\" \"a\" 1 \"b\" 2 :else 0)") (Eval.Int 2);
    is (value "(case true false 1 true 2 :else 0)") (Eval.Int 2));
  t "str formats values" (fun () ->
    assert (bound (value "(let* [s (str \"a\" 1 2.5 true [1 2 3] 1.23456 (list 1 2) {:a 1} (/ 1 3))] 1)") "s"
            = Eval.Text "a12.5true[1 2 3]1.2346[1 2]{:a 1}0.3333"));
  t "str computes group names" (fun () ->
    let r = run (check (sop "(sop/blast (sop/group_bounds (sop/box) :name (str \"floor_\" 2)) :group (str \"floor_\" 2))")) in
    assert (arg (List.hd (nodes_of r "sop/blast")) "group" = Eval.Text "floor_2");
    assert (arg (List.hd (nodes_of r "sop/group_bounds")) "name" = Eval.Text "floor_2"))

let () = (* 5. macros *)
  let radial = "(defmacro radial [i n body] `(sop/merge (for [~i (range ~n)] (sop/transform ~body :rotate [0 (* (/ ~i ~n) 6.2832) 0]))))" in
  t "macro: caller symbols may bind; expansion runs at path~for" (fun () ->
    let src = "(workspace w " ^ radial ^ " (graph g :context sop (let* [ring (radial k 12 (sop/box :size (+ 0.1 (* k 0.01))))] ring)))" in
    let r = run ~record:true (check src) in
    assert (count_of r "sop/transform" = 12);
    assert (List.length (recs r.records [ "g"; "ring"; "~for"; ":k" ]) = 12));
  t "macro: rest parameters splice" (fun () ->
    is "(workspace w (defmacro all [& xs] `(+ 0 (+ ~@xs))) (graph g :context value (all 1 2)))" (Eval.Int 3));
  t "macro: legacy value templates still work" (fun () ->
    is "(workspace w (defmacro twice [x] `(+ ~x ~x)) (graph g :context value (twice 3)))" (Eval.Int 6))

let () = (* 6. bypass, notes *)
  t "bypass passes the first input through" (fun () ->
    let r = run (check (sop "(let* [a (sop/box) b ^:bypass (sop/transform a :translate [5 0 0])] b)")) in
    assert (Array.length r.plan.nodes = 1 && result r "g" = Eval.Deferred (Ty.geometry, 0));
    (* a bypassed value call passes its first input, coerced *)
    is (value "(let* [a 2 b ^:bypass (+ a 3)] b)") (Eval.Int 2));
  t "notes survive print and the workspace still runs" (fun () ->
    let noted = "; lead\n(workspace w\n  ; a helper\n  (defn f :context value [(x : float)] (* x 2))\n  (graph g :context value\n    (let* [; first\n           a 1\n           ; the pattern\n           [b c] (list 1 2)\n           q (f a)]\n      ; result note\n      (+ a (+ b (* c q)))))\n  ; tail\n  )" in
    let printed = fst (Lisp.print (parse noted)) in
    is printed (Eval.Int 6))

let () = (* 7. round trip and running the kitchen sink *)
  t "round trip: print(read(print(x))) is stable for every new construct" (fun () ->
    let radial = "(defmacro radial [i n body] `(sop/merge (for [~i (range ~n)] (sop/transform ~body :rotate [0 (* (/ ~i ~n) 6.2832) 0]))))" in
    let all = "(workspace kitchen\n  ; macros\n  " ^ radial ^ "\n  (defmacro all [& xs] `(sop/merge ~@xs))\n  (defmacro add1 [a] `(let* [t# ~a] (+ t# 1)))\n  (defmacro twice [x] `(+ ~x ~x))\n  (defn ring :context sop [(n : int 8) (make : fn) (opts : {:scale float :tags (list text)} {:scale 1.0 :tags (list \"a\")})]\n    (sop/transform (sop/merge (map make (range n))) :uniform_scale opts.scale))\n  (graph g :context sop [(seed : int 3)]\n    (let* [; a local function\n           petal (fn [(i : int) [w h]]\n                   (sop/transform (sop/box :size [w h 0.1]) :rotate [0 0 (* i 0.5)]))\n           sizes (map (fn [k] (list (+ 0.1 (* k 0.01)) 0.5)) (range 12))\n           petals (map petal (range 12) sizes)\n           {:keys [a b]} {:a 1 :b (twice 2)}\n           [x y z] [a b 3]\n           kind (cond (< a 1) \"small\"\n                      (< a 5) \"medium\"\n                      :else \"large\")\n           steps (case seed 1 4 2 8 :else 12)\n           order (sort-by (fn [p] (- 0 (nth p 0))) sizes)\n           kept (filter (fn [p] (> (first p) 0.12)) (concat (take 3 sizes) (drop 9 sizes) (reverse (rest sizes))))\n           total (reduce + 0 (map (fn [p] (last p)) kept))\n           name (str \"floor_\" (add1 steps) \"_\" kind)\n           state (fold [{:keys [n acc]} (values :n 0 :acc 1.0)]\n                       [i (range steps)]\n                   (assoc {:n (+ n 1) :acc (* acc 0.9)} :acc (get {:acc (* acc 0.9)} :acc)))\n           ring2 (radial k 6 (sop/box :size (+ 0.1 (* k 0.01))))\n           faded ^:bypass (sop/subdivide ring2 :iterations 2)\n           shapes (all (sop/merge petals) faded (ring :n 3 :make (fn [i] (sop/box :size 0.05))) (sop/group_bounds (sop/box) :name name))\n           ; unused but checked\n           spare (count (list))]\n      ; the result\n      (sop/transform shapes :uniform_scale (* state.acc (* x (+ y (+ z total)))))))\n  ; trailing\n  )" in
    let printed = fst (Lisp.print (parse all)) in
    assert (fst (Lisp.print (parse printed)) = printed);
    let ws = check printed in
    let r = run ~record:true ws in
    assert (Array.length r.plan.nodes > 0);
    assert (ty_at ws [ "g"; "state" ] = "rec{n:int,acc:float}");
    assert (same (first_at r.records [ "g"; "[x y z]" ]) (Eval.Vec3 (1., 4., 3.)) || same (first_at r.records [ "g"; "[x y z]" ]) (ints [ 1; 4; 3 ])))

let () = (* t: live values and the split evaluation *)
  t "t drives parameters: the same network shape at every time, different values" (fun () ->
    let ws = check (sop "(let* [n 5] (sop/merge (for [i (range n)] (sop/box :size 0.1 :center [(+ i (sin t)) 0 0]))))") in
    let a = run ~time:0. ws and b = run ~time:1.2 ws in
    assert (count_of a "sop/box" = 5 && count_of b "sop/box" = 5);
    assert (a.plan.nodes <> b.plan.nodes);
    let centre (r : Eval.t) = arg (List.nth (nodes_of r "sop/box") 2) "center" in
    assert (centre a = Eval.Vec3 (2., 0., 0.) && centre b <> centre a));
  t "the Orrery case keeps its shape over time" (fun () ->
    let ws = check (read (Filename.concat cases "orrery.lisp")) in
    let n time = Array.length (run ~time ws).plan.nodes in
    assert (n 0. = n 3.1 && n 0. > 0));
  t "static leaves live terms as residuals, forced by time" (fun () ->
    let ws = check (sop "(let* [a (sop/box) s (* t 2) b (sop/transform a :translate [s 0 0] :uniform_scale 2)] b)") in
    let r = static ws in
    let n = List.hd (nodes_of r "sop/transform") in
    (match arg n "translate", arg n "uniform_scale" with
     | Eval.Residual res, Eval.Int 2 ->
         assert (Eval.is_live (arg n "translate"));
         assert (Eval.residual_eval res ~live:(Input.at_time (3. )) = Ok (Eval.Vec3 (6., 0., 0.)));
         assert (Eval.residual_eval res ~live:(Input.at_time (0.5 )) = Ok (Eval.Vec3 (1., 0., 0.)))
     | _ -> failwith "translate is not a residual");
    assert (Eval.force (arg n "translate") ~live:(Input.at_time (1. )) = Ok (Eval.Vec3 (2., 0., 0.)));
    assert (Eval.force (Eval.Int 4) ~live:(Input.at_time (1. )) = Ok (Eval.Int 4)));
  t "residuals: values, if on values, fold, fn, ref and shared chains" (fun () ->
    let at src time = match Eval.run ~time (check src) with
      | Ok r -> result r "g" | Error d -> failwith (Diagnostic.to_string d) in
    let live_of src = result (static (check src)) "g" in
    assert (Eval.is_live (live_of (value "(* t 2)")));
    assert (not (Eval.is_live (live_of (value "(* 3 2)"))));
    assert (same (at (value "(if (> (sin t) 0.5) 1 0.5)") 1.6) (Eval.Float 1.));
    assert (same (at (value "(if (> (sin t) 0.5) 1 0.5)") 0.) (Eval.Float 0.5));
    assert (same (at (value "(fold [a 0] [i (range 3)] (+ a t))") 2.) (Eval.Int 6) || same (at (value "(fold [a 0] [i (range 3)] (+ a t))") 2.) (Eval.Float 6.));
    assert (same (at (value "(let* [f (fn [x] (+ x t)) a (f 1)] (* a 2))") 3.) (Eval.Float 8.));
    assert (same (at "(workspace w (graph a :context value [(n : int 2)] (* n t)) (graph g :context value (+ (ref a) (ref a :n 5))))" 2.) (Eval.Float 14.));
    (* a long chain that reads its predecessor twice is shared, not exponential *)
    let chain = String.concat " " (List.init 40 (fun i -> Printf.sprintf "a%d (+ a%d a%d)" (i + 1) i i)) in
    assert (same (at (value ("(let* [a0 t " ^ chain ^ "] (* a40 0))")) 1.) (Eval.Float 0.));
    (* structure is identical at every time *)
    let ws = check (sop "(sop/merge (for [i (range 4)] (sop/box :center [(sin (+ t i)) 0 0])))") in
    let shape time = Array.map (fun (n : Eval.node) -> (n.kind, n.site, n.iter)) (run ~time ws).plan.nodes in
    assert (shape 0. = shape 5.3 && shape 0. = shape 100.))

let () =
  t "named argument routing preserves 16 and 17 distinct slots" (fun () ->
    List.iter (fun count ->
      let fields = List.init count (fun index -> "slot" ^ string_of_int index, Ty.Float) in
      let operator = { (Option.get (Op.find "sin" Context.value)) with
        name = "test/slots"; signature = {pos=fields;opt=[];rest=None;kw=[]};
        out = (fun _ -> Ty.Float); any_num = false; arithmetic = None;
        body = (fun ~live:_ ~node:_ args ->
          assert (List.map fst args = List.map fst fields);
          Value.Float (List.fold_left (fun sum (_, value) -> sum +. Value.num value) 0. args)) } in
      let source = value ("(test/slots " ^ String.concat " " (List.init count (fun index -> string_of_int index ^ ".0")) ^ ")") in
      let workspace = match Workspace.check ~ops:[operator] catalog (parse source) with
        | Some workspace, [] -> workspace | _, ds -> failwith (show_ds ds) in
      assert (result (static workspace) "g" = Eval.Float (float (count * (count - 1) / 2)))) [16;17]);
  t "repeated named inputs preserve separate nested plan identities" (fun () ->
    let source = sop ("(sop/merge " ^ String.concat " " (List.init 17 (fun index ->
      Printf.sprintf "(sop/box :center [%d 0 0])" index)) ^ ")") in
    let evaluation = static (check source) in
    let nodes = nodes_of evaluation "sop/box" in
    assert (List.length nodes = 17);
    let keys = List.map (fun (node : Eval.node) -> node.inst, node.site, node.iter) nodes in
    assert (List.length (List.sort_uniq compare keys) = 17));
  t "residual capture retains two used bindings out of twenty" (fun () ->
    let bindings = String.concat " " (List.init 20 (fun i -> Printf.sprintf "b%d %d.0" i i)) in
    let ws = check (value ("(let* [" ^ bindings ^ "] (+ (+ b3 b17) t))")) in
    let residual = match result (static ws) "g" with Eval.Residual r -> r | _ -> assert false in
    assert (List.map fst (Eval.Residual.residual_view residual).bindings = ["b17"; "b3"]);
    assert (Eval.residual_eval residual ~live:(Input.at_time 2.) = Ok (Eval.Float 22.)));
  t "residual capture retains a nested function's outer binding" (fun () ->
    let ws = check (value "(let* [outer 7.0 unused 99.0 mapped (map (fn [x] (+ x (+ outer t))) (array/range 2))] 0.0)") in
    let residual = match List.hd (List.assoc ["g"; "mapped"] (static ~record:true ws).records) |> snd with
      | Eval.Residual r -> r | _ -> assert false in
    assert (List.map fst (Eval.Residual.residual_view residual).bindings = ["outer"]);
    assert (Eval.residual_eval residual ~live:(Input.at_time 2.) = Ok (Eval.Float_array [|9.; 10.|])));
  t "free-name walk respects sequential bindings and nested captures" (fun () ->
    let ws = check (value "(let* [outside 7.0 unused 8.0] (let* [local (+ outside t) f (fn [x] (+ x (+ local outside))) mapped (map f (list 1.0 2.0))] 0.0))") in
    let body = (List.hd ws.graphs).body in
    let term = match body.node with Workspace.Let (_, term) -> term | _ -> assert false in
    assert (Eval.Residual.free_names term = ["outside"]);
    let walks = Eval.Private.free_name_walks () in
    ignore (Eval.Residual.free_names term);
    assert (Eval.Private.free_name_walks () = walks));
  t "free-name walk includes destructured sources and state initial values" (fun () ->
    let ws = check (value "(let* [source (list 3.0 4.0) initial 2.0] (let* [[x y] source total (state [s initial] (+ s t))] (+ x (+ y total))))") in
    let term = match (List.hd ws.graphs).body.node with Workspace.Let (_, term) -> term | _ -> assert false in
    assert (Eval.Residual.free_names term = ["initial"; "source"]))

(* the core executor against the walker, bit for bit at every time: lib/flow_ir/test_core_parity.ml *)

let () = (* value/rand: the bit-exact hash (iteration.md 2.2) *)
  t "hash: vectors ported from the study's hash" (fun () ->
    let h = Eval.hash in
    assert (h [] = 0.435769 && h [ 0. ] = 0.009611 && h [ 3.; 4.; 5. ] = 0.706664);
    assert (h [ 1.5; -2. ] = 0.622571 && h [ 7.; 0.; 1. ] = 0.089347 && h [ 1e10 ] = 0.736254);
    assert (h [ -0.25; 12345.678 ] = 0.068138 && h [ 1.; 2.; 3.; 4.; 5.; 6. ] = 0.659002);
    assert (h [ 0.000001 ] = 0.954845 && h [ 1e300 ] = h [ 0. ]);
    assert (h [ 3.; 4.; 5. ] = h [ 3.; 4.; 5. ]));
  t "hash: value/rand reads Int, Float and Bool keys" (fun () ->
    assert (g_value (value "(value/rand 3 4 5)") = Eval.Float 0.706664);
    assert (g_value (value "(value/rand 1.5 -2)") = Eval.Float 0.622571);
    assert (g_value (value "(value/rand true)") = Eval.Float (Eval.hash [ 1. ])))

(* ---- register rules that run: positive and negative ---- *)

let () = (* L3: driven bounds *)
  let src b = "(workspace w (graph g :context value [(n : int 100)] " ^ b ^ "))" in
  t "L3: a driven range over 4,096 is an error naming the count" (fun () ->
    err ~inputs:[ ("g", [ ("n", Eval.Int 5000) ]) ] (src "(count (range n))") "E_ITER_BOUND" ~text:"range 0‥5000 exceeds 4,096";
    err (src "(count (linspace 0 1 (* n 100)))") "E_ITER_BOUND" ~text:"linspace exceeds 4,096";
    is (src "(count (range 4096))") (Eval.Int 4096));
  t "L3: a driven zone over 4,096 iterations is an error naming the zone" (fun () ->
    err (src "(sum [x (range n) y (range n)] 1)") "E_ITER_BOUND" ~text:"g/@result runs more than 4,096 iterations";
    ());
  t "L3: exactly 4,096 iterations run" (fun () ->
    is "(workspace w (graph g :context value [(n : int 64)] (sum [x (range n) y (range n)] 1)))" (Eval.Int 4096));
  t "L3: concat over 4,096 elements is an error" (fun () ->
    err "(workspace w (graph g :context value [(n : int 3000)] (count (concat (range n) (range n)))))" "E_ITER_BOUND" ~text:"concat exceeds 4,096";
    is "(workspace w (graph g :context value [(n : int 2048)] (count (concat (range n) (range n)))))" (Eval.Int 4096));
  t "L3: the step budget stops a runaway" (fun () ->
    err "(workspace w (graph g :context value [(n : int 4000)] (sum [i (range n)] (sum [j (range n)] 1))))" "E_EVAL_BUDGET" ~text:"600000";
    is "(workspace w (graph g :context value [(n : int 40)] (sum [i (range n)] (sum [j (range n)] 1))))" (Eval.Int 1600))

let () = (* D3 at run time, nonfinite math, and the panel checks *)
  t "D3: a pattern longer than a driven list is an error" (fun () ->
    err "(workspace w (graph g :context value [(n : int 2)] (let* [[a b c] (take n (list 1 2 3))] a)))" "E_PATTERN" ~text:"needs 3 elements; the list has 2";
    is "(workspace w (graph g :context value [(n : int 3)] (let* [[a b c] (take n (list 1 2 3))] c)))" (Eval.Int 3));
  t "nonfinite math is an error naming the operator" (fun () ->
    err (value "(pow 10 999)") "E_NONFINITE" ~text:"pow produced a nonfinite value";
    err (value "(* (pow 10 300) (pow 10 300))") "E_NONFINITE" ~text:"*";
    is (value "(pow 10 3)") (Eval.Int 1000);
    (* the study's rules: divide and mod by zero give 0, sqrt takes the absolute value *)
    is (value "(/ 1 0)") (Eval.Int 0);
    is (value "(mod 5 0)") (Eval.Int 0);
    is (value "(sqrt -4)") (Eval.Int 2));
  t "ui/tile holds 1 to 16 panels, split takes horizontal or vertical" (fun () ->
    let ed b = "(workspace w (graph g :context editor [(n : int 3)] (ui/workspace " ^ b ^ ")))" in
    let tile = ed "(ui/tile (for [i (range n)] (ui/graph)))" in
    ignore (static (check tile));
    err ~inputs:[ ("g", [ ("n", Eval.Int 20) ]) ] tile "E_RANGE" ~text:"1–16";
    err ~inputs:[ ("g", [ ("n", Eval.Int 0) ]) ] tile "E_RANGE" ~text:"1–16";
    ignore (static (check (ed "(ui/split (str \"hori\" \"zontal\") (ui/graph) (ui/lisp))")));
    err (ed "(ui/split (str \"dia\" \"gonal\") (ui/graph) (ui/lisp))") "E_RANGE" ~text:"axis";
    err (ed "(ui/split-at \"vertical\" (* n 0.5) (ui/graph) (ui/lisp))") "E_RANGE" ~text:"ratio";
    ignore (static (check (ed "(ui/split-at \"vertical\" (* n 0.1) (ui/graph) (ui/lisp))")));
    (* settings ranges are the schema's: Editor_document.Contexts checks them when it lowers *)
    ignore (static (check "(workspace w (graph g :context settings [(f : int 60)] (settings/config :fps f)))")))

let () = (* C2: str formatting *)
  t "C2: ints plain, floats up to 4 decimals, no trailing zeros, ties away from zero" (fun () ->
    let s src = match bound (value ("(let* [z (str " ^ src ^ ")] 1)")) "z" with Eval.Text s -> s | _ -> failwith "not text" in
    assert (s "3" = "3" && s "-3" = "-3" && s "2.0" = "2" && s "100.0" = "100");
    assert (s "0.1" = "0.1" && s "0.30000000000000004" = "0.3" && s "1.23456" = "1.2346");
    List.iter (fun (x, want) -> if s x <> want then failwith (x ^ " prints " ^ s x ^ ", want " ^ want))
      [ ("0.03125", "0.0313"); ("-0.03125", "-0.0313"); ("0.00005", "0.0001"); ("0.00015", "0.0001");
        ("1.00005", "1.0001"); ("0.09375", "0.0938"); ("0.15625", "0.1563"); ("2.00005", "2");
        ("0.00035", "0.0003"); ("0.000125", "0.0001"); ("123456789.123456", "123456789.1235") ];
    assert (s "-0.00001" = "0" && s "0.99999" = "1" && s "12345.6789" = "12345.6789");
    assert (s "true false" = "truefalse" && s "[1 2.5 3]" = "[1 2.5 3]" && s "(list)" = "[]");
    assert (s "{:a 1.5 :b [1 2 3]}" = "{:a 1.5 :b [1 2 3]}" && s "\"x\" 1" = "x1"));
  t "C2: what str prints for a vec3, a list of vec3 and a record of lists" (fun () ->
    let s src = match bound (value ("(let* [z (str " ^ src ^ ")] 1)")) "z" with Eval.Text s -> s | _ -> failwith "not text" in
    assert (s "[1 0.5 0.33333]" = "[1 0.5 0.3333]");
    assert (s "(list [1 2 3] [4 5 6])" = "[[1 2 3] [4 5 6]]");
    assert (s "{:p (list 1 2) :q {:r true}}" = "{:p [1 2] :q {:r true}}"))

let () = (* L2, L4, L5: product order, empty results, ints *)
  t "L2: three clauses, nested products and dependent ranges" (fun () ->
    assert (same (bound (value "(let* [z (for [a (range 2) b (range 2) c (range 2)] (+ (* a 4) (+ (* b 2) c)))] 1)") "z") (ints [ 0; 1; 2; 3; 4; 5; 6; 7 ]));
    assert (same (bound (value "(let* [z (for [a (range 3) b (range a 3)] (+ (* a 10) b))] 1)") "z") (ints [ 0; 1; 2; 11; 12; 22 ])));
  t "L4: empty loops" (fun () ->
    is (value "(count (for [i (range 0)] i))") (Eval.Int 0);
    let r = run (check (sop "(sop/merge (for [i (range 0)] (sop/box)))")) in
    (* merge of nothing: one merge node with no inputs *)
    assert (count_of r "sop/merge" = 1 && (List.hd (nodes_of r "sop/merge")).args = []));
  t "L5: range yields ints, linspace floats including both ends" (fun () ->
    assert (bound (value "(let* [z (range 3)] 1)") "z" = ints [ 0; 1; 2 ]);
    assert (bound (value "(let* [z (linspace 0 1 3)] 1)") "z" = Eval.List [| Float 0.; Float 0.5; Float 1. |]);
    assert (bound (value "(let* [z (linspace 2 5 1)] 1)") "z" = Eval.List [| Float 2. |]);
    is (value "(count (range 2 6))") (Eval.Int 4))

let () = (* L6, L14, F3: accumulators, ref overrides, sum types *)
  t "L6: an int seed does not round a float body; sum keeps int, float, vec3" (fun () ->
    assert (g_value (value "(fold [a 1] [i (range 3)] (* a 2.5))") = Eval.Float 15.625);
    assert (g_value (value "(fold [a 0] [i (range 3)] (+ a 0.5))") = Eval.Float 1.5);
    assert (g_value (value "(fold [a 0] [i (range 3)] (+ a i))") = Eval.Int 3);
    assert (bound (value "(let* [z (scan [a 0] [i (range 2)] (+ a 0.5))] 1)") "z" = Eval.List [| Float 0.5; Float 1. |]);
    assert (bound (value "(let* [z (sum [i (range 3)] [i 0 1])] 1)") "z" = Eval.Vec3 (3., 0., 3.));
    assert (g_value (value "(sum [i (range 3)] 0.5)") = Eval.Float 1.5);
    assert (g_value (value "(sum [i (range 3)] i)") = Eval.Int 3));
  t "L14: ref overrides are cached by value and coerced to the input type" (fun () ->
    let ws = check "(workspace w (graph a :context value [(n : float 2)] (* n 3)) (graph g :context value (+ (ref a :n 5) (+ (ref a :n 5) (ref a :n 5.0)))))" in
    let r = static ws in
    assert (same (result r "g") (Eval.Int 45));
    (* a, then a with n=5 once (5 and 5.0 are the same float) *)
    assert (Array.length r.plan.instances = 3));
  t "L14: a geometry ref makes one shared instance per override tuple" (fun () ->
    let ws = check "(workspace w (graph a :context sop [(n : int 2)] (sop/box :size n)) (graph g :context sop (sop/merge (ref a) (ref a :n 3) (ref a :n 3) (ref a))))" in
    let r = static ws in
    (* a with its default, a with n=3 *)
    assert (Array.length r.plan.instances = 3);
    let boxes = nodes_of r "sop/box" in
    assert (List.map (fun (n : Eval.node) -> n.inst) boxes = [ 0; 2 ]);
    let merge = List.hd (nodes_of r "sop/merge") in
    (* the two refs with the same tuple wire the same node *)
    match merge.args with
    | [ (_, Eval.Deferred (Ty.Named "geometry", a)); (_, Deferred (Ty.Named "geometry", b)); (_, Deferred (Ty.Named "geometry", c)); (_, Deferred (Ty.Named "geometry", d)) ] -> assert (a = d && b = c && a <> b)
    | _ -> failwith "merge inputs");
  t "L14: inputs override from OCaml and are coerced" (fun () ->
    let ws = check "(workspace w (graph g :context value [(n : float 2)] (* n 3)))" in
    let r = ok (Eval.static ~inputs:[ ("g", [ ("n", Eval.Int 4) ]) ] ws) in
    assert (result r "g" = Eval.Float 12.);
    assert ((r.plan.instances.(0)).inputs = [ ("n", Eval.Float 4.) ]))

let () = (* W5: records are bounded, per path and iteration tuple *)
  t "W5: at most 4,096 records per path; geometry as references" (fun () ->
    let rs = records (value "(sum [i (range 100)] (sum [j (range 100)] (let* [x (* i j)] x)))") in
    let longest = List.fold_left (fun m (_, l) -> max m (List.length l)) 0 rs in
    assert (longest = 4096);
    let inner = List.find (fun (p, l) -> List.length l = 4096 && List.mem "x" p) rs in
    assert (List.length (snd inner) = 4096);
    let rs = records (sop "(let* [a (sop/box)] a)") in
    assert (first_at rs [ "g"; "a" ] = Eval.Deferred (Ty.geometry, 0));
    (* nothing is recorded unless asked *)
    assert ((run (check (value "(sum [i (range 3)] i)"))).records = []))

let () = (* the Wave fixture's sop/curve: a plan node whose points are live *)
  t "Wave: sop/curve nodes carry their points, live through t" (fun () ->
    let ws = check (read (Filename.concat cases "wave.lisp")) in
    let s = static ws in
    let curves = nodes_of s "sop/curve" in
    assert (List.length curves = 6);
    (match arg (List.hd curves) "points" with
     | Eval.List pts -> assert (Array.length pts = 90 && Eval.is_live (Eval.List pts))
     | _ -> failwith "points");
    let r = run ~time:0.5 ws in
    (match arg (List.hd (nodes_of r "sop/curve")) "points" with
     | Eval.List pts -> assert (Array.for_all (function Eval.Vec3 _ -> true | _ -> false) pts)
     | _ -> failwith "points"))
let () = (* W8: a loop over geometry is one zone node with a template body *)
  t "W8: for over point_list is a zone node; the body is a template" (fun () ->
    let ws = check (sop "(let* [g (sop/grid) spots (sop/scatter g :count 5 :seed 3) dot (sop/box)
      dots (for [p (sop/point_list spots :key \"id\")] (sop/transform dot :translate p))
      result (sop/merge g dots)] result)") in
    let s = static ws in
    let zone = List.hd (nodes_of s "zone/points") in
    let lo, hi = match arg zone "lo", arg zone "hi" with Eval.Int a, Eval.Int b -> a, b | _ -> failwith "range" in
    assert (hi - lo = 1);
    assert (arg zone "key" = Eval.Text "id");
    (* the element is a residual, forced with the element bound *)
    let tr = List.hd (nodes_of s "sop/transform") in
    let p = arg tr "translate" in
    assert (Eval.is_live p);
    let key = Eval.element_key [ "g"; "dots" ] in
    (match Eval.force ~elems:[ (key, Eval.Vec3 (1., 2., 3.)) ] p ~live:(Input.at_time (0. )) with
     | Ok (Eval.Vec3 (1., 2., 3.)) -> ()
     | _ -> failwith "element not bound");
    assert (Result.is_error (Eval.force p ~live:(Input.at_time (0. ))));
    (* not a list: only sop/merge takes it, and the structure cannot read the element *)
    err (sop "(let* [g (sop/grid) n (count (for [p (sop/point_list g)] (sop/box)))] g)") "E_TYPE";
    err (sop "(let* [g (sop/grid)] (sop/merge (for [p (sop/point_list g)] (if (> p.x 0) (sop/box) (sop/grid)))))") "E_ZONE";
    err (sop "(let* [g (sop/grid)] (sop/merge (for [p (sop/point_list g) i (range 2)] (sop/box))))") "E_ZONE")

let () = (* register L16: :skip leaves iterations out; the others keep their tuples *)
  let nested l = Eval.List (Array.of_list (List.map ints l)) in
  let zs src want = if not (same (bound (value ("(let* [z " ^ src ^ "] 1)")) "z") want) then failwith src in
  t "skip: a for leaves out the listed iterations" (fun () ->
    zs "(for [i (range 5)] :skip [1 3] i)" (ints [ 0; 2; 4 ]);
    zs "(for [i (range 5)] :skip [[1] [3]] i)" (ints [ 0; 2; 4 ]);
    is (value "(count (for [i (range 5)] :skip [0 1 2 3 4] i))") (Eval.Int 0));
  t "skip: a product skips by its flat index, last clause fastest" (fun () ->
    zs "(for [i (range 2) j (range 3)] :skip [4] (+ (* 10 i) j))" (ints [ 0; 1; 2; 10; 12 ]));
  t "skip: a tuple is the enclosing iterations, then this loop's" (fun () ->
    zs "(for [i (range 3)] (for [j (range 2)] :skip [[1 0] [2 1]] (+ (* 10 i) j)))"
      (nested [ [ 0; 1 ]; [ 11 ]; [ 20 ] ]);
    zs "(for [i (range 2)] (for [j (range 2)] (for [k (range 2)] :skip [[1 0 1] [0 1 0]] (+ (* 100 i) (+ (* 10 j) k)))))"
      (Eval.List [| Eval.List [| ints [ 0; 1 ]; ints [ 11 ] |]; Eval.List [| ints [ 100 ]; ints [ 110; 111 ] |] |]));
  t "skip: a skipped iteration makes no plan node, the others keep site and tuple" (fun () ->
    let all = run (check (sop "(sop/merge (for [i (range 4)] (sop/box :size (+ 1 i))))")) in
    let some = run (check (sop "(sop/merge (for [i (range 4)] :skip [2] (sop/box :size (+ 1 i))))")) in
    let keys r = List.map (fun (n : Eval.node) -> (n.site, n.iter)) (nodes_of r "sop/box") in
    assert (count_of all "sop/box" = 4 && count_of some "sop/box" = 3);
    assert (keys some = List.filter (fun (_, it) -> it <> [ 2 ]) (keys all));
    (* the boxes that stay are the same plan nodes by key, with the same arguments *)
    List.iter (fun (n : Eval.node) ->
      let twin = List.find (fun (m : Eval.node) -> m.site = n.site && m.iter = n.iter) (nodes_of all "sop/box") in
      assert (n.args = twin.args)) (nodes_of some "sop/box"));
  t "skip: the loop variable is still recorded at a skipped iteration; the body is not" (fun () ->
    let rs = records (value "(let* [xs (for [i (range 4)] :skip [2] (* i 2))] (count xs))") in
    assert (List.length (recs rs [ "g"; "xs"; ":i" ]) = 4);
    assert (List.map fst (recs rs [ "g"; "xs"; "@result" ]) = [ [ 0 ]; [ 1 ]; [ 3 ] ]));
  t "skip: a scene/merge leaves out the arguments at the listed tuples" (fun () ->
    let scene body = "(workspace w (graph s :context scene " ^ body ^ "))" in
    let r body = List.assoc "s" (run (check (scene body))).results in
    let args = function Eval.Struct (_, _, a) -> List.length a | _ -> -1 in
    assert (args (r "(scene/merge (scene/light) (scene/light) (scene/light))") = 3);
    assert (args (r "(scene/merge (scene/light) (scene/light) (scene/light) :skip [1])") = 2);
    assert (args (r "(scene/merge (scene/light) (scene/light) :skip [[0] [1]])") = 0);
    (* inside a loop the tuple starts with the loop's iteration *)
    (match r "(scene/merge (for [i (range 2)] (scene/merge (scene/light) (scene/light) :skip [[1 0]])))" with
     | Eval.Struct (_, _, [ (_, a); (_, b) ]) -> assert (args a = 2 && args b = 1)
     | _ -> failwith "the merge of merges"))

let () =
  t "kernel observation preserves evaluation and sees actual static captures" (fun () ->
    let ws = check "(workspace observation
      (defn doubled :context value [(offset : float)]
        (let* [items (map (fn [x] (+ x offset)) (array/range 2))] (array/sum items)))
      (graph g :context host [(offset : float 2.0)]
        (let* [static_map (map (fn [x] (+ x offset)) (array/range 2))
               empty_map (map (fn [x] (+ x offset)) (array/range 0))
               live_map (map (fn [x] (+ (+ x offset) t)) (array/range 3))
               from_def (doubled offset)
               nested (for [i (range 2)]
                 (let* [mapped (map (fn [x] (+ (+ x i) offset)) (array/range 2))]
                   (array/sum mapped)))
               list_map (map (fn [x] (+ x offset)) (list 1 2))
               reduced (reduce (fn [a x] (+ a x)) 0.0 (array/range 2))
               unvisited (if true 0.0 (array/sum (map (fn [x] (+ x offset)) (array/range 3))))]
          {:static static_map :empty empty_map :live live_map :def from_def
           :nested nested :list list_map :reduced reduced :unvisited unvisited}))
      (graph other :context host (ref g :offset 7.0)))" in
    let ordinary = static ~record:true ws in
    let seen = ref [] and adapter_checked = ref false in
    let observe residual =
      seen := residual :: !seen;
      let view = Eval.Residual.residual_view residual in
      if not !adapter_checked && view.site = ["g";"static_map"] then begin
        adapter_checked := true;
        let count = List.length !seen in
        assert (Eval.Residual.eval_term residual view.term ~live:(Input.at_time 0.)
          = Ok (Eval.Float_array [|2.;3.|]));
        assert (List.length !seen = count)
      end in
    let observed = ok (Eval.Residual.handle ~observe (fun () ->
      Eval.Private.static_with_requests ~record:true ws)) in
    assert (!adapter_checked);
    (* Compare ordinary identities as well as immutable data, without comparing
       the closures embedded in function/residual contexts. *)
    let rec identity = function
      | Eval.Fn fn -> Eval.Text ("fn:" ^ string_of_int (Eval.Residual.function_id fn))
      | Residual r -> Eval.Text ("residual:" ^ string_of_int (Eval.Residual.residual_id r))
      | List xs -> Eval.List (Array.map identity xs)
      | Record fs -> Eval.Record (List.map (fun (name, v) -> name, identity v) fs)
      | Struct (name, ty, fs) -> Eval.Struct (name, ty, List.map (fun (name, v) -> name, identity v) fs)
      | v -> v in
    let results r = List.map (fun (name, v) -> name, identity v) r.Eval.results in
    let records r = List.map (fun (path, values) ->
      path, List.map (fun (tuple, v) -> tuple, identity v) values) r.Eval.records in
    let instances r = Array.map (fun (i : Eval.instance) ->
      i.graph, i.default, List.map (fun (name, v) -> name, identity v) i.inputs, identity i.result) r.Eval.plan.instances in
    assert (results ordinary = results observed && records ordinary = records observed);
    assert (instances ordinary = instances observed && ordinary.authored = observed.authored);
    assert (ordinary.plan.nodes = observed.plan.nodes && ordinary.states = observed.states);
    let ids = List.map Eval.Residual.residual_id !seen in
    assert (List.for_all (fun id -> id < 0) ids);
    assert (List.length (List.sort_uniq compare ids) = List.length ids);
    let views = List.map Eval.Residual.residual_view (List.rev !seen) in
    let at path = List.filter (fun (v : Eval.Residual.residual_view) -> v.site = path) views in
    let offsets views = List.map (fun (v : Eval.Residual.residual_view) -> List.assoc "offset" v.bindings) views in
    let maps = at ["g";"static_map"] in
    assert (offsets maps = [Eval.Float 2.; Float 7.]);
    let authored = match (List.find (fun (g : Workspace.graph) -> g.name = "g") ws.graphs).body.node with
      | Workspace.Let (bindings, _) -> List.assoc (Workspace.Name "static_map") bindings
      | _ -> assert false in
    List.iter (fun (v : Eval.Residual.residual_view) ->
      assert (v.term == authored && v.iter = []);
      assert (List.map fst v.bindings = ["offset"]);
      let instance = observed.plan.instances.(v.instance) in
      assert (instance.graph = "g" && List.assoc "offset" instance.inputs = List.assoc "offset" v.bindings)) maps;
    List.iter (fun path -> assert (offsets (at ["g";path]) = [Eval.Float 2.; Float 7.]))
      ["empty_map";"live_map";"list_map"];
    assert (List.for_all (fun (v : Eval.Residual.residual_view) -> match v.term.ty with
      | Ty.List _ -> true | _ -> false) (at ["g";"list_map"]));
    assert (List.length (at ["g";"reduced"]) = 2);
    let nested = List.filter (fun (v : Eval.Residual.residual_view) -> List.mem_assoc "i" v.bindings) views in
    assert (List.map (fun (v : Eval.Residual.residual_view) -> v.iter, List.assoc "i" v.bindings,
      List.assoc "offset" v.bindings) nested =
      [[0], Eval.Int 0, Float 2.; [1], Int 1, Float 2.; [0], Int 0, Float 7.; [1], Int 1, Float 7.]);
    let definitions = List.filter (fun (v : Eval.Residual.residual_view) -> match v.term.node with
      | Workspace.Hof (`Map, _) -> List.mem "def:doubled" v.site | _ -> false) views in
    assert (offsets definitions = [Eval.Float 2.; Float 7.]);
    assert (List.exists (fun (v : Eval.Residual.residual_view) -> match v.term.node with
      | Workspace.Op {op = "array/sum"; _} -> true | _ -> false) views);
    assert (not (List.exists (fun (v : Eval.Residual.residual_view) -> List.mem "unvisited" v.site) views));
    let count = List.length !seen in
    List.iter (fun time ->
      let live = Input.at_time time in
      List.iter2 (fun (name, a) (other, b) ->
        assert (name = other && Eval.force a ~live = Eval.force b ~live)) ordinary.results observed.results;
      assert (List.length !seen = count)) [0.;0.125;1.25;7.]);
  t "live packed accumulators defer the complete iteration" (fun () ->
    List.iter (fun body ->
      let workspace = check (value body) in
      let v = result (static workspace) "g" in
      (match v with
       | Eval.Residual r -> (match (Eval.Residual.residual_view r).term.node with
           | Workspace.Loop {kind = `Fold; _} | Workspace.Hof (`Reduce, _) -> ()
           | _ -> failwith "accumulator was unrolled into per-element residuals")
       | _ -> failwith "live accumulator did not defer");
      assert (Eval.force v ~live:(Input.at_time 0.5) = Ok (Eval.Float 1025.5)))
      ["(fold [a 0] [x (array/range 2051)] (+ a t))";
       "(reduce (fn [a x] (+ a t)) 0.0 (array/range 2051))"]);
  t "image constructors preserve live arguments in deferred image nodes" (fun () ->
    List.iter (fun (body, field, expected) ->
      let evaluated = static (check ("(workspace w (graph drawing :context draw (draw/background \"#000000\"))" ^
        " (graph g :context image " ^ body ^ "))")) in
      let id = match result evaluated "g" with
        | Eval.Deferred (ty, id) when ty = Ty.image -> id
        | _ -> failwith "live image constructor did not create a deferred image node" in
      let argument = List.assoc field evaluated.plan.nodes.(id).args in
      assert (Eval.is_live argument);
      assert (Eval.force argument ~live:(Input.at_time 2.) = Ok expected))
      ["(image/noise :width 2 :height 2 :frequency (+ 0.3 t))", "frequency", Eval.Float 2.3;
       "(image/noise :width 2 :height 2 :freq (+ 0.3 t))", "frequency", Eval.Float 2.3;
       "(image/noise :width 2 :height 2 :freq 0.7 :frequency (+ 0.3 t))", "frequency", Eval.Float 2.3;
       "(image/load (if (< t 1.0) \"before.png\" \"after.png\"))", "path", Eval.Text "after.png";
       "(image/render (ref drawing) :width (if (< t 1.0) 2 3) :height 2)", "width", Eval.Int 3]);
  t "equality compares current captured record/list fields" (fun () ->
    List.iter (fun (body,want) ->
      let v=result (static (check (value body))) "g" in
      List.iter (fun time -> assert (Eval.force v ~live:(Input.at_time time) = Ok (Eval.Bool want)))
        [0.;0.37;2.])
      ["(let* [a {:v t} b {:v t}] (equal? a b))",true;
       "(let* [a (list {:v t}) b (list {:v t})] (equal? a b))",true;
       "(let* [a {:v {:x t}}] (equal? a {:v {:x t}}))",true;
       "(let* [a {:x t :y 2.} b {:y 2. :x t}] (equal? a b))",false;
       "(let* [a {:x t} b {:x (+ t 1.)}] (equal? a b))",false]);
  t "updates and accumulators use the current captured field tag" (fun () ->
    List.iter (fun body ->
      let v=result (static (check (value body))) "g" in
      List.iter (fun time -> match Eval.force v ~live:(Input.at_time time) with
        | Ok (Eval.Int 2) -> ()
        | Ok actual -> failwith (Printf.sprintf "%s at %g: got %s" body time (Eval.show actual))
        | Error d -> failwith (Diagnostic.to_string d))
        [-2.5;0.;0.37;2.])
      ["(let* [r {:n (int t)} updated (assoc r :n 1.7)] updated.n)";
       "(let* [r {:v {:n (int t)} :unused (exp (* t 100000.))} updated (assoc r :v {:n 1.7})] updated.v.n)";
       "(let* [r {:n (int t)} updated (fold [a r] [x (array/range 2)] (assoc a :n 1.7))] updated.n)";
       "(let* [r {:n (int t)} xs (scan [a r] [x (range 2)] (assoc a :n 1.7))] (get (nth xs 0) :n))";
       "(let* [r {:n (int t)} updated (reduce (fn [(a : {:n int}) x] (assoc a :n 1.7)) r (list 0))] updated.n)"]);
  t "higher-order calls retain the checked specialization and original annotations" (fun () ->
    List.iter (fun body ->
      let v=result (static (check (value body))) "g" in
      List.iter (fun time ->
        let live=Input.at_time time in
        assert (Eval.force v ~live = Ok (Eval.Int 2));
        assert (Eval.Private.force_reference v ~live = Ok (Eval.Int 2))) [-2.5;0.;0.37;2.])
      ["(let* [r {:n (int t)} updated (reduce (fn [a x] (assoc a :n 1.7)) r (list 0))] updated.n)";
       "(let* [r {:n (int t)} f (fn [a x] (assoc a :n 1.7)) updated (reduce f r (list 0))] updated.n)";
       "(get (first (map (fn [a] (assoc a :n 1.7)) (list {:n (int t)}))) :n)";
       "(get (first (filter (fn [a] (= (get (assoc a :n 1.7) :n) 2)) (list {:n 2}))) :n)";
       "(get (first (sort-by (fn [a] (get (assoc a :n 1.7) :n)) (list {:n 2} {:n (int t)}))) :n)"];
    is (value "(reduce (fn [a x] (+ a x)) 0 (list 0.5 0.25))") (Eval.Float 0.75));
  t "operator function values use declaration argument names" (fun () ->
    is (value "(let* [lengths (map array/count (list (array/float 2) (array/float 4)))] (+ (first lengths) (last lengths)))")
      (Eval.Int 6));
  if !failed <> [] then begin
    List.iter (fun (n, e) -> prerr_endline ("FAIL " ^ n ^ ": " ^ e)) (List.rev !failed);
    exit 1
  end;
  Printf.printf "Flow workspace eval: %d value cases pass\n" !passed

let () = (* report.md L10, L18, L20: one bypass index, nonfinite guards, whole indices *)
  t "bypass passes the first positional input, after keyword pairs, for every call" (fun () ->
    is (value "(let* [f (fn [a b] (+ a b))] ^:bypass (f 2 3))") (Eval.Int 2));
  t "nonfinite values are refused where they are made" (fun () ->
    List.iter (fun source ->
      match Eval.static (check ("(workspace w (graph g :context value " ^ source ^ "))")) with
      | Error d -> assert (d.code = "E_NONFINITE")
      | Ok _ -> failwith (source ^ " was accepted"))
      [ "(value/lerp 0 (pow 10 308) 100)"; "(value/polar (pow 10 308) 0 0)"; "(* [1 2 3] (* (pow 10 308) 10))" ]);
  t "nth takes a whole index" (fun () ->
    match Eval.static (check "(workspace w (graph g :context value (nth (list 1 2 3 4) 2.5)))") with
    | Error d -> assert (d.code = "E_LIST_RANGE")
    | Ok _ -> failwith "nth 2.5 read an element")

(* A static packed map of more than 4,096 elements: the host's whole value is taken only when it
   agrees with the elements the walker made; the evaluation is the ordinary one either way. *)
let () =
  let ws = check "(workspace bulk
      (graph g :context host
        (let* [f (fn [i] (let* [twice (* i 2)] [twice (+ i 0.5) 0]))
               large (map f (array/range 5000))
               again (map f (array/range 4100))
               small (map f (array/range 4096))
               floats (map (fn [i] (* i 0.25)) (array/range 4097))]
          {:large large :again again :small small :floats floats})))" in
  let ordinary = static ~record:true ws in
  let field (r : Eval.t) name = match result r "g" with
    | Eval.Record fields -> List.assoc name fields | _ -> failwith "g is a record" in
  let data (v : Eval.value) = match v with
    | Eval.Float_array d | Vec2_array d | Vec3_array d | Vec4_array d -> d | _ -> failwith "packed" in
  let records (r : Eval.t) = List.map (fun (path, values) -> path, List.length values) r.records in
  let same_as_ordinary (r : Eval.t) =
    List.iter (fun name -> assert (field r name = field ordinary name)) ["large"; "again"; "small"; "floats"];
    assert (records r = records ordinary) in
  (* the true arrays, by element count *)
  let whole n = List.find_map (fun name ->
    let v = field ordinary name in
    if Value.array_length v = n then Some v else None) ["large"; "again"; "floats"] in
  let offered = ref [] in
  let run bulk = offered := [];
    ok (Eval.Residual.handle ~bulk:(fun residual ->
      let view = Eval.Residual.residual_view residual in
      let n = match view.term.node with
        | Workspace.Hof (`Map, [_; source]) ->
            (match Eval.Residual.eval_term residual source ~live:(Input.at_time 0.) with
             | Ok v -> Value.array_length v | Error d -> failwith (Diagnostic.to_string d))
        | _ -> failwith "a map is offered" in
      offered := n :: !offered; bulk n) (fun () ->
        Eval.Private.static_with_requests ~record:true ws)) in
  (* Supplied whole and correct: used. Only the maps over 4,096 elements are offered, once each. *)
  let supplied = run whole in
  assert (List.sort compare !offered = [4097; 4100; 5000]);
  same_as_ordinary supplied;
  (* The arrays are the host's own, not copies: it was not walked past the first 4,096. *)
  assert (data (field supplied "large") == data (Option.get (whole 5000)));
  (* Refused by the host: the walker finishes. *)
  let walked = run (fun _ -> None) in
  same_as_ordinary walked;
  assert (data (field walked "large") != data (Option.get (whole 5000)));
  (* A value that differs inside the walked elements, has another length or another element kind
     is not taken. *)
  let wrong n = Option.map (fun v ->
    let d = Array.copy (data v) in
    d.(7) <- d.(7) +. 1.;
    match v with Eval.Float_array _ -> Eval.Float_array d | _ -> Eval.Vec3_array d) (whole n) in
  same_as_ordinary (run wrong);
  same_as_ordinary (run (fun _ -> whole 4100));
  same_as_ordinary (run (fun n -> Option.map (fun v -> Eval.Vec2_array (data v)) (whole n)));
  same_as_ordinary (run (fun _ -> Some (Eval.Int 1)));
  (* A difference past the walked elements cannot be seen: the host's value stands. This is the
     contract the host's kernel has with the walker, not something the evaluator can check. *)
  let late n = Option.map (fun v ->
    let d = Array.copy (data v) in
    d.(Array.length d - 1) <- 99.;
    match v with Eval.Float_array _ -> Eval.Float_array d | _ -> Eval.Vec3_array d) (whole n) in
  let trusted = run late in
  assert ((data (field trusted "large")).(14999) = 99.);
  (* A failure past the walked elements is the walker's own when the host has no value. *)
  let failing = check "(workspace bulk (graph g :context host (map (fn [i] (exp (- i 4300))) (array/range 5200))))" in
  (match Eval.static failing, Eval.Residual.handle ~bulk:(fun _ -> None) (fun () ->
      Eval.Private.static_with_requests failing) with
   | Error a, Error b -> assert (a.code = "E_NONFINITE" && a = b)
   | _ -> failwith "a nonfinite element past the walked ones fails either way");
  (* A map that reads the frame is never offered: it becomes a residual before any host call. *)
  let live = check "(workspace bulk (graph g :context host (map (fn [i] (+ i t)) (array/range 5000))))" in
  let asked = ref 0 in
  (match ok (Eval.Residual.handle ~bulk:(fun _ -> incr asked; None) (fun () ->
      Eval.Private.static_with_requests live)) with
   | r -> (match result r "g" with Eval.Residual _ -> () | _ -> failwith "a live map stays a residual"));
  assert (!asked = 0);
  print_endline "Static bulk maps: offered past 4,096 elements, checked against the walked ones, refused or wrong values walked"
