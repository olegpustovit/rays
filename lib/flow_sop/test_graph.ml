(* E.1-E.3: graph/shortest_paths, graph/color and graph/chains against
   independent oracles and the RDK callers they wrap. *)
module E = Flow.Eval
module G = Rdk.Geometry
module T = Rdk.Topology

let ok = function Ok value -> value | Error message -> failwith message
let flow_ok = function Ok value -> value | Error d -> failwith (Diagnostic.to_string d)
let ints a = E.Int_array (Array.copy a)

let fixtures () =
  let topology points vertex_points primitive_offsets primitive_kinds =
    T.create_owned ~point_count:points ~vertex_points ~primitive_offsets ~primitive_kinds |> ok
  in
  [
    ("empty", T.empty ~point_count:0);
    ("shared-polygons", topology 5 [| 0; 1; 2; 2; 1; 3 |] [| 0; 3; 6 |] [| T.Polygon; Polygon |]);
    ("open-repeated", topology 3 [| 0; 1; 1; 2; 1 |] [| 0; 5 |] [| T.Open_polyline |]);
    ("closed", topology 4 [| 3; 2; 1; 0 |] [| 0; 4 |] [| T.Closed_polyline |]);
    ( "mixed-nonmanifold",
      topology 6
        [| 0; 1; 2; 0; 1; 3; 1; 0; 4; 0; 1; 5 |]
        [| 0; 3; 6; 9; 12 |]
        [| T.Polygon; Open_polyline; Closed_polyline; Polygon |] );
  ]
  |> List.map (fun (name, topology) ->
      let count = T.point_count topology in
      let positions =
        Rdk.Packed.Float3.View.of_owned_exn ~x:(Array.init count float) ~y:(Array.make count 0.)
          ~z:(Array.make count 0.)
      in
      (name, G.create ~positions ~topology () |> ok))

let evaluate geometry decls inputs body =
  let source =
    "(workspace graph (graph g :context sop [(geo : geometry nil) " ^ decls ^ "] (let* [tested "
    ^ body ^ "] geo)))"
  in
  let forms = Flow.Syntax.parse source |> flow_ok in
  let checked =
    match
      Flow.Workspace.check ~ops:Flow_sop.Operators.all
        { Flow.Catalog.version = 1; kinds = [] }
        forms
    with
    | Some checked, [] -> checked
    | _, ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds))
  in
  let value =
    Flow.Eval.static ~record:true
      ~inputs:[ ("g", ("geo", E.Deferred (Flow.Ty.geometry, 7)) :: inputs) ]
      checked
    |> flow_ok
    |> fun evaluation -> List.assoc [ "g"; "tested" ] evaluation.records |> List.hd |> snd
  in
  ( value,
    Flow_sop.Attribute_kernel.resolve ~geometry:(fun id -> if id = 7 then Some geometry else None)
  )

(* Reference interpreter and strict compiled tier agree with [expected] at one
   and eight domains. *)
let agree ?(decls = "") ?(inputs = []) geometry body expected =
  let value, resolve = evaluate geometry decls inputs body in
  let live = Input.at_time 0.25 in
  let core = Flow_ir.Executor.compile ~strict:true value |> flow_ok in
  List.iter
    (fun domains ->
      Parallel.run ~domains (fun () ->
          assert (flow_ok (E.Host.force_reference ~resolve value ~live) = expected);
          assert (flow_ok (Flow_ir.Executor.force ~strict:true ~resolve core ~live) = expected)))
    [ 1; 8 ]

let refused ?(decls = "") ?(inputs = []) geometry body code =
  let value, resolve = evaluate geometry decls inputs body in
  match E.Host.force_reference ~resolve value ~live:(Input.at_time 0.) with
  | Error d -> assert (d.code = code)
  | Ok _ -> failwith ("expected " ^ code)

let view_of geometry =
  Rdk.Topology_index.View.view (Rdk.Topology_index.create (G.topology geometry))

(* Textbook O(n^2) Dijkstra: settle the unsettled point of least (distance, index),
   relax strictly smaller candidates so the first settled neighbour is the parent. *)
let oracle_paths ?(radius = infinity) (view : Rdk.Topology_index.View.view) lengths roots =
  let n = Array.length view.point_edge_offsets - 1 in
  let dist = Array.make n infinity
  and parent = Array.make n (-1)
  and settled = Array.make n false in
  Array.iter (fun r -> dist.(r) <- 0.) roots;
  let continue = ref true in
  while !continue do
    let best = ref (-1) in
    for p = 0 to n - 1 do
      if (not settled.(p)) && dist.(p) < infinity && (!best < 0 || dist.(p) < dist.(!best)) then
        best := p
    done;
    if !best < 0 then continue := false
    else begin
      let p = !best in
      settled.(p) <- true;
      for slot = view.point_edge_offsets.(p) to view.point_edge_offsets.(p + 1) - 1 do
        let e = view.point_edges.(slot) in
        let q = if view.edge_a.(e) = p then view.edge_b.(e) else view.edge_a.(e) in
        let candidate = dist.(p) +. lengths.(e) in
        if q <> p && candidate <= radius && candidate < dist.(q) then begin
          dist.(q) <- candidate;
          parent.(q) <- p
        end
      done
    end
  done;
  E.Record
    [
      ("distance", E.Float_array (Array.map (fun d -> if d = infinity then -1. else d) dist));
      ("parent", E.Int_array parent);
      ("path", E.Int_array [||]);
      ("order", E.Int_array [||]);
    ]

let shortest_paths () =
  List.iter
    (fun (_, geometry) ->
      let view = view_of geometry in
      let points = G.point_count geometry and edges = Array.length view.edge_a in
      List.iter
        (fun roots ->
          if roots <= points then
            List.iter
              (fun (lengths_lisp, lengths) ->
                agree geometry
                  (Printf.sprintf
                     "(graph/shortest_paths (sop/adjacency geo) %s (array/range %d :type int))"
                     lengths_lisp roots)
                  (oracle_paths view lengths (Array.init roots Fun.id)))
              [
                (Printf.sprintf "(array/float %d 1.0)" edges, Array.make edges 1.);
                ( Printf.sprintf
                    "(exact (map (fn [(i : int)] (float (int/add 1 (int/mod i 2)))) (array/range \
                     %d :type int)))"
                    edges,
                  Array.init edges (fun i -> float (1 + (i mod 2))) );
              ])
        [ 0; 1; 2 ])
    (fixtures ());
  (* A radius cuts the search; malformed input is refused. *)
  let _, geometry = List.nth (fixtures ()) 3 in
  let view = view_of geometry in
  agree geometry
    "(graph/shortest_paths (sop/adjacency geo) (array/float 4 1.0) (array/int 1 0) :radius 1.5)"
    (oracle_paths ~radius:1.5 view (Array.make 4 1.) [| 0 |]);
  refused geometry "(graph/shortest_paths (sop/adjacency geo) (array/float 3 1.0) (array/int 1 0))"
    "E_GRAPH_LENGTH";
  refused geometry "(graph/shortest_paths (sop/adjacency geo) (array/float 4 -1.0) (array/int 1 0))"
    "E_GRAPH_LENGTH";
  refused geometry "(graph/shortest_paths (sop/adjacency geo) (array/float 4 1.0) (array/int 1 9))"
    "E_GRAPH_ROOT"

let color () =
  let decls =
    "(rows : {:offsets (array int) :values (array int)} {:offsets (array/int 1) :values (array/int \
     0)}) (order : (array int) (array/int 0))"
  in
  let _, geometry = List.nth (fixtures ()) 1 in
  let bad rows order code =
    refused ~decls
      ~inputs:[ ("rows", rows); ("order", order) ]
      geometry "(graph/color rows order)" code
  in
  let one = E.Record [ ("offsets", ints [| 0; 0 |]); ("values", ints [||]) ] in
  bad one (ints [| 0; 0 |]) "E_GRAPH_ORDER";
  bad one (ints [| 1 |]) "E_GRAPH_ORDER";
  bad
    (E.Record [ ("offsets", ints [| 0; 1 |]); ("values", ints [| 5 |]) ])
    (ints [| 0 |]) "E_GRAPH_ADJACENCY"

let chains () =
  let _, geometry = List.nth (fixtures ()) 3 in
  refused geometry "(graph/chains (sop/adjacency geo) :mask (array/int 3 1))" "E_GRAPH_MASK";
  refused geometry "(graph/chains (sop/adjacency geo) :mask (array/int 4 2))" "E_GRAPH_MASK"

(* Layer by layer oracle: the next layer takes, per point, the least
   (accumulated length, predecessor index) over the current layer. *)
let oracle_hops ?(blocked_point = fun _ -> false) ?(blocked_edge = fun _ -> false) ?(target = -1)
    (view : Rdk.Topology_index.View.view) lengths roots =
  let n = Array.length view.point_edge_offsets - 1 in
  let depth = Array.make n (-1) and dist = Array.make n 0. and parent = Array.make n (-1) in
  let layer = ref [] in
  Array.iter
    (fun r ->
      if depth.(r) < 0 then begin
        depth.(r) <- 0;
        layer := r :: !layer
      end)
    roots;
  layer := List.rev !layer;
  let discovered = ref (List.rev !layer) in
  let level = ref 0 in
  while !layer <> [] && (target < 0 || depth.(target) < 0) do
    let best = Hashtbl.create 16 and found = ref [] in
    List.iter
      (fun p ->
        for slot = view.point_edge_offsets.(p) to view.point_edge_offsets.(p + 1) - 1 do
          let e = view.point_edges.(slot) in
          let q = if view.edge_a.(e) = p then view.edge_b.(e) else view.edge_a.(e) in
          if
            q <> p
            && depth.(q) < 0
            && (not (blocked_edge e))
            && (q = target || not (blocked_point q))
          then begin
            let candidate = dist.(p) +. lengths.(e) in
            if not (Hashtbl.mem best q) then found := q :: !found;
            match Hashtbl.find_opt best q with
            | Some (d, w) when (d, w) <= (candidate, p) -> ()
            | _ -> Hashtbl.replace best q (candidate, p)
          end
        done)
      !layer;
    incr level;
    layer := List.rev !found;
    List.iter
      (fun q ->
        let d, p = Hashtbl.find best q in
        depth.(q) <- !level;
        dist.(q) <- d;
        parent.(q) <- p;
        discovered := q :: !discovered)
      !layer
  done;
  let path =
    if target < 0 || depth.(target) < 0 then [||]
    else begin
      let rec walk p acc = if parent.(p) < 0 then p :: acc else walk parent.(p) (p :: acc) in
      Array.of_list (walk target [])
    end
  in
  E.Record
    [
      ("distance", E.Float_array (Array.mapi (fun p d -> if depth.(p) < 0 then -1. else d) dist));
      ("parent", E.Int_array parent);
      ("path", E.Int_array path);
      ("order", E.Int_array (Array.of_list (List.rev !discovered)));
    ]

let hops () =
  let mask_lisp count modulus =
    Printf.sprintf
      "(exact (map (fn [(i : int)] (if (int/eq (int/mod i %d) 0) 1 0)) (array/range %d :type int)))"
      modulus count
  in
  List.iter
    (fun (_, geometry) ->
      let view = view_of geometry in
      let points = G.point_count geometry and edges = Array.length view.edge_a in
      List.iter
        (fun (lengths_lisp, lengths) ->
          List.iter
            (fun roots ->
              if roots <= points && points > 0 then
                List.iter
                  (fun target ->
                    if target < points then
                      List.iter
                        (fun (point_blocks, edge_blocks) ->
                          let blocked_point = Option.map (fun m p -> p mod m = 0) point_blocks
                          and blocked_edge = Option.map (fun m e -> e mod m = 0) edge_blocks in
                          let options =
                            Printf.sprintf ":hops true%s%s%s"
                              (if target >= 0 then Printf.sprintf " :target %d" target else "")
                              (match point_blocks with
                              | None -> ""
                              | Some m -> " :blocked_points " ^ mask_lisp points m)
                              (match edge_blocks with
                              | None -> ""
                              | Some m -> " :blocked_edges " ^ mask_lisp edges m)
                          in
                          agree geometry
                            (Printf.sprintf
                               "(graph/shortest_paths (sop/adjacency geo) %s (array/range %d :type \
                                int) %s)"
                               lengths_lisp roots options)
                            (oracle_hops ?blocked_point ?blocked_edge ~target view lengths
                               (Array.init roots Fun.id)))
                        [ (None, None); (Some 2, None); (None, Some 2); (Some 3, Some 2) ])
                  [ -1; 0; points - 1; 2 ])
            [ 1; 2 ])
        [
          (Printf.sprintf "(array/float %d 1.0)" edges, Array.make edges 1.);
          ( Printf.sprintf
              "(exact (map (fn [(i : int)] (float (int/add 1 (int/mod i 2)))) (array/range %d \
               :type int)))"
              edges,
            Array.init edges (fun i -> float (1 + (i mod 2))) );
        ])
    (fixtures ());
  (* Hop count beats length: the one-hop edge of length 9 wins over two short hops. *)
  let _, geometry = List.nth (fixtures ()) 3 in
  agree geometry
    "(graph/shortest_paths (sop/adjacency geo) (array/float 4 1.0) (array/int 1 0) :hops true \
     :target 2)"
    (oracle_hops ~target:2 (view_of geometry) (Array.make 4 1.) [| 0 |]);
  refused geometry
    "(graph/shortest_paths (sop/adjacency geo) (array/float 4 1.0) (array/int 1 0) :hops true \
     :target 9)"
    "E_GRAPH_TARGET";
  refused geometry
    "(graph/shortest_paths (sop/adjacency geo) (array/float 4 1.0) (array/int 1 0) :hops true \
     :radius 1.0)"
    "E_GRAPH_RADIUS";
  refused geometry
    "(graph/shortest_paths (sop/adjacency geo) (array/float 4 1.0) (array/int 1 0) :blocked_points \
     (array/int 4 0))"
    "E_GRAPH_OPTION";
  refused geometry
    "(graph/shortest_paths (sop/adjacency geo) (array/float 4 1.0) (array/int 1 0) :hops true \
     :blocked_points (array/int 3 0))"
    "E_GRAPH_MASK";
  refused geometry
    "(graph/shortest_paths (sop/adjacency geo) (array/float 4 1.0) (array/int 1 0) :hops true \
     :blocked_edges (array/int 4 2))"
    "E_GRAPH_MASK"

(* O(n^2) oracle of the Edge Transport forest: settle the queued point of least
   (distance, root, index); at an equal distance an unsettled neighbour takes the
   smaller root, then the smaller parent. *)
let oracle_lowest ?(blocked_edge = fun _ -> false) ?(target = -1)
    (view : Rdk.Topology_index.View.view) lengths roots =
  let n = Array.length view.point_edge_offsets - 1 in
  let dist = Array.make n infinity
  and parent = Array.make n (-1)
  and root = Array.make n (-1)
  and settled = Array.make n false
  and order = ref [] in
  Array.iter
    (fun r ->
      if dist.(r) = infinity then begin
        dist.(r) <- 0.;
        parent.(r) <- r;
        root.(r) <- r
      end)
    roots;
  let continue = ref true in
  while !continue do
    let best = ref (-1) in
    for p = 0 to n - 1 do
      if
        (not settled.(p))
        && dist.(p) < infinity
        && (!best < 0 || (dist.(p), root.(p), p) < (dist.(!best), root.(!best), !best))
      then best := p
    done;
    if !best < 0 then continue := false
    else begin
      let p = !best in
      settled.(p) <- true;
      order := p :: !order;
      for slot = view.point_edge_offsets.(p) to view.point_edge_offsets.(p + 1) - 1 do
        let e = view.point_edges.(slot) in
        let q = if view.edge_a.(e) = p then view.edge_b.(e) else view.edge_a.(e) in
        if (not (blocked_edge e)) && not settled.(q) then begin
          let candidate = dist.(p) +. lengths.(e) in
          if
            candidate < dist.(q)
            || candidate = dist.(q)
               && (root.(p) < root.(q) || (root.(p) = root.(q) && p < parent.(q)))
          then begin
            dist.(q) <- candidate;
            parent.(q) <- p;
            root.(q) <- root.(p)
          end
        end
      done
    end
  done;
  let parent = Array.mapi (fun p q -> if p = q then -1 else q) parent in
  let path =
    if target < 0 || dist.(target) = infinity then [||]
    else begin
      let rec walk p acc = if parent.(p) < 0 then p :: acc else walk parent.(p) (p :: acc) in
      Array.of_list (walk target [])
    end
  in
  E.Record
    [
      ("distance", E.Float_array (Array.map (fun d -> if d = infinity then -1. else d) dist));
      ("parent", E.Int_array parent);
      ("path", E.Int_array path);
      ("order", E.Int_array (Array.of_list (List.rev !order)));
    ]

let lowest () =
  List.iter
    (fun (_, geometry) ->
      let view = view_of geometry in
      let points = G.point_count geometry and edges = Array.length view.edge_a in
      List.iter
        (fun (lengths_lisp, lengths) ->
          List.iter
            (fun roots ->
              if roots <= points && points > 0 then
                List.iter
                  (fun target ->
                    if target < points then
                      List.iter
                        (fun blocks ->
                          let blocked_edge = Option.map (fun m e -> e mod m = 0) blocks in
                          let options =
                            Printf.sprintf ":ties \"lowest\"%s%s"
                              (if target >= 0 then Printf.sprintf " :target %d" target else "")
                              (match blocks with
                              | None -> ""
                              | Some m ->
                                  Printf.sprintf
                                    " :blocked_edges (exact (map (fn [(i : int)] (if (int/eq \
                                     (int/mod i %d) 0) 1 0)) (array/range %d :type int)))"
                                    m edges)
                          in
                          agree geometry
                            (Printf.sprintf
                               "(graph/shortest_paths (sop/adjacency geo) %s (array/range %d :type \
                                int) %s)"
                               lengths_lisp roots options)
                            (oracle_lowest ?blocked_edge ~target view lengths
                               (Array.init roots Fun.id)))
                        [ None; Some 2; Some 3 ])
                  [ -1; 0; points - 1 ])
            [ 1; 2; 3 ])
        [
          (Printf.sprintf "(array/float %d 1.0)" edges, Array.make edges 1.);
          ( Printf.sprintf
              "(exact (map (fn [(i : int)] (float (int/mod i 3))) (array/range %d :type int)))"
              edges,
            Array.init edges (fun i -> float (i mod 3)) );
        ])
    (fixtures ());
  let _, geometry = List.nth (fixtures ()) 3 in
  refused geometry
    "(graph/shortest_paths (sop/adjacency geo) (array/float 4 1.0) (array/int 1 0) :ties \
     \"lowest\" :blocked_points (array/int 4 0))"
    "E_GRAPH_OPTION";
  refused geometry
    "(graph/shortest_paths (sop/adjacency geo) (array/float 4 1.0) (array/int 1 0) :ties \
     \"lowest\" :hops true)"
    "E_GRAPH_OPTION";
  refused geometry
    "(graph/shortest_paths (sop/adjacency geo) (array/float 4 1.0) (array/int 1 0) :ties \"other\")"
    "E_GRAPH_OPTION"

(* Independent forest: follow parents with a visited-in-this-walk check. *)
let oracle_forest ?mask parents =
  let n = Array.length parents in
  let selected p = match mask with None -> true | Some m -> m.(p) = 1 in
  let tree =
    Array.init n (fun p ->
        if not (selected p) then -1
        else
          let q = parents.(p) in
          if q < 0 || q >= n || q = p || not (selected q) then p else q)
  in
  let counts = Array.make (n + 1) 0 in
  Array.iteri (fun p q -> if q >= 0 && q <> p then counts.(q + 1) <- counts.(q + 1) + 1) tree;
  for p = 0 to n - 1 do
    counts.(p + 1) <- counts.(p + 1) + counts.(p)
  done;
  let children = Array.make counts.(n) 0 and fill = Array.sub counts 0 n in
  Array.iteri
    (fun p q ->
      if q >= 0 && q <> p then begin
        children.(fill.(q)) <- p;
        fill.(q) <- fill.(q) + 1
      end)
    tree;
  let depth = Array.make n (-1) and root = Array.make n (-1) and order = ref [] in
  for r = 0 to n - 1 do
    if tree.(r) = r then begin
      let queue = Queue.create () in
      depth.(r) <- 0;
      root.(r) <- r;
      Queue.add r queue;
      while not (Queue.is_empty queue) do
        let p = Queue.pop queue in
        order := p :: !order;
        for at = counts.(p) to counts.(p + 1) - 1 do
          let c = children.(at) in
          depth.(c) <- depth.(p) + 1;
          root.(c) <- r;
          Queue.add c queue
        done
      done
    end
  done;
  E.Record
    [
      ("order", ints (Array.of_list (List.rev !order)));
      ("depth", ints depth);
      ("root", ints root);
      ("offsets", ints counts);
      ("children", ints children);
    ]

let forest () =
  let decls = "(parents : (array int) (array/int 0)) (mask : (array int) (array/int 0))" in
  let geometry = snd (List.nth (fixtures ()) 0) in
  List.iter
    (fun parents ->
      let n = Array.length parents in
      List.iter
        (fun mask ->
          let body, inputs =
            match mask with
            | None -> ("(graph/forest parents)", [ ("parents", ints parents) ])
            | Some m ->
                ( "(graph/forest parents :mask mask)",
                  [ ("parents", ints parents); ("mask", ints m) ] )
          in
          agree ~decls ~inputs geometry body (oracle_forest ?mask parents))
        [
          None;
          Some (Array.make n 1);
          Some (Array.init n (fun i -> i land 1));
          Some (Array.init n (fun i -> if i mod 3 = 1 then 0 else 1));
          Some (Array.make n 0);
        ])
    [
      [||];
      [| 0 |];
      [| -1 |];
      [| -1; 0; 0; 1; 1; 2 |];
      [| 5; 0; 1; 2; 3; 4 |];
      [| 1; 0; 1; 9; -4; 4 |];
      [| -1; -1; 1; 1; 3; 2; 2; 7 |];
      [| 3; 3; 3; 3; 0; 4; 4; 5 |];
    ];
  let bad parents mask code =
    refused ~decls
      ~inputs:[ ("parents", ints parents); ("mask", ints mask) ]
      geometry "(graph/forest parents :mask mask)" code
  in
  bad [| 0; 0 |] [| 1 |] "E_GRAPH_MASK";
  bad [| 0; 0 |] [| 1; 2 |] "E_GRAPH_MASK"

let () =
  match Sys.argv with
  | [| _; "shortest_paths" |] -> shortest_paths ()
  | [| _; "color" |] -> color ()
  | [| _; "chains" |] -> chains ()
  | [| _; "hops" |] -> hops ()
  | [| _; "forest" |] -> forest ()
  | [| _; "lowest" |] -> lowest ()
  | _ ->
      shortest_paths ();
      hops ();
      lowest ();
      forest ();
      color ();
      chains ()
