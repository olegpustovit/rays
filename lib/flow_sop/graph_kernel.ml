(* Pure array-in, array-out graph algorithms (E.1-E.3). They never read
   geometry: the adjacency arrives as the record [sop/adjacency] returns (or a
   CSR row record for coloring), so the same call is a JavaScript host twin. *)
module E = Flow.Eval

let refuse code message = raise (Diagnostic.Error (Diagnostic.error ~code message))

let int_array record name = match List.assoc_opt name record with
  | Some (E.Int_array values) -> values
  | _ -> refuse "E_GRAPH_ADJACENCY" ("Graph adjacency needs the integer array " ^ name ^ ".")

let record = function
  | Some (E.Record fields) -> fields
  | _ -> refuse "E_GRAPH_ADJACENCY" "Graph adjacency must be a record of integer arrays."

(* CSR rows: monotone offsets from zero to the value count, values in range. *)
let check_rows label ~rows ~limit offsets values =
  if Array.length offsets <> rows + 1 || offsets.(0) <> 0 || offsets.(rows) <> Array.length values then
    refuse "E_GRAPH_ADJACENCY" (label ^ " offsets must start at zero and end at the value count.");
  for row = 0 to rows - 1 do
    if offsets.(row + 1) < offsets.(row) then
      refuse "E_GRAPH_ADJACENCY" (label ^ " offsets must be monotone.")
  done;
  Array.iter (fun value ->
    if value < 0 || value >= limit then refuse "E_GRAPH_ADJACENCY" (label ^ " value is out of range."))
    values

(* The point/edge part of a sop/adjacency record. *)
let point_graph adjacency =
  let fields = record adjacency in
  let point_edge_offsets = int_array fields "point_edge_offsets"
  and point_edges = int_array fields "point_edges"
  and edge_a = int_array fields "edge_a" and edge_b = int_array fields "edge_b" in
  if Array.length point_edge_offsets = 0 then
    refuse "E_GRAPH_ADJACENCY" "Graph adjacency point_edge_offsets must hold the point count plus one.";
  let points = Array.length point_edge_offsets - 1 and edges = Array.length edge_a in
  if Array.length edge_b <> edges then
    refuse "E_GRAPH_ADJACENCY" "Graph adjacency edge_a and edge_b differ in length.";
  Array.iter (fun p -> if p < 0 || p >= points then
    refuse "E_GRAPH_ADJACENCY" "Graph adjacency edge endpoint is out of range.") edge_a;
  Array.iter (fun p -> if p < 0 || p >= points then
    refuse "E_GRAPH_ADJACENCY" "Graph adjacency edge endpoint is out of range.") edge_b;
  check_rows "Graph adjacency point_edges" ~rows:points ~limit:edges point_edge_offsets point_edges;
  points, edges, point_edge_offsets, point_edges, edge_a, edge_b

let binary_mask ?(per = "edge") label count = function
  | Some (E.Int_array mask) when Array.length mask = count ->
      Array.iter (fun bit -> if bit <> 0 && bit <> 1 then
        refuse "E_GRAPH_MASK" (label ^ " holds only 0 and 1.")) mask;
      mask
  | _ -> refuse "E_GRAPH_MASK" (label ^ " is an integer array with one entry per " ^ per ^ ".")

let flag name args = match List.assoc_opt name args with
  | None -> false
  | Some (E.Bool value) -> value
  | Some _ -> refuse "E_GRAPH_OPTION" ("Graph " ^ name ^ " is a boolean.")

(* The tree path from the target back to its root, root first; empty when no
   target is given or it was not reached. *)
let tree_path ~parent ~reached target =
  if target < 0 || not (reached target) then [||]
  else begin
    let length = ref 1 and at = ref target in
    while parent.(!at) >= 0 do incr length; at := parent.(!at) done;
    let path = Array.make !length target and at = ref target in
    for slot = !length - 1 downto 0 do
      path.(slot) <- !at;
      at := parent.(!at)
    done;
    path
  end

let shortest_paths args =
  let points, edges, point_edge_offsets, point_edges, edge_a, edge_b =
    point_graph (List.assoc_opt "adjacency" args) in
  let lengths = match List.assoc_opt "lengths" args with
    | Some (E.Float_array lengths) when Array.length lengths = edges -> lengths
    | _ -> refuse "E_GRAPH_LENGTH" "Graph lengths is a float array with one entry per edge." in
  Array.iter (fun length -> if not (length >= 0.) then
    refuse "E_GRAPH_LENGTH" "Graph lengths must not be negative.") lengths;
  let roots = match List.assoc_opt "roots" args with
    | Some (E.Int_array roots) -> roots
    | _ -> refuse "E_GRAPH_ROOT" "Graph roots is an integer array of point indices." in
  Array.iter (fun root -> if root < 0 || root >= points then
    refuse "E_GRAPH_ROOT" "Graph root is not a point index.") roots;
  let hops = flag "hops" args in
  let lowest = match List.assoc_opt "ties" args with
    | None | Some (E.Text "first") -> false
    | Some (E.Text "lowest") -> true
    | Some _ -> refuse "E_GRAPH_OPTION" "Graph ties is first or lowest." in
  let target = match List.assoc_opt "target" args with
    | None -> -1
    | Some (E.Int target) when target >= 0 && target < points -> target
    | Some _ -> refuse "E_GRAPH_TARGET" "Graph target is a point index." in
  let masked = List.mem_assoc "blocked_points" args || List.mem_assoc "blocked_edges" args in
  if not hops && not lowest && masked then
    refuse "E_GRAPH_OPTION" "Graph blocked_points and blocked_edges need :hops true or :ties \"lowest\".";
  if (hops || lowest) && List.mem_assoc "radius" args then
    refuse "E_GRAPH_RADIUS" "Graph radius does not combine with :hops true or :ties \"lowest\".";
  if hops && lowest then refuse "E_GRAPH_OPTION" "Graph :hops true does not combine with :ties \"lowest\".";
  if lowest && List.mem_assoc "blocked_points" args then
    refuse "E_GRAPH_OPTION" "Graph :ties \"lowest\" takes blocked_edges only.";
  let blocked_edge = Option.map (fun mask -> let mask = binary_mask "Graph blocked_edges" edges (Some mask) in
      fun edge -> mask.(edge) = 1) (List.assoc_opt "blocked_edges" args) in
  if hops then begin
    let blocked_point = Option.map (fun mask -> let mask = binary_mask ~per:"point" "Graph blocked_points" points (Some mask) in
        fun point -> mask.(point) = 1) (List.assoc_opt "blocked_points" args) in
    let depths = Array.make points (-1) and distances = Array.make points 0.
    and previous = Array.make points (-1) and queue = Array.make points 0 in
    (try Rdk.Shortest_paths.hop_search { blocked_point = Option.value ~default:(fun _ -> false) blocked_point; blocked_edge = Option.value ~default:(fun _ -> false) blocked_edge; target; depths; distances; previous; queue; point_edge_offsets; point_edges; edge_a; edge_b; lengths } roots
     with Rdk.Shortest_paths.Hop_overflow ->
       refuse "E_GRAPH_LENGTH" "Graph accumulated path length is not representable.");
    let parent = Array.mapi (fun point depth -> if depth < 0 then -1 else previous.(point)) depths in
    let reached = Array.fold_left (fun count depth -> if depth >= 0 then count + 1 else count) 0 depths in
    Array.mapi (fun point depth -> if depth < 0 then -1. else distances.(point)) depths, parent,
    tree_path ~parent ~reached:(fun point -> depths.(point) >= 0) target, Array.sub queue 0 reached
  end else if lowest then begin
    let forest = Rdk.Shortest_paths.forest_search
      ~valid_edge:(match blocked_edge with None -> fun _ -> true | Some blocked -> fun edge -> not (blocked edge))
      ~capacity:points ~point_edge_offsets ~point_edges ~edge_a ~edge_b ~lengths roots in
    let parent = Array.mapi (fun point p -> if p = point then -1 else p) forest.parent in
    Array.map (fun d -> if d = infinity then -1. else d) forest.distance, parent,
    tree_path ~parent ~reached:(fun point -> forest.distance.(point) < infinity) target,
    Array.sub forest.order 0 forest.order_count
  end else begin
    let radius = match List.assoc_opt "radius" args with
      | None -> infinity
      | Some (E.Float radius) when radius >= 0. -> radius
      | Some _ -> refuse "E_GRAPH_RADIUS" "Graph radius is a non-negative float." in
    let distances, parents = Rdk.Shortest_paths.search { cancel = None; radius; point_edge_offsets; point_edges; edge_a; edge_b; lengths } roots in
    (* Flow floats are finite: a point outside every root's reach reports -1. *)
    Array.map (fun d -> if d = infinity then -1. else d) distances, parents,
    tree_path ~parent:parents ~reached:(fun point -> distances.(point) < infinity) target, [||]
  end


let forest args =
  let parents = match List.assoc_opt "parents" args with
    | Some (E.Int_array parents) -> parents
    | _ -> refuse "E_GRAPH_PARENT" "Graph parents is an integer array with one entry per point." in
  let points = Array.length parents in
  let selected = Option.map (fun mask -> let mask = binary_mask ~per:"point" "Graph forest mask" points (Some mask) in
      mask) (List.assoc_opt "mask" args) in
  let select = Option.map (fun mask point -> mask.(point) = 1) selected in
  let selected_count = match selected with
    | None -> points | Some mask -> Array.fold_left (+) 0 mask in
  let forest = Rdk.Forest.scan { selected = select; selected_count } parents in
  let depth, root = Rdk.Forest.labels forest in
  Array.sub forest.order 0 forest.order_count, depth, root, forest.child_offsets, forest.children

let color args =
  let fields = record (List.assoc_opt "adjacency" args) in
  let offsets = int_array fields "offsets" and neighbors = int_array fields "values" in
  if Array.length offsets = 0 then
    refuse "E_GRAPH_ADJACENCY" "Graph adjacency offsets must hold the node count plus one.";
  let nodes = Array.length offsets - 1 in
  check_rows "Graph adjacency" ~rows:nodes ~limit:nodes offsets neighbors;
  let order = match List.assoc_opt "order" args with
    | Some (E.Int_array order) -> order
    | _ -> refuse "E_GRAPH_ORDER" "Graph order is an integer array of node indices." in
  let seen = Array.make nodes false in
  Array.iter (fun node ->
    if node < 0 || node >= nodes then refuse "E_GRAPH_ORDER" "Graph order holds a node outside the graph.";
    if seen.(node) then refuse "E_GRAPH_ORDER" "Graph order lists a node twice.";
    seen.(node) <- true) order;
  let colors = Rdk.Graph_color.greedy ~offsets ~neighbors order in
  colors, 1 + Array.fold_left max (-1) colors

let chains args =
  let points, edges, _, _, edge_a, edge_b = point_graph (List.assoc_opt "adjacency" args) in
  let selected = match List.assoc_opt "mask" args with
    | None -> fun _ -> true
    | mask -> let mask = binary_mask "Graph chains mask" edges mask in fun edge -> mask.(edge) = 1 in
  match Rdk.Poly_path.chains ~point_count:points ~edge_a ~edge_b selected with
  | Ok result -> result
  | Error message -> refuse "E_GRAPH_CHAINS" message

let resolve = function
  | E.Source (name, _, args) ->
      (try
         let component = match List.assoc_opt "component" args with
           | Some (E.Text component) -> component
           | _ -> refuse "E_DATA_SOURCE" "Graph source requires a component." in
         let unknown () = refuse "E_DATA_SOURCE" "Unknown graph component." in
         match name with
         | "graph/shortest_paths" ->
             let distance, parent, path, order = shortest_paths args in
             Ok (match component with "distance" -> E.Float_array distance
               | "parent" -> E.Int_array parent | "path" -> E.Int_array path
               | "order" -> E.Int_array order
               | _ -> unknown ())
         | "graph/forest" ->
             let order, depth, root, offsets, children = forest args in
             Ok (match component with "order" -> E.Int_array order
               | "depth" -> E.Int_array depth | "root" -> E.Int_array root
               | "offsets" -> E.Int_array offsets | "children" -> E.Int_array children
               | _ -> unknown ())
         | "graph/color" ->
             let colors, count = color args in
             Ok (match component with "color" -> E.Int_array colors
               | "count" -> E.Int count | _ -> unknown ())
         | "graph/chains" ->
             let offsets, values, closed = chains args in
             Ok (match component with "offsets" -> E.Int_array offsets
               | "values" -> E.Int_array values | "closed" -> E.Int_array closed
               | _ -> unknown ())
         | _ -> refuse "E_DATA_SOURCE" "Unknown graph data source."
       with Diagnostic.Error diagnostic -> Error diagnostic)
  | _ -> Error (Diagnostic.error ~code:"E_DATA_SOURCE" "Unknown graph data source.")
