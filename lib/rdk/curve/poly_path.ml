open Rays_math

let fail message = raise (Error.Fail message)

type traced = {
  path_count : int;
  edge_order : int array;
  path_offsets : int array;
  path_start : int array;
  isolated_loop : Bytes.t;
}

(* A compact edge permutation describes every maximal path of a graph whose
   edges are [graph_a.(e)]-[graph_b.(e)] (no self edges). Branch paths are
   emitted by point number; isolated degree-two loops follow by their first
   remaining edge. Work-stealing therefore cannot affect ordering. *)
let trace_paths ~cancel ~point_count ~graph_count ~graph_a ~graph_b =
  let point_offsets = Array.make (point_count + 1) 0 in
  for edge = 0 to graph_count - 1 do
    point_offsets.(graph_a.(edge) + 1) <- point_offsets.(graph_a.(edge) + 1) + 1;
    point_offsets.(graph_b.(edge) + 1) <- point_offsets.(graph_b.(edge) + 1) + 1
  done;
  for point = 0 to point_count - 1 do
    point_offsets.(point + 1) <- point_offsets.(point + 1) + point_offsets.(point)
  done;
  let point_edges = Array.make (graph_count * 2) 0
  and cursor = Array.copy point_offsets in
  for edge = 0 to graph_count - 1 do
    let a = graph_a.(edge) and b = graph_b.(edge) in
    point_edges.(cursor.(a)) <- edge; cursor.(a) <- cursor.(a) + 1;
    point_edges.(cursor.(b)) <- edge; cursor.(b) <- cursor.(b) + 1
  done;
  let degree point = point_offsets.(point + 1) - point_offsets.(point) in
  let visited = Bytes.make graph_count '\000'
  and edge_order = Array.make graph_count 0
  and path_offsets = Array.make (graph_count + 1) 0
  and path_start = Array.make graph_count 0
  and isolated_loop = Bytes.make graph_count '\000' in
  let path_count = ref 0 and edge_at = ref 0 in
  let other edge point = if graph_a.(edge) = point
    then graph_b.(edge) else graph_a.(edge) in
  let walk_current = ref 0 and walk_edge = ref 0 in
  let trace ~isolated start first_edge =
    let path = !path_count in
    incr path_count;
    path_offsets.(path) <- !edge_at;
    path_start.(path) <- start;
    if isolated then Bytes.set isolated_loop path '\001';
    walk_current := start;
    walk_edge := first_edge;
    while !walk_edge >= 0 do
      let edge = !walk_edge in
      if Bytes.get visited edge <> '\000' then walk_edge := -1
      else begin
        Bytes.set visited edge '\001';
        edge_order.(!edge_at) <- edge;
        incr edge_at;
        let next_point = other edge !walk_current in
        if degree next_point <> 2 then begin
          walk_current := next_point;
          walk_edge := -1
        end else begin
          let first = point_offsets.(next_point) in
          let left = point_edges.(first) and right = point_edges.(first + 1) in
          let next_edge = if left = edge then right else left in
          walk_current := next_point;
          if Bytes.get visited next_edge <> '\000' then walk_edge := -1
          else walk_edge := next_edge
        end
      end
    done;
    path_offsets.(path + 1) <- !edge_at in
  for point = 0 to point_count - 1 do
    if point land 16_383 = 0 then Cancel.check_opt cancel;
    if degree point <> 2 then
      for incidence = point_offsets.(point) to point_offsets.(point + 1) - 1 do
        let edge = point_edges.(incidence) in
        if Bytes.get visited edge = '\000' then trace ~isolated:false point edge
      done
  done;
  for edge = 0 to graph_count - 1 do
    if edge land 16_383 = 0 then Cancel.check_opt cancel;
    if Bytes.get visited edge = '\000' then trace ~isolated:true graph_a.(edge) edge
  done;
  if !edge_at <> graph_count then
    fail "PolyPath internal graph traversal did not consume every edge";
  { path_count = !path_count; edge_order; path_offsets; path_start; isolated_loop }

(* Point chains of the selected non-self edges of a unique-edge graph, as the
   open polylines [run] emits: an isolated loop repeats its start point last
   and is flagged in [closed]. *)
let chains ~point_count ~edge_a ~edge_b selected =
  let cancel = None in try
    let total = Array.length edge_a in
    let graph_count = ref 0 in
    for edge = 0 to total - 1 do
      if selected edge && edge_a.(edge) <> edge_b.(edge) then incr graph_count
    done;
    let graph_a = Array.make !graph_count 0 and graph_b = Array.make !graph_count 0
    and filled = ref 0 in
    for edge = 0 to total - 1 do
      if selected edge && edge_a.(edge) <> edge_b.(edge) then begin
        graph_a.(!filled) <- edge_a.(edge); graph_b.(!filled) <- edge_b.(edge);
        incr filled
      end
    done;
    let graph_count = !graph_count in
    let traced = trace_paths ~cancel ~point_count ~graph_count ~graph_a ~graph_b in
    let offsets = Array.make (traced.path_count + 1) 0 in
    for path = 0 to traced.path_count - 1 do
      offsets.(path + 1) <- offsets.(path) + traced.path_offsets.(path + 1)
        - traced.path_offsets.(path) + 1
    done;
    let values = Array.make offsets.(traced.path_count) 0 in
    for path = 0 to traced.path_count - 1 do
      let current = ref traced.path_start.(path) and at = ref offsets.(path) in
      for order = traced.path_offsets.(path) to traced.path_offsets.(path + 1) - 1 do
        let edge = traced.edge_order.(order) in
        values.(!at) <- !current; incr at;
        current := if graph_a.(edge) = !current then graph_b.(edge) else graph_a.(edge)
      done;
      values.(!at) <- !current
    done;
    Ok (offsets, values, Array.init traced.path_count (fun path ->
      Char.code (Bytes.get traced.isolated_loop path)))
  with Error.Fail message -> Error message
