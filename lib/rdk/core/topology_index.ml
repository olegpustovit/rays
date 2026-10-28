type t = {
  topology_id : int;
  point_count : int;

  primitive_of_vertex : int array;
  next_vertex : int array;
  previous_vertex : int array;
  edge_of_vertex : int array;
  opposite_vertex : int array;
  edge_a : int array;
  edge_b : int array;
  edge_slots : int array;
  edge_offsets : int array;
  edge_vertices : int array;
  point_offsets : int array;
  point_vertices : int array;
  point_edge_offsets : int array;
  point_edges : int array;
  boundary_edge_count : int;
  non_manifold_edge_count : int;
}

let next_power_of_two value =
  let result = ref 8 in
  while !result < value do
    if !result > max_int / 2 then invalid_arg "Topology_index.create: topology is too large";
    result := !result lsl 1
  done;
  !result

let hash_pair a b =
  let value = Rays_math.Hash.mix a (b * 31_337) in
  (value lxor (value lsr 16)) land max_int

let create_from_point_index ?cancel point_index topology =
  let source = Topology.View.view topology in
  let point_view = Point_index.View.view point_index in
  let point_count = source.point_count
  and vertex_count = Array.length source.vertex_points
  and primitive_count = Bytes.length source.primitive_kinds in
  let primitive_of_vertex = point_view.primitive_of_vertex
  and next_vertex = Array.make vertex_count (-1)
  and previous_vertex = Array.make vertex_count (-1)
  and edge_of_vertex = Array.make vertex_count (-1)
  and opposite_vertex = Array.make vertex_count (-1) in
  let outgoing_count = ref 0 in
  for primitive = 0 to primitive_count - 1 do
    if primitive land 1023 = 0 then Cancel.check_opt cancel;
    let first = source.primitive_offsets.(primitive)
    and last = source.primitive_offsets.(primitive + 1) in
    let cyclic = Bytes.get source.primitive_kinds primitive <> '\001' in
    for vertex = first to last - 1 do
      if vertex + 1 < last then next_vertex.(vertex) <- vertex + 1
      else if cyclic then next_vertex.(vertex) <- first;
      if vertex > first then previous_vertex.(vertex) <- vertex - 1
      else if cyclic then previous_vertex.(vertex) <- last - 1;
      if next_vertex.(vertex) >= 0 then incr outgoing_count
    done
  done;
  if !outgoing_count > max_int / 2 then
    invalid_arg "Topology_index.create: edge count exceeds table limits";
  let table_capacity = next_power_of_two (max 8 (!outgoing_count * 2)) in
  let table_edges = Array.make table_capacity (-1)
  and edge_a_full = Array.make !outgoing_count 0
  and edge_b_full = Array.make !outgoing_count 0
  and edge_counts_full = Array.make !outgoing_count 0 in
  let edge_count = ref 0 and mask = table_capacity - 1 in
  let find_or_add a b =
    let a, b = if a <= b then a, b else b, a in
    let slot = ref (hash_pair a b land mask) in
    while table_edges.(!slot) >= 0
          && (let edge = table_edges.(!slot) in
              edge_a_full.(edge) <> a || edge_b_full.(edge) <> b) do
      slot := (!slot + 1) land mask
    done;
    if table_edges.(!slot) >= 0 then table_edges.(!slot)
    else begin
      let edge = !edge_count in
      incr edge_count;
      table_edges.(!slot) <- edge;
      edge_a_full.(edge) <- a;
      edge_b_full.(edge) <- b;
      edge
    end
  in
  for vertex = 0 to vertex_count - 1 do
    if vertex land 16383 = 0 then Cancel.check_opt cancel;
    let next = next_vertex.(vertex) in
    if next >= 0 then begin
      let edge = find_or_add source.vertex_points.(vertex)
          source.vertex_points.(next) in
      edge_of_vertex.(vertex) <- edge;
      edge_counts_full.(edge) <- edge_counts_full.(edge) + 1
    end
  done;
  let edge_count = !edge_count in
  let edge_a = Array.sub edge_a_full 0 edge_count
  and edge_b = Array.sub edge_b_full 0 edge_count
  and edge_offsets = Array.make (edge_count + 1) 0 in
  let boundary_edge_count = ref 0 and non_manifold_edge_count = ref 0 in
  for edge = 0 to edge_count - 1 do
    let count = edge_counts_full.(edge) in
    edge_offsets.(edge + 1) <- edge_offsets.(edge) + count;
    if count = 1 then incr boundary_edge_count
    else if count > 2 then incr non_manifold_edge_count
  done;
  let edge_vertices = Array.make !outgoing_count 0
  and edge_cursor = Array.copy edge_offsets in
  for vertex = 0 to vertex_count - 1 do
    let edge = edge_of_vertex.(vertex) in
    if edge >= 0 then begin
      let output = edge_cursor.(edge) in
      edge_vertices.(output) <- vertex;
      edge_cursor.(edge) <- output + 1
    end
  done;
  for edge = 0 to edge_count - 1 do
    let first = edge_offsets.(edge) and last = edge_offsets.(edge + 1) in
    if last - first = 2 then begin
      let left = edge_vertices.(first) and right = edge_vertices.(first + 1) in
      opposite_vertex.(left) <- right;
      opposite_vertex.(right) <- left
    end
  done;
  let point_offsets = point_view.point_offsets
  and point_vertices = point_view.point_vertices in
  let point_edge_offsets = Array.make (point_count + 1) 0 in
  for edge = 0 to edge_count - 1 do
    let a = edge_a.(edge) and b = edge_b.(edge) in
    point_edge_offsets.(a + 1) <- point_edge_offsets.(a + 1) + 1;
    if b <> a then point_edge_offsets.(b + 1) <- point_edge_offsets.(b + 1) + 1
  done;
  for point = 0 to point_count - 1 do
    point_edge_offsets.(point + 1) <- point_edge_offsets.(point + 1)
        + point_edge_offsets.(point)
  done;
  let point_edges = Array.make point_edge_offsets.(point_count) 0
  and point_edge_cursor = Array.copy point_edge_offsets in
  for edge = 0 to edge_count - 1 do
    let add point =
      let output = point_edge_cursor.(point) in
      point_edges.(output) <- edge;
      point_edge_cursor.(point) <- output + 1 in
    add edge_a.(edge);
    if edge_b.(edge) <> edge_a.(edge) then add edge_b.(edge)
  done;
  {
    topology_id = Topology.data_id topology;
    point_count;  primitive_of_vertex; next_vertex;
    previous_vertex; edge_of_vertex; opposite_vertex; edge_a; edge_b;
    edge_slots = table_edges;
    edge_offsets; edge_vertices; point_offsets; point_vertices;
    point_edge_offsets; point_edges;
    boundary_edge_count = !boundary_edge_count;
    non_manifold_edge_count = !non_manifold_edge_count;
  }

(* Bounded: at most [cache_capacity] live indices, released with their topology. *)
let cache_capacity = 64
let cache = Support.Identity_cache.create ~id:Topology.data_id cache_capacity

let create ?cancel topology =
  Cancel.check_opt cancel;
  Support.Identity_cache.find_or_add cache topology (fun () ->
    create_from_point_index ?cancel (Point_index.create ?cancel topology)
      topology)

let point_count value = value.point_count
let topology_data_id value = value.topology_id
let edge_count value = Array.length value.edge_a

let get name values index =
  if index < 0 || index >= Array.length values then
    invalid_arg ("Topology_index." ^ name ^ ": index out of bounds");
  values.(index)

let primitive_of_vertex value index = get "primitive_of_vertex" value.primitive_of_vertex index
let next_vertex value index = get "next_vertex" value.next_vertex index
let edge_of_vertex value index = get "edge_of_vertex" value.edge_of_vertex index
let edge_points value edge = get "edge_points" value.edge_a edge, value.edge_b.(edge)
let find_edge_index value ~a ~b =
  if a < 0 || b < 0 || a >= value.point_count || b >= value.point_count then -1
  else
    let a, b = if a <= b then a, b else b, a in
    let mask = Array.length value.edge_slots - 1 in
    let slot = ref (hash_pair a b land mask) and result = ref (-1)
    and searching = ref true in
    while !searching do
      let edge = value.edge_slots.(!slot) in
      if edge < 0 then searching := false
      else if value.edge_a.(edge) = a && value.edge_b.(edge) = b then begin
        result := edge; searching := false
      end else slot := (!slot + 1) land mask
    done;
    !result
let find_edge value ~a ~b =
  match find_edge_index value ~a ~b with -1 -> None | edge -> Some edge
let edge_incidence_count value edge =
  ignore (get "edge_incidence_count" value.edge_a edge);
  value.edge_offsets.(edge + 1) - value.edge_offsets.(edge)
let edge_vertex value ~edge ~local =
  let count = edge_incidence_count value edge in
  if local < 0 || local >= count then invalid_arg "Topology_index.edge_vertex: local index out of bounds";
  value.edge_vertices.(value.edge_offsets.(edge) + local)
let point_incidence_count value point =
  if point < 0 || point >= value.point_count then invalid_arg "Topology_index.point_incidence_count: point out of bounds";
  value.point_offsets.(point + 1) - value.point_offsets.(point)
let point_vertex value ~point ~local =
  let count = point_incidence_count value point in
  if local < 0 || local >= count then invalid_arg "Topology_index.point_vertex: local index out of bounds";
  value.point_vertices.(value.point_offsets.(point) + local)
let point_edge_count value point =
  if point < 0 || point >= value.point_count then invalid_arg "Topology_index.point_edge_count: point out of bounds";
  value.point_edge_offsets.(point + 1) - value.point_edge_offsets.(point)
let point_edge value ~point ~local =
  let count = point_edge_count value point in
  if local < 0 || local >= count then invalid_arg "Topology_index.point_edge: local index out of bounds";
  value.point_edges.(value.point_edge_offsets.(point) + local)
let boundary_edge_count value = value.boundary_edge_count
let non_manifold_edge_count value = value.non_manifold_edge_count

type index = t

module View = struct
  type view = {
    primitive_of_vertex : int array;
    next_vertex : int array;
    previous_vertex : int array;
    edge_of_vertex : int array;
    opposite_vertex : int array;
    edge_a : int array;
    edge_b : int array;
    edge_offsets : int array;
    edge_vertices : int array;
    point_offsets : int array;
    point_vertices : int array;
    point_edge_offsets : int array;
    point_edges : int array;
  }
  let view (value : index) = {
    primitive_of_vertex = value.primitive_of_vertex;
    next_vertex = value.next_vertex;
    previous_vertex = value.previous_vertex;
    edge_of_vertex = value.edge_of_vertex;
    opposite_vertex = value.opposite_vertex;
    edge_a = value.edge_a;
    edge_b = value.edge_b;
    edge_offsets = value.edge_offsets;
    edge_vertices = value.edge_vertices;
    point_offsets = value.point_offsets;
    point_vertices = value.point_vertices;
    point_edge_offsets = value.point_edge_offsets;
    point_edges = value.point_edges;
  }

end
