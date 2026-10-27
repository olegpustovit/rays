open Rays_math

exception Poly_path_error of string

let fail message = raise (Poly_path_error message)

let run_ranges ?(grain = 16_384) count operation =
  if grain <= 0 then fail "grain must be positive";
  let ranges = (count + grain - 1) / grain in
  if ranges > 0 then
    Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(ranges - 1) (fun range ->
      let first = range * grain and last = min count ((range + 1) * grain) in
      operation first last)

let next_power_of_two value =
  let result = ref 8 in
  while !result < value do
    if !result > Sys.max_array_length / 2 then
      fail "PolyPath edge table exceeds OCaml array limits";
    result := !result lsl 1
  done;
  !result

let[@inline always] hash_pair a b =
  let value = Hash.mix a (b * 31_337) in
  (value lxor (value lsr 16)) land max_int

let remap_attribute ?cancel ?grain mapping attribute =
  Topology_remap.attribute ?cancel ~grain:(Option.value ~default:16_384 grain)
    mapping attribute

let set_bit bits index =
  let byte = index lsr 3 and mask = 1 lsl (index land 7) in
  Bytes.set bits byte (Char.chr (Char.code (Bytes.get bits byte) lor mask))

let source_degrees ?edges index_view point_count =
  let degrees = Array.make point_count 0 in
  for edge = 0 to Array.length index_view.Topology_index.Private.edge_a - 1 do
    let a = index_view.edge_a.(edge) and b = index_view.edge_b.(edge) in
    if (match edges with None -> true | Some group -> Edge_group.mem edge group)
        && a <> b then begin
      degrees.(a) <- degrees.(a) + 1;
      degrees.(b) <- degrees.(b) + 1
    end
  done;
  degrees

let endpoint_point_map ?cancel ?edges ~only_end_points ~maximum_distance
    geometry source_view =
  let point_count = Geometry.point_count geometry in
  let degrees = source_degrees ?edges source_view point_count in
  let compatible left right =
    if only_end_points then degrees.(left) = 1 && degrees.(right) = 1
    else degrees.(left) = 1 || degrees.(right) = 1 in
  match Point_clusters.create ?cancel ~operation:"PolyPath endpoint connection"
      ~tolerance:maximum_distance ~compatible ~transitive:true geometry with
  | Error message -> fail message
  | Ok Point_clusters.Identity -> None
  | Ok (Point_clusters.Clusters clusters) ->
      let mapping = Array.make point_count 0 in
      for point = 0 to point_count - 1 do
        mapping.(point) <- clusters.representatives.(clusters.of_point.(point))
      done;
      Some mapping

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
let chains ?cancel ~point_count ~edge_a ~edge_b selected =
  try
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
  with Poly_path_error message -> Error message

let run ?cancel ?grain ?edges ?(preserve_source_payload = true)
    ?(connect_end_points = false)
    ?(maximum_distance = 0.001) ?(connect_only_to_other_end_points = false)
    ?(make_isolated_loops_closed = false) geometry =
  try
    (match grain with Some value when value <= 0 -> fail "grain must be positive"
     | _ -> ());
    if not (Float.is_finite maximum_distance) || maximum_distance < 0. then
      fail "PolyPath maximum distance must be finite and non-negative";
    Cancel.check_opt cancel;
    let source_topology = Geometry.topology geometry in
    let source = Topology.Private.view source_topology in
    let source_index = Topology_index.create ?cancel source_topology in
    let index = Topology_index.Private.view source_index in
    let point_count = source.point_count
    and source_edge_count = Topology_index.edge_count source_index in
    (match edges with
     | None -> ()
     | Some group ->
         if Edge_group.topology_data_id group <> Topology.data_id source_topology
            || Edge_group.length group <> source_edge_count then
           fail "PolyPath edge selection belongs to a different topology");
    let edge_selected edge = match edges with
      | None -> true
      | Some group -> Edge_group.mem edge group in
    let point_map = if connect_end_points then
        endpoint_point_map ?cancel ?edges
          ~only_end_points:connect_only_to_other_end_points
          ~maximum_distance geometry index
      else None in
    let map_point point = match point_map with
      | None -> point | Some mapping -> mapping.(point) in
    let source_groups = List.filter (fun group ->
        preserve_source_payload || Group.owner group = Group.Point)
        (Geometry.groups geometry)
    and source_edge_groups = Geometry.edge_groups geometry
    and source_attributes = List.filter (fun attribute ->
        preserve_source_payload || match Attribute.owner attribute with
          | Attribute.Point | Attribute.Detail -> true
          | Attribute.Vertex | Attribute.Primitive -> false)
        (Geometry.attributes geometry) in
    let has_primitive_groups = List.exists
        (fun group -> Group.owner group = Group.Primitive) source_groups
    and has_edge_groups = source_edge_groups <> [] in
    let needs_vertex_ancestry = List.exists
        (fun group -> Group.owner group = Group.Vertex) source_groups
      || List.exists (fun attribute -> Attribute.owner attribute = Attribute.Vertex)
          source_attributes in
    let needs_primitive_ancestry = has_primitive_groups || List.exists
        (fun attribute -> Attribute.owner attribute = Attribute.Primitive)
        source_attributes in

    (* Rewiring can collapse or duplicate formerly distinct source edges, so
       build the final unique graph directly rather than materializing an
       intermediate two-corner curve for every source edge. *)
    if source_edge_count > Sys.max_array_length / 2 then
      fail "PolyPath source edge count exceeds table limits";
    let non_self_count = ref 0 in
    for source_edge = 0 to source_edge_count - 1 do
      if edge_selected source_edge
          && index.edge_a.(source_edge) <> index.edge_b.(source_edge) then
        incr non_self_count
    done;
    let source_graph_identity = Option.is_none edges && Option.is_none point_map
        && !non_self_count = source_edge_count in
    let graph_count, graph_a, graph_b, source_of_graph, source_to_graph,
        graph_primitive =
      if source_graph_identity then begin
        let primitive = if needs_primitive_ancestry
          then Some (Array.make source_edge_count max_int) else None in
        Option.iter (fun values ->
          for edge = 0 to source_edge_count - 1 do
            for incidence = index.edge_offsets.(edge)
                to index.edge_offsets.(edge + 1) - 1 do
              let source_primitive = index.primitive_of_vertex.
                  (index.edge_vertices.(incidence)) in
              if source_primitive < values.(edge) then
                values.(edge) <- source_primitive
            done
          done) primitive;
        source_edge_count, index.edge_a, index.edge_b, None, None, primitive
      end else if Option.is_none point_map then begin
        let graph_count = !non_self_count in
        let graph_a = Array.make graph_count 0
        and graph_b = Array.make graph_count 0
        and source_of_graph = Array.make graph_count 0
        and source_to_graph = if has_primitive_groups || has_edge_groups
          then Some (Array.make source_edge_count (-1)) else None
        and primitive = if needs_primitive_ancestry
          then Some (Array.make graph_count max_int) else None in
        let graph = ref 0 in
        for source_edge = 0 to source_edge_count - 1 do
          let a = index.edge_a.(source_edge) and b = index.edge_b.(source_edge) in
          if edge_selected source_edge && a <> b then begin
            graph_a.(!graph) <- a; graph_b.(!graph) <- b;
            source_of_graph.(!graph) <- source_edge;
            Option.iter (fun mapping -> mapping.(source_edge) <- !graph)
              source_to_graph;
            Option.iter (fun values ->
              for incidence = index.edge_offsets.(source_edge)
                  to index.edge_offsets.(source_edge + 1) - 1 do
                let source_primitive = index.primitive_of_vertex.
                    (index.edge_vertices.(incidence)) in
                if source_primitive < values.(!graph) then
                  values.(!graph) <- source_primitive
              done) primitive;
            incr graph
          end
        done;
        graph_count, graph_a, graph_b, Some source_of_graph, source_to_graph,
        primitive
      end else begin
        let capacity = next_power_of_two (max 8 (!non_self_count * 2)) in
        let slots = Array.make capacity (-1) and mask = capacity - 1
        and graph_a = Array.make source_edge_count 0
        and graph_b = Array.make source_edge_count 0
        and source_of_graph = Array.make source_edge_count 0
        and source_to_graph = if has_primitive_groups || has_edge_groups
          then Some (Array.make source_edge_count (-1)) else None
        and primitive = if needs_primitive_ancestry
          then Some (Array.make source_edge_count max_int) else None in
        let graph_count = ref 0 in
        let find_or_add a b source_edge =
          let slot = ref (hash_pair a b land mask) in
          while slots.(!slot) >= 0
              && (let edge = slots.(!slot) in
                  graph_a.(edge) <> a || graph_b.(edge) <> b) do
            slot := (!slot + 1) land mask
          done;
          if slots.(!slot) >= 0 then slots.(!slot)
          else begin
            let edge = !graph_count in
            incr graph_count;
            slots.(!slot) <- edge;
            graph_a.(edge) <- a; graph_b.(edge) <- b;
            source_of_graph.(edge) <- source_edge;
            edge
          end in
        for source_edge = 0 to source_edge_count - 1 do
          if source_edge land 16_383 = 0 then Cancel.check_opt cancel;
          let mapped_a = map_point index.edge_a.(source_edge)
          and mapped_b = map_point index.edge_b.(source_edge) in
          if edge_selected source_edge && mapped_a <> mapped_b then begin
            let a, b = if mapped_a < mapped_b
              then mapped_a, mapped_b else mapped_b, mapped_a in
            let graph_edge = find_or_add a b source_edge in
            Option.iter (fun mapping -> mapping.(source_edge) <- graph_edge)
              source_to_graph;
            Option.iter (fun values ->
              for incidence = index.edge_offsets.(source_edge)
                  to index.edge_offsets.(source_edge + 1) - 1 do
                let source_primitive = index.primitive_of_vertex.
                    (index.edge_vertices.(incidence)) in
                if source_primitive < values.(graph_edge) then
                  values.(graph_edge) <- source_primitive
              done) primitive
          end
        done;
        !graph_count, graph_a, graph_b, Some source_of_graph, source_to_graph,
        primitive
      end in
    let source_edge_of_graph graph = match source_of_graph with
      | None -> graph | Some mapping -> mapping.(graph) in
    let graph_of_source_edge source_edge = match source_to_graph with
      | Some mapping -> mapping.(source_edge)
      | None -> if source_graph_identity then source_edge else -1 in
    let source_corner_at graph point =
      let source_edge = source_edge_of_graph graph in
      let corner = index.edge_vertices.(index.edge_offsets.(source_edge)) in
      let next = index.next_vertex.(corner) in
      if map_point source.vertex_points.(corner) = point then corner else next in
    let graph_primitive = Option.value ~default:[||] graph_primitive in

    let { path_count; edge_order; path_offsets; path_start;
          isolated_loop = path_isolated_loop_full } =
      trace_paths ~cancel ~point_count ~graph_count ~graph_a ~graph_b in
    let other edge point = if graph_a.(edge) = point
      then graph_b.(edge) else graph_a.(edge) in
    (* Ancestry follows the traced order: each edge knows its path and its
       position, and a path inherits the lowest source primitive it crosses. *)
    let graph_path = if has_primitive_groups
        then Array.make graph_count (-1) else [||]
    and graph_target_edge = if has_edge_groups
        then Array.make graph_count (-1) else [||]
    and path_primitive = if needs_primitive_ancestry
        then Array.make path_count max_int else [||] in
    if has_primitive_groups || has_edge_groups || needs_primitive_ancestry then
      for path = 0 to path_count - 1 do
        for at = path_offsets.(path) to path_offsets.(path + 1) - 1 do
          let edge = edge_order.(at) in
          if has_primitive_groups then graph_path.(edge) <- path;
          if has_edge_groups then graph_target_edge.(edge) <- at;
          if needs_primitive_ancestry
              && graph_primitive.(edge) < path_primitive.(path) then
            path_primitive.(path) <- graph_primitive.(edge)
        done
      done;
    let primitive_offsets = Array.make (path_count + 1) 0
    and primitive_kinds = Array.make path_count Topology.Open_polyline in
    for path = 0 to path_count - 1 do
      let edge_count = path_offsets.(path + 1) - path_offsets.(path) in
      let close = make_isolated_loops_closed
          && Bytes.get path_isolated_loop_full path <> '\000' in
      primitive_kinds.(path) <- if close then Topology.Polygon
        else Topology.Open_polyline;
      let size = edge_count + if close then 0 else 1 in
      if primitive_offsets.(path) > Sys.max_array_length - size then
        fail "PolyPath output cardinality exceeds OCaml array limits";
      primitive_offsets.(path + 1) <- primitive_offsets.(path) + size
    done;
    let output_vertex_count = primitive_offsets.(path_count) in
    let vertex_points = Array.make output_vertex_count 0
    and vertex_map = if needs_vertex_ancestry
        then Array.make output_vertex_count 0 else [||] in
    run_ranges ?grain path_count (fun first_path last_path ->
      Cancel.check_opt cancel;
      for path = first_path to last_path - 1 do
        let output = primitive_offsets.(path) and edge_first = path_offsets.(path)
        and edge_last = path_offsets.(path + 1) in
        if edge_last - edge_first = 1 then begin
          let edge = edge_order.(edge_first) and start = path_start.(path) in
          let finish = other edge start in
          vertex_points.(output) <- start;
          if needs_vertex_ancestry then
            vertex_map.(output) <- source_corner_at edge start;
          vertex_points.(output + 1) <- finish;
          if needs_vertex_ancestry then
            vertex_map.(output + 1) <- source_corner_at edge finish
        end else begin
          let current = ref path_start.(path) in
          for order = edge_first to edge_last - 1 do
            let edge = edge_order.(order)
            and target = output + order - edge_first in
            vertex_points.(target) <- !current;
            if needs_vertex_ancestry then
              vertex_map.(target) <- source_corner_at edge !current;
            current := other edge !current
          done;
          if primitive_kinds.(path) = Topology.Open_polyline then begin
            let target = primitive_offsets.(path + 1) - 1
            and edge = edge_order.(edge_last - 1) in
            vertex_points.(target) <- !current;
            if needs_vertex_ancestry then
              vertex_map.(target) <- source_corner_at edge !current
          end
        end
      done);
    let output_topology = Topology.create_owned ~point_count ~vertex_points
        ~primitive_offsets ~primitive_kinds |> Result.get_ok in

    let attributes = List.map (fun attribute ->
      match Attribute.owner attribute with
      | Attribute.Point | Attribute.Detail -> attribute
      | Attribute.Vertex -> remap_attribute ?cancel ?grain vertex_map attribute
      | Attribute.Primitive ->
          remap_attribute ?cancel ?grain path_primitive attribute)
        source_attributes in
    let groups = List.map (fun group -> match Group.owner group with
      | Group.Point -> group
      | Group.Vertex ->
          let target = Group.init ?grain ~owner:Group.Vertex
              ~name:(Group.name group) output_vertex_count (fun vertex ->
                Group.mem vertex_map.(vertex) group) in
          Group.Private.remap_order ~source:group
            ~source_of_target:vertex_map target
      | Group.Primitive ->
          let bits = Bytes.make ((path_count + 7) / 8) '\000' in
          for source_edge = 0 to source_edge_count - 1 do
            if source_edge land 16_383 = 0 then Cancel.check_opt cancel;
            let graph_edge = graph_of_source_edge source_edge in
            if graph_edge >= 0 then begin
              let member = ref false in
              for incidence = index.edge_offsets.(source_edge)
                  to index.edge_offsets.(source_edge + 1) - 1 do
                let primitive = index.primitive_of_vertex.
                    (index.edge_vertices.(incidence)) in
                if Group.mem primitive group then member := true
              done;
              if !member then set_bit bits graph_path.(graph_edge)
            end
          done;
          let target = Group.Private.of_owned_bits ~owner:Group.Primitive
              ~name:(Group.name group) ~length:path_count bits in
          Group.Private.remap_order ~source:group
            ~source_of_target:path_primitive target) source_groups in
    let edge_groups = List.map (fun source_group ->
      let bits = Bytes.make ((graph_count + 7) / 8) '\000' in
      for source_edge = 0 to source_edge_count - 1 do
        if source_edge land 16_383 = 0 then Cancel.check_opt cancel;
        let graph_edge = graph_of_source_edge source_edge in
        if graph_edge >= 0 && Edge_group.mem source_edge source_group then
          set_bit bits graph_target_edge.(graph_edge)
      done;
      Edge_group.Private.of_owned_bits ~topology:output_topology
        ~edge_count:graph_count ~name:(Edge_group.name source_group) bits)
      source_edge_groups in
    Geometry.create ~positions:(Geometry.positions geometry)
      ~topology:output_topology ~attributes ~groups ~edge_groups ()
  with Poly_path_error message -> Error message
