open Subdivide_refine

let shared_edge_endpoint index left right =
  let left_a = index.Topology_index.View.edge_a.(left)
  and left_b = index.edge_b.(left)
  and right_a = index.edge_a.(right)
  and right_b = index.edge_b.(right) in
  if left_a = right_a || left_a = right_b then left_a
  else if left_b = right_a || left_b = right_b then left_b
  else -1

let project_to_source_edge source_positions index edge x y z =
  let a = index.Topology_index.View.edge_a.(edge)
  and b = index.edge_b.(edge) in
  let ax = source_positions.Packed.Float3.View.x.(a)
  and ay = source_positions.y.(a) and az = source_positions.z.(a)
  and bx = source_positions.x.(b) and by = source_positions.y.(b)
  and bz = source_positions.z.(b) in
  let max3 a b c = max (abs_float a) (max (abs_float b) (abs_float c)) in
  let scale = max (max3 x y z) (max (max3 ax ay az) (max3 bx by bz)) in
  if scale = 0. then ax, ay, az
  else begin
    let dx = (bx /. scale) -. (ax /. scale)
    and dy = (by /. scale) -. (ay /. scale)
    and dz = (bz /. scale) -. (az /. scale)
    and rx = (x /. scale) -. (ax /. scale)
    and ry = (y /. scale) -. (ay /. scale)
    and rz = (z /. scale) -. (az /. scale) in
    let denominator = (dx *. dx) +. (dy *. dy) +. (dz *. dz) in
    let t = if denominator <= 0. || not (Float.is_finite denominator) then 0.
      else max 0. (min 1. (((rx *. dx) +. (ry *. dy) +. (rz *. dz))
        /. denominator)) in
    let one_minus_t = 1. -. t in
    (one_minus_t *. ax) +. (t *. bx),
    (one_minus_t *. ay) +. (t *. by),
    (one_minus_t *. az) +. (t *. bz)
  end

let pull_boundary_no_edge_division ?cancel ?grain plan geometry =
  let ancestry = match Geometry.find_attribute ~owner:Attribute.Vertex
      plan.ancestry_attribute geometry with
    | Some attribute ->
        (match Attribute.View.storage attribute with
         | Attribute.Int values -> values
         | _ -> fail "internal edge ancestry changed storage")
    | None -> fail "local subdivision dropped internal edge ancestry" in
  let topology = Topology.View.view (Geometry.topology geometry) in
  if Array.length ancestry <> Array.length topology.vertex_points then
    fail "internal edge ancestry cardinality mismatch";
  let source_index = Topology_index.View.view plan.source_index in
  let first_edge = Array.make topology.point_count (-1)
  and common_endpoint = Array.make topology.point_count (-1) in
  let add_constraint point edge =
    if edge < 0 || edge >= Array.length source_index.edge_a then
      fail "internal edge ancestry is out of range";
    let first = first_edge.(point) in
    if first < 0 then first_edge.(point) <- edge
    else if first <> edge then begin
      let common = common_endpoint.(point) in
      if common >= 0 then begin
        if source_index.edge_a.(edge) <> common
           && source_index.edge_b.(edge) <> common then
          fail "refined boundary point has incompatible source-edge ancestry"
      end else begin
        let shared = shared_edge_endpoint source_index first edge in
        if shared < 0 then
          fail "refined boundary point has incompatible source-edge ancestry";
        common_endpoint.(point) <- shared
      end
    end in
  for primitive = 0 to Bytes.length topology.primitive_kinds - 1 do
    Cancel.poll cancel primitive 4095;
    let first = topology.primitive_offsets.(primitive)
    and last = topology.primitive_offsets.(primitive + 1) in
    for vertex = first to last - 1 do
      let edge = ancestry.(vertex) in
      if edge >= 0 then begin
        let next = if vertex + 1 = last then first else vertex + 1 in
        add_constraint topology.vertex_points.(vertex) edge;
        add_constraint topology.vertex_points.(next) edge
      end
    done
  done;
  let source_positions = Packed.Float3.View.view plan.source_positions
  and positions = Packed.Float3.View.view (Geometry.positions geometry) in
  let x = Array.copy positions.x and y = Array.copy positions.y
  and z = Array.copy positions.z in
  run ?grain ?cancel topology.point_count (fun point ->
    let edge = first_edge.(point) in
    if edge >= 0 then begin
      let px, py, pz = if common_endpoint.(point) >= 0 then
          Packed.Float3.get plan.source_positions common_endpoint.(point)
        else project_to_source_edge source_positions source_index edge
            positions.x.(point) positions.y.(point) positions.z.(point) in
      if not (Float.is_finite px && Float.is_finite py && Float.is_finite pz) then
        fail "Pull Closed generated a non-finite boundary position";
      x.(point) <- px; y.(point) <- py; z.(point) <- pz
    end);
  let positions = Packed.Float3.View.of_owned_exn ~x ~y ~z in
  Geometry.with_positions positions geometry |> get_ok
  |> Geometry.without_attribute ~owner:Attribute.Vertex plan.ancestry_attribute

let stitch_boundary_no_edge_division ?cancel plan ~unselected
    ~free_points ~refined =
  let ancestry = match Geometry.find_attribute ~owner:Attribute.Vertex
      plan.ancestry_attribute refined with
    | Some attribute ->
        (match Attribute.View.storage attribute with
         | Attribute.Int values -> values
         | _ -> fail "internal edge ancestry changed storage")
    | None -> fail "local subdivision dropped internal edge ancestry" in
  let refined_topology = Topology.View.view (Geometry.topology refined) in
  if Array.length ancestry <> Array.length refined_topology.vertex_points then
    fail "internal edge ancestry cardinality mismatch";
  let source_index = Topology_index.View.view plan.source_index in
  let source_edges = Array.length source_index.edge_a in
  let counts = Array.make source_edges 0 in
  for primitive = 0 to Bytes.length refined_topology.primitive_kinds - 1 do
    Cancel.poll cancel primitive 4095;
    for vertex = refined_topology.primitive_offsets.(primitive)
        to refined_topology.primitive_offsets.(primitive + 1) - 1 do
      let edge = ancestry.(vertex) in
      if edge >= 0 then begin
        if edge >= source_edges then fail "internal edge ancestry is out of range";
        counts.(edge) <- checked_add "stitch descendant edge count" counts.(edge) 1
      end
    done
  done;
  let offsets = Array.make (source_edges + 1) 0 in
  for edge = 0 to source_edges - 1 do
    offsets.(edge + 1) <- checked_add "stitch descendant storage"
        offsets.(edge) counts.(edge)
  done;
  let descendant_count = offsets.(source_edges) in
  let starts = Array.make descendant_count 0
  and ends = Array.make descendant_count 0
  and start_vertices = Array.make descendant_count 0
  and end_vertices = Array.make descendant_count 0
  and cursor = Array.copy offsets in
  for primitive = 0 to Bytes.length refined_topology.primitive_kinds - 1 do
    let first = refined_topology.primitive_offsets.(primitive)
    and last = refined_topology.primitive_offsets.(primitive + 1) in
    for vertex = first to last - 1 do
      let edge = ancestry.(vertex) in
      if edge >= 0 then begin
        let next = if vertex + 1 = last then first else vertex + 1 in
        let at = cursor.(edge) in
        starts.(at) <- refined_topology.vertex_points.(vertex);
        ends.(at) <- refined_topology.vertex_points.(next);
        start_vertices.(at) <- vertex; end_vertices.(at) <- next;
        cursor.(edge) <- at + 1
      end
    done
  done;
  let added_primitives = ref 0 in
  for edge = 0 to source_edges - 1 do
    if counts.(edge) > 0 then
      added_primitives := checked_add "stitch primitive count"
          !added_primitives counts.(edge)
  done;
  let triangle_vertices = checked_mul "stitch vertex count" !added_primitives 3 in
  let triangle_points = Array.make triangle_vertices 0
  and vertex_sources = Array.make triangle_vertices 0
  and primitive_sources = Array.make !added_primitives 0 in
  let refined_point_offset = checked_add "stitch refined point offset"
      (Geometry.point_count unselected) free_points
  and refined_vertex_offset = Geometry.vertex_count unselected in
  let unselected_topology = Topology.View.view (Geometry.topology unselected) in
  let unselected_index_value = Topology_index.create ?cancel
      (Geometry.topology unselected) in
  let unselected_index = Topology_index.View.view unselected_index_value in
  let next_point = Array.make refined_topology.point_count (-1)
  and record_of_start = Array.make refined_topology.point_count (-1)
  and incoming = Bytes.make refined_topology.point_count '\000'
  and chain_points = Array.make (descendant_count + 1) 0
  and chain_vertices = Array.make (descendant_count + 1) 0 in
  let triangle_at = ref 0 and primitive_at = ref 0 in
  let emit primitive_source p0 v0 p1 v1 p2 v2 =
    let at = !triangle_at in
    triangle_points.(at) <- p0; triangle_points.(at + 1) <- p1;
    triangle_points.(at + 2) <- p2;
    vertex_sources.(at) <- v0; vertex_sources.(at + 1) <- v1;
    vertex_sources.(at + 2) <- v2;
    primitive_sources.(!primitive_at) <- primitive_source;
    triangle_at := at + 3; incr primitive_at in
  for edge = 0 to source_edges - 1 do
    let first = offsets.(edge) and last = offsets.(edge + 1) in
    let count = last - first in
    if count > 0 then begin
      Cancel.poll cancel edge 4095;
      for at = first to last - 1 do
        let start = starts.(at) and finish = ends.(at) in
        if next_point.(start) >= 0 || Bytes.get incoming finish <> '\000' then
          fail "stitch descendants do not form a simple directed chain";
        next_point.(start) <- finish;
        record_of_start.(start) <- at;
        Bytes.set incoming finish '\001'
      done;
      let chain_start = ref (-1) in
      for at = first to last - 1 do
        let point = starts.(at) in
        if Bytes.get incoming point = '\000' then begin
          if !chain_start >= 0 && !chain_start <> point then
            fail "stitch descendants contain multiple chains";
          chain_start := point
        end
      done;
      if !chain_start < 0 then fail "stitch descendants form a closed loop";
      let current = ref !chain_start in
      for local = 0 to count - 1 do
        let record = record_of_start.(!current) in
        if record < first || record >= last then
          fail "stitch descendant chain is disconnected";
        chain_points.(local) <- !current;
        chain_vertices.(local) <- start_vertices.(record);
        current := next_point.(!current);
        if local + 1 = count then begin
          chain_points.(local + 1) <- !current;
          chain_vertices.(local + 1) <- end_vertices.(record)
        end
      done;
      let source_a = plan.selected_edge_start.(edge) in
      if source_a < 0 then fail "stitch source edge has no selected orientation";
      let edge_a = source_index.edge_a.(edge) and edge_b = source_index.edge_b.(edge) in
      let source_b = if edge_a = source_a then edge_b
        else if edge_b = source_a then edge_a
        else fail "stitch source orientation is inconsistent" in
      let coarse_a = plan.unselected_point_of_source.(source_a)
      and coarse_b = plan.unselected_point_of_source.(source_b) in
      if coarse_a < 0 || coarse_b < 0 then
        fail "stitch coarse edge was removed from the unselected region";
      let coarse_edge = match Topology_index.find_edge unselected_index_value
          ~a:coarse_a ~b:coarse_b with
        | Some edge -> edge
        | None -> fail "stitch could not find the coarse boundary edge" in
      let coarse_directed = unselected_index.edge_vertices.(
          unselected_index.edge_offsets.(coarse_edge)) in
      let coarse_next = unselected_index.next_vertex.(coarse_directed) in
      if coarse_next < 0 then fail "stitch coarse edge has no complete corner";
      let coarse_a_vertex, coarse_b_vertex =
        if unselected_topology.vertex_points.(coarse_directed) = coarse_a
        then coarse_directed, coarse_next else coarse_next, coarse_directed in
      let source_primitive = plan.unselected_primitive_of_edge.(edge) in
      if source_primitive < 0 then fail "stitch edge has no unselected primitive";
      let primitive_source = plan.unselected_primitive_of_source.(source_primitive) in
      if primitive_source < 0 then fail "stitch primitive ancestry was removed";
      let split = count / 2 in
      let q point = refined_point_offset + chain_points.(point)
      and qv point = refined_vertex_offset + chain_vertices.(point) in
      for segment = 0 to split - 1 do
        emit primitive_source coarse_a coarse_a_vertex
          (q (segment + 1)) (qv (segment + 1))
          (q segment) (qv segment)
      done;
      for segment = split to count - 1 do
        emit primitive_source coarse_b coarse_b_vertex
          (q (segment + 1)) (qv (segment + 1))
          (q segment) (qv segment)
      done;
      for at = first to last - 1 do
        let start = starts.(at) and finish = ends.(at) in
        next_point.(start) <- -1; record_of_start.(start) <- -1;
        Bytes.set incoming finish '\000'
      done
    end
  done;
  if !triangle_at <> triangle_vertices || !primitive_at <> !added_primitives then
    fail "stitch output cardinality mismatch";
  { triangle_points; triangle_vertex_sources = vertex_sources;
    triangle_primitive_sources = primitive_sources }

let stitch_boundary_divide_edges ?cancel ~consistent_topology plan
    (chains : interface_chains)
    (divided : divided_boundary) ~free_points
    ~free_vertices refined =
  let source_edges = Array.length chains.edge_offsets - 1 in
  let coarse_positions = Packed.Float3.View.view
      (Geometry.positions divided.geometry)
  and refined_positions = Packed.Float3.View.view
      (Geometry.positions refined) in
  let coincident at =
    if consistent_topology then false else
    let coarse = divided.points.(at) and refined = chains.points.(at) in
    coarse_positions.x.(coarse) = refined_positions.x.(refined)
    && coarse_positions.y.(coarse) = refined_positions.y.(refined)
    && coarse_positions.z.(coarse) = refined_positions.z.(refined) in
  let added_primitives = ref 0 in
  for edge = 0 to source_edges - 1 do
    let chain_length = chains.edge_offsets.(edge + 1)
        - chains.edge_offsets.(edge) in
    if chain_length > 0 then begin
      let first = chains.edge_offsets.(edge)
      and last = chains.edge_offsets.(edge + 1) in
      for at = first to last - 2 do
        let triangles = 2 - (if coincident at then 1 else 0)
            - (if coincident (at + 1) then 1 else 0) in
        added_primitives := checked_add "divided stitch primitive count"
            !added_primitives triangles
      done
    end
  done;
  let triangle_vertices = checked_mul "divided stitch vertex count"
      !added_primitives 3 in
  let triangle_points = Array.make triangle_vertices 0
  and triangle_vertex_sources = Array.make triangle_vertices 0
  and triangle_primitive_sources = Array.make !added_primitives 0 in
  let refined_point_offset = checked_add "divided stitch refined point offset"
      (Geometry.point_count divided.geometry) free_points
  and refined_vertex_offset = checked_add "divided stitch refined vertex offset"
      (Geometry.vertex_count divided.geometry) free_vertices in
  let triangle_at = ref 0 and primitive_at = ref 0 in
  let emit primitive_source p0 v0 p1 v1 p2 v2 =
    let at = !triangle_at in
    triangle_points.(at) <- p0;
    triangle_points.(at + 1) <- p1;
    triangle_points.(at + 2) <- p2;
    triangle_vertex_sources.(at) <- v0;
    triangle_vertex_sources.(at + 1) <- v1;
    triangle_vertex_sources.(at + 2) <- v2;
    triangle_primitive_sources.(!primitive_at) <- primitive_source;
    triangle_at := at + 3;
    incr primitive_at in
  for edge = 0 to source_edges - 1 do
    let first = chains.edge_offsets.(edge)
    and last = chains.edge_offsets.(edge + 1) in
    if last > first then begin
      Cancel.poll cancel edge 4095;
      let source_primitive = plan.unselected_primitive_of_edge.(edge) in
      if source_primitive < 0 then fail "divided stitch edge has no coarse primitive";
      let primitive_source = divided.source_primitive_to_output.(source_primitive) in
      if primitive_source < 0 then
        fail "divided stitch coarse primitive ancestry was removed";
      for at = first to last - 2 do
        let coarse_a = divided.points.(at)
        and coarse_b = divided.points.(at + 1)
        and refined_a = refined_point_offset + chains.points.(at)
        and refined_b = refined_point_offset + chains.points.(at + 1)
        and coarse_a_vertex = divided.vertices.(at)
        and coarse_b_vertex = divided.vertices.(at + 1)
        and refined_a_vertex = refined_vertex_offset + chains.vertices.(at)
        and refined_b_vertex = refined_vertex_offset + chains.vertices.(at + 1) in
        if not (coincident at) then
          emit primitive_source coarse_a coarse_a_vertex
            refined_b refined_b_vertex refined_a refined_a_vertex;
        if not (coincident (at + 1)) then
          emit primitive_source coarse_a coarse_a_vertex
            coarse_b coarse_b_vertex refined_b refined_b_vertex
      done
    end
  done;
  if !triangle_at <> triangle_vertices || !primitive_at <> !added_primitives then
    fail "divided stitch output cardinality mismatch";
  { triangle_points; triangle_vertex_sources; triangle_primitive_sources }

let make_boundary_weld ?cancel ~bias ~only_equal
    (chains : interface_chains)
    (divided : divided_boundary)
    ~free_points ~total_points refined =
  let refined_point_offset = checked_add "Pull Divide refined point offset"
      (Geometry.point_count divided.geometry) free_points in
  if refined_point_offset + Geometry.point_count refined <> total_points then
    fail "boundary weld part cardinality is inconsistent";
  let representative = Array.init total_points Fun.id
  and removed = Bytes.make total_points '\000' in
  let coarse_positions = Packed.Float3.View.view
      (Geometry.positions divided.geometry)
  and refined_positions = Packed.Float3.View.view
      (Geometry.positions refined) in
  let coincident coarse refined =
    coarse_positions.x.(coarse) = refined_positions.x.(refined)
    && coarse_positions.y.(coarse) = refined_positions.y.(refined)
    && coarse_positions.z.(coarse) = refined_positions.z.(refined) in
  let source_edges = Array.length chains.edge_offsets - 1 in
  for edge = 0 to source_edges - 1 do
    Cancel.poll cancel edge 4095;
    let first = chains.edge_offsets.(edge)
    and last = chains.edge_offsets.(edge + 1) in
    for at = first to last - 1 do
      let coarse = divided.points.(at)
      and refined_local = chains.points.(at) in
      let refined = refined_point_offset + refined_local in
      if not only_equal || coincident coarse refined_local then begin
        let previous = representative.(refined) in
        if previous <> refined && previous <> coarse then
          fail "boundary weld maps one refined point to incompatible coarse points";
        representative.(refined) <- coarse;
        Bytes.set removed refined '\001'
      end
    done
  done;
  let output_of_old = Array.make total_points (-1)
  and output_points = ref 0 in
  for point = 0 to total_points - 1 do
    if Bytes.get removed point = '\000' then begin
      output_of_old.(point) <- !output_points;
      incr output_points
    end
  done;
  let point_map = Array.make total_points 0 in
  for point = 0 to total_points - 1 do
    let root = representative.(point) in
    let target = output_of_old.(root) in
    if target < 0 then fail "Pull Divide representative was unexpectedly removed";
    point_map.(point) <- target
  done;
  let output_source = Array.make !output_points 0 in
  for point = 0 to total_points - 1 do
    let output = output_of_old.(point) in
    if output >= 0 then output_source.(output) <- point
  done;
  let merge_counts = Array.make !output_points 0 in
  for point = 0 to total_points - 1 do
    if Bytes.get removed point <> '\000' then begin
      let output = point_map.(point) in
      merge_counts.(output) <- checked_add "Pull Divide merge incidence"
          merge_counts.(output) 1
    end
  done;
  let merge_offsets = Array.make (!output_points + 1) 0 in
  for point = 0 to !output_points - 1 do
    merge_offsets.(point + 1) <- checked_add "Pull Divide merge storage"
        merge_offsets.(point) merge_counts.(point)
  done;
  let merge_members = Array.make merge_offsets.(!output_points) 0
  and cursor = Array.copy merge_offsets in
  for point = 0 to total_points - 1 do
    if Bytes.get removed point <> '\000' then begin
      let output = point_map.(point) and at = cursor.(point_map.(point)) in
      merge_members.(at) <- point;
      cursor.(output) <- at + 1
    end
  done;
  { old_to_output = point_map; output_source; merge_offsets; merge_members; bias }

