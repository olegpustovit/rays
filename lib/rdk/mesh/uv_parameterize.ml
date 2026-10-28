
type boundary_mode = Circle | Preserve

exception Parameterize_error of string
let fail message = raise (Parameterize_error message)
let finite value = Float.is_finite value

let rec find_root parent value =
  let next = parent.(value) in
  if next = value then value
  else begin
    let root = find_root parent next in
    parent.(value) <- root;
    root
  end

let union_roots parent rank left right =
  let left = find_root parent left and right = find_root parent right in
  if left <> right then begin
    let left_rank = Char.code (Bytes.get rank left)
    and right_rank = Char.code (Bytes.get rank right) in
    if left_rank < right_rank then parent.(left) <- right
    else if right_rank < left_rank then parent.(right) <- left
    else begin
      let root, child = if left < right then left, right else right, left in
      parent.(child) <- root;
      Bytes.set rank root (Char.chr (left_rank + 1))
    end
  end

let run_ranges ~grain count operation =
  let range_count = (count + grain - 1) / grain in
  if range_count > 0 then
    Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(range_count - 1)
      (fun range ->
        let first = range * grain and last = min count ((range + 1) * grain) in
        operation range first last);
  range_count

let selected_legacy_seam group index_view edge =
  let first = index_view.Topology_index.View.edge_offsets.(edge)
  and last = index_view.edge_offsets.(edge + 1) in
  let selected = ref false in
  for local = first to last - 1 do
    if Group.mem index_view.edge_vertices.(local) group then selected := true
  done;
  !selected

let edge_endpoint_corners topology_view index_view edge local =
  let corner = index_view.Topology_index.View.edge_vertices.
      (index_view.edge_offsets.(edge) + local) in
  let next = index_view.next_vertex.(corner) in
  if next < 0 then fail "UV parameterization requires polygon edges";
  let a = topology_view.Topology.View.vertex_points.(corner)
  and b = topology_view.vertex_points.(next) in
  let edge_a = index_view.edge_a.(edge) in
  if a = edge_a then corner, next
  else if b = edge_a then next, corner
  else fail "UV parameterization encountered inconsistent reverse topology"

let edge_corner_variables topology_view index_view corner_to_variable edge local =
  let a, b = edge_endpoint_corners topology_view index_view edge local in
  corner_to_variable.(a), corner_to_variable.(b)

let solve ?cancel ?(grain = 16_384) ?(name = "uv") ?seams ?edge_seams
    ?(uv_tolerance = 1e-9) ?(iterations = 500) ?(tolerance = 1e-7)
    boundary_mode geometry =
  try
    if grain <= 0 then fail "grain must be positive";
    if String.trim name = "" then fail "UV attribute name must not be empty";
    if iterations <= 0 then fail "iterations must be positive";
    if not (finite tolerance) || tolerance < 0. then
      fail "solver tolerance must be finite and non-negative";
    if not (finite uv_tolerance) || uv_tolerance < 0. then
      fail "UV seam tolerance must be finite and non-negative";
    let topology = Geometry.topology geometry in
    if not (Topology.all_triangles topology) then
      fail "UV parameterization currently requires triangle polygons";
    let topology_view = Topology.View.view topology in
    let point_count = Topology.point_count topology
    and vertex_count = Topology.vertex_count topology
    and primitive_count = Topology.primitive_count topology in
    if primitive_count = 0 then fail "UV parameterization requires at least one triangle";
    Option.iter (fun group ->
      if Group.owner group <> Group.Vertex || Group.length group <> vertex_count then
        fail "UV seam group must own one outgoing-edge entry per vertex") seams;
    let index = Topology_index.create ?cancel topology in
    let index_view = Topology_index.View.view index in
    Option.iter (fun group ->
      if Edge_group.topology_data_id group <> Topology.data_id topology
          || Edge_group.length group <> Topology_index.edge_count index then
        fail "native UV seam group belongs to a different topology") edge_seams;
    Cancel.check_opt cancel;
    let positions = Packed.Float3.View.view (Geometry.positions geometry) in
    let invalid_position = Atomic.make false in
    run_ranges ~grain point_count (fun _ first last ->
      for point = first to last - 1 do
        if not (finite positions.x.(point) && finite positions.y.(point)
            && finite positions.z.(point)) then Atomic.set invalid_position true
      done) |> ignore;
    if Atomic.get invalid_position then
      fail "UV parameterization requires finite point positions";

    let edge_count = Topology_index.edge_count index in
    let edge_is_seam = Bytes.make edge_count '\000' in
    for edge = 0 to edge_count - 1 do
      if edge land 16_383 = 0 then Cancel.check_opt cancel;
      let incidence = Topology_index.edge_incidence_count index edge in
      if incidence > 2 then fail "UV parameterization rejects non-manifold edges";
      let selected = (match edge_seams with
        | Some group -> Edge_group.mem edge group
        | None -> false) || match seams with
        | Some group -> selected_legacy_seam group index_view edge
        | None -> false in
      if selected then Bytes.set edge_is_seam edge '\001'
    done;

    (* Corners sharing one point remain one UV variable exactly while their
       incident faces are connected across non-seam manifold edges. *)
    let corner_parent = Array.init vertex_count Fun.id
    and corner_rank = Bytes.make vertex_count '\000' in
    for edge = 0 to edge_count - 1 do
      if edge land 16_383 = 0 then Cancel.check_opt cancel;
      if Topology_index.edge_incidence_count index edge = 2
          && Bytes.get edge_is_seam edge = '\000' then begin
        let left_a, left_b = edge_endpoint_corners topology_view index_view edge 0
        and right_a, right_b = edge_endpoint_corners topology_view index_view edge 1 in
        union_roots corner_parent corner_rank left_a right_a;
        union_roots corner_parent corner_rank left_b right_b
      end
    done;
    let root_to_variable = Array.make vertex_count (-1)
    and corner_to_variable = Array.make vertex_count (-1) in
    let variable_count = ref 0 in
    for corner = 0 to vertex_count - 1 do
      let root = find_root corner_parent corner in
      if root_to_variable.(root) < 0 then begin
        root_to_variable.(root) <- !variable_count;
        incr variable_count
      end;
      corner_to_variable.(corner) <- root_to_variable.(root)
    done;
    let variable_count = !variable_count in
    let variable_point = Array.make variable_count (-1) in
    for corner = 0 to vertex_count - 1 do
      let variable = corner_to_variable.(corner)
      and point = topology_view.vertex_points.(corner) in
      if variable_point.(variable) < 0 then variable_point.(variable) <- point
      else if variable_point.(variable) <> point then
        fail "UV chart merged corners from distinct points"
    done;

    let component_parent = Array.init variable_count Fun.id
    and component_rank = Bytes.make variable_count '\000' in
    for primitive = 0 to primitive_count - 1 do
      let first = topology_view.primitive_offsets.(primitive) in
      let a = corner_to_variable.(first)
      and b = corner_to_variable.(first + 1)
      and c = corner_to_variable.(first + 2) in
      if a = b || b = c || c = a then
        fail "UV parameterization rejects triangles with repeated chart vertices";
      union_roots component_parent component_rank a b;
      union_roots component_parent component_rank b c
    done;
    let root_to_component = Array.make variable_count (-1)
    and variable_component = Array.make variable_count (-1) in
    let component_count = ref 0 in
    for variable = 0 to variable_count - 1 do
      let root = find_root component_parent variable in
      if root_to_component.(root) < 0 then begin
        root_to_component.(root) <- !component_count;
        incr component_count
      end;
      variable_component.(variable) <- root_to_component.(root)
    done;
    let component_count = !component_count in

    let boundary_degree = Bytes.make variable_count '\000'
    and boundary_first = Array.make variable_count (-1)
    and boundary_second = Array.make variable_count (-1) in
    let add_boundary a b =
      if a = b then fail "UV parameterization encountered a collapsed boundary edge";
      let add a b =
        let degree = Char.code (Bytes.get boundary_degree a) in
        if degree = 0 then boundary_first.(a) <- b
        else if degree = 1 && boundary_first.(a) <> b then boundary_second.(a) <- b
        else if degree >= 2 || boundary_first.(a) = b then
          fail "UV island boundary is not a simple cycle";
        Bytes.set boundary_degree a (Char.chr (degree + 1)) in
      add a b; add b a in
    for edge = 0 to edge_count - 1 do
      let incidence = Topology_index.edge_incidence_count index edge in
      if incidence = 1 then begin
        let a, b = edge_corner_variables topology_view index_view
            corner_to_variable edge 0 in
        add_boundary a b
      end else if incidence = 2 && Bytes.get edge_is_seam edge <> '\000' then
        for local = 0 to 1 do
          let a, b = edge_corner_variables topology_view index_view
              corner_to_variable edge local in
          add_boundary a b
        done
    done;
    for variable = 0 to variable_count - 1 do
      let degree = Char.code (Bytes.get boundary_degree variable) in
      if degree <> 0 && degree <> 2 then
        fail "UV island boundary is open or branched"
    done;

    let boundary_visited = Bytes.make variable_count '\000'
    and component_loop = Array.make component_count (-1)
    and loop_start = Array.make component_count (-1)
    and loop_size = Array.make component_count 0 in
    let loop_count = ref 0 in
    for start = 0 to variable_count - 1 do
      if Bytes.get boundary_degree start <> '\000'
          && Bytes.get boundary_visited start = '\000' then begin
        let component = variable_component.(start) in
        if component_loop.(component) >= 0 then
          fail "UV parameterization currently rejects islands with holes";
        component_loop.(component) <- !loop_count;
        loop_start.(component) <- start;
        let previous = ref (-1) and current = ref start and count = ref 0 in
        while !current <> start || !count = 0 do
          if !count > variable_count then
            fail "UV island boundary traversal did not close";
          if Bytes.get boundary_visited !current <> '\000' then
            fail "UV island boundary self-intersects topologically";
          Bytes.set boundary_visited !current '\001';
          incr count;
          let first = boundary_first.(!current)
          and second = boundary_second.(!current) in
          let next = if first <> !previous then first else second in
          if next < 0 then fail "UV island boundary is incomplete";
          previous := !current;
          current := next
        done;
        loop_size.(component) <- !count;
        incr loop_count
      end
    done;
    for component = 0 to component_count - 1 do
      if component_loop.(component) < 0 then
        fail "closed UV island requires at least one seam"
    done;

    let source_uv = match boundary_mode with
      | Circle -> None
      | Preserve ->
          (match Geometry.find_attribute ~owner:Attribute.Vertex name geometry with
           | None -> fail (Printf.sprintf
               "UV Relax requires vertex float2 attribute %S" name)
           | Some attribute ->
               match Attribute.View.storage attribute with
               | Attribute.Float2 values -> Some (Packed.Float2.View.view values)
               | _ -> fail (Printf.sprintf
                   "UV Relax vertex attribute %S must be float2" name)) in
    let u = Array.make variable_count 0. and v = Array.make variable_count 0. in
    (match source_uv with
     | None -> ()
     | Some source ->
         let assigned = Bytes.make variable_count '\000' in
         for corner = 0 to vertex_count - 1 do
           let source_u = source.Packed.Float2.View.x.(corner)
           and source_v = source.y.(corner) in
           if not (finite source_u && finite source_v) then
             fail "UV Relax requires finite input coordinates";
           let variable = corner_to_variable.(corner) in
           if Bytes.get assigned variable = '\000' then begin
             Bytes.set assigned variable '\001';
             u.(variable) <- source_u; v.(variable) <- source_v
           end else if abs_float (u.(variable) -. source_u) > uv_tolerance
               || abs_float (v.(variable) -. source_v) > uv_tolerance then
             fail "UV discontinuity must be declared as a seam before relaxing"
         done);

    let distance left right =
      let dx = positions.x.(left) -. positions.x.(right)
      and dy = positions.y.(left) -. positions.y.(right)
      and dz = positions.z.(left) -. positions.z.(right) in
      sqrt (dx *. dx +. dy *. dy +. dz *. dz) in
    (match boundary_mode with
     | Preserve -> ()
     | Circle ->
         let columns = int_of_float (ceil (sqrt (float_of_int component_count))) in
         let rows = (component_count + columns - 1) / columns in
         let cell_width = 1. /. float_of_int columns
         and cell_height = 1. /. float_of_int rows in
         let radius = 0.45 *. min cell_width cell_height in
         let component_center_u = Array.make component_count 0.
         and component_center_v = Array.make component_count 0. in
         for component = 0 to component_count - 1 do
           let start = loop_start.(component) in
           let perimeter = ref 0. and previous = ref (-1)
           and current = ref start and count = ref 0 in
           while !current <> start || !count = 0 do
             let next = if boundary_first.(!current) <> !previous
               then boundary_first.(!current) else boundary_second.(!current) in
             perimeter := !perimeter +. distance variable_point.(!current)
                 variable_point.(next);
             previous := !current; current := next; incr count
           done;
           if not (finite !perimeter) || !perimeter <= 0. then
             fail "UV island boundary has zero or non-finite length";
           let column = component mod columns and row = component / columns in
           let center_u = (float_of_int column +. 0.5) *. cell_width
           and center_v = (float_of_int row +. 0.5) *. cell_height in
           component_center_u.(component) <- center_u;
           component_center_v.(component) <- center_v;
           let traversed = ref 0. and previous = ref (-1)
           and current = ref start and count = ref 0 in
           while !current <> start || !count = 0 do
             let angle = 2. *. Float.pi *. !traversed /. !perimeter in
             u.(!current) <- center_u +. radius *. cos angle;
             v.(!current) <- center_v +. radius *. sin angle;
             let next = if boundary_first.(!current) <> !previous
               then boundary_first.(!current) else boundary_second.(!current) in
             traversed := !traversed +. distance variable_point.(!current)
                 variable_point.(next);
             previous := !current; current := next; incr count
           done
         done;
         for variable = 0 to variable_count - 1 do
           if Bytes.get boundary_degree variable = '\000' then begin
             let component = variable_component.(variable) in
             u.(variable) <- component_center_u.(component);
             v.(variable) <- component_center_v.(component)
           end
         done);

    (* Each triangle contributes two positive mean-value weights at every
       corner. Duplicate neighbor slots are intentional and avoid a hash/sort
       aggregation pass. *)
    let adjacency_offsets = Array.make (variable_count + 1) 0 in
    for corner = 0 to vertex_count - 1 do
      adjacency_offsets.(corner_to_variable.(corner) + 1) <-
        adjacency_offsets.(corner_to_variable.(corner) + 1) + 2
    done;
    for variable = 0 to variable_count - 1 do
      adjacency_offsets.(variable + 1) <- adjacency_offsets.(variable + 1)
        + adjacency_offsets.(variable)
    done;
    let adjacency_count = adjacency_offsets.(variable_count) in
    let adjacency = Array.make adjacency_count 0
    and weights = Array.make adjacency_count 0.
    and cursor = Array.copy adjacency_offsets
    and corner_tangent = Array.make vertex_count 0. in
    let invalid_triangle = Atomic.make false in
    run_ranges ~grain primitive_count (fun _ first last ->
      for primitive = first to last - 1 do
        let first_corner = topology_view.primitive_offsets.(primitive) in
        for local = 0 to 2 do
          let corner = first_corner + local
          and previous = first_corner + ((local + 2) mod 3)
          and next = first_corner + ((local + 1) mod 3) in
          let point = topology_view.vertex_points.(corner)
          and previous_point = topology_view.vertex_points.(previous)
          and next_point = topology_view.vertex_points.(next) in
          let ax = positions.x.(previous_point) -. positions.x.(point)
          and ay = positions.y.(previous_point) -. positions.y.(point)
          and az = positions.z.(previous_point) -. positions.z.(point)
          and bx = positions.x.(next_point) -. positions.x.(point)
          and by = positions.y.(next_point) -. positions.y.(point)
          and bz = positions.z.(next_point) -. positions.z.(point) in
          let aa = ax *. ax +. ay *. ay +. az *. az
          and bb = bx *. bx +. by *. by +. bz *. bz
          and dot = ax *. bx +. ay *. by +. az *. bz in
          let cross_x = ay *. bz -. az *. by
          and cross_y = az *. bx -. ax *. bz
          and cross_z = ax *. by -. ay *. bx in
          let cross = sqrt (cross_x *. cross_x +. cross_y *. cross_y
              +. cross_z *. cross_z) in
          let denominator = sqrt (aa *. bb) +. dot in
          if aa <= 0. || bb <= 0. || cross <= 1e-15 *. sqrt (aa *. bb)
              || denominator <= 0. then Atomic.set invalid_triangle true
          else corner_tangent.(corner) <- cross /. denominator
        done
      done) |> ignore;
    if Atomic.get invalid_triangle then
      fail "UV parameterization rejects zero-area or numerically degenerate triangles";
    for primitive = 0 to primitive_count - 1 do
      if primitive land 4095 = 0 then Cancel.check_opt cancel;
      let first = topology_view.primitive_offsets.(primitive) in
      for local = 0 to 2 do
        let corner = first + local
        and next = first + ((local + 1) mod 3)
        and previous = first + ((local + 2) mod 3) in
        let variable = corner_to_variable.(corner)
        and point = topology_view.vertex_points.(corner) in
        let install neighbor_corner =
          let slot = cursor.(variable) in
          cursor.(variable) <- slot + 1;
          let neighbor = corner_to_variable.(neighbor_corner) in
          let edge_length = distance point
              topology_view.vertex_points.(neighbor_corner) in
          let weight = corner_tangent.(corner) /. edge_length in
          if not (finite weight) || weight <= 0. then
            fail "UV parameterization produced an invalid mean-value weight";
          adjacency.(slot) <- neighbor;
          weights.(slot) <- weight in
        install previous;
        install next
      done
    done;

    let range_count = (variable_count + grain - 1) / grain in
    let range_sum = Array.make range_count 0. in
    let parallel_dot left right =
      Array.fill range_sum 0 range_count 0.;
      run_ranges ~grain variable_count (fun range first last ->
        let sum = ref 0. in
        for variable = first to last - 1 do
          sum := !sum +. left.(variable) *. right.(variable)
        done;
        range_sum.(range) <- !sum) |> ignore;
      let sum = ref 0. in
      Array.iter (fun value -> sum := !sum +. value) range_sum;
      !sum in
    let diagonal = Array.make variable_count 1. in
    run_ranges ~grain variable_count (fun _ first last ->
      for variable = first to last - 1 do
        if Bytes.get boundary_degree variable = '\000' then begin
          let sum = ref 0. in
          for slot = adjacency_offsets.(variable) to
              adjacency_offsets.(variable + 1) - 1 do
            sum := !sum +. weights.(slot)
          done;
          if !sum <= 0. || not (finite !sum) then
            fail "UV parameterization encountered an isolated chart vertex";
          diagonal.(variable) <- !sum
        end
      done) |> ignore;
    let apply source output =
      run_ranges ~grain variable_count (fun _ first last ->
        for variable = first to last - 1 do
          if Bytes.get boundary_degree variable <> '\000' then
            output.(variable) <- source.(variable)
          else begin
            let value = ref (diagonal.(variable) *. source.(variable)) in
            for slot = adjacency_offsets.(variable) to
                adjacency_offsets.(variable + 1) - 1 do
              let neighbor = adjacency.(slot) in
              if Bytes.get boundary_degree neighbor = '\000' then
                value := !value -. weights.(slot) *. source.(neighbor)
            done;
            output.(variable) <- !value
          end
        done) |> ignore in
    let solve_coordinate fixed =
      let solution = Array.copy fixed
      and rhs = Array.make variable_count 0.
      and residual = Array.make variable_count 0.
      and direction = Array.make variable_count 0.
      and preconditioned = Array.make variable_count 0.
      and applied = Array.make variable_count 0. in
      run_ranges ~grain variable_count (fun _ first last ->
        for variable = first to last - 1 do
          if Bytes.get boundary_degree variable <> '\000' then
            solution.(variable) <- 0.
          else begin
            let value = ref 0. in
            for slot = adjacency_offsets.(variable) to
                adjacency_offsets.(variable + 1) - 1 do
              let neighbor = adjacency.(slot) in
              if Bytes.get boundary_degree neighbor <> '\000' then
                value := !value +. weights.(slot) *. fixed.(neighbor)
            done;
            rhs.(variable) <- !value
          end
        done) |> ignore;
      apply solution applied;
      run_ranges ~grain variable_count (fun _ first last ->
        for variable = first to last - 1 do
          let value = rhs.(variable) -. applied.(variable) in
          residual.(variable) <- value;
          preconditioned.(variable) <- value /. diagonal.(variable);
          direction.(variable) <- preconditioned.(variable)
        done) |> ignore;
      let rhs_norm = sqrt (parallel_dot rhs rhs) in
      let threshold = tolerance *. max 1. rhs_norm in
      let residual_norm = ref (sqrt (parallel_dot residual residual)) in
      let rz = ref (parallel_dot residual preconditioned)
      and iteration = ref 0 in
      while !iteration < iterations && !residual_norm > threshold do
        Cancel.check_opt cancel;
        apply direction applied;
        let denominator = parallel_dot direction applied in
        if not (finite denominator) || denominator <= 0. then
          fail "UV parameterization solver lost positive definiteness";
        let alpha = !rz /. denominator in
        run_ranges ~grain variable_count (fun _ first last ->
          for variable = first to last - 1 do
            solution.(variable) <- solution.(variable)
              +. alpha *. direction.(variable);
            residual.(variable) <- residual.(variable)
              -. alpha *. applied.(variable);
            preconditioned.(variable) <- residual.(variable)
              /. diagonal.(variable)
          done) |> ignore;
        residual_norm := sqrt (parallel_dot residual residual);
        if !residual_norm > threshold then begin
          let next_rz = parallel_dot residual preconditioned in
          let beta = next_rz /. !rz in
          run_ranges ~grain variable_count (fun _ first last ->
            for variable = first to last - 1 do
              direction.(variable) <- preconditioned.(variable)
                +. beta *. direction.(variable)
            done) |> ignore;
          rz := next_rz
        end;
        incr iteration
      done;
      if !residual_norm > threshold then
        fail (Printf.sprintf
          "UV parameterization did not converge in %d iterations" iterations);
      for variable = 0 to variable_count - 1 do
        if Bytes.get boundary_degree variable <> '\000' then
          solution.(variable) <- fixed.(variable)
      done;
      solution in
    let solved_u = solve_coordinate u and solved_v = solve_coordinate v in
    let component_winding = Array.make component_count 0. in
    for primitive = 0 to primitive_count - 1 do
      let first = topology_view.primitive_offsets.(primitive) in
      let a = corner_to_variable.(first)
      and b = corner_to_variable.(first + 1)
      and c = corner_to_variable.(first + 2) in
      let area = (solved_u.(b) -. solved_u.(a))
            *. (solved_v.(c) -. solved_v.(a))
          -. (solved_v.(b) -. solved_v.(a))
            *. (solved_u.(c) -. solved_u.(a)) in
      if not (finite area) || abs_float area <= 1e-15 then
        fail "UV parameterization collapsed a triangle";
      let component = variable_component.(a)
      and sign = if area < 0. then -1. else 1. in
      if component_winding.(component) = 0. then component_winding.(component) <- sign
      else if component_winding.(component) <> sign then
        fail "UV parameterization produced a flipped triangle"
    done;
    let result_u = Array.make vertex_count 0.
    and result_v = Array.make vertex_count 0. in
    run_ranges ~grain vertex_count (fun _ first last ->
      for corner = first to last - 1 do
        let variable = corner_to_variable.(corner) in
        result_u.(corner) <- solved_u.(variable);
        result_v.(corner) <- solved_v.(variable)
      done) |> ignore;
    let values = Packed.Float2.of_owned ~x:result_u ~y:result_v
        |> Result.get_ok in
    let attribute = Attribute.create_key_owned
        (Attribute.key ~name ~owner:Attribute.Vertex Attribute.float2) values
        |> Result.get_ok in
    Geometry.with_attribute attribute geometry
  with Parameterize_error message -> Error message
