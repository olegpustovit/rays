
type mode =
  | Fill_single_polygon
  | Fill_triangles
  | Fill_triangle_fan

let ( let* ) = Result.bind

type boundary_plan = {
  source_index : Topology_index.t;
  source_index_view : Topology_index.View.view;
  loop_offsets : int array;
  loop_vertices : int array;
  loop_points : int array;
}

let operation = "Rdk.Poly_fill.poly_fill"

let checked_length label value =
  if Int64.compare value 0L < 0
      || Int64.compare value (Int64.of_int Sys.max_array_length) > 0 then
    Error (Printf.sprintf "%s: %s cardinality exceeds OCaml array limits"
      operation label)
  else Ok (Int64.to_int value)

let find_root parent value =
  let root = ref value in
  while parent.(!root) <> !root do root := parent.(!root) done;
  let at = ref value in
  while parent.(!at) <> !root do
    let next = parent.(!at) in
    parent.(!at) <- !root;
    at := next
  done;
  !root

let union_min parent left right =
  let left = find_root parent left and right = find_root parent right in
  if left <> right then
    if left < right then parent.(right) <- left else parent.(left) <- right

let plan_boundaries ?cancel ~grain ?boundary geometry =
  let topology = Geometry.topology geometry in
  let source = Topology.View.view topology in
  let index = Topology_index.create ?cancel topology in
  let reverse = Topology_index.View.view index in
  let edge_count = Topology_index.edge_count index in
  let* () = match boundary with
    | None -> Ok ()
    | Some group when Edge_group.topology_data_id group <> Topology.data_id topology ->
        Error (operation ^ ": boundary edge group belongs to another topology")
    | Some group when Edge_group.length group <> edge_count ->
        Error (operation ^ ": boundary edge group length does not match topology")
    | Some _ -> Ok () in
  let candidates = Bytes.make edge_count '\000'
  and parent = Array.make edge_count (-1)
  and first_at_point = Array.make source.point_count (-1)
  and outgoing = Array.make source.point_count (-1)
  and incoming = Array.make source.point_count (-1)
  and point_bad = Bytes.make source.point_count '\000' in
  for edge = 0 to edge_count - 1 do
    if edge land 16_383 = 0 then Cancel.check_opt cancel;
    if reverse.edge_offsets.(edge + 1) - reverse.edge_offsets.(edge) = 1 then begin
      let vertex = reverse.edge_vertices.(reverse.edge_offsets.(edge)) in
      let primitive = reverse.primitive_of_vertex.(vertex) in
      if Bytes.get source.primitive_kinds primitive = '\000' then begin
        Bytes.set candidates edge '\001';
        parent.(edge) <- edge
      end
    end
  done;
  let invalid_selected = ref (-1) in
  (match boundary with
   | None -> ()
   | Some group ->
       Edge_group.iter (fun edge ->
         if !invalid_selected < 0 && Bytes.get candidates edge = '\000' then
           invalid_selected := edge) group);
  if !invalid_selected >= 0 then Error (Printf.sprintf
      "%s: selected edge %d is not a one-sided polygon boundary edge"
      operation !invalid_selected)
  else begin
    for edge = 0 to edge_count - 1 do
      if edge land 16_383 = 0 then Cancel.check_opt cancel;
      if Bytes.get candidates edge <> '\000' then begin
        let a = reverse.edge_a.(edge) and b = reverse.edge_b.(edge) in
        let attach point =
          let first = first_at_point.(point) in
          if first < 0 then first_at_point.(point) <- edge
          else union_min parent first edge in
        attach a;
        attach b
      end
    done;
    for edge = 0 to edge_count - 1 do
      if parent.(edge) >= 0 then parent.(edge) <- find_root parent edge
    done;
    for edge = 0 to edge_count - 1 do
      if edge land 16_383 = 0 then Cancel.check_opt cancel;
      if Bytes.get candidates edge <> '\000' then begin
        let vertex = reverse.edge_vertices.(reverse.edge_offsets.(edge)) in
        let next = reverse.next_vertex.(vertex) in
        let a = source.vertex_points.(vertex)
        and b = source.vertex_points.(next) in
        if a = b then begin
          Bytes.set point_bad a '\001'
        end;
        if outgoing.(a) >= 0 then Bytes.set point_bad a '\001'
        else outgoing.(a) <- edge;
        if incoming.(b) >= 0 then Bytes.set point_bad b '\001'
        else incoming.(b) <- edge
      end
    done;
    let component_bad = Bytes.make edge_count '\000'
    and component_size = Array.make edge_count 0
    and component_selected = Bytes.make edge_count '\000' in
    for edge = 0 to edge_count - 1 do
      if parent.(edge) >= 0 then begin
        let root = parent.(edge) in
        component_size.(root) <- component_size.(root) + 1;
        match boundary with
        | None -> Bytes.set component_selected root '\001'
        | Some group when Edge_group.mem edge group ->
            Bytes.set component_selected root '\001'
        | Some _ -> ()
      end
    done;
    for point = 0 to source.point_count - 1 do
      if point land 16_383 = 0 then Cancel.check_opt cancel;
      let edge = first_at_point.(point) in
      if edge >= 0 then begin
        let root = find_root parent edge in
        if outgoing.(point) < 0 || incoming.(point) < 0
            || Bytes.get point_bad point <> '\000' then
          Bytes.set component_bad root '\001'
      end
    done;
    let invalid_root = ref (-1) and loop_count = ref 0
    and boundary_count = ref 0 in
    for edge = 0 to edge_count - 1 do
      if parent.(edge) = edge
          && Bytes.get component_selected edge <> '\000' then begin
        if Bytes.get component_bad edge <> '\000' && !invalid_root < 0 then
          invalid_root := edge;
        incr loop_count;
        boundary_count := !boundary_count + component_size.(edge)
      end
    done;
    if !invalid_root >= 0 then Error (Printf.sprintf
        "%s: boundary component beginning at edge %d is branched, open, or inconsistently wound"
        operation !invalid_root)
    else begin
      let loop_offsets = Array.make (!loop_count + 1) 0
      and loop_roots = Array.make !loop_count 0 in
      let loop = ref 0 in
      for edge = 0 to edge_count - 1 do
        if parent.(edge) = edge
            && Bytes.get component_selected edge <> '\000' then begin
          loop_roots.(!loop) <- edge;
          loop_offsets.(!loop + 1) <- loop_offsets.(!loop) + component_size.(edge);
          incr loop
        end
      done;
      let loop_vertices = Array.make !boundary_count 0
      and loop_points = Array.make !boundary_count 0
      and traversal_bad = Bytes.make !loop_count '\000' in
      if !loop_count > 0 then Parallel.for_
          ~chunk_size:(max 1 (grain / 16)) ~start:0 ~finish:(!loop_count - 1)
          (fun loop ->
            let first = loop_offsets.(loop)
            and last = loop_offsets.(loop + 1)
            and start_edge = loop_roots.(loop) in
            let current = ref start_edge in
            for at = first to last - 1 do
              if at land 16_383 = 0 then Cancel.check_opt cancel;
              let edge = !current in
              let vertex = reverse.edge_vertices.(reverse.edge_offsets.(edge)) in
              let next = reverse.next_vertex.(vertex) in
              let point = source.vertex_points.(vertex)
              and next_point = source.vertex_points.(next) in
              loop_vertices.(at) <- vertex;
              loop_points.(at) <- point;
              current := outgoing.(next_point)
            done;
            if !current <> start_edge then Bytes.set traversal_bad loop '\001');
      let failed = ref (-1) in
      for loop = 0 to !loop_count - 1 do
        if Bytes.get traversal_bad loop <> '\000' && !failed < 0 then failed := loop
      done;
      if !failed >= 0 then Error (Printf.sprintf
          "%s: boundary loop %d did not close exactly" operation !failed)
      else Ok { source_index = index; source_index_view = reverse;
        loop_offsets; loop_vertices; loop_points }
    end
  end

let safe_component_mean source indices first last =
  let scale = ref 0. and finite = ref true in
  for at = first to last - 1 do
    let value = source.(indices.(at)) in
    if not (Float.is_finite value) then finite := false
    else if abs_float value > !scale then scale := abs_float value
  done;
  if not !finite then begin
    let sum = ref 0. in
    for at = first to last - 1 do sum := !sum +. source.(indices.(at)) done;
    !sum /. float_of_int (last - first)
  end else if !scale = 0. then 0.
  else begin
    let sum = ref 0. in
    for at = first to last - 1 do
      sum := !sum +. (source.(indices.(at)) /. !scale)
    done;
    !scale *. (!sum /. float_of_int (last - first))
  end

let analyze_positions ?cancel ~grain plan positions =
  let loop_count = Array.length plan.loop_offsets - 1 in
  let center_x = Array.make loop_count 0.
  and center_y = Array.make loop_count 0.
  and center_z = Array.make loop_count 0.
  and failures = Bytes.make loop_count '\000' in
  if loop_count > 0 then Parallel.for_ ~chunk_size:(max 1 (grain / 64))
      ~start:0 ~finish:(loop_count - 1) (fun loop ->
        Cancel.check_opt cancel;
        let first = plan.loop_offsets.(loop)
        and last = plan.loop_offsets.(loop + 1) in
        let valid = ref true in
        for at = first to last - 1 do
          let point = plan.loop_points.(at) in
          if not (Float.is_finite positions.Packed.Float3.View.x.(point)
              && Float.is_finite positions.y.(point)
              && Float.is_finite positions.z.(point)) then valid := false
        done;
        if not !valid then Bytes.set failures loop '\001'
        else begin
          center_x.(loop) <- safe_component_mean positions.x plan.loop_points first last;
          center_y.(loop) <- safe_component_mean positions.y plan.loop_points first last;
          center_z.(loop) <- safe_component_mean positions.z plan.loop_points first last
        end);
  let failed = ref (-1) in
  for loop = 0 to loop_count - 1 do
    if Bytes.get failures loop <> '\000' && !failed < 0 then failed := loop
  done;
  if !failed >= 0 then Error (Printf.sprintf
      "%s: boundary loop %d contains a non-finite position" operation !failed)
  else Ok (center_x, center_y, center_z)

let triangulate_loops ?cancel ~grain plan positions =
  let loop_count = Array.length plan.loop_offsets - 1 in
  let boundary_count = Array.length plan.loop_points in
  let triangle_count = boundary_count - (2 * loop_count) in
  let triangle_local = Array.make (triangle_count * 3) 0
  and u = Array.make boundary_count 0.
  and v = Array.make boundary_count 0.
  and remaining = Array.make boundary_count 0
  and failures = Bytes.make loop_count '\000' in
  if loop_count > 0 then Parallel.for_ ~chunk_size:(max 1 (grain / 64))
      ~start:0 ~finish:(loop_count - 1) (fun loop ->
        Cancel.check_opt cancel;
        let first = plan.loop_offsets.(loop)
        and last = plan.loop_offsets.(loop + 1) in
        let size = last - first and coordinate_scale = ref 0. in
        for at = first to last - 1 do
          let point = plan.loop_points.(at) in
          coordinate_scale := Float.max !coordinate_scale
              (abs_float positions.Packed.Float3.View.x.(point));
          coordinate_scale := Float.max !coordinate_scale
              (abs_float positions.y.(point));
          coordinate_scale := Float.max !coordinate_scale
              (abs_float positions.z.(point))
        done;
        if !coordinate_scale = 0. then Bytes.set failures loop '\002'
        else begin
          let nx = ref 0. and ny = ref 0. and nz = ref 0. in
          for local = 0 to size - 1 do
            let next = (local + 1) mod size in
            let a = plan.loop_points.(first + local)
            and b = plan.loop_points.(first + next) in
            let ax = positions.x.(a) /. !coordinate_scale
            and ay = positions.y.(a) /. !coordinate_scale
            and az = positions.z.(a) /. !coordinate_scale
            and bx = positions.x.(b) /. !coordinate_scale
            and by = positions.y.(b) /. !coordinate_scale
            and bz = positions.z.(b) /. !coordinate_scale in
            nx := !nx +. ((ay -. by) *. (az +. bz));
            ny := !ny +. ((az -. bz) *. (ax +. bx));
            nz := !nz +. ((ax -. bx) *. (ay +. by))
          done;
          let ax = abs_float !nx and ay = abs_float !ny and az = abs_float !nz in
          let dominant = if ax >= ay && ax >= az then 0 else if ay >= az then 1 else 2 in
          let umin = ref Float.infinity and umax = ref Float.neg_infinity
          and vmin = ref Float.infinity and vmax = ref Float.neg_infinity in
          for local = 0 to size - 1 do
            let point = plan.loop_points.(first + local) in
            let x = positions.x.(point) /. !coordinate_scale
            and y = positions.y.(point) /. !coordinate_scale
            and z = positions.z.(point) /. !coordinate_scale in
            let pu, pv = if dominant = 0 then y, z
              else if dominant = 1 then x, z else x, y in
            u.(first + local) <- pu;
            v.(first + local) <- pv;
            umin := Float.min !umin pu;
            umax := Float.max !umax pu;
            vmin := Float.min !vmin pv;
            vmax := Float.max !vmax pv
          done;
          let projection_scale = Float.max (!umax -. !umin) (!vmax -. !vmin) in
          if projection_scale = 0. || not (Float.is_finite projection_scale) then
            Bytes.set failures loop '\002'
          else begin
            for local = 0 to size - 1 do
              u.(first + local) <- (u.(first + local) -. !umin) /. projection_scale;
              v.(first + local) <- (v.(first + local) -. !vmin) /. projection_scale;
              remaining.(first + local) <- local
            done;
            let area = ref 0. in
            for local = 0 to size - 1 do
              let next = (local + 1) mod size in
              area := !area +. (u.(first + local) *. v.(first + next)
                -. u.(first + next) *. v.(first + local))
            done;
            if not (Float.is_finite !area) || abs_float !area <= 1e-14 then
              Bytes.set failures loop '\002'
            else begin
              let orientation = if !area > 0. then 1. else -1. in
              let triangle_base = (first - (2 * loop)) * 3 in
              let active = ref size and emitted = ref 0 and failed = ref false in
              while !active > 3 && not !failed do
                Cancel.check_opt cancel;
                let ear = Polygon_triangulation.find_projected_ear ~u ~v
                    ~base:first ~orientation ~remaining ~active:!active in
                if ear < 0 then failed := true
                else begin
                  let a = remaining.(first + ((ear + !active - 1) mod !active))
                  and b = remaining.(first + ear)
                  and c = remaining.(first + ((ear + 1) mod !active)) in
                  let output = triangle_base + (!emitted * 3) in
                  triangle_local.(output) <- a;
                  triangle_local.(output + 1) <- b;
                  triangle_local.(output + 2) <- c;
                  Array.blit remaining (first + ear + 1) remaining
                    (first + ear) (!active - ear - 1);
                  decr active;
                  incr emitted
                end
              done;
              if !failed then Bytes.set failures loop '\003'
              else begin
                let output = triangle_base + (!emitted * 3) in
                triangle_local.(output) <- remaining.(first);
                triangle_local.(output + 1) <- remaining.(first + 1);
                triangle_local.(output + 2) <- remaining.(first + 2)
              end
            end
          end
        end);
  let failed = ref (-1) and code = ref '\000' in
  for loop = 0 to loop_count - 1 do
    if !failed < 0 && Bytes.get failures loop <> '\000' then begin
      failed := loop;
      code := Bytes.get failures loop
    end
  done;
  if !failed < 0 then Ok triangle_local
  else if !code = '\002' then Error (Printf.sprintf
      "%s: boundary loop %d has degenerate projected area" operation !failed)
  else Error (Printf.sprintf
      "%s: boundary loop %d is non-simple or cannot be triangulated"
      operation !failed)

let map_array ?cancel ~grain mapping source =
  let count = Array.length mapping in
  if count = 0 then [||]
  else Parallel.init_array ~grain count (fun index ->
    if index land 16_383 = 0 then Cancel.check_opt cancel;
    source.(mapping.(index)))

type center_map =
  | No_centers
  | Point_centers of int
  | Vertex_centers of { vertex_points : int array; first_point : int }

let center_loop centers index = match centers with
  | No_centers -> -1
  | Point_centers first -> if index < first then -1 else index - first
  | Vertex_centers { vertex_points; first_point } ->
      let point = vertex_points.(index) in
      if point < first_point then -1 else point - first_point

let map_float ?cancel ~grain ~loop_offsets ~loop_sources mapping centers source =
  let output = map_array ?cancel ~grain mapping source in
  if centers <> No_centers then begin
    let loop_count = Array.length loop_offsets - 1 in
    let means = Parallel.init_array ~grain loop_count (fun loop ->
      safe_component_mean source loop_sources loop_offsets.(loop)
        loop_offsets.(loop + 1)) in
    if Array.length output > 0 then Parallel.for_ ~chunk_size:grain ~start:0
        ~finish:(Array.length output - 1) (fun index ->
          let loop = center_loop centers index in
          if loop >= 0 then output.(index) <- means.(loop))
  end;
  output

let remap_attribute ?cancel ~grain ~point_map ~point_centers ~vertex_map
    ~vertex_centers ~primitive_map ~loop_offsets ~loop_points ~loop_vertices
    attribute =
  let owner = Attribute.owner attribute in
  if owner = Attribute.Detail
      || (owner = Attribute.Point && Array.length point_map = 0) then
    Ok attribute
  else begin
    let mapping, centers, center_sources = match owner with
      | Attribute.Point -> point_map, point_centers, loop_points
      | Attribute.Vertex -> vertex_map, vertex_centers, loop_vertices
      | Attribute.Primitive -> primitive_map, No_centers, [||]
      | Attribute.Detail -> assert false in
    let map_float_component values = map_float ?cancel ~grain ~loop_offsets
        ~loop_sources:center_sources mapping centers values in
    let storage = match Attribute.View.storage attribute with
      | Attribute.Float values -> Attribute.Float (map_float_component values)
      | Attribute.Int values -> Attribute.Int
          (map_array ?cancel ~grain mapping values)
      | Attribute.Int_array values -> Attribute.Int_array
          (Ragged_ops.remap_int ?cancel ~grain mapping values)
      | Attribute.Float_array values -> Attribute.Float_array
          (Ragged_ops.remap_float ?cancel ~grain mapping values)
      | Attribute.Text values -> Attribute.Text
          (map_array ?cancel ~grain mapping values)
      | Attribute.Float2 values ->
          let values = Packed.Float2.View.view values in
          Attribute.Float2 (Packed.Float2.of_owned
            ~x:(map_float_component values.x)
            ~y:(map_float_component values.y) |> Result.get_ok)
      | Attribute.Float3 values ->
          let values = Packed.Float3.View.view values in
          Attribute.Float3 (Packed.Float3.View.of_owned_exn
            ~x:(map_float_component values.x)
            ~y:(map_float_component values.y)
            ~z:(map_float_component values.z))
      | Attribute.Float4 values ->
          let values = Packed.Float4.View.view values in
          Attribute.Float4 (Packed.Float4.of_owned
            ~x:(map_float_component values.x)
            ~y:(map_float_component values.y)
            ~z:(map_float_component values.z)
            ~w:(map_float_component values.w) |> Result.get_ok) in
    Attribute.create_owned ~name:(Attribute.name attribute) ~owner storage
  end

let remap_group ~grain ~source_points ~source_vertices ~source_primitives
    ~point_map ~point_center_base ~vertex_map ~primitive_map group =
  let mapping, source_count, target_count, selected = match Group.owner group with
    | Group.Point ->
        let target_count = if Array.length point_map = 0
          then source_points else Array.length point_map in
        point_map, source_points, target_count,
        (fun target ->
          if target < source_points then Group.mem target group
          else (match point_center_base with
            | Some first when target >= first -> false
            | None | Some _ -> Group.mem point_map.(target) group))
    | Group.Vertex ->
        vertex_map, source_vertices, Array.length vertex_map,
        (fun target -> target < source_vertices && Group.mem target group)
    | Group.Primitive ->
        primitive_map, source_primitives, Array.length primitive_map,
        (fun target -> target < source_primitives && Group.mem target group) in
  if target_count = source_count then group
  else begin
    let target = Group.init ~grain ~owner:(Group.owner group)
        ~name:(Group.name group) target_count selected in
    let ancestry = if Array.length mapping = 0
      then Array.init target_count Fun.id else mapping in
    Group.View.remap_order ~source:group ~source_of_target:ancestry target
  end

let rec merge_patch_group patch = function
  | [] -> Ok [patch]
  | group :: rest when Group.owner group = Group.Primitive
      && String.equal (Group.name group) (Group.name patch) ->
      Result.map (fun merged -> merged :: rest) (Group.union group patch)
  | group :: rest ->
      Result.map (fun rest -> group :: rest) (merge_patch_group patch rest)

let target_edge_count ~cancel ~mode ~unique_points ~plan ~triangle_local =
  let source_edges = Topology_index.edge_count plan.source_index in
  let loop_count = Array.length plan.loop_offsets - 1
  and boundary_count = Array.length plan.loop_points in
  if unique_points then
    source_edges + (match mode with
      | Fill_single_polygon -> boundary_count
      | Fill_triangles -> (2 * boundary_count) - (3 * loop_count)
      | Fill_triangle_fan -> 2 * boundary_count)
  else match mode with
    | Fill_single_polygon -> source_edges
    | Fill_triangle_fan -> source_edges + boundary_count
    | Fill_triangles ->
        let added = ref 0 in
        for loop = 0 to loop_count - 1 do
          if loop land 4_095 = 0 then Cancel.check_opt cancel;
          let first = plan.loop_offsets.(loop)
          and size = plan.loop_offsets.(loop + 1) - plan.loop_offsets.(loop) in
          let triangle_base = (first - (2 * loop)) * 3 in
          for triangle = 0 to size - 4 do
            let a = triangle_local.(triangle_base + (triangle * 3))
            and c = triangle_local.(triangle_base + (triangle * 3) + 2) in
            let a = plan.loop_points.(first + a)
            and c = plan.loop_points.(first + c) in
            if Topology_index.find_edge_index plan.source_index ~a ~b:c < 0 then
              incr added
          done
        done;
        source_edges + !added

let remap_edge_groups ?cancel ~mode ~unique_points ~plan ~triangle_local
    ~target_topology groups =
  match groups with
  | [] -> Ok []
  | _ ->
      let edge_count = target_edge_count ~cancel ~mode ~unique_points ~plan
          ~triangle_local in
      Ok (List.map (fun group ->
        let bits = Bytes.make ((edge_count + 7) / 8) '\000' in
        Edge_group.iter (fun edge ->
          let byte = edge lsr 3 and mask = 1 lsl (edge land 7) in
          Bytes.set bits byte
            (Char.chr (Char.code (Bytes.get bits byte) lor mask))) group;
        Edge_group.View.of_owned_bits ~topology:target_topology ~edge_count
          ~name:(Edge_group.name group) bits) groups)

let run ?cancel ?(grain = 16_384) ?boundary ?(mode = Fill_triangles)
    ?(reverse_patches = false) ?(unique_points = false)
    ?(update_point_normals = false) ?patch_group geometry =
  Error.guard ~operation:"poly_fill" ~code:"invalid_geometry" @@ fun () ->
  if grain <= 0 then invalid_arg (operation ^ ": grain must be positive");
  let* () = match patch_group with
    | Some name when String.trim name = "" ->
        Error (operation ^ ": patch group name must not be empty")
    | None | Some _ -> Ok () in
  let* plan = plan_boundaries ?cancel ~grain ?boundary geometry in
  let loop_count = Array.length plan.loop_offsets - 1 in
  if loop_count = 0 then Ok geometry
  else begin
    let source_topology = Geometry.topology geometry in
    let source = Topology.View.view source_topology in
    let source_positions = Packed.Float3.View.view (Geometry.positions geometry) in
    let source_points = source.point_count
    and source_vertices = Array.length source.vertex_points
    and source_primitives = Bytes.length source.primitive_kinds
    and boundary_count = Array.length plan.loop_points in
    let patch_primitives_64 = match mode with
      | Fill_single_polygon -> Int64.of_int loop_count
      | Fill_triangles -> Int64.of_int (boundary_count - (2 * loop_count))
      | Fill_triangle_fan -> Int64.of_int boundary_count in
    let patch_vertices_64 = match mode with
      | Fill_single_polygon -> Int64.of_int boundary_count
      | Fill_triangles -> Int64.mul 3L patch_primitives_64
      | Fill_triangle_fan -> Int64.mul 3L (Int64.of_int boundary_count) in
    let added_boundary_points_64 =
      if unique_points then Int64.of_int boundary_count else 0L in
    let added_centers_64 =
      if mode = Fill_triangle_fan then Int64.of_int loop_count else 0L in
    let* _patch_primitives = checked_length "generated primitive"
        patch_primitives_64 in
    let* _patch_vertices = checked_length "generated vertex" patch_vertices_64 in
    let* output_points = checked_length "point"
        (Int64.add (Int64.of_int source_points)
          (Int64.add added_boundary_points_64 added_centers_64)) in
    let* output_vertices = checked_length "vertex"
        (Int64.add (Int64.of_int source_vertices) patch_vertices_64) in
    let* output_primitives = checked_length "primitive"
        (Int64.add (Int64.of_int source_primitives) patch_primitives_64) in
    let added_boundary_points = if unique_points then boundary_count else 0 in
    let* center_x, center_y, center_z =
      analyze_positions ?cancel ~grain plan source_positions in
    let* triangle_local = match mode with
      | Fill_triangles -> triangulate_loops ?cancel ~grain plan source_positions
      | Fill_single_polygon | Fill_triangle_fan -> Ok [||] in
    let point_map = if output_points = source_points then [||]
      else Array.make output_points 0 in
    let point_center_base = if mode = Fill_triangle_fan
      then Some (source_points + added_boundary_points) else None in
    let output_positions = if output_points = source_points then
      Geometry.positions geometry
    else begin
      let x = Array.make output_points 0. and y = Array.make output_points 0.
      and z = Array.make output_points 0. in
      Array.blit source_positions.x 0 x 0 source_points;
      Array.blit source_positions.y 0 y 0 source_points;
      Array.blit source_positions.z 0 z 0 source_points;
      for point = 0 to source_points - 1 do point_map.(point) <- point done;
      let center_base = source_points + added_boundary_points in
      Parallel.for_ ~chunk_size:(max 1 (grain / 16)) ~start:0
        ~finish:(loop_count - 1) (fun loop ->
          let first = plan.loop_offsets.(loop)
          and last = plan.loop_offsets.(loop + 1) in
          if unique_points then
            for at = first to last - 1 do
              let output = source_points + at and source_point = plan.loop_points.(at) in
              point_map.(output) <- source_point;
              x.(output) <- source_positions.x.(source_point);
              y.(output) <- source_positions.y.(source_point);
              z.(output) <- source_positions.z.(source_point)
            done;
          if mode = Fill_triangle_fan then begin
            let output = center_base + loop in
            point_map.(output) <- plan.loop_points.(first);
            x.(output) <- center_x.(loop);
            y.(output) <- center_y.(loop);
            z.(output) <- center_z.(loop)
          end);
      Packed.Float3.View.of_owned_exn ~x ~y ~z
    end in
    let patch_primitive_base = Array.make loop_count 0
    and patch_vertex_base = Array.make loop_count 0 in
    let primitive_at = ref source_primitives and vertex_at = ref source_vertices in
    for loop = 0 to loop_count - 1 do
      let size = plan.loop_offsets.(loop + 1) - plan.loop_offsets.(loop) in
      patch_primitive_base.(loop) <- !primitive_at;
      patch_vertex_base.(loop) <- !vertex_at;
      primitive_at := !primitive_at + (match mode with
        | Fill_single_polygon -> 1 | Fill_triangles -> size - 2
        | Fill_triangle_fan -> size);
      vertex_at := !vertex_at + (match mode with
        | Fill_single_polygon -> size | Fill_triangles -> (size - 2) * 3
        | Fill_triangle_fan -> size * 3)
    done;
    let vertex_points = Array.make output_vertices 0
    and vertex_map = Array.make output_vertices 0
    and primitive_offsets = Array.make (output_primitives + 1) 0
    and primitive_map = Array.make output_primitives 0
    and primitive_kinds = Bytes.make output_primitives '\000' in
    Array.blit source.vertex_points 0 vertex_points 0 source_vertices;
    Array.blit source.primitive_offsets 0 primitive_offsets 0
      (source_primitives + 1);
    Bytes.blit source.primitive_kinds 0 primitive_kinds 0 source_primitives;
    for vertex = 0 to source_vertices - 1 do vertex_map.(vertex) <- vertex done;
    for primitive = 0 to source_primitives - 1 do primitive_map.(primitive) <- primitive done;
    let point_of_local loop local =
      if unique_points then source_points + plan.loop_offsets.(loop) + local
      else plan.loop_points.(plan.loop_offsets.(loop) + local) in
    let vertex_of_local loop local =
      plan.loop_vertices.(plan.loop_offsets.(loop) + local) in
    let source_primitive_of_local loop local =
      plan.source_index_view.primitive_of_vertex.(vertex_of_local loop local) in
    let center_base = source_points + added_boundary_points in
    Parallel.for_ ~chunk_size:(max 1 (grain / 64)) ~start:0
      ~finish:(loop_count - 1) (fun loop ->
        Cancel.check_opt cancel;
        let first = plan.loop_offsets.(loop)
        and size = plan.loop_offsets.(loop + 1) - plan.loop_offsets.(loop)
        and primitive_base = patch_primitive_base.(loop)
        and vertex_base = patch_vertex_base.(loop) in
        match mode with
        | Fill_single_polygon ->
            primitive_offsets.(primitive_base) <- vertex_base;
            primitive_map.(primitive_base) <- source_primitive_of_local loop 0;
            for output_local = 0 to size - 1 do
              let source_local = if reverse_patches || output_local = 0
                then output_local else size - output_local in
              let output = vertex_base + output_local in
              vertex_points.(output) <- point_of_local loop source_local;
              vertex_map.(output) <- vertex_of_local loop source_local
            done
        | Fill_triangles ->
            let triangle_base = (first - (2 * loop)) * 3 in
            for triangle = 0 to size - 3 do
              let primitive = primitive_base + triangle
              and output = vertex_base + (triangle * 3)
              and source_triangle = triangle_base + (triangle * 3) in
              let a = triangle_local.(source_triangle)
              and b = triangle_local.(source_triangle + 1)
              and c = triangle_local.(source_triangle + 2) in
              let a, b, c = if reverse_patches then a, b, c else a, c, b in
              primitive_offsets.(primitive) <- output;
              primitive_map.(primitive) <- source_primitive_of_local loop a;
              vertex_points.(output) <- point_of_local loop a;
              vertex_points.(output + 1) <- point_of_local loop b;
              vertex_points.(output + 2) <- point_of_local loop c;
              vertex_map.(output) <- vertex_of_local loop a;
              vertex_map.(output + 1) <- vertex_of_local loop b;
              vertex_map.(output + 2) <- vertex_of_local loop c
            done
        | Fill_triangle_fan ->
            let center = center_base + loop in
            for edge = 0 to size - 1 do
              let next = (edge + 1) mod size
              and primitive = primitive_base + edge
              and output = vertex_base + (edge * 3) in
              primitive_offsets.(primitive) <- output;
              primitive_map.(primitive) <- source_primitive_of_local loop edge;
              if reverse_patches then begin
                vertex_points.(output) <- point_of_local loop edge;
                vertex_points.(output + 1) <- point_of_local loop next;
                vertex_points.(output + 2) <- center;
                vertex_map.(output) <- vertex_of_local loop edge;
                vertex_map.(output + 1) <- vertex_of_local loop next;
                vertex_map.(output + 2) <- vertex_of_local loop 0
              end else begin
                vertex_points.(output) <- point_of_local loop edge;
                vertex_points.(output + 1) <- center;
                vertex_points.(output + 2) <- point_of_local loop next;
                vertex_map.(output) <- vertex_of_local loop edge;
                vertex_map.(output + 1) <- vertex_of_local loop 0;
                vertex_map.(output + 2) <- vertex_of_local loop next
              end
            done);
    primitive_offsets.(output_primitives) <- output_vertices;
    let output_topology = Topology.View.create_validated_owned
        ~point_count:output_points ~vertex_points ~primitive_offsets
        ~primitive_kinds in
    let point_centers = match point_center_base with
      | None -> No_centers | Some first -> Point_centers first in
    let vertex_centers = match point_center_base with
      | None -> No_centers
      | Some first_point -> Vertex_centers { vertex_points; first_point } in
    let rec remap_attributes result = function
      | [] -> Ok (List.rev result)
      | attribute :: rest ->
          let* mapped = remap_attribute ?cancel ~grain ~point_map ~point_centers
              ~vertex_map ~vertex_centers ~primitive_map
              ~loop_offsets:plan.loop_offsets ~loop_points:plan.loop_points
              ~loop_vertices:plan.loop_vertices attribute in
          remap_attributes (mapped :: result) rest in
    let* attributes = remap_attributes [] (Geometry.attributes geometry) in
    let groups = List.map (remap_group ~grain ~source_points ~source_vertices
        ~source_primitives ~point_map ~point_center_base ~vertex_map ~primitive_map)
        (Geometry.groups geometry) in
    let* groups = match patch_group with
      | None -> Ok groups
      | Some name ->
          let patch = Group.init ~grain ~owner:Group.Primitive ~name
              output_primitives (fun primitive -> primitive >= source_primitives) in
          merge_patch_group patch groups in
    let* edge_groups = remap_edge_groups ?cancel ~mode ~unique_points ~plan
        ~triangle_local ~target_topology:output_topology
        (Geometry.edge_groups geometry) in
    let* output = Geometry.create ~positions:output_positions
        ~topology:output_topology ~attributes ~groups ~edge_groups () in
    if update_point_normals
        && Option.is_some (Geometry.find_attribute ~owner:Attribute.Point "N" geometry)
    then begin
      let* with_normals = Deform.normals ?cancel ~grain output in
      match Geometry.find_attribute ~owner:Attribute.Point "N" with_normals with
      | None -> Ok output
      | Some normal -> Geometry.with_attribute normal output
    end else Ok output
  end
