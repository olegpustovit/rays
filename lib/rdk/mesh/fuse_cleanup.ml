
exception Invalid of string

let fail message = raise (Invalid ("Rdk_mesh.Fuse_cleanup.fuse: " ^ message))
let get_ok = function Ok value -> value | Error message -> fail message

let run ?cancel ~grain count operation =
  if count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
      ~finish:(count - 1) (fun index ->
        if index land 4095 = 0 then Cancel.check_opt cancel;
        operation index)

let minimum_vertices kind = if kind = '\001' then 2 else 3

let remap_attribute ?cancel ~grain ~topology_changed ~point_identity
    ~vertex_identity ~primitive_identity ~point_map ~vertex_map ~primitive_map
    attribute =
  let owner = Attribute.owner attribute in
  if topology_changed && String.equal (Attribute.name attribute) "N"
      && (owner = Attribute.Point || owner = Attribute.Vertex) then None
  else
    let mapping, identity = match owner with
      | Attribute.Point -> Some point_map, point_identity
      | Attribute.Vertex -> Some vertex_map, vertex_identity
      | Attribute.Primitive -> Some primitive_map, primitive_identity
      | Attribute.Detail -> None, true in
    match mapping with
    | None -> Some attribute
    | Some _ when identity -> Some attribute
    | Some mapping ->
        Some (Topology_remap.attribute ?cancel ~grain mapping attribute)

let remap_group ?cancel ~grain ~point_identity ~vertex_identity
    ~primitive_identity ~point_map ~vertex_map ~primitive_map group =
  let mapping, identity = match Group.owner group with
    | Group.Point -> point_map, point_identity
    | Group.Vertex -> vertex_map, vertex_identity
    | Group.Primitive -> primitive_map, primitive_identity in
  if identity then group
  else Topology_remap.group ?cancel ~grain mapping group

let identity_mapping mapping source_count =
  if Array.length mapping <> source_count then false
  else begin
    let index = ref 0 in
    while !index < source_count && mapping.(!index) = !index do incr index done;
    !index = source_count
  end

let apply_with_mapping ?cancel ~grain ~remove_degenerate_primitives
    ~remove_unused_points_from_degenerate_primitives
    ~remove_all_unused_points geometry =
  try
    Cancel.check_opt cancel;
    let source_topology_value = Geometry.topology geometry in
    let source = Topology.Private.view source_topology_value in
    let source_points = source.point_count
    and source_vertices = Array.length source.vertex_points
    and source_primitives = Bytes.length source.primitive_kinds in
    if not remove_degenerate_primitives
        && not remove_all_unused_points then
      Ok (geometry, Array.init source_points Fun.id)
    else begin
      let keep_vertex = Bytes.make source_vertices '\000'
      and retained_counts = Array.make source_primitives 0 in
      if remove_degenerate_primitives then
        run ?cancel ~grain source_primitives (fun primitive ->
          let first = source.primitive_offsets.(primitive)
          and last = source.primitive_offsets.(primitive + 1) in
          let retained = ref 0 and previous_point = ref (-1)
          and first_vertex = ref (-1) and last_vertex = ref (-1) in
          for vertex = first to last - 1 do
            let point = source.vertex_points.(vertex) in
            if !retained = 0 || point <> !previous_point then begin
              Bytes.unsafe_set keep_vertex vertex '\001';
              if !retained = 0 then first_vertex := vertex;
              last_vertex := vertex;
              previous_point := point;
              incr retained
            end
          done;
          let kind = Bytes.unsafe_get source.primitive_kinds primitive in
          if kind <> '\001' && !retained > 1
              && source.vertex_points.(!first_vertex)
                 = source.vertex_points.(!last_vertex) then begin
            Bytes.unsafe_set keep_vertex !last_vertex '\000';
            decr retained
          end;
          if !retained >= minimum_vertices kind then
            retained_counts.(primitive) <- !retained)
      else
        run ?cancel ~grain source_primitives (fun primitive ->
          let first = source.primitive_offsets.(primitive)
          and last = source.primitive_offsets.(primitive + 1) in
          retained_counts.(primitive) <- last - first;
          for vertex = first to last - 1 do
            Bytes.unsafe_set keep_vertex vertex '\001'
          done);
      let primitive_target = Array.make source_primitives (-1)
      and output_vertex_first = Array.make source_primitives 0
      and output_primitives = ref 0 and output_vertices = ref 0 in
      for primitive = 0 to source_primitives - 1 do
        if primitive land 4095 = 0 then Cancel.check_opt cancel;
        let count = retained_counts.(primitive) in
        if count > 0 then begin
          if !output_vertices > max_int - count then
            fail "cleanup vertex cardinality exceeds integer limits";
          primitive_target.(primitive) <- !output_primitives;
          output_vertex_first.(primitive) <- !output_vertices;
          incr output_primitives;
          output_vertices := !output_vertices + count
        end
      done;
      let vertex_map = Array.make !output_vertices 0
      and primitive_map = Array.make !output_primitives 0
      and vertex_points = Array.make !output_vertices 0
      and primitive_offsets = Array.make (!output_primitives + 1) 0
      and primitive_kinds = Bytes.make !output_primitives '\000' in
      run ?cancel ~grain source_primitives (fun primitive ->
        let target = primitive_target.(primitive) in
        if target >= 0 then begin
          let output = ref output_vertex_first.(primitive) in
          primitive_map.(target) <- primitive;
          Bytes.unsafe_set primitive_kinds target
            (Bytes.unsafe_get source.primitive_kinds primitive);
          for vertex = source.primitive_offsets.(primitive)
              to source.primitive_offsets.(primitive + 1) - 1 do
            if Bytes.unsafe_get keep_vertex vertex <> '\000' then begin
              vertex_map.(!output) <- vertex;
              vertex_points.(!output) <- source.vertex_points.(vertex);
              incr output
            end
          done;
          primitive_offsets.(target + 1) <- !output
        end);
      let point_map, old_to_new =
        if not remove_all_unused_points
            && not remove_unused_points_from_degenerate_primitives then
          Array.init source_points Fun.id, Array.init source_points Fun.id
        else begin
          let used_after = Bytes.make source_points '\000' in
          Array.iter (fun point -> Bytes.unsafe_set used_after point '\001')
            vertex_points;
          let used_before = if remove_all_unused_points then Bytes.empty
            else begin
              let bits = Bytes.make source_points '\000' in
              Array.iter (fun point -> Bytes.unsafe_set bits point '\001')
                source.vertex_points;
              bits
            end in
          let retained = ref 0 and old_to_new = Array.make source_points (-1) in
          for point = 0 to source_points - 1 do
            if point land 16383 = 0 then Cancel.check_opt cancel;
            let keep = if remove_all_unused_points then
                Bytes.unsafe_get used_after point <> '\000'
              else Bytes.unsafe_get used_after point <> '\000'
                || Bytes.unsafe_get used_before point = '\000' in
            if keep then begin old_to_new.(point) <- !retained; incr retained end
          done;
          let point_map = Array.make !retained 0 and output = ref 0 in
          for point = 0 to source_points - 1 do
            if old_to_new.(point) >= 0 then begin
              point_map.(!output) <- point;
              incr output
            end
          done;
          point_map, old_to_new
        end in
      let point_identity = identity_mapping point_map source_points
      and vertex_identity = identity_mapping vertex_map source_vertices
      and primitive_identity = identity_mapping primitive_map source_primitives in
      let topology_changed = not vertex_identity || not primitive_identity in
      if point_identity && not topology_changed then Ok (geometry, old_to_new)
      else begin
        if not point_identity then run ?cancel ~grain !output_vertices
            (fun vertex ->
              let point = old_to_new.(vertex_points.(vertex)) in
              if point < 0 then fail "cleanup retained a deleted point";
              vertex_points.(vertex) <- point);
        let output_topology = Topology.Private.create_validated_owned
            ~point_count:(Array.length point_map) ~vertex_points
            ~primitive_offsets ~primitive_kinds in
        let positions = if point_identity then Geometry.positions geometry
          else
            let source_positions = Packed.Float3.Private.view
                (Geometry.positions geometry) in
            Packed.Float3.Private.of_owned_exn
              ~x:(Topology_remap.select ?cancel ~grain point_map source_positions.x)
              ~y:(Topology_remap.select ?cancel ~grain point_map source_positions.y)
              ~z:(Topology_remap.select ?cancel ~grain point_map source_positions.z) in
        let attributes = Geometry.attributes geometry |> List.filter_map
            (remap_attribute ?cancel ~grain ~topology_changed ~point_identity
              ~vertex_identity ~primitive_identity ~point_map ~vertex_map
              ~primitive_map) in
        let groups = Geometry.groups geometry |> List.map
            (remap_group ?cancel ~grain ~point_identity ~vertex_identity
              ~primitive_identity ~point_map ~vertex_map ~primitive_map) in
        let edge_groups = match Geometry.edge_groups geometry with
          | [] -> []
          | source_groups ->
              let source_index = Topology_index.create ?cancel source_topology_value
              and target_index = Topology_index.create ?cancel output_topology in
              List.map (fun group -> Edge_group.remap ?cancel ~source_index
                ~target_topology:output_topology ~target_index
                ~point_map:old_to_new group |> get_ok) source_groups in
        Geometry.create ~positions ~topology:output_topology ~attributes
          ~groups ~edge_groups ()
        |> Result.map (fun geometry -> geometry, old_to_new)
      end
    end
  with Invalid message -> Error message

let apply ?cancel ~grain ~remove_degenerate_primitives
    ~remove_unused_points_from_degenerate_primitives
    ~remove_all_unused_points geometry =
  apply_with_mapping ?cancel ~grain ~remove_degenerate_primitives
    ~remove_unused_points_from_degenerate_primitives ~remove_all_unused_points
    geometry
  |> Result.map fst
