open Rays_math

let run ?cancel ?(grain = 16_384) ?(keep_original = true) ~origin ~normal
    geometry =
  Error.guard ~operation:"mirror" ~code:"invalid_parameter" @@ fun () ->
  if grain <= 0 then invalid_arg "Rdk_mesh.Mirror_geometry.mirror: grain must be positive";
  let ox = origin.Rays_math.Vec3.x and oy = origin.y and oz = origin.z
  and supplied_nx = normal.Rays_math.Vec3.x and supplied_ny = normal.y
  and supplied_nz = normal.z in
  if not (Float.is_finite ox && Float.is_finite oy && Float.is_finite oz && Float.is_finite supplied_nx
          && Float.is_finite supplied_ny && Float.is_finite supplied_nz) then
    Error "Rdk_mesh.Mirror_geometry.mirror: plane origin and normal must be finite"
  else
    let length = sqrt ((supplied_nx *. supplied_nx)
        +. (supplied_ny *. supplied_ny) +. (supplied_nz *. supplied_nz)) in
    if length <= 1e-20 then Error "Rdk_mesh.Mirror_geometry.mirror: plane normal must be non-zero"
    else
      let nx = supplied_nx /. length and ny = supplied_ny /. length
      and nz = supplied_nz /. length in
      let source_points = Geometry.point_count geometry
      and source_vertices = Geometry.vertex_count geometry
      and source_primitives = Geometry.primitive_count geometry in
      let copies = if keep_original then 2 else 1 in
      if source_points > max_int / copies || source_vertices > max_int / copies
         || source_primitives > max_int / copies then
        Error "Rdk_mesh.Mirror_geometry.mirror: output cardinality exceeds OCaml array limits"
      else
        let output_points = source_points * copies
        and output_vertices = source_vertices * copies
        and output_primitives = source_primitives * copies in
        let source_positions = Packed.Float3.Private.view
            (Geometry.positions geometry) in
        let px = Array.make output_points 0. and py = Array.make output_points 0.
        and pz = Array.make output_points 0. in
        if keep_original then begin
          Array.blit source_positions.x 0 px 0 source_points;
          Array.blit source_positions.y 0 py 0 source_points;
          Array.blit source_positions.z 0 pz 0 source_points
        end;
        let reflected_point_base = if keep_original then source_points else 0 in
        if source_points > 0 then Parallel.for_ ~chunk_size:grain ~start:0
            ~finish:(source_points - 1) (fun point ->
              if point land 16383 = 0 then Cancel.check_opt cancel;
              let x = source_positions.x.(point)
              and y = source_positions.y.(point)
              and z = source_positions.z.(point) in
              let distance = ((x -. ox) *. nx) +. ((y -. oy) *. ny)
                  +. ((z -. oz) *. nz) in
              let output = reflected_point_base + point in
              px.(output) <- x -. (2. *. distance *. nx);
              py.(output) <- y -. (2. *. distance *. ny);
              pz.(output) <- z -. (2. *. distance *. nz));
        let source_topology = Topology.Private.view (Geometry.topology geometry) in
        let vertex_points = Array.make output_vertices 0
        and vertex_map = Array.make output_vertices 0
        and primitive_offsets = Array.make (output_primitives + 1) 0
        and primitive_map = Array.make output_primitives 0
        and primitive_kinds = Bytes.make output_primitives '\000' in
        if keep_original then begin
          Array.blit source_topology.vertex_points 0 vertex_points 0 source_vertices;
          for vertex = 0 to source_vertices - 1 do vertex_map.(vertex) <- vertex done;
          Array.blit source_topology.primitive_offsets 0 primitive_offsets 0
            (source_primitives + 1);
          for primitive = 0 to source_primitives - 1 do
            primitive_map.(primitive) <- primitive
          done;
          Bytes.blit source_topology.primitive_kinds 0 primitive_kinds 0
            source_primitives
        end;
        let reflected_vertex_base = if keep_original then source_vertices else 0
        and reflected_primitive_base = if keep_original then source_primitives else 0 in
        if source_primitives > 0 then Parallel.for_ ~chunk_size:(max 1 (grain / 8))
            ~start:0 ~finish:(source_primitives - 1) (fun primitive ->
              if primitive land 1023 = 0 then Cancel.check_opt cancel;
              let first = source_topology.primitive_offsets.(primitive)
              and last = source_topology.primitive_offsets.(primitive + 1) in
              let count = last - first
              and output_first = reflected_vertex_base + first
              and output_primitive = reflected_primitive_base + primitive in
              primitive_offsets.(output_primitive) <- output_first;
              primitive_map.(output_primitive) <- primitive;
              let polygon = Bytes.get source_topology.primitive_kinds primitive = '\000' in
              for local = 0 to count - 1 do
                let source_vertex = if polygon then first + count - local - 1
                  else first + local in
                let output_vertex = output_first + local in
                vertex_points.(output_vertex) <- reflected_point_base
                    + source_topology.vertex_points.(source_vertex);
                vertex_map.(output_vertex) <- source_vertex
              done;
              Bytes.set primitive_kinds output_primitive
                (Bytes.get source_topology.primitive_kinds primitive));
        primitive_offsets.(output_primitives) <- output_vertices;
        let reflect_normal_arrays owner mapping values =
          let source = Packed.Float3.Private.view values in
          let count = Array.length mapping in
          let x = Array.make count 0. and y = Array.make count 0.
          and z = Array.make count 0. in
          let original_count = match owner with
            | Attribute.Point -> source_points
            | Attribute.Vertex -> source_vertices
            | Attribute.Primitive | Attribute.Detail -> 0 in
          if count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
              ~finish:(count - 1) (fun output ->
                let input = mapping.(output) in
                let vx = source.x.(input) and vy = source.y.(input)
                and vz = source.z.(input) in
                if keep_original && output < original_count then begin
                  x.(output) <- vx; y.(output) <- vy; z.(output) <- vz
                end else begin
                  let projection = (vx *. nx) +. (vy *. ny) +. (vz *. nz) in
                  x.(output) <- vx -. (2. *. projection *. nx);
                  y.(output) <- vy -. (2. *. projection *. ny);
                  z.(output) <- vz -. (2. *. projection *. nz)
                end);
          Attribute.Float3 (Packed.Float3.Private.of_owned_exn ~x ~y ~z)
        in
        let point_map = Array.init output_points (fun output ->
            if keep_original && output < source_points then output
            else output - reflected_point_base) in
        let map_attribute attribute =
          let owner = Attribute.owner attribute in
          match owner with
          | Attribute.Detail -> Ok attribute
          | Attribute.Point | Attribute.Vertex | Attribute.Primitive ->
              let mapping = match owner with
                | Attribute.Point -> point_map
                | Attribute.Vertex -> vertex_map
                | Attribute.Primitive -> primitive_map
                | Attribute.Detail -> assert false in
              (match Attribute.Private.storage attribute with
               | Attribute.Float3 values
                 when String.equal (Attribute.name attribute) "N"
                      && (owner = Attribute.Point || owner = Attribute.Vertex) ->
                   Attribute.create_owned ~name:(Attribute.name attribute)
                     ~owner (reflect_normal_arrays owner mapping values)
               | _ -> Ok (Topology_remap.attribute ?cancel ~grain mapping attribute))
        in
        let rec map_attributes result = function
          | [] -> Ok (List.rev result)
          | attribute :: rest -> Result.bind (map_attribute attribute)
              (fun mapped -> map_attributes (mapped :: result) rest) in
        Result.bind (map_attributes [] (Geometry.attributes geometry))
          (fun attributes ->
            let groups = List.map
                (Topology_remap.mapped_group ?cancel ~grain ~point_map
                   ~vertex_map ~primitive_map)
                (Geometry.groups geometry) in
            let positions = Packed.Float3.Private.of_owned_exn ~x:px ~y:py ~z:pz
            and topology = Topology.Private.create_validated_owned
                ~point_count:output_points ~vertex_points ~primitive_offsets
                ~primitive_kinds in
            let edge_groups = match Geometry.edge_groups geometry with
              | [] -> []
              | source_groups ->
                  let source_topology = Geometry.topology geometry in
                  let source_index = Topology_index.create ?cancel source_topology
                  and target_index = Topology_index.create ?cancel topology in
                  let remap offset group =
                    let point_map = Array.init source_points (fun point ->
                      point + offset) in
                    Edge_group.remap ?cancel ~source_index
                      ~target_topology:topology ~target_index ~point_map group
                    |> Support.get_ok in
                  List.map (fun group ->
                    if keep_original then
                      Edge_group.union (remap 0 group)
                        (remap reflected_point_base group) |> Support.get_ok
                    else remap 0 group) source_groups in
            Geometry.create ~positions ~topology ~attributes ~groups ~edge_groups ())
