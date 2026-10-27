open Rays_math

let finite value = match classify_float value with
  | FP_normal | FP_subnormal | FP_zero -> true
  | FP_infinite | FP_nan -> false

let checked_total label source copies selected =
  if selected <> 0 && copies > (Sys.max_array_length - source) / selected then
    Error (Printf.sprintf "Rdk_mesh.Duplicate.duplicate: %s output exceeds array limits" label)
  else Ok (source + (copies * selected))

let selected ?cancel ~grain ~primitives ~transforms geometry =
  if grain <= 0 then invalid_arg "Rdk_mesh.Duplicate.duplicate: grain must be positive";
  Cancel.check_opt cancel;
  let source_points = Geometry.point_count geometry
  and source_vertices = Geometry.vertex_count geometry
  and source_primitives = Geometry.primitive_count geometry in
  if Group.owner primitives <> Group.Primitive then
    Error "Rdk_mesh.Duplicate.duplicate: source selection must own primitives"
  else if Group.length primitives <> source_primitives then
    Error "Rdk_mesh.Duplicate.duplicate: source selection length does not match primitive count"
  else
    let copies = Array.length transforms in
    let invalid_transform = ref (-1) in
    Array.iteri (fun copy matrix ->
      for row = 0 to 3 do
        for column = 0 to 3 do
          if !invalid_transform < 0
              && not (finite (Rays_math.Mat4.get matrix ~row ~column)) then
            invalid_transform := copy
        done
      done) transforms;
    if !invalid_transform >= 0 then Error (Printf.sprintf
        "Rdk_mesh.Duplicate.duplicate: transform for copy %d must be finite"
        (!invalid_transform + 1))
    else begin
      let topology_value = Geometry.topology geometry in
      let topology = Topology.Private.view topology_value in
      let selected_primitive_count = Group.cardinality primitives in
      let selected_primitives = Array.make selected_primitive_count 0
      and selected_vertex_count = ref 0 and primitive_at = ref 0
      and used_points = Bytes.make ((source_points + 7) / 8) '\000' in
      for primitive = 0 to source_primitives - 1 do
        if primitive land 4_095 = 0 then Cancel.check_opt cancel;
        if Group.mem primitive primitives then begin
          selected_primitives.(!primitive_at) <- primitive;
          incr primitive_at;
          let first = topology.primitive_offsets.(primitive)
          and last = topology.primitive_offsets.(primitive + 1) in
          selected_vertex_count := !selected_vertex_count + last - first;
          for vertex = first to last - 1 do
            let point = topology.vertex_points.(vertex) in
            let byte = point lsr 3 and mask = 1 lsl (point land 7) in
            Bytes.unsafe_set used_points byte (Char.unsafe_chr
              (Char.code (Bytes.unsafe_get used_points byte) lor mask))
          done
        end
      done;
      let selected_point_count = ref 0 in
      for point = 0 to source_points - 1 do
        if Char.code (Bytes.unsafe_get used_points (point lsr 3))
            land (1 lsl (point land 7)) <> 0 then incr selected_point_count
      done;
      if copies = 0 || selected_primitive_count = 0 then Ok geometry
      else Result.bind
        (checked_total "point" source_points copies !selected_point_count)
        (fun output_points -> Result.bind
        (checked_total "vertex" source_vertices copies !selected_vertex_count)
        (fun output_vertices -> Result.bind
        (checked_total "primitive" source_primitives copies
          selected_primitive_count)
        (fun output_primitives ->
          if output_primitives = Sys.max_array_length then Error
              "Rdk_mesh.Duplicate.duplicate: primitive-offset output exceeds array limits"
          else begin
            let selected_points = Array.make !selected_point_count 0
            and point_local = Array.make source_points (-1)
            and point_at = ref 0 in
            for point = 0 to source_points - 1 do
              if Char.code (Bytes.unsafe_get used_points (point lsr 3))
                  land (1 lsl (point land 7)) <> 0 then begin
                selected_points.(!point_at) <- point;
                point_local.(point) <- !point_at;
                incr point_at
              end
            done;
            let selected_vertices = Array.make !selected_vertex_count 0
            and vertex_local = Array.make source_vertices (-1)
            and selected_offsets = Array.make (selected_primitive_count + 1) 0
            and vertex_at = ref 0 in
            Array.iteri (fun selected primitive ->
              selected_offsets.(selected) <- !vertex_at;
              for vertex = topology.primitive_offsets.(primitive)
                  to topology.primitive_offsets.(primitive + 1) - 1 do
                selected_vertices.(!vertex_at) <- vertex;
                vertex_local.(vertex) <- !vertex_at;
                incr vertex_at
              done) selected_primitives;
            selected_offsets.(selected_primitive_count) <- !vertex_at;
            let primitive_local = Array.make source_primitives (-1) in
            Array.iteri (fun local primitive ->
              primitive_local.(primitive) <- local) selected_primitives;
            let point_map = Array.make output_points 0
            and vertex_map = Array.make output_vertices 0
            and primitive_map = Array.make output_primitives 0 in
            for point = 0 to source_points - 1 do point_map.(point) <- point done;
            for vertex = 0 to source_vertices - 1 do
              vertex_map.(vertex) <- vertex
            done;
            for primitive = 0 to source_primitives - 1 do
              primitive_map.(primitive) <- primitive
            done;
            let added_points = output_points - source_points
            and added_vertices = output_vertices - source_vertices
            and added_primitives = output_primitives - source_primitives in
            if added_points > 0 then Parallel.for_ ~chunk_size:grain ~start:0
                ~finish:(added_points - 1) (fun local ->
              if local land 4_095 = 0 then Cancel.check_opt cancel;
              point_map.(source_points + local) <-
                selected_points.(local mod !selected_point_count));
            if added_vertices > 0 then Parallel.for_ ~chunk_size:grain ~start:0
                ~finish:(added_vertices - 1) (fun local ->
              if local land 4_095 = 0 then Cancel.check_opt cancel;
              vertex_map.(source_vertices + local) <-
                selected_vertices.(local mod !selected_vertex_count));
            if added_primitives > 0 then Parallel.for_ ~chunk_size:grain ~start:0
                ~finish:(added_primitives - 1) (fun local ->
              if local land 4_095 = 0 then Cancel.check_opt cancel;
              primitive_map.(source_primitives + local) <-
                selected_primitives.(local mod selected_primitive_count));
            let source_positions = Packed.Float3.Private.view
                (Geometry.positions geometry) in
            let x = Array.make output_points 0. and y = Array.make output_points 0.
            and z = Array.make output_points 0. in
            Array.blit source_positions.x 0 x 0 source_points;
            Array.blit source_positions.y 0 y 0 source_points;
            Array.blit source_positions.z 0 z 0 source_points;
            let transform_rows = Array.map Rays_math.Mat4.to_rows transforms in
            if added_points > 0 then Parallel.for_ ~chunk_size:grain ~start:0
                ~finish:(added_points - 1) (fun local ->
              if local land 4_095 = 0 then Cancel.check_opt cancel;
              let copy = local / !selected_point_count
              and point = selected_points.(local mod !selected_point_count) in
              let (m00,m01,m02,m03), (m10,m11,m12,m13),
                  (m20,m21,m22,m23), (m30,m31,m32,m33) =
                transform_rows.(copy) in
              let px = source_positions.x.(point)
              and py = source_positions.y.(point)
              and pz = source_positions.z.(point) in
              let ox = m00*.px +. m01*.py +. m02*.pz +. m03
              and oy = m10*.px +. m11*.py +. m12*.pz +. m13
              and oz = m20*.px +. m21*.py +. m22*.pz +. m23
              and ow = m30*.px +. m31*.py +. m32*.pz +. m33 in
              let output = source_points + local in
              if abs_float ow <= 1e-12 then begin
                x.(output) <- ox; y.(output) <- oy; z.(output) <- oz
              end else begin
                x.(output) <- ox /. ow; y.(output) <- oy /. ow;
                z.(output) <- oz /. ow
              end);
            let vertex_points = Array.make output_vertices 0
            and primitive_offsets = Array.make (output_primitives + 1) 0
            and primitive_kinds = Bytes.make output_primitives '\000' in
            Array.blit topology.vertex_points 0 vertex_points 0 source_vertices;
            Array.blit topology.primitive_offsets 0 primitive_offsets 0
              (source_primitives + 1);
            Bytes.blit topology.primitive_kinds 0 primitive_kinds 0
              source_primitives;
            if added_vertices > 0 then Parallel.for_ ~chunk_size:grain ~start:0
                ~finish:(added_vertices - 1) (fun local ->
              if local land 4_095 = 0 then Cancel.check_opt cancel;
              let copy = local / !selected_vertex_count
              and selected_vertex = local mod !selected_vertex_count in
              let source_vertex = selected_vertices.(selected_vertex) in
              vertex_points.(source_vertices + local) <- source_points
                + (copy * !selected_point_count)
                + point_local.(topology.vertex_points.(source_vertex)));
            if added_primitives > 0 then Parallel.for_ ~chunk_size:grain ~start:0
                ~finish:(added_primitives - 1) (fun local ->
              if local land 4_095 = 0 then Cancel.check_opt cancel;
              let copy = local / selected_primitive_count
              and selected = local mod selected_primitive_count in
              let output = source_primitives + local
              and primitive = selected_primitives.(selected) in
              primitive_offsets.(output) <- source_vertices
                + (copy * !selected_vertex_count) + selected_offsets.(selected);
              Bytes.set primitive_kinds output
                (Bytes.get topology.primitive_kinds primitive));
            primitive_offsets.(output_primitives) <- output_vertices;
            let output_topology = Topology.Private.create_validated_owned
                ~point_count:output_points ~vertex_points ~primitive_offsets
                ~primitive_kinds in
            let normal_rows =
              let rows = Array.make copies (Rays_math.Mat4.to_rows Rays_math.Mat4.identity)
              and valid = ref true in
              for copy = 0 to copies - 1 do
                match Rays_math.Mat4.inverse transforms.(copy) with
                | None -> valid := false
                | Some inverse -> rows.(copy) <- Rays_math.Mat4.to_rows (Rays_math.Mat4.transpose inverse)
              done;
              if !valid then Some rows else None in
            let normal_attribute mapping source_prefix selected_count attribute =
              match normal_rows, Attribute.Private.storage attribute with
              | None, _ -> None
              | Some rows, Attribute.Float3 values ->
                  let source = Packed.Float3.Private.view values
                  and count = Array.length mapping
                  and identity_rows = Rays_math.Mat4.to_rows Rays_math.Mat4.identity in
                  let nx = Array.make count 0. and ny = Array.make count 0.
                  and nz = Array.make count 0. in
                  if count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
                      ~finish:(count - 1) (fun output ->
                    if output land 4_095 = 0 then Cancel.check_opt cancel;
                    let source_at = mapping.(output) in
                    let (m00,m01,m02,_), (m10,m11,m12,_),
                        (m20,m21,m22,_), _ =
                      if output < source_prefix then identity_rows
                      else rows.((output - source_prefix) / selected_count) in
                    let ox = m00*.source.x.(source_at) +. m01*.source.y.(source_at)
                        +. m02*.source.z.(source_at)
                    and oy = m10*.source.x.(source_at) +. m11*.source.y.(source_at)
                        +. m12*.source.z.(source_at)
                    and oz = m20*.source.x.(source_at) +. m21*.source.y.(source_at)
                        +. m22*.source.z.(source_at) in
                    let length = Float.hypot (Float.hypot ox oy) oz in
                    if length > 1e-20 then begin
                      nx.(output) <- ox /. length;
                      ny.(output) <- oy /. length;
                      nz.(output) <- oz /. length
                    end);
                  let values = Packed.Float3.Private.of_owned_exn
                      ~x:nx ~y:ny ~z:nz in
                  Some (Attribute.create_key_owned
                    (Attribute.normal ~owner:(Attribute.owner attribute)) values
                    |> Result.get_ok)
              | Some _, _ -> Some (Topology_remap.attribute ?cancel ~grain
                  mapping attribute) in
            let typed_normal attribute = match Attribute.Private.storage attribute with
              | Attribute.Float3 _ -> String.equal (Attribute.name attribute) "N"
                  && (Attribute.owner attribute = Attribute.Point
                      || Attribute.owner attribute = Attribute.Vertex)
              | _ -> false in
            let attributes = List.filter_map (fun attribute ->
              match Attribute.owner attribute with
              | Attribute.Detail -> Some attribute
              | Attribute.Point when typed_normal attribute ->
                  normal_attribute point_map source_points !selected_point_count
                    attribute
              | Attribute.Vertex when typed_normal attribute ->
                  normal_attribute vertex_map source_vertices !selected_vertex_count
                    attribute
              | Attribute.Point -> Some (Topology_remap.attribute ?cancel ~grain
                  point_map attribute)
              | Attribute.Vertex -> Some (Topology_remap.attribute ?cancel ~grain
                  vertex_map attribute)
              | Attribute.Primitive -> Some (Topology_remap.attribute ?cancel ~grain
                  primitive_map attribute)) (Geometry.attributes geometry) in
            let remap_group mapping source_count selected_count selected_local group =
              let output = Group.init ~grain ~owner:(Group.owner group)
                  ~name:(Group.name group) (Array.length mapping) (fun target ->
                    if target land 4_095 = 0 then Cancel.check_opt cancel;
                    Group.mem mapping.(target) group) in
              match Group.Private.order_view group with
              | None -> output
              | Some source_order ->
                  let order = Array.make (Group.cardinality output) 0
                  and at = ref 0 in
                  Array.iter (fun source ->
                    order.(!at) <- source; incr at) source_order;
                  for copy = 0 to copies - 1 do
                    if copy land 255 = 0 then Cancel.check_opt cancel;
                    Array.iter (fun source ->
                      let local = selected_local.(source) in
                      if local >= 0 then begin
                        order.(!at) <- source_count + (copy * selected_count) + local;
                        incr at
                      end) source_order
                  done;
                  Group.Private.with_owned_order order output in
            let groups = List.map (fun group -> match Group.owner group with
              | Group.Point -> remap_group point_map source_points
                  !selected_point_count point_local group
              | Group.Vertex -> remap_group vertex_map source_vertices
                  !selected_vertex_count vertex_local group
              | Group.Primitive -> remap_group primitive_map source_primitives
                  selected_primitive_count primitive_local group)
                (Geometry.groups geometry) in
            let edge_groups = match Geometry.edge_groups geometry with
              | [] -> []
              | source_groups ->
                  let source_index = Topology_index.create ?cancel topology_value in
                  let source_edge_count = Topology_index.edge_count source_index in
                  let index_view = Topology_index.Private.view source_index in
                  let seen = Bytes.make ((source_edge_count + 7) / 8) '\000'
                  and selected_edges_full = Array.make !selected_vertex_count 0
                  and edge_at = ref 0 in
                  Array.iter (fun vertex ->
                    let edge = index_view.edge_of_vertex.(vertex) in
                    if edge >= 0 then begin
                      let byte = edge lsr 3 and mask = 1 lsl (edge land 7) in
                      let value = Char.code (Bytes.unsafe_get seen byte) in
                      if value land mask = 0 then begin
                        Bytes.unsafe_set seen byte (Char.unsafe_chr (value lor mask));
                        selected_edges_full.(!edge_at) <- edge;
                        incr edge_at
                      end
                    end) selected_vertices;
                  let selected_edges = Array.sub selected_edges_full 0 !edge_at in
                  let output_edge_count = source_edge_count
                    + (copies * Array.length selected_edges) in
                  List.map (fun group ->
                    let bytes = Bytes.make ((output_edge_count + 7) / 8) '\000' in
                    if Bytes.length bytes > 0 then Parallel.for_
                        ~chunk_size:(max 1 (grain / 8)) ~start:0
                        ~finish:(Bytes.length bytes - 1) (fun byte ->
                      if byte land 511 = 0 then Cancel.check_opt cancel;
                      let value = ref 0 in
                      for bit = 0 to 7 do
                        let target = (byte lsl 3) + bit in
                        if target < output_edge_count then begin
                          let source = if target < source_edge_count then target
                            else selected_edges.((target - source_edge_count)
                              mod Array.length selected_edges) in
                          if Edge_group.mem source group then
                            value := !value lor (1 lsl bit)
                        end
                      done;
                      Bytes.unsafe_set bytes byte (Char.unsafe_chr !value));
                    Edge_group.Private.of_owned_bits ~topology:output_topology
                      ~edge_count:output_edge_count ~name:(Edge_group.name group)
                      bytes) source_groups in
            let positions = Packed.Float3.Private.of_owned_exn ~x ~y ~z in
            Geometry.create ~positions ~topology:output_topology ~attributes
              ~groups ~edge_groups ()
          end)))
    end

let validate_copy_groups ~prefix ~copies ~primitive_count =
  if copies < 0 || primitive_count < 0 then invalid_arg
      "Rdk_mesh.Duplicate.duplicate: negative copy-group cardinality";
  if String.trim prefix = "" then Error
      "Rdk_mesh.Duplicate.duplicate: copy-group prefix must not be empty"
  else if copies > 4_096 then Error
      "Rdk_mesh.Duplicate.duplicate: output copy groups exceed the 4096-group limit"
  else
    let bytes_per_group = (primitive_count + 7) / 8 in
    if copies <> 0 && bytes_per_group > 268_435_456 / copies then Error
        "Rdk_mesh.Duplicate.duplicate: output copy-group payload exceeds 256 MiB"
    else begin
      for copy = 0 to copies - 1 do
        let name = prefix ^ string_of_int (copy + 1) in
        ignore (Group.init ~owner:Group.Primitive ~name 0 (fun _ -> false))
      done;
      Ok ()
    end

let add_copy_groups ?cancel ~grain ~prefix ~preserve ~copies
    ~primitives_per_copy geometry =
  if grain <= 0 then invalid_arg "Rdk_mesh.Duplicate.duplicate: grain must be positive";
  let primitive_count = Geometry.primitive_count geometry in
  Result.bind (validate_copy_groups ~prefix ~copies ~primitive_count) (fun () ->
    begin
      let bytes_per_group = (primitive_count + 7) / 8 in
      let names = Array.init copies (fun copy ->
        let name = prefix ^ string_of_int (copy + 1) in
        name) in
      let generated = Array.init copies (fun copy ->
        Cancel.check_opt cancel;
        let bits = Bytes.make bytes_per_group '\000'
        and first = primitive_count - ((copies - copy) * primitives_per_copy) in
        for primitive = first to first + primitives_per_copy - 1 do
          let byte = primitive lsr 3 and mask = 1 lsl (primitive land 7) in
          Bytes.unsafe_set bits byte (Char.unsafe_chr
            (Char.code (Bytes.unsafe_get bits byte) lor mask))
        done;
        let created = Group.Private.of_owned_bits ~owner:Group.Primitive
            ~name:names.(copy) ~length:primitive_count bits in
        if not preserve then created
        else match Geometry.find_group ~owner:Group.Primitive names.(copy) geometry with
          | None -> created
          | Some existing -> Group.union existing created |> Result.get_ok) in
      let generated_names = Hashtbl.create copies in
      Array.iter (fun name -> Hashtbl.replace generated_names name ()) names;
      let retained = List.filter (fun group ->
        Group.owner group <> Group.Primitive
        || not (Hashtbl.mem generated_names (Group.name group)))
          (Geometry.groups geometry) in
      Geometry.create ~positions:(Geometry.positions geometry)
        ~topology:(Geometry.topology geometry)
        ~attributes:(Geometry.attributes geometry)
        ~groups:(retained @ Array.to_list generated)
        ~edge_groups:(Geometry.edge_groups geometry) ()
    end)
