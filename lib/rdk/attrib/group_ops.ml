open Rays_math

type owner =
  | Group_points
  | Group_vertices
  | Group_primitives
  | Group_edges

type promote_mode =
  | Include_any
  | Include_all
  | Include_shared_edge

type boundary_attribute = {
  boundary_attribute_owner : Attribute.owner;
  boundary_attribute_pattern : string;
}

type promote_boundary_options = {
  promote_boundary_attributes : boundary_attribute list;
  promote_boundary_tolerance : float;
  promote_include_unshared_edges : bool;
  promote_include_all_unshared_curve_edges : bool;
  promote_include_all_primitives_sharing_boundary_points : bool;
}

and promote_operation =
  | Promote_elements of promote_mode
  | Promote_boundary of promote_boundary_options

and promotion_rule = {
  promotion_source : owner;
  promotion_destination : owner;
  promotion_pattern : string;
  promotion_new_name : string option;
  promotion_keep_original : bool;
  promotion_output_as_attribute : bool;
  promotion_operation : promote_operation;
}

type primitive_connectivity =
  | Primitive_share_points
  | Primitive_share_edges

type expand_normal_attribute = {
  expand_normal_owner : Attribute.owner;
  expand_normal_name : string;
}

type expand_collision = {
  expand_collision_owner : owner;
  expand_collision_group : string;
  expand_collision_contain : bool;
  expand_collision_allow_boundary : bool;
}

type boolean_operation =
  | Group_replace
  | Group_union
  | Group_intersection
  | Group_subtract
  | Group_xor

type operand = { pattern : string; inverted : bool }
type combine_step = { operation : boolean_operation; operand : operand }

type range =
  | Range_start_end of { start : int; end_ : int }
  | Range_from_ends of { start : int; end_offset : int }
  | Range_start_length of { start : int; length : int }
  | Range_partition of { partition : int; partitions : int }

type range_filter = { select : int; of_ : int; offset : int }

type range_collision = {
  collision_owner : owner;
  collision_pattern : string;
  keep_boundary : bool;
}

type range_connectivity =
  | Range_disconnected of { region : int option }
  | Range_connected of {
      connectivity_attributes : string option;
      connectivity_tolerance : float;
      collision : range_collision option;
      region : int option;
      remove_other_regions : bool;
    }

type range_rule = {
  range_owner : owner;
  range_name : string;
  range_base : string option;
  range_invert : bool;
  range_filter : range_filter option;
  range_connectivity : range_connectivity option;
  range_merge : boolean_operation;
  range_specification : range;
}

let range_rule ~owner ~name specification = {
  range_owner = owner;
  range_name = name;
  range_base = None;
  range_invert = false;
  range_filter = None;
  range_connectivity = None;
  range_merge = Group_replace;
  range_specification = specification;
}

type rename_conflict =
  | Rename_skip
  | Rename_error
  | Rename_overwrite
  | Rename_union

type rename_rule = {
  rename_owner : owner option;
  rename_pattern : string;
  rename_replacement : string;
  rename_conflict : rename_conflict;
}

type delete_rule = {
  delete_owner : owner option;
  delete_pattern : string;
}

type copy_conflict = Copy_skip | Copy_overwrite | Copy_add_suffix

type copy_rule = {
  copy_owner : owner;
  copy_pattern : string;
  copy_prefix : string;
  match_attribute : string option;
}

type transfer_rule = {
  transfer_owner : owner;
  transfer_pattern : string;
  transfer_prefix : string;
}

type name_conflict = Name_replace | Name_union
type invalid_name_policy = Ignore_invalid | Force_valid
type name_overlap = First_group | Last_group | Error_on_overlap

type bounds =
  | Bounds_box of { minimum : Rays_math.Vec3.t; maximum : Rays_math.Vec3.t }
  | Bounds_sphere of { center : Rays_math.Vec3.t; radius : float }

type containment = Fully_contained | Partially_contained

type selection =
  | Ordinary of Group.t
  | Native_edges of Edge_group.t

let byte_member_bits = Array.init 256 (fun value ->
  let count = ref 0 in
  for bit = 0 to 7 do if value land (1 lsl bit) <> 0 then incr count done;
  let output = Array.make !count 0 and cursor = ref 0 in
  for bit = 0 to 7 do
    if value land (1 lsl bit) <> 0 then begin
      output.(!cursor) <- bit;
      incr cursor
    end
  done;
  output)

let packed_init ?cancel ~grain length predicate =
  let bytes_count = Support.Bits.byte_count length in
  let bits = Bytes.make bytes_count '\000' in
  if bytes_count > 0 then
    Parallel.for_ ~chunk_size:(max 1 (grain / 8)) ~start:0
      ~finish:(bytes_count - 1) (fun byte ->
        if byte land 2047 = 0 then Cancel.check_opt cancel;
        let base = byte lsl 3 and value = ref 0 in
        for bit = 0 to min 7 (length - base - 1) do
          if predicate (base + bit) then value := !value lor (1 lsl bit)
        done;
        Bytes.unsafe_set bits byte (Char.chr !value));
  bits

let owner_name = function
  | Group_points -> "point"
  | Group_vertices -> "vertex"
  | Group_primitives -> "primitive"
  | Group_edges -> "edge"

let ordinary_owner = function
  | Group_points -> Some Group.Point
  | Group_vertices -> Some Group.Vertex
  | Group_primitives -> Some Group.Primitive
  | Group_edges -> None

let owner_count geometry index = function
  | Group_points -> Geometry.point_count geometry
  | Group_vertices -> Geometry.vertex_count geometry
  | Group_primitives -> Geometry.primitive_count geometry
  | Group_edges -> Topology_index.edge_count index

let find_selection owner name geometry =
  match ordinary_owner owner with
  | Some group_owner ->
      Option.map (fun group -> Ordinary group)
        (Geometry.find_group ~owner:group_owner name geometry)
  | None ->
      Option.map (fun group -> Native_edges group)
        (Geometry.find_edge_group name geometry)

let selection_mem selection index = match selection with
  | Ordinary group -> Group.mem index group
  | Native_edges group -> Edge_group.mem index group

let selection_length = function
  | Ordinary group -> Group.length group
  | Native_edges group -> Edge_group.length group

let selection_cardinality = function
  | Ordinary group -> Group.cardinality group
  | Native_edges group -> Edge_group.cardinality group

let renamed_selection name = function
  | Ordinary group -> Some (Group.with_name name group), None
  | Native_edges group -> None, Some (Edge_group.with_name name group)

let remove_group owner name geometry = match ordinary_owner owner with
  | Some group_owner -> Geometry.without_group ~owner:group_owner name geometry
  | None -> Geometry.without_edge_group name geometry

let install_group owner group edge_group geometry = match owner, group, edge_group with
  | Group_edges, None, Some group -> Geometry.with_edge_group group geometry
  | (Group_points | Group_vertices | Group_primitives), Some group, None ->
      Geometry.with_group group geometry
  | _ -> Error "Group operation: internal output-owner mismatch"

let promote_predicate ~source ~destination ~mode topology index selection =
  let topology = Topology.Private.view topology
  and reverse = Topology_index.Private.view index in
  let member = selection_mem selection in
  match destination with
  | Group_points ->
      (match source with
       | Group_points -> member
       | Group_vertices -> (fun point ->
           let slot = ref reverse.point_offsets.(point)
           and last = reverse.point_offsets.(point + 1) and found = ref false in
           while not !found && !slot < last do
             found := member reverse.point_vertices.(!slot);
             incr slot
           done;
           !found)
       | Group_primitives -> (fun point ->
           let slot = ref reverse.point_offsets.(point)
           and last = reverse.point_offsets.(point + 1) and found = ref false in
           while not !found && !slot < last do
             let vertex = reverse.point_vertices.(!slot) in
             found := member reverse.primitive_of_vertex.(vertex);
             incr slot
           done;
           !found)
       | Group_edges -> (fun point ->
           let slot = ref reverse.point_edge_offsets.(point)
           and last = reverse.point_edge_offsets.(point + 1)
           and found = ref false in
           while not !found && !slot < last do
             found := member reverse.point_edges.(!slot);
             incr slot
           done;
           !found))
  | Group_vertices ->
      (match source with
       | Group_points -> (fun vertex -> member topology.vertex_points.(vertex))
       | Group_vertices -> member
       | Group_primitives -> (fun vertex -> member reverse.primitive_of_vertex.(vertex))
       | Group_edges ->
           (match mode with
            | Include_all -> (fun vertex ->
                let edge = reverse.edge_of_vertex.(vertex) in
                edge >= 0 && member edge)
            | Include_any | Include_shared_edge -> (fun vertex ->
                let outgoing = reverse.edge_of_vertex.(vertex) in
                let previous = reverse.previous_vertex.(vertex) in
                (outgoing >= 0 && member outgoing)
                || (previous >= 0
                    && let incoming = reverse.edge_of_vertex.(previous) in
                       incoming >= 0 && member incoming))))
  | Group_primitives ->
      (match source with
       | Group_points ->
           (fun primitive ->
             let vertex = ref topology.primitive_offsets.(primitive)
             and last = topology.primitive_offsets.(primitive + 1) in
             (match mode with
              | Include_any ->
                  let found = ref false in
                  while not !found && !vertex < last do
                    found := member topology.vertex_points.(!vertex);
                    incr vertex
                  done;
                  !found
              | Include_all ->
                  let valid = ref (!vertex < last) in
                  while !valid && !vertex < last do
                    valid := member topology.vertex_points.(!vertex);
                    incr vertex
                  done;
                  !valid
              | Include_shared_edge ->
                  let found = ref false in
                  while not !found && !vertex < last do
                    let next = reverse.next_vertex.(!vertex) in
                    found := next >= 0
                      && member topology.vertex_points.(!vertex)
                      && member topology.vertex_points.(next);
                    incr vertex
                  done;
                  !found))
       | Group_vertices ->
           (fun primitive ->
             let vertex = ref topology.primitive_offsets.(primitive)
             and last = topology.primitive_offsets.(primitive + 1) in
             (match mode with
              | Include_any ->
                  let found = ref false in
                  while not !found && !vertex < last do
                    found := member !vertex;
                    incr vertex
                  done;
                  !found
              | Include_all ->
                  let valid = ref (!vertex < last) in
                  while !valid && !vertex < last do
                    valid := member !vertex;
                    incr vertex
                  done;
                  !valid
              | Include_shared_edge ->
                  let found = ref false in
                  while not !found && !vertex < last do
                    let next = reverse.next_vertex.(!vertex) in
                    found := next >= 0 && member !vertex && member next;
                    incr vertex
                  done;
                  !found))
       | Group_primitives -> member
       | Group_edges ->
           (fun primitive ->
             let vertex = ref topology.primitive_offsets.(primitive)
             and last = topology.primitive_offsets.(primitive + 1) in
             match mode with
             | Include_any | Include_shared_edge ->
                 let found = ref false in
                 while not !found && !vertex < last do
                   let edge = reverse.edge_of_vertex.(!vertex) in
                   found := edge >= 0 && member edge;
                   incr vertex
                 done;
                 !found
             | Include_all ->
                 let valid = ref (!vertex < last) in
                 while !valid && !vertex < last do
                   let edge = reverse.edge_of_vertex.(!vertex) in
                   valid := edge >= 0 && member edge;
                   incr vertex
                 done;
                 !valid))
  | Group_edges ->
      (match source with
       | Group_points ->
           (match mode with
            | Include_any -> (fun edge ->
                member reverse.edge_a.(edge) || member reverse.edge_b.(edge))
            | Include_all | Include_shared_edge -> (fun edge ->
                member reverse.edge_a.(edge) && member reverse.edge_b.(edge)))
       | Group_vertices -> (fun edge ->
           let slot = ref reverse.edge_offsets.(edge)
           and last = reverse.edge_offsets.(edge + 1) in
           (match mode with
            | Include_any ->
                let found = ref false in
                while not !found && !slot < last do
                  let vertex = reverse.edge_vertices.(!slot) in
                  let next = reverse.next_vertex.(vertex) in
                  found := member vertex || (next >= 0 && member next);
                  incr slot
                done;
                !found
            | Include_all | Include_shared_edge ->
                let valid = ref (!slot < last) in
                while !valid && !slot < last do
                  let vertex = reverse.edge_vertices.(!slot) in
                  let next = reverse.next_vertex.(vertex) in
                  valid := member vertex && next >= 0 && member next;
                  incr slot
                done;
                !valid))
       | Group_primitives -> (fun edge ->
           let slot = ref reverse.edge_offsets.(edge)
           and last = reverse.edge_offsets.(edge + 1) in
           (match mode with
            | Include_any ->
                let found = ref false in
                while not !found && !slot < last do
                  let vertex = reverse.edge_vertices.(!slot) in
                  found := member reverse.primitive_of_vertex.(vertex);
                  incr slot
                done;
                !found
            | Include_all | Include_shared_edge ->
                let valid = ref (!slot < last) in
                while !valid && !slot < last do
                  let vertex = reverse.edge_vertices.(!slot) in
                  valid := member reverse.primitive_of_vertex.(vertex);
                  incr slot
                done;
                !valid))
       | Group_edges -> member)

let promoted_selection ?cancel ~grain ~source ~destination ~mode ~name
    selection geometry =
  if source = destination then
    match renamed_selection name selection with
    | Some group, None -> Ordinary group
    | None, Some group -> Native_edges group
    | _ -> assert false
  else
    let topology = Geometry.topology geometry in
    let index = Topology_index.create ?cancel topology in
    let predicate = promote_predicate ~source ~destination ~mode topology index
        selection in
    let count = owner_count geometry index destination in
    match ordinary_owner destination with
    | Some owner -> Ordinary (Group.init ~grain ~owner ~name count
        (fun element ->
          if element land 16_383 = 0 then Cancel.check_opt cancel;
          predicate element))
    | None -> Native_edges (Edge_group.init ~grain ~topology ~index ~name
        (fun edge ->
          if edge land 16_383 = 0 then Cancel.check_opt cancel;
          predicate edge))

type boundary_storage =
  | Boundary_selection of selection
  | Boundary_position of Packed.Float3.Private.view
  | Boundary_float of float array
  | Boundary_int of int array
  | Boundary_int_array of Packed.Int_array.Private.view
  | Boundary_float_array of Packed.Float_array.Private.view
  | Boundary_float2 of Packed.Float2.Private.view
  | Boundary_float3 of Packed.Float3.Private.view
  | Boundary_float4 of Packed.Float4.Private.view
  | Boundary_text of string array

type boundary_plane = {
  boundary_owner : Attribute.owner;
  boundary_name : string;
  boundary_storage : boundary_storage;
}

let boundary_storage_of_attribute attribute =
  match Attribute.Private.storage attribute with
  | Attribute.Float values -> Boundary_float values
  | Attribute.Int values -> Boundary_int values
  | Attribute.Int_array values ->
      Boundary_int_array (Packed.Int_array.Private.view values)
  | Attribute.Float_array values ->
      Boundary_float_array (Packed.Float_array.Private.view values)
  | Attribute.Float2 values ->
      Boundary_float2 (Packed.Float2.Private.view values)
  | Attribute.Float3 values ->
      Boundary_float3 (Packed.Float3.Private.view values)
  | Attribute.Float4 values ->
      Boundary_float4 (Packed.Float4.Private.view values)
  | Attribute.Text values -> Boundary_text values

let boundary_storage_length = function
  | Boundary_selection selection -> selection_length selection
  | Boundary_position values -> Array.length values.x
  | Boundary_float values -> Array.length values
  | Boundary_int values -> Array.length values
  | Boundary_int_array values -> Array.length values.offsets - 1
  | Boundary_float_array values -> Array.length values.offsets - 1
  | Boundary_float2 values -> Array.length values.x
  | Boundary_float3 values -> Array.length values.x
  | Boundary_float4 values -> Array.length values.x
  | Boundary_text values -> Array.length values

let boundary_storage_finite storage index = match storage with
  | Boundary_selection _ -> true
  | Boundary_position values ->
      Float.is_finite values.x.(index) && Float.is_finite values.y.(index)
      && Float.is_finite values.z.(index)
  | Boundary_float values -> Float.is_finite values.(index)
  | Boundary_float2 values ->
      Float.is_finite values.x.(index) && Float.is_finite values.y.(index)
  | Boundary_float3 values ->
      Float.is_finite values.x.(index) && Float.is_finite values.y.(index)
      && Float.is_finite values.z.(index)
  | Boundary_float4 values ->
      Float.is_finite values.x.(index) && Float.is_finite values.y.(index)
      && Float.is_finite values.z.(index) && Float.is_finite values.w.(index)
  | Boundary_float_array values ->
      let finite = ref true in
      for value = values.offsets.(index) to values.offsets.(index + 1) - 1 do
        if not (Float.is_finite values.values.(value)) then finite := false
      done;
      !finite
  | Boundary_int _ | Boundary_int_array _ | Boundary_text _ -> true

let boundary_float_differs tolerance left right =
  abs_float (left -. right) > tolerance

let boundary_storage_differs tolerance storage left right = match storage with
  | Boundary_selection selection ->
      selection_mem selection left <> selection_mem selection right
  | Boundary_position values | Boundary_float3 values ->
      boundary_float_differs tolerance values.x.(left) values.x.(right)
      || boundary_float_differs tolerance values.y.(left) values.y.(right)
      || boundary_float_differs tolerance values.z.(left) values.z.(right)
  | Boundary_float values ->
      boundary_float_differs tolerance values.(left) values.(right)
  | Boundary_int values -> values.(left) <> values.(right)
  | Boundary_int_array values ->
      let left_first = values.offsets.(left)
      and right_first = values.offsets.(right) in
      let length = values.offsets.(left + 1) - left_first in
      length <> values.offsets.(right + 1) - right_first
      || begin
        let differs = ref false in
        for offset = 0 to length - 1 do
          if values.values.(left_first + offset)
              <> values.values.(right_first + offset) then differs := true
        done;
        !differs
      end
  | Boundary_float_array values ->
      let left_first = values.offsets.(left)
      and right_first = values.offsets.(right) in
      let length = values.offsets.(left + 1) - left_first in
      length <> values.offsets.(right + 1) - right_first
      || begin
        let differs = ref false in
        for offset = 0 to length - 1 do
          if boundary_float_differs tolerance values.values.(left_first + offset)
              values.values.(right_first + offset) then differs := true
        done;
        !differs
      end
  | Boundary_float2 values ->
      boundary_float_differs tolerance values.x.(left) values.x.(right)
      || boundary_float_differs tolerance values.y.(left) values.y.(right)
  | Boundary_float4 values ->
      boundary_float_differs tolerance values.x.(left) values.x.(right)
      || boundary_float_differs tolerance values.y.(left) values.y.(right)
      || boundary_float_differs tolerance values.z.(left) values.z.(right)
      || boundary_float_differs tolerance values.w.(left) values.w.(right)
  | Boundary_text values -> not (String.equal values.(left) values.(right))

let group_from_attribute_boundary ?cancel ?(grain = 16_384) ?membership_boundary
    ?(attributes = [])
    ?(tolerance = 1e-6) ?(include_unshared_edges = false)
    ?(include_all_unshared_curve_edges = false)
    ?(include_all_primitives_sharing_boundary_points = false)
    ~owner ~name geometry =
  if grain <= 0 then Error "Group from Attribute Boundary: grain must be positive"
  else if String.trim name = "" then
    Error "Group from Attribute Boundary: empty output name"
  else if not (Float.is_finite tolerance) || tolerance < 0. then
    Error "Group from Attribute Boundary: tolerance must be finite and non-negative"
  else if owner = Group_vertices then
    Error "Group from Attribute Boundary: vertex output is not supported"
  else if include_all_unshared_curve_edges && not include_unshared_edges then
    Error "Group from Attribute Boundary: all curve edges require unshared edges"
  else if include_all_primitives_sharing_boundary_points
      && owner <> Group_primitives then
    Error "Group from Attribute Boundary: point-sharing expansion requires primitive output"
  else
    let rec compile_rules compiled = function
      | [] -> Ok (List.rev compiled)
      | rule :: rest ->
          if rule.boundary_attribute_owner = Attribute.Detail then
            Error "Group from Attribute Boundary: detail attributes have no topology boundary"
          else Result.bind
              (Attribute_pattern.compile rule.boundary_attribute_pattern)
              (fun pattern -> compile_rules
                ((rule.boundary_attribute_owner, pattern) :: compiled) rest) in
    Result.bind (compile_rules [] attributes) (fun compiled ->
      let selected attribute_owner attribute_name =
        List.exists (fun (owner, pattern) -> owner = attribute_owner
          && Attribute_pattern.matches pattern attribute_name) compiled in
      let planes = ref (match membership_boundary with
        | None -> []
        | Some (owner, selection) -> [{ boundary_owner = owner;
            boundary_name = "__membership";
            boundary_storage = Boundary_selection selection }]) in
      if selected Attribute.Point "P" then
        planes := { boundary_owner = Attribute.Point; boundary_name = "P";
          boundary_storage = Boundary_position
            (Packed.Float3.Private.view (Geometry.positions geometry)) } :: !planes;
      List.iter (fun attribute ->
        let attribute_owner = Attribute.owner attribute
        and attribute_name = Attribute.name attribute in
        if selected attribute_owner attribute_name then
          planes := { boundary_owner = attribute_owner;
            boundary_name = attribute_name;
            boundary_storage = boundary_storage_of_attribute attribute } :: !planes)
        (Geometry.attributes geometry);
      let planes = Array.of_list (List.rev !planes) in
      if attributes <> [] && Array.length planes = 0 then
        Error "Group from Attribute Boundary: attribute patterns matched nothing"
      else begin
        Cancel.check_opt cancel;
        let invalid = ref None and plane = ref 0 in
        while !plane < Array.length planes && !invalid = None do
          let current = planes.(!plane) in
          let first_invalid = Atomic.make max_int in
          let length = boundary_storage_length current.boundary_storage in
          if length > 0 then Parallel.for_ ~chunk_size:grain ~start:0
              ~finish:(length - 1) (fun element ->
                if element land 16_383 = 0 then Cancel.check_opt cancel;
                if not (boundary_storage_finite current.boundary_storage element)
                then begin
                  let rec lower observed =
                    if element < observed
                        && not (Atomic.compare_and_set first_invalid observed element)
                    then lower (Atomic.get first_invalid) in
                  lower (Atomic.get first_invalid)
                end);
          let element = Atomic.get first_invalid in
          if element <> max_int then invalid := Some (current, element);
          incr plane
        done;
        match !invalid with
        | Some (plane, element) -> Error (Printf.sprintf
            "Group from Attribute Boundary: %s attribute %S element %d is non-finite"
            (match plane.boundary_owner with
             | Attribute.Point -> "point" | Attribute.Vertex -> "vertex"
             | Attribute.Primitive -> "primitive" | Attribute.Detail -> "detail")
            plane.boundary_name element)
        | None ->
            let topology_value = Geometry.topology geometry in
            let topology = Topology.Private.view topology_value
            and index_value = Topology_index.create ?cancel topology_value in
            let index = Topology_index.Private.view index_value in
            let edge_count = Array.length index.edge_a in
            let attribute_boundary edge =
              let first = index.edge_offsets.(edge)
              and last = index.edge_offsets.(edge + 1)
              and different = ref false and plane_index = ref 0 in
              while not !different && !plane_index < Array.length planes do
                let plane = planes.(!plane_index) in
                begin match plane.boundary_owner with
                | Attribute.Point ->
                    different := boundary_storage_differs tolerance
                      plane.boundary_storage index.edge_a.(edge) index.edge_b.(edge)
                | Attribute.Primitive when last - first >= 2 ->
                    let reference = index.primitive_of_vertex.(
                        index.edge_vertices.(first)) and slot = ref (first + 1) in
                    while not !different && !slot < last do
                      let primitive = index.primitive_of_vertex.(
                          index.edge_vertices.(!slot)) in
                      different := boundary_storage_differs tolerance
                        plane.boundary_storage reference primitive;
                      incr slot
                    done
                | Attribute.Vertex when last - first >= 2 ->
                    let reference_vertex = index.edge_vertices.(first) in
                    let reference_next = index.next_vertex.(reference_vertex) in
                    let reference_starts_at_a =
                      topology.vertex_points.(reference_vertex)
                        = index.edge_a.(edge) in
                    let reference_a = if reference_starts_at_a
                      then reference_vertex else reference_next
                    and reference_b = if reference_starts_at_a
                      then reference_next else reference_vertex in
                    let slot = ref (first + 1) in
                    while not !different && !slot < last do
                      let vertex = index.edge_vertices.(!slot) in
                      let next = index.next_vertex.(vertex) in
                      let starts_at_a = topology.vertex_points.(vertex)
                        = index.edge_a.(edge) in
                      let vertex_a = if starts_at_a then vertex else next
                      and vertex_b = if starts_at_a then next else vertex in
                      different := vertex_a < 0 || vertex_b < 0
                        || boundary_storage_differs tolerance
                             plane.boundary_storage reference_a vertex_a
                        || boundary_storage_differs tolerance
                             plane.boundary_storage reference_b vertex_b;
                      incr slot
                    done
                | Attribute.Vertex | Attribute.Primitive | Attribute.Detail -> ()
                end;
                incr plane_index
              done;
              !different in
            let unshared_boundary edge =
              if not include_unshared_edges then false
              else
                let first = index.edge_offsets.(edge)
                and last = index.edge_offsets.(edge + 1) in
                if last - first <> 1 then false
                else
                  let vertex = index.edge_vertices.(first) in
                  let primitive = index.primitive_of_vertex.(vertex) in
                  match Char.code (Bytes.unsafe_get topology.primitive_kinds primitive) with
                  | 0 | 2 -> true
                  | 1 when include_all_unshared_curve_edges -> true
                  | 1 ->
                      let primitive_first = topology.primitive_offsets.(primitive)
                      and primitive_last = topology.primitive_offsets.(primitive + 1) in
                      vertex = primitive_first || vertex = primitive_last - 2
                  | _ -> false in
            let edge_bits = Bytes.make (Support.Bits.byte_count edge_count) '\000' in
            if Bytes.length edge_bits > 0 then
              Parallel.for_ ~chunk_size:(max 1 (grain / 8)) ~start:0
                ~finish:(Bytes.length edge_bits - 1) (fun byte ->
                  if byte land 2047 = 0 then Cancel.check_opt cancel;
                  let first = byte * 8 and value = ref 0 in
                  for bit = 0 to min 7 (edge_count - first - 1) do
                    let edge = first + bit in
                    if attribute_boundary edge || unshared_boundary edge then
                      value := !value lor (1 lsl bit)
                  done;
                  Bytes.unsafe_set edge_bits byte (Char.chr !value));
            let edge_selected edge = Support.Bits.mem edge_bits edge in
            match owner with
            | Group_edges ->
                Geometry.with_edge_group (Edge_group.Private.of_owned_bits
                  ~topology:topology_value ~edge_count ~name edge_bits) geometry
            | Group_points ->
                let group = Group.init ~grain ~owner:Group.Point ~name
                    (Geometry.point_count geometry) (fun point ->
                      if point land 16_383 = 0 then Cancel.check_opt cancel;
                      let slot = ref index.point_edge_offsets.(point)
                      and last = index.point_edge_offsets.(point + 1)
                      and found = ref false in
                      while not !found && !slot < last do
                        found := edge_selected index.point_edges.(!slot);
                        incr slot
                      done;
                      !found) in
                Geometry.with_group group geometry
            | Group_primitives ->
                let boundary_points = if include_all_primitives_sharing_boundary_points
                  then begin
                    let points = Bytes.make
                        (Support.Bits.byte_count (Geometry.point_count geometry)) '\000' in
                    if Bytes.length points > 0 then
                      Parallel.for_ ~chunk_size:(max 1 (grain / 8)) ~start:0
                        ~finish:(Bytes.length points - 1) (fun byte ->
                      if byte land 2047 = 0 then Cancel.check_opt cancel;
                      let first = byte * 8 and value = ref 0 in
                      for bit = 0 to min 7
                          (Geometry.point_count geometry - first - 1) do
                        let point = first + bit in
                        let slot = ref index.point_edge_offsets.(point)
                        and last = index.point_edge_offsets.(point + 1)
                        and found = ref false in
                        while not !found && !slot < last do
                          found := edge_selected index.point_edges.(!slot);
                          incr slot
                        done;
                        if !found then value := !value lor (1 lsl bit)
                      done;
                      Bytes.unsafe_set points byte (Char.chr !value));
                    Some points
                  end else None in
                let group = Group.init ~grain ~owner:Group.Primitive ~name
                    (Geometry.primitive_count geometry) (fun primitive ->
                      if primitive land 16_383 = 0 then Cancel.check_opt cancel;
                      let vertex = ref topology.primitive_offsets.(primitive)
                      and last = topology.primitive_offsets.(primitive + 1)
                      and found = ref false in
                      while not !found && !vertex < last do
                        (match boundary_points with
                         | Some points ->
                             found := Support.Bits.mem points topology.vertex_points.(!vertex)
                         | None ->
                             let edge = index.edge_of_vertex.(!vertex) in
                             found := edge >= 0 && edge_selected edge);
                        incr vertex
                      done;
                      !found) in
                Geometry.with_group group geometry
            | Group_vertices -> assert false
      end)

let fresh_edge_group_name geometry prefix =
  let rec find suffix =
    let name = prefix ^ string_of_int suffix in
    match Geometry.find_edge_group name geometry with
    | None -> name
    | Some _ -> find (suffix + 1) in
  find 0

let ordinary_attribute_owner = function
  | Group_points -> Attribute.Point
  | Group_vertices -> Attribute.Vertex
  | Group_primitives -> Attribute.Primitive
  | Group_edges -> invalid_arg "native edges do not own attributes"

let integer_attribute_of_selection ?cancel ~grain ~destination ~name = function
  | Native_edges _ -> Error
      "Group Promote: native edges do not own attributes"
  | Ordinary selected ->
      let count = Group.length selected in
      let values = Array.make count 0 in
      if count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
          ~finish:(count - 1) (fun element ->
            if element land 16_383 = 0 then Cancel.check_opt cancel;
            if Group.mem element selected then values.(element) <- 1);
      Attribute.create_owned ~owner:(ordinary_attribute_owner destination)
        ~name (Attribute.Int values)

let promote_boundary_selection ?cancel ~grain ~attributes ~tolerance
    ~include_unshared_edges ~include_all_unshared_curve_edges
    ~include_all_primitives_sharing_boundary_points ~source ~destination
    ~output_name source_selection geometry =
  let converted = promoted_selection ?cancel ~grain ~source ~destination
      ~mode:Include_any ~name:output_name source_selection geometry in
  let edge_name = fresh_edge_group_name geometry
      "__rdk_promote_boundary_edges_" in
  let configured_edges ?membership_boundary temporary_geometry =
    Result.bind (group_from_attribute_boundary ?cancel ~grain
        ?membership_boundary ~attributes ~tolerance ~include_unshared_edges
        ~include_all_unshared_curve_edges ~owner:Group_edges
        ~name:edge_name temporary_geometry) (fun geometry ->
      match Geometry.find_edge_group edge_name geometry with
      | Some edges -> Ok edges
      | None -> Error
          "Group Promote Boundary: internal boundary edge output missing") in
  let boundary_edges_result = match source_selection with
    | Ordinary selected ->
        let attribute_owner = ordinary_attribute_owner source in
        configured_edges
          ~membership_boundary:(attribute_owner, Ordinary selected) geometry
    | Native_edges selected ->
        if attributes = [] && not include_unshared_edges then Ok selected
        else Result.bind (configured_edges geometry) (fun configured ->
          Edge_group.union selected configured) in
  Result.bind boundary_edges_result (fun boundary_edges ->
    let boundary_selection = Native_edges boundary_edges in
    let boundary_converted =
      if destination = Group_primitives
          && include_all_primitives_sharing_boundary_points then
        let points = promoted_selection ?cancel ~grain
            ~source:Group_edges ~destination:Group_points
            ~mode:Include_any ~name:"__rdk_boundary_points"
            boundary_selection geometry in
        promoted_selection ?cancel ~grain ~source:Group_points
          ~destination:Group_primitives ~mode:Include_any
          ~name:output_name points geometry
      else promoted_selection ?cancel ~grain ~source:Group_edges
          ~destination ~mode:Include_any ~name:output_name
          boundary_selection geometry in
    match converted, boundary_converted with
    | Ordinary left, Ordinary right ->
        Result.map (fun group -> Ordinary group) (Group.intersection left right)
    | Native_edges left, Native_edges right ->
        Result.map (fun group -> Native_edges group)
          (Edge_group.intersection left right)
    | Ordinary _, Native_edges _ | Native_edges _, Ordinary _ ->
        Error "Group Promote Boundary: internal owner mismatch")

let group_promote_boundary ?cancel ?(grain = 16_384) ?name
    ?(keep_original = false) ?output_attribute ?(attributes = [])
    ?(tolerance = 1e-6) ?(include_unshared_edges = false)
    ?(include_all_unshared_curve_edges = false)
    ?(include_all_primitives_sharing_boundary_points = false)
    ~source ~destination ~group geometry =
  if grain <= 0 then Error "Group Promote Boundary: grain must be positive"
  else if String.trim group = "" then
    Error "Group Promote Boundary: empty source group name"
  else
    let output_name = Option.value ~default:group name in
    if String.trim output_name = "" then
      Error "Group Promote Boundary: empty output group name"
    else if (match output_attribute with
        | Some name -> String.trim name = "" | None -> false) then
      Error "Group Promote Boundary: empty output attribute name"
    else if output_attribute <> None && destination = Group_edges then
      Error "Group Promote Boundary: native edges do not own attributes"
    else if not (Float.is_finite tolerance) || tolerance < 0. then
      Error "Group Promote Boundary: tolerance must be finite and non-negative"
    else if include_all_unshared_curve_edges && not include_unshared_edges then
      Error "Group Promote Boundary: all curve edges require unshared edges"
    else if include_all_primitives_sharing_boundary_points
        && destination <> Group_primitives then
      Error "Group Promote Boundary: point-sharing expansion requires primitive output"
    else match find_selection source group geometry with
      | None -> Error (Printf.sprintf
          "Group Promote Boundary: missing %s group %S" (owner_name source) group)
      | Some source_selection ->
          Cancel.check_opt cancel;
          Result.bind (promote_boundary_selection ?cancel ~grain ~attributes
              ~tolerance ~include_unshared_edges
              ~include_all_unshared_curve_edges
              ~include_all_primitives_sharing_boundary_points ~source
              ~destination ~output_name source_selection geometry) (fun output ->
              let base = if keep_original then geometry
                else remove_group source group geometry in
              match output_attribute, output with
              | Some attribute_name, Ordinary selected ->
                  Result.bind (integer_attribute_of_selection ?cancel ~grain
                      ~destination ~name:attribute_name (Ordinary selected))
                    (fun attribute ->
                    Geometry.with_attribute attribute base)
              | Some _, Native_edges _ -> assert false
              | None, Ordinary group ->
                  install_group destination (Some group) None base
              | None, Native_edges group ->
                  install_group destination None (Some group) base)

type expand_normals = {
  expand_nx : float array;
  expand_ny : float array;
  expand_nz : float array;
  expand_minimum_dot : float;
}

type expand_constraints = {
  expand_seams : Edge_group.t option;
  expand_containment : selection option;
  expand_collision_boundary : selection option;
  expand_shrink_boundary : selection option;
  expand_normals : expand_normals option;
}

let expand_attribute_owner_count geometry = function
  | Attribute.Point -> Geometry.point_count geometry
  | Attribute.Vertex -> Geometry.vertex_count geometry
  | Attribute.Primitive -> Geometry.primitive_count geometry
  | Attribute.Detail -> 1

let expand_normal_source ?cancel ~grain ~owner ~normal_attribute geometry =
  let source_result = match normal_attribute with
    | Some source ->
        if source.expand_normal_owner = Attribute.Detail then
          Error "Group Expand: detail attributes cannot provide normal constraints"
        else if String.trim source.expand_normal_name = "" then
          Error "Group Expand: empty normal attribute name"
        else
          (match Geometry.find_attribute ~owner:source.expand_normal_owner
              source.expand_normal_name geometry with
           | None -> Error (Printf.sprintf
               "Group Expand: missing %s normal attribute %S"
               (match source.expand_normal_owner with
                | Attribute.Point -> "point" | Attribute.Vertex -> "vertex"
                | Attribute.Primitive -> "primitive"
                | Attribute.Detail -> "detail") source.expand_normal_name)
           | Some attribute -> Ok (source.expand_normal_owner, attribute))
    | None ->
        let attribute_owner = match owner with
          | Group_points -> Attribute.Point
          | Group_primitives -> Attribute.Primitive
          | Group_vertices | Group_edges -> assert false in
        let temporary_name = "__rdk_group_expand_geometric_normal" in
        Result.bind (Error.unguard (Normal_ops.run ?cancel ~grain ~owner:attribute_owner
            ~attribute:temporary_name geometry)) (fun temporary ->
          match Geometry.find_attribute ~owner:attribute_owner temporary_name
              temporary with
          | Some attribute -> Ok (attribute_owner, attribute)
          | None -> Error "Group Expand: internal geometric normal output missing") in
  Result.bind source_result (fun (source_owner, attribute) ->
    match Attribute.Private.storage attribute with
    | Attribute.Float3 packed ->
        let expected = expand_attribute_owner_count geometry source_owner in
        if Attribute.length attribute <> expected then Error (Printf.sprintf
          "Group Expand: normal attribute %S length does not match its owner"
          (Attribute.name attribute))
        else
          let source = Packed.Float3.Private.view packed in
          let first_invalid = Atomic.make max_int in
          if expected > 0 then Parallel.for_ ~chunk_size:grain ~start:0
              ~finish:(expected - 1) (fun element ->
                if element land 16_383 = 0 then Cancel.check_opt cancel;
                if not (Float.is_finite source.x.(element)
                    && Float.is_finite source.y.(element)
                    && Float.is_finite source.z.(element)) then begin
                  let rec lower observed =
                    if element < observed
                        && not (Atomic.compare_and_set first_invalid observed element)
                    then lower (Atomic.get first_invalid) in
                  lower (Atomic.get first_invalid)
                end);
          let invalid = Atomic.get first_invalid in
          if invalid <> max_int then Error (Printf.sprintf
            "Group Expand: normal attribute %S element %d is non-finite"
            (Attribute.name attribute) invalid)
          else Ok (source_owner, source)
    | _ -> Error (Printf.sprintf
        "Group Expand: normal attribute %S must use float3 storage"
        (Attribute.name attribute)))

let expand_normal_planes ?cancel ~grain ~owner ~spread ~normal_attribute geometry =
  Result.bind (expand_normal_source ?cancel ~grain ~owner ~normal_attribute geometry)
    (fun (source_owner, source) ->
      let topology_value = Geometry.topology geometry in
      let topology = Topology.Private.view topology_value
      and reverse = Topology_index.create ?cancel topology_value
          |> Topology_index.Private.view in
      let count = match owner with
        | Group_points -> Geometry.point_count geometry
        | Group_primitives -> Geometry.primitive_count geometry
        | Group_vertices | Group_edges -> assert false in
      let x = Array.make count 0. and y = Array.make count 0.
      and z = Array.make count 0. in
      (* Each parallel iteration owns one target slot, so the output planes are
         also allocation-free accumulation scratch. *)
      let add_source target source_index =
        let vx = source.x.(source_index) and vy = source.y.(source_index)
        and vz = source.z.(source_index) in
        let scale = Float.max (abs_float vx)
            (Float.max (abs_float vy) (abs_float vz)) in
        if scale > 0. then begin
          let nx = vx /. scale and ny = vy /. scale and nz = vz /. scale in
          let inverse = 1. /. sqrt ((nx *. nx) +. (ny *. ny) +. (nz *. nz)) in
          x.(target) <- x.(target) +. (nx *. inverse);
          y.(target) <- y.(target) +. (ny *. inverse);
          z.(target) <- z.(target) +. (nz *. inverse)
        end in
      let normalize_target target =
        let vx = x.(target) and vy = y.(target) and vz = z.(target) in
        let scale = Float.max (abs_float vx)
            (Float.max (abs_float vy) (abs_float vz)) in
        if scale > 0. then begin
          let nx = vx /. scale and ny = vy /. scale and nz = vz /. scale in
          let inverse = 1. /. sqrt ((nx *. nx) +. (ny *. ny) +. (nz *. nz)) in
          x.(target) <- nx *. inverse;
          y.(target) <- ny *. inverse;
          z.(target) <- nz *. inverse
        end in
      if count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
          ~finish:(count - 1) (fun target ->
            if target land 4095 = 0 then Cancel.check_opt cancel;
            begin match owner, source_owner with
            | Group_points, Attribute.Point
            | Group_primitives, Attribute.Primitive ->
                add_source target target
            | Group_points, Attribute.Vertex ->
                for slot = reverse.point_offsets.(target)
                    to reverse.point_offsets.(target + 1) - 1 do
                  add_source target reverse.point_vertices.(slot)
                done
            | Group_points, Attribute.Primitive ->
                for slot = reverse.point_offsets.(target)
                    to reverse.point_offsets.(target + 1) - 1 do
                  add_source target reverse.primitive_of_vertex.(
                    reverse.point_vertices.(slot))
                done
            | Group_primitives, Attribute.Point ->
                for vertex = topology.primitive_offsets.(target)
                    to topology.primitive_offsets.(target + 1) - 1 do
                  add_source target topology.vertex_points.(vertex)
                done
            | Group_primitives, Attribute.Vertex ->
                for vertex = topology.primitive_offsets.(target)
                    to topology.primitive_offsets.(target + 1) - 1 do
                  add_source target vertex
                done
            | (Group_points | Group_primitives), Attribute.Detail
            | (Group_vertices | Group_edges), _ -> assert false
            end;
            normalize_target target);
      Ok { expand_nx = x; expand_ny = y; expand_nz = z;
        expand_minimum_dot = cos spread })

let expand_extract_edge_group name geometry =
  match Geometry.find_edge_group name geometry with
  | Some edges -> Ok edges
  | None -> Error "Group Expand: internal boundary edge output missing"

let expand_attribute_edges ?cancel ~grain attributes tolerance geometry =
  if attributes = [] then Ok None
  else
    let name = fresh_edge_group_name geometry
        "__rdk_group_expand_attribute_edges_" in
    Result.bind (group_from_attribute_boundary ?cancel ~grain ~attributes
        ~tolerance ~owner:Group_edges ~name geometry) (fun temporary ->
      Result.map Option.some (expand_extract_edge_group name temporary))

let expand_collision_edges ?cancel ~grain collision geometry =
  match find_selection collision.expand_collision_owner
      collision.expand_collision_group geometry with
  | None -> Error (Printf.sprintf "Group Expand: missing %s collision group %S"
      (owner_name collision.expand_collision_owner)
      collision.expand_collision_group)
  | Some (Native_edges edges as selection) -> Ok (selection, edges)
  | Some (Ordinary _ as selection) ->
      let name = fresh_edge_group_name geometry
          "__rdk_group_expand_collision_edges_" in
      Result.bind (group_from_attribute_boundary ?cancel ~grain
          ~membership_boundary:(ordinary_attribute_owner
            collision.expand_collision_owner, selection)
          ~owner:Group_edges ~name geometry) (fun temporary ->
        Result.map (fun edges -> selection, edges)
          (expand_extract_edge_group name temporary))

let expand_promote_edges ?cancel ~grain ~owner ~name edges geometry =
  match promoted_selection ?cancel ~grain ~source:Group_edges ~destination:owner
      ~mode:Include_any ~name (Native_edges edges) geometry with
  | Ordinary _ as selection -> selection
  | Native_edges _ -> assert false

let compile_expand_constraints ?cancel ~grain ~owner ~normal_spread
    ~normal_attribute ~connectivity_attributes ~connectivity_tolerance
    ~collision ~growing geometry =
  let normals = match normal_spread, growing with
    | Some spread, true when spread < Float.pi ->
        Result.map Option.some (expand_normal_planes ?cancel ~grain ~owner
          ~spread ~normal_attribute geometry)
    | None, _ | Some _, false | Some _, true -> Ok None in
  let attribute_edges = expand_attribute_edges ?cancel ~grain
      connectivity_attributes connectivity_tolerance geometry in
  let collision_result = match collision with
    | None -> Ok None
    | Some collision -> Result.map Option.some
        (expand_collision_edges ?cancel ~grain collision geometry) in
  Result.bind normals (fun normals ->
    Result.bind attribute_edges (fun attribute_edges ->
      Result.bind collision_result (fun collision_result ->
        let collision_edges = Option.map snd collision_result in
        let seams = match attribute_edges, collision_edges with
          | None, None -> Ok None
          | Some edges, None | None, Some edges -> Ok (Some edges)
          | Some attributes, Some collision ->
              Result.map Option.some (Edge_group.union attributes collision) in
        Result.map (fun seams ->
          let seams = match seams with
            | Some edges when Edge_group.cardinality edges = 0 -> None
            | None | Some _ as value -> value in
          let shrink_boundary = Option.map (fun edges ->
            expand_promote_edges ?cancel ~grain ~owner
              ~name:"__rdk_group_expand_shrink_boundary" edges geometry) seams in
          let collision_boundary = Option.map (fun (_, edges) ->
            expand_promote_edges ?cancel ~grain ~owner
              ~name:"__rdk_group_expand_collision_boundary" edges geometry)
              collision_result in
          let containment = match collision, collision_result with
            | Some collision, Some (selection, _)
                when collision.expand_collision_contain ->
                Some (promoted_selection ?cancel ~grain
                  ~source:collision.expand_collision_owner ~destination:owner
                  ~mode:Include_any ~name:"__rdk_group_expand_containment"
                  selection geometry)
            | None, _ | Some _, None | Some _, Some _ -> None in
          { expand_seams = seams; expand_containment = containment;
            expand_collision_boundary = collision_boundary;
            expand_shrink_boundary = shrink_boundary;
            expand_normals = normals }) seams)))

let expand_candidate_allowed constraints collision element =
  (match constraints.expand_containment with
   | None -> true | Some selection -> selection_mem selection element)
  && (match collision, constraints.expand_collision_boundary with
      | Some collision, Some boundary
          when not collision.expand_collision_allow_boundary ->
          not (selection_mem boundary element)
      | None, _ | Some _, None | Some _, Some _ -> true)

let expand_transition_allowed constraints edge element neighbor =
  (match constraints.expand_seams with
   | Some seams when edge >= 0 -> not (Edge_group.mem edge seams)
   | None | Some _ -> true)
  && match constraints.expand_normals with
     | None -> true
     | Some normals ->
         let ax = normals.expand_nx.(element)
         and ay = normals.expand_ny.(element)
         and az = normals.expand_nz.(element)
         and bx = normals.expand_nx.(neighbor)
         and by = normals.expand_ny.(neighbor)
         and bz = normals.expand_nz.(neighbor) in
         let a_nonzero = ax <> 0. || ay <> 0. || az <> 0.
         and b_nonzero = bx <> 0. || by <> 0. || bz <> 0. in
         a_nonzero && b_nonzero
         && ((ax *. bx) +. (ay *. by) +. (az *. bz))
            >= normals.expand_minimum_dot -. (64. *. Float.epsilon)

let expand_forced_boundary constraints element =
  match constraints.expand_shrink_boundary with
  | None -> false
  | Some boundary -> selection_mem boundary element

let neighbor_matches ~owner ~primitive_connectivity
    (topology : Topology.Private.view)
    (reverse : Topology_index.Private.view) bits wanted element =
  let found = ref false in
  match owner with
  | Group_points ->
      let slot = ref reverse.point_edge_offsets.(element)
      and last = reverse.point_edge_offsets.(element + 1) in
      while not !found && !slot < last do
        let edge = reverse.point_edges.(!slot) in
        let a = reverse.edge_a.(edge) and b = reverse.edge_b.(edge) in
        let neighbor = if a = element then b else a in
        found := neighbor <> element && Support.Bits.mem bits neighbor = wanted;
        incr slot
      done;
      !found
  | Group_edges ->
      let point = ref reverse.edge_a.(element) and endpoint = ref 0 in
      while not !found && !endpoint < 2 do
        let slot = ref reverse.point_edge_offsets.(!point)
        and last = reverse.point_edge_offsets.(!point + 1) in
        while not !found && !slot < last do
          let neighbor = reverse.point_edges.(!slot) in
          found := neighbor <> element && Support.Bits.mem bits neighbor = wanted;
          incr slot
        done;
        incr endpoint;
        point := reverse.edge_b.(element);
        if reverse.edge_b.(element) = reverse.edge_a.(element) then endpoint := 2
      done;
      !found
  | Group_vertices ->
      let next = reverse.next_vertex.(element)
      and previous = reverse.previous_vertex.(element) in
      if next >= 0 && next <> element && Support.Bits.mem bits next = wanted then
        found := true;
      if not !found && previous >= 0 && previous <> element
          && Support.Bits.mem bits previous = wanted then found := true;
      if not !found then begin
        let point = topology.vertex_points.(element) in
        let slot = ref reverse.point_offsets.(point)
        and last = reverse.point_offsets.(point + 1) in
        while not !found && !slot < last do
          let neighbor = reverse.point_vertices.(!slot) in
          found := neighbor <> element && Support.Bits.mem bits neighbor = wanted;
          incr slot
        done
      end;
      !found
  | Group_primitives ->
      let vertex = ref topology.primitive_offsets.(element)
      and last = topology.primitive_offsets.(element + 1) in
      while not !found && !vertex < last do
        match primitive_connectivity with
        | Primitive_share_points ->
            let point = topology.vertex_points.(!vertex) in
            let slot = ref reverse.point_offsets.(point)
            and last = reverse.point_offsets.(point + 1) in
            while not !found && !slot < last do
              let neighbor = reverse.primitive_of_vertex.(
                  reverse.point_vertices.(!slot)) in
              found := neighbor <> element && Support.Bits.mem bits neighbor = wanted;
              incr slot
            done;
            incr vertex
        | Primitive_share_edges ->
            let edge = reverse.edge_of_vertex.(!vertex) in
            if edge >= 0 then begin
              let slot = ref reverse.edge_offsets.(edge)
              and last = reverse.edge_offsets.(edge + 1) in
              while not !found && !slot < last do
                let neighbor = reverse.primitive_of_vertex.(
                    reverse.edge_vertices.(!slot)) in
                found := neighbor <> element && Support.Bits.mem bits neighbor = wanted;
                incr slot
              done
            end;
            incr vertex
      done;
      !found

let enqueue_new bits queue tail step_values element neighbor =
  if neighbor <> element && not (Support.Bits.mem bits neighbor) then begin
    Support.Bits.set bits neighbor;
    (match step_values with
     | None -> ()
     | Some values -> values.(neighbor) <- values.(element) + 1);
    queue.(!tail) <- neighbor;
    incr tail
  end

let flood_neighbors ~owner ~primitive_connectivity
    (topology : Topology.Private.view)
    (reverse : Topology_index.Private.view) bits queue tail step_values element =
  match owner with
  | Group_points ->
      let first = reverse.point_edge_offsets.(element)
      and last = reverse.point_edge_offsets.(element + 1) in
      for slot = first to last - 1 do
        let edge = reverse.point_edges.(slot) in
        let a = reverse.edge_a.(edge) and b = reverse.edge_b.(edge) in
        enqueue_new bits queue tail step_values element
          (if a = element then b else a)
      done
  | Group_edges ->
      let a = reverse.edge_a.(element) and b = reverse.edge_b.(element) in
      let first = reverse.point_edge_offsets.(a)
      and last = reverse.point_edge_offsets.(a + 1) in
      for slot = first to last - 1 do
        enqueue_new bits queue tail step_values element reverse.point_edges.(slot)
      done;
      if b <> a then begin
        let first = reverse.point_edge_offsets.(b)
        and last = reverse.point_edge_offsets.(b + 1) in
        for slot = first to last - 1 do
          enqueue_new bits queue tail step_values element reverse.point_edges.(slot)
        done
      end
  | Group_vertices ->
      let next = reverse.next_vertex.(element)
      and previous = reverse.previous_vertex.(element) in
      if next >= 0 then enqueue_new bits queue tail step_values element next;
      if previous >= 0 then
        enqueue_new bits queue tail step_values element previous;
      let point = topology.vertex_points.(element) in
      let first = reverse.point_offsets.(point)
      and last = reverse.point_offsets.(point + 1) in
      for slot = first to last - 1 do
        enqueue_new bits queue tail step_values element
          reverse.point_vertices.(slot)
      done
  | Group_primitives ->
      let first = topology.primitive_offsets.(element)
      and last = topology.primitive_offsets.(element + 1) in
      for vertex = first to last - 1 do
        match primitive_connectivity with
        | Primitive_share_points ->
            let point = topology.vertex_points.(vertex) in
            let first = reverse.point_offsets.(point)
            and last = reverse.point_offsets.(point + 1) in
            for slot = first to last - 1 do
              enqueue_new bits queue tail step_values element
                reverse.primitive_of_vertex.(reverse.point_vertices.(slot))
            done
        | Primitive_share_edges ->
            let edge = reverse.edge_of_vertex.(vertex) in
            if edge >= 0 then begin
              let first = reverse.edge_offsets.(edge)
              and last = reverse.edge_offsets.(edge + 1) in
              for slot = first to last - 1 do
                enqueue_new bits queue tail step_values element
                  reverse.primitive_of_vertex.(reverse.edge_vertices.(slot))
              done
            end
      done

let flood_bits ?cancel ~owner ~primitive_connectivity topology reverse count bits
    step_values =
  let queue = Array.make count 0 and head = ref 0 and tail = ref 0 in
  for element = 0 to count - 1 do
    if element land 16_383 = 0 then Cancel.check_opt cancel;
    if Support.Bits.mem bits element then begin
      queue.(!tail) <- element;
      incr tail
    end
  done;
  while !head < !tail do
    if !head land 16_383 = 0 then Cancel.check_opt cancel;
    let element = queue.(!head) in
    incr head;
    flood_neighbors ~owner ~primitive_connectivity topology reverse bits queue
      tail step_values element
  done;
  bits

let stepped_bits ?cancel ~grain ~owner ~primitive_connectivity topology reverse
    count steps initial step_values =
  let growing = steps > 0 and iterations = abs steps in
  let current = ref initial and iteration = ref 0 and stable = ref false in
  while !iteration < iterations && not !stable do
    Cancel.check_opt cancel;
    let before = !current in
    let step = !iteration + 1 in
    let classify selected element =
      if growing then begin
        if selected then true
        else begin
          neighbor_matches ~owner ~primitive_connectivity topology reverse
            before true element
        end
      end else if not selected then false
      else begin
        not (neighbor_matches ~owner ~primitive_connectivity topology reverse
          before false element)
      end in
    let next = match step_values with
      | None -> packed_init ?cancel ~grain count (fun element ->
          classify (Support.Bits.mem before element) element)
      | Some values -> packed_init ?cancel ~grain count (fun element ->
          let selected = Support.Bits.mem before element in
          let result = classify selected element in
          if selected <> result then values.(element) <- step;
          result) in
    stable := Bytes.equal before next;
    current := next;
    incr iteration
  done;
  !current

let grow_point_depth_bits ?cancel reverse bits cardinality depth step_values =
  let count = Array.length reverse.Topology_index.Private.point_edge_offsets - 1 in
  let initial_capacity = if depth = max_int then count
    else min count (max 16 cardinality) in
  let queue = ref (Array.make initial_capacity 0)
  and head = ref 0 and tail = ref 0 in
  let push point =
    if !tail = Array.length !queue then begin
      let old_capacity = Array.length !queue in
      let capacity = old_capacity
          + min (max 1 old_capacity) (count - old_capacity) in
      let grown = Array.make capacity 0 in
      Array.blit !queue 0 grown 0 !tail;
      queue := grown
    end;
    (!queue).(!tail) <- point;
    incr tail in
  for point = 0 to count - 1 do
    if point land 16_383 = 0 then Cancel.check_opt cancel;
    if Support.Bits.mem bits point then push point
  done;
  let level = ref 0 in
  while !level < depth && !head < !tail do
    Cancel.check_opt cancel;
    let level_end = !tail in
    while !head < level_end do
      if !head land 16_383 = 0 then Cancel.check_opt cancel;
      let point = (!queue).(!head) in
      incr head;
      let first = reverse.point_edge_offsets.(point)
      and last = reverse.point_edge_offsets.(point + 1) in
      for slot = first to last - 1 do
        if slot land 16_383 = 0 then Cancel.check_opt cancel;
        let edge = reverse.point_edges.(slot) in
        let a = reverse.edge_a.(edge) and b = reverse.edge_b.(edge) in
        let neighbor = if a = point then b else a in
        if neighbor <> point && not (Support.Bits.mem bits neighbor) then begin
          Support.Bits.set bits neighbor;
          (match step_values with
           | None -> ()
           | Some values -> values.(neighbor) <- values.(point) + 1);
          push neighbor
        end
      done
    done;
    incr level
  done;
  bits

let constrained_neighbor_matches ~owner ~primitive_connectivity topology reverse
    constraints bits wanted element =
  let found = ref false in
  match owner with
  | Group_points ->
      let slot = ref reverse.Topology_index.Private.point_edge_offsets.(element)
      and last = reverse.point_edge_offsets.(element + 1) in
      while not !found && !slot < last do
        let edge = reverse.point_edges.(!slot) in
        let a = reverse.edge_a.(edge) and b = reverse.edge_b.(edge) in
        let neighbor = if a = element then b else a in
        found := neighbor <> element && Support.Bits.mem bits neighbor = wanted
          && expand_transition_allowed constraints edge element neighbor;
        incr slot
      done;
      !found
  | Group_primitives ->
      let vertex = ref topology.Topology.Private.primitive_offsets.(element)
      and last = topology.primitive_offsets.(element + 1) in
      while not !found && !vertex < last do
        begin match primitive_connectivity with
        | Primitive_share_points ->
            let point = topology.vertex_points.(!vertex) in
            let slot = ref reverse.point_offsets.(point)
            and last = reverse.point_offsets.(point + 1) in
            while not !found && !slot < last do
              let neighbor = reverse.primitive_of_vertex.(
                  reverse.point_vertices.(!slot)) in
              found := neighbor <> element && Support.Bits.mem bits neighbor = wanted
                && expand_transition_allowed constraints (-1) element neighbor;
              incr slot
            done
        | Primitive_share_edges ->
            let edge = reverse.edge_of_vertex.(!vertex) in
            if edge >= 0 then begin
              let slot = ref reverse.edge_offsets.(edge)
              and last = reverse.edge_offsets.(edge + 1) in
              while not !found && !slot < last do
                let neighbor = reverse.primitive_of_vertex.(
                    reverse.edge_vertices.(!slot)) in
                found := neighbor <> element && Support.Bits.mem bits neighbor = wanted
                  && expand_transition_allowed constraints edge element neighbor;
                incr slot
              done
            end
        end;
        incr vertex
      done;
      !found
  | Group_vertices | Group_edges -> assert false

let constrained_stepped_bits ?cancel ~grain ~owner ~primitive_connectivity
    topology reverse constraints collision count steps initial step_values =
  let growing = steps > 0 and iterations = abs steps in
  let current = ref initial and iteration = ref 0 and stable = ref false in
  while !iteration < iterations && not !stable do
    Cancel.check_opt cancel;
    let before = !current and step = !iteration + 1 in
    let classify selected element =
      if growing then
        selected || expand_candidate_allowed constraints collision element
          && constrained_neighbor_matches ~owner ~primitive_connectivity
               topology reverse constraints before true element
      else if not selected then false
      else not (expand_forced_boundary constraints element)
        && not (constrained_neighbor_matches ~owner ~primitive_connectivity
          topology reverse constraints before false element) in
    let next = match step_values with
      | None -> packed_init ?cancel ~grain count (fun element ->
          classify (Support.Bits.mem before element) element)
      | Some values -> packed_init ?cancel ~grain count (fun element ->
          let selected = Support.Bits.mem before element in
          let result = classify selected element in
          if selected <> result then values.(element) <- step;
          result) in
    stable := Bytes.equal before next;
    current := next;
    incr iteration
  done;
  !current

let constrained_enqueue constraints collision bits queue tail step_values
    element neighbor edge =
  if neighbor <> element && not (Support.Bits.mem bits neighbor)
      && expand_candidate_allowed constraints collision neighbor
      && expand_transition_allowed constraints edge element neighbor then begin
    Support.Bits.set bits neighbor;
    (match step_values with
     | None -> ()
     | Some values -> values.(neighbor) <- values.(element) + 1);
    queue.(!tail) <- neighbor;
    incr tail
  end

let constrained_flood_bits ?cancel ~owner ~primitive_connectivity topology reverse
    constraints collision count bits step_values =
  let queue = Array.make count 0 and head = ref 0 and tail = ref 0 in
  for element = 0 to count - 1 do
    if element land 16_383 = 0 then Cancel.check_opt cancel;
    if Support.Bits.mem bits element then begin
      queue.(!tail) <- element;
      incr tail
    end
  done;
  while !head < !tail do
    if !head land 16_383 = 0 then Cancel.check_opt cancel;
    let element = queue.(!head) in
    incr head;
    begin match owner with
    | Group_points ->
        for slot = reverse.Topology_index.Private.point_edge_offsets.(element)
            to reverse.point_edge_offsets.(element + 1) - 1 do
          let edge = reverse.point_edges.(slot) in
          let a = reverse.edge_a.(edge) and b = reverse.edge_b.(edge) in
          constrained_enqueue constraints collision bits queue tail step_values
            element (if a = element then b else a) edge
        done
    | Group_primitives ->
        for vertex = topology.Topology.Private.primitive_offsets.(element)
            to topology.primitive_offsets.(element + 1) - 1 do
          begin match primitive_connectivity with
          | Primitive_share_points ->
              let point = topology.vertex_points.(vertex) in
              for slot = reverse.point_offsets.(point)
                  to reverse.point_offsets.(point + 1) - 1 do
                constrained_enqueue constraints collision bits queue tail
                  step_values element
                  reverse.primitive_of_vertex.(reverse.point_vertices.(slot)) (-1)
              done
          | Primitive_share_edges ->
              let edge = reverse.edge_of_vertex.(vertex) in
              if edge >= 0 then
                for slot = reverse.edge_offsets.(edge)
                    to reverse.edge_offsets.(edge + 1) - 1 do
                  constrained_enqueue constraints collision bits queue tail
                    step_values element
                    reverse.primitive_of_vertex.(reverse.edge_vertices.(slot)) edge
                done
          end
        done
    | Group_vertices | Group_edges -> assert false
    end
  done;
  bits

let constrained_grow_point_depth_bits ?cancel reverse constraints collision bits
    cardinality depth step_values =
  let count = Array.length reverse.Topology_index.Private.point_edge_offsets - 1 in
  let initial_capacity = if depth = max_int then count
    else min count (max 16 cardinality) in
  let queue = ref (Array.make initial_capacity 0)
  and head = ref 0 and tail = ref 0 in
  let push point =
    if !tail = Array.length !queue then begin
      let old_capacity = Array.length !queue in
      let capacity = old_capacity
          + min (max 1 old_capacity) (count - old_capacity) in
      let grown = Array.make capacity 0 in
      Array.blit !queue 0 grown 0 !tail;
      queue := grown
    end;
    (!queue).(!tail) <- point;
    incr tail in
  for point = 0 to count - 1 do
    if point land 16_383 = 0 then Cancel.check_opt cancel;
    if Support.Bits.mem bits point then push point
  done;
  let level = ref 0 in
  while !level < depth && !head < !tail do
    Cancel.check_opt cancel;
    let level_end = !tail in
    while !head < level_end do
      if !head land 16_383 = 0 then Cancel.check_opt cancel;
      let point = (!queue).(!head) in
      incr head;
      for slot = reverse.point_edge_offsets.(point)
          to reverse.point_edge_offsets.(point + 1) - 1 do
        if slot land 16_383 = 0 then Cancel.check_opt cancel;
        let edge = reverse.point_edges.(slot) in
        let a = reverse.edge_a.(edge) and b = reverse.edge_b.(edge) in
        let neighbor = if a = point then b else a in
        if neighbor <> point && not (Support.Bits.mem bits neighbor)
            && expand_candidate_allowed constraints collision neighbor
            && expand_transition_allowed constraints edge point neighbor then begin
          Support.Bits.set bits neighbor;
          (match step_values with
           | None -> ()
           | Some values -> values.(neighbor) <- values.(point) + 1);
          push neighbor
        end
      done
    done;
    incr level
  done;
  bits

let expand ?cancel ?(grain = 16_384) ?name ?(steps = 1) ?(flood = false)
    ?step_attribute ?(primitive_connectivity = Primitive_share_points)
    ?normal_spread ?normal_attribute ?(connectivity_attributes = [])
    ?(connectivity_tolerance = 1e-6) ?collision ~owner ~group geometry =
  if grain <= 0 then Error "Group Expand: grain must be positive"
  else if String.trim group = "" then Error "Group Expand: empty base group name"
  else if steps = min_int then
    Error "Group Expand: steps magnitude exceeds the supported integer range"
  else if (match step_attribute with
      | Some name -> String.trim name = "" | None -> false) then
    Error "Group Expand: empty step attribute name"
  else if step_attribute <> None && owner = Group_edges then
    Error "Group Expand: native edges do not own attributes"
  else if flood && steps < 0 then
    Error "Group Expand: flood fill cannot be combined with shrinking"
  else if (match normal_spread with
      | Some spread -> not (Float.is_finite spread) || spread < 0.
          || spread > Float.pi
      | None -> false) then
    Error "Group Expand: normal spread must be finite and in [0, pi] radians"
  else if normal_attribute <> None && normal_spread = None then
    Error "Group Expand: a normal attribute requires a normal spread"
  else if (match normal_attribute with
      | Some value -> value.expand_normal_owner = Attribute.Detail
      | None -> false) then
    Error "Group Expand: detail attributes cannot provide normal constraints"
  else if not (Float.is_finite connectivity_tolerance)
      || connectivity_tolerance < 0. then
    Error "Group Expand: connectivity tolerance must be finite and non-negative"
  else if (match collision with
      | Some value -> String.trim value.expand_collision_group = ""
      | None -> false) then
    Error "Group Expand: empty collision group name"
  else if (match collision with
      | Some value -> value.expand_collision_contain
          && value.expand_collision_owner = Group_edges
      | None -> false) then
    Error "Group Expand: edge collision groups cannot define containment"
  else if (normal_spread <> None || connectivity_attributes <> []
      || collision <> None)
      && owner <> Group_points && owner <> Group_primitives then
    Error "Group Expand: constraints currently require point or primitive groups"
  else if (connectivity_attributes <> [] || collision <> None)
      && owner = Group_primitives
      && primitive_connectivity <> Primitive_share_edges then
    Error "Group Expand: constrained primitives must share edges"
  else begin
    let output_name = Option.value ~default:group name in
    if String.trim output_name = "" then Error "Group Expand: empty output group name"
    else begin
      match find_selection owner group geometry with
      | None -> Error (Printf.sprintf "Group Expand: missing %s group %S"
          (owner_name owner) group)
      | Some source ->
          Cancel.check_opt cancel;
          let count = selection_length source in
          let cardinality = selection_cardinality source in
          let step_values = Option.map (fun _ -> Array.make count 0)
              step_attribute in
          let growing = flood || steps > 0 in
          let effective_constraints =
            (match normal_spread with
             | Some spread -> growing && spread < Float.pi
             | None -> false)
            || connectivity_attributes <> [] || collision <> None in
          let constraints_result = if not effective_constraints || steps = 0 then
              Ok None
            else Result.map Option.some (compile_expand_constraints ?cancel
              ~grain ~owner ~normal_spread ~normal_attribute
              ~connectivity_attributes ~connectivity_tolerance ~collision
              ~growing geometry) in
          Result.bind constraints_result (fun constraints ->
          let shrink_constraints = not growing && match constraints with
            | Some value -> value.expand_shrink_boundary <> None
            | None -> false in
          let group_result = if steps = 0 || cardinality = 0
              || cardinality = count && not shrink_constraints then begin
              let ordinary, edges = renamed_selection output_name source in
              install_group owner ordinary edges geometry
            end else begin
            let topology_value = Geometry.topology geometry in
            let index = Topology_index.create ?cancel topology_value in
            let expected_count = owner_count geometry index owner in
            if expected_count <> count then
              Error "Group Expand: source group length does not match topology"
            else begin
              let initial = packed_init ?cancel ~grain count
                  (selection_mem source) in
              let topology = Topology.Private.view topology_value
              and reverse = Topology_index.Private.view index in
              let bits = match constraints with
                | None ->
                    if owner = Group_points && growing then
                      grow_point_depth_bits ?cancel reverse initial cardinality
                        (if flood then max_int else steps) step_values
                    else if flood then
                      flood_bits ?cancel ~owner ~primitive_connectivity topology
                        reverse count initial step_values
                    else stepped_bits ?cancel ~grain ~owner ~primitive_connectivity
                        topology reverse count steps initial step_values
                | Some constraints ->
                    if owner = Group_points && growing then
                      constrained_grow_point_depth_bits ?cancel reverse constraints
                        collision initial cardinality
                        (if flood then max_int else steps) step_values
                    else if flood then
                      constrained_flood_bits ?cancel ~owner ~primitive_connectivity
                        topology reverse constraints collision count initial step_values
                    else constrained_stepped_bits ?cancel ~grain ~owner
                        ~primitive_connectivity topology reverse constraints collision
                        count steps initial step_values in
              (match ordinary_owner owner with
               | Some group_owner ->
                   Geometry.with_group
                     (Group.Private.of_owned_bits ~owner:group_owner
                        ~name:output_name ~length:count bits)
                     geometry
               | None ->
                   Geometry.with_edge_group
                     (Edge_group.Private.of_owned_bits ~topology:topology_value
                        ~edge_count:count ~name:output_name bits)
                     geometry)
            end
          end in
          Result.bind group_result (fun geometry -> match step_attribute,
              step_values, ordinary_owner owner with
            | Some name, Some values, Some group_owner ->
                let attribute_owner = match group_owner with
                  | Group.Point -> Attribute.Point
                  | Group.Vertex -> Attribute.Vertex
                  | Group.Primitive -> Attribute.Primitive in
                Result.bind (Attribute.create_owned ~name ~owner:attribute_owner
                  (Attribute.Int values)) (fun attribute ->
                    Geometry.with_attribute attribute geometry)
            | None, None, _ -> Ok geometry
            | _ -> Error "Group Expand: internal step-attribute owner mismatch"))
    end
  end

(* Named-group metadata and boolean operations intentionally share one mutable
   table local to a cook. Public geometry and group values remain immutable;
   this prevents a many-rule node from rebuilding and rescanning Geometry.t
   after every metadata edit. *)

type named_entry = {
  owner : owner;
  mutable entry_name : string;
  mutable value : selection;
  mutable alive : bool;
}

type group_store = {
  table : ((owner * string), named_entry) Hashtbl.t;
  initial : named_entry array;
  mutable added_rev : named_entry list;
  mutable dirty : bool;
}

let owner_of_group = function
  | Group.Point -> Group_points
  | Group.Vertex -> Group_vertices
  | Group.Primitive -> Group_primitives

let selection_with_name name = function
  | Ordinary group -> Ordinary (Group.with_name name group)
  | Native_edges group -> Native_edges (Edge_group.with_name name group)

let store_of_geometry geometry =
  let ordinary = Geometry.groups geometry and edges = Geometry.edge_groups geometry in
  let entries_rev = ref [] in
  List.iter (fun group -> entries_rev := {
      owner = owner_of_group (Group.owner group); entry_name = Group.name group;
      value = Ordinary group; alive = true } :: !entries_rev) ordinary;
  List.iter (fun group -> entries_rev := {
      owner = Group_edges; entry_name = Edge_group.name group;
      value = Native_edges group; alive = true } :: !entries_rev) edges;
  let initial = Array.of_list (List.rev !entries_rev) in
  let table = Hashtbl.create (max 16 (Array.length initial * 2)) in
  Array.iter (fun entry ->
    Hashtbl.add table (entry.owner, entry.entry_name) entry) initial;
  { table; initial; added_rev = []; dirty = false }

let store_entries store =
  Array.to_list store.initial @ List.rev store.added_rev

let store_find store owner name =
  match Hashtbl.find_opt store.table (owner, name) with
  | Some entry when entry.alive -> Some entry
  | None | Some _ -> None

let store_remove store entry =
  if entry.alive then begin
    Hashtbl.remove store.table (entry.owner, entry.entry_name);
    entry.alive <- false;
    store.dirty <- true
  end

let store_replace store entry value =
  entry.value <- selection_with_name entry.entry_name value;
  store.dirty <- true

let store_add store owner name value =
  if Hashtbl.mem store.table (owner, name) then
    invalid_arg "Group operation: duplicate local group-store key";
  let entry = { owner; entry_name = name;
    value = selection_with_name name value; alive = true } in
  Hashtbl.add store.table (owner, name) entry;
  store.added_rev <- entry :: store.added_rev;
  store.dirty <- true;
  entry

let store_rename store entry name value =
  Hashtbl.remove store.table (entry.owner, entry.entry_name);
  entry.entry_name <- name;
  entry.value <- selection_with_name name value;
  Hashtbl.replace store.table (entry.owner, name) entry;
  store.dirty <- true

let store_commit store geometry =
  if not store.dirty then Ok geometry
  else begin
    let groups_rev = ref [] and edges_rev = ref [] in
    List.iter (fun entry -> if entry.alive then match entry.value with
      | Ordinary group -> groups_rev := group :: !groups_rev
      | Native_edges group -> edges_rev := group :: !edges_rev)
      (store_entries store);
    Geometry.create ~positions:(Geometry.positions geometry)
      ~topology:(Geometry.topology geometry)
      ~attributes:(Geometry.attributes geometry)
      ~groups:(List.rev !groups_rev) ~edge_groups:(List.rev !edges_rev) ()
  end

let validate_promotion_operation rule =
  if rule.promotion_output_as_attribute
      && rule.promotion_destination = Group_edges then
    Error "Group Promotions: native edges do not own attributes"
  else match rule.promotion_operation with
    | Promote_elements Include_shared_edge
      when rule.promotion_destination <> Group_primitives ->
        Error "Group Promotions: shared-edge inclusion requires primitive output"
    | Promote_elements Include_all
      when rule.promotion_destination = Group_points
        && rule.promotion_source <> Group_points ->
        Error "Group Promotions: entirely-contained mode is unavailable for point output"
    | Promote_elements (Include_any | Include_all | Include_shared_edge) -> Ok ()
    | Promote_boundary options ->
        if not (Float.is_finite options.promote_boundary_tolerance)
            || options.promote_boundary_tolerance < 0. then
          Error "Group Promotions: boundary tolerance must be finite and non-negative"
        else if options.promote_include_all_unshared_curve_edges
            && not options.promote_include_unshared_edges then
          Error "Group Promotions: all curve edges require unshared edges"
        else if options.promote_include_all_primitives_sharing_boundary_points
            && rule.promotion_destination <> Group_primitives then
          Error "Group Promotions: point-sharing expansion requires primitive output"
        else
          List.fold_left (fun result boundary -> Result.bind result (fun () ->
            if boundary.boundary_attribute_owner = Attribute.Detail then
              Error "Group Promotions: detail attributes have no topology boundary"
            else Result.map (fun _ -> ())
                (Attribute_pattern.compile
                  boundary.boundary_attribute_pattern)))
            (Ok ()) options.promote_boundary_attributes

let promotions ?cancel ?(grain = 16_384) ?(max_outputs = 4_096)
    ?(max_payload_bytes = 268_435_456) rules geometry =
  if grain <= 0 then Error "Group Promotions: grain must be positive"
  else if max_outputs < 0 then
    Error "Group Promotions: max_outputs must be non-negative"
  else if max_payload_bytes < 0 then
    Error "Group Promotions: max_payload_bytes must be non-negative"
  else begin
    Cancel.check_opt cancel;
    let store = store_of_geometry geometry in
    let generated_attributes_rev = ref []
    and generated_outputs = ref 0
    and generated_payload = ref 0 in
    let destination_count destination = match destination with
      | Group_points -> Geometry.point_count geometry
      | Group_vertices -> Geometry.vertex_count geometry
      | Group_primitives -> Geometry.primitive_count geometry
      | Group_edges -> Topology_index.edge_count
          (Topology_index.create ?cancel (Geometry.topology geometry)) in
    let reserve_output rule =
      if !generated_outputs >= max_outputs then Error (Printf.sprintf
          "Group Promotions: outputs exceed max_outputs=%d" max_outputs)
      else
        let count = destination_count rule.promotion_destination in
        let bytes_per_element = Sys.word_size / 8 in
        let payload = if rule.promotion_output_as_attribute then begin
            if count > max_int / bytes_per_element then max_int
            else count * bytes_per_element
          end else Support.Bits.byte_count count in
        if payload > max_payload_bytes - !generated_payload then
          Error (Printf.sprintf
            "Group Promotions: generated payload exceeds max_payload_bytes=%d"
            max_payload_bytes)
        else begin
          incr generated_outputs;
          generated_payload := !generated_payload + payload;
          Ok ()
        end in
    let remove_captured_source rule entry source_value =
      if not rule.promotion_keep_original then
        match store_find store entry.owner entry.entry_name with
        | Some current when current == entry && current.value == source_value ->
            store_remove store current
        | None | Some _ -> () in
    let publish_group rule output_name output =
      match store_find store rule.promotion_destination output_name with
      | Some entry -> store_replace store entry output
      | None -> ignore (store_add store rule.promotion_destination output_name output) in
    let apply_rule rule =
      let pattern_source = String.trim rule.promotion_pattern in
      if String.equal pattern_source "" then Ok ()
      else Result.bind (validate_promotion_operation rule) (fun () ->
        Result.bind (Attribute_pattern.compile pattern_source) (fun pattern ->
          let replacement_source = Option.bind rule.promotion_new_name
              (fun replacement ->
                let replacement = String.trim replacement in
                if String.equal replacement "" then None else Some replacement) in
          let rewrite = match replacement_source with
            | None -> Ok None
            | Some replacement -> Result.map Option.some
                (Attribute_pattern.compile_rewrite_set
                  ~pattern:pattern_source ~replacement) in
          Result.bind rewrite (fun rewrite ->
            let snapshot = List.filter_map (fun entry ->
              if entry.alive && entry.owner = rule.promotion_source
                  && Attribute_pattern.matches pattern entry.entry_name then
                Some (entry, entry.entry_name, entry.value)
              else None) (store_entries store) in
            List.fold_left (fun result (entry, source_name, source_value) ->
              Result.bind result (fun () ->
                Cancel.check_opt cancel;
                let output_name = match rewrite with
                  | None -> source_name
                  | Some rules ->
                      (match Attribute_pattern.rewrite_set rules source_name with
                       | Some name -> name
                       | None -> invalid_arg
                           "Group Promotions: internal rewrite mismatch") in
                if String.trim output_name = "" then
                  Error "Group Promotions: rewrite produced an empty output name"
                else Result.bind (reserve_output rule) (fun () ->
                  let output = match rule.promotion_operation with
                    | Promote_elements mode -> Ok (promoted_selection ?cancel
                        ~grain ~source:rule.promotion_source
                        ~destination:rule.promotion_destination ~mode
                        ~name:output_name source_value geometry)
                    | Promote_boundary options ->
                        promote_boundary_selection ?cancel ~grain
                          ~attributes:options.promote_boundary_attributes
                          ~tolerance:options.promote_boundary_tolerance
                          ~include_unshared_edges:
                            options.promote_include_unshared_edges
                          ~include_all_unshared_curve_edges:
                            options.promote_include_all_unshared_curve_edges
                          ~include_all_primitives_sharing_boundary_points:
                            options.promote_include_all_primitives_sharing_boundary_points
                          ~source:rule.promotion_source
                          ~destination:rule.promotion_destination
                          ~output_name source_value geometry in
                  Result.bind output (fun output ->
                    remove_captured_source rule entry source_value;
                    if rule.promotion_output_as_attribute then
                      Result.bind (integer_attribute_of_selection ?cancel ~grain
                          ~destination:rule.promotion_destination ~name:output_name
                          output) (fun attribute ->
                        generated_attributes_rev := attribute
                          :: !generated_attributes_rev;
                        Ok ())
                    else begin
                      publish_group rule output_name output;
                      Ok ()
                    end)))
              ) (Ok ()) snapshot))) in
    let result = List.fold_left (fun result rule ->
      Result.bind result (fun () -> apply_rule rule)) (Ok ()) rules in
    Result.bind result (fun () ->
      Result.bind (store_commit store geometry) (fun geometry ->
        match !generated_attributes_rev with
        | [] -> Ok geometry
        | attributes -> Geometry.Private.with_merged_attributes_owned
            (Array.of_list (List.rev attributes)) geometry))
  end

let valid_group_name name =
  let length = String.length name in
  let ascii_letter byte =
    (byte >= Char.code 'a' && byte <= Char.code 'z')
    || (byte >= Char.code 'A' && byte <= Char.code 'Z') in
  let ascii_digit byte = byte >= Char.code '0' && byte <= Char.code '9' in
  length > 0 && not (ascii_digit (Char.code (String.unsafe_get name 0)))
  && begin
    let index = ref 0 and valid = ref true in
    while !valid && !index < length do
      let byte = Char.code (String.unsafe_get name !index) in
      valid := ascii_letter byte || ascii_digit byte || byte = Char.code '_';
      incr index
    done;
    !valid
  end

let force_valid_group_name name =
  let length = String.length name in
  let ascii_letter byte =
    (byte >= Char.code 'a' && byte <= Char.code 'z')
    || (byte >= Char.code 'A' && byte <= Char.code 'Z') in
  let ascii_digit byte = byte >= Char.code '0' && byte <= Char.code '9' in
  let prefix_digit = length > 0
      && ascii_digit (Char.code (String.unsafe_get name 0)) in
  let output = Bytes.make (length + if prefix_digit then 1 else 0) '_' in
  let offset = if prefix_digit then 1 else 0 in
  for index = 0 to length - 1 do
    let byte = Char.code (String.unsafe_get name index) in
    if ascii_letter byte || ascii_digit byte || byte = Char.code '_' then
      Bytes.unsafe_set output (offset + index) (Char.chr byte)
  done;
  Bytes.unsafe_to_string output

let groups_from_name ?cancel ?(grain = 16_384) ?(prefix = "")
    ?(conflict = Name_replace) ?(invalid_names = Ignore_invalid)
    ?(max_groups = 4_096) ?(max_payload_bytes = 268_435_456)
    ~owner ~attribute geometry =
  let owner_result = match owner with
    | Attribute.Point -> Ok (Group.Point, Geometry.point_count geometry)
    | Attribute.Primitive ->
        Ok (Group.Primitive, Geometry.primitive_count geometry)
    | Attribute.Vertex | Attribute.Detail -> Error
        "Groups from Name: owner must be point or primitive" in
  if grain <= 0 then Error "Groups from Name: grain must be positive"
  else if max_groups < 0 then Error
      "Groups from Name: max_groups must be non-negative"
  else if max_payload_bytes < 0 then Error
      "Groups from Name: max_payload_bytes must be non-negative"
  else if String.trim attribute = "" then Error
      "Groups from Name: attribute name must not be empty"
  else Result.bind owner_result (fun (group_owner, count) ->
    match Geometry.find_attribute ~owner attribute geometry with
    | None -> Error (Printf.sprintf
        "Groups from Name: missing %s text attribute %S"
        (match owner with Attribute.Point -> "point" | Attribute.Primitive ->
          "primitive" | Attribute.Vertex -> "vertex" | Attribute.Detail ->
          "detail") attribute)
    | Some source ->
        match Attribute.Private.storage source with
        | Attribute.Text values ->
            let mapping = Array.make count (-1)
            and raw_cache = Hashtbl.create (min count 4_096)
            and name_indices = Hashtbl.create (min count 4_096)
            and names_rev = ref [] and group_count = ref 0
            and failure = ref None and element = ref 0 in
            while !failure = None && !element < count do
              if !element land 16_383 = 0 then Cancel.check_opt cancel;
              let raw = Array.unsafe_get values !element in
              if raw <> "" then begin
                let group_index = match Hashtbl.find_opt raw_cache raw with
                  | Some index -> index
                  | None ->
                      let candidate = prefix ^ raw in
                      let normalized = match invalid_names with
                        | Ignore_invalid ->
                            if valid_group_name candidate then Some candidate
                            else None
                        | Force_valid -> Some (force_valid_group_name candidate) in
                      let index = match normalized with
                        | None -> -1
                        | Some name ->
                            (match Hashtbl.find_opt name_indices name with
                             | Some index -> index
                             | None when !group_count >= max_groups ->
                                 failure := Some (Printf.sprintf
                                   "Groups from Name: distinct output groups exceed max_groups=%d"
                                   max_groups);
                                 -1
                             | None ->
                                 let index = !group_count in
                                 incr group_count;
                                 Hashtbl.add name_indices name index;
                                 names_rev := name :: !names_rev;
                                 index) in
                      Hashtbl.add raw_cache raw index;
                      index in
                Array.unsafe_set mapping !element group_index
              end;
              incr element
            done;
            (match !failure with
             | Some message -> Error message
             | None ->
                 let names = Array.of_list (List.rev !names_rev) in
                 let bytes_per_group = Support.Bits.byte_count count in
                 if !group_count <> 0
                    && bytes_per_group > max_int / !group_count then Error
                     "Groups from Name: projected group payload overflows address space"
                 else
                   let payload_bytes = !group_count * bytes_per_group in
                   if payload_bytes > max_payload_bytes then Error
                       (Printf.sprintf
                         "Groups from Name: projected packed payload %d bytes exceeds max_payload_bytes=%d"
                         payload_bytes max_payload_bytes)
                   else begin
                     let store = store_of_geometry geometry in
                     let existing = Array.init !group_count (fun index ->
                       match store_find store
                           (owner_of_group group_owner) names.(index) with
                       | Some { value = Ordinary group; _ } -> Some group
                       | Some { value = Native_edges _; _ } -> assert false
                       | None -> None) in
                     let bits = Array.init !group_count (fun _ ->
                       Bytes.make bytes_per_group '\000') in
                     (match conflict with
                      | Name_replace -> ()
                      | Name_union when !group_count > 0 ->
                          Parallel.for_ ~chunk_size:1 ~start:0
                            ~finish:(!group_count - 1) (fun group_index ->
                              if group_index land 255 = 0 then
                                Cancel.check_opt cancel;
                              match existing.(group_index) with
                              | None -> ()
                              | Some group -> Group.iter
                                  (Support.Bits.set bits.(group_index)) group)
                      | Name_union -> ());
                     if bytes_per_group > 0 then
                       Parallel.for_ ~chunk_size:(max 1 (grain / 8)) ~start:0
                         ~finish:(bytes_per_group - 1) (fun byte ->
                           if byte land 2_047 = 0 then Cancel.check_opt cancel;
                           let first = byte lsl 3 in
                           for bit = 0 to min 7 (count - first - 1) do
                             let group_index = mapping.(first + bit) in
                             if group_index >= 0 then begin
                               let output = bits.(group_index) in
                               Bytes.unsafe_set output byte (Char.chr
                                 (Char.code (Bytes.unsafe_get output byte)
                                  lor (1 lsl bit)))
                             end
                           done);
                     Array.iteri (fun index name ->
                       let group = Group.Private.of_owned_bits ~owner:group_owner
                           ~name ~length:count bits.(index) in
                       match store_find store (owner_of_group group_owner) name with
                       | Some entry -> store_replace store entry (Ordinary group)
                       | None -> ignore (store_add store
                           (owner_of_group group_owner) name (Ordinary group))) names;
                     store_commit store geometry
                   end)
        | Attribute.Float _ | Attribute.Int _ | Attribute.Int_array _
        | Attribute.Float_array _ | Attribute.Float2 _
        | Attribute.Float3 _ | Attribute.Float4 _ -> Error (Printf.sprintf
            "Groups from Name: %s attribute %S must have text storage"
            (match owner with Attribute.Point -> "point" | Attribute.Primitive ->
              "primitive" | Attribute.Vertex -> "vertex" | Attribute.Detail ->
              "detail") attribute))

let name_from_groups ?cancel ?(grain = 16_384) ?(attribute = "name")
    ?(pattern = "*") ?(default = "") ?(overlap = Last_group)
    ?(delete_groups = false) ~owner geometry =
  let owner_result = match owner with
    | Attribute.Point -> Ok (Group.Point, Geometry.point_count geometry)
    | Attribute.Vertex -> Ok (Group.Vertex, Geometry.vertex_count geometry)
    | Attribute.Primitive ->
        Ok (Group.Primitive, Geometry.primitive_count geometry)
    | Attribute.Detail -> Error "Name from Groups: detail ownership is invalid" in
  if grain <= 0 then Error "Name from Groups: grain must be positive"
  else if String.trim attribute = "" then
    Error "Name from Groups: attribute name must not be empty"
  else Result.bind owner_result (fun (group_owner, count) ->
    Result.bind (Attribute_pattern.compile pattern) (fun compiled ->
      let selected = Geometry.groups geometry |> List.filter (fun group ->
        Group.owner group = group_owner
        && Attribute_pattern.matches compiled (Group.name group))
        |> Array.of_list in
      let existing = match Geometry.find_attribute ~owner attribute geometry with
        | None -> Ok None
        | Some existing ->
            (match Attribute.Private.storage existing with
             | Attribute.Text values -> Ok (Some values)
             | Attribute.Float _ | Attribute.Int _ | Attribute.Int_array _
             | Attribute.Float_array _ | Attribute.Float2 _
             | Attribute.Float3 _ | Attribute.Float4 _ -> Error (Printf.sprintf
                 "Name from Groups: %s attribute %S must have text storage"
                 (match owner with Attribute.Point -> "point"
                   | Attribute.Vertex -> "vertex"
                   | Attribute.Primitive -> "primitive"
                   | Attribute.Detail -> "detail") attribute)) in
      Result.bind existing (fun existing ->
        let assignment = Array.make count (-1) and failure = ref None in
        let group_index = ref 0 in
        while !failure = None && !group_index < Array.length selected do
          Cancel.check_opt cancel;
          let group = selected.(!group_index) in
          let bits = Group.Private.bits_view group and byte = ref 0 in
          while !failure = None && !byte < Bytes.length bits do
            if !byte land 16_383 = 0 then Cancel.check_opt cancel;
            let members = byte_member_bits.(Char.code
                (Bytes.unsafe_get bits !byte)) in
            let member = ref 0 in
            while !failure = None && !member < Array.length members do
              let element = (!byte lsl 3) + members.(!member) in
              let previous = Array.unsafe_get assignment element in
              (match overlap with
              | First_group -> if previous < 0 then
                  Array.unsafe_set assignment element !group_index
              | Last_group ->
                  Array.unsafe_set assignment element !group_index
              | Error_on_overlap when previous < 0 ->
                  Array.unsafe_set assignment element !group_index
              | Error_on_overlap -> failure := Some (Printf.sprintf
                  "Name from Groups: element %d belongs to both %S and %S"
                  element (Group.name selected.(previous)) (Group.name group)));
              incr member
            done;
            incr byte
          done;
          incr group_index
        done;
        match !failure with
        | Some message -> Error message
        | None ->
            let output = Parallel.init_array ~grain count (fun element ->
              let selected_index = assignment.(element) in
              if selected_index >= 0 then Group.name selected.(selected_index)
              else match existing with
                | Some values -> Array.unsafe_get values element
                | None -> default) in
            Result.bind (Attribute.create_owned ~name:attribute ~owner
                (Attribute.Text output)) (fun output_attribute ->
              Result.bind (Geometry.with_attribute output_attribute geometry)
                (fun geometry ->
                  if not delete_groups || Array.length selected = 0 then Ok geometry
                  else begin
                    let store = store_of_geometry geometry in
                    Array.iter (fun group ->
                      match store_find store (owner_of_group group_owner)
                          (Group.name group) with
                      | Some entry -> store_remove store entry
                      | None -> assert false) selected;
                    store_commit store geometry
                  end)))))

let selection_complement = function
  | Ordinary group -> Ordinary (Group.complement group)
  | Native_edges group -> Native_edges (Edge_group.complement group)

let selection_boolean operation left right = match left, right with
  | Ordinary left, Ordinary right ->
      let result = match operation with
        | Group_replace -> Ok right
        | Group_union -> Group.union left right
        | Group_intersection -> Group.intersection left right
        | Group_subtract -> Group.difference left right
        | Group_xor -> Group.symmetric_difference left right in
      Result.map (fun group -> Ordinary group) result
  | Native_edges left, Native_edges right ->
      let result = match operation with
        | Group_replace -> Ok right
        | Group_union -> Edge_group.union left right
        | Group_intersection -> Edge_group.intersection left right
        | Group_subtract -> Edge_group.difference left right
        | Group_xor -> Edge_group.symmetric_difference left right in
      Result.map (fun group -> Native_edges group) result
  | Ordinary _, Native_edges _ | Native_edges _, Ordinary _ ->
      Error "Group operation: cannot combine ordinary and native-edge groups"

let empty_selection ?cancel ~grain owner geometry =
  match ordinary_owner owner with
  | Some group_owner -> Ordinary (Group.init ~grain ~owner:group_owner
      ~name:"__empty" (match owner with
        | Group_points -> Geometry.point_count geometry
        | Group_vertices -> Geometry.vertex_count geometry
        | Group_primitives -> Geometry.primitive_count geometry
        | Group_edges -> assert false) (fun _ -> false))
  | None ->
      let topology = Geometry.topology geometry in
      let index = Topology_index.create ?cancel topology in
      Native_edges (Edge_group.init ~grain ~topology ~index ~name:"__empty"
        (fun _ -> false))

let publish_merged_selection ?cancel ~grain ~merge ~owner ~name generated
    geometry =
  let store = store_of_geometry geometry in
  match store_find store owner name with
  | Some entry ->
      Result.bind (selection_boolean merge entry.value generated)
        (fun merged ->
          store_replace store entry merged;
          store_commit store geometry)
  | None ->
      let output = match merge with
        | Group_replace | Group_union | Group_xor -> generated
        | Group_intersection | Group_subtract ->
            empty_selection ?cancel ~grain owner geometry in
      ignore (store_add store owner name output);
      store_commit store geometry

let unshared_edge_bits ?cancel ~grain ~surface_only topology index =
  let edge_count = Array.length index.Topology_index.Private.edge_a in
  packed_init ?cancel ~grain edge_count (fun edge ->
    let first = index.edge_offsets.(edge)
    and last = index.edge_offsets.(edge + 1) in
    last - first = 1
    && (not surface_only
        || let vertex = index.edge_vertices.(first) in
           let primitive = index.primitive_of_vertex.(vertex) in
           Char.code (Bytes.unsafe_get topology.Topology.Private.primitive_kinds
             primitive) = 0))

let unshared_selection ?cancel ~grain ~surface_only ~owner ~name geometry =
  let topology_value = Geometry.topology geometry in
  let topology = Topology.Private.view topology_value
  and index_value = Topology_index.create ?cancel topology_value in
  let index = Topology_index.Private.view index_value in
  let edge_count = Topology_index.edge_count index_value in
  let edge_bits = unshared_edge_bits ?cancel ~grain ~surface_only topology index in
  let edge_selected edge = Support.Bits.mem edge_bits edge in
  match owner with
  | Group_edges -> Native_edges (Edge_group.Private.of_owned_bits
      ~topology:topology_value ~edge_count ~name edge_bits)
  | Group_points ->
      let count = Geometry.point_count geometry in
      let bits = packed_init ?cancel ~grain count (fun point ->
        let slot = ref index.point_edge_offsets.(point)
        and last = index.point_edge_offsets.(point + 1)
        and found = ref false in
        while not !found && !slot < last do
          found := edge_selected index.point_edges.(!slot);
          incr slot
        done;
        !found) in
      Ordinary (Group.Private.of_owned_bits ~owner:Group.Point ~name
        ~length:count bits)
  | Group_primitives ->
      let count = Geometry.primitive_count geometry in
      let bits = packed_init ?cancel ~grain count (fun primitive ->
        let vertex = ref topology.primitive_offsets.(primitive)
        and last = topology.primitive_offsets.(primitive + 1)
        and found = ref false in
        while not !found && !vertex < last do
          let edge = index.edge_of_vertex.(!vertex) in
          found := edge >= 0 && edge_selected edge;
          incr vertex
        done;
        !found) in
      Ordinary (Group.Private.of_owned_bits ~owner:Group.Primitive ~name
        ~length:count bits)
  | Group_vertices -> invalid_arg "Group Unshared: vertex output is unsupported"

let group_unshared ?cancel ?(grain = 16_384) ?(merge = Group_replace)
    ~owner ~name geometry =
  if grain <= 0 then Error "Group Unshared: grain must be positive"
  else if String.trim name = "" then Error "Group Unshared: empty output name"
  else if owner = Group_vertices then
    Error "Group Unshared: vertex output is not supported"
  else begin
    Cancel.check_opt cancel;
    let generated = unshared_selection ?cancel ~grain ~surface_only:false
        ~owner ~name geometry in
    publish_merged_selection ?cancel ~grain ~merge ~owner ~name generated geometry
  end

let group_boundary_components ?cancel ?(grain = 16_384)
    ?(prefix = "boundary") ?(conflict = Name_replace) ?(max_groups = 4_096)
    ?(max_payload_bytes = 268_435_456) geometry =
  if grain <= 0 then Error "Group Boundary Components: grain must be positive"
  else if String.trim prefix = "" then
    Error "Group Boundary Components: empty output prefix"
  else if max_groups < 0 then
    Error "Group Boundary Components: max_groups must be non-negative"
  else if max_payload_bytes < 0 then
    Error "Group Boundary Components: max_payload_bytes must be non-negative"
  else begin
    Cancel.check_opt cancel;
    let topology_value = Geometry.topology geometry in
    let topology = Topology.Private.view topology_value
    and index_value = Topology_index.create ?cancel topology_value in
    let index = Topology_index.Private.view index_value in
    let edge_count = Topology_index.edge_count index_value
    and point_count = Geometry.point_count geometry in
    let boundary = unshared_edge_bits ?cancel ~grain ~surface_only:true
        topology index in
    let parent = Array.make point_count 0 in
    let rec find point =
      let link = parent.(point) in
      if link < 0 then point
      else begin
        let root = find (link - 1) in
        parent.(point) <- root + 1;
        root
      end in
    let activate point = if parent.(point) = 0 then parent.(point) <- -1 in
    for edge = 0 to edge_count - 1 do
      if edge land 16_383 = 0 then Cancel.check_opt cancel;
      if Support.Bits.mem boundary edge then begin
        let a = index.edge_a.(edge) and b = index.edge_b.(edge) in
        activate a; activate b;
        let left = find a and right = find b in
        if left <> right then begin
          let left_size = -parent.(left) and right_size = -parent.(right) in
          if left_size >= right_size then begin
            parent.(left) <- -(left_size + right_size);
            parent.(right) <- left + 1
          end else begin
            parent.(right) <- -(left_size + right_size);
            parent.(left) <- right + 1
          end
        end
      end
    done;
    let component = Array.make point_count (-1) in
    for point = 0 to point_count - 1 do
      if point land 16_383 = 0 then Cancel.check_opt cancel;
      if parent.(point) <> 0 then component.(point) <- find point
    done;
    Array.fill parent 0 point_count (-1);
    let component_count = ref 0 and too_many = ref false in
    for point = 0 to point_count - 1 do
      let root = component.(point) in
      if root >= 0 then begin
        if parent.(root) < 0 then begin
          if !component_count >= max_groups then too_many := true
          else begin
            parent.(root) <- !component_count;
            incr component_count
          end
        end;
        if not !too_many then component.(point) <- parent.(root)
      end
    done;
    let bytes_per_group = Support.Bits.byte_count point_count in
    if !too_many then Error (Printf.sprintf
        "Group Boundary Components: outputs exceed max_groups=%d" max_groups)
    else if !component_count <> 0
        && bytes_per_group > max_int / !component_count then
      Error "Group Boundary Components: projected payload overflows address space"
    else
      let payload_bytes = !component_count * bytes_per_group in
      if payload_bytes > max_payload_bytes then Error (Printf.sprintf
          "Group Boundary Components: projected packed payload %d bytes exceeds max_payload_bytes=%d"
          payload_bytes max_payload_bytes)
      else begin
        let names = Array.init !component_count (fun component ->
          prefix ^ "__" ^ string_of_int component) in
        let bits = Array.init !component_count (fun _ ->
          Bytes.make bytes_per_group '\000') in
        let store = store_of_geometry geometry in
        (match conflict with
         | Name_replace -> ()
         | Name_union -> Array.iteri (fun component name ->
             if component land 255 = 0 then Cancel.check_opt cancel;
             match store_find store Group_points name with
             | Some { value = Ordinary existing; _ } ->
                 Group.iter (fun point ->
                   if point land 16_383 = 0 then Cancel.check_opt cancel;
                   Support.Bits.set bits.(component) point) existing
             | Some { value = Native_edges _; _ } -> assert false
             | None -> ()) names);
        if bytes_per_group > 0 then
          Parallel.for_ ~chunk_size:(max 1 (grain / 8)) ~start:0
            ~finish:(bytes_per_group - 1) (fun byte ->
              if byte land 2_047 = 0 then Cancel.check_opt cancel;
              let first = byte lsl 3 in
              for bit = 0 to min 7 (point_count - first - 1) do
                let component = component.(first + bit) in
                if component >= 0 then begin
                  let output = bits.(component) in
                  Bytes.unsafe_set output byte (Char.chr
                    (Char.code (Bytes.unsafe_get output byte) lor (1 lsl bit)))
                end
              done);
        Array.iteri (fun component name ->
          let group = Group.Private.of_owned_bits ~owner:Group.Point ~name
              ~length:point_count bits.(component) in
          match store_find store Group_points name with
          | Some entry -> store_replace store entry (Ordinary group)
          | None -> ignore (store_add store Group_points name (Ordinary group)))
          names;
        store_commit store geometry
      end
  end

let compile_nonblank label source =
  if String.trim source = "" then Error (label ^ ": empty group pattern")
  else Attribute_pattern.compile source

let resolve_pattern ?cancel ~grain store owner pattern geometry =
  Result.bind (compile_nonblank "Group operation" pattern) (fun compiled ->
    let matches = List.filter (fun entry -> entry.alive && entry.owner = owner
        && Attribute_pattern.matches compiled entry.entry_name)
        (store_entries store) in
    match matches with
    | [] -> Ok (empty_selection ?cancel ~grain owner geometry)
    | first :: rest ->
        List.fold_left (fun result entry -> Result.bind result (fun accumulated ->
          selection_boolean Group_union accumulated entry.value))
          (Ok first.value) rest)

let[@inline] mixed_pair_identity left right =
  let left, right = if left <= right then left, right else right, left in
  let left = left lxor (left lsr 29) in
  let left = left * 0x1e3779b97f4a7c15 in
  let right = right lxor (right lsr 27) in
  (left lxor (right * 0x14d049bb133111eb)) land max_int

let group_edge_depth ?cancel ?(grain = 16_384) ?(merge = Group_replace)
    ~depth ~point_group ~name geometry =
  if grain <= 0 then Error "Group Edge Depth: grain must be positive"
  else if depth < 0 then Error "Group Edge Depth: depth must be non-negative"
  else if String.trim point_group = "" then
    Error "Group Edge Depth: empty seed point group name"
  else if String.trim name = "" then
    Error "Group Edge Depth: empty output group name"
  else
    match Geometry.find_group ~owner:Group.Point point_group geometry with
    | None -> Error (Printf.sprintf
        "Group Edge Depth: missing seed point group %S" point_group)
    | Some source when Group.length source <> Geometry.point_count geometry ->
        Error "Group Edge Depth: seed group length does not match point count"
    | Some source ->
        Cancel.check_opt cancel;
        let cardinality = Group.cardinality source in
        let generated =
          if depth = 0 || cardinality = 0 || cardinality = Group.length source
          then Group.with_name name source
          else
            let reverse = Topology_index.create ?cancel
                (Geometry.topology geometry) |> Topology_index.Private.view in
            let bits = Bytes.copy (Group.Private.bits_view source) in
            let bits = grow_point_depth_bits ?cancel reverse bits cardinality
                depth None in
            Group.Private.of_owned_bits ~owner:Group.Point ~name
              ~length:(Group.length source) bits in
        publish_merged_selection ?cancel ~grain ~merge ~owner:Group_points
          ~name (Ordinary generated) geometry

let group_random ?cancel ?(grain = 16_384) ?(seed = Rays_math.Rand.seed 0)
    ?seed_attribute ?base ?(merge = Group_replace) ~probability ~owner ~name
    geometry =
  if grain <= 0 then Error "Group Random: grain must be positive"
  else if String.trim name = "" then Error "Group Random: empty output name"
  else if not (Float.is_finite probability) || probability < 0.
      || probability > 1. then Error
      "Group Random: probability must be finite and between zero and one"
  else begin
    let topology = Geometry.topology geometry in
    let topology_view = Topology.Private.view topology in
    let index = match owner with
      | Group_edges -> Some (Topology_index.create ?cancel topology)
      | Group_points | Group_vertices | Group_primitives -> None in
    let count = match owner with
      | Group_points -> Geometry.point_count geometry
      | Group_vertices -> Geometry.vertex_count geometry
      | Group_primitives -> Geometry.primitive_count geometry
      | Group_edges -> Topology_index.edge_count (Option.get index) in
    let base_selection = match base with
      | None -> Ok None
      | Some base_name when String.trim base_name = "" ->
          Error "Group Random: empty base group name"
      | Some base_name ->
          (match find_selection owner base_name geometry with
           | Some selection -> Ok (Some selection)
           | None -> Error (Printf.sprintf
               "Group Random: missing %s base group %S"
               (owner_name owner) base_name)) in
    let seed_owner = match owner with
      | Group_points -> Attribute.Point
      | Group_primitives -> Attribute.Primitive
      | Group_vertices | Group_edges -> Attribute.Point in
    let seed_values = match seed_attribute with
      | None -> Ok None
      | Some attribute when String.trim attribute = "" ->
          Error "Group Random: empty seed attribute name"
      | Some attribute ->
          (match Geometry.find_attribute ~owner:seed_owner attribute geometry with
           | None -> Error (Printf.sprintf
               "Group Random: missing %s integer seed attribute %S"
               (match seed_owner with Attribute.Point -> "point"
                 | Attribute.Primitive -> "primitive"
                 | Attribute.Vertex -> "vertex"
                 | Attribute.Detail -> "detail") attribute)
           | Some attribute_value ->
               (match Attribute.Private.storage attribute_value with
                | Attribute.Int values -> Ok (Some values)
                | Attribute.Float _ | Attribute.Int_array _
                | Attribute.Float_array _ | Attribute.Float2 _ | Attribute.Float3 _
                | Attribute.Float4 _ | Attribute.Text _ -> Error (Printf.sprintf
                    "Group Random: seed attribute %S must have integer storage"
                    attribute))) in
    Result.bind base_selection (fun base_selection ->
      Result.bind seed_values (fun seed_values ->
        let identity = match owner with
          | Group_points -> (fun element -> match seed_values with
              | Some values -> Array.unsafe_get values element
              | None -> element)
          | Group_primitives -> (fun element -> match seed_values with
              | Some values -> Array.unsafe_get values element
              | None -> element)
          | Group_vertices -> (fun vertex ->
              let point = topology_view.vertex_points.(vertex) in
              match seed_values with
              | Some values -> Array.unsafe_get values point
              | None -> point)
          | Group_edges ->
              let index_view = Topology_index.Private.view (Option.get index) in
              (fun edge ->
                let a = index_view.edge_a.(edge) and b = index_view.edge_b.(edge) in
                let a, b = match seed_values with
                  | Some values -> values.(a), values.(b)
                  | None -> a, b in
                mixed_pair_identity a b) in
        let predicate element =
          (match base_selection with
           | None -> true
           | Some selection -> selection_mem selection element)
          && (probability = 1. || probability > 0.
              && Rays_math.Rand.float_at seed ~index:(identity element) < probability) in
        let generated = match owner with
          | Group_points | Group_vertices | Group_primitives ->
              let group_owner = Option.get (ordinary_owner owner) in
              Ordinary (Group.Private.of_owned_bits ~owner:group_owner ~name
                ~length:count (packed_init ?cancel ~grain count predicate))
          | Group_edges ->
              Native_edges (Edge_group.Private.of_owned_bits ~topology
                ~edge_count:count ~name (packed_init ?cancel ~grain count predicate)) in
        publish_merged_selection ?cancel ~grain ~merge ~owner ~name generated
          geometry))
  end

let finite_vec3 value = Float.is_finite value.Rays_math.Vec3.x
    && Float.is_finite value.y && Float.is_finite value.z

let[@inline always] float_max (left : float) (right : float) =
  if left >= right then left else right
let[@inline always] float_min (left : float) (right : float) =
  if left <= right then left else right

let[@inline always] point_in_sphere_at center radius
    (positions : Packed.Float3.Private.view) point =
  let x = positions.x.(point)
  and y = positions.y.(point) and z = positions.z.(point) in
  let dx = abs_float (x -. center.Rays_math.Vec3.x)
  and dy = abs_float (y -. center.y)
  and dz = abs_float (z -. center.z) in
  if dx > radius || dy > radius || dz > radius then false
  else
    let scale = float_max dx (float_max dy dz) in
    scale = 0. || begin
      let dx = dx /. scale and dy = dy /. scale and dz = dz /. scale
      and limit = radius /. scale in
      (dx *. dx) +. (dy *. dy) +. (dz *. dz) <= limit *. limit
    end

let[@inline always] point_in_box_at minimum maximum
    (positions : Packed.Float3.Private.view) point =
  let x = positions.x.(point) and y = positions.y.(point)
  and z = positions.z.(point) in
  x >= minimum.Rays_math.Vec3.x && x <= maximum.Rays_math.Vec3.x
  && y >= minimum.y && y <= maximum.y
  && z >= minimum.z && z <= maximum.z

let[@inline always] segment_intersects_box_at minimum maximum
    (positions : Packed.Float3.Private.view) a b =
  (* Segment/AABB separating-axis test. Scaling first keeps midpoint, extent,
     and cross-product arithmetic finite even for opposite [max_float] ends. *)
  if point_in_box_at minimum maximum positions a
      || point_in_box_at minimum maximum positions b then true
  else
  let ax = positions.x.(a) and ay = positions.y.(a)
  and az = positions.z.(a) and bx = positions.x.(b)
  and by = positions.y.(b) and bz = positions.z.(b) in
  let scale_a = float_max (abs_float ax)
      (float_max (abs_float ay) (abs_float az))
  and scale_b = float_max (abs_float bx)
      (float_max (abs_float by) (abs_float bz))
  and scale_min = float_max (abs_float minimum.Rays_math.Vec3.x)
      (float_max (abs_float minimum.y) (abs_float minimum.z))
  and scale_max = float_max (abs_float maximum.Rays_math.Vec3.x)
      (float_max (abs_float maximum.y) (abs_float maximum.z)) in
  let scale = float_max (float_max scale_a scale_b)
      (float_max scale_min scale_max) in
  if scale = 0. then true
  else
    let ax = ax /. scale and ay = ay /. scale and az = az /. scale
    and bx = bx /. scale and by = by /. scale and bz = bz /. scale
    and lx = minimum.x /. scale and ly = minimum.y /. scale
    and lz = minimum.z /. scale and hx = maximum.x /. scale
    and hy = maximum.y /. scale and hz = maximum.z /. scale in
    let mx = ((ax +. bx) *. 0.5) -. ((lx +. hx) *. 0.5)
    and my = ((ay +. by) *. 0.5) -. ((ly +. hy) *. 0.5)
    and mz = ((az +. bz) *. 0.5) -. ((lz +. hz) *. 0.5)
    and dx = (bx -. ax) *. 0.5 and dy = (by -. ay) *. 0.5
    and dz = (bz -. az) *. 0.5 and ex = (hx -. lx) *. 0.5
    and ey = (hy -. ly) *. 0.5 and ez = (hz -. lz) *. 0.5 in
    let adx = abs_float dx and ady = abs_float dy and adz = abs_float dz in
    abs_float mx <= ex +. adx
    && abs_float my <= ey +. ady
    && abs_float mz <= ez +. adz
    && abs_float ((my *. dz) -. (mz *. dy)) <= (ey *. adz) +. (ez *. ady)
    && abs_float ((mz *. dx) -. (mx *. dz)) <= (ez *. adx) +. (ex *. adz)
    && abs_float ((mx *. dy) -. (my *. dx)) <= (ex *. ady) +. (ey *. adx)

let[@inline always] segment_intersects_sphere_at center radius
    (positions : Packed.Float3.Private.view) a b =
  if point_in_sphere_at center radius positions a
      || point_in_sphere_at center radius positions b then true
  else
    let ax = positions.x.(a) and ay = positions.y.(a)
    and az = positions.z.(a) and bx = positions.x.(b)
    and by = positions.y.(b) and bz = positions.z.(b) in
    let scale_a = float_max (abs_float ax)
        (float_max (abs_float ay) (abs_float az))
    and scale_b = float_max (abs_float bx)
        (float_max (abs_float by) (abs_float bz))
    and scale_c = float_max (abs_float center.Rays_math.Vec3.x)
        (float_max (abs_float center.y) (abs_float center.z)) in
    let scale = float_max scale_a (float_max scale_b scale_c) in
    if scale = 0. then radius >= 0.
    else begin
      let ax = ax /. scale and ay = ay /. scale and az = az /. scale
      and bx = bx /. scale and by = by /. scale and bz = bz /. scale
      and cx = center.x /. scale and cy = center.y /. scale
      and cz = center.z /. scale in
      let dx = bx -. ax and dy = by -. ay and dz = bz -. az
      and qx = cx -. ax and qy = cy -. ay and qz = cz -. az in
      let denominator = (dx *. dx) +. (dy *. dy) +. (dz *. dz) in
      if denominator = 0. then false
      else
        let parameter = float_max 0. (float_min 1.
            (((qx *. dx) +. (qy *. dy) +. (qz *. dz)) /. denominator)) in
        let ex = (ax +. (parameter *. dx)) -. cx
        and ey = (ay +. (parameter *. dy)) -. cy
        and ez = (az +. (parameter *. dz)) -. cz
        and limit = radius /. scale in
        (ex *. ex) +. (ey *. ey) +. (ez *. ez) <= limit *. limit
    end

let group_bounds ?cancel ?(grain = 16_384) ?base
    ?(containment = Fully_contained) ?(merge = Group_replace)
    bounds ~owner ~name geometry =
  let valid_bounds = match bounds with
    | Bounds_box { minimum; maximum } ->
        finite_vec3 minimum && finite_vec3 maximum
        && minimum.x <= maximum.x && minimum.y <= maximum.y
        && minimum.z <= maximum.z
    | Bounds_sphere { center; radius } ->
        finite_vec3 center && Float.is_finite radius && radius >= 0. in
  if grain <= 0 then Error "Group Bounds: grain must be positive"
  else if String.trim name = "" then Error "Group Bounds: empty output name"
  else if not valid_bounds then Error
      "Group Bounds: bounds must be finite, ordered, and have non-negative radius"
  else begin
    let topology = Geometry.topology geometry in
    let topology_view = Topology.Private.view topology
    and positions = Packed.Float3.Private.view (Geometry.positions geometry) in
    let index = match owner with
      | Group_edges -> Some (Topology_index.create ?cancel topology)
      | Group_points | Group_vertices | Group_primitives -> None in
    let count = match owner with
      | Group_points -> Geometry.point_count geometry
      | Group_vertices -> Geometry.vertex_count geometry
      | Group_primitives -> Geometry.primitive_count geometry
      | Group_edges -> Topology_index.edge_count (Option.get index) in
    let base_selection = match base with
      | None -> Ok None
      | Some base_name when String.trim base_name = "" ->
          Error "Group Bounds: empty base group name"
      | Some base_name ->
          (match find_selection owner base_name geometry with
           | Some selection -> Ok (Some selection)
           | None -> Error (Printf.sprintf
               "Group Bounds: missing %s base group %S"
               (owner_name owner) base_name)) in
    Result.bind base_selection (fun base_selection ->
      let point_inside = match bounds with
        | Bounds_box { minimum; maximum } ->
            (fun point -> point_in_box_at minimum maximum positions point)
        | Bounds_sphere { center; radius } ->
            (fun point -> point_in_sphere_at center radius positions point) in
      let[@inline always] edge_inside a b = match containment with
        | Fully_contained -> point_inside a && point_inside b
        | Partially_contained ->
            (match bounds with
             | Bounds_box { minimum; maximum } ->
                 segment_intersects_box_at minimum maximum positions a b
             | Bounds_sphere { center; radius } ->
                 segment_intersects_sphere_at center radius positions a b) in
      let owner_predicate = match owner with
        | Group_points -> point_inside
        | Group_vertices ->
            (fun vertex -> point_inside topology_view.vertex_points.(vertex))
        | Group_primitives ->
            (fun primitive ->
              let first = topology_view.primitive_offsets.(primitive)
              and last = topology_view.primitive_offsets.(primitive + 1) in
              if first = last then false
              else match containment with
                | Fully_contained ->
                    let vertex = ref first and selected = ref true in
                    while !selected && !vertex < last do
                      selected := point_inside topology_view.vertex_points.(!vertex);
                      incr vertex
                    done;
                    !selected
                | Partially_contained ->
                    let vertex = ref first and selected = ref false in
                    while not !selected && !vertex < last do
                      selected := point_inside topology_view.vertex_points.(!vertex);
                      incr vertex
                    done;
                    !selected)
        | Group_edges ->
            let index = Topology_index.Private.view (Option.get index) in
            (fun edge -> edge_inside index.edge_a.(edge) index.edge_b.(edge)) in
      let predicate element =
        (match base_selection with None -> true
         | Some selection -> selection_mem selection element)
        && owner_predicate element in
      let generated = match owner with
        | Group_points | Group_vertices | Group_primitives ->
            let group_owner = Option.get (ordinary_owner owner) in
            Ordinary (Group.Private.of_owned_bits ~owner:group_owner ~name
              ~length:count (packed_init ?cancel ~grain count predicate))
        | Group_edges -> Native_edges (Edge_group.Private.of_owned_bits
            ~topology ~edge_count:count ~name
            (packed_init ?cancel ~grain count predicate)) in
      publish_merged_selection ?cancel ~grain ~merge ~owner ~name generated
        geometry)
  end

let combine ?cancel ?(grain = 16_384) ~owner ~name ~base ~steps geometry =
  if grain <= 0 then Error "Group Combine: grain must be positive"
  else if String.trim name = "" then Error "Group Combine: empty output name"
  else begin
    Cancel.check_opt cancel;
    let store = store_of_geometry geometry in
    Result.bind (resolve_pattern ?cancel ~grain store owner base.pattern geometry)
      (fun initial ->
        let initial = if base.inverted then selection_complement initial else initial in
        let result = List.fold_left (fun result step ->
          Result.bind result (fun accumulated ->
            Result.bind (resolve_pattern ?cancel ~grain store owner
              step.operand.pattern geometry) (fun operand ->
                Cancel.check_opt cancel;
                let operand = if step.operand.inverted
                  then selection_complement operand else operand in
                selection_boolean step.operation accumulated operand)))
          (Ok initial) steps in
        Result.bind result (fun value ->
          match store_find store owner name with
          | Some entry -> store_replace store entry value;
              store_commit store geometry
          | None -> ignore (store_add store owner name value);
              store_commit store geometry))
  end

let range_bounds count = function
  | Range_start_end { start; end_ } -> start, end_
  | Range_from_ends { start; end_offset } ->
      if start < 0 || end_offset < 0 then
        invalid_arg "Group Range: relative offsets must be non-negative";
      start, count - end_offset - 1
  | Range_start_length { start; length } ->
      if length <= 0 then 1, 0
      else if start < 0 then begin
        (* Adding a positive value to a negative one cannot overflow [int]. *)
        let end_exclusive = start + length in
        if end_exclusive <= 0 then 1, 0
        else 0, min (count - 1) (end_exclusive - 1)
      end
      else if start >= count then 1, 0
      else
        let available = count - start in
        start, (if length >= available then count - 1 else start + length - 1)
  | Range_partition { partition; partitions } ->
      if partitions <= 0 then
        invalid_arg "Group Range: partition count must be positive";
      if partition < 0 || partition >= partitions then 1, 0
      else
        let quotient = count / partitions and remainder = count mod partitions in
        let first = (partition * quotient) + min partition remainder in
        let size = quotient + if partition < remainder then 1 else 0 in
        first, first + size - 1

let finite_positions ?cancel ~grain ~operation geometry =
  let positions = Packed.Float3.Private.view (Geometry.positions geometry) in
  let count = Geometry.point_count geometry in
  let ranges = if count = 0 then 0 else (count + grain - 1) / grain in
  let failures = Array.make ranges (-1) in
  if ranges > 0 then Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(ranges - 1)
    (fun range ->
      let first = range * grain and last = min count ((range + 1) * grain) in
      let point = ref first in
      while failures.(range) < 0 && !point < last do
        if !point land 4095 = 0 then Cancel.check_opt cancel;
        if not (Float.is_finite positions.x.(!point)
            && Float.is_finite positions.y.(!point)
            && Float.is_finite positions.z.(!point))
        then failures.(range) <- !point;
        incr point
      done);
  match Array.find_opt (fun point -> point >= 0) failures with
  | None -> Ok positions
  | Some point -> Error (Printf.sprintf
      "%s: non-finite position at point %d" operation point)

let geometric_face_directions ?cancel ~grain topology
    (positions : Packed.Float3.Private.view) =
  let topology_view = Topology.Private.view topology in
  let count = Topology.primitive_count topology in
  let nx = Array.make count 0. and ny = Array.make count 0.
  and nz = Array.make count 0. and valid = Bytes.make count '\000' in
  if count > 0 then Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(count - 1)
    (fun primitive ->
      if primitive land 4095 = 0 then Cancel.check_opt cancel;
      if Bytes.unsafe_get topology_view.primitive_kinds primitive = '\000' then begin
        let first = topology_view.primitive_offsets.(primitive)
        and last = topology_view.primitive_offsets.(primitive + 1) in
        let scale = ref 0. in
        for vertex = first to last - 1 do
          let point = topology_view.vertex_points.(vertex) in
          scale := float_max !scale (float_max (abs_float positions.x.(point))
              (float_max (abs_float positions.y.(point))
                 (abs_float positions.z.(point))))
        done;
        if !scale > 0. then begin
          let anchor = topology_view.vertex_points.(first) in
          let ax = positions.x.(anchor) /. !scale
          and ay = positions.y.(anchor) /. !scale
          and az = positions.z.(anchor) /. !scale in
          let x = ref 0. and y = ref 0. and z = ref 0. in
          for vertex = first + 1 to last - 2 do
            let b = topology_view.vertex_points.(vertex)
            and c = topology_view.vertex_points.(vertex + 1) in
            let ux = (positions.x.(b) /. !scale) -. ax
            and uy = (positions.y.(b) /. !scale) -. ay
            and uz = (positions.z.(b) /. !scale) -. az
            and vx = (positions.x.(c) /. !scale) -. ax
            and vy = (positions.y.(c) /. !scale) -. ay
            and vz = (positions.z.(c) /. !scale) -. az in
            x := !x +. ((uy *. vz) -. (uz *. vy));
            y := !y +. ((uz *. vx) -. (ux *. vz));
            z := !z +. ((ux *. vy) -. (uy *. vx))
          done;
          let magnitude_scale = float_max (abs_float !x)
              (float_max (abs_float !y) (abs_float !z)) in
          if magnitude_scale > 0. then begin
            let x = !x /. magnitude_scale and y = !y /. magnitude_scale
            and z = !z /. magnitude_scale in
            let inverse = 1. /. sqrt ((x *. x) +. (y *. y) +. (z *. z)) in
            nx.(primitive) <- x *. inverse;
            ny.(primitive) <- y *. inverse;
            nz.(primitive) <- z *. inverse;
            Bytes.unsafe_set valid primitive '\001'
          end
        end
      end);
  nx, ny, nz, valid

let[@inline always] corner_angle_at
    (positions : Packed.Float3.Private.view) point previous next =
  let scale = abs_float positions.x.(point) in
  let scale = float_max scale (abs_float positions.y.(point)) in
  let scale = float_max scale (abs_float positions.z.(point)) in
  let scale = float_max scale (abs_float positions.x.(previous)) in
  let scale = float_max scale (abs_float positions.y.(previous)) in
  let scale = float_max scale (abs_float positions.z.(previous)) in
  let scale = float_max scale (abs_float positions.x.(next)) in
  let scale = float_max scale (abs_float positions.y.(next)) in
  let scale = float_max scale (abs_float positions.z.(next)) in
  if scale = 0. then 0.
  else
    let px = positions.x.(point) /. scale
    and py = positions.y.(point) /. scale
    and pz = positions.z.(point) /. scale in
    let ux = (positions.x.(previous) /. scale) -. px
    and uy = (positions.y.(previous) /. scale) -. py
    and uz = (positions.z.(previous) /. scale) -. pz
    and vx = (positions.x.(next) /. scale) -. px
    and vy = (positions.y.(next) /. scale) -. py
    and vz = (positions.z.(next) /. scale) -. pz in
    let us = float_max (abs_float ux) (float_max (abs_float uy) (abs_float uz))
    and vs = float_max (abs_float vx) (float_max (abs_float vy) (abs_float vz)) in
    if us = 0. || vs = 0. then 0.
    else
      let ux = ux /. us and uy = uy /. us and uz = uz /. us
      and vx = vx /. vs and vy = vy /. vs and vz = vz /. vs in
      let ui = 1. /. sqrt ((ux *. ux) +. (uy *. uy) +. (uz *. uz))
      and vi = 1. /. sqrt ((vx *. vx) +. (vy *. vy) +. (vz *. vz)) in
      let ux = ux *. ui and uy = uy *. ui and uz = uz *. ui
      and vx = vx *. vi and vy = vy *. vi and vz = vz *. vi in
      let cx = (uy *. vz) -. (uz *. vy)
      and cy = (uz *. vx) -. (ux *. vz)
      and cz = (ux *. vy) -. (uy *. vx)
      and dot = (ux *. vx) +. (uy *. vy) +. (uz *. vz) in
      atan2 (sqrt ((cx *. cx) +. (cy *. cy) +. (cz *. cz))) dot

let geometric_point_directions ?cancel ~grain topology index
    (positions : Packed.Float3.Private.view)
    face_x face_y face_z face_valid =
  let topology_view = Topology.Private.view topology
  and index_view = Topology_index.Private.view index in
  let count = Topology.point_count topology in
  let nx = Array.make count 0. and ny = Array.make count 0.
  and nz = Array.make count 0. in
  if count > 0 then Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(count - 1)
    (fun point ->
      if point land 4095 = 0 then Cancel.check_opt cancel;
      let first = index_view.point_offsets.(point)
      and last = index_view.point_offsets.(point + 1) in
      let x = ref 0. and y = ref 0. and z = ref 0. in
      for slot = first to last - 1 do
        let vertex = index_view.point_vertices.(slot)
        and primitive = index_view.primitive_of_vertex.(
            index_view.point_vertices.(slot)) in
        if Bytes.unsafe_get face_valid primitive <> '\000' then begin
          let primitive_first = topology_view.primitive_offsets.(primitive)
          and primitive_last = topology_view.primitive_offsets.(primitive + 1) in
          let previous = if vertex = primitive_first
            then primitive_last - 1 else vertex - 1
          and next = if vertex + 1 = primitive_last
            then primitive_first else vertex + 1 in
          let angle = corner_angle_at positions point
              topology_view.vertex_points.(previous)
              topology_view.vertex_points.(next) in
          x := !x +. (face_x.(primitive) *. angle);
          y := !y +. (face_y.(primitive) *. angle);
          z := !z +. (face_z.(primitive) *. angle)
        end
      done;
      let scale = float_max (abs_float !x)
          (float_max (abs_float !y) (abs_float !z)) in
      if scale > 0. then begin
        let x = !x /. scale and y = !y /. scale and z = !z /. scale in
        let inverse = 1. /. sqrt ((x *. x) +. (y *. y) +. (z *. z)) in
        nx.(point) <- x *. inverse;
        ny.(point) <- y *. inverse;
        nz.(point) <- z *. inverse
      end);
  nx, ny, nz

let[@inline always] direction_matches threshold include_opposite dx dy dz x y z =
  if not (Float.is_finite x && Float.is_finite y && Float.is_finite z) then false
  else
    let scale = float_max (abs_float x) (float_max (abs_float y) (abs_float z)) in
    if scale = 0. then false
    else
    let x = x /. scale and y = y /. scale and z = z /. scale in
    let inverse = 1. /. sqrt ((x *. x) +. (y *. y) +. (z *. z)) in
    let dot = ((x *. dx) +. (y *. dy) +. (z *. dz)) *. inverse in
    let dot = if dot < -1. then -1. else if dot > 1. then 1. else dot in
    let epsilon = 64. *. Float.epsilon in
    if include_opposite then threshold <= 0. || abs_float dot >= threshold -. epsilon
    else dot >= threshold -. epsilon

let[@inline always] point_geometric_direction_matches threshold
    include_opposite dx dy dz (positions : Packed.Float3.Private.view)
    (topology : Topology.Private.view) (index : Topology_index.Private.view)
    face_x face_y face_z face_valid point =
  let first = index.point_offsets.(point)
  and last = index.point_offsets.(point + 1) in
  let x = ref 0. and y = ref 0. and z = ref 0. in
  for slot = first to last - 1 do
    let vertex = index.point_vertices.(slot)
    and primitive = index.primitive_of_vertex.(index.point_vertices.(slot)) in
    if Bytes.unsafe_get face_valid primitive <> '\000' then begin
      let primitive_first = topology.primitive_offsets.(primitive)
      and primitive_last = topology.primitive_offsets.(primitive + 1) in
      let previous = if vertex = primitive_first
        then primitive_last - 1 else vertex - 1
      and next = if vertex + 1 = primitive_last
        then primitive_first else vertex + 1 in
      let angle = corner_angle_at positions point
          topology.vertex_points.(previous) topology.vertex_points.(next) in
      x := !x +. (face_x.(primitive) *. angle);
      y := !y +. (face_y.(primitive) *. angle);
      z := !z +. (face_z.(primitive) *. angle)
    end
  done;
  direction_matches threshold include_opposite dx dy dz !x !y !z

let[@inline always] primitive_geometric_direction_matches threshold
    include_opposite dx dy dz (positions : Packed.Float3.Private.view)
    (topology : Topology.Private.view) primitive =
  if Bytes.unsafe_get topology.primitive_kinds primitive <> '\000' then false
  else
    let first = topology.primitive_offsets.(primitive)
    and last = topology.primitive_offsets.(primitive + 1) in
    let scale = ref 0. in
    for vertex = first to last - 1 do
      let point = topology.vertex_points.(vertex) in
      scale := float_max !scale (float_max (abs_float positions.x.(point))
          (float_max (abs_float positions.y.(point))
             (abs_float positions.z.(point))))
    done;
    if !scale = 0. then false
    else
      let anchor = topology.vertex_points.(first) in
      let ax = positions.x.(anchor) /. !scale
      and ay = positions.y.(anchor) /. !scale
      and az = positions.z.(anchor) /. !scale in
      let x = ref 0. and y = ref 0. and z = ref 0. in
      for vertex = first + 1 to last - 2 do
        let b = topology.vertex_points.(vertex)
        and c = topology.vertex_points.(vertex + 1) in
        let ux = (positions.x.(b) /. !scale) -. ax
        and uy = (positions.y.(b) /. !scale) -. ay
        and uz = (positions.z.(b) /. !scale) -. az
        and vx = (positions.x.(c) /. !scale) -. ax
        and vy = (positions.y.(c) /. !scale) -. ay
        and vz = (positions.z.(c) /. !scale) -. az in
        x := !x +. ((uy *. vz) -. (uz *. vy));
        y := !y +. ((uz *. vx) -. (ux *. vz));
        z := !z +. ((ux *. vy) -. (uy *. vx))
      done;
      direction_matches threshold include_opposite dx dy dz !x !y !z

let group_normal ?cancel ?(grain = 16_384) ?normal_attribute
    ?(use_existing_normal = true) ?base
    ?(include_opposite = false) ?(merge = Group_replace)
    ~direction ~spread_angle ~owner ~name geometry =
  if grain <= 0 then Error "Group Normal: grain must be positive"
  else if String.trim name = "" then Error "Group Normal: empty output name"
  else if owner = Group_vertices then Error
      "Group Normal: vertex groups are not supported"
  else if not (finite_vec3 direction) then Error
      "Group Normal: direction must be finite and non-zero"
  else if not (Float.is_finite spread_angle) || spread_angle < 0.
      || spread_angle > Float.pi then Error
      "Group Normal: spread angle must be finite and within [0, pi]"
  else
    let direction_scale = float_max (abs_float direction.Rays_math.Vec3.x)
        (float_max (abs_float direction.y) (abs_float direction.z)) in
    if direction_scale = 0. then Error
        "Group Normal: direction must be finite and non-zero"
    else begin
      let x = direction.x /. direction_scale
      and y = direction.y /. direction_scale
      and z = direction.z /. direction_scale in
      let inverse = 1. /. sqrt ((x *. x) +. (y *. y) +. (z *. z)) in
      let dx = x *. inverse and dy = y *. inverse and dz = z *. inverse
      and threshold = cos spread_angle in
      let topology = Geometry.topology geometry in
      let index = match owner with
        | Group_points | Group_edges -> Some (Topology_index.create ?cancel topology)
        | Group_primitives -> None
        | Group_vertices -> assert false in
      let count = match owner with
        | Group_points -> Geometry.point_count geometry
        | Group_primitives -> Geometry.primitive_count geometry
        | Group_edges -> Topology_index.edge_count (Option.get index)
        | Group_vertices -> assert false in
      let base_selection = match base with
        | None -> Ok None
        | Some base_name when String.trim base_name = "" ->
            Error "Group Normal: empty base group name"
        | Some base_name ->
            (match find_selection owner base_name geometry with
             | Some selection -> Ok (Some selection)
             | None -> Error (Printf.sprintf
                 "Group Normal: missing %s base group %S"
                 (owner_name owner) base_name)) in
      let attribute_values = match normal_attribute, use_existing_normal, owner with
        | None, true, (Group_points | Group_edges) ->
            (match Geometry.find_attribute ~owner:Attribute.Point "N" geometry with
             | None -> Ok None
             | Some attribute ->
                 (match Attribute.Private.storage attribute with
                  | Attribute.Float3 values -> Ok (Some
                      (Packed.Float3.Private.view values))
                  | Attribute.Float _ | Attribute.Int_array _
                  | Attribute.Float_array _ | Attribute.Float2 _ | Attribute.Float4 _
                  | Attribute.Int _ | Attribute.Text _ -> Error
                      "Group Normal: point normal attribute \"N\" must have float3 storage"))
        | None, _, _ -> Ok None
        | Some attribute_name, _, _ when String.trim attribute_name = "" ->
            Error "Group Normal: empty normal attribute name"
        | Some attribute_name, _, _ ->
            let attribute_owner = match owner with
              | Group_primitives -> Attribute.Primitive
              | Group_points | Group_edges -> Attribute.Point
              | Group_vertices -> assert false in
            (match Geometry.find_attribute ~owner:attribute_owner
                attribute_name geometry with
             | None -> Error (Printf.sprintf
                 "Group Normal: missing %s float3 normal attribute %S"
                 (match attribute_owner with
                  | Attribute.Point -> "point" | Attribute.Primitive -> "primitive"
                  | Attribute.Vertex -> "vertex" | Attribute.Detail -> "detail")
                 attribute_name)
             | Some attribute ->
                 (match Attribute.Private.storage attribute with
                  | Attribute.Float3 values -> Ok (Some
                      (Packed.Float3.Private.view values))
                  | Attribute.Float _ | Attribute.Int_array _
                  | Attribute.Float_array _ | Attribute.Float2 _ | Attribute.Float4 _
                  | Attribute.Int _ | Attribute.Text _ -> Error (Printf.sprintf
                      "Group Normal: normal attribute %S must have float3 storage"
                      attribute_name))) in
      Result.bind base_selection (fun base_selection ->
        Result.bind attribute_values (fun attribute_values ->
          Result.bind (finite_positions ?cancel ~grain ~operation:"Group Normal"
              geometry) (fun positions ->
            let geometric_faces = match owner, attribute_values with
              | _, Some _ | Group_primitives, None -> None
              | (Group_points | Group_edges), None -> Some
                  (geometric_face_directions ?cancel ~grain topology positions)
              | Group_vertices, None -> assert false in
            let point_values = match owner, attribute_values, geometric_faces with
              | (Group_points | Group_edges), Some values, _ ->
                  values.x, values.y, values.z
              | Group_edges, None,
                  Some (face_x, face_y, face_z, face_valid) ->
                  geometric_point_directions ?cancel ~grain topology
                    (Option.get index) positions face_x face_y face_z face_valid
              | Group_points, None, Some _
              | Group_primitives, _, _ | Group_vertices, _, _ -> [||], [||], [||]
              | _, None, None -> assert false in
            let owner_matches = match owner, attribute_values, geometric_faces with
              | Group_points, Some _, _ ->
                  let nx, ny, nz = point_values in
                  (fun point -> direction_matches threshold include_opposite
                      dx dy dz nx.(point) ny.(point) nz.(point))
              | Group_points, None, Some (nx, ny, nz, valid) ->
                  let topology = Topology.Private.view topology
                  and index = Topology_index.Private.view (Option.get index) in
                  (fun point -> point_geometric_direction_matches threshold
                      include_opposite dx dy dz positions topology index
                      nx ny nz valid point)
              | Group_primitives, Some values, _ ->
                  (fun primitive -> direction_matches threshold include_opposite
                      dx dy dz values.x.(primitive) values.y.(primitive)
                      values.z.(primitive))
              | Group_primitives, None, Some (nx, ny, nz, valid) ->
                  (fun primitive -> Bytes.unsafe_get valid primitive <> '\000'
                      && direction_matches threshold include_opposite dx dy dz
                           nx.(primitive) ny.(primitive) nz.(primitive))
              | Group_primitives, None, None ->
                  let topology = Topology.Private.view topology in
                  (fun primitive -> primitive_geometric_direction_matches
                      threshold include_opposite dx dy dz positions topology
                      primitive)
              | Group_edges, _, _ ->
                  let nx, ny, nz = point_values
                  and index = Topology_index.Private.view (Option.get index) in
                  (fun edge ->
                    let a = index.edge_a.(edge) and b = index.edge_b.(edge) in
                    let ascale = float_max (abs_float nx.(a))
                        (float_max (abs_float ny.(a)) (abs_float nz.(a)))
                    and bscale = float_max (abs_float nx.(b))
                        (float_max (abs_float ny.(b)) (abs_float nz.(b))) in
                    if ascale = 0. || bscale = 0. then false
                    else
                      let ax = nx.(a) /. ascale and ay = ny.(a) /. ascale
                      and az = nz.(a) /. ascale and bx = nx.(b) /. bscale
                      and by = ny.(b) /. bscale and bz = nz.(b) /. bscale in
                      let ai = 1. /. sqrt ((ax *. ax) +. (ay *. ay) +. (az *. az))
                      and bi = 1. /. sqrt ((bx *. bx) +. (by *. by) +. (bz *. bz)) in
                      direction_matches threshold include_opposite dx dy dz
                        ((ax *. ai) +. (bx *. bi)) ((ay *. ai) +. (by *. bi))
                        ((az *. ai) +. (bz *. bi)))
              | Group_vertices, _, _ | Group_points, None, None -> assert false in
            let predicate element =
              (match base_selection with None -> true
               | Some selection -> selection_mem selection element)
              && owner_matches element in
            let generated = match owner with
              | Group_points | Group_primitives ->
                  let group_owner = Option.get (ordinary_owner owner) in
                  Ordinary (Group.Private.of_owned_bits ~owner:group_owner ~name
                    ~length:count (packed_init ?cancel ~grain count predicate))
              | Group_edges -> Native_edges (Edge_group.Private.of_owned_bits
                  ~topology ~edge_count:count ~name
                  (packed_init ?cancel ~grain count predicate))
              | Group_vertices -> assert false in
            publish_merged_selection ?cancel ~grain ~merge ~owner ~name
              generated geometry)))
    end

let[@inline always] primitive_is_non_planar tolerance
    (positions : Packed.Float3.Private.view)
    (topology : Topology.Private.view) primitive =
  if Bytes.unsafe_get topology.primitive_kinds primitive <> '\000' then false
  else
    let first = topology.primitive_offsets.(primitive)
    and last = topology.primitive_offsets.(primitive + 1) in
    if last - first <= 3 then false
    else
    let scale = ref 0. in
    for vertex = first to last - 1 do
      let point = topology.vertex_points.(vertex) in
      scale := float_max !scale (float_max (abs_float positions.x.(point))
          (float_max (abs_float positions.y.(point))
             (abs_float positions.z.(point))))
    done;
    if !scale = 0. then false
    else
      let a = topology.vertex_points.(first) in
      let ax = positions.x.(a) /. !scale
      and ay = positions.y.(a) /. !scale
      and az = positions.z.(a) /. !scale in
      let b = ref a and farthest = ref 0. in
      for vertex = first + 1 to last - 1 do
        let point = topology.vertex_points.(vertex) in
        let x = (positions.x.(point) /. !scale) -. ax
        and y = (positions.y.(point) /. !scale) -. ay
        and z = (positions.z.(point) /. !scale) -. az in
        let distance = (x *. x) +. (y *. y) +. (z *. z) in
        if distance > !farthest then begin farthest := distance; b := point end
      done;
      if !farthest = 0. then false
      else
        let ux = (positions.x.(!b) /. !scale) -. ax
        and uy = (positions.y.(!b) /. !scale) -. ay
        and uz = (positions.z.(!b) /. !scale) -. az in
        let nx = ref 0. and ny = ref 0. and nz = ref 0.
        and greatest_cross = ref 0. in
        for vertex = first + 1 to last - 1 do
          let point = topology.vertex_points.(vertex) in
          let vx = (positions.x.(point) /. !scale) -. ax
          and vy = (positions.y.(point) /. !scale) -. ay
          and vz = (positions.z.(point) /. !scale) -. az in
          let x = (uy *. vz) -. (uz *. vy)
          and y = (uz *. vx) -. (ux *. vz)
          and z = (ux *. vy) -. (uy *. vx) in
          let magnitude = (x *. x) +. (y *. y) +. (z *. z) in
          if magnitude > !greatest_cross then begin
            greatest_cross := magnitude; nx := x; ny := y; nz := z
          end
        done;
        if !greatest_cross = 0. then false
        else
          let inverse = 1. /. sqrt !greatest_cross in
          let nx = !nx *. inverse and ny = !ny *. inverse
          and nz = !nz *. inverse and limit = tolerance /. !scale in
          let vertex = ref first and non_planar = ref false in
          while not !non_planar && !vertex < last do
            let point = topology.vertex_points.(!vertex) in
            let x = (positions.x.(point) /. !scale) -. ax
            and y = (positions.y.(point) /. !scale) -. ay
            and z = (positions.z.(point) /. !scale) -. az in
            non_planar := abs_float ((nx *. x) +. (ny *. y) +. (nz *. z))
                > limit;
            incr vertex
          done;
          !non_planar

let group_non_planar ?cancel ?(grain = 16_384) ?base
    ?(merge = Group_replace) ~tolerance ~name geometry =
  if grain <= 0 then Error "Group Non-Planar: grain must be positive"
  else if String.trim name = "" then Error "Group Non-Planar: empty output name"
  else if not (Float.is_finite tolerance) || tolerance < 0. then Error
      "Group Non-Planar: tolerance must be finite and non-negative"
  else
    let base_selection = match base with
      | None -> Ok None
      | Some base_name when String.trim base_name = "" ->
          Error "Group Non-Planar: empty base group name"
      | Some base_name ->
          (match find_selection Group_primitives base_name geometry with
           | Some selection -> Ok (Some selection)
           | None -> Error (Printf.sprintf
               "Group Non-Planar: missing primitive base group %S" base_name)) in
    Result.bind base_selection (fun base_selection ->
      Result.bind (finite_positions ?cancel ~grain
          ~operation:"Group Non-Planar" geometry) (fun positions ->
        let topology = Geometry.topology geometry in
        let topology_view = Topology.Private.view topology
        and count = Topology.primitive_count topology in
        let predicate primitive =
          (match base_selection with None -> true
           | Some selection -> selection_mem selection primitive)
          && primitive_is_non_planar tolerance positions topology_view primitive in
        let generated = Ordinary (Group.Private.of_owned_bits
            ~owner:Group.Primitive ~name ~length:count
            (packed_init ?cancel ~grain count predicate)) in
        publish_merged_selection ?cancel ~grain ~merge
          ~owner:Group_primitives ~name generated geometry))

let[@inline always] primitive_is_backface viewpoint
    (positions : Packed.Float3.Private.view)
    (topology : Topology.Private.view) primitive =
  if Bytes.unsafe_get topology.primitive_kinds primitive <> '\000' then false
  else
    let first = topology.primitive_offsets.(primitive)
    and last = topology.primitive_offsets.(primitive + 1) in
    let geometry_scale = ref 0. in
    for vertex = first to last - 1 do
      let point = topology.vertex_points.(vertex) in
      geometry_scale := float_max !geometry_scale
          (float_max (abs_float positions.x.(point))
             (float_max (abs_float positions.y.(point))
                (abs_float positions.z.(point))))
    done;
    if !geometry_scale = 0. then false
    else
      let anchor = topology.vertex_points.(first) in
      let ax = positions.x.(anchor) /. !geometry_scale
      and ay = positions.y.(anchor) /. !geometry_scale
      and az = positions.z.(anchor) /. !geometry_scale in
      let nx = ref 0. and ny = ref 0. and nz = ref 0.
      and cx = ref 0. and cy = ref 0. and cz = ref 0. in
      for vertex = first to last - 1 do
        let point = topology.vertex_points.(vertex) in
        cx := !cx +. (positions.x.(point) /. !geometry_scale);
        cy := !cy +. (positions.y.(point) /. !geometry_scale);
        cz := !cz +. (positions.z.(point) /. !geometry_scale)
      done;
      for vertex = first + 1 to last - 2 do
        let b = topology.vertex_points.(vertex)
        and c = topology.vertex_points.(vertex + 1) in
        let ux = (positions.x.(b) /. !geometry_scale) -. ax
        and uy = (positions.y.(b) /. !geometry_scale) -. ay
        and uz = (positions.z.(b) /. !geometry_scale) -. az
        and vx = (positions.x.(c) /. !geometry_scale) -. ax
        and vy = (positions.y.(c) /. !geometry_scale) -. ay
        and vz = (positions.z.(c) /. !geometry_scale) -. az in
        nx := !nx +. ((uy *. vz) -. (uz *. vy));
        ny := !ny +. ((uz *. vx) -. (ux *. vz));
        nz := !nz +. ((ux *. vy) -. (uy *. vx))
      done;
      let normal_scale = float_max (abs_float !nx)
          (float_max (abs_float !ny) (abs_float !nz)) in
      if normal_scale = 0. then false
      else
        let inverse_count = 1. /. float_of_int (last - first) in
        let cx = !cx *. inverse_count and cy = !cy *. inverse_count
        and cz = !cz *. inverse_count in
        let view_scale = float_max !geometry_scale
            (float_max (abs_float viewpoint.Rays_math.Vec3.x)
               (float_max (abs_float viewpoint.y) (abs_float viewpoint.z))) in
        let geometry_ratio = !geometry_scale /. view_scale in
        let vx = (viewpoint.x /. view_scale) -. (cx *. geometry_ratio)
        and vy = (viewpoint.y /. view_scale) -. (cy *. geometry_ratio)
        and vz = (viewpoint.z /. view_scale) -. (cz *. geometry_ratio) in
        let vector_scale = float_max (abs_float vx)
            (float_max (abs_float vy) (abs_float vz)) in
        if vector_scale = 0. then false
        else
          let dot = ((!nx /. normal_scale) *. (vx /. vector_scale))
              +. ((!ny /. normal_scale) *. (vy /. vector_scale))
              +. ((!nz /. normal_scale) *. (vz /. vector_scale)) in
          dot < -. (64. *. Float.epsilon)

let group_backface ?cancel ?(grain = 16_384) ?base
    ?(merge = Group_replace) ~viewpoint ~name geometry =
  if grain <= 0 then Error "Group Backface: grain must be positive"
  else if String.trim name = "" then Error "Group Backface: empty output name"
  else if not (finite_vec3 viewpoint) then Error
      "Group Backface: viewpoint must be finite"
  else
    let base_selection = match base with
      | None -> Ok None
      | Some base_name when String.trim base_name = "" ->
          Error "Group Backface: empty base group name"
      | Some base_name ->
          (match find_selection Group_primitives base_name geometry with
           | Some selection -> Ok (Some selection)
           | None -> Error (Printf.sprintf
               "Group Backface: missing primitive base group %S" base_name)) in
    Result.bind base_selection (fun base_selection ->
      Result.bind (finite_positions ?cancel ~grain
          ~operation:"Group Backface" geometry) (fun positions ->
        let topology = Geometry.topology geometry in
        let topology_view = Topology.Private.view topology
        and count = Topology.primitive_count topology in
        let predicate primitive =
          (match base_selection with None -> true
           | Some selection -> selection_mem selection primitive)
          && primitive_is_backface viewpoint positions topology_view primitive in
        let generated = Ordinary (Group.Private.of_owned_bits
            ~owner:Group.Primitive ~name ~length:count
            (packed_init ?cancel ~grain count predicate)) in
        publish_merged_selection ?cancel ~grain ~merge
          ~owner:Group_primitives ~name generated geometry))

let positive_mod value modulus =
  let value = value mod modulus in if value < 0 then value + modulus else value

type range_connectivity_configuration = {
  range_attributes : string option;
  range_tolerance : float;
  range_collision : range_collision option;
  range_region : int option;
  range_remove_other_regions : bool;
}

let range_connectivity_configuration = function
  | Range_disconnected { region } -> {
      range_attributes = None;
      range_tolerance = 0.;
      range_collision = None;
      range_region = region;
      range_remove_other_regions = true;
    }
  | Range_connected { connectivity_attributes; connectivity_tolerance;
      collision; region; remove_other_regions } -> {
      range_attributes = Option.bind connectivity_attributes (fun pattern ->
        let pattern = String.trim pattern in
        if String.equal pattern "" then None else Some pattern);
      range_tolerance = connectivity_tolerance;
      range_collision = collision;
      range_region = region;
      range_remove_other_regions = remove_other_regions;
    }

let range_boundary_edges ?cancel ~grain ~store ~owner configuration geometry =
  let extract_edges name geometry = match Geometry.find_edge_group name geometry with
    | Some edges -> Ok edges
    | None -> Error "Group Range: internal boundary edge output missing" in
  let attribute_edges = match configuration.range_attributes with
    | None -> Ok None
    | Some pattern ->
        let attribute_owner = ordinary_attribute_owner owner in
        let edge_name = fresh_edge_group_name geometry
            "__rdk_group_range_attribute_edges_" in
        Result.bind (group_from_attribute_boundary ?cancel ~grain
            ~attributes:[{ boundary_attribute_owner = attribute_owner;
              boundary_attribute_pattern = pattern }]
            ~tolerance:configuration.range_tolerance ~owner:Group_edges
            ~name:edge_name geometry) (fun temporary ->
          Result.map Option.some (extract_edges edge_name temporary)) in
  let collision_edges = match configuration.range_collision with
    | None -> Ok None
    | Some collision ->
        Result.bind (resolve_pattern ?cancel ~grain store
            collision.collision_owner collision.collision_pattern geometry)
          (fun selected -> match selected with
            | Native_edges edges -> Ok (Some edges)
            | Ordinary _ ->
                let edge_name = fresh_edge_group_name geometry
                    "__rdk_group_range_collision_edges_" in
                Result.bind (group_from_attribute_boundary ?cancel ~grain
                    ~membership_boundary:
                      (ordinary_attribute_owner collision.collision_owner, selected)
                    ~owner:Group_edges ~name:edge_name geometry)
                  (fun temporary ->
                    Result.map Option.some (extract_edges edge_name temporary))) in
  Result.bind attribute_edges (fun attribute_edges ->
    Result.bind collision_edges (fun collision_edges ->
      let seams = match attribute_edges, collision_edges with
        | None, None -> Ok None
        | Some edges, None | None, Some edges -> Ok (Some edges)
        | Some attributes, Some collision ->
            Result.map Option.some (Edge_group.union attributes collision) in
      Result.map (fun seams ->
        let seams = match seams with
          | Some edges when Edge_group.cardinality edges = 0 -> None
          | None | Some _ as value -> value in
        let included = match configuration.range_collision, collision_edges with
          | Some { keep_boundary = false; _ }, Some boundary ->
              let boundary = promoted_selection ?cancel ~grain
                  ~source:Group_edges ~destination:owner ~mode:Include_any
                  ~name:"__rdk_group_range_collision_boundary"
                  (Native_edges boundary) geometry in
              (match boundary with
               | Ordinary group -> Some (Group.complement group)
               | Native_edges _ -> assert false)
          | None, _ | Some { keep_boundary = true; _ }, _
          | Some { keep_boundary = false; _ }, None -> None in
        seams, included) seams))

let range ?cancel ?(grain = 16_384) ?base ?(invert = false) ?filter
    ?connectivity ?(merge = Group_replace) ~owner ~name specification geometry =
  if grain <= 0 then Error "Group Range: grain must be positive"
  else if String.trim name = "" then Error "Group Range: empty output name"
  else if (match connectivity, owner with
      | Some _, (Group_vertices | Group_edges) -> true
      | None, _ | Some _, (Group_points | Group_primitives) -> false) then
    Error "Group Range: connectivity is available only for points and primitives"
  else if (match connectivity with
      | Some connectivity ->
          (match (range_connectivity_configuration connectivity).range_region with
           | Some region -> region < 0 | None -> false)
      | None -> false) then
    Error "Group Range: connected region must be non-negative"
  else if (match connectivity with
      | Some (Range_connected { connectivity_tolerance; _ }) ->
          not (Float.is_finite connectivity_tolerance)
          || connectivity_tolerance < 0.
      | None | Some (Range_disconnected _) -> false) then
    Error "Group Range: connectivity tolerance must be finite and non-negative"
  else if (match connectivity with
      | Some (Range_connected { collision = Some collision; _ }) ->
          String.trim collision.collision_pattern = ""
      | None | Some (Range_disconnected _)
      | Some (Range_connected { collision = None; _ }) -> false) then
    Error "Group Range: empty collision group pattern"
  else match filter with
    | Some { select; of_; _ } when of_ <= 0 || select < 0 || select > of_ ->
        Error "Group Range: filter requires 0 <= select <= of and of > 0"
    | _ ->
      (try
        Cancel.check_opt cancel;
        let topology = Geometry.topology geometry in
        let index = if owner = Group_edges
          then Some (Topology_index.create ?cancel topology) else None in
        let count = match owner with
          | Group_points -> Geometry.point_count geometry
          | Group_vertices -> Geometry.vertex_count geometry
          | Group_primitives -> Geometry.primitive_count geometry
          | Group_edges -> Topology_index.edge_count (Option.get index) in
        let first, last = range_bounds count specification in
        let store = store_of_geometry geometry in
        let base_result = match base with
          | None -> Ok None
          | Some pattern -> Result.map Option.some
              (resolve_pattern ?cancel ~grain store owner pattern geometry) in
        let connectivity_result () = match connectivity with
          | None -> Ok None
          | Some connectivity ->
              let configuration = range_connectivity_configuration connectivity in
              Result.bind (range_boundary_edges ?cancel ~grain ~store ~owner
                  configuration geometry) (fun (seams, included) ->
                let classification = match owner with
                  | Group_points -> Analysis.classify_connectivity ?cancel ~grain
                      ?points:included ?seams Analysis.Connectivity_points geometry
                  | Group_primitives -> Analysis.classify_connectivity ?cancel
                      ~grain ?primitives:included ?seams
                      Analysis.Connectivity_primitives geometry
                  | Group_vertices | Group_edges -> assert false in
                match classification with
                | Error error when Error.code error = "cancelled" ->
                    raise Cancel.Cancelled
                | Error error -> Error (Error.to_string error)
                | Ok value -> Ok value)
              |> Result.map (fun (classes, component_count) ->
                let local_indices = Array.make count 0
                and component_sizes = Array.make component_count 0 in
                for element = 0 to count - 1 do
                  if element land 16_383 = 0 then Cancel.check_opt cancel;
                  let component = classes.(element) in
                  if component >= 0 then begin
                    local_indices.(element) <- component_sizes.(component);
                    component_sizes.(component) <- component_sizes.(component) + 1
                  end
                done;
                let firsts = component_sizes
                and lasts = Array.make component_count (-1) in
                for component = 0 to component_count - 1 do
                  let first, last = range_bounds firsts.(component)
                      specification in
                  firsts.(component) <- first;
                  lasts.(component) <- last
                done;
                Some (classes, local_indices, firsts, lasts,
                  configuration.range_region,
                  configuration.range_remove_other_regions)) in
        Result.bind base_result (fun base_selection ->
          Result.bind (connectivity_result ()) (fun connectivity_data ->
          let filter_passes ~element ~first = match filter with
            | None -> true
            | Some { select; of_; offset } ->
                let phase = positive_mod
                    (positive_mod element of_ - positive_mod first of_
                     - positive_mod offset of_) of_ in
                phase < select in
          let selected element =
            let in_base = match base_selection with
              | None -> true | Some value -> selection_mem value element in
            if not in_base then false
            else match connectivity_data with
              | None ->
                  let in_range = element >= first && element <= last
                      && filter_passes ~element ~first in
                  if invert then not in_range else in_range
              | Some (classes, local_indices, firsts, lasts, only_region,
                  remove_other_regions) ->
                  let component = classes.(element) in
                  if component < 0 then false
                  else if (match only_region with
                      | Some region -> component <> region | None -> false) then
                    not remove_other_regions
                  else
                    let local = local_indices.(element)
                    and first = firsts.(component)
                    and last = lasts.(component) in
                    let in_range = local >= first && local <= last
                        && filter_passes ~element:local ~first in
                    if invert then not in_range else in_range in
          let generated = match ordinary_owner owner with
            | Some group_owner -> Ordinary (Group.init ~grain ~owner:group_owner
                ~name count (fun element ->
                  if element land 16_383 = 0 then Cancel.check_opt cancel;
                  selected element))
            | None -> Native_edges (Edge_group.init ~grain ~topology
                ~index:(Option.get index) ~name (fun edge ->
                  if edge land 16_383 = 0 then Cancel.check_opt cancel;
                  selected edge)) in
          let existing = Option.map (fun entry -> entry.value)
              (store_find store owner name) in
          let output = match merge, existing with
            | Group_replace, _ | (Group_union | Group_xor), None -> Ok generated
            | Group_intersection, None | Group_subtract, None ->
                Ok (empty_selection ?cancel ~grain owner geometry)
            | operation, Some existing ->
                selection_boolean operation existing generated in
          Result.bind output (fun output ->
            match store_find store owner name with
            | Some entry -> store_replace store entry output;
                store_commit store geometry
            | None -> ignore (store_add store owner name output);
                store_commit store geometry)))
      with Invalid_argument message -> Error message)

let ranges ?cancel ?(grain = 16_384) rules geometry =
  if grain <= 0 then Error "Group Ranges: grain must be positive"
  else
    let rec apply geometry = function
      | [] -> Ok geometry
      | rule :: rest ->
          Cancel.check_opt cancel;
          if String.trim rule.range_name = "" then apply geometry rest
          else Result.bind
              (range ?cancel ~grain ?base:rule.range_base
                ~invert:rule.range_invert ?filter:rule.range_filter
                ?connectivity:rule.range_connectivity ~merge:rule.range_merge
                ~owner:rule.range_owner ~name:rule.range_name
                rule.range_specification geometry)
              (fun geometry -> apply geometry rest)
    in
    apply geometry rules

let owner_selected selected owner = match selected with
  | None -> true | Some expected -> expected = owner

let rename_entry store ~conflict entry new_name value =
  if String.equal entry.entry_name new_name then begin
    store_replace store entry value;
    Ok ()
  end else match store_find store entry.owner new_name with
    | None -> store_rename store entry new_name value; Ok ()
    | Some destination when destination == entry ->
        store_replace store entry value; Ok ()
    | Some destination ->
        (match conflict with
         | Rename_skip -> Ok ()
         | Rename_error -> Error (Printf.sprintf
             "Group Rename: destination %s group %S already exists"
             (owner_name entry.owner) new_name)
         | Rename_overwrite ->
             store_remove store destination;
             store_rename store entry new_name value;
             Ok ()
         | Rename_union ->
             Result.map (fun combined ->
               store_replace store destination combined;
               store_remove store entry)
               (selection_boolean Group_union destination.value value))

let rename ~rules geometry =
  Error.guard ~operation:"group_rename" ~code:"invalid_group" @@ fun () ->
  let store = store_of_geometry geometry in
  let result = List.fold_left (fun result rule -> Result.bind result (fun () ->
    Result.bind (Attribute_pattern.compile_rewrite
      ~pattern:rule.rename_pattern ~replacement:rule.rename_replacement)
      (fun rewrite ->
        let snapshot = store_entries store in
        List.fold_left (fun result entry -> Result.bind result (fun () ->
          if not entry.alive || not (owner_selected rule.rename_owner entry.owner)
          then Ok ()
          else match Attribute_pattern.rewrite rewrite entry.entry_name with
            | None -> Ok ()
            | Some new_name when String.trim new_name = "" ->
                Error "Group Rename: rewrite produced an empty group name"
            | Some new_name -> rename_entry store
                ~conflict:rule.rename_conflict entry new_name entry.value))
          (Ok ()) snapshot))) (Ok ()) rules in
  Result.bind result (fun () -> store_commit store geometry)

let invert ?(conflict = Rename_overwrite) ?owner ~pattern ?new_name geometry =
  let store = store_of_geometry geometry in
  Result.bind (compile_nonblank "Group Invert" pattern) (fun compiled ->
    let rewrite = match new_name with
      | None -> Ok None
      | Some replacement -> Result.map Option.some
          (Attribute_pattern.compile_rewrite ~pattern ~replacement) in
    Result.bind rewrite (fun rewrite ->
      let snapshot = List.filter (fun entry -> entry.alive
          && owner_selected owner entry.owner
          && Attribute_pattern.matches compiled entry.entry_name)
          (store_entries store) in
      let result = List.fold_left (fun result entry -> Result.bind result (fun () ->
        if not entry.alive then Ok ()
        else
          let output_name = match rewrite with
            | None -> entry.entry_name
            | Some rewrite -> Option.value ~default:entry.entry_name
                (Attribute_pattern.rewrite rewrite entry.entry_name) in
          if String.trim output_name = "" then
            Error "Group Invert: rewrite produced an empty group name"
          else rename_entry store ~conflict entry output_name
              (selection_complement entry.value))) (Ok ()) snapshot in
      Result.bind result (fun () -> store_commit store geometry)))

let delete ~rules ?(delete_unused = false) geometry =
  Error.guard ~operation:"group_delete" ~code:"invalid_group" @@ fun () ->
  let store = store_of_geometry geometry in
  let compiled = List.fold_left (fun result rule -> Result.bind result
      (fun rules ->
        if String.trim rule.delete_pattern = "" then Ok rules
        else Result.map (fun pattern -> (rule.delete_owner, pattern) :: rules)
            (Attribute_pattern.compile rule.delete_pattern))) (Ok []) rules in
  Result.bind compiled (fun compiled ->
    List.iter (fun entry -> if entry.alive then begin
      let matched = List.exists (fun (owner, pattern) ->
        owner_selected owner entry.owner
        && Attribute_pattern.matches pattern entry.entry_name) compiled in
      let empty = delete_unused && selection_cardinality entry.value = 0 in
      if matched || empty then store_remove store entry
    end) (store_entries store);
    store_commit store geometry)

type element_map =
  | Identity of int
  | Explicit of int array
  | Proximity of { indices : int array; distances_squared : float array }

let mapped_source mapping target = match mapping with
  | Identity source_count -> if target < source_count then target else -1
  | Explicit values -> values.(target)
  | Proximity values -> values.indices.(target)

let attribute_owner = function
  | Group_points -> Attribute.Point
  | Group_vertices -> Attribute.Vertex
  | Group_primitives -> Attribute.Primitive
  | Group_edges -> invalid_arg "Group Copy: edges cannot match by attribute"

let key_map_error map =
  Option.to_result ~none:"Group Copy: attribute match table exceeds array limits" map

let integer_first_map values =
  key_map_error (Support.Key_map.(ints First) values)

let string_first_map values =
  key_map_error (Support.Key_map.(strings First) values)

let attribute_match_map ?cancel ~grain owner name source target =
  let attribute_owner = attribute_owner owner in
  match Geometry.find_attribute ~owner:attribute_owner name source,
      Geometry.find_attribute ~owner:attribute_owner name target with
  | None, _ -> Error (Printf.sprintf
      "Group Copy: source %s attribute %S is missing" (owner_name owner) name)
  | _, None -> Error (Printf.sprintf
      "Group Copy: target %s attribute %S is missing" (owner_name owner) name)
  | Some source_attribute, Some target_attribute ->
      let source_storage = Attribute.Private.storage source_attribute
      and target_storage = Attribute.Private.storage target_attribute in
      let target_count = Attribute.length target_attribute in
      let mapping = Array.make target_count (-1) in
      (match source_storage, target_storage with
       | Attribute.Int source_values, Attribute.Int target_values ->
           Result.map (fun find ->
             if target_count > 0 then
               Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(target_count - 1)
                 (fun index ->
                   if index land 16_383 = 0 then Cancel.check_opt cancel;
                   mapping.(index) <- find target_values.(index));
             Explicit mapping) (integer_first_map source_values)
       | Attribute.Text source_values, Attribute.Text target_values ->
           Result.map (fun find ->
             if target_count > 0 then
               Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(target_count - 1)
                 (fun index ->
                   if index land 16_383 = 0 then Cancel.check_opt cancel;
                   mapping.(index) <- find target_values.(index));
             Explicit mapping) (string_first_map source_values)
       | (Attribute.Int _ | Attribute.Text _), _
       | _, (Attribute.Int _ | Attribute.Text _) -> Error (Printf.sprintf
           "Group Copy: %s attribute %S has different source/target types"
           (owner_name owner) name)
       | _ -> Error (Printf.sprintf
           "Group Copy: %s attribute %S must be integer or text"
           (owner_name owner) name))

let default_copy_map ?cancel owner source target = match owner with
  | Group_points -> Identity (Geometry.point_count source)
  | Group_primitives -> Identity (Geometry.primitive_count source)
  | Group_edges ->
      let source_index = Topology_index.create ?cancel (Geometry.topology source) in
      Identity (Topology_index.edge_count source_index)
  | Group_vertices ->
      let source_topology = Topology.Private.view (Geometry.topology source)
      and target_topology = Topology.Private.view (Geometry.topology target) in
      let mapping = Array.make (Array.length target_topology.vertex_points) (-1) in
      for primitive = 0 to Bytes.length target_topology.primitive_kinds - 1 do
        if primitive land 16_383 = 0 then Cancel.check_opt cancel;
        if primitive < Bytes.length source_topology.primitive_kinds then begin
          let source_first = source_topology.primitive_offsets.(primitive)
          and source_size = source_topology.primitive_offsets.(primitive + 1)
              - source_topology.primitive_offsets.(primitive)
          and target_first = target_topology.primitive_offsets.(primitive)
          and target_size = target_topology.primitive_offsets.(primitive + 1)
              - target_topology.primitive_offsets.(primitive) in
          for local = 0 to min source_size target_size - 1 do
            mapping.(target_first + local) <- source_first + local
          done
        end
      done;
      Explicit mapping

let target_owner_count ?cancel owner geometry = match owner with
  | Group_points -> Geometry.point_count geometry
  | Group_vertices -> Geometry.vertex_count geometry
  | Group_primitives -> Geometry.primitive_count geometry
  | Group_edges -> Topology_index.edge_count
      (Topology_index.create ?cancel (Geometry.topology geometry))

let copied_selection ?cancel ~grain owner mapping source_group target =
  let count = target_owner_count ?cancel owner target in
  let member element =
    let source = mapped_source mapping element in
    source >= 0 && selection_mem source_group source in
  match ordinary_owner owner with
  | Some group_owner ->
      let target_group = Group.init ~grain ~owner:group_owner
          ~name:"__copied" count (fun element ->
            if element land 16_383 = 0 then Cancel.check_opt cancel;
            member element) in
      let target_group = match source_group with
        | Native_edges _ -> target_group
        | Ordinary source_group when not (Group.is_ordered source_group) ->
            target_group
        | Ordinary source_group ->
            begin match mapping with
            | Explicit source_of_target ->
                Group.Private.remap_order ~source:source_group ~source_of_target
                  target_group
            | Identity source_count ->
                let source_of_target = Array.init count (fun target ->
                    if target < source_count then target else -1) in
                Group.Private.remap_order ~source:source_group ~source_of_target
                  target_group
            | Proximity { indices; distances_squared } ->
                let source_order = Option.get
                    (Group.Private.order_view source_group) in
                let rank = Array.make (Group.length source_group) (-1) in
                Array.iteri (fun position element -> rank.(element) <- position)
                  source_order;
                let order = Array.make (Group.cardinality target_group) 0
                and next = ref 0 in
                Group.iter (fun target_element ->
                  order.(!next) <- target_element;
                  incr next) target_group;
                Array.sort (fun left right ->
                  let left_source = indices.(left)
                  and right_source = indices.(right) in
                  let by_source = Int.compare rank.(left_source) rank.(right_source) in
                  if by_source <> 0 then by_source
                  else
                    let by_distance = Float.compare distances_squared.(left)
                        distances_squared.(right) in
                    if by_distance <> 0 then by_distance
                    else Int.compare left right) order;
                Group.Private.with_owned_order order target_group
            end in
      Ordinary target_group
  | None ->
      let topology = Geometry.topology target in
      let index = Topology_index.create ?cancel topology in
      Native_edges (Edge_group.init ~grain ~topology ~index ~name:"__copied"
        (fun edge ->
          if edge land 16_383 = 0 then Cancel.check_opt cancel;
          member edge))

let unique_suffix store owner base =
  let suffix = ref 2 and candidate = ref (base ^ "2") in
  while store_find store owner !candidate <> None do
    if !suffix = max_int then invalid_arg "Group Copy: suffix space exhausted";
    incr suffix;
    candidate := base ^ string_of_int !suffix
  done;
  !candidate

let default_copy_rules = [
  { copy_owner = Group_primitives; copy_pattern = "*"; copy_prefix = "";
    match_attribute = None };
  { copy_owner = Group_points; copy_pattern = "*"; copy_prefix = "";
    match_attribute = None };
  { copy_owner = Group_edges; copy_pattern = "*"; copy_prefix = "";
    match_attribute = None };
  { copy_owner = Group_vertices; copy_pattern = "*"; copy_prefix = "";
    match_attribute = None };
]

let copy ?cancel ?(grain = 16_384) ?(rules = default_copy_rules)
    ?(conflict = Copy_skip) ?(copy_empty = false) ~source ~target () =
  Error.guard ~operation:"group_copy" ~code:"invalid_group" @@ fun () ->
  if grain <= 0 then Error "Group Copy: grain must be positive"
  else begin
    let source_store = store_of_geometry source and target_store = store_of_geometry target in
    let result = List.fold_left (fun result rule -> Result.bind result (fun () ->
      Cancel.check_opt cancel;
      if rule.copy_owner = Group_edges && rule.match_attribute <> None then
        Error "Group Copy: native edges cannot match by attribute"
      else
        let pattern_source = if String.trim rule.copy_pattern = ""
          then "*" else rule.copy_pattern in
        Result.bind (Attribute_pattern.compile pattern_source) (fun pattern ->
          let sources = List.filter (fun entry -> entry.alive
              && entry.owner = rule.copy_owner
              && Attribute_pattern.matches pattern entry.entry_name)
              (store_entries source_store) in
          if sources = [] then Ok ()
          else
            let mapping = match rule.match_attribute with
              | None -> Ok (default_copy_map ?cancel rule.copy_owner source target)
              | Some name when String.trim name = "" ->
                  Error "Group Copy: empty match attribute name"
              | Some name -> attribute_match_map ?cancel ~grain rule.copy_owner
                  name source target in
            Result.bind mapping (fun mapping ->
              List.fold_left (fun result source_entry ->
                Result.bind result (fun () ->
                  let proposed = rule.copy_prefix ^ source_entry.entry_name in
                  if String.trim proposed = "" then
                    Error "Group Copy: prefix produced an empty group name"
                  else
                    let existing = store_find target_store rule.copy_owner proposed in
                    match conflict, existing with
                    | Copy_skip, Some _ -> Ok ()
                    | _ ->
                        let output_name = match conflict, existing with
                          | Copy_add_suffix, Some _ ->
                              unique_suffix target_store rule.copy_owner proposed
                          | _ -> proposed in
                        let copied = copied_selection ?cancel ~grain
                            rule.copy_owner mapping source_entry.value target in
                        if not copy_empty && selection_cardinality copied = 0 then Ok ()
                        else begin
                          match store_find target_store rule.copy_owner output_name with
                          | Some destination ->
                              store_replace target_store destination copied; Ok ()
                          | None ->
                              ignore (store_add target_store rule.copy_owner output_name copied);
                              Ok ()
                        end)) (Ok ()) sources)))
    ) (Ok ()) rules in
    Result.bind result (fun () -> store_commit target_store target)
  end

let default_transfer_rules = [
  { transfer_owner = Group_primitives; transfer_pattern = "*";
    transfer_prefix = "" };
  { transfer_owner = Group_points; transfer_pattern = "*";
    transfer_prefix = "" };
  { transfer_owner = Group_edges; transfer_pattern = "*";
    transfer_prefix = "" };
]

let point_proximity_map ?cancel ~grain ~maximum_squared source target =
  Result.bind (Spatial_index.create ?cancel ~grain (Geometry.positions source)
      |> Result.map_error Error.to_string) (fun index ->
    let count = Geometry.point_count target in
    let indices = Array.make count (-1)
    and distances = Array.make count Float.infinity
    and counts = Array.make count 0 in
    Spatial_index.Private.nearest_k_many_into ?cancel ~grain index
      ~queries:(Geometry.positions target) ~max_distance_squared:maximum_squared
      ~capacity:1 ~indices ~distances_squared:distances ~counts;
    Ok (Proximity { indices; distances_squared = distances }))

let feature_proximity_map ?cancel ~grain ~distance ~source_features
    ~target_features () =
  Result.bind (source_features ()) (fun source_features ->
    let source_index = Proximity_index.create ?cancel ~grain source_features in
    Result.bind (target_features ()) (fun target_features ->
      Result.map (fun (indices, distances_squared) ->
        Proximity { indices; distances_squared })
        (Proximity_index.nearest_entities_with_distances ?cancel ~grain
          ~max_distance:distance source_index target_features)))

let transfer_mapping ?cancel ~grain ~distance ~maximum_squared owner source target =
  match owner with
  | Group_points -> point_proximity_map ?cancel ~grain ~maximum_squared
      source target
  | Group_primitives -> feature_proximity_map ?cancel ~grain ~distance
      ~source_features:(fun () ->
        Proximity_index.primitive_features ?cancel ~grain source)
      ~target_features:(fun () ->
        Proximity_index.primitive_features ?cancel ~grain target) ()
  | Group_edges -> feature_proximity_map ?cancel ~grain ~distance
      ~source_features:(fun () ->
        Proximity_index.edge_features ?cancel ~grain source)
      ~target_features:(fun () ->
        Proximity_index.edge_features ?cancel ~grain target) ()
  | Group_vertices -> Error "Group Transfer: vertex groups are not supported"

let transfer ?cancel ?(grain = 16_384) ?(rules = default_transfer_rules)
    ?(conflict = Copy_skip) ?(create_empty = false) ?(distance = 0.001)
    ~source ~target () =
  Error.guard ~operation:"group_transfer" ~code:"invalid_group" @@ fun () ->
  if grain <= 0 then Error "Group Transfer: grain must be positive"
  else if not (Float.is_finite distance) || distance < 0.
      || distance > sqrt max_float then
    Error "Group Transfer: distance threshold must be finite, non-negative, and safely squarable"
  else
    try
      let maximum_squared = distance *. distance in
      let source_store = store_of_geometry source
      and target_store = store_of_geometry target in
      let point_mapping = ref None and primitive_mapping = ref None
      and edge_mapping = ref None in
      let mapping_for owner =
        let slot = match owner with
          | Group_points -> point_mapping
          | Group_primitives -> primitive_mapping
          | Group_edges -> edge_mapping
          | Group_vertices -> invalid_arg
              "Group Transfer: vertex groups are not supported" in
        match !slot with
        | Some result -> result
        | None ->
            let result = transfer_mapping ?cancel ~grain ~distance
                ~maximum_squared owner source target in
            slot := Some result;
            result in
      let transfer_source rule mapping source_entry =
        let proposed = rule.transfer_prefix ^ source_entry.entry_name in
        if String.trim proposed = "" then
          Error "Group Transfer: prefix produced an empty group name"
        else
          let existing = store_find target_store rule.transfer_owner proposed in
          match conflict, existing with
          | Copy_skip, Some _ -> Ok ()
          | _ ->
              let output_name = match conflict, existing with
                | Copy_add_suffix, Some _ ->
                    unique_suffix target_store rule.transfer_owner proposed
                | _ -> proposed in
              let copied = copied_selection ?cancel ~grain rule.transfer_owner
                  mapping source_entry.value target in
              if not create_empty && selection_cardinality copied = 0 then Ok ()
              else begin
                match store_find target_store rule.transfer_owner output_name with
                | Some destination ->
                    store_replace target_store destination copied; Ok ()
                | None ->
                    ignore (store_add target_store rule.transfer_owner output_name
                      copied);
                    Ok ()
              end in
      let transfer_rule rule =
        Cancel.check_opt cancel;
        if rule.transfer_owner = Group_vertices then
          Error "Group Transfer: vertex groups are not supported"
        else
          let pattern_source = if String.trim rule.transfer_pattern = ""
            then "*" else rule.transfer_pattern in
          Result.bind (Attribute_pattern.compile pattern_source) (fun pattern ->
            let sources = List.filter (fun entry -> entry.alive
                && entry.owner = rule.transfer_owner
                && Attribute_pattern.matches pattern entry.entry_name)
                (store_entries source_store) in
            if sources = [] then Ok ()
            else Result.bind (mapping_for rule.transfer_owner) (fun mapping ->
              List.fold_left (fun result source_entry ->
                Result.bind result (fun () ->
                  transfer_source rule mapping source_entry))
                (Ok ()) sources)) in
      let result = List.fold_left (fun result rule ->
        Result.bind result (fun () -> transfer_rule rule)) (Ok ()) rules in
      Result.bind result (fun () -> store_commit target_store target)
    with Invalid_argument message -> Error message

let expand ?cancel ?grain ?name ?steps ?flood ?step_attribute
    ?primitive_connectivity ?normal_spread ?normal_attribute
    ?connectivity_attributes ?connectivity_tolerance ?collision
    ~owner ~group geometry =
  Error.guard ~operation:"group_expand" ~code:"invalid_group" (fun () ->
    expand ?cancel ?grain ?name ?steps ?flood ?step_attribute
      ?primitive_connectivity ?normal_spread ?normal_attribute
      ?connectivity_attributes ?connectivity_tolerance ?collision
      ~owner ~group geometry)

let promotions ?cancel ?grain ?max_outputs ?max_payload_bytes
    ~rules geometry =
  Error.guard ~operation:"group_promotions" ~code:"invalid_group" (fun () ->
    promotions ?cancel ?grain ?max_outputs ?max_payload_bytes rules geometry)

let groups_from_name ?cancel ?grain ?prefix ?conflict ?invalid_names
    ?max_groups ?max_payload_bytes ~owner ~attribute geometry =
  Error.guard ~operation:"groups_from_name" ~code:"invalid_group" (fun () ->
    groups_from_name ?cancel ?grain ?prefix ?conflict ?invalid_names
      ?max_groups ?max_payload_bytes ~owner ~attribute geometry)

let name_from_groups ?cancel ?grain ?attribute ?pattern ?default
    ?overlap ?delete_groups ~owner geometry =
  Error.guard ~operation:"name_from_groups" ~code:"invalid_group" (fun () ->
    name_from_groups ?cancel ?grain ?attribute ?pattern ?default ?overlap
      ?delete_groups ~owner geometry)

let group_random ?cancel ?grain ?seed ?seed_attribute ?base ?merge
    ~probability ~owner ~name geometry =
  Error.guard ~operation:"group_random" ~code:"invalid_group" (fun () ->
    group_random ?cancel ?grain ?seed ?seed_attribute ?base ?merge
      ~probability ~owner ~name geometry)

let group_bounds ?cancel ?grain ?base ?containment ?merge bounds
    ~owner ~name geometry =
  Error.guard ~operation:"group_bounds" ~code:"invalid_group" (fun () ->
    group_bounds ?cancel ?grain ?base ?containment ?merge bounds
      ~owner ~name geometry)
let group_from_attribute_boundary ?cancel ?grain ?attributes ?tolerance
    ?include_unshared_edges ?include_all_unshared_curve_edges
    ?include_all_primitives_sharing_boundary_points ~owner ~name geometry =
  Error.guard ~operation:"group_from_attribute_boundary" ~code:"invalid_group"
    (fun () -> group_from_attribute_boundary ?cancel ?grain ?attributes
      ?tolerance ?include_unshared_edges ?include_all_unshared_curve_edges
      ?include_all_primitives_sharing_boundary_points ~owner ~name geometry)

let group_normal ?cancel ?grain ?normal_attribute
    ?use_existing_normal ?base ?include_opposite ?merge ~direction
    ~spread_angle ~owner ~name geometry =
  Error.guard ~operation:"group_normal" ~code:"invalid_group" (fun () ->
    group_normal ?cancel ?grain ?normal_attribute ?use_existing_normal ?base
      ?include_opposite ?merge ~direction ~spread_angle ~owner ~name geometry)

let group_non_planar ?cancel ?grain ?base ?merge ~tolerance ~name
    geometry =
  Error.guard ~operation:"group_non_planar" ~code:"invalid_group" (fun () ->
    group_non_planar ?cancel ?grain ?base ?merge ~tolerance ~name geometry)

let group_backface ?cancel ?grain ?base ?merge ~viewpoint ~name
    geometry =
  Error.guard ~operation:"group_backface" ~code:"invalid_group" (fun () ->
    group_backface ?cancel ?grain ?base ?merge ~viewpoint ~name geometry)

let group_edge_depth ?cancel ?grain ?merge ~depth ~point_group ~name
    geometry =
  Error.guard ~operation:"group_edge_depth" ~code:"invalid_group" (fun () ->
    group_edge_depth ?cancel ?grain ?merge ~depth ~point_group ~name geometry)

let group_unshared ?cancel ?grain ?merge ~owner ~name geometry =
  Error.guard ~operation:"group_unshared" ~code:"invalid_group" (fun () ->
    group_unshared ?cancel ?grain ?merge ~owner ~name geometry)

let group_boundary_components ?cancel ?grain ?prefix ?conflict
    ?max_groups ?max_payload_bytes geometry =
  Error.guard ~operation:"group_boundary_components" ~code:"invalid_group"
    (fun () -> group_boundary_components ?cancel ?grain ?prefix ?conflict
      ?max_groups ?max_payload_bytes geometry)

let group_promote_boundary ?cancel ?grain ?name ?keep_original
    ?output_attribute ?attributes ?tolerance ?include_unshared_edges
    ?include_all_unshared_curve_edges
    ?include_all_primitives_sharing_boundary_points
    ~source ~destination ~group geometry =
  Error.guard ~operation:"group_promote_boundary" ~code:"invalid_group"
    (fun () -> group_promote_boundary ?cancel ?grain ?name ?keep_original
      ?output_attribute ?attributes ?tolerance ?include_unshared_edges
      ?include_all_unshared_curve_edges
      ?include_all_primitives_sharing_boundary_points
      ~source ~destination ~group geometry)

let combine ?cancel ?grain ~owner ~name ~base ~steps geometry =
  Error.guard ~operation:"group_combine" ~code:"invalid_group" (fun () ->
    combine ?cancel ?grain ~owner ~name ~base ~steps geometry)

let range ?cancel ?grain ?base ?invert ?filter ?connectivity ?merge
    ~owner ~name specification geometry =
  Error.guard ~operation:"group_range" ~code:"invalid_group" (fun () ->
    range ?cancel ?grain ?base ?invert ?filter ?connectivity ?merge
      ~owner ~name specification geometry)

let ranges ?cancel ?grain ~rules geometry =
  Error.guard ~operation:"group_ranges" ~code:"invalid_group" (fun () ->
    ranges ?cancel ?grain rules geometry)

let invert ?conflict ?owner ~pattern ?new_name geometry =
  Error.guard ~operation:"group_invert" ~code:"invalid_group" (fun () ->
    invert ?conflict ?owner ~pattern ?new_name geometry)
