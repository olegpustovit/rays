type t = {
  positions : Packed.Float3.t;
  topology : Topology.t;
  attributes : Attribute.t array;
  groups : Group.t array;
  edge_groups : Edge_group.t array;
  data_id : int;
}

let owner_count topology = function
  | Attribute.Point -> Topology.point_count topology
  | Attribute.Vertex -> Topology.vertex_count topology
  | Attribute.Primitive -> Topology.primitive_count topology
  | Attribute.Detail -> 1

let group_count topology = function
  | Group.Point -> Topology.point_count topology
  | Group.Vertex -> Topology.vertex_count topology
  | Group.Primitive -> Topology.primitive_count topology

let duplicate key values =
  let seen = Hashtbl.create (Array.length values) in
  Array.find_opt
    (fun value ->
      let key = key value in
      if Hashtbl.mem seen key then true
      else (
        Hashtbl.add seen key ();
        false))
    values

let validate positions topology attributes groups edge_groups =
  if Packed.Float3.length positions <> Topology.point_count topology then
    Error "Geometry.create: position count must match topology point count"
  else if
    Array.exists
      (fun attribute ->
        Attribute.owner attribute = Attribute.Point && String.equal (Attribute.name attribute) "P")
      attributes
  then Error "Geometry.create: P is the canonical position buffer, not an ordinary attribute"
  else
    match
      Array.find_opt
        (fun attribute ->
          Attribute.length attribute <> owner_count topology (Attribute.owner attribute))
        attributes
    with
    | Some attribute ->
        Error
          (Printf.sprintf "Geometry.create: attribute %s has the wrong element count"
             (Attribute.name attribute))
    | None -> (
        match
          Array.find_opt
            (fun group -> Group.length group <> group_count topology (Group.owner group))
            groups
        with
        | Some group ->
            Error
              (Printf.sprintf "Geometry.create: group %s has the wrong element count"
                 (Group.name group))
        | None -> (
            match
              duplicate (fun value -> (Attribute.owner value, Attribute.name value)) attributes
            with
            | Some attribute ->
                Error ("Geometry.create: duplicate attribute " ^ Attribute.name attribute)
            | None -> (
                match duplicate (fun value -> (Group.owner value, Group.name value)) groups with
                | Some group -> Error ("Geometry.create: duplicate group " ^ Group.name group)
                | None -> (
                    match
                      Array.find_opt
                        (fun group ->
                          Edge_group.topology_data_id group <> Topology.data_id topology)
                        edge_groups
                    with
                    | Some group ->
                        Error
                          (Printf.sprintf
                             "Geometry.create: edge group %s belongs to a different topology"
                             (Edge_group.name group))
                    | None -> (
                        match duplicate Edge_group.name edge_groups with
                        | Some group ->
                            Error ("Geometry.create: duplicate edge group " ^ Edge_group.name group)
                        | None -> Ok ())))))

let make positions topology attributes groups edge_groups =
  { positions; topology; attributes; groups; edge_groups; data_id = Support.fresh_data_id () }

let create ~positions ~topology ?(attributes = []) ?(groups = []) ?(edge_groups = []) () =
  let attributes = Array.of_list attributes
  and groups = Array.of_list groups
  and edge_groups = Array.of_list edge_groups in
  Result.map
    (fun () -> make positions topology attributes groups edge_groups)
    (validate positions topology attributes groups edge_groups)

let data_id value = value.data_id
let fresh_data_id = Support.fresh_data_id
let positions value = value.positions
let topology value = value.topology
let point_count value = Topology.point_count value.topology
let vertex_count value = Topology.vertex_count value.topology
let primitive_count value = Topology.primitive_count value.topology

let payload_bytes value =
  Packed.Float3.payload_bytes value.positions
  + Topology.payload_bytes value.topology
  + Array.fold_left
      (fun total attribute -> total + Attribute.payload_bytes attribute)
      0 value.attributes
  + Array.fold_left (fun total group -> total + Group.payload_bytes group) 0 value.groups
  + Array.fold_left (fun total group -> total + Edge_group.payload_bytes group) 0 value.edge_groups

let payload_components value =
  (Packed.Float3.data_id value.positions, Packed.Float3.payload_bytes value.positions)
  :: (Topology.data_id value.topology, Topology.payload_bytes value.topology)
  :: (Array.to_list value.attributes
     |> List.map (fun attribute ->
         (Attribute.storage_id attribute, Attribute.payload_bytes attribute)))
  @ (Array.to_list value.groups
    |> List.map (fun group -> (Group.data_id group, Group.payload_bytes group)))
  @ (Array.to_list value.edge_groups
    |> List.map (fun group -> (Edge_group.data_id group, Edge_group.payload_bytes group)))

let attributes value = Array.to_list value.attributes
let groups value = Array.to_list value.groups
let edge_groups value = Array.to_list value.edge_groups

let find_attribute ~owner name value =
  Array.find_opt
    (fun attribute ->
      Attribute.owner attribute = owner && String.equal (Attribute.name attribute) name)
    value.attributes

let find_group ~owner name value =
  Array.find_opt
    (fun group -> Group.owner group = owner && String.equal (Group.name group) name)
    value.groups

let find_edge_group name value =
  Array.find_opt (fun group -> String.equal (Edge_group.name group) name) value.edge_groups

let with_positions positions value =
  Result.map
    (fun () -> make positions value.topology value.attributes value.groups value.edge_groups)
    (if Packed.Float3.length positions = point_count value then Ok ()
     else Error "Geometry.with_positions: point count changed")

let replace_or_append same item values =
  match Array.find_index (same item) values with
  | None -> Array.append values [| item |]
  | Some index ->
      let result = Array.copy values in
      result.(index) <- item;
      result

let with_attribute attribute value =
  if Attribute.owner attribute = Attribute.Point && String.equal (Attribute.name attribute) "P" then
    Error "Geometry.with_attribute: use with_positions to replace canonical P"
  else if Attribute.length attribute <> owner_count value.topology (Attribute.owner attribute) then
    Error "Geometry.with_attribute: element count does not match owner"
  else
    let same left right =
      Attribute.owner left = Attribute.owner right
      && String.equal (Attribute.name left) (Attribute.name right)
    in
    Ok
      (make value.positions value.topology
         (replace_or_append same attribute value.attributes)
         value.groups value.edge_groups)

let with_group group value =
  if Group.length group <> group_count value.topology (Group.owner group) then
    Error "Geometry.with_group: element count does not match owner"
  else
    let same left right =
      Group.owner left = Group.owner right && String.equal (Group.name left) (Group.name right)
    in
    Ok
      (make value.positions value.topology value.attributes
         (replace_or_append same group value.groups)
         value.edge_groups)

let with_edge_group group value =
  if Edge_group.topology_data_id group <> Topology.data_id value.topology then
    Error "Geometry.with_edge_group: group belongs to a different topology"
  else
    let same left right = String.equal (Edge_group.name left) (Edge_group.name right) in
    Ok
      (make value.positions value.topology value.attributes value.groups
         (replace_or_append same group value.edge_groups))

let without_attribute ~owner name value =
  let attributes =
    Array.of_list
      (Array.fold_right
         (fun attribute result ->
           if Attribute.owner attribute = owner && String.equal (Attribute.name attribute) name then
             result
           else attribute :: result)
         value.attributes [])
  in
  if Array.length attributes = Array.length value.attributes then value
  else make value.positions value.topology attributes value.groups value.edge_groups

let without_group ~owner name value =
  let groups =
    Array.of_list
      (Array.fold_right
         (fun group result ->
           if Group.owner group = owner && String.equal (Group.name group) name then result
           else group :: result)
         value.groups [])
  in
  if Array.length groups = Array.length value.groups then value
  else make value.positions value.topology value.attributes groups value.edge_groups

let without_edge_group name value =
  let groups =
    Array.of_list
      (Array.fold_right
         (fun group result ->
           if String.equal (Edge_group.name group) name then result else group :: result)
         value.edge_groups [])
  in
  if Array.length groups = Array.length value.edge_groups then value
  else make value.positions value.topology value.attributes value.groups groups

let rename_edge_group ~from ~into value =
  if String.equal from into then
    if find_edge_group from value = None then
      Error ("Geometry.rename_edge_group: missing group " ^ from)
    else Ok value
  else if find_edge_group into value <> None then
    Error ("Geometry.rename_edge_group: destination already exists: " ^ into)
  else
    match find_edge_group from value with
    | None -> Error ("Geometry.rename_edge_group: missing group " ^ from)
    | Some group ->
        with_edge_group (Edge_group.with_name into group) (without_edge_group from value)

module View = struct
  let attributes value = value.attributes

  let merge_owned key existing replacements =
    if Array.length replacements = 0 then existing
    else
      let slots = Hashtbl.create (Array.length existing + Array.length replacements) in
      Array.iteri (fun index item -> Hashtbl.add slots (key item) index) existing;
      let capacity = Array.length existing + Array.length replacements in
      let output = Array.make capacity replacements.(0) in
      Array.blit existing 0 output 0 (Array.length existing);
      let next = ref (Array.length existing) in
      Array.iter
        (fun item ->
          match Hashtbl.find_opt slots (key item) with
          | Some index -> output.(index) <- item
          | None ->
              Hashtbl.add slots (key item) !next;
              output.(!next) <- item;
              incr next)
        replacements;
      if !next = capacity then output else Array.sub output 0 !next

  let with_merged_attributes_owned replacements value =
    if Array.length replacements = 0 then Ok value
    else
      let key attribute = (Attribute.owner attribute, Attribute.name attribute) in
      let output = merge_owned key value.attributes replacements in
      Result.map
        (fun () -> make value.positions value.topology output value.groups value.edge_groups)
        (validate value.positions value.topology output value.groups value.edge_groups)

  let with_merged_owned ?positions ~attributes ~groups value =
    if positions = None && Array.length attributes = 0 && Array.length groups = 0 then Ok value
    else
      let positions = Option.value ~default:value.positions positions in
      let attributes =
        merge_owned
          (fun attribute -> (Attribute.owner attribute, Attribute.name attribute))
          value.attributes attributes
      and groups =
        merge_owned (fun group -> (Group.owner group, Group.name group)) value.groups groups
      in
      Result.map
        (fun () -> make positions value.topology attributes groups value.edge_groups)
        (validate positions value.topology attributes groups value.edge_groups)

  let with_attributes_owned attributes value =
    Result.map
      (fun () -> make value.positions value.topology attributes value.groups value.edge_groups)
      (validate value.positions value.topology attributes value.groups value.edge_groups)

  let with_positions_and_attributes_owned positions attributes value =
    Result.map
      (fun () -> make positions value.topology attributes value.groups value.edge_groups)
      (validate positions value.topology attributes value.groups value.edge_groups)
end

let generate_point_ranges count operation =
  if count < 0 then invalid_arg "Geometry.generate_point_ranges: negative point count";
  let x = Array.make count 0. and y = Array.make count 0. and z = Array.make count 0. in
  Support.run_ranges count (fun ~first ~last -> operation ~first ~last ~x ~y ~z);
  let positions = Packed.Float3.View.of_owned_exn ~x ~y ~z in
  let topology = Topology.empty ~point_count:(Packed.Float3.length positions) in
  create ~positions ~topology () |> Result.get_ok
