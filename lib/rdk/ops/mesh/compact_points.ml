let compact_points ?cancel ?(grain = 16_384) geometry =
  if grain <= 0 then invalid_arg "Rdk.Compact_points.compact_points: grain must be positive";
  Cancel.check_opt cancel;
  let point_count = Geometry.point_count geometry
  and source_topology_value = Geometry.topology geometry in
  let source_topology = Topology.View.view source_topology_value in
  let used = Array.make point_count false in
  Array.iteri
    (fun vertex point ->
      Cancel.poll cancel vertex 16_383;
      used.(point) <- true)
    source_topology.vertex_points;
  let old_to_new = Array.make point_count (-1) and retained = ref 0 in
  for point = 0 to point_count - 1 do
    Cancel.poll cancel point 16_383;
    if used.(point) then begin
      old_to_new.(point) <- !retained;
      incr retained
    end
  done;
  if !retained = point_count then Ok geometry
  else begin
    let new_to_old = Array.make !retained 0 and at = ref 0 in
    for point = 0 to point_count - 1 do
      if used.(point) then begin
        new_to_old.(!at) <- point;
        incr at
      end
    done;
    let source_positions = Packed.Float3.View.view (Geometry.positions geometry) in
    let px = Topology_remap.select_float ?cancel ~grain new_to_old source_positions.x
    and py = Topology_remap.select_float ?cancel ~grain new_to_old source_positions.y
    and pz = Topology_remap.select_float ?cancel ~grain new_to_old source_positions.z in
    let positions = Packed.Float3.View.of_owned_exn ~x:px ~y:py ~z:pz in
    let vertex_points = Array.make (Array.length source_topology.vertex_points) 0 in
    if Array.length vertex_points > 0 then
      Parallel.for_ ~chunk_size:grain ~start:0
        ~finish:(Array.length vertex_points - 1)
        (fun vertex ->
          Cancel.poll cancel vertex 16_383;
          vertex_points.(vertex) <- old_to_new.(source_topology.vertex_points.(vertex)));
    let topology =
      Topology.View.create_validated_owned ~point_count:!retained ~vertex_points
        ~primitive_offsets:(Array.copy source_topology.primitive_offsets)
        ~primitive_kinds:(Bytes.copy source_topology.primitive_kinds)
    in
    let remap_point_attribute attribute =
      if Attribute.owner attribute <> Attribute.Point then Ok attribute
      else Ok (Topology_remap.attribute ?cancel ~grain new_to_old attribute)
    in
    let rec remap_attributes result = function
      | [] -> Ok (List.rev result)
      | attribute :: rest ->
          Result.bind (remap_point_attribute attribute) (fun attribute ->
              remap_attributes (attribute :: result) rest)
    in
    Result.bind
      (remap_attributes [] (Geometry.attributes geometry))
      (fun attributes ->
        let groups =
          List.map
            (fun group ->
              if Group.owner group <> Group.Point then group
              else Topology_remap.group ?cancel ~grain new_to_old group)
            (Geometry.groups geometry)
        in
        let edge_groups =
          Topology_remap.edge_groups ?cancel ~source_topology:source_topology_value
            ~target_topology:topology ~point_map:old_to_new (Geometry.edge_groups geometry)
          |> Support.get_ok
        in
        Geometry.create ~positions ~topology ~attributes ~groups ~edge_groups ())
  end

let run ?cancel ?grain geometry =
  Error.guard ~operation:"compact_points" ~code:"invalid_geometry" (fun () ->
      compact_points ?cancel ?grain geometry)
