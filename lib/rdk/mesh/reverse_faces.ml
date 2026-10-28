
let remap_edge_groups_identity ?cancel ~source_topology ~target_topology
    ~point_count groups =
  match groups with
  | [] -> []
  | groups ->
      let source_index = Topology_index.create ?cancel source_topology
      and target_index = Topology_index.create ?cancel target_topology
      and point_map = Array.init point_count Fun.id in
      List.map (fun group -> Edge_group.remap ?cancel ~source_index
        ~target_topology ~target_index ~point_map group |> Support.get_ok) groups

type operation =
  | Reverse_vertices
  | Shift_vertices of int

let run ?cancel ?(grain = 16_384) ?primitives
    ?(operation = Reverse_vertices) geometry =
  Error.guard ~operation:"reverse" ~code:"invalid_topology" @@ fun () ->
  try
  if grain <= 0 then invalid_arg "Rdk_mesh.Reverse_faces.reverse: grain must be positive";
  Cancel.check_opt cancel;
  let topology = Geometry.topology geometry in
  let source = Topology.Private.view topology in
  let vertex_count = Topology.vertex_count topology in
  let primitive_count = Topology.primitive_count topology in
  (match primitives with
   | Some group when Group.owner group <> Group.Primitive ->
       invalid_arg "Rdk_mesh.Reverse_faces.reverse: selection must own primitives"
   | Some group when Group.length group <> primitive_count ->
       invalid_arg "Rdk_mesh.Reverse_faces.reverse: selection length does not match primitive count"
   | None | Some _ -> ());
  let selected primitive = match primitives with
    | None -> true | Some group -> Group.mem primitive group in
  let normalized_shift count offset =
    let shift = offset mod count in
    if shift < 0 then shift + count else shift in
  let changes = match operation with
    | Reverse_vertices ->
        (match primitives with
         | None -> primitive_count > 0
         | Some group -> Group.cardinality group > 0)
    | Shift_vertices 0 -> false
    | Shift_vertices offset ->
        let primitive = ref 0 and changed = ref false in
        while not !changed && !primitive < primitive_count do
          if selected !primitive then begin
            let count = source.primitive_offsets.(!primitive + 1)
                - source.primitive_offsets.(!primitive) in
            changed := normalized_shift count offset <> 0
          end;
          incr primitive
        done;
        !changed in
  if not changes then Ok geometry else
  let full_selection = match primitives with
    | None -> true
    | Some group -> Group.cardinality group = primitive_count in
  let vertex_points = if full_selection then Array.make vertex_count 0
    else Array.copy source.vertex_points
  and vertex_map = if full_selection then Array.make vertex_count 0
    else Array.init vertex_count Fun.id in
  let average_primitive_size = if primitive_count = 0 then 1
    else max 1 (vertex_count / primitive_count) in
  let primitive_grain = max 1 (grain / average_primitive_size) in
  Parallel.for_ ~chunk_size:primitive_grain ~start:0
    ~finish:(primitive_count - 1) (fun primitive ->
      if primitive land 1023 = 0 then Cancel.check_opt cancel;
      if selected primitive then begin
        let first = source.primitive_offsets.(primitive)
        and last = source.primitive_offsets.(primitive + 1) in
        let count = last - first in
        let shift = match operation with
          | Reverse_vertices -> 0
          | Shift_vertices offset -> normalized_shift count offset in
        for local = 0 to count - 1 do
          let old_local = match operation with
            | Reverse_vertices -> count - local - 1
            | Shift_vertices _ -> (local + shift) mod count in
          let vertex = first + local and old_vertex = first + old_local in
          vertex_points.(vertex) <- source.vertex_points.(old_vertex);
          vertex_map.(vertex) <- old_vertex
        done
      end);
  let reversed_topology = Topology.Private.create_validated_owned
      ~point_count:(Geometry.point_count geometry)
      ~vertex_points ~primitive_offsets:(Array.copy source.primitive_offsets)
      ~primitive_kinds:(Bytes.copy source.primitive_kinds) in
  let remap_vertex_attribute attribute =
    if Attribute.owner attribute <> Attribute.Vertex then Ok attribute
    else Ok (Topology_remap.attribute ?cancel ~grain vertex_map attribute) in
  Result.bind (Ok reversed_topology) (fun topology ->
    let rec attributes result = function
      | [] -> Ok (List.rev result)
      | attribute :: rest ->
          if operation = Reverse_vertices
             && String.equal (Attribute.name attribute) "N"
             && (Attribute.owner attribute = Attribute.Point
                 || Attribute.owner attribute = Attribute.Vertex)
          then attributes result rest
          else Result.bind (remap_vertex_attribute attribute)
              (fun attribute -> attributes (attribute :: result) rest) in
    Result.bind (attributes [] (Geometry.attributes geometry)) (fun attributes ->
      let groups = List.map (fun group ->
        if Group.owner group <> Group.Vertex then group
        else Topology_remap.group ?cancel ~grain vertex_map group)
        (Geometry.groups geometry)
      and edge_groups = remap_edge_groups_identity ?cancel
          ~source_topology:(Geometry.topology geometry) ~target_topology:topology
          ~point_count:(Geometry.point_count geometry)
          (Geometry.edge_groups geometry) in
      Geometry.create ~positions:(Geometry.positions geometry) ~topology
        ~attributes ~groups ~edge_groups ()))
  with Invalid_argument message -> Error message
