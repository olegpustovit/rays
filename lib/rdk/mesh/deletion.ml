
type topology_policy = Destroy_touched_primitives | Heal_primitives

exception Delete_error of string

let fail message = raise (Delete_error ("Rdk.Deletion.delete: " ^ message))
let get_ok = function Ok value -> value | Error message -> fail message

let minimum_vertices = function
  | Topology.Polygon | Topology.Closed_polyline -> 3
  | Topology.Open_polyline -> 2

let selected selection delete_selected index =
  Group.mem index selection = delete_selected

type plan = {
  point_map : int array;
  vertex_map : int array;
  primitive_map : int array;
  topology : Topology.t;
  healed : bool;
  point_identity : bool;
  unchanged : bool;
}

let validate_selection selection geometry =
  let expected = match Group.owner selection with
    | Group.Point -> Geometry.point_count geometry
    | Group.Vertex -> Geometry.vertex_count geometry
    | Group.Primitive -> Geometry.primitive_count geometry in
  if Group.length selection <> expected then
    fail (Printf.sprintf "selection length %d does not match its %s owner count %d"
      (Group.length selection)
      (match Group.owner selection with Point -> "point" | Vertex -> "vertex"
       | Primitive -> "primitive") expected)

let corner_deleted owner selection delete_selected topology vertex =
  match owner with
  | Group.Point -> selected selection delete_selected topology.Topology.View.vertex_points.(vertex)
  | Group.Vertex -> selected selection delete_selected vertex
  | Group.Primitive -> false

let primitive_retained_count owner selection delete_selected policy topology primitive =
  let first = topology.Topology.View.primitive_offsets.(primitive)
  and last = topology.primitive_offsets.(primitive + 1) in
  if owner = Group.Primitive then
    if selected selection delete_selected primitive then 0 else last - first
  else match policy with
    | Destroy_touched_primitives ->
        let touched = ref false in
        for vertex = first to last - 1 do
          if corner_deleted owner selection delete_selected topology vertex then touched := true
        done;
        if !touched then 0 else last - first
    | Heal_primitives ->
        let count = ref 0 in
        for vertex = first to last - 1 do
          if not (corner_deleted owner selection delete_selected topology vertex) then incr count
        done;
        if !count < minimum_vertices
            (match Bytes.get topology.primitive_kinds primitive with
             | '\000' -> Topology.Polygon
             | '\001' -> Topology.Open_polyline
             | _ -> Topology.Closed_polyline)
        then 0 else !count

let build_plan ?cancel ~selected:delete_selected ~compact_points ~policy
    selection source =
  let topology = Topology.View.view (Geometry.topology source) in
  let owner = Group.owner selection in
  let primitive_count = Bytes.length topology.primitive_kinds in
  let retained_counts = Array.make primitive_count 0
  and retained_primitives = ref 0 and retained_vertices = ref 0
  and healed = ref false in
  for primitive = 0 to primitive_count - 1 do
    if primitive land 1023 = 0 then Cancel.check_opt cancel;
    let count = primitive_retained_count owner selection delete_selected policy
        topology primitive in
    let source_count = topology.primitive_offsets.(primitive + 1)
      - topology.primitive_offsets.(primitive) in
    retained_counts.(primitive) <- count;
    if count > 0 then begin
      incr retained_primitives;
      if !retained_vertices > max_int - count then fail "vertex count exceeds integer limits";
      retained_vertices := !retained_vertices + count;
      if count <> source_count then healed := true
    end
  done;
  let vertex_map = Array.make !retained_vertices 0
  and primitive_map = Array.make !retained_primitives 0
  and primitive_offsets = Array.make (!retained_primitives + 1) 0
  and primitive_kinds = Bytes.make !retained_primitives '\000' in
  let vertex_at = ref 0 and primitive_at = ref 0 in
  for primitive = 0 to primitive_count - 1 do
    if primitive land 1023 = 0 then Cancel.check_opt cancel;
    let count = retained_counts.(primitive) in
    if count > 0 then begin
      let first = topology.primitive_offsets.(primitive)
      and last = topology.primitive_offsets.(primitive + 1) in
      primitive_map.(!primitive_at) <- primitive;
      Bytes.set primitive_kinds !primitive_at
        (Bytes.get topology.primitive_kinds primitive);
      for vertex = first to last - 1 do
        if owner = Group.Primitive
           || policy = Destroy_touched_primitives
           || not (corner_deleted owner selection delete_selected topology vertex)
        then begin
          vertex_map.(!vertex_at) <- vertex; incr vertex_at
        end
      done;
      incr primitive_at;
      primitive_offsets.(!primitive_at) <- !vertex_at
    end
  done;
  if !vertex_at <> !retained_vertices then fail "internal vertex cardinality mismatch";
  let source_points = topology.point_count in
  let used = if compact_points then Some (Array.make source_points false) else None in
  Option.iter (fun used ->
    Array.iteri (fun index vertex ->
      if index land 16_383 = 0 then Cancel.check_opt cancel;
      used.(topology.vertex_points.(vertex)) <- true) vertex_map) used;
  let retain_point point =
    let survives_selection = owner <> Group.Point
      || not (selected selection delete_selected point) in
    survives_selection && match used with None -> true | Some used -> used.(point) in
  let retained_points = ref 0 and old_to_new_point = Array.make source_points (-1) in
  for point = 0 to source_points - 1 do
    if point land 16_383 = 0 then Cancel.check_opt cancel;
    if retain_point point then begin
      old_to_new_point.(point) <- !retained_points; incr retained_points
    end
  done;
  let point_map = Array.make !retained_points 0 and point_at = ref 0 in
  for point = 0 to source_points - 1 do
    if old_to_new_point.(point) >= 0 then begin
      point_map.(!point_at) <- point; incr point_at
    end
  done;
  let vertex_points = Array.make !retained_vertices 0 in
  for vertex = 0 to !retained_vertices - 1 do
    if vertex land 16_383 = 0 then Cancel.check_opt cancel;
    let old_point = topology.vertex_points.(vertex_map.(vertex)) in
    let point = old_to_new_point.(old_point) in
    if point < 0 then fail "retained topology references a deleted point";
    vertex_points.(vertex) <- point
  done;
  let output_topology = Topology.View.create_validated_owned
      ~point_count:!retained_points ~vertex_points ~primitive_offsets
      ~primitive_kinds in
  let point_identity = Array.length point_map = source_points in
  let unchanged = point_identity
    && Array.length vertex_map = Array.length topology.vertex_points
    && Array.length primitive_map = primitive_count in
  { point_map; vertex_map; primitive_map; topology = output_topology;
    healed = !healed; point_identity; unchanged }

let remap_attribute ?cancel ?(grain = 16_384) plan attribute =
  let owner = Attribute.owner attribute in
  if plan.healed && String.equal (Attribute.name attribute) "N"
     && (owner = Attribute.Point || owner = Attribute.Vertex) then None
  else if owner = Attribute.Point && plan.point_identity then Some attribute
  else
    let mapping = match owner with
      | Attribute.Point -> Some plan.point_map
      | Attribute.Vertex -> Some plan.vertex_map
      | Attribute.Primitive -> Some plan.primitive_map
      | Attribute.Detail -> None in
    match mapping with
    | None -> Some attribute
    | Some mapping ->
        Some (Topology_remap.attribute ?cancel ~grain mapping attribute)

let remap_group ?cancel ?(grain = 16_384) plan group =
  if Group.owner group = Group.Point && plan.point_identity then group
  else
    let mapping = match Group.owner group with
      | Group.Point -> plan.point_map
      | Group.Vertex -> plan.vertex_map
      | Group.Primitive -> plan.primitive_map in
    Topology_remap.group ?cancel ~grain mapping group

let materialize_plan ?cancel ?grain ?(remap_edges = true) plan geometry =
  if plan.unchanged then geometry
  else begin
    let positions = if plan.point_identity then Geometry.positions geometry
      else begin
        let source_positions = Packed.Float3.View.view
            (Geometry.positions geometry)
        and grain = Option.value ~default:16_384 grain in
        Packed.Float3.View.of_owned_exn
          ~x:(Topology_remap.select ?cancel ~grain plan.point_map source_positions.x)
          ~y:(Topology_remap.select ?cancel ~grain plan.point_map source_positions.y)
          ~z:(Topology_remap.select ?cancel ~grain plan.point_map source_positions.z)
      end in
    let attributes = Geometry.attributes geometry
      |> List.filter_map (remap_attribute ?cancel ?grain plan) in
    let groups = Geometry.groups geometry
      |> List.map (remap_group ?cancel ?grain plan) in
    let edge_groups = if not remap_edges then []
      else match Geometry.edge_groups geometry with
      | [] -> []
      | source_groups ->
          let source_topology = Geometry.topology geometry in
          let point_map = Array.make (Geometry.point_count geometry) (-1) in
          Array.iteri (fun output source -> point_map.(source) <- output)
            plan.point_map;
          Topology_remap.edge_groups ?cancel ~source_topology
            ~target_topology:plan.topology ~point_map source_groups |> get_ok in
    Geometry.create ~positions ~topology:plan.topology ~attributes ~groups
      ~edge_groups () |> get_ok
  end

let lower_bound values needle =
  let first = ref 0 and last = ref (Array.length values) in
  while !first < !last do
    let middle = !first + ((!last - !first) lsr 1) in
    if values.(middle) < needle then first := middle + 1 else last := middle
  done;
  !first

let primitive_partitions ?cancel ?grain ~piece_count ?point_pieces
    ~primitive_pieces geometry =
  if piece_count < 0 then invalid_arg
      "Rdk.Deletion.primitive_partitions: negative piece count";
  let topology = Topology.View.view (Geometry.topology geometry) in
  let source_points = topology.point_count
  and source_primitives = Bytes.length topology.primitive_kinds in
  if Array.length primitive_pieces <> source_primitives then invalid_arg
      "Rdk.Deletion.primitive_partitions: primitive assignment length mismatch";
  Option.iter (fun pieces -> if Array.length pieces <> source_points then
    invalid_arg
      "Rdk.Deletion.primitive_partitions: point assignment length mismatch")
    point_pieces;
  let valid_piece piece = piece >= 0 && piece < piece_count in
  let check_piece owner element piece =
    if piece < -1 || piece >= piece_count then fail (Printf.sprintf
      "%s %d has invalid piece index %d for %d pieces"
      owner element piece piece_count) in
  let primitive_counts = Array.make piece_count 0
  and vertex_counts = Array.make piece_count 0 in
  Array.iteri (fun primitive piece ->
    if primitive land 4095 = 0 then Cancel.check_opt cancel;
    check_piece "primitive" primitive piece;
    if valid_piece piece then begin
      primitive_counts.(piece) <- primitive_counts.(piece) + 1;
      vertex_counts.(piece) <- vertex_counts.(piece)
        + topology.primitive_offsets.(primitive + 1)
        - topology.primitive_offsets.(primitive)
    end) primitive_pieces;
  let primitive_maps = Array.init piece_count
      (fun piece -> Array.make primitive_counts.(piece) 0)
  and vertex_maps = Array.init piece_count
      (fun piece -> Array.make vertex_counts.(piece) 0)
  and primitive_at = Array.make piece_count 0
  and vertex_at = Array.make piece_count 0 in
  Array.iteri (fun primitive piece ->
    if valid_piece piece then begin
      primitive_maps.(piece).(primitive_at.(piece)) <- primitive;
      primitive_at.(piece) <- primitive_at.(piece) + 1;
      let first = topology.primitive_offsets.(primitive)
      and last = topology.primitive_offsets.(primitive + 1) in
      for vertex = first to last - 1 do
        vertex_maps.(piece).(vertex_at.(piece)) <- vertex;
        vertex_at.(piece) <- vertex_at.(piece) + 1
      done;
    end) primitive_pieces;
  let point_maps = match point_pieces with
    | Some pieces ->
        let counts = Array.make piece_count 0 in
        Array.iteri (fun point piece ->
          if point land 16383 = 0 then Cancel.check_opt cancel;
          check_piece "point" point piece;
          if valid_piece piece then counts.(piece) <- counts.(piece) + 1)
          pieces;
        let maps = Array.init piece_count
            (fun piece -> Array.make counts.(piece) 0)
        and at = Array.make piece_count 0 in
        Array.iteri (fun point piece -> if valid_piece piece then begin
          maps.(piece).(at.(piece)) <- point;
          at.(piece) <- at.(piece) + 1
        end) pieces;
        maps
    | None ->
        Array.init piece_count (fun piece ->
          let vertices = vertex_maps.(piece) in
          let candidates = Array.map
              (fun vertex -> topology.vertex_points.(vertex)) vertices in
          Array.fast_sort Int.compare candidates;
          if Array.length candidates = 0 then [||]
          else begin
            let unique = ref 1 in
            for index = 1 to Array.length candidates - 1 do
              if candidates.(index) <> candidates.(!unique - 1) then begin
                candidates.(!unique) <- candidates.(index); incr unique
              end
            done;
            Array.sub candidates 0 !unique
          end) in
  let geometries = Array.init piece_count (fun piece ->
    Cancel.check_opt cancel;
    let point_map = point_maps.(piece)
    and vertex_map = vertex_maps.(piece)
    and primitive_map = primitive_maps.(piece) in
    let primitive_count = Array.length primitive_map in
    let primitive_offsets = Array.make (primitive_count + 1) 0
    and primitive_kinds = Bytes.make primitive_count '\000' in
    Array.iteri (fun output source ->
      primitive_offsets.(output + 1) <- primitive_offsets.(output)
        + topology.primitive_offsets.(source + 1)
        - topology.primitive_offsets.(source);
      Bytes.set primitive_kinds output
        (Bytes.get topology.primitive_kinds source)) primitive_map;
    let vertex_points = Array.make (Array.length vertex_map) 0 in
    Array.iteri (fun output source_vertex ->
      let source_point = topology.vertex_points.(source_vertex) in
      let point = lower_bound point_map source_point in
      if point >= Array.length point_map || point_map.(point) <> source_point then
        fail "piece primitive references a point outside its point partition";
      vertex_points.(output) <- point) vertex_map;
    let output_topology = Topology.View.create_validated_owned
        ~point_count:(Array.length point_map) ~vertex_points ~primitive_offsets
        ~primitive_kinds in
    let point_identity = Array.length point_map = source_points in
    let unchanged = point_identity
      && Array.length vertex_map = Array.length topology.vertex_points
      && Array.length primitive_map = source_primitives in
    materialize_plan ?cancel ?grain ~remap_edges:false {
      point_map; vertex_map; primitive_map; topology = output_topology;
      healed = false; point_identity; unchanged;
    } geometry) in
  match Geometry.edge_groups geometry with
  | [] -> geometries
  | source_groups ->
      let point_piece_counts = Array.make source_points 0 in
      Array.iter (fun points -> Array.iter (fun point ->
        point_piece_counts.(point) <- point_piece_counts.(point) + 1) points)
        point_maps;
      let point_piece_offsets = Array.make (source_points + 1) 0 in
      for point = 0 to source_points - 1 do
        point_piece_offsets.(point + 1) <- point_piece_offsets.(point)
          + point_piece_counts.(point)
      done;
      let association_count = point_piece_offsets.(source_points) in
      let point_piece_ids = Array.make association_count 0
      and point_local_ids = Array.make association_count 0
      and next = Array.copy point_piece_offsets in
      Array.iteri (fun piece points -> Array.iteri (fun local point ->
        let output = next.(point) in
        point_piece_ids.(output) <- piece;
        point_local_ids.(output) <- local;
        next.(point) <- output + 1) points) point_maps;
      let source_index = Topology_index.create ?cancel
          (Geometry.topology geometry)
      and target_indexes = Array.map (fun geometry ->
          Topology_index.create ?cancel (Geometry.topology geometry)) geometries in
      let outputs = Array.copy geometries in
      List.iter (fun source_group ->
        let builders = Array.mapi (fun piece geometry ->
          Edge_group.Builder.create ~topology:(Geometry.topology geometry)
            ~index:target_indexes.(piece) ~name:(Edge_group.name source_group))
            geometries in
        Edge_group.iter (fun edge ->
          let a, b = Topology_index.edge_points source_index edge in
          let a_at = ref point_piece_offsets.(a)
          and b_at = ref point_piece_offsets.(b)
          and a_last = point_piece_offsets.(a + 1)
          and b_last = point_piece_offsets.(b + 1) in
          while !a_at < a_last && !b_at < b_last do
            let a_piece = point_piece_ids.(!a_at)
            and b_piece = point_piece_ids.(!b_at) in
            if a_piece < b_piece then incr a_at
            else if b_piece < a_piece then incr b_at
            else begin
              (match Topology_index.find_edge target_indexes.(a_piece)
                  ~a:point_local_ids.(!a_at) ~b:point_local_ids.(!b_at) with
               | None -> ()
               | Some target -> Edge_group.Builder.set builders.(a_piece)
                   target true);
              incr a_at; incr b_at
            end
          done) source_group;
        Array.iteri (fun piece builder ->
          outputs.(piece) <- Geometry.with_edge_group
              (Edge_group.Builder.freeze builder) outputs.(piece) |> get_ok)
          builders) source_groups;
      outputs

let delete ?cancel ?grain ?(selected = true) ?(compact_points = false)
    ?(policy = Destroy_touched_primitives) selection geometry =
  Error.guard ~operation:"delete" ~code:"invalid_selection" @@ fun () ->
  try
    Cancel.check_opt cancel;
    validate_selection selection geometry;
    let deletes_none = if selected then Group.cardinality selection = 0
      else Group.cardinality selection = Group.length selection in
    if deletes_none && not compact_points then Ok geometry
    else let plan = build_plan ?cancel ~selected ~compact_points ~policy selection geometry in
    if plan.unchanged then Ok geometry
    else Ok (materialize_plan ?cancel ?grain plan geometry)
  with Delete_error message -> Error message

let delete_primitives ?cancel ?grain ?(compact_points = false)
    group geometry =
  if Group.owner group <> Group.Primitive then
    Error (Error.make ~operation:"delete_primitives" ~code:"invalid_selection"
      "selection must own primitives")
  else Result.map_error
      (fun error -> Error.make ~operation:"delete_primitives"
        ~code:(Error.code error) (Error.message error))
      (delete ?cancel ?grain ~compact_points group geometry)
