
type scheme = Catmull_clark | Bilinear

let fail = Error.fail

let checked_add name left right =
  if right < 0 || left > max_int - right then
    fail (name ^ " exceeds integer limits");
  left + right

let run ?(grain = 16_384) ?cancel count operation =
  if grain <= 0 then invalid_arg "Rdk_curve.Curve_subdivide.subdivide: grain must be positive";
  if count > 0 then
    Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(count - 1) (fun index ->
      Cancel.poll cancel index 4095;
      operation index)

type plan = {
  source : Geometry.t;

  output_topology : Topology.t;
  output_index : Topology_index.t option;
  point_offsets : int array;
  point_sources : int array;
  point_weights : float array;
  point_representative : int array;
  point_old : bytes;
  vertex_left : int array;
  vertex_right : int array;
  vertex_weight : float array;
  vertex_old : bytes;
  primitive_source : int array;
  source_edge_of_output_vertex : int array;
}

let primitive_size topology primitive =
  topology.Topology.View.primitive_offsets.(primitive + 1)
  - topology.primitive_offsets.(primitive)

let validate ?cancel geometry =
  let topology = Topology.View.view (Geometry.topology geometry) in
  if topology.point_count = 0 || Bytes.length topology.primitive_kinds = 0 then
    fail "polygon-curve input must contain at least one primitive";
  for primitive = 0 to Bytes.length topology.primitive_kinds - 1 do
    Cancel.poll cancel primitive 1023;
    let kind = Char.code (Bytes.get topology.primitive_kinds primitive) in
    if kind = 0 then fail "curve refinement received a polygon primitive"
    else if kind <> 1 && kind <> 2 then
      fail "curve refinement received an unsupported primitive kind";
    let first = topology.primitive_offsets.(primitive)
    and last = topology.primitive_offsets.(primitive + 1) in
    let cyclic = kind = 2 in
    for vertex = first to last - 1 do
      let next = if vertex + 1 < last then vertex + 1
        else if cyclic then first else -1 in
      if next >= 0
         && topology.vertex_points.(vertex) = topology.vertex_points.(next) then
        fail "a polygon curve contains a zero-length topology edge"
    done
  done;
  let positions = Packed.Float3.View.view (Geometry.positions geometry) in
  if Array.exists (fun value -> not (Float.is_finite value)) positions.x
     || Array.exists (fun value -> not (Float.is_finite value)) positions.y
     || Array.exists (fun value -> not (Float.is_finite value)) positions.z then
    fail "positions must be finite"

let other_endpoint index point edge =
  if index.Topology_index.View.edge_a.(edge) = point then index.edge_b.(edge)
  else if index.edge_b.(edge) = point then index.edge_a.(edge)
  else fail "curve point-edge incidence is inconsistent"

let shared_plan ?cancel ?grain scheme geometry topology index =
  let source_points = topology.Topology.View.point_count
  and source_primitives = Bytes.length topology.primitive_kinds
  and edge_count = Array.length index.Topology_index.View.edge_a in
  let output_points = checked_add "curve subdivision point count"
      source_points edge_count in
  let point_offsets = Array.make (output_points + 1) 0 in
  for point = 0 to source_points - 1 do
    let degree = index.point_edge_offsets.(point + 1)
        - index.point_edge_offsets.(point) in
    point_offsets.(point + 1) <- point_offsets.(point)
      + if scheme = Catmull_clark && degree = 2 then 3 else 1
  done;
  for edge = 0 to edge_count - 1 do
    point_offsets.(source_points + edge + 1) <-
      checked_add "curve subdivision point stencil"
        point_offsets.(source_points + edge) 2
  done;
  let point_sources = Array.make point_offsets.(output_points) 0
  and point_weights = Array.make point_offsets.(output_points) 0.
  and point_representative = Array.make output_points 0
  and point_old = Bytes.make output_points '\000' in
  run ?grain ?cancel source_points (fun point ->
    let first = point_offsets.(point) in
    Bytes.set point_old point '\001';
    point_representative.(point) <- point;
    let edge_first = index.point_edge_offsets.(point)
    and edge_last = index.point_edge_offsets.(point + 1) in
    if scheme = Catmull_clark && edge_last - edge_first = 2 then begin
      point_sources.(first) <- other_endpoint index point
          index.point_edges.(edge_first);
      point_weights.(first) <- 0.125;
      point_sources.(first + 1) <- point;
      point_weights.(first + 1) <- 0.75;
      point_sources.(first + 2) <- other_endpoint index point
          index.point_edges.(edge_first + 1);
      point_weights.(first + 2) <- 0.125
    end else begin
      point_sources.(first) <- point;
      point_weights.(first) <- 1.
    end);
  run ?grain ?cancel edge_count (fun edge ->
    let output = source_points + edge in
    let first = point_offsets.(output) in
    let a = index.edge_a.(edge) and b = index.edge_b.(edge) in
    point_sources.(first) <- a; point_weights.(first) <- 0.5;
    point_sources.(first + 1) <- b; point_weights.(first + 1) <- 0.5;
    point_representative.(output) <- min a b);
  let primitive_offsets = Array.make (source_primitives + 1) 0 in
  for primitive = 0 to source_primitives - 1 do
    let size = primitive_size topology primitive in
    let closed = Bytes.get topology.primitive_kinds primitive = '\002' in
    primitive_offsets.(primitive + 1) <- checked_add
        "curve subdivision vertex count" primitive_offsets.(primitive)
        ((2 * size) - if closed then 0 else 1)
  done;
  let output_vertices = primitive_offsets.(source_primitives) in
  let vertex_points = Array.make output_vertices 0
  and primitive_kinds = Bytes.copy topology.primitive_kinds
  and vertex_left = Array.make output_vertices 0
  and vertex_right = Array.make output_vertices 0
  and vertex_weight = Array.make output_vertices 0.
  and vertex_old = Bytes.make output_vertices '\000'
  and source_edge_of_output_vertex = Array.make output_vertices (-1) in
  run ?grain ?cancel source_primitives (fun primitive ->
    let first = topology.primitive_offsets.(primitive)
    and last = topology.primitive_offsets.(primitive + 1) in
    let output = ref primitive_offsets.(primitive) in
    for vertex = first to last - 1 do
      let old = !output in
      incr output;
      vertex_points.(old) <- topology.vertex_points.(vertex);
      vertex_left.(old) <- vertex; vertex_right.(old) <- vertex;
      Bytes.set vertex_old old '\001';
      let edge = index.edge_of_vertex.(vertex) in
      if edge >= 0 then begin
        source_edge_of_output_vertex.(old) <- edge;
        let midpoint = !output in
        incr output;
        let next = index.next_vertex.(vertex) in
        if next < 0 then fail "curve edge has no following corner";
        vertex_points.(midpoint) <- source_points + edge;
        vertex_left.(midpoint) <- vertex;
        vertex_right.(midpoint) <- next;
        vertex_weight.(midpoint) <- 0.5;
        source_edge_of_output_vertex.(midpoint) <- edge
      end
    done;
    if !output <> primitive_offsets.(primitive + 1) then
      fail "curve subdivision vertex cardinality mismatch");
  let output_topology = Topology.View.create_validated_owned
      ~point_count:output_points ~vertex_points ~primitive_offsets
      ~primitive_kinds in
  let output_index = match Geometry.edge_groups geometry with
    | [] -> None
    | _ -> Some (Topology_index.create ?cancel output_topology) in
  { source = geometry;
    output_topology; output_index; point_offsets; point_sources; point_weights;
    point_representative; point_old; vertex_left; vertex_right; vertex_weight;
    vertex_old; primitive_source = Array.init source_primitives Fun.id;
    source_edge_of_output_vertex }

let independent_plan ?cancel ?grain scheme geometry
    (topology : Topology.View.view)
    (index : Topology_index.View.view) =
  let source_primitives = Bytes.length topology.Topology.View.primitive_kinds in
  let output_points = ref 0 in
  let primitive_offsets = Array.make (source_primitives + 1) 0 in
  for primitive = 0 to source_primitives - 1 do
    let size = primitive_size topology primitive in
    let closed = Bytes.get topology.primitive_kinds primitive = '\002' in
    output_points := checked_add "independent curve subdivision point count"
        !output_points ((2 * size) - if closed then 0 else 1);
    primitive_offsets.(primitive + 1) <- !output_points
  done;
  let output_points = !output_points in
  let point_offsets = Array.make (output_points + 1) 0
  and point_representative = Array.make output_points 0
  and point_old = Bytes.make output_points '\000'
  and vertex_points = Array.init output_points Fun.id
  and vertex_left = Array.make output_points 0
  and vertex_right = Array.make output_points 0
  and vertex_weight = Array.make output_points 0.
  and vertex_old = Bytes.make output_points '\000'
  and source_edge_of_output_vertex = Array.make output_points (-1) in
  for primitive = 0 to source_primitives - 1 do
    let first = topology.primitive_offsets.(primitive)
    and last = topology.primitive_offsets.(primitive + 1)
    and closed = Bytes.get topology.primitive_kinds primitive = '\002' in
    let output = ref primitive_offsets.(primitive) in
    for vertex = first to last - 1 do
      let old = !output in
      incr output;
      let smooth = scheme = Catmull_clark
          && (closed || (vertex > first && vertex + 1 < last)) in
      point_offsets.(old + 1) <- point_offsets.(old) + if smooth then 3 else 1;
      point_representative.(old) <- topology.vertex_points.(vertex);
      Bytes.set point_old old '\001';
      vertex_left.(old) <- vertex; vertex_right.(old) <- vertex;
      Bytes.set vertex_old old '\001';
      let edge = index.edge_of_vertex.(vertex) in
      if edge >= 0 then begin
        source_edge_of_output_vertex.(old) <- edge;
        let midpoint = !output in
        incr output;
        point_offsets.(midpoint + 1) <- point_offsets.(midpoint) + 2;
        let next = index.next_vertex.(vertex) in
        if next < 0 then fail "curve edge has no following corner";
        let a = topology.vertex_points.(vertex)
        and b = topology.vertex_points.(next) in
        point_representative.(midpoint) <- min a b;
        vertex_left.(midpoint) <- vertex;
        vertex_right.(midpoint) <- next;
        vertex_weight.(midpoint) <- 0.5;
        source_edge_of_output_vertex.(midpoint) <- edge
      end
    done;
    if !output <> primitive_offsets.(primitive + 1) then
      fail "independent curve vertex cardinality mismatch"
  done;
  let point_sources = Array.make point_offsets.(output_points) 0
  and point_weights = Array.make point_offsets.(output_points) 0. in
  run ?grain ?cancel source_primitives (fun primitive ->
    let first = topology.primitive_offsets.(primitive)
    and last = topology.primitive_offsets.(primitive + 1)
    and closed = Bytes.get topology.primitive_kinds primitive = '\002' in
    let output = ref primitive_offsets.(primitive) in
    for vertex = first to last - 1 do
      let at = point_offsets.(!output) in
      incr output;
      let smooth = scheme = Catmull_clark
          && (closed || (vertex > first && vertex + 1 < last)) in
      if smooth then begin
        let previous = if vertex > first then vertex - 1 else last - 1
        and next = if vertex + 1 < last then vertex + 1 else first in
        point_sources.(at) <- topology.vertex_points.(previous);
        point_weights.(at) <- 0.125;
        point_sources.(at + 1) <- topology.vertex_points.(vertex);
        point_weights.(at + 1) <- 0.75;
        point_sources.(at + 2) <- topology.vertex_points.(next);
        point_weights.(at + 2) <- 0.125
      end else begin
        point_sources.(at) <- topology.vertex_points.(vertex);
        point_weights.(at) <- 1.
      end;
      let next = index.next_vertex.(vertex) in
      if next >= 0 then begin
        let midpoint_at = point_offsets.(!output) in
        incr output;
        point_sources.(midpoint_at) <- topology.vertex_points.(vertex);
        point_weights.(midpoint_at) <- 0.5;
        point_sources.(midpoint_at + 1) <- topology.vertex_points.(next);
        point_weights.(midpoint_at + 1) <- 0.5
      end
    done);
  let output_topology = Topology.View.create_validated_owned
      ~point_count:output_points ~vertex_points ~primitive_offsets
      ~primitive_kinds:(Bytes.copy topology.primitive_kinds) in
  let output_index = match Geometry.edge_groups geometry with
    | [] -> None
    | _ -> Some (Topology_index.create ?cancel output_topology) in
  { source = geometry;
    output_topology; output_index; point_offsets; point_sources; point_weights;
    point_representative; point_old; vertex_left; vertex_right; vertex_weight;
    vertex_old; primitive_source = Array.init source_primitives Fun.id;
    source_edge_of_output_vertex }

let make_plan ?cancel ?grain ~independent scheme geometry =
  validate ?cancel geometry;
  let topology = Topology.View.view (Geometry.topology geometry) in
  let index_value = Topology_index.create ?cancel (Geometry.topology geometry) in
  let index = Topology_index.View.view index_value in
  if independent then independent_plan ?cancel ?grain scheme geometry topology index
  else shared_plan ?cancel ?grain scheme geometry topology index

let point_float ?cancel ?grain plan source =
  let count = Array.length plan.point_representative in
  let output = Array.make count 0. in
  run ?grain ?cancel count (fun point ->
    let sum = ref 0. in
    for at = plan.point_offsets.(point) to plan.point_offsets.(point + 1) - 1 do
      sum := !sum +. source.(plan.point_sources.(at)) *. plan.point_weights.(at)
    done;
    output.(point) <- !sum);
  output

let vertex_float ?cancel ?grain plan source =
  let count = Array.length plan.vertex_left in
  let output = Array.make count 0. in
  run ?grain ?cancel count (fun vertex ->
    let weight = plan.vertex_weight.(vertex) in
    let left = source.(plan.vertex_left.(vertex)) in
    output.(vertex) <- left +.
      ((source.(plan.vertex_right.(vertex)) -. left) *. weight));
  output

let primitive_float ?cancel ?grain plan source =
  Topology_remap.select_float ?cancel
    ~grain:(Option.value ~default:16_384 grain) plan.primitive_source source

let discrete_mapping plan owner = match owner with
  | Attribute.Point -> plan.point_representative
  | Attribute.Vertex -> Array.init (Array.length plan.vertex_left) (fun vertex ->
      if Bytes.get plan.vertex_old vertex <> '\000' then plan.vertex_left.(vertex)
      else min plan.vertex_left.(vertex) plan.vertex_right.(vertex))
  | Attribute.Primitive -> plan.primitive_source
  | Attribute.Detail -> [|0|]

let discrete ?cancel ?grain plan owner source =
  let mapping = discrete_mapping plan owner in
  Topology_remap.select ?cancel
    ~grain:(Option.value ~default:16_384 grain) mapping source

let numeric ?cancel ?grain plan owner source = match owner with
  | Attribute.Point -> point_float ?cancel ?grain plan source
  | Attribute.Vertex -> vertex_float ?cancel ?grain plan source
  | Attribute.Primitive -> primitive_float ?cancel ?grain plan source
  | Attribute.Detail -> Array.copy source

let remap_attribute ?cancel ?grain plan attribute =
  let owner = Attribute.owner attribute in
  let storage = match Attribute.View.storage attribute with
      | Attribute.Float values -> Attribute.Float (numeric ?cancel ?grain plan owner values)
      | Attribute.Int values -> Attribute.Int (discrete ?cancel ?grain plan owner values)
      | Attribute.Text values -> Attribute.Text (discrete ?cancel ?grain plan owner values)
      | Attribute.Int_array values ->
          Attribute.Int_array (Ragged_ops.remap_int ?cancel ?grain
            (discrete_mapping plan owner) values)
      | Attribute.Float_array values ->
          Attribute.Float_array (Ragged_ops.remap_float ?cancel ?grain
            (discrete_mapping plan owner) values)
      | Attribute.Float2 values ->
          let values = Packed.Float2.View.view values in
          Attribute.Float2 (Packed.Float2.of_owned
            ~x:(numeric ?cancel ?grain plan owner values.x)
            ~y:(numeric ?cancel ?grain plan owner values.y) |> Result.get_ok)
      | Attribute.Float3 values ->
          let values = Packed.Float3.View.view values in
          Attribute.Float3 (Packed.Float3.View.of_owned_exn
            ~x:(numeric ?cancel ?grain plan owner values.x)
            ~y:(numeric ?cancel ?grain plan owner values.y)
            ~z:(numeric ?cancel ?grain plan owner values.z))
      | Attribute.Float4 values ->
          let values = Packed.Float4.View.view values in
          Attribute.Float4 (Packed.Float4.of_owned
            ~x:(numeric ?cancel ?grain plan owner values.x)
            ~y:(numeric ?cancel ?grain plan owner values.y)
            ~z:(numeric ?cancel ?grain plan owner values.z)
            ~w:(numeric ?cancel ?grain plan owner values.w) |> Result.get_ok) in
  Some (Attribute.create_owned ~name:(Attribute.name attribute) ~owner storage
    |> Result.get_ok)

let remap_group ?cancel ?grain plan group =
  let owner = Group.owner group in
  let count = match owner with
    | Group.Point -> Array.length plan.point_representative
    | Group.Vertex -> Array.length plan.vertex_left
    | Group.Primitive -> Array.length plan.primitive_source in
  let target = Group.init ?grain ~owner ~name:(Group.name group) count (fun output ->
    Cancel.poll cancel output 4095;
    match owner with
    | Group.Point ->
        if Bytes.get plan.point_old output <> '\000' then
          Group.mem plan.point_representative.(output) group
        else begin
          let first = plan.point_offsets.(output) in
          Group.mem plan.point_sources.(first) group
          && Group.mem plan.point_sources.(first + 1) group
        end
    | Group.Vertex ->
        if Bytes.get plan.vertex_old output <> '\000' then
          Group.mem plan.vertex_left.(output) group
        else Group.mem plan.vertex_left.(output) group
          && Group.mem plan.vertex_right.(output) group
    | Group.Primitive -> Group.mem plan.primitive_source.(output) group) in
  if not (Group.is_ordered group) then target
  else
    let source_of_target = match owner with
      | Group.Point -> Array.init count (fun output ->
          if Bytes.get plan.point_old output <> '\000'
          then plan.point_representative.(output) else -1)
      | Group.Vertex -> Array.init count (fun output ->
          if Bytes.get plan.vertex_old output <> '\000'
          then plan.vertex_left.(output) else -1)
      | Group.Primitive -> plan.primitive_source in
    Group.View.remap_order ~source:group ~source_of_target target

let remap_edge_group ?cancel plan group =
  if Edge_group.topology_data_id group
      <> Topology.data_id (Geometry.topology plan.source) then
    fail "edge group belongs to a different topology";
  let output_index = match plan.output_index with
    | Some index -> index
    | None -> fail "curve edge-group remap is missing its output topology index" in
  let output_view = Topology_index.View.view output_index in
  let builder = Edge_group.Builder.create ~topology:plan.output_topology
      ~index:output_index ~name:(Edge_group.name group) in
  for vertex = 0 to Array.length plan.source_edge_of_output_vertex - 1 do
    Cancel.poll cancel vertex 16_383;
    let source_edge = plan.source_edge_of_output_vertex.(vertex) in
    if source_edge >= 0 && Edge_group.mem source_edge group then begin
      let output_edge = output_view.edge_of_vertex.(vertex) in
      if output_edge < 0 then fail "curve child edge ancestry is incomplete";
      Edge_group.Builder.set builder output_edge true
    end
  done;
  Edge_group.Builder.freeze builder

let once ?cancel ?grain ~independent scheme geometry =
  let plan = make_plan ?cancel ?grain ~independent scheme geometry in
  let source_positions = Packed.Float3.View.view (Geometry.positions geometry) in
  let positions = Packed.Float3.View.of_owned_exn
      ~x:(point_float ?cancel ?grain plan source_positions.x)
      ~y:(point_float ?cancel ?grain plan source_positions.y)
      ~z:(point_float ?cancel ?grain plan source_positions.z) in
  let attributes = Geometry.attributes geometry
      |> List.filter_map (remap_attribute ?cancel ?grain plan) in
  let groups = Geometry.groups geometry
      |> List.map (remap_group ?cancel ?grain plan) in
  let edge_groups = Geometry.edge_groups geometry
      |> List.map (remap_edge_group ?cancel plan) in
  Geometry.create ~positions ~topology:plan.output_topology ~attributes ~groups
    ~edge_groups () |> Result.get_ok

let iterate ?cancel ?grain ~independent scheme iterations geometry =
  let rec loop remaining current =
    Cancel.check_opt cancel;
    if remaining = 0 then current
    else loop (remaining - 1) (once ?cancel ?grain ~independent scheme current) in
  loop iterations geometry
