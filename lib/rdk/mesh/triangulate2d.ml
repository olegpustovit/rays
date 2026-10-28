
type projection =
  | Best_fit
  | Plane_xy
  | Plane_yz
  | Plane_zx
  | Plane of { origin : Rays_math.Vec3.t; normal : Rays_math.Vec3.t }
  | Point_attribute of string

let operation = "Rdk_mesh.Triangulate2d.triangulate_2d"

let finite3 x y z = Float.is_finite x && Float.is_finite y && Float.is_finite z

let normalize3 (x,y,z) =
  let scale = max (abs_float x) (max (abs_float y) (abs_float z)) in
  if scale = 0. || not (Float.is_finite scale) then
    invalid_arg (operation ^ ": plane normal must be finite and nonzero");
  let x = x /. scale and y = y /. scale and z = z /. scale in
  let length = sqrt ((x *. x) +. (y *. y) +. (z *. z)) in
  x /. length,y /. length,z /. length

let frame_of_normal normal =
  let nx,ny,nz = normalize3 (Rays_math.Vec3.to_triple normal) in
  let ax,ay,az =
    if abs_float nx <= abs_float ny && abs_float nx <= abs_float nz then 1.,0.,0.
    else if abs_float ny <= abs_float nz then 0.,1.,0. else 0.,0.,1. in
  let ux = (ay *. nz) -. (az *. ny)
  and uy = (az *. nx) -. (ax *. nz)
  and uz = (ax *. ny) -. (ay *. nx) in
  let ux,uy,uz = normalize3 (ux,uy,uz) in
  let vx = (ny *. uz) -. (nz *. uy)
  and vy = (nz *. ux) -. (nx *. uz)
  and vz = (nx *. uy) -. (ny *. ux) in
  (ux,uy,uz),(vx,vy,vz)

let smallest_eigenvector xx xy xz yy yz zz =
  let matrix = [|xx;xy;xz; xy;yy;yz; xz;yz;zz|]
  and vectors = [|1.;0.;0.; 0.;1.;0.; 0.;0.;1.|] in
  for _ = 0 to 15 do
    let p,q =
      let a01 = abs_float matrix.(1) and a02 = abs_float matrix.(2)
      and a12 = abs_float matrix.(5) in
      if a01 >= a02 && a01 >= a12 then 0,1
      else if a02 >= a12 then 0,2 else 1,2 in
    let apq = matrix.((p * 3) + q) in
    if apq <> 0. then begin
      let app = matrix.((p * 3) + p) and aqq = matrix.((q * 3) + q) in
      let angle = 0.5 *. atan2 (2. *. apq) (aqq -. app) in
      let cosine = cos angle and sine = sin angle in
      for k = 0 to 2 do
        if k <> p && k <> q then begin
          let akp = matrix.((k * 3) + p)
          and akq = matrix.((k * 3) + q) in
          let next_p = (cosine *. akp) -. (sine *. akq)
          and next_q = (sine *. akp) +. (cosine *. akq) in
          matrix.((k * 3) + p) <- next_p;
          matrix.((p * 3) + k) <- next_p;
          matrix.((k * 3) + q) <- next_q;
          matrix.((q * 3) + k) <- next_q
        end
      done;
      matrix.((p * 3) + p) <-
        (cosine *. cosine *. app) -. (2. *. sine *. cosine *. apq)
        +. (sine *. sine *. aqq);
      matrix.((q * 3) + q) <-
        (sine *. sine *. app) +. (2. *. sine *. cosine *. apq)
        +. (cosine *. cosine *. aqq);
      matrix.((p * 3) + q) <- 0.; matrix.((q * 3) + p) <- 0.;
      for row = 0 to 2 do
        let vip = vectors.((row * 3) + p)
        and viq = vectors.((row * 3) + q) in
        vectors.((row * 3) + p) <- (cosine *. vip) -. (sine *. viq);
        vectors.((row * 3) + q) <- (sine *. vip) +. (cosine *. viq)
      done
    end
  done;
  let column = if matrix.(0) <= matrix.(4) && matrix.(0) <= matrix.(8) then 0
    else if matrix.(4) <= matrix.(8) then 1 else 2 in
  Rays_math.Vec3.create vectors.(column) vectors.(3 + column) vectors.(6 + column)

let selected_points ?cancel ~grain selection geometry =
  let topology = Geometry.topology geometry in
  Result.bind (Element_selection.validate ~operation topology selection) (fun () ->
    match selection with
    | None -> Ok (Array.init (Geometry.point_count geometry) Fun.id)
    | Some selection ->
        Result.map (fun group ->
          let points = Array.make (Group.cardinality group) 0 and count = ref 0 in
          Group.iter_ordered (fun point -> points.(!count) <- point; incr count) group;
          points)
          (Element_selection.promote ?cancel ~grain ~destination:Group.Point
             selection topology))

let constraint_endpoint_selection ?cancel ~points ?constraint_edges
    ?constraint_primitives geometry =
  let point_count = Geometry.point_count geometry
  and topology_value = Geometry.topology geometry in
  let topology = Topology.Private.view topology_value in
  let selected = Bytes.make point_count '\000' in
  Array.iter (fun point -> Bytes.unsafe_set selected point '\001') points;
  let retained = Bytes.make point_count '\000' and retained_count = ref 0 in
  let retain point =
    if Bytes.unsafe_get selected point = '\000' then
      invalid_arg (operation ^
        ": every constraint endpoint must belong to the point selection");
    if Bytes.unsafe_get retained point = '\000' then begin
      Bytes.unsafe_set retained point '\001'; incr retained_count
    end in
  Option.iter (fun group ->
    if Edge_group.topology_data_id group <> Topology.data_id topology_value then
      invalid_arg (operation ^ ": constraint edge group belongs to different topology");
    let index = Topology_index.create ?cancel topology_value in
    let view = Topology_index.Private.view index in
    Edge_group.iter (fun edge ->
      if edge land 4095 = 0 then Cancel.check_opt cancel;
      retain view.edge_a.(edge); retain view.edge_b.(edge)) group) constraint_edges;
  Option.iter (fun group ->
    if Group.owner group <> Group.Primitive then
      invalid_arg (operation ^ ": constraint primitive group must own primitives");
    if Group.length group <> Geometry.primitive_count geometry then
      invalid_arg (operation ^ ": constraint primitive group length is invalid");
    Group.iter_ordered (fun primitive ->
      if primitive land 4095 = 0 then Cancel.check_opt cancel;
      for vertex = topology.primitive_offsets.(primitive)
          to topology.primitive_offsets.(primitive + 1) - 1 do
        retain topology.vertex_points.(vertex)
      done) group) constraint_primitives;
  if !retained_count = 0 then
    invalid_arg (operation ^
      ": Ignore Non-Constraint Points requires at least one constraint");
  let output = Array.make !retained_count 0 and at = ref 0 in
  Array.iter (fun point ->
    if Bytes.unsafe_get retained point <> '\000' then begin
      output.(!at) <- point; incr at
    end) points;
  output

let constraint_segments ?cancel ~grain ~polygon_winding ~silhouette
    ~silhouette_winding ~x ~y ?constraint_edges ?constraint_primitives
    geometry local_of_point triangulation =
  let topology_value = Geometry.topology geometry
  and topology = Topology.Private.view (Geometry.topology geometry) in
  let source = ref (Array.make 32 0) and winding = ref (Array.make 16 0)
  and length = ref 0 in
  let add_local ~weight a b =
    if a = b then
      invalid_arg (operation ^
        ": a constraint collapses after projected-coordinate deduplication");
    if !length + 2 > Array.length !source then begin
      if Array.length !source > Sys.max_array_length / 2 then
        invalid_arg (operation ^ ": constraint cardinality exceeds array limits");
      let output = Array.make (Array.length !source * 2) 0 in
      Array.blit !source 0 output 0 !length; source := output
    end;
    let segment = !length / 2 in
    if segment = Array.length !winding then begin
      if segment > Sys.max_array_length / 2 then
        invalid_arg (operation ^ ": constraint cardinality exceeds array limits");
      let output = Array.make (segment * 2) 0 in
      Array.blit !winding 0 output 0 segment; winding := output
    end;
    (!source).(!length) <- a; (!source).(!length + 1) <- b;
    (!winding).(segment) <- weight;
    length := !length + 2 in
  let add ~weight source_a source_b =
    let local_a = local_of_point.(source_a)
    and local_b = local_of_point.(source_b) in
    if local_a < 0 || local_b < 0 then
      invalid_arg (operation ^ ": every constraint endpoint must belong to the point selection");
    let unique_a = Delaunay2.source_unique triangulation local_a
    and unique_b = Delaunay2.source_unique triangulation local_b in
    let a = Delaunay2.unique_source triangulation unique_a
    and b = Delaunay2.unique_source triangulation unique_b in
    if a <> b then add_local ~weight a b
    else
      invalid_arg (operation ^
        ": a constraint collapses after projected-coordinate deduplication") in
  Option.iter (fun group ->
    if Edge_group.topology_data_id group <> Topology.data_id topology_value then
      invalid_arg (operation ^ ": constraint edge group belongs to different topology");
    let index = Topology_index.create ?cancel topology_value in
    let view = Topology_index.Private.view index in
    Edge_group.iter (fun edge ->
      if edge land 4095 = 0 then Cancel.check_opt cancel;
      add ~weight:0 view.edge_a.(edge) view.edge_b.(edge)) group) constraint_edges;
  Option.iter (fun group ->
    if Group.owner group <> Group.Primitive then
      invalid_arg (operation ^ ": constraint primitive group must own primitives");
    if Group.length group <> Geometry.primitive_count geometry then
      invalid_arg (operation ^ ": constraint primitive group length is invalid");
    Group.iter_ordered (fun primitive ->
      if primitive land 4095 = 0 then Cancel.check_opt cancel;
      let first = topology.primitive_offsets.(primitive)
      and last = topology.primitive_offsets.(primitive + 1) in
      let size = last - first
      and kind = Char.code (Bytes.unsafe_get topology.primitive_kinds primitive) in
      let segments = if kind = 1 then max 0 (size - 1) else size in
      let weight = if kind = 1 || not polygon_winding then 0 else 1 in
      for local = 0 to segments - 1 do
        let next = if local + 1 < size then local + 1 else 0 in
        add ~weight topology.vertex_points.(first + local)
          topology.vertex_points.(first + next)
      done) group) constraint_primitives;
  if silhouette then begin
    let primitive_count = Geometry.primitive_count geometry
    and vertex_count = Geometry.vertex_count geometry in
    let projected_vertex_points = Array.make vertex_count (-1) in
    let map_vertex vertex =
      if vertex land 4095 = 0 then Cancel.check_opt cancel;
      let source_point = topology.vertex_points.(vertex) in
      let local = local_of_point.(source_point) in
      if local >= 0 then begin
        let unique = Delaunay2.source_unique triangulation local in
        projected_vertex_points.(vertex) <-
          Delaunay2.unique_source triangulation unique
      end in
    if vertex_count > grain then Parallel.for_ ~chunk_size:grain ~start:0
        ~finish:(vertex_count - 1) map_vertex
    else for vertex = 0 to vertex_count - 1 do map_vertex vertex done;
    let signs = Array.make primitive_count Predicates.Zero
    and selected_polygons = Bytes.make primitive_count '\000' in
    for primitive = 0 to primitive_count - 1 do
      if primitive land 4095 = 0 then Cancel.check_opt cancel;
      if Char.code (Bytes.unsafe_get topology.primitive_kinds primitive) = 0 then begin
        let first = topology.primitive_offsets.(primitive)
        and last = topology.primitive_offsets.(primitive + 1) in
        let selected = ref 0 in
        for vertex = first to last - 1 do
          if projected_vertex_points.(vertex) >= 0 then incr selected
        done;
        if !selected <> 0 && !selected <> last - first then
          invalid_arg (operation ^
            ": silhouette polygons must be wholly inside or outside the point selection");
        if !selected > 0 then Bytes.unsafe_set selected_polygons primitive '\001'
      end
    done;
    let classify primitive =
      if primitive land 4095 = 0 then Cancel.check_opt cancel;
      if Bytes.unsafe_get selected_polygons primitive = '\001' then begin
        let first = topology.primitive_offsets.(primitive) in
        signs.(primitive) <- Predicates.polygon_area_sign_packed ~x ~y
            ~points:projected_vertex_points ~first
            ~count:(topology.primitive_offsets.(primitive + 1) - first)
      end in
    if primitive_count > grain then Parallel.for_ ~chunk_size:grain ~start:0
        ~finish:(primitive_count - 1) classify
    else for primitive = 0 to primitive_count - 1 do classify primitive done;
    let chosen = if Array.exists ((=) Predicates.Positive) signs then
        Predicates.Positive
      else Predicates.Negative in
    let index = Topology_index.create ?cancel topology_value in
    let view = Topology_index.Private.view index in
    let edge_count = Array.length view.edge_a
    and edge_winding = Array.make (Array.length view.edge_a) 0 in
    let classify_edge edge =
      if edge land 4095 = 0 then Cancel.check_opt cancel;
      let first = view.edge_offsets.(edge)
      and last = view.edge_offsets.(edge + 1) and weight = ref 0 in
      for incidence = first to last - 1 do
        let vertex = view.edge_vertices.(incidence) in
        let primitive = view.primitive_of_vertex.(vertex) in
        if signs.(primitive) = chosen then begin
          let next = view.next_vertex.(vertex) in
          if next >= 0 then begin
            let a = projected_vertex_points.(vertex)
            and b = projected_vertex_points.(next) in
            if a <> b then begin
              let contribution = if a < b then 1 else -1 in
              if (contribution > 0 && !weight = max_int)
                  || (contribution < 0 && !weight = min_int) then
                invalid_arg (operation ^ ": silhouette winding exceeds integer range");
              weight := !weight + contribution
            end
          end
        end
      done;
      edge_winding.(edge) <- !weight in
    if edge_count > grain then Parallel.for_ ~chunk_size:grain ~start:0
        ~finish:(edge_count - 1) classify_edge
    else for edge = 0 to edge_count - 1 do classify_edge edge done;
    for edge = 0 to edge_count - 1 do
      if edge land 4095 = 0 then Cancel.check_opt cancel;
      let weight = edge_winding.(edge) in
      if weight <> 0 then begin
        let source_a = view.edge_a.(edge) and source_b = view.edge_b.(edge) in
        let local_a = local_of_point.(source_a)
        and local_b = local_of_point.(source_b) in
        if local_a < 0 || local_b < 0 then
          invalid_arg (operation ^
            ": silhouette edge crosses the point selection boundary");
        let a = Delaunay2.unique_source triangulation
            (Delaunay2.source_unique triangulation local_a)
        and b = Delaunay2.unique_source triangulation
            (Delaunay2.source_unique triangulation local_b) in
        if a <> b then begin
          let canonical_a,canonical_b,weight = if a < b then a,b,weight
            else b,a,- weight in
          add_local ~weight:(if silhouette_winding then weight else 0)
            canonical_a canonical_b
        end
      end
    done
  end;
  Array.sub !source 0 !length,Array.sub !winding 0 (!length / 2)

type projected_points = {
  x : float array;
  y : float array;
  embed : float -> float -> float * float * float;
}

let planes_from_positions ?cancel ~grain positions points =
  let source = Packed.Float3.Private.view positions and count = Array.length points in
  if count = 0 then invalid_arg (operation ^ ": selection contains no points");
  let maximum = ref 0. in
  for index = 0 to count - 1 do
    if index land 4095 = 0 then Cancel.check_opt cancel;
    let point = points.(index) in
    if not (finite3 source.x.(point) source.y.(point) source.z.(point)) then
      invalid_arg (Printf.sprintf "%s: point %d has a non-finite position"
        operation point);
    maximum := max !maximum (max (abs_float source.x.(point))
        (max (abs_float source.y.(point)) (abs_float source.z.(point))))
  done;
  let scale = if !maximum = 0. then 1. else !maximum in
  let mx = ref 0. and my = ref 0. and mz = ref 0. in
  for index = 0 to count - 1 do
    let point = points.(index) and n = float_of_int (index + 1) in
    mx := !mx +. ((source.x.(point) /. scale -. !mx) /. n);
    my := !my +. ((source.y.(point) /. scale -. !my) /. n);
    mz := !mz +. ((source.z.(point) /. scale -. !mz) /. n)
  done;
  let xx = ref 0. and xy = ref 0. and xz = ref 0.
  and yy = ref 0. and yz = ref 0. and zz = ref 0. in
  for index = 0 to count - 1 do
    let point = points.(index) in
    let x = (source.x.(point) /. scale) -. !mx
    and y = (source.y.(point) /. scale) -. !my
    and z = (source.z.(point) /. scale) -. !mz in
    xx := !xx +. (x *. x); xy := !xy +. (x *. y); xz := !xz +. (x *. z);
    yy := !yy +. (y *. y); yz := !yz +. (y *. z); zz := !zz +. (z *. z)
  done;
  let normal = smallest_eigenvector !xx !xy !xz !yy !yz !zz in
  let u,v = frame_of_normal normal in
  let output_x = Array.make count 0. and output_y = Array.make count 0. in
  let ux,uy,uz = u and vx,vy,vz = v in
  let project index =
    if index land 4095 = 0 then Cancel.check_opt cancel;
    let point = points.(index) in
    let x = (source.x.(point) /. scale) -. !mx
    and y = (source.y.(point) /. scale) -. !my
    and z = (source.z.(point) /. scale) -. !mz in
    output_x.(index) <- (x *. ux) +. (y *. uy) +. (z *. uz);
    output_y.(index) <- (x *. vx) +. (y *. vy) +. (z *. vz) in
  if count > grain then Parallel.for_ ~chunk_size:grain ~start:0
      ~finish:(count - 1) project
  else for index = 0 to count - 1 do project index done;
  let cx = !mx *. scale and cy = !my *. scale and cz = !mz *. scale in
  let embed x y =
    cx +. (scale *. ((x *. ux) +. (y *. vx))),
    cy +. (scale *. ((x *. uy) +. (y *. vy))),
    cz +. (scale *. ((x *. uz) +. (y *. vz))) in
  { x = output_x; y = output_y; embed }

let project ?cancel ~grain projection geometry points =
  let positions = Packed.Float3.Private.view (Geometry.positions geometry)
  and count = Array.length points in
  let fill coordinate_x coordinate_y =
    let x = Array.make count 0. and y = Array.make count 0. in
    let one index =
      if index land 4095 = 0 then Cancel.check_opt cancel;
      let point = points.(index) in
      let px = coordinate_x point and py = coordinate_y point in
      if not (Float.is_finite px && Float.is_finite py) then
        invalid_arg (Printf.sprintf
          "%s: point %d has a non-finite projected position" operation point);
      x.(index) <- px; y.(index) <- py in
    if count > grain then Parallel.for_ ~chunk_size:grain ~start:0
        ~finish:(count - 1) one
    else for index = 0 to count - 1 do one index done;
    x,y in
  match projection with
  | Plane_xy ->
      let x,y = fill (fun point -> positions.x.(point))
          (fun point -> positions.y.(point)) in
      { x; y; embed = (fun x y -> x,y,0.) }
  | Plane_yz ->
      let x,y = fill (fun point -> positions.y.(point))
          (fun point -> positions.z.(point)) in
      { x; y; embed = (fun y z -> 0.,y,z) }
  | Plane_zx ->
      let x,y = fill (fun point -> positions.z.(point))
          (fun point -> positions.x.(point)) in
      { x; y; embed = (fun z x -> x,0.,z) }
  | Best_fit -> planes_from_positions ?cancel ~grain
      (Geometry.positions geometry) points
  | Plane { origin; normal } ->
      let ox,oy,oz = Rays_math.Vec3.to_triple origin in
      if not (finite3 ox oy oz) then
        invalid_arg (operation ^ ": plane origin must be finite");
      let (ux,uy,uz),(vx,vy,vz) = frame_of_normal normal in
      let x,y = fill (fun point ->
          let x = positions.x.(point) -. ox and y = positions.y.(point) -. oy
          and z = positions.z.(point) -. oz in
          (x *. ux) +. (y *. uy) +. (z *. uz))
        (fun point ->
          let x = positions.x.(point) -. ox and y = positions.y.(point) -. oy
          and z = positions.z.(point) -. oz in
          (x *. vx) +. (y *. vy) +. (z *. vz)) in
      { x; y; embed = (fun x y ->
          ox +. (x *. ux) +. (y *. vx),
          oy +. (x *. uy) +. (y *. vy),
          oz +. (x *. uz) +. (y *. vz)) }
  | Point_attribute name ->
      if String.trim name = "" then
        invalid_arg (operation ^ ": point position attribute name is empty");
      match Geometry.find_attribute ~owner:Attribute.Point name geometry with
      | None -> invalid_arg (Printf.sprintf "%s: missing point attribute %S"
          operation name)
      | Some attribute ->
          (match Attribute.Private.storage attribute with
           | Attribute.Float2 values ->
               let values = Packed.Float2.Private.view values in
               let x,y = fill (fun point -> values.x.(point))
                   (fun point -> values.y.(point)) in
               { x; y; embed = (fun x y -> x,y,0.) }
           | Attribute.Float3 values ->
               let values = Packed.Float3.Private.view values in
               let x,y = fill (fun point -> values.x.(point))
                   (fun point -> values.y.(point)) in
               { x; y; embed = (fun x y -> x,y,0.) }
           | _ -> invalid_arg (Printf.sprintf
               "%s: point attribute %S must be float2 or float3" operation name))

type point_materialization = {
  positions : Packed.Float3.t;
  representative : int array;
  roots : int array;
  parent_first : int array;
  parent_second : int array;
  parent_third : int array;
  weight_first : float array;
  weight_second : float array;
  weight_third : float array;
  value_count : int;
  source_point_count : int;
  local_to_output : int -> int;
  split_count : int;
  refinement_count : int;
}

let weighted_value source first second third wa wb wc =
  let a = source.(first) and b = source.(second) and c = source.(third) in
  let scale = max (abs_float a) (max (abs_float b) (abs_float c)) in
  if scale = 0. then 0.
  else if Float.is_finite scale then
    scale *. (((a /. scale) *. wa) +. ((b /. scale) *. wb)
      +. ((c /. scale) *. wc))
  else (a *. wa) +. (b *. wb) +. (c *. wc)

let materialization ?cancel ~grain ~restore_original_point_positions
    ~projected_x ~projected_y ~embed ~points ~arrangement ~refinement geometry =
  let source_point_count = Geometry.point_count geometry
  and selected_count = Array.length points
  and split_count = match arrangement with None -> 0
    | Some value -> Planar_constraints.split_point_count value
  and refinement_count = match refinement with None -> 0
    | Some value -> Planar_refinement.new_point_count value in
  let initial_local_count = selected_count + split_count
  and output_point_count = source_point_count + split_count + refinement_count in
  let provenance_count = match refinement with None -> 0
    | Some value -> Planar_refinement.provenance_node_count value in
  if output_point_count > Sys.max_array_length - provenance_count then
    invalid_arg (operation ^ ": point provenance exceeds array limits");
  let value_count = output_point_count + provenance_count in
  let local_to_output point =
    if point < selected_count then points.(point)
    else if point < initial_local_count then
      source_point_count + (point - selected_count)
    else source_point_count + split_count + (point - initial_local_count) in
  let parent_first = Array.init value_count Fun.id
  and parent_second = Array.init value_count Fun.id
  and parent_third = Array.init value_count Fun.id
  and weight_first = Array.make value_count 1.
  and weight_second = Array.make value_count 0.
  and weight_third = Array.make value_count 0.
  and roots = Array.init output_point_count Fun.id in
  Option.iter (fun arrangement ->
    for split = 0 to split_count - 1 do
      let output = source_point_count + split
      and weight = Planar_constraints.split_source_parameter arrangement split in
      parent_first.(output) <- points.(Planar_constraints.split_source_first
          arrangement split);
      parent_second.(output) <- points.(Planar_constraints.split_source_second
          arrangement split);
      parent_third.(output) <- parent_first.(output);
      weight_first.(output) <- 1. -. weight; weight_second.(output) <- weight
    done) arrangement;
  Option.iter (fun refinement ->
    let map_reference reference =
      if reference < initial_local_count then local_to_output reference
      else output_point_count + reference - initial_local_count in
    for node = 0 to provenance_count - 1 do
      let slot = output_point_count + node in
      parent_first.(slot) <- map_reference
          (Planar_refinement.provenance_parent_first refinement node);
      parent_second.(slot) <- map_reference
          (Planar_refinement.provenance_parent_second refinement node);
      parent_third.(slot) <- map_reference
          (Planar_refinement.provenance_parent_third refinement node);
      let wa,wb,wc = Planar_refinement.provenance_parent_weights refinement node in
      weight_first.(slot) <- wa; weight_second.(slot) <- wb;
      weight_third.(slot) <- wc
    done;
    for generated = 0 to refinement_count - 1 do
      let local = initial_local_count + generated in
      roots.(local_to_output local) <- map_reference
          (Planar_refinement.point_provenance_ref refinement local)
    done) refinement;
  let value_representative = Array.init value_count Fun.id in
  for output = source_point_count to source_point_count + split_count - 1 do
    value_representative.(output) <- value_representative.
        ((if weight_second.(output) < 0.5 then parent_first
          else parent_second).(output))
  done;
  for node = 0 to provenance_count - 1 do
      let output = output_point_count + node in
      let best_weight = ref weight_first.(output)
      and chosen = ref value_representative.(parent_first.(output)) in
      let consider weight point =
        if weight > !best_weight || (weight = !best_weight && point < !chosen) then
          begin best_weight := weight; chosen := point end in
      consider weight_second.(output)
        value_representative.(parent_second.(output));
      consider weight_third.(output)
        value_representative.(parent_third.(output));
      value_representative.(output) <- !chosen
  done;
  let representative = Array.init output_point_count (fun output ->
      value_representative.(roots.(output))) in
  let evaluate source =
    let values = Array.make value_count 0. in
    Array.blit source 0 values 0 source_point_count;
    for slot = source_point_count to source_point_count + split_count - 1 do
      values.(slot) <- weighted_value values parent_first.(slot)
          parent_second.(slot) parent_third.(slot) weight_first.(slot)
          weight_second.(slot) weight_third.(slot)
    done;
    for node = 0 to provenance_count - 1 do
      let slot = output_point_count + node in
      values.(slot) <- weighted_value values parent_first.(slot)
          parent_second.(slot) parent_third.(slot) weight_first.(slot)
          weight_second.(slot) weight_third.(slot)
    done;
    for output = source_point_count + split_count to output_point_count - 1 do
      values.(output) <- values.(roots.(output))
    done;
    Array.sub values 0 output_point_count in
  let positions = if not restore_original_point_positions then begin
    let source = Packed.Float3.Private.view (Geometry.positions geometry) in
    let x = Array.make output_point_count 0. and y = Array.make output_point_count 0.
    and z = Array.make output_point_count 0. in
    Array.blit source.x 0 x 0 source_point_count;
    Array.blit source.y 0 y 0 source_point_count;
    Array.blit source.z 0 z 0 source_point_count;
    let local_x,local_y = match refinement,arrangement with
      | Some refinement,_ -> Planar_refinement.Private.approximate_x refinement,
          Planar_refinement.Private.approximate_y refinement
      | None,Some arrangement ->
          let view = Planar_constraints.Private.view arrangement in view.x,view.y
      | None,None -> projected_x,projected_y in
    let local_count = initial_local_count + refinement_count in
    if Array.length local_x <> local_count || Array.length local_y <> local_count then
      invalid_arg (operation ^ ": projected point materialization is malformed");
    let fill local =
      if local land 4095 = 0 then Cancel.check_opt cancel;
      let output = local_to_output local in
      let px,py,pz = embed local_x.(local) local_y.(local) in
      if not (finite3 px py pz) then
        invalid_arg (Printf.sprintf
          "%s: projected output point %d is non-finite" operation output);
      x.(output) <- px; y.(output) <- py; z.(output) <- pz in
    if local_count > grain then Parallel.for_ ~chunk_size:grain ~start:0
        ~finish:(local_count - 1) fill
    else for local = 0 to local_count - 1 do fill local done;
    Packed.Float3.Private.of_owned_exn ~x ~y ~z
  end else if split_count + refinement_count = 0 then
    Geometry.positions geometry
  else begin
    let source = Packed.Float3.Private.view (Geometry.positions geometry) in
    let x = evaluate source.x and y = evaluate source.y and z = evaluate source.z in
    for output = source_point_count to output_point_count - 1 do
      if output land 4095 = 0 then Cancel.check_opt cancel;
      if not (finite3 x.(output) y.(output) z.(output)) then
        invalid_arg (Printf.sprintf
          "%s: generated point %d has a non-finite 3D interpolation"
          operation output)
    done;
    Packed.Float3.Private.of_owned_exn ~x ~y ~z
  end in
  { positions; representative; roots; parent_first; parent_second; parent_third;
    weight_first; weight_second; weight_third; value_count; source_point_count;
    local_to_output; split_count; refinement_count }

let interpolate_point_attribute ?cancel ~grain plan attribute =
  let count = Array.length plan.representative in
  let numeric source =
    let values = Array.make plan.value_count 0. in
    Array.blit source 0 values 0 plan.source_point_count;
    for point = plan.source_point_count
        to plan.source_point_count + plan.split_count - 1 do
      if point land 4095 = 0 then Cancel.check_opt cancel;
      values.(point) <- weighted_value values
          plan.parent_first.(point) plan.parent_second.(point)
          plan.parent_third.(point) plan.weight_first.(point)
          plan.weight_second.(point) plan.weight_third.(point)
    done;
    for point = count to plan.value_count - 1 do
      if point land 4095 = 0 then Cancel.check_opt cancel;
      values.(point) <- weighted_value values
          plan.parent_first.(point) plan.parent_second.(point)
          plan.parent_third.(point) plan.weight_first.(point)
          plan.weight_second.(point) plan.weight_third.(point)
    done;
    let output = Array.sub values 0 count in
    for point = plan.source_point_count + plan.split_count to count - 1 do
      output.(point) <- values.(plan.roots.(point))
    done;
    output in
  let discrete source = Array.init count (fun point -> source.(plan.representative.(point))) in
  let storage = match Attribute.Private.storage attribute with
    | Attribute.Float values -> Attribute.Float (numeric values)
    | Attribute.Int values -> Attribute.Int (discrete values)
    | Attribute.Int_array values ->
        Attribute.Int_array (Ragged_ops.remap_int ?cancel ~grain plan.representative values)
    | Attribute.Float_array values ->
        Attribute.Float_array (Ragged_ops.remap_float ?cancel ~grain plan.representative values)
    | Attribute.Text values -> Attribute.Text (discrete values)
    | Attribute.Float2 values ->
        let values = Packed.Float2.Private.view values in
        Attribute.Float2 (Packed.Float2.of_owned ~x:(numeric values.x)
          ~y:(numeric values.y) |> Result.get_ok)
    | Attribute.Float3 values ->
        let values = Packed.Float3.Private.view values in
        let x = numeric values.x and y = numeric values.y and z = numeric values.z in
        if Attribute.name attribute = "N" then
          for point = 0 to count - 1 do
            let length = sqrt ((x.(point) *. x.(point)) +. (y.(point) *. y.(point))
                +. (z.(point) *. z.(point))) in
            if length > 1e-20 then begin
              x.(point) <- x.(point) /. length; y.(point) <- y.(point) /. length;
              z.(point) <- z.(point) /. length
            end
          done;
        Attribute.Float3 (Packed.Float3.Private.of_owned_exn ~x ~y ~z)
    | Attribute.Float4 values ->
        let values = Packed.Float4.Private.view values in
        Attribute.Float4 (Packed.Float4.of_owned ~x:(numeric values.x)
          ~y:(numeric values.y) ~z:(numeric values.z) ~w:(numeric values.w)
          |> Result.get_ok) in
  Attribute.create_owned ~name:(Attribute.name attribute) ~owner:Attribute.Point storage
  |> Result.get_ok

let checked_add label first second =
  if first < 0 || second < 0 || first > max_int - second then
    invalid_arg (operation ^ ": " ^ label ^ " exceeds integer range");
  first + second

let mapped_array ?cancel ~grain ~default mapping source =
  let count = Array.length mapping in
  let output = Array.make count default in
  let fill target =
    if target land 4095 = 0 then Cancel.check_opt cancel;
    let source_index = mapping.(target) in
    if source_index >= 0 then output.(target) <- source.(source_index) in
  if count > grain then Parallel.for_ ~chunk_size:grain ~start:0
      ~finish:(count - 1) fill
  else for target = 0 to count - 1 do fill target done;
  output

let defaulted_attribute ?cancel ~grain mapping attribute =
  let storage = match Attribute.Private.storage attribute with
    | Attribute.Float values ->
        Attribute.Float (mapped_array ?cancel ~grain ~default:0. mapping values)
    | Attribute.Int values ->
        Attribute.Int (mapped_array ?cancel ~grain ~default:0 mapping values)
    | Attribute.Text values ->
        Attribute.Text (mapped_array ?cancel ~grain ~default:"" mapping values)
    | Attribute.Int_array values ->
        Attribute.Int_array (Ragged_ops.remap_int ?cancel ~grain mapping values)
    | Attribute.Float_array values ->
        Attribute.Float_array (Ragged_ops.remap_float ?cancel ~grain mapping values)
    | Attribute.Float2 values ->
        let values = Packed.Float2.Private.view values in
        Attribute.Float2 (Packed.Float2.of_owned
          ~x:(mapped_array ?cancel ~grain ~default:0. mapping values.x)
          ~y:(mapped_array ?cancel ~grain ~default:0. mapping values.y)
          |> Result.get_ok)
    | Attribute.Float3 values ->
        let values = Packed.Float3.Private.view values in
        Attribute.Float3 (Packed.Float3.Private.of_owned_exn
          ~x:(mapped_array ?cancel ~grain ~default:0. mapping values.x)
          ~y:(mapped_array ?cancel ~grain ~default:0. mapping values.y)
          ~z:(mapped_array ?cancel ~grain ~default:0. mapping values.z))
    | Attribute.Float4 values ->
        let values = Packed.Float4.Private.view values in
        Attribute.Float4 (Packed.Float4.of_owned
          ~x:(mapped_array ?cancel ~grain ~default:0. mapping values.x)
          ~y:(mapped_array ?cancel ~grain ~default:0. mapping values.y)
          ~z:(mapped_array ?cancel ~grain ~default:0. mapping values.z)
          ~w:(mapped_array ?cancel ~grain ~default:0. mapping values.w)
          |> Result.get_ok) in
  Attribute.create_owned ~name:(Attribute.name attribute)
    ~owner:(Attribute.owner attribute) storage |> Result.get_ok

let defaulted_group ?cancel ~grain mapping group =
  let output = Group.init ~grain ~owner:(Group.owner group)
      ~name:(Group.name group) (Array.length mapping) (fun target ->
        if target land 4095 = 0 then Cancel.check_opt cancel;
        let source = mapping.(target) in source >= 0 && Group.mem source group) in
  Group.Private.remap_order ~source:group ~source_of_target:mapping output

type output_topology_plan = {
  topology : Topology.t;
  vertex_source : int array;
  primitive_source : int array;
  kept_primitive_count : int;
}

let output_topology ?cancel ~grain ~keep_primitives ~constraint_primitives
    ~source_point_output ~output_point_count ~local_to_output ~triangle_points
    geometry =
  let triangle_count = Array.length triangle_points / 3 in
  if not keep_primitives then begin
    let vertex_count = Array.length triangle_points in
    let vertex_points = Array.make vertex_count 0 in
    let fill vertex =
      if vertex land 4095 = 0 then Cancel.check_opt cancel;
      vertex_points.(vertex) <- local_to_output triangle_points.(vertex) in
    if vertex_count > grain then Parallel.for_ ~chunk_size:grain ~start:0
        ~finish:(vertex_count - 1) fill
    else for vertex = 0 to vertex_count - 1 do fill vertex done;
    let primitive_offsets = Array.init (triangle_count + 1)
        (fun primitive -> primitive * 3) in
    { topology = Topology.polygons_owned ~point_count:output_point_count
          ~vertex_points ~primitive_offsets |> Result.get_ok;
      vertex_source = [||]; primitive_source = [||];
      kept_primitive_count = 0 }
  end else begin
    let source_topology_value = Geometry.topology geometry in
    let source = Topology.Private.view source_topology_value in
    let source_primitive_count = Geometry.primitive_count geometry in
    Option.iter (fun group ->
      if Group.owner group <> Group.Primitive
          || Group.length group <> source_primitive_count then
        invalid_arg (operation ^
          ": constraint primitive group is incompatible with input topology"))
      constraint_primitives;
    let kept_vertex_offsets = Array.make (source_primitive_count + 1) 0
    and source_to_output = Array.make source_primitive_count (-1) in
    let kept_primitive_count = ref 0 in
    for primitive = 0 to source_primitive_count - 1 do
      if primitive land 4095 = 0 then Cancel.check_opt cancel;
      let retained = match constraint_primitives with
        | None -> true | Some group -> not (Group.mem primitive group) in
      kept_vertex_offsets.(primitive + 1) <- kept_vertex_offsets.(primitive);
      if retained then begin
        source_to_output.(primitive) <- !kept_primitive_count;
        incr kept_primitive_count;
        kept_vertex_offsets.(primitive + 1) <- checked_add "kept vertex count"
            kept_vertex_offsets.(primitive)
            (source.primitive_offsets.(primitive + 1)
             - source.primitive_offsets.(primitive))
      end
    done;
    let kept_vertex_count = kept_vertex_offsets.(source_primitive_count) in
    let output_primitive_count = checked_add "output primitive count"
        !kept_primitive_count triangle_count
    and output_vertex_count = checked_add "output vertex count"
        kept_vertex_count (Array.length triangle_points) in
    let vertex_points = Array.make output_vertex_count 0
    and vertex_source = Array.make output_vertex_count (-1)
    and primitive_offsets = Array.make (output_primitive_count + 1) 0
    and primitive_kinds = Bytes.make output_primitive_count '\000'
    and primitive_source = Array.make output_primitive_count (-1) in
    let output_point_of_source = if Array.length source_point_output = 0 then
        Fun.id else fun point -> source_point_output.(point) in
    let fill_source primitive =
      if primitive land 1023 = 0 then Cancel.check_opt cancel;
      let output = source_to_output.(primitive) in
      if output >= 0 then begin
        let source_first = source.primitive_offsets.(primitive)
        and source_last = source.primitive_offsets.(primitive + 1)
        and target_first = kept_vertex_offsets.(primitive) in
        primitive_offsets.(output) <- target_first;
        Bytes.unsafe_set primitive_kinds output
          (Bytes.unsafe_get source.primitive_kinds primitive);
        primitive_source.(output) <- primitive;
        for local = 0 to source_last - source_first - 1 do
          let source_vertex = source_first + local
          and target_vertex = target_first + local in
          vertex_points.(target_vertex) <-
            output_point_of_source (source.vertex_points.(source_vertex));
          vertex_source.(target_vertex) <- source_vertex
        done
      end in
    if source_primitive_count > grain then
      Parallel.for_ ~chunk_size:grain ~start:0
        ~finish:(source_primitive_count - 1) fill_source
    else for primitive = 0 to source_primitive_count - 1 do
      fill_source primitive
    done;
    let fill_triangle triangle =
      if triangle land 4095 = 0 then Cancel.check_opt cancel;
      let primitive = !kept_primitive_count + triangle
      and vertex = kept_vertex_count + (triangle * 3) in
      primitive_offsets.(primitive) <- vertex;
      vertex_points.(vertex) <- local_to_output triangle_points.(triangle * 3);
      vertex_points.(vertex + 1) <-
        local_to_output triangle_points.((triangle * 3) + 1);
      vertex_points.(vertex + 2) <-
        local_to_output triangle_points.((triangle * 3) + 2) in
    if triangle_count > grain then Parallel.for_ ~chunk_size:grain ~start:0
        ~finish:(triangle_count - 1) fill_triangle
    else for triangle = 0 to triangle_count - 1 do fill_triangle triangle done;
    primitive_offsets.(output_primitive_count) <- output_vertex_count;
    { topology = Topology.Private.create_validated_owned
          ~point_count:output_point_count ~vertex_points ~primitive_offsets
          ~primitive_kinds;
      vertex_source; primitive_source;
      kept_primitive_count = !kept_primitive_count }
  end

let replace_group group groups =
  group :: List.filter (fun existing ->
    Group.owner existing <> Group.owner group
    || not (String.equal (Group.name existing) (Group.name group))) groups

let remap_source_edge_groups ?cancel ~point_map ~target_topology ~target_index
    geometry =
  match Geometry.edge_groups geometry with
  | [] -> []
  | groups ->
      let source_topology = Geometry.topology geometry in
      let source_index = Topology_index.create ?cancel source_topology in
      let point_map = if Array.length point_map = 0 then
          Array.init (Geometry.point_count geometry) Fun.id else point_map in
      List.map (fun group ->
        match Edge_group.remap ?cancel ~source_index ~target_topology
            ~target_index ~point_map group with
        | Ok group -> group
        | Error message -> invalid_arg (operation ^ ": " ^ message)) groups

let constraint_only_seed ?cancel ~grain ~seed ~points ~x ~y ~constraints
    ~constraint_winding () =
  let count = Array.length points in
  if Array.length constraints = 0 then
    Error (operation ^ ": Ignore Non-Constraint Points requires at least one constraint")
  else begin
    let used = Bytes.make count '\000' and used_count = ref 0 in
    Array.iter (fun point ->
      if point < 0 || point >= count then
        invalid_arg (operation ^ ": internal constraint endpoint is out of range");
      if Bytes.unsafe_get used point = '\000' then begin
        Bytes.unsafe_set used point '\001'; incr used_count
      end) constraints;
    let output_points = Array.make !used_count 0
    and output_x = Array.make !used_count 0.
    and output_y = Array.make !used_count 0.
    and target_of_source = Array.make count (-1) in
    let target = ref 0 in
    for source = 0 to count - 1 do
      if source land 4095 = 0 then Cancel.check_opt cancel;
      if Bytes.unsafe_get used source <> '\000' then begin
        target_of_source.(source) <- !target;
        output_points.(!target) <- points.(source);
        output_x.(!target) <- x.(source); output_y.(!target) <- y.(source);
        incr target
      end
    done;
    let output_constraints = Array.make (Array.length constraints) 0 in
    let remap index =
      if index land 4095 = 0 then Cancel.check_opt cancel;
      let target = target_of_source.(constraints.(index)) in
      if target < 0 then
        invalid_arg (operation ^ ": internal constraint endpoint was not retained");
      output_constraints.(index) <- target in
    if Array.length constraints > grain then Parallel.for_ ~chunk_size:grain
        ~start:0 ~finish:(Array.length constraints - 1) remap
    else for index = 0 to Array.length constraints - 1 do remap index done;
    Result.map (fun triangulation ->
      output_points,output_x,output_y,triangulation,
      output_constraints,constraint_winding)
      (Delaunay2.build ?cancel ~seed ~x:output_x ~y:output_y ())
  end

let exact_points x y arrangement = match arrangement with
  | Some arrangement -> Array.init (Planar_constraints.point_count arrangement)
      (Planar_constraints.Private.point arrangement)
  | None ->
      let source = Implicit_point.source ~x ~y ~z:(Array.make (Array.length x) 0.)
          |> Result.get_ok in
      Array.init (Array.length x) (fun point ->
        Implicit_point.explicit source point |> Result.get_ok)

type params = {
  grain : int;
  selection : Element_selection.t option;
  constraint_edges : Edge_group.t option;
  constraint_primitives : Group.t option;
  projection : projection;
  seed : int64;
  split_crossing_constraints : bool;
  flood_from_hull_boundary : bool;
  remove_outside_constraint_polygons : bool;
  silhouette_constraints : bool;
  remove_outside_silhouette : bool;
  ignore_non_constraint_points : bool;
  remove_duplicate_points : bool;
  refine : bool;
  allow_constraint_splitting : bool;
  minimum_angle : float;
  maximum_area : float option;
  target_edge_length : float option;
  minimum_edge_length : float;
  maximum_new_points : int;
  regularization_steps : int;
  allow_movement_of_interior_input_points : bool;
  preserve_point_payload : bool;
  restore_original_point_positions : bool;
  keep_primitives : bool;
  split_point_group : string option;
  refinement_point_group : string option;
  triangle_group : string option;
  constraint_group : string option;
  remove_unused_points : bool;
  recompute_point_normals : bool;
}
let run ?cancel { grain; selection; constraint_edges; constraint_primitives;
    projection; seed; split_crossing_constraints; flood_from_hull_boundary;
    remove_outside_constraint_polygons; silhouette_constraints;
    remove_outside_silhouette; ignore_non_constraint_points;
    remove_duplicate_points; refine; allow_constraint_splitting; minimum_angle;
    maximum_area; target_edge_length; minimum_edge_length; maximum_new_points;
    regularization_steps; allow_movement_of_interior_input_points;
    preserve_point_payload; restore_original_point_positions; keep_primitives;
    split_point_group; refinement_point_group; triangle_group; constraint_group;
    remove_unused_points; recompute_point_normals } geometry =
  Error.guard ~operation:"triangulate_2d" ~code:"invalid_triangulation" @@ fun () ->
  let finish output =
    Result.bind (if remove_unused_points then
        Error.unguard (Compact_points.run ?cancel ~grain output)
        else Ok output) (fun output ->
      if recompute_point_normals
          && Option.is_some (Geometry.find_attribute
            ~owner:Attribute.Point "N" geometry) then
        Error.unguard (Normal_ops.run ?cancel ~grain ~owner:Attribute.Point
          ~attribute:"N" output)
      else Ok output) in
  Result.bind begin
  try
    if grain <= 0 then invalid_arg (operation ^ ": grain must be positive");
    Option.iter (fun name -> if String.trim name = "" then
      invalid_arg (operation ^ ": triangle group name must be non-empty"))
      triangle_group;
    Option.iter (fun name -> if String.trim name = "" then
      invalid_arg (operation ^ ": constraint group name must be non-empty"))
      constraint_group;
    Option.iter (fun name -> if String.trim name = "" then
      invalid_arg (operation ^ ": split point group name must be non-empty"))
      split_point_group;
    Option.iter (fun name -> if String.trim name = "" then
      invalid_arg (operation ^ ": refinement point group name must be non-empty"))
      refinement_point_group;
    Result.bind (selected_points ?cancel ~grain selection geometry) (fun points ->
      if Array.length points = 0 then
        Error (operation ^ ": selection contains no points")
      else begin
        let prefiltered_constraint_points = ignore_non_constraint_points
            && not silhouette_constraints && not remove_outside_silhouette in
        let points = if prefiltered_constraint_points then
            constraint_endpoint_selection ?cancel ~points ?constraint_edges
              ?constraint_primitives geometry
          else points in
        let projected = project ?cancel ~grain projection geometry points in
        let x = projected.x and y = projected.y in
        Result.bind (Delaunay2.build ?cancel ~seed ~x ~y ()) (fun triangulation ->
          let local_of_point = Array.make (Geometry.point_count geometry) (-1) in
          Array.iteri (fun local point -> local_of_point.(point) <- local) points;
          let constraints,constraint_winding = constraint_segments ?cancel ~grain
              ~polygon_winding:remove_outside_constraint_polygons
              ~silhouette:(silhouette_constraints || remove_outside_silhouette)
              ~silhouette_winding:remove_outside_silhouette ~x ~y
              ?constraint_edges
              ?constraint_primitives geometry local_of_point triangulation in
          let prepared = if ignore_non_constraint_points
                && not prefiltered_constraint_points then
              constraint_only_seed ?cancel ~grain ~seed ~points ~x ~y
                ~constraints ~constraint_winding ()
            else Ok (points,x,y,triangulation,constraints,constraint_winding) in
          Result.bind prepared (fun
              (points,x,y,triangulation,constraints,constraint_winding) ->
          let seed_view = Delaunay2.Private.view triangulation in
          let arrangement = if Array.length constraints = 0 then Ok None
            else Result.map Option.some (Planar_constraints.build ?cancel ~grain
                ~split_crossings:split_crossing_constraints ~x ~y
                ~embedded_points:seed_view.unique_source
                ~segment_points:constraints ~segment_winding:constraint_winding
                ()) in
          Result.bind arrangement (fun arrangement ->
          let triangulated = match arrangement with
            | None when not flood_from_hull_boundary
                && not remove_outside_constraint_polygons
                && not remove_outside_silhouette ->
                Ok (seed_view.triangle_points,[||])
            | None -> Result.map (fun value ->
                let view = Planar_cdt.Private.view value in
                view.triangle_points,view.constraint_points)
              (Planar_cdt.build ?cancel ~point_count:(Array.length points)
                ~orient:(fun a b c -> Predicates.orient2d_packed ~x ~y a b c)
                ~incircle:(fun a b c d ->
                  Predicates.incircle_packed ~x ~y a b c d)
                ~triangle_points:seed_view.triangle_points
                ~flood_from_hull_boundary ~constraint_points:[||]
                ~constraint_winding:[||]
                ~remove_outside_constraint_polygons:
                  (remove_outside_constraint_polygons || remove_outside_silhouette)
                ())
            | Some arrangement ->
              let arrangement_view = Planar_constraints.Private.view arrangement in
              Result.map (fun value ->
                let view = Planar_cdt.Private.view value in
                view.triangle_points,view.constraint_points)
              (Planar_cdt.build ?cancel
                ~point_count:(Planar_constraints.point_count arrangement)
                ~orient:(Planar_constraints.Private.orient2d arrangement)
                ~incircle:(Planar_constraints.Private.incircle arrangement)
                ~bounds_overlap:(fun a b u v ->
                  let x = arrangement_view.x and y = arrangement_view.y in
                  max (min x.(a) x.(b)) (min x.(u) x.(v))
                    <= min (max x.(a) x.(b)) (max x.(u) x.(v))
                  && max (min y.(a) y.(b)) (min y.(u) y.(v))
                    <= min (max y.(a) y.(b)) (max y.(u) y.(v)))
                ~triangle_points:seed_view.triangle_points
                ~insert_points:arrangement_view.insert_points
                ~flood_from_hull_boundary
                ~constraint_points:arrangement_view.constraint_points
                ~constraint_winding:arrangement_view.constraint_winding
                ~remove_outside_constraint_polygons:
                  (remove_outside_constraint_polygons || remove_outside_silhouette)
                ()) in
          Result.bind triangulated (fun (initial_triangles,initial_constraints) ->
          let refined = if not refine then
              Ok (initial_triangles,initial_constraints,None)
            else begin
              let initial_points = exact_points x y arrangement in
              let arrangement_new_points = match arrangement with None -> 0
                | Some value -> Planar_constraints.split_point_count value in
              if maximum_new_points < arrangement_new_points then
                invalid_arg (operation ^
                  ": maximum new points is smaller than the required constraint split count");
              let refinement_constraints,constraint_winding =
                match arrangement with
                | None -> initial_constraints,[||]
                | Some arrangement ->
                    let view = Planar_constraints.Private.view arrangement in
                    view.constraint_points,view.constraint_winding in
              Result.map (fun refinement ->
                Planar_refinement.triangle_points refinement,
                Planar_refinement.constraint_points refinement,
                Some refinement)
                (Planar_refinement.build ?cancel ~grain
                  ~initial_points ~initial_triangle_points:initial_triangles
                  ~constraint_points:refinement_constraints ~constraint_winding
                  ~minimum_angle:(Some minimum_angle) ~maximum_area
                  ~target_edge_length ~minimum_edge_length
                  ~maximum_new_points:(maximum_new_points - arrangement_new_points)
                  ~allow_constraint_splitting ~regularization_steps
                  ~allow_movement_of_interior_input_points ())
            end in
          Result.bind refined (fun (triangle_points,constraint_points,refinement) ->
          let materialization = materialization ?cancel ~grain
              ~restore_original_point_positions ~projected_x:x ~projected_y:y
              ~embed:projected.embed ~points ~arrangement ~refinement geometry in
          let positions = materialization.positions
          and local_to_output = materialization.local_to_output in
          let output_point_count = Packed.Float3.length positions in
          let split_count = materialization.split_count
          and refinement_count = materialization.refinement_count in
          let triangle_count = Array.length triangle_points / 3 in
          let source_point_output = if keep_primitives && remove_duplicate_points then
              Array.init (Geometry.point_count geometry) Fun.id else [||] in
          if keep_primitives && remove_duplicate_points then
            for local = 0 to Array.length points - 1 do
              if local land 4095 = 0 then Cancel.check_opt cancel;
              let representative = Delaunay2.unique_source triangulation
                  (Delaunay2.source_unique triangulation local) in
              source_point_output.(points.(local)) <- points.(representative)
            done;
          let topology_plan = output_topology ?cancel ~grain ~keep_primitives
              ~constraint_primitives ~source_point_output ~output_point_count
              ~local_to_output ~triangle_points geometry in
          let topology = topology_plan.topology in
          let attributes = List.filter_map (fun attribute ->
              match Attribute.owner attribute with
              | Attribute.Detail -> Some attribute
              | Attribute.Point ->
                  if not preserve_point_payload
                      || Attribute.name attribute = "N" then None
                  else if split_count + refinement_count = 0 then Some attribute
                  else Some (interpolate_point_attribute ?cancel ~grain
                    materialization attribute)
              | Attribute.Vertex ->
                  if not keep_primitives || Attribute.name attribute = "N" then None
                  else Some (defaulted_attribute ?cancel ~grain
                    topology_plan.vertex_source attribute)
              | Attribute.Primitive ->
                  if not keep_primitives then None
                  else Some (defaulted_attribute ?cancel ~grain
                    topology_plan.primitive_source attribute))
              (Geometry.attributes geometry) in
          let groups = List.filter_map (fun group -> match Group.owner group with
              | Group.Point ->
                  if not preserve_point_payload then None
                  else Some (if split_count + refinement_count = 0
                      then group else Topology_remap.group ?cancel ~grain
                        materialization.representative group)
              | Group.Vertex ->
                  if not keep_primitives then None
                  else Some (defaulted_group ?cancel ~grain
                    topology_plan.vertex_source group)
              | Group.Primitive ->
                  if not keep_primitives then None
                  else Some (defaulted_group ?cancel ~grain
                    topology_plan.primitive_source group))
              (Geometry.groups geometry) in
          let groups = match split_point_group with
            | None -> groups
            | Some name -> replace_group (Group.init ~grain ~owner:Group.Point
                ~name output_point_count
                (fun point -> point >= Geometry.point_count geometry
                  && point < Geometry.point_count geometry + split_count)) groups in
          let groups = match refinement_point_group with
            | None -> groups
            | Some name -> replace_group (Group.init ~grain ~owner:Group.Point
                ~name output_point_count
                (fun point -> point >= output_point_count - refinement_count)) groups in
          let groups = match triangle_group with
            | None -> groups
            | Some name -> replace_group (Group.init ~grain
                ~owner:Group.Primitive ~name
                (topology_plan.kept_primitive_count + triangle_count)
                (fun primitive ->
                  primitive >= topology_plan.kept_primitive_count)) groups in
          let target_index = lazy (Topology_index.create ?cancel topology) in
          let source_edge_groups = if keep_primitives
                && Geometry.edge_groups geometry <> [] then
              remap_source_edge_groups ?cancel ~point_map:source_point_output
                ~target_topology:topology ~target_index:(Lazy.force target_index)
                geometry
            else [] in
          let edge_groups = match constraint_group with
            | None -> source_edge_groups
            | Some name ->
                let index = Lazy.force target_index in
                let builder = Edge_group.Builder.create ~topology ~index ~name in
                for constraint_index = 0 to Array.length constraint_points / 2 - 1 do
                  let a = local_to_output constraint_points.(constraint_index * 2)
                  and b = local_to_output
                      constraint_points.((constraint_index * 2) + 1) in
                  let edge = Topology_index.find_edge_index index ~a ~b in
                  if edge < 0 then
                    invalid_arg (operation ^ ": recovered constraint is absent from output topology");
                  Edge_group.Builder.set builder edge true
                done;
                let constraints = Edge_group.Builder.freeze builder in
                constraints :: List.filter (fun group ->
                  not (String.equal (Edge_group.name group) name)) source_edge_groups in
          Result.bind (Geometry.create ~positions ~topology
              ~attributes ~groups ~edge_groups ()) (fun output ->
            if not remove_duplicate_points then Ok output
            else begin
              let duplicate = Bytes.make output_point_count '\000'
              and duplicate_count = ref 0 in
              for local = 0 to Array.length points - 1 do
                if local land 4095 = 0 then Cancel.check_opt cancel;
                let unique = Delaunay2.source_unique triangulation local in
                if Delaunay2.unique_source triangulation unique <> local then begin
                  let point = points.(local) in
                  if Bytes.unsafe_get duplicate point = '\000' then begin
                    Bytes.unsafe_set duplicate point '\001'; incr duplicate_count
                  end
                end
              done;
              if !duplicate_count = 0 then Ok output
              else
                let group = Group.init ~grain ~owner:Group.Point
                    ~name:"__triangulate_2d_duplicate_points" output_point_count
                    (fun point -> Bytes.unsafe_get duplicate point <> '\000') in
                Error.unguard (Deletion.delete ?cancel ~grain group output)
            end)
          )
          )
          )
          )
          )
      end)
  with
  | Cancel.Cancelled -> Error (operation ^ ": triangulation was cancelled")
  | Invalid_argument message -> Error message
  end finish
