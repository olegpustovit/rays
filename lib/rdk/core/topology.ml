type primitive_kind = Polygon | Open_polyline | Closed_polyline

let kind_code = function Polygon -> 0 | Open_polyline -> 1 | Closed_polyline -> 2
let kind_of_code = function
  | 0 -> Polygon | 1 -> Open_polyline | 2 -> Closed_polyline
  | _ -> invalid_arg "Topology: invalid primitive kind code"

type t = {
  point_count : int;
  vertex_points : int array;
  primitive_offsets : int array;
  primitive_kinds : bytes;
  data_id : int;
}
type topology = t

let validate ~point_count ~vertex_points ~primitive_offsets ~primitive_kinds =
  let rec invalid_vertex last index =
    index < last &&
    (let point = vertex_points.(index) in
     point < 0 || point >= point_count || invalid_vertex last (index + 1)) in
  let invalid_vertices () =
    let count = Array.length vertex_points and block_size = 65536 in
    if count <= block_size then invalid_vertex count 0
    else if invalid_vertex 1 0 then true
    else begin
      (* ponytail: independent blocks finish after a later fault; use a shared
         stop flag if malformed large meshes need lower rejection latency. *)
      let blocks = ((count - 1) / block_size) + 1 in
      let invalid = Array.make blocks false in
      Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(blocks - 1)
        (fun block ->
          let first = block * block_size in
          let last = first + min block_size (count - first) in
          invalid.(block) <- invalid_vertex last first);
      Array.exists Fun.id invalid
    end in
  if point_count < 0 then Error "Topology: point count is negative"
  else if Array.length primitive_offsets = 0 then
    Error "Topology: primitive offsets must contain the initial zero"
  else if primitive_offsets.(0) <> 0 then
    Error "Topology: first primitive offset must be zero"
  else if primitive_offsets.(Array.length primitive_offsets - 1)
          <> Array.length vertex_points then
    Error "Topology: final primitive offset must equal vertex count"
  else if Bytes.length primitive_kinds <> Array.length primitive_offsets - 1 then
    Error "Topology: primitive kind count must match primitive count"
  else if invalid_vertices () then Error "Topology: vertex references an invalid point"
  else begin
    let ordered = ref true in
    for index = 1 to Array.length primitive_offsets - 1 do
      if primitive_offsets.(index) < primitive_offsets.(index - 1) then
        ordered := false
    done;
    if not !ordered then Error "Topology: primitive offsets are not ordered"
    else
      let valid = ref true in
      Bytes.iteri (fun primitive code ->
        let size = primitive_offsets.(primitive + 1) - primitive_offsets.(primitive) in
        match kind_of_code (Char.code code) with
        | Polygon | Closed_polyline -> if size < 3 then valid := false
        | Open_polyline -> if size < 2 then valid := false) primitive_kinds;
      if !valid then Ok () else Error "Topology: primitive has too few vertices for its kind"
  end

let create_owned ~point_count ~vertex_points ~primitive_offsets ~primitive_kinds =
  let primitive_kinds = Bytes.init (Array.length primitive_kinds)
      (fun index -> Char.chr (kind_code primitive_kinds.(index))) in
  Result.map
    (fun () -> { point_count; vertex_points; primitive_offsets; primitive_kinds;
                 data_id = Data_id.fresh () })
    (validate ~point_count ~vertex_points ~primitive_offsets ~primitive_kinds)

let polygons_owned ~point_count ~vertex_points ~primitive_offsets =
  let primitive_kinds = Bytes.make (Array.length primitive_offsets - 1) '\000' in
  Result.map
    (fun () -> { point_count; vertex_points; primitive_offsets; primitive_kinds;
                 data_id = Data_id.fresh () })
    (validate ~point_count ~vertex_points ~primitive_offsets ~primitive_kinds)

let empty ~point_count =
  if point_count < 0 then invalid_arg "Topology.empty: negative point count";
  { point_count; vertex_points = [||]; primitive_offsets = [|0|]; primitive_kinds = Bytes.empty;
    data_id = Data_id.fresh () }
let point_count value = value.point_count
let vertex_count value = Array.length value.vertex_points
let primitive_count value = Array.length value.primitive_offsets - 1
let data_id value = value.data_id
let payload_bytes value =
  ((Array.length value.vertex_points + Array.length value.primitive_offsets)
   * (Sys.word_size / 8)) + Bytes.length value.primitive_kinds
let point_of_vertex value vertex = value.vertex_points.(vertex)
let primitive_vertex_range value primitive =
  value.primitive_offsets.(primitive), value.primitive_offsets.(primitive + 1)
let primitive_size value primitive =
  value.primitive_offsets.(primitive + 1) - value.primitive_offsets.(primitive)
let primitive_kind value primitive =
  kind_of_code (Char.code (Bytes.get value.primitive_kinds primitive))
let iter_primitive_vertices value primitive operation =
  let first, last = primitive_vertex_range value primitive in
  for vertex = first to last - 1 do
    operation vertex value.vertex_points.(vertex)
  done
let all_triangles value =
  let result = ref true in
  for primitive = 0 to primitive_count value - 1 do
    if Char.code (Bytes.get value.primitive_kinds primitive) <> 0
       || value.primitive_offsets.(primitive + 1)
          - value.primitive_offsets.(primitive) <> 3
    then result := false
  done;
  !result

module Builder = struct
  type nonrec t = {
    point_count : int;
    mutable vertex_points : int array;
    mutable primitive_offsets : int array;
    mutable primitive_kinds : bytes;
    mutable vertex_count : int;
    mutable primitive_count : int;
    mutable frozen : bool;
  }
  let create ?(vertex_capacity = 0) ?(primitive_capacity = 0) ~point_count () =
    if point_count < 0 || vertex_capacity < 0 || primitive_capacity < 0 then
      invalid_arg "Topology.Builder.create: negative count";
    { point_count; vertex_points = Array.make vertex_capacity 0;
      primitive_offsets = Array.make (primitive_capacity + 1) 0;
      primitive_kinds = Bytes.make primitive_capacity '\000';
      vertex_count = 0; primitive_count = 0; frozen = false }
  let ensure_vertices value required =
    if required > Array.length value.vertex_points then begin
      let capacity = max required (max 8 (Array.length value.vertex_points * 2)) in
      let next = Array.make capacity 0 in
      Array.blit value.vertex_points 0 next 0 value.vertex_count;
      value.vertex_points <- next
    end
  let ensure_primitives value required =
    if required + 1 > Array.length value.primitive_offsets then begin
      let capacity = max (required + 1)
          (max 8 (Array.length value.primitive_offsets * 2)) in
      let next = Array.make capacity 0 in
      Array.blit value.primitive_offsets 0 next 0 (value.primitive_count + 1);
      value.primitive_offsets <- next;
      let kinds = Bytes.make (capacity - 1) '\000' in
      Bytes.blit value.primitive_kinds 0 kinds 0 value.primitive_count;
      value.primitive_kinds <- kinds
    end
  let require_open value =
    if value.frozen then invalid_arg "Topology.Builder: builder is frozen"
  let add_primitive value kind minimum points =
    require_open value;
    if Array.length points < minimum then
      invalid_arg "Topology.Builder: primitive has too few vertices";
    Array.iter (fun point ->
      if point < 0 || point >= value.point_count then
        invalid_arg "Topology.Builder.add_polygon: invalid point") points;
    let added = Array.length points in
    ensure_vertices value (value.vertex_count + added);
    ensure_primitives value (value.primitive_count + 1);
    Array.blit points 0 value.vertex_points value.vertex_count added;
    value.vertex_count <- value.vertex_count + added;
    value.primitive_count <- value.primitive_count + 1;
    value.primitive_offsets.(value.primitive_count) <- value.vertex_count;
    Bytes.set value.primitive_kinds (value.primitive_count - 1)
      (Char.chr (kind_code kind))
  let add_open_polyline value points = add_primitive value Open_polyline 2 points
  let add_closed_polyline value points = add_primitive value Closed_polyline 3 points
  let freeze value =
    require_open value;
    value.frozen <- true;
    let take array count =
      if Array.length array = count then array else Array.sub array 0 count in
    let take_bytes values count =
      if Bytes.length values = count then values else Bytes.sub values 0 count in
    { point_count = value.point_count;
      vertex_points = take value.vertex_points value.vertex_count;
      primitive_offsets = take value.primitive_offsets (value.primitive_count + 1);
      primitive_kinds = take_bytes value.primitive_kinds value.primitive_count;
      data_id = Data_id.fresh () }
end

module Private = struct
  type view = {
    point_count : int;
    vertex_points : int array;
    primitive_offsets : int array;
    primitive_kinds : bytes;
  }
  let view (value : topology) = {
    point_count = value.point_count;
    vertex_points = value.vertex_points;
    primitive_offsets = value.primitive_offsets;
    primitive_kinds = value.primitive_kinds;
  }
  let polygons_shared = polygons_owned
  let create_validated_owned ~point_count ~vertex_points ~primitive_offsets
      ~primitive_kinds =
    { point_count; vertex_points; primitive_offsets; primitive_kinds;
      data_id = Data_id.fresh () }
  let extend_free_points ~point_count (value : topology) =
    if point_count < value.point_count then invalid_arg
        "Topology.Private.extend_free_points: point count cannot shrink";
    if point_count = value.point_count then value
    else { value with point_count; data_id = Data_id.fresh () }
end
