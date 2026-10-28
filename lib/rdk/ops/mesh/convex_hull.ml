let operation = "Rdk_mesh.Convex_hull.convex_hull"

type positions = Packed.Float3.View.view
type int_buffer = { mutable values : int array; mutable length : int }

let buffer ?(capacity = 0) () = { values = Array.make (max 0 capacity) 0; length = 0 }
let buffer_clear value = value.length <- 0

let buffer_add value element =
  if value.length = Array.length value.values then begin
    let next = if value.length = 0 then 8 else value.length * 2 in
    if next < value.length || next > Sys.max_array_length then
      invalid_arg (operation ^ ": work buffer exceeds array limits");
    let values = Array.make next 0 in
    Array.blit value.values 0 values 0 value.length;
    value.values <- values
  end;
  value.values.(value.length) <- element;
  value.length <- value.length + 1

type int_queue = {
  mutable queue_values : int array;
  mutable queue_first : int;
  mutable queue_length : int;
}

let queue () = { queue_values = Array.make 16 0; queue_first = 0; queue_length = 0 }

let queue_add queue element =
  if queue.queue_length = Array.length queue.queue_values then begin
    let old = queue.queue_values and old_length = Array.length queue.queue_values in
    let next = old_length * 2 in
    if next < old_length || next > Sys.max_array_length then
      invalid_arg (operation ^ ": face queue exceeds array limits");
    let values = Array.make next 0 in
    for index = 0 to queue.queue_length - 1 do
      values.(index) <- old.((queue.queue_first + index) mod old_length)
    done;
    queue.queue_values <- values;
    queue.queue_first <- 0
  end;
  let at = (queue.queue_first + queue.queue_length) mod Array.length queue.queue_values in
  queue.queue_values.(at) <- element;
  queue.queue_length <- queue.queue_length + 1

let queue_take queue =
  if queue.queue_length = 0 then None
  else begin
    let value = queue.queue_values.(queue.queue_first) in
    queue.queue_first <- (queue.queue_first + 1) mod Array.length queue.queue_values;
    queue.queue_length <- queue.queue_length - 1;
    Some value
  end

type faces = {
  mutable face_a : int array;
  mutable face_b : int array;
  mutable face_c : int array;
  mutable neighbor_0 : int array;
  mutable neighbor_1 : int array;
  mutable neighbor_2 : int array;
  mutable alive : bytes;
  mutable mark : int array;
  mutable outside : int_buffer array;
  mutable face_count : int;
}

let faces () =
  {
    face_a = Array.make 16 0;
    face_b = Array.make 16 0;
    face_c = Array.make 16 0;
    neighbor_0 = Array.make 16 (-1);
    neighbor_1 = Array.make 16 (-1);
    neighbor_2 = Array.make 16 (-1);
    alive = Bytes.make 16 '\000';
    mark = Array.make 16 0;
    outside = Array.init 16 (fun _ -> buffer ());
    face_count = 0;
  }

let grow_faces faces =
  let old = Array.length faces.face_a in
  let next = old * 2 in
  if next < old || next > Sys.max_array_length then
    invalid_arg (operation ^ ": hull face storage exceeds array limits");
  let ints source fill =
    let target = Array.make next fill in
    Array.blit source 0 target 0 old;
    target
  in
  faces.face_a <- ints faces.face_a 0;
  faces.face_b <- ints faces.face_b 0;
  faces.face_c <- ints faces.face_c 0;
  faces.neighbor_0 <- ints faces.neighbor_0 (-1);
  faces.neighbor_1 <- ints faces.neighbor_1 (-1);
  faces.neighbor_2 <- ints faces.neighbor_2 (-1);
  let alive = Bytes.make next '\000' in
  Bytes.blit faces.alive 0 alive 0 old;
  faces.alive <- alive;
  faces.mark <- ints faces.mark 0;
  let outside =
    Array.init next (fun index -> if index < old then faces.outside.(index) else buffer ())
  in
  faces.outside <- outside

let add_face faces a b c =
  if faces.face_count = Array.length faces.face_a then grow_faces faces;
  let face = faces.face_count in
  faces.face_count <- face + 1;
  faces.face_a.(face) <- a;
  faces.face_b.(face) <- b;
  faces.face_c.(face) <- c;
  faces.neighbor_0.(face) <- -1;
  faces.neighbor_1.(face) <- -1;
  faces.neighbor_2.(face) <- -1;
  Bytes.unsafe_set faces.alive face '\001';
  faces.mark.(face) <- 0;
  buffer_clear faces.outside.(face);
  face

let face_alive faces face = Bytes.unsafe_get faces.alive face <> '\000'

let face_edge faces face = function
  | 0 -> (faces.face_a.(face), faces.face_b.(face))
  | 1 -> (faces.face_b.(face), faces.face_c.(face))
  | 2 -> (faces.face_c.(face), faces.face_a.(face))
  | _ -> invalid_arg (operation ^ ": invalid face edge")

let face_neighbor faces face = function
  | 0 -> faces.neighbor_0.(face)
  | 1 -> faces.neighbor_1.(face)
  | 2 -> faces.neighbor_2.(face)
  | _ -> invalid_arg (operation ^ ": invalid face edge")

let set_face_neighbor faces face edge neighbor =
  match edge with
  | 0 -> faces.neighbor_0.(face) <- neighbor
  | 1 -> faces.neighbor_1.(face) <- neighbor
  | 2 -> faces.neighbor_2.(face) <- neighbor
  | _ -> invalid_arg (operation ^ ": invalid face edge")

let neighbor_slot faces face neighbor =
  if faces.neighbor_0.(face) = neighbor then 0
  else if faces.neighbor_1.(face) = neighbor then 1
  else if faces.neighbor_2.(face) = neighbor then 2
  else invalid_arg (operation ^ ": asymmetric hull adjacency")

let[@inline always] float_hash value =
  if value = 0. then 0
  else
    let bits = Int64.to_int (Int64.bits_of_float value) in
    if value < 0. then bits lxor min_int else bits

let[@inline always] hash_position x y z point mask =
  let hash = 0x9e3779b9 lxor float_hash x.(point) in
  let hash = hash lxor (hash lsr 16) * 0x45d9f3b in
  let hash = hash lxor float_hash y.(point) in
  let hash = hash lxor (hash lsr 16) * 0x45d9f3b in
  let hash = hash lxor float_hash z.(point) in
  hash lxor (hash lsr 16) * 0x45d9f3b land mask

let next_table_size count =
  if count > Sys.max_array_length / 2 then
    invalid_arg (operation ^ ": selected point set exceeds hash-table limits");
  let wanted = max 16 (count * 2) in
  let size = ref 16 in
  while !size < wanted do
    if !size > Sys.max_array_length / 2 then
      invalid_arg (operation ^ ": selected point set exceeds hash-table limits");
    size := !size * 2
  done;
  !size

let selected_unique_points ?cancel ~grain selection geometry =
  let point_count = Geometry.point_count geometry
  and positions = Packed.Float3.View.view (Geometry.positions geometry) in
  let selected point = match selection with None -> true | Some group -> Group.mem point group in
  let selected_count = ref 0 in
  for point = 0 to point_count - 1 do
    Cancel.poll cancel point 4095;
    if selected point then incr selected_count
  done;
  if !selected_count = 0 then Error (operation ^ ": selection contains no points")
  else begin
    let first_invalid = Atomic.make max_int in
    let validate point =
      Cancel.poll cancel point 4095;
      if
        selected point
        && not
             (Float.is_finite positions.x.(point)
             && Float.is_finite positions.y.(point)
             && Float.is_finite positions.z.(point))
      then begin
        let rec set () =
          let current = Atomic.get first_invalid in
          if point < current && not (Atomic.compare_and_set first_invalid current point) then set ()
        in
        set ()
      end
    in
    if point_count > grain then
      Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(point_count - 1) validate
    else
      for point = 0 to point_count - 1 do
        validate point
      done;
    let invalid = Atomic.get first_invalid in
    if invalid <> max_int then
      Error (Printf.sprintf "%s: selected point %d has a non-finite position" operation invalid)
    else begin
      let table_size = next_table_size !selected_count in
      let table = Array.make table_size 0 and mask = table_size - 1 in
      let unique = Array.make !selected_count 0 and count = ref 0 in
      for point = 0 to point_count - 1 do
        Cancel.poll cancel point 4095;
        if selected point then begin
          let slot = ref (hash_position positions.x positions.y positions.z point mask)
          and found = ref false in
          while table.(!slot) <> 0 && not !found do
            let previous = table.(!slot) - 1 in
            if
              positions.x.(previous) = positions.x.(point)
              && positions.y.(previous) = positions.y.(point)
              && positions.z.(previous) = positions.z.(point)
            then found := true
            else slot := (!slot + 1) land mask
          done;
          if not !found then begin
            table.(!slot) <- point + 1;
            unique.(!count) <- point;
            incr count
          end
        end
      done;
      Ok (Array.sub unique 0 !count)
    end
  end

let sign_nonzero = function
  | Predicates.Zero -> false
  | Predicates.Negative | Predicates.Positive -> true

let non_collinear (positions : positions) a b c =
  sign_nonzero (Predicates.orient2d_packed ~x:positions.x ~y:positions.y a b c)
  || sign_nonzero (Predicates.orient2d_packed ~x:positions.x ~y:positions.z a b c)
  || sign_nonzero (Predicates.orient2d_packed ~x:positions.y ~y:positions.z a b c)

type projection = XY | XZ | YZ

let projection (positions : positions) a b c =
  let determinant u v =
    let au = u.(a) and av = v.(a) in
    let ab_u = u.(b) -. au and ab_v = v.(b) -. av and ac_u = u.(c) -. au and ac_v = v.(c) -. av in
    let scale =
      max (abs_float ab_u) (max (abs_float ab_v) (max (abs_float ac_u) (abs_float ac_v)))
    in
    if scale = 0. || not (Float.is_finite scale) then 0.
    else abs_float ((ab_u /. scale *. (ac_v /. scale)) -. (ab_v /. scale *. (ac_u /. scale)))
  in
  let candidates =
    [|
      (XY, determinant positions.x positions.y);
      (XZ, determinant positions.x positions.z);
      (YZ, determinant positions.y positions.z);
    |]
  in
  let best = ref 0 in
  for index = 1 to 2 do
    if snd candidates.(index) > snd candidates.(!best) then best := index
  done;
  fst candidates.(!best)

let projection_planes (positions : positions) = function
  | XY -> (positions.x, positions.y)
  | XZ -> (positions.x, positions.z)
  | YZ -> (positions.y, positions.z)

let planar_hull (positions : positions) unique a b c =
  let u, v = projection_planes positions (projection positions a b c) in
  let points = Array.copy unique in
  Array.sort
    (fun left right ->
      let by_u = Float.compare u.(left) u.(right) in
      if by_u <> 0 then by_u
      else
        let by_v = Float.compare v.(left) v.(right) in
        if by_v <> 0 then by_v else Int.compare left right)
    points;
  let build source =
    let result = Array.make (Array.length source) 0 and length = ref 0 in
    Array.iter
      (fun point ->
        while
          !length >= 2
          && Predicates.orient2d_packed ~x:u ~y:v result.(!length - 2) result.(!length - 1) point
             <> Predicates.Positive
        do
          decr length
        done;
        result.(!length) <- point;
        incr length)
      source;
    (result, !length)
  in
  let lower, lower_length = build points in
  let reversed =
    Array.init (Array.length points) (fun index -> points.(Array.length points - 1 - index))
  in
  let upper, upper_length = build reversed in
  let count = lower_length + upper_length - 2 in
  let hull = Array.make count 0 in
  Array.blit lower 0 hull 0 (lower_length - 1);
  Array.blit upper 0 hull (lower_length - 1) (upper_length - 1);
  hull

let lexicographic (positions : positions) left right =
  let compare = Float.compare positions.x.(left) positions.x.(right) in
  if compare <> 0 then compare
  else
    let compare = Float.compare positions.y.(left) positions.y.(right) in
    if compare <> 0 then compare
    else
      let compare = Float.compare positions.z.(left) positions.z.(right) in
      if compare <> 0 then compare else Int.compare left right

let line_endpoints (positions : positions) unique =
  let minimum = ref unique.(0) and maximum = ref unique.(0) in
  Array.iter
    (fun point ->
      if lexicographic positions point !minimum < 0 then minimum := point;
      if lexicographic positions point !maximum > 0 then maximum := point)
    unique;
  [| !minimum; !maximum |]

let[@inline always] visible (positions : positions) faces face point =
  Predicates.orient3d_packed ~x:positions.x ~y:positions.y ~z:positions.z faces.face_a.(face)
    faces.face_b.(face) faces.face_c.(face) point
  = Predicates.Negative

let[@inline always] face_measure (positions : positions) faces face point =
  let a = faces.face_a.(face) and b = faces.face_b.(face) and c = faces.face_c.(face) in
  let ax = positions.x.(a) -. positions.x.(point)
  and ay = positions.y.(a) -. positions.y.(point)
  and az = positions.z.(a) -. positions.z.(point)
  and bx = positions.x.(b) -. positions.x.(point)
  and by = positions.y.(b) -. positions.y.(point)
  and bz = positions.z.(b) -. positions.z.(point)
  and cx = positions.x.(c) -. positions.x.(point)
  and cy = positions.y.(c) -. positions.y.(point)
  and cz = positions.z.(c) -. positions.z.(point) in
  let scale_ab = max (max (abs_float ax) (abs_float ay)) (max (abs_float az) (abs_float bx))
  and scale_bc = max (max (abs_float by) (abs_float bz)) (max (abs_float cx) (abs_float cy)) in
  let scale = max (max scale_ab scale_bc) (abs_float cz) in
  if scale = 0. || not (Float.is_finite scale) then 0.
  else begin
    let ax = ax /. scale
    and ay = ay /. scale
    and az = az /. scale
    and bx = bx /. scale
    and by = by /. scale
    and bz = bz /. scale
    and cx = cx /. scale
    and cy = cy /. scale
    and cz = cz /. scale in
    abs_float
      ((ax *. ((by *. cz) -. (bz *. cy)))
      -. (ay *. ((bx *. cz) -. (bz *. cx)))
      +. (az *. ((bx *. cy) -. (by *. cx))))
  end

let connect_initial faces =
  for left = 0 to 3 do
    for left_edge = 0 to 2 do
      if face_neighbor faces left left_edge < 0 then begin
        let a, b = face_edge faces left left_edge in
        let found = ref false in
        for right = left + 1 to 3 do
          for right_edge = 0 to 2 do
            let c, d = face_edge faces right right_edge in
            if a = d && b = c then begin
              set_face_neighbor faces left left_edge right;
              set_face_neighbor faces right right_edge left;
              found := true
            end
          done
        done;
        if (not !found) && face_neighbor faces left left_edge < 0 then
          invalid_arg (operation ^ ": initial simplex adjacency is incomplete")
      end
    done
  done

let assign_points ?cancel ~grain ?point_count (positions : positions) faces face_ids points queue =
  let point_count = Option.value ~default:(Array.length points) point_count in
  if point_count < 0 || point_count > Array.length points then
    invalid_arg (operation ^ ": invalid conflict-point prefix");
  let assignments = Array.make point_count (-1) in
  let rec first_visible point candidate_face =
    if candidate_face = Array.length face_ids then -1
    else
      let face = face_ids.(candidate_face) in
      if visible positions faces face point then face else first_visible point (candidate_face + 1)
  in
  let assign index =
    Cancel.poll cancel index 4095;
    assignments.(index) <- first_visible points.(index) 0
  in
  if point_count > grain then
    Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(point_count - 1) assign
  else
    for index = 0 to point_count - 1 do
      assign index
    done;
  for index = 0 to point_count - 1 do
    let face = assignments.(index) in
    if face >= 0 then buffer_add faces.outside.(face) points.(index)
  done;
  Array.iter (fun face -> if faces.outside.(face).length > 0 then queue_add queue face) face_ids

let oriented_face (positions : positions) faces a b c opposite =
  match Predicates.orient3d_packed ~x:positions.x ~y:positions.y ~z:positions.z a b c opposite with
  | Predicates.Positive -> add_face faces a b c
  | Predicates.Negative -> add_face faces b a c
  | Predicates.Zero -> invalid_arg (operation ^ ": degenerate initial simplex")

let farthest_outside (positions : positions) faces face =
  let outside = faces.outside.(face) in
  if outside.length = 0 then invalid_arg (operation ^ ": empty outside set");
  let chosen = ref outside.values.(0)
  and measure = ref (face_measure positions faces face outside.values.(0)) in
  for index = 1 to outside.length - 1 do
    let point = outside.values.(index)
    and candidate = face_measure positions faces face outside.values.(index) in
    if candidate > !measure || (candidate = !measure && point < !chosen) then begin
      chosen := point;
      measure := candidate
    end
  done;
  !chosen

let expand_hull ?cancel ~grain (positions : positions) faces queue radial generation seed_face eye =
  let visible_faces = buffer () and stack = buffer () in
  buffer_add stack seed_face;
  faces.mark.(seed_face) <- generation;
  while stack.length > 0 do
    Cancel.check_opt cancel;
    stack.length <- stack.length - 1;
    let face = stack.values.(stack.length) in
    buffer_add visible_faces face;
    for edge = 0 to 2 do
      let neighbor = face_neighbor faces face edge in
      if neighbor < 0 then invalid_arg (operation ^ ": open 3D hull adjacency")
      else if
        face_alive faces neighbor
        && faces.mark.(neighbor) <> generation
        && visible positions faces neighbor eye
      then begin
        faces.mark.(neighbor) <- generation;
        buffer_add stack neighbor
      end
    done
  done;
  let horizon_a = buffer ()
  and horizon_b = buffer ()
  and horizon_neighbor = buffer ()
  and horizon_visible = buffer ()
  and reassigned = buffer () in
  for index = 0 to visible_faces.length - 1 do
    let face = visible_faces.values.(index) in
    for edge = 0 to 2 do
      let neighbor = face_neighbor faces face edge in
      if faces.mark.(neighbor) <> generation then begin
        let a, b = face_edge faces face edge in
        buffer_add horizon_a a;
        buffer_add horizon_b b;
        buffer_add horizon_neighbor neighbor;
        buffer_add horizon_visible face
      end
    done;
    let outside = faces.outside.(face) in
    for at = 0 to outside.length - 1 do
      let point = outside.values.(at) in
      if point <> eye then buffer_add reassigned point
    done;
    outside.values <- [||];
    buffer_clear outside;
    Bytes.unsafe_set faces.alive face '\000'
  done;
  let horizon_count = horizon_a.length in
  if horizon_count < 3 then
    invalid_arg (operation ^ ": a hull expansion has fewer than three horizon edges");
  let order = Array.init horizon_count Fun.id in
  Array.sort
    (fun left right ->
      let compare = Int.compare horizon_a.values.(left) horizon_a.values.(right) in
      if compare <> 0 then compare
      else
        let compare = Int.compare horizon_b.values.(left) horizon_b.values.(right) in
        if compare <> 0 then compare
        else
          let compare =
            Int.compare horizon_neighbor.values.(left) horizon_neighbor.values.(right)
          in
          if compare <> 0 then compare
          else Int.compare horizon_visible.values.(left) horizon_visible.values.(right))
    order;
  let new_faces = Array.make horizon_count 0 in
  for output = 0 to horizon_count - 1 do
    let horizon = order.(output) and neighbor = horizon_neighbor.values.(order.(output)) in
    let face = add_face faces horizon_a.values.(horizon) horizon_b.values.(horizon) eye in
    set_face_neighbor faces face 0 neighbor;
    let slot = neighbor_slot faces neighbor horizon_visible.values.(horizon) in
    set_face_neighbor faces neighbor slot face;
    new_faces.(output) <- face
  done;
  Hashtbl.clear radial;
  Array.iter
    (fun face ->
      for edge = 1 to 2 do
        let a, b = face_edge faces face edge in
        match Hashtbl.find_opt radial (b, a) with
        | Some (other, other_edge) ->
            set_face_neighbor faces face edge other;
            set_face_neighbor faces other other_edge face;
            Hashtbl.remove radial (b, a)
        | None -> Hashtbl.add radial (a, b) (face, edge)
      done)
    new_faces;
  if Hashtbl.length radial <> 0 then invalid_arg (operation ^ ": horizon cycle did not close");
  assign_points ?cancel ~grain ~point_count:reassigned.length positions faces new_faces
    reassigned.values queue

let three_dimensional_hull ?cancel ~grain (positions : positions) unique a b c d =
  let faces = faces () in
  ignore (oriented_face positions faces a b c d);
  ignore (oriented_face positions faces a d b c);
  ignore (oriented_face positions faces a c d b);
  ignore (oriented_face positions faces b d c a);
  connect_initial faces;
  let simplex point = point = a || point = b || point = c || point = d in
  let candidates = Array.make (Array.length unique - 4) 0 and count = ref 0 in
  Array.iter
    (fun point ->
      if not (simplex point) then begin
        candidates.(!count) <- point;
        incr count
      end)
    unique;
  if !count <> Array.length candidates then
    invalid_arg (operation ^ ": initial simplex point accounting failed");
  let queue = queue () and radial = Hashtbl.create 64 in
  assign_points ?cancel ~grain positions faces [| 0; 1; 2; 3 |] candidates queue;
  let generation = ref 0 and continue = ref true in
  while !continue do
    match queue_take queue with
    | None -> continue := false
    | Some face when (not (face_alive faces face)) || faces.outside.(face).length = 0 -> ()
    | Some face ->
        incr generation;
        if !generation = max_int then invalid_arg (operation ^ ": hull expansion counter overflowed");
        let eye = farthest_outside positions faces face in
        expand_hull ?cancel ~grain positions faces queue radial !generation face eye
  done;
  let output = buffer () in
  for face = 0 to faces.face_count - 1 do
    if face_alive faces face then buffer_add output face
  done;
  if output.length < 4 then
    invalid_arg (operation ^ ": full-dimensional hull has fewer than four faces");
  (faces, Array.sub output.values 0 output.length)

let remap_output ?cancel ~grain ~preserve_point_payload ?source_point_attribute ?hull_group geometry
    point_map topology =
  let source_positions = Packed.Float3.View.view (Geometry.positions geometry) in
  let select source =
    let output = Array.make (Array.length point_map) 0. in
    if Array.length point_map > 0 then
      Parallel.for_ ~chunk_size:grain ~start:0
        ~finish:(Array.length point_map - 1)
        (fun point ->
          Cancel.poll cancel point 4095;
          output.(point) <- source.(point_map.(point)));
    output
  in
  let positions =
    Packed.Float3.View.of_owned_exn ~x:(select source_positions.x) ~y:(select source_positions.y)
      ~z:(select source_positions.z)
  in
  let attributes =
    List.filter_map
      (fun attribute ->
        match Attribute.owner attribute with
        | Attribute.Detail -> Some attribute
        | Attribute.Point when preserve_point_payload && Attribute.name attribute <> "N" ->
            Some (Topology_remap.attribute ?cancel ~grain point_map attribute)
        | Attribute.Point | Attribute.Vertex | Attribute.Primitive -> None)
      (Geometry.attributes geometry)
  in
  let groups =
    if not preserve_point_payload then []
    else
      List.filter_map
        (fun group ->
          match Group.owner group with
          | Group.Point -> Some (Topology_remap.group ?cancel ~grain point_map group)
          | Group.Vertex | Group.Primitive -> None)
        (Geometry.groups geometry)
  in
  Result.bind (Geometry.create ~positions ~topology ~attributes ~groups ()) (fun output ->
      let output =
        match source_point_attribute with
        | None -> output
        | Some name ->
            let attribute =
              Attribute.create_owned ~name ~owner:Attribute.Point
                (Attribute.Int (Array.copy point_map))
              |> Result.get_ok
            in
            Geometry.with_attribute attribute output |> Result.get_ok
      in
      let output =
        match hull_group with
        | None -> output
        | Some name ->
            let group =
              Group.init ~grain ~owner:Group.Primitive ~name (Geometry.primitive_count output)
                (fun _ -> true)
            in
            Geometry.with_group group output |> Result.get_ok
      in
      Ok output)

let materialize ?cancel ~grain ~preserve_point_payload ?source_point_attribute ?hull_group geometry
    = function
  | `Point source ->
      let topology = Topology.empty ~point_count:1 in
      remap_output ?cancel ~grain ~preserve_point_payload ?source_point_attribute ?hull_group
        geometry [| source |] topology
  | `Line point_map ->
      let topology =
        Topology.create_owned ~point_count:2 ~vertex_points:[| 0; 1 |] ~primitive_offsets:[| 0; 2 |]
          ~primitive_kinds:[| Topology.Open_polyline |]
        |> Result.get_ok
      in
      remap_output ?cancel ~grain ~preserve_point_payload ?source_point_attribute ?hull_group
        geometry point_map topology
  | `Plane point_map ->
      let count = Array.length point_map in
      let topology =
        Topology.polygons_owned ~point_count:count ~vertex_points:(Array.init count Fun.id)
          ~primitive_offsets:[| 0; count |]
        |> Result.get_ok
      in
      remap_output ?cancel ~grain ~preserve_point_payload ?source_point_attribute ?hull_group
        geometry point_map topology
  | `Solid (faces, alive_faces) ->
      let source_point_count = Geometry.point_count geometry in
      let used = Bytes.make source_point_count '\000' in
      Array.iter
        (fun face ->
          Bytes.unsafe_set used faces.face_a.(face) '\001';
          Bytes.unsafe_set used faces.face_b.(face) '\001';
          Bytes.unsafe_set used faces.face_c.(face) '\001')
        alive_faces;
      let output_points = ref 0 in
      for point = 0 to source_point_count - 1 do
        if Bytes.unsafe_get used point <> '\000' then incr output_points
      done;
      let point_map = Array.make !output_points 0
      and source_to_output = Array.make source_point_count (-1) in
      let at = ref 0 in
      for point = 0 to source_point_count - 1 do
        if Bytes.unsafe_get used point <> '\000' then begin
          point_map.(!at) <- point;
          source_to_output.(point) <- !at;
          incr at
        end
      done;
      let primitive_count = Array.length alive_faces in
      let triangle_a = Array.make primitive_count 0
      and triangle_b = Array.make primitive_count 0
      and triangle_c = Array.make primitive_count 0 in
      Array.iteri
        (fun primitive face ->
          let a = source_to_output.(faces.face_a.(face))
          and b = source_to_output.(faces.face_b.(face))
          and c = source_to_output.(faces.face_c.(face)) in
          if a <= b && a <= c then begin
            triangle_a.(primitive) <- a;
            triangle_b.(primitive) <- b;
            triangle_c.(primitive) <- c
          end
          else if b <= a && b <= c then begin
            triangle_a.(primitive) <- b;
            triangle_b.(primitive) <- c;
            triangle_c.(primitive) <- a
          end
          else begin
            triangle_a.(primitive) <- c;
            triangle_b.(primitive) <- a;
            triangle_c.(primitive) <- b
          end)
        alive_faces;
      let triangle_order = Array.init primitive_count Fun.id in
      Array.sort
        (fun left right ->
          let compare = Int.compare triangle_a.(left) triangle_a.(right) in
          if compare <> 0 then compare
          else
            let compare = Int.compare triangle_b.(left) triangle_b.(right) in
            if compare <> 0 then compare else Int.compare triangle_c.(left) triangle_c.(right))
        triangle_order;
      let vertex_points = Array.make (primitive_count * 3) 0
      and primitive_offsets = Array.make (primitive_count + 1) 0
      and primitive_kinds = Bytes.make primitive_count '\000' in
      let fill primitive =
        Cancel.poll cancel primitive 4095;
        let source = triangle_order.(primitive) and vertex = primitive * 3 in
        let a = triangle_a.(source) and b = triangle_b.(source) and c = triangle_c.(source) in
        vertex_points.(vertex) <- a;
        vertex_points.(vertex + 1) <- b;
        vertex_points.(vertex + 2) <- c;
        primitive_offsets.(primitive) <- vertex
      in
      if primitive_count > grain then
        Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(primitive_count - 1) fill
      else
        for primitive = 0 to primitive_count - 1 do
          fill primitive
        done;
      primitive_offsets.(primitive_count) <- primitive_count * 3;
      let topology =
        Topology.View.create_validated_owned ~point_count:!output_points ~vertex_points
          ~primitive_offsets ~primitive_kinds
      in
      remap_output ?cancel ~grain ~preserve_point_payload ?source_point_attribute ?hull_group
        geometry point_map topology

type params = {
  cancel : Cancel.t option;
  grain : int option;
  selection : Element_selection.t option;
  preserve_point_payload : bool;
  source_point_attribute : string option;
  hull_group : string option;
}

let run
    ({ cancel; grain; selection; preserve_point_payload; source_point_attribute; hull_group } :
      params) geometry =
  let grain = Option.value ~default:16_384 grain in
  Error.guard ~operation:"convex_hull" ~code:"invalid_geometry" @@ fun () ->
  try
    if grain <= 0 then invalid_arg (operation ^ ": grain must be positive");
    (match source_point_attribute with
    | Some name when String.trim name = "" || name = "P" ->
        invalid_arg (operation ^ ": source point attribute name must be non-empty and not P")
    | None | Some _ -> ());
    (match hull_group with
    | Some name when String.trim name = "" ->
        invalid_arg (operation ^ ": hull group name must be non-empty")
    | None | Some _ -> ());
    let topology = Geometry.topology geometry in
    Result.bind (Element_selection.validate ~operation topology selection) (fun () ->
        let selected =
          match selection with
          | None -> Ok None
          | Some selection ->
              Result.map Option.some
                (Element_selection.promote ?cancel ~grain ~destination:Group.Point selection
                   topology)
        in
        Result.bind selected (fun selected ->
            Result.bind (selected_unique_points ?cancel ~grain selected geometry) (fun unique ->
                let positions = Packed.Float3.View.view (Geometry.positions geometry) in
                let result =
                  if Array.length unique = 1 then `Point unique.(0)
                  else begin
                    let a = unique.(0) and b = unique.(1) in
                    let c = ref (-1) and at = ref 2 in
                    while !c < 0 && !at < Array.length unique do
                      if non_collinear positions a b unique.(!at) then c := unique.(!at);
                      incr at
                    done;
                    if !c < 0 then `Line (line_endpoints positions unique)
                    else begin
                      let d = ref (-1) and at = ref 0 in
                      while !d < 0 && !at < Array.length unique do
                        let point = unique.(!at) in
                        if
                          point <> a && point <> b && point <> !c
                          && Predicates.orient3d_packed ~x:positions.x ~y:positions.y ~z:positions.z
                               a b !c point
                             <> Predicates.Zero
                        then d := point;
                        incr at
                      done;
                      if !d < 0 then `Plane (planar_hull positions unique a b !c)
                      else `Solid (three_dimensional_hull ?cancel ~grain positions unique a b !c !d)
                    end
                  end
                in
                materialize ?cancel ~grain ~preserve_point_payload ?source_point_attribute
                  ?hull_group geometry result)))
  with Invalid_argument message -> Error message
