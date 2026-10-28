
let operation = "intersection_analysis"
let error code message = Error (Error.make ~operation ~code message)

let validate_name = function
  | None -> true
  | Some name -> String.trim name <> ""

let duplicate_name names =
  let names = List.sort String.compare (List.filter_map Fun.id names) in
  let rec loop = function
    | left :: (right :: _ as rest) -> String.equal left right || loop rest
    | [] | [_] -> false in
  loop names

let index_result role = Result.map_error (fun value ->
  Error.make ~operation ~code:(Error.code value)
    (role ^ ": " ^ Error.message value))

type raw = {
  x : float array;
  y : float array;
  z : float array;
  au : float array;
  av : float array;
  aw : float array;
  bu : float array;
  bv : float array;
  bw : float array;
  source_piece : int array;
  collision_piece : int array;
}

let event_pass ?cancel ~grain ~self ~tolerance ~include_coplanar source_index
    collision_index =
  let source_pieces, collision_pieces = if self then
      Linear_piece_index.View.overlapping_self_pairs ?cancel ~grain
        ~tolerance source_index
    else Linear_piece_index.View.overlapping_pairs ?cancel ~grain ~tolerance
        source_index collision_index in
  let candidate_count = Array.length source_pieces in
  let chunk = max 1 grain and counts = Array.make candidate_count 0 in
  let range_count = Support.ceiling_div candidate_count chunk in
  let source_positions = Linear_piece_index.View.positions source_index
  and collision_positions = Linear_piece_index.View.positions collision_index in
  let narrow scratch point_info events source_piece collision_piece =
    match Linear_piece_index.View.kind source_index source_piece,
        Linear_piece_index.View.kind collision_index collision_piece with
    | Linear_piece_index.Triangle, Linear_piece_index.Triangle ->
        Triangle_intersection.events_points_into ~scratch ~point_info ~events
          ~self ~tolerance ~include_coplanar ~left_positions:source_positions
          ~left_a:(Linear_piece_index.View.point source_index source_piece 0)
          ~left_b:(Linear_piece_index.View.point source_index source_piece 1)
          ~left_c:(Linear_piece_index.View.point source_index source_piece 2)
          ~right_positions:collision_positions
          ~right_a:(Linear_piece_index.View.point collision_index collision_piece 0)
          ~right_b:(Linear_piece_index.View.point collision_index collision_piece 1)
          ~right_c:(Linear_piece_index.View.point collision_index collision_piece 2)
    | Linear_piece_index.Segment, Linear_piece_index.Segment ->
        Curve_intersection.segment_segment_events_into ~scratch ~events ~self
          ~tolerance ~left_positions:source_positions
          ~left_a:(Linear_piece_index.View.point source_index source_piece 0)
          ~left_b:(Linear_piece_index.View.point source_index source_piece 1)
          ~left_u0:(Linear_piece_index.View.curve_u source_index source_piece 0)
          ~left_u1:(Linear_piece_index.View.curve_u source_index source_piece 1)
          ~right_positions:collision_positions
          ~right_a:(Linear_piece_index.View.point collision_index collision_piece 0)
          ~right_b:(Linear_piece_index.View.point collision_index collision_piece 1)
          ~right_u0:(Linear_piece_index.View.curve_u collision_index collision_piece 0)
          ~right_u1:(Linear_piece_index.View.curve_u collision_index collision_piece 1)
    | Linear_piece_index.Segment, Linear_piece_index.Triangle ->
        Curve_intersection.segment_triangle_events_into ~scratch ~events ~self
          ~tolerance ~include_coplanar ~segment_first:true
          ~segment_positions:source_positions
          ~segment_a:(Linear_piece_index.View.point source_index source_piece 0)
          ~segment_b:(Linear_piece_index.View.point source_index source_piece 1)
          ~segment_u0:(Linear_piece_index.View.curve_u source_index source_piece 0)
          ~segment_u1:(Linear_piece_index.View.curve_u source_index source_piece 1)
          ~triangle_positions:collision_positions
          ~triangle_a:(Linear_piece_index.View.point collision_index collision_piece 0)
          ~triangle_b:(Linear_piece_index.View.point collision_index collision_piece 1)
          ~triangle_c:(Linear_piece_index.View.point collision_index collision_piece 2)
    | Linear_piece_index.Triangle, Linear_piece_index.Segment ->
        Curve_intersection.segment_triangle_events_into ~scratch ~events ~self
          ~tolerance ~include_coplanar ~segment_first:false
          ~segment_positions:collision_positions
          ~segment_a:(Linear_piece_index.View.point collision_index collision_piece 0)
          ~segment_b:(Linear_piece_index.View.point collision_index collision_piece 1)
          ~segment_u0:(Linear_piece_index.View.curve_u collision_index collision_piece 0)
          ~segment_u1:(Linear_piece_index.View.curve_u collision_index collision_piece 1)
          ~triangle_positions:source_positions
          ~triangle_a:(Linear_piece_index.View.point source_index source_piece 0)
          ~triangle_b:(Linear_piece_index.View.point source_index source_piece 1)
          ~triangle_c:(Linear_piece_index.View.point source_index source_piece 2) in
  let visit_ranges write =
    if range_count > 0 then Parallel.for_ ~chunk_size:1 ~start:0
        ~finish:(range_count - 1) (fun range ->
      let scratch = Array.make 26 0. and point_info = Array.make 10 0
      and events = Array.make
          (Triangle_intersection.max_events * Triangle_intersection.event_stride)
          0. in
      let first = range * chunk
      and last = min candidate_count ((range + 1) * chunk) in
      for candidate = first to last - 1 do
        if candidate land 4095 = 0 then Cancel.check_opt cancel;
        let count = narrow scratch point_info events source_pieces.(candidate)
            collision_pieces.(candidate) in
        write candidate events count
      done) in
  visit_ranges (fun candidate _ count -> counts.(candidate) <- count);
  let offsets = Array.make (candidate_count + 1) 0 in
  for candidate = 0 to candidate_count - 1 do
    if candidate land 4095 = 0 then Cancel.check_opt cancel;
    if counts.(candidate) > Sys.max_array_length - offsets.(candidate) then
      invalid_arg "Intersection Analysis event cardinality exceeds array limits";
    offsets.(candidate + 1) <- offsets.(candidate) + counts.(candidate)
  done;
  let total = offsets.(candidate_count) in
  let make_float () = Array.make total 0. and make_int () = Array.make total 0 in
  let raw = {
    x = make_float (); y = make_float (); z = make_float ();
    au = make_float (); av = make_float (); aw = make_float ();
    bu = make_float (); bv = make_float (); bw = make_float ();
    source_piece = make_int (); collision_piece = make_int ();
  } in
  visit_ranges (fun candidate events count ->
    if count <> counts.(candidate) then invalid_arg
        "Intersection Analysis count/fill drift";
    let output = offsets.(candidate)
    and source_piece = source_pieces.(candidate)
    and collision_piece = collision_pieces.(candidate) in
    for event = 0 to count - 1 do
      let source = event * Triangle_intersection.event_stride
      and target = output + event in
      raw.x.(target) <- events.(source);
      raw.y.(target) <- events.(source + 1);
      raw.z.(target) <- events.(source + 2);
      raw.au.(target) <- events.(source + 3);
      raw.av.(target) <- events.(source + 4);
      raw.aw.(target) <- events.(source + 5);
      raw.bu.(target) <- events.(source + 6);
      raw.bv.(target) <- events.(source + 7);
      raw.bw.(target) <- events.(source + 8);
      raw.source_piece.(target) <- source_piece;
      raw.collision_piece.(target) <- collision_piece
    done);
  raw

let numerical_weld_tolerance requested raw =
  let extent = ref 0. and magnitude = ref 0. in
  if Array.length raw.x > 0 then begin
    let min_x = ref raw.x.(0) and max_x = ref raw.x.(0)
    and min_y = ref raw.y.(0) and max_y = ref raw.y.(0)
    and min_z = ref raw.z.(0) and max_z = ref raw.z.(0) in
    for point = 0 to Array.length raw.x - 1 do
      let x = raw.x.(point) and y = raw.y.(point) and z = raw.z.(point) in
      min_x := Float.min !min_x x; max_x := Float.max !max_x x;
      min_y := Float.min !min_y y; max_y := Float.max !max_y y;
      min_z := Float.min !min_z z; max_z := Float.max !max_z z;
      magnitude := Float.max !magnitude
          (Float.max (Float.abs x) (Float.max (Float.abs y) (Float.abs z)))
    done;
    extent := Float.max (!max_x -. !min_x)
        (Float.max (!max_y -. !min_y) (!max_z -. !min_z))
  end;
  let next = Float.next_after !magnitude Float.infinity in
  let magnitude_ulp = if Float.is_finite next then next -. !magnitude else 0. in
  Float.max requested (Float.max (8. *. magnitude_ulp)
    (256. *. Float.epsilon *. Float.max 1. !extent))

let cluster_events ?cancel tolerance raw =
  let positions = Packed.Float3.View.of_shared_exn
      ~x:raw.x ~y:raw.y ~z:raw.z in
  let temporary = match Geometry.create ~positions
      ~topology:(Topology.empty ~point_count:(Array.length raw.x)) () with
    | Ok geometry -> geometry
    | Error message -> invalid_arg message in
  match Point_clusters.create ?cancel ~operation:"Intersection Analysis weld"
      ~tolerance ~compatible:(fun _ _ -> true) temporary with
  | Ok result -> result
  | Error message -> invalid_arg message

let representative_data clusters raw = match clusters with
  | Point_clusters.Identity ->
      Array.length raw.x, Array.init (Array.length raw.x) Fun.id,
      Array.init (Array.length raw.x) Fun.id
  | Point_clusters.Clusters clusters ->
      clusters.count, clusters.of_point, clusters.representatives

let point_of_piece_vertex index piece u v w =
  let epsilon = 512. *. Float.epsilon in
  match Linear_piece_index.View.kind index piece with
  | Linear_piece_index.Triangle ->
      let local = if u >= 1. -. epsilon && v <= epsilon && w <= epsilon then 0
        else if v >= 1. -. epsilon && u <= epsilon && w <= epsilon then 1
        else if w >= 1. -. epsilon && u <= epsilon && v <= epsilon then 2
        else -1 in
      if local < 0 then -1 else Linear_piece_index.View.point index piece local
  | Linear_piece_index.Segment ->
      let u0 = Linear_piece_index.View.curve_u index piece 0
      and u1 = Linear_piece_index.View.curve_u index piece 1 in
      if Float.abs (u -. u0) <= epsilon then
        Linear_piece_index.View.point index piece 0
      else if Float.abs (u -. u1) <= epsilon then
        Linear_piece_index.View.point index piece 1
      else -1

let provenance_piece index piece u =
  match Linear_piece_index.View.kind index piece with
  | Linear_piece_index.Triangle -> piece
  | Linear_piece_index.Segment ->
      let epsilon = 512. *. Float.epsilon
      and u0 = Linear_piece_index.View.curve_u index piece 0 in
      if Linear_piece_index.View.local index piece > 0
          && Float.abs (u -. u0) <= epsilon
      then piece - 1 else piece

let build_provenance ?cancel ~self ~source_index ~collision_index clusters raw =
  let point_count, of_event, representatives = representative_data clusters raw in
  let source_count = Linear_piece_index.piece_count source_index in
  let collision_count = if self then 0 else
      Linear_piece_index.piece_count collision_index in
  if source_count > Sys.max_array_length - collision_count then
    invalid_arg "Intersection Analysis provenance key space exceeds array limits";
  let key_count = source_count + collision_count in
  let seen = Array.make key_count (-1) and counts = Array.make point_count 0 in
  let visit_record point input piece index u =
    let canonical = provenance_piece index piece u in
    let key = if input = 0 then canonical else source_count + canonical in
    if seen.(key) <> point then begin seen.(key) <- point; counts.(point) <- counts.(point) + 1 end in
  for event = 0 to Array.length raw.x - 1 do
    if event land 4095 = 0 then Cancel.check_opt cancel;
    let point = of_event.(event) in
    visit_record point 0 raw.source_piece.(event) source_index raw.au.(event);
    visit_record point (if self then 0 else 1) raw.collision_piece.(event)
      collision_index raw.bu.(event)
  done;
  let offsets = Array.make (point_count + 1) 0 in
  for point = 0 to point_count - 1 do
    if counts.(point) > Sys.max_array_length - offsets.(point) then
      invalid_arg "Intersection Analysis provenance cardinality exceeds array limits";
    offsets.(point + 1) <- offsets.(point) + counts.(point)
  done;
  Array.fill seen 0 key_count (-1);
  let total = offsets.(point_count) in
  if total > Sys.max_array_length / 3 then
    invalid_arg "Intersection Analysis UV cardinality exceeds array limits";
  let inputs = Array.make total 0 and primitives = Array.make total 0
  and point_numbers = Array.make total (-1) and uvw = Array.make (total * 3) 0.
  and cursors = Array.copy offsets in
  let emit point input piece index u v w =
    let canonical = provenance_piece index piece u in
    let key = if input = 0 then canonical else source_count + canonical in
    if seen.(key) <> point then begin
      seen.(key) <- point;
      let output = cursors.(point) in
      cursors.(point) <- output + 1;
      inputs.(output) <- input;
      primitives.(output) <- Linear_piece_index.View.primitive index piece;
      point_numbers.(output) <- point_of_piece_vertex index piece u v w;
      uvw.(output * 3) <- u;
      uvw.((output * 3) + 1) <- v;
      uvw.((output * 3) + 2) <- w
    end in
  for event = 0 to Array.length raw.x - 1 do
    if event land 4095 = 0 then Cancel.check_opt cancel;
    let point = of_event.(event) in
    emit point 0 raw.source_piece.(event) source_index
      raw.au.(event) raw.av.(event) raw.aw.(event);
    emit point (if self then 0 else 1) raw.collision_piece.(event)
      collision_index
      raw.bu.(event) raw.bv.(event) raw.bw.(event)
  done;
  let x = Array.make point_count 0. and y = Array.make point_count 0.
  and z = Array.make point_count 0. in
  if point_count > 0 then Parallel.for_ ~chunk_size:16_384 ~start:0
      ~finish:(point_count - 1) (fun point ->
    let event = representatives.(point) in
    x.(point) <- raw.x.(event); y.(point) <- raw.y.(event); z.(point) <- raw.z.(event));
  x, y, z, offsets, inputs, primitives, uvw, point_numbers

let create_output ~input_attribute ~primitive_attribute ~primitive_uvw_attribute
    ~point_attribute data =
  let x, y, z, offsets, inputs, primitives, uvw, point_numbers = data in
  let point_count = Array.length x in
  let attributes = ref [] in
  let add_int_array name values = match name with
    | None -> ()
    | Some name ->
        let packed = match Packed.Int_array.create_owned
            ~offsets:(Array.copy offsets) ~values with
          | Ok packed -> packed | Error message -> invalid_arg message in
        let attribute = match Attribute.create_owned ~owner:Attribute.Point ~name
            (Attribute.Int_array packed) with
          | Ok attribute -> attribute | Error message -> invalid_arg message in
        attributes := attribute :: !attributes in
  add_int_array input_attribute inputs;
  add_int_array primitive_attribute primitives;
  (match primitive_uvw_attribute with
   | None -> ()
   | Some name ->
       if Array.length offsets > Sys.max_array_length then
         invalid_arg "Intersection Analysis UV offset cardinality exceeds array limits";
       let uv_offsets = Array.map (fun offset ->
           if offset > Sys.max_array_length / 3 then invalid_arg
               "Intersection Analysis UV cardinality exceeds array limits";
           offset * 3) offsets in
       let packed = match Packed.Float_array.create_owned ~offsets:uv_offsets
           ~values:uvw with
         | Ok packed -> packed | Error message -> invalid_arg message in
       let attribute = match Attribute.create_owned ~owner:Attribute.Point ~name
           (Attribute.Float_array packed) with
         | Ok attribute -> attribute | Error message -> invalid_arg message in
       attributes := attribute :: !attributes);
  add_int_array point_attribute point_numbers;
  let positions = Packed.Float3.View.of_owned_exn ~x ~y ~z in
  match Geometry.create ~positions ~topology:(Topology.empty ~point_count)
      ~attributes:(List.rev !attributes) () with
  | Ok geometry -> geometry
  | Error message -> invalid_arg message

let run ?cancel ?(grain = 16_384) ?source_primitives
    ?collision_primitives ?(tolerance = 0.) ?(include_coplanar = true)
    ?(input_attribute = Some "sourceinput")
    ?(primitive_attribute = Some "sourceprim")
    ?(primitive_uvw_attribute = Some "sourceprimuv")
    ?(point_attribute = Some "sourcepoint") ?collision geometry =
  try
    if grain <= 0 then error "invalid_parameter" "grain must be positive"
    else if not (Float.is_finite tolerance) || tolerance < 0. then
      error "invalid_parameter" "tolerance must be finite and non-negative"
    else if not (List.for_all validate_name
        [input_attribute; primitive_attribute; primitive_uvw_attribute;
         point_attribute]) then
      error "invalid_parameter" "output attribute names must not be empty"
    else if duplicate_name [input_attribute; primitive_attribute;
        primitive_uvw_attribute; point_attribute] then
      error "invalid_parameter" "output attribute names must be distinct"
    else begin
      Cancel.check_opt cancel;
      let self = collision = None in
      let collision_geometry = match collision with None -> geometry | Some value -> value in
      Result.bind (index_result "source"
        (Linear_piece_index.create ?cancel ~grain
          ?primitives:source_primitives geometry)) (fun source_index ->
      Result.bind (if self then Ok source_index else index_result "collision"
          (Linear_piece_index.create ?cancel ~grain
            ?primitives:collision_primitives collision_geometry))
        (fun collision_index ->
      let raw = event_pass ?cancel ~grain ~self ~tolerance ~include_coplanar
          source_index collision_index in
      let weld = numerical_weld_tolerance tolerance raw in
      let clusters = cluster_events ?cancel weld raw in
      let data = build_provenance ?cancel ~self ~source_index ~collision_index
          clusters raw in
      Ok (create_output ~input_attribute ~primitive_attribute
        ~primitive_uvw_attribute ~point_attribute data)))
    end
  with
  | Cancel.Cancelled -> error "cancelled" "Intersection Analysis was cancelled"
  | Invalid_argument message -> error "invalid_geometry" message

