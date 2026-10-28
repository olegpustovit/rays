type expression =
  | Left
  | Right
  | Not of expression
  | And of expression * expression
  | Or of expression * expression
  | Xor of expression * expression

let union = Or (Left, Right)
let intersection = And (Left, Right)
let difference = And (Left, Not Right)
let reverse_difference = And (Right, Not Left)
let xor = Xor (Left, Right)

let operation = "boolean_extract"
let error code message = Error (Error.make ~operation ~code message)

type ancestry = {
  complex : Boolean_complex.t;
  geometry : Geometry.t;
  left_geometry : Geometry.t;
  right_geometry : Geometry.t;

  complex_output_points : int array;
  corner_complex_vertices : int array;
  primitive_complex_facets : int array;
  primitive_sides : bytes;
  primitive_faces : int array;
  primitive_triangles : int array;
  primitive_refined_triangles : int array;
  primitive_windings : bytes;
  primitive_source_points : int array;
  primitive_source_vertices : int array;
  barycentric_a : float array;
  barycentric_b : float array;
  barycentric_c : float array;
  edge_source_offsets : int array;
  edge_source_sides : bytes;
  edge_source_edges : int array;
}

let coalesce_positions ~complex_vertices ~x ~y ~z =
  let points = Array.length x in
  if Array.length y <> points || Array.length z <> points
      || Array.length complex_vertices <> points then
    invalid_arg "Boolean rounded-point coalescing cardinality mismatch";
  if points > Sys.max_array_length / 2 then
    invalid_arg "Boolean rounded-point table exceeds array limits";
  let needed = max 2 (points * 2) and capacity = ref 2 in
  while !capacity < needed do
    if !capacity > Sys.max_array_length / 2 then
      invalid_arg "Boolean rounded-point table exceeds array limits";
    capacity := !capacity * 2
  done;
  let mix bits =
    let bits = bits lxor (bits lsr 30) in
    let bits = bits * 0x1e35a7bd in
    let bits = bits lxor (bits lsr 27) in
    let bits = bits * 0x17a1465b in
    bits lxor (bits lsr 31) in
  let hash_float value =
    mix (Int64.to_int
      (if value = 0. then 0L else Int64.bits_of_float value)) in
  let rotate_left value shift =
    (value lsl shift) lor (value lsr (Sys.int_size - shift)) in
  let slots = Array.make !capacity (-1) and mask = !capacity - 1
  and old_to_new = Array.make points (-1)
  and new_complex = Array.make points (-1) and count = ref 0 in
  for point = 0 to points - 1 do
    let hash = hash_float x.(point)
        lxor rotate_left (hash_float y.(point)) 21
        lxor rotate_left (hash_float z.(point)) 42 in
    let slot = ref (hash land mask) and searching = ref true in
    while !searching do
      let other = slots.(!slot) in
      if other < 0 then begin
        slots.(!slot) <- !count;
        old_to_new.(point) <- !count;
        x.(!count) <- x.(point); y.(!count) <- y.(point); z.(!count) <- z.(point);
        new_complex.(!count) <- complex_vertices.(point);
        incr count;
        searching := false
      end else if x.(other) = x.(point) && y.(other) = y.(point)
          && z.(other) = z.(point) then begin
        old_to_new.(point) <- other;
        searching := false
      end else slot := (!slot + 1) land mask
    done
  done;
  Array.sub x 0 !count, Array.sub y 0 !count, Array.sub z 0 !count,
  Array.sub new_complex 0 !count, old_to_new

let geometry value = value.geometry
let primitive_side value primitive =
  if Bytes.unsafe_get value.primitive_sides primitive = '\000'
  then Boolean_complex.Left else Boolean_complex.Right
let primitive_face value primitive = value.primitive_faces.(primitive)
let primitive_refined_triangle value primitive =
  value.primitive_refined_triangles.(primitive)
let primitive_winding value primitive =
  if Bytes.unsafe_get value.primitive_windings primitive = '\000' then 1 else -1

let piece_ids ~weiler ~ancestry ~source ~primitive_count =
  let complex = ancestry.complex in
  let shell_to_piece = Array.make (Boolean_weiler.shell_count weiler) (-1)
  and next_piece = ref 0 in
  Array.init primitive_count (fun primitive ->
    let source_primitive = source primitive in
    let facet = ancestry.primitive_complex_facets.(source_primitive) in
    let second = ancestry.corner_complex_vertices.((source_primitive * 3) + 1) in
    let canonical_second = Boolean_complex.facet_vertex complex facet 1
    and reversed_second = Boolean_complex.facet_vertex complex facet 2 in
    let half =
      if second = canonical_second then Boolean_weiler.Negative
      else if second = reversed_second then Boolean_weiler.Positive
      else invalid_arg "Boolean output ancestry lost facet orientation" in
    let shell = Boolean_weiler.half_facet_shell weiler
        (Boolean_weiler.half_facet facet half) in
    let piece = shell_to_piece.(shell) in
    if piece >= 0 then piece else begin
      let piece = !next_piece in
      shell_to_piece.(shell) <- piece;
      incr next_piece;
      piece
    end)

let rec evaluate expression left right = match expression with
  | Left -> left
  | Right -> right
  | Not value -> not (evaluate value left right)
  | And (first, second) ->
      evaluate first left right && evaluate second left right
  | Or (first, second) ->
      evaluate first left right || evaluate second left right
  | Xor (first, second) ->
      evaluate first left right <> evaluate second left right

let edge_key point_count first second =
  let first, second = if first < second then first, second else second, first in
  if first > (max_int - second) / point_count then
    invalid_arg "Boolean output edge key exceeds integer range";
  (first * point_count) + second

let validate_positions ~x ~y ~z =
  let points = Array.length x in
  if Array.length y <> points || Array.length z <> points then
    error "rounding_cardinality" "materialized coordinate planes have different lengths"
  else begin
    if points > Sys.max_array_length / 2 then
      invalid_arg "Boolean materialization coordinate table exceeds array limits";
    let needed = max 2 (points * 2) and capacity = ref 2 in
    while !capacity < needed do
      if !capacity > Sys.max_array_length / 2 then
        invalid_arg "Boolean materialization coordinate table exceeds array limits";
      capacity := !capacity * 2
    done;
    let slots = Array.make !capacity (-1) and mask = !capacity - 1 in
    let mix bits =
      let bits = bits lxor (bits lsr 30) in
      let bits = bits * 0x1e35a7bd in
      let bits = bits lxor (bits lsr 27) in
      let bits = bits * 0x17a1465b in
      bits lxor (bits lsr 31) in
    let hash_float value =
      mix (Int64.to_int
        (if value = 0. then 0L else Int64.bits_of_float value)) in
    let rotate_left value shift =
      (value lsl shift) lor (value lsr (Sys.int_size - shift)) in
    let collision = ref None and point = ref 0 in
    while Option.is_none !collision && !point < points do
      let hash = hash_float x.(!point)
          lxor rotate_left (hash_float y.(!point)) 21
          lxor rotate_left (hash_float z.(!point)) 42 in
      let slot = ref (hash land mask) and searching = ref true in
      while !searching do
        let other = slots.(!slot) in
        if other < 0 then begin
          slots.(!slot) <- !point;
          searching := false
        end else if x.(other) = x.(!point) && y.(other) = y.(!point)
            && z.(other) = z.(!point) then begin
          collision := Some (other, !point);
          searching := false
        end else slot := (!slot + 1) land mask
      done;
      incr point
    done;
    match !collision with
    | Some (first, second) ->
        error "rounding_collision" (Printf.sprintf
          "exact output vertices %d and %d round to the same binary64 coordinate"
          first second)
    | None -> Ok ()
  end

let validate_materialized ?(allow_degenerate = false) ~x ~y ~z ~vertex_points () =
  let points = Array.length x in
  if Array.length vertex_points mod 3 <> 0 then
    error "rounding_cardinality" "materialized triangle corners are not divisible by three"
  else match validate_positions ~x ~y ~z with
    | Error _ as failure -> failure
    | Ok () ->
        let projected projection a b c = match projection with
          | 0 -> Predicates.orient2d_packed ~x ~y a b c
          | 1 -> Predicates.orient2d_packed ~x:y ~y:z a b c
          | _ -> Predicates.orient2d_packed ~x:z ~y:x a b c in
        let failure = ref None and triangle = ref 0 in
        while Option.is_none !failure
            && !triangle < Array.length vertex_points / 3 do
          let offset = !triangle * 3 in
          let a = vertex_points.(offset) and b = vertex_points.(offset + 1)
          and c = vertex_points.(offset + 2) in
          if a < 0 || a >= points || b < 0 || b >= points || c < 0 || c >= points then
            failure := Some (`Bounds !triangle)
          else begin
            let abx = x.(b) -. x.(a) and aby = y.(b) -. y.(a)
            and abz = z.(b) -. z.(a) and acx = x.(c) -. x.(a)
            and acy = y.(c) -. y.(a) and acz = z.(c) -. z.(a) in
            let nx = (aby *. acz) -. (abz *. acy)
            and ny = (abz *. acx) -. (abx *. acz)
            and nz = (abx *. acy) -. (aby *. acx) in
            let first, second, third =
              if Float.is_finite nx && Float.is_finite ny && Float.is_finite nz then
                let ax = abs_float nx and ay = abs_float ny and az = abs_float nz in
                if az >= ax && az >= ay then 0, 1, 2
                else if ax >= ay then 1, 2, 0 else 2, 0, 1
              else 0, 1, 2 in
            if not allow_degenerate
                && projected first a b c = Predicates.Zero
                && projected second a b c = Predicates.Zero
                && projected third a b c = Predicates.Zero then
              failure := Some (`Degenerate !triangle)
          end;
          incr triangle
        done;
        (match !failure with
         | None -> Ok ()
         | Some (`Bounds triangle) -> error "rounding_cardinality"
             (Printf.sprintf "materialized triangle %d references an invalid point" triangle)
         | Some (`Degenerate triangle) -> error "rounding_degenerate"
             (Printf.sprintf "exact output triangle %d becomes collinear in binary64" triangle))

let validate_closed_topology topology =
  let topology = Topology.Private.view topology in
  let primitive_count = Bytes.length topology.primitive_kinds in
  let triangle_only = ref true and primitive = ref 0 in
  while !triangle_only && !primitive < primitive_count do
    triangle_only := Bytes.unsafe_get topology.primitive_kinds !primitive = '\000'
      && topology.primitive_offsets.(!primitive + 1)
         - topology.primitive_offsets.(!primitive) = 3;
    incr primitive
  done;
  if not !triangle_only then
    error "invalid_output" "solid Boolean output is not triangle-only"
  else if primitive_count > Sys.max_array_length / 3 then
    error "rounding_cardinality" "solid Boolean edge cardinality exceeds array limits"
  else begin
    let edge_count = primitive_count * 3 in
    let keys = Array.make edge_count 0 in
    let point_count = topology.point_count in
    for triangle = 0 to primitive_count - 1 do
      let offset = triangle * 3 in
      let a = topology.vertex_points.(offset)
      and b = topology.vertex_points.(offset + 1)
      and c = topology.vertex_points.(offset + 2) in
      keys.(offset) <- edge_key point_count a b;
      keys.(offset + 1) <- edge_key point_count b c;
      keys.(offset + 2) <- edge_key point_count c a
    done;
    Array.sort Int.compare keys;
    let failure = ref None and first = ref 0 in
    while Option.is_none !failure && !first < edge_count do
      let last = ref (!first + 1) in
      while !last < edge_count && keys.(!last) = keys.(!first) do incr last done;
      let incidence = !last - !first in
      if incidence < 2 || incidence land 1 <> 0 then
        failure := Some (keys.(!first), incidence);
      first := !last
    done;
    match !failure with
    | None -> Ok ()
    | Some (key, incidence) -> error "open_output" (Printf.sprintf
        "solid Boolean output edge key %d has invalid incidence %d" key incidence)
  end

let native_edge (view : Topology_index.Private.view) primitive first second =
  if view.primitive_of_vertex.(first) = primitive
      && view.next_vertex.(first) = second then view.edge_of_vertex.(first)
  else if view.primitive_of_vertex.(second) = primitive
      && view.next_vertex.(second) = first then view.edge_of_vertex.(second)
  else -1

let explicit_id point = match Implicit_point.construction point with
  | Implicit_point.Explicit point -> point
  | Implicit_point.Rounded _ | Implicit_point.Line_plane _ | Implicit_point.Line_line _
  | Implicit_point.Triple_plane _ | Implicit_point.Midpoint _
  | Implicit_point.Centroid3 _ | Implicit_point.Circumcenter2_xy _ -> -1

let same_edge first second a b =
  (first = a && second = b) || (first = b && second = a)

let explicit_source_edge first_id second_id edge0 b_id c_id edge1 c_id' a_id
    edge2 a_id' b_id' =
  if first_id >= 0 && second_id >= 0 then
    if edge0 >= 0 && same_edge first_id second_id b_id c_id then edge0
    else if edge1 >= 0 && same_edge first_id second_id c_id' a_id then edge1
    else if edge2 >= 0 && same_edge first_id second_id a_id' b_id' then edge2
    else -1
  else -1

type barycentric_cache = {
  triangle_keys : int array;
  point_keys : int array;
  weight_a : float array;
  weight_b : float array;
  weight_c : float array;
  mask : int;
}

let barycentric_cache capacity =
  let needed = max 2 (capacity * 2) and size = ref 2 in
  while !size < needed do
    if !size > Sys.max_array_length / 2 then
      invalid_arg "Boolean barycentric cache exceeds array limits";
    size := !size * 2
  done;
  {
    triangle_keys = Array.make !size (-1);
    point_keys = Array.make !size (-1);
    weight_a = Array.make !size 0.;
    weight_b = Array.make !size 0.;
    weight_c = Array.make !size 0.;
    mask = !size - 1;
  }

let cached_barycentric cache source ~triangle ~point ~a ~b ~c implicit =
  let value = (triangle * 0x1e35a7bd) lxor (point * 0x17a1465b) in
  let slot = ref ((value lxor (value lsr 16)) land cache.mask)
  and searching = ref true and result = ref (0., 0., 0.) in
  while !searching do
    let stored_triangle = cache.triangle_keys.(!slot) in
    if stored_triangle < 0 then begin
      let wa, wb, wc = Implicit_point.barycentric_source_triangle source
          ~a ~b ~c implicit in
      cache.triangle_keys.(!slot) <- triangle;
      cache.point_keys.(!slot) <- point;
      cache.weight_a.(!slot) <- wa;
      cache.weight_b.(!slot) <- wb;
      cache.weight_c.(!slot) <- wc;
      result := wa, wb, wc;
      searching := false
    end else if stored_triangle = triangle && cache.point_keys.(!slot) = point then begin
      result := cache.weight_a.(!slot), cache.weight_b.(!slot),
        cache.weight_c.(!slot);
      searching := false
    end else slot := (!slot + 1) land cache.mask
  done;
  !result

let build_internal ?cancel ?(require_closed = true)
    ?(defer_rounded_slivers = false) ?facet_selection
    ?preferred_side ?barycentric_cache:shared_barycentrics
    ~with_ancestry ~with_corner_payload ~expression complex weiler cells =
  try
    Cancel.check_opt cancel;
    if Boolean_weiler.Private.complex weiler != complex then
      invalid_arg "Weiler graph belongs to a different Boolean complex";
    if Boolean_cells.Private.weiler cells != weiler then
      invalid_arg "cell labels belong to a different Weiler graph";
    if Boolean_cells.shell_count cells <> Boolean_weiler.shell_count weiler then
      invalid_arg "cell labels do not match the Weiler shell cardinality";
    let table = Bytes.make 4 '\000' in
    for state = 0 to 3 do
      if evaluate expression (state land 2 <> 0) (state land 1 <> 0) then
        Bytes.unsafe_set table state '\001'
    done;
    let inside shell =
      let state = (if Boolean_cells.left_winding cells shell <> 0 then 2 else 0)
          + (if Boolean_cells.right_winding cells shell <> 0 then 1 else 0) in
      Bytes.unsafe_get table state <> '\000' in
    let facets = Boolean_complex.facet_count complex in
    (match facet_selection with
     | Some values when Bytes.length values <> facets ->
         invalid_arg "Boolean facet selection cardinality mismatch"
     | Some values ->
         for facet = 0 to facets - 1 do
           let value = Bytes.unsafe_get values facet in
           if value <> '\000' && value <> '\001' && value <> '\002' then
             invalid_arg "Boolean facet selection contains an invalid orientation"
         done
     | None -> ());
    let facets = Boolean_complex.facet_count complex
    and selected = Bytes.make (Boolean_complex.facet_count complex) '\000'
    and reversed = Bytes.make (Boolean_complex.facet_count complex) '\000'
    and selected_count = ref 0 in
    for facet = 0 to facets - 1 do
      if facet land 4095 = 0 then Cancel.check_opt cancel;
      match facet_selection with
      | Some values ->
          let choice = Bytes.unsafe_get values facet in
          if choice <> '\000' then begin
            Bytes.unsafe_set selected facet '\001';
            if choice = '\002' then Bytes.unsafe_set reversed facet '\001';
            incr selected_count
          end
      | None ->
          let negative = Boolean_weiler.half_facet_shell weiler
              (Boolean_weiler.half_facet facet Boolean_weiler.Negative)
          and positive = Boolean_weiler.half_facet_shell weiler
              (Boolean_weiler.half_facet facet Boolean_weiler.Positive) in
          let negative_inside = inside negative and positive_inside = inside positive in
          if negative_inside <> positive_inside then begin
            Bytes.unsafe_set selected facet '\001';
            if positive_inside then Bytes.unsafe_set reversed facet '\001';
            incr selected_count
          end
    done;
    if !selected_count > Sys.max_array_length / 3 then
      invalid_arg "Boolean output corner cardinality exceeds array limits";
    let used = Bytes.make (Boolean_complex.vertex_count complex) '\000' in
    for facet = 0 to facets - 1 do
      if Bytes.unsafe_get selected facet <> '\000' then
        for local = 0 to 2 do
          Bytes.unsafe_set used (Boolean_complex.facet_vertex complex facet local) '\001'
        done
    done;
    let point_map = Array.make (Boolean_complex.vertex_count complex) (-1)
    and point_count = ref 0 in
    for point = 0 to Boolean_complex.vertex_count complex - 1 do
      if Bytes.unsafe_get used point <> '\000' then begin
        point_map.(point) <- !point_count;
        incr point_count
      end
    done;
    let x = Array.make !point_count 0. and y = Array.make !point_count 0.
    and z = Array.make !point_count 0.
    and materialized_complex_vertices = Array.make !point_count (-1) in
    for point = 0 to Boolean_complex.vertex_count complex - 1 do
      let output = point_map.(point) in
      if output >= 0 then begin
        materialized_complex_vertices.(output) <- point;
        let px, py, pz = Boolean_complex.approximate_vertex complex point in
        x.(output) <- px; y.(output) <- py; z.(output) <- pz
      end
    done;
    let x, y, z, materialized_complex_vertices, old_to_new =
      coalesce_positions ~complex_vertices:materialized_complex_vertices ~x ~y ~z in
    (match validate_positions ~x ~y ~z with
     | Ok () -> ()
     | Error failure -> invalid_arg
         ("Boolean rounded-point coalescing invariant failed: "
          ^ Error.to_string failure));
    for point = 0 to Array.length point_map - 1 do
      let output = point_map.(point) in
      if output >= 0 then point_map.(point) <- old_to_new.(output)
    done;
    point_count := Array.length x;
    let _point_complex_vertices = if with_ancestry
      then materialized_complex_vertices else [||]
    and complex_output_points = if with_ancestry then Array.copy point_map else [||] in
    let constraints = Boolean_complex.Private.constraints complex in
    let left_geometry = Boolean_constraints.Private.left_geometry constraints
    and right_geometry = Boolean_constraints.Private.right_geometry constraints in
    let left_edge_index =
      if with_corner_payload && Geometry.edge_groups left_geometry <> [] then
        Some (Topology_index.create ?cancel (Geometry.topology left_geometry)
          |> Topology_index.Private.view)
      else None
    and right_edge_index =
      if with_corner_payload && Geometry.edge_groups right_geometry <> [] then
        Some (Topology_index.create ?cancel (Geometry.topology right_geometry)
          |> Topology_index.Private.view)
      else None in
    let has_edge_sources = Option.is_some left_edge_index
        || Option.is_some right_edge_index in
    let selected_members = ref 0 and maximum_facet_members = ref 0 in
    if with_ancestry && has_edge_sources then
      for facet = 0 to facets - 1 do
        if Bytes.unsafe_get selected facet <> '\000' then begin
          let first, last = Boolean_complex.facet_member_range complex facet in
          if !selected_members > Sys.max_array_length - (last - first) then
            invalid_arg "Boolean edge ancestry member cardinality exceeds array limits";
          let count = last - first in
          selected_members := !selected_members + count;
          maximum_facet_members := max !maximum_facet_members count
        end
      done;
    if !selected_members > Sys.max_array_length / 3 then
      invalid_arg "Boolean edge ancestry cardinality exceeds array limits";
    let edge_source_capacity = if has_edge_sources then !selected_members * 3 else 0 in
    let vertex_points = Array.make (!selected_count * 3) 0
    and corner_complex_vertices =
      Array.make (if with_ancestry then !selected_count * 3 else 0) (-1)
    and primitive_complex_facets =
      Array.make (if with_ancestry then !selected_count else 0) (-1)
    and primitive_sides = Bytes.make (if with_ancestry then !selected_count else 0) '\000'
    and primitive_faces = Array.make (if with_ancestry then !selected_count else 0) 0
    and primitive_triangles = Array.make (if with_ancestry then !selected_count else 0) (-1)
    and primitive_refined_triangles =
      Array.make (if with_ancestry then !selected_count else 0) (-1)
    and primitive_windings =
      Bytes.make (if with_ancestry then !selected_count else 0) '\000'
    and primitive_source_points =
      Array.make (if with_ancestry then !selected_count * 3 else 0) 0
    and primitive_source_vertices =
      Array.make (if with_ancestry then !selected_count * 3 else 0) 0
    and barycentric_a =
      Array.make (if with_ancestry then !selected_count * 3 else 0) 0.
    and barycentric_b =
      Array.make (if with_ancestry then !selected_count * 3 else 0) 0.
    and barycentric_c =
      Array.make (if with_ancestry then !selected_count * 3 else 0) 0.
    and edge_source_offsets =
      Array.make (if with_ancestry then (!selected_count * 3) + 1 else 0) 0
    and edge_source_sides = Bytes.make edge_source_capacity '\000'
    and edge_source_edges = Array.make edge_source_capacity (-1)
    and edge_source_count = ref 0
    and edge_source_scratch = Array.make (!maximum_facet_members * 3) (-1)
    and edge_side_scratch = Bytes.make !maximum_facet_members '\000'
    and output = ref 0 in
    let barycentrics = match shared_barycentrics with
      | Some cache -> cache
      | None -> barycentric_cache
          (if with_corner_payload then !selected_count * 3 else 0) in
    for facet = 0 to facets - 1 do
      if facet land 4095 = 0 then Cancel.check_opt cancel;
      if Bytes.unsafe_get selected facet <> '\000' then begin
        let a = point_map.(Boolean_complex.facet_vertex complex facet 0)
        and b = point_map.(Boolean_complex.facet_vertex complex facet 1)
        and c = point_map.(Boolean_complex.facet_vertex complex facet 2)
        and offset = !output * 3 in
        vertex_points.(offset) <- a;
        if Bytes.unsafe_get reversed facet = '\000' then begin
          vertex_points.(offset + 1) <- b;
          vertex_points.(offset + 2) <- c;
          if with_ancestry then begin
            corner_complex_vertices.(offset) <-
              Boolean_complex.facet_vertex complex facet 0;
            corner_complex_vertices.(offset + 1) <-
              Boolean_complex.facet_vertex complex facet 1;
            corner_complex_vertices.(offset + 2) <-
              Boolean_complex.facet_vertex complex facet 2
          end
        end else begin
          vertex_points.(offset + 1) <- c;
          vertex_points.(offset + 2) <- b;
          if with_ancestry then begin
            corner_complex_vertices.(offset) <-
              Boolean_complex.facet_vertex complex facet 0;
            corner_complex_vertices.(offset + 1) <-
              Boolean_complex.facet_vertex complex facet 2;
            corner_complex_vertices.(offset + 2) <-
              Boolean_complex.facet_vertex complex facet 1
          end
        end;
        if with_ancestry then begin
        primitive_complex_facets.(!output) <- facet;
        let first_member, last_member =
          Boolean_complex.facet_member_range complex facet in
        if first_member = last_member then
          invalid_arg "Boolean output facet has no source member";
        let desired = if Bytes.unsafe_get reversed facet = '\000' then 1 else -1
        and member = ref first_member and preferred = ref (-1) in
        let side_matches member = match preferred_side with
          | None -> true
          | Some side -> Boolean_complex.member_side complex member = side in
        while !preferred < 0 && !member < last_member do
          if side_matches !member
              && Boolean_complex.member_winding complex !member = desired then
            preferred := !member;
          incr member
        done;
        if !preferred < 0 then begin
          member := first_member;
          while !preferred < 0 && !member < last_member do
            if side_matches !member then preferred := !member;
            incr member
          done
        end;
        if !preferred < 0 then
          invalid_arg "selected Boolean facet has no member on its requested side";
        let member = !preferred in
        if Boolean_complex.member_side complex member = Boolean_complex.Right then
          Bytes.unsafe_set primitive_sides !output '\001';
        let source_triangle = Boolean_complex.member_face complex member in
        let surface = match Boolean_complex.member_side complex member with
          | Boolean_complex.Left -> Boolean_constraints.Private.left_surface constraints
          | Boolean_complex.Right -> Boolean_constraints.Private.right_surface constraints in
        primitive_faces.(!output) <-
          Surface_index.Private.triangle_primitive surface source_triangle;
        primitive_triangles.(!output) <- source_triangle;
        primitive_refined_triangles.(!output) <-
          Boolean_complex.member_triangle complex member;
        let triangle_point = match Boolean_complex.member_side complex member with
          | Boolean_complex.Left -> Boolean_constraints.Private.left_triangle_point
          | Boolean_complex.Right -> Boolean_constraints.Private.right_triangle_point in
        let source = Boolean_constraints.Private.source constraints in
        let source_a = triangle_point constraints source_triangle 0
        and source_b = triangle_point constraints source_triangle 1
        and source_c = triangle_point constraints source_triangle 2 in
        let triangle_key = source_triangle +
          (match Boolean_complex.member_side complex member with
           | Boolean_complex.Left -> 0
           | Boolean_complex.Right ->
               Boolean_constraints.left_triangle_count constraints) in
        if with_corner_payload then begin
          for local = 0 to 2 do
            primitive_source_points.((!output * 3) + local) <-
              Surface_index.Private.triangle_point surface source_triangle local;
            primitive_source_vertices.((!output * 3) + local) <-
              Surface_index.Private.triangle_vertex surface source_triangle local
          done;
          for local = 0 to 2 do
            let facet_local = if local = 0 || desired > 0 then local else 3 - local in
            let complex_vertex = Boolean_complex.facet_vertex complex facet facet_local in
            let point = Boolean_complex.Private.vertex complex complex_vertex in
            let wa, wb, wc = cached_barycentric barycentrics source
                ~triangle:triangle_key ~point:complex_vertex
                ~a:source_a ~b:source_b ~c:source_c point in
            let corner = (!output * 3) + local in
            barycentric_a.(corner) <- wa;
            barycentric_b.(corner) <- wb;
            barycentric_c.(corner) <- wc
          done
        end;
        if edge_source_capacity > 0 then begin
          let facet_local value =
            if value = 0 || desired > 0 then value else 3 - value in
          let output_point local = Boolean_complex.Private.vertex complex
              (Boolean_complex.facet_vertex complex facet (facet_local local)) in
          let output0 = output_point 0 and output1 = output_point 1
          and output2 = output_point 2 in
          let output0_id = explicit_id output0 and output1_id = explicit_id output1
          and output2_id = explicit_id output2 in
          let candidate = ref first_member and relative = ref 0 in
          while !candidate < last_member do
            let side = Boolean_complex.member_side complex !candidate in
            Bytes.unsafe_set edge_side_scratch !relative
              (if side = Boolean_complex.Left then '\000' else '\001');
            let index = match side with
              | Boolean_complex.Left -> left_edge_index
              | Boolean_complex.Right -> right_edge_index in
            (match index with
             | None ->
                 edge_source_scratch.(!relative * 3) <- -1;
                 edge_source_scratch.((!relative * 3) + 1) <- -1;
                 edge_source_scratch.((!relative * 3) + 2) <- -1
             | Some index ->
                 let triangle = Boolean_complex.member_face complex !candidate in
                 let candidate_surface, candidate_triangle_point = match side with
                   | Boolean_complex.Left ->
                       Boolean_constraints.Private.left_surface constraints,
                       Boolean_constraints.Private.left_triangle_point
                   | Boolean_complex.Right ->
                       Boolean_constraints.Private.right_surface constraints,
                       Boolean_constraints.Private.right_triangle_point in
                 let primitive = Surface_index.Private.triangle_primitive
                     candidate_surface triangle in
                 let a_id = candidate_triangle_point constraints triangle 0
                 and b_id = candidate_triangle_point constraints triangle 1
                 and c_id = candidate_triangle_point constraints triangle 2 in
                 let va = Surface_index.Private.triangle_vertex
                     candidate_surface triangle 0
                 and vb = Surface_index.Private.triangle_vertex
                     candidate_surface triangle 1
                 and vc = Surface_index.Private.triangle_vertex
                     candidate_surface triangle 2 in
                 let edge0 = native_edge index primitive vb vc
                 and edge1 = native_edge index primitive vc va
                 and edge2 = native_edge index primitive va vb
                 and base = !relative * 3 in
                 let direct0 = explicit_source_edge output0_id output1_id
                     edge0 b_id c_id edge1 c_id a_id edge2 a_id b_id
                 and direct1 = explicit_source_edge output1_id output2_id
                     edge0 b_id c_id edge1 c_id a_id edge2 a_id b_id
                 and direct2 = explicit_source_edge output2_id output0_id
                     edge0 b_id c_id edge1 c_id a_id edge2 a_id b_id in
                 if direct0 >= 0 && direct1 >= 0 && direct2 >= 0 then begin
                   edge_source_scratch.(base) <- direct0;
                   edge_source_scratch.(base + 1) <- direct1;
                   edge_source_scratch.(base + 2) <- direct2
                 end else begin
                   let projection = Implicit_point.source_triangle_projection
                       source a_id b_id c_id in
                   let resolve direct first second =
                     if direct >= 0 then direct else
                     let contains edge source_first source_second =
                       edge >= 0
                       && Implicit_point.source_segment_contains source ~projection
                         ~first:source_first ~second:source_second first
                       && Implicit_point.source_segment_contains source ~projection
                         ~first:source_first ~second:source_second second in
                     if contains edge0 b_id c_id then edge0
                     else if contains edge1 c_id a_id then edge1
                     else if contains edge2 a_id b_id then edge2 else -1 in
                   edge_source_scratch.(base) <- resolve direct0 output0 output1;
                   edge_source_scratch.(base + 1) <- resolve direct1 output1 output2;
                   edge_source_scratch.(base + 2) <- resolve direct2 output2 output0
                 end);
            incr candidate;
            incr relative
          done;
          let member_count = last_member - first_member in
          for local = 0 to 2 do
            let corner = (!output * 3) + local in
            edge_source_offsets.(corner) <- !edge_source_count;
            for member = 0 to member_count - 1 do
              let edge = edge_source_scratch.((member * 3) + local) in
              if edge >= 0 then begin
                if !edge_source_count >= edge_source_capacity then
                  invalid_arg "Boolean edge ancestry exceeded its proven capacity";
                Bytes.unsafe_set edge_source_sides !edge_source_count
                  (Bytes.unsafe_get edge_side_scratch member);
                edge_source_edges.(!edge_source_count) <- edge;
                incr edge_source_count
              end
            done
          done
        end else
          for local = 0 to 2 do
            edge_source_offsets.((!output * 3) + local) <- !edge_source_count
          done;
        let relative_winding = desired * Boolean_complex.member_winding complex member in
        if relative_winding < 0 then
          Bytes.unsafe_set primitive_windings !output '\001';
        end;
        incr output
      end
    done;
    if with_ancestry then
      edge_source_offsets.(!selected_count * 3) <- !edge_source_count;
    (match validate_materialized ~allow_degenerate:defer_rounded_slivers
        ~x ~y ~z ~vertex_points () with
     | Error failure -> raise (Error.Rdk (Error.make ~operation
         ~code:(Error.code failure)
         ("post-coalescing extraction validation failed: "
          ^ Error.to_string failure)))
     | Ok () -> ());
    let topology = Topology.polygons_owned ~point_count:!point_count
        ~vertex_points
        ~primitive_offsets:(Array.init (!selected_count + 1)
          (fun triangle -> triangle * 3)) in
    (match topology with
     | Error message -> error "invalid_output" message
     | Ok topology ->
         (match if require_closed then validate_closed_topology topology else Ok () with
          | Error failure -> raise (Error.Rdk failure)
          | Ok () -> ());
         match Geometry.create
             ~positions:(Packed.Float3.Private.of_owned_exn ~x ~y ~z)
             ~topology () with
         | Ok geometry ->
             Ok {
             complex;
             geometry;
             left_geometry; right_geometry;
              complex_output_points;
             corner_complex_vertices; primitive_complex_facets;
             primitive_sides; primitive_faces; primitive_triangles;
             primitive_refined_triangles; primitive_windings;
             primitive_source_points; primitive_source_vertices;
             barycentric_a; barycentric_b; barycentric_c;
             edge_source_offsets; edge_source_sides; edge_source_edges;
           }
         | Error message -> error "invalid_output" message)
  with
  | Cancel.Cancelled -> error "cancelled" "Boolean extraction was cancelled"
  | Error.Rdk failure -> Error failure
  | Invalid_argument message -> error "invalid_output" message

let build_with_ancestry ?cancel ?require_closed ?defer_rounded_slivers
    ?(corner_payload = true)
    ~expression complex weiler cells =
  build_internal ?cancel ?require_closed ?defer_rounded_slivers
    ~with_ancestry:true ~with_corner_payload:corner_payload
    ~expression complex weiler cells

let build_selected_with_ancestry ?cancel
    ?defer_rounded_slivers ?barycentric_cache ?(corner_payload = true)
    ~selection ~side complex weiler cells =
  build_internal ?cancel ~require_closed:false ?defer_rounded_slivers ?barycentric_cache
    ~facet_selection:selection
    ~preferred_side:side ~with_ancestry:true ~with_corner_payload:corner_payload
    ~expression:Left
    complex weiler cells

let reverse_ancestry ?cancel value =
  try
    Cancel.check_opt cancel;
    let primitives = Geometry.primitive_count value.geometry in
    let corners = primitives * 3 in
    let source_topology = Topology.Private.view (Geometry.topology value.geometry) in
    if Array.length source_topology.vertex_points <> corners then
      invalid_arg "Boolean ancestry reversal requires triangular topology";
    let vertex_points = Array.make corners 0
    and corner_complex_vertices = Array.make corners 0
    and primitive_windings = Bytes.copy value.primitive_windings
    and barycentric_a = Array.make corners 0.
    and barycentric_b = Array.make corners 0.
    and barycentric_c = Array.make corners 0.
    and edge_source_offsets = Array.make (corners + 1) 0 in
    let edge_source_count = value.edge_source_offsets.(corners) in
    let edge_source_sides = Bytes.make edge_source_count '\000'
    and edge_source_edges = Array.make edge_source_count 0
    and next_edge_source = ref 0 in
    let old_local = function 0 -> 2 | 1 -> 1 | _ -> 0 in
    for primitive = 0 to primitives - 1 do
      if primitive land 4095 = 0 then Cancel.check_opt cancel;
      let offset = primitive * 3 in
      vertex_points.(offset) <- source_topology.vertex_points.(offset);
      vertex_points.(offset + 1) <- source_topology.vertex_points.(offset + 2);
      vertex_points.(offset + 2) <- source_topology.vertex_points.(offset + 1);
      corner_complex_vertices.(offset) <- value.corner_complex_vertices.(offset);
      corner_complex_vertices.(offset + 1) <-
        value.corner_complex_vertices.(offset + 2);
      corner_complex_vertices.(offset + 2) <-
        value.corner_complex_vertices.(offset + 1);
      barycentric_a.(offset) <- value.barycentric_a.(offset);
      barycentric_a.(offset + 1) <- value.barycentric_a.(offset + 2);
      barycentric_a.(offset + 2) <- value.barycentric_a.(offset + 1);
      barycentric_b.(offset) <- value.barycentric_b.(offset);
      barycentric_b.(offset + 1) <- value.barycentric_b.(offset + 2);
      barycentric_b.(offset + 2) <- value.barycentric_b.(offset + 1);
      barycentric_c.(offset) <- value.barycentric_c.(offset);
      barycentric_c.(offset + 1) <- value.barycentric_c.(offset + 2);
      barycentric_c.(offset + 2) <- value.barycentric_c.(offset + 1);
      Bytes.unsafe_set primitive_windings primitive
        (if Bytes.unsafe_get primitive_windings primitive = '\000'
         then '\001' else '\000');
      for local = 0 to 2 do
        let target = offset + local in
        edge_source_offsets.(target) <- !next_edge_source;
        let source = offset + old_local local in
        let first = value.edge_source_offsets.(source)
        and last = value.edge_source_offsets.(source + 1) in
        let count = last - first in
        Bytes.blit value.edge_source_sides first edge_source_sides
          !next_edge_source count;
        Array.blit value.edge_source_edges first edge_source_edges
          !next_edge_source count;
        next_edge_source := !next_edge_source + count
      done
    done;
    edge_source_offsets.(corners) <- !next_edge_source;
    if !next_edge_source <> edge_source_count then
      invalid_arg "Boolean reversed edge ancestry changed cardinality";
    match Topology.polygons_owned ~point_count:(Geometry.point_count value.geometry)
        ~vertex_points
        ~primitive_offsets:(Array.init (primitives + 1) (fun primitive -> primitive * 3))
    with
    | Error message -> error "invalid_output" message
    | Ok topology ->
        (match Geometry.create ~positions:(Geometry.positions value.geometry)
            ~topology () with
         | Error message -> error "invalid_output" message
         | Ok geometry -> Ok {
             value with geometry; corner_complex_vertices; primitive_windings;
             barycentric_a; barycentric_b; barycentric_c;
             edge_source_offsets; edge_source_sides; edge_source_edges;
           })
  with
  | Cancel.Cancelled -> error "cancelled" "Boolean ancestry reversal was cancelled"
  | Invalid_argument message -> error "invalid_output" message

let concatenate_ancestries ?cancel values =
  try
    Cancel.check_opt cancel;
    if Array.length values = 0 then
      invalid_arg "Boolean ancestry concatenation requires at least one product";
    let first = values.(0) and complex = values.(0).complex in
    let total_primitives = ref 0 and total_edge_sources = ref 0 in
    Array.iteri (fun index value ->
      if value.complex != complex then
        invalid_arg "Boolean products belong to different exact complexes";
      let primitives = Geometry.primitive_count value.geometry in
      if !total_primitives > Sys.max_array_length - primitives then
        invalid_arg "Boolean concatenated primitive cardinality exceeds array limits";
      total_primitives := !total_primitives + primitives;
      let sources = value.edge_source_offsets.
          (Array.length value.edge_source_offsets - 1) in
      if !total_edge_sources > Sys.max_array_length - sources then
        invalid_arg "Boolean concatenated edge ancestry exceeds array limits";
      total_edge_sources := !total_edge_sources + sources;
      if index land 255 = 0 then Cancel.check_opt cancel) values;
    if !total_primitives > Sys.max_array_length / 3 then
      invalid_arg "Boolean concatenated corner cardinality exceeds array limits";
    let used = Bytes.make (Boolean_complex.vertex_count complex) '\000' in
    Array.iter (fun value ->
      Array.iteri (fun point output ->
        if output >= 0 then Bytes.unsafe_set used point '\001')
        value.complex_output_points) values;
    let complex_to_output = Array.make (Bytes.length used) (-1)
    and point_count = ref 0 in
    for point = 0 to Bytes.length used - 1 do
      if Bytes.unsafe_get used point <> '\000' then begin
        complex_to_output.(point) <- !point_count;
        incr point_count
      end
    done;
    let x = Array.make !point_count 0. and y = Array.make !point_count 0.
    and z = Array.make !point_count 0.
    and materialized_complex_vertices = Array.make !point_count 0 in
    for point = 0 to Array.length complex_to_output - 1 do
      let output = complex_to_output.(point) in
      if output >= 0 then begin
        materialized_complex_vertices.(output) <- point;
        let px, py, pz = Boolean_complex.approximate_vertex complex point in
        x.(output) <- px; y.(output) <- py; z.(output) <- pz
      end
    done;
    let x, y, z, _point_complex_vertices, old_to_new =
      coalesce_positions ~complex_vertices:materialized_complex_vertices ~x ~y ~z in
    for point = 0 to Array.length complex_to_output - 1 do
      let output = complex_to_output.(point) in
      if output >= 0 then complex_to_output.(point) <- old_to_new.(output)
    done;
    point_count := Array.length x;
    let complex_output_points = Array.copy complex_to_output in
    let corners = !total_primitives * 3 in
    let vertex_points = Array.make corners 0
    and corner_complex_vertices = Array.make corners 0
    and primitive_complex_facets = Array.make !total_primitives 0
    and primitive_sides = Bytes.make !total_primitives '\000'
    and primitive_faces = Array.make !total_primitives 0
    and primitive_triangles = Array.make !total_primitives 0
    and primitive_refined_triangles = Array.make !total_primitives 0
    and primitive_windings = Bytes.make !total_primitives '\000'
    and primitive_source_points = Array.make corners 0
    and primitive_source_vertices = Array.make corners 0
    and barycentric_a = Array.make corners 0.
    and barycentric_b = Array.make corners 0.
    and barycentric_c = Array.make corners 0.
    and edge_source_offsets = Array.make (corners + 1) 0
    and edge_source_sides = Bytes.make !total_edge_sources '\000'
    and edge_source_edges = Array.make !total_edge_sources 0 in
    let primitive_base = ref 0 and edge_source_base = ref 0 in
    Array.iter (fun value ->
      let primitives = Geometry.primitive_count value.geometry in
      for corner = 0 to primitives * 3 - 1 do
        vertex_points.((!primitive_base * 3) + corner) <-
          complex_to_output.(value.corner_complex_vertices.(corner));
        corner_complex_vertices.((!primitive_base * 3) + corner) <-
          value.corner_complex_vertices.(corner)
      done;
      Array.blit value.primitive_complex_facets 0 primitive_complex_facets
        !primitive_base primitives;
      Bytes.blit value.primitive_sides 0 primitive_sides !primitive_base primitives;
      Array.blit value.primitive_faces 0 primitive_faces !primitive_base primitives;
      Array.blit value.primitive_triangles 0 primitive_triangles !primitive_base primitives;
      Array.blit value.primitive_refined_triangles 0 primitive_refined_triangles
        !primitive_base primitives;
      Bytes.blit value.primitive_windings 0 primitive_windings
        !primitive_base primitives;
      let corner_base = !primitive_base * 3 and part_corners = primitives * 3 in
      Array.blit value.primitive_source_points 0 primitive_source_points
        corner_base part_corners;
      Array.blit value.primitive_source_vertices 0 primitive_source_vertices
        corner_base part_corners;
      Array.blit value.barycentric_a 0 barycentric_a corner_base part_corners;
      Array.blit value.barycentric_b 0 barycentric_b corner_base part_corners;
      Array.blit value.barycentric_c 0 barycentric_c corner_base part_corners;
      for corner = 0 to part_corners - 1 do
        edge_source_offsets.(corner_base + corner) <-
          !edge_source_base + value.edge_source_offsets.(corner)
      done;
      let source_count = value.edge_source_offsets.
          (Array.length value.edge_source_offsets - 1) in
      Bytes.blit value.edge_source_sides 0 edge_source_sides !edge_source_base
        source_count;
      Array.blit value.edge_source_edges 0 edge_source_edges !edge_source_base
        source_count;
      primitive_base := !primitive_base + primitives;
      edge_source_base := !edge_source_base + source_count;
      Cancel.check_opt cancel) values;
    edge_source_offsets.(corners) <- !total_edge_sources;
    let topology = Topology.polygons_owned ~point_count:!point_count ~vertex_points
        ~primitive_offsets:(Array.init (!total_primitives + 1)
          (fun primitive -> primitive * 3)) in
    (match topology with
     | Error message -> error "invalid_output" message
     | Ok topology ->
         match Geometry.create
             ~positions:(Packed.Float3.Private.of_owned_exn ~x ~y ~z)
             ~topology () with
         | Error message -> error "invalid_output" message
         | Ok geometry -> Ok {
             complex; geometry;
             left_geometry = first.left_geometry;
             right_geometry = first.right_geometry;
              complex_output_points;
             corner_complex_vertices; primitive_complex_facets;
             primitive_sides; primitive_faces; primitive_triangles;
             primitive_refined_triangles; primitive_windings;
             primitive_source_points; primitive_source_vertices;
             barycentric_a; barycentric_b; barycentric_c;
             edge_source_offsets; edge_source_sides; edge_source_edges;
           })
  with
  | Cancel.Cancelled -> error "cancelled" "Boolean product concatenation was cancelled"
  | Invalid_argument message -> error "invalid_output" message

module Private = struct
  type nonrec barycentric_cache = barycentric_cache
  type ancestry_view = {

    corner_complex_vertices : int array;

    primitive_sides : bytes;
    primitive_triangles : int array;
    primitive_source_points : int array;
    primitive_source_vertices : int array;
    barycentric_a : float array;
    barycentric_b : float array;
    barycentric_c : float array;
    edge_source_offsets : int array;
    edge_source_sides : bytes;
    edge_source_edges : int array;
  }
  let ancestry_view (value : ancestry) = {

    corner_complex_vertices = value.corner_complex_vertices;

    primitive_sides = value.primitive_sides;
    primitive_triangles = value.primitive_triangles;
    primitive_source_points = value.primitive_source_points;
    primitive_source_vertices = value.primitive_source_vertices;
    barycentric_a = value.barycentric_a;
    barycentric_b = value.barycentric_b;
    barycentric_c = value.barycentric_c;
    edge_source_offsets = value.edge_source_offsets;
    edge_source_sides = value.edge_source_sides;
    edge_source_edges = value.edge_source_edges;
  }
  let validate_positions = validate_positions
  let validate_materialized = validate_materialized
  let coalesce_positions = coalesce_positions
  let validate_closed_topology = validate_closed_topology
  let barycentric_cache ~capacity = barycentric_cache capacity
  let complex value = value.complex
  let complex_output_point (value : ancestry) point =
    value.complex_output_points.(point)
  let primitive_complex_facet (value : ancestry) primitive =
    value.primitive_complex_facets.(primitive)
  let left_geometry value = value.left_geometry
  let right_geometry value = value.right_geometry
  let build_selected_with_ancestry = build_selected_with_ancestry
  let reverse_ancestry = reverse_ancestry
  let concatenate_ancestries = concatenate_ancestries
end
