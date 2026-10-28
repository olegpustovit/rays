let operation = "boolean_materialization"
let error code message = Error (Error.make ~operation ~code message)

let edge_length positions a b =
  Float.hypot
    (positions.Packed.Float3.View.x.(a) -. positions.x.(b))
    (Float.hypot
       (positions.y.(a) -. positions.y.(b))
       (positions.z.(a) -. positions.z.(b)))

let primitive_min_edge_length positions topology primitive =
  let first = topology.Topology.View.primitive_offsets.(primitive)
  and last = topology.primitive_offsets.(primitive + 1) in
  let minimum = ref infinity in
  for vertex = first to last - 1 do
    let next = if vertex + 1 = last then first else vertex + 1 in
    minimum := min !minimum (edge_length positions
      topology.vertex_points.(vertex) topology.vertex_points.(next))
  done;
  !minimum

let primitive_is_triangle topology primitive =
  Bytes.unsafe_get topology.Topology.View.primitive_kinds primitive = '\000'
  && topology.primitive_offsets.(primitive + 1)
     - topology.primitive_offsets.(primitive) = 3

let link_condition index topology stamps stamp edge a b =
  let first = index.Topology_index.View.edge_offsets.(edge) in
  let v0 = index.edge_vertices.(first) and v1 = index.edge_vertices.(first + 1) in
  let opposite vertex =
    let next = index.next_vertex.(vertex) in
    topology.Topology.View.vertex_points.(index.next_vertex.(next)) in
  let c = opposite v0 and d = opposite v1 in
  let first_a = index.point_edge_offsets.(a)
  and last_a = index.point_edge_offsets.(a + 1) in
  for local = first_a to last_a - 1 do
    let candidate = index.point_edges.(local) in
    let x = index.edge_a.(candidate) and y = index.edge_b.(candidate) in
    let neighbor = if x = a then y else x in
    if neighbor <> a then stamps.(neighbor) <- stamp
  done;
  let common = ref 0 and valid = ref true in
  let first_b = index.point_edge_offsets.(b)
  and last_b = index.point_edge_offsets.(b + 1) in
  for local = first_b to last_b - 1 do
    let candidate = index.point_edges.(local) in
    let x = index.edge_a.(candidate) and y = index.edge_b.(candidate) in
    let neighbor = if x = b then y else x in
    if neighbor <> b && stamps.(neighbor) = stamp then begin
      incr common;
      if neighbor <> c && neighbor <> d then valid := false
    end
  done;
  !valid && !common = (if c = d then 1 else 2)

let same_or_zero first second =
  first = Predicates.Zero || second = Predicates.Zero || first = second

let contraction_preserves_orientation positions topology primitive keep remove =
  let first = topology.Topology.View.primitive_offsets.(primitive) in
  let original local = topology.vertex_points.(first + local) in
  let contracted local =
    let point = original local in if point = remove then keep else point in
  let oa = original 0 and ob = original 1 and oc = original 2
  and na = contracted 0 and nb = contracted 1 and nc = contracted 2 in
  if na = nb || nb = nc || nc = na then false
  else
    let x = positions.Packed.Float3.View.x and y = positions.y
    and z = positions.z in
    let oxy = Predicates.orient2d_packed ~x ~y oa ob oc
    and nxy = Predicates.orient2d_packed ~x ~y na nb nc
    and oyz = Predicates.orient2d_packed ~x:y ~y:z oa ob oc
    and nyz = Predicates.orient2d_packed ~x:y ~y:z na nb nc
    and ozx = Predicates.orient2d_packed ~x:z ~y:x oa ob oc
    and nzx = Predicates.orient2d_packed ~x:z ~y:x na nb nc in
    same_or_zero oxy nxy && same_or_zero oyz nyz && same_or_zero ozx nzx
    && ((oxy <> Predicates.Zero && oxy = nxy)
        || (oyz <> Predicates.Zero && oyz = nyz)
        || (ozx <> Predicates.Zero && ozx = nzx))

let safe_independent_edges ?cancel ~grain ?(keep_greatest = false)
    candidates geometry =
  if grain <= 0 then error "invalid_parameter" "grain must be positive"
  else try
    Cancel.check_opt cancel;
    let topology_value = Geometry.topology geometry in
    let index_value = Topology_index.create ?cancel topology_value in
    let index = Topology_index.View.view index_value
    and topology = Topology.View.view topology_value
    and positions = Packed.Float3.View.view (Geometry.positions geometry) in
    let edge_count = Array.length index.edge_a
    and point_count = Geometry.point_count geometry
    and primitive_count = Geometry.primitive_count geometry in
    if Edge_group.topology_data_id candidates <> Topology.data_id topology_value
        || Edge_group.length candidates <> edge_count then
      invalid_arg "Boolean edge candidates belong to a different topology";
    let count = Edge_group.cardinality candidates in
    let ordered = Array.make count 0 and cursor = ref 0
    and lengths = Array.make edge_count infinity in
    Edge_group.iter (fun edge ->
      ordered.(!cursor) <- edge;
      lengths.(edge) <- edge_length positions index.edge_a.(edge) index.edge_b.(edge);
      incr cursor) candidates;
    Array.sort (fun left right ->
      let compared = Float.compare lengths.(left) lengths.(right) in
      if compared <> 0 then compared else Int.compare left right) ordered;
    let chosen = Bytes.make ((edge_count + 7) / 8) '\000'
    and used_points = Bytes.make ((point_count + 7) / 8) '\000'
    and used_primitives = Bytes.make ((primitive_count + 7) / 8) '\000'
    and neighbor_stamps = Array.make point_count (-1) in
    Array.iteri (fun stamp edge ->
      Cancel.poll cancel stamp 4095;
      let incidence_first = index.edge_offsets.(edge)
      and incidence_last = index.edge_offsets.(edge + 1) in
      if incidence_last - incidence_first = 2 then begin
        let a = index.edge_a.(edge) and b = index.edge_b.(edge) in
        let keep = if keep_greatest then max a b else min a b
        and remove = if keep_greatest then min a b else max a b in
        let primitive_a = index.primitive_of_vertex.(
            index.edge_vertices.(incidence_first))
        and primitive_b = index.primitive_of_vertex.(
            index.edge_vertices.(incidence_first + 1)) in
        if primitive_a <> primitive_b
            && primitive_is_triangle topology primitive_a
            && primitive_is_triangle topology primitive_b
            && not (Support.Bits.mem used_points a || Support.Bits.mem used_points b
              || Support.Bits.mem used_primitives primitive_a
              || Support.Bits.mem used_primitives primitive_b)
            && link_condition index topology neighbor_stamps stamp edge a b then begin
          let valid = ref true in
          let first = index.point_offsets.(remove)
          and last = index.point_offsets.(remove + 1) in
          let local = ref first in
          while !valid && !local < last do
            let primitive = index.primitive_of_vertex.(index.point_vertices.(!local)) in
            if primitive <> primitive_a && primitive <> primitive_b then
              if Support.Bits.mem used_primitives primitive
                  || not (primitive_is_triangle topology primitive)
                  || not (contraction_preserves_orientation positions topology
                    primitive keep remove) then valid := false;
            incr local
          done;
          if !valid then begin
            Support.Bits.set chosen edge;
            Support.Bits.set used_points a; Support.Bits.set used_points b;
            let reserve point =
              let first = index.point_offsets.(point)
              and last = index.point_offsets.(point + 1) in
              for local = first to last - 1 do
                Support.Bits.set used_primitives
                  index.primitive_of_vertex.(index.point_vertices.(local))
              done in
            reserve a; reserve b
          end
        end
      end) ordered;
    Ok (Edge_group.View.of_owned_bits ~topology:topology_value ~edge_count
      ~name:"__rdk_boolean_safe_tiny_seam" chosen)
  with
  | Cancel.Cancelled -> error "cancelled" "Boolean contraction planning was cancelled"
  | Invalid_argument message -> error "invalid_input" message

let speculative_independent_edges ?cancel ~grain candidates geometry =
  if grain <= 0 then error "invalid_parameter" "grain must be positive"
  else try
    Cancel.check_opt cancel;
    let topology = Geometry.topology geometry in
    let index_value = Topology_index.create ?cancel topology in
    let index = Topology_index.View.view index_value
    and positions = Packed.Float3.View.view (Geometry.positions geometry) in
    if Edge_group.topology_data_id candidates <> Topology.data_id topology
        || Edge_group.length candidates <> Array.length index.edge_a then
      invalid_arg "Boolean speculative candidates belong to a different topology";
    let ordered = Array.make (Edge_group.cardinality candidates) 0
    and cursor = ref 0 and lengths = Array.make (Array.length index.edge_a) infinity in
    Edge_group.iter (fun edge ->
      ordered.(!cursor) <- edge;
      lengths.(edge) <- edge_length positions index.edge_a.(edge) index.edge_b.(edge);
      incr cursor) candidates;
    Array.sort (fun first second ->
      let compared = Float.compare lengths.(first) lengths.(second) in
      if compared <> 0 then compared else Int.compare first second) ordered;
    let chosen = Bytes.make ((Array.length index.edge_a + 7) / 8) '\000'
    and used_primitives = Bytes.make
        ((Geometry.primitive_count geometry + 7) / 8) '\000' in
    Array.iter (fun edge ->
      let a = index.edge_a.(edge) and b = index.edge_b.(edge)
      and available = ref true in
      let inspect point =
        let first = index.point_offsets.(point)
        and last = index.point_offsets.(point + 1) in
        for slot = first to last - 1 do
          let primitive = index.primitive_of_vertex.(index.point_vertices.(slot)) in
          if Support.Bits.mem used_primitives primitive then available := false
        done in
      inspect a; inspect b;
      if !available then begin
        Support.Bits.set chosen edge;
        let reserve point =
          let first = index.point_offsets.(point)
          and last = index.point_offsets.(point + 1) in
          for slot = first to last - 1 do
            Support.Bits.set used_primitives
              index.primitive_of_vertex.(index.point_vertices.(slot))
          done in
        reserve a; reserve b
      end) ordered;
    Ok (Edge_group.View.of_owned_bits ~topology
      ~edge_count:(Array.length index.edge_a)
      ~name:"__rdk_boolean_speculative_independent" chosen)
  with
  | Cancel.Cancelled -> error "cancelled"
      "Boolean speculative contraction planning was cancelled"
  | Invalid_argument message -> error "invalid_input" message

let surface_seam_edges ?cancel ~grain ancestry seam =
  if grain <= 0 then error "invalid_parameter" "grain must be positive"
  else try
    Cancel.check_opt cancel;
    let complex = Boolean_extract.View.complex ancestry in
    if Boolean_seam.View.complex seam != complex then
      invalid_arg "Boolean seam and extraction belong to different exact complexes";
    let geometry = Boolean_extract.geometry ancestry in
    let topology = Geometry.topology geometry in
    let index = Topology_index.create ?cancel topology in
    let builder = Edge_group.Builder.create ~topology ~index
        ~name:"__rdk_boolean_exact_seam" in
    for complex_edge = 0 to Boolean_complex.edge_count complex - 1 do
      Cancel.poll cancel complex_edge 4095;
      if Boolean_seam.View.is_seam_edge seam complex_edge then begin
        let a = Boolean_extract.View.complex_output_point ancestry
            (Boolean_complex.edge_first complex complex_edge)
        and b = Boolean_extract.View.complex_output_point ancestry
            (Boolean_complex.edge_second complex complex_edge) in
        if a >= 0 && b >= 0 then begin
          let edge = Topology_index.find_edge_index index ~a ~b in
          if edge >= 0 then Edge_group.Builder.set builder edge true
        end
      end
    done;
    Ok (Edge_group.Builder.freeze builder)
  with
  | Cancel.Cancelled -> error "cancelled" "Boolean seam-edge mapping was cancelled"
  | Invalid_argument message -> error "invalid_input" message

let empty_geometry () =
  match Geometry.create
      ~positions:(Packed.Float3.View.of_owned_exn
        ~x:[||] ~y:[||] ~z:[||])
      ~topology:(Topology.empty ~point_count:0) () with
  | Ok geometry -> geometry
  | Error message -> invalid_arg message

let opposite_duplicate topology first second =
  let first_offset = topology.Topology.View.primitive_offsets.(first)
  and second_offset = topology.primitive_offsets.(second) in
  if topology.primitive_offsets.(first + 1) - first_offset <> 3
      || topology.primitive_offsets.(second + 1) - second_offset <> 3 then false
  else
    let a = topology.vertex_points.(first_offset)
    and b = topology.vertex_points.(first_offset + 1)
    and c = topology.vertex_points.(first_offset + 2)
    and x = topology.vertex_points.(second_offset)
    and y = topology.vertex_points.(second_offset + 1)
    and z = topology.vertex_points.(second_offset + 2) in
    (a = x && b = z && c = y)
    || (a = y && b = x && c = z)
    || (a = z && b = y && c = x)

let triangle_point_key topology primitive =
  let first = topology.Topology.View.primitive_offsets.(primitive) in
  let a = topology.vertex_points.(first)
  and b = topology.vertex_points.(first + 1)
  and c = topology.vertex_points.(first + 2) in
  if a <= b then
    if b <= c then a, b, c
    else if a <= c then a, c, b else c, a, b
  else if a <= c then b, a, c
  else if b <= c then b, c, a else c, b, a

let stable_counting_pass ~key_count keys values scratch counts =
  Array.fill counts 0 key_count 0;
  for slot = 0 to Array.length values - 1 do
    let key = keys.(values.(slot)) in
    if key < 0 || key >= key_count then
      invalid_arg "Boolean duplicate-facet point key is out of range";
    counts.(key) <- counts.(key) + 1
  done;
  let total = ref 0 in
  for key = 0 to key_count - 1 do
    let count = counts.(key) in
    counts.(key) <- !total;
    total := !total + count
  done;
  for slot = 0 to Array.length values - 1 do
    let primitive = values.(slot) and key = keys.(values.(slot)) in
    scratch.(counts.(key)) <- primitive;
    counts.(key) <- counts.(key) + 1
  done;
  Array.blit scratch 0 values 0 (Array.length values)

let verify_duplicate_triangles ~allow_opposite_duplicates topology =
  let primitives = Bytes.length topology.Topology.View.primitive_kinds in
  let key_a = Array.make primitives 0 and key_b = Array.make primitives 0
  and key_c = Array.make primitives 0 in
  for primitive = 0 to primitives - 1 do
    let a, b, c = triangle_point_key topology primitive in
    key_a.(primitive) <- a; key_b.(primitive) <- b; key_c.(primitive) <- c
  done;
  let ordered = Array.init primitives Fun.id
  and scratch = Array.make primitives 0
  and counts = Array.make topology.point_count 0 in
  stable_counting_pass ~key_count:topology.point_count key_c ordered scratch counts;
  stable_counting_pass ~key_count:topology.point_count key_b ordered scratch counts;
  stable_counting_pass ~key_count:topology.point_count key_a ordered scratch counts;
  let same first second =
    key_a.(first) = key_a.(second)
    && key_b.(first) = key_b.(second)
    && key_c.(first) = key_c.(second) in
  let failure = ref None and first = ref 0 in
  while Option.is_none !failure && !first < primitives do
    let last = ref (!first + 1) in
    while !last < primitives && same ordered.(!first) ordered.(!last) do
      incr last
    done;
    let count = !last - !first in
    if count > 1 then begin
      let a = ordered.(!first) and b = ordered.(!first + 1) in
      if count <> 2 || not allow_opposite_duplicates
          || not (opposite_duplicate topology a b) then
        failure := Some (a, b)
    end;
    first := !last
  done;
  match !failure with
  | None -> Ok ()
  | Some (first, second) -> error "surface_self_intersection" (Printf.sprintf
      "rounded Boolean triangles %d and %d duplicate a surface facet"
      first second)

let verify_extracted_contacts ?cancel ~grain positions topology geometry =
  match Surface_index.View.create_validated_triangles ?cancel ~grain geometry with
  | Error _ as failure -> failure
  | Ok surface ->
      let first_candidates, second_candidates =
        Surface_index.View.self_pairs_disjoint_topology
          ?cancel ~grain ~tolerance:0. surface in
      let candidate_count = Array.length first_candidates in
      let ranges = (candidate_count + grain - 1) / grain in
      let failures = Array.make ranges (-1) and failure_kinds = Bytes.make ranges '\000' in
      Support.run_indexed_ranges ~grain:grain candidate_count (fun ~range:range ~first:first ~last:last ->
        let scratch = Array.make 4 0 in

        let candidate = ref first in
        while failures.(range) < 0 && !candidate < last do
          Cancel.poll cancel !candidate 4095;
          let left = first_candidates.(!candidate)
          and right = second_candidates.(!candidate) in
          let left_a = Surface_index.View.triangle_point surface left 0
          and left_b = Surface_index.View.triangle_point surface left 1
          and left_c = Surface_index.View.triangle_point surface left 2
          and right_a = Surface_index.View.triangle_point surface right 0
          and right_b = Surface_index.View.triangle_point surface right 1
          and right_c = Surface_index.View.triangle_point surface right 2 in
          let count = Predicates.View.triangle_triangle_features_into
              ~x:positions.Packed.Float3.View.x ~y:positions.y ~z:positions.z
              ~left_a ~left_b ~left_c ~right_a ~right_b ~right_c scratch in
          if count = -2 then begin
            failures.(range) <- !candidate;
            Bytes.unsafe_set failure_kinds range '\001'
          end else if count > 0 then begin
            failures.(range) <- !candidate;
            Bytes.unsafe_set failure_kinds range '\002'
          end else if count = -1
              && Predicates.View.coplanar_triangles_contact_packed
                   ~x:positions.x ~y:positions.y ~z:positions.z
                   ~left_a ~left_b ~left_c ~right_a ~right_b ~right_c then begin
            failures.(range) <- !candidate;
            Bytes.unsafe_set failure_kinds range '\003'
          end;
          incr candidate
        done) ;
      let failure_range = ref (-1) and range = ref 0 in
      while !failure_range < 0 && !range < ranges do
        if failures.(!range) >= 0 then failure_range := !range;
        incr range
      done;
      if !failure_range < 0 then Ok ()
      else begin
        let candidate = failures.(!failure_range) in
        let first_triangle = first_candidates.(candidate)
        and second_triangle = second_candidates.(candidate) in
        let first = Surface_index.View.triangle_primitive surface first_triangle
        and second = Surface_index.View.triangle_primitive surface second_triangle in
        match Bytes.unsafe_get failure_kinds !failure_range with
        | '\001' -> error "rounding_degenerate"
            "rounded Boolean surface contains a degenerate triangle pair"
        | '\002' ->
            let first_min = primitive_min_edge_length positions topology first
            and second_min = primitive_min_edge_length positions topology second in
            error "surface_self_intersection" (Printf.sprintf
              "rounded Boolean triangles %d and %d intersect away from ordinary shared topology (minimum edge lengths %.17g and %.17g)"
              first second first_min second_min)
        | _ -> error "surface_self_intersection" (Printf.sprintf
            "rounded Boolean triangles %d and %d have non-adjacent coplanar contact"
            first second)
      end

let verify_constraint_contacts ?cancel ~grain ~allow_opposite_duplicates
    positions topology constraints =
  if Boolean_constraints.degenerate_pair_count constraints > 0 then
    error "rounding_degenerate"
      "rounded Boolean surface contains a degenerate triangle pair"
  else if Boolean_constraints.constraint_count constraints > 0 then
    let first = Boolean_constraints.constraint_first_triangle constraints 0
    and second = Boolean_constraints.constraint_second_triangle constraints 0 in
    let first_min = primitive_min_edge_length positions topology first
    and second_min = primitive_min_edge_length positions topology second in
    error "surface_self_intersection" (Printf.sprintf
      "rounded Boolean triangles %d and %d intersect away from ordinary shared topology (minimum edge lengths %.17g and %.17g)"
      first second first_min second_min)
  else if Boolean_constraints.coplanar_pair_count constraints = 0 then Ok ()
  else
    match Boolean_coplanar.build ?cancel ~grain constraints with
    | Error _ as failure -> failure
    | Ok coplanar ->
        let contact = ref (-1) and pair = ref 0 in
        while !contact < 0 && !pair < Boolean_coplanar.pair_count coplanar do
          if Boolean_coplanar.kind coplanar !pair <> Boolean_coplanar.Empty then begin
            let first = Boolean_coplanar.first_triangle coplanar !pair
            and second = Boolean_coplanar.second_triangle coplanar !pair in
            if not (allow_opposite_duplicates
                && opposite_duplicate topology first second) then
              contact := !pair
          end;
          incr pair
        done;
        if !contact < 0 then Ok ()
        else
          let first = Boolean_coplanar.first_triangle coplanar !contact
          and second = Boolean_coplanar.second_triangle coplanar !contact in
          error "surface_self_intersection" (Printf.sprintf
            "rounded Boolean triangles %d and %d have non-adjacent coplanar contact"
            first second)

let verify_extracted_orientations ?cancel ~grain ancestry geometry =
  let positions = Packed.Float3.View.view (Geometry.positions geometry)
  and topology = Topology.View.view (Geometry.topology geometry)
  and view = Boolean_extract.View.ancestry_view ancestry
  and complex = Boolean_extract.View.complex ancestry in
  let primitives = Geometry.primitive_count geometry in
  if Array.length view.corner_complex_vertices <> primitives * 3
      || Bytes.length view.primitive_sides <> primitives
      || Array.length view.primitive_triangles <> primitives then
    error "geometry_mismatch"
      "Boolean extraction corner ancestry has the wrong cardinality"
  else begin
    let constraints = Boolean_complex.View.constraints complex in
    let source = Boolean_constraints.View.source constraints in
    let ranges = (primitives + grain - 1) / grain in
    let failures = Array.make ranges (-1) in
    Support.run_indexed_ranges ~grain primitives (fun ~range ~first:first ~last:last ->
      let primitive = ref first in
      while failures.(range) < 0 && !primitive < last do
        Cancel.poll cancel !primitive 4095;
        let offset = !primitive * 3 in
        let a = topology.vertex_points.(offset)
        and b = topology.vertex_points.(offset + 1)
        and c = topology.vertex_points.(offset + 2)
        and ea = Boolean_complex.View.vertex complex
            view.corner_complex_vertices.(offset)
        and eb = Boolean_complex.View.vertex complex
            view.corner_complex_vertices.(offset + 1)
        and ec = Boolean_complex.View.vertex complex
            view.corner_complex_vertices.(offset + 2) in
        let triangle = view.primitive_triangles.(!primitive) in
        let triangle_point =
          if Bytes.unsafe_get view.primitive_sides !primitive = '\000'
          then Boolean_constraints.View.left_triangle_point
          else Boolean_constraints.View.right_triangle_point in
        let sa = triangle_point constraints triangle 0
        and sb = triangle_point constraints triangle 1
        and sc = triangle_point constraints triangle 2 in
        let exact, rounded = match
            Implicit_point.source_triangle_projection source sa sb sc with
          | Implicit_point.XY ->
              Implicit_point.orient2d_xy ea eb ec,
              Predicates.orient2d_packed ~x:positions.x ~y:positions.y a b c
          | Implicit_point.YZ ->
              Implicit_point.orient2d_yz ea eb ec,
              Predicates.orient2d_packed ~x:positions.y ~y:positions.z a b c
          | Implicit_point.ZX ->
              Implicit_point.orient2d_zx ea eb ec,
              Predicates.orient2d_packed ~x:positions.z ~y:positions.x a b c in
        if exact = Predicates.Zero || exact <> rounded then
          failures.(range) <- !primitive;
        incr primitive
      done);
    let failure = ref (-1) in
    for range = 0 to ranges - 1 do
      if !failure < 0 && failures.(range) >= 0 then failure := failures.(range)
    done;
    if !failure < 0 then Ok ()
    else error "rounding_orientation" (Printf.sprintf
        "rounded Boolean triangle %d changed its exact extraction orientation"
        !failure)
  end

let verify_extracted_surface ?cancel ~grain ~require_closed
    ~allow_opposite_duplicates ancestry geometry =
  let extracted = Boolean_extract.geometry ancestry in
  if Geometry.positions geometry != Geometry.positions extracted
      || Geometry.topology geometry != Geometry.topology extracted then
    error "geometry_mismatch"
      "fast Boolean verification requires unchanged extraction geometry"
  else try
    Cancel.check_opt cancel;
    let positions = Packed.Float3.View.view (Geometry.positions geometry)
    and topology_value = Geometry.topology geometry in
    let topology = Topology.View.view topology_value in
    match Boolean_extract.View.validate_materialized
        ~x:positions.x ~y:positions.y ~z:positions.z
        ~vertex_points:topology.vertex_points () with
    | Error failure -> error (Error.code failure)
        ("rounded surface verification failed: " ^ Error.to_string failure)
    | Ok () ->
        (match if require_closed
            then Boolean_extract.View.validate_closed_topology topology_value
            else Ok () with
         | Error _ as failure -> failure
         | Ok () ->
             (match verify_extracted_orientations ?cancel ~grain ancestry geometry with
              | Error _ as failure -> failure
              | Ok () ->
                  (match verify_duplicate_triangles ~allow_opposite_duplicates
                      topology with
                   | Error _ as failure -> failure
                   | Ok () ->
                       verify_extracted_contacts ?cancel ~grain positions topology
                         geometry)))
  with
  | Cancel.Cancelled -> error "cancelled"
      "Boolean materialization verification was cancelled"
  | Invalid_argument message -> error "invalid_output" message

let verify_surface ?cancel ~grain ~require_closed
    ?(allow_opposite_duplicates = false) geometry =
  if grain <= 0 then error "invalid_parameter" "grain must be positive"
  else try
    Cancel.check_opt cancel;
    let positions = Packed.Float3.View.view (Geometry.positions geometry)
    and topology = Geometry.topology geometry in
    let view = Topology.View.view topology in
    (match Boolean_extract.View.validate_materialized
        ~x:positions.x ~y:positions.y ~z:positions.z
        ~vertex_points:view.vertex_points () with
     | Error failure -> error (Error.code failure)
         ("rounded surface verification failed: " ^ Error.to_string failure)
     | Ok () ->
         (match if require_closed
             then Boolean_extract.View.validate_closed_topology topology
             else Ok () with
          | Error _ as failure -> failure
          | Ok () ->
              match Boolean_constraints.build ?cancel
                  ~resolve_left_self_intersections:true ~grain
                  ~skip_opposite_self_pairs:allow_opposite_duplicates
                  ~left:geometry ~right:(empty_geometry ()) () with
              | Error _ as failure -> failure
              | Ok constraints ->
                  verify_constraint_contacts ?cancel ~grain
                    ~allow_opposite_duplicates positions view constraints))
  with
  | Cancel.Cancelled -> error "cancelled"
      "Boolean materialization verification was cancelled"
  | Invalid_argument message -> error "invalid_output" message

type cleanup = {
  cleanup_geometry : Geometry.t;
  cleanup_candidate_count : int;
  cleanup_collapsed_count : int;
  cleanup_batch_count : int;

  cleanup_rounding_repaired_count : int;
  cleanup_point_sources : int array;
  cleanup_vertex_sources : int array;
  cleanup_primitive_sources : int array;
  cleanup_seam_edges : Edge_group.t;
}

let cleanup_geometry value = value.cleanup_geometry
let cleanup_primitive_source value primitive = value.cleanup_primitive_sources.(primitive)

let verify_materialized_seam_curves ?cancel ~grain seam =
  if Boolean_seam.View.curves_materialized seam then
    Boolean_seam.View.verify_curves ?cancel ~grain (Boolean_seam.curves seam)
  else Ok ()

let fresh_attribute_name geometry base =
  let used name =
    Geometry.find_attribute ~owner:Attribute.Point name geometry <> None
    || Geometry.find_attribute ~owner:Attribute.Vertex name geometry <> None
    || Geometry.find_attribute ~owner:Attribute.Primitive name geometry <> None
    || Geometry.find_attribute ~owner:Attribute.Detail name geometry <> None in
  let rec choose suffix =
    let name = if suffix = 0 then base else base ^ string_of_int suffix in
    if used name then choose (suffix + 1) else name in
  choose 0

let fresh_edge_name geometry base =
  let rec choose suffix =
    let name = if suffix = 0 then base else base ^ string_of_int suffix in
    if Geometry.find_edge_group name geometry = None then name
    else choose (suffix + 1) in
  choose 0

let install_source_ids geometry name =
  let add owner values geometry =
    match Attribute.create_owned ~owner ~name (Attribute.Int values) with
    | Error message -> invalid_arg message
    | Ok attribute ->
        (match Geometry.with_attribute attribute geometry with
         | Ok geometry -> geometry | Error message -> invalid_arg message) in
  geometry
  |> add Attribute.Point (Array.init (Geometry.point_count geometry) Fun.id)
  |> add Attribute.Vertex (Array.init (Geometry.vertex_count geometry) Fun.id)
  |> add Attribute.Primitive (Array.init (Geometry.primitive_count geometry) Fun.id)

let source_ids owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Int values -> values
       | _ -> invalid_arg "Boolean source-ID scratch has the wrong storage")
  | None -> invalid_arg "Boolean source-ID scratch was lost"

let remove_source_ids name geometry =
  geometry
  |> Geometry.without_attribute ~owner:Attribute.Point name
  |> Geometry.without_attribute ~owner:Attribute.Vertex name
  |> Geometry.without_attribute ~owner:Attribute.Primitive name

let rounded_triangle_degenerate positions topology primitive =
  if not (primitive_is_triangle topology primitive) then false
  else
    let first = topology.Topology.View.primitive_offsets.(primitive) in
    let a = topology.vertex_points.(first)
    and b = topology.vertex_points.(first + 1)
    and c = topology.vertex_points.(first + 2) in
    a = b || b = c || c = a
    || (Predicates.orient2d_packed ~x:positions.Packed.Float3.View.x
          ~y:positions.y a b c = Predicates.Zero
        && Predicates.orient2d_packed ~x:positions.y ~y:positions.z a b c
           = Predicates.Zero
        && Predicates.orient2d_packed ~x:positions.z ~y:positions.x a b c
           = Predicates.Zero)

let rounded_sliver_edges ?cancel ~grain geometry =
  if grain <= 0 then error "invalid_parameter" "grain must be positive"
  else try
    Cancel.check_opt cancel;
    let topology_value = Geometry.topology geometry in
    let topology = Topology.View.view topology_value
    and positions = Packed.Float3.View.view (Geometry.positions geometry)
    and index_value = Topology_index.create ?cancel topology_value in
    let index = Topology_index.View.view index_value
    and bits = Bytes.make ((Topology_index.edge_count index_value + 7) / 8) '\000'
    and degenerate_count = ref 0 in
    for primitive = 0 to Geometry.primitive_count geometry - 1 do
      Cancel.poll cancel primitive 4095;
      if rounded_triangle_degenerate positions topology primitive then begin
        incr degenerate_count;
        let first = topology.primitive_offsets.(primitive) in
        for local = 0 to 2 do
          let edge = index.edge_of_vertex.(first + local) in
          if edge >= 0 then Support.Bits.set bits edge
        done
      end
    done;
    Ok (!degenerate_count,
      Edge_group.View.of_owned_bits ~topology:topology_value
        ~edge_count:(Topology_index.edge_count index_value)
        ~name:"__rdk_boolean_rounded_sliver" bits)
  with
  | Cancel.Cancelled -> error "cancelled" "Boolean rounded-sliver scan was cancelled"
  | Invalid_argument message -> error "invalid_output" message

let verify_unchanged_extraction ?cancel ~grain ~require_closed
    ~allow_opposite_duplicates ancestry geometry =
  if grain <= 0 then error "invalid_parameter" "grain must be positive"
  else
    match verify_extracted_surface ?cancel ~grain ~require_closed
        ~allow_opposite_duplicates ancestry geometry with
    | Ok () -> Ok true
    | Error failure when Error.code failure = "cancelled"
        || Error.code failure = "geometry_mismatch"
        || Error.code failure = "invalid_parameter" -> Error failure
    | Error _ -> Ok false

let coordinate_ulp value =
  if not (Float.is_finite value) then infinity
  else max
      (abs_float (Float.next_after value infinity -. value))
      (abs_float (value -. Float.next_after value neg_infinity))

let primitive_ulp_scale positions topology primitive =
  let first = topology.Topology.View.primitive_offsets.(primitive)
  and last = topology.primitive_offsets.(primitive + 1)
  and scale = ref 0. in
  for vertex = first to last - 1 do
    let point = topology.vertex_points.(vertex) in
    scale := max !scale (coordinate_ulp positions.Packed.Float3.View.x.(point));
    scale := max !scale (coordinate_ulp positions.y.(point));
    scale := max !scale (coordinate_ulp positions.z.(point))
  done;
  !scale

let rounded_self_contact_edges ?cancel ~grain geometry =
  if grain <= 0 then error "invalid_parameter" "grain must be positive"
  else
    match Boolean_constraints.build ?cancel ~grain
        ~resolve_left_self_intersections:true
        ~left:geometry ~right:(empty_geometry ()) () with
    | Error _ as failure -> failure
    | Ok constraints ->
        try
          let contact_count = Boolean_constraints.constraint_count constraints in
          let topology_value = Geometry.topology geometry in
          let topology = Topology.View.view topology_value
          and positions = Packed.Float3.View.view (Geometry.positions geometry)
          and index_value = Topology_index.create ?cancel topology_value in
          let index = Topology_index.View.view index_value
          and surface = Boolean_constraints.View.left_surface constraints
          and bits = Bytes.make ((Topology_index.edge_count index_value + 7) / 8) '\000'
          and repairable = ref 0 in
          let mark triangle =
            let primitive = Surface_index.View.triangle_primitive surface triangle in
            let minimum = primitive_min_edge_length positions topology primitive
            and ulp = primitive_ulp_scale positions topology primitive in
            if minimum <= 8. *. ulp then begin
              incr repairable;
              let first = topology.primitive_offsets.(primitive) in
              for local = 0 to 2 do
                let edge = index.edge_of_vertex.(first + local) in
                if edge >= 0 then begin
                  let length = edge_length positions index.edge_a.(edge) index.edge_b.(edge) in
                  if length <= minimum then Support.Bits.set bits edge
                end
              done
            end in
          for contact = 0 to contact_count - 1 do
            Cancel.poll cancel contact 4095;
            mark (Boolean_constraints.constraint_first_triangle constraints contact);
            mark (Boolean_constraints.constraint_second_triangle constraints contact)
          done;
          Ok (contact_count, !repairable,
            Edge_group.View.of_owned_bits ~topology:topology_value
              ~edge_count:(Topology_index.edge_count index_value)
              ~name:"__rdk_boolean_rounded_self_contact" bits)
        with
        | Cancel.Cancelled -> error "cancelled"
            "Boolean rounded self-contact scan was cancelled"
        | Invalid_argument message -> error "invalid_output" message

let sorted_triangle_points topology primitive =
  let first = topology.Topology.View.primitive_offsets.(primitive) in
  let a = topology.vertex_points.(first)
  and b = topology.vertex_points.(first + 1)
  and c = topology.vertex_points.(first + 2) in
  if a <= b then
    if b <= c then a, b, c
    else if a <= c then a, c, b else c, a, b
  else if a <= c then b, a, c
  else if b <= c then b, c, a else c, b, a

let paired_rounded_slivers ?cancel ~grain geometry =
  if grain <= 0 then error "invalid_parameter" "grain must be positive"
  else try
    Cancel.check_opt cancel;
    let topology = Topology.View.view (Geometry.topology geometry)
    and positions = Packed.Float3.View.view (Geometry.positions geometry) in
    let degenerate_count = ref 0 in
    for primitive = 0 to Geometry.primitive_count geometry - 1 do
      if rounded_triangle_degenerate positions topology primitive then
        incr degenerate_count
    done;
    let keyed = Array.make !degenerate_count ((0, 0, 0), 0)
    and cursor = ref 0 in
    for primitive = 0 to Geometry.primitive_count geometry - 1 do
      Cancel.poll cancel primitive 4095;
      if rounded_triangle_degenerate positions topology primitive then begin
        keyed.(!cursor) <- sorted_triangle_points topology primitive, primitive;
        incr cursor
      end
    done;
    Array.sort (fun (left, _) (right, _) -> Stdlib.compare left right) keyed;
    let bits = Bytes.make ((Geometry.primitive_count geometry + 7) / 8) '\000'
    and removed = ref 0 and first = ref 0 in
    while !first < Array.length keyed do
      let last = ref (!first + 1) in
      while !last < Array.length keyed
          && fst keyed.(!last) = fst keyed.(!first) do incr last done;
      let paired_last = !first + (((!last - !first) / 2) * 2) in
      for slot = !first to paired_last - 1 do
        Support.Bits.set bits (snd keyed.(slot));
        incr removed
      done;
      first := !last
    done;
    Ok (!removed, Group.View.of_owned_bits ~owner:Group.Primitive
      ~name:"__rdk_boolean_paired_rounded_sliver"
      ~length:(Geometry.primitive_count geometry) bits)
  with
  | Cancel.Cancelled -> error "cancelled"
      "Boolean paired rounded-sliver scan was cancelled"
  | Invalid_argument message -> error "invalid_output" message

let closed_rounded_slivers ?cancel ~grain geometry =
  if grain <= 0 then error "invalid_parameter" "grain must be positive"
  else try
    Cancel.check_opt cancel;
    let topology_value = Geometry.topology geometry in
    let topology = Topology.View.view topology_value
    and positions = Packed.Float3.View.view (Geometry.positions geometry)
    and index_value = Topology_index.create ?cancel topology_value in
    let index = Topology_index.View.view index_value
    and primitive_count = Geometry.primitive_count geometry in
    let selected = Bytes.make ((primitive_count + 7) / 8) '\000'
    and selected_count = ref 0 in
    for primitive = 0 to primitive_count - 1 do
      Cancel.poll cancel primitive 4095;
      if rounded_triangle_degenerate positions topology primitive then begin
        Support.Bits.set selected primitive;
        incr selected_count
      end
    done;
    let valid = ref (!selected_count > 0) and edge = ref 0 in
    while !valid && !edge < Array.length index.edge_a do
      let first = index.edge_offsets.(!edge)
      and last = index.edge_offsets.(!edge + 1) and removed = ref 0 in
      for slot = first to last - 1 do
        let primitive = index.primitive_of_vertex.(index.edge_vertices.(slot)) in
        if Support.Bits.mem selected primitive then incr removed
      done;
      let remaining = last - first - !removed in
      if remaining <> 0 && (remaining < 2 || remaining land 1 <> 0) then
        valid := false;
      incr edge
    done;
    let group = Group.View.of_owned_bits ~owner:Group.Primitive
        ~name:"__rdk_boolean_closed_rounded_sliver"
        ~length:primitive_count selected in
    Ok (!valid, !selected_count, group)
  with
  | Cancel.Cancelled -> error "cancelled"
      "Boolean closed rounded-sliver scan was cancelled"
  | Invalid_argument message -> error "invalid_output" message

let delete_rounding_pairs ?cancel ~grain selected cleanup =
  if Group.cardinality selected = 0 then Ok cleanup else try
    let geometry = cleanup.cleanup_geometry in
    let source_name = fresh_attribute_name geometry "__rdk_boolean_source_id"
    and seam_name = fresh_edge_name geometry "__rdk_boolean_exact_seam" in
    let tagged = install_source_ids geometry source_name in
    let tagged_seams = Edge_group.with_name seam_name cleanup.cleanup_seam_edges in
    let tagged = match Geometry.with_edge_group tagged_seams tagged with
      | Ok geometry -> geometry | Error message -> invalid_arg message in
    match Deletion.delete_primitives { cancel; grain = Some grain; compact_points = true } selected tagged with
    | Error _ as failure -> failure
    | Ok output ->
        let point_current = source_ids Attribute.Point source_name output
        and vertex_current = source_ids Attribute.Vertex source_name output
        and primitive_current = source_ids Attribute.Primitive source_name output
        and seams = match Geometry.find_edge_group seam_name output with
          | Some group -> group
          | None -> invalid_arg "Boolean seam-edge scratch was lost" in
        let compose source mapping = Array.map (fun current ->
            if current < 0 || current >= Array.length source then
              invalid_arg "Boolean rounding-pair remap is out of range";
            source.(current)) mapping in
        let output = remove_source_ids source_name output
            |> Geometry.without_edge_group seam_name in
        Ok {
          cleanup with
          cleanup_geometry = output;
          cleanup_rounding_repaired_count = cleanup.cleanup_rounding_repaired_count
            + Group.cardinality selected;
          cleanup_point_sources = compose cleanup.cleanup_point_sources point_current;
          cleanup_vertex_sources = compose cleanup.cleanup_vertex_sources vertex_current;
          cleanup_primitive_sources =
            compose cleanup.cleanup_primitive_sources primitive_current;
          cleanup_seam_edges = seams;
        }
  with
  | Cancel.Cancelled -> error "cancelled" "Boolean rounding-pair deletion was cancelled"
  | Invalid_argument message -> error "invalid_output" message

let collapse_rounding_batch ?cancel ~grain ~keep_greatest selected cleanup =
  if Edge_group.cardinality selected = 0 then Ok cleanup else try
    let geometry = cleanup.cleanup_geometry in
    let source_name = fresh_attribute_name geometry "__rdk_boolean_source_id"
    and seam_name = fresh_edge_name geometry "__rdk_boolean_exact_seam" in
    let tagged = install_source_ids geometry source_name in
    let tagged_seams = Edge_group.with_name seam_name cleanup.cleanup_seam_edges in
    let tagged = match Geometry.with_edge_group tagged_seams tagged with
      | Ok geometry -> geometry | Error message -> invalid_arg message in
    match Edge_collapse.run { cancel; grain = Some grain; edges = Some selected; connectivity_attribute = None; position = Some (if keep_greatest then Fuse_reduce.Greatest_point_position
          else Fuse_reduce.Least_point_position); remove_degenerate_primitives = Some true; recompute_point_normals = Some true } tagged with
    | Error _ as failure -> failure
    | Ok output ->
        let point_current = source_ids Attribute.Point source_name output
        and vertex_current = source_ids Attribute.Vertex source_name output
        and primitive_current = source_ids Attribute.Primitive source_name output
        and seams = match Geometry.find_edge_group seam_name output with
          | Some group -> group
          | None -> invalid_arg "Boolean seam-edge scratch was lost" in
        let compose source mapping = Array.map (fun current ->
            if current < 0 || current >= Array.length source then
              invalid_arg "Boolean rounding-repair remap is out of range";
            source.(current)) mapping in
        let output = remove_source_ids source_name output
            |> Geometry.without_edge_group seam_name in
        Ok {
          cleanup with
          cleanup_geometry = output;
          cleanup_rounding_repaired_count =
            cleanup.cleanup_rounding_repaired_count
            + Edge_group.cardinality selected;
          cleanup_point_sources = compose cleanup.cleanup_point_sources point_current;
          cleanup_vertex_sources = compose cleanup.cleanup_vertex_sources vertex_current;
          cleanup_primitive_sources =
            compose cleanup.cleanup_primitive_sources primitive_current;
          cleanup_seam_edges = seams;
        }
  with
  | Cancel.Cancelled -> error "cancelled" "Boolean rounding repair was cancelled"
  | Invalid_argument message -> error "invalid_output" message

let repair_rounded_slivers ?cancel ~grain ~require_closed
    ~allow_opposite_duplicates ~max_batches ancestry seam geometry =
  match surface_seam_edges ?cancel ~grain ancestry seam with
  | Error _ as failure -> failure
  | Ok exact_seams ->
      let initial = {
        cleanup_geometry = geometry;
        cleanup_candidate_count = 0;
        cleanup_collapsed_count = 0;
        cleanup_batch_count = 0;

        cleanup_rounding_repaired_count = 0;
        cleanup_point_sources = Array.init (Geometry.point_count geometry) Fun.id;
        cleanup_vertex_sources = Array.init (Geometry.vertex_count geometry) Fun.id;
        cleanup_primitive_sources =
          Array.init (Geometry.primitive_count geometry) Fun.id;
        cleanup_seam_edges = exact_seams;
      } in
      let rec continue batch cleanup =
        match rounded_sliver_edges ?cancel ~grain cleanup.cleanup_geometry with
        | Error _ as failure -> failure
        | Ok (0, _) ->
            let geometry = cleanup.cleanup_geometry in
            let extracted = Boolean_extract.geometry ancestry in
            let verified =
              if Geometry.positions geometry == Geometry.positions extracted
                  && Geometry.topology geometry == Geometry.topology extracted then
                verify_extracted_surface ?cancel ~grain ~require_closed
                  ~allow_opposite_duplicates ancestry geometry
              else
                verify_surface ?cancel ~grain ~require_closed
                  ~allow_opposite_duplicates geometry in
            (match verified with
             | Ok () ->
                 (match verify_materialized_seam_curves ?cancel ~grain seam with
                  | Error _ as failure -> failure
                  | Ok () -> Ok cleanup)
             | Error initial when Error.code initial <> "surface_self_intersection" ->
                 Error initial
             | Error initial ->
            (match rounded_self_contact_edges ?cancel ~grain
                cleanup.cleanup_geometry with
             | Error _ as failure -> failure
             | Ok (contacts, _, _) when contacts > 0 && batch >= max_batches ->
                 error "unresolved_rounding_contacts" (Printf.sprintf
                   "%d rounded self-contact(s) remain after %d certified batch(es)"
                   contacts max_batches)
             | Ok (contacts, repairable, candidates) when contacts > 0 ->
                 (match safe_independent_edges ?cancel ~grain candidates
                     cleanup.cleanup_geometry with
                  | Error _ as failure -> failure
                  | Ok least when Edge_group.cardinality least > 0 ->
                      (match collapse_rounding_batch ?cancel ~grain
                          ~keep_greatest:false least cleanup with
                       | Error _ as failure -> failure
                       | Ok cleanup -> continue (batch + 1) cleanup)
                  | Ok _ ->
                      (match safe_independent_edges ?cancel ~grain
                          ~keep_greatest:true candidates cleanup.cleanup_geometry with
                       | Error _ as failure -> failure
                       | Ok greatest when Edge_group.cardinality greatest > 0 ->
                           (match collapse_rounding_batch ?cancel ~grain
                               ~keep_greatest:true greatest cleanup with
                            | Error _ as failure -> failure
                            | Ok cleanup -> continue (batch + 1) cleanup)
                       | Ok _ ->
                           (match speculative_independent_edges ?cancel ~grain
                               candidates cleanup.cleanup_geometry with
                            | Error _ as failure -> failure
                            | Ok speculative
                                when Edge_group.cardinality speculative > 1 ->
                                (match collapse_rounding_batch ?cancel ~grain
                                    ~keep_greatest:false speculative cleanup with
                                 | Error _ as failure -> failure
                                 | Ok cleanup -> continue (batch + 1) cleanup)
                            | Ok _ ->
                           let topology = Geometry.topology cleanup.cleanup_geometry in
                           let index = Topology_index.create ?cancel topology in
                           let positions = Packed.Float3.View.view
                               (Geometry.positions cleanup.cleanup_geometry) in
                           let ordered = Array.make (Edge_group.cardinality candidates) 0
                           and cursor = ref 0 in
                           Edge_group.iter (fun edge ->
                             ordered.(!cursor) <- edge; incr cursor) candidates;
                           Array.sort (fun first second ->
                             let first_length = edge_length positions
                                 (Topology_index.edge_points index first |> fst)
                                 (Topology_index.edge_points index first |> snd)
                             and second_length = edge_length positions
                                 (Topology_index.edge_points index second |> fst)
                                 (Topology_index.edge_points index second |> snd) in
                             let compared = Float.compare first_length second_length in
                             if compared <> 0 then compared else Int.compare first second)
                             ordered;
                           let rec attempt slot keep_greatest last_error =
                             if slot >= min 64 (Array.length ordered) then
                               (match last_error with
                                | Some failure -> Error failure
                                | None -> error "unresolved_rounding_contacts"
                                    (Printf.sprintf
                                      "%d rounded self-contact(s), %d ULP-scale incident triangle(s), and no certified contraction"
                                      contacts repairable))
                             else
                               let edge = ordered.(slot) in
                               let bits = Bytes.make
                                   ((Topology_index.edge_count index + 7) / 8) '\000' in
                               Support.Bits.set bits edge;
                               let selected = Edge_group.View.of_owned_bits
                                   ~topology ~edge_count:(Topology_index.edge_count index)
                                   ~name:"__rdk_boolean_speculative_rounding_contact"
                                   bits in
                               match collapse_rounding_batch ?cancel ~grain
                                   ~keep_greatest selected cleanup with
                               | Error failure when Error.code failure = "cancelled" ->
                                   Error failure
                               | Error failure ->
                                   if keep_greatest then attempt (slot + 1) false
                                       (Some failure)
                                   else attempt slot true (Some failure)
                               | Ok candidate ->
                                   (match rounded_sliver_edges ?cancel ~grain
                                       candidate.cleanup_geometry with
                                    | Error failure when Error.code failure = "cancelled" ->
                                        Error failure
                                    | Error failure ->
                                        if keep_greatest then attempt (slot + 1) false
                                            (Some failure)
                                        else attempt slot true (Some failure)
                                    | Ok (slivers, _) when slivers <> 0 ->
                                        if keep_greatest then attempt (slot + 1) false
                                            last_error
                                        else attempt slot true last_error
                                    | Ok _ ->
                                        (match rounded_self_contact_edges ?cancel ~grain
                                            candidate.cleanup_geometry with
                                         | Error failure when Error.code failure = "cancelled" ->
                                             Error failure
                                         | Ok (next_contacts, _, _)
                                             when next_contacts < contacts ->
                                             continue (batch + 1) candidate
                                         | Error failure ->
                                             if keep_greatest then
                                               attempt (slot + 1) false (Some failure)
                                             else attempt slot true (Some failure)
                                         | Ok _ ->
                                             if keep_greatest then
                                               attempt (slot + 1) false last_error
                                             else attempt slot true last_error)) in
                           attempt 0 false None)))
             | Ok _ -> Error initial))
        | Ok (remaining, _) when batch >= max_batches ->
            error "unresolved_rounding_slivers" (Printf.sprintf
              "%d rounded degenerate triangle(s) remain after %d certified batch(es)"
              remaining max_batches)
        | Ok (remaining, candidates) ->
            (match closed_rounded_slivers ?cancel ~grain
                cleanup.cleanup_geometry with
             | Error _ as failure -> failure
             | Ok (true, _, closed) ->
                 (match delete_rounding_pairs ?cancel ~grain closed cleanup with
                  | Error _ as failure -> failure
                  | Ok cleanup -> continue (batch + 1) cleanup)
             | Ok _ ->
            (match paired_rounded_slivers ?cancel ~grain
                cleanup.cleanup_geometry with
             | Error _ as failure -> failure
             | Ok (removed, paired) when removed > 0 ->
                 (match delete_rounding_pairs ?cancel ~grain paired cleanup with
                  | Error _ as failure -> failure
                  | Ok cleanup -> continue (batch + 1) cleanup)
             | Ok _ ->
            (match safe_independent_edges ?cancel ~grain candidates
                cleanup.cleanup_geometry with
             | Error _ as failure -> failure
             | Ok least when Edge_group.cardinality least > 0 ->
                 (match collapse_rounding_batch ?cancel ~grain
                     ~keep_greatest:false least cleanup with
                  | Error _ as failure -> failure
                  | Ok cleanup -> continue (batch + 1) cleanup)
             | Ok _ ->
                 (match safe_independent_edges ?cancel ~grain ~keep_greatest:true
                     candidates cleanup.cleanup_geometry with
                  | Error _ as failure -> failure
                  | Ok greatest when Edge_group.cardinality greatest > 0 ->
                      (match collapse_rounding_batch ?cancel ~grain
                          ~keep_greatest:true greatest cleanup with
                       | Error _ as failure -> failure
                       | Ok cleanup -> continue (batch + 1) cleanup)
                  | Ok _ ->
                      let topology = Geometry.topology cleanup.cleanup_geometry in
                      let index = Topology_index.create ?cancel topology in
                      let two_sided = ref 0 and other = ref 0 in
                      Edge_group.iter (fun edge ->
                        if Topology_index.edge_incidence_count index edge = 2
                        then incr two_sided else incr other) candidates;
                      error "unresolved_rounding_slivers" (Printf.sprintf
                        "%d rounded degenerate triangle(s) have no certified edge contraction (%d two-sided and %d other candidate edges)"
                        remaining !two_sided !other))))) in
      continue 0 initial

let tiny_cleanup_edges ?cancel ~grain ~threshold ancestry seam cleanup =
  if grain <= 0 then error "invalid_parameter" "grain must be positive"
  else if not (Float.is_finite threshold) || threshold < 0. then
    error "invalid_parameter" "threshold must be finite and non-negative"
  else try
    Cancel.check_opt cancel;
    let complex = Boolean_extract.View.complex ancestry in
    if Boolean_seam.View.complex seam != complex then
      invalid_arg "Boolean seam and extraction belong to different exact complexes";
    let extraction_primitives = Geometry.primitive_count
        (Boolean_extract.geometry ancestry) in
    let geometry = cleanup.cleanup_geometry
    and seam_facets = Bytes.make (Boolean_complex.facet_count complex) '\000' in
    for complex_edge = 0 to Boolean_complex.edge_count complex - 1 do
      Cancel.poll cancel complex_edge 4095;
      if Boolean_seam.View.is_seam_edge seam complex_edge then begin
        let first, last = Boolean_complex.edge_incident_range complex complex_edge in
        for incident = first to last - 1 do
          Bytes.unsafe_set seam_facets
            (Boolean_complex.edge_incident_facet complex incident) '\001'
        done
      end
    done;
    let topology = Geometry.topology geometry in
    let index_value = Topology_index.create ?cancel topology in
    let index = Topology_index.View.view index_value
    and positions = Packed.Float3.View.view (Geometry.positions geometry) in
    Ok (Edge_group.init ~grain ~topology ~index:index_value
      ~name:"__rdk_boolean_tiny_seam_continuation" (fun edge ->
        Cancel.poll cancel edge 4095;
        let first = index.edge_offsets.(edge)
        and last = index.edge_offsets.(edge + 1)
        and adjacent = ref false in
        let incident = ref first in
        while not !adjacent && !incident < last do
          let primitive = index.primitive_of_vertex.(index.edge_vertices.(!incident)) in
          let extraction = cleanup.cleanup_primitive_sources.(primitive) in
          if extraction < 0 || extraction >= extraction_primitives then
            invalid_arg "Boolean cleanup primitive ancestry is out of range";
          let facet = Boolean_extract.View.primitive_complex_facet
              ancestry extraction in
          adjacent := Bytes.unsafe_get seam_facets facet <> '\000';
          incr incident
        done;
        !adjacent && edge_length positions index.edge_a.(edge) index.edge_b.(edge)
          <= threshold))
  with
  | Cancel.Cancelled -> error "cancelled" "Boolean cleanup continuation was cancelled"
  | Invalid_argument message -> error "invalid_input" message

let collapse_cleanup_batch ?cancel ~grain ~require_closed
    ~allow_opposite_duplicates seam selected cleanup =
  if Edge_group.cardinality selected = 0 then Ok cleanup else try
    let geometry = cleanup.cleanup_geometry in
    let source_name = fresh_attribute_name geometry "__rdk_boolean_source_id"
    and seam_name = fresh_edge_name geometry "__rdk_boolean_exact_seam" in
    let tagged = install_source_ids geometry source_name in
    let tagged_seams = Edge_group.with_name seam_name cleanup.cleanup_seam_edges in
    let tagged = match Geometry.with_edge_group tagged_seams tagged with
      | Ok geometry -> geometry | Error message -> invalid_arg message in
    match Edge_collapse.run { cancel; grain = Some grain; edges = Some selected; connectivity_attribute = None; position = Some Fuse_reduce.First_position; remove_degenerate_primitives = Some true; recompute_point_normals = Some true } tagged with
    | Error _ as failure -> failure
    | Ok output ->
        let point_current = source_ids Attribute.Point source_name output
        and vertex_current = source_ids Attribute.Vertex source_name output
        and primitive_current = source_ids Attribute.Primitive source_name output
        and seams = match Geometry.find_edge_group seam_name output with
          | Some group -> group
          | None -> invalid_arg "Boolean seam-edge scratch was lost" in
        let compose source mapping = Array.map (fun current ->
            if current < 0 || current >= Array.length source then
              invalid_arg "Boolean cleanup continuation remap is out of range";
            source.(current)) mapping in
        let output = remove_source_ids source_name output
            |> Geometry.without_edge_group seam_name in
        (match verify_materialized_seam_curves ?cancel ~grain seam with
         | Error _ as failure -> failure
         | Ok () ->
             match verify_surface ?cancel ~grain ~require_closed
                 ~allow_opposite_duplicates output with
             | Error _ as failure -> failure
             | Ok () -> Ok {
                 cleanup_geometry = output;
                 cleanup_candidate_count = cleanup.cleanup_candidate_count;
                 cleanup_collapsed_count = cleanup.cleanup_collapsed_count
                   + Edge_group.cardinality selected;
                 cleanup_batch_count = cleanup.cleanup_batch_count + 1;

                 cleanup_rounding_repaired_count =
                   cleanup.cleanup_rounding_repaired_count;
                 cleanup_point_sources = compose cleanup.cleanup_point_sources
                     point_current;
                 cleanup_vertex_sources = compose cleanup.cleanup_vertex_sources
                     vertex_current;
                 cleanup_primitive_sources = compose cleanup.cleanup_primitive_sources
                     primitive_current;
                 cleanup_seam_edges = seams;
               })
  with
  | Cancel.Cancelled -> error "cancelled" "Boolean cleanup continuation was cancelled"
  | Invalid_argument message -> error "invalid_output" message

let collapse_tiny_seams ?cancel ~grain ~threshold ~require_closed
    ?(allow_opposite_duplicates = false) ?(max_batches = 8) ?(strict = true)
    ancestry seam geometry =
  if max_batches <= 0 then
    error "invalid_parameter" "max_batches must be positive"
  else
    match repair_rounded_slivers ?cancel ~grain ~require_closed
        ~allow_opposite_duplicates ~max_batches:64 ancestry seam geometry with
    | Error _ as failure -> failure
    | Ok initial ->
        let rec continue cleanup =
          match tiny_cleanup_edges ?cancel ~grain ~threshold ancestry seam cleanup with
          | Error _ as failure -> failure
          | Ok candidates ->
              let remaining = Edge_group.cardinality candidates in
              let cleanup = if cleanup.cleanup_batch_count = 0 then
                  { cleanup with cleanup_candidate_count = remaining }
                else cleanup in
              if remaining = 0 then Ok cleanup
              else if cleanup.cleanup_batch_count >= max_batches then
                if strict then error "unresolved_cleanup" (Printf.sprintf
                    "%d tiny seam-adjacent edge candidate(s) remain after %d batch(es)"
                    remaining max_batches)
                else Ok cleanup
              else
                match safe_independent_edges ?cancel ~grain candidates
                    cleanup.cleanup_geometry with
                | Error _ as failure -> failure
                | Ok selected when Edge_group.cardinality selected = 0 ->
                    if strict then error "unresolved_cleanup" (Printf.sprintf
                        "%d tiny seam-adjacent edge candidate(s) have no certified contraction"
                        remaining)
                    else Ok cleanup
                | Ok selected ->
                    (match collapse_cleanup_batch ?cancel ~grain ~require_closed
                        ~allow_opposite_duplicates seam selected cleanup with
                     | Error _ as failure -> failure
                     | Ok cleanup -> continue cleanup) in
        continue initial

let split_seam_points ?cancel ~grain cleanup =
  if grain <= 0 then error "invalid_parameter" "grain must be positive"
  else if Edge_group.cardinality cleanup.cleanup_seam_edges = 0 then Ok cleanup
  else try
    Cancel.check_opt cancel;
    let geometry = cleanup.cleanup_geometry in
    let topology_value = Geometry.topology geometry in
    let topology = Topology.View.view topology_value in
    let index_value = Topology_index.create ?cancel topology_value in
    let index = Topology_index.View.view index_value in
    let primitive_count = Geometry.primitive_count geometry
    and point_count = Geometry.point_count geometry
    and vertex_count = Geometry.vertex_count geometry in
    if Edge_group.topology_data_id cleanup.cleanup_seam_edges
        <> Topology.data_id topology_value
        || Edge_group.length cleanup.cleanup_seam_edges
           <> Array.length index.edge_a then
      invalid_arg "Boolean seam split group belongs to a different topology";
    let parent = Array.init primitive_count Fun.id
    and rank = Bytes.make primitive_count '\000' in
    let rec find primitive =
      let ancestor = parent.(primitive) in
      if ancestor = primitive then primitive
      else begin
        let root = find ancestor in
        parent.(primitive) <- root;
        root
      end in
    let union first second =
      let first = find first and second = find second in
      if first <> second then begin
        let first_rank = Char.code (Bytes.unsafe_get rank first)
        and second_rank = Char.code (Bytes.unsafe_get rank second) in
        if first_rank < second_rank then parent.(first) <- second
        else if second_rank < first_rank then parent.(second) <- first
        else begin
          parent.(second) <- first;
          Bytes.unsafe_set rank first (Char.chr (first_rank + 1))
        end
      end in
    for edge = 0 to Array.length index.edge_a - 1 do
      Cancel.poll cancel edge 4095;
      if not (Edge_group.mem edge cleanup.cleanup_seam_edges) then begin
        let first = index.edge_offsets.(edge)
        and last = index.edge_offsets.(edge + 1) in
        if first < last then begin
          let base = index.primitive_of_vertex.(index.edge_vertices.(first)) in
          for incident = first + 1 to last - 1 do
            union base index.primitive_of_vertex.(index.edge_vertices.(incident))
          done
        end
      end
    done;
    let root_component = Array.make primitive_count (-1)
    and primitive_component = Array.make primitive_count 0
    and component_count = ref 0 in
    for primitive = 0 to primitive_count - 1 do
      let root = find primitive in
      if root_component.(root) < 0 then begin
        root_component.(root) <- !component_count;
        incr component_count
      end;
      primitive_component.(primitive) <- root_component.(root)
    done;
    let component_stamps = Array.make !component_count (-1)
    and point_offsets = Array.make (point_count + 1) 0 in
    for point = 0 to point_count - 1 do
      Cancel.poll cancel point 4095;
      let first = index.point_offsets.(point)
      and last = index.point_offsets.(point + 1)
      and count = ref 0 in
      for incident = first to last - 1 do
        let primitive = index.primitive_of_vertex.(index.point_vertices.(incident)) in
        let component = primitive_component.(primitive) in
        if component_stamps.(component) <> point then begin
          component_stamps.(component) <- point;
          incr count
        end
      done;
      let count = if !count = 0 then 1 else !count in
      if point_offsets.(point) > Sys.max_array_length - count then
        invalid_arg "Boolean seam point cardinality exceeds array limits";
      point_offsets.(point + 1) <- point_offsets.(point) + count
    done;
    let output_points = point_offsets.(point_count) in
    let point_map = Array.make output_points 0
    and vertex_points = Array.make vertex_count 0
    and component_output = Array.make !component_count (-1) in
    Array.fill component_stamps 0 !component_count (-1);
    for point = 0 to point_count - 1 do
      let first = index.point_offsets.(point)
      and last = index.point_offsets.(point + 1)
      and next = ref point_offsets.(point) in
      if first = last then begin
        point_map.(!next) <- point;
        incr next
      end else
        for incident = first to last - 1 do
          let vertex = index.point_vertices.(incident) in
          let primitive = index.primitive_of_vertex.(vertex) in
          let component = primitive_component.(primitive) in
          if component_stamps.(component) <> point then begin
            component_stamps.(component) <- point;
            component_output.(component) <- !next;
            point_map.(!next) <- point;
            incr next
          end;
          vertex_points.(vertex) <- component_output.(component)
        done;
      if !next <> point_offsets.(point + 1) then
        invalid_arg "Boolean seam point plan did not match its prefix"
    done;
    let target_topology = Topology.View.create_validated_owned
        ~point_count:output_points ~vertex_points
        ~primitive_offsets:(Array.copy topology.primitive_offsets)
        ~primitive_kinds:(Bytes.copy topology.primitive_kinds) in
    let source_positions = Packed.Float3.View.view (Geometry.positions geometry) in
    let select source = Parallel.init_array ~grain output_points (fun output ->
        Cancel.poll cancel output 4095;
        source.(point_map.(output))) in
    let positions = Packed.Float3.View.of_owned_exn
        ~x:(select source_positions.x) ~y:(select source_positions.y)
        ~z:(select source_positions.z) in
    let identity_vertices = Array.init vertex_count Fun.id
    and identity_primitives = Array.init primitive_count Fun.id in
    let attributes = Topology_remap.attributes ?cancel ~grain ~point_map
        ~vertex_map:identity_vertices ~primitive_map:identity_primitives geometry
    and groups = Topology_remap.groups ?cancel ~grain ~point_map
        ~vertex_map:identity_vertices ~primitive_map:identity_primitives geometry in
    let edge_groups = match Topology_remap.split_point_edge_groups ?cancel ~grain
        ~source_index:index_value ~target_topology (Geometry.edge_groups geometry) with
      | Ok groups -> groups | Error message -> invalid_arg message in
    let seam_edges = match Topology_remap.split_point_edge_groups ?cancel ~grain
        ~source_index:index_value ~target_topology [cleanup.cleanup_seam_edges] with
      | Ok [group] -> group
      | Ok _ -> invalid_arg "Boolean seam split produced invalid edge ancestry"
      | Error message -> invalid_arg message in
    let geometry = match Geometry.create ~positions ~topology:target_topology
        ~attributes ~groups ~edge_groups () with
      | Ok geometry -> geometry | Error message -> invalid_arg message in
    Ok {
      cleanup with
      cleanup_geometry = geometry;
      cleanup_point_sources = Array.map
          (fun point -> cleanup.cleanup_point_sources.(point)) point_map;
      cleanup_seam_edges = seam_edges;
    }
  with
  | Cancel.Cancelled -> error "cancelled" "Boolean seam point splitting was cancelled"
  | Invalid_argument message -> error "invalid_output" message

type detriangulation = Triangles | All_polygons | Unchanged_polygons

let detriangulate ?cancel ~grain ~assume_flat ~mode ancestry cleanup =
  if grain <= 0 then error "invalid_parameter" "grain must be positive"
  else if mode = Triangles then Ok cleanup
  else try
    Cancel.check_opt cancel;
    let geometry = cleanup.cleanup_geometry in
    let topology_value = Geometry.topology geometry in
    let topology = Topology.View.view topology_value in
    let index_value = Topology_index.create ?cancel topology_value in
    let index = Topology_index.View.view index_value in
    let primitive_count = Geometry.primitive_count geometry in
    if Array.length cleanup.cleanup_primitive_sources <> primitive_count then
      invalid_arg "Boolean detriangulation primitive ancestry is inconsistent";
    let extraction_primitives = Geometry.primitive_count
        (Boolean_extract.geometry ancestry) in
    let left_count = Geometry.primitive_count
        (Boolean_extract.View.left_geometry ancestry)
    and right_count = Geometry.primitive_count
        (Boolean_extract.View.right_geometry ancestry) in
    let left_cut = Bytes.make left_count '\000'
    and right_cut = Bytes.make right_count '\000' in
    for primitive = 0 to extraction_primitives - 1 do
      if Boolean_extract.primitive_refined_triangle ancestry primitive >= 0 then begin
        let face = Boolean_extract.primitive_face ancestry primitive in
        match Boolean_extract.primitive_side ancestry primitive with
        | Boolean_constraints.Left -> Bytes.unsafe_set left_cut face '\001'
        | Boolean_constraints.Right -> Bytes.unsafe_set right_cut face '\001'
      end
    done;
    let source primitive =
      let extraction = cleanup.cleanup_primitive_sources.(primitive) in
      if extraction < 0 || extraction >= extraction_primitives then
        invalid_arg "Boolean detriangulation source primitive is out of range";
      Boolean_extract.primitive_side ancestry extraction,
      Boolean_extract.primitive_face ancestry extraction in
    let unchanged side face = match side with
      | Boolean_constraints.Left -> Bytes.unsafe_get left_cut face = '\000'
      | Boolean_constraints.Right -> Bytes.unsafe_get right_cut face = '\000' in
    let positions = Packed.Float3.View.view (Geometry.positions geometry) in
    let coplanar edge =
      let first = index.edge_offsets.(edge) in
      let first_vertex = index.edge_vertices.(first)
      and second_vertex = index.edge_vertices.(first + 1) in
      let opposite vertex =
        topology.vertex_points.(index.next_vertex.(index.next_vertex.(vertex))) in
      let a = index.edge_a.(edge) and b = index.edge_b.(edge)
      and c = opposite first_vertex and d = opposite second_vertex in
      Predicates.orient3d_packed ~x:positions.x ~y:positions.y ~z:positions.z
        a b c d = Predicates.Zero in
    let selected = Edge_group.init ~grain ~topology:topology_value
        ~index:index_value ~name:"__rdk_boolean_detriangulate" (fun edge ->
      Cancel.poll cancel edge 4095;
      let first = index.edge_offsets.(edge)
      and last = index.edge_offsets.(edge + 1) in
      if last - first <> 2 || Edge_group.mem edge cleanup.cleanup_seam_edges then false
      else
        let first_primitive = index.primitive_of_vertex.(index.edge_vertices.(first))
        and second_primitive = index.primitive_of_vertex.(index.edge_vertices.(first + 1)) in
        if topology.primitive_offsets.(first_primitive + 1)
            - topology.primitive_offsets.(first_primitive) <> 3
            || topology.primitive_offsets.(second_primitive + 1)
               - topology.primitive_offsets.(second_primitive) <> 3 then false
        else
          let first_side, first_face = source first_primitive
          and second_side, second_face = source second_primitive in
          first_side = second_side && first_face = second_face
          && (mode = All_polygons || unchanged first_side first_face)
          && (assume_flat || coplanar edge)) in
    (* Dissolving every diagonal of a cut source face is valid only when each
       selected triangle component has one simple boundary cycle. A component
       with a hole would otherwise be encoded by Dissolve as a repeated bridge
       walk, which is deliberately not a valid RDK polygon. Keep such a
       component triangulated rather than publishing non-simple topology. *)
    let selected = if mode = Unchanged_polygons
        || Edge_group.cardinality selected = 0 then selected else begin
      let parent = Array.init primitive_count Fun.id in
      let rec find primitive =
        let next = parent.(primitive) in
        if next = primitive then primitive else begin
          let root = find next in parent.(primitive) <- root; root
        end in
      let unite first second =
        let first = find first and second = find second in
        if first <> second then
          if first < second then parent.(second) <- first
          else parent.(first) <- second in
      for edge = 0 to Array.length index.edge_a - 1 do
        if Edge_group.mem edge selected then begin
          let first = index.edge_offsets.(edge) in
          unite
            index.primitive_of_vertex.(index.edge_vertices.(first))
            index.primitive_of_vertex.(index.edge_vertices.(first + 1))
        end
      done;
      for primitive = 0 to primitive_count - 1 do
        parent.(primitive) <- find primitive
      done;
      let component_selected = Bytes.make primitive_count '\000'
      and head = Array.make primitive_count (-1)
      and next = Array.make primitive_count (-1) in
      for primitive = primitive_count - 1 downto 0 do
        let root = parent.(primitive) in
        next.(primitive) <- head.(root);
        head.(root) <- primitive
      done;
      Edge_group.iter (fun edge ->
        let first = index.edge_offsets.(edge) in
        let primitive = index.primitive_of_vertex.(index.edge_vertices.(first)) in
        Bytes.unsafe_set component_selected parent.(primitive) '\001') selected;
      let stamp = Array.make (Geometry.point_count geometry) (-1)
      and degree = Array.make (Geometry.point_count geometry) 0
      and boundary_parent = Array.init (Geometry.point_count geometry) Fun.id
      and touched = Array.make (Geometry.point_count geometry) 0
      and safe = Bytes.make primitive_count '\000' in
      let rec boundary_find point =
        let next = boundary_parent.(point) in
        if next = point then point else begin
          let root = boundary_find next in boundary_parent.(point) <- root; root
        end in
      for root = 0 to primitive_count - 1 do
        if Bytes.unsafe_get component_selected root <> '\000' then begin
          let touched_count = ref 0 and valid = ref true in
          let touch point =
            if stamp.(point) <> root then begin
              stamp.(point) <- root;
              degree.(point) <- 0;
              boundary_parent.(point) <- point;
              touched.(!touched_count) <- point;
              incr touched_count
            end in
          let add_boundary a b =
            touch a; touch b;
            degree.(a) <- degree.(a) + 1;
            degree.(b) <- degree.(b) + 1;
            let a_root = boundary_find a and b_root = boundary_find b in
            if a_root <> b_root then boundary_parent.(b_root) <- a_root in
          let primitive = ref head.(root) in
          while !primitive >= 0 do
            let first = topology.primitive_offsets.(!primitive)
            and last = topology.primitive_offsets.(!primitive + 1) in
            for vertex = first to last - 1 do
              let edge = index.edge_of_vertex.(vertex) in
              if edge >= 0 && not (Edge_group.mem edge selected) then begin
                let internal = ref false in
                for incident = index.edge_offsets.(edge)
                    to index.edge_offsets.(edge + 1) - 1 do
                  let other = index.primitive_of_vertex.(index.edge_vertices.(incident)) in
                  if other <> !primitive && parent.(other) = root then
                    internal := true
                done;
                if !internal then valid := false
                else add_boundary index.edge_a.(edge) index.edge_b.(edge)
              end
            done;
            primitive := next.(!primitive)
          done;
          let cycles = ref 0 in
          for slot = 0 to !touched_count - 1 do
            let point = touched.(slot) in
            if degree.(point) <> 2 then valid := false;
            if boundary_find point = point then incr cycles
          done;
          if !valid && !cycles = 1 then Bytes.unsafe_set safe root '\001'
        end
      done;
      Edge_group.init ~grain ~topology:topology_value ~index:index_value
        ~name:"__rdk_boolean_detriangulate_safe" (fun edge ->
          if not (Edge_group.mem edge selected) then false else
          let first = index.edge_offsets.(edge) in
          let primitive = index.primitive_of_vertex.(index.edge_vertices.(first)) in
          Bytes.unsafe_get safe parent.(primitive) <> '\000')
    end in
    if Edge_group.cardinality selected = 0 then Ok cleanup
    else begin
      let source_name = fresh_attribute_name geometry
          "__rdk_boolean_source_id"
      and seam_name = fresh_edge_name geometry "__rdk_boolean_exact_seam" in
      let tagged = install_source_ids geometry source_name in
      let tagged_seams = Edge_group.with_name seam_name cleanup.cleanup_seam_edges in
      let tagged = match Geometry.with_edge_group tagged_seams tagged with
        | Ok geometry -> geometry | Error message -> invalid_arg message in
      match Dissolve.run { cancel; grain = Some grain; edges = Some selected; operation = Dissolve.Dissolve_selected; bridge_policy = Dissolve.Create_bridged_polygons; remove_inline_points = false; collinearity_tolerance = 0.; remove_unused_points = false; create_boundary_curves = false; recompute_normals = false } tagged with
      | Error _ as failure -> failure
      | Ok output ->
          let point_current = source_ids Attribute.Point source_name output
          and vertex_current = source_ids Attribute.Vertex source_name output
          and primitive_current = source_ids Attribute.Primitive source_name output
          and seams = match Geometry.find_edge_group seam_name output with
            | Some group -> group
            | None -> invalid_arg "Boolean detriangulation seam ancestry was lost" in
          let compose source mapping = Array.map (fun current ->
              if current < 0 || current >= Array.length source then
                invalid_arg "Boolean detriangulation remap is out of range";
              source.(current)) mapping in
          let output = remove_source_ids source_name output
              |> Geometry.without_edge_group seam_name in
          Ok {
            cleanup with
            cleanup_geometry = output;
            cleanup_point_sources = compose cleanup.cleanup_point_sources point_current;
            cleanup_vertex_sources = compose cleanup.cleanup_vertex_sources vertex_current;
            cleanup_primitive_sources =
              compose cleanup.cleanup_primitive_sources primitive_current;
            cleanup_seam_edges = seams;
          }
    end
  with
  | Cancel.Cancelled -> error "cancelled" "Boolean detriangulation was cancelled"
  | Invalid_argument message -> error "invalid_output" message
