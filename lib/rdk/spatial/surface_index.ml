
type t = {
  geometry : Geometry.t;
  positions : Packed.Float3.View.view;
  topology : Topology.View.view;
  primitives : int array;
  vertex_a : int array;
  vertex_b : int array;
  vertex_c : int array;
  order : int array;
  triangle_min_x : float array;
  triangle_min_y : float array;
  triangle_min_z : float array;
  triangle_max_x : float array;
  triangle_max_y : float array;
  triangle_max_z : float array;
  triangle_min_d : float array array;
  triangle_max_d : float array array;
  min_x : float array;
  min_y : float array;
  min_z : float array;
  max_x : float array;
  max_y : float array;
  max_z : float array;
  min_d : float array array;
  max_d : float array array;
  left : int array;
  right : int array;
  first : int array;
  count : int array;
  common_a : int array;
  common_b : int array;
  common_c : int array;
  node_count : int;
}

type hit = {
  primitive : int;
  distance : float;

}

type vertex_selection = All_triangle_vertices | Any_triangle_vertex

type ray_direction_mode =
  | Ray_forward
  | Ray_reverse
  | Ray_bidirectional_closest
  | Ray_bidirectional_farthest

type ray_surface_hit = Ray_first_surface | Ray_last_surface

type pair_builder = {
  mutable pair_first : int array;
  mutable pair_second : int array;
  mutable pair_length : int;
}

let pair_builder () = {
  pair_first = [||];
  pair_second = [||];
  pair_length = 0;
}

let pair_builder_add builder first second =
  if builder.pair_length = Array.length builder.pair_first then begin
    let capacity = Array.length builder.pair_first in
    if capacity > Sys.max_array_length / 2 then
      invalid_arg "Surface_index candidate cardinality exceeds array limits";
    let next = if capacity = 0 then 16 else capacity * 2 in
    let first_values = Array.make next 0 and second_values = Array.make next 0 in
    Array.blit builder.pair_first 0 first_values 0 capacity;
    Array.blit builder.pair_second 0 second_values 0 capacity;
    builder.pair_first <- first_values;
    builder.pair_second <- second_values
  end;
  builder.pair_first.(builder.pair_length) <- first;
  builder.pair_second.(builder.pair_length) <- second;
  builder.pair_length <- builder.pair_length + 1

let fail_code code message = Error.fail_with ~operation:"surface_index" ~code message
let fail = fail_code "invalid_surface"

let diagonal_count = 4

let[@inline always] diagonal_value axis x y z = match axis with
  | 0 -> (x +. y) +. z
  | 1 -> (x +. y) -. z
  | 2 -> (x -. y) +. z
  | _ -> ((-.x) +. y) +. z

let[@inline always] diagonal_error x y z =
  (8. *. Float.epsilon *. (abs_float x +. abs_float y +. abs_float z))
  +. Int64.float_of_bits 1L

let[@inline always] diagonal_lower axis x y z =
  let value = diagonal_value axis x y z in
  if Float.is_finite value then value -. diagonal_error x y z
  else Float.neg_infinity

let[@inline always] diagonal_upper axis x y z =
  let value = diagonal_value axis x y z in
  if Float.is_finite value then value +. diagonal_error x y z
  else Float.infinity

let[@inline] selected bits index =
  Char.code (Bytes.unsafe_get bits (index lsr 3))
  land (1 lsl (index land 7)) <> 0

let create_with ?cancel ?(grain = 16_384) ?primitives:selection ?vertices
    ?(vertex_selection = All_triangle_vertices) ?(validated_triangles = false)
    geometry =
  Error.run ~operation:"surface_index" ~code:"invalid_parameter" ~cancelled:"surface-index construction was cancelled" @@ fun () ->
  if grain <= 0 then invalid_arg "Surface_index: grain must be positive";
  if validated_triangles && (Option.is_some selection || Option.is_some vertices) then invalid_arg "Surface_index: validated triangle construction cannot select elements";
  Cancel.check_opt cancel;
  let positions = Packed.Float3.View.view (Geometry.positions geometry)
  and topology = Topology.View.view (Geometry.topology geometry) in
  let primitive_count = Bytes.length topology.primitive_kinds in
  (match vertices with
   | Some group when Group.owner group <> Group.Vertex ->
       fail_code "invalid_group" "source vertex selection must be a vertex group"
   | Some group when Group.length group <> Array.length topology.vertex_points ->
       fail_code "invalid_group" "source vertex group length does not match geometry"
   | None | Some _ -> ());
  let selected_primitives = match selection with
    | None -> Array.init primitive_count Fun.id
    | Some group ->
        if Group.owner group <> Group.Primitive then
          fail_code "invalid_group" "source selection must be a primitive group";
        if Group.length group <> primitive_count then
          fail_code "invalid_group" "source primitive group length does not match geometry";
        let selected = Array.make (Group.cardinality group) 0 and next = ref 0 in
        Group.iter (fun primitive -> selected.(!next) <- primitive; incr next) group;
        selected in
  let selected_primitives = match vertices with
    | None -> selected_primitives
    | Some vertices ->
        let vertex_bits = Group.View.bits_view vertices in
        let candidate_count = Array.length selected_primitives
        and chunk = max 1 grain in
        let range_count = Support.ceiling_div candidate_count chunk in
        let keep = Bytes.make candidate_count '\000'
        and range_counts = Array.make range_count 0 in
        if range_count > 0 then Parallel.for_ ~chunk_size:1 ~start:0
            ~finish:(range_count - 1) (fun range ->
              let first = range * chunk
              and last = min candidate_count ((range + 1) * chunk)
              and retained = ref 0 in
              for selected_index = first to last - 1 do
                if selected_index land 4095 = 0 then Cancel.check_opt cancel;
                let primitive = selected_primitives.(selected_index) in
                let vertex = ref topology.primitive_offsets.(primitive)
                and vertex_last = topology.primitive_offsets.(primitive + 1)
                and found = ref false in
                while not !found && !vertex < vertex_last do
                  found := selected vertex_bits !vertex;
                  incr vertex
                done;
                if !found then begin
                  Bytes.unsafe_set keep selected_index '\001';
                  incr retained
                end
              done;
              range_counts.(range) <- !retained);
        let range_offsets = Array.make (range_count + 1) 0 in
        for range = 0 to range_count - 1 do
          range_offsets.(range + 1) <- range_offsets.(range)
              + range_counts.(range)
        done;
        let output = Array.make range_offsets.(range_count) 0 in
        if range_count > 0 then Parallel.for_ ~chunk_size:1 ~start:0
            ~finish:(range_count - 1) (fun range ->
              let first = range * chunk
              and last = min candidate_count ((range + 1) * chunk)
              and next = ref range_offsets.(range) in
              for selected_index = first to last - 1 do
                if selected_index land 4095 = 0 then Cancel.check_opt cancel;
                if Bytes.unsafe_get keep selected_index <> '\000' then begin
                  output.(!next) <- selected_primitives.(selected_index);
                  incr next
                end
              done);
        output in
  let selected_count = Array.length selected_primitives in
  let triangle_offsets =
    if validated_triangles then Array.init (selected_count + 1) Fun.id
    else begin
      let offsets = Array.make (selected_count + 1) 0 in
      for selected = 0 to selected_count - 1 do
        let primitive = selected_primitives.(selected) in
        if primitive land 4095 = 0 then Cancel.check_opt cancel;
        if Bytes.get topology.primitive_kinds primitive <> '\000' then
          fail (Printf.sprintf "primitive %d is a curve, not a polygon" primitive);
        let size = topology.primitive_offsets.(primitive + 1)
            - topology.primitive_offsets.(primitive) in
        if size < 3 then fail (Printf.sprintf
            "primitive %d has fewer than three corners" primitive);
        if size - 2 > Sys.max_array_length - offsets.(selected) then
          fail "triangulated surface exceeds array limits";
        offsets.(selected + 1) <- offsets.(selected) + size - 2
      done;
      offsets
    end in
  let triangles = triangle_offsets.(selected_count) in
  if triangles >= Sys.max_array_length then
    fail "triangulated surface exceeds workspace array limits";
  let primitives, vertex_a, vertex_b, vertex_c =
    if validated_triangles then
      Array.init triangles Fun.id,
      Array.init triangles (fun triangle -> topology.primitive_offsets.(triangle)),
      Array.init triangles (fun triangle -> topology.primitive_offsets.(triangle) + 1),
      Array.init triangles (fun triangle -> topology.primitive_offsets.(triangle) + 2)
    else
      Array.make triangles 0, Array.make triangles 0,
      Array.make triangles 0, Array.make triangles 0 in
  let average_triangles = if selected_count = 0 then 1
    else max 1 (Support.ceiling_div triangles selected_count) in
  let primitive_chunk = max 1 (grain / average_triangles) in
  let primitive_ranges = Support.ceiling_div selected_count primitive_chunk in
  let triangulation_errors = Array.make primitive_ranges None in
  if not validated_triangles && primitive_ranges > 0 then
    Parallel.for_ ~chunk_size:1 ~start:0
      ~finish:(primitive_ranges - 1) (fun range ->
        let first_selected = range * primitive_chunk
        and last_selected = min selected_count ((range + 1) * primitive_chunk) in
        let scratch = Polygon_triangulation.create_scratch () in
        for selected = first_selected to last_selected - 1 do
          if selected land 4095 = 0 then Cancel.check_opt cancel;
          if triangulation_errors.(range) = None then begin
            let primitive = selected_primitives.(selected)
            and offset = triangle_offsets.(selected) in
            let emit local a b c =
              let triangle = offset + local in
              primitives.(triangle) <- primitive;
              vertex_a.(triangle) <- a;
              vertex_b.(triangle) <- b;
              vertex_c.(triangle) <- c in
            match Polygon_triangulation.primitive ?cancel ~positions ~topology
                ~scratch primitive ~emit with
            | Ok () -> ()
            | Error message -> triangulation_errors.(range) <- Some message
          end
        done);
  let triangulation_failure = ref None in
  Array.iter (fun error -> match !triangulation_failure, error with
    | None, Some message -> triangulation_failure := Some message
    | None, None | Some _, _ -> ()) triangulation_errors;
  Option.iter fail !triangulation_failure;
  let triangles, primitives, vertex_a, vertex_b, vertex_c = match vertices with
    | None -> triangles, primitives, vertex_a, vertex_b, vertex_c
    | Some selected_vertices ->
        let selected_vertex_bits = Group.View.bits_view selected_vertices in
        let range_count = Support.ceiling_div triangles grain in
        let keep = Bytes.make triangles '\000'
        and range_counts = Array.make range_count 0 in
        if range_count > 0 then Parallel.for_ ~chunk_size:1 ~start:0
            ~finish:(range_count - 1) (fun range ->
              let first = range * grain
              and last = min triangles ((range + 1) * grain)
              and retained = ref 0 in
              for triangle = first to last - 1 do
                if triangle land 4095 = 0 then Cancel.check_opt cancel;
                let a = selected selected_vertex_bits vertex_a.(triangle)
                and b = selected selected_vertex_bits vertex_b.(triangle)
                and c = selected selected_vertex_bits vertex_c.(triangle) in
                let selected = match vertex_selection with
                  | All_triangle_vertices -> a && b && c
                  | Any_triangle_vertex -> a || b || c in
                if selected then begin
                  Bytes.unsafe_set keep triangle '\001';
                  incr retained
                end
              done;
              range_counts.(range) <- !retained);
        let range_offsets = Array.make (range_count + 1) 0 in
        for range = 0 to range_count - 1 do
          range_offsets.(range + 1) <- range_offsets.(range)
              + range_counts.(range)
        done;
        let retained = range_offsets.(range_count) in
        let output_primitives = Array.make retained 0
        and output_a = Array.make retained 0
        and output_b = Array.make retained 0
        and output_c = Array.make retained 0 in
        if range_count > 0 then Parallel.for_ ~chunk_size:1 ~start:0
            ~finish:(range_count - 1) (fun range ->
              let first = range * grain
              and last = min triangles ((range + 1) * grain)
              and output = ref range_offsets.(range) in
              for triangle = first to last - 1 do
                if triangle land 4095 = 0 then Cancel.check_opt cancel;
                if Bytes.unsafe_get keep triangle <> '\000' then begin
                  output_primitives.(!output) <- primitives.(triangle);
                  output_a.(!output) <- vertex_a.(triangle);
                  output_b.(!output) <- vertex_b.(triangle);
                  output_c.(!output) <- vertex_c.(triangle);
                  incr output
                end
              done);
        retained, output_primitives, output_a, output_b, output_c in
  let centroid_x = Array.make triangles 0. and centroid_y = Array.make triangles 0.
  and centroid_z = Array.make triangles 0.
  and triangle_min_x = Array.make triangles 0.
  and triangle_min_y = Array.make triangles 0.
  and triangle_min_z = Array.make triangles 0.
  and triangle_max_x = Array.make triangles 0.
  and triangle_max_y = Array.make triangles 0.
  and triangle_max_z = Array.make triangles 0.
  and triangle_min_d = Array.init diagonal_count (fun _ -> Array.make triangles 0.)
  and triangle_max_d = Array.init diagonal_count (fun _ -> Array.make triangles 0.) in
  let triangle_ranges = Support.ceiling_div triangles grain in
  let triangle_errors = Array.make triangle_ranges None in
  if triangle_ranges > 0 then Parallel.for_ ~chunk_size:1 ~start:0
      ~finish:(triangle_ranges - 1) (fun range ->
    let first_slot = range * grain and last_slot = min triangles ((range + 1) * grain) in
    for slot = first_slot to last_slot - 1 do
    if slot land 4095 = 0 then Cancel.check_opt cancel;
    if triangle_errors.(range) = None then begin
    let primitive = primitives.(slot) in
    let a = topology.vertex_points.(vertex_a.(slot))
    and b = topology.vertex_points.(vertex_b.(slot))
    and c = topology.vertex_points.(vertex_c.(slot)) in
    let ax = positions.x.(a) and ay = positions.y.(a) and az = positions.z.(a)
    and bx = positions.x.(b) and by = positions.y.(b) and bz = positions.z.(b)
    and cx = positions.x.(c) and cy = positions.y.(c) and cz = positions.z.(c) in
    if not validated_triangles && not (Float.is_finite ax && Float.is_finite ay && Float.is_finite az && Float.is_finite bx && Float.is_finite by
        && Float.is_finite bz && Float.is_finite cx && Float.is_finite cy && Float.is_finite cz) then
      triangle_errors.(range) <- Some
        (Printf.sprintf "primitive %d has a non-finite position" primitive)
    else begin
    let degenerate = if validated_triangles then false else
      let xy = Predicates.orient2d_packed ~x:positions.x ~y:positions.y a b c
      and yz = Predicates.orient2d_packed ~x:positions.y ~y:positions.z a b c
      and zx = Predicates.orient2d_packed ~x:positions.z ~y:positions.x a b c in
      xy = Predicates.Zero && yz = Predicates.Zero && zx = Predicates.Zero in
    if degenerate then
      triangle_errors.(range) <- Some
        (Printf.sprintf "primitive %d is degenerate" primitive)
    else begin
      centroid_x.(slot) <- (ax +. bx +. cx) /. 3.;
      centroid_y.(slot) <- (ay +. by +. cy) /. 3.;
      centroid_z.(slot) <- (az +. bz +. cz) /. 3.;
      triangle_min_x.(slot) <- Float.min ax (Float.min bx cx);
      triangle_min_y.(slot) <- Float.min ay (Float.min by cy);
      triangle_min_z.(slot) <- Float.min az (Float.min bz cz);
      triangle_max_x.(slot) <- Float.max ax (Float.max bx cx);
      triangle_max_y.(slot) <- Float.max ay (Float.max by cy);
      triangle_max_z.(slot) <- Float.max az (Float.max bz cz);
      for axis = 0 to diagonal_count - 1 do
        triangle_min_d.(axis).(slot) <- Float.min
          (diagonal_lower axis ax ay az)
          (Float.min (diagonal_lower axis bx by bz)
            (diagonal_lower axis cx cy cz));
        triangle_max_d.(axis).(slot) <- Float.max
          (diagonal_upper axis ax ay az)
          (Float.max (diagonal_upper axis bx by bz)
            (diagonal_upper axis cx cy cz))
      done
    end
    end
    end
    done);
  let triangle_failure = ref None in
  Array.iter (fun error -> match !triangle_failure, error with
    | None, Some message -> triangle_failure := Some message
    | None, None | Some _, _ -> ()) triangle_errors;
  Option.iter fail !triangle_failure;
  let common_a = ref [||] and common_b = ref [||] and common_c = ref [||]
  and min_d = ref [||] and max_d = ref [||] in
  let extend capacity =
    common_a := Array.make capacity (-1); common_b := Array.make capacity (-1);
    common_c := Array.make capacity (-1);
    min_d := Array.init diagonal_count (fun _ -> Array.make capacity 0.);
    max_d := Array.init diagonal_count (fun _ -> Array.make capacity 0.);
    let common_a = !common_a and common_b = !common_b and common_c = !common_c
    and min_d = !min_d and max_d = !max_d in
    fun node order range_first range_last ->
      for axis = 0 to diagonal_count - 1 do
        min_d.(axis).(node) <- Float.infinity;
        max_d.(axis).(node) <- Float.neg_infinity
      done;
      let first_triangle = order.(range_first) in
      let common0 = ref topology.vertex_points.(vertex_a.(first_triangle))
      and common1 = ref topology.vertex_points.(vertex_b.(first_triangle))
      and common2 = ref topology.vertex_points.(vertex_c.(first_triangle)) in
      for at = range_first to range_last do
        let triangle = order.(at) in
        let a = topology.vertex_points.(vertex_a.(triangle))
        and b = topology.vertex_points.(vertex_b.(triangle))
        and c = topology.vertex_points.(vertex_c.(triangle)) in
        let retain point = point < 0 || point = a || point = b || point = c in
        if not (retain !common0) then common0 := -1;
        if not (retain !common1) then common1 := -1;
        if not (retain !common2) then common2 := -1;
        for axis = 0 to diagonal_count - 1 do
          if triangle_min_d.(axis).(triangle) < min_d.(axis).(node) then
            min_d.(axis).(node) <- triangle_min_d.(axis).(triangle);
          if triangle_max_d.(axis).(triangle) > max_d.(axis).(node) then
            max_d.(axis).(node) <- triangle_max_d.(axis).(triangle)
        done
      done;
      common_a.(node) <- !common0; common_b.(node) <- !common1;
      common_c.(node) <- !common2 in
  let { Bounds3_index.order; min_x; min_y; min_z; max_x; max_y; max_z;
        left; right; first; count } =
    Bounds3_index.create ?cancel ~grain:(max 4_096 grain) ~extend
      ~centroid_x ~centroid_y ~centroid_z
      ~item_min_x:triangle_min_x ~item_min_y:triangle_min_y ~item_min_z:triangle_min_z
      ~item_max_x:triangle_max_x ~item_max_y:triangle_max_y ~item_max_z:triangle_max_z () in
  let capacity = Array.length left
  and common_a = !common_a and common_b = !common_b and common_c = !common_c
  and min_d = !min_d and max_d = !max_d in
  { geometry; positions; topology; primitives; vertex_a; vertex_b; vertex_c; order;
    triangle_min_x; triangle_min_y; triangle_min_z;
    triangle_max_x; triangle_max_y; triangle_max_z;
    triangle_min_d; triangle_max_d;
    min_x; min_y; min_z; max_x; max_y; max_z; min_d; max_d;
    left; right; first; count; common_a; common_b; common_c;
    node_count = capacity }

let create ?cancel ?grain ?primitives ?vertices ?vertex_selection geometry =
  create_with ?cancel ?grain ?primitives ?vertices ?vertex_selection geometry

let create_validated_triangles ?cancel ~grain geometry =
  create_with ?cancel ~grain ~validated_triangles:true geometry

let triangle_count value = Array.length value.primitives

let triangle_bounds_into value triangle output =
  output.(0) <- value.triangle_min_x.(triangle);
  output.(1) <- value.triangle_min_y.(triangle);
  output.(2) <- value.triangle_min_z.(triangle);
  output.(3) <- value.triangle_max_x.(triangle);
  output.(4) <- value.triangle_max_y.(triangle);
  output.(5) <- value.triangle_max_z.(triangle);
  for axis = 0 to diagonal_count - 1 do
    output.(7 + axis) <- value.triangle_min_d.(axis).(triangle);
    output.(11 + axis) <- value.triangle_max_d.(axis).(triangle)
  done

let[@inline always] node_bounds_overlap query surface node =
  let tolerance = query.(6) in
  query.(0) <= surface.max_x.(node) +. tolerance
  && query.(3) >= surface.min_x.(node) -. tolerance
  && query.(1) <= surface.max_y.(node) +. tolerance
  && query.(4) >= surface.min_y.(node) -. tolerance
  && query.(2) <= surface.max_z.(node) +. tolerance
  && query.(5) >= surface.min_z.(node) -. tolerance
  && query.(7) <= surface.max_d.(0).(node) +. tolerance
  && query.(11) >= surface.min_d.(0).(node) -. tolerance
  && query.(8) <= surface.max_d.(1).(node) +. tolerance
  && query.(12) >= surface.min_d.(1).(node) -. tolerance
  && query.(9) <= surface.max_d.(2).(node) +. tolerance
  && query.(13) >= surface.min_d.(2).(node) -. tolerance
  && query.(10) <= surface.max_d.(3).(node) +. tolerance
  && query.(14) >= surface.min_d.(3).(node) -. tolerance

let[@inline always] triangle_bounds_overlap query surface triangle =
  let tolerance = query.(6) in
  query.(0) <= surface.triangle_max_x.(triangle) +. tolerance
  && query.(3) >= surface.triangle_min_x.(triangle) -. tolerance
  && query.(1) <= surface.triangle_max_y.(triangle) +. tolerance
  && query.(4) >= surface.triangle_min_y.(triangle) -. tolerance
  && query.(2) <= surface.triangle_max_z.(triangle) +. tolerance
  && query.(5) >= surface.triangle_min_z.(triangle) -. tolerance
  && query.(7) <= surface.triangle_max_d.(0).(triangle) +. tolerance
  && query.(11) >= surface.triangle_min_d.(0).(triangle) -. tolerance
  && query.(8) <= surface.triangle_max_d.(1).(triangle) +. tolerance
  && query.(12) >= surface.triangle_min_d.(1).(triangle) -. tolerance
  && query.(9) <= surface.triangle_max_d.(2).(triangle) +. tolerance
  && query.(13) >= surface.triangle_min_d.(2).(triangle) -. tolerance
  && query.(10) <= surface.triangle_max_d.(3).(triangle) +. tolerance
  && query.(14) >= surface.triangle_min_d.(3).(triangle) -. tolerance

let triangle_strictly_on_one_side plane_surface plane triangle_surface triangle =
  let point surface triangle local =
    let vertex = match local with
      | 0 -> surface.vertex_a.(triangle)
      | 1 -> surface.vertex_b.(triangle)
      | _ -> surface.vertex_c.(triangle) in
    surface.topology.vertex_points.(vertex) in
  let a = point plane_surface plane 0 and b = point plane_surface plane 1
  and c = point plane_surface plane 2 in
  let sign local =
    let d = point triangle_surface triangle local in
    if plane_surface == triangle_surface then
      Predicates.orient3d_packed
        ~x:plane_surface.positions.x ~y:plane_surface.positions.y
        ~z:plane_surface.positions.z a b c d
    else
      Predicates.orient3d
        ~ax:plane_surface.positions.x.(a) ~ay:plane_surface.positions.y.(a)
        ~az:plane_surface.positions.z.(a)
        ~bx:plane_surface.positions.x.(b) ~by:plane_surface.positions.y.(b)
        ~bz:plane_surface.positions.z.(b)
        ~cx:plane_surface.positions.x.(c) ~cy:plane_surface.positions.y.(c)
        ~cz:plane_surface.positions.z.(c)
        ~dx:triangle_surface.positions.x.(d) ~dy:triangle_surface.positions.y.(d)
        ~dz:triangle_surface.positions.z.(d) in
  match sign 0 with
  | Predicates.Zero -> false
  | (Predicates.Positive | Predicates.Negative) as first ->
      sign 1 = first && sign 2 = first

let exact_plane_overlap left_surface left_triangle right_surface right_triangle =
  not (triangle_strictly_on_one_side left_surface left_triangle
      right_surface right_triangle
    || triangle_strictly_on_one_side right_surface right_triangle
      left_surface left_triangle)

let triangle_has_point surface triangle point =
  if point < 0 then false else
  let points = surface.topology.Topology.View.vertex_points in
  point = points.(surface.vertex_a.(triangle))
  || point = points.(surface.vertex_b.(triangle))
  || point = points.(surface.vertex_c.(triangle))

let triangles_share_point left_surface left_triangle right_surface right_triangle =
  let points = left_surface.topology.Topology.View.vertex_points in
  triangle_has_point right_surface right_triangle
    points.(left_surface.vertex_a.(left_triangle))
  || triangle_has_point right_surface right_triangle
    points.(left_surface.vertex_b.(left_triangle))
  || triangle_has_point right_surface right_triangle
    points.(left_surface.vertex_c.(left_triangle))

let triangle_pairs_of ?cancel ?(single_pass = false)
    ~self ~exact_plane_filter
    ~skip_shared_points ~grain ~tolerance left_surface right_surface =
  if grain <= 0 then invalid_arg
      "Surface_index.overlapping_triangle_pairs: grain must be positive";
  if not (Float.is_finite tolerance) || tolerance < 0. then invalid_arg
      "Surface_index.overlapping_triangle_pairs: tolerance must be finite and non-negative";
  let left_count = Array.length left_surface.primitives in
  if left_count = 0 || Array.length right_surface.primitives = 0 then [||], [||]
  else begin
    let shared_topology =
      left_surface.topology.Topology.View.vertex_points
      == right_surface.topology.Topology.View.vertex_points in
    (* One-pass collection owns a builder per stable range. A lower bound keeps
       empty/disjoint workloads O(F / 128) in builder metadata even when the
       caller requests grain one; output order remains triangle-major and is
       therefore independent of this scheduling subdivision. *)
    let range_size = if single_pass then max 128 grain else max 1 grain in
    let range_count = Support.ceiling_div left_count range_size in
    let scan node_stack left_bounds left_triangle output_left
        output_right output_at builder =
      triangle_bounds_into left_surface left_triangle left_bounds;
      left_bounds.(6) <- tolerance;
      let top = ref 1 and found = ref 0 in
      node_stack.(0) <- 0;
      while !top > 0 do
        decr top;
        let node = node_stack.(!top) in
        let shared_node = skip_shared_points && shared_topology
            && (triangle_has_point left_surface left_triangle
                  right_surface.common_a.(node)
              || triangle_has_point left_surface left_triangle
                  right_surface.common_b.(node)
              || triangle_has_point left_surface left_triangle
                  right_surface.common_c.(node)) in
        if not shared_node && node_bounds_overlap left_bounds right_surface node then
          if right_surface.count.(node) > 0 then begin
            let last = right_surface.first.(node) + right_surface.count.(node) in
            for slot = right_surface.first.(node) to last - 1 do
              let right_triangle = right_surface.order.(slot) in
              if (not self || (right_triangle > left_triangle
                  && right_surface.primitives.(right_triangle)
                     <> left_surface.primitives.(left_triangle)))
                  && (not (skip_shared_points && shared_topology)
                    || not (triangles_share_point left_surface left_triangle
                      right_surface right_triangle))
                  && triangle_bounds_overlap left_bounds right_surface right_triangle
                  && (not exact_plane_filter || exact_plane_overlap
                    left_surface left_triangle right_surface right_triangle)
                then begin
                (match builder, output_left, output_right with
                 | Some output, _, _ ->
                     pair_builder_add output left_triangle right_triangle
                 | None, Some left_output, Some right_output ->
                     left_output.(output_at + !found) <- left_triangle;
                     right_output.(output_at + !found) <- right_triangle
                 | None, None, None -> ()
                 | _ -> assert false);
                incr found
              end
            done
          end else begin
            node_stack.(!top) <- right_surface.right.(node);
            node_stack.(!top + 1) <- right_surface.left.(node);
            top := !top + 2
          end
      done;
      !found in
    if single_pass then begin
      let builders = Array.init range_count (fun _ -> pair_builder ()) in
      Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(range_count - 1)
        (fun range ->
          let node_stack = Array.make 65 0
          and left_bounds = Array.make 15 0.
          and output = builders.(range) in
          let first = range * range_size
          and last = min left_count ((range + 1) * range_size) in
          for triangle = first to last - 1 do
            if triangle land 4095 = 0 then Cancel.check_opt cancel;
            ignore (scan node_stack left_bounds triangle None None 0 (Some output))
          done);
      let offsets = Array.make (range_count + 1) 0 in
      for range = 0 to range_count - 1 do
        if builders.(range).pair_length > Sys.max_array_length - offsets.(range) then
          invalid_arg "Surface_index candidate cardinality exceeds array limits";
        offsets.(range + 1) <- offsets.(range) + builders.(range).pair_length
      done;
      let pair_count = offsets.(range_count) in
      let first_output = Array.make pair_count 0
      and second_output = Array.make pair_count 0 in
      for range = 0 to range_count - 1 do
        let output = builders.(range) and at = offsets.(range) in
        Array.blit output.pair_first 0 first_output at output.pair_length;
        Array.blit output.pair_second 0 second_output at output.pair_length
      done;
      first_output, second_output
    end else begin
    let counts = Array.make left_count 0 in
    Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(range_count - 1)
      (fun range ->
        let node_stack = Array.make 65 0 in
        let left_bounds = Array.make 15 0. in
        let first = range * range_size
        and last = min left_count ((range + 1) * range_size) in
        for triangle = first to last - 1 do
          if triangle land 4095 = 0 then Cancel.check_opt cancel;
          counts.(triangle) <- scan node_stack left_bounds
              triangle None None 0 None
        done);
    let offsets = Array.make (left_count + 1) 0 in
    for triangle = 0 to left_count - 1 do
      if counts.(triangle) > Sys.max_array_length - offsets.(triangle) then
        invalid_arg "Surface_index.overlapping_triangle_pairs: candidate cardinality exceeds array limits";
      offsets.(triangle + 1) <- offsets.(triangle) + counts.(triangle)
    done;
    let pair_count = offsets.(left_count) in
    let left_output = Array.make pair_count 0
    and right_output = Array.make pair_count 0 in
    Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(range_count - 1)
      (fun range ->
        let node_stack = Array.make 65 0 in
        let left_bounds = Array.make 15 0. in
        let first = range * range_size
        and last = min left_count ((range + 1) * range_size) in
        for triangle = first to last - 1 do
          if triangle land 4095 = 0 then Cancel.check_opt cancel;
          let written = scan node_stack left_bounds triangle
              (Some left_output) (Some right_output) offsets.(triangle) None in
          if written <> counts.(triangle) then invalid_arg
              "Surface_index.overlapping_triangle_pairs: count/fill drift"
        done);
    left_output, right_output
    end
  end

let overlapping_triangle_pairs ?cancel ~grain ~tolerance left_surface
    right_surface =
  triangle_pairs_of ?cancel ~self:false ~exact_plane_filter:false
    ~skip_shared_points:false ~grain ~tolerance
    left_surface right_surface

let overlapping_self_triangle_pairs ?cancel ~grain ~tolerance surface =
  triangle_pairs_of ?cancel ~self:true ~exact_plane_filter:false
    ~skip_shared_points:false ~grain ~tolerance surface surface

let overlapping_self_triangle_pairs_disjoint_topology ?cancel ~grain ~tolerance
    surface =
  let triangles = Array.length surface.primitives
  and primitive_count = Bytes.length surface.topology.primitive_kinds in
  let original_by_primitive = Array.make primitive_count (-1)
  and eligible = ref (triangles = primitive_count) in
  for triangle = 0 to triangles - 1 do
    let primitive = surface.primitives.(triangle) in
    if primitive < 0 || primitive >= primitive_count
        || surface.topology.primitive_offsets.(primitive + 1)
           - surface.topology.primitive_offsets.(primitive) <> 3
        || original_by_primitive.(primitive) >= 0 then
      eligible := false
    else original_by_primitive.(primitive) <- triangle
  done;
  let map_pairs indexed first second =
    let count = Array.length first in
    let mapped_first = Array.make count 0 and mapped_second = Array.make count 0 in
    for pair = 0 to count - 1 do
      let a = original_by_primitive.(indexed.primitives.(first.(pair)))
      and b = original_by_primitive.(indexed.primitives.(second.(pair))) in
      if a < 0 || b < 0 then invalid_arg
          "Surface_index disjoint-topology primitive map is incomplete";
      if a < b then begin
        mapped_first.(pair) <- a; mapped_second.(pair) <- b
      end else begin
        mapped_first.(pair) <- b; mapped_second.(pair) <- a
      end
    done;
    mapped_first, mapped_second in
  let concatenate (af, as_) (bf, bs) =
    let ac = Array.length af and bc = Array.length bf in
    if ac > Sys.max_array_length - bc then invalid_arg
        "Surface_index disjoint-topology candidate cardinality exceeds array limits";
    let first = Array.make (ac + bc) 0 and second = Array.make (ac + bc) 0 in
    Array.blit af 0 first 0 ac; Array.blit as_ 0 second 0 ac;
    Array.blit bf 0 first ac bc; Array.blit bs 0 second ac bc;
    first, second in
  let subset indexed point incident =
    let bits = Bytes.make ((primitive_count + 7) / 8) '\000' in
    for triangle = 0 to Array.length indexed.primitives - 1 do
      if triangle_has_point indexed triangle point = incident then begin
        let primitive = indexed.primitives.(triangle) in
        let slot = primitive lsr 3 and mask = 1 lsl (primitive land 7) in
        Bytes.unsafe_set bits slot
          (Char.chr (Char.code (Bytes.unsafe_get bits slot) lor mask))
      end
    done;
    let group = Group.View.of_owned_bits ~owner:Group.Primitive
        ~name:"__rdk_surface_disjoint_partition" ~length:primitive_count bits in
    Error.get (create_with ?cancel ~grain ~primitives:group indexed.geometry) in
  let rec gather indexed =
    Cancel.check_opt cancel;
    let count = Array.length indexed.primitives in
    if count < 2 then [||], [||]
    else begin
      let incidences = Array.make (Array.length indexed.positions.x) 0 in
      for triangle = 0 to count - 1 do
        let points = indexed.topology.vertex_points in
        let a = points.(indexed.vertex_a.(triangle))
        and b = points.(indexed.vertex_b.(triangle))
        and c = points.(indexed.vertex_c.(triangle)) in
        incidences.(a) <- incidences.(a) + 1;
        incidences.(b) <- incidences.(b) + 1;
        incidences.(c) <- incidences.(c) + 1
      done;
      let point = ref 0 in
      for candidate = 1 to Array.length incidences - 1 do
        if incidences.(candidate) > incidences.(!point) then point := candidate
      done;
      let incidence = incidences.(!point) in
      if incidence >= 32 && incidence * 4 >= count then begin
        let incident = subset indexed !point true
        and remainder = subset indexed !point false in
        let cross =
          if Array.length remainder.primitives = 0 then [||], [||]
          else
            let first, second = triangle_pairs_of ?cancel ~self:false
                ~single_pass:true ~exact_plane_filter:false
                ~skip_shared_points:true ~grain ~tolerance
                incident remainder in
            let count = Array.length first in
            let mapped_first = Array.make count 0
            and mapped_second = Array.make count 0 in
            for pair = 0 to count - 1 do
              let a = original_by_primitive.(incident.primitives.(first.(pair)))
              and b = original_by_primitive.(remainder.primitives.(second.(pair))) in
              if a < b then begin
                mapped_first.(pair) <- a; mapped_second.(pair) <- b
              end else begin
                mapped_first.(pair) <- b; mapped_second.(pair) <- a
              end
            done;
            mapped_first, mapped_second in
        concatenate cross (gather remainder)
      end else
        let first, second = triangle_pairs_of ?cancel ~self:true
            ~single_pass:true ~exact_plane_filter:false
            ~skip_shared_points:true ~grain ~tolerance
            indexed indexed in
        map_pairs indexed first second
    end in
  if !eligible then gather surface
  else triangle_pairs_of ?cancel ~self:true ~single_pass:true
      ~exact_plane_filter:false ~skip_shared_points:true ~grain ~tolerance
      surface surface

let[@inline] aabb_distance_squared value node qx qy qz =
  let dx = if qx < value.min_x.(node) then value.min_x.(node) -. qx
    else if qx > value.max_x.(node) then qx -. value.max_x.(node) else 0.
  and dy = if qy < value.min_y.(node) then value.min_y.(node) -. qy
    else if qy > value.max_y.(node) then qy -. value.max_y.(node) else 0.
  and dz = if qz < value.min_z.(node) then value.min_z.(node) -. qz
    else if qz > value.max_z.(node) then qz -. value.max_z.(node) else 0. in
  (dx *. dx) +. (dy *. dy) +. (dz *. dz)

let[@inline] closest_triangle_into value triangle qx qy qz scratch =
  let a = value.topology.Topology.View.vertex_points.(value.vertex_a.(triangle))
  and b = value.topology.vertex_points.(value.vertex_b.(triangle))
  and c = value.topology.vertex_points.(value.vertex_c.(triangle)) in
  let ax = value.positions.x.(a) and ay = value.positions.y.(a)
  and az = value.positions.z.(a) and bx = value.positions.x.(b)
  and by = value.positions.y.(b) and bz = value.positions.z.(b)
  and cx = value.positions.x.(c) and cy = value.positions.y.(c)
  and cz = value.positions.z.(c) in
  let abx = bx -. ax and aby = by -. ay and abz = bz -. az
  and acx = cx -. ax and acy = cy -. ay and acz = cz -. az
  and apx = qx -. ax and apy = qy -. ay and apz = qz -. az in
  let d1 = (abx *. apx) +. (aby *. apy) +. (abz *. apz)
  and d2 = (acx *. apx) +. (acy *. apy) +. (acz *. apz) in
  if d1 <= 0. && d2 <= 0. then begin
    scratch.(5) <- 1.; scratch.(6) <- 0.; scratch.(7) <- 0.
  end
    else begin
      let bpx = qx -. bx and bpy = qy -. by and bpz = qz -. bz in
      let d3 = (abx *. bpx) +. (aby *. bpy) +. (abz *. bpz)
      and d4 = (acx *. bpx) +. (acy *. bpy) +. (acz *. bpz) in
      if d3 >= 0. && d4 <= d3 then begin
        scratch.(5) <- 0.; scratch.(6) <- 1.; scratch.(7) <- 0.
      end
      else begin
        let vc = (d1 *. d4) -. (d3 *. d2) in
        if vc <= 0. && d1 >= 0. && d3 <= 0. then begin
          let v = d1 /. (d1 -. d3) in
          scratch.(5) <- 1. -. v; scratch.(6) <- v; scratch.(7) <- 0.
        end
        else begin
          let cpx = qx -. cx and cpy = qy -. cy and cpz = qz -. cz in
          let d5 = (abx *. cpx) +. (aby *. cpy) +. (abz *. cpz)
          and d6 = (acx *. cpx) +. (acy *. cpy) +. (acz *. cpz) in
          if d6 >= 0. && d5 <= d6 then begin
            scratch.(5) <- 0.; scratch.(6) <- 0.; scratch.(7) <- 1.
          end
          else begin
            let vb = (d5 *. d2) -. (d1 *. d6) in
            if vb <= 0. && d2 >= 0. && d6 <= 0. then begin
              let w = d2 /. (d2 -. d6) in
              scratch.(5) <- 1. -. w; scratch.(6) <- 0.; scratch.(7) <- w
            end
            else begin
              let va = (d3 *. d6) -. (d5 *. d4) in
              if va <= 0. && (d4 -. d3) >= 0. && (d5 -. d6) >= 0. then begin
                let w = (d4 -. d3) /. ((d4 -. d3) +. (d5 -. d6)) in
                scratch.(5) <- 0.; scratch.(6) <- 1. -. w; scratch.(7) <- w
              end else begin
                let denominator = 1. /. (va +. vb +. vc) in
                let v = vb *. denominator and w = vc *. denominator in
                scratch.(5) <- 1. -. v -. w;
                scratch.(6) <- v; scratch.(7) <- w
              end
            end
          end
        end
      end
    end;
  let u = scratch.(5) and v = scratch.(6) and w = scratch.(7) in
  let px = (u *. ax) +. (v *. bx) +. (w *. cx)
  and py = (u *. ay) +. (v *. by) +. (w *. cy)
  and pz = (u *. az) +. (v *. bz) +. (w *. cz) in
  let dx = qx -. px and dy = qy -. py and dz = qz -. pz in
  scratch.(4) <- (dx *. dx) +. (dy *. dy) +. (dz *. dz)

let query_into value qx qy qz maximum scratch best_primitive best_triangle
    primitive_out triangle_out a_out b_out c_out d_out query =
  best_primitive.(0) <- -1;
  best_triangle.(0) <- -1;
  scratch.(0) <- maximum;
  scratch.(1) <- 0.; scratch.(2) <- 0.; scratch.(3) <- 0.;
  let rec visit node =
    if aabb_distance_squared value node qx qy qz <= scratch.(0) then
      if value.count.(node) > 0 then
        for at = value.first.(node) to value.first.(node) + value.count.(node) - 1 do
          let triangle = value.order.(at) in
          let primitive = value.primitives.(triangle) in
          closest_triangle_into value triangle qx qy qz scratch;
          let distance = scratch.(4) in
          if distance < scratch.(0)
             || (distance = scratch.(0)
                 && (best_primitive.(0) < 0
                     || primitive < best_primitive.(0)
                     || (primitive = best_primitive.(0)
                         && triangle < best_triangle.(0)))) then begin
            best_primitive.(0) <- primitive;
            best_triangle.(0) <- triangle;
            scratch.(0) <- distance;
            scratch.(1) <- scratch.(5); scratch.(2) <- scratch.(6);
            scratch.(3) <- scratch.(7)
          end
        done
      else begin
        let left = value.left.(node) and right = value.right.(node) in
        let left_distance = aabb_distance_squared value left qx qy qz
        and right_distance = aabb_distance_squared value right qx qy qz in
        if left_distance < right_distance
           || (left_distance = right_distance && left < right) then begin
          visit left; visit right
        end else begin visit right; visit left end
      end in
  if value.node_count > 0 then visit 0;
  primitive_out.(query) <- best_primitive.(0);
  triangle_out.(query) <- best_triangle.(0);
  a_out.(query) <- scratch.(1); b_out.(query) <- scratch.(2);
  c_out.(query) <- scratch.(3);
  d_out.(query) <- if best_primitive.(0) < 0 then Float.infinity else scratch.(0)

let closest_many_into ?cancel ?selection ?position_indices ~grain value ~queries
    ~max_distance_squared
    ~primitives ~triangles ~barycentric_a ~barycentric_b ~barycentric_c
    ~distances_squared =
  if grain <= 0 then invalid_arg "Surface_index: grain must be positive";
  let position_count = Packed.Float3.length queries in
  let query_count = match position_indices with
    | None -> position_count | Some values -> Array.length values in
  (match selection with
   | Some group when Group.length group <> query_count ->
       invalid_arg "Surface_index: query selection length does not match queries"
   | None | Some _ -> ());
  (match position_indices with
   | None -> ()
   | Some values ->
       Array.iter (fun point -> if point < 0 || point >= position_count then
         invalid_arg "Surface_index: query position index is out of bounds") values);
  if Array.length primitives < query_count || Array.length triangles < query_count
     || Array.length barycentric_a < query_count
     || Array.length barycentric_b < query_count
     || Array.length barycentric_c < query_count
     || Array.length distances_squared < query_count then
    invalid_arg "Surface_index: output arrays are too short";
  let queries = Packed.Float3.View.view queries in
  let range_count = (query_count + grain - 1) / grain in
  if range_count > 0 then Parallel.for_ ~chunk_size:1 ~start:0
      ~finish:(range_count - 1) (fun range ->
    let first = range * grain and last = min query_count ((range + 1) * grain) in
    let scratch = Array.make 8 0. and best_primitive = [|-1|]
    and best_triangle = [|-1|] in
    for query = first to last - 1 do
      if query land 4095 = 0 then Cancel.check_opt cancel;
      if (match selection with None -> true
          | Some group -> Group.mem query group) then begin
        let point = match position_indices with None -> query
          | Some values -> values.(query) in
        let qx = queries.x.(point) and qy = queries.y.(point)
        and qz = queries.z.(point) in
        if not (Float.is_finite qx && Float.is_finite qy && Float.is_finite qz) then
          invalid_arg "Surface_index: query positions must be finite";
        query_into value qx qy qz max_distance_squared scratch best_primitive
          best_triangle primitives triangles barycentric_a barycentric_b
          barycentric_c distances_squared query
      end
    done)

let query_distance_squared_into value qx qy qz maximum scratch output query =
  scratch.(0) <- maximum;
  scratch.(1) <- 0.;
  let rec visit node =
    if aabb_distance_squared value node qx qy qz <= scratch.(0) then
      if value.count.(node) > 0 then
        for at = value.first.(node) to value.first.(node) + value.count.(node) - 1 do
          closest_triangle_into value value.order.(at) qx qy qz scratch;
          if scratch.(4) < scratch.(0) then begin
            scratch.(0) <- scratch.(4);
            scratch.(1) <- 1.
          end
        done
      else begin
        let left = value.left.(node) and right = value.right.(node) in
        let left_distance = aabb_distance_squared value left qx qy qz
        and right_distance = aabb_distance_squared value right qx qy qz in
        if left_distance < right_distance
            || (left_distance = right_distance && left < right) then begin
          visit left; visit right
        end else begin visit right; visit left end
      end in
  if value.node_count > 0 then visit 0;
  output.(query) <- if scratch.(1) = 0. then Float.infinity else scratch.(0)

let closest_distances_many_into ?cancel ?selection ~grain value
    ~queries ~max_distance_squared ~distances_squared =
  if grain <= 0 then invalid_arg "Surface_index: grain must be positive";
  let query_count = Packed.Float3.length queries in
  (match selection with
   | Some group when Group.owner group <> Group.Point
       || Group.length group <> query_count ->
       invalid_arg "Surface_index: query selection must be a matching point group"
   | None | Some _ -> ());
  if Array.length distances_squared < query_count then
    invalid_arg "Surface_index: output array is too short";
  let queries = Packed.Float3.View.view queries in
  let range_count = (query_count + grain - 1) / grain in
  if range_count > 0 then Parallel.for_ ~chunk_size:1 ~start:0
      ~finish:(range_count - 1) (fun range ->
    let first = range * grain and last = min query_count ((range + 1) * grain) in
    let scratch = Array.make 8 0. in
    for query = first to last - 1 do
      if query land 4095 = 0 then Cancel.check_opt cancel;
      if match selection with None -> true | Some group -> Group.mem query group
      then begin
        let qx = queries.x.(query) and qy = queries.y.(query)
        and qz = queries.z.(query) in
        if not (Float.is_finite qx && Float.is_finite qy && Float.is_finite qz) then
          invalid_arg "Surface_index: query positions must be finite";
        query_distance_squared_into value qx qy qz max_distance_squared scratch
          distances_squared query
      end
    done)

let maximum_squared_distance = sqrt max_float

let[@inline] ray_aabb_intersects value node ox oy oz dx dy dz minimum maximum
    tolerance =
  let xmin = value.min_x.(node) -. tolerance
  and ymin = value.min_y.(node) -. tolerance
  and zmin = value.min_z.(node) -. tolerance
  and xmax = value.max_x.(node) +. tolerance
  and ymax = value.max_y.(node) +. tolerance
  and zmax = value.max_z.(node) +. tolerance in
  let x_near = if dx > 0. then (xmin -. ox) /. dx
    else if dx < 0. then (xmax -. ox) /. dx
    else Float.neg_infinity
  and x_far = if dx > 0. then (xmax -. ox) /. dx
    else if dx < 0. then (xmin -. ox) /. dx
    else Float.infinity in
  if dx = 0. && (ox < xmin || ox > xmax) then false
  else
    let y_near = if dy > 0. then (ymin -. oy) /. dy
      else if dy < 0. then (ymax -. oy) /. dy
      else Float.neg_infinity
    and y_far = if dy > 0. then (ymax -. oy) /. dy
      else if dy < 0. then (ymin -. oy) /. dy
      else Float.infinity in
    if dy = 0. && (oy < ymin || oy > ymax) then false
    else
      let z_near = if dz > 0. then (zmin -. oz) /. dz
        else if dz < 0. then (zmax -. oz) /. dz
        else Float.neg_infinity
      and z_far = if dz > 0. then (zmax -. oz) /. dz
        else if dz < 0. then (zmin -. oz) /. dz
        else Float.infinity in
      if dz = 0. && (oz < zmin || oz > zmax) then false
      else
        let entry = Float.max minimum
            (Float.max x_near (Float.max y_near z_near))
        and exit = Float.min maximum
            (Float.min x_far (Float.min y_far z_far)) in
        entry <= exit

let[@inline] ray_triangle_into value triangle ox oy oz dx dy dz minimum maximum
    tolerance scratch =
  let a = value.topology.Topology.View.vertex_points.(value.vertex_a.(triangle))
  and b = value.topology.vertex_points.(value.vertex_b.(triangle))
  and c = value.topology.vertex_points.(value.vertex_c.(triangle)) in
  let ax = value.positions.x.(a) and ay = value.positions.y.(a)
  and az = value.positions.z.(a) and bx = value.positions.x.(b)
  and by = value.positions.y.(b) and bz = value.positions.z.(b)
  and cx = value.positions.x.(c) and cy = value.positions.y.(c)
  and cz = value.positions.z.(c) in
  let e1x = bx -. ax and e1y = by -. ay and e1z = bz -. az
  and e2x = cx -. ax and e2y = cy -. ay and e2z = cz -. az in
  let px = (dy *. e2z) -. (dz *. e2y)
  and py = (dz *. e2x) -. (dx *. e2z)
  and pz = (dx *. e2y) -. (dy *. e2x) in
  let determinant = (e1x *. px) +. (e1y *. py) +. (e1z *. pz) in
  let e1_length = Float.hypot e1x (Float.hypot e1y e1z)
  and e2_length = Float.hypot e2x (Float.hypot e2y e2z) in
  let determinant_scale = e1_length *. e2_length in
  if determinant_scale = 0. || not (Float.is_finite determinant_scale)
      || abs_float determinant <= Float.epsilon *. determinant_scale then false
  else
    let inverse = 1. /. determinant in
    let tx = ox -. ax and ty = oy -. ay and tz = oz -. az in
    let u = ((tx *. px) +. (ty *. py) +. (tz *. pz)) *. inverse in
    let qx = (ty *. e1z) -. (tz *. e1y)
    and qy = (tz *. e1x) -. (tx *. e1z)
    and qz = (tx *. e1y) -. (ty *. e1x) in
    let v = ((dx *. qx) +. (dy *. qy) +. (dz *. qz)) *. inverse in
    let distance = ((e2x *. qx) +. (e2y *. qy) +. (e2z *. qz)) *. inverse in
    if not (Float.is_finite u && Float.is_finite v && Float.is_finite distance)
        || distance < minimum || distance > maximum then false
    else if u >= 0. && v >= 0. && u +. v <= 1. then begin
      scratch.(0) <- distance;
      scratch.(1) <- 1. -. u -. v; scratch.(2) <- u; scratch.(3) <- v;
      true
    end else if tolerance = 0. then false
    else begin
      let qx = ox +. (distance *. dx)
      and qy = oy +. (distance *. dy)
      and qz = oz +. (distance *. dz) in
      closest_triangle_into value triangle qx qy qz scratch;
      if scratch.(4) > tolerance *. tolerance then false
      else begin
        scratch.(0) <- distance;
        scratch.(1) <- scratch.(5); scratch.(2) <- scratch.(6);
        scratch.(3) <- scratch.(7);
        true
      end
    end

let ray_query_once value ox oy oz dx dy dz minimum maximum tolerance surface_hit
    scratch node_stack integer_results float_results result =
  let integer = result * 2 and floating = result * 4 in
  integer_results.(integer) <- -1;
  integer_results.(integer + 1) <- -1;
  float_results.(floating) <- (match surface_hit with
    | Ray_first_surface -> Float.infinity
    | Ray_last_surface -> Float.neg_infinity);
  float_results.(floating + 1) <- 0.;
  float_results.(floating + 2) <- 0.;
  float_results.(floating + 3) <- 0.;
  let top_slot = Array.length node_stack - 1 in
  if value.node_count > 0 then begin
    node_stack.(0) <- 0; node_stack.(top_slot) <- 1
  end else node_stack.(top_slot) <- 0;
  while node_stack.(top_slot) > 0 do
    let top = node_stack.(top_slot) - 1 in
    node_stack.(top_slot) <- top;
    let node = node_stack.(top) in
    let bound = match surface_hit with
      | Ray_first_surface -> Float.min maximum float_results.(floating)
      | Ray_last_surface -> maximum in
    if ray_aabb_intersects value node ox oy oz dx dy dz minimum bound tolerance
    then
      if value.count.(node) > 0 then
        for at = value.first.(node) to value.first.(node) + value.count.(node) - 1 do
          let triangle = value.order.(at) in
          if ray_triangle_into value triangle ox oy oz dx dy dz minimum bound
              tolerance scratch then begin
            let primitive = value.primitives.(triangle)
            and distance = scratch.(0) in
            let current = float_results.(floating) in
            let better = match surface_hit with
              | Ray_first_surface -> distance < current
              | Ray_last_surface -> distance > current in
            if better || (distance = current
                && (integer_results.(integer) < 0
                    || primitive < integer_results.(integer)
                    || (primitive = integer_results.(integer)
                        && triangle < integer_results.(integer + 1)))) then begin
              integer_results.(integer) <- primitive;
              integer_results.(integer + 1) <- triangle;
              float_results.(floating) <- distance;
              float_results.(floating + 1) <- scratch.(1);
              float_results.(floating + 2) <- scratch.(2);
              float_results.(floating + 3) <- scratch.(3)
            end
          end
        done
      else begin
        if top > top_slot - 2 then
          invalid_arg "Surface_index: ray traversal stack overflow";
        node_stack.(top) <- value.right.(node);
        node_stack.(top + 1) <- value.left.(node);
        node_stack.(top_slot) <- top + 2
      end
  done

type ray_directions =
  | Constant_direction of { x : float; y : float; z : float }
  | Per_query_directions of Packed.Float3.View.view

type sample_combine =
  | Sample_average
  | Sample_median
  | Sample_shortest
  | Sample_longest

let[@inline] direction_at directions query = match directions with
  | Constant_direction { x; y; z } -> x, y, z
  | Per_query_directions values ->
      values.x.(query), values.y.(query), values.z.(query)

let[@inline] normalized_direction vx vy vz =
  let scale = Float.max (abs_float vx)
      (Float.max (abs_float vy) (abs_float vz)) in
  if not (Float.is_finite vx && Float.is_finite vy && Float.is_finite vz) || scale = 0. then
    invalid_arg
      "Surface_index: selected ray directions must be finite and non-zero";
  let sx = vx /. scale and sy = vy /. scale and sz = vz /. scale in
  let inverse = 1. /. sqrt ((sx *. sx) +. (sy *. sy) +. (sz *. sz)) in
  sx *. inverse, sy *. inverse, sz *. inverse

let[@inline] jitter_stream query sample component =
  (query * 0x1e3779b97f4a7c15)
  lxor (sample * 0x14d049bb133111eb)
  lxor (component * 0x11b54a32d192ed03)

let jittered_direction seed query sample jitter_scale dx dy dz output =
  if sample = 0 || jitter_scale = 0. then begin
    output.(0) <- dx; output.(1) <- dy; output.(2) <- dz
  end else begin
    let inverse_xy = 1. /. Float.hypot dx dy in
    let ux, uy, uz = if Float.is_finite inverse_xy then
        -.dy *. inverse_xy, dx *. inverse_xy, 0.
      else 1., 0., 0. in
    let vx = (dy *. uz) -. (dz *. uy)
    and vy = (dz *. ux) -. (dx *. uz)
    and vz = (dx *. uy) -. (dy *. ux) in
    let random = Rays_math.Rand.seed seed in
    let radius = sqrt (Rays_math.Rand.float_at random
        ~index:(jitter_stream query sample 0)) *. jitter_scale
    and angle = 2. *. Float.pi *. Rays_math.Rand.float_at random
        ~index:(jitter_stream query sample 1) in
    let tangent_u = radius *. cos angle
    and tangent_v = radius *. sin angle in
    let scale = Float.max 1.
        (Float.max (abs_float tangent_u) (abs_float tangent_v)) in
    let tangent_u = tangent_u /. scale and tangent_v = tangent_v /. scale in
    let x = (dx /. scale) +. (tangent_u *. ux) +. (tangent_v *. vx)
    and y = (dy /. scale) +. (tangent_u *. uy) +. (tangent_v *. vy)
    and z = (dz /. scale) +. (tangent_u *. uz) +. (tangent_v *. vz) in
    let inverse = 1. /. sqrt ((x *. x) +. (y *. y) +. (z *. z)) in
    output.(0) <- x *. inverse; output.(1) <- y *. inverse;
    output.(2) <- z *. inverse
  end

let[@inline] sample_distance float_results result = float_results.(result * 4)
let[@inline] sample_hit integer_results result = integer_results.(result * 2) >= 0

let[@inline] compare_samples float_results left right =
  let left_distance = float_results.(left * 4)
  and right_distance = float_results.(right * 4) in
  if left_distance < right_distance then -1
  else if left_distance > right_distance then 1
  else Int.compare left right

let sample_swap values left right =
  if left <> right then begin
    let value = values.(left) in values.(left) <- values.(right);
    values.(right) <- value
  end

let sample_sift_down float_results values root count state =
  state.(0) <- root; state.(1) <- 1;
  while state.(1) <> 0 do
    let child = (state.(0) * 2) + 1 in
    if child >= count then state.(1) <- 0
    else begin
      let selected = if child + 1 < count
          && compare_samples float_results values.(child) values.(child + 1) < 0
        then child + 1 else child in
      if compare_samples float_results values.(state.(0)) values.(selected) < 0
      then begin sample_swap values state.(0) selected; state.(0) <- selected end
      else state.(1) <- 0
    end
  done

let sort_samples float_results values count state =
  for root = (count / 2) - 1 downto 0 do
    sample_sift_down float_results values root count state
  done;
  for remaining = count - 1 downto 1 do
    sample_swap values 0 remaining;
    sample_sift_down float_results values 0 remaining state
  done

let combine_samples combine integer_results float_results first samples order
    sort_state combined_counts combined_distances combined_selected direction =
  let count = ref 0 and mean = ref 0. and shortest = ref (-1)
  and longest = ref (-1) in
  for sample = 0 to samples - 1 do
    let result = first + sample in
    if sample_hit integer_results result then begin
      order.(!count) <- result; incr count;
      let distance = sample_distance float_results result in
      mean := !mean +. ((distance -. !mean) /. float_of_int !count);
      if !shortest < 0 || distance < sample_distance float_results !shortest
      then shortest := result;
      if !longest < 0 || distance > sample_distance float_results !longest
      then longest := result
    end
  done;
  combined_counts.(direction) <- !count;
  if !count > 0 then begin
    let distance, selected = match combine with
      | Sample_shortest -> sample_distance float_results !shortest, !shortest
      | Sample_longest -> sample_distance float_results !longest, !longest
      | Sample_median ->
          sort_samples float_results order !count sort_state;
          let selected = order.(!count / 2) in
          sample_distance float_results selected, selected
      | Sample_average ->
          let distance = !mean in
          let selected = ref order.(0) and difference = ref Float.infinity in
          for slot = 0 to !count - 1 do
            let candidate = order.(slot) in
            let candidate_difference = abs_float
                (sample_distance float_results candidate -. distance) in
            if candidate_difference < !difference then begin
              difference := candidate_difference; selected := candidate
            end
          done;
          distance, !selected in
    combined_distances.(direction) <- distance;
    combined_selected.(direction) <- selected
  end

let geometric_normal_into value triangle output =
  let a = value.topology.Topology.View.vertex_points.(value.vertex_a.(triangle))
  and b = value.topology.vertex_points.(value.vertex_b.(triangle))
  and c = value.topology.vertex_points.(value.vertex_c.(triangle)) in
  let ax = value.positions.x.(a) and ay = value.positions.y.(a)
  and az = value.positions.z.(a) and bx = value.positions.x.(b)
  and by = value.positions.y.(b) and bz = value.positions.z.(b)
  and cx = value.positions.x.(c) and cy = value.positions.y.(c)
  and cz = value.positions.z.(c) in
  let scale = Float.max
      (Float.max (abs_float ax) (Float.max (abs_float ay) (abs_float az)))
      (Float.max
        (Float.max (abs_float bx) (Float.max (abs_float by) (abs_float bz)))
        (Float.max (abs_float cx) (Float.max (abs_float cy) (abs_float cz)))) in
  let scale = if scale = 0. then 1. else scale in
  let abx = (bx /. scale) -. (ax /. scale)
  and aby = (by /. scale) -. (ay /. scale)
  and abz = (bz /. scale) -. (az /. scale)
  and acx = (cx /. scale) -. (ax /. scale)
  and acy = (cy /. scale) -. (ay /. scale)
  and acz = (cz /. scale) -. (az /. scale) in
  let nx = (aby *. acz) -. (abz *. acy)
  and ny = (abz *. acx) -. (abx *. acz)
  and nz = (abx *. acy) -. (aby *. acx) in
  let length = Float.hypot nx (Float.hypot ny nz) in
  if length = 0. || not (Float.is_finite length) then begin
    output.(0) <- 0.; output.(1) <- 0.; output.(2) <- 0.
  end else begin
    output.(0) <- nx /. length; output.(1) <- ny /. length;
    output.(2) <- nz /. length
  end

let validate_sample_arrays query_count ~primitives ~triangles ~barycentric_a
    ~barycentric_b ~barycentric_c ~distances ~direction_signs ~hit_counts =
  if Array.length primitives < query_count || Array.length triangles < query_count
      || Array.length barycentric_a < query_count
      || Array.length barycentric_b < query_count
      || Array.length barycentric_c < query_count
      || Array.length distances < query_count
      || Array.length direction_signs < query_count
      || Array.length hit_counts < query_count then
    invalid_arg "Surface_index: sampled ray output arrays are too short"

let raycast_samples_into ?cancel ?selection ~grain value ~queries ~directions
    ~min_distance ~max_distance ~tolerance ~direction_mode ~surface_hit
    ~samples ~jitter_scale ~seed ~combine ~primitives ~triangles ~barycentric_a
    ~barycentric_b ~barycentric_c ~distances ~direction_signs ~hit_counts
    ~normal_x ~normal_y ~normal_z =
  if grain <= 0 then invalid_arg "Surface_index: grain must be positive";
  if samples < 2 || samples > 1024 then
    invalid_arg "Surface_index: sampled ray count must be between 2 and 1024";
  if not (Float.is_finite jitter_scale && jitter_scale >= 0.) then
    invalid_arg "Surface_index: ray jitter scale must be finite and non-negative";
  let query_count = Packed.Float3.length queries in
  validate_sample_arrays query_count ~primitives ~triangles ~barycentric_a
    ~barycentric_b ~barycentric_c ~distances ~direction_signs ~hit_counts;
  (match directions with
   | Per_query_directions values when Array.length values.x <> query_count
       || Array.length values.y <> query_count
       || Array.length values.z <> query_count ->
       invalid_arg "Surface_index: ray direction count does not match queries"
   | Constant_direction _ | Per_query_directions _ -> ());
  (match selection with
   | Some group when Group.owner group <> Group.Point
       || Group.length group <> query_count ->
       invalid_arg "Surface_index: ray selection must match query points"
   | None | Some _ -> ());
  let requested_normals = match normal_x, normal_y, normal_z with
    | None, None, None -> false
    | Some x, Some y, Some z ->
        if Array.length x < query_count || Array.length y < query_count
            || Array.length z < query_count then
          invalid_arg "Surface_index: sampled ray normal arrays are too short";
        true
    | _ -> invalid_arg "Surface_index: sampled ray normal arrays must be supplied together" in
  let queries = Packed.Float3.View.view queries in
  let range_count = Support.ceiling_div query_count grain in
  if range_count > 0 then Parallel.for_ ~chunk_size:1 ~start:0
      ~finish:(range_count - 1) (fun range ->
    let first = range * grain and last = min query_count ((range + 1) * grain) in
    let result_count = samples * 2 in
    let scratch = Array.make 8 0. and node_stack = Array.make 65 0
    and integer_results = Array.make (result_count * 2) (-1)
    and float_results = Array.make (result_count * 4) 0.
    and order = Array.make samples 0 and direction = Array.make 3 0.
    and sort_state = Array.make 2 0
    and normal = Array.make 3 0. and combined_counts = Array.make 2 0
    and combined_distances = Array.make 2 Float.infinity
    and combined_selected = Array.make 2 (-1) in
    for query = first to last - 1 do
      if query land 4095 = 0 then Cancel.check_opt cancel;
      primitives.(query) <- -1; triangles.(query) <- -1;
      barycentric_a.(query) <- 0.; barycentric_b.(query) <- 0.;
      barycentric_c.(query) <- 0.; distances.(query) <- Float.infinity;
      direction_signs.(query) <- 0; hit_counts.(query) <- 0;
      if match selection with None -> true | Some group -> Group.mem query group then begin
        let ox = queries.x.(query) and oy = queries.y.(query)
        and oz = queries.z.(query) in
        if not (Float.is_finite ox && Float.is_finite oy && Float.is_finite oz) then
          invalid_arg "Surface_index: selected ray origins must be finite";
        let vx, vy, vz = direction_at directions query in
        let dx, dy, dz = normalized_direction vx vy vz in
        let directions_to_query = match direction_mode with
          | Ray_forward | Ray_reverse -> 1
          | Ray_bidirectional_closest | Ray_bidirectional_farthest -> 2 in
        for side = 0 to directions_to_query - 1 do
          let sign = match direction_mode with
            | Ray_reverse -> -1.
            | Ray_forward -> 1.
            | Ray_bidirectional_closest | Ray_bidirectional_farthest ->
                if side = 0 then 1. else -1. in
          let base = side * samples in
          for sample = 0 to samples - 1 do
            if sample land 63 = 0 then Cancel.check_opt cancel;
            jittered_direction seed query sample jitter_scale
              (sign *. dx) (sign *. dy) (sign *. dz) direction;
            ray_query_once value ox oy oz direction.(0) direction.(1) direction.(2)
              min_distance max_distance tolerance surface_hit scratch node_stack
              integer_results float_results (base + sample)
          done;
          combine_samples combine integer_results float_results base samples order
            sort_state
            combined_counts combined_distances combined_selected side
        done;
        let chosen = if directions_to_query = 1 then 0
          else if combined_counts.(0) = 0 then 1
          else if combined_counts.(1) = 0 then 0
          else match direction_mode with
            | Ray_bidirectional_closest ->
                if combined_distances.(1) < combined_distances.(0) then 1 else 0
            | Ray_bidirectional_farthest ->
                if combined_distances.(1) > combined_distances.(0) then 1 else 0
            | Ray_forward | Ray_reverse -> assert false in
        let selected = combined_selected.(chosen) in
        if selected >= 0 then begin
          let integer = selected * 2 and floating = selected * 4 in
          primitives.(query) <- integer_results.(integer);
          triangles.(query) <- integer_results.(integer + 1);
          barycentric_a.(query) <- float_results.(floating + 1);
          barycentric_b.(query) <- float_results.(floating + 2);
          barycentric_c.(query) <- float_results.(floating + 3);
          distances.(query) <- combined_distances.(chosen);
          hit_counts.(query) <- combined_counts.(chosen);
          direction_signs.(query) <- (match direction_mode with
            | Ray_reverse -> -1 | Ray_forward -> 1
            | Ray_bidirectional_closest | Ray_bidirectional_farthest ->
                if chosen = 0 then 1 else -1);
          if requested_normals then begin
            let nx = ref 0. and ny = ref 0. and nz = ref 0. in
            if combine = Sample_average then begin
              let base = chosen * samples in
              for sample = 0 to samples - 1 do
                let result = base + sample in
                if sample_hit integer_results result then begin
                  geometric_normal_into value integer_results.((result * 2) + 1)
                    normal;
                  nx := !nx +. normal.(0); ny := !ny +. normal.(1);
                  nz := !nz +. normal.(2)
                end
              done;
              let inverse = 1. /. float_of_int combined_counts.(chosen) in
              nx := !nx *. inverse; ny := !ny *. inverse; nz := !nz *. inverse
            end else begin
              geometric_normal_into value triangles.(query) normal;
              nx := normal.(0); ny := normal.(1); nz := normal.(2)
            end;
            (Option.get normal_x).(query) <- !nx;
            (Option.get normal_y).(query) <- !ny;
            (Option.get normal_z).(query) <- !nz
          end
        end
      end
    done)

let raycast_average_drivers_into ?cancel ?selection ~grain value ~queries
    ~directions ~min_distance ~max_distance ~tolerance ~surface_hit ~samples
    ~jitter_scale ~seed ~direction_signs ~hit_counts ~offsets ~vertex_numbers
    ~vertex_weights =
  let query_count = Packed.Float3.length queries in
  if Array.length direction_signs < query_count
      || Array.length hit_counts < query_count
      || Array.length offsets <> query_count + 1
      || Array.length vertex_numbers < offsets.(query_count)
      || Array.length vertex_weights < offsets.(query_count) then
    invalid_arg "Surface_index: average ray driver arrays are inconsistent";
  let queries = Packed.Float3.View.view queries in
  let range_count = Support.ceiling_div query_count grain in
  if range_count > 0 then Parallel.for_ ~chunk_size:1 ~start:0
      ~finish:(range_count - 1) (fun range ->
    let first = range * grain and last = min query_count ((range + 1) * grain) in
    let scratch = Array.make 8 0. and node_stack = Array.make 65 0
    and integer_results = Array.make 2 (-1)
    and float_results = Array.make 4 0. and direction = Array.make 3 0. in
    for query = first to last - 1 do
      if query land 4095 = 0 then Cancel.check_opt cancel;
      if direction_signs.(query) <> 0
          && (match selection with None -> true | Some group -> Group.mem query group)
      then begin
        let ox = queries.x.(query) and oy = queries.y.(query)
        and oz = queries.z.(query) in
        let vx, vy, vz = direction_at directions query in
        let dx, dy, dz = normalized_direction vx vy vz in
        let sign = float_of_int direction_signs.(query)
        and scale = 1. /. float_of_int hit_counts.(query) in
        let at = ref offsets.(query) in
        for sample = 0 to samples - 1 do
          if sample land 63 = 0 then Cancel.check_opt cancel;
          jittered_direction seed query sample jitter_scale
            (sign *. dx) (sign *. dy) (sign *. dz) direction;
          ray_query_once value ox oy oz direction.(0) direction.(1) direction.(2)
            min_distance max_distance tolerance surface_hit scratch node_stack
            integer_results float_results 0;
          if integer_results.(0) >= 0 then begin
            let triangle = integer_results.(1) and floating = 0 in
            vertex_numbers.(!at) <- value.vertex_a.(triangle);
            vertex_numbers.(!at + 1) <- value.vertex_b.(triangle);
            vertex_numbers.(!at + 2) <- value.vertex_c.(triangle);
            vertex_weights.(!at) <- float_results.(floating + 1) *. scale;
            vertex_weights.(!at + 1) <- float_results.(floating + 2) *. scale;
            vertex_weights.(!at + 2) <- float_results.(floating + 3) *. scale;
            at := !at + 3
          end
        done;
        if !at <> offsets.(query + 1) then
          invalid_arg "Surface_index: average ray driver recount differed"
      end
    done)

let raycast_many_into ?cancel ?selection ~grain value ~queries ~directions
    ~min_distance ~max_distance ~tolerance ~direction_mode ~surface_hit
    ~primitives ~triangles ~barycentric_a ~barycentric_b ~barycentric_c
    ~distances =
  if grain <= 0 then invalid_arg "Surface_index: grain must be positive";
  let query_count = Packed.Float3.length queries in
  (match directions with
   | Per_query_directions values when Array.length values.x <> query_count
       || Array.length values.y <> query_count
       || Array.length values.z <> query_count ->
       invalid_arg "Surface_index: ray direction count does not match queries"
   | Constant_direction _ | Per_query_directions _ -> ());
  (match selection with
   | Some group when Group.owner group <> Group.Point
       || Group.length group <> query_count ->
       invalid_arg "Surface_index: ray selection must match query points"
   | None | Some _ -> ());
  if not (Float.is_finite min_distance && min_distance >= 0.) then
    invalid_arg "Surface_index: minimum ray distance must be finite and non-negative";
  if not ((Float.is_finite max_distance || max_distance = Float.infinity)
      && max_distance >= min_distance) then
    invalid_arg "Surface_index: maximum ray distance must be at least the minimum";
  if not (Float.is_finite tolerance && tolerance >= 0.
      && tolerance <= maximum_squared_distance) then
    invalid_arg "Surface_index: ray tolerance must be finite, non-negative, and safely squarable";
  if Array.length primitives < query_count || Array.length triangles < query_count
      || Array.length barycentric_a < query_count
      || Array.length barycentric_b < query_count
      || Array.length barycentric_c < query_count
      || Array.length distances < query_count then
    invalid_arg "Surface_index: ray output arrays are too short";
  let queries = Packed.Float3.View.view queries in
  let range_count = Support.ceiling_div query_count grain in
  if range_count > 0 then Parallel.for_ ~chunk_size:1 ~start:0
      ~finish:(range_count - 1) (fun range ->
    let first = range * grain and last = min query_count ((range + 1) * grain) in
    let scratch = Array.make 8 0.
    and node_stack = Array.make 65 0
    and integer_results = Array.make 4 (-1)
    and float_results = Array.make 8 0. in
    for query = first to last - 1 do
      if query land 4095 = 0 then Cancel.check_opt cancel;
      primitives.(query) <- -1; triangles.(query) <- -1;
      barycentric_a.(query) <- 0.; barycentric_b.(query) <- 0.;
      barycentric_c.(query) <- 0.; distances.(query) <- Float.infinity;
      if match selection with None -> true | Some group -> Group.mem query group then begin
        let ox = queries.x.(query) and oy = queries.y.(query)
        and oz = queries.z.(query) in
        let vx = match directions with
          | Constant_direction { x; _ } -> x
          | Per_query_directions values -> values.x.(query)
        and vy = match directions with
          | Constant_direction { y; _ } -> y
          | Per_query_directions values -> values.y.(query)
        and vz = match directions with
          | Constant_direction { z; _ } -> z
          | Per_query_directions values -> values.z.(query) in
        let scale = Float.max (abs_float vx)
            (Float.max (abs_float vy) (abs_float vz)) in
        if not (Float.is_finite ox && Float.is_finite oy && Float.is_finite oz) then
          invalid_arg "Surface_index: selected ray origins must be finite";
        if not (Float.is_finite vx && Float.is_finite vy && Float.is_finite vz) || scale = 0. then
          invalid_arg
            "Surface_index: selected ray directions must be finite and non-zero";
        let sx = vx /. scale and sy = vy /. scale and sz = vz /. scale in
        let inverse = 1. /. sqrt ((sx *. sx) +. (sy *. sy) +. (sz *. sz)) in
        let dx = sx *. inverse and dy = sy *. inverse and dz = sz *. inverse in
        (match direction_mode with
         | Ray_forward ->
             ray_query_once value ox oy oz dx dy dz min_distance max_distance
               tolerance surface_hit scratch node_stack integer_results
               float_results 0
         | Ray_reverse ->
             ray_query_once value ox oy oz (-.dx) (-.dy) (-.dz) min_distance
               max_distance tolerance surface_hit scratch node_stack
               integer_results float_results 0
         | Ray_bidirectional_closest | Ray_bidirectional_farthest ->
             ray_query_once value ox oy oz dx dy dz min_distance max_distance
               tolerance surface_hit scratch node_stack integer_results
               float_results 0;
             ray_query_once value ox oy oz (-.dx) (-.dy) (-.dz) min_distance
               max_distance tolerance surface_hit scratch node_stack
               integer_results float_results 1);
        let chosen = match direction_mode with
          | Ray_forward | Ray_reverse -> 0
          | Ray_bidirectional_closest | Ray_bidirectional_farthest ->
              let forward = integer_results.(0) >= 0
              and reverse = integer_results.(2) >= 0 in
              if not forward then 1
              else if not reverse then 0
              else
                let fd = float_results.(0) and rd = float_results.(4) in
                match direction_mode with
                | Ray_bidirectional_closest -> if rd < fd then 1 else 0
                | Ray_bidirectional_farthest -> if rd > fd then 1 else 0
                | Ray_forward | Ray_reverse -> assert false in
        let integer = chosen * 2 and floating = chosen * 4 in
        if integer_results.(integer) >= 0 then begin
          primitives.(query) <- integer_results.(integer);
          triangles.(query) <- integer_results.(integer + 1);
          barycentric_a.(query) <- float_results.(floating + 1);
          barycentric_b.(query) <- float_results.(floating + 2);
          barycentric_c.(query) <- float_results.(floating + 3);
          distances.(query) <- float_results.(floating)
        end
      end
    done)

let raycast value ~origin ~direction =
  try
    let queries = Packed.Float3.View.of_owned_exn ~x:[|origin.Rays_math.Vec3.x|]
        ~y:[|origin.y|] ~z:[|origin.z|]
    in
    let primitive = [|-1|] and triangle = [|-1|]
    and a = [|0.|] and b = [|0.|] and c = [|0.|]
    and distance = [|Float.infinity|] in
    raycast_many_into ~grain:1 value ~queries
      ~directions:(Constant_direction {
        x = direction.Rays_math.Vec3.x; y = direction.y; z = direction.z })
      ~min_distance:0. ~max_distance:Float.infinity ~tolerance:0.
      ~direction_mode:Ray_forward ~surface_hit:Ray_first_surface
      ~primitives:primitive ~triangles:triangle ~barycentric_a:a
      ~barycentric_b:b ~barycentric_c:c ~distances:distance;
    if primitive.(0) < 0 then Ok None else Ok (Some {
      primitive = primitive.(0); distance = distance.(0) })
  with Invalid_argument message -> Error (Error.make ~operation:"surface_index"
      ~code:"invalid_ray" message)

module View = struct
  type nonrec ray_directions = ray_directions =
    | Constant_direction of { x : float; y : float; z : float }
    | Per_query_directions of Packed.Float3.View.view
  type nonrec sample_combine = sample_combine =
    | Sample_average
    | Sample_median
    | Sample_shortest
    | Sample_longest

  let closest_many_into = closest_many_into
  let create_validated_triangles = create_validated_triangles
  let closest_distances_many_into = closest_distances_many_into
  let raycast_many_into = raycast_many_into
  let raycast_samples_into = raycast_samples_into
  let raycast_average_drivers_into = raycast_average_drivers_into
  let[@inline] triangle_vertex value triangle local =
    if local = 0 then value.vertex_a.(triangle)
    else if local = 1 then value.vertex_b.(triangle)
    else value.vertex_c.(triangle)
  let[@inline] triangle_point value triangle local =
    value.topology.vertex_points.(triangle_vertex value triangle local)
  let[@inline] triangle_primitive value triangle = value.primitives.(triangle)
  let overlapping_triangle_pairs = overlapping_triangle_pairs
  let overlapping_self_triangle_pairs = overlapping_self_triangle_pairs
  let overlapping_self_triangle_pairs_disjoint_topology =
    overlapping_self_triangle_pairs_disjoint_topology
end
