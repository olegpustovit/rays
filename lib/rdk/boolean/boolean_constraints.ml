open Rays_math

let operation = "boolean_constraints"

type side = Left | Right

type t = {
  left_geometry : Geometry.t;
  right_geometry : Geometry.t;
  resolve_left_self_intersections : bool;
  resolve_right_self_intersections : bool;
  left_surface : Surface_index.t;
  right_surface : Surface_index.t;
  source : Implicit_point.source;
  source_points : Implicit_point.t array;
  left_triangle_points : int array;
  right_triangle_points : int array;
  points : Implicit_point.t array;

  first : int array;
  second : int array;
  constraint_first_sides : bytes;
  constraint_first_triangles : int array;
  constraint_second_sides : bytes;
  constraint_second_triangles : int array;
  left_offsets : int array;
  left_constraints : int array;
  right_offsets : int array;
  right_constraints : int array;
  coplanar_first_sides : bytes;
  coplanar_first_triangles : int array;
  coplanar_second_sides : bytes;
  coplanar_second_triangles : int array;

  degenerate_pairs : int;
}

let error code message = Error (Error.make ~operation ~code message)

let point_count value = Array.length value.points
let constraint_count value = Array.length value.first
let constraint_first value constraint_index = value.first.(constraint_index)
let constraint_second value constraint_index = value.second.(constraint_index)
let side_at sides index = if Bytes.unsafe_get sides index = '\000' then Left else Right
let constraint_first_side value constraint_index =
  side_at value.constraint_first_sides constraint_index
let constraint_second_side value constraint_index =
  side_at value.constraint_second_sides constraint_index
let constraint_first_triangle value constraint_index =
  value.constraint_first_triangles.(constraint_index)
let constraint_second_triangle value constraint_index =
  value.constraint_second_triangles.(constraint_index)
let left_triangle_count value = Array.length value.left_offsets - 1
let right_triangle_count value = Array.length value.right_offsets - 1
let left_constraint_range value triangle =
  value.left_offsets.(triangle), value.left_offsets.(triangle + 1)
let right_constraint_range value triangle =
  value.right_offsets.(triangle), value.right_offsets.(triangle + 1)
let left_constraint value slot = value.left_constraints.(slot)
let right_constraint value slot = value.right_constraints.(slot)
let coplanar_pair_count value = Array.length value.coplanar_first_triangles
let coplanar_first_side value pair = side_at value.coplanar_first_sides pair
let coplanar_second_side value pair = side_at value.coplanar_second_sides pair
let coplanar_first_triangle value pair = value.coplanar_first_triangles.(pair)
let coplanar_second_triangle value pair = value.coplanar_second_triangles.(pair)
let degenerate_pair_count value = value.degenerate_pairs

module Private = struct
  let left_geometry value = value.left_geometry
  let right_geometry value = value.right_geometry
  let resolve_left_self_intersections value = value.resolve_left_self_intersections
  let resolve_right_self_intersections value = value.resolve_right_self_intersections
  let left_surface value = value.left_surface
  let right_surface value = value.right_surface
  let point value point = value.points.(point)
  let source value = value.source
  let source_point_count value = Array.length value.source_points
  let source_point value point = value.source_points.(point)
  let left_triangle_point value triangle local =
    value.left_triangle_points.((triangle * 3) + local)
  let right_triangle_point value triangle local =
    value.right_triangle_points.((triangle * 3) + local)
  let triangle_point value side triangle local = match side with
    | Left -> left_triangle_point value triangle local
    | Right -> right_triangle_point value triangle local
end

let concatenate_positions left right =
  let left = Packed.Float3.Private.view (Geometry.positions left)
  and right = Packed.Float3.Private.view (Geometry.positions right) in
  let left_count = Array.length left.x and right_count = Array.length right.x in
  if left_count > Sys.max_array_length - right_count then
    invalid_arg "Boolean constraint point cardinality exceeds array limits";
  let count = left_count + right_count in
  let x = Array.make count 0. and y = Array.make count 0. and z = Array.make count 0. in
  Array.blit left.x 0 x 0 left_count;
  Array.blit left.y 0 y 0 left_count;
  Array.blit left.z 0 z 0 left_count;
  Array.blit right.x 0 x left_count right_count;
  Array.blit right.y 0 y left_count right_count;
  Array.blit right.z 0 z left_count right_count;
  (left_count, x, y, z)

let all_triangle_points topology surface offset =
  let triangle_count = Surface_index.triangle_count surface in
  let output = Array.make (triangle_count * 3) 0 in
  for triangle = 0 to triangle_count - 1 do
    for local = 0 to 2 do
      let vertex = Surface_index.Private.triangle_vertex surface triangle local in
      output.((triangle * 3) + local) <-
        offset + topology.Topology.Private.vertex_points.(vertex)
    done
  done;
  output

let local_vertex triangle local = triangle.(local)
let local_edge triangle edge = triangle.(edge), triangle.((edge + 1) mod 3)

let edge_constant_components source triangle edge =
  let first, second = local_edge triangle edge in
  Implicit_point.constant_components source first second

let construct_event source source_points left_triangle right_triangle
    left_feature right_feature =
  let left_vertex = Predicates.Private.triangle_feature_vertex_local left_feature
  and right_vertex = Predicates.Private.triangle_feature_vertex_local right_feature in
  if left_vertex >= 0 then Ok source_points.(local_vertex left_triangle left_vertex)
  else if right_vertex >= 0 then
    Ok source_points.(local_vertex right_triangle right_vertex)
  else
      let construct triangle edge plane =
        let line_start, line_end = local_edge triangle edge in
        Implicit_point.line_plane source ~line_start ~line_end
          ~plane_a:plane.(0) ~plane_b:plane.(1) ~plane_c:plane.(2) in
      let left_edge = Predicates.Private.triangle_feature_edge_local left_feature
      and right_edge = Predicates.Private.triangle_feature_edge_local right_feature in
      if left_edge >= 0 && right_edge >= 0
          && edge_constant_components source right_triangle right_edge
             > edge_constant_components source left_triangle left_edge then
           (match construct right_triangle right_edge left_triangle with
            | Ok _ as point -> point
            | Error Implicit_point.Parallel_line_and_plane ->
                construct left_triangle left_edge right_triangle
            | Error _ as failure -> failure)
      else if left_edge >= 0 then
           (match construct left_triangle left_edge right_triangle with
            | Ok _ as point -> point
            | Error Implicit_point.Parallel_line_and_plane ->
                if right_edge >= 0 then
                  construct right_triangle right_edge left_triangle
                else Error Implicit_point.Parallel_line_and_plane
            | Error _ as failure -> failure)
      else if right_edge >= 0 then construct right_triangle right_edge left_triangle
      else Error Implicit_point.Dependent_planes

let compare_points points left right =
  let left_point = Option.get points.(left)
  and right_point = Option.get points.(right) in
  let comparison = Implicit_point.compare_x left_point right_point in
  if comparison <> 0 then comparison else
  let comparison = Implicit_point.compare_y left_point right_point in
  if comparison <> 0 then comparison else
  let comparison = Implicit_point.compare_z left_point right_point in
  if comparison <> 0 then comparison else Int.compare left right

let compact_pairs counts expected first_candidate_sides first_candidates
    second_candidate_sides second_candidates =
  let first_sides = Bytes.make expected '\000'
  and second_sides = Bytes.make expected '\000'
  and first = Array.make expected 0 and second = Array.make expected 0
  and output = ref 0 in
  for candidate = 0 to Array.length counts - 1 do
    if counts.(candidate) = -1 then begin
      Bytes.unsafe_set first_sides !output
        (Bytes.unsafe_get first_candidate_sides candidate);
      Bytes.unsafe_set second_sides !output
        (Bytes.unsafe_get second_candidate_sides candidate);
      first.(!output) <- first_candidates.(candidate);
      second.(!output) <- second_candidates.(candidate);
      incr output
    end
  done;
  first_sides, first, second_sides, second

let make_face_csr target_side triangle_count first_sides first_triangles
    second_sides second_triangles =
  let counts = Array.make triangle_count 0 in
  for constraint_index = 0 to Array.length first_triangles - 1 do
    if side_at first_sides constraint_index = target_side then begin
      let triangle = first_triangles.(constraint_index) in
      counts.(triangle) <- counts.(triangle) + 1
    end;
    if side_at second_sides constraint_index = target_side then begin
      let triangle = second_triangles.(constraint_index) in
      counts.(triangle) <- counts.(triangle) + 1
    end
  done;
  let offsets = Array.make (triangle_count + 1) 0 in
  for triangle = 0 to triangle_count - 1 do
    offsets.(triangle + 1) <- offsets.(triangle) + counts.(triangle)
  done;
  let values = Array.make offsets.(triangle_count) 0 and cursors = Array.copy offsets in
  for constraint_index = 0 to Array.length first_triangles - 1 do
    let add triangle =
      values.(cursors.(triangle)) <- constraint_index;
      cursors.(triangle) <- cursors.(triangle) + 1 in
    if side_at first_sides constraint_index = target_side then
      add first_triangles.(constraint_index);
    if side_at second_sides constraint_index = target_side then
      add second_triangles.(constraint_index)
  done;
  offsets, values

let event_explicit_point first_triangle second_triangle first_feature second_feature =
  let first = Predicates.Private.triangle_feature_vertex_local first_feature in
  if first >= 0 then first_triangle.(first) else
  let second = Predicates.Private.triangle_feature_vertex_local second_feature in
  if second >= 0 then second_triangle.(second) else -1

let ordinary_topology_contact first_triangle second_triangle count
    feature0 feature1 feature2 feature3 =
  if count <= 0 then false else begin
    let b0 = second_triangle.(0) and b1 = second_triangle.(1)
    and b2 = second_triangle.(2) in
    let common point = point = b0 || point = b1 || point = b2 in
    let c0 = if common first_triangle.(0) then first_triangle.(0) else -1
    and c1 = if common first_triangle.(1) then first_triangle.(1) else -1
    and c2 = if common first_triangle.(2) then first_triangle.(2) else -1 in
    let common_count = (if c0 >= 0 then 1 else 0)
        + (if c1 >= 0 then 1 else 0) + (if c2 >= 0 then 1 else 0) in
    let is_common point = point = c0 || point = c1 || point = c2 in
    let first = event_explicit_point first_triangle second_triangle feature0 feature1 in
    if count = 1 then first >= 0 && is_common first
    else if count = 2 && common_count >= 2 then
      let second = event_explicit_point first_triangle second_triangle feature2 feature3 in
      first >= 0 && second >= 0 && first <> second
      && is_common first && is_common second
    else false
  end

let ordinary_shared_edge_only ~x ~y ~z first_triangle second_triangle =
  let b0 = second_triangle.(0) and b1 = second_triangle.(1)
  and b2 = second_triangle.(2) in
  let in_second point = point = b0 || point = b1 || point = b2 in
  let c0 = if in_second first_triangle.(0) then first_triangle.(0) else -1
  and c1 = if in_second first_triangle.(1) then first_triangle.(1) else -1
  and c2 = if in_second first_triangle.(2) then first_triangle.(2) else -1 in
  let shared_count = (if c0 >= 0 then 1 else 0)
      + (if c1 >= 0 then 1 else 0) + (if c2 >= 0 then 1 else 0) in
  if shared_count <> 2 then false else begin
    let first = if c0 >= 0 then c0 else c1
    and second = if c2 >= 0 then c2 else c1 in
    let first_other = if not (first_triangle.(0) = first
        || first_triangle.(0) = second) then first_triangle.(0)
      else if not (first_triangle.(1) = first || first_triangle.(1) = second)
      then first_triangle.(1) else first_triangle.(2) in
    let second_other = if not (b0 = first || b0 = second) then b0
      else if not (b1 = first || b1 = second) then b1 else b2 in
    match Predicates.orient3d_packed ~x ~y ~z first second first_other second_other with
    | Predicates.Positive | Predicates.Negative -> true
    | Predicates.Zero ->
        let a_sign, b_sign =
          let a = Predicates.orient2d_packed ~x ~y first second first_other in
          if a <> Predicates.Zero then
            a, Predicates.orient2d_packed ~x ~y first second second_other
          else
            let a = Predicates.orient2d_packed ~x:y ~y:z first second first_other in
            if a <> Predicates.Zero then
              a, Predicates.orient2d_packed ~x:y ~y:z first second second_other
            else
              Predicates.orient2d_packed ~x:z ~y:x first second first_other,
              Predicates.orient2d_packed ~x:z ~y:x first second second_other in
        (match a_sign, b_sign with
         | Predicates.Positive, Predicates.Negative
         | Predicates.Negative, Predicates.Positive -> true
         | _ -> false)
  end

let opposite_duplicate first_triangle second_triangle =
  let a = first_triangle.(0) and b = first_triangle.(1)
  and c = first_triangle.(2) and x = second_triangle.(0)
  and y = second_triangle.(1) and z = second_triangle.(2) in
  (a = x && b = z && c = y)
  || (a = y && b = x && c = z)
  || (a = z && b = y && c = x)

let build ?cancel ?(resolve_left_self_intersections = false)
    ?(resolve_right_self_intersections = false)
    ?(ignore_opposite_duplicate_self_pairs = false)
    ~grain ~left ~right () =
  try
    if grain <= 0 then error "invalid_parameter" "grain must be positive"
    else
      let left_surface, right_surface = Parallel.both
          (fun () -> Surface_index.create ?cancel ~grain left)
          (fun () -> Surface_index.create ?cancel ~grain right) in
      match left_surface with
      | Error failure -> Error failure
      | Ok left_surface ->
          match right_surface with
          | Error failure -> Error failure
          | Ok right_surface ->
              let cross_first, cross_second =
                Surface_index.Private.overlapping_triangle_pairs ?cancel ~grain
                  ~tolerance:0. left_surface right_surface in
              let left_self_first, left_self_second =
                if resolve_left_self_intersections then
                  Surface_index.Private.overlapping_self_triangle_pairs
                    ?cancel ~grain ~tolerance:0. left_surface
                else [||], [||] in
              let right_self_first, right_self_second =
                if resolve_right_self_intersections then
                  Surface_index.Private.overlapping_self_triangle_pairs
                    ?cancel ~grain ~tolerance:0. right_surface
                else [||], [||] in
              let cross_count = Array.length cross_first
              and left_self_count = Array.length left_self_first
              and right_self_count = Array.length right_self_first in
              if cross_count > Sys.max_array_length - left_self_count
                  || cross_count + left_self_count > Sys.max_array_length - right_self_count
              then invalid_arg "Boolean candidate cardinality exceeds array limits";
              let candidate_count = cross_count + left_self_count + right_self_count in
              let first_candidate_sides = Bytes.make candidate_count '\000'
              and second_candidate_sides = Bytes.make candidate_count '\000' in
              Bytes.fill second_candidate_sides 0 cross_count '\001';
              let first_candidates, second_candidates =
                if left_self_count = 0 && right_self_count = 0 then
                  (* The common clean-input path borrows the cross broad-phase
                     planes directly. Besides skipping two BVH traversals this
                     avoids copying both candidate arrays. *)
                  cross_first, cross_second
                else begin
                  let first = Array.make candidate_count 0
                  and second = Array.make candidate_count 0 in
                  Array.blit cross_first 0 first 0 cross_count;
                  Array.blit cross_second 0 second 0 cross_count;
                  Array.blit left_self_first 0 first cross_count left_self_count;
                  Array.blit left_self_second 0 second cross_count left_self_count;
                  let right_start = cross_count + left_self_count in
                  Array.blit right_self_first 0 first right_start right_self_count;
                  Array.blit right_self_second 0 second right_start right_self_count;
                  Bytes.fill first_candidate_sides right_start right_self_count '\001';
                  Bytes.fill second_candidate_sides right_start right_self_count '\001';
                  first, second
                end in
              let left_offset, x, y, z = concatenate_positions left right in
              let source = match Implicit_point.source ~x ~y ~z with
                | Ok source -> source
                | Error _ -> invalid_arg "Boolean constraint source is invalid" in
              let source_points = Array.init (Array.length x) (fun point ->
                  Implicit_point.explicit source point |> Result.get_ok) in
              let left_topology = Topology.Private.view (Geometry.topology left)
              and right_topology = Topology.Private.view (Geometry.topology right) in
              let left_triangle_points =
                all_triangle_points left_topology left_surface 0
              and right_triangle_points =
                all_triangle_points right_topology right_surface left_offset in
              let counts = Array.make candidate_count 0
              and feature0 = Array.make candidate_count 0
              and feature1 = Array.make candidate_count 0
              and feature2 = Array.make candidate_count 0
              and feature3 = Array.make candidate_count 0 in
              let chunk = max 1 grain in
              let ranges = (candidate_count + chunk - 1) / chunk in
              if ranges > 0 then Parallel.for_ ~chunk_size:1 ~start:0
                  ~finish:(ranges - 1) (fun range ->
                let scratch = Array.make 4 0 and left_triangle = Array.make 3 0
                and right_triangle = Array.make 3 0 in
                let first = range * chunk
                and last = min candidate_count ((range + 1) * chunk) in
                for candidate = first to last - 1 do
                  if candidate land 4095 = 0 then Cancel.check_opt cancel;
                  let first_points = if Bytes.unsafe_get first_candidate_sides
                      candidate = '\000' then left_triangle_points
                    else right_triangle_points
                  and second_points = if Bytes.unsafe_get second_candidate_sides
                      candidate = '\000' then left_triangle_points
                    else right_triangle_points in
                  Array.blit first_points
                    (first_candidates.(candidate) * 3) left_triangle 0 3;
                  Array.blit second_points
                    (second_candidates.(candidate) * 3) right_triangle 0 3;
                  let self = Bytes.unsafe_get first_candidate_sides candidate
                      = Bytes.unsafe_get second_candidate_sides candidate in
                  let count = if self
                      && ((ignore_opposite_duplicate_self_pairs
                            && opposite_duplicate left_triangle right_triangle)
                          || ordinary_shared_edge_only ~x ~y ~z
                            left_triangle right_triangle)
                    then 0 else
                    Predicates.Private.triangle_triangle_features_into
                      ~x ~y ~z
                      ~left_a:left_triangle.(0) ~left_b:left_triangle.(1)
                      ~left_c:left_triangle.(2)
                      ~right_a:right_triangle.(0) ~right_b:right_triangle.(1)
                      ~right_c:right_triangle.(2) scratch in
                  counts.(candidate) <- count;
                  if count > 0 then begin
                    feature0.(candidate) <- scratch.(0);
                    feature1.(candidate) <- scratch.(1)
                  end;
                  if count > 1 then begin
                    feature2.(candidate) <- scratch.(2);
                    feature3.(candidate) <- scratch.(3)
                  end;
                  if self
                      && ordinary_topology_contact left_triangle right_triangle count
                           feature0.(candidate) feature1.(candidate)
                           feature2.(candidate) feature3.(candidate)
                  then counts.(candidate) <- 0
                done);
              let event_offsets = Array.make (candidate_count + 1) 0
              and coplanar_count = ref 0 and degenerate_pairs = ref 0 in
              for candidate = 0 to candidate_count - 1 do
                let count = counts.(candidate) in
                event_offsets.(candidate + 1) <- event_offsets.(candidate)
                    + if count > 0 then count else 0;
                if count = -1 then incr coplanar_count
                else if count = -2 then incr degenerate_pairs
              done;
              let event_count = event_offsets.(candidate_count) in
              let event_points = Array.make event_count None
              and first_error = ref None
              and left_triangle = Array.make 3 0
              and right_triangle = Array.make 3 0 in
              (* Exact construction is deliberately joined onto one stable
                 stream. The boxed dyadic fallback is allocation-bound and
                 benchmarks slower under concurrent minor heaps; candidate
                 classification above remains parallel and zero-allocation. *)
              for candidate = 0 to candidate_count - 1 do
                if candidate land 4095 = 0 then Cancel.check_opt cancel;
                let count = counts.(candidate) in
                if count > 0 then begin
                  let first_points = if Bytes.unsafe_get first_candidate_sides
                      candidate = '\000' then left_triangle_points
                    else right_triangle_points
                  and second_points = if Bytes.unsafe_get second_candidate_sides
                      candidate = '\000' then left_triangle_points
                    else right_triangle_points in
                  Array.blit first_points
                    (first_candidates.(candidate) * 3) left_triangle 0 3;
                  Array.blit second_points
                    (second_candidates.(candidate) * 3) right_triangle 0 3;
                  for event = 0 to count - 1 do
                    let left_feature, right_feature = if event = 0 then
                        feature0.(candidate), feature1.(candidate)
                      else feature2.(candidate), feature3.(candidate) in
                    let output = event_offsets.(candidate) + event in
                    match construct_event source source_points
                        left_triangle right_triangle
                        left_feature right_feature with
                    | Ok point -> event_points.(output) <- Some point
                    | Error failure when Option.is_none !first_error ->
                        first_error := Some failure
                    | Error _ -> ()
                  done
                end
              done;
              match !first_error with
              | Some _ -> error "exact_construction_failed"
                  "an exact triangle intersection point could not be constructed"
              | None ->
                  let order = Array.init event_count Fun.id in
                  Array.sort (compare_points event_points) order;
                  let unique_of_event = Array.make event_count 0
                  and unique_count = ref 0 and previous = ref (-1) in
                  Array.iter (fun event ->
                    if !previous < 0 || not (Implicit_point.equal
                        (Option.get event_points.(event))
                        (Option.get event_points.(!previous))) then begin
                      previous := event;
                      incr unique_count
                    end;
                    unique_of_event.(event) <- !unique_count - 1) order;
                  let points = if !unique_count = 0 then [||] else begin
                    let first_point = Option.get event_points.(order.(0)) in
                    let output = Array.make !unique_count first_point
                    and unique = ref (-1) and previous = ref (-1) in
                    Array.iter (fun event ->
                      if !previous < 0 || not (Implicit_point.equal
                          (Option.get event_points.(event))
                          (Option.get event_points.(!previous))) then begin
                        incr unique; previous := event;
                        output.(!unique) <- Option.get event_points.(event)
                      end) order;
                    output
                  end in
                  let constraint_count = ref 0 in
                  for candidate = 0 to candidate_count - 1 do
                    if counts.(candidate) > 0 then incr constraint_count
                  done;
                  let kinds = Bytes.make !constraint_count '\000'
                  and first = Array.make !constraint_count 0
                  and second = Array.make !constraint_count 0
                  and constraint_first_sides = Bytes.make !constraint_count '\000'
                  and constraint_second_sides = Bytes.make !constraint_count '\000'
                  and constraint_first_triangles = Array.make !constraint_count 0
                  and constraint_second_triangles = Array.make !constraint_count 0
                  and output = ref 0 in
                  for candidate = 0 to candidate_count - 1 do
                    let count = counts.(candidate) in
                    if count > 0 then begin
                      let first_point = unique_of_event.(event_offsets.(candidate)) in
                      let second_point = if count = 1 then first_point else
                          unique_of_event.(event_offsets.(candidate) + 1) in
                      first.(!output) <- first_point;
                      second.(!output) <- second_point;
                      if first_point <> second_point then
                        Bytes.unsafe_set kinds !output '\001';
                      Bytes.unsafe_set constraint_first_sides !output
                        (Bytes.unsafe_get first_candidate_sides candidate);
                      Bytes.unsafe_set constraint_second_sides !output
                        (Bytes.unsafe_get second_candidate_sides candidate);
                      constraint_first_triangles.(!output) <- first_candidates.(candidate);
                      constraint_second_triangles.(!output) <- second_candidates.(candidate);
                      incr output
                    end
                  done;
                  let left_offsets, left_constraints = make_face_csr
                      Left (Surface_index.triangle_count left_surface)
                      constraint_first_sides constraint_first_triangles
                      constraint_second_sides constraint_second_triangles
                  and right_offsets, right_constraints = make_face_csr
                      Right (Surface_index.triangle_count right_surface)
                      constraint_first_sides constraint_first_triangles
                      constraint_second_sides constraint_second_triangles in
                  let coplanar_first_sides, coplanar_first_triangles,
                      coplanar_second_sides, coplanar_second_triangles =
                    compact_pairs counts !coplanar_count first_candidate_sides
                      first_candidates second_candidate_sides second_candidates in
                  Ok { left_geometry = left; right_geometry = right;
                       resolve_left_self_intersections;
                       resolve_right_self_intersections;
                       left_surface; right_surface; source; source_points;
                       left_triangle_points; right_triangle_points;
                       points;  first; second; constraint_first_sides;
                       constraint_first_triangles; constraint_second_sides;
                       constraint_second_triangles; left_offsets; left_constraints;
                       right_offsets; right_constraints; coplanar_first_sides;
                       coplanar_first_triangles; coplanar_second_sides;
                       coplanar_second_triangles;
                       degenerate_pairs = !degenerate_pairs }
  with
  | Cancel.Cancelled -> error "cancelled" "Boolean constraint planning was cancelled"
  | Invalid_argument message -> error "invalid_geometry" message
