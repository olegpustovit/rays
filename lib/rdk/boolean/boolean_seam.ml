let operation = "boolean_seam"
let error code message = Error (Error.make ~operation ~code message)

type kind = Left_self | Between | Right_self

type t = {
  complex : Boolean_complex.t;
  curves_materialized : bool;
  curves : Geometry.t;
  coincident : Geometry.t;
  edge_kinds : bytes;
  curve_kinds : bytes;

}

let curves value = value.curves
let coincident value = value.coincident
let kind_code = function Left_self -> 0 | Between -> 1 | Right_self -> 2
let kind_of_code = function
  | 0 -> Left_self | 1 -> Between | 2 -> Right_self
  | _ -> invalid_arg "Boolean seam kind is invalid"
let curve_kind value curve =
  kind_of_code (Char.code (Bytes.unsafe_get value.curve_kinds curve))

let side_bit = function Boolean_complex.Left -> 1 | Boolean_complex.Right -> 2

let facet_mask complex facet =
  let first, last = Boolean_complex.facet_member_range complex facet in
  let mask = ref 0 in
  for member = first to last - 1 do
    mask := !mask lor side_bit (Boolean_complex.member_side complex member)
  done;
  !mask lor if last - first > 1 then 4 else 0

let native_edge (view : Topology_index.View.view) primitive first second =
  if view.primitive_of_vertex.(first) = primitive
      && view.next_vertex.(first) = second then view.edge_of_vertex.(first)
  else if view.primitive_of_vertex.(second) = primitive
      && view.next_vertex.(second) = first then view.edge_of_vertex.(second)
  else -1

let source_native_edge constraints index side triangle first second =
  let surface, triangle_point = match side with
    | Boolean_complex.Left ->
        Boolean_constraints.View.left_surface constraints,
        Boolean_constraints.View.left_triangle_point
    | Boolean_complex.Right ->
        Boolean_constraints.View.right_surface constraints,
        Boolean_constraints.View.right_triangle_point in
  let source = Boolean_constraints.View.source constraints in
  let first_id = triangle_point constraints triangle 0
  and second_id = triangle_point constraints triangle 1
  and third_id = triangle_point constraints triangle 2 in
  let projection = Implicit_point.source_triangle_projection source
      first_id second_id third_id in
  let primitive = Surface_index.View.triangle_primitive surface triangle in
  let result = ref (-1) and local = ref 0 in
  while !result < 0 && !local < 3 do
    let next = (!local + 1) mod 3 in
    let source_first = triangle_point constraints triangle !local
    and source_second = triangle_point constraints triangle next in
    if Implicit_point.source_segment_contains source ~projection
        ~first:source_first ~second:source_second first
        && Implicit_point.source_segment_contains source ~projection
          ~first:source_first ~second:source_second second then
      result := native_edge index primitive
          (Surface_index.View.triangle_vertex surface triangle !local)
          (Surface_index.View.triangle_vertex surface triangle next);
    incr local
  done;
  !result

let is_self_edge complex constraints index side edge =
  let first_point = Boolean_complex.View.vertex complex
      (Boolean_complex.edge_first complex edge)
  and second_point = Boolean_complex.View.vertex complex
      (Boolean_complex.edge_second complex edge) in
  let first_incident, last_incident = Boolean_complex.edge_incident_range complex edge in
  let common = ref (-2) and self = ref false in
  for incident = first_incident to last_incident - 1 do
    let facet = Boolean_complex.edge_incident_facet complex incident in
    let first, last = Boolean_complex.facet_member_range complex facet in
    for member = first to last - 1 do
      if Boolean_complex.member_side complex member = side then begin
        let source_edge = source_native_edge constraints index side
            (Boolean_complex.member_face complex member) first_point second_point in
        if source_edge < 0 then self := true
        else if !common = -2 then common := source_edge
        else if !common <> source_edge then self := true
      end
    done
  done;
  !self

let coordinates complex used =
  let map = Array.make (Boolean_complex.vertex_count complex) (-1)
  and count = ref 0 in
  for point = 0 to Bytes.length used - 1 do
    if Bytes.unsafe_get used point <> '\000' then begin
      map.(point) <- !count;
      incr count
    end
  done;
  let x = Array.make !count 0. and y = Array.make !count 0.
  and z = Array.make !count 0. and complex_vertices = Array.make !count (-1) in
  for point = 0 to Bytes.length used - 1 do
    let output = map.(point) in
    if output >= 0 then begin
      let px, py, pz = Boolean_complex.approximate_vertex complex point in
      x.(output) <- px; y.(output) <- py; z.(output) <- pz;
      complex_vertices.(output) <- point
    end
  done;
  let x, y, z, _, old_to_new = Boolean_extract.View.coalesce_positions
      ~complex_vertices ~x ~y ~z in
  for point = 0 to Array.length map - 1 do
    let output = map.(point) in
    if output >= 0 then map.(point) <- old_to_new.(output)
  done;
  map, x, y, z

let geometry ~x ~y ~z topology =
  match Geometry.create
      ~positions:(Packed.Float3.View.of_owned_exn ~x ~y ~z) ~topology () with
  | Ok geometry -> geometry
  | Error message -> invalid_arg message

let empty_geometry () =
  geometry ~x:[||] ~y:[||] ~z:[||] (Topology.empty ~point_count:0)

let verify_curves ?cancel ~grain curves =
  try
  if grain <= 0 then error "invalid_parameter" "grain must be positive"
  else begin
    let topology = Geometry.topology curves in
    let invalid = ref None and primitive = ref 0 in
    while Option.is_none !invalid && !primitive < Topology.primitive_count topology do
      if !primitive land 4095 = 0 then Cancel.check_opt cancel;
      let size = Topology.primitive_size topology !primitive in
      (match Topology.primitive_kind topology !primitive with
       | Topology.Polygon -> invalid := Some (Printf.sprintf
           "primitive %d is a polygon, not a seam curve" !primitive)
       | Topology.Open_polyline when size < 2 -> invalid := Some (Printf.sprintf
           "open seam curve %d has fewer than two points" !primitive)
       | Topology.Closed_polyline when size < 3 -> invalid := Some (Printf.sprintf
           "closed seam curve %d has fewer than three points" !primitive)
       | Topology.Open_polyline | Topology.Closed_polyline -> ());
      incr primitive
    done;
    match !invalid with
    | Some message -> error "invalid_output" message
    | None ->
    match Linear_piece_index.create ?cancel ~grain curves with
    | Error failure when Error.code failure = "cancelled" ->
        error "cancelled" "Boolean seam verification was cancelled"
    | Error failure -> Error (Error.make ~operation ~code:"invalid_output"
          ~hints:[Error.to_string failure]
          "Boolean seam curves could not be indexed for verification")
    | Ok index ->
        let positions = Linear_piece_index.View.positions index in
        let invalid_pair left right =
          let left_a = Linear_piece_index.View.point index left 0
          and left_b = Linear_piece_index.View.point index left 1
          and right_a = Linear_piece_index.View.point index right 0
          and right_b = Linear_piece_index.View.point index right 1 in
          let shared = left_a = right_a || left_a = right_b
              || left_b = right_a || left_b = right_b in
          match Predicates.segment_segment_packed
              ~x:positions.x ~y:positions.y ~z:positions.z
              ~left_a ~left_b ~right_a ~right_b with
          | Predicates.Segments_disjoint -> false
          | Predicates.Segments_point -> not shared
          | Predicates.Segments_overlap
          | Predicates.Segments_degenerate -> true in
        (match Linear_piece_index.View.find_overlapping_self_pair
            ?cancel ~grain ~tolerance:0. index invalid_pair with
         | None -> Ok ()
         | Some (left, right) ->
             let left_primitive = Linear_piece_index.View.primitive index left
             and right_primitive = Linear_piece_index.View.primitive index right
             and left_local = Linear_piece_index.View.local index left
             and right_local = Linear_piece_index.View.local index right in
             Error (Error.make ~operation ~code:"seam_self_intersection"
               ~hints:["reduce the cleanup tolerance or inspect the reported curve segments"]
               (Printf.sprintf
                  "rounded seam curve %d segment %d intersects curve %d segment %d"
                  left_primitive left_local right_primitive right_local)))
  end
  with
  | Cancel.Cancelled -> error "cancelled" "Boolean seam verification was cancelled"

let validate_curve_ancestry ?cancel complex_edge_count selected curves curve_kinds
    curve_edge_offsets curve_edges =
  let topology = Geometry.topology curves
  and curve_count = Geometry.primitive_count curves in
  if Bytes.length curve_kinds <> curve_count
      || Array.length curve_edge_offsets <> curve_count + 1
      || Array.length curve_edges <> selected
      || curve_edge_offsets.(0) <> 0
      || curve_edge_offsets.(curve_count) <> selected then
    invalid_arg "Boolean seam curve ancestry has inconsistent cardinality";
  let seen = Bytes.make complex_edge_count '\000' in
  for curve = 0 to curve_count - 1 do
    if curve land 4095 = 0 then Cancel.check_opt cancel;
    let first = curve_edge_offsets.(curve)
    and last = curve_edge_offsets.(curve + 1) in
    if first > last then invalid_arg "Boolean seam curve ancestry offsets decrease";
    let expected = match Topology.primitive_kind topology curve with
      | Topology.Open_polyline -> Topology.primitive_size topology curve - 1
      | Topology.Closed_polyline -> Topology.primitive_size topology curve
      | Topology.Polygon -> invalid_arg "Boolean seam ancestry references a polygon" in
    if last - first <> expected then
      invalid_arg "Boolean seam curve incidence differs from edge ancestry";
    for slot = first to last - 1 do
      let edge = curve_edges.(slot) in
      if edge < 0 || edge >= complex_edge_count then
        invalid_arg "Boolean seam ancestry edge is out of bounds";
      if Bytes.unsafe_get seen edge <> '\000' then
        invalid_arg "Boolean seam ancestry repeats a complex edge";
      Bytes.unsafe_set seen edge '\001'
    done
  done

let materialize_coincident ?cancel complex facet_masks =
  let selected = ref 0 in
  Array.iter (fun mask -> if mask land 4 <> 0 then incr selected) facet_masks;
  let used = Bytes.make (Boolean_complex.vertex_count complex) '\000' in
  for facet = 0 to Boolean_complex.facet_count complex - 1 do
    if facet land 4095 = 0 then Cancel.check_opt cancel;
    if facet_masks.(facet) land 4 <> 0 then
      for local = 0 to 2 do
        Bytes.unsafe_set used (Boolean_complex.facet_vertex complex facet local) '\001'
      done
  done;
  let map, x, y, z = coordinates complex used in
  let vertices = Array.make (!selected * 3) 0 and output = ref 0 in
  for facet = 0 to Boolean_complex.facet_count complex - 1 do
    if facet_masks.(facet) land 4 <> 0 then begin
      for local = 0 to 2 do
        vertices.((!output * 3) + local) <-
          map.(Boolean_complex.facet_vertex complex facet local)
      done;
      incr output
    end
  done;
  (match Boolean_extract.View.validate_materialized ~x ~y ~z
      ~vertex_points:vertices () with
   | Ok () -> () | Error failure -> raise (Error.Rdk failure));
  let topology = Topology.polygons_owned ~point_count:(Array.length x)
      ~vertex_points:vertices
      ~primitive_offsets:(Array.init (!selected + 1) (fun index -> index * 3))
      |> Result.get_ok in
  geometry ~x ~y ~z topology

let materialize_curves ?cancel complex edge_kinds =
  let vertex_count = Boolean_complex.vertex_count complex in
  let used = Bytes.make vertex_count '\000' in
  for edge = 0 to Boolean_complex.edge_count complex - 1 do
    if edge land 4095 = 0 then Cancel.check_opt cancel;
    let code = Char.code (Bytes.unsafe_get edge_kinds edge) in
    if code < 3 then begin
      let first = Boolean_complex.edge_first complex edge
      and second = Boolean_complex.edge_second complex edge in
      Bytes.unsafe_set used first '\001'; Bytes.unsafe_set used second '\001';
    end
  done;
  let map, x, y, z = coordinates complex used in
  let degree = Array.make (vertex_count * 3) 0
  and adjacency_counts = Array.make vertex_count 0
  and materialized_selected = ref 0 in
  for edge = 0 to Boolean_complex.edge_count complex - 1 do
    let code = Char.code (Bytes.unsafe_get edge_kinds edge) in
    if code < 3 then begin
      let first = Boolean_complex.edge_first complex edge
      and second = Boolean_complex.edge_second complex edge in
      if map.(first) <> map.(second) then begin
        degree.((first * 3) + code) <- degree.((first * 3) + code) + 1;
        degree.((second * 3) + code) <- degree.((second * 3) + code) + 1;
        adjacency_counts.(first) <- adjacency_counts.(first) + 1;
        adjacency_counts.(second) <- adjacency_counts.(second) + 1;
        incr materialized_selected
      end
    end
  done;
  let offsets = Array.make (vertex_count + 1) 0 in
  for vertex = 0 to vertex_count - 1 do
    offsets.(vertex + 1) <- offsets.(vertex) + adjacency_counts.(vertex)
  done;
  let adjacency = Array.make (!materialized_selected * 2) 0
  and cursors = Array.copy offsets in
  for edge = 0 to Boolean_complex.edge_count complex - 1 do
    if Char.code (Bytes.unsafe_get edge_kinds edge) < 3 then begin
      let first = Boolean_complex.edge_first complex edge
      and second = Boolean_complex.edge_second complex edge in
      if map.(first) <> map.(second) then begin
        adjacency.(cursors.(first)) <- edge; cursors.(first) <- cursors.(first) + 1;
        adjacency.(cursors.(second)) <- edge; cursors.(second) <- cursors.(second) + 1
      end
    end
  done;
  (match Boolean_extract.View.validate_positions ~x ~y ~z with
   | Ok () -> () | Error failure -> raise (Error.Rdk failure));
  let vertices = Array.make (!materialized_selected * 2) 0
  and primitive_offsets = Array.make (!materialized_selected + 1) 0
  and primitive_kinds = Bytes.make !materialized_selected '\001'
  and curve_kinds = Bytes.make !materialized_selected '\000'
  and curve_edge_offsets = Array.make (!materialized_selected + 1) 0
  and curve_edges = Array.make !materialized_selected 0
  and visited = Bytes.make (Boolean_complex.edge_count complex) '\000'
  and vertex_output = ref 0 and curve_output = ref 0 and edge_output = ref 0 in
  let other edge vertex =
    let first = Boolean_complex.edge_first complex edge
    and second = Boolean_complex.edge_second complex edge in
    if first = vertex then second else if second = vertex then first
    else invalid_arg "Boolean seam adjacency is inconsistent" in
  let next_edge vertex code =
    let result = ref (-1) and slot = ref offsets.(vertex) in
    while !result < 0 && !slot < offsets.(vertex + 1) do
      let edge = adjacency.(!slot) in
      if Char.code (Bytes.unsafe_get edge_kinds edge) = code
          && Bytes.unsafe_get visited edge = '\000' then result := edge;
      incr slot
    done;
    !result in
  let emit start first_edge code =
    let curve = !curve_output in
    primitive_offsets.(curve) <- !vertex_output;
    curve_edge_offsets.(curve) <- !edge_output;
    Bytes.unsafe_set curve_kinds curve (Char.chr code);
    vertices.(!vertex_output) <- map.(start); incr vertex_output;
    let current_vertex = ref start and current_edge = ref first_edge
    and closed = ref false and running = ref true in
    while !running do
      if !edge_output >= !materialized_selected
          || !vertex_output >= !materialized_selected * 2 then
        invalid_arg "Boolean seam chaining exceeded proven cardinality";
      Bytes.unsafe_set visited !current_edge '\001';
      curve_edges.(!edge_output) <- !current_edge; incr edge_output;
      let next = other !current_edge !current_vertex in
      if next = start then begin closed := true; running := false end
      else begin
        vertices.(!vertex_output) <- map.(next); incr vertex_output;
        if degree.((next * 3) + code) <> 2 then running := false
        else begin
          let following = next_edge next code in
          if following < 0 then running := false
          else begin current_vertex := next; current_edge := following end
        end
      end
    done;
    if !closed then Bytes.unsafe_set primitive_kinds curve '\002';
    incr curve_output
  in
  for vertex = 0 to vertex_count - 1 do
    for code = 0 to 2 do
      if degree.((vertex * 3) + code) <> 0
          && degree.((vertex * 3) + code) <> 2 then begin
        let edge = ref (next_edge vertex code) in
        while !edge >= 0 do
          emit vertex !edge code;
          edge := next_edge vertex code
        done
      end
    done
  done;
  for edge = 0 to Boolean_complex.edge_count complex - 1 do
    let code = Char.code (Bytes.unsafe_get edge_kinds edge) in
    if code < 3 && map.(Boolean_complex.edge_first complex edge)
        <> map.(Boolean_complex.edge_second complex edge)
        && Bytes.unsafe_get visited edge = '\000' then
      emit (min (Boolean_complex.edge_first complex edge)
              (Boolean_complex.edge_second complex edge)) edge code
  done;
  primitive_offsets.(!curve_output) <- !vertex_output;
  curve_edge_offsets.(!curve_output) <- !edge_output;
  if !edge_output <> !materialized_selected then
    invalid_arg "Boolean seam chaining did not consume every selected edge";
  let vertices = Array.sub vertices 0 !vertex_output
  and primitive_offsets = Array.sub primitive_offsets 0 (!curve_output + 1)
  and primitive_kinds = Bytes.sub primitive_kinds 0 !curve_output
  and curve_kinds = Bytes.sub curve_kinds 0 !curve_output
  and curve_edge_offsets = Array.sub curve_edge_offsets 0 (!curve_output + 1) in
  let topology = Topology.View.create_validated_owned
      ~point_count:(Array.length x) ~vertex_points:vertices
      ~primitive_offsets ~primitive_kinds in
  geometry ~x ~y ~z topology, curve_kinds, curve_edge_offsets, curve_edges,
  !materialized_selected

let build ?cancel ?(grain = 16_384) ?(parallel_cutoff = 200_000)
    ?(materialize = true) complex =
  if grain <= 0 then error "invalid_parameter" "grain must be positive"
  else if parallel_cutoff <= 0 then
    error "invalid_parameter" "parallel_cutoff must be positive"
  else try
    Cancel.check_opt cancel;
    let facets = Boolean_complex.facet_count complex in
    (* Chaining and packed materialization dominate small jobs. At 140k edges
       scheduling only classification regresses, while the 700k-edge fixture
       benefits within an already pooled multi-domain run. *)
    let facet_masks =
      if facets < parallel_cutoff then Array.init facets (facet_mask complex)
      else Parallel.init_array ~grain facets (facet_mask complex) in
    let constraints = Boolean_complex.View.constraints complex in
    let left_index =
      if Boolean_constraints.View.resolve_left_self_intersections constraints then
        Some (Boolean_constraints.View.left_geometry constraints
          |> Geometry.topology |> Topology_index.create ?cancel
          |> Topology_index.View.view)
      else None
    and right_index =
      if Boolean_constraints.View.resolve_right_self_intersections constraints then
        Some (Boolean_constraints.View.right_geometry constraints
          |> Geometry.topology |> Topology_index.create ?cancel
          |> Topology_index.View.view)
      else None in
    let edge_count = Boolean_complex.edge_count complex in
    let edge_kinds = Bytes.make edge_count '\255' in
    let classify_range first_edge last_edge =
      let selected = ref 0 in
        for edge = first_edge to last_edge - 1 do
          if edge land 4095 = 0 then Cancel.check_opt cancel;
          let first, last = Boolean_complex.edge_incident_range complex edge
          and mask = ref 0 and has_unshared = ref false in
          for incident = first to last - 1 do
            let current = facet_masks.(
                Boolean_complex.edge_incident_facet complex incident) land 3 in
            mask := !mask lor current;
            if current <> 3 then has_unshared := true
          done;
          let kind =
            if !mask = 3 && !has_unshared then Some Between
            else if !mask = 1 && last - first > 2
                && Option.is_some left_index
                && is_self_edge complex constraints (Option.get left_index)
                     Boolean_complex.Left edge then Some Left_self
            else if !mask = 2 && last - first > 2
                && Option.is_some right_index
                && is_self_edge complex constraints (Option.get right_index)
                     Boolean_complex.Right edge then Some Right_self
            else None in
          match kind with
          | None -> ()
          | Some kind ->
              Bytes.unsafe_set edge_kinds edge (Char.chr (kind_code kind));
              incr selected
        done;
        !selected in
    if edge_count < parallel_cutoff then ignore (classify_range 0 edge_count)
    else begin
        let range_count = 1 + ((edge_count - 1) / grain) in
        let range_selected = Array.make range_count 0 in
        Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(range_count - 1)
          (fun range ->
            let first_edge = range * grain
            and last_edge = min edge_count ((range + 1) * grain) in
            range_selected.(range) <- classify_range first_edge last_edge);
        ignore (Array.fold_left ( + ) 0 range_selected)
      end;
    let curves, curve_kinds, _curve_edge_offsets, _curve_edges, coincident =
      if materialize then begin
        let curves, curve_kinds, curve_edge_offsets, curve_edges,
            materialized_selected = materialize_curves ?cancel complex edge_kinds in
        validate_curve_ancestry ?cancel edge_count materialized_selected curves
          curve_kinds curve_edge_offsets curve_edges;
        (match verify_curves ?cancel ~grain curves with
         | Ok () -> () | Error failure -> raise (Error.Rdk failure));
        curves, curve_kinds, curve_edge_offsets, curve_edges,
          materialize_coincident ?cancel complex facet_masks
      end else
        empty_geometry (), Bytes.empty, [|0|], [||], empty_geometry () in
    Ok {
      complex; curves_materialized = materialize; curves; coincident; edge_kinds;
      curve_kinds;
    }
  with
  | Cancel.Cancelled -> error "cancelled" "Boolean seam extraction was cancelled"
  | Error.Rdk failure -> Error failure
  | Invalid_argument message -> error "invalid_output" message

module View = struct
  let complex value = value.complex
  let curves_materialized value = value.curves_materialized
  let is_seam_edge value edge =
    if edge < 0 || edge >= Bytes.length value.edge_kinds then
      invalid_arg "Boolean seam edge is out of bounds";
    Char.code (Bytes.unsafe_get value.edge_kinds edge) < 3
  let verify_curves = verify_curves
end
