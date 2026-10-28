
type element = Poly_cut_points | Poly_cut_edges
type strategy = Poly_cut_remove | Poly_cut_cut

type detection =
  | Poly_cut_all
  | Poly_cut_crossing of { attribute : string; value : float }
  | Poly_cut_change of { attribute : string; threshold : float }

type numeric =
  | Scalar_float of float array
  | Scalar_int of int array
  | Tuple2 of Packed.Float2.View.view
  | Tuple3 of Packed.Float3.View.view
  | Tuple4 of Packed.Float4.View.view

let fail message = raise (Error.Fail message)

let block_count length grain =
  if length = 0 then 0 else 1 + ((length - 1) / grain)

let block_bounds length grain block =
  let first = block * grain in
  first, min length (first + grain)

let selected selection index = match selection with
  | None -> true
  | Some group -> Group.mem index group

let validate_primitive_selection primitive_count = function
  | None -> ()
  | Some group ->
      if Group.owner group <> Group.Primitive then
        fail "primitive selection must be primitive-owned";
      if Group.length group <> primitive_count then
        fail (Printf.sprintf
          "primitive selection length %d does not match primitive count %d"
          (Group.length group) primitive_count)

let validate_point_selection point_count = function
  | None -> ()
  | Some group ->
      if Group.owner group <> Group.Point then
        fail "cut-point selection must be point-owned";
      if Group.length group <> point_count then
        fail (Printf.sprintf
          "cut-point selection length %d does not match point count %d"
          (Group.length group) point_count)

let numeric_attribute name geometry =
  if String.trim name = "" then fail "cut attribute name must not be empty";
  if String.equal name "P" then
    Tuple3 (Packed.Float3.View.view (Geometry.positions geometry))
  else match Geometry.find_attribute ~owner:Attribute.Point name geometry with
    | None -> fail (Printf.sprintf "point cut attribute %S was not found" name)
    | Some attribute ->
        match Attribute.View.storage attribute with
        | Attribute.Float values -> Scalar_float values
        | Attribute.Int values -> Scalar_int values
        | Attribute.Float2 values -> Tuple2 (Packed.Float2.View.view values)
        | Attribute.Float3 values -> Tuple3 (Packed.Float3.View.view values)
        | Attribute.Float4 values -> Tuple4 (Packed.Float4.View.view values)
        | _ -> fail (Printf.sprintf
            "point cut attribute %S must have numeric scalar or tuple storage, not %s"
            name (Attribute.kind_name attribute))

let[@inline always] scalar value point = match value with
  | Scalar_float values -> Array.unsafe_get values point
  | Scalar_int values -> Float.of_int (Array.unsafe_get values point)
  | Tuple2 _ | Tuple3 _ | Tuple4 _ -> assert false

let finite_numeric value point = match value with
  | Scalar_float values -> Float.is_finite (Array.unsafe_get values point)
  | Scalar_int _ -> true
  | Tuple2 values -> Float.is_finite (Array.unsafe_get values.x point)
      && Float.is_finite (Array.unsafe_get values.y point)
  | Tuple3 values -> Float.is_finite (Array.unsafe_get values.x point)
      && Float.is_finite (Array.unsafe_get values.y point)
      && Float.is_finite (Array.unsafe_get values.z point)
  | Tuple4 values -> Float.is_finite (Array.unsafe_get values.x point)
      && Float.is_finite (Array.unsafe_get values.y point)
      && Float.is_finite (Array.unsafe_get values.z point)
      && Float.is_finite (Array.unsafe_get values.w point)

let stable_norm2 x y =
  let scale = max (abs_float x) (abs_float y) in
  if scale = 0. then 0.
  else if not (Float.is_finite scale) then Float.infinity
  else scale *. sqrt (((x /. scale) *. (x /. scale))
      +. ((y /. scale) *. (y /. scale)))

let stable_norm3 x y z =
  let scale = max (abs_float x) (max (abs_float y) (abs_float z)) in
  if scale = 0. then 0.
  else if not (Float.is_finite scale) then Float.infinity
  else scale *. sqrt (((x /. scale) *. (x /. scale))
      +. ((y /. scale) *. (y /. scale)) +. ((z /. scale) *. (z /. scale)))

let stable_norm4 x y z w =
  let scale = max (max (abs_float x) (abs_float y))
      (max (abs_float z) (abs_float w)) in
  if scale = 0. then 0.
  else if not (Float.is_finite scale) then Float.infinity
  else scale *. sqrt (((x /. scale) *. (x /. scale))
      +. ((y /. scale) *. (y /. scale)) +. ((z /. scale) *. (z /. scale))
      +. ((w /. scale) *. (w /. scale)))

let change_distance value left right = match value with
  | Scalar_float values ->
      abs_float (Array.unsafe_get values right -. Array.unsafe_get values left)
  | Scalar_int values ->
      abs_float (Float.of_int (Array.unsafe_get values right)
        -. Float.of_int (Array.unsafe_get values left))
  | Tuple2 values -> stable_norm2
      (Array.unsafe_get values.x right -. Array.unsafe_get values.x left)
      (Array.unsafe_get values.y right -. Array.unsafe_get values.y left)
  | Tuple3 values -> stable_norm3
      (Array.unsafe_get values.x right -. Array.unsafe_get values.x left)
      (Array.unsafe_get values.y right -. Array.unsafe_get values.y left)
      (Array.unsafe_get values.z right -. Array.unsafe_get values.z left)
  | Tuple4 values -> stable_norm4
      (Array.unsafe_get values.x right -. Array.unsafe_get values.x left)
      (Array.unsafe_get values.y right -. Array.unsafe_get values.y left)
      (Array.unsafe_get values.z right -. Array.unsafe_get values.z left)
      (Array.unsafe_get values.w right -. Array.unsafe_get values.w left)

let crossing_weight threshold left right =
  if left = threshold then 0.
  else if right = threshold then 1.
  else begin
    let scale = max (abs_float threshold)
        (max (abs_float left) (abs_float right)) in
    if scale = 0. then 0.5
    else begin
      let distance_left = abs_float ((threshold /. scale) -. (left /. scale))
      and distance_right = abs_float ((right /. scale) -. (threshold /. scale)) in
      distance_left /. (distance_left +. distance_right)
    end
  end

let interpolate left right weight =
  let delta = right -. left in
  if Float.is_finite delta then left +. (delta *. weight)
  else (left *. (1. -. weight)) +. (right *. weight)

let remap_edge_groups ?cancel ~grain ~source_index ~target_topology
    ~source_edge_of_vertex groups =
  match groups with
  | [] -> []
  | groups ->
      let target_index = Topology_index.create ?cancel target_topology in
      let target = Topology_index.View.view target_index in
      let source_of_target = Array.make (Topology_index.edge_count target_index) (-1) in
      let conflict = ref None in
      for vertex = 0 to Array.length source_edge_of_vertex - 1 do
        if vertex land 4095 = 0 then Cancel.check_opt cancel;
        let source_edge = Array.unsafe_get source_edge_of_vertex vertex
        and target_edge = Array.unsafe_get target.edge_of_vertex vertex in
        if source_edge >= 0 && target_edge >= 0 then begin
          let previous = Array.unsafe_get source_of_target target_edge in
          if previous >= 0 && previous <> source_edge then
            conflict := Some target_edge
          else Array.unsafe_set source_of_target target_edge source_edge
        end
      done;
      (match !conflict with
       | Some edge -> fail (Printf.sprintf
           "output edge %d has conflicting source ancestry" edge)
       | None -> ());
      let edge_count = Array.length source_of_target in
      List.map (fun group ->
        if Edge_group.topology_data_id group
            <> Topology_index.topology_data_id source_index then
          fail (Printf.sprintf "edge group %S belongs to a different topology"
            (Edge_group.name group));
        let bytes = Bytes.make ((edge_count + 7) / 8) '\000' in
        if Bytes.length bytes > 0 then Parallel.for_
            ~chunk_size:(max 1 (grain / 8)) ~start:0
            ~finish:(Bytes.length bytes - 1) (fun byte ->
          if byte land 511 = 0 then Cancel.check_opt cancel;
          let value = ref 0 and first = byte lsl 3 in
          for bit = 0 to 7 do
            let edge = first + bit in
            if edge < edge_count then begin
              let source_edge = Array.unsafe_get source_of_target edge in
              if source_edge >= 0 && Edge_group.mem source_edge group then
                value := !value lor (1 lsl bit)
            end
          done;
          Bytes.unsafe_set bytes byte (Char.unsafe_chr !value));
        Edge_group.View.of_owned_bits ~topology:target_topology ~edge_count
          ~name:(Edge_group.name group) bytes) groups

let edge_remove_fast ?cancel ~grain ~keep_closed ~topology_value ~topology
    ~source_index ~index ~edge_parts ~primitive_action geometry =
  let topology : Topology.View.view = topology
  and index : Topology_index.View.view = index in
  let primitive_count = Geometry.primitive_count geometry in
  let fragment_counts = Array.make primitive_count 0
  and corner_counts = Array.make primitive_count 0 in
  if primitive_count > 0 then Parallel.for_
      ~chunk_size:(max 1 (grain / 256)) ~start:0 ~finish:(primitive_count - 1)
      (fun primitive ->
        if primitive land 1023 = 0 then Cancel.check_opt cancel;
        let first = Array.unsafe_get topology.primitive_offsets primitive
        and last = Array.unsafe_get topology.primitive_offsets (primitive + 1) in
        let size = last - first in
        if Bytes.unsafe_get primitive_action primitive = '\000' then begin
          Array.unsafe_set fragment_counts primitive 1;
          Array.unsafe_set corner_counts primitive size
        end else begin
          let fragments = ref 0 and retained_edges = ref 0 in
          if Topology.primitive_kind topology_value primitive
              = Topology.Open_polyline then begin
            let in_run = ref false in
            for vertex = first to last - 1 do
              if Array.unsafe_get index.next_vertex vertex >= 0
                  && Array.unsafe_get edge_parts vertex > 0 then begin
                incr retained_edges;
                if not !in_run then begin incr fragments; in_run := true end
              end else in_run := false
            done
          end else begin
            for vertex = first to last - 1 do
              if Array.unsafe_get edge_parts vertex > 0 then begin
                incr retained_edges;
                let previous = Array.unsafe_get index.previous_vertex vertex in
                if previous < 0 || Array.unsafe_get edge_parts previous = 0 then
                  incr fragments
              end
            done
          end;
          Array.unsafe_set fragment_counts primitive !fragments;
          Array.unsafe_set corner_counts primitive (!retained_edges + !fragments)
        end);
  let primitive_bases = Array.make (primitive_count + 1) 0 in
  for primitive = 0 to primitive_count - 1 do
    let count = Array.unsafe_get fragment_counts primitive in
    if Array.unsafe_get primitive_bases primitive > Sys.max_array_length - count then
      fail "output primitive cardinality exceeds array limits";
    Array.unsafe_set primitive_bases (primitive + 1)
      (Array.unsafe_get primitive_bases primitive + count)
  done;
  let output_primitive_count = Array.unsafe_get primitive_bases primitive_count in
  let fragment_start = Array.make output_primitive_count (-1)
  and fragment_edges = Array.make output_primitive_count (-1)
  and primitive_map = Array.make output_primitive_count 0
  and fragment_sizes = Array.make output_primitive_count 0 in
  if primitive_count > 0 then Parallel.for_
      ~chunk_size:(max 1 (grain / 256)) ~start:0 ~finish:(primitive_count - 1)
      (fun primitive ->
        if primitive land 1023 = 0 then Cancel.check_opt cancel;
        let target_base = Array.unsafe_get primitive_bases primitive
        and first = Array.unsafe_get topology.primitive_offsets primitive
        and last = Array.unsafe_get topology.primitive_offsets (primitive + 1) in
        if Bytes.unsafe_get primitive_action primitive = '\000' then begin
          Array.unsafe_set primitive_map target_base primitive;
          Array.unsafe_set fragment_sizes target_base (last - first)
        end else begin
          let target = ref target_base and active = ref false
          and edge_count = ref 0 in
          let begin_run vertex =
            active := true;
            edge_count := 0;
            Array.unsafe_set fragment_start !target vertex;
            Array.unsafe_set primitive_map !target primitive in
          let finish_run () =
            if !active then begin
              Array.unsafe_set fragment_edges !target !edge_count;
              Array.unsafe_set fragment_sizes !target (!edge_count + 1);
              incr target;
              active := false
            end in
          if Topology.primitive_kind topology_value primitive
              = Topology.Open_polyline then begin
            for vertex = first to last - 1 do
              if Array.unsafe_get index.next_vertex vertex >= 0
                  && Array.unsafe_get edge_parts vertex > 0 then begin
                if not !active then begin_run vertex;
                incr edge_count
              end else finish_run ()
            done;
            finish_run ()
          end else begin
            let start = ref (-1) in
            for vertex = first to last - 1 do
              let previous = Array.unsafe_get index.previous_vertex vertex in
              if !start < 0 && Array.unsafe_get edge_parts vertex > 0
                  && (previous < 0 || Array.unsafe_get edge_parts previous = 0)
              then start := vertex
            done;
            if !start >= 0 then begin
              let size = last - first in
              for offset = 0 to size - 1 do
                let vertex = first + ((!start - first + offset) mod size) in
                if Array.unsafe_get edge_parts vertex > 0 then begin
                  if not !active then begin_run vertex;
                  incr edge_count
                end else finish_run ()
              done;
              finish_run ()
            end
          end
        end);
  let primitive_offsets = Array.make (output_primitive_count + 1) 0 in
  for primitive = 0 to output_primitive_count - 1 do
    let size = Array.unsafe_get fragment_sizes primitive in
    if Array.unsafe_get primitive_offsets primitive > Sys.max_array_length - size then
      fail "output corner cardinality exceeds array limits";
    Array.unsafe_set primitive_offsets (primitive + 1)
      (Array.unsafe_get primitive_offsets primitive + size)
  done;
  let output_vertex_count = Array.unsafe_get primitive_offsets output_primitive_count in
  let vertex_points = Array.make output_vertex_count 0
  and vertex_map = Array.make output_vertex_count 0
  and primitive_kinds = Array.make output_primitive_count Topology.Open_polyline
  and source_edge_of_vertex = Array.make output_vertex_count (-1) in
  if output_primitive_count > 0 then Parallel.for_
      ~chunk_size:(max 1 (grain / 8)) ~start:0
      ~finish:(output_primitive_count - 1) (fun target ->
    if target land 1023 = 0 then Cancel.check_opt cancel;
    let primitive = Array.unsafe_get primitive_map target
    and output = Array.unsafe_get primitive_offsets target
    and source_first = Array.unsafe_get topology.primitive_offsets
        (Array.unsafe_get primitive_map target)
    and source_last = Array.unsafe_get topology.primitive_offsets
        (Array.unsafe_get primitive_map target + 1) in
    if Array.unsafe_get fragment_start target < 0 then begin
      Array.unsafe_set primitive_kinds target
        (Topology.primitive_kind topology_value primitive);
      for vertex = source_first to source_last - 1 do
        let target_vertex = output + vertex - source_first in
        Array.unsafe_set vertex_points target_vertex
          (Array.unsafe_get topology.vertex_points vertex);
        Array.unsafe_set vertex_map target_vertex vertex;
        Array.unsafe_set source_edge_of_vertex target_vertex
          (Array.unsafe_get index.edge_of_vertex vertex)
      done
    end else begin
      let start = Array.unsafe_get fragment_start target
      and edges = Array.unsafe_get fragment_edges target
      and source_size = source_last - source_first in
      for local = 0 to edges do
        let source_vertex = if Topology.primitive_kind topology_value primitive
              = Topology.Open_polyline then start + local
          else source_first + ((start - source_first + local) mod source_size) in
        Array.unsafe_set vertex_points (output + local)
          (Array.unsafe_get topology.vertex_points source_vertex);
        Array.unsafe_set vertex_map (output + local) source_vertex;
        if local < edges then Array.unsafe_set source_edge_of_vertex (output + local)
          (Array.unsafe_get index.edge_of_vertex source_vertex)
      done;
      let size = edges + 1 and source_kind =
        Topology.primitive_kind topology_value primitive in
      Array.unsafe_set primitive_kinds target
        (if source_kind <> Topology.Open_polyline && keep_closed && size >= 3
         then source_kind else Topology.Open_polyline)
    end);
  let output_topology = Topology.create_owned
      ~point_count:(Geometry.point_count geometry) ~vertex_points
      ~primitive_offsets ~primitive_kinds |> Result.get_ok in
  let attributes = List.map (fun attribute -> match Attribute.owner attribute with
    | Attribute.Point | Attribute.Detail -> attribute
    | Attribute.Vertex -> Topology_remap.attribute ?cancel ~grain vertex_map attribute
    | Attribute.Primitive ->
        Topology_remap.attribute ?cancel ~grain primitive_map attribute)
      (Geometry.attributes geometry)
  and groups = List.map (fun group -> match Group.owner group with
    | Group.Point -> group
    | Group.Vertex -> Topology_remap.group ?cancel ~grain vertex_map group
    | Group.Primitive -> Topology_remap.group ?cancel ~grain primitive_map group)
      (Geometry.groups geometry) in
  let edge_groups = remap_edge_groups ?cancel ~grain ~source_index
      ~target_topology:output_topology ~source_edge_of_vertex
      (Geometry.edge_groups geometry) in
  Geometry.create ~positions:(Geometry.positions geometry) ~topology:output_topology
    ~attributes ~groups ~edge_groups ()

let cut ?cancel ?(grain = 16_384) ?primitives ?cut_points ?cut_edges
    ?(element = Poly_cut_points) ?(strategy = Poly_cut_remove)
    ?(detection = Poly_cut_all) ?(keep_closed = true) geometry =
  Error.guard ~operation:"poly_cut" ~code:"invalid_poly_cut" @@ fun () ->
  try
    if grain <= 0 then fail "grain must be positive";
    Cancel.check_opt cancel;
    let topology_value = Geometry.topology geometry in
    let topology = Topology.View.view topology_value in
    let source_index = Topology_index.create ?cancel topology_value in
    let index = Topology_index.View.view source_index in
    let point_count = Geometry.point_count geometry
    and vertex_count = Geometry.vertex_count geometry
    and primitive_count = Geometry.primitive_count geometry in
    validate_primitive_selection primitive_count primitives;
    (match element with
     | Poly_cut_points ->
         validate_point_selection point_count cut_points;
         Option.iter (fun _ ->
           fail "an edge selection is only valid when cutting edges") cut_edges
     | Poly_cut_edges ->
         Option.iter (fun _ ->
           fail "a point selection is only valid when cutting points") cut_points;
         Option.iter (fun group ->
           if Edge_group.topology_data_id group <> Topology.data_id topology_value
               || Edge_group.length group <> Topology_index.edge_count source_index
           then fail "cut-edge selection belongs to a different topology") cut_edges);
    let compiled_detection = match detection with
      | Poly_cut_all -> None
      | Poly_cut_crossing { attribute; value } ->
          if not (Float.is_finite value) then fail "crossing value must be finite";
          let numeric = numeric_attribute attribute geometry in
          (match numeric with
           | Scalar_float _ | Scalar_int _ -> ()
           | Tuple2 _ | Tuple3 _ | Tuple4 _ ->
               fail "attribute crossing requires scalar float or integer storage");
          Some (`Crossing (numeric, value))
      | Poly_cut_change { attribute; threshold } ->
          if not (Float.is_finite threshold) || threshold < 0. then
            fail "change threshold must be finite and non-negative";
          if strategy = Poly_cut_cut && threshold = 0. then
            fail "cut-at-change requires a positive threshold";
          Some (`Change (numeric_attribute attribute geometry, threshold)) in
    let edge_parts = Array.make vertex_count 0
    and edge_cut_weight = Array.make vertex_count (-1.)
    and endpoint_bits = Bytes.make vertex_count '\000'
    and invalid_edges = Bytes.make vertex_count '\000' in
    let blocks = block_count vertex_count grain in
    let error_vertices = Array.make blocks (-1)
    and error_kinds = Bytes.make blocks '\000' in
    if blocks > 0 then Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(blocks - 1)
      (fun block ->
        Cancel.check_opt cancel;
        let first, last = block_bounds vertex_count grain block in
        let first_error = ref (-1) and first_kind = ref 0 in
        for vertex = first to last - 1 do
          if vertex land 4095 = 0 then Cancel.check_opt cancel;
          let next = Array.unsafe_get index.next_vertex vertex in
          if next >= 0 then begin
            let primitive = Array.unsafe_get index.primitive_of_vertex vertex
            and edge = Array.unsafe_get index.edge_of_vertex vertex in
            if not (selected primitives primitive)
                || (match cut_edges with
                    | Some group when element = Poly_cut_edges ->
                        not (Edge_group.mem edge group)
                    | _ -> false)
            then Array.unsafe_set edge_parts vertex 1
            else begin
              let left = Array.unsafe_get topology.vertex_points vertex
              and right = Array.unsafe_get topology.vertex_points next in
              let eligible = match element, cut_points with
                | Poly_cut_points, Some group ->
                    Group.mem left group || Group.mem right group
                | _ -> true in
              if not eligible then Array.unsafe_set edge_parts vertex 1
              else begin
              let invalid, cut_weight, parts, endpoint =
                match compiled_detection with
                | None -> true, -1., 0, 0
                | Some (`Crossing (values, threshold)) ->
                    if not (finite_numeric values left
                        && finite_numeric values right) then begin
                      if !first_error < 0 then begin
                        first_error := vertex; first_kind := 1
                      end;
                      false, -1., 1, 0
                    end else begin
                      let a = scalar values left and b = scalar values right in
                      let crosses = (a <= threshold && b >= threshold)
                          || (a >= threshold && b <= threshold) in
                      if not crosses then false, -1., 1, 0
                      else if a = threshold && b = threshold then
                        true, -1., 0, 0
                      else
                        let weight = crossing_weight threshold a b in
                        if weight <= 0. then true, -1., 1, 1
                        else if weight >= 1. then true, -1., 1, 2
                        else true, weight, 2, 0
                    end
                | Some (`Change (values, threshold)) ->
                    if not (finite_numeric values left
                        && finite_numeric values right) then begin
                      if !first_error < 0 then begin
                        first_error := vertex; first_kind := 1
                      end;
                      false, -1., 1, 0
                    end else begin
                      let distance = change_distance values left right in
                      if not (Float.is_finite distance) then begin
                        if !first_error < 0 then begin
                          first_error := vertex; first_kind := 2
                        end;
                        false, -1., 1, 0
                      end else if distance <= threshold then false, -1., 1, 0
                      else if strategy = Poly_cut_remove then true, -1., 0, 0
                      else begin
                        let ratio = distance /. threshold in
                        if not (Float.is_finite ratio)
                            || ratio > Float.of_int Sys.max_array_length then begin
                          if !first_error < 0 then begin
                            first_error := vertex; first_kind := 3
                          end;
                          false, -1., 1, 0
                        end else true, -1., int_of_float (ceil ratio), 0
                      end
                    end in
              if invalid then Bytes.unsafe_set invalid_edges vertex '\001';
              if element = Poly_cut_edges then begin
                let output_parts = match strategy with
                  | Poly_cut_remove -> if invalid then 0 else 1
                  | Poly_cut_cut -> if invalid then parts else 1 in
                Array.unsafe_set edge_parts vertex output_parts;
                Array.unsafe_set edge_cut_weight vertex cut_weight;
                Bytes.unsafe_set endpoint_bits vertex (Char.unsafe_chr endpoint)
              end
              end
            end
          end
        done;
        if !first_error >= 0 then begin
          Array.unsafe_set error_vertices block !first_error;
          Bytes.unsafe_set error_kinds block (Char.unsafe_chr !first_kind)
        end);
    (match Array.find_index (fun vertex -> vertex >= 0) error_vertices with
     | None -> ()
     | Some block ->
         let vertex = Array.unsafe_get error_vertices block in
         let point = Array.unsafe_get topology.vertex_points vertex in
         let message = match Char.code (Bytes.unsafe_get error_kinds block) with
           | 1 -> Printf.sprintf
               "cut attribute is non-finite at operated point %d" point
           | 2 -> Printf.sprintf
               "attribute change is not representable on edge from vertex %d" vertex
           | _ -> Printf.sprintf
               "cut subdivision cardinality exceeds array limits at vertex %d" vertex in
         fail message);
    let vertex_break = Bytes.make vertex_count '\000' in
    if vertex_count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
        ~finish:(vertex_count - 1) (fun vertex ->
      if vertex land 4095 = 0 then Cancel.check_opt cancel;
      let next = Array.unsafe_get index.next_vertex vertex in
      if next >= 0 || Array.unsafe_get index.previous_vertex vertex >= 0 then begin
        match element with
        | Poly_cut_points ->
            let point = Array.unsafe_get topology.vertex_points vertex in
            if selected cut_points point then begin
              let outgoing = next >= 0
                  && Bytes.unsafe_get invalid_edges vertex <> '\000' in
              let previous = Array.unsafe_get index.previous_vertex vertex in
              let incoming = previous >= 0
                  && Bytes.unsafe_get invalid_edges previous <> '\000' in
              if outgoing || incoming then Bytes.unsafe_set vertex_break vertex '\001'
            end
        | Poly_cut_edges ->
            let outgoing = next >= 0
                && Char.code (Bytes.unsafe_get endpoint_bits vertex) land 1 <> 0 in
            let previous = Array.unsafe_get index.previous_vertex vertex in
            let incoming = previous >= 0
                && Char.code (Bytes.unsafe_get endpoint_bits previous) land 2 <> 0 in
            if outgoing || incoming then Bytes.unsafe_set vertex_break vertex '\001'
      end);
    if element = Poly_cut_points then begin
      if vertex_count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
          ~finish:(vertex_count - 1) (fun vertex ->
        if vertex land 4095 = 0 then Cancel.check_opt cancel;
        let next = Array.unsafe_get index.next_vertex vertex in
        if next >= 0 then Array.unsafe_set edge_parts vertex
          (match strategy with
           | Poly_cut_cut -> 1
           | Poly_cut_remove ->
               if Bytes.unsafe_get vertex_break vertex <> '\000'
                   || Bytes.unsafe_get vertex_break next <> '\000'
               then 0 else 1))
    end;
    let primitive_action = Bytes.make primitive_count '\000' in
    if primitive_count > 0 then Parallel.for_ ~chunk_size:(max 1 (grain / 256))
        ~start:0 ~finish:(primitive_count - 1) (fun primitive ->
      if primitive land 1023 = 0 then Cancel.check_opt cancel;
      if selected primitives primitive then begin
        let first = Array.unsafe_get topology.primitive_offsets primitive
        and last = Array.unsafe_get topology.primitive_offsets (primitive + 1) in
        let changed = ref false in
        for vertex = first to last - 1 do
          if Array.unsafe_get index.next_vertex vertex >= 0
              && Array.unsafe_get edge_parts vertex <> 1 then changed := true
        done;
        if not !changed then
          for vertex = first to last - 1 do
            let previous = Array.unsafe_get index.previous_vertex vertex
            and next = Array.unsafe_get index.next_vertex vertex in
            if previous >= 0 && next >= 0
                && Array.unsafe_get edge_parts previous > 0
                && Array.unsafe_get edge_parts vertex > 0
                && Bytes.unsafe_get vertex_break vertex <> '\000'
            then changed := true
          done;
        if !changed then Bytes.unsafe_set primitive_action primitive '\001'
      end);
    if not (Bytes.exists (fun byte -> byte <> '\000') primitive_action) then
      Ok geometry
    else if element = Poly_cut_edges && strategy = Poly_cut_remove then
      edge_remove_fast ?cancel ~grain ~keep_closed ~topology_value ~topology
        ~source_index ~index ~edge_parts ~primitive_action geometry
    else begin
      let primitive_segment_offsets = Array.make (primitive_count + 1) 0 in
      for primitive = 0 to primitive_count - 1 do
        let first = Array.unsafe_get topology.primitive_offsets primitive
        and last = Array.unsafe_get topology.primitive_offsets (primitive + 1) in
        let count = ref 0 in
        for vertex = first to last - 1 do
          let parts = Array.unsafe_get edge_parts vertex in
          if !count > Sys.max_array_length - parts then
            fail "expanded cut-segment cardinality exceeds array limits";
          count := !count + parts
        done;
        if Array.unsafe_get primitive_segment_offsets primitive
            > Sys.max_array_length - !count then
          fail "expanded cut-segment cardinality exceeds array limits";
        Array.unsafe_set primitive_segment_offsets (primitive + 1)
          (Array.unsafe_get primitive_segment_offsets primitive + !count)
      done;
      let segment_count = Array.unsafe_get primitive_segment_offsets primitive_count in
      let segment_vertex = Array.make segment_count 0
      and segment_part = Array.make segment_count 0
      and segment_boundary = Bytes.make segment_count '\000' in
      if primitive_count > 0 then Parallel.for_
          ~chunk_size:(max 1 (grain / 256)) ~start:0
          ~finish:(primitive_count - 1) (fun primitive ->
        if primitive land 1023 = 0 then Cancel.check_opt cancel;
        let first = Array.unsafe_get topology.primitive_offsets primitive
        and last = Array.unsafe_get topology.primitive_offsets (primitive + 1)
        and output = ref (Array.unsafe_get primitive_segment_offsets primitive) in
        let closed = Topology.primitive_kind topology_value primitive
            <> Topology.Open_polyline in
        for vertex = first to last - 1 do
          let parts = Array.unsafe_get edge_parts vertex in
          for part = 0 to parts - 1 do
            Array.unsafe_set segment_vertex !output vertex;
            Array.unsafe_set segment_part !output part;
            let boundary =
              if part > 0 then 2
              else if not closed && vertex = first then 1
              else begin
                let previous = Array.unsafe_get index.previous_vertex vertex in
                if previous < 0 || Array.unsafe_get edge_parts previous = 0 then 1
                else if Bytes.unsafe_get vertex_break vertex <> '\000' then 2
                else 0
              end in
            Bytes.unsafe_set segment_boundary !output (Char.unsafe_chr boundary);
            incr output
          done
        done);
      let fragment_counts = Array.make primitive_count 0
      and corner_counts = Array.make primitive_count 0 in
      if primitive_count > 0 then Parallel.for_
          ~chunk_size:(max 1 (grain / 256)) ~start:0
          ~finish:(primitive_count - 1) (fun primitive ->
        if primitive land 1023 = 0 then Cancel.check_opt cancel;
        let source_size = Array.unsafe_get topology.primitive_offsets (primitive + 1)
            - Array.unsafe_get topology.primitive_offsets primitive in
        if Bytes.unsafe_get primitive_action primitive = '\000' then begin
          Array.unsafe_set fragment_counts primitive 1;
          Array.unsafe_set corner_counts primitive source_size
        end else begin
          let first = Array.unsafe_get primitive_segment_offsets primitive
          and last = Array.unsafe_get primitive_segment_offsets (primitive + 1) in
          let segments = last - first in
          if segments > 0 then begin
            let boundaries = ref 0 in
            for segment = first to last - 1 do
              if Bytes.unsafe_get segment_boundary segment <> '\000' then
                incr boundaries
            done;
            let closed = Topology.primitive_kind topology_value primitive
                <> Topology.Open_polyline in
            let fragments = if closed && !boundaries = 0 then 1 else !boundaries in
            Array.unsafe_set fragment_counts primitive fragments;
            Array.unsafe_set corner_counts primitive
              (if closed && !boundaries = 0 then source_size
               else segments + fragments)
          end
        end);
      let primitive_bases = Array.make (primitive_count + 1) 0 in
      for primitive = 0 to primitive_count - 1 do
        let count = Array.unsafe_get fragment_counts primitive in
        if Array.unsafe_get primitive_bases primitive
            > Sys.max_array_length - count then
          fail "output primitive cardinality exceeds array limits";
        Array.unsafe_set primitive_bases (primitive + 1)
          (Array.unsafe_get primitive_bases primitive + count)
      done;
      let output_primitive_count = Array.unsafe_get primitive_bases primitive_count in
      let fragment_first = Array.make output_primitive_count (-1)
      and fragment_segments = Array.make output_primitive_count (-1)
      and primitive_map = Array.make output_primitive_count 0
      and fragment_sizes = Array.make output_primitive_count 0 in
      if primitive_count > 0 then Parallel.for_
          ~chunk_size:(max 1 (grain / 256)) ~start:0
          ~finish:(primitive_count - 1) (fun primitive ->
        if primitive land 1023 = 0 then Cancel.check_opt cancel;
        let target_base = Array.unsafe_get primitive_bases primitive
        and source_size = Array.unsafe_get topology.primitive_offsets (primitive + 1)
            - Array.unsafe_get topology.primitive_offsets primitive in
        if Bytes.unsafe_get primitive_action primitive = '\000' then begin
          Array.unsafe_set primitive_map target_base primitive;
          Array.unsafe_set fragment_sizes target_base source_size
        end else begin
          let segment_base = Array.unsafe_get primitive_segment_offsets primitive
          and segment_last = Array.unsafe_get primitive_segment_offsets (primitive + 1) in
          let segments = segment_last - segment_base in
          if segments > 0 then begin
            let closed = Topology.primitive_kind topology_value primitive
                <> Topology.Open_polyline in
            let first_boundary = ref (-1) in
            for segment = segment_base to segment_last - 1 do
              if !first_boundary < 0
                  && Bytes.unsafe_get segment_boundary segment <> '\000'
              then first_boundary := segment
            done;
            if closed && !first_boundary < 0 then begin
              Array.unsafe_set primitive_map target_base primitive;
              Array.unsafe_set fragment_sizes target_base source_size
            end else begin
              let start = if !first_boundary >= 0 then !first_boundary else segment_base in
              let target = ref (target_base - 1)
              and current_count = ref 0 in
              for offset = 0 to segments - 1 do
                let segment = segment_base
                    + ((start - segment_base + offset) mod segments) in
                if Bytes.unsafe_get segment_boundary segment <> '\000' then begin
                  if !target >= target_base then begin
                    Array.unsafe_set fragment_segments !target !current_count;
                    Array.unsafe_set fragment_sizes !target (!current_count + 1)
                  end;
                  incr target;
                  current_count := 0;
                  Array.unsafe_set fragment_first !target segment;
                  Array.unsafe_set primitive_map !target primitive
                end;
                incr current_count
              done;
              if !target >= target_base then begin
                Array.unsafe_set fragment_segments !target !current_count;
                Array.unsafe_set fragment_sizes !target (!current_count + 1)
              end
            end
          end
        end);
      let primitive_offsets = Array.make (output_primitive_count + 1) 0 in
      for primitive = 0 to output_primitive_count - 1 do
        let size = Array.unsafe_get fragment_sizes primitive in
        if Array.unsafe_get primitive_offsets primitive
            > Sys.max_array_length - size then
          fail "output corner cardinality exceeds array limits";
        Array.unsafe_set primitive_offsets (primitive + 1)
          (Array.unsafe_get primitive_offsets primitive + size)
      done;
      let output_vertex_count = Array.unsafe_get primitive_offsets output_primitive_count in
      let primitive_kinds = Array.make output_primitive_count Topology.Open_polyline
      and point_left = Array.make output_vertex_count 0
      and point_right = Array.make output_vertex_count 0
      and vertex_left = Array.make output_vertex_count 0
      and vertex_right = Array.make output_vertex_count 0
      and weights = Array.make output_vertex_count 0.
      and generated = Bytes.make output_vertex_count '\000'
      and source_edge_of_vertex = Array.make output_vertex_count (-1) in
      let write_sample output source_vertex part at_end clone =
        let next = Array.unsafe_get index.next_vertex source_vertex in
        let parts = Array.unsafe_get edge_parts source_vertex in
        let crossing = Array.unsafe_get edge_cut_weight source_vertex in
        let weight = if crossing >= 0. && parts = 2 then
            if at_end then (if part = 0 then crossing else 1.)
            else if part = 0 then 0. else crossing
          else if at_end then Float.of_int (part + 1) /. Float.of_int parts
          else Float.of_int part /. Float.of_int parts in
        let left_point = Array.unsafe_get topology.vertex_points source_vertex
        and right_point = Array.unsafe_get topology.vertex_points next in
        if weight <= 0. then begin
          Array.unsafe_set point_left output left_point;
          Array.unsafe_set point_right output left_point;
          Array.unsafe_set vertex_left output source_vertex;
          Array.unsafe_set vertex_right output source_vertex;
          Array.unsafe_set weights output 0.
        end else if weight >= 1. then begin
          Array.unsafe_set point_left output right_point;
          Array.unsafe_set point_right output right_point;
          Array.unsafe_set vertex_left output next;
          Array.unsafe_set vertex_right output next;
          Array.unsafe_set weights output 0.
        end else begin
          Array.unsafe_set point_left output left_point;
          Array.unsafe_set point_right output right_point;
          Array.unsafe_set vertex_left output source_vertex;
          Array.unsafe_set vertex_right output next;
          Array.unsafe_set weights output weight
        end;
        if clone || (weight > 0. && weight < 1.) then
          Bytes.unsafe_set generated output '\001' in
      if output_primitive_count > 0 then Parallel.for_
          ~chunk_size:(max 1 (grain / 8)) ~start:0
          ~finish:(output_primitive_count - 1) (fun target ->
        if target land 1023 = 0 then Cancel.check_opt cancel;
        let primitive = Array.unsafe_get primitive_map target
        and output_first = Array.unsafe_get primitive_offsets target in
        if Array.unsafe_get fragment_first target < 0 then begin
          let source_first = Array.unsafe_get topology.primitive_offsets primitive
          and source_last = Array.unsafe_get topology.primitive_offsets (primitive + 1) in
          Array.unsafe_set primitive_kinds target
            (Topology.primitive_kind topology_value primitive);
          for source_vertex = source_first to source_last - 1 do
            let output = output_first + source_vertex - source_first in
            let point = Array.unsafe_get topology.vertex_points source_vertex in
            Array.unsafe_set point_left output point;
            Array.unsafe_set point_right output point;
            Array.unsafe_set vertex_left output source_vertex;
            Array.unsafe_set vertex_right output source_vertex;
            Array.unsafe_set source_edge_of_vertex output
              (Array.unsafe_get index.edge_of_vertex source_vertex)
          done
        end else begin
          let first_segment = Array.unsafe_get fragment_first target
          and count = Array.unsafe_get fragment_segments target
          and segment_base = Array.unsafe_get primitive_segment_offsets primitive
          and total_segments = Array.unsafe_get primitive_segment_offsets (primitive + 1)
              - Array.unsafe_get primitive_segment_offsets primitive in
          let first_source_vertex = Array.unsafe_get segment_vertex first_segment
          and first_part = Array.unsafe_get segment_part first_segment in
          let clone = Char.code (Bytes.unsafe_get segment_boundary first_segment) = 2 in
          write_sample output_first first_source_vertex first_part false clone;
          for local = 0 to count - 1 do
            let segment = segment_base
                + ((first_segment - segment_base + local) mod total_segments) in
            let source_vertex = Array.unsafe_get segment_vertex segment
            and part = Array.unsafe_get segment_part segment
            and output = output_first + local in
            Array.unsafe_set source_edge_of_vertex output
              (Array.unsafe_get index.edge_of_vertex source_vertex);
            write_sample (output + 1) source_vertex part true false
          done;
          let size = count + 1 in
          let source_kind = Topology.primitive_kind topology_value primitive in
          Array.unsafe_set primitive_kinds target
            (if source_kind <> Topology.Open_polyline && keep_closed && size >= 3
             then source_kind else Topology.Open_polyline)
        end);
      let source_point_used = Bytes.make point_count '\000'
      and vertex_points = Array.make output_vertex_count 0 in
      if element <> Poly_cut_points || strategy <> Poly_cut_remove then
        Bytes.fill source_point_used 0 point_count '\001'
      else begin
        for point = 0 to point_count - 1 do
          if Array.unsafe_get index.point_offsets point
              = Array.unsafe_get index.point_offsets (point + 1) then
            Bytes.unsafe_set source_point_used point '\001'
        done
      end;
      let generated_count = ref 0 in
      for vertex = 0 to output_vertex_count - 1 do
        if Bytes.unsafe_get generated vertex <> '\000' then begin
          Array.unsafe_set vertex_points vertex !generated_count;
          incr generated_count
        end else begin
          let point = Array.unsafe_get point_left vertex in
          if point <> Array.unsafe_get point_right vertex
              || Array.unsafe_get weights vertex <> 0. then
            fail "internal non-generated cut point is not an exact source point";
          Bytes.unsafe_set source_point_used point '\001'
        end
      done;
      let source_to_output = Array.make point_count (-1)
      and base_point_count = ref 0 in
      for point = 0 to point_count - 1 do
        if Bytes.unsafe_get source_point_used point <> '\000' then begin
          Array.unsafe_set source_to_output point !base_point_count;
          incr base_point_count
        end
      done;
      if !base_point_count > Sys.max_array_length - !generated_count then
        fail "output point cardinality exceeds array limits";
      let output_point_count = !base_point_count + !generated_count in
      if output_vertex_count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
          ~finish:(output_vertex_count - 1) (fun vertex ->
        if vertex land 4095 = 0 then Cancel.check_opt cancel;
        if Bytes.unsafe_get generated vertex <> '\000' then
          Array.unsafe_set vertex_points vertex
            (!base_point_count + Array.unsafe_get vertex_points vertex)
        else Array.unsafe_set vertex_points vertex
          (Array.unsafe_get source_to_output (Array.unsafe_get point_left vertex)));
      let output_point_left = Array.make output_point_count 0
      and output_point_right = Array.make output_point_count 0
      and output_point_weight = Array.make output_point_count 0. in
      for point = 0 to point_count - 1 do
        let output = Array.unsafe_get source_to_output point in
        if output >= 0 then begin
          Array.unsafe_set output_point_left output point;
          Array.unsafe_set output_point_right output point
        end
      done;
      if output_vertex_count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
          ~finish:(output_vertex_count - 1) (fun vertex ->
        if vertex land 4095 = 0 then Cancel.check_opt cancel;
        if Bytes.unsafe_get generated vertex <> '\000' then begin
          let output = Array.unsafe_get vertex_points vertex in
          Array.unsafe_set output_point_left output (Array.unsafe_get point_left vertex);
          Array.unsafe_set output_point_right output (Array.unsafe_get point_right vertex);
          Array.unsafe_set output_point_weight output (Array.unsafe_get weights vertex)
        end);
      let source_positions = Packed.Float3.View.view (Geometry.positions geometry) in
      let x = Array.make output_point_count 0.
      and y = Array.make output_point_count 0.
      and z = Array.make output_point_count 0. in
      let position_errors = Array.make (block_count output_point_count grain) (-1) in
      if output_point_count > 0 then Parallel.for_ ~chunk_size:1 ~start:0
          ~finish:(Array.length position_errors - 1) (fun block ->
        Cancel.check_opt cancel;
        let first, last = block_bounds output_point_count grain block
        and first_error = ref (-1) in
        for point = first to last - 1 do
          let left = Array.unsafe_get output_point_left point
          and right = Array.unsafe_get output_point_right point
          and weight = Array.unsafe_get output_point_weight point in
          let px = interpolate (Array.unsafe_get source_positions.x left)
              (Array.unsafe_get source_positions.x right) weight
          and py = interpolate (Array.unsafe_get source_positions.y left)
              (Array.unsafe_get source_positions.y right) weight
          and pz = interpolate (Array.unsafe_get source_positions.z left)
              (Array.unsafe_get source_positions.z right) weight in
          if !first_error < 0 && not (Float.is_finite px && Float.is_finite py && Float.is_finite pz) then
            first_error := point;
          Array.unsafe_set x point px;
          Array.unsafe_set y point py;
          Array.unsafe_set z point pz
        done;
        Array.unsafe_set position_errors block !first_error);
      (match Array.find_opt (fun point -> point >= 0) position_errors with
       | Some point -> fail (Printf.sprintf
           "interpolated position is non-finite at output point %d" point)
       | None -> ());
      let output_topology = Topology.create_owned ~point_count:output_point_count
          ~vertex_points ~primitive_offsets ~primitive_kinds |> Result.get_ok in
      let attributes = List.map (fun attribute -> match Attribute.owner attribute with
        | Attribute.Point -> Curve_join.interpolate_attribute ?cancel ~grain
            output_point_left output_point_right output_point_weight
            [||] [||] [||] attribute
        | Attribute.Vertex -> Curve_join.interpolate_attribute ?cancel ~grain
            [||] [||] [||] vertex_left vertex_right weights attribute
        | Attribute.Primitive ->
            Topology_remap.attribute ?cancel ~grain primitive_map attribute
        | Attribute.Detail -> attribute) (Geometry.attributes geometry) in
      let nearest left right weight = Array.init (Array.length weight) (fun index ->
        if Array.unsafe_get weight index < 0.5
        then Array.unsafe_get left index else Array.unsafe_get right index) in
      let point_group_map = lazy (nearest output_point_left output_point_right
          output_point_weight)
      and vertex_group_map = lazy (nearest vertex_left vertex_right weights) in
      let groups = List.map (fun group -> match Group.owner group with
        | Group.Point -> Topology_remap.group ?cancel ~grain
            (Lazy.force point_group_map) group
        | Group.Vertex -> Topology_remap.group ?cancel ~grain
            (Lazy.force vertex_group_map) group
        | Group.Primitive -> Topology_remap.group ?cancel ~grain primitive_map group)
          (Geometry.groups geometry) in
      let edge_groups = remap_edge_groups ?cancel ~grain ~source_index
          ~target_topology:output_topology ~source_edge_of_vertex
          (Geometry.edge_groups geometry) in
      Geometry.create
        ~positions:(Packed.Float3.View.of_owned_exn ~x ~y ~z)
        ~topology:output_topology ~attributes ~groups ~edge_groups ()
    end
  with Error.Fail message -> Error message
