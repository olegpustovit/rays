type curve_join_end = Join_curve_start | Join_curve_end
type curve_join_pick = { primitive : int; end_ : curve_join_end }

let fail message = raise (Error.Fail message)

let validate_selection operation selection primitive_count =
  match selection with
  | None -> ()
  | Some group ->
      if Group.owner group <> Group.Primitive then
        fail (operation ^ " selection must own primitives");
      if Group.length group <> primitive_count then
        fail (operation ^ " selection length does not match primitive count")

let remap_attribute ?cancel ?grain mapping attribute =
  Topology_remap.attribute ?cancel ~grain:(Option.value ~default:16_384 grain) mapping attribute

let interpolate_attribute ?cancel ?grain point_left point_right point_weight vertex_left
    vertex_right vertex_weight attribute =
  let mapping =
    match Attribute.owner attribute with
    | Attribute.Point -> Some (point_left, point_right, point_weight)
    | Attribute.Vertex -> Some (vertex_left, vertex_right, vertex_weight)
    | Attribute.Primitive | Attribute.Detail -> None
  in
  match mapping with
  | None -> attribute
  | Some (left, right, weight) ->
      let grain = Option.value grain ~default:16_384 in
      if grain <= 0 then fail "grain must be positive";
      Topology_remap.blend_attribute ?cancel ~grain left right weight attribute

type endpoint_tree = {
  endpoint_ids : int array;
  endpoint_node : int array;
  endpoint_x : float array;
  endpoint_y : float array;
  endpoint_z : float array;
  left : int array;
  right : int array;
  parent : int array;
  axis : Bytes.t;
  active : Bytes.t;
  active_count : int array;
  mutable nearest_endpoint : int;
  mutable nearest_distance : float;
  root : int;
}

let[@inline always] robust_distance_xyz ax ay az bx by bz =
  let dx = ax -. bx and dy = ay -. by and dz = az -. bz in
  Float.hypot (Float.hypot dx dy) dz

let endpoint_tree ?cancel endpoint_x endpoint_y endpoint_z =
  let count = Array.length endpoint_x in
  if Array.length endpoint_y <> count || Array.length endpoint_z <> count then
    invalid_arg "Curve Join endpoint coordinate length mismatch";
  let endpoint_ids = Array.init count Fun.id
  and endpoint_node = Array.make count (-1)
  and left = Array.make count (-1)
  and right = Array.make count (-1)
  and parent = Array.make count (-1)
  and axis = Bytes.make count '\000'
  and active = Bytes.make count '\001'
  and active_count = Array.make count 0 in
  let compare axis left right =
    let order =
      if axis = 0 then Float.compare endpoint_x.(left) endpoint_x.(right)
      else if axis = 1 then Float.compare endpoint_y.(left) endpoint_y.(right)
      else Float.compare endpoint_z.(left) endpoint_z.(right)
    in
    if order <> 0 then order else Int.compare left right
  in
  let swap left right =
    let value = endpoint_ids.(left) in
    endpoint_ids.(left) <- endpoint_ids.(right);
    endpoint_ids.(right) <- value
  in
  let median_of_three axis first middle last =
    let a = endpoint_ids.(first) and b = endpoint_ids.(middle) and c = endpoint_ids.(last) in
    if compare axis a b <= 0 then
      if compare axis b c <= 0 then middle else if compare axis a c <= 0 then last else first
    else if compare axis a c <= 0 then first
    else if compare axis b c <= 0 then last
    else middle
  in
  let partition axis first last pivot_index =
    let pivot = endpoint_ids.(pivot_index) in
    swap pivot_index last;
    let store = ref first in
    for index = first to last - 1 do
      Cancel.poll cancel index 16_383;
      if compare axis endpoint_ids.(index) pivot < 0 then begin
        swap index !store;
        incr store
      end
    done;
    swap !store last;
    !store
  in
  let select axis first last target =
    let first = ref first and last = ref last in
    while !first < !last do
      Cancel.check_opt cancel;
      let middle = !first + ((!last - !first) lsr 1) in
      let pivot = median_of_three axis !first middle !last in
      let pivot = partition axis !first !last pivot in
      if target < pivot then last := pivot - 1
      else if target > pivot then first := pivot + 1
      else begin
        first := target;
        last := target
      end
    done
  in
  let min_x = Array.make Sys.int_size 0.
  and max_x = Array.make Sys.int_size 0.
  and min_y = Array.make Sys.int_size 0.
  and max_y = Array.make Sys.int_size 0.
  and min_z = Array.make Sys.int_size 0.
  and max_z = Array.make Sys.int_size 0. in
  let choose_axis first last depth =
    let endpoint = endpoint_ids.(first) in
    min_x.(depth) <- endpoint_x.(endpoint);
    max_x.(depth) <- endpoint_x.(endpoint);
    min_y.(depth) <- endpoint_y.(endpoint);
    max_y.(depth) <- endpoint_y.(endpoint);
    min_z.(depth) <- endpoint_z.(endpoint);
    max_z.(depth) <- endpoint_z.(endpoint);
    for index = first + 1 to last - 1 do
      Cancel.poll cancel index 16_383;
      let endpoint = endpoint_ids.(index) in
      let x = endpoint_x.(endpoint) and y = endpoint_y.(endpoint) and z = endpoint_z.(endpoint) in
      if x < min_x.(depth) then min_x.(depth) <- x;
      if x > max_x.(depth) then max_x.(depth) <- x;
      if y < min_y.(depth) then min_y.(depth) <- y;
      if y > max_y.(depth) then max_y.(depth) <- y;
      if z < min_z.(depth) then min_z.(depth) <- z;
      if z > max_z.(depth) then max_z.(depth) <- z
    done;
    let x_span = max_x.(depth) -. min_x.(depth)
    and y_span = max_y.(depth) -. min_y.(depth)
    and z_span = max_z.(depth) -. min_z.(depth) in
    if y_span > x_span then if z_span > y_span then 2 else 1 else if z_span > x_span then 2 else 0
  in
  let rec build first last depth parent_node =
    if first >= last then -1
    else begin
      let axis_value = choose_axis first last depth and middle = first + ((last - first) lsr 1) in
      select axis_value first (last - 1) middle;
      parent.(middle) <- parent_node;
      Bytes.set axis middle (Char.chr axis_value);
      endpoint_node.(endpoint_ids.(middle)) <- middle;
      let left_node = build first middle (depth + 1) middle
      and right_node = build (middle + 1) last (depth + 1) middle in
      left.(middle) <- left_node;
      right.(middle) <- right_node;
      active_count.(middle) <-
        (1
        + (if left_node < 0 then 0 else active_count.(left_node))
        + if right_node < 0 then 0 else active_count.(right_node));
      middle
    end
  in
  let root = build 0 count 0 (-1) in
  {
    endpoint_ids;
    endpoint_node;
    endpoint_x;
    endpoint_y;
    endpoint_z;
    left;
    right;
    parent;
    axis;
    active;
    active_count;
    nearest_endpoint = -1;
    nearest_distance = infinity;
    root;
  }

let endpoint_tree_remove tree endpoint =
  if Bytes.get tree.active endpoint <> '\000' then begin
    Bytes.set tree.active endpoint '\000';
    let node = ref tree.endpoint_node.(endpoint) in
    while !node >= 0 do
      tree.active_count.(!node) <- tree.active_count.(!node) - 1;
      node := tree.parent.(!node)
    done
  end

let endpoint_tree_nearest ?cancel tree query_endpoint =
  let qx = tree.endpoint_x.(query_endpoint)
  and qy = tree.endpoint_y.(query_endpoint)
  and qz = tree.endpoint_z.(query_endpoint) in
  tree.nearest_endpoint <- -1;
  tree.nearest_distance <- infinity;
  let visited = ref 0 in
  let rec visit node =
    if node >= 0 && tree.active_count.(node) > 0 then begin
      incr visited;
      Cancel.poll cancel !visited 16_383;
      let endpoint = tree.endpoint_ids.(node) and axis = Char.code (Bytes.get tree.axis node) in
      let delta =
        if axis = 0 then qx -. tree.endpoint_x.(endpoint)
        else if axis = 1 then qy -. tree.endpoint_y.(endpoint)
        else qz -. tree.endpoint_z.(endpoint)
      in
      let near, far =
        if delta <= 0. then (tree.left.(node), tree.right.(node))
        else (tree.right.(node), tree.left.(node))
      in
      visit near;
      if Bytes.get tree.active endpoint <> '\000' then begin
        let distance =
          robust_distance_xyz qx qy qz tree.endpoint_x.(endpoint) tree.endpoint_y.(endpoint)
            tree.endpoint_z.(endpoint)
        in
        if
          distance < tree.nearest_distance
          || distance = tree.nearest_distance
             && (tree.nearest_endpoint < 0 || endpoint < tree.nearest_endpoint)
        then begin
          tree.nearest_endpoint <- endpoint;
          tree.nearest_distance <- distance
        end
      end;
      if abs_float delta <= tree.nearest_distance then visit far
    end
  in
  visit tree.root

type params = {
  cancel : Cancel.t option;
  grain : int option;
  primitives : Group.t option;
  picked_ends : curve_join_pick array option;
  orient_closest : bool;
  connect_closest_ends : bool;
  only_connected : bool;
  group_size : int option;
  keep_originals : bool;
  tolerance : float;
  wrap : bool;
}

let join
    ({
       cancel;
       grain;
       primitives;
       picked_ends;
       orient_closest;
       connect_closest_ends;
       only_connected;
       group_size;
       keep_originals;
       tolerance;
       wrap;
     } :
      params) geometry =
  try
    if (not (Float.is_finite tolerance)) || tolerance < 0. then
      fail "Curve Join tolerance must be finite and non-negative";
    (match group_size with
    | Some size when size <= 0 -> fail "Curve Join group size must be positive"
    | None | Some _ -> ());
    (match (primitives, picked_ends) with
    | Some _, Some _ -> fail "Curve Join primitive selection and picked ends are mutually exclusive"
    | None, _ | _, None -> ());
    (match picked_ends with
    | Some _ when connect_closest_ends ->
        fail "Curve Join picked ends and Connect Closest Ends are mutually exclusive"
    | None | Some _ -> ());
    Cancel.check_opt cancel;
    let topology = Geometry.topology geometry in
    let source = Topology.View.view topology in
    let positions = Packed.Float3.View.view (Geometry.positions geometry) in
    let primitive_count = Topology.primitive_count topology in
    validate_selection "Curve Join" primitives primitive_count;
    let selected_count =
      match picked_ends with
      | Some picks -> Array.length picks
      | None -> (
          match primitives with Some group -> Group.cardinality group | None -> primitive_count)
    in
    if selected_count = 0 then Ok geometry
    else begin
      let selected_primitives = Array.make selected_count 0
      and selected_index = Array.make primitive_count (-1) in
      (match picked_ends with
      | Some picks ->
          Array.iteri
            (fun index pick ->
              let primitive = pick.primitive in
              if primitive < 0 || primitive >= primitive_count then
                fail (Printf.sprintf "Curve Join picked primitive %d is out of bounds" primitive);
              if selected_index.(primitive) >= 0 then
                fail (Printf.sprintf "Curve Join picked primitive %d more than once" primitive);
              selected_index.(primitive) <- index;
              selected_primitives.(index) <- primitive)
            picks
      | None -> (
          match primitives with
          | Some group -> (
              match Group.View.order_view group with
              | Some order -> Array.blit order 0 selected_primitives 0 selected_count
              | None ->
                  let at = ref 0 in
                  Group.iter
                    (fun primitive ->
                      selected_primitives.(!at) <- primitive;
                      incr at)
                    group)
          | None ->
              Array.iteri (fun index _ -> selected_primitives.(index) <- index) selected_primitives));
      Array.iter
        (fun primitive ->
          if Topology.primitive_kind topology primitive <> Topology.Open_polyline then
            fail (Printf.sprintf "Curve Join primitive %d is not an open polygon curve" primitive))
        selected_primitives;
      let reversed = Bytes.make selected_count '\000'
      and welded = Bytes.make selected_count '\000'
      and selected_chain = Array.make selected_count 0 in
      let chain_count = ref 1 in
      let starts_subgroup index =
        match group_size with Some size -> index > 0 && index mod size = 0 | None -> false
      in
      let endpoint primitive reverse last =
        let first = source.primitive_offsets.(primitive)
        and stop = source.primitive_offsets.(primitive + 1) in
        let vertex = if last <> reverse then stop - 1 else first in
        source.vertex_points.(vertex)
      in
      let distance left right =
        if
          not
            (Float.is_finite positions.x.(left)
            && Float.is_finite positions.y.(left)
            && Float.is_finite positions.z.(left)
            && Float.is_finite positions.x.(right)
            && Float.is_finite positions.y.(right)
            && Float.is_finite positions.z.(right))
        then fail "Curve Join requires finite selected endpoint positions";
        robust_distance_xyz positions.x.(left) positions.y.(left) positions.z.(left)
          positions.x.(right) positions.y.(right) positions.z.(right)
      in
      if Option.is_some picked_ends then begin
        let picks = Option.get picked_ends in
        let picked_reverse index =
          let subgroup = starts_subgroup index in
          match picks.(index).end_ with
          | Join_curve_start -> index = 0 || subgroup
          | Join_curve_end -> index > 0 && not subgroup
        in
        if only_connected then
          for index = 0 to selected_count - 1 do
            Cancel.poll cancel index 4095;
            let subgroup = starts_subgroup index and reverse = picked_reverse index in
            if reverse then Bytes.set reversed index '\001';
            if index > 0 then begin
              let previous = selected_primitives.(index - 1)
              and primitive = selected_primitives.(index) in
              let previous_reversed = picked_reverse (index - 1) in
              let current_end = endpoint previous previous_reversed true
              and next_start = endpoint primitive reverse false in
              let join_distance = distance current_end next_start in
              if subgroup || join_distance > tolerance then begin
                selected_chain.(index) <- !chain_count;
                incr chain_count
              end
              else begin
                selected_chain.(index) <- !chain_count - 1;
                Bytes.set welded index '\001'
              end
            end
          done
        else begin
          let fixed_chain_count =
            match group_size with None -> 1 | Some size -> 1 + ((selected_count - 1) / size)
          in
          chain_count := fixed_chain_count;
          Support.run_ranges ?grain selected_count (fun ~first ~last ->
              Cancel.check_opt cancel;
              for index = first to last - 1 do
                let subgroup = starts_subgroup index and reverse = picked_reverse index in
                if reverse then Bytes.set reversed index '\001';
                selected_chain.(index) <-
                  (match group_size with None -> 0 | Some size -> index / size);
                if index > 0 && not subgroup then begin
                  let previous = selected_primitives.(index - 1)
                  and primitive = selected_primitives.(index) in
                  let current_end = endpoint previous (picked_reverse (index - 1)) true
                  and next_start = endpoint primitive reverse false in
                  if distance current_end next_start <= tolerance then Bytes.set welded index '\001'
                end
              done)
        end
      end
      else if connect_closest_ends then begin
        if selected_count > max_int / 2 then
          fail "Curve Join endpoint count exceeds OCaml array limits";
        let input_primitives = Array.copy selected_primitives in
        let endpoint_count = selected_count * 2 in
        let endpoint_x = Array.make endpoint_count 0.
        and endpoint_y = Array.make endpoint_count 0.
        and endpoint_z = Array.make endpoint_count 0. in
        Support.run_ranges ?grain endpoint_count (fun ~first ~last ->
            for endpoint_slot = first to last - 1 do
              Cancel.poll cancel endpoint_slot 16_383;
              let primitive = input_primitives.(endpoint_slot lsr 1) in
              let point = endpoint primitive false (endpoint_slot land 1 = 1) in
              let x = positions.x.(point) and y = positions.y.(point) and z = positions.z.(point) in
              if not (Float.is_finite x && Float.is_finite y && Float.is_finite z) then
                fail "Curve Join requires finite selected endpoint positions";
              endpoint_x.(endpoint_slot) <- x;
              endpoint_y.(endpoint_slot) <- y;
              endpoint_z.(endpoint_slot) <- z
            done);
        let tree = endpoint_tree ?cancel endpoint_x endpoint_y endpoint_z
        and used = Bytes.make selected_count '\000' in
        let remove primitive_slot =
          Bytes.set used primitive_slot '\001';
          endpoint_tree_remove tree (primitive_slot * 2);
          endpoint_tree_remove tree ((primitive_slot * 2) + 1)
        in
        let orient_root primitive_slot =
          if tree.active_count.(tree.root) = 0 then false
          else begin
            endpoint_tree_nearest ?cancel tree (primitive_slot * 2);
            let from_start = tree.nearest_distance in
            endpoint_tree_nearest ?cancel tree ((primitive_slot * 2) + 1);
            from_start < tree.nearest_distance
          end
        in
        remove 0;
        let current_endpoint = ref 1 and next_root = ref 1 in
        for index = 1 to selected_count - 1 do
          Cancel.poll cancel index 4095;
          endpoint_tree_nearest ?cancel tree !current_endpoint;
          let nearest = tree.nearest_endpoint and nearest_distance = tree.nearest_distance in
          if nearest < 0 then fail "Curve Join endpoint search exhausted early";
          let subgroup = starts_subgroup index in
          let disconnected = only_connected && nearest_distance > tolerance in
          let new_chain = subgroup || disconnected in
          let removed = ref false in
          let primitive_slot, reverse, join_distance =
            if new_chain then begin
              let primitive_slot =
                if subgroup then nearest lsr 1
                else begin
                  while !next_root < selected_count && Bytes.get used !next_root <> '\000' do
                    incr next_root
                  done;
                  if !next_root >= selected_count then
                    fail "Curve Join connected-component planning exhausted early";
                  !next_root
                end
              in
              remove primitive_slot;
              removed := true;
              (primitive_slot, orient_root primitive_slot, infinity)
            end
            else (nearest lsr 1, nearest land 1 = 1, nearest_distance)
          in
          selected_primitives.(index) <- input_primitives.(primitive_slot);
          if new_chain then begin
            selected_chain.(index) <- !chain_count;
            incr chain_count
          end
          else begin
            selected_chain.(index) <- !chain_count - 1;
            if reverse then Bytes.set reversed index '\001';
            if join_distance <= tolerance then Bytes.set welded index '\001'
          end;
          if not !removed then remove primitive_slot;
          current_endpoint := (primitive_slot * 2) + if reverse then 0 else 1
        done
      end
      else
        for index = 1 to selected_count - 1 do
          let previous = selected_primitives.(index - 1)
          and primitive = selected_primitives.(index) in
          let previous_reversed = Bytes.get reversed (index - 1) <> '\000' in
          let current_end = endpoint previous previous_reversed true
          and next_start = endpoint primitive false false
          and next_end = endpoint primitive false true in
          let start_distance = distance current_end next_start
          and end_distance = distance current_end next_end in
          let subgroup = starts_subgroup index in
          let reverse, distance =
            if subgroup then begin
              let reverse =
                if orient_closest && index + 1 < selected_count && not (starts_subgroup (index + 1))
                then
                  let following = selected_primitives.(index + 1) in
                  let following_start = endpoint following false false
                  and following_end = endpoint following false true
                  and root_start = endpoint primitive false false
                  and root_end = endpoint primitive false true in
                  let from_start =
                    min (distance root_start following_start) (distance root_start following_end)
                  and from_end =
                    min (distance root_end following_start) (distance root_end following_end)
                  in
                  from_start < from_end
                else false
              in
              (reverse, infinity)
            end
            else begin
              let reverse = orient_closest && end_distance < start_distance in
              (reverse, if reverse then end_distance else start_distance)
            end
          in
          if subgroup || (only_connected && distance > tolerance) then begin
            selected_chain.(index) <- !chain_count;
            if reverse then Bytes.set reversed index '\001';
            incr chain_count
          end
          else begin
            selected_chain.(index) <- !chain_count - 1;
            if reverse then Bytes.set reversed index '\001';
            if distance <= tolerance then Bytes.set welded index '\001'
          end
        done;
      Array.fill selected_index 0 primitive_count (-1);
      Array.iteri (fun index primitive -> selected_index.(primitive) <- index) selected_primitives;
      let chain_count = !chain_count in
      let chain_first = Array.make chain_count (-1)
      and chain_last = Array.make chain_count (-1)
      and chain_size = Array.make chain_count 0 in
      for index = 0 to selected_count - 1 do
        let chain = selected_chain.(index) in
        if chain_first.(chain) < 0 then chain_first.(chain) <- index;
        chain_last.(chain) <- index;
        let primitive = selected_primitives.(index) in
        let size =
          source.primitive_offsets.(primitive + 1) - source.primitive_offsets.(primitive)
        in
        chain_size.(chain) <-
          (chain_size.(chain) + size
          - if index > chain_first.(chain) && Bytes.get welded index <> '\000' then 1 else 0)
      done;
      if wrap then
        Array.iteri
          (fun chain size ->
            if size < 3 then
              fail
                (Printf.sprintf "Curve Join cannot wrap chain %d with fewer than three vertices"
                   chain))
          chain_size;
      let retained_primitive_count =
        if keep_originals then primitive_count else primitive_count - selected_count
      in
      if retained_primitive_count > max_int - chain_count then
        fail "Curve Join primitive cardinality exceeds OCaml array limits";
      let output_primitive_count = retained_primitive_count + chain_count in
      let descriptor_primitive = Array.make output_primitive_count (-1)
      and descriptor_chain = Array.make output_primitive_count (-1)
      and primitive_map = Array.make output_primitive_count 0
      and output_sizes = Array.make output_primitive_count 0
      and output_kinds = Array.make output_primitive_count Topology.Open_polyline in
      let output_primitive = ref 0 in
      let emit_primitive primitive =
        let output = !output_primitive in
        incr output_primitive;
        descriptor_primitive.(output) <- primitive;
        primitive_map.(output) <- primitive;
        output_sizes.(output) <-
          source.primitive_offsets.(primitive + 1) - source.primitive_offsets.(primitive);
        output_kinds.(output) <- Topology.primitive_kind topology primitive
      in
      let emit_chain chain =
        let first_index = chain_first.(chain) in
        let output = !output_primitive in
        incr output_primitive;
        descriptor_chain.(output) <- chain;
        primitive_map.(output) <- selected_primitives.(first_index);
        output_sizes.(output) <- chain_size.(chain);
        output_kinds.(output) <- (if wrap then Topology.Closed_polyline else Topology.Open_polyline)
      in
      if keep_originals then begin
        for primitive = 0 to primitive_count - 1 do
          emit_primitive primitive
        done;
        for chain = 0 to chain_count - 1 do
          emit_chain chain
        done
      end
      else
        for primitive = 0 to primitive_count - 1 do
          let index = selected_index.(primitive) in
          if index < 0 then emit_primitive primitive
          else begin
            let chain = selected_chain.(index) in
            if chain_first.(chain) = index then emit_chain chain
          end
        done;
      let primitive_offsets = Array.make (output_primitive_count + 1) 0 in
      for primitive = 0 to output_primitive_count - 1 do
        if primitive_offsets.(primitive) > max_int - output_sizes.(primitive) then
          fail "Curve Join output cardinality exceeds OCaml array limits";
        primitive_offsets.(primitive + 1) <-
          primitive_offsets.(primitive) + output_sizes.(primitive)
      done;
      let output_vertex_count = primitive_offsets.(output_primitive_count) in
      let vertex_map = Array.make output_vertex_count 0
      and vertex_points = Array.make output_vertex_count 0
      and edge_source_corner = Array.make output_vertex_count (-1) in
      let fill_source output_at previous source_first source_size reverse skip_first =
        let first_step = if skip_first then 1 else 0 in
        for step = first_step to source_size - 1 do
          let local = if reverse then source_size - 1 - step else step in
          let source_vertex = source_first + local and output = !output_at in
          vertex_map.(output) <- source_vertex;
          vertex_points.(output) <- source.vertex_points.(source_vertex);
          if !previous >= 0 then begin
            let source_edge_corner =
              if step = first_step && first_step = 0 then -1
              else if reverse then source_first + local
              else source_first + local - 1
            in
            edge_source_corner.(!previous) <- source_edge_corner
          end;
          previous := output;
          incr output_at
        done
      in
      Support.run_ranges ?grain output_primitive_count (fun ~first:first_output ~last:last_output ->
          Cancel.check_opt cancel;
          for output_primitive = first_output to last_output - 1 do
            let output_at = ref primitive_offsets.(output_primitive) and previous = ref (-1) in
            let single = descriptor_primitive.(output_primitive) in
            if single >= 0 then begin
              let source_first = source.primitive_offsets.(single)
              and source_size =
                source.primitive_offsets.(single + 1) - source.primitive_offsets.(single)
              in
              fill_source output_at previous source_first source_size false false;
              if Topology.primitive_kind topology single <> Topology.Open_polyline then
                edge_source_corner.(!previous) <- source_first + source_size - 1
            end
            else begin
              let chain = descriptor_chain.(output_primitive) in
              for index = chain_first.(chain) to chain_last.(chain) do
                let primitive = selected_primitives.(index) in
                let source_first = source.primitive_offsets.(primitive)
                and source_size =
                  source.primitive_offsets.(primitive + 1) - source.primitive_offsets.(primitive)
                in
                fill_source output_at previous source_first source_size
                  (Bytes.get reversed index <> '\000')
                  (index > chain_first.(chain) && Bytes.get welded index <> '\000')
              done;
              edge_source_corner.(!previous) <- -1
            end
          done);
      let output_topology =
        Topology.create_owned ~point_count:source.point_count ~vertex_points ~primitive_offsets
          ~primitive_kinds:output_kinds
        |> Result.get_ok
      in
      let attributes =
        List.map
          (fun attribute ->
            match Attribute.owner attribute with
            | Attribute.Point | Attribute.Detail -> attribute
            | Attribute.Vertex -> remap_attribute ?cancel ?grain vertex_map attribute
            | Attribute.Primitive -> remap_attribute ?cancel ?grain primitive_map attribute)
          (Geometry.attributes geometry)
      in
      let groups =
        List.map
          (fun group ->
            match Group.owner group with
            | Group.Point -> group
            | Group.Vertex ->
                let target =
                  Group.init ?grain ~owner:Group.Vertex ~name:(Group.name group) output_vertex_count
                    (fun vertex -> Group.mem vertex_map.(vertex) group)
                in
                Group.View.remap_order ~source:group ~source_of_target:vertex_map target
            | Group.Primitive ->
                let target =
                  Group.init ?grain ~owner:Group.Primitive ~name:(Group.name group)
                    output_primitive_count (fun output ->
                      let single = descriptor_primitive.(output) in
                      if single >= 0 then Group.mem single group
                      else begin
                        let chain = descriptor_chain.(output) and member = ref false in
                        for index = chain_first.(chain) to chain_last.(chain) do
                          if Group.mem selected_primitives.(index) group then member := true
                        done;
                        !member
                      end)
                in
                Group.View.remap_order ~source:group ~source_of_target:primitive_map target)
          (Geometry.groups geometry)
      in
      let edge_groups =
        match Geometry.edge_groups geometry with
        | [] -> []
        | source_groups ->
            let seen = Bytes.make source.point_count '\000' and unique_points = ref true in
            Array.iter
              (fun point ->
                if Bytes.get seen point <> '\000' then unique_points := false
                else Bytes.set seen point '\001')
              source.vertex_points;
            if !unique_points && not keep_originals then begin
              let edge_count kind size =
                size - 1
                +
                match kind with
                | Topology.Polygon | Topology.Closed_polyline -> 1
                | Topology.Open_polyline -> 0
              in
              let source_corner_edge = Array.make (Array.length source.vertex_points) (-1) in
              let source_edge_at = ref 0 in
              for primitive = 0 to primitive_count - 1 do
                let first = source.primitive_offsets.(primitive)
                and size =
                  source.primitive_offsets.(primitive + 1) - source.primitive_offsets.(primitive)
                in
                let edges = edge_count (Topology.primitive_kind topology primitive) size in
                for local = 0 to edges - 1 do
                  source_corner_edge.(first + local) <- !source_edge_at;
                  incr source_edge_at
                done
              done;
              let target_edge_offsets = Array.make (output_primitive_count + 1) 0 in
              for primitive = 0 to output_primitive_count - 1 do
                target_edge_offsets.(primitive + 1) <-
                  target_edge_offsets.(primitive)
                  + edge_count output_kinds.(primitive) output_sizes.(primitive)
              done;
              let target_edge_count = target_edge_offsets.(output_primitive_count) in
              List.map
                (fun source_group ->
                  let bits = Bytes.make ((target_edge_count + 7) / 8) '\000' in
                  let set edge =
                    let byte = edge lsr 3 and mask = 1 lsl (edge land 7) in
                    Bytes.set bits byte (Char.chr (Char.code (Bytes.get bits byte) lor mask))
                  in
                  for primitive = 0 to output_primitive_count - 1 do
                    Cancel.poll cancel primitive 4095;
                    let edges =
                      target_edge_offsets.(primitive + 1) - target_edge_offsets.(primitive)
                    and first = primitive_offsets.(primitive) in
                    for local = 0 to edges - 1 do
                      let source_corner = edge_source_corner.(first + local) in
                      if source_corner >= 0 then begin
                        let source_edge = source_corner_edge.(source_corner) in
                        if source_edge >= 0 && Edge_group.mem source_edge source_group then
                          set (target_edge_offsets.(primitive) + local)
                      end
                    done
                  done;
                  Edge_group.View.of_owned_bits ~topology:output_topology
                    ~edge_count:target_edge_count ~name:(Edge_group.name source_group) bits)
                source_groups
            end
            else begin
              let source_index = Topology_index.create ?cancel topology
              and target_index = Topology_index.create ?cancel output_topology in
              let target_view = Topology_index.View.view target_index in
              List.map
                (fun source_group ->
                  let builder =
                    Edge_group.Builder.create ~topology:output_topology ~index:target_index
                      ~name:(Edge_group.name source_group)
                  in
                  for vertex = 0 to output_vertex_count - 1 do
                    Cancel.poll cancel vertex 16_383;
                    let source_corner = edge_source_corner.(vertex) in
                    if source_corner >= 0 then begin
                      let source_edge = Topology_index.edge_of_vertex source_index source_corner in
                      let target_edge = target_view.edge_of_vertex.(vertex) in
                      if
                        source_edge >= 0 && target_edge >= 0
                        && Edge_group.mem source_edge source_group
                      then Edge_group.Builder.set builder target_edge true
                    end
                  done;
                  Edge_group.Builder.freeze builder)
                source_groups
            end
      in
      Geometry.create ~positions:(Geometry.positions geometry) ~topology:output_topology ~attributes
        ~groups ~edge_groups ()
    end
  with Error.Fail message -> Error message
