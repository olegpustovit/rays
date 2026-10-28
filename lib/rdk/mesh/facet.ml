
let[@inline always] max_abs3 x y z =
  let x = abs_float x and y = abs_float y and z = abs_float z in
  if x >= y then if x >= z then x else z else if y >= z then y else z

let remap_point_attribute ?cancel ~grain point_map attribute =
  if Attribute.owner attribute <> Attribute.Point then Ok attribute
  else Ok (Topology_remap.attribute ?cancel ~grain point_map attribute)

let remap_point_group ?cancel ~grain point_map group =
  if Group.owner group <> Group.Point then group
  else Topology_remap.group ?cancel ~grain point_map group

let remap_vertex_attribute ?cancel ~grain vertex_map attribute =
  if Attribute.owner attribute <> Attribute.Vertex then Ok attribute
  else Ok (Topology_remap.attribute ?cancel ~grain vertex_map attribute)

let remap_vertex_group ?cancel ~grain vertex_map group =
  if Group.owner group <> Group.Vertex then group
  else Topology_remap.group ?cancel ~grain vertex_map group

let remap_edge_groups_from_index ?cancel ~grain ~source_index ~target_topology
    groups =
  match groups with
  | [] -> []
  | groups ->
      let target_index = Topology_index.create ?cancel target_topology in
      let source = Topology_index.Private.view source_index
      and target = Topology_index.Private.view target_index in
      List.map (fun group ->
        Edge_group.init ~grain ~topology:target_topology ~index:target_index
          ~name:(Edge_group.name group) (fun edge ->
            if edge land 4095 = 0 then Cancel.check_opt cancel;
            let first = target.edge_offsets.(edge) in
            let target_vertex = target.edge_vertices.(first) in
            let source_edge = source.edge_of_vertex.(target_vertex) in
            source_edge >= 0 && Edge_group.mem source_edge group)) groups

let remap_edge_groups ?cancel ~grain ~source_topology ~target_topology groups =
  match groups with
  | [] -> []
  | _ ->
      let source_index = Topology_index.create ?cancel source_topology in
      remap_edge_groups_from_index ?cancel ~grain ~source_index ~target_topology
        groups

let remap_healed_edge_groups ?cancel ~grain ~source_index ~target_topology
    ~vertex_map groups =
  match groups with
  | [] -> []
  | groups ->
      let target_index = Topology_index.create ?cancel target_topology in
      let source = Topology_index.Private.view source_index
      and target = Topology_index.Private.view target_index in
      List.map (fun group ->
        Edge_group.init ~grain ~topology:target_topology ~index:target_index
          ~name:(Edge_group.name group) (fun edge ->
            if edge land 4095 = 0 then Cancel.check_opt cancel;
            let found = ref false
            and incidence = ref target.edge_offsets.(edge) in
            let incidence_last = target.edge_offsets.(edge + 1) in
            while not !found && !incidence < incidence_last do
              let target_vertex = target.edge_vertices.(!incidence) in
              let source_first = vertex_map.(target_vertex)
              and source_last = vertex_map.(target.next_vertex.(target_vertex)) in
              let source_vertex = ref source_first and traversed = ref 0 in
              while not !found && !source_vertex <> source_last
                  && !traversed <= Array.length source.next_vertex do
                let source_edge = source.edge_of_vertex.(!source_vertex) in
                if source_edge >= 0 && Edge_group.mem source_edge group then
                  found := true;
                source_vertex := source.next_vertex.(!source_vertex);
                incr traversed
              done;
              incr incidence
            done;
            !found)) groups

let[@inline always] max_abs9 ax ay az bx by bz cx cy cz =
  let a = max_abs3 ax ay az and b = max_abs3 bx by bz
  and c = max_abs3 cx cy cz in
  if a >= b then if a >= c then a else c else if b >= c then b else c

let[@inline always] inline_corner ~tolerance positions topology previous next
    vertex =
  let previous_point = topology.Topology.Private.vertex_points.(previous)
  and point = topology.vertex_points.(vertex)
  and next_point = topology.vertex_points.(next) in
  let ax = positions.Packed.Float3.Private.x.(previous_point)
  and ay = positions.y.(previous_point)
  and az = positions.z.(previous_point)
  and bx = positions.x.(point)
  and by = positions.y.(point)
  and bz = positions.z.(point)
  and cx = positions.x.(next_point)
  and cy = positions.y.(next_point)
  and cz = positions.z.(next_point) in
  if not (Float.is_finite ax && Float.is_finite ay && Float.is_finite az
      && Float.is_finite bx && Float.is_finite by && Float.is_finite bz
      && Float.is_finite cx && Float.is_finite cy && Float.is_finite cz)
  then 2
  else
    let scale = max_abs9 ax ay az bx by bz cx cy cz in
    if scale = 0. then 1
    else
      let ax = ax /. scale and ay = ay /. scale and az = az /. scale
      and bx = bx /. scale and by = by /. scale and bz = bz /. scale
      and cx = cx /. scale and cy = cy /. scale and cz = cz /. scale in
      let dx = cx -. ax and dy = cy -. ay and dz = cz -. az
      and vx = bx -. ax and vy = by -. ay and vz = bz -. az in
      let difference_scale =
        let d = max_abs3 dx dy dz and v = max_abs3 vx vy vz in
        if d >= v then d else v in
      let denominator = (dx *. dx) +. (dy *. dy) +. (dz *. dz) in
      let parameter = if denominator = 0. then 0. else
        let value = ((vx *. dx) +. (vy *. dy) +. (vz *. dz)) /. denominator in
        if value <= 0. then 0. else if value >= 1. then 1. else value in
      let rx = vx -. (parameter *. dx)
      and ry = vy -. (parameter *. dy)
      and rz = vz -. (parameter *. dz) in
      let scaled_tolerance = (tolerance /. scale)
        +. (64. *. Float.epsilon *. difference_scale) in
      if (rx *. rx) +. (ry *. ry) +. (rz *. rz)
          <= scaled_tolerance *. scaled_tolerance
      then 1 else 0

let remove_inline_points ?cancel ?(grain = 16_384) ?primitives ~distance geometry =
  if grain <= 0 then invalid_arg "Rdk_mesh.Facet.facet: grain must be positive";
  if not (Float.is_finite distance) || distance < 0. then
    Error "Facet inline distance must be finite and non-negative"
  else begin
    Cancel.check_opt cancel;
    let source_topology = Geometry.topology geometry in
    let topology = Topology.Private.view source_topology
    and positions = Packed.Float3.Private.view (Geometry.positions geometry) in
    let primitive_count = Array.length topology.primitive_offsets - 1
    and vertex_count = Array.length topology.vertex_points in
    let selection_error = match primitives with
      | Some group when Group.owner group <> Group.Primitive ->
          Some "Facet selection must own primitives"
      | Some group when Group.length group <> primitive_count ->
          Some "Facet selection length does not match primitive count"
      | _ -> None in
    match selection_error with
    | Some message -> Error message
    | None ->
    let selected primitive = match primitives with
      | None -> true
      | Some group -> Group.mem primitive group in
    let keep = Bytes.make vertex_count '\001'
    and queued = Bytes.make vertex_count '\000'
    and previous = Array.make vertex_count 0
    and next = Array.make vertex_count 0
    and queue = Array.make vertex_count 0
    and retained_counts = Array.make primitive_count 0
    and failures = Bytes.make primitive_count '\000' in
    if primitive_count > 0 then Parallel.for_
        ~chunk_size:(max 1 (grain / 8)) ~start:0 ~finish:(primitive_count - 1)
        (fun primitive ->
      if primitive land 1023 = 0 then Cancel.check_opt cancel;
      let first = topology.primitive_offsets.(primitive)
      and last = topology.primitive_offsets.(primitive + 1) in
      let count = last - first in
      if not (selected primitive)
          || Topology.primitive_kind source_topology primitive <> Topology.Polygon
          || count <= 3 then retained_counts.(primitive) <- count
      else begin
        for local = 0 to count - 1 do
          let vertex = first + local in
          previous.(vertex) <- first + ((local + count - 1) mod count);
          next.(vertex) <- first + ((local + 1) mod count);
          queue.(first + local) <- vertex;
          Bytes.set queued vertex '\001'
        done;
        let head = ref 0 and tail = ref 0 and queued_count = ref count
        and active = ref count in
        while !queued_count > 0 && !active > 3 do
          let vertex = queue.(first + !head) in
          head := (!head + 1) mod count;
          decr queued_count;
          Bytes.set queued vertex '\000';
          if Bytes.get keep vertex = '\001' then begin
            match inline_corner ~tolerance:distance positions topology
                previous.(vertex) next.(vertex) vertex with
            | 2 -> Bytes.set failures primitive '\001'
            | 1 ->
                let left = previous.(vertex) and right = next.(vertex) in
                Bytes.set keep vertex '\000';
                decr active;
                next.(left) <- right;
                previous.(right) <- left;
                if Bytes.get keep left = '\001'
                    && Bytes.get queued left = '\000' then begin
                  queue.(first + !tail) <- left;
                  tail := (!tail + 1) mod count;
                  incr queued_count;
                  Bytes.set queued left '\001'
                end;
                if Bytes.get keep right = '\001'
                    && Bytes.get queued right = '\000' then begin
                  queue.(first + !tail) <- right;
                  tail := (!tail + 1) mod count;
                  incr queued_count;
                  Bytes.set queued right '\001'
                end
            | _ -> ()
          end
        done;
        retained_counts.(primitive) <- !active
      end);
    let invalid = ref (-1) and primitive = ref 0 in
    while !invalid < 0 && !primitive < primitive_count do
      if Bytes.get failures !primitive <> '\000' then invalid := !primitive;
      incr primitive
    done;
    if !invalid >= 0 then Error (Printf.sprintf
        "Facet Remove Inline Points encountered a non-finite position in primitive %d"
        !invalid)
    else begin
      let output_vertices = ref 0 in
      for primitive = 0 to primitive_count - 1 do
        output_vertices := !output_vertices + retained_counts.(primitive)
      done;
      if !output_vertices = vertex_count then Ok geometry
      else begin
        let primitive_offsets = Array.make (primitive_count + 1) 0 in
        for primitive = 0 to primitive_count - 1 do
          primitive_offsets.(primitive + 1) <- primitive_offsets.(primitive)
            + retained_counts.(primitive)
        done;
        let vertex_map = Array.make !output_vertices 0 in
        if primitive_count > 0 then Parallel.for_
            ~chunk_size:(max 1 (grain / 8)) ~start:0
            ~finish:(primitive_count - 1) (fun primitive ->
          if primitive land 1023 = 0 then Cancel.check_opt cancel;
          let output = ref primitive_offsets.(primitive) in
          for vertex = topology.primitive_offsets.(primitive)
              to topology.primitive_offsets.(primitive + 1) - 1 do
            if Bytes.get keep vertex = '\001' then begin
              vertex_map.(!output) <- vertex;
              incr output
            end
          done);
        let source_points = topology.point_count in
        let source_used = Bytes.make source_points '\000'
        and target_used = Bytes.make source_points '\000' in
        for vertex = 0 to vertex_count - 1 do
          Bytes.set source_used topology.vertex_points.(vertex) '\001'
        done;
        for vertex = 0 to !output_vertices - 1 do
          Bytes.set target_used topology.vertex_points.(vertex_map.(vertex)) '\001'
        done;
        let output_points = ref 0 in
        for point = 0 to source_points - 1 do
          if Bytes.get target_used point = '\001'
              || Bytes.get source_used point = '\000' then incr output_points
        done;
        let point_map = Array.make !output_points 0
        and old_to_new = Array.make source_points (-1) and output = ref 0 in
        for point = 0 to source_points - 1 do
          if Bytes.get target_used point = '\001'
              || Bytes.get source_used point = '\000' then begin
            point_map.(!output) <- point;
            old_to_new.(point) <- !output;
            incr output
          end
        done;
        let vertex_points = Array.make !output_vertices 0 in
        if !output_vertices > 0 then Parallel.for_ ~chunk_size:grain ~start:0
            ~finish:(!output_vertices - 1) (fun vertex ->
          if vertex land 4095 = 0 then Cancel.check_opt cancel;
          vertex_points.(vertex) <- old_to_new.
            (topology.vertex_points.(vertex_map.(vertex))));
        let target_topology = Topology.Private.create_validated_owned
            ~point_count:!output_points ~vertex_points ~primitive_offsets
            ~primitive_kinds:(Bytes.copy topology.primitive_kinds) in
        let point_identity = !output_points = source_points in
        let positions = if point_identity then Geometry.positions geometry
          else Packed.Float3.Private.of_owned_exn
            ~x:(Topology_remap.select_float ?cancel ~grain point_map positions.x)
            ~y:(Topology_remap.select_float ?cancel ~grain point_map positions.y)
            ~z:(Topology_remap.select_float ?cancel ~grain point_map positions.z) in
        let rec attributes result = function
          | [] -> Ok (List.rev result)
          | attribute :: rest ->
              let remapped = if point_identity then Ok attribute
                else remap_point_attribute ?cancel ~grain point_map attribute in
              Result.bind remapped (fun attribute ->
                Result.bind
                  (remap_vertex_attribute ?cancel ~grain vertex_map attribute)
                  (fun attribute -> attributes (attribute :: result) rest)) in
        Result.bind (attributes [] (Geometry.attributes geometry))
          (fun attributes ->
        let groups = List.map (fun group ->
            let group = if point_identity then group
              else remap_point_group ?cancel ~grain point_map group in
            remap_vertex_group ?cancel ~grain vertex_map group)
            (Geometry.groups geometry) in
        let edge_groups = match Geometry.edge_groups geometry with
          | [] -> []
          | groups ->
              let source_index = Topology_index.create ?cancel source_topology in
              remap_healed_edge_groups ?cancel ~grain ~source_index
                ~target_topology ~vertex_map groups in
        Geometry.create ~positions ~topology:target_topology ~attributes ~groups
          ~edge_groups ())
      end
    end
  end

let unique_points_all ?cancel ?(grain = 16_384) geometry =
  if grain <= 0 then invalid_arg "Rdk_mesh.Facet.facet: grain must be positive";
  let source_topology = Geometry.topology geometry in
  let topology = Topology.Private.view source_topology in
  let index = Topology_index.create ?cancel source_topology in
  let incidence = Topology_index.Private.view index in
  let source_points = topology.point_count
  and source_vertices = Array.length topology.vertex_points in
  let shared = ref false in
  let point = ref 0 in
  while not !shared && !point < source_points do
    if !point land 4095 = 0 then Cancel.check_opt cancel;
    if incidence.point_offsets.(!point + 1)
        - incidence.point_offsets.(!point) > 1 then shared := true;
    incr point
  done;
  if not !shared then Ok geometry
  else begin
    let isolated = ref 0 in
    for point = 0 to source_points - 1 do
      if point land 4095 = 0 then Cancel.check_opt cancel;
      if incidence.point_offsets.(point) = incidence.point_offsets.(point + 1)
      then incr isolated
    done;
    if !isolated > max_int - source_vertices then
      Error "Facet Unique Points output exceeds integer cardinality limits"
    else begin
      let output_points = source_vertices + !isolated in
      let point_map = Array.make output_points 0 in
      if source_vertices > 0 then Parallel.for_ ~chunk_size:grain ~start:0
          ~finish:(source_vertices - 1) (fun vertex ->
        if vertex land 4095 = 0 then Cancel.check_opt cancel;
        point_map.(vertex) <- topology.vertex_points.(vertex));
      let output = ref source_vertices in
      for point = 0 to source_points - 1 do
        if incidence.point_offsets.(point) = incidence.point_offsets.(point + 1)
        then begin point_map.(!output) <- point; incr output end
      done;
      let vertex_points = Array.init source_vertices Fun.id in
      let target_topology = Topology.Private.create_validated_owned
          ~point_count:output_points ~vertex_points
          ~primitive_offsets:(Array.copy topology.primitive_offsets)
          ~primitive_kinds:(Bytes.copy topology.primitive_kinds) in
      let source_positions = Packed.Float3.Private.view
          (Geometry.positions geometry) in
      let positions = Packed.Float3.Private.of_owned_exn
          ~x:(Topology_remap.select_float ?cancel ~grain point_map source_positions.x)
          ~y:(Topology_remap.select_float ?cancel ~grain point_map source_positions.y)
          ~z:(Topology_remap.select_float ?cancel ~grain point_map source_positions.z) in
      let rec attributes result = function
        | [] -> Ok (List.rev result)
        | attribute :: rest ->
            Result.bind (remap_point_attribute ?cancel ~grain point_map attribute)
              (fun attribute -> attributes (attribute :: result) rest) in
      Result.bind (attributes [] (Geometry.attributes geometry))
        (fun attributes ->
      let groups = List.map (remap_point_group ?cancel ~grain point_map)
          (Geometry.groups geometry) in
      let edge_groups = remap_edge_groups_from_index ?cancel ~grain
          ~source_index:index ~target_topology (Geometry.edge_groups geometry) in
      Geometry.create ~positions ~topology:target_topology ~attributes ~groups
        ~edge_groups ())
    end
  end

let unique_points_selected ?cancel ?(grain = 16_384) primitives geometry =
  if grain <= 0 then invalid_arg "Rdk_mesh.Facet.facet: grain must be positive";
  let source_topology = Geometry.topology geometry in
  let topology = Topology.Private.view source_topology in
  let primitive_count = Topology.primitive_count source_topology in
  if Group.owner primitives <> Group.Primitive then
    Error "Facet selection must own primitives"
  else if Group.length primitives <> primitive_count then
    Error "Facet selection length does not match primitive count"
  else begin
    let selected_count = Group.cardinality primitives in
    if selected_count = 0 then Ok geometry
    else if selected_count = primitive_count then
      unique_points_all ?cancel ~grain geometry
    else begin
      let source_points = topology.point_count
      and source_vertices = Array.length topology.vertex_points in
      let incidence_count = Bytes.make source_points '\000'
      and selected_reference = Bytes.make source_points '\000'
      and unselected_reference = Bytes.make source_points '\000' in
      for primitive = 0 to primitive_count - 1 do
        if primitive land 1023 = 0 then Cancel.check_opt cancel;
        let is_selected = Group.mem primitive primitives in
        for vertex = topology.primitive_offsets.(primitive)
            to topology.primitive_offsets.(primitive + 1) - 1 do
          let point = topology.vertex_points.(vertex) in
          let count = Char.code (Bytes.unsafe_get incidence_count point) in
          if count < 2 then
            Bytes.unsafe_set incidence_count point (Char.unsafe_chr (count + 1));
          Bytes.unsafe_set
            (if is_selected then selected_reference else unselected_reference)
            point '\001'
        done
      done;
      let selected_shared = ref false and point = ref 0 in
      while not !selected_shared && !point < source_points do
        if !point land 4095 = 0 then Cancel.check_opt cancel;
        if Bytes.unsafe_get selected_reference !point <> '\000'
            && Char.code (Bytes.unsafe_get incidence_count !point) > 1 then
          selected_shared := true;
        incr point
      done;
      if not !selected_shared then Ok geometry
      else begin
        let selected_offsets = Array.make (primitive_count + 1) 0 in
        for primitive = 0 to primitive_count - 1 do
          let selected_vertices = if Group.mem primitive primitives then
              topology.primitive_offsets.(primitive + 1)
                - topology.primitive_offsets.(primitive)
            else 0 in
          selected_offsets.(primitive + 1) <- selected_offsets.(primitive)
              + selected_vertices
        done;
        let retained = Bytes.make source_points '\000'
        and retained_count = ref 0 in
        for source_point = 0 to source_points - 1 do
          if source_point land 4095 = 0 then Cancel.check_opt cancel;
          if Bytes.unsafe_get incidence_count source_point = '\000'
              || Bytes.unsafe_get unselected_reference source_point <> '\000'
          then begin
            Bytes.set retained source_point '\001';
            incr retained_count
          end
        done;
        let selected_vertices = selected_offsets.(primitive_count) in
        if !retained_count > max_int - selected_vertices then
          Error "Facet Unique Points output exceeds integer cardinality limits"
        else begin
          let output_points = !retained_count + selected_vertices in
          let point_map = Array.make output_points 0
          and old_to_retained = Array.make source_points (-1) in
          let output = ref 0 in
          for source_point = 0 to source_points - 1 do
            if Bytes.get retained source_point = '\001' then begin
              point_map.(!output) <- source_point;
              old_to_retained.(source_point) <- !output;
              incr output
            end
          done;
          let vertex_points = Array.make source_vertices 0 in
          if primitive_count > 0 then Parallel.for_
              ~chunk_size:(max 1 (grain / 8)) ~start:0
              ~finish:(primitive_count - 1) (fun primitive ->
            if primitive land 1023 = 0 then Cancel.check_opt cancel;
            let first = topology.primitive_offsets.(primitive)
            and last = topology.primitive_offsets.(primitive + 1) in
            if Group.mem primitive primitives then begin
              let target = !retained_count + selected_offsets.(primitive) in
              for vertex = first to last - 1 do
                let point = target + vertex - first in
                point_map.(point) <- topology.vertex_points.(vertex);
                vertex_points.(vertex) <- point
              done
            end else
              for vertex = first to last - 1 do
                vertex_points.(vertex) <- old_to_retained.
                    (topology.vertex_points.(vertex))
              done);
          let target_topology = Topology.Private.create_validated_owned
              ~point_count:output_points ~vertex_points
              ~primitive_offsets:(Array.copy topology.primitive_offsets)
              ~primitive_kinds:(Bytes.copy topology.primitive_kinds) in
          let source_positions = Packed.Float3.Private.view
              (Geometry.positions geometry) in
          let positions = Packed.Float3.Private.of_owned_exn
              ~x:(Topology_remap.select_float ?cancel ~grain point_map source_positions.x)
              ~y:(Topology_remap.select_float ?cancel ~grain point_map source_positions.y)
              ~z:(Topology_remap.select_float ?cancel ~grain point_map source_positions.z) in
          let rec attributes result = function
            | [] -> Ok (List.rev result)
            | attribute :: rest ->
                Result.bind (remap_point_attribute ?cancel ~grain point_map attribute)
                  (fun attribute -> attributes (attribute :: result) rest) in
          Result.bind (attributes [] (Geometry.attributes geometry))
            (fun attributes ->
          let groups = List.map (remap_point_group ?cancel ~grain point_map)
              (Geometry.groups geometry) in
          let edge_groups = remap_edge_groups ?cancel ~grain ~source_topology
              ~target_topology (Geometry.edge_groups geometry) in
          Geometry.create ~positions ~topology:target_topology ~attributes ~groups
            ~edge_groups ())
        end
      end
    end
  end

let unique_points ?cancel ?grain ?primitives geometry = match primitives with
  | None -> unique_points_all ?cancel ?grain geometry
  | Some primitives -> unique_points_selected ?cancel ?grain primitives geometry

let orient_polygons ?cancel ?(grain = 16_384) ?primitives geometry =
  if grain <= 0 then invalid_arg "Rdk_mesh.Facet.facet: grain must be positive";
  let source_topology = Geometry.topology geometry in
  let topology = Topology.Private.view source_topology in
  let index = Topology_index.create ?cancel source_topology in
  let view = Topology_index.Private.view index in
  let primitive_count = Array.length topology.primitive_offsets - 1 in
  let selection_error = match primitives with
    | Some group when Group.owner group <> Group.Primitive ->
        Some "Facet selection must own primitives"
    | Some group when Group.length group <> primitive_count ->
        Some "Facet selection length does not match primitive count"
    | _ -> None in
  match selection_error with
  | Some message -> Error message
  | None ->
  let selected primitive = match primitives with
    | None -> true
    | Some group -> Group.mem primitive group in
  let orientation = Bytes.make primitive_count '\255' in
  let queue = Array.make primitive_count 0 in
  let queue_first = ref 0 and queue_last = ref 0
  and changed = ref false and invalid_edge = ref (-1)
  and contradiction = ref (-1) in
  let assign primitive flipped =
    Bytes.set orientation primitive (if flipped then '\001' else '\000');
    queue.(!queue_last) <- primitive;
    incr queue_last in
  let primitive = ref 0 in
  while !primitive < primitive_count && !invalid_edge < 0
      && !contradiction < 0 do
    if !primitive land 1023 = 0 then Cancel.check_opt cancel;
    if Bytes.get orientation !primitive = '\255' then begin
      if not (selected !primitive)
          || Topology.primitive_kind source_topology !primitive <> Topology.Polygon
      then Bytes.set orientation !primitive '\000'
      else begin
        queue_first := 0; queue_last := 0; assign !primitive false;
        while !queue_first < !queue_last && !invalid_edge < 0
            && !contradiction < 0 do
          let current = queue.(!queue_first) in
          incr queue_first;
          let current_flipped = Bytes.get orientation current = '\001' in
          let first = topology.primitive_offsets.(current)
          and last = topology.primitive_offsets.(current + 1) in
          for vertex = first to last - 1 do
            let edge = view.edge_of_vertex.(vertex) in
            if edge >= 0 && !invalid_edge < 0 && !contradiction < 0 then begin
              let edge_first = view.edge_offsets.(edge)
              and edge_last = view.edge_offsets.(edge + 1) in
              let incidence = ref 0 and other_vertex = ref (-1) in
              for at = edge_first to edge_last - 1 do
                let candidate_vertex = view.edge_vertices.(at) in
                let candidate = view.primitive_of_vertex.(candidate_vertex) in
                if selected candidate then begin
                  incr incidence;
                  if candidate <> current then other_vertex := candidate_vertex
                end
              done;
              if !incidence > 2 then invalid_edge := edge
              else if !incidence = 2 && !other_vertex >= 0 then begin
                let other_vertex = !other_vertex in
                let other = view.primitive_of_vertex.(other_vertex) in
                if other <> current
                    && Topology.primitive_kind source_topology other
                       = Topology.Polygon then begin
                  let same_direction =
                    topology.vertex_points.(vertex)
                    = topology.vertex_points.(other_vertex) in
                  let other_flipped = current_flipped <> same_direction in
                  let expected = if other_flipped then '\001' else '\000' in
                  match Bytes.get orientation other with
                  | '\255' -> assign other other_flipped
                  | assigned when assigned <> expected -> contradiction := edge
                  | _ -> ()
                end
              end
            end
          done
        done
      end
    end;
    incr primitive
  done;
  if !invalid_edge >= 0 then Error (Printf.sprintf
      "Facet Orient Polygons requires manifold edges; edge %d has more than two incident primitives"
      !invalid_edge)
  else if !contradiction >= 0 then Error (Printf.sprintf
      "Facet Orient Polygons found a non-orientable constraint at edge %d"
      !contradiction)
  else begin
    for primitive = 0 to primitive_count - 1 do
      if Bytes.get orientation primitive = '\001' then changed := true
    done;
    if not !changed then Ok geometry
    else begin
      let vertex_count = Array.length topology.vertex_points in
      let vertex_points = Array.make vertex_count 0
      and vertex_map = Array.make vertex_count 0 in
      if primitive_count > 0 then Parallel.for_
          ~chunk_size:(max 1 (grain / 8)) ~start:0 ~finish:(primitive_count - 1)
          (fun primitive ->
        if primitive land 1023 = 0 then Cancel.check_opt cancel;
        let first = topology.primitive_offsets.(primitive)
        and last = topology.primitive_offsets.(primitive + 1) in
        if Bytes.get orientation primitive = '\001' then
          for vertex = first to last - 1 do
            let source = first + last - vertex - 1 in
            vertex_points.(vertex) <- topology.vertex_points.(source);
            vertex_map.(vertex) <- source
          done
        else
          for vertex = first to last - 1 do
            vertex_points.(vertex) <- topology.vertex_points.(vertex);
            vertex_map.(vertex) <- vertex
          done);
      let target_topology = Topology.Private.create_validated_owned
          ~point_count:topology.point_count ~vertex_points
          ~primitive_offsets:(Array.copy topology.primitive_offsets)
          ~primitive_kinds:(Bytes.copy topology.primitive_kinds) in
      let rec attributes result = function
        | [] -> Ok (List.rev result)
        | attribute :: rest ->
            Result.bind
              (remap_vertex_attribute ?cancel ~grain vertex_map attribute)
              (fun attribute -> attributes (attribute :: result) rest) in
      Result.bind (attributes [] (Geometry.attributes geometry))
        (fun attributes ->
      let groups = List.map (remap_vertex_group ?cancel ~grain vertex_map)
          (Geometry.groups geometry) in
      let edge_groups = match Geometry.edge_groups geometry with
        | [] -> Ok []
        | source_groups ->
            let target_index = Topology_index.create ?cancel target_topology in
            let point_map = Array.init topology.point_count Fun.id in
            let rec loop result = function
              | [] -> Ok (List.rev result)
              | group :: rest ->
                  Result.bind (Edge_group.remap ?cancel ~source_index:index
                      ~target_topology ~target_index ~point_map group)
                    (fun group -> loop (group :: result) rest) in
            loop [] source_groups in
      Result.bind edge_groups
        (fun edge_groups ->
      Geometry.create ~positions:(Geometry.positions geometry)
        ~topology:target_topology ~attributes ~groups ~edge_groups ()))
    end
  end

let split_points_on_edge_ends ?cancel ~grain ?primitives ~index ~split_ends
    geometry =
  let source_topology = Geometry.topology geometry in
  let topology = Topology.Private.view source_topology
  and view = Topology_index.Private.view index in
  let vertex_count = Array.length topology.vertex_points
  and edge_count = Array.length view.edge_a in
  let selected primitive = match primitives with
    | None -> true | Some group -> Group.mem primitive group in
  let parent = Array.init vertex_count Fun.id
  and rank = Bytes.make vertex_count '\000' in
  let root vertex =
    let representative = ref vertex in
    while parent.(!representative) <> !representative do
      representative := parent.(!representative)
    done;
    let representative = !representative and current = ref vertex in
    while parent.(!current) <> representative do
      let next = parent.(!current) in
      parent.(!current) <- representative;
      current := next
    done;
    representative in
  let union left right =
    let left = root left and right = root right in
    if left <> right then begin
      let left_rank = Char.code (Bytes.get rank left)
      and right_rank = Char.code (Bytes.get rank right) in
      if left_rank < right_rank then parent.(left) <- right
      else if right_rank < left_rank then parent.(right) <- left
      else begin
        let representative, child =
          if left < right then left, right else right, left in
        parent.(child) <- representative;
        Bytes.set rank representative (Char.chr (left_rank + 1))
      end
    end in
  let affected = Bytes.make topology.point_count '\000' in
  for edge = 0 to edge_count - 1 do
    let flags = Char.code (Bytes.get split_ends edge) in
    if flags land 1 <> 0 then Bytes.set affected view.edge_a.(edge) '\001';
    if flags land 2 <> 0 then Bytes.set affected view.edge_b.(edge) '\001'
  done;
  for point = 0 to topology.point_count - 1 do
    if point land 4095 = 0 then Cancel.check_opt cancel;
    let first = view.point_offsets.(point)
    and last = view.point_offsets.(point + 1) in
    let representative = ref (-1) in
    for at = first to last - 1 do
      let vertex = view.point_vertices.(at) in
      if Bytes.get affected point = '\000'
          || not (selected view.primitive_of_vertex.(vertex)) then
        if !representative < 0 then representative := vertex
        else union !representative vertex
    done
  done;
  for edge = 0 to edge_count - 1 do
    if edge land 4095 = 0 then Cancel.check_opt cancel;
    let flags = Char.code (Bytes.get split_ends edge)
    and first = view.edge_offsets.(edge)
    and last = view.edge_offsets.(edge + 1) in
    if last > first then begin
      let a = view.edge_a.(edge) and b = view.edge_b.(edge)
      and representative_a = ref (-1) and representative_b = ref (-1) in
      for at = first to last - 1 do
        let vertex = view.edge_vertices.(at) in
        let vertex_a = if topology.vertex_points.(vertex) = a then vertex
          else view.next_vertex.(vertex)
        and vertex_b = if topology.vertex_points.(vertex) = b then vertex
          else view.next_vertex.(vertex) in
        if flags land 1 = 0 then
          if !representative_a < 0 then representative_a := vertex_a
          else union !representative_a vertex_a;
        if flags land 2 = 0 then
          if !representative_b < 0 then representative_b := vertex_b
          else union !representative_b vertex_b
      done
    end
  done;
  let source_points = topology.point_count in
  let root_output = Array.make vertex_count (-1) in
  let extra_count = ref 0 and overflow = ref false in
  for point = 0 to source_points - 1 do
    if point land 4095 = 0 then Cancel.check_opt cancel;
    let first = view.point_offsets.(point)
    and last = view.point_offsets.(point + 1) in
    let component = ref 0 in
    (match primitives with
     | None -> ()
     | Some _ ->
         let at = ref first in
         while !at < last && !component = 0 do
           let vertex = view.point_vertices.(!at) in
           if not (selected view.primitive_of_vertex.(vertex)) then begin
             let representative = root vertex in
             root_output.(representative) <- point;
             component := 1
           end;
           incr at
         done);
    for local = first to last - 1 do
      let representative = root view.point_vertices.(local) in
      if root_output.(representative) < 0 then begin
        if !component = 0 then root_output.(representative) <- point
        else if !extra_count >= max_int - source_points then overflow := true
        else begin
          root_output.(representative) <- source_points + !extra_count;
          incr extra_count
        end;
        incr component
      end
    done
  done;
  if !overflow then Error "Facet point split exceeds integer cardinality limits"
  else if !extra_count = 0 then Ok geometry
  else begin
    let output_points = source_points + !extra_count in
    let point_map = Array.init output_points (fun point ->
        if point < source_points then point else 0) in
    for representative = 0 to vertex_count - 1 do
      let output = root_output.(representative) in
      if output >= source_points then
        point_map.(output) <- topology.vertex_points.(representative)
    done;
    let vertex_points = Array.make vertex_count 0 in
    if vertex_count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
        ~finish:(vertex_count - 1) (fun vertex ->
      if vertex land 4095 = 0 then Cancel.check_opt cancel;
      vertex_points.(vertex) <- root_output.(root vertex));
    let target_topology = Topology.Private.create_validated_owned
        ~point_count:output_points ~vertex_points
        ~primitive_offsets:(Array.copy topology.primitive_offsets)
        ~primitive_kinds:(Bytes.copy topology.primitive_kinds) in
    let source_positions = Packed.Float3.Private.view
        (Geometry.positions geometry) in
    let positions = Packed.Float3.Private.of_owned_exn
        ~x:(Topology_remap.select_float ?cancel ~grain point_map source_positions.x)
        ~y:(Topology_remap.select_float ?cancel ~grain point_map source_positions.y)
        ~z:(Topology_remap.select_float ?cancel ~grain point_map source_positions.z) in
    let rec attributes result = function
      | [] -> Ok (List.rev result)
      | attribute :: rest ->
          Result.bind (remap_point_attribute ?cancel ~grain point_map attribute)
            (fun attribute -> attributes (attribute :: result) rest) in
    Result.bind (attributes [] (Geometry.attributes geometry)) (fun attributes ->
      let groups = List.map (remap_point_group ?cancel ~grain point_map)
          (Geometry.groups geometry) in
      let edge_groups = remap_edge_groups_from_index ?cancel ~grain
          ~source_index:index ~target_topology (Geometry.edge_groups geometry) in
      Geometry.create ~positions ~topology:target_topology ~attributes
        ~groups ~edge_groups ())
  end

let cusp_polygons ?cancel ?(grain = 16_384) ?primitives ~angle geometry =
  if grain <= 0 then invalid_arg "Rdk_mesh.Facet.facet: grain must be positive";
  if not (Float.is_finite angle) || angle < 0. || angle > Float.pi then
    Error "Facet cusp angle must be finite and within [0, pi]"
  else
    let source_topology = Geometry.topology geometry in
    let topology = Topology.Private.view source_topology in
    let primitive_count = Array.length topology.primitive_offsets - 1 in
    let selection_error = match primitives with
      | Some group when Group.owner group <> Group.Primitive ->
          Some "Facet selection must own primitives"
      | Some group when Group.length group <> primitive_count ->
          Some "Facet selection length does not match primitive count"
      | _ -> None in
    match selection_error with
    | Some message -> Error message
    | None ->
    let selected primitive = match primitives with
      | None -> true
      | Some group -> Group.mem primitive group in
    let non_polygon = ref (-1) in
    let primitive = ref 0 in
    while !non_polygon < 0 && !primitive < primitive_count do
      if !primitive land 1023 = 0 then Cancel.check_opt cancel;
      if selected !primitive
          && Topology.primitive_kind source_topology !primitive <> Topology.Polygon
      then non_polygon := !primitive;
      incr primitive
    done;
    if !non_polygon >= 0 then Error (Printf.sprintf
        "Facet Cusp Polygons requires polygon-only topology; primitive %d is not a polygon"
        !non_polygon)
    else
      let index = Topology_index.create ?cancel source_topology in
      let view = Topology_index.Private.view index in
      let normal_primitives = match primitives with
        | None -> None
        | Some _ -> Some (Group.init ~grain ~owner:Group.Primitive
            ~name:"__facet_cusp_normals" primitive_count (fun primitive ->
              if selected primitive then true
              else if Topology.primitive_kind source_topology primitive
                  <> Topology.Polygon then false
              else begin
                let first = topology.primitive_offsets.(primitive)
                and last = topology.primitive_offsets.(primitive + 1) in
                let found = ref false and vertex = ref first in
                while not !found && !vertex < last do
                  let edge = view.edge_of_vertex.(!vertex) in
                  if edge >= 0 then begin
                    let at = ref view.edge_offsets.(edge)
                    and finish = view.edge_offsets.(edge + 1) in
                    while not !found && !at < finish do
                      found := selected
                          view.primitive_of_vertex.(view.edge_vertices.(!at));
                      incr at
                    done
                  end;
                  incr vertex
                done;
                !found
              end)) in
      Result.bind (Face_normals.compute ?cancel ~grain ?primitives:normal_primitives
        ~operation:"Facet Cusp Polygons" geometry) (fun (nx, ny, nz) ->
      let edge_count = Array.length view.edge_a in
      let cusp = Bytes.make edge_count '\000' in
      let threshold = cos angle and epsilon = 64. *. Float.epsilon in
      if edge_count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
          ~finish:(edge_count - 1) (fun edge ->
        if edge land 4095 = 0 then Cancel.check_opt cancel;
        let first = view.edge_offsets.(edge)
        and last = view.edge_offsets.(edge + 1) in
        let touches_selection = ref false in
        for at = first to last - 1 do
          let primitive = view.primitive_of_vertex.(view.edge_vertices.(at)) in
          if selected primitive then touches_selection := true
        done;
        if !touches_selection && last - first > 2 then Bytes.set cusp edge '\002'
        else if !touches_selection && last - first = 2 then begin
          let left = view.primitive_of_vertex.(view.edge_vertices.(first))
          and right = view.primitive_of_vertex.(view.edge_vertices.(first + 1)) in
          if Topology.primitive_kind source_topology left = Topology.Polygon
              && Topology.primitive_kind source_topology right = Topology.Polygon
          then begin
            let dot = (nx.(left) *. nx.(right)) +. (ny.(left) *. ny.(right))
                +. (nz.(left) *. nz.(right)) in
            if dot < threshold -. epsilon then Bytes.set cusp edge '\001'
          end
        end);
      let invalid_edge = ref (-1) and edge = ref 0 in
      while !invalid_edge < 0 && !edge < edge_count do
        if Bytes.get cusp !edge = '\002' then invalid_edge := !edge;
        incr edge
      done;
      if !invalid_edge >= 0 then Error (Printf.sprintf
          "Facet Cusp Polygons requires manifold edges; edge %d has more than two incident primitives"
          !invalid_edge)
      else
        let split_ends = Bytes.map (fun value ->
          if value = '\001' then '\003' else '\000') cusp in
        split_points_on_edge_ends ?cancel ~grain ?primitives ~index ~split_ends
          geometry)

let[@inline always] planar_local values point origin scale relative =
  if relative then (values.(point) -. origin) /. scale
  else values.(point) /. scale

let[@inline always] planar_world value origin scale relative =
  if relative then origin +. (value *. scale) else value *. scale

let make_planar ?cancel ?(grain = 16_384) ?primitives geometry =
  if grain <= 0 then invalid_arg "Rdk_mesh.Facet.facet: grain must be positive";
  Cancel.check_opt cancel;
  let source_topology = Geometry.topology geometry in
  let topology = Topology.Private.view source_topology in
  let primitive_count = Topology.primitive_count source_topology in
  if match primitives with
    | Some group -> Group.owner group <> Group.Primitive
        || Group.length group <> primitive_count
    | None -> false
  then Error "Facet selection must be a matching primitive group"
  else begin
    let selected primitive = match primitives with
      | None -> true | Some group -> Group.mem primitive group in
    let source_positions = Packed.Float3.Private.view
        (Geometry.positions geometry) in
    let vertex_count = Array.length topology.vertex_points in
    let changed = Bytes.make primitive_count '\000'
    and failures = Bytes.make primitive_count '\000'
    and projected_x = Array.make vertex_count 0.
    and projected_y = Array.make vertex_count 0.
    and projected_z = Array.make vertex_count 0. in
    if primitive_count > 0 then Parallel.for_
        ~chunk_size:(max 1 (grain / 8)) ~start:0
        ~finish:(primitive_count - 1) (fun primitive ->
      if primitive land 1023 = 0 then Cancel.check_opt cancel;
      let first = topology.primitive_offsets.(primitive)
      and last = topology.primitive_offsets.(primitive + 1) in
      let count = last - first in
      if selected primitive && Topology.primitive_kind source_topology primitive
          = Topology.Polygon && count > 3 then begin
        let first_point = topology.vertex_points.(first) in
        let ox = source_positions.x.(first_point)
        and oy = source_positions.y.(first_point)
        and oz = source_positions.z.(first_point) in
        if not (Float.is_finite ox && Float.is_finite oy && Float.is_finite oz)
        then Bytes.unsafe_set failures primitive '\001'
        else begin
          let scale = ref 0. and relative = ref true in
          for vertex = first + 1 to last - 1 do
            let point = topology.vertex_points.(vertex) in
            let px = source_positions.x.(point)
            and py = source_positions.y.(point)
            and pz = source_positions.z.(point) in
            if not (Float.is_finite px && Float.is_finite py
                && Float.is_finite pz) then
              Bytes.unsafe_set failures primitive '\001'
            else begin
              let dx = px -. ox and dy = py -. oy and dz = pz -. oz in
              if not (Float.is_finite dx && Float.is_finite dy
                  && Float.is_finite dz) then relative := false
              else begin
                let local_scale = max_abs3 dx dy dz in
                if local_scale > !scale then scale := local_scale
              end
            end
          done;
          if Bytes.unsafe_get failures primitive = '\000' then begin
            if not !relative then begin
              scale := max_abs3 ox oy oz;
              for vertex = first + 1 to last - 1 do
                let point = topology.vertex_points.(vertex) in
                let value = max_abs3 source_positions.x.(point)
                    source_positions.y.(point) source_positions.z.(point) in
                if value > !scale then scale := value
              done
            end;
            if !scale = 0. then Bytes.unsafe_set failures primitive '\002'
            else begin
              let cx = ref 0. and cy = ref 0. and cz = ref 0. in
              for vertex = first to last - 1 do
                let point = topology.vertex_points.(vertex) in
                cx := !cx +. planar_local source_positions.x point ox !scale
                    !relative;
                cy := !cy +. planar_local source_positions.y point oy !scale
                    !relative;
                cz := !cz +. planar_local source_positions.z point oz !scale
                    !relative
              done;
              let divisor = float_of_int count in
              cx := !cx /. divisor; cy := !cy /. divisor; cz := !cz /. divisor;
              let nx = ref 0. and ny = ref 0. and nz = ref 0. in
              for vertex = first to last - 1 do
                let next = if vertex + 1 < last then vertex + 1 else first in
                let point = topology.vertex_points.(vertex)
                and next_point = topology.vertex_points.(next) in
                let ax = planar_local source_positions.x point ox !scale !relative
                and ay = planar_local source_positions.y point oy !scale !relative
                and az = planar_local source_positions.z point oz !scale !relative
                and bx = planar_local source_positions.x next_point ox !scale
                    !relative
                and by = planar_local source_positions.y next_point oy !scale
                    !relative
                and bz = planar_local source_positions.z next_point oz !scale
                    !relative in
                nx := !nx +. ((ay -. by) *. (az +. bz));
                ny := !ny +. ((az -. bz) *. (ax +. bx));
                nz := !nz +. ((ax -. bx) *. (ay +. by))
              done;
              let normal_scale = max_abs3 !nx !ny !nz in
              if normal_scale = 0. || not (Float.is_finite !nx
                  && Float.is_finite !ny && Float.is_finite !nz) then
                Bytes.unsafe_set failures primitive '\002'
              else begin
                let nx = !nx /. normal_scale and ny = !ny /. normal_scale
                and nz = !nz /. normal_scale in
                let inverse = 1. /. sqrt
                    ((nx *. nx) +. (ny *. ny) +. (nz *. nz)) in
                let nx = nx *. inverse and ny = ny *. inverse
                and nz = nz *. inverse in
                let maximum_distance = ref 0. in
                for vertex = first to last - 1 do
                  let point = topology.vertex_points.(vertex) in
                  let x = planar_local source_positions.x point ox !scale !relative
                  and y = planar_local source_positions.y point oy !scale !relative
                  and z = planar_local source_positions.z point oz !scale !relative in
                  let distance = ((x -. !cx) *. nx) +. ((y -. !cy) *. ny)
                      +. ((z -. !cz) *. nz) in
                  let absolute = abs_float distance in
                  if absolute > !maximum_distance then
                    maximum_distance := absolute;
                  let x = planar_world (x -. (distance *. nx)) ox !scale !relative
                  and y = planar_world (y -. (distance *. ny)) oy !scale !relative
                  and z = planar_world (z -. (distance *. nz)) oz !scale !relative in
                  if not (Float.is_finite x && Float.is_finite y
                      && Float.is_finite z) then
                    Bytes.unsafe_set failures primitive '\003'
                  else begin
                    projected_x.(vertex) <- x;
                    projected_y.(vertex) <- y;
                    projected_z.(vertex) <- z
                  end
                done;
                if Bytes.unsafe_get failures primitive = '\000'
                    && !maximum_distance > 512. *. Float.epsilon then
                  Bytes.unsafe_set changed primitive '\001'
              end
            end
          end
        end
      end);
    let failed = ref (-1) and primitive = ref 0 in
    while !failed < 0 && !primitive < primitive_count do
      if Bytes.unsafe_get failures !primitive <> '\000' then failed := !primitive;
      incr primitive
    done;
    if !failed >= 0 then
      let reason = match Bytes.unsafe_get failures !failed with
        | '\001' -> "contains a non-finite position"
        | '\002' -> "has no stable plane"
        | _ -> "projection is outside finite coordinate range" in
      Error (Printf.sprintf "Facet Make Planar primitive %d %s" !failed reason)
    else if not (Bytes.exists ((<>) '\000') changed) then Ok geometry
    else begin
      let unchanged_reference = Bytes.make topology.point_count '\000' in
      for primitive = 0 to primitive_count - 1 do
        if Bytes.unsafe_get changed primitive = '\000' then
          for vertex = topology.primitive_offsets.(primitive)
              to topology.primitive_offsets.(primitive + 1) - 1 do
            Bytes.unsafe_set unchanged_reference
              topology.vertex_points.(vertex) '\001'
          done
      done;
      let claimed = Bytes.make topology.point_count '\000'
      and extras = ref 0 and overflow = ref false in
      for primitive = 0 to primitive_count - 1 do
        if Bytes.unsafe_get changed primitive <> '\000' then
          for vertex = topology.primitive_offsets.(primitive)
              to topology.primitive_offsets.(primitive + 1) - 1 do
            let point = topology.vertex_points.(vertex) in
            if Bytes.unsafe_get unchanged_reference point <> '\000'
                || Bytes.unsafe_get claimed point <> '\000' then
              if !extras >= max_int - topology.point_count then overflow := true
              else incr extras
            else Bytes.unsafe_set claimed point '\001'
          done
      done;
      if !overflow then Error
          "Facet Make Planar output exceeds integer cardinality limits"
      else begin
        let output_points = topology.point_count + !extras in
        let point_map = Array.init output_points (fun point ->
            if point < topology.point_count then point else 0) in
        let vertex_points = Array.copy topology.vertex_points
        and x = Array.make output_points 0. and y = Array.make output_points 0.
        and z = Array.make output_points 0. in
        Array.blit source_positions.x 0 x 0 topology.point_count;
        Array.blit source_positions.y 0 y 0 topology.point_count;
        Array.blit source_positions.z 0 z 0 topology.point_count;
        Bytes.fill claimed 0 topology.point_count '\000';
        let extra = ref topology.point_count in
        for primitive = 0 to primitive_count - 1 do
          if primitive land 1023 = 0 then Cancel.check_opt cancel;
          if Bytes.unsafe_get changed primitive <> '\000' then
            for vertex = topology.primitive_offsets.(primitive)
                to topology.primitive_offsets.(primitive + 1) - 1 do
              let source_point = topology.vertex_points.(vertex) in
              let target = if Bytes.unsafe_get unchanged_reference source_point
                    = '\000' && Bytes.unsafe_get claimed source_point = '\000'
                then begin
                  Bytes.unsafe_set claimed source_point '\001';
                  source_point
                end else begin
                  let target = !extra in
                  incr extra;
                  point_map.(target) <- source_point;
                  target
                end in
              vertex_points.(vertex) <- target;
              x.(target) <- projected_x.(vertex);
              y.(target) <- projected_y.(vertex);
              z.(target) <- projected_z.(vertex)
            done
        done;
        let positions = Packed.Float3.Private.of_owned_exn ~x ~y ~z in
        if !extras = 0 then Geometry.with_positions positions geometry
        else begin
          let target_topology = Topology.Private.create_validated_owned
              ~point_count:output_points ~vertex_points
              ~primitive_offsets:(Array.copy topology.primitive_offsets)
              ~primitive_kinds:(Bytes.copy topology.primitive_kinds) in
          let rec attributes result = function
            | [] -> Ok (List.rev result)
            | attribute :: rest ->
                Result.bind
                  (remap_point_attribute ?cancel ~grain point_map attribute)
                  (fun attribute -> attributes (attribute :: result) rest) in
          Result.bind (attributes [] (Geometry.attributes geometry))
            (fun attributes ->
          let groups = List.map (remap_point_group ?cancel ~grain point_map)
              (Geometry.groups geometry) in
          let edge_groups = remap_edge_groups ?cancel ~grain ~source_topology
              ~target_topology (Geometry.edge_groups geometry) in
          Geometry.create ~positions ~topology:target_topology ~attributes
            ~groups ~edge_groups ())
        end
      end
    end
  end

let validate_normal_view ?cancel ~grain owner values =
  let values = Packed.Float3.Private.view values in
  let count = Array.length values.x in
  let ranges = if count = 0 then 0 else (count + grain - 1) / grain in
  let failures = Array.make ranges (-1) in
  if ranges > 0 then Parallel.for_ ~chunk_size:1 ~start:0
      ~finish:(ranges - 1) (fun range ->
    let first = range * grain and last = min count ((range + 1) * grain) in
    let element = ref first in
    while failures.(range) < 0 && !element < last do
      if !element land 4095 = 0 then Cancel.check_opt cancel;
      if not (Float.is_finite values.x.(!element)
          && Float.is_finite values.y.(!element)
          && Float.is_finite values.z.(!element)) then
        failures.(range) <- !element;
      incr element
    done);
  let failure = ref (-1) in
  for range = ranges - 1 downto 0 do
    if failures.(range) >= 0 then failure := failures.(range)
  done;
  if !failure >= 0 then Error (Printf.sprintf
      "Facet Consolidate Normals %s N contains a non-finite vector at element %d"
      (if owner = Attribute.Point then "point" else "vertex") !failure)
  else Ok values

let[@inline always] scaled_average sum divisor scale =
  if scale = 0. then 0.
  else
    let value = sum /. divisor in
    let value = if value > 1. then 1. else if value < -1. then -1. else value in
    value *. scale

let consolidate_point_normal ?cancel ~grain clusters
    (source : Packed.Float3.Private.view) =
  let x = Array.copy source.x and y = Array.copy source.y
  and z = Array.copy source.z in
  if clusters.Point_clusters.count > 0 then Parallel.for_
      ~chunk_size:(max 1 (grain / 8)) ~start:0
      ~finish:(clusters.count - 1) (fun cluster ->
    if cluster land 1023 = 0 then Cancel.check_opt cancel;
    let first = clusters.offsets.(cluster)
    and last = clusters.offsets.(cluster + 1) in
    if last - first > 1 then begin
      let scale = ref 0. in
      for slot = first to last - 1 do
        let point = clusters.members.(slot) in
        let value = max_abs3 source.x.(point) source.y.(point)
            source.z.(point) in
        if value > !scale then scale := value
      done;
      let ax = ref 0. and ay = ref 0. and az = ref 0. in
      if !scale <> 0. then
        for slot = first to last - 1 do
          let point = clusters.members.(slot) in
          ax := !ax +. (source.x.(point) /. !scale);
          ay := !ay +. (source.y.(point) /. !scale);
          az := !az +. (source.z.(point) /. !scale)
        done;
      let divisor = float_of_int (last - first) in
      let nx = scaled_average !ax divisor !scale
      and ny = scaled_average !ay divisor !scale
      and nz = scaled_average !az divisor !scale in
      for slot = first to last - 1 do
        let point = clusters.members.(slot) in
        x.(point) <- nx; y.(point) <- ny; z.(point) <- nz
      done
    end);
  Packed.Float3.Private.of_owned_exn ~x ~y ~z

let consolidate_vertex_normal ?cancel ~grain ?primitives clusters incidence
    (source : Packed.Float3.Private.view) =
  let x = Array.copy source.x and y = Array.copy source.y
  and z = Array.copy source.z in
  if clusters.Point_clusters.count > 0 then Parallel.for_
      ~chunk_size:(max 1 (grain / 8)) ~start:0
      ~finish:(clusters.count - 1) (fun cluster ->
    if cluster land 1023 = 0 then Cancel.check_opt cancel;
    let first = clusters.offsets.(cluster)
    and last = clusters.offsets.(cluster + 1) in
    if last - first > 1 then begin
      let scale = ref 0. and elements = ref 0 in
      for slot = first to last - 1 do
        let point = clusters.members.(slot) in
        for at = incidence.Topology_index.Private.point_offsets.(point)
            to incidence.point_offsets.(point + 1) - 1 do
          let vertex = incidence.point_vertices.(at) in
          if match primitives with None -> true | Some group ->
              Group.mem incidence.primitive_of_vertex.(vertex) group
          then begin
            let value = max_abs3 source.x.(vertex) source.y.(vertex)
                source.z.(vertex) in
            if value > !scale then scale := value;
            incr elements
          end
        done
      done;
      if !elements > 0 then begin
      let ax = ref 0. and ay = ref 0. and az = ref 0. in
      if !scale <> 0. then
        for slot = first to last - 1 do
          let point = clusters.members.(slot) in
          for at = incidence.point_offsets.(point)
              to incidence.point_offsets.(point + 1) - 1 do
            let vertex = incidence.point_vertices.(at) in
            if match primitives with None -> true | Some group ->
                Group.mem incidence.primitive_of_vertex.(vertex) group
            then begin
              ax := !ax +. (source.x.(vertex) /. !scale);
              ay := !ay +. (source.y.(vertex) /. !scale);
              az := !az +. (source.z.(vertex) /. !scale)
            end
          done
        done;
      let divisor = float_of_int !elements in
      let nx = scaled_average !ax divisor !scale
      and ny = scaled_average !ay divisor !scale
      and nz = scaled_average !az divisor !scale in
      for slot = first to last - 1 do
        let point = clusters.members.(slot) in
        for at = incidence.point_offsets.(point)
            to incidence.point_offsets.(point + 1) - 1 do
          let vertex = incidence.point_vertices.(at) in
          if match primitives with None -> true | Some group ->
              Group.mem incidence.primitive_of_vertex.(vertex) group
          then begin x.(vertex) <- nx; y.(vertex) <- ny; z.(vertex) <- nz end
        done
      done
      end
    end);
  Packed.Float3.Private.of_owned_exn ~x ~y ~z

let consolidate_normals ?cancel ?(grain = 16_384) ?primitives ~distance geometry =
  if grain <= 0 then invalid_arg "Rdk_mesh.Facet.facet: grain must be positive";
  if not (Float.is_finite distance) || distance < 0. then
    Error "Facet normal consolidation distance must be finite and non-negative"
  else begin
    Cancel.check_opt cancel;
    let topology = Geometry.topology geometry in
    let primitive_count = Topology.primitive_count topology in
    let selection_error = match primitives with
      | Some group when Group.owner group <> Group.Primitive ->
          Some "Facet selection must own primitives"
      | Some group when Group.length group <> primitive_count ->
          Some "Facet selection length does not match primitive count"
      | _ -> None in
    match selection_error with
    | Some message -> Error message
    | None ->
    let point_selection = Option.map (fun primitives ->
      let index = Topology_index.create ?cancel topology in
      Group.init ~grain ~owner:Group.Point ~name:"__facet_points"
        (Topology.point_count topology) (fun point ->
          let local = ref 0 and found = ref false in
          let count = Topology_index.point_incidence_count index point in
          while not !found && !local < count do
            let vertex = Topology_index.point_vertex index ~point ~local:!local in
            found := Group.mem (Topology_index.primitive_of_vertex index vertex)
                primitives;
            incr local
          done;
          !found)) primitives in
    let normals = Geometry.attributes geometry |> List.filter (fun attribute ->
        String.equal (Attribute.name attribute) "N"
        && (Attribute.owner attribute = Attribute.Point
            || Attribute.owner attribute = Attribute.Vertex)) in
    if normals = [] then Ok geometry
    else begin
      let rec validate validated = function
        | [] -> Ok (List.rev validated)
        | attribute :: rest ->
            (match Attribute.Private.storage attribute with
             | Attribute.Float3 values ->
                 Result.bind (validate_normal_view ?cancel ~grain
                     (Attribute.owner attribute) values) (fun view ->
                   validate ((Attribute.owner attribute, view) :: validated) rest)
             | _ -> Error (Printf.sprintf
                 "Facet Consolidate Normals %s N must have float3 storage"
                 (if Attribute.owner attribute = Attribute.Point
                  then "point" else "vertex"))) in
      Result.bind (validate [] normals) (fun validated ->
      Result.bind (Point_clusters.create ?cancel ?selection:point_selection
          ~operation:"Facet Consolidate Normals" ~tolerance:distance
          ~compatible:(fun _ _ -> true) geometry) (function
        | Point_clusters.Identity -> Ok geometry
        | Point_clusters.Clusters clusters ->
            let vertex_needed = List.exists
                (fun (owner, _) -> owner = Attribute.Vertex) validated in
            let incidence = if vertex_needed then Some
                (Topology_index.create ?cancel (Geometry.topology geometry)
                  |> Topology_index.Private.view)
              else None in
            let rec attributes result = function
              | [] -> Ok (List.rev result)
              | attribute :: rest ->
                  if not (String.equal (Attribute.name attribute) "N"
                      && (Attribute.owner attribute = Attribute.Point
                          || Attribute.owner attribute = Attribute.Vertex)) then
                    attributes (attribute :: result) rest
                  else
                    let source = match Attribute.Private.storage attribute with
                      | Attribute.Float3 values -> Packed.Float3.Private.view values
                      | _ -> assert false in
                    let values = if Attribute.owner attribute = Attribute.Point
                      then consolidate_point_normal ?cancel ~grain clusters source
                      else consolidate_vertex_normal ?cancel ~grain ?primitives clusters
                        (Option.get incidence) source in
                    Result.bind (Attribute.create_owned ~name:"N"
                        ~owner:(Attribute.owner attribute)
                        (Attribute.Float3 values)) (fun attribute ->
                      attributes (attribute :: result) rest) in
            Result.bind (attributes [] (Geometry.attributes geometry))
              (fun attributes ->
            Geometry.create ~positions:(Geometry.positions geometry)
              ~topology:(Geometry.topology geometry) ~attributes
              ~groups:(Geometry.groups geometry)
              ~edge_groups:(Geometry.edge_groups geometry) ())))
    end
  end

let adjust_normal_attribute ?cancel ~grain ?selected ~unit_length ~reverse attribute =
  let owner = Attribute.owner attribute in
  if not (String.equal (Attribute.name attribute) "N"
      && (owner = Attribute.Point || owner = Attribute.Vertex)) then
    Ok attribute
  else match Attribute.Private.storage attribute with
    | Attribute.Float3 values ->
        let values = Packed.Float3.Private.view values in
        let count = Array.length values.x in
        let x = Array.copy values.x and y = Array.copy values.y
        and z = Array.copy values.z in
        let ranges = if count = 0 then 0 else (count + grain - 1) / grain in
        let failures = Bytes.make ranges '\000' in
        if ranges > 0 then Parallel.for_ ~chunk_size:1 ~start:0
            ~finish:(ranges - 1) (fun range ->
          let first = range * grain and last = min count ((range + 1) * grain) in
          for element = first to last - 1 do
            if element land 4095 = 0 then Cancel.check_opt cancel;
            if match selected with None -> true | Some predicate -> predicate element
            then begin
              let vx = x.(element) and vy = y.(element) and vz = z.(element) in
              if not (Float.is_finite vx && Float.is_finite vy
                  && Float.is_finite vz) then Bytes.set failures range '\001'
              else begin
              let sign = if reverse then -1. else 1. in
              if not unit_length then begin
                x.(element) <- sign *. vx;
                y.(element) <- sign *. vy;
                z.(element) <- sign *. vz
              end else begin
                let scale = max_abs3 vx vy vz in
                if scale = 0. then begin
                  x.(element) <- 0.; y.(element) <- 0.; z.(element) <- 0.
                end else begin
                  let sx = vx /. scale and sy = vy /. scale
                  and sz = vz /. scale in
                  let length = sqrt
                      ((sx *. sx) +. (sy *. sy) +. (sz *. sz)) in
                  x.(element) <- sign *. sx /. length;
                  y.(element) <- sign *. sy /. length;
                  z.(element) <- sign *. sz /. length
                end
              end
              end
            end
          done);
        if Bytes.exists ((<>) '\000') failures then Error (Printf.sprintf
            "%s normal attribute contains a non-finite vector"
            (if owner = Attribute.Point then "point" else "vertex"))
        else Attribute.create_owned ~name:"N" ~owner
            (Attribute.Float3 (Packed.Float3.Private.of_owned_exn ~x ~y ~z))
    | _ -> Error (Printf.sprintf "%s normal attribute must have float3 storage"
        (if owner = Attribute.Point then "point" else "vertex"))

let adjust_normals ?cancel ?(grain = 16_384) ?primitives ~unit_length ~reverse
    geometry =
  if grain <= 0 then invalid_arg "Rdk_mesh.Facet.facet: grain must be positive";
  if not unit_length && not reverse then Ok geometry
  else begin
    let topology = Geometry.topology geometry in
    let selection_error = match primitives with
      | Some group when Group.owner group <> Group.Primitive ->
          Some "Facet selection must own primitives"
      | Some group when Group.length group <> Topology.primitive_count topology ->
          Some "Facet selection length does not match primitive count"
      | _ -> None in
    match selection_error with
    | Some message -> Error message
    | None ->
    let selected_points, selected_vertices = match primitives with
      | None -> None, None
      | Some group ->
          let view = Topology.Private.view topology in
          let points = Bytes.make view.point_count '\000'
          and vertices = Bytes.make (Array.length view.vertex_points) '\000' in
          for primitive = 0 to Topology.primitive_count topology - 1 do
            if primitive land 1023 = 0 then Cancel.check_opt cancel;
            if Group.mem primitive group then
              for vertex = view.primitive_offsets.(primitive)
                  to view.primitive_offsets.(primitive + 1) - 1 do
                Bytes.unsafe_set vertices vertex '\001';
                Bytes.unsafe_set points view.vertex_points.(vertex) '\001'
              done
          done;
          Some points, Some vertices in
    let selected owner =
      let mask = if owner = Attribute.Vertex then selected_vertices
        else selected_points in
      Option.map (fun mask element ->
        Bytes.unsafe_get mask element <> '\000') mask in
    let rec loop changed result = function
      | [] -> if not changed then Ok geometry else
          Geometry.Private.with_attributes_owned (Array.of_list (List.rev result))
            geometry
      | attribute :: rest ->
          Result.bind (adjust_normal_attribute ?cancel ~grain
              ?selected:(selected (Attribute.owner attribute)) ~unit_length
              ~reverse attribute) (fun adjusted ->
            loop (changed || adjusted != attribute) (adjusted :: result) rest) in
    loop false [] (Geometry.attributes geometry)
  end

(* The Facet SOP: composes the kernels above over an optional selection. *)

type deform_selection = Deform.selection =
  | Selected_points of Group.t
  | Selected_vertices of Group.t
  | Selected_primitives of Group.t
  | Selected_edges of Edge_group.t

let facet_point_selection ?cancel ~grain primitives geometry =
  let topology = Geometry.topology geometry in
  let index = Topology_index.create ?cancel topology in
  let view = Topology_index.Private.view index in
  Group.init ~grain ~owner:Group.Point ~name:"__facet_selected_points"
    (Topology.point_count topology) (fun point ->
      let found = ref false and at = ref view.point_offsets.(point) in
      let finish = view.point_offsets.(point + 1) in
      while not !found && !at < finish do
        found := Group.mem view.primitive_of_vertex.(view.point_vertices.(!at))
            primitives;
        incr at
      done;
      !found)

let facet_primitives_of_selection ?cancel ~grain selection geometry =
  let topology = Geometry.topology geometry in
  Cancel.check_opt cancel;
  Result.bind (Deform.validate_selection topology (Some selection)) (fun () ->
  match selection with
  | Selected_primitives group -> Ok group
  | Selected_points points ->
      let view = Topology.Private.view topology in
      Ok (Group.init ~grain ~owner:Group.Primitive
        ~name:"__facet_from_points" (Topology.primitive_count topology)
        (fun primitive ->
          if primitive land 4095 = 0 then Cancel.check_opt cancel;
          let found = ref false
          and vertex = ref view.primitive_offsets.(primitive) in
          let last = view.primitive_offsets.(primitive + 1) in
          while not !found && !vertex < last do
            found := Group.mem view.vertex_points.(!vertex) points;
            incr vertex
          done;
          !found))
  | Selected_vertices vertices ->
      let view = Topology.Private.view topology in
      Ok (Group.init ~grain ~owner:Group.Primitive
        ~name:"__facet_from_vertices" (Topology.primitive_count topology)
        (fun primitive ->
          if primitive land 4095 = 0 then Cancel.check_opt cancel;
          let found = ref false
          and vertex = ref view.primitive_offsets.(primitive) in
          let last = view.primitive_offsets.(primitive + 1) in
          while not !found && !vertex < last do
            found := Group.mem !vertex vertices;
            incr vertex
          done;
          !found))
  | Selected_edges edges ->
      let view = Topology.Private.view topology in
      let index = Topology_index.create ?cancel topology in
      let incidence = Topology_index.Private.view index in
      Ok (Group.init ~grain ~owner:Group.Primitive
        ~name:"__facet_from_edges" (Topology.primitive_count topology)
        (fun primitive ->
          if primitive land 4095 = 0 then Cancel.check_opt cancel;
          let found = ref false
          and vertex = ref view.primitive_offsets.(primitive) in
          let last = view.primitive_offsets.(primitive + 1) in
          while not !found && !vertex < last do
            let edge = incidence.edge_of_vertex.(!vertex) in
            found := edge >= 0 && Edge_group.mem edge edges;
            incr vertex
          done;
          !found)))

(* The kernels whose names [facet] reuses as option labels. *)
module Kernels = struct
  let unique_points = unique_points
  let remove_inline_points = remove_inline_points
  let orient_polygons = orient_polygons
  let make_planar = make_planar
end

let facet ?cancel ?(grain = 16_384) ?primitives
    ?(pre_compute_normals = false)
    ?(make_normals_unit_length = false) ?(unique_points = false)
    ?consolidate_distance ?consolidate_normals_distance
    ?(remove_inline_points = false)
    ?(inline_distance = 0.) ?(orient_polygons = false) ?cusp_angle
    ?(remove_degenerate = false) ?(make_planar = false)
    ?(post_compute_normals = false) ?(reverse_normals = false) geometry =
  if grain <= 0 then invalid_arg "Rdk_mesh.Facet.run: grain must be positive";
  let primitive_count = Geometry.primitive_count geometry in
  if match primitives with
    | Some group -> Group.owner group <> Group.Primitive
        || Group.length group <> primitive_count
    | None -> false
  then Error "Facet selection must be a matching primitive group"
  else if match consolidate_distance with Some value ->
      not (Float.is_finite value) || value < 0. | None -> false
  then Error "Facet consolidation distance must be finite and non-negative"
  else if match consolidate_normals_distance with Some value ->
      not (Float.is_finite value) || value < 0. | None -> false
  then Error "Facet normal consolidation distance must be finite and non-negative"
  else if consolidate_distance <> None && consolidate_normals_distance <> None
  then Error "Facet point and normal consolidation modes are mutually exclusive"
  else if remove_inline_points
      && (not (Float.is_finite inline_distance) || inline_distance < 0.)
  then Error "Facet inline distance must be finite and non-negative"
  else if match primitives with Some group -> Group.cardinality group = 0
      | None -> false then Ok geometry
  else
    let original_geometry = geometry in
    let selection_name, geometry = match primitives with
      | None -> None, geometry
      | Some group when Group.cardinality group = primitive_count ->
          None, geometry
      | Some group ->
          let rec available suffix =
            let name = Printf.sprintf "__rdk_facet_selection_%d"
                suffix in
            if Geometry.find_group ~owner:Group.Primitive name geometry = None
            then name else available (suffix + 1) in
          let name = available 0 in
          let private_group = Group.init ~grain ~owner:Group.Primitive ~name
              primitive_count (fun primitive -> Group.mem primitive group) in
          Some name, Geometry.with_group private_group geometry |> Result.get_ok in
    let selection_geometry = geometry in
    let current_selection geometry = match selection_name with
      | None -> None
      | Some name -> Geometry.find_group ~owner:Group.Primitive name geometry in
    let result = if pre_compute_normals then
        Deform.normals ?cancel ~grain
          ?primitives:(current_selection geometry) geometry
      else Ok geometry in
    let result = Result.bind result (fun geometry ->
      adjust_normals ?cancel ~grain
        ?primitives:(current_selection geometry)
        ~unit_length:(make_normals_unit_length && not pre_compute_normals)
        ~reverse:false geometry) in
    let result = Result.bind result (fun geometry ->
      if unique_points then Kernels.unique_points ?cancel ~grain
        ?primitives:(current_selection geometry) geometry
      else Ok geometry) in
    let result = Result.bind result (fun geometry ->
      match consolidate_distance, consolidate_normals_distance with
      | None, None -> Ok geometry
      | Some tolerance, None ->
          let selection = Option.map (fun primitives ->
            facet_point_selection ?cancel ~grain primitives geometry)
              (current_selection geometry) in
          Error.unguard (Fuse_grid.fuse ?cancel ~grain ?selection ~tolerance geometry)
      | None, Some distance ->
          consolidate_normals ?cancel ~grain
            ?primitives:(current_selection geometry) ~distance geometry
      | Some _, Some _ -> assert false) in
    let result = Result.bind result (fun geometry ->
      if remove_inline_points then
        Kernels.remove_inline_points ?cancel ~grain
          ?primitives:(current_selection geometry) ~distance:inline_distance geometry
      else Ok geometry) in
    let result = Result.bind result (fun geometry ->
      if orient_polygons then Kernels.orient_polygons ?cancel ~grain
          ?primitives:(current_selection geometry) geometry
      else Ok geometry) in
    let result = Result.bind result (fun geometry -> match cusp_angle with
      | None -> Ok geometry
      | Some angle -> cusp_polygons ?cancel ~grain
          ?primitives:(current_selection geometry) ~angle geometry) in
    let result = Result.bind result (fun geometry ->
      if remove_degenerate then
        Clean.delete_degenerate ?cancel ~grain
          ?primitives:(current_selection geometry) ~epsilon:1e-12 geometry
      else Ok geometry) in
    let result = Result.bind result (fun geometry ->
      if make_planar then Kernels.make_planar ?cancel ~grain
          ?primitives:(current_selection geometry) geometry
      else Ok geometry) in
    let result = Result.bind result (fun geometry ->
      if post_compute_normals then Deform.normals ?cancel ~grain
          ?primitives:(current_selection geometry) geometry
      else Ok geometry) in
    let result = Result.bind result (fun geometry ->
      adjust_normals ?cancel ~grain ~unit_length:false
        ?primitives:(current_selection geometry)
        ~reverse:reverse_normals geometry) in
    Result.map (fun geometry -> match selection_name with
      | None -> geometry
      | Some _ when geometry == selection_geometry -> original_geometry
      | Some name -> Geometry.without_group ~owner:Group.Primitive name geometry)
      result

let run ?cancel ?grain ?selection ?pre_compute_normals
    ?make_normals_unit_length ?unique_points ?consolidate_distance
    ?consolidate_normals_distance ?remove_inline_points ?inline_distance
    ?orient_polygons ?cusp_angle ?remove_degenerate ?make_planar
    ?post_compute_normals ?reverse_normals geometry =
  Error.guard ~operation:"facet" ~code:"invalid_geometry" (fun () ->
    let resolved = match selection with
      | None -> Ok None
      | Some selection -> Result.map Option.some
          (facet_primitives_of_selection ?cancel
            ~grain:(Option.value ~default:16_384 grain) selection geometry) in
    Result.bind resolved (fun primitives ->
      facet ?cancel ?grain ?primitives ?pre_compute_normals
        ?make_normals_unit_length ?unique_points ?consolidate_distance
        ?consolidate_normals_distance ?remove_inline_points ?inline_distance
        ?orient_polygons ?remove_degenerate ?make_planar ?cusp_angle
        ?post_compute_normals ?reverse_normals geometry))
