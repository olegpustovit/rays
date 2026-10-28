
type shape =
  | Bevel_chamfer
  | Bevel_round of { convexity : float }

let ( let* ) = Result.bind

let checked_length label value =
  if Int64.compare value 0L < 0
      || Int64.compare value (Int64.of_int Sys.max_array_length) > 0 then
    Error ("Rdk_mesh.Poly_bevel.poly_bevel: " ^ label ^
      " cardinality exceeds OCaml array limits")
  else Ok (Int64.to_int value)

let validate_name label = function
  | None -> Ok ()
  | Some name when String.trim name = "" ->
      Error ("Rdk_mesh.Poly_bevel.poly_bevel: " ^ label ^ " group name must not be empty")
  | Some _ -> Ok ()

let[@inline always] length3 x y z =
  let scale = max (abs_float x) (max (abs_float y) (abs_float z)) in
  if scale = 0. then 0.
  else
    let x = x /. scale and y = y /. scale and z = z /. scale in
    scale *. sqrt (x *. x +. y *. y +. z *. z)

let[@inline always] unit_between positions source destination =
  let dx = positions.Packed.Float3.View.x.(destination)
      -. positions.x.(source)
  and dy = positions.y.(destination) -. positions.y.(source)
  and dz = positions.z.(destination) -. positions.z.(source) in
  let length = length3 dx dy dz in
  if length = 0. || not (Float.is_finite length) then None
  else Some (dx /. length, dy /. length, dz /. length, length)

let merge_group value groups =
  let rec loop prefix = function
    | [] -> Ok (List.rev_append prefix [value])
    | existing :: rest
      when Group.owner existing = Group.owner value
           && String.equal (Group.name existing) (Group.name value) ->
        let* combined = Group.union existing value in
        Ok (List.rev_append prefix (combined :: rest))
    | existing :: rest -> loop (existing :: prefix) rest
  in
  loop [] groups

let merge_edge_group value groups =
  let rec loop prefix = function
    | [] -> Ok (List.rev_append prefix [value])
    | existing :: rest
      when String.equal (Edge_group.name existing) (Edge_group.name value) ->
        let* combined = Edge_group.union existing value in
        Ok (List.rev_append prefix (combined :: rest))
    | existing :: rest -> loop (existing :: prefix) rest
  in
  loop [] groups

let nearest_mapping left right weights =
  Array.init (Array.length weights) (fun index ->
    if weights.(index) < 0.5 then left.(index) else right.(index))

let select_array ?cancel ~grain mapping source =
  let count = Array.length mapping in
  if count = 0 then [||]
  else begin
    let output = Array.make count source.(mapping.(0)) in
    Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(count - 1) (fun index ->
      if index land 16_383 = 0 then Cancel.check_opt cancel;
      output.(index) <- source.(mapping.(index)));
    output
  end

let interpolate_float ?cancel ~grain left right weights source =
  Parallel.init_array ~grain (Array.length weights) (fun index ->
    if index land 16_383 = 0 then Cancel.check_opt cancel;
    let t = weights.(index) in
    source.(left.(index))
      +. ((source.(right.(index)) -. source.(left.(index))) *. t))

let interpolate_attribute ?cancel ~grain ~point_map ~vertex_left ~vertex_right
    ~vertex_weight ~primitive_map attribute =
  let owner = Attribute.owner attribute in
  if String.equal (Attribute.name attribute) "N"
      && (owner = Attribute.Point || owner = Attribute.Vertex) then Ok None
  else match owner with
    | Attribute.Detail -> Ok (Some attribute)
    | Attribute.Point | Attribute.Primitive ->
        let mapping = if owner = Attribute.Point then point_map else primitive_map in
        Ok (Some (Topology_remap.attribute ?cancel ~grain mapping attribute))
    | Attribute.Vertex ->
        let nearest = lazy (nearest_mapping vertex_left vertex_right vertex_weight) in
        let storage = match Attribute.View.storage attribute with
          | Attribute.Float values -> Attribute.Float
              (interpolate_float ?cancel ~grain vertex_left vertex_right
                 vertex_weight values)
          | Attribute.Int values -> Attribute.Int
              (select_array ?cancel ~grain (Lazy.force nearest) values)
          | Attribute.Text values -> Attribute.Text
              (select_array ?cancel ~grain (Lazy.force nearest) values)
          | Attribute.Int_array values -> Attribute.Int_array
              (Ragged_ops.remap_int ?cancel ~grain (Lazy.force nearest) values)
          | Attribute.Float_array values -> Attribute.Float_array
              (Ragged_ops.remap_float ?cancel ~grain (Lazy.force nearest) values)
          | Attribute.Float2 values ->
              let values = Packed.Float2.View.view values in
              Attribute.Float2 (Packed.Float2.of_owned
                ~x:(interpolate_float ?cancel ~grain vertex_left vertex_right
                      vertex_weight values.x)
                ~y:(interpolate_float ?cancel ~grain vertex_left vertex_right
                      vertex_weight values.y) |> Result.get_ok)
          | Attribute.Float3 values ->
              let values = Packed.Float3.View.view values in
              Attribute.Float3 (Packed.Float3.View.of_owned_exn
                ~x:(interpolate_float ?cancel ~grain vertex_left vertex_right
                      vertex_weight values.x)
                ~y:(interpolate_float ?cancel ~grain vertex_left vertex_right
                      vertex_weight values.y)
                ~z:(interpolate_float ?cancel ~grain vertex_left vertex_right
                      vertex_weight values.z))
          | Attribute.Float4 values ->
              let values = Packed.Float4.View.view values in
              Attribute.Float4 (Packed.Float4.of_owned
                ~x:(interpolate_float ?cancel ~grain vertex_left vertex_right
                      vertex_weight values.x)
                ~y:(interpolate_float ?cancel ~grain vertex_left vertex_right
                      vertex_weight values.y)
                ~z:(interpolate_float ?cancel ~grain vertex_left vertex_right
                      vertex_weight values.z)
                ~w:(interpolate_float ?cancel ~grain vertex_left vertex_right
                      vertex_weight values.w) |> Result.get_ok) in
        let* attribute = Attribute.create_owned ~name:(Attribute.name attribute)
            ~owner storage in
        Ok (Some attribute)

let remap_group ?cancel ~grain ~point_map ~vertex_left ~vertex_right
    ~vertex_weight ~primitive_map group =
  let mapping = match Group.owner group with
    | Group.Point -> point_map
    | Group.Vertex -> nearest_mapping vertex_left vertex_right vertex_weight
    | Group.Primitive -> primitive_map in
  Topology_remap.group ?cancel ~grain mapping group

let add_empty_outputs ~grain ?edge_group ?corner_group ?offset_group geometry =
  let* groups = match edge_group with
    | None -> Ok (Geometry.groups geometry)
    | Some name -> merge_group
        (Group.init ~grain ~owner:Group.Primitive ~name
          (Geometry.primitive_count geometry) (fun _ -> false))
        (Geometry.groups geometry) in
  let* groups = match corner_group with
    | None -> Ok groups
    | Some name -> merge_group
        (Group.init ~grain ~owner:Group.Primitive ~name
          (Geometry.primitive_count geometry) (fun _ -> false)) groups in
  let* edge_groups = match offset_group with
    | None -> Ok (Geometry.edge_groups geometry)
    | Some name ->
        let topology = Geometry.topology geometry in
        let index = Topology_index.create topology in
        merge_edge_group
          (Edge_group.init ~grain ~topology ~index ~name (fun _ -> false))
          (Geometry.edge_groups geometry) in
  Geometry.create ~positions:(Geometry.positions geometry)
    ~topology:(Geometry.topology geometry) ~attributes:(Geometry.attributes geometry)
    ~groups ~edge_groups ()

let run ?cancel ?(grain = 16_384) ?edges ?(shape = Bevel_chamfer)
    ?(divisions = 1) ?point_scale_attribute ?ignore_flat_angle
    ?(clamp_overlap = true) ?edge_group ?corner_group ?offset_group
    ?(recompute_point_normals = true) ~distance geometry =
  Error.guard ~operation:"poly_bevel" ~code:"invalid_topology" @@ fun () ->
  if grain <= 0 then invalid_arg "Rdk_mesh.Poly_bevel.poly_bevel: grain must be positive";
  if not (Float.is_finite distance) || distance < 0. then
    Error "Rdk_mesh.Poly_bevel.poly_bevel: distance must be finite and non-negative"
  else if divisions <= 0 then
    Error "Rdk_mesh.Poly_bevel.poly_bevel: divisions must be positive"
  else
  let* () = match shape with
    | Bevel_chamfer -> Ok ()
    | Bevel_round { convexity }
      when Float.is_finite convexity && convexity >= -1. && convexity <= 1. -> Ok ()
    | Bevel_round _ ->
        Error "Rdk_mesh.Poly_bevel.poly_bevel: round convexity must be finite and within [-1, 1]" in
  let* () = match ignore_flat_angle with
    | None -> Ok ()
    | Some value when Float.is_finite value && value >= 0. && value <= Float.pi -> Ok ()
    | Some _ ->
        Error "Rdk_mesh.Poly_bevel.poly_bevel: flatness angle must be finite and within [0, pi]" in
  let* () = validate_name "edge fillet" edge_group in
  let* () = validate_name "corner fillet" corner_group in
  let* () = validate_name "offset edge" offset_group in
  let topology = Geometry.topology geometry in
  let topology_view = Topology.View.view topology in
  let index_value = Topology_index.create ?cancel topology in
  let index = Topology_index.View.view index_value in
  let edge_count = Array.length index.edge_a
  and point_count = topology_view.point_count
  and vertex_count = Array.length topology_view.vertex_points
  and primitive_count = Bytes.length topology_view.primitive_kinds in
  let* () = match edges with
    | None -> Ok ()
    | Some group when Edge_group.topology_data_id group <> Topology.data_id topology ->
        Error "Rdk_mesh.Poly_bevel.poly_bevel: edge selection belongs to a different topology"
    | Some group when Edge_group.length group <> edge_count ->
        Error "Rdk_mesh.Poly_bevel.poly_bevel: edge selection length does not match topology edge count"
    | Some _ -> Ok () in
  let point_scale = match point_scale_attribute with
    | None -> Ok None
    | Some name when String.trim name = "" ->
        Error "Rdk_mesh.Poly_bevel.poly_bevel: point scale attribute name must not be empty"
    | Some name ->
        (match Geometry.find_attribute ~owner:Attribute.Point name geometry with
         | None -> Error (Printf.sprintf
             "Rdk_mesh.Poly_bevel.poly_bevel: point float scale attribute %S is missing" name)
         | Some attribute ->
             (match Attribute.View.storage attribute with
              | Attribute.Float values -> Ok (Some values)
              | _ -> Error (Printf.sprintf
                  "Rdk_mesh.Poly_bevel.poly_bevel: point scale attribute %S must use float storage"
                  name))) in
  let* point_scale = point_scale in
  let* () = match point_scale with
    | None -> Ok ()
    | Some values ->
        let invalid = ref (-1) in
        let point = ref 0 in
        while !invalid < 0 && !point < Array.length values do
          let value = values.(!point) in
          if not (Float.is_finite value) || value < 0.
              || not (Float.is_finite (distance *. value)) then invalid := !point;
          incr point
        done;
        if !invalid < 0 then Ok () else Error (Printf.sprintf
          "Rdk_mesh.Poly_bevel.poly_bevel: point scale at point %d must produce a finite non-negative distance"
          !invalid) in
  let positions = Packed.Float3.View.view (Geometry.positions geometry) in
  let requested edge = match edges with
    | None -> true
    | Some group -> Edge_group.mem edge group in
  let candidate = Bytes.make edge_count '\000' in
  let candidate_faces = Bytes.make ((primitive_count + 7) / 8) '\000' in
  let mark_face primitive =
    let byte = primitive lsr 3 and mask = 1 lsl (primitive land 7) in
    Bytes.set candidate_faces byte
      (Char.chr (Char.code (Bytes.get candidate_faces byte) lor mask)) in
  let candidate_count = ref 0 in
  for edge = 0 to edge_count - 1 do
    if edge land 16_383 = 0 then Cancel.check_opt cancel;
    if requested edge && index.edge_offsets.(edge + 1) - index.edge_offsets.(edge) = 2
    then begin
      let first = index.edge_offsets.(edge) in
      let left = index.edge_vertices.(first)
      and right = index.edge_vertices.(first + 1) in
      let left_next = index.next_vertex.(left)
      and right_next = index.next_vertex.(right) in
      let left_primitive = index.primitive_of_vertex.(left)
      and right_primitive = index.primitive_of_vertex.(right) in
      if left_next >= 0 && right_next >= 0
          && Bytes.get topology_view.primitive_kinds left_primitive = '\000'
          && Bytes.get topology_view.primitive_kinds right_primitive = '\000'
          && topology_view.vertex_points.(left)
             = topology_view.vertex_points.(right_next)
          && topology_view.vertex_points.(left_next)
             = topology_view.vertex_points.(right)
          && index.edge_a.(edge) <> index.edge_b.(edge) then begin
        Bytes.set candidate edge '\001';
        incr candidate_count;
        mark_face left_primitive;
        mark_face right_primitive
      end
    end
  done;
  let face_normals = match ignore_flat_angle with
    | None -> Ok None
    | Some _ when !candidate_count = 0 -> Ok None
    | Some _ ->
        let selection = Group.init ~grain ~owner:Group.Primitive
            ~name:"__rdk_poly_bevel_faces" primitive_count (fun primitive ->
              Char.code (Bytes.get candidate_faces (primitive lsr 3))
                land (1 lsl (primitive land 7)) <> 0) in
        Result.map Option.some (Face_normals.compute ?cancel ~grain
          ~primitives:selection ~operation:"Poly Bevel" geometry) in
  let* face_normals = face_normals in
  let selected = Bytes.make edge_count '\000' in
  let selected_count = ref 0 in
  for edge = 0 to edge_count - 1 do
    if Bytes.get candidate edge <> '\000' then begin
      let effective = match ignore_flat_angle, face_normals with
        | None, _ -> true
        | Some threshold, Some (nx, ny, nz) ->
            let first = index.edge_offsets.(edge) in
            let left = index.primitive_of_vertex.(index.edge_vertices.(first))
            and right = index.primitive_of_vertex.(index.edge_vertices.(first + 1)) in
            let dot = nx.(left) *. nx.(right) +. ny.(left) *. ny.(right)
                +. nz.(left) *. nz.(right) in
            let dot = max (-1.) (min 1. dot) in
            acos dot > threshold +. (64. *. Float.epsilon)
        | Some _, None -> assert false in
      if effective then begin
        Bytes.set selected edge '\001';
        incr selected_count
      end
    end
  done;
  if distance = 0. || !selected_count = 0 then
    if edge_group = None && corner_group = None && offset_group = None then Ok geometry
    else add_empty_outputs ~grain ?edge_group ?corner_group ?offset_group geometry
  else begin
    let edge_selected edge = edge >= 0 && Bytes.get selected edge <> '\000' in
    let scale_at point = match point_scale with
      | None -> distance
      | Some values -> distance *. values.(point) in
    let touched = Bytes.make vertex_count '\000'
    and predecessor = Array.make vertex_count (-1)
    and successor = Array.make vertex_count (-1)
    and move_to_previous = Array.make vertex_count 0.
    and move_to_next = Array.make vertex_count 0. in
    let touched_count = ref 0 in
    for vertex = 0 to vertex_count - 1 do
      if vertex land 16_383 = 0 then Cancel.check_opt cancel;
      let primitive = index.primitive_of_vertex.(vertex) in
      if Bytes.get topology_view.primitive_kinds primitive = '\000' then begin
        let previous = index.previous_vertex.(vertex) in
        let incoming = if previous < 0 then -1 else index.edge_of_vertex.(previous)
        and outgoing = index.edge_of_vertex.(vertex) in
        let incoming_selected = edge_selected incoming
        and outgoing_selected = edge_selected outgoing in
        if incoming_selected || outgoing_selected then begin
          Bytes.set touched vertex '\001';
          incr touched_count;
          let point = topology_view.vertex_points.(vertex) in
          let amount = scale_at point in
          if not (Float.is_finite amount) then assert false;
          if outgoing_selected then begin
            move_to_previous.(vertex) <- amount;
            let opposite = index.opposite_vertex.(vertex) in
            if opposite < 0 then assert false;
            successor.(vertex) <- index.next_vertex.(opposite)
          end;
          if incoming_selected then begin
            move_to_next.(vertex) <- amount;
            let opposite = index.opposite_vertex.(previous) in
            if opposite < 0 then assert false;
            predecessor.(vertex) <- opposite
          end
        end
      end
    done;
    (* Each face edge owns exactly these two slide distances, so collision
       limiting is deterministic and does not require relaxation passes. *)
    if clamp_overlap then
      for vertex = 0 to vertex_count - 1 do
        if vertex land 16_383 = 0 then Cancel.check_opt cancel;
        let primitive = index.primitive_of_vertex.(vertex) in
        if Bytes.get topology_view.primitive_kinds primitive = '\000' then begin
          let next = index.next_vertex.(vertex) in
          let start_move = move_to_next.(vertex)
          and end_move = move_to_previous.(next) in
          let total = start_move +. end_move in
          if total > 0. then begin
            let a = topology_view.vertex_points.(vertex)
            and b = topology_view.vertex_points.(next) in
            match unit_between positions a b with
            | None ->
                move_to_next.(vertex) <- 0.;
                move_to_previous.(next) <- 0.
            | Some (_, _, _, length) ->
                let limit = length *. (1. -. (128. *. Float.epsilon)) in
                if total > limit then begin
                  let factor = max 0. limit /. total in
                  move_to_next.(vertex) <- start_move *. factor;
                  move_to_previous.(next) <- end_move *. factor
                end
          end
        end
      done;
    (* Equal slide positions on the two sides of an unselected manifold ring
       edge share one topological point. Continuation detection below then
       shares profile rails at degree-two selected points, avoiding duplicate
       corner patches and four-incidence edges. *)
    let corner_parent = Array.init vertex_count Fun.id in
    let rec corner_root vertex =
      let parent = corner_parent.(vertex) in
      if parent = vertex then vertex
      else begin
        let root = corner_root parent in
        corner_parent.(vertex) <- root;
        root
      end in
    let union_corners left right =
      let left = corner_root left and right = corner_root right in
      if left <> right then
        if left < right then corner_parent.(right) <- left
        else corner_parent.(left) <- right in
    for edge = 0 to edge_count - 1 do
      if not (edge_selected edge)
          && index.edge_offsets.(edge + 1) - index.edge_offsets.(edge) = 2 then begin
        let first = index.edge_offsets.(edge) in
        let left = index.edge_vertices.(first)
        and right = index.edge_vertices.(first + 1) in
        let left_next = index.next_vertex.(left)
        and right_next = index.next_vertex.(right) in
        if left_next >= 0 && right_next >= 0
            && Bytes.get topology_view.primitive_kinds
                 index.primitive_of_vertex.(left) = '\000'
            && Bytes.get topology_view.primitive_kinds
                 index.primitive_of_vertex.(right) = '\000' then begin
          if Bytes.get touched left <> '\000'
              && Bytes.get touched right_next <> '\000'
              && move_to_next.(left) = move_to_previous.(right_next)
          then union_corners left right_next;
          if Bytes.get touched left_next <> '\000'
              && Bytes.get touched right <> '\000'
              && move_to_previous.(left_next) = move_to_next.(right)
          then union_corners left_next right
        end
      end
    done;
    let corner_point = Array.make vertex_count (-1) in
    let visited = Bytes.make vertex_count '\000' in
    let component_offsets = Array.make (!touched_count + 1) 0
    and component_corners = Array.make !touched_count 0
    and component_points = Array.make !touched_count 0
    and component_closed = Bytes.make !touched_count '\000' in
    let component_count = ref 0 and component_cursor = ref 0 in
    let add_component start closed =
      let component = !component_count in
      component_offsets.(component) <- !component_cursor;
      component_points.(component) <- topology_view.vertex_points.(start);
      if closed then Bytes.set component_closed component '\001';
      let current = ref start and continuing = ref true in
      while !continuing do
        if Bytes.get visited !current <> '\000' then continuing := false
        else begin
          Bytes.set visited !current '\001';
          component_corners.(!component_cursor) <- !current;
          incr component_cursor;
          let next = successor.(!current) in
          if next < 0 || next = start then continuing := false
          else current := next
        end
      done;
      incr component_count in
    for vertex = 0 to vertex_count - 1 do
      if Bytes.get touched vertex <> '\000' && predecessor.(vertex) < 0
          && Bytes.get visited vertex = '\000' then add_component vertex false
    done;
    for vertex = 0 to vertex_count - 1 do
      if Bytes.get touched vertex <> '\000'
          && Bytes.get visited vertex = '\000' then add_component vertex true
    done;
    component_offsets.(!component_count) <- !component_cursor;
    if !component_cursor <> !touched_count then
      Error "Rdk_mesh.Poly_bevel.poly_bevel: selected edge network has inconsistent point fans"
    else begin
      let component_joined = Bytes.make !component_count '\000' in
      for component = 0 to !component_count - 1 do
        if Bytes.get component_closed component = '\000' then begin
          let first = component_offsets.(component)
          and last = component_offsets.(component + 1) in
          if last - first >= 2
              && corner_root component_corners.(first)
                 = corner_root component_corners.(last - 1) then
            Bytes.set component_joined component '\001'
        end
      done;
      let keep_original = Bytes.make point_count '\000' in
      let mark_original point = Bytes.set keep_original point '\001' in
      for point = 0 to point_count - 1 do
        if index.point_offsets.(point) = index.point_offsets.(point + 1) then
          mark_original point
      done;
      for vertex = 0 to vertex_count - 1 do
        let primitive = index.primitive_of_vertex.(vertex) in
        if Bytes.get topology_view.primitive_kinds primitive <> '\000'
            || Bytes.get touched vertex = '\000' then
          mark_original topology_view.vertex_points.(vertex)
      done;
      for component = 0 to !component_count - 1 do
        if Bytes.get component_closed component = '\000'
            && Bytes.get component_joined component = '\000' then
          mark_original component_points.(component)
      done;
      let kept_count = ref 0 and old_to_output = Array.make point_count (-1) in
      for point = 0 to point_count - 1 do
        if Bytes.get keep_original point <> '\000' then begin
          old_to_output.(point) <- !kept_count;
          incr kept_count
        end
      done;
      let edge_ordinal = Array.make edge_count (-1) and ordinal = ref 0 in
      for edge = 0 to edge_count - 1 do
        if edge_selected edge then begin edge_ordinal.(edge) <- !ordinal; incr ordinal end
      done;
      let interior_per_edge = 2 * (divisions - 1) in
      let* output_points = checked_length "point"
          (Int64.add (Int64.of_int !kept_count)
            (Int64.add (Int64.of_int !touched_count)
              (Int64.mul (Int64.of_int !selected_count)
                (Int64.of_int interior_per_edge)))) in
      let selected_first_point = !kept_count + !touched_count in
      let px = Array.make output_points 0. and py = Array.make output_points 0.
      and pz = Array.make output_points 0.
      and point_map = Array.make output_points 0 in
      for point = 0 to point_count - 1 do
        let output = old_to_output.(point) in
        if output >= 0 then begin
          px.(output) <- positions.x.(point);
          py.(output) <- positions.y.(point);
          pz.(output) <- positions.z.(point);
          point_map.(output) <- point
        end
      done;
      let next_corner = ref !kept_count in
      for vertex = 0 to vertex_count - 1 do
        if Bytes.get touched vertex <> '\000' then begin
          corner_point.(vertex) <- !next_corner;
          incr next_corner
        end
      done;
      let corner_storage = Array.copy corner_point in
      let invalid_position = Atomic.make (-1) in
      if !touched_count > 0 then
        Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(vertex_count - 1)
          (fun vertex ->
            if vertex land 16_383 = 0 then Cancel.check_opt cancel;
            let output = corner_storage.(vertex) in
            if output >= 0 then begin
              let point = topology_view.vertex_points.(vertex)
              and previous = topology_view.vertex_points.(index.previous_vertex.(vertex))
              and next = topology_view.vertex_points.(index.next_vertex.(vertex)) in
              let x = ref positions.x.(point)
              and y = ref positions.y.(point)
              and z = ref positions.z.(point) in
              if move_to_previous.(vertex) <> 0. then
                (match unit_between positions point previous with
                 | None -> ()
                 | Some (dx, dy, dz, _) ->
                     x := !x +. move_to_previous.(vertex) *. dx;
                     y := !y +. move_to_previous.(vertex) *. dy;
                     z := !z +. move_to_previous.(vertex) *. dz);
              if move_to_next.(vertex) <> 0. then
                (match unit_between positions point next with
                 | None -> ()
                 | Some (dx, dy, dz, _) ->
                     x := !x +. move_to_next.(vertex) *. dx;
                     y := !y +. move_to_next.(vertex) *. dy;
                     z := !z +. move_to_next.(vertex) *. dz);
              if not (Float.is_finite !x && Float.is_finite !y && Float.is_finite !z) then
                ignore (Atomic.compare_and_set invalid_position (-1) vertex)
              else begin
                px.(output) <- !x; py.(output) <- !y; pz.(output) <- !z;
                point_map.(output) <- point
              end
            end);
      if Atomic.get invalid_position >= 0 then Error (Printf.sprintf
          "Rdk_mesh.Poly_bevel.poly_bevel: corner position for vertex %d is not finite"
          (Atomic.get invalid_position))
      else begin
        for vertex = 0 to vertex_count - 1 do
          if corner_storage.(vertex) >= 0 then begin
            let root = corner_root vertex in
            corner_point.(vertex) <- corner_storage.(root)
          end
        done;
        let edge_halves edge =
          let first = index.edge_offsets.(edge) in
          index.edge_vertices.(first), index.edge_vertices.(first + 1) in
        let endpoint_corners edge endpoint =
          let left, right = edge_halves edge in
          if endpoint = 0 then left, index.next_vertex.(right)
          else index.next_vertex.(left), right in
        let interior_point edge endpoint layer =
          selected_first_point + edge_ordinal.(edge) * interior_per_edge
            + endpoint * (divisions - 1) + layer - 1 in
        let profile_alias_count = !selected_count * interior_per_edge in
        let profile_alias = Array.init profile_alias_count
            (fun local -> selected_first_point + local) in
        let profile_point edge endpoint layer =
          let side0, side1 = endpoint_corners edge endpoint in
          if layer = 0 then corner_point.(side0)
          else if layer = divisions then corner_point.(side1)
          else profile_alias.
              (interior_point edge endpoint layer - selected_first_point) in
        let fill_profile edge endpoint layer =
          let output = interior_point edge endpoint layer in
          let side0, side1 = endpoint_corners edge endpoint in
          let q0 = corner_point.(side0) and q1 = corner_point.(side1) in
          let source_point = topology_view.vertex_points.(side0) in
          let t = float_of_int layer /. float_of_int divisions in
          let one = 1. -. t in
          let lx = one *. px.(q0) +. t *. px.(q1)
          and ly = one *. py.(q0) +. t *. py.(q1)
          and lz = one *. pz.(q0) +. t *. pz.(q1) in
          let x, y, z = match shape with
            | Bevel_chamfer -> lx, ly, lz
            | Bevel_round { convexity } ->
                let ax = px.(q0) -. positions.x.(source_point)
                and ay = py.(q0) -. positions.y.(source_point)
                and az = pz.(q0) -. positions.z.(source_point)
                and bx = px.(q1) -. positions.x.(source_point)
                and by = py.(q1) -. positions.y.(source_point)
                and bz = pz.(q1) -. positions.z.(source_point) in
                let al = length3 ax ay az and bl = length3 bx by bz in
                let dot = if al = 0. || bl = 0. then 1.
                  else max (-1.) (min 1.
                    ((ax *. bx +. ay *. by +. az *. bz) /. al /. bl)) in
                let weight = sqrt (max 0. ((1. +. dot) *. 0.5)) in
                let denominator = one *. one +. 2. *. one *. t *. weight
                    +. t *. t in
                let arc coordinate0 coordinate1 center =
                  if denominator = 0. then one *. coordinate0 +. t *. coordinate1
                  else (one *. one *. coordinate0
                      +. 2. *. one *. t *. weight *. center
                      +. t *. t *. coordinate1) /. denominator in
                let rx = arc px.(q0) px.(q1) positions.x.(source_point)
                and ry = arc py.(q0) py.(q1) positions.y.(source_point)
                and rz = arc pz.(q0) pz.(q1) positions.z.(source_point) in
                lx +. convexity *. (rx -. lx),
                ly +. convexity *. (ry -. ly),
                lz +. convexity *. (rz -. lz) in
          px.(output) <- x; py.(output) <- y; pz.(output) <- z;
          point_map.(output) <- source_point in
        if divisions > 1 && !selected_count > 0 then
          Parallel.for_ ~chunk_size:(max 1 (grain / max 1 (2 * (divisions - 1))))
            ~start:0 ~finish:(edge_count - 1) (fun edge ->
              if edge land 4_095 = 0 then Cancel.check_opt cancel;
              if edge_selected edge then
                for endpoint = 0 to 1 do
                  for layer = 1 to divisions - 1 do
                    fill_profile edge endpoint layer
                  done
                done);
        let skip_corner_point = Bytes.make point_count '\000' in
        for point = 0 to point_count - 1 do
          let first = index.point_edge_offsets.(point)
          and last = index.point_edge_offsets.(point + 1) in
          let first_edge = ref (-1) and second_edge = ref (-1)
          and count = ref 0 in
          for at = first to last - 1 do
            let edge = index.point_edges.(at) in
            if edge_selected edge then begin
              if !count = 0 then first_edge := edge
              else if !count = 1 then second_edge := edge;
              incr count
            end
          done;
          if !count = 2 then begin
            let endpoint edge =
              let left, _ = edge_halves edge in
              if topology_view.vertex_points.(left) = point then 0 else 1 in
            let edge0, edge1 = if !first_edge < !second_edge
              then !first_edge, !second_edge else !second_edge, !first_edge in
            let endpoint0 = endpoint edge0 and endpoint1 = endpoint edge1 in
            let a0_corner, a1_corner = endpoint_corners edge0 endpoint0
            and b0_corner, b1_corner = endpoint_corners edge1 endpoint1 in
            let a0 = corner_point.(a0_corner) and a1 = corner_point.(a1_corner)
            and b0 = corner_point.(b0_corner) and b1 = corner_point.(b1_corner) in
            let same = a0 = b0 && a1 = b1
            and reversed = a0 = b1 && a1 = b0 in
            if same || reversed then begin
              Bytes.set skip_corner_point point '\001';
              for layer = 1 to divisions - 1 do
                let source_layer = if same then layer else divisions - layer in
                profile_alias.
                  (interior_point edge1 endpoint1 layer - selected_first_point) <-
                  interior_point edge0 endpoint0 source_layer
              done
            end
          end
        done;
        let base_point vertex =
          let point = corner_point.(vertex) in
          if point >= 0 then point
          else old_to_output.(topology_view.vertex_points.(vertex)) in
        let parameter_on_edge a b output =
          match unit_between positions a b with
          | None -> 0.
          | Some (dx, dy, dz, length) ->
              let qx = px.(output) -. positions.x.(a)
              and qy = py.(output) -. positions.y.(a)
              and qz = pz.(output) -. positions.z.(a) in
              max 0. (min 1. ((qx *. dx +. qy *. dy +. qz *. dz) /. length)) in
        let insert_count = Bytes.make vertex_count '\000'
        and insert_point0 = Array.make vertex_count (-1)
        and insert_point1 = Array.make vertex_count (-1)
        and insert_weight0 = Array.make vertex_count 0.
        and insert_weight1 = Array.make vertex_count 0. in
        for vertex = 0 to vertex_count - 1 do
          if vertex land 16_383 = 0 then Cancel.check_opt cancel;
          let primitive = index.primitive_of_vertex.(vertex) in
          if Bytes.get topology_view.primitive_kinds primitive = '\000' then begin
            let edge = index.edge_of_vertex.(vertex) in
            if not (edge_selected edge) then begin
              let opposite = index.opposite_vertex.(vertex) in
              if opposite >= 0 then begin
                let next = index.next_vertex.(vertex) in
                let a = topology_view.vertex_points.(vertex)
                and b = topology_view.vertex_points.(next) in
                let start_point = base_point vertex
                and end_point = base_point next in
                let start_t = parameter_on_edge a b start_point
                and end_t = parameter_on_edge a b end_point in
                let count = ref 0 in
                let add candidate =
                  if candidate >= 0 && candidate <> start_point
                      && candidate <> end_point then begin
                    let t = parameter_on_edge a b candidate in
                    if t >= start_t && t <= end_t then begin
                      if !count = 0 then begin
                        insert_point0.(vertex) <- candidate;
                        insert_weight0.(vertex) <- t;
                        count := 1
                      end else if candidate <> insert_point0.(vertex) then begin
                        if t < insert_weight0.(vertex) then begin
                          insert_point1.(vertex) <- insert_point0.(vertex);
                          insert_weight1.(vertex) <- insert_weight0.(vertex);
                          insert_point0.(vertex) <- candidate;
                          insert_weight0.(vertex) <- t
                        end else begin
                          insert_point1.(vertex) <- candidate;
                          insert_weight1.(vertex) <- t
                        end;
                        count := 2
                      end
                    end
                  end in
                add (corner_point.(index.next_vertex.(opposite)));
                add (corner_point.(opposite));
                Bytes.set insert_count vertex (Char.chr !count)
              end
            end
          end
        done;
        let base_offsets = Array.make (primitive_count + 1) 0 in
        for primitive = 0 to primitive_count - 1 do
          let first = topology_view.primitive_offsets.(primitive)
          and last = topology_view.primitive_offsets.(primitive + 1) in
          let count = ref (last - first) in
          for vertex = first to last - 1 do
            count := !count + Char.code (Bytes.get insert_count vertex)
          done;
          base_offsets.(primitive + 1) <- base_offsets.(primitive) + !count
        done;
        let base_vertices = base_offsets.(primitive_count) in
        let patch_vertices = ref 0 and patch_count = ref 0 in
        let component_output = Array.make !component_count (-1) in
        for component = 0 to !component_count - 1 do
          let corners = component_offsets.(component + 1)
              - component_offsets.(component) in
          let joined = Bytes.get component_joined component <> '\000' in
          let emitted_corners = if joined then corners - 1 else corners in
          let connections = if Bytes.get component_closed component = '\000'
            then corners - 1 else corners in
          let emitted_vertices = emitted_corners
              + connections * (divisions - 1)
              + if Bytes.get component_closed component = '\000' && not joined
                then 1 else 0 in
          if emitted_vertices >= 3 && not (joined && corners <= 2)
              && Bytes.get skip_corner_point component_points.(component) = '\000'
          then begin
            component_output.(component) <- !patch_count;
            incr patch_count;
            patch_vertices := !patch_vertices + emitted_vertices
          end
        done;
        let strip_primitives = !selected_count * divisions
        and strip_vertices = !selected_count * divisions * 4 in
        let* output_primitives = checked_length "primitive"
            (Int64.add (Int64.of_int primitive_count)
              (Int64.add (Int64.of_int strip_primitives)
                (Int64.of_int !patch_count))) in
        let* output_vertices = checked_length "vertex"
            (Int64.add (Int64.of_int base_vertices)
              (Int64.add (Int64.of_int strip_vertices)
                (Int64.of_int !patch_vertices))) in
        let vertex_points = Array.make output_vertices 0
        and vertex_left = Array.make output_vertices 0
        and vertex_right = Array.make output_vertices 0
        and vertex_weight = Array.make output_vertices 0.
        and primitive_offsets = Array.make (output_primitives + 1) 0
        and primitive_kinds = Bytes.make output_primitives '\000'
        and primitive_map = Array.make output_primitives 0
        and edge_membership = Bytes.make output_primitives '\000'
        and corner_membership = Bytes.make output_primitives '\000' in
        for primitive = 0 to primitive_count - 1 do
          primitive_offsets.(primitive) <- base_offsets.(primitive);
          primitive_map.(primitive) <- primitive;
          Bytes.set primitive_kinds primitive
            (Bytes.get topology_view.primitive_kinds primitive)
        done;
        if primitive_count > 0 then
          Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(primitive_count - 1)
            (fun primitive ->
              if primitive land 4_095 = 0 then Cancel.check_opt cancel;
              let first = topology_view.primitive_offsets.(primitive)
              and last = topology_view.primitive_offsets.(primitive + 1) in
              let output = ref base_offsets.(primitive) in
              for vertex = first to last - 1 do
                vertex_points.(!output) <- base_point vertex;
                vertex_left.(!output) <- vertex;
                vertex_right.(!output) <- vertex;
                incr output;
                let next = index.next_vertex.(vertex) in
                let count = Char.code (Bytes.get insert_count vertex) in
                if count > 0 then begin
                  vertex_points.(!output) <- insert_point0.(vertex);
                  vertex_left.(!output) <- vertex;
                  vertex_right.(!output) <- next;
                  vertex_weight.(!output) <- insert_weight0.(vertex);
                  incr output
                end;
                if count > 1 then begin
                  vertex_points.(!output) <- insert_point1.(vertex);
                  vertex_left.(!output) <- vertex;
                  vertex_right.(!output) <- next;
                  vertex_weight.(!output) <- insert_weight1.(vertex);
                  incr output
                end
              done);
        let strip_first_primitive = primitive_count
        and strip_first_vertex = base_vertices in
        for edge = 0 to edge_count - 1 do
          let ordinal = edge_ordinal.(edge) in
          if ordinal >= 0 then begin
            let left, right = edge_halves edge in
            let left_next = index.next_vertex.(left)
            and right_next = index.next_vertex.(right) in
            let left_primitive = index.primitive_of_vertex.(left)
            and right_primitive = index.primitive_of_vertex.(right) in
            for layer = 0 to divisions - 1 do
              let primitive = strip_first_primitive + ordinal * divisions + layer
              and output = strip_first_vertex + (ordinal * divisions + layer) * 4 in
              primitive_offsets.(primitive) <- output;
              primitive_map.(primitive) <- min left_primitive right_primitive;
              Bytes.set edge_membership primitive '\001';
              let set local endpoint row source0 source1 =
                let vertex = output + local in
                vertex_points.(vertex) <- profile_point edge endpoint row;
                vertex_left.(vertex) <- source0;
                vertex_right.(vertex) <- source1;
                vertex_weight.(vertex) <- float_of_int row /. float_of_int divisions in
              (* Opposite to the left face along their shared edge, so the
                 strip winds outward like its neighbours. *)
              set 0 1 layer left_next right;
              set 1 0 layer left right_next;
              set 2 0 (layer + 1) left right_next;
              set 3 1 (layer + 1) left_next right
            done
          end
        done;
        let patch_first_primitive = strip_first_primitive + strip_primitives
        and patch_first_vertex = strip_first_vertex + strip_vertices in
        let patch_cursor = ref patch_first_vertex in
        for component = 0 to !component_count - 1 do
          let output_component = component_output.(component) in
          if output_component >= 0 then begin
          let primitive = patch_first_primitive + output_component
          and first = component_offsets.(component)
          and last = component_offsets.(component + 1) in
          let joined = Bytes.get component_joined component <> '\000' in
          let emitted_last = if joined then last - 1 else last in
          primitive_offsets.(primitive) <- !patch_cursor;
          Bytes.set corner_membership primitive '\001';
          let source_primitive = ref max_int in
          for at = first to emitted_last - 1 do
            let corner = component_corners.(at) in
            vertex_points.(!patch_cursor) <- corner_point.(corner);
            vertex_left.(!patch_cursor) <- corner;
            vertex_right.(!patch_cursor) <- corner;
            source_primitive := min !source_primitive index.primitive_of_vertex.(corner);
            incr patch_cursor;
            let connection_follows = at + 1 < last
                || Bytes.get component_closed component <> '\000' || joined in
            if connection_follows then begin
              let edge = index.edge_of_vertex.(corner) in
              let left, right = edge_halves edge in
              let endpoint, ascending, source0, source1 =
                if corner = left then
                  0, true, left, index.next_vertex.(right)
                else if corner = right then
                  1, false, index.next_vertex.(left), right
                else assert false in
              for local = 1 to divisions - 1 do
                let layer = if ascending then local else divisions - local in
                vertex_points.(!patch_cursor) <- profile_point edge endpoint layer;
                vertex_left.(!patch_cursor) <- source0;
                vertex_right.(!patch_cursor) <- source1;
                vertex_weight.(!patch_cursor) <-
                  float_of_int layer /. float_of_int divisions;
                incr patch_cursor
              done
            end
          done;
          if Bytes.get component_closed component = '\000' && not joined then begin
            vertex_points.(!patch_cursor) <- old_to_output.(component_points.(component));
            let source_corner = component_corners.(first) in
            vertex_left.(!patch_cursor) <- source_corner;
            vertex_right.(!patch_cursor) <- source_corner;
            incr patch_cursor
          end;
          (* The fan order runs against the faces around the point; the
             patch winds outward reversed. *)
          let lo = ref primitive_offsets.(primitive) and hi = ref (!patch_cursor - 1) in
          while !lo < !hi do
            let swap array = let t = array.(!lo) in array.(!lo) <- array.(!hi); array.(!hi) <- t in
            swap vertex_points; swap vertex_left; swap vertex_right; swap vertex_weight;
            incr lo; decr hi
          done;
          primitive_map.(primitive) <- !source_primitive
          end
        done;
        primitive_offsets.(output_primitives) <- output_vertices;
        if !patch_cursor <> output_vertices then assert false;
        let used_points = Bytes.make output_points '\000' in
        Array.iter (fun point -> Bytes.set used_points point '\001') vertex_points;
        let retained_points = ref 0
        and point_compact_map = Array.make output_points (-1) in
        for point = 0 to output_points - 1 do
          if Bytes.get used_points point <> '\000' then begin
            point_compact_map.(point) <- !retained_points;
            incr retained_points
          end
        done;
        let px, py, pz, point_map =
          if !retained_points = output_points then
            px, py, pz, point_map
          else begin
            let compact_x = Array.make !retained_points 0.
            and compact_y = Array.make !retained_points 0.
            and compact_z = Array.make !retained_points 0.
            and compact_source = Array.make !retained_points 0 in
            for point = 0 to output_points - 1 do
              let target = point_compact_map.(point) in
              if target >= 0 then begin
                compact_x.(target) <- px.(point);
                compact_y.(target) <- py.(point);
                compact_z.(target) <- pz.(point);
                compact_source.(target) <- point_map.(point)
              end
            done;
            Array.iteri (fun vertex point ->
              vertex_points.(vertex) <- point_compact_map.(point)) vertex_points;
            compact_x, compact_y, compact_z, compact_source
          end in
        let output_positions = Packed.Float3.View.of_owned_exn ~x:px ~y:py ~z:pz in
        let output_topology = Topology.View.create_validated_owned
            ~point_count:!retained_points ~vertex_points ~primitive_offsets
            ~primitive_kinds in
        let rec map_attributes result = function
          | [] -> Ok (List.rev result)
          | attribute :: rest ->
              let* mapped = interpolate_attribute ?cancel ~grain ~point_map
                  ~vertex_left ~vertex_right ~vertex_weight ~primitive_map attribute in
              map_attributes (match mapped with None -> result
                | Some value -> value :: result) rest in
        let* attributes = map_attributes [] (Geometry.attributes geometry) in
        let groups = List.map (remap_group ?cancel ~grain ~point_map ~vertex_left
            ~vertex_right ~vertex_weight ~primitive_map) (Geometry.groups geometry) in
        let* groups = match edge_group with
          | None -> Ok groups
          | Some name -> merge_group
              (Group.init ~grain ~owner:Group.Primitive ~name output_primitives
                (fun primitive -> Bytes.get edge_membership primitive <> '\000')) groups in
        let* groups = match corner_group with
          | None -> Ok groups
          | Some name -> merge_group
              (Group.init ~grain ~owner:Group.Primitive ~name output_primitives
                (fun primitive -> Bytes.get corner_membership primitive <> '\000')) groups in
        let target_edges = Topology_edge_lookup.create ?cancel
            (Topology.View.view output_topology) in
        let target_edge_count = Topology_edge_lookup.count target_edges in
        let edge_bits predicate =
          let byte_count = (target_edge_count + 7) / 8 in
          let bits = Bytes.make byte_count '\000' in
          let bytes_per_range = max 1 (grain / 8) in
          let range_count = if byte_count = 0 then 0
            else ((byte_count - 1) / bytes_per_range) + 1 in
          if range_count > 0 then
            Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(range_count - 1)
              (fun range ->
                Cancel.check_opt cancel;
                let first = range * bytes_per_range
                and last = min byte_count ((range + 1) * bytes_per_range) in
                for byte = first to last - 1 do
                  let base = byte * 8 and value = ref 0 in
                  for bit = 0 to min 7 (target_edge_count - base - 1) do
                    if predicate (base + bit) then value := !value lor (1 lsl bit)
                  done;
                  Bytes.unsafe_set bits byte (Char.unsafe_chr !value)
                done);
          bits in
        let source_edge_groups = Geometry.edge_groups geometry in
        let edge_groups = List.map (fun source_group ->
          let bits = edge_bits (fun edge ->
              if edge land 16_383 = 0 then Cancel.check_opt cancel;
              let target_a, target_b =
                Topology_edge_lookup.endpoints target_edges edge in
              let a = point_map.(target_a) and b = point_map.(target_b) in
              if a = b then false
              else let source_edge = Topology_index.find_edge_index index_value ~a ~b in
                source_edge >= 0 && Edge_group.mem source_edge source_group) in
          Edge_group.View.of_owned_bits ~topology:output_topology
            ~edge_count:target_edge_count ~name:(Edge_group.name source_group) bits)
          source_edge_groups in
        let* edge_groups = match offset_group with
          | None -> Ok edge_groups
          | Some name ->
              let bits = Bytes.make ((target_edge_count + 7) / 8) '\000' in
              let set edge =
                let byte = edge lsr 3 and mask = 1 lsl (edge land 7) in
                Bytes.set bits byte
                  (Char.chr (Char.code (Bytes.get bits byte) lor mask)) in
              for edge = 0 to edge_count - 1 do
                if edge_selected edge then begin
                  for side = 0 to 1 do
                    let a = point_compact_map.(profile_point edge 0 (side * divisions))
                    and b = point_compact_map.(profile_point edge 1 (side * divisions)) in
                    let target = Topology_edge_lookup.find target_edges ~a ~b in
                    if target >= 0 then set target
                  done
                end
              done;
              merge_edge_group (Edge_group.View.of_owned_bits
                ~topology:output_topology ~edge_count:target_edge_count ~name bits)
                edge_groups in
        let* output = Geometry.create ~positions:output_positions
            ~topology:output_topology ~attributes ~groups ~edge_groups () in
        if recompute_point_normals
            && Option.is_some
              (Geometry.find_attribute ~owner:Attribute.Point "N" geometry)
        then begin
          let* with_normals = Deform.normals ?cancel ~grain output in
          match Geometry.find_attribute ~owner:Attribute.Point "N" with_normals with
          | None -> Ok output
          | Some normal -> Geometry.with_attribute normal output
        end else Ok output
      end
    end
  end
