
type owner = Mirror_point_attributes | Mirror_vertex_attributes
  | Mirror_primitive_attributes

type group_use = Mirror_group_as_source | Mirror_group_as_destination

type method_ =
  | Mirror_by_plane of {
      origin : Rays_math.Vec3.t;
      normal : Rays_math.Vec3.t;
      distance : float;
      tolerance : float;
    }
  | Mirror_by_mapping of {
      mapping_attribute : string;
      destination_group : Group.t;
    }

type transform =
  | Mirror_copy
  | Mirror_uv of {
      origin_u : float;
      origin_v : float;
      direction_u : float;
      direction_v : float;
    }
  | Mirror_vector
  | Mirror_point

type plane = { ox : float; oy : float; oz : float;
  nx : float; ny : float; nz : float; tolerance : float }

type mapping = {
  source_of_output : int array;
  pair_destination : bytes;
  plane : plane option;
}

let fail message = raise (Error.Fail message)

let attribute_owner = function
  | Mirror_point_attributes -> Attribute.Point
  | Mirror_vertex_attributes -> Attribute.Vertex
  | Mirror_primitive_attributes -> Attribute.Primitive

let group_owner = function
  | Mirror_point_attributes -> Group.Point
  | Mirror_vertex_attributes -> Group.Vertex
  | Mirror_primitive_attributes -> Group.Primitive

let owner_name = function
  | Mirror_point_attributes -> "point"
  | Mirror_vertex_attributes -> "vertex"
  | Mirror_primitive_attributes -> "primitive"

let owner_count owner geometry = match owner with
  | Mirror_point_attributes -> Geometry.point_count geometry
  | Mirror_vertex_attributes -> Geometry.vertex_count geometry
  | Mirror_primitive_attributes -> Geometry.primitive_count geometry

let record_first_bad bad element =
  let current = ref (Atomic.get bad) in
  while element < !current
      && not (Atomic.compare_and_set bad !current element) do
    current := Atomic.get bad
  done

let validate_group label owner count = function
  | None -> ()
  | Some group when Group.owner group <> group_owner owner
      || Group.length group <> count ->
      fail (Printf.sprintf "Attribute Mirror %s must be a matching %s group"
        label (owner_name owner))
  | Some _ -> ()

let validate_name label = function
  | None -> ()
  | Some name when String.trim name = "" ->
      fail ("Attribute Mirror " ^ label ^ " must be non-empty")
  | Some _ -> ()

let resolve_plane origin normal distance tolerance =
  let ox = origin.Rays_math.Vec3.x and oy = origin.y and oz = origin.z
  and nx = normal.Rays_math.Vec3.x and ny = normal.y and nz = normal.z in
  if not (Float.is_finite ox && Float.is_finite oy && Float.is_finite oz
      && Float.is_finite nx && Float.is_finite ny && Float.is_finite nz
      && Float.is_finite distance && Float.is_finite tolerance)
  then fail "Attribute Mirror plane parameters must be finite";
  if tolerance < 0. || tolerance > sqrt Float.max_float then
    fail "Attribute Mirror tolerance must be non-negative and safely squarable";
  let scale = Float.max (Float.abs nx)
      (Float.max (Float.abs ny) (Float.abs nz)) in
  if scale = 0. then fail "Attribute Mirror plane normal must be non-zero";
  let sx = nx /. scale and sy = ny /. scale and sz = nz /. scale in
  let length = sqrt ((sx *. sx) +. (sy *. sy) +. (sz *. sz)) in
  let nx = sx /. length and ny = sy /. length and nz = sz /. length in
  let ox = ox +. (distance *. nx) and oy = oy +. (distance *. ny)
  and oz = oz +. (distance *. nz) in
  if not (Float.is_finite ox && Float.is_finite oy && Float.is_finite oz) then
    fail "Attribute Mirror displaced plane origin is not representable";
  { ox; oy; oz; nx; ny; nz; tolerance }

let point_locations geometry = Geometry.positions geometry

let primitive_locations ?cancel ~grain geometry =
  let topology = Geometry.topology geometry
  and positions = Packed.Float3.View.view (Geometry.positions geometry) in
  let count = Geometry.primitive_count geometry in
  let x = Array.make count 0. and y = Array.make count 0.
  and z = Array.make count 0. and bad = Atomic.make max_int in
  Support.parallel_for ?cancel ~grain count (fun primitive ->
    let first, last = Topology.primitive_vertex_range topology primitive in
    if last <= first then record_first_bad bad primitive
    else begin
      let point = Topology.point_of_vertex topology first in
      let min_x = ref positions.x.(point) and max_x = ref positions.x.(point)
      and min_y = ref positions.y.(point) and max_y = ref positions.y.(point)
      and min_z = ref positions.z.(point) and max_z = ref positions.z.(point) in
      for vertex = first + 1 to last - 1 do
        let point = Topology.point_of_vertex topology vertex in
        let px = positions.x.(point) and py = positions.y.(point)
        and pz = positions.z.(point) in
        if px < !min_x then min_x := px; if px > !max_x then max_x := px;
        if py < !min_y then min_y := py; if py > !max_y then max_y := py;
        if pz < !min_z then min_z := pz; if pz > !max_z then max_z := pz
      done;
      let cx = (!min_x *. 0.5) +. (!max_x *. 0.5)
      and cy = (!min_y *. 0.5) +. (!max_y *. 0.5)
      and cz = (!min_z *. 0.5) +. (!max_z *. 0.5) in
      if Float.is_finite cx && Float.is_finite cy && Float.is_finite cz then begin
        x.(primitive) <- cx; y.(primitive) <- cy; z.(primitive) <- cz
      end else record_first_bad bad primitive
    end);
  if Atomic.get bad <> max_int then fail (Printf.sprintf
      "Attribute Mirror primitive %d has no finite bounding-box center"
      (Atomic.get bad));
  Packed.Float3.View.of_owned_exn ~x ~y ~z

let locations ?cancel ~grain owner geometry = match owner with
  | Mirror_point_attributes -> point_locations geometry
  | Mirror_primitive_attributes -> primitive_locations ?cancel ~grain geometry
  | Mirror_vertex_attributes ->
      fail "Attribute Mirror plane correspondence does not yet support vertex attributes"

let plane_distance plane x y z =
  ((x -. plane.ox) *. plane.nx) +. ((y -. plane.oy) *. plane.ny)
    +. ((z -. plane.oz) *. plane.nz)

let selected_for_side group group_use ~source element = match group with
  | None -> true
  | Some group ->
      let member = Group.mem element group in
      match group_use, source with
      | Mirror_group_as_source, true
      | Mirror_group_as_destination, false -> member
      | Mirror_group_as_source, false
      | Mirror_group_as_destination, true -> not member

let finalize_mapping plane source_of_output =
  let count = Array.length source_of_output in
  let pair_destination = Bytes.make count '\000' in
  for destination = 0 to count - 1 do
    let source = source_of_output.(destination) in
    if source >= 0 then begin
      Bytes.set pair_destination destination '\001'
    end else source_of_output.(destination) <- destination
  done;
  { source_of_output; pair_destination; plane }

let plane_mapping ?cancel ~grain ~group ~group_use owner plane geometry =
  let count = owner_count owner geometry in
  let locations = locations ?cancel ~grain owner geometry in
  let view = Packed.Float3.View.view locations in
  let side = Bytes.make count '\000' and bad = Atomic.make max_int in
  Support.parallel_for ?cancel ~grain count (fun element ->
    let distance = plane_distance plane view.x.(element) view.y.(element)
        view.z.(element) in
    if not (Float.is_finite distance) then record_first_bad bad element
    else if distance < 0.
        && selected_for_side group group_use ~source:true element then
      Bytes.set side element '\001'
    else if distance > 0.
        && selected_for_side group group_use ~source:false element then
      Bytes.set side element '\002');
  if Atomic.get bad <> max_int then fail (Printf.sprintf
      "Attribute Mirror %s %d has an unrepresentable plane distance"
      (owner_name owner) (Atomic.get bad));
  let source_count = ref 0 and destination_count = ref 0 in
  for element = 0 to count - 1 do
    if Bytes.get side element = '\001' then incr source_count
    else if Bytes.get side element = '\002' then incr destination_count
  done;
  let source_elements = Array.make !source_count 0
  and x = Array.make !source_count 0. and y = Array.make !source_count 0.
  and z = Array.make !source_count 0. in
  let next = ref 0 in
  for element = 0 to count - 1 do
    if Bytes.get side element = '\001' then begin
      let slot = !next in incr next; source_elements.(slot) <- element;
      let px = view.x.(element) and py = view.y.(element)
      and pz = view.z.(element) in
      let distance = plane_distance plane px py pz in
      x.(slot) <- px -. (2. *. distance *. plane.nx);
      y.(slot) <- py -. (2. *. distance *. plane.ny);
      z.(slot) <- pz -. (2. *. distance *. plane.nz);
      if not (Float.is_finite x.(slot) && Float.is_finite y.(slot)
          && Float.is_finite z.(slot)) then fail (Printf.sprintf
          "Attribute Mirror reflected %s %d is not representable"
          (owner_name owner) element)
    end
  done;
  let reflected = Packed.Float3.View.of_owned_exn ~x ~y ~z in
  let index = Spatial_index.create ?cancel ~grain reflected
    |> function Ok index -> index | Error error -> fail (Error.to_string error) in
  let destination_elements = Array.make !destination_count 0
  and query_x = Array.make !destination_count 0.
  and query_y = Array.make !destination_count 0.
  and query_z = Array.make !destination_count 0. in
  let next = ref 0 in
  for element = 0 to count - 1 do
    if Bytes.get side element = '\002' then begin
      let slot = !next in
      incr next;
      destination_elements.(slot) <- element;
      query_x.(slot) <- view.x.(element);
      query_y.(slot) <- view.y.(element);
      query_z.(slot) <- view.z.(element)
    end
  done;
  let queries = Packed.Float3.View.of_owned_exn
      ~x:query_x ~y:query_y ~z:query_z in
  let nearest = Array.make !destination_count (-1)
  and distances = Array.make !destination_count infinity
  and counts = Array.make !destination_count 0 in
  Spatial_index.View.nearest_k_many_into ?cancel ~grain index
    ~queries ~max_distance_squared:(plane.tolerance *. plane.tolerance)
    ~capacity:1 ~indices:nearest ~distances_squared:distances ~counts;
  let source_of_output = Array.make count (-1) in
  Support.parallel_for ?cancel ~grain !destination_count (fun slot ->
    if counts.(slot) = 1 then
      source_of_output.(destination_elements.(slot)) <-
        source_elements.(nearest.(slot)));
  finalize_mapping (Some plane) source_of_output

let explicit_mapping ?cancel ~grain ~group ~group_use owner
    mapping_attribute destination_group geometry =
  let count = owner_count owner geometry and attribute_owner = attribute_owner owner in
  if String.trim mapping_attribute = "" then
    fail "Attribute Mirror mapping attribute must be non-empty";
  validate_group "mapping destination group" owner count
    (Some destination_group);
  let values = match Geometry.find_attribute ~owner:attribute_owner
      mapping_attribute geometry with
    | None -> fail (Printf.sprintf
        "Attribute Mirror could not find %s mapping attribute %S"
        (owner_name owner) mapping_attribute)
    | Some attribute ->
        (match Attribute.View.storage attribute with
         | Attribute.Int values when Array.length values = count -> values
         | Attribute.Int _ -> fail "Attribute Mirror mapping cardinality mismatch"
         | _ -> fail "Attribute Mirror mapping attribute must be integer") in
  let source_of_output = Array.init count Fun.id
  and pair_destination = Bytes.make count '\000' in
  Support.parallel_for ?cancel ~grain count (fun destination ->
    if Group.mem destination destination_group then begin
      let source = values.(destination) in
      if source >= 0 && source < count then begin
        let selected = match group with
          | None -> true
          | Some group ->
              (match group_use with
               | Mirror_group_as_source -> Group.mem source group
               | Mirror_group_as_destination -> Group.mem destination group) in
        if selected then begin
          source_of_output.(destination) <- source;
          Bytes.set pair_destination destination '\001'
        end
      end
    end);
  { source_of_output; pair_destination; plane = None }

let mapping_changed mapping =
  let changed = ref false and element = ref 0 in
  while not !changed && !element < Bytes.length mapping.pair_destination do
    changed := Bytes.get mapping.pair_destination !element = '\001';
    incr element
  done;
  !changed

let output_mapping_values mapping =
  let count = Array.length mapping.source_of_output in
  let output = Array.make count (-1) in
  for destination = 0 to count - 1 do
    if Bytes.get mapping.pair_destination destination = '\001' then begin
      let source = mapping.source_of_output.(destination) in
      output.(destination) <- source;
      output.(source) <- source
    end
  done;
  output

let source_members mapping =
  let count = Array.length mapping.source_of_output in
  let members = Bytes.make count '\000' in
  for destination = 0 to count - 1 do
    if Bytes.get mapping.pair_destination destination = '\001' then
      Bytes.set members mapping.source_of_output.(destination) '\001'
  done;
  members

let replace_all ~search ~replacement source =
  if search = "" then invalid_arg "Attribute Mirror empty string search";
  let search_length = String.length search and length = String.length source in
  let matches_at offset =
    if offset + search_length > length then false
    else begin
      let matches = ref true and index = ref 0 in
      while !matches && !index < search_length do
        if source.[offset + !index] <> search.[!index] then matches := false;
        incr index
      done;
      !matches
    end in
  let buffer = Buffer.create length and cursor = ref 0 and changed = ref false in
  while !cursor < length do
    if matches_at !cursor then begin
      Buffer.add_string buffer replacement;
      cursor := !cursor + search_length;
      changed := true
    end else begin
      Buffer.add_char buffer source.[!cursor]; incr cursor
    end
  done;
  if !changed then Buffer.contents buffer else source

let copy_array ?cancel ~grain mapping values =
  let count = Array.length mapping.source_of_output in
  let output = Array.make count values.(0) in
  Support.parallel_for ?cancel ~grain count (fun element ->
    output.(element) <- values.(mapping.source_of_output.(element)));
  output

let copy_float ?cancel ~grain mapping label values =
  let count = Array.length mapping.source_of_output in
  let output = Array.make count 0. and bad = Atomic.make max_int in
  Support.parallel_for ?cancel ~grain count (fun element ->
    let value = values.(mapping.source_of_output.(element)) in
    if Float.is_finite value then output.(element) <- value
    else record_first_bad bad element);
  if Atomic.get bad <> max_int then fail (Printf.sprintf
      "Attribute Mirror %s is non-finite at element %d" label (Atomic.get bad));
  output

let reflected_uv transform u v = match transform with
  | Mirror_uv { origin_u; origin_v; direction_u; direction_v } ->
      let du = u -. origin_u and dv = v -. origin_v in
      let normal_u = -. direction_v and normal_v = direction_u in
      let distance = (du *. normal_u) +. (dv *. normal_v) in
      u -. (2. *. distance *. normal_u), v -. (2. *. distance *. normal_v)
  | Mirror_copy | Mirror_vector | Mirror_point -> u, v

let reflected_xyz transform plane x y z = match transform with
  | Mirror_vector ->
      let plane = Option.get plane in
      let distance = (x *. plane.nx) +. (y *. plane.ny) +. (z *. plane.nz) in
      x -. (2. *. distance *. plane.nx),
      y -. (2. *. distance *. plane.ny),
      z -. (2. *. distance *. plane.nz)
  | Mirror_point ->
      let plane = Option.get plane in
      let distance = plane_distance plane x y z in
      x -. (2. *. distance *. plane.nx),
      y -. (2. *. distance *. plane.ny),
      z -. (2. *. distance *. plane.nz)
  | Mirror_copy | Mirror_uv _ -> x, y, z

let copy_tuple ?cancel ~grain mapping transform label source =
  let width = Array.length source and count = Array.length mapping.source_of_output in
  let output = Array.init width (fun _ -> Array.make count 0.)
  and bad = Atomic.make max_int in
  Support.parallel_for ?cancel ~grain count (fun element ->
    let source_element = mapping.source_of_output.(element) in
    let valid = ref true in
    for component = 0 to width - 1 do
      if not (Float.is_finite source.(component).(source_element)) then valid := false
    done;
    if not !valid then record_first_bad bad element
    else begin
      for component = 0 to width - 1 do
        output.(component).(element) <- source.(component).(source_element)
      done;
      if Bytes.get mapping.pair_destination element = '\001' then begin
        (match transform with
         | Mirror_uv _ when width >= 2 ->
             let u, v = reflected_uv transform output.(0).(element)
                 output.(1).(element) in
             output.(0).(element) <- u; output.(1).(element) <- v
         | (Mirror_vector | Mirror_point) when width >= 3 ->
             let x, y, z = reflected_xyz transform mapping.plane
                 output.(0).(element) output.(1).(element) output.(2).(element) in
             output.(0).(element) <- x; output.(1).(element) <- y;
             output.(2).(element) <- z
         | Mirror_copy | Mirror_uv _ | Mirror_vector | Mirror_point -> ());
        for component = 0 to width - 1 do
          if not (Float.is_finite output.(component).(element)) then valid := false
        done;
        if not !valid then record_first_bad bad element
      end
    end);
  if Atomic.get bad <> max_int then fail (Printf.sprintf
      "Attribute Mirror %s is or becomes non-finite at element %d"
      label (Atomic.get bad));
  output

let remap_attribute ?cancel ~grain ~transform ~string_replace mapping attribute =
  let name = Attribute.name attribute and owner = Attribute.owner attribute in
  let label = Printf.sprintf "%s attribute %S"
      (match owner with Attribute.Point -> "point" | Attribute.Vertex -> "vertex"
        | Attribute.Primitive -> "primitive" | Attribute.Detail -> "detail") name in
  let storage = match Attribute.View.storage attribute with
    | Attribute.Float values -> Attribute.Float
        (copy_float ?cancel ~grain mapping label values)
    | Attribute.Int values -> Attribute.Int
        (if Array.length values = 0 then [||] else copy_array ?cancel ~grain mapping values)
    | Attribute.Text values ->
        let output = if Array.length values = 0 then [||]
          else copy_array ?cancel ~grain mapping values in
        (match string_replace with
         | None -> ()
         | Some (search, replacement) ->
             Support.parallel_for ?cancel ~grain (Array.length output) (fun element ->
               if Bytes.get mapping.pair_destination element = '\001' then
                 output.(element) <- replace_all ~search ~replacement output.(element)));
        Attribute.Text output
    | Attribute.Float2 values ->
        let values = Packed.Float2.View.view values in
        let output = copy_tuple ?cancel ~grain mapping transform label
            [|values.x; values.y|] in
        Attribute.Float2 (Packed.Float2.of_owned ~x:output.(0) ~y:output.(1)
          |> Result.get_ok)
    | Attribute.Float3 values ->
        let values = Packed.Float3.View.view values in
        let output = copy_tuple ?cancel ~grain mapping transform label
            [|values.x; values.y; values.z|] in
        Attribute.Float3 (Packed.Float3.View.of_owned_exn
          ~x:output.(0) ~y:output.(1) ~z:output.(2))
    | Attribute.Float4 values ->
        let values = Packed.Float4.View.view values in
        let output = copy_tuple ?cancel ~grain mapping transform label
            [|values.x; values.y; values.z; values.w|] in
        Attribute.Float4 (Packed.Float4.of_owned ~x:output.(0) ~y:output.(1)
          ~z:output.(2) ~w:output.(3) |> Result.get_ok)
    | Attribute.Int_array values -> Attribute.Int_array
        (Ragged_ops.remap_int ?cancel ~grain mapping.source_of_output values)
    | Attribute.Float_array values -> Attribute.Float_array
        (Ragged_ops.remap_float ?cancel ~grain mapping.source_of_output values) in
  Attribute.create_owned ~owner ~name storage
  |> function Ok attribute -> attribute | Error message -> fail message

let position_attribute ?cancel ~grain ~transform mapping geometry =
  let values = Packed.Float3.View.view (Geometry.positions geometry) in
  let output = copy_tuple ?cancel ~grain mapping transform "point attribute P"
      [|values.x; values.y; values.z|] in
  Packed.Float3.View.of_owned_exn ~x:output.(0) ~y:output.(1) ~z:output.(2)

let run ?cancel ?(grain = 16_384) ?group
    ?(group_use = Mirror_group_as_source) ?(attributes = "Cd")
    ?(transform = Mirror_copy) ?string_replace ?output_mapping ?source_group
    ?destination_group ~owner ~method_ geometry =
  Error.guard ~operation:"attribute_mirror" ~code:"invalid_attribute_mirror" @@ fun () ->
  try
    if grain <= 0 then fail "Attribute Mirror grain must be positive";
    let count = owner_count owner geometry in
    validate_group "group" owner count group;
    validate_name "output mapping name" output_mapping;
    validate_name "source group name" source_group;
    validate_name "destination group name" destination_group;
    (match source_group, destination_group with
     | Some source, Some destination when String.equal source destination ->
         fail "Attribute Mirror source and destination group names must differ"
     | _ -> ());
    Option.iter (fun (search, _) -> if search = "" then
      fail "Attribute Mirror string search must be non-empty") string_replace;
    let pattern = Attribute_pattern.compile attributes
      |> function Ok pattern -> pattern | Error message -> fail message in
    let transform = match transform with
      | Mirror_uv { origin_u; origin_v; direction_u; direction_v } ->
          if not (Float.is_finite origin_u && Float.is_finite origin_v
              && Float.is_finite direction_u && Float.is_finite direction_v) then
            fail "Attribute Mirror UV line must be finite";
          let scale = Float.max (Float.abs direction_u) (Float.abs direction_v) in
          if scale = 0. then fail "Attribute Mirror UV direction must be non-zero";
          let u = direction_u /. scale and v = direction_v /. scale in
          let length = sqrt ((u *. u) +. (v *. v)) in
          Mirror_uv { origin_u; origin_v;
            direction_u = u /. length; direction_v = v /. length }
      | transform -> transform in
    let mapping = match method_ with
      | Mirror_by_plane { origin; normal; distance; tolerance } ->
          let plane = resolve_plane origin normal distance tolerance in
          plane_mapping ?cancel ~grain ~group ~group_use owner plane geometry
      | Mirror_by_mapping { mapping_attribute; destination_group } ->
          (match transform with
           | Mirror_vector | Mirror_point -> fail
               "Attribute Mirror vector/point transformation requires plane correspondence"
           | Mirror_copy | Mirror_uv _ -> ());
          explicit_mapping ?cancel ~grain ~group ~group_use owner
            mapping_attribute destination_group geometry in
    let attribute_owner = attribute_owner owner in
    let changed = mapping_changed mapping in
    let mirror_position = owner = Mirror_point_attributes
        && changed && Attribute_pattern.matches pattern "P" in
    let selected = Geometry.attributes geometry |> List.filter (fun attribute ->
      Attribute.owner attribute = attribute_owner
      && Attribute_pattern.matches pattern (Attribute.name attribute)) in
    let attributes = if changed then Array.of_list (List.map
        (remap_attribute ?cancel ~grain ~transform ~string_replace mapping) selected)
      else [||] in
    let positions = if mirror_position then Some
        (position_attribute ?cancel ~grain ~transform mapping geometry)
      else None in
    let generated = ref (Array.to_list attributes) in
    Option.iter (fun name ->
      let attribute = Attribute.create_owned ~owner:attribute_owner ~name
          (Attribute.Int (output_mapping_values mapping))
        |> function Ok value -> value | Error message -> fail message in
      generated := attribute :: !generated) output_mapping;
    let groups = ref [] in
    Option.iter (fun name ->
      let members = source_members mapping in
      groups := Group.init ~grain ~owner:(group_owner owner) ~name count
          (fun element -> Bytes.get members element = '\001') :: !groups)
      source_group;
    Option.iter (fun name -> groups := Group.init ~grain ~owner:(group_owner owner)
        ~name count (fun element -> Bytes.get mapping.pair_destination element = '\001')
      :: !groups) destination_group;
    if not changed && positions = None && Array.length attributes = 0
        && output_mapping = None && !groups = [] then Ok geometry
    else begin
      let output = Geometry.View.with_merged_owned
          ?positions ~attributes:(Array.of_list (List.rev !generated))
          ~groups:(Array.of_list (List.rev !groups)) geometry
        |> function Ok output -> output | Error message -> fail message in
      let mirrored_normal owner = List.exists (fun attribute ->
        Attribute.owner attribute = owner
        && String.equal (Attribute.name attribute) "N") selected in
      let output = match positions with
        | None -> output
        | Some _ ->
            let output = if mirrored_normal Attribute.Point then output else
                Geometry.without_attribute ~owner:Attribute.Point "N" output in
            if mirrored_normal Attribute.Vertex then output else
              Geometry.without_attribute ~owner:Attribute.Vertex "N" output in
      Ok output
    end
  with Error.Fail message -> Error message
