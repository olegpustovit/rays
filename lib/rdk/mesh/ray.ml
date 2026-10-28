
type method_ = Ray_minimum_distance | Ray_project

type direction =
  | Ray_vector of Rays_math.Vec3.t
  | Ray_normal
  | Ray_attribute of string

type direction_mode = Surface_index.ray_direction_mode =
  | Ray_forward
  | Ray_reverse
  | Ray_bidirectional_closest
  | Ray_bidirectional_farthest

type surface_hit = Surface_index.ray_surface_hit =
  | Ray_first_surface
  | Ray_last_surface

type combine =
  | Ray_average
  | Ray_median
  | Ray_shortest
  | Ray_longest

exception Ray_error of string * string
exception Ray_rdk_error of Error.t

let fail code message = raise (Ray_error (code, message))
let get_string = function Ok value -> value | Error message -> fail "invalid_output" message
let get_rdk = function Ok value -> value | Error error -> raise (Ray_rdk_error error)

let validate_name label = function
  | None -> ()
  | Some name when String.trim name = "" ->
      fail "invalid_name" (label ^ " must not be empty")
  | Some "P" -> fail "invalid_name" (label ^ " cannot be P")
  | Some _ -> ()

let unique_attribute_name source collision reserved base =
  let exists geometry name = List.exists (fun attribute ->
    String.equal (Attribute.name attribute) name) (Geometry.attributes geometry) in
  let rec find suffix =
    let name = if suffix = 0 then base else base ^ string_of_int suffix in
    if List.mem name reserved || exists source name || exists collision name
    then find (suffix + 1) else name in
  find 0

let point_selection ?cancel ~grain geometry selection =
  let topology = Geometry.topology geometry in
  (match Deform.validate_selection topology selection with
   | Ok () -> ()
   | Error message -> fail "invalid_selection" message);
  match selection with
  | None -> None
  | Some (Deform.Selected_points group) -> Some group
  | Some _ ->
      let index = Some (Topology_index.create ?cancel topology) in
      Some (Group.init ~grain ~owner:Group.Point ~name:"__ray_selection"
        (Geometry.point_count geometry) (fun point ->
          if point land 4095 = 0 then Cancel.check_opt cancel;
          Deform.point_selected selection index point))

let direction_planes ?cancel ~grain source = function
  | Ray_vector value ->
      if not (Float.is_finite value.Rays_math.Vec3.x && Float.is_finite value.y && Float.is_finite value.z) then
        fail "invalid_direction" "ray direction vector must be finite";
      if value.x = 0. && value.y = 0. && value.z = 0. then
        fail "invalid_direction" "ray direction vector must be non-zero";
      Surface_index.Private.Constant_direction {
        x = value.x; y = value.y; z = value.z }
  | Ray_attribute name ->
      if String.trim name = "" then
        fail "invalid_direction" "ray direction attribute name must not be empty";
      let values = match Deform.point_vector_attribute "Rdk.Ray.ray" name source with
        | Ok values -> values | Error message -> fail "invalid_direction" message in
      let packed = Packed.Float3.Private.of_shared_exn
          ~x:values.x ~y:values.y ~z:values.z in
      Surface_index.Private.Per_query_directions
        (Packed.Float3.Private.view packed)
  | Ray_normal ->
      let values = match Deform.resolve_directions ?cancel ~grain source with
        | Ok values -> values | Error message -> fail "invalid_direction" message in
      let packed = Packed.Float3.Private.of_shared_exn
          ~x:values.x ~y:values.y ~z:values.z in
      Surface_index.Private.Per_query_directions
        (Packed.Float3.Private.view packed)

let existing_point_float name count geometry default =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | None -> Array.make count default
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Float values -> Array.copy values
       | _ -> fail "invalid_output" (Printf.sprintf
           "point output attribute %S must have float storage" name))

let existing_point_int name count geometry default =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | None -> Array.make count default
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Int values -> Array.copy values
       | _ -> fail "invalid_output" (Printf.sprintf
           "point output attribute %S must have integer storage" name))

let existing_point_float3 name count geometry =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | None -> Array.make count 0., Array.make count 0., Array.make count 0.
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Float3 values ->
           let values = Packed.Float3.Private.view values in
           Array.copy values.x, Array.copy values.y, Array.copy values.z
       | _ -> fail "invalid_output" (Printf.sprintf
           "point output attribute %S must have float3 storage" name))

let run ?cancel ?(grain = 16_384) ?selection ?collision_primitives
    ?(method_ = Ray_project) ?(direction = Ray_normal)
    ?(direction_mode = Ray_forward) ?(surface_hit = Ray_first_surface)
    ?(samples = 1) ?(jitter_scale = 1.) ?(seed = 0)
    ?(combine = Ray_average)
    ?(min_distance = 0.) ?max_distance ?(tolerance = 0.) ?(scale = 1.)
    ?(lift = 0.) ?distance_attribute ?primitive_attribute
    ?source_vertex_numbers_attribute ?source_vertex_weights_attribute
    ?hit_group ?normal_attribute ?point_pattern ?vertex_pattern
    ?primitive_pattern ?detail_pattern ?(match_groups = false)
    ~source ~collision () =
  try
    if grain <= 0 then fail "invalid_parameter" "grain must be positive";
    if samples < 1 || samples > 1024 then fail "invalid_parameter"
        "ray samples must be between 1 and 1024";
    if not (Float.is_finite jitter_scale && jitter_scale >= 0.) then
      fail "invalid_parameter" "ray jitter scale must be finite and non-negative";
    if method_ = Ray_minimum_distance && samples <> 1 then
      fail "invalid_parameter" "multiple samples require directional projection";
    if not (Float.is_finite scale && Float.is_finite lift) then
      fail "invalid_parameter" "scale and lift must be finite";
    if not (Float.is_finite min_distance && min_distance >= 0.) then
      fail "invalid_distance" "minimum distance must be finite and non-negative";
    if not (Float.is_finite tolerance && tolerance >= 0.
        && tolerance <= sqrt max_float) then
      fail "invalid_distance"
        "ray tolerance must be finite, non-negative, and safely squarable";
    let maximum = match max_distance with
      | None -> Float.infinity
      | Some value when Float.is_finite value && value >= min_distance -> value
      | Some _ -> fail "invalid_distance"
          "maximum distance must be finite and at least the minimum distance" in
    if method_ = Ray_minimum_distance && maximum <> Float.infinity
        && maximum > sqrt max_float then
      fail "invalid_distance" "maximum closest distance is too large to square";
    List.iter (fun (label, name) -> validate_name label name) [
      "distance attribute", distance_attribute;
      "primitive attribute", primitive_attribute;
      "source vertex numbers attribute", source_vertex_numbers_attribute;
      "source vertex weights attribute", source_vertex_weights_attribute;
      "normal attribute", normal_attribute ];
    (match hit_group with
     | Some name when String.trim name = "" ->
         fail "invalid_name" "hit group name must not be empty"
     | None | Some _ -> ());
    if match_groups && point_pattern = None && vertex_pattern = None
        && primitive_pattern = None then
      fail "invalid_pattern"
        "group interpolation requires a point, vertex, or primitive pattern";
    let paired = match source_vertex_numbers_attribute,
        source_vertex_weights_attribute with
      | None, None | Some _, Some _ -> true
      | None, Some _ | Some _, None -> false in
    if not paired then fail "invalid_name"
        "source vertex number and weight outputs must be requested together";
    let output_names = List.filter_map Fun.id [distance_attribute;
      primitive_attribute; source_vertex_numbers_attribute;
      source_vertex_weights_attribute; normal_attribute] in
    if List.length output_names
        <> List.length (List.sort_uniq String.compare output_names) then
      fail "invalid_name" "ray output attribute names must be distinct";
    let point_count = Geometry.point_count source in
    let selection = point_selection ?cancel ~grain source selection in
    let surface = get_rdk (Surface_index.create ?cancel ~grain
      ?primitives:collision_primitives collision) in
    let normal_planes = match normal_attribute with
      | None when lift = 0. -> None
      | None -> Some (Array.make point_count 0., Array.make point_count 0.,
          Array.make point_count 0.)
      | Some name -> Some (existing_point_float3 name point_count source) in
    let primitives = Array.make point_count (-1)
    and triangles = Array.make point_count (-1)
    and barycentric_a = Array.make point_count 0.
    and barycentric_b = Array.make point_count 0.
    and barycentric_c = Array.make point_count 0.
    and distances = Array.make point_count Float.infinity
    and direction_signs = Array.make point_count 0
    and hit_counts = Array.make point_count 0 in
    let directions = match method_ with
      | Ray_minimum_distance -> None
      | Ray_project -> Some (direction_planes ?cancel ~grain source direction) in
    (match method_ with
     | Ray_minimum_distance ->
         let maximum_squared = if maximum = Float.infinity then Float.infinity
           else maximum *. maximum in
         Surface_index.Private.closest_many_into ?cancel ?selection ~grain surface
           ~queries:(Geometry.positions source) ~max_distance_squared:maximum_squared
           ~primitives ~triangles ~barycentric_a ~barycentric_b ~barycentric_c
           ~distances_squared:distances;
         Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(point_count - 1)
           (fun point -> if primitives.(point) >= 0 then
             distances.(point) <- sqrt distances.(point))
     | Ray_project ->
         let directions = Option.get directions in
         if samples = 1 then
           Surface_index.Private.raycast_many_into ?cancel ?selection ~grain surface
             ~queries:(Geometry.positions source) ~directions
             ~min_distance ~max_distance:maximum ~tolerance ~direction_mode
             ~surface_hit ~primitives ~triangles ~barycentric_a ~barycentric_b
             ~barycentric_c ~distances
         else
           let combine = match combine with
             | Ray_average -> Surface_index.Private.Sample_average
             | Ray_median -> Surface_index.Private.Sample_median
             | Ray_shortest -> Surface_index.Private.Sample_shortest
             | Ray_longest -> Surface_index.Private.Sample_longest in
           let normal_x, normal_y, normal_z = match normal_planes with
             | None -> None, None, None
             | Some (x, y, z) -> Some x, Some y, Some z in
           Surface_index.Private.raycast_samples_into ?cancel ?selection ~grain
             surface ~queries:(Geometry.positions source) ~directions
             ~min_distance ~max_distance:maximum ~tolerance ~direction_mode
             ~surface_hit ~samples ~jitter_scale ~seed ~combine ~primitives
             ~triangles ~barycentric_a ~barycentric_b ~barycentric_c ~distances
             ~direction_signs ~hit_counts ~normal_x ~normal_y ~normal_z);
    let source_positions = Packed.Float3.Private.view (Geometry.positions source)
    and collision_positions = Packed.Float3.Private.view
        (Geometry.positions collision)
    and collision_topology = Topology.Private.view (Geometry.topology collision) in
    let px = Array.copy source_positions.x and py = Array.copy source_positions.y
    and pz = Array.copy source_positions.z in
    let range_count = if point_count = 0 then 0 else
        (point_count + grain - 1) / grain in
    let errors = Array.make range_count (-1) and changed = Array.make range_count false in
    if range_count > 0 then Parallel.for_ ~chunk_size:1 ~start:0
        ~finish:(range_count - 1) (fun range ->
      let first = range * grain and last = min point_count ((range + 1) * grain) in
      let normal = Array.make 3 0. in
      for point = first to last - 1 do
        if point land 4095 = 0 then Cancel.check_opt cancel;
        let triangle = triangles.(point) in
        if triangle >= 0 then begin
          let va = Surface_index.Private.triangle_vertex surface triangle 0
          and vb = Surface_index.Private.triangle_vertex surface triangle 1
          and vc = Surface_index.Private.triangle_vertex surface triangle 2 in
          let a = collision_topology.vertex_points.(va)
          and b = collision_topology.vertex_points.(vb)
          and c = collision_topology.vertex_points.(vc) in
          let wa = barycentric_a.(point) and wb = barycentric_b.(point)
          and wc = barycentric_c.(point) in
          let coordinate_scale = Float.max
              (Float.max (abs_float collision_positions.x.(a))
                (Float.max (abs_float collision_positions.y.(a))
                  (abs_float collision_positions.z.(a))))
              (Float.max
                (Float.max (abs_float collision_positions.x.(b))
                  (Float.max (abs_float collision_positions.y.(b))
                    (abs_float collision_positions.z.(b))))
                (Float.max (abs_float collision_positions.x.(c))
                  (Float.max (abs_float collision_positions.y.(c))
                    (abs_float collision_positions.z.(c))))) in
          let coordinate_scale = if coordinate_scale = 0. then 1.
            else coordinate_scale in
          let hx, hy, hz = if samples = 1 then
              coordinate_scale *. ((wa *. (collision_positions.x.(a)
                /. coordinate_scale)) +. (wb *. (collision_positions.x.(b)
                /. coordinate_scale)) +. (wc *. (collision_positions.x.(c)
                /. coordinate_scale))),
              coordinate_scale *. ((wa *. (collision_positions.y.(a)
                /. coordinate_scale)) +. (wb *. (collision_positions.y.(b)
                /. coordinate_scale)) +. (wc *. (collision_positions.y.(c)
                /. coordinate_scale))),
              coordinate_scale *. ((wa *. (collision_positions.z.(a)
                /. coordinate_scale)) +. (wb *. (collision_positions.z.(b)
                /. coordinate_scale)) +. (wc *. (collision_positions.z.(c)
                /. coordinate_scale)))
            else begin
              let vx, vy, vz = match Option.get directions with
                | Surface_index.Private.Constant_direction { x; y; z } -> x, y, z
                | Surface_index.Private.Per_query_directions values ->
                    values.x.(point), values.y.(point), values.z.(point) in
              let direction_scale = Float.max (abs_float vx)
                  (Float.max (abs_float vy) (abs_float vz)) in
              let dx = vx /. direction_scale and dy = vy /. direction_scale
              and dz = vz /. direction_scale in
              let inverse = 1. /. sqrt ((dx *. dx) +. (dy *. dy) +. (dz *. dz)) in
              let signed_distance = float_of_int direction_signs.(point)
                  *. distances.(point) *. inverse in
              source_positions.x.(point) +. (signed_distance *. dx),
              source_positions.y.(point) +. (signed_distance *. dy),
              source_positions.z.(point) +. (signed_distance *. dz)
            end in
          normal.(0) <- 0.; normal.(1) <- 0.; normal.(2) <- 0.;
          (match normal_planes with
           | None -> ()
           | Some (output_x, output_y, output_z) ->
               if samples > 1 then begin
                 normal.(0) <- output_x.(point); normal.(1) <- output_y.(point);
                 normal.(2) <- output_z.(point);
                 let length = sqrt ((normal.(0) *. normal.(0))
                     +. (normal.(1) *. normal.(1))
                     +. (normal.(2) *. normal.(2))) in
                 if length > 0. then begin
                   normal.(0) <- normal.(0) /. length;
                   normal.(1) <- normal.(1) /. length;
                   normal.(2) <- normal.(2) /. length
                 end
               end else begin
                 let ax = collision_positions.x.(a) /. coordinate_scale
                 and ay = collision_positions.y.(a) /. coordinate_scale
                 and az = collision_positions.z.(a) /. coordinate_scale
                 and bx = collision_positions.x.(b) /. coordinate_scale
                 and by = collision_positions.y.(b) /. coordinate_scale
                 and bz = collision_positions.z.(b) /. coordinate_scale
                 and cx = collision_positions.x.(c) /. coordinate_scale
                 and cy = collision_positions.y.(c) /. coordinate_scale
                 and cz = collision_positions.z.(c) /. coordinate_scale in
                 let abx = bx -. ax and aby = by -. ay and abz = bz -. az
                 and acx = cx -. ax and acy = cy -. ay and acz = cz -. az in
                 normal.(0) <- (aby *. acz) -. (abz *. acy);
                 normal.(1) <- (abz *. acx) -. (abx *. acz);
                 normal.(2) <- (abx *. acy) -. (aby *. acx);
                 let inverse = 1. /. sqrt ((normal.(0) *. normal.(0))
                     +. (normal.(1) *. normal.(1))
                     +. (normal.(2) *. normal.(2))) in
                 normal.(0) <- normal.(0) *. inverse;
                 normal.(1) <- normal.(1) *. inverse;
                 normal.(2) <- normal.(2) *. inverse;
                 output_x.(point) <- normal.(0); output_y.(point) <- normal.(1);
                 output_z.(point) <- normal.(2)
               end);
          let x = source_positions.x.(point)
              +. (scale *. (hx -. source_positions.x.(point)))
              +. (lift *. normal.(0))
          and y = source_positions.y.(point)
              +. (scale *. (hy -. source_positions.y.(point)))
              +. (lift *. normal.(1))
          and z = source_positions.z.(point)
              +. (scale *. (hz -. source_positions.z.(point)))
              +. (lift *. normal.(2)) in
          if not (Float.is_finite x && Float.is_finite y && Float.is_finite z) then errors.(range) <- point
          else begin
            px.(point) <- x; py.(point) <- y; pz.(point) <- z;
            if x <> source_positions.x.(point) || y <> source_positions.y.(point)
                || z <> source_positions.z.(point) then changed.(range) <- true
          end
        end
      done);
    (match Array.find_opt (fun point -> point >= 0) errors with
     | Some point -> fail "non_finite_output" (Printf.sprintf
         "ray transform produced a non-finite position at point %d" point)
     | None -> ());
    let positions = Packed.Float3.Private.of_owned_exn ~x:px ~y:py ~z:pz in
    let needs_interpolation = point_pattern <> None || vertex_pattern <> None
        || primitive_pattern <> None || detail_pattern <> None || match_groups in
    let keep_drivers = source_vertex_numbers_attribute <> None in
    let need_drivers = needs_interpolation || keep_drivers in
    let reserved = "P" :: output_names in
    let numbers_name = match source_vertex_numbers_attribute with
      | Some name -> name
      | None -> unique_attribute_name source collision reserved
          "__rays_ray_vertex_numbers" in
    let weights_name = match source_vertex_weights_attribute with
      | Some name -> name
      | None -> unique_attribute_name source collision (numbers_name :: reserved)
          "__rays_ray_vertex_weights" in
    let attributes = ref [] and protected_attributes = ref [] in
    let protect attribute =
      attributes := attribute :: !attributes;
      protected_attributes := attribute :: !protected_attributes in
    let imported_point_attribute name = match point_pattern with
      | None -> false
      | Some pattern ->
          (match Attribute_pattern.compile pattern with
           | Error _ -> false
           | Ok pattern -> Attribute_pattern.matches pattern name
               && Geometry.find_attribute ~owner:Attribute.Point name collision
                    <> None) in
    (match distance_attribute with
     | None -> ()
     | Some name ->
         let values = existing_point_float name point_count source (-1.) in
         for point = 0 to point_count - 1 do
           if match selection with None -> true | Some group -> Group.mem point group
           then values.(point) <- if primitives.(point) < 0 then -1.
             else distances.(point)
         done;
         protect (Attribute.create_owned ~name ~owner:Attribute.Point
           (Attribute.Float values) |> get_string));
    (match primitive_attribute with
     | None -> ()
     | Some name ->
         let values = existing_point_int name point_count source (-1) in
         for point = 0 to point_count - 1 do
           if match selection with None -> true | Some group -> Group.mem point group
           then values.(point) <- primitives.(point)
         done;
         protect (Attribute.create_owned ~name ~owner:Attribute.Point
           (Attribute.Int values) |> get_string));
    (match normal_attribute, normal_planes with
     | Some name, Some (x, y, z) ->
         let attribute = Attribute.create_owned ~name ~owner:Attribute.Point
           (Attribute.Float3 (Packed.Float3.Private.of_owned_exn ~x ~y ~z))
           |> get_string in
         attributes := attribute :: !attributes;
         if not (imported_point_attribute name) then
           protected_attributes := attribute :: !protected_attributes
     | None, _ -> ()
     | Some _, None -> assert false);
    if need_drivers then begin
      let average_drivers = samples > 1 && combine = Ray_average in
      let offsets = Array.make (point_count + 1) 0 in
      for point = 0 to point_count - 1 do
        if point land 16_383 = 0 then Cancel.check_opt cancel;
        let hits = if average_drivers then hit_counts.(point)
          else if triangles.(point) >= 0 then 1 else 0 in
        if hits > Sys.max_array_length / 3
            || offsets.(point) > Sys.max_array_length - (hits * 3) then
          fail "cardinality_overflow" "ray provenance exceeds array limits";
        offsets.(point + 1) <- offsets.(point) + (hits * 3)
      done;
      let value_count = offsets.(point_count) in
      let numbers = Array.make value_count 0 and weights = Array.make value_count 0. in
      if average_drivers then
        Surface_index.Private.raycast_average_drivers_into ?cancel ?selection
          ~grain surface ~queries:(Geometry.positions source)
          ~directions:(Option.get directions) ~min_distance ~max_distance:maximum
          ~tolerance ~surface_hit ~samples ~jitter_scale ~seed ~direction_signs
          ~hit_counts ~offsets ~vertex_numbers:numbers ~vertex_weights:weights
      else if point_count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
          ~finish:(point_count - 1) (fun point ->
        let triangle = triangles.(point) in
        if triangle >= 0 then begin
          let output = offsets.(point) in
          numbers.(output) <- Surface_index.Private.triangle_vertex surface triangle 0;
          numbers.(output + 1) <- Surface_index.Private.triangle_vertex surface triangle 1;
          numbers.(output + 2) <- Surface_index.Private.triangle_vertex surface triangle 2;
          weights.(output) <- barycentric_a.(point);
          weights.(output + 1) <- barycentric_b.(point);
          weights.(output + 2) <- barycentric_c.(point)
        end);
      let numbers_attribute = Attribute.create_owned ~name:numbers_name
          ~owner:Attribute.Point (Attribute.Int_array
            (Packed.Int_array.Private.create_validated_owned
              ~offsets ~values:numbers)) |> get_string
      and weights_attribute = Attribute.create_owned ~name:weights_name
          ~owner:Attribute.Point (Attribute.Float_array
            (Packed.Float_array.Private.create_validated_owned
              ~offsets:(Array.copy offsets) ~values:weights)) |> get_string in
      attributes := numbers_attribute :: weights_attribute :: !attributes;
      if keep_drivers then protected_attributes := numbers_attribute
          :: weights_attribute :: !protected_attributes
    end;
    let hit_selection = if needs_interpolation || hit_group <> None then
        Some (Group.init ~grain ~owner:Group.Point
          ~name:(Option.value ~default:"__ray_hits" hit_group) point_count
          (fun point -> triangles.(point) >= 0))
      else None in
    let groups = match hit_group, hit_selection with
      | Some _, Some group -> [|group|] | None, _ -> [||]
      | Some _, None -> assert false in
    let changed_positions = Array.exists Fun.id changed in
    let target = if changed_positions then source
        |> Geometry.without_attribute ~owner:Attribute.Point "N"
        |> Geometry.without_attribute ~owner:Attribute.Vertex "N"
      else source in
    let target = Geometry.Private.with_merged_attributes_and_groups_owned
        ~positions ~attributes:(Array.of_list (List.rev !attributes)) ~groups target
        |> get_string in
    let output = if not needs_interpolation then target else begin
        let selection = Option.get hit_selection in
        get_rdk (Attribute_ops.interpolate ?cancel ~grain ~selection
          ~driver:(Attribute_ops.Vertex_weights {
            numbers_attribute = numbers_name; weights_attribute = weights_name })
          ?point_pattern ?vertex_pattern ?primitive_pattern ?detail_pattern
          ~match_groups ~target_owner:Attribute.Point ~attributes:[]
          ~source:collision ~target ())
      end in
    let output = if need_drivers && not keep_drivers then output
        |> Geometry.without_attribute ~owner:Attribute.Point numbers_name
        |> Geometry.without_attribute ~owner:Attribute.Point weights_name
      else output in
    let output = Geometry.with_positions positions output |> get_string in
    let output = List.fold_left (fun output attribute ->
      Geometry.with_attribute attribute output |> get_string)
        output !protected_attributes in
    let output = match hit_group, hit_selection with
      | Some _, Some group -> Geometry.with_group group output |> get_string
      | None, _ -> output | Some _, None -> assert false in
    Ok output
  with
  | Cancel.Cancelled -> Error (Error.make ~operation:"ray" ~code:"cancelled"
      "ray projection was cancelled")
  | Ray_error (code, message) -> Error (Error.make ~operation:"ray" ~code message)
  | Ray_rdk_error error -> Error (Error.make ~hints:(Error.hints error)
      ~operation:"ray" ~code:(Error.code error) (Error.message error))
  | Invalid_argument message ->
      let code =
        if String.starts_with ~prefix:"Surface_index: selected ray origins"
            message then "invalid_query"
        else if String.starts_with
            ~prefix:"Surface_index: selected ray directions" message
        then "invalid_direction"
        else "invalid_parameter" in
      Error (Error.make ~operation:"ray" ~code message)
