
type method_ = Ray_minimum_distance | Ray_project

type direction =
  | Ray_vector of Rays_math.Vec3.t
  | Ray_normal
  | Ray_attribute of string



type combine =
  | Ray_average
  | Ray_median
  | Ray_shortest
  | Ray_longest

let fail code message = Error.fail_with ~operation:"ray" ~code message
let get_string = function Ok value -> value | Error message -> fail "invalid_output" message
let get_rdk = function Ok value -> value | Error error -> raise (Error.Rdk error)

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
  | Some (Element_selection.Selected_points group) -> Some group
  | Some _ ->
      let index = Some (Topology_index.create ?cancel topology) in
      Some (Group.init ~grain ~owner:Group.Point ~name:"__ray_selection"
        (Geometry.point_count geometry) (fun point ->
          Cancel.poll cancel point 4095;
          Deform.point_selected selection index point))

let direction_planes ?cancel ~grain source = function
  | Ray_vector value ->
      if not (Float.is_finite value.Rays_math.Vec3.x && Float.is_finite value.y && Float.is_finite value.z) then
        fail "invalid_direction" "ray direction vector must be finite";
      if value.x = 0. && value.y = 0. && value.z = 0. then
        fail "invalid_direction" "ray direction vector must be non-zero";
      Surface_index.View.Constant_direction {
        x = value.x; y = value.y; z = value.z }
  | Ray_attribute name ->
      if String.trim name = "" then
        fail "invalid_direction" "ray direction attribute name must not be empty";
      let values = match Deform.point_vector_attribute "Rdk.Ray.ray" name source with
        | Ok values -> values | Error message -> fail "invalid_direction" message in
      let packed = Packed.Float3.View.of_shared_exn
          ~x:values.x ~y:values.y ~z:values.z in
      Surface_index.View.Per_query_directions
        (Packed.Float3.View.view packed)
  | Ray_normal ->
      let values = match Deform.resolve_directions ?cancel ~grain source with
        | Ok values -> values | Error message -> fail "invalid_direction" message in
      let packed = Packed.Float3.View.of_shared_exn
          ~x:values.x ~y:values.y ~z:values.z in
      Surface_index.View.Per_query_directions
        (Packed.Float3.View.view packed)

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
      | Ray_shortest -> sample_distance float_results !shortest, !shortest
      | Ray_longest -> sample_distance float_results !longest, !longest
      | Ray_median ->
          sort_samples float_results order !count sort_state;
          let selected = order.(!count / 2) in
          sample_distance float_results selected, selected
      | Ray_average ->
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
   | Surface_index.View.Per_query_directions values when Array.length values.x <> query_count
       || Array.length values.y <> query_count
       || Array.length values.z <> query_count ->
       invalid_arg "Surface_index: ray direction count does not match queries"
   | Surface_index.View.Constant_direction _ | Surface_index.View.Per_query_directions _ -> ());
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
      Cancel.poll cancel query 4095;
      primitives.(query) <- -1; triangles.(query) <- -1;
      barycentric_a.(query) <- 0.; barycentric_b.(query) <- 0.;
      barycentric_c.(query) <- 0.; distances.(query) <- Float.infinity;
      direction_signs.(query) <- 0; hit_counts.(query) <- 0;
      if match selection with None -> true | Some group -> Group.mem query group then begin
        let ox = queries.x.(query) and oy = queries.y.(query)
        and oz = queries.z.(query) in
        if not (Float.is_finite ox && Float.is_finite oy && Float.is_finite oz) then
          invalid_arg "Surface_index: selected ray origins must be finite";
        let vx, vy, vz = Surface_index.View.direction_at directions query in
        let dx, dy, dz = Surface_index.View.normalized_direction vx vy vz in
        let directions_to_query = match direction_mode with
          | Surface_index.Ray_forward | Surface_index.Ray_reverse -> 1
          | Surface_index.Ray_bidirectional_closest | Surface_index.Ray_bidirectional_farthest -> 2 in
        for side = 0 to directions_to_query - 1 do
          let sign = match direction_mode with
            | Surface_index.Ray_reverse -> -1.
            | Surface_index.Ray_forward -> 1.
            | Surface_index.Ray_bidirectional_closest | Surface_index.Ray_bidirectional_farthest ->
                if side = 0 then 1. else -1. in
          let base = side * samples in
          for sample = 0 to samples - 1 do
            Cancel.poll cancel sample 63;
            jittered_direction seed query sample jitter_scale
              (sign *. dx) (sign *. dy) (sign *. dz) direction;
            Surface_index.View.ray_query_once value ox oy oz direction.(0) direction.(1) direction.(2)
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
            | Surface_index.Ray_bidirectional_closest ->
                if combined_distances.(1) < combined_distances.(0) then 1 else 0
            | Surface_index.Ray_bidirectional_farthest ->
                if combined_distances.(1) > combined_distances.(0) then 1 else 0
            | Surface_index.Ray_forward | Surface_index.Ray_reverse -> assert false in
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
            | Surface_index.Ray_reverse -> -1 | Surface_index.Ray_forward -> 1
            | Surface_index.Ray_bidirectional_closest | Surface_index.Ray_bidirectional_farthest ->
                if chosen = 0 then 1 else -1);
          if requested_normals then begin
            let nx = ref 0. and ny = ref 0. and nz = ref 0. in
            if combine = Ray_average then begin
              let base = chosen * samples in
              for sample = 0 to samples - 1 do
                let result = base + sample in
                if sample_hit integer_results result then begin
                  Surface_index.View.triangle_normal_into value integer_results.((result * 2) + 1)
                    normal;
                  nx := !nx +. normal.(0); ny := !ny +. normal.(1);
                  nz := !nz +. normal.(2)
                end
              done;
              let inverse = 1. /. float_of_int combined_counts.(chosen) in
              nx := !nx *. inverse; ny := !ny *. inverse; nz := !nz *. inverse
            end else begin
              Surface_index.View.triangle_normal_into value triangles.(query) normal;
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
      Cancel.poll cancel query 4095;
      if direction_signs.(query) <> 0
          && (match selection with None -> true | Some group -> Group.mem query group)
      then begin
        let ox = queries.x.(query) and oy = queries.y.(query)
        and oz = queries.z.(query) in
        let vx, vy, vz = Surface_index.View.direction_at directions query in
        let dx, dy, dz = Surface_index.View.normalized_direction vx vy vz in
        let sign = float_of_int direction_signs.(query)
        and scale = 1. /. float_of_int hit_counts.(query) in
        let at = ref offsets.(query) in
        for sample = 0 to samples - 1 do
          Cancel.poll cancel sample 63;
          jittered_direction seed query sample jitter_scale
            (sign *. dx) (sign *. dy) (sign *. dz) direction;
          Surface_index.View.ray_query_once value ox oy oz direction.(0) direction.(1) direction.(2)
            min_distance max_distance tolerance surface_hit scratch node_stack
            integer_results float_results 0;
          if integer_results.(0) >= 0 then begin
            let triangle = integer_results.(1) and floating = 0 in
            vertex_numbers.(!at) <- Surface_index.View.triangle_vertex value triangle 0;
            vertex_numbers.(!at + 1) <- Surface_index.View.triangle_vertex value triangle 1;
            vertex_numbers.(!at + 2) <- Surface_index.View.triangle_vertex value triangle 2;
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

let existing_point_float name count geometry default =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | None -> Array.make count default
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Float values -> Array.copy values
       | _ -> fail "invalid_output" (Printf.sprintf
           "point output attribute %S must have float storage" name))

let existing_point_int name count geometry default =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | None -> Array.make count default
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Int values -> Array.copy values
       | _ -> fail "invalid_output" (Printf.sprintf
           "point output attribute %S must have integer storage" name))

let existing_point_float3 name count geometry =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | None -> Array.make count 0., Array.make count 0., Array.make count 0.
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Float3 values ->
           let values = Packed.Float3.View.view values in
           Array.copy values.x, Array.copy values.y, Array.copy values.z
       | _ -> fail "invalid_output" (Printf.sprintf
           "point output attribute %S must have float3 storage" name))

type params = {
  grain : int;
  selection : Element_selection.t option;
  collision_primitives : Group.t option;
  method_ : method_;
  direction : direction;
  direction_mode : Surface_index.ray_direction_mode;
  surface_hit : Surface_index.ray_surface_hit;
  samples : int;
  jitter_scale : float;
  seed : int;
  combine : combine;
  min_distance : float;
  max_distance : float option;
  tolerance : float;
  scale : float;
  lift : float;
  distance_attribute : string option;
  primitive_attribute : string option;
  source_vertex_numbers_attribute : string option;
  source_vertex_weights_attribute : string option;
  hit_group : string option;
  normal_attribute : string option;
  point_pattern : string option;
  vertex_pattern : string option;
  primitive_pattern : string option;
  detail_pattern : string option;
  match_groups : bool;
}
let run ?cancel { grain; selection; collision_primitives; method_; direction;
    direction_mode; surface_hit; samples; jitter_scale; seed; combine;
    min_distance; max_distance; tolerance; scale; lift; distance_attribute;
    primitive_attribute; source_vertex_numbers_attribute;
    source_vertex_weights_attribute; hit_group; normal_attribute; point_pattern;
    vertex_pattern; primitive_pattern; detail_pattern; match_groups }
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
         Surface_index.View.closest_many_into ?cancel ?selection ~grain surface
           ~queries:(Geometry.positions source) ~max_distance_squared:maximum_squared
           ~primitives ~triangles ~barycentric_a ~barycentric_b ~barycentric_c
           ~distances_squared:distances;
         Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(point_count - 1)
           (fun point -> if primitives.(point) >= 0 then
             distances.(point) <- sqrt distances.(point))
     | Ray_project ->
         let directions = Option.get directions in
         if samples = 1 then
           Surface_index.View.raycast_many_into ?cancel ?selection ~grain surface
             ~queries:(Geometry.positions source) ~directions
             ~min_distance ~max_distance:maximum ~tolerance ~direction_mode
             ~surface_hit ~primitives ~triangles ~barycentric_a ~barycentric_b
             ~barycentric_c ~distances
         else
           let normal_x, normal_y, normal_z = match normal_planes with
             | None -> None, None, None
             | Some (x, y, z) -> Some x, Some y, Some z in
           raycast_samples_into ?cancel ?selection ~grain
             surface ~queries:(Geometry.positions source) ~directions
             ~min_distance ~max_distance:maximum ~tolerance ~direction_mode
             ~surface_hit ~samples ~jitter_scale ~seed ~combine ~primitives
             ~triangles ~barycentric_a ~barycentric_b ~barycentric_c ~distances
             ~direction_signs ~hit_counts ~normal_x ~normal_y ~normal_z);
    let source_positions = Packed.Float3.View.view (Geometry.positions source)
    and collision_positions = Packed.Float3.View.view
        (Geometry.positions collision)
    and collision_topology = Topology.View.view (Geometry.topology collision) in
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
        Cancel.poll cancel point 4095;
        let triangle = triangles.(point) in
        if triangle >= 0 then begin
          let va = Surface_index.View.triangle_vertex surface triangle 0
          and vb = Surface_index.View.triangle_vertex surface triangle 1
          and vc = Surface_index.View.triangle_vertex surface triangle 2 in
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
                | Surface_index.View.Constant_direction { x; y; z } -> x, y, z
                | Surface_index.View.Per_query_directions values ->
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
    let positions = Packed.Float3.View.of_owned_exn ~x:px ~y:py ~z:pz in
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
           (Attribute.Float3 (Packed.Float3.View.of_owned_exn ~x ~y ~z))
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
        Cancel.poll cancel point 16_383;
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
        raycast_average_drivers_into ?cancel ?selection
          ~grain surface ~queries:(Geometry.positions source)
          ~directions:(Option.get directions) ~min_distance ~max_distance:maximum
          ~tolerance ~surface_hit ~samples ~jitter_scale ~seed ~direction_signs
          ~hit_counts ~offsets ~vertex_numbers:numbers ~vertex_weights:weights
      else if point_count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
          ~finish:(point_count - 1) (fun point ->
        let triangle = triangles.(point) in
        if triangle >= 0 then begin
          let output = offsets.(point) in
          numbers.(output) <- Surface_index.View.triangle_vertex surface triangle 0;
          numbers.(output + 1) <- Surface_index.View.triangle_vertex surface triangle 1;
          numbers.(output + 2) <- Surface_index.View.triangle_vertex surface triangle 2;
          weights.(output) <- barycentric_a.(point);
          weights.(output + 1) <- barycentric_b.(point);
          weights.(output + 2) <- barycentric_c.(point)
        end);
      let numbers_attribute = Attribute.create_owned ~name:numbers_name
          ~owner:Attribute.Point (Attribute.Int_array
            (Packed.Int_array.View.create_validated_owned
              ~offsets ~values:numbers)) |> get_string
      and weights_attribute = Attribute.create_owned ~name:weights_name
          ~owner:Attribute.Point (Attribute.Float_array
            (Packed.Float_array.View.create_validated_owned
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
    let target = Geometry.View.with_merged_owned
        ~positions ~attributes:(Array.of_list (List.rev !attributes)) ~groups target
        |> get_string in
    let output = if not needs_interpolation then target else begin
        let selection = Option.get hit_selection in
        get_rdk (Attribute_ops.interpolate { cancel; grain = Some grain; selection = Some selection;
          driver = Some (Attribute_ops.Vertex_weights { numbers_attribute = numbers_name;
            weights_attribute = weights_name }); compute_weights = None; point_pattern; vertex_pattern; primitive_pattern; detail_pattern;
          match_groups; pre_scale = None; normalize_weights = None; threshold = None; blend = None;
          unmatched = Keep_target; target_owner = Attribute.Point; attributes = [];
          source = collision; target } ())
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
  | Error.Rdk error -> Error (Error.make ~hints:(Error.hints error)
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
