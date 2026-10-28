
let operation = "boolean_detect"

let error code message = Error (Error.make ~operation ~code message)

let sort_range ?cancel values first last =
  let swap left right =
    if left <> right then begin
      let value = values.(left) in values.(left) <- values.(right);
      values.(right) <- value
    end in
  let rec sift root count =
    let child = (root * 2) + 1 in
    if child < count then begin
      let selected = if child + 1 < count
          && values.(first + child) < values.(first + child + 1)
        then child + 1 else child in
      if values.(first + root) < values.(first + selected) then begin
        swap (first + root) (first + selected); sift selected count
      end
    end in
  let count = last - first in
  for root = (count / 2) - 1 downto 0 do
    if root land 4095 = 0 then Cancel.check_opt cancel;
    sift root count
  done;
  for remaining = count - 1 downto 1 do
    if remaining land 4095 = 0 then Cancel.check_opt cancel;
    swap first (first + remaining); sift 0 remaining
  done

let install_outputs ?cancel ~grain ~intersecting_group ~intersections_attribute
    ~count_attribute ~raw_counts ~raw_values geometry =
  let primitive_count = Geometry.primitive_count geometry in
  let raw_offsets = Array.make (primitive_count + 1) 0 in
  for primitive = 0 to primitive_count - 1 do
    if primitive land 4095 = 0 then Cancel.check_opt cancel;
    raw_offsets.(primitive + 1) <- raw_offsets.(primitive) + raw_counts.(primitive)
  done;
  if primitive_count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
      ~finish:(primitive_count - 1) (fun primitive ->
    if primitive land 4095 = 0 then Cancel.check_opt cancel;
    sort_range ?cancel raw_values raw_offsets.(primitive) raw_offsets.(primitive + 1));
  let unique_counts = Array.make primitive_count 0 in
  if primitive_count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
      ~finish:(primitive_count - 1) (fun primitive ->
    if primitive land 4095 = 0 then Cancel.check_opt cancel;
    let first = raw_offsets.(primitive) and last = raw_offsets.(primitive + 1) in
    if first < last then begin
      unique_counts.(primitive) <- 1;
      for slot = first + 1 to last - 1 do
        if raw_values.(slot) <> raw_values.(slot - 1) then
          unique_counts.(primitive) <- unique_counts.(primitive) + 1
      done
    end
  );
  let offsets = Array.make (primitive_count + 1) 0 in
  for primitive = 0 to primitive_count - 1 do
    if primitive land 4095 = 0 then Cancel.check_opt cancel;
    offsets.(primitive + 1) <- offsets.(primitive) + unique_counts.(primitive)
  done;
  let values = Array.make offsets.(primitive_count) 0 in
  if primitive_count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
      ~finish:(primitive_count - 1) (fun primitive ->
    if primitive land 4095 = 0 then Cancel.check_opt cancel;
    let first = raw_offsets.(primitive) and last = raw_offsets.(primitive + 1)
    and output = ref offsets.(primitive) in
    for slot = first to last - 1 do
      if slot = first || raw_values.(slot) <> raw_values.(slot - 1) then begin
        values.(!output) <- raw_values.(slot); incr output
      end
    done
  );
  let with_group = match intersecting_group with
    | None -> Ok geometry
    | Some name -> Geometry.with_group
        (Group.init ~grain ~owner:Group.Primitive ~name primitive_count
          (fun primitive ->
            if primitive land 4095 = 0 then Cancel.check_opt cancel;
            unique_counts.(primitive) > 0)) geometry in
  Result.bind with_group (fun geometry ->
  let with_intersections = match intersections_attribute with
    | None -> Ok geometry
    | Some name ->
        Result.bind (Packed.Int_array.create_owned ~offsets ~values)
          (fun values -> Result.bind (Attribute.create_owned
            ~owner:Attribute.Primitive ~name (Attribute.Int_array values))
            (fun attribute -> Geometry.with_attribute attribute geometry)) in
  Result.bind with_intersections (fun geometry -> match count_attribute with
    | None -> Ok geometry
    | Some name -> Result.bind (Attribute.create_owned ~owner:Attribute.Primitive
        ~name (Attribute.Int unique_counts)) (fun attribute ->
      Geometry.with_attribute attribute geometry)))

let detect_pairs ?cancel ~grain ~tolerance ~include_coplanar ~self
    source_surface source_geometry collision_surface collision_geometry =
  let source_triangles, collision_triangles = if self then
      Surface_index.View.overlapping_self_triangle_pairs ?cancel ~grain
        ~tolerance source_surface
    else Surface_index.View.overlapping_triangle_pairs ?cancel ~grain
        ~tolerance source_surface collision_surface in
  let candidates = Array.length source_triangles in
  let keep = Bytes.make ((candidates + 7) / 8) '\000' in
  let source_narrow = Triangle_intersection.surface source_surface source_geometry
  and collision_narrow = Triangle_intersection.surface collision_surface
      collision_geometry in
  let byte_count = Bytes.length keep in
  let byte_grain = max 1 (grain / 8) in
  let range_count = (byte_count / byte_grain)
      + if byte_count mod byte_grain = 0 then 0 else 1 in
  if range_count > 0 then Parallel.for_ ~chunk_size:1
      ~start:0 ~finish:(range_count - 1) (fun range ->
    let scratch = Array.make 26 0. and point_info = Array.make 10 0
    and events = Array.make
        (Triangle_intersection.max_events * Triangle_intersection.event_stride) 0. in
    let first_byte = range * byte_grain
    and last_byte = min byte_count ((range + 1) * byte_grain) in
    for byte = first_byte to last_byte - 1 do
      let first = byte * 8 in
      let last = min candidates (first + 8) and bits = ref 0 in
      for candidate = first to last - 1 do
        if candidate land 4095 = 0 then Cancel.check_opt cancel;
        if Triangle_intersection.events_into ~scratch ~point_info ~events ~self
            ~tolerance ~include_coplanar source_narrow source_triangles.(candidate)
            collision_narrow collision_triangles.(candidate) > 0 then
          bits := !bits lor (1 lsl (candidate - first))
      done;
      Bytes.unsafe_set keep byte (Char.unsafe_chr !bits)
    done);
  let primitive_count = Geometry.primitive_count source_geometry in
  let raw_counts = Array.make primitive_count 0 and exact = ref 0 in
  for candidate = 0 to candidates - 1 do
    if candidate land 4095 = 0 then Cancel.check_opt cancel;
    if Support.Bits.mem keep candidate then begin
      let source_primitive = Surface_index.View.triangle_primitive
          source_surface source_triangles.(candidate) in
      raw_counts.(source_primitive) <- raw_counts.(source_primitive) + 1;
      if self then begin
        let collision_primitive = Surface_index.View.triangle_primitive
            collision_surface collision_triangles.(candidate) in
        raw_counts.(collision_primitive) <- raw_counts.(collision_primitive) + 1
      end;
      incr exact
    end
  done;
  let raw_offsets = Array.make (primitive_count + 1) 0 in
  for primitive = 0 to primitive_count - 1 do
    if primitive land 4095 = 0 then Cancel.check_opt cancel;
    if raw_counts.(primitive) > Sys.max_array_length - raw_offsets.(primitive) then
      invalid_arg "Boolean Detect intersection cardinality exceeds array limits";
    raw_offsets.(primitive + 1) <- raw_offsets.(primitive) + raw_counts.(primitive)
  done;
  let multiplicity = if self then 2 else 1 in
  if !exact > Sys.max_array_length / multiplicity then
    invalid_arg "Boolean Detect intersection cardinality exceeds array limits";
  let entry_count = !exact * multiplicity in
  let raw_values = Array.make entry_count 0 and cursors = Array.copy raw_offsets in
  for candidate = 0 to candidates - 1 do
    if candidate land 4095 = 0 then Cancel.check_opt cancel;
    if Support.Bits.mem keep candidate then begin
      let source_primitive = Surface_index.View.triangle_primitive
          source_surface source_triangles.(candidate)
      and collision_primitive = Surface_index.View.triangle_primitive
          collision_surface collision_triangles.(candidate) in
      raw_values.(cursors.(source_primitive)) <- collision_primitive;
      cursors.(source_primitive) <- cursors.(source_primitive) + 1;
      if self then begin
        raw_values.(cursors.(collision_primitive)) <- source_primitive;
        cursors.(collision_primitive) <- cursors.(collision_primitive) + 1
      end
    end
  done;
  raw_counts, raw_values

let duplicate_name names =
  let names = List.sort String.compare names in
  let rec loop = function
    | left :: (right :: _ as rest) -> String.equal left right || loop rest
    | [] | [_] -> false in
  loop names

let run ?cancel ~grain ?source_primitives ?collision_primitives ~tolerance
    ~include_coplanar ~intersecting_group ~intersections_attribute
    ~count_attribute ~self_intersecting_group ~self_intersections_attribute
    ~self_count_attribute ~collision geometry =
  try
    if grain <= 0 then error "invalid_parameter" "grain must be positive"
    else if not (Float.is_finite tolerance) || tolerance < 0. then
      error "invalid_parameter" "tolerance must be finite and non-negative"
    else if intersecting_group = None && intersections_attribute = None
        && count_attribute = None && self_intersecting_group = None
        && self_intersections_attribute = None && self_count_attribute = None then
      error "invalid_parameter" "at least one output must be requested"
    else begin
      let names = List.filter_map Fun.id
          [intersections_attribute; count_attribute; self_intersections_attribute;
           self_count_attribute] in
      let groups = List.filter_map Fun.id
          [intersecting_group; self_intersecting_group] in
      if List.exists (fun name -> String.trim name = "") names
          || List.exists (fun name -> String.trim name = "") groups
      then error "invalid_parameter" "output names must not be empty"
      else if duplicate_name names then
        error "invalid_parameter" "Boolean Detect attribute outputs must have distinct names"
      else if duplicate_name groups then
        error "invalid_parameter" "Boolean Detect group outputs must have distinct names"
      else
        Result.bind (Surface_index.create ?cancel ~grain
          ?primitives:source_primitives geometry) (fun source_surface ->
        let cross_requested = intersecting_group <> None
            || intersections_attribute <> None || count_attribute <> None in
        let self_requested = self_intersecting_group <> None
            || self_intersections_attribute <> None || self_count_attribute <> None in
        let with_cross = if not cross_requested then Ok geometry else
          Result.bind (Surface_index.create ?cancel ~grain
            ?primitives:collision_primitives collision) (fun collision_surface ->
          let raw_counts, raw_values = detect_pairs ?cancel ~grain ~tolerance
              ~include_coplanar ~self:false source_surface geometry
              collision_surface collision in
          match install_outputs ?cancel ~grain ~intersecting_group
              ~intersections_attribute ~count_attribute ~raw_counts ~raw_values
              geometry with
          | Ok geometry -> Ok geometry
          | Error message -> error "invalid_output" message) in
        Result.bind with_cross (fun geometry ->
          if not self_requested then Ok geometry else
          let raw_counts, raw_values = detect_pairs ?cancel ~grain ~tolerance
              ~include_coplanar ~self:true source_surface geometry
              source_surface geometry in
          match install_outputs ?cancel ~grain
              ~intersecting_group:self_intersecting_group
              ~intersections_attribute:self_intersections_attribute
              ~count_attribute:self_count_attribute ~raw_counts ~raw_values geometry with
          | Ok geometry -> Ok geometry
          | Error message -> error "invalid_output" message))
    end
  with
  | Cancel.Cancelled -> error "cancelled" "Boolean Detect was cancelled"
  | Invalid_argument message -> error "invalid_geometry" message

let run_checked ?cancel ?(grain = 16_384) ?source_primitives
    ?collision_primitives ?(tolerance = 0.) ?(include_coplanar = true)
    ?(intersecting_group = Some "boolean_intersections")
    ?intersections_attribute ?count_attribute ?self_intersecting_group
    ?self_intersections_attribute ?self_count_attribute ~collision geometry =
  run ?cancel ~grain ?source_primitives ?collision_primitives
    ~tolerance ~include_coplanar ~intersecting_group ~intersections_attribute
    ~count_attribute ~self_intersecting_group ~self_intersections_attribute
    ~self_count_attribute ~collision geometry
