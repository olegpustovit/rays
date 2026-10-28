type using = Least_target_point | Closest_target_point
type match_condition = Equal_attribute_values | Unequal_attribute_values
type targeting = Near_points | Specified_points of string

let validate_group label owner_count = function
  | None -> ()
  | Some group when Group.owner group = Group.Point && Group.length group = owner_count -> ()
  | Some _ -> raise (Error.Fail (label ^ " must be a matching point group"))

let scalar_attribute label name geometry =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | None -> raise (Error.Fail (Printf.sprintf "%s point attribute %S is missing" label name))
  | Some attribute -> (
      match Attribute.View.storage attribute with
      | Attribute.Float values -> `Float values
      | Attribute.Int values -> `Int values
      | Attribute.Text values -> `Text values
      | _ ->
          raise
            (Error.Fail
               (Printf.sprintf "%s point attribute %S must be scalar float, integer, or text" label
                  name)))

let numeric_attribute label name geometry =
  match scalar_attribute label name geometry with
  | `Float values -> `Float values
  | `Int values -> `Int values
  | `Text _ ->
      raise
        (Error.Fail
           (Printf.sprintf "%s point attribute %S must be scalar float or integer" label name))

let radius_view label name geometry =
  let values = numeric_attribute label name geometry in
  let count = Geometry.point_count geometry in
  let maximum = ref 0. in
  for point = 0 to count - 1 do
    let value =
      match values with
      | `Float values -> values.(point)
      | `Int values -> float_of_int values.(point)
    in
    if (not (Float.is_finite value)) || value < 0. then
      raise
        (Error.Fail
           (Printf.sprintf "%s radius attribute %S has a non-finite or negative value at point %d"
              label name point));
    if value > !maximum then maximum := value
  done;
  (values, !maximum)

let radius_at values point =
  match values with
  | None -> 0.
  | Some (`Float values, _) -> values.(point)
  | Some (`Int values, _) -> float_of_int values.(point)

type match_views =
  | Match_float of float array * float array
  | Match_int of int array * int array
  | Match_text of string array * string array

let match_views name source target =
  match (scalar_attribute "query" name source, scalar_attribute "target" name target) with
  | `Float query, `Float target -> Match_float (query, target)
  | `Int query, `Int target -> Match_int (query, target)
  | `Text query, `Text target -> Match_text (query, target)
  | _ ->
      raise
        (Error.Fail
           (Printf.sprintf
              "match attribute %S must have identical scalar storage on query and target geometry"
              name))

let compatible condition tolerance views query target =
  let equal =
    match views with
    | None -> true
    | Some (Match_float (queries, targets)) ->
        abs_float (queries.(query) -. targets.(target)) <= tolerance
    | Some (Match_int (queries, targets)) -> queries.(query) = targets.(target)
    | Some (Match_text (queries, targets)) -> String.equal queries.(query) targets.(target)
  in
  match condition with Equal_attribute_values -> equal | Unequal_attribute_values -> not equal

let validate_positions ?cancel label geometry =
  let positions = Packed.Float3.View.view (Geometry.positions geometry) in
  for point = 0 to Geometry.point_count geometry - 1 do
    Cancel.poll cancel point 4095;
    if
      not
        (Float.is_finite positions.x.(point)
        && Float.is_finite positions.y.(point)
        && Float.is_finite positions.z.(point))
    then raise (Error.Fail (Printf.sprintf "%s point %d has a non-finite position" label point))
  done;
  positions

let specified ?cancel ~grain ?queries ?targets attribute_name ~source ~target =
  let source_count = Geometry.point_count source and target_count = Geometry.point_count target in
  ignore (validate_positions ?cancel "query" source);
  ignore (validate_positions ?cancel "target" target);
  let values =
    match Geometry.find_attribute ~owner:Attribute.Point attribute_name source with
    | Some attribute -> (
        match Attribute.View.storage attribute with
        | Attribute.Int values -> values
        | _ ->
            raise
              (Error.Fail
                 (Printf.sprintf "specified target attribute %S must be a point integer"
                    attribute_name)))
    | None ->
        raise
          (Error.Fail
             (Printf.sprintf "specified target point attribute %S is missing" attribute_name))
  in
  let same = Geometry.data_id source = Geometry.data_id target in
  let output = Array.make source_count (-1) in
  if source_count > 0 then
    Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(source_count - 1) (fun query ->
        Cancel.poll cancel query 4095;
        if match queries with None -> true | Some group -> Group.mem query group then
          let destination = values.(query) in
          if
            destination >= 0 && destination < target_count
            && (match targets with None -> true | Some group -> Group.mem destination group)
            && ((not same) || destination <> query)
          then output.(query) <- destination);
  output

let near ?cancel ~grain ?queries ?targets ~using ~tolerance ~metric ~inclusive ?radius_attribute
    ?match_attribute ~match_condition ~match_tolerance ~source ~target () =
  let source_count = Geometry.point_count source and target_count = Geometry.point_count target in
  let query_positions = validate_positions ?cancel "query" source
  and target_positions = validate_positions ?cancel "target" target in
  let query_radius, target_radius =
    match radius_attribute with
    | None -> (None, None)
    | Some name -> (Some (radius_view "query" name source), Some (radius_view "target" name target))
  in
  let matches = Option.map (fun name -> match_views name source target) match_attribute in
  let target_slots =
    match targets with None -> target_count | Some group -> Group.cardinality group
  in
  let output = Array.make source_count (-1) in
  if target_slots > Sys.max_array_length / 2 then
    raise (Error.Fail "target point count exceeds spatial-table limits")
  else if target_slots = 0 || source_count = 0 then output
  else begin
    let min_x = ref infinity
    and min_y = ref infinity
    and min_z = ref infinity
    and max_x = ref neg_infinity
    and max_y = ref neg_infinity
    and max_z = ref neg_infinity in
    let visit_target point =
      min_x := Float.min !min_x target_positions.x.(point);
      min_y := Float.min !min_y target_positions.y.(point);
      min_z := Float.min !min_z target_positions.z.(point);
      max_x := Float.max !max_x target_positions.x.(point);
      max_y := Float.max !max_y target_positions.y.(point);
      max_z := Float.max !max_z target_positions.z.(point)
    in
    (match targets with
    | None ->
        for point = 0 to target_count - 1 do
          visit_target point
        done
    | Some group -> Group.iter visit_target group);
    let max_query_radius = match query_radius with None -> 0. | Some (_, v) -> v
    and max_target_radius = match target_radius with None -> 0. | Some (_, v) -> v in
    let cell_size = tolerance +. max_query_radius +. max_target_radius in
    if not (Float.is_finite cell_size) then
      raise (Error.Fail "tolerance plus point radii exceeds the finite range");
    let exact = cell_size = 0. in
    let scaled_delta left right =
      let delta = right -. left in
      if Float.is_finite delta then delta /. cell_size
      else (right /. cell_size) -. (left /. cell_size)
    in
    let span_x = if exact then 0. else scaled_delta !min_x !max_x
    and span_y = if exact then 0. else scaled_delta !min_y !max_y
    and span_z = if exact then 0. else scaled_delta !min_z !max_z in
    if not exact then begin
      let largest = Float.max span_x (Float.max span_y span_z) in
      if (not (Float.is_finite largest)) || largest > float_of_int (max_int / 4) then
        raise (Error.Fail "snap distance is too small for the target geometry extent")
    end;
    let grid = Point_grid.create ~cells:target_slots ~points:target_count in
    let { Point_grid.cell_head; point_next; _ } = grid in
    let find_cell = Point_grid.find grid in
    let cell_coord value minimum =
      if exact then Point_grid.float_cell value
      else int_of_float (floor (scaled_delta minimum value))
    in
    let insert_target point =
      let cx = cell_coord target_positions.x.(point) !min_x
      and cy = cell_coord target_positions.y.(point) !min_y
      and cz = cell_coord target_positions.z.(point) !min_z in
      Point_grid.insert grid cx cy cz point
    in
    (match targets with
    | None ->
        for point = 0 to target_count - 1 do
          insert_target point
        done
    | Some group -> Group.iter insert_target group);
    let same = Geometry.data_id source = Geometry.data_id target in
    let neighbor_delta = if exact then 0 else 1 in
    if source_count > 0 then
      Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(source_count - 1) (fun query ->
          Cancel.poll cancel query 4095;
          if match queries with None -> true | Some group -> Group.mem query group then begin
            let qx = query_positions.x.(query)
            and qy = query_positions.y.(query)
            and qz = query_positions.z.(query) in
            let query_cell =
              if exact then Some (cell_coord qx !min_x, cell_coord qy !min_y, cell_coord qz !min_z)
              else
                let sx = scaled_delta !min_x qx
                and sy = scaled_delta !min_y qy
                and sz = scaled_delta !min_z qz in
                if
                  (not (Float.is_finite sx && Float.is_finite sy && Float.is_finite sz))
                  || sx < -1. || sy < -1. || sz < -1.
                  || sx > span_x +. 1.
                  || sy > span_y +. 1.
                  || sz > span_z +. 1.
                then None
                else Some (int_of_float (floor sx), int_of_float (floor sy), int_of_float (floor sz))
            in
            match query_cell with
            | None -> ()
            | Some (cx, cy, cz) ->
                let best = ref (-1) and best_distance = ref infinity in
                for dx = -neighbor_delta to neighbor_delta do
                  for dy = -neighbor_delta to neighbor_delta do
                    for dz = -neighbor_delta to neighbor_delta do
                      let cell = find_cell (cx + dx) (cy + dy) (cz + dz) in
                      if cell >= 0 then begin
                        let candidate = ref cell_head.(cell) in
                        while !candidate >= 0 do
                          let target_point = !candidate in
                          if
                            ((not same) || query <> target_point)
                            && compatible match_condition match_tolerance matches query target_point
                          then begin
                            let px = qx -. target_positions.x.(target_point)
                            and py = qy -. target_positions.y.(target_point)
                            and pz = qz -. target_positions.z.(target_point) in
                            let threshold =
                              tolerance +. radius_at query_radius query
                              +. radius_at target_radius target_point
                            in
                            let position_matches, distance =
                              if threshold = 0. then (px = 0. && py = 0. && pz = 0., 0.)
                              else
                                match metric with
                                | Point_clusters.Euclidean ->
                                    let scale =
                                      Float.max (abs_float px)
                                        (Float.max (abs_float py) (abs_float pz))
                                    in
                                    if scale = 0. then (true, 0.)
                                    else if scale > threshold then (false, infinity)
                                    else
                                      let nx = px /. scale
                                      and ny = py /. scale
                                      and nz = pz /. scale in
                                      let ratio = scale /. threshold in
                                      let normalized =
                                        ratio *. ratio *. ((nx *. nx) +. (ny *. ny) +. (nz *. nz))
                                      in
                                      ( (if inclusive then normalized <= 1. else normalized < 1.),
                                        scale *. sqrt ((nx *. nx) +. (ny *. ny) +. (nz *. nz)) )
                                | Point_clusters.Componentwise ->
                                    let maximum =
                                      Float.max (abs_float px)
                                        (Float.max (abs_float py) (abs_float pz))
                                    in
                                    ( (if inclusive then maximum <= threshold
                                       else maximum < threshold),
                                      maximum )
                            in
                            if position_matches then
                              match using with
                              | Least_target_point ->
                                  if !best < 0 || target_point < !best then best := target_point
                              | Closest_target_point ->
                                  if
                                    distance < !best_distance
                                    || distance = !best_distance
                                       && (!best < 0 || target_point < !best)
                                  then begin
                                    best := target_point;
                                    best_distance := distance
                                  end
                          end;
                          candidate := point_next.(target_point)
                        done
                      end
                    done
                  done
                done;
                output.(query) <- !best
          end);
    output
  end

let plan ?cancel ~grain ?queries ?targets ~targeting ~using ~tolerance ~metric ~inclusive
    ?radius_attribute ?match_attribute ~match_condition ~match_tolerance ~source ~target () =
  try
    if grain <= 0 then invalid_arg "Rdk_spatial.Point_snap.fuse: grain must be positive";
    validate_group "query selection" (Geometry.point_count source) queries;
    validate_group "target selection" (Geometry.point_count target) targets;
    if (not (Float.is_finite tolerance)) || tolerance < 0. then
      raise (Error.Fail "snap distance must be finite and non-negative");
    if (not (Float.is_finite match_tolerance)) || match_tolerance < 0. then
      raise (Error.Fail "match tolerance must be finite and non-negative");
    Option.iter
      (fun name ->
        if String.trim name = "" then raise (Error.Fail "radius attribute name must not be empty"))
      radius_attribute;
    Option.iter
      (fun name ->
        if String.trim name = "" then raise (Error.Fail "match attribute name must not be empty"))
      match_attribute;
    (match match_attribute with
    | None when match_condition <> Equal_attribute_values ->
        raise (Error.Fail "unequal match condition requires a match attribute")
    | None when match_tolerance <> 0. ->
        raise (Error.Fail "match tolerance requires a match attribute")
    | None | Some _ -> ());
    Cancel.check_opt cancel;
    Ok
      (match targeting with
      | Specified_points attribute ->
          if radius_attribute <> None || match_attribute <> None then
            raise
              (Error.Fail "specified-point targeting does not accept radius or match attributes");
          if String.trim attribute = "" then
            raise (Error.Fail "specified target attribute name must not be empty");
          specified ?cancel ~grain ?queries ?targets attribute ~source ~target
      | Near_points ->
          near ?cancel ~grain ?queries ?targets ~using ~tolerance ~metric ~inclusive
            ?radius_attribute ?match_attribute ~match_condition ~match_tolerance ~source ~target ())
  with Error.Fail message | Invalid_argument message ->
    Error ("Rdk_spatial.Point_snap.fuse: " ^ message)
