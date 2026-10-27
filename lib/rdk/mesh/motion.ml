open Rays_math

type rest_mode = Store_rest | Extract_rest | Swap_rest
type rest_normals = No_rest_normals | Rest_normals_if_present | Rest_normals_always

type velocity_approximation = Backward_difference | Central_difference | Forward_difference
type velocity_initialization =
  | Compute_from_deformation
  | Keep_incoming
  | Set_value of Rays_math.Vec3.t
  | From_attribute of { name : string; scale : float }
type velocity_unmatched = Velocity_unmatched_error | Velocity_unmatched_zero

let fail operation code message =
  Error (Error.make ~operation ~code message)

let valid_name value = String.trim value <> ""

let point_float3 operation name geometry =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | None -> Ok None
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Float3 values -> Ok (Some values)
       | _ -> fail operation "invalid_attribute"
           (Printf.sprintf "point attribute %S must have float3 storage" name))

let make_point_float3 operation name values =
  match Attribute.create_owned ~name ~owner:Attribute.Point
      (Attribute.Float3 values) with
  | Ok attribute -> Ok attribute
  | Error message -> fail operation "invalid_attribute" message

let computed_point_normals ?cancel ~grain geometry =
  match Normal_ops.run ?cancel ~grain ~owner:Attribute.Point geometry with
  | Error error -> fail "rest_position" "normal_computation_failed"
      (Error.message error)
  | Ok geometry ->
      (match point_float3 "rest_position" "N" geometry with
       | Ok (Some values) -> Ok values
       | Ok None -> fail "rest_position" "normal_computation_failed"
           "point normal computation did not create N"
       | Error _ as error -> error)

let source_normals ?cancel ~grain policy ~normal_attribute geometry =
  match policy with
  | No_rest_normals -> Ok None
  | Rest_normals_if_present -> point_float3 "rest_position" normal_attribute geometry
  | Rest_normals_always ->
      (match point_float3 "rest_position" normal_attribute geometry with
       | Ok (Some _ as values) -> Ok values
       | Ok None when String.equal normal_attribute "N" ->
           Result.map Option.some (computed_point_normals ?cancel ~grain geometry)
       | Ok None -> fail "rest_position" "missing_attribute"
           (Printf.sprintf "cannot compute nonstandard normal attribute %S"
              normal_attribute)
       | Error _ as error -> error)

let rest_position_raw ?cancel ~grain ?reference ~rest_attribute ~normals
    ~normal_attribute ~rest_normal_attribute mode geometry =
  if grain <= 0 then invalid_arg "Rdk.Motion.rest_position: grain must be positive";
  Cancel.check_opt cancel;
  if (not (valid_name rest_attribute)) && normals = No_rest_normals then Ok geometry
  else if String.equal rest_attribute "P" then
    fail "rest_position" "invalid_attribute" "rest attribute cannot be named P"
  else if normals <> No_rest_normals
      && (not (valid_name normal_attribute)
          || not (valid_name rest_normal_attribute)
          || String.equal normal_attribute "P"
          || String.equal rest_normal_attribute "P") then
    fail "rest_position" "invalid_attribute" "normal attribute names must be non-empty and cannot be P"
  else
    let stored = if valid_name rest_attribute then
        Geometry.find_attribute ~owner:Attribute.Point rest_attribute geometry
      else None in
    let actual_mode = match mode, stored with
      | (Extract_rest | Swap_rest), None -> Store_rest
      | value, _ -> value in
    let source = Option.value ~default:geometry reference in
    if actual_mode = Store_rest
       && Geometry.point_count source <> Geometry.point_count geometry then
      fail "rest_position" "cardinality_mismatch"
        "reference and destination must have the same point count"
    else
      let stored_positions = match stored with
        | None -> Ok None
        | Some attribute ->
            (match Attribute.Private.storage attribute with
             | Attribute.Float3 values -> Ok (Some values)
             | _ -> fail "rest_position" "invalid_attribute"
                 (Printf.sprintf "point attribute %S must have float3 storage"
                    rest_attribute)) in
      Result.bind stored_positions (fun stored_positions ->
        let positions, rest_positions = match actual_mode with
          | Store_rest -> Geometry.positions geometry,
              (if valid_name rest_attribute then Some (Geometry.positions source)
               else None)
          | Extract_rest -> Option.get stored_positions, None
          | Swap_rest -> Option.get stored_positions,
              (if valid_name rest_attribute then Some (Geometry.positions geometry)
               else None) in
        Result.bind (source_normals ?cancel ~grain normals ~normal_attribute
            (if actual_mode = Store_rest then source else geometry))
          (fun current_normals ->
            Result.bind (point_float3 "rest_position" rest_normal_attribute geometry)
              (fun stored_normals ->
                let output_normal, output_rest_normal = match actual_mode with
                  | Store_rest -> None, current_normals
                  | Extract_rest ->
                      (match stored_normals with
                       | Some value -> Some value, None
                       | None -> None, None)
                  | Swap_rest -> stored_normals, current_normals in
                let additions = ref [] in
                let add name values =
                  Result.bind (make_point_float3 "rest_position" name values)
                    (fun attribute -> additions := attribute :: !additions; Ok ()) in
                Result.bind
                  (match rest_positions with None -> Ok ()
                   | Some values -> add rest_attribute values)
                  (fun () -> Result.bind
                    (match output_rest_normal with None -> Ok ()
                     | Some values -> add rest_normal_attribute values)
                    (fun () -> Result.bind
                      (match output_normal with None -> Ok ()
                       | Some values -> add normal_attribute values)
                      (fun () ->
                        match Geometry.Private.with_merged_attributes_and_groups_owned
                            ~positions ~attributes:(Array.of_list !additions)
                            ~groups:[||] geometry with
                        | Ok output -> Ok output
                        | Error message -> fail "rest_position"
                            "invalid_geometry" message))))))

let rest_position ?cancel ?(grain = 16_384) ?reference
    ?(rest_attribute = "rest") ?(normals = No_rest_normals)
    ?(normal_attribute = "N") ?(rest_normal_attribute = "restN") mode geometry =
  try rest_position_raw ?cancel ~grain ?reference ~rest_attribute ~normals
      ~normal_attribute ~rest_normal_attribute mode geometry with
  | Cancel.Cancelled -> fail "rest_position" "cancelled"
      "rest position operation was cancelled"
  | Invalid_argument message -> fail "rest_position" "invalid_argument" message

let check_point_group count = function
  | None -> Ok ()
  | Some group when Group.owner group <> Group.Point ->
      fail "point_velocity" "invalid_group" "selection must own points"
  | Some group when Group.length group <> count ->
      fail "point_velocity" "cardinality_mismatch"
        "selection length must equal the current point count"
  | Some _ -> Ok ()

let selected group point = match group with None -> true | Some value -> Group.mem point value

let copy_or_zero count = function
  | None -> Array.make count 0., Array.make count 0., Array.make count 0.
  | Some values ->
      let values = Packed.Float3.Private.view values in
      Array.copy values.x, Array.copy values.y, Array.copy values.z

let check_finite3 operation x y z =
  if not (Float.is_finite x && Float.is_finite y && Float.is_finite z) then
    invalid_arg (operation ^ ": non-finite vector component")

let next_power_of_two value =
  let result = ref 16 in
  while !result < value do
    if !result > Sys.max_array_length / 2 then
      invalid_arg "Rdk.Motion.point_velocity: match table too large";
    result := !result lsl 1
  done;
  !result

let int_hash value =
  let open Int64 in
  let x = logxor (of_int value) (shift_right_logical (of_int value) 33) in
  let x = mul x 0xff51afd7ed558ccdL in
  let x = logxor x (shift_right_logical x 33) in
  to_int x

let int_mapping ?cancel ~grain current reference =
  if Array.length reference > Sys.max_array_length / 2 then
    invalid_arg "Rdk.Motion.point_velocity: match table too large";
  let capacity = next_power_of_two (max 16 (Array.length reference * 2)) in
  let mask = capacity - 1 and used = Bytes.make capacity '\000'
  and keys = Array.make capacity 0 and values = Array.make capacity 0 in
  let locate key =
    let slot = ref (int_hash key land mask) in
    while Bytes.unsafe_get used !slot <> '\000' && keys.(!slot) <> key do
      slot := (!slot + 1) land mask
    done;
    !slot in
  let duplicate = ref None in
  for index = 0 to Array.length reference - 1 do
    if index land 4095 = 0 then Cancel.check_opt cancel;
    let key = reference.(index) and slot = locate reference.(index) in
    if Bytes.unsafe_get used slot <> '\000' then duplicate := Some key
    else begin Bytes.unsafe_set used slot '\001'; keys.(slot) <- key;
      values.(slot) <- index end
  done;
  match !duplicate with
  | Some key -> Error (Printf.sprintf "duplicate reference match key %d" key)
  | None ->
      let mapping = Array.make (Array.length current) (-1) in
      if Array.length current > 0 then Parallel.for_ ~chunk_size:grain ~start:0
          ~finish:(Array.length current - 1) (fun index ->
        if index land 4095 = 0 then Cancel.check_opt cancel;
        let slot = locate current.(index) in
        if Bytes.unsafe_get used slot <> '\000' then
          mapping.(index) <- values.(slot));
      Ok mapping

let text_mapping ?cancel ~grain current reference =
  let table = Hashtbl.create (Array.length reference) and duplicate = ref None in
  Array.iteri (fun index key ->
    if index land 4095 = 0 then Cancel.check_opt cancel;
    if Hashtbl.mem table key then duplicate := Some key
    else Hashtbl.add table key index) reference;
  match !duplicate with
  | Some key -> Error (Printf.sprintf "duplicate reference match key %S" key)
  | None ->
      let mapping = Array.make (Array.length current) (-1) in
      if Array.length current > 0 then Parallel.for_ ~chunk_size:grain ~start:0
          ~finish:(Array.length current - 1) (fun index ->
        if index land 4095 = 0 then Cancel.check_opt cancel;
        match Hashtbl.find_opt table current.(index) with
        | Some value -> mapping.(index) <- value
        | None -> ());
      Ok mapping

let sample_mapping ?cancel ~grain ~match_attribute current sample =
  match match_attribute with
  | None ->
      if Geometry.point_count current <> Geometry.point_count sample then
        Error "point-number matching requires equal point counts"
      else Ok None
  | Some name ->
      let attribute geometry =
        match Geometry.find_attribute ~owner:Attribute.Point name geometry with
        | None -> Error (Printf.sprintf "missing point match attribute %S" name)
        | Some value -> Ok (Attribute.Private.storage value) in
      Result.bind (attribute current) (fun current_values ->
        Result.bind (attribute sample) (fun sample_values ->
          match current_values, sample_values with
          | Attribute.Int current, Attribute.Int sample ->
              Result.map Option.some (int_mapping ?cancel ~grain current sample)
          | Attribute.Text current, Attribute.Text sample ->
              Result.map Option.some (text_mapping ?cancel ~grain current sample)
          | _ -> Error (Printf.sprintf
              "point match attribute %S must use the same integer or text storage"
              name)))

let sample_index mapping point = match mapping with None -> point | Some map -> map.(point)

let point_velocity_raw ?cancel ~grain ?points ?previous ?next ~approximation ~dt
    ~initialization ?match_attribute ~unmatched ~velocity_attribute ~add_velocity
    ~compute_acceleration ~acceleration_attribute geometry =
  if grain <= 0 then invalid_arg "Rdk.Motion.point_velocity: grain must be positive";
  if initialization = Compute_from_deformation
      && (not (Float.is_finite dt) || dt <= 0.) then
    invalid_arg "Rdk.Motion.point_velocity: dt must be finite and positive";
  if not (valid_name velocity_attribute) || String.equal velocity_attribute "P"
      || (compute_acceleration && (not (valid_name acceleration_attribute)
          || String.equal acceleration_attribute "P")) then
    invalid_arg "Rdk.Motion.point_velocity: output names must be non-empty and cannot be P";
  if compute_acceleration && String.equal velocity_attribute acceleration_attribute then
    invalid_arg "Rdk.Motion.point_velocity: velocity and acceleration attributes must differ";
  (match initialization, match_attribute with
   | Compute_from_deformation, Some name when not (valid_name name) ->
       invalid_arg "Rdk.Motion.point_velocity: empty match attribute"
   | _ -> ());
  (match initialization with
   | Set_value value ->
       check_finite3 "Rdk.Motion.point_velocity set value"
         value.Rays_math.Vec3.x value.y value.z
   | From_attribute { name; scale } ->
       if not (valid_name name) then
         invalid_arg "Rdk.Motion.point_velocity: empty source attribute";
       if not (Float.is_finite scale) then
         invalid_arg "Rdk.Motion.point_velocity: non-finite attribute scale"
   | Compute_from_deformation | Keep_incoming -> ());
  check_finite3 "Rdk.Motion.point_velocity add_velocity"
    add_velocity.Rays_math.Vec3.x add_velocity.y add_velocity.z;
  Cancel.check_opt cancel;
  let count = Geometry.point_count geometry in
  Result.bind (check_point_group count points) (fun () ->
    Result.bind (point_float3 "point_velocity" velocity_attribute geometry)
      (fun existing_velocity ->
        Result.bind (if compute_acceleration then
            point_float3 "point_velocity" acceleration_attribute geometry
          else Ok None) (fun existing_acceleration ->
          let zero_add = add_velocity.Rays_math.Vec3.x = 0. && add_velocity.y = 0.
              && add_velocity.z = 0. in
          let fast_path = match initialization with
            | Keep_incoming when zero_add && not compute_acceleration ->
                Option.map (fun _ -> Ok geometry) existing_velocity
            | From_attribute { name; scale = 1. }
                when points = None && zero_add && not compute_acceleration ->
                Some (Result.bind (point_float3 "point_velocity" name geometry)
                  (function
                    | None -> fail "point_velocity" "missing_attribute"
                        (Printf.sprintf "missing point attribute %S" name)
                    | Some values ->
                        Result.bind (make_point_float3 "point_velocity"
                            velocity_attribute values) (fun attribute ->
                          match Geometry.Private.with_merged_attributes_owned
                              [|attribute|] geometry with
                          | Ok output -> Ok output
                          | Error message -> fail "point_velocity"
                              "invalid_geometry" message)))
            | Compute_from_deformation | Keep_incoming | Set_value _
            | From_attribute _ -> None in
          match fast_path with
          | Some output -> output
          | None ->
          let x, y, z = copy_or_zero count existing_velocity in
          let acceleration = if compute_acceleration then
              Some (copy_or_zero count existing_acceleration) else None in
          let current = Packed.Float3.Private.view (Geometry.positions geometry) in
          let fill_initialized () =
            let source = match initialization with
              | From_attribute { name; _ } ->
                  (match point_float3 "point_velocity" name geometry with
                   | Ok (Some values) -> Some (Packed.Float3.Private.view values)
                   | Ok None -> raise (Invalid_argument
                       (Printf.sprintf "point_velocity: missing point attribute %S" name))
                   | Error error -> raise (Invalid_argument (Error.to_string error)))
              | _ -> None in
            if count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
                ~finish:(count - 1) (fun point ->
              if point land 4095 = 0 then Cancel.check_opt cancel;
              if selected points point then begin
                let vx, vy, vz = match initialization, source with
                  | Keep_incoming, _ -> x.(point), y.(point), z.(point)
                  | Set_value value, _ -> value.Rays_math.Vec3.x, value.y, value.z
                  | From_attribute { scale; _ }, Some source ->
                      source.x.(point) *. scale, source.y.(point) *. scale,
                      source.z.(point) *. scale
                  | _ -> assert false in
                check_finite3 "point_velocity" vx vy vz;
                x.(point) <- vx +. add_velocity.x;
                y.(point) <- vy +. add_velocity.y;
                z.(point) <- vz +. add_velocity.z
              end)
          in
          let deformation () =
            let needed_previous = match approximation with
              | Backward_difference | Central_difference -> true
              | Forward_difference -> false
            and needed_next = match approximation with
              | Forward_difference | Central_difference -> true
              | Backward_difference -> false in
            let previous = if needed_previous then match previous with
              | Some value -> Ok (Some value)
              | None -> Error "backward/central deformation needs a previous snapshot"
              else Ok None
            and next = if needed_next then match next with
              | Some value -> Ok (Some value)
              | None -> Error "forward/central deformation needs a next snapshot"
              else Ok None in
            Result.bind previous (fun previous -> Result.bind next (fun next ->
              let mapping sample = sample_mapping ?cancel ~grain ~match_attribute
                  geometry sample in
              Result.bind (match previous with None -> Ok None
                  | Some sample -> Result.map Option.some (mapping sample))
                (fun previous_mapping ->
                Result.bind (match next with None -> Ok None
                    | Some sample -> Result.map Option.some (mapping sample))
                  (fun next_mapping ->
                  let previous_positions = Option.map (fun value ->
                      Packed.Float3.Private.view (Geometry.positions value)) previous
                  and next_positions = Option.map (fun value ->
                      Packed.Float3.Private.view (Geometry.positions value)) next in
                  let previous_mapping = Option.join previous_mapping
                  and next_mapping = Option.join next_mapping in
                  let missing = Atomic.make false in
                  if count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
                      ~finish:(count - 1) (fun point ->
                    if point land 4095 = 0 then Cancel.check_opt cancel;
                    if selected points point then begin
                      let cx = current.x.(point) and cy = current.y.(point)
                      and cz = current.z.(point) in
                      if not (Float.is_finite cx && Float.is_finite cy
                              && Float.is_finite cz) then
                        invalid_arg "point_velocity current P: non-finite component";
                      let previous_index = match previous_positions with
                        | None -> point
                        | Some _ -> sample_index previous_mapping point
                      and next_index = match next_positions with
                        | None -> point
                        | Some _ -> sample_index next_mapping point in
                      let previous_found = previous_index >= 0
                      and next_found = next_index >= 0 in
                      let px = match previous_positions with
                        | Some values when previous_found -> values.x.(previous_index)
                        | _ -> cx
                      and py = match previous_positions with
                        | Some values when previous_found -> values.y.(previous_index)
                        | _ -> cy
                      and pz = match previous_positions with
                        | Some values when previous_found -> values.z.(previous_index)
                        | _ -> cz
                      and nx = match next_positions with
                        | Some values when next_found -> values.x.(next_index)
                        | _ -> cx
                      and ny = match next_positions with
                        | Some values when next_found -> values.y.(next_index)
                        | _ -> cy
                      and nz = match next_positions with
                        | Some values when next_found -> values.z.(next_index)
                        | _ -> cz in
                      if not (Float.is_finite px && Float.is_finite py
                              && Float.is_finite pz && Float.is_finite nx
                              && Float.is_finite ny && Float.is_finite nz) then
                        invalid_arg "point_velocity sample P: non-finite component";
                      if not previous_found || not next_found then Atomic.set missing true;
                      let found = previous_found && next_found in
                      if not found then begin
                        x.(point) <- add_velocity.x;
                        y.(point) <- add_velocity.y;
                        z.(point) <- add_velocity.z
                      end else begin
                        match approximation with
                        | Backward_difference ->
                            x.(point) <- ((cx -. px) /. dt) +. add_velocity.x;
                            y.(point) <- ((cy -. py) /. dt) +. add_velocity.y;
                            z.(point) <- ((cz -. pz) /. dt) +. add_velocity.z
                        | Forward_difference ->
                            x.(point) <- ((nx -. cx) /. dt) +. add_velocity.x;
                            y.(point) <- ((ny -. cy) /. dt) +. add_velocity.y;
                            z.(point) <- ((nz -. cz) /. dt) +. add_velocity.z
                        | Central_difference ->
                            let scale = 1. /. (2. *. dt) in
                            x.(point) <- ((nx -. px) *. scale) +. add_velocity.x;
                            y.(point) <- ((ny -. py) *. scale) +. add_velocity.y;
                            z.(point) <- ((nz -. pz) *. scale) +. add_velocity.z
                      end;
                      match acceleration with
                      | Some (ax, ay, az) ->
                          let inverse = 1. /. (dt *. dt) in
                          if found then begin
                            ax.(point) <- (nx -. (2. *. cx) +. px) *. inverse;
                            ay.(point) <- (ny -. (2. *. cy) +. py) *. inverse;
                            az.(point) <- (nz -. (2. *. cz) +. pz) *. inverse
                          end else begin
                            ax.(point) <- 0.; ay.(point) <- 0.; az.(point) <- 0.
                          end
                      | None -> ()
                    end);
                  if Atomic.get missing && unmatched = Velocity_unmatched_error then
                    Error "one or more selected points have no matching sample"
                  else Ok ())))) in
          let initialized = match initialization with
            | Compute_from_deformation ->
                if compute_acceleration && approximation <> Central_difference then
                  Error "acceleration requires central difference"
                else deformation ()
            | Keep_incoming | Set_value _ | From_attribute _ ->
                if compute_acceleration then
                  Error "acceleration requires deformation initialization"
                else (fill_initialized (); Ok ()) in
          let initialized = Result.map_error (fun message ->
              Error.make ~operation:"point_velocity" ~code:"invalid_deformation"
                message) initialized in
          Result.bind initialized (fun () ->
            let velocity = Packed.Float3.Private.of_owned_exn ~x ~y ~z in
            Result.bind (make_point_float3 "point_velocity" velocity_attribute velocity)
              (fun velocity_attribute_value ->
                let attributes = ref [velocity_attribute_value] in
                Result.bind (match acceleration with
                  | None -> Ok ()
                  | Some (x, y, z) ->
                      let values = Packed.Float3.Private.of_owned_exn ~x ~y ~z in
                      Result.map (fun attribute -> attributes := attribute :: !attributes)
                        (make_point_float3 "point_velocity"
                           acceleration_attribute values))
                  (fun () ->
                    match Geometry.Private.with_merged_attributes_owned
                        (Array.of_list !attributes) geometry with
                    | Ok output -> Ok output
                    | Error message -> fail "point_velocity" "invalid_geometry" message))))))

let point_velocity ?cancel ?(grain = 16_384) ?points ?previous ?next
    ?(approximation = Backward_difference) ?(dt = 1. /. 60.)
    ?(initialization = Compute_from_deformation) ?match_attribute
    ?(unmatched = Velocity_unmatched_error) ?(velocity_attribute = "v")
    ?(add_velocity = Rays_math.Vec3.zero) ?(compute_acceleration = false)
    ?(acceleration_attribute = "accel") geometry =
  try point_velocity_raw ?cancel ~grain ?points ?previous ?next ~approximation
      ~dt ~initialization ?match_attribute ~unmatched ~velocity_attribute
      ~add_velocity ~compute_acceleration ~acceleration_attribute geometry with
  | Cancel.Cancelled -> fail "point_velocity" "cancelled"
      "point velocity operation was cancelled"
  | Invalid_argument message -> fail "point_velocity" "invalid_argument" message
