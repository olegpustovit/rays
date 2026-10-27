open Rays_math

let error message = Error ("Rdk_gen.Point_jitter.point_jitter: " ^ message)

let point_float ?(missing = false) name geometry =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | None when missing -> Ok None
  | None -> error (Printf.sprintf "point float attribute %S does not exist" name)
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Float values -> Ok (Some values)
       | _ -> error (Printf.sprintf
           "point attribute %S must use float storage" name))

let point_int name geometry =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | None -> Ok None
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Int values -> Ok (Some values)
       | _ -> error (Printf.sprintf
           "point attribute %S must use int storage" name))

let finite_vec3 value = Float.is_finite value.Rays_math.Vec3.x
  && Float.is_finite value.y && Float.is_finite value.z

let empty_name = function
  | None -> false
  | Some name -> String.trim name = ""

let run ?cancel ?(grain = 16_384) ?points ?mask_attribute ?id_attribute
    ?(use_point_scale = false) ~seed ~scale ?(axis_scales = Rays_math.Vec3.create 1. 1. 1.)
    geometry =
  let selection_error = match points with
    | Some group when Group.owner group <> Group.Point ->
        Some "selection must own points"
    | Some group when Group.length group <> Geometry.point_count geometry ->
        Some "selection length does not match point count"
    | None | Some _ -> None in
  match selection_error with
  | Some message -> Error (Error.of_string ~operation:"point_jitter"
      ~code:"invalid_selection" message)
  | None ->
  Error.guard ~operation:"point_jitter" ~code:"invalid_attribute" @@ fun () ->
  if grain <= 0 then invalid_arg "Rdk_gen.Point_jitter.point_jitter: grain must be positive";
  if not (Float.is_finite scale) then error "scale must be finite"
  else if not (finite_vec3 axis_scales) then error "axis scales must be finite"
  else if empty_name mask_attribute then
    error "mask attribute name must not be empty"
  else if empty_name id_attribute then
    error "id attribute name must not be empty"
  else
    let count = Geometry.point_count geometry in
    let selection = match points with
      | None -> Ok None
      | Some group when Group.owner group <> Group.Point ->
          error "selection must own points"
      | Some group when Group.length group <> count ->
          error "selection length does not match point count"
      | Some group -> Ok (Some group) in
    Result.bind selection (fun selection ->
    Result.bind (match mask_attribute with
      | None -> Ok None
      | Some name -> point_float name geometry) (fun mask ->
    Result.bind (if use_point_scale then point_float ~missing:true "pscale" geometry
      else Ok None) (fun point_scale ->
    Result.bind (match id_attribute with
      | None -> Ok None
      | Some name -> point_int name geometry) (fun ids ->
      let source = Packed.Float3.Private.view (Geometry.positions geometry) in
      let x = Array.copy source.x and y = Array.copy source.y
      and z = Array.copy source.z in
      let range_count = if count = 0 then 0 else (count + grain - 1) / grain in
      let errors = Array.make range_count (-1)
      and changed_counts = Array.make range_count 0 in
      Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(range_count - 1) (fun range ->
        let first = range * grain in
        let last = min count (first + grain) in
        for point = first to last - 1 do
          let () = if point land 4095 = 0 then Cancel.check_opt cancel in
          let selected = match selection with None -> true
            | Some group -> Group.mem point group in
          if selected then begin
            let mask_value = match mask with None -> 1.
              | Some values -> values.(point) in
            let point_scale_value = match point_scale with None -> 1.
              | Some values -> values.(point) in
            let amplitude = scale *. mask_value *. point_scale_value in
            if not (Float.is_finite mask_value
                && Float.is_finite point_scale_value
                && Float.is_finite amplitude) then begin
              if errors.(range) < 0 then errors.(range) <- point
            end
            else begin
              let identity = (match ids with None -> point
                | Some values -> values.(point)) * 0x1e3779b97f4a7c15 in
              let sample_x = Rays_math.Rand.float_at seed ~index:(identity lxor
                  0x11b54a32d192ed03) -. 0.5
              and sample_y = Rays_math.Rand.float_at seed ~index:(identity lxor
                  (2 * 0x11b54a32d192ed03)) -. 0.5
              and sample_z = Rays_math.Rand.float_at seed ~index:(identity lxor
                  (3 * 0x11b54a32d192ed03)) -. 0.5 in
              let nx = source.x.(point)
                  +. (sample_x *. amplitude *. axis_scales.Rays_math.Vec3.x)
              and ny = source.y.(point)
                  +. (sample_y *. amplitude *. axis_scales.y)
              and nz = source.z.(point)
                  +. (sample_z *. amplitude *. axis_scales.z) in
              if not (Float.is_finite nx && Float.is_finite ny
                  && Float.is_finite nz) then begin
                if errors.(range) < 0 then errors.(range) <- point
              end
              else begin
                x.(point) <- nx; y.(point) <- ny; z.(point) <- nz;
                if nx <> source.x.(point) || ny <> source.y.(point)
                    || nz <> source.z.(point) then
                  changed_counts.(range) <- changed_counts.(range) + 1
              end
            end
          end
        done);
      match Array.find_opt (fun point -> point >= 0) errors with
      | Some point -> error (Printf.sprintf
          "selected point %d has non-finite input or output" point)
      | None when Array.fold_left ( + ) 0 changed_counts = 0 -> Ok geometry
      | None ->
          let positions = Packed.Float3.Private.of_owned_exn ~x ~y ~z in
          let attributes = Geometry.Private.attributes geometry in
          let output_attributes = Array.to_list attributes
            |> List.filter (fun attribute ->
              let owner = Attribute.owner attribute in
              not (String.equal (Attribute.name attribute) "N"
                && (owner = Attribute.Point || owner = Attribute.Vertex)))
            |> Array.of_list in
          Geometry.Private.with_positions_and_attributes_owned positions
            output_attributes geometry
    ))))
