open Rays_math

type mode =
  | Generate_total of int
  | Generate_per_point of {
      points_per_point : float;
      scale_attribute : string option;
    }
  | Generate_probability of { attribute : string }

exception Cardinality_error of string

let error message = Error ("Rdk.Point_generate.point_generate: " ^ message)

let nonempty label name =
  if String.trim name = "" then error (label ^ " must not be empty") else Ok ()

let compile_optional_pattern label source =
  if String.trim source = "" then Ok None
  else Result.map Option.some (Attribute_pattern.compile source)
    |> Result.map_error (fun message -> "Rdk.Point_generate.point_generate: " ^ label
        ^ ": " ^ message)

let point_float name geometry =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | None -> error (Printf.sprintf "point float attribute %S does not exist" name)
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Float values -> Ok values
       | _ -> error (Printf.sprintf
           "point attribute %S must use float storage" name))

let checked_add label left right =
  if right > Sys.max_array_length - left then
    raise (Cardinality_error (label ^ " exceeds OCaml array limits"));
  left + right

let[@inline always] selected points point = match points with
  | None -> true
  | Some group -> Group.mem point group

let plan_counts ?cancel ~grain ~seed ~points ?count_ids mode geometry =
  let point_count = Geometry.point_count geometry in
  match mode with
  | Generate_total count ->
      if count < 0 then error "total point count must be non-negative"
      else Ok (None, count)
  | Generate_per_point { points_per_point; scale_attribute } ->
      if not (Float.is_finite points_per_point) || points_per_point < 0. then
        error "points per point must be finite and non-negative"
      else Result.bind (match scale_attribute with
        | None -> Ok None
        | Some name -> Result.map Option.some (point_float name geometry))
        (fun scale ->
        let counts = Array.make point_count 0 in
        let range_count = if point_count = 0 then 0
          else (point_count + grain - 1) / grain in
        let failures = Array.make range_count (-1) in
        if range_count > 0 then Parallel.for_ ~chunk_size:1 ~start:0
            ~finish:(range_count - 1) (fun range ->
          let first = range * grain and last = min point_count ((range + 1) * grain) in
          for point = first to last - 1 do
            if point land 4095 = 0 then Cancel.check_opt cancel;
            if selected points point then begin
              let weight = match scale with None -> 1.
                | Some values -> values.(point) in
              let expected = points_per_point *. weight in
              if not (Float.is_finite weight) || weight < 0.
                  || not (Float.is_finite expected)
                  || expected > float_of_int Sys.max_array_length then
                failures.(range) <- if failures.(range) < 0 then point
                  else failures.(range)
              else begin
                let integral = int_of_float (Float.floor expected) in
                let fraction = expected -. float_of_int integral in
                let identity = match count_ids with None -> point
                  | Some values -> values.(point) in
                counts.(point) <- integral +
                  if fraction > 0.
                      && Rays_math.Rand.float_at seed
                           ~index:(identity lxor 0x27d4eb2d) < fraction
                  then 1 else 0
              end
            end
          done);
        match Array.find_opt (fun point -> point >= 0) failures with
        | Some point -> error (Printf.sprintf
            "selected point %d has a non-finite, negative, or excessive generation scale"
            point)
        | None ->
            (try
               let total = Array.fold_left
                   (fun total count -> checked_add "generated point count" total count)
                   0 counts in
               Ok (Some counts, total)
             with Cardinality_error message -> error message))
  | Generate_probability { attribute } ->
      Result.bind (nonempty "probability attribute name" attribute) (fun () ->
      Result.bind (point_float attribute geometry) (fun probabilities ->
        let counts = Array.make point_count 0 in
        let range_count = if point_count = 0 then 0
          else (point_count + grain - 1) / grain in
        let failures = Array.make range_count (-1) in
        if range_count > 0 then Parallel.for_ ~chunk_size:1 ~start:0
            ~finish:(range_count - 1) (fun range ->
          let first = range * grain and last = min point_count ((range + 1) * grain) in
          for point = first to last - 1 do
            if point land 4095 = 0 then Cancel.check_opt cancel;
            if selected points point then begin
              let probability = probabilities.(point) in
              if not (Float.is_finite probability)
                  || probability < 0. || probability > 1. then
                failures.(range) <- if failures.(range) < 0 then point
                  else failures.(range)
              else
                let identity = match count_ids with None -> point
                  | Some values -> values.(point) in
                if Rays_math.Rand.float_at seed ~index:(identity lxor 0x165667b1)
                  < probability then counts.(point) <- 1
            end
          done);
        match Array.find_opt (fun point -> point >= 0) failures with
        | Some point -> error (Printf.sprintf
            "selected point %d probability must be finite and in [0,1]" point)
        | None -> Ok (Some counts, Array.fold_left ( + ) 0 counts)))

let generated_maps ?cancel ~grain ~mode ~counts ~generated_count () =
  let sources = Array.make generated_count (-1)
  and indices = Array.make generated_count 0 in
  match counts with
  | None ->
      if generated_count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
          ~finish:(generated_count - 1) (fun generated ->
        if generated land 4095 = 0 then Cancel.check_opt cancel;
        indices.(generated) <- generated);
      sources, indices
  | Some counts ->
      let offsets = Array.make (Array.length counts + 1) 0 in
      for point = 0 to Array.length counts - 1 do
        offsets.(point + 1) <- offsets.(point) + counts.(point)
      done;
      if Array.length counts > 0 then Parallel.for_ ~chunk_size:(max 1 (grain / 8))
          ~start:0 ~finish:(Array.length counts - 1) (fun point ->
        if point land 4095 = 0 then Cancel.check_opt cancel;
        for generated = offsets.(point) to offsets.(point + 1) - 1 do
          sources.(generated) <- point;
          indices.(generated) <- generated - offsets.(point)
        done);
      (match mode with Generate_total _ -> assert false
       | Generate_per_point _ | Generate_probability _ -> ());
      sources, indices

let map_fixed ?cancel ~grain ~total source_at default source =
  let output = Array.make total default in
  if total > 0 then Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(total - 1)
      (fun target ->
        if target land 4095 = 0 then Cancel.check_opt cancel;
        let source_index = source_at target in
        if source_index >= 0 then output.(target) <- source.(source_index));
  output

let remap_point_attribute ?cancel ~grain ~total ~source_at attribute =
  let storage = match Attribute.Private.storage attribute with
    | Attribute.Float values ->
        Attribute.Float (map_fixed ?cancel ~grain ~total source_at 0. values)
    | Attribute.Int values ->
        Attribute.Int (map_fixed ?cancel ~grain ~total source_at 0 values)
    | Attribute.Text values ->
        Attribute.Text (map_fixed ?cancel ~grain ~total source_at "" values)
    | Attribute.Float2 values ->
        let values = Packed.Float2.Private.view values in
        Attribute.Float2 (Packed.Float2.of_owned
          ~x:(map_fixed ?cancel ~grain ~total source_at 0. values.x)
          ~y:(map_fixed ?cancel ~grain ~total source_at 0. values.y)
          |> Result.get_ok)
    | Attribute.Float3 values ->
        let values = Packed.Float3.Private.view values in
        Attribute.Float3 (Packed.Float3.Private.of_owned_exn
          ~x:(map_fixed ?cancel ~grain ~total source_at 0. values.x)
          ~y:(map_fixed ?cancel ~grain ~total source_at 0. values.y)
          ~z:(map_fixed ?cancel ~grain ~total source_at 0. values.z))
    | Attribute.Float4 values ->
        let values = Packed.Float4.Private.view values in
        Attribute.Float4 (Packed.Float4.of_owned
          ~x:(map_fixed ?cancel ~grain ~total source_at 0. values.x)
          ~y:(map_fixed ?cancel ~grain ~total source_at 0. values.y)
          ~z:(map_fixed ?cancel ~grain ~total source_at 0. values.z)
          ~w:(map_fixed ?cancel ~grain ~total source_at 0. values.w)
          |> Result.get_ok)
    | Attribute.Int_array values ->
        let values = Packed.Int_array.Private.view values in
        let offsets = Array.make (total + 1) 0 in
        for target = 0 to total - 1 do
          let source = source_at target in
          let width = if source < 0 then 0
            else values.offsets.(source + 1) - values.offsets.(source) in
          offsets.(target + 1) <- checked_add "integer-array payload"
              offsets.(target) width
        done;
        let output = Array.make offsets.(total) 0 in
        if total > 0 then Parallel.for_ ~chunk_size:grain ~start:0
            ~finish:(total - 1) (fun target ->
          if target land 4095 = 0 then Cancel.check_opt cancel;
          let source = source_at target in
          if source >= 0 then Array.blit values.values values.offsets.(source)
              output offsets.(target)
              (offsets.(target + 1) - offsets.(target)));
        Attribute.Int_array (Packed.Int_array.Private.create_validated_owned
          ~offsets ~values:output)
    | Attribute.Float_array values ->
        let values = Packed.Float_array.Private.view values in
        let offsets = Array.make (total + 1) 0 in
        for target = 0 to total - 1 do
          let source = source_at target in
          let width = if source < 0 then 0
            else values.offsets.(source + 1) - values.offsets.(source) in
          offsets.(target + 1) <- checked_add "float-array payload"
              offsets.(target) width
        done;
        let output = Array.make offsets.(total) 0. in
        if total > 0 then Parallel.for_ ~chunk_size:grain ~start:0
            ~finish:(total - 1) (fun target ->
          if target land 4095 = 0 then Cancel.check_opt cancel;
          let source = source_at target in
          if source >= 0 then Array.blit values.values values.offsets.(source)
              output offsets.(target)
              (offsets.(target + 1) - offsets.(target)));
        Attribute.Float_array (Packed.Float_array.Private.create_validated_owned
          ~offsets ~values:output) in
  Attribute.create_owned ~owner:Attribute.Point ~name:(Attribute.name attribute)
    storage |> Result.get_ok

let extend_group ?cancel ~grain ~total ~prefix ~generated_member group =
  let target = Group.init ~grain ~owner:Group.Point ~name:(Group.name group) total
      (fun point ->
        if point land 4095 = 0 then Cancel.check_opt cancel;
        if point < prefix then Group.mem point group else generated_member) in
  match Group.Private.order_view group with
  | None -> target
  | Some order when not generated_member ->
      Group.Private.with_owned_order (Array.copy order) target
  | Some order ->
      let generated = total - prefix in
      let output = Array.make (Array.length order + generated) 0 in
      Array.blit order 0 output 0 (Array.length order);
      for index = 0 to generated - 1 do
        output.(Array.length order + index) <- prefix + index
      done;
      Group.Private.with_owned_order output target

let metadata_attribute ?cancel ~grain ~name ~prefix ~generated_values geometry =
  let total = prefix + Array.length generated_values in
  let existing = Geometry.find_attribute ~owner:Attribute.Point name geometry in
  (match existing with
   | None -> ()
   | Some attribute -> (match Attribute.Private.storage attribute with
       | Attribute.Int _ -> ()
       | _ -> raise (Invalid_argument (Printf.sprintf
           "metadata attribute %S must use point integer storage" name))));
  let values = if prefix = 0 then generated_values else begin
    let values = Array.make total (-1) in
    (match existing with
     | Some attribute -> (match Attribute.Private.storage attribute with
         | Attribute.Int source -> Array.blit source 0 values 0 prefix
         | _ -> assert false)
     | None -> ());
    if Array.length generated_values > 0 then Parallel.for_ ~chunk_size:grain
        ~start:0 ~finish:(Array.length generated_values - 1) (fun generated ->
      if generated land 4095 = 0 then Cancel.check_opt cancel;
      values.(prefix + generated) <- generated_values.(generated));
    values
  end in
  Attribute.create_owned ~owner:Attribute.Point ~name (Attribute.Int values)
  |> Result.get_ok

let run ?cancel ?(grain = 16_384) ?points ?count_ids ?(keep_input = false)
    ?(seed = Rays_math.Rand.seed 0) ?generated_group
    ?(source_point_attribute = "sourcepoint")
    ?(source_index_attribute = "sourceindex")
    ?(copy_point_attributes = "*") ?(copy_detail_attributes = "")
    ~mode geometry =
  Error.guard ~operation:"point_generate" ~code:"invalid_geometry" @@ fun () ->
  if grain <= 0 then invalid_arg "Rdk.Point_generate.point_generate: grain must be positive";
  let point_count = Geometry.point_count geometry in
  let selection = match points with
    | None -> Ok None
    | Some group when Group.owner group <> Group.Point ->
        error "selection must own points"
    | Some group when Group.length group <> point_count ->
        error "selection length does not match point count"
    | Some group -> Ok (Some group) in
  Result.bind selection (fun points ->
  Result.bind (nonempty "source point attribute name" source_point_attribute)
    (fun () ->
  Result.bind (nonempty "source index attribute name" source_index_attribute)
    (fun () ->
  if String.equal source_point_attribute source_index_attribute then
    error "source point and source index attribute names must differ"
  else if String.equal source_point_attribute "P"
      || String.equal source_index_attribute "P" then
    error "metadata attributes cannot be named P"
  else if match generated_group with Some name -> String.trim name = ""
      | None -> false then error "generated group name must not be empty"
  else Result.bind (compile_optional_pattern "point attribute pattern"
      copy_point_attributes) (fun point_pattern ->
  Result.bind (compile_optional_pattern "detail attribute pattern"
      copy_detail_attributes) (fun detail_pattern ->
  Result.bind (plan_counts ?cancel ~grain ~seed ~points ?count_ids mode geometry)
    (fun (counts, generated_count) ->
  try
    let prefix = if keep_input then point_count else 0 in
    let total = checked_add "output point count" prefix generated_count in
    let generated_sources, generated_indices = generated_maps ?cancel ~grain
        ~mode ~counts ~generated_count () in
    let source_positions = Packed.Float3.Private.view (Geometry.positions geometry) in
    let x = Array.make total 0. and y = Array.make total 0.
    and z = Array.make total 0. in
    if prefix > 0 then begin
      Array.blit source_positions.x 0 x 0 prefix;
      Array.blit source_positions.y 0 y 0 prefix;
      Array.blit source_positions.z 0 z 0 prefix
    end;
    let range_count = if generated_count = 0 then 0
      else (generated_count + grain - 1) / grain in
    let failures = Array.make range_count (-1) in
    if range_count > 0 then Parallel.for_ ~chunk_size:1 ~start:0
        ~finish:(range_count - 1) (fun range ->
      let first = range * grain and last = min generated_count ((range + 1) * grain) in
      for generated = first to last - 1 do
        if generated land 4095 = 0 then Cancel.check_opt cancel;
        let source = generated_sources.(generated) in
        if source >= 0 then begin
          let px = source_positions.x.(source)
          and py = source_positions.y.(source)
          and pz = source_positions.z.(source) in
          if Float.is_finite px && Float.is_finite py && Float.is_finite pz then begin
            x.(prefix + generated) <- px;
            y.(prefix + generated) <- py;
            z.(prefix + generated) <- pz
          end else if failures.(range) < 0 then failures.(range) <- source
        end
      done);
    match Array.find_opt (fun source -> source >= 0) failures with
    | Some source -> error (Printf.sprintf
        "selected source point %d has a non-finite position" source)
    | None ->
      let source_topology = Geometry.topology geometry in
      let topology = if keep_input then
          Topology.Private.extend_free_points ~point_count:total source_topology
        else Topology.empty ~point_count:total in
      let positions = Packed.Float3.Private.of_owned_exn ~x ~y ~z in
      let point_name name = String.equal name source_point_attribute
          || String.equal name source_index_attribute in
      let source_at ~copy_generated target =
        if target < prefix then target
        else if copy_generated then generated_sources.(target - prefix)
        else -1 in
      let attributes = Geometry.attributes geometry |> List.filter_map
          (fun attribute ->
        let owner = Attribute.owner attribute and name = Attribute.name attribute in
        if owner = Attribute.Point then
          if point_name name then None
          else
            let copy_generated = Option.is_some counts && match point_pattern with
              | Some pattern -> Attribute_pattern.matches pattern name
              | None -> false in
            if not keep_input && not copy_generated then None
            else Some (remap_point_attribute ?cancel ~grain ~total
              ~source_at:(source_at ~copy_generated) attribute)
        else if keep_input then Some attribute
        else if owner = Attribute.Detail && (match detail_pattern with
          | Some pattern -> Attribute_pattern.matches pattern name
          | None -> false) then Some attribute
        else None) in
      let source_metadata = metadata_attribute ?cancel ~grain
          ~name:source_point_attribute ~prefix
          ~generated_values:generated_sources geometry
      and index_metadata = metadata_attribute ?cancel ~grain
          ~name:source_index_attribute ~prefix
          ~generated_values:generated_indices geometry in
      let attributes = attributes @ [source_metadata; index_metadata] in
      let source_groups = if keep_input then Geometry.groups geometry else [] in
      let groups, found_output = List.fold_right (fun group (groups, found) ->
        match Group.owner group with
        | Group.Point ->
            let output = match generated_group with
              | Some name when String.equal name (Group.name group) -> true
              | None | Some _ -> false in
            extend_group ?cancel ~grain ~total ~prefix
              ~generated_member:output group :: groups,
            found || output
        | Group.Vertex | Group.Primitive -> group :: groups, found)
        source_groups ([], false) in
      let groups = match generated_group, found_output with
        | Some name, false ->
            Group.init ~grain ~owner:Group.Point ~name total
              (fun point -> point >= prefix) :: groups
        | None, _ | Some _, true -> groups in
      let edge_groups = if not keep_input then [] else
        Geometry.edge_groups geometry |> List.map
          (Edge_group.Private.rebind_appended_free_points
            ~source_topology ~target_topology:topology) in
      Geometry.create ~positions ~topology ~attributes ~groups ~edge_groups ()
  with
  | Cardinality_error message -> error message
  | Invalid_argument message -> error message
  ))))))
