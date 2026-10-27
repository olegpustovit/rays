open Rays_math
open Fuse_reduce

let finite value = Float.is_finite value

type fuse_metric = Euclidean | Componentwise
type fuse_using = Point_snap.using =
  | Least_target_point
  | Closest_target_point
type fuse_match_condition = Point_snap.match_condition =
  | Equal_attribute_values
  | Unequal_attribute_values
type fuse_targeting = Point_snap.targeting =
  | Near_points
  | Specified_points of string
type grid_rounding = Grid_nearest | Grid_down | Grid_up

type fuse_attribute_view =
  | Fuse_float of float array
  | Fuse_int of int array
  | Fuse_float2 of Packed.Float2.Private.view
  | Fuse_float3 of Packed.Float3.Private.view
  | Fuse_float4 of Packed.Float4.Private.view
  | Fuse_int_array of Packed.Int_array.Private.view
  | Fuse_float_array of Packed.Float_array.Private.view
  | Fuse_text of string array

let fuse_clusters ?cancel ?(grain = 16_384) ?selection ?(tolerance = 1e-6)
    ?(position = Average_position) ?weight_attribute ?(attributes = Keep_first)
    ?(attribute_rules = []) ?(group_rules = [])
    ?(metric = Euclidean) ?(inclusive = true) ?(match_attributes = false)
    ?(keep_fused_points = false) geometry =
  if grain <= 0 then invalid_arg "Rdk.Fuse_grid.fuse: grain must be positive";
  let count = Geometry.point_count geometry in
  if match selection with
    | Some group -> Group.owner group <> Group.Point
        || Group.length group <> count
    | None -> false
  then Error "Rdk.Fuse_grid.fuse: selection must be a matching point group"
  else if not (finite tolerance) || tolerance < 0. then
    Error "Rdk.Fuse_grid.fuse: tolerance must be finite and non-negative"
  else
    let exact = tolerance = 0. in
    let point_attributes = if match_attributes then
              Geometry.attributes geometry
              |> List.filter_map (fun attribute ->
                if Attribute.owner attribute <> Attribute.Point then None
                else Some (match Attribute.Private.storage attribute with
                  | Attribute.Float values -> Fuse_float values
                  | Attribute.Int values -> Fuse_int values
                  | Attribute.Text values -> Fuse_text values
                  | Attribute.Float2 values ->
                      Fuse_float2 (Packed.Float2.Private.view values)
                  | Attribute.Float3 values ->
                      Fuse_float3 (Packed.Float3.Private.view values)
                  | Attribute.Float4 values ->
                      Fuse_float4 (Packed.Float4.Private.view values)
                  | Attribute.Int_array values ->
                      Fuse_int_array (Packed.Int_array.Private.view values)
                  | Attribute.Float_array values ->
                      Fuse_float_array (Packed.Float_array.Private.view values)))
              |> Array.of_list
      else [||] in
    let within value = if exact then value = 0.
      else if inclusive then value <= tolerance else value < tolerance in
    let near left right = within (abs_float (left -. right)) in
    let same_row offsets left right equal values =
      let left_first = offsets.(left) and left_last = offsets.(left + 1)
      and right_first = offsets.(right) and right_last = offsets.(right + 1) in
      let length = left_last - left_first in
      if length <> right_last - right_first then false
      else begin
        let local = ref 0 and result = ref true in
        while !result && !local < length do
          result := equal values.(left_first + !local)
              values.(right_first + !local);
          incr local
        done;
        !result
      end in
    let compatible left right =
      let index = ref 0 and result = ref true in
      while !result && !index < Array.length point_attributes do
        result := (match point_attributes.(!index) with
                | Fuse_float values -> near values.(left) values.(right)
                | Fuse_int values -> values.(left) = values.(right)
                | Fuse_text values -> String.equal values.(left) values.(right)
                | Fuse_float2 view -> near view.x.(left) view.x.(right)
                    && near view.y.(left) view.y.(right)
                | Fuse_float3 view -> near view.x.(left) view.x.(right)
                    && near view.y.(left) view.y.(right)
                    && near view.z.(left) view.z.(right)
                | Fuse_float4 view -> near view.x.(left) view.x.(right)
                    && near view.y.(left) view.y.(right)
                    && near view.z.(left) view.z.(right)
                    && near view.w.(left) view.w.(right)
                | Fuse_int_array view -> same_row view.offsets left right ( = )
                    view.values
                | Fuse_float_array view -> same_row view.offsets left right near
                    view.values);
        incr index
      done;
      !result in
    let cluster_metric = match metric with
      | Euclidean -> Point_clusters.Euclidean
      | Componentwise -> Point_clusters.Componentwise in
    Result.bind (Point_clusters.create ?cancel ?selection ~metric:cluster_metric
        ~inclusive ~operation:"Rdk.Fuse_grid.fuse" ~tolerance ~compatible geometry)
      (function
      | Point_clusters.Identity -> Ok geometry
      | Point_clusters.Clusters clusters ->
          Fuse_reduce.apply ?cancel ~grain ~position ?weight_attribute ~attributes
            ~attribute_rules ~group_rules
            ~compact:(not keep_fused_points) ~rewire:true clusters geometry)

let fuse ?cancel ?(grain = 16_384) ?selection ?target_selection
    ?(targeting = Near_points) ?(using = Least_target_point)
    ?(tolerance = 1e-6) ?(position = Average_position)
    ?weight_attribute ?(attributes = Keep_first) ?(attribute_rules = [])
    ?(group_rules = []) ?(metric = Euclidean)
    ?(inclusive = true)
    ?(match_attributes = false) ?radius_attribute ?match_attribute
    ?(match_condition = Equal_attribute_values) ?(match_tolerance = 0.)
    ?(modify_target = false) ?(fuse_points = true)
    ?(keep_fused_points = false) ?snapped_group
    ?snapped_destination_attribute ?(remove_degenerate_primitives = false)
    ?(remove_unused_points_from_degenerate_primitives = false)
    ?(remove_all_unused_points = false) ?target geometry =
  Error.guard ~operation:"fuse" ~code:"invalid_geometry" @@ fun () ->
  match Fuse_rules.validate ~attribute_rules ~group_rules geometry with
  | Error message -> Error message
  | Ok () ->
  let advanced = target <> None || target_selection <> None
      || targeting <> Near_points || using <> Least_target_point
      || radius_attribute <> None || match_attribute <> None
      || match_condition <> Equal_attribute_values || match_tolerance <> 0.
      || modify_target || not fuse_points || snapped_group <> None
      || snapped_destination_attribute <> None in
  let cleanup result = Result.bind result (Fuse_cleanup.apply ?cancel ~grain
      ~remove_degenerate_primitives
      ~remove_unused_points_from_degenerate_primitives
      ~remove_all_unused_points) in
  if not advanced then cleanup (fuse_clusters ?cancel ~grain ?selection
      ~tolerance ~position ?weight_attribute ~attributes ~metric ~inclusive
      ~attribute_rules ~group_rules ~match_attributes ~keep_fused_points geometry)
  else cleanup (begin
    let invalid_name label = function
      | Some name when String.trim name = "" ->
          Some (Printf.sprintf "Rdk.Fuse_grid.fuse: %s name must not be empty" label)
      | _ -> None in
    match invalid_name "snapped group" snapped_group with
    | Some message -> Error message
    | None ->
      (match invalid_name "snapped destination attribute"
          snapped_destination_attribute with
       | Some message -> Error message
       | None ->
        let target_geometry = Option.value ~default:geometry target in
        let same = Geometry.data_id geometry
            = Geometry.data_id target_geometry in
        if keep_fused_points && not fuse_points then Error
            "Rdk.Fuse_grid.fuse: Keep Fused Points requires Fuse Snapped Points"
        else if modify_target && not same then Error
            "Rdk.Fuse_grid.fuse: Modify Target is unavailable with a second input"
        else
        let effective_targets = match target_selection, same with
          | Some group, _ -> Some group
          | None, true -> selection
          | None, false -> None in
        let effective_modify_target = modify_target
            || (same && target_selection = None) in
        let cluster_metric = match metric with
          | Euclidean -> Point_clusters.Euclidean
          | Componentwise -> Point_clusters.Componentwise in
        Result.bind (Point_snap.plan ?cancel ~grain ?queries:selection
            ?targets:effective_targets ~targeting ~using ~tolerance
            ~metric:cluster_metric ~inclusive ?radius_attribute
            ?match_attribute ~match_condition ~match_tolerance
            ~source:geometry ~target:target_geometry ())
          (fun destinations ->
            let count = Geometry.point_count geometry in
            let mapped = ref 0 and moved = ref 0 in
            let source = Packed.Float3.Private.view
                (Geometry.positions geometry)
            and target_positions = Packed.Float3.Private.view
                (Geometry.positions target_geometry) in
            for point = 0 to count - 1 do
              let destination = destinations.(point) in
              if destination >= 0 then begin
                incr mapped;
                if source.x.(point) <> target_positions.x.(destination)
                    || source.y.(point) <> target_positions.y.(destination)
                    || source.z.(point) <> target_positions.z.(destination)
                then incr moved
              end
            done;
            let destination_bits = if same && fuse_points && !mapped > 0 then
                let bits = Bytes.make ((count + 7) / 8) '\000' in
                for point = 0 to count - 1 do
                  let destination = destinations.(point) in
                  if destination >= 0 then begin
                    let byte = destination lsr 3
                    and mask = 1 lsl (destination land 7) in
                    Bytes.unsafe_set bits byte (Char.chr
                      (Char.code (Bytes.unsafe_get bits byte) lor mask))
                  end
                done;
                Some bits
              else None in
            let install_outputs output =
              let output = match snapped_destination_attribute with
                | None -> output
                | Some name ->
                    let values = Array.copy destinations in
                    (match destination_bits with
                     | None -> ()
                     | Some bits ->
                         for point = 0 to count - 1 do
                           if values.(point) < 0
                               && Char.code (Bytes.unsafe_get bits (point lsr 3))
                                  land (1 lsl (point land 7)) <> 0
                           then values.(point) <- point
                         done);
                    let attribute = Attribute.create_owned ~owner:Attribute.Point
                        ~name (Attribute.Int values) |> Support.get_ok in
                    Geometry.with_attribute attribute output |> Support.get_ok in
              match snapped_group with
              | None -> output
              | Some name ->
                  let group = Group.init ~grain ~owner:Group.Point ~name count
                      (fun point -> destinations.(point) >= 0) in
                  Geometry.with_group group output |> Support.get_ok in
            let install_reduced_outputs clusters ~compact output =
              let output_count = Geometry.point_count output in
              let source_point output_point = if compact then
                  let first = clusters.Point_clusters.offsets.(output_point)
                  and last = clusters.offsets.(output_point + 1) in
                  let slot = ref first and selected = ref (-1) in
                  while !slot < last && !selected < 0 do
                    let point = clusters.members.(!slot) in
                    if destinations.(point) >= 0 then selected := point;
                    incr slot
                  done;
                  !selected
                else output_point in
              let output = match snapped_destination_attribute with
                | None -> output
                | Some name ->
                    let values = Array.init output_count (fun output_point ->
                      let point = source_point output_point in
                      if point < 0 then -1 else destinations.(point)) in
                    let attribute = Attribute.create_owned ~owner:Attribute.Point
                        ~name (Attribute.Int values) |> Support.get_ok in
                    Geometry.with_attribute attribute output |> Support.get_ok in
              match snapped_group with
              | None -> output
              | Some name ->
                  let group = Group.init ~grain ~owner:Group.Point ~name
                      output_count (fun output_point ->
                        source_point output_point >= 0) in
                  Geometry.with_group group output |> Support.get_ok in
            if same && effective_modify_target && !mapped > 0 then
              Result.bind (Point_clusters.of_links ?cancel
                  ~operation:"Rdk.Fuse_grid.fuse" destinations) (function
                | Point_clusters.Identity -> Ok (install_outputs geometry)
                | Point_clusters.Clusters clusters ->
                    let compact = fuse_points && not keep_fused_points in
                    Result.map (install_reduced_outputs clusters ~compact)
                      (Fuse_reduce.apply ?cancel ~grain ~position ?weight_attribute
                        ~attributes ~attribute_rules ~group_rules ~compact
                        ~rewire:fuse_points clusters geometry))
            else
            let output = if !moved = 0 then geometry else begin
              let x = Array.copy source.x and y = Array.copy source.y
              and z = Array.copy source.z in
              if count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
                  ~finish:(count - 1) (fun point ->
                    if point land 4095 = 0 then Cancel.check_opt cancel;
                    let destination = destinations.(point) in
                    if destination >= 0 then begin
                      x.(point) <- target_positions.x.(destination);
                      y.(point) <- target_positions.y.(destination);
                      z.(point) <- target_positions.z.(destination)
                    end);
              Geometry.with_positions
                (Packed.Float3.Private.of_owned_exn ~x ~y ~z) geometry
                |> Support.get_ok
                |> Geometry.without_attribute ~owner:Attribute.Point "N"
                |> Geometry.without_attribute ~owner:Attribute.Vertex "N"
            end in
            Result.bind (Fuse_target_rules.apply ?cancel ~grain ~attribute_rules
                ~group_rules ~destinations ~source:output ~target:target_geometry ())
              (fun output ->
            let output = install_outputs output in
            if not fuse_points || !mapped = 0 then Ok output
            else begin
              let selected = Group.init ~grain ~owner:Group.Point
                  ~name:"__rdk_fuse_snapped" count (fun point ->
                    destinations.(point) >= 0
                    || match destination_bits with
                       | None -> false
                       | Some bits -> Char.code
                           (Bytes.unsafe_get bits (point lsr 3))
                           land (1 lsl (point land 7)) <> 0) in
              fuse_clusters ?cancel ~grain ~selection:selected ~tolerance:0.
                ~position:First_position ~attributes ~metric:Euclidean
                ~attribute_rules:[] ~group_rules:[] ~inclusive:true ~match_attributes
                ~keep_fused_points output
            end)
            ))
  end)

let snap_to_grid ?cancel ?(grain = 16_384) ?selection
    ?(spacing = Rays_math.Vec3.create 1. 1. 1.) ?(offset = Rays_math.Vec3.zero)
    ?(rounding = Grid_nearest) ?max_distance ?(fuse_points = false)
    ?(position = Average_position) ?weight_attribute
    ?(attributes = Keep_first) ?(attribute_rules = []) ?(group_rules = [])
    ?snapped_group geometry =
  Error.guard ~operation:"snap_to_grid" ~code:"invalid_geometry" @@ fun () ->
  if grain <= 0 then invalid_arg "Rdk.Fuse_grid.snap_to_grid: grain must be positive";
  let count = Geometry.point_count geometry in
  let finite3 value = finite value.Rays_math.Vec3.x && finite value.y && finite value.z in
  if not (finite3 spacing) || spacing.x <= 0. || spacing.y <= 0.
      || spacing.z <= 0. then
    Error "Rdk.Fuse_grid.snap_to_grid: spacing must be finite and positive on every axis"
  else if not (finite3 offset) || offset.x < 0. || offset.x > 1.
      || offset.y < 0. || offset.y > 1. || offset.z < 0. || offset.z > 1.
  then Error "Rdk.Fuse_grid.snap_to_grid: offset fractions must be finite and in [0, 1]"
  else if match max_distance with
    | Some distance -> not (finite distance) || distance < 0.
    | None -> false
  then Error "Rdk.Fuse_grid.snap_to_grid: maximum distance must be finite and non-negative"
  else if match selection with
    | Some group -> Group.owner group <> Group.Point
        || Group.length group <> count
    | None -> false
  then Error "Rdk.Fuse_grid.snap_to_grid: selection must be a matching point group"
  else if match snapped_group with
    | Some name -> String.trim name = ""
    | None -> false
  then Error "Rdk.Fuse_grid.snap_to_grid: snapped group name must not be empty"
  else begin
    let source = Packed.Float3.Private.view (Geometry.positions geometry) in
    let x = Array.copy source.x and y = Array.copy source.y
    and z = Array.copy source.z in
    let byte_count = (count + 7) / 8 and byte_grain = max 1 (grain / 8) in
    let changed = Bytes.make byte_count '\000' in
    let range_count = if byte_count = 0 then 0
      else (byte_count + byte_grain - 1) / byte_grain in
    let errors = Array.make range_count (-1)
    and changed_counts = Array.make range_count 0 in
    let round = match rounding with
      | Grid_nearest -> fun value -> floor (value +. 0.5)
      | Grid_down -> floor
      | Grid_up -> ceil in
    let origin_x = spacing.x *. offset.x
    and origin_y = spacing.y *. offset.y
    and origin_z = spacing.z *. offset.z in
    if range_count > 0 then Parallel.for_ ~chunk_size:1 ~start:0
        ~finish:(range_count - 1) (fun range ->
      let first_byte = range * byte_grain
      and last_byte = min byte_count ((range + 1) * byte_grain) in
      let first = first_byte * 8 and last = min count (last_byte * 8) in
      for point = first to last - 1 do
        if point land 4095 = 0 then Cancel.check_opt cancel;
        if (match selection with None -> true
            | Some group -> Group.mem point group) then begin
          let sx = source.x.(point) and sy = source.y.(point)
          and sz = source.z.(point) in
          let qx = (sx -. origin_x) /. spacing.x
          and qy = (sy -. origin_y) /. spacing.y
          and qz = (sz -. origin_z) /. spacing.z in
          let nx = origin_x +. (spacing.x *. round qx)
          and ny = origin_y +. (spacing.y *. round qy)
          and nz = origin_z +. (spacing.z *. round qz) in
          if not (finite sx && finite sy && finite sz && finite qx && finite qy
              && finite qz && finite nx && finite ny && finite nz) then
            errors.(range) <- if errors.(range) < 0 then point else errors.(range)
          else begin
            let dx = nx -. sx and dy = ny -. sy and dz = nz -. sz in
            let within = match max_distance with
              | None -> true
              | Some distance -> Float.hypot dx (Float.hypot dy dz) <= distance in
            if within && (nx <> sx || ny <> sy || nz <> sz) then begin
              x.(point) <- nx; y.(point) <- ny; z.(point) <- nz;
              let byte = point lsr 3 and bit = 1 lsl (point land 7) in
              Bytes.unsafe_set changed byte
                (Char.chr (Char.code (Bytes.unsafe_get changed byte) lor bit));
              changed_counts.(range) <- changed_counts.(range) + 1
            end
          end
        end
      done);
    let invalid = Array.fold_left (fun earliest point ->
        if point < 0 then earliest else if earliest < 0 then point
        else min earliest point) (-1) errors in
    if invalid >= 0 then Error (Printf.sprintf
        "Rdk.Fuse_grid.snap_to_grid: selected point %d cannot be snapped to this grid"
        invalid)
    else
      let changed_count = Array.fold_left ( + ) 0 changed_counts in
      if changed_count = 0 && snapped_group = None && not fuse_points then
        Ok geometry
      else
        let output = if changed_count = 0 then geometry else
            Geometry.with_positions
              (Packed.Float3.Private.of_owned_exn ~x ~y ~z) geometry |> Support.get_ok
            |> Geometry.without_attribute ~owner:Attribute.Point "N"
            |> Geometry.without_attribute ~owner:Attribute.Vertex "N" in
        let output = match snapped_group with
          | None -> output
          | Some name ->
              let group = Group.Private.of_owned_bits ~owner:Group.Point ~name
                  ~length:count changed in
              Geometry.with_group group output |> Support.get_ok in
        if not fuse_points then Ok output
        else Error.unguard (fuse ?cancel ~grain ?selection ~tolerance:0. ~position
            ?weight_attribute ~attributes ~attribute_rules ~group_rules output)
  end
