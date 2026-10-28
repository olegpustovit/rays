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

type fuse_attribute_view =
  | Fuse_float of float array
  | Fuse_int of int array
  | Fuse_float2 of Packed.Float2.View.view
  | Fuse_float3 of Packed.Float3.View.view
  | Fuse_float4 of Packed.Float4.View.view
  | Fuse_int_array of Packed.Int_array.View.view
  | Fuse_float_array of Packed.Float_array.View.view
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
                else Some (match Attribute.View.storage attribute with
                  | Attribute.Float values -> Fuse_float values
                  | Attribute.Int values -> Fuse_int values
                  | Attribute.Text values -> Fuse_text values
                  | Attribute.Float2 values ->
                      Fuse_float2 (Packed.Float2.View.view values)
                  | Attribute.Float3 values ->
                      Fuse_float3 (Packed.Float3.View.view values)
                  | Attribute.Float4 values ->
                      Fuse_float4 (Packed.Float4.View.view values)
                  | Attribute.Int_array values ->
                      Fuse_int_array (Packed.Int_array.View.view values)
                  | Attribute.Float_array values ->
                      Fuse_float_array (Packed.Float_array.View.view values)))
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

type params = {
  cancel : Rdk_core.Cancel.t option;
  grain : int;
  selection : Rdk_core.Group.t option;
  target_selection : Rdk_core.Group.t option;
  targeting : fuse_targeting;
  using : fuse_using;
  tolerance : float;
  position : Fuse_reduce.position;
  weight_attribute : string option;
  attributes : Fuse_reduce.attributes;
  attribute_rules : Fuse_reduce.attribute_rule list;
  group_rules : Fuse_reduce.group_rule list;
  metric : fuse_metric;
  inclusive : bool;
  match_attributes : bool;
  radius_attribute : string option;
  match_attribute : string option;
  match_condition : fuse_match_condition;
  match_tolerance : float;
  modify_target : bool;
  fuse_points : bool;
  keep_fused_points : bool;
  snapped_group : string option;
  snapped_destination_attribute : string option;
  remove_degenerate_primitives : bool;
  remove_unused_points_from_degenerate_primitives : bool;
  remove_all_unused_points : bool;
  target : Rdk_core.Geometry.t option;
}

let fuse ({ cancel; grain; selection; target_selection; targeting; using; tolerance; position; weight_attribute; attributes; attribute_rules; group_rules; metric; inclusive; match_attributes; radius_attribute; match_attribute; match_condition; match_tolerance; modify_target; fuse_points; keep_fused_points; snapped_group; snapped_destination_attribute; remove_degenerate_primitives; remove_unused_points_from_degenerate_primitives; remove_all_unused_points; target } : params) geometry =
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
            let source = Packed.Float3.View.view
                (Geometry.positions geometry)
            and target_positions = Packed.Float3.View.view
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
                (Packed.Float3.View.of_owned_exn ~x ~y ~z) geometry
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

let weld ?cancel ~grain ?selection ~tolerance geometry =
  fuse { cancel; grain; selection; target_selection = None; targeting = Near_points;
    using = Least_target_point; tolerance; position = Average_position; weight_attribute = None;
    attributes = Keep_first; attribute_rules = []; group_rules = []; metric = Euclidean;
    inclusive = true; match_attributes = false; radius_attribute = None; match_attribute = None;
    match_condition = Equal_attribute_values; match_tolerance = 0.; modify_target = false;
    fuse_points = true; keep_fused_points = false; snapped_group = None;
    snapped_destination_attribute = None; remove_degenerate_primitives = false;
    remove_unused_points_from_degenerate_primitives = false; remove_all_unused_points = false;
    target = None } geometry
