
type position =
  | First_position
  | Least_point_position
  | Greatest_point_position
  | Average_position
  | Minimum_position
  | Maximum_position
  | Mode_position
  | Median_position
  | Sum_position
  | Sum_squares_position
  | Root_mean_square_position
  | Weighted_average_position
  | Weighted_sum_position
  | Minimum_weight_position
  | Maximum_weight_position

type attributes = Fuse_rules.default_attributes = Keep_first | Average_numeric

type attribute_method = Fuse_rules.attribute_method =
  | Attribute_average
  | Attribute_least_point
  | Attribute_greatest_point
  | Attribute_maximum
  | Attribute_minimum
  | Attribute_mode
  | Attribute_median
  | Attribute_sum
  | Attribute_sum_squares
  | Attribute_root_mean_square
  | Attribute_concatenate
  | Attribute_weighted_average
  | Attribute_weighted_sum
  | Attribute_minimum_weight
  | Attribute_maximum_weight
  | Attribute_concatenate_weight_order

type attribute_rule = Fuse_rules.attribute_rule = {
  pattern : string;
  method_ : attribute_method;
  weight_attribute : string option;
}

type group_method = Fuse_rules.group_method =
  | Group_least_point
  | Group_greatest_point
  | Group_union
  | Group_intersection
  | Group_most_common

type group_rule = Fuse_rules.group_rule = {
  group_pattern : string;
  group_method : group_method;
}

exception Invalid of string

let get_ok = function Ok value -> value | Error message -> raise (Invalid message)

let average_plane ~grain clusters source =
  let output = Array.make clusters.Point_clusters.count 0. in
  Parallel.for_ ~chunk_size:(max 1 (grain / 8)) ~start:0
    ~finish:(clusters.count - 1) (fun cluster ->
      let sum = ref 0. in
      for slot = clusters.offsets.(cluster) to clusters.offsets.(cluster + 1) - 1 do
        sum := !sum +. source.(clusters.members.(slot))
      done;
      let count = clusters.offsets.(cluster + 1) - clusters.offsets.(cluster) in
      if Float.is_finite !sum then output.(cluster) <- !sum /. float_of_int count
      else begin
        let scale = ref 0. in
        for slot = clusters.offsets.(cluster) to clusters.offsets.(cluster + 1) - 1 do
          scale := Float.max !scale (abs_float source.(clusters.members.(slot)))
        done;
        let normalized = ref 0. in
        if !scale <> 0. then
          for slot = clusters.offsets.(cluster) to clusters.offsets.(cluster + 1) - 1 do
            normalized := !normalized
              +. (source.(clusters.members.(slot)) /. !scale)
          done;
        output.(cluster) <- (!normalized /. float_of_int count) *. !scale
      end);
  output

let sorted_reduction ~grain clusters source mode =
  let values = Array.init (Array.length clusters.Point_clusters.members)
      (fun slot -> source.(clusters.members.(slot)))
  and points = Array.copy clusters.members
  and output = Array.make clusters.count 0. in
  Parallel.for_ ~chunk_size:(max 1 (grain / 8)) ~start:0
    ~finish:(clusters.count - 1) (fun cluster ->
      let first = clusters.offsets.(cluster)
      and last = clusters.offsets.(cluster + 1) in
      Fuse_rules.sort_range Fuse_rules.compare_float_entry values points first last;
      match mode with
      | `Median -> output.(cluster) <- values.(first + ((last - first) / 2))
      | `Mode ->
          let best_count = ref 0 and best_first_point = ref max_int
          and best_value = ref values.(first) and run_first = ref first in
          let commit run_last =
            let count = run_last - !run_first in
            let point = points.(!run_first) in
            if count > !best_count
                || (count = !best_count && point < !best_first_point) then begin
              best_count := count; best_first_point := point;
              best_value := values.(!run_first)
            end in
          for slot = first + 1 to last - 1 do
            if Float.compare values.(slot) values.(!run_first) <> 0 then begin
              commit slot; run_first := slot
            end
          done;
          commit last;
          output.(cluster) <- !best_value);
  output

let numeric_reduction ~grain clusters source position weights =
  match position with
  | First_position | Least_point_position ->
      Fuse_rules.select source clusters.Point_clusters.representatives
  | Greatest_point_position -> Array.init clusters.count (fun cluster ->
      source.(clusters.members.(clusters.offsets.(cluster + 1) - 1)))
  | Average_position -> average_plane ~grain clusters source
  | Median_position -> sorted_reduction ~grain clusters source `Median
  | Mode_position -> sorted_reduction ~grain clusters source `Mode
  | Minimum_position | Maximum_position | Sum_position | Sum_squares_position
  | Root_mean_square_position | Weighted_average_position
  | Weighted_sum_position ->
      let output = Array.make clusters.count 0. in
      Parallel.for_ ~chunk_size:(max 1 (grain / 8)) ~start:0
        ~finish:(clusters.count - 1) (fun cluster ->
          let first = clusters.offsets.(cluster)
          and last = clusters.offsets.(cluster + 1) in
          let value = ref source.(clusters.members.(first)) in
          (match position with
           | Minimum_position | Maximum_position ->
               for slot = first + 1 to last - 1 do
                 let candidate = source.(clusters.members.(slot)) in
                 value := (if position = Minimum_position then Float.min
                           else Float.max) !value candidate
               done
           | Sum_position ->
               value := 0.;
               for slot = first to last - 1 do
                 value := !value +. source.(clusters.members.(slot))
               done
           | Sum_squares_position ->
               value := 0.;
               for slot = first to last - 1 do
                 let item = source.(clusters.members.(slot)) in
                 value := !value +. (item *. item)
               done
           | Root_mean_square_position ->
               let scale = ref 0. in
               for slot = first to last - 1 do
                 scale := Float.max !scale
                   (abs_float source.(clusters.members.(slot)))
               done;
               let squares = ref 0. in
               if !scale <> 0. then
                 for slot = first to last - 1 do
                   let item = source.(clusters.members.(slot)) /. !scale in
                   squares := !squares +. (item *. item)
                 done;
               value := !scale *. sqrt (!squares /. float_of_int (last - first))
           | Weighted_average_position | Weighted_sum_position ->
               let weights = Option.get weights in
               let weighted = ref 0. and total = ref 0. in
               for slot = first to last - 1 do
                 let point = clusters.members.(slot) in
                 weighted := !weighted +. (source.(point) *. weights.(point));
                 total := !total +. weights.(point)
               done;
               if position = Weighted_average_position then begin
                 if !total = 0. then raise (Invalid (Printf.sprintf
                     "Rdk.Fuse_reduce.fuse: position weight sum is zero for cluster %d"
                     cluster));
                 value := !weighted /. !total
               end else value := !weighted
           | _ -> assert false);
          output.(cluster) <- !value);
      output
  | Minimum_weight_position | Maximum_weight_position -> assert false

let weight_values name geometry =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | None -> raise (Invalid (Printf.sprintf
      "Rdk.Fuse_reduce.fuse: position weight point attribute %S is missing" name))
  | Some attribute ->
      let values = match Attribute.View.storage attribute with
        | Attribute.Float values -> Array.copy values
        | Attribute.Int values -> Array.map float_of_int values
        | _ -> raise (Invalid (Printf.sprintf
            "Rdk.Fuse_reduce.fuse: position weight attribute %S must be scalar float or integer"
            name)) in
      Array.iteri (fun point value -> if not (Float.is_finite value) then
        raise (Invalid (Printf.sprintf
          "Rdk.Fuse_reduce.fuse: position weight attribute %S is non-finite at point %d"
          name point))) values;
      values

let selected_by_weight clusters weights minimum =
  Array.init clusters.Point_clusters.count (fun cluster ->
    let first = clusters.offsets.(cluster) and last = clusters.offsets.(cluster + 1) in
    let selected = ref clusters.members.(first) in
    for slot = first + 1 to last - 1 do
      let point = clusters.members.(slot) in
      if (if minimum then weights.(point) < weights.(!selected)
          else weights.(point) > weights.(!selected)) then selected := point
    done;
    !selected)

let validate_positions x y z =
  for point = 0 to Array.length x - 1 do
    if not (Float.is_finite x.(point) && Float.is_finite y.(point) && Float.is_finite z.(point)) then
      raise (Invalid (Printf.sprintf
        "Rdk.Fuse_reduce.fuse: position reduction is non-finite at output point %d" point))
  done

let apply ?cancel ~grain ~position ?weight_attribute ~attributes
    ~attribute_rules ~group_rules ~compact ~rewire
    ?(remap_edge_groups = true) clusters geometry =
  try
    let source_count = Geometry.point_count geometry in
    let weighted = match position with Weighted_average_position
        | Weighted_sum_position | Minimum_weight_position
        | Maximum_weight_position -> true | _ -> false in
    let weights = match weighted, weight_attribute with
      | true, None -> raise (Invalid
          "Rdk.Fuse_reduce.fuse: weighted position reduction requires a weight attribute")
      | true, Some name when String.trim name = "" -> raise (Invalid
          "Rdk.Fuse_reduce.fuse: position weight attribute name must not be empty")
      | true, Some name -> Some (weight_values name geometry)
      | false, _ -> None in
    let positions = Packed.Float3.View.view (Geometry.positions geometry) in
    let selected = match position with
      | Minimum_weight_position -> Some (selected_by_weight clusters
          (Option.get weights) true)
      | Maximum_weight_position -> Some (selected_by_weight clusters
          (Option.get weights) false)
      | _ -> None in
    let cluster_plane source = match selected with
      | Some mapping -> Fuse_rules.select source mapping
      | None -> numeric_reduction ~grain clusters source position weights in
    let px = Fuse_rules.expand clusters compact (cluster_plane positions.x)
    and py = Fuse_rules.expand clusters compact (cluster_plane positions.y)
    and pz = Fuse_rules.expand clusters compact (cluster_plane positions.z) in
    validate_positions px py pz;
    let output_positions = Packed.Float3.View.of_owned_exn ~x:px ~y:py ~z:pz in
    let point_map = if compact then clusters.of_point
      else if rewire then Array.init source_count (fun point ->
        clusters.representatives.(clusters.of_point.(point)))
      else Array.init source_count Fun.id in
    let source_topology_value = Geometry.topology geometry in
    let source_topology = Topology.View.view source_topology_value in
    let output_topology = if not compact && not rewire then source_topology_value
      else begin
        let vertex_points = Array.make (Geometry.vertex_count geometry) 0 in
        if Array.length vertex_points > 0 then Parallel.for_ ~chunk_size:grain
            ~start:0 ~finish:(Array.length vertex_points - 1) (fun vertex ->
              if vertex land 16383 = 0 then Cancel.check_opt cancel;
              vertex_points.(vertex) <- point_map.(source_topology.vertex_points.(vertex)));
        Topology.View.create_validated_owned
          ~point_count:(if compact then clusters.count else source_count)
          ~vertex_points ~primitive_offsets:(Array.copy source_topology.primitive_offsets)
          ~primitive_kinds:(Bytes.copy source_topology.primitive_kinds)
      end in
    let output_attributes, output_groups = Fuse_rules.apply ?cancel ~grain
        ~default_attributes:attributes ~attribute_rules ~group_rules ~compact
        ~rewire clusters geometry in
    let edge_groups = if not remap_edge_groups then []
      else if not compact && not rewire then Geometry.edge_groups geometry
      else match Geometry.edge_groups geometry with
      | [] -> []
      | source_groups ->
          let source_index = Topology_index.create ?cancel source_topology_value
          and target_index = Topology_index.create ?cancel output_topology in
          List.map (fun group -> Edge_group.remap ?cancel ~source_index
            ~target_topology:output_topology ~target_index ~point_map group
            |> get_ok) source_groups in
    Geometry.create ~positions:output_positions ~topology:output_topology
      ~attributes:output_attributes ~groups:output_groups ~edge_groups ()
  with
  | Invalid message -> Error message
  | Invalid_argument message -> Error message
