
type default_attributes = Keep_first | Average_numeric

type attribute_method =
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

type attribute_rule = {
  pattern : string;
  method_ : attribute_method;
  weight_attribute : string option;
}

type group_method =
  | Group_least_point
  | Group_greatest_point
  | Group_union
  | Group_intersection
  | Group_most_common

type group_rule = { group_pattern : string; group_method : group_method }

type compiled_attribute_rule = {
  attribute_pattern : Attribute_pattern.t;
  attribute_method : attribute_method;
  attribute_weight : string option;
}

type compiled_group_rule = {
  group_pattern : Attribute_pattern.t;
  group_method : group_method;
}

let fail format = Printf.ksprintf (fun message ->
    invalid_arg ("Rdk_mesh.Fuse_rules.fuse: " ^ message)) format

let get_ok = function Ok value -> value | Error message -> fail "%s" message

let run ?cancel ~grain count operation =
  if count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
      ~finish:(count - 1) (fun index ->
        if index land 4095 = 0 then Cancel.check_opt cancel;
        operation index)

let compile_attribute_rules rules = Array.of_list (List.map (fun rule ->
    let needs_weight = match rule.method_ with
      | Attribute_weighted_average | Attribute_weighted_sum
      | Attribute_minimum_weight | Attribute_maximum_weight
      | Attribute_concatenate_weight_order -> true
      | Attribute_average | Attribute_least_point | Attribute_greatest_point
      | Attribute_maximum | Attribute_minimum | Attribute_mode
      | Attribute_median | Attribute_sum | Attribute_sum_squares
      | Attribute_root_mean_square | Attribute_concatenate -> false in
    if needs_weight && rule.weight_attribute = None then
      fail "attribute rule %S requires a weight attribute" rule.pattern;
    if needs_weight then Option.iter (fun name -> if String.trim name = "" then
      fail "attribute rule %S has an empty weight attribute" rule.pattern)
      rule.weight_attribute;
    { attribute_pattern = Attribute_pattern.compile rule.pattern |> get_ok;
      attribute_method = rule.method_;
      attribute_weight = if needs_weight then rule.weight_attribute else None })
    rules)

let compile_group_rules rules = Array.of_list (List.map (fun (rule : group_rule) ->
    { group_pattern = Attribute_pattern.compile rule.group_pattern |> get_ok;
      group_method = rule.group_method }) rules)

let find_attribute_rule rules name =
  let selected = ref None in
  Array.iter (fun rule ->
    if Attribute_pattern.matches rule.attribute_pattern name then
      selected := Some rule) rules;
  !selected

let find_group_rule rules name =
  let selected = ref None in
  Array.iter (fun rule ->
    if Attribute_pattern.matches rule.group_pattern name then
      selected := Some rule) rules;
  !selected

let representative_mapping clusters compact =
  if compact then clusters.Point_clusters.representatives
  else Array.init (Array.length clusters.of_point) (fun point ->
      clusters.representatives.(clusters.of_point.(point)))

let expand clusters compact values =
  if compact then values
  else Array.init (Array.length clusters.Point_clusters.of_point) (fun point ->
      values.(clusters.of_point.(point)))

let greatest_mapping clusters = Array.init clusters.Point_clusters.count
    (fun cluster -> clusters.members.(clusters.offsets.(cluster + 1) - 1))

let select source mapping = Array.init (Array.length mapping)
    (fun index -> source.(mapping.(index)))

let[@inline] compare_float_entry values points left right =
  let compared = Float.compare values.(left) values.(right) in
  if compared <> 0 then compared else Int.compare points.(left) points.(right)

let[@inline] compare_int_entry values points left right =
  let compared = Int.compare values.(left) values.(right) in
  if compared <> 0 then compared else Int.compare points.(left) points.(right)

let[@inline] compare_text_entry values points left right =
  let compared = String.compare values.(left) values.(right) in
  if compared <> 0 then compared else Int.compare points.(left) points.(right)

let swap values points left right =
  if left <> right then begin
    let value = values.(left) in values.(left) <- values.(right);
    values.(right) <- value;
    let point = points.(left) in points.(left) <- points.(right);
    points.(right) <- point
  end

let sift_down compare values points first root count =
  let root = ref root and active = ref true in
  while !active do
    let child = (!root * 2) + 1 in
    if child >= count then active := false
    else begin
      let selected = if child + 1 < count
          && compare values points (first + child) (first + child + 1) < 0
        then child + 1 else child in
      if compare values points (first + !root) (first + selected) < 0 then begin
        swap values points (first + !root) (first + selected);
        root := selected
      end else active := false
    end
  done

let sort_range compare values points first last =
  let count = last - first in
  for root = (count / 2) - 1 downto 0 do
    sift_down compare values points first root count
  done;
  for remaining = count - 1 downto 1 do
    swap values points first (first + remaining);
    sift_down compare values points first 0 remaining
  done

let sorted_float ?cancel ~grain clusters source method_ =
  let values = Array.init (Array.length clusters.Point_clusters.members)
      (fun slot -> source.(clusters.members.(slot)))
  and points = Array.copy clusters.members
  and output = Array.make clusters.count 0. in
  run ?cancel ~grain:(max 1 (grain / 8)) clusters.count (fun cluster ->
    let first = clusters.offsets.(cluster)
    and last = clusters.offsets.(cluster + 1) in
    sort_range compare_float_entry values points first last;
    if method_ = Attribute_median then
      output.(cluster) <- values.(first + ((last - first) / 2))
    else begin
      let best_count = ref 0 and best_value = ref values.(first)
      and run_first = ref first in
      let commit run_last =
        let count = run_last - !run_first in
        if count > !best_count then begin
          best_count := count; best_value := values.(!run_first)
        end in
      for slot = first + 1 to last - 1 do
        if Float.compare values.(slot) values.(!run_first) <> 0 then begin
          commit slot; run_first := slot
        end
      done;
      commit last;
      output.(cluster) <- !best_value
    end);
  output

let sorted_int ?cancel ~grain clusters source method_ =
  let values = Array.init (Array.length clusters.Point_clusters.members)
      (fun slot -> source.(clusters.members.(slot)))
  and points = Array.copy clusters.members
  and output = Array.make clusters.count 0 in
  run ?cancel ~grain:(max 1 (grain / 8)) clusters.count (fun cluster ->
    let first = clusters.offsets.(cluster)
    and last = clusters.offsets.(cluster + 1) in
    sort_range compare_int_entry values points first last;
    if method_ = Attribute_median then
      output.(cluster) <- values.(first + ((last - first) / 2))
    else begin
      let best_count = ref 0 and best_value = ref values.(first)
      and run_first = ref first in
      let commit run_last =
        let count = run_last - !run_first in
        if count > !best_count then begin
          best_count := count; best_value := values.(!run_first)
        end in
      for slot = first + 1 to last - 1 do
        if values.(slot) <> values.(!run_first) then begin
          commit slot; run_first := slot
        end
      done;
      commit last;
      output.(cluster) <- !best_value
    end);
  output

let sorted_text ?cancel ~grain clusters source method_ =
  let values = Array.init (Array.length clusters.Point_clusters.members)
      (fun slot -> source.(clusters.members.(slot)))
  and points = Array.copy clusters.members
  and output = Array.make clusters.count "" in
  run ?cancel ~grain:(max 1 (grain / 8)) clusters.count (fun cluster ->
    let first = clusters.offsets.(cluster)
    and last = clusters.offsets.(cluster + 1) in
    sort_range compare_text_entry values points first last;
    if method_ = Attribute_median then
      output.(cluster) <- values.(first + ((last - first) / 2))
    else begin
      let best_count = ref 0 and best_value = ref values.(first)
      and run_first = ref first in
      let commit run_last =
        let count = run_last - !run_first in
        if count > !best_count then begin
          best_count := count; best_value := values.(!run_first)
        end in
      for slot = first + 1 to last - 1 do
        if not (String.equal values.(slot) values.(!run_first)) then begin
          commit slot; run_first := slot
        end
      done;
      commit last;
      output.(cluster) <- !best_value
    end);
  output

let weight_values geometry name =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | None -> fail "attribute-rule weight point attribute %S is missing" name
  | Some attribute ->
      let values = match Attribute.Private.storage attribute with
        | Attribute.Float values -> Array.copy values
        | Attribute.Int values -> Array.map float_of_int values
        | _ -> fail "attribute-rule weight %S must be scalar float or integer"
            name in
      Array.iteri (fun point value -> if not (Float.is_finite value) then
        fail "attribute-rule weight %S is non-finite at point %d" name point)
        values;
      values

let validate ~attribute_rules ~group_rules geometry =
  try
    ignore geometry;
    ignore (compile_attribute_rules attribute_rules);
    ignore (compile_group_rules group_rules);
    Ok ()
  with Invalid_argument message -> Error message

let weight_mapping clusters weights minimum = Array.init clusters.Point_clusters.count
    (fun cluster ->
      let first = clusters.offsets.(cluster)
      and last = clusters.offsets.(cluster + 1) in
      let selected = ref clusters.members.(first) in
      for slot = first + 1 to last - 1 do
        let point = clusters.members.(slot) in
        if (if minimum then weights.(point) < weights.(!selected)
            else weights.(point) > weights.(!selected)) then selected := point
      done;
      !selected)

let source_mapping clusters method_ weights = match method_ with
  | Attribute_least_point -> Some clusters.Point_clusters.representatives
  | Attribute_greatest_point -> Some (greatest_mapping clusters)
  | Attribute_minimum_weight -> Some (weight_mapping clusters
      (Option.get weights) true)
  | Attribute_maximum_weight -> Some (weight_mapping clusters
      (Option.get weights) false)
  | Attribute_average | Attribute_maximum | Attribute_minimum
  | Attribute_mode | Attribute_median | Attribute_sum | Attribute_sum_squares
  | Attribute_root_mean_square | Attribute_concatenate
  | Attribute_weighted_average | Attribute_weighted_sum
  | Attribute_concatenate_weight_order -> None

let validate_float name source = Array.iteri (fun point value ->
    if not (Float.is_finite value) then
      fail "attribute %S is non-finite at point %d" name point) source

let average_float clusters source cluster =
  let first = clusters.Point_clusters.offsets.(cluster)
  and last = clusters.offsets.(cluster + 1) in
  let sum = ref 0. in
  for slot = first to last - 1 do
    sum := !sum +. source.(clusters.members.(slot))
  done;
  if Float.is_finite !sum then !sum /. float_of_int (last - first)
  else begin
    let scale = ref 0. in
    for slot = first to last - 1 do
      scale := Float.max !scale (abs_float source.(clusters.members.(slot)))
    done;
    let normalized = ref 0. in
    if !scale <> 0. then for slot = first to last - 1 do
      normalized := !normalized +. source.(clusters.members.(slot)) /. !scale
    done;
    (!normalized /. float_of_int (last - first)) *. !scale
  end

let reduce_float ?cancel ~grain ~name clusters source method_ weights =
  validate_float name source;
  match source_mapping clusters method_ weights with
  | Some mapping -> select source mapping
  | None when method_ = Attribute_mode || method_ = Attribute_median ->
      sorted_float ?cancel ~grain clusters source method_
  | None ->
      let output = Array.make clusters.Point_clusters.count 0. in
      run ?cancel ~grain:(max 1 (grain / 8)) clusters.count (fun cluster ->
        let first = clusters.offsets.(cluster)
        and last = clusters.offsets.(cluster + 1) in
        let value = match method_ with
          | Attribute_average -> average_float clusters source cluster
          | Attribute_minimum | Attribute_maximum ->
              let value = ref source.(clusters.members.(first)) in
              for slot = first + 1 to last - 1 do
                value := (if method_ = Attribute_minimum then Float.min
                          else Float.max) !value
                    source.(clusters.members.(slot))
              done;
              !value
          | Attribute_sum | Attribute_sum_squares ->
              let value = ref 0. in
              for slot = first to last - 1 do
                let item = source.(clusters.members.(slot)) in
                value := !value +. (if method_ = Attribute_sum_squares
                    then item *. item else item)
              done;
              !value
          | Attribute_root_mean_square ->
              let scale = ref 0. in
              for slot = first to last - 1 do
                scale := Float.max !scale
                    (abs_float source.(clusters.members.(slot)))
              done;
              let squares = ref 0. in
              if !scale <> 0. then for slot = first to last - 1 do
                let item = source.(clusters.members.(slot)) /. !scale in
                squares := !squares +. (item *. item)
              done;
              !scale *. sqrt (!squares /. float_of_int (last - first))
          | Attribute_weighted_average | Attribute_weighted_sum ->
              let weights = Option.get weights in
              let value = ref 0. and total = ref 0. in
              for slot = first to last - 1 do
                let point = clusters.members.(slot) in
                value := !value +. (source.(point) *. weights.(point));
                total := !total +. weights.(point)
              done;
              if method_ = Attribute_weighted_average then begin
                if !total = 0. then fail
                    "attribute %S weighted-average denominator is zero in cluster %d"
                    name cluster;
                !value /. !total
              end else !value
          | Attribute_least_point | Attribute_greatest_point
          | Attribute_mode | Attribute_median | Attribute_concatenate
          | Attribute_minimum_weight | Attribute_maximum_weight
          | Attribute_concatenate_weight_order -> assert false in
        if not (Float.is_finite value) then
          fail "attribute %S reduction is non-finite in cluster %d" name cluster;
        output.(cluster) <- value);
      output

let checked_add name cluster left right =
  if (right > 0 && left > max_int - right)
      || (right < 0 && left < min_int - right) then
    fail "integer attribute %S overflows in cluster %d" name cluster;
  left + right

let checked_square name cluster value =
  if value = min_int then fail "integer attribute %S square overflows in cluster %d"
      name cluster;
  let absolute = abs value in
  if absolute <> 0 && absolute > max_int / absolute then
    fail "integer attribute %S square overflows in cluster %d" name cluster;
  absolute * absolute

let int_of_float_checked name cluster value =
  let lower = float_of_int min_int in
  let upper = -. lower in
  if not (Float.is_finite value) || value < lower || value >= upper then
    fail "integer attribute %S reduction overflows in cluster %d" name cluster;
  int_of_float value

let reduce_int ?cancel ~grain ~name clusters source method_ weights =
  match source_mapping clusters method_ weights with
  | Some mapping -> select source mapping
  | None when method_ = Attribute_mode || method_ = Attribute_median ->
      sorted_int ?cancel ~grain clusters source method_
  | None ->
      let output = Array.make clusters.Point_clusters.count 0 in
      run ?cancel ~grain:(max 1 (grain / 8)) clusters.count (fun cluster ->
        let first = clusters.offsets.(cluster)
        and last = clusters.offsets.(cluster + 1) in
        let value = match method_ with
          | Attribute_minimum | Attribute_maximum ->
              let value = ref source.(clusters.members.(first)) in
              for slot = first + 1 to last - 1 do
                let candidate = source.(clusters.members.(slot)) in
                value := (if method_ = Attribute_minimum then Int.min
                          else Int.max) !value candidate
              done;
              !value
          | Attribute_average ->
              let count = last - first and quotients = ref 0
              and remainders = ref 0 in
              for slot = first to last - 1 do
                let value = source.(clusters.members.(slot)) in
                quotients := checked_add name cluster !quotients (value / count);
                remainders := checked_add name cluster !remainders (value mod count)
              done;
              checked_add name cluster !quotients (!remainders / count)
          | Attribute_sum ->
              let value = ref 0 in
              for slot = first to last - 1 do
                value := checked_add name cluster !value
                    source.(clusters.members.(slot))
              done;
              !value
          | Attribute_sum_squares ->
              let value = ref 0 in
              for slot = first to last - 1 do
                let square = checked_square name cluster
                    source.(clusters.members.(slot)) in
                value := checked_add name cluster !value square
              done;
              !value
          | Attribute_root_mean_square ->
              let scale = ref 0. in
              for slot = first to last - 1 do
                scale := Float.max !scale
                    (abs_float (float_of_int source.(clusters.members.(slot))))
              done;
              let squares = ref 0. in
              if !scale <> 0. then for slot = first to last - 1 do
                let item = float_of_int source.(clusters.members.(slot)) /. !scale in
                squares := !squares +. (item *. item)
              done;
              int_of_float_checked name cluster
                (!scale *. sqrt (!squares /. float_of_int (last - first)))
          | Attribute_weighted_average | Attribute_weighted_sum ->
              let weights = Option.get weights in
              let value = ref 0. and total = ref 0. in
              for slot = first to last - 1 do
                let point = clusters.members.(slot) in
                value := !value +. (float_of_int source.(point) *. weights.(point));
                total := !total +. weights.(point)
              done;
              if method_ = Attribute_weighted_average then begin
                if !total = 0. then fail
                    "attribute %S weighted-average denominator is zero in cluster %d"
                    name cluster;
                int_of_float_checked name cluster (!value /. !total)
              end else int_of_float_checked name cluster !value
          | Attribute_least_point | Attribute_greatest_point
          | Attribute_mode | Attribute_median | Attribute_concatenate
          | Attribute_minimum_weight | Attribute_maximum_weight
          | Attribute_concatenate_weight_order -> assert false in
        output.(cluster) <- value);
      output

let concatenate_text ?cancel ~grain clusters source ordered_members =
  let output = Array.make clusters.Point_clusters.count "" in
  run ?cancel ~grain:(max 1 (grain / 8)) clusters.count (fun cluster ->
    let first = clusters.offsets.(cluster)
    and last = clusters.offsets.(cluster + 1) and length = ref 0 in
    for slot = first to last - 1 do
      let item = String.length source.(ordered_members.(slot)) in
      if item > Sys.max_string_length - !length then
        fail "concatenated text exceeds string limits";
      length := !length + item
    done;
    let bytes = Bytes.create !length and at = ref 0 in
    for slot = first to last - 1 do
      let item = source.(ordered_members.(slot)) in
      Bytes.blit_string item 0 bytes !at (String.length item);
      at := !at + String.length item
    done;
    output.(cluster) <- Bytes.unsafe_to_string bytes);
  output

let reduce_text ?cancel ~grain ~name clusters source method_ weights
    ordered_members =
  match source_mapping clusters method_ weights with
  | Some mapping -> select source mapping
  | None when method_ = Attribute_mode || method_ = Attribute_median ->
      sorted_text ?cancel ~grain clusters source method_
  | None when method_ = Attribute_minimum || method_ = Attribute_maximum ->
      Array.init clusters.Point_clusters.count (fun cluster ->
        let first = clusters.offsets.(cluster)
        and last = clusters.offsets.(cluster + 1) in
        let value = ref source.(clusters.members.(first)) in
        for slot = first + 1 to last - 1 do
          let candidate = source.(clusters.members.(slot)) in
          if (if method_ = Attribute_minimum
              then String.compare candidate !value < 0
              else String.compare candidate !value > 0) then value := candidate
        done;
        !value)
  | None when method_ = Attribute_concatenate
      || method_ = Attribute_concatenate_weight_order ->
      concatenate_text ?cancel ~grain clusters source ordered_members
  | None -> fail "attribute %S method is unsupported for text storage" name

let weighted_order ?cancel ~grain clusters weights =
  let values = Array.init (Array.length clusters.Point_clusters.members)
      (fun slot -> weights.(clusters.members.(slot)))
  and points = Array.copy clusters.members in
  run ?cancel ~grain:(max 1 (grain / 8)) clusters.count (fun cluster ->
    sort_range compare_float_entry values points clusters.offsets.(cluster)
      clusters.offsets.(cluster + 1));
  points

let output_offsets clusters compact cluster_lengths =
  let count = if compact then clusters.Point_clusters.count
      else Array.length clusters.of_point in
  let offsets = Array.make (count + 1) 0 in
  for output = 0 to count - 1 do
    let cluster = if compact then output else clusters.of_point.(output) in
    let length = cluster_lengths.(cluster) in
    if offsets.(output) > Sys.max_array_length - length then
      fail "concatenated attribute exceeds array limits";
    offsets.(output + 1) <- offsets.(output) + length
  done;
  offsets

let concatenate_int_scalar ?cancel ~grain clusters compact source ordered =
  let lengths = Array.init clusters.Point_clusters.count (fun cluster ->
      clusters.offsets.(cluster + 1) - clusters.offsets.(cluster)) in
  let offsets = output_offsets clusters compact lengths
  and count = if compact then clusters.count else Array.length clusters.of_point in
  let values = Array.make (offsets.(count)) 0 in
  run ?cancel ~grain count (fun output ->
    let cluster = if compact then output else clusters.of_point.(output) in
    let at = ref offsets.(output) in
    for slot = clusters.offsets.(cluster) to clusters.offsets.(cluster + 1) - 1 do
      values.(!at) <- source.(ordered.(slot)); incr at
    done);
  Packed.Int_array.Private.create_validated_owned ~offsets ~values

let concatenate_float_scalar ?cancel ~grain clusters compact source ordered =
  let lengths = Array.init clusters.Point_clusters.count (fun cluster ->
      clusters.offsets.(cluster + 1) - clusters.offsets.(cluster)) in
  let offsets = output_offsets clusters compact lengths
  and count = if compact then clusters.count else Array.length clusters.of_point in
  let values = Array.make (offsets.(count)) 0. in
  run ?cancel ~grain count (fun output ->
    let cluster = if compact then output else clusters.of_point.(output) in
    let at = ref offsets.(output) in
    for slot = clusters.offsets.(cluster) to clusters.offsets.(cluster + 1) - 1 do
      values.(!at) <- source.(ordered.(slot)); incr at
    done);
  Packed.Float_array.Private.create_validated_owned ~offsets ~values

let concatenate_int_rows ?cancel ~grain clusters compact source ordered =
  let view = Packed.Int_array.Private.view source in
  let lengths = Array.make clusters.Point_clusters.count 0 in
  for cluster = 0 to clusters.count - 1 do
    for slot = clusters.offsets.(cluster) to clusters.offsets.(cluster + 1) - 1 do
      let point = ordered.(slot) in
      let length = view.offsets.(point + 1) - view.offsets.(point) in
      if lengths.(cluster) > Sys.max_array_length - length then
        fail "concatenated integer array exceeds array limits";
      lengths.(cluster) <- lengths.(cluster) + length
    done
  done;
  let offsets = output_offsets clusters compact lengths
  and count = if compact then clusters.count else Array.length clusters.of_point in
  let values = Array.make offsets.(count) 0 in
  run ?cancel ~grain count (fun output ->
    let cluster = if compact then output else clusters.of_point.(output) in
    let at = ref offsets.(output) in
    for slot = clusters.offsets.(cluster) to clusters.offsets.(cluster + 1) - 1 do
      let point = ordered.(slot) and first = view.offsets.(ordered.(slot)) in
      let length = view.offsets.(point + 1) - first in
      Array.blit view.values first values !at length; at := !at + length
    done);
  Packed.Int_array.Private.create_validated_owned ~offsets ~values

let concatenate_float_rows ?cancel ~grain clusters compact source ordered =
  let view = Packed.Float_array.Private.view source in
  let lengths = Array.make clusters.Point_clusters.count 0 in
  for cluster = 0 to clusters.count - 1 do
    for slot = clusters.offsets.(cluster) to clusters.offsets.(cluster + 1) - 1 do
      let point = ordered.(slot) in
      let length = view.offsets.(point + 1) - view.offsets.(point) in
      if lengths.(cluster) > Sys.max_array_length - length then
        fail "concatenated float array exceeds array limits";
      lengths.(cluster) <- lengths.(cluster) + length
    done
  done;
  let offsets = output_offsets clusters compact lengths
  and count = if compact then clusters.count else Array.length clusters.of_point in
  let values = Array.make offsets.(count) 0. in
  run ?cancel ~grain count (fun output ->
    let cluster = if compact then output else clusters.of_point.(output) in
    let at = ref offsets.(output) in
    for slot = clusters.offsets.(cluster) to clusters.offsets.(cluster + 1) - 1 do
      let point = ordered.(slot) and first = view.offsets.(ordered.(slot)) in
      let length = view.offsets.(point + 1) - first in
      Array.blit view.values first values !at length; at := !at + length
    done);
  Packed.Float_array.Private.create_validated_owned ~offsets ~values

let ordered_members ?cancel ~grain clusters method_ weights =
  if method_ = Attribute_concatenate_weight_order then
    weighted_order ?cancel ~grain clusters (Option.get weights)
  else clusters.Point_clusters.members

let reduced_attribute ?cancel ~grain ~compact clusters geometry weights_cache
    rule attribute =
  let name = Attribute.name attribute and method_ = rule.attribute_method in
  let weights = match rule.attribute_weight with
    | None -> None
    | Some name ->
        Some (match Hashtbl.find_opt weights_cache name with
          | Some values -> values
          | None -> let values = weight_values geometry name in
              Hashtbl.add weights_cache name values; values) in
  let ordered = ordered_members ?cancel ~grain clusters method_ weights in
  let plane_float source = expand clusters compact
      (reduce_float ?cancel ~grain ~name clusters source method_ weights) in
  let plane_int source = expand clusters compact
      (reduce_int ?cancel ~grain ~name clusters source method_ weights) in
  let storage = match Attribute.Private.storage attribute with
    | Attribute.Float values when method_ = Attribute_concatenate
        || method_ = Attribute_concatenate_weight_order ->
        Attribute.Float_array (concatenate_float_scalar ?cancel ~grain clusters
          compact values ordered)
    | Attribute.Int values when method_ = Attribute_concatenate
        || method_ = Attribute_concatenate_weight_order ->
        Attribute.Int_array (concatenate_int_scalar ?cancel ~grain clusters
          compact values ordered)
    | Attribute.Float values -> Attribute.Float (plane_float values)
    | Attribute.Int values -> Attribute.Int (plane_int values)
    | Attribute.Text values -> Attribute.Text (expand clusters compact
        (reduce_text ?cancel ~grain ~name clusters values method_ weights ordered))
    | Attribute.Float2 values ->
        if method_ = Attribute_concatenate
            || method_ = Attribute_concatenate_weight_order then
          fail "attribute %S concatenate requires scalar or array storage" name;
        let view = Packed.Float2.Private.view values in
        Attribute.Float2 (Packed.Float2.of_owned ~x:(plane_float view.x)
          ~y:(plane_float view.y) |> get_ok)
    | Attribute.Float3 values ->
        if method_ = Attribute_concatenate
            || method_ = Attribute_concatenate_weight_order then
          fail "attribute %S concatenate requires scalar or array storage" name;
        let view = Packed.Float3.Private.view values in
        Attribute.Float3 (Packed.Float3.Private.of_owned_exn
          ~x:(plane_float view.x) ~y:(plane_float view.y)
          ~z:(plane_float view.z))
    | Attribute.Float4 values ->
        if method_ = Attribute_concatenate
            || method_ = Attribute_concatenate_weight_order then
          fail "attribute %S concatenate requires scalar or array storage" name;
        let view = Packed.Float4.Private.view values in
        Attribute.Float4 (Packed.Float4.of_owned ~x:(plane_float view.x)
          ~y:(plane_float view.y) ~z:(plane_float view.z)
          ~w:(plane_float view.w) |> get_ok)
    | Attribute.Int_array values ->
        (match method_ with
         | Attribute_concatenate | Attribute_concatenate_weight_order ->
             Attribute.Int_array (concatenate_int_rows ?cancel ~grain clusters
               compact values ordered)
         | Attribute_least_point | Attribute_greatest_point
         | Attribute_minimum_weight | Attribute_maximum_weight ->
             let mapping = source_mapping clusters method_ weights |> Option.get in
             Attribute.Int_array (Ragged_ops.remap_int ?cancel ~grain
               (if compact then mapping else expand clusters false mapping) values)
         | _ -> fail "attribute %S method is unsupported for integer arrays" name)
    | Attribute.Float_array values ->
        (match method_ with
         | Attribute_concatenate | Attribute_concatenate_weight_order ->
             Attribute.Float_array (concatenate_float_rows ?cancel ~grain clusters
               compact values ordered)
         | Attribute_least_point | Attribute_greatest_point
         | Attribute_minimum_weight | Attribute_maximum_weight ->
             let mapping = source_mapping clusters method_ weights |> Option.get in
             Attribute.Float_array (Ragged_ops.remap_float ?cancel ~grain
               (if compact then mapping else expand clusters false mapping) values)
         | _ -> fail "attribute %S method is unsupported for float arrays" name) in
  Attribute.create_owned ~name ~owner:Attribute.Point storage |> get_ok

let default_attribute ?cancel ~grain ~compact clusters default attribute =
  let representatives = representative_mapping clusters compact in
  let average source = expand clusters compact (Array.init clusters.Point_clusters.count
      (fun cluster -> average_float clusters source cluster)) in
  let first source = select source representatives in
  let average_numeric = default = Average_numeric in
  let storage = match Attribute.Private.storage attribute with
    | Attribute.Float values ->
        Attribute.Float (if average_numeric then average values else first values)
    | Attribute.Int values -> Attribute.Int (first values)
    | Attribute.Text values -> Attribute.Text (first values)
    | Attribute.Float2 values ->
        let view = Packed.Float2.Private.view values in
        Attribute.Float2 (Packed.Float2.of_owned
          ~x:(if average_numeric then average view.x else first view.x)
          ~y:(if average_numeric then average view.y else first view.y) |> get_ok)
    | Attribute.Float3 values ->
        let view = Packed.Float3.Private.view values in
        let x = if average_numeric then average view.x else first view.x
        and y = if average_numeric then average view.y else first view.y
        and z = if average_numeric then average view.z else first view.z in
        if average_numeric && String.equal (Attribute.name attribute) "N" then
          run ?cancel ~grain (Array.length x) (fun point ->
            let length = sqrt ((x.(point) *. x.(point)) +. (y.(point) *. y.(point))
                +. (z.(point) *. z.(point))) in
            if length > 1e-20 then begin
              x.(point) <- x.(point) /. length;
              y.(point) <- y.(point) /. length;
              z.(point) <- z.(point) /. length
            end);
        Attribute.Float3 (Packed.Float3.Private.of_owned_exn ~x ~y ~z)
    | Attribute.Float4 values ->
        let view = Packed.Float4.Private.view values in
        let plane source = if average_numeric then average source else first source in
        Attribute.Float4 (Packed.Float4.of_owned ~x:(plane view.x)
          ~y:(plane view.y) ~z:(plane view.z) ~w:(plane view.w) |> get_ok)
    | Attribute.Int_array values -> Attribute.Int_array
        (Ragged_ops.remap_int ?cancel ~grain representatives values)
    | Attribute.Float_array values -> Attribute.Float_array
        (Ragged_ops.remap_float ?cancel ~grain representatives values) in
  Attribute.create_owned ~name:(Attribute.name attribute) ~owner:Attribute.Point
    storage |> get_ok

let cluster_membership clusters group method_ cluster =
  let first = clusters.Point_clusters.offsets.(cluster)
  and last = clusters.offsets.(cluster + 1) in
  match method_ with
  | Group_least_point -> Group.mem clusters.members.(first) group
  | Group_greatest_point -> Group.mem clusters.members.(last - 1) group
  | Group_union | Group_intersection | Group_most_common ->
      let members = ref 0 in
      for slot = first to last - 1 do
        if Group.mem clusters.members.(slot) group then incr members
      done;
      match method_ with
      | Group_union -> !members > 0
      | Group_intersection -> !members = last - first
      | Group_most_common -> !members * 2 > last - first
      | Group_least_point | Group_greatest_point -> assert false

let with_cluster_order clusters compact source target =
  match Group.Private.order_view source with
  | None -> target
  | Some source_order ->
      let cluster_count = clusters.Point_clusters.count in
      let seen = Bytes.make ((cluster_count + 7) / 8) '\000'
      and order = Array.make (Group.cardinality target) 0 and at = ref 0 in
      let visit cluster =
        let byte = cluster lsr 3 and mask = 1 lsl (cluster land 7) in
        let current = Char.code (Bytes.unsafe_get seen byte) in
        if current land mask = 0 then begin
          Bytes.unsafe_set seen byte (Char.chr (current lor mask));
          if compact then begin
            if Group.mem cluster target then begin order.(!at) <- cluster; incr at end
          end else if Group.mem clusters.members.(clusters.offsets.(cluster)) target
          then for slot = clusters.offsets.(cluster)
              to clusters.offsets.(cluster + 1) - 1 do
            order.(!at) <- clusters.members.(slot); incr at
          done
        end in
      Array.iter (fun point -> visit clusters.of_point.(point)) source_order;
      for cluster = 0 to cluster_count - 1 do visit cluster done;
      Group.Private.with_owned_order order target

let reduced_group ~grain ~compact clusters method_ group =
  let cluster_group = Group.init ~grain ~owner:Group.Point
      ~name:(Group.name group) clusters.Point_clusters.count
      (cluster_membership clusters group method_) in
  let target = if compact then cluster_group else
      Group.init ~grain ~owner:Group.Point ~name:(Group.name group)
        (Array.length clusters.of_point) (fun point ->
          Group.mem clusters.of_point.(point) cluster_group) in
  with_cluster_order clusters compact group target

let apply ?cancel ~grain ~default_attributes ~attribute_rules ~group_rules
    ~compact ~rewire clusters geometry =
  let attribute_rules = compile_attribute_rules attribute_rules
  and group_rules = compile_group_rules group_rules in
  let weights_cache = Hashtbl.create 4 in
  let attributes = List.map (fun attribute ->
    if Attribute.owner attribute <> Attribute.Point then attribute
    else match find_attribute_rule attribute_rules (Attribute.name attribute) with
      | Some rule -> reduced_attribute ?cancel ~grain ~compact clusters geometry
          weights_cache rule attribute
      | None when Array.length attribute_rules > 0 && not compact && not rewire ->
          attribute
      | None -> default_attribute ?cancel ~grain ~compact clusters
          default_attributes attribute) (Geometry.attributes geometry) in
  let groups = List.map (fun group ->
    if Group.owner group <> Group.Point then group
    else match find_group_rule group_rules (Group.name group) with
      | Some rule -> reduced_group ~grain ~compact clusters
          rule.group_method group
      | None when Array.length group_rules > 0 && not compact && not rewire -> group
      | None -> reduced_group ~grain ~compact clusters Group_union group)
      (Geometry.groups geometry) in
  attributes, groups
