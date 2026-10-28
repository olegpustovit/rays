
type compiled_attribute_rule = {
  pattern : Attribute_pattern.t;
  method_ : Fuse_rules.attribute_method;
  weight_attribute : string option;
}

type compiled_group_rule = {
  pattern : Attribute_pattern.t;
}

let fail format = Printf.ksprintf (fun message ->
    invalid_arg ("Rdk_mesh.Fuse_target_rules.fuse: " ^ message)) format

let get_ok = function Ok value -> value | Error message -> fail "%s" message

let run ?cancel ~grain count operation =
  if count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
      ~finish:(count - 1) (fun point ->
        Cancel.poll cancel point 4095;
        operation point)

let compile_attribute_rules rules = Array.of_list (List.map
    (fun (rule : Fuse_rules.attribute_rule) ->
      { pattern = Attribute_pattern.compile rule.pattern |> get_ok;
        method_ = rule.method_; weight_attribute = rule.weight_attribute }) rules)

let compile_group_rules rules = Array.of_list (List.map
    (fun (rule : Fuse_rules.group_rule) ->
      { pattern = Attribute_pattern.compile rule.group_pattern |> get_ok;
      }) rules)

let find_attribute_rule (rules : compiled_attribute_rule array) name =
  let found : compiled_attribute_rule option ref = ref None in
  Array.iter (fun (rule : compiled_attribute_rule) ->
    if Attribute_pattern.matches rule.pattern name then
    found := Some rule) rules;
  !found

let find_group_rule (rules : compiled_group_rule array) name =
  let found : compiled_group_rule option ref = ref None in
  Array.iter (fun (rule : compiled_group_rule) ->
    if Attribute_pattern.matches rule.pattern name then
    found := Some rule) rules;
  !found

let weighted = function
  | Fuse_rules.Attribute_weighted_average | Attribute_weighted_sum
  | Attribute_minimum_weight | Attribute_maximum_weight
  | Attribute_concatenate_weight_order -> true
  | Attribute_average | Attribute_least_point | Attribute_greatest_point
  | Attribute_maximum | Attribute_minimum | Attribute_mode | Attribute_median
  | Attribute_sum | Attribute_sum_squares | Attribute_root_mean_square
  | Attribute_concatenate -> false

let selection = function
  | Fuse_rules.Attribute_least_point | Attribute_greatest_point
  | Attribute_minimum_weight | Attribute_maximum_weight -> true
  | Attribute_average | Attribute_maximum | Attribute_minimum | Attribute_mode
  | Attribute_median | Attribute_sum | Attribute_sum_squares
  | Attribute_root_mean_square | Attribute_concatenate
  | Attribute_weighted_average | Attribute_weighted_sum
  | Attribute_concatenate_weight_order -> false

let concatenate = function
  | Fuse_rules.Attribute_concatenate | Attribute_concatenate_weight_order -> true
  | Attribute_average | Attribute_least_point | Attribute_greatest_point
  | Attribute_maximum | Attribute_minimum | Attribute_mode | Attribute_median
  | Attribute_sum | Attribute_sum_squares | Attribute_root_mean_square
  | Attribute_weighted_average | Attribute_weighted_sum
  | Attribute_minimum_weight | Attribute_maximum_weight -> false

let validate_weight target (rule : compiled_attribute_rule) =
  if weighted rule.method_ then
  match rule.weight_attribute with
  | None -> fail "fixed-target attribute rule requires a weight attribute"
  | Some name ->
      (match Geometry.find_attribute ~owner:Attribute.Point name target with
       | None -> fail "target weight point attribute %S is missing" name
       | Some attribute ->
           let check values = Array.iteri (fun point value ->
             if not (Float.is_finite value) then
               fail "target weight attribute %S is non-finite at point %d"
                 name point) values in
           match Attribute.View.storage attribute with
           | Attribute.Float values -> check values
           | Attribute.Int _ -> ()
           | _ -> fail "target weight attribute %S must be scalar float or integer"
               name)

let mapped_array ?cancel ~grain destinations source target =
  let output = Array.copy source in
  run ?cancel ~grain (Array.length destinations) (fun point ->
    let destination = destinations.(point) in
    if destination >= 0 then output.(point) <- target.(destination));
  output

let scalar_int_array ?cancel ~grain destinations source target =
  let count = Array.length destinations in
  let offsets = Array.init (count + 1) Fun.id
  and values = Array.copy source in
  run ?cancel ~grain count (fun point ->
    let destination = destinations.(point) in
    if destination >= 0 then values.(point) <- target.(destination));
  Packed.Int_array.View.create_validated_owned ~offsets ~values

let scalar_float_array ?cancel ~grain destinations source target =
  let count = Array.length destinations in
  let offsets = Array.init (count + 1) Fun.id
  and values = Array.copy source in
  run ?cancel ~grain count (fun point ->
    let destination = destinations.(point) in
    if destination >= 0 then values.(point) <- target.(destination));
  Packed.Float_array.View.create_validated_owned ~offsets ~values

let mapped_int_rows ?cancel ~grain destinations source target =
  let source = Packed.Int_array.View.view source
  and target = Packed.Int_array.View.view target in
  let count = Array.length destinations and offsets = Array.make
      (Array.length destinations + 1) 0 in
  for point = 0 to count - 1 do
    let destination = destinations.(point) in
    let length = if destination < 0
      then source.offsets.(point + 1) - source.offsets.(point)
      else target.offsets.(destination + 1) - target.offsets.(destination) in
    if offsets.(point) > Sys.max_array_length - length then
      fail "fixed-target integer array exceeds array limits";
    offsets.(point + 1) <- offsets.(point) + length
  done;
  let values = Array.make offsets.(count) 0 in
  run ?cancel ~grain count (fun point ->
    let destination = destinations.(point) in
    let input, first, last = if destination < 0 then
        source.values, source.offsets.(point), source.offsets.(point + 1)
      else target.values, target.offsets.(destination),
        target.offsets.(destination + 1) in
    Array.blit input first values offsets.(point) (last - first));
  Packed.Int_array.View.create_validated_owned ~offsets ~values

let mapped_float_rows ?cancel ~grain destinations source target =
  let source = Packed.Float_array.View.view source
  and target = Packed.Float_array.View.view target in
  let count = Array.length destinations and offsets = Array.make
      (Array.length destinations + 1) 0 in
  for point = 0 to count - 1 do
    let destination = destinations.(point) in
    let length = if destination < 0
      then source.offsets.(point + 1) - source.offsets.(point)
      else target.offsets.(destination + 1) - target.offsets.(destination) in
    if offsets.(point) > Sys.max_array_length - length then
      fail "fixed-target float array exceeds array limits";
    offsets.(point + 1) <- offsets.(point) + length
  done;
  let values = Array.make offsets.(count) 0. in
  run ?cancel ~grain count (fun point ->
    let destination = destinations.(point) in
    let input, first, last = if destination < 0 then
        source.values, source.offsets.(point), source.offsets.(point + 1)
      else target.values, target.offsets.(destination),
        target.offsets.(destination + 1) in
    Array.blit input first values offsets.(point) (last - first));
  Packed.Float_array.View.create_validated_owned ~offsets ~values

let target_attribute name target =
  match Geometry.find_attribute ~owner:Attribute.Point name target with
  | Some attribute -> attribute
  | None -> fail "target point attribute %S is missing" name

let apply_attribute ?cancel ~grain destinations target
    (rule : compiled_attribute_rule) source_attribute =
  validate_weight target rule;
  let name = Attribute.name source_attribute in
  let target_attribute = target_attribute name target in
  let incompatible () = fail
      "source and target point attribute %S storage differs or does not support this rule"
      name in
  let storage = match Attribute.View.storage source_attribute,
      Attribute.View.storage target_attribute with
    | Attribute.Float source, Attribute.Float target when concatenate rule.method_ ->
        Attribute.Float_array
          (scalar_float_array ?cancel ~grain destinations source target)
    | Attribute.Int source, Attribute.Int target when concatenate rule.method_ ->
        Attribute.Int_array
          (scalar_int_array ?cancel ~grain destinations source target)
    | Attribute.Float source, Attribute.Float target ->
        Attribute.Float (mapped_array ?cancel ~grain destinations source target)
    | Attribute.Int source, Attribute.Int target ->
        Attribute.Int (mapped_array ?cancel ~grain destinations source target)
    | Attribute.Text source, Attribute.Text target ->
        if not (selection rule.method_ || concatenate rule.method_
            || List.mem rule.method_ [Fuse_rules.Attribute_minimum;
              Attribute_maximum; Attribute_mode; Attribute_median]) then
          incompatible ();
        Attribute.Text (mapped_array ?cancel ~grain destinations source target)
    | Attribute.Float2 source, Attribute.Float2 target ->
        if concatenate rule.method_ then incompatible ();
        let source = Packed.Float2.View.view source
        and target = Packed.Float2.View.view target in
        Attribute.Float2 (Packed.Float2.of_owned
          ~x:(mapped_array ?cancel ~grain destinations source.x target.x)
          ~y:(mapped_array ?cancel ~grain destinations source.y target.y) |> get_ok)
    | Attribute.Float3 source, Attribute.Float3 target ->
        if concatenate rule.method_ then incompatible ();
        let source = Packed.Float3.View.view source
        and target = Packed.Float3.View.view target in
        Attribute.Float3 (Packed.Float3.View.of_owned_exn
          ~x:(mapped_array ?cancel ~grain destinations source.x target.x)
          ~y:(mapped_array ?cancel ~grain destinations source.y target.y)
          ~z:(mapped_array ?cancel ~grain destinations source.z target.z))
    | Attribute.Float4 source, Attribute.Float4 target ->
        if concatenate rule.method_ then incompatible ();
        let source = Packed.Float4.View.view source
        and target = Packed.Float4.View.view target in
        Attribute.Float4 (Packed.Float4.of_owned
          ~x:(mapped_array ?cancel ~grain destinations source.x target.x)
          ~y:(mapped_array ?cancel ~grain destinations source.y target.y)
          ~z:(mapped_array ?cancel ~grain destinations source.z target.z)
          ~w:(mapped_array ?cancel ~grain destinations source.w target.w) |> get_ok)
    | Attribute.Int_array source, Attribute.Int_array target
        when concatenate rule.method_ || selection rule.method_ ->
        Attribute.Int_array
          (mapped_int_rows ?cancel ~grain destinations source target)
    | Attribute.Float_array source, Attribute.Float_array target
        when concatenate rule.method_ || selection rule.method_ ->
        Attribute.Float_array
          (mapped_float_rows ?cancel ~grain destinations source target)
    | _ -> incompatible () in
  Attribute.create_owned ~owner:Attribute.Point ~name storage |> get_ok

let apply_groups ~grain rules destinations source target =
  let source_groups = Geometry.groups source in
  let source_names = Hashtbl.create (List.length source_groups) in
  List.iter (fun group -> if Group.owner group = Group.Point then
    Hashtbl.replace source_names (Group.name group) ()) source_groups;
  let append = Geometry.groups target |> List.filter (fun group ->
    Group.owner group = Group.Point
    && not (Hashtbl.mem source_names (Group.name group))
    && find_group_rule rules (Group.name group) <> None) in
  let groups = source_groups @ append in
  List.map (fun group ->
    if Group.owner group <> Group.Point then group
    else match find_group_rule rules (Group.name group) with
      | None -> group
      | Some _ ->
          let source_group = Geometry.find_group ~owner:Group.Point
              (Group.name group) source
          and target_group = Geometry.find_group ~owner:Group.Point
              (Group.name group) target in
          Group.init ~grain ~owner:Group.Point ~name:(Group.name group)
            (Geometry.point_count source) (fun point ->
              let destination = destinations.(point) in
              if destination >= 0 then match target_group with
                | Some group -> Group.mem destination group
                | None -> false
              else match source_group with Some group -> Group.mem point group
                | None -> false)) groups

let apply ?cancel ~grain ~attribute_rules ~group_rules ~destinations
    ~source ~target () =
  try
    if Array.length destinations <> Geometry.point_count source then
      fail "fixed-target destination count differs from source point count";
    let attribute_rules = compile_attribute_rules attribute_rules
    and group_rules = compile_group_rules group_rules in
    if Array.length attribute_rules = 0 && Array.length group_rules = 0 then Ok source
    else begin
      let attributes = List.map (fun attribute ->
        if Attribute.owner attribute <> Attribute.Point then attribute
        else match find_attribute_rule attribute_rules (Attribute.name attribute) with
          | None -> attribute
          | Some rule -> apply_attribute ?cancel ~grain destinations target rule
              attribute) (Geometry.attributes source) in
      let groups = apply_groups ~grain group_rules destinations source target in
      Geometry.create ~positions:(Geometry.positions source)
        ~topology:(Geometry.topology source) ~attributes ~groups
        ~edge_groups:(Geometry.edge_groups source) ()
    end
  with Invalid_argument message -> Error message
