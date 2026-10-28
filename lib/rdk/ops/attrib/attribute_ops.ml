



let delete ?cancel ?reference ?delete_non_selected ?point_pattern
    ?vertex_pattern ?primitive_pattern ?detail_pattern geometry =
  try Result.map_error
      (Error.make ~operation:"attribute_delete" ~code:"invalid_attribute")
      (Attribute_lifecycle.delete ?cancel ?reference ?delete_non_selected
         ?point_pattern ?vertex_pattern ?primitive_pattern ?detail_pattern
         geometry)
  with Cancel.Cancelled -> Error (Error.make ~operation:"attribute_delete"
      ~code:"cancelled" "attribute deletion was cancelled")

let rename ?cancel ~rules geometry =
  try Result.map_error
      (Error.make ~operation:"attribute_rename" ~code:"invalid_attribute")
      (Attribute_lifecycle.rename ?cancel ~rules geometry)
  with Cancel.Cancelled -> Error (Error.make ~operation:"attribute_rename"
      ~code:"cancelled" "attribute renaming was cancelled")



let swap ?cancel ~rules geometry =
  try Result.map_error
      (Error.make ~operation:"attribute_swap" ~code:"invalid_attribute")
      (Attribute_lifecycle.swap ?cancel ~rules geometry)
  with Cancel.Cancelled -> Error (Error.make ~operation:"attribute_swap"
      ~code:"cancelled" "attribute swap was cancelled")



type interpolate_attribute = {
  interpolate_owner : Attribute.owner;
  interpolate_source : string;
  interpolate_target : string;
}
type interpolate_driver =
  | Primitive_uvw of {
      primitive_attribute : string;
      uvw_attribute : string;
    }
  | Point_weights of {
      numbers_attribute : string;
      weights_attribute : string;
    }
  | Vertex_weights of {
      numbers_attribute : string;
      weights_attribute : string;
    }
  | Primitive_weights of {
      numbers_attribute : string;
      weights_attribute : string;
    }
type interpolate_computed = {
  computed_owner : Attribute.owner;
  computed_numbers_attribute : string;
  computed_weights_attribute : string;
}


let blur_points = Attribute_blur.run

let interpolate_attribute ?into ~owner name =
  if String.trim name = "" then invalid_arg
      "Rdk.Attribute_ops.interpolate_attribute: empty source name";
  if String.equal name "P" && owner <> Attribute.Point then invalid_arg
      "Rdk.Attribute_ops.interpolate_attribute: canonical P must be point owned";
  let target = Option.value ~default:name into in
  if String.trim target = "" then invalid_arg
      "Rdk.Attribute_ops.interpolate_attribute: empty target name";
  { interpolate_owner = owner; interpolate_source = name;
    interpolate_target = target }

let expand_interpolate_patterns ~match_groups ?point_pattern ?vertex_pattern
    ?primitive_pattern ?detail_pattern source =
  let compile owner = function
    | None -> Ok (owner, None)
    | Some source -> Result.map (fun pattern -> owner, Some pattern)
        (Attribute_pattern.compile source) in
  let patterns_result = [
    compile Attribute.Point point_pattern;
    compile Attribute.Vertex vertex_pattern;
    compile Attribute.Primitive primitive_pattern;
    compile Attribute.Detail detail_pattern] in
  Result.bind (List.fold_left (fun result item ->
    Result.bind result (fun values -> Result.map (fun value -> value :: values)
      item)) (Ok []) patterns_result) (fun reversed ->
  let patterns = Array.of_list (List.rev reversed) in
  let pattern owner = Array.find_map (fun (candidate, pattern) ->
      if candidate = owner then pattern else None) patterns in
  let output = ref [] and groups = ref [] in
  (match pattern Attribute.Point with
   | Some pattern when Attribute_pattern.matches pattern "P" ->
       output := interpolate_attribute ~owner:Attribute.Point "P" :: !output
   | None | Some _ -> ());
  List.iter (fun attribute ->
    match pattern (Attribute.owner attribute) with
    | Some pattern when Attribute_pattern.matches pattern
        (Attribute.name attribute) ->
        output := interpolate_attribute ~owner:(Attribute.owner attribute)
            (Attribute.name attribute) :: !output
    | None | Some _ -> ()) (Geometry.attributes source);
  if match_groups then List.iter (fun group ->
    let owner = match Group.owner group with
      | Group.Point -> Attribute.Point
      | Group.Vertex -> Attribute.Vertex
      | Group.Primitive -> Attribute.Primitive in
    match pattern owner with
    | Some pattern when Attribute_pattern.matches pattern (Group.name group) ->
        groups := {
          Attribute_interpolate.group_source_owner = owner;
          group_source = group } :: !groups
    | None | Some _ -> ()) (Geometry.groups source);
  Ok (List.rev !output, List.rev !groups))

type params = {
  cancel : Cancel.t option;
  grain : int option;
  selection : Group.t option;
  driver : interpolate_driver option;
  compute_weights : interpolate_computed option;
  point_pattern : string option;
  vertex_pattern : string option;
  primitive_pattern : string option;
  detail_pattern : string option;
  match_groups : bool;
  pre_scale : float option;
  normalize_weights : bool option;
  threshold : float option;
  blend : float option;
  unmatched : Attribute_transfer.unmatched;
  target_owner : Attribute.owner;
  attributes : interpolate_attribute list;
  source : Geometry.t;
  target : Geometry.t;
}

let interpolate ({ cancel; grain; selection; driver; compute_weights; point_pattern; vertex_pattern;
    primitive_pattern; detail_pattern; match_groups; pre_scale; normalize_weights; threshold;
    blend; unmatched; target_owner; attributes; source; target } : params) () =
  let unmatched = match unmatched with
    | Keep_target -> Attribute_interpolate.Keep_target
    | Default_value -> Attribute_interpolate.Default_value in
  let compute_result = match compute_weights with
    | None -> Ok None
    | Some value ->
        let computed_owner = match value.computed_owner with
          | Attribute.Point -> Ok Attribute_interpolate.Points
          | Attribute.Vertex -> Ok Attribute_interpolate.Vertices
          | Attribute.Primitive | Attribute.Detail -> Error
              "Attribute Interpolate: computed weights require point or vertex ownership" in
        Result.map (fun computed_owner -> Some {
          Attribute_interpolate.computed_owner;
          computed_numbers_attribute = value.computed_numbers_attribute;
          computed_weights_attribute = value.computed_weights_attribute })
          computed_owner in
  let driver = Option.value driver ~default:(Primitive_uvw {
    primitive_attribute = "source_primitive"; uvw_attribute = "source_uvw" }) in
  try Result.bind (Result.map_error
      (Error.make ~operation:"attribute_interpolate"
        ~code:"invalid_interpolate")
      (expand_interpolate_patterns ~match_groups ?point_pattern ?vertex_pattern
        ?primitive_pattern ?detail_pattern source)) (fun (expanded, groups) ->
    let attributes = List.map (fun attribute -> {
      Attribute_interpolate.source_owner = attribute.interpolate_owner;
      source_name = attribute.interpolate_source;
      target_name = attribute.interpolate_target }) (attributes @ expanded) in
    Result.bind (Result.map_error
      (Error.make ~operation:"attribute_interpolate"
        ~code:"invalid_interpolate") compute_result) (fun compute ->
    Result.map_error
      (Error.make ~operation:"attribute_interpolate"
        ~code:"invalid_interpolate")
      (match driver with
       | Primitive_uvw { primitive_attribute; uvw_attribute } ->
           Attribute_interpolate.interpolate_primitive ?cancel ?grain ?selection
             ?compute ~primitive_attribute ~uvw_attribute ?pre_scale ?blend ~unmatched
             ~target_owner ~attributes ~groups ~source ~target ()
       | Point_weights { numbers_attribute; weights_attribute } ->
           if Option.is_some compute then Error
               "Attribute Interpolate: computed weights require primitive/UVW mode"
           else
           Attribute_interpolate.interpolate_weighted ?cancel ?grain ?selection
             ~numbers_attribute ~weights_attribute ?pre_scale ?normalize_weights
             ?threshold ?blend ~unmatched
             ~weighted_owner:Attribute_interpolate.Points ~target_owner
             ~attributes ~groups ~source ~target ()
       | Vertex_weights { numbers_attribute; weights_attribute } ->
           if Option.is_some compute then Error
               "Attribute Interpolate: computed weights require primitive/UVW mode"
           else
           Attribute_interpolate.interpolate_weighted ?cancel ?grain ?selection
             ~numbers_attribute ~weights_attribute ?pre_scale ?normalize_weights
             ?threshold ?blend ~unmatched
             ~weighted_owner:Attribute_interpolate.Vertices ~target_owner
             ~attributes ~groups ~source ~target ()
       | Primitive_weights { numbers_attribute; weights_attribute } ->
           if Option.is_some compute then Error
               "Attribute Interpolate: computed weights require primitive/UVW mode"
           else
           Attribute_interpolate.interpolate_weighted ?cancel ?grain ?selection
             ~numbers_attribute ~weights_attribute ?pre_scale ?normalize_weights
             ?threshold ?blend ~unmatched
             ~weighted_owner:Attribute_interpolate.Primitives ~target_owner
             ~attributes ~groups ~source ~target ())))
  with
  | Cancel.Cancelled -> Error (Error.make ~operation:"attribute_interpolate"
      ~code:"cancelled" "attribute interpolation was cancelled")
  | Invalid_argument message -> Error (Error.make
      ~operation:"attribute_interpolate" ~code:"invalid_parameter" message)

let transfer_points = Attribute_transfer.transfer_points
let transfer_primitives = Attribute_transfer.transfer_primitives
let transfer_detail = Attribute_transfer.transfer_detail
let transfer_surface = Attribute_transfer.transfer_surface
let transfer_vertices = Attribute_transfer.transfer_vertices
let transfer_all = Attribute_transfer.transfer_all
