
type mode =
  | Nearest
  | Inverse_distance of { neighbors : int; power : float }
  | Kernel of {
      neighbors : int;
      radius : float;
      kernel : kernel;
    }

and kernel = Links | RenderMan | Hart

type unmatched = Keep_target | Default_value
type falloff = Linear | Smoothstep | Uniform of float
type surface_falloff = falloff
type surface_vertex_selection = Surface_index.vertex_selection =
  | All_triangle_vertices
  | Any_triangle_vertex
let owner_count geometry = function
  | Attribute.Point -> Geometry.point_count geometry
  | Attribute.Vertex -> Geometry.vertex_count geometry
  | Attribute.Primitive -> Geometry.primitive_count geometry
  | Attribute.Detail -> 1

let same_kind left right =
  String.equal (Attribute.kind_name left) (Attribute.kind_name right)

let owner_label = function
  | Attribute.Point -> "point"
  | Attribute.Vertex -> "vertex"
  | Attribute.Primitive -> "primitive"
  | Attribute.Detail -> "detail"

let selected_attributes ~operation ~owner ~names ~pattern source =
  let available = Geometry.attributes source
      |> List.filter (fun attribute -> Attribute.owner attribute = owner) in
  match names, pattern with
  | Some _, Some _ -> Error
      (operation ^ ": names and pattern are mutually exclusive")
  | None, None -> Ok (Array.of_list available)
  | None, Some source ->
      Result.map (fun pattern -> available
        |> List.filter (fun attribute ->
          Attribute_pattern.matches pattern (Attribute.name attribute))
        |> Array.of_list)
        (Attribute_pattern.compile source)
  | Some names, None ->
      let seen = Hashtbl.create (List.length names) in
      let rec collect result = function
        | [] -> Ok (Array.of_list (List.rev result))
        | name :: _ when String.trim name = "" ->
            Error (operation ^ ": attribute names must not be empty")
        | "P" :: _ when owner = Attribute.Point ->
            Error (operation ^ ": canonical P cannot be transferred as an ordinary attribute")
        | name :: _ when Hashtbl.mem seen name ->
            Error (operation ^ ": duplicate attribute name " ^ name)
        | name :: rest ->
            Hashtbl.add seen name ();
            (match Geometry.find_attribute ~owner name source with
             | None -> Error
                 (Printf.sprintf "%s: missing source %s attribute %s"
                    operation (owner_label owner) name)
             | Some attribute -> collect (attribute :: result) rest)
      in
      collect [] names

let validate_target_kinds ~operation ~owner attributes target =
  Array.fold_left (fun result source_attribute ->
    Result.bind result (fun () ->
      match Geometry.find_attribute ~owner
          (Attribute.name source_attribute) target with
      | None -> Ok ()
      | Some target_attribute when same_kind source_attribute target_attribute -> Ok ()
      | Some target_attribute -> Error (Printf.sprintf
          "%s: target attribute %s has %s storage, source has %s"
          operation
          (Attribute.name source_attribute) (Attribute.kind_name target_attribute)
          (Attribute.kind_name source_attribute)))) (Ok ()) attributes

let validate_sampled_storage ~operation attributes =
  Array.fold_left (fun result attribute ->
    Result.bind result (fun () ->
      match Attribute.Private.storage attribute with
      | Attribute.Int_array _ | Attribute.Float_array _ -> Error (Printf.sprintf
          "%s: sampled transfer of array attribute %s requires an explicit array policy"
          operation (Attribute.name attribute))
      | Attribute.Float _ | Attribute.Int _ | Attribute.Float2 _
      | Attribute.Float3 _ | Attribute.Float4 _ | Attribute.Text _ -> Ok ()))
    (Ok ()) attributes

let make_attribute_array count make =
  if count = 0 then Ok [||]
  else Result.bind (make 0) (fun first ->
    let attributes = Array.make count first in
    let rec fill index =
      if index = count then Ok attributes
      else Result.bind (make index) (fun attribute ->
        attributes.(index) <- attribute;
        fill (index + 1)) in
    fill 1)

let install_attributes_batch geometry attributes =
  Geometry.Private.with_merged_attributes_owned attributes geometry

let reset_selected selection reset = match selection with
  | None -> ()
  | Some group -> Group.iter reset group

let initial_float ?selection unmatched target_attribute count =
  match unmatched, target_attribute with
  | Keep_target, Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Float values -> Array.copy values | _ -> assert false)
  | Default_value, Some attribute when selection <> None ->
      (match Attribute.Private.storage attribute with
       | Attribute.Float values ->
           let output = Array.copy values in
           reset_selected selection (fun element -> output.(element) <- 0.);
           output
       | _ -> assert false)
  | Keep_target, None | Default_value, _ -> Array.make count 0.

let initial_int ?selection unmatched target_attribute count =
  match unmatched, target_attribute with
  | Keep_target, Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Int values -> Array.copy values | _ -> assert false)
  | Default_value, Some attribute when selection <> None ->
      (match Attribute.Private.storage attribute with
       | Attribute.Int values ->
           let output = Array.copy values in
           reset_selected selection (fun element -> output.(element) <- 0);
           output
       | _ -> assert false)
  | Keep_target, None | Default_value, _ -> Array.make count 0

let initial_text ?selection unmatched target_attribute count =
  match unmatched, target_attribute with
  | Keep_target, Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Text values -> Array.copy values | _ -> assert false)
  | Default_value, Some attribute when selection <> None ->
      (match Attribute.Private.storage attribute with
       | Attribute.Text values ->
           let output = Array.copy values in
           reset_selected selection (fun element -> output.(element) <- "");
           output
       | _ -> assert false)
  | Keep_target, None | Default_value, _ -> Array.make count ""

let initial_float2 ?selection unmatched target_attribute count =
  match unmatched, target_attribute with
  | Keep_target, Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Float2 values ->
           let values = Packed.Float2.Private.view values in
           Array.copy values.x, Array.copy values.y
       | _ -> assert false)
  | Default_value, Some attribute when selection <> None ->
      (match Attribute.Private.storage attribute with
       | Attribute.Float2 values ->
           let values = Packed.Float2.Private.view values in
           let x = Array.copy values.x and y = Array.copy values.y in
           reset_selected selection (fun element ->
             x.(element) <- 0.; y.(element) <- 0.);
           x, y
       | _ -> assert false)
  | Keep_target, None | Default_value, _ -> Array.make count 0., Array.make count 0.

let initial_float3 ?selection unmatched target_attribute count =
  match unmatched, target_attribute with
  | Keep_target, Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Float3 values ->
           let values = Packed.Float3.Private.view values in
           Array.copy values.x, Array.copy values.y, Array.copy values.z
       | _ -> assert false)
  | Default_value, Some attribute when selection <> None ->
      (match Attribute.Private.storage attribute with
       | Attribute.Float3 values ->
           let values = Packed.Float3.Private.view values in
           let x = Array.copy values.x and y = Array.copy values.y
           and z = Array.copy values.z in
           reset_selected selection (fun element ->
             x.(element) <- 0.; y.(element) <- 0.; z.(element) <- 0.);
           x, y, z
       | _ -> assert false)
  | Keep_target, None | Default_value, _ ->
      Array.make count 0., Array.make count 0., Array.make count 0.

let initial_float4 ?selection unmatched target_attribute count =
  match unmatched, target_attribute with
  | Keep_target, Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Float4 values ->
           let values = Packed.Float4.Private.view values in
           Array.copy values.x, Array.copy values.y, Array.copy values.z,
           Array.copy values.w
       | _ -> assert false)
  | Default_value, Some attribute when selection <> None ->
      (match Attribute.Private.storage attribute with
       | Attribute.Float4 values ->
           let values = Packed.Float4.Private.view values in
           let x = Array.copy values.x and y = Array.copy values.y
           and z = Array.copy values.z and w = Array.copy values.w in
           reset_selected selection (fun element ->
             x.(element) <- 0.; y.(element) <- 0.; z.(element) <- 0.;
             w.(element) <- 0.);
           x, y, z, w
       | _ -> assert false)
  | Keep_target, None | Default_value, _ ->
      Array.make count 0., Array.make count 0., Array.make count 0.,
      Array.make count 0.

let group_owner_of_attribute = function
  | Attribute.Point -> Group.Point
  | Attribute.Vertex -> Group.Vertex
  | Attribute.Primitive -> Group.Primitive
  | Attribute.Detail -> invalid_arg "detail attributes do not have element groups"

let validate_transfer_falloff operation = function
  | Linear | Smoothstep -> Ok ()
  | Uniform bias when Float.is_finite bias && bias >= 0. && bias <= 1. -> Ok ()
  | Uniform _ -> Error (operation ^ ": uniform bias must be finite and in [0, 1]")

let transfer_distance_window ~operation ~blend_width ~falloff max_distance =
  if not (Float.is_finite blend_width) || blend_width < 0. then Error
      (operation ^ ": blend width must be finite and non-negative")
  else Result.bind (validate_transfer_falloff operation falloff) (fun () ->
    match max_distance with
    | None when blend_width > 0. -> Error
        (operation ^ ": blend width requires a maximum distance")
    | None -> Ok (Float.infinity, Float.infinity)
    | Some value when Float.is_finite value && value >= 0.
        && value <= sqrt max_float ->
        let outer = value +. blend_width in
        if not (Float.is_finite outer) || outer > sqrt max_float then Error
            (operation ^ ": distance plus blend width must be safely squarable")
        else Ok (value *. value, outer *. outer)
    | Some _ -> Error
        (operation ^
         ": maximum distance must be finite, non-negative, and safely squarable"))

let[@inline] transfer_influence ~falloff ~threshold_squared ~blend_width
    distance_squared =
  if blend_width = 0. || distance_squared <= threshold_squared then 1.
  else
    let threshold = sqrt threshold_squared in
    let remaining = 1. -. ((sqrt distance_squared -. threshold) /. blend_width) in
    let remaining = Float.max 0. (Float.min 1. remaining) in
    match falloff with
    | Linear -> remaining
    | Smoothstep -> remaining *. remaining *. (3. -. (2. *. remaining))
    | Uniform bias -> bias

let[@inline always] transfer_kernel_weight kernel t =
  if t >= 1. then 0.
  else
    let t = Float.max 0. t in
    match kernel with
    | Links ->
        if t < (1. /. 3.) then 1. -. (3. *. t *. t)
        else 1.5 *. (1. -. t) *. (1. -. t)
    | RenderMan ->
        let one_minus_square = 1. -. (t *. t) in
        one_minus_square *. one_minus_square *. one_minus_square
    | Hart ->
        let t2 = t *. t and t3 = t *. t *. t in
        1. -. (10. *. t3) +. (15. *. t2 *. t2) -.
          (6. *. t2 *. t3)

let primitive_barycenters ?cancel ~grain geometry =
  let topology = Topology.Private.view (Geometry.topology geometry)
  and positions = Packed.Float3.Private.view (Geometry.positions geometry) in
  let count = Geometry.primitive_count geometry in
  let x = Array.make count 0. and y = Array.make count 0.
  and z = Array.make count 0. in
  if count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
      ~finish:(count - 1) (fun primitive ->
        if primitive land 4095 = 0 then Cancel.check_opt cancel;
        let first = topology.primitive_offsets.(primitive)
        and last = topology.primitive_offsets.(primitive + 1) in
        let sx = ref 0. and sy = ref 0. and sz = ref 0. in
        for vertex = first to last - 1 do
          let point = topology.vertex_points.(vertex) in
          sx := !sx +. positions.x.(point);
          sy := !sy +. positions.y.(point);
          sz := !sz +. positions.z.(point)
        done;
        let scale = 1. /. float_of_int (last - first) in
        x.(primitive) <- !sx *. scale;
        y.(primitive) <- !sy *. scale;
        z.(primitive) <- !sz *. scale);
  Packed.Float3.Private.of_owned_exn ~x ~y ~z

let spatial_transfer ?cancel ?(grain = 16_384) ?names ?pattern
    ?(mode = Nearest) ?max_distance ?(blend_width = 0.)
    ?(falloff = Smoothstep) ?(unmatched = Keep_target)
    ?source_elements ?target_elements
    ~operation ~owner ~source_positions ~target_positions ~source ~target () =
  if owner = Attribute.Detail || owner = Attribute.Vertex then invalid_arg
      (operation ^ ": spatial samples require point or primitive ownership");
  if grain <= 0 then invalid_arg (operation ^ ": grain must be positive");
  Cancel.check_opt cancel;
  let neighbor_capacity_result = match mode with
    | Nearest -> Ok 1
    | Inverse_distance { neighbors; power }
      when neighbors > 0 && Float.is_finite power && power > 0. -> Ok neighbors
    | Inverse_distance _ -> Error
        (operation ^ ": neighbors and power must be positive; power must be finite")
    | Kernel { neighbors; radius; _ }
      when neighbors > 0 && Float.is_finite radius && radius >= 0.
        && radius <= sqrt max_float -> Ok neighbors
    | Kernel _ -> Error
        (operation ^
         ": kernel neighbors must be positive and radius must be finite, non-negative, and safely squarable") in
  let maximum_result = transfer_distance_window ~operation ~blend_width ~falloff
      max_distance in
  let expected_group_owner = group_owner_of_attribute owner
  and source_count = owner_count source owner
  and target_count = owner_count target owner in
  let groups_result =
    if (match source_elements with None -> false | Some group ->
        Group.owner group <> expected_group_owner
        || Group.length group <> source_count) then
      Error (Printf.sprintf "%s: source selection must be a matching %s group"
        operation (owner_label owner))
    else if (match target_elements with None -> false | Some group ->
        Group.owner group <> expected_group_owner
        || Group.length group <> target_count) then
      Error (Printf.sprintf "%s: target selection must be a matching %s group"
        operation (owner_label owner))
    else Ok () in
  Result.bind neighbor_capacity_result (fun neighbor_capacity ->
  Result.bind maximum_result (fun (threshold_squared, maximum_squared) ->
  Result.bind groups_result (fun () ->
  Result.bind (selected_attributes ~operation ~owner ~names ~pattern source)
      (fun attributes ->
  Result.bind (validate_sampled_storage ~operation attributes) (fun () ->
  Result.bind (validate_target_kinds ~operation ~owner attributes target) (fun () ->
  if Array.length attributes = 0 then Ok target else
  if target_count > Sys.max_array_length / neighbor_capacity then Error
      (operation ^ ": neighbor table is too large")
  else
    let source_positions = source_positions ()
    and target_positions = target_positions () in
    if Packed.Float3.length source_positions <> source_count
       || Packed.Float3.length target_positions <> target_count then
      invalid_arg (operation ^ ": internal sample cardinality mismatch");
    let source_points = Option.map
        (Group.Private.with_owner Group.Point) source_elements
    and target_points = Option.map
        (Group.Private.with_owner Group.Point) target_elements in
    match Spatial_index.create ?cancel ~grain ?points:source_points source_positions with
    | Error error when String.equal (Error.code error) "cancelled" ->
        raise Cancel.Cancelled
    | Error error -> Error (Error.to_string error)
    | Ok index ->
        let target_view = Packed.Float3.Private.view target_positions in
        for element = 0 to target_count - 1 do
          if element land 16_383 = 0 then Cancel.check_opt cancel;
          if (match target_elements with None -> true
              | Some group -> Group.mem element group)
             && not (Float.is_finite target_view.x.(element)
              && Float.is_finite target_view.y.(element)
              && Float.is_finite target_view.z.(element)) then
            invalid_arg "target positions must be finite"
        done;
        let slots = target_count * neighbor_capacity in
        let neighbors = Array.make slots (-1)
        and distances = Array.make slots Float.infinity
        and neighbor_counts = Array.make target_count 0 in
        Spatial_index.Private.nearest_k_many_into ?cancel ?points:target_points
          ~grain index
          ~queries:target_positions
          ~max_distance_squared:maximum_squared ~capacity:neighbor_capacity
          ~indices:neighbors ~distances_squared:distances
          ~counts:neighbor_counts;
        let influence = if blend_width = 0. then None
          else
            let values = Array.make target_count 0. in
            if target_count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
                ~finish:(target_count - 1) (fun target_element ->
                  if target_element land 4095 = 0 then Cancel.check_opt cancel;
                  if neighbor_counts.(target_element) > 0 then
                    values.(target_element) <- transfer_influence ~falloff
                      ~threshold_squared ~blend_width
                      distances.(target_element * neighbor_capacity));
            Some values in
        let sample_weights = match mode with
          | Nearest -> None
          | Inverse_distance { power; _ } ->
              let weights = distances in
              if target_count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
                  ~finish:(target_count - 1) (fun target_element ->
                    if target_element land 4095 = 0 then Cancel.check_opt cancel;
                    let count = neighbor_counts.(target_element) in
                    if count > 0 then begin
                      let offset = target_element * neighbor_capacity
                      and nearest_distance = distances.(target_element
                        * neighbor_capacity) in
                      if nearest_distance = 0.
                          || not (Float.is_finite nearest_distance) then begin
                        for slot = 0 to count - 1 do
                          weights.(offset + slot) <- 0.
                        done;
                        weights.(offset) <- 1.
                      end
                      else begin
                        let nearest = sqrt nearest_distance and total = ref 0. in
                        for slot = 0 to count - 1 do
                          let weight =
                            (nearest /. sqrt distances.(offset + slot)) ** power in
                          weights.(offset + slot) <- weight;
                          total := !total +. weight
                        done;
                        let inverse = 1. /. !total in
                        for slot = 0 to count - 1 do
                          weights.(offset + slot) <-
                            weights.(offset + slot) *. inverse
                        done
                      end
                    end);
              Some weights
          | Kernel { radius; kernel; _ } ->
              let weights = distances in
              if target_count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
                  ~finish:(target_count - 1) (fun target_element ->
                    if target_element land 4095 = 0 then Cancel.check_opt cancel;
                    let count = neighbor_counts.(target_element) in
                    if count > 0 then begin
                      let offset = target_element * neighbor_capacity in
                      if radius = 0. then begin
                        for slot = 0 to count - 1 do
                          weights.(offset + slot) <- 0.
                        done;
                        weights.(offset) <- 1.
                      end
                      else begin
                        let total = ref 0. in
                        for slot = 0 to count - 1 do
                          let weight = transfer_kernel_weight kernel
                              (sqrt distances.(offset + slot) /. radius) in
                          weights.(offset + slot) <- weight;
                          total := !total +. weight
                        done;
                        if !total > 0. && Float.is_finite !total then begin
                          let inverse = 1. /. !total in
                          for slot = 0 to count - 1 do
                            weights.(offset + slot) <-
                              weights.(offset + slot) *. inverse
                          done
                        end else begin
                          for slot = 0 to count - 1 do
                            weights.(offset + slot) <- 0.
                          done;
                          weights.(offset) <- 1.
                        end
                      end
                    end);
              Some weights in
        let transfer_plane source_values output =
          if target_count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
              ~finish:(target_count - 1) (fun target_element ->
                if target_element land 4095 = 0 then Cancel.check_opt cancel;
                let count = neighbor_counts.(target_element) in
                if count > 0 then begin
                  let offset = target_element * neighbor_capacity in
                  let sampled = match mode with
                  | Nearest -> source_values.(neighbors.(offset))
                  | Inverse_distance _ | Kernel _ ->
                      let weights = Option.get sample_weights
                      and weighted = ref 0. in
                      for slot = 0 to count - 1 do
                        weighted := !weighted +. (weights.(offset + slot) *.
                          source_values.(neighbors.(offset + slot)))
                      done;
                      !weighted in
                  output.(target_element) <- match influence with
                  | None -> sampled
                  | Some values -> output.(target_element) +.
                      (values.(target_element) *.
                       (sampled -. output.(target_element)))
                end) in
        let transfer_discrete source_values output =
          if target_count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
              ~finish:(target_count - 1) (fun target_element ->
                if target_element land 4095 = 0 then Cancel.check_opt cancel;
                if neighbor_counts.(target_element) > 0
                    && (match influence with None -> true
                        | Some values -> values.(target_element) >= 0.5) then
                  output.(target_element) <- source_values.(neighbors.(
                    target_element * neighbor_capacity))) in
        let make_attribute source_attribute =
          let name = Attribute.name source_attribute in
          let existing = Geometry.find_attribute ~owner name target in
          let storage = match Attribute.Private.storage source_attribute with
            | Attribute.Float source_values ->
                let output = initial_float ?selection:target_elements
                    unmatched existing target_count in
                transfer_plane source_values output;
                Attribute.Float output
            | Attribute.Int source_values ->
                let output = initial_int ?selection:target_elements
                    unmatched existing target_count in
                transfer_discrete source_values output;
                Attribute.Int output
            | Attribute.Text source_values ->
                let output = initial_text ?selection:target_elements
                    unmatched existing target_count in
                transfer_discrete source_values output;
                Attribute.Text output
            | Attribute.Float2 source_values ->
                let source_values = Packed.Float2.Private.view source_values in
                let x, y = initial_float2 ?selection:target_elements
                    unmatched existing target_count in
                transfer_plane source_values.x x; transfer_plane source_values.y y;
                Attribute.Float2 (Packed.Float2.of_owned ~x ~y |> Result.get_ok)
            | Attribute.Float3 source_values ->
                let source_values = Packed.Float3.Private.view source_values in
                let x, y, z = initial_float3 ?selection:target_elements
                    unmatched existing target_count in
                transfer_plane source_values.x x; transfer_plane source_values.y y;
                transfer_plane source_values.z z;
                if String.equal name "N" && target_count > 0 then
                  Parallel.for_ ~chunk_size:grain ~start:0
                    ~finish:(target_count - 1) (fun target_element ->
                      if neighbor_counts.(target_element) > 0
                          && (match influence with None -> true
                              | Some values -> values.(target_element) > 0.) then begin
                        let length = sqrt ((x.(target_element) *. x.(target_element))
                          +. (y.(target_element) *. y.(target_element))
                          +. (z.(target_element) *. z.(target_element))) in
                        if length > 1e-20 then begin
                          x.(target_element) <- x.(target_element) /. length;
                          y.(target_element) <- y.(target_element) /. length;
                          z.(target_element) <- z.(target_element) /. length
                        end
                      end);
                Attribute.Float3 (Packed.Float3.Private.of_owned_exn ~x ~y ~z)
            | Attribute.Float4 source_values ->
                let source_values = Packed.Float4.Private.view source_values in
                let x, y, z, w = initial_float4 ?selection:target_elements
                    unmatched existing target_count in
                transfer_plane source_values.x x; transfer_plane source_values.y y;
                transfer_plane source_values.z z; transfer_plane source_values.w w;
                Attribute.Float4 (Packed.Float4.of_owned ~x ~y ~z ~w
                  |> Result.get_ok)
            | Attribute.Int_array _ | Attribute.Float_array _ -> assert false in
          Attribute.create_owned ~name ~owner storage
        in
        Result.bind (make_attribute_array (Array.length attributes)
            (fun index -> make_attribute attributes.(index)))
          (install_attributes_batch target)))))))

let spatial ?cancel ?grain ?names ?pattern ?mode ?max_distance
    ?blend_width ?falloff ?unmatched ?source_elements ?target_elements
    ~owner ~source ~target () =
  Error.guard ~cancelled:"attribute transfer was cancelled" ~invalid_argument:"invalid_position" ~operation:"attribute_transfer"
    ~code:"invalid_transfer" @@ fun () ->
  let grain = Option.value ~default:16_384 grain in
  let operation, source_positions, target_positions = match owner with
    | Attribute.Point ->
        "Rdk.Attribute_ops.transfer_points",
        (fun () -> Geometry.positions source),
        (fun () -> Geometry.positions target)
    | Attribute.Primitive ->
        "Rdk.Attribute_ops.transfer_primitives",
        (fun () -> primitive_barycenters ?cancel ~grain source),
        (fun () -> primitive_barycenters ?cancel ~grain target)
    | Attribute.Vertex | Attribute.Detail ->
        invalid_arg "Rdk.Attribute_ops.transfer: owner must be point or primitive" in
  spatial_transfer ?cancel ~grain ?names ?pattern ?mode ?max_distance
    ?blend_width ?falloff ?unmatched ?source_elements ?target_elements
    ~operation ~owner ~source_positions ~target_positions ~source ~target ()

let transfer_points ?cancel ?grain ?names ?pattern ?mode ?max_distance
    ?blend_width ?falloff ?unmatched ?source_points ?target_points
    ~source ~target () =
  spatial ?cancel ?grain ?names ?pattern ?mode ?max_distance ?blend_width
    ?falloff ?unmatched ?source_elements:source_points
    ?target_elements:target_points ~owner:Attribute.Point ~source ~target ()

let transfer_primitives ?cancel ?grain ?names ?pattern ?mode ?max_distance
    ?blend_width ?falloff ?unmatched ?source_primitives ?target_primitives
    ~source ~target () =
  spatial ?cancel ?grain ?names ?pattern ?mode ?max_distance ?blend_width
    ?falloff ?unmatched ?source_elements:source_primitives
    ?target_elements:target_primitives ~owner:Attribute.Primitive ~source ~target ()

let transfer_detail ?names ?pattern ~source ~target () =
  Error.guard ~operation:"attribute_transfer"
    ~code:"invalid_transfer" @@ fun () ->
  Result.bind (selected_attributes ~operation:"Rdk.Attribute_ops.transfer_detail"
      ~owner:Attribute.Detail ~names ~pattern source) (fun attributes ->
    Result.bind (validate_target_kinds
        ~operation:"Rdk.Attribute_ops.transfer_detail" ~owner:Attribute.Detail
        attributes target) (fun () ->
      install_attributes_batch target attributes))

type surface_attribute = {
  source_owner : Attribute.owner;
  source_name : string;
  target_name : string;
}

let surface_attribute ?into ~owner name =
  if owner = Attribute.Detail then
    invalid_arg "Rdk.Attribute_ops.surface_attribute: detail ownership is not spatial";
  if String.trim name = "" then
    invalid_arg "Rdk.Attribute_ops.surface_attribute: empty source name";
  let target_name = Option.value ~default:name into in
  if String.trim target_name = "" then
    invalid_arg "Rdk.Attribute_ops.surface_attribute: empty target name";
  if owner = Attribute.Point && String.equal name "P" then
    invalid_arg "Rdk.Attribute_ops.surface_attribute: canonical P is not an ordinary attribute";
  { source_owner = owner; source_name = name; target_name }

let transfer_surface ?cancel ?(grain = 16_384) ?max_distance
    ?(blend_width = 0.) ?(falloff = Smoothstep) ?(unmatched = Keep_target)
    ?(target_owner = Attribute.Point) ?distance_attribute ?source_primitives
    ?source_vertices ?(source_vertex_selection = All_triangle_vertices)
    ?target_elements
    ~attributes ~source ~target () =
  Error.guard ~cancelled:"surface attribute transfer was cancelled" ~invalid_argument:"invalid_position" ~operation:"attribute_transfer_surface"
    ~code:"invalid_transfer" @@ fun () ->
  if grain <= 0 then invalid_arg
      "Rdk.Attribute_ops.transfer_surface: grain must be positive";
  Cancel.check_opt cancel;
  let operation = "Rdk.Attribute_ops.transfer_surface" in
  Result.bind (transfer_distance_window ~operation ~blend_width ~falloff
      max_distance) (fun (threshold_squared, maximum_squared) ->
  let specifications = Array.of_list attributes in
  let source_attributes = Array.make (Array.length specifications) None in
  let seen = Hashtbl.create (Array.length specifications + 1) in
  let failure = ref None in
  if target_owner = Attribute.Detail then
    failure := Some "detail ownership is not a spatial surface destination";
  (match source_primitives with
   | Some group when Group.owner group <> Group.Primitive
       || Group.length group <> Geometry.primitive_count source ->
       failure := Some "source selection must be a matching primitive group"
   | None | Some _ -> ());
  (match source_vertices with
   | Some group when Group.owner group <> Group.Vertex
       || Group.length group <> Geometry.vertex_count source ->
       failure := Some "source vertex selection must be a matching vertex group"
   | None | Some _ -> ());
  (match target_elements with
   | Some group when
       let expected_owner, expected_length = match target_owner with
         | Attribute.Point -> Group.Point, Geometry.point_count target
         | Attribute.Vertex -> Group.Vertex, Geometry.vertex_count target
         | Attribute.Primitive -> Group.Primitive, Geometry.primitive_count target
         | Attribute.Detail -> Group.Point, 0 in
       Group.owner group <> expected_owner || Group.length group <> expected_length ->
       failure := Some "target selection owner/length does not match destination elements"
   | None | Some _ -> ());
  Array.iteri (fun index specification ->
    if !failure = None then begin
      if specification.source_owner = Attribute.Detail then
        failure := Some "detail attributes are not spatial surface attributes"
      else if String.trim specification.source_name = ""
           || String.trim specification.target_name = "" then
        failure := Some "attribute names must not be empty"
      else if specification.source_owner = Attribute.Point
          && String.equal specification.source_name "P" then
        failure := Some "canonical P cannot be transferred as an ordinary attribute"
      else if target_owner = Attribute.Point
          && String.equal specification.target_name "P" then
        failure := Some "canonical target P cannot be an ordinary attribute"
      else if Hashtbl.mem seen specification.target_name then
        failure := Some ("duplicate target attribute " ^ specification.target_name)
      else begin
        Hashtbl.add seen specification.target_name ();
        match Geometry.find_attribute ~owner:specification.source_owner
            specification.source_name source with
        | None -> failure := Some (Printf.sprintf "missing source %s attribute %s"
            (match specification.source_owner with Point -> "point" | Vertex -> "vertex"
             | Primitive -> "primitive" | Detail -> "detail")
            specification.source_name)
        | Some attribute ->
            (match Attribute.Private.storage attribute with
             | Attribute.Int_array _ | Attribute.Float_array _ ->
                 failure := Some (Printf.sprintf
                   "sampled transfer of array attribute %s requires an explicit array policy"
                   specification.source_name)
             | Attribute.Float _ | Attribute.Int _ | Attribute.Float2 _
             | Attribute.Float3 _ | Attribute.Float4 _ | Attribute.Text _ ->
                 (match Geometry.find_attribute ~owner:target_owner
                     specification.target_name target with
                  | Some target_attribute when not (same_kind attribute target_attribute) ->
                      failure := Some (Printf.sprintf
                        "target attribute %s has %s storage, source has %s"
                        specification.target_name (Attribute.kind_name target_attribute)
                        (Attribute.kind_name attribute))
                  | _ -> source_attributes.(index) <- Some attribute))
      end
    end) specifications;
  let distance_name = match distance_attribute with
    | None -> None
    | Some name when String.trim name = "" ->
        failure := Some "distance attribute name must not be empty"; None
    | Some name when Hashtbl.mem seen name ->
        failure := Some ("distance attribute conflicts with " ^ name); None
    | Some "P" when target_owner = Attribute.Point ->
        failure := Some "canonical target P cannot store transfer distance"; None
    | Some name ->
        (match Geometry.find_attribute ~owner:target_owner name target with
         | Some attribute when Attribute.kind_name attribute <> "float" ->
             failure := Some ("distance target attribute " ^ name ^ " is not float")
         | _ -> ());
        Some name in
  match !failure with Some message -> Error message | None ->
  if Array.length specifications = 0 && distance_name = None then Ok target
  else if (match source_primitives, source_vertices with
      | Some group, _ when Group.cardinality group = 0 -> true
      | _, Some group when Group.cardinality group = 0 -> true
      | None, None | None, Some _ | Some _, None | Some _, Some _ ->
          Geometry.primitive_count source = 0) then begin
    let target_count = owner_count target target_owner in
    let make_unmatched_attribute index =
      let specification = specifications.(index)
      and source_attribute = Option.get source_attributes.(index) in
      let existing = Geometry.find_attribute ~owner:target_owner
          specification.target_name target in
      let storage = match Attribute.Private.storage source_attribute with
        | Attribute.Float _ -> Attribute.Float
            (initial_float ?selection:target_elements unmatched existing target_count)
        | Attribute.Int _ -> Attribute.Int
            (initial_int ?selection:target_elements unmatched existing target_count)
        | Attribute.Text _ -> Attribute.Text
            (initial_text ?selection:target_elements unmatched existing target_count)
        | Attribute.Float2 _ ->
            let x, y = initial_float2 ?selection:target_elements unmatched existing
                target_count in
            Attribute.Float2 (Packed.Float2.of_owned ~x ~y |> Result.get_ok)
        | Attribute.Float3 _ ->
            let x, y, z = initial_float3 ?selection:target_elements unmatched existing
                target_count in
            Attribute.Float3 (Packed.Float3.Private.of_owned_exn ~x ~y ~z)
        | Attribute.Float4 _ ->
            let x, y, z, w = initial_float4 ?selection:target_elements unmatched
                existing target_count in
            Attribute.Float4
              (Packed.Float4.of_owned ~x ~y ~z ~w |> Result.get_ok)
        | Attribute.Int_array _ | Attribute.Float_array _ -> assert false in
      Attribute.create_owned ~name:specification.target_name ~owner:target_owner storage in
    let attribute_count = Array.length specifications
        + if distance_name = None then 0 else 1 in
    Result.bind (make_attribute_array attribute_count (fun index ->
      if index < Array.length specifications then make_unmatched_attribute index
      else
        let name = Option.get distance_name in
        let existing = Geometry.find_attribute ~owner:target_owner name target in
        let values = initial_float ?selection:target_elements unmatched existing
            target_count in
        Attribute.create_owned ~name ~owner:target_owner (Attribute.Float values)))
      (install_attributes_batch target)
  end
  else match Surface_index.create ?cancel ~grain ?primitives:source_primitives
      ?vertices:source_vertices ~vertex_selection:source_vertex_selection source with
  | Error error when String.equal (Error.code error) "cancelled" -> raise Cancel.Cancelled
  | Error error -> Error (Error.to_string error)
  | Ok surface ->
      let target_count = owner_count target target_owner in
      let query_positions, position_indices = match target_owner with
        | Attribute.Point -> Geometry.positions target, None
        | Attribute.Vertex ->
            let topology = Topology.Private.view (Geometry.topology target) in
            Geometry.positions target, Some topology.vertex_points
        | Attribute.Primitive -> primitive_barycenters ?cancel ~grain target, None
        | Attribute.Detail -> assert false in
      let primitives = Array.make target_count (-1)
      and triangles = Array.make target_count (-1)
      and barycentric_a = Array.make target_count 0.
      and barycentric_b = Array.make target_count 0.
      and barycentric_c = Array.make target_count 0.
      and distances_squared = Array.make target_count Float.infinity in
      Surface_index.Private.closest_many_into ?cancel ?selection:target_elements
        ?position_indices ~grain surface ~queries:query_positions
        ~max_distance_squared:maximum_squared
        ~primitives ~triangles ~barycentric_a ~barycentric_b ~barycentric_c
        ~distances_squared;
      let influence = if blend_width = 0. then None
        else
          let values = Array.make target_count 0. in
          if target_count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
              ~finish:(target_count - 1) (fun point ->
                if primitives.(point) >= 0 then
                  values.(point) <- transfer_influence ~falloff
                    ~threshold_squared ~blend_width distances_squared.(point));
          Some values in
      let source_topology = Topology.Private.view (Geometry.topology source) in
      let source_element owner primitive triangle local = match owner with
        | Attribute.Point ->
            let vertex = Surface_index.Private.triangle_vertex surface triangle local in
            source_topology.vertex_points.(vertex)
        | Attribute.Vertex ->
            Surface_index.Private.triangle_vertex surface triangle local
        | Attribute.Primitive -> primitive
        | Attribute.Detail -> 0 in
      let transfer_numeric owner source_values output =
        if target_count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
            ~finish:(target_count - 1) (fun point ->
              if point land 4095 = 0 then Cancel.check_opt cancel;
              let primitive = primitives.(point) in
              if primitive >= 0 then
                let sampled = if owner = Attribute.Primitive then source_values.(primitive)
                  else
                    (barycentric_a.(point) *.
                       source_values.(source_element owner primitive triangles.(point) 0))
                    +. (barycentric_b.(point) *.
                       source_values.(source_element owner primitive triangles.(point) 1))
                    +. (barycentric_c.(point) *.
                       source_values.(source_element owner primitive triangles.(point) 2)) in
                output.(point) <- match influence with
                | None -> sampled
                | Some values -> output.(point)
                    +. (values.(point) *. (sampled -. output.(point)))) in
      let transfer_discrete owner source_values output =
        if target_count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
            ~finish:(target_count - 1) (fun point ->
              if point land 4095 = 0 then Cancel.check_opt cancel;
              let primitive = primitives.(point) in
              if primitive >= 0
                  && (match influence with None -> true
                      | Some values -> values.(point) >= 0.5) then begin
                let local = if owner = Attribute.Primitive then 0
                  else if barycentric_a.(point) >= barycentric_b.(point)
                       && barycentric_a.(point) >= barycentric_c.(point) then 0
                  else if barycentric_b.(point) >= barycentric_c.(point) then 1
                  else 2 in
                output.(point) <- source_values.(source_element owner primitive
                  triangles.(point) local)
              end) in
      let make_attribute index =
        let specification = specifications.(index)
        and source_attribute = Option.get source_attributes.(index) in
        let existing = Geometry.find_attribute ~owner:target_owner
            specification.target_name target in
        let owner = specification.source_owner in
        let storage = match Attribute.Private.storage source_attribute with
          | Attribute.Float values ->
              let output = initial_float ?selection:target_elements
                  unmatched existing target_count in
              transfer_numeric owner values output; Attribute.Float output
          | Attribute.Int values ->
              let output = initial_int ?selection:target_elements
                  unmatched existing target_count in
              transfer_discrete owner values output; Attribute.Int output
          | Attribute.Text values ->
              let output = initial_text ?selection:target_elements
                  unmatched existing target_count in
              transfer_discrete owner values output; Attribute.Text output
          | Attribute.Float2 values ->
              let values = Packed.Float2.Private.view values in
              let x, y = initial_float2 ?selection:target_elements
                  unmatched existing target_count in
              transfer_numeric owner values.x x; transfer_numeric owner values.y y;
              Attribute.Float2 (Packed.Float2.of_owned ~x ~y |> Result.get_ok)
          | Attribute.Float3 values ->
              let values = Packed.Float3.Private.view values in
              let x, y, z = initial_float3 ?selection:target_elements
                  unmatched existing target_count in
              transfer_numeric owner values.x x; transfer_numeric owner values.y y;
              transfer_numeric owner values.z z;
              if String.equal specification.source_name "N" && target_count > 0 then
                Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(target_count - 1)
                  (fun point -> if primitives.(point) >= 0
                      && (match influence with None -> true
                          | Some values -> values.(point) > 0.) then begin
                    let length = sqrt ((x.(point) *. x.(point))
                      +. (y.(point) *. y.(point)) +. (z.(point) *. z.(point))) in
                    if length > 1e-20 then begin x.(point) <- x.(point) /. length;
                      y.(point) <- y.(point) /. length; z.(point) <- z.(point) /. length end
                  end);
              Attribute.Float3 (Packed.Float3.Private.of_owned_exn ~x ~y ~z)
          | Attribute.Float4 values ->
              let values = Packed.Float4.Private.view values in
              let x, y, z, w = initial_float4 ?selection:target_elements
                  unmatched existing target_count in
              transfer_numeric owner values.x x; transfer_numeric owner values.y y;
              transfer_numeric owner values.z z; transfer_numeric owner values.w w;
              Attribute.Float4 (Packed.Float4.of_owned ~x ~y ~z ~w
                |> Result.get_ok)
          | Attribute.Int_array _ | Attribute.Float_array _ -> assert false in
        Attribute.create_owned ~name:specification.target_name
          ~owner:target_owner storage in
      let attribute_count = Array.length specifications
          + if distance_name = None then 0 else 1 in
      Result.bind (make_attribute_array attribute_count (fun index ->
        if index < Array.length specifications then make_attribute index
        else
          let name = Option.get distance_name in
          let existing = Geometry.find_attribute ~owner:target_owner name target in
          let values = initial_float ?selection:target_elements
              unmatched existing target_count in
          if target_count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
              ~finish:(target_count - 1) (fun point ->
                if primitives.(point) >= 0 then
                  values.(point) <- sqrt distances_squared.(point));
          Attribute.create_owned ~name ~owner:target_owner
            (Attribute.Float values)))
        (install_attributes_batch target))

let transfer_vertices ?cancel ?grain ?names ?pattern ?max_distance
    ?blend_width ?falloff ?unmatched ?source_primitives ?source_vertices
    ?source_vertex_selection ?target_vertices
    ~source ~target () =
  Error.guard ~cancelled:"attribute transfer was cancelled" ~invalid_argument:"invalid_position" ~operation:"attribute_transfer"
    ~code:"invalid_transfer" @@ fun () ->
  Result.bind (selected_attributes
      ~operation:"Rdk.Attribute_ops.transfer_vertices"
      ~owner:Attribute.Vertex ~names ~pattern source) (fun attributes ->
    let attributes = Array.to_list attributes |> List.map (fun attribute ->
      surface_attribute ~owner:Attribute.Vertex (Attribute.name attribute)) in
    Ok (Error.get (transfer_surface ?cancel ?grain ?max_distance ?blend_width
      ?falloff ?unmatched ~target_owner:Attribute.Vertex ?source_primitives
      ?source_vertices ?source_vertex_selection
      ?target_elements:target_vertices ~attributes ~source ~target ())))

let transfer_all ?cancel ?grain ?point_pattern ?vertex_pattern
    ?primitive_pattern ?detail_pattern ?mode ?max_distance ?blend_width ?falloff
    ?unmatched ~source ~target () =
  Error.guard ~cancelled:"multi-owner attribute transfer was cancelled" ~invalid_argument:"invalid_position" ~operation:"attribute_transfer_all"
    ~code:"invalid_transfer" @@ fun () ->
  let apply pattern transfer geometry = match pattern with
    | None -> Ok geometry
    | Some pattern -> transfer ~pattern geometry in
  Result.bind
    (apply point_pattern (fun ~pattern target ->
       Ok (Error.get (spatial ?cancel ?grain ~pattern ?mode ?max_distance
         ?blend_width ?falloff ?unmatched ~owner:Attribute.Point
         ~source ~target ()))) target)
    (fun target -> Result.bind
      (apply primitive_pattern (fun ~pattern target ->
         Ok (Error.get (spatial ?cancel ?grain ~pattern ?mode ?max_distance
           ?blend_width ?falloff ?unmatched ~owner:Attribute.Primitive
           ~source ~target ()))) target)
      (fun target -> Result.bind
        (apply vertex_pattern (fun ~pattern target ->
           Ok (Error.get (transfer_vertices ?cancel ?grain ~pattern ?max_distance
             ?blend_width ?falloff ?unmatched ~source ~target ()))) target)
        (fun target -> apply detail_pattern (fun ~pattern target ->
           Ok (Error.get (transfer_detail ~pattern ~source ~target ()))) target)))
