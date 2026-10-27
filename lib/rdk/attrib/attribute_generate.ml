open Rays_math

type numeric_value =
  | Scalar of float
  | Vec2 of Rays_math.Vec2.t
  | Vec3 of Rays_math.Vec3.t
  | Vec4 of float * float * float * float

type random_operation =
  | Random_set
  | Random_add
  | Random_minimum
  | Random_maximum
  | Random_multiply

type noise_kind = Noise_float | Noise_vector | Noise_quaternion

type noise_location =
  | Noise_position
  | Noise_element_number
  | Noise_attribute of string

type noise_range =
  | Noise_positive
  | Noise_zero_centered
  | Noise_min_max of numeric_value * numeric_value

type noise_operation =
  | Noise_set_initial
  | Noise_set
  | Noise_add
  | Noise_subtract
  | Noise_multiply
  | Noise_minimum
  | Noise_maximum

type random_selection =
  | Random_points of Group.t
  | Random_vertices of Group.t
  | Random_primitives of Group.t
  | Random_edges of Edge_group.t

type random_distribution =
  | Random_constant of numeric_value
  | Random_two_values of {
      a : numeric_value;
      b : numeric_value;
      probability_b : float;
    }
  | Random_uniform of { min : numeric_value; max : numeric_value }
  | Random_uniform_discrete of {
      min : numeric_value;
      max : numeric_value;
      step : numeric_value;
    }
  | Random_normal of { middle : numeric_value; scale : numeric_value }
  | Random_exponential of { median : numeric_value }
  | Random_log_normal of { median : numeric_value; stddev : numeric_value }
  | Random_cauchy of { median : numeric_value; scale : numeric_value }
  | Random_direction of {
      direction : numeric_value;
      cone_angle : float;
    }
  | Random_inside_sphere of { dimensions : int }
  | Random_inside_sphere_cone of {
      direction : numeric_value;
      cone_angle : float;
    }
  | Random_custom_ramp of {
      ramp : (float * float) list;
      fit_min : numeric_value;
      fit_max : numeric_value;
    }
  | Random_custom_discrete of (numeric_value * float) list
  | Random_custom_discrete_text of (string * float) list

type remap_input = Remap_explicit of {
  min : numeric_value;
  max : numeric_value;
} | Remap_auto

type remap_policy = Remap_clamp | Remap_cycle | Remap_extrapolate

type storage_kind = Position | Float | Float2 | Float3 | Float4

let value_array = function
  | Scalar value -> [|value|]
  | Vec2 value -> [|value.Rays_math.Vec2.x; value.y|]
  | Vec3 value -> [|value.Rays_math.Vec3.x; value.y; value.z|]
  | Vec4 (x, y, z, w) -> [|x; y; z; w|]

let finite_array values = Array.for_all Float.is_finite values

let same_dimensions label arrays =
  let dimension = Array.length arrays.(0) in
  if dimension < 1 || dimension > 4 then Error
      ("Rdk.Attribute_ops." ^ label ^ ": dimensions must be between one and four")
  else if Array.exists (fun values -> Array.length values <> dimension) arrays then
    Error ("Rdk.Attribute_ops." ^ label ^ ": parameter dimensions differ")
  else if Array.exists (fun values -> not (finite_array values)) arrays then
    Error ("Rdk.Attribute_ops." ^ label ^ ": parameters must be finite")
  else Ok dimension

type direction_plan = {
  axis : float array;
  householder : float array;
  householder_norm2 : float;
  cap_angle : float;
  bias : float;
}

type prepared_distribution =
  | Prepared_constant of float array
  | Prepared_two_values of {
      a : float array;
      b : float array;
      probability_b : float;
    }
  | Prepared_uniform of { min : float array; span : float array }
  | Prepared_uniform_discrete of {
      min : float array;
      max : float array;
      step : float array;
      choices : float array;
    }
  | Prepared_normal of { middle : float array; scale : float array }
  | Prepared_exponential of { median : float array }
  | Prepared_log_normal of { median : float array; sigma : float array }
  | Prepared_cauchy of {
      median : float array;
      scale : float;
      direction : direction_plan option;
    }
  | Prepared_direction of direction_plan
  | Prepared_inside_sphere of { dimensions : int }
  | Prepared_inside_sphere_cone of direction_plan
  | Prepared_custom_ramp of {
      knots : (float * float) array;
      fit_min : float array;
      fit_span : float array;
    }
  | Prepared_custom_discrete of {
      values : float array array;
      cumulative : float array;
      total : float;
      last_positive : int;
    }
  | Prepared_custom_discrete_text of {
      values : string array;
      cumulative : float array;
      total : float;
      last_positive : int;
    }

let validate_knots operation ramp =
  let knots = Array.of_list ramp in
  if Array.length knots < 2 then Error (Printf.sprintf
      "Rdk.Attribute_ops.%s: ramp requires at least two knots" operation)
  else begin
    let failure = ref None in
    Array.iteri (fun index (position, value) ->
      if !failure = None then
        if not (Float.is_finite position && Float.is_finite value) then
          failure := Some "ramp knots must be finite"
        else if position < 0. || position > 1. then
          failure := Some "ramp positions must lie in [0, 1]"
        else if index > 0 && position <= fst knots.(index - 1) then
          failure := Some "ramp positions must be strictly increasing") knots;
    match !failure with
    | Some message -> Error (Printf.sprintf "Rdk.Attribute_ops.%s: %s"
        operation message)
    | None when fst knots.(0) <> 0.
        || fst knots.(Array.length knots - 1) <> 1. -> Error (Printf.sprintf
          "Rdk.Attribute_ops.%s: ramp must have endpoints at 0 and 1" operation)
    | None -> Ok knots
  end

let[@inline] max_abs values =
  let result = ref 0. in
  for index = 0 to Array.length values - 1 do
    let value = abs_float values.(index) in
    if value > !result then result := value
  done;
  !result

let prepare_direction_plan ~orientation ~bias direction cone_angle =
  let invalid message = Error ("Rdk.Attribute_ops.randomize: " ^ message) in
  let axis = value_array direction in
  Result.bind (same_dimensions "randomize" [|axis|]) (fun dimensions ->
    let maximum = if orientation && dimensions = 4 then 2. *. Float.pi
      else Float.pi in
    if dimensions < 2 then invalid
        "direction distribution requires two, three, or four dimensions"
    else if not (Float.is_finite cone_angle) || cone_angle < 0.
        || cone_angle > maximum then invalid
        "direction cone angle is outside its valid range"
    else
      let scale = max_abs axis in
      if scale = 0. then invalid "direction must be non-zero"
      else begin
        let length2 = ref 0. in
        for component = 0 to dimensions - 1 do
          let value = axis.(component) /. scale in
          length2 := !length2 +. (value *. value)
        done;
        let inverse = 1. /. sqrt !length2 in
        for component = 0 to dimensions - 1 do
          axis.(component) <- (axis.(component) /. scale) *. inverse
        done;
        let householder = Array.map (fun value -> -.value) axis in
        householder.(dimensions - 1) <- householder.(dimensions - 1) +. 1.;
        let householder_norm2 = ref 0. in
        for component = 0 to dimensions - 1 do
          householder_norm2 := !householder_norm2 +.
            (householder.(component) *. householder.(component))
        done;
        Ok { axis; householder; householder_norm2 = !householder_norm2;
          cap_angle = if orientation && dimensions = 4 then cone_angle *. 0.5
            else cone_angle;
          bias }
      end)

let prepare_distribution ~direction_bias distribution =
  let invalid message = Error ("Rdk.Attribute_ops.randomize: " ^ message) in
  let directional = match distribution with
    | Random_direction _ | Random_inside_sphere_cone _ -> true
    | _ -> false in
  if not (Float.is_finite direction_bias) || direction_bias <= -1. then
    invalid "direction bias must be finite and greater than -1"
  else if not directional && direction_bias <> 0. then
    invalid "direction bias requires a direction or sphere-cone distribution"
  else match distribution with
  | Random_constant value ->
      let values = value_array value in
      Result.map (fun _ -> Prepared_constant values)
        (same_dimensions "randomize" [|values|])
  | Random_two_values { a; b; probability_b } ->
      let a = value_array a and b = value_array b in
      Result.bind (same_dimensions "randomize" [|a; b|]) (fun _ ->
        if Float.is_finite probability_b && probability_b >= 0.
            && probability_b <= 1. then
          Ok (Prepared_two_values { a; b; probability_b })
        else invalid "probability must be finite and in [0, 1]")
  | Random_uniform { min; max } ->
      let min = value_array min and max = value_array max in
      Result.bind (same_dimensions "randomize" [|min; max|]) (fun _ ->
        if Array.for_all2 (fun min max -> max >= min) min max then
          Ok (Prepared_uniform { min;
            span = Array.mapi (fun component value -> max.(component) -. value)
              min })
        else invalid "uniform maximum must be at least its minimum")
  | Random_uniform_discrete { min; max; step } ->
      let min = value_array min and max = value_array max
      and step = value_array step in
      Result.bind (same_dimensions "randomize" [|min; max; step|]) (fun _ ->
        if Array.for_all2 (fun min max -> max >= min) min max
            && Array.for_all (fun value -> value > 0.) step then
          Ok (Prepared_uniform_discrete { min; max; step;
            choices = Array.mapi (fun component value ->
              floor ((max.(component) -. value) /. step.(component)) +. 1.) min })
        else invalid "discrete bounds must be ordered and steps positive")
  | Random_normal { middle; scale } ->
      let middle = value_array middle and scale = value_array scale in
      Result.bind (same_dimensions "randomize" [|middle; scale|]) (fun _ ->
        if Array.for_all (fun value -> value >= 0.) scale then
          Ok (Prepared_normal { middle; scale })
        else invalid "distribution scale must be non-negative")
  | Random_exponential { median } ->
      let median = value_array median in
      Result.bind (same_dimensions "randomize" [|median|]) (fun _ ->
        if Array.for_all (fun value -> value > 0.) median then
          Ok (Prepared_exponential { median })
        else invalid "exponential median must be positive")
  | Random_log_normal { median; stddev } ->
      let median = value_array median and stddev = value_array stddev in
      Result.bind (same_dimensions "randomize" [|median; stddev|]) (fun _ ->
        if Array.for_all (fun value -> value > 0.) median
            && Array.for_all (fun value -> value >= 0.) stddev then
          let sigma = Array.mapi (fun component median ->
            let deviation = stddev.(component) in
            if deviation = 0. then 0. else
              let ratio = (deviation *. deviation) /. (median *. median) in
              sqrt (log (0.5 *. (1. +. sqrt (1. +. (4. *. ratio))))))
              median in
          if Array.for_all Float.is_finite sigma then
            Ok (Prepared_log_normal { median; sigma })
          else invalid "log-normal parameters overflowed"
        else invalid
          "log-normal median must be positive and deviation non-negative")
  | Random_cauchy { median; scale } ->
      let median = value_array median and scale = value_array scale in
      Result.bind (same_dimensions "randomize" [|median; scale|]) (fun dimensions ->
        if not (Array.for_all (fun value -> value >= 0.) scale) then
          invalid "distribution scale must be non-negative"
        else if dimensions > 1 &&
            not (Array.for_all (fun value -> value = scale.(0)) scale) then
          invalid "multidimensional Cauchy scale must be isotropic"
        else if dimensions = 1 then
          Ok (Prepared_cauchy { median; scale = scale.(0); direction = None })
        else
          let axis = Array.make dimensions 0. in
          axis.(dimensions - 1) <- 1.;
          let direction_value = match dimensions with
            | 2 -> Vec2 (Rays_math.Vec2.create axis.(0) axis.(1))
            | 3 -> Vec3 (Rays_math.Vec3.create axis.(0) axis.(1) axis.(2))
            | 4 -> Vec4 (axis.(0), axis.(1), axis.(2), axis.(3))
            | _ -> assert false in
          Result.map (fun direction -> Prepared_cauchy {
            median; scale = scale.(0); direction = Some direction })
            (prepare_direction_plan ~orientation:false ~bias:0.
               direction_value Float.pi))
  | Random_direction { direction; cone_angle } ->
      Result.map (fun plan -> Prepared_direction plan)
        (prepare_direction_plan ~orientation:true ~bias:direction_bias direction
          cone_angle)
  | Random_inside_sphere { dimensions } ->
      if dimensions < 2 || dimensions > 4 then invalid
          "inside-sphere distribution requires two, three, or four dimensions"
      else Ok (Prepared_inside_sphere { dimensions })
  | Random_inside_sphere_cone { direction; cone_angle } ->
      Result.map (fun plan -> Prepared_inside_sphere_cone plan)
        (prepare_direction_plan ~orientation:false ~bias:direction_bias direction
          cone_angle)
  | Random_custom_ramp { ramp; fit_min; fit_max } ->
      let fit_min = value_array fit_min and fit_max = value_array fit_max in
      Result.bind (same_dimensions "randomize" [|fit_min; fit_max|]) (fun _ ->
        Result.map (fun knots ->
          let fit_span = Array.mapi (fun index min -> fit_max.(index) -. min)
              fit_min in
          Prepared_custom_ramp { knots; fit_min; fit_span })
          (validate_knots "randomize" ramp))
  | Random_custom_discrete entries ->
      if entries = [] then invalid "custom discrete values must not be empty"
      else
        let entries = Array.of_list entries in
        let values = Array.map (fun (value, _) -> value_array value) entries in
        Result.bind (same_dimensions "randomize" values) (fun _ ->
          let cumulative = Array.make (Array.length entries) 0.
          and total = ref 0. and last_positive = ref (-1)
          and failure = ref None in
          Array.iteri (fun index (_, weight) ->
            if !failure = None then
              if not (Float.is_finite weight) || weight < 0. then
                failure := Some "custom discrete weights must be finite and non-negative"
              else begin
                total := !total +. weight;
                if not (Float.is_finite !total) then
                  failure := Some "custom discrete weight sum overflowed"
                else if weight > 0. then last_positive := index
              end;
            cumulative.(index) <- !total) entries;
          match !failure with
          | Some message -> invalid message
          | None when !total <= 0. -> invalid
              "custom discrete values require at least one positive weight"
          | None -> Ok (Prepared_custom_discrete { values; cumulative;
              total = !total; last_positive = !last_positive }))
  | Random_custom_discrete_text entries ->
      if entries = [] then invalid "custom discrete values must not be empty"
      else
        let entries = Array.of_list entries in
        let values = Array.map fst entries
        and cumulative = Array.make (Array.length entries) 0.
        and total = ref 0. and last_positive = ref (-1)
        and failure = ref None in
        Array.iteri (fun index (_, weight) ->
          if !failure = None then
            if not (Float.is_finite weight) || weight < 0. then
              failure := Some
                "custom discrete weights must be finite and non-negative"
            else begin
              total := !total +. weight;
              if not (Float.is_finite !total) then
                failure := Some "custom discrete weight sum overflowed"
              else if weight > 0. then last_positive := index
            end;
          cumulative.(index) <- !total) entries;
        (match !failure with
         | Some message -> invalid message
         | None when !total <= 0. -> invalid
             "custom discrete values require at least one positive weight"
         | None -> Ok (Prepared_custom_discrete_text { values; cumulative;
             total = !total; last_positive = !last_positive }))

let prepared_dimension = function
  | Prepared_constant values -> Array.length values
  | Prepared_two_values { a; _ } -> Array.length a
  | Prepared_uniform { min; _ }
  | Prepared_uniform_discrete { min; _ } -> Array.length min
  | Prepared_normal { middle; _ } -> Array.length middle
  | Prepared_exponential { median }
  | Prepared_log_normal { median; _ }
  | Prepared_cauchy { median; _ } -> Array.length median
  | Prepared_direction { axis; _ } -> Array.length axis
  | Prepared_inside_sphere { dimensions } -> dimensions
  | Prepared_inside_sphere_cone { axis; _ } -> Array.length axis
  | Prepared_custom_ramp { fit_min; _ } -> Array.length fit_min
  | Prepared_custom_discrete { values; _ } -> Array.length values.(0)
  | Prepared_custom_discrete_text _ -> 1

let owner_count geometry = function
  | Attribute.Point -> Geometry.point_count geometry
  | Attribute.Vertex -> Geometry.vertex_count geometry
  | Attribute.Primitive -> Geometry.primitive_count geometry
  | Attribute.Detail -> 1

let group_owner = function
  | Attribute.Point -> Some Group.Point
  | Attribute.Vertex -> Some Group.Vertex
  | Attribute.Primitive -> Some Group.Primitive
  | Attribute.Detail -> None

let validate_selection operation owner count = function
  | None -> Ok ()
  | Some _ when owner = Attribute.Detail -> Error
      ("Rdk.Attribute_ops." ^ operation ^ ": detail attributes do not accept groups")
  | Some group ->
      let expected = Option.get (group_owner owner) in
      if Group.owner group = expected && Group.length group = count then Ok ()
      else Error ("Rdk.Attribute_ops." ^ operation
        ^ ": selection owner or length does not match the attribute")

let random_selection_element = function
  | Random_points group -> Element_selection.Selected_points group
  | Random_vertices group -> Element_selection.Selected_vertices group
  | Random_primitives group -> Element_selection.Selected_primitives group
  | Random_edges group -> Element_selection.Selected_edges group

let random_selection_destination = function
  | Attribute.Point -> Some Group.Point
  | Attribute.Vertex -> Some Group.Vertex
  | Attribute.Primitive -> Some Group.Primitive
  | Attribute.Detail -> None

let resolve_random_selection ?cancel ~grain ~owner ~count ~geometry selection
    element_selection =
  match selection, element_selection with
  | Some _, Some _ -> Error
      "Rdk.Attribute_ops.randomize: selection arguments are mutually exclusive"
  | Some selection, None ->
      Result.map (fun () -> Some selection)
        (validate_selection "randomize" owner count (Some selection))
  | None, None -> Ok None
  | None, Some _ when owner = Attribute.Detail -> Error
      "Rdk.Attribute_ops.randomize: detail attributes do not accept groups"
  | None, Some selected ->
      let topology = Geometry.topology geometry in
      Result.bind
        (Element_selection.validate ~operation:"Rdk.Attribute_ops.randomize"
           topology (Some (random_selection_element selected)))
        (fun () ->
          let destination = Option.get (random_selection_destination owner) in
          Result.map Option.some (Element_selection.promote ?cancel ~grain
            ~name:"__attribute_randomize_selection" ~destination
            (random_selection_element selected) (Geometry.topology geometry)))

type sample_limits =
  | No_limits
  | Minimum_only of float array
  | Maximum_only of float array
  | Minimum_and_maximum of float array * float array

let prepare_limits dimension minimum maximum =
  let parameter label = function
    | None -> Ok None
    | Some value ->
        let values = value_array value in
        if Array.length values <> dimension then Error
            ("Rdk.Attribute_ops.randomize: " ^ label
             ^ " dimension does not match the distribution")
        else if not (finite_array values) then Error
            ("Rdk.Attribute_ops.randomize: " ^ label ^ " must be finite")
        else Ok (Some values) in
  Result.bind (parameter "minimum" minimum) (fun minimum ->
  Result.bind (parameter "maximum" maximum) (fun maximum ->
    match minimum, maximum with
    | None, None -> Ok No_limits
    | Some minimum, None -> Ok (Minimum_only minimum)
    | None, Some maximum -> Ok (Maximum_only maximum)
    | Some minimum, Some maximum ->
        if Array.for_all2 (fun minimum maximum -> minimum <= maximum)
            minimum maximum then
          Ok (Minimum_and_maximum (minimum, maximum))
        else Error
          "Rdk.Attribute_ops.randomize: minimum must not exceed maximum"))

let[@inline] limit_sample limits component value = match limits with
  | No_limits -> value
  | Minimum_only minimum -> Float.max minimum.(component) value
  | Maximum_only maximum -> Float.min maximum.(component) value
  | Minimum_and_maximum (minimum, maximum) ->
      Float.max minimum.(component) (Float.min maximum.(component) value)

let kind_dimension = function
  | Position | Float3 -> 3
  | Float -> 1
  | Float2 -> 2
  | Float4 -> 4

let kind_of_dimension = function
  | 1 -> Float | 2 -> Float2 | 3 -> Float3 | 4 -> Float4
  | _ -> invalid_arg "Rdk.Attribute_ops: invalid numeric dimension"

let planes_of_storage = function
  | Attribute.Float values -> Ok (Float, [|values|])
  | Attribute.Float2 values ->
      let values = Packed.Float2.Private.view values in
      Ok (Float2, [|values.x; values.y|])
  | Attribute.Float3 values ->
      let values = Packed.Float3.Private.view values in
      Ok (Float3, [|values.x; values.y; values.z|])
  | Attribute.Float4 values ->
      let values = Packed.Float4.Private.view values in
      Ok (Float4, [|values.x; values.y; values.z; values.w|])
  | Attribute.Int _ | Attribute.Text _ | Attribute.Int_array _
  | Attribute.Float_array _ -> Error
      "attribute must have floating scalar or tuple storage"

let source_planes ~owner ~name geometry =
  if owner = Attribute.Point && String.equal name "P" then
    let values = Packed.Float3.Private.view (Geometry.positions geometry) in
    Ok (Position, [|values.x; values.y; values.z|])
  else match Geometry.find_attribute ~owner name geometry with
    | None -> Error (Printf.sprintf "missing source attribute %s" name)
    | Some attribute -> planes_of_storage (Attribute.Private.storage attribute)

let existing_planes ~owner ~name geometry =
  if owner = Attribute.Point && String.equal name "P" then
    let values = Packed.Float3.Private.view (Geometry.positions geometry) in
    Some (Position, [|values.x; values.y; values.z|])
  else match Geometry.find_attribute ~owner name geometry with
    | None -> None
    | Some attribute ->
        (match planes_of_storage (Attribute.Private.storage attribute) with
         | Ok result -> Some result
         | Error _ -> Some (Float, [||]))

let install ~owner ~name kind planes geometry = match kind with
  | Position ->
      Geometry.with_positions (Packed.Float3.Private.of_owned_exn
        ~x:planes.(0) ~y:planes.(1) ~z:planes.(2)) geometry
  | Float ->
      Result.bind (Attribute.create_owned ~name ~owner
          (Attribute.Float planes.(0))) (fun attribute ->
        Geometry.with_attribute attribute geometry)
  | Float2 ->
      Result.bind (Packed.Float2.of_owned ~x:planes.(0) ~y:planes.(1))
        (fun values -> Result.bind (Attribute.create_owned ~name ~owner
          (Attribute.Float2 values)) (fun attribute ->
          Geometry.with_attribute attribute geometry))
  | Float3 ->
      let values = Packed.Float3.Private.of_owned_exn
          ~x:planes.(0) ~y:planes.(1) ~z:planes.(2) in
      Result.bind (Attribute.create_owned ~name ~owner (Attribute.Float3 values))
        (fun attribute -> Geometry.with_attribute attribute geometry)
  | Float4 ->
      Result.bind (Packed.Float4.of_owned ~x:planes.(0) ~y:planes.(1)
          ~z:planes.(2) ~w:planes.(3)) (fun values ->
        Result.bind (Attribute.create_owned ~name ~owner
          (Attribute.Float4 values)) (fun attribute ->
          Geometry.with_attribute attribute geometry))

let remove_stale_normals geometry =
  geometry
  |> Geometry.without_attribute ~owner:Attribute.Point "N"
  |> Geometry.without_attribute ~owner:Attribute.Vertex "N"

let changed source output =
  let result = ref false and component = ref 0 in
  while !component < Array.length source && not !result do
    let element = ref 0 in
    while !element < Array.length source.(!component) && not !result do
      if Int64.bits_of_float source.(!component).(!element)
          <> Int64.bits_of_float output.(!component).(!element) then
        result := true;
      incr element
    done;
    incr component
  done;
  !result

let seed_values owner name geometry = match name with
  | None -> Ok None
  | Some name when String.trim name = "" -> Error
      "Rdk.Attribute_ops.randomize: seed attribute name must not be empty"
  | Some name ->
      (match Geometry.find_attribute ~owner name geometry with
       | None -> Ok None
       | Some attribute ->
           (match Attribute.Private.storage attribute with
            | Attribute.Int values -> Ok (Some values)
            | _ -> Error
                "Rdk.Attribute_ops.randomize: seed attribute must be integer"))

let expected_fraction_dimension prepared = match prepared with
  | Prepared_two_values _ | Prepared_custom_discrete _
  | Prepared_custom_discrete_text _ -> 1
  | Prepared_direction { axis; _ } -> Array.length axis - 1
  | Prepared_inside_sphere { dimensions } -> dimensions
  | Prepared_inside_sphere_cone { axis; _ } -> Array.length axis
  | _ -> prepared_dimension prepared

let fraction_values owner name expected geometry = match name with
  | None -> Ok None
  | Some name when String.trim name = "" -> Error
      "Rdk.Attribute_ops.randomize: fraction attribute name must not be empty"
  | Some name ->
      Result.bind (source_planes ~owner ~name geometry) (fun (_, planes) ->
        if Array.length planes <> expected then Error (Printf.sprintf
            "Rdk.Attribute_ops.randomize: fraction attribute %s must have %d component%s"
            name expected (if expected = 1 then "" else "s"))
        else Ok (Some planes))

let[@inline] sample_linear_knots knots value =
  if value <= 0. then snd knots.(0)
  else if value >= 1. then snd knots.(Array.length knots - 1)
  else begin
    let low = ref 0 and high = ref (Array.length knots - 1) in
    while !high - !low > 1 do
      let middle = (!low + !high) / 2 in
      if fst knots.(middle) <= value then low := middle else high := middle
    done;
    let x0, y0 = knots.(!low) and x1, y1 = knots.(!high) in
    y0 +. (((value -. x0) /. (x1 -. x0)) *. (y1 -. y0))
  end

(* Peter J. Acklam's inverse-normal rational approximation. The tails remain
   deliberately infinite at exact quantiles zero and one, so the operation's
   ordinary finite-output validation reports an unusable fraction. *)
let[@inline] normal_quantile probability =
  if probability <= 0. then Float.neg_infinity
  else if probability >= 1. then Float.infinity
  else if probability < 0.02425 then begin
    let q = sqrt (-2. *. log probability) in
    (((((-0.007784894002430293 *. q -. 0.3223964580411365) *. q
       -. 2.400758277161838) *. q -. 2.549732539343734) *. q
       +. 4.374664141464968) *. q +. 2.938163982698783) /.
    ((((0.007784695709041462 *. q +. 0.3224671290700398) *. q
       +. 2.445134137142996) *. q +. 3.754408661907416) *. q +. 1.)
  end else if probability > 0.97575 then begin
    let q = sqrt (-2. *. log (1. -. probability)) in
    -.(((((-0.007784894002430293 *. q -. 0.3223964580411365) *. q
       -. 2.400758277161838) *. q -. 2.549732539343734) *. q
       +. 4.374664141464968) *. q +. 2.938163982698783) /.
    ((((0.007784695709041462 *. q +. 0.3224671290700398) *. q
       +. 2.445134137142996) *. q +. 3.754408661907416) *. q +. 1.)
  end else begin
    let q = probability -. 0.5 in
    let r = q *. q in
    (((((-39.69683028665376 *. r +. 220.9460984245205) *. r
       -. 275.9285104469687) *. r +. 138.3577518672690) *. r
       -. 30.66479806614716) *. r +. 2.506628277459239) *. q /.
    (((((-54.47609879822406 *. r +. 161.5858368580409) *. r
       -. 155.6989798598866) *. r +. 66.80131188771972) *. r
       -. 13.28068155288572) *. r +. 1.)
  end

let[@inline] indexed_uniform fractions seed identity element slot =
  match fractions with
  | Some planes -> planes.(slot).(element)
  | None -> Rays_math.Rand.float_at seed ~index:(identity lxor
      ((slot + 1) * 0x11b54a32d192ed03))

let[@inline] choice_uniform fractions seed identity element =
  match fractions with
  | Some planes -> planes.(0).(element)
  | None -> Rays_math.Rand.float_at seed ~index:identity

let[@inline] weighted_choice cumulative total last_positive fraction =
  let target = fraction *. total in
  let low = ref 0 and high = ref (Array.length cumulative) in
  while !low < !high do
    let middle = (!low + !high) / 2 in
    if cumulative.(middle) > target then high := middle
    else low := middle + 1
  done;
  if !low = Array.length cumulative then last_positive else !low

let[@inline] standard_normal seed identity slot =
  let stream = identity lxor ((slot + 1) * 0x11b54a32d192ed03) in
  let u1 = Float.max Float.min_float (Rays_math.Rand.float_at seed ~index:stream)
  and u2 = Rays_math.Rand.float_at seed ~index:(stream lxor 0x14d049bb133111eb) in
  sqrt (-2. *. log u1) *. cos (2. *. Float.pi *. u2)

let cauchy_radius_quantile dimensions fraction =
  if fraction <= 0. then 0.
  else if fraction >= 1. then Float.infinity
  else if dimensions = 2 then begin
    let cosine = 1. -. fraction in
    sqrt (Float.max 0. ((1. /. (cosine *. cosine)) -. 1.))
  end else begin
    let low = ref 0. and high = ref (Float.pi *. 0.5) in
    for _ = 0 to 47 do
      let angle = (!low +. !high) *. 0.5 in
      let sine = sin angle and cosine = cos angle in
      let cumulative = if dimensions = 3 then
          (2. /. Float.pi) *. (angle -. (sine *. cosine))
        else 1. -. (1.5 *. cosine) +. (0.5 *. cosine *. cosine *. cosine) in
      if cumulative < fraction then low := angle else high := angle
    done;
    tan ((!low +. !high) *. 0.5)
  end

let cap4_angle fraction maximum =
  if maximum = 0. then 0.
  else begin
    let denominator = (maximum *. 0.5) -. (sin (2. *. maximum) *. 0.25) in
    let target = fraction *. denominator in
    let low = ref 0. and high = ref maximum in
    for _ = 0 to 39 do
      let middle = (!low +. !high) *. 0.5 in
      let integral = (middle *. 0.5) -. (sin (2. *. middle) *. 0.25) in
      if integral < target then low := middle else high := middle
    done;
    (!low +. !high) *. 0.5
  end

let[@inline] sample_direction fractions seed identity element axis householder
    householder_norm2 cap_angle bias scratch =
  let dimensions = Array.length axis in
  if cap_angle = 0. then
    Array.blit axis 0 scratch 0 dimensions
  else if dimensions = 4 && cap_angle = Float.pi && bias = 0. then begin
    let split = indexed_uniform fractions seed identity element 0
    and angle_a = 2. *. Float.pi *.
        indexed_uniform fractions seed identity element 1
    and angle_b = 2. *. Float.pi *.
        indexed_uniform fractions seed identity element 2 in
    let lower = sqrt (1. -. split) and upper = sqrt split in
    scratch.(0) <- lower *. sin angle_a;
    scratch.(1) <- lower *. cos angle_a;
    scratch.(2) <- upper *. sin angle_b;
    scratch.(3) <- upper *. cos angle_b
  end
  else if dimensions = 2 then begin
    let fraction = indexed_uniform fractions seed identity element 0 in
    let signed = (2. *. fraction) -. 1. in
    let signed = if bias = 0. then signed else
      let magnitude = abs_float signed in
      let biased = 1. -. ((1. -. magnitude) ** (1. /. (bias +. 1.))) in
      if signed < 0. then -.biased else biased in
    let angle = signed *. cap_angle in
    let cosine = cos angle and sine = sin angle in
    scratch.(0) <- (axis.(0) *. cosine) -. (axis.(1) *. sine);
    scratch.(1) <- (axis.(1) *. cosine) +. (axis.(0) *. sine)
  end else begin
    let axial = indexed_uniform fractions seed identity element 0 in
    let axial = if bias = 0. then axial
      else 1. -. ((1. -. axial) ** (1. /. (bias +. 1.))) in
    let theta = if dimensions = 3 then
        acos (1. -. (axial *. (1. -. cos cap_angle)))
      else cap4_angle axial cap_angle in
    let tangent_fraction = indexed_uniform fractions seed identity element 1 in
    let azimuth = 2. *. Float.pi *. tangent_fraction in
    if dimensions = 3 then begin
      scratch.(0) <- cos azimuth;
      scratch.(1) <- sin azimuth;
      scratch.(2) <- 0.
    end else begin
      let elevation = indexed_uniform fractions seed identity element 2 in
      let tangent_z = 1. -. (2. *. elevation) in
      let tangent_radius = sqrt (Float.max 0.
          (1. -. (tangent_z *. tangent_z))) in
      scratch.(0) <- tangent_radius *. cos azimuth;
      scratch.(1) <- tangent_radius *. sin azimuth;
      scratch.(2) <- tangent_z;
      scratch.(3) <- 0.
    end;
    let projection = ref 0. in
    for component = 0 to dimensions - 1 do
      projection := !projection +.
        (householder.(component) *. scratch.(component))
    done;
    let factor = if householder_norm2 = 0. then 0.
      else (2. *. !projection) /. householder_norm2 in
    let cosine = cos theta and sine = sin theta in
    for component = 0 to dimensions - 1 do
      let tangent = scratch.(component) -.
          (householder.(component) *. factor) in
      scratch.(component) <- (axis.(component) *. cosine) +. (tangent *. sine)
    done
  end

let[@inline] sample_inside_sphere fractions seed identity element dimensions scratch =
  if dimensions = 2 then begin
    let angle = 2. *. Float.pi *.
        indexed_uniform fractions seed identity element 0 in
    let radius = sqrt (indexed_uniform fractions seed identity element 1) in
    scratch.(0) <- radius *. cos angle;
    scratch.(1) <- radius *. sin angle
  end else if dimensions = 3 then begin
    let axial = 1. -. (2. *.
        indexed_uniform fractions seed identity element 0) in
    let angle = 2. *. Float.pi *.
        indexed_uniform fractions seed identity element 1 in
    let planar = sqrt (Float.max 0. (1. -. (axial *. axial))) in
    let radius = (indexed_uniform fractions seed identity element 2) **
        (1. /. 3.) in
    scratch.(0) <- radius *. planar *. cos angle;
    scratch.(1) <- radius *. planar *. sin angle;
    scratch.(2) <- radius *. axial
  end else begin
    let split = indexed_uniform fractions seed identity element 0
    and angle_a = 2. *. Float.pi *.
        indexed_uniform fractions seed identity element 1
    and angle_b = 2. *. Float.pi *.
        indexed_uniform fractions seed identity element 2 in
    let radius = sqrt (sqrt
        (indexed_uniform fractions seed identity element 3))
    and lower = sqrt (1. -. split) and upper = sqrt split in
    scratch.(0) <- radius *. lower *. sin angle_a;
    scratch.(1) <- radius *. lower *. cos angle_a;
    scratch.(2) <- radius *. upper *. sin angle_b;
    scratch.(3) <- radius *. upper *. cos angle_b
  end

let[@inline] sample_prepared prepared fractions seed identity element scratch =
  match prepared with
  | Prepared_constant values ->
      Array.blit values 0 scratch 0 (Array.length values)
  | Prepared_two_values { a; b; probability_b } ->
      let values = if choice_uniform fractions seed identity element < probability_b
        then b else a in
      Array.blit values 0 scratch 0 (Array.length values)
  | Prepared_uniform { min; span } ->
      for component = 0 to Array.length min - 1 do
        scratch.(component) <- min.(component) +.
          (indexed_uniform fractions seed identity element component *.
            span.(component))
      done
  | Prepared_uniform_discrete { min; max; step; choices } ->
      for component = 0 to Array.length min - 1 do
        let sample = floor (indexed_uniform fractions seed identity element
            component *. choices.(component)) in
        scratch.(component) <- Float.min max.(component)
            (min.(component) +. (sample *. step.(component)))
      done
  | Prepared_normal { middle; scale } ->
      for component = 0 to Array.length middle - 1 do
        let normal = match fractions with
          | Some _ -> normal_quantile
              (indexed_uniform fractions seed identity element component)
          | None ->
              let stream = identity lxor
                  ((component + 1) * 0x11b54a32d192ed03) in
              let u1 = Float.max Float.min_float
                  (Rays_math.Rand.float_at seed ~index:stream)
              and u2 = Rays_math.Rand.float_at seed
                  ~index:(stream lxor 0x14d049bb133111eb) in
              sqrt (-2. *. log u1) *. cos (2. *. Float.pi *. u2) in
        scratch.(component) <- middle.(component) +.
            (scale.(component) *. normal)
      done
  | Prepared_exponential { median } ->
      for component = 0 to Array.length median - 1 do
        let fraction = indexed_uniform fractions seed identity element component in
        scratch.(component) <- (-.median.(component) /. log 2.) *.
            log (1. -. fraction)
      done
  | Prepared_log_normal { median; sigma } ->
      for component = 0 to Array.length median - 1 do
        if sigma.(component) = 0. then scratch.(component) <- median.(component)
        else begin
          let normal = match fractions with
            | Some _ -> normal_quantile
                (indexed_uniform fractions seed identity element component)
            | None ->
                let stream = identity lxor
                    ((component + 1) * 0x11b54a32d192ed03) in
                let u1 = Float.max Float.min_float
                    (Rays_math.Rand.float_at seed ~index:stream)
                and u2 = Rays_math.Rand.float_at seed
                    ~index:(stream lxor 0x14d049bb133111eb) in
                sqrt (-2. *. log u1) *. cos (2. *. Float.pi *. u2) in
          scratch.(component) <- exp (log median.(component) +.
              (sigma.(component) *. normal))
        end
      done
  | Prepared_cauchy { median; scale; direction = None } ->
      let fraction = indexed_uniform fractions seed identity element 0 in
      scratch.(0) <- median.(0) +.
        (scale *. tan (Float.pi *. (fraction -. 0.5)))
  | Prepared_cauchy { median; scale; direction = Some direction } ->
      let dimensions = Array.length median in
      (match fractions with
       | None ->
           let denominator = abs_float (standard_normal seed identity dimensions) in
           for component = 0 to dimensions - 1 do
             scratch.(component) <- median.(component) +.
               (scale *. (standard_normal seed identity component /. denominator))
           done
       | Some _ ->
           sample_direction fractions seed identity element direction.axis
             direction.householder direction.householder_norm2
             direction.cap_angle direction.bias scratch;
           let radius = cauchy_radius_quantile dimensions
               (indexed_uniform fractions seed identity element (dimensions - 1)) in
           for component = 0 to dimensions - 1 do
             scratch.(component) <- median.(component) +.
               (scale *. radius *. scratch.(component))
           done)
  | Prepared_direction
      { axis; householder; householder_norm2; cap_angle; bias } ->
      sample_direction fractions seed identity element axis householder
        householder_norm2 cap_angle bias scratch
  | Prepared_inside_sphere { dimensions } ->
      sample_inside_sphere fractions seed identity element dimensions scratch
  | Prepared_inside_sphere_cone
      { axis; householder; householder_norm2; cap_angle; bias } ->
      let dimensions = Array.length axis in
      sample_direction fractions seed identity element axis householder
        householder_norm2 cap_angle bias scratch;
      let radius = indexed_uniform fractions seed identity element
          (dimensions - 1) ** (1. /. float_of_int dimensions) in
      for component = 0 to dimensions - 1 do
        scratch.(component) <- scratch.(component) *. radius
      done
  | Prepared_custom_ramp { knots; fit_min; fit_span } ->
      for component = 0 to Array.length fit_min - 1 do
        let quantile = sample_linear_knots knots
            (indexed_uniform fractions seed identity element component) in
        scratch.(component) <- fit_min.(component) +.
            (quantile *. fit_span.(component))
      done
  | Prepared_custom_discrete { values; cumulative; total; last_positive } ->
      let selected = weighted_choice cumulative total last_positive
          (choice_uniform fractions seed identity element) in
      Array.blit values.(selected) 0 scratch 0 (Array.length values.(selected))
  | Prepared_custom_discrete_text _ -> assert false

let randomize_text ?cancel ~grain ~selection ?seed_attribute
    ?fraction_attribute ~seed ~owner ~name ~operation ~scale ~minimum ~maximum
    ~values ~cumulative ~total ~last_positive geometry =
  if operation <> Random_set then Error
      "Rdk.Attribute_ops.randomize: text output only supports Set Value"
  else if scale <> 1. then Error
      "Rdk.Attribute_ops.randomize: text output does not accept Global Scale"
  else if minimum <> None || maximum <> None then Error
      "Rdk.Attribute_ops.randomize: text output does not accept numeric limits"
  else if owner = Attribute.Point && String.equal name "P" then Error
      "Rdk.Attribute_ops.randomize: canonical P cannot use text storage"
  else
    let count = owner_count geometry owner in
    let target = Geometry.find_attribute ~owner name geometry in
    let output_result = match target with
      | None -> Ok (Array.make count "")
      | Some attribute ->
          (match Attribute.Private.storage attribute with
           | Attribute.Text existing -> Ok (Array.copy existing)
           | _ -> Error
               "Rdk.Attribute_ops.randomize: existing attribute is not text") in
    Result.bind output_result (fun output ->
    Result.bind (if fraction_attribute = None then
        seed_values owner seed_attribute geometry
      else match seed_attribute with
        | None -> Ok None
        | Some _ -> Error
            "Rdk.Attribute_ops.randomize: seed and fraction attributes are mutually exclusive")
      (fun seeds ->
    Result.bind (fraction_values owner fraction_attribute 1 geometry)
      (fun fractions ->
        let ranges = if count = 0 then 0 else (count + grain - 1) / grain in
        let errors = Array.make ranges (-1) in
        Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(ranges - 1) (fun range ->
          let first = range * grain and last = min count ((range + 1) * grain) in
          for element = first to last - 1 do
            if element land 4095 = 0 then Cancel.check_opt cancel;
            if match selection with None -> true
                | Some group -> Group.mem element group then begin
              let element_seed = match seeds with
                | None -> element | Some values -> values.(element) in
              let identity = element_seed * 0x1e3779b97f4a7c15 in
              let fraction = choice_uniform fractions seed identity element in
              if not (Float.is_finite fraction)
                  || fraction < 0. || fraction > 1. then
                errors.(range) <- element
              else
                output.(element) <- values.(weighted_choice cumulative total
                  last_positive fraction)
            end
          done);
        match Array.find_opt (fun element -> element >= 0) errors with
        | Some element -> Error (Printf.sprintf
            "Rdk.Attribute_ops.randomize: fraction is invalid at element %d"
            element)
        | None ->
            let unchanged = match target with
              | Some attribute ->
                  (match Attribute.Private.storage attribute with
                   | Attribute.Text source ->
                       Array.for_all2 String.equal source output
                   | _ -> false)
              | None -> false in
            if unchanged then Ok geometry
            else Result.bind (Attribute.create_owned ~name ~owner
                (Attribute.Text output)) (fun attribute ->
              Geometry.with_attribute attribute geometry)
    )))

let randomize ?cancel ~grain ?selection ?element_selection ?seed_attribute
    ?fraction_attribute ?minimum ?maximum ~direction_bias ~seed ~owner ~name
    ~operation ~scale distribution geometry =
  if grain <= 0 then invalid_arg
      "Rdk.Attribute_ops.randomize: grain must be positive";
  if String.trim name = "" then Error
      "Rdk.Attribute_ops.randomize: attribute name must not be empty"
  else if not (Float.is_finite scale) then Error
      "Rdk.Attribute_ops.randomize: global scale must be finite"
  else
    let count = owner_count geometry owner in
    Result.bind (resolve_random_selection ?cancel ~grain ~owner ~count ~geometry
        selection element_selection) (fun selection ->
    Result.bind (prepare_distribution ~direction_bias distribution) (fun prepared ->
    let dimension = prepared_dimension prepared in
    match prepared with
    | Prepared_custom_discrete_text
        { values; cumulative; total; last_positive } ->
        randomize_text ?cancel ~grain ~selection ?seed_attribute
          ?fraction_attribute ~seed ~owner ~name ~operation ~scale ~minimum
          ~maximum ~values ~cumulative ~total ~last_positive geometry
    | prepared ->
    Result.bind (prepare_limits dimension minimum maximum) (fun limits ->
    let target = existing_planes ~owner ~name geometry in
    let target_result = match target with
      | None ->
          if owner = Attribute.Point && String.equal name "P" then assert false;
          Ok (kind_of_dimension dimension,
            Array.init dimension (fun _ -> Array.make count 0.))
      | Some (_, planes) when Array.length planes = 0 -> Error
          "Rdk.Attribute_ops.randomize: existing attribute is not floating point"
      | Some (kind, _) when kind_dimension kind <> dimension -> Error
          "Rdk.Attribute_ops.randomize: distribution dimension does not match target"
      | Some (kind, planes) -> Ok (kind, Array.map Array.copy planes) in
    Result.bind target_result (fun (kind, output) ->
    Result.bind (if fraction_attribute = None then
        seed_values owner seed_attribute geometry
      else match seed_attribute with
        | None -> Ok None
        | Some _ -> Error
            "Rdk.Attribute_ops.randomize: seed and fraction attributes are mutually exclusive")
      (fun seeds ->
    Result.bind (fraction_values owner fraction_attribute
        (expected_fraction_dimension prepared) geometry) (fun fractions ->
      let ranges = if count = 0 then 0 else (count + grain - 1) / grain in
      let errors = Array.make ranges (-1) in
      Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(ranges - 1) (fun range ->
        let first = range * grain and last = min count ((range + 1) * grain) in
        match prepared, fractions, limits with
        | Prepared_uniform { min; span }, None, No_limits ->
          for element = first to last - 1 do
            if element land 4095 = 0 then Cancel.check_opt cancel;
            if match selection with None -> true
                | Some group -> Group.mem element group then begin
              let element_seed = match seeds with
                | None -> element | Some values -> values.(element) in
              let identity = element_seed * 0x1e3779b97f4a7c15 in
              for component = 0 to dimension - 1 do
                let stream = identity lxor
                    ((component + 1) * 0x11b54a32d192ed03) in
                let generated = (min.(component) +.
                    (Rays_math.Rand.float_at seed ~index:stream *. span.(component))) *.
                    scale
                and previous = output.(component).(element) in
                let value = match operation with
                  | Random_set -> generated
                  | Random_add -> previous +. generated
                  | Random_minimum -> Float.min previous generated
                  | Random_maximum -> Float.max previous generated
                  | Random_multiply -> previous *. generated in
                output.(component).(element) <- value;
                if errors.(range) < 0 && not (Float.is_finite value) then
                  errors.(range) <- element
              done
            end
          done
        | _ ->
        let scratch = Array.make 4 0. in
        for element = first to last - 1 do
          if element land 4095 = 0 then Cancel.check_opt cancel;
          if match selection with None -> true | Some group -> Group.mem element group
          then begin
            let element_seed = match seeds with
              | None -> element | Some values -> values.(element) in
            let identity = element_seed * 0x1e3779b97f4a7c15 in
            let fraction_valid = match fractions with
              | None -> true
              | Some planes ->
                  let component = ref 0 and valid = ref true in
                  while !component < Array.length planes && !valid do
                    let value = planes.(!component).(element) in
                    valid := Float.is_finite value && value >= 0. && value <= 1.;
                    incr component
                  done;
                  !valid in
            if not fraction_valid then errors.(range) <- element
            else begin
              sample_prepared prepared fractions seed identity element scratch;
              for component = 0 to dimension - 1 do
              let generated = limit_sample limits component
                  scratch.(component) *. scale
              and previous = output.(component).(element) in
              let value = match operation with
                | Random_set -> generated
                | Random_add -> previous +. generated
                | Random_minimum -> Float.min previous generated
                | Random_maximum -> Float.max previous generated
                | Random_multiply -> previous *. generated in
              output.(component).(element) <- value;
              if errors.(range) < 0 && not (Float.is_finite value) then
                errors.(range) <- element
              done
            end
          end
        done);
      match Array.find_opt (fun element -> element >= 0) errors with
      | Some element -> Error (Printf.sprintf
          "Rdk.Attribute_ops.randomize: output is non-finite at element %d" element)
      | None ->
          let changed = match target with
            | Some (_, source) -> changed source output
            | None -> true in
          if not changed then Ok geometry
          else Result.bind (install ~owner ~name kind output geometry) (fun geometry ->
            Ok (if kind = Position then remove_stale_normals geometry else geometry))
    ))))))

type noise_coordinates = { nx : float array; ny : float array; nz : float array }

let noise_coordinates ~owner ~location geometry =
  let count = owner_count geometry owner in
  let topology = Geometry.topology geometry in
  let position = Packed.Float3.Private.view (Geometry.positions geometry) in
  let from_attribute name =
    if owner = Attribute.Point && String.equal name "P" then
      Ok { nx = position.x; ny = position.y; nz = position.z }
    else match Geometry.find_attribute ~owner name geometry with
      | None -> Error (Printf.sprintf
          "Rdk.Attribute_ops.noise: missing location attribute %s" name)
      | Some attribute ->
          (match Attribute.Private.storage attribute with
           | Attribute.Float3 values ->
               let values = Packed.Float3.Private.view values in
               Ok { nx = values.x; ny = values.y; nz = values.z }
           | _ -> Error (Printf.sprintf
               "Rdk.Attribute_ops.noise: location attribute %s must have float3 storage"
               name))
  in
  match location with
  | Noise_attribute name when String.trim name = "" ->
      Error "Rdk.Attribute_ops.noise: location attribute name must not be empty"
  | Noise_attribute name -> from_attribute name
  | Noise_element_number ->
      Ok { nx = Array.init count float_of_int; ny = Array.make count 0.;
        nz = Array.make count 0. }
  | Noise_position ->
      (match owner with
       | Attribute.Point -> from_attribute "P"
       | Attribute.Vertex ->
           let nx = Array.make count 0. and ny = Array.make count 0.
           and nz = Array.make count 0. in
           for vertex = 0 to count - 1 do
             let point = Topology.point_of_vertex topology vertex in
             nx.(vertex) <- position.x.(point);
             ny.(vertex) <- position.y.(point);
             nz.(vertex) <- position.z.(point)
           done;
           Ok { nx; ny; nz }
       | Attribute.Primitive ->
           let nx = Array.make count 0. and ny = Array.make count 0.
           and nz = Array.make count 0. in
           for primitive = 0 to count - 1 do
             let first, last = Topology.primitive_vertex_range topology primitive in
             let size = last - first in
             for vertex = first to last - 1 do
               let point = Topology.point_of_vertex topology vertex in
               nx.(primitive) <- nx.(primitive) +. position.x.(point);
               ny.(primitive) <- ny.(primitive) +. position.y.(point);
               nz.(primitive) <- nz.(primitive) +. position.z.(point)
             done;
             if size > 0 then begin
               let inverse = 1. /. float_of_int size in
               nx.(primitive) <- nx.(primitive) *. inverse;
               ny.(primitive) <- ny.(primitive) *. inverse;
               nz.(primitive) <- nz.(primitive) *. inverse
             end
           done;
           Ok { nx; ny; nz }
       | Attribute.Detail ->
           let nx = [|0.|] and ny = [|0.|] and nz = [|0.|] in
           let points = Geometry.point_count geometry in
           for point = 0 to points - 1 do
             nx.(0) <- nx.(0) +. position.x.(point);
             ny.(0) <- ny.(0) +. position.y.(point);
             nz.(0) <- nz.(0) +. position.z.(point)
           done;
           if points > 0 then begin
             let inverse = 1. /. float_of_int points in
             nx.(0) <- nx.(0) *. inverse;
             ny.(0) <- ny.(0) *. inverse;
             nz.(0) <- nz.(0) *. inverse
           end;
           Ok { nx; ny; nz })

let noise ?cancel ~grain ?selection ~seed ~owner ~name ~kind ~location ~range
    ~operation ~blend ~frequency ~offset ~octaves ~lacunarity ~roughness
    geometry =
  if grain <= 0 then invalid_arg "Rdk.Attribute_ops.noise: grain must be positive";
  let dimension = match kind with
    | Noise_float -> 1 | Noise_vector -> 3 | Noise_quaternion -> 4 in
  if String.trim name = "" then Error
      "Rdk.Attribute_ops.noise: attribute name must not be empty"
  else if not (Float.is_finite blend && blend >= 0. && blend <= 1.) then Error
      "Rdk.Attribute_ops.noise: blend must be finite and in [0, 1]"
  else if not (Float.is_finite frequency.Rays_math.Vec3.x
      && Float.is_finite frequency.y && Float.is_finite frequency.z
      && Float.is_finite offset.Rays_math.Vec3.x && Float.is_finite offset.y
      && Float.is_finite offset.z && Float.is_finite lacunarity
      && Float.is_finite roughness) then Error
      "Rdk.Attribute_ops.noise: numeric parameters must be finite"
  else if octaves < 1 || octaves > 64 then Error
      "Rdk.Attribute_ops.noise: octaves must be in [1, 64]"
  else if lacunarity <= 0. || roughness < 0. || roughness > 1. then Error
      "Rdk.Attribute_ops.noise: lacunarity must be positive and roughness in [0, 1]"
  else if kind = Noise_quaternion && operation <> Noise_set
      && operation <> Noise_set_initial then Error
      "Rdk.Attribute_ops.noise: quaternion output supports set operations only"
  else
    let count = owner_count geometry owner in
    Result.bind (validate_selection "noise" owner count selection) (fun () ->
    Result.bind (noise_coordinates ~owner ~location geometry) (fun coordinates ->
    let bounds = match range with
      | Noise_positive -> Ok (Array.make dimension 0., Array.make dimension 1.)
      | Noise_zero_centered ->
          Ok (Array.make dimension (-1.), Array.make dimension 1.)
      | Noise_min_max (minimum, maximum) ->
          let minimum = value_array minimum and maximum = value_array maximum in
          if Array.length minimum <> dimension || Array.length maximum <> dimension
          then Error "Rdk.Attribute_ops.noise: range dimension does not match output"
          else if not (finite_array minimum && finite_array maximum) then Error
              "Rdk.Attribute_ops.noise: range values must be finite"
          else if not (Array.for_all2 (fun a b -> a <= b) minimum maximum)
          then Error "Rdk.Attribute_ops.noise: range minimum must not exceed maximum"
          else Ok (minimum, maximum)
    in
    Result.bind bounds (fun (minimum, maximum) ->
    let target = existing_planes ~owner ~name geometry in
    if operation = Noise_set_initial && target <> None then Ok geometry
    else
      let target_result = match target with
        | None ->
            let planes = Array.init dimension (fun _ -> Array.make count 0.) in
            if kind = Noise_quaternion then Array.fill planes.(3) 0 count 1.;
            Ok (kind_of_dimension dimension, planes)
        | Some (_, planes) when Array.length planes = 0 -> Error
            "Rdk.Attribute_ops.noise: existing attribute is not floating point"
        | Some (target_kind, _) when kind_dimension target_kind <> dimension ->
            Error "Rdk.Attribute_ops.noise: output dimension does not match target"
        | Some (target_kind, planes) -> Ok (target_kind, Array.map Array.copy planes)
      in
      Result.bind target_result (fun (target_kind, output) ->
      let source = match target with None -> None | Some (_, planes) -> Some planes in
      let generator = Rays_math.Noise.create seed in
      let ranges = if count = 0 then 0 else (count + grain - 1) / grain in
      let errors = Array.make ranges (-1) in
      Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(ranges - 1) (fun range_index ->
        let first = range_index * grain
        and last = min count ((range_index + 1) * grain) in
        let scratch = Rays_math.Noise.Private.create_fbm3_scratch () in
        let generated = Array.make 4 0. in
        for element = first to last - 1 do
          if element land 4095 = 0 then Cancel.check_opt cancel;
          if match selection with None -> true | Some group -> Group.mem element group
          then begin
            let x = (coordinates.nx.(element) +. offset.x) *. frequency.x
            and y = (coordinates.ny.(element) +. offset.y) *. frequency.y
            and z = (coordinates.nz.(element) +. offset.z) *. frequency.z in
            for component = 0 to dimension - 1 do
              let shift = float_of_int component *. 47.117 in
              let sample = Rays_math.Noise.Private.fbm3_with_scratch scratch generator
                  ~octaves ~lacunarity ~gain:roughness ~x:(x +. shift)
                  ~y:(y -. (shift *. 0.37)) ~z:(z +. (shift *. 0.73)) in
              generated.(component) <- minimum.(component) +.
                  (sample *. (maximum.(component) -. minimum.(component)))
            done;
            if kind = Noise_quaternion then begin
              let length = sqrt (Array.fold_left
                  (fun sum value -> sum +. (value *. value)) 0. generated) in
              if length > 1e-15 && Float.is_finite length then
                for component = 0 to 3 do
                  generated.(component) <- generated.(component) /. length
                done
              else begin
                generated.(0) <- 0.; generated.(1) <- 0.;
                generated.(2) <- 0.; generated.(3) <- 1.
              end
            end;
            for component = 0 to dimension - 1 do
              let previous = match source with
                | None -> 0. | Some planes -> planes.(component).(element) in
              let noise = generated.(component) in
              let operated = match operation with
                | Noise_set_initial | Noise_set -> noise
                | Noise_add -> previous +. noise
                | Noise_subtract -> previous -. noise
                | Noise_multiply -> previous *. noise
                | Noise_minimum -> Float.min previous noise
                | Noise_maximum -> Float.max previous noise in
              let value = previous +. (blend *. (operated -. previous)) in
              output.(component).(element) <- value;
              if errors.(range_index) < 0 && not (Float.is_finite value) then
                errors.(range_index) <- element
            done;
            if kind = Noise_quaternion then begin
              let length = sqrt ((output.(0).(element) ** 2.)
                  +. (output.(1).(element) ** 2.)
                  +. (output.(2).(element) ** 2.)
                  +. (output.(3).(element) ** 2.)) in
              if length > 1e-15 && Float.is_finite length then
                for component = 0 to 3 do
                  output.(component).(element) <-
                    output.(component).(element) /. length
                done
              else begin
                output.(0).(element) <- 0.; output.(1).(element) <- 0.;
                output.(2).(element) <- 0.; output.(3).(element) <- 1.
              end
            end
          end
        done);
      match Array.find_opt (fun element -> element >= 0) errors with
      | Some element -> Error (Printf.sprintf
          "Rdk.Attribute_ops.noise: non-finite output at element %d" element)
      | None ->
          let changed = match source with None -> true | Some source -> changed source output in
          if not changed then Ok geometry
          else Result.bind (install ~owner ~name target_kind output geometry)
              (fun geometry -> Ok (if target_kind = Position then
                  remove_stale_normals geometry else geometry))
    ))))

let validate_ramp ramp =
  let knots = Array.of_list ramp in
  if Array.length knots = 0 then Ok None
  else begin
    let failure = ref None in
    Array.iteri (fun index (position, value) ->
      if !failure = None then
        if not (Float.is_finite position && Float.is_finite value) then
          failure := Some "ramp knots must be finite"
        else if position < 0. || position > 1. then
          failure := Some "ramp positions must lie in [0, 1]"
        else if index > 0 && position <= fst knots.(index - 1) then
          failure := Some "ramp positions must be strictly increasing") knots;
    match !failure with
    | Some message -> Error ("Rdk.Attribute_ops.remap: " ^ message)
    | None when fst knots.(0) <> 0. || fst knots.(Array.length knots - 1) <> 1. ->
        Error "Rdk.Attribute_ops.remap: ramp must have endpoints at 0 and 1"
    | None -> Ok (Some knots)
  end

let[@inline] ramp_sample knots value =
  if value <= 0. then snd knots.(0)
  else if value >= 1. then snd knots.(Array.length knots - 1)
  else begin
    let low = ref 0 and high = ref (Array.length knots - 1) in
    while !high - !low > 1 do
      let middle = (!low + !high) / 2 in
      if fst knots.(middle) <= value then low := middle else high := middle
    done;
    let x0, y0 = knots.(!low) and x1, y1 = knots.(!high) in
    y0 +. (((value -. x0) /. (x1 -. x0)) *. (y1 -. y0))
  end

let remap ?cancel ~grain ?selection ~owner ~name ?into ~input ~output_min
    ~output_max ~policy ~ramp geometry =
  if grain <= 0 then invalid_arg "Rdk.Attribute_ops.remap: grain must be positive";
  let into = Option.value ~default:name into in
  if String.trim name = "" || String.trim into = "" then Error
      "Rdk.Attribute_ops.remap: attribute names must not be empty"
  else if owner = Attribute.Point && String.equal into "P"
      && not (String.equal name "P") then Error
      "Rdk.Attribute_ops.remap: only canonical P can write canonical P"
  else
    let count = owner_count geometry owner in
    Result.bind (validate_selection "remap" owner count selection) (fun () ->
    Result.bind (source_planes ~owner ~name geometry) (fun (source_kind, source) ->
    let dimension = kind_dimension source_kind in
    let output_parameters = [|value_array output_min; value_array output_max|] in
    Result.bind (same_dimensions "remap" output_parameters) (fun output_dimension ->
    if output_dimension <> dimension then Error
        "Rdk.Attribute_ops.remap: output range dimension does not match source"
    else Result.bind (validate_ramp ramp) (fun ramp ->
    let input_result = match input with
      | Remap_explicit { min; max } ->
          let parameters = [|value_array min; value_array max|] in
          Result.bind (same_dimensions "remap" parameters) (fun input_dimension ->
            if input_dimension <> dimension then Error
                "Rdk.Attribute_ops.remap: input range dimension does not match source"
            else if not (Array.for_all2 (fun min max -> max > min)
                parameters.(0) parameters.(1)) then Error
                "Rdk.Attribute_ops.remap: explicit input maximum must exceed minimum"
            else Ok parameters)
      | Remap_auto ->
          let ranges = if count = 0 then 0 else (count + grain - 1) / grain in
          let minima = Array.init dimension (fun _ -> Array.make ranges Float.infinity)
          and maxima = Array.init dimension (fun _ -> Array.make ranges Float.neg_infinity)
          and errors = Array.make ranges (-1) in
          Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(ranges - 1) (fun range ->
            let first = range * grain and last = min count ((range + 1) * grain) in
            for element = first to last - 1 do
              if element land 4095 = 0 then Cancel.check_opt cancel;
              if match selection with None -> true | Some group -> Group.mem element group
              then for component = 0 to dimension - 1 do
                let value = source.(component).(element) in
                if not (Float.is_finite value) then errors.(range) <- element
                else begin
                  if value < minima.(component).(range) then
                    minima.(component).(range) <- value;
                  if value > maxima.(component).(range) then
                    maxima.(component).(range) <- value
                end
              done
            done);
          (match Array.find_opt (fun element -> element >= 0) errors with
           | Some element -> Error (Printf.sprintf
               "Rdk.Attribute_ops.remap: source is non-finite at element %d" element)
           | None ->
               let minimum = Array.make dimension Float.infinity
               and maximum = Array.make dimension Float.neg_infinity in
               for component = 0 to dimension - 1 do
                 for range = 0 to ranges - 1 do
                   if minima.(component).(range) < minimum.(component) then
                     minimum.(component) <- minima.(component).(range);
                   if maxima.(component).(range) > maximum.(component) then
                     maximum.(component) <- maxima.(component).(range)
                 done
               done;
               Ok [|minimum; maximum|]) in
    Result.bind input_result (fun input_range ->
    let target_kind = if owner = Attribute.Point && String.equal into "P" then Position
        else if source_kind = Position then Float3 else source_kind in
    let destination = existing_planes ~owner ~name:into geometry in
    let output_result = match destination with
      | None -> Ok (Array.init dimension (fun _ -> Array.make count 0.))
      | Some (_, planes) when Array.length planes = 0 -> Error
          "Rdk.Attribute_ops.remap: destination attribute is not floating point"
      | Some (kind, _) when kind_dimension kind <> dimension -> Error
          "Rdk.Attribute_ops.remap: destination dimension does not match source"
      | Some (_, planes) -> Ok (Array.map Array.copy planes) in
    Result.bind output_result (fun output ->
      let ranges = if count = 0 then 0 else (count + grain - 1) / grain in
      let errors = Array.make ranges (-1) in
      Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(ranges - 1) (fun range ->
        let first = range * grain and last = min count ((range + 1) * grain) in
        for element = first to last - 1 do
          if element land 4095 = 0 then Cancel.check_opt cancel;
          if match selection with None -> true | Some group -> Group.mem element group
          then for component = 0 to dimension - 1 do
            let value = source.(component).(element)
            and minimum = input_range.(0).(component)
            and maximum = input_range.(1).(component) in
            let normalized = if maximum = minimum then 0.
              else (value -. minimum) /. (maximum -. minimum) in
            let shaped = match policy with
              | Remap_extrapolate -> normalized
              | Remap_clamp ->
                  let normalized = Float.max 0. (Float.min 1. normalized) in
                  (match ramp with None -> normalized
                   | Some knots -> ramp_sample knots normalized)
              | Remap_cycle ->
                  let normalized = if normalized >= 0. && normalized <= 1.
                      then normalized else normalized -. floor normalized in
                  (match ramp with None -> normalized
                   | Some knots -> ramp_sample knots normalized) in
            let mapped = output_parameters.(0).(component) +. (shaped *.
                (output_parameters.(1).(component)
                  -. output_parameters.(0).(component))) in
            output.(component).(element) <- mapped;
            if errors.(range) < 0 && not (Float.is_finite mapped) then
              errors.(range) <- element
          done
        done);
      match Array.find_opt (fun element -> element >= 0) errors with
      | Some element -> Error (Printf.sprintf
          "Rdk.Attribute_ops.remap: output is non-finite at element %d" element)
      | None ->
          let same_source = String.equal name into in
          let source_changed = same_source && changed source output in
          if same_source && not source_changed then Ok geometry
          else Result.bind (install ~owner ~name:into target_kind output geometry)
            (fun geometry -> Ok (if target_kind = Position && source_changed
              then remove_stale_normals geometry else geometry))
    ))))))
