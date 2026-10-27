open Rays_math

type deform_selection = Deform.selection =
  | Selected_points of Group.t
  | Selected_vertices of Group.t
  | Selected_primitives of Group.t
  | Selected_edges of Edge_group.t

let finite_vec3 value = Float.is_finite value.Rays_math.Vec3.x && Float.is_finite value.y && Float.is_finite value.z
let selected_bounds = Bound.Private.selected_bounds
let transform = Transform_ops.transform

type match_size_fit =
  | Translate_only
  | Stretch
  | Contain
  | Cover
  | Match_x
  | Match_y
  | Match_z
  | Match_perimeter
  | Match_area
  | Match_volume

let match_axis ?grain ~from ~into geometry =
  Error.guard ~operation:"match_axis" ~code:"invalid_axis" @@ fun () ->
  if not (finite_vec3 from && finite_vec3 into)
     || Rays_math.Vec3.length_sq from <= 1e-30 || Rays_math.Vec3.length_sq into <= 1e-30 then
    Error "Rdk.Match_size.match_axis: vectors must be finite and non-zero"
  else
    let from = Rays_math.Vec3.normalize from and into = Rays_math.Vec3.normalize into in
    let dot = max (-1.) (min 1. (Rays_math.Vec3.dot from into)) in
    let cross = Rays_math.Vec3.cross from into in
    let cross_length = Rays_math.Vec3.length cross in
    let matrix = if cross_length > 1e-15 then
        Rays_math.Mat4.rotation ~axis:(Rays_math.Vec3.scale cross (1. /. cross_length))
          (atan2 cross_length dot)
      else if dot >= 0. then Rays_math.Mat4.identity
      else
        let basis = if abs_float from.x <= abs_float from.y
            && abs_float from.x <= abs_float from.z then Rays_math.Vec3.unit_x
          else if abs_float from.y <= abs_float from.z then Rays_math.Vec3.unit_y
          else Rays_math.Vec3.unit_z in
        Rays_math.Mat4.rotation ~axis:(Rays_math.Vec3.normalize (Rays_math.Vec3.cross from basis)) Float.pi in
    Ok (transform ?grain matrix geometry)

let match_size_metric_primitives operation selection = match selection with
  | None -> Ok None
  | Some (Selected_primitives group) -> Ok (Some group)
  | Some _ -> Error (Printf.sprintf
      "Rdk.Match_size.match_size: %s must be primitive-owned for metric fitting"
      operation)

let match_size_measure ?cancel ~grain fit selection geometry =
  Result.bind (match_size_metric_primitives "bounds selection" selection)
    (fun primitives ->
  let measured = match fit with
    | Match_perimeter -> Analysis.perimeter ?cancel ~grain ?primitives geometry
    | Match_area -> Analysis.surface_area ?cancel ~grain ?primitives geometry
    | Match_volume -> Analysis.signed_volume ?cancel ~grain ?primitives geometry
    | Translate_only | Stretch | Contain | Cover | Match_x | Match_y | Match_z ->
        assert false in
  Result.bind measured (fun value ->
    let value = abs_float value in
    if not (Float.is_finite value) then Error
        "Rdk.Match_size.match_size: metric measurement is not finite"
    else if value <= 1e-20 then Error
        "Rdk.Match_size.match_size: metric fitting requires a positive measurement"
    else Ok value))

let match_size_bounds ~center ~size =
  if not (finite_vec3 center && finite_vec3 size)
     || size.x < 0. || size.y < 0. || size.z < 0. then
    Error "Rdk.Match_size.match_size: target center must be finite and target size finite and non-negative"
  else
    let hx = size.x *. 0.5 and hy = size.y *. 0.5 and hz = size.z *. 0.5 in
    let minimum = Rays_math.Vec3.create (center.x -. hx) (center.y -. hy) (center.z -. hz)
    and maximum = Rays_math.Vec3.create (center.x +. hx) (center.y +. hy) (center.z +. hz) in
    if not (finite_vec3 minimum && finite_vec3 maximum) then Error
        "Rdk.Match_size.match_size: numeric target bounds overflow"
    else Ok Analysis.{ min = minimum; max = maximum; center; size }

let match_size_transform ?cancel ~grain ?selection ~scale ~translation geometry =
  let topology = Geometry.topology geometry in
  Result.bind (Deform.validate_selection topology selection) (fun () ->
  let point_count = Geometry.point_count geometry in
  let selection_empty = match selection with
    | None -> point_count = 0
    | Some (Selected_points group | Selected_vertices group
        | Selected_primitives group) -> Group.cardinality group = 0
    | Some (Selected_edges group) -> Edge_group.cardinality group = 0 in
  if scale.Rays_math.Vec3.x = 1. && scale.y = 1. && scale.z = 1.
     && translation.Rays_math.Vec3.x = 0. && translation.y = 0. && translation.z = 0.
     || selection_empty then Ok geometry
  else begin
    let topology_view = Topology.Private.view topology in
    let needs_index = Deform.selection_needs_index selection in
    let index = if needs_index then Some (Topology_index.create ?cancel topology)
      else None in
    let selected_mask = if not needs_index then None else begin
        let mask = Bytes.make point_count '\000' in
        if point_count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
            ~finish:(point_count - 1) (fun point ->
          if point land 4095 = 0 then Cancel.check_opt cancel;
          if Deform.point_selected selection index point then
            Bytes.unsafe_set mask point '\001');
        Some mask
      end in
    let[@inline] selected point = match selected_mask with
      | Some mask -> Bytes.unsafe_get mask point <> '\000'
      | None -> Deform.point_selected selection index point in
    let source = Packed.Float3.Private.view (Geometry.positions geometry) in
    let x = Array.copy source.x and y = Array.copy source.y
    and z = Array.copy source.z in
    let range_count = if point_count = 0 then 0
      else (point_count + grain - 1) / grain in
    let errors = Array.make range_count (-1) in
    if range_count > 0 then Parallel.for_ ~chunk_size:1 ~start:0
        ~finish:(range_count - 1) (fun range ->
      let first = range * grain and last = min point_count ((range + 1) * grain) in
      for point = first to last - 1 do
        if point land 4095 = 0 then Cancel.check_opt cancel;
        if selected point then begin
          let ox = (scale.x *. source.x.(point)) +. translation.x
          and oy = (scale.y *. source.y.(point)) +. translation.y
          and oz = (scale.z *. source.z.(point)) +. translation.z in
          if Float.is_finite ox && Float.is_finite oy && Float.is_finite oz then begin
            x.(point) <- ox; y.(point) <- oy; z.(point) <- oz
          end else if errors.(range) < 0 then errors.(range) <- point
        end
      done);
    let invalid = Array.fold_left (fun first point ->
        if point < 0 then first else if first < 0 then point else min first point)
        (-1) errors in
    if invalid >= 0 then Error (Printf.sprintf
        "Rdk.Match_size.match_size: transformed point %d is not finite" invalid)
    else
      let positions = Packed.Float3.Private.of_owned_exn ~x ~y ~z in
      let output = Geometry.with_positions positions geometry |> Support.get_ok in
      if scale.x = 1. && scale.y = 1. && scale.z = 1. then Ok output
      else if abs_float scale.x <= 1e-20 || abs_float scale.y <= 1e-20
          || abs_float scale.z <= 1e-20 then
        Ok (output
          |> Geometry.without_attribute ~owner:Attribute.Point "N"
          |> Geometry.without_attribute ~owner:Attribute.Vertex "N")
      else
        let transform_normals owner output =
          match Geometry.find_attribute ~owner "N" geometry with
          | None -> Ok output
          | Some attribute ->
              (match Attribute.get (Attribute.normal ~owner) attribute with
               | None -> Ok output
               | Some packed ->
                   let source = Packed.Float3.Private.view packed in
                   let count = Array.length source.x in
                   let x = Array.copy source.x and y = Array.copy source.y
                   and z = Array.copy source.z in
                   let range_count = if count = 0 then 0
                     else (count + grain - 1) / grain in
                   let errors = Array.make range_count (-1) in
                   if range_count > 0 then Parallel.for_ ~chunk_size:1 ~start:0
                       ~finish:(range_count - 1) (fun range ->
                     let first = range * grain
                     and last = min count ((range + 1) * grain) in
                     for element = first to last - 1 do
                       if element land 4095 = 0 then Cancel.check_opt cancel;
                       let point = match owner with
                         | Attribute.Point -> element
                         | Attribute.Vertex ->
                             topology_view.vertex_points.(element)
                         | Attribute.Primitive | Attribute.Detail -> assert false in
                       if selected point then begin
                         let ox = source.x.(element) /. scale.x
                         and oy = source.y.(element) /. scale.y
                         and oz = source.z.(element) /. scale.z in
                         let length = sqrt ((ox *. ox) +. (oy *. oy) +. (oz *. oz)) in
                         if Float.is_finite length then begin
                           if length > 1e-20 then begin
                             x.(element) <- ox /. length;
                             y.(element) <- oy /. length;
                             z.(element) <- oz /. length
                           end else begin
                             x.(element) <- 0.; y.(element) <- 0.; z.(element) <- 0.
                           end
                         end else if errors.(range) < 0 then
                           errors.(range) <- element
                       end
                     done);
                   let invalid = Array.fold_left (fun first element ->
                       if element < 0 then first else if first < 0 then element
                       else min first element) (-1) errors in
                   if invalid >= 0 then Error (Printf.sprintf
                       "Rdk.Match_size.match_size: transformed %s normal %d is not finite"
                       (match owner with Attribute.Point -> "point"
                        | Attribute.Vertex -> "vertex"
                        | Attribute.Primitive | Attribute.Detail -> assert false)
                       invalid)
                   else
                     let packed = Packed.Float3.Private.of_owned_exn ~x ~y ~z in
                     let attribute = Attribute.create_key_owned
                         (Attribute.normal ~owner) packed |> Support.get_ok in
                     Geometry.with_attribute attribute output) in
        Result.bind (transform_normals Attribute.Point output)
          (transform_normals Attribute.Vertex)
  end)

let match_size ?cancel ?(grain = 16_384) ?selection ?source_selection
    ?target_selection ?(fit = Contain) ?(translate_axes = true, true, true)
    ?(scale_axes = true, true, true) ?(justify = Rays_math.Vec3.zero) ?target_justify
    ?(offset = Rays_math.Vec3.zero) ?(scale = 1.) ?target_center ?target_size ?target
    geometry =
  Error.guard ~operation:"match_size" ~code:"invalid_geometry" @@ fun () ->
  let target_justify = Option.value ~default:justify target_justify in
  let tx_enabled, ty_enabled, tz_enabled = translate_axes
  and sx_enabled, sy_enabled, sz_enabled = scale_axes in
  if grain <= 0 then Error "Rdk.Match_size.match_size: grain must be positive"
  else if not (finite_vec3 justify && finite_vec3 target_justify)
      || abs_float justify.x > 1. || abs_float justify.y > 1.
      || abs_float justify.z > 1. || abs_float target_justify.x > 1.
      || abs_float target_justify.y > 1. || abs_float target_justify.z > 1. then
    Error "Rdk.Match_size.match_size: justification components must be finite and between -1 and 1"
  else if not (finite_vec3 offset && Float.is_finite scale) || scale < 0. then
    Error "Rdk.Match_size.match_size: offset must be finite and scale finite and non-negative"
  else if Option.is_some target &&
      (Option.is_some target_center || Option.is_some target_size) then
    Error "Rdk.Match_size.match_size: geometry and numeric targets are mutually exclusive"
  else if Option.is_none target && Option.is_some target_selection then
    Error "Rdk.Match_size.match_size: target selection requires target geometry"
  else
    Result.bind (selected_bounds ?cancel ~grain ~operation:"match_size source"
        source_selection geometry) (fun source_bounds ->
    let target_bounds_result = match target with
      | Some target -> selected_bounds ?cancel ~grain
          ~operation:"match_size target" target_selection target
      | None -> match_size_bounds
          ~center:(Option.value ~default:Rays_math.Vec3.zero target_center)
          ~size:(Option.value ~default:(Rays_math.Vec3.create 1. 1. 1.) target_size) in
    Result.bind target_bounds_result (fun target_bounds ->
    let ratio source target = if source <= 1e-20 then 1. else target /. source in
    let rx = ratio source_bounds.size.x target_bounds.size.x
    and ry = ratio source_bounds.size.y target_bounds.size.y
    and rz = ratio source_bounds.size.z target_bounds.size.z in
    let uniform choose =
      let value = ref None in
      let consider source ratio = if source > 1e-20 then
        value := Some (match !value with None -> ratio
          | Some current -> choose current ratio) in
      consider source_bounds.size.x rx;
      consider source_bounds.size.y ry;
      consider source_bounds.size.z rz;
      Option.value ~default:1. !value in
    let uniform_scale value =
      let value = value *. scale in Rays_math.Vec3.create value value value in
    let computed_scale = match fit with
      | Translate_only -> Ok (Rays_math.Vec3.create 1. 1. 1.)
      | Stretch -> Ok (Rays_math.Vec3.create
          (if sx_enabled then rx *. scale else 1.)
          (if sy_enabled then ry *. scale else 1.)
          (if sz_enabled then rz *. scale else 1.))
      | Contain -> Ok (uniform_scale (uniform Float.min))
      | Cover -> Ok (uniform_scale (uniform Float.max))
      | Match_x when source_bounds.size.x <= 1e-20 -> Error
          "Rdk.Match_size.match_size: X-axis fitting requires non-degenerate source bounds"
      | Match_y when source_bounds.size.y <= 1e-20 -> Error
          "Rdk.Match_size.match_size: Y-axis fitting requires non-degenerate source bounds"
      | Match_z when source_bounds.size.z <= 1e-20 -> Error
          "Rdk.Match_size.match_size: Z-axis fitting requires non-degenerate source bounds"
      | Match_x -> Ok (uniform_scale rx)
      | Match_y -> Ok (uniform_scale ry)
      | Match_z -> Ok (uniform_scale rz)
      | Match_perimeter | Match_area | Match_volume ->
          (match target with
           | None -> Error
               "Rdk.Match_size.match_size: metric fitting requires target geometry"
           | Some target ->
               Result.bind (match_size_measure ?cancel ~grain fit
                   source_selection geometry) (fun source_measure ->
               Result.map (fun target_measure ->
                 let ratio = target_measure /. source_measure in
                 let linear = match fit with
                   | Match_perimeter -> ratio
                   | Match_area -> sqrt ratio
                   | Match_volume -> ratio ** (1. /. 3.)
                   | _ -> assert false in
                 uniform_scale linear)
                 (match_size_measure ?cancel ~grain fit target_selection target))) in
    Result.bind computed_scale (fun computed_scale ->
    if not (finite_vec3 computed_scale) then Error
        "Rdk.Match_size.match_size: computed scale is not finite"
    else
      let anchor (bounds : Analysis.bounds) justification = Rays_math.Vec3.create
          (bounds.center.x +. (justification.Rays_math.Vec3.x *. bounds.size.x *. 0.5))
          (bounds.center.y +. (justification.y *. bounds.size.y *. 0.5))
          (bounds.center.z +. (justification.z *. bounds.size.z *. 0.5)) in
      let source_anchor = anchor source_bounds justify
      and target_anchor = anchor target_bounds target_justify in
      let translation = Rays_math.Vec3.create
          (if tx_enabled then target_anchor.x +. offset.x
             -. (source_anchor.x *. computed_scale.x) else 0.)
          (if ty_enabled then target_anchor.y +. offset.y
             -. (source_anchor.y *. computed_scale.y) else 0.)
          (if tz_enabled then target_anchor.z +. offset.z
             -. (source_anchor.z *. computed_scale.z) else 0.) in
      if not (finite_vec3 translation) then Error
          "Rdk.Match_size.match_size: computed translation is not finite"
      else match_size_transform ?cancel ~grain ?selection
          ~scale:computed_scale ~translation geometry)))

let run = match_size
