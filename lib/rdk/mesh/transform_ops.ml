open Rays_math

type deform_selection = Deform.selection =
  | Selected_points of Group.t
  | Selected_vertices of Group.t
  | Selected_primitives of Group.t
  | Selected_edges of Edge_group.t

type transform_order =
  | Transform_srt | Transform_str | Transform_rst
  | Transform_rts | Transform_tsr | Transform_trs

type transform_rotation_order =
  | Transform_xyz | Transform_xzy | Transform_yxz
  | Transform_yzx | Transform_zxy | Transform_zyx

type soft_transform_metric =
  | Soft_radius
  | Soft_edge
  | Soft_attribute of { attribute : string; apply_rolloff : bool }

type soft_transform_falloff = Soft_linear | Soft_quadratic | Soft_cubic

type distance_along_radius =
  | Distance_fixed of float
  | Distance_maximum

type distance_from_geometry_reference =
  | Distance_reference_points
  | Distance_reference_primitives

type distance_from_target_projection =
  | Distance_target_spherical
  | Distance_target_cylindrical
  | Distance_target_planar

type distance_from_target_metric =
  | Distance_target_absolute
  | Distance_target_signed

let normalized_direction ?(grain = 16_384) matrix values =
  let source = Packed.Float3.Private.view values in
  let count = Array.length source.x in
  let x = Array.make count 0. and y = Array.make count 0. and z = Array.make count 0. in
  let (m00,m01,m02,_), (m10,m11,m12,_), (m20,m21,m22,_), _ =
    Rays_math.Mat4.to_rows matrix in
  if count > 0 then Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(count - 1)
      (fun index ->
        let vx = source.x.(index) and vy = source.y.(index)
        and vz = source.z.(index) in
        let ox = m00 *. vx +. m01 *. vy +. m02 *. vz
        and oy = m10 *. vx +. m11 *. vy +. m12 *. vz
        and oz = m20 *. vx +. m21 *. vy +. m22 *. vz in
        let length = sqrt ((ox *. ox) +. (oy *. oy) +. (oz *. oz)) in
        if length > 1e-20 then begin
          x.(index) <- ox /. length;
          y.(index) <- oy /. length;
          z.(index) <- oz /. length
        end);
  Packed.Float3.Private.of_owned_exn ~x ~y ~z

let transform_rotation_matrix order angles =
  let x = Rays_math.Mat4.rotation_x angles.Rays_math.Vec3.x
  and y = Rays_math.Mat4.rotation_y angles.y
  and z = Rays_math.Mat4.rotation_z angles.z in
  let first, second, third = match order with
    | Transform_xyz -> x, y, z | Transform_xzy -> x, z, y
    | Transform_yxz -> y, x, z | Transform_yzx -> y, z, x
    | Transform_zxy -> z, x, y | Transform_zyx -> z, y, x in
  Rays_math.Mat4.mul third (Rays_math.Mat4.mul second first)

let compose_transform_raw ?(order = Transform_srt)
    ?(rotation_order = Transform_xyz) ?(translate = Rays_math.Vec3.zero)
    ?(rotate = Rays_math.Vec3.zero) ?(scale = Rays_math.Vec3.create 1. 1. 1.) ?(shear = Rays_math.Vec3.zero)
    ?(uniform_scale = 1.) ?(pivot = Rays_math.Vec3.zero)
    ?(pivot_rotation = Rays_math.Vec3.zero) ?(invert = false) () =
  let vectors = ["translate", translate; "rotate", rotate; "scale", scale;
    "shear", shear; "pivot", pivot; "pivot_rotation", pivot_rotation] in
  match List.find_opt (fun (_, value) -> not (Float.is_finite value.Rays_math.Vec3.x
      && Float.is_finite value.y && Float.is_finite value.z)) vectors with
  | Some (name, _) -> Error ("Rdk.Transform_ops.compose_transform: non-finite " ^ name)
  | None when not (Float.is_finite uniform_scale) ->
      Error "Rdk.Transform_ops.compose_transform: non-finite uniform scale"
  | None ->
      let scale = Rays_math.Vec3.scale scale uniform_scale in
      let scale_shear = Rays_math.Mat4.mul
          (Rays_math.Mat4.of_rows
            (1., shear.x, shear.y, 0.)
            (0., 1., shear.z, 0.)
            (0., 0., 1., 0.)
            (0., 0., 0., 1.))
          (Rays_math.Mat4.scaling scale)
      and rotation = transform_rotation_matrix rotation_order rotate
      and translation = Rays_math.Mat4.translation translate in
      let first, second, third = match order with
        | Transform_srt -> scale_shear, rotation, translation
        | Transform_str -> scale_shear, translation, rotation
        | Transform_rst -> rotation, scale_shear, translation
        | Transform_rts -> rotation, translation, scale_shear
        | Transform_tsr -> translation, scale_shear, rotation
        | Transform_trs -> translation, rotation, scale_shear in
      let core = Rays_math.Mat4.mul third (Rays_math.Mat4.mul second first) in
      let pivot_rotation = transform_rotation_matrix rotation_order pivot_rotation in
      let pivot_inverse = Rays_math.Mat4.transpose pivot_rotation in
      let matrix = Rays_math.Mat4.mul (Rays_math.Mat4.translation pivot)
          (Rays_math.Mat4.mul pivot_rotation
            (Rays_math.Mat4.mul core
              (Rays_math.Mat4.mul pivot_inverse
                (Rays_math.Mat4.translation (Rays_math.Vec3.scale pivot (-1.)))))) in
      if not invert then Ok matrix
      else match Rays_math.Mat4.inverse matrix with
        | Some inverse -> Ok inverse
        | None -> Error "Rdk.Transform_ops.compose_transform: cannot invert a singular transform"

let transform_matrix_finite matrix =
  let rows = Rays_math.Mat4.to_rows matrix in
  let finite_row (x, y, z, w) = Float.is_finite x && Float.is_finite y
      && Float.is_finite z && Float.is_finite w in
  let a, b, c, d = rows in
  finite_row a && finite_row b && finite_row c && finite_row d

let transform_selected_positions ?cancel ~grain matrix selected geometry =
  let source = Packed.Float3.Private.view (Geometry.positions geometry) in
  let count = Array.length source.x in
  let x = Array.copy source.x and y = Array.copy source.y
  and z = Array.copy source.z in
  let (m00,m01,m02,m03), (m10,m11,m12,m13),
      (m20,m21,m22,m23), (m30,m31,m32,m33) = Rays_math.Mat4.to_rows matrix in
  if count > 0 then Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(count - 1)
      (fun point ->
        if point land 4095 = 0 then Cancel.check_opt cancel;
        if (match selected with None -> true
            | Some selected -> Group.mem point selected) then begin
          let vx = source.x.(point) and vy = source.y.(point)
          and vz = source.z.(point) in
          let ox = m00*.vx +. m01*.vy +. m02*.vz +. m03
          and oy = m10*.vx +. m11*.vy +. m12*.vz +. m13
          and oz = m20*.vx +. m21*.vy +. m22*.vz +. m23
          and ow = m30*.vx +. m31*.vy +. m32*.vz +. m33 in
          if abs_float ow <= 1e-12 then begin
            x.(point) <- ox; y.(point) <- oy; z.(point) <- oz
          end else begin
            x.(point) <- ox /. ow; y.(point) <- oy /. ow;
            z.(point) <- oz /. ow
          end
        end);
  Geometry.with_positions (Packed.Float3.Private.of_owned_exn ~x ~y ~z) geometry

let transform_selected_normals ?cancel ~grain ~preserve_length matrix selected
    owner geometry =
  match Geometry.find_attribute ~owner "N" geometry with
  | None -> Ok geometry
  | Some attribute ->
      (match Attribute.get (Attribute.normal ~owner) attribute with
       | None -> Ok geometry
       | Some packed ->
           let source = Packed.Float3.Private.view packed in
           let count = Array.length source.x in
           let x = Array.copy source.x and y = Array.copy source.y
           and z = Array.copy source.z in
           let (m00,m01,m02,_), (m10,m11,m12,_), (m20,m21,m22,_), _ =
             Rays_math.Mat4.to_rows matrix in
           let topology = Topology.Private.view (Geometry.topology geometry) in
           let selected_element element = match selected with
             | None -> true
             | Some selected -> match owner with
               | Attribute.Point -> Group.mem element selected
               | Attribute.Vertex ->
                   Group.mem topology.vertex_points.(element) selected
               | Attribute.Primitive | Attribute.Detail -> assert false in
           if count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
               ~finish:(count - 1) (fun element ->
                 if element land 4095 = 0 then Cancel.check_opt cancel;
                 if selected_element element then begin
                   let vx = source.x.(element) and vy = source.y.(element)
                   and vz = source.z.(element) in
                   let ox = m00*.vx +. m01*.vy +. m02*.vz
                   and oy = m10*.vx +. m11*.vy +. m12*.vz
                   and oz = m20*.vx +. m21*.vy +. m22*.vz in
                   let output_length = sqrt (ox*.ox +. oy*.oy +. oz*.oz) in
                   if output_length > 1e-20 then begin
                     let target_length = if preserve_length then
                         sqrt (vx*.vx +. vy*.vy +. vz*.vz) else 1. in
                     let factor = target_length /. output_length in
                     x.(element) <- ox *. factor; y.(element) <- oy *. factor;
                     z.(element) <- oz *. factor
                   end else begin
                     x.(element) <- 0.; y.(element) <- 0.; z.(element) <- 0.
                   end
                 end);
           Result.bind (Attribute.create_key_owned (Attribute.normal ~owner)
               (Packed.Float3.Private.of_owned_exn ~x ~y ~z))
             (fun normal -> Geometry.with_attribute normal geometry))

let transform_selected_raw ?cancel ?(grain = 16_384) ?selection
    ?(preserve_normal_length = false) ?(recompute_normals = false)
    matrix geometry =
  if grain <= 0 then Error "Rdk.Transform_ops.transform_selected: grain must be positive"
  else if not (transform_matrix_finite matrix) then
    Error "Rdk.Transform_ops.transform_selected: matrix must be finite"
  else Result.bind (Element_selection.validate ~operation:"Rdk.Transform_ops.transform_selected"
      (Geometry.topology geometry) selection) (fun () ->
    let point_count = Geometry.point_count geometry in
    let selected_result = match selection with
      | None -> Ok None
      | Some selection -> Result.map Option.some
          (Element_selection.promote ?cancel ~grain ~destination:Group.Point
            selection (Geometry.topology geometry)) in
    Result.bind selected_result (fun selected ->
      let selected_count = match selected with
        | None -> point_count | Some group -> Group.cardinality group in
      if selected_count = 0 || Rays_math.Mat4.nearly_equal matrix Rays_math.Mat4.identity ~eps:0.
      then Ok geometry
      else begin
        Cancel.check_opt cancel;
        let positioned = transform_selected_positions ?cancel ~grain matrix
            selected geometry in
        Result.bind positioned (fun positioned ->
          let existing_normal_owners = List.filter (fun owner ->
              Geometry.find_attribute ~owner "N" geometry <> None)
              [Attribute.Point; Attribute.Vertex] in
          if recompute_normals then
            List.fold_left (fun result owner -> Result.bind result (fun output ->
                Error.unguard (Normal_ops.run ?cancel ~grain ~owner output)))
              (Ok positioned) existing_normal_owners
          else match Rays_math.Mat4.inverse matrix with
            | None -> Ok (positioned
                |> Geometry.without_attribute ~owner:Attribute.Point "N"
                |> Geometry.without_attribute ~owner:Attribute.Vertex "N")
            | Some inverse ->
                let normal_matrix = Rays_math.Mat4.transpose inverse in
                List.fold_left (fun result owner -> Result.bind result
                    (transform_selected_normals ?cancel ~grain
                      ~preserve_length:preserve_normal_length normal_matrix
                      selected owner))
                  (Ok positioned) existing_normal_owners
        )
      end))

let soft_transform_weight falloff distance radius =
  if distance > radius then 0.
  else
    let t = Float.max 0. (Float.min 1. (distance /. radius)) in
    match falloff with
    | Soft_linear -> 1. -. t
    | Soft_quadratic -> 1. -. (t *. t)
    | Soft_cubic ->
        let t2 = t *. t in
        1. -. ((3. *. t2) -. (2. *. t2 *. t))

let validate_finite_positions ?cancel ~grain ~operation geometry =
  let positions = Packed.Float3.Private.view (Geometry.positions geometry) in
  let count = Array.length positions.x and first_invalid = Atomic.make max_int in
  if count > 0 then Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(count - 1)
      (fun point ->
        if point land 4095 = 0 then Cancel.check_opt cancel;
        if not (Float.is_finite positions.x.(point) && Float.is_finite positions.y.(point)
            && Float.is_finite positions.z.(point)) then begin
          let rec lower observed =
            if point < observed
                && not (Atomic.compare_and_set first_invalid observed point)
            then lower (Atomic.get first_invalid) in
          lower (Atomic.get first_invalid)
        end);
  let invalid = Atomic.get first_invalid in
  if invalid = max_int then Ok positions
  else Error (Printf.sprintf
      "%s: point %d has a non-finite position" operation invalid)

let soft_radius_weights ?cancel ~grain ~radius ~falloff selected geometry =
  let positions = Geometry.positions geometry in
  Result.bind (Spatial_index.create ?cancel ~grain ?points:selected positions
      |> Result.map_error Error.to_string) (fun index ->
    let count = Geometry.point_count geometry in
    let indices = Array.make count (-1) and distances = Array.make count infinity
    and counts = Array.make count 0 in
    Spatial_index.Private.nearest_k_many_into ?cancel ~grain index
      ~queries:positions ~max_distance_squared:(radius *. radius) ~capacity:1
      ~indices ~distances_squared:distances ~counts;
    let weights = Array.make count 0. in
    if count > 0 then Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(count - 1)
        (fun point ->
          if point land 4095 = 0 then Cancel.check_opt cancel;
          if counts.(point) <> 0 then
            weights.(point) <- soft_transform_weight falloff
                (sqrt distances.(point)) radius);
    Ok weights)

let soft_edge_distances ?cancel ~radius selected geometry =
  let count = Geometry.point_count geometry in
  let source_count = match selected with
    | None -> count
    | Some group -> Group.cardinality group in
  if source_count = 0 || count = 0 then Array.make count infinity
  else if source_count = count then Array.make count 0.
  else
  let topology_value = Geometry.topology geometry in
  let reverse = Topology_index.create ?cancel topology_value
      |> Topology_index.Private.view in
  let positions = Packed.Float3.Private.view (Geometry.positions geometry) in
  let lengths = Array.init (Array.length reverse.edge_a) (fun edge ->
    let a = reverse.edge_a.(edge) and b = reverse.edge_b.(edge) in
    let dx = positions.x.(b) -. positions.x.(a) and dy = positions.y.(b) -. positions.y.(a)
    and dz = positions.z.(b) -. positions.z.(a) in
    sqrt (dx*.dx +. dy*.dy +. dz*.dz)) in
  let roots = Array.make source_count 0 and filled = ref 0 in
  for point = 0 to count - 1 do
    if point land 16_383 = 0 then Cancel.check_opt cancel;
    if match selected with None -> true | Some group -> Group.mem point group
    then begin roots.(!filled) <- point; incr filled end
  done;
  fst (Shortest_paths.search ?cancel ~radius ~point_edge_offsets:reverse.point_edge_offsets
    ~point_edges:reverse.point_edges ~edge_a:reverse.edge_a ~edge_b:reverse.edge_b ~lengths roots)

let point_float_output ~operation ~name ~default geometry =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | None -> Ok (Array.make (Geometry.point_count geometry) default)
  | Some attribute -> match Attribute.Private.storage attribute with
    | Attribute.Float values ->
        if Array.length values <> Geometry.point_count geometry then Error
            (Printf.sprintf "%s: point attribute %S length mismatch"
              operation name)
        else Ok (Array.copy values)
    | _ -> Error (Printf.sprintf "%s: point attribute %S must be float"
        operation name)

let valid_output_attribute_name = function
  | None -> true
  | Some name ->
      let name = String.trim name in
      name <> "" && name <> "P"

let distance_along_geometry_raw ?cancel ?(grain = 16_384) ?affected
    ?(falloff = Soft_linear) ?(radius = Distance_maximum)
    ?(distance_attribute = Some "distance") ?mask_attribute ~start geometry =
  let operation = "Rdk.Transform_ops.distance_along_geometry" in
  if grain <= 0 then Error (operation ^ ": grain must be positive")
  else if not (valid_output_attribute_name distance_attribute) then Error
      (operation ^ ": distance attribute name must be non-empty and not P")
  else if not (valid_output_attribute_name mask_attribute) then Error
      (operation ^ ": mask attribute name must be non-empty and not P")
  else if distance_attribute = None && mask_attribute = None then Error
      (operation ^ ": enable at least one distance or mask output")
  else if match distance_attribute, mask_attribute with
      | Some distance, Some mask -> distance = mask
      | _ -> false then Error
      (operation ^ ": distance and mask attributes must have distinct names")
  else if match radius with
      | Distance_fixed value -> not (Float.is_finite value) || value <= 0.
      | Distance_maximum -> false then Error
      (operation ^ ": fixed radius must be finite and positive")
  else
    let topology = Geometry.topology geometry in
    Result.bind (Element_selection.validate ~operation topology (Some start))
      (fun () ->
    Result.bind (Element_selection.validate ~operation topology affected)
      (fun () ->
    Result.bind (validate_finite_positions ?cancel ~grain ~operation geometry)
      (fun _positions ->
    Result.bind (Element_selection.promote ?cancel ~grain
        ~name:"__distance_start" ~destination:Group.Point start topology)
      (fun start_points ->
    let affected_result = match affected with
      | None -> Ok None
      | Some selection -> Result.map Option.some
          (Element_selection.promote ?cancel ~grain
            ~name:"__distance_affected" ~destination:Group.Point
            selection topology) in
    Result.bind affected_result (fun affected_points ->
      let traversal_radius = match radius, distance_attribute with
        | Distance_maximum, _ | Distance_fixed _, Some _ -> max_float
        | Distance_fixed value, None -> value in
      let distances = soft_edge_distances ?cancel ~radius:traversal_radius
          (Some start_points) geometry in
      let point_count = Array.length distances in
      let affected_point point = match affected_points with
        | None -> true
        | Some group -> Group.mem point group in
      let maximum = match radius with
        | Distance_fixed value -> value
        | Distance_maximum ->
            let maximum = ref 0. in
            for point = 0 to point_count - 1 do
              if point land 16_383 = 0 then Cancel.check_opt cancel;
              let distance = distances.(point) in
              if affected_point point && Float.is_finite distance && distance > !maximum
              then maximum := distance
            done;
            !maximum in
      let distance_values = match distance_attribute with
        | None -> Ok None
        | Some name -> Result.map Option.some
            (point_float_output ~operation ~name ~default:(-1.) geometry) in
      Result.bind distance_values (fun distance_values ->
      let mask_values = match mask_attribute with
        | None -> Ok None
        | Some name -> Result.map Option.some
            (point_float_output ~operation ~name ~default:0. geometry) in
      Result.bind mask_values (fun mask_values ->
        if point_count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
            ~finish:(point_count - 1) (fun point ->
              if point land 4095 = 0 then Cancel.check_opt cancel;
              if affected_point point then begin
                let distance = distances.(point) in
                Option.iter (fun values ->
                    values.(point) <- if Float.is_finite distance then distance else -1.)
                  distance_values;
                Option.iter (fun values ->
                    values.(point) <- if not (Float.is_finite distance) then 0.
                      else if maximum = 0. then
                        if distance = 0. then 1. else 0.
                      else soft_transform_weight falloff distance maximum)
                  mask_values
              end);
        let attributes_result = match distance_attribute, distance_values,
            mask_attribute, mask_values with
          | Some distance_name, Some distances, Some mask_name, Some masks ->
              Result.bind (Attribute.create_owned ~owner:Attribute.Point
                  ~name:distance_name (Attribute.Float distances))
                (fun distance -> Result.map (fun mask -> [|distance; mask|])
                  (Attribute.create_owned ~owner:Attribute.Point ~name:mask_name
                    (Attribute.Float masks)))
          | Some name, Some values, None, None
          | None, None, Some name, Some values ->
              Result.map (fun attribute -> [|attribute|])
                (Attribute.create_owned ~owner:Attribute.Point ~name
                  (Attribute.Float values))
          | _ -> assert false in
        Result.bind attributes_result (fun attributes ->
          Geometry.Private.with_merged_attributes_owned attributes geometry)
      )))))))

let distance_from_geometry_raw ?cancel ?(grain = 16_384) ?affected
    ?reference_selection ?(reference_kind = Distance_reference_primitives)
    ?(falloff = Soft_linear) ?(radius = Distance_maximum)
    ?(distance_attribute = Some "distance") ?mask_attribute ~reference source =
  let operation = "Rdk.Transform_ops.distance_from_geometry" in
  if grain <= 0 then Error (operation ^ ": grain must be positive")
  else if not (valid_output_attribute_name distance_attribute) then Error
      (operation ^ ": distance attribute name must be non-empty and not P")
  else if not (valid_output_attribute_name mask_attribute) then Error
      (operation ^ ": mask attribute name must be non-empty and not P")
  else if distance_attribute = None && mask_attribute = None then Error
      (operation ^ ": enable at least one distance or mask output")
  else if match distance_attribute, mask_attribute with
      | Some distance, Some mask -> distance = mask
      | _ -> false then Error
      (operation ^ ": distance and mask attributes must have distinct names")
  else if match radius with
      | Distance_fixed value -> not (Float.is_finite value) || value <= 0.
      | Distance_maximum -> false then Error
      (operation ^ ": fixed radius must be finite and positive")
  else
    let source_topology = Geometry.topology source
    and reference_topology = Geometry.topology reference in
    Result.bind (Element_selection.validate ~operation source_topology affected)
      (fun () ->
    Result.bind (Element_selection.validate ~operation reference_topology
        reference_selection) (fun () ->
    Result.bind (validate_finite_positions ?cancel ~grain ~operation source)
      (fun _positions ->
    let affected_result = match affected with
      | None -> Ok None
      | Some selection -> Result.map Option.some
          (Element_selection.promote ?cancel ~grain
            ~name:"__distance_from_affected" ~destination:Group.Point
            selection source_topology) in
    Result.bind affected_result (fun affected_points ->
    let reference_owner = match reference_kind with
      | Distance_reference_points -> Group.Point
      | Distance_reference_primitives -> Group.Primitive in
    let reference_result = match reference_selection with
      | None -> Ok None
      | Some selection -> Result.map Option.some
          (Element_selection.promote ?cancel ~grain
            ~name:"__distance_from_reference" ~destination:reference_owner
            selection reference_topology) in
    Result.bind reference_result (fun reference_group ->
      let point_count = Geometry.point_count source in
      let distances_squared = Array.make point_count Float.infinity in
      let maximum_squared = match radius, distance_attribute with
        | Distance_maximum, _ | Distance_fixed _, Some _ -> Float.infinity
        | Distance_fixed value, None -> value *. value in
      let query_result = match reference_kind with
        | Distance_reference_points ->
            Result.bind (Spatial_index.create ?cancel ~grain
                ?points:reference_group (Geometry.positions reference)
                |> Result.map_error Error.to_string) (fun index ->
              Spatial_index.Private.nearest_distances_many_into ?cancel
                ?points:affected_points ~grain index
                ~queries:(Geometry.positions source)
                ~max_distance_squared:maximum_squared
                ~distances_squared;
              Ok ())
        | Distance_reference_primitives ->
            Result.bind (Surface_index.create ?cancel ~grain
                ?primitives:reference_group reference
                |> Result.map_error Error.to_string) (fun index ->
              Surface_index.Private.closest_distances_many_into ?cancel
                ?selection:affected_points ~grain index
                ~queries:(Geometry.positions source)
                ~max_distance_squared:maximum_squared
                ~distances_squared;
              Ok ()) in
      Result.bind query_result (fun () ->
        let affected_point point = match affected_points with
          | None -> true
          | Some group -> Group.mem point group in
        let maximum = match radius with
          | Distance_fixed value -> value
          | Distance_maximum ->
              let maximum = ref 0. in
              for point = 0 to point_count - 1 do
                if point land 16_383 = 0 then Cancel.check_opt cancel;
                let squared = distances_squared.(point) in
                if affected_point point && Float.is_finite squared then begin
                  let distance = sqrt squared in
                  if distance > !maximum then maximum := distance
                end
              done;
              !maximum in
        let distance_values = match distance_attribute with
          | None -> Ok None
          | Some name -> Result.map Option.some
              (point_float_output ~operation ~name ~default:(-1.) source) in
        Result.bind distance_values (fun distance_values ->
        let mask_values = match mask_attribute with
          | None -> Ok None
          | Some name -> Result.map Option.some
              (point_float_output ~operation ~name ~default:0. source) in
        Result.bind mask_values (fun mask_values ->
          if point_count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
              ~finish:(point_count - 1) (fun point ->
                if point land 4095 = 0 then Cancel.check_opt cancel;
                if affected_point point then begin
                  let squared = distances_squared.(point) in
                  let distance = if Float.is_finite squared then sqrt squared
                    else Float.infinity in
                  Option.iter (fun values -> values.(point) <-
                      if Float.is_finite distance then distance else -1.) distance_values;
                  Option.iter (fun values -> values.(point) <-
                      if not (Float.is_finite distance) then 0.
                      else if maximum = 0. then
                        if distance = 0. then 1. else 0.
                      else soft_transform_weight falloff distance maximum)
                    mask_values
                end);
          let attributes_result = match distance_attribute, distance_values,
              mask_attribute, mask_values with
            | Some distance_name, Some distances, Some mask_name, Some masks ->
                Result.bind (Attribute.create_owned ~owner:Attribute.Point
                    ~name:distance_name (Attribute.Float distances))
                  (fun distance -> Result.map (fun mask -> [|distance; mask|])
                    (Attribute.create_owned ~owner:Attribute.Point ~name:mask_name
                      (Attribute.Float masks)))
            | Some name, Some values, None, None
            | None, None, Some name, Some values ->
                Result.map (fun attribute -> [|attribute|])
                  (Attribute.create_owned ~owner:Attribute.Point ~name
                    (Attribute.Float values))
            | _ -> assert false in
          Result.bind attributes_result (fun attributes ->
            Geometry.Private.with_merged_attributes_owned attributes source)
        ))))))))

let distance_from_target_raw ?cancel ?(grain = 16_384) ?affected
    ?(projection = Distance_target_spherical) ?(origin = Rays_math.Vec3.zero)
    ?(direction = Rays_math.Vec3.unit_y) ?(metric = Distance_target_absolute)
    ?(falloff = Soft_linear) ?(radius = Distance_maximum)
    ?(distance_attribute = Some "distance") ?mask_attribute geometry =
  let operation = "Rdk.Transform_ops.distance_from_target" in
  if grain <= 0 then Error (operation ^ ": grain must be positive")
  else if not (valid_output_attribute_name distance_attribute) then Error
      (operation ^ ": distance attribute name must be non-empty and not P")
  else if not (valid_output_attribute_name mask_attribute) then Error
      (operation ^ ": mask attribute name must be non-empty and not P")
  else if distance_attribute = None && mask_attribute = None then Error
      (operation ^ ": enable at least one distance or mask output")
  else if match distance_attribute, mask_attribute with
      | Some distance, Some mask -> distance = mask
      | _ -> false then Error
      (operation ^ ": distance and mask attributes must have distinct names")
  else if match radius with
      | Distance_fixed value -> not (Float.is_finite value) || value <= 0.
      | Distance_maximum -> false then Error
      (operation ^ ": fixed radius must be finite and positive")
  else if not (Float.is_finite origin.Rays_math.Vec3.x && Float.is_finite origin.y && Float.is_finite origin.z) then
    Error (operation ^ ": origin must be finite")
  else if metric = Distance_target_signed
      && projection <> Distance_target_planar then Error
    (operation ^ ": signed distance is only defined for planar projection")
  else
    let needs_direction = projection <> Distance_target_spherical in
    let direction_squared = direction.Rays_math.Vec3.x *. direction.x
        +. direction.y *. direction.y +. direction.z *. direction.z in
    if needs_direction && (not (Float.is_finite direction.x && Float.is_finite direction.y
        && Float.is_finite direction.z) || not (Float.is_finite direction_squared)
        || direction_squared <= 0.) then Error
      (operation ^ ": cylindrical and planar direction must be finite and non-zero")
    else
      let topology = Geometry.topology geometry in
      Result.bind (Element_selection.validate ~operation topology affected)
        (fun () ->
      Result.bind (validate_finite_positions ?cancel ~grain ~operation geometry)
        (fun positions ->
      let affected_result = match affected with
        | None -> Ok None
        | Some selection -> Result.map Option.some
            (Element_selection.promote ?cancel ~grain
              ~name:"__distance_target_affected" ~destination:Group.Point
              selection topology) in
      Result.bind affected_result (fun affected_points ->
        let point_count = Geometry.point_count geometry in
        let distance_values = match distance_attribute with
          | None -> Ok None
          | Some name -> Result.map Option.some
              (point_float_output ~operation ~name ~default:(-1.) geometry) in
        Result.bind distance_values (fun distance_values ->
        let mask_values = match mask_attribute with
          | None -> Ok None
          | Some name -> Result.map Option.some
              (point_float_output ~operation ~name ~default:0. geometry) in
        Result.bind mask_values (fun mask_values ->
          let affected_point point = match affected_points with
            | None -> true
            | Some group -> Group.mem point group in
          let inverse_direction_length = if needs_direction then
              1. /. sqrt direction_squared else 0. in
          let nx = direction.x *. inverse_direction_length
          and ny = direction.y *. inverse_direction_length
          and nz = direction.z *. inverse_direction_length
          and ox = origin.x and oy = origin.y and oz = origin.z in
          let raw_distance point =
            let dx = positions.x.(point) -. ox
            and dy = positions.y.(point) -. oy
            and dz = positions.z.(point) -. oz in
            match projection with
            | Distance_target_spherical -> sqrt (dx*.dx +. dy*.dy +. dz*.dz)
            | Distance_target_cylindrical ->
                let axial = dx*.nx +. dy*.ny +. dz*.nz in
                sqrt (max 0. (dx*.dx +. dy*.dy +. dz*.dz -. axial*.axial))
            | Distance_target_planar ->
                let signed = dx*.nx +. dy*.ny +. dz*.nz in
                if metric = Distance_target_signed then signed
                else abs_float signed in
          let maximum_scratch = match radius, mask_values, distance_values with
            | Distance_maximum, Some _, None -> Some (Array.make point_count 0.)
            | _ -> None in
          if point_count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
              ~finish:(point_count - 1) (fun point ->
                if point land 4095 = 0 then Cancel.check_opt cancel;
                if affected_point point then begin
                  let raw = raw_distance point in
                  Option.iter (fun values -> values.(point) <- raw)
                    distance_values;
                  Option.iter (fun values -> values.(point) <- raw)
                    maximum_scratch;
                  match radius, mask_values with
                  | Distance_fixed radius, Some values ->
                      values.(point) <- soft_transform_weight falloff
                          (abs_float raw) radius
                  | _ -> ()
                end);
          (match radius, mask_values with
           | Distance_maximum, Some masks ->
               let raw_values = match distance_values, maximum_scratch with
                 | Some values, _ | None, Some values -> values
                 | None, None -> assert false in
               let maximum = ref 0. in
               for point = 0 to point_count - 1 do
                 if point land 16_383 = 0 then Cancel.check_opt cancel;
                 if affected_point point then begin
                   let value = abs_float raw_values.(point) in
                   if value > !maximum then maximum := value
                 end
               done;
               if point_count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
                   ~finish:(point_count - 1) (fun point ->
                     if point land 4095 = 0 then Cancel.check_opt cancel;
                     if affected_point point then
                       let value = abs_float raw_values.(point) in
                       masks.(point) <- if !maximum = 0. then 1.
                         else soft_transform_weight falloff value !maximum)
           | _ -> ());
          let attributes_result = match distance_attribute, distance_values,
              mask_attribute, mask_values with
            | Some distance_name, Some distances, Some mask_name, Some masks ->
                Result.bind (Attribute.create_owned ~owner:Attribute.Point
                    ~name:distance_name (Attribute.Float distances))
                  (fun distance -> Result.map (fun mask -> [|distance; mask|])
                    (Attribute.create_owned ~owner:Attribute.Point ~name:mask_name
                      (Attribute.Float masks)))
            | Some name, Some values, None, None
            | None, None, Some name, Some values ->
                Result.map (fun attribute -> [|attribute|])
                  (Attribute.create_owned ~owner:Attribute.Point ~name
                    (Attribute.Float values))
            | _ -> assert false in
          Result.bind attributes_result (fun attributes ->
            Geometry.Private.with_merged_attributes_owned attributes geometry)
        )))))

let soft_attribute_weights ?cancel ~grain ~radius ~falloff ~apply_rolloff
    selected name geometry =
  if String.trim name = "" then
    Error "Rdk.Transform_ops.soft_transform: empty distance attribute name"
  else match Geometry.find_attribute ~owner:Attribute.Point name geometry with
    | None -> Error (Printf.sprintf
        "Rdk.Transform_ops.soft_transform: missing point distance attribute %S" name)
    | Some attribute -> match Attribute.Private.storage attribute with
      | Attribute.Float values ->
          let count = Array.length values in
          let weights = Array.make count 0.
          and first_invalid = Atomic.make max_int in
          if count <> Geometry.point_count geometry then Error
              "Rdk.Transform_ops.soft_transform: distance attribute length mismatch"
          else begin
            if count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
                ~finish:(count - 1) (fun point ->
                  if point land 4095 = 0 then Cancel.check_opt cancel;
                  let value = values.(point) in
                  if not (Float.is_finite value) then begin
                    let rec lower observed =
                      if point < observed && not (Atomic.compare_and_set
                          first_invalid observed point)
                      then lower (Atomic.get first_invalid) in
                    lower (Atomic.get first_invalid)
                  end else if match selected with None -> true
                      | Some group -> Group.mem point group then
                    weights.(point) <- if apply_rolloff then
                        soft_transform_weight falloff value radius else value);
            let invalid = Atomic.get first_invalid in
            if invalid = max_int then Ok weights else Error (Printf.sprintf
              "Rdk.Transform_ops.soft_transform: distance attribute %S element %d is non-finite"
              name invalid)
          end
      | _ -> Error (Printf.sprintf
          "Rdk.Transform_ops.soft_transform: point distance attribute %S must be float" name)

let soft_transform_raw ?cancel ?(grain = 16_384) ?selection
    ?(metric = Soft_radius) ?(falloff = Soft_cubic) ?(radius = 1.)
    ?falloff_attribute ?(recompute_normals = true) matrix geometry =
  if grain <= 0 then Error "Rdk.Transform_ops.soft_transform: grain must be positive"
  else if not (Float.is_finite radius) || radius < 0. then
    Error "Rdk.Transform_ops.soft_transform: radius must be finite and non-negative"
  else if not (transform_matrix_finite matrix) then
    Error "Rdk.Transform_ops.soft_transform: matrix must be finite"
  else if (match falloff_attribute with
      | Some name -> String.trim name = "" | None -> false) then
    Error "Rdk.Transform_ops.soft_transform: empty falloff attribute name"
  else if (match metric with Soft_radius | Soft_edge -> radius <= 0.
      | Soft_attribute { apply_rolloff = true; _ } -> radius <= 0.
      | Soft_attribute { apply_rolloff = false; _ } -> false) then
    Error "Rdk.Transform_ops.soft_transform: rolloff radius must be positive"
  else Result.bind (Element_selection.validate ~operation:"Rdk.Transform_ops.soft_transform"
      (Geometry.topology geometry) selection) (fun () ->
    Result.bind (validate_finite_positions ?cancel ~grain
        ~operation:"Rdk.Transform_ops.soft_transform" geometry)
      (fun positions ->
      let selected_result = match selection with
        | None -> Ok None
        | Some selection -> Result.map Option.some
            (Element_selection.promote ?cancel ~grain ~destination:Group.Point
              selection (Geometry.topology geometry)) in
      Result.bind selected_result (fun selected ->
        let all_selected = match selected with
          | None -> true
          | Some group -> Group.cardinality group = Geometry.point_count geometry in
        let weights_result = match metric with
          | (Soft_radius | Soft_edge) when all_selected ->
              Ok (Array.make (Geometry.point_count geometry) 1.)
          | Soft_radius -> soft_radius_weights ?cancel ~grain ~radius ~falloff
              selected geometry
          | Soft_edge ->
              let distances = soft_edge_distances ?cancel ~radius selected geometry in
              let count = Array.length distances in
              let weights = Array.make count 0. in
              if count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
                  ~finish:(count - 1) (fun point ->
                    if point land 4095 = 0 then Cancel.check_opt cancel;
                    weights.(point) <- soft_transform_weight falloff
                        distances.(point) radius);
              Ok weights
          | Soft_attribute { attribute; apply_rolloff } ->
              soft_attribute_weights ?cancel ~grain ~radius ~falloff
                ~apply_rolloff selected attribute geometry in
        Result.bind weights_result (fun weights ->
          let count = Array.length weights and changed = Atomic.make false in
          let moves_positions = not (Rays_math.Mat4.nearly_equal matrix Rays_math.Mat4.identity ~eps:0.) in
          let x = Array.copy positions.x and y = Array.copy positions.y
          and z = Array.copy positions.z in
          let (m00,m01,m02,m03), (m10,m11,m12,m13),
              (m20,m21,m22,m23), (m30,m31,m32,m33) = Rays_math.Mat4.to_rows matrix in
          if count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
              ~finish:(count - 1) (fun point ->
                if point land 4095 = 0 then Cancel.check_opt cancel;
                let weight = weights.(point) in
                if moves_positions && weight <> 0. then begin
                  Atomic.set changed true;
                  let vx = positions.x.(point) and vy = positions.y.(point)
                  and vz = positions.z.(point) in
                  let ox = m00*.vx +. m01*.vy +. m02*.vz +. m03
                  and oy = m10*.vx +. m11*.vy +. m12*.vz +. m13
                  and oz = m20*.vx +. m21*.vy +. m22*.vz +. m23
                  and ow = m30*.vx +. m31*.vy +. m32*.vz +. m33 in
                  let tx, ty, tz = if abs_float ow <= 1e-12
                    then ox, oy, oz else ox /. ow, oy /. ow, oz /. ow in
                  x.(point) <- vx +. weight *. (tx -. vx);
                  y.(point) <- vy +. weight *. (ty -. vy);
                  z.(point) <- vz +. weight *. (tz -. vz)
                end);
          let positioned = if Atomic.get changed then
              Geometry.with_positions
                (Packed.Float3.Private.of_owned_exn ~x ~y ~z) geometry
            else Ok geometry in
          Result.bind positioned (fun output ->
            let output = if not (Atomic.get changed) then output
              else if recompute_normals then output
              else output |> Geometry.without_attribute ~owner:Attribute.Point "N"
                |> Geometry.without_attribute ~owner:Attribute.Vertex "N" in
            let normal_owners = if not (Atomic.get changed) || not recompute_normals
              then [] else List.filter (fun owner ->
                Geometry.find_attribute ~owner "N" geometry <> None)
                [Attribute.Point; Attribute.Vertex] in
            let output = List.fold_left (fun result owner -> Result.bind result
                (fun output -> Error.unguard (Normal_ops.run ?cancel ~grain ~owner output)))
                (Ok output) normal_owners in
            Result.bind output (fun output -> match falloff_attribute with
              | None -> Ok output
              | Some name -> Result.bind (Attribute.create_owned
                  ~owner:Attribute.Point ~name (Attribute.Float weights))
                  (fun attribute -> Geometry.with_attribute attribute output)))))))

let transform ?grain matrix geometry =
  let transformed = Kernel.transform ?grain matrix geometry in
  match Rays_math.Mat4.inverse matrix with
  | None -> transformed
      |> Geometry.without_attribute ~owner:Attribute.Point "N"
      |> Geometry.without_attribute ~owner:Attribute.Vertex "N"
  | Some inverse ->
      let normal_matrix = Rays_math.Mat4.transpose inverse in
      List.fold_left (fun output owner ->
        match Geometry.find_attribute ~owner "N" geometry with
        | None -> output
        | Some attribute ->
            (match Attribute.get (Attribute.normal ~owner) attribute with
             | None -> output
             | Some normals ->
                 let normals = normalized_direction ?grain normal_matrix normals in
                 let attribute = Attribute.create_key_owned
                     (Attribute.normal ~owner) normals |> Support.get_ok in
                 Geometry.with_attribute attribute output |> Support.get_ok))
        transformed [Attribute.Point; Attribute.Vertex]

let compose_transform ?order ?rotation_order ?translate ?rotate ?scale ?shear
    ?uniform_scale ?pivot ?pivot_rotation ?invert () =
  Result.map_error (Error.of_string ~operation:"compose_transform"
      ~code:"invalid_transform")
    (compose_transform_raw ?order ?rotation_order ?translate ?rotate ?scale
      ?shear ?uniform_scale ?pivot ?pivot_rotation ?invert ())

let transform_selected ?cancel ?grain ?selection ?preserve_normal_length
    ?recompute_normals matrix geometry =
  Error.guard ~operation:"transform_selected" ~code:"invalid_transform"
    (fun () -> transform_selected_raw ?cancel ?grain ?selection
      ?preserve_normal_length ?recompute_normals matrix geometry)

let soft_transform ?cancel ?grain ?selection ?metric ?falloff ?radius
    ?falloff_attribute ?recompute_normals matrix geometry =
  Error.guard ~operation:"soft_transform" ~code:"invalid_transform"
    (fun () -> soft_transform_raw ?cancel ?grain ?selection ?metric ?falloff
      ?radius ?falloff_attribute ?recompute_normals matrix geometry)

let distance_along_geometry ?cancel ?grain ?affected ?falloff ?radius
    ?distance_attribute ?mask_attribute ~start geometry =
  Error.guard ~operation:"distance_along_geometry" ~code:"invalid_distance"
    (fun () -> distance_along_geometry_raw ?cancel ?grain ?affected ?falloff
      ?radius ?distance_attribute ?mask_attribute ~start geometry)

let distance_from_geometry ?cancel ?grain ?affected ?reference_selection
    ?reference_kind ?falloff ?radius ?distance_attribute ?mask_attribute
    ~reference source =
  Error.guard ~operation:"distance_from_geometry" ~code:"invalid_distance"
    (fun () -> distance_from_geometry_raw ?cancel ?grain ?affected
      ?reference_selection ?reference_kind ?falloff ?radius
      ?distance_attribute ?mask_attribute ~reference source)

let distance_from_target ?cancel ?grain ?affected ?projection ?origin ?direction
    ?metric ?falloff ?radius ?distance_attribute ?mask_attribute geometry =
  Error.guard ~operation:"distance_from_target" ~code:"invalid_distance"
    (fun () -> distance_from_target_raw ?cancel ?grain ?affected ?projection
      ?origin ?direction ?metric ?falloff ?radius ?distance_attribute
      ?mask_attribute geometry)
