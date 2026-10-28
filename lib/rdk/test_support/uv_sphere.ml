open Rdk_core

let normalize_plane_axis = Plane_generators.normalize_plane_axis

type sphere_connectivity =
  | Sphere_triangles
  | Sphere_alternating_triangles
  | Sphere_quads
  | Sphere_rows
  | Sphere_columns
  | Sphere_rows_and_columns
  | Sphere_points
type sphere_normals =
  | Sphere_no_normals | Sphere_point_normals | Sphere_vertex_normals
type sphere_orientation =
  | Sphere_x | Sphere_y | Sphere_z | Sphere_axis of Rays_math.Vec3.t
type sphere_rotation_order =
  | Sphere_xyz | Sphere_xzy | Sphere_yxz
  | Sphere_yzx | Sphere_zxy | Sphere_zyx
let sphere_rotation_matrix order rotation =
  let x = Rays_math.Mat4.rotation_x rotation.Rays_math.Vec3.x
  and y = Rays_math.Mat4.rotation_y rotation.y
  and z = Rays_math.Mat4.rotation_z rotation.z in
  match order with
  | Sphere_xyz -> Rays_math.Mat4.mul z (Rays_math.Mat4.mul y x)
  | Sphere_xzy -> Rays_math.Mat4.mul y (Rays_math.Mat4.mul z x)
  | Sphere_yxz -> Rays_math.Mat4.mul z (Rays_math.Mat4.mul x y)
  | Sphere_yzx -> Rays_math.Mat4.mul x (Rays_math.Mat4.mul z y)
  | Sphere_zxy -> Rays_math.Mat4.mul y (Rays_math.Mat4.mul x z)
  | Sphere_zyx -> Rays_math.Mat4.mul x (Rays_math.Mat4.mul y z)

let sphere_frame orientation rotation_order (rotation : Rays_math.Vec3.t) =
  let oriented = match orientation with
    | Sphere_x -> Ok (Rays_math.Vec3.unit_y, Rays_math.Vec3.unit_x,
        Rays_math.Vec3.create 0. 0. (-1.))
    | Sphere_y -> Ok (Rays_math.Vec3.unit_x, Rays_math.Vec3.unit_y, Rays_math.Vec3.unit_z)
    | Sphere_z -> Ok (Rays_math.Vec3.unit_x, Rays_math.Vec3.unit_z,
        Rays_math.Vec3.create 0. (-1.) 0.)
    | Sphere_axis axis ->
        Result.bind (normalize_plane_axis "Rdk.Uv_sphere.uv_sphere" "pole" axis)
          (fun pole ->
            let ax = abs_float pole.x and ay = abs_float pole.y
            and az = abs_float pole.z in
            let reference = if ax <= ay && ax <= az then Rays_math.Vec3.unit_x
              else if ay <= az then Rays_math.Vec3.unit_y else Rays_math.Vec3.unit_z in
            let projection = Rays_math.Vec3.dot reference pole in
            let radial = Rays_math.Vec3.create
                (reference.x -. (projection *. pole.x))
                (reference.y -. (projection *. pole.y))
                (reference.z -. (projection *. pole.z)) in
            Result.bind (normalize_plane_axis "Rdk.Uv_sphere.uv_sphere" "radial"
                radial) (fun radial ->
              Result.map (fun tangent -> radial, pole, tangent)
                (normalize_plane_axis "Rdk.Uv_sphere.uv_sphere" "tangent"
                   (Rays_math.Vec3.cross radial pole)))) in
  Result.map (fun (radial, pole, tangent) ->
    if rotation.x = 0. && rotation.y = 0. && rotation.z = 0. then
      radial, pole, tangent
    else
      let matrix = sphere_rotation_matrix rotation_order rotation in
      Rays_math.Mat4.transform_direction matrix radial,
      Rays_math.Mat4.transform_direction matrix pole,
      Rays_math.Mat4.transform_direction matrix tangent) oriented

let run
    ?(connectivity = Sphere_triangles)

    ?(center = Rays_math.Vec3.zero)

    ?(segments = 48) ?(rings = 24)
    ~radius () =
  let cancel = None in let grain = (16_384) in let normals = None in let orientation = (Sphere_y) in let radius_x = None in let radius_y = None in let radius_z = None in let rotation = (Rays_math.Vec3.zero) in let rotation_order = (Sphere_xyz) in let triangular_poles = (true) in let uniform_scale = (1.) in let unique_points_per_pole = (false) in let uv_attribute = None in Error.guard ~operation:"uv_sphere" ~code:"invalid_parameter" @@ fun () ->
  let point_mode = connectivity = Sphere_points in
  let normal_mode = match normals with
    | Some mode -> mode
    | None -> Sphere_point_normals in
  let rx = Option.value ~default:radius radius_x *. uniform_scale
  and ry = Option.value ~default:radius radius_y *. uniform_scale
  and rz = Option.value ~default:radius radius_z *. uniform_scale in
  if grain <= 0 then Error "Rdk.Uv_sphere.uv_sphere: grain must be positive"
  else if segments < 3 then
    Error "Rdk.Uv_sphere.uv_sphere: segments must be at least 3"
  else if rings < 2 then Error "Rdk.Uv_sphere.uv_sphere: rings must be at least 2"
  else if segments = max_int || rings = max_int then
    Error "Rdk.Uv_sphere.uv_sphere: output cardinality overflows"
  else if not (Float.is_finite radius && radius > 0. && Float.is_finite uniform_scale
      && uniform_scale > 0. && Float.is_finite rx && Float.is_finite ry && Float.is_finite rz
      && rx > 0. && ry > 0. && rz > 0.) then
    Error "Rdk.Uv_sphere.uv_sphere: radii and uniform scale must be finite and positive"
  else if not (Float.is_finite center.x && Float.is_finite center.y && Float.is_finite center.z
      && Float.is_finite rotation.x && Float.is_finite rotation.y && Float.is_finite rotation.z) then
    Error "Rdk.Uv_sphere.uv_sphere: center and rotation must be finite"
  else if (match uv_attribute with
      | Some name -> String.trim name = "" || String.equal name "P"
          || String.equal name "N"
      | None -> false) then
    Error "Rdk.Uv_sphere.uv_sphere: UV attribute name must be non-empty and cannot be P or N"
  else if point_mode && normal_mode = Sphere_vertex_normals then
    Error "Rdk.Uv_sphere.uv_sphere: point output cannot carry vertex normals"
  else Result.bind (sphere_frame orientation rotation_order rotation)
      (fun (radial_axis, pole_axis, tangent_axis) ->
    let point_limit = Sys.max_array_length
    and primitive_limit = min (Sys.max_array_length - 1) Sys.max_string_length in
    let checked_mul left right limit =
      if left = 0 || right <= limit / left then Some (left * right) else None
    and checked_add left right limit =
      if right <= limit - left then Some (left + right) else None in
    let include_poles = connectivity <> Sphere_rows in
    let pole_count = if include_poles then
        if unique_points_per_pole then segments else 1
      else 0 in
    let interior_count = checked_mul (rings - 1) segments point_limit in
    let point_count = match interior_count,
        checked_mul 2 pole_count point_limit with
      | Some interior, Some poles -> checked_add interior poles point_limit
      | _ -> None in
    let row_vertices = interior_count in
    let column_vertices = checked_mul segments (rings + 1) point_limit in
    let middle_cells = checked_mul (rings - 2) segments point_limit in
    let surface_bands = checked_mul (rings - 1) segments point_limit in
    let topology_cardinality = match connectivity with
      | Sphere_points -> Some (0, 0)
      | Sphere_rows -> Option.bind row_vertices (fun vertices ->
          if rings - 1 <= primitive_limit then Some (vertices, rings - 1)
          else None)
      | Sphere_columns -> Option.bind column_vertices (fun vertices ->
          if segments <= primitive_limit then Some (vertices, segments)
          else None)
      | Sphere_rows_and_columns ->
          (match row_vertices, column_vertices with
           | Some rows, Some columns ->
               Option.bind (checked_add rows columns point_limit) (fun vertices ->
                 if segments <= primitive_limit - (rings - 1) then
                   Some (vertices, segments + rings - 1) else None)
           | _ -> None)
      | Sphere_triangles | Sphere_alternating_triangles ->
          Option.bind surface_bands (fun bands ->
            match checked_mul 2 bands primitive_limit with
            | None -> None
            | Some primitives -> Option.bind
                (checked_mul 3 primitives point_limit)
                (fun vertices -> Some (vertices, primitives)))
      | Sphere_quads ->
          (match checked_mul segments rings primitive_limit, middle_cells with
           | Some primitives, Some middle ->
               let vertices = if triangular_poles then
                   Option.bind (checked_mul 4 middle point_limit)
                     (fun middle_vertices ->
                       Option.bind (checked_mul 6 segments point_limit)
                         (fun pole_vertices -> checked_add middle_vertices
                             pole_vertices point_limit))
                 else checked_mul 4 primitives point_limit in
               Option.map (fun vertices -> vertices, primitives) vertices
           | _ -> None) in
    match point_count, topology_cardinality, interior_count, middle_cells with
    | None, _, _, _ ->
        Error "Rdk.Uv_sphere.uv_sphere: point cardinality exceeds OCaml array limits"
    | _, None, _, _ ->
        Error "Rdk.Uv_sphere.uv_sphere: topology cardinality exceeds OCaml array limits"
    | _, _, None, _ | _, _, _, None ->
        Error "Rdk.Uv_sphere.uv_sphere: output cardinality exceeds OCaml array limits"
    | Some point_count, Some (vertex_count, primitive_count),
      Some interior_count, Some middle_cells ->
        let identity_frame = orientation = Sphere_y
          && rotation.x = 0. && rotation.y = 0. && rotation.z = 0.
          && center.x = 0. && center.y = 0. && center.z = 0. in
        let compatible_normals = identity_frame
          && connectivity = Sphere_triangles && not unique_points_per_pole
          && triangular_poles && normal_mode = Sphere_point_normals
          && Option.is_none uv_attribute && uniform_scale = 1.
          && rx = radius && ry = radius && rz = radius in
        let theta_sine = Array.make (rings + 1) 0.
        and theta_cosine = Array.make (rings + 1) 0.
        and phi_sine = Array.make segments 0.
        and phi_cosine = Array.make segments 0. in
        Parallel.for_ ~chunk_size:grain ~start:0 ~finish:rings (fun ring ->
          if ring land 4095 = 0 then Cancel.check_opt cancel;
          let theta = Float.pi *. float_of_int ring /. float_of_int rings in
          theta_sine.(ring) <- sin theta;
          theta_cosine.(ring) <- cos theta);
        Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(segments - 1)
          (fun segment ->
            if segment land 4095 = 0 then Cancel.check_opt cancel;
            let phi = Float.pi *. 2. *. float_of_int segment
                /. float_of_int segments in
            phi_sine.(segment) <- sin phi;
            phi_cosine.(segment) <- cos phi);
        let top_count = pole_count and interior_base = pole_count in
        let bottom_base = interior_base + interior_count in
        let[@inline always] point_of ring segment =
          let segment = if segment = segments then 0 else segment in
          if ring = 0 then if unique_points_per_pole then segment else 0
          else if ring = rings then bottom_base
              + if unique_points_per_pole then segment else 0
          else interior_base + ((ring - 1) * segments) + segment in
        let px = Array.make point_count 0. and py = Array.make point_count 0.
        and pz = Array.make point_count 0. in
        let point_normals = match normal_mode with
          | Sphere_point_normals -> Some (Array.make point_count 0.,
              Array.make point_count 0., Array.make point_count 0.)
          | Sphere_no_normals | Sphere_vertex_normals -> None in
        let point_uv = match uv_attribute, point_mode with
          | Some _, true -> Some (Array.make point_count 0.,
              Array.make point_count 0.)
          | _ -> None in
        let minimum_radius = min rx (min ry rz) in
        let inverse_weight_x = minimum_radius /. rx
        and inverse_weight_y = minimum_radius /. ry
        and inverse_weight_z = minimum_radius /. rz in
        let[@inline always] write_normal nx ny nz at ring segment =
          let segment = if segment = segments then 0 else segment in
          if compatible_normals then begin
            nx.(at) <- px.(at) /. radius;
            ny.(at) <- py.(at) /. radius;
            nz.(at) <- pz.(at) /. radius
          end else begin
            let sine_theta = theta_sine.(ring)
            and cosine_theta = theta_cosine.(ring)
            and sine_phi = phi_sine.(segment)
            and cosine_phi = phi_cosine.(segment) in
            let tx = sine_theta *. cosine_phi and ty = cosine_theta
            and tz = sine_theta *. sine_phi in
            let gx = tx *. inverse_weight_x
            and gy = ty *. inverse_weight_y
            and gz = tz *. inverse_weight_z in
            let length = sqrt ((gx *. gx) +. (gy *. gy) +. (gz *. gz)) in
            let gx, gy, gz = if length > 0. then
                gx /. length, gy /. length, gz /. length
              else
                let score value radius = if value = 0. then neg_infinity
                  else log (abs_float value) -. log radius in
                let sx = score tx rx and sy = score ty ry
                and sz = score tz rz in
                let maximum = Float.max sx (Float.max sy sz) in
                let component value score = if value = 0. then 0.
                  else
                    let magnitude = exp (score -. maximum) in
                    if value < 0. then -.magnitude else magnitude in
                let gx = component tx sx and gy = component ty sy
                and gz = component tz sz in
                let length = sqrt ((gx *. gx) +. (gy *. gy) +. (gz *. gz)) in
                gx /. length, gy /. length, gz /. length in
            if identity_frame then begin
              nx.(at) <- gx; ny.(at) <- gy; nz.(at) <- gz
            end else begin
              nx.(at) <- (radial_axis.x *. gx) +. (pole_axis.x *. gy)
                  +. (tangent_axis.x *. gz);
              ny.(at) <- (radial_axis.y *. gx) +. (pole_axis.y *. gy)
                  +. (tangent_axis.y *. gz);
              nz.(at) <- (radial_axis.z *. gx) +. (pole_axis.z *. gy)
                  +. (tangent_axis.z *. gz)
            end
          end in
        let[@inline always] write_point point ring segment =
          let segment = if segment = segments then 0 else segment in
          let local_x, local_y, local_z = if ring = 0 then 0., ry, 0.
            else if ring = rings then 0., -.ry, 0.
            else
              let radial = theta_sine.(ring) in
              (rx *. radial) *. phi_cosine.(segment),
              ry *. theta_cosine.(ring),
              (rz *. radial) *. phi_sine.(segment) in
          let x, y, z = if identity_frame then local_x, local_y, local_z
            else
              center.x +. (radial_axis.x *. local_x)
                +. (pole_axis.x *. local_y) +. (tangent_axis.x *. local_z),
              center.y +. (radial_axis.y *. local_x)
                +. (pole_axis.y *. local_y) +. (tangent_axis.y *. local_z),
              center.z +. (radial_axis.z *. local_x)
                +. (pole_axis.z *. local_y) +. (tangent_axis.z *. local_z) in
          if Float.is_finite x && Float.is_finite y && Float.is_finite z then begin
            px.(point) <- x; py.(point) <- y; pz.(point) <- z;
            (match point_normals with
             | Some (nx, ny, nz) -> write_normal nx ny nz point ring segment
             | None -> ());
            (match point_uv with
             | Some (u, v) ->
                 u.(point) <- float_of_int segment /. float_of_int segments;
                 v.(point) <- float_of_int ring /. float_of_int rings
             | None -> ());
            true
          end else false in
        let range_count = ((point_count - 1) / grain) + 1 in
        let errors = Array.make range_count (-1) in
        Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(range_count - 1)
          (fun range ->
            let first = range * grain
            and last = min point_count ((range + 1) * grain) in
            for point = first to last - 1 do
              if point land 4095 = 0 then Cancel.check_opt cancel;
              let ring, segment = if not include_poles then
                  1 + (point / segments), point mod segments
                else if point < top_count then 0,
                  if unique_points_per_pole then point else 0
                else if point < bottom_base then
                  let local = point - interior_base in
                  1 + (local / segments), local mod segments
                else rings, if unique_points_per_pole
                    then point - bottom_base else 0 in
              if not (write_point point ring segment) && errors.(range) < 0 then
                errors.(range) <- point
            done);
        let invalid = Array.fold_left (fun first point ->
            if point < 0 then first else if first < 0 || point < first
            then point else first) (-1) errors in
        if invalid >= 0 then Error (Printf.sprintf
            "Rdk.Uv_sphere.uv_sphere: generated point %d is not finite" invalid)
        else
          let vertex_normals = match normal_mode with
            | Sphere_vertex_normals -> Some (Array.make vertex_count 0.,
                Array.make vertex_count 0., Array.make vertex_count 0.)
            | Sphere_no_normals | Sphere_point_normals -> None in
          let vertex_uv = match uv_attribute, point_mode with
            | Some _, false -> Some (Array.make vertex_count 0.,
                Array.make vertex_count 0.)
            | _ -> None in
          let[@inline always] write_vertex vertex point ring segment u_twice =
            let points = point in
            (match vertex_normals with
             | Some (nx, ny, nz) -> write_normal nx ny nz vertex ring segment
             | None -> ());
            (match vertex_uv with
             | Some (u, v) ->
                 u.(vertex) <- float_of_int u_twice
                     /. float_of_int (2 * segments);
                 v.(vertex) <- float_of_int ring /. float_of_int rings
             | None -> ());
            points in
          let topology = match connectivity with
            | Sphere_points -> Topology.empty ~point_count
            | Sphere_rows | Sphere_columns | Sphere_rows_and_columns ->
                let vertex_points = Array.make vertex_count 0
                and primitive_offsets = Array.make (primitive_count + 1) 0
                and primitive_kinds = Bytes.make primitive_count '\001' in
                let row_count = match connectivity with
                  | Sphere_rows | Sphere_rows_and_columns -> rings - 1
                  | _ -> 0 in
                let row_vertex_count = row_count * segments in
                Parallel.for_ ~chunk_size:(max 1 (grain / (rings + segments)))
                  ~start:0 ~finish:(primitive_count - 1) (fun primitive ->
                    if primitive land 1023 = 0 then Cancel.check_opt cancel;
                    if primitive < row_count then begin
                      let ring = primitive + 1 and at = primitive * segments in
                      for segment = 0 to segments - 1 do
                        let vertex = at + segment in
                        vertex_points.(vertex) <- write_vertex vertex
                            (point_of ring segment) ring segment (2 * segment)
                      done
                    end else begin
                      let column = primitive - row_count in
                      let at = row_vertex_count + (column * (rings + 1)) in
                      for ring = 0 to rings do
                        let vertex = at + ring in
                        vertex_points.(vertex) <- write_vertex vertex
                            (point_of ring column) ring column (2 * column)
                      done
                    end);
                Parallel.for_ ~chunk_size:grain ~start:0 ~finish:primitive_count
                  (fun primitive ->
                    if primitive land 4095 = 0 then Cancel.check_opt cancel;
                    primitive_offsets.(primitive) <- if primitive <= row_count
                      then primitive * segments
                      else row_vertex_count
                        + ((primitive - row_count) * (rings + 1)));
                Topology.View.create_validated_owned ~point_count
                  ~vertex_points ~primitive_offsets ~primitive_kinds
            | Sphere_triangles | Sphere_alternating_triangles | Sphere_quads ->
                let vertex_points = Array.make vertex_count 0
                and primitive_offsets = Array.make (primitive_count + 1) 0
                and primitive_kinds = Bytes.make primitive_count '\000' in
                let triangle_mode = connectivity <> Sphere_quads in
                let bottom_first = if triangle_mode then primitive_count - segments
                  else segments + middle_cells in
                let vertex_offset primitive =
                  if triangle_mode then primitive * 3
                  else if not triangular_poles then primitive * 4
                  else if primitive <= segments then primitive * 3
                  else if primitive <= bottom_first then
                    (segments * 3) + ((primitive - segments) * 4)
                  else
                    (segments * 3) + (middle_cells * 4)
                      + ((primitive - bottom_first) * 3) in
                Parallel.for_ ~chunk_size:(max 1 (grain / 6)) ~start:0
                  ~finish:(primitive_count - 1) (fun primitive ->
                    if primitive land 4095 = 0 then Cancel.check_opt cancel;
                    let at = vertex_offset primitive in
                    if primitive < segments then begin
                      let segment = primitive and next = primitive + 1 in
                      if triangle_mode || triangular_poles then begin
                        vertex_points.(at) <- write_vertex at
                            (point_of 0 segment) 0 segment ((2 * segment) + 1);
                        vertex_points.(at + 1) <- write_vertex (at + 1)
                            (point_of 1 next) 1 next (2 * next);
                        vertex_points.(at + 2) <- write_vertex (at + 2)
                            (point_of 1 segment) 1 segment (2 * segment)
                      end else begin
                        vertex_points.(at) <- write_vertex at
                            (point_of 0 segment) 0 segment (2 * segment);
                        vertex_points.(at + 1) <- write_vertex (at + 1)
                            (point_of 0 next) 0 next (2 * next);
                        vertex_points.(at + 2) <- write_vertex (at + 2)
                            (point_of 1 next) 1 next (2 * next);
                        vertex_points.(at + 3) <- write_vertex (at + 3)
                            (point_of 1 segment) 1 segment (2 * segment)
                      end
                    end else if primitive >= bottom_first then begin
                      let segment = primitive - bottom_first
                      and next = primitive - bottom_first + 1 in
                      if triangle_mode || triangular_poles then begin
                        vertex_points.(at) <- write_vertex at
                            (point_of rings segment) rings segment
                            ((2 * segment) + 1);
                        vertex_points.(at + 1) <- write_vertex (at + 1)
                            (point_of (rings - 1) segment) (rings - 1) segment
                            (2 * segment);
                        vertex_points.(at + 2) <- write_vertex (at + 2)
                            (point_of (rings - 1) next) (rings - 1) next
                            (2 * next)
                      end else begin
                        vertex_points.(at) <- write_vertex at
                            (point_of (rings - 1) segment) (rings - 1) segment
                            (2 * segment);
                        vertex_points.(at + 1) <- write_vertex (at + 1)
                            (point_of (rings - 1) next) (rings - 1) next
                            (2 * next);
                        vertex_points.(at + 2) <- write_vertex (at + 2)
                            (point_of rings next) rings next (2 * next);
                        vertex_points.(at + 3) <- write_vertex (at + 3)
                            (point_of rings segment) rings segment (2 * segment)
                      end
                    end else begin
                      let local = if triangle_mode
                          then (primitive - segments) / 2
                        else primitive - segments in
                      let half = if triangle_mode
                          then (primitive - segments) land 1 else 0 in
                      let ring = 1 + (local / segments)
                      and segment = local mod segments in
                      let next = segment + 1 in
                      let a = point_of ring segment and b = point_of ring next
                      and c = point_of (ring + 1) next
                      and d = point_of (ring + 1) segment in
                      if not triangle_mode then begin
                        vertex_points.(at) <- write_vertex at a ring segment
                            (2 * segment);
                        vertex_points.(at + 1) <- write_vertex (at + 1) b ring
                            next (2 * next);
                        vertex_points.(at + 2) <- write_vertex (at + 2) c
                            (ring + 1) next (2 * next);
                        vertex_points.(at + 3) <- write_vertex (at + 3) d
                            (ring + 1) segment (2 * segment)
                      end else
                        let alternate = connectivity
                            = Sphere_alternating_triangles
                          && ((ring - 1 + segment) land 1 = 1) in
                        if not alternate && half = 0 then begin
                          vertex_points.(at) <- write_vertex at a ring segment
                              (2 * segment);
                          vertex_points.(at + 1) <- write_vertex (at + 1) c
                              (ring + 1) next (2 * next);
                          vertex_points.(at + 2) <- write_vertex (at + 2) d
                              (ring + 1) segment (2 * segment)
                        end else if not alternate then begin
                          vertex_points.(at) <- write_vertex at a ring segment
                              (2 * segment);
                          vertex_points.(at + 1) <- write_vertex (at + 1) b ring
                              next (2 * next);
                          vertex_points.(at + 2) <- write_vertex (at + 2) c
                              (ring + 1) next (2 * next)
                        end else if half = 0 then begin
                          vertex_points.(at) <- write_vertex at a ring segment
                              (2 * segment);
                          vertex_points.(at + 1) <- write_vertex (at + 1) b ring
                              next (2 * next);
                          vertex_points.(at + 2) <- write_vertex (at + 2) d
                              (ring + 1) segment (2 * segment)
                        end else begin
                          vertex_points.(at) <- write_vertex at b ring next
                              (2 * next);
                          vertex_points.(at + 1) <- write_vertex (at + 1) c
                              (ring + 1) next (2 * next);
                          vertex_points.(at + 2) <- write_vertex (at + 2) d
                              (ring + 1) segment (2 * segment)
                        end
                    end);
                Parallel.for_ ~chunk_size:grain ~start:0 ~finish:primitive_count
                  (fun primitive ->
                    if primitive land 4095 = 0 then Cancel.check_opt cancel;
                    primitive_offsets.(primitive) <- vertex_offset primitive);
                Topology.View.create_validated_owned ~point_count
                  ~vertex_points ~primitive_offsets ~primitive_kinds in
          let positions = Packed.Float3.View.of_owned_exn ~x:px ~y:py ~z:pz in
          let attributes = ref [] in
          (match point_normals with
           | Some (x, y, z) ->
               let values = Packed.Float3.View.of_owned_exn ~x ~y ~z in
               attributes := (Attribute.create_key_owned
                   (Attribute.normal ~owner:Attribute.Point) values |> Support.get_ok)
                 :: !attributes
           | None -> ());
          (match vertex_normals with
           | Some (x, y, z) ->
               let values = Packed.Float3.View.of_owned_exn ~x ~y ~z in
               attributes := (Attribute.create_key_owned
                   (Attribute.normal ~owner:Attribute.Vertex) values |> Support.get_ok)
                 :: !attributes
           | None -> ());
          (match uv_attribute, point_uv, vertex_uv with
           | Some name, Some (x, y), None ->
               let values = Packed.Float2.of_owned ~x ~y |> Support.get_ok in
               attributes := (Attribute.create_owned ~name ~owner:Attribute.Point
                   (Attribute.Float2 values) |> Support.get_ok) :: !attributes
           | Some name, None, Some (x, y) ->
               let values = Packed.Float2.of_owned ~x ~y |> Support.get_ok in
               attributes := (Attribute.create_owned ~name ~owner:Attribute.Vertex
                   (Attribute.Float2 values) |> Support.get_ok) :: !attributes
           | None, None, None -> ()
           | _ -> assert false);
          Geometry.create ~positions ~topology ~attributes:(List.rev !attributes)
            ())
