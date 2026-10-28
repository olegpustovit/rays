open Rdk_core

let normalize_plane_axis = Plane_generators.normalize_plane_axis

type torus_connectivity =
  | Torus_triangles
  | Torus_alternating_triangles
  | Torus_quads
  | Torus_rows
  | Torus_columns
  | Torus_rows_and_columns
  | Torus_points
type torus_normals =
  | Torus_no_normals | Torus_point_normals | Torus_vertex_normals
type torus_orientation =
  | Torus_x | Torus_y | Torus_z | Torus_axis of Rays_math.Vec3.t
type torus_rotation_order =
  | Torus_xyz | Torus_xzy | Torus_yxz
  | Torus_yzx | Torus_zxy | Torus_zyx

let torus_rotation_matrix order rotation =
  let x = Rays_math.Mat4.rotation_x rotation.Rays_math.Vec3.x
  and y = Rays_math.Mat4.rotation_y rotation.y
  and z = Rays_math.Mat4.rotation_z rotation.z in
  match order with
  | Torus_xyz -> Rays_math.Mat4.mul z (Rays_math.Mat4.mul y x)
  | Torus_xzy -> Rays_math.Mat4.mul y (Rays_math.Mat4.mul z x)
  | Torus_yxz -> Rays_math.Mat4.mul z (Rays_math.Mat4.mul x y)
  | Torus_yzx -> Rays_math.Mat4.mul x (Rays_math.Mat4.mul z y)
  | Torus_zxy -> Rays_math.Mat4.mul y (Rays_math.Mat4.mul x z)
  | Torus_zyx -> Rays_math.Mat4.mul x (Rays_math.Mat4.mul y z)

let torus_frame orientation rotation_order (rotation : Rays_math.Vec3.t) =
  let oriented = match orientation with
    | Torus_x -> Ok (Rays_math.Vec3.unit_y, Rays_math.Vec3.unit_x, Rays_math.Vec3.create 0. 0. (-1.))
    | Torus_y -> Ok (Rays_math.Vec3.unit_x, Rays_math.Vec3.unit_y, Rays_math.Vec3.unit_z)
    | Torus_z -> Ok (Rays_math.Vec3.unit_x, Rays_math.Vec3.unit_z, Rays_math.Vec3.create 0. (-1.) 0.)
    | Torus_axis axis ->
        Result.bind (normalize_plane_axis "Rdk.Parametric_generators.torus" "hole" axis)
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
            Result.bind (normalize_plane_axis "Rdk.Parametric_generators.torus" "radial" radial)
              (fun radial ->
                Result.map (fun tangent -> radial, pole, tangent)
                  (normalize_plane_axis "Rdk.Parametric_generators.torus" "tangent"
                     (Rays_math.Vec3.cross radial pole)))) in
  Result.map (fun (radial, pole, tangent) ->
    if rotation.x = 0. && rotation.y = 0. && rotation.z = 0. then
      radial, pole, tangent
    else
      let matrix = torus_rotation_matrix rotation_order rotation in
      Rays_math.Mat4.transform_direction matrix radial,
      Rays_math.Mat4.transform_direction matrix pole,
      Rays_math.Mat4.transform_direction matrix tangent) oriented

let torus ?cancel ?(grain = 16_384) ?(connectivity = Torus_triangles)
    ?normals ?(orientation = Torus_y) ?(center = Rays_math.Vec3.zero)
    ?(rotation = Rays_math.Vec3.zero) ?(rotation_order = Torus_xyz)
    ?(uniform_scale = 1.) ?(u_start = 0.) ?(u_end = 2. *. Float.pi)
    ?(v_start = 0.) ?(v_end = 2. *. Float.pi) ?(u_wrap = true)
    ?(v_wrap = true) ?(u_end_caps = false) ?(v_end_cap = false)
    ?uv_attribute ?(rows = 48) ?(columns = 24) ~major_radius ~minor_radius () =
  Error.guard ~operation:"torus" ~code:"invalid_parameter" @@ fun () ->
  let point_mode = connectivity = Torus_points in
  let polygon_mode = match connectivity with
    | Torus_triangles | Torus_alternating_triangles | Torus_quads -> true
    | Torus_rows | Torus_columns | Torus_rows_and_columns | Torus_points -> false
  in
  let normal_mode = Option.value ~default:Torus_point_normals normals in
  let major_radius_scaled = major_radius *. uniform_scale
  and minor_radius_scaled = minor_radius *. uniform_scale in
  let u_span = u_end -. u_start and v_span = v_end -. v_start in
  let minimum_u = if u_wrap then 3 else 2
  and minimum_v = if v_wrap then 3 else 2 in
  if grain <= 0 then Error "Rdk.Parametric_generators.torus: grain must be positive"
  else if rows < minimum_u then Error (Printf.sprintf
      "Rdk.Parametric_generators.torus: rows must be at least %d for the selected U wrap" minimum_u)
  else if columns < minimum_v then Error (Printf.sprintf
      "Rdk.Parametric_generators.torus: columns must be at least %d for the selected V wrap" minimum_v)
  else if not (Float.is_finite major_radius && major_radius > 0.
      && Float.is_finite minor_radius && minor_radius > 0.
      && Float.is_finite uniform_scale && uniform_scale > 0.
      && Float.is_finite major_radius_scaled && major_radius_scaled > 0.
      && Float.is_finite minor_radius_scaled && minor_radius_scaled > 0.) then
    Error "Rdk.Parametric_generators.torus: radii and uniform scale must be finite and positive"
  else if not (Float.is_finite u_start && Float.is_finite u_end && Float.is_finite v_start && Float.is_finite v_end
      && Float.is_finite u_span && Float.is_finite v_span && u_span <> 0. && v_span <> 0.) then
    Error "Rdk.Parametric_generators.torus: angle endpoints must define finite non-zero spans"
  else if not (Float.is_finite center.x && Float.is_finite center.y && Float.is_finite center.z
      && Float.is_finite rotation.x && Float.is_finite rotation.y && Float.is_finite rotation.z) then
    Error "Rdk.Parametric_generators.torus: center and rotation must be finite"
  else if (match uv_attribute with
      | Some name -> String.trim name = "" || String.equal name "P"
          || String.equal name "N"
      | None -> false) then
    Error "Rdk.Parametric_generators.torus: UV attribute name must be non-empty and cannot be P or N"
  else if point_mode && normal_mode = Torus_vertex_normals then
    Error "Rdk.Parametric_generators.torus: point output cannot carry vertex normals"
  else if (u_end_caps || v_end_cap) && not polygon_mode then
    Error "Rdk.Parametric_generators.torus: end caps require triangle or quad connectivity"
  else if u_end_caps && u_wrap then
    Error "Rdk.Parametric_generators.torus: U end caps require an open U sweep"
  else if v_end_cap && v_wrap then
    Error "Rdk.Parametric_generators.torus: a V end cap requires an open V sweep"
  else if u_end_caps && columns < 3 then
    Error "Rdk.Parametric_generators.torus: U end caps require at least three columns"
  else
    let v_chord_x = cos v_end -. cos v_start
    and v_chord_y = sin v_end -. sin v_start in
    let v_chord_scale = Float.max (abs_float v_chord_x) (abs_float v_chord_y) in
    if v_end_cap && v_chord_scale <= 64. *. Float.epsilon then
      Error "Rdk.Parametric_generators.torus: V end cap endpoints are geometrically coincident"
    else Result.bind (torus_frame orientation rotation_order rotation)
      (fun (radial_axis, pole_axis, tangent_axis) ->
      let point_limit = Sys.max_array_length
      and primitive_limit = min (Sys.max_array_length - 1) Sys.max_string_length in
      let checked_mul left right limit =
        if left = 0 || right <= limit / left then Some (left * right) else None
      and checked_add left right limit =
        if right <= limit - left then Some (left + right) else None in
      let u_cells = if u_wrap then rows else rows - 1
      and v_cells = if v_wrap then columns else columns - 1 in
      let reverse_surface = (u_span < 0.) <> (v_span < 0.)
      and u_direction = if u_span < 0. then -1. else 1. in
      let point_count = checked_mul rows columns point_limit in
      let topology_cardinality = match point_count with
        | None -> None
        | Some points ->
            (match connectivity with
             | Torus_points -> Some (0, 0, 0, 0, 0)
             | Torus_rows -> Some (points, columns, 0, 0, 0)
             | Torus_columns -> Some (points, rows, 0, 0, 0)
             | Torus_rows_and_columns ->
                 Option.bind (checked_mul 2 points point_limit) (fun vertices ->
                   Option.map (fun primitives -> vertices, primitives, 0, 0, 0)
                     (checked_add rows columns primitive_limit))
             | Torus_triangles | Torus_alternating_triangles | Torus_quads ->
                 Option.bind (checked_mul u_cells v_cells primitive_limit)
                   (fun cells ->
                     let primitives_per_cell = if connectivity = Torus_quads
                       then 1 else 2 in
                     let vertices_per_cell = primitives_per_cell * 3
                         + if primitives_per_cell = 1 then 1 else 0 in
                     Option.bind (checked_mul cells primitives_per_cell
                         primitive_limit) (fun surface_primitives ->
                     Option.bind (checked_mul cells vertices_per_cell point_limit)
                       (fun surface_vertices ->
                     let v_cap_primitives = if v_end_cap
                         then u_cells * primitives_per_cell else 0
                     and v_cap_vertices = if v_end_cap
                         then u_cells * vertices_per_cell else 0 in
                     Option.bind (checked_add surface_primitives v_cap_primitives
                         primitive_limit) (fun fixed_primitives ->
                     Option.bind (checked_add surface_vertices v_cap_vertices
                         point_limit) (fun fixed_vertices ->
                     let u_cap_primitives = if u_end_caps then 2 else 0
                     and u_cap_vertices = if u_end_caps then 2 * columns else 0 in
                     Option.bind (checked_add fixed_primitives u_cap_primitives
                         primitive_limit) (fun primitives ->
                     Option.map (fun vertices -> vertices, primitives,
                         cells, fixed_primitives, fixed_vertices)
                       (checked_add fixed_vertices u_cap_vertices point_limit)))))))) in
      match point_count, topology_cardinality with
      | None, _ | _, None ->
          Error "Rdk.Parametric_generators.torus: output cardinality exceeds OCaml array limits"
      | Some point_count,
        Some (vertex_count, primitive_count, surface_cells,
          fixed_primitive_count, fixed_vertex_count) ->
          let u_sine = Array.make rows 0. and u_cosine = Array.make rows 0.
          and u_parameter = Array.make rows 0.
          and v_sine = Array.make columns 0. and v_cosine = Array.make columns 0.
          and v_parameter = Array.make columns 0. in
          let angle start span wrap count index =
            if not wrap && index = count - 1 then start +. span
            else
              let divisor = if wrap then count else count - 1 in
              start +. (span *. float_of_int index /. float_of_int divisor) in
          Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(rows - 1)
            (fun row ->
              if row land 4095 = 0 then Cancel.check_opt cancel;
              let value = angle u_start u_span u_wrap rows row in
              u_sine.(row) <- sin value; u_cosine.(row) <- cos value;
              u_parameter.(row) <- float_of_int row
                  /. float_of_int (if u_wrap then rows else rows - 1));
          Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(columns - 1)
            (fun column ->
              if column land 4095 = 0 then Cancel.check_opt cancel;
              let value = angle v_start v_span v_wrap columns column in
              v_sine.(column) <- sin value; v_cosine.(column) <- cos value;
              v_parameter.(column) <- float_of_int column
                  /. float_of_int (if v_wrap then columns else columns - 1));
          let identity_axes = orientation = Torus_y
              && rotation.x = 0. && rotation.y = 0. && rotation.z = 0. in
          let px = Array.make point_count 0. and py = Array.make point_count 0.
          and pz = Array.make point_count 0. in
          let point_normals = match normal_mode with
            | Torus_point_normals -> Some (Array.make point_count 0.,
                Array.make point_count 0., Array.make point_count 0.)
            | Torus_no_normals | Torus_vertex_normals -> None in
          let point_uv = match uv_attribute, point_mode with
            | Some _, true -> Some (Array.make point_count 0.,
                Array.make point_count 0.)
            | _ -> None in
          let[@inline always] write_direction x_out y_out z_out at x y z =
            if identity_axes then begin
              x_out.(at) <- x; y_out.(at) <- y; z_out.(at) <- z
            end else begin
              x_out.(at) <- (radial_axis.x *. x) +. (pole_axis.x *. y)
                  +. (tangent_axis.x *. z);
              y_out.(at) <- (radial_axis.y *. x) +. (pole_axis.y *. y)
                  +. (tangent_axis.y *. z);
              z_out.(at) <- (radial_axis.z *. x) +. (pole_axis.z *. y)
                  +. (tangent_axis.z *. z)
            end in
          let[@inline always] write_smooth_normal nx ny nz at row column =
            write_direction nx ny nz at
              (v_cosine.(column) *. u_cosine.(row))
              v_sine.(column)
              (v_cosine.(column) *. u_sine.(row)) in
          let[@inline always] write_point point row column =
            let ring_radius = major_radius_scaled
                +. (minor_radius_scaled *. v_cosine.(column)) in
            let local_x = ring_radius *. u_cosine.(row)
            and local_y = minor_radius_scaled *. v_sine.(column)
            and local_z = ring_radius *. u_sine.(row) in
            let x, y, z = if identity_axes then
                center.x +. local_x, center.y +. local_y, center.z +. local_z
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
               | Some (nx, ny, nz) ->
                   write_smooth_normal nx ny nz point row column
               | None -> ());
              (match point_uv with
               | Some (u, v) ->
                   u.(point) <- u_parameter.(row);
                   v.(point) <- v_parameter.(column)
               | None -> ());
              true
            end else false in
          let row_grain = max 1 (grain / columns) in
          let range_count = ((rows - 1) / row_grain) + 1 in
          let errors = Array.make range_count (-1) in
          Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(range_count - 1)
            (fun range ->
              let first_row = range * row_grain
              and last_row = min rows ((range + 1) * row_grain) in
              for row = first_row to last_row - 1 do
                let at = row * columns in
                for column = 0 to columns - 1 do
                  let point = at + column in
                  if point land 4095 = 0 then Cancel.check_opt cancel;
                  if not (write_point point row column) && errors.(range) < 0 then
                    errors.(range) <- point
                done
              done);
          let invalid = Array.fold_left (fun first point ->
              if point < 0 then first else if first < 0 || point < first
              then point else first) (-1) errors in
          if invalid >= 0 then Error (Printf.sprintf
              "Rdk.Parametric_generators.torus: generated point %d is not finite" invalid)
          else
            let vertex_normals = match normal_mode with
              | Torus_vertex_normals -> Some (Array.make vertex_count 0.,
                  Array.make vertex_count 0., Array.make vertex_count 0.)
              | Torus_no_normals | Torus_point_normals -> None in
            let vertex_uv = match uv_attribute, point_mode with
              | Some _, false -> Some (Array.make vertex_count 0.,
                  Array.make vertex_count 0.)
              | _ -> None in
            let[@inline always] point_of row column =
              let row = if row = rows then 0 else row
              and column = if column = columns then 0 else column in
              (row * columns) + column in
            let[@inline always] write_surface_vertex vertex point row column =
              (match vertex_normals with
               | Some (nx, ny, nz) ->
                   write_smooth_normal nx ny nz vertex
                     (if row = rows then 0 else row)
                     (if column = columns then 0 else column)
               | None -> ());
              (match vertex_uv with
               | Some (u, v) ->
                   u.(vertex) <- if row = rows then 1. else u_parameter.(row);
                   v.(vertex) <- if column = columns then 1.
                     else v_parameter.(column)
               | None -> ());
              point in
            let topology = match connectivity with
              | Torus_points -> Topology.empty ~point_count
              | Torus_rows | Torus_columns | Torus_rows_and_columns ->
                  let vertex_points = Array.make vertex_count 0
                  and primitive_offsets = Array.make (primitive_count + 1) 0
                  and primitive_kinds = Bytes.make primitive_count '\001' in
                  let row_primitive_count = match connectivity with
                    | Torus_rows | Torus_rows_and_columns -> columns
                    | _ -> 0 in
                  let row_vertex_count = row_primitive_count * rows in
                  Parallel.for_ ~chunk_size:(max 1 (grain / max rows columns))
                    ~start:0 ~finish:(primitive_count - 1) (fun primitive ->
                      if primitive land 1023 = 0 then Cancel.check_opt cancel;
                      if primitive < row_primitive_count then begin
                        let column = primitive and at = primitive * rows in
                        Bytes.set primitive_kinds primitive
                          (if u_wrap then '\002' else '\001');
                        for row = 0 to rows - 1 do
                          let vertex = at + row in
                          vertex_points.(vertex) <- write_surface_vertex vertex
                              (point_of row column) row column
                        done
                      end else begin
                        let row = primitive - row_primitive_count in
                        let at = row_vertex_count + (row * columns) in
                        Bytes.set primitive_kinds primitive
                          (if v_wrap then '\002' else '\001');
                        for column = 0 to columns - 1 do
                          let vertex = at + column in
                          vertex_points.(vertex) <- write_surface_vertex vertex
                              (point_of row column) row column
                        done
                      end);
                  Parallel.for_ ~chunk_size:grain ~start:0 ~finish:primitive_count
                    (fun primitive ->
                      if primitive land 4095 = 0 then Cancel.check_opt cancel;
                      primitive_offsets.(primitive) <-
                        if primitive <= row_primitive_count
                        then primitive * rows
                        else row_vertex_count
                          + ((primitive - row_primitive_count) * columns));
                  Topology.Private.create_validated_owned ~point_count
                    ~vertex_points ~primitive_offsets ~primitive_kinds
              | Torus_triangles | Torus_alternating_triangles | Torus_quads ->
                  let triangle_mode = connectivity <> Torus_quads in
                  let vertices_per_cell = if triangle_mode then 6 else 4 in
                  let surface_vertex_count = surface_cells * vertices_per_cell in
                  let vertex_points = Array.make vertex_count 0
                  and primitive_offsets = Array.make (primitive_count + 1) 0
                  and primitive_kinds = Bytes.make primitive_count '\000' in
                  let[@inline always] set_surface vertex point row column =
                    vertex_points.(vertex) <- write_surface_vertex vertex point
                        row column in
                  Parallel.for_ ~chunk_size:(max 1 (grain / vertices_per_cell))
                    ~start:0 ~finish:(surface_cells - 1) (fun cell ->
                      if cell land 4095 = 0 then Cancel.check_opt cancel;
                      let row = cell / v_cells and column = cell mod v_cells in
                      let next_row = row + 1 and next_column = column + 1 in
                      let a = point_of row column
                      and b = point_of next_row column
                      and c = point_of next_row next_column
                      and d = point_of row next_column in
                      let at = cell * vertices_per_cell in
                      if not triangle_mode then begin
                        if not reverse_surface then begin
                          set_surface at a row column;
                          set_surface (at + 1) d row next_column;
                          set_surface (at + 2) c next_row next_column;
                          set_surface (at + 3) b next_row column
                        end else begin
                          set_surface at a row column;
                          set_surface (at + 1) b next_row column;
                          set_surface (at + 2) c next_row next_column;
                          set_surface (at + 3) d row next_column
                        end
                      end else
                        let alternate = connectivity = Torus_alternating_triangles
                            && ((row + column) land 1 = 1) in
                        if not alternate && not reverse_surface then begin
                          set_surface at a row column;
                          set_surface (at + 1) d row next_column;
                          set_surface (at + 2) c next_row next_column;
                          set_surface (at + 3) a row column;
                          set_surface (at + 4) c next_row next_column;
                          set_surface (at + 5) b next_row column
                        end else if not alternate then begin
                          set_surface at a row column;
                          set_surface (at + 1) c next_row next_column;
                          set_surface (at + 2) d row next_column;
                          set_surface (at + 3) a row column;
                          set_surface (at + 4) b next_row column;
                          set_surface (at + 5) c next_row next_column
                        end else if not reverse_surface then begin
                          set_surface at a row column;
                          set_surface (at + 1) d row next_column;
                          set_surface (at + 2) b next_row column;
                          set_surface (at + 3) d row next_column;
                          set_surface (at + 4) c next_row next_column;
                          set_surface (at + 5) b next_row column
                        end else begin
                          set_surface at a row column;
                          set_surface (at + 1) b next_row column;
                          set_surface (at + 2) d row next_column;
                          set_surface (at + 3) d row next_column;
                          set_surface (at + 4) b next_row column;
                          set_surface (at + 5) c next_row next_column
                        end);
                  let v_cap_vertex_base = surface_vertex_count in
                  let[@inline always] write_v_cap_vertex vertex point row side =
                    vertex_points.(vertex) <- point;
                    (match vertex_normals with
                     | Some (nx, ny, nz) ->
                         let row = if row = rows then 0 else row in
                         let x = u_direction *. -.v_chord_y *. u_cosine.(row)
                         and y = u_direction *. v_chord_x
                         and z = u_direction *. -.v_chord_y *. u_sine.(row) in
                         let scale = Float.max (abs_float x)
                             (Float.max (abs_float y) (abs_float z)) in
                         let x = x /. scale and y = y /. scale
                         and z = z /. scale in
                         let length = sqrt ((x *. x) +. (y *. y) +. (z *. z)) in
                         write_direction nx ny nz vertex
                           (x /. length) (y /. length) (z /. length)
                     | None -> ());
                    match vertex_uv with
                    | Some (u, v) ->
                        u.(vertex) <- if row = rows then 1.
                          else u_parameter.(row);
                        v.(vertex) <- float_of_int side
                    | None -> () in
                  if v_end_cap then
                    Parallel.for_
                      ~chunk_size:(max 1 (grain / vertices_per_cell))
                      ~start:0 ~finish:(u_cells - 1) (fun row ->
                        if row land 4095 = 0 then Cancel.check_opt cancel;
                        let next_row = row + 1 in
                        let a = point_of row 0 and b = point_of next_row 0
                        and c = point_of next_row (columns - 1)
                        and d = point_of row (columns - 1) in
                        let at = v_cap_vertex_base + (row * vertices_per_cell) in
                        if not triangle_mode then begin
                          write_v_cap_vertex at a row 0;
                          write_v_cap_vertex (at + 1) b next_row 0;
                          write_v_cap_vertex (at + 2) c next_row 1;
                          write_v_cap_vertex (at + 3) d row 1
                        end else
                          let alternate = connectivity
                              = Torus_alternating_triangles && (row land 1 = 1) in
                          if not alternate then begin
                            write_v_cap_vertex at a row 0;
                            write_v_cap_vertex (at + 1) b next_row 0;
                            write_v_cap_vertex (at + 2) c next_row 1;
                            write_v_cap_vertex (at + 3) a row 0;
                            write_v_cap_vertex (at + 4) c next_row 1;
                            write_v_cap_vertex (at + 5) d row 1
                          end else begin
                            write_v_cap_vertex at a row 0;
                            write_v_cap_vertex (at + 1) b next_row 0;
                            write_v_cap_vertex (at + 2) d row 1;
                            write_v_cap_vertex (at + 3) b next_row 0;
                            write_v_cap_vertex (at + 4) c next_row 1;
                            write_v_cap_vertex (at + 5) d row 1
                          end);
                  let[@inline always] write_u_cap_vertex vertex point start column =
                    vertex_points.(vertex) <- point;
                    (match vertex_normals with
                     | Some (nx, ny, nz) ->
                         let row = if start then 0 else rows - 1 in
                         let sign = u_direction
                             *. if start then -1. else 1. in
                         write_direction nx ny nz vertex
                           (sign *. -.u_sine.(row)) 0.
                           (sign *. u_cosine.(row))
                     | None -> ());
                    match vertex_uv with
                    | Some (u, v) ->
                        u.(vertex) <- 0.5 +. (0.5 *. v_cosine.(column));
                        v.(vertex) <- 0.5 +. (0.5 *. v_sine.(column))
                    | None -> () in
                  if u_end_caps then begin
                    let start_base = fixed_vertex_count
                    and end_base = fixed_vertex_count + columns in
                    Parallel.for_ ~chunk_size:grain ~start:0
                      ~finish:((2 * columns) - 1) (fun local ->
                        if local land 4095 = 0 then Cancel.check_opt cancel;
                        if local < columns then begin
                          let column = if reverse_surface then local
                            else columns - 1 - local in
                          write_u_cap_vertex (start_base + local)
                            (point_of 0 column) true column
                        end else begin
                          let index = local - columns in
                          let column = if reverse_surface
                            then columns - 1 - index else index in
                          write_u_cap_vertex (end_base + index)
                            (point_of (rows - 1) column) false column
                        end)
                  end;
                  Parallel.for_ ~chunk_size:grain ~start:0
                    ~finish:fixed_primitive_count (fun primitive ->
                      if primitive land 4095 = 0 then Cancel.check_opt cancel;
                      primitive_offsets.(primitive) <- primitive
                          * (if triangle_mode then 3 else 4));
                  if u_end_caps then begin
                    primitive_offsets.(fixed_primitive_count + 1) <-
                      fixed_vertex_count + columns;
                    primitive_offsets.(fixed_primitive_count + 2) <-
                      fixed_vertex_count + (2 * columns)
                  end;
                  Topology.Private.create_validated_owned ~point_count
                    ~vertex_points ~primitive_offsets ~primitive_kinds in
            let positions = Packed.Float3.Private.of_owned_exn ~x:px ~y:py ~z:pz in
            let attributes = ref [] in
            (match point_normals with
             | Some (x, y, z) ->
                 let values = Packed.Float3.Private.of_owned_exn ~x ~y ~z in
                 attributes := (Attribute.create_key_owned
                     (Attribute.normal ~owner:Attribute.Point) values |> Support.get_ok)
                   :: !attributes
             | None -> ());
            (match vertex_normals with
             | Some (x, y, z) ->
                 let values = Packed.Float3.Private.of_owned_exn ~x ~y ~z in
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
                 attributes := (Attribute.create_owned ~name
                     ~owner:Attribute.Vertex (Attribute.Float2 values) |> Support.get_ok)
                   :: !attributes
             | None, None, None -> ()
             | _ -> assert false);
            Geometry.create ~positions ~topology ~attributes:(List.rev !attributes)
              ())
