open Rdk_core
open Rays_math

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
type tube_connectivity =
  | Tube_triangles
  | Tube_alternating_triangles
  | Tube_quads
  | Tube_rows
  | Tube_columns
  | Tube_rows_and_columns
  | Tube_points
type tube_normals =
  | Tube_no_normals | Tube_point_normals | Tube_vertex_normals
type tube_orientation =
  | Tube_x | Tube_y | Tube_z | Tube_axis of Rays_math.Vec3.t
type tube_rotation_order =
  | Tube_xyz | Tube_xzy | Tube_yxz
  | Tube_yzx | Tube_zxy | Tube_zyx
type platonic_kind =
  | Platonic_tetrahedron
  | Platonic_cube
  | Platonic_octahedron
  | Platonic_icosahedron
  | Platonic_dodecahedron
  | Platonic_soccer_ball
type platonic_normals =
  | Platonic_no_normals | Platonic_point_normals | Platonic_vertex_normals
type platonic_orientation =
  | Platonic_x | Platonic_y | Platonic_z | Platonic_axis of Rays_math.Vec3.t
type platonic_rotation_order =
  | Platonic_xyz | Platonic_xzy | Platonic_yxz
  | Platonic_yzx | Platonic_zxy | Platonic_zyx

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

let tube_rotation_matrix order rotation =
  let x = Rays_math.Mat4.rotation_x rotation.Rays_math.Vec3.x
  and y = Rays_math.Mat4.rotation_y rotation.y
  and z = Rays_math.Mat4.rotation_z rotation.z in
  match order with
  | Tube_xyz -> Rays_math.Mat4.mul z (Rays_math.Mat4.mul y x)
  | Tube_xzy -> Rays_math.Mat4.mul y (Rays_math.Mat4.mul z x)
  | Tube_yxz -> Rays_math.Mat4.mul z (Rays_math.Mat4.mul x y)
  | Tube_yzx -> Rays_math.Mat4.mul x (Rays_math.Mat4.mul z y)
  | Tube_zxy -> Rays_math.Mat4.mul y (Rays_math.Mat4.mul x z)
  | Tube_zyx -> Rays_math.Mat4.mul x (Rays_math.Mat4.mul y z)

let tube_frame orientation rotation_order (rotation : Rays_math.Vec3.t) =
  let oriented = match orientation with
    | Tube_x -> Ok (Rays_math.Vec3.unit_y, Rays_math.Vec3.unit_x, Rays_math.Vec3.create 0. 0. (-1.))
    | Tube_y -> Ok (Rays_math.Vec3.unit_x, Rays_math.Vec3.unit_y, Rays_math.Vec3.unit_z)
    | Tube_z -> Ok (Rays_math.Vec3.unit_x, Rays_math.Vec3.unit_z, Rays_math.Vec3.create 0. (-1.) 0.)
    | Tube_axis axis ->
        Result.bind (normalize_plane_axis "Rdk.Parametric_generators.tube" "primary" axis)
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
            Result.bind (normalize_plane_axis "Rdk.Parametric_generators.tube" "radial" radial)
              (fun radial ->
                Result.map (fun tangent -> radial, pole, tangent)
                  (normalize_plane_axis "Rdk.Parametric_generators.tube" "tangent"
                     (Rays_math.Vec3.cross radial pole)))) in
  Result.map (fun (radial, pole, tangent) ->
    if rotation.x = 0. && rotation.y = 0. && rotation.z = 0. then
      radial, pole, tangent
    else
      let matrix = tube_rotation_matrix rotation_order rotation in
      Rays_math.Mat4.transform_direction matrix radial,
      Rays_math.Mat4.transform_direction matrix pole,
      Rays_math.Mat4.transform_direction matrix tangent) oriented

let tube ?cancel ?(grain = 16_384) ?(connectivity = Tube_quads)
    ?(end_caps = false) ?(consolidate_cap_points = true) ?normals
    ?(orientation = Tube_y) ?(center = Rays_math.Vec3.zero) ?(rotation = Rays_math.Vec3.zero)
    ?(rotation_order = Tube_xyz) ?(radius_scale = 1.) ?uv_attribute ?cap_group
    ?(rows = 2) ?(columns = 32) ~top_radius ~bottom_radius ~height () =
  Error.guard ~operation:"tube" ~code:"invalid_parameter" @@ fun () ->
  let point_mode = connectivity = Tube_points in
  let polygon_mode = match connectivity with
    | Tube_triangles | Tube_alternating_triangles | Tube_quads -> true
    | Tube_rows | Tube_columns | Tube_rows_and_columns | Tube_points -> false in
  let normal_mode = Option.value ~default:Tube_point_normals normals in
  let top_radius_scaled = top_radius *. radius_scale
  and bottom_radius_scaled = bottom_radius *. radius_scale in
  if grain <= 0 then Error "Rdk.Parametric_generators.tube: grain must be positive"
  else if rows < 2 then Error "Rdk.Parametric_generators.tube: rows must be at least two"
  else if columns < 3 then Error "Rdk.Parametric_generators.tube: columns must be at least three"
  else if not (Float.is_finite top_radius && top_radius >= 0.
      && Float.is_finite bottom_radius && bottom_radius >= 0.
      && Float.is_finite radius_scale && radius_scale > 0.
      && Float.is_finite top_radius_scaled && top_radius_scaled >= 0.
      && Float.is_finite bottom_radius_scaled && bottom_radius_scaled >= 0.
      && (top_radius_scaled > 0. || bottom_radius_scaled > 0.)) then
    Error "Rdk.Parametric_generators.tube: radii must be finite/non-negative, at least one positive, and radius scale positive"
  else if not (Float.is_finite height && height > 0.) then
    Error "Rdk.Parametric_generators.tube: height must be finite and positive"
  else if not (Float.is_finite center.x && Float.is_finite center.y && Float.is_finite center.z
      && Float.is_finite rotation.x && Float.is_finite rotation.y && Float.is_finite rotation.z) then
    Error "Rdk.Parametric_generators.tube: center and rotation must be finite"
  else if (match uv_attribute with
      | Some name -> String.trim name = "" || String.equal name "P"
          || String.equal name "N"
      | None -> false) then
    Error "Rdk.Parametric_generators.tube: UV attribute name must be non-empty and cannot be P or N"
  else if (match cap_group with Some name -> String.trim name = "" | None -> false)
  then Error "Rdk.Parametric_generators.tube: cap group name must be non-empty"
  else if point_mode && normal_mode = Tube_vertex_normals then
    Error "Rdk.Parametric_generators.tube: point output cannot carry vertex normals"
  else if end_caps && not polygon_mode then
    Error "Rdk.Parametric_generators.tube: end caps require triangle or quad connectivity"
  else if Option.is_some cap_group && not end_caps then
    Error "Rdk.Parametric_generators.tube: cap group requires end caps"
  else Result.bind (tube_frame orientation rotation_order rotation)
      (fun (radial_axis, pole_axis, tangent_axis) ->
      let point_limit = Sys.max_array_length
      and primitive_limit = min (Sys.max_array_length - 1) Sys.max_string_length in
      let checked_mul left right limit =
        if left = 0 || right <= limit / left then Some (left * right) else None
      and checked_add left right limit =
        if right <= limit - left then Some (left + right) else None in
      let bottom_tip = bottom_radius_scaled = 0.
      and top_tip = top_radius_scaled = 0. in
      let tip_count = (if bottom_tip then 1 else 0) + if top_tip then 1 else 0 in
      let cap_count = if not end_caps then 0 else
          (if bottom_tip then 0 else 1) + if top_tip then 0 else 1 in
      let ring_points = match checked_mul rows columns point_limit with
        | Some value -> Some (value - (tip_count * (columns - 1)))
        | None -> None in
      let duplicate_cap_points = if end_caps && not consolidate_cap_points
        then cap_count * columns else 0 in
      let point_count = Option.bind ring_points (fun count ->
          checked_add count duplicate_cap_points point_limit) in
      let bands = rows - 1 in
      let ordinary_bands = bands - tip_count in
      let ring_curve_count = rows - tip_count in
      let topology_cardinality = match point_count with
        | None -> None
        | Some _ ->
            (match connectivity with
             | Tube_points -> Some (0, 0, 0, 0)
             | Tube_rows ->
                 Option.map (fun vertices -> vertices, columns, 0, 0)
                   (checked_mul rows columns point_limit)
             | Tube_columns ->
                 Option.map (fun vertices -> vertices, ring_curve_count, 0, 0)
                   (checked_mul ring_curve_count columns point_limit)
             | Tube_rows_and_columns ->
                 Option.bind (checked_mul rows columns point_limit)
                   (fun row_vertices ->
                     Option.bind (checked_mul ring_curve_count columns point_limit)
                       (fun column_vertices ->
                         Option.bind (checked_add row_vertices column_vertices
                             point_limit) (fun vertices ->
                           Option.map (fun primitives -> vertices, primitives, 0, 0)
                             (checked_add columns ring_curve_count
                                primitive_limit))))
             | Tube_triangles | Tube_alternating_triangles | Tube_quads ->
                 let triangle_mode = connectivity <> Tube_quads in
                 let ordinary_primitives_per_cell = if triangle_mode then 2 else 1
                 and ordinary_vertices_per_cell = if triangle_mode then 6 else 4 in
                 (match checked_mul ordinary_bands columns primitive_limit with
                  | None -> None
                  | Some ordinary_cells ->
                      let ordinary_primitives = checked_mul ordinary_cells
                          ordinary_primitives_per_cell primitive_limit
                      and tip_primitives = checked_mul tip_count columns
                          primitive_limit
                      and ordinary_vertices = checked_mul ordinary_cells
                          ordinary_vertices_per_cell point_limit
                      and tip_vertices = checked_mul (tip_count * columns) 3
                          point_limit in
                      (match ordinary_primitives, tip_primitives,
                          ordinary_vertices, tip_vertices with
                       | Some ordinary_primitives, Some tip_primitives,
                         Some ordinary_vertices, Some tip_vertices ->
                           Option.bind (checked_add ordinary_primitives
                               tip_primitives primitive_limit)
                             (fun side_primitives ->
                               Option.bind (checked_add ordinary_vertices
                                   tip_vertices point_limit)
                                 (fun side_vertices ->
                                   Option.bind (checked_add side_primitives
                                       cap_count primitive_limit)
                                     (fun primitives ->
                                       Option.map (fun vertices ->
                                           vertices, primitives, side_vertices,
                                           side_primitives)
                                         (checked_add side_vertices
                                            (cap_count * columns)
                                            point_limit))))
                       | _ -> None))) in
      match ring_points, point_count, topology_cardinality with
      | None, _, _ | _, None, _ | _, _, None ->
          Error "Rdk.Parametric_generators.tube: output cardinality exceeds OCaml array limits"
      | Some side_point_count, Some point_count,
        Some (vertex_count, primitive_count, side_vertex_count,
          side_primitive_count) ->
          let ring_offsets = Array.make (rows + 1) 0 in
          for row = 0 to rows - 1 do
            let tip = row = 0 && bottom_tip || row = rows - 1 && top_tip in
            ring_offsets.(row + 1) <- ring_offsets.(row)
                + if tip then 1 else columns
          done;
          let bottom_cap_point_base = if end_caps && not consolidate_cap_points
              && not bottom_tip then side_point_count else -1 in
          let top_cap_point_base = if end_caps && not consolidate_cap_points
              && not top_tip then side_point_count
                + if bottom_cap_point_base >= 0 then columns else 0
            else -1 in
          let sine = Array.make columns 0. and cosine = Array.make columns 0.
          and u_parameter = Array.make columns 0.
          and row_radius = Array.make rows 0. and row_height = Array.make rows 0.
          and v_parameter = Array.make rows 0. in
          Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(columns - 1)
            (fun column ->
              if column land 4095 = 0 then Cancel.check_opt cancel;
              let angle = 2. *. Float.pi *. float_of_int column
                  /. float_of_int columns in
              sine.(column) <- sin angle; cosine.(column) <- cos angle;
              u_parameter.(column) <- float_of_int column /. float_of_int columns);
          Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(rows - 1)
            (fun row ->
              if row land 4095 = 0 then Cancel.check_opt cancel;
              let t = float_of_int row /. float_of_int (rows - 1) in
              row_radius.(row) <- if row = 0 then bottom_radius_scaled
                else if row = rows - 1 then top_radius_scaled
                else (bottom_radius_scaled *. (1. -. t))
                  +. (top_radius_scaled *. t);
              row_height.(row) <- height *. (t -. 0.5);
              v_parameter.(row) <- t);
          let normal_scale = Float.max height
              (abs_float (bottom_radius_scaled -. top_radius_scaled)) in
          let scaled_radial = height /. normal_scale
          and scaled_axial = (bottom_radius_scaled -. top_radius_scaled)
              /. normal_scale in
          let normal_length = sqrt ((scaled_radial *. scaled_radial)
              +. (scaled_axial *. scaled_axial)) in
          let side_radial = scaled_radial /. normal_length
          and side_axial = scaled_axial /. normal_length in
          let identity_axes = orientation = Tube_y
              && rotation.x = 0. && rotation.y = 0. && rotation.z = 0. in
          let px = Array.make point_count 0. and py = Array.make point_count 0.
          and pz = Array.make point_count 0. in
          let point_normals = match normal_mode with
            | Tube_point_normals -> Some (Array.make point_count 0.,
                Array.make point_count 0., Array.make point_count 0.)
            | Tube_no_normals | Tube_vertex_normals -> None in
          let point_uv = match uv_attribute, point_mode with
            | Some _, true -> Some (Array.make point_count 0.,
                Array.make point_count 0.)
            | _ -> None in
          let[@inline always] point_of row column =
            let tip = row = 0 && bottom_tip || row = rows - 1 && top_tip in
            ring_offsets.(row) + if tip then 0
              else if column = columns then 0 else column in
          let[@inline always] write_point_normal nx ny nz point row column =
            if row = 0 && bottom_tip then begin
              nx.(point) <- -.pole_axis.x; ny.(point) <- -.pole_axis.y;
              nz.(point) <- -.pole_axis.z
            end else if row = rows - 1 && top_tip then begin
              nx.(point) <- pole_axis.x; ny.(point) <- pole_axis.y;
              nz.(point) <- pole_axis.z
            end else
              let local_x = side_radial *. cosine.(column)
              and local_y = side_axial
              and local_z = side_radial *. sine.(column) in
              if identity_axes then begin
                nx.(point) <- local_x; ny.(point) <- local_y;
                nz.(point) <- local_z
              end else begin
                nx.(point) <- (radial_axis.x *. local_x)
                    +. (pole_axis.x *. local_y) +. (tangent_axis.x *. local_z);
                ny.(point) <- (radial_axis.y *. local_x)
                    +. (pole_axis.y *. local_y) +. (tangent_axis.y *. local_z);
                nz.(point) <- (radial_axis.z *. local_x)
                    +. (pole_axis.z *. local_y) +. (tangent_axis.z *. local_z)
              end in
          let[@inline always] write_side_point point row column =
            let radius = row_radius.(row) and local_y = row_height.(row) in
            let local_x = radius *. cosine.(column)
            and local_z = radius *. sine.(column) in
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
                   write_point_normal nx ny nz point row column
               | None -> ());
              (match point_uv with
               | Some (u, v) ->
                   u.(point) <- if row = 0 && bottom_tip
                       || row = rows - 1 && top_tip then 0.5
                     else u_parameter.(column);
                   v.(point) <- v_parameter.(row)
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
                let tip = row = 0 && bottom_tip || row = rows - 1 && top_tip in
                let count = if tip then 1 else columns in
                for column = 0 to count - 1 do
                  let point = ring_offsets.(row) + column in
                  if point land 4095 = 0 then Cancel.check_opt cancel;
                  if not (write_side_point point row column)
                      && errors.(range) < 0 then errors.(range) <- point
                done
              done);
          let invalid = Array.fold_left (fun first point ->
              if point < 0 then first else if first < 0 || point < first
              then point else first) (-1) errors in
          if invalid >= 0 then Error (Printf.sprintf
              "Rdk.Parametric_generators.tube: generated point %d is not finite" invalid)
          else begin
            let copy_cap_points source base sign =
              if base >= 0 then begin
                Array.blit px source px base columns;
                Array.blit py source py base columns;
                Array.blit pz source pz base columns;
                match point_normals with
                | Some (nx, ny, nz) ->
                    for column = 0 to columns - 1 do
                      let point = base + column in
                      nx.(point) <- sign *. pole_axis.x;
                      ny.(point) <- sign *. pole_axis.y;
                      nz.(point) <- sign *. pole_axis.z
                    done
                | None -> ()
              end in
            copy_cap_points ring_offsets.(0) bottom_cap_point_base (-1.);
            copy_cap_points ring_offsets.(rows - 1) top_cap_point_base 1.;
            let vertex_normals = match normal_mode with
              | Tube_vertex_normals -> Some (Array.make vertex_count 0.,
                  Array.make vertex_count 0., Array.make vertex_count 0.)
              | Tube_no_normals | Tube_point_normals -> None in
            let vertex_uv = match uv_attribute, point_mode with
              | Some _, false -> Some (Array.make vertex_count 0.,
                  Array.make vertex_count 0.)
              | _ -> None in
            let[@inline always] write_side_vertex vertex point row column =
              (match vertex_normals with
               | Some (nx, ny, nz) ->
                   let column = if column = columns then 0 else column in
                   let local_x = side_radial *. cosine.(column)
                   and local_y = side_axial
                   and local_z = side_radial *. sine.(column) in
                   if identity_axes then begin
                     nx.(vertex) <- local_x; ny.(vertex) <- local_y;
                     nz.(vertex) <- local_z
                   end else begin
                     nx.(vertex) <- (radial_axis.x *. local_x)
                         +. (pole_axis.x *. local_y)
                         +. (tangent_axis.x *. local_z);
                     ny.(vertex) <- (radial_axis.y *. local_x)
                         +. (pole_axis.y *. local_y)
                         +. (tangent_axis.y *. local_z);
                     nz.(vertex) <- (radial_axis.z *. local_x)
                         +. (pole_axis.z *. local_y)
                         +. (tangent_axis.z *. local_z)
                   end
               | None -> ());
              (match vertex_uv with
               | Some (u, v) ->
                   u.(vertex) <- if column = columns then 1.
                     else u_parameter.(column);
                   v.(vertex) <- v_parameter.(row)
               | None -> ());
              point in
            let topology = match connectivity with
              | Tube_points -> Topology.empty ~point_count
              | Tube_rows | Tube_columns | Tube_rows_and_columns ->
                  let vertex_points = Array.make vertex_count 0
                  and primitive_offsets = Array.make (primitive_count + 1) 0
                  and primitive_kinds = Bytes.make primitive_count '\001' in
                  let row_primitive_count = match connectivity with
                    | Tube_rows | Tube_rows_and_columns -> columns
                    | _ -> 0 in
                  let row_vertex_count = row_primitive_count * rows in
                  Parallel.for_ ~chunk_size:(max 1 (grain / max rows columns))
                    ~start:0 ~finish:(primitive_count - 1) (fun primitive ->
                      if primitive land 1023 = 0 then Cancel.check_opt cancel;
                      if primitive < row_primitive_count then begin
                        let column = primitive and at = primitive * rows in
                        for row = 0 to rows - 1 do
                          let vertex = at + row in
                          vertex_points.(vertex) <- write_side_vertex vertex
                              (point_of row column) row column
                        done
                      end else begin
                        let curve = primitive - row_primitive_count in
                        let row = curve + if bottom_tip then 1 else 0 in
                        let at = row_vertex_count + (curve * columns) in
                        Bytes.set primitive_kinds primitive '\002';
                        for column = 0 to columns - 1 do
                          let vertex = at + column in
                          vertex_points.(vertex) <- write_side_vertex vertex
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
              | Tube_triangles | Tube_alternating_triangles | Tube_quads ->
                  let triangle_mode = connectivity <> Tube_quads in
                  let band_primitive_offsets = Array.make (bands + 1) 0
                  and band_vertex_offsets = Array.make (bands + 1) 0 in
                  for band = 0 to bands - 1 do
                    let tip_band = band = 0 && bottom_tip
                        || band = bands - 1 && top_tip in
                    band_primitive_offsets.(band + 1) <-
                      band_primitive_offsets.(band)
                        + (columns * if tip_band || not triangle_mode then 1 else 2);
                    band_vertex_offsets.(band + 1) <-
                      band_vertex_offsets.(band)
                        + (columns * if tip_band then 3
                           else if triangle_mode then 6 else 4)
                  done;
                  let vertex_points = Array.make vertex_count 0
                  and primitive_offsets = Array.make (primitive_count + 1) 0
                  and primitive_kinds = Bytes.make primitive_count '\000' in
                  let[@inline always] set_side vertex point row radial =
                    vertex_points.(vertex) <- write_side_vertex vertex point
                        row radial in
                  Parallel.for_ ~chunk_size:(max 1 (grain / columns)) ~start:0
                    ~finish:(bands - 1) (fun band ->
                      Cancel.check_opt cancel;
                      let bottom_band = band = 0 && bottom_tip
                      and top_band = band = bands - 1 && top_tip in
                      let tip_band = bottom_band || top_band in
                      for column = 0 to columns - 1 do
                      if column land 4095 = 0 then Cancel.check_opt cancel;
                      let next_column = column + 1 in
                      let primitive = band_primitive_offsets.(band)
                          + (column * if tip_band || not triangle_mode then 1 else 2)
                      and at = band_vertex_offsets.(band)
                          + (column * if tip_band then 3
                             else if triangle_mode then 6 else 4) in
                      let a = point_of band column
                      and b = point_of (band + 1) column
                      and c = point_of (band + 1) next_column
                      and d = point_of band next_column in
                      if bottom_band then begin
                        primitive_offsets.(primitive) <- at;
                        set_side at a band column;
                        set_side (at + 1) b (band + 1) column;
                        set_side (at + 2) c (band + 1) next_column
                      end else if top_band then begin
                        primitive_offsets.(primitive) <- at;
                        set_side at a band column;
                        set_side (at + 1) b (band + 1) column;
                        set_side (at + 2) d band next_column
                      end else if not triangle_mode then begin
                        primitive_offsets.(primitive) <- at;
                        set_side at a band column;
                        set_side (at + 1) b (band + 1) column;
                        set_side (at + 2) c (band + 1) next_column;
                        set_side (at + 3) d band next_column
                      end else
                        let alternate = connectivity = Tube_alternating_triangles
                            && ((band + column) land 1 = 1) in
                        primitive_offsets.(primitive) <- at;
                        primitive_offsets.(primitive + 1) <- at + 3;
                        if not alternate then begin
                          set_side at a band column;
                          set_side (at + 1) b (band + 1) column;
                          set_side (at + 2) c (band + 1) next_column;
                          set_side (at + 3) a band column;
                          set_side (at + 4) c (band + 1) next_column;
                          set_side (at + 5) d band next_column
                        end else begin
                          set_side at a band column;
                          set_side (at + 1) b (band + 1) column;
                          set_side (at + 2) d band next_column;
                          set_side (at + 3) b (band + 1) column;
                          set_side (at + 4) c (band + 1) next_column;
                          set_side (at + 5) d band next_column
                        end
                      done);
                  primitive_offsets.(side_primitive_count) <- side_vertex_count;
                  let cap_vertex_base = side_vertex_count in
                  let write_cap_vertex vertex point sign column =
                    vertex_points.(vertex) <- point;
                    (match vertex_normals with
                     | Some (nx, ny, nz) ->
                         nx.(vertex) <- sign *. pole_axis.x;
                         ny.(vertex) <- sign *. pole_axis.y;
                         nz.(vertex) <- sign *. pole_axis.z
                     | None -> ());
                    match vertex_uv with
                    | Some (u, v) ->
                        u.(vertex) <- 0.5 +. (0.5 *. cosine.(column));
                        v.(vertex) <- 0.5 +. (0.5 *. sine.(column))
                    | None -> () in
                  if cap_count > 0 then
                    Parallel.for_ ~chunk_size:grain ~start:0
                      ~finish:((cap_count * columns) - 1) (fun local ->
                        if local land 4095 = 0 then Cancel.check_opt cancel;
                        let cap = local / columns and index = local mod columns in
                        let bottom = not bottom_tip && (cap = 0) in
                        let column = if bottom then index else columns - 1 - index in
                        let base = if bottom then
                            if bottom_cap_point_base >= 0
                            then bottom_cap_point_base else ring_offsets.(0)
                          else if top_cap_point_base >= 0 then top_cap_point_base
                          else ring_offsets.(rows - 1) in
                        write_cap_vertex (cap_vertex_base + local)
                          (base + column) (if bottom then -1. else 1.) column);
                  for cap = 0 to cap_count do
                    primitive_offsets.(side_primitive_count + cap) <-
                      side_vertex_count + (cap * columns)
                  done;
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
            let groups = match cap_group with
              | None -> []
              | Some name ->
                  let builder = Group.Builder.create ~owner:Group.Primitive
                      ~name primitive_count in
                  for primitive = side_primitive_count to primitive_count - 1 do
                    Group.Builder.set builder primitive true
                  done;
                  [Group.Builder.freeze builder] in
            Geometry.create ~positions ~topology ~attributes:(List.rev !attributes)
              ~groups ()
          end)

type platonic_data = {
  platonic_x : float array;
  platonic_y : float array;
  platonic_z : float array;
  platonic_vertex_points : int array;
  platonic_primitive_offsets : int array;
  platonic_face_nx : float array;
  platonic_face_ny : float array;
  platonic_face_nz : float array;
  platonic_face_sizes : int array;
  platonic_soccer_pentagons : int;
}

let platonic_data ?(soccer_pentagons = 0) raw_points raw_faces =
  let point_count = Array.length raw_points in
  let x = Array.make point_count 0. and y = Array.make point_count 0.
  and z = Array.make point_count 0. in
  Array.iteri (fun point (px, py, pz) ->
    let length = sqrt ((px *. px) +. (py *. py) +. (pz *. pz)) in
    x.(point) <- px /. length; y.(point) <- py /. length;
    z.(point) <- pz /. length) raw_points;
  let primitive_count = Array.length raw_faces in
  let faces = Array.mapi (fun _primitive source ->
    let face = Array.copy source and count = Array.length source in
    let cx = ref 0. and cy = ref 0. and cz = ref 0. in
    Array.iter (fun point ->
      cx := !cx +. x.(point); cy := !cy +. y.(point);
      cz := !cz +. z.(point)) face;
    let a = face.(0) and b = face.(1) and c = face.(2) in
    let abx = x.(b) -. x.(a) and aby = y.(b) -. y.(a)
    and abz = z.(b) -. z.(a) and acx = x.(c) -. x.(a)
    and acy = y.(c) -. y.(a) and acz = z.(c) -. z.(a) in
    let nx = (aby *. acz) -. (abz *. acy)
    and ny = (abz *. acx) -. (abx *. acz)
    and nz = (abx *. acy) -. (aby *. acx) in
    if (nx *. !cx) +. (ny *. !cy) +. (nz *. !cz) < 0. then
      for left = 0 to (count / 2) - 1 do
        let right = count - 1 - left and value = face.(left) in
        face.(left) <- face.(right); face.(right) <- value
      done;
    face) raw_faces in
  let primitive_offsets = Array.make (primitive_count + 1) 0
  and face_sizes = Array.make primitive_count 0 in
  for primitive = 0 to primitive_count - 1 do
    let size = Array.length faces.(primitive) in
    face_sizes.(primitive) <- size;
    primitive_offsets.(primitive + 1) <- primitive_offsets.(primitive) + size
  done;
  let vertex_points = Array.make primitive_offsets.(primitive_count) 0
  and face_nx = Array.make primitive_count 0.
  and face_ny = Array.make primitive_count 0.
  and face_nz = Array.make primitive_count 0. in
  Array.iteri (fun primitive face ->
    Array.blit face 0 vertex_points primitive_offsets.(primitive)
      (Array.length face);
    let a = face.(0) and b = face.(1) and c = face.(2) in
    let abx = x.(b) -. x.(a) and aby = y.(b) -. y.(a)
    and abz = z.(b) -. z.(a) and acx = x.(c) -. x.(a)
    and acy = y.(c) -. y.(a) and acz = z.(c) -. z.(a) in
    let nx = (aby *. acz) -. (abz *. acy)
    and ny = (abz *. acx) -. (abx *. acz)
    and nz = (abx *. acy) -. (aby *. acx) in
    let length = sqrt ((nx *. nx) +. (ny *. ny) +. (nz *. nz)) in
    face_nx.(primitive) <- nx /. length;
    face_ny.(primitive) <- ny /. length;
    face_nz.(primitive) <- nz /. length) faces;
  { platonic_x = x; platonic_y = y; platonic_z = z;
    platonic_vertex_points = vertex_points;
    platonic_primitive_offsets = primitive_offsets;
    platonic_face_nx = face_nx; platonic_face_ny = face_ny;
    platonic_face_nz = face_nz; platonic_face_sizes = face_sizes;
    platonic_soccer_pentagons = soccer_pentagons }

let platonic_tetrahedron_data = platonic_data [|
    1., 1., 1.; -1., -1., 1.; -1., 1., -1.; 1., -1., -1.
  |] [|[|0;2;1|]; [|0;1;3|]; [|0;3;2|]; [|1;2;3|]|]

let platonic_cube_data = platonic_data [|
    -1., -1., -1.; 1., -1., -1.; 1., 1., -1.; -1., 1., -1.;
    -1., -1., 1.; 1., -1., 1.; 1., 1., 1.; -1., 1., 1.
  |] [|
    [|0;1;2;3|]; [|4;7;6;5|]; [|0;4;5;1|];
    [|1;5;6;2|]; [|2;6;7;3|]; [|3;7;4;0|]
  |]

let platonic_octahedron_data = platonic_data [|
    1., 0., 0.; -1., 0., 0.; 0., 1., 0.;
    0., -1., 0.; 0., 0., 1.; 0., 0., -1.
  |] [|
    [|0;2;4|]; [|4;2;1|]; [|1;2;5|]; [|5;2;0|];
    [|4;3;0|]; [|1;3;4|]; [|5;3;1|]; [|0;3;5|]
  |]

let platonic_icosahedron_points =
  let phi = (1. +. sqrt 5.) /. 2. in [|
    -1., phi, 0.; 1., phi, 0.; -1., -.phi, 0.; 1., -.phi, 0.;
    0., -1., phi; 0., 1., phi; 0., -1., -.phi; 0., 1., -.phi;
    phi, 0., -1.; phi, 0., 1.; -.phi, 0., -1.; -.phi, 0., 1.
  |]

let platonic_icosahedron_faces = [|
  [|0;11;5|]; [|0;5;1|]; [|0;1;7|]; [|0;7;10|]; [|0;10;11|];
  [|1;5;9|]; [|5;11;4|]; [|11;10;2|]; [|10;7;6|]; [|7;1;8|];
  [|3;9;4|]; [|3;4;2|]; [|3;2;6|]; [|3;6;8|]; [|3;8;9|];
  [|4;9;5|]; [|2;4;11|]; [|6;2;10|]; [|8;6;7|]; [|9;8;1|]
|]

let platonic_icosahedron_data =
  platonic_data platonic_icosahedron_points platonic_icosahedron_faces

let platonic_dodecahedron_data =
  let phi = (1. +. sqrt 5.) /. 2. and inv = 2. /. (1. +. sqrt 5.) in
  platonic_data [|
    1., 1., 1.; 1., 1., -1.; 1., -1., 1.; 1., -1., -1.;
    -1., 1., 1.; -1., 1., -1.; -1., -1., 1.; -1., -1., -1.;
    0., inv, phi; 0., inv, -.phi; 0., -.inv, phi; 0., -.inv, -.phi;
    inv, phi, 0.; inv, -.phi, 0.; -.inv, phi, 0.; -.inv, -.phi, 0.;
    phi, 0., inv; phi, 0., -.inv; -.phi, 0., inv; -.phi, 0., -.inv
  |] [|
    [|0;8;10;2;16|]; [|0;16;17;1;12|]; [|0;12;14;4;8|];
    [|8;4;18;6;10|]; [|10;6;15;13;2|]; [|2;13;3;17;16|];
    [|1;9;5;14;12|]; [|1;17;3;11;9|]; [|4;14;5;19;18|];
    [|6;18;19;7;15|]; [|3;13;15;7;11|]; [|5;9;11;7;19|]
  |]

let platonic_soccer_ball_data =
  let directed = Array.make (12 * 12) (-1)
  and points = Array.make 60 (0., 0., 0.) and count = ref 0 in
  let ensure a b =
    let key = (a * 12) + b in
    if directed.(key) < 0 then begin
      let ax, ay, az = platonic_icosahedron_points.(a)
      and bx, by, bz = platonic_icosahedron_points.(b) in
      directed.(key) <- !count;
      points.(!count) <- ((2. *. ax +. bx) /. 3.,
        (2. *. ay +. by) /. 3., (2. *. az +. bz) /. 3.);
      incr count
    end;
    directed.(key) in
  Array.iter (fun face ->
    let a = face.(0) and b = face.(1) and c = face.(2) in
    ignore (ensure a b); ignore (ensure b a);
    ignore (ensure b c); ignore (ensure c b);
    ignore (ensure c a); ignore (ensure a c)) platonic_icosahedron_faces;
  assert (!count = 60);
  let successor = Array.make (12 * 12) (-1) in
  Array.iter (fun face ->
    let a = face.(0) and b = face.(1) and c = face.(2) in
    successor.((a * 12) + b) <- c;
    successor.((b * 12) + c) <- a;
    successor.((c * 12) + a) <- b) platonic_icosahedron_faces;
  let faces = Array.init 32 (fun face ->
    if face < 12 then begin
      let start = ref (-1) in
      for neighbor = 0 to 11 do
        if !start < 0 && directed.((face * 12) + neighbor) >= 0 then
          start := neighbor
      done;
      let neighbor = ref !start in
      Array.init 5 (fun _ ->
        let point = directed.((face * 12) + !neighbor) in
        neighbor := successor.((face * 12) + !neighbor);
        point)
    end else begin
      let triangle = platonic_icosahedron_faces.(face - 12) in
      let a = triangle.(0) and b = triangle.(1) and c = triangle.(2) in
      [|ensure a b; ensure b a; ensure b c; ensure c b; ensure c a; ensure a c|]
    end) in
  platonic_data ~soccer_pentagons:12 points faces

let platonic ?cancel ?(kind = Platonic_tetrahedron)
    ?(normals = Platonic_point_normals) ?(orientation = Platonic_y)
    ?(center = Rays_math.Vec3.zero) ?(rotation = Rays_math.Vec3.zero)
    ?(rotation_order = Platonic_xyz) ?face_groups ~radius () =
  Error.guard ~operation:"platonic" ~code:"invalid_parameter" @@ fun () ->
  Cancel.check_opt cancel;
  if not (Float.is_finite radius && radius > 0.) then
    Error "Rdk.Parametric_generators.platonic: radius must be finite and positive"
  else if not (Float.is_finite center.x && Float.is_finite center.y && Float.is_finite center.z
      && Float.is_finite rotation.x && Float.is_finite rotation.y && Float.is_finite rotation.z) then
    Error "Rdk.Parametric_generators.platonic: center and rotation must be finite"
  else if (match face_groups with
      | Some name -> String.trim name = "" | None -> false) then
    Error "Rdk.Parametric_generators.platonic: face group prefix must be non-empty"
  else
    let tube_orientation = match orientation with
      | Platonic_x -> Tube_x | Platonic_y -> Tube_y | Platonic_z -> Tube_z
      | Platonic_axis axis -> Tube_axis axis in
    let tube_rotation_order = match rotation_order with
      | Platonic_xyz -> Tube_xyz | Platonic_xzy -> Tube_xzy
      | Platonic_yxz -> Tube_yxz | Platonic_yzx -> Tube_yzx
      | Platonic_zxy -> Tube_zxy | Platonic_zyx -> Tube_zyx in
    Result.bind (tube_frame tube_orientation tube_rotation_order rotation)
      (fun (x_axis, y_axis, z_axis) ->
      let data = match kind with
        | Platonic_tetrahedron -> platonic_tetrahedron_data
        | Platonic_cube -> platonic_cube_data
        | Platonic_octahedron -> platonic_octahedron_data
        | Platonic_icosahedron -> platonic_icosahedron_data
        | Platonic_dodecahedron -> platonic_dodecahedron_data
        | Platonic_soccer_ball -> platonic_soccer_ball_data in
      let point_count = Array.length data.platonic_x
      and vertex_count = Array.length data.platonic_vertex_points
      and primitive_count = Array.length data.platonic_face_sizes in
      let px = Array.make point_count 0. and py = Array.make point_count 0.
      and pz = Array.make point_count 0. in
      let point_normal = match normals with
        | Platonic_point_normals -> Some (Array.make point_count 0.,
            Array.make point_count 0., Array.make point_count 0.)
        | Platonic_no_normals | Platonic_vertex_normals -> None in
      let identity_axes = orientation = Platonic_y
          && rotation.x = 0. && rotation.y = 0. && rotation.z = 0. in
      let invalid = ref (-1) in
      for point = 0 to point_count - 1 do
        if point land 15 = 0 then Cancel.check_opt cancel;
        let x = data.platonic_x.(point) and y = data.platonic_y.(point)
        and z = data.platonic_z.(point) in
        let tx, ty, tz = if identity_axes then
            center.x +. (radius *. x), center.y +. (radius *. y),
            center.z +. (radius *. z)
          else
            center.x +. (radius *. ((x_axis.x *. x) +. (y_axis.x *. y)
              +. (z_axis.x *. z))),
            center.y +. (radius *. ((x_axis.y *. x) +. (y_axis.y *. y)
              +. (z_axis.y *. z))),
            center.z +. (radius *. ((x_axis.z *. x) +. (y_axis.z *. y)
              +. (z_axis.z *. z))) in
        if Float.is_finite tx && Float.is_finite ty && Float.is_finite tz then begin
          px.(point) <- tx; py.(point) <- ty; pz.(point) <- tz;
          match point_normal with
          | None -> ()
          | Some (nx, ny, nz) when identity_axes ->
              nx.(point) <- x; ny.(point) <- y; nz.(point) <- z
          | Some (nx, ny, nz) ->
              nx.(point) <- (x_axis.x *. x) +. (y_axis.x *. y)
                  +. (z_axis.x *. z);
              ny.(point) <- (x_axis.y *. x) +. (y_axis.y *. y)
                  +. (z_axis.y *. z);
              nz.(point) <- (x_axis.z *. x) +. (y_axis.z *. y)
                  +. (z_axis.z *. z)
        end else if !invalid < 0 then invalid := point
      done;
      if !invalid >= 0 then Error (Printf.sprintf
          "Rdk.Parametric_generators.platonic: generated point %d is not finite" !invalid)
      else begin
        let topology = Topology.Private.create_validated_owned ~point_count
            ~vertex_points:(Array.copy data.platonic_vertex_points)
            ~primitive_offsets:(Array.copy data.platonic_primitive_offsets)
            ~primitive_kinds:(Bytes.make primitive_count '\000') in
        let attributes = ref [] in
        (match point_normal with
         | Some (x, y, z) ->
             let values = Packed.Float3.Private.of_owned_exn ~x ~y ~z in
             attributes := (Attribute.create_key_owned
                 (Attribute.normal ~owner:Attribute.Point) values |> Support.get_ok)
               :: !attributes
         | None -> ());
        (match normals with
         | Platonic_vertex_normals ->
             let nx = Array.make vertex_count 0.
             and ny = Array.make vertex_count 0.
             and nz = Array.make vertex_count 0. in
             for primitive = 0 to primitive_count - 1 do
               let x = data.platonic_face_nx.(primitive)
               and y = data.platonic_face_ny.(primitive)
               and z = data.platonic_face_nz.(primitive) in
               let tx, ty, tz = if identity_axes then x, y, z else
                   (x_axis.x *. x) +. (y_axis.x *. y) +. (z_axis.x *. z),
                   (x_axis.y *. x) +. (y_axis.y *. y) +. (z_axis.y *. z),
                   (x_axis.z *. x) +. (y_axis.z *. y) +. (z_axis.z *. z) in
               for vertex = data.platonic_primitive_offsets.(primitive)
                   to data.platonic_primitive_offsets.(primitive + 1) - 1 do
                 nx.(vertex) <- tx; ny.(vertex) <- ty; nz.(vertex) <- tz
               done
             done;
             let values = Packed.Float3.Private.of_owned_exn ~x:nx ~y:ny ~z:nz in
             attributes := (Attribute.create_key_owned
                 (Attribute.normal ~owner:Attribute.Vertex) values |> Support.get_ok)
               :: !attributes
         | Platonic_no_normals | Platonic_point_normals -> ());
        if kind = Platonic_soccer_ball then begin
          let x = Array.make primitive_count 1.
          and y = Array.make primitive_count 1.
          and z = Array.make primitive_count 1. in
          for primitive = 0 to data.platonic_soccer_pentagons - 1 do
            x.(primitive) <- 0.; y.(primitive) <- 0.; z.(primitive) <- 0.
          done;
          let values = Packed.Float3.Private.of_owned_exn ~x ~y ~z in
          attributes := (Attribute.create_owned ~owner:Attribute.Primitive
              ~name:"Cd" (Attribute.Float3 values) |> Support.get_ok) :: !attributes
        end;
        let groups = match face_groups with
          | None -> []
          | Some prefix ->
              let group size suffix =
                if not (Array.exists (( = ) size) data.platonic_face_sizes)
                then None
                else
                  let builder = Group.Builder.create ~owner:Group.Primitive
                      ~name:(prefix ^ "_" ^ suffix) primitive_count in
                  Array.iteri (fun primitive actual ->
                    if actual = size then Group.Builder.set builder primitive true)
                    data.platonic_face_sizes;
                  Some (Group.Builder.freeze builder) in
              [group 3 "triangles"; group 4 "quads";
               group 5 "pentagons"; group 6 "hexagons"]
              |> List.filter_map Fun.id in
        let positions = Packed.Float3.Private.of_owned_exn ~x:px ~y:py ~z:pz in
        Geometry.create ~positions ~topology ~attributes:(List.rev !attributes)
          ~groups ()
      end)
