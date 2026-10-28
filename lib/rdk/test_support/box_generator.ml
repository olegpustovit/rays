open Rdk_core

type box_connectivity =
  | Box_triangles
  | Box_quads
  | Box_surface_points
  | Box_lattice_points
type box_normals = Box_no_normals | Box_point_normals | Box_vertex_normals
type box_rotation_order =
  | Box_xyz | Box_xzy | Box_yxz | Box_yzx | Box_zxy | Box_zyx

let box_rotation_matrix order rotation =
  let x = Rays_math.Mat4.rotation_x rotation.Rays_math.Vec3.x
  and y = Rays_math.Mat4.rotation_y rotation.y
  and z = Rays_math.Mat4.rotation_z rotation.z in
  match order with
  | Box_xyz -> Rays_math.Mat4.mul z (Rays_math.Mat4.mul y x)
  | Box_xzy -> Rays_math.Mat4.mul y (Rays_math.Mat4.mul z x)
  | Box_yxz -> Rays_math.Mat4.mul z (Rays_math.Mat4.mul x y)
  | Box_yzx -> Rays_math.Mat4.mul x (Rays_math.Mat4.mul z y)
  | Box_zxy -> Rays_math.Mat4.mul y (Rays_math.Mat4.mul x z)
  | Box_zyx -> Rays_math.Mat4.mul x (Rays_math.Mat4.mul y z)

let box ?cancel ?(grain = 16_384) ?(connectivity = Box_triangles)
    ?(consolidate_points = false) ?normals ?(center = Rays_math.Vec3.zero)
    ?(rotation = Rays_math.Vec3.zero) ?(rotation_order = Box_xyz)
    ?(uniform_scale = 1.) ?(x_divisions = 1) ?(y_divisions = 1)
    ?(z_divisions = 1) ?uv_attribute ?face_groups ~size () =
  Error.guard ~operation:"box" ~code:"invalid_parameter" @@ fun () ->
  let polygon_mode = match connectivity with
    | Box_triangles | Box_quads -> true
    | Box_surface_points | Box_lattice_points -> false in
  let normal_mode = match normals with
    | Some mode -> mode
    | None -> if polygon_mode then Box_point_normals else Box_no_normals in
  let sx = size.Rays_math.Vec3.x *. uniform_scale
  and sy = size.y *. uniform_scale
  and sz = size.z *. uniform_scale in
  if grain <= 0 then Error "Rdk.Box_generator.box: grain must be positive"
  else if x_divisions <= 0 || y_divisions <= 0 || z_divisions <= 0 then
    Error "Rdk.Box_generator.box: axis divisions must be positive"
  else if x_divisions = max_int || y_divisions = max_int
      || z_divisions = max_int then
    Error "Rdk.Box_generator.box: point cardinality overflows"
  else if not (Float.is_finite size.x && Float.is_finite size.y && Float.is_finite size.z
      && Float.is_finite uniform_scale && uniform_scale > 0.
      && Float.is_finite sx && Float.is_finite sy && Float.is_finite sz
      && sx > 0. && sy > 0. && sz > 0.) then
    Error "Rdk.Box_generator.box: dimensions and uniform scale must be finite and positive"
  else if not (Float.is_finite center.x && Float.is_finite center.y && Float.is_finite center.z
      && Float.is_finite rotation.x && Float.is_finite rotation.y && Float.is_finite rotation.z) then
    Error "Rdk.Box_generator.box: center and rotation must be finite"
  else if (match uv_attribute with
      | Some name -> String.trim name = "" || String.equal name "P"
          || String.equal name "N"
      | None -> false) then
    Error "Rdk.Box_generator.box: UV attribute name must be non-empty and cannot be P or N"
  else if (match face_groups with
      | Some prefix -> String.trim prefix = ""
      | None -> false) then
    Error "Rdk.Box_generator.box: face-group prefix must be non-empty"
  else if not polygon_mode && Option.is_some uv_attribute then
    Error "Rdk.Box_generator.box: point output cannot carry vertex UVs"
  else if not polygon_mode && Option.is_some face_groups then
    Error "Rdk.Box_generator.box: point output cannot carry primitive face groups"
  else if not polygon_mode && normal_mode = Box_vertex_normals then
    Error "Rdk.Box_generator.box: point output cannot carry vertex normals"
  else if connectivity = Box_lattice_points
      && normal_mode <> Box_no_normals then
    Error "Rdk.Box_generator.box: volume lattice points do not have surface normals"
  else if connectivity = Box_triangles && not consolidate_points
      && normal_mode = Box_point_normals
      && center.x = 0. && center.y = 0. && center.z = 0.
      && rotation.x = 0. && rotation.y = 0. && rotation.z = 0.
      && uniform_scale = 1. && x_divisions = 1 && y_divisions = 1
      && z_divisions = 1 && Option.is_none uv_attribute
      && Option.is_none face_groups then begin
    Cancel.check_opt cancel;
    let hx = sx *. 0.5 and hy = sy *. 0.5 and hz = sz *. 0.5 in
    let positions = Packed.Float3.View.of_owned_exn
        ~x:[|hx;hx;hx;hx; -.hx;-.hx;-.hx;-.hx;
             -.hx;-.hx;hx;hx; -.hx;-.hx;hx;hx;
             -.hx;hx;hx;-.hx; hx;-.hx;-.hx;hx|]
        ~y:[|-.hy;hy;hy;-.hy; -.hy;hy;hy;-.hy;
             hy;hy;hy;hy; -.hy;-.hy;-.hy;-.hy;
             -.hy;-.hy;hy;hy; -.hy;-.hy;hy;hy|]
        ~z:[|-.hz;-.hz;hz;hz; hz;hz;-.hz;-.hz;
             -.hz;hz;hz;-.hz; hz;-.hz;-.hz;hz;
             hz;hz;hz;hz; -.hz;-.hz;-.hz;-.hz|] in
    let normals = Packed.Float3.View.of_owned_exn
        ~x:[|1.;1.;1.;1.; -1.;-1.;-1.;-1.;
             0.;0.;0.;0.; 0.;0.;0.;0.; 0.;0.;0.;0.; 0.;0.;0.;0.|]
        ~y:[|0.;0.;0.;0.; 0.;0.;0.;0.;
             1.;1.;1.;1.; -1.;-1.;-1.;-1.;
             0.;0.;0.;0.; 0.;0.;0.;0.|]
        ~z:[|0.;0.;0.;0.; 0.;0.;0.;0.; 0.;0.;0.;0.; 0.;0.;0.;0.;
             1.;1.;1.;1.; -1.;-1.;-1.;-1.|] in
    let vertex_points = Array.init 36 (fun vertex ->
      let face = vertex / 6 and local = vertex mod 6 in
      (face * 4) + (match local with
        | 0 | 3 -> 0 | 1 -> 1 | 2 | 4 -> 2 | _ -> 3)) in
    let topology = Topology.View.create_validated_owned ~point_count:24
        ~vertex_points ~primitive_offsets:(Array.init 13 (fun index -> index * 3))
        ~primitive_kinds:(Bytes.make 12 '\000') in
    let normal = Attribute.create_key_owned
        (Attribute.normal ~owner:Attribute.Point) normals |> Support.get_ok in
    Geometry.create ~positions ~topology ~attributes:[normal] ()
  end
  else
    let point_limit = Sys.max_array_length
    and primitive_limit = min (Sys.max_array_length - 1) Sys.max_string_length in
    let checked_mul left right limit =
      if left = 0 || right <= limit / left then Some (left * right) else None
    and checked_add left right limit =
      if right <= limit - left then Some (left + right) else None in
    let nxp = x_divisions + 1 and nyp = y_divisions + 1
    and nzp = z_divisions + 1 in
    let pair left right = checked_mul left right point_limit in
    let sum3 a b c = match checked_add a b point_limit with
      | None -> None
      | Some value -> checked_add value c point_limit in
    let xy_points = pair nxp nyp and yz_points = pair nyp nzp
    and zx_points = pair nzp nxp in
    let xy_cells = pair x_divisions y_divisions
    and yz_cells = pair y_divisions z_divisions
    and zx_cells = pair z_divisions x_divisions in
    let face_local_points = match xy_points, yz_points, zx_points with
      | Some xy, Some yz, Some zx ->
          Option.bind (sum3 xy yz zx) (fun half -> checked_mul 2 half point_limit)
      | _ -> None in
    let boundary_points = match xy_points with
      | None -> None
      | Some plane ->
          (match checked_mul 2 nxp point_limit,
                 checked_mul 2 (y_divisions - 1) point_limit with
           | Some a, Some b ->
               Option.bind (checked_add a b point_limit) (fun ring ->
               Option.bind (checked_mul (z_divisions - 1) ring point_limit)
                 (fun middle ->
               Option.bind (checked_mul 2 plane point_limit)
                 (fun caps -> checked_add caps middle point_limit)))
           | _ -> None) in
    let lattice_points = match xy_points with
      | Some xy -> checked_mul xy nzp point_limit
      | None -> None in
    let surface_cells = match xy_cells, yz_cells, zx_cells with
      | Some xy, Some yz, Some zx ->
          Option.bind (sum3 xy yz zx) (fun half -> checked_mul 2 half point_limit)
      | _ -> None in
    let point_count = match connectivity, consolidate_points with
      | Box_lattice_points, _ -> lattice_points
      | (Box_triangles | Box_quads | Box_surface_points), true -> boundary_points
      | (Box_triangles | Box_quads | Box_surface_points), false ->
          face_local_points in
    let topology_cardinality = match connectivity, surface_cells with
      | (Box_surface_points | Box_lattice_points), _ -> Some (0, 0)
      | _, None -> None
      | Box_quads, Some cells ->
          if cells <= primitive_limit && cells <= point_limit / 4
          then Some (cells * 4, cells) else None
      | Box_triangles, Some cells ->
          if cells <= primitive_limit / 2 && cells <= point_limit / 6
          then Some (cells * 6, cells * 2) else None in
    match point_count, topology_cardinality with
    | None, _ -> Error "Rdk.Box_generator.box: point cardinality exceeds OCaml array limits"
    | _, None -> Error "Rdk.Box_generator.box: topology cardinality exceeds OCaml array limits"
    | Some point_count, Some (vertex_count, primitive_count) ->
        let hx = sx *. 0.5 and hy = sy *. 0.5 and hz = sz *. 0.5 in
        let rotated = rotation.x <> 0. || rotation.y <> 0. || rotation.z <> 0. in
        let matrix = box_rotation_matrix rotation_order rotation in
        let m00 = Rays_math.Mat4.get matrix ~row:0 ~column:0
        and m01 = Rays_math.Mat4.get matrix ~row:0 ~column:1
        and m02 = Rays_math.Mat4.get matrix ~row:0 ~column:2
        and m10 = Rays_math.Mat4.get matrix ~row:1 ~column:0
        and m11 = Rays_math.Mat4.get matrix ~row:1 ~column:1
        and m12 = Rays_math.Mat4.get matrix ~row:1 ~column:2
        and m20 = Rays_math.Mat4.get matrix ~row:2 ~column:0
        and m21 = Rays_math.Mat4.get matrix ~row:2 ~column:1
        and m22 = Rays_math.Mat4.get matrix ~row:2 ~column:2 in
        let[@inline always] rotate x y z =
          if rotated then
            (m00 *. x) +. (m01 *. y) +. (m02 *. z),
            (m10 *. x) +. (m11 *. y) +. (m12 *. z),
            (m20 *. x) +. (m21 *. y) +. (m22 *. z)
          else x, y, z in
        let[@inline always] axis half extent index divisions =
          if index = 0 then -.half
          else if index = divisions then half
          else -.half +. (extent *. float_of_int index /. float_of_int divisions) in
        let x_axis = Array.init nxp (fun index ->
          axis hx sx index x_divisions)
        and y_axis = Array.init nyp (fun index ->
          axis hy sy index y_divisions)
        and z_axis = Array.init nzp (fun index ->
          axis hz sz index z_divisions) in
        let face_u_div face = match face with
          | 0 | 1 -> y_divisions | 2 | 3 -> z_divisions | _ -> x_divisions
        and face_v_div face = match face with
          | 0 | 1 -> z_divisions | 2 | 3 -> x_divisions | _ -> y_divisions in
        let face_point_offsets = Array.make 7 0
        and face_cell_offsets = Array.make 7 0 in
        for face = 0 to 5 do
          let u = face_u_div face and v = face_v_div face in
          face_point_offsets.(face + 1) <- face_point_offsets.(face)
            + ((u + 1) * (v + 1));
          face_cell_offsets.(face + 1) <- face_cell_offsets.(face) + (u * v)
        done;
        let face_normal face = match face with
          | 0 -> 1., 0., 0. | 1 -> -1., 0., 0.
          | 2 -> 0., 1., 0. | 3 -> 0., -1., 0.
          | 4 -> 0., 0., 1. | _ -> 0., 0., -1. in
        let face_normals = Array.init 6 (fun face ->
          let x, y, z = face_normal face in rotate x y z) in
        let plane = nxp * nyp and ring = (2 * nxp) + (2 * (y_divisions - 1)) in
        let top_base = plane + ((z_divisions - 1) * ring) in
        let[@inline always] boundary_index x y z =
          if z = 0 then (y * nxp) + x
          else if z = z_divisions then top_base + (y * nxp) + x
          else
            let base = plane + ((z - 1) * ring) in
            if y = 0 then base + x
            else if y = y_divisions then
              base + nxp + (2 * (y_divisions - 1)) + x
            else base + nxp + (2 * (y - 1))
                + if x = 0 then 0 else 1 in
        let[@inline always] face_point_index face row column =
          if consolidate_points then
            match face with
            | 0 -> boundary_index x_divisions column row
            | 1 -> boundary_index 0 column (z_divisions - row)
            | 2 -> boundary_index row y_divisions column
            | 3 -> boundary_index row 0 (z_divisions - column)
            | 4 -> boundary_index column row z_divisions
            | _ -> boundary_index (x_divisions - column) row 0
          else
            let u_points = face_u_div face + 1 in
            face_point_offsets.(face) + (row * u_points)
              + if row land 1 = 0 then column else u_points - 1 - column in
        let px = Array.make point_count 0. and py = Array.make point_count 0.
        and pz = Array.make point_count 0. in
        let point_normals = match normal_mode with
          | Box_point_normals -> Some (Array.make point_count 0.,
              Array.make point_count 0., Array.make point_count 0.)
          | Box_no_normals | Box_vertex_normals -> None in
        let[@inline always] write_grid_position point x_index y_index z_index =
          let local_x = x_axis.(x_index) and local_y = y_axis.(y_index)
          and local_z = z_axis.(z_index) in
          let x = center.x +. if rotated then
              (m00 *. local_x) +. (m01 *. local_y) +. (m02 *. local_z)
            else local_x
          and y = center.y +. if rotated then
              (m10 *. local_x) +. (m11 *. local_y) +. (m12 *. local_z)
            else local_y
          and z = center.z +. if rotated then
              (m20 *. local_x) +. (m21 *. local_y) +. (m22 *. local_z)
            else local_z in
          if Float.is_finite x && Float.is_finite y && Float.is_finite z then begin
            px.(point) <- x; py.(point) <- y; pz.(point) <- z;
            true
          end else false in
        let[@inline always] write_boundary point x y z =
          let valid = write_grid_position point x y z in
          if valid then (
            match point_normals with
            | Some (nx, ny, nz) ->
                let qx = if x = 0 then -1. else if x = x_divisions then 1.
                  else 0.
                and qy = if y = 0 then -1. else if y = y_divisions then 1.
                  else 0.
                and qz = if z = 0 then -1. else if z = z_divisions then 1.
                  else 0. in
                let length = sqrt ((qx *. qx) +. (qy *. qy) +. (qz *. qz)) in
                let qx = qx /. length and qy = qy /. length
                and qz = qz /. length in
                if rotated then begin
                  nx.(point) <- (m00 *. qx) +. (m01 *. qy) +. (m02 *. qz);
                  ny.(point) <- (m10 *. qx) +. (m11 *. qy) +. (m12 *. qz);
                  nz.(point) <- (m20 *. qx) +. (m21 *. qy) +. (m22 *. qz)
                end else begin
                  nx.(point) <- qx; ny.(point) <- qy; nz.(point) <- qz
                end
            | None -> ());
          valid in
        let invalid_point = ref (-1) in
        let note_errors errors =
          Array.iter (fun point -> if point >= 0
              && (!invalid_point < 0 || point < !invalid_point)
            then invalid_point := point) errors in
        (match connectivity, consolidate_points with
         | Box_lattice_points, _ ->
             let range_count = ((point_count - 1) / grain) + 1 in
             let errors = Array.make range_count (-1) in
             Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(range_count - 1)
               (fun range ->
                 let first = range * grain
                 and last = min point_count ((range + 1) * grain) in
                 for point = first to last - 1 do
                   if point land 4095 = 0 then Cancel.check_opt cancel;
                   let x = point mod nxp in
                   let yz = point / nxp in
                   let y = yz mod nyp and z = yz / nyp in
                   if not (write_grid_position point x y z)
                      && errors.(range) < 0 then errors.(range) <- point
                 done);
             note_errors errors
         | _, true ->
             let range_count = ((point_count - 1) / grain) + 1 in
             let errors = Array.make range_count (-1) in
             Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(range_count - 1)
               (fun range ->
                 let first = range * grain
                 and last = min point_count ((range + 1) * grain) in
                 for point = first to last - 1 do
                   if point land 4095 = 0 then Cancel.check_opt cancel;
                   let valid =
                     if point < plane then
                       write_boundary point (point mod nxp) (point / nxp) 0
                     else if point >= top_base then
                       let local = point - top_base in
                       write_boundary point (local mod nxp) (local / nxp)
                         z_divisions
                     else
                       let local = point - plane in
                       let z = 1 + (local / ring)
                       and within = local mod ring in
                       if within < nxp then write_boundary point within 0 z
                       else if within < nxp + (2 * (y_divisions - 1)) then
                         let pair = within - nxp in
                         write_boundary point
                           (if pair land 1 = 0 then 0 else x_divisions)
                           (1 + (pair / 2)) z
                       else write_boundary point
                           (within - nxp - (2 * (y_divisions - 1)))
                           y_divisions z in
                   if not valid && errors.(range) < 0 then
                     errors.(range) <- point
                 done);
             note_errors errors
         | _, false ->
             for face = 0 to 5 do
               let u_div = face_u_div face and v_div = face_v_div face in
               let u_points = u_div + 1 in
               let row_grain = max 1 (grain / u_points) in
               let range_count = (v_div / row_grain) + 1 in
               let errors = Array.make range_count (-1) in
               let nnx, nny, nnz = face_normals.(face) in
               Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(range_count - 1)
                 (fun range ->
                   let first_row = range * row_grain
                   and last_row = min (v_div + 1) ((range + 1) * row_grain) in
                   for row = first_row to last_row - 1 do
                     for column = 0 to u_div do
                       let point = face_point_index face row column in
                       if point land 4095 = 0 then Cancel.check_opt cancel;
                       let valid = match face with
                         | 0 -> write_grid_position point x_divisions column row
                         | 1 -> write_grid_position point 0 column
                             (z_divisions - row)
                         | 2 -> write_grid_position point row y_divisions column
                         | 3 -> write_grid_position point row 0
                             (z_divisions - column)
                         | 4 -> write_grid_position point column row z_divisions
                         | _ -> write_grid_position point
                             (x_divisions - column) row 0 in
                       if valid then
                         (match point_normals with
                          | Some (nx, ny, nz) ->
                              nx.(point) <- nnx; ny.(point) <- nny;
                              nz.(point) <- nnz
                          | None -> ())
                       else if errors.(range) < 0 then errors.(range) <- point
                     done
                   done);
               note_errors errors
             done);
        if !invalid_point >= 0 then Error (Printf.sprintf
            "Rdk.Box_generator.box: generated point %d is not finite" !invalid_point)
        else
          let positions = Packed.Float3.View.of_owned_exn ~x:px ~y:py ~z:pz in
          let topology, vertex_normals, uv = if not polygon_mode then
              Topology.empty ~point_count, None, None
            else
              let slots = if connectivity = Box_quads then 4 else 6 in
              let primitive_per_cell = if slots = 4 then 1 else 2 in
              let vertex_points = Array.make vertex_count 0
              and primitive_offsets = Array.make (primitive_count + 1) 0
              and primitive_kinds = Bytes.make primitive_count '\000' in
              let vertex_normals = match normal_mode with
                | Box_vertex_normals -> Some (Array.make vertex_count 0.,
                    Array.make vertex_count 0., Array.make vertex_count 0.)
                | Box_no_normals | Box_point_normals -> None in
              let uv = match uv_attribute with
                | Some _ -> Some (Array.make vertex_count 0.,
                    Array.make vertex_count 0.)
                | None -> None in
              for face = 0 to 5 do
                let u_div = face_u_div face and v_div = face_v_div face in
                let cell_count = u_div * v_div in
                let cell_base = face_cell_offsets.(face) in
                let nnx, nny, nnz = face_normals.(face) in
                let[@inline always] write_vertex at slot point u_index v_index =
                  let vertex = at + slot in
                  vertex_points.(vertex) <- point;
                  (match vertex_normals with
                   | Some (x, y, z) ->
                       x.(vertex) <- nnx; y.(vertex) <- nny; z.(vertex) <- nnz
                   | None -> ());
                  match uv with
                  | Some (x, y) ->
                      x.(vertex) <- float_of_int u_index /. float_of_int u_div;
                      y.(vertex) <- float_of_int v_index /. float_of_int v_div
                  | None -> () in
                Parallel.for_ ~chunk_size:(max 1 (grain / slots)) ~start:0
                  ~finish:(cell_count - 1) (fun local_cell ->
                    if local_cell land 4095 = 0 then Cancel.check_opt cancel;
                    let row = local_cell / u_div and column = local_cell mod u_div in
                    let a = face_point_index face row column
                    and b = face_point_index face row (column + 1)
                    and c = face_point_index face (row + 1) (column + 1)
                    and d = face_point_index face (row + 1) column in
                    let at = (cell_base + local_cell) * slots in
                    if slots = 4 then begin
                      write_vertex at 0 a column row;
                      write_vertex at 1 b (column + 1) row;
                      write_vertex at 2 c (column + 1) (row + 1);
                      write_vertex at 3 d column (row + 1)
                    end else begin
                      write_vertex at 0 a column row;
                      write_vertex at 1 b (column + 1) row;
                      write_vertex at 2 c (column + 1) (row + 1);
                      write_vertex at 3 a column row;
                      write_vertex at 4 c (column + 1) (row + 1);
                      write_vertex at 5 d column (row + 1)
                    end)
              done;
              Parallel.for_ ~chunk_size:grain ~start:0 ~finish:primitive_count
                (fun primitive ->
                  if primitive land 4095 = 0 then Cancel.check_opt cancel;
                  primitive_offsets.(primitive) <- primitive
                    * (slots / primitive_per_cell));
              Topology.View.create_validated_owned ~point_count
                ~vertex_points ~primitive_offsets ~primitive_kinds,
              vertex_normals, uv in
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
          (match uv_attribute, uv with
           | Some name, Some (x, y) ->
               let values = Packed.Float2.of_owned ~x ~y |> Support.get_ok in
               attributes := (Attribute.create_owned ~name ~owner:Attribute.Vertex
                   (Attribute.Float2 values) |> Support.get_ok) :: !attributes
           | None, None -> ()
           | _ -> assert false);
          let groups = match face_groups with
            | None -> []
            | Some prefix ->
                let names = [|"right"; "left"; "top"; "bottom";
                  "front"; "back"|] in
                Array.to_list (Array.init 6 (fun face ->
                  let first = face_cell_offsets.(face)
                      * (if connectivity = Box_triangles then 2 else 1)
                  and last = face_cell_offsets.(face + 1)
                      * (if connectivity = Box_triangles then 2 else 1) in
                  let builder = Group.Builder.create ~owner:Group.Primitive
                      ~name:(prefix ^ "__" ^ names.(face)) primitive_count in
                  for primitive = first to last - 1 do
                    if primitive land 4095 = 0 then Cancel.check_opt cancel;
                    Group.Builder.set builder primitive true
                  done;
                  Group.Builder.freeze builder)) in
          Geometry.create ~positions ~topology ~attributes:(List.rev !attributes)
            ~groups ()
