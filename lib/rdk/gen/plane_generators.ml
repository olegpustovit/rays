open Rdk_core

type grid_counts = Grid_divisions | Grid_point_counts
type grid_orientation =
  | Grid_xy
  | Grid_xz
  | Grid_yz
  | Grid_axes of { horizontal : Rays_math.Vec3.t; vertical : Rays_math.Vec3.t }
type grid_connectivity =
  | Grid_points
  | Grid_rows
  | Grid_columns
  | Grid_rows_and_columns
  | Grid_quads
  | Grid_triangles
  | Grid_alternating_triangles
  | Grid_reverse_triangles

let normal_attribute owner count nx ny nz =
  let x = Array.make count nx and y = Array.make count ny
  and z = Array.make count nz in
  let values = Packed.Float3.Private.of_owned_exn ~x ~y ~z in
  Attribute.create_key_owned (Attribute.normal ~owner) values |> Support.get_ok

let normalize_plane_axis operation label value =
  if not (Float.is_finite value.Rays_math.Vec3.x && Float.is_finite value.y && Float.is_finite value.z) then
    Error (operation ^ ": " ^ label ^ " axis must be finite")
  else
    let scale = max (abs_float value.x)
        (max (abs_float value.y) (abs_float value.z)) in
    if scale = 0. then Error (operation ^ ": " ^ label ^ " axis must be non-zero")
    else
      let x = value.x /. scale and y = value.y /. scale
      and z = value.z /. scale in
      let length = sqrt ((x *. x) +. (y *. y) +. (z *. z)) in
      if not (Float.is_finite length) || length <= 1e-15 then
        Error (operation ^ ": " ^ label ^ " axis cannot be normalized")
      else Ok (Rays_math.Vec3.create (x /. length) (y /. length) (z /. length))

let plane_frame operation ~horizontal ~vertical rotation =
  Result.bind (normalize_plane_axis operation "horizontal" horizontal)
    (fun horizontal ->
  Result.bind (normalize_plane_axis operation "vertical" vertical)
    (fun vertical ->
  let projection = Rays_math.Vec3.dot vertical horizontal in
  let vertical = Rays_math.Vec3.create
      (vertical.x -. (projection *. horizontal.x))
      (vertical.y -. (projection *. horizontal.y))
      (vertical.z -. (projection *. horizontal.z)) in
  Result.bind (normalize_plane_axis operation "vertical" vertical)
    (fun vertical ->
    let horizontal, vertical = if rotation = 0. then horizontal, vertical
      else
        let cosine = cos rotation and sine = sin rotation in
        Rays_math.Vec3.create
          ((cosine *. horizontal.x) +. (sine *. vertical.x))
          ((cosine *. horizontal.y) +. (sine *. vertical.y))
          ((cosine *. horizontal.z) +. (sine *. vertical.z)),
        Rays_math.Vec3.create
          ((-.sine *. horizontal.x) +. (cosine *. vertical.x))
          ((-.sine *. horizontal.y) +. (cosine *. vertical.y))
          ((-.sine *. horizontal.z) +. (cosine *. vertical.z)) in
    Result.map (fun normal -> horizontal, vertical, normal)
      (normalize_plane_axis operation "normal"
         (Rays_math.Vec3.cross vertical horizontal)))))

let grid_frame orientation rotation =
  let horizontal, vertical = match orientation with
    | Grid_xy -> Rays_math.Vec3.unit_x, Rays_math.Vec3.neg Rays_math.Vec3.unit_y
    | Grid_xz -> Rays_math.Vec3.unit_x, Rays_math.Vec3.unit_z
    | Grid_yz -> Rays_math.Vec3.unit_z, Rays_math.Vec3.unit_y
    | Grid_axes { horizontal; vertical } -> horizontal, vertical in
  plane_frame "Rdk.Plane_generators.grid" ~horizontal ~vertical rotation

let grid ?cancel ?(grain = 16_384) ?(counts = Grid_divisions)
    ?(connectivity = Grid_triangles) ?(orientation = Grid_xz)
    ?(center = Rays_math.Vec3.zero) ?width ?height ?(rotation = 0.) ?uv_attribute
    ~columns ~rows ~size () =
  Error.guard ~operation:"grid" ~code:"invalid_parameter" @@ fun () ->
  let width = Option.value ~default:size width
  and height = Option.value ~default:size height in
  let minimum_columns, minimum_rows = match counts, connectivity with
    | Grid_divisions, _ -> 1, 1
    | Grid_point_counts, Grid_points -> 1, 1
    | Grid_point_counts, Grid_rows -> 2, 1
    | Grid_point_counts, Grid_columns -> 1, 2
    | Grid_point_counts, (Grid_rows_and_columns | Grid_quads | Grid_triangles
        | Grid_alternating_triangles | Grid_reverse_triangles) -> 2, 2 in
  if grain <= 0 then Error "Rdk.Plane_generators.grid: grain must be positive"
  else if columns < minimum_columns || rows < minimum_rows then Error
      (Printf.sprintf "Rdk.Plane_generators.grid: this count/connectivity mode requires at least %d columns and %d rows"
         minimum_columns minimum_rows)
  else if counts = Grid_divisions && (columns = max_int || rows = max_int) then
    Error "Rdk.Plane_generators.grid: point cardinality overflows"
  else if not (Float.is_finite size && size > 0. && Float.is_finite width && width > 0.
      && Float.is_finite height && height > 0.) then
    Error "Rdk.Plane_generators.grid: size, width, and height must be finite and positive"
  else if not (Float.is_finite center.x && Float.is_finite center.y && Float.is_finite center.z
      && Float.is_finite rotation) then
    Error "Rdk.Plane_generators.grid: center and rotation must be finite"
  else if match uv_attribute with
    | Some name -> String.trim name = "" || String.equal name "P"
        || String.equal name "N"
    | None -> false then
    Error "Rdk.Plane_generators.grid: UV attribute name must be non-empty and cannot be P or N"
  else Result.bind (grid_frame orientation rotation)
      (fun (horizontal, vertical, normal) ->
    let u_points, v_points = match counts with
      | Grid_divisions -> columns + 1, rows + 1
      | Grid_point_counts -> columns, rows in
    let u_divisions = u_points - 1 and v_divisions = v_points - 1 in
    let checked_product left right limit =
      if left = 0 || right <= limit / left then Some (left * right) else None in
    let point_limit = Sys.max_array_length
    and primitive_limit = min (Sys.max_array_length - 1) Sys.max_string_length in
    match checked_product u_points v_points point_limit with
    | None -> Error "Rdk.Plane_generators.grid: point cardinality exceeds OCaml array limits"
    | Some point_count ->
        let topology_cardinality =
          let cells = match checked_product u_divisions v_divisions point_limit with
            | Some cells -> cells | None -> -1 in
          match connectivity with
          | Grid_points -> Some (0, 0)
          | Grid_rows -> if v_points <= primitive_limit then
                Some (point_count, v_points) else None
          | Grid_columns -> if u_points <= primitive_limit then
                Some (point_count, u_points) else None
          | Grid_rows_and_columns ->
              if point_count <= point_limit / 2
                 && u_points <= primitive_limit - v_points
              then Some (point_count * 2, u_points + v_points) else None
          | Grid_quads ->
              if cells >= 0 && cells <= point_limit / 4
                 && cells <= primitive_limit
              then Some (cells * 4, cells) else None
          | Grid_triangles | Grid_alternating_triangles
          | Grid_reverse_triangles ->
              if cells >= 0 && cells <= point_limit / 6
                 && cells <= primitive_limit / 2
              then Some (cells * 6, cells * 2) else None in
        (match topology_cardinality with
         | None -> Error
             "Rdk.Plane_generators.grid: topology cardinality exceeds OCaml array limits"
         | Some (vertex_count, primitive_count) ->
             let px = Array.make point_count 0. and py = Array.make point_count 0.
             and pz = Array.make point_count 0. in
             let uv_x, uv_y = match uv_attribute with
               | None -> None, None
               | Some _ -> Some (Array.make point_count 0.),
                   Some (Array.make point_count 0.) in
             let row_grain = max 1 (grain / u_points) in
             let range_count = ((v_points - 1) / row_grain) + 1 in
             let errors = Array.make range_count (-1) in
             Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(range_count - 1)
               (fun range ->
                 let first_row = range * row_grain
                 and last_row = min v_points ((range + 1) * row_grain) in
                 for row = first_row to last_row - 1 do
                   let v = if v_points = 1 then 0.5
                     else float_of_int row /. float_of_int v_divisions in
                   let dv = (v -. 0.5) *. height in
                   let base_x = center.x +. (vertical.x *. dv)
                   and base_y = center.y +. (vertical.y *. dv)
                   and base_z = center.z +. (vertical.z *. dv)
                   and at = row * u_points in
                   for column = 0 to u_points - 1 do
                     let point = at + column in
                     if point land 4095 = 0 then Cancel.check_opt cancel;
                     let u = if u_points = 1 then 0.5
                       else float_of_int column /. float_of_int u_divisions in
                     let du = (u -. 0.5) *. width in
                     let x = base_x +. (horizontal.x *. du)
                     and y = base_y +. (horizontal.y *. du)
                     and z = base_z +. (horizontal.z *. du) in
                     if Float.is_finite x && Float.is_finite y && Float.is_finite z then begin
                       px.(point) <- x; py.(point) <- y; pz.(point) <- z;
                       (match uv_x, uv_y with
                        | Some uv_x, Some uv_y ->
                            uv_x.(point) <- u; uv_y.(point) <- v
                        | None, None -> ()
                        | _ -> assert false)
                     end else if errors.(range) < 0 then errors.(range) <- point
                   done
                 done);
             let invalid = Array.fold_left (fun first point ->
                 if point < 0 then first else if first < 0 then point
                 else min first point) (-1) errors in
             if invalid >= 0 then Error (Printf.sprintf
                 "Rdk.Plane_generators.grid: generated point %d is not finite" invalid)
             else
               let positions = Packed.Float3.Private.of_owned_exn
                   ~x:px ~y:py ~z:pz in
               let line_topology mode =
                 let vertex_points = Array.make vertex_count 0
                 and primitive_offsets = Array.make (primitive_count + 1) 0
                 and primitive_kinds = Bytes.make primitive_count '\001' in
                 let primitive_grain =
                   max 1 (grain / max 1 (max u_points v_points)) in
                 let fill_row at =
                   for column = 0 to u_points - 1 do
                     vertex_points.(at + column) <- at + column
                   done
                 and fill_column column at =
                   for row = 0 to v_points - 1 do
                     vertex_points.(at + row) <- (row * u_points) + column
                   done in
                 Parallel.for_ ~chunk_size:primitive_grain ~start:0
                   ~finish:(primitive_count - 1) (fun primitive ->
                     if primitive land 1023 = 0 then Cancel.check_opt cancel;
                     match mode with
                     | `Rows ->
                         let at = primitive * u_points in
                         fill_row at
                     | `Columns ->
                         fill_column primitive (primitive * v_points)
                     | `Both ->
                         if primitive < v_points then
                           fill_row (primitive * u_points)
                         else
                           let column = primitive - v_points in
                           fill_column column
                             (point_count + (column * v_points)));
                 (match mode with
                  | `Rows ->
                      for primitive = 0 to primitive_count do
                        primitive_offsets.(primitive) <- primitive * u_points
                      done
                  | `Columns ->
                      for primitive = 0 to primitive_count do
                        primitive_offsets.(primitive) <- primitive * v_points
                      done
                  | `Both ->
                      for row = 0 to v_points do
                        primitive_offsets.(row) <- row * u_points
                      done;
                      for column = 0 to u_points do
                        primitive_offsets.(v_points + column) <-
                          point_count + (column * v_points)
                      done);
                 Topology.Private.create_validated_owned ~point_count
                   ~vertex_points ~primitive_offsets ~primitive_kinds in
               let polygon_topology slots reverse_mode =
                 let vertex_points = Array.make vertex_count 0
                 and primitive_offsets = Array.make (primitive_count + 1) 0
                 and primitive_kinds = Bytes.make primitive_count '\000' in
                 let cell_count = u_divisions * v_divisions in
                 Parallel.for_ ~chunk_size:(max 1 (grain / slots)) ~start:0
                   ~finish:(cell_count - 1) (fun cell ->
                     if cell land 4095 = 0 then Cancel.check_opt cancel;
                     let row = cell / u_divisions
                     and column = cell mod u_divisions in
                     let a = (row * u_points) + column in
                     let b = a + 1 and d = a + u_points in
                     let c = d + 1 and at = cell * slots in
                     if slots = 4 then begin
                       vertex_points.(at) <- a; vertex_points.(at + 1) <- d;
                       vertex_points.(at + 2) <- c; vertex_points.(at + 3) <- b
                     end else
                       let reverse = reverse_mode = 1
                         || reverse_mode = 2 && ((row + column) land 1 = 1) in
                       if reverse then begin
                         vertex_points.(at) <- a; vertex_points.(at + 1) <- d;
                         vertex_points.(at + 2) <- b; vertex_points.(at + 3) <- b;
                         vertex_points.(at + 4) <- d; vertex_points.(at + 5) <- c
                       end else begin
                         vertex_points.(at) <- a; vertex_points.(at + 1) <- d;
                         vertex_points.(at + 2) <- c; vertex_points.(at + 3) <- a;
                         vertex_points.(at + 4) <- c; vertex_points.(at + 5) <- b
                       end);
                 let primitive_size = if slots = 4 then 4 else 3 in
                 Parallel.for_ ~chunk_size:grain ~start:0 ~finish:primitive_count
                   (fun primitive ->
                     if primitive land 4095 = 0 then Cancel.check_opt cancel;
                     primitive_offsets.(primitive) <- primitive * primitive_size);
                 Topology.Private.create_validated_owned ~point_count
                   ~vertex_points ~primitive_offsets ~primitive_kinds in
               let topology = match connectivity with
                 | Grid_points -> Topology.empty ~point_count
                 | Grid_rows -> line_topology `Rows
                 | Grid_columns -> line_topology `Columns
                 | Grid_rows_and_columns -> line_topology `Both
                 | Grid_quads -> polygon_topology 4 0
                 | Grid_triangles -> polygon_topology 6 0
                 | Grid_reverse_triangles -> polygon_topology 6 1
                 | Grid_alternating_triangles -> polygon_topology 6 2 in
               let normal_attribute = normal_attribute Attribute.Point point_count
                   normal.x normal.y normal.z in
               let attributes = match uv_attribute, uv_x, uv_y with
                 | None, None, None -> [normal_attribute]
                 | Some name, Some x, Some y ->
                     let uv = Packed.Float2.of_owned ~x ~y |> Support.get_ok in
                     let uv = Attribute.create_owned ~name ~owner:Attribute.Point
                         (Attribute.Float2 uv) |> Support.get_ok in
                     [normal_attribute; uv]
                 | _ -> assert false in
               Geometry.create ~positions ~topology ~attributes ()))
