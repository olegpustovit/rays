open Rays_math

type projection =
  | Planar of { origin : Rays_math.Vec3.t; u_axis : Rays_math.Vec3.t; v_axis : Rays_math.Vec3.t }
  | Cylindrical of {
      origin : Rays_math.Vec3.t; axis : Rays_math.Vec3.t; seam : Rays_math.Vec3.t; height : float;
    }
  | Spherical of { origin : Rays_math.Vec3.t; axis : Rays_math.Vec3.t; seam : Rays_math.Vec3.t }

type unitize_mode = Per_face | Islands

let flatten ?cancel ?grain ?name ?seams ?edge_seams ?iterations ?tolerance
    geometry =
  Error.guard ~operation:"uv_flatten" ~code:"invalid_uv" @@ fun () ->
  Uv_parameterize.solve ?cancel ?grain ?name ?seams ?edge_seams ?iterations
    ?tolerance Uv_parameterize.Circle geometry

let relax ?cancel ?grain ?name ?seams ?edge_seams ?uv_tolerance ?iterations
    ?tolerance geometry =
  Error.guard ~operation:"uv_relax" ~code:"invalid_uv" @@ fun () ->
  Uv_parameterize.solve ?cancel ?grain ?name ?seams ?edge_seams ?uv_tolerance
    ?iterations ?tolerance Uv_parameterize.Preserve geometry

type prepared =
  | P_planar of {
      ox : float; oy : float; oz : float;
      ux : float; uy : float; uz : float;
      vx : float; vy : float; vz : float;
      cosine : float; inverse_u : float; inverse_v : float;
      inverse_det : float;
    }
  | P_cylindrical of {
      ox : float; oy : float; oz : float;
      ax : float; ay : float; az : float;
      sx : float; sy : float; sz : float;
      tx : float; ty : float; tz : float;
      inverse_height : float;
    }
  | P_spherical of {
      ox : float; oy : float; oz : float;
      ax : float; ay : float; az : float;
      sx : float; sy : float; sz : float;
      tx : float; ty : float; tz : float;
    }

exception Uv_error of string
let fail message = raise (Uv_error message)
let finite value = Float.is_finite value
let finite3 value = finite value.Rays_math.Vec3.x && finite value.y && finite value.z
let finite2 value = finite value.Rays_math.Vec2.x && finite value.y

let run_ranges ?(grain = 16_384) count operation =
  if grain <= 0 then fail "grain must be positive";
  let ranges = (count + grain - 1) / grain in
  if ranges > 0 then
    Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(ranges - 1) (fun range ->
      let first = range * grain and last = min count ((range + 1) * grain) in
      operation ~first ~last)

let normalized_with_inverse name value =
  if not (finite3 value) then fail (name ^ " must have finite components");
  let scale = ref (abs_float value.Rays_math.Vec3.x) in
  let absolute_y = abs_float value.y and absolute_z = abs_float value.z in
  if absolute_y > !scale then scale := absolute_y;
  if absolute_z > !scale then scale := absolute_z;
  if !scale = 0. then fail (name ^ " must be non-zero");
  let x = value.x /. !scale and y = value.y /. !scale
  and z = value.z /. !scale in
  let normalized_length = sqrt (x *. x +. y *. y +. z *. z) in
  let inverse_normalized = 1. /. normalized_length in
  x *. inverse_normalized, y *. inverse_normalized, z *. inverse_normalized,
  inverse_normalized /. !scale

let normalized name value =
  let x, y, z, _ = normalized_with_inverse name value in x, y, z

let radial_frame ~axis ~seam =
  let ax, ay, az = normalized "projection axis" axis in
  let seam_x, seam_y, seam_z = normalized "projection seam" seam in
  let projection = seam_x *. ax +. seam_y *. ay +. seam_z *. az in
  let rx = seam_x -. projection *. ax
  and ry = seam_y -. projection *. ay
  and rz = seam_z -. projection *. az in
  let radial_sq = rx *. rx +. ry *. ry +. rz *. rz in
  if radial_sq <= 1e-24 then
    fail "projection seam must not be parallel to the projection axis";
  let inverse = 1. /. sqrt radial_sq in
  let sx = rx *. inverse and sy = ry *. inverse and sz = rz *. inverse in
  let tx = ay *. sz -. az *. sy
  and ty = az *. sx -. ax *. sz
  and tz = ax *. sy -. ay *. sx in
  ax, ay, az, sx, sy, sz, tx, ty, tz

let prepare = function
  | Planar { origin; u_axis; v_axis } ->
      if not (finite3 origin && finite3 u_axis && finite3 v_axis) then
        fail "planar projection vectors must have finite components";
      let ux, uy, uz, inverse_u =
        normalized_with_inverse "planar U axis" u_axis
      and vx, vy, vz, inverse_v =
        normalized_with_inverse "planar V axis" v_axis in
      let cosine = ux *. vx +. uy *. vy +. uz *. vz in
      let determinant = 1. -. cosine *. cosine in
      if determinant <= 1e-12
      then fail "planar projection axes must be non-zero and non-parallel";
      P_planar {
        ox = origin.x; oy = origin.y; oz = origin.z;
        ux; uy; uz; vx; vy; vz; cosine; inverse_u; inverse_v;
        inverse_det = 1. /. determinant;
      }
  | Cylindrical { origin; axis; seam; height } ->
      if not (finite3 origin) then fail "projection origin must be finite";
      if not (finite height) || height <= 0. then
        fail "cylindrical projection height must be finite and positive";
      let ax, ay, az, sx, sy, sz, tx, ty, tz =
        radial_frame ~axis ~seam in
      P_cylindrical {
        ox = origin.x; oy = origin.y; oz = origin.z;
        ax; ay; az; sx; sy; sz; tx; ty; tz;
        inverse_height = 1. /. height;
      }
  | Spherical { origin; axis; seam } ->
      if not (finite3 origin) then fail "projection origin must be finite";
      let ax, ay, az, sx, sy, sz, tx, ty, tz =
        radial_frame ~axis ~seam in
      P_spherical {
        ox = origin.x; oy = origin.y; oz = origin.z;
        ax; ay; az; sx; sy; sz; tx; ty; tz;
      }

let validate_range name (first, last) =
  if not (finite first && finite last) then fail (name ^ " must be finite")

let existing_vertex_uv ~name geometry count =
  match Geometry.find_attribute ~owner:Attribute.Vertex name geometry with
  | None -> Array.make count 0., Array.make count 0.
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Float2 values ->
           let values = Packed.Float2.Private.view values in
           Array.copy values.x, Array.copy values.y
       | _ -> fail (Printf.sprintf
           "existing vertex attribute %S must have float2 storage" name))

let validate_primitives selection primitive_count =
  match selection with
  | None -> ()
  | Some group ->
      if Group.owner group <> Group.Primitive then
        fail "UV Project selection must own primitives";
      if Group.length group <> primitive_count then
        fail "UV Project selection length does not match primitive count"

let project ?cancel ?grain ?(name = "uv") ?primitives
    ?(u_range = (0., 1.)) ?(v_range = (0., 1.))
    ?(fix_seams = true) ?(fix_poles = true) projection geometry =
  Error.guard ~operation:"uv_project" ~code:"invalid_projection" @@ fun () ->
  try
    if String.trim name = "" then fail "UV attribute name must not be empty";
    validate_range "U range" u_range;
    validate_range "V range" v_range;
    let prepared = prepare projection in
    let topology = Geometry.topology geometry in
    let topology_view = Topology.Private.view topology in
    let primitive_count = Array.length topology_view.primitive_offsets - 1
    and vertex_count = Array.length topology_view.vertex_points in
    validate_primitives primitives primitive_count;
    Cancel.check_opt cancel;
    let positions = Packed.Float3.Private.view (Geometry.positions geometry) in
    let invalid_position = Atomic.make false in
    run_ranges ?grain (Array.length positions.x) (fun ~first ~last ->
      Cancel.check_opt cancel;
      let invalid = ref false in
      for index = first to last - 1 do
        if index land 16_383 = 0 then Cancel.check_opt cancel;
        if not (finite positions.x.(index) && finite positions.y.(index)
            && finite positions.z.(index))
        then invalid := true
      done;
      if !invalid then Atomic.set invalid_position true);
    if Atomic.get invalid_position then
      fail "UV Project requires finite point positions";
    let output_u, output_v = existing_vertex_uv ~name geometry vertex_count in
    let u0, u1 = u_range and v0, v1 = v_range in
    let u_scale = u1 -. u0 and v_scale = v1 -. v0 in
    let invalid_output = Atomic.make false in
    run_ranges ?grain primitive_count (fun ~first ~last ->
      Cancel.check_opt cancel;
      let scratch = Array.make 4 0. in
      for primitive = first to last - 1 do
        let selected = match primitives with
          | None -> true
          | Some group -> Group.mem primitive group in
        if selected then begin
          let first_vertex = topology_view.primitive_offsets.(primitive)
          and last_vertex = topology_view.primitive_offsets.(primitive + 1) in
          match prepared with
          | P_planar frame ->
              for vertex = first_vertex to last_vertex - 1 do
                if vertex land 16_383 = 0 then Cancel.check_opt cancel;
                let point = topology_view.vertex_points.(vertex) in
                let dx = positions.x.(point) -. frame.ox
                and dy = positions.y.(point) -. frame.oy
                and dz = positions.z.(point) -. frame.oz in
                let du = dx *. frame.ux +. dy *. frame.uy +. dz *. frame.uz
                and dv = dx *. frame.vx +. dy *. frame.vy +. dz *. frame.vz in
                let u = (du -. dv *. frame.cosine) *. frame.inverse_det
                    *. frame.inverse_u +. 0.5
                and v = (dv -. du *. frame.cosine) *. frame.inverse_det
                    *. frame.inverse_v +. 0.5 in
                let u = u0 +. u *. u_scale
                and v = v0 +. v *. v_scale in
                if finite u && finite v then begin
                  output_u.(vertex) <- u;
                  output_v.(vertex) <- v
                end else Atomic.set invalid_output true
              done
          | P_cylindrical frame ->
              scratch.(0) <- infinity;
              scratch.(1) <- neg_infinity;
              for vertex = first_vertex to last_vertex - 1 do
                if vertex land 16_383 = 0 then Cancel.check_opt cancel;
                let point = topology_view.vertex_points.(vertex) in
                let dx = positions.x.(point) -. frame.ox
                and dy = positions.y.(point) -. frame.oy
                and dz = positions.z.(point) -. frame.oz in
                let axial = dx *. frame.ax +. dy *. frame.ay +. dz *. frame.az in
                let radial_x = dx -. axial *. frame.ax
                and radial_y = dy -. axial *. frame.ay
                and radial_z = dz -. axial *. frame.az in
                let radial_sq = radial_x *. radial_x +. radial_y *. radial_y
                    +. radial_z *. radial_z in
                let length_sq = dx *. dx +. dy *. dy +. dz *. dz in
                if radial_sq <= 1e-24 *. length_sq
                then output_u.(vertex) <- nan
                else begin
                  let cosine = radial_x *. frame.sx +. radial_y *. frame.sy
                      +. radial_z *. frame.sz
                  and sine = radial_x *. frame.tx +. radial_y *. frame.ty
                      +. radial_z *. frame.tz in
                  let u = atan2 sine cosine /. (2. *. Float.pi) in
                  let u = if u < 0. then u +. 1. else u in
                  output_u.(vertex) <- u;
                  if u < scratch.(0) then scratch.(0) <- u;
                  if u > scratch.(1) then scratch.(1) <- u
                end;
                output_v.(vertex) <- axial *. frame.inverse_height +. 0.5
              done;
              scratch.(2) <- 0.; scratch.(3) <- 0.;
              let crosses = fix_seams && scratch.(1) -. scratch.(0) > 0.5 in
              for vertex = first_vertex to last_vertex - 1 do
                if vertex land 16_383 = 0 then Cancel.check_opt cancel;
                let u = output_u.(vertex) in
                if not (Float.is_nan u) then begin
                  let u = if crosses && u < 0.5 then u +. 1. else u in
                  output_u.(vertex) <- u;
                  scratch.(2) <- scratch.(2) +. u;
                  scratch.(3) <- scratch.(3) +. 1.
                end
              done;
              let pole_u = if fix_poles && scratch.(3) > 0.
                then scratch.(2) /. scratch.(3) else 0. in
              for vertex = first_vertex to last_vertex - 1 do
                if vertex land 16_383 = 0 then Cancel.check_opt cancel;
                let u = output_u.(vertex) in
                let u = if Float.is_nan u then pole_u else u in
                let u = u0 +. u *. u_scale
                and v = v0 +. output_v.(vertex) *. v_scale in
                if finite u && finite v then begin
                  output_u.(vertex) <- u;
                  output_v.(vertex) <- v
                end else Atomic.set invalid_output true
              done
          | P_spherical frame ->
              scratch.(0) <- infinity;
              scratch.(1) <- neg_infinity;
              for vertex = first_vertex to last_vertex - 1 do
                if vertex land 16_383 = 0 then Cancel.check_opt cancel;
                let point = topology_view.vertex_points.(vertex) in
                let dx = positions.x.(point) -. frame.ox
                and dy = positions.y.(point) -. frame.oy
                and dz = positions.z.(point) -. frame.oz in
                let length_sq = dx *. dx +. dy *. dy +. dz *. dz in
                if length_sq = 0. then begin
                  output_u.(vertex) <- nan;
                  output_v.(vertex) <- 0.5
                end else begin
                  let inverse_length = 1. /. sqrt length_sq in
                  let axial = (dx *. frame.ax +. dy *. frame.ay
                      +. dz *. frame.az) *. inverse_length in
                  let axial = if axial < -1. then -1.
                    else if axial > 1. then 1. else axial in
                  output_v.(vertex) <- asin axial /. Float.pi +. 0.5;
                  let radial_x = dx -. axial /. inverse_length *. frame.ax
                  and radial_y = dy -. axial /. inverse_length *. frame.ay
                  and radial_z = dz -. axial /. inverse_length *. frame.az in
                  let radial_sq = radial_x *. radial_x +. radial_y *. radial_y
                      +. radial_z *. radial_z in
                  if radial_sq <= 1e-24 *. length_sq
                  then output_u.(vertex) <- nan
                  else begin
                    let cosine = radial_x *. frame.sx +. radial_y *. frame.sy
                        +. radial_z *. frame.sz
                    and sine = radial_x *. frame.tx +. radial_y *. frame.ty
                        +. radial_z *. frame.tz in
                    let u = atan2 sine cosine /. (2. *. Float.pi) in
                    let u = if u < 0. then u +. 1. else u in
                    output_u.(vertex) <- u;
                    if u < scratch.(0) then scratch.(0) <- u;
                    if u > scratch.(1) then scratch.(1) <- u
                  end
                end
              done;
              scratch.(2) <- 0.; scratch.(3) <- 0.;
              let crosses = fix_seams && scratch.(1) -. scratch.(0) > 0.5 in
              for vertex = first_vertex to last_vertex - 1 do
                if vertex land 16_383 = 0 then Cancel.check_opt cancel;
                let u = output_u.(vertex) in
                if not (Float.is_nan u) then begin
                  let u = if crosses && u < 0.5 then u +. 1. else u in
                  output_u.(vertex) <- u;
                  scratch.(2) <- scratch.(2) +. u;
                  scratch.(3) <- scratch.(3) +. 1.
                end
              done;
              let pole_u = if fix_poles && scratch.(3) > 0.
                then scratch.(2) /. scratch.(3) else 0. in
              for vertex = first_vertex to last_vertex - 1 do
                if vertex land 16_383 = 0 then Cancel.check_opt cancel;
                let u = output_u.(vertex) in
                let u = if Float.is_nan u then pole_u else u in
                let u = u0 +. u *. u_scale
                and v = v0 +. output_v.(vertex) *. v_scale in
                if finite u && finite v then begin
                  output_u.(vertex) <- u;
                  output_v.(vertex) <- v
                end else Atomic.set invalid_output true
              done
        end
      done);
    if Atomic.get invalid_output then
      fail "UV Project produced a non-finite coordinate; check projection scale and input magnitude";
    let values = Packed.Float2.of_owned ~x:output_u ~y:output_v
        |> Result.get_ok in
    let attribute = Attribute.create_key_owned
        (Attribute.key ~name ~owner:Attribute.Vertex Attribute.float2) values
        |> Result.get_ok in
    Geometry.with_attribute attribute geometry
  with Uv_error message -> Error message

let expected_group_owner = function
  | Attribute.Point -> Some Group.Point
  | Attribute.Vertex -> Some Group.Vertex
  | Attribute.Primitive | Attribute.Detail -> None

let transform ?cancel ?grain ?(name = "uv") ?selection ~owner
    ?(translate = Rays_math.Vec2.zero) ?(scale = Rays_math.Vec2.create 1. 1.) ?(angle = 0.)
    ?(pivot = Rays_math.Vec2.create 0.5 0.5) geometry =
  Error.guard ~operation:"uv_transform" ~code:"invalid_attribute" @@ fun () ->
  try
    if String.trim name = "" then fail "UV attribute name must not be empty";
    if not (finite2 translate && finite2 scale && finite angle && finite2 pivot)
    then fail "UV transform parameters must be finite";
    let group_owner = match expected_group_owner owner with
      | None -> fail "UV Transform owner must be Point or Vertex"
      | Some owner -> owner in
    Option.iter (fun group ->
      if Group.owner group <> group_owner then
        fail "UV Transform selection owner must match the UV attribute owner")
      selection;
    let attribute = match Geometry.find_attribute ~owner name geometry with
      | None -> fail (Printf.sprintf "missing %s UV attribute %S"
          (if owner = Attribute.Point then "point" else "vertex") name)
      | Some attribute -> attribute in
    let source = match Attribute.Private.storage attribute with
      | Attribute.Float2 values -> Packed.Float2.Private.view values
      | _ -> fail (Printf.sprintf "UV attribute %S must have float2 storage" name) in
    let count = Array.length source.x in
    Option.iter (fun group ->
      if Group.length group <> count then
        fail "UV Transform selection length does not match attribute length")
      selection;
    Cancel.check_opt cancel;
    let output_u = Array.copy source.x and output_v = Array.copy source.y in
    let cosine = cos angle and sine = sin angle in
    let invalid_uv = Atomic.make false in
    run_ranges ?grain count (fun ~first ~last ->
      Cancel.check_opt cancel;
      for index = first to last - 1 do
        if index land 16_383 = 0 then Cancel.check_opt cancel;
        let selected = match selection with
          | None -> true
          | Some group -> Group.mem index group in
        if selected then begin
          if not (finite source.x.(index) && finite source.y.(index)) then
            Atomic.set invalid_uv true
          else begin
            let u = (source.x.(index) -. pivot.x) *. scale.x
            and v = (source.y.(index) -. pivot.y) *. scale.y in
            let output_x = pivot.x +. translate.x +. cosine *. u -. sine *. v
            and output_y = pivot.y +. translate.y +. sine *. u +. cosine *. v in
            if finite output_x && finite output_y then begin
              output_u.(index) <- output_x;
              output_v.(index) <- output_y
            end else Atomic.set invalid_uv true
          end
        end
      done);
    if Atomic.get invalid_uv then
      fail "UV Transform requires finite selected input and output coordinates";
    let values = Packed.Float2.of_owned ~x:output_u ~y:output_v
        |> Result.get_ok in
    let attribute = Attribute.create_key_owned
        (Attribute.key ~name ~owner Attribute.float2) values |> Result.get_ok in
    Geometry.with_attribute attribute geometry
  with Uv_error message -> Error message

let selected_primitive selection primitive = match selection with
  | None -> true
  | Some group -> Group.mem primitive group

let validate_primitive_selection operation selection primitive_count =
  match selection with
  | None -> ()
  | Some group ->
      if Group.owner group <> Group.Primitive then
        fail (operation ^ " selection must own primitives");
      if Group.length group <> primitive_count then
        fail (operation ^ " selection length does not match primitive count")

let validate_selected_polygons operation selection topology =
  for primitive = 0 to Topology.primitive_count topology - 1 do
    if selected_primitive selection primitive
       && Topology.primitive_kind topology primitive <> Topology.Polygon
    then fail (operation ^ " supports selected polygon primitives only")
  done

let vertex_uv_attribute ~operation ~name geometry =
  match Geometry.find_attribute ~owner:Attribute.Vertex name geometry with
  | None -> fail (Printf.sprintf "%s requires vertex float2 attribute %S"
      operation name)
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Float2 values -> Packed.Float2.Private.view values
       | _ -> fail (Printf.sprintf "%s vertex attribute %S must be float2"
           operation name))

let primitive_int_attribute ~operation ~name geometry =
  match Geometry.find_attribute ~owner:Attribute.Primitive name geometry with
  | None -> fail (Printf.sprintf "%s could not find primitive integer attribute %S"
      operation name)
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Int values -> values
       | _ -> fail (Printf.sprintf "%s primitive attribute %S must be integer"
           operation name))

let max4 a b c d =
  let result = if a > b then a else b in
  let result = if c > result then c else result in
  if d > result then d else result

let edge_uv_discontinuous index_view topology_view uv tolerance edge =
  let first = index_view.Topology_index.Private.edge_offsets.(edge) in
  let left = index_view.edge_vertices.(first)
  and right = index_view.edge_vertices.(first + 1) in
  let left_next = index_view.next_vertex.(left)
  and right_next = index_view.next_vertex.(right) in
  if left_next < 0 || right_next < 0 then true
  else begin
    let endpoint = index_view.edge_a.(edge) in
    let left_a, left_b =
      if topology_view.Topology.Private.vertex_points.(left) = endpoint
      then left, left_next else left_next, left
    and right_a, right_b =
      if topology_view.vertex_points.(right) = endpoint
      then right, right_next else right_next, right in
    max4
      (abs_float (uv.Packed.Float2.Private.x.(left_a) -. uv.x.(right_a)))
      (abs_float (uv.y.(left_a) -. uv.y.(right_a)))
      (abs_float (uv.x.(left_b) -. uv.x.(right_b)))
      (abs_float (uv.y.(left_b) -. uv.y.(right_b))) > tolerance
  end

let rec find_root parent value =
  let next = parent.(value) in
  if next = value then value
  else begin
    let root = find_root parent next in
    parent.(value) <- root;
    root
  end

let union_roots parent rank left right =
  let left = find_root parent left and right = find_root parent right in
  if left <> right then begin
    let left_rank = Char.code (Bytes.get rank left)
    and right_rank = Char.code (Bytes.get rank right) in
    if left_rank < right_rank then parent.(left) <- right
    else if right_rank < left_rank then parent.(right) <- left
    else begin
      let root, child = if left < right then left, right else right, left in
      parent.(child) <- root;
      Bytes.set rank root (Char.chr (left_rank + 1))
    end
  end

let stable_islands ?cancel selection index_view edge_seam primitive_count =
  let parent = Array.init primitive_count Fun.id
  and rank = Bytes.make primitive_count '\000' in
  for edge = 0 to Bytes.length edge_seam - 1 do
    if edge land 16_383 = 0 then Cancel.check_opt cancel;
    if Bytes.get edge_seam edge = '\000' then begin
      let first = index_view.Topology_index.Private.edge_offsets.(edge)
      and last = index_view.edge_offsets.(edge + 1) in
      if last - first = 2 then begin
        let left = index_view.primitive_of_vertex.(index_view.edge_vertices.(first))
        and right = index_view.primitive_of_vertex.(index_view.edge_vertices.(first + 1)) in
        if left <> right && selected_primitive selection left
           && selected_primitive selection right
        then union_roots parent rank left right
      end
    end
  done;
  let root_to_island = Array.make primitive_count (-1)
  and primitive_island = Array.make primitive_count (-1) in
  let island_count = ref 0 in
  for primitive = 0 to primitive_count - 1 do
    if selected_primitive selection primitive then begin
      let root = find_root parent primitive in
      if root_to_island.(root) < 0 then begin
        root_to_island.(root) <- !island_count;
        incr island_count
      end;
      primitive_island.(primitive) <- root_to_island.(root)
    end
  done;
  primitive_island, !island_count

let auto_seam ?cancel ?grain ?(name = "uv_seams") ?primitives
    ?(angle = Float.pi /. 3.) ?(include_boundaries = true)
    ?(include_non_manifold = true) ?partition_attribute ?existing_uv
    ?(uv_tolerance = 1e-9) ?island_attribute geometry =
  Error.guard ~operation:"uv_auto_seam" ~code:"invalid_topology" @@ fun () ->
  try
    if String.trim name = "" then fail "UV Auto Seam group name must not be empty";
    if not (finite angle) || angle < 0. || angle > Float.pi then
      fail "UV Auto Seam angle must be finite and within [0, pi]";
    if not (finite uv_tolerance) || uv_tolerance < 0. then
      fail "UV Auto Seam tolerance must be finite and non-negative";
    Option.iter (fun name -> if String.trim name = "" then
      fail "UV Auto Seam partition attribute must not be empty") partition_attribute;
    Option.iter (fun name -> if String.trim name = "" then
      fail "UV Auto Seam UV attribute must not be empty") existing_uv;
    Option.iter (fun name -> if String.trim name = "" then
      fail "UV Auto Seam island attribute must not be empty") island_attribute;
    let topology = Geometry.topology geometry in
    let primitive_count = Topology.primitive_count topology in
    validate_primitive_selection "UV Auto Seam" primitives primitive_count;
    validate_selected_polygons "UV Auto Seam" primitives topology;
    Cancel.check_opt cancel;
    let nx, ny, nz = match Face_normals.compute ?cancel ?grain
        ?primitives ~operation:"UV Auto Seam" geometry with
      | Ok values -> values | Error message -> fail message in
    let index = Topology_index.create ?cancel topology in
    let index_view = Topology_index.Private.view index
    and topology_view = Topology.Private.view topology in
    let partition = Option.map (fun name ->
      primitive_int_attribute ~operation:"UV Auto Seam" ~name geometry)
        partition_attribute
    and uv = Option.map (fun name ->
      vertex_uv_attribute ~operation:"UV Auto Seam" ~name geometry) existing_uv in
    Option.iter (fun uv ->
      let invalid = Atomic.make false in
      run_ranges ?grain (Array.length uv.Packed.Float2.Private.x)
        (fun ~first ~last ->
          Cancel.check_opt cancel;
          for vertex = first to last - 1 do
            if not (finite uv.x.(vertex) && finite uv.y.(vertex)) then
              Atomic.set invalid true
          done);
      if Atomic.get invalid then fail "UV Auto Seam requires finite existing UVs") uv;
    let edge_count = Topology_index.edge_count index in
    let edge_seam = Bytes.make edge_count '\000' in
    let cosine_threshold = cos angle in
    run_ranges ?grain edge_count (fun ~first ~last ->
      Cancel.check_opt cancel;
      for edge = first to last - 1 do
        if edge land 16_383 = 0 then Cancel.check_opt cancel;
        let first_corner = index_view.edge_offsets.(edge)
        and last_corner = index_view.edge_offsets.(edge + 1) in
        let incidence = last_corner - first_corner in
        let set_seam value = if value then Bytes.set edge_seam edge '\001' in
        if incidence = 1 then begin
          let primitive = index_view.primitive_of_vertex.
              (index_view.edge_vertices.(first_corner)) in
          if selected_primitive primitives primitive then set_seam include_boundaries
        end else if incidence = 2 then begin
            let left = index_view.primitive_of_vertex.(index_view.edge_vertices.(first_corner))
            and right = index_view.primitive_of_vertex.(index_view.edge_vertices.(first_corner + 1)) in
            let left_selected = selected_primitive primitives left
            and right_selected = selected_primitive primitives right in
            if left_selected <> right_selected then
              set_seam include_boundaries
            else if left_selected then begin
              let partition_cut = match partition with
                | None -> false
                | Some values -> values.(left) <> values.(right) in
              let uv_cut = match uv with
                | None -> false
                | Some uv -> edge_uv_discontinuous index_view topology_view uv
                    uv_tolerance edge in
              let dot = nx.(left) *. nx.(right) +. ny.(left) *. ny.(right)
                  +. nz.(left) *. nz.(right) in
              set_seam (left = right || partition_cut || uv_cut
                  || dot < cosine_threshold)
            end
          end else begin
            let rec has_selected local =
              local < last_corner
              && (let primitive = index_view.primitive_of_vertex.
                    (index_view.edge_vertices.(local)) in
                  selected_primitive primitives primitive
                  || has_selected (local + 1)) in
            if has_selected first_corner then set_seam include_non_manifold
          end
      done);
    let corner_seams = Group.init ?grain ~owner:Group.Vertex ~name
        (Topology.vertex_count topology) (fun vertex ->
          let edge = index_view.edge_of_vertex.(vertex) in
          edge >= 0 && Bytes.get edge_seam edge <> '\000') in
    let seams = Edge_group.init ?grain ~topology ~index ~name (fun edge ->
      Bytes.get edge_seam edge <> '\000') in
    let geometry = Geometry.with_group corner_seams geometry |> Result.get_ok
        |> Geometry.with_edge_group seams |> Result.get_ok in
    match island_attribute with
    | None -> Ok geometry
    | Some name ->
        let primitive_island, _ = stable_islands ?cancel primitives index_view
            edge_seam primitive_count in
        let values = match Geometry.find_attribute ~owner:Attribute.Primitive
            name geometry with
          | None -> primitive_island
          | Some attribute ->
              (match Attribute.Private.storage attribute with
               | Attribute.Int existing ->
                   Array.mapi (fun primitive island ->
                     if selected_primitive primitives primitive then island
                     else existing.(primitive)) primitive_island
               | _ -> fail (Printf.sprintf
                   "UV Auto Seam output attribute %S must be integer" name)) in
        let attribute = Attribute.create_owned ~name ~owner:Attribute.Primitive
            (Attribute.Int values) |> Result.get_ok in
        Geometry.with_attribute attribute geometry
  with Uv_error message -> Error message

let validate_seams seams vertex_count = match seams with
  | None -> ()
  | Some group ->
      if Group.owner group <> Group.Vertex then
        fail "UV Unitize seam group must own outgoing-edge vertices";
      if Group.length group <> vertex_count then
        fail "UV Unitize seam group length does not match vertex count"

let validate_edge_seams topology index seams = match seams with
  | None -> ()
  | Some group ->
      if Edge_group.topology_data_id group <> Topology.data_id topology then
        fail "UV Unitize edge seam group belongs to a different topology";
      if Edge_group.length group <> Topology_index.edge_count index then
        fail "UV Unitize edge seam group length does not match edge count"

let unitize_values ?cancel ~uniform ~minimum_u ~maximum_u ~minimum_v ~maximum_v
    source_u source_v output_u output_v first last =
  let span_u = maximum_u -. minimum_u and span_v = maximum_v -. minimum_v in
  if not (finite span_u && finite span_v) then
    fail "UV Unitize selected UV extent is not finite";
  if uniform then begin
    let span = if span_u > span_v then span_u else span_v in
    if span = 0. then
      for vertex = first to last - 1 do
        output_u.(vertex) <- 0.5; output_v.(vertex) <- 0.5
      done
    else begin
      let inverse = 1. /. span
      and center_u = minimum_u +. span_u *. 0.5
      and center_v = minimum_v +. span_v *. 0.5 in
      for vertex = first to last - 1 do
        if vertex land 16_383 = 0 then Cancel.check_opt cancel;
        output_u.(vertex) <- 0.5 +. (source_u.(vertex) -. center_u) *. inverse;
        output_v.(vertex) <- 0.5 +. (source_v.(vertex) -. center_v) *. inverse
      done
    end
  end else begin
    let inverse_u = if span_u = 0. then 0. else 1. /. span_u
    and inverse_v = if span_v = 0. then 0. else 1. /. span_v in
    for vertex = first to last - 1 do
      if vertex land 16_383 = 0 then Cancel.check_opt cancel;
      output_u.(vertex) <- if span_u = 0. then 0.5
        else (source_u.(vertex) -. minimum_u) *. inverse_u;
      output_v.(vertex) <- if span_v = 0. then 0.5
        else (source_v.(vertex) -. minimum_v) *. inverse_v
    done
  end

let unitize ?cancel ?grain ?(name = "uv") ?primitives ?seams ?edge_seams
    ?(tolerance = 1e-9) ?(uniform = true) mode geometry =
  Error.guard ~operation:"uv_unitize" ~code:"invalid_uv" @@ fun () ->
  try
    if String.trim name = "" then fail "UV Unitize attribute name must not be empty";
    if not (finite tolerance) || tolerance < 0. then
      fail "UV Unitize tolerance must be finite and non-negative";
    let topology = Geometry.topology geometry in
    let topology_view = Topology.Private.view topology in
    let primitive_count = Topology.primitive_count topology
    and vertex_count = Topology.vertex_count topology in
    validate_primitive_selection "UV Unitize" primitives primitive_count;
    validate_selected_polygons "UV Unitize" primitives topology;
    validate_seams seams vertex_count;
    let uv = vertex_uv_attribute ~operation:"UV Unitize" ~name geometry in
    let invalid = Atomic.make false in
    run_ranges ?grain primitive_count (fun ~first ~last ->
      Cancel.check_opt cancel;
      for primitive = first to last - 1 do
        if selected_primitive primitives primitive then begin
          let first_vertex = topology_view.primitive_offsets.(primitive)
          and last_vertex = topology_view.primitive_offsets.(primitive + 1) in
          for vertex = first_vertex to last_vertex - 1 do
            if not (finite uv.x.(vertex) && finite uv.y.(vertex)) then
              Atomic.set invalid true
          done
        end
      done);
    if Atomic.get invalid then fail "UV Unitize requires finite selected UVs";
    let output_u = Array.copy uv.x and output_v = Array.copy uv.y in
    (match mode with
     | Per_face ->
         run_ranges ?grain primitive_count (fun ~first ~last ->
           Cancel.check_opt cancel;
           let bounds = Array.make 4 0. in
           for primitive = first to last - 1 do
             if selected_primitive primitives primitive then begin
               let first_vertex = topology_view.primitive_offsets.(primitive)
               and last_vertex = topology_view.primitive_offsets.(primitive + 1) in
               bounds.(0) <- infinity; bounds.(1) <- neg_infinity;
               bounds.(2) <- infinity; bounds.(3) <- neg_infinity;
               for vertex = first_vertex to last_vertex - 1 do
                 let u = uv.x.(vertex) and v = uv.y.(vertex) in
                 if u < bounds.(0) then bounds.(0) <- u;
                 if u > bounds.(1) then bounds.(1) <- u;
                 if v < bounds.(2) then bounds.(2) <- v;
                 if v > bounds.(3) then bounds.(3) <- v
               done;
               unitize_values ?cancel ~uniform ~minimum_u:bounds.(0)
                 ~maximum_u:bounds.(1) ~minimum_v:bounds.(2)
                 ~maximum_v:bounds.(3) uv.x uv.y output_u output_v
                 first_vertex last_vertex
             end
           done)
     | Islands ->
         let index = Topology_index.create ?cancel topology in
         let index_view = Topology_index.Private.view index in
         validate_edge_seams topology index edge_seams;
         let edge_count = Topology_index.edge_count index in
         let edge_seam = Bytes.make edge_count '\001' in
         run_ranges ?grain edge_count (fun ~first ~last ->
           Cancel.check_opt cancel;
           for edge = first to last - 1 do
             let first_corner = index_view.edge_offsets.(edge)
             and last_corner = index_view.edge_offsets.(edge + 1) in
             if last_corner - first_corner = 2 then begin
               let left_corner = index_view.edge_vertices.(first_corner)
               and right_corner = index_view.edge_vertices.(first_corner + 1) in
               let left = index_view.primitive_of_vertex.(left_corner)
               and right = index_view.primitive_of_vertex.(right_corner) in
               let forced = match seams with
                 | None -> false
                 | Some group -> Group.mem left_corner group
                     || Group.mem right_corner group in
               let forced = forced || match edge_seams with
                 | None -> false
                 | Some group -> Edge_group.mem edge group in
               if not (left = right || forced
                   || not (selected_primitive primitives left
                     && selected_primitive primitives right)
                   || edge_uv_discontinuous index_view topology_view uv
                        tolerance edge)
               then Bytes.set edge_seam edge '\000'
             end
           done);
         let primitive_island, island_count = stable_islands ?cancel primitives
             index_view edge_seam primitive_count in
         let primitive_min_u = Array.make primitive_count infinity
         and primitive_max_u = Array.make primitive_count neg_infinity
         and primitive_min_v = Array.make primitive_count infinity
         and primitive_max_v = Array.make primitive_count neg_infinity in
         run_ranges ?grain primitive_count (fun ~first ~last ->
           Cancel.check_opt cancel;
           for primitive = first to last - 1 do
             if primitive_island.(primitive) >= 0 then begin
               let first_vertex = topology_view.primitive_offsets.(primitive)
               and last_vertex = topology_view.primitive_offsets.(primitive + 1) in
               for vertex = first_vertex to last_vertex - 1 do
                 let u = uv.x.(vertex) and v = uv.y.(vertex) in
                 if u < primitive_min_u.(primitive) then primitive_min_u.(primitive) <- u;
                 if u > primitive_max_u.(primitive) then primitive_max_u.(primitive) <- u;
                 if v < primitive_min_v.(primitive) then primitive_min_v.(primitive) <- v;
                 if v > primitive_max_v.(primitive) then primitive_max_v.(primitive) <- v
               done
             end
           done);
         let island_min_u = Array.make island_count infinity
         and island_max_u = Array.make island_count neg_infinity
         and island_min_v = Array.make island_count infinity
         and island_max_v = Array.make island_count neg_infinity in
         for primitive = 0 to primitive_count - 1 do
           let island = primitive_island.(primitive) in
           if island >= 0 then begin
             if primitive_min_u.(primitive) < island_min_u.(island) then
               island_min_u.(island) <- primitive_min_u.(primitive);
             if primitive_max_u.(primitive) > island_max_u.(island) then
               island_max_u.(island) <- primitive_max_u.(primitive);
             if primitive_min_v.(primitive) < island_min_v.(island) then
               island_min_v.(island) <- primitive_min_v.(primitive);
             if primitive_max_v.(primitive) > island_max_v.(island) then
               island_max_v.(island) <- primitive_max_v.(primitive)
           end
         done;
         run_ranges ?grain primitive_count (fun ~first ~last ->
           Cancel.check_opt cancel;
           for primitive = first to last - 1 do
             let island = primitive_island.(primitive) in
             if island >= 0 then begin
               let first_vertex = topology_view.primitive_offsets.(primitive)
               and last_vertex = topology_view.primitive_offsets.(primitive + 1) in
               unitize_values ?cancel ~uniform ~minimum_u:island_min_u.(island)
                 ~maximum_u:island_max_u.(island)
                 ~minimum_v:island_min_v.(island)
                 ~maximum_v:island_max_v.(island)
                 uv.x uv.y output_u output_v first_vertex last_vertex
             end
           done));
    let values = Packed.Float2.of_owned ~x:output_u ~y:output_v
        |> Result.get_ok in
    let attribute = Attribute.create_key_owned
        (Attribute.key ~name ~owner:Attribute.Vertex Attribute.float2) values
        |> Result.get_ok in
    Geometry.with_attribute attribute geometry
  with Uv_error message -> Error message
