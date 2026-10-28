module Exact = Exact_dyadic

type source = { x : float array; y : float array; z : float array }
type projection = XY | YZ | ZX
type homogeneous = { hx : Exact.t; hy : Exact.t; hz : Exact.t; hw : Exact.t }

type t = {
  source : source;
  construction : construction;
  homogeneous : homogeneous option Atomic.t;
  approximate_x : float;
  approximate_y : float;
  approximate_z : float;
  x_lower : float;
  x_upper : float;
  y_lower : float;
  y_upper : float;
  z_lower : float;
  z_upper : float;
  x_error : float;
  y_error : float;
  z_error : float;
}

and construction =
  | Explicit of int
  | Rounded of { x : float; y : float; z : float }
  | Line_plane of { line_start : int; line_end : int; plane_a : int; plane_b : int; plane_c : int }
  | Line_line of {
      projection : projection;
      first_start : int;
      first_end : int;
      second_start : int;
      second_end : int;
    }
  | Triple_plane of {
      first_a : int;
      first_b : int;
      first_c : int;
      second_a : int;
      second_b : int;
      second_c : int;
      third_a : int;
      third_b : int;
      third_c : int;
    }
  | Midpoint of { first : t; second : t }
  | Centroid3 of { first : t; second : t; third : t }
  | Circumcenter2_xy of { first : t; second : t; third : t }

type error =
  | Coordinate_plane_size_mismatch
  | Non_finite_coordinate of int
  | Index_out_of_bounds of int
  | Parallel_line_and_plane
  | Parallel_lines
  | Dependent_planes
  | Point_source_mismatch
  | Degenerate_triangle
  | Non_finite_approximation

let source ~x ~y ~z =
  let count = Array.length x in
  if Array.length y <> count || Array.length z <> count then Error Coordinate_plane_size_mismatch
  else begin
    let invalid = ref (-1) and index = ref 0 in
    while !invalid < 0 && !index < count do
      if not (Float.is_finite x.(!index) && Float.is_finite y.(!index) && Float.is_finite z.(!index))
      then invalid := !index;
      incr index
    done;
    if !invalid >= 0 then Error (Non_finite_coordinate !invalid) else Ok { x; y; z }
  end

let construction point = point.construction

let constant_components source first second =
  (if source.x.(first) = source.x.(second) then 1 else 0)
  + (if source.y.(first) = source.y.(second) then 1 else 0)
  + if source.z.(first) = source.z.(second) then 1 else 0

let valid_index source index = index >= 0 && index < Array.length source.x

let source_coordinate source point =
  if not (valid_index source point) then invalid_arg "implicit source point is out of bounds";
  (source.x.(point), source.y.(point), source.z.(point))

let exact_at source index =
  (Exact.of_float source.x.(index), Exact.of_float source.y.(index), Exact.of_float source.z.(index))

let add3 (ax, ay, az) (bx, by, bz) = (Exact.add ax bx, Exact.add ay by, Exact.add az bz)

let subtract3 (ax, ay, az) (bx, by, bz) =
  (Exact.subtract ax bx, Exact.subtract ay by, Exact.subtract az bz)

let scale3 scalar (x, y, z) =
  (Exact.multiply scalar x, Exact.multiply scalar y, Exact.multiply scalar z)

let dot (ax, ay, az) (bx, by, bz) =
  Exact.add (Exact.add (Exact.multiply ax bx) (Exact.multiply ay by)) (Exact.multiply az bz)

let cross (ax, ay, az) (bx, by, bz) =
  ( Exact.subtract (Exact.multiply ay bz) (Exact.multiply az by),
    Exact.subtract (Exact.multiply az bx) (Exact.multiply ax bz),
    Exact.subtract (Exact.multiply ax by) (Exact.multiply ay bx) )

let negate_homogeneous point =
  {
    hx = Exact.negate point.hx;
    hy = Exact.negate point.hy;
    hz = Exact.negate point.hz;
    hw = Exact.negate point.hw;
  }

let normalize point = if Exact.compare_zero point.hw < 0 then negate_homogeneous point else point

let approximate_ratio numerator denominator =
  if Exact.is_zero numerator then 0.
  else begin
    let numerator_mantissa, numerator_exponent = Exact.to_scaled_float numerator
    and denominator_mantissa, denominator_exponent = Exact.to_scaled_float denominator in
    Float.ldexp
      (numerator_mantissa /. denominator_mantissa)
      (numerator_exponent - denominator_exponent)
  end

let[@inline always] downward value = Float.next_after value Float.neg_infinity
let[@inline always] upward value = Float.next_after value Float.infinity
let[@inline always] float_min left right = if left <= right then left else right
let[@inline always] float_max left right = if left >= right then left else right

let ratio_interval numerator denominator =
  if Exact.is_zero numerator then (0., 0.)
  else begin
    let numerator_lower, numerator_upper, numerator_exponent = Exact.to_scaled_interval numerator
    and denominator_lower, denominator_upper, denominator_exponent =
      Exact.to_scaled_interval denominator
    in
    assert (denominator_lower > 0.);
    let first = numerator_lower /. denominator_lower
    and second = numerator_lower /. denominator_upper
    and third = numerator_upper /. denominator_lower
    and fourth = numerator_upper /. denominator_upper in
    let lower = float_min (float_min first second) (float_min third fourth)
    and upper = float_max (float_max first second) (float_max third fourth)
    and exponent = numerator_exponent - denominator_exponent in
    (downward (Float.ldexp lower exponent), upward (Float.ldexp upper exponent))
  end

let make source construction homogeneous =
  let homogeneous = normalize homogeneous in
  let approximate_x = approximate_ratio homogeneous.hx homogeneous.hw
  and approximate_y = approximate_ratio homogeneous.hy homogeneous.hw
  and approximate_z = approximate_ratio homogeneous.hz homogeneous.hw
  and x_lower, x_upper = ratio_interval homogeneous.hx homogeneous.hw
  and y_lower, y_upper = ratio_interval homogeneous.hy homogeneous.hw
  and z_lower, z_upper = ratio_interval homogeneous.hz homogeneous.hw in
  if Float.is_finite approximate_x && Float.is_finite approximate_y && Float.is_finite approximate_z
  then
    Ok
      {
        source;
        construction;
        homogeneous = Atomic.make (Some homogeneous);
        approximate_x;
        approximate_y;
        approximate_z;
        x_lower;
        x_upper;
        y_lower;
        y_upper;
        z_lower;
        z_upper;
        x_error =
          Float.max (abs_float (approximate_x -. x_lower)) (abs_float (x_upper -. approximate_x));
        y_error =
          Float.max (abs_float (approximate_y -. y_lower)) (abs_float (y_upper -. approximate_y));
        z_error =
          Float.max (abs_float (approximate_z -. z_lower)) (abs_float (z_upper -. approximate_z));
      }
  else Error Non_finite_approximation

let make_deferred_explicit source construction x y z =
  {
    source;
    construction;
    homogeneous = Atomic.make None;
    approximate_x = x;
    approximate_y = y;
    approximate_z = z;
    x_lower = x;
    x_upper = x;
    y_lower = y;
    y_upper = y;
    z_lower = z;
    z_upper = z;
    x_error = 0.;
    y_error = 0.;
    z_error = 0.;
  }

let explicit source index =
  if not (valid_index source index) then Error (Index_out_of_bounds index)
  else
    Ok
      (make_deferred_explicit source (Explicit index) source.x.(index) source.y.(index)
         source.z.(index))

let rounded ~reference ~x ~y ~z =
  if not (Float.is_finite x && Float.is_finite y && Float.is_finite z) then
    Error Non_finite_approximation
  else Ok (make_deferred_explicit reference.source (Rounded { x; y; z }) x y z)

let first_invalid source indices =
  let invalid = ref None and index = ref 0 in
  while Option.is_none !invalid && !index < Array.length indices do
    let candidate = indices.(!index) in
    if not (valid_index source candidate) then invalid := Some candidate;
    incr index
  done;
  !invalid

let try_line_plane_deferred source construction ~line_start ~line_end ~plane_a ~plane_b ~plane_c =
  let px = source.x.(line_start)
  and py = source.y.(line_start)
  and pz = source.z.(line_start)
  and qx = source.x.(line_end)
  and qy = source.y.(line_end)
  and qz = source.z.(line_end)
  and rx = source.x.(plane_a)
  and ry = source.y.(plane_a)
  and rz = source.z.(plane_a)
  and sx = source.x.(plane_b)
  and sy = source.y.(plane_b)
  and sz = source.z.(plane_b)
  and tx = source.x.(plane_c)
  and ty = source.y.(plane_c)
  and tz = source.z.(plane_c) in
  let factor = 8. *. Float.epsilon and smallest = Int64.float_of_bits 1L in
  let dx = qx -. px
  and dy = qy -. py
  and dz = qz -. pz
  and ux = sx -. rx
  and uy = sy -. ry
  and uz = sz -. rz
  and vx = tx -. rx
  and vy = ty -. ry
  and vz = tz -. rz
  and wx = rx -. px
  and wy = ry -. py
  and wz = rz -. pz in
  let dx_error = (factor *. (abs_float qx +. abs_float px)) +. smallest
  and dy_error = (factor *. (abs_float qy +. abs_float py)) +. smallest
  and dz_error = (factor *. (abs_float qz +. abs_float pz)) +. smallest
  and ux_error = (factor *. (abs_float sx +. abs_float rx)) +. smallest
  and uy_error = (factor *. (abs_float sy +. abs_float ry)) +. smallest
  and uz_error = (factor *. (abs_float sz +. abs_float rz)) +. smallest
  and vx_error = (factor *. (abs_float tx +. abs_float rx)) +. smallest
  and vy_error = (factor *. (abs_float ty +. abs_float ry)) +. smallest
  and vz_error = (factor *. (abs_float tz +. abs_float rz)) +. smallest
  and wx_error = (factor *. (abs_float rx +. abs_float px)) +. smallest
  and wy_error = (factor *. (abs_float ry +. abs_float py)) +. smallest
  and wz_error = (factor *. (abs_float rz +. abs_float pz)) +. smallest in
  let nx_left = uy *. vz
  and nx_right = uz *. vy
  and ny_left = uz *. vx
  and ny_right = ux *. vz
  and nz_left = ux *. vy
  and nz_right = uy *. vx in
  let nx_left_error =
    (abs_float uy *. vz_error)
    +. (abs_float vz *. uy_error)
    +. (uy_error *. vz_error)
    +. (factor *. abs_float nx_left)
    +. smallest
  and nx_right_error =
    (abs_float uz *. vy_error)
    +. (abs_float vy *. uz_error)
    +. (uz_error *. vy_error)
    +. (factor *. abs_float nx_right)
    +. smallest
  and ny_left_error =
    (abs_float uz *. vx_error)
    +. (abs_float vx *. uz_error)
    +. (uz_error *. vx_error)
    +. (factor *. abs_float ny_left)
    +. smallest
  and ny_right_error =
    (abs_float ux *. vz_error)
    +. (abs_float vz *. ux_error)
    +. (ux_error *. vz_error)
    +. (factor *. abs_float ny_right)
    +. smallest
  and nz_left_error =
    (abs_float ux *. vy_error)
    +. (abs_float vy *. ux_error)
    +. (ux_error *. vy_error)
    +. (factor *. abs_float nz_left)
    +. smallest
  and nz_right_error =
    (abs_float uy *. vx_error)
    +. (abs_float vx *. uy_error)
    +. (uy_error *. vx_error)
    +. (factor *. abs_float nz_right)
    +. smallest
  in
  let nx = nx_left -. nx_right and ny = ny_left -. ny_right and nz = nz_left -. nz_right in
  let nx_error =
    nx_left_error +. nx_right_error
    +. (factor *. (abs_float nx_left +. abs_float nx_right))
    +. smallest
  and ny_error =
    ny_left_error +. ny_right_error
    +. (factor *. (abs_float ny_left +. abs_float ny_right))
    +. smallest
  and nz_error =
    nz_left_error +. nz_right_error
    +. (factor *. (abs_float nz_left +. abs_float nz_right))
    +. smallest
  in
  let denominator_x = nx *. dx
  and denominator_y = ny *. dy
  and denominator_z = nz *. dz
  and numerator_x = nx *. wx
  and numerator_y = ny *. wy
  and numerator_z = nz *. wz in
  let denominator_x_error =
    (abs_float nx *. dx_error)
    +. (abs_float dx *. nx_error)
    +. (nx_error *. dx_error)
    +. (factor *. abs_float denominator_x)
    +. smallest
  and denominator_y_error =
    (abs_float ny *. dy_error)
    +. (abs_float dy *. ny_error)
    +. (ny_error *. dy_error)
    +. (factor *. abs_float denominator_y)
    +. smallest
  and denominator_z_error =
    (abs_float nz *. dz_error)
    +. (abs_float dz *. nz_error)
    +. (nz_error *. dz_error)
    +. (factor *. abs_float denominator_z)
    +. smallest
  and numerator_x_error =
    (abs_float nx *. wx_error)
    +. (abs_float wx *. nx_error)
    +. (nx_error *. wx_error)
    +. (factor *. abs_float numerator_x)
    +. smallest
  and numerator_y_error =
    (abs_float ny *. wy_error)
    +. (abs_float wy *. ny_error)
    +. (ny_error *. wy_error)
    +. (factor *. abs_float numerator_y)
    +. smallest
  and numerator_z_error =
    (abs_float nz *. wz_error)
    +. (abs_float wz *. nz_error)
    +. (nz_error *. wz_error)
    +. (factor *. abs_float numerator_z)
    +. smallest
  in
  let denominator = denominator_x +. denominator_y +. denominator_z
  and numerator = numerator_x +. numerator_y +. numerator_z in
  let denominator_error =
    upward
      (denominator_x_error +. denominator_y_error +. denominator_z_error
      +. (factor *. (abs_float denominator_x +. abs_float denominator_y +. abs_float denominator_z))
      +. smallest)
  and numerator_error =
    upward
      (numerator_x_error +. numerator_y_error +. numerator_z_error
      +. (factor *. (abs_float numerator_x +. abs_float numerator_y +. abs_float numerator_z))
      +. smallest)
  in
  let absolute_denominator = abs_float denominator in
  if
    (not
       (Float.is_finite denominator && Float.is_finite numerator
      && Float.is_finite denominator_error && Float.is_finite numerator_error))
    || absolute_denominator <= denominator_error
  then None
  else begin
    let parameter = numerator /. denominator
    and margin = absolute_denominator -. denominator_error in
    let parameter_error =
      upward
        (((numerator_error *. absolute_denominator) +. (abs_float numerator *. denominator_error))
         /. (absolute_denominator *. margin)
        +. (factor *. abs_float (numerator /. denominator))
        +. smallest)
    in
    let x_delta = dx *. parameter and y_delta = dy *. parameter and z_delta = dz *. parameter in
    let x_delta_error =
      (abs_float dx *. parameter_error)
      +. (abs_float parameter *. dx_error)
      +. (dx_error *. parameter_error)
      +. (factor *. abs_float x_delta)
      +. smallest
    and y_delta_error =
      (abs_float dy *. parameter_error)
      +. (abs_float parameter *. dy_error)
      +. (dy_error *. parameter_error)
      +. (factor *. abs_float y_delta)
      +. smallest
    and z_delta_error =
      (abs_float dz *. parameter_error)
      +. (abs_float parameter *. dz_error)
      +. (dz_error *. parameter_error)
      +. (factor *. abs_float z_delta)
      +. smallest
    in
    let raw_x = px +. x_delta and raw_y = py +. y_delta and raw_z = pz +. z_delta in
    let raw_x_error =
      upward (x_delta_error +. (factor *. (abs_float px +. abs_float x_delta)) +. smallest)
    and raw_y_error =
      upward (y_delta_error +. (factor *. (abs_float py +. abs_float y_delta)) +. smallest)
    and raw_z_error =
      upward (z_delta_error +. (factor *. (abs_float pz +. abs_float z_delta)) +. smallest)
    in
    (* An LPI coordinate is exactly constant when both line endpoints carry
       the same binary64 component. Preserve that singleton certificate; it
       avoids forcing exact homogeneous values while ordering axis-aligned
       seams. *)
    let approximate_x, x_error, x_lower, x_upper =
      if dx = 0. then (px, 0., px, px)
      else (raw_x, raw_x_error, downward (raw_x -. raw_x_error), upward (raw_x +. raw_x_error))
    and approximate_y, y_error, y_lower, y_upper =
      if dy = 0. then (py, 0., py, py)
      else (raw_y, raw_y_error, downward (raw_y -. raw_y_error), upward (raw_y +. raw_y_error))
    and approximate_z, z_error, z_lower, z_upper =
      if dz = 0. then (pz, 0., pz, pz)
      else (raw_z, raw_z_error, downward (raw_z -. raw_z_error), upward (raw_z +. raw_z_error))
    in
    if
      Float.is_finite approximate_x && Float.is_finite approximate_y
      && Float.is_finite approximate_z && Float.is_finite x_lower && Float.is_finite x_upper
      && Float.is_finite y_lower && Float.is_finite y_upper && Float.is_finite z_lower
      && Float.is_finite z_upper && Float.is_finite x_error && Float.is_finite y_error
      && Float.is_finite z_error
    then
      Some
        {
          source;
          construction;
          homogeneous = Atomic.make None;
          approximate_x;
          approximate_y;
          approximate_z;
          x_lower;
          x_upper;
          y_lower;
          y_upper;
          z_lower;
          z_upper;
          x_error;
          y_error;
          z_error;
        }
    else None
  end

let line_plane source ~line_start ~line_end ~plane_a ~plane_b ~plane_c =
  match first_invalid source [| line_start; line_end; plane_a; plane_b; plane_c |] with
  | Some index -> Error (Index_out_of_bounds index)
  | None -> (
      let construction = Line_plane { line_start; line_end; plane_a; plane_b; plane_c } in
      match
        try_line_plane_deferred source construction ~line_start ~line_end ~plane_a ~plane_b ~plane_c
      with
      | Some point -> Ok point
      | None ->
          let p = exact_at source line_start
          and q = exact_at source line_end
          and r = exact_at source plane_a
          and s = exact_at source plane_b
          and plane_t = exact_at source plane_c in
          let direction = subtract3 q p in
          let normal = cross (subtract3 s r) (subtract3 plane_t r) in
          let hw = dot normal direction in
          if Exact.is_zero hw then Error Parallel_line_and_plane
          else begin
            let line_parameter_numerator = dot normal (subtract3 r p) in
            let hx, hy, hz = add3 (scale3 hw p) (scale3 line_parameter_numerator direction) in
            make source construction { hx; hy; hz; hw }
          end)

let projected projection (x, y, z) =
  match projection with XY -> (x, y) | YZ -> (y, z) | ZX -> (z, x)

let line_line_homogeneous source projection first_start first_end second_start second_end =
  let p = exact_at source first_start
  and q = exact_at source first_end
  and r = exact_at source second_start
  and s = exact_at source second_end in
  let direction = subtract3 q p and other_direction = subtract3 s r in
  let dx, dy = projected projection direction
  and ox, oy = projected projection other_direction
  and rx, ry = projected projection (subtract3 r p) in
  let hw = Exact.subtract (Exact.multiply dx oy) (Exact.multiply dy ox) in
  if Exact.is_zero hw then Error Parallel_lines
  else begin
    let parameter = Exact.subtract (Exact.multiply rx oy) (Exact.multiply ry ox) in
    let hx, hy, hz = add3 (scale3 hw p) (scale3 parameter direction) in
    Ok (normalize { hx; hy; hz; hw })
  end

let try_line_line_deferred source construction projection first_start first_end second_start
    second_end =
  let first_plane, second_plane =
    match projection with
    | XY -> (source.x, source.y)
    | YZ -> (source.y, source.z)
    | ZX -> (source.z, source.x)
  in
  let pu = first_plane.(first_start)
  and pv = second_plane.(first_start)
  and qu = first_plane.(first_end)
  and qv = second_plane.(first_end)
  and ru = first_plane.(second_start)
  and rv = second_plane.(second_start)
  and su = first_plane.(second_end)
  and sv = second_plane.(second_end) in
  let factor = 8. *. Float.epsilon and smallest = Int64.float_of_bits 1L in
  let du = qu -. pu
  and dv = qv -. pv
  and ou = su -. ru
  and ov = sv -. rv
  and wu = ru -. pu
  and wv = rv -. pv in
  let du_error = (factor *. (abs_float qu +. abs_float pu)) +. smallest
  and dv_error = (factor *. (abs_float qv +. abs_float pv)) +. smallest
  and ou_error = (factor *. (abs_float su +. abs_float ru)) +. smallest
  and ov_error = (factor *. (abs_float sv +. abs_float rv)) +. smallest
  and wu_error = (factor *. (abs_float ru +. abs_float pu)) +. smallest
  and wv_error = (factor *. (abs_float rv +. abs_float pv)) +. smallest in
  let denominator_left = du *. ov
  and denominator_right = dv *. ou
  and numerator_left = wu *. ov
  and numerator_right = wv *. ou in
  let product_error left left_error right right_error product =
    (abs_float left *. right_error)
    +. (abs_float right *. left_error)
    +. (left_error *. right_error)
    +. (factor *. abs_float product)
    +. smallest
  in
  let denominator_left_error = product_error du du_error ov ov_error denominator_left
  and denominator_right_error = product_error dv dv_error ou ou_error denominator_right
  and numerator_left_error = product_error wu wu_error ov ov_error numerator_left
  and numerator_right_error = product_error wv wv_error ou ou_error numerator_right in
  let denominator = denominator_left -. denominator_right
  and numerator = numerator_left -. numerator_right in
  let denominator_error =
    upward
      (denominator_left_error +. denominator_right_error
      +. (factor *. (abs_float denominator_left +. abs_float denominator_right))
      +. smallest)
  and numerator_error =
    upward
      (numerator_left_error +. numerator_right_error
      +. (factor *. (abs_float numerator_left +. abs_float numerator_right))
      +. smallest)
  in
  let absolute_denominator = abs_float denominator in
  if
    (not
       (Float.is_finite denominator && Float.is_finite numerator
      && Float.is_finite denominator_error && Float.is_finite numerator_error))
    || absolute_denominator <= denominator_error
  then None
  else begin
    let parameter = numerator /. denominator
    and margin = absolute_denominator -. denominator_error in
    let parameter_error =
      upward
        (((numerator_error *. absolute_denominator) +. (abs_float numerator *. denominator_error))
         /. (absolute_denominator *. margin)
        +. (factor *. abs_float (numerator /. denominator))
        +. smallest)
    in
    let coordinate plane =
      let p = plane.(first_start) and q = plane.(first_end) in
      let direction = q -. p in
      let direction_error = (factor *. (abs_float q +. abs_float p)) +. smallest in
      let delta = direction *. parameter in
      let delta_error =
        (abs_float direction *. parameter_error)
        +. (abs_float parameter *. direction_error)
        +. (direction_error *. parameter_error)
        +. (factor *. abs_float delta)
        +. smallest
      in
      let raw = p +. delta in
      let raw_error =
        upward (delta_error +. (factor *. (abs_float p +. abs_float delta)) +. smallest)
      in
      if direction = 0. then (p, 0., p, p)
      else (raw, raw_error, downward (raw -. raw_error), upward (raw +. raw_error))
    in
    let approximate_x, x_error, x_lower, x_upper = coordinate source.x
    and approximate_y, y_error, y_lower, y_upper = coordinate source.y
    and approximate_z, z_error, z_lower, z_upper = coordinate source.z in
    if
      Float.is_finite approximate_x && Float.is_finite approximate_y
      && Float.is_finite approximate_z && Float.is_finite x_lower && Float.is_finite x_upper
      && Float.is_finite y_lower && Float.is_finite y_upper && Float.is_finite z_lower
      && Float.is_finite z_upper && Float.is_finite x_error && Float.is_finite y_error
      && Float.is_finite z_error
    then
      Some
        {
          source;
          construction;
          homogeneous = Atomic.make None;
          approximate_x;
          approximate_y;
          approximate_z;
          x_lower;
          x_upper;
          y_lower;
          y_upper;
          z_lower;
          z_upper;
          x_error;
          y_error;
          z_error;
        }
    else None
  end

let line_line source ~projection ~first_start ~first_end ~second_start ~second_end =
  match first_invalid source [| first_start; first_end; second_start; second_end |] with
  | Some index -> Error (Index_out_of_bounds index)
  | None -> (
      let construction =
        Line_line { projection; first_start; first_end; second_start; second_end }
      in
      match
        try_line_line_deferred source construction projection first_start first_end second_start
          second_end
      with
      | Some point -> Ok point
      | None -> (
          match
            line_line_homogeneous source projection first_start first_end second_start second_end
          with
          | Error _ as failure -> failure
          | Ok homogeneous -> make source construction homogeneous))

let plane source a b c =
  let p = exact_at source a in
  let normal = cross (subtract3 (exact_at source b) p) (subtract3 (exact_at source c) p) in
  (normal, dot normal p)

let triple_plane source ~first_a ~first_b ~first_c ~second_a ~second_b ~second_c ~third_a ~third_b
    ~third_c =
  match
    first_invalid source
      [| first_a; first_b; first_c; second_a; second_b; second_c; third_a; third_b; third_c |]
  with
  | Some index -> Error (Index_out_of_bounds index)
  | None ->
      let first_normal, first_constant = plane source first_a first_b first_c
      and second_normal, second_constant = plane source second_a second_b second_c
      and third_normal, third_constant = plane source third_a third_b third_c in
      let second_cross_third = cross second_normal third_normal
      and third_cross_first = cross third_normal first_normal
      and first_cross_second = cross first_normal second_normal in
      let hw = dot first_normal second_cross_third in
      if Exact.is_zero hw then Error Dependent_planes
      else begin
        let hx, hy, hz =
          add3
            (add3
               (scale3 first_constant second_cross_third)
               (scale3 second_constant third_cross_first))
            (scale3 third_constant first_cross_second)
        in
        make source
          (Triple_plane
             { first_a; first_b; first_c; second_a; second_b; second_c; third_a; third_b; third_c })
          { hx; hy; hz; hw }
      end

type rational = { numerator : Exact.t; denominator : Exact.t }

let rational_of_homogeneous component point =
  { numerator = component point; denominator = point.hw }

let rational_add left right =
  {
    numerator =
      Exact.add
        (Exact.multiply left.numerator right.denominator)
        (Exact.multiply right.numerator left.denominator);
    denominator = Exact.multiply left.denominator right.denominator;
  }

let rational_subtract left right =
  {
    numerator =
      Exact.subtract
        (Exact.multiply left.numerator right.denominator)
        (Exact.multiply right.numerator left.denominator);
    denominator = Exact.multiply left.denominator right.denominator;
  }

let rational_multiply left right =
  {
    numerator = Exact.multiply left.numerator right.numerator;
    denominator = Exact.multiply left.denominator right.denominator;
  }

let rational_divide left right =
  {
    numerator = Exact.multiply left.numerator right.denominator;
    denominator = Exact.multiply left.denominator right.numerator;
  }

let rational_scale value scalar = { value with numerator = Exact.multiply value.numerator scalar }

let rational_integer value =
  { numerator = Exact.of_float (float_of_int value); denominator = Exact.of_float 1. }

let rational_square value = rational_multiply value value

let circumcenter_homogeneous first second third =
  let ax = rational_of_homogeneous (fun point -> point.hx) first
  and ay = rational_of_homogeneous (fun point -> point.hy) first
  and az = rational_of_homogeneous (fun point -> point.hz) first
  and bx = rational_of_homogeneous (fun point -> point.hx) second
  and by = rational_of_homogeneous (fun point -> point.hy) second
  and bz = rational_of_homogeneous (fun point -> point.hz) second
  and cx = rational_of_homogeneous (fun point -> point.hx) third
  and cy = rational_of_homogeneous (fun point -> point.hy) third
  and cz = rational_of_homogeneous (fun point -> point.hz) third in
  let norm x y = rational_add (rational_square x) (rational_square y) in
  let an = norm ax ay and bn = norm bx by and cn = norm cx cy in
  let denominator =
    rational_scale
      (rational_add
         (rational_add
            (rational_multiply ax (rational_subtract by cy))
            (rational_multiply bx (rational_subtract cy ay)))
         (rational_multiply cx (rational_subtract ay by)))
      (Exact.of_float 2.)
  in
  if Exact.is_zero denominator.numerator then Error Degenerate_triangle
  else begin
    let ux =
      rational_divide
        (rational_add
           (rational_add
              (rational_multiply an (rational_subtract by cy))
              (rational_multiply bn (rational_subtract cy ay)))
           (rational_multiply cn (rational_subtract ay by)))
        denominator
    and uy =
      rational_divide
        (rational_add
           (rational_add
              (rational_multiply an (rational_subtract cx bx))
              (rational_multiply bn (rational_subtract ax cx)))
           (rational_multiply cn (rational_subtract bx ax)))
        denominator
    and uz = rational_divide (rational_add (rational_add az bz) cz) (rational_integer 3) in
    let xy_denominator = Exact.multiply ux.denominator uy.denominator in
    Ok
      (normalize
         {
           hx = Exact.multiply ux.numerator (Exact.multiply uy.denominator uz.denominator);
           hy = Exact.multiply uy.numerator (Exact.multiply ux.denominator uz.denominator);
           hz = Exact.multiply uz.numerator xy_denominator;
           hw = Exact.multiply xy_denominator uz.denominator;
         })
  end

let rec compute_homogeneous source = function
  | Explicit index ->
      let hx, hy, hz = exact_at source index in
      { hx; hy; hz; hw = Exact.of_float 1. }
  | Rounded { x; y; z } ->
      {
        hx = Exact.of_float x;
        hy = Exact.of_float y;
        hz = Exact.of_float z;
        hw = Exact.of_float 1.;
      }
  | Line_plane { line_start; line_end; plane_a; plane_b; plane_c } ->
      let p = exact_at source line_start
      and q = exact_at source line_end
      and r = exact_at source plane_a
      and s = exact_at source plane_b
      and plane_t = exact_at source plane_c in
      let direction = subtract3 q p in
      let normal = cross (subtract3 s r) (subtract3 plane_t r) in
      let hw = dot normal direction in
      assert (not (Exact.is_zero hw));
      let line_parameter_numerator = dot normal (subtract3 r p) in
      let hx, hy, hz = add3 (scale3 hw p) (scale3 line_parameter_numerator direction) in
      normalize { hx; hy; hz; hw }
  | Line_line { projection; first_start; first_end; second_start; second_end } -> (
      match
        line_line_homogeneous source projection first_start first_end second_start second_end
      with
      | Ok homogeneous -> homogeneous
      | Error _ -> assert false)
  | Triple_plane
      { first_a; first_b; first_c; second_a; second_b; second_c; third_a; third_b; third_c } ->
      let first_normal, first_constant = plane source first_a first_b first_c
      and second_normal, second_constant = plane source second_a second_b second_c
      and third_normal, third_constant = plane source third_a third_b third_c in
      let second_cross_third = cross second_normal third_normal
      and third_cross_first = cross third_normal first_normal
      and first_cross_second = cross first_normal second_normal in
      let hw = dot first_normal second_cross_third in
      assert (not (Exact.is_zero hw));
      let hx, hy, hz =
        add3
          (add3
             (scale3 first_constant second_cross_third)
             (scale3 second_constant third_cross_first))
          (scale3 third_constant first_cross_second)
      in
      normalize { hx; hy; hz; hw }
  | Midpoint { first; second } ->
      let first = exact_homogeneous first and second = exact_homogeneous second in
      normalize
        {
          hx = Exact.add (Exact.multiply first.hx second.hw) (Exact.multiply second.hx first.hw);
          hy = Exact.add (Exact.multiply first.hy second.hw) (Exact.multiply second.hy first.hw);
          hz = Exact.add (Exact.multiply first.hz second.hw) (Exact.multiply second.hz first.hw);
          hw = Exact.multiply (Exact.of_float 2.) (Exact.multiply first.hw second.hw);
        }
  | Centroid3 { first; second; third } ->
      let first = exact_homogeneous first
      and second = exact_homogeneous second
      and third = exact_homogeneous third in
      let second_third = Exact.multiply second.hw third.hw
      and first_third = Exact.multiply first.hw third.hw
      and first_second = Exact.multiply first.hw second.hw in
      let component first_component second_component third_component =
        Exact.add
          (Exact.add
             (Exact.multiply first_component second_third)
             (Exact.multiply second_component first_third))
          (Exact.multiply third_component first_second)
      in
      normalize
        {
          hx = component first.hx second.hx third.hx;
          hy = component first.hy second.hy third.hy;
          hz = component first.hz second.hz third.hz;
          hw = Exact.multiply (Exact.of_float 3.) (Exact.multiply first.hw second_third);
        }
  | Circumcenter2_xy { first; second; third } -> (
      match
        circumcenter_homogeneous (exact_homogeneous first) (exact_homogeneous second)
          (exact_homogeneous third)
      with
      | Ok value -> value
      | Error _ -> assert false)

and exact_homogeneous point =
  match Atomic.get point.homogeneous with
  | Some homogeneous -> homogeneous
  | None ->
      let homogeneous = compute_homogeneous point.source point.construction in
      let cached = Some homogeneous in
      if Atomic.compare_and_set point.homogeneous None cached then homogeneous
      else Option.get (Atomic.get point.homogeneous)

let centroid3 first second third =
  if first.source != second.source || first.source != third.source then Error Point_source_mismatch
  else begin
    let construction = Centroid3 { first; second; third } in
    make first.source construction (compute_homogeneous first.source construction)
  end

let midpoint first second =
  if first.source != second.source then Error Point_source_mismatch
  else begin
    let construction = Midpoint { first; second } in
    make first.source construction (compute_homogeneous first.source construction)
  end

let approximate point = (point.approximate_x, point.approximate_y, point.approximate_z)

let bounds point =
  ((point.x_lower, point.x_upper), (point.y_lower, point.y_upper), (point.z_lower, point.z_upper))

let ensure_same_source left right =
  if left.source != right.source then
    invalid_arg "Rdk implicit points belong to different coordinate sources"

let diametral_dot_xy ~first ~second point =
  ensure_same_source first second;
  ensure_same_source first point;
  let first = exact_homogeneous first
  and second = exact_homogeneous second
  and point = exact_homogeneous point in
  let difference component endpoint =
    Exact.subtract
      (Exact.multiply (component point) endpoint.hw)
      (Exact.multiply (component endpoint) point.hw)
  in
  let pfx = difference (fun value -> value.hx) first
  and pfy = difference (fun value -> value.hy) first
  and psx = difference (fun value -> value.hx) second
  and psy = difference (fun value -> value.hy) second in
  let value = Exact.add (Exact.multiply pfx psx) (Exact.multiply pfy psy) in
  let sign = Exact.compare_zero value in
  if sign < 0 then Predicates.Negative
  else if sign > 0 then Predicates.Positive
  else Predicates.Zero

let compare_component component lower upper left right =
  if left == right then 0
  else begin
    ensure_same_source left right;
    if upper left < lower right then -1
    else if lower left > upper right then 1
    else if lower left = upper left && lower right = upper right && lower left = lower right then 0
    else begin
      let left_h = exact_homogeneous left and right_h = exact_homogeneous right in
      Exact.Scratch.compare_products (component left_h) right_h.hw (component right_h) left_h.hw
    end
  end

let compare_x =
  compare_component
    (fun point -> point.hx)
    (fun point -> point.x_lower)
    (fun point -> point.x_upper)

let compare_y =
  compare_component
    (fun point -> point.hy)
    (fun point -> point.y_lower)
    (fun point -> point.y_upper)

let compare_z =
  compare_component
    (fun point -> point.hz)
    (fun point -> point.z_lower)
    (fun point -> point.z_upper)

let equal left right =
  left == right || (compare_x left right = 0 && compare_y left right = 0 && compare_z left right = 0)

let sign_of_comparison comparison =
  if comparison < 0 then Predicates.Negative
  else if comparison > 0 then Predicates.Positive
  else Predicates.Zero

let source_triangle_projection source a b c =
  if not (valid_index source a && valid_index source b && valid_index source c) then
    invalid_arg "implicit source triangle index is out of bounds";
  if
    Predicates.orient2d ~ax:source.x.(a) ~ay:source.y.(a) ~bx:source.x.(b) ~by:source.y.(b)
      ~cx:source.x.(c) ~cy:source.y.(c)
    <> Predicates.Zero
  then XY
  else if
    Predicates.orient2d ~ax:source.y.(a) ~ay:source.z.(a) ~bx:source.y.(b) ~by:source.z.(b)
      ~cx:source.y.(c) ~cy:source.z.(c)
    <> Predicates.Zero
  then YZ
  else if
    Predicates.orient2d ~ax:source.z.(a) ~ay:source.x.(a) ~bx:source.z.(b) ~by:source.x.(b)
      ~cx:source.z.(c) ~cy:source.x.(c)
    <> Predicates.Zero
  then ZX
  else invalid_arg "implicit source triangle is exactly degenerate"

let compare_homogeneous_float_bound numerator denominator lower upper value =
  if upper < value then -1
  else if lower > value then 1
  else Exact.Scratch.compare_homogeneous_float ~numerator ~denominator ~value

let between_homogeneous numerator denominator lower upper first second =
  if first <= second then
    compare_homogeneous_float_bound numerator denominator lower upper first >= 0
    && compare_homogeneous_float_bound numerator denominator lower upper second <= 0
  else
    compare_homogeneous_float_bound numerator denominator lower upper second >= 0
    && compare_homogeneous_float_bound numerator denominator lower upper first <= 0

let source_segment_contains source ~projection ~first ~second point =
  if not (valid_index source first && valid_index source second) then
    invalid_arg "implicit source segment index is out of bounds";
  if point.source != source then
    invalid_arg "Rdk implicit point and source segment belong to different sources";
  let homogeneous = exact_homogeneous point in
  let orientation =
    match projection with
    | XY ->
        Exact.Scratch.orient2d_ee_homogeneous ~ax:source.x.(first) ~ay:source.y.(first)
          ~bx:source.x.(second) ~by:source.y.(second) ~cx:homogeneous.hx ~cy:homogeneous.hy
          ~cw:homogeneous.hw
    | YZ ->
        Exact.Scratch.orient2d_ee_homogeneous ~ax:source.y.(first) ~ay:source.z.(first)
          ~bx:source.y.(second) ~by:source.z.(second) ~cx:homogeneous.hy ~cy:homogeneous.hz
          ~cw:homogeneous.hw
    | ZX ->
        Exact.Scratch.orient2d_ee_homogeneous ~ax:source.z.(first) ~ay:source.x.(first)
          ~bx:source.z.(second) ~by:source.x.(second) ~cx:homogeneous.hz ~cy:homogeneous.hx
          ~cw:homogeneous.hw
  in
  if orientation <> 0 then false
  else begin
    if source.x.(first) <> source.x.(second) then
      between_homogeneous homogeneous.hx homogeneous.hw point.x_lower point.x_upper source.x.(first)
        source.x.(second)
    else if source.y.(first) <> source.y.(second) then
      between_homogeneous homogeneous.hy homogeneous.hw point.y_lower point.y_upper source.y.(first)
        source.y.(second)
    else
      between_homogeneous homogeneous.hz homogeneous.hw point.z_lower point.z_upper source.z.(first)
        source.z.(second)
  end

let barycentric_source_triangle source ~a ~b ~c point =
  if not (valid_index source a && valid_index source b && valid_index source c) then
    invalid_arg "implicit source triangle index is out of bounds";
  if point.source != source then
    invalid_arg "Rdk implicit point and source triangle belong to different sources";
  let homogeneous = exact_homogeneous point in
  Exact.Scratch.barycentric_tri_homogeneous ~ax:source.x.(a) ~ay:source.y.(a) ~az:source.z.(a)
    ~bx:source.x.(b) ~by:source.y.(b) ~bz:source.z.(b) ~cx:source.x.(c) ~cy:source.y.(c)
    ~cz:source.z.(c) ~px:homogeneous.hx ~py:homogeneous.hy ~pz:homogeneous.hz ~pw:homogeneous.hw

let x point = point.hx
let y point = point.hy
let z point = point.hz

let exact_orient2d_components first_component second_component a b c =
  let ah = exact_homogeneous a and bh = exact_homogeneous b and ch = exact_homogeneous c in
  Exact.Scratch.orient2d_homogeneous ~ax:(first_component ah) ~ay:(second_component ah) ~aw:ah.hw
    ~bx:(first_component bh) ~by:(second_component bh) ~bw:bh.hw ~cx:(first_component ch)
    ~cy:(second_component ch) ~cw:ch.hw

let radial_dot_arena_exact edge_start edge_end left right =
  ensure_same_source edge_start edge_end;
  ensure_same_source edge_start left;
  ensure_same_source edge_start right;
  let edge_start = exact_homogeneous edge_start
  and edge_end = exact_homogeneous edge_end
  and left = exact_homogeneous left
  and right = exact_homogeneous right in
  sign_of_comparison
    (Exact.Scratch.radial_dot_homogeneous ~ex0:edge_start.hx ~ey0:edge_start.hy ~ez0:edge_start.hz
       ~ew0:edge_start.hw ~ex1:edge_end.hx ~ey1:edge_end.hy ~ez1:edge_end.hz ~ew1:edge_end.hw
       ~lx:left.hx ~ly:left.hy ~lz:left.hz ~lw:left.hw ~rx:right.hx ~ry:right.hy ~rz:right.hz
       ~rw:right.hw)

let radial_dot edge_start edge_end left right =
  ensure_same_source edge_start edge_end;
  ensure_same_source edge_start left;
  ensure_same_source edge_start right;
  let factor = 16. *. Float.epsilon and smallest = Int64.float_of_bits 1L in
  let ex = edge_end.approximate_x -. edge_start.approximate_x
  and ey = edge_end.approximate_y -. edge_start.approximate_y
  and ez = edge_end.approximate_z -. edge_start.approximate_z
  and lx = left.approximate_x -. edge_start.approximate_x
  and ly = left.approximate_y -. edge_start.approximate_y
  and lz = left.approximate_z -. edge_start.approximate_z
  and rx = right.approximate_x -. edge_start.approximate_x
  and ry = right.approximate_y -. edge_start.approximate_y
  and rz = right.approximate_z -. edge_start.approximate_z in
  let difference_error first first_error second second_error =
    first_error +. second_error +. (factor *. (abs_float first +. abs_float second)) +. smallest
  in
  let ex_error =
    difference_error edge_end.approximate_x edge_end.x_error edge_start.approximate_x
      edge_start.x_error
  and ey_error =
    difference_error edge_end.approximate_y edge_end.y_error edge_start.approximate_y
      edge_start.y_error
  and ez_error =
    difference_error edge_end.approximate_z edge_end.z_error edge_start.approximate_z
      edge_start.z_error
  and lx_error =
    difference_error left.approximate_x left.x_error edge_start.approximate_x edge_start.x_error
  and ly_error =
    difference_error left.approximate_y left.y_error edge_start.approximate_y edge_start.y_error
  and lz_error =
    difference_error left.approximate_z left.z_error edge_start.approximate_z edge_start.z_error
  and rx_error =
    difference_error right.approximate_x right.x_error edge_start.approximate_x edge_start.x_error
  and ry_error =
    difference_error right.approximate_y right.y_error edge_start.approximate_y edge_start.y_error
  and rz_error =
    difference_error right.approximate_z right.z_error edge_start.approximate_z edge_start.z_error
  in
  let product_error first first_error second second_error product =
    (abs_float first *. second_error)
    +. (abs_float second *. first_error)
    +. (first_error *. second_error)
    +. (factor *. abs_float product)
    +. smallest
  in
  let dot_error ax ax_error ay ay_error az az_error bx bx_error by by_error bz bz_error px py pz =
    product_error ax ax_error bx bx_error px
    +. product_error ay ay_error by by_error py
    +. product_error az az_error bz bz_error pz
    +. (factor *. (abs_float px +. abs_float py +. abs_float pz))
    +. smallest
  in
  let lr_x = lx *. rx
  and lr_y = ly *. ry
  and lr_z = lz *. rz
  and ee_x = ex *. ex
  and ee_y = ey *. ey
  and ee_z = ez *. ez
  and le_x = lx *. ex
  and le_y = ly *. ey
  and le_z = lz *. ez
  and re_x = rx *. ex
  and re_y = ry *. ey
  and re_z = rz *. ez in
  let lr = lr_x +. lr_y +. lr_z
  and ee = ee_x +. ee_y +. ee_z
  and le = le_x +. le_y +. le_z
  and re = re_x +. re_y +. re_z in
  let lr_error =
    dot_error lx lx_error ly ly_error lz lz_error rx rx_error ry ry_error rz rz_error lr_x lr_y lr_z
  and ee_error =
    dot_error ex ex_error ey ey_error ez ez_error ex ex_error ey ey_error ez ez_error ee_x ee_y ee_z
  and le_error =
    dot_error lx lx_error ly ly_error lz lz_error ex ex_error ey ey_error ez ez_error le_x le_y le_z
  and re_error =
    dot_error rx rx_error ry ry_error rz rz_error ex ex_error ey ey_error ez ez_error re_x re_y re_z
  in
  let first = lr *. ee and second = le *. re in
  let first_error = product_error lr lr_error ee ee_error first
  and second_error = product_error le le_error re re_error second in
  let determinant = first -. second in
  let determinant_error =
    upward
      (first_error +. second_error +. (factor *. (abs_float first +. abs_float second)) +. smallest)
  in
  if
    Float.is_finite determinant && Float.is_finite determinant_error
    && abs_float determinant > determinant_error
  then if determinant < 0. then Predicates.Negative else Predicates.Positive
  else radial_dot_arena_exact edge_start edge_end left right

let normal_dot_symbolic first second third =
  ensure_same_source first second;
  ensure_same_source first third;
  let first = exact_homogeneous first
  and second = exact_homogeneous second
  and third = exact_homogeneous third in
  sign_of_comparison
    (Exact.Scratch.normal_dot_symbolic_homogeneous ~fx:first.hx ~fy:first.hy ~fz:first.hz
       ~fw:first.hw ~sx:second.hx ~sy:second.hy ~sz:second.hz ~sw:second.hw ~tx:third.hx
       ~ty:third.hy ~tz:third.hz ~tw:third.hw)

type symbolic_ray_triangle =
  | Symbolic_miss
  | Symbolic_hit of Predicates.sign
  | Symbolic_origin_boundary
  | Symbolic_degenerate

type axis_ray_triangle = Axis_miss | Axis_hit of Predicates.sign | Axis_boundary | Axis_parallel

let symbolic_ray_source_triangle source ~a ~b ~c query =
  if not (valid_index source a && valid_index source b && valid_index source c) then
    invalid_arg "implicit symbolic source triangle index is out of bounds";
  if query.source != source then
    invalid_arg "Rdk implicit point and symbolic source triangle belong to different sources";
  let query = exact_homogeneous query in
  match
    Exact.Scratch.symbolic_ray_tri_homogeneous ~ax:source.x.(a) ~ay:source.y.(a) ~az:source.z.(a)
      ~bx:source.x.(b) ~by:source.y.(b) ~bz:source.z.(b) ~cx:source.x.(c) ~cy:source.y.(c)
      ~cz:source.z.(c) ~qx:query.hx ~qy:query.hy ~qz:query.hz ~qw:query.hw
  with
  | 0 -> Symbolic_miss
  | 1 -> Symbolic_hit Predicates.Negative
  | 2 -> Symbolic_hit Predicates.Positive
  | 3 -> Symbolic_origin_boundary
  | 4 -> Symbolic_degenerate
  | _ -> assert false

let filter_subtraction_error left right right_error =
  right_error
  +. (4. *. Float.epsilon *. (abs_float left +. abs_float right))
  +. Int64.float_of_bits 1L

let filter_product_error left left_error right right_error product =
  (abs_float left *. right_error)
  +. (abs_float right *. left_error)
  +. (left_error *. right_error)
  +. (4. *. Float.epsilon *. abs_float product)
  +. Int64.float_of_bits 1L

let filtered_projected_edge au av bu bv qu qv qu_error qv_error =
  let acu = au -. qu and bcu = bu -. qu and acv = av -. qv and bcv = bv -. qv in
  let acu_error = filter_subtraction_error au qu qu_error
  and bcu_error = filter_subtraction_error bu qu qu_error
  and acv_error = filter_subtraction_error av qv qv_error
  and bcv_error = filter_subtraction_error bv qv qv_error in
  let left = acu *. bcv and right = acv *. bcu in
  let left_error = filter_product_error acu acu_error bcv bcv_error left
  and right_error = filter_product_error acv acv_error bcu bcu_error right in
  let determinant = left -. right in
  let error =
    upward
      (left_error +. right_error
      +. (4. *. Float.epsilon *. (abs_float left +. abs_float right))
      +. Int64.float_of_bits 1L)
  in
  if Float.is_finite determinant && Float.is_finite error && abs_float determinant > error then
    if determinant < 0. then -1 else 1
  else 0

let filtered_axis_edge source query axis first second =
  if axis = 0 then
    filtered_projected_edge source.y.(first) source.z.(first) source.y.(second) source.z.(second)
      query.approximate_y query.approximate_z query.y_error query.z_error
  else if axis = 1 then
    filtered_projected_edge source.z.(first) source.x.(first) source.z.(second) source.x.(second)
      query.approximate_z query.approximate_x query.z_error query.x_error
  else
    filtered_projected_edge source.x.(first) source.y.(first) source.x.(second) source.y.(second)
      query.approximate_x query.approximate_y query.x_error query.y_error

let filtered_orient3_explicit_query source a b c query =
  let adx = source.x.(a) -. query.approximate_x
  and ady = source.y.(a) -. query.approximate_y
  and adz = source.z.(a) -. query.approximate_z
  and bdx = source.x.(b) -. query.approximate_x
  and bdy = source.y.(b) -. query.approximate_y
  and bdz = source.z.(b) -. query.approximate_z
  and cdx = source.x.(c) -. query.approximate_x
  and cdy = source.y.(c) -. query.approximate_y
  and cdz = source.z.(c) -. query.approximate_z in
  let adx_error = filter_subtraction_error source.x.(a) query.approximate_x query.x_error
  and ady_error = filter_subtraction_error source.y.(a) query.approximate_y query.y_error
  and adz_error = filter_subtraction_error source.z.(a) query.approximate_z query.z_error
  and bdx_error = filter_subtraction_error source.x.(b) query.approximate_x query.x_error
  and bdy_error = filter_subtraction_error source.y.(b) query.approximate_y query.y_error
  and bdz_error = filter_subtraction_error source.z.(b) query.approximate_z query.z_error
  and cdx_error = filter_subtraction_error source.x.(c) query.approximate_x query.x_error
  and cdy_error = filter_subtraction_error source.y.(c) query.approximate_y query.y_error
  and cdz_error = filter_subtraction_error source.z.(c) query.approximate_z query.z_error in
  let first_left = bdy *. cdz
  and first_right = bdz *. cdy
  and second_left = bdz *. cdx
  and second_right = bdx *. cdz
  and third_left = bdx *. cdy
  and third_right = bdy *. cdx in
  let first_left_error = filter_product_error bdy bdy_error cdz cdz_error first_left
  and first_right_error = filter_product_error bdz bdz_error cdy cdy_error first_right
  and second_left_error = filter_product_error bdz bdz_error cdx cdx_error second_left
  and second_right_error = filter_product_error bdx bdx_error cdz cdz_error second_right
  and third_left_error = filter_product_error bdx bdx_error cdy cdy_error third_left
  and third_right_error = filter_product_error bdy bdy_error cdx cdx_error third_right in
  let first_minor = first_left -. first_right
  and second_minor = second_left -. second_right
  and third_minor = third_left -. third_right in
  let factor = 4. *. Float.epsilon and smallest = Int64.float_of_bits 1L in
  let first_minor_error =
    first_left_error +. first_right_error
    +. (factor *. (abs_float first_left +. abs_float first_right))
    +. smallest
  and second_minor_error =
    second_left_error +. second_right_error
    +. (factor *. (abs_float second_left +. abs_float second_right))
    +. smallest
  and third_minor_error =
    third_left_error +. third_right_error
    +. (factor *. (abs_float third_left +. abs_float third_right))
    +. smallest
  in
  let first = adx *. first_minor and second = ady *. second_minor and third = adz *. third_minor in
  let first_error = filter_product_error adx adx_error first_minor first_minor_error first
  and second_error = filter_product_error ady ady_error second_minor second_minor_error second
  and third_error = filter_product_error adz adz_error third_minor third_minor_error third in
  let determinant = first +. second +. third in
  let error =
    upward
      (first_error +. second_error +. third_error
      +. (factor *. (abs_float first +. abs_float second +. abs_float third))
      +. smallest)
  in
  if Float.is_finite determinant && Float.is_finite error && abs_float determinant > error then
    if determinant < 0. then -1 else 1
  else 0

let exact_axis_ray_source_triangle source ~a ~b ~c ~axis ~positive query =
  let query = exact_homogeneous query in
  match
    Exact.Scratch.axis_ray_tri_homogeneous ~axis ~positive ~ax:source.x.(a) ~ay:source.y.(a)
      ~az:source.z.(a) ~bx:source.x.(b) ~by:source.y.(b) ~bz:source.z.(b) ~cx:source.x.(c)
      ~cy:source.y.(c) ~cz:source.z.(c) ~qx:query.hx ~qy:query.hy ~qz:query.hz ~qw:query.hw
  with
  | 0 -> Axis_miss
  | 1 -> Axis_hit Predicates.Negative
  | 2 -> Axis_hit Predicates.Positive
  | 3 -> Axis_boundary
  | 4 -> Axis_parallel
  | _ -> assert false

let axis_ray_source_triangle source ~a ~b ~c ~axis ~positive query =
  if not (valid_index source a && valid_index source b && valid_index source c) then
    invalid_arg "implicit axis-ray source triangle index is out of bounds";
  if query.source != source then
    invalid_arg "Rdk implicit point and axis-ray source triangle belong to different sources";
  if axis < 0 || axis > 2 then invalid_arg "Rdk exact ray axis is out of bounds";
  let first_plane, second_plane =
    if axis = 0 then (source.y, source.z)
    else if axis = 1 then (source.z, source.x)
    else (source.x, source.y)
  in
  let normal =
    Predicates.orient2d ~ax:first_plane.(a) ~ay:second_plane.(a) ~bx:first_plane.(b)
      ~by:second_plane.(b) ~cx:first_plane.(c) ~cy:second_plane.(c)
  in
  if normal = Predicates.Zero then Axis_parallel
  else begin
    let first = filtered_axis_edge source query axis a b
    and second = filtered_axis_edge source query axis b c
    and third = filtered_axis_edge source query axis c a
    and plane = filtered_orient3_explicit_query source a b c query in
    if first = 0 || second = 0 || third = 0 || plane = 0 then
      exact_axis_ray_source_triangle source ~a ~b ~c ~axis ~positive query
    else begin
      let direction = if positive then 1 else -1 in
      let normal = (if normal = Predicates.Positive then 1 else -1) * direction
      and first = first * direction
      and second = second * direction
      and third = third * direction in
      if first <> normal || second <> normal || third <> normal then Axis_miss
      else if plane <> normal then Axis_miss
      else Axis_hit (if normal < 0 then Predicates.Negative else Predicates.Positive)
    end
  end

let minimum_subnormal = Int64.float_of_bits 1L

let orient2d_xy a b c =
  ensure_same_source a b;
  ensure_same_source a c;
  let acx = a.approximate_x -. c.approximate_x
  and bcx = b.approximate_x -. c.approximate_x
  and acy = a.approximate_y -. c.approximate_y
  and bcy = b.approximate_y -. c.approximate_y in
  let subtraction_factor = 4. *. Float.epsilon in
  let acx_error =
    a.x_error +. c.x_error
    +. (subtraction_factor *. (abs_float a.approximate_x +. abs_float c.approximate_x))
    +. minimum_subnormal
  and bcx_error =
    b.x_error +. c.x_error
    +. (subtraction_factor *. (abs_float b.approximate_x +. abs_float c.approximate_x))
    +. minimum_subnormal
  and acy_error =
    a.y_error +. c.y_error
    +. (subtraction_factor *. (abs_float a.approximate_y +. abs_float c.approximate_y))
    +. minimum_subnormal
  and bcy_error =
    b.y_error +. c.y_error
    +. (subtraction_factor *. (abs_float b.approximate_y +. abs_float c.approximate_y))
    +. minimum_subnormal
  in
  let left = acx *. bcy and right = acy *. bcx in
  let product_factor = 4. *. Float.epsilon in
  let left_error =
    (abs_float acx *. bcy_error)
    +. (abs_float bcy *. acx_error)
    +. (acx_error *. bcy_error)
    +. (product_factor *. abs_float left)
    +. minimum_subnormal
  and right_error =
    (abs_float acy *. bcx_error)
    +. (abs_float bcx *. acy_error)
    +. (acy_error *. bcx_error)
    +. (product_factor *. abs_float right)
    +. minimum_subnormal
  in
  let determinant = left -. right in
  let error =
    upward
      (left_error +. right_error
      +. (product_factor *. (abs_float left +. abs_float right))
      +. minimum_subnormal)
  in
  if Float.is_finite determinant && Float.is_finite error && abs_float determinant > error then
    if determinant < 0. then Predicates.Negative else Predicates.Positive
  else begin
    sign_of_comparison (exact_orient2d_components x y a b c)
  end

let orient2d_yz a b c =
  ensure_same_source a b;
  ensure_same_source a c;
  let acy = a.approximate_y -. c.approximate_y
  and bcy = b.approximate_y -. c.approximate_y
  and acz = a.approximate_z -. c.approximate_z
  and bcz = b.approximate_z -. c.approximate_z in
  let subtraction_factor = 4. *. Float.epsilon in
  let acy_error =
    a.y_error +. c.y_error
    +. (subtraction_factor *. (abs_float a.approximate_y +. abs_float c.approximate_y))
    +. minimum_subnormal
  and bcy_error =
    b.y_error +. c.y_error
    +. (subtraction_factor *. (abs_float b.approximate_y +. abs_float c.approximate_y))
    +. minimum_subnormal
  and acz_error =
    a.z_error +. c.z_error
    +. (subtraction_factor *. (abs_float a.approximate_z +. abs_float c.approximate_z))
    +. minimum_subnormal
  and bcz_error =
    b.z_error +. c.z_error
    +. (subtraction_factor *. (abs_float b.approximate_z +. abs_float c.approximate_z))
    +. minimum_subnormal
  in
  let left = acy *. bcz and right = acz *. bcy in
  let product_factor = 4. *. Float.epsilon in
  let left_error =
    (abs_float acy *. bcz_error)
    +. (abs_float bcz *. acy_error)
    +. (acy_error *. bcz_error)
    +. (product_factor *. abs_float left)
    +. minimum_subnormal
  and right_error =
    (abs_float acz *. bcy_error)
    +. (abs_float bcy *. acz_error)
    +. (acz_error *. bcy_error)
    +. (product_factor *. abs_float right)
    +. minimum_subnormal
  in
  let determinant = left -. right in
  let error =
    upward
      (left_error +. right_error
      +. (product_factor *. (abs_float left +. abs_float right))
      +. minimum_subnormal)
  in
  if Float.is_finite determinant && Float.is_finite error && abs_float determinant > error then
    if determinant < 0. then Predicates.Negative else Predicates.Positive
  else begin
    sign_of_comparison (exact_orient2d_components y z a b c)
  end

let orient2d_zx a b c =
  ensure_same_source a b;
  ensure_same_source a c;
  let acz = a.approximate_z -. c.approximate_z
  and bcz = b.approximate_z -. c.approximate_z
  and acx = a.approximate_x -. c.approximate_x
  and bcx = b.approximate_x -. c.approximate_x in
  let subtraction_factor = 4. *. Float.epsilon in
  let acz_error =
    a.z_error +. c.z_error
    +. (subtraction_factor *. (abs_float a.approximate_z +. abs_float c.approximate_z))
    +. minimum_subnormal
  and bcz_error =
    b.z_error +. c.z_error
    +. (subtraction_factor *. (abs_float b.approximate_z +. abs_float c.approximate_z))
    +. minimum_subnormal
  and acx_error =
    a.x_error +. c.x_error
    +. (subtraction_factor *. (abs_float a.approximate_x +. abs_float c.approximate_x))
    +. minimum_subnormal
  and bcx_error =
    b.x_error +. c.x_error
    +. (subtraction_factor *. (abs_float b.approximate_x +. abs_float c.approximate_x))
    +. minimum_subnormal
  in
  let left = acz *. bcx and right = acx *. bcz in
  let product_factor = 4. *. Float.epsilon in
  let left_error =
    (abs_float acz *. bcx_error)
    +. (abs_float bcx *. acz_error)
    +. (acz_error *. bcx_error)
    +. (product_factor *. abs_float left)
    +. minimum_subnormal
  and right_error =
    (abs_float acx *. bcz_error)
    +. (abs_float bcz *. acx_error)
    +. (acx_error *. bcz_error)
    +. (product_factor *. abs_float right)
    +. minimum_subnormal
  in
  let determinant = left -. right in
  let error =
    upward
      (left_error +. right_error
      +. (product_factor *. (abs_float left +. abs_float right))
      +. minimum_subnormal)
  in
  if Float.is_finite determinant && Float.is_finite error && abs_float determinant > error then
    if determinant < 0. then Predicates.Negative else Predicates.Positive
  else begin
    sign_of_comparison (exact_orient2d_components z x a b c)
  end

let orient3d a b c d =
  ensure_same_source a b;
  ensure_same_source a c;
  ensure_same_source a d;
  let adx = a.approximate_x -. d.approximate_x
  and ady = a.approximate_y -. d.approximate_y
  and adz = a.approximate_z -. d.approximate_z
  and bdx = b.approximate_x -. d.approximate_x
  and bdy = b.approximate_y -. d.approximate_y
  and bdz = b.approximate_z -. d.approximate_z
  and cdx = c.approximate_x -. d.approximate_x
  and cdy = c.approximate_y -. d.approximate_y
  and cdz = c.approximate_z -. d.approximate_z in
  let subtraction_factor = 4. *. Float.epsilon in
  let adx_error =
    a.x_error +. d.x_error
    +. (subtraction_factor *. (abs_float a.approximate_x +. abs_float d.approximate_x))
    +. minimum_subnormal
  and ady_error =
    a.y_error +. d.y_error
    +. (subtraction_factor *. (abs_float a.approximate_y +. abs_float d.approximate_y))
    +. minimum_subnormal
  and adz_error =
    a.z_error +. d.z_error
    +. (subtraction_factor *. (abs_float a.approximate_z +. abs_float d.approximate_z))
    +. minimum_subnormal
  and bdx_error =
    b.x_error +. d.x_error
    +. (subtraction_factor *. (abs_float b.approximate_x +. abs_float d.approximate_x))
    +. minimum_subnormal
  and bdy_error =
    b.y_error +. d.y_error
    +. (subtraction_factor *. (abs_float b.approximate_y +. abs_float d.approximate_y))
    +. minimum_subnormal
  and bdz_error =
    b.z_error +. d.z_error
    +. (subtraction_factor *. (abs_float b.approximate_z +. abs_float d.approximate_z))
    +. minimum_subnormal
  and cdx_error =
    c.x_error +. d.x_error
    +. (subtraction_factor *. (abs_float c.approximate_x +. abs_float d.approximate_x))
    +. minimum_subnormal
  and cdy_error =
    c.y_error +. d.y_error
    +. (subtraction_factor *. (abs_float c.approximate_y +. abs_float d.approximate_y))
    +. minimum_subnormal
  and cdz_error =
    c.z_error +. d.z_error
    +. (subtraction_factor *. (abs_float c.approximate_z +. abs_float d.approximate_z))
    +. minimum_subnormal
  in
  let product_factor = 4. *. Float.epsilon in
  let first_left = bdy *. cdz
  and first_right = bdz *. cdy
  and second_left = bdz *. cdx
  and second_right = bdx *. cdz
  and third_left = bdx *. cdy
  and third_right = bdy *. cdx in
  let first_left_error =
    (abs_float bdy *. cdz_error)
    +. (abs_float cdz *. bdy_error)
    +. (bdy_error *. cdz_error)
    +. (product_factor *. abs_float first_left)
    +. minimum_subnormal
  and first_right_error =
    (abs_float bdz *. cdy_error)
    +. (abs_float cdy *. bdz_error)
    +. (bdz_error *. cdy_error)
    +. (product_factor *. abs_float first_right)
    +. minimum_subnormal
  and second_left_error =
    (abs_float bdz *. cdx_error)
    +. (abs_float cdx *. bdz_error)
    +. (bdz_error *. cdx_error)
    +. (product_factor *. abs_float second_left)
    +. minimum_subnormal
  and second_right_error =
    (abs_float bdx *. cdz_error)
    +. (abs_float cdz *. bdx_error)
    +. (bdx_error *. cdz_error)
    +. (product_factor *. abs_float second_right)
    +. minimum_subnormal
  and third_left_error =
    (abs_float bdx *. cdy_error)
    +. (abs_float cdy *. bdx_error)
    +. (bdx_error *. cdy_error)
    +. (product_factor *. abs_float third_left)
    +. minimum_subnormal
  and third_right_error =
    (abs_float bdy *. cdx_error)
    +. (abs_float cdx *. bdy_error)
    +. (bdy_error *. cdx_error)
    +. (product_factor *. abs_float third_right)
    +. minimum_subnormal
  in
  let first_minor = first_left -. first_right
  and second_minor = second_left -. second_right
  and third_minor = third_left -. third_right in
  let first_minor_error =
    first_left_error +. first_right_error
    +. (product_factor *. (abs_float first_left +. abs_float first_right))
    +. minimum_subnormal
  and second_minor_error =
    second_left_error +. second_right_error
    +. (product_factor *. (abs_float second_left +. abs_float second_right))
    +. minimum_subnormal
  and third_minor_error =
    third_left_error +. third_right_error
    +. (product_factor *. (abs_float third_left +. abs_float third_right))
    +. minimum_subnormal
  in
  let first = adx *. first_minor and second = ady *. second_minor and third = adz *. third_minor in
  let first_error =
    (abs_float adx *. first_minor_error)
    +. (abs_float first_minor *. adx_error)
    +. (adx_error *. first_minor_error)
    +. (product_factor *. abs_float first)
    +. minimum_subnormal
  and second_error =
    (abs_float ady *. second_minor_error)
    +. (abs_float second_minor *. ady_error)
    +. (ady_error *. second_minor_error)
    +. (product_factor *. abs_float second)
    +. minimum_subnormal
  and third_error =
    (abs_float adz *. third_minor_error)
    +. (abs_float third_minor *. adz_error)
    +. (adz_error *. third_minor_error)
    +. (product_factor *. abs_float third)
    +. minimum_subnormal
  in
  let determinant = first +. second +. third in
  let error =
    upward
      (first_error +. second_error +. third_error
      +. (product_factor *. (abs_float first +. abs_float second +. abs_float third))
      +. minimum_subnormal)
  in
  if Float.is_finite determinant && Float.is_finite error && abs_float determinant > error then
    if determinant < 0. then Predicates.Negative else Predicates.Positive
  else begin
    let ah = exact_homogeneous a
    and bh = exact_homogeneous b
    and ch = exact_homogeneous c
    and dh = exact_homogeneous d in
    sign_of_comparison
      (Exact.Scratch.orient3d_homogeneous ~ax:ah.hx ~ay:ah.hy ~az:ah.hz ~aw:ah.hw ~bx:bh.hx
         ~by:bh.hy ~bz:bh.hz ~bw:bh.hw ~cx:ch.hx ~cy:ch.hy ~cz:ch.hz ~cw:ch.hw ~dx:dh.hx ~dy:dh.hy
         ~dz:dh.hz ~dw:dh.hw)
  end

let incircle_components first_component second_component a b c d =
  ensure_same_source a b;
  ensure_same_source a c;
  ensure_same_source a d;
  let ah = exact_homogeneous a
  and bh = exact_homogeneous b
  and ch = exact_homogeneous c
  and dh = exact_homogeneous d in
  sign_of_comparison
    (Exact.Scratch.incircle_homogeneous ~ax:(first_component ah) ~ay:(second_component ah) ~aw:ah.hw
       ~bx:(first_component bh) ~by:(second_component bh) ~bw:bh.hw ~cx:(first_component ch)
       ~cy:(second_component ch) ~cw:ch.hw ~dx:(first_component dh) ~dy:(second_component dh)
       ~dw:dh.hw)

let incircle_projected projection a b c d =
  ensure_same_source a b;
  ensure_same_source a c;
  ensure_same_source a d;
  let a_first =
    match projection with 0 -> a.approximate_x | 1 -> a.approximate_y | _ -> a.approximate_z
  and a_second =
    match projection with 0 -> a.approximate_y | 1 -> a.approximate_z | _ -> a.approximate_x
  and b_first =
    match projection with 0 -> b.approximate_x | 1 -> b.approximate_y | _ -> b.approximate_z
  and b_second =
    match projection with 0 -> b.approximate_y | 1 -> b.approximate_z | _ -> b.approximate_x
  and c_first =
    match projection with 0 -> c.approximate_x | 1 -> c.approximate_y | _ -> c.approximate_z
  and c_second =
    match projection with 0 -> c.approximate_y | 1 -> c.approximate_z | _ -> c.approximate_x
  and d_first =
    match projection with 0 -> d.approximate_x | 1 -> d.approximate_y | _ -> d.approximate_z
  and d_second =
    match projection with 0 -> d.approximate_y | 1 -> d.approximate_z | _ -> d.approximate_x
  and a_first_error = match projection with 0 -> a.x_error | 1 -> a.y_error | _ -> a.z_error
  and a_second_error = match projection with 0 -> a.y_error | 1 -> a.z_error | _ -> a.x_error
  and b_first_error = match projection with 0 -> b.x_error | 1 -> b.y_error | _ -> b.z_error
  and b_second_error = match projection with 0 -> b.y_error | 1 -> b.z_error | _ -> b.x_error
  and c_first_error = match projection with 0 -> c.x_error | 1 -> c.y_error | _ -> c.z_error
  and c_second_error = match projection with 0 -> c.y_error | 1 -> c.z_error | _ -> c.x_error
  and d_first_error = match projection with 0 -> d.x_error | 1 -> d.y_error | _ -> d.z_error
  and d_second_error = match projection with 0 -> d.y_error | 1 -> d.z_error | _ -> d.x_error in
  let adx = a_first -. d_first
  and ady = a_second -. d_second
  and bdx = b_first -. d_first
  and bdy = b_second -. d_second
  and cdx = c_first -. d_first
  and cdy = c_second -. d_second in
  let subtraction_factor = 4. *. Float.epsilon in
  let adx_error =
    a_first_error +. d_first_error
    +. (subtraction_factor *. (abs_float a_first +. abs_float d_first))
    +. minimum_subnormal
  and ady_error =
    a_second_error +. d_second_error
    +. (subtraction_factor *. (abs_float a_second +. abs_float d_second))
    +. minimum_subnormal
  and bdx_error =
    b_first_error +. d_first_error
    +. (subtraction_factor *. (abs_float b_first +. abs_float d_first))
    +. minimum_subnormal
  and bdy_error =
    b_second_error +. d_second_error
    +. (subtraction_factor *. (abs_float b_second +. abs_float d_second))
    +. minimum_subnormal
  and cdx_error =
    c_first_error +. d_first_error
    +. (subtraction_factor *. (abs_float c_first +. abs_float d_first))
    +. minimum_subnormal
  and cdy_error =
    c_second_error +. d_second_error
    +. (subtraction_factor *. (abs_float c_second +. abs_float d_second))
    +. minimum_subnormal
  in
  let product_factor = 4. *. Float.epsilon in
  let adx2 = adx *. adx
  and ady2 = ady *. ady
  and bdx2 = bdx *. bdx
  and bdy2 = bdy *. bdy
  and cdx2 = cdx *. cdx
  and cdy2 = cdy *. cdy in
  let adx2_error =
    (2. *. abs_float adx *. adx_error)
    +. (adx_error *. adx_error)
    +. (product_factor *. abs_float adx2)
    +. minimum_subnormal
  and ady2_error =
    (2. *. abs_float ady *. ady_error)
    +. (ady_error *. ady_error)
    +. (product_factor *. abs_float ady2)
    +. minimum_subnormal
  and bdx2_error =
    (2. *. abs_float bdx *. bdx_error)
    +. (bdx_error *. bdx_error)
    +. (product_factor *. abs_float bdx2)
    +. minimum_subnormal
  and bdy2_error =
    (2. *. abs_float bdy *. bdy_error)
    +. (bdy_error *. bdy_error)
    +. (product_factor *. abs_float bdy2)
    +. minimum_subnormal
  and cdx2_error =
    (2. *. abs_float cdx *. cdx_error)
    +. (cdx_error *. cdx_error)
    +. (product_factor *. abs_float cdx2)
    +. minimum_subnormal
  and cdy2_error =
    (2. *. abs_float cdy *. cdy_error)
    +. (cdy_error *. cdy_error)
    +. (product_factor *. abs_float cdy2)
    +. minimum_subnormal
  in
  let alift = adx2 +. ady2 and blift = bdx2 +. bdy2 and clift = cdx2 +. cdy2 in
  let alift_error =
    adx2_error +. ady2_error
    +. (product_factor *. (abs_float adx2 +. abs_float ady2))
    +. minimum_subnormal
  and blift_error =
    bdx2_error +. bdy2_error
    +. (product_factor *. (abs_float bdx2 +. abs_float bdy2))
    +. minimum_subnormal
  and clift_error =
    cdx2_error +. cdy2_error
    +. (product_factor *. (abs_float cdx2 +. abs_float cdy2))
    +. minimum_subnormal
  in
  let bc_left = bdx *. cdy
  and bc_right = cdx *. bdy
  and ca_left = cdx *. ady
  and ca_right = adx *. cdy
  and ab_left = adx *. bdy
  and ab_right = bdx *. ady in
  let bc_left_error =
    (abs_float bdx *. cdy_error)
    +. (abs_float cdy *. bdx_error)
    +. (bdx_error *. cdy_error)
    +. (product_factor *. abs_float bc_left)
    +. minimum_subnormal
  and bc_right_error =
    (abs_float cdx *. bdy_error)
    +. (abs_float bdy *. cdx_error)
    +. (cdx_error *. bdy_error)
    +. (product_factor *. abs_float bc_right)
    +. minimum_subnormal
  and ca_left_error =
    (abs_float cdx *. ady_error)
    +. (abs_float ady *. cdx_error)
    +. (cdx_error *. ady_error)
    +. (product_factor *. abs_float ca_left)
    +. minimum_subnormal
  and ca_right_error =
    (abs_float adx *. cdy_error)
    +. (abs_float cdy *. adx_error)
    +. (adx_error *. cdy_error)
    +. (product_factor *. abs_float ca_right)
    +. minimum_subnormal
  and ab_left_error =
    (abs_float adx *. bdy_error)
    +. (abs_float bdy *. adx_error)
    +. (adx_error *. bdy_error)
    +. (product_factor *. abs_float ab_left)
    +. minimum_subnormal
  and ab_right_error =
    (abs_float bdx *. ady_error)
    +. (abs_float ady *. bdx_error)
    +. (bdx_error *. ady_error)
    +. (product_factor *. abs_float ab_right)
    +. minimum_subnormal
  in
  let difference_error left left_error right right_error =
    left_error +. right_error
    +. (subtraction_factor *. (abs_float left +. abs_float right))
    +. minimum_subnormal
  in
  let bc = bc_left -. bc_right and ca = ca_left -. ca_right and ab = ab_left -. ab_right in
  let bc_error = difference_error bc_left bc_left_error bc_right bc_right_error
  and ca_error = difference_error ca_left ca_left_error ca_right ca_right_error
  and ab_error = difference_error ab_left ab_left_error ab_right ab_right_error in
  let first = alift *. bc and second = blift *. ca and third = clift *. ab in
  let term_error left left_error right right_error product =
    (abs_float left *. right_error)
    +. (abs_float right *. left_error)
    +. (left_error *. right_error)
    +. (product_factor *. abs_float product)
    +. minimum_subnormal
  in
  let first_error = term_error alift alift_error bc bc_error first
  and second_error = term_error blift blift_error ca ca_error second
  and third_error = term_error clift clift_error ab ab_error third in
  let determinant = first +. second +. third in
  let determinant_error =
    first_error +. second_error +. third_error
    +. (8. *. Float.epsilon *. (abs_float first +. abs_float second +. abs_float third))
    +. minimum_subnormal
  in
  if
    Float.is_finite determinant && Float.is_finite determinant_error
    && abs_float determinant > determinant_error
  then if determinant > 0. then Predicates.Positive else Predicates.Negative
  else
    let first_component, second_component =
      match projection with 0 -> (x, y) | 1 -> (y, z) | _ -> (z, x)
    in
    incircle_components first_component second_component a b c d

let incircle_xy = incircle_projected 0
let incircle_yz = incircle_projected 1
let incircle_zx = incircle_projected 2
