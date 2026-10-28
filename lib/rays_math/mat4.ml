type t = float array

let index row column = (row * 4) + column

let of_rows (a, b, c, d) (e, f, g, h) (i, j, k, l) (m, n, o, p) =
  [| a; b; c; d; e; f; g; h; i; j; k; l; m; n; o; p |]

let identity = of_rows (1., 0., 0., 0.) (0., 1., 0., 0.) (0., 0., 1., 0.) (0., 0., 0., 1.)

let get matrix ~row ~column =
  if row < 0 || row > 3 || column < 0 || column > 3 then
    invalid_arg "Mat4.get: indices must be in 0..3";
  matrix.(index row column)

let mul left right =
  Array.init 16 (fun position ->
      let row = position / 4 and column = position mod 4 in
      let result = ref 0. in
      for inner = 0 to 3 do
        result := !result +. (left.(index row inner) *. right.(index inner column))
      done;
      !result)

let transpose matrix =
  Array.init 16 (fun position ->
      let row = position / 4 and column = position mod 4 in
      matrix.(index column row))

let inverse matrix =
  let augmented =
    Array.init 4 (fun row ->
        Array.init 8 (fun column ->
            if column < 4 then matrix.(index row column) else if column - 4 = row then 1. else 0.))
  in
  let swap left right =
    let temporary = augmented.(left) in
    augmented.(left) <- augmented.(right);
    augmented.(right) <- temporary
  in
  let singular = ref false in
  for column = 0 to 3 do
    let pivot = ref column in
    for row = column + 1 to 3 do
      if abs_float augmented.(row).(column) > abs_float augmented.(!pivot).(column) then
        pivot := row
    done;
    if abs_float augmented.(!pivot).(column) <= 1e-12 then singular := true
    else begin
      swap column !pivot;
      let divisor = augmented.(column).(column) in
      for entry = 0 to 7 do
        augmented.(column).(entry) <- augmented.(column).(entry) /. divisor
      done;
      for row = 0 to 3 do
        if row <> column then begin
          let factor = augmented.(row).(column) in
          for entry = 0 to 7 do
            augmented.(row).(entry) <-
              augmented.(row).(entry) -. (factor *. augmented.(column).(entry))
          done
        end
      done
    end
  done;
  if !singular then None
  else
    Some
      (Array.init 16 (fun position ->
           let row = position / 4 and column = position mod 4 in
           augmented.(row).(column + 4)))

let translation value =
  of_rows (1., 0., 0., value.Vec3.x) (0., 1., 0., value.y) (0., 0., 1., value.z) (0., 0., 0., 1.)

let scaling value =
  of_rows (value.Vec3.x, 0., 0., 0.) (0., value.y, 0., 0.) (0., 0., value.z, 0.) (0., 0., 0., 1.)

let rotation_x angle =
  let cosine = cos angle and sine = sin angle in
  of_rows (1., 0., 0., 0.) (0., cosine, -.sine, 0.) (0., sine, cosine, 0.) (0., 0., 0., 1.)

let rotation_y angle =
  let cosine = cos angle and sine = sin angle in
  of_rows (cosine, 0., sine, 0.) (0., 1., 0., 0.) (-.sine, 0., cosine, 0.) (0., 0., 0., 1.)

let rotation_z angle =
  let cosine = cos angle and sine = sin angle in
  of_rows (cosine, -.sine, 0., 0.) (sine, cosine, 0., 0.) (0., 0., 1., 0.) (0., 0., 0., 1.)

let rotation ~axis angle =
  let axis = Vec3.normalize axis in
  if Vec3.length_sq axis = 0. then identity
  else
    let x = axis.x and y = axis.y and z = axis.z in
    let cosine = cos angle and sine = sin angle in
    let one_minus = 1. -. cosine in
    of_rows
      ( cosine +. (x *. x *. one_minus),
        (x *. y *. one_minus) -. (z *. sine),
        (x *. z *. one_minus) +. (y *. sine),
        0. )
      ( (y *. x *. one_minus) +. (z *. sine),
        cosine +. (y *. y *. one_minus),
        (y *. z *. one_minus) -. (x *. sine),
        0. )
      ( (z *. x *. one_minus) -. (y *. sine),
        (z *. y *. one_minus) +. (x *. sine),
        cosine +. (z *. z *. one_minus),
        0. )
      (0., 0., 0., 1.)

let validate_frustum name near far =
  if (not (Float.is_finite near && Float.is_finite far)) || near <= 0. || far <= near then
    invalid_arg (name ^ ": require 0 < near < far")

let perspective ~fov_y ~aspect ~near ~far =
  validate_frustum "Mat4.perspective" near far;
  if (not (Float.is_finite fov_y)) || fov_y <= 0. || fov_y >= Float.pi then
    invalid_arg "Mat4.perspective: fov_y must be in (0, pi)";
  if (not (Float.is_finite aspect)) || aspect <= 0. then
    invalid_arg "Mat4.perspective: aspect must be finite and positive";
  let focal = 1. /. tan (fov_y /. 2.) in
  of_rows
    (focal /. aspect, 0., 0., 0.)
    (0., focal, 0., 0.)
    (0., 0., (far +. near) /. (near -. far), 2. *. far *. near /. (near -. far))
    (0., 0., -1., 0.)

let orthographic ~left ~right ~bottom ~top ~near ~far =
  if left = right || bottom = top || near = far then
    invalid_arg "Mat4.orthographic: bounds must span non-zero ranges";
  let x = right -. left and y = top -. bottom and z = far -. near in
  of_rows
    (2. /. x, 0., 0., -.((right +. left) /. x))
    (0., 2. /. y, 0., -.((top +. bottom) /. y))
    (0., 0., -2. /. z, -.((far +. near) /. z))
    (0., 0., 0., 1.)

let look_at ~eye ~target ~up =
  let forward = Vec3.normalize (Vec3.sub target eye) in
  if Vec3.length_sq forward = 0. then invalid_arg "Mat4.look_at: eye and target must differ";
  let side = Vec3.normalize (Vec3.cross forward up) in
  if Vec3.length_sq side = 0. then
    invalid_arg "Mat4.look_at: up must not be parallel to the view direction";
  let actual_up = Vec3.cross side forward in
  of_rows
    (side.x, side.y, side.z, -.Vec3.dot side eye)
    (actual_up.x, actual_up.y, actual_up.z, -.Vec3.dot actual_up eye)
    (-.forward.x, -.forward.y, -.forward.z, Vec3.dot forward eye)
    (0., 0., 0., 1.)

let transform matrix (x, y, z, w) =
  let component row =
    (matrix.(index row 0) *. x)
    +. (matrix.(index row 1) *. y)
    +. (matrix.(index row 2) *. z)
    +. (matrix.(index row 3) *. w)
  in
  (component 0, component 1, component 2, component 3)

let transform_point matrix value =
  let vx = value.Vec3.x and vy = value.y and vz = value.z in
  let x = (matrix.(0) *. vx) +. (matrix.(1) *. vy) +. (matrix.(2) *. vz) +. matrix.(3)
  and y = (matrix.(4) *. vx) +. (matrix.(5) *. vy) +. (matrix.(6) *. vz) +. matrix.(7)
  and z = (matrix.(8) *. vx) +. (matrix.(9) *. vy) +. (matrix.(10) *. vz) +. matrix.(11)
  and w = (matrix.(12) *. vx) +. (matrix.(13) *. vy) +. (matrix.(14) *. vz) +. matrix.(15) in
  if abs_float w <= 1e-12 then Vec3.create x y z else Vec3.create (x /. w) (y /. w) (z /. w)

let transform_direction matrix value =
  let vx = value.Vec3.x and vy = value.y and vz = value.z in
  Vec3.create
    ((matrix.(0) *. vx) +. (matrix.(1) *. vy) +. (matrix.(2) *. vz))
    ((matrix.(4) *. vx) +. (matrix.(5) *. vy) +. (matrix.(6) *. vz))
    ((matrix.(8) *. vx) +. (matrix.(9) *. vy) +. (matrix.(10) *. vz))

let nearly_equal left right ~eps =
  let result = ref true in
  for index = 0 to 15 do
    if abs_float (left.(index) -. right.(index)) > eps then result := false
  done;
  !result

let to_rows matrix =
  let row offset =
    (matrix.(offset), matrix.(offset + 1), matrix.(offset + 2), matrix.(offset + 3))
  in
  (row 0, row 4, row 8, row 12)
