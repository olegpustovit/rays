let stride = Triangle_intersection.event_stride

let[@inline always] clamp01 value =
  if value < 0. then 0. else if value > 1. then 1. else value

let[@inline always] dot ax ay az bx by bz =
  (ax *. bx) +. (ay *. by) +. (az *. bz)

let[@inline always] barycentric px py pz ax ay az bx by bz cx cy cz output offset =
  let v0x = bx -. ax and v0y = by -. ay and v0z = bz -. az
  and v1x = cx -. ax and v1y = cy -. ay and v1z = cz -. az
  and v2x = px -. ax and v2y = py -. ay and v2z = pz -. az in
  let d00 = dot v0x v0y v0z v0x v0y v0z
  and d01 = dot v0x v0y v0z v1x v1y v1z
  and d11 = dot v1x v1y v1z v1x v1y v1z
  and d20 = dot v2x v2y v2z v0x v0y v0z
  and d21 = dot v2x v2y v2z v1x v1y v1z in
  let denominator = (d00 *. d11) -. (d01 *. d01) in
  if denominator <= 0. then false else begin
    let v = clamp01 (((d11 *. d20) -. (d01 *. d21)) /. denominator)
    and w = clamp01 (((d00 *. d21) -. (d01 *. d20)) /. denominator) in
    let u = clamp01 (1. -. v -. w) in
    let sum = u +. v +. w in
    output.(offset) <- u /. sum;
    output.(offset + 1) <- v /. sum;
    output.(offset + 2) <- w /. sum;
    true
  end

let[@inline always] load_relative scratch target positions point ox oy oz =
  scratch.(target) <- positions.Packed.Float3.View.x.(point) -. ox;
  scratch.(target + 1) <- positions.y.(point) -. oy;
  scratch.(target + 2) <- positions.z.(point) -. oz

let normalize scratch tolerance last ox oy oz =
  let scale = ref 0. in
  for component = 0 to last do
    scale := Float.max !scale (Float.abs scratch.(component))
  done;
  if !scale = 0. then false else begin
    let inverse = 1. /. !scale in
    for component = 0 to last do scratch.(component) <- scratch.(component) *. inverse done;
    scratch.(18) <- Float.max (128. *. Float.epsilon) (tolerance *. inverse);
    scratch.(19) <- ox; scratch.(20) <- oy; scratch.(21) <- oz;
    scratch.(22) <- !scale;
    true
  end

let[@inline always] shared_point_suppressed self shared_count shared_x shared_y
    shared_z epsilon x y z =
  self && shared_count = 1
  && let dx = x -. shared_x and dy = y -. shared_y and dz = z -. shared_z in
     (dx *. dx) +. (dy *. dy) +. (dz *. dz) <= epsilon *. epsilon

let[@inline always] emit_segment_segment events count ~left_u0 ~left_u1
    ~right_u0 ~right_u1 ~left_t ~right_t x y z =
  let offset = count * stride in
  events.(offset) <- x; events.(offset + 1) <- y; events.(offset + 2) <- z;
  events.(offset + 3) <- left_u0 +. (clamp01 left_t *. (left_u1 -. left_u0));
  events.(offset + 4) <- 0.; events.(offset + 5) <- 0.;
  events.(offset + 6) <- right_u0 +. (clamp01 right_t *. (right_u1 -. right_u0));
  events.(offset + 7) <- 0.; events.(offset + 8) <- 0.;
  count + 1

let segment_segment_events_into ~scratch ~events ~self ~tolerance
    ~left_positions ~left_a ~left_b ~left_u0 ~left_u1 ~right_positions
    ~right_a ~right_b ~right_u0 ~right_u1 =
  if Array.length scratch < 26 || Array.length events < 2 * stride then
    invalid_arg "Curve_intersection.segment_segment_events_into: scratch too small";
  let ox = left_positions.Packed.Float3.View.x.(left_a)
  and oy = left_positions.y.(left_a) and oz = left_positions.z.(left_a) in
  scratch.(0) <- 0.; scratch.(1) <- 0.; scratch.(2) <- 0.;
  load_relative scratch 3 left_positions left_b ox oy oz;
  load_relative scratch 6 right_positions right_a ox oy oz;
  load_relative scratch 9 right_positions right_b ox oy oz;
  if not (normalize scratch tolerance 11 ox oy oz) then 0 else begin
    let d1x = scratch.(3) and d1y = scratch.(4) and d1z = scratch.(5)
    and p2x = scratch.(6) and p2y = scratch.(7) and p2z = scratch.(8) in
    let d2x = scratch.(9) -. p2x and d2y = scratch.(10) -. p2y
    and d2z = scratch.(11) -. p2z in
    let a = dot d1x d1y d1z d1x d1y d1z
    and e = dot d2x d2y d2z d2x d2y d2z
    and b = dot d1x d1y d1z d2x d2y d2z
    and c = -. (dot d1x d1y d1z p2x p2y p2z)
    and f = -. (dot d2x d2y d2z p2x p2y p2z)
    and epsilon = scratch.(18) in
    let cross_x = (d1y *. d2z) -. (d1z *. d2y)
    and cross_y = (d1z *. d2x) -. (d1x *. d2z)
    and cross_z = (d1x *. d2y) -. (d1y *. d2x) in
    let offset_cross_x = (p2y *. d1z) -. (p2z *. d1y)
    and offset_cross_y = (p2z *. d1x) -. (p2x *. d1z)
    and offset_cross_z = (p2x *. d1y) -. (p2y *. d1x) in
    let parallel2 = dot cross_x cross_y cross_z cross_x cross_y cross_z
    and collinear2 = dot offset_cross_x offset_cross_y offset_cross_z
        offset_cross_x offset_cross_y offset_cross_z in
    let shared_count = if self then
        (if left_a = right_a || left_a = right_b then 1 else 0)
        + (if left_b = right_a || left_b = right_b then 1 else 0)
      else 0 in
    let shared_point = if shared_count = 1 then
        if left_a = right_a || left_a = right_b then 0 else 3
      else 0 in
    let shared_x = scratch.(shared_point) and shared_y = scratch.(shared_point + 1)
    and shared_z = scratch.(shared_point + 2) in
    let count = if parallel2 <= epsilon *. epsilon *. a *. e
        && collinear2 <= epsilon *. epsilon *. a then begin
      let t0 = dot p2x p2y p2z d1x d1y d1z /. a
      and t1 = dot scratch.(9) scratch.(10) scratch.(11) d1x d1y d1z /. a in
      let lower = Float.max 0. (Float.min t0 t1)
      and upper = Float.min 1. (Float.max t0 t1) in
      if lower > upper +. epsilon then 0 else
        let emit count t =
          let x = t *. d1x and y = t *. d1y and z = t *. d1z in
          if shared_point_suppressed self shared_count shared_x shared_y shared_z
              epsilon x y z then count else
            let right_t = dot (x -. p2x) (y -. p2y) (z -. p2z)
                d2x d2y d2z /. e in
            emit_segment_segment events count ~left_u0 ~left_u1 ~right_u0
              ~right_u1 ~left_t:t ~right_t x y z in
        let count = emit 0 lower in
        if upper -. lower <= epsilon then count else emit count upper
    end else begin
      let denominator = (a *. e) -. (b *. b) in
      let left_t = if denominator <= epsilon *. epsilon then 0.
        else clamp01 (((b *. f) -. (c *. e)) /. denominator) in
      let right_t = ref ((b *. left_t +. f) /. e) in
      let left_t = ref left_t in
      if !right_t < 0. then begin
        right_t := 0.; left_t := clamp01 (-.c /. a)
      end else if !right_t > 1. then begin
        right_t := 1.; left_t := clamp01 ((b -. c) /. a)
      end;
      let lx = !left_t *. d1x and ly = !left_t *. d1y
      and lz = !left_t *. d1z
      and rx = p2x +. (!right_t *. d2x)
      and ry = p2y +. (!right_t *. d2y)
      and rz = p2z +. (!right_t *. d2z) in
      let dx = lx -. rx and dy = ly -. ry and dz = lz -. rz in
      if (dx *. dx) +. (dy *. dy) +. (dz *. dz) > epsilon *. epsilon then 0
      else
        let x = (lx +. rx) *. 0.5 and y = (ly +. ry) *. 0.5
        and z = (lz +. rz) *. 0.5 in
        if shared_point_suppressed self shared_count shared_x shared_y shared_z
            epsilon x y z then 0 else
          emit_segment_segment events 0 ~left_u0 ~left_u1 ~right_u0 ~right_u1
            ~left_t:!left_t ~right_t:!right_t x y z
    end in
    let scale = scratch.(22) and ox = scratch.(19) and oy = scratch.(20)
    and oz = scratch.(21) in
    for event = 0 to count - 1 do
      let offset = event * stride in
      events.(offset) <- ox +. (events.(offset) *. scale);
      events.(offset + 1) <- oy +. (events.(offset + 1) *. scale);
      events.(offset + 2) <- oz +. (events.(offset + 2) *. scale)
    done;
    count
  end

let[@inline always] emit_segment_triangle events count ~segment_first
    ~segment_u0 ~segment_u1 ~along x y z ax ay az bx by bz cx cy cz =
  let offset = count * stride in
  events.(offset) <- x; events.(offset + 1) <- y; events.(offset + 2) <- z;
  let segment_offset, triangle_offset = if segment_first then offset + 3, offset + 6
    else offset + 6, offset + 3 in
  events.(segment_offset) <- segment_u0
      +. (clamp01 along *. (segment_u1 -. segment_u0));
  events.(segment_offset + 1) <- 0.; events.(segment_offset + 2) <- 0.;
  if barycentric x y z ax ay az bx by bz cx cy cz events triangle_offset
  then count + 1 else count

let segment_triangle_events_into ~scratch ~events ~self ~tolerance
    ~include_coplanar ~segment_first ~segment_positions ~segment_a ~segment_b
    ~segment_u0 ~segment_u1 ~triangle_positions ~triangle_a ~triangle_b
    ~triangle_c =
  if Array.length scratch < 26 || Array.length events < 2 * stride then
    invalid_arg "Curve_intersection.segment_triangle_events_into: scratch too small";
  let ox = segment_positions.Packed.Float3.View.x.(segment_a)
  and oy = segment_positions.y.(segment_a) and oz = segment_positions.z.(segment_a) in
  scratch.(0) <- 0.; scratch.(1) <- 0.; scratch.(2) <- 0.;
  load_relative scratch 3 segment_positions segment_b ox oy oz;
  load_relative scratch 6 triangle_positions triangle_a ox oy oz;
  load_relative scratch 9 triangle_positions triangle_b ox oy oz;
  load_relative scratch 12 triangle_positions triangle_c ox oy oz;
  if not (normalize scratch tolerance 14 ox oy oz) then 0 else begin
    let dx = scratch.(3) and dy = scratch.(4) and dz = scratch.(5)
    and ax = scratch.(6) and ay = scratch.(7) and az = scratch.(8)
    and bx = scratch.(9) and by = scratch.(10) and bz = scratch.(11)
    and cx = scratch.(12) and cy = scratch.(13) and cz = scratch.(14)
    and epsilon = scratch.(18) in
    let e1x = bx -. ax and e1y = by -. ay and e1z = bz -. az
    and e2x = cx -. ax and e2y = cy -. ay and e2z = cz -. az in
    let nx = (e1y *. e2z) -. (e1z *. e2y)
    and ny = (e1z *. e2x) -. (e1x *. e2z)
    and nz = (e1x *. e2y) -. (e1y *. e2x) in
    let normal_length = sqrt (dot nx ny nz nx ny nz) in
    let p0_plane = -. (dot nx ny nz ax ay az)
    and p1_plane = dot nx ny nz (dx -. ax) (dy -. ay) (dz -. az) in
    let coplanar = Float.abs p0_plane <= epsilon *. normal_length
        && Float.abs p1_plane <= epsilon *. normal_length in
    let shared_count = if self then
        (if segment_a = triangle_a || segment_a = triangle_b
            || segment_a = triangle_c then 1 else 0)
        + (if segment_b = triangle_a || segment_b = triangle_b
            || segment_b = triangle_c then 1 else 0)
      else 0 in
    let suppress x y z =
      if not self || shared_count = 0 then false
      else if shared_count = 2 then true
      else
        let point = if segment_a = triangle_a || segment_a = triangle_b
            || segment_a = triangle_c then 0 else 3 in
        let ex = x -. scratch.(point) and ey = y -. scratch.(point + 1)
        and ez = z -. scratch.(point + 2) in
        (ex *. ex) +. (ey *. ey) +. (ez *. ez) <= epsilon *. epsilon in
    let emit count along =
      let along = clamp01 along in
      let x = along *. dx and y = along *. dy and z = along *. dz in
      if suppress x y z then count else emit_segment_triangle events count
        ~segment_first ~segment_u0 ~segment_u1 ~along x y z ax ay az bx by bz
        cx cy cz in
    let count = if coplanar then
        if not include_coplanar then 0 else begin
          let anx = Float.abs nx and any = Float.abs ny and anz = Float.abs nz in
          let drop = if anx >= any && anx >= anz then 0
            else if any >= anz then 1 else 2 in
          let project x y z = if drop = 0 then y, z
            else if drop = 1 then x, z else x, y in
          let sx, sy = project 0. 0. 0. and ex, ey = project dx dy dz
          and a2x, a2y = project ax ay az and b2x, b2y = project bx by bz
          and c2x, c2y = project cx cy cz in
          let orient x0 y0 x1 y1 x y =
            ((x1 -. x0) *. (y -. y0)) -. ((y1 -. y0) *. (x -. x0)) in
          let area = orient a2x a2y b2x b2y c2x c2y in
          let sign = if area >= 0. then 1. else -1. in
          let lower = ref 0. and upper = ref 1. and valid = ref true in
          let clip x0 y0 x1 y1 =
            let f0 = sign *. orient x0 y0 x1 y1 sx sy
            and f1 = sign *. orient x0 y0 x1 y1 ex ey in
            if f0 < -.epsilon && f1 < -.epsilon then valid := false
            else if f0 < -.epsilon || f1 < -.epsilon then begin
              let at = f0 /. (f0 -. f1) in
              if f0 < -.epsilon then lower := Float.max !lower at
              else upper := Float.min !upper at
            end in
          clip a2x a2y b2x b2y; clip b2x b2y c2x c2y;
          clip c2x c2y a2x a2y;
          if not !valid || !lower > !upper +. epsilon then 0 else
            let count = emit 0 !lower in
            if !upper -. !lower <= epsilon then count else emit count !upper
        end
      else begin
        let px = (dy *. e2z) -. (dz *. e2y)
        and py = (dz *. e2x) -. (dx *. e2z)
        and pz = (dx *. e2y) -. (dy *. e2x) in
        let determinant = dot e1x e1y e1z px py pz in
        if Float.abs determinant <= epsilon then 0 else
          let inverse = 1. /. determinant
          and tx = -.ax and ty = -.ay and tz = -.az in
          let u = dot tx ty tz px py pz *. inverse in
          if u < -.epsilon || u > 1. +. epsilon then 0 else
            let qx = (ty *. e1z) -. (tz *. e1y)
            and qy = (tz *. e1x) -. (tx *. e1z)
            and qz = (tx *. e1y) -. (ty *. e1x) in
            let v = dot dx dy dz qx qy qz *. inverse in
            if v < -.epsilon || u +. v > 1. +. epsilon then 0 else
              let along = dot e2x e2y e2z qx qy qz *. inverse in
              if along < -.epsilon || along > 1. +. epsilon then 0
              else emit 0 along
      end in
    let scale = scratch.(22) and ox = scratch.(19) and oy = scratch.(20)
    and oz = scratch.(21) in
    for event = 0 to count - 1 do
      let offset = event * stride in
      events.(offset) <- ox +. (events.(offset) *. scale);
      events.(offset + 1) <- oy +. (events.(offset + 1) *. scale);
      events.(offset + 2) <- oz +. (events.(offset + 2) *. scale)
    done;
    count
  end
