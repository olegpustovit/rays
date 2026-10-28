type surface = {
  index : Surface_index.t;
  positions : Packed.Float3.View.view;
  topology : Topology.View.view;
}

let surface index geometry = {
  index;
  positions = Packed.Float3.View.view (Geometry.positions geometry);
  topology = Topology.View.view (Geometry.topology geometry);
}

let max_events = 12
let event_stride = 9

let[@inline always] clamp01 value =
  if value < 0. then 0. else if value > 1. then 1. else value

let[@inline always] coordinate scratch point component =
  scratch.((point * 3) + component)

let[@inline always] projected_u scratch axis point =
  if axis = 0 then coordinate scratch point 1 else coordinate scratch point 0

let[@inline always] projected_v scratch axis point =
  if axis = 2 then coordinate scratch point 1 else coordinate scratch point 2

let[@inline always] cross2 ax ay bx by = (ax *. by) -. (ay *. bx)

let[@inline always] point_in_projected_triangle scratch epsilon axis point a b c =
  let px = projected_u scratch axis point
  and py = projected_v scratch axis point
  and ax = projected_u scratch axis a
  and ay = projected_v scratch axis a
  and bx = projected_u scratch axis b
  and by = projected_v scratch axis b
  and cx = projected_u scratch axis c
  and cy = projected_v scratch axis c in
  let ab = cross2 (bx -. ax) (by -. ay) (px -. ax) (py -. ay)
  and bc = cross2 (cx -. bx) (cy -. by) (px -. bx) (py -. by)
  and ca = cross2 (ax -. cx) (ay -. cy) (px -. cx) (py -. cy) in
  (ab >= -.epsilon && bc >= -.epsilon && ca >= -.epsilon)
  || (ab <= epsilon && bc <= epsilon && ca <= epsilon)

let[@inline always] barycentric_into scratch point a b c output offset =
  let px = scratch.(point) and py = scratch.(point + 1)
  and pz = scratch.(point + 2)
  and ax = scratch.(a) and ay = scratch.(a + 1) and az = scratch.(a + 2)
  and bx = scratch.(b) and by = scratch.(b + 1) and bz = scratch.(b + 2)
  and cx = scratch.(c) and cy = scratch.(c + 1) and cz = scratch.(c + 2) in
  let v0x = bx -. ax and v0y = by -. ay and v0z = bz -. az
  and v1x = cx -. ax and v1y = cy -. ay and v1z = cz -. az
  and v2x = px -. ax and v2y = py -. ay and v2z = pz -. az in
  let d00 = (v0x *. v0x) +. (v0y *. v0y) +. (v0z *. v0z)
  and d01 = (v0x *. v1x) +. (v0y *. v1y) +. (v0z *. v1z)
  and d11 = (v1x *. v1x) +. (v1y *. v1y) +. (v1z *. v1z)
  and d20 = (v2x *. v0x) +. (v2y *. v0y) +. (v2z *. v0z)
  and d21 = (v2x *. v1x) +. (v2y *. v1y) +. (v2z *. v1z) in
  let denominator = (d00 *. d11) -. (d01 *. d01) in
  if denominator <= 0. then false
  else begin
    let v = ((d11 *. d20) -. (d01 *. d21)) /. denominator
    and w = ((d00 *. d21) -. (d01 *. d20)) /. denominator in
    let u = 1. -. v -. w in
    let u = clamp01 u and v = clamp01 v and w = clamp01 w in
    let sum = u +. v +. w in
    output.(offset) <- u /. sum;
    output.(offset + 1) <- v /. sum;
    output.(offset + 2) <- w /. sum;
    true
  end

let[@inline always] point_segment_distance_squared scratch point a b =
  let px = scratch.(point) and py = scratch.(point + 1)
  and pz = scratch.(point + 2)
  and ax = scratch.(a) and ay = scratch.(a + 1) and az = scratch.(a + 2)
  and bx = scratch.(b) and by = scratch.(b + 1) and bz = scratch.(b + 2) in
  let dx = bx -. ax and dy = by -. ay and dz = bz -. az in
  let length_squared = (dx *. dx) +. (dy *. dy) +. (dz *. dz) in
  let along = if length_squared = 0. then 0. else clamp01
      (((px -. ax) *. dx +. (py -. ay) *. dy +. (pz -. az) *. dz)
       /. length_squared) in
  let qx = ax +. (along *. dx) and qy = ay +. (along *. dy)
  and qz = az +. (along *. dz) in
  let ex = px -. qx and ey = py -. qy and ez = pz -. qz in
  (ex *. ex) +. (ey *. ey) +. (ez *. ez)

let[@inline always] suppressed_self_event scratch point_info point epsilon =
  match point_info.(6) with
  | 1 ->
      let shared = point_info.(7) * 3 in
      let dx = scratch.(point) -. scratch.(shared)
      and dy = scratch.(point + 1) -. scratch.(shared + 1)
      and dz = scratch.(point + 2) -. scratch.(shared + 2) in
      (dx *. dx) +. (dy *. dy) +. (dz *. dz) <= epsilon *. epsilon
  | 2 -> point_segment_distance_squared scratch point
      (point_info.(7) * 3) (point_info.(8) * 3) <= epsilon *. epsilon
  | _ -> false

let[@inline always] add_event scratch point_info events count ~self point =
  let epsilon = scratch.(18) in
  if self && suppressed_self_event scratch point_info point epsilon then count
  else begin
    let duplicate = ref false and event = ref 0 in
    while not !duplicate && !event < count do
      let offset = !event * event_stride in
      let dx = scratch.(point) -. events.(offset)
      and dy = scratch.(point + 1) -. events.(offset + 1)
      and dz = scratch.(point + 2) -. events.(offset + 2) in
      duplicate := (dx *. dx) +. (dy *. dy) +. (dz *. dz)
          <= epsilon *. epsilon;
      incr event
    done;
    if !duplicate || count = max_events then count
    else
      let offset = count * event_stride in
      events.(offset) <- scratch.(point);
      events.(offset + 1) <- scratch.(point + 1);
      events.(offset + 2) <- scratch.(point + 2);
      if barycentric_into scratch point 0 3 6 events (offset + 3)
          && barycentric_into scratch point 9 12 15 events (offset + 6)
      then count + 1 else count
  end

let[@inline always] add_interpolated scratch point_info events count ~self p0 p1 along =
  let along = clamp01 along in
  scratch.(23) <- scratch.(p0) +. (along *. (scratch.(p1) -. scratch.(p0)));
  scratch.(24) <- scratch.(p0 + 1)
      +. (along *. (scratch.(p1 + 1) -. scratch.(p0 + 1)));
  scratch.(25) <- scratch.(p0 + 2)
      +. (along *. (scratch.(p1 + 2) -. scratch.(p0 + 2)));
  add_event scratch point_info events count ~self 23

let[@inline always] segment_triangle_event scratch point_info events count ~self
    p0 p1 a0 a1 a2 =
  let p0x = scratch.(p0) and p0y = scratch.(p0 + 1)
  and p0z = scratch.(p0 + 2) and p1x = scratch.(p1)
  and p1y = scratch.(p1 + 1) and p1z = scratch.(p1 + 2)
  and a0x = scratch.(a0) and a0y = scratch.(a0 + 1)
  and a0z = scratch.(a0 + 2) and a1x = scratch.(a1)
  and a1y = scratch.(a1 + 1) and a1z = scratch.(a1 + 2)
  and a2x = scratch.(a2) and a2y = scratch.(a2 + 1)
  and a2z = scratch.(a2 + 2) and epsilon = scratch.(18) in
  let dx = p1x -. p0x and dy = p1y -. p0y and dz = p1z -. p0z
  and e1x = a1x -. a0x and e1y = a1y -. a0y and e1z = a1z -. a0z
  and e2x = a2x -. a0x and e2y = a2y -. a0y and e2z = a2z -. a0z in
  let px = dy *. e2z -. dz *. e2y
  and py = dz *. e2x -. dx *. e2z
  and pz = dx *. e2y -. dy *. e2x in
  let determinant = (e1x *. px) +. (e1y *. py) +. (e1z *. pz) in
  if Float.abs determinant <= epsilon then count
  else
    let inverse = 1. /. determinant
    and tx = p0x -. a0x and ty = p0y -. a0y and tz = p0z -. a0z in
    let u = ((tx *. px) +. (ty *. py) +. (tz *. pz)) *. inverse in
    if u < -.epsilon || u > 1. +. epsilon then count
    else
      let qx = (ty *. e1z) -. (tz *. e1y)
      and qy = (tz *. e1x) -. (tx *. e1z)
      and qz = (tx *. e1y) -. (ty *. e1x) in
      let v = ((dx *. qx) +. (dy *. qy) +. (dz *. qz)) *. inverse in
      if v < -.epsilon || u +. v > 1. +. epsilon then count
      else
        let along = ((e2x *. qx) +. (e2y *. qy) +. (e2z *. qz)) *. inverse in
        if along < -.epsilon || along > 1. +. epsilon then count
        else add_interpolated scratch point_info events count ~self p0 p1 along

let[@inline always] coplanar_segment_events scratch point_info events count ~self
    axis a b c d =
  let ax = projected_u scratch axis a and ay = projected_v scratch axis a
  and bx = projected_u scratch axis b and by = projected_v scratch axis b
  and cx = projected_u scratch axis c and cy = projected_v scratch axis c
  and dx = projected_u scratch axis d and dy = projected_v scratch axis d
  and epsilon = scratch.(18) in
  let rx = bx -. ax and ry = by -. ay and sx = dx -. cx and sy = dy -. cy in
  let denominator = cross2 rx ry sx sy
  and qpx = cx -. ax and qpy = cy -. ay in
  if Float.abs denominator > epsilon then begin
    let along = cross2 qpx qpy sx sy /. denominator
    and other = cross2 qpx qpy rx ry /. denominator in
    if along >= -.epsilon && along <= 1. +. epsilon
        && other >= -.epsilon && other <= 1. +. epsilon
    then add_interpolated scratch point_info events count ~self (a * 3) (b * 3)
        along
    else count
  end else if Float.abs (cross2 qpx qpy rx ry) > epsilon then count
  else begin
    let use_x = Float.abs rx >= Float.abs ry in
    let start = if use_x then ax else ay and finish = if use_x then bx else by
    and c_value = if use_x then cx else cy and d_value = if use_x then dx else dy in
    let delta = finish -. start in
    if Float.abs delta <= epsilon then begin
      let ex = ax -. cx and ey = ay -. cy in
      if (ex *. ex) +. (ey *. ey) <= epsilon *. epsilon then
        add_interpolated scratch point_info events count ~self (a * 3) (b * 3) 0.
      else count
    end else begin
      let t0 = (c_value -. start) /. delta
      and t1 = (d_value -. start) /. delta in
      let lower = Float.max 0. (Float.min t0 t1)
      and upper = Float.min 1. (Float.max t0 t1) in
      if lower > upper +. epsilon then count
      else
        let count = add_interpolated scratch point_info events count ~self
            (a * 3) (b * 3) lower in
        if upper -. lower <= epsilon then count else
          add_interpolated scratch point_info events count ~self
            (a * 3) (b * 3) upper
    end
  end

let[@inline always] add_vertex_if_inside scratch point_info events count ~self
    axis point a b c =
  if point_in_projected_triangle scratch scratch.(18) axis point a b c then
    add_event scratch point_info events count ~self (point * 3)
  else count

let[@inline always] prepare_points scratch point_info ~self ~tolerance
    (left_positions : Packed.Float3.View.view) a0 a1 a2
    (right_positions : Packed.Float3.View.view) b0 b1 b2 =
  point_info.(0) <- a0; point_info.(1) <- a1; point_info.(2) <- a2;
  point_info.(3) <- b0; point_info.(4) <- b1; point_info.(5) <- b2;
  point_info.(6) <- 0;
  if self then
    for source = 0 to 2 do
      let already = ref false in
      for slot = 0 to point_info.(6) - 1 do
        if point_info.(7 + slot) = source then already := true
      done;
      if not !already then begin
        let collision = ref 3 and found = ref false in
        while not !found && !collision <= 5 do
          found := point_info.(source) = point_info.(!collision);
          incr collision
        done;
        if !found then begin
          point_info.(7 + point_info.(6)) <- source;
          point_info.(6) <- point_info.(6) + 1
        end
      end
    done;
  let origin_x = left_positions.Packed.Float3.View.x.(a0)
  and origin_y = left_positions.y.(a0) and origin_z = left_positions.z.(a0) in
  scratch.(0) <- 0.; scratch.(1) <- 0.; scratch.(2) <- 0.;
  scratch.(3) <- left_positions.Packed.Float3.View.x.(a1) -. origin_x;
  scratch.(4) <- left_positions.y.(a1) -. origin_y;
  scratch.(5) <- left_positions.z.(a1) -. origin_z;
  scratch.(6) <- left_positions.x.(a2) -. origin_x;
  scratch.(7) <- left_positions.y.(a2) -. origin_y;
  scratch.(8) <- left_positions.z.(a2) -. origin_z;
  scratch.(9) <- right_positions.x.(b0) -. origin_x;
  scratch.(10) <- right_positions.y.(b0) -. origin_y;
  scratch.(11) <- right_positions.z.(b0) -. origin_z;
  scratch.(12) <- right_positions.x.(b1) -. origin_x;
  scratch.(13) <- right_positions.y.(b1) -. origin_y;
  scratch.(14) <- right_positions.z.(b1) -. origin_z;
  scratch.(15) <- right_positions.x.(b2) -. origin_x;
  scratch.(16) <- right_positions.y.(b2) -. origin_y;
  scratch.(17) <- right_positions.z.(b2) -. origin_z;
  let scale = ref 0. in
  for component = 3 to 17 do
    scale := Float.max !scale (Float.abs scratch.(component))
  done;
  if !scale = 0. then false
  else begin
    let inverse = 1. /. !scale in
    for component = 3 to 17 do
      scratch.(component) <- scratch.(component) *. inverse
    done;
    scratch.(18) <- Float.max (128. *. Float.epsilon) (tolerance *. inverse);
    scratch.(19) <- origin_x;
    scratch.(20) <- origin_y;
    scratch.(21) <- origin_z;
    scratch.(22) <- !scale;
    true
  end

let[@inline always] events_points_into ~scratch ~point_info ~events ~self
    ~tolerance ~include_coplanar ~left_positions ~left_a ~left_b ~left_c
    ~right_positions ~right_a ~right_b ~right_c =
  if Array.length scratch < 26 || Array.length point_info < 10
      || Array.length events < max_events * event_stride then
    invalid_arg "Triangle_intersection.events_into: scratch is too small";
  if not (prepare_points scratch point_info ~self ~tolerance left_positions
      left_a left_b left_c right_positions right_a right_b right_c) then 0
  else begin
    let ax = scratch.(3) and ay = scratch.(4) and az = scratch.(5)
    and bx = scratch.(6) and by = scratch.(7) and bz = scratch.(8) in
    let nx = (ay *. bz) -. (az *. by)
    and ny = (az *. bx) -. (ax *. bz)
    and nz = (ax *. by) -. (ay *. bx) in
    let normal_length = sqrt ((nx *. nx) +. (ny *. ny) +. (nz *. nz))
    and epsilon = scratch.(18) in
    let b0_plane = (nx *. scratch.(9)) +. (ny *. scratch.(10))
        +. (nz *. scratch.(11))
    and b1_plane = (nx *. scratch.(12)) +. (ny *. scratch.(13))
        +. (nz *. scratch.(14))
    and b2_plane = (nx *. scratch.(15)) +. (ny *. scratch.(16))
        +. (nz *. scratch.(17)) in
    let coplanar = normal_length > 0.
        && Float.abs b0_plane <= epsilon *. normal_length
        && Float.abs b1_plane <= epsilon *. normal_length
        && Float.abs b2_plane <= epsilon *. normal_length in
    let count = if coplanar then
        if not include_coplanar then 0 else begin
          let anx = Float.abs nx and any = Float.abs ny and anz = Float.abs nz in
          let axis = if anx >= any && anx >= anz then 0
            else if any >= anz then 1 else 2 in
          let count = add_vertex_if_inside scratch point_info events 0 ~self axis
              0 3 4 5 in
          let count = add_vertex_if_inside scratch point_info events count ~self axis
              1 3 4 5 in
          let count = add_vertex_if_inside scratch point_info events count ~self axis
              2 3 4 5 in
          let count = add_vertex_if_inside scratch point_info events count ~self axis
              3 0 1 2 in
          let count = add_vertex_if_inside scratch point_info events count ~self axis
              4 0 1 2 in
          let count = add_vertex_if_inside scratch point_info events count ~self axis
              5 0 1 2 in
          let count = coplanar_segment_events scratch point_info events count
              ~self axis 0 1 3 4 in
          let count = coplanar_segment_events scratch point_info events count
              ~self axis 0 1 4 5 in
          let count = coplanar_segment_events scratch point_info events count
              ~self axis 0 1 5 3 in
          let count = coplanar_segment_events scratch point_info events count
              ~self axis 1 2 3 4 in
          let count = coplanar_segment_events scratch point_info events count
              ~self axis 1 2 4 5 in
          let count = coplanar_segment_events scratch point_info events count
              ~self axis 1 2 5 3 in
          let count = coplanar_segment_events scratch point_info events count
              ~self axis 2 0 3 4 in
          let count = coplanar_segment_events scratch point_info events count
              ~self axis 2 0 4 5 in
          coplanar_segment_events scratch point_info events count
              ~self axis 2 0 5 3
        end
      else
        let count = segment_triangle_event scratch point_info events 0 ~self
            0 3 9 12 15 in
        let count = segment_triangle_event scratch point_info events count ~self
            3 6 9 12 15 in
        let count = segment_triangle_event scratch point_info events count ~self
            6 0 9 12 15 in
        let count = segment_triangle_event scratch point_info events count ~self
            9 12 0 3 6 in
        let count = segment_triangle_event scratch point_info events count ~self
            12 15 0 3 6 in
        segment_triangle_event scratch point_info events count ~self
            15 9 0 3 6 in
    let origin_x = scratch.(19) and origin_y = scratch.(20)
    and origin_z = scratch.(21) and scale = scratch.(22) in
    for event = 0 to count - 1 do
      let offset = event * event_stride in
      events.(offset) <- origin_x +. (events.(offset) *. scale);
      events.(offset + 1) <- origin_y +. (events.(offset + 1) *. scale);
      events.(offset + 2) <- origin_z +. (events.(offset + 2) *. scale)
    done;
    count
  end

let[@inline always] events_into ~scratch ~point_info ~events ~self ~tolerance
    ~include_coplanar left left_triangle right right_triangle =
  let left_vertices = left.topology.Topology.View.vertex_points
  and right_vertices = right.topology.Topology.View.vertex_points in
  events_points_into ~scratch ~point_info ~events ~self ~tolerance
    ~include_coplanar ~left_positions:left.positions
    ~left_a:left_vertices.(Surface_index.View.triangle_vertex
      left.index left_triangle 0)
    ~left_b:left_vertices.(Surface_index.View.triangle_vertex
      left.index left_triangle 1)
    ~left_c:left_vertices.(Surface_index.View.triangle_vertex
      left.index left_triangle 2)
    ~right_positions:right.positions
    ~right_a:right_vertices.(Surface_index.View.triangle_vertex
      right.index right_triangle 0)
    ~right_b:right_vertices.(Surface_index.View.triangle_vertex
      right.index right_triangle 1)
    ~right_c:right_vertices.(Surface_index.View.triangle_vertex
      right.index right_triangle 2)
