type sign = Negative | Zero | Positive

type segment_segment =
  | Segments_disjoint
  | Segments_point
  | Segments_overlap
  | Segments_degenerate

module Dyadic = Exact_dyadic

let sign_of_comparison comparison =
  if comparison < 0 then Negative
  else if comparison > 0 then Positive
  else Zero

let exact_orient2d ~ax ~ay ~bx ~by ~cx ~cy =
  sign_of_comparison
    (Dyadic.Scratch.orient2d ~ax ~ay ~bx ~by ~cx ~cy)

let orient2d ~ax ~ay ~bx ~by ~cx ~cy =
  if not (Float.is_finite ax && Float.is_finite ay
      && Float.is_finite bx && Float.is_finite by
      && Float.is_finite cx && Float.is_finite cy) then
    invalid_arg "Rdk.Predicates.orient2d: coordinates must be finite";
  let acx = ax -. cx and bcx = bx -. cx
  and acy = ay -. cy and bcy = by -. cy in
  let left = acx *. bcy and right = acy *. bcx in
  let determinant = left -. right
  and permanent = abs_float left +. abs_float right in
  let error_bound = (3. +. (16. *. Float.epsilon)) *. Float.epsilon in
  if Float.is_finite determinant && Float.is_finite permanent
      && abs_float determinant > error_bound *. permanent
  then if determinant < 0. then Negative else Positive
  else exact_orient2d ~ax ~ay ~bx ~by ~cx ~cy

let[@inline] orient2d_packed ~x ~y a b c =
  let ax = x.(a) and ay = y.(a) and bx = x.(b) and by = y.(b)
  and cx = x.(c) and cy = y.(c) in
  if not (Float.is_finite ax && Float.is_finite ay
      && Float.is_finite bx && Float.is_finite by
      && Float.is_finite cx && Float.is_finite cy) then
    invalid_arg "Rdk.Predicates.orient2d_packed: coordinates must be finite";
  let acx = ax -. cx and bcx = bx -. cx
  and acy = ay -. cy and bcy = by -. cy in
  let left = acx *. bcy and right = acy *. bcx in
  let determinant = left -. right
  and permanent = abs_float left +. abs_float right in
  let error_bound = (3. +. (16. *. Float.epsilon)) *. Float.epsilon in
  if Float.is_finite determinant && Float.is_finite permanent
      && abs_float determinant > error_bound *. permanent
  then if determinant < 0. then Negative else Positive
  else exact_orient2d ~ax ~ay ~bx ~by ~cx ~cy

let exact_orient3d ~ax ~ay ~az ~bx ~by ~bz ~cx ~cy ~cz ~dx ~dy ~dz =
  sign_of_comparison (Dyadic.Scratch.orient3d
      ~ax ~ay ~az ~bx ~by ~bz ~cx ~cy ~cz ~dx ~dy ~dz)

let orient3d ~ax ~ay ~az ~bx ~by ~bz ~cx ~cy ~cz ~dx ~dy ~dz =
  if not (Float.is_finite ax && Float.is_finite ay && Float.is_finite az
      && Float.is_finite bx && Float.is_finite by && Float.is_finite bz
      && Float.is_finite cx && Float.is_finite cy && Float.is_finite cz
      && Float.is_finite dx && Float.is_finite dy && Float.is_finite dz) then
    invalid_arg "Rdk.Predicates.orient3d: coordinates must be finite";
  let adx = ax -. dx and ady = ay -. dy and adz = az -. dz
  and bdx = bx -. dx and bdy = by -. dy and bdz = bz -. dz
  and cdx = cx -. dx and cdy = cy -. dy and cdz = cz -. dz in
  let bdxcdy = bdx *. cdy and cdxbdy = cdx *. bdy
  and cdxady = cdx *. ady and adxcdy = adx *. cdy
  and adxbdy = adx *. bdy and bdxady = bdx *. ady in
  let determinant =
    (adz *. (bdxcdy -. cdxbdy))
    +. (bdz *. (cdxady -. adxcdy))
    +. (cdz *. (adxbdy -. bdxady)) in
  let permanent =
    ((abs_float bdxcdy +. abs_float cdxbdy) *. abs_float adz)
    +. ((abs_float cdxady +. abs_float adxcdy) *. abs_float bdz)
    +. ((abs_float adxbdy +. abs_float bdxady) *. abs_float cdz) in
  let error_bound = (7. +. (56. *. Float.epsilon)) *. Float.epsilon in
  if Float.is_finite determinant && Float.is_finite permanent
      && abs_float determinant > error_bound *. permanent
  then if determinant < 0. then Negative else Positive
  else exact_orient3d ~ax ~ay ~az ~bx ~by ~bz ~cx ~cy ~cz ~dx ~dy ~dz

let orient3d_packed ~x ~y ~z a b c d =
  let ax = x.(a) and ay = y.(a) and az = z.(a)
  and bx = x.(b) and by = y.(b) and bz = z.(b)
  and cx = x.(c) and cy = y.(c) and cz = z.(c)
  and dx = x.(d) and dy = y.(d) and dz = z.(d) in
  if not (Float.is_finite ax && Float.is_finite ay && Float.is_finite az
      && Float.is_finite bx && Float.is_finite by && Float.is_finite bz
      && Float.is_finite cx && Float.is_finite cy && Float.is_finite cz
      && Float.is_finite dx && Float.is_finite dy && Float.is_finite dz) then
    invalid_arg "Rdk.Predicates.orient3d_packed: coordinates must be finite";
  let adx = ax -. dx and ady = ay -. dy and adz = az -. dz
  and bdx = bx -. dx and bdy = by -. dy and bdz = bz -. dz
  and cdx = cx -. dx and cdy = cy -. dy and cdz = cz -. dz in
  let bdxcdy = bdx *. cdy and cdxbdy = cdx *. bdy
  and cdxady = cdx *. ady and adxcdy = adx *. cdy
  and adxbdy = adx *. bdy and bdxady = bdx *. ady in
  let determinant =
    (adz *. (bdxcdy -. cdxbdy))
    +. (bdz *. (cdxady -. adxcdy))
    +. (cdz *. (adxbdy -. bdxady)) in
  let permanent =
    ((abs_float bdxcdy +. abs_float cdxbdy) *. abs_float adz)
    +. ((abs_float cdxady +. abs_float adxcdy) *. abs_float bdz)
    +. ((abs_float adxbdy +. abs_float bdxady) *. abs_float cdz) in
  let error_bound = (7. +. (56. *. Float.epsilon)) *. Float.epsilon in
  if Float.is_finite determinant && Float.is_finite permanent
      && abs_float determinant > error_bound *. permanent
  then if determinant < 0. then Negative else Positive
  else exact_orient3d ~ax ~ay ~az ~bx ~by ~bz ~cx ~cy ~cz ~dx ~dy ~dz

let exact_incircle ~ax ~ay ~bx ~by ~cx ~cy ~dx ~dy =
  sign_of_comparison (Dyadic.Scratch.incircle
      ~ax ~ay ~bx ~by ~cx ~cy ~dx ~dy)

let[@inline] incircle_values ax ay bx by cx cy dx dy =
  if not (Float.is_finite ax && Float.is_finite ay
      && Float.is_finite bx && Float.is_finite by
      && Float.is_finite cx && Float.is_finite cy
      && Float.is_finite dx && Float.is_finite dy) then
    invalid_arg "Rdk.Predicates.incircle: coordinates must be finite";
  let adx = ax -. dx and ady = ay -. dy
  and bdx = bx -. dx and bdy = by -. dy
  and cdx = cx -. dx and cdy = cy -. dy in
  let bdxcdy = bdx *. cdy and cdxbdy = cdx *. bdy
  and cdxady = cdx *. ady and adxcdy = adx *. cdy
  and adxbdy = adx *. bdy and bdxady = bdx *. ady in
  let alift = (adx *. adx) +. (ady *. ady)
  and blift = (bdx *. bdx) +. (bdy *. bdy)
  and clift = (cdx *. cdx) +. (cdy *. cdy) in
  let determinant =
    (alift *. (bdxcdy -. cdxbdy))
    +. (blift *. (cdxady -. adxcdy))
    +. (clift *. (adxbdy -. bdxady)) in
  let permanent =
    ((abs_float bdxcdy +. abs_float cdxbdy) *. alift)
    +. ((abs_float cdxady +. abs_float adxcdy) *. blift)
    +. ((abs_float adxbdy +. abs_float bdxady) *. clift) in
  let error_bound = (10. +. (96. *. Float.epsilon)) *. Float.epsilon in
  if Float.is_finite determinant && Float.is_finite permanent
      && abs_float determinant > error_bound *. permanent then
    if determinant < 0. then Negative else Positive
  else exact_incircle ~ax ~ay ~bx ~by ~cx ~cy ~dx ~dy

let[@inline] incircle_packed ~x ~y a b c d =
  incircle_values x.(a) y.(a) x.(b) y.(b) x.(c) y.(c) x.(d) y.(d)

let polygon_area_sign_packed ~x ~y ~points ~first ~count =
  if count < 3 || first < 0 || first > Array.length points - count then
    invalid_arg "Rdk.Predicates.polygon_area_sign_packed: invalid polygon range";
  let sum = ref 0. and correction = ref 0. and permanent = ref 0. in
  for local = 0 to count - 1 do
    let next = if local + 1 = count then 0 else local + 1 in
    let a = points.(first + local) and b = points.(first + next) in
    if a < 0 || a >= Array.length x || b < 0 || b >= Array.length x
        || Array.length y <> Array.length x then
      invalid_arg "Rdk.Predicates.polygon_area_sign_packed: invalid packed planes";
    let ax = x.(a) and ay = y.(a) and bx = x.(b) and by = y.(b) in
    if not (Float.is_finite ax && Float.is_finite ay
        && Float.is_finite bx && Float.is_finite by) then
      invalid_arg "Rdk.Predicates.polygon_area_sign_packed: coordinates must be finite";
    let left = ax *. by and right = bx *. ay in
    let term = left -. right in
    let adjusted = term -. !correction in
    let next_sum = !sum +. adjusted in
    correction := (next_sum -. !sum) -. adjusted;
    sum := next_sum;
    permanent := !permanent +. abs_float left +. abs_float right
  done;
  let operations = (6. *. float_of_int count) +. 2. in
  let product = operations *. Float.epsilon in
  let error_bound = if product < 0.5 then product /. (1. -. product) else infinity in
  if Float.is_finite !sum && Float.is_finite !permanent
      && abs_float !sum > error_bound *. !permanent then
    if !sum < 0. then Negative else Positive
  else sign_of_comparison (Dyadic.Scratch.polygon_area_packed
      ~x ~y ~points ~first ~count)

let[@inline always] opposite_signs left right = match left, right with
  | Negative, Positive | Positive, Negative -> true
  | Negative, Negative | Negative, Zero | Zero, Negative
  | Zero, Zero | Zero, Positive | Positive, Zero | Positive, Positive -> false

let[@inline always] projected_segment_orientation projection
    ax ay az bx by bz cx cy cz =
  if projection = 0 then orient2d ~ax ~ay ~bx ~by ~cx ~cy
  else if projection = 1 then orient2d
      ~ax:ay ~ay:az ~bx:by ~by:bz ~cx:cy ~cy:cz
  else orient2d ~ax:az ~ay:ax ~bx:bz ~by:bx ~cx:cz ~cy:cx

let[@inline always] projected_on_segment projection
    ax ay az bx by bz px py pz =
  if projection = 0 then
    px >= Float.min ax bx && px <= Float.max ax bx
    && py >= Float.min ay by && py <= Float.max ay by
  else if projection = 1 then
    py >= Float.min ay by && py <= Float.max ay by
    && pz >= Float.min az bz && pz <= Float.max az bz
  else
    pz >= Float.min az bz && pz <= Float.max az bz
    && px >= Float.min ax bx && px <= Float.max ax bx

let[@inline always] projected_segment_contact projection first second third fourth
    lax lay laz lbx lby lbz rax ray raz rbx rby rbz =
  if opposite_signs first second && opposite_signs third fourth then
    Segments_point
  else if (first = Zero
      && projected_on_segment projection lax lay laz lbx lby lbz rax ray raz)
      || (second = Zero
          && projected_on_segment projection lax lay laz lbx lby lbz rbx rby rbz)
      || (third = Zero
          && projected_on_segment projection rax ray raz rbx rby rbz lax lay laz)
      || (fourth = Zero
          && projected_on_segment projection rax ray raz rbx rby rbz lbx lby lbz)
  then Segments_point else Segments_disjoint

let[@inline always] collinear_segment_contact
    lax lay laz lbx lby lbz rax ray raz rbx rby rbz =
  let first, second, third, fourth =
    if lax <> lbx || rax <> rbx then lax, lbx, rax, rbx
    else if lay <> lby || ray <> rby then lay, lby, ray, rby
    else laz, lbz, raz, rbz in
  let lower = Float.max (Float.min first second) (Float.min third fourth)
  and upper = Float.min (Float.max first second) (Float.max third fourth) in
  if lower > upper then Segments_disjoint
  else if lower = upper then Segments_point else Segments_overlap

let segment_segment_packed ~x ~y ~z ~left_a ~left_b ~right_a ~right_b =
  let lax = x.(left_a) and lay = y.(left_a) and laz = z.(left_a)
  and lbx = x.(left_b) and lby = y.(left_b) and lbz = z.(left_b)
  and rax = x.(right_a) and ray = y.(right_a) and raz = z.(right_a)
  and rbx = x.(right_b) and rby = y.(right_b) and rbz = z.(right_b) in
  if not (Float.is_finite lax && Float.is_finite lay && Float.is_finite laz
      && Float.is_finite lbx && Float.is_finite lby && Float.is_finite lbz
      && Float.is_finite rax && Float.is_finite ray && Float.is_finite raz
      && Float.is_finite rbx && Float.is_finite rby && Float.is_finite rbz) then
    invalid_arg "Rdk.Predicates.segment_segment_packed: coordinates must be finite";
  if (lax = lbx && lay = lby && laz = lbz)
      || (rax = rbx && ray = rby && raz = rbz) then Segments_degenerate
  else if orient3d ~ax:lax ~ay:lay ~az:laz ~bx:lbx ~by:lby ~bz:lbz
      ~cx:rax ~cy:ray ~cz:raz ~dx:rbx ~dy:rby ~dz:rbz <> Zero then
    Segments_disjoint
  else begin
    let xy_first = projected_segment_orientation 0
        lax lay laz lbx lby lbz rax ray raz
    and xy_second = projected_segment_orientation 0
        lax lay laz lbx lby lbz rbx rby rbz
    and xy_third = projected_segment_orientation 0
        rax ray raz rbx rby rbz lax lay laz
    and xy_fourth = projected_segment_orientation 0
        rax ray raz rbx rby rbz lbx lby lbz in
    if xy_first <> Zero || xy_second <> Zero
        || xy_third <> Zero || xy_fourth <> Zero then
      projected_segment_contact 0 xy_first xy_second xy_third xy_fourth
        lax lay laz lbx lby lbz rax ray raz rbx rby rbz
    else begin
      let yz_first = projected_segment_orientation 1
          lax lay laz lbx lby lbz rax ray raz
      and yz_second = projected_segment_orientation 1
          lax lay laz lbx lby lbz rbx rby rbz
      and yz_third = projected_segment_orientation 1
          rax ray raz rbx rby rbz lax lay laz
      and yz_fourth = projected_segment_orientation 1
          rax ray raz rbx rby rbz lbx lby lbz in
      if yz_first <> Zero || yz_second <> Zero
          || yz_third <> Zero || yz_fourth <> Zero then
        projected_segment_contact 1 yz_first yz_second yz_third yz_fourth
          lax lay laz lbx lby lbz rax ray raz rbx rby rbz
      else begin
        let zx_first = projected_segment_orientation 2
            lax lay laz lbx lby lbz rax ray raz
        and zx_second = projected_segment_orientation 2
            lax lay laz lbx lby lbz rbx rby rbz
        and zx_third = projected_segment_orientation 2
            rax ray raz rbx rby rbz lax lay laz
        and zx_fourth = projected_segment_orientation 2
            rax ray raz rbx rby rbz lbx lby lbz in
        if zx_first <> Zero || zx_second <> Zero
            || zx_third <> Zero || zx_fourth <> Zero then
          projected_segment_contact 2 zx_first zx_second zx_third zx_fourth
            lax lay laz lbx lby lbz rax ray raz rbx rby rbz
        else collinear_segment_contact
            lax lay laz lbx lby lbz rax ray raz rbx rby rbz
      end
    end
  end

let[@inline always] same_nonzero_side left right =
  match left, right with
  | Negative, Negative | Positive, Positive -> true
  | _ -> false

let[@inline always] compatible_sides first second third =
  let has_negative = first = Negative || second = Negative || third = Negative
  and has_positive = first = Positive || second = Positive || third = Positive in
  not (has_negative && has_positive)

let triangle_location_code first second third =
  if not (compatible_sides first second third) then -1
  else match first, second, third with
  | Zero, Zero, Zero -> -1
  | Zero, Zero, _ -> 5
  | _, Zero, Zero -> 6
  | Zero, _, Zero -> 4
  | Zero, _, _ -> 1
  | _, Zero, _ -> 2
  | _, _, Zero -> 3
  | _ -> 0

let projected_triangle_location ~x ~y ~z a b c point =
  let classify first second third orientation =
    let first, second, third = if orientation = Negative then
        (match first with Negative -> Positive | Positive -> Negative | Zero -> Zero),
        (match second with Negative -> Positive | Positive -> Negative | Zero -> Zero),
        (match third with Negative -> Positive | Positive -> Negative | Zero -> Zero)
      else first, second, third in
    triangle_location_code first second third in
  let xy = orient2d_packed ~x ~y a b c in
  if xy <> Zero then classify
      (orient2d_packed ~x ~y a b point)
      (orient2d_packed ~x ~y b c point)
      (orient2d_packed ~x ~y c a point) xy
  else
    let yz = orient2d_packed ~x:y ~y:z a b c in
    if yz <> Zero then classify
        (orient2d_packed ~x:y ~y:z a b point)
        (orient2d_packed ~x:y ~y:z b c point)
        (orient2d_packed ~x:y ~y:z c a point) yz
    else
      let zx = orient2d_packed ~x:z ~y:x a b c in
      if zx <> Zero then classify
          (orient2d_packed ~x:z ~y:x a b point)
          (orient2d_packed ~x:z ~y:x b c point)
          (orient2d_packed ~x:z ~y:x c a point) zx
      else -1

let segment_triangle_code_packed ~x ~y ~z ~segment_start ~segment_end
    ~triangle_a ~triangle_b ~triangle_c =
  let a = triangle_a and b = triangle_b and c = triangle_c
  and p = segment_start and q = segment_end in
  let triangle_degenerate =
    orient2d_packed ~x ~y a b c = Zero
    && orient2d_packed ~x:y ~y:z a b c = Zero
    && orient2d_packed ~x:z ~y:x a b c = Zero in
  if triangle_degenerate then 2
  else
    let start_side = orient3d_packed ~x ~y ~z a b c p
    and end_side = orient3d_packed ~x ~y ~z a b c q in
    if start_side = Zero && end_side = Zero then 1
    else if same_nonzero_side start_side end_side then 0
    else if start_side = Zero then
      let location = projected_triangle_location ~x ~y ~z a b c p in
      if location < 0 then 0 else 3 + location
    else if end_side = Zero then
      let location = projected_triangle_location ~x ~y ~z a b c q in
      if location < 0 then 0 else 10 + location
    else
      let ab = orient3d_packed ~x ~y ~z p q a b
      and bc = orient3d_packed ~x ~y ~z p q b c
      and ca = orient3d_packed ~x ~y ~z p q c a in
      let location = triangle_location_code ab bc ca in
      if location < 0 then 0 else 17 + location

let triangle_degenerate_packed ~x ~y ~z a b c =
  orient2d_packed ~x ~y a b c = Zero
  && orient2d_packed ~x:y ~y:z a b c = Zero
  && orient2d_packed ~x:z ~y:x a b c = Zero

let[@inline always] encoded_segment_location code =
  if code < 10 then 0 else if code < 17 then 1 else 2

let[@inline always] encoded_triangle_location code =
  if code < 10 then code - 3 else if code < 17 then code - 10 else code - 17

let[@inline always] feature_of_segment_location edge = function
  | 0 -> edge
  | 1 -> (edge + 1) mod 3
  | _ -> 4 + edge

let[@inline always] feature_of_triangle_location = function
  | 0 -> 8
  | 1 -> 4 | 2 -> 5 | 3 -> 6
  | 4 -> 0 | 5 -> 1 | 6 -> 2
  | _ -> invalid_arg "Rdk.Predicates: invalid encoded triangle location"

let add_feature_pair output count left right =
  let duplicate = ref false and event = ref 0 in
  while not !duplicate && !event < count do
    duplicate := output.(!event * 2) = left
        && output.((!event * 2) + 1) = right;
    incr event
  done;
  if !duplicate then count
  else if count = 2 then invalid_arg
      "Rdk.Predicates: non-coplanar triangles produced more than two intersection points"
  else begin
    output.(count * 2) <- left;
    output.((count * 2) + 1) <- right;
    count + 1
  end

let[@inline always] triangle_vertex a b c = function
  | 0 -> a | 1 -> b | _ -> c

let triangle_triangle_features_into ~x ~y ~z ~left_a ~left_b ~left_c
    ~right_a ~right_b ~right_c output =
  if Array.length output < 4 then invalid_arg
      "Rdk.Predicates.triangle_triangle_features_into: output is too small";
  if triangle_degenerate_packed ~x ~y ~z left_a left_b left_c
      || triangle_degenerate_packed ~x ~y ~z right_a right_b right_c then -2
  else if orient3d_packed ~x ~y ~z left_a left_b left_c right_a = Zero
      && orient3d_packed ~x ~y ~z left_a left_b left_c right_b = Zero
      && orient3d_packed ~x ~y ~z left_a left_b left_c right_c = Zero then -1
  else begin
    let count = ref 0 in
    for edge = 0 to 2 do
      if !count < 2 then begin
        let code = segment_triangle_code_packed ~x ~y ~z
            ~segment_start:(triangle_vertex left_a left_b left_c edge)
            ~segment_end:(triangle_vertex left_a left_b left_c
              ((edge + 1) mod 3))
            ~triangle_a:right_a ~triangle_b:right_b ~triangle_c:right_c in
        if code >= 3 then begin
          let left_feature = feature_of_segment_location edge
              (encoded_segment_location code)
          and right_feature = feature_of_triangle_location
              (encoded_triangle_location code) in
          count := add_feature_pair output !count left_feature right_feature
        end
      end
    done;
    for edge = 0 to 2 do
      if !count < 2 then begin
        let code = segment_triangle_code_packed ~x ~y ~z
            ~segment_start:(triangle_vertex right_a right_b right_c edge)
            ~segment_end:(triangle_vertex right_a right_b right_c
              ((edge + 1) mod 3))
            ~triangle_a:left_a ~triangle_b:left_b ~triangle_c:left_c in
        if code >= 3 then begin
          let left_feature = feature_of_triangle_location
              (encoded_triangle_location code)
          and right_feature = feature_of_segment_location edge
              (encoded_segment_location code) in
          count := add_feature_pair output !count left_feature right_feature
        end
      end
    done;
    if !count = 2 then begin
      let left0 = output.(0) and right0 = output.(1)
      and left1 = output.(2) and right1 = output.(3) in
      if left1 < left0 || (left1 = left0 && right1 < right0) then begin
        output.(0) <- left1; output.(1) <- right1;
        output.(2) <- left0; output.(3) <- right0
      end
    end;
    !count
  end

let[@inline always] projected_orient projection x y z a b c =
  if projection = 0 then orient2d_packed ~x ~y a b c
  else if projection = 1 then orient2d_packed ~x:y ~y:z a b c
  else orient2d_packed ~x:z ~y:x a b c

let[@inline always] projected_u projection x y z point =
  if projection = 0 then x.(point)
  else if projection = 1 then y.(point)
  else z.(point)

let[@inline always] projected_v projection x y z point =
  if projection = 0 then y.(point)
  else if projection = 1 then z.(point)
  else x.(point)

let[@inline always] opposite_sign first second =
  (first = Positive && second = Negative)
  || (first = Negative && second = Positive)

let[@inline always] projected_on_segment projection x y z a b point =
  let au = projected_u projection x y z a
  and av = projected_v projection x y z a
  and bu = projected_u projection x y z b
  and bv = projected_v projection x y z b
  and pu = projected_u projection x y z point
  and pv = projected_v projection x y z point in
  pu >= min au bu && pu <= max au bu && pv >= min av bv && pv <= max av bv

let projected_segments_contact projection x y z a b c d =
  let abc = projected_orient projection x y z a b c
  and abd = projected_orient projection x y z a b d
  and cda = projected_orient projection x y z c d a
  and cdb = projected_orient projection x y z c d b in
  (opposite_sign abc abd && opposite_sign cda cdb)
  || (abc = Zero && projected_on_segment projection x y z a b c)
  || (abd = Zero && projected_on_segment projection x y z a b d)
  || (cda = Zero && projected_on_segment projection x y z c d a)
  || (cdb = Zero && projected_on_segment projection x y z c d b)

let projected_point_in_triangle projection x y z a b c point =
  let ab = projected_orient projection x y z a b point
  and bc = projected_orient projection x y z b c point
  and ca = projected_orient projection x y z c a point in
  (ab <> Negative && bc <> Negative && ca <> Negative)
  || (ab <> Positive && bc <> Positive && ca <> Positive)

let coplanar_triangles_contact_packed ~x ~y ~z ~left_a ~left_b ~left_c
    ~right_a ~right_b ~right_c =
  let xy = orient2d_packed ~x ~y left_a left_b left_c in
  let projection = if xy <> Zero then 0
    else if orient2d_packed ~x:y ~y:z left_a left_b left_c <> Zero then 1
    else if orient2d_packed ~x:z ~y:x left_a left_b left_c <> Zero then 2
    else -1 in
  if projection < 0 then false
  else begin
    let contact = ref false and first_edge = ref 0 in
    while not !contact && !first_edge < 3 do
      let second_edge = ref 0 in
      while not !contact && !second_edge < 3 do
        contact := projected_segments_contact projection x y z
            (triangle_vertex left_a left_b left_c !first_edge)
            (triangle_vertex left_a left_b left_c ((!first_edge + 1) mod 3))
            (triangle_vertex right_a right_b right_c !second_edge)
            (triangle_vertex right_a right_b right_c ((!second_edge + 1) mod 3));
        incr second_edge
      done;
      incr first_edge
    done;
    !contact
    || projected_point_in_triangle projection x y z
         left_a left_b left_c right_a
    || projected_point_in_triangle projection x y z
         right_a right_b right_c left_a
  end

module Private = struct
  let triangle_feature_vertex_local feature =
    if feature >= 0 && feature <= 2
    then feature else -1

  let triangle_feature_edge_local feature =
    if feature >= 4 && feature <= 6
    then feature - 4 else -1

  let segment_triangle_code_packed = segment_triangle_code_packed
  let triangle_triangle_features_into = triangle_triangle_features_into
  let coplanar_triangles_contact_packed = coplanar_triangles_contact_packed
  let exact_orient2d_arena = exact_orient2d
  let exact_orient3d_arena = exact_orient3d
  let exact_incircle_arena = exact_incircle
end
