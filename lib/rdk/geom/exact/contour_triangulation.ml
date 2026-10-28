let epsilon = 1e-12
let fail message = raise (Error.Fail message)

let[@inline] cross x y a b c =
  ((x.(b) -. x.(a)) *. (y.(c) -. y.(a))) -. ((y.(b) -. y.(a)) *. (x.(c) -. x.(a)))

let[@inline] same_point x y a b =
  abs_float (x.(a) -. x.(b)) <= epsilon && abs_float (y.(a) -. y.(b)) <= epsilon

let signed_area x y contour =
  let area = ref 0. and count = Array.length contour in
  for index = 0 to count - 1 do
    let next = if index + 1 = count then 0 else index + 1 in
    let a = contour.(index) and b = contour.(next) in
    area := !area +. ((x.(a) *. y.(b)) -. (x.(b) *. y.(a)))
  done;
  !area

let oriented x y ~counter_clockwise contour =
  let area = signed_area x y contour in
  if (not (Float.is_finite area)) || abs_float area <= epsilon then
    fail "contour has degenerate projected area";
  if area > 0. = counter_clockwise then contour
  else Array.init (Array.length contour) (fun index -> contour.(Array.length contour - index - 1))

let point_inside x y contour px py =
  let inside = ref false
  and count = Array.length contour
  and previous = ref (Array.length contour - 1) in
  for current = 0 to count - 1 do
    let a = contour.(!previous) and b = contour.(current) in
    if
      y.(a) > py <> (y.(b) > py)
      && px < ((x.(b) -. x.(a)) *. (py -. y.(a)) /. (y.(b) -. y.(a))) +. x.(a)
    then inside := not !inside;
    previous := current
  done;
  !inside

let[@inline] between value left right =
  value >= Float.min left right -. epsilon && value <= Float.max left right +. epsilon

let point_on_segment x y point a b =
  abs_float (cross x y a b point) <= epsilon
  && between x.(point) x.(a) x.(b)
  && between y.(point) y.(a) y.(b)

let segments_intersect x y a b c d =
  let ab_c = cross x y a b c
  and ab_d = cross x y a b d
  and cd_a = cross x y c d a
  and cd_b = cross x y c d b in
  ((ab_c > epsilon && ab_d < -.epsilon) || (ab_c < -.epsilon && ab_d > epsilon))
  && ((cd_a > epsilon && cd_b < -.epsilon) || (cd_a < -.epsilon && cd_b > epsilon))
  || (abs_float ab_c <= epsilon && point_on_segment x y c a b)
  || (abs_float ab_d <= epsilon && point_on_segment x y d a b)
  || (abs_float cd_a <= epsilon && point_on_segment x y a c d)
  || (abs_float cd_b <= epsilon && point_on_segment x y b c d)

let clear_of_contour x y ~a ~b contour =
  let clear = ref true and count = Array.length contour in
  for edge = 0 to count - 1 do
    if !clear then begin
      let c = contour.(edge) and d = contour.(if edge + 1 = count then 0 else edge + 1) in
      if c <> a && d <> a && c <> b && d <> b && segments_intersect x y a b c d then clear := false
    end
  done;
  !clear

let splice outer outer_index hole hole_index =
  let outer_count = Array.length outer and hole_count = Array.length hole in
  Array.init
    (outer_count + hole_count + 2)
    (fun output ->
      if output <= outer_index then outer.(output)
      else begin
        let local = output - outer_index - 1 in
        if local <= hole_count then hole.((hole_index + local) mod hole_count)
        else outer.(outer_index + local - hole_count - 1)
      end)

let triangulate ?cancel ~point ~outer ~holes () =
  try
    if Array.length outer < 3 then fail "outer contour has fewer than three points";
    Array.iteri
      (fun hole contour ->
        if Array.length contour < 3 then
          fail (Printf.sprintf "hole contour %d has fewer than three points" hole))
      holes;
    let original_count =
      Array.length outer
      + Array.fold_left (fun total contour -> total + Array.length contour) 0 holes
    in
    if Array.length holes > (Sys.max_array_length - original_count) / 2 then
      fail "bridged contour exceeds OCaml array limits";
    let token = Array.make original_count 0
    and raw_x = Array.make original_count 0.
    and raw_y = Array.make original_count 0.
    and at = ref 0 in
    let copy_contour contour =
      let local = Array.make (Array.length contour) 0 in
      Array.iteri
        (fun index value ->
          Cancel.check_opt cancel;
          let px, py = point value in
          if not (Float.is_finite px && Float.is_finite py) then
            fail "contour contains a non-finite projected point";
          token.(!at) <- value;
          raw_x.(!at) <- px;
          raw_y.(!at) <- py;
          local.(index) <- !at;
          incr at)
        contour;
      local
    in
    let outer = copy_contour outer and holes = Array.map copy_contour holes in
    let coordinate_scale = ref 0. in
    for index = 0 to original_count - 1 do
      coordinate_scale := Float.max !coordinate_scale (abs_float raw_x.(index));
      coordinate_scale := Float.max !coordinate_scale (abs_float raw_y.(index))
    done;
    if !coordinate_scale = 0. then fail "contours have zero coordinate extent";
    let x = Array.make original_count 0.
    and y = Array.make original_count 0.
    and min_x = ref Float.infinity
    and max_x = ref Float.neg_infinity
    and min_y = ref Float.infinity
    and max_y = ref Float.neg_infinity in
    for index = 0 to original_count - 1 do
      x.(index) <- raw_x.(index) /. !coordinate_scale;
      y.(index) <- raw_y.(index) /. !coordinate_scale;
      min_x := Float.min !min_x x.(index);
      max_x := Float.max !max_x x.(index);
      min_y := Float.min !min_y y.(index);
      max_y := Float.max !max_y y.(index)
    done;
    let projection_scale = Float.max (!max_x -. !min_x) (!max_y -. !min_y) in
    if projection_scale = 0. || not (Float.is_finite projection_scale) then
      fail "contours have degenerate projected extent";
    for index = 0 to original_count - 1 do
      x.(index) <- (x.(index) -. !min_x) /. projection_scale;
      y.(index) <- (y.(index) -. !min_y) /. projection_scale
    done;
    let outer = oriented x y ~counter_clockwise:true outer
    and holes = Array.map (oriented x y ~counter_clockwise:false) holes in
    let contours = Array.append [| outer |] holes in
    let rightmost contour =
      let best = ref 0 in
      for index = 1 to Array.length contour - 1 do
        let point = contour.(index) and current = contour.(!best) in
        if x.(point) > x.(current) || (x.(point) = x.(current) && y.(point) < y.(current)) then
          best := index
      done;
      !best
    in
    let hole_bridge = Array.map rightmost holes in
    let order = Array.init (Array.length holes) Fun.id in
    Array.sort
      (fun left right ->
        let lp = holes.(left).(hole_bridge.(left)) and rp = holes.(right).(hole_bridge.(right)) in
        let compared = Float.compare x.(rp) x.(lp) in
        if compared <> 0 then compared
        else
          let compared = Float.compare y.(lp) y.(rp) in
          if compared <> 0 then compared else Int.compare left right)
      order;
    let ring = ref outer in
    Array.iter
      (fun hole_number ->
        Cancel.check_opt cancel;
        let hole = holes.(hole_number) and hole_index = hole_bridge.(hole_number) in
        let h = hole.(hole_index) and best = ref (-1) and best_distance = ref Float.infinity in
        for candidate = 0 to Array.length !ring - 1 do
          let v = !ring.(candidate) in
          if not (same_point x y h v) then begin
            let mx = (x.(h) +. x.(v)) *. 0.5 and my = (y.(h) +. y.(v)) *. 0.5 in
            let in_material =
              point_inside x y outer mx my
              && not (Array.exists (fun contour -> point_inside x y contour mx my) holes)
            in
            let clear =
              in_material
              && clear_of_contour x y ~a:h ~b:v !ring
              && Array.for_all (clear_of_contour x y ~a:h ~b:v) contours
            in
            if clear then begin
              let dx = x.(h) -. x.(v) and dy = y.(h) -. y.(v) in
              let distance = (dx *. dx) +. (dy *. dy) in
              if
                distance < !best_distance -. epsilon
                || abs_float (distance -. !best_distance) <= epsilon
                   && (!best < 0 || candidate < !best)
              then begin
                best := candidate;
                best_distance := distance
              end
            end
          end
        done;
        if !best < 0 then
          fail (Printf.sprintf "hole contour %d has no non-crossing visibility bridge" hole_number);
        ring := splice !ring !best hole hole_index)
      order;
    let ring = !ring and ring_count = Array.length !ring in
    if ring_count > Sys.max_array_length / 3 then
      fail "triangulation output exceeds OCaml array limits";
    let output = Array.make ((ring_count - 2) * 3) 0
    and remaining = Array.copy ring
    and active = ref ring_count
    and emitted = ref 0 in
    while !active > 3 do
      Cancel.check_opt cancel;
      let ear =
        Polygon_triangulation.find_ear
          ~distinct:(fun p a b c ->
            not (same_point x y p a || same_point x y p b || same_point x y p c))
          ~u:x ~v:y ~base:0 ~orientation:1. ~remaining ~active:!active
      in
      if ear < 0 then fail "bridged contours are non-simple or cannot be triangulated";
      let a = remaining.((ear + !active - 1) mod !active)
      and b = remaining.(ear)
      and c = remaining.((ear + 1) mod !active) in
      let base = !emitted * 3 in
      output.(base) <- token.(a);
      output.(base + 1) <- token.(b);
      output.(base + 2) <- token.(c);
      Array.blit remaining (ear + 1) remaining ear (!active - ear - 1);
      decr active;
      incr emitted
    done;
    let base = !emitted * 3 in
    output.(base) <- token.(remaining.(0));
    output.(base + 1) <- token.(remaining.(1));
    output.(base + 2) <- token.(remaining.(2));
    Ok output
  with Error.Fail message -> Error message
