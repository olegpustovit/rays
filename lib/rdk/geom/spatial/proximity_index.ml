type features = {
  positions : Packed.Float3.View.view;
  entity_count : int;
  offsets : int array;
  entities : int array;
  kinds : bytes;
  a : int array;
  b : int array;
  c : int array;
}

type t = {
  features : features;
  order : int array;
  min_x : float array;
  min_y : float array;
  min_z : float array;
  max_x : float array;
  max_y : float array;
  max_z : float array;
  left : int array;
  right : int array;
  first : int array;
  count : int array;
}

let segment = '\000'
let triangle = '\001'
let feature_count features = Array.length features.a

let validate_point positions point label =
  let x = positions.Packed.Float3.View.x.(point)
  and y = positions.y.(point)
  and z = positions.z.(point) in
  if not (Float.is_finite x && Float.is_finite y && Float.is_finite z) then
    raise (Error.Fail (Printf.sprintf "%s references non-finite point %d" label point))

let primitive_features ?cancel ?(grain = 16_384) geometry =
  try
    if grain <= 0 then invalid_arg "Proximity index: grain must be positive";
    let positions = Packed.Float3.View.view (Geometry.positions geometry)
    and topology = Topology.View.view (Geometry.topology geometry) in
    let entity_count = Geometry.primitive_count geometry in
    let offsets = Array.make (entity_count + 1) 0 in
    for primitive = 0 to entity_count - 1 do
      Cancel.poll cancel primitive 4095;
      let corners =
        topology.primitive_offsets.(primitive + 1) - topology.primitive_offsets.(primitive)
      in
      let count =
        match Char.code (Bytes.unsafe_get topology.primitive_kinds primitive) with
        | 0 -> corners - 2
        | 1 -> corners - 1
        | _ -> corners
      in
      if count > Sys.max_array_length - offsets.(primitive) then
        raise (Error.Fail "primitive feature count exceeds array limits");
      offsets.(primitive + 1) <- offsets.(primitive) + count
    done;
    let total = offsets.(entity_count) in
    let entities = Array.make total 0
    and kinds = Bytes.make total segment
    and a = Array.make total 0
    and b = Array.make total 0
    and c = Array.make total (-1) in
    let ranges = Support.ceiling_div entity_count grain in
    let failures = Array.make ranges None in
    Support.run_indexed_ranges ~grain entity_count
      (fun ~range ~first:first_primitive ~last:last_primitive ->
        let scratch = Polygon_triangulation.create_scratch () in
        for primitive = first_primitive to last_primitive - 1 do
          Cancel.poll cancel primitive 4095;
          if failures.(range) = None then begin
            let first = topology.primitive_offsets.(primitive)
            and last = topology.primitive_offsets.(primitive + 1)
            and at = offsets.(primitive) in
            match Char.code (Bytes.unsafe_get topology.primitive_kinds primitive) with
            | 0 -> (
                let emit local va vb vc =
                  let feature = at + local in
                  entities.(feature) <- primitive;
                  Bytes.unsafe_set kinds feature triangle;
                  a.(feature) <- topology.vertex_points.(va);
                  b.(feature) <- topology.vertex_points.(vb);
                  c.(feature) <- topology.vertex_points.(vc)
                in
                match
                  Polygon_triangulation.primitive ?cancel ~positions ~topology ~scratch primitive
                    ~emit
                with
                | Ok () -> ()
                | Error message -> failures.(range) <- Some message)
            | kind ->
                let closed = kind = 2 in
                for local = 0 to last - first - 2 do
                  let feature = at + local in
                  entities.(feature) <- primitive;
                  a.(feature) <- topology.vertex_points.(first + local);
                  b.(feature) <- topology.vertex_points.(first + local + 1)
                done;
                if closed then begin
                  let feature = offsets.(primitive + 1) - 1 in
                  entities.(feature) <- primitive;
                  a.(feature) <- topology.vertex_points.(last - 1);
                  b.(feature) <- topology.vertex_points.(first)
                end
          end
        done);
    let failure = ref None in
    Array.iter
      (fun item ->
        match (!failure, item) with
        | None, Some message -> failure := Some message
        | None, None | Some _, _ -> ())
      failures;
    Option.iter (fun message -> raise (Error.Fail message)) !failure;
    for feature = 0 to total - 1 do
      Cancel.poll cancel feature 4095;
      validate_point positions a.(feature) "primitive";
      validate_point positions b.(feature) "primitive";
      if Bytes.unsafe_get kinds feature = triangle then begin
        validate_point positions c.(feature) "primitive";
        let pa = a.(feature) and pb = b.(feature) and pc = c.(feature) in
        let abx = positions.x.(pb) -. positions.x.(pa)
        and aby = positions.y.(pb) -. positions.y.(pa)
        and abz = positions.z.(pb) -. positions.z.(pa)
        and acx = positions.x.(pc) -. positions.x.(pa)
        and acy = positions.y.(pc) -. positions.y.(pa)
        and acz = positions.z.(pc) -. positions.z.(pa) in
        let nx = (aby *. acz) -. (abz *. acy)
        and ny = (abz *. acx) -. (abx *. acz)
        and nz = (abx *. acy) -. (aby *. acx) in
        let area_squared = (nx *. nx) +. (ny *. ny) +. (nz *. nz) in
        if (not (Float.is_finite area_squared)) || area_squared <= 1e-30 then
          raise (Error.Fail (Printf.sprintf "primitive feature %d is degenerate" feature))
      end
    done;
    Ok { positions; entity_count; offsets; entities; kinds; a; b; c }
  with
  | Error.Fail message -> Error message
  | Invalid_argument message -> Error message

let edge_features ?cancel ?(grain = 16_384) geometry =
  try
    if grain <= 0 then invalid_arg "Proximity index: grain must be positive";
    let positions = Packed.Float3.View.view (Geometry.positions geometry) in
    let index = Topology_index.create ?cancel (Geometry.topology geometry) in
    let reverse = Topology_index.View.view index in
    let entity_count = Topology_index.edge_count index in
    let offsets = Array.init (entity_count + 1) Fun.id
    and entities = Array.init entity_count Fun.id
    and kinds = Bytes.make entity_count segment
    and a = Array.copy reverse.edge_a
    and b = Array.copy reverse.edge_b
    and c = Array.make entity_count (-1) in
    for edge = 0 to entity_count - 1 do
      Cancel.poll cancel edge 4095;
      validate_point positions a.(edge) "edge";
      validate_point positions b.(edge) "edge"
    done;
    Ok { positions; entity_count; offsets; entities; kinds; a; b; c }
  with
  | Error.Fail message -> Error message
  | Invalid_argument message -> Error message

let create ?cancel ?(grain = 16_384) features =
  if grain <= 0 then invalid_arg "Proximity index: grain must be positive";
  let total = feature_count features in
  if total = 0 then
    {
      features;
      order = [||];
      min_x = [||];
      min_y = [||];
      min_z = [||];
      max_x = [||];
      max_y = [||];
      max_z = [||];
      left = [||];
      right = [||];
      first = [||];
      count = [||];
    }
  else begin
    let centroid_x = Array.make total 0.
    and centroid_y = Array.make total 0.
    and centroid_z = Array.make total 0.
    and fmin_x = Array.make total 0.
    and fmin_y = Array.make total 0.
    and fmin_z = Array.make total 0.
    and fmax_x = Array.make total 0.
    and fmax_y = Array.make total 0.
    and fmax_z = Array.make total 0. in
    Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(total - 1) (fun feature ->
        Cancel.poll cancel feature 4095;
        let point_a = features.a.(feature) and point_b = features.b.(feature) in
        let ax = features.positions.x.(point_a)
        and ay = features.positions.y.(point_a)
        and az = features.positions.z.(point_a)
        and bx = features.positions.x.(point_b)
        and by = features.positions.y.(point_b)
        and bz = features.positions.z.(point_b) in
        let point_c = features.c.(feature) in
        let cx = if point_c >= 0 then features.positions.x.(point_c) else bx
        and cy = if point_c >= 0 then features.positions.y.(point_c) else by
        and cz = if point_c >= 0 then features.positions.z.(point_c) else bz in
        fmin_x.(feature) <- Float.min ax (Float.min bx cx);
        fmin_y.(feature) <- Float.min ay (Float.min by cy);
        fmin_z.(feature) <- Float.min az (Float.min bz cz);
        fmax_x.(feature) <- Float.max ax (Float.max bx cx);
        fmax_y.(feature) <- Float.max ay (Float.max by cy);
        fmax_z.(feature) <- Float.max az (Float.max bz cz);
        centroid_x.(feature) <- (ax /. 3.) +. (bx /. 3.) +. (cx /. 3.);
        centroid_y.(feature) <- (ay /. 3.) +. (by /. 3.) +. (cy /. 3.);
        centroid_z.(feature) <- (az /. 3.) +. (bz /. 3.) +. (cz /. 3.));
    let bounds =
      Bounds3_index.create ?cancel ~grain ~centroid_x ~centroid_y ~centroid_z ~item_min_x:fmin_x
        ~item_min_y:fmin_y ~item_min_z:fmin_z ~item_max_x:fmax_x ~item_max_y:fmax_y
        ~item_max_z:fmax_z ()
    in
    {
      features;
      order = bounds.order;
      min_x = bounds.min_x;
      min_y = bounds.min_y;
      min_z = bounds.min_z;
      max_x = bounds.max_x;
      max_y = bounds.max_y;
      max_z = bounds.max_z;
      left = bounds.left;
      right = bounds.right;
      first = bounds.first;
      count = bounds.count;
    }
  end

let[@inline] clamp01 value = if value <= 0. then 0. else if value >= 1. then 1. else value

let[@inline] point_point_squared px py pz qx qy qz =
  let dx = px -. qx and dy = py -. qy and dz = pz -. qz in
  (dx *. dx) +. (dy *. dy) +. (dz *. dz)

let[@inline] point_triangle_squared px py pz ax ay az bx by bz cx cy cz =
  let abx = bx -. ax
  and aby = by -. ay
  and abz = bz -. az
  and acx = cx -. ax
  and acy = cy -. ay
  and acz = cz -. az
  and apx = px -. ax
  and apy = py -. ay
  and apz = pz -. az in
  let d1 = (abx *. apx) +. (aby *. apy) +. (abz *. apz)
  and d2 = (acx *. apx) +. (acy *. apy) +. (acz *. apz) in
  if d1 <= 0. && d2 <= 0. then point_point_squared px py pz ax ay az
  else
    let bpx = px -. bx and bpy = py -. by and bpz = pz -. bz in
    let d3 = (abx *. bpx) +. (aby *. bpy) +. (abz *. bpz)
    and d4 = (acx *. bpx) +. (acy *. bpy) +. (acz *. bpz) in
    if d3 >= 0. && d4 <= d3 then point_point_squared px py pz bx by bz
    else
      let vc = (d1 *. d4) -. (d3 *. d2) in
      if vc <= 0. && d1 >= 0. && d3 <= 0. then
        let v = d1 /. (d1 -. d3) in
        point_point_squared px py pz (ax +. (v *. abx)) (ay +. (v *. aby)) (az +. (v *. abz))
      else
        let cpx = px -. cx and cpy = py -. cy and cpz = pz -. cz in
        let d5 = (abx *. cpx) +. (aby *. cpy) +. (abz *. cpz)
        and d6 = (acx *. cpx) +. (acy *. cpy) +. (acz *. cpz) in
        if d6 >= 0. && d5 <= d6 then point_point_squared px py pz cx cy cz
        else
          let vb = (d5 *. d2) -. (d1 *. d6) in
          if vb <= 0. && d2 >= 0. && d6 <= 0. then
            let w = d2 /. (d2 -. d6) in
            point_point_squared px py pz (ax +. (w *. acx)) (ay +. (w *. acy)) (az +. (w *. acz))
          else
            let va = (d3 *. d6) -. (d5 *. d4) in
            if va <= 0. && d4 -. d3 >= 0. && d5 -. d6 >= 0. then
              let w = (d4 -. d3) /. (d4 -. d3 +. (d5 -. d6)) in
              point_point_squared px py pz
                (bx +. (w *. (cx -. bx)))
                (by +. (w *. (cy -. by)))
                (bz +. (w *. (cz -. bz)))
            else
              let inverse = 1. /. (va +. vb +. vc) in
              let v = vb *. inverse and w = vc *. inverse in
              point_point_squared px py pz
                (ax +. (v *. abx) +. (w *. acx))
                (ay +. (v *. aby) +. (w *. acy))
                (az +. (v *. abz) +. (w *. acz))

let[@inline] segment_parameters_squared rx ry rz d1x d1y d1z d2x d2y d2z s t =
  let dx = rx +. (s *. d1x) -. (t *. d2x)
  and dy = ry +. (s *. d1y) -. (t *. d2y)
  and dz = rz +. (s *. d1z) -. (t *. d2z) in
  (dx *. dx) +. (dy *. dy) +. (dz *. dz)

let[@inline] segment_segment_squared p1x p1y p1z q1x q1y q1z p2x p2y p2z q2x q2y q2z =
  let d1x = q1x -. p1x
  and d1y = q1y -. p1y
  and d1z = q1z -. p1z
  and d2x = q2x -. p2x
  and d2y = q2y -. p2y
  and d2z = q2z -. p2z
  and rx = p1x -. p2x
  and ry = p1y -. p2y
  and rz = p1z -. p2z in
  let a = (d1x *. d1x) +. (d1y *. d1y) +. (d1z *. d1z)
  and e = (d2x *. d2x) +. (d2y *. d2y) +. (d2z *. d2z) in
  if a <= 1e-30 && e <= 1e-30 then segment_parameters_squared rx ry rz d1x d1y d1z d2x d2y d2z 0. 0.
  else if a <= 1e-30 then
    let t = clamp01 (((d2x *. rx) +. (d2y *. ry) +. (d2z *. rz)) /. e) in
    segment_parameters_squared rx ry rz d1x d1y d1z d2x d2y d2z 0. t
  else
    let c = (d1x *. rx) +. (d1y *. ry) +. (d1z *. rz) in
    if e <= 1e-30 then
      segment_parameters_squared rx ry rz d1x d1y d1z d2x d2y d2z (clamp01 (-.c /. a)) 0.
    else
      let b = (d1x *. d2x) +. (d1y *. d2y) +. (d1z *. d2z)
      and f = (d2x *. rx) +. (d2y *. ry) +. (d2z *. rz) in
      let denominator = (a *. e) -. (b *. b) in
      let s = if denominator <> 0. then clamp01 (((b *. f) -. (c *. e)) /. denominator) else 0. in
      let t = ((b *. s) +. f) /. e in
      if t < 0. then
        segment_parameters_squared rx ry rz d1x d1y d1z d2x d2y d2z (clamp01 (-.c /. a)) 0.
      else if t > 1. then
        segment_parameters_squared rx ry rz d1x d1y d1z d2x d2y d2z (clamp01 ((b -. c) /. a)) 1.
      else segment_parameters_squared rx ry rz d1x d1y d1z d2x d2y d2z s t

let[@inline] segment_intersects_triangle px py pz qx qy qz ax ay az bx by bz cx cy cz =
  let dx = qx -. px
  and dy = qy -. py
  and dz = qz -. pz
  and e1x = bx -. ax
  and e1y = by -. ay
  and e1z = bz -. az
  and e2x = cx -. ax
  and e2y = cy -. ay
  and e2z = cz -. az in
  let hx = (dy *. e2z) -. (dz *. e2y)
  and hy = (dz *. e2x) -. (dx *. e2z)
  and hz = (dx *. e2y) -. (dy *. e2x) in
  let determinant = (e1x *. hx) +. (e1y *. hy) +. (e1z *. hz) in
  if determinant = 0. then false
  else
    let inverse = 1. /. determinant in
    let sx = px -. ax and sy = py -. ay and sz = pz -. az in
    let u = inverse *. ((sx *. hx) +. (sy *. hy) +. (sz *. hz)) in
    if u < 0. || u > 1. then false
    else
      let vx = (sy *. e1z) -. (sz *. e1y)
      and vy = (sz *. e1x) -. (sx *. e1z)
      and vz = (sx *. e1y) -. (sy *. e1x) in
      let v = inverse *. ((dx *. vx) +. (dy *. vy) +. (dz *. vz)) in
      if v < 0. || u +. v > 1. then false
      else
        let along = inverse *. ((e2x *. vx) +. (e2y *. vy) +. (e2z *. vz)) in
        along >= 0. && along <= 1.

let[@inline] segment_triangle_squared px py pz qx qy qz ax ay az bx by bz cx cy cz =
  if segment_intersects_triangle px py pz qx qy qz ax ay az bx by bz cx cy cz then 0.
  else
    Float.min
      (Float.min
         (point_triangle_squared px py pz ax ay az bx by bz cx cy cz)
         (point_triangle_squared qx qy qz ax ay az bx by bz cx cy cz))
      (Float.min
         (segment_segment_squared px py pz qx qy qz ax ay az bx by bz)
         (Float.min
            (segment_segment_squared px py pz qx qy qz bx by bz cx cy cz)
            (segment_segment_squared px py pz qx qy qz cx cy cz ax ay az)))

let[@inline always] feature_distance_squared left li right ri =
  let lpa = left.a.(li) and lpb = left.b.(li) and rpa = right.a.(ri) and rpb = right.b.(ri) in
  let lax = left.positions.x.(lpa)
  and lay = left.positions.y.(lpa)
  and laz = left.positions.z.(lpa)
  and lbx = left.positions.x.(lpb)
  and lby = left.positions.y.(lpb)
  and lbz = left.positions.z.(lpb)
  and rax = right.positions.x.(rpa)
  and ray = right.positions.y.(rpa)
  and raz = right.positions.z.(rpa)
  and rbx = right.positions.x.(rpb)
  and rby = right.positions.y.(rpb)
  and rbz = right.positions.z.(rpb) in
  let lpc = left.c.(li) and rpc = right.c.(ri) in
  let lcx = if lpc >= 0 then left.positions.x.(lpc) else lbx
  and lcy = if lpc >= 0 then left.positions.y.(lpc) else lby
  and lcz = if lpc >= 0 then left.positions.z.(lpc) else lbz
  and rcx = if rpc >= 0 then right.positions.x.(rpc) else rbx
  and rcy = if rpc >= 0 then right.positions.y.(rpc) else rby
  and rcz = if rpc >= 0 then right.positions.z.(rpc) else rbz in
  match (Bytes.unsafe_get left.kinds li, Bytes.unsafe_get right.kinds ri) with
  | left_kind, right_kind when left_kind = segment && right_kind = segment ->
      segment_segment_squared lax lay laz lbx lby lbz rax ray raz rbx rby rbz
  | left_kind, _ when left_kind = segment ->
      segment_triangle_squared lax lay laz lbx lby lbz rax ray raz rbx rby rbz rcx rcy rcz
  | _, right_kind when right_kind = segment ->
      segment_triangle_squared rax ray raz rbx rby rbz lax lay laz lbx lby lbz lcx lcy lcz
  | _ ->
      let first =
        segment_triangle_squared lax lay laz lbx lby lbz rax ray raz rbx rby rbz rcx rcy rcz
      in
      if first = 0. then 0.
      else
        let second =
          segment_triangle_squared lbx lby lbz lcx lcy lcz rax ray raz rbx rby rbz rcx rcy rcz
        in
        if second = 0. then 0.
        else
          let third =
            segment_triangle_squared lcx lcy lcz lax lay laz rax ray raz rbx rby rbz rcx rcy rcz
          in
          if third = 0. then 0.
          else
            let fourth =
              segment_triangle_squared rax ray raz rbx rby rbz lax lay laz lbx lby lbz lcx lcy lcz
            in
            if fourth = 0. then 0.
            else
              let fifth =
                segment_triangle_squared rbx rby rbz rcx rcy rcz lax lay laz lbx lby lbz lcx lcy lcz
              in
              if fifth = 0. then 0.
              else
                let sixth =
                  segment_triangle_squared rcx rcy rcz rax ray raz lax lay laz lbx lby lbz lcx lcy
                    lcz
                in
                Float.min first
                  (Float.min second (Float.min third (Float.min fourth (Float.min fifth sixth))))

let[@inline] feature_aabb_into features feature bounds =
  let pa = features.a.(feature) and pb = features.b.(feature) in
  let ax = features.positions.x.(pa)
  and ay = features.positions.y.(pa)
  and az = features.positions.z.(pa)
  and bx = features.positions.x.(pb)
  and by = features.positions.y.(pb)
  and bz = features.positions.z.(pb) in
  let pc = features.c.(feature) in
  let cx = if pc >= 0 then features.positions.x.(pc) else bx
  and cy = if pc >= 0 then features.positions.y.(pc) else by
  and cz = if pc >= 0 then features.positions.z.(pc) else bz in
  bounds.(0) <- Float.min ax (Float.min bx cx);
  bounds.(1) <- Float.min ay (Float.min by cy);
  bounds.(2) <- Float.min az (Float.min bz cz);
  bounds.(3) <- Float.max ax (Float.max bx cx);
  bounds.(4) <- Float.max ay (Float.max by cy);
  bounds.(5) <- Float.max az (Float.max bz cz)

let[@inline] aabb_distance_squared index node qmin_x qmin_y qmin_z qmax_x qmax_y qmax_z =
  let dx =
    if qmax_x < index.min_x.(node) then index.min_x.(node) -. qmax_x
    else if qmin_x > index.max_x.(node) then qmin_x -. index.max_x.(node)
    else 0.
  and dy =
    if qmax_y < index.min_y.(node) then index.min_y.(node) -. qmax_y
    else if qmin_y > index.max_y.(node) then qmin_y -. index.max_y.(node)
    else 0.
  and dz =
    if qmax_z < index.min_z.(node) then index.min_z.(node) -. qmax_z
    else if qmin_z > index.max_z.(node) then qmin_z -. index.max_z.(node)
    else 0.
  in
  (dx *. dx) +. (dy *. dy) +. (dz *. dz)

let query_feature index queries query best_distance best_ids stack bounds =
  if Array.length index.order > 0 then begin
    feature_aabb_into queries query bounds;
    let qmin_x = bounds.(0)
    and qmin_y = bounds.(1)
    and qmin_z = bounds.(2)
    and qmax_x = bounds.(3)
    and qmax_y = bounds.(4)
    and qmax_z = bounds.(5) in
    let stack_size = ref 1 in
    stack.(0) <- 0;
    while !stack_size > 0 do
      decr stack_size;
      let node = stack.(!stack_size) in
      if
        aabb_distance_squared index node qmin_x qmin_y qmin_z qmax_x qmax_y qmax_z
        <= best_distance.(0)
      then
        if index.count.(node) > 0 then
          for at = index.first.(node) to index.first.(node) + index.count.(node) - 1 do
            let feature = index.order.(at) in
            let entity = index.features.entities.(feature) in
            let distance = feature_distance_squared index.features feature queries query in
            if not (Float.is_finite distance) then
              raise (Error.Fail "feature distance overflowed for finite input coordinates");
            if
              distance < best_distance.(0)
              || distance = best_distance.(0)
                 && (best_ids.(0) < 0
                    || entity < best_ids.(0)
                    || (entity = best_ids.(0) && feature < best_ids.(1)))
            then begin
              best_distance.(0) <- distance;
              best_ids.(0) <- entity;
              best_ids.(1) <- feature
            end
          done
        else begin
          let left = index.left.(node) and right = index.right.(node) in
          let ld = aabb_distance_squared index left qmin_x qmin_y qmin_z qmax_x qmax_y qmax_z
          and rd = aabb_distance_squared index right qmin_x qmin_y qmin_z qmax_x qmax_y qmax_z in
          if ld <= best_distance.(0) || rd <= best_distance.(0) then begin
            if ld < rd || (ld = rd && left < right) then begin
              if rd <= best_distance.(0) then begin
                stack.(!stack_size) <- right;
                incr stack_size
              end;
              if ld <= best_distance.(0) then begin
                stack.(!stack_size) <- left;
                incr stack_size
              end
            end
            else begin
              if ld <= best_distance.(0) then begin
                stack.(!stack_size) <- left;
                incr stack_size
              end;
              if rd <= best_distance.(0) then begin
                stack.(!stack_size) <- right;
                incr stack_size
              end
            end
          end
        end
    done
  end

let nearest_entities_with_distances ?cancel ?(grain = 16_384) ~max_distance source queries =
  if grain <= 0 then Error "Proximity query: grain must be positive"
  else if (not (Float.is_finite max_distance)) || max_distance < 0. || max_distance > sqrt max_float
  then
    Error "Proximity query: distance threshold must be finite, non-negative, and safely squarable"
  else
    try
      let maximum = max_distance *. max_distance in
      let output = Array.make queries.entity_count (-1)
      and distances = Array.make queries.entity_count Float.infinity in
      Support.run_indexed_ranges ~grain queries.entity_count
        (fun ~range ~first:first_entity ~last:last_entity ->
          let best_distance = [| maximum |]
          and best_ids = [| -1; -1 |]
          and stack = Array.make 128 0
          and bounds = Array.make 6 0. in
          for entity = first_entity to last_entity - 1 do
            Cancel.poll cancel entity 4095;
            best_distance.(0) <- maximum;
            best_ids.(0) <- -1;
            best_ids.(1) <- -1;
            for feature = queries.offsets.(entity) to queries.offsets.(entity + 1) - 1 do
              query_feature source queries feature best_distance best_ids stack bounds
            done;
            if best_ids.(0) >= 0 then begin
              output.(entity) <- best_ids.(0);
              distances.(entity) <- best_distance.(0)
            end
          done);
      Ok (output, distances)
    with
    | Error.Fail message -> Error message
    | Invalid_argument message -> Error message
