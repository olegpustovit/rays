
type t = {
  initial_point_count : int;
  points : Implicit_point.t array;
  x : float array;
  y : float array;
  triangles : int array;
  constraints : int array;

  point_roots : int array;
  provenance_first : int array;
  provenance_second : int array;
  provenance_third : int array;
  provenance_weight_first : float array;
  provenance_weight_second : float array;
  provenance_weight_third : float array;
}

let point_count value = Array.length value.points
let new_point_count value = point_count value - value.initial_point_count
let triangle_points value = Array.copy value.triangles
let constraint_points value = Array.copy value.constraints

let check_new value point =
  if point < value.initial_point_count || point >= point_count value then
    invalid_arg "Planar_refinement: generated point is out of range";
  point - value.initial_point_count

let point_provenance_ref value point = value.point_roots.(check_new value point)
let provenance_node_count value = Array.length value.provenance_first

let check_provenance value node =
  if node < 0 || node >= provenance_node_count value then
    invalid_arg "Planar_refinement: provenance node is out of range"

let provenance_parent_first value node =
  check_provenance value node; value.provenance_first.(node)
let provenance_parent_second value node =
  check_provenance value node; value.provenance_second.(node)
let provenance_parent_third value node =
  check_provenance value node; value.provenance_third.(node)
let provenance_parent_weights value node =
  check_provenance value node;
  value.provenance_weight_first.(node),value.provenance_weight_second.(node),
  value.provenance_weight_third.(node)

module View = struct

  let approximate_x value = value.x
  let approximate_y value = value.y
end

let finite_positive label = function
  | None -> ()
  | Some value when Float.is_finite value && value > 0. -> ()
  | Some _ -> invalid_arg ("Planar refinement " ^ label ^ " must be finite and positive")

let stable_length ax ay bx by =
  let dx = bx -. ax and dy = by -. ay in
  let scale = max (abs_float dx) (abs_float dy) in
  if scale = 0. then 0.
  else if not (Float.is_finite scale) then infinity
  else let dx = dx /. scale and dy = dy /. scale in
    scale *. sqrt ((dx *. dx) +. (dy *. dy))

let stable_angle x y opposite first second =
  let ux = x.(first) -. x.(opposite)
  and uy = y.(first) -. y.(opposite)
  and vx = x.(second) -. x.(opposite)
  and vy = y.(second) -. y.(opposite) in
  let scale = max (abs_float ux)
      (max (abs_float uy) (max (abs_float vx) (abs_float vy))) in
  if scale = 0. || not (Float.is_finite scale) then 0.
  else let ux = ux /. scale and uy = uy /. scale
    and vx = vx /. scale and vy = vy /. scale in
    atan2 (abs_float ((ux *. vy) -. (uy *. vx)))
      ((ux *. vx) +. (uy *. vy))

let triangle_requested x y a b c ~minimum_angle ~maximum_area
    ~target_edge_length ~minimum_edge_length =
  let ab = stable_length x.(a) y.(a) x.(b) y.(b)
  and bc = stable_length x.(b) y.(b) x.(c) y.(c)
  and ca = stable_length x.(c) y.(c) x.(a) y.(a) in
  let longest = max ab (max bc ca) in
  let scale = max (abs_float (x.(b) -. x.(a)))
      (max (abs_float (y.(b) -. y.(a)))
        (max (abs_float (x.(c) -. x.(a)))
          (abs_float (y.(c) -. y.(a))))) in
  let twice_area = if scale = 0. then 0. else
      let abx = (x.(b) -. x.(a)) /. scale
      and aby = (y.(b) -. y.(a)) /. scale
      and acx = (x.(c) -. x.(a)) /. scale
      and acy = (y.(c) -. y.(a)) /. scale in
      abs_float ((abx *. acy) -. (aby *. acx)) *. scale *. scale in
  let smallest_angle = min (stable_angle x y a b c)
      (min (stable_angle x y b c a) (stable_angle x y c a b)) in
  let area = 0.5 *. twice_area in
  let requested =
    (match minimum_angle with Some limit -> smallest_angle < limit | None -> false)
    || (match maximum_area with Some limit -> area > limit | None -> false)
    || (match target_edge_length with
        | Some limit -> longest > limit | None -> false) in
  requested && longest >= minimum_edge_length

let inside_triangle points a b c point =
  let orient u v = Implicit_point.orient2d_xy points.(u) points.(v) point in
  let nonnegative = function Predicates.Negative -> false | _ -> true in
  nonnegative (orient a b) && nonnegative (orient b c) && nonnegative (orient c a)

let strict_segment_contains first second point =
  if Implicit_point.equal first point || Implicit_point.equal second point then false
  else if Implicit_point.orient2d_xy first second point <> Predicates.Zero then false
  else
    let compare = if Implicit_point.compare_x first second <> 0
      then Implicit_point.compare_x else Implicit_point.compare_y in
    let lower,upper = if compare first second < 0 then first,second else second,first in
    compare lower point < 0 && compare point upper < 0

let barycentric x y a b c px py =
  let scale = max (abs_float (x.(b) -. x.(a)))
      (max (abs_float (y.(b) -. y.(a)))
        (max (abs_float (x.(c) -. x.(a)))
          (max (abs_float (y.(c) -. y.(a)))
            (max (abs_float (px -. x.(a))) (abs_float (py -. y.(a))))))) in
  if scale = 0. || not (Float.is_finite scale) then 1. /. 3.,1. /. 3.,1. /. 3.
  else begin
    let ax = x.(a) /. scale and ay = y.(a) /. scale
    and bx = x.(b) /. scale and by = y.(b) /. scale
    and cx = x.(c) /. scale and cy = y.(c) /. scale
    and px = px /. scale and py = py /. scale in
    let denominator = ((bx -. ax) *. (cy -. ay))
        -. ((by -. ay) *. (cx -. ax)) in
    if denominator = 0. || not (Float.is_finite denominator) then
      1. /. 3.,1. /. 3.,1. /. 3.
    else
      let wb = (((px -. ax) *. (cy -. ay))
          -. ((py -. ay) *. (cx -. ax))) /. denominator in
      let wc = (((bx -. ax) *. (py -. ay))
          -. ((by -. ay) *. (px -. ax))) /. denominator in
      let wa = 1. -. wb -. wc in
      if Float.is_finite wa && Float.is_finite wb && Float.is_finite wc then
        wa,wb,wc else 1. /. 3.,1. /. 3.,1. /. 3.
  end

let rounded_centroid points x y a b c =
  let coordinate values =
    let first = values.(a) and second = values.(b) and third = values.(c) in
    let scale = max (abs_float first) (max (abs_float second) (abs_float third)) in
    if scale = 0. then 0.
    else scale *. (((first /. scale) /. 3.) +. ((second /. scale) /. 3.)
      +. ((third /. scale) /. 3.)) in
  Implicit_point.rounded ~reference:points.(a)
    ~x:(coordinate x) ~y:(coordinate y) ~z:0. |> Result.get_ok

let rounded_circumcenter ~fallback points x y a b c =
  let ax = x.(a) and ay = y.(a) in
  let bax = x.(b) -. ax and bay = y.(b) -. ay
  and cax = x.(c) -. ax and cay = y.(c) -. ay in
  let scale = max (abs_float bax)
      (max (abs_float bay) (max (abs_float cax) (abs_float cay))) in
  if scale = 0. || not (Float.is_finite scale) then fallback
  else begin
    let bx = bax /. scale and by = bay /. scale
    and cx = cax /. scale and cy = cay /. scale in
    let denominator = 2. *. ((bx *. cy) -. (by *. cx)) in
    if denominator = 0. || not (Float.is_finite denominator) then fallback
    else begin
      let blift = (bx *. bx) +. (by *. by)
      and clift = (cx *. cx) +. (cy *. cy) in
      let ux = ((blift *. cy) -. (clift *. by)) /. denominator
      and uy = ((clift *. bx) -. (blift *. cx)) /. denominator in
      let px = ax +. (scale *. ux) and py = ay +. (scale *. uy) in
      if Float.is_finite px && Float.is_finite py then
        match Implicit_point.rounded ~reference:points.(a) ~x:px ~y:py ~z:0. with
        | Ok point -> point | Error _ -> fallback
      else fallback
    end
  end

let canonical_bits value =
  if value = 0. then 0L else Int64.bits_of_float value

module Coordinate_buckets = struct
  type t = {mutable keys:int array;mutable heads:int array;
    mutable next:int array;mutable count:int}

  let capacity expected =
    if expected < 0 || expected > Sys.max_array_length / 2 then
      invalid_arg "Planar refinement coordinate table exceeds array limits";
    let capacity = ref 16 in
    while !capacity < max 16 (expected * 2) do
      if !capacity > Sys.max_array_length / 2 then
        invalid_arg "Planar refinement coordinate table exceeds array limits";
      capacity := !capacity * 2
    done;
    !capacity

  let create expected =
    let size = capacity expected in
    {keys=Array.make size (-1);heads=Array.make size (-1);
     next=Array.make (max 1 expected) (-1);count=0}

  let key x y =
    (* Collisions are checked against canonical coordinates and symbolic points. *)
    let mix value =
      let value=(value lxor (value lsr 30))*0x3f4a7c159e3779b in
      let value=(value lxor (value lsr 27))*0x2f1bbcdc676dfd1 in
      value lxor (value lsr 31) in
    (mix (Int64.to_int x) lxor (mix (Int64.to_int y) lsl 1)) land max_int

  let slot keys key =
    let mask=Array.length keys - 1 and result=ref(key land (Array.length keys - 1))in
    while keys.(!result) <> -1 && keys.(!result) <> key do
      result:=(!result+1)land mask
    done;
    !result

  let grow table =
    if Array.length table.keys > Sys.max_array_length / 2 then
      invalid_arg "Planar refinement coordinate table exceeds array limits";
    let keys=Array.make (Array.length table.keys * 2) (-1)
    and heads=Array.make (Array.length table.keys * 2) (-1) in
    Array.iteri(fun index key->if key<> -1 then(
      let target=slot keys key in keys.(target)<-key;
      heads.(target)<-table.heads.(index)))table.keys;
    table.keys<-keys;table.heads<-heads

  let ensure_next table index =
    if index >= Array.length table.next then begin
      let size=ref(Array.length table.next)in
      while !size <= index do
        if !size > Sys.max_array_length / 2 then
          invalid_arg "Planar refinement coordinate chain exceeds array limits";
        size:= !size*2
      done;
      let next=Array.make !size (-1)in
      Array.blit table.next 0 next 0 (Array.length table.next);
      table.next<-next
    end

  let find table key =
    let index=slot table.keys key in
    if table.keys.(index)=key then table.heads.(index) else -1

  let next table index=table.next.(index)

  let add table key value =
    if table.count >= Array.length table.keys / 2 then grow table;
    ensure_next table value;
    let index=slot table.keys key in
    if table.keys.(index)= -1 then(
      table.keys.(index)<-key;table.count<-table.count+1);
    table.next.(value)<-table.heads.(index);
    table.heads.(index)<-value
end

let[@inline always] downward value =
  Float.next_after value Float.neg_infinity

let[@inline always] upward value =
  Float.next_after value Float.infinity

let half_sum_lower first second =
  downward ((first *. 0.5) +. (second *. 0.5))

let half_sum_upper first second =
  upward ((first *. 0.5) +. (second *. 0.5))

let difference_upper first second =
  let value = first -. second in
  if Float.is_finite value then upward (abs_float value) else infinity

let radius_upper first second =
  let (first_x_lower,first_x_upper),(first_y_lower,first_y_upper),_ =
    Implicit_point.bounds first
  and (second_x_lower,second_x_upper),(second_y_lower,second_y_upper),_ =
    Implicit_point.bounds second in
  let dx = max (difference_upper first_x_lower second_x_upper)
      (difference_upper first_x_upper second_x_lower)
  and dy = max (difference_upper first_y_lower second_y_upper)
      (difference_upper first_y_upper second_y_lower) in
  if not (Float.is_finite dx && Float.is_finite dy) then infinity
  else upward (upward (dx *. 0.5) +. upward (dy *. 0.5))

let lower_radius center radius =
  if radius = infinity then neg_infinity else downward (center -. radius)

let upper_radius center radius =
  if radius = infinity then infinity else upward (center +. radius)

let constraint_index ?cancel points constraints =
  let count = Array.length constraints / 2 in
  let min_x = Array.make count 0. and min_y = Array.make count 0.
  and max_x = Array.make count 0. and max_y = Array.make count 0. in
  for segment = 0 to count - 1 do
    if segment land 16_383 = 0 then Cancel.check_opt cancel;
    let first = points.(constraints.(segment * 2))
    and second = points.(constraints.((segment * 2) + 1)) in
    let (first_x_lower,first_x_upper),(first_y_lower,first_y_upper),_ =
      Implicit_point.bounds first
    and (second_x_lower,second_x_upper),(second_y_lower,second_y_upper),_ =
      Implicit_point.bounds second in
    let center_x_lower = half_sum_lower first_x_lower second_x_lower
    and center_x_upper = half_sum_upper first_x_upper second_x_upper
    and center_y_lower = half_sum_lower first_y_lower second_y_lower
    and center_y_upper = half_sum_upper first_y_upper second_y_upper in
    let radius = radius_upper first second in
    min_x.(segment) <- lower_radius center_x_lower radius;
    min_y.(segment) <- lower_radius center_y_lower radius;
    max_x.(segment) <- upper_radius center_x_upper radius;
    max_y.(segment) <- upper_radius center_y_upper radius
  done;
  Bounds2_index.create ?cancel ~min_x ~min_y ~max_x ~max_y ()

type provenance_builder = {
  mutable first : int array;
  mutable second : int array;
  mutable third : int array;
  mutable weight_first : float array;
  mutable weight_second : float array;
  mutable weight_third : float array;
  mutable length : int;
}

let provenance_builder capacity = {
  first = Array.make (max 16 capacity) 0;
  second = Array.make (max 16 capacity) 0;
  third = Array.make (max 16 capacity) 0;
  weight_first = Array.make (max 16 capacity) 0.;
  weight_second = Array.make (max 16 capacity) 0.;
  weight_third = Array.make (max 16 capacity) 0.;
  length = 0;
}

let grow_provenance value =
  if value.length = Array.length value.first then begin
    if value.length > Sys.max_array_length / 2 then
      invalid_arg "Planar refinement provenance exceeds array limits";
    let capacity = value.length * 2 in
    let grow source default =
      let output = Array.make capacity default in
      Array.blit source 0 output 0 value.length; output in
    value.first <- grow value.first 0;
    value.second <- grow value.second 0;
    value.third <- grow value.third 0;
    value.weight_first <- grow value.weight_first 0.;
    value.weight_second <- grow value.weight_second 0.;
    value.weight_third <- grow value.weight_third 0.
  end

let append_provenance value ~first ~second ~third ~wa ~wb ~wc =
  grow_provenance value;
  let node = value.length in
  value.first.(node) <- first; value.second.(node) <- second;
  value.third.(node) <- third; value.weight_first.(node) <- wa;
  value.weight_second.(node) <- wb; value.weight_third.(node) <- wc;
  value.length <- node + 1;
  node

type mesh_index = {
  neighbor_offsets : int array;
  neighbors : int array;
  incident_offsets : int array;
  incidents : int array;
  fixed : bytes;
}

let mesh_index ?cancel ~point_count ~triangles ~constraints () =
  let triangle_count = Array.length triangles / 3 in
  if triangle_count > Sys.max_array_length / 3 then
    invalid_arg "Planar refinement edge cardinality exceeds array limits";
  let edge_count = triangle_count * 3 in
  let edge_a = Array.make edge_count 0 and edge_b = Array.make edge_count 0 in
  let write edge first second =
    if first < second then begin edge_a.(edge) <- first; edge_b.(edge) <- second end
    else begin edge_a.(edge) <- second; edge_b.(edge) <- first end in
  for triangle = 0 to triangle_count - 1 do
    if triangle land 4095 = 0 then Cancel.check_opt cancel;
    let a = triangles.(triangle * 3) and b = triangles.((triangle * 3) + 1)
    and c = triangles.((triangle * 3) + 2) in
    write (triangle * 3) a b; write ((triangle * 3) + 1) b c;
    write ((triangle * 3) + 2) c a
  done;
  let order = Array.init edge_count Fun.id in
  Array.sort (fun left right ->
      let compared = Int.compare edge_a.(left) edge_a.(right) in
      if compared <> 0 then compared else Int.compare edge_b.(left) edge_b.(right))
    order;
  let unique = ref 0 in
  for index = 0 to edge_count - 1 do
    let edge = order.(index) in
    if index = 0 || edge_a.(edge) <> edge_a.(order.(index - 1))
        || edge_b.(edge) <> edge_b.(order.(index - 1)) then incr unique
  done;
  let unique_a = Array.make !unique 0 and unique_b = Array.make !unique 0
  and incidence = Array.make !unique 0 in
  let at = ref (-1) in
  for index = 0 to edge_count - 1 do
    let edge = order.(index) in
    if index = 0 || edge_a.(edge) <> edge_a.(order.(index - 1))
        || edge_b.(edge) <> edge_b.(order.(index - 1)) then begin
      incr at; unique_a.(!at) <- edge_a.(edge); unique_b.(!at) <- edge_b.(edge)
    end;
    incidence.(!at) <- incidence.(!at) + 1
  done;
  if !unique > Sys.max_array_length / 2 then
    invalid_arg "Planar refinement neighbor cardinality exceeds array limits";
  let neighbor_counts = Array.make point_count 0 and fixed = Bytes.make point_count '\000' in
  for edge = 0 to !unique - 1 do
    let a = unique_a.(edge) and b = unique_b.(edge) in
    neighbor_counts.(a) <- neighbor_counts.(a) + 1;
    neighbor_counts.(b) <- neighbor_counts.(b) + 1;
    if incidence.(edge) = 1 then begin
      Bytes.unsafe_set fixed a '\001'; Bytes.unsafe_set fixed b '\001'
    end else if incidence.(edge) <> 2 then
      invalid_arg "Planar refinement topology contains a non-manifold edge"
  done;
  Array.iter (fun point ->
    if point < 0 || point >= point_count then
      invalid_arg "Planar refinement constraint endpoint is out of range";
    Bytes.unsafe_set fixed point '\001') constraints;
  let neighbor_offsets = Array.make (point_count + 1) 0 in
  for point = 0 to point_count - 1 do
    neighbor_offsets.(point + 1) <- neighbor_offsets.(point) + neighbor_counts.(point)
  done;
  let neighbors = Array.make neighbor_offsets.(point_count) 0
  and next = Array.copy neighbor_offsets in
  for edge = 0 to !unique - 1 do
    let a = unique_a.(edge) and b = unique_b.(edge) in
    neighbors.(next.(a)) <- b; next.(a) <- next.(a) + 1;
    neighbors.(next.(b)) <- a; next.(b) <- next.(b) + 1
  done;
  let incident_counts = Array.make point_count 0 in
  Array.iter (fun point -> incident_counts.(point) <- incident_counts.(point) + 1)
    triangles;
  let incident_offsets = Array.make (point_count + 1) 0 in
  for point = 0 to point_count - 1 do
    incident_offsets.(point + 1) <- incident_offsets.(point) + incident_counts.(point)
  done;
  let incidents = Array.make incident_offsets.(point_count) 0
  and next = Array.copy incident_offsets in
  for triangle = 0 to triangle_count - 1 do
    for local = 0 to 2 do
      let point = triangles.((triangle * 3) + local) in
      incidents.(next.(point)) <- triangle; next.(point) <- next.(point) + 1
    done
  done;
  { neighbor_offsets; neighbors; incident_offsets; incidents; fixed }

let stable_average values offsets members point =
  let first = offsets.(point) and last = offsets.(point + 1) in
  let scale = ref 0. in
  for at = first to last - 1 do scale := max !scale (abs_float values.(members.(at))) done;
  if !scale = 0. then 0.
  else begin
    let sum = ref 0. and compensation = ref 0. in
    for at = first to last - 1 do
      let value = (values.(members.(at)) /. !scale) -. !compensation in
      let next = !sum +. value in
      compensation := (next -. !sum) -. value; sum := next
    done;
    let normalized = !sum /. float_of_int (last - first) in
    !scale *. max (-1.) (min 1. normalized)
  end

let stable_blend first second alpha =
  let scale = max (abs_float first) (abs_float second) in
  if scale = 0. then 0.
  else scale *. (((first /. scale) *. (1. -. alpha))
    +. ((second /. scale) *. alpha))

let build ?cancel ~grain ~initial_points ~initial_triangle_points
    ~constraint_points ~constraint_winding ~minimum_angle ~maximum_area
    ~target_edge_length ~minimum_edge_length ~maximum_new_points
    ~allow_constraint_splitting ~regularization_steps
    ~allow_movement_of_interior_input_points () =
  try
    if grain <= 0 then invalid_arg "Planar refinement grain must be positive";
    if minimum_edge_length < 0. || not (Float.is_finite minimum_edge_length) then
      invalid_arg "Planar refinement minimum edge length must be finite and non-negative";
    if maximum_new_points < 0 then
      invalid_arg "Planar refinement maximum new points must be non-negative";
    if regularization_steps < 0 then
      invalid_arg "Planar refinement regularization steps must be non-negative";
    finite_positive "minimum angle" minimum_angle;
    finite_positive "maximum area" maximum_area;
    finite_positive "target edge length" target_edge_length;
    Option.iter (fun angle -> if angle >= Float.pi /. 3. then invalid_arg
        "Planar refinement minimum angle must be less than pi/3") minimum_angle;
    if Array.length constraint_points land 1 <> 0 then
      invalid_arg "Planar refinement constraint endpoint array is malformed";
    if Array.length constraint_winding <> 0
        && Array.length constraint_winding <> Array.length constraint_points / 2 then
      invalid_arg "Planar refinement constraint winding array is malformed";
    if Array.length initial_points > Sys.max_array_length - maximum_new_points then
      invalid_arg "Planar refinement point limit exceeds array limits";
    let capacity = Array.length initial_points + maximum_new_points in
    let dummy = initial_points.(0) in
    let points = Array.make capacity dummy and x = Array.make capacity 0.
    and y = Array.make capacity 0. in
    Array.blit initial_points 0 points 0 (Array.length initial_points);
    Array.iteri (fun point value ->
      let px,py,_ = Implicit_point.approximate value in x.(point) <- px; y.(point) <- py)
      initial_points;
    let point_roots = Array.make maximum_new_points 0
    and provenance = provenance_builder maximum_new_points in
    let point_count = ref (Array.length initial_points)
    and constraints = ref (Array.copy constraint_points)
    and winding = ref (if Array.length constraint_winding = 0 then
        Array.make (Array.length constraint_points / 2) 0
      else Array.copy constraint_winding)
    and triangles = ref (Array.copy initial_triangle_points) in
    let cdt_workspace = Planar_cdt.View.create_workspace
        ~point_capacity:capacity
        ~triangle_capacity:(max 1 (Array.length initial_triangle_points / 3)) () in
    let buckets = Coordinate_buckets.create (Array.length initial_points) in
    let add_bucket point =
      let key = Coordinate_buckets.key (canonical_bits x.(point))
        (canonical_bits y.(point)) in
      Coordinate_buckets.add buckets key point in
    for point = 0 to !point_count - 1 do add_bucket point done;
    let exists candidate =
      let px,py,_ = Implicit_point.approximate candidate in
      let bx=canonical_bits px and by=canonical_bits py in
      let point=ref(Coordinate_buckets.find buckets
        (Coordinate_buckets.key bx by))and found=ref false in
      while !point>=0&&not !found do
        found:=canonical_bits x.(!point)=bx&&canonical_bits y.(!point)=by&&
          Implicit_point.equal points.(!point) candidate;
        point:=Coordinate_buckets.next buckets !point
      done;
      !found in
    let rec refine () =
      Cancel.check_opt cancel;
      let triangle_count = Array.length !triangles / 3 in
      let bad = Bytes.make triangle_count '\000' in
      let classify triangle =
        if triangle land 4095 = 0 then Cancel.check_opt cancel;
        let a = (!triangles).(triangle * 3)
        and b = (!triangles).((triangle * 3) + 1)
        and c = (!triangles).((triangle * 3) + 2) in
        if triangle_requested x y a b c ~minimum_angle ~maximum_area
            ~target_edge_length ~minimum_edge_length then
          Bytes.unsafe_set bad triangle '\001' in
      if triangle_count > grain then Parallel.for_ ~chunk_size:grain ~start:0
          ~finish:(triangle_count - 1) classify
      else for triangle = 0 to triangle_count - 1 do classify triangle done;
      let bad_count = ref 0 in
      for triangle = 0 to triangle_count - 1 do
        if Bytes.unsafe_get bad triangle <> '\000' then incr bad_count
      done;
      if !bad_count > 0
          && !point_count - Array.length initial_points < maximum_new_points
      then begin
        let candidate_points = Array.make !bad_count dummy
        and candidate_a = Array.make !bad_count 0
        and candidate_b = Array.make !bad_count 0
        and candidate_c = Array.make !bad_count 0
        and candidate_split = Array.make !bad_count (-1) in
        let at = ref 0 in
        for triangle = 0 to triangle_count - 1 do
          if Bytes.unsafe_get bad triangle <> '\000' then begin
            let a = (!triangles).(triangle * 3)
            and b = (!triangles).((triangle * 3) + 1)
            and c = (!triangles).((triangle * 3) + 2) in
            candidate_a.(!at) <- a; candidate_b.(!at) <- b; candidate_c.(!at) <- c;
            incr at
          end
        done;
        let segment_count = Array.length !constraints / 2 in
        let constraint_tree = if segment_count = 0 then None
          else Some (constraint_index ?cancel points !constraints) in
        let construct candidate =
          if candidate land 1023 = 0 then Cancel.check_opt cancel;
          let a = candidate_a.(candidate) and b = candidate_b.(candidate)
          and c = candidate_c.(candidate) in
          let centroid = rounded_centroid points x y a b c in
          let circumcenter = rounded_circumcenter ~fallback:centroid
              points x y a b c in
          let center = if inside_triangle points a b c circumcenter
            then circumcenter else centroid in
          let encroached = ref (-1) in
          (match constraint_tree with
           | None -> ()
           | Some constraint_tree ->
            let (point_min_x,point_max_x),(point_min_y,point_max_y),_ =
              Implicit_point.bounds center in
            Bounds2_index.query ?cancel constraint_tree
              ~min_x:point_min_x ~min_y:point_min_y
              ~max_x:point_max_x ~max_y:point_max_y (fun segment ->
              let first = (!constraints).(segment * 2)
              and second = (!constraints).((segment * 2) + 1) in
              if Implicit_point.diametral_dot_xy
                  ~first:points.(first) ~second:points.(second) center
                  <> Predicates.Positive
                  && (!encroached < 0 || segment < !encroached) then
                encroached := segment));
          if !encroached >= 0 && allow_constraint_splitting then begin
            let first = (!constraints).(!encroached * 2)
            and second = (!constraints).((!encroached * 2) + 1) in
            candidate_points.(candidate) <- Result.get_ok
                (Implicit_point.midpoint points.(first) points.(second));
            candidate_a.(candidate) <- first; candidate_b.(candidate) <- second;
            candidate_c.(candidate) <- first; candidate_split.(candidate) <- !encroached
          end else
            candidate_points.(candidate) <-
              if !encroached >= 0 then centroid else center in
        if !bad_count > grain then Parallel.for_ ~chunk_size:grain ~start:0
            ~finish:(!bad_count - 1) construct
        else for candidate = 0 to !bad_count - 1 do construct candidate done;
        let remaining = maximum_new_points
            - (!point_count - Array.length initial_points) in
        let selected = Bytes.make !bad_count '\000' and selected_count = ref 0
        and candidate_buckets = Coordinate_buckets.create !bad_count in
        for candidate = 0 to !bad_count - 1 do
          if !selected_count < remaining && not (exists candidate_points.(candidate)) then begin
            let px,py,_ = Implicit_point.approximate candidate_points.(candidate) in
            let bx=canonical_bits px and by=canonical_bits py in
            let key=Coordinate_buckets.key bx by in
            let other=ref(Coordinate_buckets.find candidate_buckets key)
            and duplicate=ref false in
            while !other>=0&&not !duplicate do
                let ox,oy,_=Implicit_point.approximate candidate_points.(!other)in
                duplicate:=canonical_bits ox=bx&&canonical_bits oy=by&&
                  Implicit_point.equal candidate_points.(!other)
                    candidate_points.(candidate);
                other:=Coordinate_buckets.next candidate_buckets !other
            done;
            if not !duplicate then begin
              Bytes.unsafe_set selected candidate '\001'; incr selected_count;
              Coordinate_buckets.add candidate_buckets key candidate
            end
          end
        done;
        if !selected_count > 0 then begin
          let first_new_point = !point_count in
          for candidate = 0 to !bad_count - 1 do
            if Bytes.unsafe_get selected candidate <> '\000' then begin
              let output = !point_count in
              let generated = output - Array.length initial_points in
              let value = candidate_points.(candidate) in
              points.(output) <- value;
              let px,py,_ = Implicit_point.approximate value in
              x.(output) <- px; y.(output) <- py;
              let a = candidate_a.(candidate) and b = candidate_b.(candidate)
              and c = candidate_c.(candidate) in
              let value_ref point = if point < Array.length initial_points then point
                else point_roots.(point - Array.length initial_points) in
              let wa,wb,wc = if candidate_split.(candidate) >= 0 then
                  0.5,0.5,0.
                else barycentric x y a b c px py in
              let node = append_provenance provenance
                  ~first:(value_ref a) ~second:(value_ref b)
                  ~third:(value_ref c) ~wa ~wb ~wc in
              point_roots.(generated) <- Array.length initial_points + node;
              incr point_count; add_bucket output
            end
          done;
          let events = Array.make segment_count [] and event_count = ref 0 in
          (match constraint_tree with
           | None -> ()
           | Some constraint_tree ->
            for point = first_new_point to !point_count - 1 do
              let (point_min_x,point_max_x),(point_min_y,point_max_y),_ =
                Implicit_point.bounds points.(point) in
              Bounds2_index.query ?cancel constraint_tree
                ~min_x:point_min_x ~min_y:point_min_y
                ~max_x:point_max_x ~max_y:point_max_y (fun segment ->
                let a = (!constraints).(segment * 2)
                and b = (!constraints).((segment * 2) + 1) in
                if strict_segment_contains points.(a) points.(b) points.(point) then begin
                  events.(segment) <- point :: events.(segment); incr event_count
                end)
            done);
          if !event_count > 0 then begin
            let next_constraints = Array.make ((segment_count + !event_count) * 2) 0
            and next_winding = Array.make (segment_count + !event_count) 0 in
            let output = ref 0 in
            for segment = 0 to segment_count - 1 do
              let a = (!constraints).(segment * 2)
              and b = (!constraints).((segment * 2) + 1) in
              let compare = if Implicit_point.compare_x points.(a) points.(b) <> 0
                then (fun left right -> Implicit_point.compare_x
                    points.(left) points.(right))
                else (fun left right -> Implicit_point.compare_y
                    points.(left) points.(right)) in
              let ordered = List.sort compare events.(segment) in
              let ordered = if compare a b < 0 then ordered else List.rev ordered in
              let previous = ref a in
              let emit next =
                next_constraints.(!output * 2) <- !previous;
                next_constraints.((!output * 2) + 1) <- next;
                next_winding.(!output) <- (!winding).(segment);
                previous := next; incr output in
              List.iter emit ordered; emit b
            done;
            constraints := next_constraints; winding := next_winding
          end;
          let insert_points = Array.init (!point_count - first_new_point)
              (fun point -> first_new_point + point) in
          let orient a b c = Implicit_point.orient2d_xy
              points.(a) points.(b) points.(c)
          and incircle a b c d = Implicit_point.incircle_xy
              points.(a) points.(b) points.(c) points.(d) in
          let rebuilt = Planar_cdt.build ?cancel ~workspace:cdt_workspace
              ~point_count:!point_count
              ~orient ~incircle ~triangle_points:!triangles ~insert_points
              ~constraint_points:!constraints ~constraint_winding:!winding
              () in
          (match rebuilt with
           | Error message -> invalid_arg ("Planar refinement: " ^ message)
           | Ok rebuilt ->
               triangles := (Planar_cdt.View.view rebuilt).triangle_points;
               refine ())
        end
      end in
    let regularize () =
      let step = ref 0 and continue = ref true in
      while !continue && !step < regularization_steps do
        Cancel.check_opt cancel;
        let index = mesh_index ?cancel ~point_count:!point_count
            ~triangles:!triangles ~constraints:!constraints () in
        let eligible = Bytes.make !point_count '\000'
        and target_x = Array.sub x 0 !point_count
        and target_y = Array.sub y 0 !point_count in
        let prepare point =
          if point land 4095 = 0 then Cancel.check_opt cancel;
          let first = index.neighbor_offsets.(point)
          and last = index.neighbor_offsets.(point + 1) in
          if first < last && Bytes.unsafe_get index.fixed point = '\000'
              && (point >= Array.length initial_points
                || allow_movement_of_interior_input_points) then begin
            let tx = stable_average x index.neighbor_offsets index.neighbors point
            and ty = stable_average y index.neighbor_offsets index.neighbors point in
            if Float.is_finite tx && Float.is_finite ty then begin
              target_x.(point) <- tx; target_y.(point) <- ty;
              Bytes.unsafe_set eligible point '\001'
            end
          end in
        if !point_count > grain then Parallel.for_ ~chunk_size:grain ~start:0
            ~finish:(!point_count - 1) prepare
        else for point = 0 to !point_count - 1 do prepare point done;
        let next_points = Array.sub points 0 !point_count
        and next_x = Array.sub x 0 !point_count
        and next_y = Array.sub y 0 !point_count
        and containing = Array.make !point_count (-1)
        and moved = Bytes.make !point_count '\000' in
        let accepted = ref false and alpha = ref 1. and attempt = ref 0 in
        while not !accepted && !attempt < 30 do
          let construct point =
            if Bytes.unsafe_get eligible point <> '\000' then begin
              let px = stable_blend x.(point) target_x.(point) !alpha
              and py = stable_blend y.(point) target_y.(point) !alpha in
              let candidate = Implicit_point.rounded ~reference:points.(point)
                  ~x:px ~y:py ~z:0. |> Result.get_ok in
              next_points.(point) <- candidate; next_x.(point) <- px; next_y.(point) <- py;
              if Implicit_point.equal candidate points.(point) then begin
                Bytes.unsafe_set moved point '\000'; containing.(point) <- -2
              end else begin
                Bytes.unsafe_set moved point '\001'; containing.(point) <- -1;
                let at = ref index.incident_offsets.(point)
                and last = index.incident_offsets.(point + 1) in
                while containing.(point) < 0 && !at < last do
                  let triangle = index.incidents.(!at) in
                  let a = (!triangles).(triangle * 3)
                  and b = (!triangles).((triangle * 3) + 1)
                  and c = (!triangles).((triangle * 3) + 2) in
                  if inside_triangle points a b c candidate then
                    containing.(point) <- triangle;
                  incr at
                done
              end
            end else begin
              next_points.(point) <- points.(point); next_x.(point) <- x.(point);
              next_y.(point) <- y.(point); containing.(point) <- -2;
              Bytes.unsafe_set moved point '\000'
            end in
          if !point_count > grain then Parallel.for_ ~chunk_size:grain ~start:0
              ~finish:(!point_count - 1) construct
          else for point = 0 to !point_count - 1 do construct point done;
          let triangle_count = Array.length !triangles / 3 in
          let valid = Bytes.make triangle_count '\000' in
          let validate triangle =
            if triangle land 4095 = 0 then Cancel.check_opt cancel;
            let a = (!triangles).(triangle * 3)
            and b = (!triangles).((triangle * 3) + 1)
            and c = (!triangles).((triangle * 3) + 2) in
            if Implicit_point.orient2d_xy next_points.(a) next_points.(b)
                next_points.(c) = Predicates.Positive then
              Bytes.unsafe_set valid triangle '\001' in
          if triangle_count > grain then Parallel.for_ ~chunk_size:grain ~start:0
              ~finish:(triangle_count - 1) validate
          else for triangle = 0 to triangle_count - 1 do validate triangle done;
          let invalid = ref false and point = ref 0 in
          while not !invalid && !point < !point_count do
            if Bytes.unsafe_get moved !point <> '\000'
                && containing.(!point) < 0 then invalid := true;
            incr point
          done;
          let triangle = ref 0 in
          while not !invalid && !triangle < triangle_count do
            if Bytes.unsafe_get valid !triangle = '\000' then invalid := true;
            incr triangle
          done;
          if not !invalid then accepted := true
          else begin alpha := !alpha *. 0.5; incr attempt end
        done;
        if not !accepted then continue := false
        else begin
          let moved_count = ref 0 in
          for point = 0 to !point_count - 1 do
            if Bytes.unsafe_get moved point <> '\000' then incr moved_count
          done;
          if !moved_count = 0 then continue := false
          else begin
            let current_new_count = !point_count - Array.length initial_points in
            let next_roots = Array.sub point_roots 0 current_new_count in
            let value_ref point = if point < Array.length initial_points then point
              else point_roots.(point - Array.length initial_points) in
            for point = Array.length initial_points to !point_count - 1 do
              if Bytes.unsafe_get moved point <> '\000' then begin
                let triangle = containing.(point) in
                let a = (!triangles).(triangle * 3)
                and b = (!triangles).((triangle * 3) + 1)
                and c = (!triangles).((triangle * 3) + 2) in
                let wa,wb,wc = barycentric x y a b c
                    next_x.(point) next_y.(point) in
                let node = append_provenance provenance
                    ~first:(value_ref a) ~second:(value_ref b)
                    ~third:(value_ref c) ~wa ~wb ~wc in
                next_roots.(point - Array.length initial_points) <-
                  Array.length initial_points + node
              end
            done;
            Array.blit next_roots 0 point_roots 0 current_new_count;
            Array.blit next_points 0 points 0 !point_count;
            Array.blit next_x 0 x 0 !point_count; Array.blit next_y 0 y 0 !point_count;
            let orient a b c = Implicit_point.orient2d_xy
                points.(a) points.(b) points.(c)
            and incircle a b c d = Implicit_point.incircle_xy
                points.(a) points.(b) points.(c) points.(d) in
            let rebuilt = Planar_cdt.build ?cancel ~workspace:cdt_workspace
                ~point_count:!point_count
                ~orient ~incircle ~triangle_points:!triangles
                ~constraint_points:!constraints ~constraint_winding:!winding () in
            (match rebuilt with
             | Error message -> invalid_arg ("Planar regularization: " ^ message)
             | Ok rebuilt ->
                 triangles := (Planar_cdt.View.view rebuilt).triangle_points);
            incr step
          end
        end
      done in
    refine ();
    regularize ();
    let new_count = !point_count - Array.length initial_points in
    Ok {
      initial_point_count = Array.length initial_points;
      points = Array.sub points 0 !point_count;
      x = Array.sub x 0 !point_count; y = Array.sub y 0 !point_count;
      triangles = !triangles; constraints = !constraints;
      point_roots = Array.sub point_roots 0 new_count;
      provenance_first = Array.sub provenance.first 0 provenance.length;
      provenance_second = Array.sub provenance.second 0 provenance.length;
      provenance_third = Array.sub provenance.third 0 provenance.length;
      provenance_weight_first = Array.sub provenance.weight_first 0 provenance.length;
      provenance_weight_second = Array.sub provenance.weight_second 0 provenance.length;
      provenance_weight_third = Array.sub provenance.weight_third 0 provenance.length;
    }
  with
  | Cancel.Cancelled -> Error "Planar refinement was cancelled"
  | Invalid_argument message -> Error message
