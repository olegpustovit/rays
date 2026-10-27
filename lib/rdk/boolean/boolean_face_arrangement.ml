type side = Boolean_constraints.side = Left | Right

type t = {
  points : Implicit_point.t array;
  point_handles : int array;
  segment_first : int array;
  segment_second : int array;
}

let operation = "boolean_face_arrangement"
let error code message = Error (Error.make ~operation ~code message)

let point_count value = Array.length value.points
let segment_count value = Array.length value.segment_first
let segment_first value segment = value.segment_first.(segment)
let segment_second value segment = value.segment_second.(segment)

module Private = struct
  let point value point = value.points.(point)
  let point_handle value point = value.point_handles.(point)
end

let face_constraint_range constraints side triangle = match side with
  | Left -> Boolean_constraints.left_constraint_range constraints triangle
  | Right -> Boolean_constraints.right_constraint_range constraints triangle

let face_constraint constraints side slot = match side with
  | Left -> Boolean_constraints.left_constraint constraints slot
  | Right -> Boolean_constraints.right_constraint constraints slot

let face_coplanar_range coplanar side triangle = match side with
  | Left -> Boolean_coplanar.Private.left_pair_range coplanar triangle
  | Right -> Boolean_coplanar.Private.right_pair_range coplanar triangle

let face_coplanar_pair coplanar side slot = match side with
  | Left -> Boolean_coplanar.Private.left_pair coplanar slot
  | Right -> Boolean_coplanar.Private.right_pair coplanar slot

let face_triangle_point constraints side triangle local = match side with
  | Left -> Boolean_constraints.Private.left_triangle_point
      constraints triangle local
  | Right -> Boolean_constraints.Private.right_triangle_point
      constraints triangle local

let opponent constraints side triangle constraint_index =
  let first_side = Boolean_constraints.constraint_first_side constraints constraint_index
  and first_triangle = Boolean_constraints.constraint_first_triangle
      constraints constraint_index in
  if first_side = side && first_triangle = triangle then
    Boolean_constraints.constraint_second_side constraints constraint_index,
    Boolean_constraints.constraint_second_triangle constraints constraint_index
  else
    Boolean_constraints.constraint_first_side constraints constraint_index,
    first_triangle

let opponent_triangle_point constraints side triangle local =
  Boolean_constraints.Private.triangle_point constraints side triangle local

let sign_opposite left right = match left, right with
  | Predicates.Negative, Predicates.Positive
  | Predicates.Positive, Predicates.Negative -> true
  | _ -> false

let projection points =
  match Implicit_point.orient2d_xy points.(0) points.(1) points.(2) with
  | Predicates.Positive | Predicates.Negative -> 0
  | Predicates.Zero ->
      (match Implicit_point.orient2d_yz points.(0) points.(1) points.(2) with
       | Predicates.Positive | Predicates.Negative -> 1
       | Predicates.Zero -> 2)

let orient projection = match projection with
  | 0 -> Implicit_point.orient2d_xy
  | 1 -> Implicit_point.orient2d_yz
  | _ -> Implicit_point.orient2d_zx

module Orientation_cache = struct
  type t = {
    mutable key_a : int array;
    mutable key_b : int array;
    mutable key_c : int array;
    mutable signs : bytes;
    mutable count : int;
  }

  let create point_count =
    let capacity = ref 16 and needed = max 16 (point_count * 8) in
    while !capacity < needed do capacity := !capacity * 2 done;
    { key_a = Array.make !capacity (-1); key_b = Array.make !capacity (-1);
      key_c = Array.make !capacity (-1); signs = Bytes.make !capacity '\000';
      count = 0 }

  let[@inline] hash a b c =
    let value = (a * 0x1e35a7bd) lxor (b * 0x17a1465b)
        lxor (c * 0x45d9f3b) in
    (value * 0x45d9f3b) land max_int

  let find_slot key_a key_b key_c a b c =
    let mask = Array.length key_a - 1 in
    let slot = ref (hash a b c land mask) in
    while key_a.(!slot) >= 0
        && (key_a.(!slot) <> a || key_b.(!slot) <> b || key_c.(!slot) <> c) do
      slot := (!slot + 1) land mask
    done;
    !slot

  let grow cache =
    let previous_a = cache.key_a and previous_b = cache.key_b
    and previous_c = cache.key_c and previous_signs = cache.signs in
    cache.key_a <- Array.make (Array.length previous_a * 2) (-1);
    cache.key_b <- Array.make (Array.length cache.key_a) (-1);
    cache.key_c <- Array.make (Array.length cache.key_a) (-1);
    cache.signs <- Bytes.make (Array.length cache.key_a) '\000';
    for slot = 0 to Array.length previous_a - 1 do
      let a = previous_a.(slot) in
      if a >= 0 then begin
        let b = previous_b.(slot) and c = previous_c.(slot) in
        let target = find_slot cache.key_a cache.key_b cache.key_c a b c in
        cache.key_a.(target) <- a;
        cache.key_b.(target) <- b;
        cache.key_c.(target) <- c;
        Bytes.unsafe_set cache.signs target (Bytes.unsafe_get previous_signs slot)
      end
    done

  let encode = function
    | Predicates.Negative -> '\000'
    | Predicates.Zero -> '\001'
    | Predicates.Positive -> '\002'

  let decode value = match value with
    | '\000' -> Predicates.Negative
    | '\001' -> Predicates.Zero
    | _ -> Predicates.Positive

  let reverse = function
    | Predicates.Negative -> Predicates.Positive
    | Predicates.Positive -> Predicates.Negative
    | Predicates.Zero -> Predicates.Zero

  let get cache exact a b c =
    if a = b || b = c || c = a then Predicates.Zero
    else begin
      let inversions = (if a > b then 1 else 0) + (if a > c then 1 else 0)
          + (if b > c then 1 else 0) in
      let a, b, c =
        if a <= b then
          if b <= c then a, b, c
          else if a <= c then a, c, b else c, a, b
        else if a <= c then b, a, c
        else if b <= c then b, c, a else c, b, a in
      if cache.count * 3 >= Array.length cache.key_a * 2 then grow cache;
      let slot = find_slot cache.key_a cache.key_b cache.key_c a b c in
      let sign = if cache.key_a.(slot) = a then
          decode (Bytes.unsafe_get cache.signs slot)
        else begin
          let sign = exact a b c in
          cache.key_a.(slot) <- a;
          cache.key_b.(slot) <- b;
          cache.key_c.(slot) <- c;
          Bytes.unsafe_set cache.signs slot (encode sign);
          cache.count <- cache.count + 1;
          sign
        end in
      if inversions land 1 = 0 then sign else reverse sign
    end
end

let compare_axis axis = match axis with
  | 0 -> Implicit_point.compare_x
  | 1 -> Implicit_point.compare_y
  | _ -> Implicit_point.compare_z

let dominant_segment_axis first second =
  if Implicit_point.compare_x first second <> 0 then 0
  else if Implicit_point.compare_y first second <> 0 then 1
  else 2

let between first point second =
  let compare = compare_axis (dominant_segment_axis first second) in
  let order = compare first second in
  if order < 0 then compare first point <= 0 && compare point second <= 0
  else compare second point <= 0 && compare point first <= 0

let grow_int values needed =
  if needed <= Array.length values then values
  else begin
    let capacity = ref (max 4 (Array.length values)) in
    while !capacity < needed do
      capacity := min Sys.max_array_length
          (max (!capacity + 1) (!capacity * 2))
    done;
    let output = Array.make !capacity 0 in
    Array.blit values 0 output 0 (Array.length values);
    output
  end

let grow_points values needed fallback =
  if needed <= Array.length values then values
  else begin
    let capacity = ref (max 4 (Array.length values)) in
    while !capacity < needed do
      capacity := min Sys.max_array_length
          (max (!capacity + 1) (!capacity * 2))
    done;
    let output = Array.make !capacity fallback in
    Array.blit values 0 output 0 (Array.length values);
    output
  end

let sort_range values first last compare =
  let swap left right =
    let value = values.(left) in values.(left) <- values.(right);
    values.(right) <- value in
  for i = first + 1 to last - 1 do
    let j = ref i in
    while !j > first && compare values.(!j) values.(!j - 1) < 0 do
      swap !j (!j - 1); decr j
    done
  done

type candidate_plan = {
  candidate_first : int array;
  candidate_second : int array;
  candidate_order : int array;
  candidate_count : int;
}

type candidate_result = Small_scan | Candidates of candidate_plan | Dense

let candidate_limit item_count =
  let scaled = if item_count > 1_000_000 / 64 then 1_000_000
    else item_count * 64 in
  max 4_096 (min 1_000_000 scaled)

let initial_candidate_capacity item_count multiplier =
  let limit = candidate_limit item_count in
  let scaled = if item_count > limit / multiplier then limit
    else item_count * multiplier in
  min limit (max 64 scaled)

let finish_candidates first second count =
  let order = Array.init count Fun.id in
  Array.sort (fun left right ->
    let comparison = Int.compare first.(left) first.(right) in
    if comparison <> 0 then comparison
    else Int.compare second.(left) second.(right)) order;
  { candidate_first = first; candidate_second = second;
    candidate_order = order; candidate_count = count }

module Stable_bounds_bvh = struct
  type t = {
    item_u_min : float array;
    item_u_max : float array;
    item_v_min : float array;
    item_v_max : float array;
    node_u_min : float array;
    node_u_max : float array;
    node_v_min : float array;
    node_v_max : float array;
    node_left : int array;
    node_right : int array;
    node_first : int array;
    node_last : int array;
    node_count : int;
  }

  let build item_u_min item_u_max item_v_min item_v_max =
    let item_count = Array.length item_u_min in
    if Array.length item_u_max <> item_count
        || Array.length item_v_min <> item_count
        || Array.length item_v_max <> item_count then
      invalid_arg "Boolean face arrangement BVH plane cardinality mismatch";
    if item_count = 0 then invalid_arg "Boolean face arrangement BVH is empty";
    if item_count > (Sys.max_array_length - 1) / 2 then
      invalid_arg "Boolean face arrangement BVH exceeds array limits";
    let capacity = (item_count * 2) - 1 in
    let node_u_min = Array.make capacity 0.
    and node_u_max = Array.make capacity 0.
    and node_v_min = Array.make capacity 0.
    and node_v_max = Array.make capacity 0.
    and node_left = Array.make capacity (-1)
    and node_right = Array.make capacity (-1)
    and node_first = Array.make capacity 0
    and node_last = Array.make capacity 0
    and node_count = ref 0 in
    let rec make first last =
      let node = !node_count in
      incr node_count;
      node_first.(node) <- first;
      node_last.(node) <- last;
      let u_min = ref item_u_min.(first) and u_max = ref item_u_max.(first)
      and v_min = ref item_v_min.(first) and v_max = ref item_v_max.(first) in
      for item = first + 1 to last - 1 do
        u_min := min !u_min item_u_min.(item);
        u_max := max !u_max item_u_max.(item);
        v_min := min !v_min item_v_min.(item);
        v_max := max !v_max item_v_max.(item)
      done;
      node_u_min.(node) <- !u_min; node_u_max.(node) <- !u_max;
      node_v_min.(node) <- !v_min; node_v_max.(node) <- !v_max;
      if last - first > 8 then begin
        let middle = first + ((last - first) / 2) in
        node_left.(node) <- make first middle;
        node_right.(node) <- make middle last
      end;
      node in
    ignore (make 0 item_count);
    { item_u_min; item_u_max; item_v_min; item_v_max;
      node_u_min; node_u_max; node_v_min; node_v_max;
      node_left; node_right; node_first; node_last;
      node_count = !node_count }

  let overlaps u_min u_max v_min v_max other_u_min other_u_max
      other_v_min other_v_max =
    u_max >= other_u_min && other_u_max >= u_min
    && v_max >= other_v_min && other_v_max >= v_min

  let query ?cancel value stack output ~after ~u_min ~u_max ~v_min ~v_max =
    if Array.length stack < value.node_count
        || Array.length output < Array.length value.item_u_min then
      invalid_arg "Boolean face arrangement BVH scratch plane is too small";
    let stack_count = ref 1 and output_count = ref 0 and visited = ref 0 in
    stack.(0) <- 0;
    while !stack_count > 0 do
      decr stack_count;
      let node = stack.(!stack_count) in
      if overlaps u_min u_max v_min v_max
          value.node_u_min.(node) value.node_u_max.(node)
          value.node_v_min.(node) value.node_v_max.(node) then begin
        let left = value.node_left.(node) in
        if left < 0 then begin
          for item = value.node_first.(node) to value.node_last.(node) - 1 do
            if item > after
                && overlaps u_min u_max v_min v_max
                  value.item_u_min.(item) value.item_u_max.(item)
                  value.item_v_min.(item) value.item_v_max.(item) then begin
              output.(!output_count) <- item;
              incr output_count
            end
          done
        end else begin
          (* Push right first so stable index ranges are visited in ascending
             order. This preserves the exact oracle's construction order. *)
          stack.(!stack_count) <- value.node_right.(node);
          incr stack_count;
          stack.(!stack_count) <- left;
          incr stack_count
        end
      end;
      incr visited;
      if !visited land 1023 = 0 then Cancel.check_opt cancel
    done;
    !output_count
end

module Exact_point_index = struct
  type t = {
    mutable left : int array;
    mutable right : int array;
    mutable height : int array;
    mutable root : int;
  }

  let create expected =
    let capacity = max 4 expected in
    { left = Array.make capacity (-1);
      right = Array.make capacity (-1);
      height = Array.make capacity 0;
      root = -1 }

  let grow values capacity fallback =
    let output = Array.make capacity fallback in
    Array.blit values 0 output 0 (Array.length values);
    output

  let ensure value needed =
    if needed > Array.length value.left then begin
      let capacity = ref (Array.length value.left) in
      while !capacity < needed do
        if !capacity >= Sys.max_array_length then
          invalid_arg "Boolean face arrangement point index exceeds array limits";
        capacity := min Sys.max_array_length
            (max (!capacity + 1) (!capacity * 2))
      done;
      value.left <- grow value.left !capacity (-1);
      value.right <- grow value.right !capacity (-1);
      value.height <- grow value.height !capacity 0
    end

  let compare first second =
    let comparison = Implicit_point.compare_x first second in
    if comparison <> 0 then comparison else
      let comparison = Implicit_point.compare_y first second in
      if comparison <> 0 then comparison
      else Implicit_point.compare_z first second

  let node_height value node = if node < 0 then 0 else value.height.(node)

  let update value node =
    value.height.(node) <- 1 + max
        (node_height value value.left.(node))
        (node_height value value.right.(node))

  let rotate_left value node =
    let replacement = value.right.(node) in
    value.right.(node) <- value.left.(replacement);
    value.left.(replacement) <- node;
    update value node;
    update value replacement;
    replacement

  let rotate_right value node =
    let replacement = value.left.(node) in
    value.left.(node) <- value.right.(replacement);
    value.right.(replacement) <- node;
    update value node;
    update value replacement;
    replacement

  let balance value node =
    update value node;
    let difference = node_height value value.left.(node)
        - node_height value value.right.(node) in
    if difference > 1 then begin
      let left = value.left.(node) in
      if node_height value value.left.(left)
          < node_height value value.right.(left) then
        value.left.(node) <- rotate_left value left;
      rotate_right value node
    end else if difference < -1 then begin
      let right = value.right.(node) in
      if node_height value value.right.(right)
          < node_height value value.left.(right) then
        value.right.(node) <- rotate_right value right;
      rotate_left value node
    end else node

  let build value points count =
    ensure value count;
    let order = Array.init count Fun.id in
    Array.sort (fun left right ->
      let comparison = compare points.(left) points.(right) in
      if comparison <> 0 then comparison else Int.compare left right) order;
    let rec make first last =
      if first = last then -1 else begin
        let middle = first + ((last - first) / 2) in
        let node = order.(middle) in
        value.left.(node) <- make first middle;
        value.right.(node) <- make (middle + 1) last;
        update value node;
        node
      end in
    value.root <- make 0 count

  let find value points point =
    let current = ref value.root and found = ref (-1) in
    while !current >= 0 && !found < 0 do
      let comparison = compare point points.(!current) in
      if comparison = 0 then found := !current
      else current := if comparison < 0 then value.left.(!current)
        else value.right.(!current)
    done;
    !found

  let add value points node =
    ensure value (node + 1);
    value.left.(node) <- -1;
    value.right.(node) <- -1;
    value.height.(node) <- 1;
    let rec insert current =
      if current < 0 then node else begin
        let comparison = compare points.(node) points.(current) in
        if comparison < 0 then value.left.(current) <- insert value.left.(current)
        else if comparison > 0 then
          value.right.(current) <- insert value.right.(current)
        else invalid_arg "Boolean face arrangement point index duplicated a point";
        balance value current
      end in
    value.root <- insert value.root
end

module Split_set = struct
  type t = {
    mutable segments : int array;
    mutable points : int array;
    mutable count : int;
  }

  let capacity_for expected =
    let needed = max 16 (if expected > Sys.max_array_length / 2 then
        Sys.max_array_length else expected * 2) in
    let capacity = ref 16 in
    while !capacity < needed do
      if !capacity > Sys.max_array_length / 2 then
        invalid_arg "Boolean face arrangement split set exceeds array limits";
      capacity := !capacity * 2
    done;
    !capacity

  let create expected =
    let capacity = capacity_for expected in
    { segments = Array.make capacity (-1);
      points = Array.make capacity (-1);
      count = 0 }

  let hash segment point =
    Rays_math.Hash.mix_with 65_599 segment (point * 1_000_003) land max_int

  let find_slot value segment point =
    let mask = Array.length value.segments - 1 in
    let slot = ref (hash segment point land mask) in
    while value.segments.(!slot) >= 0
        && (value.segments.(!slot) <> segment || value.points.(!slot) <> point) do
      slot := (!slot + 1) land mask
    done;
    !slot

  let add_raw value segment point =
    let slot = find_slot value segment point in
    if value.segments.(slot) = segment && value.points.(slot) = point then false
    else begin
      value.segments.(slot) <- segment;
      value.points.(slot) <- point;
      value.count <- value.count + 1;
      true
    end

  let rehash value capacity =
    let segments = value.segments and points = value.points in
    value.segments <- Array.make capacity (-1);
    value.points <- Array.make capacity (-1);
    value.count <- 0;
    for slot = 0 to Array.length segments - 1 do
      if segments.(slot) >= 0 then ignore (add_raw value segments.(slot) points.(slot))
    done

  let add value segment point =
    let capacity = Array.length value.segments in
    if value.count + 1 > capacity - (capacity / 3) then begin
      if capacity > Sys.max_array_length / 2 then
        invalid_arg "Boolean face arrangement split set exceeds array limits";
      rehash value (capacity * 2)
    end;
    add_raw value segment point

  let mem value segment point =
    let slot = find_slot value segment point in
    value.segments.(slot) = segment && value.points.(slot) = point
end

let projected_bounds points point_count projection =
  let u_min = Array.make point_count 0. and u_max = Array.make point_count 0.
  and v_min = Array.make point_count 0. and v_max = Array.make point_count 0. in
  for point = 0 to point_count - 1 do
    let (x_min,x_max),(y_min,y_max),(z_min,z_max) =
      Implicit_point.bounds points.(point) in
    let first_min, first_max, second_min, second_max = match projection with
      | 0 -> x_min, x_max, y_min, y_max
      | 1 -> y_min, y_max, z_min, z_max
      | _ -> z_min, z_max, x_min, x_max in
    u_min.(point) <- first_min; u_max.(point) <- first_max;
    v_min.(point) <- second_min; v_max.(point) <- second_max
  done;
  u_min, u_max, v_min, v_max

let segment_bounds segment_first segment_second segment_count
    point_u_min point_u_max point_v_min point_v_max =
  let u_min = Array.make segment_count 0. and u_max = Array.make segment_count 0.
  and v_min = Array.make segment_count 0. and v_max = Array.make segment_count 0. in
  for segment = 0 to segment_count - 1 do
    let first = segment_first.(segment) and second = segment_second.(segment) in
    u_min.(segment) <- min point_u_min.(first) point_u_min.(second);
    u_max.(segment) <- max point_u_max.(first) point_u_max.(second);
    v_min.(segment) <- min point_v_min.(first) point_v_min.(second);
    v_max.(segment) <- max point_v_max.(first) point_v_max.(second)
  done;
  u_min, u_max, v_min, v_max

let interval_score lower upper =
  let count = Array.length lower in
  if count = 0 then 0. else begin
    let global_lower = ref lower.(0) and global_upper = ref upper.(0)
    and widths = ref 0. in
    for index = 0 to count - 1 do
      global_lower := min !global_lower lower.(index);
      global_upper := max !global_upper upper.(index);
      widths := !widths +. max 0. (upper.(index) -. lower.(index))
    done;
    let span = !global_upper -. !global_lower in
    if span > 0. then !widths /. span else max_float
  end

let choose_sweep_axis u_min u_max v_min v_max =
  if interval_score v_min v_max < interval_score u_min u_max then
    v_min, v_max, u_min, u_max
  else u_min, u_max, v_min, v_max

let segment_pair_candidates ?cancel segment_first segment_second segment_count
    points point_count projection =
  if segment_count < 32 then Small_scan else begin
    let point_u_min, point_u_max, point_v_min, point_v_max =
      projected_bounds points point_count projection in
    let u_min,u_max,v_min,v_max = segment_bounds segment_first segment_second
        segment_count point_u_min point_u_max point_v_min point_v_max in
    let u_min,u_max,v_min,v_max = choose_sweep_axis u_min u_max v_min v_max in
    let order = Array.init segment_count Fun.id in
    Array.sort (fun left right ->
      let comparison = Float.compare u_min.(left) u_min.(right) in
      if comparison <> 0 then comparison else Int.compare left right) order;
    let active = Array.make segment_count 0 and active_count = ref 0
    and first = ref (Array.make (initial_candidate_capacity segment_count 1) 0)
    and second = ref (Array.make (initial_candidate_capacity segment_count 1) 0)
    and count = ref 0 and dense = ref false in
    let limit = candidate_limit segment_count in
    let add left right =
      if !count = limit then dense := true else begin
        first := grow_int !first (!count + 1);
        second := grow_int !second (!count + 1);
        (!first).(!count) <- min left right;
        (!second).(!count) <- max left right;
        incr count
      end in
    let event = ref 0 in
    while !event < segment_count && not !dense do
      if !event land 1023 = 0 then Cancel.check_opt cancel;
      let current = order.(!event) and retained = ref 0 in
      for slot = 0 to !active_count - 1 do
        let candidate = active.(slot) in
        if u_max.(candidate) >= u_min.(current) then begin
          active.(!retained) <- candidate; incr retained;
          if v_max.(candidate) >= v_min.(current)
              && v_max.(current) >= v_min.(candidate) then
            add candidate current
        end
      done;
      active_count := !retained;
      active.(!active_count) <- current;
      incr active_count;
      incr event
    done;
    if !dense then Dense
    else Candidates (finish_candidates !first !second !count)
  end

let point_segment_candidates ?cancel segment_first segment_second segment_count
    points point_count projection =
  if segment_count < 32 || point_count < 32 then Small_scan else begin
    let point_u_min, point_u_max, point_v_min, point_v_max =
      projected_bounds points point_count projection in
    let segment_u_min,segment_u_max,segment_v_min,segment_v_max =
      segment_bounds segment_first segment_second segment_count
        point_u_min point_u_max point_v_min point_v_max in
    let swap = interval_score segment_v_min segment_v_max
        < interval_score segment_u_min segment_u_max in
    let point_u_min,point_u_max,point_v_min,point_v_max,
        segment_u_min,segment_u_max,segment_v_min,segment_v_max =
      if swap then
        point_v_min,point_v_max,point_u_min,point_u_max,
        segment_v_min,segment_v_max,segment_u_min,segment_u_max
      else
        point_u_min,point_u_max,point_v_min,point_v_max,
        segment_u_min,segment_u_max,segment_v_min,segment_v_max in
    let event_count = segment_count + point_count in
    let events = Array.init event_count Fun.id in
    let event_lower event = if event < segment_count then segment_u_min.(event)
      else point_u_min.(event - segment_count) in
    Array.sort (fun left right ->
      let comparison = Float.compare (event_lower left) (event_lower right) in
      if comparison <> 0 then comparison else Int.compare left right) events;
    let active_segments = Array.make segment_count 0 and active_segment_count = ref 0
    and active_points = Array.make point_count 0 and active_point_count = ref 0
    and first = ref (Array.make (initial_candidate_capacity segment_count 2) 0)
    and second = ref (Array.make (initial_candidate_capacity segment_count 2) 0)
    and count = ref 0 and dense = ref false in
    let limit = candidate_limit segment_count in
    let add segment point =
      if !count = limit then dense := true else begin
        first := grow_int !first (!count + 1);
        second := grow_int !second (!count + 1);
        (!first).(!count) <- segment; (!second).(!count) <- point;
        incr count
      end in
    let event_slot = ref 0 in
    while !event_slot < event_count && not !dense do
      if !event_slot land 1023 = 0 then Cancel.check_opt cancel;
      let event = events.(!event_slot) in
      if event < segment_count then begin
        let segment = event and retained = ref 0 in
        for slot = 0 to !active_point_count - 1 do
          let point = active_points.(slot) in
          if point_u_max.(point) >= segment_u_min.(segment) then begin
            active_points.(!retained) <- point; incr retained;
            if point_v_max.(point) >= segment_v_min.(segment)
                && segment_v_max.(segment) >= point_v_min.(point) then
              add segment point
          end
        done;
        active_point_count := !retained;
        active_segments.(!active_segment_count) <- segment;
        incr active_segment_count
      end else begin
        let point = event - segment_count and retained = ref 0 in
        for slot = 0 to !active_segment_count - 1 do
          let segment = active_segments.(slot) in
          if segment_u_max.(segment) >= point_u_min.(point) then begin
            active_segments.(!retained) <- segment; incr retained;
            if segment_v_max.(segment) >= point_v_min.(point)
                && point_v_max.(point) >= segment_v_min.(segment) then
              add segment point
          end
        done;
        active_segment_count := !retained;
        active_points.(!active_point_count) <- point;
        incr active_point_count
      end;
      incr event_slot
    done;
    if !dense then Dense
    else Candidates (finish_candidates !first !second !count)
  end

let canonicalize_points points count =
  if count = 0 then [||], [||] else begin
    let compare_point left right =
      let comparison = Implicit_point.compare_x points.(left) points.(right) in
      if comparison <> 0 then comparison else
        let comparison = Implicit_point.compare_y points.(left) points.(right) in
        if comparison <> 0 then comparison else
          let comparison = Implicit_point.compare_z points.(left) points.(right) in
          if comparison <> 0 then comparison else Int.compare left right in
    let same_point left right =
      Implicit_point.equal points.(left) points.(right) in
    let order = Array.init count Fun.id in
    Array.sort compare_point order;
    let representative = Array.make count 0 and first = ref 0 in
    while !first < count do
      let last = ref (!first + 1) and earliest = ref order.(!first) in
      while !last < count && same_point order.(!first) order.(!last) do
        earliest := min !earliest order.(!last);
        incr last
      done;
      for slot = !first to !last - 1 do
        representative.(order.(slot)) <- !earliest
      done;
      first := !last
    done;
    let old_to_new = Array.make count 0 and unique_count = ref 0 in
    for point = 0 to count - 1 do
      if representative.(point) = point then begin
        old_to_new.(point) <- !unique_count;
        incr unique_count
      end
    done;
    for point = 0 to count - 1 do
      old_to_new.(point) <- old_to_new.(representative.(point))
    done;
    let output = Array.make !unique_count points.(0) in
    for point = 0 to count - 1 do
      if representative.(point) = point then
        output.(old_to_new.(point)) <- points.(point)
    done;
    output, old_to_new
  end

let build ?cancel ?coplanar constraints ~side ~triangle =
  try
    Cancel.check_opt cancel;
    Option.iter (fun value ->
      if Boolean_coplanar.Private.constraints value != constraints then
        invalid_arg "coplanar plan belongs to a different constraint plan") coplanar;
    let first_slot, last_slot = face_constraint_range constraints side triangle in
    let face_constraint_count = last_slot - first_slot in
    let first_pair, last_pair = match coplanar with
      | None -> 0, 0
      | Some coplanar -> face_coplanar_range coplanar side triangle in
    if face_constraint_count = 0 && first_pair = last_pair then
      Ok { points = [||]; point_handles = [||];
           segment_first = [||]; segment_second = [||] }
    else begin
      let raw_segment_capacity = ref 0 and point_capacity = ref 0 in
      for slot = first_slot to last_slot - 1 do
        let constraint_index = face_constraint constraints side slot in
        let first = Boolean_constraints.constraint_first constraints constraint_index
        and second = Boolean_constraints.constraint_second constraints constraint_index in
        incr point_capacity;
        if second <> first then begin
          incr point_capacity;
          incr raw_segment_capacity
        end
      done;
      Option.iter (fun coplanar ->
        for slot = first_pair to last_pair - 1 do
          let pair = face_coplanar_pair coplanar side slot in
          point_capacity := !point_capacity + Boolean_coplanar.point_count coplanar pair;
          raw_segment_capacity := !raw_segment_capacity
              + Boolean_coplanar.boundary_count coplanar pair
        done) coplanar;
      let source = Boolean_constraints.Private.source constraints in
      let fallback = Implicit_point.explicit source
          (face_triangle_point constraints side triangle 0) |> Result.get_ok in
      let points = ref (Array.make (max 4 !point_capacity) fallback)
      and point_handles = ref (Array.make (max 4 !point_capacity) (-1))
      and point_count = ref 0 in
      let append_point ?(handle = -1) point =
        points := grow_points !points (!point_count + 1) fallback;
        point_handles := grow_int !point_handles (!point_count + 1);
        (!points).(!point_count) <- point;
        (!point_handles).(!point_count) <- handle;
        let result = !point_count in incr point_count; result in
      let segment_first = Array.make !raw_segment_capacity 0
      and segment_second = Array.make !raw_segment_capacity 0
      and segment_constraint = Array.make !raw_segment_capacity (-1)
      and segment_coplanar = Bytes.make !raw_segment_capacity '\000'
      and segment_support_first = Array.make !raw_segment_capacity (-1)
      and segment_support_second = Array.make !raw_segment_capacity (-1)
      and raw_segment_count = ref 0 in
      let append_segment first second constraint_index support_first support_second =
        let segment = !raw_segment_count in
        segment_first.(segment) <- first;
        segment_second.(segment) <- second;
        segment_constraint.(segment) <- constraint_index;
        if support_first >= 0 then begin
          Bytes.unsafe_set segment_coplanar segment '\001';
          segment_support_first.(segment) <- support_first;
          segment_support_second.(segment) <- support_second
        end;
        incr raw_segment_count in
      for slot = first_slot to last_slot - 1 do
        let constraint_index = face_constraint constraints side slot in
        let first_handle = Boolean_constraints.constraint_first
            constraints constraint_index
        and second_handle = Boolean_constraints.constraint_second
            constraints constraint_index in
        let first = append_point ~handle:first_handle
            (Boolean_constraints.Private.point constraints first_handle) in
        if first_handle <> second_handle then begin
          let second = append_point ~handle:second_handle
              (Boolean_constraints.Private.point constraints second_handle) in
          append_segment first second constraint_index (-1) (-1)
        end
      done;
      Option.iter (fun coplanar ->
        for slot = first_pair to last_pair - 1 do
          let pair = face_coplanar_pair coplanar side slot in
          let count = Boolean_coplanar.point_count coplanar pair in
          let point_map = Array.init count (fun point -> append_point
              (Boolean_coplanar.Private.point coplanar pair point)) in
          for boundary = 0 to Boolean_coplanar.boundary_count coplanar pair - 1 do
            append_segment
              point_map.(Boolean_coplanar.boundary_first coplanar pair boundary)
              point_map.(Boolean_coplanar.boundary_second coplanar pair boundary)
              (-1)
              (Boolean_coplanar.Private.boundary_support_first
                 coplanar pair boundary)
              (Boolean_coplanar.Private.boundary_support_second
                 coplanar pair boundary)
          done
        done) coplanar;
      let raw_point_count = !point_count in
      let canonical, point_map = canonicalize_points !points raw_point_count in
      let canonical_handles = Array.make (Array.length canonical) (-1) in
      for point = 0 to raw_point_count - 1 do
        let handle = (!point_handles).(point) in
        if handle >= 0 then begin
          let target = point_map.(point) in
          let present = canonical_handles.(target) in
          if present < 0 || handle < present then canonical_handles.(target) <- handle
        end
      done;
      points := canonical;
      point_handles := canonical_handles;
      point_count := Array.length canonical;
      for segment = 0 to !raw_segment_count - 1 do
        segment_first.(segment) <- point_map.(segment_first.(segment));
        segment_second.(segment) <- point_map.(segment_second.(segment))
      done;
      if !point_count = 0 then
        Ok { points = [||]; point_handles = [||];
             segment_first = [||]; segment_second = [||] }
      else begin
      let face_points = Array.init 3 (fun local ->
        Implicit_point.explicit source
          (face_triangle_point constraints side triangle local)
        |> Result.get_ok) in
      let projection_axis = projection face_points in
      let orient_points = orient projection_axis in
      let orientation_cache = Orientation_cache.create !point_count in
      let orient_ids first second third =
        Orientation_cache.get orientation_cache
          (fun first second third ->
            orient_points (!points).(first) (!points).(second) (!points).(third))
          first second third in
      let point_index = Exact_point_index.create !point_count in
      Exact_point_index.build point_index !points !point_count;
      let intern_point point =
        let existing = Exact_point_index.find point_index !points point in
        if existing >= 0 then existing else begin
          let point_id = append_point point in
          Exact_point_index.add point_index !points point_id;
          point_id
        end in
      let split_segments = ref (Array.make (max 4 (!raw_segment_count * 2)) 0)
      and split_points = ref (Array.make (max 4 (!raw_segment_count * 2)) 0)
      and split_next = ref (Array.make (max 4 (!raw_segment_count * 2)) (-1))
      and split_count = ref 0 in
      let split_set = Split_set.create (max 4 (!raw_segment_count * 3)) in
      let split_head = Array.make !raw_segment_count (-1)
      and segment_split_count = Array.make !raw_segment_count 0 in
      let add_split segment point =
        if Split_set.add split_set segment point then begin
          let needed = !split_count + 1 in
          split_segments := grow_int !split_segments needed;
          split_points := grow_int !split_points needed;
          split_next := grow_int !split_next needed;
          (!split_segments).(!split_count) <- segment;
          (!split_points).(!split_count) <- point;
          (!split_next).(!split_count) <- split_head.(segment);
          split_head.(segment) <- !split_count;
          segment_split_count.(segment) <- segment_split_count.(segment) + 1;
          incr split_count
        end in
      for segment = 0 to !raw_segment_count - 1 do
        add_split segment segment_first.(segment);
        add_split segment segment_second.(segment)
      done;
      let existing_crossing left right =
        let common source_segment target_segment =
          if segment_split_count.(source_segment) > 16 then -1 else begin
            let split = ref split_head.(source_segment) and found = ref (-1) in
            while !split >= 0 && !found < 0 do
              let point_id = (!split_points).(!split) in
              if Split_set.mem split_set target_segment point_id then
                found := point_id;
              split := (!split_next).(!split)
            done;
            !found
          end in
        let search source_segment target_segment =
          if segment_split_count.(source_segment) > 16 then -1 else begin
            let target_first_id = segment_first.(target_segment)
            and target_second_id = segment_second.(target_segment) in
            let target_first = (!points).(target_first_id)
            and target_second = (!points).(target_second_id) in
            let split = ref split_head.(source_segment) and found = ref (-1) in
            while !split >= 0 && !found < 0 do
              let point_id = (!split_points).(!split) in
              if point_id <> target_first_id && point_id <> target_second_id then begin
                let point = (!points).(point_id) in
                if orient_ids target_first_id target_second_id point_id
                    = Predicates.Zero
                    && between target_first point target_second then
                  found := point_id
              end;
              split := (!split_next).(!split)
            done;
            !found
          end in
        let found = common left right in
        if found >= 0 then found else begin
          let found = search left right in
          if found >= 0 then found else search right left
        end in
      let process_segment_pair left right =
        let a_id = segment_first.(left) and b_id = segment_second.(left)
        and c_id = segment_first.(right) and d_id = segment_second.(right) in
        if a_id <> c_id && a_id <> d_id && b_id <> c_id && b_id <> d_id then begin
          let o1 = orient_ids a_id b_id c_id
          and o2 = orient_ids a_id b_id d_id
          and o3 = orient_ids c_id d_id a_id
          and o4 = orient_ids c_id d_id b_id in
          if sign_opposite o1 o2 && sign_opposite o3 o4 then begin
            let existing = existing_crossing left right in
            if existing >= 0 then begin
              add_split left existing;
              add_split right existing
            end else begin
            let plane_point face local = face_triangle_point constraints side face local in
            let coplanar_left = Bytes.unsafe_get segment_coplanar left <> '\000'
            and coplanar_right = Bytes.unsafe_get segment_coplanar right <> '\000' in
            let projected = match projection_axis with
              | 0 -> Implicit_point.XY
              | 1 -> Implicit_point.YZ
              | _ -> Implicit_point.ZX in
            let line_plane coplanar_segment noncoplanar_segment =
              let opponent_side, opponent = opponent constraints side triangle
                  segment_constraint.(noncoplanar_segment) in
              Implicit_point.line_plane source
                ~line_start:segment_support_first.(coplanar_segment)
                ~line_end:segment_support_second.(coplanar_segment)
                ~plane_a:(opponent_triangle_point constraints opponent_side opponent 0)
                ~plane_b:(opponent_triangle_point constraints opponent_side opponent 1)
                ~plane_c:(opponent_triangle_point constraints opponent_side opponent 2) in
            let point = match coplanar_left, coplanar_right with
              | false, false ->
                  let left_side, left_opponent = opponent constraints side triangle
                      segment_constraint.(left)
                  and right_side, right_opponent = opponent constraints side triangle
                      segment_constraint.(right) in
                  Implicit_point.triple_plane source
                    ~first_a:(plane_point triangle 0)
                    ~first_b:(plane_point triangle 1)
                    ~first_c:(plane_point triangle 2)
                    ~second_a:(opponent_triangle_point constraints left_side left_opponent 0)
                    ~second_b:(opponent_triangle_point constraints left_side left_opponent 1)
                    ~second_c:(opponent_triangle_point constraints left_side left_opponent 2)
                    ~third_a:(opponent_triangle_point constraints right_side right_opponent 0)
                    ~third_b:(opponent_triangle_point constraints right_side right_opponent 1)
                    ~third_c:(opponent_triangle_point constraints right_side right_opponent 2)
              | true, true ->
                  Implicit_point.line_line source ~projection:projected
                    ~first_start:segment_support_first.(left)
                    ~first_end:segment_support_second.(left)
                    ~second_start:segment_support_first.(right)
                    ~second_end:segment_support_second.(right)
              | true, false -> line_plane left right
              | false, true -> line_plane right left in
            (match point with
             | Error _ -> invalid_arg "exact construction failed at proper constraint crossing"
             | Ok point ->
                 let point = intern_point point in
                 add_split left point; add_split right point)
            end
          end else begin
            let add_if_on point_id first_id second_id segment =
              if orient_ids first_id second_id point_id = Predicates.Zero
                  && between (!points).(first_id) (!points).(point_id)
                    (!points).(second_id)
              then add_split segment point_id in
            add_if_on c_id a_id b_id left;
            add_if_on d_id a_id b_id left;
            add_if_on a_id c_id d_id right;
            add_if_on b_id c_id d_id right
          end
        end in
      let scan_segment_pairs () =
        for left = 0 to !raw_segment_count - 1 do
          if left land 255 = 0 then Cancel.check_opt cancel;
          for right = left + 1 to !raw_segment_count - 1 do
            process_segment_pair left right
          done
        done in
      let indexed_segment_pairs () =
        let point_u_min,point_u_max,point_v_min,point_v_max =
          projected_bounds !points !point_count projection_axis in
        let u_min,u_max,v_min,v_max = segment_bounds segment_first segment_second
            !raw_segment_count point_u_min point_u_max point_v_min point_v_max in
        let u_min,u_max,v_min,v_max = choose_sweep_axis u_min u_max v_min v_max in
        let index = Stable_bounds_bvh.build u_min u_max v_min v_max in
        let stack = Array.make index.Stable_bounds_bvh.node_count 0
        and candidates = Array.make !raw_segment_count 0 in
        for left = 0 to !raw_segment_count - 1 do
          if left land 255 = 0 then Cancel.check_opt cancel;
          let count = Stable_bounds_bvh.query ?cancel index stack candidates
              ~after:left ~u_min:u_min.(left) ~u_max:u_max.(left)
              ~v_min:v_min.(left) ~v_max:v_max.(left) in
          for slot = 0 to count - 1 do
            process_segment_pair left candidates.(slot)
          done
        done in
      (match segment_pair_candidates ?cancel segment_first segment_second
          !raw_segment_count !points !point_count projection_axis with
      | Small_scan -> scan_segment_pairs ()
      | Dense -> indexed_segment_pairs ()
      | Candidates candidates ->
          for slot = 0 to candidates.candidate_count - 1 do
            if slot land 1023 = 0 then Cancel.check_opt cancel;
            let candidate = candidates.candidate_order.(slot) in
            process_segment_pair candidates.candidate_first.(candidate)
              candidates.candidate_second.(candidate)
          done);
      (* A point-only contact or a multi-way TPI may lie in the interior of a
         segment without participating in that segment's pair which created
         it. Insert every exact on-segment point before materializing edges. *)
      let process_point_segment segment point_id =
        let first_id = segment_first.(segment)
        and second_id = segment_second.(segment) in
        if point_id <> first_id && point_id <> second_id then begin
          let first = (!points).(first_id) and second = (!points).(second_id)
          and point = (!points).(point_id) in
          if orient_ids first_id second_id point_id = Predicates.Zero
              && between first point second then add_split segment point_id
        end in
      let scan_point_segments () =
        for segment = 0 to !raw_segment_count - 1 do
          if segment land 255 = 0 then Cancel.check_opt cancel;
          for point_id = 0 to !point_count - 1 do
            process_point_segment segment point_id
          done
        done in
      let indexed_point_segments () =
        let point_u_min,point_u_max,point_v_min,point_v_max =
          projected_bounds !points !point_count projection_axis in
        let segment_u_min,segment_u_max,segment_v_min,segment_v_max =
          segment_bounds segment_first segment_second !raw_segment_count
            point_u_min point_u_max point_v_min point_v_max in
        let swap = interval_score segment_v_min segment_v_max
            < interval_score segment_u_min segment_u_max in
        let point_u_min,point_u_max,point_v_min,point_v_max,
            segment_u_min,segment_u_max,segment_v_min,segment_v_max =
          if swap then
            point_v_min,point_v_max,point_u_min,point_u_max,
            segment_v_min,segment_v_max,segment_u_min,segment_u_max
          else
            point_u_min,point_u_max,point_v_min,point_v_max,
            segment_u_min,segment_u_max,segment_v_min,segment_v_max in
        let index = Stable_bounds_bvh.build point_u_min point_u_max
            point_v_min point_v_max in
        let stack = Array.make index.Stable_bounds_bvh.node_count 0
        and candidates = Array.make !point_count 0 in
        for segment = 0 to !raw_segment_count - 1 do
          if segment land 255 = 0 then Cancel.check_opt cancel;
          let count = Stable_bounds_bvh.query ?cancel index stack candidates
              ~after:(-1) ~u_min:segment_u_min.(segment)
              ~u_max:segment_u_max.(segment) ~v_min:segment_v_min.(segment)
              ~v_max:segment_v_max.(segment) in
          for slot = 0 to count - 1 do
            process_point_segment segment candidates.(slot)
          done
        done in
      (match point_segment_candidates ?cancel segment_first segment_second
          !raw_segment_count !points !point_count projection_axis with
      | Small_scan -> scan_point_segments ()
      | Dense -> indexed_point_segments ()
      | Candidates candidates ->
          for slot = 0 to candidates.candidate_count - 1 do
            if slot land 1023 = 0 then Cancel.check_opt cancel;
            let candidate = candidates.candidate_order.(slot) in
            process_point_segment candidates.candidate_first.(candidate)
              candidates.candidate_second.(candidate)
          done);
      let counts = Array.make !raw_segment_count 0 in
      for split = 0 to !split_count - 1 do
        counts.((!split_segments).(split)) <-
          counts.((!split_segments).(split)) + 1
      done;
      let offsets = Array.make (!raw_segment_count + 1) 0 in
      for segment = 0 to !raw_segment_count - 1 do
        offsets.(segment + 1) <- offsets.(segment) + counts.(segment)
      done;
      let values = Array.make !split_count 0 and cursors = Array.copy offsets in
      for split = 0 to !split_count - 1 do
        let segment = (!split_segments).(split) in
        values.(cursors.(segment)) <- (!split_points).(split);
        cursors.(segment) <- cursors.(segment) + 1
      done;
      let output_count = ref 0 in
      for segment = 0 to !raw_segment_count - 1 do
        let first = offsets.(segment) and last = offsets.(segment + 1) in
        let axis = dominant_segment_axis
            (!points).(segment_first.(segment)) (!points).(segment_second.(segment)) in
        let compare = compare_axis axis in
        sort_range values first last (fun left right ->
          compare (!points).(left) (!points).(right));
        for split = first + 1 to last - 1 do
          if values.(split) <> values.(split - 1) then incr output_count
        done
      done;
      let output_first = Array.make !output_count 0
      and output_second = Array.make !output_count 0 and output = ref 0 in
      for segment = 0 to !raw_segment_count - 1 do
        let first = offsets.(segment) and last = offsets.(segment + 1) in
        let previous = ref values.(first) in
        for split = first + 1 to last - 1 do
          let point = values.(split) in
          if point <> !previous then begin
            output_first.(!output) <- !previous;
            output_second.(!output) <- point;
            incr output;
            previous := point
          end
        done
      done;
      let order = Array.init !output_count Fun.id in
      Array.sort (fun left right ->
        let left_a = min output_first.(left) output_second.(left)
        and left_b = max output_first.(left) output_second.(left)
        and right_a = min output_first.(right) output_second.(right)
        and right_b = max output_first.(right) output_second.(right) in
        let comparison = Int.compare left_a right_a in
        if comparison <> 0 then comparison else Int.compare left_b right_b) order;
      let unique_segments = ref 0 and previous_a = ref (-1)
      and previous_b = ref (-1) in
      Array.iter (fun segment ->
        let a = min output_first.(segment) output_second.(segment)
        and b = max output_first.(segment) output_second.(segment) in
        if a <> !previous_a || b <> !previous_b then begin
          incr unique_segments; previous_a := a; previous_b := b
        end) order;
      let unique_first = Array.make !unique_segments 0
      and unique_second = Array.make !unique_segments 0
      and output = ref 0 in
      previous_a := -1; previous_b := -1;
      Array.iter (fun segment ->
        let a = min output_first.(segment) output_second.(segment)
        and b = max output_first.(segment) output_second.(segment) in
        if a <> !previous_a || b <> !previous_b then begin
          unique_first.(!output) <- a; unique_second.(!output) <- b;
          incr output; previous_a := a; previous_b := b
        end) order;
      Ok {
        points = Array.sub !points 0 !point_count;
        point_handles = Array.sub !point_handles 0 !point_count;
        segment_first = unique_first;
        segment_second = unique_second;
      }
    end
    end
  with
  | Cancel.Cancelled -> error "cancelled" "Boolean face arrangement was cancelled"
  | Invalid_argument message -> error "invalid_constraints" message
