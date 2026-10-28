type t = {
  source_count : int;

  unique_source : int array;
  source_unique : int array;
  triangle_points : int array;
}

type result_t = t

let unique_count value = Array.length value.unique_source

let check_index label count index =
  if index < 0 || index >= count then
    invalid_arg (Printf.sprintf "Delaunay2.%s: index %d is outside [0,%d)"
      label index count)

let unique_source value unique =
  check_index "unique_source" (unique_count value) unique;
  value.unique_source.(unique)

let source_unique value source =
  check_index "source_unique" value.source_count source;
  value.source_unique.(source)

module View = struct
  type view = {

    unique_source : int array;

    triangle_points : int array;
  }

  let view (value : result_t) = {

    unique_source = value.unique_source;

    triangle_points = value.triangle_points;
  }
end

type triangles = {
  mutable a : int array;
  mutable b : int array;
  mutable c : int array;
  mutable n0 : int array;
  mutable n1 : int array;
  mutable n2 : int array;
  mutable alive : bool array;
  mutable count : int;
  mutable free : int array;
  mutable free_count : int;
}

let make_triangles capacity = {
  a = Array.make capacity 0;
  b = Array.make capacity 0;
  c = Array.make capacity 0;
  n0 = Array.make capacity (-1);
  n1 = Array.make capacity (-1);
  n2 = Array.make capacity (-1);
  alive = Array.make capacity false;
  count = 0;
  free = Array.make capacity 0;
  free_count = 0;
}

let grow_int source capacity default =
  let output = Array.make capacity default in
  Array.blit source 0 output 0 (Array.length source);
  output

let grow_bool source capacity =
  let output = Array.make capacity false in
  Array.blit source 0 output 0 (Array.length source);
  output

let ensure_triangle_capacity triangles needed =
  if needed > Array.length triangles.a then begin
    let capacity = ref (max 16 (Array.length triangles.a * 2)) in
    while !capacity < needed do
      if !capacity > Sys.max_array_length / 2 then
        invalid_arg "Delaunay triangulation exceeds array limits";
      capacity := !capacity * 2
    done;
    triangles.a <- grow_int triangles.a !capacity 0;
    triangles.b <- grow_int triangles.b !capacity 0;
    triangles.c <- grow_int triangles.c !capacity 0;
    triangles.n0 <- grow_int triangles.n0 !capacity (-1);
    triangles.n1 <- grow_int triangles.n1 !capacity (-1);
    triangles.n2 <- grow_int triangles.n2 !capacity (-1);
    triangles.alive <- grow_bool triangles.alive !capacity;
    triangles.free <- grow_int triangles.free !capacity 0
  end

let allocate_triangle triangles =
  if triangles.free_count > 0 then begin
    triangles.free_count <- triangles.free_count - 1;
    triangles.free.(triangles.free_count)
  end else begin
    ensure_triangle_capacity triangles (triangles.count + 1);
    let result = triangles.count in
    triangles.count <- result + 1;
    result
  end

let release_triangle triangles triangle =
  if not triangles.alive.(triangle) then
    invalid_arg "Delaunay triangulation released an inactive triangle";
  triangles.alive.(triangle) <- false;
  if triangles.free_count = Array.length triangles.free then begin
    if triangles.free_count >= Sys.max_array_length then
      invalid_arg "Delaunay free list exceeds array limits";
    triangles.free <- grow_int triangles.free
        (min Sys.max_array_length (max 16 (triangles.free_count * 2))) 0
  end;
  triangles.free.(triangles.free_count) <- triangle;
  triangles.free_count <- triangles.free_count + 1

let neighbor triangles triangle edge = match edge with
  | 0 -> triangles.n0.(triangle)
  | 1 -> triangles.n1.(triangle)
  | _ -> triangles.n2.(triangle)

let[@inline always] edge_first triangles triangle edge = match edge with
  | 0 -> triangles.a.(triangle)
  | 1 -> triangles.b.(triangle)
  | _ -> triangles.c.(triangle)

let[@inline always] edge_second triangles triangle edge = match edge with
  | 0 -> triangles.b.(triangle)
  | 1 -> triangles.c.(triangle)
  | _ -> triangles.a.(triangle)

let replace_neighbor_edge triangles triangle first second replacement =
  if triangle >= 0 then begin
    let found = ref (-1) in
    for edge = 0 to 2 do
      let other_first = edge_first triangles triangle edge
      and other_second = edge_second triangles triangle edge in
      if other_first = second && other_second = first then found := edge
    done;
    if !found < 0 then
      invalid_arg "Delaunay triangulation lost a boundary edge"
    else match !found with
      | 0 -> triangles.n0.(triangle) <- replacement
      | 1 -> triangles.n1.(triangle) <- replacement
      | _ -> triangles.n2.(triangle) <- replacement
  end

let compare_source_position x y left right =
  let comparison = Float.compare x.(left) x.(right) in
  if comparison <> 0 then comparison else
  let comparison = Float.compare y.(left) y.(right) in
  if comparison <> 0 then comparison else Int.compare left right

let exact_duplicates x y =
  let source_count = Array.length x in
  let order = Array.init source_count Fun.id in
  Array.sort (compare_source_position x y) order;
  let unique_x = Array.make source_count 0.
  and unique_y = Array.make source_count 0.
  and unique_source = Array.make source_count 0
  and source_unique = Array.make source_count 0
  and unique_count = ref 0 in
  let position = ref 0 in
  while !position < source_count do
    let first = order.(!position) and last = ref (!position + 1)
    and representative = ref order.(!position) in
    while !last < source_count
        && x.(order.(!last)) = x.(first) && y.(order.(!last)) = y.(first) do
      representative := min !representative order.(!last);
      incr last
    done;
    let unique = !unique_count in
    unique_x.(unique) <- x.(first);
    unique_y.(unique) <- y.(first);
    unique_source.(unique) <- !representative;
    for index = !position to !last - 1 do
      source_unique.(order.(index)) <- unique
    done;
    incr unique_count;
    position := !last
  done;
  Array.sub unique_x 0 !unique_count,
  Array.sub unique_y 0 !unique_count,
  Array.sub unique_source 0 !unique_count,
  source_unique

let shuffle_spatial_blocks ~seed order =
  let random = ref (Rays_math.Rand.seed64 seed) in
  let block = 256 and first = ref 0 in
  while !first < Array.length order do
    let last = min (Array.length order) (!first + block) in
    for index = last - 1 downto !first + 1 do
      let offset,next = Rays_math.Rand.int ~bound:(index - !first + 1) !random in
      random := next;
      let other = !first + offset and value = order.(index) in
      order.(index) <- order.(other);
      order.(other) <- value
    done;
    first := last
  done

let canonical_triangle_into output offset first second third =
  if first <= second && first <= third then begin
    output.(offset) <- first; output.(offset + 1) <- second;
    output.(offset + 2) <- third
  end else if second <= first && second <= third then begin
    output.(offset) <- second; output.(offset + 1) <- third;
    output.(offset + 2) <- first
  end else begin
    output.(offset) <- third; output.(offset + 1) <- first;
    output.(offset + 2) <- second
  end

let build ?cancel ?(seed = 0L) ~x ~y () =
  try
    Cancel.check_opt cancel;
    if Array.length x <> Array.length y then
      invalid_arg "Delaunay x and y arrays must have equal cardinality";
    Array.iteri (fun point value ->
        if not (Float.is_finite value && Float.is_finite y.(point)) then
          invalid_arg "Delaunay coordinates must be finite") x;
    let source_count = Array.length x in
    let unique_x,unique_y,unique_source,source_unique = exact_duplicates x y in
    let point_count = Array.length unique_x in
    if point_count < 3 then Ok {
      source_count;   unique_source; source_unique;
      triangle_points = [||];
    } else begin
      let maximum = ref 0. in
      for point = 0 to point_count - 1 do
        maximum := max !maximum (max (abs_float unique_x.(point))
            (abs_float unique_y.(point)))
      done;
      let _,base_exponent = Float.frexp !maximum in
      let radius_exponent = min 1026 (max 2 (base_exponent + 2)) in
      let super_weight = Float.ldexp 1. (-radius_exponent) in
      let finite_super = radius_exponent <= 1021 in
      let exact_one = Exact_dyadic.of_float 1.
      and exact_weight = Exact_dyadic.of_float super_weight in
      let super_x = [|-4.;4.;0.|] and super_y = [|-2.;-2.;4.|] in
      let exact_super_x = Array.map Exact_dyadic.of_float super_x
      and exact_super_y = Array.map Exact_dyadic.of_float super_y in
      let predicate_x = if finite_super then begin
          let values = Array.make (point_count + 3) 0. in
          Array.blit unique_x 0 values 0 point_count;
          for local = 0 to 2 do
            values.(point_count + local) <-
              Float.ldexp super_x.(local) radius_exponent
          done;
          values
        end else unique_x
      and predicate_y = if finite_super then begin
          let values = Array.make (point_count + 3) 0. in
          Array.blit unique_y 0 values 0 point_count;
          for local = 0 to 2 do
            values.(point_count + local) <-
              Float.ldexp super_y.(local) radius_exponent
          done;
          values
        end else unique_y in
      let homogeneous point =
        if point < point_count then
          Exact_dyadic.of_float unique_x.(point),
          Exact_dyadic.of_float unique_y.(point), exact_one
        else
          let local = point - point_count in
          exact_super_x.(local),exact_super_y.(local),exact_weight in
      let orient first second third =
        if finite_super || first < point_count && second < point_count
            && third < point_count then
          match Predicates.orient2d_packed ~x:predicate_x ~y:predicate_y
              first second third with
          | Predicates.Negative -> -1 | Predicates.Zero -> 0
          | Predicates.Positive -> 1
        else begin
          let ax,ay,aw = homogeneous first
          and bx,by,bw = homogeneous second
          and cx,cy,cw = homogeneous third in
          Exact_dyadic.Scratch.orient2d_homogeneous
            ~ax ~ay ~aw ~bx ~by ~bw ~cx ~cy ~cw
        end in
      let incircle first second third point =
        if finite_super || first < point_count && second < point_count
            && third < point_count && point < point_count then
          match Predicates.incircle_packed ~x:predicate_x ~y:predicate_y
              first second third point with
          | Predicates.Negative -> -1 | Predicates.Zero -> 0
          | Predicates.Positive -> 1
        else begin
          let ax,ay,aw = homogeneous first
          and bx,by,bw = homogeneous second
          and cx,cy,cw = homogeneous third
          and dx,dy,dw = homogeneous point in
          Exact_dyadic.Scratch.incircle_homogeneous
            ~ax ~ay ~aw ~bx ~by ~bw ~cx ~cy ~cw ~dx ~dy ~dw
        end in
      let initial_capacity =
        if point_count > (Sys.max_array_length - 8) / 2 then
          invalid_arg "Delaunay point cardinality exceeds array limits"
        else max 16 ((2 * point_count) + 8) in
      let triangles = make_triangles initial_capacity in
      let initial = allocate_triangle triangles in
      let s0 = point_count and s1 = point_count + 1 and s2 = point_count + 2 in
      if orient s0 s1 s2 <= 0 then
        invalid_arg "Delaunay internal super triangle is not counter-clockwise";
      triangles.a.(initial) <- s0; triangles.b.(initial) <- s1;
      triangles.c.(initial) <- s2; triangles.alive.(initial) <- true;
      let insertion_order = Array.init point_count Fun.id in
      shuffle_spatial_blocks ~seed insertion_order;
      let marks = ref (Array.make (Array.length triangles.a) 0)
      and generation = ref 0 and walk_start = ref initial in
      let ensure_marks () =
        if Array.length !marks < Array.length triangles.a then
          marks := grow_int !marks (Array.length triangles.a) 0 in
      let work = ref (Array.make 64 0)
      and boundary_u = ref (Array.make 64 0)
      and boundary_v = ref (Array.make 64 0)
      and boundary_outside = ref (Array.make 64 (-1))
      and boundary_order = ref (Array.make 64 0)
      and created = ref (Array.make 64 0) in
      let boundary_next = Array.make (point_count + 3) (-1)
      and boundary_stamp = Array.make (point_count + 3) 0
      and boundary_generation = ref 0 in
      let ensure_array storage needed default =
        if needed > Array.length !storage then begin
          let capacity = ref (Array.length !storage * 2) in
          while !capacity < needed do
            if !capacity > Sys.max_array_length / 2 then
              invalid_arg "Delaunay work buffer exceeds array limits";
            capacity := !capacity * 2
          done;
          storage := grow_int !storage !capacity default
        end in
      let next_generation () =
        incr generation;
        if !generation = max_int then begin
          Array.fill !marks 0 (Array.length !marks) 0;
          generation := 1
        end;
        !generation in
      for insertion = 0 to point_count - 1 do
        if insertion land 255 = 0 then Cancel.check_opt cancel;
        let point = insertion_order.(insertion) in
        ensure_marks ();
        let current = ref !walk_start and steps = ref 0 and located = ref false in
        while not !located && !steps <= triangles.count do
          if !current < 0 || !current >= triangles.count
              || not triangles.alive.(!current) then
            invalid_arg "Delaunay point-location walk reached inactive topology";
          let first = triangles.a.(!current) and second = triangles.b.(!current)
          and third = triangles.c.(!current) in
          let ab = orient first second point in
          if ab < 0 then current := triangles.n0.(!current) else begin
            let bc = orient second third point in
            if bc < 0 then current := triangles.n1.(!current) else begin
              let ca = orient third first point in
              if ca < 0 then current := triangles.n2.(!current)
              else located := true
            end
          end;
          incr steps
        done;
        if not !located then
          invalid_arg "Delaunay point-location walk did not converge";
        let visited_generation = next_generation () in
        let work_count = ref 1 and cursor = ref 0 in
        (!work).(0) <- !current;
        (!marks).(!current) <- visited_generation;
        while !cursor < !work_count do
          let triangle = (!work).(!cursor) in
          incr cursor;
          for edge = 0 to 2 do
            let adjacent = neighbor triangles triangle edge in
            if adjacent >= 0 && (!marks).(adjacent) <> visited_generation then begin
              (!marks).(adjacent) <- visited_generation;
              if incircle triangles.a.(adjacent) triangles.b.(adjacent)
                  triangles.c.(adjacent) point >= 0 then begin
                ensure_array work (!work_count + 1) 0;
                (!work).(!work_count) <- adjacent;
                incr work_count
              end
            end
          done
        done;
        if incircle triangles.a.(!current) triangles.b.(!current)
            triangles.c.(!current) point < 0 then
          invalid_arg "Delaunay cavity is empty for a located point";
        let cavity_generation = next_generation () in
        for index = 0 to !work_count - 1 do
          (!marks).((!work).(index)) <- cavity_generation
        done;
        let boundary_count = ref 0 in
        for index = 0 to !work_count - 1 do
          let triangle = (!work).(index) in
          for edge = 0 to 2 do
            let adjacent = neighbor triangles triangle edge in
            if adjacent < 0 || (!marks).(adjacent) <> cavity_generation then begin
              ensure_array boundary_u (!boundary_count + 1) 0;
              ensure_array boundary_v (!boundary_count + 1) 0;
              ensure_array boundary_outside (!boundary_count + 1) (-1);
              let first = edge_first triangles triangle edge
              and second = edge_second triangles triangle edge in
              (!boundary_u).(!boundary_count) <- first;
              (!boundary_v).(!boundary_count) <- second;
              (!boundary_outside).(!boundary_count) <- adjacent;
              incr boundary_count
            end
          done
        done;
        if !boundary_count < 3 then
          invalid_arg "Delaunay cavity has an invalid boundary";
        incr boundary_generation;
        if !boundary_generation = max_int then begin
          Array.fill boundary_stamp 0 (Array.length boundary_stamp) 0;
          boundary_generation := 1
        end;
        for boundary = 0 to !boundary_count - 1 do
          let first = (!boundary_u).(boundary) in
          if boundary_stamp.(first) = !boundary_generation then
            invalid_arg (Printf.sprintf
                "Delaunay cavity boundary repeats start %d while inserting point %d"
                first point);
          boundary_stamp.(first) <- !boundary_generation;
          boundary_next.(first) <- boundary
        done;
        ensure_array boundary_order !boundary_count 0;
        let start = ref 0 in
        for boundary = 1 to !boundary_count - 1 do
          if (!boundary_u).(boundary) < (!boundary_u).(!start)
              || ((!boundary_u).(boundary) = (!boundary_u).(!start)
                  && (!boundary_v).(boundary) < (!boundary_v).(!start)) then
            start := boundary
        done;
        let boundary = ref !start in
        for index = 0 to !boundary_count - 1 do
          if !boundary < 0 then
            invalid_arg "Delaunay cavity boundary cycle is open";
          (!boundary_order).(index) <- !boundary;
          let next_start = (!boundary_v).(!boundary) in
          boundary := if boundary_stamp.(next_start) = !boundary_generation
            then boundary_next.(next_start) else -1
        done;
        if !boundary <> !start then
          invalid_arg "Delaunay cavity boundary contains multiple cycles";
        for index = 0 to !work_count - 1 do
          release_triangle triangles (!work).(index)
        done;
        ensure_array created !boundary_count 0;
        for index = 0 to !boundary_count - 1 do
          let boundary = (!boundary_order).(index) in
          let triangle = allocate_triangle triangles in
          let first = (!boundary_u).(boundary)
          and second = (!boundary_v).(boundary)
          and outside = (!boundary_outside).(boundary) in
          if orient first second point <= 0 then
            invalid_arg "Delaunay cavity emitted a non-positive triangle";
          triangles.a.(triangle) <- first;
          triangles.b.(triangle) <- second;
          triangles.c.(triangle) <- point;
          triangles.n0.(triangle) <- outside;
          triangles.alive.(triangle) <- true;
          if outside >= 0 then
            replace_neighbor_edge triangles outside first second triangle;
          (!created).(index) <- triangle
        done;
        for index = 0 to !boundary_count - 1 do
          let triangle = (!created).(index) in
          triangles.n1.(triangle) <- (!created).((index + 1) mod !boundary_count);
          triangles.n2.(triangle) <- (!created).((index + !boundary_count - 1)
              mod !boundary_count)
        done;
        walk_start := (!created).(0)
      done;
      let output_count = ref 0 in
      for triangle = 0 to triangles.count - 1 do
        if triangles.alive.(triangle)
            && triangles.a.(triangle) < point_count
            && triangles.b.(triangle) < point_count
            && triangles.c.(triangle) < point_count then incr output_count
      done;
      let unsorted = Array.make (!output_count * 3) 0 and output = ref 0 in
      for triangle = 0 to triangles.count - 1 do
        if triangles.alive.(triangle)
            && triangles.a.(triangle) < point_count
            && triangles.b.(triangle) < point_count
            && triangles.c.(triangle) < point_count then begin
          let first = unique_source.(triangles.a.(triangle))
          and second = unique_source.(triangles.b.(triangle))
          and third = unique_source.(triangles.c.(triangle)) in
          canonical_triangle_into unsorted (!output * 3) first second third;
          incr output
        end
      done;
      let order = Array.init !output_count Fun.id in
      Array.sort (fun left right ->
          let left_at = left * 3 and right_at = right * 3 in
          let comparison = Int.compare unsorted.(left_at) unsorted.(right_at) in
          if comparison <> 0 then comparison else
          let comparison = Int.compare unsorted.(left_at + 1)
              unsorted.(right_at + 1) in
          if comparison <> 0 then comparison
          else Int.compare unsorted.(left_at + 2) unsorted.(right_at + 2)) order;
      let triangle_points = Array.make (!output_count * 3) 0 in
      Array.iteri (fun triangle source ->
          let target = triangle * 3 and source = source * 3 in
          triangle_points.(target) <- unsorted.(source);
          triangle_points.(target + 1) <- unsorted.(source + 1);
          triangle_points.(target + 2) <- unsorted.(source + 2)) order;
      Ok { source_count;   unique_source; source_unique;
        triangle_points }
    end
  with
  | Cancel.Cancelled -> Error "Delaunay triangulation was cancelled"
  | Invalid_argument message -> Error message
