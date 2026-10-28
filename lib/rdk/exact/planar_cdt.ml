type t = {
  triangle_points : int array;
  constraint_points : int array;
}

type result_t = t

let edge_key ~point_count ~key_stride first second =
  let first,second = if first < second then first,second else second,first in
  if first < 0 || second < 0 || second >= point_count then
    invalid_arg "Planar CDT edge endpoint is out of range";
  if first > (max_int - second) / key_stride then
    invalid_arg "Planar CDT edge key exceeds integer range";
  (first * key_stride) + second

module Edge_table = struct
  let empty = -1
  let tombstone = -2

  type t = {
    mutable keys : int array;
    mutable first_triangles : int array;
    mutable first_opposites : int array;
    mutable second_triangles : int array;
    mutable second_opposites : int array;
    mutable constrained : bytes;
    mutable edge_count : int;
    mutable tombstone_count : int;
  }

  let capacity_for expected =
    if expected < 0 || expected > Sys.max_array_length / 2 then
      invalid_arg "Planar CDT edge cardinality exceeds array limits";
    let needed = max 16 (expected * 2) and capacity = ref 16 in
    while !capacity < needed do
      if !capacity > Sys.max_array_length / 2 then
        invalid_arg "Planar CDT edge table exceeds array limits";
      capacity := !capacity * 2
    done;
    !capacity

  let create expected =
    let capacity = capacity_for expected in
    { keys = Array.make capacity empty;
      first_triangles = Array.make capacity (-1);
      first_opposites = Array.make capacity (-1);
      second_triangles = Array.make capacity (-1);
      second_opposites = Array.make capacity (-1);
      constrained = Bytes.make capacity '\000';
      edge_count = 0; tombstone_count = 0 }

  let reset table =
    Array.fill table.keys 0 (Array.length table.keys) empty;
    table.edge_count <- 0;
    table.tombstone_count <- 0

  let clear_constraints table =
    Bytes.fill table.constrained 0 (Bytes.length table.constrained) '\000'

  let[@inline always] hash key =
    let value = key lxor (key lsr 16) in
    (value * 0x45d9f3b) land max_int

  let find_slot table key =
    let mask = Array.length table.keys - 1 in
    let slot = ref (hash key land mask) and first_tombstone = ref (-1)
    and found = ref (-1) and searching = ref true in
    while !searching do
      let present = table.keys.(!slot) in
      if present = key then begin found := !slot; searching := false end
      else if present = empty then begin
        found := if !first_tombstone >= 0 then !first_tombstone else !slot;
        searching := false
      end else begin
        if present = tombstone && !first_tombstone < 0 then
          first_tombstone := !slot;
        slot := (!slot + 1) land mask
      end
    done;
    !found

  let find table key =
    let slot = find_slot table key in
    if table.keys.(slot) = key then slot else -1

  let add_raw table key triangle opposite is_constrained =
    let slot = find_slot table key in
    if table.keys.(slot) <> key then begin
      if table.keys.(slot) = tombstone then
        table.tombstone_count <- table.tombstone_count - 1;
      table.keys.(slot) <- key;
      table.first_triangles.(slot) <- triangle;
      table.first_opposites.(slot) <- opposite;
      table.second_triangles.(slot) <- -1;
      table.second_opposites.(slot) <- -1;
      Bytes.unsafe_set table.constrained slot
        (if is_constrained then '\001' else '\000');
      table.edge_count <- table.edge_count + 1
    end else begin
      if is_constrained then Bytes.unsafe_set table.constrained slot '\001';
      let first = table.first_triangles.(slot)
      and second = table.second_triangles.(slot) in
      if triangle = first || triangle = second then
        invalid_arg "Planar CDT duplicated one triangle edge incidence"
      else if second >= 0 then
        invalid_arg "Planar CDT input contains a non-manifold edge"
      else if triangle < first then begin
        table.second_triangles.(slot) <- first;
        table.second_opposites.(slot) <- table.first_opposites.(slot);
        table.first_triangles.(slot) <- triangle;
        table.first_opposites.(slot) <- opposite
      end else begin
        table.second_triangles.(slot) <- triangle;
        table.second_opposites.(slot) <- opposite
      end
    end

  let rehash table capacity =
    let keys = table.keys and first_triangles = table.first_triangles
    and first_opposites = table.first_opposites
    and second_triangles = table.second_triangles
    and second_opposites = table.second_opposites
    and constrained = table.constrained in
    table.keys <- Array.make capacity empty;
    table.first_triangles <- Array.make capacity (-1);
    table.first_opposites <- Array.make capacity (-1);
    table.second_triangles <- Array.make capacity (-1);
    table.second_opposites <- Array.make capacity (-1);
    table.constrained <- Bytes.make capacity '\000';
    table.edge_count <- 0; table.tombstone_count <- 0;
    for slot = 0 to Array.length keys - 1 do
      if keys.(slot) >= 0 then begin
        let fixed = Bytes.unsafe_get constrained slot <> '\000' in
        add_raw table keys.(slot) first_triangles.(slot)
          first_opposites.(slot) fixed;
        if second_triangles.(slot) >= 0 then
          add_raw table keys.(slot) second_triangles.(slot)
            second_opposites.(slot) fixed
      end
    done

  let prepare_insert table =
    let capacity = Array.length table.keys in
    let used = table.edge_count + table.tombstone_count + 1 in
    if used >= capacity - (capacity / 3) then begin
      if capacity > Sys.max_array_length / 2 then
        invalid_arg "Planar CDT edge table exceeds array limits";
      rehash table (capacity * 2)
    end else if table.tombstone_count > table.edge_count
        && table.tombstone_count > 64 then rehash table capacity

  let add table key triangle opposite =
    prepare_insert table;
    add_raw table key triangle opposite false

  let remove table key triangle =
    let slot = find table key in
    if slot < 0 then invalid_arg "Planar CDT lost an indexed edge";
    if table.first_triangles.(slot) = triangle then begin
      if table.second_triangles.(slot) >= 0 then begin
        table.first_triangles.(slot) <- table.second_triangles.(slot);
        table.first_opposites.(slot) <- table.second_opposites.(slot);
        table.second_triangles.(slot) <- -1;
        table.second_opposites.(slot) <- -1
      end else begin
        table.keys.(slot) <- tombstone;
        table.first_triangles.(slot) <- -1;
        table.first_opposites.(slot) <- -1;
        Bytes.unsafe_set table.constrained slot '\000';
        table.edge_count <- table.edge_count - 1;
        table.tombstone_count <- table.tombstone_count + 1
      end
    end else if table.second_triangles.(slot) = triangle then begin
      table.second_triangles.(slot) <- -1;
      table.second_opposites.(slot) <- -1
    end else invalid_arg "Planar CDT lost one edge incidence"

  let constrain table key =
    let slot = find table key in
    if slot < 0 then invalid_arg "Planar CDT cannot mark a missing edge";
    Bytes.unsafe_set table.constrained slot '\001'

  let unconstrain table key =
    let slot = find table key in
    if slot >= 0 then Bytes.unsafe_set table.constrained slot '\000';
    slot
end

type workspace = {
  mutable triangle_a : int array;
  mutable triangle_b : int array;
  mutable triangle_c : int array;
  mutable point_triangle : int array;
  mutable key_stride : int;
  mutable point_count : int;
  mutable triangle_count : int;
  mutable last_output : int array option;
  mutable constraints : int array;
  mutable valid : bool;
  mutable full_build_count : int;
  mutable incremental_build_count : int;
  edge_table : Edge_table.t;
}

let checked_edge_capacity triangle_capacity =
  if triangle_capacity < 0 || triangle_capacity > (max_int - 3) / 2 then
    invalid_arg "Planar CDT triangle cardinality exceeds edge-table limits";
  max 1 ((triangle_capacity * 2) + 3)

let create_workspace ?point_capacity ~triangle_capacity () =
  if triangle_capacity < 0 || triangle_capacity > Sys.max_array_length then
    invalid_arg "Planar CDT workspace triangle capacity exceeds array limits";
  let point_capacity = Option.value point_capacity
      ~default:(max 1 (triangle_capacity + 2)) in
  if point_capacity <= 0 || point_capacity > Sys.max_array_length then
    invalid_arg "Planar CDT workspace point capacity exceeds array limits";
  let capacity = max 1 triangle_capacity in
  { triangle_a = Array.make capacity 0;
    triangle_b = Array.make capacity 0;
    triangle_c = Array.make capacity 0;
    point_triangle = Array.make point_capacity (-1);
    key_stride = point_capacity;
    point_count = 0;
    triangle_count = 0;
    last_output = None;
    constraints = [||];
    valid = false;
    full_build_count = 0;
    incremental_build_count = 0;
    edge_table = Edge_table.create (checked_edge_capacity capacity) }

let ensure_workspace workspace required =
  if required > Array.length workspace.triangle_a then begin
    let capacity = ref (Array.length workspace.triangle_a) in
    while !capacity < required do
      if !capacity > Sys.max_array_length / 2 then capacity := required
      else capacity := min Sys.max_array_length (!capacity * 2)
    done;
    let grow source =
      let output = Array.make !capacity 0 in
      Array.blit source 0 output 0 (Array.length source);
      output in
    workspace.triangle_a <- grow workspace.triangle_a;
    workspace.triangle_b <- grow workspace.triangle_b;
    workspace.triangle_c <- grow workspace.triangle_c
  end

let prepare_workspace_points workspace required =
  if required > workspace.key_stride then begin
    let capacity = ref workspace.key_stride in
    while !capacity < required do
      if !capacity > Sys.max_array_length / 2 then capacity := required
      else capacity := min Sys.max_array_length (!capacity * 2)
    done;
    workspace.point_triangle <- Array.make !capacity (-1);
    workspace.key_stride <- !capacity
  end

module View = struct
  type view = {
    triangle_points : int array;
    constraint_points : int array;
  }

  type nonrec workspace = workspace

  let view (value : result_t) = {
    triangle_points = value.triangle_points;
    constraint_points = value.constraint_points;
  }

  let create_workspace = create_workspace
end

module Key_heap = struct
  type t = { mutable values : int array; mutable count : int }
  let create capacity = { values = Array.make (max 16 capacity) 0; count = 0 }
  let push heap key =
    if heap.count = Array.length heap.values then begin
      if heap.count > Sys.max_array_length / 2 then
        invalid_arg "Planar CDT edge queue exceeds array limits";
      let values = Array.make (heap.count * 2) 0 in
      Array.blit heap.values 0 values 0 heap.count; heap.values <- values
    end;
    let slot = ref heap.count in heap.count <- heap.count + 1;
    while !slot > 0 && heap.values.((!slot - 1) / 2) > key do
      let parent = (!slot - 1) / 2 in
      heap.values.(!slot) <- heap.values.(parent); slot := parent
    done;
    heap.values.(!slot) <- key
  let pop heap =
    if heap.count = 0 then None else begin
      let result = heap.values.(0) in heap.count <- heap.count - 1;
      if heap.count > 0 then begin
        let last = heap.values.(heap.count) and slot = ref 0
        and moving = ref true in
        while !moving do
          let left = (!slot * 2) + 1 in
          if left >= heap.count then moving := false else begin
            let right = left + 1 in
            let child = if right < heap.count
                && heap.values.(right) < heap.values.(left) then right else left in
            if heap.values.(child) >= last then moving := false
            else begin heap.values.(!slot) <- heap.values.(child); slot := child end
          end
        done;
        heap.values.(!slot) <- last
      end;
      Some result
    end
end

let opposite_sign left right = match left,right with
  | Predicates.Negative,Predicates.Positive
  | Predicates.Positive,Predicates.Negative -> true
  | _ -> false

let squared_limit count =
  if count <= 0 then 0 else if count > max_int / count then max_int else count * count

let canonicalize ~count triangle_a triangle_b triangle_c =
  let packed = Array.make (count * 3) 0 in
  let write triangle a b c =
    let at = triangle * 3 in
    if a <= b && a <= c then begin packed.(at) <- a; packed.(at+1) <- b; packed.(at+2) <- c end
    else if b <= c then begin packed.(at) <- b; packed.(at+1) <- c; packed.(at+2) <- a end
    else begin packed.(at) <- c; packed.(at+1) <- a; packed.(at+2) <- b end in
  for triangle = 0 to count - 1 do
    write triangle triangle_a.(triangle) triangle_b.(triangle) triangle_c.(triangle)
  done;
  let order = Array.init count Fun.id in
  Array.sort (fun left right ->
      let l = left * 3 and r = right * 3 in
      let compared = Int.compare packed.(l) packed.(r) in
      if compared <> 0 then compared else
      let compared = Int.compare packed.(l+1) packed.(r+1) in
      if compared <> 0 then compared else Int.compare packed.(l+2) packed.(r+2)) order;
  for target = 0 to count - 1 do
    if order.(target) >= 0 then begin
      let at = target * 3 in
      let saved_a = packed.(at) and saved_b = packed.(at + 1)
      and saved_c = packed.(at + 2) in
      let current = ref target and complete = ref false in
      while not !complete do
        let source = order.(!current) in
        order.(!current) <- -1;
        if source = target then begin
          let destination = !current * 3 in
          packed.(destination) <- saved_a;
          packed.(destination + 1) <- saved_b;
          packed.(destination + 2) <- saved_c;
          complete := true
        end else begin
          let destination = !current * 3 and source_at = source * 3 in
          packed.(destination) <- packed.(source_at);
          packed.(destination + 1) <- packed.(source_at + 1);
          packed.(destination + 2) <- packed.(source_at + 2);
          current := source
        end
      done
    end
  done;
  packed

let canonical_constraints ~point_count ~key_stride constraints winding =
  if Array.length constraints mod 2 <> 0 then
    invalid_arg "Planar CDT constraint endpoint array must contain pairs";
  let count = Array.length constraints / 2 in
  let winding = match winding with
    | None -> Array.make count 0
    | Some values when Array.length values = count -> values
    | Some _ -> invalid_arg
        "Planar CDT constraint winding cardinality must match constraint count" in
  let pairs = Array.make count 0 and weights = Array.make count 0 in
  for constraint_index = 0 to count - 1 do
    let first = constraints.(constraint_index * 2)
    and second = constraints.((constraint_index * 2) + 1) in
    if first = second then invalid_arg "Planar CDT constraint has equal endpoints";
    pairs.(constraint_index) <- edge_key ~point_count ~key_stride first second;
    let weight = winding.(constraint_index) in
    weights.(constraint_index) <- if first < second then weight
      else if weight = min_int then
        invalid_arg "Planar CDT constraint winding exceeds integer range"
      else -weight
  done;
  let order = Array.init count Fun.id in
  Array.sort (fun left right -> Int.compare pairs.(left) pairs.(right)) order;
  let unique_pairs = Array.make count 0 and unique_weights = Array.make count 0
  and unique = ref 0 in
  for index = 0 to count - 1 do
    let source = order.(index) in
    if index = 0 || pairs.(source) <> pairs.(order.(index - 1)) then begin
      unique_pairs.(!unique) <- pairs.(source);
      unique_weights.(!unique) <- weights.(source);
      incr unique
    end else begin
      let previous = unique_weights.(!unique - 1) and value = weights.(source) in
      if (value > 0 && previous > max_int - value)
          || (value < 0 && previous < min_int - value) then
        invalid_arg "Planar CDT constraint winding exceeds integer range";
      unique_weights.(!unique - 1) <- previous + value
    end
  done;
  let output = Array.make (!unique * 2) 0 in
  for index = 0 to !unique - 1 do
    output.(index * 2) <- unique_pairs.(index) / key_stride;
    output.((index * 2) + 1) <- unique_pairs.(index) mod key_stride
  done;
  output,Array.sub unique_weights 0 !unique

let constraint_delta previous current =
  let previous_count = Array.length previous / 2
  and current_count = Array.length current / 2 in
  let compare previous_index current_index =
    let compared = Int.compare previous.(previous_index * 2)
        current.(current_index * 2) in
    if compared <> 0 then compared else
      Int.compare previous.((previous_index * 2) + 1)
        current.((current_index * 2) + 1) in
  let removed_count = ref 0 and added_count = ref 0
  and previous_index = ref 0 and current_index = ref 0 in
  while !previous_index < previous_count || !current_index < current_count do
    if !previous_index = previous_count then begin
      incr added_count; incr current_index
    end else if !current_index = current_count then begin
      incr removed_count; incr previous_index
    end else begin
      let compared = compare !previous_index !current_index in
      if compared = 0 then begin incr previous_index; incr current_index end
      else if compared < 0 then begin incr removed_count; incr previous_index end
      else begin incr added_count; incr current_index end
    end
  done;
  let removed = Array.make (!removed_count * 2) 0
  and added = Array.make (!added_count * 2) 0 in
  previous_index := 0; current_index := 0;
  let removed_index = ref 0 and added_index = ref 0 in
  let copy source source_index output output_index =
    output.(output_index * 2) <- source.(source_index * 2);
    output.((output_index * 2) + 1) <- source.((source_index * 2) + 1) in
  while !previous_index < previous_count || !current_index < current_count do
    if !previous_index = previous_count then begin
      copy current !current_index added !added_index;
      incr added_index; incr current_index
    end else if !current_index = current_count then begin
      copy previous !previous_index removed !removed_index;
      incr removed_index; incr previous_index
    end else begin
      let compared = compare !previous_index !current_index in
      if compared = 0 then begin incr previous_index; incr current_index end
      else if compared < 0 then begin
        copy previous !previous_index removed !removed_index;
        incr removed_index; incr previous_index
      end else begin
        copy current !current_index added !added_index;
        incr added_index; incr current_index
      end
    end
  done;
  removed,added

let build ?cancel ?workspace ~point_count ~orient ~incircle
    ?(bounds_overlap = fun _ _ _ _ -> true) ~triangle_points
    ?(insert_points = [||]) ?(flood_from_hull_boundary = false)
    ~constraint_points ?constraint_winding
    ?(remove_outside_constraint_polygons = false) () =
  try
    Cancel.check_opt cancel;
    if point_count <= 0 then invalid_arg "Planar CDT point count must be positive";
    if Array.length triangle_points mod 3 <> 0 then
      invalid_arg "Planar CDT triangle array must contain triples";
    let initial_triangle_count = Array.length triangle_points / 3 in
    if Array.length insert_points > (Sys.max_array_length - initial_triangle_count) / 2
    then invalid_arg "Planar CDT inserted-point cardinality exceeds array limits";
    let incremental = match workspace with
      | Some workspace when workspace.valid
          && not flood_from_hull_boundary
          && not remove_outside_constraint_polygons
          && workspace.key_stride >= point_count
          && workspace.point_count + Array.length insert_points = point_count
          && workspace.triangle_count = initial_triangle_count
          && (match workspace.last_output with
              | Some previous -> previous == triangle_points | None -> false) ->
          let contiguous = ref true in
          for index = 0 to Array.length insert_points - 1 do
            if insert_points.(index) <> workspace.point_count + index then
              contiguous := false
          done;
          !contiguous
      | Some _ | None -> false in
    let triangle_capacity = initial_triangle_count + (2 * Array.length insert_points) in
    let triangle_count = ref (match workspace with
      | Some workspace when incremental -> workspace.triangle_count
      | Some _ | None -> initial_triangle_count) in
    let triangle_a,triangle_b,triangle_c = match workspace with
      | None -> Array.make triangle_capacity 0,Array.make triangle_capacity 0,
          Array.make triangle_capacity 0
      | Some workspace ->
          if not incremental then prepare_workspace_points workspace point_count;
          ensure_workspace workspace triangle_capacity;
          workspace.triangle_a,workspace.triangle_b,workspace.triangle_c in
    let key_stride = match workspace with
      | None -> point_count | Some workspace -> workspace.key_stride in
    let edge_key = edge_key ~point_count ~key_stride in
    Option.iter (fun workspace ->
      workspace.valid <- false;
      workspace.last_output <- None) workspace;
    if not incremental then begin
      let referenced = Bytes.make point_count '\000' in
      for triangle = 0 to initial_triangle_count - 1 do
        let a = triangle_points.(triangle * 3)
        and b = triangle_points.((triangle * 3) + 1)
        and c = triangle_points.((triangle * 3) + 2) in
        if a < 0 || a >= point_count || b < 0 || b >= point_count
            || c < 0 || c >= point_count then
          invalid_arg "Planar CDT triangle point is out of range";
        (match orient a b c with
         | Predicates.Positive ->
             triangle_a.(triangle) <- a; triangle_b.(triangle) <- b;
             triangle_c.(triangle) <- c
         | Predicates.Negative ->
             triangle_a.(triangle) <- a; triangle_b.(triangle) <- c;
             triangle_c.(triangle) <- b
         | Predicates.Zero ->
             invalid_arg "Planar CDT input contains a degenerate triangle");
        Bytes.unsafe_set referenced a '\001';
        Bytes.unsafe_set referenced b '\001';
        Bytes.unsafe_set referenced c '\001'
      done;
      Array.iter (fun point ->
        if point < 0 || point >= point_count then
          invalid_arg "Planar CDT inserted point is out of range";
        if Bytes.unsafe_get referenced point <> '\000' then
          invalid_arg "Planar CDT inserted point is duplicated or already referenced";
        Bytes.unsafe_set referenced point '\001') insert_points
    end else begin
      for triangle = 0 to initial_triangle_count - 1 do
        let a = triangle_a.(triangle) and b = triangle_b.(triangle)
        and c = triangle_c.(triangle) in
        match orient a b c with
        | Predicates.Positive -> ()
        | Predicates.Negative ->
            triangle_b.(triangle) <- c;
            triangle_c.(triangle) <- b
        | Predicates.Zero ->
            invalid_arg "Planar CDT input contains a degenerate triangle"
      done
    end;
    let constraints,constraint_winding = canonical_constraints ~point_count
        ~key_stride constraint_points constraint_winding in
    let removed_constraints,added_constraints = match workspace with
      | Some workspace when incremental ->
          constraint_delta workspace.constraints constraints
      | Some _ | None -> [||],[||] in
    let constraint_index key =
      let first = ref 0 and last = ref (Array.length constraints / 2) in
      while !first < !last do
        let middle = !first + ((!last - !first) / 2) in
        let present = edge_key constraints.(middle * 2)
            constraints.((middle * 2) + 1) in
        if present < key then first := middle + 1 else last := middle
      done;
      if !first < Array.length constraints / 2
          && edge_key constraints.(!first * 2)
            constraints.((!first * 2) + 1) = key then !first else -1 in
    let constraint_key key = constraint_index key >= 0 in
    let constraint_weight key =
      let index = constraint_index key in
      if index < 0 then 0 else constraint_winding.(index) in
    let edge_capacity = checked_edge_capacity triangle_capacity in
    let table = match workspace with
      | None -> Edge_table.create edge_capacity
      | Some workspace when incremental -> workspace.edge_table
      | Some workspace ->
          Edge_table.reset workspace.edge_table;
          workspace.edge_table in
    let has_constraints = Array.length constraints > 0 in
    let add_edge key triangle opposite =
      Edge_table.add table key triangle opposite;
      if incremental && has_constraints && constraint_key key then
        Edge_table.constrain table key in
    let add_triangle triangle =
      let a = triangle_a.(triangle) and b = triangle_b.(triangle)
      and c = triangle_c.(triangle) in
      add_edge (edge_key a b) triangle c;
      add_edge (edge_key b c) triangle a;
      add_edge (edge_key c a) triangle b in
    let remove_triangle triangle =
      let a = triangle_a.(triangle) and b = triangle_b.(triangle)
      and c = triangle_c.(triangle) in
      Edge_table.remove table (edge_key a b) triangle;
      Edge_table.remove table (edge_key b c) triangle;
      Edge_table.remove table (edge_key c a) triangle in
    if not incremental then
      for triangle = 0 to initial_triangle_count - 1 do add_triangle triangle done;
    let point_triangle = match workspace with
      | None -> Array.make point_count (-1)
      | Some workspace ->
          if incremental then
            Array.fill workspace.point_triangle workspace.point_count
              (point_count - workspace.point_count) (-1)
          else Array.fill workspace.point_triangle 0 point_count (-1);
          workspace.point_triangle in
    let seed_triangle triangle =
      point_triangle.(triangle_a.(triangle)) <- triangle;
      point_triangle.(triangle_b.(triangle)) <- triangle;
      point_triangle.(triangle_c.(triangle)) <- triangle in
    if not incremental then
      for triangle = 0 to initial_triangle_count - 1 do seed_triangle triangle done;
    if incremental then
      for constraint_index = 0 to Array.length removed_constraints / 2 - 1 do
        let key = edge_key removed_constraints.(constraint_index * 2)
            removed_constraints.((constraint_index * 2) + 1) in
        ignore (Edge_table.unconstrain table key)
      done;
    let set_triangle triangle a b c =
      match orient a b c with
      | Predicates.Positive ->
          triangle_a.(triangle) <- a; triangle_b.(triangle) <- b;
          triangle_c.(triangle) <- c
      | Predicates.Negative ->
          triangle_a.(triangle) <- a; triangle_b.(triangle) <- c;
          triangle_c.(triangle) <- b
      | Predicates.Zero -> invalid_arg "Planar CDT flip produced a degenerate triangle" in
    let flip left right left_opposite right_opposite edge_a edge_b =
      remove_triangle left; remove_triangle right;
      set_triangle left left_opposite right_opposite edge_a;
      set_triangle right right_opposite left_opposite edge_b;
      add_triangle left; add_triangle right; seed_triangle left; seed_triangle right in
    let append_triangle a b c =
      if !triangle_count >= triangle_capacity then
        invalid_arg "Planar CDT inserted-point triangle bound was exceeded";
      let triangle = !triangle_count in
      incr triangle_count;
      set_triangle triangle a b c;
      triangle in
    let adjacent_across triangle first second =
      let slot = Edge_table.find table (edge_key first second) in
      if slot < 0 then -1 else
      let left = table.Edge_table.first_triangles.(slot)
      and right = table.Edge_table.second_triangles.(slot) in
      if left = triangle then right else if right = triangle then left else -1 in
    let walk_start = ref 0 in
    let locate point =
      if !triangle_count = 0 then
        invalid_arg "Planar CDT cannot insert a point into empty topology";
      let current = ref (min !walk_start (!triangle_count - 1))
      and steps = ref 0 and result = ref None in
      while Option.is_none !result && !steps <= !triangle_count do
        let triangle = !current in
        if triangle < 0 || triangle >= !triangle_count then steps := !triangle_count + 1
        else begin
          let a = triangle_a.(triangle) and b = triangle_b.(triangle)
          and c = triangle_c.(triangle) in
          let ab = orient a b point in
          if ab = Predicates.Negative then current := adjacent_across triangle a b
          else begin
            let bc = orient b c point in
            if bc = Predicates.Negative then current := adjacent_across triangle b c
            else begin
              let ca = orient c a point in
              if ca = Predicates.Negative then current := adjacent_across triangle c a
              else begin
                let edge = ref None in
                let choose first second opposite sign =
                  if sign = Predicates.Zero then
                    let key = edge_key first second in
                    match !edge with
                    | None -> edge := Some (key,first,second,opposite)
                    | Some (present,_,_,_) when key < present ->
                        edge := Some (key,first,second,opposite)
                    | Some _ -> () in
                choose a b c ab; choose b c a bc; choose c a b ca;
                result := Some (triangle,!edge)
              end
            end
          end
        end;
        incr steps
      done;
      match !result with
      | Some value -> value
      | None ->
          (* Audited compatibility oracle for a stale walk seed or a point
             exactly on a high-valence fan. *)
          let triangle = ref 0 and found = ref None in
          while Option.is_none !found && !triangle < !triangle_count do
            let a = triangle_a.(!triangle) and b = triangle_b.(!triangle)
            and c = triangle_c.(!triangle) in
            let ab = orient a b point and bc = orient b c point
            and ca = orient c a point in
            if ab <> Predicates.Negative && bc <> Predicates.Negative
                && ca <> Predicates.Negative then begin
              let edge = ref None in
              let choose first second opposite sign =
                if sign = Predicates.Zero then
                  let key = edge_key first second in
                  match !edge with
                  | None -> edge := Some (key,first,second,opposite)
                  | Some (present,_,_,_) when key < present ->
                      edge := Some (key,first,second,opposite)
                  | Some _ -> () in
              choose a b c ab; choose b c a bc; choose c a b ca;
              found := Some (!triangle,!edge)
            end;
            incr triangle
          done;
          match !found with Some value -> value | None ->
            invalid_arg "Planar CDT inserted point lies outside seed triangulation" in
    let insertion_queue = Key_heap.create 32 in
    let legalize_inserted point triangles =
      insertion_queue.Key_heap.count <- 0;
      let enqueue_opposite triangle =
        let a = triangle_a.(triangle) and b = triangle_b.(triangle)
        and c = triangle_c.(triangle) in
        if a = point then Key_heap.push insertion_queue (edge_key b c)
        else if b = point then Key_heap.push insertion_queue (edge_key c a)
        else if c = point then Key_heap.push insertion_queue (edge_key a b) in
      Array.iter enqueue_opposite triangles;
      let flips = ref 0 and running = ref true in
      while !running do
        match Key_heap.pop insertion_queue with
        | None -> running := false
        | Some key ->
            let slot = Edge_table.find table key in
            if slot >= 0 && table.Edge_table.second_triangles.(slot) >= 0 then begin
              let first = table.Edge_table.first_triangles.(slot)
              and second = table.Edge_table.second_triangles.(slot)
              and first_opposite = table.Edge_table.first_opposites.(slot)
              and second_opposite = table.Edge_table.second_opposites.(slot)
              and edge_a = key / key_stride and edge_b = key mod key_stride in
              if first_opposite = point || second_opposite = point then begin
                let inserted,opposite,left,right = if first_opposite = point then
                    first_opposite,second_opposite,first,second
                  else second_opposite,first_opposite,second,first in
                let circle = incircle edge_a edge_b inserted opposite
                and orientation = orient edge_a edge_b inserted in
                let illegal = match orientation,circle with
                  | Predicates.Positive,Predicates.Positive
                  | Predicates.Negative,Predicates.Negative -> true
                  | _,Predicates.Zero -> edge_key inserted opposite < key
                  | _ -> false in
                if illegal && opposite_sign
                    (orient inserted opposite edge_a)
                    (orient inserted opposite edge_b) then begin
                  incr flips;
                  if !flips > squared_limit !triangle_count then
                    invalid_arg
                      "Planar CDT inserted-point legalization did not converge";
                  flip left right inserted opposite edge_a edge_b;
                  enqueue_opposite left;
                  enqueue_opposite right
                end
              end
            end
      done in
    Array.iteri (fun insertion point ->
      Cancel.poll cancel insertion 255;
      let containing,edge = locate point in
      let inserted_triangles = match edge with
       | None ->
           let a = triangle_a.(containing) and b = triangle_b.(containing)
           and c = triangle_c.(containing) in
           remove_triangle containing;
           set_triangle containing a b point;
           let second = append_triangle b c point
           and third = append_triangle c a point in
           add_triangle containing; add_triangle second; add_triangle third;
           seed_triangle containing; seed_triangle second; seed_triangle third;
           [|containing;second;third|]
       | Some (_,u,v,opposite) ->
           let slot = Edge_table.find table (edge_key u v) in
           if slot < 0 then invalid_arg "Planar CDT lost an inserted-point split edge";
           let left = table.Edge_table.first_triangles.(slot)
           and right = table.Edge_table.second_triangles.(slot) in
           let adjacent = if left = containing then right else left in
           let adjacent_opposite = if adjacent < 0 then -1
             else if left = adjacent then table.Edge_table.first_opposites.(slot)
             else table.Edge_table.second_opposites.(slot) in
           remove_triangle containing;
           if adjacent >= 0 then remove_triangle adjacent;
           set_triangle containing u point opposite;
           let containing_second = append_triangle point v opposite in
           add_triangle containing; add_triangle containing_second;
           seed_triangle containing; seed_triangle containing_second;
           if adjacent >= 0 then begin
             set_triangle adjacent v point adjacent_opposite;
             let adjacent_second = append_triangle point u adjacent_opposite in
             add_triangle adjacent; add_triangle adjacent_second;
             seed_triangle adjacent; seed_triangle adjacent_second;
             [|containing;containing_second;adjacent;adjacent_second|]
           end else [|containing;containing_second|]
      in
      legalize_inserted point inserted_triangles;
      walk_start := containing) insert_points;
    let properly_crosses a b u v =
      u <> a && u <> b && v <> a && v <> b && bounds_overlap a b u v
      && opposite_sign (orient a b u) (orient a b v)
      && opposite_sign (orient u v a) (orient u v b) in
    let triangle_has triangle point = triangle_a.(triangle) = point
        || triangle_b.(triangle) = point || triangle_c.(triangle) = point in
    let crossing_key a b triangle entry =
      let found = ref (-1) in
      let consider u v =
        let key = edge_key u v in
        if key <> entry && properly_crosses a b u v
            && (!found < 0 || key < !found) then found := key in
      consider triangle_a.(triangle) triangle_b.(triangle);
      consider triangle_b.(triangle) triangle_c.(triangle);
      consider triangle_c.(triangle) triangle_a.(triangle); !found in
    let adjacent triangle u v =
      let slot = Edge_table.find table (edge_key u v) in
      if slot < 0 then -1 else
      let first = table.Edge_table.first_triangles.(slot)
      and second = table.Edge_table.second_triangles.(slot) in
      if first = triangle then second else if second = triangle then first else -1 in
    let other triangle u v =
      let a = triangle_a.(triangle) and b = triangle_b.(triangle)
      and c = triangle_c.(triangle) in
      if a <> u && a <> v then a else if b <> u && b <> v then b else c in
    let incident_start a b =
      let seed = point_triangle.(a) in
      if seed < 0 || not (triangle_has seed a) then -1
      else if crossing_key a b seed (-1) >= 0 then seed
      else begin
        let ta = triangle_a.(seed) and tb = triangle_b.(seed)
        and tc = triangle_c.(seed) in
        let first,second = if ta = a then tb,tc else if tb = a then tc,ta else ta,tb in
        let search entry_vertex =
          let current = ref (adjacent seed a entry_vertex)
          and entry = ref entry_vertex and steps = ref 0 and found = ref (-1) in
          while !found < 0 && !current >= 0 && !current <> seed
              && !steps <= !triangle_count do
            if crossing_key a b !current (-1) >= 0 then found := !current
            else begin
              let exit = other !current a !entry in
              let next = adjacent !current a exit in
              entry := exit; current := next
            end;
            incr steps
          done; !found in
        let found = search first in if found >= 0 then found else search second
      end in
    let strip_marks = Bytes.make triangle_capacity '\000'
    and boundary_stamps = Array.make point_count 0
    and boundary_edges = Hashtbl.create 16
    and boundary_generation = ref 0 in
    let recover_constraint a b =
      let strip = Array.make !triangle_count 0 and strip_count = ref 0
      and current = ref (incident_start a b) and entry_key = ref (-1)
      and steps = ref 0 and complete = ref false in
      while not !complete && !current >= 0 && !steps <= !triangle_count do
        if Bytes.unsafe_get strip_marks !current <> '\000' then
          invalid_arg "Planar CDT constraint trace entered a cycle";
        Bytes.unsafe_set strip_marks !current '\001';
        strip.(!strip_count) <- !current; incr strip_count;
        if triangle_has !current b then complete := true
        else begin
          let key = crossing_key a b !current !entry_key in
          let slot = if key < 0 then -1 else Edge_table.find table key in
          if slot < 0 || table.Edge_table.second_triangles.(slot) < 0 then
            current := -1
          else if constraint_key key then
            invalid_arg "Planar CDT constraints cross; crossing splitting is required"
          else begin
            let left = table.Edge_table.first_triangles.(slot)
            and right = table.Edge_table.second_triangles.(slot) in
            let next = if left = !current then right
              else if right = !current then left else -1 in
            entry_key := key; current := next
          end
        end;
        incr steps
      done;
      if not !complete then
        invalid_arg "Planar CDT constraint trace failed; an open constraint segment contains an unsplit point";
      let boundary_u = Array.make ((!strip_count * 3) + 1) 0
      and boundary_v = Array.make ((!strip_count * 3) + 1) 0
      and boundary_triangle = Array.make ((!strip_count * 3) + 1) 0
      and boundary_count = ref 0 in
      let consider triangle first second =
        let slot = Edge_table.find table (edge_key first second) in
        if slot < 0 then invalid_arg "Planar CDT strip lost a boundary edge";
        let left = table.Edge_table.first_triangles.(slot)
        and right = table.Edge_table.second_triangles.(slot) in
        let outside = if left = triangle then right
          else if right = triangle then left else
            invalid_arg "Planar CDT strip edge incidence is inconsistent" in
        if outside < 0 || Bytes.unsafe_get strip_marks outside = '\000' then begin
          boundary_u.(!boundary_count) <- first;
          boundary_v.(!boundary_count) <- second;
          boundary_triangle.(!boundary_count) <- triangle;
          incr boundary_count
        end in
      for index = 0 to !strip_count - 1 do
        let triangle = strip.(index) in
        consider triangle triangle_a.(triangle) triangle_b.(triangle);
        consider triangle triangle_b.(triangle) triangle_c.(triangle);
        consider triangle triangle_c.(triangle) triangle_a.(triangle)
      done;
      incr boundary_generation;
      if !boundary_generation = max_int then begin
        Array.fill boundary_stamps 0 point_count 0; boundary_generation := 1
      end;
      for edge = 0 to !boundary_count - 1 do
        boundary_stamps.(boundary_u.(edge)) <- !boundary_generation;
        boundary_stamps.(boundary_v.(edge)) <- !boundary_generation
      done;
      if boundary_stamps.(a) <> !boundary_generation
          || boundary_stamps.(b) <> !boundary_generation then
        invalid_arg "Planar CDT constraint endpoints are absent from the strip boundary";
      let directed_key first second = (first * point_count) + second in
      Hashtbl.clear boundary_edges;
      for edge = 0 to !boundary_count - 1 do
        Hashtbl.replace boundary_edges
          (directed_key boundary_u.(edge) boundary_v.(edge)) edge
      done;
      let next_boundary edge =
        let current_triangle = ref boundary_triangle.(edge)
        and first = ref boundary_u.(edge) and second = boundary_v.(edge)
        and next = ref (-1) and steps = ref 0 in
        while !next < 0 && !steps <= !strip_count do
          let ta = triangle_a.(!current_triangle)
          and tb = triangle_b.(!current_triangle)
          and tc = triangle_c.(!current_triangle) in
          let third = if ta = !first && tb = second then tc
            else if tb = !first && tc = second then ta
            else if tc = !first && ta = second then tb
            else invalid_arg "Planar CDT boundary walk lost its directed triangle edge" in
          let slot = Edge_table.find table (edge_key second third) in
          if slot < 0 then invalid_arg "Planar CDT boundary walk lost an edge";
          let left = table.Edge_table.first_triangles.(slot)
          and right = table.Edge_table.second_triangles.(slot) in
          let adjacent = if left = !current_triangle then right
            else if right = !current_triangle then left else
              invalid_arg "Planar CDT boundary walk found inconsistent incidence" in
          if adjacent < 0 || Bytes.unsafe_get strip_marks adjacent = '\000' then
            next := (match Hashtbl.find_opt boundary_edges
                (directed_key second third) with
              | Some value -> value
              | None -> invalid_arg "Planar CDT boundary walk did not find its next edge")
          else begin
            current_triangle := adjacent;
            first := third
          end;
          incr steps
        done;
        if !next < 0 then invalid_arg "Planar CDT boundary walk did not terminate";
        !next in
      let visited_boundary = Bytes.make !boundary_count '\000'
      and cycles = ref [] in
      for start = 0 to !boundary_count - 1 do
        if Bytes.unsafe_get visited_boundary start = '\000' then begin
          let values = Array.make !boundary_count 0 and count = ref 0
          and current = ref start in
          while Bytes.unsafe_get visited_boundary !current = '\000' do
            Bytes.unsafe_set visited_boundary !current '\001';
            values.(!count) <- boundary_u.(!current); incr count;
            current := next_boundary !current
          done;
          if !current <> start then
            invalid_arg "Planar CDT strip boundary cycles merge";
          cycles := Array.sub values 0 !count :: !cycles
        end
      done;
      let cycles = List.rev !cycles in
      let contains point cycle = Array.exists (( = ) point) cycle in
      let main_cycles = List.filter (fun cycle -> contains a cycle && contains b cycle)
          cycles in
      let initial_main_cycle = match main_cycles with
        | [cycle] -> cycle
        | [] -> invalid_arg "Planar CDT constraint strip did not reach its endpoint"
        | _ -> invalid_arg "Planar CDT constraint belongs to multiple strip cycles" in
      let point_index point cycle =
        let found = ref (-1) in
        Array.iteri (fun index value -> if value = point && !found < 0 then
          found := index) cycle;
        !found in
      let bridge_cycle outer hole =
        let target = Bytes.make point_count '\000' in
        Array.iter (fun point -> Bytes.unsafe_set target point '\001') hole;
        let parent = Array.make point_count (-2)
        and queue = Array.make point_count 0 and first = ref 0 and last = ref 0 in
        Array.iter (fun point ->
          if parent.(point) = -2 then begin
            parent.(point) <- -1; queue.(!last) <- point; incr last
          end) outer;
        let reached = ref (-1) in
        while !reached < 0 && !first < !last do
          let point = queue.(!first) in incr first;
          if Bytes.unsafe_get target point <> '\000' then reached := point
          else begin
            let consider neighbor =
              if parent.(neighbor) = -2
                  && not (properly_crosses a b point neighbor) then begin
                parent.(neighbor) <- point;
                queue.(!last) <- neighbor; incr last
              end in
            for index = 0 to !strip_count - 1 do
              let triangle = strip.(index) in
              let ta = triangle_a.(triangle) and tb = triangle_b.(triangle)
              and tc = triangle_c.(triangle) in
              if ta = point then begin consider tb; consider tc end
              else if tb = point then begin consider ta; consider tc end
              else if tc = point then begin consider ta; consider tb end
            done
          end
        done;
        if !reached < 0 then
          invalid_arg "Planar CDT could not bridge a constraint-strip hole";
        let reverse_path = Array.make point_count 0 and count = ref 0
        and point = ref !reached in
        while !point >= 0 do
          reverse_path.(!count) <- !point; incr count;
          point := parent.(!point)
        done;
        let path = Array.init !count (fun index -> reverse_path.(!count - 1 - index)) in
        Array.iter (fun point -> boundary_stamps.(point) <- !boundary_generation) path;
        let outer_index = point_index path.(0) outer
        and hole_index = point_index path.(Array.length path - 1) hole in
        if outer_index < 0 || hole_index < 0 then
          invalid_arg "Planar CDT hole bridge endpoints are not on their cycles";
        let outer_count = Array.length outer and hole_count = Array.length hole
        and path_count = Array.length path in
        let output_count = outer_count + hole_count + (2 * (path_count - 1)) in
        let output = Array.make output_count 0 and cursor = ref 0 in
        let append point = output.(!cursor) <- point; incr cursor in
        append outer.(outer_index);
        for index = 1 to path_count - 1 do append path.(index) done;
        for offset = 1 to hole_count - 1 do
          append hole.((hole_index + offset) mod hole_count)
        done;
        append hole.(hole_index);
        for index = path_count - 2 downto 0 do append path.(index) done;
        for offset = 1 to outer_count - 1 do
          append outer.((outer_index + offset) mod outer_count)
        done;
        if !cursor <> output_count then
          invalid_arg "Planar CDT hole bridge cardinality is inconsistent";
        output in
      let holes = List.filter (fun cycle -> cycle != initial_main_cycle) cycles in
      let main_cycle = List.fold_left bridge_cycle initial_main_cycle holes in
      let cycles = [main_cycle] in
      let emitted = Array.make (!strip_count * 3) 0 and emitted_count = ref 0 in
      let write_emitted triangle a b c =
        let at = triangle * 3 in
        match orient a b c with
        | Predicates.Positive ->
            emitted.(at) <- a; emitted.(at + 1) <- b; emitted.(at + 2) <- c
        | Predicates.Negative ->
            emitted.(at) <- a; emitted.(at + 1) <- c; emitted.(at + 2) <- b
        | Predicates.Zero ->
            invalid_arg "Planar CDT constraint cavity emitted a degenerate triangle" in
      let emit a b c =
        if !emitted_count >= !strip_count then
          invalid_arg "Planar CDT constraint cavity exceeded its triangle budget";
        write_emitted !emitted_count a b c;
        incr emitted_count in
      (* Not Polygon_triangulation.find_ear: exact orientation predicates and the lowest-point ear, not the first. *)
      let ear_clip path =
        let size = Array.length path in
        if size >= 3 then begin
          let remaining = Array.init size Fun.id and active = ref size in
          while !active > 3 do
            Cancel.check_opt cancel;
            let best_slot = ref (-1) and best_point = ref max_int in
            for slot = 0 to !active - 1 do
              let previous = remaining.((slot + !active - 1) mod !active)
              and current = remaining.(slot)
              and next = remaining.((slot + 1) mod !active) in
              let pa = path.(previous) and pb = path.(current)
              and pc = path.(next) in
              if orient pa pb pc = Predicates.Positive then begin
                let blocked = ref false and other = ref 0 in
                while not !blocked && !other < !active do
                  let candidate = remaining.(!other) in
                  if candidate <> previous && candidate <> current
                      && candidate <> next then begin
                    let p = path.(candidate) in
                    blocked := p <> pa && p <> pb && p <> pc
                        && orient pa pb p <> Predicates.Negative
                        && orient pb pc p <> Predicates.Negative
                        && orient pc pa p <> Predicates.Negative
                  end;
                  incr other
                done;
                if not !blocked && pb < !best_point then begin
                  best_point := pb; best_slot := slot
                end
              end
            done;
            if !best_slot < 0 then
              invalid_arg (Printf.sprintf
                "Planar CDT constraint cavity is non-simple or degenerate (constraint=%d,%d path=%s)"
                a b (String.concat ":" (Array.to_list path
                  |> List.map string_of_int)));
            let previous = remaining.((!best_slot + !active - 1) mod !active)
            and current = remaining.(!best_slot)
            and next = remaining.((!best_slot + 1) mod !active) in
            emit path.(previous) path.(current) path.(next);
            Array.blit remaining (!best_slot + 1) remaining !best_slot
              (!active - !best_slot - 1);
            decr active
          done;
          emit path.(remaining.(0)) path.(remaining.(1)) path.(remaining.(2))
        end in
      let cyclic_path cycle first last =
        let size = Array.length cycle in
        let count = ((last - first + size) mod size) + 1 in
        Array.init count (fun index -> cycle.((first + index) mod size)) in
      List.iter (fun cycle ->
        if cycle == main_cycle then begin
          let a_index = point_index a cycle and b_index = point_index b cycle in
          if a_index < 0 || b_index < 0 then
            invalid_arg "Planar CDT constraint endpoints are absent from main strip cycle";
          ear_clip (cyclic_path cycle a_index b_index);
          ear_clip (cyclic_path cycle b_index a_index)
        end else ear_clip cycle) cycles;
      let interior = Array.make (!strip_count * 3) 0
      and interior_count = ref 0
      and interior_seen = Bytes.make point_count '\000' in
      for index = 0 to !strip_count - 1 do
        let triangle = strip.(index) in
        let consider point =
          if boundary_stamps.(point) <> !boundary_generation
              && Bytes.unsafe_get interior_seen point = '\000' then begin
            Bytes.unsafe_set interior_seen point '\001';
            interior.(!interior_count) <- point;
            incr interior_count
          end in
        consider triangle_a.(triangle);
        consider triangle_b.(triangle);
        consider triangle_c.(triangle)
      done;
      let interior = if !interior_count = Array.length interior then interior
        else begin
          let values = Array.sub interior 0 !interior_count in
          values
        end in
      Array.sort Int.compare interior;
      let triangle_has_edge triangle u v =
        let at = triangle * 3 in
        let a = emitted.(at) and b = emitted.(at + 1)
        and c = emitted.(at + 2) in
        (a = u || b = u || c = u) && (a = v || b = v || c = v) in
      let split_triangle_edge triangle u v point =
        let at = triangle * 3 in
        let a = emitted.(at) and b = emitted.(at + 1)
        and c = emitted.(at + 2) in
        let opposite = if a <> u && a <> v then a
          else if b <> u && b <> v then b else c in
        write_emitted triangle u point opposite;
        emit point v opposite in
      Array.iter (fun point ->
        Cancel.check_opt cancel;
        let containing = ref (-1) and edge_u = ref (-1) and edge_v = ref (-1)
        and triangle = ref 0 in
        while !containing < 0 && !triangle < !emitted_count do
          let at = !triangle * 3 in
          let a = emitted.(at) and b = emitted.(at + 1)
          and c = emitted.(at + 2) in
          let ab = orient a b point and bc = orient b c point
          and ca = orient c a point in
          if ab <> Predicates.Negative && bc <> Predicates.Negative
              && ca <> Predicates.Negative then begin
            containing := !triangle;
            if ab = Predicates.Zero then begin edge_u := a; edge_v := b end
            else if bc = Predicates.Zero then begin edge_u := b; edge_v := c end
            else if ca = Predicates.Zero then begin edge_u := c; edge_v := a end
          end;
          incr triangle
        done;
        if !containing < 0 then
          invalid_arg "Planar CDT could not reinsert a constraint-cavity point";
        if !edge_u < 0 then begin
          let at = !containing * 3 in
          let a = emitted.(at) and b = emitted.(at + 1)
          and c = emitted.(at + 2) in
          write_emitted !containing a b point;
          emit b c point; emit c a point
        end else begin
          let adjacent = ref (-1) in
          for candidate = 0 to !emitted_count - 1 do
            if candidate <> !containing
                && triangle_has_edge candidate !edge_u !edge_v then
              adjacent := candidate
          done;
          if !adjacent < 0 then
            invalid_arg "Planar CDT cavity point lies on its unsplit boundary";
          let first = !containing and second = !adjacent in
          split_triangle_edge first !edge_u !edge_v point;
          split_triangle_edge second !edge_u !edge_v point
        end) interior;
      if !emitted_count <> !strip_count then
        invalid_arg (Printf.sprintf
            "Planar CDT constraint cavity changed triangle cardinality (%d strip triangles, %d boundary edges, %d cycles, %d interior points, %d emitted)"
            !strip_count !boundary_count (List.length cycles) !interior_count
            !emitted_count);
      for index = 0 to !strip_count - 1 do remove_triangle strip.(index) done;
      for index = 0 to !strip_count - 1 do
        let at = index * 3 and triangle = strip.(index) in
        set_triangle triangle emitted.(at) emitted.(at + 1) emitted.(at + 2)
      done;
      for index = 0 to !strip_count - 1 do
        add_triangle strip.(index); seed_triangle strip.(index);
        Bytes.unsafe_set strip_marks strip.(index) '\000'
      done in
    let recover_constraints values =
      for constraint_index = 0 to Array.length values / 2 - 1 do
        Cancel.poll cancel constraint_index 255;
        let a = values.(constraint_index * 2)
        and b = values.((constraint_index * 2) + 1) in
        let key = edge_key a b in
        if Edge_table.find table key < 0 then recover_constraint a b;
        if Edge_table.find table key < 0 then
          invalid_arg
            "Planar CDT cavity retriangulation did not recover its constraint";
        Edge_table.constrain table key
      done in
    if incremental then recover_constraints added_constraints
    else begin
      Edge_table.clear_constraints table;
      recover_constraints constraints
    end;
    let queue = Key_heap.create table.Edge_table.edge_count in
    for slot = 0 to Array.length table.Edge_table.keys - 1 do
      if table.Edge_table.keys.(slot) >= 0
          && table.Edge_table.second_triangles.(slot) >= 0
          && Bytes.unsafe_get table.Edge_table.constrained slot = '\000' then
        Key_heap.push queue table.Edge_table.keys.(slot)
    done;
    let flips = ref 0 and continue = ref true in
    while !continue do match Key_heap.pop queue with
      | None -> continue := false
      | Some key ->
          Cancel.poll cancel !flips 255;
          let slot = Edge_table.find table key in
          if slot >= 0 && table.Edge_table.second_triangles.(slot) >= 0
              && Bytes.unsafe_get table.Edge_table.constrained slot = '\000' then begin
            let left = table.Edge_table.first_triangles.(slot)
            and right = table.Edge_table.second_triangles.(slot)
            and lo = table.Edge_table.first_opposites.(slot)
            and ro = table.Edge_table.second_opposites.(slot)
            and ea = key / key_stride and eb = key mod key_stride in
            let circle = incircle ea eb lo ro and orientation = orient ea eb lo in
            let illegal = match orientation,circle with
              | Predicates.Positive,Predicates.Positive
              | Predicates.Negative,Predicates.Negative -> true
              | _,Predicates.Zero -> edge_key lo ro < key
              | _ -> false in
            if illegal && opposite_sign (orient lo ro ea) (orient lo ro eb) then begin
              incr flips;
              if !flips > squared_limit !triangle_count then
                invalid_arg "Planar CDT Delaunay repair did not converge";
              flip left right lo ro ea eb;
              let enqueue triangle =
                let a = triangle_a.(triangle) and b = triangle_b.(triangle)
                and c = triangle_c.(triangle) in
                Key_heap.push queue (edge_key a b);
                Key_heap.push queue (edge_key b c);
                Key_heap.push queue (edge_key c a) in
              enqueue left; enqueue right
            end
          end
    done;
    let triangle_a,triangle_b,triangle_c,output_triangle_count,constraints =
      if not flood_from_hull_boundary
          && not remove_outside_constraint_polygons then
        triangle_a,triangle_b,triangle_c,!triangle_count,constraints
      else begin
        let outside = if flood_from_hull_boundary then
            Bytes.make !triangle_count '\000' else Bytes.empty
        and queue = if flood_from_hull_boundary then
            Array.make !triangle_count 0 else [||]
        and read = ref 0 and write = ref 0 in
        let mark triangle =
          if triangle >= 0 && Bytes.unsafe_get outside triangle = '\000' then begin
            Bytes.unsafe_set outside triangle '\001';
            queue.(!write) <- triangle; incr write
          end in
        if flood_from_hull_boundary then begin
          for slot = 0 to Array.length table.Edge_table.keys - 1 do
            if table.Edge_table.keys.(slot) >= 0
                && table.Edge_table.second_triangles.(slot) < 0
                && Bytes.unsafe_get table.Edge_table.constrained slot = '\000' then
              mark table.Edge_table.first_triangles.(slot)
          done;
          while !read < !write do
            Cancel.poll cancel !read 4095;
            let triangle = queue.(!read) in incr read;
            let visit a b =
              let slot = Edge_table.find table (edge_key a b) in
              if slot < 0 then invalid_arg "Planar CDT outside flood lost an edge";
              if Bytes.unsafe_get table.Edge_table.constrained slot = '\000' then begin
                let first = table.Edge_table.first_triangles.(slot)
                and second = table.Edge_table.second_triangles.(slot) in
                mark (if first = triangle then second else first)
              end in
            let a = triangle_a.(triangle) and b = triangle_b.(triangle)
            and c = triangle_c.(triangle) in
            visit a b; visit b c; visit c a
          done
        end;
        let winding = if remove_outside_constraint_polygons then
            Array.make !triangle_count 0 else [||]
        and winding_known = if remove_outside_constraint_polygons then
            Bytes.make !triangle_count '\000' else Bytes.empty
        and winding_queue = if remove_outside_constraint_polygons then
            Array.make !triangle_count 0 else [||]
        and winding_read = ref 0 and winding_write = ref 0 in
        let checked_add left right =
          if (right > 0 && left > max_int - right)
              || (right < 0 && left < min_int - right) then
            invalid_arg "Planar CDT polygon winding exceeds integer range";
          left + right in
        let checked_negate value =
          if value = min_int then
            invalid_arg "Planar CDT polygon winding exceeds integer range";
          -value in
        let assign_winding triangle value =
          if Bytes.unsafe_get winding_known triangle = '\000' then begin
            Bytes.unsafe_set winding_known triangle '\001';
            winding.(triangle) <- value;
            winding_queue.(!winding_write) <- triangle;
            incr winding_write
          end else if winding.(triangle) <> value then
            invalid_arg
              "Planar CDT polygon constraints have inconsistent winding"
        in
        if remove_outside_constraint_polygons then begin
          for slot = 0 to Array.length table.Edge_table.keys - 1 do
            let key = table.Edge_table.keys.(slot) in
            if key >= 0 && table.Edge_table.second_triangles.(slot) < 0 then begin
              let triangle = table.Edge_table.first_triangles.(slot)
              and opposite = table.Edge_table.first_opposites.(slot)
              and u = key / key_stride and v = key mod key_stride
              and weight = constraint_weight key in
              let value = if weight = 0 then 0 else
                match orient u v opposite with
                | Predicates.Positive -> weight
                | Predicates.Negative -> checked_negate weight
                | Predicates.Zero -> invalid_arg
                    "Planar CDT hull contains a degenerate edge" in
              assign_winding triangle value
            end
          done;
          while !winding_read < !winding_write do
            Cancel.poll cancel !winding_read 4095;
            let triangle = winding_queue.(!winding_read) in
            incr winding_read;
            let visit u v opposite =
              let key = edge_key u v in
              let slot = Edge_table.find table key in
              if slot < 0 then
                invalid_arg "Planar CDT winding propagation lost an edge";
              let first = table.Edge_table.first_triangles.(slot)
              and second = table.Edge_table.second_triangles.(slot) in
              let neighbor = if first = triangle then second else first in
              if neighbor >= 0 then begin
                let canonical_u = key / key_stride
                and canonical_v = key mod key_stride
                and weight = constraint_weight key in
                let delta = if weight = 0 then 0 else
                  match orient canonical_u canonical_v opposite with
                  | Predicates.Positive -> checked_negate weight
                  | Predicates.Negative -> weight
                  | Predicates.Zero -> invalid_arg
                      "Planar CDT triangle contains a degenerate edge" in
                assign_winding neighbor (checked_add winding.(triangle) delta)
              end in
            let a = triangle_a.(triangle) and b = triangle_b.(triangle)
            and c = triangle_c.(triangle) in
            visit a b c; visit b c a; visit c a b
          done;
          for triangle = 0 to !triangle_count - 1 do
            if Bytes.unsafe_get winding_known triangle = '\000' then
              invalid_arg "Planar CDT could not classify polygon winding"
          done
        end;
        let removed = Bytes.make !triangle_count '\000' and removed_count = ref 0 in
        for triangle = 0 to !triangle_count - 1 do
          if (flood_from_hull_boundary
                && Bytes.unsafe_get outside triangle <> '\000')
              || (remove_outside_constraint_polygons
                && winding.(triangle) = 0) then begin
            Bytes.unsafe_set removed triangle '\001'; incr removed_count
          end
        done;
        let kept = !triangle_count - !removed_count in
        let output_a = Array.make kept 0 and output_b = Array.make kept 0
        and output_c = Array.make kept 0 and at = ref 0 in
        for triangle = 0 to !triangle_count - 1 do
          if Bytes.unsafe_get removed triangle = '\000' then begin
            output_a.(!at) <- triangle_a.(triangle);
            output_b.(!at) <- triangle_b.(triangle);
            output_c.(!at) <- triangle_c.(triangle);
            incr at
          end
        done;
        let kept_constraints = Array.make (Array.length constraints) 0
        and kept_constraint_count = ref 0 in
        for constraint_index = 0 to Array.length constraints / 2 - 1 do
          let a = constraints.(constraint_index * 2)
          and b = constraints.((constraint_index * 2) + 1) in
          let slot = Edge_table.find table (edge_key a b) in
          if slot < 0 then
            invalid_arg "Planar CDT outside flood lost a constrained edge";
          let first = table.Edge_table.first_triangles.(slot)
          and second = table.Edge_table.second_triangles.(slot) in
          if (first >= 0 && Bytes.unsafe_get removed first = '\000')
              || (second >= 0 && Bytes.unsafe_get removed second = '\000') then begin
            let target = !kept_constraint_count * 2 in
            kept_constraints.(target) <- a; kept_constraints.(target + 1) <- b;
            incr kept_constraint_count
          end
        done;
        output_a,output_b,output_c,kept,
        Array.sub kept_constraints 0 (!kept_constraint_count * 2)
      end in
    let triangle_points = canonicalize ~count:output_triangle_count
        triangle_a triangle_b triangle_c in
    let result = { triangle_points; constraint_points = constraints } in
    Option.iter (fun workspace ->
      if not flood_from_hull_boundary
          && not remove_outside_constraint_polygons then begin
        workspace.point_count <- point_count;
        workspace.triangle_count <- output_triangle_count;
        workspace.last_output <- Some triangle_points;
        workspace.constraints <- constraints;
        workspace.valid <- true
      end;
      if incremental then
        workspace.incremental_build_count <- workspace.incremental_build_count + 1
      else workspace.full_build_count <- workspace.full_build_count + 1) workspace;
    Ok result
  with
  | Cancel.Cancelled -> Error "Planar CDT was cancelled"
  | Invalid_argument message -> Error message
