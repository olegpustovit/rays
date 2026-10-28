
type t = {
  constraints : Boolean_constraints.t;
  vertices : Implicit_point.t array;
  facet_a : int array;
  facet_b : int array;
  facet_c : int array;
  member_offsets : int array;
  member_sides : bytes;
  member_faces : int array;
  member_triangles : int array;
  member_windings : bytes;
  edge_first : int array;
  edge_second : int array;
  edge_offsets : int array;
  edge_facets : int array;
  edge_locals : bytes;
}

let operation = "boolean_complex"
let error code message = Error (Error.make ~operation ~code message)

let vertex_count value = Array.length value.vertices
let approximate_vertex value vertex = Implicit_point.approximate value.vertices.(vertex)
let facet_count value = Array.length value.facet_a
let facet_vertex value facet local = match local with
  | 0 -> value.facet_a.(facet)
  | 1 -> value.facet_b.(facet)
  | 2 -> value.facet_c.(facet)
  | _ -> invalid_arg "Boolean complex facet corner must be 0, 1, or 2"
let facet_member_range value facet =
  value.member_offsets.(facet), value.member_offsets.(facet + 1)
let member_side value member =
  if Bytes.unsafe_get value.member_sides member = '\000' then Boolean_constraints.Left else Boolean_constraints.Right
let member_face value member = value.member_faces.(member)
let member_triangle value member = value.member_triangles.(member)
let member_winding value member =
  if Bytes.unsafe_get value.member_windings member = '\000' then 1 else -1
let edge_count value = Array.length value.edge_first
let edge_first value edge = value.edge_first.(edge)
let edge_second value edge = value.edge_second.(edge)
let edge_incident_range value edge = value.edge_offsets.(edge), value.edge_offsets.(edge + 1)
let edge_incident_facet value incident = value.edge_facets.(incident)
let edge_incident_local value incident = Char.code (Bytes.unsafe_get value.edge_locals incident)

module View = struct
  let constraints value = value.constraints
  let vertex value vertex = value.vertices.(vertex)
end

let compare_points points left right =
  let comparison = Implicit_point.compare_x points.(left) points.(right) in
  if comparison <> 0 then comparison else
  let comparison = Implicit_point.compare_y points.(left) points.(right) in
  if comparison <> 0 then comparison else
  let comparison = Implicit_point.compare_z points.(left) points.(right) in
  if comparison <> 0 then comparison else Int.compare left right

let sorted3 a b c =
  if a <= b then
    if b <= c then a,b,c else if a <= c then a,c,b else c,a,b
  else if a <= c then b,a,c else if b <= c then b,c,a else c,b,a

let ordered_pair first second =
  if first <= second then first,second else second,first

let compare2 (a,b) (x,y) =
  let comparison = Int.compare a x in
  if comparison <> 0 then comparison else Int.compare b y

let compare3 (a,b,c) (x,y,z) =
  let comparison = Int.compare a x in
  if comparison <> 0 then comparison else
  let comparison = Int.compare b y in
  if comparison <> 0 then comparison else Int.compare c z

let ordered_triple first second third =
  if compare3 first second <= 0 then
    if compare3 second third <= 0 then first,second,third
    else if compare3 first third <= 0 then first,third,second
    else third,first,second
  else if compare3 first third <= 0 then second,first,third
  else if compare3 second third <= 0 then second,third,first
  else third,second,first

module Construction_table = struct
  type t = {
    mutable kinds : bytes;
    mutable keys : int array;
    mutable tokens : int array;
    mutable count : int;
  }

  let width = 9

  let create expected =
    let maximum_capacity = Sys.max_array_length / width in
    let needed = max 16 (if expected > maximum_capacity / 2 then
        maximum_capacity else expected * 2) in
    let capacity = ref 16 in
    while !capacity < needed do
      if !capacity > maximum_capacity / 2 then
        invalid_arg "Boolean complex construction table exceeds array limits";
      capacity := !capacity * 2
    done;
    if !capacity > maximum_capacity then
      invalid_arg "Boolean complex construction keys exceed array limits";
    { kinds = Bytes.make !capacity '\000';
      keys = Array.make (!capacity * width) (-1);
      tokens = Array.make !capacity (-1); count = 0 }

  let[@inline always] hash kind a b c d e f g h i =
    let mix value key = (Rays_math.Hash.mix value key) land max_int in
    mix (mix (mix (mix (mix (mix (mix (mix (mix kind a) b) c) d) e) f) g) h) i

  let same table slot kind a b c d e f g h i =
    if Bytes.unsafe_get table.kinds slot <> Char.chr kind then false
    else
      let at = slot * width in
      table.keys.(at) = a && table.keys.(at + 1) = b
      && table.keys.(at + 2) = c && table.keys.(at + 3) = d
      && table.keys.(at + 4) = e && table.keys.(at + 5) = f
      && table.keys.(at + 6) = g && table.keys.(at + 7) = h
      && table.keys.(at + 8) = i

  let find_slot table kind a b c d e f g h i =
    let mask = Bytes.length table.kinds - 1 in
    let slot = ref (hash kind a b c d e f g h i land mask) in
    while Bytes.unsafe_get table.kinds !slot <> '\000'
        && not (same table !slot kind a b c d e f g h i) do
      slot := (!slot + 1) land mask
    done;
    !slot

  let add_raw table kind a b c d e f g h i token =
    let slot = find_slot table kind a b c d e f g h i in
    if Bytes.unsafe_get table.kinds slot <> '\000' then
      invalid_arg "Boolean complex duplicated a construction key";
    Bytes.unsafe_set table.kinds slot (Char.chr kind);
    let at = slot * width in
    table.keys.(at) <- a; table.keys.(at + 1) <- b;
    table.keys.(at + 2) <- c; table.keys.(at + 3) <- d;
    table.keys.(at + 4) <- e; table.keys.(at + 5) <- f;
    table.keys.(at + 6) <- g; table.keys.(at + 7) <- h;
    table.keys.(at + 8) <- i; table.tokens.(slot) <- token;
    table.count <- table.count + 1

  let grow table =
    let previous_kinds = table.kinds and previous_keys = table.keys
    and previous_tokens = table.tokens in
    let previous_capacity = Bytes.length previous_kinds in
    if previous_capacity > Sys.max_array_length / 2
        || previous_capacity * 2 > Sys.max_array_length / width then
      invalid_arg "Boolean complex construction table exceeds array limits";
    let capacity = previous_capacity * 2 in
    table.kinds <- Bytes.make capacity '\000';
    table.keys <- Array.make (capacity * width) (-1);
    table.tokens <- Array.make capacity (-1);
    table.count <- 0;
    for slot = 0 to previous_capacity - 1 do
      let kind = Char.code (Bytes.unsafe_get previous_kinds slot) in
      if kind <> 0 then begin
        let at = slot * width in
        add_raw table kind previous_keys.(at) previous_keys.(at + 1)
          previous_keys.(at + 2) previous_keys.(at + 3)
          previous_keys.(at + 4) previous_keys.(at + 5)
          previous_keys.(at + 6) previous_keys.(at + 7)
          previous_keys.(at + 8) previous_tokens.(slot)
      end
    done

  let find table kind a b c d e f g h i =
    let slot = find_slot table kind a b c d e f g h i in
    if Bytes.unsafe_get table.kinds slot = '\000' then -1 else table.tokens.(slot)

  let add table kind a b c d e f g h i token =
    let capacity = Bytes.length table.kinds in
    if table.count + 1 >= capacity - (capacity / 3) then grow table;
    add_raw table kind a b c d e f g h i token
end

let parallel_sort_indices ?cancel compare values =
  let count = Array.length values and run_size = 2_048 in
  if count < run_size * 2 then Array.sort compare values
  else begin
    let source = ref (Array.copy values)
    and destination = ref (Array.make count values.(0)) in
    let runs = (count + run_size - 1) / run_size in
    Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(runs - 1) (fun run ->
      Cancel.check_opt cancel;
      let first = run * run_size
      and length = min run_size (count - (run * run_size)) in
      let local = Array.sub !source first length in
      Array.sort compare local;
      Array.blit local 0 !source first length);
    let width = ref run_size in
    while !width < count do
      let span = !width * 2 in
      let merges = (count + span - 1) / span in
      Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(merges - 1) (fun merge ->
        Cancel.check_opt cancel;
        let first = merge * span
        and middle = min count ((merge * span) + !width)
        and last = min count ((merge + 1) * span) in
        let left = ref first and right = ref middle and output = ref first in
        while !left < middle && !right < last do
          if compare (!source).(!left) (!source).(!right) <= 0 then begin
            (!destination).(!output) <- (!source).(!left);
            incr left
          end else begin
            (!destination).(!output) <- (!source).(!right);
            incr right
          end;
          incr output
        done;
        if !left < middle then
          Array.blit !source !left !destination !output (middle - !left)
        else if !right < last then
          Array.blit !source !right !destination !output (last - !right));
      let swap = !source in
      source := !destination;
      destination := swap;
      if !width > count / 2 then width := count else width := !width * 2
    done;
    Array.blit !source 0 values 0 count
  end

let stable_counting_pass ?cancel ~key_count keys values scratch counts =
  Array.fill counts 0 key_count 0;
  for slot = 0 to Array.length values - 1 do
    Cancel.poll cancel slot 16383;
    let key = keys.(values.(slot)) in
    if key < 0 || key >= key_count then
      invalid_arg "Boolean complex counting-sort key is out of range";
    counts.(key) <- counts.(key) + 1
  done;
  let total = ref 0 in
  for key = 0 to key_count - 1 do
    let count = counts.(key) in
    counts.(key) <- !total;
    total := !total + count
  done;
  for slot = 0 to Array.length values - 1 do
    let value = values.(slot) and key = keys.(values.(slot)) in
    scratch.(counts.(key)) <- value;
    counts.(key) <- counts.(key) + 1
  done;
  Array.blit scratch 0 values 0 (Array.length values)

let stable_counting_sort3 ?cancel ~key_count key_a key_b key_c values =
  let scratch = Array.make (Array.length values) 0
  and counts = Array.make key_count 0 in
  stable_counting_pass ?cancel ~key_count key_c values scratch counts;
  stable_counting_pass ?cancel ~key_count key_b values scratch counts;
  stable_counting_pass ?cancel ~key_count key_a values scratch counts

let stable_counting_sort2 ?cancel ~key_count key_a key_b values =
  let scratch = Array.make (Array.length values) 0
  and counts = Array.make key_count 0 in
  stable_counting_pass ?cancel ~key_count key_b values scratch counts;
  stable_counting_pass ?cancel ~key_count key_a values scratch counts

let build ?cancel constraints refinement =
  try
    Cancel.check_opt cancel;
    if Boolean_refinement.View.constraints refinement != constraints then
      invalid_arg "refinement belongs to a different constraint plan";
    let left_faces = Boolean_constraints.left_triangle_count constraints
    and right_faces = Boolean_constraints.right_triangle_count constraints
    and left_geometry = Boolean_constraints.View.left_geometry constraints
    and right_geometry = Boolean_constraints.View.right_geometry constraints in
    let source_point_count =
      Geometry.point_count left_geometry + Geometry.point_count right_geometry in
    let global_point_count = source_point_count
        + Boolean_constraints.point_count constraints in
    let raw_count = ref 0 and token_count = ref global_point_count in
    let add_faces count get =
      for face = 0 to count - 1 do
        let count = match get refinement face with
          | None -> 1
          | Some value ->
              let extra_points = ref 0 in
              for point = 3 to Boolean_face_cdt.point_count value - 1 do
                if Boolean_face_cdt.View.global_point_token value point < 0 then
                  incr extra_points
              done;
              if !token_count > Sys.max_array_length - !extra_points then
                invalid_arg "Boolean complex point-token cardinality exceeds array limits";
              token_count := !token_count + !extra_points;
              Boolean_face_cdt.triangle_count value in
        if !raw_count > Sys.max_array_length - count then
          invalid_arg "Boolean complex facet cardinality exceeds array limits";
        raw_count := !raw_count + count
      done in
    add_faces left_faces Boolean_refinement.left_face;
    add_faces right_faces Boolean_refinement.right_face;
    if !raw_count > Sys.max_array_length / 3 then
      invalid_arg "Boolean complex corner cardinality exceeds array limits";
    let corner_count = !raw_count * 3 in
    let fallback = if left_faces > 0 then
        Boolean_constraints.View.source_point constraints
          (Boolean_constraints.View.left_triangle_point constraints 0 0)
      else if right_faces > 0 then
        Boolean_constraints.View.source_point constraints
          (Boolean_constraints.View.right_triangle_point constraints 0 0)
      else invalid_arg "Boolean complex has no source faces" in
    let token_points = Array.make !token_count fallback in
    for point = 0 to source_point_count - 1 do
      token_points.(point) <- Boolean_constraints.View.source_point constraints point
    done;
    for point = 0 to Boolean_constraints.point_count constraints - 1 do
      token_points.(source_point_count + point) <-
        Boolean_constraints.View.point constraints point
    done;
    let raw_point_tokens = Array.make corner_count 0
    and raw_sides = Bytes.make !raw_count '\000'
    and raw_faces = Array.make !raw_count 0
    and raw_triangles = Array.make !raw_count (-1)
    and raw_orientations = Bytes.make !raw_count '\000'
    and raw = ref 0 and next_token = ref global_point_count in
    let construction_tokens = Construction_table.create
        (max 16 ((!token_count - global_point_count) / 3)) in
    let allocate_local point =
      let token = !next_token in
      incr next_token;
      token_points.(token) <- point;
      token in
    let intern_key kind a b c d e f g h i point =
      let present = Construction_table.find construction_tokens
          kind a b c d e f g h i in
      if present >= 0 then present else begin
        let token = allocate_local point in
        Construction_table.add construction_tokens
          kind a b c d e f g h i token;
        token
      end in
    let intern_local_point point = match Implicit_point.construction point with
      | Implicit_point.Line_plane {
          line_start; line_end; plane_a; plane_b; plane_c } ->
          let line_start,line_end = ordered_pair line_start line_end
          and plane_a,plane_b,plane_c = sorted3 plane_a plane_b plane_c in
          intern_key 1 line_start line_end plane_a plane_b plane_c
            (-1) (-1) (-1) (-1) point
      | Implicit_point.Line_line {
          first_start; first_end; second_start; second_end; _ } ->
          let first = ordered_pair first_start first_end
          and second = ordered_pair second_start second_end in
          let (first_start,first_end),(second_start,second_end) =
            if compare2 first second <= 0 then first,second else second,first in
          intern_key 2 first_start first_end second_start second_end
            (-1) (-1) (-1) (-1) (-1) point
      | Implicit_point.Triple_plane {
          first_a; first_b; first_c; second_a; second_b; second_c;
          third_a; third_b; third_c } ->
          let first = sorted3 first_a first_b first_c
          and second = sorted3 second_a second_b second_c
          and third = sorted3 third_a third_b third_c in
          let (first_a,first_b,first_c),(second_a,second_b,second_c),
              (third_a,third_b,third_c) = ordered_triple first second third in
          intern_key 3 first_a first_b first_c second_a second_b second_c
            third_a third_b third_c point
      | Implicit_point.Explicit _ | Implicit_point.Rounded _
      | Implicit_point.Midpoint _ | Implicit_point.Centroid3 _
      | Implicit_point.Circumcenter2_xy _ ->
          allocate_local point in
    let append side face point_index point_token orientation =
      let facet = !raw in
      if side = Boolean_constraints.Right then Bytes.unsafe_set raw_sides facet '\001';
      raw_faces.(facet) <- face;
      raw_triangles.(facet) <- point_index;
      if orientation < 0 then Bytes.unsafe_set raw_orientations facet '\001';
      for local = 0 to 2 do
        raw_point_tokens.((facet * 3) + local) <- point_token local
      done;
      incr raw in
    let append_side side face_count get triangle_point =
      for face = 0 to face_count - 1 do
        Cancel.poll cancel face 255;
        match get refinement face with
        | None ->
            append side face (-1)
              (fun local -> triangle_point constraints face local) 1
        | Some value ->
            let orientation = Boolean_face_cdt.View.source_winding value in
            let point_count = Boolean_face_cdt.point_count value in
            let point_tokens = Array.make point_count 0 in
            for local = 0 to min 2 (point_count - 1) do
              point_tokens.(local) <- triangle_point constraints face local
            done;
            for point = 3 to point_count - 1 do
              let global = Boolean_face_cdt.View.global_point_token value point in
              if global >= 0 then point_tokens.(point) <- global
              else begin
                point_tokens.(point) <- intern_local_point
                    (Boolean_face_cdt.View.point value point)
              end
            done;
            for triangle = 0 to Boolean_face_cdt.triangle_count value - 1 do
              append side face triangle
                (fun local -> point_tokens.(
                  Boolean_face_cdt.triangle_point value triangle local))
                orientation
            done
      done in
    append_side Boolean_constraints.Left left_faces Boolean_refinement.left_face
      Boolean_constraints.View.left_triangle_point;
    append_side Boolean_constraints.Right right_faces Boolean_refinement.right_face
      Boolean_constraints.View.right_triangle_point;
    token_count := !next_token;
    let merge_orders first second =
      let output = Array.make (Array.length first + Array.length second) 0
      and left = ref 0 and right = ref 0 and next = ref 0 in
      while !left < Array.length first && !right < Array.length second do
        if compare_points token_points first.(!left) second.(!right) <= 0 then begin
          output.(!next) <- first.(!left); incr left
        end else begin
          output.(!next) <- second.(!right); incr right
        end;
        incr next
      done;
      if !left < Array.length first then
        Array.blit first !left output !next (Array.length first - !left)
      else if !right < Array.length second then
        Array.blit second !right output !next (Array.length second - !right);
      output in
    (* Constraint points are already exact-coordinate sorted and unique by the
       constraint planner. Sort only the small explicit-source prefix and the
       genuinely face-local constructions, then merge the three runs. *)
    let source_order = Array.init source_point_count Fun.id in
    parallel_sort_indices ?cancel (compare_points token_points) source_order;
    let constraint_order = Array.init (global_point_count - source_point_count)
        (fun point -> source_point_count + point) in
    let local_order = Array.init (!token_count - global_point_count)
        (fun point -> global_point_count + point) in
    parallel_sort_indices ?cancel (compare_points token_points) local_order;
    let token_order = merge_orders (merge_orders source_order constraint_order)
        local_order in
    let vertex_of_token = Array.make !token_count 0
    and unique_count = ref 0 and previous = ref (-1) in
    Array.iter (fun token ->
      if !previous < 0 || not (Implicit_point.equal token_points.(token)
          token_points.(!previous)) then begin
        previous := token;
        incr unique_count
      end;
      vertex_of_token.(token) <- !unique_count - 1) token_order;
    let vertices = if !unique_count = 0 then [||] else begin
      let output = Array.make !unique_count token_points.(token_order.(0))
      and output_index = ref (-1) and previous = ref (-1) in
      Array.iter (fun token ->
        if !previous < 0 || not (Implicit_point.equal token_points.(token)
            token_points.(!previous)) then begin
          incr output_index; previous := token;
          output.(!output_index) <- token_points.(token)
        end) token_order;
      output
    end in
    let raw_a = Array.make !raw_count 0 and raw_b = Array.make !raw_count 0
    and raw_c = Array.make !raw_count 0 and key_a = Array.make !raw_count 0
    and key_b = Array.make !raw_count 0 and key_c = Array.make !raw_count 0 in
    for facet = 0 to !raw_count - 1 do
      let a = vertex_of_token.(raw_point_tokens.(facet * 3))
      and b = vertex_of_token.(raw_point_tokens.((facet * 3) + 1))
      and c = vertex_of_token.(raw_point_tokens.((facet * 3) + 2)) in
      if a = b || b = c || c = a then
        invalid_arg "Boolean complex contains a degenerate refined facet";
      raw_a.(facet) <- a; raw_b.(facet) <- b; raw_c.(facet) <- c;
      let a,b,c = sorted3 a b c in
      key_a.(facet) <- a; key_b.(facet) <- b; key_c.(facet) <- c
    done;
    let facet_order = Array.init !raw_count Fun.id in
    stable_counting_sort3 ?cancel ~key_count:!unique_count key_a key_b key_c
      facet_order;
    let unique_facets = ref 0 and previous_facet = ref (-1) in
    Array.iter (fun facet ->
      if !previous_facet < 0
          || key_a.(facet) <> key_a.(!previous_facet)
          || key_b.(facet) <> key_b.(!previous_facet)
          || key_c.(facet) <> key_c.(!previous_facet) then begin
        incr unique_facets; previous_facet := facet
      end) facet_order;
    let facet_a = Array.make !unique_facets 0 and facet_b = Array.make !unique_facets 0
    and facet_c = Array.make !unique_facets 0 and member_offsets = Array.make (!unique_facets + 1) 0
    and group_of_raw = Array.make !raw_count 0 and roots = Array.make !unique_facets 0 in
    let group = ref (-1) in previous_facet := -1;
    Array.iter (fun facet ->
      if !previous_facet < 0
          || key_a.(facet) <> key_a.(!previous_facet)
          || key_b.(facet) <> key_b.(!previous_facet)
          || key_c.(facet) <> key_c.(!previous_facet) then begin
        incr group; previous_facet := facet; roots.(!group) <- facet;
        facet_a.(!group) <- raw_a.(facet);
        facet_b.(!group) <- raw_b.(facet);
        facet_c.(!group) <- raw_c.(facet)
      end;
      group_of_raw.(facet) <- !group;
      member_offsets.(!group + 1) <- member_offsets.(!group + 1) + 1) facet_order;
    for facet = 0 to !unique_facets - 1 do
      member_offsets.(facet + 1) <- member_offsets.(facet + 1) + member_offsets.(facet)
    done;
    let member_sides = Bytes.make !raw_count '\000'
    and member_faces = Array.make !raw_count 0
    and member_triangles = Array.make !raw_count 0
    and member_windings = Bytes.make !raw_count '\000'
    and cursor = Array.copy member_offsets in
    for raw_facet = 0 to !raw_count - 1 do
      let group = group_of_raw.(raw_facet) in
      let root = roots.(group) and member = cursor.(group) in
      cursor.(group) <- member + 1;
      Bytes.unsafe_set member_sides member (Bytes.unsafe_get raw_sides raw_facet);
      member_faces.(member) <- raw_faces.(raw_facet);
      member_triangles.(member) <- raw_triangles.(raw_facet);
      let position value =
        if value = raw_a.(root) then 0
        else if value = raw_b.(root) then 1 else 2 in
      let p0 = position raw_a.(raw_facet)
      and p1 = position raw_b.(raw_facet)
      and p2 = position raw_c.(raw_facet) in
      let odd = (if p0 > p1 then 1 else 0)
          + (if p0 > p2 then 1 else 0)
          + (if p1 > p2 then 1 else 0) in
      let sign = (if odd land 1 = 0 then 1 else -1)
          * (if Bytes.unsafe_get raw_orientations raw_facet = '\000' then 1 else -1) in
      if sign < 0 then Bytes.unsafe_set member_windings member '\001'
    done;
    let edge_entries = !unique_facets * 3 in
    let entry_first = Array.make edge_entries 0 and entry_second = Array.make edge_entries 0
    and entry_facet = Array.make edge_entries 0 and entry_local = Bytes.make edge_entries '\000' in
    for facet = 0 to !unique_facets - 1 do
      let a = facet_a.(facet) and b = facet_b.(facet) and c = facet_c.(facet)
      and entry = facet * 3 in
      entry_first.(entry) <- min a b; entry_second.(entry) <- max a b;
      entry_facet.(entry) <- facet;
      entry_first.(entry + 1) <- min b c; entry_second.(entry + 1) <- max b c;
      entry_facet.(entry + 1) <- facet;
      Bytes.unsafe_set entry_local (entry + 1) '\001';
      entry_first.(entry + 2) <- min c a; entry_second.(entry + 2) <- max c a;
      entry_facet.(entry + 2) <- facet;
      Bytes.unsafe_set entry_local (entry + 2) '\002'
    done;
    let edge_order = Array.init edge_entries Fun.id in
    stable_counting_sort2 ?cancel ~key_count:!unique_count entry_first entry_second
      edge_order;
    let edges = ref 0 and previous_entry = ref (-1) in
    Array.iter (fun entry ->
      if !previous_entry < 0 || entry_first.(entry) <> entry_first.(!previous_entry)
          || entry_second.(entry) <> entry_second.(!previous_entry) then begin
        incr edges; previous_entry := entry
      end) edge_order;
    let edge_first = Array.make !edges 0 and edge_second = Array.make !edges 0
    and edge_offsets = Array.make (!edges + 1) 0 and edge_facets = Array.make edge_entries 0
    and edge_locals = Bytes.make edge_entries '\000' in
    let edge = ref (-1) and incident = ref 0 in previous_entry := -1;
    Array.iter (fun entry ->
      if !previous_entry < 0 || entry_first.(entry) <> entry_first.(!previous_entry)
          || entry_second.(entry) <> entry_second.(!previous_entry) then begin
        if !edge >= 0 then edge_offsets.(!edge + 1) <- !incident;
        incr edge; previous_entry := entry;
        edge_first.(!edge) <- entry_first.(entry);
        edge_second.(!edge) <- entry_second.(entry)
      end;
      edge_facets.(!incident) <- entry_facet.(entry);
      Bytes.unsafe_set edge_locals !incident (Bytes.unsafe_get entry_local entry);
      incr incident) edge_order;
    if !edges > 0 then edge_offsets.(!edges) <- !incident;
    Ok { constraints; vertices; facet_a; facet_b; facet_c; member_offsets; member_sides;
         member_faces; member_triangles; member_windings; edge_first; edge_second;
         edge_offsets; edge_facets; edge_locals }
  with
  | Cancel.Cancelled -> error "cancelled" "Boolean complex assembly was cancelled"
  | Invalid_argument message -> error "invalid_refinement" message
