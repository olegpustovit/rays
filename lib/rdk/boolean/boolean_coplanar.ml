open Rays_math

type overlap_kind = Empty | Point | Segment | Polygon

type overlap = {
  kind : overlap_kind;
  points : Implicit_point.t array;
  support_first : int array;
  support_second : int array;
}

type t = {
  constraints : Boolean_constraints.t;
  overlaps : overlap array;
  left_offsets : int array;
  left_pairs : int array;
  right_offsets : int array;
  right_pairs : int array;
}

let operation = "boolean_coplanar"
let error code message = Error (Error.make ~operation ~code message)

let empty = {
  kind = Empty; points = [||]; support_first = [||]; support_second = [||];
}

let pair_count value = Array.length value.overlaps
let first_triangle value pair =
  Boolean_constraints.coplanar_first_triangle value.constraints pair
let second_triangle value pair =
  Boolean_constraints.coplanar_second_triangle value.constraints pair
let kind value pair = value.overlaps.(pair).kind
let point_count value pair = Array.length value.overlaps.(pair).points

let boundary_count value pair = match kind value pair with
  | Empty | Point -> 0
  | Segment -> 1
  | Polygon -> point_count value pair

let boundary_first value pair boundary = match kind value pair with
  | Segment when boundary = 0 -> 0
  | Polygon ->
      if boundary < 0 || boundary >= point_count value pair then
        invalid_arg "Coplanar Boolean boundary index is out of bounds";
      boundary
  | _ -> invalid_arg "Coplanar Boolean overlap has no requested boundary"

let boundary_second value pair boundary = match kind value pair with
  | Segment when boundary = 0 -> 1
  | Polygon ->
      let count = point_count value pair in
      if boundary < 0 || boundary >= count then
        invalid_arg "Coplanar Boolean boundary index is out of bounds";
      (boundary + 1) mod count
  | _ -> invalid_arg "Coplanar Boolean overlap has no requested boundary"

module View = struct
  let constraints value = value.constraints
  let point value pair point = value.overlaps.(pair).points.(point)
  let boundary_support_first value pair boundary =
    value.overlaps.(pair).support_first.(boundary)
  let boundary_support_second value pair boundary =
    value.overlaps.(pair).support_second.(boundary)
  let left_pair_range value triangle =
    value.left_offsets.(triangle), value.left_offsets.(triangle + 1)
  let right_pair_range value triangle =
    value.right_offsets.(triangle), value.right_offsets.(triangle + 1)
  let left_pair value slot = value.left_pairs.(slot)
  let right_pair value slot = value.right_pairs.(slot)
end

let sign_opposite left right = match left, right with
  | Predicates.Negative, Predicates.Positive
  | Predicates.Positive, Predicates.Negative -> true
  | _ -> false

let projection points =
  match Implicit_point.orient2d_xy points.(0) points.(1) points.(2) with
  | Predicates.Positive | Predicates.Negative -> Implicit_point.XY
  | Predicates.Zero ->
      (match Implicit_point.orient2d_yz points.(0) points.(1) points.(2) with
       | Predicates.Positive | Predicates.Negative -> Implicit_point.YZ
       | Predicates.Zero -> Implicit_point.ZX)

let orient projection = match projection with
  | Implicit_point.XY -> Implicit_point.orient2d_xy
  | Implicit_point.YZ -> Implicit_point.orient2d_yz
  | Implicit_point.ZX -> Implicit_point.orient2d_zx

let compare_projected projection left right =
  let first, second = match projection with
    | Implicit_point.XY -> Implicit_point.compare_x, Implicit_point.compare_y
    | Implicit_point.YZ -> Implicit_point.compare_y, Implicit_point.compare_z
    | Implicit_point.ZX -> Implicit_point.compare_z, Implicit_point.compare_x in
  let comparison = first left right in
  if comparison <> 0 then comparison else second left right

let inside_triangle orient triangle point =
  let orientation = orient triangle.(0) triangle.(1) triangle.(2) in
  let first = orient triangle.(0) triangle.(1) point
  and second = orient triangle.(1) triangle.(2) point
  and third = orient triangle.(2) triangle.(0) point in
  match orientation with
  | Predicates.Positive ->
      first <> Predicates.Negative
      && second <> Predicates.Negative
      && third <> Predicates.Negative
  | Predicates.Negative ->
      first <> Predicates.Positive
      && second <> Predicates.Positive
      && third <> Predicates.Positive
  | Predicates.Zero -> invalid_arg "degenerate triangle in coplanar pair"

let convex_hull projection candidates count =
  if count <= 1 then Array.sub candidates 0 count
  else begin
    let points = Array.sub candidates 0 count in
    Array.sort (compare_projected projection) points;
    let orient = orient projection in
    let hull = Array.make (count * 2) 0 and size = ref 0 in
    let append index =
      while !size >= 2
          && orient points.(hull.(!size - 2)) points.(hull.(!size - 1))
               points.(index) <> Predicates.Positive do
        decr size
      done;
      hull.(!size) <- index;
      incr size in
    for index = 0 to count - 1 do append index done;
    let upper_start = !size + 1 in
    for index = count - 2 downto 0 do
      while !size >= upper_start
          && orient points.(hull.(!size - 2)) points.(hull.(!size - 1))
               points.(index) <> Predicates.Positive do
        decr size
      done;
      hull.(!size) <- index;
      incr size
    done;
    if !size > 1 then decr size;
    Array.init !size (fun index -> points.(hull.(index)))
  end

exception Construction_failed

let build_pair constraints pair =
  let source = Boolean_constraints.View.source constraints
  and first_side = Boolean_constraints.coplanar_first_side constraints pair
  and second_side = Boolean_constraints.coplanar_second_side constraints pair
  and first_face = Boolean_constraints.coplanar_first_triangle constraints pair
  and second_face = Boolean_constraints.coplanar_second_triangle constraints pair in
  let first_ids = Array.init 3 (fun local ->
      Boolean_constraints.View.triangle_point
        constraints first_side first_face local)
  and second_ids = Array.init 3 (fun local ->
      Boolean_constraints.View.triangle_point
        constraints second_side second_face local) in
  let first_triangle = Array.map (fun point ->
      Implicit_point.explicit source point |> Result.get_ok) first_ids
  and second_triangle = Array.map (fun point ->
      Implicit_point.explicit source point |> Result.get_ok) second_ids in
  let projection = projection first_triangle in
  let orient = orient projection in
  let fallback = first_triangle.(0) in
  let candidates = Array.make 15 fallback and count = ref 0 in
  let append point =
    let found = ref false and index = ref 0 in
    while not !found && !index < !count do
      found := Implicit_point.equal candidates.(!index) point;
      incr index
    done;
    if not !found then begin
      candidates.(!count) <- point;
      incr count
    end in
  for local = 0 to 2 do
    if inside_triangle orient second_triangle first_triangle.(local) then
      append first_triangle.(local);
    if inside_triangle orient first_triangle second_triangle.(local) then
      append second_triangle.(local)
  done;
  for first_edge = 0 to 2 do
    let first_next = (first_edge + 1) mod 3 in
    for second_edge = 0 to 2 do
      let second_next = (second_edge + 1) mod 3 in
      let first = orient first_triangle.(first_edge) first_triangle.(first_next)
          second_triangle.(second_edge)
      and second = orient first_triangle.(first_edge) first_triangle.(first_next)
          second_triangle.(second_next)
      and third = orient second_triangle.(second_edge) second_triangle.(second_next)
          first_triangle.(first_edge)
      and fourth = orient second_triangle.(second_edge) second_triangle.(second_next)
          first_triangle.(first_next) in
      if sign_opposite first second && sign_opposite third fourth then
        match Implicit_point.line_line source ~projection
            ~first_start:first_ids.(first_edge) ~first_end:first_ids.(first_next)
            ~second_start:second_ids.(second_edge) ~second_end:second_ids.(second_next) with
        | Ok point -> append point
        | Error _ -> raise Construction_failed
    done
  done;
  let points = convex_hull projection candidates !count in
  let kind = match Array.length points with
    | 0 -> Empty
    | 1 -> Point
    | 2 -> Segment
    | _ -> Polygon in
  let common = Array.make 3 (-1) and common_count = ref 0 in
  if first_side = second_side then
    for a = 0 to 2 do
      let found = ref false in
      for b = 0 to 2 do if first_ids.(a) = second_ids.(b) then found := true done;
      if !found then begin common.(!common_count) <- first_ids.(a); incr common_count end
    done;
  let common_point point =
    let found = ref false in
    for slot = 0 to !common_count - 1 do
      let explicit = Implicit_point.explicit source common.(slot) |> Result.get_ok in
      if Implicit_point.equal point explicit then found := true
    done;
    !found in
  let topology_contact = match kind with
    | Point -> !common_count >= 1 && common_point points.(0)
    | Segment -> !common_count >= 2 && common_point points.(0)
        && common_point points.(1)
    | Empty | Polygon -> false in
  if topology_contact then empty else
  let boundary_count = match kind with
    | Empty | Point -> 0 | Segment -> 1 | Polygon -> Array.length points in
  let support_first = Array.make boundary_count 0
  and support_second = Array.make boundary_count 0 in
  for boundary = 0 to boundary_count - 1 do
    let first = points.(boundary)
    and second = points.(if kind = Segment then 1
        else (boundary + 1) mod Array.length points) in
    let best_first = ref (-1) and best_second = ref (-1) in
    let consider ids triangle edge =
      let next = (edge + 1) mod 3 in
      if orient triangle.(edge) triangle.(next) first = Predicates.Zero
          && orient triangle.(edge) triangle.(next) second = Predicates.Zero then begin
        let a = min ids.(edge) ids.(next) and b = max ids.(edge) ids.(next) in
        if !best_first < 0 || a < !best_first
            || (a = !best_first && b < !best_second) then begin
          best_first := a; best_second := b
        end
      end in
    for edge = 0 to 2 do
      consider first_ids first_triangle edge;
      consider second_ids second_triangle edge
    done;
    if !best_first < 0 then
      invalid_arg "coplanar overlap boundary has no source-edge support";
    support_first.(boundary) <- !best_first;
    support_second.(boundary) <- !best_second
  done;
  { kind; points; support_first; support_second }

let make_face_csr target_side face_count constraints =
  let pair_count = Boolean_constraints.coplanar_pair_count constraints in
  let counts = Array.make face_count 0 in
  for pair = 0 to pair_count - 1 do
    if Boolean_constraints.coplanar_first_side constraints pair = target_side then begin
      let face = Boolean_constraints.coplanar_first_triangle constraints pair in
      counts.(face) <- counts.(face) + 1
    end;
    if Boolean_constraints.coplanar_second_side constraints pair = target_side then begin
      let face = Boolean_constraints.coplanar_second_triangle constraints pair in
      counts.(face) <- counts.(face) + 1
    end
  done;
  let offsets = Array.make (face_count + 1) 0 in
  for face = 0 to face_count - 1 do
    offsets.(face + 1) <- offsets.(face) + counts.(face)
  done;
  let pairs = Array.make offsets.(face_count) 0 and cursor = Array.copy offsets in
  for pair = 0 to pair_count - 1 do
    let add face = pairs.(cursor.(face)) <- pair;
      cursor.(face) <- cursor.(face) + 1 in
    if Boolean_constraints.coplanar_first_side constraints pair = target_side then
      add (Boolean_constraints.coplanar_first_triangle constraints pair);
    if Boolean_constraints.coplanar_second_side constraints pair = target_side then
      add (Boolean_constraints.coplanar_second_triangle constraints pair)
  done;
  offsets, pairs

let build ?cancel ~grain constraints =
  try
    if grain <= 0 then error "invalid_parameter" "grain must be positive"
    else begin
      Cancel.check_opt cancel;
      let count = Boolean_constraints.coplanar_pair_count constraints in
      let overlaps = Array.make count empty and failures = Bytes.make count '\000' in
      (* Each pair owns its output and cooks in stable index ranges. Exact
         homogeneous fallback is still allocator-bound, so this preserves
         deterministic concurrency without promising multicore speedup. *)
      if count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
          ~finish:(count - 1) (fun pair ->
        Cancel.poll cancel pair 255;
        try overlaps.(pair) <- build_pair constraints pair
        with Construction_failed | Invalid_argument _ ->
          Bytes.unsafe_set failures pair '\001');
      let failed = ref (-1) and pair = ref 0 in
      while !failed < 0 && !pair < count do
        if Bytes.unsafe_get failures !pair <> '\000' then failed := !pair;
        incr pair
      done;
      if !failed >= 0 then error "exact_construction_failed"
          (Printf.sprintf "coplanar pair %d could not be arranged exactly" !failed)
      else begin
        let left_offsets, left_pairs = make_face_csr
            Boolean_constraints.Left
            (Boolean_constraints.left_triangle_count constraints) constraints
        and right_offsets, right_pairs = make_face_csr
            Boolean_constraints.Right
            (Boolean_constraints.right_triangle_count constraints) constraints in
        Ok { constraints; overlaps; left_offsets; left_pairs;
             right_offsets; right_pairs }
      end
    end
  with Cancel.Cancelled -> error "cancelled" "Coplanar Boolean arrangement was cancelled"
