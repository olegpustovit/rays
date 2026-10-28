type t = {
  points : Implicit_point.t array;
  global_point_tokens : int array;
  source_winding : int;
  triangle_a : int array;
  triangle_b : int array;
  triangle_c : int array;

}

type workspace = Planar_cdt.Private.workspace

let create_workspace () =
  Planar_cdt.Private.create_workspace ~point_capacity:16
    ~triangle_capacity:16 ()

let operation = "boolean_face_cdt"
let error code message = Error (Error.make ~operation ~code message)

let point_count value = Array.length value.points
let triangle_count value = Array.length value.triangle_a
let triangle_point value triangle local = match local with
  | 0 -> value.triangle_a.(triangle)
  | 1 -> value.triangle_b.(triangle)
  | 2 -> value.triangle_c.(triangle)
  | _ -> invalid_arg "Boolean face CDT local corner must be 0, 1, or 2"

module Private = struct
  let point value point = value.points.(point)
  let global_point_token value point = value.global_point_tokens.(point)
  let source_winding value = value.source_winding
end

let face_triangle_point constraints side triangle local = match side with
  | Boolean_face_arrangement.Left ->
      Boolean_constraints.Private.left_triangle_point constraints triangle local
  | Boolean_face_arrangement.Right ->
      Boolean_constraints.Private.right_triangle_point constraints triangle local

let face_triangle_implicit constraints side triangle local =
  Boolean_constraints.Private.source_point constraints
    (face_triangle_point constraints side triangle local)

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

let incircle projection = match projection with
  | 0 -> Implicit_point.incircle_xy
  | 1 -> Implicit_point.incircle_yz
  | _ -> Implicit_point.incircle_zx

module Orientation_cache = struct
  type table = {
    stride : int;
    mutable keys : int array;
    mutable signs : bytes;
    mutable count : int;
  }
  type t = Disabled | Table of table

  let create point_count =
    if point_count <= 0 || point_count > max_int / point_count
        || point_count * point_count > max_int / point_count then Disabled
    else begin
      let needed = if point_count > Sys.max_array_length / 8 then
          Sys.max_array_length else max 16 (point_count * 8) in
      let capacity = ref 16 in
      while !capacity < needed do
        if !capacity > Sys.max_array_length / 2 then capacity := needed
        else capacity := !capacity * 2
      done;
      Table { stride = point_count; keys = Array.make !capacity (-1);
        signs = Bytes.make !capacity '\000'; count = 0 }
    end

  let[@inline] key stride a b c = (((a * stride) + b) * stride) + c
  let[@inline] hash key = ((key lxor (key lsr 16)) * 0x45d9f3b) land max_int

  let find_slot keys key =
    let mask = Array.length keys - 1 in
    let slot = ref (hash key land mask) in
    while keys.(!slot) >= 0 && keys.(!slot) <> key do
      slot := (!slot + 1) land mask
    done;
    !slot

  let grow cache =
    let previous_keys = cache.keys and previous_signs = cache.signs in
    if Array.length previous_keys > Sys.max_array_length / 2 then
      invalid_arg "Boolean face CDT orientation cache exceeds array limits";
    cache.keys <- Array.make (Array.length previous_keys * 2) (-1);
    cache.signs <- Bytes.make (Array.length cache.keys) '\000';
    for slot = 0 to Array.length previous_keys - 1 do
      let key = previous_keys.(slot) in
      if key >= 0 then begin
        let target = find_slot cache.keys key in
        cache.keys.(target) <- key;
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

  let get cache exact a b c = match cache with
    | Disabled -> exact a b c
    | Table cache ->
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
      if cache.count * 3 >= Array.length cache.keys * 2 then grow cache;
      let key = key cache.stride a b c in
      let slot = find_slot cache.keys key in
      let sign = if cache.keys.(slot) = key then
          decode (Bytes.unsafe_get cache.signs slot)
        else begin
          let sign = exact a b c in
          cache.keys.(slot) <- key;
          Bytes.unsafe_set cache.signs slot (encode sign);
          cache.count <- cache.count + 1;
          sign
        end in
      if inversions land 1 = 0 then sign else reverse sign
    end
end

let global_point_tokens constraints arrangement arrangement_to_point
    ~side ~triangle ~point_count =
  let tokens = Array.make point_count (-1) in
  for local = 0 to 2 do
    tokens.(local) <- face_triangle_point constraints side triangle local
  done;
  let source_count = Boolean_constraints.Private.source_point_count constraints in
  for arrangement_point = 0 to Array.length arrangement_to_point - 1 do
    let handle = Boolean_face_arrangement.Private.point_handle
        arrangement arrangement_point in
    if handle >= 0 then begin
      let point = arrangement_to_point.(arrangement_point) in
      if tokens.(point) < 0 then tokens.(point) <- source_count + handle
    end
  done;
  tokens

(* Boolean arrangements and the general planar CDT share the same exact PSLG
   contract. Keep this adapter at the representation boundary so Boolean face
   refinement uses the terminating strip/cavity recovery owned by Planar_cdt
   rather than maintaining a second constraint-insertion kernel. *)
let build ?cancel ?workspace
    constraints arrangement ~side ~triangle =
  try
    Cancel.check_opt cancel;
    let arrangement_count = Boolean_face_arrangement.point_count arrangement in
    let point_capacity = 3 + arrangement_count in
    let fallback = face_triangle_implicit constraints side triangle 0 in
    let points = Array.make point_capacity fallback in
    for local = 0 to 2 do
      points.(local) <- face_triangle_implicit constraints side triangle local
    done;
    let arrangement_to_point = Array.make arrangement_count 0
    and actual_point_count = ref 3 in
    for arrangement_point = 0 to arrangement_count - 1 do
      let point = Boolean_face_arrangement.Private.point arrangement arrangement_point in
      let found = ref (-1) and candidate = ref 0 in
      while !found < 0 && !candidate < 3 do
        if Implicit_point.equal points.(!candidate) point then found := !candidate;
        incr candidate
      done;
      if !found >= 0 then arrangement_to_point.(arrangement_point) <- !found
      else begin
        points.(!actual_point_count) <- point;
        arrangement_to_point.(arrangement_point) <- !actual_point_count;
        incr actual_point_count
      end
    done;
    let points = Array.sub points 0 !actual_point_count in
    let projection_axis = projection points in
    let orient_points = orient projection_axis
    and incircle_points = incircle projection_axis in
    let orientation_cache = Orientation_cache.create (Array.length points) in
    let orient_indices a b c = Orientation_cache.get orientation_cache
        (fun a b c -> orient_points points.(a) points.(b) points.(c)) a b c in
    let source_winding = match orient_indices 0 1 2 with
      | Predicates.Positive -> 1
      | Predicates.Negative -> -1
      | Predicates.Zero -> invalid_arg "degenerate source triangle during face CDT" in
    let initial_triangle_points = if source_winding > 0
      then [|0;1;2|] else [|0;2;1|] in
    let insert_points = Array.init (Array.length points - 3) (fun index -> index + 3) in
    let source_constraint_count =
      Boolean_face_arrangement.segment_count arrangement in
    (* Face arrangement already canonicalizes every construction and performs
       its own indexed all-point/segment incidence pass before materializing
       unique atomic edges. Repeating that exact O(points * segments) scan here
       dominated dense fracture refinement and could not discover a new cut. *)
    let constraint_points = Array.init (source_constraint_count * 2) (fun slot ->
        let segment = slot / 2 in
        let arrangement_point = if slot land 1 = 0 then
            Boolean_face_arrangement.segment_first arrangement segment
          else Boolean_face_arrangement.segment_second arrangement segment in
        arrangement_to_point.(arrangement_point)) in
    let constraint_count = source_constraint_count in
    let point_u_min = Array.make (Array.length points) 0.
    and point_u_max = Array.make (Array.length points) 0.
    and point_v_min = Array.make (Array.length points) 0.
    and point_v_max = Array.make (Array.length points) 0. in
    for point = 0 to Array.length points - 1 do
      let (x_min,x_max),(y_min,y_max),(z_min,z_max) =
        Implicit_point.bounds points.(point) in
      let u_min,u_max,v_min,v_max = match projection_axis with
        | 0 -> x_min,x_max,y_min,y_max
        | 1 -> y_min,y_max,z_min,z_max
        | _ -> z_min,z_max,x_min,x_max in
      point_u_min.(point) <- u_min; point_u_max.(point) <- u_max;
      point_v_min.(point) <- v_min; point_v_max.(point) <- v_max
    done;
    let bounds_overlap first second edge_a edge_b =
      let first_u_min = min point_u_min.(first) point_u_min.(second)
      and first_u_max = max point_u_max.(first) point_u_max.(second)
      and first_v_min = min point_v_min.(first) point_v_min.(second)
      and first_v_max = max point_v_max.(first) point_v_max.(second)
      and second_u_min = min point_u_min.(edge_a) point_u_min.(edge_b)
      and second_u_max = max point_u_max.(edge_a) point_u_max.(edge_b)
      and second_v_min = min point_v_min.(edge_a) point_v_min.(edge_b)
      and second_v_max = max point_v_max.(edge_a) point_v_max.(edge_b) in
      first_u_max >= second_u_min && second_u_max >= first_u_min
      && first_v_max >= second_v_min && second_v_max >= first_v_min in
    match Planar_cdt.build ?cancel ?workspace ~point_count:(Array.length points)
        ~orient:orient_indices
        ~incircle:(fun a b c d ->
          incircle_points points.(a) points.(b) points.(c) points.(d))
        ~bounds_overlap ~triangle_points:initial_triangle_points ~insert_points
        ~constraint_points () with
    | Error message -> error "invalid_constraints" (Printf.sprintf
          "%s (side=%s source_triangle=%d points=%d constraints=%d)" message
          (match side with Boolean_face_arrangement.Left -> "left"
           | Boolean_face_arrangement.Right -> "right")
          triangle (Array.length points) constraint_count)
    | Ok triangulation ->
        let view = Planar_cdt.Private.view triangulation in
        let triangle_count = Array.length view.triangle_points / 3 in
        Ok {
          points;
          global_point_tokens = global_point_tokens constraints arrangement
              arrangement_to_point ~side ~triangle
              ~point_count:(Array.length points);
          source_winding;
          triangle_a = Array.init triangle_count (fun triangle ->
            view.triangle_points.(triangle * 3));
          triangle_b = Array.init triangle_count (fun triangle ->
            view.triangle_points.((triangle * 3) + 1));
          triangle_c = Array.init triangle_count (fun triangle ->
            view.triangle_points.((triangle * 3) + 2));

        }
  with
  | Cancel.Cancelled -> error "cancelled" "Boolean face CDT was cancelled"
  | Invalid_argument message -> error "invalid_constraints" message
