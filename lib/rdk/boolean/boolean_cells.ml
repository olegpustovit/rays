type t = {
  weiler : Boolean_weiler.t;
  left : int array;
  right : int array;
}

type axis = X | Y | Z

type components = {
  offsets : int array;
  triangles : int array;
  min_x : float array; min_y : float array; min_z : float array;
  max_x : float array; max_y : float array; max_z : float array;
  order : int array;
  tree_min_x : float array; tree_min_y : float array; tree_min_z : float array;
  tree_max_x : float array; tree_max_y : float array; tree_max_z : float array;
  query_candidates : int array;
  query_first : int array;
  query_last : int array;
}

let operation = "boolean_cells"
let error code message = Error (Error.make ~operation ~code message)
let shell_count value = Array.length value.left
let left_winding value shell = value.left.(shell)
let right_winding value shell = value.right.(shell)

module View = struct
  let weiler value = value.weiler
end

let orient2 axis = match axis with
  | X -> Implicit_point.orient2d_yz
  | Y -> Implicit_point.orient2d_zx
  | Z -> Implicit_point.orient2d_xy

let axis_index = function X -> 0 | Y -> 1 | Z -> 2

let checked_add left right =
  if right > 0 && left > max_int - right
      || right < 0 && left < min_int - right then
    invalid_arg "Boolean winding integer overflow";
  left + right

let center lower upper = (lower *. 0.5) +. (upper *. 0.5)

let build_component_tree ?cancel min_x min_y min_z max_x max_y max_z =
  let count = Array.length min_x in
  let order = Array.init count Fun.id in
  if count > 1 then begin
    let span lower upper =
      let minimum = ref Float.infinity and maximum = ref Float.neg_infinity in
      for component = 0 to count - 1 do
        Cancel.poll cancel component 4095;
        let value = center lower.(component) upper.(component) in
        minimum := Float.min !minimum value;
        maximum := Float.max !maximum value
      done;
      !maximum -. !minimum in
    let spans = [|span min_x max_x, 0; span min_y max_y, 1;
      span min_z max_z, 2|] in
    Array.sort (fun (left_span, left_axis) (right_span, right_axis) ->
      let compared = Float.compare right_span left_span in
      if compared <> 0 then compared else Int.compare left_axis right_axis) spans;
    let axis = snd spans.(0) in
    let lower, upper = if axis = 0 then min_x, max_x
      else if axis = 1 then min_y, max_y else min_z, max_z in
    let comparisons = ref 0 in
    Array.sort (fun left right ->
      incr comparisons;
      Cancel.poll cancel !comparisons 4095;
      let compared = Float.compare
          (center lower.(left) upper.(left))
          (center lower.(right) upper.(right)) in
      if compared <> 0 then compared else Int.compare left right) order
  end;
  let tree_min_x = Array.make count Float.infinity
  and tree_min_y = Array.make count Float.infinity
  and tree_min_z = Array.make count Float.infinity
  and tree_max_x = Array.make count Float.neg_infinity
  and tree_max_y = Array.make count Float.neg_infinity
  and tree_max_z = Array.make count Float.neg_infinity in
  let include_node parent child =
    tree_min_x.(parent) <- Float.min tree_min_x.(parent) tree_min_x.(child);
    tree_min_y.(parent) <- Float.min tree_min_y.(parent) tree_min_y.(child);
    tree_min_z.(parent) <- Float.min tree_min_z.(parent) tree_min_z.(child);
    tree_max_x.(parent) <- Float.max tree_max_x.(parent) tree_max_x.(child);
    tree_max_y.(parent) <- Float.max tree_max_y.(parent) tree_max_y.(child);
    tree_max_z.(parent) <- Float.max tree_max_z.(parent) tree_max_z.(child) in
  let built = ref 0 in
  let rec build first last =
    if first <= last then begin
      incr built;
      Cancel.poll cancel !built 4095;
      let middle = first + ((last - first) / 2) in
      let component = order.(middle) in
      tree_min_x.(middle) <- min_x.(component);
      tree_min_y.(middle) <- min_y.(component);
      tree_min_z.(middle) <- min_z.(component);
      tree_max_x.(middle) <- max_x.(component);
      tree_max_y.(middle) <- max_y.(component);
      tree_max_z.(middle) <- max_z.(component);
      if first < middle then begin
        build first (middle - 1);
        include_node middle (first + (((middle - 1) - first) / 2))
      end;
      if middle < last then begin
        build (middle + 1) last;
        include_node middle ((middle + 1) + ((last - (middle + 1)) / 2))
      end
    end in
  build 0 (count - 1);
  Cancel.check_opt cancel;
  order, tree_min_x, tree_min_y, tree_min_z,
  tree_max_x, tree_max_y, tree_max_z

let build_components ?cancel constraints side =
  let triangle_count, triangle_point = match side with
    | Boolean_complex.Left ->
        Boolean_constraints.left_triangle_count constraints,
        Boolean_constraints.View.left_triangle_point
    | Boolean_complex.Right ->
        Boolean_constraints.right_triangle_count constraints,
        Boolean_constraints.View.right_triangle_point in
  let parent = Array.init triangle_count Fun.id in
  let rec find value =
    let next = parent.(value) in
    if next = value then value else begin
      let root = find next in parent.(value) <- root; root
    end in
  let unite left right =
    let left = find left and right = find right in
    if left <> right then
      if left < right then parent.(right) <- left else parent.(left) <- right in
  let first_triangle = Array.make
      (Boolean_constraints.View.source_point_count constraints) (-1) in
  for triangle = 0 to triangle_count - 1 do
    Cancel.poll cancel triangle 4095;
    for local = 0 to 2 do
      let point = triangle_point constraints triangle local in
      if point < 0 || point >= Array.length first_triangle then
        invalid_arg "Boolean component source point is out of range";
      let other = first_triangle.(point) in
      if other < 0 then first_triangle.(point) <- triangle
      else unite triangle other
    done
  done;
  for triangle = 0 to triangle_count - 1 do
    Cancel.poll cancel triangle 4095;
    parent.(triangle) <- find triangle
  done;
  let component_of_root = Array.make triangle_count (-1) and count = ref 0 in
  for triangle = 0 to triangle_count - 1 do
    Cancel.poll cancel triangle 4095;
    if parent.(triangle) = triangle then begin
      component_of_root.(triangle) <- !count;
      incr count
    end
  done;
  let component_of_triangle = Array.init triangle_count
      (fun triangle ->
        Cancel.poll cancel triangle 4095;
        component_of_root.(parent.(triangle))) in
  let counts = Array.make !count 0 in
  Array.iter (fun component -> counts.(component) <- counts.(component) + 1)
    component_of_triangle;
  let offsets = Array.make (!count + 1) 0 in
  for component = 0 to !count - 1 do
    offsets.(component + 1) <- offsets.(component) + counts.(component)
  done;
  let triangles = Array.make triangle_count 0 and cursor = Array.copy offsets in
  for triangle = 0 to triangle_count - 1 do
    Cancel.poll cancel triangle 4095;
    let component = component_of_triangle.(triangle) in
    triangles.(cursor.(component)) <- triangle;
    cursor.(component) <- cursor.(component) + 1
  done;
  let min_x = Array.make !count Float.infinity
  and min_y = Array.make !count Float.infinity
  and min_z = Array.make !count Float.infinity
  and max_x = Array.make !count Float.neg_infinity
  and max_y = Array.make !count Float.neg_infinity
  and max_z = Array.make !count Float.neg_infinity
  and source = Boolean_constraints.View.source constraints in
  for triangle = 0 to triangle_count - 1 do
    Cancel.poll cancel triangle 4095;
    let component = component_of_triangle.(triangle) in
    for local = 0 to 2 do
      let x, y, z = Implicit_point.source_coordinate source
          (triangle_point constraints triangle local) in
      min_x.(component) <- Float.min min_x.(component) x;
      min_y.(component) <- Float.min min_y.(component) y;
      min_z.(component) <- Float.min min_z.(component) z;
      max_x.(component) <- Float.max max_x.(component) x;
      max_y.(component) <- Float.max max_y.(component) y;
      max_z.(component) <- Float.max max_z.(component) z
    done
  done;
  let order, tree_min_x, tree_min_y, tree_min_z,
      tree_max_x, tree_max_y, tree_max_z =
    build_component_tree ?cancel min_x min_y min_z max_x max_y max_z in
  let rec tree_depth depth width =
    if width <= 1 then depth else tree_depth (depth + 1) ((width + 1) / 2) in
  let query_stack_capacity = max 1 (tree_depth 1 !count) in
  { offsets; triangles; min_x; min_y; min_z; max_x; max_y; max_z;
    order; tree_min_x; tree_min_y; tree_min_z;
    tree_max_x; tree_max_y; tree_max_z;
    query_candidates = Array.make !count 0;
    query_first = Array.make query_stack_capacity 0;
    query_last = Array.make query_stack_capacity 0 }

let query_components_indexed components query =
  let (x_lower, x_upper), (y_lower, y_upper), (z_lower, z_upper) =
    Implicit_point.bounds query in
  let found = ref 0 and top = ref (-1) in
  if Array.length components.order > 0 then begin
    top := 0;
    components.query_first.(0) <- 0;
    components.query_last.(0) <- Array.length components.order - 1
  end;
  while !top >= 0 do
      let first = components.query_first.(!top)
      and last = components.query_last.(!top) in
      decr top;
      let middle = first + ((last - first) / 2) in
      if components.tree_max_x.(middle) >= x_lower
          && components.tree_min_x.(middle) <= x_upper
          && components.tree_max_y.(middle) >= y_lower
          && components.tree_min_y.(middle) <= y_upper
          && components.tree_max_z.(middle) >= z_lower
          && components.tree_min_z.(middle) <= z_upper then begin
        let component = components.order.(middle) in
        if components.max_x.(component) >= x_lower
            && components.min_x.(component) <= x_upper
            && components.max_y.(component) >= y_lower
            && components.min_y.(component) <= y_upper
            && components.max_z.(component) >= z_lower
            && components.min_z.(component) <= z_upper then begin
          components.query_candidates.(!found) <- component;
          incr found
        end;
        if middle < last then begin
          incr top;
          if !top >= Array.length components.query_first then
            invalid_arg "Boolean component BVH traversal exceeded its proven depth";
          components.query_first.(!top) <- middle + 1;
          components.query_last.(!top) <- last
        end;
        if first < middle then begin
          incr top;
          if !top >= Array.length components.query_first then
            invalid_arg "Boolean component BVH traversal exceeded its proven depth";
          components.query_first.(!top) <- first;
          components.query_last.(!top) <- middle - 1
        end
      end
  done;
  !found

let classify_operand ?cancel constraints components side query axis
    ~positive skip =
  let triangle_point = match side with
    | Boolean_complex.Left ->
        Boolean_constraints.View.left_triangle_point
    | Boolean_complex.Right ->
        Boolean_constraints.View.right_triangle_point in
  let source = Boolean_constraints.View.source constraints
  and axis = axis_index axis and winding = ref 0 and ambiguous = ref false in
  let component_count = query_components_indexed components query in
  for candidate = 0 to component_count - 1 do
    let component = components.query_candidates.(candidate) in
    for slot = components.offsets.(component) to components.offsets.(component + 1) - 1 do
    let face = components.triangles.(slot) in
    Cancel.poll cancel face 4095;
    if not (skip face) then begin
      match Implicit_point.axis_ray_source_triangle source
          ~a:(triangle_point constraints face 0)
          ~b:(triangle_point constraints face 1)
          ~c:(triangle_point constraints face 2)
          ~axis ~positive query with
      | Implicit_point.Axis_miss | Implicit_point.Axis_parallel -> ()
      | Implicit_point.Axis_boundary -> ambiguous := true
      | Implicit_point.Axis_hit normal ->
          winding := checked_add !winding
              (if normal = Predicates.Positive then 1 else -1)
    end
    done
  done;
  if !ambiguous then None else Some !winding

type symbolic_operand_classification =
  | Symbolic_winding of int
  | Symbolic_seed_boundary of int
  | Symbolic_source_degenerate of int

let classify_operand_symbolic ?cancel constraints components side query
    skip =
  let triangle_point = match side with
    | Boolean_complex.Left ->
        Boolean_constraints.View.left_triangle_point
    | Boolean_complex.Right ->
        Boolean_constraints.View.right_triangle_point in
  let source = Boolean_constraints.View.source constraints
  and winding = ref 0 and boundary = ref (-1) and degenerate = ref (-1) in
  let component_count = query_components_indexed components query in
  for candidate = 0 to component_count - 1 do
    let component = components.query_candidates.(candidate) in
    for slot = components.offsets.(component)
        to components.offsets.(component + 1) - 1 do
      let face = components.triangles.(slot) in
      Cancel.poll cancel face 4095;
      if not (skip face) then begin
        match Implicit_point.symbolic_ray_source_triangle source
            ~a:(triangle_point constraints face 0)
            ~b:(triangle_point constraints face 1)
            ~c:(triangle_point constraints face 2) query with
        | Implicit_point.Symbolic_miss -> ()
        | Implicit_point.Symbolic_hit normal ->
            winding := checked_add !winding
                (if normal = Predicates.Positive then 1 else -1)
        | Implicit_point.Symbolic_origin_boundary ->
            if !boundary < 0 then boundary := face
        | Implicit_point.Symbolic_degenerate ->
            if !degenerate < 0 then degenerate := face
      end
    done
  done;
  if !degenerate >= 0 then Symbolic_source_degenerate !degenerate
  else if !boundary >= 0 then Symbolic_seed_boundary !boundary
  else Symbolic_winding !winding

exception Ambiguous_classification of int * string

let build ?cancel
    ?(track_left = true) ?(track_right = true) complex weiler =
  try
    Cancel.check_opt cancel;
    if Boolean_weiler.View.complex weiler != complex then
      invalid_arg "Weiler graph belongs to a different Boolean complex";
    let constraints = Boolean_complex.View.constraints complex
    and shells = Boolean_weiler.shell_count weiler
    and facets = Boolean_complex.facet_count complex in
    let left_components, right_components = Parallel.both
        (fun () -> if track_left then
          Some (build_components ?cancel constraints Boolean_complex.Left) else None)
        (fun () -> if track_right then
          Some (build_components ?cancel constraints Boolean_complex.Right) else None) in
    let unknown = min_int and left = Array.make shells min_int
    and right = Array.make shells min_int in
    let counts = Array.make shells 0 in
    for facet = 0 to facets - 1 do
      let negative = Boolean_weiler.half_facet_shell weiler
          (Boolean_weiler.half_facet facet Boolean_weiler.Negative)
      and positive = Boolean_weiler.half_facet_shell weiler
          (Boolean_weiler.half_facet facet Boolean_weiler.Positive) in
      counts.(negative) <- counts.(negative) + 1;
      counts.(positive) <- counts.(positive) + 1
    done;
    let offsets = Array.make (shells + 1) 0 in
    for shell = 0 to shells - 1 do
      offsets.(shell + 1) <- offsets.(shell) + counts.(shell)
    done;
    let incidences = Array.make offsets.(shells) 0 and cursor = Array.copy offsets in
    for facet = 0 to facets - 1 do
      let negative = Boolean_weiler.half_facet_shell weiler
          (Boolean_weiler.half_facet facet Boolean_weiler.Negative)
      and positive = Boolean_weiler.half_facet_shell weiler
          (Boolean_weiler.half_facet facet Boolean_weiler.Positive) in
      incidences.(cursor.(negative)) <- facet * 2;
      cursor.(negative) <- cursor.(negative) + 1;
      incidences.(cursor.(positive)) <- (facet * 2) + 1;
      cursor.(positive) <- cursor.(positive) + 1
    done;
    let queue = Array.make shells 0 and queue_first = ref 0 and queue_last = ref 0 in
    let assign shell left_value right_value =
      if left.(shell) = unknown then begin
        left.(shell) <- left_value; right.(shell) <- right_value;
        queue.(!queue_last) <- shell; incr queue_last
      end else if left.(shell) <> left_value || right.(shell) <> right_value then
        invalid_arg "inconsistent winding propagation across Weiler cells" in
    let propagate () =
      while !queue_first < !queue_last do
        let shell = queue.(!queue_first) in incr queue_first;
        for slot = offsets.(shell) to offsets.(shell + 1) - 1 do
          let incidence = incidences.(slot) in
          let facet = incidence lsr 1
          and current_positive = incidence land 1 = 1 in
          let negative = Boolean_weiler.half_facet_shell weiler
              (Boolean_weiler.half_facet facet Boolean_weiler.Negative)
          and positive = Boolean_weiler.half_facet_shell weiler
              (Boolean_weiler.half_facet facet Boolean_weiler.Positive)
          and delta_left = if track_left then
              Boolean_weiler.facet_left_winding weiler facet else 0
          and delta_right = if track_right then
              Boolean_weiler.facet_right_winding weiler facet else 0 in
          if current_positive then
            assign negative
              (checked_add left.(shell) delta_left)
              (checked_add right.(shell) delta_right)
          else
            assign positive
              (checked_add left.(shell) (-delta_left))
              (checked_add right.(shell) (-delta_right))
        done
      done in
    let seed_component shell =
      let incidence = incidences.(offsets.(shell)) in
      let facet = incidence lsr 1 in
      let a = Boolean_complex.View.vertex complex
          (Boolean_complex.facet_vertex complex facet 0)
      and b = Boolean_complex.View.vertex complex
          (Boolean_complex.facet_vertex complex facet 1)
      and c = Boolean_complex.View.vertex complex
          (Boolean_complex.facet_vertex complex facet 2) in
      let query = match Implicit_point.centroid3 a b c with
        | Ok point -> point
        | Error _ -> invalid_arg "could not construct exact facet centroid" in
      let first_member, last_member = Boolean_complex.facet_member_range complex facet in
      let skip side face =
        let found = ref false and member = ref first_member in
        while not !found && !member < last_member do
          found := Boolean_complex.member_side complex !member = side
              && Boolean_complex.member_face complex !member = face;
          incr member
        done;
        !found in
      let rec choose = function
        | [] ->
            (match (if track_left then
                      classify_operand_symbolic ?cancel constraints
                        (Option.get left_components) Boolean_complex.Left query
                        (skip Boolean_complex.Left)
                    else Symbolic_winding 0),
                  (if track_right then
                     classify_operand_symbolic ?cancel constraints
                       (Option.get right_components) Boolean_complex.Right query
                       (skip Boolean_complex.Right)
                   else Symbolic_winding 0) with
             | Symbolic_winding left_value, Symbolic_winding right_value ->
                 let normal = Implicit_point.normal_dot_symbolic a b c in
                 if normal = Predicates.Zero then
                   raise (Ambiguous_classification (facet,
                     "the refined seed facet is exactly degenerate"))
                 else begin
                   let side = if normal = Predicates.Positive
                       then Boolean_weiler.Positive else Boolean_weiler.Negative in
                   let seed_shell = Boolean_weiler.half_facet_shell weiler
                       (Boolean_weiler.half_facet facet side) in
                   assign seed_shell left_value right_value
                 end
             | Symbolic_seed_boundary face, _ ->
                 raise (Ambiguous_classification (facet,
                   Printf.sprintf
                     "left source triangle %d contains the seed but is not a refined-facet member"
                     face))
             | _, Symbolic_seed_boundary face ->
                 raise (Ambiguous_classification (facet,
                   Printf.sprintf
                     "right source triangle %d contains the seed but is not a refined-facet member"
                     face))
             | Symbolic_source_degenerate face, _ ->
                 raise (Ambiguous_classification (facet,
                   Printf.sprintf
                     "left source triangle %d is exactly degenerate" face))
             | _, Symbolic_source_degenerate face ->
                 raise (Ambiguous_classification (facet,
                   Printf.sprintf
                     "right source triangle %d is exactly degenerate" face)))
        | (axis, positive) :: rest ->
            let normal = (orient2 axis) a b c in
            if normal = Predicates.Zero then choose rest
            else
              match (if track_left then
                       classify_operand ?cancel constraints
                         (Option.get left_components)
                         Boolean_complex.Left query axis ~positive
                         (skip Boolean_complex.Left)
                     else Some 0),
                    (if track_right then
                       classify_operand ?cancel constraints
                         (Option.get right_components)
                         Boolean_complex.Right query axis ~positive
                         (skip Boolean_complex.Right)
                     else Some 0) with
              | Some left_value, Some right_value ->
                  let side = if (normal = Predicates.Positive) = positive
                      then Boolean_weiler.Positive else Boolean_weiler.Negative in
                  let seed_shell = Boolean_weiler.half_facet_shell weiler
                      (Boolean_weiler.half_facet facet side) in
                  assign seed_shell left_value right_value
              | _ -> choose rest in
      choose [X,false; Y,false; Z,false; X,true; Y,true; Z,true] in
    for shell = 0 to shells - 1 do
      if left.(shell) = unknown then begin
        queue_first := 0; queue_last := 0;
        seed_component shell;
        propagate ()
      end
    done;
    Ok { weiler; left; right }
  with
  | Cancel.Cancelled -> error "cancelled" "Boolean cell classification was cancelled"
  | Ambiguous_classification (facet, reason) -> error "ambiguous_classification"
      (Printf.sprintf
         "could not classify refined seed facet %d: %s" facet reason)
  | Invalid_argument message -> error "invalid_complex" message
