
type t = {
  positions : Packed.Float3.View.view;
  order : int array;
  axes : int array;
}

exception Group_error of string

let finite value = Float.is_finite value
let length value = Array.length value.order

let create_raw ?cancel ?(grain = 16_384) ?points:selection packed =
  if grain <= 0 then invalid_arg "grain must be positive";
  let positions = Packed.Float3.View.view packed in
  let count = Packed.Float3.length packed in
  let order = match selection with
    | None -> Array.init count Fun.id
    | Some group ->
        if Group.owner group <> Group.Point then
          raise (Group_error "source selection must be a point group");
        if Group.length group <> count then
          raise (Group_error "source point group length does not match positions");
        let selected = Array.make (Group.cardinality group) 0 and next = ref 0 in
        Group.iter (fun point -> selected.(!next) <- point; incr next) group;
        selected in
  let min_x = ref Float.infinity and min_y = ref Float.infinity
  and min_z = ref Float.infinity and max_x = ref Float.neg_infinity
  and max_y = ref Float.neg_infinity and max_z = ref Float.neg_infinity in
  for slot = 0 to Array.length order - 1 do
    if slot land 16_383 = 0 then Cancel.check_opt cancel;
    let point = order.(slot) in
    if not (finite positions.x.(point) && finite positions.y.(point)
        && finite positions.z.(point)) then
      invalid_arg "source positions must be finite";
    min_x := Float.min !min_x positions.x.(point);
    min_y := Float.min !min_y positions.y.(point);
    min_z := Float.min !min_z positions.z.(point);
    max_x := Float.max !max_x positions.x.(point);
    max_y := Float.max !max_y positions.y.(point);
    max_z := Float.max !max_z positions.z.(point)
  done;
  let ranked_axes = [|
    (!max_x -. !min_x, 0); (!max_y -. !min_y, 1); (!max_z -. !min_z, 2)
  |] in
  Array.sort (fun (left_span, left_axis) (right_span, right_axis) ->
    let compared = Float.compare right_span left_span in
    if compared <> 0 then compared else Int.compare left_axis right_axis)
    ranked_axes;
  let active_count = Array.fold_left (fun count (span, _) ->
    if span > 0. then count + 1 else count) 0 ranked_axes in
  let axes = if active_count = 0 then [|0|]
    else Array.init active_count (fun index -> snd ranked_axes.(index)) in
  let rec partition first last depth =
    if first < last then begin
      Cancel.check_opt cancel;
      let middle = first + ((last - first) / 2) in
      let axis = axes.(depth mod Array.length axes) in
      let coordinates = if axis = 0 then positions.x
        else if axis = 1 then positions.y else positions.z in
      Support.select coordinates order first last middle;
      if last - first + 1 >= grain then
        ignore (Parallel.both
          (fun () -> partition first (middle - 1) (depth + 1))
          (fun () -> partition (middle + 1) last (depth + 1)))
      else begin
        partition first (middle - 1) (depth + 1);
        partition (middle + 1) last (depth + 1)
      end
    end
  in
  partition 0 (Array.length order - 1) 0;
  { positions; order; axes }

let create ?cancel ?grain ?points packed =
  try Ok (create_raw ?cancel ?grain ?points packed) with
  | Cancel.Cancelled -> Error (Error.make ~operation:"spatial_index"
      ~code:"cancelled" "spatial-index construction was cancelled")
  | Invalid_argument message -> Error (Error.make ~operation:"spatial_index"
      ~code:(if String.equal message "grain must be positive"
        then "invalid_parameter" else "invalid_position") message)
  | Group_error message -> Error (Error.make ~operation:"spatial_index"
      ~code:"invalid_group" message)

type query_context = {
  tree : t;
  queries : Packed.Float3.View.view;
  maximum_squared : float;
  indices : int array;
  distances : float array;
  capacity : int;
}

let rec visit context query offset first last depth found =
  if first > last then found
  else
    let tree = context.tree and positions = context.tree.positions in
    let middle = first + ((last - first) / 2) in
    let point = tree.order.(middle) in
    let dx = positions.x.(point) -. context.queries.x.(query)
    and dy = positions.y.(point) -. context.queries.y.(query)
    and dz = positions.z.(point) -. context.queries.z.(query) in
    let distance = (dx *. dx) +. (dy *. dy) +. (dz *. dz) in
    let found = if distance > context.maximum_squared then found
        else
          let last_slot = offset + context.capacity - 1 in
          if found >= context.capacity
             && not (distance < context.distances.(last_slot)
               || (distance = context.distances.(last_slot)
                   && point < context.indices.(last_slot)))
          then found
          else begin
            let position = ref (min found (context.capacity - 1)) in
            while !position > 0
                && (distance < context.distances.(offset + !position - 1)
                  || (distance = context.distances.(offset + !position - 1)
                    && point < context.indices.(offset + !position - 1))) do
              if !position < context.capacity then begin
                context.indices.(offset + !position) <-
                  context.indices.(offset + !position - 1);
                context.distances.(offset + !position) <-
                  context.distances.(offset + !position - 1)
              end;
              decr position
            done;
            context.indices.(offset + !position) <- point;
            context.distances.(offset + !position) <- distance;
            min context.capacity (found + 1)
          end in
    let axis = tree.axes.(depth mod Array.length tree.axes) in
    let delta = if axis = 0 then
        context.queries.x.(query) -. positions.x.(point)
      else if axis = 1 then
        context.queries.y.(query) -. positions.y.(point)
      else context.queries.z.(query) -. positions.z.(point) in
    if delta <= 0. then begin
      let found = visit context query offset first (middle - 1)
          (depth + 1) found in
      let threshold = if found < context.capacity then context.maximum_squared
        else let worst = context.distances.(offset + context.capacity - 1) in
          if context.maximum_squared < worst then context.maximum_squared else worst in
      if (delta *. delta) <= threshold then
        visit context query offset (middle + 1) last (depth + 1) found
      else found
    end else begin
      let found = visit context query offset (middle + 1) last
          (depth + 1) found in
      let threshold = if found < context.capacity then context.maximum_squared
        else let worst = context.distances.(offset + context.capacity - 1) in
          if context.maximum_squared < worst then context.maximum_squared else worst in
      if (delta *. delta) <= threshold then
        visit context query offset first (middle - 1) (depth + 1) found
      else found
    end

let nearest_k_into value ~x ~y ~z ~max_distance_squared ~indices
    ~distances_squared ~offset ~count =
  if count < 0 || offset < 0 || offset > Array.length indices - count
     || offset > Array.length distances_squared - count then
    invalid_arg "Spatial_index.View.nearest_k_into: invalid output slice";
  if count = 0 || length value = 0 then 0
  else
    let queries : Packed.Float3.View.view =
      { x = [|x|]; y = [|y|]; z = [|z|] } in
    let context = { tree = value; queries; maximum_squared = max_distance_squared;
      indices; distances = distances_squared; capacity = count } in
    visit context 0 offset 0 (length value - 1) 0 0

let nearest_k_many_into ?cancel ?points ~grain value ~queries ~max_distance_squared
    ~capacity ~indices ~distances_squared ~counts =
  if grain <= 0 then invalid_arg
      "Spatial_index.View.nearest_k_many_into: grain must be positive";
  let query_count = Packed.Float3.length queries in
  (match points with
   | Some group when Group.owner group <> Group.Point
       || Group.length group <> query_count ->
       invalid_arg "Spatial_index: query selection must be a matching point group"
   | None | Some _ -> ());
  if capacity <= 0 || query_count > Sys.max_array_length / capacity
     || Array.length indices < query_count * capacity
     || Array.length distances_squared < query_count * capacity
     || Array.length counts < query_count then
    invalid_arg "Spatial_index.View.nearest_k_many_into: invalid output storage";
  let context = { tree = value; queries = Packed.Float3.View.view queries;
    maximum_squared = max_distance_squared; indices;
    distances = distances_squared; capacity } in
  if query_count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
      ~finish:(query_count - 1) (fun query ->
        if query land 4095 = 0 then Cancel.check_opt cancel;
        if (match points with None -> true | Some group -> Group.mem query group) then
          counts.(query) <- if length value = 0 then 0 else
            visit context query (query * capacity) 0 (length value - 1) 0 0)

type distance_query_context = {
  distance_tree : t;
  distance_queries : Packed.Float3.View.view;
  distance_maximum_squared : float;
  distance_output : float array;
}

let rec visit_distance_squared context query first last depth =
  if first <= last then begin
    let tree = context.distance_tree and queries = context.distance_queries in
    let positions = tree.positions in
    let middle = first + ((last - first) / 2) in
    let point = tree.order.(middle) in
    let dx = positions.x.(point) -. queries.x.(query)
    and dy = positions.y.(point) -. queries.y.(query)
    and dz = positions.z.(point) -. queries.z.(query) in
    let distance = (dx *. dx) +. (dy *. dy) +. (dz *. dz) in
    if distance <= context.distance_maximum_squared
        && distance < context.distance_output.(query) then
      context.distance_output.(query) <- distance;
    let axis = tree.axes.(depth mod Array.length tree.axes) in
    let delta = if axis = 0 then queries.x.(query) -. positions.x.(point)
      else if axis = 1 then queries.y.(query) -. positions.y.(point)
      else queries.z.(query) -. positions.z.(point) in
    if delta <= 0. then begin
      visit_distance_squared context query first (middle - 1) (depth + 1);
      if (delta *. delta) <= Float.min context.distance_maximum_squared
          context.distance_output.(query) then
        visit_distance_squared context query (middle + 1) last (depth + 1)
    end else begin
      visit_distance_squared context query (middle + 1) last (depth + 1);
      if (delta *. delta) <= Float.min context.distance_maximum_squared
          context.distance_output.(query) then
        visit_distance_squared context query first (middle - 1) (depth + 1)
    end
  end

let nearest_distances_many_into ?cancel ?points ~grain value ~queries
    ~max_distance_squared ~distances_squared =
  if grain <= 0 then invalid_arg
      "Spatial_index.View.nearest_distances_many_into: grain must be positive";
  let query_count = Packed.Float3.length queries in
  (match points with
   | Some group when Group.owner group <> Group.Point
       || Group.length group <> query_count ->
       invalid_arg "Spatial_index: query selection must be a matching point group"
   | None | Some _ -> ());
  if Array.length distances_squared < query_count then invalid_arg
      "Spatial_index.View.nearest_distances_many_into: output array is too short";
  let queries = Packed.Float3.View.view queries in
  let context = { distance_tree = value; distance_queries = queries;
    distance_maximum_squared = max_distance_squared;
    distance_output = distances_squared } in
  if query_count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
      ~finish:(query_count - 1) (fun query ->
        if query land 4095 = 0 then Cancel.check_opt cancel;
        if match points with None -> true | Some group -> Group.mem query group
        then begin
          distances_squared.(query) <- Float.infinity;
          if length value > 0 then
            visit_distance_squared context query 0 (length value - 1) 0
        end)

module View = struct
  let nearest_k_into = nearest_k_into
  let nearest_k_many_into = nearest_k_many_into
  let nearest_distances_many_into = nearest_distances_many_into
end
