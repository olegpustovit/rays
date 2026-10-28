type t = {
  order : int array;
  item_min_x : float array;
  item_min_y : float array;
  item_max_x : float array;
  item_max_y : float array;
  min_x : float array;
  min_y : float array;
  max_x : float array;
  max_y : float array;
  left : int array;
  right : int array;
  first : int array;
  count : int array;
  mutable nodes : int;
}

let midpoint lower upper =
  if Float.is_finite lower && Float.is_finite upper then (lower *. 0.5) +. (upper *. 0.5)
  else if lower = Float.neg_infinity && upper = Float.infinity then 0.
  else if upper = Float.infinity then max_float
  else if lower = Float.neg_infinity then -.max_float
  else 0.

let create ?cancel ~min_x:item_min_x ~min_y:item_min_y ~max_x:item_max_x ~max_y:item_max_y () =
  let items = Array.length item_min_x in
  if
    Array.length item_min_y <> items
    || Array.length item_max_x <> items
    || Array.length item_max_y <> items
  then invalid_arg "Bounds2_index: bound plane sizes differ";
  if items > Sys.max_array_length / 2 then
    invalid_arg "Bounds2_index: item cardinality exceeds array limits";
  for item = 0 to items - 1 do
    Cancel.poll cancel item 16_383;
    if
      Float.is_nan item_min_x.(item)
      || Float.is_nan item_min_y.(item)
      || Float.is_nan item_max_x.(item)
      || Float.is_nan item_max_y.(item)
      || item_min_x.(item) > item_max_x.(item)
      || item_min_y.(item) > item_max_y.(item)
    then invalid_arg "Bounds2_index: malformed bound"
  done;
  let capacity = max 1 (items * 2) in
  let tree =
    {
      order = Array.init items Fun.id;
      item_min_x;
      item_min_y;
      item_max_x;
      item_max_y;
      min_x = Array.make capacity 0.;
      min_y = Array.make capacity 0.;
      max_x = Array.make capacity 0.;
      max_y = Array.make capacity 0.;
      left = Array.make capacity (-1);
      right = Array.make capacity (-1);
      first = Array.make capacity 0;
      count = Array.make capacity 0;
      nodes = 0;
    }
  in
  let cx = Array.init items (fun item -> midpoint item_min_x.(item) item_max_x.(item))
  and cy = Array.init items (fun item -> midpoint item_min_y.(item) item_max_y.(item)) in
  let global_min_x = ref infinity
  and global_min_y = ref infinity
  and global_max_x = ref neg_infinity
  and global_max_y = ref neg_infinity in
  for item = 0 to items - 1 do
    global_min_x := min !global_min_x item_min_x.(item);
    global_min_y := min !global_min_y item_min_y.(item);
    global_max_x := max !global_max_x item_max_x.(item);
    global_max_y := max !global_max_y item_max_y.(item)
  done;
  let root_axis =
    if !global_max_x -. !global_min_x >= !global_max_y -. !global_min_y then 0 else 1
  in
  let rec build depth first count =
    Cancel.check_opt cancel;
    let node = tree.nodes in
    tree.nodes <- node + 1;
    tree.first.(node) <- first;
    tree.count.(node) <- count;
    if count > 8 then begin
      let axis = (root_axis + depth) land 1 in
      let left_count = count / 2 in
      let middle = first + left_count in
      Support.select ?cancel
        (if axis = 0 then cx else cy)
        tree.order first
        (first + count - 1)
        middle;
      let left = build (depth + 1) first left_count in
      let right = build (depth + 1) middle (count - left_count) in
      tree.left.(node) <- left;
      tree.right.(node) <- right;
      tree.min_x.(node) <- min tree.min_x.(left) tree.min_x.(right);
      tree.min_y.(node) <- min tree.min_y.(left) tree.min_y.(right);
      tree.max_x.(node) <- max tree.max_x.(left) tree.max_x.(right);
      tree.max_y.(node) <- max tree.max_y.(left) tree.max_y.(right)
    end
    else begin
      let min_x = ref infinity
      and min_y = ref infinity
      and max_x = ref neg_infinity
      and max_y = ref neg_infinity in
      for slot = first to first + count - 1 do
        let item = tree.order.(slot) in
        min_x := min !min_x item_min_x.(item);
        min_y := min !min_y item_min_y.(item);
        max_x := max !max_x item_max_x.(item);
        max_y := max !max_y item_max_y.(item)
      done;
      tree.min_x.(node) <- !min_x;
      tree.min_y.(node) <- !min_y;
      tree.max_x.(node) <- !max_x;
      tree.max_y.(node) <- !max_y
    end;
    node
  in
  if items > 0 then ignore (build 0 0 items);
  tree

let node_overlap tree node ~min_x ~min_y ~max_x ~max_y =
  tree.max_x.(node) >= min_x
  && max_x >= tree.min_x.(node)
  && tree.max_y.(node) >= min_y
  && max_y >= tree.min_y.(node)

let item_overlap tree item ~min_x ~min_y ~max_x ~max_y =
  tree.item_max_x.(item) >= min_x
  && max_x >= tree.item_min_x.(item)
  && tree.item_max_y.(item) >= min_y
  && max_y >= tree.item_min_y.(item)

let query ?cancel tree ~min_x ~min_y ~max_x ~max_y visit =
  if
    Float.is_nan min_x || Float.is_nan min_y || Float.is_nan max_x || Float.is_nan max_y
    || min_x > max_x || min_y > max_y
  then invalid_arg "Bounds2_index.query: malformed bound";
  let visited = ref 0 in
  let rec node index =
    incr visited;
    Cancel.poll cancel !visited 4095;
    if node_overlap tree index ~min_x ~min_y ~max_x ~max_y then
      if tree.left.(index) < 0 then begin
        let last = tree.first.(index) + tree.count.(index) in
        for slot = tree.first.(index) to last - 1 do
          let item = tree.order.(slot) in
          if item_overlap tree item ~min_x ~min_y ~max_x ~max_y then visit item
        done
      end
      else begin
        node tree.left.(index);
        node tree.right.(index)
      end
  in
  if tree.nodes > 0 then node 0

let node_pair_overlap tree left right =
  tree.max_x.(left) >= tree.min_x.(right)
  && tree.max_x.(right) >= tree.min_x.(left)
  && tree.max_y.(left) >= tree.min_y.(right)
  && tree.max_y.(right) >= tree.min_y.(left)

let source_pair_overlap tree left right =
  tree.item_max_x.(left) >= tree.item_min_x.(right)
  && tree.item_max_x.(right) >= tree.item_min_x.(left)
  && tree.item_max_y.(left) >= tree.item_min_y.(right)
  && tree.item_max_y.(right) >= tree.item_min_y.(left)

let candidate_pairs ?cancel tree visit =
  let visited_nodes = ref 0 in
  let rec pairs left right =
    incr visited_nodes;
    Cancel.poll cancel !visited_nodes 4095;
    if not (node_pair_overlap tree left right) then ()
    else if left = right then
      if tree.left.(left) < 0 then begin
        let first = tree.first.(left) in
        let last = first + tree.count.(left) in
        for a = first to last - 1 do
          for b = a + 1 to last - 1 do
            let left = tree.order.(a) and right = tree.order.(b) in
            if source_pair_overlap tree left right then visit left right
          done
        done
      end
      else begin
        pairs tree.left.(left) tree.left.(left);
        pairs tree.left.(left) tree.right.(left);
        pairs tree.right.(left) tree.right.(left)
      end
    else if tree.left.(left) < 0 && tree.left.(right) < 0 then begin
      let left_last = tree.first.(left) + tree.count.(left)
      and right_last = tree.first.(right) + tree.count.(right) in
      for a = tree.first.(left) to left_last - 1 do
        for b = tree.first.(right) to right_last - 1 do
          let first = tree.order.(a) and second = tree.order.(b) in
          if source_pair_overlap tree first second then
            if first < second then visit first second else visit second first
        done
      done
    end
    else if
      tree.left.(right) < 0 || (tree.left.(left) >= 0 && tree.count.(left) >= tree.count.(right))
    then begin
      pairs tree.left.(left) right;
      pairs tree.right.(left) right
    end
    else begin
      pairs left tree.left.(right);
      pairs left tree.right.(right)
    end
  in
  if tree.nodes > 0 then begin
    Cancel.check_opt cancel;
    pairs 0 0
  end
