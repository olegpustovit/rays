
type t = {
  order : int array;
  min_x : float array;
  min_y : float array;
  min_z : float array;
  max_x : float array;
  max_y : float array;
  max_z : float array;
  left : int array;
  right : int array;
  first : int array;
  count : int array;
}

let leaf_size = 8

let create ?cancel ~grain ~centroid_x ~centroid_y ~centroid_z
    ~item_min_x ~item_min_y ~item_min_z ~item_max_x ~item_max_y ~item_max_z () =
  let total = Array.length centroid_x in
  let subtree_nodes = Array.make (total + 1) 0 in
  for size = 1 to total do
    subtree_nodes.(size) <- if size <= leaf_size then 1 else
      let left_size = size / 2 in
      1 + subtree_nodes.(left_size) + subtree_nodes.(size - left_size)
  done;
  let capacity = subtree_nodes.(total) in
  let min_x = Array.make capacity 0. and min_y = Array.make capacity 0.
  and min_z = Array.make capacity 0. and max_x = Array.make capacity 0.
  and max_y = Array.make capacity 0. and max_z = Array.make capacity 0.
  and left = Array.make capacity (-1) and right = Array.make capacity (-1)
  and first = Array.make capacity 0 and count = Array.make capacity 0
  and order = Array.init total Fun.id in
  let rec build node range_first range_last =
    Cancel.check_opt cancel;
    min_x.(node) <- Float.infinity; min_y.(node) <- Float.infinity;
    min_z.(node) <- Float.infinity; max_x.(node) <- Float.neg_infinity;
    max_y.(node) <- Float.neg_infinity; max_z.(node) <- Float.neg_infinity;
    for at = range_first to range_last do
      let item = order.(at) in
      if item_min_x.(item) < min_x.(node) then min_x.(node) <- item_min_x.(item);
      if item_min_y.(item) < min_y.(node) then min_y.(node) <- item_min_y.(item);
      if item_min_z.(item) < min_z.(node) then min_z.(node) <- item_min_z.(item);
      if item_max_x.(item) > max_x.(node) then max_x.(node) <- item_max_x.(item);
      if item_max_y.(item) > max_y.(node) then max_y.(node) <- item_max_y.(item);
      if item_max_z.(item) > max_z.(node) then max_z.(node) <- item_max_z.(item)
    done;
    let range_count = range_last - range_first + 1 in
    if range_count <= leaf_size then begin
      first.(node) <- range_first; count.(node) <- range_count
    end else begin
      let ex = max_x.(node) -. min_x.(node)
      and ey = max_y.(node) -. min_y.(node)
      and ez = max_z.(node) -. min_z.(node) in
      let axis = if ex >= ey && ex >= ez then 0 else if ey >= ez then 1 else 2 in
      let middle = range_first + (range_count / 2) in
      Support.select
        (if axis = 0 then centroid_x else if axis = 1 then centroid_y else centroid_z)
        order range_first range_last middle;
      let left_size = middle - range_first in
      let left_node = node + 1
      and right_node = node + 1 + subtree_nodes.(left_size) in
      left.(node) <- left_node; right.(node) <- right_node;
      if range_count / 2 >= grain then ignore (Parallel.both
        (fun () -> build left_node range_first (middle - 1))
        (fun () -> build right_node middle range_last))
      else begin
        build left_node range_first (middle - 1);
        build right_node middle range_last
      end
    end in
  if total > 0 then build 0 0 (total - 1);
  { order; min_x; min_y; min_z; max_x; max_y; max_z; left; right; first; count }
