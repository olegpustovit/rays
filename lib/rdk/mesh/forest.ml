
(* Tree scan over a parent array: the shared core of Edge Transport Parent and
   the Lisp [graph/forest]. *)

type t = {
  tree_parent : int array;
  child_offsets : int array;
  children : int array;
  order : int array;
  order_count : int;
  root_offsets : int array;
  root_count : int;
}

let scan ?selected ~selected_count parents =
  let cancel = None in let grain = (16_384) in let point_count = Array.length parents in
  let tree_parent = match selected with
    | None ->
        let needs_normalization = Atomic.make false in
        Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(point_count - 1)
          (fun point ->
            if point land 4095 = 0 then Cancel.check_opt cancel;
            let parent = parents.(point) in
            if parent < 0 || parent >= point_count then
              Atomic.set needs_normalization true);
        if not (Atomic.get needs_normalization) then parents
        else begin
          let normalized = Array.make point_count 0 in
          Parallel.for_ ~chunk_size:grain ~start:0
            ~finish:(point_count - 1) (fun point ->
              let parent = parents.(point) in
              normalized.(point) <- if parent < 0 || parent >= point_count
                then point else parent);
          normalized
        end
    | Some selected ->
        let normalized = Array.make point_count (-1) in
        Parallel.for_ ~chunk_size:grain ~start:0
          ~finish:(point_count - 1) (fun point ->
            if point land 4095 = 0 then Cancel.check_opt cancel;
            if selected point then begin
              let parent = parents.(point) in
              normalized.(point) <- if parent < 0 || parent >= point_count
                  || parent = point || not (selected parent)
                then point else parent
            end);
        normalized in
  let child_count = Array.make point_count 0 and root_count = ref 0 in
  for point = 0 to point_count - 1 do
    let parent = tree_parent.(point) in
    if parent = point then incr root_count
    else if parent >= 0 then child_count.(parent) <- child_count.(parent) + 1
  done;
  let child_offsets = Array.make (point_count + 1) 0 in
  for point = 0 to point_count - 1 do
    child_offsets.(point + 1) <- child_offsets.(point) + child_count.(point)
  done;
  let children = Array.make child_offsets.(point_count) 0 in
  Array.fill child_count 0 point_count 0;
  for point = 0 to point_count - 1 do
    let parent = tree_parent.(point) in
    if parent >= 0 && parent <> point then begin
      let target = child_offsets.(parent) + child_count.(parent) in
      children.(target) <- point;
      child_count.(parent) <- child_count.(parent) + 1
    end
  done;
  let order = Array.make selected_count 0
  and root_offsets = Array.make (!root_count + 1) 0 in
  let order_count = ref 0 and root_index = ref 0 in
  for root = 0 to point_count - 1 do
    if tree_parent.(root) = root then begin
      let first = !order_count in
      root_offsets.(!root_index) <- first;
      order.(!order_count) <- root; incr order_count;
      let read = ref first in
      while !read < !order_count do
        let point = order.(!read) in
        for at = child_offsets.(point) to child_offsets.(point + 1) - 1 do
          order.(!order_count) <- children.(at); incr order_count
        done;
        incr read
      done;
      incr root_index;
      root_offsets.(!root_index) <- !order_count
    end
  done;
  { tree_parent; child_offsets; children; order; order_count = !order_count;
    root_offsets; root_count = !root_count }

let labels forest =
  let point_count = Array.length forest.tree_parent in
  let depth = Array.make point_count (-1) and root = Array.make point_count (-1) in
  for tree = 0 to forest.root_count - 1 do
    for at = forest.root_offsets.(tree) to forest.root_offsets.(tree + 1) - 1 do
      let point = forest.order.(at) in
      let parent = forest.tree_parent.(point) in
      if parent = point then begin
        depth.(point) <- 0; root.(point) <- point
      end else begin
        depth.(point) <- depth.(parent) + 1; root.(point) <- root.(parent)
      end
    done
  done;
  depth, root
