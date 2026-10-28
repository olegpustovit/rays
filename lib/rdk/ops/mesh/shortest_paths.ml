(* Heap-ordered single-source-set search over point/edge adjacency. Ties on
   distance are broken by the lower point index, so the pop order, the
   distances and the parents are a pure function of the inputs. *)

type search_params = {
  cancel : Cancel.t option;
  radius : float;
  point_edge_offsets : int array;
  point_edges : int array;
  edge_a : int array;
  edge_b : int array;
  lengths : float array;
}

let search ({ cancel; radius; point_edge_offsets; point_edges; edge_a; edge_b; lengths } : search_params) roots =
  let count = Array.length point_edge_offsets - 1 in
  let distances = Array.make count infinity and parents = Array.make count (-1) in
  (* An indexed decrease-key heap admits each point at most once. [positions]
     is -1 before insertion, a heap slot while queued, and -2 after pop. *)
  let heap_points = Array.make count 0 and heap_positions = Array.make count (-1)
  and heap_size = ref 0 in
  let less_point left right = distances.(left) < distances.(right)
      || (distances.(left) = distances.(right) && left < right) in
  let assign slot point =
    heap_points.(slot) <- point; heap_positions.(point) <- slot in
  let rec bubble_up slot point =
    if slot = 0 then assign 0 point
    else
      let parent = (slot - 1) / 2 and parent_point = heap_points.((slot - 1) / 2) in
      if less_point point parent_point then begin
        assign slot parent_point;
        bubble_up parent point
      end else assign slot point in
  let enqueue_or_decrease point =
    let slot = heap_positions.(point) in
    if slot = -1 then begin
      let slot = !heap_size in
      incr heap_size;
      bubble_up slot point
    end else if slot >= 0 then bubble_up slot point in
  let rec sift_down slot point =
    let left = (slot * 2) + 1 in
    if left >= !heap_size then assign slot point
    else
      let right = left + 1 in
      let child = if right < !heap_size
          && less_point heap_points.(right) heap_points.(left)
        then right else left in
      let child_point = heap_points.(child) in
      if less_point child_point point then begin
        assign slot child_point;
        sift_down child point
      end else assign slot point in
  let pop () =
    let point = heap_points.(0) in
    heap_positions.(point) <- -2;
    decr heap_size;
    if !heap_size > 0 then sift_down 0 heap_points.(!heap_size);
    point in
  Array.iteri (fun index point ->
    Cancel.poll cancel index 16_383;
    distances.(point) <- 0.;
    enqueue_or_decrease point) roots;
  let visits = ref 0 in
  while !heap_size > 0 do
    Cancel.poll cancel !visits 16_383;
    incr visits;
    let point = pop () in
    let distance = distances.(point) in
    if distance <= radius then
      for slot = point_edge_offsets.(point) to point_edge_offsets.(point + 1) - 1 do
        let edge = point_edges.(slot) in
        let a = edge_a.(edge) and b = edge_b.(edge) in
        let neighbor = if a = point then b else a in
        if neighbor <> point then begin
          let candidate = distance +. lengths.(edge) in
          if candidate <= radius && candidate < distances.(neighbor) then begin
            distances.(neighbor) <- candidate;
            parents.(neighbor) <- point;
            enqueue_or_decrease neighbor
          end
        end
      done
  done;
  distances, parents

(* Hop-first search: layer-synchronous breadth-first levels, then the smaller
   accumulated length, then the lower predecessor index. Every point of layer d
   is settled before layer d+1 is read, so results do not depend on the
   queue order within a layer. This is the one implementation behind
   [Group_path] and the Lisp [graph/shortest_paths :hops true]. *)
exception Hop_overflow

type hop_search_params = {
  blocked_point : (int -> bool);
  blocked_edge : (int -> bool);
  target : int;
  depths : int array;
  distances : float array;
  previous : int array;
  queue : int array;
  point_edge_offsets : int array;
  point_edges : int array;
  edge_a : int array;
  edge_b : int array;
  lengths : float array;
}

let hop_search ({ blocked_point; blocked_edge; target; depths; distances; previous; queue; point_edge_offsets; point_edges; edge_a; edge_b; lengths } : hop_search_params) roots =
  let cancel = None in let count = Array.length point_edge_offsets - 1 in
  Array.fill depths 0 count (-1);
  let head = ref 0 and tail = ref 0 and overflow = ref false in
  Array.iter (fun root ->
    if depths.(root) < 0 then begin
      depths.(root) <- 0;
      distances.(root) <- 0.;
      previous.(root) <- -1;
      queue.(!tail) <- root;
      incr tail
    end) roots;
  while !head < !tail
      && (target < 0 || depths.(target) < 0
          || depths.(queue.(!head)) < depths.(target)) do
    let element = queue.(!head) in
    Cancel.poll cancel !head 4095;
    incr head;
    for slot = point_edge_offsets.(element) to point_edge_offsets.(element + 1) - 1 do
      let edge = point_edges.(slot) in
      if not (blocked_edge edge) then begin
        let a = edge_a.(edge) and b = edge_b.(edge) in
        let next = if a = element then b else a in
        if next <> element && (next = target || not (blocked_point next)) then begin
          let next_depth = depths.(element) + 1
          and next_distance = distances.(element) +. lengths.(edge) in
          if not (Float.is_finite next_distance) then overflow := true
          else if depths.(next) < 0
              || next_depth < depths.(next)
              || (next_depth = depths.(next)
                  && (next_distance < distances.(next)
                      || (next_distance = distances.(next)
                          && element < previous.(next)))) then begin
            if depths.(next) < 0 then begin
              queue.(!tail) <- next;
              incr tail
            end;
            depths.(next) <- next_depth;
            distances.(next) <- next_distance;
            previous.(next) <- element
          end
        end
      end
    done
  done;
  if !overflow then raise Hop_overflow

(* The Edge Transport shortest-path forest: roots are numbered by their own point
   index, the heap orders by (distance, root, point), and at an equal distance a
   still unsettled neighbour takes the smaller root and then the smaller parent.
   Shared by [Edge_transport] and the Lisp [graph/shortest_paths :ties "lowest"]. *)
type forest = {
  distance : float array;
  parent : int array;
  edge : int array;
  root : int array;
  order : int array;
  order_count : int;
}

let forest_search ~valid_edge ~capacity
    ~point_edge_offsets ~point_edges ~edge_a ~edge_b ~lengths roots =
  let cancel = None in let drain_each = (false) in let point_count = Array.length point_edge_offsets - 1 in
  let distance = Array.make point_count infinity
  and tree_parent = Array.make point_count (-1)
  and tree_edge = Array.make point_count (-1)
  and tree_root = Array.make point_count (-1)
  and heap_points = Array.make point_count 0
  and heap_positions = Array.make point_count (-1)
  and order = Array.make capacity 0 in
  let heap_size = ref 0 and order_count = ref 0 in
  let less left right =
    distance.(left) < distance.(right)
    || (distance.(left) = distance.(right)
        && (tree_root.(left) < tree_root.(right)
            || (tree_root.(left) = tree_root.(right) && left < right))) in
  let assign slot point =
    heap_points.(slot) <- point; heap_positions.(point) <- slot in
  let rec bubble slot point =
    if slot = 0 then assign 0 point
    else let parent_slot = (slot - 1) / 2 in
      let parent = heap_points.(parent_slot) in
      if less point parent then begin
        assign slot parent; bubble parent_slot point
      end else assign slot point in
  let enqueue point =
    let slot = heap_positions.(point) in
    if slot = -1 then begin
      let slot = !heap_size in incr heap_size; bubble slot point
    end else if slot >= 0 then bubble slot point in
  let rec sift_down slot point =
    let left = slot * 2 + 1 in
    if left >= !heap_size then assign slot point
    else begin
      let right = left + 1 in
      let child = if right < !heap_size
          && less heap_points.(right) heap_points.(left) then right else left in
      let child_point = heap_points.(child) in
      if less child_point point then begin
        assign slot child_point; sift_down child point
      end else assign slot point
    end in
  let pop () =
    let point = heap_points.(0) in
    heap_positions.(point) <- -2; decr heap_size;
    if !heap_size > 0 then sift_down 0 heap_points.(!heap_size);
    point in
  let add_root point =
    if distance.(point) = infinity then begin
      distance.(point) <- 0.; tree_parent.(point) <- point;
      tree_root.(point) <- point; enqueue point
    end in
  let visits = ref 0 in
  let drain_heap () =
    while !heap_size > 0 do
      Cancel.poll cancel !visits 4095;
      incr visits;
      let point = pop () in
      order.(!order_count) <- point; incr order_count;
      for at = point_edge_offsets.(point) to point_edge_offsets.(point + 1) - 1 do
        let edge = point_edges.(at) in
        if valid_edge edge then begin
          let a = edge_a.(edge) and b = edge_b.(edge) in
          let neighbor = if a = point then b else a in
          if heap_positions.(neighbor) <> -2 then begin
            let candidate = distance.(point) +. lengths.(edge) in
            let better = candidate < distance.(neighbor)
              || (candidate = distance.(neighbor)
                  && (tree_root.(point) < tree_root.(neighbor)
                      || (tree_root.(point) = tree_root.(neighbor)
                          && point < tree_parent.(neighbor)))) in
            if better then begin
              distance.(neighbor) <- candidate;
              tree_parent.(neighbor) <- point;
              tree_edge.(neighbor) <- edge;
              tree_root.(neighbor) <- tree_root.(point);
              enqueue neighbor
            end
          end
        end
      done
    done in
  Array.iter (fun root -> add_root root; if drain_each then drain_heap ()) roots;
  drain_heap ();
  { distance; parent = tree_parent; edge = tree_edge; root = tree_root; order;
    order_count = !order_count }
