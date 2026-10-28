(** Breadth-first tree scan over a parent array. *)

type t = {
  tree_parent : int array;
  (** Normalised parents: a root is its own parent, an unselected point is
      [-1]. *)
  child_offsets : int array;
  children : int array;
  (** Children CSR over every non-root selected point, in index order
      (members of parent cycles included). *)
  order : int array;
  (** First [order_count] entries: roots by increasing index, each followed
      by its tree breadth first. Cycle members and unselected points are
      absent; the array may be longer than [order_count]. *)
  order_count : int;
  root_offsets : int array;
  (** Start of each tree in [order]; [root_count + 1] entries. *)
  root_count : int;
}

type params = {
  selected : (int -> bool) option;
  selected_count : int;
}

val scan : params -> int array -> t
(** [scan ~selected_count parents]. A parent that is out of range, the point
    itself or (with [selected]) unselected makes the point a root.
    [selected_count] is the number of selected points, sizing [order]. *)

val labels : t -> int array * int array
(** [(depth, root)] per point: hops from the tree root and the root's point
    index; both [-1] for points outside every tree. *)
