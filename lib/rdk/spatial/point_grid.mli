(** Open-addressing uniform grid over integer cells; each cell heads a
    linked list of points, newest first. Callers may reuse the cell arrays as
    scratch once the grid is no longer queried. *)

type t = {
  cell_x : int array;
  cell_y : int array;
  cell_z : int array;
  cell_head : int array;
  table : int array;
  point_next : int array;
  mask : int;
  mutable cell_count : int;
}

val create : cells:int -> points:int -> t
(** Room for [cells] distinct cells and point numbers below [points]. *)

val float_cell : float -> int
(** Exact cell coordinate of a float (zero maps to zero). *)

val find : t -> int -> int -> int -> int
(** The cell index at (x, y, z), or [-1]. *)

val insert : t -> int -> int -> int -> int -> unit
(** [insert grid x y z point] links [point] into the cell, creating it. *)
