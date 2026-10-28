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

let next_power_of_two value =
  let result = ref 8 in
  while !result < value do
    result := !result lsl 1
  done;
  !result

let[@inline always] integer_hash x y z =
  let value = x * 73_856_093 lxor (y * 19_349_663) lxor (z * 83_492_791) in
  value lxor (value lsr 16) land max_int

let[@inline always] float_cell value =
  if value = 0. then 0
  else
    let bits = Int64.bits_of_float value in
    Int64.to_int (Int64.logxor bits (Int64.shift_right_logical bits 32))

let create ~cells ~points =
  let capacity = next_power_of_two (max 8 (cells * 2)) in
  {
    cell_x = Array.make cells 0;
    cell_y = Array.make cells 0;
    cell_z = Array.make cells 0;
    cell_head = Array.make cells (-1);
    table = Array.make capacity (-1);
    point_next = Array.make points (-1);
    mask = capacity - 1;
    cell_count = 0;
  }

(* The table slot holding cell (x, y, z), or the empty slot where it belongs. *)
let slot grid x y z =
  let slot = ref (integer_hash x y z land grid.mask) in
  while
    grid.table.(!slot) >= 0
    &&
    let cell = grid.table.(!slot) in
    grid.cell_x.(cell) <> x || grid.cell_y.(cell) <> y || grid.cell_z.(cell) <> z
  do
    slot := (!slot + 1) land grid.mask
  done;
  !slot

let find grid x y z = grid.table.(slot grid x y z)

let find_or_add grid x y z =
  let slot = slot grid x y z in
  if grid.table.(slot) >= 0 then grid.table.(slot)
  else begin
    let cell = grid.cell_count in
    grid.cell_count <- cell + 1;
    grid.table.(slot) <- cell;
    grid.cell_x.(cell) <- x;
    grid.cell_y.(cell) <- y;
    grid.cell_z.(cell) <- z;
    cell
  end

let insert grid x y z point =
  let cell = find_or_add grid x y z in
  grid.point_next.(point) <- grid.cell_head.(cell);
  grid.cell_head.(cell) <- point
