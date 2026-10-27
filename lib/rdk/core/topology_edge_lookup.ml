type t = {
  point_count : int;
  edge_a : int array;
  edge_b : int array;
  slots : int array;
}

let next_power_of_two value =
  let result = ref 8 in
  while !result < value do
    if !result > Sys.max_array_length / 2 then
      invalid_arg "Topology edge lookup exceeds array limits";
    result := !result lsl 1
  done;
  !result

let[@inline always] hash a b =
  let value = Rays_math.Hash.mix a (b * 31_337) in
  (value lxor (value lsr 16)) land max_int

let create ?cancel (topology : Topology.Private.view) =
  let outgoing = ref 0 in
  for primitive = 0 to Bytes.length topology.primitive_kinds - 1 do
    if primitive land 4_095 = 0 then Cancel.check_opt cancel;
    let size = topology.primitive_offsets.(primitive + 1)
        - topology.primitive_offsets.(primitive) in
    outgoing := !outgoing +
      if Bytes.get topology.primitive_kinds primitive = '\001'
      then max 0 (size - 1) else size
  done;
  if !outgoing > Sys.max_array_length / 2 then
    invalid_arg "Topology edge lookup exceeds array limits";
  let slots = Array.make (next_power_of_two (max 8 (!outgoing * 2))) (-1)
  and edge_a = Array.make !outgoing 0
  and edge_b = Array.make !outgoing 0 in
  let edge_count = ref 0 and mask = Array.length slots - 1 in
  let add a b =
    let a, b = if a <= b then a, b else b, a in
    let slot = ref (hash a b land mask) in
    while slots.(!slot) >= 0
        && (let edge = slots.(!slot) in
            edge_a.(edge) <> a || edge_b.(edge) <> b) do
      slot := (!slot + 1) land mask
    done;
    if slots.(!slot) < 0 then begin
      let edge = !edge_count in
      slots.(!slot) <- edge;
      edge_a.(edge) <- a;
      edge_b.(edge) <- b;
      incr edge_count
    end in
  for primitive = 0 to Bytes.length topology.primitive_kinds - 1 do
    if primitive land 4_095 = 0 then Cancel.check_opt cancel;
    let first = topology.primitive_offsets.(primitive)
    and last = topology.primitive_offsets.(primitive + 1) in
    for vertex = first to last - 1 do
      let next = if vertex + 1 < last then vertex + 1
        else if Bytes.get topology.primitive_kinds primitive = '\001'
        then -1 else first in
      if next >= 0 then
        add topology.vertex_points.(vertex) topology.vertex_points.(next)
    done
  done;
  { point_count = topology.point_count;
    edge_a = Array.sub edge_a 0 !edge_count;
    edge_b = Array.sub edge_b 0 !edge_count;
    slots }

let count value = Array.length value.edge_a
let endpoints value edge = value.edge_a.(edge), value.edge_b.(edge)

let find value ~a ~b =
  if a < 0 || b < 0 || a >= value.point_count || b >= value.point_count
  then -1
  else
    let a, b = if a <= b then a, b else b, a in
    let mask = Array.length value.slots - 1 in
    let slot = ref (hash a b land mask) and result = ref (-1)
    and searching = ref true in
    while !searching do
      let edge = value.slots.(!slot) in
      if edge < 0 then searching := false
      else if value.edge_a.(edge) = a && value.edge_b.(edge) = b then begin
        result := edge;
        searching := false
      end else slot := (!slot + 1) land mask
    done;
    !result
