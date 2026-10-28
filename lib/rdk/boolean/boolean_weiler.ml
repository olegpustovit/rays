type side = Negative | Positive

type t = {
  complex : Boolean_complex.t;
  shells : int array;
  shell_count : int;
  left_winding : int array;
  right_winding : int array;
}

let operation = "boolean_weiler"
let error code message = Error (Error.make ~operation ~code message)
let half_facet facet side = (facet * 2) + match side with Negative -> 0 | Positive -> 1
let shell_count value = value.shell_count
let half_facet_shell value half_facet = value.shells.(half_facet)
let facet_left_winding value facet = value.left_winding.(facet)
let facet_right_winding value facet = value.right_winding.(facet)

module View = struct
  let complex value = value.complex
end

let find parent value =
  let root = ref value in
  while parent.(!root) <> !root do
    root := parent.(!root)
  done;
  let cursor = ref value in
  while parent.(!cursor) <> !cursor do
    let next = parent.(!cursor) in
    parent.(!cursor) <- !root;
    cursor := next
  done;
  !root

let unite parent left right =
  let left = find parent left and right = find parent right in
  if left <> right then if left < right then parent.(right) <- left else parent.(left) <- right

let build ?cancel complex radial =
  try
    Cancel.check_opt cancel;
    if Boolean_radial.View.complex radial != complex then
      invalid_arg "radial order belongs to a different Boolean complex";
    let facets = Boolean_complex.facet_count complex in
    if facets > Sys.max_array_length / 6 then
      invalid_arg "Weiler half-facet cardinality exceeds array limits";
    let half_facets = facets * 2 in
    let neighbors = Array.make (half_facets * 3) (-1) and parent = Array.init half_facets Fun.id in
    let set_neighbor half local neighbor =
      let slot = (half * 3) + local in
      if neighbors.(slot) >= 0 && neighbors.(slot) <> neighbor then
        invalid_arg "radial cycle assigned two neighbors to one half-edge";
      neighbors.(slot) <- neighbor
    in
    for edge = 0 to Boolean_complex.edge_count complex - 1 do
      Cancel.poll cancel edge 255;
      let first, last = Boolean_radial.incident_range radial edge in
      let count = last - first in
      if count = 0 then invalid_arg "complex edge has no radial chart";
      let edge_first = Boolean_complex.edge_first complex edge
      and edge_second = Boolean_complex.edge_second complex edge in
      let aligned facet local =
        let first = Boolean_complex.facet_vertex complex facet local
        and second = Boolean_complex.facet_vertex complex facet ((local + 1) mod 3) in
        if first = edge_first && second = edge_second then true
        else if first = edge_second && second = edge_first then false
        else invalid_arg "radial chart local edge does not match complex edge"
      in
      for index = 0 to count - 1 do
        let next_index = (index + 1) mod count in
        let current_slot = first + index and next_slot = first + next_index in
        let current_facet = Boolean_radial.incident_facet radial current_slot
        and current_local = Boolean_radial.incident_local radial current_slot
        and next_facet = Boolean_radial.incident_facet radial next_slot
        and next_local = Boolean_radial.incident_local radial next_slot in
        let current_side = if aligned current_facet current_local then Positive else Negative
        and next_side = if aligned next_facet next_local then Negative else Positive in
        let current_half = half_facet current_facet current_side
        and next_half = half_facet next_facet next_side in
        set_neighbor current_half current_local next_half;
        set_neighbor next_half next_local current_half;
        unite parent current_half next_half
      done
    done;
    for slot = 0 to Array.length neighbors - 1 do
      if neighbors.(slot) < 0 then invalid_arg "Weiler half-edge has no radial neighbor"
    done;
    for half = 0 to half_facets - 1 do
      parent.(half) <- find parent half
    done;
    let shell_of_root = Array.make half_facets (-1) and shell_count = ref 0 in
    for half = 0 to half_facets - 1 do
      if parent.(half) = half then begin
        shell_of_root.(half) <- !shell_count;
        incr shell_count
      end
    done;
    let shells = Array.init half_facets (fun half -> shell_of_root.(parent.(half))) in
    let left_winding = Array.make facets 0 and right_winding = Array.make facets 0 in
    for facet = 0 to facets - 1 do
      let first, last = Boolean_complex.facet_member_range complex facet in
      for member = first to last - 1 do
        let winding = Boolean_complex.member_winding complex member in
        match Boolean_complex.member_side complex member with
        | Boolean_constraints.Left -> left_winding.(facet) <- left_winding.(facet) + winding
        | Boolean_constraints.Right -> right_winding.(facet) <- right_winding.(facet) + winding
      done
    done;
    Ok { complex; shells; shell_count = !shell_count; left_winding; right_winding }
  with
  | Cancel.Cancelled -> error "cancelled" "Weiler adjacency construction was cancelled"
  | Invalid_argument message -> error "invalid_complex" message
