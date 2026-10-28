let fail = Error.fail

(* The first-fit rule: the smallest color in the window [base, limit) whose
   mark is not [stamp], the element being colored. *)
let first_free marks ~base ~limit ~stamp =
  let color = ref 0 in
  while base + !color < limit && marks.(base + !color) = stamp do
    incr color
  done;
  !color

let greedy ~offsets ~neighbors order =
  let count = Array.length offsets - 1 in
  let colors = Array.make count (-1) and marks = Array.make count (-1) in
  Array.iter
    (fun element ->
      for at = offsets.(element) to offsets.(element + 1) - 1 do
        let color = colors.(neighbors.(at)) in
        if color >= 0 then marks.(color) <- element
      done;
      let color = first_free marks ~base:0 ~limit:count ~stamp:element in
      if color >= count then fail "Graph Color internal color bound was exceeded";
      colors.(element) <- color)
    order;
  colors
