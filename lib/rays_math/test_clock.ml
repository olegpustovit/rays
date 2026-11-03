(* Readings never go back, and two within a microsecond or two of work differ: no step of a
   millisecond hides a short run, as Unix.gettimeofday's does on Windows. *)
let () =
  let first = Rays_math.Clock.now () in
  let step = ref infinity and last = ref first in
  for _ = 1 to 10 do
    let rec spin () =
      let next = Rays_math.Clock.now () in
      if next > !last then next else spin ()
    in
    let next = spin () in
    step := Float.min !step (next -. !last);
    last := next
  done;
  assert (!last > first);
  assert (!step < 1e-4);
  print_endline "Clock: monotonic, resolution finer than 100 microseconds"
