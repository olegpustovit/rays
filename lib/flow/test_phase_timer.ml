open Flow.Phase_timer
let () =
  let time = ref 0. in
  let clock () = !time in
  let _, timing = sample ~clock (fun () -> measure "user/outer" (fun () ->
    time := 1.; measure "user/inner" (fun () -> time := 3.);
    time := 4.; measure "user/inner" (fun () -> time := 5.); time := 6.)) in
  assert (timing.total = 6. && seconds timing "user/outer" = 3.);
  assert (calls timing "user/inner" = 2 && seconds timing "user/inner" = 3.);
  assert (List.map (fun entry -> entry.phase) timing.entries = ["user/inner"; "user/outer"]);
  assert (summary timing = "user/inner 3000.0 ms · user/outer 3000.0 ms");
  let _, internal = sample ~clock (fun () -> measure "lower" (fun () ->
    time := 7.;
    (try without (fun () -> measure "evaluate" (fun () ->
      time := 8.; failwith "body")) with Failure _ -> ());
    measure "check" (fun () -> time := 9.))) in
  assert (calls internal "lower" = 1 && seconds internal "lower" = 2.);
  assert (calls internal "evaluate" = 0 && calls internal "check" = 1);
  let _, outer = sample ~clock (fun () ->
    (try measure "check" (fun () -> failwith "refused") with Failure _ -> ());
    ignore (sample ~clock (fun () -> measure "parse" (fun () -> time := 10.)));
    measure "print" (fun () -> time := 11.)) in
  assert (calls outer "check" = 1 && calls outer "parse" = 1 && calls outer "print" = 1);
  let _, backward = sample ~clock (fun () -> measure "check" (fun () -> time := 0.)) in
  assert (backward.total = 0. && seconds backward "check" = 0.);
  (try ignore (sample ~clock (fun () -> failwith "outside")) with Failure _ -> ());
  measure "check" (fun () -> ());
  let _, next = sample ~clock (fun () -> ()) in
  assert (next.entries = []);
  let _, parent = sample ~clock (fun () ->
    let child = Domain.spawn (fun () ->
      let _, child = sample ~clock:(fun () -> 0.) (fun () -> measure "lower" (fun () -> ())) in
      assert (calls child "lower" = 1)) in
    Domain.join child) in
  assert (parent.entries = [])
