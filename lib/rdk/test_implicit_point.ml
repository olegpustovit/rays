open Rdk_test_support


let expect_ok = function
  | Ok value -> value
  | Error _ -> failf "expected exact point construction to succeed"

let expect_error expected = function
  | Error actual when actual = expected -> ()
  | Error _ -> failf "unexpected exact point construction error"
  | Ok _ -> failf "expected exact point construction to failf"

let assert_close expected actual =
  if abs_float (expected -. actual) > 1e-14 then
    failf "expected %.17g, got %.17g" expected actual

let assert_sign expected actual =
  if actual <> expected then failf "unexpected predicate sign"

let coordinates = [|
  (0., 0., 0.);       (* 0 *)
  (2., 0., 0.);       (* 1 *)
  (1., -1., -1.);     (* 2: x = 1 plane *)
  (1., 1., -1.);      (* 3 *)
  (1., 0., 1.);       (* 4 *)
  (1., 0., 0.);       (* 5: first LPI explicitly *)
  (1., 0., 0.);       (* 6: x = 1 plane *)
  (1., 1., 0.);       (* 7 *)
  (1., 0., 1.);       (* 8 *)
  (0., 2., 0.);       (* 9: y = 2 plane *)
  (1., 2., 0.);       (* 10 *)
  (0., 2., 1.);       (* 11 *)
  (0., 0., 3.);       (* 12: z = 3 plane *)
  (1., 0., 3.);       (* 13 *)
  (0., 1., 3.);       (* 14 *)
  (1., 2., 3.);       (* 15: TPI explicitly *)
  (0., 2., 0.);       (* 16: mixed orientation points *)
  (2., 2., 0.);       (* 17 *)
  (1., 1., 1.);       (* 18 *)
|]

let x = Array.map (fun (x, _, _) -> x) coordinates
let y = Array.map (fun (_, y, _) -> y) coordinates
let z = Array.map (fun (_, _, z) -> z) coordinates

let deterministic_value multiplier offset index =
  ((index * multiplier + offset) mod 101) - 50

let incircle_integer_sign ax ay bx by cx cy dx dy =
  let open Int64 in
  let ax = of_int (ax - dx) and ay = of_int (ay - dy)
  and bx = of_int (bx - dx) and by = of_int (by - dy)
  and cx = of_int (cx - dx) and cy = of_int (cy - dy) in
  let lift x y = add (mul x x) (mul y y)
  and cross ax ay bx by = sub (mul ax by) (mul ay bx) in
  let determinant = add
      (add
         (mul (lift ax ay) (cross bx by cx cy))
         (mul (lift bx by) (cross cx cy ax ay)))
      (mul (lift cx cy) (cross ax ay bx by)) in
  if determinant < 0L then Rdk.Predicates.Negative
  else if determinant > 0L then Rdk.Predicates.Positive
  else Rdk.Predicates.Zero

let run () =
  ()
