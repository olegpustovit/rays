open Rdk

let fail format = Printf.ksprintf failwith format
let check condition =
  Printf.ksprintf (fun message -> if not condition then failwith message)
let get = function Ok value -> value | Error message -> fail "%s" message

let sign_positive = function Predicates.Positive -> true | _ -> false

let signature value =
  Array.init (Delaunay2.triangle_count value) (fun triangle ->
      Delaunay2.triangle_point value triangle 0,
      Delaunay2.triangle_point value triangle 1,
      Delaunay2.triangle_point value triangle 2)

let validate x y value =
  let triangle_count = Delaunay2.triangle_count value in
  let edges = Hashtbl.create (max 16 (triangle_count * 3)) in
  for triangle = 0 to triangle_count - 1 do
    let a = Delaunay2.triangle_point value triangle 0
    and b = Delaunay2.triangle_point value triangle 1
    and c = Delaunay2.triangle_point value triangle 2 in
    check (a <> b && b <> c && c <> a) "repeated triangle point";
    check (Predicates.orient2d ~ax:x.(a) ~ay:y.(a) ~bx:x.(b) ~by:y.(b)
        ~cx:x.(c) ~cy:y.(c) = Predicates.Positive)
      "triangle %d is not counter-clockwise" triangle;
    let add_edge first second =
      let key = min first second,max first second in
      Hashtbl.replace edges key (1 + Option.value ~default:0 (Hashtbl.find_opt edges key))
    in
    add_edge a b; add_edge b c; add_edge c a;
    for point = 0 to Array.length x - 1 do
      if point <> a && point <> b && point <> c then
        check (not (sign_positive (Predicates.incircle
            ~ax:x.(a) ~ay:y.(a) ~bx:x.(b) ~by:y.(b)
            ~cx:x.(c) ~cy:y.(c) ~dx:x.(point) ~dy:y.(point))))
          "point %d is inside triangle %d circumcircle" point triangle
    done
  done;
  Hashtbl.iter (fun _ count -> check (count <= 2) "non-manifold planar edge") edges

let test_triangle () =
  let x = [|0.;2.;0.|] and y = [|0.;0.;2.|] in
  let value = Delaunay2.build ~x ~y () |> get in
  check (Delaunay2.triangle_count value = 1) "triangle count differs";
  validate x y value

let test_square_and_seed () =
  let x = [|0.;1.;1.;0.|] and y = [|0.;0.;1.;1.|] in
  for seed = 0 to 15 do
    let value = Delaunay2.build ~seed:(Int64.of_int seed) ~x ~y () |> get in
    check (Delaunay2.triangle_count value = 2) "square count differs for seed %d" seed;
    validate x y value
  done

let test_duplicates_and_collinear () =
  let x = [|0.;1.;0.;1.;0.;2.|] and y = [|0.;0.;1.;0.;0.;0.|] in
  let value = Delaunay2.build ~x ~y () |> get in
  check (Delaunay2.unique_count value = 4) "duplicate count differs";
  check (Delaunay2.source_unique value 0 = Delaunay2.source_unique value 4)
    "duplicate zero did not alias";
  check (Delaunay2.source_unique value 1 = Delaunay2.source_unique value 3)
    "duplicate one did not alias";
  validate x y value;
  let value = Delaunay2.build ~x:[|0.;1.;2.;3.|] ~y:[|0.;0.;0.;0.|] () |> get in
  check (Delaunay2.triangle_count value = 0) "collinear input emitted triangles"

let test_extreme_ranges () =
  let largest = Float.max_float in
  let x = [|-.largest;largest;largest;-.largest;0.|]
  and y = [|-.largest;-.largest;largest;largest;0.|] in
  let value = Delaunay2.build ~x ~y () |> get in
  check (Delaunay2.triangle_count value = 4) "maximum-range count differs";
  validate x y value;
  let tiny = Int64.float_of_bits 1L in
  let x = [|0.;tiny;tiny;0.|] and y = [|0.;0.;tiny;tiny|] in
  let value = Delaunay2.build ~x ~y () |> get in
  check (Delaunay2.triangle_count value = 2) "subnormal count differs";
  validate x y value

let coordinate index salt =
  let value = ((index * 1_103_515_245) + (salt * 12_345) + 97) land max_int in
  float_of_int (value mod 10_007) +. (float_of_int index *. 1e-7)

let test_random_and_domains () =
  let count = 200 in
  let x = Array.init count (fun point -> coordinate point 3)
  and y = Array.init count (fun point -> coordinate point 11) in
  let run domains = Parallel.run ~domains (fun () ->
      Delaunay2.build ~seed:97L ~x ~y () |> get) in
  let one = run 1 and four = run 4 in
  check (signature one = signature four) "domain count changed triangulation";
  validate x y one

let test_failures_and_cancellation () =
  (match Delaunay2.build ~x:[|0.|] ~y:[||] () with
   | Error _ -> () | Ok _ -> fail "mismatched coordinates succeeded");
  (match Delaunay2.build ~x:[|Float.nan|] ~y:[|0.|] () with
   | Error _ -> () | Ok _ -> fail "non-finite coordinate succeeded");
  let cancel = Cancel.create () in
  Cancel.cancel cancel;
  match Delaunay2.build ~cancel ~x:[|0.;1.;0.|] ~y:[|0.;0.;1.|] () with
  | Error _ -> () | Ok _ -> fail "cancelled triangulation succeeded"

let run () =
  test_triangle ();
  test_square_and_seed ();
  test_duplicates_and_collinear ();
  test_extreme_ranges ();
  test_random_and_domains ();
  test_failures_and_cancellation ()
