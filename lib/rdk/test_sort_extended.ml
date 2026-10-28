open Rdk
open Rdk_test_support

let int_attribute name geometry =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Int values -> values
       | _ -> fail (name ^ " has wrong storage"))
  | None -> fail ("missing " ^ name)

let primitive_int_attribute name geometry =
  match Geometry.find_attribute ~owner:Attribute.Primitive name geometry with
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Int values -> values
       | _ -> fail (name ^ " has wrong primitive storage"))
  | None -> fail ("missing primitive " ^ name)

let with_int name values geometry =
  let attribute = Attribute.create_owned ~owner:Attribute.Point ~name
      (Attribute.Int values) |> Result.get_ok in
  Geometry.with_attribute attribute geometry |> Result.get_ok

let with_float name values geometry =
  let attribute = Attribute.create_owned ~owner:Attribute.Point ~name
      (Attribute.Float values) |> Result.get_ok in
  Geometry.with_attribute attribute geometry |> Result.get_ok

let ids geometry = int_attribute "id" geometry

let points values =
  Line_geometry.points (Array.map (fun x -> x, 0., 0.) values)
  |> with_int "id" (Array.init (Array.length values) Fun.id)

let permutation values =
  let seen = Bytes.make (Array.length values) '\000' and valid = ref true in
  Array.iter (fun value ->
    if value < 0 || value >= Array.length values
        || Bytes.get seen value <> '\000' then valid := false
    else Bytes.set seen value '\001') values;
  !valid

let same_int_array left right =
  Array.length left = Array.length right
  && let same = ref true in
     for index = 0 to Array.length left - 1 do
       if left.(index) <> right.(index) then same := false
     done;
     !same

let test_random () =
  let source = points (Array.init 32 float_of_int) in
  let run seed domains = Parallel.run ~domains (fun () ->
      Ordering.sort ~grain:3 ~owner:Ordering.Points ~key:(Ordering.Random seed) source
      |> get_ok) in
  let one = run 42L 1 and four = run 42L 4 and other = run 43L 1 in
  check (same_int_array (ids one) (ids four))
    "random Sort one/four-domain exactness";
  check (permutation (ids one) && not (same_int_array (ids one) (ids source)))
    "random Sort permutation";
  check (not (same_int_array (ids one) (ids other)))
    "random Sort seed identity";
  let selected = Group.init ~grain:1 ~owner:Group.Point ~name:"selected" 32
      (fun point -> point land 1 = 0) in
  (match Ordering.sort ~selection:selected ~owner:Ordering.Points ~key:(Ordering.Random 42L)
      source with
   | Error error -> check (Error.code error = "invalid_sort")
       "random Sort restricted diagnostic"
   | Ok _ -> fail "random Sort accepted group restriction")

let test_indirect () =
  let source = points [|3.; 1.; 2.; 1.|] in
  let ranked = Ordering.sort ~grain:1 ~owner:Ordering.Points ~key:Ordering.X
      ~output_indices:"rank" source |> get_ok in
  check (same_int_array (int_attribute "rank" ranked) [|3; 0; 2; 1|]
      && same_int_array (ids ranked) [|0; 1; 2; 3|])
    "Sort Indices stable ranks without reorder";
  let reordered = Ordering.sort ~grain:1 ~owner:Ordering.Points
      ~key:(Ordering.Index_attribute "rank") ranked |> get_ok in
  check (same_int_array (ids reordered) [|1; 3; 2; 0|])
    "reorder by Sort index attribute";
  let combined_source = points [|0.; 1.; 2.; 3.|]
      |> with_int "primary" [|1; 0; 1; 0|]
      |> with_int "secondary" [|1; 1; 0; 0|] in
  let secondary = Ordering.sort ~grain:1 ~owner:Ordering.Points
      ~key:(Ordering.Attribute_component { name = "secondary"; component = 0 })
      ~output_indices:"rank" combined_source |> get_ok in
  let combined = Ordering.sort ~grain:1 ~owner:Ordering.Points
      ~key:(Ordering.Attribute_component { name = "primary"; component = 0 })
      ~output_indices:"rank" ~combine_indices:true secondary |> get_ok in
  check (same_int_array (int_attribute "rank" combined) [|3; 1; 2; 0|])
    "combined Sort Indices stable ordering";
  let reordered = Ordering.sort ~grain:1 ~owner:Ordering.Points
      ~key:(Ordering.Index_attribute "rank") combined |> get_ok in
  check (same_int_array (ids reordered) [|3; 1; 2; 0|])
    "combined indirect Sort reorder"

let test_restricted_and_errors () =
  let source = points [|0.; 1.; 2.; 3.|]
      |> with_int "restricted_rank" [|2; 99; 0; -8|] in
  let selected = Group.init ~grain:1 ~owner:Group.Point ~name:"selected" 4
      (fun point -> point = 0 || point = 2) in
  let reordered = Ordering.sort ~grain:1 ~selection:selected ~owner:Ordering.Points
      ~key:(Ordering.Index_attribute "restricted_rank") source |> get_ok in
  check (same_int_array (ids reordered) [|2; 1; 0; 3|])
    "restricted reorder-by-index selected slots";
  let expect_invalid geometry key message =
    match Ordering.sort ~owner:Ordering.Points ~key geometry with
    | Error error -> check (Error.code error = "invalid_sort") message
    | Ok _ -> fail (message ^ ": unexpectedly accepted") in
  expect_invalid (source |> with_int "bad" [|0; 0; 2; 3|])
    (Ordering.Index_attribute "bad") "duplicate Sort index";
  expect_invalid (source |> with_int "bad" [|0; 1; 2; 4|])
    (Ordering.Index_attribute "bad") "out-of-range Sort index";
  expect_invalid (source |> with_float "bad" [|0.; 1.; 2.; 3.|])
    (Ordering.Index_attribute "bad") "non-integer Sort index";
  expect_invalid source (Ordering.Index_attribute "missing") "missing Sort index";
  (match Ordering.sort ~owner:Ordering.Points ~key:Ordering.X ~combine_indices:true source with
   | Error error -> check (Error.code error = "invalid_sort")
       "combine Sort Indices requires output"
   | Ok _ -> fail "combine Sort Indices accepted no output");
  (match Ordering.sort ~owner:Ordering.Points ~key:Ordering.X ~output_indices:"P" source with
   | Error error -> check (Error.code error = "invalid_sort")
       "Sort Indices reserved output"
   | Ok _ -> fail "Sort Indices accepted P output")

let test_primitive_indirect () =
  let source = Plane_generators.grid ~columns:3 ~rows:2 ~size:2.
      ~connectivity:Plane_generators.Grid_triangles () |> get_ok in
  let count = Geometry.primitive_count source in
  let ids = Attribute.create_owned ~owner:Attribute.Primitive ~name:"pid"
      (Attribute.Int (Array.init count Fun.id)) |> Result.get_ok
  and order = Attribute.create_owned ~owner:Attribute.Primitive ~name:"order"
      (Attribute.Int (Array.init count (fun primitive -> count - 1 - primitive)))
      |> Result.get_ok in
  let source = source |> Geometry.with_attribute ids |> Result.get_ok
      |> Geometry.with_attribute order |> Result.get_ok in
  let reordered = Ordering.sort ~grain:1 ~owner:Ordering.Primitives
      ~key:(Ordering.Index_attribute "order") source |> get_ok in
  check (same_int_array (primitive_int_attribute "pid" reordered)
      (Array.init count (fun primitive -> count - 1 - primitive)))
    "primitive reorder by index attribute";
  let ranked = Ordering.sort ~grain:1 ~owner:Ordering.Primitives ~key:(Ordering.Random 77L)
      ~output_indices:"rank" source |> get_ok in
  check (permutation (primitive_int_attribute "rank" ranked)
      && same_int_array (primitive_int_attribute "pid" ranked)
           (Array.init count Fun.id))
    "primitive random Sort Indices without reorder"

let test_topology_and_spatial_keys () =
  let positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.; 1.; 2.; 3.; 4.|] ~y:(Array.make 5 0.) ~z:(Array.make 5 0.) in
  let builder = Rdk_test_support.Tb.create ~point_count:5 () in
  Rdk_test_support.Tb.add_triangle builder 2 0 3;
  Rdk_test_support.Tb.add_triangle builder 3 1 2;
  let geometry = Geometry.create ~positions ~topology:(Rdk_test_support.Tb.freeze builder)
      () |> Result.get_ok |> with_int "id" [|0; 1; 2; 3; 4|] in
  let vertex_order = Ordering.sort ~grain:1 ~owner:Ordering.Points
      ~key:Ordering.By_vertex_order geometry |> get_ok in
  check (same_int_array (ids vertex_order) [|4; 2; 0; 3; 1|])
    "Sort points by first vertex order with unconnected-first policy";
  let primitive_index = Ordering.sort ~grain:1 ~owner:Ordering.Points
      ~key:Ordering.By_primitive_index geometry |> get_ok in
  check (same_int_array (ids primitive_index) [|4; 0; 2; 3; 1|])
    "Sort points by lowest primitive index with unconnected-first policy";
  let locality = Line_geometry.points
      [|0., 0., 1.; 0., 1., 0.; 1., 0., 0.; 0., 0., 0.|]
      |> with_int "id" [|0; 1; 2; 3|]
      |> Ordering.sort ~grain:1 ~owner:Ordering.Points ~key:Ordering.Spatial_locality
      |> get_ok in
  check (same_int_array (ids locality) [|3; 2; 1; 0|])
    "Morton spatial-locality ordering";
  let extreme = Line_geometry.points
      [|max_float, max_float, max_float; -.max_float, -.max_float, -.max_float|]
      |> Ordering.sort ~grain:1 ~owner:Ordering.Points ~key:Ordering.Spatial_locality in
  (match extreme with
   | Ok _ -> ()
   | Error error -> fail ("extreme spatial Sort failed: " ^ Error.to_string error));
  (match Line_geometry.points [|Float.nan, 0., 0.|]
      |> Ordering.sort ~owner:Ordering.Points ~key:Ordering.Spatial_locality with
   | Error error -> check (Error.code error = "invalid_sort")
       "non-finite spatial Sort diagnostic"
   | Ok _ -> fail "spatial Sort accepted non-finite position");
  (match Ordering.sort ~owner:Ordering.Primitives ~key:Ordering.By_vertex_order geometry with
   | Error error -> check (Error.code error = "invalid_sort")
       "point-only topology Sort diagnostic"
   | Ok _ -> fail "primitive Sort accepted By Vertex Order")

let test_dense_exactness () =
  let source = points (Array.init 150_001 (fun point ->
      sin (float_of_int point *. 0.001))) in
  let run domains = Parallel.run ~domains (fun () ->
      let random = Ordering.sort ~grain:4096 ~owner:Ordering.Points
          ~key:(Ordering.Random 918273L) source |> get_ok in
      Ordering.sort ~grain:4096 ~owner:Ordering.Points ~key:Ordering.X
        ~output_indices:"rank" random |> get_ok) in
  let one = run 1 and four = run 4 in
  check (same_int_array (ids one) (ids four)
      && same_int_array (int_attribute "rank" one)
           (int_attribute "rank" four))
    "dense random/indirect Sort one/four-domain exactness";
  let locality domains = Parallel.run ~domains (fun () ->
      Ordering.sort ~grain:4096 ~owner:Ordering.Points ~key:Ordering.Spatial_locality source
      |> get_ok) in
  check (same_int_array (ids (locality 1)) (ids (locality 4)))
    "dense spatial-locality Sort one/four-domain exactness"

let run () =
  test_random ();
  test_indirect ();
  test_restricted_and_errors ();
  test_primitive_indirect ();
  test_topology_and_spatial_keys ();
  test_dense_exactness ();
  print_endline "extended sort tests passed"
