open Rdk

module Constraints = Rdk_boolean.Boolean_constraints
module Coplanar = Rdk_boolean.Boolean_coplanar
module Arrangement = Rdk_boolean.Boolean_face_arrangement
module Triangulation = Rdk_boolean.Boolean_face_cdt

let fail format = Printf.ksprintf failwith format
let check condition message = if not condition then fail "%s" message
let get = function Ok value -> value | Error error -> fail "%s" (Error.to_string error)
let get_string = function Ok value -> value | Error message -> fail "%s" message

let geometry points triangles =
  let count = Array.length points in
  let x = Array.init count (fun point -> let x, _, _ = points.(point) in x)
  and y = Array.init count (fun point -> let _, y, _ = points.(point) in y)
  and z = Array.init count (fun point -> let _, _, z = points.(point) in z) in
  let topology = Topology.polygons_owned ~point_count:count
      ~vertex_points:triangles
      ~primitive_offsets:(Array.init ((Array.length triangles / 3) + 1)
        (fun primitive -> primitive * 3)) |> get_string in
  Geometry.create ~positions:(Packed.Float3.Private.of_owned_exn ~x ~y ~z)
    ~topology () |> get_string

let crossing_input () =
  let left = geometry
      [|-3.,-3.,0.; 3.,-3.,0.; 0.,3.,0.|] [|0;1;2|]
  and right = geometry
      [|0.,-2.,-1.; 0.,2.,-1.; 0.,0.,1.;
        -2.,0.,-1.; 2.,0.,-1.; 0.,0.,1.|]
      [|0;1;2; 3;4;5|] in
  let constraints = Constraints.build ~grain:1 ~left ~right () |> get in
  let arrangement = Arrangement.build constraints
      ~side:Arrangement.Left ~triangle:0 |> get in
  constraints, arrangement

let edge_present triangulation first second =
  let present = ref false in
  for triangle = 0 to Triangulation.triangle_count triangulation - 1 do
    let a = Triangulation.triangle_point triangulation triangle 0
    and b = Triangulation.triangle_point triangulation triangle 1
    and c = Triangulation.triangle_point triangulation triangle 2 in
    if (a = first && b = second) || (a = second && b = first)
        || (b = first && c = second) || (b = second && c = first)
        || (c = first && a = second) || (c = second && a = first)
    then present := true
  done;
  !present

let validate triangulation =
  let points = Triangulation.point_count triangulation in
  for triangle = 0 to Triangulation.triangle_count triangulation - 1 do
    let a = Triangulation.triangle_point triangulation triangle 0
    and b = Triangulation.triangle_point triangulation triangle 1
    and c = Triangulation.triangle_point triangulation triangle 2 in
    check (a >= 0 && a < points && b >= 0 && b < points
        && c >= 0 && c < points) "CDT triangle point is out of range";
    check (a <> b && b <> c && c <> a) "CDT emitted a repeated triangle point"
  done;
  for constraint_index = 0 to Triangulation.constraint_count triangulation - 1 do
    check (edge_present triangulation
        (Triangulation.constraint_first triangulation constraint_index)
        (Triangulation.constraint_second triangulation constraint_index))
      "CDT did not recover a constraint edge"
  done

let signature triangulation =
  Array.init (Triangulation.triangle_count triangulation) (fun triangle ->
      Triangulation.triangle_point triangulation triangle 0,
      Triangulation.triangle_point triangulation triangle 1,
      Triangulation.triangle_point triangulation triangle 2),
  Array.init (Triangulation.constraint_count triangulation) (fun constraint_index ->
      Triangulation.constraint_first triangulation constraint_index,
      Triangulation.constraint_second triangulation constraint_index)

let test_crossing () =
  let constraints, arrangement = crossing_input () in
  let triangulation = Triangulation.build constraints arrangement
      ~side:Arrangement.Left ~triangle:0 |> get in
  check (Triangulation.point_count triangulation = 8)
    "crossing CDT point cardinality is wrong";
  check (Triangulation.triangle_count triangulation = 11)
    "crossing CDT triangle cardinality is wrong";
  check (Triangulation.constraint_count triangulation = 4)
    "crossing CDT constraint cardinality is wrong";
  validate triangulation

let test_single_segment () =
  let left = geometry [|0.,0.,0.; 2.,0.,0.; 0.,2.,0.|] [|0;1;2|]
  and right = geometry
      [|0.5,-0.5,-1.; 0.5,1.5,1.; 0.5,1.5,-1.|] [|0;1;2|] in
  let constraints = Constraints.build ~grain:1 ~left ~right () |> get in
  let arrangement = Arrangement.build constraints
      ~side:Arrangement.Left ~triangle:0 |> get in
  let triangulation = Triangulation.build constraints arrangement
      ~side:Arrangement.Left ~triangle:0 |> get in
  check (Triangulation.point_count triangulation = 5)
    "single-segment CDT point cardinality is wrong";
  check (Triangulation.triangle_count triangulation = 4)
    "single-segment CDT triangle cardinality is wrong";
  validate triangulation

let coplanar_input () =
  let left = geometry
      [|0.,0.,0.; 4.,0.,0.; 2.,4.,0.|] [|0;1;2|]
  and right = geometry
      [|0.,3.,0.; 4.,3.,0.; 2.,-1.,0.|] [|0;1;2|] in
  let constraints = Constraints.build ~grain:1 ~left ~right () |> get in
  let coplanar = Coplanar.build ~grain:1 constraints |> get in
  let arrangement = Arrangement.build ~coplanar constraints
      ~side:Arrangement.Left ~triangle:0 |> get in
  constraints, arrangement

let test_coplanar_overlay () =
  let constraints, arrangement = coplanar_input () in
  let triangulation = Triangulation.build constraints arrangement
      ~side:Arrangement.Left ~triangle:0 |> get in
  check (Triangulation.point_count triangulation = 9)
    "coplanar CDT point cardinality is wrong";
  check (Triangulation.triangle_count triangulation = 7)
    "coplanar CDT triangle cardinality is wrong";
  check (Triangulation.constraint_count triangulation = 6)
    "coplanar CDT constraint cardinality is wrong";
  validate triangulation

let test_domain_exactness () =
  let run domains = Parallel.run ~domains (fun () ->
      let constraints, arrangement = crossing_input () in
      Triangulation.build constraints arrangement
        ~side:Arrangement.Left ~triangle:0 |> get |> signature) in
  if run 1 <> run 4 then fail "face CDT differs between one and four domains"

let many_constraint_input count =
  let scale = float_of_int count in
  let left = geometry
      [|-.scale,-.scale,0.; 2.*.scale,-.scale,0.;
        scale/.2.,2.*.scale,0.|] [|0;1;2|] in
  let points = Array.make (count * 3) (0.,0.,0.)
  and triangles = Array.init (count * 3) Fun.id in
  for segment = 0 to count - 1 do
    let point = segment * 3 and x = float_of_int segment +. 0.25 in
    points.(point) <- x,-1.,-1.;
    points.(point + 1) <- x,1.,-1.;
    points.(point + 2) <- x,0.,1.
  done;
  let right = geometry points triangles in
  let constraints = Constraints.build ~grain:7 ~left ~right () |> get in
  let arrangement = Arrangement.build constraints
      ~side:Arrangement.Left ~triangle:0 |> get in
  constraints,arrangement

let test_incremental_topology_and_walk () =
  let count = 64 in
  let run domains =
    Parallel.run ~domains (fun () ->
      let constraints,arrangement = many_constraint_input count in
      Triangulation.build constraints arrangement
        ~side:Arrangement.Left ~triangle:0 |> get) in
  let triangulation = run 1 in
  check (Triangulation.point_count triangulation = (count * 2) + 3
      && Triangulation.triangle_count triangulation = (count * 4) + 1
      && Triangulation.constraint_count triangulation = count)
    "incremental face CDT has incorrect dense cardinality";
  validate triangulation;
  check (signature triangulation = signature (run 4))
    "incremental face CDT differs between domain counts"

let test_cancellation () =
  let constraints, arrangement = crossing_input () in
  let cancel = Cancel.create () in Cancel.cancel cancel;
  match Triangulation.build ~cancel constraints arrangement
      ~side:Arrangement.Left ~triangle:0 with
  | Error error when Error.code error = "cancelled" -> ()
  | Error error -> fail "unexpected cancellation error: %s" (Error.to_string error)
  | Ok _ -> fail "cancelled face CDT completed"

let run () =
  test_crossing ();
  test_single_segment ();
  test_coplanar_overlay ();
  test_domain_exactness ();
  test_incremental_topology_and_walk ();
  test_cancellation ()
