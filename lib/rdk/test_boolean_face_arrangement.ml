open Rdk

module Constraints = Rdk_boolean.Boolean_constraints
module Coplanar = Rdk_boolean.Boolean_coplanar
module Arrangement = Rdk_boolean.Boolean_face_arrangement
let approximate_point value point =
  Rdk_exact.Implicit_point.approximate (Arrangement.Private.point value point)

let fail format = Printf.ksprintf failwith format
let check condition message = if not condition then fail "%s" message
let close left right = abs_float (left -. right) <= 1e-11
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

let crossing_plan () =
  let left = geometry
      [|-3.,-3.,0.; 3.,-3.,0.; 0.,3.,0.|] [|0;1;2|]
  and right = geometry
      [|0.,-2.,-1.; 0.,2.,-1.; 0.,0.,1.;
        -2.,0.,-1.; 2.,0.,-1.; 0.,0.,1.|]
      [|0;1;2; 3;4;5|] in
  Constraints.build ~grain:1 ~left ~right () |> get

let sorted_points arrangement =
  let points = Array.init (Arrangement.point_count arrangement)
      (approximate_point arrangement) in
  Array.sort Stdlib.compare points;
  points

let test_proper_crossing () =
  let arrangement = Arrangement.build (crossing_plan ())
      ~side:Arrangement.Left ~triangle:0 |> get in
  check (Arrangement.point_count arrangement = 5)
    "proper crossing did not add one exact TPI";
  check (Arrangement.segment_count arrangement = 4)
    "two crossing constraints were not split into four segments";
  let origin = ref (-1) in
  Array.iteri (fun point (x,y,z) ->
    if close x 0. && close y 0. && close z 0. then origin := point)
    (Array.init (Arrangement.point_count arrangement)
       (approximate_point arrangement));
  check (!origin >= 0) "TPI origin is missing";
  let incident = ref 0 in
  for segment = 0 to Arrangement.segment_count arrangement - 1 do
    let first = Arrangement.segment_first arrangement segment
    and second = Arrangement.segment_second arrangement segment in
    check (first <> second) "arrangement emitted a zero-length segment";
    if first = !origin || second = !origin then incr incident
  done;
  check (!incident = 4) "TPI should have four incident split segments";
  let points = sorted_points arrangement in
  let expected = [|(-1.,0.,0.); (0.,-1.,0.); (0.,0.,0.);
                   (0.,1.,0.); (1.,0.,0.)|] in
  Array.iteri (fun index (x,y,z) ->
    let ex,ey,ez = expected.(index) in
    if not (close x ex && close y ey && close z ez) then
      fail "arrangement point %d differs: %.17g %.17g %.17g" index x y z)
    points

let test_single_segment () =
  let left = geometry [|0.,0.,0.; 2.,0.,0.; 0.,2.,0.|] [|0;1;2|]
  and right = geometry
      [|0.5,-0.5,-1.; 0.5,1.5,1.; 0.5,1.5,-1.|] [|0;1;2|] in
  let plan = Constraints.build ~grain:1 ~left ~right () |> get in
  let arrangement = Arrangement.build plan ~side:Arrangement.Left ~triangle:0
      |> get in
  check (Arrangement.point_count arrangement = 2
      && Arrangement.segment_count arrangement = 1)
    "single constraint changed during face arrangement"

let test_multiway_crossing () =
  let left = geometry
      [|-3.,-3.,0.; 3.,-3.,0.; 0.,3.,0.|] [|0;1;2|]
  and right = geometry
      [|0.,-2.,-1.; 0.,2.,-1.; 0.,0.,1.;
        -2.,0.,-1.; 2.,0.,-1.; 0.,0.,1.;
        -1.6,-1.6,-1.; 1.6,1.6,-1.; 0.,0.,1.|]
      [|0;1;2; 3;4;5; 6;7;8|] in
  let plan = Constraints.build ~grain:1 ~left ~right () |> get in
  let arrangement = Arrangement.build plan ~side:Arrangement.Left ~triangle:0
      |> get in
  check (Arrangement.point_count arrangement = 7)
    "three-way crossing did not deduplicate the shared TPI";
  check (Arrangement.segment_count arrangement = 6)
    "three-way crossing did not split all incident constraints";
  let origin = ref (-1) in
  for point = 0 to Arrangement.point_count arrangement - 1 do
    let x,y,z = approximate_point arrangement point in
    if close x 0. && close y 0. && close z 0. then origin := point
  done;
  let incident = ref 0 in
  for segment = 0 to Arrangement.segment_count arrangement - 1 do
    if Arrangement.segment_first arrangement segment = !origin
        || Arrangement.segment_second arrangement segment = !origin then
      incr incident
  done;
  check (!incident = 6) "shared TPI does not have six exact incident segments"

let test_coplanar_overlay () =
  let left = geometry
      [|0.,0.,0.; 4.,0.,0.; 2.,4.,0.|] [|0;1;2|]
  and right = geometry
      [|0.,3.,0.; 4.,3.,0.; 2.,-1.,0.|] [|0;1;2|] in
  let constraints = Constraints.build ~grain:1 ~left ~right () |> get in
  let coplanar = Coplanar.build ~grain:1 constraints |> get in
  let arrangement = Arrangement.build ~coplanar constraints
      ~side:Arrangement.Left ~triangle:0 |> get in
  check (Arrangement.point_count arrangement = 6)
    "coplanar polygon points did not enter the source-face arrangement";
  check (Arrangement.segment_count arrangement = 6)
    "coplanar polygon boundary did not enter the source-face arrangement";
  let right_arrangement = Arrangement.build ~coplanar constraints
      ~side:Arrangement.Right ~triangle:0 |> get in
  check (sorted_points arrangement = sorted_points right_arrangement)
    "two sides of a coplanar pair received different exact boundaries"

let test_mixed_coplanar_noncoplanar_crossing () =
  let left = geometry
      [|0.,0.,0.; 4.,0.,0.; 2.,4.,0.|] [|0;1;2|]
  and right = geometry
      [|0.,3.,0.; 4.,3.,0.; 2.,-1.,0.;
        2.,-1.,-1.; 2.,5.,-1.; 2.,2.,1.|]
      [|0;1;2; 3;4;5|] in
  let constraints = Constraints.build ~grain:1 ~left ~right () |> get in
  check (Constraints.coplanar_pair_count constraints = 1)
    "mixed fixture lost its coplanar pair";
  let coplanar = Coplanar.build ~grain:1 constraints |> get in
  let arrangement = Arrangement.build ~coplanar constraints
      ~side:Arrangement.Left ~triangle:0 |> get in
  check (Arrangement.point_count arrangement = 9)
    "mixed overlap did not construct the exact edge/plane crossing";
  check (Arrangement.segment_count arrangement = 9)
    "mixed overlap did not split both crossing constraints";
  let crossing = ref (-1) and incident = ref 0 in
  for point = 0 to Arrangement.point_count arrangement - 1 do
    let x, y, z = approximate_point arrangement point in
    if close x 2. && close y 3. && close z 0. then crossing := point
  done;
  check (!crossing >= 0) "mixed exact crossing at (2,3,0) is missing";
  for segment = 0 to Arrangement.segment_count arrangement - 1 do
    if Arrangement.segment_first arrangement segment = !crossing
        || Arrangement.segment_second arrangement segment = !crossing then
      incr incident
  done;
  check (!incident = 4) "mixed edge/plane crossing is not four-valent"

let many_parallel_plan ~count ~point_contact =
  let scale = float_of_int count in
  let left = geometry
      [|-.scale,-.scale,0.; 2.*.scale,-.scale,0.;
        scale/.2.,2.*.scale,0.|] [|0;1;2|] in
  let primitive_count = count + if point_contact then 1 else 0 in
  let points = Array.make (primitive_count * 3) (0.,0.,0.)
  and triangles = Array.init (primitive_count * 3) Fun.id in
  for segment = 0 to count - 1 do
    let point = segment * 3 and x = float_of_int segment +. 0.25 in
    points.(point) <- x,-1.,-1.;
    points.(point + 1) <- x,1.,-1.;
    points.(point + 2) <- x,0.,1.
  done;
  if point_contact then begin
    let point = count * 3 and x = float_of_int (count / 2) +. 0.25 in
    points.(point) <- x,0.,0.;
    points.(point + 1) <- x-.0.25,0.,1.;
    points.(point + 2) <- x+.0.25,0.,1.
  end;
  left, geometry points triangles

let arrangement_signature arrangement =
  Array.init (Arrangement.point_count arrangement)
    (approximate_point arrangement),
  Array.init (Arrangement.segment_count arrangement) (fun segment ->
    Arrangement.segment_first arrangement segment,
    Arrangement.segment_second arrangement segment)

let test_sparse_sweep_and_point_candidates () =
  let count = 64 in
  let run domains point_contact =
    Parallel.run ~domains (fun () ->
      let left,right = many_parallel_plan ~count ~point_contact in
      let constraints = Constraints.build ~grain:7 ~left ~right () |> get in
      Arrangement.build constraints
        ~side:Arrangement.Left ~triangle:0 |> get) in
  let plain = run 1 false in
  check (Arrangement.point_count plain = count * 2
      && Arrangement.segment_count plain = count)
    "sparse face sweep changed independent constraint cardinality";
  check (arrangement_signature plain = arrangement_signature (run 4 false))
    "sparse face sweep differs between domain counts";
  let contacted = run 1 true in
  check (Arrangement.point_count contacted = (count * 2) + 1
      && Arrangement.segment_count contacted = count + 1)
    "point/segment sweep did not split an interior point contact";
  check (arrangement_signature contacted = arrangement_signature (run 4 true))
    "point/segment sweep differs between domain counts"

let multiway_plan count =
  let left = geometry
      [|-10.,-10.,0.; 10.,-10.,0.; 0.,10.,0.|] [|0;1;2|] in
  let points = Array.make (count * 3) (0.,0.,0.)
  and triangles = Array.init (count * 3) Fun.id in
  for line = 0 to count - 1 do
    let angle = Float.pi *. float_of_int line /. float_of_int count in
    let dx = 4. *. cos angle and dy = 4. *. sin angle
    and point = line * 3 in
    points.(point) <- -.dx,-.dy,-1.;
    points.(point + 1) <- dx,dy,-1.;
    points.(point + 2) <- 0.,0.,1.
  done;
  Constraints.build ~grain:7 ~left ~right:(geometry points triangles) () |> get

let test_dense_multiway_sweep () =
  let count = 96 in
  let run domains =
    Parallel.run ~domains (fun () ->
      Arrangement.build (multiway_plan count)
        ~side:Arrangement.Left ~triangle:0 |> get) in
  let arrangement = run 1 in
  check (Arrangement.point_count arrangement = (count * 2) + 1
      && Arrangement.segment_count arrangement = count * 2)
    "dense face sweep did not aggregate a multi-way exact crossing";
  check (arrangement_signature arrangement = arrangement_signature (run 4))
    "dense face sweep differs between domain counts"

let test_cancellation () =
  let cancel = Cancel.create () in
  Cancel.cancel cancel;
  match Arrangement.build ~cancel (crossing_plan ())
      ~side:Arrangement.Left ~triangle:0 with
  | Error error when Error.code error = "cancelled" -> ()
  | Error error -> fail "unexpected cancellation error: %s" (Error.to_string error)
  | Ok _ -> fail "cancelled face arrangement completed"

let run () =
  test_proper_crossing ();
  test_single_segment ();
  test_multiway_crossing ();
  test_coplanar_overlay ();
  test_mixed_coplanar_noncoplanar_crossing ();
  test_sparse_sweep_and_point_candidates ();
  test_dense_multiway_sweep ();
  test_cancellation ()
