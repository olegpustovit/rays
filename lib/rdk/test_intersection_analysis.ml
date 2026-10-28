open Rays
open Rdk
open Rdk_test_support

let get = function Ok value -> value | Error error -> fail (Error.to_string error)

let geometry points triangles =
  let count = Array.length points in
  let x = Array.init count (fun point -> let x, _, _ = points.(point) in x)
  and y = Array.init count (fun point -> let _, y, _ = points.(point) in y)
  and z = Array.init count (fun point -> let _, _, z = points.(point) in z) in
  let topology = Topology.polygons_owned ~point_count:count
      ~vertex_points:triangles
      ~primitive_offsets:(Array.init ((Array.length triangles / 3) + 1)
        (fun primitive -> primitive * 3)) |> get_string in
  Geometry.create ~positions:(Packed.Float3.View.of_owned_exn ~x ~y ~z)
    ~topology () |> get_string

let geometry_with_topology points vertex_points primitive_offsets primitive_kinds =
  let count = Array.length points in
  let x = Array.init count (fun point -> let x, _, _ = points.(point) in x)
  and y = Array.init count (fun point -> let _, y, _ = points.(point) in y)
  and z = Array.init count (fun point -> let _, _, z = points.(point) in z) in
  let topology = Topology.create_owned ~point_count:count ~vertex_points
      ~primitive_offsets ~primitive_kinds |> get_string in
  Geometry.create ~positions:(Packed.Float3.View.of_owned_exn ~x ~y ~z)
    ~topology () |> get_string

let open_curve points =
  geometry_with_topology points (Array.init (Array.length points) Fun.id)
    [|0; Array.length points|] [|Topology.Open_polyline|]

let closed_curve points =
  geometry_with_topology points (Array.init (Array.length points) Fun.id)
    [|0; Array.length points|] [|Topology.Closed_polyline|]

let segment_network count endpoints =
  let points = Array.init (count * 2) (fun point ->
      let first, second = endpoints (point / 2) in
      if point land 1 = 0 then first else second) in
  geometry_with_topology points (Array.init (count * 2) Fun.id)
    (Array.init (count + 1) (fun primitive -> primitive * 2))
    (Array.make count Topology.Open_polyline)

let source () = geometry
    [|0.,0.,0.; 2.,0.,0.; 0.,2.,0.; 10.,0.,0.; 12.,0.,0.; 10.,2.,0.|]
    [|0;1;2; 3;4;5|]

let collision () = geometry
    [|0.5,-0.5,-1.; 0.5,1.5,1.; 0.5,1.5,-1.;
      0.25,0.25,0.; 0.5,0.25,0.; 0.25,0.5,0.|]
    [|0;1;2; 3;4;5|]

let int_rows name geometry =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Int_array value -> Packed.Int_array.View.view value
       | _ -> fail (name ^ " has wrong storage"))
  | None -> fail ("missing " ^ name)

let float_rows name geometry =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Float_array value -> Packed.Float_array.View.view value
       | _ -> fail (name ^ " has wrong storage"))
  | None -> fail ("missing " ^ name)

let positions geometry =
  let view = Packed.Float3.View.view (Geometry.positions geometry) in
  view.x, view.y, view.z

let expect code = function
  | Error error when Error.code error = code -> ()
  | Error error -> fail (Printf.sprintf "expected %s, got %s: %s"
      code (Error.code error) (Error.to_string error))
  | Ok _ -> fail ("expected " ^ code)

let close left right = Float.abs (left -. right) <= 1e-12

let equal_output left right =
  positions left = positions right
  && let left_input = int_rows "sourceinput" left
     and right_input = int_rows "sourceinput" right
     and left_primitive = int_rows "sourceprim" left
     and right_primitive = int_rows "sourceprim" right
     and left_uvw = float_rows "sourceprimuv" left
     and right_uvw = float_rows "sourceprimuv" right
     and left_point = int_rows "sourcepoint" left
     and right_point = int_rows "sourcepoint" right in
     left_input.offsets = right_input.offsets
     && left_input.values = right_input.values
     && left_primitive.offsets = right_primitive.offsets
     && left_primitive.values = right_primitive.values
     && left_uvw.offsets = right_uvw.offsets
     && left_uvw.values = right_uvw.values
     && left_point.offsets = right_point.offsets
     && left_point.values = right_point.values

let test_crossing_and_provenance () =
  let output = Intersection_analysis.run ~grain:1 ~include_coplanar:false
      ~collision:(collision ()) (source ()) |> get in
  check (Geometry.point_count output = 2 && Geometry.vertex_count output = 0
      && Geometry.primitive_count output = 0)
    "Intersection Analysis did not emit the crossing segment endpoints";
  let x, y, z = positions output in
  check (x = [|0.5;0.5|] && y = [|1.5;0.5|] && z = [|0.;0.|])
    (Printf.sprintf "Intersection Analysis crossing positions: (%g,%g,%g), (%g,%g,%g)"
      x.(0) y.(0) z.(0) x.(1) y.(1) z.(1));
  let inputs = int_rows "sourceinput" output
  and primitives = int_rows "sourceprim" output
  and uvw = float_rows "sourceprimuv" output
  and points = int_rows "sourcepoint" output in
  check (inputs.offsets = [|0;2;4|]
      && inputs.values = [|0;1;0;1|]
      && primitives.offsets = inputs.offsets
      && primitives.values = [|0;0;0;0|]
      && uvw.offsets = [|0;6;12|]
      && Array.length uvw.values = 12
      && points.offsets = inputs.offsets
      && points.values = [|-1;-1;-1;-1|])
    "Intersection Analysis aligned provenance";
  for row = 0 to Geometry.point_count output - 1 do
    let first, last = uvw.offsets.(row), uvw.offsets.(row + 1) in
    for slot = first / 3 to (last / 3) - 1 do
      let base = slot * 3 in
      check (Float.abs (uvw.values.(base) +. uvw.values.(base + 1)
          +. uvw.values.(base + 2) -. 1.) < 1e-12)
        "Intersection Analysis barycentric row does not sum to one"
    done
  done;
  let no_attributes = Intersection_analysis.run ~collision:(collision ())
      ~include_coplanar:false ~input_attribute:None ~primitive_attribute:None
      ~primitive_uvw_attribute:None ~point_attribute:None (source ()) |> get in
  check (Geometry.point_count no_attributes = 2
      && Geometry.attributes no_attributes = [])
    "Intersection Analysis requires optional provenance outputs"

let test_coplanar_and_self () =
  let output = Intersection_analysis.run ~collision:(collision ()) (source ())
      |> get in
  check (Geometry.point_count output = 5)
    "Intersection Analysis did not include the coplanar overlap polygon";
  check (Array.exists (fun point -> point >= 3)
      (int_rows "sourcepoint" output).values)
    "Intersection Analysis lost existing incident-point provenance";
  let duplicate = geometry [|0.,0.,0.; 2.,0.,0.; 0.,2.,0.|]
      [|0;1;2; 2;1;0|] in
  let duplicate_output = Intersection_analysis.run duplicate |> get in
  check (Geometry.point_count duplicate_output = 3)
    "Intersection Analysis did not weld duplicate-face overlap points";
  let duplicate_without = Intersection_analysis.run ~include_coplanar:false
      duplicate |> get in
  check (Geometry.point_count duplicate_without = 0)
    "Intersection Analysis ignored include_coplanar=false";
  let adjacent = geometry
      [|0.,0.,0.; 1.,0.,0.; 1.,1.,0.; 0.,1.,0.|]
      [|0;1;2; 0;2;3|] in
  check (Geometry.point_count (Intersection_analysis.run adjacent |> get) = 0)
    "Intersection Analysis exposed an ordinary shared edge";
  let crossing = geometry
      [|0.,0.,0.; 2.,0.,0.; 0.,2.,0.;
        0.5,-0.5,-1.; 0.5,1.5,1.; 0.5,1.5,-1.|]
      [|0;1;2; 3;4;5|] |> Intersection_analysis.run |> get in
  check (Geometry.point_count crossing = 2
      && (int_rows "sourceinput" crossing).values = [|0;0;0;0|]
      && (int_rows "sourceprim" crossing).values = [|0;1;0;1|])
    "Intersection Analysis AxA provenance"

let test_curve_intersections () =
  let horizontal = open_curve [|-1.,0.,0.; 1.,0.,0.|]
  and vertical = open_curve [|0.,-1.,0.; 0.,1.,0.|] in
  let crossing = Intersection_analysis.run ~collision:vertical horizontal |> get in
  check (Geometry.point_count crossing = 1)
    "Intersection Analysis curve/curve crossing cardinality";
  let x, y, z = positions crossing
  and inputs = int_rows "sourceinput" crossing
  and primitives = int_rows "sourceprim" crossing
  and uvw = float_rows "sourceprimuv" crossing
  and points = int_rows "sourcepoint" crossing in
  check (x = [|0.|] && y = [|0.|] && z = [|0.|]
      && inputs.offsets = [|0;2|] && inputs.values = [|0;1|]
      && primitives.values = [|0;0|]
      && uvw.offsets = [|0;6|]
      && uvw.values = [|0.5;0.;0.; 0.5;0.;0.|]
      && points.values = [|-1;-1|])
    "Intersection Analysis curve/curve aligned provenance";
  let joint = open_curve [|-1.,0.,0.; 0.,0.,0.; 1.,0.,0.|] in
  let joint_hit = Intersection_analysis.run ~collision:vertical joint |> get in
  check (Geometry.point_count joint_hit = 1
      && (int_rows "sourceinput" joint_hit).values = [|0;1|]
      && (int_rows "sourceprim" joint_hit).values = [|0;0|]
      && (float_rows "sourceprimuv" joint_hit).values
        = [|0.5;0.;0.; 0.5;0.;0.|]
      && (int_rows "sourcepoint" joint_hit).values = [|1;-1|])
    "Intersection Analysis duplicated an internal curve-vertex incidence";
  let overlap = Intersection_analysis.run
      ~collision:(open_curve [|0.,0.,0.; 2.,0.,0.|]) horizontal |> get in
  let ox, oy, oz = positions overlap and opoints = int_rows "sourcepoint" overlap in
  check (Geometry.point_count overlap = 2 && close ox.(0) 0. && close ox.(1) 1.
      && Array.for_all (fun value -> close value 0.) oy
      && Array.for_all (fun value -> close value 0.) oz
      && opoints.values = [|-1;0;1;-1|])
    (Printf.sprintf
      "Intersection Analysis collinear curve overlap endpoints: count=%d x=%s points=%s"
      (Geometry.point_count overlap)
      (String.concat "," (Array.to_list (Array.map string_of_float ox)))
      (String.concat "," (Array.to_list (Array.map string_of_int opoints.values))));
  let bow = open_curve
      [|-1.,-1.,0.; 1.,1.,0.; -1.,1.,0.; 1.,-1.,0.|] in
  let self = Intersection_analysis.run bow |> get in
  let sx, sy, sz = positions self and sprim = int_rows "sourceprim" self
  and sinput = int_rows "sourceinput" self
  and suv = float_rows "sourceprimuv" self in
  check (Geometry.point_count self = 1 && sx = [|0.|] && sy = [|0.|]
      && sz = [|0.|] && sinput.values = [|0;0|]
      && sprim.values = [|0;0|] && Array.length suv.values = 6
      && close suv.values.(0) (1. /. 6.) && suv.values.(1) = 0.
      && suv.values.(2) = 0. && close suv.values.(3) (5. /. 6.)
      && suv.values.(4) = 0. && suv.values.(5) = 0.)
    (Printf.sprintf
      "Intersection Analysis same-primitive curve self-crossing provenance: count=%d p=(%s;%s;%s) input=%s prim=%s uv=%s"
      (Geometry.point_count self)
      (String.concat "," (Array.to_list (Array.map string_of_float sx)))
      (String.concat "," (Array.to_list (Array.map string_of_float sy)))
      (String.concat "," (Array.to_list (Array.map string_of_float sz)))
      (String.concat "," (Array.to_list (Array.map string_of_int sinput.values)))
      (String.concat "," (Array.to_list (Array.map string_of_int sprim.values)))
      (String.concat "," (Array.to_list (Array.map string_of_float suv.values))));
  let square = closed_curve
      [|0.,0.,0.; 1.,0.,0.; 1.,1.,0.; 0.,1.,0.|] in
  check (Geometry.point_count (Intersection_analysis.run square |> get) = 0)
    "Intersection Analysis exposed closed-curve adjacency contacts"

let test_curve_triangle_intersections () =
  let triangle = geometry [|0.,0.,0.; 1.,0.,0.; 0.,1.,0.|] [|0;1;2|] in
  let piercing = open_curve [|0.25,0.25,-1.; 0.25,0.25,1.|] in
  let hit = Intersection_analysis.run ~collision:triangle piercing |> get in
  let x, y, z = positions hit and uvw = float_rows "sourceprimuv" hit in
  check (Geometry.point_count hit = 1 && x = [|0.25|] && y = [|0.25|]
      && z = [|0.|]
      && uvw.values = [|0.5;0.;0.; 0.5;0.25;0.25|])
    "Intersection Analysis segment/triangle crossing provenance";
  let reverse = Intersection_analysis.run ~collision:piercing triangle |> get in
  check (Geometry.point_count reverse = 1
      && (float_rows "sourceprimuv" reverse).values
        = [|0.5;0.25;0.25; 0.5;0.;0.|])
    "Intersection Analysis triangle/segment provenance ordering";
  let coplanar = open_curve [|-1.,0.25,0.; 1.,0.25,0.|] in
  let clipped = Intersection_analysis.run ~collision:triangle coplanar |> get in
  let cx, cy, cz = positions clipped in
  check (Geometry.point_count clipped = 2 && cx = [|0.;0.75|]
      && cy = [|0.25;0.25|] && cz = [|0.;0.|])
    "Intersection Analysis coplanar segment/triangle clipping";
  check (Geometry.point_count (Intersection_analysis.run ~include_coplanar:false
      ~collision:triangle coplanar |> get) = 0)
    "Intersection Analysis ignored include_coplanar for segment/triangle";
  let mixed = geometry_with_topology
      [|0.,0.,0.; 1.,0.,0.; 0.,1.,0.;
        0.25,0.25,-1.; 0.25,0.25,1.|]
      [|0;1;2; 3;4|] [|0;3;5|]
      [|Topology.Polygon; Topology.Open_polyline|] in
  let mixed_hit = Intersection_analysis.run mixed |> get in
  check (Geometry.point_count mixed_hit = 1
      && (int_rows "sourceinput" mixed_hit).values = [|0;0|]
      && (int_rows "sourceprim" mixed_hit).values = [|0;1|]
      && (float_rows "sourceprimuv" mixed_hit).values
        = [|0.5;0.25;0.25; 0.5;0.;0.|])
    "Intersection Analysis mixed-geometry self analysis"

let test_parallel_exact () =
  let source = Plane_generators.grid ~grain:31 ~counts:Plane_generators.Grid_point_counts
      ~connectivity:Plane_generators.Grid_alternating_triangles
      ~columns:80 ~rows:60 ~size:20. () |> get in
  let collision = Transform_ops.transform ~grain:31
      (Rays_math.Mat4.rotation_x (Float.pi /. 2.)) source in
  let run domains = Parallel.run ~domains (fun () ->
    Intersection_analysis.run ~grain:31 ~include_coplanar:false ~collision source
    |> get) in
  let one = run 1 and four = run 4 in
  check (Geometry.point_count one > 0)
    "Intersection Analysis scale fixture produced no points";
  check (equal_output one four)
    "Intersection Analysis differs across one and four domains";
  let horizontal_count = 96 and vertical_count = 72 in
  let horizontal = segment_network horizontal_count (fun line ->
      let y = -.1. +. (2. *. float_of_int line
        /. float_of_int (horizontal_count - 1)) in
      (-1., y, 0.), (1., y, 0.))
  and vertical = segment_network vertical_count (fun line ->
      let x = -.1. +. (2. *. float_of_int line
        /. float_of_int (vertical_count - 1)) in
      (x, -1., 0.), (x, 1., 0.)) in
  let run_curves domains = Parallel.run ~domains (fun () ->
    Intersection_analysis.run ~grain:17 ~collision:vertical horizontal |> get) in
  let one = run_curves 1 and four = run_curves 4 in
  check (Geometry.point_count one = horizontal_count * vertical_count)
    "Intersection Analysis curve scale fixture cardinality";
  check (equal_output one four)
    "Intersection Analysis curve output differs across one and four domains"

let run () =
  test_crossing_and_provenance ();
  test_coplanar_and_self ();
  test_curve_intersections ();
  test_curve_triangle_intersections ();
  test_parallel_exact ();
  print_endline "intersection analysis tests passed"
