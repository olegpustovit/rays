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

let source () = geometry
    [|0.,0.,0.; 2.,0.,0.; 0.,2.,0.; 10.,0.,0.; 12.,0.,0.; 10.,2.,0.|]
    [|0;1;2; 3;4;5|]

let collision () = geometry
    [|0.5,-0.5,-1.; 0.5,1.5,1.; 0.5,1.5,-1.;
      0.25,0.25,0.; 0.5,0.25,0.; 0.25,0.5,0.|]
    [|0;1;2; 3;4;5|]

let int_array name geometry =
  match Geometry.find_attribute ~owner:Attribute.Primitive name geometry with
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Int_array values -> Packed.Int_array.View.view values
       | _ -> fail (name ^ " has wrong storage"))
  | None -> fail ("missing " ^ name)

let ints name geometry =
  match Geometry.find_attribute ~owner:Attribute.Primitive name geometry with
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Int values -> values | _ -> fail (name ^ " has wrong storage"))
  | None -> fail ("missing " ^ name)

let group name geometry = Geometry.find_group ~owner:Group.Primitive name geometry
    |> Option.get

let topology_equal left right =
  let left = Topology.View.view (Geometry.topology left)
  and right = Topology.View.view (Geometry.topology right) in
  left.point_count = right.point_count
  && left.vertex_points = right.vertex_points
  && left.primitive_offsets = right.primitive_offsets
  && Bytes.equal left.primitive_kinds right.primitive_kinds

let output_signature geometry =
  let rows = int_array "hits" geometry in
  let selected = Array.init (Geometry.primitive_count geometry)
      (fun primitive -> Group.mem primitive (group "intersections" geometry)) in
  rows.offsets, rows.values, ints "hit_count" geometry, selected

let expect code = function
  | Error error when Error.code error = code -> ()
  | Error error -> fail (Printf.sprintf "expected %s, got %s: %s"
      code (Error.code error) (Error.to_string error))
  | Ok _ -> fail ("expected " ^ code)

let test_cross_and_coplanar () =
  let source = source () and collision = collision () in
  let temperature = Attribute.create_owned ~owner:Attribute.Point
      ~name:"temperature" (Attribute.Float [|0.;1.;2.;3.;4.;5.|]) |> get_string in
  let preserved = Group.init ~owner:Group.Primitive ~name:"preserved" 2
      (fun primitive -> primitive = 1) in
  let stale_hits = Attribute.create_owned ~owner:Attribute.Primitive ~name:"hits"
      (Attribute.Int [|9;9|]) |> get_string in
  let stale_intersections = Group.init ~owner:Group.Primitive
      ~name:"intersections" 2 (Fun.const true) in
  let source = source |> Geometry.with_attribute temperature |> get_string
      |> Geometry.with_attribute stale_hits |> get_string
      |> Geometry.with_group preserved |> get_string
      |> Geometry.with_group stale_intersections |> get_string in
  let output = Boolean_detect.run_checked ~grain:1 ~collision
      ~intersecting_group:(Some "intersections")
      ~intersections_attribute:"hits" ~count_attribute:"hit_count" source
      |> get in
  check (Geometry.topology output == Geometry.topology source
      && Geometry.positions output == Geometry.positions source)
    "Boolean Detect copied unchanged source geometry";
  let retained_temperature = Geometry.find_attribute ~owner:Attribute.Point
      "temperature" output |> Option.get
  and retained_group = Geometry.find_group ~owner:Group.Primitive "preserved"
      output |> Option.get in
  check (retained_temperature == temperature && retained_group == preserved)
    "Boolean Detect did not preserve unrelated source metadata";
  let rows = int_array "hits" output in
  check (rows.offsets = [|0;2;2|] && rows.values = [|0;1|]
      && ints "hit_count" output = [|2;0|]
      && Group.cardinality (group "intersections" output) = 1
      && Group.mem 0 (group "intersections" output))
    "Boolean Detect cross/coplanar primitive aggregation";
  let crossing_only = Boolean_detect.run_checked ~grain:1 ~collision
      ~include_coplanar:false ~intersecting_group:None
      ~intersections_attribute:"hits" source |> get in
  let crossing_rows = int_array "hits" crossing_only in
  check (crossing_rows.offsets = [|0;1;1|] && crossing_rows.values = [|0|])
    "Boolean Detect include_coplanar=false";
  check (topology_equal output source) "Boolean Detect changed topology values"

let test_tolerance_and_translation () =
  let single_source = geometry [|0.,0.,0.; 2.,0.,0.; 0.,2.,0.|] [|0;1;2|] in
  let separated = geometry
      [|2.01,0.,0.; 2.51,0.,0.; 2.01,0.5,0.|] [|0;1;2|] in
  let without = Boolean_detect.run_checked ~collision:separated
      ~intersections_attribute:"hits" single_source |> get |> int_array "hits" in
  let within = Boolean_detect.run_checked ~collision:separated ~tolerance:0.02
      ~intersections_attribute:"hits" single_source |> get |> int_array "hits" in
  check (without.values = [||] && within.values = [|0|])
    "Boolean Detect world-space tolerance";
  let move = Rays_math.Mat4.translation (Rays_math.Vec3.create 1e12 (-1e12) 1e12) in
  let moved_source = Transform_ops.transform move (source ())
  and moved_collision = Transform_ops.transform move (collision ()) in
  let origin_output = Boolean_detect.run_checked ~collision:(collision ())
      ~intersecting_group:(Some "intersections")
      ~intersections_attribute:"hits" ~count_attribute:"hit_count" (source ())
      |> get
  and moved_output = Boolean_detect.run_checked ~collision:moved_collision
      ~intersecting_group:(Some "intersections")
      ~intersections_attribute:"hits" ~count_attribute:"hit_count" moved_source
      |> get in
  check (output_signature origin_output = output_signature moved_output)
    "Boolean Detect changed under a large common translation"

let self_detect geometry =
  Boolean_detect.run_checked ~collision:(Line_geometry.points [||]) ~intersecting_group:None
    ~self_intersecting_group:"self_intersections"
    ~self_intersections_attribute:"self_hits"
    ~self_count_attribute:"self_hit_count" geometry |> get

let test_self_intersections () =
  let crossing_source = geometry
      [|0.,0.,0.; 2.,0.,0.; 0.,2.,0.;
        0.5,-0.5,-1.; 0.5,1.5,1.; 0.5,1.5,-1.|]
      [|0;1;2; 3;4;5|] in
  let crossing = self_detect crossing_source in
  let rows = int_array "self_hits" crossing in
  check (rows.offsets = [|0;1;2|] && rows.values = [|1;0|]
      && ints "self_hit_count" crossing = [|1;1|]
      && Group.cardinality (group "self_intersections" crossing) = 2)
    "Boolean Detect AxA symmetric crossing output";
  let both = Boolean_detect.run_checked ~collision:(collision ())
      ~intersecting_group:(Some "cross") ~intersections_attribute:"cross_hits"
      ~self_intersecting_group:"self" ~self_intersections_attribute:"self_hits"
      crossing_source |> get in
  check (Geometry.find_group ~owner:Group.Primitive "cross" both <> None
      && Geometry.find_group ~owner:Group.Primitive "self" both <> None
      && (int_array "cross_hits" both).values <> [||]
      && (int_array "self_hits" both).values = [|1;0|])
    "Boolean Detect combined AxA/AxB atomic outputs";
  let adjacent = geometry
      [|0.,0.,0.; 1.,0.,0.; 1.,1.,0.; 0.,1.,0.|]
      [|0;1;2; 0;2;3|] |> self_detect in
  check ((int_array "self_hits" adjacent).values = [||])
    "Boolean Detect treated an ordinary shared edge as a self-intersection";
  let vertex_touch = geometry
      [|0.,0.,0.; 1.,0.,0.; 0.,1.,0.; 0.,0.,1.; 0.,1.,1.|]
      [|0;1;2; 0;3;4|] |> self_detect in
  check ((int_array "self_hits" vertex_touch).values = [||])
    "Boolean Detect treated an ordinary shared vertex as a self-intersection";
  let folded_overlap = geometry
      [|0.,0.,0.; 2.,0.,0.; 0.,2.,0.; 0.5,0.5,0.|]
      [|0;1;2; 0;1;3|] |> self_detect in
  check ((int_array "self_hits" folded_overlap).values = [|1;0|])
    "Boolean Detect missed coplanar overlap beyond a shared edge";
  let duplicate = geometry
      [|0.,0.,0.; 2.,0.,0.; 0.,2.,0.|]
      [|0;1;2; 2;1;0|] |> self_detect in
  check ((int_array "self_hits" duplicate).values = [|1;0|])
    "Boolean Detect missed duplicate overlapping primitives";
  let duplicate_without_coplanar = Boolean_detect.run_checked
      ~collision:(Line_geometry.points [||]) ~intersecting_group:None
      ~include_coplanar:false ~self_intersections_attribute:"self_hits"
      (geometry [|0.,0.,0.; 2.,0.,0.; 0.,2.,0.|]
        [|0;1;2; 2;1;0|]) |> get in
  check ((int_array "self_hits" duplicate_without_coplanar).values = [||])
    "Boolean Detect AxA include_coplanar=false";
  let quad = geometry [|0.,0.,0.; 1.,0.,0.; 1.,1.,0.; 0.,1.,0.|]
      [|0;1;2; 0;2;3|] in
  let topology = Topology.create_owned ~point_count:4
      ~vertex_points:[|0;1;2;3|] ~primitive_offsets:[|0;4|]
      ~primitive_kinds:[|Topology.Polygon|] |> get_string in
  let quad = Geometry.create ~positions:(Geometry.positions quad) ~topology ()
      |> get_string |> self_detect in
  check ((int_array "self_hits" quad).values = [||])
    "Boolean Detect exposed one polygon's triangulation diagonal as AxA"

let test_parallel_exact () =
  let source = Plane_generators.grid ~grain:31 ~counts:Plane_generators.Grid_point_counts
      ~connectivity:Plane_generators.Grid_alternating_triangles
      ~columns:80 ~rows:60 ~size:20. () |> get in
  let collision = Transform_ops.transform ~grain:31 (Rays_math.Mat4.rotation_x (Float.pi /. 2.))
      source in
  let run domains = Parallel.run ~domains (fun () ->
    Boolean_detect.run_checked ~grain:31 ~collision
      ~intersecting_group:(Some "intersections")
      ~intersections_attribute:"hits" ~count_attribute:"hit_count" source
    |> get) in
  let one = run 1 and four = run 4 in
  check (output_signature one = output_signature four)
    "Boolean Detect one/four-domain output drift";
  check (Group.cardinality (group "intersections" one) > 0)
    "Boolean Detect scale fixture found no intersections";
  let combined = Mesh_merge.run [source; collision] |> get in
  let run_self domains = Parallel.run ~domains (fun () -> self_detect combined) in
  let self_signature geometry =
    let rows = int_array "self_hits" geometry in
    let selected = Geometry.find_group ~owner:Group.Primitive
        "self_intersections" geometry |> Option.get in
    rows.offsets, rows.values, ints "self_hit_count" geometry,
    Array.init (Geometry.primitive_count geometry)
      (fun primitive -> Group.mem primitive selected) in
  let one = run_self 1 and four = run_self 4 in
  check (self_signature one = self_signature four)
    "Boolean Detect AxA one/four-domain output drift";
  check (Array.length (int_array "self_hits" one).values > 0)
    "Boolean Detect AxA scale fixture found no intersections"

let run () =
  test_cross_and_coplanar ();
  test_tolerance_and_translation ();
  test_self_intersections ();
  test_parallel_exact ();
  print_endline "boolean detect tests passed"
