open Rays
open Rdk
open Rdk_test_support

let float3_attribute owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Float3 values -> Packed.Float3.Private.view values
       | _ -> fail (name ^ " has unexpected storage"))
  | None -> fail ("missing float3 attribute " ^ name)

let float2_attribute owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Float2 values -> Packed.Float2.Private.view values
       | _ -> fail (name ^ " has unexpected storage"))
  | None -> fail ("missing float2 attribute " ^ name)

let equal_storage left right = match Attribute.storage left, Attribute.storage right with
  | Attribute.Float left, Attribute.Float right -> left = right
  | Attribute.Int left, Attribute.Int right -> left = right
  | Attribute.Int_array left, Attribute.Int_array right ->
      let left = Packed.Int_array.Private.view left
      and right = Packed.Int_array.Private.view right in
      left.offsets = right.offsets && left.values = right.values
  | Attribute.Float_array left, Attribute.Float_array right ->
      let left = Packed.Float_array.Private.view left
      and right = Packed.Float_array.Private.view right in
      left.offsets = right.offsets && left.values = right.values
  | Attribute.Text left, Attribute.Text right -> left = right
  | Attribute.Float2 left, Attribute.Float2 right ->
      let left = Packed.Float2.Private.view left
      and right = Packed.Float2.Private.view right in
      left.x = right.x && left.y = right.y
  | Attribute.Float3 left, Attribute.Float3 right ->
      let left = Packed.Float3.Private.view left
      and right = Packed.Float3.Private.view right in
      left.x = right.x && left.y = right.y && left.z = right.z
  | Attribute.Float4 left, Attribute.Float4 right ->
      let left = Packed.Float4.Private.view left
      and right = Packed.Float4.Private.view right in
      left.x = right.x && left.y = right.y && left.z = right.z
      && left.w = right.w
  | _ -> false

let equal_attribute left right =
  Attribute.owner left = Attribute.owner right
  && String.equal (Attribute.name left) (Attribute.name right)
  && String.equal (Attribute.kind_name left) (Attribute.kind_name right)
  && equal_storage left right

let equal_geometry left right =
  let left_positions = positions left and right_positions = positions right
  and left_topology = Topology.Private.view (Geometry.topology left)
  and right_topology = Topology.Private.view (Geometry.topology right) in
  left_positions.x = right_positions.x
  && left_positions.y = right_positions.y
  && left_positions.z = right_positions.z
  && left_topology.point_count = right_topology.point_count
  && left_topology.vertex_points = right_topology.vertex_points
  && left_topology.primitive_offsets = right_topology.primitive_offsets
  && Bytes.equal left_topology.primitive_kinds right_topology.primitive_kinds
  && List.equal equal_attribute (Geometry.attributes left) (Geometry.attributes right)

let check_default_compatibility () =
  let geometry = Plane_generators.grid ~columns:1 ~rows:1 ~size:2. () |> get_ok in
  let point = positions geometry
  and topology = Topology.Private.view (Geometry.topology geometry)
  and normals = float3_attribute Attribute.Point "N" geometry in
  check (point.x = [|-1.; 1.; -1.; 1.|]
      && point.y = [|0.; 0.; 0.; 0.|]
      && point.z = [|-1.; -1.; 1.; 1.|])
    "default Grid point ordering changed";
  check (topology.vertex_points = [|0; 2; 3; 0; 3; 1|]
      && topology.primitive_offsets = [|0; 3; 6|]
      && topology.primitive_kinds = Bytes.make 2 '\000')
    "default Grid topology/winding changed";
  check (normals.x = [|0.;0.;0.;0.|] && normals.y = [|1.;1.;1.;1.|]
      && normals.z = [|0.;0.;0.;0.|])
    "default Grid normals changed"

let check_connectivity () =
  let make connectivity = Plane_generators.grid ~connectivity ~columns:3 ~rows:2 ~size:2. ()
      |> get_ok in
  let points = make Plane_generators.Grid_points in
  check (Geometry.point_count points = 12 && Geometry.vertex_count points = 0
      && Geometry.primitive_count points = 0) "Grid points cardinality";
  let rows = make Plane_generators.Grid_rows in
  let row_topology = Topology.Private.view (Geometry.topology rows) in
  check (Geometry.vertex_count rows = 12 && Geometry.primitive_count rows = 3
      && row_topology.vertex_points = Array.init 12 Fun.id
      && row_topology.primitive_offsets = [|0;4;8;12|]
      && row_topology.primitive_kinds = Bytes.make 3 '\001')
    "Grid row-polyline topology";
  let columns = make Plane_generators.Grid_columns in
  let column_topology = Topology.Private.view (Geometry.topology columns) in
  check (Geometry.vertex_count columns = 12 && Geometry.primitive_count columns = 4
      && column_topology.vertex_points = [|0;4;8; 1;5;9; 2;6;10; 3;7;11|]
      && column_topology.primitive_offsets = [|0;3;6;9;12|])
    "Grid column-polyline topology";
  let both = make Plane_generators.Grid_rows_and_columns in
  check (Geometry.vertex_count both = 24 && Geometry.primitive_count both = 7)
    "Grid row-and-column cardinality";
  let quads = make Plane_generators.Grid_quads in
  let quad_topology = Topology.Private.view (Geometry.topology quads) in
  check (Geometry.vertex_count quads = 24 && Geometry.primitive_count quads = 6
      && Array.sub quad_topology.vertex_points 0 4 = [|0;4;5;1|])
    "Grid quad topology";
  let triangles = make Plane_generators.Grid_triangles
  and reversed = make Plane_generators.Grid_reverse_triangles
  and alternating = make Plane_generators.Grid_alternating_triangles in
  let regular = Topology.Private.view (Geometry.topology triangles)
  and reversed_topology = Topology.Private.view (Geometry.topology reversed)
  and alternating_topology = Topology.Private.view (Geometry.topology alternating) in
  check (Geometry.vertex_count triangles = 36
      && Geometry.primitive_count triangles = 12
      && Array.sub regular.vertex_points 0 6 = [|0;4;5;0;5;1|])
    "Grid triangle topology";
  check (Array.sub reversed_topology.vertex_points 0 6 = [|0;4;1;1;4;5|])
    "Grid reverse-triangle topology";
  check (Array.sub alternating_topology.vertex_points 0 12
      = [|0;4;5;0;5;1; 1;5;2;2;5;6|])
    "Grid alternating-triangle topology";
  List.iter (fun geometry ->
    let point = positions geometry
    and topology = Topology.Private.view (Geometry.topology geometry)
    and normal = float3_attribute Attribute.Point "N" geometry in
    for primitive = 0 to Geometry.primitive_count geometry - 1 do
      let first = topology.primitive_offsets.(primitive) in
      let a = topology.vertex_points.(first)
      and b = topology.vertex_points.(first + 1)
      and c = topology.vertex_points.(first + 2) in
      let ux = point.x.(b) -. point.x.(a)
      and uy = point.y.(b) -. point.y.(a)
      and uz = point.z.(b) -. point.z.(a)
      and vx = point.x.(c) -. point.x.(a)
      and vy = point.y.(c) -. point.y.(a)
      and vz = point.z.(c) -. point.z.(a) in
      let nx = (uy *. vz) -. (uz *. vy)
      and ny = (uz *. vx) -. (ux *. vz)
      and nz = (ux *. vy) -. (uy *. vx) in
      check ((nx *. normal.x.(a)) +. (ny *. normal.y.(a))
          +. (nz *. normal.z.(a)) > 0.)
        "Grid polygon winding disagrees with N"
    done) [quads; triangles; reversed; alternating]

let check_count_modes () =
  let point_counts = Plane_generators.grid ~counts:Plane_generators.Grid_point_counts
      ~connectivity:Plane_generators.Grid_quads ~columns:4 ~rows:3 ~size:2. () |> get_ok in
  check (Geometry.point_count point_counts = 12
      && Geometry.primitive_count point_counts = 6)
    "Grid point-count resolution";
  let singleton = Plane_generators.grid ~counts:Plane_generators.Grid_point_counts
      ~connectivity:Plane_generators.Grid_points ~center:(Rays_math.Vec3.create 1. 2. 3.)
      ~uv_attribute:"uv" ~columns:1 ~rows:1 ~size:2. () |> get_ok in
  let point = positions singleton
  and uv = float2_attribute Attribute.Point "uv" singleton in
  check (point.x = [|1.|] && point.y = [|2.|] && point.z = [|3.|]
      && uv.x = [|0.5|] && uv.y = [|0.5|])
    "Grid singleton point lattice"

let check_validation () =
  expect_code "invalid_parameter"
    (Plane_generators.grid ~grain:0 ~columns:1 ~rows:1 ~size:1. ());
  expect_code "invalid_parameter"
    (Plane_generators.grid ~columns:0 ~rows:1 ~size:1. ());
  expect_code "invalid_parameter"
    (Plane_generators.grid ~counts:Plane_generators.Grid_point_counts ~connectivity:Plane_generators.Grid_quads
       ~columns:1 ~rows:2 ~size:1. ());
  expect_code "invalid_parameter"
    (Plane_generators.grid ~columns:1 ~rows:1 ~size:Float.nan ());
  expect_code "invalid_parameter"
    (Plane_generators.grid ~width:(-1.) ~columns:1 ~rows:1 ~size:1. ());
  expect_code "invalid_parameter"
    (Plane_generators.grid ~center:(Rays_math.Vec3.create Float.nan 0. 0.)
       ~columns:1 ~rows:1 ~size:1. ());
  expect_code "invalid_parameter"
    (Plane_generators.grid ~orientation:(Plane_generators.Grid_axes {
         horizontal=Rays_math.Vec3.zero; vertical=Rays_math.Vec3.unit_z })
       ~columns:1 ~rows:1 ~size:1. ());
  expect_code "invalid_parameter"
    (Plane_generators.grid ~orientation:(Plane_generators.Grid_axes {
         horizontal=Rays_math.Vec3.unit_x; vertical=Rays_math.Vec3.unit_x })
       ~columns:1 ~rows:1 ~size:1. ());
  expect_code "invalid_parameter"
    (Plane_generators.grid ~uv_attribute:"P" ~columns:1 ~rows:1 ~size:1. ());
  expect_code "invalid_parameter"
    (Plane_generators.grid ~columns:max_int ~rows:max_int ~size:1. ());
  expect_code "invalid_parameter"
    (Plane_generators.grid ~counts:Plane_generators.Grid_point_counts ~connectivity:Plane_generators.Grid_points
       ~columns:max_int ~rows:2 ~size:1. ());
  expect_code "invalid_parameter"
    (Plane_generators.grid ~center:(Rays_math.Vec3.create max_float 0. 0.) ~width:max_float
       ~columns:1 ~rows:1 ~size:1. ());
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  expect_code "cancelled"
    (Plane_generators.grid ~cancel:cancelled ~columns:500 ~rows:500 ~size:1. ())

let check_parallel_exact () =
  let connectivities = [
    Plane_generators.Grid_points; Plane_generators.Grid_rows; Plane_generators.Grid_columns;
    Plane_generators.Grid_rows_and_columns; Plane_generators.Grid_quads; Plane_generators.Grid_triangles;
    Plane_generators.Grid_alternating_triangles; Plane_generators.Grid_reverse_triangles ] in
  List.iter (fun connectivity ->
    let run domains = Parallel.run ~domains (fun () ->
      Plane_generators.grid ~grain:1024 ~connectivity
        ~orientation:(Plane_generators.Grid_axes {
          horizontal=Rays_math.Vec3.create 1. 2. 0.5;
          vertical=Rays_math.Vec3.create (-0.25) 0.75 2. })
        ~center:(Rays_math.Vec3.create 3. (-2.) 5.) ~width:40. ~height:25.
        ~rotation:0.37 ~uv_attribute:"uv"
        ~columns:700 ~rows:500 ~size:1. () |> get_ok) in
    let one = run 1 and many = run 4 in
    check (equal_geometry one many)
      ("one-domain and four-domain Grid differ for " ^
       (match connectivity with
        | Plane_generators.Grid_points -> "points" | Plane_generators.Grid_rows -> "rows"
        | Plane_generators.Grid_columns -> "columns"
        | Plane_generators.Grid_rows_and_columns -> "rows+columns"
        | Plane_generators.Grid_quads -> "quads" | Plane_generators.Grid_triangles -> "triangles"
        | Plane_generators.Grid_alternating_triangles -> "alternating triangles"
        | Plane_generators.Grid_reverse_triangles -> "reverse triangles"));
    check (Geometry.point_count one = 351_201) "Grid scale point cardinality")
    connectivities

let run () =
  check_default_compatibility ();
  check_connectivity ();
  check_count_modes ();
  check_validation ();
  check_parallel_exact ();
  print_endline "grid tests passed"
