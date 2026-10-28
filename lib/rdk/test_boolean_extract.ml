open Rdk
open Rdk_test_support

module Constraints = Rdk_boolean.Boolean_constraints
module Coplanar = Rdk_boolean.Boolean_coplanar
module Refinement = Rdk_boolean.Boolean_refinement
module Complex = Rdk_boolean.Boolean_complex
module Radial = Rdk_boolean.Boolean_radial
module Weiler = Rdk_boolean.Boolean_weiler
module Cells = Rdk_boolean.Boolean_cells
module Extract = Rdk_boolean.Boolean_extract


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

let polygon_geometry points vertex_points primitive_offsets =
  let count = Array.length points in
  let x = Array.init count (fun point -> let x, _, _ = points.(point) in x)
  and y = Array.init count (fun point -> let _, y, _ = points.(point) in y)
  and z = Array.init count (fun point -> let _, _, z = points.(point) in z) in
  let topology = Topology.polygons_owned ~point_count:count ~vertex_points
      ~primitive_offsets |> get_string in
  Geometry.create ~positions:(Packed.Float3.View.of_owned_exn ~x ~y ~z)
    ~topology () |> get_string

let tetra ~origin:(ox,oy,oz) size = geometry
    [|ox,oy,oz; ox+.size,oy,oz; ox,oy+.size,oz; ox,oy,oz+.size|]
    [|0;2;1; 0;1;3; 1;2;3; 2;0;3|]

let two_transverse_tetra_shells () = geometry
    [|0.,0.,0.; 2.,0.,0.; 0.,2.,0.; 0.,0.,2.;
      0.5,0.2,0.2; 2.5,0.2,0.2; 0.5,2.2,0.2; 0.5,0.2,2.2|]
    [|0;2;1; 0;1;3; 1;2;3; 2;0;3;
      4;6;5; 4;5;7; 5;6;7; 6;4;7|]

let two_identical_tetra_shells () = geometry
    [|0.,0.,0.; 2.,0.,0.; 0.,2.,0.; 0.,0.,2.;
      0.,0.,0.; 2.,0.,0.; 0.,2.,0.; 0.,0.,2.|]
    [|0;2;1; 0;1;3; 1;2;3; 2;0;3;
      4;6;5; 4;5;7; 5;6;7; 6;4;7|]

let cube_quads () = polygon_geometry
    [|0.,0.,0.; 1.,0.,0.; 1.,1.,0.; 0.,1.,0.;
      0.,0.,1.; 1.,0.,1.; 1.,1.,1.; 0.,1.,1.|]
    [|0;3;2;1; 4;5;6;7; 0;1;5;4; 3;7;6;2; 0;4;7;3; 1;2;6;5|]
    [|0;4;8;12;16;20;24|]

let pipeline ?(resolve_left_self_intersections = false)
    ?(resolve_right_self_intersections = false) left right =
  let constraints = Constraints.build ~resolve_left_self_intersections
      ~resolve_right_self_intersections ~grain:1 ~left ~right () |> get_rdk in
  let coplanar = Coplanar.build ~grain:1 constraints |> get_rdk in
  let refinement = Refinement.build ~coplanar ~grain:1 constraints |> get_rdk in
  let complex = Complex.build constraints refinement |> get_rdk in
  let radial = Radial.build complex |> get_rdk in
  let weiler = Weiler.build complex radial |> get_rdk in
  let cells = Cells.build complex weiler |> get_rdk in
  complex, weiler, cells

let signed_volume geometry =
  let positions = Packed.Float3.View.view (Geometry.positions geometry)
  and topology = Topology.View.view (Geometry.topology geometry) in
  let volume = ref 0. in
  for triangle = 0 to Geometry.primitive_count geometry - 1 do
    let offset = triangle * 3 in
    let a = topology.vertex_points.(offset)
    and b = topology.vertex_points.(offset + 1)
    and c = topology.vertex_points.(offset + 2) in
    volume := !volume +.
      (positions.x.(a) *. ((positions.y.(b) *. positions.z.(c))
                           -. (positions.z.(b) *. positions.y.(c)))
       +. positions.y.(a) *. ((positions.z.(b) *. positions.x.(c))
                              -. (positions.x.(b) *. positions.z.(c)))
       +. positions.z.(a) *. ((positions.x.(b) *. positions.y.(c))
                              -. (positions.y.(b) *. positions.x.(c)))) /. 6.
  done;
  !volume

let signature geometry =
  let positions = Packed.Float3.View.view (Geometry.positions geometry)
  and topology = Topology.View.view (Geometry.topology geometry) in
  Array.copy positions.x, Array.copy positions.y, Array.copy positions.z,
  Array.copy topology.vertex_points, Array.copy topology.primitive_offsets

let test_materialization_validation () =
  let validate = Extract.View.validate_materialized in
  (match validate ~x:[|0.;1.;0.|] ~y:[|0.;0.;1.|] ~z:[|0.;0.;0.|]
      ~vertex_points:[|0;1;2|] () with
   | Ok () -> ()
   | Error error -> failf "valid rounded triangle failed: %s" (Error.to_string error));
  (match validate ~x:[|0.;0.|] ~y:[|0.;0.|] ~z:[|0.;0.|]
      ~vertex_points:[||] () with
   | Error error when Error.code error = "rounding_collision" -> ()
   | Error error -> failf "unexpected rounding collision error: %s" (Error.to_string error)
   | Ok () -> failf "rounded exact-point collision was accepted");
  (match validate ~x:[|0.;1.;2.|] ~y:[|0.;0.;0.|] ~z:[|0.;0.;0.|]
      ~vertex_points:[|0;1;2|] () with
   | Error error when Error.code error = "rounding_degenerate" -> ()
   | Error error -> failf "unexpected rounding degeneracy error: %s" (Error.to_string error)
   | Ok () -> failf "rounded collinear output triangle was accepted");
  (match validate ~allow_degenerate:true
      ~x:[|0.;1.;2.|] ~y:[|0.;0.;0.|] ~z:[|0.;0.;0.|]
      ~vertex_points:[|0;1;2|] () with
   | Ok () -> ()
   | Error error -> failf "explicit deferred rounding repair was rejected: %s"
       (Error.to_string error));
  let x, y, z, representatives, map = Extract.View.coalesce_positions
      ~complex_vertices:[|10;11;12;13;14|]
      ~x:[|0.;1.;1.;2.;1.|] ~y:[|0.;0.;0.;0.;0.|]
      ~z:[|0.;0.;0.;0.;0.|] in
  check (x = [|0.;1.;2.|] && y = [|0.;0.;0.|] && z = [|0.;0.;0.|]
      && representatives = [|10;11;13|] && map = [|0;1;1;2;1|])
    "rounded-point coalescing lost stable representatives after duplicate gaps"

let run () =
  test_materialization_validation ()
