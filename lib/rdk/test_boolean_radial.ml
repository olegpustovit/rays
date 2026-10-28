open Rdk
open Rdk_test_support

module Point = Rdk_exact.Implicit_point
module Constraints = Rdk_boolean.Boolean_constraints
module Coplanar = Rdk_boolean.Boolean_coplanar
module Refinement = Rdk_boolean.Boolean_refinement
module Complex = Rdk_boolean.Boolean_complex
module Radial = Rdk_boolean.Boolean_radial

let get_point = function Ok value -> value | Error _ -> failf "exact point construction failed"
let close left right = abs_float (left -. right) <= 1e-11

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

let triangle points = geometry points [|0;1;2|]

let pipeline left right =
  let constraints = Constraints.build ~grain:1 ~left ~right () |> get_rdk in
  let coplanar = Coplanar.build ~grain:1 constraints |> get_rdk in
  let refinement = Refinement.build ~coplanar ~grain:1 constraints |> get_rdk in
  let complex = Complex.build constraints refinement |> get_rdk in
  complex, Radial.build complex |> get_rdk

let test_exact_radial_dot () =
  let source = Point.source
      ~x:[|0.;0.;1.;2.;-1.;0.;0.|]
      ~y:[|0.;0.;0.;0.;0.;1.;0.|]
      ~z:[|0.;1.;0.;0.5;0.2;0.4;0.5|] |> get_point in
  let points = Array.init 7 (fun point -> Point.explicit source point |> get_point) in
  check (Point.radial_dot points.(0) points.(1) points.(2) points.(3)
      = Predicates.Positive) "same projected ray has non-positive dot";
  check (Point.radial_dot points.(0) points.(1) points.(2) points.(4)
      = Predicates.Negative) "opposite projected ray has non-negative dot";
  check (Point.radial_dot points.(0) points.(1) points.(2) points.(5)
      = Predicates.Zero) "orthogonal projected rays have non-zero dot";
  check (Point.radial_dot points.(0) points.(1) points.(2) points.(6)
      = Predicates.Zero) "point on the edge has non-zero radial projection"

let seam_bundle () =
  let left = triangle [|0.,0.,0.; 2.,0.,0.; 0.,2.,0.|]
  and right = triangle
      [|0.5,-0.5,-1.; 0.5,1.5,1.; 0.5,1.5,-1.|] in
  let complex, radial = pipeline left right in
  let seam = ref (-1) in
  for edge = 0 to Complex.edge_count complex - 1 do
    let first = Complex.edge_first complex edge and second = Complex.edge_second complex edge in
    let ax, ay, az = Complex.approximate_vertex complex first
    and bx, by, bz = Complex.approximate_vertex complex second in
    if ((close ax 0.5 && close ay 0.5 && close az 0.
          && close bx 0.5 && close by 1.5 && close bz 0.)
        || (close bx 0.5 && close by 0.5 && close bz 0.
          && close ax 0.5 && close ay 1.5 && close az 0.)) then seam := edge
  done;
  check (!seam >= 0) "radial seam edge is missing";
  complex, radial, !seam

let test_transverse_cyclic_order () =
  let complex, radial, seam = seam_bundle () in
  let first, last = Radial.incident_range radial seam in
  check (last - first = 4) "radial seam does not contain four charts";
  let sides = Array.init 4 (fun index ->
      let facet = Radial.incident_facet radial (first + index) in
      let member, _ = Complex.facet_member_range complex facet in
      Complex.member_side complex member) in
  for index = 0 to 3 do
    check (sides.(index) <> sides.((index + 1) mod 4))
      "two charts from one surface are adjacent in the radial cycle"
  done

let test_cancellation () =
  let complex, _, _ = seam_bundle () in
  let cancel = Cancel.create () in Cancel.cancel cancel;
  match Radial.build ~cancel complex with
  | Error error when Error.code error = "cancelled" -> ()
  | Error error -> failf "unexpected cancellation error: %s" (Error.to_string error)
  | Ok _ -> failf "cancelled radial ordering completed"

let run () =
  test_exact_radial_dot ();
  test_transverse_cyclic_order ();
  test_cancellation ()
