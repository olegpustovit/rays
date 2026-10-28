open Rdk
open Rdk_test_support

module Constraints = Rdk_boolean.Boolean_constraints
module Coplanar = Rdk_boolean.Boolean_coplanar
module Refinement = Rdk_boolean.Boolean_refinement
module Complex = Rdk_boolean.Boolean_complex

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
  constraints, refinement, Complex.build constraints refinement |> get_rdk

let test_disjoint_closed_surfaces () =
  let tetra offset = geometry
      [|offset,0.,0.; offset +. 1.,0.,0.; offset,1.,0.; offset,0.,1.|]
      [|0;2;1; 0;1;3; 1;2;3; 2;0;3|] in
  let _, _, value = pipeline (tetra 0.) (tetra 10.) in
  check (Complex.vertex_count value = 8) "disjoint tetrahedra lost vertices";
  check (Complex.facet_count value = 8) "disjoint tetrahedra lost facets";
  check (Complex.edge_count value = 12) "disjoint tetrahedra edge table is wrong";
  for edge = 0 to Complex.edge_count value - 1 do
    let first, last = Complex.edge_incident_range value edge in
    check (last - first = 2) "closed tetrahedron edge is not two-manifold"
  done

let test_identical_facet_ownership () =
  let left = triangle [|0.,0.,0.; 2.,0.,0.; 0.,2.,0.|]
  and right = triangle [|0.,2.,0.; 2.,0.,0.; 0.,0.,0.|] in
  let _, _, value = pipeline left right in
  check (Complex.vertex_count value = 3 && Complex.facet_count value = 1)
    "identical facets were not merged in the two-complex";
  let first, last = Complex.facet_member_range value 0 in
  check (last - first = 2) "identical facet lost an owner";
  check (Complex.member_side value first = Complex.Left
      && Complex.member_side value (first + 1) = Complex.Right
      && Complex.member_winding value first = 1
      && Complex.member_winding value (first + 1) = -1)
    "identical facet winding vector is wrong"

let test_partial_coplanar_overlap () =
  let left = triangle [|0.,0.,0.; 4.,0.,0.; 2.,4.,0.|]
  and right = triangle [|0.,3.,0.; 4.,3.,0.; 2.,-1.,0.|] in
  let _, _, value = pipeline left right in
  check (Complex.vertex_count value = 12)
    "coplanar overlay did not globally deduplicate exact vertices";
  check (Complex.facet_count value = 10)
    "coplanar overlay did not merge four shared refined facets";
  let shared = ref 0 in
  for facet = 0 to Complex.facet_count value - 1 do
    let first, last = Complex.facet_member_range value facet in
    if last - first = 2 then incr shared
    else check (last - first = 1) "facet has an unexpected ownership count"
  done;
  check (!shared = 4) "partial overlap has the wrong coincident facet count"

let test_transverse_radial_bundle () =
  let left = triangle [|0.,0.,0.; 2.,0.,0.; 0.,2.,0.|]
  and right = triangle
      [|0.5,-0.5,-1.; 0.5,1.5,1.; 0.5,1.5,-1.|] in
  let _, _, value = pipeline left right in
  let seam = ref (-1) in
  for edge = 0 to Complex.edge_count value - 1 do
    let first = Complex.edge_first value edge and second = Complex.edge_second value edge in
    let ax, ay, az = Complex.approximate_vertex value first
    and bx, by, bz = Complex.approximate_vertex value second in
    let matches x0 y0 z0 x1 y1 z1 =
      close ax x0 && close ay y0 && close az z0
      && close bx x1 && close by y1 && close bz z1 in
    if matches 0.5 0.5 0. 0.5 1.5 0.
        || matches 0.5 1.5 0. 0.5 0.5 0. then seam := edge
  done;
  check (!seam >= 0) "transverse intersection is not a complex edge";
  let first, last = Complex.edge_incident_range value !seam in
  check (last - first = 4)
    "transverse seam does not retain its four incident surface charts"

let signature value =
  Array.init (Complex.vertex_count value) (Complex.approximate_vertex value),
  Array.init (Complex.facet_count value) (fun facet ->
      Complex.facet_vertex value facet 0,
      Complex.facet_vertex value facet 1,
      Complex.facet_vertex value facet 2,
      Complex.facet_member_range value facet),
  Array.init (Complex.edge_count value) (fun edge ->
      Complex.edge_first value edge,
      Complex.edge_second value edge,
      Complex.edge_incident_range value edge)

let test_domain_exactness () =
  let left = triangle [|0.,0.,0.; 4.,0.,0.; 2.,4.,0.|]
  and right = triangle [|0.,3.,0.; 4.,3.,0.; 2.,-1.,0.|] in
  let run domains = Parallel.run ~domains (fun () ->
      let _, _, value = pipeline left right in signature value) in
  if run 1 <> run 4 then failf "Boolean complex differs between domain counts"

let test_cancellation () =
  let left = triangle [|0.,0.,0.; 2.,0.,0.; 0.,2.,0.|]
  and right = triangle [|10.,0.,0.; 12.,0.,0.; 10.,2.,0.|] in
  let constraints, refinement, _ = pipeline left right in
  let cancel = Cancel.create () in Cancel.cancel cancel;
  match Complex.build ~cancel constraints refinement with
  | Error error when Error.code error = "cancelled" -> ()
  | Error error -> failf "unexpected cancellation error: %s" (Error.to_string error)
  | Ok _ -> failf "cancelled Boolean complex assembly completed"

let run () =
  test_disjoint_closed_surfaces ();
  test_identical_facet_ownership ();
  test_partial_coplanar_overlap ();
  test_transverse_radial_bundle ();
  test_domain_exactness ();
  test_cancellation ()
