open Rdk

module Constraints = Rdk_boolean.Boolean_constraints
module Coplanar = Rdk_boolean.Boolean_coplanar
module Refinement = Rdk_boolean.Boolean_refinement
module Complex = Rdk_boolean.Boolean_complex
module Radial = Rdk_boolean.Boolean_radial
module Weiler = Rdk_boolean.Boolean_weiler

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

let triangle points = geometry points [|0;1;2|]

let pipeline left right =
  let constraints = Constraints.build ~grain:1 ~left ~right () |> get in
  let coplanar = Coplanar.build ~grain:1 constraints |> get in
  let refinement = Refinement.build ~coplanar ~grain:1 constraints |> get in
  let complex = Complex.build constraints refinement |> get in
  let radial = Radial.build complex |> get in
  complex, radial, Weiler.build complex radial |> get

let validate_neighbors value =
  for half = 0 to Weiler.half_facet_count value - 1 do
    for local = 0 to 2 do
      let neighbor = Weiler.neighbor value ~half_facet:half ~local_edge:local in
      check (neighbor >= 0 && neighbor < Weiler.half_facet_count value)
        "Weiler neighbor is out of range";
      let reverse = ref false in
      for other_local = 0 to 2 do
        if Weiler.neighbor value ~half_facet:neighbor ~local_edge:other_local = half then
          reverse := true
      done;
      check !reverse "Weiler adjacency is not symmetric"
    done
  done

let test_disconnected_closed_shells () =
  let tetra offset = geometry
      [|offset,0.,0.; offset +. 1.,0.,0.; offset,1.,0.; offset,0.,1.|]
      [|0;2;1; 0;1;3; 1;2;3; 2;0;3|] in
  let _, _, value = pipeline (tetra 0.) (tetra 10.) in
  check (Weiler.half_facet_count value = 16)
    "two tetrahedra have the wrong half-facet cardinality";
  check (Weiler.shell_count value = 4)
    "two disconnected closed surfaces should form four local shells";
  validate_neighbors value

let test_coincident_winding_vector () =
  let left = triangle [|0.,0.,0.; 2.,0.,0.; 0.,2.,0.|]
  and right = triangle [|0.,2.,0.; 2.,0.,0.; 0.,0.,0.|] in
  let complex, _, value = pipeline left right in
  check (Complex.facet_count complex = 1) "coincident facet was not merged";
  check (Weiler.facet_left_winding value 0 = 1
      && Weiler.facet_right_winding value 0 = -1)
    "coincident opposite facet winding vector is wrong";
  check (Weiler.shell_count value = 1)
    "an open coincident triangle should connect around its boundary";
  validate_neighbors value

let test_transverse_connectivity () =
  let left = triangle [|0.,0.,0.; 2.,0.,0.; 0.,2.,0.|]
  and right = triangle
      [|0.5,-0.5,-1.; 0.5,1.5,1.; 0.5,1.5,-1.|] in
  let _, _, value = pipeline left right in
  validate_neighbors value;
  for facet = 0 to (Weiler.half_facet_count value / 2) - 1 do
    let left = Weiler.facet_left_winding value facet
    and right = Weiler.facet_right_winding value facet in
    check ((abs left = 1 && right = 0) || (left = 0 && abs right = 1))
      "transverse facet did not retain exactly one operand contribution"
  done

let signature value =
  Array.init (Weiler.half_facet_count value) (fun half ->
      Weiler.half_facet_shell value half,
      Weiler.neighbor value ~half_facet:half ~local_edge:0,
      Weiler.neighbor value ~half_facet:half ~local_edge:1,
      Weiler.neighbor value ~half_facet:half ~local_edge:2),
  Array.init (Weiler.half_facet_count value / 2) (fun facet ->
      Weiler.facet_left_winding value facet,
      Weiler.facet_right_winding value facet)

let test_domain_exactness () =
  let left = triangle [|0.,0.,0.; 4.,0.,0.; 2.,4.,0.|]
  and right = triangle [|0.,3.,0.; 4.,3.,0.; 2.,-1.,0.|] in
  let run domains = Parallel.run ~domains (fun () ->
      let _, _, value = pipeline left right in signature value) in
  if run 1 <> run 4 then fail "Weiler complex differs between domain counts"

let test_cancellation () =
  let complex, radial, _ = pipeline
      (triangle [|0.,0.,0.; 2.,0.,0.; 0.,2.,0.|])
      (triangle [|10.,0.,0.; 12.,0.,0.; 10.,2.,0.|]) in
  let cancel = Cancel.create () in Cancel.cancel cancel;
  match Weiler.build ~cancel complex radial with
  | Error error when Error.code error = "cancelled" -> ()
  | Error error -> fail "unexpected cancellation error: %s" (Error.to_string error)
  | Ok _ -> fail "cancelled Weiler construction completed"

let run () =
  test_disconnected_closed_shells ();
  test_coincident_winding_vector ();
  test_transverse_connectivity ();
  test_domain_exactness ();
  test_cancellation ()
