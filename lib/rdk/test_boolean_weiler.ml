open Rdk
open Rdk_test_support

module Constraints = Rdk_boolean.Boolean_constraints
module Coplanar = Rdk_boolean.Boolean_coplanar
module Refinement = Rdk_boolean.Boolean_refinement
module Complex = Rdk_boolean.Boolean_complex
module Radial = Rdk_boolean.Boolean_radial
module Weiler = Rdk_boolean.Boolean_weiler


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
  let radial = Radial.build complex |> get_rdk in
  complex, radial, Weiler.build complex radial |> get_rdk

let test_cancellation () =
  let complex, radial, _ = pipeline
      (triangle [|0.,0.,0.; 2.,0.,0.; 0.,2.,0.|])
      (triangle [|10.,0.,0.; 12.,0.,0.; 10.,2.,0.|]) in
  let cancel = Cancel.create () in Cancel.cancel cancel;
  match Weiler.build ~cancel complex radial with
  | Error error when Error.code error = "cancelled" -> ()
  | Error error -> failf "unexpected cancellation error: %s" (Error.to_string error)
  | Ok _ -> failf "cancelled Weiler construction completed"

let run () =
  test_cancellation ()
