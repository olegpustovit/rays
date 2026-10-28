open Rdk
open Rdk_test_support

module Constraints = Rdk_boolean.Boolean_constraints
module Coplanar = Rdk_boolean.Boolean_coplanar
module Triangulation = Rdk_boolean.Boolean_face_cdt
module Refinement = Rdk_boolean.Boolean_refinement


let geometry pair_count ~right =
  let point_count = pair_count * 3 in
  let x = Array.make point_count 0. and y = Array.make point_count 0.
  and z = Array.make point_count 0. and vertices = Array.init point_count Fun.id in
  for pair = 0 to pair_count - 1 do
    let point = pair * 3 and offset = float_of_int pair *. 4. in
    if right then begin
      x.(point) <- offset +. 0.5; y.(point) <- -0.5; z.(point) <- -1.;
      x.(point + 1) <- offset +. 0.5; y.(point + 1) <- 1.5; z.(point + 1) <- 1.;
      x.(point + 2) <- offset +. 0.5; y.(point + 2) <- 1.5; z.(point + 2) <- -1.
    end else begin
      x.(point) <- offset; y.(point) <- 0.; z.(point) <- 0.;
      x.(point + 1) <- offset +. 2.; y.(point + 1) <- 0.; z.(point + 1) <- 0.;
      x.(point + 2) <- offset; y.(point + 2) <- 2.; z.(point + 2) <- 0.
    end
  done;
  let offsets = Array.init (pair_count + 1) (fun pair -> pair * 3) in
  let topology = Topology.polygons_owned ~point_count ~vertex_points:vertices
      ~primitive_offsets:offsets |> get_string in
  Geometry.create ~positions:(Packed.Float3.View.of_owned_exn ~x ~y ~z)
    ~topology () |> get_string

let constraints pair_count = Constraints.build ~grain:7
    ~left:(geometry pair_count ~right:false)
    ~right:(geometry pair_count ~right:true) () |> get_rdk

let coplanar_geometry pair_count ~right =
  let point_count = pair_count * 3 in
  let x = Array.make point_count 0. and y = Array.make point_count 0.
  and z = Array.make point_count 0. and vertices = Array.init point_count Fun.id in
  for pair = 0 to pair_count - 1 do
    let point = pair * 3 and offset = float_of_int pair *. 8. in
    if right then begin
      x.(point) <- offset; y.(point) <- 3.;
      x.(point + 1) <- offset +. 4.; y.(point + 1) <- 3.;
      x.(point + 2) <- offset +. 2.; y.(point + 2) <- -1.
    end else begin
      x.(point) <- offset; y.(point) <- 0.;
      x.(point + 1) <- offset +. 4.; y.(point + 1) <- 0.;
      x.(point + 2) <- offset +. 2.; y.(point + 2) <- 4.
    end
  done;
  let topology = Topology.polygons_owned ~point_count ~vertex_points:vertices
      ~primitive_offsets:(Array.init (pair_count + 1) (fun pair -> pair * 3))
      |> get_string in
  Geometry.create ~positions:(Packed.Float3.View.of_owned_exn ~x ~y ~z)
    ~topology () |> get_string

let coplanar_input pair_count =
  let constraints = Constraints.build ~grain:7
      ~left:(coplanar_geometry pair_count ~right:false)
      ~right:(coplanar_geometry pair_count ~right:true) () |> get_rdk in
  constraints, Coplanar.build ~grain:7 constraints |> get_rdk

let face_signature = function
  | None -> [||]
  | Some face -> Array.init (Triangulation.triangle_count face) (fun triangle ->
      Triangulation.triangle_point face triangle 0,
      Triangulation.triangle_point face triangle 1,
      Triangulation.triangle_point face triangle 2)

let test_validation () =
  let input = constraints 1 in
  (match Refinement.build ~grain:0 input with
   | Error error when Error.code error = "invalid_parameter" -> ()
   | Error error -> failf "unexpected grain error: %s" (Error.to_string error)
   | Ok _ -> failf "zero refinement grain was accepted");
  let cancel = Cancel.create () in Cancel.cancel cancel;
  match Refinement.build ~cancel ~grain:1 input with
  | Error error when Error.code error = "cancelled" -> ()
  | Error error -> failf "unexpected cancellation error: %s" (Error.to_string error)
  | Ok _ -> failf "cancelled refinement completed"

let run () =
  test_validation ()
