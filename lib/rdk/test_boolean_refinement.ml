open Rdk

module Constraints = Rdk_boolean.Boolean_constraints
module Coplanar = Rdk_boolean.Boolean_coplanar
module Triangulation = Rdk_boolean.Boolean_face_cdt
module Refinement = Rdk_boolean.Boolean_refinement

let fail format = Printf.ksprintf failwith format
let check condition message = if not condition then fail "%s" message
let get = function Ok value -> value | Error error -> fail "%s" (Error.to_string error)
let get_string = function Ok value -> value | Error message -> fail "%s" message

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
  Geometry.create ~positions:(Packed.Float3.Private.of_owned_exn ~x ~y ~z)
    ~topology () |> get_string

let constraints pair_count = Constraints.build ~grain:7
    ~left:(geometry pair_count ~right:false)
    ~right:(geometry pair_count ~right:true) () |> get

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
  Geometry.create ~positions:(Packed.Float3.Private.of_owned_exn ~x ~y ~z)
    ~topology () |> get_string

let coplanar_input pair_count =
  let constraints = Constraints.build ~grain:7
      ~left:(coplanar_geometry pair_count ~right:false)
      ~right:(coplanar_geometry pair_count ~right:true) () |> get in
  constraints, Coplanar.build ~grain:7 constraints |> get

let face_signature = function
  | None -> [||]
  | Some face -> Array.init (Triangulation.triangle_count face) (fun triangle ->
      Triangulation.triangle_point face triangle 0,
      Triangulation.triangle_point face triangle 1,
      Triangulation.triangle_point face triangle 2)

let signature value =
  Array.init (Refinement.left_face_count value)
    (fun face -> face_signature (Refinement.left_face value face)),
  Array.init (Refinement.right_face_count value)
    (fun face -> face_signature (Refinement.right_face value face))

let test_batch () =
  let pair_count = 127 in
  let value = Refinement.build ~grain:11 (constraints pair_count) |> get in
  check (Refinement.left_face_count value = pair_count
      && Refinement.right_face_count value = pair_count)
    "refinement face cardinality is wrong";
  check (Refinement.refined_left_count value = pair_count
      && Refinement.refined_right_count value = pair_count)
    "not every intersected face was refined";
  for face = 0 to pair_count - 1 do
    check (Option.is_some (Refinement.left_face value face)
        && Option.is_some (Refinement.right_face value face))
      "refinement omitted an affected face"
  done

let test_domain_exactness () =
  let input = constraints 257 in
  let run domains = Parallel.run ~domains (fun () ->
      Refinement.build ~grain:17 input |> get |> signature) in
  if run 1 <> run 4 then fail "batch face refinement differs by domain count"

let test_coplanar_batch () =
  let pair_count = 127 in
  let constraints, coplanar = coplanar_input pair_count in
  let value = Refinement.build ~coplanar ~grain:11 constraints |> get in
  check (Refinement.refined_left_count value = pair_count
      && Refinement.refined_right_count value = pair_count)
    "coplanar batch did not refine both source surfaces";
  for face = 0 to pair_count - 1 do
    match Refinement.left_face value face, Refinement.right_face value face with
    | Some left, Some right ->
        check (Triangulation.triangle_count left = 7
            && Triangulation.triangle_count right = 7)
          "coplanar batch face has the wrong CDT cardinality"
    | _ -> fail "coplanar batch omitted an affected face"
  done;
  let run domains = Parallel.run ~domains (fun () ->
      Refinement.build ~coplanar ~grain:13 constraints |> get |> signature) in
  if run 1 <> run 4 then
    fail "coplanar batch refinement differs by domain count"

let test_validation () =
  let input = constraints 1 in
  (match Refinement.build ~grain:0 input with
   | Error error when Error.code error = "invalid_parameter" -> ()
   | Error error -> fail "unexpected grain error: %s" (Error.to_string error)
   | Ok _ -> fail "zero refinement grain was accepted");
  let cancel = Cancel.create () in Cancel.cancel cancel;
  match Refinement.build ~cancel ~grain:1 input with
  | Error error when Error.code error = "cancelled" -> ()
  | Error error -> fail "unexpected cancellation error: %s" (Error.to_string error)
  | Ok _ -> fail "cancelled refinement completed"

let run () =
  test_batch ();
  test_domain_exactness ();
  test_coplanar_batch ();
  test_validation ()
