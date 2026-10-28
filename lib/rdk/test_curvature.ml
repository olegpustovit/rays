open Rays
open Rdk
open Rdk_test_support

let get = function Ok value -> value | Error error -> fail (Error.to_string error)

let geometry ?(attributes = []) ?(groups = []) points triangles =
  let points = Array.of_list points and triangles = Array.of_list triangles in
  let point_count = Array.length points and primitive_count = Array.length triangles in
  let positions = Packed.Float3.View.of_owned_exn
      ~x:(Array.map (fun (x,_,_) -> x) points)
      ~y:(Array.map (fun (_,y,_) -> y) points)
      ~z:(Array.map (fun (_,_,z) -> z) points) in
  let vertex_points = Array.make (primitive_count * 3) 0 in
  Array.iteri (fun primitive (a,b,c) ->
    vertex_points.(primitive * 3) <- a;
    vertex_points.((primitive * 3) + 1) <- b;
    vertex_points.((primitive * 3) + 2) <- c) triangles;
  let topology = Topology.create_owned ~point_count ~vertex_points
      ~primitive_offsets:(Array.init (primitive_count + 1)
        (fun primitive -> primitive * 3))
      ~primitive_kinds:(Array.make primitive_count Topology.Polygon)
      |> get_string in
  Geometry.create ~positions ~topology ~attributes ~groups () |> get_string

let octahedron ?(scale = 1.) ?(center = 0.,0.,0.) () =
  let cx,cy,cz = center in
  let p x y z = cx +. (x *. scale),cy +. (y *. scale),cz +. (z *. scale) in
  geometry [p 0. 0. 1.; p 1. 0. 0.; p 0. 1. 0.; p (-1.) 0. 0.;
    p 0. (-1.) 0.; p 0. 0. (-1.)]
    [(0,1,2);(0,2,3);(0,3,4);(0,4,1);
     (5,2,1);(5,3,2);(5,4,3);(5,1,4)]

let values name geometry =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Float values -> values
       | _ -> fail (name ^ " has non-float storage"))
  | None -> fail (name ^ " is missing")

let all_outputs = {
  Curvature.mean = Some "mean";
  gaussian = Some "gaussian";
  minimum = Some "minimum";
  maximum = Some "maximum";
  curvedness = Some "curvedness";
  shape_index = Some "shape";
}

let expect_invalid work message = match work () with
  | Error error when Error.code error = "invalid_curvature" -> ()
  | Error error -> fail (message ^ ": " ^ Error.to_string error)
  | Ok _ -> fail (message ^ ": unexpectedly accepted")

let run () =
  print_endline "curvature tests passed"
