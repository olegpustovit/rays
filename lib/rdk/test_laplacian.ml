open Rays
open Rdk
open Rdk_test_support

let get = function Ok value -> value | Error error -> fail (Error.to_string error)

let with_attribute name storage geometry =
  let attribute = Attribute.create_owned ~owner:Attribute.Point ~name storage |> get_string in
  Geometry.with_attribute attribute geometry |> get_string

let scalar name geometry =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | Some attribute -> (
      match Attribute.View.storage attribute with
      | Attribute.Float values -> values
      | _ -> fail (name ^ " is not a scalar float field"))
  | None -> fail (name ^ " is missing")

let float3 name geometry =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | Some attribute -> (
      match Attribute.View.storage attribute with
      | Attribute.Float3 values -> Packed.Float3.View.view values
      | _ -> fail (name ^ " is not a float3 field"))
  | None -> fail (name ^ " is missing")

let float4_field name geometry =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | Some attribute -> (
      match Attribute.View.storage attribute with
      | Attribute.Float4 values -> Packed.Float4.View.view values
      | _ -> fail (name ^ " is not a float4 field"))
  | None -> fail (name ^ " is missing")

let grid ?(size = 2.) () =
  Plane_generators.grid ~connectivity:Plane_generators.Grid_quads ~columns:2 ~rows:2 ~size () |> get

let degenerate_triangle () =
  let positions =
    Packed.Float3.View.of_owned_exn ~x:[| 0.; 1.; 2. |] ~y:[| 0.; 0.; 0. |] ~z:[| 0.; 0.; 0. |]
  in
  let topology =
    Topology.create_owned ~point_count:3 ~vertex_points:[| 0; 1; 2 |] ~primitive_offsets:[| 0; 3 |]
      ~primitive_kinds:[| Topology.Polygon |]
    |> get_string
  in
  Geometry.create ~positions ~topology () |> get_string

let center_group geometry =
  Group.init ~owner:Group.Point ~name:"center" (Geometry.point_count geometry) (fun point ->
      point = 4)

let expect_invalid work message =
  match work () with
  | Error error when Error.code error = "invalid_laplacian" -> ()
  | Error error -> fail (message ^ ": " ^ Error.to_string error)
  | Ok _ -> fail (message ^ ": unexpectedly accepted")

let run () = print_endline "attribute Laplacian tests passed"
