open Rdk_test_support
open Rays
open Rdk

let fail message = failwith message
let get_string_ok = function Ok value -> value | Error message -> fail message
let near ?(epsilon = 1e-11) left right = abs_float (left -. right) <= epsilon

let add attribute geometry = Geometry.with_attribute attribute geometry |> get_string_ok
let add_group group geometry = Geometry.with_group group geometry |> get_string_ok

let float_attribute ~owner name values =
  Attribute.create_owned ~name ~owner (Attribute.Float values) |> get_string_ok

let text_attribute ~owner name values =
  Attribute.create_owned ~name ~owner (Attribute.Text values) |> get_string_ok

let float2_attribute ~owner name x y =
  let values = Packed.Float2.of_owned ~x ~y |> get_string_ok in
  Attribute.create_owned ~name ~owner (Attribute.Float2 values) |> get_string_ok

let float4_attribute ~owner name x y z w =
  let values = Packed.Float4.of_owned ~x ~y ~z ~w |> get_string_ok in
  Attribute.create_owned ~name ~owner (Attribute.Float4 values) |> get_string_ok

let attribute geometry owner name =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute -> attribute
  | None -> fail ("missing attribute " ^ name)

let point_float geometry name = match Attribute.Private.storage
    (attribute geometry Attribute.Point name) with
  | Attribute.Float values -> values
  | _ -> fail (name ^ " is not point float")

let point_int geometry name = match Attribute.Private.storage
    (attribute geometry Attribute.Point name) with
  | Attribute.Int values -> values
  | _ -> fail (name ^ " is not point int")

let point_text geometry name = match Attribute.Private.storage
    (attribute geometry Attribute.Point name) with
  | Attribute.Text values -> values
  | _ -> fail (name ^ " is not point text")

let point_float2 geometry name = match Attribute.Private.storage
    (attribute geometry Attribute.Point name) with
  | Attribute.Float2 values -> Packed.Float2.Private.view values
  | _ -> fail (name ^ " is not point float2")

let point_float3 geometry name = match Attribute.Private.storage
    (attribute geometry Attribute.Point name) with
  | Attribute.Float3 values -> Packed.Float3.Private.view values
  | _ -> fail (name ^ " is not point float3")

let point_float4 geometry name = match Attribute.Private.storage
    (attribute geometry Attribute.Point name) with
  | Attribute.Float4 values -> Packed.Float4.Private.view values
  | _ -> fail (name ^ " is not point float4")

let point_int_array geometry name = match Attribute.Private.storage
    (attribute geometry Attribute.Point name) with
  | Attribute.Int_array values -> Packed.Int_array.Private.view values
  | _ -> fail (name ^ " is not point int array")

let point_float_array geometry name = match Attribute.Private.storage
    (attribute geometry Attribute.Point name) with
  | Attribute.Float_array values -> Packed.Float_array.Private.view values
  | _ -> fail (name ^ " is not point float array")

let expect_code code = function
  | Error error when String.equal (Error.code error) code -> ()
  | Error error -> fail (Printf.sprintf "expected %s, received %s" code
      (Error.to_string error))
  | Ok _ -> fail ("expected " ^ code)

let two_triangles () =
  let positions = Packed.Float3.Private.of_owned_exn
      ~x:[|0.; 1.; 0.; 10.; 11.; 10.|]
      ~y:[|0.; 0.; 0.; 0.; 0.; 0.|]
      ~z:[|0.; 0.; 1.; 0.; 0.; 1.|] in
  let topology = Rdk_test_support.Tb.create ~point_count:6 () in
  Rdk_test_support.Tb.add_triangle topology 0 1 2;
  Rdk_test_support.Tb.add_triangle topology 3 4 5;
  Geometry.create ~positions ~topology:(Rdk_test_support.Tb.freeze topology) ()
  |> get_string_ok

let source_with_payload () =
  let source = two_triangles () in
  let point_x = Packed.Float3.Private.view (Geometry.positions source) in
  source
  |> add (float_attribute ~owner:Attribute.Primitive "density" [|0.; 2.|])
  |> add (float_attribute ~owner:Attribute.Point "foo" (Array.copy point_x.x))
  |> add (float2_attribute ~owner:Attribute.Vertex "corner"
       [|0.;1.;2.;3.;4.;5.|] [|10.;11.;12.;13.;14.;15.|])
  |> add (text_attribute ~owner:Attribute.Primitive "material" [|"left";"right"|])
  |> add (float4_attribute ~owner:Attribute.Detail "meta"
       [|7.|] [|8.|] [|9.|] [|10.|])
  |> add_group (Group.init ~owner:Group.Point ~name:"point_mark" 6
       (fun point -> point >= 3))
  |> add_group (Group.init ~owner:Group.Vertex ~name:"vertex_mark" 6
       (fun vertex -> vertex >= 3))
  |> add_group (Group.init ~owner:Group.Primitive ~name:"primitive_mark" 2
       (fun primitive -> primitive = 1))

let run () =
  print_endline "scatter tests passed"
