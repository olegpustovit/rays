open Rays
open Rdk
open Rdk_test_support

module Attribute_ops = struct
  include Attribute_ops
  let copy_rule ?into ~owner pattern =
    { Attribute_copy.copy_owner = owner; copy_pattern = pattern; copy_into = into }
end

let add storage ~owner ~name geometry =
  let attribute = Attribute.create_owned ~owner ~name storage |> Result.get_ok in
  Geometry.with_attribute attribute geometry |> Result.get_ok

let float_values ~owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Float values -> values
       | _ -> fail ("wrong float storage for " ^ name))
  | None -> fail ("missing " ^ name)

let int_values ~owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Int values -> values
       | _ -> fail ("wrong int storage for " ^ name))
  | None -> fail ("missing " ^ name)

let text_values ~owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Text values -> values
       | _ -> fail ("wrong text storage for " ^ name))
  | None -> fail ("missing " ^ name)

let float2_values ~owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Float2 values -> Packed.Float2.View.view values
       | _ -> fail ("wrong float2 storage for " ^ name))
  | None -> fail ("missing " ^ name)

let float4_values ~owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Float4 values -> Packed.Float4.View.view values
       | _ -> fail ("wrong float4 storage for " ^ name))
  | None -> fail ("missing " ^ name)

let two_triangles () =
  let positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.;1.;0.; 2.;3.;2.|] ~y:[|0.;0.;1.; 0.;0.;1.|]
      ~z:(Array.make 6 0.) in
  let topology = Rdk_test_support.Tb.create ~point_count:6 () in
  Rdk_test_support.Tb.add_triangle topology 0 1 2;
  Rdk_test_support.Tb.add_triangle topology 3 4 5;
  Geometry.create ~positions ~topology:(Rdk_test_support.Tb.freeze topology) ()
  |> Result.get_ok

let quad () =
  let positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.;1.;1.;0.|] ~y:[|0.;0.;1.;1.|] ~z:(Array.make 4 1.) in
  let topology = Rdk_test_support.Tb.create ~point_count:4 () in
  Rdk_test_support.Tb.add_polygon topology [|0;1;2;3|];
  Geometry.create ~positions ~topology:(Rdk_test_support.Tb.freeze topology) ()
  |> Result.get_ok

let run () =
  print_endline "attribute copy tests passed"
