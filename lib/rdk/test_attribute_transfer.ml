open Rays
open Rdk
open Rdk_test_support

let float_values ~owner ~name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Float values -> values
       | _ -> fail ("unexpected storage for " ^ name))
  | None -> fail ("missing attribute " ^ name)

let int_values ~owner ~name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Int values -> values
       | _ -> fail ("unexpected storage for " ^ name))
  | None -> fail ("missing attribute " ^ name)

let float3_values ~owner ~name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Float3 values -> Packed.Float3.View.view values
       | _ -> fail ("unexpected storage for " ^ name))
  | None -> fail ("missing attribute " ^ name)

let add_float ~owner ~name values geometry =
  let attribute = Attribute.create_owned ~owner ~name (Attribute.Float values)
      |> Result.get_ok in
  Geometry.with_attribute attribute geometry |> Result.get_ok

let add_int ~owner ~name values geometry =
  let attribute = Attribute.create_owned ~owner ~name (Attribute.Int values)
      |> Result.get_ok in
  Geometry.with_attribute attribute geometry |> Result.get_ok

let add_float3 ~owner ~name ~x ~y ~z geometry =
  let values = Packed.Float3.View.of_owned_exn ~x ~y ~z in
  let attribute = Attribute.create_owned ~owner ~name (Attribute.Float3 values)
      |> Result.get_ok in
  Geometry.with_attribute attribute geometry |> Result.get_ok

let triangle z =
  let positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.; 2.; 0.|] ~y:[|0.; 0.; 2.|] ~z:(Array.make 3 z) in
  let builder = Rdk_test_support.Tb.create ~point_count:3 () in
  Rdk_test_support.Tb.add_triangle builder 0 1 2;
  Geometry.create ~positions ~topology:(Rdk_test_support.Tb.freeze builder) ()
  |> Result.get_ok

let make_source () =
  triangle 0.
  |> add_float ~owner:Attribute.Point ~name:"point_value" [|10.; 20.; 30.|]
  |> add_float ~owner:Attribute.Vertex ~name:"vertex_value" [|4.; 8.; 12.|]
  |> add_float ~owner:Attribute.Primitive ~name:"primitive_value" [|40.|]
  |> add_int ~owner:Attribute.Detail ~name:"detail_value" [|50|]

let make_target () =
  triangle 1.5
  |> add_float ~owner:Attribute.Point ~name:"point_value" (Array.make 3 2.)
  |> add_float ~owner:Attribute.Vertex ~name:"vertex_value" (Array.make 3 2.)
  |> add_float ~owner:Attribute.Primitive ~name:"primitive_value" [|2.|]
  |> add_int ~owner:Attribute.Detail ~name:"detail_value" [|2|]

let two_triangle_source () =
  let positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.; 1.; 0.; 10.; 11.; 10.|]
      ~y:[|0.; 0.; 1.; 0.; 0.; 1.|]
      ~z:(Array.make 6 0.) in
  let builder = Rdk_test_support.Tb.create ~point_count:6 () in
  Rdk_test_support.Tb.add_triangle builder 0 1 2;
  Rdk_test_support.Tb.add_triangle builder 3 4 5;
  Geometry.create ~positions ~topology:(Rdk_test_support.Tb.freeze builder) ()
  |> Result.get_ok
  |> add_float ~owner:Attribute.Vertex ~name:"corner_value"
       [|0.; 10.; 20.; 100.; 110.; 120.|]

let compare_transferred left right =
  List.iter (fun (owner, name) ->
    if float_values ~owner ~name left <> float_values ~owner ~name right then
      fail ("domain/sequential mismatch for " ^ name))
    [Attribute.Point, "point_value";
     Attribute.Vertex, "vertex_value";
     Attribute.Primitive, "primitive_value"];
  if int_values ~owner:Attribute.Detail ~name:"detail_value" left
      <> int_values ~owner:Attribute.Detail ~name:"detail_value" right then
    fail "domain/sequential mismatch for detail_value"

let near left right = Float.abs (left -. right) <= 1e-12

let test_source_kernels () =
  let source = Line_geometry.points [|(0.,0.,0.);(1.,0.,0.);(2.,0.,0.)|]
      |> add_float ~owner:Attribute.Point ~name:"weight" [|0.;10.;20.|]
      |> add_int ~owner:Attribute.Point ~name:"id" [|3;5;7|] in
  let target = Line_geometry.points [|(0.5,0.,0.)|]
      |> add_float ~owner:Attribute.Point ~name:"weight" [|100.|]
      |> add_int ~owner:Attribute.Point ~name:"id" [|99|] in
  let sample kernel = Attribute_ops.transfer_points ~grain:1
      ~mode:(Attribute_ops.Kernel { neighbors=3; radius=2.; kernel })
      ~source ~target () |> get_ok in
  let links = sample Attribute_ops.Links
  and renderman = sample Attribute_ops.RenderMan
  and hart = sample Attribute_ops.Hart in
  if not (near (float_values ~owner:Attribute.Point ~name:"weight" links).(0)
      (64. /. 11.))
      || not (near
        (float_values ~owner:Attribute.Point ~name:"weight" renderman).(0)
        (40610. /. 7093.))
      || not (near (float_values ~owner:Attribute.Point ~name:"weight" hart).(0)
        (5650. /. 971.)) then
    fail "Links/RenderMan/Hart kernel formula or normalization";
  if int_values ~owner:Attribute.Point ~name:"id" links <> [|3|]
      || int_values ~owner:Attribute.Point ~name:"id" renderman <> [|3|]
      || int_values ~owner:Attribute.Point ~name:"id" hart <> [|3|] then
    fail "kernel transfer changed stable closest discrete source";
  let fallback radius = Attribute_ops.transfer_points ~grain:1
      ~mode:(Attribute_ops.Kernel {
        neighbors=3; radius; kernel=Attribute_ops.Hart })
      ~source ~target () |> get_ok in
  if float_values ~owner:Attribute.Point ~name:"weight" (fallback 0.) <> [|0.|]
      || float_values ~owner:Attribute.Point ~name:"weight" (fallback 0.1)
         <> [|0.|] then
    fail "zero-radius or zero-support kernel nearest fallback";
  let large_count = 20_003 in
  let large_target = Line_geometry.points (Array.init large_count (fun point ->
      float_of_int (point mod 2001) /. 1000., 0., 0.)) in
  let run domains = Parallel.run ~domains (fun () ->
    Attribute_ops.transfer_points ~grain:257
      ~mode:(Attribute_ops.Kernel {
        neighbors=3; radius=2.; kernel=Attribute_ops.RenderMan })
      ~source ~target:large_target () |> get_ok) in
  let one = run 1 and four = run 4 in
  if float_values ~owner:Attribute.Point ~name:"weight" one
      <> float_values ~owner:Attribute.Point ~name:"weight" four
      || int_values ~owner:Attribute.Point ~name:"id" one
         <> int_values ~owner:Attribute.Point ~name:"id" four then
    fail "kernel transfer one/four-domain exactness";
  List.iter (fun mode ->
    match Attribute_ops.transfer_points ~mode ~source ~target () with
    | Error error when Error.code error = "invalid_transfer" -> ()
    | _ -> fail "kernel transfer accepted invalid sample controls") [
      Attribute_ops.Kernel {
        neighbors=0; radius=1.; kernel=Attribute_ops.Links };
      Attribute_ops.Kernel {
        neighbors=1; radius=Float.nan; kernel=Attribute_ops.RenderMan };
      Attribute_ops.Kernel {
        neighbors=1; radius=max_float; kernel=Attribute_ops.Hart }]

let test_blend_falloff () =
  let source = Line_geometry.points [|(0., 0., 0.)|]
      |> add_float ~owner:Attribute.Point ~name:"weight" [|10.|]
      |> add_int ~owner:Attribute.Point ~name:"id" [|7|]
      |> add_float3 ~owner:Attribute.Point ~name:"N"
           ~x:[|1.|] ~y:[|0.|] ~z:[|0.|] in
  let target = Line_geometry.points [|(1.5, 0., 0.); (2.5, 0., 0.)|]
      |> add_float ~owner:Attribute.Point ~name:"weight" [|2.; 2.|]
      |> add_int ~owner:Attribute.Point ~name:"id" [|3; 3|]
      |> add_float3 ~owner:Attribute.Point ~name:"N"
           ~x:[|0.; 0.|] ~y:[|2.; 2.|] ~z:[|0.; 0.|] in
  let linear = Attribute_ops.transfer_points ~grain:1 ~max_distance:1.
      ~blend_width:1. ~falloff:Attribute_ops.Linear ~source ~target () |> get_ok in
  if float_values ~owner:Attribute.Point ~name:"weight" linear <> [|6.; 2.|]
      || int_values ~owner:Attribute.Point ~name:"id" linear <> [|7; 3|] then
    fail "linear point-transfer blend band";
  let uniform = Attribute_ops.transfer_points ~grain:1 ~max_distance:1.
      ~blend_width:1. ~falloff:(Attribute_ops.Uniform 0.25)
      ~source ~target () |> get_ok in
  if float_values ~owner:Attribute.Point ~name:"weight" uniform <> [|4.; 2.|]
      || int_values ~owner:Attribute.Point ~name:"id" uniform <> [|3; 3|] then
    fail "uniform point-transfer blend/discrete threshold";
  let zero_influence = Attribute_ops.transfer_points ~grain:1 ~names:["N"]
      ~max_distance:1. ~blend_width:1. ~falloff:(Attribute_ops.Uniform 0.)
      ~source ~target () |> get_ok in
  let normal = float3_values ~owner:Attribute.Point ~name:"N" zero_influence in
  if normal.x <> [|0.; 0.|] || normal.y <> [|2.; 2.|]
      || normal.z <> [|0.; 0.|] then
    fail "zero transfer influence changed or normalized target N";
  (match Attribute_ops.transfer_points ~max_distance:1. ~blend_width:1.
      ~falloff:(Attribute_ops.Uniform Float.nan) ~source ~target () with
   | Error error when Error.code error = "invalid_transfer" -> ()
   | _ -> fail "point transfer accepted a non-finite uniform bias");
  (match Attribute_ops.transfer_points ~blend_width:1. ~source ~target () with
   | Error error when Error.code error = "invalid_transfer" -> ()
   | _ -> fail "point transfer accepted blend width without a threshold")

let test_multi_owner_exactness () =
  let source = make_source () and target = make_target () in
  let transfer domains = Parallel.run ~domains (fun () ->
    Attribute_ops.transfer_all ~grain:1 ~point_pattern:"point_*"
      ~primitive_pattern:"primitive_*" ~vertex_pattern:"vertex_*"
      ~detail_pattern:"detail_*" ~max_distance:1. ~blend_width:1.
      ~falloff:Attribute_ops.Linear ~source ~target () |> get_ok) in
  let one = transfer 1 and four = transfer 4 in
  compare_transferred one four;
  let sequential =
    Attribute_ops.transfer_points ~grain:1 ~pattern:"point_*"
      ~max_distance:1. ~blend_width:1. ~falloff:Attribute_ops.Linear
      ~source ~target () |> get_ok
    |> fun target -> Attribute_ops.transfer_primitives ~grain:1
        ~pattern:"primitive_*" ~max_distance:1. ~blend_width:1.
        ~falloff:Attribute_ops.Linear ~source ~target () |> get_ok
    |> fun target -> Attribute_ops.transfer_vertices ~grain:1
        ~pattern:"vertex_*" ~max_distance:1. ~blend_width:1.
        ~falloff:Attribute_ops.Linear ~source ~target () |> get_ok
    |> fun target -> Attribute_ops.transfer_detail ~pattern:"detail_*"
        ~source ~target () |> get_ok in
  compare_transferred one sequential;
  if float_values ~owner:Attribute.Point ~name:"point_value" one
      <> [|6.; 11.; 16.|]
      || float_values ~owner:Attribute.Primitive ~name:"primitive_value" one
         <> [|21.|]
      || float_values ~owner:Attribute.Vertex ~name:"vertex_value" one
         <> [|3.; 5.; 7.|]
      || int_values ~owner:Attribute.Detail ~name:"detail_value" one <> [|50|]
  then fail "multi-owner transfer values";
  (match Attribute_ops.transfer_all ~point_pattern:"bad[" ~source ~target () with
   | Error error when Error.code error = "invalid_transfer" -> ()
   | _ -> fail "multi-owner transfer accepted a malformed pattern");
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  (match Attribute_ops.transfer_all ~cancel:cancelled ~point_pattern:"*"
      ~source ~target () with
   | Error error when Error.code error = "cancelled" -> ()
   | _ -> fail "cancelled multi-owner transfer published output")

let test_empty_surface_selection () =
  let source = make_source () and target = make_target () in
  let empty = Group.init ~owner:Group.Primitive ~name:"empty" 1
      (fun _ -> false) in
  let transferred = Attribute_ops.transfer_vertices ~grain:1
      ~pattern:"vertex_*" ~unmatched:Attribute_ops.Default_value
      ~source_primitives:empty ~source ~target () |> get_ok in
  if float_values ~owner:Attribute.Vertex ~name:"vertex_value" transferred
      <> [|0.; 0.; 0.|] then
    fail "empty source surface selection did not publish unmatched defaults"

let test_source_vertex_surface_selection () =
  let source = two_triangle_source () in
  let target = Line_geometry.points [|(0.25, 0.25, 0.2); (10.25, 0.25, 0.2)|]
      |> add_float ~owner:Attribute.Point ~name:"sampled" [|9.; 9.|] in
  let spec = { Attribute_ops.source_owner = Attribute.Vertex;
    source_name = "corner_value"; target_name = "sampled" } in
  let first_triangle = Group.init ~grain:1 ~owner:Group.Vertex
      ~name:"first_triangle" 6 (fun vertex -> vertex < 3) in
  let index = Surface_index.create ~grain:1 ~vertices:first_triangle source
      |> get_ok in
  if Surface_index.triangle_count index <> 1 then
    fail "source vertex selection did not retain exactly one triangle";
  let second_triangle = Group.init ~grain:1 ~owner:Group.Vertex
      ~name:"second_triangle" 6 (fun vertex -> vertex >= 3) in
  let second = Surface_index.create ~grain:1 ~vertices:second_triangle source
      |> get_ok in
  if Surface_index.View.triangle_primitive index 0 <> 0
      || Surface_index.triangle_count second <> 1
      || Surface_index.View.triangle_primitive second 0 <> 1 then
    fail "restricted surface index lost the original primitive identity";
  let run domains = Parallel.run ~domains (fun () ->
    Attribute_ops.transfer_surface ~grain:1 ~max_distance:0.5
      ~unmatched:Attribute_ops.Default_value ~source_vertices:first_triangle
      ~attributes:[spec] ~source ~target () |> get_ok) in
  let one = run 1 and four = run 4 in
  let one_values = float_values ~owner:Attribute.Point ~name:"sampled" one in
  if one_values <> [|7.5; 0.|]
      || one_values <> float_values ~owner:Attribute.Point ~name:"sampled" four then
    fail "source vertex all-corners transfer/domain exactness";
  let one_corner = Group.init ~grain:1 ~owner:Group.Vertex ~name:"one_corner" 6
      (fun vertex -> vertex = 3) in
  let any = Attribute_ops.transfer_surface ~grain:1 ~max_distance:0.5
      ~unmatched:Attribute_ops.Default_value ~source_vertices:one_corner
      ~source_vertex_selection:Attribute_ops.Any_triangle_vertex
      ~attributes:[spec] ~source ~target () |> get_ok in
  if float_values ~owner:Attribute.Point ~name:"sampled" any <> [|0.; 107.5|] then
    fail "source vertex any-corner transfer";
  let none = Attribute_ops.transfer_surface ~grain:1 ~max_distance:0.5
      ~unmatched:Attribute_ops.Default_value ~source_vertices:one_corner
      ~source_vertex_selection:Attribute_ops.All_triangle_vertices
      ~attributes:[spec] ~source ~target () |> get_ok in
  if float_values ~owner:Attribute.Point ~name:"sampled" none <> [|0.; 0.|] then
    fail "source vertex all-corners empty surface";
  let second_primitive = Group.init ~grain:1 ~owner:Group.Primitive
      ~name:"second_primitive" 2 (fun primitive -> primitive = 1) in
  let disjoint = Attribute_ops.transfer_surface ~grain:1 ~max_distance:0.5
      ~unmatched:Attribute_ops.Default_value ~source_primitives:second_primitive
      ~source_vertices:first_triangle ~attributes:[spec] ~source ~target ()
      |> get_ok in
  if float_values ~owner:Attribute.Point ~name:"sampled" disjoint <> [|0.; 0.|] then
    fail "primitive and vertex source restrictions were not intersected";
  let wrong_owner = Group.init ~grain:1 ~owner:Group.Point ~name:"wrong" 6
      (fun _ -> true) in
  (match Attribute_ops.transfer_surface ~source_vertices:wrong_owner
      ~attributes:[spec] ~source ~target () with
   | Error error when Error.code error = "invalid_transfer" -> ()
   | _ -> fail "source vertex transfer accepted wrong group ownership");
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  (match Attribute_ops.transfer_surface ~cancel:cancelled
      ~source_vertices:first_triangle ~attributes:[spec] ~source ~target () with
   | Error error when Error.code error = "cancelled" -> ()
   | _ -> fail "cancelled source vertex transfer published output")

let test_group_union_many () =
  let make name predicate = Group.init ~grain:1 ~owner:Group.Point ~name 100_003
      predicate in
  let a = make "a" (fun index -> index mod 2 = 0)
  and b = make "b" (fun index -> index mod 3 = 0)
  and c = make "c" (fun index -> index mod 5 = 0) in
  let run domains = Parallel.run ~domains (fun () ->
    Group.union_many ~grain:257 ~name:"combined" [a; b; c] |> Result.get_ok) in
  let one = run 1 and four = run 4 in
  if Group.cardinality one <> 73_336
      || Group.View.bits_view one <> Group.View.bits_view four
      || Group.is_ordered one then
    fail "packed union-many cardinality/domain exactness";
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  (match (try Group.union_many ~cancel:cancelled ~grain:1 ~name:"cancelled"
      [a; b] with Cancel.Cancelled -> Error "cancelled") with
   | Error "cancelled" -> ()
   | _ -> fail "group union-many ignored cancellation")

let test_scale_exactness () =
  let count = 20_001 in
  let points = Array.init count (fun index ->
    let x = float_of_int index *. 0.001 in x, sin x, cos x) in
  let source = Line_geometry.points points
      |> add_float ~owner:Attribute.Point ~name:"sample"
           (Array.init count float_of_int) in
  let target = Line_geometry.points points in
  let run domains = Parallel.run ~domains (fun () ->
    Attribute_ops.transfer_all ~grain:257 ~point_pattern:"sample"
      ~max_distance:0. ~source ~target () |> get_ok) in
  let one = run 1 and four = run 4 in
  let expected = Array.init count float_of_int in
  if float_values ~owner:Attribute.Point ~name:"sample" one <> expected
      || float_values ~owner:Attribute.Point ~name:"sample" one
         <> float_values ~owner:Attribute.Point ~name:"sample" four then
    fail "20k point multi-owner transfer scale/domain exactness"

let run () =
  test_source_kernels ();
  test_blend_falloff ();
  test_multi_owner_exactness ();
  test_empty_surface_selection ();
  test_source_vertex_surface_selection ();
  test_group_union_many ();
  test_scale_exactness ();
  print_endline "attribute transfer tests passed"
