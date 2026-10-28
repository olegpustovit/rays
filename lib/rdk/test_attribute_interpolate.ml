open Rays
open Rdk
open Rdk_test_support

let add storage ~owner ~name geometry =
  let attribute = Attribute.create_owned ~owner ~name storage |> Result.get_ok in
  Geometry.with_attribute attribute geometry |> Result.get_ok

let add_group ~owner ~name elements geometry =
  let length = match owner with
    | Group.Point -> Geometry.point_count geometry
    | Group.Vertex -> Geometry.vertex_count geometry
    | Group.Primitive -> Geometry.primitive_count geometry in
  let group = Group.ordered ~owner ~name ~length elements |> Result.get_ok in
  Geometry.with_group group geometry |> Result.get_ok

let group_values ~owner name geometry =
  match Geometry.find_group ~owner name geometry with
  | None -> fail ("missing group " ^ name)
  | Some group -> Array.init (Group.length group) (fun element ->
      Group.mem element group)

let packed3 values =
  let count = Array.length values in
  Packed.Float3.View.of_owned_exn
    ~x:(Array.init count (fun index -> let x, _, _ = values.(index) in x))
    ~y:(Array.init count (fun index -> let _, y, _ = values.(index) in y))
    ~z:(Array.init count (fun index -> let _, _, z = values.(index) in z))

let make_geometry positions add_primitives =
  let topology = Rdk_test_support.Tb.create ~point_count:(Array.length positions) () in
  add_primitives topology;
  Geometry.create ~positions:(packed3 positions)
    ~topology:(Rdk_test_support.Tb.freeze topology) () |> Result.get_ok

let points count =
  Geometry.create
    ~positions:(Packed.Float3.View.of_owned_exn
      ~x:(Array.make count 0.) ~y:(Array.make count 0.) ~z:(Array.make count 0.))
    ~topology:(Topology.empty ~point_count:count) () |> Result.get_ok

let add_drivers ~owner primitives uvw geometry =
  geometry
  |> add (Attribute.Int primitives) ~owner ~name:"source_primitive"
  |> add (Attribute.Float3 (packed3 uvw)) ~owner ~name:"source_uvw"

let int_rows rows =
  let offsets = Array.make (Array.length rows + 1) 0 in
  Array.iteri (fun row values ->
    offsets.(row + 1) <- offsets.(row) + Array.length values) rows;
  Packed.Int_array.create_owned ~offsets
    ~values:(Array.concat (Array.to_list rows)) |> Result.get_ok

let float_rows rows =
  let offsets = Array.make (Array.length rows + 1) 0 in
  Array.iteri (fun row values ->
    offsets.(row + 1) <- offsets.(row) + Array.length values) rows;
  Packed.Float_array.create_owned ~offsets
    ~values:(Array.concat (Array.to_list rows)) |> Result.get_ok

let add_weight_drivers ~owner numbers weights geometry =
  geometry
  |> add (Attribute.Int_array (int_rows numbers)) ~owner ~name:"source_elements"
  |> add (Attribute.Float_array (float_rows weights)) ~owner ~name:"source_weights"

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

let float3_values ~owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Float3 values -> Packed.Float3.View.view values
       | _ -> fail ("wrong float3 storage for " ^ name))
  | None -> fail ("missing " ^ name)

let float4_values ~owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Float4 values -> Packed.Float4.View.view values
       | _ -> fail ("wrong float4 storage for " ^ name))
  | None -> fail ("missing " ^ name)

let int_array_values ~owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Int_array values -> values
       | _ -> fail ("wrong int-array storage for " ^ name))
  | None -> fail ("missing " ^ name)

let float_array_values ~owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Float_array values -> values
       | _ -> fail ("wrong float-array storage for " ^ name))
  | None -> fail ("missing " ^ name)

let spec ?into owner name =
  { Attribute_ops.interpolate_owner = owner; interpolate_source = name;
    interpolate_target = Option.value ~default:name into }

let interpolate ?grain ?selection ?pre_scale ?blend ?unmatched ~target_owner
    ~attributes ~source target =
  Attribute_ops_front.interpolate ?grain ?selection ?pre_scale ?blend ?unmatched
    ~target_owner ~attributes ~source ~target () |> get_ok

let near left right = Float.abs (left -. right) <= 1e-12

let triangle_source () =
  make_geometry [|(0.,0.,0.); (2.,0.,0.); (0.,2.,0.)|]
    (fun topology -> Rdk_test_support.Tb.add_triangle topology 0 1 2)
  |> add (Attribute.Float [|0.;10.;20.|]) ~owner:Attribute.Point ~name:"weight"
  |> add (Attribute.Int [|10;20;30|]) ~owner:Attribute.Point ~name:"id"
  |> add (Attribute.Text [|"a";"b";"c"|]) ~owner:Attribute.Point ~name:"label"
  |> add (Attribute.Float2 (Packed.Float2.of_owned
      ~x:[|0.;2.;0.|] ~y:[|0.;0.;2.|] |> Result.get_ok))
       ~owner:Attribute.Point ~name:"uv"
  |> add (Attribute.Float3 (packed3
      [|(2.,0.,0.); (0.,2.,0.); (0.,0.,2.)|]))
       ~owner:Attribute.Point ~name:"N"
  |> add (Attribute.Float4 (Packed.Float4.of_owned
      ~x:[|0.;1.;2.|] ~y:[|3.;4.;5.|] ~z:[|6.;7.;8.|]
      ~w:[|9.;10.;11.|] |> Result.get_ok))
       ~owner:Attribute.Point ~name:"tuple4"
  |> add (Attribute.Float [|100.;200.;300.|])
       ~owner:Attribute.Vertex ~name:"vertex_weight"
  |> add (Attribute.Float [|42.|])
       ~owner:Attribute.Primitive ~name:"primitive_weight"
  |> add (Attribute.Float [|7.|]) ~owner:Attribute.Detail ~name:"detail_weight"

let test_triangle_mixed_storage_and_position () =
  let source = triangle_source () in
  let target = points 4 |> add_drivers ~owner:Attribute.Point
      [|0;0;0;-1|]
      [|(0.,0.,0.); (0.25,0.25,0.); (1.,0.,0.); (0.,0.,0.)|] in
  let result = interpolate ~target_owner:Attribute.Point ~source target
      ~attributes:[
        spec Attribute.Point "P";
        spec Attribute.Point "weight";
        spec Attribute.Point "id";
        spec Attribute.Point "label";
        spec Attribute.Point "uv";
        spec Attribute.Point "N";
        spec Attribute.Point "tuple4";
        spec Attribute.Vertex "vertex_weight";
        spec Attribute.Primitive "primitive_weight";
        spec Attribute.Detail "detail_weight";
      ] in
  let position = Packed.Float3.View.view (Geometry.positions result)
  and uv = float2_values ~owner:Attribute.Point "uv" result
  and normal = float3_values ~owner:Attribute.Point "N" result
  and tuple4 = float4_values ~owner:Attribute.Point "tuple4" result in
  if position.x <> [|0.;0.5;2.;0.|] || position.y <> [|0.;0.5;0.;0.|]
      || float_values ~owner:Attribute.Point "weight" result
         <> [|0.;7.5;10.;0.|]
      || int_values ~owner:Attribute.Point "id" result <> [|10;10;20;0|]
      || text_values ~owner:Attribute.Point "label" result
         <> [|"a";"a";"b";""|]
      || uv.x <> [|0.;0.5;2.;0.|] || uv.y <> [|0.;0.5;0.;0.|]
      || tuple4.x <> [|0.;0.75;1.;0.|]
      || float_values ~owner:Attribute.Point "vertex_weight" result
         <> [|100.;175.;200.;0.|]
      || float_values ~owner:Attribute.Point "primitive_weight" result
         <> [|42.;42.;42.;0.|]
      || float_values ~owner:Attribute.Point "detail_weight" result
         <> [|7.;7.;7.;0.|] then
    fail "triangle mixed-storage interpolation";
  if not (near normal.x.(1) (2. /. sqrt 6.)
      && near normal.y.(1) (1. /. sqrt 6.)
      && near normal.z.(1) (1. /. sqrt 6.)) then
    fail "interpolated N was not normalized"

let test_quad_ngon_and_curves () =
  let quad = make_geometry
      [|(0.,0.,0.); (0.,1.,0.); (1.,1.,0.); (1.,0.,0.)|]
      (fun topology -> Rdk_test_support.Tb.add_polygon topology [|0;1;2;3|])
      |> add (Attribute.Float [|0.;10.;20.;30.|])
           ~owner:Attribute.Point ~name:"value" in
  let target = points 5 |> add_drivers ~owner:Attribute.Point
      (Array.make 5 0)
      [|(0.,0.,0.); (0.,1.,0.); (1.,1.,0.); (1.,0.,0.); (0.5,0.5,0.)|] in
  let result = interpolate ~target_owner:Attribute.Point ~source:quad target
      ~attributes:[spec Attribute.Point "value"] in
  if float_values ~owner:Attribute.Point "value" result
      <> [|0.;10.;20.;30.;15.|] then fail "bilinear quad parameter space";
  let ngon = make_geometry
      [|(0.,0.,0.); (1.,0.,0.); (1.5,1.,0.); (0.5,2.,0.); (-0.5,1.,0.)|]
      (fun topology -> Rdk_test_support.Tb.add_polygon topology [|0;1;2;3;4|])
      |> add (Attribute.Float [|0.;10.;20.;30.;40.|])
           ~owner:Attribute.Point ~name:"value" in
  let target = points 3 |> add_drivers ~owner:Attribute.Point [|0;0;0|]
      [|(0.1,0.,0.); (0.7,1.,0.); (0.3,0.5,0.)|] in
  let result = interpolate ~target_owner:Attribute.Point ~source:ngon target
      ~attributes:[spec Attribute.Point "value"] in
  if float_values ~owner:Attribute.Point "value" result
      <> [|5.;20.;17.5|] then fail "polygon-fan parameter space";
  let curves = make_geometry [|(0.,0.,0.); (1.,0.,0.); (2.,0.,0.)|]
      (fun topology ->
        Rdk_test_support.Tb.add_open_polyline topology [|0;1;2|];
        Rdk_test_support.Tb.add_closed_polyline topology [|0;1;2|])
      |> add (Attribute.Float [|0.;10.;20.|])
           ~owner:Attribute.Point ~name:"value" in
  let target = points 7 |> add_drivers ~owner:Attribute.Point
      [|0;0;0;0;1;1;1|]
      [|(0.,0.,0.); (0.25,0.,0.); (0.5,0.,0.); (1.,0.,0.);
        (1. /. 6.,0.,0.); (0.5,0.,0.); (5. /. 6.,0.,0.)|] in
  let result = interpolate ~target_owner:Attribute.Point ~source:curves target
      ~attributes:[spec Attribute.Point "value"] in
  if float_values ~owner:Attribute.Point "value" result
      <> [|0.;5.;10.;20.;5.;15.;10.|] then
    fail "open/closed curve parameter space"

let target_topology () =
  make_geometry [|(0.,0.,0.); (1.,0.,0.); (1.,1.,0.); (0.,1.,0.)|]
    (fun topology ->
      Rdk_test_support.Tb.add_triangle topology 0 1 2;
      Rdk_test_support.Tb.add_triangle topology 0 2 3)

let test_destination_owners_group_blend_and_misses () =
  let source = triangle_source () in
  let vertex_target = target_topology () |> add_drivers ~owner:Attribute.Vertex
      [|0;0;0;0;0;0|] (Array.make 6 (0.25,0.25,0.)) in
  let vertex = interpolate ~target_owner:Attribute.Vertex ~source vertex_target
      ~attributes:[spec Attribute.Point "weight"] in
  if float_values ~owner:Attribute.Vertex "weight" vertex
      <> Array.make 6 7.5 then fail "vertex destination owner";
  let primitive_target = target_topology ()
      |> add_drivers ~owner:Attribute.Primitive [|0;0|]
           [|(0.,0.,0.); (1.,0.,0.)|] in
  let primitive = interpolate ~target_owner:Attribute.Primitive ~source
      primitive_target ~attributes:[spec Attribute.Point "weight"] in
  if float_values ~owner:Attribute.Primitive "weight" primitive
      <> [|0.;10.|] then fail "primitive destination owner";
  let detail_target = points 1 |> add_drivers ~owner:Attribute.Detail [|0|]
      [|(0.25,0.25,0.)|] in
  let detail = interpolate ~target_owner:Attribute.Detail ~source detail_target
      ~attributes:[spec Attribute.Point "weight"] in
  if float_values ~owner:Attribute.Detail "weight" detail <> [|7.5|] then
    fail "detail destination owner";
  let target = points 4
      |> add_drivers ~owner:Attribute.Point [|0;0;-1;0|]
           [|(0.,0.,0.); (1.,0.,0.); (0.,0.,0.); (0.,1.,0.)|]
      |> add (Attribute.Float [|100.;100.;100.;100.|])
           ~owner:Attribute.Point ~name:"weight" in
  let selection = Group.ordered ~owner:Group.Point ~name:"selected" ~length:4
      [|1;2;3|] |> Result.get_ok in
  let kept = interpolate ~selection ~blend:0.5 ~target_owner:Attribute.Point
      ~source target ~attributes:[spec Attribute.Point "weight"] in
  if float_values ~owner:Attribute.Point "weight" kept
      <> [|100.;55.;100.;60.|] then fail "grouped blend/keep miss";
  let defaulted = interpolate ~selection ~unmatched:Attribute_transfer.Default_value
      ~target_owner:Attribute.Point ~source target
      ~attributes:[spec Attribute.Point "weight"] in
  if float_values ~owner:Attribute.Point "weight" defaulted
      <> [|100.;10.;0.;20.|] then fail "grouped default miss";
  let prescaled = interpolate ~pre_scale:0.5 ~target_owner:Attribute.Point
      ~source (points 1 |> add_drivers ~owner:Attribute.Point [|0|]
        [|(1.,0.,0.)|]) ~attributes:[spec Attribute.Point "weight"] in
  if float_values ~owner:Attribute.Point "weight" prescaled <> [|5.|] then
    fail "UVW pre-scale"

let test_explicit_weight_modes () =
  let source = triangle_source () in
  let point_target = points 3
      |> add_weight_drivers ~owner:Attribute.Point
           [|[|0;1|]; [|1;2|]; [||]|]
           [|[|0.25;0.75|]; [|0.5;0.5|]; [||]|] in
  let point = Attribute_ops_front.interpolate
      ~driver:(Attribute_ops.Point_weights {
        numbers_attribute="source_elements"; weights_attribute="source_weights" })
      ~target_owner:Attribute.Point ~attributes:[
        spec Attribute.Point "weight"; spec Attribute.Point "id";
        spec Attribute.Detail "detail_weight"] ~source ~target:point_target ()
      |> get_ok in
  if float_values ~owner:Attribute.Point "weight" point <> [|7.5;15.;0.|]
      || int_values ~owner:Attribute.Point "id" point <> [|20;20;0|]
      || float_values ~owner:Attribute.Point "detail_weight" point
         <> [|7.;7.;0.|] then
    fail "point-number weighted interpolation";
  let normalized_target = points 1
      |> add_weight_drivers ~owner:Attribute.Point [|[|0;1|]|] [|[|0.1;0.1|]|]
      |> add (Attribute.Float [|100.|]) ~owner:Attribute.Point ~name:"weight"
      |> add (Attribute.Float [|100.|]) ~owner:Attribute.Point
           ~name:"detail_weight" in
  let normalized = Attribute_ops_front.interpolate
      ~driver:(Attribute_ops.Point_weights {
        numbers_attribute="source_elements"; weights_attribute="source_weights" })
      ~pre_scale:2. ~normalize_weights:true ~threshold:0.8
      ~target_owner:Attribute.Point ~attributes:[spec Attribute.Point "weight";
        spec Attribute.Detail "detail_weight"] ~source ~target:normalized_target ()
      |> get_ok in
  if not (near (float_values ~owner:Attribute.Point "weight" normalized).(0) 52.5)
      || not (near
        (float_values ~owner:Attribute.Point "detail_weight" normalized).(0) 53.5)
  then fail "normalized weighted threshold/pre-scale/blend influence";
  let vertex_target = points 1
      |> add_weight_drivers ~owner:Attribute.Point [|[|0;2|]|]
           [|[|0.25;0.75|]|] in
  let vertex = Attribute_ops_front.interpolate
      ~driver:(Attribute_ops.Vertex_weights {
        numbers_attribute="source_elements"; weights_attribute="source_weights" })
      ~target_owner:Attribute.Point ~attributes:[spec Attribute.Point "weight";
        spec Attribute.Vertex "vertex_weight";
        spec Attribute.Primitive "primitive_weight";
        spec Attribute.Detail "detail_weight"] ~source ~target:vertex_target ()
      |> get_ok in
  if float_values ~owner:Attribute.Point "weight" vertex <> [|15.|]
      || float_values ~owner:Attribute.Point "vertex_weight" vertex <> [|250.|]
      || float_values ~owner:Attribute.Point "primitive_weight" vertex <> [|42.|]
      || float_values ~owner:Attribute.Point "detail_weight" vertex <> [|7.|]
  then fail "vertex-number mixed-owner weighted interpolation";
  let primitive_source = make_geometry
      [|(0.,0.,0.);(1.,0.,0.);(0.,1.,0.);(2.,0.,0.);(3.,0.,0.);(2.,1.,0.)|]
      (fun topology ->
        Rdk_test_support.Tb.add_triangle topology 0 1 2;
        Rdk_test_support.Tb.add_triangle topology 3 4 5)
      |> add (Attribute.Float [|10.;30.|]) ~owner:Attribute.Primitive
           ~name:"piece"
      |> add (Attribute.Float [|2.|]) ~owner:Attribute.Detail ~name:"detail" in
  let primitive_target = points 1
      |> add_weight_drivers ~owner:Attribute.Point [|[|0;1|]|]
           [|[|0.25;0.75|]|] in
  let primitive = Attribute_ops_front.interpolate
      ~driver:(Attribute_ops.Primitive_weights {
        numbers_attribute="source_elements"; weights_attribute="source_weights" })
      ~target_owner:Attribute.Point ~attributes:[
        spec Attribute.Primitive "piece"; spec Attribute.Detail "detail"]
      ~source:primitive_source ~target:primitive_target () |> get_ok in
  if float_values ~owner:Attribute.Point "piece" primitive <> [|25.|]
      || float_values ~owner:Attribute.Point "detail" primitive <> [|2.|]
  then fail "primitive-number weighted interpolation"

let test_signed_normalization_and_group_threshold () =
  let source = triangle_source ()
      |> add_group ~owner:Group.Point ~name:"hot" [|1|] in
  let target = points 3
      |> add_weight_drivers ~owner:Attribute.Point
           [|[|0;1|]; [|0;1|]; [|0;1|]|]
           [|[|-1.;-1.|]; [|2.;-1.|]; [|1.;-1.|]|]
      |> add (Attribute.Float [|100.;100.;100.|])
           ~owner:Attribute.Point ~name:"weight"
      |> add (Attribute.Int [|99;99;99|])
           ~owner:Attribute.Point ~name:"id"
      |> add_group ~owner:Group.Point ~name:"hot" [|2|] in
  let result = Attribute_ops_front.interpolate
      ~driver:(Attribute_ops.Point_weights {
        numbers_attribute="source_elements";
        weights_attribute="source_weights" })
      ~normalize_weights:true ~threshold:0.5 ~match_groups:true
      ~point_pattern:"hot" ~target_owner:Attribute.Point
      ~attributes:[spec Attribute.Point "weight"; spec Attribute.Point "id"]
      ~source ~target () |> get_ok in
  if float_values ~owner:Attribute.Point "weight" result
      <> [|5.;-10.;100.|] then
    fail "signed or zero-sum normalized numeric interpolation";
  if int_values ~owner:Attribute.Point "id" result <> [|10;10;99|] then
    fail "signed or zero-sum normalized discrete interpolation";
  if group_values ~owner:Group.Point "hot" result
      <> [|true;false;true|] then
    fail "inclusive group threshold or zero-sum group preservation"

let test_array_field_row_selection () =
  let source = triangle_source ()
      |> add (Attribute.Int_array (int_rows [|[|1;2|];[|3|];[||]|]))
           ~owner:Attribute.Point ~name:"neighbors"
      |> add (Attribute.Float_array
          (float_rows [|[|0.1|];[|0.2;0.3|];[|0.4|]|]))
           ~owner:Attribute.Point ~name:"samples" in
  let attributes = [spec Attribute.Point "neighbors";
    spec Attribute.Point "samples"] in
  let primitive_target = points 3
      |> add_drivers ~owner:Attribute.Point [|0;0;0|]
           [|(0.,0.,0.);(1.,0.,0.);(0.,1.,0.)|] in
  let primitive = Attribute_ops_front.interpolate ~target_owner:Attribute.Point
      ~attributes ~source ~target:primitive_target () |> get_ok in
  if Array.init 3 (Packed.Int_array.get
      (int_array_values ~owner:Attribute.Point "neighbors" primitive))
      <> [|[|1;2|];[|3|];[||]|]
      || Array.init 3 (Packed.Float_array.get
        (float_array_values ~owner:Attribute.Point "samples" primitive))
         <> [|[|0.1|];[|0.2;0.3|];[|0.4|]|] then
    fail "primitive UVW array-row selection";
  let weighted_target = points 3
      |> add_weight_drivers ~owner:Attribute.Point
           [|[|0;1|];[|0;1|];[|0;2|]|]
           [|[|0.25;0.75|];[|0.5;0.5|];[|1.;-1.|]|]
      |> add (Attribute.Int_array (int_rows [|[|9|];[|8|];[|7|]|]))
           ~owner:Attribute.Point ~name:"neighbors"
      |> add (Attribute.Float_array
          (float_rows [|[|9.|];[|8.|];[|7.|]|]))
           ~owner:Attribute.Point ~name:"samples" in
  let weighted = Attribute_ops_front.interpolate
      ~driver:(Attribute_ops.Point_weights {
        numbers_attribute="source_elements";
        weights_attribute="source_weights" })
      ~normalize_weights:true ~threshold:0.1 ~target_owner:Attribute.Point
      ~attributes ~source ~target:weighted_target () |> get_ok in
  if Array.init 3 (Packed.Int_array.get
      (int_array_values ~owner:Attribute.Point "neighbors" weighted))
      <> [|[|3|];[|1;2|];[|7|]|]
      || Array.init 3 (Packed.Float_array.get
        (float_array_values ~owner:Attribute.Point "samples" weighted))
         <> [|[|0.2;0.3|];[|0.1|];[|7.|]|] then
    fail "weighted array-row selection, stable tie, or zero-sum preservation";
  let blend_target = points 2
      |> add_weight_drivers ~owner:Attribute.Point [|[|1|];[|1|]|]
           [|[|1.|];[|1.|]|]
      |> add (Attribute.Int_array (int_rows [|[|9|];[|8|]|]))
           ~owner:Attribute.Point ~name:"neighbors" in
  let run_blend blend = Attribute_ops_front.interpolate
      ~driver:(Attribute_ops.Point_weights {
        numbers_attribute="source_elements";
        weights_attribute="source_weights" })
      ~blend ~target_owner:Attribute.Point
      ~attributes:[spec Attribute.Point "neighbors"]
      ~source ~target:blend_target () |> get_ok in
  if Packed.Int_array.get
      (int_array_values ~owner:Attribute.Point "neighbors" (run_blend 0.49)) 0
      <> [|9|]
      || Packed.Int_array.get
        (int_array_values ~owner:Attribute.Point "neighbors" (run_blend 0.5)) 0
         <> [|3|] then
    fail "array-row destination blend threshold";
  let missed = points 1
      |> add_drivers ~owner:Attribute.Point [|-1|] [|(0.,0.,0.)|]
      |> add (Attribute.Int_array (int_rows [|[|9|]|]))
           ~owner:Attribute.Point ~name:"neighbors" in
  let defaulted = Attribute_ops_front.interpolate
      ~unmatched:Attribute_transfer.Default_value ~target_owner:Attribute.Point
      ~attributes:[spec Attribute.Point "neighbors"]
      ~source ~target:missed () |> get_ok in
  if Packed.Int_array.get
      (int_array_values ~owner:Attribute.Point "neighbors" defaulted) 0
      <> [||] then fail "array-row default miss policy"

let test_computed_weights_equivalence () =
  let source = triangle_source () in
  let target = points 3 |> add_drivers ~owner:Attribute.Point [|0;0;-1|]
      [|(0.,0.,0.);(0.25,0.25,0.);(0.,0.,0.)|] in
  let compute_weights : Attribute_ops.interpolate_computed = {
    computed_owner = Attribute.Point;
    computed_numbers_attribute = "computed_points";
    computed_weights_attribute = "computed_weights" } in
  let computed_only = Attribute_ops_front.interpolate ~compute_weights
      ~target_owner:Attribute.Point ~attributes:[] ~source ~target () |> get_ok in
  if Array.init 3 (Packed.Int_array.get
      (int_array_values ~owner:Attribute.Point "computed_points" computed_only))
      <> [|[|0;1;2|];[|0;1;2|];[||]|] then
    fail "compute-only Attribute Interpolate rows";
  let primitive = Attribute_ops_front.interpolate ~compute_weights
      ~target_owner:Attribute.Point ~attributes:[spec Attribute.Point "weight";
        spec Attribute.Point "id"] ~source ~target () |> get_ok in
  let numbers = int_array_values ~owner:Attribute.Point "computed_points"
      primitive in
  if Array.init 3 (Packed.Int_array.get numbers)
      <> [|[|0;1;2|];[|0;1;2|];[||]|] then
    fail "computed point-number rows";
  let weighted = Attribute_ops_front.interpolate
      ~driver:(Attribute_ops.Point_weights {
        numbers_attribute="computed_points";
        weights_attribute="computed_weights" })
      ~target_owner:Attribute.Point ~attributes:[spec Attribute.Point "weight";
        spec Attribute.Point "id"] ~source ~target:primitive () |> get_ok in
  if float_values ~owner:Attribute.Point "weight" weighted
      <> float_values ~owner:Attribute.Point "weight" primitive
      || int_values ~owner:Attribute.Point "id" weighted
         <> int_values ~owner:Attribute.Point "id" primitive then
    fail "computed point weights are not equivalent to primitive UVW"

let test_attribute_pattern_expansion () =
  let expect_invalid label = function
    | Error error when String.equal (Error.code error) "invalid_interpolate" -> ()
    | _ -> fail label in
  let source = triangle_source () in
  let target = points 1 |> add_drivers ~owner:Attribute.Point [|0|]
      [|(0.25,0.25,0.)|] in
  let result = Attribute_ops_front.interpolate ~point_pattern:"P weight ^id"
      ~vertex_pattern:"vertex_*" ~primitive_pattern:"primitive_*"
      ~detail_pattern:"detail_*" ~target_owner:Attribute.Point
      ~attributes:[] ~source ~target () |> get_ok in
  let positions = Packed.Float3.View.view (Geometry.positions result) in
  if positions.x <> [|0.5|] || positions.y <> [|0.5|]
      || float_values ~owner:Attribute.Point "weight" result <> [|7.5|]
      || float_values ~owner:Attribute.Point "vertex_weight" result <> [|175.|]
      || float_values ~owner:Attribute.Point "primitive_weight" result <> [|42.|]
      || float_values ~owner:Attribute.Point "detail_weight" result <> [|7.|]
      || Geometry.find_attribute ~owner:Attribute.Point "id" result <> None then
    fail "owner-specific Attribute Interpolate pattern expansion";
  expect_invalid "malformed interpolation pattern accepted"
    (Attribute_ops_front.interpolate ~point_pattern:"[bad"
      ~target_owner:Attribute.Point ~attributes:[] ~source ~target ());
  expect_invalid "explicit/pattern duplicate output accepted"
    (Attribute_ops_front.interpolate ~point_pattern:"weight"
      ~target_owner:Attribute.Point ~attributes:[spec Attribute.Point "weight"]
      ~source ~target ())

let test_source_group_matching () =
  let source = triangle_source ()
      |> add_group ~owner:Group.Point ~name:"hot" [|1|]
      |> add_group ~owner:Group.Vertex ~name:"corner_hot" [|2|]
      |> add_group ~owner:Group.Primitive ~name:"face_hot" [|0|] in
  let target = points 3 |> add_drivers ~owner:Attribute.Point [|0;0;0|]
      [|(0.,0.,0.);(1.,0.,0.);(0.,1.,0.)|]
      |> add_group ~owner:Group.Point ~name:"hot" [|0;2|]
      |> add_group ~owner:Group.Point ~name:"corner_hot" [|2|] in
  let selection = Group.ordered ~owner:Group.Point ~name:"selected" ~length:3
      [|0;1|] |> Result.get_ok in
  let result = Attribute_ops_front.interpolate ~selection ~match_groups:true
      ~point_pattern:"hot" ~vertex_pattern:"corner_*"
      ~primitive_pattern:"face_*" ~target_owner:Attribute.Point
      ~attributes:[] ~source ~target () |> get_ok in
  if group_values ~owner:Group.Point "hot" result
      <> [|false;true;true|]
      || group_values ~owner:Group.Point "corner_hot" result
         <> [|false;false;true|]
      || group_values ~owner:Group.Point "face_hot" result
         <> [|true;true;false|] then
    fail "primitive/UVW source group interpolation, selection, or preservation";
  let blend_target = points 2 |> add_drivers ~owner:Attribute.Point [|0;0|]
      [|(1.,0.,0.);(0.,0.,0.)|]
      |> add_group ~owner:Group.Point ~name:"hot" [|1|] in
  let blended = Attribute_ops_front.interpolate ~blend:0.5 ~match_groups:true
      ~point_pattern:"hot" ~target_owner:Attribute.Point ~attributes:[]
      ~source ~target:blend_target () |> get_ok in
  if group_values ~owner:Group.Point "hot" blended <> [|true;true|] then
    fail "source group inclusive half-threshold/existing blend";
  let weighted_target = points 2
      |> add_weight_drivers ~owner:Attribute.Point
           [|[|0;2|];[|0;1|]|] [|[|0.25;0.75|];[|0.8;0.2|]|] in
  let weighted = Attribute_ops_front.interpolate
      ~driver:(Attribute_ops.Vertex_weights {
        numbers_attribute="source_elements";
        weights_attribute="source_weights" })
      ~match_groups:true ~point_pattern:"hot" ~vertex_pattern:"corner_*"
      ~primitive_pattern:"face_*" ~target_owner:Attribute.Point
      ~attributes:[] ~source ~target:weighted_target () |> get_ok in
  if group_values ~owner:Group.Point "hot" weighted <> [|false;false|]
      || group_values ~owner:Group.Point "corner_hot" weighted
         <> [|true;false|]
      || group_values ~owner:Group.Point "face_hot" weighted
         <> [|true;true|] then
    fail "weighted mixed-owner source group interpolation";
  let normalized_target = points 1
      |> add_weight_drivers ~owner:Attribute.Point [|[|1|]|] [|[|0.1|]|] in
  let normalized = Attribute_ops_front.interpolate
      ~driver:(Attribute_ops.Point_weights {
        numbers_attribute="source_elements";
        weights_attribute="source_weights" })
      ~normalize_weights:true ~threshold:0.4 ~match_groups:true
      ~point_pattern:"hot" ~target_owner:Attribute.Point ~attributes:[]
      ~source ~target:normalized_target () |> get_ok in
  if group_values ~owner:Group.Point "hot" normalized <> [|false|] then
    fail "normalized source group threshold influence";
  let duplicate_source = source
      |> add_group ~owner:Group.Primitive ~name:"hot" [|0|] in
  let expect_invalid label = function
    | Error error when String.equal (Error.code error) "invalid_interpolate" -> ()
    | _ -> fail label in
  expect_invalid "duplicate cross-owner destination group accepted"
    (Attribute_ops_front.interpolate ~match_groups:true ~point_pattern:"hot"
      ~primitive_pattern:"hot" ~target_owner:Attribute.Point ~attributes:[]
      ~source:duplicate_source ~target ());
  expect_invalid "point-weight mode accepted primitive source group"
    (Attribute_ops_front.interpolate
      ~driver:(Attribute_ops.Point_weights {
        numbers_attribute="source_elements";
        weights_attribute="source_weights" })
      ~match_groups:true ~primitive_pattern:"face_*"
      ~target_owner:Attribute.Point ~attributes:[] ~source
      ~target:weighted_target ())

let expect_code code label = function
  | Error error when String.equal (Error.code error) code -> ()
  | _ -> fail label

let test_errors_and_cancellation () =
  let source = triangle_source () and target = points 1 in
  expect_code "invalid_interpolate" "missing drivers accepted"
    (Attribute_ops_front.interpolate ~target_owner:Attribute.Point
      ~attributes:[spec Attribute.Point "weight"] ~source ~target ());
  let wrong_driver = target
      |> add (Attribute.Float [|0.|]) ~owner:Attribute.Point
           ~name:"source_primitive"
      |> add (Attribute.Float3 (packed3 [|(0.,0.,0.)|]))
           ~owner:Attribute.Point ~name:"source_uvw" in
  expect_code "invalid_interpolate" "wrong driver storage accepted"
    (Attribute_ops_front.interpolate ~target_owner:Attribute.Point
      ~attributes:[spec Attribute.Point "weight"] ~source
      ~target:wrong_driver ());
  let incompatible = points 1 |> add_drivers ~owner:Attribute.Point [|0|]
      [|(0.,0.,0.)|]
      |> add (Attribute.Text [|"bad"|]) ~owner:Attribute.Point ~name:"weight" in
  expect_code "invalid_interpolate" "incompatible target accepted"
    (Attribute_ops_front.interpolate ~target_owner:Attribute.Point
      ~attributes:[spec Attribute.Point "weight"] ~source
      ~target:incompatible ());
  let driven = points 1 |> add_drivers ~owner:Attribute.Point [|0|]
      [|(Float.nan,0.,0.)|] in
  expect_code "invalid_parameter" "non-finite UVW accepted"
    (Attribute_ops_front.interpolate ~target_owner:Attribute.Point
      ~attributes:[spec Attribute.Point "weight"] ~source ~target:driven ());
  let driven = points 1 |> add_drivers ~owner:Attribute.Point [|0|]
      [|(0.,0.,0.)|] in
  expect_code "invalid_interpolate" "duplicate target accepted"
    (Attribute_ops_front.interpolate ~target_owner:Attribute.Point
      ~attributes:[spec Attribute.Point "weight";
        spec ~into:"weight" Attribute.Point "id"] ~source ~target:driven ());
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  expect_code "cancelled" "cancelled interpolation published output"
    (Attribute_ops_front.interpolate ~cancel:cancelled ~target_owner:Attribute.Point
      ~attributes:[spec Attribute.Point "weight"] ~source ~target:driven ());
  let bad_layout = points 2
      |> add (Attribute.Int_array (int_rows [|[|0|];[|1|]|]))
           ~owner:Attribute.Point ~name:"source_elements"
      |> add (Attribute.Float_array (float_rows [|[|1.;0.|];[||]|]))
           ~owner:Attribute.Point ~name:"source_weights" in
  expect_code "invalid_interpolate" "mismatched weighted row layouts accepted"
    (Attribute_ops_front.interpolate ~driver:(Attribute_ops.Point_weights {
       numbers_attribute="source_elements"; weights_attribute="source_weights" })
      ~target_owner:Attribute.Point ~attributes:[spec Attribute.Point "weight"]
      ~source ~target:bad_layout ());
  let bad_number = points 1 |> add_weight_drivers ~owner:Attribute.Point
      [|[|99|]|] [|[|1.|]|] in
  expect_code "invalid_interpolate" "out-of-range weighted number accepted"
    (Attribute_ops_front.interpolate ~driver:(Attribute_ops.Point_weights {
       numbers_attribute="source_elements"; weights_attribute="source_weights" })
      ~target_owner:Attribute.Point ~attributes:[spec Attribute.Point "weight"]
      ~source ~target:bad_number ());
  let bad_weight = points 1 |> add_weight_drivers ~owner:Attribute.Point
      [|[|0|]|] [|[|Float.nan|]|] in
  expect_code "invalid_interpolate" "non-finite explicit weight accepted"
    (Attribute_ops_front.interpolate ~driver:(Attribute_ops.Point_weights {
       numbers_attribute="source_elements"; weights_attribute="source_weights" })
      ~target_owner:Attribute.Point ~attributes:[spec Attribute.Point "weight"]
      ~source ~target:bad_weight ());
  let point_weighted = points 1 |> add_weight_drivers ~owner:Attribute.Point
      [|[|0|]|] [|[|1.|]|] in
  expect_code "invalid_interpolate" "point mode accepted vertex source field"
    (Attribute_ops_front.interpolate ~driver:(Attribute_ops.Point_weights {
       numbers_attribute="source_elements"; weights_attribute="source_weights" })
      ~target_owner:Attribute.Point
      ~attributes:[spec Attribute.Vertex "vertex_weight"]
      ~source ~target:point_weighted ());
  expect_code "invalid_interpolate" "weighted mode accepted computed outputs"
    (Attribute_ops_front.interpolate ~driver:(Attribute_ops.Point_weights {
       numbers_attribute="source_elements"; weights_attribute="source_weights" })
      ~compute_weights:{ computed_owner=Attribute.Point;
        computed_numbers_attribute="computed_numbers";
        computed_weights_attribute="computed_weights" }
      ~target_owner:Attribute.Point ~attributes:[spec Attribute.Point "weight"]
      ~source ~target:point_weighted ());
  expect_code "cancelled" "cancelled weighted interpolation published output"
    (Attribute_ops_front.interpolate ~cancel:cancelled
      ~driver:(Attribute_ops.Point_weights {
       numbers_attribute="source_elements"; weights_attribute="source_weights" })
      ~target_owner:Attribute.Point ~attributes:[spec Attribute.Point "weight"]
      ~source ~target:point_weighted ())

let test_parallel_exact_scale () =
  let count = 200_003 in
  let source = make_geometry
      [|(0.,0.,0.); (0.,1.,0.); (1.,1.,0.); (1.,0.,0.)|]
      (fun topology -> Rdk_test_support.Tb.add_polygon topology [|0;1;2;3|])
      |> add (Attribute.Float (Array.init 4 float_of_int))
           ~owner:Attribute.Point ~name:"weight"
      |> add (Attribute.Int [|3;5;7;11|]) ~owner:Attribute.Point ~name:"id"
      |> add (Attribute.Text [|"a";"b";"c";"d"|])
           ~owner:Attribute.Point ~name:"label"
      |> add (Attribute.Float4 (Packed.Float4.of_owned
        ~x:[|0.;1.;2.;3.|] ~y:[|4.;5.;6.;7.|]
        ~z:[|8.;9.;10.;11.|] ~w:[|12.;13.;14.;15.|] |> Result.get_ok))
           ~owner:Attribute.Point ~name:"tuple4"
      |> add (Attribute.Int_array
          (int_rows [|[|0|];[|1;2|];[||];[|3;4;5|]|]))
           ~owner:Attribute.Point ~name:"neighbors"
      |> add (Attribute.Float_array
          (float_rows [|[|0.25|];[|1.;2.|];[||];[|3.;4.;5.|]|]))
           ~owner:Attribute.Point ~name:"samples" in
  let primitives = Array.make count 0 in
  let uvw = Array.init count (fun index ->
    float_of_int (index land 255) /. 255.,
    float_of_int ((index * 17) land 255) /. 255., 0.) in
  let target = points count |> add_drivers ~owner:Attribute.Point primitives uvw in
  let attributes = [spec Attribute.Point "P"; spec Attribute.Point "weight";
    spec Attribute.Point "id"; spec Attribute.Point "label";
    spec Attribute.Point "tuple4"; spec Attribute.Point "neighbors";
    spec Attribute.Point "samples"] in
  let compute_weights : Attribute_ops.interpolate_computed = {
    computed_owner=Attribute.Point;
    computed_numbers_attribute="computed_points";
    computed_weights_attribute="computed_weights" } in
  let run domains = Parallel.run ~domains (fun () ->
    Attribute_ops_front.interpolate ~grain:257 ~target_owner:Attribute.Point
      ~compute_weights ~attributes ~source ~target () |> get_ok) in
  let one = run 1 and four = run 4 in
  let p_one = Packed.Float3.View.view (Geometry.positions one)
  and p_four = Packed.Float3.View.view (Geometry.positions four)
  and q_one = float4_values ~owner:Attribute.Point "tuple4" one
  and q_four = float4_values ~owner:Attribute.Point "tuple4" four
  and cn_one = Packed.Int_array.View.view
      (int_array_values ~owner:Attribute.Point "computed_points" one)
  and cn_four = Packed.Int_array.View.view
      (int_array_values ~owner:Attribute.Point "computed_points" four)
  and array_i_one = Packed.Int_array.View.view
      (int_array_values ~owner:Attribute.Point "neighbors" one)
  and array_i_four = Packed.Int_array.View.view
      (int_array_values ~owner:Attribute.Point "neighbors" four)
  and array_f_one = Packed.Float_array.View.view
      (float_array_values ~owner:Attribute.Point "samples" one)
  and array_f_four = Packed.Float_array.View.view
      (float_array_values ~owner:Attribute.Point "samples" four) in
  let cw geometry = Geometry.find_attribute ~owner:Attribute.Point
      "computed_weights" geometry |> Option.get
      |> Attribute.View.storage
      |> (function Attribute.Float_array values -> values
                 | _ -> fail "wrong computed_weights storage")
      |> Packed.Float_array.View.view in
  let cw_one = cw one and cw_four = cw four in
  if p_one.x <> p_four.x || p_one.y <> p_four.y || p_one.z <> p_four.z
      || float_values ~owner:Attribute.Point "weight" one
         <> float_values ~owner:Attribute.Point "weight" four
      || int_values ~owner:Attribute.Point "id" one
         <> int_values ~owner:Attribute.Point "id" four
      || text_values ~owner:Attribute.Point "label" one
         <> text_values ~owner:Attribute.Point "label" four
      || q_one.x <> q_four.x || q_one.y <> q_four.y
      || q_one.z <> q_four.z || q_one.w <> q_four.w
      || cn_one.offsets <> cn_four.offsets || cn_one.values <> cn_four.values
      || array_i_one.offsets <> array_i_four.offsets
      || array_i_one.values <> array_i_four.values
      || array_f_one.offsets <> array_f_four.offsets
      || array_f_one.values <> array_f_four.values
      || cw_one.offsets <> cw_four.offsets || cw_one.values <> cw_four.values then
    fail "200k Attribute Interpolate one/four-domain exactness";
  let weighted_run domains = Parallel.run ~domains (fun () ->
    Attribute_ops_front.interpolate ~grain:257
      ~driver:(Attribute_ops.Point_weights {
        numbers_attribute="computed_points";
        weights_attribute="computed_weights" })
      ~target_owner:Attribute.Point ~attributes ~source ~target:one () |> get_ok) in
  let weighted_one = weighted_run 1 and weighted_four = weighted_run 4 in
  let wp_one = Packed.Float3.View.view (Geometry.positions weighted_one)
  and wp_four = Packed.Float3.View.view (Geometry.positions weighted_four)
  and wq_one = float4_values ~owner:Attribute.Point "tuple4" weighted_one
  and wq_four = float4_values ~owner:Attribute.Point "tuple4" weighted_four in
  if wp_one.x <> wp_four.x || wp_one.y <> wp_four.y || wp_one.z <> wp_four.z
      || float_values ~owner:Attribute.Point "weight" weighted_one
         <> float_values ~owner:Attribute.Point "weight" weighted_four
      || int_values ~owner:Attribute.Point "id" weighted_one
         <> int_values ~owner:Attribute.Point "id" weighted_four
      || wq_one.x <> wq_four.x || wq_one.y <> wq_four.y
      || wq_one.z <> wq_four.z || wq_one.w <> wq_four.w
      || geometry_bytes weighted_one <> geometry_bytes weighted_four then
    fail "200k weighted Attribute Interpolate one/four-domain exactness"

let run () =
  test_triangle_mixed_storage_and_position ();
  test_quad_ngon_and_curves ();
  test_destination_owners_group_blend_and_misses ();
  test_explicit_weight_modes ();
  test_signed_normalization_and_group_threshold ();
  test_array_field_row_selection ();
  test_computed_weights_equivalence ();
  test_attribute_pattern_expansion ();
  test_source_group_matching ();
  test_errors_and_cancellation ();
  test_parallel_exact_scale ();
  print_endline "attribute interpolate tests passed"
