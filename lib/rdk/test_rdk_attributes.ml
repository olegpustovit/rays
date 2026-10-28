open Rays
open Rdk
open Rdk_test_support

let get_ok = function Ok value -> value | Error _ -> fail "unexpected error"

let int_array_values ~owner ~name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Int_array values -> Packed.Int_array.View.view values
       | _ -> fail ("unexpected non-integer-array storage for " ^ name))
  | None -> fail ("missing integer-array attribute " ^ name)

let promotion ~geometry ~triangle_geometry =
  let point_weight = Attribute.create_owned ~name:"weight"
      ~owner:Attribute.Point (Attribute.Float [|1.; 2.; 3.; 4.|]) |> get_ok in
  let weighted_quad = Geometry.with_attribute point_weight geometry |> get_ok in
  let promoted domains = Parallel.run ~domains (fun () ->
      promote ~grain:1 ~source:Attribute.Point
        ~destination:Attribute.Primitive ~name:"weight" weighted_quad |> get_ok) in
  let promoted_one = promoted 1 and promoted_many = promoted 4 in
  let promoted_values value = Geometry.find_attribute
      ~owner:Attribute.Primitive "weight" value |> Option.get
      |> Attribute.get (Attribute.key ~name:"weight" ~owner:Attribute.Primitive
          Attribute.float) |> Option.get in
  if promoted_values promoted_one <> [|2.5|]
     || promoted_values promoted_one <> promoted_values promoted_many
     || Geometry.find_attribute ~owner:Attribute.Point "weight" promoted_one <> None
  then fail "point-to-primitive attribute promotion";
  let primitive_weight = Attribute.create_owned ~name:"piece_weight"
      ~owner:Attribute.Primitive (Attribute.Float [|10.; 20.|]) |> get_ok in
  let weighted_triangles = Geometry.with_attribute primitive_weight
      triangle_geometry |> get_ok in
  let point_weights = promote ~grain:1
      ~source:Attribute.Primitive ~destination:Attribute.Point
      ~name:"piece_weight" weighted_triangles |> get_ok in
  let point_weights = Geometry.find_attribute ~owner:Attribute.Point
      "piece_weight" point_weights |> Option.get
      |> Attribute.get (Attribute.key ~name:"piece_weight" ~owner:Attribute.Point
          Attribute.float) |> Option.get in
  if point_weights <> [|15.; 10.; 15.; 20.|] then
    fail "primitive-to-point unique-incidence promotion";
  let reduction_values = Attribute.create_owned ~name:"sample"
      ~owner:Attribute.Point (Attribute.Float [|9.; 2.; 9.; 2.|]) |> get_ok
  and reduction_ids = Attribute.create_owned ~name:"sample_id"
      ~owner:Attribute.Point (Attribute.Int [|9; 2; 9; 2|]) |> get_ok
  and reduction_labels = Attribute.create_owned ~name:"sample_label"
      ~owner:Attribute.Point (Attribute.Text [|"z"; "a"; "z"; "a"|]) |> get_ok in
  let reductions = geometry
      |> Geometry.with_attribute reduction_values |> get_ok
      |> Geometry.with_attribute reduction_ids |> get_ok
      |> Geometry.with_attribute reduction_labels |> get_ok in
  let owner_point = Attribute.create_owned ~name:"owner_point"
      ~owner:Attribute.Point (Attribute.Int [|10; 11; 12; 13|]) |> get_ok
  and owner_vertex = Attribute.create_owned ~name:"owner_vertex"
      ~owner:Attribute.Vertex (Attribute.Int [|20; 21; 22; 23; 24; 25|]) |> get_ok
  and owner_primitive = Attribute.create_owned ~name:"owner_primitive"
      ~owner:Attribute.Primitive (Attribute.Int [|30; 31|]) |> get_ok
  and owner_detail = Attribute.create_owned ~name:"owner_detail"
      ~owner:Attribute.Detail (Attribute.Int [|40|]) |> get_ok in
  let owner_source = List.fold_left (fun geometry attribute ->
    Geometry.with_attribute attribute geometry |> get_ok) triangle_geometry
      [owner_point;owner_vertex;owner_primitive;owner_detail] in
  let owner_source_bytes = geometry_bytes owner_source in
  let transfer_owners domains = Parallel.run ~domains (fun () ->
    Attribute_ops.transfer_all ~grain:1 ~point_pattern:"owner*"
      ~vertex_pattern:"owner*" ~primitive_pattern:"owner*" ~detail_pattern:"owner*"
      ~source:owner_source ~target:triangle_geometry () |> get_ok) in
  let owners_one = transfer_owners 1 and owners_many = transfer_owners 4 in
  let owner_specific = Attribute_ops.transfer_points ~pattern:"owner*"
      ~source:owner_source ~target:triangle_geometry () |> get_ok in
  let owner_specific = Attribute_ops.transfer_primitives ~pattern:"owner*"
      ~source:owner_source ~target:owner_specific () |> get_ok in
  let owner_specific = Attribute_ops.transfer_vertices ~pattern:"owner*"
      ~source:owner_source ~target:owner_specific () |> get_ok in
  let owner_specific = Attribute_ops.transfer_detail ~pattern:"owner*"
      ~source:owner_source ~target:owner_specific () |> get_ok in
  check (geometry_bytes owners_one = geometry_bytes owners_many
      && geometry_bytes owners_one = geometry_bytes owner_specific
      && geometry_bytes owner_source = owner_source_bytes)
    "all-owner transfer matches owner kernels/domain exactness/unchanged input";
  List.iter (fun (owner, name, values) ->
    check (Attribute.storage (Geometry.find_attribute ~owner name owners_one
        |> Option.get) = Attribute.Int values)
      ("all-owner transfer values for " ^ name)) Attribute.[
      Point,"owner_point",[|10;11;12;13|];
      Vertex,"owner_vertex",[|20;21;22;20;22;25|];
      Primitive,"owner_primitive",[|30;31|];
      Detail,"owner_detail",[|40|]];
  let promote_array domains method_ name = Parallel.run ~domains (fun () ->
    promote ~grain:1 ~method_ ~delete_source:false
      ~source:Attribute.Point ~destination:Attribute.Detail ~name reductions
    |> get_ok) in
  let float_all_one = promote_array 1 Attribute_ops.Array_all "sample"
  and float_all_many = promote_array 4 Attribute_ops.Array_all "sample"
  and float_unique_one = promote_array 1 Attribute_ops.Unique_values "sample"
  and float_unique_many = promote_array 4 Attribute_ops.Unique_values "sample"
  and int_all_one = promote_array 1 Attribute_ops.Array_all "sample_id"
  and int_all_many = promote_array 4 Attribute_ops.Array_all "sample_id"
  and int_unique_one = promote_array 1 Attribute_ops.Unique_values "sample_id"
  and int_unique_many = promote_array 4 Attribute_ops.Unique_values "sample_id" in
  let float_all = float_array_values ~owner:Attribute.Detail ~name:"sample"
      float_all_one
  and float_all_parallel = float_array_values ~owner:Attribute.Detail
      ~name:"sample" float_all_many
  and float_unique = float_array_values ~owner:Attribute.Detail ~name:"sample"
      float_unique_one
  and float_unique_parallel = float_array_values ~owner:Attribute.Detail
      ~name:"sample" float_unique_many
  and int_all = int_array_values ~owner:Attribute.Detail ~name:"sample_id"
      int_all_one
  and int_all_parallel = int_array_values ~owner:Attribute.Detail
      ~name:"sample_id" int_all_many
  and int_unique = int_array_values ~owner:Attribute.Detail ~name:"sample_id"
      int_unique_one
  and int_unique_parallel = int_array_values ~owner:Attribute.Detail
      ~name:"sample_id" int_unique_many in
  if float_all.offsets <> [|0; 4|] || float_all.values <> [|9.; 2.; 9.; 2.|]
      || float_unique.offsets <> [|0; 2|]
      || float_unique.values <> [|2.; 9.|]
      || int_all.offsets <> [|0; 4|] || int_all.values <> [|9; 2; 9; 2|]
      || int_unique.offsets <> [|0; 2|] || int_unique.values <> [|2; 9|]
      || float_all <> float_all_parallel
      || float_unique <> float_unique_parallel
      || int_all <> int_all_parallel || int_unique <> int_unique_parallel then
    fail "packed all/unique promotion values/domain exactness";
  let unique_special = Attribute.create_owned ~name:"unique_special"
      ~owner:Attribute.Point (Attribute.Float [|nan; nan; 0.; -0.|]) |> get_ok in
  let unique_special_source = Geometry.with_attribute unique_special geometry
      |> get_ok in
  let unique_special domains = Parallel.run ~domains (fun () ->
      promote ~grain:1 ~method_:Attribute_ops.Unique_values
        ~source:Attribute.Point ~destination:Attribute.Detail
        ~name:"unique_special" unique_special_source |> get_ok
      |> float_array_values ~owner:Attribute.Detail ~name:"unique_special") in
  let unique_special_one = unique_special 1
  and unique_special_many = unique_special 4 in
  let exact_special = unique_special_one.offsets = unique_special_many.offsets
      && Array.length unique_special_one.values
         = Array.length unique_special_many.values
      && Array.for_all2 (fun left right ->
           Int64.bits_of_float left = Int64.bits_of_float right)
           unique_special_one.values unique_special_many.values in
  if not exact_special || unique_special_one.offsets <> [|0; 2|]
      || not (Array.exists Float.is_nan unique_special_one.values)
      || not (Array.exists (fun value -> value = 0.) unique_special_one.values)
  then fail "unique float total-comparison equivalence/domain exactness";
  let promoted_pattern domains = Parallel.run ~domains (fun () ->
      Attribute_ops.promote_pattern ~grain:1 ~method_:Attribute_ops.First
        ~delete_source:false ~source:Attribute.Point
        ~destination:Attribute.Detail ~pattern:"sample* ^sample_label"
        reductions |> get_ok) in
  let pattern_one = promoted_pattern 1 and pattern_many = promoted_pattern 4 in
  check (geometry_bytes pattern_one = geometry_bytes pattern_many
      && Attribute.storage (Geometry.find_attribute ~owner:Attribute.Detail
          "sample" pattern_one |> Option.get) = Attribute.Float [|9.|]
      && Attribute.storage (Geometry.find_attribute ~owner:Attribute.Detail
          "sample_id" pattern_one |> Option.get) = Attribute.Int [|9|]
      && Geometry.find_attribute ~owner:Attribute.Detail "sample_label"
          pattern_one = None
      && List.for_all (fun name -> Geometry.find_attribute ~owner:Attribute.Point
          name pattern_one <> None) ["sample";"sample_id";"sample_label"])
    "pattern promotion values/exclusion/source preservation/domain exactness";
  let array_pattern domains = Parallel.run ~domains (fun () ->
      Attribute_ops.promote_pattern ~grain:1
        ~method_:Attribute_ops.Unique_values ~delete_source:false
        ~source:Attribute.Point ~destination:Attribute.Detail
        ~pattern:"sample sample_id" reductions |> get_ok) in
  let array_pattern_one = array_pattern 1 and array_pattern_many = array_pattern 4 in
  let array_pattern_float geometry = float_array_values ~owner:Attribute.Detail
      ~name:"sample" geometry
  and array_pattern_int geometry = int_array_values ~owner:Attribute.Detail
      ~name:"sample_id" geometry in
  if array_pattern_float array_pattern_one
      <> array_pattern_float array_pattern_many
      || array_pattern_int array_pattern_one <> array_pattern_int array_pattern_many
      || (array_pattern_float array_pattern_one).values <> [|2.; 9.|]
      || (array_pattern_int array_pattern_one).values <> [|2; 9|] then
    fail "pattern unique-array promotion/domain exactness";
  let unmatched_pattern = Attribute_ops.promote_pattern
      ~source:Attribute.Point ~destination:Attribute.Detail
      ~pattern:"missing*" reductions |> get_ok in
  if Geometry.data_id unmatched_pattern <> Geometry.data_id reductions then
    fail "unmatched promotion pattern was not an identity";
  (match Attribute_ops.promote_pattern ~source:Attribute.Point
      ~destination:Attribute.Detail ~pattern:"[bad" reductions with
   | Error error when Error.code error = "invalid_attribute" -> ()
   | _ -> fail "pattern promotion accepted a malformed glob");
  let overlap_source = Rdk.Line_geometry.points
      [|(0., 0., 0.); (1., 0., 0.); (2., 0., 0.)|] in
  let overlap_a = Attribute.create_owned ~name:"a" ~owner:Attribute.Point
      (Attribute.Int [|1; 2; 3|]) |> get_ok
  and overlap_copy = Attribute.create_owned ~name:"copy_a"
      ~owner:Attribute.Point (Attribute.Int [|4; 5; 6|]) |> get_ok
  and overlap_piece = Attribute.create_owned ~name:"piece"
      ~owner:Attribute.Point (Attribute.Int [|0; 1; 2|]) |> get_ok in
  let overlap_source = overlap_source
      |> Geometry.with_attribute overlap_a |> get_ok
      |> Geometry.with_attribute overlap_copy |> get_ok
      |> Geometry.with_attribute overlap_piece |> get_ok in
  let same_owner = Attribute_ops.promote_pattern ~method_:Attribute_ops.First
      ~source:Attribute.Point ~destination:Attribute.Point ~pattern:"a copy_a"
      ~into_pattern:"renamed copy_a" overlap_source |> get_ok in
  let same_owner_values name = match Attribute.storage
      (Geometry.find_attribute ~owner:Attribute.Point name same_owner
       |> Option.get) with Attribute.Int values -> values | _ -> [||] in
  if Geometry.find_attribute ~owner:Attribute.Point "a" same_owner <> None
      || same_owner_values "renamed" <> [|1; 2; 3|]
      || same_owner_values "copy_a" <> [|4; 5; 6|] then
    fail "pattern same-owner rename changed an unrenamed attribute";
  let same_owner_noop = Attribute_ops.promote_pattern ~source:Attribute.Point
      ~destination:Attribute.Point ~pattern:"a" overlap_source |> get_ok in
  if Geometry.data_id same_owner_noop <> Geometry.data_id overlap_source then
    fail "pattern same-owner identity changed geometry";
  let overlap_promoted = Attribute_ops.promote_pattern ~method_:Attribute_ops.First
      ~piece_attribute:"piece" ~source:Attribute.Point
      ~destination:Attribute.Point ~pattern:"[ac]*"
      ~into_pattern:"copy_?*" overlap_source |> get_ok in
  let overlap_result name = match Attribute.storage
      (Geometry.find_attribute ~owner:Attribute.Point name overlap_promoted
       |> Option.get) with Attribute.Int values -> values | _ -> [||] in
  if Geometry.find_attribute ~owner:Attribute.Point "a" overlap_promoted <> None
      || overlap_result "copy_a" <> [|1; 2; 3|]
      || overlap_result "copy_copy_a" <> [|4; 5; 6|] then
    fail "pattern promotion overlapping simultaneous rename/delete";
  (match Attribute_ops.promote_pattern ~source:Attribute.Point
      ~destination:Attribute.Point ~pattern:"sample*" ~into_pattern:"P*"
      reductions with
   | Error error when Error.code error = "invalid_attribute" -> ()
   | _ -> fail "pattern promotion accepted a canonical P rewrite");
  (match promote ~method_:Attribute_ops.Average
      ~index_attribute:"source_index" ~source:Attribute.Point
      ~destination:Attribute.Detail ~name:"sample" reductions with
   | Error error when Error.code error = "invalid_attribute" -> ()
   | _ -> fail "average promotion accepted a source index output");
  (match promote ~method_:Attribute_ops.First
      ~into:"collision" ~index_attribute:"collision" ~source:Attribute.Point
      ~destination:Attribute.Detail ~name:"sample" reductions with
   | Error error when Error.code error = "invalid_attribute" -> ()
   | _ -> fail "promotion accepted colliding value/index names");
  let tuple_values = Packed.Float2.of_owned ~x:[|1.; 2.; 3.; 4.|]
      ~y:[|4.; 3.; 2.; 1.|] |> get_ok in
  let tuple_attribute = Attribute.create_owned ~name:"tuple_value"
      ~owner:Attribute.Point (Attribute.Float2 tuple_values) |> get_ok in
  let tuple_source = Geometry.with_attribute tuple_attribute reductions |> get_ok in
  let tuple_indexed = promote ~method_:Attribute_ops.Maximum
      ~index_attribute:"tuple_source" ~source:Attribute.Point
      ~destination:Attribute.Detail ~name:"tuple_value" tuple_source |> get_ok in
  let tuple_source_indices = int_array_values ~owner:Attribute.Detail
      ~name:"tuple_source" tuple_indexed in
  if tuple_source_indices.offsets <> [|0;2|]
      || tuple_source_indices.values <> [|3;0|] then
    fail "tuple promotion component source indices";
  let empty_source = Line_geometry.points [||] in
  let empty_value = Attribute.create_owned ~name:"empty_value"
      ~owner:Attribute.Point (Attribute.Float [||]) |> get_ok in
  let empty_source = Geometry.with_attribute empty_value empty_source |> get_ok in
  let empty_array = promote ~method_:Attribute_ops.Array_all
      ~source:Attribute.Point ~destination:Attribute.Detail ~name:"empty_value"
      empty_source |> get_ok
      |> float_array_values ~owner:Attribute.Detail ~name:"empty_value" in
  if empty_array.offsets <> [|0; 0|] || empty_array.values <> [||] then
    fail "empty array promotion row";
  (match promote ~method_:Attribute_ops.Array_all
      ~index_attribute:"source_index" ~source:Attribute.Point
      ~destination:Attribute.Detail ~name:"sample" reductions with
   | Error error when Error.code error = "invalid_attribute" -> ()
   | _ -> fail "array promotion accepted an index attribute");
  (match promote ~method_:Attribute_ops.Unique_values
      ~source:Attribute.Point ~destination:Attribute.Detail
      ~name:"tuple_value" tuple_source with
   | Error error when Error.code error = "invalid_attribute" -> ()
   | _ -> fail "array promotion flattened a wider tuple ambiguously");
  (match promote ~method_:Attribute_ops.Array_all
      ~source:Attribute.Point ~destination:Attribute.Detail
      ~name:"sample_label" reductions with
   | Error error when Error.code error = "invalid_attribute" -> ()
   | _ -> fail "array promotion invented text-array storage");
  let source_array = Packed.Int_array.create_owned ~offsets:[|0; 1; 2; 3; 4|]
      ~values:[|1; 2; 3; 4|] |> get_ok in
  let source_array = Attribute.create_owned ~name:"source_array"
      ~owner:Attribute.Point (Attribute.Int_array source_array) |> get_ok in
  let source_array_geometry = Geometry.with_attribute source_array reductions
      |> get_ok in
  (match promote ~method_:Attribute_ops.Array_all
      ~source:Attribute.Point ~destination:Attribute.Detail
      ~name:"source_array" source_array_geometry with
   | Error error when Error.code error = "invalid_attribute" -> ()
   | _ -> fail "array promotion nested existing arrays without a policy");
  let piece = Attribute.create_owned ~name:"piece"
      ~owner:Attribute.Primitive (Attribute.Int [|7; 7|]) |> get_ok in
  let piece_source = triangle_geometry
      |> Geometry.with_attribute reduction_values |> get_ok
      |> Geometry.with_attribute piece |> get_ok in
  let piece_promoted = promote ~grain:1
      ~method_:Attribute_ops.First ~delete_source:false ~piece_attribute:"piece"
      ~source:Attribute.Point ~destination:Attribute.Primitive ~name:"sample"
      piece_source |> get_ok in
  let piece_values = Geometry.find_attribute ~owner:Attribute.Primitive "sample"
      piece_promoted |> Option.get |> Attribute.get (Attribute.key ~name:"sample"
        ~owner:Attribute.Primitive Attribute.float) |> Option.get in
  if piece_values <> [|9.; 9.|] then
    fail "piece promotion did not deduplicate/order contributing sources";
  let piece_arrays = promote ~grain:1
      ~method_:Attribute_ops.Array_all ~delete_source:false
      ~piece_attribute:"piece" ~into:"piece_samples" ~source:Attribute.Point
      ~destination:Attribute.Primitive ~name:"sample" piece_source |> get_ok in
  let piece_arrays = float_array_values ~owner:Attribute.Primitive
      ~name:"piece_samples" piece_arrays in
  if piece_arrays.offsets <> [|0; 4; 8|]
      || piece_arrays.values <> [|9.; 2.; 9.; 2.; 9.; 2.; 9.; 2.|] then
    fail "piece array promotion did not remap complete ordered rows";
  let piece_averaged = promote ~grain:1
      ~method_:Attribute_ops.Average ~delete_source:false ~piece_attribute:"piece"
      ~source:Attribute.Point ~destination:Attribute.Primitive ~name:"sample"
      piece_source |> get_ok in
  let piece_averages = Geometry.find_attribute ~owner:Attribute.Primitive "sample"
      piece_averaged |> Option.get |> Attribute.get (Attribute.key ~name:"sample"
        ~owner:Attribute.Primitive Attribute.float) |> Option.get in
  if piece_averages <> [|5.5; 5.5|] then
    fail "piece promotion counted shared source elements more than once";
  let primitive_arrays = promote ~grain:1
      ~method_:Attribute_ops.Array_all ~delete_source:false
      ~source:Attribute.Primitive ~destination:Attribute.Point
      ~name:"piece_weight" weighted_triangles |> get_ok in
  let primitive_arrays = float_array_values ~owner:Attribute.Point
      ~name:"piece_weight" primitive_arrays in
  if primitive_arrays.offsets <> [|0; 2; 3; 5; 6|]
      || primitive_arrays.values <> [|10.; 20.; 10.; 10.; 20.; 20.|] then
    fail "primitive-to-point array promotion incidence order";
  let point_piece = Attribute.create_owned ~name:"point_piece"
      ~owner:Attribute.Point (Attribute.Text [|"a"; "a"; "b"; "b"|]) |> get_ok in
  let point_piece_source = reductions |> Geometry.with_attribute point_piece |> get_ok in
  let same_owner_piece = promote ~method_:Attribute_ops.Average
      ~delete_source:false ~piece_attribute:"point_piece" ~into:"piece_mean"
      ~source:Attribute.Point ~destination:Attribute.Point ~name:"sample"
      point_piece_source |> get_ok in
  let piece_means = Geometry.find_attribute ~owner:Attribute.Point "piece_mean"
      same_owner_piece |> Option.get |> Attribute.get (Attribute.key
        ~name:"piece_mean" ~owner:Attribute.Point Attribute.float) |> Option.get in
  if piece_means <> [|5.5; 5.5; 5.5; 5.5|] then
    fail "text-partition same-owner piece promotion";
  let tuple = Packed.Float2.of_owned ~x:[|1.; 4.; 3.; 2.|]
      ~y:[|10.; 1.; 8.; 7.|] |> get_ok in
  let tuple = Attribute.create_owned ~name:"tuple" ~owner:Attribute.Point
      (Attribute.Float2 tuple) |> get_ok in
  let tuple_source = Geometry.with_attribute tuple point_piece_source |> get_ok in
  let tuple_median = promote ~method_:Attribute_ops.Median
      ~delete_source:false ~piece_attribute:"point_piece" ~into:"tuple_median"
      ~source:Attribute.Point ~destination:Attribute.Point ~name:"tuple"
      tuple_source |> get_ok in
  let tuple_median = Geometry.find_attribute ~owner:Attribute.Point "tuple_median"
      tuple_median |> Option.get |> Attribute.get (Attribute.key
        ~name:"tuple_median" ~owner:Attribute.Point Attribute.float2) |> Option.get
      |> Packed.Float2.View.view in
  if tuple_median.x <> [|4.; 4.; 3.; 3.|]
     || tuple_median.y <> [|10.; 10.; 8.; 8.|] then
    fail "piece tuple promotion was not component-wise upper median";
  let tuple_indexed domains = Parallel.run ~domains (fun () ->
    promote ~grain:1 ~method_:Attribute_ops.Maximum
      ~delete_source:false ~index_attribute:"tuple_sources"
      ~into:"tuple_maximum" ~source:Attribute.Point
      ~destination:Attribute.Detail ~name:"tuple" tuple_source |> get_ok) in
  let tuple_index_one = tuple_indexed 1 and tuple_index_many = tuple_indexed 4 in
  let tuple_maximum geometry = Geometry.find_attribute ~owner:Attribute.Detail
      "tuple_maximum" geometry |> Option.get |> Attribute.get (Attribute.key
        ~name:"tuple_maximum" ~owner:Attribute.Detail Attribute.float2)
      |> Option.get |> Packed.Float2.View.view
  and tuple_sources geometry = int_array_values ~owner:Attribute.Detail
      ~name:"tuple_sources" geometry in
  let maximum = tuple_maximum tuple_index_one
  and sources = tuple_sources tuple_index_one
  and parallel_sources = tuple_sources tuple_index_many in
  if maximum.x <> [|4.|] || maximum.y <> [|10.|]
      || sources.offsets <> [|0;2|] || sources.values <> [|1;0|]
      || sources <> parallel_sources then
    fail "component-wise tuple source-index output/domain exactness";
  let bad_piece = Attribute.create_owned ~name:"bad_piece"
      ~owner:Attribute.Primitive (Attribute.Float [|0.; 0.|]) |> get_ok in
  let bad_piece_source = Geometry.with_attribute bad_piece piece_source |> get_ok in
  (match promote ~piece_attribute:"missing"
      ~source:Attribute.Point ~destination:Attribute.Primitive ~name:"sample"
      piece_source with
   | Error error when Error.code error = "invalid_attribute" -> ()
   | _ -> fail "attribute promote accepted a missing piece attribute");
  (match promote ~piece_attribute:"bad_piece"
      ~source:Attribute.Point ~destination:Attribute.Primitive ~name:"sample"
      bad_piece_source with
   | Error error when Error.code error = "invalid_attribute" -> ()
   | _ -> fail "attribute promote accepted a numeric piece attribute");
  let dense_count = 40_000 and dense_piece_size = 40 in
  let dense = Line_geometry.points (Array.init dense_count (fun point ->
      float_of_int point, 0., 0.)) in
  let dense_values = Attribute.create_owned ~name:"dense_value"
      ~owner:Attribute.Point (Attribute.Int
        (Array.init dense_count (fun point -> (point * 17) mod 101))) |> get_ok
  and dense_pieces = Attribute.create_owned ~name:"dense_piece"
      ~owner:Attribute.Point (Attribute.Int
        (Array.init dense_count (fun point -> point / dense_piece_size))) |> get_ok in
  let dense = dense |> Geometry.with_attribute dense_values |> get_ok
      |> Geometry.with_attribute dense_pieces |> get_ok in
  let dense_promoted domains = Parallel.run ~domains (fun () ->
      promote ~grain:7 ~method_:Attribute_ops.Mode
        ~piece_attribute:"dense_piece" ~into:"piece_mode" ~delete_source:false
        ~source:Attribute.Point ~destination:Attribute.Point ~name:"dense_value"
        dense |> get_ok) in
  let dense_one = dense_promoted 1 and dense_many = dense_promoted 4 in
  check (geometry_bytes dense_one = geometry_bytes dense_many)
    "piece mode promotion complete geometry differs by domain count";
  (* Each 40-point partition has distinct values, so every mode is tied. *)
  let expected_modes = Array.init (dense_count / dense_piece_size) (fun piece ->
    let smallest = ref max_int in
    for point = piece * dense_piece_size to (piece + 1) * dense_piece_size - 1 do
      smallest := min !smallest ((point * 17) mod 101)
    done;
    !smallest) in
  (match Attribute.storage (Geometry.find_attribute ~owner:Attribute.Point
      "piece_mode" dense_one |> Option.get) with
   | Attribute.Int values when Array.length values = dense_count ->
       Array.iteri (fun point value -> check
         (value = expected_modes.(point / dense_piece_size))
         "piece mode promotion chooses the smallest tied value") values
   | _ -> fail "piece mode promotion storage/cardinality");
  let dense_unique domains = Parallel.run ~domains (fun () ->
      promote ~grain:257 ~method_:Attribute_ops.Unique_values
        ~piece_attribute:"dense_piece" ~into:"piece_unique"
        ~delete_source:false ~source:Attribute.Point
        ~destination:Attribute.Point ~name:"dense_value" dense |> get_ok) in
  let dense_unique_one = dense_unique 1 |> int_array_values
      ~owner:Attribute.Point ~name:"piece_unique"
  and dense_unique_many = dense_unique 4 |> int_array_values
      ~owner:Attribute.Point ~name:"piece_unique" in
  if dense_unique_one <> dense_unique_many
      || Array.length dense_unique_one.offsets <> dense_count + 1
      || Array.length dense_unique_one.values <> dense_count * dense_piece_size
      || dense_unique_one.offsets.(dense_count) <> dense_count * dense_piece_size
  then fail "piece unique-array scale/domain exactness";
  let cancelled_promote = Cancel.create () in
  Cancel.cancel cancelled_promote;
  (match promote ~cancel:cancelled_promote
      ~method_:Attribute_ops.Median ~source:Attribute.Point
      ~destination:Attribute.Detail ~name:"dense_value" dense with
   | Error error when Error.code error = "cancelled" -> ()
   | _ -> fail "median promotion ignored cancellation");
  (match Attribute_ops.promote_pattern ~cancel:cancelled_promote
      ~method_:Attribute_ops.Median ~source:Attribute.Point
      ~destination:Attribute.Detail ~pattern:"dense_*" dense with
   | Error error when Error.code error = "cancelled" -> ()
   | _ -> fail "pattern promotion ignored cancellation");
  (match Attribute_ops.promote_pattern ~cancel:cancelled_promote
      ~method_:Attribute_ops.Maximum ~source:Attribute.Point
      ~destination:Attribute.Detail ~pattern:"dense_*"
      ~index_pattern:"source_*" dense with
   | Error error when Error.code error = "cancelled" -> ()
   | _ -> fail "indexed pattern promotion ignored cancellation");
  (match promote ~cancel:cancelled_promote
      ~method_:Attribute_ops.Unique_values ~source:Attribute.Point
      ~destination:Attribute.Detail ~name:"dense_value" dense with
   | Error error when Error.code error = "cancelled" -> ()
   | _ -> fail "unique-array promotion ignored cancellation");
  (match promote ~grain:0 ~source:Attribute.Point
      ~destination:Attribute.Detail ~name:"dense_value" dense with
   | Error error when Error.code error = "invalid_parameter" -> ()
   | _ -> fail "attribute promotion did not structure an invalid grain error");
  let detail = Attribute.create_owned ~name:"gain" ~owner:Attribute.Detail
      (Attribute.Float [|7.|]) |> get_ok in
  let detailed = Geometry.with_attribute detail triangle_geometry |> get_ok in
  let expanded_detail = promote ~source:Attribute.Detail
      ~destination:Attribute.Vertex ~name:"gain" detailed |> get_ok in
  (match Geometry.find_attribute ~owner:Attribute.Vertex "gain" expanded_detail with
   | Some attribute ->
       (match Attribute.storage attribute with
        | Attribute.Float values when values = Array.make 6 7. -> ()
        | _ -> fail "detail-to-vertex attribute promotion")
   | None -> fail "detail promotion missing output");
  let integer = Attribute.create_owned ~name:"id" ~owner:Attribute.Point
      (Attribute.Int [|1; 2; 3; 4|]) |> get_ok in
  let integer_geometry = Geometry.with_attribute integer geometry |> get_ok in
  (match promote ~method_:Attribute_ops.Sum
      ~source:Attribute.Point ~destination:Attribute.Detail ~name:"id"
      integer_geometry with
   | Error error when Error.code error = "invalid_attribute" -> ()
   | _ -> fail "unsupported integer promotion did not return a structured error");
  weighted_triangles

let transfer ~geometry ~triangle_geometry =
  let transfer_source = Line_geometry.points [|(0., 0., 0.); (2., 0., 0.)|] in
  let transfer_weight = Attribute.create_owned ~name:"weight"
      ~owner:Attribute.Point (Attribute.Float [|0.; 10.|]) |> get_ok
  and transfer_id = Attribute.create_owned ~name:"id"
      ~owner:Attribute.Point (Attribute.Int [|5; 9|]) |> get_ok in
  let transfer_source = transfer_source
      |> Geometry.with_attribute transfer_weight |> get_ok
      |> Geometry.with_attribute transfer_id |> get_ok in
  let transfer_target = Line_geometry.points
      [|(0., 0., 0.); (1., 0., 0.); (10., 0., 0.)|] in
  let existing_weight = Attribute.create_owned ~name:"weight"
      ~owner:Attribute.Point (Attribute.Float [|100.; 100.; 100.|]) |> get_ok in
  let transfer_target = Geometry.with_attribute existing_weight transfer_target
      |> get_ok in
  let transferred domains = Parallel.run ~domains (fun () ->
      Attribute_ops.transfer_points ~grain:1 ~max_distance:3.
        ~mode:(Attribute_ops.Inverse_distance { neighbors = 2; power = 1. })
        ~source:transfer_source ~target:transfer_target () |> get_ok) in
  let transfer_one = transferred 1 and transfer_many = transferred 4 in
  check (geometry_bytes transfer_one = geometry_bytes transfer_many
      && Attribute.storage (Geometry.find_attribute ~owner:Attribute.Point
          "weight" transfer_one |> Option.get) = Attribute.Float [|0.;5.;100.|]
      && Attribute.storage (Geometry.find_attribute ~owner:Attribute.Point
          "id" transfer_one |> Option.get) = Attribute.Int [|5;5;0|])
    "point transfer exact match/interpolation/ties/miss/domain exactness";
  (match Attribute_ops.transfer_points ~names:["weight"] ~pattern:"*"
      ~source:transfer_source ~target:transfer_target () with
   | Error error when Error.code error = "invalid_transfer" -> ()
   | _ -> fail "point transfer accepted both exact names and a pattern");
  (match Attribute_ops.transfer_points ~pattern:"bad["
      ~source:transfer_source ~target:transfer_target () with
   | Error error when Error.code error = "invalid_transfer" -> ()
   | _ -> fail "point transfer accepted a malformed pattern");
  let source_second = Group.init ~owner:Group.Point ~name:"source_second" 2
      (fun point -> point = 1)
  and target_outer = Group.init ~owner:Group.Point ~name:"target_outer" 3
      (fun point -> point <> 1) in
  let restricted_transfer domains = Parallel.run ~domains (fun () ->
    Attribute_ops.transfer_points ~grain:1 ~max_distance:3.
      ~source_points:source_second ~target_points:target_outer
      ~source:transfer_source ~target:transfer_target () |> get_ok) in
  let restricted_one = restricted_transfer 1
  and restricted_many = restricted_transfer 4 in
  check (geometry_bytes restricted_one = geometry_bytes restricted_many
      && Attribute.storage (Geometry.find_attribute ~owner:Attribute.Point
          "weight" restricted_one |> Option.get)
        = Attribute.Float [|10.;100.;100.|])
    "point transfer source/target restrictions preserve unselected and missed values";
  let primitive_weight = Attribute.create_owned ~name:"primitive_weight"
      ~owner:Attribute.Primitive (Attribute.Float [|10.; 20.|]) |> get_ok
  and primitive_id = Attribute.create_owned ~name:"primitive_id"
      ~owner:Attribute.Primitive (Attribute.Int [|4; 8|]) |> get_ok in
  let primitive_source = triangle_geometry
      (* Exact centroids (2,1)/(1,2) make the target (1.5,1.5) a true tie. *)
      |> Transform_ops.transform (Rays_math.Mat4.scaling (Rays_math.Vec3.create 3. 3. 3.))
      |> Geometry.with_attribute primitive_weight |> get_ok
      |> Geometry.with_attribute primitive_id |> get_ok in
  let primitive_target_positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.; 3.; 1.5|] ~y:[|0.; 0.; 4.5|] ~z:[|0.; 0.; 0.|] in
  let primitive_target_topology = Tb.create ~point_count:3 () in
  Tb.add_triangle primitive_target_topology 0 1 2;
  let primitive_target_geometry = Geometry.create
      ~positions:primitive_target_positions
      ~topology:(Tb.freeze primitive_target_topology) () |> get_ok in
  let primitive_transferred domains = Parallel.run ~domains (fun () ->
      Attribute_ops.transfer_primitives ~grain:1
        ~mode:(Attribute_ops.Inverse_distance { neighbors = 2; power = 1. })
        ~source:primitive_source ~target:primitive_target_geometry () |> get_ok) in
  let primitive_one = primitive_transferred 1
  and primitive_many = primitive_transferred 4 in
  check (geometry_bytes primitive_one = geometry_bytes primitive_many)
    "primitive transfer complete geometry differs by domain count";
  (match Attribute.storage (Geometry.find_attribute ~owner:Attribute.Primitive
      "primitive_id" primitive_one |> Option.get) with
   | Attribute.Int [|value|] -> check (value = 4)
       (Printf.sprintf "primitive transfer discrete tie: got %d, expected 4" value)
   | _ -> fail "primitive transfer integer storage/cardinality");
  (match Attribute.storage (Geometry.find_attribute ~owner:Attribute.Primitive
      "primitive_weight" primitive_one |> Option.get) with
   | Attribute.Float [|value|] -> check (near value 15.)
       "primitive transfer barycenter interpolation"
   | _ -> fail "primitive transfer floating storage/cardinality");
  (match Attribute_ops.transfer_primitives ~source_primitives:source_second
      ~source:primitive_source ~target:primitive_target_geometry () with
   | Error error when Error.code error = "invalid_transfer" -> ()
   | _ -> fail "primitive transfer accepted a point-owned source group");
  let source_detail = Attribute.create_owned ~name:"asset"
      ~owner:Attribute.Detail (Attribute.Text [|"oak"|]) |> get_ok in
  let detail_source = Geometry.with_attribute source_detail primitive_source
      |> get_ok in
  let detail_target = Attribute_ops.transfer_detail ~source:detail_source
      ~target:primitive_target_geometry () |> get_ok in
  (match Geometry.find_attribute ~owner:Attribute.Detail "asset" detail_target with
   | Some attribute when Attribute.storage_id attribute
       = Attribute.storage_id source_detail ->
       (match Attribute.storage attribute with
        | Attribute.Text [|"oak"|] -> ()
        | _ -> fail "detail transfer payload")
   | _ -> fail "detail transfer did not structurally share source payload");
  let detail_p = Attribute.create_owned ~name:"P" ~owner:Attribute.Detail
      (Attribute.Int [|23|]) |> get_ok in
  let detail_p_source = Geometry.with_attribute detail_p detail_source |> get_ok in
  let detail_pattern_target = Attribute_ops.transfer_detail ~pattern:"^P"
      ~source:detail_p_source ~target:primitive_target_geometry () |> get_ok in
  if Geometry.find_attribute ~owner:Attribute.Detail "asset"
       detail_pattern_target = None
     || Geometry.find_attribute ~owner:Attribute.Detail "P"
          detail_pattern_target <> None then
    fail "detail transfer leading-exclusion pattern";
  (match Attribute_ops.transfer_detail ~names:["P"] ~source:detail_p_source
      ~target:primitive_target_geometry () with
   | Ok geometry ->
       (match Geometry.find_attribute ~owner:Attribute.Detail "P" geometry with
        | Some attribute ->
            (match Attribute.storage attribute with
             | Attribute.Int [|23|] -> ()
             | _ -> fail "detail-owned P transfer payload")
        | None -> fail "detail-owned P transfer missing")
   | Error _ -> fail "detail-owned ordinary P was rejected");
  let indexed_points = Array.init 257 (fun point ->
      let value = float_of_int point in
      (sin (value *. 0.73) *. 7., cos (value *. 1.17) *. 5.,
       sin (value *. 0.19) *. 3.)) in
  let indexed_geometry = Line_geometry.points indexed_points in
  (match Spatial_index.create ~grain:0 (Geometry.positions indexed_geometry) with
   | Error error when Error.code error = "invalid_parameter" -> ()
   | _ -> fail "spatial index accepted a non-positive parallel grain");
  let indexed domains = Parallel.run ~domains (fun () ->
    Spatial_index.create ~grain:7 (Geometry.positions indexed_geometry) |> get_ok) in
  let indexes = [indexed 1; indexed 4] in
  for query = 0 to 127 do
    let value = float_of_int query in
    let qx = sin (value *. 0.31) *. 6.
    and qy = cos (value *. 0.43) *. 4.
    and qz = sin (value *. 0.61) *. 2. in
    let expected = ref 0 and expected_distance = ref Float.infinity in
    Array.iteri (fun point (x, y, z) ->
      let dx = x -. qx and dy = y -. qy and dz = z -. qz in
      let distance = (dx *. dx) +. (dy *. dy) +. (dz *. dz) in
      if distance < !expected_distance
         || (distance = !expected_distance && point < !expected) then begin
        expected := point; expected_distance := distance
      end) indexed_points;
    List.iter (fun index ->
      let indices = [|-17|] and distances_squared = [|-17.|] and counts = [|0|] in
      Spatial_index.View.nearest_k_many_into ~grain:1 index
        ~queries:(Packed.Float3.View.of_owned_exn ~x:[|qx|] ~y:[|qy|] ~z:[|qz|])
        ~max_distance_squared:Float.infinity ~capacity:1
        ~indices ~distances_squared ~counts;
      check (counts = [|1|] && indices = [|!expected|]
          && distances_squared = [|!expected_distance|])
        "spatial index nearest/reference/domain exactness and output slice") indexes
  done;
  let surface_positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.; 2.; 0.|] ~y:[|0.; 0.; 2.|] ~z:[|0.; 0.; 0.|] in
  let surface_topology = Tb.create ~point_count:3 () in
  Tb.add_triangle surface_topology 0 1 2;
  let surface_weight = Attribute.create_owned ~name:"weight"
      ~owner:Attribute.Point (Attribute.Float [|0.; 2.; 4.|]) |> get_ok
  and surface_vertex = Attribute.create_owned ~name:"corner_value"
      ~owner:Attribute.Vertex (Attribute.Float [|10.; 20.; 30.|]) |> get_ok
  and surface_piece = Attribute.create_owned ~name:"piece"
      ~owner:Attribute.Primitive (Attribute.Int [|7|]) |> get_ok in
  let surface_source = Geometry.create ~positions:surface_positions
      ~topology:(Tb.freeze surface_topology)
      ~attributes:[surface_weight; surface_vertex; surface_piece] () |> get_ok in
  let vertex_owned_transfer domains = Parallel.run ~domains (fun () ->
      Attribute_ops.transfer_vertices ~grain:1 ~pattern:"corner*"
        ~source:surface_source ~target:triangle_geometry () |> get_ok) in
  let vertex_owned_one = vertex_owned_transfer 1
  and vertex_owned_many = vertex_owned_transfer 4 in
  let transferred_corner geometry = Geometry.find_attribute
      ~owner:Attribute.Vertex "corner_value" geometry |> Option.get
      |> Attribute.get (Attribute.key ~name:"corner_value"
          ~owner:Attribute.Vertex Attribute.float) |> Option.get in
  if not (near_array [|10.; 15.; 25.; 10.; 25.; 20.|]
      (transferred_corner vertex_owned_one))
     || transferred_corner vertex_owned_one
        <> transferred_corner vertex_owned_many then
    fail "vertex closest-surface transfer/domain determinism";
  let one_surface_vertex = Group.init ~owner:Group.Vertex ~name:"one" 3
      (fun vertex -> vertex = 0) in
  let any_corner_surface = Surface_index.create ~vertices:one_surface_vertex
      ~vertex_selection:Surface_index.Any_triangle_vertex surface_source
      |> get_ok in
  if Surface_index.triangle_count any_corner_surface <> 1 then
    fail "surface index any-corner vertex restriction";
  let all_corner_surface = Surface_index.create ~vertices:one_surface_vertex
      ~vertex_selection:Surface_index.All_triangle_vertices surface_source
      |> get_ok in
  if Surface_index.triangle_count all_corner_surface <> 0 then
    fail "surface index all-corners vertex restriction";
  let initial_surface_weight = Attribute.create_owned ~name:"sampled_weight"
      ~owner:Attribute.Point (Attribute.Float [|99.; 99.|]) |> get_ok
  and initial_corner = Attribute.create_owned ~name:"sampled_corner"
      ~owner:Attribute.Point (Attribute.Float [|88.; 88.|]) |> get_ok
  and initial_piece = Attribute.create_owned ~name:"sampled_piece"
      ~owner:Attribute.Point (Attribute.Int [|6; 6|]) |> get_ok
  and initial_distance = Attribute.create_owned ~name:"surface_distance"
      ~owner:Attribute.Point (Attribute.Float [|77.; 77.|]) |> get_ok in
  let duplicate_geometry = Geometry.create ~positions:surface_positions
      ~topology:(Tb.freeze (let builder =
        Tb.create ~point_count:3 () in
        Tb.add_triangle builder 0 1 2;
        Tb.add_triangle builder 0 1 2; builder)) () |> get_ok in
  let restricted_piece = Attribute.create_owned ~name:"piece"
      ~owner:Attribute.Primitive (Attribute.Int [|7; 9|]) |> get_ok in
  let restricted_source = Geometry.with_attribute restricted_piece
      duplicate_geometry |> get_ok
  and restricted_target = Line_geometry.points [|(0.5,0.5,1.); (0.5,0.5,1.)|] in
  let restricted_target = List.fold_left (fun geometry attribute ->
    Geometry.with_attribute attribute geometry |> get_ok) restricted_target
      [initial_surface_weight;initial_corner;initial_piece;initial_distance] in
  let restricted_target_bytes = geometry_bytes restricted_target in
  let target_first = Group.init ~owner:Group.Point ~name:"first" 2
      (fun point -> point = 0) in
  let surface_attributes = List.map (fun (source_owner, source_name, target_name) ->
    Attribute_ops.{source_owner;source_name;target_name}) Attribute.[
      Point,"weight","sampled_weight";
      Vertex,"corner_value","sampled_corner";
      Primitive,"piece","sampled_piece"] in
  let sampled domains = Parallel.run ~domains (fun () ->
    Attribute_ops.transfer_surface ~grain:1 ~target_elements:target_first
      ~distance_attribute:"surface_distance" ~attributes:surface_attributes
      ~source:surface_source ~target:restricted_target () |> get_ok) in
  let sampled_one = sampled 1 and sampled_many = sampled 4 in
  check (geometry_bytes sampled_one = geometry_bytes sampled_many
      && geometry_bytes restricted_target = restricted_target_bytes)
    "surface transfer selected targets/domain exactness/unchanged input";
  List.iter (fun (name, storage) ->
    check (Attribute.storage (Geometry.find_attribute ~owner:Attribute.Point
        name sampled_one |> Option.get) = storage)
      ("surface transfer interpolation/unselected value for " ^ name)) Attribute.[
      "sampled_weight",Float [|1.5;99.|];
      "sampled_corner",Float [|17.5;88.|];
      "sampled_piece",Int [|7;6|];
      "surface_distance",Float [|1.;77.|]];
  let source_second = Group.init ~owner:Group.Primitive ~name:"second" 2
      (fun primitive -> primitive = 1) in
  let restricted_sample domains = Parallel.run ~domains (fun () ->
    Attribute_ops.transfer_surface ~grain:1 ~source_primitives:source_second
      ~target_elements:target_first
      ~attributes:[Attribute_ops.{source_owner=Attribute.Primitive;
        source_name="piece";target_name="sampled_piece"}]
      ~source:restricted_source ~target:restricted_target () |> get_ok) in
  let restricted_one = restricted_sample 1
  and restricted_many = restricted_sample 4 in
  check (geometry_bytes restricted_one = geometry_bytes restricted_many
      && geometry_bytes restricted_target = restricted_target_bytes
      && Attribute.storage (Geometry.find_attribute ~owner:Attribute.Point
          "sampled_piece" restricted_one |> Option.get) = Attribute.Int [|9;6|]
      && Attribute.storage (Geometry.find_attribute ~owner:Attribute.Point
          "surface_distance" restricted_one |> Option.get)
        = Attribute.Float [|77.;77.|])
    "surface transfer source primitive restriction overrides coincident tie";
  let degenerate_positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.; 1.; 2.|] ~y:[|0.; 0.; 0.|] ~z:[|0.; 0.; 0.|] in
  let degenerate_topology = Tb.create ~point_count:3 () in
  Tb.add_triangle degenerate_topology 0 1 2;
  let degenerate_surface = Geometry.create ~positions:degenerate_positions
      ~topology:(Tb.freeze degenerate_topology) () |> get_ok in
  (match Surface_index.create degenerate_surface with
   | Error error when Error.code error = "invalid_surface" -> ()
   | _ -> fail "surface index accepted a degenerate triangle");
  let tiny = 1e-100 in
  let tiny_positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.; tiny; 0.|] ~y:[|0.; 0.; tiny|] ~z:[|0.; 0.; 0.|] in
  let tiny_topology = Tb.create ~point_count:3 () in
  Tb.add_triangle tiny_topology 0 1 2;
  let tiny_surface = Geometry.create ~positions:tiny_positions
      ~topology:(Tb.freeze tiny_topology) () |> get_ok in
  let tiny_index = Surface_index.create tiny_surface |> get_ok in
  if Surface_index.triangle_count tiny_index <> 1 then
    fail "surface index rejected an exact non-collinear subnormal-area triangle";
  let surface_cancel = Cancel.create () in
  Cancel.cancel surface_cancel;
  (match Surface_index.create ~cancel:surface_cancel surface_source with
   | Error error when Error.code error = "cancelled" -> ()
   | _ -> fail "cancelled surface index construction returned the wrong result");
  let quad_surface_weight = Attribute.create_owned ~name:"quad_weight"
      ~owner:Attribute.Point (Attribute.Float [|0.; 1.; 2.; 3.|]) |> get_ok
  and quad_surface_corner = Attribute.create_owned ~name:"quad_corner"
      ~owner:Attribute.Vertex (Attribute.Float [|10.; 20.; 30.; 40.|]) |> get_ok in
  let quad_surface = geometry
      |> Geometry.with_attribute quad_surface_weight |> get_ok
      |> Geometry.with_attribute quad_surface_corner |> get_ok in
  let quad_index = Surface_index.create quad_surface |> get_ok in
  if Surface_index.triangle_count quad_index <> 2 then
    fail "surface index did not triangulate a quad exactly";
  let quad_target = Line_geometry.points [|(0.2, 0.7, 1.); (0.8, 0.7, 1.)|] in
  let quad_transfer = Attribute_ops.transfer_surface ~grain:1
      ~attributes:Attribute_ops.[
        {source_owner = Attribute.Point; source_name = "quad_weight";
         target_name = "quad_weight"};
        {source_owner = Attribute.Vertex; source_name = "quad_corner";
         target_name = "quad_corner"}]
      ~source:quad_surface ~target:quad_target () |> get_ok in
  let surface_float name geometry = Geometry.find_attribute
      ~owner:Attribute.Point name geometry |> Option.get
      |> Attribute.get (Attribute.key ~name ~owner:Attribute.Point Attribute.float)
      |> Option.get in
  if not (near_array [|2.3; 1.9|] (surface_float "quad_weight" quad_transfer))
     || not (near_array [|33.; 29.|] (surface_float "quad_corner" quad_transfer))
  then fail "general-polygon point/vertex surface interpolation";
  let concave_positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.; 2.; 2.; 1.; 0.|] ~y:[|0.; 0.; 2.; 1.; 2.|]
      ~z:(Array.make 5 0.) in
  let concave_topology = Tb.create ~point_count:5 () in
  Tb.add_polygon concave_topology [|0; 1; 2; 3; 4|];
  let concave_geometry = Geometry.create ~positions:concave_positions
      ~topology:(Tb.freeze concave_topology) () |> get_ok in
  let concave_index = Surface_index.create concave_geometry |> get_ok in
  if Surface_index.triangle_count concave_index <> 3 then
    fail "surface index concave polygon triangle cardinality";
  (match Surface_index.raycast concave_index ~origin:(Rays_math.Vec3.create 0.4 1.4 0.75)
      ~direction:(Rays_math.Vec3.neg Rays_math.Vec3.unit_z) with
   | Ok (Some hit) when hit.primitive = 0 && near hit.distance 0.75 -> ()
   | _ -> fail "surface index missed a concave polygon interior");
  let bow_tie_positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.; 2.; 0.; 2.|] ~y:[|0.; 2.; 2.; 0.|]
      ~z:(Array.make 4 0.) in
  let bow_tie_topology = Tb.create ~point_count:4 () in
  Tb.add_polygon bow_tie_topology [|0; 1; 2; 3|];
  let bow_tie = Geometry.create ~positions:bow_tie_positions
      ~topology:(Tb.freeze bow_tie_topology) () |> get_ok in
  (match Surface_index.create bow_tie with
   | Error error when Error.code error = "invalid_surface" -> ()
   | _ -> fail "surface index accepted a self-intersecting polygon");
  let bad_positions = Packed.Float3.View.of_owned_exn
      ~x:[|Float.nan|] ~y:[|0.|] ~z:[|0.|] in
  (match Spatial_index.create bad_positions with
   | Error error when Error.code error = "invalid_position" -> ()
   | _ -> fail "spatial index accepted a non-finite point");
  let conflicting_id = Attribute.create_owned ~name:"id" ~owner:Attribute.Point
      (Attribute.Float (Array.make 3 0.)) |> get_ok in
  let conflicting_target = Geometry.with_attribute conflicting_id
      transfer_target |> get_ok in
  (match Attribute_ops.transfer_points ~names:["id"] ~source:transfer_source
      ~target:conflicting_target () with
   | Error error when Error.code error = "invalid_transfer" -> ()
   | _ -> fail "attribute transfer accepted incompatible target storage");
  (match Attribute_ops.transfer_points
      ~mode:(Attribute_ops.Inverse_distance { neighbors = 0; power = 1. })
      ~source:transfer_source ~target:transfer_target () with
   | Error error when Error.code error = "invalid_transfer" -> ()
   | _ -> fail "attribute transfer accepted invalid neighbor count");
  (transfer_source, transfer_target)
