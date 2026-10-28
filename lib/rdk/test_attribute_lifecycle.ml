open Rays
open Rdk
open Rdk_test_support

let get_ok = get

let owner_count geometry = function
  | Attribute.Point -> Geometry.point_count geometry
  | Attribute.Vertex -> Geometry.vertex_count geometry
  | Attribute.Primitive -> Geometry.primitive_count geometry
  | Attribute.Detail -> 1

let add_float owner name seed geometry =
  let count = owner_count geometry owner in
  let attribute = Attribute.create_owned ~name ~owner
      (Attribute.Float (Array.init count (fun index ->
        seed +. float_of_int index))) |> get_ok in
  Geometry.with_attribute attribute geometry |> get_ok

let add_storage owner name storage geometry =
  let attribute = Attribute.create_owned ~name ~owner storage |> get_ok in
  Geometry.with_attribute attribute geometry |> get_ok

let has owner name geometry =
  Geometry.find_attribute ~owner name geometry <> None

let attribute owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some value -> value
  | None -> fail ("missing attribute " ^ name)

let fixture () =
  Plane_generators.grid ~columns:1 ~rows:1 ~size:1. () |> get_rdk
  |> add_float Attribute.Point "keep_point" 1.
  |> add_float Attribute.Point "temporary_point" 2.
  |> add_float Attribute.Point "temporary_keep" 3.
  |> add_float Attribute.Point "target" 4.
  |> add_float Attribute.Vertex "keep_vertex" 5.
  |> add_float Attribute.Vertex "temporary_vertex" 6.
  |> add_float Attribute.Primitive "keep_primitive" 7.
  |> add_float Attribute.Primitive "temporary_primitive" 8.
  |> add_float Attribute.Detail "keep_detail" 9.
  |> add_float Attribute.Detail "temporary_detail" 10.

let names geometry =
  Geometry.attributes geometry
  |> List.map (fun value -> Attribute.owner value, Attribute.name value)

let run () =
  let applied = Attribute_pattern.compile "^bar score" |> get_ok in
  check (not (Attribute_pattern.apply ~selected:true applied "bar"))
    "pattern apply did not subtract an implicit selection";
  check (Attribute_pattern.apply ~selected:false applied "score")
    "pattern apply did not add to an implicit selection";

  let source = fixture () in
  let source_id = Geometry.data_id source in
  let no_rules = Attribute_ops.delete source |> get_rdk in
  check (Geometry.data_id no_rules = source_id)
    "empty Attribute Delete did not preserve geometry identity";
  let blank = Attribute_ops.delete ~point_pattern:"  " source |> get_rdk in
  check (Geometry.data_id blank = source_id)
    "blank Attribute Delete did not preserve geometry identity";
  let keep_nothing = Attribute_ops.delete ~delete_non_selected:true source
      |> get_rdk in
  check (Geometry.attributes keep_nothing = [])
    "empty Attribute Delete keep mode did not remove ordinary metadata";

  let survivor_before = attribute Attribute.Point "keep_point" source in
  let deleted = Attribute_ops.delete
      ~point_pattern:"temporary_* ^temporary_keep"
      ~vertex_pattern:"temporary_*"
      ~primitive_pattern:"temporary_*"
      ~detail_pattern:"temporary_*" source |> get_rdk in
  check (not (has Attribute.Point "temporary_point" deleted)
      && has Attribute.Point "temporary_keep" deleted
      && not (has Attribute.Vertex "temporary_vertex" deleted)
      && not (has Attribute.Primitive "temporary_primitive" deleted)
      && not (has Attribute.Detail "temporary_detail" deleted))
    "owner-specific Attribute Delete selection";
  let survivor_after = attribute Attribute.Point "keep_point" deleted in
  check (Attribute.data_id survivor_after = Attribute.data_id survivor_before
      && Attribute.storage_id survivor_after = Attribute.storage_id survivor_before)
    "Attribute Delete copied surviving metadata or payload";
  check (Packed.Float3.data_id (Geometry.positions source)
      = Packed.Float3.data_id (Geometry.positions deleted)
      && Topology.data_id (Geometry.topology source)
         = Topology.data_id (Geometry.topology deleted))
    "Attribute Delete copied positions or topology";

  let kept = Attribute_ops.delete ~delete_non_selected:true
      ~point_pattern:"keep_*"
      ~vertex_pattern:"keep_*"
      ~primitive_pattern:"keep_*"
      ~detail_pattern:"keep_*" source |> get_rdk in
  check (List.length (Geometry.attributes kept) = 4
      && has Attribute.Point "keep_point" kept
      && has Attribute.Vertex "keep_vertex" kept
      && has Attribute.Primitive "keep_primitive" kept
      && has Attribute.Detail "keep_detail" kept)
    "Attribute Delete keep-pattern mode";

  let reference = Line_geometry.points [|(0., 0., 0.)|]
      |> add_float Attribute.Point "bar" 1.
      |> add_float Attribute.Point "foo" 2. in
  let reference_source = Line_geometry.points [|(0., 0., 0.)|]
      |> add_float Attribute.Point "bar" 1.
      |> add_float Attribute.Point "foo" 2.
      |> add_float Attribute.Point "score" 3.
      |> add_float Attribute.Point "weight" 4. in
  let reference_deleted = Attribute_ops.delete ~reference
      ~point_pattern:"^bar score" reference_source |> get_rdk in
  check (has Attribute.Point "bar" reference_deleted
      && not (has Attribute.Point "foo" reference_deleted)
      && not (has Attribute.Point "score" reference_deleted)
      && has Attribute.Point "weight" reference_deleted)
    "Attribute Delete reference prepend/exclusion semantics";
  let reference_kept = Attribute_ops.delete ~reference
      ~delete_non_selected:true ~point_pattern:"^bar" reference_source
      |> get_rdk in
  check (not (has Attribute.Point "bar" reference_kept)
      && has Attribute.Point "foo" reference_kept
      && not (has Attribute.Point "score" reference_kept))
    "Attribute Delete reference keep semantics";
  let exclusion = Attribute_ops.delete ~point_pattern:"^keep_point" source
      |> get_rdk in
  check (has Attribute.Point "keep_point" exclusion
      && not (has Attribute.Point "target" exclusion)
      && has Attribute.Vertex "keep_vertex" exclusion)
    "Attribute Delete leading exclusion semantics or owner isolation";
  check (Geometry.point_count deleted = Geometry.point_count source)
    "Attribute Delete changed canonical P/cardinality";

  (match Attribute_ops.delete ~point_pattern:"[bad" source with
   | Error error -> check (Error.code error = "invalid_attribute")
       "malformed Attribute Delete pattern diagnostic"
   | Ok _ -> fail "Attribute Delete accepted malformed pattern");
  let delete_cancel = Cancel.create () in
  Cancel.cancel delete_cancel;
  (match Attribute_ops.delete ~cancel:delete_cancel ~point_pattern:"*" source with
   | Error error -> check (Error.code error = "cancelled")
       "Attribute Delete cancellation diagnostic"
   | Ok _ -> fail "cancelled Attribute Delete published geometry");

  let temporary_before = attribute Attribute.Point "temporary_point" source
  and untouched_before = attribute Attribute.Point "keep_point" source in
  let renamed = Attribute_ops.rename ~rules:[{
      rename_attribute_owner = Some Attribute.Point;
      rename_attribute_pattern = "temporary_*";
      rename_attribute_replacement = "stage_*";
      rename_attribute_conflict = Attribute_ops.Attribute_rename_error;
    }; {
      rename_attribute_owner = None;
      rename_attribute_pattern = "stage_*";
      rename_attribute_replacement = "final_*";
      rename_attribute_conflict = Attribute_ops.Attribute_rename_error;
    }] source |> get_rdk in
  check (has Attribute.Point "final_point" renamed
      && has Attribute.Point "final_keep" renamed
      && has Attribute.Vertex "temporary_vertex" renamed
      && not (has Attribute.Point "temporary_point" renamed))
    "ordered Attribute Rename rules or owner restriction";
  let temporary_after = attribute Attribute.Point "final_point" renamed
  and untouched_after = attribute Attribute.Point "keep_point" renamed in
  check (Attribute.data_id temporary_after <> Attribute.data_id temporary_before
      && Attribute.storage_id temporary_after
         = Attribute.storage_id temporary_before)
    "Attribute Rename did not share the renamed payload";
  check (Attribute.data_id untouched_after = Attribute.data_id untouched_before)
    "Attribute Rename copied untouched metadata";

  let rename_none = Attribute_ops.rename ~rules:[] source |> get_rdk
  and rename_missing = Attribute_ops.rename ~rules:[{
      rename_attribute_owner = None;
      rename_attribute_pattern = "missing_*";
      rename_attribute_replacement = "still_missing_*";
      rename_attribute_conflict = Attribute_ops.Attribute_rename_error;
    }] source |> get_rdk in
  let rename_same = Attribute_ops.rename ~rules:[{
      rename_attribute_owner = Some Attribute.Point;
      rename_attribute_pattern = "keep_point";
      rename_attribute_replacement = "keep_point";
      rename_attribute_conflict = Attribute_ops.Attribute_rename_error;
    }] source |> get_rdk in
  check (Geometry.data_id rename_none = source_id
      && Geometry.data_id rename_missing = source_id
      && Geometry.data_id rename_same = source_id)
    "no-op Attribute Rename did not preserve geometry identity";

  let skipped = Attribute_ops.rename ~rules:[{
      rename_attribute_owner = Some Attribute.Point;
      rename_attribute_pattern = "temporary_point";
      rename_attribute_replacement = "target";
      rename_attribute_conflict = Attribute_ops.Attribute_rename_skip;
    }] source |> get_rdk in
  check (Geometry.data_id skipped = source_id
      && has Attribute.Point "temporary_point" skipped
      && has Attribute.Point "target" skipped)
    "Attribute Rename skip conflict";
  (match Attribute_ops.rename ~rules:[{
      rename_attribute_owner = Some Attribute.Point;
      rename_attribute_pattern = "temporary_point";
      rename_attribute_replacement = "target";
      rename_attribute_conflict = Attribute_ops.Attribute_rename_error;
    }] source with
   | Error error -> check (Error.code error = "invalid_attribute"
       && Geometry.data_id source = source_id)
       "Attribute Rename error conflict was not atomic"
   | Ok _ -> fail "Attribute Rename accepted an error conflict");
  let overwritten = Attribute_ops.rename ~rules:[{
      rename_attribute_owner = Some Attribute.Point;
      rename_attribute_pattern = "temporary_point";
      rename_attribute_replacement = "target";
      rename_attribute_conflict = Attribute_ops.Attribute_rename_overwrite;
    }] source |> get_rdk in
  check (not (has Attribute.Point "temporary_point" overwritten)
      && Attribute.storage_id (attribute Attribute.Point "target" overwritten)
         = Attribute.storage_id temporary_before
      && List.length (Geometry.attributes overwritten)
         = List.length (Geometry.attributes source) - 1)
    "Attribute Rename overwrite conflict";
  (match Attribute_ops.rename ~rules:[{
      rename_attribute_owner = Some Attribute.Point;
      rename_attribute_pattern = "temporary_point";
      rename_attribute_replacement = "P";
      rename_attribute_conflict = Attribute_ops.Attribute_rename_overwrite;
    }] source with
   | Error error -> check (Error.code error = "invalid_attribute")
       "Attribute Rename canonical P diagnostic"
   | Ok _ -> fail "Attribute Rename replaced canonical P");
  (match Attribute_ops.rename ~rules:[{
      rename_attribute_owner = None;
      rename_attribute_pattern = "temporary_*";
      rename_attribute_replacement = "fixed";
      rename_attribute_conflict = Attribute_ops.Attribute_rename_error;
    }] source with
   | Error error -> check (Error.code error = "invalid_attribute")
       "Attribute Rename malformed rewrite diagnostic"
   | Ok _ -> fail "Attribute Rename accepted mismatched wildcards");
  let rename_cancel = Cancel.create () in
  Cancel.cancel rename_cancel;
  (match Attribute_ops.rename ~cancel:rename_cancel ~rules:[{
      rename_attribute_owner = None;
      rename_attribute_pattern = "temporary_*";
      rename_attribute_replacement = "cancelled_*";
      rename_attribute_conflict = Attribute_ops.Attribute_rename_error;
    }] source with
   | Error error -> check (Error.code error = "cancelled")
       "Attribute Rename cancellation diagnostic"
   | Ok _ -> fail "cancelled Attribute Rename published geometry");

  let swap_source = Line_geometry.points [|(1., 2., 3.); (4., 5., 6.)|]
      |> add_float Attribute.Point "left_weight" 10.
      |> add_float Attribute.Point "right_weight" 20.
      |> add_float Attribute.Vertex "left_corner" 30.
      |> add_float Attribute.Primitive "left_face" 40.
      |> add_storage Attribute.Detail "left_float" (Attribute.Float [|1.25|])
      |> add_storage Attribute.Detail "left_int" (Attribute.Int [|7|])
      |> add_storage Attribute.Detail "left_int_array"
           (Attribute.Int_array (Packed.Int_array.create_owned
              ~offsets:[|0; 2|] ~values:[|2; 5|] |> get_ok))
      |> add_storage Attribute.Detail "left_float_array"
           (Attribute.Float_array (Packed.Float_array.create_owned
              ~offsets:[|0; 2|] ~values:[|2.5; 5.5|] |> get_ok))
      |> add_storage Attribute.Detail "left_float2"
           (Attribute.Float2 (Packed.Float2.of_owned
              ~x:[|2.|] ~y:[|3.|] |> get_ok))
      |> add_storage Attribute.Detail "left_float3"
           (Attribute.Float3 (Packed.Float3.of_owned
              ~x:[|2.|] ~y:[|3.|] ~z:[|4.|] |> get_ok))
      |> add_storage Attribute.Detail "left_float4"
           (Attribute.Float4 (Packed.Float4.of_owned
              ~x:[|2.|] ~y:[|3.|] ~z:[|4.|] ~w:[|5.|] |> get_ok))
      |> add_storage Attribute.Detail "left_text" (Attribute.Text [|"payload"|])
  in
  let left_weight = attribute Attribute.Point "left_weight" swap_source
  and right_weight = attribute Attribute.Point "right_weight" swap_source in
  let swapped = Attribute_ops.swap ~rules:[{
      swap_attribute_owner = Attribute.Point;
      swap_attribute_source = "left_*";
      swap_attribute_destination = "right_*";
      swap_attribute_method = Attribute_ops.Attribute_swap;
    }] swap_source |> get_rdk in
  check (Attribute.storage_id (attribute Attribute.Point "left_weight" swapped)
      = Attribute.storage_id right_weight
      && Attribute.storage_id (attribute Attribute.Point "right_weight" swapped)
         = Attribute.storage_id left_weight)
    "Attribute Swap did not exchange packed payloads";
  check (Geometry.point_count swapped = Geometry.point_count swap_source
      && Geometry.vertex_count swapped = Geometry.vertex_count swap_source
      && Geometry.primitive_count swapped = Geometry.primitive_count swap_source)
    "Attribute Swap changed geometry cardinality";

  let copied = Attribute_ops.swap ~rules:[
      { swap_attribute_owner = Attribute.Vertex;
        swap_attribute_source = "left_*";
        swap_attribute_destination = "copied_*";
        swap_attribute_method = Attribute_ops.Attribute_copy };
      { swap_attribute_owner = Attribute.Primitive;
        swap_attribute_source = "left_*";
        swap_attribute_destination = "copied_*";
        swap_attribute_method = Attribute_ops.Attribute_copy };
      { swap_attribute_owner = Attribute.Detail;
        swap_attribute_source = "left_*";
        swap_attribute_destination = "copied_*";
        swap_attribute_method = Attribute_ops.Attribute_copy }]
      swap_source |> get_rdk in
  List.iter (fun suffix ->
    let source = attribute Attribute.Detail ("left_" ^ suffix) copied
    and destination = attribute Attribute.Detail ("copied_" ^ suffix) copied in
    check (Attribute.storage_id source = Attribute.storage_id destination)
      ("Attribute Copy duplicated " ^ suffix ^ " storage"))
    ["float"; "int"; "int_array"; "float_array"; "float2"; "float3";
     "float4"; "text"];
  check (has Attribute.Vertex "left_corner" copied
      && has Attribute.Vertex "copied_corner" copied
      && has Attribute.Primitive "left_face" copied
      && has Attribute.Primitive "copied_face" copied)
    "Attribute Copy owner coverage";

  let moved = Attribute_ops.swap ~rules:[{
      swap_attribute_owner = Attribute.Point;
      swap_attribute_source = "left_weight";
      swap_attribute_destination = "right_weight";
      swap_attribute_method = Attribute_ops.Attribute_move;
    }] swap_source |> get_rdk in
  check (not (has Attribute.Point "left_weight" moved)
      && Attribute.storage_id (attribute Attribute.Point "right_weight" moved)
         = Attribute.storage_id left_weight
      && List.length (Geometry.attributes moved)
         = List.length (Geometry.attributes swap_source) - 1)
    "Attribute Move overwrite behavior";

  let swap_missing_destination = Attribute_ops.swap ~rules:[{
      swap_attribute_owner = Attribute.Point;
      swap_attribute_source = "left_weight";
      swap_attribute_destination = "new_weight";
      swap_attribute_method = Attribute_ops.Attribute_swap;
    }] swap_source |> get_rdk in
  check (has Attribute.Point "left_weight" swap_missing_destination
      && Attribute.storage_id
           (attribute Attribute.Point "new_weight" swap_missing_destination)
         = Attribute.storage_id left_weight)
    "Attribute Swap missing destination did not copy source";
  let swap_missing_source = Attribute_ops.swap ~rules:[{
      swap_attribute_owner = Attribute.Point;
      swap_attribute_source = "new_weight";
      swap_attribute_destination = "left_weight";
      swap_attribute_method = Attribute_ops.Attribute_swap;
    }] swap_source |> get_rdk in
  check (has Attribute.Point "left_weight" swap_missing_source
      && Attribute.storage_id
           (attribute Attribute.Point "new_weight" swap_missing_source)
         = Attribute.storage_id left_weight)
    "Attribute Swap missing source did not copy destination";

  let position_copy = Attribute_ops.swap ~rules:[{
      swap_attribute_owner = Attribute.Point;
      swap_attribute_source = "P";
      swap_attribute_destination = "rest";
      swap_attribute_method = Attribute_ops.Attribute_copy;
    }] swap_source |> get_rdk in
  let rest = attribute Attribute.Point "rest" position_copy in
  (match Attribute.View.storage rest with
   | Attribute.Float3 values ->
       check (Packed.Float3.data_id values
           = Packed.Float3.data_id (Geometry.positions swap_source))
         "Attribute Copy P did not share packed positions"
   | _ -> fail "Attribute Copy P emitted non-float3 storage");
  let alternate = Packed.Float3.of_owned ~x:[|9.; 8.|] ~y:[|7.; 6.|]
      ~z:[|5.; 4.|] |> get_ok in
  let with_rest = swap_source
      |> add_storage Attribute.Point "rest" (Attribute.Float3 alternate) in
  let position_swapped = Attribute_ops.swap ~rules:[{
      swap_attribute_owner = Attribute.Point;
      swap_attribute_source = "P";
      swap_attribute_destination = "rest";
      swap_attribute_method = Attribute_ops.Attribute_swap;
    }] with_rest |> get_rdk in
  check (Packed.Float3.get (Geometry.positions position_swapped) 0 = (9., 7., 5.))
    "Attribute Swap did not install rest into P";
  (match Attribute.View.storage
      (attribute Attribute.Point "rest" position_swapped) with
   | Attribute.Float3 values -> check (Packed.Float3.get values 0 = (1., 2., 3.))
       "Attribute Swap did not install old P into rest"
   | _ -> fail "Attribute Swap P/rest storage kind");
  let position_moved = Attribute_ops.swap ~rules:[{
      swap_attribute_owner = Attribute.Point;
      swap_attribute_source = "rest";
      swap_attribute_destination = "P";
      swap_attribute_method = Attribute_ops.Attribute_move;
    }] with_rest |> get_rdk in
  check (not (has Attribute.Point "rest" position_moved)
      && Packed.Float3.get (Geometry.positions position_moved) 1 = (8., 6., 4.))
    "Attribute Move into P did not consume float3 source";
  let p_move_is_copy = Attribute_ops.swap ~rules:[{
      swap_attribute_owner = Attribute.Point;
      swap_attribute_source = "P";
      swap_attribute_destination = "rest";
      swap_attribute_method = Attribute_ops.Attribute_move;
    }] swap_source |> get_rdk in
  check (has Attribute.Point "rest" p_move_is_copy
      && Geometry.point_count p_move_is_copy = 2)
    "moving canonical P was not converted to Copy";

  let ordered = Attribute_ops.swap ~rules:[
      { swap_attribute_owner = Attribute.Point;
        swap_attribute_source = "left_weight";
        swap_attribute_destination = "stage_weight";
        swap_attribute_method = Attribute_ops.Attribute_move };
      { swap_attribute_owner = Attribute.Point;
        swap_attribute_source = "stage_*";
        swap_attribute_destination = "final_*";
        swap_attribute_method = Attribute_ops.Attribute_copy }]
      swap_source |> get_rdk in
  check (not (has Attribute.Point "left_weight" ordered)
      && has Attribute.Point "stage_weight" ordered
      && has Attribute.Point "final_weight" ordered)
    "ordered Attribute Swap rules did not observe earlier results";

  let swap_none = Attribute_ops.swap ~rules:[] swap_source |> get_rdk
  and swap_missing = Attribute_ops.swap ~rules:[{
      swap_attribute_owner = Attribute.Detail;
      swap_attribute_source = "absent";
      swap_attribute_destination = "also_absent";
      swap_attribute_method = Attribute_ops.Attribute_copy;
    }] swap_source |> get_rdk in
  check (Geometry.data_id swap_none = Geometry.data_id swap_source
      && Geometry.data_id swap_missing = Geometry.data_id swap_source)
    "no-op Attribute Swap did not preserve geometry identity";
  (match Attribute_ops.swap ~rules:[{
      swap_attribute_owner = Attribute.Point;
      swap_attribute_source = "left_*";
      swap_attribute_destination = "fixed";
      swap_attribute_method = Attribute_ops.Attribute_copy;
    }] swap_source with
   | Error error -> check (Error.code error = "invalid_attribute")
       "Attribute Swap malformed rewrite diagnostic"
   | Ok _ -> fail "Attribute Swap accepted mismatched wildcards");
  (match Attribute_ops.swap ~rules:[
      { swap_attribute_owner = Attribute.Point;
        swap_attribute_source = "left_weight";
        swap_attribute_destination = "temporary_copy";
        swap_attribute_method = Attribute_ops.Attribute_copy };
      { swap_attribute_owner = Attribute.Point;
        swap_attribute_source = "left_weight";
        swap_attribute_destination = "P";
        swap_attribute_method = Attribute_ops.Attribute_copy }]
      swap_source with
   | Error error -> check (Error.code error = "invalid_attribute"
         && not (has Attribute.Point "temporary_copy" swap_source))
       "Attribute Swap P type error was not atomic"
   | Ok _ -> fail "Attribute Swap installed scalar storage as P");
  let swap_cancel = Cancel.create () in
  Cancel.cancel swap_cancel;
  (match Attribute_ops.swap ~cancel:swap_cancel ~rules:[{
      swap_attribute_owner = Attribute.Detail;
      swap_attribute_source = "left_*";
      swap_attribute_destination = "cancelled_*";
      swap_attribute_method = Attribute_ops.Attribute_copy;
    }] swap_source with
   | Error error -> check (Error.code error = "cancelled")
       "Attribute Swap cancellation diagnostic"
   | Ok _ -> fail "cancelled Attribute Swap published geometry");

  let dense_base = Line_geometry.points [|(0., 0., 0.)|] in
  let dense_attributes = Array.init 10_000 (fun index ->
      Attribute.create_owned
        ~name:(Printf.sprintf "%s_%05d"
          (if index land 1 = 0 then "temporary" else "keep") index)
        ~owner:Attribute.Detail (Attribute.Float [|float_of_int index|])
      |> get_ok) in
  let dense = Geometry.create ~positions:(Geometry.positions dense_base)
      ~topology:(Geometry.topology dense_base)
      ~attributes:(Array.to_list dense_attributes) () |> get_ok in
  let run domains = Parallel.run ~domains (fun () ->
      dense
      |> Attribute_ops.rename ~rules:[{
          rename_attribute_owner = Some Attribute.Detail;
          rename_attribute_pattern = "temporary_*";
          rename_attribute_replacement = "final_*";
          rename_attribute_conflict = Attribute_ops.Attribute_rename_error;
        }] |> get_rdk
      |> Attribute_ops.delete ~detail_pattern:"keep_*" |> get_rdk) in
  let one = run 1 and many = run 4 in
  check (names one = names many
      && List.length (Geometry.attributes one) = 5_000)
    "Attribute lifecycle scale cardinality or domain exactness";
  List.iter (fun value ->
    let original_name = "temporary_" ^ String.sub (Attribute.name value) 6 5 in
    let original = attribute Attribute.Detail original_name dense in
    check (Attribute.storage_id value = Attribute.storage_id original)
      "dense Attribute Rename copied packed payload")
    (Geometry.attributes one);
  let run_swap domains = Parallel.run ~domains (fun () ->
      Attribute_ops.swap ~rules:[{
        swap_attribute_owner = Attribute.Detail;
        swap_attribute_source = "temporary_*";
        swap_attribute_destination = "copy_*";
        swap_attribute_method = Attribute_ops.Attribute_copy;
      }] dense |> get_rdk) in
  let swap_one = run_swap 1 and swap_many = run_swap 4 in
  check (names swap_one = names swap_many
      && List.length (Geometry.attributes swap_one) = 15_000)
    "Attribute Swap scale cardinality or domain exactness";
  List.iter (fun value ->
    if String.starts_with ~prefix:"copy_" (Attribute.name value) then
      let source = "temporary_" ^ String.sub (Attribute.name value) 5 5 in
      check (Attribute.storage_id value
          = Attribute.storage_id (attribute Attribute.Detail source dense))
        "dense Attribute Copy duplicated packed payload")
    (Geometry.attributes swap_one);
  print_endline "attribute lifecycle tests passed"
