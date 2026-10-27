open Rays
open Rdk
open Rdk_test_support

let get_group_ok = function
  | Ok value -> value
  | Error message -> fail message

let point_cloud count =
  Line_geometry.points (Array.init count (fun point -> float_of_int point, 0., 0.))

let mesh primitive_points point_count =
  let positions = Packed.Float3.Builder.create point_count in
  for point = 0 to point_count - 1 do
    Packed.Float3.Builder.set positions point (float_of_int point) 0. 0.
  done;
  let topology = Rdk_test_support.Tb.create ~point_count () in
  Array.iter (Rdk_test_support.Tb.add_polygon topology) primitive_points;
  Geometry.create ~positions:(Packed.Float3.Builder.freeze positions)
    ~topology:(Rdk_test_support.Tb.freeze topology) ()
  |> function Ok geometry -> geometry | Error message -> fail message

let two_quads () =
  mesh [|[|0; 1; 4; 3|]; [|1; 2; 5; 4|]|] 6

let owner_count geometry = function
  | Group.Point -> Geometry.point_count geometry
  | Group.Vertex -> Geometry.vertex_count geometry
  | Group.Primitive -> Geometry.primitive_count geometry

let with_group owner name predicate geometry =
  Geometry.with_group
    (Group.init ~grain:1 ~owner ~name (owner_count geometry owner) predicate)
    geometry
  |> function Ok geometry -> geometry | Error message -> fail message

let with_ordered_group owner name elements geometry =
  let group = Group.ordered ~owner ~name ~length:(owner_count geometry owner)
      elements |> get_group_ok in
  Geometry.with_group group geometry
  |> function Ok geometry -> geometry | Error message -> fail message

let with_edge_group name predicate geometry =
  let topology = Geometry.topology geometry in
  let index = Topology_index.create topology in
  Geometry.with_edge_group
    (Edge_group.init ~grain:1 ~topology ~index ~name predicate) geometry
  |> function Ok geometry -> geometry | Error message -> fail message

let with_int owner name values geometry =
  let attribute = Attribute.create_owned ~owner ~name (Attribute.Int values)
    |> function Ok value -> value | Error message -> fail message in
  Geometry.with_attribute attribute geometry
  |> function Ok geometry -> geometry | Error message -> fail message

let with_text owner name values geometry =
  let attribute = Attribute.create_owned ~owner ~name (Attribute.Text values)
    |> function Ok value -> value | Error message -> fail message in
  Geometry.with_attribute attribute geometry
  |> function Ok geometry -> geometry | Error message -> fail message

let ordinary owner name geometry =
  match Geometry.find_group ~owner name geometry with
  | Some group -> group
  | None -> fail ("missing group " ^ name)

let edge name geometry =
  match Geometry.find_edge_group name geometry with
  | Some group -> group
  | None -> fail ("missing edge group " ^ name)

let group_members group =
  let members = ref [] in
  Group.iter (fun element -> members := element :: !members) group;
  List.rev !members

let ordered_group_members group =
  let members = ref [] in
  Group.iter_ordered (fun element -> members := element :: !members) group;
  List.rev !members

let edge_members group =
  let members = ref [] in
  Edge_group.iter (fun element -> members := element :: !members) group;
  List.rev !members

let expect_members expected group message =
  check (group_members group = expected) message

let same_group left right =
  Group.owner left = Group.owner right && Group.length left = Group.length right
  && group_members left = group_members right
  && Group.ordered_elements left = Group.ordered_elements right

let test_ordered_group_core () =
  let left = Group.ordered ~owner:Group.Point ~name:"left" ~length:6
      [|3; 1; 4|] |> get_group_ok in
  let right = Group.ordered ~owner:Group.Point ~name:"right" ~length:6
      [|4; 2; 1|] |> get_group_ok in
  check (group_members left = [1; 3; 4])
    "ordered group preserves sorted legacy iteration";
  check (ordered_group_members left = [3; 1; 4])
    "ordered group preserves explicit traversal";
  check (Group.is_ordered left) "ordered group reports its order plane";
  let exposed = Option.get (Group.ordered_elements left) in
  exposed.(0) <- 0;
  check (ordered_group_members left = [3; 1; 4])
    "ordered group returns a defensive order copy";
  check (Group.payload_bytes left
      = ((Group.length left + 7) / 8)
        + (3 * (Sys.word_size / 8)))
    "ordered group accounts for order payload storage";
  let renamed = Group.with_name "renamed" left in
  check (ordered_group_members renamed = [3; 1; 4]
      && Group.data_id renamed <> Group.data_id left)
    "group rename preserves order with a fresh payload identity";
  let union = Group.union left right |> get_group_ok in
  check (ordered_group_members union = [3; 1; 4; 2])
    "ordered union follows left order then unseen right members";
  let intersection = Group.intersection left right |> get_group_ok in
  check (ordered_group_members intersection = [1; 4])
    "ordered intersection follows the left order";
  let difference = Group.difference left right |> get_group_ok in
  check (ordered_group_members difference = [3])
    "ordered difference preserves surviving left order";
  let xor = Group.symmetric_difference left right |> get_group_ok in
  check (ordered_group_members xor = [3; 2])
    "ordered symmetric difference preserves deterministic operand order";
  let complement = Group.complement left in
  check (not (Group.is_ordered complement)
      && group_members complement = [0; 2; 5])
    "group complement drops an order that cannot describe new members";
  (match Group.ordered ~owner:Group.Point ~name:"bad" ~length:4 [|1; 1|] with
   | Error _ -> ()
   | Ok _ -> fail "ordered group accepted duplicate elements");
  (match Group.ordered ~owner:Group.Point ~name:"bad" ~length:4 [|4|] with
   | Error _ -> ()
   | Ok _ -> fail "ordered group accepted an out-of-range element");
  (match Group.ordered ~owner:Group.Point ~name:" " ~length:4 [||] with
   | Error _ -> ()
   | Ok _ -> fail "ordered group accepted an empty name")

let test_ordered_group_topology_remap () =
  let source = point_cloud 6
      |> with_ordered_group Group.Point "path" [|4; 1; 5|] in
  let sorted = Ordering.sort ~grain:1 ~owner:Ordering.Points ~key:Ordering.Reverse source
      |> get_ok in
  check (ordered_group_members (ordinary Group.Point "path" sorted)
      = [1; 4; 0])
    "point reorder remaps explicit group order by element ancestry";
  let remove = Group.init ~grain:1 ~owner:Group.Point ~name:"remove" 6
      (fun point -> point = 1) in
  let deleted = Deletion.delete ~grain:1 remove source |> get_ok in
  check (ordered_group_members (ordinary Group.Point "path" deleted)
      = [3; 4])
    "point deletion filters and remaps explicit group order";
  let duplicated = Instance_copy.duplicate ~grain:1 ~copies:1 source |> get_ok in
  check (ordered_group_members (ordinary Group.Point "path" duplicated)
      = [4; 1; 5; 10; 7; 11])
    "duplicate preserves explicit order in copy-major order";
  let merged = Mesh_merge.run ~grain:1 [source; source] |> get_ok in
  check (ordered_group_members (ordinary Group.Point "path" merged)
      = [4; 1; 5; 10; 7; 11])
    "merge concatenates explicit group order by input";
  let fuse_source = Line_geometry.points
      [|(0., 0., 0.); (0., 0., 0.); (1., 0., 0.); (2., 0., 0.)|]
      |> with_ordered_group Group.Point "path" [|1; 0; 3|] in
  let fused = Fuse_grid.fuse ~grain:1 ~tolerance:0. fuse_source |> get_ok in
  check (ordered_group_members (ordinary Group.Point "path" fused) = [0; 2])
    "fuse collapses duplicate ordered members at first sequence occurrence"

let test_complement_and_combine () =
  let base = point_cloud 10
      |> with_group Group.Point "a1" (fun point -> point = 0 || point = 2)
      |> with_group Group.Point "a2" (fun point -> point = 1 || point = 2)
      |> with_group Group.Point "mask" (fun point -> point = 2 || point = 3) in
  let complemented = Group.complement (ordinary Group.Point "a1" base) in
  expect_members [1; 3; 4; 5; 6; 7; 8; 9] complemented
    "Group complement clears only live membership bits";
  let combined = Group_ops.combine ~grain:1 ~owner:Group_ops.Group_points
      ~name:"result" ~base:{ pattern = "a*"; inverted = false }
      ~steps:[{ operation = Group_ops.Group_intersection;
        operand = { pattern = "mask"; inverted = false } }] base |> get_ok in
  expect_members [2] (ordinary Group.Point "result" combined)
    "Group Combine unions pattern matches before intersection";
  let all_but = Group_ops.combine ~grain:1 ~owner:Group_ops.Group_points
      ~name:"outside" ~base:{ pattern = "a1"; inverted = true }
      ~steps:[{ operation = Group_ops.Group_subtract;
        operand = { pattern = "mask"; inverted = false } }] base |> get_ok in
  expect_members [1; 4; 5; 6; 7; 8; 9]
    (ordinary Group.Point "outside" all_but)
    "Group Combine complemented base and subtraction";
  let unmatched = Group_ops.combine ~owner:Group_ops.Group_points ~name:"empty"
      ~base:{ pattern = "missing*"; inverted = false } ~steps:[] base |> get_ok in
  check (Group.cardinality (ordinary Group.Point "empty" unmatched) = 0)
    "Group Combine unmatched pattern is empty";
  let mesh = two_quads () |> with_edge_group "first_edge" (fun edge -> edge = 0)
      |> with_edge_group "second_edge" (fun edge -> edge = 1) in
  let edge_combined = Group_ops.combine ~grain:1 ~owner:Group_ops.Group_edges
      ~name:"both" ~base:{ pattern = "*_edge"; inverted = false }
      ~steps:[] mesh |> get_ok in
  check (Edge_group.cardinality (edge "both" edge_combined) = 2)
    "Group Combine native-edge parity"

let test_range () =
  let base = point_cloud 10 in
  let filtered = Group_ops.range ~grain:1 ~owner:Group_ops.Group_points ~name:"filtered"
      ~filter:{ select = 2; of_ = 3; offset = 1 }
      (Group_ops.Range_start_end { start = 2; end_ = 7 }) base |> get_ok in
  expect_members [3; 4; 6; 7] (ordinary Group.Point "filtered" filtered)
    "Group Range periodic filter and offset";
  let partition = Group_ops.range ~grain:1 ~owner:Group_ops.Group_points
      ~name:"partition" (Group_ops.Range_partition { partition = 1; partitions = 3 })
      base |> get_ok in
  expect_members [4; 5; 6] (ordinary Group.Point "partition" partition)
    "Group Range balanced equal partitions";
  let masked = base
      |> with_group Group.Point "even" (fun point -> point land 1 = 0)
      |> Group_ops.range ~grain:1 ~base:"even" ~invert:true
        ~owner:Group_ops.Group_points ~name:"masked"
        (Group_ops.Range_start_length { start = 2; length = 4 }) |> get_ok in
  expect_members [0; 6; 8] (ordinary Group.Point "masked" masked)
    "Group Range inversion remains constrained to the base group";
  let unioned = base
      |> Group_ops.range ~grain:1 ~owner:Group_ops.Group_points ~name:"merged"
        (Group_ops.Range_start_end { start = 0; end_ = 2 }) |> get_ok
      |> Group_ops.range ~grain:1 ~merge:Group_ops.Group_union
        ~owner:Group_ops.Group_points ~name:"merged"
        (Group_ops.Range_from_ends { start = 7; end_offset = 0 }) |> get_ok in
  expect_members [0; 1; 2; 7; 8; 9] (ordinary Group.Point "merged" unioned)
    "Group Range merge union";
  let empty_extreme = Group_ops.range ~owner:Group_ops.Group_points ~name:"extreme"
      (Group_ops.Range_start_length { start = min_int; length = max_int }) base
      |> get_ok in
  check (Group.cardinality (ordinary Group.Point "extreme" empty_extreme) = 0)
    "Group Range handles extreme signed bounds without overflow";
  let mesh = two_quads () in
  let vertices = Group_ops.range ~grain:1 ~owner:Group_ops.Group_vertices
      ~name:"corners" (Group_ops.Range_start_end { start = 1; end_ = 3 }) mesh
      |> get_ok in
  expect_members [1; 2; 3] (ordinary Group.Vertex "corners" vertices)
    "Group Range vertex extension";
  let edges = Group_ops.range ~grain:1 ~owner:Group_ops.Group_edges ~name:"edge_range"
      (Group_ops.Range_start_end { start = 0; end_ = 1 }) mesh |> get_ok in
  check (edge_members (edge "edge_range" edges) = [0; 1])
    "Group Range native-edge extension";
  (match Group_ops.range ~filter:{ select = 2; of_ = 1; offset = 0 }
      ~owner:Group_ops.Group_points ~name:"bad"
      (Group_ops.Range_start_end { start = 0; end_ = 1 }) base with
   | Error error -> check (Error.code error = "invalid_group")
       "Group Range invalid filter error code"
   | Ok _ -> fail "Group Range accepted an invalid filter")

let test_rename_invert_delete () =
  let base = point_cloud 6
      |> with_group Group.Point "piece_a" (fun point -> point = 0)
      |> with_group Group.Point "piece_b" (fun point -> point = 1)
      |> with_group Group.Point "keep" (fun point -> point = 2)
      |> with_group Group.Point "unused" (fun _ -> false) in
  let sequential = Group_ops.rename ~rules:[
      { rename_owner = Some Group_ops.Group_points; rename_pattern = "piece_*";
        rename_replacement = "part_*"; rename_conflict = Group_ops.Rename_error };
      { rename_owner = Some Group_ops.Group_points; rename_pattern = "part_*";
        rename_replacement = "final_*"; rename_conflict = Group_ops.Rename_error }]
      base |> get_ok in
  check (Geometry.find_group ~owner:Group.Point "piece_a" sequential = None
      && Geometry.find_group ~owner:Group.Point "part_a" sequential = None
      && Geometry.find_group ~owner:Group.Point "final_a" sequential <> None)
    "Group Rename rules observe earlier rewrites";
  let unioned = Group_ops.rename ~rules:[
      { rename_owner = Some Group_ops.Group_points; rename_pattern = "piece_a";
        rename_replacement = "piece_b"; rename_conflict = Group_ops.Rename_union }]
      base |> get_ok in
  expect_members [0; 1] (ordinary Group.Point "piece_b" unioned)
    "Group Rename union conflict";
  check (Geometry.find_group ~owner:Group.Point "piece_a" unioned = None)
    "Group Rename union removes the source name";
  (match Group_ops.rename ~rules:[
      { rename_owner = Some Group_ops.Group_points; rename_pattern = "piece_a";
        rename_replacement = "piece_b"; rename_conflict = Group_ops.Rename_error }]
      base with
   | Error error -> check (Error.code error = "invalid_group")
       "Group Rename conflict error code"
   | Ok _ -> fail "Group Rename ignored an error conflict");
  let inverted = Group_ops.invert ~owner:Group_ops.Group_points ~pattern:"piece_*"
      ~new_name:"not_*" base |> get_ok in
  expect_members [1; 2; 3; 4; 5] (ordinary Group.Point "not_a" inverted)
    "Group Invert wildcard rewrite membership";
  check (Geometry.find_group ~owner:Group.Point "piece_a" inverted = None)
    "Group Invert new name replaces the source name";
  let mesh = two_quads () |> with_edge_group "edge_keep" (fun edge -> edge = 0)
      |> with_edge_group "edge_drop" (fun edge -> edge = 1) in
  let edge_inverted = Group_ops.invert ~owner:Group_ops.Group_edges
      ~pattern:"edge_keep" mesh |> get_ok in
  let edge_count = Topology_index.edge_count
      (Topology_index.create (Geometry.topology mesh)) in
  check (Edge_group.cardinality (edge "edge_keep" edge_inverted) = edge_count - 1)
    "Group Invert native-edge parity";
  let deleted = Group_ops.delete ~delete_unused:true ~rules:[
      { delete_owner = Some Group_ops.Group_points; delete_pattern = "piece_*" }]
      base |> get_ok in
  check (Geometry.find_group ~owner:Group.Point "piece_a" deleted = None
      && Geometry.find_group ~owner:Group.Point "piece_b" deleted = None
      && Geometry.find_group ~owner:Group.Point "unused" deleted = None
      && Geometry.find_group ~owner:Group.Point "keep" deleted <> None)
    "Group Delete patterns and unused cleanup";
  let kept = Group_ops.delete ~rules:[
      { delete_owner = None; delete_pattern = "* ^keep" }] base |> get_ok in
  check (List.map Group.name (Geometry.groups kept) = ["keep"])
    "Group Delete include/exclude name pattern";
  let unchanged = Group_ops.delete ~rules:[] base |> get_ok in
  check (unchanged == base) "Group Delete no-op preserves geometry identity"

let test_copy () =
  let source = point_cloud 5
      |> with_group Group.Point "picked" (fun point -> point = 1 || point = 2)
      |> with_group Group.Point "past_target" (fun point -> point = 4)
      |> with_int Attribute.Point "id" [|7; 7; 9; 11; 12|]
      |> with_text Attribute.Point "label" [|"a"; "b"; "c"; "d"; "e"|] in
  let target = point_cloud 3
      |> with_int Attribute.Point "id" [|7; 9; 99|]
      |> with_text Attribute.Point "label" [|"b"; "e"; "missing"|] in
  let by_index = Group_ops.copy ~grain:1 ~rules:[
      { copy_owner = Group_ops.Group_points; copy_pattern = "picked";
        copy_prefix = "src_"; match_attribute = None }]
      ~source ~target () |> get_ok in
  expect_members [1; 2] (ordinary Group.Point "src_picked" by_index)
    "Group Copy point index mapping and prefix";
  let by_integer = Group_ops.copy ~grain:1 ~rules:[
      { copy_owner = Group_ops.Group_points; copy_pattern = "picked";
        copy_prefix = "int_"; match_attribute = Some "id" }]
      ~source ~target () |> get_ok in
  expect_members [1] (ordinary Group.Point "int_picked" by_integer)
    "Group Copy integer matching uses first duplicate source value";
  let by_text = Group_ops.copy ~grain:1 ~rules:[
      { copy_owner = Group_ops.Group_points; copy_pattern = "past_target";
        copy_prefix = "text_"; match_attribute = Some "label" }]
      ~source ~target () |> get_ok in
  expect_members [1] (ordinary Group.Point "text_past_target" by_text)
    "Group Copy text attribute matching";
  let suppressed = Group_ops.copy ~grain:1 ~rules:[
      { copy_owner = Group_ops.Group_points; copy_pattern = "past_target";
        copy_prefix = ""; match_attribute = None }]
      ~source ~target () |> get_ok in
  check (Geometry.find_group ~owner:Group.Point "past_target" suppressed = None)
    "Group Copy suppresses empty outputs by default";
  let retained = Group_ops.copy ~grain:1 ~copy_empty:true ~rules:[
      { copy_owner = Group_ops.Group_points; copy_pattern = "past_target";
        copy_prefix = ""; match_attribute = None }]
      ~source ~target () |> get_ok in
  check (Group.cardinality (ordinary Group.Point "past_target" retained) = 0)
    "Group Copy optionally retains empty outputs";
  let conflict_target = target
      |> with_group Group.Point "picked" (fun point -> point = 0) in
  let rule = [{ Group_ops.copy_owner = Group_ops.Group_points; copy_pattern = "picked";
    copy_prefix = ""; match_attribute = None }] in
  let skipped = Group_ops.copy ~grain:1 ~rules:rule ~conflict:Group_ops.Copy_skip
      ~source ~target:conflict_target () |> get_ok in
  expect_members [0] (ordinary Group.Point "picked" skipped)
    "Group Copy skip conflict";
  let overwritten = Group_ops.copy ~grain:1 ~rules:rule
      ~conflict:Group_ops.Copy_overwrite ~source ~target:conflict_target () |> get_ok in
  expect_members [1; 2] (ordinary Group.Point "picked" overwritten)
    "Group Copy overwrite conflict";
  let suffixed = Group_ops.copy ~grain:1 ~rules:rule
      ~conflict:Group_ops.Copy_add_suffix ~source ~target:conflict_target () |> get_ok in
  expect_members [0] (ordinary Group.Point "picked" suffixed)
    "Group Copy suffix preserves destination";
  expect_members [1; 2] (ordinary Group.Point "picked2" suffixed)
    "Group Copy suffix starts at two";
  let source_mesh = mesh [|[|0; 1; 2|]; [|2; 3; 4; 5|]|] 6
      |> with_group Group.Vertex "corners"
        (fun vertex -> vertex = 1 || vertex = 5)
      |> with_edge_group "edge_zero" (fun edge -> edge = 0) in
  let target_mesh = mesh [|[|5; 4; 3; 2|]; [|2; 1; 0|]|] 6 in
  let copied_mesh = Group_ops.copy ~grain:1 ~rules:[
      { copy_owner = Group_ops.Group_vertices; copy_pattern = "*";
        copy_prefix = ""; match_attribute = None };
      { copy_owner = Group_ops.Group_edges; copy_pattern = "*";
        copy_prefix = ""; match_attribute = None }]
      ~source:source_mesh ~target:target_mesh () |> get_ok in
  expect_members [1; 6] (ordinary Group.Vertex "corners" copied_mesh)
    "Group Copy vertices match primitive/local-corner coordinates";
  check (edge_members (edge "edge_zero" copied_mesh) = [0])
    "Group Copy native edges match stable edge indices";
  (match Group_ops.copy ~rules:[
      { copy_owner = Group_ops.Group_edges; copy_pattern = "*";
        copy_prefix = ""; match_attribute = Some "id" }]
      ~source:source_mesh ~target:target_mesh () with
   | Error error -> check (Error.code error = "invalid_group")
       "Group Copy rejects edge attribute matching"
   | Ok _ -> fail "Group Copy accepted edge attribute matching")

let test_parallel_exactness_and_cancellation () =
  let base = Plane_generators.grid ~columns:500 ~rows:300 ~size:20. () |> get_ok in
  let width = 501 in
  let source = base
      |> with_group Group.Point "stripe_a"
        (fun point -> point mod width = width / 3)
      |> with_group Group.Point "stripe_b"
        (fun point -> point mod width = (2 * width) / 3)
      |> with_int Attribute.Point "id"
        (Array.init (Geometry.point_count base) Fun.id) in
  let target = base |> with_int Attribute.Point "id"
      (Array.init (Geometry.point_count base) (fun index ->
        Geometry.point_count base - index - 1)) in
  let run domains = Parallel.run ~domains (fun () ->
    let ranged = Group_ops.range ~grain:257 ~owner:Group_ops.Group_points
        ~name:"bands" ~filter:{ select = 5; of_ = 13; offset = 3 }
        (Group_ops.Range_from_ends { start = 17; end_offset = 23 }) source |> get_ok in
    let combined = Group_ops.combine ~grain:257 ~owner:Group_ops.Group_points
        ~name:"selection" ~base:{ pattern = "stripe_*"; inverted = false }
        ~steps:[{ operation = Group_ops.Group_xor;
          operand = { pattern = "bands"; inverted = false } }] ranged |> get_ok in
    Group_ops.copy ~grain:257 ~rules:[
      { copy_owner = Group_ops.Group_points; copy_pattern = "selection";
        copy_prefix = "copied_"; match_attribute = Some "id" }]
      ~source:combined ~target () |> get_ok) in
  let one = run 1 and four = run 4 in
  check (same_group (ordinary Group.Point "copied_selection" one)
      (ordinary Group.Point "copied_selection" four))
    "Group Range/Combine/Copy one/four-domain exactness";
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  (match Group_ops.range ~cancel:cancelled ~owner:Group_ops.Group_points ~name:"x"
      (Group_ops.Range_start_end { start = 0; end_ = 100 }) source with
   | Error error -> check (Error.code error = "cancelled")
       "Group Range cancellation code"
   | Ok _ -> fail "cancelled Group Range published geometry");
  (match Group_ops.copy ~cancel:cancelled ~source ~target () with
   | Error error -> check (Error.code error = "cancelled")
       "Group Copy cancellation code"
   | Ok _ -> fail "cancelled Group Copy published geometry")

let transfer_rule owner pattern prefix = {
  Group_ops.transfer_owner = owner;
  transfer_pattern = pattern;
  transfer_prefix = prefix;
}

let curve_mesh positions curves =
  let positions = Packed.Float3.Private.of_owned_exn
      ~x:(Array.map (fun (x, _, _) -> x) positions)
      ~y:(Array.map (fun (_, y, _) -> y) positions)
      ~z:(Array.map (fun (_, _, z) -> z) positions) in
  let topology = Rdk_test_support.Tb.create ~point_count:(Packed.Float3.length positions) () in
  Array.iter (Rdk_test_support.Tb.add_open_polyline topology) curves;
  Geometry.create ~positions ~topology:(Rdk_test_support.Tb.freeze topology) ()
  |> function Ok geometry -> geometry | Error message -> fail message

let test_transfer () =
  let source = point_cloud 2
      |> with_group Group.Point "picked" (fun point -> point = 0)
      |> with_group Group.Point "unused" (fun point -> point = 1) in
  let source = Transform_ops.transform (Rays_math.Mat4.scaling (Rays_math.Vec3.create 10. 1. 1.)) source in
  let target = Line_geometry.points [|(0.1, 0., 0.); (9.9, 0., 0.); (5., 0., 0.)|] in
  let rules = [transfer_rule Group_ops.Group_points "picked" "near_"] in
  let transferred = Group_ops.transfer ~grain:1 ~distance:0.2 ~rules
      ~source ~target () |> get_ok in
  expect_members [0] (ordinary Group.Point "near_picked" transferred)
    "Group Transfer point proximity/threshold";
  let tied = Group_ops.transfer ~grain:1 ~distance:5. ~rules
      ~source ~target () |> get_ok in
  expect_members [0; 2] (ordinary Group.Point "near_picked" tied)
    "Group Transfer point equal-distance lower-index tie";
  let ordered_source = Line_geometry.points [|(0., 0., 0.); (10., 0., 0.)|]
      |> with_ordered_group Group.Point "path" [|1; 0|] in
  let ordered_target = Line_geometry.points
      [|(0.9, 0., 0.); (0.1, 0., 0.); (9.8, 0., 0.); (10.5, 0., 0.)|] in
  let ordered_transfer = Group_ops.transfer ~grain:1 ~distance:1.
      ~rules:[transfer_rule Group_ops.Group_points "path" "mapped_"]
      ~source:ordered_source ~target:ordered_target () |> get_ok in
  check (ordered_group_members
      (ordinary Group.Point "mapped_path" ordered_transfer) = [2; 3; 1; 0])
    "Group Transfer orders destinations by source sequence then proximity";
  let conflict_target = target
      |> with_group Group.Point "near_picked" (fun point -> point = 2) in
  let skipped = Group_ops.transfer ~grain:1 ~distance:0.2 ~rules
      ~source ~target:conflict_target () |> get_ok in
  expect_members [2] (ordinary Group.Point "near_picked" skipped)
    "Group Transfer skip conflict";
  let overwritten = Group_ops.transfer ~grain:1 ~distance:0.2 ~rules
      ~conflict:Group_ops.Copy_overwrite ~source ~target:conflict_target () |> get_ok in
  expect_members [0] (ordinary Group.Point "near_picked" overwritten)
    "Group Transfer overwrite conflict";
  let suffixed = Group_ops.transfer ~grain:1 ~distance:0.2 ~rules
      ~conflict:Group_ops.Copy_add_suffix ~source ~target:conflict_target () |> get_ok in
  expect_members [2] (ordinary Group.Point "near_picked" suffixed)
    "Group Transfer suffix preserves destination";
  expect_members [0] (ordinary Group.Point "near_picked2" suffixed)
    "Group Transfer suffix begins at two";
  let empty_rule = [transfer_rule Group_ops.Group_points "unused" "near_"] in
  let omitted = Group_ops.transfer ~grain:1 ~distance:0.05 ~rules:empty_rule
      ~source ~target () |> get_ok in
  check (Geometry.find_group ~owner:Group.Point "near_unused" omitted = None)
    "Group Transfer created an empty group by default";
  let retained = Group_ops.transfer ~grain:1 ~distance:0.05 ~rules:empty_rule
      ~create_empty:true ~source ~target () |> get_ok in
  check (Group.cardinality (ordinary Group.Point "near_unused" retained) = 0)
    "Group Transfer create-empty policy";
  let no_op = Group_ops.transfer ~rules:[transfer_rule Group_ops.Group_points
      "missing*" "mapped_"] ~source ~target () |> get_ok in
  check (no_op == target) "Group Transfer unmatched pattern rebuilt target";
  let curve_source = curve_mesh
      [|(-1.,0.,0.); (1.,0.,0.); (5.,0.,0.); (6.,0.,0.)|]
      [|[|0; 1|]; [|2; 3|]|]
      |> with_group Group.Primitive "crossing_curve" (fun primitive -> primitive = 0)
      |> with_edge_group "crossing_edge" (fun edge -> edge = 0) in
  let curve_target = curve_mesh [|(0.,-1.,0.); (0.,1.,0.)|] [|[|0; 1|]|] in
  let curve_transferred = Group_ops.transfer ~grain:1 ~distance:0.
      ~rules:[transfer_rule Group_ops.Group_primitives "crossing_curve" "mapped_";
        transfer_rule Group_ops.Group_edges "crossing_edge" "mapped_"]
      ~source:curve_source ~target:curve_target () |> get_ok in
  expect_members [0] (ordinary Group.Primitive "mapped_crossing_curve"
      curve_transferred) "Group Transfer exact crossing curve primitives";
  check (edge_members (edge "mapped_crossing_edge" curve_transferred) = [0])
    "Group Transfer exact crossing native edges";
  let triangle_source = mesh
      [|[|0; 1; 2|]; [|3; 4; 5|]|] 6 in
  let triangle_positions = Packed.Float3.Private.of_owned_exn
      ~x:[|0.; 0.; 0.; -0.2; 0.2; 0.|]
      ~y:[|-1.; 1.; 0.; -0.2; -0.2; 0.2|]
      ~z:[|-1.; -1.; 1.; 0.05; 0.05; 0.05|] in
  let triangle_source = Geometry.with_positions triangle_positions triangle_source
      |> function Ok geometry -> geometry | Error message -> fail message in
  let triangle_source = with_group Group.Primitive "intersecting"
      (fun primitive -> primitive = 0) triangle_source in
  let triangle_target = mesh [|[|0; 1; 2|]|] 3 in
  let target_positions = Packed.Float3.Private.of_owned_exn
      ~x:[|-0.2; 0.2; 0.|] ~y:[|-0.2; -0.2; 0.2|]
      ~z:[|0.; 0.; 0.|] in
  let triangle_target = Geometry.with_positions target_positions triangle_target
      |> function Ok geometry -> geometry | Error message -> fail message in
  let triangle_transferred = Group_ops.transfer ~grain:1 ~distance:0.
      ~rules:[transfer_rule Group_ops.Group_primitives "intersecting" "mapped_"]
      ~source:triangle_source ~target:triangle_target () |> get_ok in
  expect_members [0] (ordinary Group.Primitive "mapped_intersecting"
      triangle_transferred) "Group Transfer triangle intersection proximity";
  (match Group_ops.transfer ~distance:(-1.) ~source ~target () with
   | Error error -> check (Error.code error = "invalid_group")
       "Group Transfer invalid distance code"
   | Ok _ -> fail "Group Transfer accepted a negative distance");
  (match Group_ops.transfer ~rules:[transfer_rule Group_ops.Group_vertices "*" ""]
      ~source ~target () with
   | Error error -> check (Error.code error = "invalid_group")
       "Group Transfer vertex-owner rejection code"
   | Ok _ -> fail "Group Transfer accepted vertex groups");
  let degenerate = mesh [|[|0; 1; 2|]|] 3
      |> with_group Group.Primitive "bad" (fun _ -> true) in
  (match Group_ops.transfer ~distance:1.
      ~rules:[transfer_rule Group_ops.Group_primitives "bad" ""]
      ~source:degenerate ~target:triangle_target () with
   | Error error -> check (Error.code error = "invalid_group")
       "Group Transfer degenerate-primitive error code"
   | Ok _ -> fail "Group Transfer accepted a degenerate polygon");
  let nonfinite = Line_geometry.points [|(Float.nan, 0., 0.)|]
      |> with_group Group.Point "bad" (fun _ -> true) in
  (match Group_ops.transfer ~distance:1.
      ~rules:[transfer_rule Group_ops.Group_points "bad" ""]
      ~source:nonfinite ~target () with
   | Error error -> check (Error.code error = "invalid_group")
       "Group Transfer non-finite source error code"
   | Ok _ -> fail "Group Transfer accepted a non-finite source point");
  let huge_curve = curve_mesh
      [|(-.max_float, 0., 0.); (max_float, 0., 0.)|] [|[|0; 1|]|]
      |> with_group Group.Primitive "huge" (fun _ -> true) in
  (match Group_ops.transfer ~distance:1.
      ~rules:[transfer_rule Group_ops.Group_primitives "huge" ""]
      ~source:huge_curve ~target:huge_curve () with
   | Error error -> check (Error.code error = "invalid_group")
       "Group Transfer finite-overflow diagnostic code"
   | Ok _ -> fail "Group Transfer silently accepted overflowing distance math")

let test_transfer_parallel_exactness () =
  let source = Plane_generators.grid ~columns:100 ~rows:80 ~size:20. () |> get_ok in
  let source = source
      |> with_group Group.Point "point_band" (fun point -> point mod 101 < 7)
      |> with_ordered_group Group.Point "ordered_seed" [|404; 5; 8_000; 2|]
      |> with_group Group.Primitive "face_band" (fun primitive -> primitive mod 19 < 3)
      |> with_edge_group "edge_band" (fun edge -> edge mod 23 < 2) in
  let target = Transform_ops.transform ~grain:257
      (Rays_math.Mat4.translation (Rays_math.Vec3.create 0.0001 0. 0.0001)) source in
  let rules = [transfer_rule Group_ops.Group_points "point_band" "mapped_";
    transfer_rule Group_ops.Group_points "ordered_seed" "mapped_";
    transfer_rule Group_ops.Group_primitives "face_band" "mapped_";
    transfer_rule Group_ops.Group_edges "edge_band" "mapped_"] in
  let run domains = Parallel.run ~domains (fun () ->
    Group_ops.transfer ~grain:257 ~distance:0.01 ~rules
      ~conflict:Group_ops.Copy_overwrite ~source ~target () |> get_ok) in
  let one = run 1 and four = run 4 in
  check (same_group (ordinary Group.Point "mapped_point_band" one)
      (ordinary Group.Point "mapped_point_band" four))
    "Group Transfer point one/four-domain exactness";
  check (same_group (ordinary Group.Primitive "mapped_face_band" one)
      (ordinary Group.Primitive "mapped_face_band" four))
    "Group Transfer primitive one/four-domain exactness";
  check (same_group (ordinary Group.Point "mapped_ordered_seed" one)
      (ordinary Group.Point "mapped_ordered_seed" four))
    "Group Transfer ordered point one/four-domain exactness";
  check (edge_members (edge "mapped_edge_band" one)
      = edge_members (edge "mapped_edge_band" four))
    "Group Transfer edge one/four-domain exactness";
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  (match Group_ops.transfer ~cancel:cancelled ~rules ~source ~target () with
   | Error error -> check (Error.code error = "cancelled")
       "Group Transfer cancellation code"
   | Ok _ -> fail "cancelled Group Transfer published geometry")

let run () =
  test_ordered_group_core ();
  test_ordered_group_topology_remap ();
  test_complement_and_combine ();
  test_range ();
  test_rename_invert_delete ();
  test_copy ();
  test_parallel_exactness_and_cancellation ();
  test_transfer ();
  test_transfer_parallel_exactness ();
  print_endline "group catalog tests passed"
