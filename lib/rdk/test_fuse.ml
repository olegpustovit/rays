open Rays
open Rdk
open Rdk_test_support

(* Rule records with the removed constructors' arguments. *)
let fuse_attribute_rule ?weight_attribute ~pattern method_ =
  { Fuse_reduce.pattern; method_; weight_attribute }
let fuse_group_rule ~pattern method_ =
  { Fuse_reduce.group_pattern = pattern; group_method = method_ }

let near left right = abs_float (left -. right) <= 1e-12

let add_attribute attribute geometry =
  Geometry.with_attribute attribute geometry |> get_string_ok

let point_float geometry name =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Float values -> values
       | _ -> fail (name ^ " has unexpected storage"))
  | None -> fail ("missing point float " ^ name)

let point_int geometry name =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Int values -> values
       | _ -> fail (name ^ " has unexpected storage"))
  | None -> fail ("missing point integer " ^ name)

let owned_int geometry owner name =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Int values -> values
       | _ -> fail (name ^ " has unexpected storage"))
  | None -> fail ("missing integer attribute " ^ name)

let add_point_int name values geometry =
  let attribute = Attribute.create_owned ~owner:Attribute.Point ~name
      (Attribute.Int values) |> get_string_ok in
  add_attribute attribute geometry

let add_point_float name values geometry =
  let attribute = Attribute.create_owned ~owner:Attribute.Point ~name
      (Attribute.Float values) |> get_string_ok in
  add_attribute attribute geometry

let add_point_text name values geometry =
  let attribute = Attribute.create_owned ~owner:Attribute.Point ~name
      (Attribute.Text values) |> get_string_ok in
  add_attribute attribute geometry

let add_point_int_array name ~offsets ~values geometry =
  let values = Packed.Int_array.create_owned ~offsets ~values |> get_string_ok in
  let attribute = Attribute.create_owned ~owner:Attribute.Point ~name
      (Attribute.Int_array values) |> get_string_ok in
  add_attribute attribute geometry

let point_text geometry name =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Text values -> values
       | _ -> fail (name ^ " has unexpected storage"))
  | None -> fail ("missing point text " ^ name)

let point_int_array geometry name =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Int_array values -> Packed.Int_array.Private.view values
       | _ -> fail (name ^ " has unexpected storage"))
  | None -> fail ("missing point integer array " ^ name)

let check_point_group geometry name expected =
  match Geometry.find_group ~owner:Group.Point name geometry with
  | None -> fail ("missing point group " ^ name)
  | Some group ->
      check (Group.cardinality group = List.length expected)
        (name ^ " cardinality");
      for point = 0 to Geometry.point_count geometry - 1 do
        check (Group.mem point group = List.mem point expected)
          (name ^ " membership")
      done

let equal_target_output left right =
  let lp = positions left and rp = positions right
  and lt = Topology.Private.view (Geometry.topology left)
  and rt = Topology.Private.view (Geometry.topology right) in
  lp.x = rp.x && lp.y = rp.y && lp.z = rp.z
  && lt.vertex_points = rt.vertex_points
  && lt.primitive_offsets = rt.primitive_offsets
  && lt.primitive_kinds = rt.primitive_kinds
  && point_int left "destination" = point_int right "destination"
  && (match Geometry.find_group ~owner:Group.Point "snapped" left,
      Geometry.find_group ~owner:Group.Point "snapped" right with
      | Some a, Some b ->
          Group.cardinality a = Group.cardinality b
          && (let equal = ref true in
              for point = 0 to Geometry.point_count left - 1 do
                if Group.mem point a <> Group.mem point b then equal := false
              done;
              !equal)
      | _ -> false)

let check_target_policies () =
  let source = Line_geometry.points [|(0.1,0.,0.); (0.9,0.,0.); (2.,0.,0.);
      (5.,0.,0.)|]
  and target = Line_geometry.points [|(0.,0.,0.); (1.,0.,0.); (3.,0.,0.)|] in
  let queries = Group.init ~owner:Group.Point ~name:"queries" 4
      (fun point -> point < 3)
  and targets = Group.init ~owner:Group.Point ~name:"targets" 3
      (fun point -> point < 2) in
  let run using = Fuse_grid.fuse ~selection:queries ~target_selection:targets
      ~target ~using ~tolerance:1. ~fuse_points:false
      ~snapped_group:"snapped" ~snapped_destination_attribute:"destination"
      source |> get_ok in
  let least = run Fuse_grid.Least_target_point and closest = run Fuse_grid.Closest_target_point in
  let lp = positions least and cp = positions closest in
  check (lp.x = [|0.;0.;1.;5.|]) "least-target Fuse positions";
  check (point_int least "destination" = [|0;0;1;-1|])
    "least-target destination output";
  check_point_group least "snapped" [0;1;2];
  check (cp.x = [|0.;1.;1.;5.|]) "closest-target Fuse positions";
  check (point_int closest "destination" = [|0;1;1;-1|])
    "closest-target destination output";
  let fused = Fuse_grid.fuse ~selection:queries ~target_selection:targets ~target
      ~using:Fuse_grid.Closest_target_point ~tolerance:1.
      ~snapped_group:"snapped" ~snapped_destination_attribute:"destination"
      source |> get_ok in
  check (Geometry.point_count fused = 3 && (positions fused).x = [|0.;1.;5.|])
    "external-target post-snap Fuse cardinality/order";
  check (point_int fused "destination" = [|0;1;-1|])
    "post-fuse destination remap";
  check_point_group fused "snapped" [0;1]

let check_specified_targets () =
  let source = Line_geometry.points [|(9.,0.,0.); (9.,0.,0.); (2.,0.,0.); (5.,0.,0.)|]
      |> add_point_int "target_point" [|2;0;99;-1|]
  and target = Line_geometry.points [|(0.,0.,0.); (1.,0.,0.); (3.,0.,0.)|] in
  let targets = Group.init ~owner:Group.Point ~name:"targets" 3
      (fun point -> point <> 1) in
  let output = Fuse_grid.fuse ~target ~target_selection:targets
      ~targeting:(Fuse_grid.Specified_points "target_point") ~fuse_points:false
      ~snapped_destination_attribute:"destination" source |> get_ok in
  check ((positions output).x = [|3.;0.;2.;5.|])
    "specified-target Fuse positions";
  check (point_int output "destination" = [|2;0;-1;-1|])
    "specified-target validity/group filtering";
  expect_code "invalid_geometry" (Fuse_grid.fuse ~target
      ~targeting:(Fuse_grid.Specified_points "missing") source)

let check_radius_and_match () =
  let source = Line_geometry.points [|(1.9,0.,0.); (0.9,0.,0.); (0.1,0.,0.)|]
      |> add_point_float "radius" [|0.6;0.;0.|]
      |> add_point_int "piece" [|3;2;1|]
  and target = Line_geometry.points [|(0.,0.,0.); (1.,0.,0.); (3.,0.,0.)|]
      |> add_point_float "radius" [|0.;0.;0.5|]
      |> add_point_int "piece" [|1;2;3|] in
  let radius = Fuse_grid.fuse ~target ~tolerance:0. ~radius_attribute:"radius"
      ~using:Fuse_grid.Closest_target_point ~fuse_points:false source |> get_ok in
  check ((positions radius).x = [|3.;0.9;0.1|])
    "radius-expanded Fuse threshold";
  let equal = Fuse_grid.fuse ~target ~tolerance:2. ~match_attribute:"piece"
      ~using:Fuse_grid.Closest_target_point ~fuse_points:false source |> get_ok in
  check ((positions equal).x = [|3.;1.;0.|])
    "equal match-attribute Fuse filter";
  let unequal = Fuse_grid.fuse ~target ~tolerance:2. ~match_attribute:"piece"
      ~match_condition:Fuse_grid.Unequal_attribute_values
      ~using:Fuse_grid.Closest_target_point ~fuse_points:false source |> get_ok in
  check ((positions unequal).x = [|1.;0.;1.|])
    "unequal match-attribute Fuse filter";
  let float_source = Line_geometry.points [|(0.,0.,0.)|]
      |> add_point_float "key" [|1.001|]
  and float_target = Line_geometry.points [|(1.,0.,0.)|]
      |> add_point_float "key" [|1.|] in
  let tolerant = Fuse_grid.fuse ~target:float_target ~tolerance:2.
      ~match_attribute:"key" ~match_tolerance:0.01 ~fuse_points:false
      float_source |> get_ok in
  check ((positions tolerant).x = [|1.|]) "float match tolerance"

let check_separate_same_geometry_groups () =
  let source = Line_geometry.points [|(0.,0.,0.); (0.1,0.,0.); (10.,0.,0.)|] in
  let queries = Group.init ~owner:Group.Point ~name:"queries" 3
      (fun point -> point = 1)
  and targets = Group.init ~owner:Group.Point ~name:"targets" 3
      (fun point -> point = 0) in
  let output = Fuse_grid.fuse ~selection:queries ~target_selection:targets
      ~tolerance:0.2 ~snapped_destination_attribute:"destination" source
      |> get_ok in
  check (Geometry.point_count output = 2 && (positions output).x = [|0.;10.|])
    "same-geometry separate query/target fusion";
  check (point_int output "destination" = [|0;-1|])
    "same-geometry destination ancestry"

let check_target_scale_and_validation () =
  let center = 1e150 and scale = 1e140 in
  let source = Line_geometry.points [|(center +. scale, center, center)|]
  and target = Line_geometry.points [|(center, center, center)|] in
  let output = Fuse_grid.fuse ~target ~tolerance:(2. *. scale)
      ~using:Fuse_grid.Closest_target_point ~fuse_points:false source |> get_ok in
  check ((positions output).x = [|center|])
    "large finite target Fuse normalization";
  let invalid_radius = Line_geometry.points [|(0.,0.,0.)|]
      |> add_point_float "radius" [|(-1.)|] in
  let valid_radius = Line_geometry.points [|(0.,0.,0.)|]
      |> add_point_float "radius" [|0.|] in
  expect_code "invalid_geometry" (Fuse_grid.fuse ~target:valid_radius
      ~radius_attribute:"radius" invalid_radius);
  expect_code "invalid_geometry" (Fuse_grid.fuse ~target
      ~match_condition:Fuse_grid.Unequal_attribute_values source);
  expect_code "invalid_geometry" (Fuse_grid.fuse ~target
      ~targeting:(Fuse_grid.Specified_points "destination")
      ~radius_attribute:"radius" source);
  let wrong_target_group = Group.init ~owner:Group.Point ~name:"wrong" 2
      (fun _ -> true) in
  expect_code "invalid_geometry" (Fuse_grid.fuse ~target
      ~target_selection:wrong_target_group source);
  let nonfinite = Line_geometry.points [|(Float.infinity,0.,0.)|] in
  expect_code "invalid_geometry" (Fuse_grid.fuse ~target:nonfinite
      ~using:Fuse_grid.Closest_target_point source);
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  expect_code "cancelled" (Fuse_grid.fuse ~cancel:cancelled ~target
      ~using:Fuse_grid.Closest_target_point source)

let check_target_parallel_exact () =
  let target = Plane_generators.grid ~columns:420 ~rows:320 ~size:30. () |> get_ok in
  let source = target
      |> Transform_ops.transform (Rays_math.Mat4.translation (Rays_math.Vec3.create 0.013 (-0.017) 0.009)) in
  let run domains = Parallel.run ~domains (fun () ->
    Fuse_grid.fuse ~grain:1024 ~target ~using:Fuse_grid.Closest_target_point
      ~tolerance:0.05 ~fuse_points:false ~snapped_group:"snapped"
      ~snapped_destination_attribute:"destination" source |> get_ok) in
  let one = run 1 and four = run 4 in
  check (equal_target_output one four)
    "one/four-domain target Fuse output differs";
  let base = Plane_generators.grid ~columns:220 ~rows:160 ~size:20. () |> get_ok in
  let half = Geometry.point_count base in
  let linked = Mesh_merge.run [base;
      Transform_ops.transform (Rays_math.Mat4.translation (Rays_math.Vec3.create 0.013 (-0.017) 0.009)) base]
      |> get_ok in
  let linked_count = Geometry.point_count linked in
  let linked = linked
      |> add_point_float "rule_weight" (Array.init linked_count (fun point ->
          if point < half then 1. else 3.))
      |> add_point_float "rule_value" (Array.init linked_count (fun point ->
          sin (float_of_int (point mod half) *. 0.013)))
      |> add_point_int "rule_id" (Array.init linked_count (fun point -> point mod 17))
      |> add_point_text "rule_text" (Array.init linked_count (fun point ->
          if point < half then "q" else "t")) in
  let linked = Geometry.with_group
      (Group.init ~owner:Group.Point ~name:"rule_group" linked_count
        (fun point -> point land 1 = 0)) linked |> get_string_ok in
  let queries = Group.init ~owner:Group.Point ~name:"queries" linked_count
      (fun point -> point < half)
  and targets = Group.init ~owner:Group.Point ~name:"targets" linked_count
      (fun point -> point >= half) in
  let rules_run domains = Parallel.run ~domains (fun () -> Fuse_grid.fuse ~grain:257
      ~selection:queries ~target_selection:targets ~modify_target:true
      ~using:Fuse_grid.Closest_target_point ~tolerance:0.05
      ~attribute_rules:[
        fuse_attribute_rule ~pattern:"rule_value"
          ~weight_attribute:"rule_weight" Fuse_reduce.Attribute_weighted_average;
        fuse_attribute_rule ~pattern:"rule_id" Fuse_reduce.Attribute_mode;
        fuse_attribute_rule ~pattern:"rule_text"
          ~weight_attribute:"rule_weight"
          Fuse_reduce.Attribute_concatenate_weight_order]
      ~group_rules:[fuse_group_rule ~pattern:"rule_group"
        Fuse_reduce.Group_most_common] linked |> get_ok) in
  let rules_one = rules_run 1 and rules_four = rules_run 4 in
  let one_topology = Topology.Private.view (Geometry.topology rules_one)
  and four_topology = Topology.Private.view (Geometry.topology rules_four) in
  check ((positions rules_one).x = (positions rules_four).x
      && (positions rules_one).y = (positions rules_four).y
      && (positions rules_one).z = (positions rules_four).z
      && one_topology.vertex_points = four_topology.vertex_points
      && point_float rules_one "rule_value" = point_float rules_four "rule_value"
      && point_int rules_one "rule_id" = point_int rules_four "rule_id"
      && point_text rules_one "rule_text" = point_text rules_four "rule_text")
    "one/four-domain Fuse attribute-rule output differs";
  (match Geometry.find_group ~owner:Group.Point "rule_group" rules_one,
      Geometry.find_group ~owner:Group.Point "rule_group" rules_four with
   | Some one, Some four ->
       check (Group.Private.bits_view one = Group.Private.bits_view four)
         "one/four-domain Fuse group-rule output differs"
   | _ -> fail "parallel Fuse rule group missing")

let check_position_reductions () =
  let source = Line_geometry.points [|(0.,3.,6.); (2.,1.,4.); (4.,2.,5.)|]
      |> add_point_float "weight" [|1.;2.;3.|] in
  let position mode = Fuse_grid.fuse ~tolerance:10. ~position:mode
      ~weight_attribute:"weight" source |> get_ok |> positions in
  let check_position mode expected label =
    let output = position mode in
    check (near output.x.(0) (fst expected)
        && near output.y.(0) (fst (snd expected))
        && near output.z.(0) (snd (snd expected))) label in
  check_position Fuse_reduce.First_position (0., (3., 6.)) "first position reduction";
  check_position Fuse_reduce.Least_point_position (0., (3., 6.))
    "least-point position reduction";
  check_position Fuse_reduce.Greatest_point_position (4., (2., 5.))
    "greatest-point position reduction";
  check_position Fuse_reduce.Average_position (2., (2., 5.))
    "average position reduction";
  check_position Fuse_reduce.Minimum_position (0., (1., 4.))
    "minimum position reduction";
  check_position Fuse_reduce.Maximum_position (4., (3., 6.))
    "maximum position reduction";
  check_position Fuse_reduce.Median_position (2., (2., 5.))
    "median position reduction";
  check_position Fuse_reduce.Sum_position (6., (6., 15.)) "sum position reduction";
  check_position Fuse_reduce.Sum_squares_position (20., (14., 77.))
    "sum-squares position reduction";
  let rms = position Fuse_reduce.Root_mean_square_position in
  check (near rms.x.(0) (sqrt (20. /. 3.))
      && near rms.y.(0) (sqrt (14. /. 3.))
      && near rms.z.(0) (sqrt (77. /. 3.))) "RMS position reduction";
  check_position Fuse_reduce.Weighted_average_position
    (16. /. 6., (11. /. 6., 29. /. 6.)) "weighted-average position reduction";
  check_position Fuse_reduce.Weighted_sum_position (16., (11., 29.))
    "weighted-sum position reduction";
  check_position Fuse_reduce.Minimum_weight_position (0., (3., 6.))
    "minimum-weight position reduction";
  check_position Fuse_reduce.Maximum_weight_position (4., (2., 5.))
    "maximum-weight position reduction";
  let mode_source = Line_geometry.points
      [|(0.,3.,6.); (2.,1.,4.); (2.,1.,5.); (4.,2.,5.)|] in
  let mode = Fuse_grid.fuse ~tolerance:10. ~position:Fuse_reduce.Mode_position mode_source
      |> get_ok |> positions in
  check (mode.x = [|2.|] && mode.y = [|1.|] && mode.z = [|5.|])
    "component mode position reduction";
  let huge = Line_geometry.points [|(max_float,0.,0.); (max_float,0.,0.)|]
      |> Fuse_grid.fuse ~tolerance:0. ~position:Fuse_reduce.Average_position |> get_ok in
  check ((positions huge).x = [|max_float|]) "overflow-safe average position";
  expect_code "invalid_geometry" (Fuse_grid.fuse ~tolerance:0.
      ~position:Fuse_reduce.Sum_squares_position
      (Line_geometry.points [|(1e200,0.,0.); (1e200,0.,0.)|]));
  let zero_weight = Line_geometry.points [|(0.,0.,0.); (0.,0.,0.)|]
      |> add_point_float "weight" [|1.;(-1.)|] in
  expect_code "invalid_geometry" (Fuse_grid.fuse ~tolerance:0.
      ~position:Fuse_reduce.Weighted_average_position ~weight_attribute:"weight"
      zero_weight)

let check_modify_and_keep_target () =
  let source = Line_geometry.points [|(0.,0.,0.); (2.,0.,0.); (10.,0.,0.)|] in
  let queries = Group.init ~owner:Group.Point ~name:"queries" 3
      (fun point -> point = 0)
  and targets = Group.init ~owner:Group.Point ~name:"targets" 3
      (fun point -> point = 1) in
  let modified = Fuse_grid.fuse ~selection:queries ~target_selection:targets
      ~modify_target:true ~tolerance:3. ~position:Fuse_reduce.Average_position source
      |> get_ok in
  check (Geometry.point_count modified = 2 && (positions modified).x = [|1.;10.|])
    "Modify Target compact average";
  let snapped = Fuse_grid.fuse ~selection:queries ~target_selection:targets
      ~modify_target:true ~fuse_points:false ~tolerance:3.
      ~position:Fuse_reduce.Average_position source |> get_ok in
  check (Geometry.point_count snapped = 3
      && (positions snapped).x = [|1.;1.;10.|])
    "Modify Target snap-only average";
  expect_code "invalid_geometry" (Fuse_grid.fuse ~target:(Line_geometry.points [|(2.,0.,0.)|])
      ~modify_target:true source);
  let positions_value = Packed.Float3.Private.of_owned_exn
      ~x:[|0.;0.1;1.|] ~y:[|0.;0.;0.|] ~z:[|0.;0.;0.|] in
  let topology = Topology.create_owned ~point_count:3
      ~vertex_points:[|0;1;2|] ~primitive_offsets:[|0;3|]
      ~primitive_kinds:[|Topology.Open_polyline|] |> get_string_ok in
  let geometry = Geometry.create ~positions:positions_value ~topology ()
      |> get_string_ok in
  let kept = Fuse_grid.fuse ~tolerance:0.2 ~position:Fuse_reduce.Average_position
      ~keep_fused_points:true geometry |> get_ok in
  let kept_topology = Topology.Private.view (Geometry.topology kept) in
  check (Geometry.point_count kept = 3 && (positions kept).x = [|0.05;0.05;1.|]
      && kept_topology.vertex_points = [|0;0;2|])
    "Keep Fused Points retained snapped unused point";
  expect_code "invalid_geometry" (Fuse_grid.fuse ~tolerance:0.2
      ~fuse_points:false ~keep_fused_points:true geometry)

let cleanup_fixture () =
  let positions_value = Packed.Float3.Private.of_owned_exn
      ~x:[|0.;0.1;1.;2.;5.;5.1;9.|]
      ~y:(Array.make 7 0.) ~z:(Array.make 7 0.) in
  let topology = Topology.create_owned ~point_count:7
      ~vertex_points:[|0;1;2;3; 0;1; 4;5|]
      ~primitive_offsets:[|0;4;6;8|]
      ~primitive_kinds:[|Topology.Polygon; Topology.Open_polyline;
        Topology.Open_polyline|] |> get_string_ok in
  let point_id = Attribute.create_owned ~owner:Attribute.Point ~name:"point_id"
      (Attribute.Int [|100;101;102;103;104;105;106|]) |> get_string_ok
  and vertex_id = Attribute.create_owned ~owner:Attribute.Vertex
      ~name:"vertex_id" (Attribute.Int [|10;11;12;13;14;15;16;17|])
      |> get_string_ok
  and primitive_id = Attribute.create_owned ~owner:Attribute.Primitive
      ~name:"primitive_id" (Attribute.Int [|20;21;22|]) |> get_string_ok in
  let unused = Group.ordered ~owner:Group.Point ~name:"unused" ~length:7
      [|6|] |> get_string_ok in
  let geometry = Geometry.create ~positions:positions_value ~topology
      ~attributes:[point_id;vertex_id;primitive_id] ~groups:[unused] ()
      |> get_string_ok in
  let index = Topology_index.create topology in
  let edge = match Topology_index.find_edge index ~a:1 ~b:2 with
    | Some edge -> edge | None -> fail "cleanup fixture edge missing" in
  let builder = Edge_group.Builder.create ~topology ~index ~name:"edge" in
  Edge_group.Builder.set builder edge true;
  Geometry.with_edge_group (Edge_group.Builder.freeze builder) geometry
  |> get_string_ok

let check_cleanup () =
  let source = cleanup_fixture () in
  let unclean = Fuse_grid.fuse ~tolerance:0.2 source |> get_ok in
  let unclean_topology = Topology.Private.view (Geometry.topology unclean) in
  check (Geometry.primitive_count unclean = 3
      && unclean_topology.vertex_points = [|0;0;1;2; 0;0; 3;3|])
    "Fuse cleanup unexpectedly enabled by default";
  let cleaned = Fuse_grid.fuse ~grain:1 ~tolerance:0.2
      ~remove_degenerate_primitives:true source |> get_ok in
  let topology = Topology.Private.view (Geometry.topology cleaned) in
  check (Geometry.point_count cleaned = 5
      && Geometry.vertex_count cleaned = 3
      && Geometry.primitive_count cleaned = 1
      && topology.vertex_points = [|0;1;2|]
      && topology.primitive_offsets = [|0;3|])
    "Fuse repeated-vertex/degenerate cleanup topology";
  check (owned_int cleaned Attribute.Vertex "vertex_id" = [|10;12;13|]
      && owned_int cleaned Attribute.Primitive "primitive_id" = [|20|])
    "Fuse cleanup vertex/primitive ancestry";
  (match Geometry.find_edge_group "edge" cleaned with
   | Some group ->
       check (Edge_group.cardinality group = 1)
         "Fuse cleanup native edge ancestry"
   | None -> fail "Fuse cleanup dropped native edge group");
  let selective = Fuse_grid.fuse ~grain:1 ~tolerance:0.2
      ~remove_degenerate_primitives:true
      ~remove_unused_points_from_degenerate_primitives:true source |> get_ok in
  check (Geometry.point_count selective = 4
      && (positions selective).x = [|0.05;1.;2.;9.|]
      && owned_int selective Attribute.Point "point_id" = [|100;102;103;106|])
    "Fuse cleanup selective unused-point compaction";
  check_point_group selective "unused" [3];
  let compact = Fuse_grid.fuse ~grain:1 ~tolerance:0.2
      ~remove_degenerate_primitives:true ~remove_all_unused_points:true source
      |> get_ok in
  check (Geometry.point_count compact = 3
      && (positions compact).x = [|0.05;1.;2.|])
    "Fuse cleanup all-unused-point compaction";
  let run domains = Parallel.run ~domains (fun () ->
      Fuse_grid.fuse ~grain:1 ~tolerance:0.2 ~remove_degenerate_primitives:true
        ~remove_all_unused_points:true source |> get_ok) in
  let one = run 1 and four = run 4 in
  let one_topology = Topology.Private.view (Geometry.topology one)
  and four_topology = Topology.Private.view (Geometry.topology four) in
  check ((positions one).x = (positions four).x
      && one_topology.vertex_points = four_topology.vertex_points
      && owned_int one Attribute.Vertex "vertex_id"
         = owned_int four Attribute.Vertex "vertex_id")
    "Fuse cleanup one/four-domain output differs"

let check_attribute_and_group_rules () =
  let source = Line_geometry.points
      [|(0.,0.,0.); (0.,0.,0.); (0.,0.,0.); (0.,0.,0.)|]
      |> add_point_float "f" [|1.;2.;2.;4.|]
      |> add_point_int "i" [|1;2;3;4|]
      |> add_point_int "cat" [|7;8;9;10|]
      |> add_point_float "weight" [|4.;1.;3.;2.|]
      |> add_point_text "label" [|"b";"a";"a";"c"|] in
  let add_group name members geometry = Geometry.with_group
      (Group.init ~owner:Group.Point ~name (Geometry.point_count geometry)
        (fun point -> List.mem point members))
      geometry |> get_string_ok in
  let source = source
      |> add_group "least" [0]
      |> add_group "greatest" [3]
      |> add_group "any" [2]
      |> add_group "all" [0;1;2;3]
      |> add_group "majority" [0;1;2] in
  let ar ?weight pattern method_ = fuse_attribute_rule ?weight_attribute:weight
      ~pattern method_ in
  let gr pattern method_ = fuse_group_rule ~pattern method_ in
  let output = Fuse_grid.fuse ~tolerance:0. ~attribute_rules:[
      ar ~weight:"weight" "f" Fuse_reduce.Attribute_weighted_average;
      ar "i" Fuse_reduce.Attribute_average;
      ar "cat" Fuse_reduce.Attribute_concatenate;
      ar ~weight:"weight" "label" Fuse_reduce.Attribute_concatenate_weight_order;
    ] ~group_rules:[
      gr "least" Fuse_reduce.Group_least_point;
      gr "greatest" Fuse_reduce.Group_greatest_point;
      gr "any" Fuse_reduce.Group_union;
      gr "all" Fuse_reduce.Group_intersection;
      gr "majority" Fuse_reduce.Group_most_common;
    ] source |> get_ok in
  check (point_float output "f" = [|2.|]) "Fuse weighted attribute rule";
  check (point_int output "i" = [|2|]) "Fuse integer-average attribute rule";
  check (point_text output "label" = [|"acab"|])
    "Fuse weighted text concatenate rule";
  let cat = point_int_array output "cat" in
  check (cat.offsets = [|0;4|] && cat.values = [|7;8;9;10|])
    "Fuse scalar-to-array concatenate rule";
  List.iter (fun name -> check_point_group output name [0])
    ["least";"greatest";"any";"all";"majority"];
  let reduce method_ = Fuse_grid.fuse ~tolerance:0.
      ~attribute_rules:[ar "f" method_] source |> get_ok
      |> fun geometry -> point_float geometry "f" in
  check (reduce Fuse_reduce.Attribute_minimum = [|1.|]) "Fuse attribute minimum";
  check (reduce Fuse_reduce.Attribute_maximum = [|4.|]) "Fuse attribute maximum";
  check (reduce Fuse_reduce.Attribute_mode = [|2.|]) "Fuse attribute mode";
  check (reduce Fuse_reduce.Attribute_median = [|2.|]) "Fuse attribute median";
  check (reduce Fuse_reduce.Attribute_sum = [|9.|]) "Fuse attribute sum";
  check (reduce Fuse_reduce.Attribute_sum_squares = [|25.|])
    "Fuse attribute sum squares";
  check (near (reduce Fuse_reduce.Attribute_root_mean_square).(0) 2.5)
    "Fuse attribute RMS";
  let text method_ = Fuse_grid.fuse ~tolerance:0.
      ~attribute_rules:[ar "label" method_] source |> get_ok
      |> fun geometry -> point_text geometry "label" in
  check (text Fuse_reduce.Attribute_minimum = [|"a"|]) "Fuse text minimum";
  check (text Fuse_reduce.Attribute_maximum = [|"c"|]) "Fuse text maximum";
  check (text Fuse_reduce.Attribute_mode = [|"a"|]) "Fuse text mode";
  check (text Fuse_reduce.Attribute_median = [|"b"|]) "Fuse text median";
  check (text Fuse_reduce.Attribute_concatenate = [|"baac"|])
    "Fuse text concatenate";
  let row_source = Line_geometry.points [|(0.,0.,0.); (0.,0.,0.)|]
      |> add_point_int_array "rows" ~offsets:[|0;2;3|] ~values:[|1;2;3|] in
  let row_output = Fuse_grid.fuse ~tolerance:0.
      ~attribute_rules:[ar "rows" Fuse_reduce.Attribute_concatenate] row_source
      |> get_ok |> fun geometry -> point_int_array geometry "rows" in
  check (row_output.offsets = [|0;3|]
      && row_output.values = [|1;2;3|])
    "Fuse packed integer-array concatenate";
  let extreme = Line_geometry.points [|(0.,0.,0.); (0.,0.,0.)|]
      |> add_point_int "extreme" [|max_int;max_int|] in
  let extreme_average = Fuse_grid.fuse ~tolerance:0.
      ~attribute_rules:[ar "extreme" Fuse_reduce.Attribute_average] extreme |> get_ok in
  check (point_int extreme_average "extreme" = [|max_int|])
    "Fuse integer average overflowed equal maximum values";
  let overflow = Line_geometry.points [|(0.,0.,0.); (0.,0.,0.)|]
      |> add_point_int "overflow" [|max_int;1|] in
  expect_code "invalid_geometry" (Fuse_grid.fuse ~tolerance:0.
      ~attribute_rules:[ar "overflow" Fuse_reduce.Attribute_sum] overflow);
  let ignored_weight = Fuse_grid.fuse ~tolerance:0. ~attribute_rules:[
      ar ~weight:"missing" "f" Fuse_reduce.Attribute_average] source |> get_ok in
  check (point_float ignored_weight "f" = [|2.25|])
    "Fuse unweighted rule consulted an irrelevant weight attribute";
  expect_code "invalid_geometry" (Fuse_grid.fuse ~attribute_rules:[
      ar "[bad" Fuse_reduce.Attribute_minimum] (Line_geometry.points [|(0.,0.,0.)|]));
  expect_code "invalid_geometry" (Fuse_grid.fuse ~attribute_rules:[
      ar "f" Fuse_reduce.Attribute_weighted_sum] source);
  let pair = Line_geometry.points [|(0.,0.,0.); (2.,0.,0.); (10.,0.,0.)|]
      |> add_point_float "f" [|0.;10.;99.|]
      |> add_point_int "unchanged" [|3;4;5|]
      |> add_group "linked" [0]
      |> add_group "minority" [0] in
  let queries = Group.init ~owner:Group.Point ~name:"queries" 3
      (fun point -> point = 0)
  and targets = Group.init ~owner:Group.Point ~name:"targets" 3
      (fun point -> point = 1) in
  let modified = Fuse_grid.fuse ~selection:queries ~target_selection:targets
      ~modify_target:true ~fuse_points:false ~tolerance:3.
      ~attribute_rules:[ar "f" Fuse_reduce.Attribute_average]
      ~group_rules:[gr "linked" Fuse_reduce.Group_union;
        gr "minority" Fuse_reduce.Group_most_common] pair |> get_ok in
  check (point_float modified "f" = [|5.;5.;99.|]
      && point_int modified "unchanged" = [|3;4;5|])
    "Modify Target pattern-scoped attribute reduction";
  check_point_group modified "linked" [0;1];
  check_point_group modified "minority" [];
  let metadata = Fuse_grid.fuse ~selection:queries ~target_selection:targets
      ~modify_target:true ~tolerance:3.
      ~attribute_rules:[ar "*" Fuse_reduce.Attribute_average]
      ~snapped_group:"snapped_after_rules"
      ~snapped_destination_attribute:"destination_after_rules" pair |> get_ok in
  check (point_int metadata "destination_after_rules" = [|1;-1|])
    "Fuse output metadata was reduced by wildcard attribute rules";
  check_point_group metadata "snapped_after_rules" [0];
  let fixed_source = Line_geometry.points [|(0.1,0.,0.); (0.9,0.,0.)|]
      |> add_point_float "f" [|1.;2.|]
      |> add_point_int "cat" [|7;8|]
  and fixed_target = Line_geometry.points [|(0.,0.,0.); (1.,0.,0.)|]
      |> add_point_float "f" [|10.;20.|]
      |> add_point_int "cat" [|30;40|]
      |> add_point_float "tw" [|2.;3.|]
      |> fun geometry -> Geometry.with_group
          (Group.init ~owner:Group.Point ~name:"target_only" 2
            (fun point -> point = 1)) geometry |> get_string_ok in
  let fixed = Fuse_grid.fuse ~target:fixed_target ~using:Fuse_grid.Closest_target_point
      ~tolerance:0.2 ~fuse_points:false ~attribute_rules:[
        ar ~weight:"tw" "f" Fuse_reduce.Attribute_weighted_average;
        ar "cat" Fuse_reduce.Attribute_concatenate]
      ~group_rules:[gr "target_only" Fuse_reduce.Group_union] fixed_source |> get_ok in
  check (point_float fixed "f" = [|10.;20.|])
    "fixed-target attribute copying";
  let fixed_cat = point_int_array fixed "cat" in
  check (fixed_cat.offsets = [|0;1;2|] && fixed_cat.values = [|30;40|])
    "fixed-target concatenate shape";
  check_point_group fixed "target_only" [1];
  let fixed_same = Line_geometry.points [|(0.,0.,0.); (2.,0.,0.); (10.,0.,0.)|]
      |> add_point_float "f" [|0.;10.;99.|] in
  let fixed_same_output = Fuse_grid.fuse ~selection:queries
      ~target_selection:targets ~fuse_points:false ~tolerance:3.
      ~attribute_rules:[ar "f" Fuse_reduce.Attribute_average] fixed_same |> get_ok in
  check (point_float fixed_same_output "f" = [|10.;10.;99.|])
    "same-geometry fixed target copied rather than interpolated";
  let implicit_modify = Line_geometry.points [|(0.,0.,0.); (1.,0.,0.)|]
      |> add_point_float "f" [|0.;10.|]
      |> Fuse_grid.fuse ~tolerance:2. ~fuse_points:false
          ~attribute_rules:[ar "f" Fuse_reduce.Attribute_average] |> get_ok in
  check (point_float implicit_modify "f" = [|5.;5.|])
    "omitted target group did not imply Modify Target";
  let kept = Fuse_grid.fuse ~tolerance:0. ~keep_fused_points:true
      ~attribute_rules:[ar "f" Fuse_reduce.Attribute_average] source |> get_ok in
  check (Geometry.point_count kept = 4
      && point_float kept "f" = Array.make 4 2.25)
    "Keep Fused Points did not expand rule reduction";
  let grid_rule = Line_geometry.points [|(0.2,0.,0.); (0.3,0.,0.)|]
      |> add_point_float "f" [|1.;3.|]
      |> Fuse_grid.snap_to_grid ~fuse_points:true
          ~attribute_rules:[ar "f" Fuse_reduce.Attribute_average] |> get_ok in
  check (Geometry.point_count grid_rule = 1
      && point_float grid_rule "f" = [|2.|])
    "Grid Snap did not apply Fuse attribute rules";
  let incompatible_source = Line_geometry.points [|(0.1,0.,0.)|]
      |> add_point_float "value" [|1.|]
  and incompatible_target = Line_geometry.points [|(0.,0.,0.)|]
      |> add_point_int "value" [|1|] in
  expect_code "invalid_geometry" (Fuse_grid.fuse ~target:incompatible_target
      ~tolerance:1. ~fuse_points:false
      ~attribute_rules:[ar "value" Fuse_reduce.Attribute_average]
      incompatible_source);
  let fixed_position = Fuse_grid.fuse ~target:(Line_geometry.points [|(2.,0.,0.)|])
      ~tolerance:3. ~position:Fuse_reduce.Sum_position
      (Line_geometry.points [|(0.,0.,0.); (0.1,0.,0.)|]) |> get_ok in
  check (Geometry.point_count fixed_position = 1
      && (positions fixed_position).x = [|2.|])
    "fixed-target position heuristic counted duplicate queries"

let check_restricted_fuse () =
  let source = Line_geometry.points [|(0.,0.,0.); (0.0005,0.,0.); (0.0005,0.,0.)|] in
  let selection = Group.init ~owner:Group.Point ~name:"query" 3
      (fun point -> point < 2) in
  let output = Fuse_grid.fuse ~selection ~tolerance:0.001
      ~position:Fuse_reduce.Average_position source |> get_ok in
  let p = positions output in
  check (Geometry.point_count output = 2) "restricted Fuse cardinality";
  check (near p.x.(0) 0.00025 && near p.x.(1) 0.0005)
    "restricted Fuse position";
  let only_unselected = Group.init ~owner:Group.Point ~name:"query" 3
      (fun point -> point = 2) in
  let unchanged = Fuse_grid.fuse ~selection:only_unselected ~tolerance:1. source
      |> get_ok in
  check (unchanged == source) "restricted Fuse no-op identity"

let check_cluster_contract () =
  let chain = Line_geometry.points [|(0.,0.,0.); (0.009,0.,0.); (0.018,0.,0.)|]
      |> Fuse_grid.fuse ~tolerance:0.01 |> get_ok in
  let chain_positions = positions chain in
  check (Geometry.point_count chain = 2
      && near chain_positions.x.(0) 0.0045
      && near chain_positions.x.(1) 0.018)
    "Fuse lost representative-bounded non-transitive clustering";
  let signed_zero = Line_geometry.points [|(0.,0.,0.); (-0.,0.,0.)|]
      |> Fuse_grid.fuse ~tolerance:0. |> get_ok in
  check (Geometry.point_count signed_zero = 1)
    "Fuse exact hashing distinguished signed zero";
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  expect_code "cancelled" (Fuse_grid.fuse ~cancel:cancelled ~tolerance:0.
      (Line_geometry.points [|(0.,0.,0.); (0.,0.,0.)|]))

let check_rounding () =
  let source = Line_geometry.points [|(-0.6,0.,0.); (-0.5,0.,0.); (-0.4,0.,0.);
      (0.4,0.,0.); (0.5,0.,0.); (0.6,0.,0.)|] in
  let snap rounding = Fuse_grid.snap_to_grid ~rounding source |> get_ok |> positions in
  check ((snap Fuse_grid.Grid_nearest).x = [|-1.;0.;0.;0.;1.;1.|])
    "nearest grid rounding and tie policy";
  check ((snap Fuse_grid.Grid_down).x = [|-1.;-1.;-1.;0.;0.;0.|])
    "down grid rounding";
  check ((snap Fuse_grid.Grid_up).x = [|0.;0.;0.;1.;1.;1.|])
    "up grid rounding"

let check_offset_tolerance_selection () =
  let source = Line_geometry.points [|(0.2,0.,0.); (0.8,0.,0.); (0.2,0.,0.)|] in
  let selection = Group.init ~owner:Group.Point ~name:"selected" 3
      (fun point -> point < 2) in
  let output = Fuse_grid.snap_to_grid ~selection
      ~offset:(Rays_math.Vec3.create 0.25 0. 0.) ~max_distance:0.1
      ~snapped_group:"snapped" source |> get_ok in
  let p = positions output in
  check (near p.x.(0) 0.25 && near p.x.(1) 0.8 && near p.x.(2) 0.2)
    "grid offset, tolerance, and point restriction";
  (match Geometry.find_group ~owner:Group.Point "snapped" output with
   | Some group -> check (Group.cardinality group = 1 && Group.mem 0 group)
       "snapped output group"
   | None -> fail "missing snapped output group")

let check_fuse_and_payload () =
  let source = Line_geometry.points [|(0.2,0.,0.); (0.3,0.,0.)|] in
  let weight = Attribute.create_owned ~owner:Attribute.Point ~name:"weight"
      (Attribute.Float [|2.;4.|]) |> get_string_ok in
  let normals = Packed.Float3.Private.of_owned_exn ~x:[|1.;1.|]
      ~y:[|0.;0.|] ~z:[|0.;0.|] in
  let normal = Attribute.create_key_owned (Attribute.normal ~owner:Attribute.Point)
      normals |> get_string_ok in
  let source = source |> add_attribute weight |> add_attribute normal in
  let output = Fuse_grid.snap_to_grid ~fuse_points:true
      ~attributes:Fuse_reduce.Average_numeric ~snapped_group:"snapped" source |> get_ok in
  check (Geometry.point_count output = 1) "grid post-snap fusion cardinality";
  check ((point_float output "weight").(0) = 3.)
    "grid post-snap numeric reduction";
  check (Geometry.find_attribute ~owner:Attribute.Point "N" output = None)
    "grid position change invalidated normals";
  (match Geometry.find_group ~owner:Group.Point "snapped" output with
   | Some group -> check (Group.cardinality group = 1)
       "grid post-fuse snapped group"
   | None -> fail "grid post-fuse snapped group missing");
  let coincident = Line_geometry.points [|(0.,0.,0.); (0.,0.,0.)|]
      |> Fuse_grid.snap_to_grid ~fuse_points:true |> get_ok in
  check (Geometry.point_count coincident = 1)
    "grid fusion skipped already-snapped duplicates"

let check_noop_and_validation () =
  let source = Line_geometry.points [|(0.,0.,0.)|] in
  check (Fuse_grid.snap_to_grid source |> get_ok == source)
    "exact grid input did not preserve identity";
  let wrong = Group.init ~owner:Group.Primitive ~name:"wrong" 0
      (fun _ -> false) in
  expect_code "invalid_geometry" (Fuse_grid.snap_to_grid ~selection:wrong source);
  expect_code "invalid_geometry" (Fuse_grid.snap_to_grid
      ~spacing:(Rays_math.Vec3.create 1. 0. 1.) source);
  expect_code "invalid_geometry" (Fuse_grid.snap_to_grid
      ~offset:(Rays_math.Vec3.create 1.1 0. 0.) source);
  expect_code "invalid_geometry" (Fuse_grid.snap_to_grid ~max_distance:(-1.) source);
  expect_code "invalid_geometry" (Fuse_grid.snap_to_grid ~snapped_group:"" source);
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  expect_code "cancelled" (Fuse_grid.snap_to_grid ~cancel:cancelled
      (Plane_generators.grid ~columns:10 ~rows:10 ~size:1. () |> get_ok))

let check_parallel_exact () =
  let source = Plane_generators.grid ~columns:500 ~rows:300 ~size:20. () |> get_ok
      |> Deform.noise_displace ~seed:81 ~amplitude:0.37 ~frequency:0.29 |> get_ok in
  let run domains = Parallel.run ~domains (fun () ->
    Fuse_grid.snap_to_grid ~grain:1024 ~spacing:(Rays_math.Vec3.create 0.03125 0.03125 0.03125)
      ~offset:(Rays_math.Vec3.create 0.25 0.5 0.75) ~snapped_group:"snapped" source
      |> get_ok) in
  let one = run 1 and many = run 4 in
  let a = positions one and b = positions many in
  check (a.x = b.x && a.y = b.y && a.z = b.z)
    "one/four-domain grid positions differ";
  (match Geometry.find_group ~owner:Group.Point "snapped" one,
      Geometry.find_group ~owner:Group.Point "snapped" many with
   | Some a, Some b ->
       check (Group.cardinality a = Group.cardinality b)
         "one/four-domain snapped cardinality differs";
       for point = 0 to Geometry.point_count one - 1 do
         check (Group.mem point a = Group.mem point b)
           "one/four-domain snapped membership differs"
       done
   | _ -> fail "parallel snapped groups missing")

let run () =
  check_restricted_fuse ();
  check_cluster_contract ();
  check_target_policies ();
  check_specified_targets ();
  check_radius_and_match ();
  check_separate_same_geometry_groups ();
  check_target_scale_and_validation ();
  check_target_parallel_exact ();
  check_position_reductions ();
  check_modify_and_keep_target ();
  check_cleanup ();
  check_attribute_and_group_rules ();
  check_rounding ();
  check_offset_tolerance_selection ();
  check_fuse_and_payload ();
  check_noop_and_validation ();
  check_parallel_exact ();
  print_endline "fuse/grid snap tests passed"
