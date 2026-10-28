open Bench_util
open Rdk

let points = integer_env "RAYS_DELAUNAY_POINTS" 100_000
let repeats = integer_env "RAYS_DELAUNAY_REPEATS" 3
let constraint_segments = integer_env "RAYS_CONSTRAINT_SEGMENTS" 100_000
let crossing_axis_segments = integer_env "RAYS_CROSSING_AXIS_SEGMENTS" 256
let refinement_points = integer_env "RAYS_REFINEMENT_POINTS" 100_000
let refinement_constraints = integer_env "RAYS_REFINEMENT_CONSTRAINTS" 10_000
let regularization_points = integer_env "RAYS_REGULARIZATION_POINTS" 10_000
let domains = integer_env "RAYS_DELAUNAY_DOMAINS"
    (Parallel.recommended_domains ())

let coordinate point salt =
  let state = Rays_math.Rand.seed ((point * 2) + salt) in
  let value,_ = Rays_math.Rand.float state in
  value

let x = Array.init points (fun point -> coordinate point 17)
and y = Array.init points (fun point -> coordinate point 53)

let hash value =
  let result = ref 17 in
  for triangle = 0 to Delaunay2.triangle_count value - 1 do
    for local = 0 to 2 do
      result := ((!result * 65_599)
          lxor Delaunay2.triangle_point value triangle local) land max_int
    done
  done;
  !result

let hash_cdt value =
  let result = ref 17 in
  for triangle = 0 to Planar_cdt.triangle_count value - 1 do
    for local = 0 to 2 do
      result := ((!result * 65_599)
          lxor Planar_cdt.triangle_point value triangle local) land max_int
    done
  done;
  !result

let hash_ints values =
  Array.fold_left (fun result value ->
      ((result * 65_599) lxor value) land max_int) 17 values

let hash_geometry geometry =
  let topology = Geometry.topology geometry |> Topology.Private.view in
  hash_ints topology.vertex_points

let measure label run cardinality hash =
  let times = Array.make repeats 0. and allocated = Array.make repeats 0.
  and promoted = Array.make repeats 0. and major = Array.make repeats 0.
  and expected_hash = ref (-1) and size = ref 0 in
  for repeat = 0 to repeats - 1 do
    Gc.full_major ();
    let before = Gc.quick_stat () and bytes = Gc.allocated_bytes ()
    and started = Unix.gettimeofday () in
    let value = run () in
    times.(repeat) <- Unix.gettimeofday () -. started;
    allocated.(repeat) <- Gc.allocated_bytes () -. bytes;
    let after = Gc.quick_stat () in
    promoted.(repeat) <- (after.promoted_words -. before.promoted_words) *. 8.;
    major.(repeat) <- (after.major_words -. before.major_words) *. 8.;
    let current_hash = hash value in
    if !expected_hash >= 0 && current_hash <> !expected_hash then
      failwith (label ^ " benchmark output is nondeterministic");
    expected_hash := current_hash; size := cardinality value
  done;
  Printf.printf "%s,%d,%d,%.6f,%.0f,%.0f,%.0f,%d\n" label !size repeats
    (median times) (median allocated) (median promoted) (median major)
    !expected_hash

let run_benchmarks () =
  let times = Array.make repeats 0. and allocated = Array.make repeats 0.
  and promoted = Array.make repeats 0. and major = Array.make repeats 0.
  and expected_hash = ref (-1) and triangle_count = ref 0 in
  for repeat = 0 to repeats - 1 do
    Gc.full_major ();
    let before = Gc.quick_stat () and bytes = Gc.allocated_bytes ()
    and started = Unix.gettimeofday () in
    let value = match Delaunay2.build ~seed:0L ~x ~y () with
      | Ok value -> value | Error message -> failwith message in
    times.(repeat) <- Unix.gettimeofday () -. started;
    allocated.(repeat) <- Gc.allocated_bytes () -. bytes;
    let after = Gc.quick_stat () in
    promoted.(repeat) <- (after.promoted_words -. before.promoted_words) *. 8.;
    major.(repeat) <- (after.major_words -. before.major_words) *. 8.;
    let current_hash = hash value in
    if !expected_hash >= 0 && current_hash <> !expected_hash then
      failwith "Delaunay benchmark output is nondeterministic";
    expected_hash := current_hash;
    triangle_count := Delaunay2.triangle_count value
  done;
  Printf.printf
    "points,triangles,repeats,seconds,current_domain_allocated_bytes,promoted_bytes,major_bytes,hash\n%d,%d,%d,%.6f,%.0f,%.0f,%.0f,%d\n"
    points !triangle_count repeats (median times) (median allocated)
    (median promoted) (median major) !expected_hash;
  let seed = match Delaunay2.build ~seed:0L ~x ~y () with
    | Ok value -> value | Error message -> failwith message in
  let seed_view = Delaunay2.Private.view seed in
  let repair ?workspace triangle_points =
    Planar_cdt.build ?workspace ~point_count:points
      ~orient:(fun a b c -> Predicates.orient2d_packed ~x ~y a b c)
      ~incircle:(fun a b c d -> Predicates.incircle_packed ~x ~y a b c d)
      ~triangle_points ~constraint_points:[||] ()
    |> function Ok value -> value | Error message -> failwith message in
  measure "cdt_repair_rebuild"
    (fun () -> repair seed_view.triangle_points)
    Planar_cdt.triangle_count hash_cdt;
  let repair_workspace = Planar_cdt.Private.create_workspace
      ~point_capacity:points ~triangle_capacity:(Delaunay2.triangle_count seed) () in
  let repair_snapshot = ref (repair ~workspace:repair_workspace
      seed_view.triangle_points) in
  measure "cdt_repair_incremental"
    (fun () ->
      let value = repair ~workspace:repair_workspace
          (Planar_cdt.Private.view !repair_snapshot).triangle_points in
      repair_snapshot := value;
      value)
    Planar_cdt.triangle_count hash_cdt;
  let first_constraint = 0 and second_constraint = 1 in
  let times = Array.make repeats 0. and allocated = Array.make repeats 0.
  and promoted = Array.make repeats 0. and major = Array.make repeats 0.
  and expected_hash = ref (-1) in
  for repeat = 0 to repeats - 1 do
    Gc.full_major ();
    let before = Gc.quick_stat () and bytes = Gc.allocated_bytes ()
    and started = Unix.gettimeofday () in
    let value = Planar_cdt.build ~point_count:points
        ~orient:(fun a b c -> Predicates.orient2d_packed ~x ~y a b c)
        ~incircle:(fun a b c d -> Predicates.incircle_packed ~x ~y a b c d)
        ~triangle_points:seed_view.triangle_points
        ~constraint_points:[|first_constraint;second_constraint|] () |> function
      | Ok value -> value | Error message -> failwith message in
    times.(repeat) <- Unix.gettimeofday () -. started;
    allocated.(repeat) <- Gc.allocated_bytes () -. bytes;
    let after = Gc.quick_stat () in
    promoted.(repeat) <- (after.promoted_words -. before.promoted_words) *. 8.;
    major.(repeat) <- (after.major_words -. before.major_words) *. 8.;
    let current_hash = hash_cdt value in
    if !expected_hash >= 0 && current_hash <> !expected_hash then
      failwith "Planar CDT benchmark output is nondeterministic";
    expected_hash := current_hash
  done;
  Printf.printf
    "constraint_recovery,%d,%d,%.6f,%.0f,%.0f,%.0f,%d\n"
    points repeats (median times) (median allocated) (median promoted)
    (median major) !expected_hash;
  measure "hull_flood_unblocked"
    (fun () -> Planar_cdt.build ~point_count:points
        ~orient:(fun a b c -> Predicates.orient2d_packed ~x ~y a b c)
        ~incircle:(fun a b c d -> Predicates.incircle_packed ~x ~y a b c d)
        ~triangle_points:seed_view.triangle_points
        ~flood_from_hull_boundary:true ~constraint_points:[||] ()
      |> function Ok value -> value | Error message -> failwith message)
    Planar_cdt.triangle_count hash_cdt;
  measure "polygon_winding_empty"
    (fun () -> Planar_cdt.build ~point_count:points
        ~orient:(fun a b c -> Predicates.orient2d_packed ~x ~y a b c)
        ~incircle:(fun a b c d -> Predicates.incircle_packed ~x ~y a b c d)
        ~triangle_points:seed_view.triangle_points
        ~constraint_points:[||] ~constraint_winding:[||]
        ~remove_outside_constraint_polygons:true ()
      |> function Ok value -> value | Error message -> failwith message)
    Planar_cdt.triangle_count hash_cdt;
  let source_positions = Packed.Float3.Private.of_owned_exn
      ~x:(Array.copy x) ~y:(Array.copy y) ~z:(Array.make points 0.) in
  if points >= 3 then begin
    let constraint_only_topology = Topology.polygons_owned ~point_count:points
        ~vertex_points:[|0;1;2|] ~primitive_offsets:[|0;3|]
        |> function Ok value -> value | Error message -> failwith message in
    let constraint_only_source = Geometry.create ~positions:source_positions
        ~topology:constraint_only_topology ()
        |> function Ok value -> value | Error message -> failwith message in
    let constraint_only_group = Group.init ~owner:Group.Primitive
        ~name:"constraint" 1 (fun _ -> true) in
    measure "constraint_only_adapter"
      (fun () -> Triangulate2d.run ~projection:Triangulate2d.Plane_xy
          ~constraint_primitives:constraint_only_group
          ~ignore_non_constraint_points:true constraint_only_source
        |> function Ok value -> value
          | Error message -> failwith (Error.to_string message))
      Geometry.primitive_count hash_geometry
  end;
  if points >= 4 then begin
    let duplicate_source = Line_geometry.points (Array.init points (fun point ->
        match point land 3 with
        | 0 -> 0.,0.,float_of_int point
        | 1 -> 1.,0.,float_of_int point
        | 2 -> 1.,1.,float_of_int point
        | _ -> 0.,1.,float_of_int point)) in
    measure "duplicate_removal_adapter"
      (fun () -> Triangulate2d.run ~projection:Triangulate2d.Plane_xy
          ~remove_duplicate_points:true duplicate_source
        |> function Ok value -> value
          | Error message -> failwith (Error.to_string message))
      Geometry.point_count hash_geometry
  end;
  let projected_positions_source = Geometry.create ~positions:source_positions
      ~topology:(Topology.empty ~point_count:points) ()
      |> function Ok value -> value | Error message -> failwith message in
  measure "projected_positions_adapter"
    (fun () -> Triangulate2d.run ~projection:Triangulate2d.Plane_xy
        ~restore_original_point_positions:false
        projected_positions_source
      |> function Ok value -> value
        | Error message -> failwith (Error.to_string message))
    Geometry.point_count hash_geometry;
  let refinement_source = Line_geometry.points
      [|0.,0.,0.; 1.,0.,0.; 1.,1.,0.; 0.,1.,0.|] in
  let target_edge_length = 1.8 /. sqrt (float_of_int refinement_points) in
  measure "quality_refinement_adapter"
    (fun () -> Triangulate2d.run ~projection:Triangulate2d.Plane_xy
        ~refine:true ~minimum_angle:1e-6 ~target_edge_length
        ~maximum_new_points:refinement_points refinement_source
      |> function Ok value -> value
        | Error message -> failwith (Error.to_string message))
    Geometry.point_count hash_geometry;
  let regularization_target = 1.8 /. sqrt (float_of_int regularization_points) in
  let regularization_run steps () =
    Triangulate2d.run ~projection:Triangulate2d.Plane_xy
      ~refine:true ~minimum_angle:1e-6
      ~target_edge_length:regularization_target
      ~maximum_new_points:regularization_points ~regularization_steps:steps
      refinement_source
    |> function Ok value -> value
      | Error message -> failwith (Error.to_string message) in
  measure "regularization_seed_adapter" (regularization_run 0)
    Geometry.point_count hash_geometry;
  measure "regularization_two_steps_adapter" (regularization_run 2)
    Geometry.point_count hash_geometry;
  let constrained_positions = Packed.Float3.Private.of_owned_exn
      ~x:(Array.init refinement_constraints (fun point ->
        cos ((2. *. Float.pi) *. float_of_int point
          /. float_of_int refinement_constraints)))
      ~y:(Array.init refinement_constraints (fun point ->
        sin ((2. *. Float.pi) *. float_of_int point
          /. float_of_int refinement_constraints)))
      ~z:(Array.make refinement_constraints 0.) in
  let constrained_topology = Topology.polygons_owned
      ~point_count:refinement_constraints
      ~vertex_points:(Array.init refinement_constraints Fun.id)
      ~primitive_offsets:[|0;refinement_constraints|]
      |> function Ok value -> value | Error message -> failwith message in
  let constrained_source = Geometry.create ~positions:constrained_positions
      ~topology:constrained_topology ()
      |> function Ok value -> value | Error message -> failwith message in
  let constrained_group = Group.init ~owner:Group.Primitive
      ~name:"boundary" 1 (fun _ -> true) in
  measure "quality_refinement_constrained_adapter"
    (fun () -> Triangulate2d.run ~projection:Triangulate2d.Plane_xy
        ~constraint_primitives:constrained_group
        ~remove_outside_constraint_polygons:true ~refine:true
        ~minimum_angle:1e-6
        ~maximum_area:(Float.pi /. (2. *. float_of_int refinement_constraints))
        ~allow_constraint_splitting:false ~maximum_new_points:256
        constrained_source
      |> function Ok value -> value
        | Error message -> failwith (Error.to_string message))
    Geometry.point_count hash_geometry;
  let source_topology = Topology.polygons_owned ~point_count:points
      ~vertex_points:(Array.copy seed_view.triangle_points)
      ~primitive_offsets:(Array.init (Delaunay2.triangle_count seed + 1)
        (fun primitive -> primitive * 3))
      |> function Ok value -> value | Error message -> failwith message in
  let silhouette_source = Geometry.create ~positions:source_positions
      ~topology:source_topology ()
      |> function Ok value -> value | Error message -> failwith message in
  measure "projected_silhouette_adapter"
    (fun () -> Triangulate2d.run ~projection:Triangulate2d.Plane_xy
      ~silhouette_constraints:true silhouette_source
      |> function Ok value -> value
        | Error message -> failwith (Error.to_string message))
    Geometry.primitive_count hash_geometry;
  let keep_source =
    let vertex_payload = Attribute.create_owned ~owner:Attribute.Vertex
        ~name:"vertex_payload"
        (Attribute.Float (Array.init (Geometry.vertex_count silhouette_source)
          float_of_int))
        |> function Ok value -> value | Error message -> failwith message
    and primitive_payload = Attribute.create_owned ~owner:Attribute.Primitive
        ~name:"primitive_payload"
        (Attribute.Int (Array.init (Geometry.primitive_count silhouette_source)
          Fun.id))
        |> function Ok value -> value | Error message -> failwith message in
    silhouette_source |> Geometry.with_attribute vertex_payload
      |> function Error message -> failwith message | Ok value -> value
      |> Geometry.with_attribute primitive_payload
      |> function Error message -> failwith message | Ok value -> value in
  measure "keep_primitives_adapter"
    (fun () -> Triangulate2d.run ~projection:Triangulate2d.Plane_xy
      ~keep_primitives:true keep_source
      |> function Ok value -> value
        | Error message -> failwith (Error.to_string message))
    Geometry.primitive_count hash_geometry;
  let disjoint_x = Array.init (constraint_segments * 2) (fun point ->
      if point land 1 = 0 then 0. else 0.5)
  and disjoint_y = Array.init (constraint_segments * 2) (fun point ->
      float_of_int (point / 2))
  and disjoint_edges = Array.init (constraint_segments * 2) Fun.id in
  measure "constraint_arrangement_disjoint"
    (fun () -> Planar_constraints.build ~split_crossings:true
        ~x:disjoint_x ~y:disjoint_y ~segment_points:disjoint_edges ()
      |> function Ok value -> value | Error message -> failwith message)
    (fun value -> Array.length
        (Planar_constraints.Private.view value).constraint_points / 2)
    (fun value -> hash_ints
        (Planar_constraints.Private.view value).constraint_points);
  let embedded_count = max 2 constraint_segments in
  let embedded_x = Array.init embedded_count float_of_int
  and embedded_y = Array.make embedded_count 0.
  and embedded_points = Array.init embedded_count Fun.id in
  measure "constraint_arrangement_embedded_chain"
    (fun () -> Planar_constraints.build ~split_crossings:true
        ~x:embedded_x ~y:embedded_y
        ~segment_points:[|0;embedded_count - 1|] ~embedded_points ()
      |> function Ok value -> value | Error message -> failwith message)
    (fun value -> Array.length
        (Planar_constraints.Private.view value).constraint_points / 2)
    (fun value -> hash_ints
        (Planar_constraints.Private.view value).constraint_points);
  let axis = crossing_axis_segments in
  let crossing_x = Array.make (axis * 4) 0.
  and crossing_y = Array.make (axis * 4) 0.
  and crossing_edges = Array.init (axis * 4) Fun.id in
  for segment = 0 to axis - 1 do
    let coordinate = float_of_int segment in
    crossing_x.(segment * 2) <- coordinate;
    crossing_x.((segment * 2) + 1) <- coordinate;
    crossing_y.(segment * 2) <- -1.;
    crossing_y.((segment * 2) + 1) <- float_of_int axis;
    let offset = (axis * 2) + (segment * 2) in
    crossing_x.(offset) <- -1.;
    crossing_x.(offset + 1) <- float_of_int axis;
    crossing_y.(offset) <- coordinate;
    crossing_y.(offset + 1) <- coordinate
  done;
  measure "constraint_arrangement_grid"
    (fun () -> Planar_constraints.build ~split_crossings:true
        ~x:crossing_x ~y:crossing_y ~segment_points:crossing_edges ()
      |> function Ok value -> value | Error message -> failwith message)
    Planar_constraints.split_point_count
    (fun value -> hash_ints
        (Planar_constraints.Private.view value).constraint_points)

let run () =
  Printf.eprintf "bench_delaunay2: domains=%d ocaml=%s\n%!" domains Sys.ocaml_version;
  Parallel.run ~domains run_benchmarks
