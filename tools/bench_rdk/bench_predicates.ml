open Bench_util
open Rdk

let count = integer_env "RAYS_PREDICATE_BENCH_COUNT" 5_000_000
let repeats = integer_env "RAYS_PREDICATE_BENCH_REPEATS" 5
let sink = ref 0

let sign_code = function
  | Predicates.Negative -> -1
  | Predicates.Zero -> 0
  | Predicates.Positive -> 1

let segment_code = function
  | Predicates.Segments_disjoint -> 0
  | Predicates.Segments_point -> 1
  | Predicates.Segments_overlap -> 2
  | Predicates.Segments_degenerate -> 3

let measure name iterations operation =
  let times = Array.make repeats 0. and allocations = Array.make repeats 0.
  and promoted = Array.make repeats 0. and major = Array.make repeats 0. in
  for repeat = 0 to repeats - 1 do
    Gc.full_major ();
    let before = Gc.quick_stat () and allocated_before = Gc.allocated_bytes ()
    and started = Unix.gettimeofday () in
    let local = ref 0 in
    for index = 0 to iterations - 1 do
      local := !local + operation index
    done;
    sink := !sink lxor !local;
    times.(repeat) <- Unix.gettimeofday () -. started;
    allocations.(repeat) <- Gc.allocated_bytes () -. allocated_before;
    let after = Gc.quick_stat () in
    promoted.(repeat) <- (after.promoted_words -. before.promoted_words) *. 8.;
    major.(repeat) <- (after.major_words -. before.major_words) *. 8.
  done;
  Printf.printf "%s,%d,%d,%.6f,%.3f,%.0f,%.0f\n%!" name iterations repeats
    (median times) (median allocations /. float iterations)
    (median promoted) (median major)

let run () =
  Printf.printf
    "benchmark,calls,repeats,median_seconds,allocated_bytes_per_call,promoted_bytes,major_bytes\n%!";
  let fast_x = Array.init 65_536 (fun index -> float_of_int index *. 0.125)
  and fast_y = Array.init 65_536 (fun index ->
      match index land 3 with 0 -> 0. | 1 -> 0.25 | 2 -> 1.75 | _ -> 0.5)
  and fast_z = Array.init 65_536 (fun index ->
      match index land 3 with 0 | 1 -> 0. | 2 -> 1. | _ -> 0.) in
  measure "orient2d_packed_filtered" count (fun index ->
    let a = index land 65_532 in
    Predicates.orient2d_packed ~x:fast_x ~y:fast_y a (a + 1) (a + 2)
    |> sign_code);
  measure "orient3d_packed_filtered" count (fun index ->
    let a = index land 65_532 in
    Predicates.orient3d_packed ~x:fast_x ~y:fast_y ~z:fast_z
      a (a + 1) (a + 2) (a + 3) |> sign_code);
  let segment_x = [|0.;2.;0.; 0.5;0.5; 2.;3.|]
  and segment_y = [|0.;0.;2.; 0.5;0.5; 2.;3.|]
  and segment_z = [|0.;0.;0.; -1.;1.; -1.;1.|] in
  measure "segment_triangle_packed_filtered" count (fun index ->
    let p, q = if index land 1 = 0 then 3, 4 else 5, 6 in
    Predicates.View.segment_triangle_code_packed
      ~x:segment_x ~y:segment_y ~z:segment_z
      ~segment_start:p ~segment_end:q ~triangle_a:0 ~triangle_b:1
      ~triangle_c:2);
  let contact_count = max 1 (count / 100) in
  let contact_x = [|-1.;1.;0.;0.|]
  and contact_y = [|0.;0.;-1.;1.|]
  and contact_z = [|0.;0.;0.;0.|] in
  measure "segment_segment_packed_exact_contact" contact_count (fun _ ->
    Predicates.segment_segment_packed
      ~x:contact_x ~y:contact_y ~z:contact_z
      ~left_a:0 ~left_b:1 ~right_a:2 ~right_b:3 |> segment_code);
  let triangle_x = [|0.;2.;0.; 0.2;1.2;0.3|]
  and triangle_y = [|0.;0.;2.; 0.2;0.3;1.2|]
  and triangle_z = [|0.;0.;0.; -1.1;1.3;-0.9|]
  and triangle_output = Array.make 4 0 in
  measure "triangle_triangle_packed_filtered" count (fun _ ->
    Predicates.View.triangle_triangle_features_into
      ~x:triangle_x ~y:triangle_y ~z:triangle_z
      ~left_a:0 ~left_b:1 ~left_c:2 ~right_a:3 ~right_b:4 ~right_c:5
      triangle_output);
  let fallback_count = max 1 (count / 1_000) in
  let edge_x = [|0.;2.;0.; 0.5;0.5;0.5|]
  and edge_y = [|0.;0.;2.; -0.5;1.5;1.5|]
  and edge_z = [|0.;0.;0.; -1.;1.;-1.|] in
  measure "triangle_triangle_edge_edge_fallback" fallback_count (fun _ ->
    Predicates.View.triangle_triangle_features_into
      ~x:edge_x ~y:edge_y ~z:edge_z
      ~left_a:0 ~left_b:1 ~left_c:2 ~right_a:3 ~right_b:4 ~right_c:5
      triangle_output);
  let n = Float.ldexp 1. 52 in
  let exact_x = [|0.; n; n -. 1.|]
  and exact_y = [|0.; n -. 1.; n -. 2.|] in
  measure "orient2d_packed_exact_fallback" fallback_count (fun _ ->
    Predicates.orient2d_packed ~x:exact_x ~y:exact_y 0 1 2 |> sign_code);
  let smallest = Int64.float_of_bits 1L
  and minimum_normal = Float.ldexp 1. (-1022) in
  let underflow_x = [|minimum_normal;0.;0.;0.|]
  and underflow_y = [|0.;smallest;0.;0.|]
  and underflow_z = [|0.;0.;smallest;0.|] in
  measure "orient3d_packed_exact_underflow" fallback_count (fun _ ->
    Predicates.orient3d_packed ~x:underflow_x ~y:underflow_y
      ~z:underflow_z 0 1 2 3 |> sign_code);
  let module Point = struct
    include Rdk_geom.Implicit_point
  end in
  let construction_x = [|
    0.; 2.; 1.; 1.; 1.; 1.;
    1.; 1.; 1.; 0.; 1.; 0.; 0.; 1.; 0.; 1.
  |]
  and construction_y = [|
    0.; 0.; -1.; 1.; 0.; 0.;
    0.; 1.; 0.; 2.; 2.; 2.; 0.; 0.; 1.; 2.
  |]
  and construction_z = [|
    0.; 0.; -1.; -1.; 1.; 0.;
    0.; 0.; 1.; 0.; 0.; 1.; 3.; 3.; 3.; 3.
  |] in
  let construction_source = Point.source
      ~x:construction_x ~y:construction_y ~z:construction_z
      |> Result.get_ok in
  let explicit_lpi = Point.explicit construction_source 5 |> Result.get_ok
  and explicit_tpi = Point.explicit construction_source 15 |> Result.get_ok in
  measure "line_plane_exact_construction" fallback_count (fun _ ->
    match Point.line_plane construction_source
        ~line_start:0 ~line_end:1 ~plane_a:2 ~plane_b:3 ~plane_c:4 with
    | Error _ -> min_int
    | Ok point -> Point.compare_x point explicit_lpi);
  let tpi_count = max 1 (fallback_count / 10) in
  measure "triple_plane_exact_construction" tpi_count (fun _ ->
    match Point.triple_plane construction_source
        ~first_a:6 ~first_b:7 ~first_c:8
        ~second_a:9 ~second_b:10 ~second_c:11
        ~third_a:12 ~third_b:13 ~third_c:14 with
    | Error _ -> min_int
    | Ok point -> Point.compare_z point explicit_tpi);
  let lpi = Point.line_plane construction_source
      ~line_start:0 ~line_end:1 ~plane_a:2 ~plane_b:3 ~plane_c:4
      |> Result.get_ok
  and tpi = Point.triple_plane construction_source
      ~first_a:6 ~first_b:7 ~first_c:8
      ~second_a:9 ~second_b:10 ~second_c:11
      ~third_a:12 ~third_b:13 ~third_c:14 |> Result.get_ok
  and origin = Point.explicit construction_source 0 |> Result.get_ok
  and x_axis = Point.explicit construction_source 1 |> Result.get_ok
  and plane_corner = Point.explicit construction_source 3 |> Result.get_ok
  and circle_point = Point.explicit construction_source 2 |> Result.get_ok in
  measure "implicit_compare_x_filtered" count (fun _ ->
    Point.compare_x origin lpi);
  measure "implicit_orient2d_filtered" count (fun _ ->
    Point.orient2d_xy lpi tpi origin |> sign_code);
  measure "implicit_orient3d_filtered" count (fun _ ->
    Point.orient3d origin x_axis plane_corner tpi |> sign_code);
  measure "implicit_incircle_filtered" count (fun _ ->
    Point.incircle_xy origin x_axis plane_corner tpi |> sign_code);
  measure "implicit_incircle_exact_fallback" fallback_count (fun _ ->
    Point.incircle_xy origin x_axis plane_corner circle_point |> sign_code);
  let barycentric_code (a, b, c) =
    Int64.to_int (Int64.bits_of_float (a +. (3. *. b) +. (7. *. c))) in
  measure "implicit_source_barycentric_exact_arena" fallback_count (fun _ ->
    Point.barycentric_source_triangle construction_source
      ~a:0 ~b:1 ~c:7 lpi |> barycentric_code);
  if !sink = min_int then Printf.eprintf "unreachable\n"
