open Bench_util
open Rays
open Rdk

let size = integer_env "RAYS_CENTROID_SIZE" 1_000_000
let repeats = integer_env "RAYS_CENTROID_REPEATS" 5
let domains = integer_env "RAYS_BENCH_DOMAINS" (Parallel.recommended_domains ())
let grain = integer_env "RAYS_CENTROID_GRAIN" 16_384

let point_cloud () = Line_geometry.points (Array.init size (fun point ->
  let x = float_of_int (point mod 10_007) /. 10_007.
  and y = float_of_int ((point * 97) mod 10_009) /. 10_009.
  and z = float_of_int ((point * 193) mod 10_037) /. 10_037. in
  x,y,z))

let grid () =
  let side = max 1 (int_of_float (sqrt (float_of_int (size / 2)))) in
  let geometry = Plane_generators.grid ~connectivity:Plane_generators.Grid_triangles ~columns:side
      ~rows:side ~size:100. () |> get in
  let piece = Attribute.create_owned ~owner:Attribute.Primitive ~name:"piece"
      (Attribute.Int (Array.init (Geometry.primitive_count geometry)
        (fun primitive -> primitive / 2))) |> Result.get_ok in
  Geometry.with_attribute piece geometry |> Result.get_ok

let hash geometry =
  let p = Packed.Float3.Private.view (Geometry.positions geometry) in
  let value = ref 17 in
  let mix_float item =
    value := ((!value * 65_599) lxor
      Int64.to_int (Int64.bits_of_float item)) land max_int in
  Array.iter mix_float p.x; Array.iter mix_float p.y; Array.iter mix_float p.z;
  !value

let measure name operation input =
  let times = Array.make repeats 0. and allocations = Array.make repeats 0.
  and promoted = Array.make repeats 0. and major = Array.make repeats 0.
  and output_points = ref 0 and output_hash = ref 0 in
  Parallel.run ~domains (fun () ->
    for repeat = 0 to repeats - 1 do
      Gc.full_major ();
      let before = Gc.quick_stat () and allocated = Gc.allocated_bytes ()
      and started = Unix.gettimeofday () in
      let output = operation input |> get in
      times.(repeat) <- Unix.gettimeofday () -. started;
      allocations.(repeat) <- Gc.allocated_bytes () -. allocated;
      let after = Gc.quick_stat () in
      promoted.(repeat) <- (after.promoted_words -. before.promoted_words) *. 8.;
      major.(repeat) <- (after.major_words -. before.major_words) *. 8.;
      output_points := Geometry.point_count output; output_hash := hash output
    done);
  Printf.printf "%s,%d,%d,%d,%d,%.6f,%.0f,%.0f,%.0f,%d,%d\n%!"
    name (Geometry.point_count input) domains grain repeats (median times)
    (median allocations) (median promoted) (median major) !output_points !output_hash

let run () =
  Printf.printf "case,input_points,domains,grain,repeats,seconds,current_domain_allocated_bytes,promoted_bytes,major_bytes,output_points,hash\n";
  let points = point_cloud () and mesh = grid () in
  measure "detail_point_mass" (Curve_topology.extract_centroid ~grain) points;
  measure "detail_bounds"
    (Curve_topology.extract_centroid ~grain ~method_:Curve_topology.Centroid_bounding_box) points;
  measure "primitive_bounds" (Curve_topology.extract_centroid ~grain
    ~run_over:Curve_topology.Centroid_primitives ~method_:Curve_topology.Centroid_bounding_box) mesh;
  measure "piece_point_mass" (Curve_topology.extract_centroid ~grain
    ~run_over:(Curve_topology.Centroid_pieces {
      owner=Curve_topology.Centroid_piece_primitives; attribute="piece"})) mesh
