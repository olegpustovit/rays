open Bench_util
open Rays
open Rdk

let interior_points = integer_env "RAYS_HULL_INTERIOR_POINTS" 1_000_000
let surface_points = max 4 (integer_env "RAYS_HULL_SURFACE_POINTS" 4_096)
let planar_points = integer_env "RAYS_HULL_PLANAR_POINTS" 1_000_000
let repeats = integer_env "RAYS_HULL_REPEATS" 5
let domains = integer_env "RAYS_BENCH_DOMAINS" (Parallel.recommended_domains ())
let grain = integer_env "RAYS_HULL_GRAIN" 16_384

let geometry values = Line_geometry.points values

let cube_interior count =
  let corners = [|
    -1.,-1.,-1.; 1.,-1.,-1.; 1.,1.,-1.; -1.,1.,-1.;
    -1.,-1.,1.; 1.,-1.,1.; 1.,1.,1.; -1.,1.,1.
  |] in
  Array.init (count + 8) (fun point ->
    if point < 8 then corners.(point)
    else begin
      let index = point - 8 in
      let coordinate multiplier modulus =
        (2. *. float_of_int ((index * multiplier + 17) mod modulus)
          /. float_of_int modulus) -. 1. in
      coordinate 104_729 1_000_003,
      coordinate 130_363 1_000_033,
      coordinate 155_921 1_000_037
    end)

let fibonacci_sphere count =
  let golden = Float.pi *. (3. -. sqrt 5.) in
  Array.init count (fun point ->
    let y = 1. -. (2. *. (float_of_int point +. 0.5) /. float_of_int count) in
    let radius = sqrt (max 0. (1. -. (y *. y)))
    and angle = golden *. float_of_int point in
    radius *. cos angle, y, radius *. sin angle)

let planar_cloud count =
  let side = max 2 (int_of_float (ceil (sqrt (float_of_int count)))) in
  Array.init count (fun point ->
    let x = point mod side and y = point / side in
    2. *. float_of_int x /. float_of_int (side - 1) -. 1.,
    2. *. float_of_int y /. float_of_int (side - 1) -. 1., 0.)

let line_cloud count =
  Array.init count (fun point -> float_of_int point, 0., 0.)

let duplicate_cloud count =
  Array.make count (0.25, -0.5, 0.75)

let hash geometry =
  let positions = Packed.Float3.View.view (Geometry.positions geometry)
  and topology = Topology.View.view (Geometry.topology geometry)
  and value = ref 17 in
  let mix item = value := ((!value * 65_599) lxor item) land max_int in
  Array.iter (fun item -> mix (Int64.to_int (Int64.bits_of_float item))) positions.x;
  Array.iter (fun item -> mix (Int64.to_int (Int64.bits_of_float item))) positions.y;
  Array.iter (fun item -> mix (Int64.to_int (Int64.bits_of_float item))) positions.z;
  Array.iter mix topology.vertex_points;
  Array.iter mix topology.primitive_offsets;
  !value

let measure name input =
  let times = Array.make repeats 0. and allocations = Array.make repeats 0.
  and promoted = Array.make repeats 0. and major = Array.make repeats 0.
  and output_points = ref 0 and output_primitives = ref 0
  and output_hash = ref 0 in
  Parallel.run ~domains (fun () ->
    for repeat = 0 to repeats - 1 do
      Gc.full_major ();
      let before = Gc.quick_stat () and allocated = Gc.allocated_bytes ()
      and started = Unix.gettimeofday () in
      let output = Convex_hull.run { cancel = None; grain = Some grain; selection = None; preserve_point_payload = true; source_point_attribute = None; hull_group = None } input |> get in
      times.(repeat) <- Unix.gettimeofday () -. started;
      allocations.(repeat) <- Gc.allocated_bytes () -. allocated;
      let after = Gc.quick_stat () in
      promoted.(repeat) <- (after.promoted_words -. before.promoted_words) *. 8.;
      major.(repeat) <- (after.major_words -. before.major_words) *. 8.;
      output_points := Geometry.point_count output;
      output_primitives := Geometry.primitive_count output;
      output_hash := hash output
    done);
  Printf.printf "%s,%d,%d,%d,%d,%.6f,%.0f,%.0f,%.0f,%d,%d,%d\n%!"
    name (Geometry.point_count input) domains grain repeats (median times)
    (median allocations) (median promoted) (median major) !output_points
    !output_primitives !output_hash

let run () =
  Printf.printf
    "case,input_points,domains,grain,repeats,seconds,current_domain_allocated_bytes,promoted_bytes,major_bytes,output_points,output_primitives,hash\n";
  measure "duplicates" (geometry (duplicate_cloud planar_points));
  measure "line" (geometry (line_cloud planar_points));
  measure "interior" (geometry (cube_interior interior_points));
  measure "surface" (geometry (fibonacci_sphere surface_points));
  measure "planar" (geometry (planar_cloud planar_points))
