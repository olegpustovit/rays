open Bench_util
open Rays
open Rdk

let points = integer_env "RAYS_CIRCLE_EDGE_POINTS" 1_000_000
let repeats = integer_env "RAYS_CIRCLE_EDGE_REPEATS" 5
let domains = integer_env "RAYS_BENCH_DOMAINS"
    (Parallel.recommended_domains ())
let grain = integer_env "RAYS_CIRCLE_EDGE_GRAIN" 16_384

let loops ~count ~vertices_per_loop =
  let point_count = count * vertices_per_loop in
  let x = Array.make point_count 0. and y = Array.make point_count 0.
  and z = Array.make point_count 0. in
  for component = 0 to count - 1 do
    let cx = float_of_int (component mod 1_000) *. 4.
    and cy = float_of_int (component / 1_000) *. 4. in
    for local = 0 to vertices_per_loop - 1 do
      let point = (component * vertices_per_loop) + local
      and angle = 2. *. Float.pi *. float_of_int local
          /. float_of_int vertices_per_loop in
      let radius = 1. +. (0.15 *. sin (3. *. angle))
          +. (0.04 *. cos (7. *. angle)) in
      x.(point) <- cx +. (radius *. cos angle);
      y.(point) <- cy +. (radius *. sin angle);
      z.(point) <- 0.06 *. cos (2. *. angle)
    done
  done;
  let topology = Topology.create_owned ~point_count
      ~vertex_points:(Array.init point_count Fun.id)
      ~primitive_offsets:(Array.init (count + 1)
        (fun primitive -> primitive * vertices_per_loop))
      ~primitive_kinds:(Array.make count Topology.Closed_polyline)
      |> Result.get_ok in
  Geometry.create ~positions:(Packed.Float3.Private.of_owned_exn ~x ~y ~z)
    ~topology () |> Result.get_ok

let hash geometry =
  let positions = Packed.Float3.Private.view (Geometry.positions geometry) in
  let value = ref 17 in
  let mix item = value := ((!value * 65_599) lxor
      Int64.to_int (Int64.bits_of_float item)) land max_int in
  Array.iter mix positions.x; Array.iter mix positions.y;
  Array.iter mix positions.z;
  !value

let measure name components input =
  let times = Array.make repeats 0. and allocated = Array.make repeats 0.
  and promoted = Array.make repeats 0. and major = Array.make repeats 0.
  and output_hash = ref 0 in
  Parallel.run ~domains (fun () ->
    for repeat = 0 to repeats - 1 do
      Gc.full_major ();
      let before = Gc.quick_stat () and bytes_before = Gc.allocated_bytes ()
      and started = Unix.gettimeofday () in
      let output = Circle_from_edges.run ~grain input |> get in
      times.(repeat) <- Unix.gettimeofday () -. started;
      allocated.(repeat) <- Gc.allocated_bytes () -. bytes_before;
      let after = Gc.quick_stat () in
      promoted.(repeat) <- (after.promoted_words -. before.promoted_words) *. 8.;
      major.(repeat) <- (after.major_words -. before.major_words) *. 8.;
      let current_hash = hash output in
      if repeat > 0 && current_hash <> !output_hash then
        failwith (name ^ ": nondeterministic output");
      output_hash := current_hash
    done);
  Printf.printf "%s,%d,%d,%d,%d,%d,%.6f,%.0f,%.0f,%.0f,%d\n%!"
    name (Geometry.point_count input) components domains grain repeats
    (median times) (median allocated) (median promoted) (median major)
    !output_hash

let run () =
  Printf.printf "case,input_points,components,domains,grain,repeats,seconds,current_domain_allocated_bytes,promoted_bytes,major_bytes,hash\n";
  let vertices_per_loop = 16 in
  let component_count = max 1 (points / vertices_per_loop) in
  let many = loops ~count:component_count ~vertices_per_loop in
  let one = loops ~count:1 ~vertices_per_loop:points in
  measure "many_16_point_loops" component_count many;
  measure "single_large_loop" 1 one
