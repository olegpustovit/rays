open Bench_util
open Rdk
open Rays_math

let get = Result.get_ok

let run () =
  let count = if Array.length Sys.argv > 2 then int_of_string Sys.argv.(2) else 100_000 in
  let domains = if Array.length Sys.argv > 3 then int_of_string Sys.argv.(3) else 1 in
  if count < 1 then invalid_arg "positive primitive count required";
  let positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.;1.;0.|] ~y:[|0.;0.;1.|] ~z:[|0.;0.;0.|] in
  let topology = Topology.polygons_owned ~point_count:3
      ~vertex_points:(Array.init (count * 3) (fun i -> i mod 3))
      ~primitive_offsets:(Array.init (count + 1) (fun i -> i * 3)) |> get in
  let geometry = Geometry.create ~positions ~topology () |> get in
  let seconds = Array.make 7 0. and allocations = Array.make 7 0. in
  Parallel.run ~domains (fun () ->
    for trial = 0 to 6 do
      Gc.full_major ();
      let allocated = Gc.allocated_bytes () and started = Unix.gettimeofday () in
      let result = Material_assign.run ~name:"cobalt" ~color:(0.15,0.43,0.96)
          ~roughness:0.3 ~emission:(0.,0.,0.) geometry |> get in
      seconds.(trial) <- Unix.gettimeofday () -. started;
      allocations.(trial) <- Gc.allocated_bytes () -. allocated;
      assert (Geometry.primitive_count result = count)
    done);
  Printf.printf "material_assign primitives=%d domains=%d sequential_kernel trials=7 median_ms=%.3f allocated_bytes=%.0f\n%!"
    count domains (1000. *. median seconds) (median allocations)
