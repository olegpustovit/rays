
let run ?(grain = 16_384) count operation =
  if grain <= 0 then invalid_arg "Kernel: grain must be positive";
  if count > 0 then
    Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(count - 1) operation

let geometry_of_positions positions =
  let topology = Topology.empty ~point_count:(Packed.Float3.length positions) in
  Geometry.create ~positions ~topology () |> Result.get_ok

let generate_point_ranges count operation =
  if count < 0 then invalid_arg "Kernel.generate_point_ranges: negative point count";
  let x = Array.make count 0. and y = Array.make count 0. and z = Array.make count 0. in
  Support.run_ranges count (fun ~first ~last -> operation ~first ~last ~x ~y ~z);
  Packed.Float3.View.of_owned_exn ~x ~y ~z |> geometry_of_positions
