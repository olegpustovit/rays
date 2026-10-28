
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

let edit_point_ranges ?grain operation geometry =
  let source = Packed.Float3.View.view (Geometry.positions geometry) in
  let x = Array.copy source.x and y = Array.copy source.y and z = Array.copy source.z in
  Support.run_ranges ?grain (Array.length x)
    (fun ~first ~last -> operation ~first ~last ~x ~y ~z);
  let positions = Packed.Float3.View.of_owned_exn ~x ~y ~z in
  Geometry.with_positions positions geometry |> Result.get_ok

let transform ?grain matrix geometry =
  let source = Packed.Float3.View.view (Geometry.positions geometry) in
  let count = Array.length source.x in
  let x = Array.make count 0. and y = Array.make count 0. and z = Array.make count 0. in
  let (m00,m01,m02,m03), (m10,m11,m12,m13),
      (m20,m21,m22,m23), (m30,m31,m32,m33) = Rays_math.Mat4.to_rows matrix in
  run ?grain count (fun index ->
    let vx = source.x.(index) and vy = source.y.(index) and vz = source.z.(index) in
    let ox = m00*.vx +. m01*.vy +. m02*.vz +. m03
    and oy = m10*.vx +. m11*.vy +. m12*.vz +. m13
    and oz = m20*.vx +. m21*.vy +. m22*.vz +. m23
    and ow = m30*.vx +. m31*.vy +. m32*.vz +. m33 in
    if abs_float ow <= 1e-12 then begin
      x.(index) <- ox; y.(index) <- oy; z.(index) <- oz
    end else begin
      x.(index) <- ox /. ow; y.(index) <- oy /. ow; z.(index) <- oz /. ow
    end);
  let positions = Packed.Float3.View.of_owned_exn ~x ~y ~z in
  Geometry.with_positions positions geometry |> Result.get_ok
