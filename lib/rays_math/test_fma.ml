(* The correctly rounded result of 0.1 * 1e-308 + 1e-308 is subnormal and rounds up; the Windows
   runtime emulation truncated it (0x7e8ebea82e680). *)
let () =
  let r = Rays_math.Fma.fma 0.1 1e-308 1e-308 in
  assert (Int64.bits_of_float r = 0x7e8ebea82e681L);
  assert (Rays_math.Fma.fma 3. 5. 7. = 22.);
  assert (Rays_math.Fma.fma 1.0000000000000002 1.0000000000000002 (-1.0000000000000004) = 4.930380657631324e-32)
