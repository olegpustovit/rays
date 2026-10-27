let close a b = abs_float (a -. b) < 1e-15

let () =
  assert (Rays_math.Color.to_hex_string (Rays_math.Color.rgb 18 52 86) = "#123456");
  assert (Rays_math.Color.to_hex_string (Rays_math.Color.rgba 18 52 86 120) = "#12345678");
  assert (Rays_math.Color.of_unit_floats ~alpha:0.5 0.5 1. 0. =
    Rays_math.Color.rgba 128 255 0 128);
  assert (Rays_math.Color.of_unit_floats (-1.) 2. 0. =
    Rays_math.Color.rgb 0 255 0);
  assert (Rays_math.Color.hsl_channels 120. 1. 0.5 = (0., 1., 0.));
  assert (Rays_math.Color.hsv 0. 1. 1. = (1., 0., 0.));
  let hr, hg, hb = Rays_math.Color.hsv (1. /. 3.) 1. 1. in
  assert (close hr 0. && close hg 1. && close hb 0.);
  assert (Rays_math.Color.aces 0. = 0.);
  assert (close (Rays_math.Color.aces 1.) (2.54 /. 3.16));
  assert (Rays_math.Color.Linear.rgb 1.5 (-0.25) 4. = {r=1.5;g=(-0.25);b=4.});
  assert (Rays_math.Hash.mix 3 7 = (3 * 65599) lxor 7);
  assert (Rays_math.Hash.mix_with 1_000_003 3 7 = (3 * 1_000_003) lxor 7);
  assert (Rays_math.Hash.fnv64_mix 0xcbf29ce484222325L 65 =
    Int64.mul (Int64.logxor 0xcbf29ce484222325L 65L) 0x100000001b3L);
  assert (Rays_math.Hash.float_sequence32 [1.; 2.] = 3_648_346_971);
  List.iter (fun seed -> List.iter (fun index ->
    assert (Rays_math.Rand.float_at_int seed ~index =
      Rays_math.Rand.float_at (Rays_math.Rand.seed seed) ~index))
      [min_int; -1; 0; 1; max_int]) [min_int; -1; 0; 1; max_int];
  assert (Rays_math.Hash.string "" = 0xcbf29ce484222325L)
