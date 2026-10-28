open Rays

let check name ok = if not ok then failwith ("test_world: " ^ name)

let same_bits a b =
  Float.Array.length a = Float.Array.length b
  &&
  let ok = ref true in
  Float.Array.iteri
    (fun i x ->
      if Int64.bits_of_float x <> Int64.bits_of_float (Float.Array.get b i) then ok := false)
    a;
  !ok

let same_baked (a : World.baked) (b : World.baked) =
  same_bits a.camera.pixels b.camera.pixels
  && same_bits a.lighting.pixels b.lighting.pixels
  && same_bits a.cdf.marginal b.cdf.marginal
  && same_bits a.cdf.conditional b.cdf.conditional
  && Array.for_all2
       (fun (x : World.map) (y : World.map) -> same_bits x.pixels y.pixels)
       a.specular b.specular
  && a.sh9 = b.sh9 && a.lights = b.lights && a.sun = b.sun

let entry layer = { World.name = "layer"; layer; visible = true }
let with_layers layers = { World.default with layers = List.map entry layers }

let dark v =
  World.Gradient
    { zenith = World.rgb v v v; horizon = World.rgb v v v; nadir = World.rgb v v v; sharpness = 1. }

let scatter ?(mirror = false) ?(emit = World.Dome) seed =
  World.Scatter
    {
      seed;
      count = 12;
      elevation_min = 0.1;
      elevation_max = 1.;
      size_min = 0.1;
      size_max = 0.3;
      palette = [ World.rgb 1. 0.5 0.2 ];
      intensity = 3.;
      mirror;
      emit;
    }

let domains () =
  List.iter
    (fun (name, world) ->
      let bake domains = World.bake ~domains ~width:96 ~height:48 world in
      check ("domains " ^ name) (same_baked (bake 1) (bake 4)))
    World.presets

let seeds () =
  let bake seed =
    World.bake ~domains:1 ~width:64 ~height:32 (with_layers [ dark 0.; scatter seed ])
  in
  check "same seed" (same_baked (bake 3) (bake 3));
  check "different seed" (not (same_bits (bake 3).camera.pixels (bake 4).camera.pixels))

(* Red irradiance at unit normal [n] from the baked coefficients, on the real
   basis World.baked documents for [sh9]. *)
let irradiance (baked : World.baked) (n : Rays_math.Vec3.t) =
  let { Rays_math.Vec3.x; y; z } = n in
  let basis =
    [|
      0.282095;
      0.488603 *. y;
      0.488603 *. z;
      0.488603 *. x;
      1.092548 *. x *. y;
      1.092548 *. y *. z;
      0.315392 *. ((3. *. z *. z) -. 1.);
      1.092548 *. x *. z;
      0.546274 *. ((x *. x) -. (y *. y));
    |]
  in
  let sum = ref 0. in
  Array.iteri (fun k (c : World.rgb) -> sum := !sum +. (c.r *. basis.(k))) baked.sh9;
  !sum

let sh9 () =
  let world =
    with_layers
      [
        World.Gradient
          {
            zenith = World.rgb 1. 0.9 0.8;
            horizon = World.rgb 0.4 0.4 0.5;
            nadir = World.rgb 0.1 0.1 0.1;
            sharpness = 1.;
          };
        World.Shape
          {
            shape = Circle;
            azimuth = 1.;
            elevation = 0.4;
            width = 1.2;
            height = 0.;
            softness = 0.6;
            color = World.rgb 2. 1. 0.5;
            intensity = 1.;
            emit = Dome;
          };
      ]
  in
  let baked = World.bake ~width:128 ~height:64 world in
  let m = baked.lighting in
  let brute n =
    let sum = ref 0. in
    for j = 0 to m.height - 1 do
      let v = (float j +. 0.5) /. float m.height in
      let dw =
        2. *. Float.pi /. float m.width *. (Float.pi /. float m.height) *. sin (v *. Float.pi)
      in
      for i = 0 to m.width - 1 do
        let d = World.direction_of_uv ((float i +. 0.5) /. float m.width) v in
        let c = Float.max 0. (Rays_math.Vec3.dot d n) in
        sum := !sum +. (Float.Array.get m.pixels (((j * m.width) + i) * 3) *. c *. dw)
      done
    done;
    !sum
  in
  let dirs =
    [
      Rays_math.Vec3.unit_y;
      Rays_math.Vec3.neg Rays_math.Vec3.unit_y;
      Rays_math.Vec3.unit_x;
      Rays_math.Vec3.unit_z;
      Rays_math.Vec3.normalize (Rays_math.Vec3.create 1. 0.5 (-1.));
    ]
  in
  let reference = List.map brute dirs in
  let scale = List.fold_left Float.max 0. reference in
  List.iter2
    (fun n e ->
      let got = irradiance baked n in
      Printf.printf "world sh9 irradiance %.4f vs brute force %.4f\n" got e;
      check (Printf.sprintf "sh9 %.3f vs %.3f" got e) (Float.abs (got -. e) <= 0.08 *. scale))
    dirs reference

let promoted () =
  let manual = `Manual (1., 0.5)
  and sun = World.Sun { angular_radius = 0.05; intensity = 50.; tint = World.rgb 1. 1. 0.9 } in
  let rect =
    World.Shape
      {
        shape = Rect;
        azimuth = -0.8;
        elevation = 0.2;
        width = 0.6;
        height = 0.3;
        softness = 0.1;
        color = World.rgb 0.2 1. 0.4;
        intensity = 5.;
        emit = Light;
      }
  in
  let circle =
    World.Shape
      {
        shape = Circle;
        azimuth = 2.;
        elevation = 0.;
        width = 0.5;
        height = 0.;
        softness = 0.1;
        color = World.rgb 1. 1. 1.;
        intensity = 2.;
        emit = Dome;
      }
  in
  let world layers = { (with_layers layers) with sun = manual; rotation = 0.3 } in
  let full =
    World.bake ~width:128 ~height:64
      (world [ dark 0.2; rect; scatter ~mirror:true ~emit:Light 5; circle; sun ])
  and only = World.bake ~width:128 ~height:64 (world [ rect; scatter ~emit:Light 5; sun ]) in
  let p = Float.Array.length full.camera.pixels in
  for k = 0 to p - 1 do
    let cam = Float.Array.get full.camera.pixels k in
    let diff = cam -. Float.Array.get full.lighting.pixels k in
    check "lighting of promoted-only is black" (Float.Array.get only.lighting.pixels k = 0.);
    check "camera - lighting = promoted + disc"
      (Float.abs (diff -. Float.Array.get only.camera.pixels k) <= 1e-9 *. (1. +. cam))
  done;
  check "some promoted radiance" (Float.Array.fold_left ( +. ) 0. only.camera.pixels > 0.);
  check "light count" (List.length full.lights = 13);
  check "sun extracted" (full.sun <> None);
  let room emit =
    World.bake ~width:64 ~height:32
      (with_layers
         [
           World.Room
             {
               width = 6.;
               depth = 8.;
               height = 3.;
               rows = 2;
               columns = 3;
               panel = 0.5;
               intensity = 4.;
               wall = World.rgb 0.8 0.8 0.8;
               cove = 1.;
               emit;
             };
         ])
  in
  let lit = room Light and dome = room Dome in
  check "room camera independent of emit" (same_bits lit.camera.pixels dome.camera.pixels);
  check "room dome lighting = camera" (same_bits dome.lighting.pixels dome.camera.pixels);
  check "room panels leave lighting"
    (Float.Array.fold_left ( +. ) 0. lit.lighting.pixels
    < Float.Array.fold_left ( +. ) 0. lit.camera.pixels);
  check "room panel lights" (List.length lit.lights = 6 && dome.lights = [])

let cdf () =
  let world =
    with_layers
      [
        dark 1e-4;
        World.Shape
          {
            shape = Circle;
            azimuth = 1.;
            elevation = 0.3;
            width = 0.2;
            height = 0.;
            softness = 0.;
            color = World.rgb 1. 1. 1.;
            intensity = 100.;
            emit = Dome;
          };
      ]
  in
  let baked = World.bake ~width:128 ~height:64 world in
  let c = baked.cdf and w = 128 and h = 64 in
  check "marginal ends at 1" (Float.Array.get c.marginal h = 1.);
  for j = 0 to h - 1 do
    check "conditional row ends at 1" (Float.Array.get c.conditional ((j * (w + 1)) + w) = 1.)
  done;
  (* First index k in [o, o + n) with a.(k + 1) > x. *)
  let invert a o n x =
    let lo = ref 0 and hi = ref (n - 1) in
    while !lo < !hi do
      let mid = (!lo + !hi) / 2 in
      if Float.Array.get a (o + mid + 1) > x then hi := mid else lo := mid + 1
    done;
    !lo
  in
  let centre = Rays_math.Vec3.create (cos 0.3 *. sin 1.) (sin 0.3) (-.cos 0.3 *. cos 1.) in
  let hits = ref 0 and n = 1000 in
  for s = 0 to n - 1 do
    let x1 = (float s +. 0.5) /. float n and x2 = Float.rem (float s *. 0.618034) 1. in
    let j = invert c.marginal 0 h x1 in
    let i = invert c.conditional (j * (w + 1)) w x2 in
    let d = World.direction_of_uv ((float i +. 0.5) /. float w) ((float j +. 0.5) /. float h) in
    if Rays_math.Vec3.dot d centre > cos 0.2 then incr hits
  done;
  Printf.printf "world cdf: %d/%d samples inside the peak\n" !hits n;
  check "cdf concentrates" (!hits > 9 * n / 10)

(* The linked sun as the bake extracts it: the direction toward the sun
   (east = +X, up = +Y, north = -Z), or [None] below the horizon. *)
let linked_sun ~latitude ~day_of_year ~hours =
  let world =
    with_layers
      [ dark 0.; World.Sun { angular_radius = 0.05; intensity = 50.; tint = World.rgb 1. 1. 0.9 } ]
  in
  (World.bake ~width:16 ~height:8
     { world with sun = `Linked; rotation = 0.; latitude; day_of_year; time_of_day = hours })
    .sun
  |> Option.map (fun (sun : World.sun) -> sun.direction)

let solar () =
  (match linked_sun ~latitude:0. ~day_of_year:80 ~hours:12. with
  | Some d -> check "equinox noon at zenith" (d.y > 0.99)
  | None -> check "equinox noon has a sun" false);
  check "midnight below horizon" (linked_sun ~latitude:0.7 ~day_of_year:172 ~hours:0. = None);
  match linked_sun ~latitude:0.7 ~day_of_year:172 ~hours:8. with
  | Some d -> check "morning sun in the east" (d.x > 0. && d.y > 0.)
  | None -> check "a summer morning has a sun" false

(* The equirectangular coordinates of a unit direction, as World documents
   them: u = atan2 x (-z) / 2pi + 0.5 in [0, 1), v = acos y / pi. *)
let uv_of_direction (d : Rays_math.Vec3.t) =
  let u = (Float.atan2 d.x (-.d.z) /. (2. *. Float.pi)) +. 0.5 in
  ((if u >= 1. then u -. 1. else u), Float.acos (Float.max (-1.) (Float.min 1. d.y)) /. Float.pi)

let round_trip () =
  for k = 0 to 99 do
    let u = (float k +. 0.5) /. 100. and v = 0.02 +. (0.96 *. Float.rem (float k *. 0.618) 1.) in
    let d = World.direction_of_uv u v in
    check "unit direction" (Float.abs (Rays_math.Vec3.length d -. 1.) < 1e-12);
    let u', v' = uv_of_direction d in
    check "uv round trip" (Float.abs (u -. u') < 1e-12 && Float.abs (v -. v') < 1e-12)
  done;
  let front = World.direction_of_uv 0.5 0.5 in
  check "front is u = 0.5"
    (Float.abs front.x < 1e-12 && Float.abs front.y < 1e-12 && Float.abs (front.z +. 1.) < 1e-12);
  check "row 0 is the zenith" ((World.direction_of_uv 0.25 0.).y = 1.);
  check "azimuth grows toward +X" ((World.direction_of_uv 0.75 0.5).x > 0.999999)

let cached () =
  World.Private.with_cache_context (World.Private.create_cache_context ()) (fun () ->
      let a = World.bake_cached ~width:32 ~height:16 World.default in
      let b = World.bake_cached ~width:32 ~height:16 { World.default with exposure = 2. } in
      check "exposure-only change hits the cache" (a.camera == b.camera && b.exposure = 2.))

let run () =
  domains ();
  seeds ();
  sh9 ();
  promoted ();
  cdf ();
  solar ();
  round_trip ();
  cached ()
