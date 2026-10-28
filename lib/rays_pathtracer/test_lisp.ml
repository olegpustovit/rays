(* The tracer's kernels (pathtrace.rays) on Metal against the CPU walker, the oracle, on a small scene (a plane,
   one material with round corners, a rect light, no lens) lit by the sky/ground environment with a dome panel
   or by a baked World (camera and lighting maps, CDF, sun, a World rect light). Both accumulate the same frames
   with the same PCG draws; the RGBA8 films are compared as RMSE of (byte difference / 255). Then the trace
   kernel's static shape and register pressure. PT_BENCH=1 prints each frame's GPU time; PT_INSTANCED=1 traces
   the plane as a one-object scene_mesh (an instance structure), as the editor does. *)
module P = Rays_pathtracer
module W = Rays.World
let get = function Ok v -> v | Error m -> failwith m
let env name default = match Sys.getenv_opt name with Some v -> int_of_string v | None -> default
let width = env "PT_W" 16 and height = env "PT_H" 12 and frames = env "PT_FRAMES" 4
let bench = Sys.getenv_opt "PT_BENCH" <> None
let instanced = Sys.getenv_opt "PT_INSTANCED" <> None
let color r g b = Rays_math.Color.Linear.rgb r g b
let has fragment line = let n = String.length fragment in
  let rec at i = i + n <= String.length line && (String.sub line i n = fragment || at (i + 1)) in at 0
let mesh = Rays.Mesh.plane ~width:4. ~height:4. () |> Rays.Mesh.recalculate_normals
let material = P.material ~roughness:0.5 ~metallic:0.2 ~round:0.3 (color 0.7 0.5 0.3)
let light = { P.at = Rays_math.Vec3.create 0.5 0.8 2.; target = Rays_math.Vec3.create 0.5 0.8 0.; size = (1.5, 1.); color = color 1. 1. 1.; intensity = 6. }
let panel = { P.direction = Rays_math.Vec3.create 0.3 0.6 0.74; width = 0.5; height = 0.35; softness = 0.2; color = color 1. 0.9 0.8; intensity = 3. }
let environment = { P.sky = color 0.5 0.6 0.8; ground = color 0.2 0.2 0.2; panels = [ panel ] }
let camera = Rays.Camera.perspective ~fov_y:0.9 ~at:(Rays_math.Vec3.create 0.3 (-3.) 2.5) ~target:(Rays_math.Vec3.create 0. 0. 0.) ()
(* a small World: a dim gradient, a bright disc baked into both maps, a rect-light strip and a sun *)
let baked = lazy (W.bake ~domains:1 ~width:16 ~height:8 { W.default with
  exposure = 0.5; sun = `Manual (0.8, 0.9);
  layers = (List.map (fun layer -> { W.name = "layer"; layer; visible = true }) [
    W.Gradient { zenith = W.rgb 0.3 0.35 0.5; horizon = W.rgb 0.2 0.2 0.2; nadir = W.rgb 0.05 0.05 0.05; sharpness = 1. };
    W.Shape { shape = W.Circle; azimuth = 2.5; elevation = 0.6; width = 0.6; height = 0.6; softness = 0.1;
              color = W.rgb 1. 0.8 0.6; intensity = 4.; emit = W.Dome };
    W.Shape { shape = W.Rect; azimuth = -1.; elevation = 0.4; width = 0.5; height = 0.3; softness = 0.;
              color = W.rgb 1. 1. 1.; intensity = 3.; emit = W.Light };
    W.Sun { angular_radius = 0.1; intensity = 20.; tint = W.rgb 1. 0.95 0.9 } ]) })
(* pathtrace.metal, the handwritten kernel this one replaced, was 908 lines *)
let handwritten_lines = 908

(* the CPU walker's scene: brute-force Moller-Trumbore over the plane's triangles, as scene/trace reports a hit *)
let triangles =
  let view = Rays.Mesh.Private.packed_view mesh in
  Array.map (fun k -> (view.vertices.x.(k), view.vertices.y.(k), view.vertices.z.(k))) view.indices
let () =
  let sub (a, b, c) (x, y, z) = (a -. x, b -. y, c -. z) and dot (a, b, c) (x, y, z) = a *. x +. b *. y +. c *. z in
  let cross (a, b, c) (x, y, z) = (b *. z -. c *. y, c *. x -. a *. z, a *. y -. b *. x) in
  let normalize v = let k = Float.sqrt (dot v v) in let x, y, z = v in (x /. k, y /. k, z /. k) in
  Flow.Ray_host.register 1 (fun q ->
    let o = (q.(0), q.(1), q.(2)) and d = normalize (q.(3), q.(4), q.(5)) in
    let best = ref Flow.Ray_host.miss in
    for k = 0 to Array.length triangles / 3 - 1 do
      let p0 = triangles.(3 * k) and p1 = triangles.(3 * k + 1) and p2 = triangles.(3 * k + 2) in
      let e1 = sub p1 p0 and e2 = sub p2 p0 in
      let pv = cross d e2 in
      let det = dot e1 pv in
      if Float.abs det > 1e-12 then begin
        let inv = 1. /. det and tv = sub o p0 in
        let u = dot tv pv *. inv and qv = cross tv e1 in
        let v = dot d qv *. inv and t = dot e2 qv *. inv in
        if u >= 0. && v >= 0. && u +. v <= 1. && t >= q.(6) && t <= q.(7) && ((not !best.Flow.Ray_host.hit) || t < !best.t) then
          let nx, ny, nz = normalize (cross e1 e2) in
          best := { Flow.Ray_host.t; prim = k; u; v; nx; ny; nz; hit = true; instance = 0 }
      end
    done;
    !best)

let scenario label world =
  let scene = { P.objects = [ (mesh, material) ]; spheres = []; strands = []; environment; lights = [ light ] } in
  let t = get (P.create ~spp:2 ~bounces:4 ~profile:bench ~width ~height scene) in
  get (P.set_world t world);
  if instanced then get (P.replace_mesh t (get (P.scene_mesh [ Rays_math.Mat4.identity, get (P.mesh [ (mesh, material) ]) ])));
  P.reset t;
  (* the CPU walker accumulates the same frames, each before the device renders it *)
  let accum = ref (Array.make (4 * width * height) 0.) in
  for _ = 1 to frames do
    if not bench then accum := get (P.Private.cpu_trace t camera ~accum:!accum);
    get (P.render t camera); get (P.flush t)
  done;
  if not bench then begin
    let film = get (Rays.Image.pixels (P.image t)) in
    let exposure = match world with Some (b : W.baked) -> 2. ** b.exposure | None -> 1. in
    let aces c = let c = c *. exposure in
      Float.pow (Float.min 1. (Float.max 0. (c *. (2.51 *. c +. 0.03) /. (c *. (2.43 *. c +. 0.59) +. 0.14)))) (1. /. 2.2) in
    let squares = ref 0. and different = ref 0 and distinct = Hashtbl.create 16 in
    for pixel = 0 to width * height - 1 do
      let w = Float.max 1. !accum.(4 * pixel + 3) in
      for c = 0 to 2 do
        let x = Float.round (aces (!accum.(4 * pixel + c) /. w) *. 255.) and y = float (Bytes.get_uint8 film (4 * pixel + c)) in
        if x <> y then incr different;
        squares := !squares +. (((x -. y) /. 255.) ** 2.)
      done;
      Hashtbl.replace distinct (Bytes.get_uint8 film (4 * pixel), Bytes.get_uint8 film (4 * pixel + 1)) ()
    done;
    let rmse = Float.sqrt (!squares /. float (width * height * 3)) in
    Printf.printf "pathtrace.rays %s, Metal vs the CPU walker: %dx%d, %d frames x 2 spp, %d distinct colours, %d of %d channels differ, RMSE %.3g\n%!"
      label width height frames (Hashtbl.length distinct) !different (width * height * 3) rmse;
    assert (Hashtbl.length distinct > 4 && rmse <= 1e-4)
  end;
  (* static shape gates (specification/gpu-codegen.md): one atomic, no 64-bit arithmetic in the loops but their
     bound checks, within twice the handwritten kernel's lines; register pressure: Metal lowers a pipeline's
     thread limit when a kernel needs more registers *)
  let threads, source = get (P.Private.lisp_kernel t "trace") in
  let source = String.split_on_char '\n' source in
  assert (List.length (List.filter (has "atomic_fetch_or_explicit(") source) = 1);
  let rec loops = function [] -> [] | line :: rest -> if has "for(r" line then line :: rest else loops rest in
  assert (instanced || List.for_all (fun line -> not (has "ulong(" line) || has "bool over" line) (loops source));
  assert (List.length source <= 2 * handwritten_lines);
  Printf.printf "pathtrace.rays %s: trace kernel %d lines, maxTotalThreadsPerThreadgroup %d (pathtrace.metal 384)\n%!"
    label (List.length source) threads;
  assert (instanced || threads >= 448);  (* the recorded flat limit (bound materials and bands); a drop is a regression *)
  P.destroy t

let () =
  let only = Sys.getenv_opt "PT_SCENE" in
  if only <> Some "world" then scenario "dome" None;
  if only <> Some "dome" then scenario "world" (Some (Lazy.force baked))
