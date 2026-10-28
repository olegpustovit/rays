(* P8.S3 film A/B: the tracer on its Lisp kernels (pathtrace.rays) against pathtrace.metal on a small scene
   (a plane, one material with round corners, a rect light, no lens), lit by the sky/ground environment with a
   dome panel or by a baked World (camera and lighting maps, CDF, sun, a World rect light). Both accumulate the
   same frames with the same PCG draws; the RGBA8 films are compared as RMSE of (byte difference / 255).
   PT_BENCH=1 interleaves five frame pairs and prints each kernel's GPU time. *)
module P = Rays_pathtracer
module W = Rays.World
let get = function Ok v -> v | Error m -> failwith m
let env name default = match Sys.getenv_opt name with Some v -> int_of_string v | None -> default
let width = env "PT_W" 16 and height = env "PT_H" 12 and frames = env "PT_FRAMES" 4
let bench = Sys.getenv_opt "PT_BENCH" <> None
(* PT_INSTANCED=1: the plane as a one-object scene_mesh (an instance structure), as the editor traces it *)
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
let pathtrace_metal_lines = List.length (String.split_on_char '\n' (In_channel.with_open_bin "pathtrace.metal" In_channel.input_all))

let tracer ~metal world =
  let scene = { P.objects = [ (mesh, material) ]; spheres = []; strands = []; environment; lights = [ light ] } in
  let t = get (P.create ~spp:2 ~bounces:4 ~profile:bench ~width ~height scene) in
  get (P.set_world t world);
  if instanced then get (P.replace_mesh t (get (P.scene_mesh [ Rays_math.Mat4.identity, get (P.mesh [ (mesh, material) ]) ])));
  if metal then P.Private.use_metal t true;
  P.reset t;
  t

let film t = Bytes.copy (get (Rays.Image.Private.pixels (P.image t)))

let scenario label world =
  let lisp = tracer ~metal:false world and metal = tracer ~metal:true world in
  for _ = 1 to frames do
    List.iter (fun t -> get (P.render t camera); get (P.flush t)) [ lisp; metal ]
  done;
  let a = film lisp and b = film metal in
  let squares = ref 0. and different = ref 0 and distinct = Hashtbl.create 16 in
  for pixel = 0 to width * height - 1 do
    for c = 0 to 2 do
      let x = Bytes.get_uint8 a (4 * pixel + c) and y = Bytes.get_uint8 b (4 * pixel + c) in
      if x <> y then incr different;
      squares := !squares +. ((float (x - y) /. 255.) ** 2.)
    done;
    Hashtbl.replace distinct (Bytes.get_uint8 b (4 * pixel), Bytes.get_uint8 b (4 * pixel + 1)) ()
  done;
  let rmse = Float.sqrt (!squares /. float (width * height * 3)) in
  Printf.printf "pathtrace.rays %s vs pathtrace.metal: %dx%d, %d frames x 2 spp, %d distinct colours, %d of %d channels differ, RMSE %.3g\n%!"
    label width height frames (Hashtbl.length distinct) !different (width * height * 3) rmse;
  assert (Hashtbl.length distinct > 4 && rmse <= 1e-4);
  (* static shape gates (specification/gpu-codegen.md): one atomic, no 64-bit arithmetic in the loops but their
     bound checks, within twice the handwritten kernel's lines; register pressure: Metal lowers a pipeline's
     thread limit when a kernel needs more registers *)
  let threads, source = get (P.Private.lisp_kernel lisp "trace") in
  let source = String.split_on_char '\n' source in
  assert (List.length (List.filter (has "atomic_fetch_or_explicit(") source) = 1);
  let rec loops = function [] -> [] | line :: rest -> if has "for(r" line then line :: rest else loops rest in
  assert (instanced || List.for_all (fun line -> not (has "ulong(" line) || has "bool over" line) (loops source));
  assert (List.length source <= 2 * pathtrace_metal_lines);
  Printf.printf "pathtrace.rays %s: trace kernel %d lines, maxTotalThreadsPerThreadgroup %d (pathtrace.metal %d)\n%!"
    label (List.length source) threads (get (P.Private.max_total_threads metal));
  assert (instanced || threads >= 448);  (* the recorded flat limit (bound materials and bands); a drop is a regression *)
  if bench then begin
    (* interleaved A/B: each frame's bands print their GPU time as pathtracer gpu_ms *)
    prerr_endline ("bench " ^ label);
    for _ = 1 to 5 do
      List.iter (fun (name, t) -> prerr_endline name; get (P.render t camera); get (P.flush t)) [ "metal", metal; "lisp", lisp ]
    done
  end;
  P.destroy lisp;
  P.destroy metal

let () =
  let only = Sys.getenv_opt "PT_SCENE" in
  if only <> Some "world" then scenario "dome" None;
  if only <> Some "dome" then scenario "world" (Some (Lazy.force baked))
