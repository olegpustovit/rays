(* P8.S3 film A/B: pathtrace.rays (trace-pixel, on the CPU walker and on Metal) against pathtrace.metal on a tiny
   scene: a plane, one material, rect lights, no lens; lit by the sky/ground environment with a dome panel, or by a
   baked World (camera and lighting maps, CDF, sun, a World rect light). Both accumulate the same frames with the
   same PCG draws; the resolved RGBA8 images are compared as RMSE of (byte difference / 255). *)
module P = Rays_pathtracer
module W = Rays.World
let get = function Ok v -> v | Error m -> failwith m
let ok = function Ok v -> v | Error d -> failwith (Diagnostic.to_string d)
let read path = In_channel.with_open_bin path In_channel.input_all
let num x = let s = Printf.sprintf "%.17g" x in if String.exists (fun c -> c = '.' || c = 'e' || c = 'n') s then s else s ^ "."
let vec (x, y, z) = Printf.sprintf "[%s %s %s]" (num x) (num y) (num z)
let width = int_of_string (try Sys.getenv "PT_W" with Not_found -> "16") and height = int_of_string (try Sys.getenv "PT_H" with Not_found -> "12")
let spp = 2 and bounces = 4 and frames = int_of_string (try Sys.getenv "PT_FRAMES" with Not_found -> "4") and exposure = 1.
let normalize (x, y, z) = let k = Float.sqrt (x *. x +. y *. y +. z *. z) in (x /. k, y /. k, z /. k)
let sub (a, b, c) (x, y, z) = (a -. x, b -. y, c -. z)
let cross (a, b, c) (x, y, z) = (b *. z -. c *. y, c *. x -. a *. z, a *. y -. b *. x)
let dot (a, b, c) (x, y, z) = a *. x +. b *. y +. c *. z
let scale k (a, b, c) = (a *. k, b *. k, c *. k)
let triple (v : Rays_math.Vec3.t) = (v.x, v.y, v.z)
let rgb (c : P.Linear_color.t) = (c.r, c.g, c.b)
(* A Lisp array of literals indexed by position, as a balanced tree of selects *)
let table name print zero rows =
  let rows = Array.of_list rows in
  let rec pick lo hi = if hi - lo = 1 then print rows.(lo)
    else let mid = (lo + hi) / 2 in Printf.sprintf "(if (int/lt i %d) %s %s)" mid (pick lo mid) (pick mid hi) in
  let body = if rows = [||] then zero else pick 0 (Array.length rows) in
  Printf.sprintf "%s (map (fn [(i : int)] %s) (array/range %d :type int))" name body (max 1 (Array.length rows))
let has fragment line = let n = String.length fragment in
  let rec at i = i + n <= String.length line && (String.sub line i n = fragment || at (i + 1)) in at 0
(* the light rows of pathtrace.metal's light buffer: corner, u, v, radiance, [one-sided area 0] *)
let light_rows (l : P.light) =
  let lw, lh = l.size in
  let lforward = normalize (sub (triple l.target) (triple l.at)) in
  let lright = normalize (cross lforward (if Float.abs (let _, y, _ = lforward in y) > 0.999 then (0., 0., 1.) else (0., 1., 0.))) in
  let lup = cross lright lforward in
  let lu = scale lw lright and lv = scale lh lup in
  let (x, y, z) = triple l.at and (ux, uy, uz) = lu and (vx, vy, vz) = lv in
  [ (x -. (ux +. vx) /. 2., y -. (uy +. vy) /. 2., z -. (uz +. vz) /. 2.); lu; lv; scale l.intensity (rgb l.color); (0., lw *. lh, 0.) ]
let world_light_rows (l : W.light) =
  let u = scale 2. (triple l.u) and v = scale 2. (triple l.v) in
  let n = cross u v in
  let u, v = if dot n (triple l.normal) < 0. then (v, u) else (u, v) in
  let origin = sub (sub (triple l.position) (triple l.u)) (triple l.v) in
  [ origin; u; v; rgb l.radiance; (1., Float.sqrt (dot n n), 0.) ]
let color r g b = Rays_math.Color.Linear.rgb r g b
let mesh = Rays.Mesh.plane ~width:4. ~height:4. () |> Rays.Mesh.recalculate_normals
let positions, normals =
  let view = Rays.Mesh.Private.packed_view mesh in
  let at (plane : Rays.Mesh.Private.vec3_view) k = (plane.x.(k), plane.y.(k), plane.z.(k)) in
  let corners = Array.to_list view.indices in
  List.map (at view.vertices) corners, List.map (at (Option.get view.normals)) corners
let triangles = Array.of_list positions
let material = P.material ~roughness:0.5 ~metallic:0.2 ~round:0.3 (color 0.7 0.5 0.3)
let light = { P.at = Rays_math.Vec3.create 0.5 0.8 2.; target = Rays_math.Vec3.create 0.5 0.8 0.; size = (1.5, 1.); color = color 1. 1. 1.; intensity = 6. }
let panel = { P.direction = Rays_math.Vec3.create 0.3 0.6 0.74; width = 0.5; height = 0.35; softness = 0.2; color = color 1. 0.9 0.8; intensity = 3. }
let environment = { P.sky = color 0.5 0.6 0.8; ground = color 0.2 0.2 0.2; panels = [ panel ] }
let eye = (0.3, -3., 2.5)
let camera = Rays.Camera.perspective ~fov_y:0.9 ~at:(let x, y, z = eye in Rays_math.Vec3.create x y z) ~target:(Rays_math.Vec3.create 0. 0. 0.) ()
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
(* CPU ray host: brute-force Moller-Trumbore over the triangles *)
let () = Flow.Ray_host.register 1 (fun q ->
  let o = (q.(0), q.(1), q.(2)) and d = normalize (q.(3), q.(4), q.(5)) in
  let best = ref Flow.Ray_host.miss in
  for k = 0 to Array.length triangles / 3 - 1 do
    let p0 = triangles.(3 * k) and p1 = triangles.(3 * k + 1) and p2 = triangles.(3 * k + 2) in
    let e1 = sub p1 p0 and e2 = sub p2 p0 in
    let pv = cross d e2 in
    let det = dot e1 pv in
    if Float.abs det > 1e-12 then begin
      let inv = 1. /. det in
      let tv = sub o p0 in
      let u = dot tv pv *. inv in
      let qv = cross tv e1 in
      let v = dot d qv *. inv in
      let t = dot e2 qv *. inv in
      if u >= 0. && v >= 0. && u +. v <= 1. && t >= q.(6) && t <= q.(7) && ((not !best.Flow.Ray_host.hit) || t < !best.t) then
        best := { Flow.Ray_host.t; prim = k; u; v; nx = 0.; ny = 0.; nz = 0.; hit = true; instance = 0 }
    end
  done;
  !best)
let declarations = read "pathtrace_helpers.rays" ^ read "pathtrace.rays"
let live = Input.at_time 0.
let vec4 = function Flow.Eval.Vec4_array v -> v | _ -> assert false
(* The device side: one lease, the plane's acceleration structure *)
let lease = match Renderer.acquire_gpu () with Ok l -> l | Error _ -> failwith "no GPU"
let gpu r = match r with Ok v -> v | Error e -> failwith (Ogpu.Error.to_string e)
let device = Renderer.gpu_device lease and queue = Renderer.gpu_queue lease
let buffer ?(memory = Ogpu.Types.Shared) size = gpu (Ogpu.Backend.create_buffer ~memory device
  { label = Some "pt"; size = Int64.of_int size; usage = [ Ogpu.Types.Storage ] })
let vertices, spheres, blas =
  let bytes = Bytes.create (4 * 3 * Array.length triangles) in
  Array.iteri (fun k (x, y, z) -> List.iteri (fun c v -> Bytes.set_int32_le bytes (4 * (3 * k + c)) (Int32.bits_of_float v)) [x; y; z]) triangles;
  let vertices = buffer (Bytes.length bytes) and spheres = buffer 16 in
  gpu (Ogpu.Backend.write_buffer vertices ~offset:0L bytes);
  let blas = gpu (Ogpu.Backend.create_accel device (Ogpu.Backend.Blas { geometries = [ Ogpu.Backend.Triangles
    { vertices; offset = 0L; length = Int64.of_int (Bytes.length bytes); vertex_stride = 12; vertex_count = Array.length triangles } ] })) in
  let scratch = buffer ~memory:Ogpu.Types.Device_local (Int64.to_int (Ogpu.Backend.accel_sizes blas).build_scratch_size) in
  let commands = gpu (Ogpu.Backend.begin_commands queue) in
  let encoder = gpu (Ogpu.Backend.accel_encoder commands) in
  gpu (Ogpu.Backend.build_accel encoder blas ~scratch ());
  gpu (Ogpu.Backend.end_accel encoder);
  gpu (Ogpu.Backend.complete_through queue (gpu (Ogpu.Backend.commit commands)).epoch);
  vertices, spheres, blas
let pipelines = Flow_gpu.Pipelines.create ~clock:Unix.gettimeofday device

let scenario label (world : W.baked option) =
  let scene = { P.objects = [ (mesh, material) ]; spheres = []; strands = []; environment; lights = [ light ] } in
  let tracer = get (P.create ~spp ~bounces ~exposure ~profile:(Sys.getenv_opt "PT_BENCH" <> None) ~width ~height scene) in
  get (P.set_world tracer world);
  P.reset tracer; (* a World edit makes the next frame a preview *)
  for _ = 1 to frames do get (P.render tracer camera); get (P.flush tracer) done;
  let msl = Bytes.copy (get (Rays.Image.Private.pixels (P.image tracer))) in
  (* the Lisp side: the same uniforms *)
  let forward = normalize (sub (0., 0., 0.) eye) in
  let right = normalize (cross forward (0., 1., 0.)) in
  let up = cross right forward in
  let world_lights = match world with Some b -> b.lights | None -> [] in
  let lights = light_rows light @ List.concat_map world_light_rows world_lights in
  let panels = if world = None then List.concat_map (fun (p : P.panel) ->
    [ normalize (triple p.direction); scale p.intensity (rgb p.color); (p.width, p.height, p.softness) ]) environment.panels else [] in
  let map_w, map_h, maps, cdf, film_exposure = match world with
    | None -> 1, 1, [], [], exposure
    | Some (b : W.baked) ->
        let texels (m : W.map) = List.init (m.width * m.height) (fun k ->
          (Float.Array.get m.pixels (3 * k), Float.Array.get m.pixels (3 * k + 1), Float.Array.get m.pixels (3 * k + 2))) in
        b.camera.width, b.camera.height, texels b.camera @ texels b.lighting,
        Float.Array.to_list b.cdf.marginal @ Float.Array.to_list b.cdf.conditional, exposure *. (2. ** b.exposure) in
  let background, bg = match world with
    | Some { background = W.Color c; _ } -> 1, rgb c | Some { background = W.Transparent; _ } -> 2, (0., 0., 0.)
    | _ -> 0, (0., 0., 0.) in
  let sun_dir, sun_cos, sun_rad, sun_pdf = match world with
    | Some { sun = Some sun; _ } when sun.angular_radius > 0. ->
        let s = sin (sun.angular_radius /. 2.) in let one_minus_cos = 2. *. s *. s in
        normalize (triple sun.direction), one_minus_cos, rgb sun.radiance, 1. /. (2. *. Float.pi *. one_minus_cos)
    | _ -> (0., 0., 0.), 0., (0., 0., 0.), 0. in
  let mat_lit = Printf.sprintf "{:albedo %s :rough %s :emit %s :metal %s :round %s}" (vec (rgb material.albedo)) (num material.roughness)
    (vec (rgb material.emission)) (num material.metallic) (num material.round) in
  let tables = String.concat " " [ table "pos" vec "[0. 0. 0.]" positions; table "nrm" vec "[0. 0. 0.]" normals;
    table "pan" vec "[0. 0. 0.]" panels; table "lights" vec "[0. 0. 0.]" lights; table "wmap" vec "[0. 0. 0.]" maps;
    table "cdf" num "0." cdf ] in
  let program_of frame =
    let u = Printf.sprintf "{:eye %s :lens 0. :fwd %s :rgt %s :up %s :tan-half %s :aspect %s :focus 1. :width %d :height %d :spp %d :bounces %d :frame %d :sky %s :ground %s :panels %d :lights %d :world %d :background %d :map-w %d :map-h %d :bg %s :sun-dir %s :sun-cos %s :sun-rad %s :sun-pdf %s :round-samples 4}"
      (vec eye) (vec forward) (vec right) (vec up) (num (tan (0.9 /. 2.))) (num (float width /. float height)) width height spp bounces frame
      (vec (rgb environment.sky)) (vec (rgb environment.ground)) (List.length panels / 3) (List.length lights / 5)
      (if world = None then 0 else 1) background map_w map_h (vec bg) (vec sun_dir) (num sun_cos) (vec sun_rad) (num sun_pdf) in
    let source = Printf.sprintf "(let* [%s] (map (fn [(pixel : int)] (let* [s (trace-pixel %s lights %s pos nrm pan wmap cdf pixel)] [s.sum.x s.sum.y s.sum.z (+ s.cov (* 0. t))])) (array/range %d :type int)))"
      tables u mat_lit (width * height) in
    Test_program.compile ~declarations source in
  let cpu program = vec4 (ok (Flow_ir.Packed.force program ~live)) in
  let gpu_run program =
    let msl = ok (Flow_gpu.Emit.kernel ~fast:true program) in
    Option.iter (fun path -> Out_channel.with_open_bin (path ^ "." ^ label) (fun o -> output_string o msl.Flow_gpu.Emit.source)) (Sys.getenv_opt "PT_DUMP");
    (* static shape gates (specification/gpu-codegen.md): the kernel's one atomic, no 64-bit
       arithmetic in the loops but their bound checks (the pixel's x/y split before them is the
       one 64-bit division), within twice the handwritten kernel's lines *)
    let lines text = String.split_on_char '\n' text in
    let source = lines msl.Flow_gpu.Emit.source in
    assert (List.length (List.filter (has "atomic_fetch_or_explicit(") source) = 1);
    let rec loops = function [] -> [] | line :: rest -> if has "for(r" line then line :: rest else loops rest in
    assert (List.for_all (fun line -> not (has "ulong(" line) || has "bool over" line) (loops source));
    assert (List.length source <= 2 * List.length (lines (read "pathtrace.metal")));
    let runner = Flow_gpu.Run.create lease pipelines msl in
    let values = ok (Flow_ir.Packed.Private.prepare ~fast:true program ~live) in
    let scene = { Flow_gpu.Run.accel = blas; vertices; spheres; sphere_count = 0; instances = None } in
    let result = vec4 (ok (Flow_gpu.Run.readback (ok (Flow_gpu.Run.dispatch runner ~scene values)))) in
    (* PT_BENCH: interleaved A/B, a pathtrace.metal frame (its bands print as gpu_ms) then the Lisp dispatch *)
    if Sys.getenv_opt "PT_BENCH" <> None then begin prerr_endline ("bench " ^ label); for _ = 1 to 5 do
      get (P.render tracer camera); get (P.flush tracer);
      let again = ok (Flow_gpu.Run.dispatch runner ~scene values) in
      Printf.eprintf "lisp kernel GPU ms: %s\n%!" (match Flow_gpu.Run.gpu_seconds again with Some s -> Printf.sprintf "%.2f" (s *. 1000.) | None -> "n/a") done end;
    Flow_gpu.Run.close runner; result in
  let compare name run =
    let total = Array.make (width * height * 3) 0. in
    for frame = 0 to frames - 1 do
      let v = run (program_of frame) in
      for pixel = 0 to width * height - 1 do for c = 0 to 2 do
        total.(3 * pixel + c) <- total.(3 * pixel + c) +. v.(4 * pixel + c) done done
    done;
    let aces c = let c = c *. film_exposure in
      let v = c *. (2.51 *. c +. 0.03) /. (c *. (2.43 *. c +. 0.59) +. 0.14) in
      Float.pow (Float.min 1. (Float.max 0. v)) (1. /. 2.2) in
    let count = float (frames * spp) and squares = ref 0. and different = ref 0 in
    for pixel = 0 to width * height - 1 do for c = 0 to 2 do
      let byte = Float.of_int (int_of_float ((aces (total.(3 * pixel + c) /. count) *. 255.) +. 0.5)) in
      let other = float (Bytes.get_uint8 msl (4 * pixel + c)) in
      if byte <> other then incr different;
      squares := !squares +. (((byte -. other) /. 255.) ** 2.)
    done done;
    let distinct = Hashtbl.create 16 in
    for pixel = 0 to width * height - 1 do Hashtbl.replace distinct (Bytes.get_uint8 msl (4 * pixel), Bytes.get_uint8 msl (4 * pixel + 1)) () done;
    let rmse = Float.sqrt (!squares /. float (width * height * 3)) in
    Printf.printf "pathtrace.rays %s (%s) vs pathtrace.metal: %dx%d, %d frames x %d spp, %d distinct colours, %d of %d channels differ, RMSE %.3g\n%!"
      label name width height frames spp (Hashtbl.length distinct) !different (width * height * 3) rmse;
    assert (Hashtbl.length distinct > 4 && rmse <= 1e-4) in
  if Sys.getenv_opt "PT_SKIP_CPU" = None then compare "CPU walker" cpu;
  compare "Metal" gpu_run;
  P.destroy tracer

let () =
  let only = Sys.getenv_opt "PT_SCENE" in
  if only <> Some "world" then scenario "dome" None;
  if only <> Some "dome" then scenario "world" (Some (Lazy.force baked))
