(* P8.S3 film A/B: pathtrace.rays (trace-pixel, on the CPU walker and on Metal) against pathtrace.metal on a tiny
   scene, the kernel's scene arrays and camera bound as host values and its scene facts as literals: a plane, one material, rect lights, no lens; lit by the sky/ground environment with a dome panel, or by a
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
  (* Host bindings: the scene's arrays and the camera are values bound to the kernel's captures
     (uniforms and gather tables), the scene facts are literals the static stage folds. The kernel
     compiles once per scene; a frame rebinds the camera and frame index. *)
  let module E = Flow.Eval in
  let host = [ "pos", Flow.Ty.Array Vec3; "nrm", Array Vec3; "pan", Array Vec3; "lights", Array Vec3;
    "wmap", Array Vec3; "cdf", Array Float; "eye", Vec3; "fwd", Vec3; "rgt", Vec3; "up", Vec3;
    "tanhalf", Float; "aspect", Float; "frame", Int ] in
  let ops = List.map (fun (name, ty) -> { Flow.Op.noise3 with name = "pt/" ^ name; packed_extension = None;
    signature = { pos = []; opt = []; rest = None; kw = [] }; out = (fun _ -> ty);
    shape = Flow.Op.Struct { splice = false }; check = (fun _ -> ());
    body = (fun ~live:_ ~node:_ _ -> Flow.Value.Source ("pt/" ^ name, ty, [])) }) host in
  let facts = Printf.sprintf "{:eye eye :lens 0. :fwd fwd :rgt rgt :up up :tan-half tanhalf :aspect aspect :focus 1. :width %d :height %d :spp %d :bounces %d :frame frame :sky %s :ground %s :panels %d :lights %d :world %d :background %d :map-w %d :map-h %d :bg %s :sun-dir %s :sun-cos %s :sun-rad %s :sun-pdf %s :round-samples 4}"
    width height spp bounces (vec (rgb environment.sky)) (vec (rgb environment.ground)) (List.length panels / 3) (List.length lights / 5)
    (if world = None then 0 else 1) background map_w map_h (vec bg) (vec sun_dir) (num sun_cos) (vec sun_rad) (num sun_pdf) in
  let text = Printf.sprintf "(workspace pt %s (graph g :context value (let* [%s kernel (fn [(pixel : int)] (let* [u %s s (trace-pixel u lights %s pos nrm pan wmap cdf pixel)] [s.sum.x s.sum.y s.sum.z s.cov]))] 0.0)))"
    declarations (String.concat " " (List.map (fun (name, _) -> Printf.sprintf "%s (pt/%s)" name name) host)) facts mat_lit in
  let kernel = match Flow.Workspace.check ~ops { Flow.Catalog.version = 1; kinds = [] } (ok (Flow.Syntax.parse text)) with
    | Some ws, [] -> (match snd (List.hd (List.assoc [ "g"; "kernel" ] (ok (E.static ~record:true ws)).records)) with
        | E.Fn fn -> fn | _ -> failwith "kernel is not a function")
    | _, ds -> failwith (String.concat "\n" (List.map Diagnostic.to_string ds)) in
  let vec3s rows = E.Vec3_array (Array.of_list (List.concat_map (fun (x, y, z) -> [ x; y; z ]) (if rows = [] then [ (0., 0., 0.) ] else rows))) in
  let scene_values = [ "pos", vec3s positions; "nrm", vec3s normals; "pan", vec3s panels; "lights", vec3s lights;
    "wmap", vec3s maps; "cdf", E.Float_array (Array.of_list (if cdf = [] then [ 0. ] else cdf));
    "eye", (let x, y, z = eye in E.Vec3 (x, y, z)); "fwd", (let x, y, z = forward in E.Vec3 (x, y, z));
    "rgt", (let x, y, z = right in E.Vec3 (x, y, z)); "up", (let x, y, z = up in E.Vec3 (x, y, z));
    "tanhalf", E.Float (tan (0.9 /. 2.)); "aspect", E.Float (float width /. float height) ] in
  let pixels = E.Int_array (Array.init (width * height) Fun.id) in
  let residual_of dynamic =
    let values = dynamic @ scene_values in
    let bound = E.Host.map_function_bindings (function
      | Flow.Value.Source (name, _, []) when String.starts_with ~prefix:"pt/" name ->
          List.assoc (String.sub name 3 (String.length name - 3)) values
      | value -> value) kernel in
    match ok (E.Host.map_function ~signature:{ params = [ Int ]; result = Vec4 } bound [ pixels ]) with
    | E.Residual r -> r | _ -> failwith "kernel did not defer" in
  let compile r = match Flow_ir.Packed.compile_result r (E.Residual.residual_view r).term with
    | Ok p -> p | Error ds -> failwith (String.concat "\n" (List.map Diagnostic.to_string ds)) in
  let cpu frame = vec4 (ok (Flow_ir.Packed.force (compile (residual_of [ "frame", E.Int frame ])) ~live)) in
  (* The device kernel compiles once: its template keeps the per-frame bindings (here the frame
     index) as uniforms, and a frame patches their words in the prepared inputs. *)
  let frame0 = E.Int 0 in
  let template = let r = residual_of [ "frame", frame0 ] in
    Option.get (Flow_ir.Packed.compile_template r (E.Residual.residual_view r).term) in
  let start = Unix.gettimeofday () in
  let kernel_msl = ok (Flow_gpu.Emit.kernel ~fast:true template) in
  if Sys.getenv_opt "PT_BENCH" <> None then
    Printf.eprintf "bind, compile and emit once: %.1f ms\n%!" ((Unix.gettimeofday () -. start) *. 1000.);
  Option.iter (fun path -> Out_channel.with_open_bin (path ^ "." ^ label) (fun o -> output_string o kernel_msl.Flow_gpu.Emit.source)) (Sys.getenv_opt "PT_DUMP");
  (* static shape gates (specification/gpu-codegen.md): the kernel's one atomic, no 64-bit
     arithmetic in the loops but their bound checks (the pixel's x/y split before them is the
     one 64-bit division), within twice the handwritten kernel's lines *)
  let lines text = String.split_on_char '\n' text in
  let source = lines kernel_msl.Flow_gpu.Emit.source in
  assert (List.length (List.filter (has "atomic_fetch_or_explicit(") source) = 1);
  let rec loops = function [] -> [] | line :: rest -> if has "for(r" line then line :: rest else loops rest in
  assert (List.for_all (fun line -> not (has "ulong(" line) || has "bool over" line) (loops source));
  assert (List.length source <= 2 * List.length (lines (read "pathtrace.metal")));
  let runner = Flow_gpu.Run.create lease pipelines kernel_msl in
  (* register pressure: Metal lowers a pipeline's thread limit when a kernel needs more registers *)
  let threads = ok (Flow_gpu.Run.max_total_threads runner) in
  if Sys.getenv_opt "PT_BENCH" <> None then
    Printf.eprintf "maxTotalThreadsPerThreadgroup %d (pathtrace.metal %d)\n%!" threads (get (P.Private.max_total_threads tracer));
  assert (threads >= (if label = "world" then 448 else 512)); (* the recorded limits; a drop is a regression *)
  let base = ok (Flow_ir.Packed.Host.prepare ~fast:true template ~live) in
  let shapes = Flow_ir.Packed.Host.uniform_shapes template in
  let inputs frame = { base with uniforms = Array.mapi (fun i words -> match shapes.(i) with
    | Flow_ir.Packed.Host.Value v when v == frame0 -> [| float frame |] | _ -> words) base.uniforms } in
  let scene = { Flow_gpu.Run.accel = blas; vertices; spheres; sphere_count = 0; instances = None } in
  let gpu_run frame = vec4 (ok (Flow_gpu.Run.readback (ok (Flow_gpu.Run.dispatch runner ~scene (inputs frame))))) in
  let bench () =
    (* PT_BENCH: interleaved A/B, a pathtrace.metal frame (its bands print as gpu_ms) then the Lisp dispatch *)
    if Sys.getenv_opt "PT_BENCH" <> None then begin prerr_endline ("bench " ^ label); for frame = 1 to 5 do
      get (P.render tracer camera); get (P.flush tracer);
      let again = ok (Flow_gpu.Run.dispatch runner ~scene (inputs frame)) in
      Printf.eprintf "lisp kernel GPU ms: %s\n%!" (match Flow_gpu.Run.gpu_seconds again with Some s -> Printf.sprintf "%.2f" (s *. 1000.) | None -> "n/a") done end in
  let compare name run =
    let total = Array.make (width * height * 3) 0. in
    for frame = 0 to frames - 1 do
      let v = run frame in
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
  bench ();
  Flow_gpu.Run.close runner;
  P.destroy tracer

let () =
  let only = Sys.getenv_opt "PT_SCENE" in
  if only <> Some "world" then scenario "dome" None;
  if only <> Some "dome" then scenario "world" (Some (Lazy.force baked))
