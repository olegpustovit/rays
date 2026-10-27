(* P8.S3 film A/B: pathtrace.rays (trace-pixel, on the CPU walker) against pathtrace.metal on a tiny scene:
   a plane, one material, one rect light, the sky/ground environment, no lens. Both accumulate the same frames
   with the same PCG draws; the resolved RGBA8 images are compared as RMSE of (byte difference / 255). *)
module P = Rays_pathtracer
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
let table name rows = (* a Lisp array of vec3 literals indexed by position *)
  let rec chain k = function [] -> "[0. 0. 0.]" | row :: rest ->
    if rest = [] then vec row else Printf.sprintf "(if (int/lt i %d) %s %s)" (k + 1) (vec row) (chain (k + 1) rest) in
  Printf.sprintf "%s (map (fn [(i : int)] %s) (array/range %d :type int))" name (chain 0 rows) (List.length rows)
let () =
  let mesh = Rays.Mesh.plane ~width:4. ~height:4. () |> Rays.Mesh.recalculate_normals in
  let view = Rays.Mesh.Private.packed_view mesh in
  let nrm_view = Option.get view.normals in
  let at (plane : Rays.Mesh.Private.vec3_view) k = (plane.x.(k), plane.y.(k), plane.z.(k)) in
  let corners = Array.to_list view.indices in
  let positions = List.map (at view.vertices) corners and normals = List.map (at nrm_view) corners in
  let color r g b = Rays_math.Color.Linear.rgb r g b in
  let material = P.material ~roughness:0.5 ~metallic:0.2 (color 0.7 0.5 0.3) in
  let light = { P.at = Rays_math.Vec3.create 0.5 0.8 2.; target = Rays_math.Vec3.create 0.5 0.8 0.; size = (1.5, 1.); color = color 1. 1. 1.; intensity = 6. } in
  let environment = { P.sky = color 0.5 0.6 0.8; ground = color 0.2 0.2 0.2; panels = [] } in
  let scene = { P.objects = [ (mesh, material) ]; spheres = []; strands = []; environment; lights = [ light ] } in
  let eye = Rays_math.Vec3.create 0.3 (-3.) 2.5 and target = Rays_math.Vec3.create 0. 0. 0. in
  let camera = Rays.Camera.perspective ~fov_y:0.9 ~at:eye ~target () in
  let tracer = get (P.create ~spp ~bounces ~exposure ~width ~height scene) in
  for _ = 1 to frames do get (P.render tracer camera); get (P.flush tracer) done;
  let msl = Bytes.copy (get (Rays.Image.Private.pixels (P.image tracer))) in
  P.destroy tracer;
  (* the Lisp side: the same uniforms *)
  let e = (0.3, -3., 2.5) in
  let forward = normalize (sub (0., 0., 0.) e) in
  let right = normalize (cross forward (0., 1., 0.)) in
  let up = cross right forward in
  let l = light in
  let lw, lh = l.size in
  let lforward = normalize (sub (l.target.x, l.target.y, l.target.z) (l.at.x, l.at.y, l.at.z)) in
  let lright = normalize (cross lforward (if Float.abs (let _, y, _ = lforward in y) > 0.999 then (0., 0., 1.) else (0., 1., 0.))) in
  let lup = cross lright lforward in
  let scale k (a, b, c) = (a *. k, b *. k, c *. k) in
  let lu = scale lw lright and lv = scale lh lup in
  let origin = (l.at.x -. (let x, _, _ = lu and a, _, _ = lv in (x +. a) /. 2.), l.at.y -. (let _, y, _ = lu and _, a, _ = lv in (y +. a) /. 2.),
    l.at.z -. (let _, _, z = lu and _, _, a = lv in (z +. a) /. 2.)) in
  let rgb (c : P.Linear_color.t) = (c.r, c.g, c.b) in
  let declarations = read "pathtrace_helpers.rays" ^ read "pathtrace.rays" in
  let scale3 (r, g, b) k = (r *. k, g *. k, b *. k) in
  let light_lit = Printf.sprintf "{:lo %s :lu %s :lv %s :one-sided 0. :rad %s :area %s}" (vec origin) (vec lu) (vec lv)
    (vec (scale3 (rgb l.color) l.intensity)) (num (lw *. lh)) in
  let mat_lit = Printf.sprintf "{:albedo %s :rough %s :emit %s :metal %s}" (vec (rgb material.albedo)) (num material.roughness)
    (vec (rgb material.emission)) (num material.metallic) in
  (* CPU ray host: brute-force Moller-Trumbore over the triangles *)
  let triangles = Array.of_list positions in
  Flow.Ray_host.register 1 (fun q ->
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
    !best);
  let live = Input.at_time 0. in
  let program_of frame =
    let u = Printf.sprintf "{:eye %s :lens 0. :fwd %s :rgt %s :up %s :tan-half %s :aspect %s :focus 1. :width %d :height %d :spp %d :bounces %d :frame %d :sky %s :ground %s}"
      (vec e) (vec forward) (vec right) (vec up) (num (tan (0.9 /. 2.))) (num (float width /. float height)) width height spp bounces frame
      (vec (rgb environment.sky)) (vec (rgb environment.ground)) in
    let source = Printf.sprintf "(let* [%s %s] (map (fn [(pixel : int)] (let* [s (trace-pixel %s %s %s pos nrm pixel)] [s.x s.y s.z (+ 0. (* 0. t))])) (array/range %d :type int)))"
      (table "pos" positions) (table "nrm" normals) u light_lit mat_lit (width * height) in
    Test_program.compile ~declarations source in
  let vec4 = function Flow.Eval.Vec4_array v -> v | _ -> assert false in
  let cpu program = vec4 (ok (Flow_ir.Packed.force program ~live)) in
  let gpu_run =
    let lease = match Renderer.acquire_gpu () with Ok l -> l | Error _ -> failwith "no GPU" in
    let device = Renderer.gpu_device lease and queue = Renderer.gpu_queue lease in
    let gpu r = match r with Ok v -> v | Error e -> failwith (Ogpu.Error.to_string e) in
    let buffer ?(memory = Ogpu.Types.Shared) size = gpu (Ogpu.Backend.create_buffer ~memory device
      { label = Some "pt"; size = Int64.of_int size; usage = [ Ogpu.Types.Storage ] }) in
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
    let pipelines = Flow_gpu.Pipelines.create ~clock:Unix.gettimeofday device in
    fun program ->
      let runner = Flow_gpu.Run.create lease pipelines (ok (Flow_gpu.Emit.kernel ~fast:true program)) in
      let values = ok (Flow_ir.Packed.Private.prepare ~fast:true program ~live) in
      let scene = { Flow_gpu.Run.accel = blas; vertices; spheres; sphere_count = 0; instances = None } in
      let result = vec4 (ok (Flow_gpu.Run.readback (ok (Flow_gpu.Run.dispatch runner ~scene values)))) in
      Flow_gpu.Run.close runner; result in
  let compare name run =
    let total = Array.make (width * height * 3) 0. in
    for frame = 0 to frames - 1 do
      let v = run (program_of frame) in
      for pixel = 0 to width * height - 1 do for c = 0 to 2 do
        total.(3 * pixel + c) <- total.(3 * pixel + c) +. v.(4 * pixel + c) done done
    done;
  let aces c = let c = c *. exposure in
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
  Printf.printf "%d distinct msl colours\n" (Hashtbl.length distinct);
  let rmse = Float.sqrt (!squares /. float (width * height * 3)) in
  Printf.printf "pathtrace.rays (%s) vs pathtrace.metal: %dx%d, %d frames x %d spp, %d of %d channels differ, RMSE %.3g\n" name width height frames spp !different (width * height * 3) rmse
  ;
  assert (rmse <= 1e-4) in
  compare "CPU walker" cpu;
  compare "Metal" gpu_run
