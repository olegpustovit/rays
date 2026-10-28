(* P8.S3: the path tracer helpers of pathtrace_helpers.rays on random inputs. The CPU walker/Packed result is
   checked against pathtrace.metal's arithmetic written out in OCaml (PCG exactly), and the native GPU kernel
   against the CPU within float32 tolerance. *)
let n = 4096
let read path = In_channel.with_open_bin path In_channel.input_all
let vec4 = function Flow.Eval.Vec4_array v -> v | _ -> assert false
let prelude = "(let* [s0 (int/and (int/mul (int/add i 1) 2654435761) 4294967295)
  a (pcg-next s0) b (pcg-next a.next) c (pcg-next b.next) d (pcg-next c.next)
  r1 (unit-float a.value) r2 (unit-float b.value) r3 (unit-float c.value) r4 (unit-float d.value)
  ns (normalize [(- (* r1 2.) 1.) (- (* r2 2.) 1.) (+ r3 0.1)])
  v (normalize [r2 r3 (+ r1 0.2)]) l (normalize [r3 r1 (+ r2 0.2)])
  a2 (+ 0.01 (* r4 0.5)) fr (fresnel [r1 r2 r3] r4)] "
let kernel body = Printf.sprintf "(map (fn [(i : int)] %s %s) (array/range %d :type int))" prelude body n
let first = kernel "[(float (int/and a.value 16777215)) (d-ggx r1 a2) (v-smith r2 r3 a2) (+ r4 (* 0. t))])"
let more = "  df [r1 r2 r3] f0 [(* r4 0.5) (* r3 0.5) (* r2 0.5)] bs (basis ns)
  lo [(- (* r1 2.) 1.) (- (* r2 2.) 1.) 3.] lu [1. r3 0.] lv [0. 1. r4] ray (normalize [(- r1 0.5) (- r2 0.5) 1.])
  rh (rect-hit lo lu lv (if (> r4 0.5) 1. 0.) [0. 0. 0.] ray 10.) wuv (world-uv ns) wd (world-dir r1 r2) br (eval-brdf ns v l df f0 a2)] "
let kernel3 body = Printf.sprintf "(map (fn [(i : int)] %s%s %s)) (array/range %d :type int))" (String.sub prelude 0 (String.length prelude - 2)) more body n
let extra = [ "[br.x br.y br.z (+ (power-weight r1 r2) (* 0. t))]"; "[bs.tan.x bs.tan.y bs.tan.z (+ bs.b.x (* 0. t))]";
  "[bs.b.y bs.b.z (+ rh.t (* 0. t)) rh.cos]"; "[wuv.u wuv.v wd.x (+ wd.z (* 0. t))]" ]
let second = kernel "[fr.x fr.y fr.z (+ (bsdf-pdf ns v l a2 r4) (* 0. t))])"
(* the MSL arithmetic *)
let pcg state =
  let word = (((state lsr ((state lsr 28) + 4)) lxor state) * 277803737) land 0xffffffff in
  ((state * 747796405 + 2891336453) land 0xffffffff, (word lsr 22) lxor word)
let unit_float w = float (w land 0xffffff) /. 16777216.
let randoms i =
  let s = ((i + 1) * 2654435761) land 0xffffffff in
  let s1, w1 = pcg s in let s2, w2 = pcg s1 in let s3, w3 = pcg s2 in let _, w4 = pcg s3 in
  w1, unit_float w1, unit_float w2, unit_float w3, unit_float w4
let normalize (x, y, z) = let k = Float.sqrt (x *. x +. y *. y +. z *. z) in (x /. k, y /. k, z /. k)
let dot (a, b, c) (x, y, z) = a *. x +. b *. y +. c *. z
let d_ggx noh a2 = let d = noh *. noh *. (a2 -. 1.) +. 1. in a2 /. (Float.pi *. d *. d)
let v_smith nov nol a2 =
  let gv = nol *. Float.sqrt (nov *. nov *. (1. -. a2) +. a2) and gl = nov *. Float.sqrt (nol *. nol *. (1. -. a2) +. a2) in
  0.5 /. Float.max (gv +. gl) 1e-6
let close ?(tolerance = 1e-9) name a b =
  if Float.abs (a -. b) > tolerance *. Float.max 1. (Float.abs b) then failwith (Printf.sprintf "%s: %.9g vs %.9g" name a b)
let () =
  let declarations = read "pathtrace_helpers.rays" in
  let live = Input.at_time 1. in
  let programs = List.map (fun src -> Test_program.compile ~declarations src) ([ first; second ] @ List.map kernel3 extra) in
  let cpu = List.map (fun p -> vec4 (Test_program.ok (Flow_ir.Packed.force p ~live))) programs in
  for i = 0 to n - 1 do
    let w, r1, r2, r3, r4 = randoms i in
    let a2 = 0.01 +. r4 *. 0.5 in
    let one = List.nth cpu 0 and two = List.nth cpu 1 in
    close ~tolerance:0. "pcg" one.(4 * i) (float (w land 0xffffff));
    close "d_ggx" one.(4 * i + 1) (d_ggx r1 a2);
    close "v_smith" one.(4 * i + 2) (v_smith r2 r3 a2);
    let ns = normalize (r1 *. 2. -. 1., r2 *. 2. -. 1., r3 +. 0.1) and v = normalize (r2, r3, r1 +. 0.2)
    and l = normalize (r3, r1, r2 +. 0.2) in
    let h = normalize (let (x, y, z) = l and (a, b, c) = v in (x +. a, y +. b, z +. c)) in
    let nol = Float.max (dot ns l) 0. and noh = Float.max (dot ns h) 0. and voh = Float.max (dot v h) 1e-4 in
    close "bsdf_pdf" two.(4 * i + 3) (r4 *. (d_ggx noh a2 *. noh /. (4. *. voh)) +. (1. -. r4) *. (nol /. Float.pi));
    close "fresnel" two.(4 * i) (r1 +. (1. -. r1) *. Float.pow (1. -. r4) 5.);
    (* basis, eval_brdf, power_weight, rect_hit, world_uv, world_dir: pathtrace.metal's arithmetic *)
    let cross (a, b, c) (x, y, z) = (b *. z -. c *. y, c *. x -. a *. z, a *. y -. b *. x) in
    let sub3 (a, b, c) (x, y, z) = (a -. x, b -. y, c -. z) and scale k (a, b, c) = (a *. k, b *. k, c *. k) in
    let add3 (a, b, c) (x, y, z) = (a +. x, b +. y, c +. z) in
    let (nx, ny, nz) = ns in
    let sg = if nz >= 0. then 1. else -1. in
    let ba = -1. /. (sg +. nz) in let bc = nx *. ny *. ba in
    let bt = (1. +. sg *. nx *. nx *. ba, sg *. bc, -.sg *. nx) and bb = (bc, sg +. ny *. ny *. ba, -.ny) in
    let df = (r1, r2, r3) and f0 = (r4 *. 0.5, r3 *. 0.5, r2 *. 0.5) in
    let fres (a, b, c) = let f = Float.pow (1. -. voh) 5. in (a +. (1. -. a) *. f, b +. (1. -. b) *. f, c +. (1. -. c) *. f) in
    let nov = Float.max (dot ns v) 1e-4 in
    let (fr1, fr2, fr3) = fres f0 in
    let channel d f = d *. (1. -. f) /. Float.pi +. d_ggx noh a2 *. v_smith nov (Float.max (dot ns l) 1e-4) a2 *. f in
    let (d1, d2, d3) = df in
    let r = r2 /. r1 in
    let three = List.nth cpu 2 and four = List.nth cpu 3 and five = List.nth cpu 4 and six = List.nth cpu 5 in
    close "eval_brdf" three.(4 * i) (channel d1 fr1); close "eval_brdf" three.(4 * i + 1) (channel d2 fr2);
    close "eval_brdf" three.(4 * i + 2) (channel d3 fr3);
    close "power_weight" three.(4 * i + 3) (1. /. (1. +. r *. r));
    let (tx, ty, tz) = bt and (bx, by, bz) = bb in
    close "basis" four.(4 * i) tx; close "basis" four.(4 * i + 1) ty; close "basis" four.(4 * i + 2) tz; close "basis" four.(4 * i + 3) bx;
    close "basis" five.(4 * i) by; close "basis" five.(4 * i + 1) bz;
    let lo = (r1 *. 2. -. 1., r2 *. 2. -. 1., 3.) and lu = (1., r3, 0.) and lv = (0., 1., r4) in
    let ray = normalize (r1 -. 0.5, r2 -. 0.5, 1.) in
    let n3 = cross lu lv in let dn = dot ray n3 and nn = dot n3 n3 in
    let t = dot (sub3 lo (0., 0., 0.)) n3 /. dn in
    let p = sub3 (add3 (0., 0., 0.) (scale t ray)) lo in
    let ra = dot (cross p lv) n3 /. nn and rb = dot (cross lu p) n3 /. nn in
    let c = -.dn /. Float.sqrt nn in
    let cosl = if r4 > 0.5 then c else Float.abs c in
    let ok = Float.abs dn >= 1e-12 && nn > 0. && t > 1e-4 && t < 10. && ra >= 0. && ra <= 1. && rb >= 0. && rb <= 1. && cosl > 1e-4 in
    close "rect_hit" five.(4 * i + 2) (if ok then t else -1.); close "rect_cos" five.(4 * i + 3) cosl;
    let u0 = Float.atan2 nx (-.nz) /. (2. *. Float.pi) +. 0.5 in
    let u = if u0 >= 1. then u0 -. 1. else u0 in
    close "world_uv" six.(4 * i) u; close "world_uv" six.(4 * i + 1) (Float.acos (Float.min 1. (Float.max (-1.) ny)) /. Float.pi);
    let phi = (r1 -. 0.5) *. 2. *. Float.pi and theta = r2 *. Float.pi in
    close "world_dir" six.(4 * i + 2) (Float.sin theta *. Float.sin phi);
    close "world_dir" six.(4 * i + 3) (-.(Float.sin theta *. Float.cos phi))
  done;
  match Renderer.acquire_gpu () with
  | Error e -> Format.eprintf "%a@." Renderer.pp_error e; exit 2
  | Ok lease -> Fun.protect ~finally:(fun () -> Renderer.release_gpu lease) (fun () ->
      let pipelines = Flow_gpu.Pipelines.create ~clock:Unix.gettimeofday (Renderer.gpu_device lease) in
      List.iteri (fun k program ->
        let runner = Flow_gpu.Run.create lease pipelines (Test_program.ok (Flow_gpu.Emit.kernel ~fast:true program)) in
        let values = Test_program.ok (Flow_ir.Packed.Host.prepare ~fast:true program ~live) in
        let gpu = vec4 (Test_program.ok (Flow_gpu.Run.readback (Test_program.ok (Flow_gpu.Run.dispatch runner values)))) in
        let expected = List.nth cpu k and worst = ref 0. in
        Array.iteri (fun j v -> worst := Float.max !worst (Float.abs (v -. expected.(j)) /. Float.max 1. (Float.abs expected.(j)))) gpu;
        Printf.printf "pathtrace_helpers.rays kernel %d: %d rows, GPU vs CPU max relative error %.3g\n%!" k n !worst;
        assert (!worst <= 1e-3);
        Flow_gpu.Run.close runner) programs;
      Flow_gpu.Pipelines.close pipelines)
