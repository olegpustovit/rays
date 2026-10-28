(* P8.S3: the sample-loop prologue and the per-bounce sampling of trace_pixel as Lisp defns: pixel-seed,
   camera-ray (pinhole and thin lens), bsdf-sample and light-sample. The CPU result equals pathtrace.metal's
   arithmetic written out in OCaml (the PCG seed exactly); the native GPU kernel matches the CPU. *)
let n = 4096
let read path = In_channel.with_open_bin path In_channel.input_all
let vec4 = function Flow.Eval.Vec4_array v -> v | _ -> assert false
let prelude = "(let* [seed (pixel-seed (int/mod i 1000) (int/div i 1000))
  a (pcg-next seed) b (pcg-next a.next) c (pcg-next b.next) d (pcg-next c.next)
  r1 (unit-float a.value) r2 (unit-float b.value) r3 (unit-float c.value) r4 (unit-float d.value)
  eye [(- (* r1 4.) 2.) (* r2 2.) (+ 5. r3)] fwd (normalize [(- r2 0.5) (- r3 0.5) -1.])
  rgt (normalize (cross fwd [0. 1. 0.])) upv (cross rgt fwd)
  tan-half (+ 0.4 (* r4 0.3)) aspect (+ 1. (* r1 0.5)) lens (if (> r2 0.5) (* 0.1 r3) 0.) focus (+ 3. r4)
  cr (camera-ray eye lens fwd rgt upv tan-half aspect focus (- (* r2 2.) 1.) (- (* r3 2.) 1.) r4 r1)
  ns (normalize [(- (* r1 2.) 1.) (- (* r2 2.) 1.) (+ r3 0.1)]) v (normalize [r2 r3 (+ r1 0.2)])
  a2 (+ 0.01 (* r4 0.5)) ps (+ 0.05 (* r1 0.9))
  bsm (bsdf-sample ns v ps a2 r1 r2 r3)
  ls (light-sample [(- (* r1 2.) 1.) (- (* r2 2.) 1.) 3.] [1. r3 0.] [0. 1. r4] (if (> r4 0.5) 1. 0.) [0. 0. 0.] r2 r3)]"
let kernel body = Printf.sprintf "(map (fn [(i : int)] (let* %s %s)) (array/range %d :type int))"
  (String.sub prelude 5 (String.length prelude - 5)) body n
let bodies = [ "[(float (int/and seed 16777215)) (float (int/and (int/shr seed 8) 16777215)) (+ cr.o.x (* 0. t)) cr.d.x]";
  "[cr.o.y cr.o.z cr.d.y (+ cr.d.z (* 0. t))]"; "[bsm.l.x bsm.l.y bsm.l.z (+ bsm.h.x (* 0. t))]";
  "[bsm.h.y bsm.h.z ls.dist (+ ls.cosl (* 0. t))]"; "[ls.wi.x ls.wi.y ls.wi.z (+ ls.dist2 (* 0. t))]" ]
let pcg state =
  let word = (((state lsr ((state lsr 28) + 4)) lxor state) * 277803737) land 0xffffffff in
  ((state * 747796405 + 2891336453) land 0xffffffff, (word lsr 22) lxor word)
let unit_float w = float (w land 0xffffff) /. 16777216.
let sub3 (a, b, c) (x, y, z) = (a -. x, b -. y, c -. z) and add3 (a, b, c) (x, y, z) = (a +. x, b +. y, c +. z)
let scale k (a, b, c) = (a *. k, b *. k, c *. k)
let dot (a, b, c) (x, y, z) = a *. x +. b *. y +. c *. z
let cross (a, b, c) (x, y, z) = (b *. z -. c *. y, c *. x -. a *. z, a *. y -. b *. x)
let normalize v = scale (1. /. Float.sqrt (dot v v)) v
let close name a b =
  if Float.abs (a -. b) > 1e-9 *. Float.max 1. (Float.abs b) then failwith (Printf.sprintf "%s: %.9g vs %.9g" name a b)
let () =
  let declarations = read "pathtrace_helpers.rays" in
  let live = Input.at_time 1. in
  let programs = List.map (fun body -> Test_program.compile ~declarations (kernel body)) bodies in
  let cpu = List.map (fun p -> vec4 (Test_program.ok (Flow_ir.Packed.force p ~live))) programs in
  let at k i c = (List.nth cpu k).(4 * i + c) in
  for i = 0 to n - 1 do
    let pixel = i mod 1000 and frame = i / 1000 in
    let state = ((((pixel + 1) * 0x9E3779B9) land 0xffffffff) lxor (((frame + 1) * 0x85EBCA6B) land 0xffffffff)) in
    let s1, _ = pcg state in let seed, _ = pcg s1 in
    close "seed" (at 0 i 0) (float (seed land 0xffffff)); close "seed" (at 0 i 1) (float ((seed lsr 8) land 0xffffff));
    let s2, w1 = pcg seed in let s3, w2 = pcg s2 in let s4, w3 = pcg s3 in let _, w4 = pcg s4 in
    let r1 = unit_float w1 and r2 = unit_float w2 and r3 = unit_float w3 and r4 = unit_float w4 in
    let eye = (r1 *. 4. -. 2., r2 *. 2., 5. +. r3) in
    let fwd = normalize (r2 -. 0.5, r3 -. 0.5, -1.) in
    let rgt = normalize (cross fwd (0., 1., 0.)) in let upv = cross rgt fwd in
    let tan_half = 0.4 +. r4 *. 0.3 and aspect = 1. +. r1 *. 0.5 and focus = 3. +. r4 in
    let lens = if r2 > 0.5 then 0.1 *. r3 else 0. in
    let px = r2 *. 2. -. 1. and py = r3 *. 2. -. 1. in
    let d = normalize (add3 fwd (add3 (scale (px *. (tan_half *. aspect)) rgt) (scale (py *. tan_half) upv))) in
    let aim = add3 eye (scale (focus /. dot d fwd) d) in
    let radius = lens *. Float.sqrt r4 and angle = 6.28318530717959 *. r1 in
    let o' = add3 eye (add3 (scale (radius *. cos angle) rgt) (scale (radius *. sin angle) upv)) in
    let (ox, oy, oz), (dx, dy, dz) = if lens > 0. then o', normalize (sub3 aim o') else eye, d in
    close "camera o.x" (at 0 i 2) ox; close "camera d.x" (at 0 i 3) dx;
    close "camera o.y" (at 1 i 0) oy; close "camera o.z" (at 1 i 1) oz; close "camera d.y" (at 1 i 2) dy; close "camera d.z" (at 1 i 3) dz;
    let ns = normalize (r1 *. 2. -. 1., r2 *. 2. -. 1., r3 +. 0.1) and v = normalize (r2, r3, r1 +. 0.2) in
    let a2 = 0.01 +. r4 *. 0.5 and ps = 0.05 +. r1 *. 0.9 in
    let (nx, ny, nz) = ns in
    let sg = if nz >= 0. then 1. else -1. in
    let ba = -1. /. (sg +. nz) in let bc = nx *. ny *. ba in
    let bt = (1. +. sg *. nx *. nx *. ba, sg *. bc, -.sg *. nx) and bb = (bc, sg +. ny *. ny *. ba, -.ny) in
    let phi = 6.28318530717959 *. r1 in
    let l, h =
      if r3 < ps then begin
        let cos_t = Float.sqrt ((1. -. r2) /. (1. +. (a2 -. 1.) *. r2)) in
        let sin_t = Float.sqrt (Float.max 0. (1. -. cos_t *. cos_t)) in
        let h = normalize (add3 (scale (sin_t *. cos phi) bt) (add3 (scale (sin_t *. sin phi) bb) (scale cos_t ns))) in
        sub3 (scale (2. *. dot v h) h) v, h
      end else begin
        let rr = Float.sqrt r2 in
        let l = normalize (add3 (scale (rr *. cos phi) bt) (add3 (scale (rr *. sin phi) bb) (scale (Float.sqrt (Float.max 0. (1. -. r2))) ns))) in
        l, normalize (add3 l v)
      end in
    let (lx, ly, lz) = l and (hx, hy, hz) = h in
    close "bsdf l.x" (at 2 i 0) lx; close "bsdf l.y" (at 2 i 1) ly; close "bsdf l.z" (at 2 i 2) lz;
    close "bsdf h.x" (at 2 i 3) hx; close "bsdf h.y" (at 3 i 0) hy; close "bsdf h.z" (at 3 i 1) hz;
    let lo = (r1 *. 2. -. 1., r2 *. 2. -. 1., 3.) and lu = (1., r3, 0.) and lv = (0., 1., r4) in
    let lp = add3 lo (add3 (scale r2 lu) (scale r3 lv)) in
    let ln = normalize (cross lu lv) in
    let wi = sub3 lp (0., 0., 0.) in
    let dist2 = Float.max (dot wi wi) 1e-8 in let dist = Float.sqrt dist2 in
    let w = scale (1. /. dist) wi in
    let cl = dot ln w in let cosl = if r4 > 0.5 then -.cl else Float.abs cl in
    let (wx, wy, wz) = w in
    close "light dist" (at 3 i 2) dist; close "light cos" (at 3 i 3) cosl;
    close "light wi.x" (at 4 i 0) wx; close "light wi.y" (at 4 i 1) wy; close "light wi.z" (at 4 i 2) wz;
    close "light dist2" (at 4 i 3) dist2
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
        Array.iteri (fun j x -> worst := Float.max !worst (Float.abs (x -. expected.(j)) /. Float.max 1. (Float.abs expected.(j)))) gpu;
        Printf.printf "pathtrace sampling kernel %d: %d rows, GPU vs CPU max relative error %.3g\n%!" k n !worst;
        assert (!worst <= 2e-3);
        Flow_gpu.Run.close runner) programs;
      Flow_gpu.Pipelines.close pipelines)
