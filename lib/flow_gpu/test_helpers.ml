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
  let programs = List.map (fun src -> Test_program.compile ~declarations src) [ first; second ] in
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
    close "fresnel" two.(4 * i) (r1 +. (1. -. r1) *. Float.pow (1. -. r4) 5.)
  done;
  match Renderer.acquire_gpu () with
  | Error e -> Format.eprintf "%a@." Renderer.pp_error e; exit 2
  | Ok lease -> Fun.protect ~finally:(fun () -> Renderer.release_gpu lease) (fun () ->
      let pipelines = Flow_gpu.Pipelines.create ~clock:Unix.gettimeofday (Renderer.gpu_device lease) in
      List.iteri (fun k program ->
        let runner = Flow_gpu.Run.create lease pipelines (Test_program.ok (Flow_gpu.Emit.kernel ~fast:true program)) in
        let values = Test_program.ok (Flow_ir.Packed.Private.prepare ~fast:true program ~live) in
        let gpu = vec4 (Test_program.ok (Flow_gpu.Run.readback (Test_program.ok (Flow_gpu.Run.dispatch runner values)))) in
        let expected = List.nth cpu k and worst = ref 0. in
        Array.iteri (fun j v -> worst := Float.max !worst (Float.abs (v -. expected.(j)) /. Float.max 1. (Float.abs expected.(j)))) gpu;
        Printf.printf "pathtrace_helpers.rays kernel %d: %d rows, GPU vs CPU max relative error %.3g\n%!" k n !worst;
        assert (!worst <= 1e-3);
        Flow_gpu.Run.close runner) programs;
      Flow_gpu.Pipelines.close pipelines)
