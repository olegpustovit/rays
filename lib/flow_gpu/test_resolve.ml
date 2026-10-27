(* P8.S3 slice 1: resolve_preview.rays (the Lisp port of pathtrace.metal's resolve_preview) through the
   normal Flow pipeline: CPU walker vs native GPU on synthetic colour/geometry films. *)
let w = 64 and h = 48
let read path = In_channel.with_open_bin path In_channel.input_all
let array = function Flow.Eval.Vec4_array v -> v | _ -> assert false
let n = w * h
let color_src = Printf.sprintf "(map (fn [(i : int)] [(+ 0.5 (noise3 [(* (float i) 0.07) 1. (* 0. t)] :seed 1)) (+ 0.4 (noise3 [(* (float i) 0.05) 2. 0.] :seed 2)) (+ 0.3 (noise3 [(* (float i) 0.03) 3. 0.] :seed 3)) 1.]) (array/range %d :type int))" n
let geom_src = Printf.sprintf "(map (fn [(i : int)] [0. 0. 1. (+ (* 0. t) (if (int/lt (int/mod i 11) 2) 0. (+ 1. (* 0.004 (float (int/mod i 7))))))]) (array/range %d :type int))" n
let film = Printf.sprintf "(let* [color %s geom %s]
     (map (fn [(i : int)] (resolve-pixel color geom i %d %d (+ 1.3 (* 0. t)) true)) (array/range %d :type int)))" color_src geom_src w h n
(* pathtrace.metal's resolve_preview, written out in OCaml (the A side of the A/B) *)
let msl color geom =
  let aces c = let c = c *. 1.3 in
    Float.pow (Float.min 1. (Float.max 0. (c *. (2.51 *. c +. 0.03) /. (c *. (2.43 *. c +. 0.59) +. 0.14)))) (1. /. 2.2) in
  Array.init (n * 4) (fun k -> let p = k / 4 and ch = k mod 4 in
    let cw = geom.(p * 4 + 3) in
    let sum = Array.init 3 (fun c -> color.(p * 4 + c)) and weight = ref 1. in
    if cw > 0. then for dy = -1 to 1 do for dx = -1 to 1 do
      let x = p mod w + dx and y = p / w + dy in
      if (dx <> 0 || dy <> 0) && x >= 0 && y >= 0 && x < w && y < h then begin
        let q = y * w + x in
        let sw = geom.(q * 4 + 3) in
        if sw > 0. && Float.abs (sw -. cw) < Float.max 0.03 (cw *. 0.01)
           && geom.(q * 4) *. geom.(p * 4) +. geom.(q * 4 + 1) *. geom.(p * 4 + 1) +. geom.(q * 4 + 2) *. geom.(p * 4 + 2) > 0.95 then begin
          let wt = if dx = 0 || dy = 0 then 0.5 else 0.25 in
          for c = 0 to 2 do sum.(c) <- sum.(c) +. color.(q * 4 + c) *. wt done;
          weight := !weight +. wt end end done done;
    if ch = 3 then (if cw <= 0. then 0. else 1.) else aces (sum.(ch) /. !weight))
let rmse a b = let sq = ref 0. in
  Array.iteri (fun i v -> sq := !sq +. ((v -. b.(i)) ** 2.)) a; sqrt (!sq /. float (Array.length a))
let () =
  let program = Test_program.compile ~declarations:(read "resolve_preview.rays") (film) in
  let live = Input.at_time 1. in
  let table src = array (Test_program.ok (Flow_ir.Packed.force (Test_program.compile src) ~live)) in
  let cpu = array (Test_program.ok (Flow_ir.Packed.force program ~live)) in
  let a = msl (table color_src) (table geom_src) in
  Printf.printf "resolve_preview.rays: CPU vs MSL-semantics RMSE %.3g\n%!" (rmse cpu a);
  assert (rmse cpu a <= 1e-4);
  match Renderer.acquire_gpu () with
  | Error e -> Format.eprintf "%a@." Renderer.pp_error e; exit 2
  | Ok gpu -> Fun.protect ~finally:(fun () -> Renderer.release_gpu gpu) (fun () ->
      let pipelines = Flow_gpu.Pipelines.create ~clock:Unix.gettimeofday (Renderer.gpu_device gpu) in
      Fun.protect ~finally:(fun () -> Flow_gpu.Pipelines.close pipelines) (fun () ->
        let msl = Test_program.ok (Flow_gpu.Emit.kernel ~fast:true program) in
        let runner = Flow_gpu.Run.create gpu pipelines msl in
        Fun.protect ~finally:(fun () -> Flow_gpu.Run.close runner) (fun () ->
          let values = Test_program.ok (Flow_ir.Packed.Private.prepare ~fast:true program ~live) in
          let out = Test_program.ok (Flow_gpu.Run.dispatch runner values) in
          let gpu_out = array (Test_program.ok (Flow_gpu.Run.readback out)) in
          assert (Array.length gpu_out = Array.length cpu);
          let rmse = rmse gpu_out a in
          Printf.printf "resolve_preview.rays: %d px, GPU vs MSL-semantics RMSE %.3g\n%!" (w * h) rmse;
          assert (rmse <= 1e-4))))
