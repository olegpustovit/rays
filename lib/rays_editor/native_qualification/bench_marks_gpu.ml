(* Flow_native.Drawing.render_prepared with GPU mark attributes: circles and rects whose positions are a
   GPU region output, with uniform style (constant radius or size and fill: what the removed gpu_circles
   arm served) and per-instance style. A per-instance radius, size or fill that does not read the frame
   is a static region: it runs once on the CPU and its bytes are kept. The "moving" styles read [t], so
   the extent and the fill are regions of every frame, left on the GPU as the positions are. Wall time
   and allocated bytes per frame, median of the frames run; the frame includes the kernel dispatches
   with their completion and status read, not the render pass.
   Run: _build/default/lib/rays_editor/native_qualification/bench_marks_gpu.exe *)
module L = Flow_sop.Lower

let ok = function Ok x -> x | Error d -> failwith (Diagnostic.to_string d)
let size = (800, 600)

let points n =
  Printf.sprintf
    "(map (fn [i] [(* (mod i 800) 1.0) (+ (/ i 1250) (* 5 (sin t))) 0]) (array/range %d))" n

let colours ~moving n =
  Printf.sprintf "(map (fn [i] [(/ (mod i 256) 255) %s 0.8]) (array/range %d))"
    (if moving then "(* 0.5 (+ 1 (sin (+ t (* i 0.001)))))" else "0.5")
    n

(* [radius] and [fill] say which attributes are per instance; [moving] makes those read the frame time. *)
let picture kind ~radius ~fill ~moving n =
  let extent body =
    if moving then Printf.sprintf "(+ %s (+ 1 (sin (+ t (* i 0.01)))))" body else body
  in
  let extent_circle =
    if radius then Printf.sprintf "(map (fn [i] %s) (array/range %d))" (extent "(+ 1 (mod i 4))") n
    else "2"
  and extent_rect =
    if radius then
      Printf.sprintf "(map (fn [i] [%s 3 0]) (array/range %d))" (extent "(+ 2 (mod i 3))") n
    else "[3 3 0]"
  and fill = if fill then colours ~moving n else "\"#5aaaf0\"" in
  match kind with
  | "circles" ->
      Printf.sprintf "(draw/circles %s :radius %s :fill %s)" (points n) extent_circle fill
  | _ -> Printf.sprintf "(draw/rects %s %s :fill %s)" (points n) extent_rect fill

let median a =
  let a = Array.copy a in
  Array.sort Float.compare a;
  a.(Array.length a / 2)

let measure kind (radius, fill, moving) n =
  let lowered =
    Flow.Syntax.parse
      (Printf.sprintf "(workspace w (graph g :context draw %s))"
         (picture kind ~radius ~fill ~moving n))
    |> ok |> L.workspace |> ok
  in
  let prepared =
    Flow_native.Drawing.prepare ~approx:lowered.approx ~states:lowered.states lowered.plan
      (List.assoc "g" lowered.evaluated.results)
    |> ok
  in
  let gpu = Flow_native.Workspace_gpu.create () in
  Flow_native.Workspace_gpu.qualification gpu;
  Fun.protect
    ~finally:(fun () -> Flow_native.Workspace_gpu.close gpu)
    (fun () ->
      let frames = if n >= 1_000_000 then 5 else 11 in
      let state = Flow.Eval.create_state () in
      let frame i =
        Flow_native.Workspace_gpu.with_backend gpu (fun _gpu ->
            ok
              (Flow_native.Drawing.render_prepared ~state
                 ~gpu:(Flow_native.Workspace_gpu.marks gpu)
                 prepared
                 ~live:{ (Input.at_time (float i /. 60.)) with frame = i; tick = i; size }
                 ~size))
      in
      ignore (frame 0);
      let samples =
        Array.init frames (fun i ->
            Gc.full_major ();
            let bytes = Gc.allocated_bytes () and start = Unix.gettimeofday () in
            ignore (frame (i + 1));
            (Unix.gettimeofday () -. start, Gc.allocated_bytes () -. bytes))
      in
      let host, _, _, _, _ = Flow_native.Workspace_gpu.image_stats gpu in
      if host.runners_created = 0 then failwith "the region did not run on the GPU";
      (median (Array.map fst samples), median (Array.map snd samples), host.runners_created))

let () =
  Printf.printf "%-8s %-13s %9s | %10s %14s %8s\n" "kind" "style" "n" "ms" "bytes" "kernels";
  List.iter
    (fun n ->
      List.iter
        (fun kind ->
          List.iter
            (fun (name, style) ->
              let t, b, kernels = measure kind style n in
              Printf.printf "%-8s %-13s %9d | %10.3f %14.0f %8d\n%!" kind name n (t *. 1e3) b
                kernels)
            [
              ("uniform", (false, false, false));
              ("radius", (true, false, false));
              ("fill", (false, true, false));
              ("per-instance", (true, true, false));
              ("moving radius", (true, false, true));
              ("moving fill", (false, true, true));
              ("moving both", (true, true, true));
            ])
        [ "circles"; "rects" ])
    [ 100_000; 1_000_000 ]
