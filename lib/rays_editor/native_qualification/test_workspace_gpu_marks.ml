(* A mark set whose positions, radii and colours are GPU region outputs is drawn by the Marks
   family with those buffers bound directly, inside a transform too, and lights the same pixels
   as the CPU path. The regions that read [t] (positions, radii, the rects' fill, the lines'
   colour) stay on the GPU; the static ones (sizes, line ends) run once on the CPU and ride in the
   same draw as host buffers. Inputs are multiples of 1/8, exact in binary32, and so is every
   colour byte. *)
let ok = function Ok value -> value | Error d -> failwith (Diagnostic.to_string d)
let render canvas scene=match Rays.Canvas.render canvas scene with Ok()->()|Error message->failwith message
let pixels canvas scene=render canvas scene;Result.get_ok(Rays.Canvas.pixels canvas)
module L = Flow_sop.Lower
let source = {|(workspace gpu_marks
  (graph picture :context draw
    (let* [indices (array/range 2048)
           positions (map (fn [i] (let* [column (mod i 64)
                                         row (/ (- i column) 64)]
                                    [(+ (* column 2) (* t 8)) (* row 2) 0])) indices)
           radii (map (fn [i] (+ 1 (* (mod (+ i (* t 8)) 3) 0.5))) indices)]
      (draw/merge (draw/background "#102030")
        (draw/translate [5 7 0] (draw/circles positions :radius radii :fill "#76baca" :stroke "#ffffff"))
        (draw/translate [3 40 0]
          (draw/rects positions (map (fn [i] [2 1.5 0]) indices)
            :fill (map (fn [i] [(/ (mod (+ i (* t 8)) 8) 8) 0.5 0.25]) indices) :stroke "#ff8800"))
        (draw/lines positions (map (fn [i] [(+ (* (mod i 64) 2) 3) (+ (/ (- i (mod i 64)) 32) 5) 0]) indices)
          :color (map (fn [i] [0.5 (/ (mod (+ i (* t 8)) 4) 4) 1]) indices) :width 1.5)))))|}

let () =
  let gpu = Flow_native.Workspace_gpu.create () in
  Flow_native.Workspace_gpu.qualification gpu;
  let gpu_config=Flow_native.Workspace_gpu.config gpu in
  let lowered = Flow.Syntax.parse source |> ok |> L.workspace ~gpu:gpu_config ~factories:Sop_catalog.Editor.factories |> ok in
  let value = List.assoc "picture" lowered.evaluated.results in
  let prepared = Flow_native.Drawing.prepare ~gpu:gpu_config ~approx:lowered.approx ~states:lowered.states lowered.plan value |> ok in
  let canvas = Rays.Canvas.create_exn ~width:160 ~height:96 in
  let capture scene = pixels canvas scene in
  let token_sets scene = Array.fold_left (fun n -> function
    | Scene_command.Render_ir.Shapes s when Scene_command.Ui_batch.marks s <> None -> n + 1 | _ -> n)
    0 (Rays.Scene.Private.commands scene) in
  Fun.protect ~finally:(fun () -> ignore(Rays.Canvas.destroy canvas)) (fun () ->
    List.iter (fun time ->
      let live = {(Input.at_time time) with size = (160, 96)} in
      let cpu = Flow_native.Drawing.render_prepared prepared ~live ~size:(160, 96) |> ok in
      assert (token_sets cpu = 0);
      let expected = capture cpu in
      let scene = Flow_native.Workspace_gpu.with_backend gpu (fun _gpu ->
        Flow_native.Drawing.render_prepared ~gpu:(Flow_native.Workspace_gpu.marks gpu) prepared ~live ~size:(160, 96)) |> ok in
      assert (token_sets scene = 3);
      let actual = capture scene in
      if actual <> expected then begin
        let first = ref (-1) in
        let bbox a = let x0 = ref 999 and x1 = ref 0 and y0 = ref 999 and y1 = ref 0 and n = ref 0 in
          Array.iteri (fun i (c : Rays_math.Color.t) -> if c <> a.(159) then begin incr n;
            x0 := min !x0 (i mod 160); x1 := max !x1 (i mod 160); y0 := min !y0 (i / 160); y1 := max !y1 (i / 160) end) a;
          Printf.sprintf "n=%d x %d..%d y %d..%d bg=%s" !n !x0 !x1 !y0 !y1 (Format.asprintf "%a" (fun f (c : Rays_math.Color.t) -> Format.fprintf f "%d,%d,%d,%d" c.r c.g c.b c.a) a.(159)) in
        prerr_endline ("cpu " ^ bbox expected ^ "\ngpu " ^ bbox actual);
        let n = ref 0 and worst = ref 0 in
        Array.iteri (fun i (c : Rays_math.Color.t) -> let e = expected.(i) in if c <> e then begin
          if !first < 0 then first := i; incr n;
          worst := max !worst (max (abs (c.r - e.r)) (max (abs (c.g - e.g)) (max (abs (c.b - e.b)) (abs (c.a - e.a))))) end) actual;
        failwith (Printf.sprintf "t=%g: GPU marks differ from the CPU path at pixel %d (%d pixels, worst channel %d)" time !first !n !worst)
      end;
      let painted = ref 0 in
      Array.iter (fun c -> if c <> expected.(159) then incr painted) expected;
      assert (!painted > 500)) [0.; 0.5; 0.25]);
  (* Two drawings alive in one frame keep their own sources. *)
  let live = {(Input.at_time 0.5) with size = (160, 96)} in
  let again = Flow_native.Drawing.prepare ~gpu:gpu_config ~approx:lowered.approx ~states:lowered.states lowered.plan value |> ok in
  let both = Flow_native.Workspace_gpu.with_backend gpu (fun _gpu ->
    let first = Flow_native.Drawing.render_prepared ~gpu:(Flow_native.Workspace_gpu.marks gpu) prepared ~live ~size:(160, 96) |> ok in
    let second = Flow_native.Drawing.render_prepared ~gpu:(Flow_native.Workspace_gpu.marks gpu) again ~live ~size:(160, 96) |> ok in
    first, second) in
  let canvas = Rays.Canvas.create_exn ~width:160 ~height:96 in
  Fun.protect ~finally:(fun () -> ignore(Rays.Canvas.destroy canvas)) (fun () ->
    let render scene = pixels canvas scene in
    let a = render (fst both) in
    assert (a = render (snd both)));
  let host, _, _, _, _ = Flow_native.Workspace_gpu.image_stats gpu in
  (* Positions and radii share one dispatch; fill and line colour keep their
     own. The positions are also read by the other two mark sets. *)
  if host.runners_created <> 6 then
    failwith (Printf.sprintf "expected 6 GPU kernels (fused positions/radii, fill, colour, twice), got %d" host.runners_created);
  Flow_native.Workspace_gpu.close gpu;
  print_endline "GPU marks: circle, rect and line sets with GPU positions, radii, fills and colours and host sizes and ends, inside a transform, equal the CPU path"
