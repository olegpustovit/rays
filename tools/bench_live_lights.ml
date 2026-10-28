(* Full window-free editor updates: constant baseline vs residual lighting. *)
open Rays
module E = Rays_editor.Editor

let frame count : Frame.t =
  {
    input =
      {
        (Input.at_time (float count /. 60.)) with
        dt = 1. /. 60.;
        frame = count;
        tick = count;
        size = (900, 640);
        pointer = (450., 300.);
      };
    pixel_scale = (1., 1.);
    fps = 60.;
    mouse_delta = (0., 0.);
  }

let run views live =
  let source = Buffer.create 2048 in
  Printf.bprintf source
    {|(workspace benchmark
    (graph g :context sop (sop/box))
    (graph scene :context scene [(lamp : float 1)]
      (scene/merge (scene/geometry (ref g))
        (scene/light :intensity %s :color %s)))
    (graph editor :context editor (let* [|}
    (if live then "(+ lamp (* 0.1 (sin t)))" else "lamp")
    (if live then "[(+ 0.5 (* 0.1 (sin t))) 0.5 0.5]" else "[0.5 0.5 0.5]");
  for i = 0 to views - 1 do
    Printf.bprintf source "v%d (ui/viewport (ref scene :lamp %d))\n" i (i + 1)
  done;
  Buffer.add_string source "] (ui/workspace (ui/tile";
  for i = 0 to views - 1 do
    Printf.bprintf source " v%d" i
  done;
  Buffer.add_string source ")))))";
  let workspace = Rays_editor.Workspace.load (Buffer.contents source) |> Result.get_ok in
  let prepares = ref 0 and drawings = ref 0 in
  let e =
    ref
      (E.create ~workspace ~domains:1 ~await:true ~seed:42L
         ~presets:(Filename.temp_dir "rays-live-light-bench" "")
         ~prepare:(fun s o ->
           incr prepares;
           E.prepare s o)
         ~draw:(fun _ _ ->
           incr drawings;
           Scene.D3.empty)
         ()
      |> Result.get_ok)
  in
  Fun.protect
    ~finally:(fun () -> E.close !e)
    (fun () ->
      for i = 1 to 10 do
        e := E.update !e (frame i)
      done;
      let prepared = !prepares and drawn = !drawings in
      for sample = 1 to 5 do
        Gc.full_major ();
        let allocated = Gc.allocated_bytes () and started = Unix.gettimeofday () in
        for i = 1 to 200 do
          e := E.update !e (frame (10 + ((sample - 1) * 200) + i))
        done;
        Printf.printf "%d views, %s, sample %d: %.6f ms/frame, %.0f bytes/frame\n%!" views
          (if live then "live" else "static")
          sample
          ((Unix.gettimeofday () -. started) *. 5.)
          ((Gc.allocated_bytes () -. allocated) /. 200.);
        assert (!prepares = prepared && !drawings = drawn)
      done)

let () =
  List.iter
    (fun views ->
      run views false;
      run views true)
    [ 1; 16 ]
