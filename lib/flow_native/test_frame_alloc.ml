(* Zero-allocation frames: recursive_signals at full depth (its fold into a draw, on the editor's own
   path: Drawing over the workspace GPU owner, fast precision) drawn many times. After warm-up a frame
   allocates at most a fixed small number of bytes and creates no GPU buffer. The sketch animates, so
   every frame does its work and its counts move; the pointer rests.

   The fold's arrays stay on the device through the draw: the picture must match the one drawn after
   reading them back (the host declines to draw over them, so they come back and the marks upload).

   RAYS_ALLOC_PROFILE=1 prints the measured frames' allocation sites (sampled), heaviest first. *)
module E = Flow.Eval

let fail text =
  prerr_endline text;
  exit 1

let get = function Ok v -> v | Error d -> fail (Diagnostic.to_string d)

(* What a frame still allocates is constant in the data: the OGPU/Metal encoding of its ~130
   dispatches and 32 command buffers (wrappers, retains, results, about 8 KB a dispatch) and the
   fold's per-region bookkeeping; no array the size of the data, no readback, no CPU kernel. The
   measurement is deterministic, so a regression of a few percent fails.
   ponytail: one sketch and a byte bound at today's encoding cost; a few kilobytes needs a dispatch
   list OGPU records once and replays (both backends), then lower this *)
let budget = 1_100_000

let profile frames run =
  let sites = Hashtbl.create 64 in
  let sample (info : Gc.Memprof.allocation) =
    let stack = Printexc.raw_backtrace_to_string info.callstack in
    let key = String.concat " < " (List.filteri (fun i _ -> i < 6) (String.split_on_char '\n' stack)) in
    Hashtbl.replace sites key (info.n_samples + Option.value ~default:0 (Hashtbl.find_opt sites key));
    None
  in
  let rate = 1e-4 in
  ignore
    (Gc.Memprof.start ~sampling_rate:rate ~callstack_size:8
       { Gc.Memprof.null_tracker with alloc_minor = sample; alloc_major = sample });
  run ();
  Gc.Memprof.stop ();
  Hashtbl.fold (fun key n all -> (n, key) :: all) sites []
  |> List.sort (fun (a, _) (b, _) -> compare b a)
  |> List.iteri (fun i (n, key) ->
      if i < 25 then
        Printf.printf "%9.0f B/frame  %s\n" (float n *. 8. /. rate /. float frames) key)

let () =
  let _, doc = Web_export.load Sys.argv.(1) in
  let doc =
    match doc with
    | Ok d -> d
    | Error ds -> fail (String.concat "\n" (List.map Diagnostic.to_string ds))
  in
  let lowered = get (Flow_sop.Lower.of_checked ~inputs:doc.inputs doc.checked) in
  let picture = List.assoc "picture" lowered.evaluated.results in
  let ws = Flow_native.Workspace_gpu.create () in
  Flow_native.Workspace_gpu.qualification ws;
  let drawing =
    get
      (Flow_native.Drawing.prepare
         ~gpu:(Flow_native.Workspace_gpu.config ~fast:true ws)
         ~states:lowered.states lowered.evaluated.plan picture)
  in
  let state = E.create_state () and size = (840, 1000) in
  let draw ?(read_back = false) k =
    (* the pointer at the bottom: density 1, 32 levels *)
    let live =
      {
        (Input.at_time (float k /. 60.)) with
        dt = 1. /. 60.;
        frame = k;
        tick = k;
        size;
        pointer = (420., 1000.);
      }
    in
    let scene =
      Flow_native.Workspace_gpu.with_backend ~fast:true ws (fun gpu ->
          let resident =
            Option.map
              (fun (r, show) -> (r, if read_back then fun _ ~live:_ -> Some None else show))
              (Flow_native.Workspace_gpu.resident gpu ws)
          in
          get
            (Flow_native.Drawing.render_prepared ~state
               ~gpu:(Flow_native.Workspace_gpu.marks ws)
               ?resident drawing ~live ~size))
    in
    Flow_native.Workspace_gpu.frame ws;
    scene
  in
  let pixels scene =
    let canvas = Rays.Canvas.create_exn ~width:(fst size) ~height:(snd size) in
    Fun.protect
      ~finally:(fun () -> ignore (Rays.Canvas.destroy canvas))
      (fun () ->
        match Result.bind (Rays.Canvas.render canvas scene) (fun () -> Rays.Canvas.pixels canvas) with
        | Ok pixels -> pixels
        | Error message -> fail message)
  in
  let buffers () =
    let host, _, _, sink_buffers, _ = Flow_native.Workspace_gpu.image_stats ws in
    host.buffer_creations + sink_buffers
  in
  (* warm-up: kernels compile, placement settles, and every slot grows to the counts of these times
     (a count past its capacity is a change of shape: it reruns and grows) *)
  for k = 1 to 60 do
    ignore (draw k)
  done;
  (* one after the other: the second draw replaces the outputs the first scene binds *)
  let kept = pixels (draw 50) in
  let read = pixels (draw ~read_back:true 50) in
  let differ = ref 0 in
  Array.iteri (fun i c -> if c <> read.(i) then incr differ) kept;
  if !differ > 0 then
    fail (Printf.sprintf "recursive_signals: the kept fold draws %d pixels differently" !differ);
  (* the measured frames replay times the warm-up drew: the same shapes, all the work again *)
  let frames = 20 in
  let run () =
    for k = 41 to 40 + frames do
      ignore (draw k)
    done
  in
  if Sys.getenv_opt "RAYS_ALLOC_PROFILE" = Some "1" then profile frames run;
  let before = Gc.allocated_bytes () and made = buffers () in
  run ();
  let per_frame = int_of_float ((Gc.allocated_bytes () -. before) /. float frames) in
  let made = buffers () - made in
  Printf.printf "recursive_signals: %d bytes and %d GPU buffers a frame after warm-up (budget %d)\n"
    per_frame made budget;
  Flow_native.Workspace_gpu.close ws;
  if per_frame > budget || made > 0 then exit 1
