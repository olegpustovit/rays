(* Does a live resize freeze frames?

   On macOS, dragging a window edge runs AppKit's resize tracking loop inside
   the call that pumps events, so a program that renders from its own loop
   (Rays's) gets no iteration until the drag ends: the frames freeze while
   the window is being resized. SDL can call a function from inside that loop
   (an event watch), but running OCaml there is a callback into OCaml, which
   specification/sdl3.md forbids; that exception is an open decision.

   This executable measures it with a real window.

   --auto  (the default, run by @qualification): resizes the window
           programmatically and checks that frames keep coming and that the
           window's size reaches the runtime through SDL's size events, never
           by polling. A programmatic resize does not enter AppKit's tracking
           loop, so this is the control: it shows nothing freezes without the
           drag, and that the event path used after a drag is correct.

   --drag SECONDS (by hand: dune exec lib/runtime/native_qualification/runtime_live_resize.exe -- --drag 15):
           opens the window and measures the longest gap between frames while
           you drag an edge of it. A gap of 100 ms or more while you were
           dragging is the freeze; a smooth drag keeps every gap under about
           40 ms at 60 Hz. It cannot run unattended: only a pointer drag on
           the window border enters the tracking loop. *)

let get = function
  | Ok value -> value
  | Error error -> failwith (Format.asprintf "%a" Renderer.pp_error error)

let create width height =
  Renderer.create {Renderer.default_configuration with logical_width=width;logical_height=height;
    drawable_width=width;drawable_height=height;vsync=true}

let events = match Runtime_input.create ~max_events:256 ~logical_width:1 ~logical_height:1 with
  | Ok source -> source
  | Error message -> failwith message

(* One frame as the sketch loop does it: pump, tell the runtime what SDL
   announced, render. Returns what the pump delivered. *)
let frame runtime ~clear =
  (match Runtime_input_sdl3.pump events with
   | Ok () -> ()
   | Error message -> failwith ("pump: " ^ message));
  let delivered = Runtime_input.drain events in
  if List.exists (function
      | Input.Resized _ | Pixel_size_changed _ -> true | _ -> false) delivered
  then Runtime.window_changed (Renderer.Native.runtime runtime);
  ignore (get (Renderer.draw ~clear runtime []));
  delivered

let seconds_now = Unix.gettimeofday

let auto () =
  let runtime = get (create 320 240) in
  Fun.protect ~finally:(fun () -> ignore (Renderer.destroy runtime)) (fun () ->
    ignore (frame runtime ~clear:(0.1, 0.1, 0.1, 1.));
    let size_events = ref 0 and longest_gap = ref 0. in
    let last = ref (seconds_now ()) in
    let sizes = [ 400, 300; 480, 360; 360, 270; 320, 240 ] in
    List.iter (fun (width, height) ->
      get (Runtime.resize (Renderer.Native.runtime runtime) ~width ~height);
      for step = 1 to 12 do
        let delivered = frame runtime ~clear:(float step /. 12., 0.2, 0.3, 1.) in
        size_events := !size_events + List.length (List.filter (function
          | Input.Resized _ | Pixel_size_changed _ -> true | _ -> false) delivered);
        let now = seconds_now () in
        longest_gap := Float.max !longest_gap (now -. !last);
        last := now
      done;
      let facts = get (Runtime.presentation_facts (Renderer.Native.runtime runtime)) in
      if facts.logical_width <> width || facts.logical_height <> height then
        failwith (Printf.sprintf "the runtime's size %dx%d did not follow the window's %dx%d"
          facts.logical_width facts.logical_height width height))
      sizes;
    if !size_events = 0 then
      failwith "SDL announced no size change for four programmatic resizes";
    (* A size changed behind the runtime's back (what a drag does) reaches it
       through SDL's event alone: the frame pumps, sees Resized and
       Pixel_size_changed, announces, and renders at the new size. *)
    let window = Option.get (Renderer.Native.window_handle runtime) in
    (match Sdl3.Window.set_size window ~width:500 ~height:400 with
     | Ok () -> () | Error error -> failwith (Format.asprintf "%a" Sdl3.pp_error error));
    (match Sdl3.Window.sync window with
     | Ok () -> () | Error error -> failwith (Format.asprintf "%a" Sdl3.pp_error error));
    let announced = frame runtime ~clear:(0.3, 0.3, 0.3, 1.) in
    if not (List.exists (function Input.Resized (500, 400) -> true | _ -> false) announced)
       || not (List.exists (function Input.Pixel_size_changed _ -> true | _ -> false) announced)
    then failwith "SDL did not announce the new size as Resized and Pixel_size_changed";
    ignore (frame runtime ~clear:(0.4, 0.4, 0.4, 1.));
    let after = get (Runtime.presentation_facts (Renderer.Native.runtime runtime)) in
    if after.logical_width <> 500 || after.logical_height <> 400 then
      failwith "SDL's size event did not bring the new size to the runtime";
    if !longest_gap > 0.25 then
      failwith (Printf.sprintf "a programmatic resize stalled the frame loop for %.0f ms"
        (!longest_gap *. 1000.));
    Printf.printf
      "live resize control: 4 programmatic resizes, %d size events, longest frame gap %.1f ms; frames \
       did not freeze (AppKit's drag loop is not entered without a pointer drag; run with --drag)\n%!"
      !size_events (!longest_gap *. 1000.))

let drag duration =
  let runtime = get (create 640 480) in
  Fun.protect ~finally:(fun () -> ignore (Renderer.destroy runtime)) (fun () ->
    Printf.printf "Drag an edge or corner of the window for %.0f seconds...\n%!" duration;
    let started = seconds_now () in
    let last = ref started and frames = ref 0 and size_events = ref 0 in
    let longest = ref 0. and long_gaps = ref 0 in
    while seconds_now () -. started < duration do
      let delivered = frame runtime
          ~clear:(0.5 +. 0.5 *. sin (seconds_now () -. started), 0.2, 0.3, 1.) in
      size_events := !size_events + List.length (List.filter (function
        | Input.Resized _ -> true | _ -> false) delivered);
      let now = seconds_now () in
      let gap = now -. !last in
      last := now;
      incr frames;
      longest := Float.max !longest gap;
      if gap >= 0.1 then incr long_gaps
    done;
    Printf.printf
      "%d frames in %.0f s, %d size events, longest gap %.0f ms, %d gaps of 100 ms or more\n%!"
      !frames duration !size_events (!longest *. 1000.) !long_gaps;
    if !size_events > 0 && !long_gaps > 0 then
      print_endline
        "FROZEN: the window was resized and the frame loop stalled while the \
         resize tracking loop ran."
    else if !size_events > 0 then
      print_endline "NOT FROZEN: the window was resized and frames kept coming."
    else print_endline "INCONCLUSIVE: the window was never resized; drag its edge.")

let () =
  match Array.to_list Sys.argv with
  | [ _ ] | [ _; "--auto" ] ->
      (match create 1 1 with
       | Error _ -> print_endline "live resize control: skipped (no window/Metal device)"
       | Ok probe -> ignore (Renderer.destroy probe); auto ())
  | [ _; "--drag"; seconds ] -> drag (float_of_string seconds)
  | _ -> prerr_endline "usage: runtime_live_resize [--auto | --drag SECONDS]"; exit 2
