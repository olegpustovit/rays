open Rays
open Sop
module Cook = Rays_editor__.Cook
module Settings = Rays_editor.Settings
module Timeline = Rays_editor.Timeline
module Document = Editor_document.Document

let check condition message = if not condition then failwith message
let settings mode = Settings.make Test_editor_commands.schema mode
let mode settings = Settings.get Test_editor_commands.schema settings
let network id graph = (id, Flow_sop.Network.of_geometry (Edit_graph.of_graph graph), Node.id graph)

let frame =
  let frame = Test_editor_input.frame (0., 0.) [] 0 in
  { frame with input = { frame.input with dt = 0. } }

let stopped = fst (Timeline.stop (Timeline.create ()))

let await predicate =
  let deadline = Unix.gettimeofday () +. 60. in
  let rec loop () =
    if predicate () then ()
    else if Unix.gettimeofday () >= deadline then failwith "editor cook barrier timeout"
    else (
      Unix.sleepf 0.0001;
      loop ())
  in
  loop ()

let barrier () =
  let mutex = Mutex.create ()
  and ready = Condition.create ()
  and entered = Atomic.make false
  and released = ref false in
  let wait () =
    Mutex.lock mutex;
    Fun.protect
      ~finally:(fun () -> Mutex.unlock mutex)
      (fun () ->
        Atomic.set entered true;
        while not !released do
          Condition.wait ready mutex
        done)
  in
  let release () =
    Mutex.lock mutex;
    released := true;
    Condition.broadcast ready;
    Mutex.unlock mutex
  in
  (entered, wait, release)

let with_cook ?(domains = 1) prepare run =
  let current =
    ref
      (Cook.create ~prepare ~seed:42L ~grain:64 ~domains ~max_entries:16
         ~max_payload_bytes:(16 * 1024 * 1024)
         ()
      |> Result.get_ok)
  in
  Fun.protect
    ~finally:(fun () -> Cook.close !current)
    (fun () ->
      let step ?(settings = Settings.none) ?(timeline = stopped) ?(changes = []) ?(buttons = [])
          ?frame_request ?probes objects =
        let update =
          Cook.update ~live:false ?probes !current ~settings ~objects ~edit_error:None
            ~effects:Param.no_effects ~timeline_changes:changes ~timeline
            ~frame:{ frame with input = { frame.input with buttons } }
            ~frame_request
        in
        current := update.cook;
        update
      in
      let finish ?settings ?timeline ?changes objects =
        let published = ref None and first = ref true in
        await (fun () ->
            let update =
              step ?settings ?timeline ?changes:(if !first then changes else None) objects
            in
            first := false;
            if update.prepared_changed && Cook.status !current = Async_cook.Idle then
              published := Some update;
            Option.is_some !published);
        Option.get !published
      in
      run current step finish)

let run () =
  let source = Lisp_sop.snapshot (Rdk.Line_geometry.points [| (0., 0., 0.) |]) in
  let objects = [ network 1 source; network 2 source ] in
  let prepares = Atomic.make 0 in
  with_cook
    (fun settings output ->
      Atomic.incr prepares;
      Editor_prepare.empty settings output)
    (fun current step finish ->
      let a = settings 0 and b = settings 1 in
      ignore (finish ~settings:a objects);
      let second =
        List.assoc 2
          (List.map (fun (piece : Cook.piece) -> (piece.id, piece)) (Cook.pieces !current))
      in
      let changed =
        network 1
          (let migration_translation = Rays_math.Vec3.create 2. 0. 0. in
           Lisp_sop.node
             ~with_:[ ("source", source) ]
             (Printf.sprintf
                {|(sop/transform (sop/ext_source) :mode "Matrix" :m03 %s :m13 %s :m23 %s)|}
                (Lisp_sop.float migration_translation.Rays_math.Vec3.x)
                (Lisp_sop.float migration_translation.Rays_math.Vec3.y)
                (Lisp_sop.float migration_translation.Rays_math.Vec3.z)))
      in
      ignore (finish ~settings:a [ changed; List.nth objects 1 ]);
      check (Atomic.get prepares = 3) "one changed object re-prepared its static sibling";
      check
        (List.find (fun (piece : Cook.piece) -> piece.id = 2) (Cook.pieces !current) == second)
        "static prepared piece lost identity";
      ignore (finish ~settings:b [ changed; List.nth objects 1 ]);
      check
        (Atomic.get prepares = 5
        && List.for_all (fun (piece : Cook.piece) -> piece.settings == b) (Cook.pieces !current))
        "settings did not invalidate both prepares without a cook effect";
      for _ = 1 to 5 do
        ignore (step ~settings:b [ changed; List.nth objects 1 ])
      done;
      check (Atomic.get prepares = 5) "idle frames resubmitted static objects";
      (* footers are counted by a job of their own after the display: the pieces stay, nothing is
         prepared again, and the counts arrive *)
      let pieces = Cook.pieces !current and node_id = (fun (_, _, id) -> id) (List.hd objects) in
      let probes = [ (1, node_id) ] in
      await (fun () ->
          ignore (step ~settings:b ~probes [ changed; List.nth objects 1 ]);
          Cook.geometry !current ~object_id:1 ~node_id <> None);
      check
        (List.for_all2 ( == ) pieces (Cook.pieces !current) && Atomic.get prepares = 5)
        "counting a footer re-prepared the displayed objects";
      (* a scrub counts no footer: neither the edit's display cook nor the recook a live document
         makes the next frame carries any; the counts arrive once, after the edits settle *)
      let scrubbed i =
        network 1
          (Lisp_sop.node
             ~with_:[ ("source", source) ]
             (Printf.sprintf {|(sop/transform (sop/ext_source) :mode "Matrix" :m03 %d.0)|} i))
      in
      let objects = [ scrubbed 0; List.nth objects 1 ] in
      let update ~recook objects =
        let update =
          Cook.update ~live:true ~probes !current ~settings:b ~objects ~edit_error:None
            ~effects:{ Param.no_effects with cook = recook } ~timeline_changes:[] ~timeline:stopped
            ~frame ~frame_request:None
        in
        current := update.cook;
        update
      in
      let before = (!current).Cook.summaries in
      for i = 1 to 5 do
        let objects = [ scrubbed i; List.nth objects 1 ] in
        let run ~recook =
          let first = ref true in
          await (fun () ->
              let u = update ~recook:(recook && !first) objects in
              first := false;
              check ((!current).Cook.summaries == before) "a scrub step counted footers";
              u.prepared_changed && Cook.status !current = Async_cook.Idle)
        in
        run ~recook:false;
        run ~recook:true
      done;
      await (fun () ->
          ignore (update ~recook:false objects);
          (!current).Cook.summaries != before));
  let dependencies = Context.Dependencies.(union (one Time) (one Frame)) in
  let dynamic =
    Sop.Custom.plain ~operation:"editor_cook_clock" ~dependencies [ source ] (fun ~context inputs ->
        Rdk.Geometry.with_positions
          (Rdk.Packed.Float3.View.of_owned_exn
             ~x:[| Context.time context |]
             ~y:[| Int64.to_float (Context.frame context) |]
             ~z:[| 0. |])
          inputs.(0))
  in
  with_cook Editor_prepare.surface (fun current _ finish ->
      let objects = [ network 1 dynamic; network 2 source ] in
      ignore (finish objects);
      let static = List.find (fun (piece : Cook.piece) -> piece.id = 2) (Cook.pieces !current) in
      let timeline, changes = Timeline.seek stopped ~frame:60L in
      ignore (finish ~timeline ~changes objects);
      let pieces = Cook.pieces !current in
      check
        (Editor_prepare.first_point
           (List.find (fun (piece : Cook.piece) -> piece.id = 1) pieces).prepared
        = (1., 60., 0.))
        "Time/Frame prepare reused stale output";
      check
        (List.find (fun (piece : Cook.piece) -> piece.id = 2) pieces == static)
        "dynamic context invalidated a static sibling";
      let timeline, changes = Timeline.seek timeline ~frame:0L in
      ignore (finish ~timeline ~changes objects);
      check
        (Editor_prepare.first_point (List.hd (Cook.pieces !current)).prepared = (0., 0., 0.))
        "seek back kept stale context");
  (* a live drive: the box's scale is the clock *)
  let lowered =
    match
      Flow.Syntax.parse "(workspace live (graph g :context sop (sop/box :uniform_scale t)))"
    with
    | Error d -> failwith (Diagnostic.to_string d)
    | Ok forms -> (
        match Flow_sop.Lower.workspace ~extra:Editor_document.Kinds.descriptors forms with
        | Ok lowered -> lowered
        | Error d -> failwith (Diagnostic.to_string d))
  in
  let graph = List.hd lowered.graphs in
  let network = graph.network and root = Option.get graph.root in
  let target = { Flow_sop.Port.node = root; path = "uniform_scale" } in
  let positions = Editor_prepare.surface in
  let points_of (prepared : Cook.prepared) =
    let points = Rdk.Geometry.positions prepared.geometry in
    Array.init (Rdk.Packed.Float3.length points) (Rdk.Packed.Float3.get points)
  in
  let exact first second =
    Array.length first = Array.length second
    && Array.for_all2
         (fun (ax, ay, az) (bx, by, bz) ->
           Int64.bits_of_float ax = Int64.bits_of_float bx
           && Int64.bits_of_float ay = Int64.bits_of_float by
           && Int64.bits_of_float az = Int64.bits_of_float bz)
         first second
  in
  let one_domain = ref [||] in
  with_cook positions (fun current step finish ->
      let objects = [ (1, network, root) ] in
      let timeline, changes = Timeline.seek stopped ~frame:60L in
      ignore (finish ~timeline ~changes objects);
      let first = points_of (List.hd (Cook.pieces !current)).prepared in
      one_domain := first;
      check
        (Flow_sop.Port.Map.find_opt target (Option.get (Cook.applied !current 1)).applied
        = Some (Flow.Port_type.Float_value 1.))
        "cook skipped the time drive at frame 60";
      let timeline, changes = Timeline.seek timeline ~frame:120L in
      ignore (finish ~timeline ~changes objects);
      let second = points_of (List.hd (Cook.pieces !current)).prepared in
      check
        (first <> second
        && Flow_sop.Port.Map.find_opt target (Option.get (Cook.applied !current 1)).applied
           = Some (Flow.Port_type.Float_value 2.))
        "timeline seek did not apply a new value before geometry cooking";
      let before = List.hd (Cook.pieces !current) in
      for _ = 1 to 3 do
        ignore (step ~timeline objects)
      done;
      check (List.hd (Cook.pieces !current) == before) "unchanged time drive resubmitted a cook");
  with_cook ~domains:3 positions (fun current _ finish ->
      let timeline, changes = Timeline.seek stopped ~frame:60L in
      ignore (finish ~timeline ~changes [ (1, network, root) ]);
      check
        (exact !one_domain (points_of (List.hd (Cook.pieces !current)).prepared))
        "driven geometry differed between one and three domains");
  let entered, wait, release = barrier () in
  let first = Atomic.make true in
  Fun.protect ~finally:release (fun () ->
      with_cook
        (fun settings _ ->
          if Atomic.exchange first false then wait ();
          Editor_prepare.at (float (mode settings)))
        (fun current step finish ->
          let a = settings 0 and b = settings 1 in
          ignore (step ~settings:a objects);
          await (fun () -> Atomic.get entered);
          ignore (step ~settings:b ~buttons:[ Input.LeftButton ] objects);
          release ();
          await (fun () -> Cook.status !current = Async_cook.Idle);
          ignore (step ~settings:b ~buttons:[ Input.LeftButton ] objects);
          check
            (List.for_all
               (fun (piece : Cook.piece) ->
                 Editor_prepare.x_of piece.prepared = 0. && piece.settings == a)
               (Cook.pieces !current)
            && List.length (Cook.pieces !current) = 2)
            "delayed completion was stamped with the current settings";
          ignore (finish ~settings:b objects);
          check
            (List.for_all
               (fun (piece : Cook.piece) -> Editor_prepare.x_of piece.prepared = 1.)
               (Cook.pieces !current))
            "held settings change was not replaced after release"));
  List.iter
    (fun removed ->
      let entered, wait, release = barrier () and first = Atomic.make true in
      Fun.protect ~finally:release (fun () ->
          with_cook
            (fun settings _ ->
              if Atomic.exchange first false then wait ();
              Editor_prepare.at (float (mode settings)))
            (fun current step finish ->
              ignore (step ~settings:(settings 0) objects);
              await (fun () -> Atomic.get entered);
              let latest = if removed then [] else objects in
              let b = settings 2 in
              ignore (step ~settings:b latest);
              check ((not removed) || Cook.pieces !current = []) "empty objects retained previews";
              release ();
              ignore (finish ~settings:b latest);
              check
                (if removed then Cook.pieces !current = []
                 else
                   List.for_all
                     (fun (piece : Cook.piece) ->
                       Editor_prepare.x_of piece.prepared = 2. && piece.settings == b)
                     (Cook.pieces !current))
                "superseded completion republished old objects/settings")))
    [ false; true ];
  let entered, wait, release = barrier () and exited = Atomic.make false in
  Fun.protect ~finally:release (fun () ->
      with_cook
        (fun settings output ->
          wait ();
          Atomic.set exited true;
          Editor_prepare.empty settings output)
        (fun current step _ ->
          ignore (step objects);
          await (fun () -> Atomic.get entered);
          let closing = Atomic.make false in
          let unblock =
            Domain.spawn (fun () ->
                await (fun () -> Atomic.get closing);
                release ())
          in
          Atomic.set closing true;
          Cook.close !current;
          Domain.join unblock;
          check (Atomic.get exited) "close returned before the callback exited"));
  print_endline
    "editor cook: per-object reuse, settings/context provenance, explicit late/superseded/empty \
     barriers and joined close passed"
