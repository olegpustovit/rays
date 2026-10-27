open Scene_command.Render_ir

let get = function Ok value->value |Error error->
  failwith (Format.asprintf "%a" Renderer.pp_error error)
let identity={xx=1.;xy=0.;yx=0.;yy=1.;tx=0.;ty=0.}

let scene ~reference ~count ~phase =
  let commands=ref [] in
  let add command=commands:=command::!commands in
  add (Push_clip {x=3.;y=2.;width=57.;height=43.});
  add (Push_transform {identity with xx=0.87;yy=1.13;tx=0.375+.phase;ty=0.125});
  for group=0 to 1 do
    add (Set_blend (if group=0 then Alpha else Add));
    for i=0 to count-1 do
      let x=float (i*7 mod 64)-.2.125 and y=float (i*13 mod 48)-.1.375 in
      let color=Int32.logor (Int32.shift_left (Int32.of_int (i mod 255)) 24)
        (Int32.of_int (0x81b000 lor (45+i mod 180))) in
      add (Geometry {vertices=[|x;y;x+.7.25;y;x+.7.25;y+.4.5;x;y+.4.5|];
        indices=[|0;1;2;0;2;3|];color});
      (* Identity state barriers force the independently cached preparation
         path without changing pixels or painter order. *)
      if reference then (add (Push_transform identity);add Pop_transform)
    done
  done;
  add Pop_transform;add Pop_clip;
  Result.get_ok (create (Array.of_list (List.rev !commands)))

let run () =
  let live_handles=snd(Ogpu.Impl.create_driver()) in
  let baseline=live_handles() in
  List.iter (fun scale ->
    let execution=get (Renderer.create_offscreen
      {Renderer.
        logical_width=64;logical_height=48;drawable_width=64*scale;drawable_height=48*scale;
        title="dense-scene2";vsync=false;high_density=true}) in
    Fun.protect ~finally:(fun () -> ignore (get (Renderer.destroy execution)))
      (fun () ->
        let render ir =
          let pixels = Test_ui_pipeline.capture ~clear:(0.,0.,0.,1.) execution
            (fun submission -> Renderer.Private.lower_scene2 submission
              ~density:scale ~resource:(fun _ -> None) ir) in
          if Bytes.length pixels <> 64 * 48 * scale * scale * 4 then
            failwith "dense scene2 readback length";
          let painted = ref 0 in
          Bytes.iteri (fun i value -> if i mod 4 <> 3 && value <> '\000' then incr painted) pixels;
          if !painted < 100 then failwith "dense scene2 submitted no visible geometry";
          pixels in
        List.iter (fun count ->
          for frame=0 to 3 do
            let phase=float frame*.0.125 in
            let expected=render (scene ~reference:true ~count ~phase) in
            let ir=scene ~reference:false ~count ~phase in
            for repeat=0 to 2 do
              if render ir<>expected then failwith (Printf.sprintf
                "dense run drift: scale=%d count=%d frame=%d repeat=%d" scale count frame repeat)
            done
          done) [63;64;65;1024];
        let colored ?(barrier=false) color = scene ~reference:false ~count:63 ~phase:0.
          |> Private.commands_readonly
          |> Array.map (function Geometry g->Geometry {g with color}|c->c)
          |> (fun commands->if barrier then Array.append [|Set_blend Alpha|] commands
              else commands)
          |> create |> Result.get_ok
          |> Renderer.lower_scene2 execution ~density:scale ~resource:(fun _->None)
          |> get in
        let white=colored 0xffffffffl and dark=colored 0x101010ffl in
        let retained identity draws =
          let submission=get (Renderer.Private.begin_submission execution) in
          Fun.protect ~finally:(fun () -> Renderer.Private.cancel submission) (fun () ->
            let batch=get (Renderer.Private.adopt_draws submission draws) in
            ignore (get (Renderer.Private.step ~identity ~version:1L
              ~clear:(0.,0.,0.,1.) submission [batch]));
            get (Renderer.capture execution)) in
        let expected=retained "retained-white" white in
        for _=1 to 12 do
          if retained "retained-dark" dark=expected then failwith "color fixture is ineffective";
          if retained "retained-white" white<>expected then
            failwith "retained mesh key reused another scene's payload"
        done;
        for variant=1 to 5 do
          let color=Int32.of_int(0x100000ff lor (variant lsl 16))in
          ignore(colored color);
          ignore(colored ~barrier:true color)
        done;
        if retained "retained-white-rebuilt" (colored ~barrier:true 0xffffffffl)
            <>expected then failwith "evicted geometry index reused stale colors";
        (* A cache hit must not rewrite transform bytes that an earlier
           lowering's draws still reference; distinct transforms bypass the plan cache. *)
        let shifted tx=Result.get_ok (create [|Push_transform {identity with tx};
          Geometry {vertices=[|4.;4.;20.;4.;20.;20.;4.;20.|];indices=[|0;1;2;0;2;3|];
            color=0xffffffffl};Pop_transform|])
          |> Renderer.lower_scene2 execution ~density:scale ~resource:(fun _->None)
          |> get in
        let render_draws draws =
          Test_ui_pipeline.capture ~clear:(0.,0.,0.,1.) execution
            (fun submission -> Renderer.Private.adopt_draws submission draws) in
        ignore (shifted 1.);
        let first=shifted 2. in
        let expected=render_draws first in
        let moved = shifted 30. in
        if render_draws moved = expected then failwith "shifted geometry did not move";
        if render_draws first<>expected then
          failwith "geometry cache hit rewrote an earlier lowering's transform")) [1;2];
  let after=live_handles() in
  if after<>baseline then failwith "dense scene2 handle delta";
  print_endline "dense scene2: exact 1x/2x pixels, alpha/add, fractional transforms, alternating retained payloads, zero handles"
