let state viewport scissor : Scene_execution.state =
  {viewport;scissor;cull=Ogpu.Render_pass.Cull_none;
   depth_compare=Ogpu.Render_pass.Always;depth_write=false;
   depth_load=Ogpu.Render_pass.Load;depth_clear=1.;transform_uniforms=None}

let run () =
  let mesh:Scene_execution.mesh={key=Host "scale";vertices=Bytes.empty;
    vertex_count=1;indices=Bytes.empty;index_count=1;
    primitive=Ogpu.Render_pass.Triangle_list}in
  let draw={Scene_execution.mesh;state=state(1,2,3,4)(0,0,4,4)}in
  let one:Runtime.frame_facts={logical_width=4;logical_height=4;
    drawable_width=4;drawable_height=4;pixel_scale_x=1.;pixel_scale_y=1.}in
  let retina:Runtime.frame_facts={logical_width=4;logical_height=4;
    drawable_width=8;drawable_height=12;pixel_scale_x=2.;pixel_scale_y=3.}in
  let draws=[draw]in
  if Renderer.Native.scale_draws one draws!=draws then
    failwith"1x draw graph was copied";
  let sampled=[{Scene_execution.family=Scene2;blend=Ogpu.Pipeline.Replace;
    texture=None;auxiliary=None;vertex_attributes=None;samples=1;draw}]in
  if Renderer.Native.scale_sampled_resources one sampled!=sampled then
    failwith"1x sampled graph was copied";
  let scaled=List.hd(Renderer.Native.scale_draws retina draws)in
  if scaled==draw||scaled.state.viewport<>(2,6,6,12)||
     scaled.state.scissor<>(0,0,8,12)then
    failwith"Retina draw edges were not scaled exactly";
  let unchanged={draw with state=state(0,0,0,0)(0,0,0,0)}in
  if List.hd(Renderer.Native.scale_draws retina[unchanged])!=unchanged then
    failwith"unchanged Retina draw was copied";
  let resized={retina with Runtime.drawable_width=12;drawable_height=8}in
  let changed=List.hd(Renderer.Native.scale_draws resized draws)in
  if changed.state.viewport<>(3,4,9,8)||changed.state.scissor<>(0,0,12,8)then
    failwith"resized nonuniform edges were not recomputed";
  (* The per-frame path memoizes on list identity: the same list and facts
     return the same scaled list without allocating; a new list rescales. *)
  let cache=Renderer.Native.new_scaled_cache()in
  let many=List.init 1000(fun _->List.hd sampled)in
  let first=Renderer.Native.scale_sampled_cached cache retina many in
  if first==many||List.length first<>1000 then failwith"cached Retina list was not scaled";
  if Renderer.Native.scale_sampled_cached cache retina many!=first then
    failwith"unchanged draw list was rescaled";
  let words=(Gc.quick_stat()).minor_words in
  for _=1 to 1000 do
    ignore(Renderer.Native.scale_sampled_cached cache retina many)done;
  let per_call=((Gc.quick_stat()).minor_words-.words)/.1000. in
  if per_call>8. then failwith(Printf.sprintf"cached scaling allocated %.0f words per call"per_call);
  let words=(Gc.quick_stat()).minor_words in
  for _=1 to 1000 do ignore(Renderer.Native.scale_sampled_resources retina many)done;
  let uncached=((Gc.quick_stat()).minor_words-.words)/.1000. in
  if Renderer.Native.scale_sampled_cached cache retina(List.tl many)==first then
    failwith"a different draw list reused the cached scaling";
  if Renderer.Native.scale_sampled_cached cache one many!=many then
    failwith"1x facts did not bypass the cache";
  (* A window is not asked for its size every frame: SDL announces a change,
     and one announcement costs exactly one query however many arrive. *)
  let flag=Runtime.Change_flag.create()in
  let queries=ref 0 in
  let query()=incr queries;!queries in
  for _=1 to 1000 do
    if Runtime.Change_flag.refresh flag query<>None then
      failwith"a frame with no size event queried the window"done;
  if !queries<>0 then failwith"quiet frames queried the window";
  Runtime.Change_flag.announce flag;
  Runtime.Change_flag.announce flag;
  if Runtime.Change_flag.refresh flag query<>Some 1 then
    failwith"an announced change did not query the window once";
  if Runtime.Change_flag.refresh flag query<>None then
    failwith"one announcement queried the window twice";
  Runtime.Change_flag.announce flag;
  if Runtime.Change_flag.refresh flag query<>Some 2 then
    failwith"a later announcement was lost";
  Printf.printf"runtime scaling: 1x identity, Retina and resize exact, no per-frame window query; 1000 draws at 2x: %.0f words/frame uncached, %.0f cached\n"uncached per_call

let () = run ()
