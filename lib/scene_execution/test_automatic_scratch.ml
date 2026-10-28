(* Tuple shorthand for the record-based [Scene_execution.submit]. *)
let sampled ?texture ?auxiliary ?(samples=1) family blend draw : Scene_execution.sampled_draw =
  {family;blend;texture;auxiliary;vertex_attributes=None;samples;draw}
let render_blended ?clear r draws=Scene_execution.submit ?clear r
  (List.map(fun(blend,draw)->sampled Scene_execution.Scene2 blend draw)draws)
let get=function Ok value->value|Error error->failwith(Ogpu.Error.to_string error)
let require condition message=if not condition then failwith message

let configuration:Ogpu.Surface.configuration={logical_width=64;logical_height=64;
  physical_width=64;physical_height=64;format=Rgba8_unorm;
  present_mode=Immediate;max_acquired=2;layer=None}

let state:Scene_execution.state={viewport=(0,0,64,64);scissor=(0,0,64,64);
  cull=Ogpu.Render_pass.Cull_none;depth_compare=Always;depth_write=false;
  depth_load=Load;depth_clear=1.;transform_uniforms=None}

let mesh index:Scene_execution.mesh=
  let vertices=Scene_execution_fixtures.scene2_plain_vertices ()and indices=Bytes.make 12 '\000'in
  Bytes.set_int32_le indices 4 1l;Bytes.set_int32_le indices 8 2l;
  {key=Host(Printf.sprintf"scratch-%03d"index);vertices;vertex_count=3;
   indices;index_count=3; primitive=Ogpu.Render_pass.Triangle_list}

let draws count=List.init count(fun index->
  ((if index land 1=0 then Ogpu.Pipeline.Replace else Alpha),
   {Scene_execution.mesh=mesh index;state}))

let with_renderer driver name body=
  match Scene_execution_fixtures.create_offscreen driver configuration with
  |Error{Ogpu.Error.kind=No_adapter;_}->Printf.printf"automatic scratch (%s): skipped (no device)\n"name
  |Error error->failwith(Ogpu.Error.to_string error)
  |Ok renderer->Fun.protect ~finally:(fun()->get(Scene_execution.destroy renderer))
      (fun()->body renderer)

let run () =
  let driver = Ogpu.Impl.create_driver () in let live_handles = Ogpu.Impl.live_handles in
  let before=live_handles()in
  with_renderer driver"retained"(fun replay_renderer->
  let stable=draws 10 in
  let retained=stable|>List.map(fun(blend,draw)->
    {Scene_execution.family=Scene2;blend;texture=None;auxiliary=None;vertex_attributes=None;
     samples=1;draw})in
  ignore(get(Scene_execution.submit
    ~prepared:("scratch-retained",7L) replay_renderer retained));
  (match get(Scene_execution.replay_prepared_sampled_resources
      ~identity:"scratch-retained"~version:7L replay_renderer)with
   |Some(true,10)->()
   |_->failwith"retained replay did not return its exact draw count");
  (match get(Scene_execution.replay_prepared_sampled_resources
      ~identity:"scratch-retained"~version:8L replay_renderer)with
   |None->()
   |Some _->failwith"retained replay accepted a stale version"));
  with_renderer driver"capacity"(fun renderer->
  let over=draws 65_537 in
  (match render_blended renderer over with
   |Error error when error.Ogpu.Error.kind=Capacity->()
   |_->failwith"prepared scratch accepted more than 65,536 draws"));
  with_renderer driver"segments"(fun renderer->
  let first={Scene_execution.mesh=mesh 70_000;state}
  and scene3_mesh,attributes=Scene_execution_fixtures.scene3_mesh~color:0x4080bfffl"scratch-70001"in
  let second={Scene_execution.mesh=scene3_mesh;
    state={state with depth_write=true;
      transform_uniforms=Some(Scene_execution_fixtures.scene3_surface())}}in
  let stable=[{Scene_execution.family=Scene2;blend=Ogpu.Pipeline.Replace;
    texture=None;auxiliary=None;vertex_attributes=None;samples=1;draw=first};
    {Scene_execution.family=Scene3;blend=Ogpu.Pipeline.Replace;
    texture=None;auxiliary=None;vertex_attributes=Some attributes;samples=1;
    draw=second}]in
  let render ~border draws=
    require(get(Scene_execution.submit renderer draws))
      "mixed Scene2/Scene3 frame was skipped";
    let pixels=get(Scene_execution.read_pixels renderer~bytes_per_row:(64*4))in
    require(Bytes.length pixels=64*64*4)"mixed frame readback length";
    for y=0 to 63 do for x=0 to 63 do
      let at=(y*64+x)*4 in
      let channel i=Char.code(Bytes.get pixels(at+i))in
      let expected=if border&&(x=0||x=63||y=0||y=63)then
        255,255,255,255 else 64,128,191,255 in
      require((channel 0,channel 1,channel 2,channel 3)=expected)
        (Printf.sprintf"mixed frame pixel (%d,%d), border=%b"x y border)
    done done in
  render ~border:false stable;
  render ~border:false stable;
  let stats=Scene_execution.retained_stats renderer in
  require(stats.plan_hits=0L&&stats.plan_misses=2L)
    "automatic replay did not wait for two identical frames";
  render ~border:false stable;
  let stats=Scene_execution.retained_stats renderer in
  require(stats.plan_hits=1L&&stats.plan_misses=2L)
    "mixed automatic plan was not replayed";
  let changed=[List.hd stable;
    {(List.nth stable 1) with draw=
      {second with state={second.state with scissor=(1,1,62,62)}}}]in
  render ~border:true changed;
  render ~border:true changed;
  let stats=Scene_execution.retained_stats renderer in
  require(stats.plan_hits=1L&&stats.plan_misses=4L)
    "changed scissor reused a stale automatic plan";
  render ~border:true changed;
  let stats=Scene_execution.retained_stats renderer in
  require(stats.plan_hits=2L&&stats.plan_misses=4L)
    "changed mixed plan was not admitted after two frames");
  with_renderer driver"release"(fun renderer->
  let releases=ref 0 in
  let release()=incr releases in
  let texture:Scene_execution.sampled_texture={key="lease-release";
    levels=[|{width=1;height=1;bytes=Bytes.of_string"\xff\xff\xff\xff"}|];
    sampler={label=None;min_filter=Nearest;mag_filter=Nearest;mip_filter=No_mip;
      address_u=Clamp_to_edge;address_v=Clamp_to_edge;lod_min=0.;lod_max=0.;
      max_anisotropy=1};gpu=None}in
  ignore(get(Scene_execution.submit
    ~after_prepare:release renderer
    [{Scene_execution.family=Scene2;blend=Ogpu.Pipeline.Replace;
      texture=Some texture;auxiliary=None;vertex_attributes=None;samples=1;
      draw={mesh=mesh 0;state}}]));
  require(!releases=1)"post-upload release hook did not run exactly once";
  let malformed={texture with levels=[||]}in
  (match Scene_execution.submit ~after_prepare:release renderer
      [{Scene_execution.family=Scene2;blend=Ogpu.Pipeline.Replace;
        texture=Some malformed;auxiliary=None;vertex_attributes=None;samples=1;
        draw={mesh=mesh 1;state}}]with
   |Error _->()|Ok _->failwith"malformed sampled texture unexpectedly rendered");
  require(!releases=2)"failed upload did not run release hook exactly once");
  with_renderer driver"viewport"(fun renderer->
  let oversized={state with viewport=(0,0,128,128);scissor=(16,-8,128,128)}in
  let pixel pixels x y=let at=(y*64+x)*4 in
    Char.code(Bytes.get pixels at),Char.code(Bytes.get pixels(at+1)),Char.code(Bytes.get pixels(at+2))in
  let read()=get(Scene_execution.read_pixels renderer~bytes_per_row:(64*4))in
  require(get(render_blended~clear:(0.,0.,0.,1.) renderer
    [Ogpu.Pipeline.Replace,{Scene_execution.mesh=mesh 0;state=oversized}]))
    "oversized viewport frame was skipped";
  (* The scissor is clipped to the target: columns 0-15 keep the clear. *)
  let pixels=read()in
  require(pixel pixels 15 0=(0,0,0)&&pixel pixels 15 63=(0,0,0))"scissor left edge was painted";
  require(pixel pixels 16 0=(255,255,255)&&pixel pixels 63 63=(255,255,255)&&pixel pixels 40 30=(255,255,255))
    "clipped viewport did not cover the target right of the scissor";
  let offset={state with viewport=(100,100,8,8);scissor=(100,100,8,8)}in
  require(get(render_blended~clear:(0.,0.,0.,1.) renderer
    [Ogpu.Pipeline.Replace,{Scene_execution.mesh=mesh 1;state=offset}]))
    "off-target frame was skipped";
  let pixels=read()in
  require(List.for_all(fun(x,y)->pixel pixels x y=(0,0,0))[0,0;31,31;63,63;16,0])
    "a draw entirely off the target painted the target";
  (* A viewport wider than the target is not squeezed into it: the target shows
     the half of a red|green image its pixels cover, at either overhang. *)
  let halves:Scene_execution.sampled_texture={key="viewport-halves";
    levels=[|{width=2;height=1;bytes=Bytes.of_string"\xff\x00\x00\xff\x00\xff\x00\xff"}|];
    sampler={label=None;min_filter=Nearest;mag_filter=Nearest;mip_filter=No_mip;
      address_u=Clamp_to_edge;address_v=Clamp_to_edge;lod_min=0.;lod_max=0.;
      max_anisotropy=1};gpu=None}in
  List.iteri(fun index(x,expected,label)->
    require(get(Scene_execution.submit~clear:(0.,0.,0.,1.)renderer
      [sampled~texture:halves Scene_execution.Scene2_textured Ogpu.Pipeline.Replace
        {mesh={(mesh(2+index)) with vertices=Scene_execution_fixtures.scene2_vertices~uv:(fun x _->(x+.1.)/.2.,0.)()};
         state={state with viewport=(x,0,128,64)}}]))
      "overhanging viewport frame was skipped";
    let pixels=read()in
    for y=0 to 63 do for x=0 to 63 do
      require(pixel pixels x y=expected)(Printf.sprintf"%s viewport half at %d,%d"label x y)
    done done)
    [0,(255,0,0),"right-overhanging";-64,(0,255,0),"left-overhanging"]);
  with_renderer driver"uniforms"(fun renderer->
  let affine offset=let bytes=Bytes.make 24 '\000'in
    Bytes.set_int32_le bytes 0(Int32.bits_of_float 1.);
    Bytes.set_int32_le bytes 16(Int32.bits_of_float 1.);
    Bytes.set_int32_le bytes 8(Int32.bits_of_float offset);bytes in
  let transforms=[|affine 0.;affine 1.|]in
  let uniform_draws()=Array.to_list(Array.mapi(fun index bytes->
    Ogpu.Pipeline.Replace,{Scene_execution.mesh=mesh index;
      state={state with transform_uniforms=Some bytes}})transforms)in
  ignore(get(render_blended renderer(uniform_draws())));
  let uploaded=Scene_execution.upload_bytes renderer in
  ignore(get(render_blended renderer(uniform_draws())));
  require(Scene_execution.upload_bytes renderer=uploaded)
    "unchanged uniforms were uploaded again";
  Bytes.set_int32_le transforms.(1) 8(Int32.bits_of_float 2.);
  ignore(get(render_blended renderer(uniform_draws())));
  require(Scene_execution.upload_bytes renderer=Int64.add uploaded 48L)
    "mutated input bytes were mistaken for the completed uniform page";
  for frame=3 to 7 do
    Bytes.set_int32_le transforms.(1) 8(Int32.bits_of_float(float frame));
    ignore(get(render_blended renderer(uniform_draws())))
  done);
  let after=live_handles()in
  require(after=before)(Printf.sprintf"automatic scratch leaked handles %d -> %d"before after)
