let get=function Ok value->value|Error error->failwith(Ogpu.Error.to_string error)
let expect kind=function
  |Error error when error.Ogpu.Error.kind=kind->()
  |Error error->failwith(Ogpu.Error.to_string error)
  |Ok _->failwith"GPU vertex guard accepted invalid resource"
let ()=
  let base,live=Ogpu.Impl.create_driver()in
  (* Record native command bindings and ownership, without rasterizing. *)
  let pipelines=ref 0 and next=ref 1_000_000L in
  let indices_seen=ref[]and vertex_bindings=ref[]and producer=ref 0L and mesh_uploads=ref 0 in
  let encoder:Ogpu.Backend.driver_render_encoder={
    render_set_pipeline=(fun _->Ok());
    set_stage_buffer=(fun stage~index~offset:_ buffer->
      if stage=Vertex then indices_seen:=index:: !indices_seen;
      if stage=Vertex&&index=0 then vertex_bindings:=buffer::!vertex_bindings;Ok());
    set_stage_texture=(fun _~index:_ _->Ok());set_stage_sampler=(fun _~index:_ _->Ok());
    set_viewport=(fun _->Ok());set_scissor=(fun _->Ok());set_cull=(fun _->Ok());
    set_winding=(fun _->Ok());set_depth_state=(fun _->Ok());
    draw=(fun~primitive:_~first:_~count:_~instances:_->Ok());
    draw_indexed=(fun~primitive:_~index_type:_ _~offset:_~count:_~instances:_->Ok());
    draw_indexed_indirect=(fun~primitive:_~index_type:_ _~offset:_ _~arguments_offset:_->Ok());
    draw_batch=(fun draws->Array.iter(fun(draw:Ogpu.Backend.driver_batch_draw)->
      Array.iter(fun(stage,index,buffer,_)->
        if stage=Ogpu.Backend.Vertex then indices_seen:=index:: !indices_seen;
        if stage=Ogpu.Backend.Vertex&&index=0 then
        vertex_bindings:=buffer::!vertex_bindings)draw.batch_buffers)draws;Ok());
    use_resources=(fun _->Ok());execute_icb=(fun _~location:_~length:_->Ok());
    end_render=(fun()->Ok())}in
  let driver:Ogpu.Backend.driver={create_device=(fun()->Result.map(fun(raw:Ogpu.Backend.driver_device)->
    {raw with capabilities={raw.capabilities with render_pipeline=true};
      create_buffer=(fun memory descriptor->Result.map(fun buffer->
        if descriptor.Ogpu.Types.label=Some"producer-instances"then producer:=buffer.Ogpu.Backend.token;
        if Option.fold~none:false~some:(String.starts_with~prefix:"scene-mesh-")descriptor.label then
          {buffer with write=(fun offset bytes->mesh_uploads:= !mesh_uploads+Bytes.length bytes;
            buffer.write offset bytes)}else buffer)(raw.create_buffer memory descriptor));
      create_render_pipeline=(fun _ _->incr pipelines;next:=Int64.succ !next;
        Ok{Ogpu.Backend.pipeline_token= !next;max_threads=1024;
          destroy_pipeline=(fun()->decr pipelines;Ok())});
      create_queue=(fun()->Result.map(fun(queue:Ogpu.Backend.driver_queue)->{queue with
        begin_commands=(fun()->Result.map(fun(commands:Ogpu.Backend.driver_commands)->{commands with
          render_encoder=(fun _->Ok encoder)})(queue.begin_commands()))})
        (raw.create_queue()))})(base.create_device()))}in
  let device=get(Ogpu.Backend.create_device driver)in
  let other=get(Ogpu.Backend.create_device driver)in
  let buffer=get(Ogpu.Backend.create_buffer device
    {label=Some"producer-instances";size=128L;usage=[Vertex;Storage;Copy_dst]})in
  let other_buffer=get(Ogpu.Backend.create_buffer other
    {label=Some"producer-instances-other";size=128L;usage=[Vertex;Storage;Copy_dst]})in
  let configuration:Ogpu.Surface.configuration={logical_width=16;logical_height=16;
    physical_width=16;physical_height=16;format=Bgra8_unorm;present_mode=Fifo;
    max_acquired=1;layer=None}in
  let renderer=get(Scene_execution.create~device~offscreen:true driver configuration
    Scene_execution_fixtures.make)in
  let owner=Scene_execution.Private.gpu_vertices_owner renderer in
  let other_renderer=get(Scene_execution.create~device:other~offscreen:true driver configuration
    Scene_execution_fixtures.make)in
  let other_owner=Scene_execution.Private.gpu_vertices_owner other_renderer in
  let registration=ref None in
  Fun.protect~finally:(fun()->
    Option.iter Scene_execution.Private.unregister_gpu_vertices !registration;
    ignore(get(Scene_execution.destroy other_renderer));
    ignore(get(Scene_execution.destroy renderer));
    ignore(get(Ogpu.Backend.destroy_buffer other_buffer));
    ignore(get(Ogpu.Backend.destroy_buffer buffer));
    ignore(get(Ogpu.Backend.destroy_device other));
    ignore(get(Ogpu.Backend.destroy_device device))) (fun()->
    let alive=ref true in
    let key="gpu:vertices:ownership-test"in
    let register device=get(Scene_execution.Private.register_gpu_vertices owner
      ~key~device~buffer~vertex_count:8~valid:(fun()-> !alive)())in
    let source=register device in registration:=Some source;
    expect Invalid_argument(Scene_execution.Private.register_gpu_vertices owner
      ~key~device~buffer~vertex_count:8~valid:(fun()->true)());
    let indices=Bytes.create 48 in
    Array.iteri(fun i value->Bytes.set_int32_le indices(i*4)(Int32.of_int value))
      [|0;1;2;0;2;3;4;5;6;4;6;7|];
    let affine=Bytes.make 24 '\000'in
    Bytes.set_int32_le affine 0(Int32.bits_of_float 1.);
    Bytes.set_int32_le affine 16(Int32.bits_of_float 1.);
    let mesh:Scene_execution.mesh={key;vertices=Bytes.empty;vertex_count=8;
      indices;index_count=12;primitive=Triangle_list}in
    let state:Scene_execution.state={viewport=(0,0,16,16);scissor=(2,3,10,11);
      cull=Cull_none;depth_compare=Always;depth_write=false;depth_load=Load;
      depth_clear=1.;transform_uniforms=Some affine}in
    let texture:Scene_execution.sampled_texture={key="gpu-vertex-white";
      levels=[|{width=1;height=1;bytes=Bytes.make 4 '\255'}|];
      sampler={label=None;min_filter=Nearest;mag_filter=Nearest;mip_filter=No_mip;
        address_u=Clamp_to_edge;address_v=Clamp_to_edge;lod_min=0.;lod_max=0.;
        max_anisotropy=1};gpu=None}in
    let entry:Scene_execution.sampled_draw={family=Ui;blend=Alpha;texture=Some texture;
      auxiliary=None;vertex_attributes=None;samples=1;draw={mesh;state}}in
    let render()=Scene_execution.submit
      ~prepared:("gpu-vertex-plan",1L) renderer[entry]in
    assert(get(render()));
    assert(!vertex_bindings<>[]&&List.for_all((=) !producer)!vertex_bindings);
    let uploaded=Scene_execution.upload_bytes renderer in
    assert(uploaded>0L&& !mesh_uploads=48);
    assert(get(render()));assert(Scene_execution.upload_bytes renderer=uploaded);
    assert(get(Scene_execution.replay_prepared_sampled_resources
      ~identity:"gpu-vertex-plan"~version:1L renderer)=Some(true,1));
    alive:=false;
    expect Stale_handle(Scene_execution.replay_prepared_sampled_resources
      ~identity:"gpu-vertex-plan"~version:1L renderer);
    alive:=true;
    let next_key="gpu:vertices:ownership-test-next"in
    let next_source=get(Scene_execution.Private.register_gpu_vertices owner
      ~key:next_key~device~buffer~vertex_count:8~valid:(fun()-> !alive)())in
    Fun.protect~finally:(fun()->Scene_execution.Private.unregister_gpu_vertices next_source)(fun()->
      let next={entry with draw={entry.draw with mesh={mesh with key=next_key}}}in
      assert(get(Scene_execution.submit
        ~prepared:("gpu-vertex-plan",2L) renderer[next]));
      assert(Scene_execution.upload_bytes renderer=uploaded&& !mesh_uploads=48));
    alive:=false;
    expect Stale_handle(Scene_execution.replay_prepared_sampled_resources
      ~identity:"gpu-vertex-plan"~version:2L renderer);
    expect Stale_handle(render());
    alive:=true;
    expect Stale_handle(Domain.join(Domain.spawn(fun()->render())));
    let uploaded=Scene_execution.upload_bytes renderer in
    List.iter(fun candidate->
      let releases=ref 0 in
      expect Invalid_argument(Scene_execution.submit
        ~after_prepare:(fun()->incr releases) renderer
        [{entry with draw={entry.draw with mesh=candidate}}]);
      assert(!releases=1&&Scene_execution.upload_bytes renderer=uploaded))[
      {mesh with indices=Bytes.make 47 '\000'};
      {mesh with key="cpu:index-overflow";vertices=Bytes.make 128 '\000';
        index_count=1 lsl(Sys.int_size-2);indices=Bytes.empty}
    ];
    let bounded=ref[]in
    Fun.protect~finally:(fun()->List.iter Scene_execution.Private.unregister_gpu_vertices !bounded)(fun()->
      for index=1 to 127 do bounded:=get(Scene_execution.Private.register_gpu_vertices owner
        ~key:("gpu:vertices:capacity:"^string_of_int index)~device~buffer
        ~vertex_count:4~valid:(fun()->true)())::!bounded done;
      assert(Scene_execution.Private.gpu_vertex_count_for_test owner=128);
      let other_source=get(Scene_execution.Private.register_gpu_vertices other_owner
        ~key:"gpu:vertices:capacity:other-device"~device:other~buffer:other_buffer
        ~vertex_count:4~valid:(fun()->true)())in
      assert(Scene_execution.Private.gpu_vertex_count_for_test other_owner=1);
      Scene_execution.Private.unregister_gpu_vertices other_source;
      expect Capacity(Scene_execution.Private.register_gpu_vertices owner
        ~key:"gpu:vertices:capacity:overflow"~device~buffer~vertex_count:4~valid:(fun()->true)()));
    let malformed={entry with draw={entry.draw with mesh={mesh with vertex_count=4}}}in
    expect Invalid_argument(Scene_execution.submit renderer[malformed]);
    (* A Marks draw needs exactly five extra buffers, a Ui draw none; the extras bind at 1 to 5. *)
    expect Invalid_argument(Scene_execution.submit renderer[{entry with family=Marks}]);
    let extra=List.init 5(fun _->get(Ogpu.Backend.create_buffer device
      {label=Some"marks-extra";size=64L;usage=[Vertex;Storage;Copy_dst]}))in
    let marks_key="gpu:vertices:marks-test"in
    let marks_source=get(Scene_execution.Private.register_gpu_vertices owner~extra
      ~key:marks_key~device~buffer~vertex_count:8~valid:(fun()->true)())in
    let marks_entry={entry with family=Marks;texture=None;
      draw={entry.draw with mesh={mesh with key=marks_key}}}in
    indices_seen:=[];
    assert(get(Scene_execution.submit renderer[marks_entry]));
    assert(List.for_all(fun i->List.mem i !indices_seen)[0;1;2;3;4;5;6]);
    expect Invalid_argument(Scene_execution.submit renderer[{marks_entry with family=Ui}]);
    expect Invalid_argument(Scene_execution.Private.register_gpu_vertices owner~extra:[]
      ~key:"gpu:vertices:marks-small"~device~buffer:(List.hd extra)~vertex_count:8~valid:(fun()->true)());
    Scene_execution.Private.unregister_gpu_vertices marks_source;
    List.iter(fun b->ignore(get(Ogpu.Backend.destroy_buffer b)))extra;
    Scene_execution.Private.unregister_gpu_vertices source;
    Scene_execution.Private.unregister_gpu_vertices source;
    expect Stale_handle(render());
    let source=register other in registration:=Some source;
    expect Cross_device(Scene_execution.submit renderer[entry]);
    Scene_execution.Private.unregister_gpu_vertices source;registration:=None;
    (* Closing a registration does not destroy its producer-owned buffer. *)
    assert(Bytes.length(get(Ogpu.Backend.read_buffer buffer~offset:0L~length:128))=128);
    assert(Scene_execution.Private.gpu_vertex_count_for_test owner=0));
  assert(live()=0);
  assert(!pipelines=0);
  print_endline"GPU vertices: borrowed ownership, upload reuse, stale replay, domain and device guards passed on mock"
