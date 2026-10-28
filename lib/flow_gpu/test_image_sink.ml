module B=Ogpu.Backend
module S=Flow_gpu.Image_sink
let get=Test_program.ok
let native=function Ok x->x|Error e->failwith(Ogpu.Error.to_string e)
let injected ()=Error(Ogpu.Error.make "test" Invalid_state "injected image sink failure")
let ()=
  let base = Ogpu.Impl.create_driver () in let live = Ogpu.Impl.live_handles in
  let device=native(B.create_device base)in
  let queue=native(B.create_queue device)in
  assert(Result.is_error(S.Private.create_owned device queue));
  ignore(B.destroy_queue queue);ignore(B.destroy_device device);assert(live()=0);
  let fail_pipeline=ref false and fail_texture=ref false and fail_dispatch=ref false
  and fail_copy=ref false and fail_complete=ref false in
  let libraries=ref 0 and pipelines=ref 0 and reads=ref 0 and texture_reads=ref 0
  and abandoned=ref 0 and order=ref [] in
  let event name=order:=name::!order in
  (* The mock checks commands and lifetimes; native tests execute the shader. *)
  let driver=B.{create_device=(fun()->Result.map(fun raw->{raw with
    capabilities={raw.capabilities with compute_pipeline=true};
    create_buffer=(fun memory descriptor->Result.map(fun(resource:B.driver_resource)->
      {resource with read=(fun offset length->assert(length=4);incr reads;resource.read offset length)})
      (raw.create_buffer memory descriptor));
    create_texture=(fun descriptor->if !fail_texture then injected()else
      Result.map(fun(resource:B.driver_resource)->{resource with
        read=(fun offset length->incr texture_reads;resource.read offset length)})(raw.create_texture descriptor));
    create_library=(fun shader->Result.map(fun(library:B.driver_library)->incr libraries;{
      destroy_library=(fun()->decr libraries;library.destroy_library());
      create_compute_pipeline_in=(fun ~entry:_ ~constants:_ ~interface:_ ~linked:_->
        if !fail_pipeline then injected()else begin incr pipelines;
          Ok{max_threads=1024;pipeline_token=1L;create_table=(fun ~capacity:_->assert false);
            destroy_pipeline=(fun()->decr pipelines;Ok())}end)}) (raw.create_library shader));
    create_queue=(fun()->Result.map(fun queue->{queue with
      complete_through=(fun epoch->event "complete";if !fail_complete then injected()else queue.complete_through epoch);
      begin_commands=(fun()->Result.map(fun commands->{commands with
        commit=(fun()->event "commit";commands.commit());
        abandon=(fun()->incr abandoned;commands.abandon());
        compute_encoder=(fun()->event "compute";Ok{
          set_pipeline=(fun _->Ok());set_buffer=(fun ~index:_ ~offset:_ _->Ok());
          set_bytes=(fun ~index:_ _->Ok());set_texture=(fun ~index:_ _->assert false);
          set_accel=(fun ~index:_ _->assert false);set_table=(fun ~index:_ _->assert false);
          compute_use_accels=(fun _->assert false);
          dispatch_threads=(fun ~threads:_ ~threadgroup:_->event "dispatch";if !fail_dispatch then injected()else Ok());
          end_compute=(fun()->event "end_compute";Ok())});
        blit_encoder=(fun()->event "blit";Result.map(fun(encoder:B.driver_blit_encoder)->{
          buffer_to_texture=(fun ~src ~offset ~bytes_per_row ~bytes_per_image ~dst ~mip ~origin ~extent->
            event "copy";assert(bytes_per_row=Int64.of_int((extent.Ogpu.Types.width*4+255)/256*256));
            assert(bytes_per_image=Int64.mul bytes_per_row(Int64.of_int extent.height));
            if !fail_copy then injected()else encoder.buffer_to_texture
              ~src ~offset ~bytes_per_row ~bytes_per_image ~dst ~mip ~origin ~extent);
          end_blit=(fun()->event "end_blit";encoder.end_blit())})(commands.blit_encoder()))})
        (queue.begin_commands()))})(raw.create_queue()))})(base.create_device()))}in
  let device=native(B.create_device driver)in
  let queue=native(B.create_queue device)in
  fail_pipeline:=true;
  assert(Result.is_error(S.Private.create_owned device queue));
  assert(!libraries=0 && !pipelines=0);fail_pipeline:=false;
  let cache=Flow_gpu.Pipelines.create ~clock:Unix.gettimeofday device in
  let packed=List.assoc "vec4"(Test_program.vector_fixtures 195)in
  let run=Flow_gpu.Run.Private.create_owned device queue cache (Flow_gpu.Emit.kernel packed |> get)in
  let sink=S.Private.create_owned device queue |> get in
  Fun.protect ~finally:(fun()->S.close sink;Flow_gpu.Run.close run;Flow_gpu.Pipelines.close cache;
    ignore(B.destroy_queue queue);ignore(B.destroy_device device))(fun()->
    let inputs=Flow_ir.Packed.Private.prepare packed ~live:(Input.at_time 0.5) |> get in
    let input=Flow_gpu.Run.dispatch run inputs |> get in
    let convert width height=S.convert sink ~width ~height input in
    order:=[];let first=convert 65 3 |> get in
    assert(List.rev !order=["compute";"dispatch";"end_compute";"blit";"copy";"end_blit";"commit";"complete"]);
    let first_texture=S.texture first |> Option.get in
    let d=B.Private.texture_descriptor first_texture in
    assert(d.width=65 && d.height=3 && d.format=Rgba8_unorm
      && d.usage=[Texture_binding;Texture_copy_src;Texture_copy_dst]);
    let creates=S.Private.buffer_creations sink,S.Private.texture_creations sink in
    let next=convert 65 3 |> get in
    assert(S.texture first=None && Option.fold ~none:false ~some:((==)first_texture)(S.texture next));
    assert(creates=(S.Private.buffer_creations sink,S.Private.texture_creations sink));
    List.iter(fun(w,h)->assert(Result.is_error(convert w h))) [0,3;65,0;64,3;max_int,2];
    assert(creates=(S.Private.buffer_creations sink,S.Private.texture_creations sink));
    assert(S.texture next<>None);
    fail_texture:=true;assert(Result.is_error(convert 39 5));fail_texture:=false;
    assert(S.texture next=None && not(B.Private.texture_destroyed first_texture));
    fail_dispatch:=true;assert(Result.is_error(convert 65 3));fail_dispatch:=false;
    fail_copy:=true;assert(Result.is_error(convert 39 5));fail_copy:=false;
    fail_complete:=true;assert(Result.is_error(convert 39 5));fail_complete:=false;
    let recovered=convert 65 3 |> get in
    assert(Option.fold ~none:false ~some:((==)first_texture)(S.texture recovered) && !abandoned=2);
    let resized=convert 39 5 |> get in
    assert(S.texture recovered=None && B.Private.texture_destroyed first_texture);
    assert(S.texture resized<>None && !reads=1 && !texture_reads=0);
    assert(Flow_gpu.Run.Private.status_reads run= !reads && Flow_gpu.Run.Private.readback_bytes run=0);
    assert(Domain.join(Domain.spawn(fun()->S.texture resized=None)));
    assert(Domain.join(Domain.spawn(fun()->Result.is_error(convert 39 5))));
    assert(Domain.join(Domain.spawn(fun()->try S.close sink;false with Invalid_argument _->true)));
    assert(Domain.join(Domain.spawn(fun()->Result.is_error(S.Private.create_owned device queue))));
    let scalar=List.assoc "arithmetic"(Test_program.fixtures 195)in
    let scalar_run=Flow_gpu.Run.Private.create_owned device queue cache (Flow_gpu.Emit.kernel scalar |> get)in
    Fun.protect ~finally:(fun()->Flow_gpu.Run.close scalar_run)(fun()->
      let values=Flow_ir.Packed.Private.prepare scalar ~live:(Input.at_time 0.5) |> get in
      let output=Flow_gpu.Run.dispatch scalar_run values |> get in
      assert(Result.is_error(S.convert sink ~width:65 ~height:3 output)));
    assert(S.texture resized<>None);
    ignore(Flow_gpu.Run.dispatch run inputs |> get);
    assert(Result.is_error(convert 39 5));
    S.close sink;S.close sink;assert(S.texture resized=None && Result.is_error(convert 39 5)));
  assert(live()=0 && !libraries=0 && !pipelines=0);
  print_endline "GPU image sink: mock copy order, padded rows, reuse, transactional resize/failure cleanup, generations and zero pixel reads pass"
