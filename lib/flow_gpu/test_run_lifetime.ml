module B=Ogpu.Backend
module R=Flow_gpu.Run
let get=function Ok value->value|Error error->failwith(Ogpu.Error.to_string error)
let wrong_domain f=Domain.join(Domain.spawn(fun()->try f();false with Invalid_argument _->true))
let ()=
  let base = Ogpu.Impl.create_driver () in let live = Ogpu.Impl.live_handles in
  let fail=ref false and pipelines=ref 0 and submitted=ref 0 and invalid_output=ref false in
  let fail_table=ref false and table_writes=ref 0 in
  let fail_input=ref false and input_writes=ref 0 in
  (* This driver checks ownership and failed-command lifetimes only. It does
     not execute a shader or stand in for native numerical qualification. *)
  let driver=B.{create_device=(fun()->Result.map(fun raw->{raw with
    capabilities={raw.capabilities with compute_pipeline=true};
    create_buffer=(fun memory descriptor->Result.map(fun(resource:B.driver_resource)->
      {resource with write=(fun offset bytes->
        if descriptor.Ogpu.Types.size=2048L then begin
          incr table_writes;
          if !fail_table then Error(Ogpu.Error.make "test" Invalid_state "injected table upload")
          else resource.write offset bytes
        end else if descriptor.Ogpu.Types.size>=4096L then begin
          incr input_writes;
          let result=resource.write offset bytes in
          if !fail_input then Error(Ogpu.Error.make "test" Invalid_state "injected partial input upload")
          else result
        end else resource.write offset bytes);
        read=(fun offset length->if !invalid_output && length=4 then
        let flags=Bytes.make 4 '\000'in Bytes.set_int32_le flags 0 1l;Ok flags
        else resource.read offset length)})(raw.create_buffer memory descriptor));
    create_library=(fun shader->Result.map(fun library->{library with
      create_compute_pipeline_in=(fun ~entry:_ ~constants:_ ~interface:_ ->
        incr pipelines;Ok{max_threads=1024;pipeline_token=1L;
          destroy_pipeline=(fun()->decr pipelines;Ok())})})(raw.create_library shader));
    create_queue=(fun()->Result.map(fun queue->{queue with
      complete_through=(fun epoch->if !fail then Error(Ogpu.Error.make "test" Invalid_state "injected completion")
        else queue.complete_through epoch);
      begin_commands=(fun()->Result.map(fun commands->{commands with
        commit=(fun()->incr submitted;commands.commit());
        compute_encoder=(fun()->Ok{
          set_pipeline=(fun _->Ok());set_buffer=(fun ~index:_ ~offset:_ _->Ok());
          set_bytes=(fun ~index:_ _->Ok());set_texture=(fun ~index:_ _->assert false);
          set_accel=(fun ~index:_ _->assert false);
          compute_use_accels=(fun _->assert false);dispatch_threads=(fun ~threads:_ ~threadgroup:_->Ok());
          end_compute=(fun()->Ok())})})(queue.begin_commands()))})(raw.create_queue()))})(base.create_device()))}in
  let device=get(B.create_device driver)in
  let queue=get(B.create_queue device)in
  let cache=Flow_gpu.Pipelines.create ~clock:Unix.gettimeofday device in
  let packed=List.assoc "arithmetic"(Test_program.fixtures 1024)in
  let msl=Test_program.ok(Flow_gpu.Emit.kernel packed)in
  let run=R.Private.create_owned device queue cache msl in
  Fun.protect ~finally:(fun()->R.close run;Flow_gpu.Pipelines.close cache;
    ignore(B.destroy_queue queue);ignore(B.destroy_device device))(fun()->
    let values=Test_program.ok(Flow_ir.Packed.Host.prepare packed ~live:(Input.at_time 1.))in
    let first=Test_program.ok(R.dispatch run values)in
    assert(R.buffer first<>None);
    assert(Domain.join(Domain.spawn(fun()->R.buffer first=None)));
    assert(wrong_domain(fun()->R.close run));
    assert(wrong_domain(fun()->Flow_gpu.Pipelines.close cache));
    ignore(Test_program.ok(R.dispatch run values));
    assert(!input_writes=1 && R.Private.input_uploads run=1);
    let replacement={values with arrays=Array.map Array.copy values.arrays}in
    replacement.arrays.(0).(0)<-17.;
    fail_input:=true;
    assert(Result.is_error(R.dispatch run replacement));
    assert(!input_writes=2 && R.Private.input_uploads run=1);
    fail_input:=false;
    ignore(Test_program.ok(R.dispatch run values));
    assert(!input_writes=3);
    ignore(Test_program.ok(R.dispatch run replacement));
    ignore(Test_program.ok(R.dispatch run replacement));
    assert(!input_writes=4);
    ignore(Test_program.ok(R.dispatch run {replacement with count=512}));
    ignore(Test_program.ok(R.dispatch run replacement));
    assert(!input_writes=6 && R.Private.input_uploads run=5);
    assert(R.Private.input_uploaded_bytes run=5*4096);
    fail:=true;
    let larger=List.assoc "arithmetic"(Test_program.fixtures 65536)in
    let values=Test_program.ok(Flow_ir.Packed.Host.prepare larger ~live:(Input.at_time 1.))in
    assert(Result.is_error(R.dispatch run values));
    assert(R.buffer first=None && Result.is_error(R.readback first));
    assert(R.Private.buffer_creations run=5 && !submitted=8 && !input_writes=7);
    fail:=false;
    let fresh=Test_program.ok(R.dispatch run values)in
    assert(R.buffer fresh<>None);
    invalid_output:=true;
    assert(match R.dispatch run values with Error d->d.Diagnostic.code="E_KERNEL"|Ok _->false);
    assert(R.buffer fresh=None);
    invalid_output:=false;
    let invalid={values with arrays=Array.map Array.copy values.arrays}in
    invalid.arrays.(0).(0)<-3.5e38;
    assert(Result.is_error(R.dispatch run invalid));
    assert(R.buffer fresh=None);
    R.close run;R.close run;
    assert(R.buffer fresh=None);
    let packed=List.assoc "noise"(Test_program.fixtures 1024)in
    let noise=R.Private.create_owned device queue cache (Test_program.ok(Flow_gpu.Emit.kernel packed))in
    Fun.protect ~finally:(fun()->R.close noise)(fun()->
      let values=Test_program.ok(Flow_ir.Packed.Host.prepare packed ~live:(Input.at_time 1.))in
      fail_table:=true;
      assert(Result.is_error(R.dispatch noise values));
      assert(!table_writes=1);
      fail_table:=false;
      ignore(Test_program.ok(R.dispatch noise values));
      assert(!table_writes=2);
      ignore(Test_program.ok(R.dispatch noise values));
      assert(!table_writes=2)));
  assert(live()=0 && !pipelines=0);
  print_endline "GPU output lifetimes: failed completion/reallocation invalidation, float32 input bounds and creating-domain close passed"
