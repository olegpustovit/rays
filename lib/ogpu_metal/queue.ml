type cleanup=unit->unit
type pending={epoch:int64;command:Metal.Command_buffer.t;cleanup:cleanup list}
type t={device:Device.t;metal:Metal.Command_queue.t;max_frames:int;
  mutable next_epoch:int64;mutable completed:int64;mutable active:int;
  mutable pending:pending list;
  mutable durations:(int64*float)list;mutable dead:bool}
type receipt={epoch:int64}
type gpu_timing={supported:bool;duration_seconds:float;sample_count:int64}
type gpu_timing_accumulator={mutable supported:bool;mutable duration_seconds:float;mutable sample_count:int64;mutable queues:int}
let gpu_timings:(int64,gpu_timing_accumulator)Hashtbl.t=Hashtbl.create 4
let gpu_timing_for_device device=
  let id=Device.id device in
  match Hashtbl.find_opt gpu_timings id with
  |Some value->{supported=value.supported;duration_seconds=value.duration_seconds;sample_count=value.sample_count}
  |None->{supported=false;duration_seconds=0.;sample_count=0L}
let valid_gpu_duration duration=Float.is_finite duration&&duration>0.
let record_gpu_duration device duration=
  match Hashtbl.find_opt gpu_timings(Device.id device)with
  |Some value when valid_gpu_duration duration->value.supported<-true;value.duration_seconds<-value.duration_seconds+.duration;value.sample_count<-Int64.succ value.sample_count
  |_->()
let error op kind message=Error(Ogpu_core.Error.make op kind message)
(* The last few completed epochs' GPU durations, for profiling callers. *)
let duration_capacity=16
let remember_duration value epoch duration=
  value.durations<-List.take duration_capacity((epoch,duration)::value.durations)
let gpu_duration value epoch=List.assoc_opt epoch value.durations
let admit value=
  let operation="Ogpu_metal.Queue.submit_native"in
  if Device.destroyed value.device then
    error operation Ogpu_core.Error.Device_lost"queue device is lost"
  else if value.active>=value.max_frames then
    error operation Ogpu_core.Error.Capacity"frames-in-flight capacity reached"
  else begin
    let epoch=value.next_epoch in
    value.next_epoch<-Int64.succ epoch;
    value.active<-value.active+1;
    Ok epoch
  end
let complete_epoch value epoch=
  let operation="Ogpu_metal.Queue.complete_through"in
  if epoch<value.completed||epoch>=value.next_epoch then
    error operation Ogpu_core.Error.Invalid_argument"completion epoch is invalid"
  else begin
    value.active<-max 0(value.active-Int64.to_int(Int64.sub epoch value.completed));
    value.completed<-epoch;
    Ok()
  end
let create device=
  let max_frames=3 in
  let op="Ogpu_metal.Queue.create"in
  if Device.destroyed device then error op Ogpu_core.Error.Stale_handle"device is destroyed"
  else match Metal.Command_queue.create(Device.Private.metal device)with
  |Error e->Error(Device.of_metal_error~operation:op e)
  |Ok metal->
    let id=Device.id device in
    (match Hashtbl.find_opt gpu_timings id with
     |Some timing->timing.queues<-timing.queues+1
     |None->Hashtbl.add gpu_timings id{supported=false;duration_seconds=0.;sample_count=0L;queues=1});
    Device.Private.attach_resource device;
    Ok{device;metal;max_frames;next_epoch=1L;completed=0L;active=0;pending=[];durations=[];dead=false}
let completed_epoch value=value.completed
(* Admits an externally encoded, uncommitted native command buffer. On any
   failure the caller still owns [native] and [retained]. *)
let submit_native value native ~retained=
  let op="Ogpu_metal.Queue.submit_native"in
  if value.dead then error op Ogpu_core.Error.Stale_handle"queue is destroyed"
  else match admit value with Error _ as e->e|Ok epoch->
    match Metal.Command_buffer.commit native with
    |Error e->(value.active<-value.active-1;value.next_epoch<-epoch;Error(Device.of_metal_error~operation:op e))
    |Ok()->value.pending<-value.pending@[{epoch;command=native;cleanup=retained}];Ok{epoch}
let wait_through value epoch=
  let op="Ogpu_metal.Queue.wait_through"in
  if epoch<=completed_epoch value||epoch>Int64.of_int max_int then
    error op Ogpu_core.Error.Invalid_argument"completion epoch is invalid"
  else
    let ready,later=List.partition(fun(pending:pending)->pending.epoch<=epoch)value.pending in
    if ready=[]then error op Ogpu_core.Error.Invalid_argument"epoch was not submitted"
    else
      let finish pending=
        let outcome=match Metal.Command_buffer.wait_until_completed pending.command with
          |Error native_error->Error(Device.of_metal_error~operation:op native_error)
          |Ok()->
            (match Metal.Command_buffer.diagnostics pending.command with
             |Ok diagnostics when diagnostics.gpu_end_time>=diagnostics.gpu_start_time->
                 let duration=diagnostics.gpu_end_time-.diagnostics.gpu_start_time in
                 record_gpu_duration value.device duration;
                 remember_duration value pending.epoch duration
             |Ok _|Error _->());
            Ok()in
        (* [wait_until_completed] returns only after a terminal native status,
           including its error result, so the handle and all submission-owned
           cleanup are safe to release. *)
        ignore(Metal.Command_buffer.destroy pending.command);
        List.iter(fun release->release())pending.cleanup;
        outcome in
      let first_error=List.fold_left(fun first pending->
        match first,finish pending with
        |None,Error failure->Some failure
        |first,_->first)None ready in
      value.pending<-later;
      let portable=complete_epoch value epoch in
      match first_error,portable with
      |Some failure,_->Error failure
      |None,result->result
let poll_through value epoch=
  let op="Ogpu_metal.Queue.poll_through"in
  if epoch<=0L then error op Ogpu_core.Error.Invalid_argument"completion epoch is invalid"
  else if epoch<=value.completed then Ok true
  else if epoch>=value.next_epoch then error op Ogpu_core.Error.Invalid_argument"epoch was not submitted"
  else
    let rec ready=function
      |[]->Ok true
      |({epoch=pending_epoch;_}:pending)::_ when pending_epoch>epoch->Ok true
      |pending::rest->
          (match Metal.Command_buffer.status pending.command with
           |Error native_error->Error(Device.of_metal_error~operation:op native_error)
           |Ok(Metal.Command_buffer.Completed|Metal.Command_buffer.Error _)->ready rest
           |Ok _->Ok false)in
    match ready value.pending with
    |Error _ as failure->failure
    |Ok false->Ok false
    |Ok true->Result.map(fun()->true)(wait_through value epoch)
let destroy value=
  let op="Ogpu_metal.Queue.destroy"in
  if value.dead then Ok()
  else if value.pending<>[]then error op Ogpu_core.Error.Invalid_state"queue has commands in flight"
  else match Metal.Command_queue.destroy value.metal with
    |Error e->Error(Device.of_metal_error~operation:op e)
    |Ok()->
      value.dead<-true;
      let id=Device.id value.device in
      (match Hashtbl.find_opt gpu_timings id with
       |Some timing->timing.queues<-timing.queues-1;if timing.queues<=0 then Hashtbl.remove gpu_timings id
       |None->());
      Device.Private.detach_resource value.device;Ok()
module Private=struct
  let metal value=value.metal
end
