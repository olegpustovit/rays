open Ogpu_core
type queue_clock={mutable epoch:int64;mutable completed:int64}
type texture_storage={descriptor:Types.texture_descriptor;levels:bytes array}
type control={mutable next:int64;queue_clocks:(int64,queue_clock)Hashtbl.t;memory:(int64,bytes)Hashtbl.t;texture_storage:(int64,texture_storage)Hashtbl.t;lost:bool;mutable fail_submission:bool;mutable fail_completion:bool;mutable fail_texture_after:int option;mutable fail_depth_after:int option;mutable fail_configure:bool;mutable trace:string list;mutable buffers:int;mutable textures:int;mutable pipelines:int;mutable queues:int;mutable surfaces:int}
let error op kind text=Error(Error.make op kind text)
let add c text=c.trace<-text::c.trace
let token c=let value=c.next in c.next<-Int64.succ value;value
let mip_extent value mip=max 1 (value lsr mip)
let texture_level_size (d:Types.texture_descriptor) mip=
  let texels=Int64.mul
    (Int64.of_int(mip_extent d.width mip))
    (Int64.mul (Int64.of_int(mip_extent d.height mip))
      (Int64.of_int(mip_extent d.depth mip))) in
  let texel=Types.texel_bytes d.format in
  if texels>Int64.of_int(Sys.max_string_length/texel) then None
  else Some(Int64.to_int texels*texel)
(* Blit operations recorded by the mock encoder and replayed at commit. *)
type transfer=
  |Buffer_to_texture of int64*int64*int64*int64*int64*int*Types.origin*Types.extent

let execute_transfer c resources description =
  let op="Backend_mock.transfer" in
  let range bytes offset length =
    offset>=0L && length>=0L &&
    offset<=Int64.of_int(Bytes.length bytes) &&
    length<=Int64.sub (Int64.of_int(Bytes.length bytes)) offset in
  let product a b=
    if a<0L || b<0L || (b<>0L && a>Int64.div Int64.max_int b)
    then None else Some(Int64.mul a b) in
  let sum a b=
    if a<0L || b<0L || a>Int64.sub Int64.max_int b
    then None else Some(Int64.add a b) in
  let buffer_region texel bytes offset row image (extent:Types.extent)=
    let width=Int64.of_int extent.width in
    let height=Int64.of_int extent.height in
    let depth=Int64.of_int extent.depth in
    if width<=0L || height<=0L || depth<=0L then false
    else match product width (Int64.of_int texel),product row height with
      |Some pixels,Some minimum_image
        when row>=pixels && image>=minimum_image->
          (match product image (Int64.pred depth),
                 product row (Int64.pred height) with
           |Some slices,Some rows->
               (match sum slices rows with
                |None->false
                |Some span->match sum span pixels with
                  |None->false
                  |Some span->range bytes offset span)
           |_->false)
      |_->false in
  let bytes id=
    match List.assoc_opt id resources with
    |None->error op Error.Invalid_argument"command resource is absent"
    |Some token->match Hashtbl.find_opt c.memory token with
      |Some bytes->Ok bytes
      |None->error op Error.Stale_handle"command resource is destroyed" in
  let texture id=
    match List.assoc_opt id resources with
    |None->error op Error.Invalid_argument"command resource is absent"
    |Some token->match Hashtbl.find_opt c.texture_storage token with
      |Some storage->Ok storage
      |None->error op Error.Invalid_argument"resource is not a color texture" in
  let valid_region storage mip (origin:Types.origin)
      (extent:Types.extent)=
    mip>=0 && mip<Array.length storage.levels &&
    let width=mip_extent storage.descriptor.width mip in
    let height=mip_extent storage.descriptor.height mip in
    let depth=mip_extent storage.descriptor.depth mip in
    origin.x>=0 && origin.y>=0 && origin.z>=0 &&
    extent.width>0 && extent.height>0 && extent.depth>0 &&
    origin.x<=width-extent.width &&
    origin.y<=height-extent.height &&
    origin.z<=depth-extent.depth in
  let texture_offset storage mip (origin:Types.origin)=
    let width=mip_extent storage.descriptor.width mip in
    let height=mip_extent storage.descriptor.height mip in
    ((origin.z*height+origin.y)*width+origin.x)*Types.texel_bytes storage.descriptor.format in
  let copy_rows ~texel ~source ~source_offset ~source_row ~source_image
      ~target ~target_offset ~target_row ~target_image
      (extent:Types.extent)=
    for z=0 to extent.depth-1 do
      for y=0 to extent.height-1 do
        Bytes.blit source (source_offset+z*source_image+y*source_row)
          target (target_offset+z*target_image+y*target_row)
          (extent.width*texel)
      done
    done in
  begin match description with
          |Buffer_to_texture(src,offset,row,image,dst,mip,
              origin,extent)->
              Result.bind (bytes src) (fun source->
                Result.bind (texture dst) (fun target->
                  let texel=Types.texel_bytes target.descriptor.format in
                  if not(valid_region target mip origin extent &&
                    buffer_region texel source offset row image extent) then
                    error op Error.Invalid_argument"texture upload range is invalid"
                  else begin
                    let width=mip_extent target.descriptor.width mip in
                    let height=mip_extent target.descriptor.height mip in
                    copy_rows ~texel ~source ~source_offset:(Int64.to_int offset)
                      ~source_row:(Int64.to_int row)
                      ~source_image:(Int64.to_int image)
                      ~target:target.levels.(mip)
                      ~target_offset:(texture_offset target mip origin)
                      ~target_row:(width*texel) ~target_image:(width*height*texel)
                      extent;
                    Ok()
                  end))

  end
let transfer_one=execute_transfer
let create()=
  let capabilities={Caps.minimum_m1 with Caps.compute_pipeline=false;render_pipeline=false;ray_tracing=false;ray_tracing_curves=false;ray_tracing_motion=false} in
  let run_ops ops=
    let rec go=function
      |[]->Ok()
      |op::rest->(match op()with Ok()->go rest|Error _ as failure->failure)in
    go ops in

  let c={next=1L;queue_clocks=Hashtbl.create 4;memory=Hashtbl.create 16;texture_storage=Hashtbl.create 8;lost=false;fail_submission=false;fail_completion=false;fail_texture_after=None;fail_depth_after=None;fail_configure=false;trace=[];buffers=0;textures=0;pipelines=0;queues=0;surfaces=0}in
  let should_fail field=match field with Some 0->true|None|Some _->false in
  let advance field=match field with None->None|Some 0->None|Some remaining->Some(remaining-1)in
  let resource kind count set size=let id=token c and bytes=Bytes.make size '\000' in Hashtbl.add c.memory id bytes;set(count+1);add c(Printf.sprintf"create-%s:%Ld"kind id);let range offset length=offset>=0L&&length>=0&&length<=size&&offset<=Int64.of_int(size-length)in let read_into offset destination destination_offset length=if not(range offset length)||destination_offset<0||length>Bytes.length destination-destination_offset then error"Backend_mock.read_into"Error.Invalid_argument"range"else(Bytes.blit bytes(Int64.to_int offset)destination destination_offset length;Ok())in Ok{Backend.token=id;write=(fun offset source->if not(range offset(Bytes.length source))then error"Backend_mock.write"Error.Invalid_argument"range"else(Bytes.blit source 0 bytes(Int64.to_int offset)(Bytes.length source);Ok()));read=(fun offset length->if not(range offset length)then error"Backend_mock.read"Error.Invalid_argument"range"else Ok(Bytes.sub bytes(Int64.to_int offset)length));read_into;destroy=(fun()->Hashtbl.remove c.memory id;Hashtbl.remove c.texture_storage id;set((if kind="buffer"then c.buffers else c.textures)-1);add c(Printf.sprintf"destroy-%s:%Ld"kind id);Ok())}in
  let create_device()=let _device_token=token c and device_handle=Handle.create_device()in Ok{Backend.device_handle;capabilities;
    create_buffer=(fun _memory d->resource"buffer"c.buffers(fun n->c.buffers<-n)(Int64.to_int d.Types.size));
    create_library=(fun _shader->
      let id=token c in
      c.pipelines<-c.pipelines+1;
      add c(Printf.sprintf"create-library:%Ld"id);
      Ok{
        create_compute_pipeline_in=(fun ~entry:_ ~constants:_ ~interface:_->
          error"Backend_mock.create_compute_pipeline_in"Error.Unsupported
            "mock backend does not execute compute shaders");
        destroy_library=(fun()->c.pipelines<-c.pipelines-1;
          add c(Printf.sprintf"destroy-library:%Ld"id);Ok())});
    create_accel=(fun _->error"Backend_mock.create_accel"Error.Unsupported
      "mock backend has no ray tracing");
    create_sampler=(fun _->
      let id=token c in
      c.pipelines<-c.pipelines+1;
      add c(Printf.sprintf"create-sampler:%Ld"id);
      Ok{Backend.sampler_token=id;destroy_sampler=(fun()->c.pipelines<-c.pipelines-1;
        add c(Printf.sprintf"destroy-sampler:%Ld"id);Ok())});
    create_render_pipeline=(fun _ _->error"Backend_mock.create_render_pipeline"Error.Unsupported
      "mock backend does not rasterize");
    create_icb=(fun ~max_commands:_->error"Backend_mock.create_icb"Error.Unsupported
      "mock backend does not rasterize");
    create_argument=(fun ~pipeline:_ _ ~index:_->error"Backend_mock.create_argument"Error.Unsupported
      "mock backend does not rasterize");
    (* The mock mirrors Metal's documented instance record layouts. *)
    instance_layout=(function
      |Backend.User_id_instances->[|68;0;48;52;60;64;-1;-1;-1;-1;-1;-1|]
      |Motion_instances->[|44;-1;8;12;20;24;0;4;28;32;36;40|]);
    create_texture=(fun d->
      let operation="Backend_mock.create_texture" in
      if should_fail c.fail_texture_after then begin
        c.fail_texture_after<-None;
        error operation Error.Capacity"injected texture allocation failure"
      end else begin
        c.fail_texture_after<-advance c.fail_texture_after;
        match texture_level_size d 0 with
        |None->error operation Error.Capacity"texture exceeds mock byte capacity"
        |Some size->
            match resource"texture"c.textures(fun n->c.textures<-n)size with
            |Error _ as failure->failure
            |Ok raw->
                (try
                   let levels=Array.init d.mip_levels (fun mip->
                     if mip=0 then Hashtbl.find c.memory raw.token
                     else match texture_level_size d mip with
                       |None->raise Out_of_memory
                       |Some size->Bytes.make size '\000') in
                   Hashtbl.add c.texture_storage raw.token
                     {descriptor=d;levels};
                   Ok raw
                 with Out_of_memory ->
                   ignore(raw.destroy());
                   error operation Error.Capacity
                     "texture exceeds mock byte capacity")
      end);
    create_depth_texture=(fun _d->if should_fail c.fail_depth_after then(c.fail_depth_after<-None;error"Backend_mock.create_depth_texture"Error.Capacity"injected depth allocation failure")else(c.fail_depth_after<-advance c.fail_depth_after;let id=token c in c.textures<-c.textures+1;add c(Printf.sprintf"create-depth-texture:%Ld"id);Ok{Backend.token=id;write=(fun _ _->error"Backend_mock.depth.write"Error.Unsupported"depth textures are not host writable");read=(fun _ _->error"Backend_mock.depth.read"Error.Unsupported"depth textures are not host readable");read_into=(fun _ _ _ _->error"Backend_mock.depth.read_into"Error.Unsupported"depth textures are not host readable");destroy=(fun()->c.textures<-c.textures-1;add c(Printf.sprintf"destroy-depth-texture:%Ld"id);Ok())}));
    create_queue=(fun()->
      let id=token c and clock={epoch=0L;completed=0L} in
      let run=run_ops in
      c.queues<-c.queues+1;
      Hashtbl.add c.queue_clocks id clock;
      let complete_through epoch=
        if epoch<=clock.completed||epoch>clock.epoch then
          error"Backend_mock.complete"Error.Invalid_argument"epoch is invalid"
        else begin
          clock.completed<-epoch;
          add c(Printf.sprintf"complete:%Ld"epoch);
          if c.fail_completion then begin
            c.fail_completion<-false;
            error"Backend_mock.complete"Error.Device_lost
              "injected terminal completion failure"
          end else Ok()
        end in
      let poll_through epoch=
        if epoch<=0L then
          error"Backend_mock.poll"Error.Invalid_argument"epoch is invalid"
        else if epoch<=clock.completed then Ok true
        else Result.map(fun()->true)(complete_through epoch) in
      let begin_commands()=
        let commands=token c and ops=ref[] in
        let blit_encoder()=
          Ok{

            Backend.buffer_to_texture=(fun ~src ~offset ~bytes_per_row ~bytes_per_image ~dst ~mip ~origin ~extent->
              ops:=(fun()->transfer_one c[src,src;dst,dst]
                (Buffer_to_texture(src,offset,bytes_per_row,bytes_per_image,dst,mip,origin,extent)))::!ops;Ok());

            end_blit=(fun()->Ok())}in
        let render_encoder _target=
          error"Backend_mock.render_encoder"Error.Unsupported
            "mock backend does not rasterize"in
        let commit_present ~source (frame:Backend.driver_frame)=
          let operation="Backend_mock.commit_present"in
          if c.fail_submission then begin
            c.fail_submission<-false;
            error operation Error.Invalid_state"injected submission failure"
          end
          else if c.lost then error operation Error.Device_lost"injected device loss"
          else match run(List.rev !ops)with
            |Error _ as failure->failure
            |Ok()->
                clock.epoch<-Int64.succ clock.epoch;
                add c(Printf.sprintf"commit-present:%Ld:%Ld:%Ld:%Ld"commands source frame.frame_token clock.epoch);
                Ok{Backend.epoch=clock.epoch}in
        let commit()=
          let operation="Backend_mock.commit"in
          if c.fail_submission then begin
            c.fail_submission<-false;
            error operation Error.Invalid_state"injected submission failure"
          end
          else if c.lost then error operation Error.Device_lost"injected device loss"
          else match run(List.rev !ops)with
            |Error _ as failure->failure
            |Ok()->
                clock.epoch<-Int64.succ clock.epoch;
                add c(Printf.sprintf"commit:%Ld:%Ld"commands clock.epoch);
                Ok{Backend.epoch=clock.epoch}in
        Ok{
          Backend.compute_encoder=(fun _->error"Backend_mock.compute_encoder"Error.Unsupported
            "mock backend does not execute compute shaders");
          accel_encoder=(fun()->error"Backend_mock.accel_encoder"Error.Unsupported
            "mock backend has no ray tracing");
          blit_encoder;render_encoder;

          commit;commit_present;abandon=(fun()->ops:=[];Ok())}in
      Ok{Backend.complete_through;poll_through;
        completed_epoch=(fun()->clock.completed);
        begin_commands;

        gpu_duration=(fun _->None);
        gpu_timing=(fun()->{Backend.gpu_seconds=0.});
        destroy_queue=(fun()->c.queues<-c.queues-1;Hashtbl.remove c.queue_clocks id;Ok())});
    create_surface=(fun _->
      let _id=token c and frame=ref 0L in
      c.surfaces<-c.surfaces+1;
      let acquire()=if c.lost then Ok`Device_lost else(frame:=Int64.succ !frame;Ok(`Acquired({Backend.frame_token = !frame}:Backend.driver_frame)))in Ok({configure=(fun _->if c.fail_configure then(c.fail_configure<-false;error"Backend_mock.configure"Error.Invalid_state"injected configure failure")else Ok());acquire_sync=acquire;discard=(fun _->add c"discard";Ok());destroy_surface=(fun()->c.surfaces<-c.surfaces-1;Ok())}:Backend.driver_surface));

    destroy_device=(fun()->Handle.destroy_device device_handle;add c"destroy-device";Ok())}in
  {Backend.create_device},c
let live_counts c=c.buffers,c.textures,c.pipelines,c.queues,c.surfaces
