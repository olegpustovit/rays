open Ogpu_metal_native
let get = function Ok value->value|Error value->failwith(Ogpu.Error.to_string value)
let get_metal = function Ok value->value|Error value->failwith(Format.asprintf"%a"Metal.pp_error value)
let expect kind=function Error value when value.Ogpu.Error.kind=kind->()|Error value->failwith(Ogpu.Error.to_string value)|Ok _->failwith"unexpected acceleration success"
let triangle=let bytes=Bytes.make 36 '\000'in Array.iteri(fun i v->Bytes.set_int32_le bytes(i*4)(Int32.bits_of_float v))[|-1.;-1.;0.;1.;-1.;0.;0.;1.;0.|];bytes
let run () =
  let device=get(Device.system_default())and other=get(Device.system_default())in
  let before=get_metal(Metal.Release_queue.stats())in
  let vertices=get(Buffer.create device~memory:Buffer.Shared{Ogpu.Types.size=36L;usage=[Storage;Vertex];label=Some"triangle"})in
  get(Buffer.write_bytes device vertices~dst_offset:0L triangle);
  let buffers=Hashtbl.create 4 and structures=Hashtbl.create 4 in
  Hashtbl.add buffers 1L vertices;
  let resolve_buffer token=match Hashtbl.find_opt buffers token with Some b->Ok b|None->Error(Ogpu.Error.make"test"Ogpu.Error.Invalid_argument"unknown buffer")in
  let resolve_structure token=match Hashtbl.find_opt structures token with Some s->Ok s|None->Error(Ogpu.Error.make"test"Ogpu.Error.Invalid_argument"unknown structure")in
  let create descriptor=Acceleration.create device descriptor~resolve_buffer~resolve_structure in
  let triangles=Ogpu.Backend.Driver_triangles{vertices=1L;offset=0L;length=36L;vertex_stride=12;vertex_count=3}in
  if(Device.capabilities device).Ogpu.Caps.ray_tracing then begin
    let structure=get(create(Driver_blas{geometries=[|triangles|];motion=None}))in
    Hashtbl.add structures 10L structure;
    expect Ogpu.Error.Invalid_argument(create(Driver_blas{geometries=[|Driver_triangles{vertices=1L;offset=0L;length=36L;vertex_stride=12;vertex_count=6}|];motion=None}));
    let scratch=get(Buffer.create device~memory:Buffer.Device_local{Ogpu.Types.size=1048576L;usage=[Storage];label=Some"scratch"})in
    let queue=get(Queue.create device)in
    let commands=get_metal(Metal.Command_buffer.create(Queue.Private.metal queue)())in
    let encoder=get_metal(Metal.Acceleration_encoder.create commands)in
    expect Ogpu.Error.Cross_device(Acceleration.encode_build encoder other structure~scratch~scratch_offset:0L);
    get(Acceleration.encode_build encoder device structure~scratch~scratch_offset:0L);
    let instances=get(Buffer.create device~memory:Buffer.Shared{Ogpu.Types.size=68L;usage=[Storage];label=Some"instances"})in
    Hashtbl.add buffers 2L instances;
    get(Buffer.write_bytes device instances~dst_offset:0L(Ogpu.Acceleration.pack_instances[|68;0;48;52;60;64;-1;-1;-1;-1;-1;-1|][|[|1.;0.;0.;0.;0.;1.;0.;0.;0.;0.;1.;0.|],0xFFFF_FFFF,0,7|]));
    let top=get(create(Driver_tlas{instances=2L;offset=0L;instance_count=1;kind=User_id_instances;structures=[|10L|];motion_transforms=None}))in
    Hashtbl.add structures 11L top;
    expect Ogpu.Error.Invalid_argument(create(Driver_tlas{instances=2L;offset=0L;instance_count=1;kind=User_id_instances;structures=[|11L|];motion_transforms=None}));
    get(Acceleration.encode_build encoder device top~scratch~scratch_offset:0L);
    let sized=get(create(Driver_sized{size=(Acceleration.sizes structure).acceleration_structure_size;template=10L}))in
    expect Ogpu.Error.Invalid_state(Acceleration.encode_build encoder device sized~scratch~scratch_offset:0L);
    get_metal(Metal.Acceleration_encoder.end_encoding encoder);
    let release=get(Acceleration.Private.retain_submission top)in
    let release_sized=get(Acceleration.Private.retain_submission sized)in
    let receipt=get(Queue.submit_native queue commands~retained:[release;release_sized])in
    get(Queue.wait_through queue receipt.epoch);
    get(Acceleration.destroy sized);get(Acceleration.destroy top);get(Acceleration.destroy structure);
    get(Buffer.destroy instances);get(Buffer.destroy scratch);get(Queue.destroy queue)
  end else begin
    expect Ogpu.Error.Unsupported(create(Driver_blas{geometries=[|triangles|];motion=None}))
  end;
  get(Buffer.destroy vertices);get(Device.destroy device);get(Device.destroy other);ignore(get_metal(Metal.Release_queue.drain()));
  let after=get_metal(Metal.Release_queue.stats())in if after.live_handles<>before.live_handles-2 then failwith"acceleration live-handle delta";
  print_endline"ogpu_metal acceleration: generic build for primitives and user-id instances, typed rejections, zero delta ok"
