open Metal
let fail fmt=Printf.ksprintf failwith fmt
let get=function Ok x->x|Error e->fail "%s"(Format.asprintf "%a"pp_error e)
let expect kind=function Error e when e.kind=kind->()|Error e->fail "wrong error: %s"(Format.asprintf "%a"pp_error e)|Ok _->fail "expected rejection"
let source={|#include <metal_stdlib>
using namespace metal;
kernel void add_step(device uint *out [[buffer(0)]], constant uint &step [[buffer(1)]],
    uint i [[thread_position_in_grid]]) { out[i] += step; }
|}
(* One dispatch through every binding call the encoder keeps: the kernel adds
   the inline constant to each word, resources bound to an encoded command
   cannot be destroyed under it, and the result reads back. *)
let run () =
  match Device.system_default()with Error _->print_endline"compute encoder: skipped (no device)"|Ok device->
  let queue=get(Command_queue.create device)in
  let library=get(Library.compile_source~device source)in
  let function_=get(Function.find~library "add_step")in
  let pipeline=get(Compute_pipeline.create function_)in
  let buffer=get(Buffer.create~device~length:16L~storage:Buffer.Shared())in
  let initial=Bytes.create 16 in
  for i=0 to 3 do Bytes.set_int32_le initial(i*4)(Int32.of_int(i*10))done;
  get(Buffer.write_bytes buffer~dst_offset:0L initial);
  let texture=get(Texture.create~device(Texture.descriptor_2d~storage:Buffer.Shared~usage:[Texture.Shader_read]~format:Texture.Rgba8_unorm~width:1~height:1()))in
  let command=get(Command_buffer.create queue())in
  let encoder=get(Compute_encoder.create command)in
  expect Invalid_argument(Compute_encoder.set_buffer encoder~index:(-1)~offset:0L buffer);
  expect Invalid_argument(Compute_encoder.set_buffer encoder~index:0~offset:17L buffer);
  expect Invalid_argument(Compute_encoder.set_bytes encoder~index:1 Bytes.empty);
  expect Invalid_state(Compute_encoder.dispatch_threads encoder~threads:(4,1,1)~threadgroup:(1,1,1));
  get(Compute_encoder.set_pipeline encoder pipeline);
  expect Invalid_argument(Compute_encoder.dispatch_threads encoder~threads:(0,1,1)~threadgroup:(1,1,1));
  get(Compute_encoder.set_buffer encoder~index:0~offset:0L buffer);
  let step=Bytes.create 4 in Bytes.set_int32_le step 0 3l;
  get(Compute_encoder.set_bytes encoder~index:1 step);
  get(Compute_encoder.set_texture encoder~index:0 texture);
  get(Compute_encoder.dispatch_threads encoder~threads:(4,1,1)~threadgroup:(1,1,1));
  get(Compute_encoder.end_encoding encoder);
  expect Destroyed(Compute_encoder.dispatch_threads encoder~threads:(4,1,1)~threadgroup:(1,1,1));
  List.iter(fun destroy->expect Parent_has_dependents(destroy()))
    [(fun()->Buffer.destroy buffer);(fun()->Texture.destroy texture)];
  get(Command_buffer.commit command);get(Command_buffer.wait_until_completed command);get(Command_buffer.destroy command);
  let bytes=get(Buffer.read_bytes buffer~offset:0L~length:16)in
  for i=0 to 3 do if Bytes.get_int32_le bytes(i*4)<>Int32.of_int(i*10+3)then fail"compute output mismatch" done;
  get(Buffer.destroy buffer);get(Texture.destroy texture);get(Compute_pipeline.destroy pipeline);
  get(Function.destroy function_);get(Library.destroy library);get(Command_queue.destroy queue);get(Device.destroy device);
  print_endline"compute encoder safe: ok"
