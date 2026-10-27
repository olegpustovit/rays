open Metal
let get=function Ok x->x|Error e->failwith(Format.asprintf"%a"pp_error e)
let run () =match Device.system_default()with Error _->print_endline"compute pipeline: skipped"|Ok device->
  let source="#include <metal_stdlib>\nusing namespace metal; kernel void increment_output(device uint *out [[buffer(0)]], uint i [[thread_position_in_grid]]) { out[i] = i + 7; }"in
  let library=get(Library.compile_source~device source)in let function_=get(Function.find~library "increment_output")in
  let pipeline=get(Compute_pipeline.create~reflection:true function_)in
  (match Compute_pipeline.bindings pipeline with Some(_::_)->()|_->failwith"missing immutable reflection snapshot");
  let plain=get(Compute_pipeline.create function_)in
  (match Compute_pipeline.bindings plain with
   |None->()|Some _->failwith"reflection was kept without being asked for");
  get(Compute_pipeline.destroy plain);
  let queue=get(Command_queue.create device)in let buffer=get(Buffer.create~device~length:16L~storage:Buffer.Shared())in
  let command=get(Command_buffer.create queue())in let encoder=get(Compute_encoder.create command)in
  get(Compute_encoder.set_pipeline encoder pipeline);get(Compute_encoder.set_buffer encoder~index:0~offset:0L buffer);
  get(Compute_encoder.dispatch_threads encoder~threads:(4,1,1)~threadgroup:(1,1,1));get(Compute_encoder.end_encoding encoder);
  get(Command_buffer.commit command);get(Command_buffer.wait_until_completed command);
  let bytes=get(Buffer.read_bytes buffer~offset:0L~length:16)in for i=0 to 3 do if Bytes.get_int32_le bytes(i*4)<>Int32.of_int(i+7)then failwith"compute readback mismatch"done;
  get(Command_buffer.destroy command);get(Buffer.destroy buffer);get(Compute_pipeline.destroy pipeline);get(Function.destroy function_);get(Library.destroy library);get(Command_queue.destroy queue);get(Device.destroy device);print_endline"compute pipeline safe: reflection/dispatch ok"
