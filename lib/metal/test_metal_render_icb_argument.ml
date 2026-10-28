open Metal

let get = function Ok value -> value | Error error -> failwith error.message

let source =
  {|
#include <metal_stdlib>
using namespace metal;
struct O { float4 position [[position]]; };
struct A { texture2d<float,access::sample> image [[id(0)]]; sampler sampling [[id(1)]]; };
vertex O vertex_main(device const float2 *p [[buffer(0)]], uint i [[vertex_id]]) {
  O o; o.position=float4(p[i],0.,1.); return o;
}
fragment float4 fragment_main(O o [[stage_in]], constant A& args [[buffer(1)]]) {
  return args.image.sample(args.sampling,float2(.5));
}
fragment float4 fragment_solid(O o [[stage_in]]) { return float4(.25,.5,.75,1.); }
fragment float4 fragment_direct(O o [[stage_in]],texture2d<float> image [[texture(0)]],sampler sampling [[sampler(1)]]) { return image.sample(sampling,float2(.5)); }
|}

let run () =
  let device = get (Device.system_default ()) in
  let library = get (Library.compile_source ~device source) in
  let fragment = get (Function.find ~library "fragment_main") in
  let argument_encoder = get (Function.argument_encoder fragment ~buffer_index:1L) in
  let vertices = Bytes.create 24 in
  List.iteri
    (fun i value -> Bytes.set_int32_le vertices (i * 4) (Int32.bits_of_float value))
    [ -1.; -1.; 3.; -1.; -1.; 3. ];
  let sampled =
    get
      (Texture.create ~device
         (Texture.descriptor_2d ~storage:Buffer.Shared ~usage:[ Texture.Shader_read ]
            ~format:Texture.Rgba8_unorm ~width:1 ~height:1 ()))
  in
  let sampler =
    get (Sampler.create ~device { (Sampler.default ()) with support_argument_buffers = true })
  in
  let argument_buffer =
    get
      (Buffer.create ~device
         ~length:(Shader_argument_encoder.encoded_length argument_encoder)
         ~storage:Buffer.Shared ())
  in
  get (Shader_argument_encoder.set_argument_buffer argument_encoder argument_buffer ~offset:0L ());
  get
    (Shader_argument_encoder.set argument_encoder ~index:0L
       (Shader_argument_encoder.Texture sampled));
  get
    (Shader_argument_encoder.set argument_encoder ~index:1L
       (Shader_argument_encoder.Sampler sampler))
