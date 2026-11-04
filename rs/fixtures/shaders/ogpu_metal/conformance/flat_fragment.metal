#include <metal_stdlib>
using namespace metal;
struct V { float4 position [[position]]; float2 uv; };
vertex V flat_vertex(uint i [[vertex_id]], const device float *offsets [[buffer(0)]]) { float2 p[3]={{-1.,-1.},{3.,-1.},{-1.,3.}}; V v; v.position=float4(p[i]+float2(offsets[0],offsets[1]),0.,1.); v.uv=(p[i]+1.)*.5; return v; }
fragment float4 flat_fragment(V v [[stage_in]], constant float4 &color [[buffer(1)]]) { return color; }
struct Args { texture2d<float, access::sample> image [[id(0)]]; sampler sampling [[id(1)]]; };
fragment float4 argument_fragment(V v [[stage_in]], constant Args &args [[buffer(1)]]) { return args.image.sample(args.sampling, v.uv); }
fragment float4 textured_fragment(V v [[stage_in]], texture2d<float> image [[texture(1)]], sampler sampling [[sampler(2)]]) { return image.sample(sampling, v.uv); }
