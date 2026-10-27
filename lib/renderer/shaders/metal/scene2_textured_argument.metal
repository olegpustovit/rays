#include <metal_stdlib>
using namespace metal;
struct Out { float4 position [[position]]; float4 color; float2 uv; };
inline float scene_double(const device uchar *p){uint lo=*reinterpret_cast<const device uint*>(p);uint hi=*reinterpret_cast<const device uint*>(p+4);ulong bits=(ulong(hi)<<32)|ulong(lo);float sign=(hi>>31)==0?1.:-1.;int exponent=int((bits>>52)&0x7fful);ulong fraction=bits&0xffffffffffffful;if(exponent==0)return sign*ldexp(float(fraction)/4503599627370496.,-1022);return sign*ldexp(1.+float(fraction)/4503599627370496.,exponent-1023);}
vertex Out scene_vertex(uint i [[vertex_id]],const device uchar *input [[buffer(0)]],const device float *affine [[buffer(6)]]){const device uchar*p=input+i*68;Out v;float x=scene_double(p),y=scene_double(p+8);v.position=float4(affine[0]*x+affine[1]*y+affine[2],affine[3]*x+affine[4]*y+affine[5],0.,1.);v.color=unpack_unorm4x8_to_float(*reinterpret_cast<const device uint*>(p+48)).abgr;v.uv=float2(scene_double(p+52),scene_double(p+60));return v;}
struct Scene2_arguments { texture2d<float, access::sample> image [[id(0)]]; sampler sampling [[id(1)]]; };
fragment float4 scene_fragment_argument(Out value [[stage_in]],constant Scene2_arguments&args [[buffer(1)]]){return value.color*args.image.sample(args.sampling,value.uv);}
