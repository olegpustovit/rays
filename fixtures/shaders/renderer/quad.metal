#include <metal_stdlib>
using namespace metal;
struct raster_texture { texture2d<float, access::sample> image [[id(0)]]; sampler sampling [[id(1)]]; };
struct quad_vertex_out {
  float4 position [[position]];
  float2 uv;
  float4 ink [[flat]];
};
vertex quad_vertex_out quad_vertex(
    uint vid [[vertex_id]],
    device const float* buffer0 [[buffer(0)]]) {
  quad_vertex_out o;
  uint sb=0u;
  uint r2=4u,r5=20u,r7=1u,r9=2u,r19=12u,r22=14u,r27=13u,r30=15u,r35=0u,r46=3u,r54=5u;
  float r58=as_type<float>(0x0u),r59=as_type<float>(0x3f800000u);
  uint r60=16u,r63=18u,r70=17u,r73=19u,r80=8u,r83=9u,r86=10u,r89=11u;
  long r0=long(vid);
  long r3=(long(r2)==0||long(r2)==-1?0:r0%long(r2));
  sb|=(r2==0)?2u:0u;
  long r4=(long(r2)==0?0:long(r2)==-1?as_type<long>(0ul-ulong(r0)):r0/long(r2));
  sb|=(r2==0)?2u:0u;
  uint r6=(uint(r4)*r5);
  float r11=(((r3==long(r7)?1.0f:0.0f))!=0.0f||((r3==long(r9)?1.0f:0.0f))!=0.0f);
  float r14=(r11!=0.0f?(as_type<float>(0x3f800000u)):(as_type<float>(0x0u)));
  float r15=(r3>=long(r9)?1.0f:0.0f);
  float r18=(r15!=0.0f?(as_type<float>(0x3f800000u)):(as_type<float>(0x0u)));
  float r26=((buffer0[((r6+r19))*1+0])+(((buffer0[((r6+r22))*1+0])*r14)));
  float r34=((buffer0[((r6+r27))*1+0])+(((buffer0[((r6+r30))*1+0])*r18)));
  float r45=((((((buffer0[((r6+r35))*1+0])*r26))+(((buffer0[((r6+r7))*1+0])*r34))))+(buffer0[((r6+r9))*1+0]));
  float r57=((((((buffer0[((r6+r46))*1+0])*r26))+(((buffer0[((r6+r2))*1+0])*r34))))+(buffer0[((r6+r54))*1+0]));
  float r69=((((buffer0[((r6+r60))*1+0])*((r59-r14))))+(((buffer0[((r6+r63))*1+0])*r14)));
  float r79=((((buffer0[((r6+r70))*1+0])*((r59-r18))))+(((buffer0[((r6+r73))*1+0])*r18)));
  float r82=buffer0[((r6+r80))*1+0];
  float r85=buffer0[((r6+r83))*1+0];
  float r88=buffer0[((r6+r86))*1+0];
  float r91=buffer0[((r6+r89))*1+0];
  o.position.x=r45;
  o.position.y=r57;
  o.position.z=r58;
  o.position.w=r59;
  o.uv.x=r69;
  o.uv.y=r79;
  o.ink.x=r82;
  o.ink.y=r85;
  o.ink.z=r88;
  o.ink.w=r91;
  return o;
}
fragment float4 quad_fragment(
    quad_vertex_out in [[stage_in]],
    constant raster_texture& arguments7 [[buffer(7)]]) {
  float4 colour;
  texture2d<float> texture7=arguments7.image;
  sampler sampler8=arguments7.sampling;
  uint sb=0u;
  float r10=((in.ink.x)*(texture7.sample(sampler8,float2((in.uv.x),(in.uv.y))).x));
  float r11=((in.ink.y)*(texture7.sample(sampler8,float2((in.uv.x),(in.uv.y))).y));
  float r12=((in.ink.z)*(texture7.sample(sampler8,float2((in.uv.x),(in.uv.y))).z));
  float r13=((in.ink.w)*(texture7.sample(sampler8,float2((in.uv.x),(in.uv.y))).w));
  colour.x=r10;
  colour.y=r11;
  colour.z=r12;
  colour.w=r13;
  return colour;
}
