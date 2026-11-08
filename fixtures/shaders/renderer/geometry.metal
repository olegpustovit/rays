#include <metal_stdlib>
using namespace metal;
struct geometry_vertex_out {
  float4 position [[position]];
  float4 ink [[flat]];
};
vertex geometry_vertex_out geometry_vertex(
    uint vid [[vertex_id]],
    device const uint* buffer0 [[buffer(0)]],
    device const float* buffer1 [[buffer(1)]]) {
  geometry_vertex_out o;
  uint sb=0u;
  uint r6=1u,r8=3u,r9=2u;
  float r34=as_type<float>(0x0u),r35=as_type<float>(0x3f800000u);
  uint r36=2u,r39=24u,r41=255u;
  float r44=as_type<float>(0x437f0000u),r45=0.0f;
  uint r46=16u;
  float r50=0.0f;
  uint r51=8u;
  float r55=0.0f,r58=0.0f,r60=0.0f,r62=0.0f,r64=0.0f,r66=0.0f;
  float r7=((long((buffer1[(6u)*1+0])))==long(r6)?1.0f:0.0f);
  uint r10=(r7!=0.0f?r8:r9);
  uint r11=(uint((long(vid)))*r10);
  float r13=as_type<float>(uint((long(buffer0[r11*1+0]))));
  float r16=as_type<float>(uint((long(buffer0[((r11+r6))*1+0]))));
  float r24=((((((buffer1[(0u)*1+0])*r13))+(((buffer1[r6*1+0])*r16))))+(buffer1[r9*1+0]));
  float r33=((((((buffer1[r8*1+0])*r13))+(((buffer1[(4u)*1+0])*r16))))+(buffer1[(5u)*1+0]));
  if(r7!=0.0f) {
    long r38=long(buffer0[((r11+r36))*1+0]);
    long r40=as_type<long>(ulong(r38)>>(long(r39)&63));
    sb|=(r39>63u)?4u:0u;
    r45=(r44==0.0f?0.0f:(float(((uint(r40)&r41))))/r44);
    long r47=as_type<long>(ulong(r38)>>(long(r46)&63));
    sb|=(r46>63u)?4u:0u;
    r50=(r44==0.0f?0.0f:(float(((uint(r47)&r41))))/r44);
    long r52=as_type<long>(ulong(r38)>>(long(r51)&63));
    sb|=(r51>63u)?4u:0u;
    r55=(r44==0.0f?0.0f:(float(((uint(r52)&r41))))/r44);
    r58=(r44==0.0f?0.0f:(float(((uint(r38)&r41))))/r44);
  }
  if(r7==0.0f) {
    r60=buffer1[(8u)*1+0];
    r62=buffer1[(9u)*1+0];
    r64=buffer1[(10u)*1+0];
    r66=buffer1[(11u)*1+0];
  }
  float r67=(r7!=0.0f?r45:r60);
  float r68=(r7!=0.0f?r50:r62);
  float r69=(r7!=0.0f?r55:r64);
  float r70=(r7!=0.0f?r58:r66);
  o.position.x=r24;
  o.position.y=r33;
  o.position.z=r34;
  o.position.w=r35;
  o.ink.x=r67;
  o.ink.y=r68;
  o.ink.z=r69;
  o.ink.w=r70;
  return o;
}
fragment float4 geometry_fragment(
    geometry_vertex_out in [[stage_in]]) {
  float4 colour;
  uint sb=0u;
  float r0=in.ink.x;
  float r1=in.ink.y;
  float r2=in.ink.z;
  float r3=in.ink.w;
  colour.x=r0;
  colour.y=r1;
  colour.z=r2;
  colour.w=r3;
  return colour;
}
