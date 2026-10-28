// Generated from present.rays by Flow_gpu.Raster (lib/flow_gpu/test_raster.exe --write); do not edit.
#include <metal_stdlib>
using namespace metal;
long fmod64(long a,long b){if(b==0)return 0;long r=a%b;return (r!=0&&((r<0)!=(b<0)))?r+b:r;}
struct present_vertex_out {
  float4 position [[position]];
};
vertex present_vertex_out present_vertex(
    uint vid [[vertex_id]]) {
  present_vertex_out o;
  uint sb=0u;
  float r11=as_type<float>(0x0u),r12=as_type<float>(0x3f800000u);
  long r0=long(vid);
  float r2=(r0==long((1u))?1.0f:0.0f);
  float r5=(r2!=0.0f?(as_type<float>(0x40400000u)):(as_type<float>(0xbf800000u)));
  float r7=(r0==long((2u))?1.0f:0.0f);
  float r10=(r7!=0.0f?(as_type<float>(0x40400000u)):(as_type<float>(0xbf800000u)));
  o.position.x=r5;
  o.position.y=r10;
  o.position.z=r11;
  o.position.w=r12;
  return o;
}
fragment float4 present_fragment(
    present_vertex_out in [[stage_in]],
    texture2d<float> texture0 [[texture(0)]]) {
  float4 colour;
  uint sb=0u;
  float r0=in.position.x;
  float r1=in.position.y;
  float r2=texture0.read(uint2(r0,r1)).x;
  float r3=texture0.read(uint2(r0,r1)).y;
  float r4=texture0.read(uint2(r0,r1)).z;
  float r5=texture0.read(uint2(r0,r1)).w;
  colour.x=r2;
  colour.y=r3;
  colour.z=r4;
  colour.w=r5;
  return colour;
}
