// Generated from scene.rays by Flow_gpu.Raster (lib/flow_gpu/test_raster.exe --write); do not edit.
#include <metal_stdlib>
using namespace metal;
struct sun_vertex_out {
  float4 position [[position]];
};
vertex sun_vertex_out sun_vertex(
    uint vid [[vertex_id]],
    uint iid [[instance_id]],
    device const float* buffer0 [[buffer(0)]],
    device const uint* buffer1 [[buffer(1)]],
    device const float* buffer2 [[buffer(2)]],
    device const float* buffer3 [[buffer(3)]],
    device const float* buffer4 [[buffer(4)]]) {
  sun_vertex_out o;
  uint sb=0u;
  uint r6=6u,r9=1u,r12=2u;
  float r15=as_type<float>(0x3f800000u);
  uint r17=48u,r23=16u,r25=3u,r28=2u,r31=1u;
  float r38=0.0f;
  uint r39=4u;
  float r51=0.0f;
  uint r52=8u;
  float r64=0.0f;
  uint r65=12u;
  float r77=0.0f,r89=0.0f,r101=0.0f,r113=0.0f,r125=0.0f;
  uint r7=(uint((long(vid)))*r6);
  float r8=buffer0[r7*1+0];
  float r11=buffer0[((r7+r9))*1+0];
  float r14=buffer0[((r7+r12))*1+0];
  float r22=((buffer3[(83u)*1+0])>(as_type<float>(0x3f000000u)));
  if(r22!=0.0f) {
    uint r24=(((uint((long(iid)))*r17))+r23);
    r38=fma(r15,(buffer4[((r24+r25))*1+0]),(fma(r14,(buffer4[((r24+r28))*1+0]),(fma(r11,(buffer4[((r24+r31))*1+0]),((r8*(buffer4[r24*1+0]))))))));
    uint r40=(r24+r39);
    r51=fma(r15,(buffer4[((r40+r25))*1+0]),(fma(r14,(buffer4[((r40+r28))*1+0]),(fma(r11,(buffer4[((r40+r31))*1+0]),((r8*(buffer4[r40*1+0]))))))));
    uint r53=(r24+r52);
    r64=fma(r15,(buffer4[((r53+r25))*1+0]),(fma(r14,(buffer4[((r53+r28))*1+0]),(fma(r11,(buffer4[((r53+r31))*1+0]),((r8*(buffer4[r53*1+0]))))))));
    uint r66=(r24+r65);
    r77=fma(r15,(buffer4[((r66+r25))*1+0]),(fma(r14,(buffer4[((r66+r28))*1+0]),(fma(r11,(buffer4[((r66+r31))*1+0]),((r8*(buffer4[r66*1+0]))))))));
  }
  if(r22==0.0f) {
    r89=fma(r15,(buffer3[(19u)*1+0]),(fma(r14,(buffer3[(18u)*1+0]),(fma(r11,(buffer3[(17u)*1+0]),((r8*(buffer3[(16u)*1+0]))))))));
    r101=fma(r15,(buffer3[(23u)*1+0]),(fma(r14,(buffer3[(22u)*1+0]),(fma(r11,(buffer3[(21u)*1+0]),((r8*(buffer3[(20u)*1+0]))))))));
    r113=fma(r15,(buffer3[(27u)*1+0]),(fma(r14,(buffer3[(26u)*1+0]),(fma(r11,(buffer3[(25u)*1+0]),((r8*(buffer3[(24u)*1+0]))))))));
    r125=fma(r15,(buffer3[(31u)*1+0]),(fma(r14,(buffer3[(30u)*1+0]),(fma(r11,(buffer3[(29u)*1+0]),((r8*(buffer3[(28u)*1+0]))))))));
  }
  float r126=(r22!=0.0f?r38:r89);
  float r127=(r22!=0.0f?r51:r101);
  float r128=(r22!=0.0f?r64:r113);
  float r129=(r22!=0.0f?r77:r125);
  float r141=fma(r129,(buffer2[(43u)*1+0]),(fma(r128,(buffer2[(42u)*1+0]),(fma(r127,(buffer2[(41u)*1+0]),((r126*(buffer2[(40u)*1+0]))))))));
  float r153=fma(r129,(buffer2[(47u)*1+0]),(fma(r128,(buffer2[(46u)*1+0]),(fma(r127,(buffer2[(45u)*1+0]),((r126*(buffer2[(44u)*1+0]))))))));
  float r164=fma(r129,(buffer2[(51u)*1+0]),(fma(r128,(buffer2[(50u)*1+0]),(fma(r127,(buffer2[(49u)*1+0]),((r126*(buffer2[r17*1+0]))))))));
  float r176=fma(r129,(buffer2[(55u)*1+0]),(fma(r128,(buffer2[(54u)*1+0]),(fma(r127,(buffer2[(53u)*1+0]),((r126*(buffer2[(52u)*1+0]))))))));
  o.position.x=r141;
  o.position.y=r153;
  o.position.z=r164;
  o.position.w=r176;
  return o;
}
fragment float4 sun_fragment(
    sun_vertex_out in [[stage_in]]) {
  float4 colour;
  uint sb=0u;
  float r3=as_type<float>(0x3f800000u);
  uint r9=16u,r11=255u;
  float r14=as_type<float>(0x437f0000u);
  uint r16=8u,r21=0u;
  long r8=long((fma((min((max((in.position.z),(as_type<float>(0x0u)))),r3)),(as_type<float>(0x4b7fffffu)),(as_type<float>(0x3f000000u)))));
  long r10=(as_type<long>(ulong(long((ulong(r8)&0x7fffffffffffffffUL)>>(long(r9)&63)))<<1)>>1);
  sb|=(r9>62u)?4u:0u;
  float r15=(r14==0.0f?0.0f:(float(((uint(r10)&r11))))/r14);
  long r17=(as_type<long>(ulong(long((ulong(r8)&0x7fffffffffffffffUL)>>(long(r16)&63)))<<1)>>1);
  sb|=(r16>62u)?4u:0u;
  float r20=(r14==0.0f?0.0f:(float(((uint(r17)&r11))))/r14);
  long r22=(as_type<long>(ulong(long((ulong(r8)&0x7fffffffffffffffUL)>>(long(r21)&63)))<<1)>>1);
  sb|=(r21>62u)?4u:0u;
  float r25=(r14==0.0f?0.0f:(float(((uint(r22)&r11))))/r14);
  colour.x=r15;
  colour.y=r20;
  colour.z=r25;
  colour.w=r3;
  return colour;
}
