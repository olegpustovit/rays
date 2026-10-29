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
  float r8=as_type<float>(0x3f000000u);
  uint r15=1u,r18=2u;
  float r21=as_type<float>(0x3f800000u);
  uint r23=48u,r28=16u,r30=3u,r33=2u,r36=1u;
  float r43=0.0f;
  uint r44=4u;
  float r56=0.0f;
  uint r57=8u;
  float r69=0.0f;
  uint r70=12u;
  float r82=0.0f,r94=0.0f,r106=0.0f,r118=0.0f,r130=0.0f;
  uint r12=((((buffer3[(79u)*1+0])>r8))!=0.0f?(3u):(6u));
  uint r13=(uint((long(vid)))*r12);
  float r14=buffer0[r13*1+0];
  float r17=buffer0[((r13+r15))*1+0];
  float r20=buffer0[((r13+r18))*1+0];
  float r27=((buffer3[(83u)*1+0])>r8);
  if(r27!=0.0f) {
    uint r29=(((uint((long(iid)))*r23))+r28);
    r43=fma(r21,(buffer4[((r29+r30))*1+0]),(fma(r20,(buffer4[((r29+r33))*1+0]),(fma(r17,(buffer4[((r29+r36))*1+0]),((r14*(buffer4[r29*1+0]))))))));
    uint r45=(r29+r44);
    r56=fma(r21,(buffer4[((r45+r30))*1+0]),(fma(r20,(buffer4[((r45+r33))*1+0]),(fma(r17,(buffer4[((r45+r36))*1+0]),((r14*(buffer4[r45*1+0]))))))));
    uint r58=(r29+r57);
    r69=fma(r21,(buffer4[((r58+r30))*1+0]),(fma(r20,(buffer4[((r58+r33))*1+0]),(fma(r17,(buffer4[((r58+r36))*1+0]),((r14*(buffer4[r58*1+0]))))))));
    uint r71=(r29+r70);
    r82=fma(r21,(buffer4[((r71+r30))*1+0]),(fma(r20,(buffer4[((r71+r33))*1+0]),(fma(r17,(buffer4[((r71+r36))*1+0]),((r14*(buffer4[r71*1+0]))))))));
  }
  if(r27==0.0f) {
    r94=fma(r21,(buffer3[(19u)*1+0]),(fma(r20,(buffer3[(18u)*1+0]),(fma(r17,(buffer3[(17u)*1+0]),((r14*(buffer3[(16u)*1+0]))))))));
    r106=fma(r21,(buffer3[(23u)*1+0]),(fma(r20,(buffer3[(22u)*1+0]),(fma(r17,(buffer3[(21u)*1+0]),((r14*(buffer3[(20u)*1+0]))))))));
    r118=fma(r21,(buffer3[(27u)*1+0]),(fma(r20,(buffer3[(26u)*1+0]),(fma(r17,(buffer3[(25u)*1+0]),((r14*(buffer3[(24u)*1+0]))))))));
    r130=fma(r21,(buffer3[(31u)*1+0]),(fma(r20,(buffer3[(30u)*1+0]),(fma(r17,(buffer3[(29u)*1+0]),((r14*(buffer3[(28u)*1+0]))))))));
  }
  float r131=(r27!=0.0f?r43:r94);
  float r132=(r27!=0.0f?r56:r106);
  float r133=(r27!=0.0f?r69:r118);
  float r134=(r27!=0.0f?r82:r130);
  float r146=fma(r134,(buffer2[(43u)*1+0]),(fma(r133,(buffer2[(42u)*1+0]),(fma(r132,(buffer2[(41u)*1+0]),((r131*(buffer2[(40u)*1+0]))))))));
  float r158=fma(r134,(buffer2[(47u)*1+0]),(fma(r133,(buffer2[(46u)*1+0]),(fma(r132,(buffer2[(45u)*1+0]),((r131*(buffer2[(44u)*1+0]))))))));
  float r169=fma(r134,(buffer2[(51u)*1+0]),(fma(r133,(buffer2[(50u)*1+0]),(fma(r132,(buffer2[(49u)*1+0]),((r131*(buffer2[r23*1+0]))))))));
  float r181=fma(r134,(buffer2[(55u)*1+0]),(fma(r133,(buffer2[(54u)*1+0]),(fma(r132,(buffer2[(53u)*1+0]),((r131*(buffer2[(52u)*1+0]))))))));
  o.position.x=r146;
  o.position.y=r158;
  o.position.z=r169;
  o.position.w=r181;
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
