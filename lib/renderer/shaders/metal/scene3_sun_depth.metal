// Generated from scene.rays by Flow_gpu.Raster (lib/flow_gpu/test_raster.exe --write); do not edit.
#include <metal_stdlib>
using namespace metal;
long fmod64(long a,long b){if(b==0)return 0;long r=a%b;return (r!=0&&((r<0)!=(b<0)))?r+b:r;}
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
  float r8=as_type<float>(0x3f000000u),r14=0.0f;
  uint r15=1u;
  float r17=0.0f;
  uint r18=2u;
  float r20=0.0f;
  uint r23=1u,r26=31u,r28=0u;
  float r29=0.0f;
  uint r33=20u,r35=2047u,r37=1048575u,r39=32u;
  float r43=as_type<float>(0x59800000u),r45=0.0f,r48=0.0f;
  uint r51=1023u;
  float r55=0.0f,r56=0.0f;
  uint r57=2u;
  float r63=0.0f,r74=0.0f,r77=0.0f;
  uint r80=1023u;
  float r84=0.0f,r85=0.0f;
  uint r86=4u;
  float r92=0.0f,r103=0.0f,r106=0.0f;
  uint r109=1023u;
  float r113=0.0f,r114=0.0f;
  uint r121=48u,r126=16u,r128=3u,r131=2u,r134=1u;
  float r141=0.0f;
  uint r142=4u;
  float r154=0.0f;
  uint r155=8u;
  float r167=0.0f;
  uint r168=12u;
  float r180=0.0f,r192=0.0f,r204=0.0f,r216=0.0f,r228=0.0f;
  float r9=((buffer3[(82u)*1+0])>r8);
  uint r12=(r9!=0.0f?(6u):(17u));
  uint r13=(uint((long(vid)))*r12);
  if(r9!=0.0f) {
    r14=buffer0[r13*1+0];
    r17=buffer0[((r13+r15))*1+0];
    r20=buffer0[((r13+r18))*1+0];
  }
  if(r9==0.0f) {
    long r22=long(buffer1[r13*1+0]);
    long r25=long(buffer1[((r13+r23))*1+0]);
    long r27=(as_type<long>(ulong(long((ulong(r25)&0x7fffffffffffffffUL)>>(long(r26)&63)))<<1)>>1);
    sb|=(r26>62u)?4u:0u;
    r29=(r27==long(r28)?1.0f:0.0f);
    float r32=(r29!=0.0f?(as_type<float>(0x3f800000u)):(as_type<float>(0xbf800000u)));
    long r34=(as_type<long>(ulong(long((ulong(r25)&0x7fffffffffffffffUL)>>(long(r33)&63)))<<1)>>1);
    sb|=(r33>62u)?4u:0u;
    uint r36=(uint(r34)&r35);
    long r40=(as_type<long>(ulong((ulong(long(((uint(r25)&r37))))<<(long(r39)&63)))<<1)>>1);
    sb|=(r39>62u)?4u:0u;
    float r44=(r43==0.0f?0.0f:(float(((as_type<long>(ulong((r40|r22))<<1)>>1))))/r43);
    r45=(long(r36)==long(r28)?1.0f:0.0f);
    if(r45!=0.0f) {
      r48=(r32*(ldexp(r44,int((as_type<float>(0xc47f8000u))))));
    }
    if(r45==0.0f) {
      r55=(r32*(ldexp((((as_type<float>(0x3f800000u))+r44)),int((float(((as_type<long>(ulong((ulong(long(r36))-ulong(long(r51))))<<1)>>1))))))));
    }
    r56=(r45!=0.0f?r48:r55);
    uint r58=(r13+r57);
    long r59=long(buffer1[r58*1+0]);
    long r61=long(buffer1[((r58+r23))*1+0]);
    long r62=(as_type<long>(ulong(long((ulong(r61)&0x7fffffffffffffffUL)>>(long(r26)&63)))<<1)>>1);
    sb|=(r26>62u)?4u:0u;
    r63=(r62==long(r28)?1.0f:0.0f);
    float r66=(r63!=0.0f?(as_type<float>(0x3f800000u)):(as_type<float>(0xbf800000u)));
    long r67=(as_type<long>(ulong(long((ulong(r61)&0x7fffffffffffffffUL)>>(long(r33)&63)))<<1)>>1);
    sb|=(r33>62u)?4u:0u;
    uint r68=(uint(r67)&r35);
    long r70=(as_type<long>(ulong((ulong(long(((uint(r61)&r37))))<<(long(r39)&63)))<<1)>>1);
    sb|=(r39>62u)?4u:0u;
    float r73=(r43==0.0f?0.0f:(float(((as_type<long>(ulong((r70|r59))<<1)>>1))))/r43);
    r74=(long(r68)==long(r28)?1.0f:0.0f);
    if(r74!=0.0f) {
      r77=(r66*(ldexp(r73,int((as_type<float>(0xc47f8000u))))));
    }
    if(r74==0.0f) {
      r84=(r66*(ldexp((((as_type<float>(0x3f800000u))+r73)),int((float(((as_type<long>(ulong((ulong(long(r68))-ulong(long(r80))))<<1)>>1))))))));
    }
    r85=(r74!=0.0f?r77:r84);
    uint r87=(r13+r86);
    long r88=long(buffer1[r87*1+0]);
    long r90=long(buffer1[((r87+r23))*1+0]);
    long r91=(as_type<long>(ulong(long((ulong(r90)&0x7fffffffffffffffUL)>>(long(r26)&63)))<<1)>>1);
    sb|=(r26>62u)?4u:0u;
    r92=(r91==long(r28)?1.0f:0.0f);
    float r95=(r92!=0.0f?(as_type<float>(0x3f800000u)):(as_type<float>(0xbf800000u)));
    long r96=(as_type<long>(ulong(long((ulong(r90)&0x7fffffffffffffffUL)>>(long(r33)&63)))<<1)>>1);
    sb|=(r33>62u)?4u:0u;
    uint r97=(uint(r96)&r35);
    long r99=(as_type<long>(ulong((ulong(long(((uint(r90)&r37))))<<(long(r39)&63)))<<1)>>1);
    sb|=(r39>62u)?4u:0u;
    float r102=(r43==0.0f?0.0f:(float(((as_type<long>(ulong((r99|r88))<<1)>>1))))/r43);
    r103=(long(r97)==long(r28)?1.0f:0.0f);
    if(r103!=0.0f) {
      r106=(r95*(ldexp(r102,int((as_type<float>(0xc47f8000u))))));
    }
    if(r103==0.0f) {
      r113=(r95*(ldexp((((as_type<float>(0x3f800000u))+r102)),int((float(((as_type<long>(ulong((ulong(long(r97))-ulong(long(r109))))<<1)>>1))))))));
    }
    r114=(r103!=0.0f?r106:r113);
  }
  float r116=(r9!=0.0f?r14:r56);
  float r117=(r9!=0.0f?r17:r85);
  float r118=(r9!=0.0f?r20:r114);
  float r119=(r9!=0.0f?(as_type<float>(0x3f800000u)):(as_type<float>(0x3f800000u)));
  float r125=((buffer3[(83u)*1+0])>r8);
  if(r125!=0.0f) {
    uint r127=(((uint((long(iid)))*r121))+r126);
    r141=fma(r119,(buffer4[((r127+r128))*1+0]),(fma(r118,(buffer4[((r127+r131))*1+0]),(fma(r117,(buffer4[((r127+r134))*1+0]),((r116*(buffer4[r127*1+0]))))))));
    uint r143=(r127+r142);
    r154=fma(r119,(buffer4[((r143+r128))*1+0]),(fma(r118,(buffer4[((r143+r131))*1+0]),(fma(r117,(buffer4[((r143+r134))*1+0]),((r116*(buffer4[r143*1+0]))))))));
    uint r156=(r127+r155);
    r167=fma(r119,(buffer4[((r156+r128))*1+0]),(fma(r118,(buffer4[((r156+r131))*1+0]),(fma(r117,(buffer4[((r156+r134))*1+0]),((r116*(buffer4[r156*1+0]))))))));
    uint r169=(r127+r168);
    r180=fma(r119,(buffer4[((r169+r128))*1+0]),(fma(r118,(buffer4[((r169+r131))*1+0]),(fma(r117,(buffer4[((r169+r134))*1+0]),((r116*(buffer4[r169*1+0]))))))));
  }
  if(r125==0.0f) {
    r192=fma(r119,(buffer3[(19u)*1+0]),(fma(r118,(buffer3[(18u)*1+0]),(fma(r117,(buffer3[(17u)*1+0]),((r116*(buffer3[(16u)*1+0]))))))));
    r204=fma(r119,(buffer3[(23u)*1+0]),(fma(r118,(buffer3[(22u)*1+0]),(fma(r117,(buffer3[(21u)*1+0]),((r116*(buffer3[(20u)*1+0]))))))));
    r216=fma(r119,(buffer3[(27u)*1+0]),(fma(r118,(buffer3[(26u)*1+0]),(fma(r117,(buffer3[(25u)*1+0]),((r116*(buffer3[(24u)*1+0]))))))));
    r228=fma(r119,(buffer3[(31u)*1+0]),(fma(r118,(buffer3[(30u)*1+0]),(fma(r117,(buffer3[(29u)*1+0]),((r116*(buffer3[(28u)*1+0]))))))));
  }
  float r229=(r125!=0.0f?r141:r192);
  float r230=(r125!=0.0f?r154:r204);
  float r231=(r125!=0.0f?r167:r216);
  float r232=(r125!=0.0f?r180:r228);
  float r244=fma(r232,(buffer2[(43u)*1+0]),(fma(r231,(buffer2[(42u)*1+0]),(fma(r230,(buffer2[(41u)*1+0]),((r229*(buffer2[(40u)*1+0]))))))));
  float r256=fma(r232,(buffer2[(47u)*1+0]),(fma(r231,(buffer2[(46u)*1+0]),(fma(r230,(buffer2[(45u)*1+0]),((r229*(buffer2[(44u)*1+0]))))))));
  float r267=fma(r232,(buffer2[(51u)*1+0]),(fma(r231,(buffer2[(50u)*1+0]),(fma(r230,(buffer2[(49u)*1+0]),((r229*(buffer2[r121*1+0]))))))));
  float r279=fma(r232,(buffer2[(55u)*1+0]),(fma(r231,(buffer2[(54u)*1+0]),(fma(r230,(buffer2[(53u)*1+0]),((r229*(buffer2[(52u)*1+0]))))))));
  o.position.x=r244;
  o.position.y=r256;
  o.position.z=r267;
  o.position.w=r279;
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
