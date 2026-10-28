// Generated from marks.rays by Flow_gpu.Raster (lib/flow_gpu/test_raster.exe --write); do not edit.
#include <metal_stdlib>
using namespace metal;
long fmod64(long a,long b){if(b==0)return 0;long r=a%b;return (r!=0&&((r<0)!=(b<0)))?r+b:r;}
struct marks_vertex_out {
  float4 position [[position]];
  float4 color;
  float4 color2;
  float2 local;
  float4 box [[flat]];
  float4 params [[flat]];
  float4 extra [[flat]];
};
vertex marks_vertex_out marks_vertex(
    uint vid [[vertex_id]],
    device const float* buffer0 [[buffer(0)]],
    device const float* buffer1 [[buffer(1)]],
    device const uint* buffer2 [[buffer(2)]],
    device const uint* buffer3 [[buffer(3)]],
    device const float* buffer4 [[buffer(4)]],
    device const uint* buffer5 [[buffer(5)]],
    device const float* buffer6 [[buffer(6)]],
    device const float* buffer7 [[buffer(7)]],
    device const float* buffer8 [[buffer(8)]]) {
  marks_vertex_out o;
  uint sb=0u;
  uint r10=4u,r13=1u,r15=2u,r25=0u,r34=5u;
  long r43=0;
  float r45=as_type<float>(0x0u),r47=as_type<float>(0x3f800000u),r49=as_type<float>(0x437f0000u),r51=as_type<float>(0x3f000000u);
  uint r54=24u,r56=1u,r64=16u,r67=2u,r75=8u,r77=255u;
  long r79=0;
  float r82=0.0f;
  uint r83=1u;
  float r85=0.0f,r93=0.0f,r94=0.0f,r95=0.0f,r100=as_type<float>(0x0u),r120=as_type<float>(0x3f800000u),r125=0.0f,r126=0.0f,r135=0.0f,r145=0.0f;
  uint r146=24u,r148=255u;
  float r151=as_type<float>(0x437f0000u),r152=0.0f;
  uint r153=16u;
  float r157=0.0f;
  uint r158=8u;
  float r162=0.0f,r165=0.0f;
  uint r167=4u;
  float r168=0.0f,r170=0.0f,r171=0.0f,r172=0.0f,r173=0.0f,r175=0.0f;
  uint r176=1u;
  float r179=0.0f,r180=0.0f,r181=0.0f,r182=0.0f,r183=0.0f;
  uint r184=0u;
  float r185=0.0f,r187=0.0f,r188=as_type<float>(0x3f800000u),r198=0.0f,r201=0.0f,r210=0.0f;
  uint r211=3u;
  float r219=0.0f,r220=as_type<float>(0x0u),r225=0.0f;
  long r226=0;
  float r228=as_type<float>(0x0u),r230=as_type<float>(0x3f800000u),r232=as_type<float>(0x437f0000u),r234=as_type<float>(0x3f000000u);
  uint r237=24u,r239=1u,r247=16u,r250=2u,r258=8u,r260=255u;
  long r262=0;
  uint r264=24u,r266=255u;
  float r269=as_type<float>(0x437f0000u),r270=0.0f;
  uint r271=16u;
  float r275=0.0f;
  uint r276=8u;
  float r280=0.0f,r283=0.0f,r287=0.0f,r291=0.0f,r295=0.0f,r298=0.0f,r299=0.0f;
  long r0=long(vid);
  long r11=(as_type<long>(ulong((long(r10)==0?0:r0/long(r10)))<<1)>>1);
  sb|=(r10==0)?2u:0u;
  long r12=(as_type<long>(ulong((long(r10)==0?0:r0%long(r10)))<<1)>>1);
  sb|=(r10==0)?2u:0u;
  float r17=(((r12==long(r13)?1.0f:0.0f))!=0.0f||((r12==long(r15)?1.0f:0.0f))!=0.0f);
  float r20=(r17!=0.0f?(as_type<float>(0x3f800000u)):(as_type<float>(0x0u)));
  float r21=(r12>=long(r15)?1.0f:0.0f);
  float r24=(r21!=0.0f?(as_type<float>(0x3f800000u)):(as_type<float>(0x0u)));
  long r26=long(buffer5[r25*1+0]);
  long r27=long(buffer5[r13*1+0]);
  uint r28=(uint(r11)*uint(r27));
  long r29=long(buffer5[r15*1+0]);
  uint r30=(uint(r11)*uint(r29));
  float r31=buffer0[r28*1+0];
  float r33=buffer0[((r28+r13))*1+0];
  long r35=long(buffer5[r34*1+0]);
  float r37=buffer4[((uint(r11)*uint(r35)))*1+0];
  long r38=long(buffer5[r10*1+0]);
  uint r39=(uint(r11)*uint(r38));
  float r42=((long(buffer5[(7u)*1+0]))==long(r25)?1.0f:0.0f);
  if(r42!=0.0f) {
    r43=long(buffer3[r39*1+0]);
  }
  if(r42==0.0f) {
    long r55=(as_type<long>(ulong((ulong((long(floor((((((min((max((buffer8[r39*1+0]),r45)),r47))*r49))+r51))))))<<(long(r54)&63)))<<1)>>1);
    sb|=(r54>62u)?4u:0u;
    long r65=(as_type<long>(ulong((ulong((long(floor((((((min((max((buffer8[((r39+r56))*1+0]),r45)),r47))*r49))+r51))))))<<(long(r64)&63)))<<1)>>1);
    sb|=(r64>62u)?4u:0u;
    long r76=(as_type<long>(ulong((ulong((long(floor((((((min((max((buffer8[((r39+r67))*1+0]),r45)),r47))*r49))+r51))))))<<(long(r75)&63)))<<1)>>1);
    sb|=(r75>62u)?4u:0u;
    long r78=(as_type<long>(ulong((r76|long(r77)))<<1)>>1);
    r79=(as_type<long>(ulong((((as_type<long>(ulong((r55|r65))<<1)>>1))|r78))<<1)>>1);
  }
  long r80=(r42!=0.0f?r43:r79);
  float r81=(r26==long(r34)?1.0f:0.0f);
  if(r81!=0.0f) {
    r82=buffer1[r30*1+0];
    r85=buffer1[((r30+r83))*1+0];
    float r86=(r82-r31);
    float r87=(r85-r33);
    float r91=sqrt(fabs(((((r86*r86))+((r87*r87))))));
    r93=(r91>(as_type<float>(0x358637bdu)));
    if(r93!=0.0f) {
      r94=(r91==0.0f?0.0f:r86/r91);
      r95=(r91==0.0f?0.0f:r87/r91);
    }
    float r98=(r93!=0.0f?r94:(as_type<float>(0x3f800000u)));
    float r99=(r93!=0.0f?r95:(as_type<float>(0x0u)));
    float r105=(((r37*(as_type<float>(0x3f000000u))))+(as_type<float>(0x3fc00000u)));
    float r106=(r98*r105);
    float r107=(r99*r105);
    float r108=(r31-r106);
    float r109=(r33-r107);
    float r122=(((((r24*(as_type<float>(0x40000000u))))-r120))*r105);
    r125=(((r108+((((((r82+r106))-r108))*r20))))+((((r100-r99))*r122)));
    r126=(((r109+((((((r85+r107))-r109))*r20))))+((r98*r122)));
    r135=((((((buffer6[(0u)*1+0])*r125))+(((buffer6[r83*1+0])*r126))))+(buffer6[(2u)*1+0]));
    r145=((((((buffer6[(3u)*1+0])*r125))+(((buffer6[(4u)*1+0])*r126))))+(buffer6[(5u)*1+0]));
    long r147=(as_type<long>(ulong(long((ulong(r80)&0x7fffffffffffffffUL)>>(long(r146)&63)))<<1)>>1);
    sb|=(r146>62u)?4u:0u;
    r152=(r151==0.0f?0.0f:(float(((uint(r147)&r148))))/r151);
    long r154=(as_type<long>(ulong(long((ulong(r80)&0x7fffffffffffffffUL)>>(long(r153)&63)))<<1)>>1);
    sb|=(r153>62u)?4u:0u;
    r157=(r151==0.0f?0.0f:(float(((uint(r154)&r148))))/r151);
    long r159=(as_type<long>(ulong(long((ulong(r80)&0x7fffffffffffffffUL)>>(long(r158)&63)))<<1)>>1);
    sb|=(r158>62u)?4u:0u;
    r162=(r151==0.0f?0.0f:(float(((uint(r159)&r148))))/r151);
    r165=(r151==0.0f?0.0f:(float(((uint(r80)&r148))))/r151);
  }
  if(r81==0.0f) {
    r168=(r26==long(r167)?1.0f:0.0f);
    if(r168!=0.0f) {
      float r169=buffer1[r30*1+0];
      r170=(r31-r169);
      r171=(r33-r169);
      r172=(r31+r169);
      r173=(r33+r169);
    }
    if(r168==0.0f) {
      r175=(r31+(buffer1[r30*1+0]));
      r179=(r33+(buffer1[((r30+r176))*1+0]));
    }
    r180=(r168!=0.0f?r170:r31);
    r181=(r168!=0.0f?r171:r33);
    r182=(r168!=0.0f?r172:r175);
    r183=(r168!=0.0f?r173:r179);
    r185=(r80==long(r184)?1.0f:0.0f);
    r187=(r185!=0.0f?(as_type<float>(0x0u)):r37);
    float r191=(r188+((r187*(as_type<float>(0x3f000000u)))));
    float r192=(r180-r191);
    float r193=(r181-r191);
    r198=(r192+((((((r182+r191))-r192))*r20)));
    r201=(r193+((((((r183+r191))-r193))*r24)));
    r210=((((((buffer6[r184*1+0])*r198))+(((buffer6[(1u)*1+0])*r201))))+(buffer6[(2u)*1+0]));
    r219=((((((buffer6[r211*1+0])*r198))+(((buffer6[r167*1+0])*r201))))+(buffer6[(5u)*1+0]));
    long r221=long(buffer5[r211*1+0]);
    uint r222=(uint(r11)*uint(r221));
    r225=((long(buffer5[(6u)*1+0]))==long(r184)?1.0f:0.0f);
    if(r225!=0.0f) {
      r226=long(buffer2[r222*1+0]);
    }
    if(r225==0.0f) {
      long r238=(as_type<long>(ulong((ulong((long(floor((((((min((max((buffer7[r222*1+0]),r228)),r230))*r232))+r234))))))<<(long(r237)&63)))<<1)>>1);
      sb|=(r237>62u)?4u:0u;
      long r248=(as_type<long>(ulong((ulong((long(floor((((((min((max((buffer7[((r222+r239))*1+0]),r228)),r230))*r232))+r234))))))<<(long(r247)&63)))<<1)>>1);
      sb|=(r247>62u)?4u:0u;
      long r259=(as_type<long>(ulong((ulong((long(floor((((((min((max((buffer7[((r222+r250))*1+0]),r228)),r230))*r232))+r234))))))<<(long(r258)&63)))<<1)>>1);
      sb|=(r258>62u)?4u:0u;
      long r261=(as_type<long>(ulong((r259|long(r260)))<<1)>>1);
      r262=(as_type<long>(ulong((((as_type<long>(ulong((r238|r248))<<1)>>1))|r261))<<1)>>1);
    }
    long r263=(r225!=0.0f?r226:r262);
    long r265=(as_type<long>(ulong(long((ulong(r263)&0x7fffffffffffffffUL)>>(long(r264)&63)))<<1)>>1);
    sb|=(r264>62u)?4u:0u;
    r270=(r269==0.0f?0.0f:(float(((uint(r265)&r266))))/r269);
    long r272=(as_type<long>(ulong(long((ulong(r263)&0x7fffffffffffffffUL)>>(long(r271)&63)))<<1)>>1);
    sb|=(r271>62u)?4u:0u;
    r275=(r269==0.0f?0.0f:(float(((uint(r272)&r266))))/r269);
    long r277=(as_type<long>(ulong(long((ulong(r263)&0x7fffffffffffffffUL)>>(long(r276)&63)))<<1)>>1);
    sb|=(r276>62u)?4u:0u;
    r280=(r269==0.0f?0.0f:(float(((uint(r277)&r266))))/r269);
    r283=(r269==0.0f?0.0f:(float(((uint(r263)&r266))))/r269);
    long r284=(as_type<long>(ulong(long((ulong(r80)&0x7fffffffffffffffUL)>>(long(r264)&63)))<<1)>>1);
    sb|=(r264>62u)?4u:0u;
    r287=(r269==0.0f?0.0f:(float(((uint(r284)&r266))))/r269);
    long r288=(as_type<long>(ulong(long((ulong(r80)&0x7fffffffffffffffUL)>>(long(r271)&63)))<<1)>>1);
    sb|=(r271>62u)?4u:0u;
    r291=(r269==0.0f?0.0f:(float(((uint(r288)&r266))))/r269);
    long r292=(as_type<long>(ulong(long((ulong(r80)&0x7fffffffffffffffUL)>>(long(r276)&63)))<<1)>>1);
    sb|=(r276>62u)?4u:0u;
    r295=(r269==0.0f?0.0f:(float(((uint(r292)&r266))))/r269);
    r298=(r269==0.0f?0.0f:(float(((uint(r80)&r266))))/r269);
    r299=float(r26);
  }
  float r300=(r81!=0.0f?r135:r210);
  float r301=(r81!=0.0f?r145:r219);
  float r302=(r81!=0.0f?r100:r220);
  float r303=(r81!=0.0f?r120:r188);
  float r304=(r81!=0.0f?r152:r270);
  float r305=(r81!=0.0f?r157:r275);
  float r306=(r81!=0.0f?r162:r280);
  float r307=(r81!=0.0f?r165:r283);
  float r308=(r81!=0.0f?r100:r287);
  float r309=(r81!=0.0f?r100:r291);
  float r310=(r81!=0.0f?r100:r295);
  float r311=(r81!=0.0f?r100:r298);
  float r312=(r81!=0.0f?r125:r198);
  float r313=(r81!=0.0f?r126:r201);
  float r314=(r81!=0.0f?r100:r180);
  float r315=(r81!=0.0f?r100:r181);
  float r316=(r81!=0.0f?r100:r182);
  float r317=(r81!=0.0f?r100:r183);
  float r318=(r81!=0.0f?r37:r187);
  float r319=(r81!=0.0f?(as_type<float>(0x40a00000u)):r299);
  float r320=(r81!=0.0f?r31:r220);
  float r321=(r81!=0.0f?r33:r220);
  float r322=(r81!=0.0f?r82:r220);
  float r323=(r81!=0.0f?r85:r220);
  o.position.x=r300;
  o.position.y=r301;
  o.position.z=r302;
  o.position.w=r303;
  o.color.x=r304;
  o.color.y=r305;
  o.color.z=r306;
  o.color.w=r307;
  o.color2.x=r308;
  o.color2.y=r309;
  o.color2.z=r310;
  o.color2.w=r311;
  o.local.x=r312;
  o.local.y=r313;
  o.box.x=r314;
  o.box.y=r315;
  o.box.z=r316;
  o.box.w=r317;
  o.params.x=r302;
  o.params.y=r318;
  o.params.z=r303;
  o.params.w=r319;
  o.extra.x=r320;
  o.extra.y=r321;
  o.extra.z=r322;
  o.extra.w=r323;
  return o;
}
fragment float4 marks_fragment(
    marks_vertex_out in [[stage_in]]) {
  float4 colour;
  uint sb=0u;
  float r34=as_type<float>(0x3f000000u),r42=as_type<float>(0x0u),r74=0.0f,r80=0.0f,r83=as_type<float>(0x3f000000u),r91=0.0f,r98=0.0f,r101=as_type<float>(0x0u),r117=0.0f,r131=as_type<float>(0x0u),r136=0.0f,r137=as_type<float>(0x3f000000u);
  float r143=as_type<float>(0x0u),r145=as_type<float>(0x3f800000u),r166=0.0f,r167=as_type<float>(0x0u),r168=0.0f,r169=0.0f,r170=0.0f,r171=0.0f,r172=0.0f,r173=0.0f,r174=0.0f,r175=0.0f;
  float r23=((in.params.w)==(as_type<float>(0x40a00000u)));
  if(r23!=0.0f) {
    float r24=((in.extra.z)-(in.extra.x));
    float r25=((in.extra.w)-(in.extra.y));
    float r29=sqrt(fabs(((((r24*r24))+((r25*r25))))));
    float r30=(r29==0.0f?0.0f:r24/r29);
    float r31=(r29==0.0f?0.0f:r25/r29);
    float r37=((in.local.x)-(((((in.extra.x)+(in.extra.z)))*r34)));
    float r38=((in.local.y)-(((((in.extra.y)+(in.extra.w)))*r34)));
    float r51=((((fabs(((((r37*r30))+((r38*r31))))))-((r29*r34))))+r42);
    float r54=((((fabs(((((r37*((r42-r31))))+((r38*r30))))))-(((in.params.y)*r34))))+r42);
    float r55=max(r51,r42);
    float r56=max(r54,r42);
    float r65=fwidth((in.local.x));
    float r66=fwidth((in.local.y));
    r74=(r29<=(as_type<float>(0x358637bdu)));
    r80=((in.color.w)*(min((max(((r34-(((max((sqrt(fabs(((((r65*r65))+((r66*r66))))))),(as_type<float>(0x38d1b717u))))==0.0f?0.0f:(((((sqrt(fabs(((((r55*r55))+((r56*r56)))))))+(min((max(r51,r54)),r42))))-r42))/(max((sqrt(fabs(((((r65*r65))+((r66*r66))))))),(as_type<float>(0x38d1b717u)))))))),r42)),(as_type<float>(0x3f800000u)))));
  }
  if(r23==0.0f) {
    float r84=((((in.box.x)+(in.box.z)))*r83);
    float r85=((((in.box.y)+(in.box.w)))*r83);
    float r88=((((in.box.z)-(in.box.x)))*r83);
    r91=((in.params.w)==(as_type<float>(0x40800000u)));
    if(r91!=0.0f) {
      float r92=((in.local.x)-r84);
      float r93=((in.local.y)-r85);
      r98=((sqrt(fabs(((((r92*r92))+((r93*r93)))))))-r88);
    }
    if(r91==0.0f) {
      float r104=((((fabs((((in.local.x)-r84))))-r88))+r101);
      float r107=((((fabs((((in.local.y)-r85))))-(((((in.box.w)-(in.box.y)))*r83))))+r101);
      float r108=max(r104,r101);
      float r109=max(r107,r101);
      r117=((((sqrt(fabs(((((r108*r108))+((r109*r109)))))))+(min((max(r104,r107)),r101))))-r101);
    }
    float r118=(r91!=0.0f?r98:r117);
    float r119=fwidth((in.local.x));
    float r120=fwidth((in.local.y));
    float r128=max((((sqrt(fabs(((((r119*r119))+((r120*r120)))))))*(as_type<float>(0x3f3504f3u)))),(as_type<float>(0x38d1b717u)));
    float r135=((in.color.w)*(min((max(((r83-((r128==0.0f?0.0f:r118/r128)))),r131)),(as_type<float>(0x3f800000u)))));
    r136=((in.params.y)<=r131);
    if(r136==0.0f) {
      float r147=((in.color2.w)*(min((max(((r137-((r128==0.0f?0.0f:(((fabs(r118))-(((in.params.y)*r137))))/r128)))),r143)),r145)));
      float r148=(r145-r147);
      float r150=(r147+((r135*r148)));
      r166=(r150<=r143);
      r168=(r166!=0.0f?r167:((r150==0.0f?0.0f:(((((in.color2.x)*r147))+(((((in.color.x)*r135))*r148))))/r150)));
      r169=(r166!=0.0f?r167:((r150==0.0f?0.0f:(((((in.color2.y)*r147))+(((((in.color.y)*r135))*r148))))/r150)));
      r170=(r166!=0.0f?r167:((r150==0.0f?0.0f:(((((in.color2.z)*r147))+(((((in.color.z)*r135))*r148))))/r150)));
      r171=(r166!=0.0f?r167:r150);
    }
    r172=(r136!=0.0f?(in.color.x):r168);
    r173=(r136!=0.0f?(in.color.y):r169);
    r174=(r136!=0.0f?(in.color.z):r170);
    r175=(r136!=0.0f?r135:r171);
  }
  float r176=(r23!=0.0f?r74:r131);
  float r177=(r23!=0.0f?(in.color.x):r172);
  float r178=(r23!=0.0f?(in.color.y):r173);
  float r179=(r23!=0.0f?(in.color.z):r174);
  float r180=(r23!=0.0f?r80:r175);
  colour.x=r177;
  colour.y=r178;
  colour.z=r179;
  colour.w=r180;
  if(r176!=0.0f) discard_fragment();
  return colour;
}
