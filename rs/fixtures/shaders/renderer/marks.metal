#include <metal_stdlib>
using namespace metal;
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
    device const float* buffer6 [[buffer(6)]]) {
  marks_vertex_out o;
  uint sb=0u;
  uint r8=4u,r11=1u,r13=2u,r23=0u,r32=5u;
  long r41=0;
  float r44=as_type<float>(0x0u),r46=as_type<float>(0x3f800000u),r48=as_type<float>(0x437f0000u),r50=as_type<float>(0x3f000000u);
  uint r53=24u,r55=1u,r64=16u,r67=2u,r76=8u,r78=255u;
  long r80=0;
  float r83=0.0f;
  uint r84=1u;
  float r86=0.0f,r94=0.0f,r95=0.0f,r96=0.0f,r101=as_type<float>(0x0u),r121=as_type<float>(0x3f800000u),r126=0.0f,r127=0.0f,r136=0.0f,r146=0.0f;
  uint r147=24u,r149=255u;
  float r152=as_type<float>(0x437f0000u),r153=0.0f;
  uint r154=16u;
  float r158=0.0f;
  uint r159=8u;
  float r163=0.0f,r166=0.0f;
  uint r168=4u;
  float r169=0.0f,r171=0.0f,r172=0.0f,r173=0.0f,r174=0.0f,r176=0.0f;
  uint r177=1u;
  float r180=0.0f,r181=0.0f,r182=0.0f,r183=0.0f,r184=0.0f;
  uint r185=0u;
  float r186=0.0f,r188=0.0f,r189=as_type<float>(0x3f800000u),r199=0.0f,r202=0.0f,r211=0.0f;
  uint r212=3u;
  float r220=0.0f,r221=as_type<float>(0x0u),r226=0.0f;
  long r227=0;
  float r230=as_type<float>(0x0u),r232=as_type<float>(0x3f800000u),r234=as_type<float>(0x437f0000u),r236=as_type<float>(0x3f000000u);
  uint r239=24u,r241=1u,r250=16u,r253=2u,r262=8u,r264=255u;
  long r266=0;
  uint r268=24u,r270=255u;
  float r273=as_type<float>(0x437f0000u),r274=0.0f;
  uint r275=16u;
  float r279=0.0f;
  uint r280=8u;
  float r284=0.0f,r287=0.0f,r291=0.0f,r295=0.0f,r299=0.0f,r302=0.0f,r303=0.0f;
  long r0=long(vid);
  long r9=(as_type<long>(ulong((long(r8)==0?0:r0/long(r8)))<<1)>>1);
  sb|=(r8==0)?2u:0u;
  long r10=(as_type<long>(ulong((long(r8)==0?0:r0%long(r8)))<<1)>>1);
  sb|=(r8==0)?2u:0u;
  float r15=(((r10==long(r11)?1.0f:0.0f))!=0.0f||((r10==long(r13)?1.0f:0.0f))!=0.0f);
  float r18=(r15!=0.0f?(as_type<float>(0x3f800000u)):(as_type<float>(0x0u)));
  float r19=(r10>=long(r13)?1.0f:0.0f);
  float r22=(r19!=0.0f?(as_type<float>(0x3f800000u)):(as_type<float>(0x0u)));
  long r24=long(buffer5[r23*1+0]);
  long r25=long(buffer5[r11*1+0]);
  uint r26=(uint(r9)*uint(r25));
  long r27=long(buffer5[r13*1+0]);
  uint r28=(uint(r9)*uint(r27));
  float r29=buffer0[r26*1+0];
  float r31=buffer0[((r26+r11))*1+0];
  long r33=long(buffer5[r32*1+0]);
  float r35=buffer4[((uint(r9)*uint(r33)))*1+0];
  long r36=long(buffer5[r8*1+0]);
  uint r37=(uint(r9)*uint(r36));
  float r40=((long(buffer5[(7u)*1+0]))==long(r23)?1.0f:0.0f);
  if(r40!=0.0f) {
    r41=long(buffer3[r37*1+0]);
  }
  if(r40==0.0f) {
    long r54=(as_type<long>(ulong((ulong((long(floor((((((min((max((as_type<float>(uint((long(buffer3[r37*1+0]))))),r44)),r46))*r48))+r50))))))<<(long(r53)&63)))<<1)>>1);
    sb|=(r53>62u)?4u:0u;
    long r65=(as_type<long>(ulong((ulong((long(floor((((((min((max((as_type<float>(uint((long(buffer3[((r37+r55))*1+0]))))),r44)),r46))*r48))+r50))))))<<(long(r64)&63)))<<1)>>1);
    sb|=(r64>62u)?4u:0u;
    long r77=(as_type<long>(ulong((ulong((long(floor((((((min((max((as_type<float>(uint((long(buffer3[((r37+r67))*1+0]))))),r44)),r46))*r48))+r50))))))<<(long(r76)&63)))<<1)>>1);
    sb|=(r76>62u)?4u:0u;
    long r79=(as_type<long>(ulong((r77|long(r78)))<<1)>>1);
    r80=(as_type<long>(ulong((((as_type<long>(ulong((r54|r65))<<1)>>1))|r79))<<1)>>1);
  }
  long r81=(r40!=0.0f?r41:r80);
  float r82=(r24==long(r32)?1.0f:0.0f);
  if(r82!=0.0f) {
    r83=buffer1[r28*1+0];
    r86=buffer1[((r28+r84))*1+0];
    float r87=(r83-r29);
    float r88=(r86-r31);
    float r92=sqrt(fabs(((((r87*r87))+((r88*r88))))));
    r94=(r92>(as_type<float>(0x358637bdu)));
    if(r94!=0.0f) {
      r95=(r92==0.0f?0.0f:r87/r92);
      r96=(r92==0.0f?0.0f:r88/r92);
    }
    float r99=(r94!=0.0f?r95:(as_type<float>(0x3f800000u)));
    float r100=(r94!=0.0f?r96:(as_type<float>(0x0u)));
    float r106=(((r35*(as_type<float>(0x3f000000u))))+(as_type<float>(0x3fc00000u)));
    float r107=(r99*r106);
    float r108=(r100*r106);
    float r109=(r29-r107);
    float r110=(r31-r108);
    float r123=(((((r22*(as_type<float>(0x40000000u))))-r121))*r106);
    r126=(((r109+((((((r83+r107))-r109))*r18))))+((((r101-r100))*r123)));
    r127=(((r110+((((((r86+r108))-r110))*r18))))+((r99*r123)));
    r136=((((((buffer6[(0u)*1+0])*r126))+(((buffer6[r84*1+0])*r127))))+(buffer6[(2u)*1+0]));
    r146=((((((buffer6[(3u)*1+0])*r126))+(((buffer6[(4u)*1+0])*r127))))+(buffer6[(5u)*1+0]));
    long r148=(as_type<long>(ulong(long((ulong(r81)&0x7fffffffffffffffUL)>>(long(r147)&63)))<<1)>>1);
    sb|=(r147>62u)?4u:0u;
    r153=(r152==0.0f?0.0f:(float(((uint(r148)&r149))))/r152);
    long r155=(as_type<long>(ulong(long((ulong(r81)&0x7fffffffffffffffUL)>>(long(r154)&63)))<<1)>>1);
    sb|=(r154>62u)?4u:0u;
    r158=(r152==0.0f?0.0f:(float(((uint(r155)&r149))))/r152);
    long r160=(as_type<long>(ulong(long((ulong(r81)&0x7fffffffffffffffUL)>>(long(r159)&63)))<<1)>>1);
    sb|=(r159>62u)?4u:0u;
    r163=(r152==0.0f?0.0f:(float(((uint(r160)&r149))))/r152);
    r166=(r152==0.0f?0.0f:(float(((uint(r81)&r149))))/r152);
  }
  if(r82==0.0f) {
    r169=(r24==long(r168)?1.0f:0.0f);
    if(r169!=0.0f) {
      float r170=buffer1[r28*1+0];
      r171=(r29-r170);
      r172=(r31-r170);
      r173=(r29+r170);
      r174=(r31+r170);
    }
    if(r169==0.0f) {
      r176=(r29+(buffer1[r28*1+0]));
      r180=(r31+(buffer1[((r28+r177))*1+0]));
    }
    r181=(r169!=0.0f?r171:r29);
    r182=(r169!=0.0f?r172:r31);
    r183=(r169!=0.0f?r173:r176);
    r184=(r169!=0.0f?r174:r180);
    r186=(r81==long(r185)?1.0f:0.0f);
    r188=(r186!=0.0f?(as_type<float>(0x0u)):r35);
    float r192=(r189+((r188*(as_type<float>(0x3f000000u)))));
    float r193=(r181-r192);
    float r194=(r182-r192);
    r199=(r193+((((((r183+r192))-r193))*r18)));
    r202=(r194+((((((r184+r192))-r194))*r22)));
    r211=((((((buffer6[r185*1+0])*r199))+(((buffer6[(1u)*1+0])*r202))))+(buffer6[(2u)*1+0]));
    r220=((((((buffer6[r212*1+0])*r199))+(((buffer6[r168*1+0])*r202))))+(buffer6[(5u)*1+0]));
    long r222=long(buffer5[r212*1+0]);
    uint r223=(uint(r9)*uint(r222));
    r226=((long(buffer5[(6u)*1+0]))==long(r185)?1.0f:0.0f);
    if(r226!=0.0f) {
      r227=long(buffer2[r223*1+0]);
    }
    if(r226==0.0f) {
      long r240=(as_type<long>(ulong((ulong((long(floor((((((min((max((as_type<float>(uint((long(buffer2[r223*1+0]))))),r230)),r232))*r234))+r236))))))<<(long(r239)&63)))<<1)>>1);
      sb|=(r239>62u)?4u:0u;
      long r251=(as_type<long>(ulong((ulong((long(floor((((((min((max((as_type<float>(uint((long(buffer2[((r223+r241))*1+0]))))),r230)),r232))*r234))+r236))))))<<(long(r250)&63)))<<1)>>1);
      sb|=(r250>62u)?4u:0u;
      long r263=(as_type<long>(ulong((ulong((long(floor((((((min((max((as_type<float>(uint((long(buffer2[((r223+r253))*1+0]))))),r230)),r232))*r234))+r236))))))<<(long(r262)&63)))<<1)>>1);
      sb|=(r262>62u)?4u:0u;
      long r265=(as_type<long>(ulong((r263|long(r264)))<<1)>>1);
      r266=(as_type<long>(ulong((((as_type<long>(ulong((r240|r251))<<1)>>1))|r265))<<1)>>1);
    }
    long r267=(r226!=0.0f?r227:r266);
    long r269=(as_type<long>(ulong(long((ulong(r267)&0x7fffffffffffffffUL)>>(long(r268)&63)))<<1)>>1);
    sb|=(r268>62u)?4u:0u;
    r274=(r273==0.0f?0.0f:(float(((uint(r269)&r270))))/r273);
    long r276=(as_type<long>(ulong(long((ulong(r267)&0x7fffffffffffffffUL)>>(long(r275)&63)))<<1)>>1);
    sb|=(r275>62u)?4u:0u;
    r279=(r273==0.0f?0.0f:(float(((uint(r276)&r270))))/r273);
    long r281=(as_type<long>(ulong(long((ulong(r267)&0x7fffffffffffffffUL)>>(long(r280)&63)))<<1)>>1);
    sb|=(r280>62u)?4u:0u;
    r284=(r273==0.0f?0.0f:(float(((uint(r281)&r270))))/r273);
    r287=(r273==0.0f?0.0f:(float(((uint(r267)&r270))))/r273);
    long r288=(as_type<long>(ulong(long((ulong(r81)&0x7fffffffffffffffUL)>>(long(r268)&63)))<<1)>>1);
    sb|=(r268>62u)?4u:0u;
    r291=(r273==0.0f?0.0f:(float(((uint(r288)&r270))))/r273);
    long r292=(as_type<long>(ulong(long((ulong(r81)&0x7fffffffffffffffUL)>>(long(r275)&63)))<<1)>>1);
    sb|=(r275>62u)?4u:0u;
    r295=(r273==0.0f?0.0f:(float(((uint(r292)&r270))))/r273);
    long r296=(as_type<long>(ulong(long((ulong(r81)&0x7fffffffffffffffUL)>>(long(r280)&63)))<<1)>>1);
    sb|=(r280>62u)?4u:0u;
    r299=(r273==0.0f?0.0f:(float(((uint(r296)&r270))))/r273);
    r302=(r273==0.0f?0.0f:(float(((uint(r81)&r270))))/r273);
    r303=float(r24);
  }
  float r304=(r82!=0.0f?r136:r211);
  float r305=(r82!=0.0f?r146:r220);
  float r306=(r82!=0.0f?r101:r221);
  float r307=(r82!=0.0f?r121:r189);
  float r308=(r82!=0.0f?r153:r274);
  float r309=(r82!=0.0f?r158:r279);
  float r310=(r82!=0.0f?r163:r284);
  float r311=(r82!=0.0f?r166:r287);
  float r312=(r82!=0.0f?r101:r291);
  float r313=(r82!=0.0f?r101:r295);
  float r314=(r82!=0.0f?r101:r299);
  float r315=(r82!=0.0f?r101:r302);
  float r316=(r82!=0.0f?r126:r199);
  float r317=(r82!=0.0f?r127:r202);
  float r318=(r82!=0.0f?r101:r181);
  float r319=(r82!=0.0f?r101:r182);
  float r320=(r82!=0.0f?r101:r183);
  float r321=(r82!=0.0f?r101:r184);
  float r322=(r82!=0.0f?r35:r188);
  float r323=(r82!=0.0f?(as_type<float>(0x40a00000u)):r303);
  float r324=(r82!=0.0f?r29:r221);
  float r325=(r82!=0.0f?r31:r221);
  float r326=(r82!=0.0f?r83:r221);
  float r327=(r82!=0.0f?r86:r221);
  o.position.x=r304;
  o.position.y=r305;
  o.position.z=r306;
  o.position.w=r307;
  o.color.x=r308;
  o.color.y=r309;
  o.color.z=r310;
  o.color.w=r311;
  o.color2.x=r312;
  o.color2.y=r313;
  o.color2.z=r314;
  o.color2.w=r315;
  o.local.x=r316;
  o.local.y=r317;
  o.box.x=r318;
  o.box.y=r319;
  o.box.z=r320;
  o.box.w=r321;
  o.params.x=r306;
  o.params.y=r322;
  o.params.z=r307;
  o.params.w=r323;
  o.extra.x=r324;
  o.extra.y=r325;
  o.extra.z=r326;
  o.extra.w=r327;
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
