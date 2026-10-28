// Generated from scene.rays by Flow_gpu.Raster (lib/flow_gpu/test_raster.exe --write); do not edit.
#include <metal_stdlib>
using namespace metal;
long fmod64(long a,long b){if(b==0)return 0;long r=a%b;return (r!=0&&((r<0)!=(b<0)))?r+b:r;}
struct scene3_vertex_out {
  float4 position [[position]];
  float4 color;
  float2 uv;
  float3 world;
  float3 normal;
};
vertex scene3_vertex_out scene3_vertex(
    uint vid [[vertex_id]],
    uint iid [[instance_id]],
    device const float* buffer0 [[buffer(0)]],
    device const uint* buffer1 [[buffer(1)]],
    device const float* buffer2 [[buffer(2)]],
    device const float* buffer3 [[buffer(3)]],
    device const float* buffer4 [[buffer(4)]],
    device const uint* buffer5 [[buffer(5)]]) {
  scene3_vertex_out o;
  uint sb=0u;
  float r9=as_type<float>(0x3f000000u),r15=0.0f;
  uint r16=1u;
  float r18=0.0f;
  uint r19=2u;
  float r21=0.0f;
  uint r24=1u,r27=31u,r29=0u;
  float r30=0.0f;
  uint r34=20u,r36=2047u,r38=1048575u,r40=32u;
  float r44=as_type<float>(0x59800000u),r46=0.0f,r49=0.0f;
  uint r52=1023u;
  float r56=0.0f,r57=0.0f;
  uint r58=2u;
  float r64=0.0f,r75=0.0f,r78=0.0f;
  uint r81=1023u;
  float r85=0.0f,r86=0.0f;
  uint r87=4u;
  float r93=0.0f,r104=0.0f,r107=0.0f;
  uint r110=1023u;
  float r114=0.0f,r115=0.0f;
  uint r121=3u;
  float r123=0.0f;
  uint r124=4u;
  float r126=0.0f;
  uint r127=5u;
  float r129=0.0f;
  uint r131=6u;
  float r137=0.0f,r148=0.0f,r151=0.0f;
  uint r154=1023u;
  float r158=0.0f,r159=0.0f;
  uint r160=8u;
  float r166=0.0f,r177=0.0f,r180=0.0f;
  uint r183=1023u;
  float r187=0.0f,r188=0.0f;
  uint r189=10u;
  float r195=0.0f,r206=0.0f,r209=0.0f;
  uint r212=1023u;
  float r216=0.0f,r217=0.0f;
  uint r227=48u,r229=3u,r232=2u,r235=1u;
  float r242=0.0f;
  uint r243=4u;
  float r255=0.0f;
  uint r256=8u;
  float r268=0.0f;
  uint r269=12u;
  float r281=0.0f,r293=0.0f,r305=0.0f,r317=0.0f,r329=0.0f;
  uint r334=16u;
  float r346=0.0f,r358=0.0f,r370=0.0f,r394=0.0f,r406=0.0f,r418=0.0f;
  uint r435=32u;
  float r447=0.0f,r459=0.0f,r471=0.0f,r495=0.0f,r507=0.0f,r519=0.0f;
  uint r536=3u;
  long r538=0;
  uint r539=12u;
  long r541=0;
  uint r543=24u,r545=255u;
  float r548=as_type<float>(0x437f0000u);
  uint r550=16u,r555=8u;
  float r564=0.0f,r566=0.0f;
  uint r567=13u;
  float r573=0.0f,r584=0.0f,r587=0.0f;
  uint r590=1023u;
  float r594=0.0f,r595=0.0f;
  uint r596=15u;
  float r602=0.0f,r613=0.0f,r616=0.0f;
  uint r619=1023u;
  float r623=0.0f,r624=0.0f;
  long r0=long(vid);
  float r10=((buffer2[(82u)*1+0])>r9);
  uint r13=(r10!=0.0f?(6u):(17u));
  uint r14=(uint(r0)*r13);
  if(r10!=0.0f) {
    r15=buffer0[r14*1+0];
    r18=buffer0[((r14+r16))*1+0];
    r21=buffer0[((r14+r19))*1+0];
  }
  if(r10==0.0f) {
    long r23=long(buffer1[r14*1+0]);
    long r26=long(buffer1[((r14+r24))*1+0]);
    long r28=(as_type<long>(ulong(long((ulong(r26)&0x7fffffffffffffffUL)>>(long(r27)&63)))<<1)>>1);
    sb|=(r27>62u)?4u:0u;
    r30=(r28==long(r29)?1.0f:0.0f);
    float r33=(r30!=0.0f?(as_type<float>(0x3f800000u)):(as_type<float>(0xbf800000u)));
    long r35=(as_type<long>(ulong(long((ulong(r26)&0x7fffffffffffffffUL)>>(long(r34)&63)))<<1)>>1);
    sb|=(r34>62u)?4u:0u;
    uint r37=(uint(r35)&r36);
    long r41=(as_type<long>(ulong((ulong(long(((uint(r26)&r38))))<<(long(r40)&63)))<<1)>>1);
    sb|=(r40>62u)?4u:0u;
    float r45=(r44==0.0f?0.0f:(float(((as_type<long>(ulong((r41|r23))<<1)>>1))))/r44);
    r46=(long(r37)==long(r29)?1.0f:0.0f);
    if(r46!=0.0f) {
      r49=(r33*(ldexp(r45,int((as_type<float>(0xc47f8000u))))));
    }
    if(r46==0.0f) {
      r56=(r33*(ldexp((((as_type<float>(0x3f800000u))+r45)),int((float(((as_type<long>(ulong((ulong(long(r37))-ulong(long(r52))))<<1)>>1))))))));
    }
    r57=(r46!=0.0f?r49:r56);
    uint r59=(r14+r58);
    long r60=long(buffer1[r59*1+0]);
    long r62=long(buffer1[((r59+r24))*1+0]);
    long r63=(as_type<long>(ulong(long((ulong(r62)&0x7fffffffffffffffUL)>>(long(r27)&63)))<<1)>>1);
    sb|=(r27>62u)?4u:0u;
    r64=(r63==long(r29)?1.0f:0.0f);
    float r67=(r64!=0.0f?(as_type<float>(0x3f800000u)):(as_type<float>(0xbf800000u)));
    long r68=(as_type<long>(ulong(long((ulong(r62)&0x7fffffffffffffffUL)>>(long(r34)&63)))<<1)>>1);
    sb|=(r34>62u)?4u:0u;
    uint r69=(uint(r68)&r36);
    long r71=(as_type<long>(ulong((ulong(long(((uint(r62)&r38))))<<(long(r40)&63)))<<1)>>1);
    sb|=(r40>62u)?4u:0u;
    float r74=(r44==0.0f?0.0f:(float(((as_type<long>(ulong((r71|r60))<<1)>>1))))/r44);
    r75=(long(r69)==long(r29)?1.0f:0.0f);
    if(r75!=0.0f) {
      r78=(r67*(ldexp(r74,int((as_type<float>(0xc47f8000u))))));
    }
    if(r75==0.0f) {
      r85=(r67*(ldexp((((as_type<float>(0x3f800000u))+r74)),int((float(((as_type<long>(ulong((ulong(long(r69))-ulong(long(r81))))<<1)>>1))))))));
    }
    r86=(r75!=0.0f?r78:r85);
    uint r88=(r14+r87);
    long r89=long(buffer1[r88*1+0]);
    long r91=long(buffer1[((r88+r24))*1+0]);
    long r92=(as_type<long>(ulong(long((ulong(r91)&0x7fffffffffffffffUL)>>(long(r27)&63)))<<1)>>1);
    sb|=(r27>62u)?4u:0u;
    r93=(r92==long(r29)?1.0f:0.0f);
    float r96=(r93!=0.0f?(as_type<float>(0x3f800000u)):(as_type<float>(0xbf800000u)));
    long r97=(as_type<long>(ulong(long((ulong(r91)&0x7fffffffffffffffUL)>>(long(r34)&63)))<<1)>>1);
    sb|=(r34>62u)?4u:0u;
    uint r98=(uint(r97)&r36);
    long r100=(as_type<long>(ulong((ulong(long(((uint(r91)&r38))))<<(long(r40)&63)))<<1)>>1);
    sb|=(r40>62u)?4u:0u;
    float r103=(r44==0.0f?0.0f:(float(((as_type<long>(ulong((r100|r89))<<1)>>1))))/r44);
    r104=(long(r98)==long(r29)?1.0f:0.0f);
    if(r104!=0.0f) {
      r107=(r96*(ldexp(r103,int((as_type<float>(0xc47f8000u))))));
    }
    if(r104==0.0f) {
      r114=(r96*(ldexp((((as_type<float>(0x3f800000u))+r103)),int((float(((as_type<long>(ulong((ulong(long(r98))-ulong(long(r110))))<<1)>>1))))))));
    }
    r115=(r104!=0.0f?r107:r114);
  }
  float r117=(r10!=0.0f?r15:r57);
  float r118=(r10!=0.0f?r18:r86);
  float r119=(r10!=0.0f?r21:r115);
  float r120=(r10!=0.0f?(as_type<float>(0x3f800000u)):(as_type<float>(0x3f800000u)));
  if(r10!=0.0f) {
    r123=buffer0[((r14+r121))*1+0];
    r126=buffer0[((r14+r124))*1+0];
    r129=buffer0[((r14+r127))*1+0];
  }
  if(r10==0.0f) {
    uint r132=(r14+r131);
    long r133=long(buffer1[r132*1+0]);
    long r135=long(buffer1[((r132+r24))*1+0]);
    long r136=(as_type<long>(ulong(long((ulong(r135)&0x7fffffffffffffffUL)>>(long(r27)&63)))<<1)>>1);
    sb|=(r27>62u)?4u:0u;
    r137=(r136==long(r29)?1.0f:0.0f);
    float r140=(r137!=0.0f?(as_type<float>(0x3f800000u)):(as_type<float>(0xbf800000u)));
    long r141=(as_type<long>(ulong(long((ulong(r135)&0x7fffffffffffffffUL)>>(long(r34)&63)))<<1)>>1);
    sb|=(r34>62u)?4u:0u;
    uint r142=(uint(r141)&r36);
    long r144=(as_type<long>(ulong((ulong(long(((uint(r135)&r38))))<<(long(r40)&63)))<<1)>>1);
    sb|=(r40>62u)?4u:0u;
    float r147=(r44==0.0f?0.0f:(float(((as_type<long>(ulong((r144|r133))<<1)>>1))))/r44);
    r148=(long(r142)==long(r29)?1.0f:0.0f);
    if(r148!=0.0f) {
      r151=(r140*(ldexp(r147,int((as_type<float>(0xc47f8000u))))));
    }
    if(r148==0.0f) {
      r158=(r140*(ldexp((((as_type<float>(0x3f800000u))+r147)),int((float(((as_type<long>(ulong((ulong(long(r142))-ulong(long(r154))))<<1)>>1))))))));
    }
    r159=(r148!=0.0f?r151:r158);
    uint r161=(r14+r160);
    long r162=long(buffer1[r161*1+0]);
    long r164=long(buffer1[((r161+r24))*1+0]);
    long r165=(as_type<long>(ulong(long((ulong(r164)&0x7fffffffffffffffUL)>>(long(r27)&63)))<<1)>>1);
    sb|=(r27>62u)?4u:0u;
    r166=(r165==long(r29)?1.0f:0.0f);
    float r169=(r166!=0.0f?(as_type<float>(0x3f800000u)):(as_type<float>(0xbf800000u)));
    long r170=(as_type<long>(ulong(long((ulong(r164)&0x7fffffffffffffffUL)>>(long(r34)&63)))<<1)>>1);
    sb|=(r34>62u)?4u:0u;
    uint r171=(uint(r170)&r36);
    long r173=(as_type<long>(ulong((ulong(long(((uint(r164)&r38))))<<(long(r40)&63)))<<1)>>1);
    sb|=(r40>62u)?4u:0u;
    float r176=(r44==0.0f?0.0f:(float(((as_type<long>(ulong((r173|r162))<<1)>>1))))/r44);
    r177=(long(r171)==long(r29)?1.0f:0.0f);
    if(r177!=0.0f) {
      r180=(r169*(ldexp(r176,int((as_type<float>(0xc47f8000u))))));
    }
    if(r177==0.0f) {
      r187=(r169*(ldexp((((as_type<float>(0x3f800000u))+r176)),int((float(((as_type<long>(ulong((ulong(long(r171))-ulong(long(r183))))<<1)>>1))))))));
    }
    r188=(r177!=0.0f?r180:r187);
    uint r190=(r14+r189);
    long r191=long(buffer1[r190*1+0]);
    long r193=long(buffer1[((r190+r24))*1+0]);
    long r194=(as_type<long>(ulong(long((ulong(r193)&0x7fffffffffffffffUL)>>(long(r27)&63)))<<1)>>1);
    sb|=(r27>62u)?4u:0u;
    r195=(r194==long(r29)?1.0f:0.0f);
    float r198=(r195!=0.0f?(as_type<float>(0x3f800000u)):(as_type<float>(0xbf800000u)));
    long r199=(as_type<long>(ulong(long((ulong(r193)&0x7fffffffffffffffUL)>>(long(r34)&63)))<<1)>>1);
    sb|=(r34>62u)?4u:0u;
    uint r200=(uint(r199)&r36);
    long r202=(as_type<long>(ulong((ulong(long(((uint(r193)&r38))))<<(long(r40)&63)))<<1)>>1);
    sb|=(r40>62u)?4u:0u;
    float r205=(r44==0.0f?0.0f:(float(((as_type<long>(ulong((r202|r191))<<1)>>1))))/r44);
    r206=(long(r200)==long(r29)?1.0f:0.0f);
    if(r206!=0.0f) {
      r209=(r198*(ldexp(r205,int((as_type<float>(0xc47f8000u))))));
    }
    if(r206==0.0f) {
      r216=(r198*(ldexp((((as_type<float>(0x3f800000u))+r205)),int((float(((as_type<long>(ulong((ulong(long(r200))-ulong(long(r212))))<<1)>>1))))))));
    }
    r217=(r206!=0.0f?r209:r216);
  }
  float r219=(r10!=0.0f?r123:r159);
  float r220=(r10!=0.0f?r126:r188);
  float r221=(r10!=0.0f?r129:r217);
  float r222=(r10!=0.0f?(as_type<float>(0x0u)):(as_type<float>(0x0u)));
  float r225=((buffer2[(83u)*1+0])>r9);
  uint r228=(uint((long(iid)))*r227);
  if(r225!=0.0f) {
    r242=fma(r120,(buffer3[((r228+r229))*1+0]),(fma(r119,(buffer3[((r228+r232))*1+0]),(fma(r118,(buffer3[((r228+r235))*1+0]),((r117*(buffer3[r228*1+0]))))))));
    uint r244=(r228+r243);
    r255=fma(r120,(buffer3[((r244+r229))*1+0]),(fma(r119,(buffer3[((r244+r232))*1+0]),(fma(r118,(buffer3[((r244+r235))*1+0]),((r117*(buffer3[r244*1+0]))))))));
    uint r257=(r228+r256);
    r268=fma(r120,(buffer3[((r257+r229))*1+0]),(fma(r119,(buffer3[((r257+r232))*1+0]),(fma(r118,(buffer3[((r257+r235))*1+0]),((r117*(buffer3[r257*1+0]))))))));
    uint r270=(r228+r269);
    r281=fma(r120,(buffer3[((r270+r229))*1+0]),(fma(r119,(buffer3[((r270+r232))*1+0]),(fma(r118,(buffer3[((r270+r235))*1+0]),((r117*(buffer3[r270*1+0]))))))));
  }
  if(r225==0.0f) {
    r293=fma(r120,(buffer2[(3u)*1+0]),(fma(r119,(buffer2[(2u)*1+0]),(fma(r118,(buffer2[(1u)*1+0]),((r117*(buffer2[(0u)*1+0]))))))));
    r305=fma(r120,(buffer2[(7u)*1+0]),(fma(r119,(buffer2[(6u)*1+0]),(fma(r118,(buffer2[(5u)*1+0]),((r117*(buffer2[(4u)*1+0]))))))));
    r317=fma(r120,(buffer2[(11u)*1+0]),(fma(r119,(buffer2[(10u)*1+0]),(fma(r118,(buffer2[(9u)*1+0]),((r117*(buffer2[(8u)*1+0]))))))));
    r329=fma(r120,(buffer2[(15u)*1+0]),(fma(r119,(buffer2[(14u)*1+0]),(fma(r118,(buffer2[(13u)*1+0]),((r117*(buffer2[(12u)*1+0]))))))));
  }
  float r330=(r225!=0.0f?r242:r293);
  float r331=(r225!=0.0f?r255:r305);
  float r332=(r225!=0.0f?r268:r317);
  float r333=(r225!=0.0f?r281:r329);
  if(r225!=0.0f) {
    uint r335=(r228+r334);
    r346=fma(r120,(buffer3[((r335+r229))*1+0]),(fma(r119,(buffer3[((r335+r232))*1+0]),(fma(r118,(buffer3[((r335+r235))*1+0]),((r117*(buffer3[r335*1+0]))))))));
    uint r347=(r335+r243);
    r358=fma(r120,(buffer3[((r347+r229))*1+0]),(fma(r119,(buffer3[((r347+r232))*1+0]),(fma(r118,(buffer3[((r347+r235))*1+0]),((r117*(buffer3[r347*1+0]))))))));
    uint r359=(r335+r256);
    r370=fma(r120,(buffer3[((r359+r229))*1+0]),(fma(r119,(buffer3[((r359+r232))*1+0]),(fma(r118,(buffer3[((r359+r235))*1+0]),((r117*(buffer3[r359*1+0]))))))));
  }
  if(r225==0.0f) {
    r394=fma(r120,(buffer2[(19u)*1+0]),(fma(r119,(buffer2[(18u)*1+0]),(fma(r118,(buffer2[(17u)*1+0]),((r117*(buffer2[(16u)*1+0]))))))));
    r406=fma(r120,(buffer2[(23u)*1+0]),(fma(r119,(buffer2[(22u)*1+0]),(fma(r118,(buffer2[(21u)*1+0]),((r117*(buffer2[(20u)*1+0]))))))));
    r418=fma(r120,(buffer2[(27u)*1+0]),(fma(r119,(buffer2[(26u)*1+0]),(fma(r118,(buffer2[(25u)*1+0]),((r117*(buffer2[(24u)*1+0]))))))));
  }
  float r431=(r225!=0.0f?r346:r394);
  float r432=(r225!=0.0f?r358:r406);
  float r433=(r225!=0.0f?r370:r418);
  if(r225!=0.0f) {
    uint r436=(r228+r435);
    r447=fma(r222,(buffer3[((r436+r229))*1+0]),(fma(r221,(buffer3[((r436+r232))*1+0]),(fma(r220,(buffer3[((r436+r235))*1+0]),((r219*(buffer3[r436*1+0]))))))));
    uint r448=(r436+r243);
    r459=fma(r222,(buffer3[((r448+r229))*1+0]),(fma(r221,(buffer3[((r448+r232))*1+0]),(fma(r220,(buffer3[((r448+r235))*1+0]),((r219*(buffer3[r448*1+0]))))))));
    uint r460=(r436+r256);
    r471=fma(r222,(buffer3[((r460+r229))*1+0]),(fma(r221,(buffer3[((r460+r232))*1+0]),(fma(r220,(buffer3[((r460+r235))*1+0]),((r219*(buffer3[r460*1+0]))))))));
  }
  if(r225==0.0f) {
    r495=fma(r222,(buffer2[(35u)*1+0]),(fma(r221,(buffer2[(34u)*1+0]),(fma(r220,(buffer2[(33u)*1+0]),((r219*(buffer2[(32u)*1+0]))))))));
    r507=fma(r222,(buffer2[(39u)*1+0]),(fma(r221,(buffer2[(38u)*1+0]),(fma(r220,(buffer2[(37u)*1+0]),((r219*(buffer2[(36u)*1+0]))))))));
    r519=fma(r222,(buffer2[(43u)*1+0]),(fma(r221,(buffer2[(42u)*1+0]),(fma(r220,(buffer2[(41u)*1+0]),((r219*(buffer2[(40u)*1+0]))))))));
  }
  float r532=(r225!=0.0f?r447:r495);
  float r533=(r225!=0.0f?r459:r507);
  float r534=(r225!=0.0f?r471:r519);
  uint r537=(uint(r0)*r536);
  if(r10!=0.0f) {
    r538=long(buffer5[r537*1+0]);
  }
  if(r10==0.0f) {
    r541=long(buffer1[((r14+r539))*1+0]);
  }
  long r542=(r10!=0.0f?r538:r541);
  long r544=(as_type<long>(ulong(long((ulong(r542)&0x7fffffffffffffffUL)>>(long(r543)&63)))<<1)>>1);
  sb|=(r543>62u)?4u:0u;
  float r549=(r548==0.0f?0.0f:(float(((uint(r544)&r545))))/r548);
  long r551=(as_type<long>(ulong(long((ulong(r542)&0x7fffffffffffffffUL)>>(long(r550)&63)))<<1)>>1);
  sb|=(r550>62u)?4u:0u;
  float r554=(r548==0.0f?0.0f:(float(((uint(r551)&r545))))/r548);
  long r556=(as_type<long>(ulong(long((ulong(r542)&0x7fffffffffffffffUL)>>(long(r555)&63)))<<1)>>1);
  sb|=(r555>62u)?4u:0u;
  float r559=(r548==0.0f?0.0f:(float(((uint(r556)&r545))))/r548);
  float r562=(r548==0.0f?0.0f:(float(((uint(r542)&r545))))/r548);
  if(r10!=0.0f) {
    r564=buffer4[((r537+r16))*1+0];
    r566=buffer4[((r537+r19))*1+0];
  }
  if(r10==0.0f) {
    uint r568=(r14+r567);
    long r569=long(buffer1[r568*1+0]);
    long r571=long(buffer1[((r568+r24))*1+0]);
    long r572=(as_type<long>(ulong(long((ulong(r571)&0x7fffffffffffffffUL)>>(long(r27)&63)))<<1)>>1);
    sb|=(r27>62u)?4u:0u;
    r573=(r572==long(r29)?1.0f:0.0f);
    float r576=(r573!=0.0f?(as_type<float>(0x3f800000u)):(as_type<float>(0xbf800000u)));
    long r577=(as_type<long>(ulong(long((ulong(r571)&0x7fffffffffffffffUL)>>(long(r34)&63)))<<1)>>1);
    sb|=(r34>62u)?4u:0u;
    uint r578=(uint(r577)&r36);
    long r580=(as_type<long>(ulong((ulong(long(((uint(r571)&r38))))<<(long(r40)&63)))<<1)>>1);
    sb|=(r40>62u)?4u:0u;
    float r583=(r44==0.0f?0.0f:(float(((as_type<long>(ulong((r580|r569))<<1)>>1))))/r44);
    r584=(long(r578)==long(r29)?1.0f:0.0f);
    if(r584!=0.0f) {
      r587=(r576*(ldexp(r583,int((as_type<float>(0xc47f8000u))))));
    }
    if(r584==0.0f) {
      r594=(r576*(ldexp((((as_type<float>(0x3f800000u))+r583)),int((float(((as_type<long>(ulong((ulong(long(r578))-ulong(long(r590))))<<1)>>1))))))));
    }
    r595=(r584!=0.0f?r587:r594);
    uint r597=(r14+r596);
    long r598=long(buffer1[r597*1+0]);
    long r600=long(buffer1[((r597+r24))*1+0]);
    long r601=(as_type<long>(ulong(long((ulong(r600)&0x7fffffffffffffffUL)>>(long(r27)&63)))<<1)>>1);
    sb|=(r27>62u)?4u:0u;
    r602=(r601==long(r29)?1.0f:0.0f);
    float r605=(r602!=0.0f?(as_type<float>(0x3f800000u)):(as_type<float>(0xbf800000u)));
    long r606=(as_type<long>(ulong(long((ulong(r600)&0x7fffffffffffffffUL)>>(long(r34)&63)))<<1)>>1);
    sb|=(r34>62u)?4u:0u;
    uint r607=(uint(r606)&r36);
    long r609=(as_type<long>(ulong((ulong(long(((uint(r600)&r38))))<<(long(r40)&63)))<<1)>>1);
    sb|=(r40>62u)?4u:0u;
    float r612=(r44==0.0f?0.0f:(float(((as_type<long>(ulong((r609|r598))<<1)>>1))))/r44);
    r613=(long(r607)==long(r29)?1.0f:0.0f);
    if(r613!=0.0f) {
      r616=(r605*(ldexp(r612,int((as_type<float>(0xc47f8000u))))));
    }
    if(r613==0.0f) {
      r623=(r605*(ldexp((((as_type<float>(0x3f800000u))+r612)),int((float(((as_type<long>(ulong((ulong(long(r607))-ulong(long(r619))))<<1)>>1))))))));
    }
    r624=(r613!=0.0f?r616:r623);
  }
  float r625=(r10!=0.0f?r564:r595);
  float r626=(r10!=0.0f?r566:r624);
  float r630=rsqrt(fabs((fma(r534,r534,(fma(r533,r533,((r532*r532))))))));
  float r631=(r532*r630);
  float r632=(r533*r630);
  float r633=(r534*r630);
  o.position.x=r330;
  o.position.y=r331;
  o.position.z=r332;
  o.position.w=r333;
  o.color.x=r549;
  o.color.y=r554;
  o.color.z=r559;
  o.color.w=r562;
  o.uv.x=r625;
  o.uv.y=r626;
  o.world.x=r431;
  o.world.y=r432;
  o.world.z=r433;
  o.normal.x=r631;
  o.normal.y=r632;
  o.normal.z=r633;
  return o;
}
fragment float4 scene3_fragment(
    scene3_vertex_out in [[stage_in]],
    bool front [[front_facing]],
    device const float* buffer2 [[buffer(2)]]) {
  float4 colour;
  uint sb=0u;
  float r13=as_type<float>(0x3f800000u),r23=as_type<float>(0x3f000000u),r28=as_type<float>(0xbf800000u),r29=0.0f,r30=0.0f,r31=0.0f;
  uint r54=64u;
  float r77=as_type<float>(0x0u);
  uint r78=0u;
  int r79=0;
  float r80=0,r81=0,r82=0,r83=0,r84=0,r85=0,r440=0.0f,r441=0.0f,r442=0.0f,r446=0.0f,r447=0.0f,r448=0.0f;
  float r17=rsqrt(fabs((fma((in.normal.z),(in.normal.z),(fma((in.normal.y),(in.normal.y),(((in.normal.x)*(in.normal.x)))))))));
  float r18=((in.normal.x)*r17);
  float r19=((in.normal.y)*r17);
  float r20=((in.normal.z)*r17);
  float r27=((((buffer2[(74u)*1+0])>r23))!=0.0f&&((((front?1.0f:0.0f))==0.0f))!=0.0f);
  if(r27!=0.0f) {
    r29=(r18*r28);
    r30=(r19*r28);
    r31=(r20*r28);
  }
  float r32=(r27!=0.0f?r29:r18);
  float r33=(r27!=0.0f?r30:r19);
  float r34=(r27!=0.0f?r31:r20);
  float r41=((buffer2[(48u)*1+0])-(in.world.x));
  float r42=((buffer2[(49u)*1+0])-(in.world.y));
  float r43=((buffer2[(50u)*1+0])-(in.world.z));
  float r47=rsqrt(fabs((fma(r43,r43,(fma(r42,r42,((r41*r41))))))));
  float r48=(r41*r47);
  float r49=(r42*r47);
  float r50=(r43*r47);
  long r53=long((buffer2[(73u)*1+0]));
  long r56=(((r53<long(r54)?1.0f:0.0f))!=0.0f?r53:long(r54));
  bool over79=r56>long(r78) && ulong(r56-long(r78))>4096uL;
  sb|=over79?1u:0u;
  r80=(fma((buffer2[(52u)*1+0]),(buffer2[(69u)*1+0]),(buffer2[r54*1+0])));
  r81=(fma((buffer2[(53u)*1+0]),(buffer2[(70u)*1+0]),(buffer2[(65u)*1+0])));
  r82=(fma((buffer2[(54u)*1+0]),(buffer2[(71u)*1+0]),(buffer2[(66u)*1+0])));
  r83=r77;
  r84=r77;
  r85=r77;
  for(r79=long(r78);r79<(over79?long(r78):r56);r79++){
  uint r86=20u,r91=8u,r94=1u,r99=2u;
  float r104=as_type<float>(0xbf800000u),r112=0.0f,r113=0.0f,r114=0.0f,r116=0.0f,r124=as_type<float>(0x0u),r125=0.0f,r126=0.0f,r127=0.0f,r128=0.0f,r129=as_type<float>(0x0u),r130=0.0f;
  float r131=0.0f,r132=0.0f,r133=as_type<float>(0x3fc00000u);
  uint r136=9u,r139=2u,r143=1u;
  float r149=0.0f,r150=0.0f,r153=as_type<float>(0xbf800000u),r173=0.0f;
  uint r174=13u;
  float r177=0.0f;
  uint r181=14u;
  float r184=0.0f,r186=0.0f,r187=0.0f;
  uint r188=9u,r191=1u,r194=2u;
  float r206=0.0f,r207=as_type<float>(0x0u),r209=as_type<float>(0x0u),r214=as_type<float>(0xbf800000u);
  uint r240=14u;
  float r250=as_type<float>(0x0u);
  uint r251=0u;
  int r252=0;
  float r253=0,r254=0,r255=0,r256=0,r343=0.0f,r348=0.0f,r353=0.0f,r354=0.0f,r355=0.0f,r356=as_type<float>(0x0u),r357=0.0f,r358=0.0f;
  float r359=0.0f,r360=0.0f,r361=0.0f,r362=0.0f,r363=0.0f;
  uint r368=4u;
  float r378=as_type<float>(0x0u),r421=0.0f,r422=0.0f,r423=0.0f;
  uint r87=(uint(r79)*r86);
  uint r89=((84u)+r87);
  float r90=buffer2[r89*1+0];
  float r93=buffer2[((r89+r91))*1+0];
  uint r95=(r89+r94);
  float r96=buffer2[r95*1+0];
  float r98=buffer2[((r95+r94))*1+0];
  float r101=buffer2[((r95+r99))*1+0];
  float r103=(r90<(as_type<float>(0x3f000000u)));
  if(r103!=0.0f) {
    float r105=(r96*r104);
    float r106=(r98*r104);
    float r107=(r101*r104);
    float r111=rsqrt(fabs((fma(r107,r107,(fma(r106,r106,((r105*r105))))))));
    r112=(r105*r111);
    r113=(r106*r111);
    r114=(r107*r111);
  }
  if(r103==0.0f) {
    r116=(r90<(as_type<float>(0x40200000u)));
    if(r116!=0.0f) {
      float r117=(r96-(in.world.x));
      float r118=(r98-(in.world.y));
      float r119=(r101-(in.world.z));
      float r123=sqrt(fabs((fma(r119,r119,(fma(r118,r118,((r117*r117))))))));
      r125=(r123>r124);
      if(r125!=0.0f) {
        r126=(r123==0.0f?0.0f:r117/r123);
        r127=(r123==0.0f?0.0f:r118/r123);
        r128=(r123==0.0f?0.0f:r119/r123);
      }
      r130=(r125!=0.0f?r126:r129);
      r131=(r125!=0.0f?r127:r129);
      r132=(r125!=0.0f?r128:r129);
      uint r137=(((r90<r133))!=0.0f?r136:(15u));
      uint r138=(r89+r137);
      float r148=fma((((buffer2[((r138+r139))*1+0])*r123)),r123,(fma((buffer2[((r138+r143))*1+0]),r123,(buffer2[r138*1+0]))));
      r149=(r148>r124);
      if(r149!=0.0f) {
        r150=(r148==0.0f?0.0f:r93/r148);
      }
      float r152=(r149!=0.0f?r150:(as_type<float>(0x0u)));
      uint r157=(r89+r136);
      float r158=buffer2[r157*1+0];
      float r160=buffer2[((r157+r143))*1+0];
      float r162=buffer2[((r157+r139))*1+0];
      float r166=rsqrt(fabs((fma(r162,r162,(fma(r160,r160,((r158*r158))))))));
      float r172=fma(((r132*r153)),((r162*r166)),(fma(((r131*r153)),((r160*r166)),((((r130*r153))*((r158*r166)))))));
      r173=(r90>=r133);
      if(r173!=0.0f) {
        r177=(r172<(buffer2[((r89+r174))*1+0]));
        if(r177==0.0f) {
          r184=pow(fabs((max(r172,(as_type<float>(0x0u))))),(buffer2[((r89+r181))*1+0]));
        }
        r186=(r152*((r177!=0.0f?(as_type<float>(0x0u)):r184)));
      }
      r187=(r173!=0.0f?r186:r152);
    }
    if(r116==0.0f) {
      uint r189=(r89+r188);
      float r190=buffer2[r189*1+0];
      float r193=buffer2[((r189+r191))*1+0];
      float r196=buffer2[((r189+r194))*1+0];
      float r200=rsqrt(fabs((fma(r196,r196,(fma(r193,r193,((r190*r190))))))));
      float r201=(r190*r200);
      float r202=(r193*r200);
      float r203=(r196*r200);
      r206=((fabs(r203))<(as_type<float>(0x3f7fbe77u)));
      float r211=(r206!=0.0f?r207:r209);
      float r212=(r206!=0.0f?r207:(as_type<float>(0x3f800000u)));
      float r213=(r206!=0.0f?(as_type<float>(0x3f800000u)):r209);
      float r217=fma(r212,r203,((r214*((r213*r202)))));
      float r220=fma(r213,r201,((r214*((r211*r203)))));
      float r223=fma(r211,r202,((r214*((r212*r201)))));
      float r227=rsqrt(fabs((fma(r223,r223,(fma(r220,r220,((r217*r217))))))));
      float r228=(r217*r227);
      float r229=(r220*r227);
      float r230=(r223*r227);
      float r233=fma(r202,r230,((r214*((r203*r229)))));
      float r236=fma(r203,r228,((r214*((r201*r230)))));
      float r239=fma(r201,r229,((r214*((r202*r228)))));
      long r244=long(round((sqrt(fabs((buffer2[((r89+r240))*1+0]))))));
      long r245=r244;
      long r247=(((r245<long(r191)?1.0f:0.0f))!=0.0f?long(r191):r245);
      long r248=(as_type<long>(ulong((ulong(r247)*ulong(r247)))<<1)>>1);
      float r249=float(r248);
      bool over252=r247>long(r251) && ulong(r247-long(r251))>4096uL;
      sb|=over252?1u:0u;
      r253=r250;
      r254=r250;
      r255=r250;
      r256=r250;
      for(r252=long(r251);r252<(over252?long(r251):r247);r252++){
  uint r257=0u;
  int r258=0;
  float r259=0,r260=0,r261=0,r262=0,r335=0.0f,r336=0.0f,r337=0.0f,r338=0.0f;
  if(r103==0.0f) {
    if(r116==0.0f) {
      bool over258=r247>long(r257) && ulong(r247-long(r257))>4096uL;
      sb|=over258?1u:0u;
      r259=r253;
      r260=r254;
      r261=r255;
      r262=r256;
      for(r258=long(r257);r258<(over258?long(r257):r247);r258++){
  float r264=as_type<float>(0x3f000000u);
  uint r273=12u,r277=13u;
  float r295=0.0f,r299=as_type<float>(0xbf800000u),r306=as_type<float>(0x0u);
  uint r308=15u,r310=2u,r314=1u;
  float r320=0.0f,r323=0.0f,r326=0.0f,r327=0.0f,r328=0.0f,r329=0.0f,r330=0.0f,r331=0.0f,r332=0.0f,r333=0.0f;
  if(r103==0.0f) {
    if(r116==0.0f) {
      float r266=float(r247);
      float r276=(((((r266==0.0f?0.0f:(((float(r258))+r264))/r266))-r264))*(buffer2[((r89+r273))*1+0]));
      float r280=(((((r266==0.0f?0.0f:(((float(r252))+r264))/r266))-r264))*(buffer2[((r89+r277))*1+0]));
      float r287=((fma(r280,r233,(fma(r276,r228,r96))))-(in.world.x));
      float r288=((fma(r280,r236,(fma(r276,r229,r98))))-(in.world.y));
      float r289=((fma(r280,r239,(fma(r276,r230,r101))))-(in.world.z));
      float r293=sqrt(fabs((fma(r289,r289,(fma(r288,r288,((r287*r287))))))));
      r295=(r293>(as_type<float>(0x0u)));
      if(r295!=0.0f) {
        float r296=(r293==0.0f?0.0f:r287/r293);
        float r297=(r293==0.0f?0.0f:r288/r293);
        float r298=(r293==0.0f?0.0f:r289/r293);
        uint r309=(r89+r308);
        float r319=fma((((buffer2[((r309+r310))*1+0])*r293)),r293,(fma((buffer2[((r309+r314))*1+0]),r293,(buffer2[r309*1+0]))));
        r320=(r319>r306);
        if(r320!=0.0f) {
          r323=(((r249*r319))==0.0f?0.0f:((r93*(max((fma(r203,((r298*r299)),(fma(r202,((r297*r299)),((r201*((r296*r299)))))))),r306))))/((r249*r319)));
        }
        float r325=(r320!=0.0f?r323:(as_type<float>(0x0u)));
        r326=fma(r296,r325,r259);
        r327=fma(r297,r325,r260);
        r328=fma(r298,r325,r261);
        r329=(r262+r325);
      }
      r330=(r295!=0.0f?r326:r259);
      r331=(r295!=0.0f?r327:r260);
      r332=(r295!=0.0f?r328:r261);
      r333=(r295!=0.0f?r329:r262);
    }
  }
  float next258_0=r330;
  float next258_1=r331;
  float next258_2=r332;
  float next258_3=r333;
  r259=next258_0;
  r260=next258_1;
  r261=next258_2;
  r262=next258_3;
      }
      r335=r259;
      r336=r260;
      r337=r261;
      r338=r262;
    }
  }
  float next252_0=r335;
  float next252_1=r336;
  float next252_2=r337;
  float next252_3=r338;
  r253=next252_0;
  r254=next252_1;
  r255=next252_2;
  r256=next252_3;
      }
      float r340=r253;
      float r341=r254;
      float r342=r255;
      r343=r256;
      r348=((sqrt(fabs((fma(r342,r342,(fma(r341,r341,((r340*r340)))))))))>r250);
      if(r348!=0.0f) {
        float r352=rsqrt(fabs((fma(r342,r342,(fma(r341,r341,((r340*r340))))))));
        r353=(r340*r352);
        r354=(r341*r352);
        r355=(r342*r352);
      }
      r357=(r348!=0.0f?r353:r356);
      r358=(r348!=0.0f?r354:r356);
      r359=(r348!=0.0f?r355:r356);
    }
    r360=(r116!=0.0f?r130:r357);
    r361=(r116!=0.0f?r131:r358);
    r362=(r116!=0.0f?r132:r359);
    r363=(r116!=0.0f?r187:r343);
  }
  float r364=(r103!=0.0f?r112:r360);
  float r365=(r103!=0.0f?r113:r361);
  float r366=(r103!=0.0f?r114:r362);
  float r367=(r103!=0.0f?r93:r363);
  uint r369=(r89+r368);
  float r370=buffer2[r369*1+0];
  float r372=buffer2[((r369+r94))*1+0];
  float r374=buffer2[((r369+r99))*1+0];
  float r380=((max((fma(r34,r366,(fma(r33,r365,((r32*r364)))))),r378))*r367);
  float r381=(r364+r48);
  float r382=(r365+r49);
  float r383=(r366+r50);
  float r387=rsqrt(fabs((fma(r383,r383,(fma(r382,r382,((r381*r381))))))));
  float r398=((pow(fabs((max((fma(r34,((r383*r387)),(fma(r33,((r382*r387)),((r32*((r381*r387)))))))),r378))),(buffer2[(68u)*1+0])))*r367);
  float r411=(r380>r378);
  if(r411!=0.0f) {
    r421=fma((((buffer2[(60u)*1+0])*r370)),r398,r83);
    r422=fma((((buffer2[(61u)*1+0])*r372)),r398,r84);
    r423=fma((((buffer2[(62u)*1+0])*r374)),r398,r85);
  }
  float next79_0=(fma((((buffer2[(56u)*1+0])*r370)),r380,r80));
  float next79_1=(fma((((buffer2[(57u)*1+0])*r372)),r380,r81));
  float next79_2=(fma((((buffer2[(58u)*1+0])*r374)),r380,r82));
  float next79_3=((r411!=0.0f?r421:r83));
  float next79_4=((r411!=0.0f?r422:r84));
  float next79_5=((r411!=0.0f?r423:r85));
  r80=next79_0;
  r81=next79_1;
  r82=next79_2;
  r83=next79_3;
  r84=next79_4;
  r85=next79_5;
  }
  float r428=r80;
  float r429=r81;
  float r430=r82;
  float r431=r83;
  float r432=r84;
  float r433=r85;
  float r434=((in.color.x)*r13);
  float r435=((in.color.y)*r13);
  float r436=((in.color.z)*r13);
  float r439=((buffer2[(75u)*1+0])>r23);
  if(r439!=0.0f) {
    r440=fma(r428,r434,r431);
    r441=fma(r429,r435,r432);
    r442=fma(r430,r436,r433);
  }
  if(r439==0.0f) {
    r446=(((r428+r431))*r434);
    r447=(((r429+r432))*r435);
    r448=(((r430+r433))*r436);
  }
  float r449=(r439!=0.0f?r440:r446);
  float r450=(r439!=0.0f?r441:r447);
  float r451=(r439!=0.0f?r442:r448);
  float r455=((((buffer2[(59u)*1+0])*(in.color.w)))*r13);
  colour.x=r449;
  colour.y=r450;
  colour.z=r451;
  colour.w=r455;
  return colour;
}
