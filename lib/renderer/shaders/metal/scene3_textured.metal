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
fragment float4 scene3_textured_fragment(
    scene3_vertex_out in [[stage_in]],
    bool front [[front_facing]],
    texture2d<float> texture6 [[texture(6)]],
    sampler sampler7 [[sampler(7)]],
    device const float* buffer2 [[buffer(2)]]) {
  float4 colour;
  uint sb=0u;
  float r26=as_type<float>(0x3f000000u),r31=as_type<float>(0xbf800000u),r32=0.0f,r33=0.0f,r34=0.0f;
  uint r57=64u;
  float r80=as_type<float>(0x0u);
  uint r81=0u;
  int r82=0;
  float r83=0,r84=0,r85=0,r86=0,r87=0,r88=0,r443=0.0f,r444=0.0f,r445=0.0f,r449=0.0f,r450=0.0f,r451=0.0f;
  float r20=rsqrt(fabs((fma((in.normal.z),(in.normal.z),(fma((in.normal.y),(in.normal.y),(((in.normal.x)*(in.normal.x)))))))));
  float r21=((in.normal.x)*r20);
  float r22=((in.normal.y)*r20);
  float r23=((in.normal.z)*r20);
  float r30=((((buffer2[(74u)*1+0])>r26))!=0.0f&&((((front?1.0f:0.0f))==0.0f))!=0.0f);
  if(r30!=0.0f) {
    r32=(r21*r31);
    r33=(r22*r31);
    r34=(r23*r31);
  }
  float r35=(r30!=0.0f?r32:r21);
  float r36=(r30!=0.0f?r33:r22);
  float r37=(r30!=0.0f?r34:r23);
  float r44=((buffer2[(48u)*1+0])-(in.world.x));
  float r45=((buffer2[(49u)*1+0])-(in.world.y));
  float r46=((buffer2[(50u)*1+0])-(in.world.z));
  float r50=rsqrt(fabs((fma(r46,r46,(fma(r45,r45,((r44*r44))))))));
  float r51=(r44*r50);
  float r52=(r45*r50);
  float r53=(r46*r50);
  long r56=long((buffer2[(73u)*1+0]));
  long r59=(((r56<long(r57)?1.0f:0.0f))!=0.0f?r56:long(r57));
  bool over82=r59>long(r81) && ulong(r59-long(r81))>4096uL;
  sb|=over82?1u:0u;
  r83=(fma((buffer2[(52u)*1+0]),(buffer2[(69u)*1+0]),(buffer2[r57*1+0])));
  r84=(fma((buffer2[(53u)*1+0]),(buffer2[(70u)*1+0]),(buffer2[(65u)*1+0])));
  r85=(fma((buffer2[(54u)*1+0]),(buffer2[(71u)*1+0]),(buffer2[(66u)*1+0])));
  r86=r80;
  r87=r80;
  r88=r80;
  for(r82=long(r81);r82<(over82?long(r81):r59);r82++){
  uint r89=20u,r94=8u,r97=1u,r102=2u;
  float r107=as_type<float>(0xbf800000u),r115=0.0f,r116=0.0f,r117=0.0f,r119=0.0f,r127=as_type<float>(0x0u),r128=0.0f,r129=0.0f,r130=0.0f,r131=0.0f,r132=as_type<float>(0x0u),r133=0.0f;
  float r134=0.0f,r135=0.0f,r136=as_type<float>(0x3fc00000u);
  uint r139=9u,r142=2u,r146=1u;
  float r152=0.0f,r153=0.0f,r156=as_type<float>(0xbf800000u),r176=0.0f;
  uint r177=13u;
  float r180=0.0f;
  uint r184=14u;
  float r187=0.0f,r189=0.0f,r190=0.0f;
  uint r191=9u,r194=1u,r197=2u;
  float r209=0.0f,r210=as_type<float>(0x0u),r212=as_type<float>(0x0u),r217=as_type<float>(0xbf800000u);
  uint r243=14u;
  float r253=as_type<float>(0x0u);
  uint r254=0u;
  int r255=0;
  float r256=0,r257=0,r258=0,r259=0,r346=0.0f,r351=0.0f,r356=0.0f,r357=0.0f,r358=0.0f,r359=as_type<float>(0x0u),r360=0.0f,r361=0.0f;
  float r362=0.0f,r363=0.0f,r364=0.0f,r365=0.0f,r366=0.0f;
  uint r371=4u;
  float r381=as_type<float>(0x0u),r424=0.0f,r425=0.0f,r426=0.0f;
  uint r90=(uint(r82)*r89);
  uint r92=((84u)+r90);
  float r93=buffer2[r92*1+0];
  float r96=buffer2[((r92+r94))*1+0];
  uint r98=(r92+r97);
  float r99=buffer2[r98*1+0];
  float r101=buffer2[((r98+r97))*1+0];
  float r104=buffer2[((r98+r102))*1+0];
  float r106=(r93<(as_type<float>(0x3f000000u)));
  if(r106!=0.0f) {
    float r108=(r99*r107);
    float r109=(r101*r107);
    float r110=(r104*r107);
    float r114=rsqrt(fabs((fma(r110,r110,(fma(r109,r109,((r108*r108))))))));
    r115=(r108*r114);
    r116=(r109*r114);
    r117=(r110*r114);
  }
  if(r106==0.0f) {
    r119=(r93<(as_type<float>(0x40200000u)));
    if(r119!=0.0f) {
      float r120=(r99-(in.world.x));
      float r121=(r101-(in.world.y));
      float r122=(r104-(in.world.z));
      float r126=sqrt(fabs((fma(r122,r122,(fma(r121,r121,((r120*r120))))))));
      r128=(r126>r127);
      if(r128!=0.0f) {
        r129=(r126==0.0f?0.0f:r120/r126);
        r130=(r126==0.0f?0.0f:r121/r126);
        r131=(r126==0.0f?0.0f:r122/r126);
      }
      r133=(r128!=0.0f?r129:r132);
      r134=(r128!=0.0f?r130:r132);
      r135=(r128!=0.0f?r131:r132);
      uint r140=(((r93<r136))!=0.0f?r139:(15u));
      uint r141=(r92+r140);
      float r151=fma((((buffer2[((r141+r142))*1+0])*r126)),r126,(fma((buffer2[((r141+r146))*1+0]),r126,(buffer2[r141*1+0]))));
      r152=(r151>r127);
      if(r152!=0.0f) {
        r153=(r151==0.0f?0.0f:r96/r151);
      }
      float r155=(r152!=0.0f?r153:(as_type<float>(0x0u)));
      uint r160=(r92+r139);
      float r161=buffer2[r160*1+0];
      float r163=buffer2[((r160+r146))*1+0];
      float r165=buffer2[((r160+r142))*1+0];
      float r169=rsqrt(fabs((fma(r165,r165,(fma(r163,r163,((r161*r161))))))));
      float r175=fma(((r135*r156)),((r165*r169)),(fma(((r134*r156)),((r163*r169)),((((r133*r156))*((r161*r169)))))));
      r176=(r93>=r136);
      if(r176!=0.0f) {
        r180=(r175<(buffer2[((r92+r177))*1+0]));
        if(r180==0.0f) {
          r187=pow(fabs((max(r175,(as_type<float>(0x0u))))),(buffer2[((r92+r184))*1+0]));
        }
        r189=(r155*((r180!=0.0f?(as_type<float>(0x0u)):r187)));
      }
      r190=(r176!=0.0f?r189:r155);
    }
    if(r119==0.0f) {
      uint r192=(r92+r191);
      float r193=buffer2[r192*1+0];
      float r196=buffer2[((r192+r194))*1+0];
      float r199=buffer2[((r192+r197))*1+0];
      float r203=rsqrt(fabs((fma(r199,r199,(fma(r196,r196,((r193*r193))))))));
      float r204=(r193*r203);
      float r205=(r196*r203);
      float r206=(r199*r203);
      r209=((fabs(r206))<(as_type<float>(0x3f7fbe77u)));
      float r214=(r209!=0.0f?r210:r212);
      float r215=(r209!=0.0f?r210:(as_type<float>(0x3f800000u)));
      float r216=(r209!=0.0f?(as_type<float>(0x3f800000u)):r212);
      float r220=fma(r215,r206,((r217*((r216*r205)))));
      float r223=fma(r216,r204,((r217*((r214*r206)))));
      float r226=fma(r214,r205,((r217*((r215*r204)))));
      float r230=rsqrt(fabs((fma(r226,r226,(fma(r223,r223,((r220*r220))))))));
      float r231=(r220*r230);
      float r232=(r223*r230);
      float r233=(r226*r230);
      float r236=fma(r205,r233,((r217*((r206*r232)))));
      float r239=fma(r206,r231,((r217*((r204*r233)))));
      float r242=fma(r204,r232,((r217*((r205*r231)))));
      long r247=long(round((sqrt(fabs((buffer2[((r92+r243))*1+0]))))));
      long r248=r247;
      long r250=(((r248<long(r194)?1.0f:0.0f))!=0.0f?long(r194):r248);
      long r251=(as_type<long>(ulong((ulong(r250)*ulong(r250)))<<1)>>1);
      float r252=float(r251);
      bool over255=r250>long(r254) && ulong(r250-long(r254))>4096uL;
      sb|=over255?1u:0u;
      r256=r253;
      r257=r253;
      r258=r253;
      r259=r253;
      for(r255=long(r254);r255<(over255?long(r254):r250);r255++){
  uint r260=0u;
  int r261=0;
  float r262=0,r263=0,r264=0,r265=0,r338=0.0f,r339=0.0f,r340=0.0f,r341=0.0f;
  if(r106==0.0f) {
    if(r119==0.0f) {
      bool over261=r250>long(r260) && ulong(r250-long(r260))>4096uL;
      sb|=over261?1u:0u;
      r262=r256;
      r263=r257;
      r264=r258;
      r265=r259;
      for(r261=long(r260);r261<(over261?long(r260):r250);r261++){
  float r267=as_type<float>(0x3f000000u);
  uint r276=12u,r280=13u;
  float r298=0.0f,r302=as_type<float>(0xbf800000u),r309=as_type<float>(0x0u);
  uint r311=15u,r313=2u,r317=1u;
  float r323=0.0f,r326=0.0f,r329=0.0f,r330=0.0f,r331=0.0f,r332=0.0f,r333=0.0f,r334=0.0f,r335=0.0f,r336=0.0f;
  if(r106==0.0f) {
    if(r119==0.0f) {
      float r269=float(r250);
      float r279=(((((r269==0.0f?0.0f:(((float(r261))+r267))/r269))-r267))*(buffer2[((r92+r276))*1+0]));
      float r283=(((((r269==0.0f?0.0f:(((float(r255))+r267))/r269))-r267))*(buffer2[((r92+r280))*1+0]));
      float r290=((fma(r283,r236,(fma(r279,r231,r99))))-(in.world.x));
      float r291=((fma(r283,r239,(fma(r279,r232,r101))))-(in.world.y));
      float r292=((fma(r283,r242,(fma(r279,r233,r104))))-(in.world.z));
      float r296=sqrt(fabs((fma(r292,r292,(fma(r291,r291,((r290*r290))))))));
      r298=(r296>(as_type<float>(0x0u)));
      if(r298!=0.0f) {
        float r299=(r296==0.0f?0.0f:r290/r296);
        float r300=(r296==0.0f?0.0f:r291/r296);
        float r301=(r296==0.0f?0.0f:r292/r296);
        uint r312=(r92+r311);
        float r322=fma((((buffer2[((r312+r313))*1+0])*r296)),r296,(fma((buffer2[((r312+r317))*1+0]),r296,(buffer2[r312*1+0]))));
        r323=(r322>r309);
        if(r323!=0.0f) {
          r326=(((r252*r322))==0.0f?0.0f:((r96*(max((fma(r206,((r301*r302)),(fma(r205,((r300*r302)),((r204*((r299*r302)))))))),r309))))/((r252*r322)));
        }
        float r328=(r323!=0.0f?r326:(as_type<float>(0x0u)));
        r329=fma(r299,r328,r262);
        r330=fma(r300,r328,r263);
        r331=fma(r301,r328,r264);
        r332=(r265+r328);
      }
      r333=(r298!=0.0f?r329:r262);
      r334=(r298!=0.0f?r330:r263);
      r335=(r298!=0.0f?r331:r264);
      r336=(r298!=0.0f?r332:r265);
    }
  }
  float next261_0=r333;
  float next261_1=r334;
  float next261_2=r335;
  float next261_3=r336;
  r262=next261_0;
  r263=next261_1;
  r264=next261_2;
  r265=next261_3;
      }
      r338=r262;
      r339=r263;
      r340=r264;
      r341=r265;
    }
  }
  float next255_0=r338;
  float next255_1=r339;
  float next255_2=r340;
  float next255_3=r341;
  r256=next255_0;
  r257=next255_1;
  r258=next255_2;
  r259=next255_3;
      }
      float r343=r256;
      float r344=r257;
      float r345=r258;
      r346=r259;
      r351=((sqrt(fabs((fma(r345,r345,(fma(r344,r344,((r343*r343)))))))))>r253);
      if(r351!=0.0f) {
        float r355=rsqrt(fabs((fma(r345,r345,(fma(r344,r344,((r343*r343))))))));
        r356=(r343*r355);
        r357=(r344*r355);
        r358=(r345*r355);
      }
      r360=(r351!=0.0f?r356:r359);
      r361=(r351!=0.0f?r357:r359);
      r362=(r351!=0.0f?r358:r359);
    }
    r363=(r119!=0.0f?r133:r360);
    r364=(r119!=0.0f?r134:r361);
    r365=(r119!=0.0f?r135:r362);
    r366=(r119!=0.0f?r190:r346);
  }
  float r367=(r106!=0.0f?r115:r363);
  float r368=(r106!=0.0f?r116:r364);
  float r369=(r106!=0.0f?r117:r365);
  float r370=(r106!=0.0f?r96:r366);
  uint r372=(r92+r371);
  float r373=buffer2[r372*1+0];
  float r375=buffer2[((r372+r97))*1+0];
  float r377=buffer2[((r372+r102))*1+0];
  float r383=((max((fma(r37,r369,(fma(r36,r368,((r35*r367)))))),r381))*r370);
  float r384=(r367+r51);
  float r385=(r368+r52);
  float r386=(r369+r53);
  float r390=rsqrt(fabs((fma(r386,r386,(fma(r385,r385,((r384*r384))))))));
  float r401=((pow(fabs((max((fma(r37,((r386*r390)),(fma(r36,((r385*r390)),((r35*((r384*r390)))))))),r381))),(buffer2[(68u)*1+0])))*r370);
  float r414=(r383>r381);
  if(r414!=0.0f) {
    r424=fma((((buffer2[(60u)*1+0])*r373)),r401,r86);
    r425=fma((((buffer2[(61u)*1+0])*r375)),r401,r87);
    r426=fma((((buffer2[(62u)*1+0])*r377)),r401,r88);
  }
  float next82_0=(fma((((buffer2[(56u)*1+0])*r373)),r383,r83));
  float next82_1=(fma((((buffer2[(57u)*1+0])*r375)),r383,r84));
  float next82_2=(fma((((buffer2[(58u)*1+0])*r377)),r383,r85));
  float next82_3=((r414!=0.0f?r424:r86));
  float next82_4=((r414!=0.0f?r425:r87));
  float next82_5=((r414!=0.0f?r426:r88));
  r83=next82_0;
  r84=next82_1;
  r85=next82_2;
  r86=next82_3;
  r87=next82_4;
  r88=next82_5;
  }
  float r431=r83;
  float r432=r84;
  float r433=r85;
  float r434=r86;
  float r435=r87;
  float r436=r88;
  float r437=((in.color.x)*(texture6.sample(sampler7,float2((in.uv.x),(in.uv.y))).x));
  float r438=((in.color.y)*(texture6.sample(sampler7,float2((in.uv.x),(in.uv.y))).y));
  float r439=((in.color.z)*(texture6.sample(sampler7,float2((in.uv.x),(in.uv.y))).z));
  float r442=((buffer2[(75u)*1+0])>r26);
  if(r442!=0.0f) {
    r443=fma(r431,r437,r434);
    r444=fma(r432,r438,r435);
    r445=fma(r433,r439,r436);
  }
  if(r442==0.0f) {
    r449=(((r431+r434))*r437);
    r450=(((r432+r435))*r438);
    r451=(((r433+r436))*r439);
  }
  float r452=(r442!=0.0f?r443:r449);
  float r453=(r442!=0.0f?r444:r450);
  float r454=(r442!=0.0f?r445:r451);
  float r458=((((buffer2[(59u)*1+0])*(in.color.w)))*(texture6.sample(sampler7,float2((in.uv.x),(in.uv.y))).w));
  colour.x=r452;
  colour.y=r453;
  colour.z=r454;
  colour.w=r458;
  return colour;
}
