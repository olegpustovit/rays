#include <metal_stdlib>
using namespace metal;
struct marks_vertex_out {
  float4 position [[position]];
  float2 local;
  float4 color [[flat]];
  float4 color2 [[flat]];
  float4 shape [[flat]];
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
  uint r8=2u,r10=3u,r12=1u,r23=0u,r32=5u;
  long r42=0;
  float r45=as_type<float>(0x0u),r47=as_type<float>(0x3f800000u),r49=as_type<float>(0x437f0000u),r51=as_type<float>(0x3f000000u);
  uint r54=24u,r56=1u,r65=16u,r68=2u,r77=8u,r79=255u;
  long r81=0;
  float r83=as_type<float>(0x40000000u),r84=as_type<float>(0x0u),r85=as_type<float>(0x3f800000u);
  uint r88=1u;
  float r98=0.0f,r99=0.0f,r100=0.0f,r103=0.0f,r104=0.0f,r105=as_type<float>(0x0u),r107=as_type<float>(0x3f000000u),r108=0.0f,r141=0.0f,r151=0.0f,r154=0.0f,r155=0.0f;
  float r156=0.0f,r157=0.0f,r162=0.0f,r163=0.0f;
  uint r164=24u,r166=255u;
  float r169=as_type<float>(0x437f0000u),r170=0.0f;
  uint r171=16u;
  float r175=0.0f;
  uint r176=8u;
  float r180=0.0f,r183=0.0f,r184=0.0f,r187=0.0f,r189=0.0f,r190=0.0f,r191=0.0f,r192=0.0f,r194=0.0f;
  uint r195=1u;
  float r198=0.0f;
  uint r203=0u;
  float r204=0.0f,r206=0.0f,r208=as_type<float>(0x3f000000u),r222=0.0f,r224=0.0f,r225=as_type<float>(0x0u),r230=0.0f,r240=0.0f,r250=0.0f,r253=0.0f,r254=0.0f,r255=0.0f;
  float r256=0.0f,r261=0.0f,r262=0.0f,r268=0.0f;
  long r269=0;
  float r272=as_type<float>(0x0u),r274=as_type<float>(0x3f800000u),r276=as_type<float>(0x437f0000u),r278=as_type<float>(0x3f000000u);
  uint r281=24u,r283=1u,r292=16u,r295=2u,r304=8u,r306=255u;
  long r308=0;
  uint r310=24u,r312=255u;
  float r315=as_type<float>(0x437f0000u),r316=0.0f;
  uint r317=16u;
  float r321=0.0f;
  uint r322=8u;
  float r326=0.0f,r329=0.0f,r333=0.0f,r337=0.0f,r341=0.0f,r344=0.0f,r345=0.0f;
  long r0=long(vid);
  long r9=as_type<long>(ulong(r0)>>(long(r8)&63));
  sb|=(r8>63u)?4u:0u;
  uint r11=(uint(r0)&r10);
  float r15=(((long(r11)==long(r12)?1.0f:0.0f))!=0.0f||((long(r11)==long(r8)?1.0f:0.0f))!=0.0f);
  float r18=(r15!=0.0f?(as_type<float>(0x3f800000u)):(as_type<float>(0x0u)));
  float r19=(long(r11)>=long(r8)?1.0f:0.0f);
  float r22=(r19!=0.0f?(as_type<float>(0x3f800000u)):(as_type<float>(0x0u)));
  long r24=long(buffer5[r23*1+0]);
  long r25=long(buffer5[r12*1+0]);
  uint r26=(uint(r9)*uint(r25));
  long r27=long(buffer5[r8*1+0]);
  uint r28=(uint(r9)*uint(r27));
  float r29=buffer0[r26*1+0];
  float r31=buffer0[((r26+r12))*1+0];
  long r33=long(buffer5[r32*1+0]);
  float r35=buffer4[((uint(r9)*uint(r33)))*1+0];
  long r37=long(buffer5[(4u)*1+0]);
  uint r38=(uint(r9)*uint(r37));
  float r41=((long(buffer5[(7u)*1+0]))==long(r23)?1.0f:0.0f);
  if(r41!=0.0f) {
    r42=long(buffer3[r38*1+0]);
  }
  if(r41==0.0f) {
    long r55=as_type<long>(ulong((long(floor((((((min((max((as_type<float>(uint((long(buffer3[r38*1+0]))))),r45)),r47))*r49))+r51))))))<<(long(r54)&63));
    sb|=(r54>63u)?4u:0u;
    long r66=as_type<long>(ulong((long(floor((((((min((max((as_type<float>(uint((long(buffer3[((r38+r56))*1+0]))))),r45)),r47))*r49))+r51))))))<<(long(r65)&63));
    sb|=(r65>63u)?4u:0u;
    long r78=as_type<long>(ulong((long(floor((((((min((max((as_type<float>(uint((long(buffer3[((r38+r68))*1+0]))))),r45)),r47))*r49))+r51))))))<<(long(r77)&63));
    sb|=(r77>63u)?4u:0u;
    long r80=(r78|long(r79));
    r81=(((r55|r66))|r80);
  }
  long r82=(r41!=0.0f?r42:r81);
  float r86=(r24==long(r32)?1.0f:0.0f);
  if(r86!=0.0f) {
    float r87=buffer1[r28*1+0];
    float r90=buffer1[((r28+r88))*1+0];
    float r91=(r87-r29);
    float r92=(r90-r31);
    float r96=sqrt(fabs(((((r91*r91))+((r92*r92))))));
    r98=(r96>(as_type<float>(0x358637bdu)));
    if(r98!=0.0f) {
      r99=(r96==0.0f?0.0f:r91/r96);
      r100=(r96==0.0f?0.0f:r92/r96);
    }
    r103=(r98!=0.0f?r99:(as_type<float>(0x3f800000u)));
    r104=(r98!=0.0f?r100:(as_type<float>(0x0u)));
    r108=(r35*r107);
    float r110=(r108+(as_type<float>(0x3fc00000u)));
    float r111=(r103*r110);
    float r112=(r104*r110);
    float r113=(r29-r111);
    float r114=(r31-r112);
    float r127=(((((r22*(as_type<float>(0x40000000u))))-(as_type<float>(0x3f800000u))))*r110);
    float r130=(((r113+((((((r87+r111))-r113))*r18))))+((((r105-r104))*r127)));
    float r131=(((r114+((((((r90+r112))-r114))*r18))))+((r103*r127)));
    if(r98!=0.0f) {
      r141=((((((buffer6[(0u)*1+0])*r130))+(((buffer6[(1u)*1+0])*r131))))+(buffer6[(2u)*1+0]));
      r151=((((((buffer6[(3u)*1+0])*r130))+(((buffer6[(4u)*1+0])*r131))))+(buffer6[(5u)*1+0]));
    }
    r154=(r98!=0.0f?r141:r83);
    r155=(r98!=0.0f?r151:r83);
    r156=(r98!=0.0f?(as_type<float>(0x0u)):r84);
    r157=(r98!=0.0f?(as_type<float>(0x3f800000u)):r85);
    r162=(r130-((((r29+r87))*r107)));
    r163=(r131-((((r31+r90))*r107)));
    long r165=as_type<long>(ulong(r82)>>(long(r164)&63));
    sb|=(r164>63u)?4u:0u;
    r170=(r169==0.0f?0.0f:(float(((uint(r165)&r166))))/r169);
    long r172=as_type<long>(ulong(r82)>>(long(r171)&63));
    sb|=(r171>63u)?4u:0u;
    r175=(r169==0.0f?0.0f:(float(((uint(r172)&r166))))/r169);
    long r177=as_type<long>(ulong(r82)>>(long(r176)&63));
    sb|=(r176>63u)?4u:0u;
    r180=(r169==0.0f?0.0f:(float(((uint(r177)&r166))))/r169);
    r183=(r169==0.0f?0.0f:(float(((uint(r82)&r166))))/r169);
    r184=(r96*r107);
  }
  if(r86==0.0f) {
    r187=(r24==long((4u))?1.0f:0.0f);
    if(r187!=0.0f) {
      float r188=buffer1[r28*1+0];
      r189=(r29-r188);
      r190=(r31-r188);
      r191=(r29+r188);
      r192=(r31+r188);
    }
    if(r187==0.0f) {
      r194=(r29+(buffer1[r28*1+0]));
      r198=(r31+(buffer1[((r28+r195))*1+0]));
    }
    float r199=(r187!=0.0f?r189:r29);
    float r200=(r187!=0.0f?r190:r31);
    float r201=(r187!=0.0f?r191:r194);
    float r202=(r187!=0.0f?r192:r198);
    r204=(r82==long(r203)?1.0f:0.0f);
    r206=(r204!=0.0f?(as_type<float>(0x0u)):r35);
    float r210=((as_type<float>(0x3f800000u))+((r206*r208)));
    float r211=(r199-r210);
    float r212=(r200-r210);
    float r217=(r211+((((((r201+r210))-r211))*r18)));
    float r220=(r212+((((((r202+r210))-r212))*r22)));
    r222=(((r201-r199))*r208);
    r224=(((r202-r200))*r208);
    r230=(((r206>r225))!=0.0f||((((r222>r225))!=0.0f&&((r224>r225))!=0.0f))!=0.0f);
    if(r230!=0.0f) {
      r240=((((((buffer6[(0u)*1+0])*r217))+(((buffer6[(1u)*1+0])*r220))))+(buffer6[(2u)*1+0]));
      r250=((((((buffer6[(3u)*1+0])*r217))+(((buffer6[(4u)*1+0])*r220))))+(buffer6[(5u)*1+0]));
    }
    r253=(r230!=0.0f?r240:r83);
    r254=(r230!=0.0f?r250:r83);
    r255=(r230!=0.0f?(as_type<float>(0x0u)):r84);
    r256=(r230!=0.0f?(as_type<float>(0x3f800000u)):r85);
    r261=(r217-((((r199+r201))*r208)));
    r262=(r220-((((r200+r202))*r208)));
    long r264=long(buffer5[(3u)*1+0]);
    uint r265=(uint(r9)*uint(r264));
    r268=((long(buffer5[(6u)*1+0]))==long(r203)?1.0f:0.0f);
    if(r268!=0.0f) {
      r269=long(buffer2[r265*1+0]);
    }
    if(r268==0.0f) {
      long r282=as_type<long>(ulong((long(floor((((((min((max((as_type<float>(uint((long(buffer2[r265*1+0]))))),r272)),r274))*r276))+r278))))))<<(long(r281)&63));
      sb|=(r281>63u)?4u:0u;
      long r293=as_type<long>(ulong((long(floor((((((min((max((as_type<float>(uint((long(buffer2[((r265+r283))*1+0]))))),r272)),r274))*r276))+r278))))))<<(long(r292)&63));
      sb|=(r292>63u)?4u:0u;
      long r305=as_type<long>(ulong((long(floor((((((min((max((as_type<float>(uint((long(buffer2[((r265+r295))*1+0]))))),r272)),r274))*r276))+r278))))))<<(long(r304)&63));
      sb|=(r304>63u)?4u:0u;
      long r307=(r305|long(r306));
      r308=(((r282|r293))|r307);
    }
    long r309=(r268!=0.0f?r269:r308);
    long r311=as_type<long>(ulong(r309)>>(long(r310)&63));
    sb|=(r310>63u)?4u:0u;
    r316=(r315==0.0f?0.0f:(float(((uint(r311)&r312))))/r315);
    long r318=as_type<long>(ulong(r309)>>(long(r317)&63));
    sb|=(r317>63u)?4u:0u;
    r321=(r315==0.0f?0.0f:(float(((uint(r318)&r312))))/r315);
    long r323=as_type<long>(ulong(r309)>>(long(r322)&63));
    sb|=(r322>63u)?4u:0u;
    r326=(r315==0.0f?0.0f:(float(((uint(r323)&r312))))/r315);
    r329=(r315==0.0f?0.0f:(float(((uint(r309)&r312))))/r315);
    long r330=as_type<long>(ulong(r82)>>(long(r310)&63));
    sb|=(r310>63u)?4u:0u;
    r333=(r315==0.0f?0.0f:(float(((uint(r330)&r312))))/r315);
    long r334=as_type<long>(ulong(r82)>>(long(r317)&63));
    sb|=(r317>63u)?4u:0u;
    r337=(r315==0.0f?0.0f:(float(((uint(r334)&r312))))/r315);
    long r338=as_type<long>(ulong(r82)>>(long(r322)&63));
    sb|=(r322>63u)?4u:0u;
    r341=(r315==0.0f?0.0f:(float(((uint(r338)&r312))))/r315);
    r344=(r315==0.0f?0.0f:(float(((uint(r82)&r312))))/r315);
    r345=float(r24);
  }
  float r346=(r86!=0.0f?r154:r253);
  float r347=(r86!=0.0f?r155:r254);
  float r348=(r86!=0.0f?r156:r255);
  float r349=(r86!=0.0f?r157:r256);
  float r350=(r86!=0.0f?r162:r261);
  float r351=(r86!=0.0f?r163:r262);
  float r352=(r86!=0.0f?r170:r316);
  float r353=(r86!=0.0f?r175:r321);
  float r354=(r86!=0.0f?r180:r326);
  float r355=(r86!=0.0f?r183:r329);
  float r356=(r86!=0.0f?r103:r333);
  float r357=(r86!=0.0f?r104:r337);
  float r358=(r86!=0.0f?r105:r341);
  float r359=(r86!=0.0f?r105:r344);
  float r360=(r86!=0.0f?r184:r222);
  float r361=(r86!=0.0f?r108:r224);
  float r362=(r86!=0.0f?r105:r206);
  float r363=(r86!=0.0f?(as_type<float>(0x40a00000u)):r345);
  o.position.x=r346;
  o.position.y=r347;
  o.position.z=r348;
  o.position.w=r349;
  o.local.x=r350;
  o.local.y=r351;
  o.color.x=r352;
  o.color.y=r353;
  o.color.z=r354;
  o.color.w=r355;
  o.color2.x=r356;
  o.color2.y=r357;
  o.color2.z=r358;
  o.color2.w=r359;
  o.shape.x=r360;
  o.shape.y=r361;
  o.shape.z=r362;
  o.shape.w=r363;
  return o;
}
fragment float4 marks_fragment(
    marks_vertex_out in [[stage_in]]) {
  float4 colour;
  uint sb=0u;
  float r22=as_type<float>(0x0u),r53=0.0f,r55=0.0f,r60=0.0f,r61=as_type<float>(0x0u),r77=0.0f,r92=as_type<float>(0x0u),r97=0.0f,r98=as_type<float>(0x3f000000u),r104=as_type<float>(0x0u),r106=as_type<float>(0x3f800000u),r127=0.0f;
  float r128=as_type<float>(0x0u),r129=0.0f,r130=0.0f,r131=0.0f,r132=0.0f,r133=0.0f,r134=0.0f,r135=0.0f,r136=0.0f;
  float r15=((in.shape.w)==(as_type<float>(0x40a00000u)));
  if(r15!=0.0f) {
    float r25=((((fabs((((((in.local.x)*(in.color2.x)))+(((in.local.y)*(in.color2.y)))))))-(in.shape.x)))+r22);
    float r28=((((fabs((((((in.local.y)*(in.color2.x)))-(((in.local.x)*(in.color2.y)))))))-(in.shape.y)))+r22);
    float r29=max(r25,r22);
    float r30=max(r28,r22);
    float r39=fwidth((in.local.x));
    float r40=fwidth((in.local.y));
    r53=((in.color.w)*(min((max((((as_type<float>(0x3f000000u))-(((max((sqrt(fabs(((((r39*r39))+((r40*r40))))))),(as_type<float>(0x38d1b717u))))==0.0f?0.0f:(((((sqrt(fabs(((((r29*r29))+((r30*r30)))))))+(min((max(r25,r28)),r22))))-r22))/(max((sqrt(fabs(((((r39*r39))+((r40*r40))))))),(as_type<float>(0x38d1b717u)))))))),r22)),(as_type<float>(0x3f800000u)))));
  }
  if(r15==0.0f) {
    r55=((in.shape.w)==(as_type<float>(0x40800000u)));
    if(r55!=0.0f) {
      r60=((sqrt(fabs((((((in.local.x)*(in.local.x)))+(((in.local.y)*(in.local.y))))))))-(in.shape.x));
    }
    if(r55==0.0f) {
      float r64=((((fabs((in.local.x)))-(in.shape.x)))+r61);
      float r67=((((fabs((in.local.y)))-(in.shape.y)))+r61);
      float r68=max(r64,r61);
      float r69=max(r67,r61);
      r77=((((sqrt(fabs(((((r68*r68))+((r69*r69)))))))+(min((max(r64,r67)),r61))))-r61);
    }
    float r78=(r55!=0.0f?r60:r77);
    float r79=fwidth((in.local.x));
    float r80=fwidth((in.local.y));
    float r88=max((((sqrt(fabs(((((r79*r79))+((r80*r80)))))))*(as_type<float>(0x3f3504f3u)))),(as_type<float>(0x38d1b717u)));
    float r96=((in.color.w)*(min((max((((as_type<float>(0x3f000000u))-((r88==0.0f?0.0f:r78/r88)))),r92)),(as_type<float>(0x3f800000u)))));
    r97=((in.shape.z)<=r92);
    if(r97==0.0f) {
      float r108=((in.color2.w)*(min((max(((r98-((r88==0.0f?0.0f:(((fabs(r78))-(((in.shape.z)*r98))))/r88)))),r104)),r106)));
      float r109=(r106-r108);
      float r111=(r108+((r96*r109)));
      r127=(r111<=r104);
      r129=(r127!=0.0f?r128:((r111==0.0f?0.0f:(((((in.color2.x)*r108))+(((((in.color.x)*r96))*r109))))/r111)));
      r130=(r127!=0.0f?r128:((r111==0.0f?0.0f:(((((in.color2.y)*r108))+(((((in.color.y)*r96))*r109))))/r111)));
      r131=(r127!=0.0f?r128:((r111==0.0f?0.0f:(((((in.color2.z)*r108))+(((((in.color.z)*r96))*r109))))/r111)));
      r132=(r127!=0.0f?r128:r111);
    }
    r133=(r97!=0.0f?(in.color.x):r129);
    r134=(r97!=0.0f?(in.color.y):r130);
    r135=(r97!=0.0f?(in.color.z):r131);
    r136=(r97!=0.0f?r96:r132);
  }
  float r137=(r15!=0.0f?(in.color.x):r133);
  float r138=(r15!=0.0f?(in.color.y):r134);
  float r139=(r15!=0.0f?(in.color.z):r135);
  float r140=(r15!=0.0f?r53:r136);
  colour.x=r137;
  colour.y=r138;
  colour.z=r139;
  colour.w=r140;
  return colour;
}
