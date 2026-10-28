// Generated from ui.rays by Flow_gpu.Raster (lib/flow_gpu/test_raster.exe --write); do not edit.
#include <metal_stdlib>
using namespace metal;
long fmod64(long a,long b){if(b==0)return 0;long r=a%b;return (r!=0&&((r<0)!=(b<0)))?r+b:r;}
struct ui_vertex_out {
  float4 position [[position]];
  float4 color;
  float4 color2;
  float2 local;
  float2 uv;
  float4 box [[flat]];
  float4 params [[flat]];
  float4 extra [[flat]];
};
vertex ui_vertex_out ui_vertex(
    uint vid [[vertex_id]],
    device const float* buffer0 [[buffer(0)]],
    device const uint* buffer1 [[buffer(1)]],
    device const float* buffer2 [[buffer(2)]]) {
  ui_vertex_out o;
  uint sb=0u;
  uint r4=4u,r6=16u,r9=1u,r11=2u,r21=10u,r24=8u,r27=24u,r29=255u;
  float r32=as_type<float>(0x437f0000u);
  uint r45=9u,r63=11u,r66=12u,r69=13u,r75=5u,r78=6u,r81=7u,r87=0u,r90=1u,r93=2u,r96=3u,r99=4u;
  uint r102=5u,r105=6u,r108=7u,r111=14u;
  float r114=as_type<float>(0x3f800000u),r120=as_type<float>(0x40400000u),r138=0.0f,r139=0.0f,r140=0.0f,r141=0.0f;
  uint r142=15u;
  float r167=0.0f,r168=0.0f,r169=0.0f,r170=0.0f,r178=0.0f,r179=0.0f,r180=0.0f,r185=as_type<float>(0x0u);
  uint r187=12u;
  float r213=0.0f,r214=0.0f;
  uint r215=0u;
  float r219=0.0f;
  uint r220=13u;
  float r224=0.0f;
  uint r226=12u;
  float r231=0.0f;
  uint r232=12u;
  float r236=0.0f;
  uint r237=12u;
  float r241=0.0f,r243=0.0f,r244=0.0f,r248=0.0f;
  uint r250=1u;
  float r252=0.0f;
  uint r254=2u;
  float r256=0.0f;
  uint r258=3u;
  float r260=0.0f,r266=0.0f,r267=0.0f,r268=0.0f;
  uint r269=4u,r272=5u,r275=6u,r278=7u;
  float r285=0.0f,r286=0.0f,r287=as_type<float>(0x0u),r288=0.0f,r289=0.0f,r318=as_type<float>(0x0u),r319=as_type<float>(0x3f800000u);
  long r0=long(vid);
  long r5=(as_type<long>(ulong((long(r4)==0?0:r0/long(r4)))<<1)>>1);
  sb|=(r4==0)?2u:0u;
  uint r7=(uint(r5)*r6);
  long r8=(as_type<long>(ulong((long(r4)==0?0:r0%long(r4)))<<1)>>1);
  sb|=(r4==0)?2u:0u;
  float r13=(((r8==long(r9)?1.0f:0.0f))!=0.0f||((r8==long(r11)?1.0f:0.0f))!=0.0f);
  float r16=(r13!=0.0f?(as_type<float>(0x3f800000u)):(as_type<float>(0x0u)));
  float r17=(r8>=long(r11)?1.0f:0.0f);
  float r20=(r17!=0.0f?(as_type<float>(0x3f800000u)):(as_type<float>(0x0u)));
  long r23=long(buffer1[((r7+r21))*1+0]);
  long r26=long(buffer1[((r7+r24))*1+0]);
  long r28=(as_type<long>(ulong(long((ulong(r26)&0x7fffffffffffffffUL)>>(long(r27)&63)))<<1)>>1);
  sb|=(r27>62u)?4u:0u;
  float r33=(r32==0.0f?0.0f:(float(((uint(r28)&r29))))/r32);
  long r34=(as_type<long>(ulong(long((ulong(r26)&0x7fffffffffffffffUL)>>(long(r6)&63)))<<1)>>1);
  sb|=(r6>62u)?4u:0u;
  float r37=(r32==0.0f?0.0f:(float(((uint(r34)&r29))))/r32);
  long r38=(as_type<long>(ulong(long((ulong(r26)&0x7fffffffffffffffUL)>>(long(r24)&63)))<<1)>>1);
  sb|=(r24>62u)?4u:0u;
  float r41=(r32==0.0f?0.0f:(float(((uint(r38)&r29))))/r32);
  float r44=(r32==0.0f?0.0f:(float(((uint(r26)&r29))))/r32);
  long r47=long(buffer1[((r7+r45))*1+0]);
  long r48=(as_type<long>(ulong(long((ulong(r47)&0x7fffffffffffffffUL)>>(long(r27)&63)))<<1)>>1);
  sb|=(r27>62u)?4u:0u;
  float r51=(r32==0.0f?0.0f:(float(((uint(r48)&r29))))/r32);
  long r52=(as_type<long>(ulong(long((ulong(r47)&0x7fffffffffffffffUL)>>(long(r6)&63)))<<1)>>1);
  sb|=(r6>62u)?4u:0u;
  float r55=(r32==0.0f?0.0f:(float(((uint(r52)&r29))))/r32);
  long r56=(as_type<long>(ulong(long((ulong(r47)&0x7fffffffffffffffUL)>>(long(r24)&63)))<<1)>>1);
  sb|=(r24>62u)?4u:0u;
  float r59=(r32==0.0f?0.0f:(float(((uint(r56)&r29))))/r32);
  float r62=(r32==0.0f?0.0f:(float(((uint(r47)&r29))))/r32);
  float r65=buffer0[((r7+r63))*1+0];
  float r68=buffer0[((r7+r66))*1+0];
  float r71=buffer0[((r7+r69))*1+0];
  float r72=float(r23);
  float r84=(r23==long(r75)?1.0f:0.0f);
  float r86=(((r23==long(r11)?1.0f:0.0f))!=0.0f||r84!=0.0f);
  if(r86!=0.0f) {
    float r89=buffer0[((r7+r87))*1+0];
    float r92=buffer0[((r7+r90))*1+0];
    float r95=buffer0[((r7+r93))*1+0];
    float r98=buffer0[((r7+r96))*1+0];
    float r101=buffer0[((r7+r99))*1+0];
    float r104=buffer0[((r7+r102))*1+0];
    float r107=buffer0[((r7+r105))*1+0];
    float r110=buffer0[((r7+r108))*1+0];
    if(r84==0.0f) {
      float r113=buffer0[((r7+r111))*1+0];
      float r115=(r114-r113);
      float r117=(((r115*r115))*r115);
      float r121=(r120*r115);
      float r123=(((r121*r115))*r113);
      float r129=(((r121*r113))*r113);
      float r135=(((r113*r113))*r113);
      r138=(((((((r117*r89))+((r123*r95))))+((r129*r101))))+((r135*r107)));
      r139=(((((((r117*r92))+((r123*r98))))+((r129*r104))))+((r135*r110)));
    }
    r140=(r84!=0.0f?r89:r138);
    r141=(r84!=0.0f?r92:r139);
    if(r84==0.0f) {
      float r144=buffer0[((r7+r142))*1+0];
      float r145=(r114-r144);
      float r147=(((r145*r145))*r145);
      float r150=(r120*r145);
      float r152=(((r150*r145))*r144);
      float r158=(((r150*r144))*r144);
      float r164=(((r144*r144))*r144);
      r167=(((((((r147*r89))+((r152*r95))))+((r158*r101))))+((r164*r107)));
      r168=(((((((r147*r92))+((r152*r98))))+((r158*r104))))+((r164*r110)));
    }
    r169=(r84!=0.0f?r95:r167);
    r170=(r84!=0.0f?r98:r168);
    float r171=(r169-r140);
    float r172=(r170-r141);
    float r176=sqrt(fabs(((((r171*r171))+((r172*r172))))));
    r178=(r176>(as_type<float>(0x358637bdu)));
    if(r178!=0.0f) {
      r179=(r176==0.0f?0.0f:r171/r176);
      r180=(r176==0.0f?0.0f:r172/r176);
    }
    float r183=(r178!=0.0f?r179:(as_type<float>(0x3f800000u)));
    float r184=(r178!=0.0f?r180:(as_type<float>(0x0u)));
    float r193=((((buffer0[((r7+r187))*1+0])*(as_type<float>(0x3f000000u))))+(as_type<float>(0x3fc00000u)));
    float r194=(r183*r193);
    float r195=(r184*r193);
    float r196=(r140-r194);
    float r197=(r141-r195);
    float r210=(((((r20*(as_type<float>(0x40000000u))))-(as_type<float>(0x3f800000u))))*r193);
    r213=(((r196+((((((r169+r194))-r196))*r16))))+((((r185-r184))*r210)));
    r214=(((r197+((((((r170+r195))-r197))*r16))))+((r183*r210)));
  }
  if(r86==0.0f) {
    r219=(((r23==long(r215)?1.0f:0.0f))!=0.0f||((r23==long((4u))?1.0f:0.0f))!=0.0f);
    if(r219!=0.0f) {
      r224=((buffer0[((r7+r220))*1+0])>(as_type<float>(0x0u)));
      if(r224!=0.0f) {
        r231=((as_type<float>(0x3f800000u))+(((buffer0[((r7+r226))*1+0])*(as_type<float>(0x3f000000u)))));
      }
      if(r224==0.0f) {
        r236=((buffer0[((r7+r232))*1+0])>(as_type<float>(0x0u)));
        if(r236!=0.0f) {
          r241=((buffer0[((r7+r237))*1+0])*(as_type<float>(0x3f000000u)));
        }
        r243=(r236!=0.0f?r241:(as_type<float>(0x0u)));
      }
      r244=(r224!=0.0f?r231:r243);
    }
    float r246=(r219!=0.0f?r244:(as_type<float>(0x0u)));
    r248=buffer0[((r7+r215))*1+0];
    float r249=(r248-r246);
    r252=buffer0[((r7+r250))*1+0];
    float r253=(r252-r246);
    r256=buffer0[((r7+r254))*1+0];
    r260=buffer0[((r7+r258))*1+0];
    r266=(r249+((((((r256+r246))-r249))*r16)));
    r267=(r253+((((((r260+r246))-r253))*r20)));
    r268=(r23==long(r250)?1.0f:0.0f);
    if(r268!=0.0f) {
      float r271=buffer0[((r7+r269))*1+0];
      float r274=buffer0[((r7+r272))*1+0];
      r285=(r271+(((((buffer0[((r7+r275))*1+0])-r271))*r16)));
      r286=(r274+(((((buffer0[((r7+r278))*1+0])-r274))*r20)));
    }
    r288=(r268!=0.0f?r285:r287);
    r289=(r268!=0.0f?r286:r287);
  }
  float r290=(r86!=0.0f?r213:r266);
  float r291=(r86!=0.0f?r214:r267);
  float r292=(r86!=0.0f?r185:r288);
  float r293=(r86!=0.0f?r185:r289);
  float r294=(r86!=0.0f?r185:r248);
  float r295=(r86!=0.0f?r185:r252);
  float r296=(r86!=0.0f?r185:r256);
  float r297=(r86!=0.0f?r185:r260);
  float r298=(r86!=0.0f?r140:(buffer0[((r7+r4))*1+0]));
  float r299=(r86!=0.0f?r141:(buffer0[((r7+r75))*1+0]));
  float r300=(r86!=0.0f?r169:(buffer0[((r7+r78))*1+0]));
  float r301=(r86!=0.0f?r170:(buffer0[((r7+r81))*1+0]));
  float r309=((((((buffer2[(0u)*1+0])*r290))+(((buffer2[r9*1+0])*r291))))+(buffer2[r11*1+0]));
  float r317=((((((buffer2[(3u)*1+0])*r290))+(((buffer2[r4*1+0])*r291))))+(buffer2[r75*1+0]));
  o.position.x=r309;
  o.position.y=r317;
  o.position.z=r318;
  o.position.w=r319;
  o.color.x=r33;
  o.color.y=r37;
  o.color.z=r41;
  o.color.w=r44;
  o.color2.x=r51;
  o.color2.y=r55;
  o.color2.z=r59;
  o.color2.w=r62;
  o.local.x=r290;
  o.local.y=r291;
  o.uv.x=r292;
  o.uv.y=r293;
  o.box.x=r294;
  o.box.y=r295;
  o.box.z=r296;
  o.box.w=r297;
  o.params.x=r65;
  o.params.y=r68;
  o.params.z=r71;
  o.params.w=r72;
  o.extra.x=r298;
  o.extra.y=r299;
  o.extra.z=r300;
  o.extra.w=r301;
  return o;
}
fragment float4 ui_fragment(
    ui_vertex_out in [[stage_in]],
    texture2d<float> texture3 [[texture(3)]],
    sampler sampler4 [[sampler(4)]]) {
  float4 colour;
  uint sb=0u;
  float r35=0.0f,r36=0.0f,r37=0.0f,r38=0.0f,r43=0.0f,r60=as_type<float>(0x0u),r62=as_type<float>(0x3f800000u),r74=as_type<float>(0x3f000000u),r86=0.0f,r90=as_type<float>(0x0u),r95=as_type<float>(0x3f000000u),r113=0.0f;
  float r123=0.0f,r128=0.0f,r130=0.0f,r145=as_type<float>(0x3f000000u),r155=as_type<float>(0x38d1b717u),r161=as_type<float>(0x0u),r163=as_type<float>(0x3f800000u),r171=0.0f,r173=0.0f,r176=as_type<float>(0x3a83126fu),r180=0.0f,r191=0.0f;
  float r193=0.0f,r194=0.0f,r195=0.0f,r196=0.0f,r197=0.0f,r200=as_type<float>(0x3f000000u),r208=0.0f,r215=0.0f,r226=as_type<float>(0x0u),r236=0.0f,r250=as_type<float>(0x0u),r255=0.0f;
  float r256=as_type<float>(0x3f000000u),r262=as_type<float>(0x0u),r264=as_type<float>(0x3f800000u),r285=0.0f,r286=as_type<float>(0x0u),r287=0.0f,r288=0.0f,r289=0.0f,r290=0.0f,r291=0.0f,r292=0.0f,r293=0.0f;
  float r294=0.0f,r295=0.0f,r296=0.0f,r297=0.0f,r298=0.0f,r299=0.0f,r300=0.0f,r301=0.0f,r302=0.0f,r303=0.0f,r304=0.0f,r305=0.0f;
  float r306=0.0f,r307=0.0f,r308=0.0f,r309=0.0f;
  float r25=((in.params.w)==(as_type<float>(0x3f800000u)));
  if(r25!=0.0f) {
    float r29=((float(texture3.get_width()))==0.0f?0.0f:(in.uv.x)/(float(texture3.get_width())));
    float r30=((float(texture3.get_height()))==0.0f?0.0f:(in.uv.y)/(float(texture3.get_height())));
    r35=((in.color.x)*(texture3.sample(sampler4,float2(r29,r30)).x));
    r36=((in.color.y)*(texture3.sample(sampler4,float2(r29,r30)).y));
    r37=((in.color.z)*(texture3.sample(sampler4,float2(r29,r30)).z));
    r38=((in.color.w)*(texture3.sample(sampler4,float2(r29,r30)).w));
  }
  if(r25==0.0f) {
    r43=((((in.params.w)==(as_type<float>(0x40000000u))))!=0.0f||(((in.params.w)==(as_type<float>(0x40a00000u))))!=0.0f);
    if(r43!=0.0f) {
      float r44=((in.extra.z)-(in.extra.x));
      float r45=((in.extra.w)-(in.extra.y));
      float r48=(((r44*r44))+((r45*r45)));
      float r49=sqrt(fabs(r48));
      float r50=fwidth((in.local.x));
      float r51=fwidth((in.local.y));
      float r63=min((max((((max(r48,(as_type<float>(0x2b8cbcccu))))==0.0f?0.0f:(((((((in.local.x)-(in.extra.x)))*r44))+(((((in.local.y)-(in.extra.y)))*r45))))/(max(r48,(as_type<float>(0x2b8cbcccu)))))),r60)),r62);
      float r68=((in.local.x)-(((in.extra.x)+((r44*r63)))));
      float r69=((in.local.y)-(((in.extra.y)+((r45*r63)))));
      float r77=(r49==0.0f?0.0f:r44/r49);
      float r78=(r49==0.0f?0.0f:r45/r49);
      float r83=((in.local.x)-(((((in.extra.x)+(in.extra.z)))*r74)));
      float r84=((in.local.y)-(((((in.extra.y)+(in.extra.w)))*r74)));
      r86=((in.params.w)==(as_type<float>(0x40a00000u)));
      if(r86!=0.0f) {
        float r100=((((fabs(((((r83*r77))+((r84*r78))))))-((r49*r95))))+r90);
        float r103=((((fabs(((((r83*((r90-r78))))+((r84*r77))))))-(((in.params.y)*r95))))+r90);
        float r104=max(r100,r90);
        float r105=max(r103,r90);
        r113=((((sqrt(fabs(((((r104*r104))+((r105*r105)))))))+(min((max(r100,r103)),r90))))-r90);
      }
      r123=(r86!=0.0f&&((r49<=(as_type<float>(0x358637bdu))))!=0.0f);
      r128=((in.color.w)*(min((max(((r74-(((max((sqrt(fabs(((((r50*r50))+((r51*r51))))))),(as_type<float>(0x38d1b717u))))==0.0f?0.0f:((r86!=0.0f?r113:(((sqrt(fabs(((((r68*r68))+((r69*r69)))))))-(((in.params.y)*r74))))))/(max((sqrt(fabs(((((r50*r50))+((r51*r51))))))),(as_type<float>(0x38d1b717u)))))))),r60)),r62)));
    }
    if(r43==0.0f) {
      r130=((in.params.w)==(as_type<float>(0x40400000u)));
      if(r130!=0.0f) {
        float r131=((in.local.x)-(in.extra.x));
        float r132=((in.local.y)-(in.extra.y));
        float r135=((in.extra.z)==0.0f?0.0f:r131/(in.extra.z));
        long r136=long(floor(r135));
        float r138=((in.extra.z)==0.0f?0.0f:r132/(in.extra.z));
        long r139=long(floor(r138));
        float r146=((in.extra.w)*r145);
        float r148=fabs(((((r131-(((floor(r135))*(in.extra.z)))))-r146)));
        float r150=fabs(((((r132-(((floor(r138))*(in.extra.z)))))-r146)));
        r171=((in.color.w)*(((min((max((((((max((fwidth((in.local.x))),r155))==0.0f?0.0f:((r146-(min(r148,(((in.extra.z)-r148))))))/(max((fwidth((in.local.x))),r155))))+r145)),r161)),r163))*(min((max((((((max((fwidth((in.local.y))),r155))==0.0f?0.0f:((r146-(min(r150,(((in.extra.z)-r150))))))/(max((fwidth((in.local.y))),r155))))+r145)),r161)),r163)))));
      }
      if(r130==0.0f) {
        r173=((in.params.z)<=(as_type<float>(0x0u)));
        if(r173!=0.0f) {
          float r175=((in.params.y)*(as_type<float>(0x3f000000u)));
          float r177=((in.local.x)+r176);
          float r178=((in.local.y)+r176);
          r180=((in.params.y)>(as_type<float>(0x0u)));
          if(r180!=0.0f) {
            r191=(((((r177>=(((in.box.x)+r175))))!=0.0f&&((r177<(((in.box.z)-r175))))!=0.0f))!=0.0f&&((((r178>=(((in.box.y)+r175))))!=0.0f&&((r178<(((in.box.w)-r175))))!=0.0f))!=0.0f);
          }
          r193=(r180!=0.0f?r191:(as_type<float>(0x0u)));
          r194=(r180!=0.0f?(in.color2.x):(in.color.x));
          r195=(r180!=0.0f?(in.color2.y):(in.color.y));
          r196=(r180!=0.0f?(in.color2.z):(in.color.z));
          r197=(r180!=0.0f?(in.color2.w):(in.color.w));
        }
        if(r173==0.0f) {
          float r201=((((in.box.x)+(in.box.z)))*r200);
          float r202=((((in.box.y)+(in.box.w)))*r200);
          float r205=((((in.box.z)-(in.box.x)))*r200);
          float r206=((((in.box.w)-(in.box.y)))*r200);
          r208=((in.params.w)==(as_type<float>(0x40800000u)));
          if(r208!=0.0f) {
            float r209=((in.local.x)-r201);
            float r210=((in.local.y)-r202);
            r215=((sqrt(fabs(((((r209*r209))+((r210*r210)))))))-r205);
          }
          if(r208==0.0f) {
            float r219=min((in.params.x),(min(r205,r206)));
            float r222=((((fabs((((in.local.x)-r201))))-r205))+r219);
            float r225=((((fabs((((in.local.y)-r202))))-r206))+r219);
            float r227=max(r222,r226);
            float r228=max(r225,r226);
            r236=((((sqrt(fabs(((((r227*r227))+((r228*r228)))))))+(min((max(r222,r225)),r226))))-r219);
          }
          float r237=(r208!=0.0f?r215:r236);
          float r238=fwidth((in.local.x));
          float r239=fwidth((in.local.y));
          float r247=max((((sqrt(fabs(((((r238*r238))+((r239*r239)))))))*(as_type<float>(0x3f3504f3u)))),(as_type<float>(0x38d1b717u)));
          float r254=((in.color.w)*(min((max(((r200-((r247==0.0f?0.0f:r237/r247)))),r250)),(as_type<float>(0x3f800000u)))));
          r255=((in.params.y)<=r250);
          if(r255==0.0f) {
            float r266=((in.color2.w)*(min((max(((r256-((r247==0.0f?0.0f:(((fabs(r237))-(((in.params.y)*r256))))/r247)))),r262)),r264)));
            float r267=(r264-r266);
            float r269=(r266+((r254*r267)));
            r285=(r269<=r262);
            r287=(r285!=0.0f?r286:((r269==0.0f?0.0f:(((((in.color2.x)*r266))+(((((in.color.x)*r254))*r267))))/r269)));
            r288=(r285!=0.0f?r286:((r269==0.0f?0.0f:(((((in.color2.y)*r266))+(((((in.color.y)*r254))*r267))))/r269)));
            r289=(r285!=0.0f?r286:((r269==0.0f?0.0f:(((((in.color2.z)*r266))+(((((in.color.z)*r254))*r267))))/r269)));
            r290=(r285!=0.0f?r286:r269);
          }
          r291=(r255!=0.0f?(in.color.x):r287);
          r292=(r255!=0.0f?(in.color.y):r288);
          r293=(r255!=0.0f?(in.color.z):r289);
          r294=(r255!=0.0f?r254:r290);
        }
        r295=(r173!=0.0f?r193:r250);
        r296=(r173!=0.0f?r194:r291);
        r297=(r173!=0.0f?r195:r292);
        r298=(r173!=0.0f?r196:r293);
        r299=(r173!=0.0f?r197:r294);
      }
      r300=(r130!=0.0f?r161:r295);
      r301=(r130!=0.0f?(in.color.x):r296);
      r302=(r130!=0.0f?(in.color.y):r297);
      r303=(r130!=0.0f?(in.color.z):r298);
      r304=(r130!=0.0f?r171:r299);
    }
    r305=(r43!=0.0f?r123:r300);
    r306=(r43!=0.0f?(in.color.x):r301);
    r307=(r43!=0.0f?(in.color.y):r302);
    r308=(r43!=0.0f?(in.color.z):r303);
    r309=(r43!=0.0f?r128:r304);
  }
  float r310=(r25!=0.0f?(as_type<float>(0x0u)):r305);
  float r311=(r25!=0.0f?r35:r306);
  float r312=(r25!=0.0f?r36:r307);
  float r313=(r25!=0.0f?r37:r308);
  float r314=(r25!=0.0f?r38:r309);
  colour.x=r311;
  colour.y=r312;
  colour.z=r313;
  colour.w=r314;
  if(r310!=0.0f) discard_fragment();
  return colour;
}
