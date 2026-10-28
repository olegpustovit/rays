// Generated from scene.rays by Flow_gpu.Raster (lib/flow_gpu/test_raster.exe --write); do not edit.
#include <metal_stdlib>
using namespace metal;
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
  uint r7=6u,r10=1u,r13=2u;
  float r16=as_type<float>(0x3f800000u);
  uint r17=3u,r20=4u,r23=5u;
  float r26=as_type<float>(0x0u);
  uint r32=48u,r34=3u,r37=2u,r40=1u;
  float r47=0.0f;
  uint r48=4u;
  float r60=0.0f;
  uint r61=8u;
  float r73=0.0f;
  uint r74=12u;
  float r86=0.0f,r98=0.0f,r110=0.0f,r122=0.0f,r134=0.0f;
  uint r139=16u;
  float r151=0.0f,r163=0.0f,r175=0.0f,r199=0.0f,r211=0.0f,r223=0.0f;
  uint r240=32u;
  float r252=0.0f,r264=0.0f,r276=0.0f,r300=0.0f,r312=0.0f,r324=0.0f;
  uint r343=24u,r345=255u;
  float r348=as_type<float>(0x437f0000u);
  uint r350=16u,r355=8u;
  long r0=long(vid);
  uint r8=(uint(r0)*r7);
  float r9=buffer0[r8*1+0];
  float r12=buffer0[((r8+r10))*1+0];
  float r15=buffer0[((r8+r13))*1+0];
  float r19=buffer0[((r8+r17))*1+0];
  float r22=buffer0[((r8+r20))*1+0];
  float r25=buffer0[((r8+r23))*1+0];
  float r30=((buffer2[(83u)*1+0])>(as_type<float>(0x3f000000u)));
  uint r33=(uint((long(iid)))*r32);
  if(r30!=0.0f) {
    r47=fma(r16,(buffer3[((r33+r34))*1+0]),(fma(r15,(buffer3[((r33+r37))*1+0]),(fma(r12,(buffer3[((r33+r40))*1+0]),((r9*(buffer3[r33*1+0]))))))));
    uint r49=(r33+r48);
    r60=fma(r16,(buffer3[((r49+r34))*1+0]),(fma(r15,(buffer3[((r49+r37))*1+0]),(fma(r12,(buffer3[((r49+r40))*1+0]),((r9*(buffer3[r49*1+0]))))))));
    uint r62=(r33+r61);
    r73=fma(r16,(buffer3[((r62+r34))*1+0]),(fma(r15,(buffer3[((r62+r37))*1+0]),(fma(r12,(buffer3[((r62+r40))*1+0]),((r9*(buffer3[r62*1+0]))))))));
    uint r75=(r33+r74);
    r86=fma(r16,(buffer3[((r75+r34))*1+0]),(fma(r15,(buffer3[((r75+r37))*1+0]),(fma(r12,(buffer3[((r75+r40))*1+0]),((r9*(buffer3[r75*1+0]))))))));
  }
  if(r30==0.0f) {
    r98=fma(r16,(buffer2[(3u)*1+0]),(fma(r15,(buffer2[(2u)*1+0]),(fma(r12,(buffer2[(1u)*1+0]),((r9*(buffer2[(0u)*1+0]))))))));
    r110=fma(r16,(buffer2[(7u)*1+0]),(fma(r15,(buffer2[(6u)*1+0]),(fma(r12,(buffer2[(5u)*1+0]),((r9*(buffer2[(4u)*1+0]))))))));
    r122=fma(r16,(buffer2[(11u)*1+0]),(fma(r15,(buffer2[(10u)*1+0]),(fma(r12,(buffer2[(9u)*1+0]),((r9*(buffer2[(8u)*1+0]))))))));
    r134=fma(r16,(buffer2[(15u)*1+0]),(fma(r15,(buffer2[(14u)*1+0]),(fma(r12,(buffer2[(13u)*1+0]),((r9*(buffer2[(12u)*1+0]))))))));
  }
  float r135=(r30!=0.0f?r47:r98);
  float r136=(r30!=0.0f?r60:r110);
  float r137=(r30!=0.0f?r73:r122);
  float r138=(r30!=0.0f?r86:r134);
  if(r30!=0.0f) {
    uint r140=(r33+r139);
    r151=fma(r16,(buffer3[((r140+r34))*1+0]),(fma(r15,(buffer3[((r140+r37))*1+0]),(fma(r12,(buffer3[((r140+r40))*1+0]),((r9*(buffer3[r140*1+0]))))))));
    uint r152=(r140+r48);
    r163=fma(r16,(buffer3[((r152+r34))*1+0]),(fma(r15,(buffer3[((r152+r37))*1+0]),(fma(r12,(buffer3[((r152+r40))*1+0]),((r9*(buffer3[r152*1+0]))))))));
    uint r164=(r140+r61);
    r175=fma(r16,(buffer3[((r164+r34))*1+0]),(fma(r15,(buffer3[((r164+r37))*1+0]),(fma(r12,(buffer3[((r164+r40))*1+0]),((r9*(buffer3[r164*1+0]))))))));
  }
  if(r30==0.0f) {
    r199=fma(r16,(buffer2[(19u)*1+0]),(fma(r15,(buffer2[(18u)*1+0]),(fma(r12,(buffer2[(17u)*1+0]),((r9*(buffer2[(16u)*1+0]))))))));
    r211=fma(r16,(buffer2[(23u)*1+0]),(fma(r15,(buffer2[(22u)*1+0]),(fma(r12,(buffer2[(21u)*1+0]),((r9*(buffer2[(20u)*1+0]))))))));
    r223=fma(r16,(buffer2[(27u)*1+0]),(fma(r15,(buffer2[(26u)*1+0]),(fma(r12,(buffer2[(25u)*1+0]),((r9*(buffer2[(24u)*1+0]))))))));
  }
  float r236=(r30!=0.0f?r151:r199);
  float r237=(r30!=0.0f?r163:r211);
  float r238=(r30!=0.0f?r175:r223);
  if(r30!=0.0f) {
    uint r241=(r33+r240);
    r252=fma(r26,(buffer3[((r241+r34))*1+0]),(fma(r25,(buffer3[((r241+r37))*1+0]),(fma(r22,(buffer3[((r241+r40))*1+0]),((r19*(buffer3[r241*1+0]))))))));
    uint r253=(r241+r48);
    r264=fma(r26,(buffer3[((r253+r34))*1+0]),(fma(r25,(buffer3[((r253+r37))*1+0]),(fma(r22,(buffer3[((r253+r40))*1+0]),((r19*(buffer3[r253*1+0]))))))));
    uint r265=(r241+r61);
    r276=fma(r26,(buffer3[((r265+r34))*1+0]),(fma(r25,(buffer3[((r265+r37))*1+0]),(fma(r22,(buffer3[((r265+r40))*1+0]),((r19*(buffer3[r265*1+0]))))))));
  }
  if(r30==0.0f) {
    r300=fma(r26,(buffer2[(35u)*1+0]),(fma(r25,(buffer2[(34u)*1+0]),(fma(r22,(buffer2[(33u)*1+0]),((r19*(buffer2[(32u)*1+0]))))))));
    r312=fma(r26,(buffer2[(39u)*1+0]),(fma(r25,(buffer2[(38u)*1+0]),(fma(r22,(buffer2[(37u)*1+0]),((r19*(buffer2[(36u)*1+0]))))))));
    r324=fma(r26,(buffer2[(43u)*1+0]),(fma(r25,(buffer2[(42u)*1+0]),(fma(r22,(buffer2[(41u)*1+0]),((r19*(buffer2[(40u)*1+0]))))))));
  }
  float r337=(r30!=0.0f?r252:r300);
  float r338=(r30!=0.0f?r264:r312);
  float r339=(r30!=0.0f?r276:r324);
  uint r341=(uint(r0)*r17);
  long r342=long(buffer5[r341*1+0]);
  long r344=(as_type<long>(ulong(long((ulong(r342)&0x7fffffffffffffffUL)>>(long(r343)&63)))<<1)>>1);
  sb|=(r343>62u)?4u:0u;
  float r349=(r348==0.0f?0.0f:(float(((uint(r344)&r345))))/r348);
  long r351=(as_type<long>(ulong(long((ulong(r342)&0x7fffffffffffffffUL)>>(long(r350)&63)))<<1)>>1);
  sb|=(r350>62u)?4u:0u;
  float r354=(r348==0.0f?0.0f:(float(((uint(r351)&r345))))/r348);
  long r356=(as_type<long>(ulong(long((ulong(r342)&0x7fffffffffffffffUL)>>(long(r355)&63)))<<1)>>1);
  sb|=(r355>62u)?4u:0u;
  float r359=(r348==0.0f?0.0f:(float(((uint(r356)&r345))))/r348);
  float r362=(r348==0.0f?0.0f:(float(((uint(r342)&r345))))/r348);
  float r364=buffer4[((r341+r10))*1+0];
  float r366=buffer4[((r341+r13))*1+0];
  float r370=rsqrt(fabs((fma(r339,r339,(fma(r338,r338,((r337*r337))))))));
  float r371=(r337*r370);
  float r372=(r338*r370);
  float r373=(r339*r370);
  o.position.x=r135;
  o.position.y=r136;
  o.position.z=r137;
  o.position.w=r138;
  o.color.x=r349;
  o.color.y=r354;
  o.color.z=r359;
  o.color.w=r362;
  o.uv.x=r364;
  o.uv.y=r366;
  o.world.x=r236;
  o.world.y=r237;
  o.world.z=r238;
  o.normal.x=r371;
  o.normal.y=r372;
  o.normal.z=r373;
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
