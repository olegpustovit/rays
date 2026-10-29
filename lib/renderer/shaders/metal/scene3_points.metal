// Generated from scene.rays by Flow_gpu.Raster (lib/flow_gpu/test_raster.exe --write); do not edit.
#include <metal_stdlib>
using namespace metal;
struct scene3_points_vertex_out {
  float4 position [[position]];
  float point_size [[point_size]];
  float4 color;
  float2 uv;
  float3 world;
  float3 normal;
};
vertex scene3_points_vertex_out scene3_points_vertex(
    uint vid [[vertex_id]],
    uint iid [[instance_id]],
    device const float* buffer0 [[buffer(0)]],
    device const float* buffer1 [[buffer(1)]],
    device const float* buffer2 [[buffer(2)]],
    device const float* buffer3 [[buffer(3)]],
    device const float* buffer4 [[buffer(4)]],
    device const uint* buffer5 [[buffer(5)]]) {
  scene3_points_vertex_out o;
  uint sb=0u;
  float r9=as_type<float>(0x3f000000u);
  uint r12=3u,r16=1u,r19=2u;
  float r22=as_type<float>(0x3f800000u),r26=0.0f,r27=0.0f;
  uint r28=1u;
  float r30=0.0f;
  uint r31=2u;
  float r33=0.0f,r35=as_type<float>(0x0u),r37=0.0f,r38=0.0f,r39=0.0f,r40=0.0f;
  uint r41=3u;
  float r43=0.0f;
  uint r44=4u;
  float r46=0.0f;
  uint r47=5u;
  float r49=0.0f;
  uint r59=48u,r61=3u,r64=2u,r67=1u;
  float r74=0.0f;
  uint r75=4u;
  float r87=0.0f;
  uint r88=8u;
  float r100=0.0f;
  uint r101=12u;
  float r113=0.0f,r125=0.0f,r137=0.0f,r149=0.0f,r161=0.0f;
  uint r166=16u;
  float r178=0.0f,r190=0.0f,r202=0.0f,r226=0.0f,r238=0.0f,r250=0.0f;
  uint r267=32u;
  float r279=0.0f,r291=0.0f,r303=0.0f,r327=0.0f,r339=0.0f,r351=0.0f;
  uint r370=24u,r372=255u;
  float r375=as_type<float>(0x437f0000u);
  uint r377=16u,r382=8u;
  long r0=long(vid);
  float r10=((buffer2[(79u)*1+0])>r9);
  uint r13=(r10!=0.0f?r12:(6u));
  uint r14=(uint(r0)*r13);
  float r15=buffer0[r14*1+0];
  float r18=buffer0[((r14+r16))*1+0];
  float r21=buffer0[((r14+r19))*1+0];
  if(r10!=0.0f) {
    r26=((buffer2[(80u)*1+0])>(as_type<float>(0x3f000000u)));
    if(r26!=0.0f) {
      r27=buffer1[r14*1+0];
      r30=buffer1[((r14+r28))*1+0];
      r33=buffer1[((r14+r31))*1+0];
    }
    r37=(r26!=0.0f?r27:r35);
    r38=(r26!=0.0f?r30:r35);
    r39=(r26!=0.0f?r33:(as_type<float>(0x3f800000u)));
    r40=(r26!=0.0f?(as_type<float>(0x0u)):r35);
  }
  if(r10==0.0f) {
    r43=buffer0[((r14+r41))*1+0];
    r46=buffer0[((r14+r44))*1+0];
    r49=buffer0[((r14+r47))*1+0];
  }
  float r51=(r10!=0.0f?r37:r43);
  float r52=(r10!=0.0f?r38:r46);
  float r53=(r10!=0.0f?r39:r49);
  float r54=(r10!=0.0f?r40:(as_type<float>(0x0u)));
  float r57=((buffer2[(83u)*1+0])>r9);
  uint r60=(uint((long(iid)))*r59);
  if(r57!=0.0f) {
    r74=fma(r22,(buffer3[((r60+r61))*1+0]),(fma(r21,(buffer3[((r60+r64))*1+0]),(fma(r18,(buffer3[((r60+r67))*1+0]),((r15*(buffer3[r60*1+0]))))))));
    uint r76=(r60+r75);
    r87=fma(r22,(buffer3[((r76+r61))*1+0]),(fma(r21,(buffer3[((r76+r64))*1+0]),(fma(r18,(buffer3[((r76+r67))*1+0]),((r15*(buffer3[r76*1+0]))))))));
    uint r89=(r60+r88);
    r100=fma(r22,(buffer3[((r89+r61))*1+0]),(fma(r21,(buffer3[((r89+r64))*1+0]),(fma(r18,(buffer3[((r89+r67))*1+0]),((r15*(buffer3[r89*1+0]))))))));
    uint r102=(r60+r101);
    r113=fma(r22,(buffer3[((r102+r61))*1+0]),(fma(r21,(buffer3[((r102+r64))*1+0]),(fma(r18,(buffer3[((r102+r67))*1+0]),((r15*(buffer3[r102*1+0]))))))));
  }
  if(r57==0.0f) {
    r125=fma(r22,(buffer2[(3u)*1+0]),(fma(r21,(buffer2[(2u)*1+0]),(fma(r18,(buffer2[(1u)*1+0]),((r15*(buffer2[(0u)*1+0]))))))));
    r137=fma(r22,(buffer2[(7u)*1+0]),(fma(r21,(buffer2[(6u)*1+0]),(fma(r18,(buffer2[(5u)*1+0]),((r15*(buffer2[(4u)*1+0]))))))));
    r149=fma(r22,(buffer2[(11u)*1+0]),(fma(r21,(buffer2[(10u)*1+0]),(fma(r18,(buffer2[(9u)*1+0]),((r15*(buffer2[(8u)*1+0]))))))));
    r161=fma(r22,(buffer2[(15u)*1+0]),(fma(r21,(buffer2[(14u)*1+0]),(fma(r18,(buffer2[(13u)*1+0]),((r15*(buffer2[(12u)*1+0]))))))));
  }
  float r162=(r57!=0.0f?r74:r125);
  float r163=(r57!=0.0f?r87:r137);
  float r164=(r57!=0.0f?r100:r149);
  float r165=(r57!=0.0f?r113:r161);
  if(r57!=0.0f) {
    uint r167=(r60+r166);
    r178=fma(r22,(buffer3[((r167+r61))*1+0]),(fma(r21,(buffer3[((r167+r64))*1+0]),(fma(r18,(buffer3[((r167+r67))*1+0]),((r15*(buffer3[r167*1+0]))))))));
    uint r179=(r167+r75);
    r190=fma(r22,(buffer3[((r179+r61))*1+0]),(fma(r21,(buffer3[((r179+r64))*1+0]),(fma(r18,(buffer3[((r179+r67))*1+0]),((r15*(buffer3[r179*1+0]))))))));
    uint r191=(r167+r88);
    r202=fma(r22,(buffer3[((r191+r61))*1+0]),(fma(r21,(buffer3[((r191+r64))*1+0]),(fma(r18,(buffer3[((r191+r67))*1+0]),((r15*(buffer3[r191*1+0]))))))));
  }
  if(r57==0.0f) {
    r226=fma(r22,(buffer2[(19u)*1+0]),(fma(r21,(buffer2[(18u)*1+0]),(fma(r18,(buffer2[(17u)*1+0]),((r15*(buffer2[(16u)*1+0]))))))));
    r238=fma(r22,(buffer2[(23u)*1+0]),(fma(r21,(buffer2[(22u)*1+0]),(fma(r18,(buffer2[(21u)*1+0]),((r15*(buffer2[(20u)*1+0]))))))));
    r250=fma(r22,(buffer2[(27u)*1+0]),(fma(r21,(buffer2[(26u)*1+0]),(fma(r18,(buffer2[(25u)*1+0]),((r15*(buffer2[(24u)*1+0]))))))));
  }
  float r263=(r57!=0.0f?r178:r226);
  float r264=(r57!=0.0f?r190:r238);
  float r265=(r57!=0.0f?r202:r250);
  if(r57!=0.0f) {
    uint r268=(r60+r267);
    r279=fma(r54,(buffer3[((r268+r61))*1+0]),(fma(r53,(buffer3[((r268+r64))*1+0]),(fma(r52,(buffer3[((r268+r67))*1+0]),((r51*(buffer3[r268*1+0]))))))));
    uint r280=(r268+r75);
    r291=fma(r54,(buffer3[((r280+r61))*1+0]),(fma(r53,(buffer3[((r280+r64))*1+0]),(fma(r52,(buffer3[((r280+r67))*1+0]),((r51*(buffer3[r280*1+0]))))))));
    uint r292=(r268+r88);
    r303=fma(r54,(buffer3[((r292+r61))*1+0]),(fma(r53,(buffer3[((r292+r64))*1+0]),(fma(r52,(buffer3[((r292+r67))*1+0]),((r51*(buffer3[r292*1+0]))))))));
  }
  if(r57==0.0f) {
    r327=fma(r54,(buffer2[(35u)*1+0]),(fma(r53,(buffer2[(34u)*1+0]),(fma(r52,(buffer2[(33u)*1+0]),((r51*(buffer2[(32u)*1+0]))))))));
    r339=fma(r54,(buffer2[(39u)*1+0]),(fma(r53,(buffer2[(38u)*1+0]),(fma(r52,(buffer2[(37u)*1+0]),((r51*(buffer2[(36u)*1+0]))))))));
    r351=fma(r54,(buffer2[(43u)*1+0]),(fma(r53,(buffer2[(42u)*1+0]),(fma(r52,(buffer2[(41u)*1+0]),((r51*(buffer2[(40u)*1+0]))))))));
  }
  float r364=(r57!=0.0f?r279:r327);
  float r365=(r57!=0.0f?r291:r339);
  float r366=(r57!=0.0f?r303:r351);
  uint r368=(uint(r0)*r12);
  long r369=long(buffer5[r368*1+0]);
  long r371=(as_type<long>(ulong(long((ulong(r369)&0x7fffffffffffffffUL)>>(long(r370)&63)))<<1)>>1);
  sb|=(r370>62u)?4u:0u;
  float r376=(r375==0.0f?0.0f:(float(((uint(r371)&r372))))/r375);
  long r378=(as_type<long>(ulong(long((ulong(r369)&0x7fffffffffffffffUL)>>(long(r377)&63)))<<1)>>1);
  sb|=(r377>62u)?4u:0u;
  float r381=(r375==0.0f?0.0f:(float(((uint(r378)&r372))))/r375);
  long r383=(as_type<long>(ulong(long((ulong(r369)&0x7fffffffffffffffUL)>>(long(r382)&63)))<<1)>>1);
  sb|=(r382>62u)?4u:0u;
  float r386=(r375==0.0f?0.0f:(float(((uint(r383)&r372))))/r375);
  float r389=(r375==0.0f?0.0f:(float(((uint(r369)&r372))))/r375);
  float r391=buffer4[((r368+r16))*1+0];
  float r393=buffer4[((r368+r19))*1+0];
  float r397=rsqrt(fabs((fma(r366,r366,(fma(r365,r365,((r364*r364))))))));
  float r398=(r364*r397);
  float r399=(r365*r397);
  float r400=(r366*r397);
  o.position.x=r162;
  o.position.y=r163;
  o.position.z=r164;
  o.position.w=r165;
  o.point_size=r22;
  o.color.x=r376;
  o.color.y=r381;
  o.color.z=r386;
  o.color.w=r389;
  o.uv.x=r391;
  o.uv.y=r393;
  o.world.x=r263;
  o.world.y=r264;
  o.world.z=r265;
  o.normal.x=r398;
  o.normal.y=r399;
  o.normal.z=r400;
  return o;
}
fragment float4 scene3_fragment(
    scene3_points_vertex_out in [[stage_in]],
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
