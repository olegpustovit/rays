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
    device const float* buffer1 [[buffer(1)]],
    device const float* buffer2 [[buffer(2)]],
    device const float* buffer3 [[buffer(3)]],
    device const float* buffer4 [[buffer(4)]],
    device const uint* buffer5 [[buffer(5)]]) {
  scene3_vertex_out o;
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
fragment float4 world_fragment(
    scene3_vertex_out in [[stage_in]],
    bool front [[front_facing]],
    texture2d<float> texture7 [[texture(7)]],
    sampler sampler8 [[sampler(8)]],
    texture2d<float> texture9 [[texture(9)]],
    sampler sampler10 [[sampler(10)]],
    texture2d<float> texture11 [[texture(11)]],
    sampler sampler12 [[sampler(12)]],
    texture2d<float> texture13 [[texture(13)]],
    device const float* buffer2 [[buffer(2)]],
    device const float* buffer6 [[buffer(6)]]) {
  float4 colour;
  uint sb=0u;
  float r31=as_type<float>(0x0u),r32=as_type<float>(0x3f800000u),r107=as_type<float>(0x3f000000u),r108=0.0f,r110=0.0f,r112=0.0f,r114=0.0f,r115=as_type<float>(0xbf800000u),r128=0.0f,r129=0.0f,r130=0.0f,r138=as_type<float>(0x4020a3d7u);
  float r139=as_type<float>(0x3cf5c28fu),r146=as_type<float>(0x401b851fu),r147=as_type<float>(0x3f170a3du),r151=as_type<float>(0x3e0f5c29u),r164=as_type<float>(0x3ee8ba2eu),r168=as_type<float>(0x437f0000u),r172=0.0f,r176=0.0f,r180=0.0f,r184=0.0f,r191=0.0f,r192=0.0f;
  float r193=0.0f,r203=as_type<float>(0x3f000000u),r207=0.0f,r208=as_type<float>(0xbf800000u),r209=0.0f,r210=0.0f,r211=0.0f,r240=as_type<float>(0x400ccccdu),r246=as_type<float>(0x3cf5c28fu),r257=as_type<float>(0xbf800000u),r258=as_type<float>(0x3f800000u),r264=as_type<float>(0x3f851eb8u);
  float r280=as_type<float>(0x3e4ccccdu),r282=as_type<float>(0x0u),r304=as_type<float>(0x40490fdbu),r313=0.0f,r323=0.0f,r324=0.0f,r325=0.0f;
  uint r332=0u;
  float r338=as_type<float>(0x3e906ec1u),r348=as_type<float>(0x3efa2a2cu),r379=as_type<float>(0x3f8bd89du);
  uint r447=64u;
  float r475=0.0f,r476=as_type<float>(0x3f800000u),r528=as_type<float>(0x3f000000u),r536=as_type<float>(0x0u),r550=0.0f,r568=as_type<float>(0x0u),r570=as_type<float>(0x3f800000u);
  uint r596=2u,r598=1u,r600=0u;
  int r601=0;
  float r602=0,r669=0.0f,r684=as_type<float>(0x38d1b717u),r706=as_type<float>(0x40490fdbu),r747=0.0f,r748=0.0f,r749=0.0f;
  int r758=0;
  float r759=0,r760=0,r761=0,r1272=as_type<float>(0x4020a3d7u),r1279=as_type<float>(0x401b851fu),r1280=as_type<float>(0x3f170a3du),r1284=as_type<float>(0x3e0f5c29u),r1297=as_type<float>(0x3ee8ba2eu),r1301=as_type<float>(0x437f0000u),r1305=0.0f,r1309=0.0f,r1313=0.0f;
  float r1314=0.0f,r1315=0.0f,r1316=0.0f,r1317=0.0f;
  float r15=buffer6[(27u)*1+0];
  float r20=((in.color.x)*(texture7.sample(sampler8,float2((in.uv.x),(in.uv.y))).x));
  float r21=((in.color.y)*(texture7.sample(sampler8,float2((in.uv.x),(in.uv.y))).y));
  float r22=((in.color.z)*(texture7.sample(sampler8,float2((in.uv.x),(in.uv.y))).z));
  float r30=((buffer2[(76u)*1+0])>(as_type<float>(0x3fc00000u)));
  if(r30!=0.0f) {
    float r35=buffer2[(87u)*1+0];
    float r37=buffer2[(86u)*1+0];
    float r42=fma((in.world.y),(buffer2[(85u)*1+0]),(((in.world.x)*(buffer2[(84u)*1+0]))));
    float r47=buffer2[(91u)*1+0];
    float r49=buffer2[(90u)*1+0];
    float r54=fma((in.world.y),(buffer2[(89u)*1+0]),(((in.world.x)*(buffer2[(88u)*1+0]))));
    float r59=buffer2[(95u)*1+0];
    float r61=buffer2[(94u)*1+0];
    float r66=fma((in.world.y),(buffer2[(93u)*1+0]),(((in.world.x)*(buffer2[(92u)*1+0]))));
    float r71=buffer2[(99u)*1+0];
    float r73=buffer2[(98u)*1+0];
    float r78=fma((in.world.y),(buffer2[(97u)*1+0]),(((in.world.x)*(buffer2[(96u)*1+0]))));
    float r80=fma(r32,r71,(fma(r31,r73,r78)));
    float r88=fma(r32,r71,(fma(r32,r73,r78)));
    float r95=(((r88==0.0f?0.0f:(fma(r32,r35,(fma(r32,r37,r42))))/r88))-((r80==0.0f?0.0f:(fma(r32,r35,(fma(r31,r37,r42))))/r80)));
    float r96=(((r88==0.0f?0.0f:(fma(r32,r47,(fma(r32,r49,r54))))/r88))-((r80==0.0f?0.0f:(fma(r32,r47,(fma(r31,r49,r54))))/r80)));
    float r97=(((r88==0.0f?0.0f:(fma(r32,r59,(fma(r32,r61,r66))))/r88))-((r80==0.0f?0.0f:(fma(r32,r59,(fma(r31,r61,r66))))/r80)));
    float r101=rsqrt(fabs((fma(r97,r97,(fma(r96,r96,((r95*r95))))))));
    r108=((buffer6[(35u)*1+0])>r107);
    if(r108!=0.0f) {
      r110=buffer6[(36u)*1+0];
      r112=buffer6[(37u)*1+0];
      r114=buffer6[(38u)*1+0];
    }
    if(r108==0.0f) {
      float r121=((((as_type<float>(0x40c90fdbu))==0.0f?0.0f:(atan2(((r95*r101)),((((r97*r101))*r115))))/(as_type<float>(0x40c90fdbu))))+(as_type<float>(0x3f000000u)));
      float r127=((as_type<float>(0x40490fdbu))==0.0f?0.0f:((fabs((min((max(((r96*r101)),r115)),(as_type<float>(0x3f800000u)))))>1.0f?as_type<float>(0x7fc00000u):acos((min((max(((r96*r101)),r115)),(as_type<float>(0x3f800000u)))))))/(as_type<float>(0x40490fdbu)));
      r128=texture7.sample(sampler8,float2(r121,r127)).x;
      r129=texture7.sample(sampler8,float2(r121,r127)).y;
      r130=texture7.sample(sampler8,float2(r121,r127)).z;
    }
    float r135=(((r108!=0.0f?r110:r128))*r15);
    float r136=(((r108!=0.0f?r112:r129))*r15);
    float r137=(((r108!=0.0f?r114:r130))*r15);
    float r169=fma((pow(fabs((min((max((((fma(r135,(fma(r146,r135,r147)),r151))==0.0f?0.0f:((r135*(fma(r138,r135,r139))))/(fma(r135,(fma(r146,r135,r147)),r151)))),r31)),r32))),r164)),r168,r107);
    long r170=long(r169);
    r172=(r168==0.0f?0.0f:(trunc(r169))/r168);
    float r173=fma((pow(fabs((min((max((((fma(r136,(fma(r146,r136,r147)),r151))==0.0f?0.0f:((r136*(fma(r138,r136,r139))))/(fma(r136,(fma(r146,r136,r147)),r151)))),r31)),r32))),r164)),r168,r107);
    long r174=long(r173);
    r176=(r168==0.0f?0.0f:(trunc(r173))/r168);
    float r177=fma((pow(fabs((min((max((((fma(r137,(fma(r146,r137,r147)),r151))==0.0f?0.0f:((r137*(fma(r138,r137,r139))))/(fma(r137,(fma(r146,r137,r147)),r151)))),r31)),r32))),r164)),r168,r107);
    long r178=long(r177);
    r180=(r168==0.0f?0.0f:(trunc(r177))/r168);
  }
  if(r30==0.0f) {
    r184=((buffer2[(76u)*1+0])>(as_type<float>(0x3f000000u)));
    if(r184!=0.0f) {
      r191=((buffer2[(64u)*1+0])*r20);
      r192=((buffer2[(65u)*1+0])*r21);
      r193=((buffer2[(66u)*1+0])*r22);
    }
    if(r184==0.0f) {
      float r197=rsqrt(fabs((fma((in.normal.z),(in.normal.z),(fma((in.normal.y),(in.normal.y),(((in.normal.x)*(in.normal.x)))))))));
      float r198=((in.normal.x)*r197);
      float r199=((in.normal.y)*r197);
      float r200=((in.normal.z)*r197);
      r207=((((buffer2[(74u)*1+0])>r203))!=0.0f&&((((front?1.0f:0.0f))==0.0f))!=0.0f);
      if(r207!=0.0f) {
        r209=(r198*r208);
        r210=(r199*r208);
        r211=(r200*r208);
      }
      float r212=(r207!=0.0f?r209:r198);
      float r213=(r207!=0.0f?r210:r199);
      float r214=(r207!=0.0f?r211:r200);
      float r221=((buffer2[(48u)*1+0])-(in.world.x));
      float r222=((buffer2[(49u)*1+0])-(in.world.y));
      float r223=((buffer2[(50u)*1+0])-(in.world.z));
      float r227=rsqrt(fabs((fma(r223,r223,(fma(r222,r222,((r221*r221))))))));
      float r228=(r221*r227);
      float r229=(r222*r227);
      float r230=(r223*r227);
      float r241=pow(fabs((((buffer2[(56u)*1+0])*r20))),r240);
      float r242=pow(fabs((((buffer2[(57u)*1+0])*r21))),r240);
      float r243=pow(fabs((((buffer2[(58u)*1+0])*r22))),r240);
      float r245=buffer2[(77u)*1+0];
      float r247=max(r245,r246);
      float r248=(r247*r247);
      float r249=(r248*r248);
      float r251=buffer2[(78u)*1+0];
      float r259=fma(r247,r257,r258);
      float r275=fma((min(((r259*r259)),(pow(fabs((as_type<float>(0x40000000u))),(((as_type<float>(0xc1147ae1u))*(max((fma(r214,r230,(fma(r213,r229,((r212*r228)))))),(as_type<float>(0x38d1b717u)))))))))),r259,(fma(r247,(as_type<float>(0xbce147aeu)),(as_type<float>(0x3d2e147bu)))));
      float r279=fma(r251,(fma((as_type<float>(0xbf851eb8u)),r275,(fma(r247,(as_type<float>(0xbf126e98u)),r264)))),(fma(r264,r275,(fma(r247,(as_type<float>(0x3cb43958u)),(as_type<float>(0xbd23d70au)))))));
      float r284=min((max(((r280==0.0f?0.0f:r245/r280)),r282)),r258);
      float r285=(r228*r257);
      float r286=(r229*r257);
      float r287=(r230*r257);
      float r292=((as_type<float>(0xc0000000u))*(fma(r214,r287,(fma(r213,r286,((r212*r285)))))));
      float r300=((((as_type<float>(0x40c90fdbu))==0.0f?0.0f:(atan2((fma(r292,r212,r285)),(((fma(r292,r214,r287))*r257))))/(as_type<float>(0x40c90fdbu))))+r203);
      float r305=(r304==0.0f?0.0f:((fabs((min((max((fma(r292,r213,r286)),r257)),r258)))>1.0f?as_type<float>(0x7fc00000u):acos((min((max((fma(r292,r213,r286)),r257)),r258)))))/r304);
      float r308=((max(r247,r280))*(as_type<float>(0x40a00000u)));
      float r309=texture9.sample(sampler10,float2(r300,r305),level(r308)).x;
      float r310=texture9.sample(sampler10,float2(r300,r305),level(r308)).y;
      float r311=texture9.sample(sampler10,float2(r300,r305),level(r308)).z;
      r313=(r284<r258);
      if(r313!=0.0f) {
        float r314=texture11.sample(sampler12,float2(r300,r305)).x;
        float r315=texture11.sample(sampler12,float2(r300,r305)).y;
        float r316=texture11.sample(sampler12,float2(r300,r305)).z;
        float r319=((as_type<float>(0xbf800000u))*r284);
        r323=fma(r284,r309,(fma(r319,r314,r314)));
        r324=fma(r284,r310,(fma(r319,r315,r315)));
        r325=fma(r284,r311,(fma(r319,r316,r316)));
      }
      float r349=(r348*r213);
      float r359=(r348*r214);
      float r369=(r348*r212);
      float r380=(r379*r212);
      float r381=(r380*r213);
      float r392=(((r379*r213))*r214);
      float r406=((as_type<float>(0x3ea17b0fu))*(fma((((as_type<float>(0x40400000u))*r214)),r214,r257)));
      float r416=(r380*r214);
      float r430=((as_type<float>(0x3f0bd89du))*(fma(r212,r212,((r257*((r213*r213)))))));
      float r440=(r258-r279);
      float r459=((fma(((((r304==0.0f?0.0f:r241/r304))*(max((fma((buffer6[(24u)*1+0]),r430,(fma((buffer6[(21u)*1+0]),r416,(fma((buffer6[(18u)*1+0]),r406,(fma((buffer6[(15u)*1+0]),r392,(fma((buffer6[(12u)*1+0]),r381,(fma((buffer6[(9u)*1+0]),r369,(fma((buffer6[(6u)*1+0]),r359,(fma((buffer6[(3u)*1+0]),r349,(fma((buffer6[r332*1+0]),r338,r282)))))))))))))))))),r282)))),r440,((((r313!=0.0f?r323:r309))*r279))))+(pow(fabs((((buffer2[r447*1+0])*r20))),r240)));
      float r460=((fma(((((r304==0.0f?0.0f:r242/r304))*(max((fma((buffer6[(25u)*1+0]),r430,(fma((buffer6[(22u)*1+0]),r416,(fma((buffer6[(19u)*1+0]),r406,(fma((buffer6[(16u)*1+0]),r392,(fma((buffer6[(13u)*1+0]),r381,(fma((buffer6[(10u)*1+0]),r369,(fma((buffer6[(7u)*1+0]),r359,(fma((buffer6[(4u)*1+0]),r349,(fma((buffer6[(1u)*1+0]),r338,r282)))))))))))))))))),r282)))),r440,((((r313!=0.0f?r324:r310))*r279))))+(pow(fabs((((buffer2[(65u)*1+0])*r21))),r240)));
      float r461=((fma(((((r304==0.0f?0.0f:r243/r304))*(max((fma((buffer6[(26u)*1+0]),r430,(fma((buffer6[(23u)*1+0]),r416,(fma((buffer6[(20u)*1+0]),r406,(fma((buffer6[(17u)*1+0]),r392,(fma((buffer6[(14u)*1+0]),r381,(fma((buffer6[(11u)*1+0]),r369,(fma((buffer6[(8u)*1+0]),r359,(fma((buffer6[(5u)*1+0]),r349,(fma((buffer6[(2u)*1+0]),r338,r282)))))))))))))))))),r282)))),r440,((((r313!=0.0f?r325:r311))*r279))))+(pow(fabs((((buffer2[(66u)*1+0])*r22))),r240)));
      float r463=buffer6[(28u)*1+0];
      float r465=buffer6[(29u)*1+0];
      float r467=buffer6[(30u)*1+0];
      float r470=fma(r214,r467,(fma(r213,r465,((r212*r463)))));
      r475=((((buffer6[(31u)*1+0])>r203))!=0.0f&&((r470>r282))!=0.0f);
      if(r475!=0.0f) {
        float r524=fma(r476,(buffer6[(55u)*1+0]),(fma((in.world.z),(buffer6[(54u)*1+0]),(fma((in.world.y),(buffer6[(53u)*1+0]),(((in.world.x)*(buffer6[(52u)*1+0]))))))));
        float r527=(r524==0.0f?0.0f:(fma(r476,(buffer6[(51u)*1+0]),(fma((in.world.z),(buffer6[(50u)*1+0]),(fma((in.world.y),(buffer6[(49u)*1+0]),(((in.world.x)*(buffer6[(48u)*1+0])))))))))/r524);
        float r529=fma(((r524==0.0f?0.0f:(fma(r476,(buffer6[(43u)*1+0]),(fma((in.world.z),(buffer6[(42u)*1+0]),(fma((in.world.y),(buffer6[(41u)*1+0]),(((in.world.x)*(buffer6[(40u)*1+0])))))))))/r524)),r528,r528);
        float r532=fma((((as_type<float>(0xbf800000u))*((r524==0.0f?0.0f:(fma(r476,(buffer6[(47u)*1+0]),(fma((in.world.z),(buffer6[(46u)*1+0]),(fma((in.world.y),(buffer6[(45u)*1+0]),(((in.world.x)*(buffer6[(44u)*1+0])))))))))/r524)))),r528,r528);
        r550=((((((buffer6[(39u)*1+0])<r528))!=0.0f||((r524<=r536))!=0.0f))!=0.0f||((((((((r529<r536))!=0.0f||((r532<r536))!=0.0f))!=0.0f||((((r529>r476))!=0.0f||((r532>r476))!=0.0f))!=0.0f))!=0.0f||((((r527<r536))!=0.0f||((r527>r476))!=0.0f))!=0.0f))!=0.0f);
        if(r550==0.0f) {
          float r555=rsqrt(fabs((fma((in.normal.z),(in.normal.z),(fma((in.normal.y),(in.normal.y),(((in.normal.x)*(in.normal.x)))))))));
          float r578=(r527-(fma((buffer6[(57u)*1+0]),((r570-(min((max((fma((((in.normal.z)*r555)),(buffer6[(30u)*1+0]),(fma((((in.normal.y)*r555)),(buffer6[(29u)*1+0]),(((((in.normal.x)*r555))*(buffer6[(28u)*1+0]))))))),r568)),r570)))),(buffer6[(56u)*1+0]))));
          float r580=buffer6[(60u)*1+0];
          long r581=long(r580);
          float r583=buffer6[(61u)*1+0];
          long r584=long(r583);
          long r587=long((buffer6[(59u)*1+0]));
          long r590=long(floor(((r529*(trunc(r580))))));
          long r591=r590;
          long r594=long(floor(((r532*(trunc(r583))))));
          long r595=r594;
          long r599=(as_type<long>(ulong((ulong(((as_type<long>(ulong((ulong(r587)*ulong(long(r596))))<<1)>>1)))+ulong(long(r598))))<<1)>>1);
          bool over601=r599>long(r600) && ulong(r599-long(r600))>4096uL;
          sb|=over601?1u:0u;
          r602=r568;
          for(r601=long(r600);r601<(over601?long(r600):r599);r601++){
  uint r603=0u;
  int r604=0;
  float r605=0,r659=0.0f;
  if(r30==0.0f) {
    if(r184==0.0f) {
      if(r475!=0.0f) {
        if(r550==0.0f) {
          bool over604=r599>long(r603) && ulong(r599-long(r603))>4096uL;
          sb|=over604?1u:0u;
          r605=r602;
          for(r604=long(r603);r604<(over604?long(r603):r599);r604++){
  uint r610=0u;
  float r617=0.0f,r622=0.0f,r629=as_type<float>(0x437f0000u),r640=as_type<float>(0x43800000u),r645=0.0f,r650=0.0f,r652=0.0f,r655=0.0f,r657=0.0f;
  if(r30==0.0f) {
    if(r184==0.0f) {
      if(r475!=0.0f) {
        if(r550==0.0f) {
          long r606=(as_type<long>(ulong((ulong(r604)-ulong(r587)))<<1)>>1);
          long r607=(as_type<long>(ulong((ulong(r591)+ulong(r606)))<<1)>>1);
          long r608=(as_type<long>(ulong((ulong(r601)-ulong(r587)))<<1)>>1);
          long r609=(as_type<long>(ulong((ulong(r595)+ulong(r608)))<<1)>>1);
          r617=(((((r607<long(r610)?1.0f:0.0f))!=0.0f||((r609<long(r610)?1.0f:0.0f))!=0.0f))!=0.0f||((((r607>=r581?1.0f:0.0f))!=0.0f||((r609>=r584?1.0f:0.0f))!=0.0f))!=0.0f);
          if(r617==0.0f) {
            r622=((buffer6[(39u)*1+0])>(as_type<float>(0x3fc00000u)));
            if(r622!=0.0f) {
              float r623=float(r607);
              float r624=float(r609);
              float r630=((texture13.read(uint2(r623,r624)).x)*r629);
              long r631=long(round(r630));
              float r633=((texture13.read(uint2(r623,r624)).y)*r629);
              long r634=long(round(r633));
              float r635=round(r633);
              float r636=((texture13.read(uint2(r623,r624)).z)*r629);
              r645=((as_type<float>(0x4b7fffffu))==0.0f?0.0f:(((fma((round(r630)),(as_type<float>(0x47800000u)),(float((r634*256L)))))+(round(r636))))/(as_type<float>(0x4b7fffffu)));
            }
            if(r622==0.0f) {
              uint r647=(((uint(r609)*uint(r581)))+uint(r607));
              r650=buffer6[(((64u)+r647))*1+0];
            }
            r652=(r578<=((r622!=0.0f?r645:r650)));
            r655=(r652!=0.0f?(as_type<float>(0x3f800000u)):(as_type<float>(0x0u)));
          }
          r657=(r605+((r617!=0.0f?(as_type<float>(0x3f800000u)):r655)));
        }
      }
    }
  }
  float next604_0=r657;
  r605=next604_0;
          }
          r659=r605;
        }
      }
    }
  }
  float next601_0=r659;
  r602=next601_0;
          }
          float r663=buffer6[(58u)*1+0];
          r669=(((r570-r663))+(((float(((as_type<long>(ulong((ulong(r599)*ulong(r599)))<<1)>>1))))==0.0f?0.0f:((r663*(r602)))/(float(((as_type<long>(ulong((ulong(r599)*ulong(r599)))<<1)>>1)))))));
        }
        float r670=(r550!=0.0f?(as_type<float>(0x3f800000u)):r669);
        float r671=(r463+r228);
        float r672=(r465+r229);
        float r673=(r467+r230);
        float r677=rsqrt(fabs((fma(r673,r673,(fma(r672,r672,((r671*r671))))))));
        float r678=(r671*r677);
        float r679=(r672*r677);
        float r680=(r673*r677);
        float r685=max((fma(r214,r467,(fma(r213,r465,((r212*r463)))))),r684);
        float r689=max((fma(r214,r230,(fma(r213,r229,((r212*r228)))))),r684);
        float r693=max((fma(r214,r680,(fma(r213,r679,((r212*r678)))))),r536);
        float r702=fma(((r476-r251)),(pow(fabs(((r476-(max((fma(r230,r680,(fma(r229,r679,((r228*r678)))))),r684))))),(as_type<float>(0x40a00000u)))),r251);
        float r705=fma(((r693*r693)),((r249-r476)),r476);
        float r712=(r476-r249);
        float r724=(((r284*((((((r706*r705))*r705))==0.0f?0.0f:r249/((((r706*r705))*r705))))))*(((max(((((r685*(sqrt(fabs((fma(((r689*r689)),r712,r249)))))))+((r689*(sqrt(fabs((fma(((r685*r685)),r712,r249))))))))),(as_type<float>(0x358637bdu))))==0.0f?0.0f:r528/(max(((((r685*(sqrt(fabs((fma(((r689*r689)),r712,r249)))))))+((r689*(sqrt(fabs((fma(((r685*r685)),r712,r249))))))))),(as_type<float>(0x358637bdu)))))));
        float r725=(r476-r702);
        r747=fma((((((fma(r724,r702,((r706==0.0f?0.0f:((r241*r725))/r706))))*(buffer6[(32u)*1+0])))*r470)),r670,r459);
        r748=fma((((((fma(r724,r702,((r706==0.0f?0.0f:((r242*r725))/r706))))*(buffer6[(33u)*1+0])))*r470)),r670,r460);
        r749=fma((((((fma(r724,r702,((r706==0.0f?0.0f:((r243*r725))/r706))))*(buffer6[(34u)*1+0])))*r470)),r670,r461);
      }
      long r755=long((buffer2[(73u)*1+0]));
      long r757=(((r755<long(r447)?1.0f:0.0f))!=0.0f?r755:long(r447));
      bool over758=r757>long(r332) && ulong(r757-long(r332))>4096uL;
      sb|=over758?1u:0u;
      r759=((r475!=0.0f?r747:r459));
      r760=((r475!=0.0f?r748:r460));
      r761=((r475!=0.0f?r749:r461));
      for(r758=long(r332);r758<(over758?long(r332):r757);r758++){
  uint r762=20u;
  float r768=0.0f;
  uint r770=8u,r773=1u,r778=2u;
  float r782=0.0f,r783=as_type<float>(0xbf800000u),r791=0.0f,r792=0.0f,r793=0.0f,r795=0.0f,r803=as_type<float>(0x0u),r804=0.0f,r805=0.0f,r806=0.0f,r807=0.0f,r808=as_type<float>(0x0u);
  float r809=0.0f,r810=0.0f,r811=0.0f,r812=as_type<float>(0x3fc00000u);
  uint r815=9u,r818=2u,r822=1u;
  float r828=0.0f,r829=0.0f,r832=as_type<float>(0xbf800000u),r852=0.0f;
  uint r853=13u;
  float r856=0.0f;
  uint r860=14u;
  float r863=0.0f,r865=0.0f,r866=0.0f;
  uint r867=9u,r870=1u,r873=2u;
  float r885=0.0f,r886=as_type<float>(0x0u),r888=as_type<float>(0x0u),r893=as_type<float>(0xbf800000u);
  uint r919=14u;
  float r929=as_type<float>(0x0u);
  uint r930=0u;
  int r931=0;
  float r932=0,r933=0,r934=0,r935=0,r1022=0.0f,r1027=0.0f,r1032=0.0f,r1033=0.0f,r1034=0.0f,r1035=as_type<float>(0x0u),r1036=0.0f,r1037=0.0f;
  float r1038=0.0f,r1039=0.0f,r1040=0.0f,r1041=0.0f,r1042=0.0f,r1043=0.0f,r1044=0.0f,r1045=0.0f,r1046=0.0f;
  uint r1047=9u,r1050=1u,r1053=2u,r1056=12u,r1063=15u,r1070=8u;
  float r1079=as_type<float>(0x0u);
  uint r1080=0u,r1081=0;
  float r1082=0,r1083=0,r1084=0,r1085=0,r1146=0.0f,r1151=0.0f,r1156=0.0f,r1157=0.0f,r1158=0.0f,r1159=as_type<float>(0x0u),r1160=0.0f,r1161=0.0f;
  float r1162=0.0f,r1170=as_type<float>(0x0u),r1173=0.0f,r1187=as_type<float>(0x38d1b717u),r1202=as_type<float>(0x3f800000u),r1211=as_type<float>(0x40490fdbu);
  uint r1241=4u,r1244=1u,r1247=2u;
  float r1259=0.0f,r1260=0.0f,r1261=0.0f,r1262=0.0f,r1263=0.0f,r1264=0.0f;
  if(r30==0.0f) {
    if(r184==0.0f) {
      uint r763=(uint(r758)*r762);
      uint r765=((84u)+r763);
      r768=((buffer2[r765*1+0])<(as_type<float>(0x40600000u)));
      if(r768!=0.0f) {
        float r769=buffer2[r765*1+0];
        float r772=buffer2[((r765+r770))*1+0];
        uint r774=(r765+r773);
        float r775=buffer2[r774*1+0];
        float r777=buffer2[((r774+r773))*1+0];
        float r780=buffer2[((r774+r778))*1+0];
        r782=(r769<(as_type<float>(0x3f000000u)));
        if(r782!=0.0f) {
          float r784=(r775*r783);
          float r785=(r777*r783);
          float r786=(r780*r783);
          float r790=rsqrt(fabs((fma(r786,r786,(fma(r785,r785,((r784*r784))))))));
          r791=(r784*r790);
          r792=(r785*r790);
          r793=(r786*r790);
        }
        if(r782==0.0f) {
          r795=(r769<(as_type<float>(0x40200000u)));
          if(r795!=0.0f) {
            float r796=(r775-(in.world.x));
            float r797=(r777-(in.world.y));
            float r798=(r780-(in.world.z));
            float r802=sqrt(fabs((fma(r798,r798,(fma(r797,r797,((r796*r796))))))));
            r804=(r802>r803);
            if(r804!=0.0f) {
              r805=(r802==0.0f?0.0f:r796/r802);
              r806=(r802==0.0f?0.0f:r797/r802);
              r807=(r802==0.0f?0.0f:r798/r802);
            }
            r809=(r804!=0.0f?r805:r808);
            r810=(r804!=0.0f?r806:r808);
            r811=(r804!=0.0f?r807:r808);
            uint r816=(((r769<r812))!=0.0f?r815:(15u));
            uint r817=(r765+r816);
            float r827=fma((((buffer2[((r817+r818))*1+0])*r802)),r802,(fma((buffer2[((r817+r822))*1+0]),r802,(buffer2[r817*1+0]))));
            r828=(r827>r803);
            if(r828!=0.0f) {
              r829=(r827==0.0f?0.0f:r772/r827);
            }
            float r831=(r828!=0.0f?r829:(as_type<float>(0x0u)));
            uint r836=(r765+r815);
            float r837=buffer2[r836*1+0];
            float r839=buffer2[((r836+r822))*1+0];
            float r841=buffer2[((r836+r818))*1+0];
            float r845=rsqrt(fabs((fma(r841,r841,(fma(r839,r839,((r837*r837))))))));
            float r851=fma(((r811*r832)),((r841*r845)),(fma(((r810*r832)),((r839*r845)),((((r809*r832))*((r837*r845)))))));
            r852=(r769>=r812);
            if(r852!=0.0f) {
              r856=(r851<(buffer2[((r765+r853))*1+0]));
              if(r856==0.0f) {
                r863=pow(fabs((max(r851,(as_type<float>(0x0u))))),(buffer2[((r765+r860))*1+0]));
              }
              r865=(r831*((r856!=0.0f?(as_type<float>(0x0u)):r863)));
            }
            r866=(r852!=0.0f?r865:r831);
          }
          if(r795==0.0f) {
            uint r868=(r765+r867);
            float r869=buffer2[r868*1+0];
            float r872=buffer2[((r868+r870))*1+0];
            float r875=buffer2[((r868+r873))*1+0];
            float r879=rsqrt(fabs((fma(r875,r875,(fma(r872,r872,((r869*r869))))))));
            float r880=(r869*r879);
            float r881=(r872*r879);
            float r882=(r875*r879);
            r885=((fabs(r882))<(as_type<float>(0x3f7fbe77u)));
            float r890=(r885!=0.0f?r886:r888);
            float r891=(r885!=0.0f?r886:(as_type<float>(0x3f800000u)));
            float r892=(r885!=0.0f?(as_type<float>(0x3f800000u)):r888);
            float r896=fma(r891,r882,((r893*((r892*r881)))));
            float r899=fma(r892,r880,((r893*((r890*r882)))));
            float r902=fma(r890,r881,((r893*((r891*r880)))));
            float r906=rsqrt(fabs((fma(r902,r902,(fma(r899,r899,((r896*r896))))))));
            float r907=(r896*r906);
            float r908=(r899*r906);
            float r909=(r902*r906);
            float r912=fma(r881,r909,((r893*((r882*r908)))));
            float r915=fma(r882,r907,((r893*((r880*r909)))));
            float r918=fma(r880,r908,((r893*((r881*r907)))));
            long r923=long(round((sqrt(fabs((buffer2[((r765+r919))*1+0]))))));
            long r924=r923;
            long r926=(((r924<long(r870)?1.0f:0.0f))!=0.0f?long(r870):r924);
            long r927=(as_type<long>(ulong((ulong(r926)*ulong(r926)))<<1)>>1);
            float r928=float(r927);
            bool over931=r926>long(r930) && ulong(r926-long(r930))>4096uL;
            sb|=over931?1u:0u;
            r932=r929;
            r933=r929;
            r934=r929;
            r935=r929;
            for(r931=long(r930);r931<(over931?long(r930):r926);r931++){
  uint r936=0u;
  int r937=0;
  float r938=0,r939=0,r940=0,r941=0,r1014=0.0f,r1015=0.0f,r1016=0.0f,r1017=0.0f;
  if(r30==0.0f) {
    if(r184==0.0f) {
      if(r768!=0.0f) {
        if(r782==0.0f) {
          if(r795==0.0f) {
            bool over937=r926>long(r936) && ulong(r926-long(r936))>4096uL;
            sb|=over937?1u:0u;
            r938=r932;
            r939=r933;
            r940=r934;
            r941=r935;
            for(r937=long(r936);r937<(over937?long(r936):r926);r937++){
  float r943=as_type<float>(0x3f000000u);
  uint r952=12u,r956=13u;
  float r974=0.0f,r978=as_type<float>(0xbf800000u),r985=as_type<float>(0x0u);
  uint r987=15u,r989=2u,r993=1u;
  float r999=0.0f,r1002=0.0f,r1005=0.0f,r1006=0.0f,r1007=0.0f,r1008=0.0f,r1009=0.0f,r1010=0.0f,r1011=0.0f,r1012=0.0f;
  if(r30==0.0f) {
    if(r184==0.0f) {
      if(r768!=0.0f) {
        if(r782==0.0f) {
          if(r795==0.0f) {
            float r945=float(r926);
            float r955=(((((r945==0.0f?0.0f:(((float(r937))+r943))/r945))-r943))*(buffer2[((r765+r952))*1+0]));
            float r959=(((((r945==0.0f?0.0f:(((float(r931))+r943))/r945))-r943))*(buffer2[((r765+r956))*1+0]));
            float r966=((fma(r959,r912,(fma(r955,r907,r775))))-(in.world.x));
            float r967=((fma(r959,r915,(fma(r955,r908,r777))))-(in.world.y));
            float r968=((fma(r959,r918,(fma(r955,r909,r780))))-(in.world.z));
            float r972=sqrt(fabs((fma(r968,r968,(fma(r967,r967,((r966*r966))))))));
            r974=(r972>(as_type<float>(0x0u)));
            if(r974!=0.0f) {
              float r975=(r972==0.0f?0.0f:r966/r972);
              float r976=(r972==0.0f?0.0f:r967/r972);
              float r977=(r972==0.0f?0.0f:r968/r972);
              uint r988=(r765+r987);
              float r998=fma((((buffer2[((r988+r989))*1+0])*r972)),r972,(fma((buffer2[((r988+r993))*1+0]),r972,(buffer2[r988*1+0]))));
              r999=(r998>r985);
              if(r999!=0.0f) {
                r1002=(((r928*r998))==0.0f?0.0f:((r772*(max((fma(r882,((r977*r978)),(fma(r881,((r976*r978)),((r880*((r975*r978)))))))),r985))))/((r928*r998)));
              }
              float r1004=(r999!=0.0f?r1002:(as_type<float>(0x0u)));
              r1005=fma(r975,r1004,r938);
              r1006=fma(r976,r1004,r939);
              r1007=fma(r977,r1004,r940);
              r1008=(r941+r1004);
            }
            r1009=(r974!=0.0f?r1005:r938);
            r1010=(r974!=0.0f?r1006:r939);
            r1011=(r974!=0.0f?r1007:r940);
            r1012=(r974!=0.0f?r1008:r941);
          }
        }
      }
    }
  }
  float next937_0=r1009;
  float next937_1=r1010;
  float next937_2=r1011;
  float next937_3=r1012;
  r938=next937_0;
  r939=next937_1;
  r940=next937_2;
  r941=next937_3;
            }
            r1014=r938;
            r1015=r939;
            r1016=r940;
            r1017=r941;
          }
        }
      }
    }
  }
  float next931_0=r1014;
  float next931_1=r1015;
  float next931_2=r1016;
  float next931_3=r1017;
  r932=next931_0;
  r933=next931_1;
  r934=next931_2;
  r935=next931_3;
            }
            float r1019=r932;
            float r1020=r933;
            float r1021=r934;
            r1022=r935;
            r1027=((sqrt(fabs((fma(r1021,r1021,(fma(r1020,r1020,((r1019*r1019)))))))))>r929);
            if(r1027!=0.0f) {
              float r1031=rsqrt(fabs((fma(r1021,r1021,(fma(r1020,r1020,((r1019*r1019))))))));
              r1032=(r1019*r1031);
              r1033=(r1020*r1031);
              r1034=(r1021*r1031);
            }
            r1036=(r1027!=0.0f?r1032:r1035);
            r1037=(r1027!=0.0f?r1033:r1035);
            r1038=(r1027!=0.0f?r1034:r1035);
          }
          r1039=(r795!=0.0f?r809:r1036);
          r1040=(r795!=0.0f?r810:r1037);
          r1041=(r795!=0.0f?r811:r1038);
          r1042=(r795!=0.0f?r866:r1022);
        }
        r1043=(r782!=0.0f?r791:r1039);
        r1044=(r782!=0.0f?r792:r1040);
        r1045=(r782!=0.0f?r793:r1041);
        r1046=(r782!=0.0f?r772:r1042);
      }
      if(r768==0.0f) {
        uint r1048=(r765+r1047);
        float r1049=buffer2[r1048*1+0];
        float r1052=buffer2[((r1048+r1050))*1+0];
        float r1055=buffer2[((r1048+r1053))*1+0];
        uint r1057=(r765+r1056);
        float r1058=buffer2[r1057*1+0];
        float r1060=buffer2[((r1057+r1050))*1+0];
        float r1062=buffer2[((r1057+r1053))*1+0];
        uint r1064=(r765+r1063);
        float r1065=buffer2[r1064*1+0];
        float r1067=buffer2[((r1064+r1050))*1+0];
        float r1069=buffer2[((r1064+r1053))*1+0];
        float r1072=buffer2[((r765+r1070))*1+0];
        uint r1073=(r765+r1050);
        float r1074=buffer2[r1073*1+0];
        float r1076=buffer2[((r1073+r1050))*1+0];
        float r1078=buffer2[((r1073+r1053))*1+0];
        bool over1081=long(r1053)>long(r1080) && ulong(long(r1053)-long(r1080))>4096uL;
        sb|=over1081?1u:0u;
        r1082=r1079;
        r1083=r1079;
        r1084=r1079;
        r1085=r1079;
        for(r1081=r1080;r1081<(over1081?r1080:r1053);r1081++){
  uint r1086=0u,r1087=2u,r1088=0;
  float r1089=0,r1090=0,r1091=0,r1092=0,r1138=0.0f,r1139=0.0f,r1140=0.0f,r1141=0.0f;
  if(r30==0.0f) {
    if(r184==0.0f) {
      if(r768==0.0f) {
        bool over1088=long(r1087)>long(r1086) && ulong(long(r1087)-long(r1086))>4096uL;
        sb|=over1088?1u:0u;
        r1089=r1082;
        r1090=r1083;
        r1091=r1084;
        r1092=r1085;
        for(r1088=r1086;r1088<(over1088?r1086:r1087);r1088++){
  float r1094=as_type<float>(0x3f000000u),r1111=0.0f,r1116=as_type<float>(0xbf800000u),r1129=0.0f,r1130=0.0f,r1131=0.0f,r1132=0.0f,r1133=0.0f,r1134=0.0f,r1135=0.0f,r1136=0.0f;
  if(r30==0.0f) {
    if(r184==0.0f) {
      if(r768==0.0f) {
        float r1095=((float(r1088))-r1094);
        float r1097=((float(r1081))-r1094);
        float r1104=((fma(r1097,r1058,(fma(r1095,r1049,r1074))))-(in.world.x));
        float r1105=((fma(r1097,r1060,(fma(r1095,r1052,r1076))))-(in.world.y));
        float r1106=((fma(r1097,r1062,(fma(r1095,r1055,r1078))))-(in.world.z));
        float r1109=fma(r1106,r1106,(fma(r1105,r1105,((r1104*r1104)))));
        r1111=(r1109>(as_type<float>(0x0u)));
        if(r1111!=0.0f) {
          float r1112=rsqrt(fabs(r1109));
          float r1113=(r1104*r1112);
          float r1114=(r1105*r1112);
          float r1115=(r1106*r1112);
          float r1128=((((as_type<float>(0x40800000u))*r1109))==0.0f?0.0f:((r1072*(max((fma(r1069,((r1115*r1116)),(fma(r1067,((r1114*r1116)),((r1065*((r1113*r1116)))))))),(as_type<float>(0x0u))))))/(((as_type<float>(0x40800000u))*r1109)));
          r1129=fma(r1113,r1128,r1089);
          r1130=fma(r1114,r1128,r1090);
          r1131=fma(r1115,r1128,r1091);
          r1132=(r1092+r1128);
        }
        r1133=(r1111!=0.0f?r1129:r1089);
        r1134=(r1111!=0.0f?r1130:r1090);
        r1135=(r1111!=0.0f?r1131:r1091);
        r1136=(r1111!=0.0f?r1132:r1092);
      }
    }
  }
  float next1088_0=r1133;
  float next1088_1=r1134;
  float next1088_2=r1135;
  float next1088_3=r1136;
  r1089=next1088_0;
  r1090=next1088_1;
  r1091=next1088_2;
  r1092=next1088_3;
        }
        r1138=r1089;
        r1139=r1090;
        r1140=r1091;
        r1141=r1092;
      }
    }
  }
  float next1081_0=r1138;
  float next1081_1=r1139;
  float next1081_2=r1140;
  float next1081_3=r1141;
  r1082=next1081_0;
  r1083=next1081_1;
  r1084=next1081_2;
  r1085=next1081_3;
        }
        float r1143=r1082;
        float r1144=r1083;
        float r1145=r1084;
        r1146=r1085;
        r1151=((sqrt(fabs((fma(r1145,r1145,(fma(r1144,r1144,((r1143*r1143)))))))))>r1079);
        if(r1151!=0.0f) {
          float r1155=rsqrt(fabs((fma(r1145,r1145,(fma(r1144,r1144,((r1143*r1143))))))));
          r1156=(r1143*r1155);
          r1157=(r1144*r1155);
          r1158=(r1145*r1155);
        }
        r1160=(r1151!=0.0f?r1156:r1159);
        r1161=(r1151!=0.0f?r1157:r1159);
        r1162=(r1151!=0.0f?r1158:r1159);
      }
      float r1163=(r768!=0.0f?r1043:r1160);
      float r1164=(r768!=0.0f?r1044:r1161);
      float r1165=(r768!=0.0f?r1045:r1162);
      float r1166=(r768!=0.0f?r1046:r1146);
      float r1169=fma(r214,r1165,(fma(r213,r1164,((r212*r1163)))));
      r1173=(((r1169>r1170))!=0.0f&&((r1166>r1170))!=0.0f);
      if(r1173!=0.0f) {
        float r1174=(r1163+r228);
        float r1175=(r1164+r229);
        float r1176=(r1165+r230);
        float r1180=rsqrt(fabs((fma(r1176,r1176,(fma(r1175,r1175,((r1174*r1174))))))));
        float r1181=(r1174*r1180);
        float r1182=(r1175*r1180);
        float r1183=(r1176*r1180);
        float r1188=max((fma(r214,r1165,(fma(r213,r1164,((r212*r1163)))))),r1187);
        float r1192=max((fma(r214,r230,(fma(r213,r229,((r212*r228)))))),r1187);
        float r1197=max((fma(r214,r1183,(fma(r213,r1182,((r212*r1181)))))),(as_type<float>(0x0u)));
        float r1207=fma(((r1202-r251)),(pow(fabs(((r1202-(max((fma(r230,r1183,(fma(r229,r1182,((r228*r1181)))))),r1187))))),(as_type<float>(0x40a00000u)))),r251);
        float r1210=fma(((r1197*r1197)),((r249-r1202)),r1202);
        float r1217=(r1202-r249);
        float r1230=(((r284*((((((r1211*r1210))*r1210))==0.0f?0.0f:r249/((((r1211*r1210))*r1210))))))*(((max(((((r1188*(sqrt(fabs((fma(((r1192*r1192)),r1217,r249)))))))+((r1192*(sqrt(fabs((fma(((r1188*r1188)),r1217,r249))))))))),(as_type<float>(0x358637bdu))))==0.0f?0.0f:(as_type<float>(0x3f000000u))/(max(((((r1188*(sqrt(fabs((fma(((r1192*r1192)),r1217,r249)))))))+((r1192*(sqrt(fabs((fma(((r1188*r1188)),r1217,r249))))))))),(as_type<float>(0x358637bdu)))))));
        float r1231=(r1202-r1207);
        uint r1242=(r765+r1241);
        r1259=fma((((((((fma(r1230,r1207,((r1211==0.0f?0.0f:((r241*r1231))/r1211))))*(buffer2[r1242*1+0])))*r1166))*r1211)),r1169,r759);
        r1260=fma((((((((fma(r1230,r1207,((r1211==0.0f?0.0f:((r242*r1231))/r1211))))*(buffer2[((r1242+r1244))*1+0])))*r1166))*r1211)),r1169,r760);
        r1261=fma((((((((fma(r1230,r1207,((r1211==0.0f?0.0f:((r243*r1231))/r1211))))*(buffer2[((r1242+r1247))*1+0])))*r1166))*r1211)),r1169,r761);
      }
      r1262=(r1173!=0.0f?r1259:r759);
      r1263=(r1173!=0.0f?r1260:r760);
      r1264=(r1173!=0.0f?r1261:r761);
    }
  }
  float next758_0=r1262;
  float next758_1=r1263;
  float next758_2=r1264;
  r759=next758_0;
  r760=next758_1;
  r761=next758_2;
      }
      float r1269=((r759)*r15);
      float r1270=((r760)*r15);
      float r1271=((r761)*r15);
      float r1302=fma((pow(fabs((min((max((((fma(r1269,(fma(r1279,r1269,r1280)),r1284))==0.0f?0.0f:((r1269*(fma(r1272,r1269,r246))))/(fma(r1269,(fma(r1279,r1269,r1280)),r1284)))),r282)),r258))),r1297)),r1301,r203);
      long r1303=long(r1302);
      r1305=(r1301==0.0f?0.0f:(trunc(r1302))/r1301);
      float r1306=fma((pow(fabs((min((max((((fma(r1270,(fma(r1279,r1270,r1280)),r1284))==0.0f?0.0f:((r1270*(fma(r1272,r1270,r246))))/(fma(r1270,(fma(r1279,r1270,r1280)),r1284)))),r282)),r258))),r1297)),r1301,r203);
      long r1307=long(r1306);
      r1309=(r1301==0.0f?0.0f:(trunc(r1306))/r1301);
      float r1310=fma((pow(fabs((min((max((((fma(r1271,(fma(r1279,r1271,r1280)),r1284))==0.0f?0.0f:((r1271*(fma(r1272,r1271,r246))))/(fma(r1271,(fma(r1279,r1271,r1280)),r1284)))),r282)),r258))),r1297)),r1301,r203);
      long r1311=long(r1310);
      r1313=(r1301==0.0f?0.0f:(trunc(r1310))/r1301);
    }
    r1314=(r184!=0.0f?r191:r1305);
    r1315=(r184!=0.0f?r192:r1309);
    r1316=(r184!=0.0f?r193:r1313);
    r1317=(((((buffer2[(59u)*1+0])*(in.color.w)))*(texture7.sample(sampler8,float2((in.uv.x),(in.uv.y))).w)));
  }
  float r1318=(r30!=0.0f?r172:r1314);
  float r1319=(r30!=0.0f?r176:r1315);
  float r1320=(r30!=0.0f?r180:r1316);
  float r1321=(r30!=0.0f?r32:r1317);
  colour.x=r1318;
  colour.y=r1319;
  colour.z=r1320;
  colour.w=r1321;
  return colour;
}
