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
  uint r11=3u,r16=1u,r19=2u;
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
  uint r373=0u;
  float r375=as_type<float>(0x3f800000u),r377=0.0f,r379=as_type<float>(0x3f800000u),r380=as_type<float>(0x0u),r383=as_type<float>(0x43800000u),r385=as_type<float>(0x437f0000u),r387=as_type<float>(0x3f000000u),r393=0.0f;
  uint r394=1u;
  long r395=0;
  float r400=0.0f;
  uint r401=1u;
  long r402=0;
  uint r404=24u,r406=1u;
  float r418=0.0f;
  uint r419=1u;
  long r420=0;
  float r425=0.0f;
  uint r426=1u;
  long r427=0;
  uint r429=16u,r431=2u;
  float r443=0.0f;
  uint r444=1u;
  long r445=0;
  float r450=0.0f;
  uint r451=1u;
  long r452=0;
  uint r454=8u;
  float r457=0.0f;
  uint r458=3u;
  float r469=as_type<float>(0x3f000000u),r475=0.0f;
  uint r476=1u;
  long r477=0;
  float r482=0.0f;
  uint r483=1u;
  long r484=0,r485=0;
  uint r486=255u;
  float r494=0.0f,r498=0.0f,r502=0.0f,r505=0.0f;
  uint r507=24u,r509=255u;
  float r512=as_type<float>(0x437f0000u),r513=0.0f;
  uint r514=16u;
  float r518=0.0f;
  uint r519=8u;
  float r523=0.0f,r526=0.0f,r527=0.0f,r528=0.0f,r529=0.0f,r530=0.0f,r537=as_type<float>(0x0u);
  uint r538=1u;
  float r540=0.0f;
  uint r541=2u;
  float r543=0.0f;
  long r0=long(vid);
  float r10=((buffer2[(79u)*1+0])>r9);
  uint r13=(r10!=0.0f?r11:(6u));
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
  uint r368=(uint(r0)*r11);
  long r371=long((buffer2[(81u)*1+0]));
  uint r372=(uint(r0)*uint(r371));
  float r374=(r371<long(r373)?1.0f:0.0f);
  if(r374==0.0f) {
    r377=(r371>long((0u))?1.0f:0.0f);
    if(r377!=0.0f) {
      float r382=min(r379,(max(r380,(buffer4[r372*1+0]))));
      float r384=(r382*r383);
      float r388=(((r382*r385))+r387);
      long r389=long(floor(r388));
      r393=(((r384-(((floor(r388))-r387))))<r382);
      if(r393!=0.0f) {
        r395=as_type<long>(ulong(r389)-ulong(long(r394)));
      }
      long r396=(r393!=0.0f?r395:r389);
      r400=(((r384-(((float(r396))+r387))))>=r382);
      if(r400!=0.0f) {
        r402=as_type<long>(ulong(r396)+ulong(long(r401)));
      }
      long r405=as_type<long>(ulong(((r400!=0.0f?r402:r396)))<<(long(r404)&63));
      sb|=(r404>63u)?4u:0u;
      float r410=min(r379,(max(r380,(buffer4[((r372+r406))*1+0]))));
      float r411=(r410*r383);
      float r413=(((r410*r385))+r387);
      long r414=long(floor(r413));
      r418=(((r411-(((floor(r413))-r387))))<r410);
      if(r418!=0.0f) {
        r420=as_type<long>(ulong(r414)-ulong(long(r419)));
      }
      long r421=(r418!=0.0f?r420:r414);
      r425=(((r411-(((float(r421))+r387))))>=r410);
      if(r425!=0.0f) {
        r427=as_type<long>(ulong(r421)+ulong(long(r426)));
      }
      long r430=as_type<long>(ulong(((r425!=0.0f?r427:r421)))<<(long(r429)&63));
      sb|=(r429>63u)?4u:0u;
      float r435=min(r379,(max(r380,(buffer4[((r372+r431))*1+0]))));
      float r436=(r435*r383);
      float r438=(((r435*r385))+r387);
      long r439=long(floor(r438));
      r443=(((r436-(((floor(r438))-r387))))<r435);
      if(r443!=0.0f) {
        r445=as_type<long>(ulong(r439)-ulong(long(r444)));
      }
      long r446=(r443!=0.0f?r445:r439);
      r450=(((r436-(((float(r446))+r387))))>=r435);
      if(r450!=0.0f) {
        r452=as_type<long>(ulong(r446)+ulong(long(r451)));
      }
      long r455=as_type<long>(ulong(((r450!=0.0f?r452:r446)))<<(long(r454)&63));
      sb|=(r454>63u)?4u:0u;
      r457=(r371==long((4u))?1.0f:0.0f);
      if(r457!=0.0f) {
        float r464=min((as_type<float>(0x3f800000u)),(max((as_type<float>(0x0u)),(buffer4[((r372+r458))*1+0]))));
        float r466=(r464*(as_type<float>(0x43800000u)));
        float r470=(((r464*(as_type<float>(0x437f0000u))))+r469);
        long r471=long(floor(r470));
        r475=(((r466-(((floor(r470))-r469))))<r464);
        if(r475!=0.0f) {
          r477=as_type<long>(ulong(r471)-ulong(long(r476)));
        }
        long r478=(r475!=0.0f?r477:r471);
        r482=(((r466-(((float(r478))+r469))))>=r464);
        if(r482!=0.0f) {
          r484=as_type<long>(ulong(r478)+ulong(long(r483)));
        }
        r485=(r482!=0.0f?r484:r478);
      }
      long r487=(r457!=0.0f?r485:long(r486));
      long r488=(r455|r487);
      long r489=(r430|r488);
      long r490=(r405|r489);
      long r491=as_type<long>(ulong(r490)>>(long(r404)&63));
      sb|=(r404>63u)?4u:0u;
      r494=(r385==0.0f?0.0f:(float(((uint(r491)&r486))))/r385);
      long r495=as_type<long>(ulong(r490)>>(long(r429)&63));
      sb|=(r429>63u)?4u:0u;
      r498=(r385==0.0f?0.0f:(float(((uint(r495)&r486))))/r385);
      long r499=as_type<long>(ulong(r490)>>(long(r454)&63));
      sb|=(r454>63u)?4u:0u;
      r502=(r385==0.0f?0.0f:(float(((uint(r499)&r486))))/r385);
      r505=(r385==0.0f?0.0f:(float(((uint(r490)&r486))))/r385);
    }
    if(r377==0.0f) {
      long r506=long(buffer5[r368*1+0]);
      long r508=as_type<long>(ulong(r506)>>(long(r507)&63));
      sb|=(r507>63u)?4u:0u;
      r513=(r512==0.0f?0.0f:(float(((uint(r508)&r509))))/r512);
      long r515=as_type<long>(ulong(r506)>>(long(r514)&63));
      sb|=(r514>63u)?4u:0u;
      r518=(r512==0.0f?0.0f:(float(((uint(r515)&r509))))/r512);
      long r520=as_type<long>(ulong(r506)>>(long(r519)&63));
      sb|=(r519>63u)?4u:0u;
      r523=(r512==0.0f?0.0f:(float(((uint(r520)&r509))))/r512);
      r526=(r512==0.0f?0.0f:(float(((uint(r506)&r509))))/r512);
    }
    r527=(r377!=0.0f?r494:r513);
    r528=(r377!=0.0f?r498:r518);
    r529=(r377!=0.0f?r502:r523);
    r530=(r377!=0.0f?r505:r526);
  }
  float r531=(r374!=0.0f?r375:r527);
  float r532=(r374!=0.0f?r375:r528);
  float r533=(r374!=0.0f?r375:r529);
  float r534=(r374!=0.0f?r375:r530);
  float r536=(((r371==long(r373)?1.0f:0.0f))==0.0f);
  if(r536==0.0f) {
    r540=buffer4[((r368+r538))*1+0];
    r543=buffer4[((r368+r541))*1+0];
  }
  float r544=(r536!=0.0f?r537:r540);
  float r545=(r536!=0.0f?r537:r543);
  float r549=rsqrt(fabs((fma(r366,r366,(fma(r365,r365,((r364*r364))))))));
  float r550=(r364*r549);
  float r551=(r365*r549);
  float r552=(r366*r549);
  o.position.x=r162;
  o.position.y=r163;
  o.position.z=r164;
  o.position.w=r165;
  o.color.x=r531;
  o.color.y=r532;
  o.color.z=r533;
  o.color.w=r534;
  o.uv.x=r544;
  o.uv.y=r545;
  o.world.x=r263;
  o.world.y=r264;
  o.world.z=r265;
  o.normal.x=r550;
  o.normal.y=r551;
  o.normal.z=r552;
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
  float r475=0.0f,r476=as_type<float>(0x3f800000u),r528=as_type<float>(0x3f000000u),r536=as_type<float>(0x0u),r538=0.0f,r539=as_type<float>(0x0u),r543=as_type<float>(0x3f800000u),r551=0.0f,r552=0.0f,r570=as_type<float>(0x0u),r572=as_type<float>(0x3f800000u);
  uint r598=2u,r600=1u,r602=0u;
  int r603=0;
  float r604=0,r671=0.0f,r686=as_type<float>(0x38d1b717u),r708=as_type<float>(0x40490fdbu),r749=0.0f,r750=0.0f,r751=0.0f;
  int r760=0;
  float r761=0,r762=0,r763=0,r1274=as_type<float>(0x4020a3d7u),r1281=as_type<float>(0x401b851fu),r1282=as_type<float>(0x3f170a3du),r1286=as_type<float>(0x3e0f5c29u),r1299=as_type<float>(0x3ee8ba2eu),r1303=as_type<float>(0x437f0000u),r1307=0.0f,r1311=0.0f,r1315=0.0f;
  float r1316=0.0f,r1317=0.0f,r1318=0.0f,r1319=0.0f;
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
        r538=((((buffer6[(39u)*1+0])<r528))!=0.0f||((r524<=r536))!=0.0f);
        if(r538==0.0f) {
          r551=(((((((r529<r539))!=0.0f||((r532<r539))!=0.0f))!=0.0f||((((r529>r543))!=0.0f||((r532>r543))!=0.0f))!=0.0f))!=0.0f||((((r527<r539))!=0.0f||((r527>r543))!=0.0f))!=0.0f);
        }
        r552=(r538!=0.0f||r551!=0.0f);
        if(r552==0.0f) {
          float r557=rsqrt(fabs((fma((in.normal.z),(in.normal.z),(fma((in.normal.y),(in.normal.y),(((in.normal.x)*(in.normal.x)))))))));
          float r580=(r527-(fma((buffer6[(57u)*1+0]),((r572-(min((max((fma((((in.normal.z)*r557)),(buffer6[(30u)*1+0]),(fma((((in.normal.y)*r557)),(buffer6[(29u)*1+0]),(((((in.normal.x)*r557))*(buffer6[(28u)*1+0]))))))),r570)),r572)))),(buffer6[(56u)*1+0]))));
          float r582=buffer6[(60u)*1+0];
          long r583=long(r582);
          float r585=buffer6[(61u)*1+0];
          long r586=long(r585);
          long r589=long((buffer6[(59u)*1+0]));
          long r592=long(floor(((r529*(trunc(r582))))));
          long r593=r592;
          long r596=long(floor(((r532*(trunc(r585))))));
          long r597=r596;
          long r601=as_type<long>(ulong((as_type<long>(ulong(r589)*ulong(long(r598)))))+ulong(long(r600)));
          bool over603=r601>long(r602) && ulong(r601-long(r602))>4096uL;
          sb|=over603?1u:0u;
          r604=r570;
          for(r603=long(r602);r603<(over603?long(r602):r601);r603++){
  uint r605=0u;
  int r606=0;
  float r607=0,r661=0.0f;
  if(r30==0.0f) {
    if(r184==0.0f) {
      if(r475!=0.0f) {
        if(r552==0.0f) {
          bool over606=r601>long(r605) && ulong(r601-long(r605))>4096uL;
          sb|=over606?1u:0u;
          r607=r604;
          for(r606=long(r605);r606<(over606?long(r605):r601);r606++){
  uint r612=0u;
  float r619=0.0f,r624=0.0f,r631=as_type<float>(0x437f0000u),r642=as_type<float>(0x43800000u),r647=0.0f,r652=0.0f,r654=0.0f,r657=0.0f,r659=0.0f;
  if(r30==0.0f) {
    if(r184==0.0f) {
      if(r475!=0.0f) {
        if(r552==0.0f) {
          long r608=as_type<long>(ulong(r606)-ulong(r589));
          long r609=as_type<long>(ulong(r593)+ulong(r608));
          long r610=as_type<long>(ulong(r603)-ulong(r589));
          long r611=as_type<long>(ulong(r597)+ulong(r610));
          r619=(((((r609<long(r612)?1.0f:0.0f))!=0.0f||((r611<long(r612)?1.0f:0.0f))!=0.0f))!=0.0f||((((r609>=r583?1.0f:0.0f))!=0.0f||((r611>=r586?1.0f:0.0f))!=0.0f))!=0.0f);
          if(r619==0.0f) {
            r624=((buffer6[(39u)*1+0])>(as_type<float>(0x3fc00000u)));
            if(r624!=0.0f) {
              float r625=float(r609);
              float r626=float(r611);
              float r632=((texture13.read(uint2(r625,r626)).x)*r631);
              long r633=long(round(r632));
              float r635=((texture13.read(uint2(r625,r626)).y)*r631);
              long r636=long(round(r635));
              float r637=round(r635);
              float r638=((texture13.read(uint2(r625,r626)).z)*r631);
              r647=((as_type<float>(0x4b7fffffu))==0.0f?0.0f:(((fma((round(r632)),(as_type<float>(0x47800000u)),(float((r636*256L)))))+(round(r638))))/(as_type<float>(0x4b7fffffu)));
            }
            if(r624==0.0f) {
              uint r649=(((uint(r611)*uint(r583)))+uint(r609));
              r652=buffer6[(((64u)+r649))*1+0];
            }
            r654=(r580<=((r624!=0.0f?r647:r652)));
            r657=(r654!=0.0f?(as_type<float>(0x3f800000u)):(as_type<float>(0x0u)));
          }
          r659=(r607+((r619!=0.0f?(as_type<float>(0x3f800000u)):r657)));
        }
      }
    }
  }
  float next606_0=r659;
  r607=next606_0;
          }
          r661=r607;
        }
      }
    }
  }
  float next603_0=r661;
  r604=next603_0;
          }
          float r665=buffer6[(58u)*1+0];
          r671=(((r572-r665))+(((float((as_type<long>(ulong(r601)*ulong(r601)))))==0.0f?0.0f:((r665*(r604)))/(float((as_type<long>(ulong(r601)*ulong(r601))))))));
        }
        float r672=(r552!=0.0f?(as_type<float>(0x3f800000u)):r671);
        float r673=(r463+r228);
        float r674=(r465+r229);
        float r675=(r467+r230);
        float r679=rsqrt(fabs((fma(r675,r675,(fma(r674,r674,((r673*r673))))))));
        float r680=(r673*r679);
        float r681=(r674*r679);
        float r682=(r675*r679);
        float r687=max((fma(r214,r467,(fma(r213,r465,((r212*r463)))))),r686);
        float r691=max((fma(r214,r230,(fma(r213,r229,((r212*r228)))))),r686);
        float r695=max((fma(r214,r682,(fma(r213,r681,((r212*r680)))))),r536);
        float r704=fma(((r476-r251)),(pow(fabs(((r476-(max((fma(r230,r682,(fma(r229,r681,((r228*r680)))))),r686))))),(as_type<float>(0x40a00000u)))),r251);
        float r707=fma(((r695*r695)),((r249-r476)),r476);
        float r714=(r476-r249);
        float r726=(((r284*((((((r708*r707))*r707))==0.0f?0.0f:r249/((((r708*r707))*r707))))))*(((max(((((r687*(sqrt(fabs((fma(((r691*r691)),r714,r249)))))))+((r691*(sqrt(fabs((fma(((r687*r687)),r714,r249))))))))),(as_type<float>(0x358637bdu))))==0.0f?0.0f:r528/(max(((((r687*(sqrt(fabs((fma(((r691*r691)),r714,r249)))))))+((r691*(sqrt(fabs((fma(((r687*r687)),r714,r249))))))))),(as_type<float>(0x358637bdu)))))));
        float r727=(r476-r704);
        r749=fma((((((fma(r726,r704,((r708==0.0f?0.0f:((r241*r727))/r708))))*(buffer6[(32u)*1+0])))*r470)),r672,r459);
        r750=fma((((((fma(r726,r704,((r708==0.0f?0.0f:((r242*r727))/r708))))*(buffer6[(33u)*1+0])))*r470)),r672,r460);
        r751=fma((((((fma(r726,r704,((r708==0.0f?0.0f:((r243*r727))/r708))))*(buffer6[(34u)*1+0])))*r470)),r672,r461);
      }
      long r757=long((buffer2[(73u)*1+0]));
      long r759=(((r757<long(r447)?1.0f:0.0f))!=0.0f?r757:long(r447));
      bool over760=r759>long(r332) && ulong(r759-long(r332))>4096uL;
      sb|=over760?1u:0u;
      r761=((r475!=0.0f?r749:r459));
      r762=((r475!=0.0f?r750:r460));
      r763=((r475!=0.0f?r751:r461));
      for(r760=long(r332);r760<(over760?long(r332):r759);r760++){
  uint r764=20u;
  float r770=0.0f;
  uint r772=8u,r775=1u,r780=2u;
  float r784=0.0f,r785=as_type<float>(0xbf800000u),r793=0.0f,r794=0.0f,r795=0.0f,r797=0.0f,r805=as_type<float>(0x0u),r806=0.0f,r807=0.0f,r808=0.0f,r809=0.0f,r810=as_type<float>(0x0u);
  float r811=0.0f,r812=0.0f,r813=0.0f,r814=as_type<float>(0x3fc00000u);
  uint r816=9u,r820=2u,r824=1u;
  float r830=0.0f,r831=0.0f,r834=as_type<float>(0xbf800000u),r854=0.0f;
  uint r855=13u;
  float r858=0.0f;
  uint r862=14u;
  float r865=0.0f,r867=0.0f,r868=0.0f;
  uint r869=9u,r872=1u,r875=2u;
  float r887=0.0f,r888=as_type<float>(0x0u),r890=as_type<float>(0x0u),r895=as_type<float>(0xbf800000u);
  uint r921=14u;
  float r931=as_type<float>(0x0u);
  uint r932=0u;
  int r933=0;
  float r934=0,r935=0,r936=0,r937=0,r1024=0.0f,r1029=0.0f,r1034=0.0f,r1035=0.0f,r1036=0.0f,r1037=as_type<float>(0x0u),r1038=0.0f,r1039=0.0f;
  float r1040=0.0f,r1041=0.0f,r1042=0.0f,r1043=0.0f,r1044=0.0f,r1045=0.0f,r1046=0.0f,r1047=0.0f,r1048=0.0f;
  uint r1049=9u,r1052=1u,r1055=2u,r1058=12u,r1065=15u,r1072=8u;
  float r1081=as_type<float>(0x0u);
  uint r1082=0u,r1083=0;
  float r1084=0,r1085=0,r1086=0,r1087=0,r1148=0.0f,r1153=0.0f,r1158=0.0f,r1159=0.0f,r1160=0.0f,r1161=as_type<float>(0x0u),r1162=0.0f,r1163=0.0f;
  float r1164=0.0f,r1172=as_type<float>(0x0u),r1175=0.0f,r1189=as_type<float>(0x38d1b717u),r1204=as_type<float>(0x3f800000u),r1213=as_type<float>(0x40490fdbu);
  uint r1243=4u,r1246=1u,r1249=2u;
  float r1261=0.0f,r1262=0.0f,r1263=0.0f,r1264=0.0f,r1265=0.0f,r1266=0.0f;
  if(r30==0.0f) {
    if(r184==0.0f) {
      uint r765=(uint(r760)*r764);
      uint r767=((84u)+r765);
      r770=((buffer2[r767*1+0])<(as_type<float>(0x40600000u)));
      if(r770!=0.0f) {
        float r771=buffer2[r767*1+0];
        float r774=buffer2[((r767+r772))*1+0];
        uint r776=(r767+r775);
        float r777=buffer2[r776*1+0];
        float r779=buffer2[((r776+r775))*1+0];
        float r782=buffer2[((r776+r780))*1+0];
        r784=(r771<(as_type<float>(0x3f000000u)));
        if(r784!=0.0f) {
          float r786=(r777*r785);
          float r787=(r779*r785);
          float r788=(r782*r785);
          float r792=rsqrt(fabs((fma(r788,r788,(fma(r787,r787,((r786*r786))))))));
          r793=(r786*r792);
          r794=(r787*r792);
          r795=(r788*r792);
        }
        if(r784==0.0f) {
          r797=(r771<(as_type<float>(0x40200000u)));
          if(r797!=0.0f) {
            float r798=(r777-(in.world.x));
            float r799=(r779-(in.world.y));
            float r800=(r782-(in.world.z));
            float r804=sqrt(fabs((fma(r800,r800,(fma(r799,r799,((r798*r798))))))));
            r806=(r804>r805);
            if(r806!=0.0f) {
              r807=(r804==0.0f?0.0f:r798/r804);
              r808=(r804==0.0f?0.0f:r799/r804);
              r809=(r804==0.0f?0.0f:r800/r804);
            }
            r811=(r806!=0.0f?r807:r810);
            r812=(r806!=0.0f?r808:r810);
            r813=(r806!=0.0f?r809:r810);
            uint r818=(((r771<r814))!=0.0f?r816:(15u));
            uint r819=(r767+r818);
            float r829=fma((((buffer2[((r819+r820))*1+0])*r804)),r804,(fma((buffer2[((r819+r824))*1+0]),r804,(buffer2[r819*1+0]))));
            r830=(r829>r805);
            if(r830!=0.0f) {
              r831=(r829==0.0f?0.0f:r774/r829);
            }
            float r833=(r830!=0.0f?r831:(as_type<float>(0x0u)));
            uint r838=(r767+r816);
            float r839=buffer2[r838*1+0];
            float r841=buffer2[((r838+r824))*1+0];
            float r843=buffer2[((r838+r820))*1+0];
            float r847=rsqrt(fabs((fma(r843,r843,(fma(r841,r841,((r839*r839))))))));
            float r853=fma(((r813*r834)),((r843*r847)),(fma(((r812*r834)),((r841*r847)),((((r811*r834))*((r839*r847)))))));
            r854=(r771>=r814);
            if(r854!=0.0f) {
              r858=(r853<(buffer2[((r767+r855))*1+0]));
              if(r858==0.0f) {
                r865=pow(fabs((max(r853,(as_type<float>(0x0u))))),(buffer2[((r767+r862))*1+0]));
              }
              r867=(r833*((r858!=0.0f?(as_type<float>(0x0u)):r865)));
            }
            r868=(r854!=0.0f?r867:r833);
          }
          if(r797==0.0f) {
            uint r870=(r767+r869);
            float r871=buffer2[r870*1+0];
            float r874=buffer2[((r870+r872))*1+0];
            float r877=buffer2[((r870+r875))*1+0];
            float r881=rsqrt(fabs((fma(r877,r877,(fma(r874,r874,((r871*r871))))))));
            float r882=(r871*r881);
            float r883=(r874*r881);
            float r884=(r877*r881);
            r887=((fabs(r884))<(as_type<float>(0x3f7fbe77u)));
            float r892=(r887!=0.0f?r888:r890);
            float r893=(r887!=0.0f?r888:(as_type<float>(0x3f800000u)));
            float r894=(r887!=0.0f?(as_type<float>(0x3f800000u)):r890);
            float r898=fma(r893,r884,((r895*((r894*r883)))));
            float r901=fma(r894,r882,((r895*((r892*r884)))));
            float r904=fma(r892,r883,((r895*((r893*r882)))));
            float r908=rsqrt(fabs((fma(r904,r904,(fma(r901,r901,((r898*r898))))))));
            float r909=(r898*r908);
            float r910=(r901*r908);
            float r911=(r904*r908);
            float r914=fma(r883,r911,((r895*((r884*r910)))));
            float r917=fma(r884,r909,((r895*((r882*r911)))));
            float r920=fma(r882,r910,((r895*((r883*r909)))));
            long r925=long(round((sqrt(fabs((buffer2[((r767+r921))*1+0]))))));
            long r926=r925;
            long r928=(((r926<long(r872)?1.0f:0.0f))!=0.0f?long(r872):r926);
            long r929=as_type<long>(ulong(r928)*ulong(r928));
            float r930=float(r929);
            bool over933=r928>long(r932) && ulong(r928-long(r932))>4096uL;
            sb|=over933?1u:0u;
            r934=r931;
            r935=r931;
            r936=r931;
            r937=r931;
            for(r933=long(r932);r933<(over933?long(r932):r928);r933++){
  uint r938=0u;
  int r939=0;
  float r940=0,r941=0,r942=0,r943=0,r1016=0.0f,r1017=0.0f,r1018=0.0f,r1019=0.0f;
  if(r30==0.0f) {
    if(r184==0.0f) {
      if(r770!=0.0f) {
        if(r784==0.0f) {
          if(r797==0.0f) {
            bool over939=r928>long(r938) && ulong(r928-long(r938))>4096uL;
            sb|=over939?1u:0u;
            r940=r934;
            r941=r935;
            r942=r936;
            r943=r937;
            for(r939=long(r938);r939<(over939?long(r938):r928);r939++){
  float r945=as_type<float>(0x3f000000u);
  uint r954=12u,r958=13u;
  float r976=0.0f,r980=as_type<float>(0xbf800000u),r987=as_type<float>(0x0u);
  uint r989=15u,r991=2u,r995=1u;
  float r1001=0.0f,r1004=0.0f,r1007=0.0f,r1008=0.0f,r1009=0.0f,r1010=0.0f,r1011=0.0f,r1012=0.0f,r1013=0.0f,r1014=0.0f;
  if(r30==0.0f) {
    if(r184==0.0f) {
      if(r770!=0.0f) {
        if(r784==0.0f) {
          if(r797==0.0f) {
            float r947=float(r928);
            float r957=(((((r947==0.0f?0.0f:(((float(r939))+r945))/r947))-r945))*(buffer2[((r767+r954))*1+0]));
            float r961=(((((r947==0.0f?0.0f:(((float(r933))+r945))/r947))-r945))*(buffer2[((r767+r958))*1+0]));
            float r968=((fma(r961,r914,(fma(r957,r909,r777))))-(in.world.x));
            float r969=((fma(r961,r917,(fma(r957,r910,r779))))-(in.world.y));
            float r970=((fma(r961,r920,(fma(r957,r911,r782))))-(in.world.z));
            float r974=sqrt(fabs((fma(r970,r970,(fma(r969,r969,((r968*r968))))))));
            r976=(r974>(as_type<float>(0x0u)));
            if(r976!=0.0f) {
              float r977=(r974==0.0f?0.0f:r968/r974);
              float r978=(r974==0.0f?0.0f:r969/r974);
              float r979=(r974==0.0f?0.0f:r970/r974);
              uint r990=(r767+r989);
              float r1000=fma((((buffer2[((r990+r991))*1+0])*r974)),r974,(fma((buffer2[((r990+r995))*1+0]),r974,(buffer2[r990*1+0]))));
              r1001=(r1000>r987);
              if(r1001!=0.0f) {
                r1004=(((r930*r1000))==0.0f?0.0f:((r774*(max((fma(r884,((r979*r980)),(fma(r883,((r978*r980)),((r882*((r977*r980)))))))),r987))))/((r930*r1000)));
              }
              float r1006=(r1001!=0.0f?r1004:(as_type<float>(0x0u)));
              r1007=fma(r977,r1006,r940);
              r1008=fma(r978,r1006,r941);
              r1009=fma(r979,r1006,r942);
              r1010=(r943+r1006);
            }
            r1011=(r976!=0.0f?r1007:r940);
            r1012=(r976!=0.0f?r1008:r941);
            r1013=(r976!=0.0f?r1009:r942);
            r1014=(r976!=0.0f?r1010:r943);
          }
        }
      }
    }
  }
  float next939_0=r1011;
  float next939_1=r1012;
  float next939_2=r1013;
  float next939_3=r1014;
  r940=next939_0;
  r941=next939_1;
  r942=next939_2;
  r943=next939_3;
            }
            r1016=r940;
            r1017=r941;
            r1018=r942;
            r1019=r943;
          }
        }
      }
    }
  }
  float next933_0=r1016;
  float next933_1=r1017;
  float next933_2=r1018;
  float next933_3=r1019;
  r934=next933_0;
  r935=next933_1;
  r936=next933_2;
  r937=next933_3;
            }
            float r1021=r934;
            float r1022=r935;
            float r1023=r936;
            r1024=r937;
            r1029=((sqrt(fabs((fma(r1023,r1023,(fma(r1022,r1022,((r1021*r1021)))))))))>r931);
            if(r1029!=0.0f) {
              float r1033=rsqrt(fabs((fma(r1023,r1023,(fma(r1022,r1022,((r1021*r1021))))))));
              r1034=(r1021*r1033);
              r1035=(r1022*r1033);
              r1036=(r1023*r1033);
            }
            r1038=(r1029!=0.0f?r1034:r1037);
            r1039=(r1029!=0.0f?r1035:r1037);
            r1040=(r1029!=0.0f?r1036:r1037);
          }
          r1041=(r797!=0.0f?r811:r1038);
          r1042=(r797!=0.0f?r812:r1039);
          r1043=(r797!=0.0f?r813:r1040);
          r1044=(r797!=0.0f?r868:r1024);
        }
        r1045=(r784!=0.0f?r793:r1041);
        r1046=(r784!=0.0f?r794:r1042);
        r1047=(r784!=0.0f?r795:r1043);
        r1048=(r784!=0.0f?r774:r1044);
      }
      if(r770==0.0f) {
        uint r1050=(r767+r1049);
        float r1051=buffer2[r1050*1+0];
        float r1054=buffer2[((r1050+r1052))*1+0];
        float r1057=buffer2[((r1050+r1055))*1+0];
        uint r1059=(r767+r1058);
        float r1060=buffer2[r1059*1+0];
        float r1062=buffer2[((r1059+r1052))*1+0];
        float r1064=buffer2[((r1059+r1055))*1+0];
        uint r1066=(r767+r1065);
        float r1067=buffer2[r1066*1+0];
        float r1069=buffer2[((r1066+r1052))*1+0];
        float r1071=buffer2[((r1066+r1055))*1+0];
        float r1074=buffer2[((r767+r1072))*1+0];
        uint r1075=(r767+r1052);
        float r1076=buffer2[r1075*1+0];
        float r1078=buffer2[((r1075+r1052))*1+0];
        float r1080=buffer2[((r1075+r1055))*1+0];
        bool over1083=long(r1055)>long(r1082) && ulong(long(r1055)-long(r1082))>4096uL;
        sb|=over1083?1u:0u;
        r1084=r1081;
        r1085=r1081;
        r1086=r1081;
        r1087=r1081;
        for(r1083=r1082;r1083<(over1083?r1082:r1055);r1083++){
  uint r1088=0u,r1089=2u,r1090=0;
  float r1091=0,r1092=0,r1093=0,r1094=0,r1140=0.0f,r1141=0.0f,r1142=0.0f,r1143=0.0f;
  if(r30==0.0f) {
    if(r184==0.0f) {
      if(r770==0.0f) {
        bool over1090=long(r1089)>long(r1088) && ulong(long(r1089)-long(r1088))>4096uL;
        sb|=over1090?1u:0u;
        r1091=r1084;
        r1092=r1085;
        r1093=r1086;
        r1094=r1087;
        for(r1090=r1088;r1090<(over1090?r1088:r1089);r1090++){
  float r1096=as_type<float>(0x3f000000u),r1113=0.0f,r1118=as_type<float>(0xbf800000u),r1131=0.0f,r1132=0.0f,r1133=0.0f,r1134=0.0f,r1135=0.0f,r1136=0.0f,r1137=0.0f,r1138=0.0f;
  if(r30==0.0f) {
    if(r184==0.0f) {
      if(r770==0.0f) {
        float r1097=((float(r1090))-r1096);
        float r1099=((float(r1083))-r1096);
        float r1106=((fma(r1099,r1060,(fma(r1097,r1051,r1076))))-(in.world.x));
        float r1107=((fma(r1099,r1062,(fma(r1097,r1054,r1078))))-(in.world.y));
        float r1108=((fma(r1099,r1064,(fma(r1097,r1057,r1080))))-(in.world.z));
        float r1111=fma(r1108,r1108,(fma(r1107,r1107,((r1106*r1106)))));
        r1113=(r1111>(as_type<float>(0x0u)));
        if(r1113!=0.0f) {
          float r1114=rsqrt(fabs(r1111));
          float r1115=(r1106*r1114);
          float r1116=(r1107*r1114);
          float r1117=(r1108*r1114);
          float r1130=((((as_type<float>(0x40800000u))*r1111))==0.0f?0.0f:((r1074*(max((fma(r1071,((r1117*r1118)),(fma(r1069,((r1116*r1118)),((r1067*((r1115*r1118)))))))),(as_type<float>(0x0u))))))/(((as_type<float>(0x40800000u))*r1111)));
          r1131=fma(r1115,r1130,r1091);
          r1132=fma(r1116,r1130,r1092);
          r1133=fma(r1117,r1130,r1093);
          r1134=(r1094+r1130);
        }
        r1135=(r1113!=0.0f?r1131:r1091);
        r1136=(r1113!=0.0f?r1132:r1092);
        r1137=(r1113!=0.0f?r1133:r1093);
        r1138=(r1113!=0.0f?r1134:r1094);
      }
    }
  }
  float next1090_0=r1135;
  float next1090_1=r1136;
  float next1090_2=r1137;
  float next1090_3=r1138;
  r1091=next1090_0;
  r1092=next1090_1;
  r1093=next1090_2;
  r1094=next1090_3;
        }
        r1140=r1091;
        r1141=r1092;
        r1142=r1093;
        r1143=r1094;
      }
    }
  }
  float next1083_0=r1140;
  float next1083_1=r1141;
  float next1083_2=r1142;
  float next1083_3=r1143;
  r1084=next1083_0;
  r1085=next1083_1;
  r1086=next1083_2;
  r1087=next1083_3;
        }
        float r1145=r1084;
        float r1146=r1085;
        float r1147=r1086;
        r1148=r1087;
        r1153=((sqrt(fabs((fma(r1147,r1147,(fma(r1146,r1146,((r1145*r1145)))))))))>r1081);
        if(r1153!=0.0f) {
          float r1157=rsqrt(fabs((fma(r1147,r1147,(fma(r1146,r1146,((r1145*r1145))))))));
          r1158=(r1145*r1157);
          r1159=(r1146*r1157);
          r1160=(r1147*r1157);
        }
        r1162=(r1153!=0.0f?r1158:r1161);
        r1163=(r1153!=0.0f?r1159:r1161);
        r1164=(r1153!=0.0f?r1160:r1161);
      }
      float r1165=(r770!=0.0f?r1045:r1162);
      float r1166=(r770!=0.0f?r1046:r1163);
      float r1167=(r770!=0.0f?r1047:r1164);
      float r1168=(r770!=0.0f?r1048:r1148);
      float r1171=fma(r214,r1167,(fma(r213,r1166,((r212*r1165)))));
      r1175=(((r1171>r1172))!=0.0f&&((r1168>r1172))!=0.0f);
      if(r1175!=0.0f) {
        float r1176=(r1165+r228);
        float r1177=(r1166+r229);
        float r1178=(r1167+r230);
        float r1182=rsqrt(fabs((fma(r1178,r1178,(fma(r1177,r1177,((r1176*r1176))))))));
        float r1183=(r1176*r1182);
        float r1184=(r1177*r1182);
        float r1185=(r1178*r1182);
        float r1190=max((fma(r214,r1167,(fma(r213,r1166,((r212*r1165)))))),r1189);
        float r1194=max((fma(r214,r230,(fma(r213,r229,((r212*r228)))))),r1189);
        float r1199=max((fma(r214,r1185,(fma(r213,r1184,((r212*r1183)))))),(as_type<float>(0x0u)));
        float r1209=fma(((r1204-r251)),(pow(fabs(((r1204-(max((fma(r230,r1185,(fma(r229,r1184,((r228*r1183)))))),r1189))))),(as_type<float>(0x40a00000u)))),r251);
        float r1212=fma(((r1199*r1199)),((r249-r1204)),r1204);
        float r1219=(r1204-r249);
        float r1232=(((r284*((((((r1213*r1212))*r1212))==0.0f?0.0f:r249/((((r1213*r1212))*r1212))))))*(((max(((((r1190*(sqrt(fabs((fma(((r1194*r1194)),r1219,r249)))))))+((r1194*(sqrt(fabs((fma(((r1190*r1190)),r1219,r249))))))))),(as_type<float>(0x358637bdu))))==0.0f?0.0f:(as_type<float>(0x3f000000u))/(max(((((r1190*(sqrt(fabs((fma(((r1194*r1194)),r1219,r249)))))))+((r1194*(sqrt(fabs((fma(((r1190*r1190)),r1219,r249))))))))),(as_type<float>(0x358637bdu)))))));
        float r1233=(r1204-r1209);
        uint r1244=(r767+r1243);
        r1261=fma((((((((fma(r1232,r1209,((r1213==0.0f?0.0f:((r241*r1233))/r1213))))*(buffer2[r1244*1+0])))*r1168))*r1213)),r1171,r761);
        r1262=fma((((((((fma(r1232,r1209,((r1213==0.0f?0.0f:((r242*r1233))/r1213))))*(buffer2[((r1244+r1246))*1+0])))*r1168))*r1213)),r1171,r762);
        r1263=fma((((((((fma(r1232,r1209,((r1213==0.0f?0.0f:((r243*r1233))/r1213))))*(buffer2[((r1244+r1249))*1+0])))*r1168))*r1213)),r1171,r763);
      }
      r1264=(r1175!=0.0f?r1261:r761);
      r1265=(r1175!=0.0f?r1262:r762);
      r1266=(r1175!=0.0f?r1263:r763);
    }
  }
  float next760_0=r1264;
  float next760_1=r1265;
  float next760_2=r1266;
  r761=next760_0;
  r762=next760_1;
  r763=next760_2;
      }
      float r1271=((r761)*r15);
      float r1272=((r762)*r15);
      float r1273=((r763)*r15);
      float r1304=fma((pow(fabs((min((max((((fma(r1271,(fma(r1281,r1271,r1282)),r1286))==0.0f?0.0f:((r1271*(fma(r1274,r1271,r246))))/(fma(r1271,(fma(r1281,r1271,r1282)),r1286)))),r282)),r258))),r1299)),r1303,r203);
      long r1305=long(r1304);
      r1307=(r1303==0.0f?0.0f:(trunc(r1304))/r1303);
      float r1308=fma((pow(fabs((min((max((((fma(r1272,(fma(r1281,r1272,r1282)),r1286))==0.0f?0.0f:((r1272*(fma(r1274,r1272,r246))))/(fma(r1272,(fma(r1281,r1272,r1282)),r1286)))),r282)),r258))),r1299)),r1303,r203);
      long r1309=long(r1308);
      r1311=(r1303==0.0f?0.0f:(trunc(r1308))/r1303);
      float r1312=fma((pow(fabs((min((max((((fma(r1273,(fma(r1281,r1273,r1282)),r1286))==0.0f?0.0f:((r1273*(fma(r1274,r1273,r246))))/(fma(r1273,(fma(r1281,r1273,r1282)),r1286)))),r282)),r258))),r1299)),r1303,r203);
      long r1313=long(r1312);
      r1315=(r1303==0.0f?0.0f:(trunc(r1312))/r1303);
    }
    r1316=(r184!=0.0f?r191:r1307);
    r1317=(r184!=0.0f?r192:r1311);
    r1318=(r184!=0.0f?r193:r1315);
    r1319=(((((buffer2[(59u)*1+0])*(in.color.w)))*(texture7.sample(sampler8,float2((in.uv.x),(in.uv.y))).w)));
  }
  float r1320=(r30!=0.0f?r172:r1316);
  float r1321=(r30!=0.0f?r176:r1317);
  float r1322=(r30!=0.0f?r180:r1318);
  float r1323=(r30!=0.0f?r32:r1319);
  colour.x=r1320;
  colour.y=r1321;
  colour.z=r1322;
  colour.w=r1323;
  return colour;
}
