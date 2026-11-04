#include <metal_stdlib>
using namespace metal;
kernel void triangle_indices(
    device const float* buffer0 [[buffer(0)]],
    device const uint* buffer1 [[buffer(1)]],
    device const uint* buffer2 [[buffer(2)]],
    device const uint* buffer3 [[buffer(3)]],
    device uint* buffer4 [[buffer(4)]],
    device uint* buffer5 [[buffer(5)]],
    device uint* buffer6 [[buffer(6)]],
    device const uint* buffer7 [[buffer(7)]],
    uint gid [[thread_position_in_grid]],
    uint lid [[thread_position_in_threadgroup]]) {
  uint sb=0u;
  uint r9=256u,r16=255u,r21=0u;
  float r23=0.0f;
  uint r24=0u,r26=1u,r28=2u,r36=3u;
  float r41=0.0f,r43=0.0f,r46=0.0f,r47=0.0f,r67=0.0f;
  uint r69=1u,r72=2u;
  float r76=0.0f;
  uint r77=3u;
  long r79=0;
  float r87=0.0f,r89=0.0f;
  uint r91=1u,r94=2u,r99=3u,r100=0u,r101=3u,r104=1u,r107=2u;
  float r206=0.0f;
  uint r210=0u,r211=0u;
  float r213=0.0f,r215=0.0f,r217=0.0f,r219=0.0f,r221=0.0f,r222=0.0f,r224=0.0f,r225=0.0f,r228=0.0f,r229=0.0f,r232=0.0f,r233=0.0f;
  float r236=0.0f,r237=0.0f,r256=0.0f,r266=0.0f,r277=0.0f,r278=0.0f,r309=0.0f;
  long r317=0,r319=0,r321=0;
  uint r322=0u;
  float r324=0.0f,r335=0.0f,r336=0.0f,r367=0.0f;
  long r375=0,r377=0,r379=0,r381=0,r382=long(-1L);
  float r384=0.0f,r395=0.0f,r396=0.0f,r427=0.0f;
  long r435=0,r437=0,r439=0,r441=0;
  float r443=0.0f,r454=0.0f,r455=0.0f,r486=0.0f;
  long r494=0,r496=0,r498=0,r500=0;
  uint r502=1u;
  float r503=0.0f;
  uint r504=1u;
  float r505=0.0f,r507=0.0f;
  long r512=0,r514=0,r515=0;
  float r517=0.0f,r519=0.0f;
  uint r521=1u,r524=2u,r528=0u;
  float r530=0.0f;
  uint r532=1u,r535=2u,r539=0u;
  float r541=0.0f;
  uint r543=1u,r546=2u,r550=0u,r552=1u,r555=2u,r559=0u,r560=0u,r561=0u,r563=3u,r571=0u,r578=0u,r585=0u;
  uint r592=0u,r593=0u,r594=0u,r597=6u,r598=0u,r599=0u,r600=0u,r601=0u,r602=0u,r603=0u,r605=0u,r606=0u;
  int r607=0;
  uint r608=0,r618=0u,scan619_before=0u,scan619_total=0u;
  float r624=0.0f;
  uint r625=0u,r627=0u,r628=0u;
  long r0=long(gid);
  long r11=long(buffer7[(2u)*1+0]);
  long r13=long(buffer7[(3u)*1+0]);
  long r14=(as_type<long>(ulong((long(r9)==0?0:r0/long(r9)))<<1)>>1);
  sb|=(r9==0)?2u:0u;
  long r15=(as_type<long>(ulong((long(r9)==0?0:r0%long(r9)))<<1)>>1);
  sb|=(r9==0)?2u:0u;
  long r18=(as_type<long>(ulong((long(r9)==0?0:((as_type<long>(ulong((ulong(r11)+ulong(long(r16))))<<1)>>1))/long(r9)))<<1)>>1);
  sb|=(r9==0)?2u:0u;
  long r20=(as_type<long>(ulong((long(r9)==0?0:((as_type<long>(ulong((ulong(r18)+ulong(long(r16))))<<1)>>1))/long(r9)))<<1)>>1);
  sb|=(r9==0)?2u:0u;
  float r22=(r13==long(r21)?1.0f:0.0f);
  if(r22!=0.0f) {
    r23=(r0<r11?1.0f:0.0f);
    if(r23!=0.0f) {
      long r25=long(buffer7[r24*1+0]);
      long r27=long(buffer7[r26*1+0]);
      long r29=long(buffer7[r28*1+0]);
      long r30=long(buffer2[r0*1+0]);
      long r31=(as_type<long>(ulong((ulong(r0)+ulong(long(r26))))<<1)>>1);
      long r32=long(buffer2[r31*1+0]);
      long r33=(as_type<long>(ulong((ulong(r32)-ulong(r30)))<<1)>>1);
      long r34=(as_type<long>(ulong((ulong(r0)*ulong(long(r28))))<<1)>>1);
      long r37=(as_type<long>(ulong((ulong(((as_type<long>(ulong((ulong(r30)-ulong(r34)))<<1)>>1)))*ulong(long(r36))))<<1)>>1);
      long r38=(as_type<long>(ulong((ulong(r29)*ulong(long(r28))))<<1)>>1);
      r41=(r0==long(r24)?1.0f:0.0f);
      if(r41!=0.0f) {
        r43=(r30==long((0u))?1.0f:0.0f);
      }
      r46=(r31==r29?1.0f:0.0f);
      if(r46!=0.0f) {
        r47=(r32==r27?1.0f:0.0f);
      }
      long r60=(as_type<long>(ulong((ulong(((as_type<long>(ulong((ulong(r33)-ulong(long(r28))))<<1)>>1)))*ulong(long(r36))))<<1)>>1);
      r67=(((((r41!=0.0f?r43:(as_type<float>(0x3f800000u))))!=0.0f&&((r46!=0.0f?r47:(as_type<float>(0x3f800000u))))!=0.0f))!=0.0f&&(((((long(buffer3[r0*1+0]))==long(r24)?1.0f:0.0f))!=0.0f&&((((((r33==long(r36)?1.0f:0.0f))!=0.0f||((r33==long((4u))?1.0f:0.0f))!=0.0f))!=0.0f&&((((r30>=r34?1.0f:0.0f))!=0.0f&&((((r32<=r27?1.0f:0.0f))!=0.0f&&((((as_type<long>(ulong((ulong(r37)+ulong(r60)))<<1)>>1))<=((as_type<long>(ulong((ulong(((as_type<long>(ulong((ulong(r27)-ulong(r38)))<<1)>>1)))*ulong(long(r36))))<<1)>>1))?1.0f:0.0f))!=0.0f))!=0.0f))!=0.0f))!=0.0f))!=0.0f);
      if(r67!=0.0f) {
        long r68=long(buffer1[r30*1+0]);
        long r71=long(buffer1[((uint(r30)+r69))*1+0]);
        long r74=long(buffer1[((uint(r30)+r72))*1+0]);
        r76=(r33==long((4u))?1.0f:0.0f);
        if(r76!=0.0f) {
          r79=long(buffer1[((uint(r30)+r77))*1+0]);
        }
        long r80=(r76!=0.0f?r79:r74);
        r87=(((r68<r25?1.0f:0.0f))!=0.0f&&((((r71<r25?1.0f:0.0f))!=0.0f&&((((r74<r25?1.0f:0.0f))!=0.0f&&((r80<r25?1.0f:0.0f))!=0.0f))!=0.0f))!=0.0f);
        if(r87!=0.0f) {
          r89=(r33==long((3u))?1.0f:0.0f);
          if(r89!=0.0f) {
            uint r90=(buffer4[uint(r37)]=uint(r68),1u);
            uint r93=(buffer4[((uint(r37)+r91))]=uint(r71),1u);
            uint r96=(buffer4[((uint(r37)+r94))]=uint(r74),1u);
            uint r97=(r93+r96);
            r100=(((r90+r97))-r99);
          }
          if(r89==0.0f) {
            uint r102=(uint(r68)*r101);
            float r103=buffer0[r102*1+0];
            float r106=buffer0[((r102+r104))*1+0];
            float r109=buffer0[((r102+r107))*1+0];
            uint r110=(uint(r71)*r101);
            float r111=buffer0[r110*1+0];
            float r113=buffer0[((r110+r104))*1+0];
            float r115=buffer0[((r110+r107))*1+0];
            uint r116=(uint(r74)*r101);
            float r117=buffer0[r116*1+0];
            float r119=buffer0[((r116+r104))*1+0];
            float r121=buffer0[((r116+r107))*1+0];
            uint r122=(uint(r80)*r101);
            float r123=buffer0[r122*1+0];
            float r125=buffer0[((r122+r104))*1+0];
            float r127=buffer0[((r122+r107))*1+0];
            float r150=max((max((max((fabs(r103)),(max((fabs(r106)),(fabs(r109)))))),(max((fabs(r111)),(max((fabs(r113)),(fabs(r115)))))))),(max((max((fabs(r117)),(max((fabs(r119)),(fabs(r121)))))),(max((fabs(r123)),(max((fabs(r125)),(fabs(r127)))))))));
            float r155=(((((r150*r150))*(as_type<float>(0x358637bdu))))+(as_type<float>(0x2edbe6ffu)));
            float r201=fabs(((((((((((r106-r113))*((r109+r115))))+((((r113-r119))*((r115+r121))))))+((((r119-r125))*((r121+r127))))))+((((r125-r106))*((r127+r109)))))));
            float r202=fabs(((((((((((r109-r115))*((r103+r111))))+((((r115-r121))*((r111+r117))))))+((((r121-r127))*((r117+r123))))))+((((r127-r109))*((r123+r103)))))));
            float r203=fabs(((((((((((r103-r111))*((r106+r113))))+((((r111-r117))*((r113+r119))))))+((((r117-r123))*((r119+r125))))))+((((r123-r103))*((r125+r106)))))));
            r206=(((r201>=r202))!=0.0f&&((r201>=r203))!=0.0f);
            if(r206==0.0f) {
              r210=(((r202>=r203))!=0.0f?(1u):(2u));
            }
            uint r212=(r206!=0.0f?r211:r210);
            r213=(long(r212)==long(r211)?1.0f:0.0f);
            if(r213!=0.0f) {
              r215=(r201-(max(r202,r203)));
            }
            if(r213==0.0f) {
              r217=(long(r212)==long((1u))?1.0f:0.0f);
              if(r217!=0.0f) {
                r219=(r202-(max(r201,r203)));
              }
              if(r217==0.0f) {
                r221=(r203-(max(r201,r202)));
              }
              r222=(r217!=0.0f?r219:r221);
              r224=r103;
              r225=(r217!=0.0f?r109:r106);
            }
            float r226=(r213!=0.0f?r106:r224);
            float r227=(r213!=0.0f?r109:r225);
            if(r213==0.0f) {
              r228=r111;
              r229=(r217!=0.0f?r115:r113);
            }
            float r230=(r213!=0.0f?r113:r228);
            float r231=(r213!=0.0f?r115:r229);
            if(r213==0.0f) {
              r232=r117;
              r233=(r217!=0.0f?r121:r119);
            }
            float r234=(r213!=0.0f?r119:r232);
            float r235=(r213!=0.0f?r121:r233);
            if(r213==0.0f) {
              r236=r123;
              r237=(r217!=0.0f?r127:r125);
            }
            float r238=(r213!=0.0f?r125:r236);
            float r239=(r213!=0.0f?r127:r237);
            float r254=(((((((((r226*r231))-((r230*r227))))+((((r230*r235))-((r234*r231))))))+((((r234*r239))-((r238*r235))))))+((((r238*r227))-((r226*r239)))));
            r256=(r254>(as_type<float>(0x0u)));
            float r259=(r256!=0.0f?(as_type<float>(0x3f800000u)):(as_type<float>(0xbf800000u)));
            r266=(((r150<(as_type<float>(0x5368d4a5u))))!=0.0f&&((((((r213!=0.0f?r215:r222))>r155))!=0.0f&&(((fabs(r254))>r155))!=0.0f))!=0.0f);
            if(r266!=0.0f) {
              float r274=(((((((r226-r238))*((r231-r239))))-((((r227-r239))*((r230-r238))))))*r259);
              r277=(r274<((r155*(as_type<float>(0xbf800000u)))));
              if(r277==0.0f) {
                r278=(r274>r155);
                if(r278!=0.0f) {
                  float r286=(((((((r226-r238))*((r235-r239))))-((((r227-r239))*((r234-r238))))))*r259);
                  float r294=(((((((r230-r226))*((r235-r227))))-((((r231-r227))*((r234-r226))))))*r259);
                  float r302=(((((((r238-r230))*((r235-r231))))-((((r239-r231))*((r234-r230))))))*r259);
                  float r304=(r155*(as_type<float>(0xbf800000u)));
                  r309=(((r286<r304))!=0.0f||((((r294<r304))!=0.0f||((r302<r304))!=0.0f))!=0.0f);
                  if(r309==0.0f) {
                    r317=(((((r286>r155))!=0.0f&&((((r294>r155))!=0.0f&&((r302>r155))!=0.0f))!=0.0f))!=0.0f?long((0u)):(long(-1L)));
                  }
                  r319=(r309!=0.0f?long((1u)):r317);
                }
                r321=(r278!=0.0f?r319:(long(-1L)));
              }
              long r323=(r277!=0.0f?long(r322):r321);
              r324=(r323==long(r322)?1.0f:0.0f);
              if(r324!=0.0f) {
                float r332=(((((((r230-r226))*((r235-r227))))-((((r231-r227))*((r234-r226))))))*r259);
                r335=(r332<((r155*(as_type<float>(0xbf800000u)))));
                if(r335==0.0f) {
                  r336=(r332>r155);
                  if(r336!=0.0f) {
                    float r344=(((((((r230-r226))*((r239-r227))))-((((r231-r227))*((r238-r226))))))*r259);
                    float r352=(((((((r234-r230))*((r239-r231))))-((((r235-r231))*((r238-r230))))))*r259);
                    float r360=(((((((r226-r234))*((r239-r235))))-((((r227-r235))*((r238-r234))))))*r259);
                    float r362=(r155*(as_type<float>(0xbf800000u)));
                    r367=(((r344<r362))!=0.0f||((((r352<r362))!=0.0f||((r360<r362))!=0.0f))!=0.0f);
                    if(r367==0.0f) {
                      r375=(((((r344>r155))!=0.0f&&((((r352>r155))!=0.0f&&((r360>r155))!=0.0f))!=0.0f))!=0.0f?long((0u)):(long(-1L)));
                    }
                    r377=(r367!=0.0f?long((1u)):r375);
                  }
                  r379=(r336!=0.0f?r377:(long(-1L)));
                }
                r381=(r335!=0.0f?long((0u)):r379);
              }
              long r383=(r324!=0.0f?r381:r382);
              r384=(r383==long(r322)?1.0f:0.0f);
              if(r384!=0.0f) {
                float r392=(((((((r234-r230))*((r239-r231))))-((((r235-r231))*((r238-r230))))))*r259);
                r395=(r392<((r155*(as_type<float>(0xbf800000u)))));
                if(r395==0.0f) {
                  r396=(r392>r155);
                  if(r396!=0.0f) {
                    float r404=(((((((r234-r230))*((r227-r231))))-((((r235-r231))*((r226-r230))))))*r259);
                    float r412=(((((((r238-r234))*((r227-r235))))-((((r239-r235))*((r226-r234))))))*r259);
                    float r420=(((((((r230-r238))*((r227-r239))))-((((r231-r239))*((r226-r238))))))*r259);
                    float r422=(r155*(as_type<float>(0xbf800000u)));
                    r427=(((r404<r422))!=0.0f||((((r412<r422))!=0.0f||((r420<r422))!=0.0f))!=0.0f);
                    if(r427==0.0f) {
                      r435=(((((r404>r155))!=0.0f&&((((r412>r155))!=0.0f&&((r420>r155))!=0.0f))!=0.0f))!=0.0f?long((0u)):(long(-1L)));
                    }
                    r437=(r427!=0.0f?long((1u)):r435);
                  }
                  r439=(r396!=0.0f?r437:(long(-1L)));
                }
                r441=(r395!=0.0f?long((0u)):r439);
              }
              long r442=(r384!=0.0f?r441:r382);
              r443=(r442==long(r322)?1.0f:0.0f);
              if(r443!=0.0f) {
                float r451=(((((((r238-r234))*((r227-r235))))-((((r239-r235))*((r226-r234))))))*r259);
                r454=(r451<((r155*(as_type<float>(0xbf800000u)))));
                if(r454==0.0f) {
                  r455=(r451>r155);
                  if(r455!=0.0f) {
                    float r463=(((((((r238-r234))*((r231-r235))))-((((r239-r235))*((r230-r234))))))*r259);
                    float r471=(((((((r226-r238))*((r231-r239))))-((((r227-r239))*((r230-r238))))))*r259);
                    float r479=(((((((r234-r226))*((r231-r227))))-((((r235-r227))*((r230-r226))))))*r259);
                    float r481=(r155*(as_type<float>(0xbf800000u)));
                    r486=(((r463<r481))!=0.0f||((((r471<r481))!=0.0f||((r479<r481))!=0.0f))!=0.0f);
                    if(r486==0.0f) {
                      r494=(((((r463>r155))!=0.0f&&((((r471>r155))!=0.0f&&((r479>r155))!=0.0f))!=0.0f))!=0.0f?long((0u)):(long(-1L)));
                    }
                    r496=(r486!=0.0f?long((1u)):r494);
                  }
                  r498=(r455!=0.0f?r496:(long(-1L)));
                }
                r500=(r454!=0.0f?long((0u)):r498);
              }
              r503=(r323==long(r502)?1.0f:0.0f);
              if(r503==0.0f) {
                r505=(r383==long(r504)?1.0f:0.0f);
                if(r505==0.0f) {
                  r507=(r442==long((1u))?1.0f:0.0f);
                  if(r507==0.0f) {
                    r512=(((((r443!=0.0f?r500:r382))==long((1u))?1.0f:0.0f))!=0.0f?long((3u)):(long(-1L)));
                  }
                  r514=(r507!=0.0f?long((2u)):r512);
                }
                r515=(r505!=0.0f?long(r504):r514);
              }
              long r516=(r503!=0.0f?long(r322):r515);
              r517=(r516<long(r322)?1.0f:0.0f);
              if(r517==0.0f) {
                r519=(r516==long((0u))?1.0f:0.0f);
                if(r519!=0.0f) {
                  uint r520=(buffer4[uint(r37)]=uint(r80),1u);
                  uint r523=(buffer4[((uint(r37)+r521))]=uint(r68),1u);
                  uint r526=(buffer4[((uint(r37)+r524))]=uint(r71),1u);
                  uint r527=(r523+r526);
                  r528=(r520+r527);
                }
                if(r519==0.0f) {
                  r530=(r516==long((1u))?1.0f:0.0f);
                  if(r530!=0.0f) {
                    uint r531=(buffer4[uint(r37)]=uint(r68),1u);
                    uint r534=(buffer4[((uint(r37)+r532))]=uint(r71),1u);
                    uint r537=(buffer4[((uint(r37)+r535))]=uint(r74),1u);
                    uint r538=(r534+r537);
                    r539=(r531+r538);
                  }
                  if(r530==0.0f) {
                    r541=(r516==long((2u))?1.0f:0.0f);
                    if(r541!=0.0f) {
                      uint r542=(buffer4[uint(r37)]=uint(r71),1u);
                      uint r545=(buffer4[((uint(r37)+r543))]=uint(r74),1u);
                      uint r548=(buffer4[((uint(r37)+r546))]=uint(r80),1u);
                      uint r549=(r545+r548);
                      r550=(r542+r549);
                    }
                    if(r541==0.0f) {
                      uint r551=(buffer4[uint(r37)]=uint(r74),1u);
                      uint r554=(buffer4[((uint(r37)+r552))]=uint(r80),1u);
                      uint r557=(buffer4[((uint(r37)+r555))]=uint(r68),1u);
                      uint r558=(r554+r557);
                      r559=(r551+r558);
                    }
                    r560=(r541!=0.0f?r550:r559);
                  }
                  r561=(r530!=0.0f?r539:r560);
                }
                uint r564=(uint(r37)+r563);
                if(r519!=0.0f) {
                  uint r565=(buffer4[r564]=uint(r71),1u);
                  uint r567=(buffer4[((r564+r521))]=uint(r74),1u);
                  uint r569=(buffer4[((r564+r524))]=uint(r80),1u);
                  uint r570=(r567+r569);
                  r571=(r565+r570);
                }
                if(r519==0.0f) {
                  if(r530!=0.0f) {
                    uint r572=(buffer4[r564]=uint(r68),1u);
                    uint r574=(buffer4[((r564+r532))]=uint(r74),1u);
                    uint r576=(buffer4[((r564+r535))]=uint(r80),1u);
                    uint r577=(r574+r576);
                    r578=(r572+r577);
                  }
                  if(r530==0.0f) {
                    if(r541!=0.0f) {
                      uint r579=(buffer4[r564]=uint(r68),1u);
                      uint r581=(buffer4[((r564+r543))]=uint(r71),1u);
                      uint r583=(buffer4[((r564+r546))]=uint(r80),1u);
                      uint r584=(r581+r583);
                      r585=(r579+r584);
                    }
                    if(r541==0.0f) {
                      uint r586=(buffer4[r564]=uint(r68),1u);
                      uint r588=(buffer4[((r564+r552))]=uint(r71),1u);
                      uint r590=(buffer4[((r564+r555))]=uint(r74),1u);
                      uint r591=(r588+r590);
                      r592=(r586+r591);
                    }
                    r593=(r541!=0.0f?r585:r592);
                  }
                  r594=(r530!=0.0f?r578:r593);
                }
                uint r595=(r519!=0.0f?r571:r594);
                r598=(((((r519!=0.0f?r528:r561))+r595))-r597);
              }
              r599=(r517!=0.0f?r502:r598);
            }
            r600=(r266!=0.0f?r599:r104);
          }
          r601=(r89!=0.0f?r100:r600);
        }
        r602=(r87!=0.0f?r601:r69);
      }
      r603=(r67!=0.0f?r602:r26);
    }
    r605=(r23!=0.0f?r603:(0u));
  }
  if(r22==0.0f) {
    bool over607=r20>long(r606) && ulong(r20-long(r606))>4096uL;
    sb|=over607?1u:0u;
    r608=r606;
    for(r607=long(r606);r607<(over607?long(r606):r20);r607++){
  uint r609=256u;
  float r612=0.0f;
  long r613=0;
  uint r616=0u;
  if(r22==0.0f) {
    long r610=(as_type<long>(ulong((ulong(r607)*ulong(long(r609))))<<1)>>1);
    long r611=(as_type<long>(ulong((ulong(r15)+ulong(r610)))<<1)>>1);
    r612=(r611<r18?1.0f:0.0f);
    if(r612!=0.0f) {
      r613=long(buffer5[r611*1+0]);
    }
    uint r615=(r612!=0.0f?uint(r613):(0u));
    r616=(r608+r615);
  }
  uint next607_0=r616;
  r608=next607_0;
    }
    r618=r608;
  }
  threadgroup uint scan619[256];
  {uint v=((r22!=0.0f?r605:r618));scan619[lid]=v;for(uint d=1u;d<256u;d<<=1u){threadgroup_barrier(mem_flags::mem_threadgroup);uint w=lid>=d?scan619[lid-d]:0u;threadgroup_barrier(mem_flags::mem_threadgroup);v+=w;scan619[lid]=v;}threadgroup_barrier(mem_flags::mem_threadgroup);scan619_before=v-((r22!=0.0f?r605:r618));scan619_total=scan619[256-1];}
  uint r620=scan619_total;
  float r622=(r15==long(r21)?1.0f:0.0f);
  if(r622!=0.0f) {
    r624=(r13==long((0u))?1.0f:0.0f);
    if(r624!=0.0f) {
      r625=(buffer5[uint(r14)]=r620,1u);
    }
    if(r624==0.0f) {
      r627=(buffer6[(0u)]=r620,1u);
    }
    r628=(r624!=0.0f?r625:r627);
  }
  uint r629=(r622!=0.0f?r628:r21);
  (void)float(r629);
}
